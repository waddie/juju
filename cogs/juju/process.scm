;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; process.scm - the single VCS process entry point
;;;
;;; Every git/jj invocation goes through `run-vcs`. It:
;;;   - passes the workspace root as the binary's own flag (git -C, jj -R),
;;;     never a shell `cd`, so there is no quoting and no interactive shell;
;;;   - pipes and captures both stdout and stderr plus the exit status;
;;;   - never spawns a pager or editor (paging disabled per backend, messages
;;;     and instructions always supplied non-interactively by callers);
;;;   - returns a uniform data hash. Parsers turn that into structs; nothing
;;;     downstream ever sees a live process or raw terminal.

(require "steel/result")
(require-builtin steel/process)
(require "string-utils.scm")

(provide run-vcs
  run-vcs-input
  run-vcs-env
  run-vcs-lines
  vcs-ok?
  vcs-stdout
  vcs-stderr
  vcs-exit
  result-tail
  binary-available?)

;; Environment forced on every child so no command can page or open an editor,
;; and so output is stable rather than localised or coloured.
;;   GIT_PAGER / PAGER cat      - git never invokes less
;;   GIT_TERMINAL_PROMPT 0      - git fails instead of prompting for credentials
;;   GIT_EDITOR / EDITOR true   - any stray editor invocation exits cleanly
;;   GIT_OPTIONAL_LOCKS 0       - status never blocks on the index lock
;;   JJ_CONFIG-less paging      - jj paging disabled via --config below
(define (apply-non-interactive-env! cmd)
  (set-env-var! cmd "GIT_PAGER" "cat")
  (set-env-var! cmd "PAGER" "cat")
  (set-env-var! cmd "GIT_TERMINAL_PROMPT" "0")
  (set-env-var! cmd "GIT_EDITOR" "true")
  (set-env-var! cmd "EDITOR" "true")
  (set-env-var! cmd "GIT_OPTIONAL_LOCKS" "0")
  (set-env-var! cmd "CLICOLOR" "0")
  (set-env-var! cmd "NO_COLOR" "1")
  cmd)

;; Apply caller env overrides on top of the non-interactive defaults. The only
;; current user is the interactive-rebase reword path, which must point
;; GIT_EDITOR at a message-feeding helper (the default forces it to `true`).
(define (apply-env-overrides! cmd env)
  (when (hash? env)
    (for-each (lambda (k) (set-env-var! cmd k (hash-ref env k))) (hash-keys->list env)))
  cmd)

;; Per-backend leading args: the workspace-root flag plus paging/colour off.
;; `git -C <root> --no-pager -c color.ui=never`; `jj -R <root> --no-pager
;; --color never`. These precede the caller's args so a command never has to
;; remember them.
(define (root-args program root)
  (cond
    [(string=? program "git")
      (list "-C" root "--no-pager" "-c" "color.ui=never")]
    [(string=? program "jj")
      (list "-R" root "--no-pager" "--color" "never")]
    [else (list)]))

;;@doc
;; Run `program` ("git" or "jj") with `args` (a list of strings) in workspace
;; `root`. Returns a hash:
;;   'stdout  captured standard output (string)
;;   'stderr  captured standard error (string)
;;   'exit    integer exit code, or #f if the process was killed by a signal
;;   'ok      #t when the binary ran and exited 0, #f otherwise
;; A missing binary or spawn failure yields 'ok #f with the error text in
;; 'stderr rather than throwing.
(define (run-vcs root program args)
  (run-vcs* root program args #f #f))

;;@doc
;; As `run-vcs`, but writes `input` (a string) to the child's standard input
;; and closes it (sending EOF) before reading output. Used to feed a constructed
;; patch to `git apply -` without a temp file.
(define (run-vcs-input root program args input)
  (run-vcs* root program args input #f))

;;@doc
;; As `run-vcs`, but with `env` (a string->string hash) merged over the forced
;; non-interactive environment. Used by the interactive rebase to override
;; GIT_EDITOR for reword message feeding.
(define (run-vcs-env root program args env)
  (run-vcs* root program args #f env))

;; Core spawn/capture. When `input` is a string it is written to the child's
;; stdin and the stream closed; when #f stdin is left unwritten (it closes on
;; drop, giving the child EOF, so nothing blocks reading the tty).
(define (run-vcs* root program args input env)
  (with-handler
    (lambda (err)
      (hash 'stdout "" 'stderr (to-string err) 'exit #f 'ok #f))
    (let* ([full-args (append (root-args program root) args)]
           [cmd (command program full-args)])
      (apply-non-interactive-env! cmd)
      (apply-env-overrides! cmd env)
      ;; Pipe stdout, stderr, and stdin.
      (set-piped-stdout! cmd)
      (let ([spawn-result (spawn-process cmd)])
        (if (Err? spawn-result)
          (hash 'stdout "" 'stderr (to-string (Err->value spawn-result)) 'exit #f 'ok #f)
          (let* ([child (Ok->value spawn-result)]
                 ;; Feed stdin first (and close it) so a command reading a patch
                 ;; from `-` sees EOF and proceeds before we drain stdout.
                 [_ (when (string? input)
                     (let ([sin (child-stdin child)])
                       (when sin
                         (display input sin)
                         (close-output-port sin))))]
                 ;; Drain stdout and stderr concurrently on native threads.
                 ;; Each reader returns on its pipe's EOF, so neither can wedge
                 ;; the other by filling its pipe while we block on the far
                 ;; stream. Reap only once both pipes have closed.
                 [out-box (box "")]
                 [err-box (box "")]
                 [out-t (spawn-native-thread
                         (lambda ()
                           (set-box! out-box
                             (read-port-to-string (child-stdout child)))))]
                 [err-t (spawn-native-thread
                         (lambda ()
                           (set-box! err-box
                             (let ([e (child-stderr child)])
                               (if e (read-port-to-string e) "")))))]
                 [_ (thread-join! out-t)]
                 [_ (thread-join! err-t)]
                 [wait-result (wait child)]
                 [exit (if (Ok? wait-result) (Ok->value wait-result) #f)])
            (hash 'stdout (unbox out-box)
              'stderr
              (unbox err-box)
              'exit
              exit
              'ok
              (equal? exit 0))))))))

;;@doc
;; As `run-vcs`, but returns the captured stdout split into a list of lines
;; (trailing newline dropped). Convenience for line-oriented parsers.
(define (run-vcs-lines root program args)
  (split-lines (vcs-stdout (run-vcs root program args))))

;;@doc Accessors for a run-vcs result hash.
(define (vcs-ok? r) (hash-ref r 'ok))
(define (vcs-stdout r) (hash-ref r 'stdout))
(define (vcs-stderr r) (hash-ref r 'stderr))
(define (vcs-exit r) (hash-ref r 'exit))

;;@doc
;; The reportable tail of a run-vcs result: the last non-blank line of stderr if
;; any, else of stdout. Used to build a one-line message from a failed command.
(define (result-tail res)
  (let ([err (last-line (vcs-stderr res))])
    (if (string=? err "") (last-line (vcs-stdout res)) err)))

;;@doc
;; #t when `program` is on PATH.
(define (binary-available? program)
  (with-handler (lambda (err) #f)
    (if (which program) #t #f)))
