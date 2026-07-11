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
;;;
;;; The spawn/capture core (build a command, feed stdin, drain stdout and
;;; stderr concurrently, reap, return errors as data) lives in the shared
;;; `run-command` cog. This module is the juju-specific adapter over its
;;; `run-argv`: it supplies the workspace-root flags and the forced
;;; non-interactive environment, and re-exports the `run-vcs` surface the rest
;;; of the codebase calls.

(require-builtin steel/process)
(require "run-command/run-command.scm")
(require "string-utils.scm")
(require "ui-utils.hx/strings.scm")

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
;;   CLICOLOR 0 / NO_COLOR 1    - stable, uncoloured output
;; (jj paging is disabled via the --no-pager flag in `root-args`.)
(define non-interactive-env
  (hash "GIT_PAGER" "cat"
    "PAGER"
    "cat"
    "GIT_TERMINAL_PROMPT"
    "0"
    "GIT_EDITOR"
    "true"
    "EDITOR"
    "true"
    "GIT_OPTIONAL_LOCKS"
    "0"
    "CLICOLOR"
    "0"
    "NO_COLOR"
    "1"))

;; Overlay caller env overrides on top of the non-interactive defaults,
;; returning a new hash. The only current override is the interactive-rebase
;; reword path, which must point GIT_EDITOR at a message-feeding helper (the
;; default forces it to `true`).
(define (merge-env env)
  (if (hash? env)
    (foldl (lambda (k acc) (hash-insert acc k (hash-ref env k)))
      non-interactive-env
      (hash-keys->list env))
    non-interactive-env))

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

;; Core spawn/capture, delegated to the shared `run-command` cog. The
;; workspace-root flags are prepended to the caller's args, and the caller's
;; env overrides are overlaid on the forced non-interactive defaults. `run-argv`
;; runs the program directly (no shell), feeds `input` to stdin only when it is
;; a string (matching the old "leave stdin unwritten on #f" behaviour), drains
;; stdout and stderr concurrently, reaps the child, and returns errors as data.
;; Its result hash carries an extra 'timed-out key (always #f here, since juju
;; sets no timeout); the accessors below ignore it.
(define (run-vcs* root program args input env)
  (run-argv program
    (append (root-args program root) args)
    (hash 'env (merge-env env) 'stdin input)))

;;@doc
;; As `run-vcs`, but returns the captured stdout split into a list of lines
;; (trailing newline dropped). Convenience for line-oriented parsers.
(define (run-vcs-lines root program args)
  (split-lines (vcs-stdout (run-vcs root program args))))

;;@doc
;; Accessors for a run-vcs result hash.
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
