;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; commands.scm - command bodies, dispatched through the active backend
;;;
;;; Each command resolves the active backend for the editor's workspace, gathers
;;; whatever argument it needs (a file path, a backend name), calls the backend,
;;; and presents the result. No command branches on git-vs-jj: divergence lives
;;; in the backends, and feature availability is read from capabilities.

(require-builtin helix/components)
(require "helix/misc.scm")
(require "process.scm")
(require "backend-detect.scm")
(require "backend-interface.scm")
(require "model.scm")
(require "config.scm")
(require "status-view.scm")
(require "text-view.scm")
(require "view-rows.scm")
(require "rebase-todo.scm")
(require "rebase-view.scm")
(require "blame-view.scm")
(require "log-view.scm")
(require "menu-model.scm")
(require "menu.scm")
(require "prompts.scm")
(require "string-utils.scm")

(provide juju-status
  juju-log
  juju-diff
  juju-blame
  juju-set-backend
  ;; mutations
  juju-stage
  juju-unstage
  juju-discard
  juju-stage-all
  juju-unstage-all
  juju-commit
  juju-amend
  juju-commit-fixup
  juju-extend
  juju-fetch
  juju-pull
  juju-push
  juju-undo
  juju-redo
  ;; history rewriting
  juju-rebase
  juju-rebase-interactive
  juju-rebase-continue
  juju-rebase-abort
  juju-rebase-skip
  juju-cherry-pick
  juju-revert
  juju-reset
  juju-squash
  juju-split
  juju-abandon
  juju-describe
  ;; branch/bookmark, stash, listings
  juju-switch
  juju-edit
  juju-branch-create
  juju-branch-set
  juju-branch-rename
  juju-branch-delete
  juju-set-upstream
  juju-stash
  juju-stash-pop
  juju-stash-apply
  juju-stash-drop
  juju-refs
  juju-remote
  juju-oplog
  juju-reflog
  juju-worktree
  juju-submodule
  ;; polish: escape hatch, transient menus, aliases
  juju-run
  juju-dispatch
  juju-rebase-menu
  juju-remote-menu
  juju-branch-menu
  juju-commit-menu
  juju-log-menu
  juju-annotate
  juju-reword
  juju-drop
  juju-bookmark-create
  juju-bookmark-set
  juju-bookmark-move
  juju-bookmark-rename
  juju-bookmark-delete)

;; Resolve the active backend for the editor's workspace, or #f (with an echoed
;; message) when the editor is not inside a repository.
(define (resolve-backend)
  (let ([cwd (editor-cwd)])
    (if (not cwd)
      (begin (set-status! "juju: cannot determine working directory") #f)
      (let ([b (active-backend cwd)])
        (if b b
          (begin (set-status! "juju: not inside a git or jj repository") #f))))))

;;@doc Open the status view for the current workspace.
(define (juju-status)
  (let ([cwd (editor-cwd)])
    (if cwd
      (open-status-view cwd)
      (set-status! "juju: cannot determine working directory"))))

;;@doc Show the recent log of the current workspace in an interactive overlay.
(define (juju-log)
  (let ([b (resolve-backend)])
    (when b (open-log-for b (juju-log-count)))))

;; Open the interactive log view. Its mutation epilogue mirrors `report`: after
;; an edit/new/revert/... an open status view refreshes so it reflects the move.
(define (open-log-for b limit)
  (open-log-view b (hash 'limit limit)
    (lambda () (when (juju-auto-refresh) (refresh-open-view!)))))

;;@doc Show the workspace diff (changes against HEAD / the working copy).
(define (juju-diff)
  (let ([b (resolve-backend)])
    (when b
      (let ([hunks (backend-diff b (hash 'type 'worktree))])
        (show-text-view " juju diff "
          (if (null? hunks) '() (hunks->lines hunks)))))))

;;@doc
;; Blame the current file interactively: Enter shows a line's commit, l chases
;; to the parent revision, h goes back.
(define (juju-blame)
  (let ([b (resolve-backend)])
    (when b
      (let ([path (current-file-path)])
        (if (not path)
          (set-status! "juju: no file to blame (open one first)")
          (let ([rel (rel-path (backend-root b) path)])
            (open-blame-view rel
              (run-blame-query b rel #f #f)
              (blame-query-fn b)
              (blame-show-fn b))))))))

;; The 'blame query behind the view's chase loop. rev #f is the working copy;
;; before #t blames at the parent of rev (suffix syntax stays in the backend).
(define (run-blame-query b file rev before)
  (backend-query b 'blame (list (hash 'file file 'rev rev 'before before))))

(define (blame-query-fn b)
  (lambda (file rev before) (run-blame-query b file rev before)))

;; Show a blamed line's commit in a text view. A commit-less result (e.g.
;; git's all-zero "not committed yet" sha) echoes instead.
(define (blame-show-fn b)
  (lambda (commit-id)
    (let* ([shown (backend-show b commit-id)]
           [commit (hash-ref shown 'commit)])
      (if (not commit)
        (set-status! "juju: nothing to show (line not committed?)")
        (show-text-view
          (string-append " " (commit-record-short-id commit) "  "
            (commit-record-subject commit)
            " ")
          (commit-show-lines shown))))))

;;@doc
;; Override which backend the current workspace uses ('git or 'jj), remembered
;; for the session. With no/unknown argument, report the current choice.
(define (juju-set-backend . args)
  (let ([cwd (editor-cwd)])
    (if (not cwd)
      (set-status! "juju: cannot determine working directory")
      (let* ([root (workspace-root-from cwd)]
             [arg (if (pair? args) (normalise-backend-arg (car args)) #f)])
        (cond
          [(not root) (set-status! "juju: not inside a git or jj repository")]
          [(not arg)
            (set-status!
              (string-append "juju: backend is "
                (symbol->string (detect-backend-name root))
                "  (use :juju-backend git|jj)"))]
          [(not (member arg (available-backends root)))
            (set-status! (string-append "juju: this workspace has no "
                          (symbol->string arg)
                          " repository"))]
          [else
            (set-workspace-backend-override! root arg)
            (set-status! (string-append "juju: backend set to " (symbol->string arg)))])))))

(define (normalise-backend-arg a)
  (let ([s (if (symbol? a) (symbol->string a) (to-string a))])
    (cond
      [(string=? s "git") 'git]
      [(string=? s "jj") 'jj]
      [else #f])))

;;; Mutations (typed commands) ;;;
;;;
;;; The selection-first mutations live in the status view; these typed commands
;;; are the keyboard-free equivalents, acting on the current file (stage/unstage/
;;; discard) or the whole workspace (the rest). They run synchronously, echo the
;;; result, and refresh an open status view (see `report`).

;;@doc Stage the current file's changes.
(define (juju-stage)
  (with-current-file-spec 'stage '(untracked unstaged conflicts)
    "juju: current file has no unstaged changes"
    (lambda (b spec) (report (backend-stage b (list spec))))))

;;@doc Unstage the current file's changes (git only).
(define (juju-unstage)
  (with-current-file-spec 'unstage '(staged)
    "juju: current file has no staged changes"
    (lambda (b spec) (report (backend-unstage b (list spec))))))

;;@doc Discard the current file's changes (confirms first).
(define (juju-discard)
  (with-current-file-spec 'discard '(untracked unstaged staged working-copy)
    "juju: current file has no changes to discard"
    (lambda (b spec)
      (push-component!
        (prompt (string-append "Discard changes to the current file? "
                 (discard-confirm-note b)
                 " [y/N]: ")
          (lambda (input)
            (when (confirmed? input) (report (backend-discard b (list spec))))))))))

;;@doc Stage every change in the workspace (git only).
(define (juju-stage-all) (bulk 'stage-all (lambda (b) (backend-stage-all b))))

;;@doc Unstage every change in the workspace (git only).
(define (juju-unstage-all) (bulk 'unstage-all (lambda (b) (backend-unstage-all b))))

;;@doc Commit. With arguments, they form the message; otherwise prompts.
(define (juju-commit . args) (commit-command args #f))

;;@doc Amend the latest commit. With arguments they replace the message;
;; otherwise prompts (an empty message keeps the existing one).
(define (juju-amend . args) (commit-command args #t))

;;@doc Record a fixup! commit (git) / squash into a change (jj): needs a rev.
(define (juju-commit-fixup . args)
  (let ([b (resolve-backend)])
    (when b
      (if (null? args)
        (set-status! "juju: usage - :juju-commit-fixup <rev>")
        (report (backend-commit-fixup b (to-string (car args)) (hash)))))))

;;@doc Fold the working changes into the latest commit, message unchanged.
(define (juju-extend)
  (let ([b (resolve-backend)]) (when b (report (backend-extend b (hash))))))

;;@doc Fetch from a remote (optional remote name argument).
(define (juju-fetch . args) (network-command 'fetch args))

;;@doc Pull/integrate from a remote (optional remote name argument).
(define (juju-pull . args) (network-command 'pull args))

;;@doc Push to a remote (optional remote name argument).
(define (juju-push . args) (network-command 'push args))

;;@doc Undo the last operation (jj: `jj undo`; git: best-effort via the reflog).
(define (juju-undo) (bulk 'undo (lambda (b) (backend-undo b))))

;;@doc Redo the last undone operation (jj only).
(define (juju-redo) (bulk 'redo (lambda (b) (backend-redo b))))

;;; History rewriting (typed commands) ;;;
;;;
;;; These take a rev/ref argument rather than a status-view selection.
;;; Each resolves the backend, checks the capability, and reports uniformly;
;;; capabilities differ by backend (rebase/cherry-pick/revert/reset on git;
;;; squash/split/abandon/describe/rebase/revert on jj), so a command the active
;;; backend lacks reports "not supported" rather than acting.

;;@doc Rebase onto a ref: :juju-rebase [--autosquash] <ref>.
(define (juju-rebase . args)
  (with-cap 'rebase
    (lambda (b)
      (let* ([autosquash (and (pair? args) (string=? (to-string (car args)) "--autosquash"))]
             [rest (if autosquash (cdr args) args)]
             [onto (if (pair? rest) (to-string (car rest)) #f)])
        (if (not onto)
          (set-status! "juju: usage - :juju-rebase [--autosquash] <ref>")
          (report (backend-rebase b (hash 'onto onto 'autosquash autosquash))))))))

;;@doc
;; Open the interactive rebase editor: :juju-rebase-interactive [base]. Edits the
;; commits base..tip (the upstream by default on git; the mutable ancestors of @
;; on jj). Reorder, then assign pick/reword/edit/squash/fixup/drop, then Enter to
;; apply (q to cancel). On git an `edit` step or a conflict pauses the rebase;
;; resume with :juju-rebase-continue / -abort / -skip.
(define (juju-rebase-interactive . args)
  (with-cap 'rebase-interactive
    (lambda (b)
      (open-rebase-interactive b (if (pair? args) (to-string (car args)) #f)))))

;; The thunk the editor runs on confirm: apply the edited plan and report. The
;; backend (and its base) are captured when the editor opens; the root is stable
;; for the editor's lifetime.
(define (rebase-apply-callback b base)
  (lambda (entries)
    (report (backend-rebase-interactive b (hash 'entries entries 'base base)))))

;;@doc Continue a paused rebase after resolving an edit/conflict (git).
(define (juju-rebase-continue)
  (with-cap 'rebase-interactive (lambda (b) (report (backend-rebase-continue b)))))

;;@doc Abort a paused rebase, restoring the original tip (git).
(define (juju-rebase-abort)
  (with-cap 'rebase-interactive (lambda (b) (report (backend-rebase-abort b)))))

;;@doc Skip the current commit in a paused rebase (git).
(define (juju-rebase-skip)
  (with-cap 'rebase-interactive (lambda (b) (report (backend-rebase-skip b)))))

;;@doc Cherry-pick a commit onto the current branch: :juju-cherry-pick <rev> (git).
(define (juju-cherry-pick . args)
  (with-cap 'cherry-pick
    (lambda (b)
      (if (null? args)
        (set-status! "juju: usage - :juju-cherry-pick <rev>")
        (report (backend-cherry-pick b (to-string (car args)) (hash)))))))

;;@doc Revert a commit: :juju-revert <rev>.
(define (juju-revert . args)
  (with-cap 'revert
    (lambda (b)
      (if (null? args)
        (set-status! "juju: usage - :juju-revert <rev>")
        (report (backend-revert b (to-string (car args)) (hash)))))))

;;@doc Reset HEAD: :juju-reset [soft|mixed|hard] [rev]. Hard confirms first.
(define (juju-reset . args)
  (with-cap 'reset
    (lambda (b)
      (let* ([mr (reset-mode-and-rev args)]
             [mode (car mr)]
             [rev (cdr mr)])
        (if (eq? mode 'hard)
          (push-component!
            (prompt "Hard reset discards uncommitted changes. Continue? [y/N]: "
              (lambda (input)
                (when (confirmed? input) (report (backend-reset b mode rev (hash)))))))
          (report (backend-reset b mode rev (hash))))))))

;;@doc Squash into a change: :juju-squash [rev] (jj; folds @ into parent or rev).
(define (juju-squash . args)
  (with-cap 'squash
    (lambda (b)
      (let ([into (if (pair? args) (to-string (car args)) #f)])
        (report (backend-squash b (if into (hash 'into into) (hash))))))))

;;@doc Split files out of @ into a new change: :juju-split <path...> (jj).
;; With no path, splits out the current file.
(define (juju-split . args)
  (with-cap 'split
    (lambda (b)
      (let ([paths (if (pair? args) (map to-string args) (current-file-rel-list b))])
        (if (null? paths)
          (set-status! "juju: usage - :juju-split <path> (or open a file to split out)")
          (report (backend-split b paths (hash))))))))

;;@doc Abandon a change: :juju-abandon [rev] (jj; @ when omitted).
(define (juju-abandon . args)
  (with-cap 'abandon
    (lambda (b)
      (report (backend-abandon b (if (pair? args) (to-string (car args)) #f) (hash))))))

;;@doc Set @'s description: :juju-describe [message] (jj; prompts when omitted).
(define (juju-describe . args)
  (with-cap 'describe
    (lambda (b)
      (let ([msg (args->message args)])
        (if msg
          (report (backend-describe b msg (hash)))
          (push-component!
            (prompt "Description: "
              (lambda (input)
                (when input (report (backend-describe b input (hash))))))))))))

;;; Branch/bookmark and stash (typed commands) ;;;
;;;
;;; A named ref is a branch (git) or bookmark (jj); the same commands serve both
;;; through the backend's capability and op symbols. set-upstream and the stash
;;; family are git-only and report "not supported" under jj.

;;@doc Switch to a branch/bookmark or commit: :juju-switch <target> (prompts if omitted).
(define (juju-switch . args)
  (with-cap 'switch
    (lambda (b)
      (if (pair? args)
        (report (backend-switch b (to-string (car args))))
        (push-component!
          (prompt "Switch to (branch/bookmark/rev): "
            (lambda (input)
              (when (not (blank? input)) (report (backend-switch b input))))))))))

;;@doc Edit a change, making it the working copy: :juju-edit <rev> (jj; prompts if omitted).
(define (juju-edit . args)
  (with-cap 'edit
    (lambda (b)
      (if (pair? args)
        (report (backend-edit b (to-string (car args))))
        (push-component!
          (prompt "Edit which change (rev): "
            (lambda (input)
              (when (not (blank? input)) (report (backend-edit b input))))))))))

;;@doc Create a branch/bookmark: :juju-branch-create <name> [rev] (prompts for name if omitted).
(define (juju-branch-create . args)
  (with-cap 'branch
    (lambda (b)
      (if (pair? args)
        (let ([name (to-string (car args))]
              [rev (if (pair? (cdr args)) (to-string (cadr args)) #f)])
          (report (backend-branch-create b name rev)))
        (push-component!
          (prompt "New branch/bookmark name: "
            (lambda (input)
              (when (not (blank? input)) (report (backend-branch-create b input #f))))))))))

;;@doc Set/move a branch/bookmark to a rev: :juju-branch-set <name> [rev] (rev defaults to current; prompts for name if omitted).
(define (juju-branch-set . args)
  (with-cap 'branch
    (lambda (b)
      (if (pair? args)
        (let ([name (to-string (car args))]
              [rev (if (pair? (cdr args)) (to-string (cadr args)) #f)])
          (report (backend-branch-set b name rev)))
        (push-component!
          (prompt "Set which branch/bookmark: "
            (lambda (input)
              (when (not (blank? input)) (report (backend-branch-set b input #f))))))))))

;;@doc Rename a branch/bookmark: :juju-branch-rename <old> <new>.
(define (juju-branch-rename . args)
  (with-cap 'branch
    (lambda (b)
      (if (< (length args) 2)
        (set-status! "juju: usage - :juju-branch-rename <old> <new>")
        (report (backend-branch-rename b (to-string (car args)) (to-string (cadr args))))))))

;;@doc Delete a branch/bookmark: :juju-branch-delete <name>.
(define (juju-branch-delete . args)
  (with-cap 'branch
    (lambda (b)
      (if (null? args)
        (set-status! "juju: usage - :juju-branch-delete <name>")
        (report (backend-branch-delete b (to-string (car args)) (hash)))))))

;;@doc Set a branch's upstream: :juju-set-upstream <branch> <upstream> (git).
(define (juju-set-upstream . args)
  (with-cap 'set-upstream
    (lambda (b)
      (if (< (length args) 2)
        (set-status! "juju: usage - :juju-set-upstream <branch> <upstream>")
        (report (backend-set-upstream b (to-string (car args)) (to-string (cadr args))))))))

;;@doc Stash the working changes: :juju-stash [message] (git).
(define (juju-stash . args)
  (with-cap 'stash
    (lambda (b)
      (let ([msg (args->message args)])
        (report (backend-stash b (if msg (hash 'message msg) (hash))))))))

;;@doc Pop a stash: :juju-stash-pop [stash@{N}] (git; latest when omitted).
(define (juju-stash-pop . args)
  (with-cap 'stash
    (lambda (b) (report (backend-stash-pop b (if (pair? args) (to-string (car args)) #f))))))

;;@doc Apply a stash without dropping it: :juju-stash-apply [stash@{N}] (git).
(define (juju-stash-apply . args)
  (with-cap 'stash
    (lambda (b) (report (backend-stash-apply b (if (pair? args) (to-string (car args)) #f))))))

;;@doc Drop a stash: :juju-stash-drop [stash@{N}] (git; latest when omitted).
(define (juju-stash-drop . args)
  (with-cap 'stash
    (lambda (b) (report (backend-stash-drop b (if (pair? args) (to-string (car args)) #f))))))

;;; Read-only listings (typed commands) ;;;

;;@doc Show all refs: branches/tags/remotes (git) or bookmarks (jj).
(define (juju-refs)
  (let ([b (resolve-backend)])
    (when b
      (show-text-view
        (string-append " juju refs (" (symbol->string (backend-name b)) ") ")
        (backend-refs b)))))

;;@doc Show the configured remotes.
(define (juju-remote)
  (let ([b (resolve-backend)])
    (when b
      (show-text-view
        (string-append " juju remotes (" (symbol->string (backend-name b)) ") ")
        (backend-remotes b)))))

;;@doc Show the jj operation log.
(define (juju-oplog)
  (with-cap 'oplog
    (lambda (b) (show-text-view " juju op log " (backend-oplog b)))))

;;@doc Show the git reflog.
(define (juju-reflog)
  (with-cap 'reflog
    (lambda (b) (show-text-view " juju reflog " (backend-reflog b)))))

;;@doc Show the worktrees (git) or workspaces (jj).
(define (juju-worktree)
  (let ([b (resolve-backend)])
    (when b
      (show-text-view
        (string-append " juju worktrees (" (symbol->string (backend-name b)) ") ")
        (backend-worktrees b)))))

;;@doc Show the submodule status (git; jj has none).
(define (juju-submodule)
  (let ([b (resolve-backend)])
    (when b
      (show-text-view
        (string-append " juju submodules (" (symbol->string (backend-name b)) ") ")
        (backend-submodules b)))))

;;; Escape hatch ;;;

;;@doc
;; Run an arbitrary command line for the active backend in the workspace root and
;; show its output: :juju-run <args...> (e.g. :juju-run log --oneline -5). The
;; binary is the active backend's (git or jj); switch with :juju-backend. Output
;; is captured and shown in a text overlay, then an open status view refreshes.
(define (juju-run . args)
  (let ([b (resolve-backend)])
    (when b
      (if (null? args)
        (set-status! "juju: usage - :juju-run <args...>")
        (let* ([prog (symbol->string (backend-name b))]
               [argv (map to-string args)]
               [res (run-vcs (backend-root b) prog argv)])
          (show-text-view
            (string-append " juju run: " prog " " (string-join argv " ") " ")
            (run-output-lines res))
          (when (juju-auto-refresh) (refresh-open-view!)))))))

;; Combine a run-vcs result into display lines: stdout, then stderr after a blank
;; separator when present. A silent command shows its exit code so the overlay is
;; never empty (which would otherwise echo "nothing to show").
(define (run-output-lines res)
  (let* ([out (split-lines (vcs-stdout res))]
         [err (split-lines (vcs-stderr res))]
         [combined (append out (if (null? err) '() (cons "" err)))])
    (if (null? combined)
      (list (string-append "(exit " (to-string (vcs-exit res)) ", no output)"))
      combined)))

;;; Transient menus ;;;
;;;
;;; The discoverable counterpart to the typed history/branch/remote commands: a
;;; popup listing switches and actions, each on one key (see menu.scm). Built per
;;; active backend, so a switch only the backend supports (autosquash, force push)
;;; appears only when its capability is set: feature availability comes from
;;; capabilities, never the backend name.

(define (menu-title b label)
  (string-append " " label " (" (symbol->string (backend-name b)) ") "))

;; Read switch `flag` from a menu's switch-state hash (#f when absent).
(define (sw switches flag)
  (and (hash-contains? switches flag) (hash-ref switches flag)))

;; Collect the switches in `flags` that are on into an opts hash for the
;; backend call. Off switches are omitted; backends read them with a #f
;; default, and the network backends additionally gate each flag on its
;; subcommand.
(define (switch-opts switches flags)
  (foldl (lambda (flag h) (if (sw switches flag) (hash-insert h flag #t) h))
    (hash)
    flags))

;;@doc Open the rebase transient (--autosquash on git, --skip-emptied on jj; action: onto a ref).
(define (juju-rebase-menu)
  (with-cap 'rebase
    (lambda (b) (show-menu (menu-title b "Rebase") (rebase-menu-entries b)))))

(define (rebase-menu-entries b)
  (append
    (list (menu-info "Rebase"))
    (if (backend-supports? b 'autosquash)
      (list (menu-switch #\a 'autosquash "--autosquash (fold fixup!/squash!)" #f))
      '())
    (if (backend-supports? b 'rebase-skip-emptied)
      (list (menu-switch #\e 'skip-emptied "--skip-emptied (abandon emptied commits)" #f))
      '())
    (list
      (menu-action #\o "onto a ref"
        (lambda (switches)
          (push-component!
            (prompt "Rebase onto (ref): "
              (lambda (input)
                (when (not (blank? input))
                  (report (backend-rebase b
                           (hash-insert
                             (switch-opts switches '(autosquash skip-emptied))
                             'onto
                             input)))))))))
      (menu-action #\i "interactive (edit todo)"
        (lambda (switches) (open-rebase-interactive b #f))))))

;; Open the interactive editor for backend `b` over `base..tip` (default base
;; when #f). Shared by the typed command and the rebase menu.
(define (open-rebase-interactive b base)
  (let* ([range (backend-query b 'rebase-range (list (hash 'base base)))]
         [commits (hash-ref range 'commits)]
         [resolved (hash-ref range 'base)])
    (cond
      [(and (null? commits) (not resolved))
        (set-status!
          "juju: HEAD has no upstream; pass a base - :juju-rebase-interactive <base>")]
      [(null? commits) (set-status! "juju: no commits to rebase in range")]
      [else (open-rebase-view (make-todo commits) (rebase-apply-callback b resolved))])))

;;@doc Open the remote transient (fetch / pull / push, with per-action switches).
(define (juju-remote-menu)
  (with-cap 'fetch
    (lambda (b) (show-menu (menu-title b "Remote") (remote-menu-entries b)))))

(define (remote-menu-entries b)
  (append
    (list (menu-info "Remote"))
    (if (backend-supports? b 'force-push)
      (list (menu-switch #\F 'force "force-with-lease (push)" #f))
      '())
    (if (backend-supports? b 'fetch-prune)
      (list (menu-switch #\P 'prune "--prune (fetch)" #f))
      '())
    (list (menu-switch #\A 'all-remotes "--all-remotes (fetch)" #f))
    (if (backend-supports? b 'push-set-upstream)
      (list (menu-switch #\U 'set-upstream "--set-upstream (push)" #f))
      '())
    (if (backend-supports? b 'pull-rebase)
      (list (menu-switch #\R 'rebase "--rebase (pull)" #f))
      '())
    (list
      (menu-action #\f "fetch"
        (lambda (switches)
          (report (backend-mutate b 'fetch
                   (list (switch-opts switches '(prune all-remotes)))))))
      (menu-action #\u "pull"
        (lambda (switches)
          (report (backend-mutate b 'pull
                   (list (switch-opts switches '(rebase)))))))
      (menu-action #\p "push"
        (lambda (switches)
          (report (backend-mutate b 'push
                   (list (switch-opts switches '(force set-upstream))))))))))

;;@doc Open the branch/bookmark transient (create / switch / rename / delete).
(define (juju-branch-menu)
  (with-cap 'branch
    (lambda (b) (show-menu (menu-title b "Branch / bookmark") (branch-menu-entries b)))))

(define (branch-menu-entries b)
  (append
    (list (menu-info "Branch / bookmark"))
    (if (backend-supports? b 'branch-force-delete)
      (list (menu-switch #\f 'force "force (delete: -D)" #f))
      '())
    (list
      (menu-action #\c "create"
        (lambda (switches)
          (push-component!
            (prompt "New branch/bookmark name: "
              (lambda (input)
                (when (not (blank? input)) (report (backend-branch-create b input #f))))))))
      (menu-action #\m "set/move"
        (lambda (switches)
          (push-component!
            (prompt "Set which branch/bookmark: "
              (lambda (input)
                (when (not (blank? input)) (report (backend-branch-set b input #f))))))))
      (menu-action #\s "switch"
        (lambda (switches)
          (push-component!
            (prompt "Switch to (branch/bookmark/rev): "
              (lambda (input)
                (when (not (blank? input)) (report (backend-switch b input))))))))
      (menu-action #\r "rename"
        (lambda (switches)
          (push-component!
            (prompt "Rename which branch/bookmark: "
              (lambda (old)
                (when (not (blank? old))
                  (push-component!
                    (prompt "New name: "
                      (lambda (new)
                        (when (not (blank? new)) (report (backend-branch-rename b old new))))))))))))
      (menu-action #\d "delete"
        (lambda (switches)
          (let ([opts (if (sw switches 'force) (hash 'force #t) (hash))])
            (push-component!
              (prompt "Delete which branch/bookmark: "
                (lambda (input)
                  (when (not (blank? input)) (report (backend-branch-delete b input opts))))))))))
    (if (backend-supports? b 'set-upstream)
      (list
        (menu-action #\u "set upstream"
          (lambda (switches)
            (push-component!
              (prompt "Set upstream of which branch: "
                (lambda (name)
                  (when (not (blank? name))
                    (push-component!
                      (prompt "Upstream ref: "
                        (lambda (up)
                          (when (not (blank? up)) (report (backend-set-upstream b name up)))))))))))))
      '())))

;;@doc Open the commit transient (--no-verify / --signoff on git; commit / amend).
(define (juju-commit-menu)
  (with-cap 'commit
    (lambda (b) (show-menu (menu-title b "Commit") (commit-menu-entries b)))))

(define (commit-menu-entries b)
  (append
    (list (menu-info "Commit"))
    (if (backend-supports? b 'commit-no-verify)
      (list (menu-switch #\n 'no-verify "--no-verify (skip hooks)" #f))
      '())
    (if (backend-supports? b 'commit-signoff)
      (list (menu-switch #\S 'signoff "--signoff" #f))
      '())
    (list
      (menu-action #\c "commit"
        (lambda (switches)
          (push-component!
            (prompt "Commit message: "
              (lambda (input)
                (report (backend-commit b (or input "") (commit-opts switches))))))))
      (menu-action #\a "amend"
        (lambda (switches)
          (push-component!
            (prompt "Amend message (empty keeps existing): "
              (lambda (input)
                (report (backend-amend b (or input "") (commit-opts switches)))))))))))

;; Assemble the commit/amend opts hash from the menu's switch state.
(define (commit-opts switches)
  (let* ([h (hash)]
         [h (if (sw switches 'no-verify) (hash-insert h 'no-verify #t) h)]
         [h (if (sw switches 'signoff) (hash-insert h 'signoff #t) h)])
    h))

;;@doc Open the log transient (-n count infix, then show the log).
(define (juju-log-menu)
  (let ([b (resolve-backend)])
    (when b (show-menu (menu-title b "Log") (log-menu-entries b)))))

(define (log-menu-entries b)
  (list
    (menu-info "Log")
    (menu-arg #\n 'count "-n count" (number->string (juju-log-count)))
    (menu-action #\l "show log"
      (lambda (switches)
        (let ([n (parse-positive-int (sw switches 'count) (juju-log-count))])
          (open-log-for b n))))))

;; Parse a positive integer from `s` (an arg value), falling back to `default`.
(define (parse-positive-int s default)
  (let ([n (and (string? s) (string->number (string-trim s)))])
    (if (and (integer? n) (> n 0)) n default)))

;;@doc Open the top-level transient: a menu of the juju sub-menus.
(define (juju-dispatch)
  (let ([b (resolve-backend)])
    (when b (show-menu (menu-title b "Dispatch") (dispatch-entries)))))

;; The sub-menu launchers. Each action closes this menu (handle-menu closes
;; before running the thunk) and opens the chosen transient.
(define (dispatch-entries)
  (list
    (menu-info "juju dispatch")
    (menu-action #\c "commit…" (lambda (switches) (juju-commit-menu)))
    (menu-action #\r "rebase…" (lambda (switches) (juju-rebase-menu)))
    (menu-action #\R "remote…" (lambda (switches) (juju-remote-menu)))
    (menu-action #\b "branch / bookmark…" (lambda (switches) (juju-branch-menu)))
    (menu-action #\l "log…" (lambda (switches) (juju-log-menu)))))

;;; Command aliases ;;;
;;;
;;; A single concept that each backend names differently gets an alias, so a user
;;; reaches for their backend's term and lands on the same juju command. These add
;;; no behaviour: they delegate to the canonical command (which gates by capability).

;;@doc Alias of :juju-blame (git says blame; jj says `file annotate`).
(define (juju-annotate) (juju-blame))

;;@doc Alias of :juju-describe (jj reword): :juju-reword [message].
(define (juju-reword . args) (apply juju-describe args))

;;@doc Alias of :juju-abandon (jj drop): :juju-drop [rev].
(define (juju-drop . args) (apply juju-abandon args))

;;@doc Alias of :juju-branch-create (jj bookmark): :juju-bookmark-create <name> [rev].
(define (juju-bookmark-create . args) (apply juju-branch-create args))

;;@doc Alias of :juju-branch-set (jj bookmark set): :juju-bookmark-set <name> [rev].
(define (juju-bookmark-set . args) (apply juju-branch-set args))

;;@doc Alias of :juju-branch-set (jj bookmark move): :juju-bookmark-move <name> [rev].
(define (juju-bookmark-move . args) (apply juju-branch-set args))

;;@doc Alias of :juju-branch-rename (jj bookmark): :juju-bookmark-rename <old> <new>.
(define (juju-bookmark-rename . args) (apply juju-branch-rename args))

;;@doc Alias of :juju-branch-delete (jj bookmark): :juju-bookmark-delete <name>.
(define (juju-bookmark-delete . args) (apply juju-branch-delete args))

;;; Mutation plumbing ;;;

;; Echo a mutation's outcome and refresh an open status view so it reflects the
;; change without the user pressing `g`. In a colocated repo on the git backend
;; the message carries the desync note; refresh is gated by `juju-auto-refresh`.
(define (report r)
  (set-status! (string-append (result-message r) (colocated-note)))
  (when (juju-auto-refresh) (refresh-open-view!)))

;; The colocated-repo desync note, or "" when it does not apply. Fires only when
;; the warning is enabled, the workspace is colocated, and the active backend is
;; git (a git mutation is what desyncs jj's recorded working copy until its next
;; command re-imports). Resolved from the editor cwd so `report` needs no backend.
(define (colocated-note)
  (if (not (juju-warn-colocated))
    ""
    (let ([cwd (editor-cwd)])
      (if (not cwd)
        ""
        (let ([root (workspace-root-from cwd)])
          ;; Reading the backend name here is intentional, not a feature gate:
          ;; the desync is a git-specific display warning, not a capability.
          (if (and root (colocated? root) (eq? (detect-backend-name root) 'git))
            "  (colocated: jj re-imports on next jj command)"
            ""))))))

(define (unsupported-status b cap)
  (set-status! (unsupported-message b cap)))

;; Resolve the backend, check `cap`, locate the current file as a whole-file
;; operand within `kinds`, and call `k` with (backend spec). Echoes `none-msg`
;; when the file is not among the wanted sections.
(define (with-current-file-spec cap kinds none-msg k)
  (let ([b (resolve-backend)])
    (when b
      (cond
        [(not (backend-supports? b cap)) (unsupported-status b cap)]
        [else
          (let ([spec (current-file-operand b kinds)])
            (if spec (k b spec) (set-status! none-msg)))]))))

(define (bulk cap thunk)
  (let ([b (resolve-backend)])
    (when b
      (if (not (backend-supports? b cap)) (unsupported-status b cap) (report (thunk b))))))

;; Resolve the backend and check `cap`, then call `k` with the backend. Unlike
;; `bulk`, `k` does its own reporting (so commands that prompt, parse arguments,
;; or branch on the result can). Echoes the unsupported message when `cap` is off.
(define (with-cap cap k)
  (let ([b (resolve-backend)])
    (when b
      (if (backend-supports? b cap) (k b) (unsupported-status b cap)))))

;; Parse :juju-reset arguments into (mode . rev). A leading soft/mixed/hard sets
;; the mode (default mixed) and any following token is the rev; otherwise the
;; first token is the rev and the mode stays mixed. rev is #f (-> HEAD) when none.
(define (reset-mode-and-rev args)
  (if (null? args)
    (cons 'mixed #f)
    (let ([kw (reset-keyword (to-string (car args)))])
      (if kw
        (cons kw (if (pair? (cdr args)) (to-string (cadr args)) #f))
        (cons 'mixed (to-string (car args)))))))

(define (reset-keyword s)
  (cond
    [(string=? s "soft") 'soft]
    [(string=? s "mixed") 'mixed]
    [(string=? s "hard") 'hard]
    [else #f]))

;; The current file as a one-element root-relative path list, or '() when no
;; file is focused. Used by :juju-split with no explicit path.
(define (current-file-rel-list b)
  (let ([path (current-file-path)])
    (if path (list (rel-path (backend-root b) path)) '())))

(define (commit-command args amend?)
  (let ([b (resolve-backend)]
        [cap (if amend? 'amend 'commit)])
    (when b
      (if (not (backend-supports? b cap))
        (unsupported-status b cap)
        (let ([msg (args->message args)])
          (if msg
            (report (if amend? (backend-amend b msg (hash)) (backend-commit b msg (hash))))
            (push-component!
              (prompt (if amend? "Amend message (empty keeps existing): " "Commit message: ")
                (lambda (input)
                  (report (if amend?
                           (backend-amend b (or input "") (hash))
                           (backend-commit b (or input "") (hash)))))))))))))

(define (network-command op args)
  (let ([b (resolve-backend)])
    (when b
      (if (not (backend-supports? b op))
        (unsupported-status b op)
        (let ([opts (if (pair? args) (hash 'remote (to-string (car args))) (hash))])
          (report (backend-mutate b op (list opts))))))))

;; Join command arguments into a message string, or #f when there are none.
(define (args->message args)
  (if (null? args)
    #f
    (let ([s (string-trim (string-join (map to-string args) " "))])
      (if (string=? s "") #f s))))

;; The current file as a whole-file operand spec, located within `kinds`, or #f
;; when the focused file is not a changed file in those sections.
(define (current-file-operand b kinds)
  (let ([path (current-file-path)])
    (if (not path)
      #f
      (let* ([rel (rel-path (backend-root b) path)]
             [found (locate-file b rel kinds)])
        (if found
          (hash 'section-kind (car found)
            'path
            (file-item-path (cdr found))
            'status-code
            (file-item-status-code (cdr found))
            'scope
            'file)
          #f)))))

(define (rel-path root abs)
  (let ([prefix (string-append root "/")])
    (if (string-prefix? prefix abs) (string-drop abs (string-length prefix)) abs)))

;; (cons section-kind file-item) for `rel` within a section whose kind is in
;; `kinds`, or #f.
(define (locate-file b rel kinds)
  (let loop ([secs (status-sections (backend-status b))])
    (if (null? secs)
      #f
      (let ([sec (car secs)])
        (if (member (section-kind sec) kinds)
          (let ([fi (find-file-item (section-items sec) rel)])
            (if fi (cons (section-kind sec) fi) (loop (cdr secs))))
          (loop (cdr secs)))))))

(define (find-file-item items rel)
  (cond
    [(null? items) #f]
    [(and (file-item? (car items)) (string=? (file-item-path (car items)) rel)) (car items)]
    [else (find-file-item (cdr items) rel)]))

;;; Formatting helpers ;;;

;; Reconstruct displayable diff lines from parsed hunks (for the diff overlay).
(define (hunks->lines hunks)
  (apply append
    (map (lambda (h)
          (cons (hunk-header h)
            (map diff-line->string (hunk-lines h))))
      hunks)))

(define (diff-line->string dl)
  (let ([k (diff-line-kind dl)]
        [t (diff-line-text dl)])
    (cond
      [(eq? k 'add) (string-append "+" t)]
      [(eq? k 'del) (string-append "-" t)]
      [(eq? k 'meta) t]
      [else (string-append " " t)])))
