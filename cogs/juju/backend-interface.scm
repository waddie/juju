;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; backend-interface.scm

(provide make-backend
  backend?
  backend-name
  backend-root
  backend-capabilities
  backend-supports?
  unsupported-message
  discard-confirm-note
  ;; read operations (blame is a 'blame query op, not a named field)
  backend-status
  backend-diff
  backend-log
  backend-show
  ;; read-only listings (dispatched through the backend's query-fn)
  backend-query
  backend-refs
  backend-remotes
  backend-oplog
  backend-reflog
  backend-worktrees
  backend-submodules
  ;; mutating operations (dispatched through the backend's mutate-fn)
  backend-mutate
  backend-stage
  backend-unstage
  backend-discard
  backend-stage-all
  backend-unstage-all
  backend-commit
  backend-amend
  backend-commit-fixup
  backend-extend
  backend-push
  backend-pull
  backend-fetch
  backend-undo
  backend-redo
  ;; history rewriting (phase 3)
  backend-rebase
  backend-cherry-pick
  backend-revert
  backend-reset
  backend-squash
  backend-split
  backend-abandon
  backend-describe
  backend-rebase-interactive
  backend-rebase-continue
  backend-rebase-abort
  backend-rebase-skip
  ;; branch/bookmark + stash (phase 4)
  backend-switch
  backend-edit
  backend-branch-create
  backend-branch-set
  backend-branch-rename
  backend-branch-delete
  backend-set-upstream
  backend-stash
  backend-stash-pop
  backend-stash-apply
  backend-stash-drop
  ;; result helpers
  make-result
  ok-result
  err-result
  unsupported-result
  result-ok?
  result-message
  result-raw)

;; name: 'git | 'jj
;; root: workspace root path (string)
;; capabilities: list of feature symbols this backend's model supports
;; *-fn: operation implementations, each taking the backend as first argument
;; mutate-fn: (backend op-symbol args-list) -> result hash; #f when the backend
;;            implements no mutations yet (phase 1 read-only stubs).
;; query-fn: (backend op-symbol args-list) -> arbitrary value (usually a list of
;;           display lines); the read-only counterpart to mutate-fn for reads
;;           that do not fit the four core ones (refs, op log, reflog, remotes,
;;           blame). #f when the backend provides none.
(struct backend
  (name root capabilities status-fn diff-fn log-fn show-fn mutate-fn query-fn)
  #:transparent)

;;@doc
;; Build a backend value. The read fns are required; `mutate-fn` and `query-fn`
;; are optional positional extras (in that order). A missing `mutate-fn` makes
;; every mutation report "not supported"; a missing `query-fn` makes every
;; listing empty.
(define (make-backend name root capabilities status-fn diff-fn log-fn show-fn . opt)
  (let ([mutate-fn (if (and (pair? opt) (car opt)) (car opt) #f)]
        [query-fn (if (and (pair? opt) (pair? (cdr opt)) (cadr opt)) (cadr opt) #f)])
    (backend name root capabilities
      status-fn
      diff-fn
      log-fn
      show-fn
      mutate-fn
      query-fn)))

;;@doc
;; #t when `cap` is in the backend's capability set.
(define (backend-supports? b cap)
  (and (member cap (backend-capabilities b)) #t))

;;@doc
;; The standard message for an operation `cap` the backend `b` does not support,
;; e.g. "juju: stage is not supported by jj". Used by both the status view and
;; the typed commands so the wording is identical everywhere.
(define (unsupported-message b cap)
  (string-append "juju: " (symbol->string cap)
    " is not supported by "
    (symbol->string (backend-name b))))

;;@doc
;; The warning appended to a discard confirmation prompt. A backend with a
;; first-class operation log ('oplog, i.e. jj) can reverse a discard with undo;
;; elsewhere it is permanent. Shared so the typed command and the status view
;; word it identically.
(define (discard-confirm-note b)
  (if (backend-supports? b 'oplog)
    "(undo reverses it)"
    "This cannot be undone"))

;;; Read operations ;;;

;;@doc
;; Produce the backend's `status` struct.
(define (backend-status b) ((backend-status-fn b) b))

;;@doc
;; Diff for `target` (a target hash, see backend-git) -> list of hunk.
(define (backend-diff b target) ((backend-diff-fn b) b target))

;;@doc
;; Log for `revset`/`range` with `opts` -> list of commit-record.
(define (backend-log b revset opts) ((backend-log-fn b) b revset opts))

;;@doc
;; Show a single rev -> hash with 'commit and 'hunks.
(define (backend-show b rev) ((backend-show-fn b) b rev))

;;; Read-only listings ;;;
;;;
;;; Dispatched through the backend's own query-fn, the read counterpart to
;;; backend-mutate. Each returns a list of display lines (or '() when the backend
;;; provides nothing for that op), so a text view can show them without parsing.

;;@doc
;; Invoke read-only listing `op` with `args` (a list) -> value (usually lines).
(define (backend-query b op args)
  (let ([qfn (backend-query-fn b)])
    (if qfn (qfn b op args) '())))

;; refs: branches/tags/remotes (git) or bookmarks (jj). oplog/reflog: the
;; backend's operation history (jj op log / git reflog). remotes: configured
;; remotes. A backend lacking a listing returns '().
(define (backend-refs b) (backend-query b 'refs '()))
(define (backend-remotes b) (backend-query b 'remotes '()))
(define (backend-oplog b) (backend-query b 'oplog '()))
(define (backend-reflog b) (backend-query b 'reflog '()))
;; worktrees: git worktrees / jj workspaces. submodules: git only ('() under jj).
(define (backend-worktrees b) (backend-query b 'worktrees '()))
(define (backend-submodules b) (backend-query b 'submodules '()))

;;; Mutating operations ;;;
;;;
;;; Dispatched through the backend's own mutate-fn. The wrappers give command
;;; code a stable, named surface; the backend decides how each op maps to its
;;; VCS. A backend without a mutate-fn (or that returns #f for an op) yields an
;;; unsupported-result, so callers report it uniformly.

;;@doc
;; Invoke mutating operation `op` with `args` (a list) -> result hash.
(define (backend-mutate b op args)
  (let ([mfn (backend-mutate-fn b)])
    (if mfn
      (let ([r (mfn b op args)])
        (if r r (unsupported-result op)))
      (unsupported-result op))))

(define (backend-stage b sel) (backend-mutate b 'stage (list sel)))
(define (backend-unstage b sel) (backend-mutate b 'unstage (list sel)))
(define (backend-discard b sel) (backend-mutate b 'discard (list sel)))
(define (backend-stage-all b) (backend-mutate b 'stage-all '()))
(define (backend-unstage-all b) (backend-mutate b 'unstage-all '()))
(define (backend-commit b message opts) (backend-mutate b 'commit (list message opts)))
(define (backend-amend b message opts) (backend-mutate b 'amend (list message opts)))

;; commit-fixup: record a fixup! commit against `rev` (git) / squash selected
;; changes into `rev` (jj). extend: fold the working changes into the most recent
;; commit without changing its message (git amend --no-edit / jj squash into @-).
(define (backend-commit-fixup b rev opts) (backend-mutate b 'commit-fixup (list rev opts)))
(define (backend-extend b opts) (backend-mutate b 'extend (list opts)))

(define (backend-push b opts) (backend-mutate b 'push (list opts)))
(define (backend-pull b opts) (backend-mutate b 'pull (list opts)))
(define (backend-fetch b opts) (backend-mutate b 'fetch (list opts)))
(define (backend-undo b) (backend-mutate b 'undo '()))
(define (backend-redo b) (backend-mutate b 'redo '()))

;; History rewriting. `opts` is an option hash each backend reads as it needs:
;;   rebase   'onto (dest rev), 'autosquash (#t, git only)
;;   reset    `mode` is 'soft | 'mixed | 'hard; `rev` the target (#f -> HEAD)
;;   squash   'into / 'from (rev), 'message
;; cherry-pick/revert/abandon/describe take their primary operand explicitly.
;; A backend lacking the capability returns an unsupported-result (its mutate-fn
;; returns #f for the op), reported uniformly by callers.
(define (backend-rebase b opts) (backend-mutate b 'rebase (list opts)))
(define (backend-cherry-pick b rev opts) (backend-mutate b 'cherry-pick (list rev opts)))
(define (backend-revert b rev opts) (backend-mutate b 'revert (list rev opts)))
(define (backend-reset b mode rev opts) (backend-mutate b 'reset (list mode rev opts)))
(define (backend-squash b opts) (backend-mutate b 'squash (list opts)))
(define (backend-split b paths opts) (backend-mutate b 'split (list paths opts)))
(define (backend-abandon b rev opts) (backend-mutate b 'abandon (list rev opts)))
(define (backend-describe b message opts) (backend-mutate b 'describe (list message opts)))

;; Interactive rebase. `plan` is a backend-neutral hash carrying the ordered
;; todo-entry list ('entries) and the base revision ('base); each backend
;; projects it its own way (git writes a todo file; jj runs a step sequence). A
;; git rebase may pause (an `edit` entry or a conflict); the continue/abort/skip
;; ops drive it from there. jj never pauses, so it leaves those unsupported.
(define (backend-rebase-interactive b plan) (backend-mutate b 'rebase-interactive (list plan)))
(define (backend-rebase-continue b) (backend-mutate b 'rebase-continue '()))
(define (backend-rebase-abort b) (backend-mutate b 'rebase-abort '()))
(define (backend-rebase-skip b) (backend-mutate b 'rebase-skip '()))

;; Branch/bookmark management and git stash. A named ref is a branch (git) or a
;; bookmark (jj); both back the same op symbols, so command code never branches
;; on the backend. `switch` moves to a ref/commit (git checkout / jj new).
;; `branch-set` create-or-moves an existing ref to a revision (git branch -f / jj
;; bookmark set), the counterpart to `branch-create` which only creates.
;; set-upstream and the stash family are git-only (jj has no index/stash and
;; tracks remotes differently); the capability set gates them.
;; `edit` makes an existing change the working copy in place (jj edit); git has
;; no safe equivalent, so only jj advertises the capability.
(define (backend-switch b rev) (backend-mutate b 'switch (list rev)))
(define (backend-edit b rev) (backend-mutate b 'edit (list rev)))
(define (backend-branch-create b name rev) (backend-mutate b 'branch-create (list name rev)))
(define (backend-branch-set b name rev) (backend-mutate b 'branch-set (list name rev)))
(define (backend-branch-rename b old new) (backend-mutate b 'branch-rename (list old new)))
(define (backend-branch-delete b name opts) (backend-mutate b 'branch-delete (list name opts)))
(define (backend-set-upstream b name upstream) (backend-mutate b 'set-upstream (list name upstream)))
(define (backend-stash b opts) (backend-mutate b 'stash (list opts)))
(define (backend-stash-pop b ref) (backend-mutate b 'stash-pop (list ref)))
(define (backend-stash-apply b ref) (backend-mutate b 'stash-apply (list ref)))
(define (backend-stash-drop b ref) (backend-mutate b 'stash-drop (list ref)))

;;; Result helpers ;;;
;;;
;;; Every mutating op returns a uniform result hash so command code reports
;;; success/failure identically across backends.

;;@doc
;; Build a result hash: 'ok bool, 'message str, 'raw process-hash-or-#f.
(define (make-result ok? message raw)
  (hash 'ok ok? 'message message 'raw raw))

(define (ok-result message raw) (make-result #t message raw))

(define (err-result message raw) (make-result #f message raw))

;;@doc
;; Result reported when a backend does not implement `op` at all.
(define (unsupported-result op)
  (make-result #f
    (string-append "not supported: " (symbol->string op))
    #f))

(define (result-ok? r) (hash-ref r 'ok))
(define (result-message r) (hash-ref r 'message))
(define (result-raw r) (hash-ref r 'raw))
