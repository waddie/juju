;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; juju.scm - entry module
;;;
;;; A git/jj interface for Helix.
;;;
;;; Add
;;;
;;;   (require "juju/juju.scm")
;;;
;;; to your init.scm (forge install) or `(require "juju.scm")` if installed via
;;; install.sh. See keybindings-example.scm for a suggested keymap.

(require (prefix-in cmd. "cogs/juju/commands.scm"))
(require "cogs/juju/config.scm")

(provide juju
  ;; configuration setters/getters (see config.scm)
  juju-recent-count
  set-juju-recent-count!
  juju-log-count
  set-juju-log-count!
  juju-colocated-default
  set-juju-colocated-default!
  juju-auto-refresh
  set-juju-auto-refresh!
  juju-warn-colocated
  set-juju-warn-colocated!
  juju-overlay-scale
  set-juju-overlay-scale!
  juju-section-color
  set-juju-section-color!
  juju-status
  juju-log
  juju-diff
  juju-blame
  juju-backend
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
  juju-rebase
  juju-cherry-pick
  juju-revert
  juju-reset
  juju-squash
  juju-split
  juju-abandon
  juju-describe
  juju-switch
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
  juju-run
  juju-rebase-menu
  juju-remote-menu
  juju-branch-menu
  juju-annotate
  juju-reword
  juju-drop
  juju-bookmark-create
  juju-bookmark-set
  juju-bookmark-move
  juju-bookmark-rename
  juju-bookmark-delete)

;;@doc
;; Open the juju status view for the current workspace (git or jj, auto-detected).
(define (juju) (cmd.juju-status))

;;@doc
;; Open the juju status view for the current workspace (git or jj, auto-detected).
(define (juju-status) (cmd.juju-status))

;;@doc
;; Show the recent commit/change log for the current workspace.
(define (juju-log) (cmd.juju-log))

;;@doc
;; Show the working-copy diff (changes against HEAD / the working-copy commit).
(define (juju-diff) (cmd.juju-diff))

;;@doc
;; Blame the current file.
(define (juju-blame) (cmd.juju-blame))

;;@doc
;; Set or report the backend for a colocated workspace: :juju-backend git|jj.
(define (juju-backend . args) (apply cmd.juju-set-backend args))

;;; Mutations ;;;
;;;
;;; Keyboard-free equivalents of the status-view actions. stage/unstage/discard
;;; act on the current file; the rest act on the workspace. They run
;;; synchronously and echo the outcome.

;;@doc
;; Stage the current file's changes (git only).
(define (juju-stage) (cmd.juju-stage))

;;@doc
;; Unstage the current file's changes (git only).
(define (juju-unstage) (cmd.juju-unstage))

;;@doc
;; Discard the current file's changes (confirms first).
(define (juju-discard) (cmd.juju-discard))

;;@doc
;; Stage every change in the workspace (git only).
(define (juju-stage-all) (cmd.juju-stage-all))

;;@doc
;; Unstage every change in the workspace (git only).
(define (juju-unstage-all) (cmd.juju-unstage-all))

;;@doc
;; Commit: :juju-commit [message]. Prompts when no message is given.
(define (juju-commit . args) (apply cmd.juju-commit args))

;;@doc
;; Amend the latest commit: :juju-amend [message].
(define (juju-amend . args) (apply cmd.juju-amend args))

;;@doc
;; Record a fixup!/squash against a rev: :juju-commit-fixup <rev>.
(define (juju-commit-fixup . args) (apply cmd.juju-commit-fixup args))

;;@doc
;; Fold working changes into the latest commit, message unchanged.
(define (juju-extend) (cmd.juju-extend))

;;@doc
;; Fetch from a remote: :juju-fetch [remote].
(define (juju-fetch . args) (apply cmd.juju-fetch args))

;;@doc
;; Pull from a remote: :juju-pull [remote].
(define (juju-pull . args) (apply cmd.juju-pull args))

;;@doc
;; Push to a remote: :juju-push [remote].
(define (juju-push . args) (apply cmd.juju-push args))

;;@doc
;; Undo the last operation (jj only for now; git undo is not yet supported).
(define (juju-undo) (cmd.juju-undo))

;;@doc
;; Redo the last undone operation (jj only).
(define (juju-redo) (cmd.juju-redo))

;;; History rewriting ;;;
;;;
;;; Capabilities differ by backend: rebase/cherry-pick/revert/reset on git;
;;; squash/split/abandon/describe (plus rebase/revert) on jj. A command the
;;; active backend lacks reports "not supported".

;;@doc
;; Rebase onto a ref: :juju-rebase [--autosquash] <ref>.
(define (juju-rebase . args) (apply cmd.juju-rebase args))

;;@doc
;; Cherry-pick a commit onto the current branch: :juju-cherry-pick <rev> (git).
(define (juju-cherry-pick . args) (apply cmd.juju-cherry-pick args))

;;@doc
;; Revert a commit: :juju-revert <rev>.
(define (juju-revert . args) (apply cmd.juju-revert args))

;;@doc
;; Reset HEAD: :juju-reset [soft|mixed|hard] [rev]. A hard reset confirms first.
(define (juju-reset . args) (apply cmd.juju-reset args))

;;@doc
;; Squash into a change: :juju-squash [rev] (jj; folds @ into its parent or rev).
(define (juju-squash . args) (apply cmd.juju-squash args))

;;@doc
;; Split files out of @ into a new change: :juju-split <path...> (jj).
(define (juju-split . args) (apply cmd.juju-split args))

;;@doc
;; Abandon a change: :juju-abandon [rev] (jj; @ when omitted).
(define (juju-abandon . args) (apply cmd.juju-abandon args))

;;@doc
;; Set @'s description: :juju-describe [message] (jj; prompts when omitted).
(define (juju-describe . args) (apply cmd.juju-describe args))

;;; Branch/bookmark, stash, and listings ;;;
;;;
;;; A named ref is a branch (git) or bookmark (jj); the create/rename/delete and
;;; switch commands serve both. set-upstream and the stash family are git-only.

;;@doc
;; Switch to a branch/bookmark or commit: :juju-switch <target> (prompts if omitted).
(define (juju-switch . args) (apply cmd.juju-switch args))

;;@doc
;; Create a branch/bookmark: :juju-branch-create <name> [rev].
(define (juju-branch-create . args) (apply cmd.juju-branch-create args))

;;@doc
;; Set/move a branch/bookmark to a rev: :juju-branch-set <name> [rev].
(define (juju-branch-set . args) (apply cmd.juju-branch-set args))

;;@doc
;; Rename a branch/bookmark: :juju-branch-rename <old> <new>.
(define (juju-branch-rename . args) (apply cmd.juju-branch-rename args))

;;@doc
;; Delete a branch/bookmark: :juju-branch-delete <name>.
(define (juju-branch-delete . args) (apply cmd.juju-branch-delete args))

;;@doc
;; Set a branch's upstream: :juju-set-upstream <branch> <upstream> (git).
(define (juju-set-upstream . args) (apply cmd.juju-set-upstream args))

;;@doc
;; Stash the working changes: :juju-stash [message] (git).
(define (juju-stash . args) (apply cmd.juju-stash args))

;;@doc
;; Pop a stash: :juju-stash-pop [stash@{N}] (git).
(define (juju-stash-pop . args) (apply cmd.juju-stash-pop args))

;;@doc
;; Apply a stash without dropping it: :juju-stash-apply [stash@{N}] (git).
(define (juju-stash-apply . args) (apply cmd.juju-stash-apply args))

;;@doc
;; Drop a stash: :juju-stash-drop [stash@{N}] (git).
(define (juju-stash-drop . args) (apply cmd.juju-stash-drop args))

;;@doc
;; Show all refs: branches/tags/remotes (git) or bookmarks (jj).
(define (juju-refs) (cmd.juju-refs))

;;@doc
;; Show the configured remotes.
(define (juju-remote) (cmd.juju-remote))

;;@doc
;; Show the operation log (jj).
(define (juju-oplog) (cmd.juju-oplog))

;;@doc
;; Show the reflog (git).
(define (juju-reflog) (cmd.juju-reflog))

;;@doc
;; Show the worktrees (git) or workspaces (jj).
(define (juju-worktree) (cmd.juju-worktree))

;;@doc
;; Show the submodule status (git).
(define (juju-submodule) (cmd.juju-submodule))

;;; Polish: escape hatch, transient menus, aliases ;;;
;;;
;;; :juju-run is the raw escape hatch. The *-menu commands pop a transient popup
;;; (switches + actions) for the common branch/remote/rebase flows. The aliases
;;; reach the same command under a backend's own term (annotate, reword, drop,
;;; bookmark).

;;@doc
;; Run a raw backend command line in the workspace root and show its output:
;; :juju-run <args...> (e.g. :juju-run log --oneline -5). Uses the active backend.
(define (juju-run . args) (apply cmd.juju-run args))

;;@doc
;; Open the rebase transient menu (switch --autosquash on git; rebase onto a ref).
(define (juju-rebase-menu) (cmd.juju-rebase-menu))

;;@doc
;; Open the remote transient menu (fetch / pull / push; force-with-lease on git).
(define (juju-remote-menu) (cmd.juju-remote-menu))

;;@doc
;; Open the branch/bookmark transient menu (create / switch / rename / delete).
(define (juju-branch-menu) (cmd.juju-branch-menu))

;;@doc
;; Blame the current file. Alias of :juju-blame (jj calls it annotate).
(define (juju-annotate) (cmd.juju-annotate))

;;@doc
;; Re-describe @: :juju-reword [message]. Alias of :juju-describe (jj).
(define (juju-reword . args) (apply cmd.juju-reword args))

;;@doc
;; Abandon a change: :juju-drop [rev]. Alias of :juju-abandon (jj).
(define (juju-drop . args) (apply cmd.juju-drop args))

;;@doc
;; Create a bookmark/branch: :juju-bookmark-create <name> [rev]. Alias of create.
(define (juju-bookmark-create . args) (apply cmd.juju-bookmark-create args))

;;@doc
;; Set/move a bookmark/branch to a rev: :juju-bookmark-set <name> [rev]. Alias of set.
(define (juju-bookmark-set . args) (apply cmd.juju-bookmark-set args))

;;@doc
;; Move a bookmark/branch to a rev: :juju-bookmark-move <name> [rev]. Alias of set.
(define (juju-bookmark-move . args) (apply cmd.juju-bookmark-move args))

;;@doc
;; Rename a bookmark/branch: :juju-bookmark-rename <old> <new>. Alias of rename.
(define (juju-bookmark-rename . args) (apply cmd.juju-bookmark-rename args))

;;@doc
;; Delete a bookmark/branch: :juju-bookmark-delete <name>. Alias of delete.
(define (juju-bookmark-delete . args) (apply cmd.juju-bookmark-delete args))
