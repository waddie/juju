;; juju example keybindings (opt-in).
;;
;; Copy into your init.scm and adapt.
;;
;; Most mutations are driven from inside the status view (`:juju`), where the
;; action keys (s/u/x, c/a/e, f/F/P, V/y/r/b on a commit, z/Z, v to mark) apply
;; to the current selection. The bindings below are the entry points and the
;; keyboard-free typed-command equivalents that act on the current file, a rev
;; argument, or the workspace.
;;
;; The `M` group opens transient popups (switches + actions) for the branch,
;; rebase, and remote flows.
;;
;; Commands that strictly require a typed argument are deliberately not bound: a
;; keybinding sends the whole command with no way to enter an argument, so type
;; those in full (`:juju-run`, `:juju-cherry-pick`, `:juju-revert`,
;; `:juju-rebase`, `:juju-branch-rename`, `:juju-branch-delete`,
;; `:juju-set-upstream`). The `M` menus and the status-view action keys cover the
;; common cases by prompting or acting on the selection. Bound commands either
;; take no argument, prompt when one is omitted, or have a sensible no-arg default.

(keymap (global)
  (normal
    (space (J
            (g ":juju")
            (l ":juju-log")
            (d ":juju-diff")
            (b ":juju-blame")
            (s ":juju-stage")
            (u ":juju-unstage")
            (x ":juju-discard")
            (c ":juju-commit")
            (a ":juju-amend")
            (e ":juju-extend")
            (f ":juju-fetch")
            (F ":juju-pull")
            (P ":juju-push")
            ;; history rewriting (capabilities differ by backend)
            (r
              (R ":juju-reset")
              (s ":juju-squash")
              (p ":juju-split")
              (a ":juju-abandon")
              (d ":juju-describe")
              (z ":juju-undo")
              (Z ":juju-redo"))
            ;; branches / bookmarks
            (B
              (s ":juju-switch")
              (c ":juju-branch-create"))
            ;; git stash
            (S
              (s ":juju-stash")
              (p ":juju-stash-pop")
              (a ":juju-stash-apply")
              (d ":juju-stash-drop"))
            ;; transient menus (popup of switches + actions)
            (M
              (b ":juju-branch-menu")
              (r ":juju-rebase-menu")
              (R ":juju-remote-menu"))
            ;; extra listings
            (w ":juju-worktree")
            (W ":juju-submodule")
            (i ":juju-refs")
            (m ":juju-remote")
            (o ":juju-oplog")
            (L ":juju-reflog"))))
  (select
    (space (J
            (g ":juju")
            (l ":juju-log")
            (d ":juju-diff")
            (b ":juju-blame")
            (s ":juju-stage")
            (u ":juju-unstage")
            (x ":juju-discard")
            (c ":juju-commit")
            (a ":juju-amend")
            (e ":juju-extend")
            (f ":juju-fetch")
            (F ":juju-pull")
            (P ":juju-push")
            ;; history rewriting (capabilities differ by backend)
            (r
              (R ":juju-reset")
              (s ":juju-squash")
              (p ":juju-split")
              (a ":juju-abandon")
              (d ":juju-describe")
              (z ":juju-undo")
              (Z ":juju-redo"))
            ;; branches / bookmarks
            (B
              (s ":juju-switch")
              (c ":juju-branch-create"))
            ;; git stash
            (S
              (s ":juju-stash")
              (p ":juju-stash-pop")
              (a ":juju-stash-apply")
              (d ":juju-stash-drop"))
            ;; transient menus (popup of switches + actions)
            (M
              (b ":juju-branch-menu")
              (r ":juju-rebase-menu")
              (R ":juju-remote-menu"))
            ;; extra listings
            (w ":juju-worktree")
            (W ":juju-submodule")
            (i ":juju-refs")
            (m ":juju-remote")
            (o ":juju-oplog")
            (L ":juju-reflog")))))
