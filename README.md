# Juju

A `git`/`jj` interface for the [Helix](https://helix-editor.com) editor.

One interface, two backends: Git and [Jujutsu](https://github.com/jj-vcs/jj).

`juju` shells out to the `git` and `jj` binaries; there is no FFI.

This is very early alpha software. Use at own risk.

## Demo

![Interactive rebase of a git repo with Juju](https://github.com/waddie/juju/blob/main/images/juju.gif?raw=true)

## Install

### Forge (Steel package manager)

```
forge pkg install --git https://github.com/waddie/juju
```

Then add to `~/.config/helix/init.scm`:

```scheme
(require "juju/juju.scm")
```

### Manual

From a checkout:

```
./install.sh
```

This copies the plugin into `~/.steel/cogs/juju/` (the same place forge uses).
Add the `require` line above to your `init.scm` and restart Helix.

## Commands

| Command                           | Action                                                                            |
| --------------------------------- | --------------------------------------------------------------------------------- |
| `:juju` / `:juju-status`          | Open the status view                                                              |
| `:juju-log`                       | Recent commits / changes, with actions on the change under the cursor             |
| `:juju-diff`                      | Working-copy diff                                                                 |
| `:juju-blame`                     | Blame the current file interactively (show, chase revisions)                      |
| `:juju-backend git\|jj`           | Set or report the backend for this workspace                                      |
| `:juju-stage`                     | Stage the current file (`git`)                                                    |
| `:juju-unstage`                   | Unstage the current file (`git`)                                                  |
| `:juju-discard`                   | Discard the current file (confirms first)                                         |
| `:juju-stage-all`                 | Stage every change (`git`)                                                        |
| `:juju-unstage-all`               | Unstage every change (`git`)                                                      |
| `:juju-commit [msg]`              | Commit (prompts when no message given)                                            |
| `:juju-amend [msg]`               | Amend HEAD / re-describe `@` (`jj`)                                               |
| `:juju-extend`                    | Fold changes into the latest commit                                               |
| `:juju-commit-fixup rev`          | Record a fixup! (`git`) / squash into (`jj`)                                      |
| `:juju-fetch [remote]`            | Fetch                                                                             |
| `:juju-pull [remote]`             | Pull / integrate                                                                  |
| `:juju-push [remote]`             | Push                                                                              |
| `:juju-undo`                      | Undo the last operation (jj; reflog on git)                                       |
| `:juju-redo`                      | Redo the last undone operation (`jj`)                                             |
| `:juju-rebase [-as] ref`          | Rebase onto a ref (`--autosquash`) (git/jj)                                       |
| `:juju-rebase-interactive [base]` | Open the interactive rebase editor (git/jj)                                       |
| `:juju-rebase-continue`           | Continue a paused rebase (`git`)                                                  |
| `:juju-rebase-abort`              | Abort a paused rebase, restore the tip (`git`)                                    |
| `:juju-rebase-skip`               | Skip the current commit in a paused rebase (`git`)                                |
| `:juju-cherry-pick rev`           | Cherry-pick a commit (`git`)                                                      |
| `:juju-revert rev`                | Revert a commit (git/jj)                                                          |
| `:juju-reset [mode] rev`          | Reset HEAD: soft / mixed / hard (`git`)                                           |
| `:juju-squash [rev]`              | Fold `@` into its parent or a rev (`jj`)                                          |
| `:juju-split path...`             | Split files out of `@` into a new change (`jj`)                                   |
| `:juju-abandon [rev]`             | Abandon a change, `@` when omitted (`jj`)                                         |
| `:juju-describe [msg]`            | Set `@`’s description (`jj`)                                                      |
| `:juju-switch [target]`           | Switch to a branch/bookmark/commit                                                |
| `:juju-edit [rev]`                | Edit a change: make it the working copy (`jj`)                                    |
| `:juju-branch-create n`           | Create a branch/bookmark (optional rev)                                           |
| `:juju-branch-rename`             | Rename a branch/bookmark: `<old> <new>`                                           |
| `:juju-branch-delete n`           | Delete a branch/bookmark                                                          |
| `:juju-set-upstream`              | Set a branch’s upstream: `<branch> <up>` (`git`)                                  |
| `:juju-stash [msg]`               | Stash the working changes (`git`)                                                 |
| `:juju-stash-pop [ref]`           | Pop a stash, latest when omitted (`git`)                                          |
| `:juju-stash-apply [ref]`         | Apply a stash without dropping it (`git`)                                         |
| `:juju-stash-drop [ref]`          | Drop a stash (`git`)                                                              |
| `:juju-refs`                      | List branches/tags/remotes, or bookmarks                                          |
| `:juju-remote`                    | List configured remotes                                                           |
| `:juju-oplog`                     | Show the operation log (`jj`)                                                     |
| `:juju-reflog`                    | Show the reflog (`git`)                                                           |
| `:juju-worktree`                  | List worktrees (`git`) / workspaces (`jj`)                                        |
| `:juju-submodule`                 | List submodule status (`git`)                                                     |
| `:juju-run args...`               | Run a raw backend line in root, show output                                       |
| `:juju-dispatch`                  | Transient: top-level menu of the sub-menus                                        |
| `:juju-rebase-menu`               | Transient: rebase (`--autosquash`; interactive)                                   |
| `:juju-remote-menu`               | Transient: fetch / pull / push (`--prune`, `--rebase`, force-with-lease on `git`) |
| `:juju-branch-menu`               | Transient: create / switch / rename / delete                                      |
| `:juju-commit-menu`               | Transient: commit / amend (`--no-verify`, `--signoff` on `git`)                   |
| `:juju-log-menu`                  | Transient: log with a `-n count` infix                                            |
| `:juju-annotate`                  | Alias of `:juju-blame`                                                            |
| `:juju-reword [msg]`              | Alias of `:juju-describe`                                                         |
| `:juju-drop [rev]`                | Alias of `:juju-abandon`                                                          |
| `:juju-bookmark-create`           | Alias of `:juju-branch-create`                                                    |
| `:juju-bookmark-rename`           | Alias of `:juju-branch-rename`                                                    |
| `:juju-bookmark-delete`           | Alias of `:juju-branch-delete`                                                    |

The `:juju-stage` / `:juju-unstage` / `:juju-discard` typed commands act on the
whole current file. For hunk- or line-level granularity, use the status view,
where the action applies to whatever is selected.

## Status-view keys

| Key                  | Action                                                    |
| -------------------- | --------------------------------------------------------- |
| `j` / `k`, `↑` / `↓` | Move                                                      |
| `Ctrl-d` / `Ctrl-u`  | Page down / up                                            |
| `Home` / `End`       | First / last                                              |
| `}` / `{`            | Next / previous section                                   |
| `^`                  | Jump to the enclosing section header                      |
| `/`                  | Search; `n` / `N` jump to the next / previous match       |
| `Tab`                | Fold / unfold the section or file under the cursor        |
| `Enter`              | Visit a file / show a commit’s diff / fold a section      |
| `v`                  | Mark / unmark the current row for a multi-row action      |
| `s` / `u`            | Stage / unstage the selection                             |
| `x`                  | Discard files / drop stash / abandon commit (confirms)    |
| `S` / `U`            | Stage all / unstage all                                   |
| `c` / `a` / `e`      | Commit / amend / extend (`e` on a commit row: edit, `jj`) |
| `f` / `F` / `P`      | Fetch / pull / push                                       |
| `V` / `y` / `r`      | Revert / cherry-pick / rebase-onto the selected commit    |
| `i`                  | Interactive rebase from the selected commit to the tip    |
| `b`                  | Switch to the selected branch/bookmark/commit             |
| `p`                  | Pop the selected stash                                    |
| `z` / `Z`            | Undo / redo (`jj op log`; `git reflog`, best-effort)      |
| `?`                  | Key reference                                             |
| `g`                  | Refresh                                                   |
| `q` / `Esc`          | Close                                                     |

Actions are selection-first: mark rows with `v` and the next action applies to
all of them; with nothing marked, it applies to the row under the cursor. The
granularity is whatever the selection covers, files, hunks, or individual diff
lines, so there is no separate file-vs-hunk-vs-region distinction. The same key
adapts to its operand: `x` discards file rows, drops a stash row, or abandons a
commit row, and `e` extends everywhere except on a commit row under `jj`, where
it edits that change. History keys (`V` / `y` / `r` / `b`) act on commit rows in
the recent, bookmark, and unpushed/unpulled sections. Keys for features a
backend lacks (staging under `jj`, stash under `jj`) are inert.

## Log view

`:juju-log` (or `:juju-log-menu` for a `-n count` infix) opens a floating log of
recent commits/changes. Each row is a commit, and the commit actions from the
status view work on the row under the cursor.

| Key                  | Action                                               |
| -------------------- | ---------------------------------------------------- |
| `j` / `k`, `↑` / `↓` | Move the cursor                                      |
| `Ctrl-u` / `Ctrl-d`  | Move ten lines                                       |
| `Enter`              | Show the commit's diff                               |
| `e`                  | Edit: make the change the working copy (`jj`)        |
| `n` / `b`            | New change on the commit (`jj new` / `git checkout`) |
| `V` / `y` / `r`      | Revert / cherry-pick / rebase-onto the commit        |
| `g`                  | Refresh                                              |
| `?`                  | Key reference                                        |
| `q` / `Esc`          | Quit                                                 |

Mutations refresh the log in place (the cursor stays on the same change) and
any open status view. `jj` refuses to edit an immutable commit; the error shows
on the log's status line.

## Interactive rebase editor

`:juju-rebase-interactive [base]` (or `i` on a commit in the status view) opens a
floating editor listing the commits to be rebased, oldest at the top. Reorder
them and assign each an action, then `Enter` to apply or `q` to cancel. Without a
`base` it edits the commits above the upstream (`git`) or the mutable ancestors of
`@` (`jj`); launched from a commit it edits from that commit to the tip.

| Key                  | Action                                                    |
| -------------------- | --------------------------------------------------------- |
| `j` / `k`, `↑` / `↓` | Move the cursor                                           |
| `J` / `K`            | Move the commit under the cursor down / up                |
| `p`                  | pick (keep as-is)                                         |
| `r`                  | reword (prompts for a new message on apply)               |
| `e`                  | edit (pause to amend; `git` only)                         |
| `s`                  | squash (fold into the commit above, keep both messages)   |
| `f`                  | fixup (fold into the commit above, drop its message)      |
| `d`                  | drop (remove the commit)                                  |
| `v`                  | Mark / unmark a row (an action then applies to all marks) |
| `Enter`              | Apply the plan                                            |
| `q` / `Esc`          | Cancel                                                    |

The same plan drives both backends. On `git` it becomes a `rebase -i` todo; an
`edit` step or a conflict pauses the rebase, surfaced in the status header, and
`:juju-rebase-continue` / `-abort` / `-skip` drive it from there. On `jj` it
becomes a sequence of `abandon` / `squash` / `describe` / `rebase` commands keyed
on change-ids; `jj` never pauses, and the whole batch is reversible with
`:juju-undo`. A reword whose prompt is left blank keeps the commit unchanged.

## Interactive blame

`:juju-blame` (alias `:juju-annotate`) opens a floating blame of the current
file. Every line carries the commit (`git`) or change (`jj`) that introduced
it; the first line of each run is coloured to mark the boundary.

| Key                  | Action                                            |
| -------------------- | ------------------------------------------------- |
| `j` / `k`, `↑` / `↓` | Move the cursor                                   |
| `Ctrl-u` / `Ctrl-d`  | Move ten lines                                    |
| `Enter`              | Show the cursor line's commit                     |
| `l`                  | Chase: reblame at the parent of the line's commit |
| `h` / `Backspace`    | Go back to the previous blame                     |
| `q` / `Esc`          | Quit                                              |

Chasing from a root commit's line (or a line whose file did not exist at the
parent) reports and stays put. Under `git`, uncommitted lines blame to the
all-zero id and have no commit to show.

## Keybindings

`juju` ships an example keymap rather than forcing one. See
`keybindings-example.scm` for a `space.J` menu to copy into your `init.scm`.

## Configuration

Settings are plain functions you call from `init.scm` after requiring juju. All
have (hopefully) sensible defaults; set only what you want to change.

| Setter                              | Default   | Effect                                      |
| ----------------------------------- | --------- | ------------------------------------------- |
| `(set-juju-recent-count! n)`        | `10`      | Entries in the Recent section               |
| `(set-juju-log-count! n)`           | `50`      | Entries `:juju-log` lists                   |
| `(set-juju-colocated-default! sym)` | `'jj`     | Backend for a colocated repo (`'git`/`'jj`) |
| `(set-juju-auto-refresh! bool)`     | `#t`      | Typed commands refresh an open view         |
| `(set-juju-warn-colocated! bool)`   | `#t`      | Show the colocated `git`→`jj` desync note   |
| `(set-juju-overlay-scale! pct)`     | `90`      | Overlay size, percent of the terminal       |
| `(set-juju-section-color! sym)`     | `'yellow` | Section-header colour                       |

The colour symbol is one of `'yellow 'green 'cyan 'blue 'magenta 'red 'white`
(an unknown name falls back to the theme default).

## Requirements

- Helix with the Steel plugin system.
- `git` and/or `jj` on `PATH`. `jj` parsing assumes a recent `jj` (0.42+),
  pinned via explicit templates and `--git` diffs.

## Development

Pure parsers (diff, porcelain status, `jj` summary, log records) are
unit tested:

```
./tests/run.sh
```

## License

AGPL-3.0-or-later.

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.
