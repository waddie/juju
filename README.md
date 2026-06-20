# juju

A `git`/`jj` interface for the [Helix](https://helix-editor.com) editor.

One interface, two backends: Git and [Jujutsu](https://github.com/jj-vcs/jj).

`juju` shells out to the `git` and `jj` binaries; there is no FFI.

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

| Command                   | Action                                           |
| ------------------------- | ------------------------------------------------ |
| `:juju` / `:juju-status`  | Open the status view                             |
| `:juju-log`               | Recent commits / changes                         |
| `:juju-diff`              | Working-copy diff                                |
| `:juju-blame`             | Blame the current file                           |
| `:juju-backend git\|jj`   | Set or report the backend for this workspace     |
| `:juju-stage`             | Stage the current file (`git`)                   |
| `:juju-unstage`           | Unstage the current file (`git`)                 |
| `:juju-discard`           | Discard the current file (confirms first)        |
| `:juju-stage-all`         | Stage every change (`git`)                       |
| `:juju-unstage-all`       | Unstage every change (`git`)                     |
| `:juju-commit [msg]`      | Commit (prompts when no message given)           |
| `:juju-amend [msg]`       | Amend HEAD / re-describe `@` (`jj`)              |
| `:juju-extend`            | Fold changes into the latest commit              |
| `:juju-commit-fixup rev`  | Record a fixup! (`git`) / squash into (`jj`)     |
| `:juju-fetch [remote]`    | Fetch                                            |
| `:juju-pull [remote]`     | Pull / integrate                                 |
| `:juju-push [remote]`     | Push                                             |
| `:juju-undo`              | Undo the last operation (jj; reflog on git)      |
| `:juju-redo`              | Redo the last undone operation (`jj`)            |
| `:juju-rebase [-as] ref`  | Rebase onto a ref (`--autosquash`) (git/jj)      |
| `:juju-cherry-pick rev`   | Cherry-pick a commit (`git`)                     |
| `:juju-revert rev`        | Revert a commit (git/jj)                         |
| `:juju-reset [mode] rev`  | Reset HEAD: soft / mixed / hard (`git`)          |
| `:juju-squash [rev]`      | Fold `@` into its parent or a rev (`jj`)         |
| `:juju-split path...`     | Split files out of `@` into a new change (`jj`)  |
| `:juju-abandon [rev]`     | Abandon a change, `@` when omitted (`jj`)        |
| `:juju-describe [msg]`    | Set `@`’s description (`jj`)                     |
| `:juju-switch [target]`   | Switch to a branch/bookmark/commit               |
| `:juju-branch-create n`   | Create a branch/bookmark (optional rev)          |
| `:juju-branch-rename`     | Rename a branch/bookmark: `<old> <new>`          |
| `:juju-branch-delete n`   | Delete a branch/bookmark                         |
| `:juju-set-upstream`      | Set a branch’s upstream: `<branch> <up>` (`git`) |
| `:juju-stash [msg]`       | Stash the working changes (`git`)                |
| `:juju-stash-pop [ref]`   | Pop a stash, latest when omitted (`git`)         |
| `:juju-stash-apply [ref]` | Apply a stash without dropping it (`git`)        |
| `:juju-stash-drop [ref]`  | Drop a stash (`git`)                             |
| `:juju-refs`              | List branches/tags/remotes, or bookmarks         |
| `:juju-remote`            | List configured remotes                          |
| `:juju-oplog`             | Show the operation log (`jj`)                    |
| `:juju-reflog`            | Show the reflog (`git`)                          |
| `:juju-worktree`          | List worktrees (`git`) / workspaces (`jj`)       |
| `:juju-submodule`         | List submodule status (`git`)                    |
| `:juju-run args...`       | Run a raw backend line in root, show output      |
| `:juju-rebase-menu`       | Transient: rebase (switch `--autosquash`)        |
| `:juju-remote-menu`       | Transient: fetch / pull / push                   |
| `:juju-branch-menu`       | Transient: create / switch / rename / delete     |
| `:juju-annotate`          | Alias of `:juju-blame`                           |
| `:juju-reword [msg]`      | Alias of `:juju-describe`                        |
| `:juju-drop [rev]`        | Alias of `:juju-abandon`                         |
| `:juju-bookmark-create`   | Alias of `:juju-branch-create`                   |
| `:juju-bookmark-rename`   | Alias of `:juju-branch-rename`                   |
| `:juju-bookmark-delete`   | Alias of `:juju-branch-delete`                   |

The `:juju-stage` / `:juju-unstage` / `:juju-discard` typed commands act on the
whole current file. For hunk- or line-level granularity, use the status view,
where the action applies to whatever is selected.

## Status-view keys

| Key                  | Action                                                 |
| -------------------- | ------------------------------------------------------ |
| `j` / `k`, `↑` / `↓` | Move                                                   |
| `Ctrl-d` / `Ctrl-u`  | Page down / up                                         |
| `Home` / `End`       | First / last                                           |
| `Tab`                | Fold / unfold the section or file under the cursor     |
| `Enter`              | Visit a file / show a commit’s diff / fold a section   |
| `v`                  | Mark / unmark the current row for a multi-row action   |
| `s` / `u`            | Stage / unstage the selection                          |
| `x`                  | Discard files / drop stash / abandon commit (confirms) |
| `S` / `U`            | Stage all / unstage all                                |
| `c` / `a` / `e`      | Commit / amend / extend                                |
| `f` / `F` / `P`      | Fetch / pull / push                                    |
| `V` / `y` / `r`      | Revert / cherry-pick / rebase-onto the selected commit |
| `b`                  | Switch to the selected branch/bookmark/commit          |
| `p`                  | Pop the selected stash                                 |
| `z` / `Z`            | Undo / redo (`jj op log`; `git reflog`, best-effort)   |
| `?`                  | Key reference                                          |
| `g`                  | Refresh                                                |
| `q` / `Esc`          | Close                                                  |

Actions are selection-first: mark rows with `v` and the next action applies to
all of them; with nothing marked, it applies to the row under the cursor. The
granularity is whatever the selection covers, files, hunks, or individual diff
lines, so there is no separate file-vs-hunk-vs-region distinction. The same key
adapts to its operand: `x` discards file rows, drops a stash row, or abandons a
commit row. History keys (`V` / `y` / `r` / `b`) act on commit rows in the
recent, bookmark, and unpushed/unpulled sections. Keys for features a backend
lacks (staging under `jj`, stash under `jj`) are inert.

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
