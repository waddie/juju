;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; backend-jj.scm - the Jujutsu implementation of the backend interface
;;;
;;; jj has no index: the working copy is itself a commit (`@`). So there is no
;;; untracked/unstaged/staged split - just "changes in the working-copy commit".
;;; The jj status view shows the working-copy/parent header, the working-copy
;;; changes, bookmarks, recent changes, and the operation log (which Git has no
;;; equivalent for). Staging and stash sections are simply never produced.
;;;
;;; Output is read from explicit templates (`-T ...`) and `--git` diffs rather
;;; than default human formatting, which drifts across jj versions.

(require "process.scm")
(require "model.scm")
(require "diff.scm")
(require "string-utils.scm")
(require "backend-interface.scm")
(require "config.scm")

(provide make-jj-backend
  parse-jj-summary ; exported for unit tests
  parse-jj-conflict-line)

;; Features the jj model supports. No 'stage/'unstage/'stash: jj has no index.
;; 'redo/'split/'abandon/'oplog/'describe are jj-only and have no Git analogue.
(define jj-capabilities
  '(status diff log show blame oplog
    discard
    commit
    amend
    push
    pull
    fetch
    bookmark
    branch
    switch
    squash
    split
    abandon
    describe
    rebase
    revert
    undo
    redo))

;; Templates: fields unit-separated (0x1f), records terminated (0x1e). Change id
;; first, then commit id, author, relative time, first line of description.
(define JJ-LOG-TEMPLATE
  (string-append
    "change_id.short()"
    " ++ \"\\x1f\" ++ commit_id.short()"
    " ++ \"\\x1f\" ++ author.name()"
    " ++ \"\\x1f\" ++ committer.timestamp().ago()"
    " ++ \"\\x1f\" ++ if(description, description.first_line(), \"(no description)\")"
    " ++ \"\\x1f\" ++ bookmarks.join(\",\")"
    " ++ \"\\x1e\""))

(define JJ-OP-TEMPLATE
  (string-append
    "id.short()"
    " ++ \"\\x1f\" ++ if(description, description, \"\")"
    " ++ \"\\x1f\" ++ time.start().ago()"
    " ++ \"\\x1e\""))

;; Bookmark name, target commit id, and the target's first description line.
(define JJ-BOOKMARK-TEMPLATE
  (string-append
    "name"
    " ++ \"\\x1f\" ++ if(normal_target, normal_target.commit_id().short(), \"\")"
    " ++ \"\\x1f\" ++ if(normal_target, normal_target.description().first_line(), \"\")"
    " ++ \"\\x1e\""))

;;; Status ;;;

(define (jj-status b)
  (let* ([root (backend-root b)]
         [wc-parent (jj-wc-and-parent root)]
         [header (jj-header wc-parent)]
         [changes (jj-working-copy-changes root)]
         [conflicts (jj-conflicts root)]
         [bookmarks (jj-bookmarks root)]
         [recent (jj-log* root "::@- | @" (hash 'limit (juju-recent-count)))]
         [ops (jj-op-log root (juju-recent-count))])
    (make-status header
      (append
        (maybe-section "working-copy" "Changes in working copy" 'working-copy changes)
        (maybe-section "conflicts" "Conflicts" 'conflicts conflicts)
        (maybe-section "bookmarks" "Bookmarks" 'bookmarks bookmarks)
        (maybe-section "recent" "Recent changes" 'recent recent)
        (maybe-section "operations" "Operations" 'operations ops)))))

;; Conflicted files in the working copy, from `jj resolve --list`. That command
;; exits non-zero ("No conflicts found") when there are none, so a failed run is
;; simply an empty list. Each line is "<path>  <description>"; the path is the
;; leading field.
(define (jj-conflicts root)
  (let ([res (run-vcs root "jj" (list "resolve" "--list"))])
    (if (vcs-ok? res)
      (filter-map parse-jj-conflict-line (split-lines (vcs-stdout res)))
      '())))

(define (parse-jj-conflict-line line)
  (let ([trimmed (string-trim line)])
    (if (string=? trimmed "")
      #f
      (let ([sp (first-space-index trimmed)])
        (make-file-item (if sp (substring trimmed 0 sp) trimmed)
          'conflicted
          #f
          #f
          (hash))))))

;; Index of the first space in `s`, or #f. Used to split a path from a trailing
;; description; jj does not percent-encode paths, but conflicted paths with
;; spaces are rare and degrade only the trailing-description split, not the path.
(define (first-space-index s)
  (let ([len (string-length s)])
    (let loop ([i 0])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref s i) #\space) i]
        [else (loop (+ i 1))]))))

;; The `@` and `@-` commit records, for the header. Each is fetched by its own
;; revset query, so revset ordering does not matter.
(define (jj-wc-and-parent root)
  (let ([wc (jj-single root "@")]
        [parent (jj-single root "@-")])
    (hash 'wc wc 'parent parent)))

;; One commit-record for revset `rev`, or #f.
(define (jj-single root rev)
  (let ([records (jj-log* root rev (hash 'limit 1))])
    (if (null? records) #f (car records))))

(define (jj-header wc-parent)
  (let ([wc (hash-ref wc-parent 'wc)]
        [parent (hash-ref wc-parent 'parent)])
    (append
      (if wc
        (list (cons "Working copy" (commit-summary-line wc)))
        '())
      (if parent
        (list (cons "Parent" (commit-summary-line parent)))
        '()))))

(define (commit-summary-line c)
  (string-append (commit-record-id c) "  " (commit-record-short-id c)
    "  "
    (commit-record-subject c)))

;; Working-copy changes via `jj diff -r @ --summary` -> file-items.
(define (jj-working-copy-changes root)
  (let* ([res (run-vcs root "jj" (list "diff" "-r" "@" "--summary"))]
         [lines (split-lines (vcs-stdout res))])
    (parse-jj-summary lines)))

;;@doc
;; Parse `jj diff --summary` lines ("M path", "A path", "D path", "R old new")
;; into file-items. Pure.
(define (parse-jj-summary lines)
  (filter (lambda (x) x)
    (map (lambda (line)
          (if (< (string-length line) 2)
            #f
            (let ([code (string-ref line 0)]
                  [path (string-trim (string-drop line 1))])
              (make-file-item path (jj-code->status code) #f #f (hash)))))
      lines)))

(define (jj-code->status c)
  (cond
    [(char=? c #\M) 'modified]
    [(char=? c #\A) 'added]
    [(char=? c #\D) 'deleted]
    [(char=? c #\R) 'renamed]
    [(char=? c #\C) 'copied]
    [else 'modified]))

;;; Bookmarks ;;;

;; Bookmarks as commit-records: id is the bookmark name (the operand a switch /
;; delete / rename acts on), subject is its target id and description for the
;; row. Parsed from a unit/record-separated template so names with spaces or
;; colons survive.
(define (jj-bookmarks root)
  (let* ([res (run-vcs root "jj" (list "bookmark" "list" "-T" JJ-BOOKMARK-TEMPLATE))]
         [records (split-many (vcs-stdout res) (string (integer->char 30)))]
         [non-empty (filter (lambda (r) (not (string=? (string-trim r) ""))) records)])
    (map (lambda (rec)
          (let* ([fields (field-split (trim-start rec))]
                 [get (lambda (i) (if (> (length fields) i) (list-ref fields i) ""))]
                 [name (get 0)]
                 [target (get 1)]
                 [desc (get 2)])
            (make-commit-record name name "" ""
              (string-trim (string-append target " " desc))
              '())))
      non-empty)))

;;; Diff ;;;
;;;
;;; target hash:
;;;   (hash 'type 'file 'path <path>)        change in the working copy
;;;   (hash 'type 'commit 'rev <rev>)        a change's full diff

(define (jj-diff b target)
  (let ([root (backend-root b)]
        [type (hash-ref target 'type)])
    (cond
      [(eq? type 'file)
        (let ([res (run-vcs root "jj"
                    (list "diff" "-r" "@" "--git" "--" (jj-fileset (hash-ref target 'path))))])
          (parse-unified-diff (vcs-stdout res)))]
      [(eq? type 'commit)
        (let ([res (run-vcs root "jj"
                    (list "diff" "-r" (hash-ref target 'rev) "--git"))])
          (parse-unified-diff (vcs-stdout res)))]
      [(eq? type 'worktree)
        (parse-unified-diff
          (vcs-stdout (run-vcs root "jj" (list "diff" "-r" "@" "--git"))))]
      [else '()])))

;;; Log ;;;

(define (jj-log b revset opts) (jj-log* (backend-root b) revset opts))

(define (jj-log* root revset opts)
  (let* ([limit (if (hash-contains? opts 'limit) (hash-ref opts 'limit) 50)]
         [args (append
                (list "log" "--no-graph" "-T" JJ-LOG-TEMPLATE
                  "-n"
                  (number->string limit))
                (if revset (list "-r" revset) '()))]
         [res (run-vcs root "jj" args)])
    (if (vcs-ok? res)
      (parse-jj-records (vcs-stdout res))
      '())))

(define (parse-jj-records text)
  (let* ([records (split-many text (string (integer->char 30)))]
         [non-empty (filter (lambda (r) (not (string=? (string-trim r) ""))) records)])
    (map parse-jj-record non-empty)))

(define (parse-jj-record rec)
  (let* ([fields (field-split (trim-start rec))]
         [get (lambda (i) (if (> (length fields) i) (list-ref fields i) ""))]
         [bk (get 5)]
         [refs (if (string=? (string-trim bk) "") '() (map string-trim (split-many bk ",")))])
    (make-commit-record (get 0) (get 1) (get 2) (get 3) (get 4) refs)))

;;; Operation log ;;;

(define (jj-op-log root limit)
  (let* ([res (run-vcs root "jj"
               (list "op" "log" "--no-graph" "-T" JJ-OP-TEMPLATE
                 "-n"
                 (number->string limit)))]
         [records (split-many (vcs-stdout res) (string (integer->char 30)))]
         [non-empty (filter (lambda (r) (not (string=? (string-trim r) ""))) records)])
    (map (lambda (rec)
          (let* ([fields (field-split (trim-start rec))]
                 [get (lambda (i) (if (> (length fields) i) (list-ref fields i) ""))])
            (make-commit-record (get 0) (get 0) "" (get 2) (get 1) '())))
      non-empty)))

;;; Show ;;;

(define (jj-show b rev)
  (let* ([root (backend-root b)]
         [commit (jj-single root rev)]
         [res (run-vcs root "jj" (list "diff" "-r" rev "--git"))]
         [hunks (parse-unified-diff (vcs-stdout res))])
    (hash 'commit commit 'hunks hunks)))

;;; Blame ;;;

(define (jj-blame b file line-range)
  (let* ([root (backend-root b)]
         [res (run-vcs root "jj" (list "file" "annotate" file))])
    (if (vcs-ok? res) (split-lines (vcs-stdout res)) '())))

;;; Mutations ;;;
;;;
;;; jj has no index, so stage/unstage/stage-all/unstage-all are unsupported (not
;;; in jj-capabilities, and `jj-mutate` returns #f for them, reported uniformly).
;;; The working copy is the commit `@`, so:
;;;   discard      jj restore <paths>   (revert selected paths in @ to its parent)
;;;   commit       jj commit -m msg     (describe @, then start a fresh @)
;;;   amend        jj describe -m msg   (re-describe @ in place)
;;;   commit-fixup jj squash --into rev (fold @'s changes into a target change)
;;;   extend       jj squash            (fold @'s changes into its parent @-)
;;;   fetch/pull   jj git fetch
;;;   push         jj git push
;;;   undo/redo    jj undo / jj redo    (first-class over the operation log)
;;; Mutations run synchronously (see backend-git for the rationale).

(define (jj-mutate b op args)
  (let ([root (backend-root b)])
    (cond
      [(eq? op 'discard) (jj-discard root (car args))]
      [(eq? op 'commit) (jj-commit root (car args))]
      [(eq? op 'amend) (jj-amend root (car args))]
      [(eq? op 'commit-fixup) (jj-commit-fixup root (car args))]
      [(eq? op 'extend) (jj-extend root)]
      [(eq? op 'fetch) (jj-network root "fetch" (car args) "Fetched")]
      [(eq? op 'pull) (jj-network root "fetch" (car args) "Fetched (pull)")]
      [(eq? op 'push) (jj-network root "push" (car args) "Pushed")]
      [(eq? op 'undo) (jj-run* root (list "undo") "Undid last operation")]
      [(eq? op 'redo) (jj-run* root (list "redo") "Redid operation")]
      [(eq? op 'squash) (jj-squash root (car args))]
      [(eq? op 'split) (jj-split root (car args))]
      [(eq? op 'abandon) (jj-abandon root (car args))]
      [(eq? op 'describe) (jj-describe root (car args))]
      [(eq? op 'rebase) (jj-rebase root (car args))]
      [(eq? op 'revert) (jj-revert root (car args))]
      [(eq? op 'switch) (jj-switch root (car args))]
      [(eq? op 'branch-create) (jj-bookmark-create root (car args) (cadr args))]
      [(eq? op 'branch-set) (jj-bookmark-set root (car args) (cadr args))]
      [(eq? op 'branch-rename) (jj-bookmark-rename root (car args) (cadr args))]
      [(eq? op 'branch-delete) (jj-bookmark-delete root (car args))]
      [else #f]))) ; stage/unstage/stash/set-upstream/etc: unsupported under jj

(define (jj-run* root args success-msg)
  (let ([res (run-vcs root "jj" args)])
    (if (vcs-ok? res)
      (ok-result success-msg res)
      (err-result (string-append "jj failed: " (result-tail res)) res))))

;; Discard reverts selected paths in @ to the parent's content. jj has no index,
;; so partial-line discard is not expressible through `jj restore`; line-scope
;; specs are refused (select the whole file instead).
(define (jj-discard root specs)
  (if (null? specs)
    (err-result "nothing selected" #f)
    (let ([line-specs (filter (lambda (s) (eq? (hash-ref s 'scope) 'lines)) specs)]
          [paths (filter-map (lambda (s)
                              (and (eq? (hash-ref s 'scope) 'file) (hash-ref s 'path)))
                  specs)])
      (cond
        [(not (null? line-specs))
          (err-result "partial-line discard is unsupported under jj; discard the whole file" #f)]
        [(null? paths) (err-result "nothing to discard" #f)]
        [else (jj-run* root (append (list "restore" "--") (map jj-fileset paths))
               (string-append "Discarded " (count-label (length paths))))]))))

;; A repo-root-relative path argument for jj. jj resolves bare path args against
;; the process working directory, not `-R`, so every path argument is wrapped in
;; a `root:"..."` fileset (quoted and escaped) to anchor it at the workspace
;; root regardless of where the editor launched the process.
(define (jj-fileset path)
  (string-append "root:\"" (jj-escape path) "\""))

(define (jj-escape path)
  (list->string
    (apply append
      (map (lambda (c) (if (or (char=? c #\") (char=? c #\\)) (list #\\ c) (list c)))
        (string->list path)))))

(define (jj-commit root message)
  (if (blank? message)
    (err-result "commit aborted: empty message" #f)
    (jj-run* root (list "commit" "-m" message) "Committed working copy")))

(define (jj-amend root message)
  (if (blank? message)
    (err-result "describe aborted: empty message" #f)
    (jj-run* root (list "describe" "-m" message) "Re-described @")))

(define (jj-commit-fixup root rev)
  (if (blank? rev)
    (err-result "no target change for squash" #f)
    (jj-run* root (list "squash" "--into" rev)
      (string-append "Squashed into " rev))))

(define (jj-extend root)
  (jj-run* root (list "squash") "Squashed @ into parent"))

;; opts may carry 'remote (string). jj uses its configured default otherwise.
(define (jj-network root subcmd opts success-msg)
  (let* ([remote (if (and (hash? opts) (hash-contains? opts 'remote)) (hash-ref opts 'remote) #f)]
         [args (append (list "git" subcmd) (if remote (list "--remote" remote) '()))]
         [res (run-vcs root "jj" args)])
    (if (vcs-ok? res)
      (ok-result (let ([tail (result-tail res)])
                  (if (string=? tail "") success-msg (string-append success-msg ": " tail)))
        res)
      (err-result (string-append "jj git " subcmd " failed: " (result-tail res)) res))))

;;; History rewriting ;;;
;;;
;;; jj rewrites history safely (every step is an operation `jj undo` can reverse),
;;; so these need no confirmation. They run non-interactively: where jj would
;;; open a description editor, the forced EDITOR=true (process.scm) accepts an
;;; empty description. Path arguments are wrapped as filesets (jj-fileset).
;;;
;;; Note the overlap with phase-2 verbs: `extend` and a no-target `squash` both
;;; fold @ into its parent; `describe` and `amend` both re-describe @.

;; opts: 'into (fold into this rev), 'from (fold this rev out), 'message. With
;; neither from nor into, folds @ into its parent (same as `extend`).
(define (jj-squash root squash-opts)
  (let* ([from (opt squash-opts 'from #f)]
         [into (opt squash-opts 'into #f)]
         [message (opt squash-opts 'message #f)]
         [args (append (list "squash")
                (if from (list "--from" from) '())
                (if into (list "--into" into) '())
                (if (blank? message) '() (list "-m" message)))])
    (jj-run* root args
      (if into (string-append "Squashed into " into) "Squashed into parent"))))

;; Split the given paths out of @ into a new commit, leaving the rest in @.
;; Pathless split is interactive (a diff editor), so it is refused: select the
;; file(s) to split out, or pass them to the typed command.
(define (jj-split root paths)
  (if (null? paths)
    (err-result "split needs file paths (interactive split is unsupported)" #f)
    (jj-run* root (append (list "split") (map jj-fileset paths))
      (string-append "Split out " (count-label (length paths))))))

;; Abandon a revision (@ when none given); jj rebases its descendants onto its
;; parent. Reversible via `jj undo`.
(define (jj-abandon root rev)
  (let ([target (if (blank? rev) "@" rev)])
    (jj-run* root (list "abandon" target) (string-append "Abandoned " target))))

(define (jj-describe root message)
  (if (blank? message)
    (err-result "describe aborted: empty message" #f)
    (jj-run* root (list "describe" "-m" message) "Described @")))

;; Rebase the branch containing @ onto `onto`.
(define (jj-rebase root rebase-opts)
  (let ([onto (opt rebase-opts 'onto #f)])
    (if (blank? onto)
      (err-result "rebase needs a destination rev" #f)
      (jj-run* root (list "rebase" "-b" "@" "--onto" onto)
        (string-append "Rebased onto " onto)))))

;; Create a change that reverts `rev`, inserted after @ (so it lands on top of
;; the current work).
(define (jj-revert root rev)
  (if (blank? rev)
    (err-result "revert needs a rev" #f)
    (jj-run* root (list "revert" "-r" rev "--insert-after" "@")
      (string-append "Reverted " rev))))

;;; Bookmarks (jj's branches) and switch ;;;
;;;
;;; A bookmark is jj's named pointer. create/rename/delete back the same op
;;; symbols git's branch ops do, so command code never branches on the backend.
;;; switch uses `jj new <rev>` (the FAQ-recommended way to resume from a point):
;;; it starts a fresh working-copy change on top of the target rather than
;;; editing an existing commit, which is always safe.

(define (jj-switch root rev)
  (if (blank? rev)
    (err-result "switch needs a bookmark or rev" #f)
    (jj-run* root (list "new" rev) (string-append "New change on " rev))))

;; Create a bookmark at `rev` (@ when none given), without moving @.
(define (jj-bookmark-create root name rev)
  (if (blank? name)
    (err-result "bookmark needs a name" #f)
    (jj-run* root (list "bookmark" "create" name "-r" (if (blank? rev) "@" rev))
      (string-append "Created bookmark " name))))

;; Create-or-move a bookmark to `rev` (@ when none given). --allow-backwards so
;; pointing it at an ancestor just works, matching juju's "set it here" intent.
(define (jj-bookmark-set root name rev)
  (if (blank? name)
    (err-result "bookmark needs a name" #f)
    (jj-run* root (list "bookmark" "set" name "-r" (if (blank? rev) "@" rev) "--allow-backwards")
      (string-append "Set bookmark " name))))

(define (jj-bookmark-rename root old new)
  (if (or (blank? old) (blank? new))
    (err-result "rename needs old and new names" #f)
    (jj-run* root (list "bookmark" "rename" old new)
      (string-append "Renamed " old " to " new))))

(define (jj-bookmark-delete root name)
  (if (blank? name)
    (err-result "delete needs a bookmark name" #f)
    (jj-run* root (list "bookmark" "delete" name) (string-append "Deleted bookmark " name))))

;;; Read-only listings (query-fn) ;;;

(define (jj-query b op args)
  (let ([root (backend-root b)])
    (cond
      [(eq? op 'refs)
        (run-vcs-lines root "jj" (list "bookmark" "list" "--all-remotes"))]
      [(eq? op 'remotes) (run-vcs-lines root "jj" (list "git" "remote" "list"))]
      [(eq? op 'oplog)
        (run-vcs-lines root "jj"
          (list "op" "log" "--no-graph" "-n" (number->string (juju-recent-count))))]
      ;; jj's workspaces are its worktree analogue; it has no submodules.
      [(eq? op 'worktrees) (run-vcs-lines root "jj" (list "workspace" "list"))]
      [else '()]))) ; 'reflog/'submodules have no jj analogue

;;; Constructor (defined last; see backend-git for the Steel ordering note) ;;;

;;@doc Build a jj backend rooted at `root`. Mutations are dispatched through
;; `jj-mutate`.
(define (make-jj-backend root)
  (make-backend 'jj root jj-capabilities
    jj-status
    jj-diff
    jj-log
    jj-show
    jj-blame
    jj-mutate
    jj-query))
