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
(require "ui-utils.hx/strings.scm")
(require "backend-interface.scm")
(require "config.scm")
(require "rebase-todo.scm")

(provide make-jj-backend
  parse-jj-summary ; exported for unit tests
  parse-jj-conflict-line
  parse-jj-annotate)

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
    edit
    squash
    split
    abandon
    describe
    rebase
    rebase-interactive
    rebase-skip-emptied
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

;; One annotation record per output line: the change id, its short prefix, the
;; line's number in the originating change, then the content (which carries the
;; terminating newline, so lines double as records).
(define JJ-ANNOTATE-TEMPLATE
  (string-append
    "commit.change_id()"
    " ++ \"\\x1f\" ++ commit.change_id().shortest(8)"
    " ++ \"\\x1f\" ++ original_line_number"
    " ++ \"\\x1f\" ++ content"))

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
;; Parse `jj diff --summary` lines ("M path", "A path", "D path",
;; "R {old => new}" / "C {old => new}" - the rename segment may also sit
;; mid-path, "dir/{a => b}/f.txt") into file-items. A rename/copy resolves to
;; the new path, with the old path under 'orig-path so the view labels it and
;; path-taking operations receive a real file. Pure.
(define (parse-jj-summary lines)
  (filter (lambda (x) x)
    (map (lambda (line)
          (if (< (string-length line) 2)
            #f
            (let* ([code (string-ref line 0)]
                   [resolved (jj-resolve-rename-path (string-trim (string-drop line 1)))]
                   [path (car resolved)]
                   [orig (cdr resolved)])
              (make-file-item path (jj-code->status code) #f #f
                (if orig (hash 'orig-path orig 'rename? #t) (hash))))))
      lines)))

;; Resolve a summary path that may carry `{old => new}` rename segments, whole
;; ("{a.txt => b.txt}") or mid-path ("dir/{a => b}/f.txt"). Returns
;; (cons new-path old-path); old-path is #f when there is no rename segment.
;; An empty side ("dir/{ => sub}/f") drops out of its path; the doubled
;; separator it leaves is collapsed. Braces without a " => " inside are treated
;; as literal path text.
(define (jj-resolve-rename-path s)
  (let loop ([rest s] [new-acc ""] [old-acc ""] [found #f])
    (let ([open (jj-index-of rest "{" 0)]
          [finish (lambda (tail)
                   (if found
                     (cons (collapse-slashes (string-append new-acc tail))
                       (collapse-slashes (string-append old-acc tail)))
                     (cons s #f)))])
      (if (not open)
        (finish rest)
        (let* ([after-open (string-drop rest (+ open 1))]
               [close (jj-index-of after-open "}" 0)]
               [inner (if close (substring after-open 0 close) "")]
               [arrow (and close (jj-index-of inner " => " 0))])
          (if (not arrow)
            (finish rest)
            (loop (string-drop after-open (+ close 1))
              (string-append new-acc (substring rest 0 open)
                (string-drop inner (+ arrow 4)))
              (string-append old-acc (substring rest 0 open)
                (substring inner 0 arrow))
              #t)))))))

;; Index of the first occurrence of `needle` in `s` at or after `from`, or #f.
(define (jj-index-of s needle from)
  (let ([sl (string-length s)] [nl (string-length needle)])
    (let loop ([i from])
      (cond
        [(> (+ i nl) sl) #f]
        [(string=? (substring s i (+ i nl)) needle) i]
        [else (loop (+ i 1))]))))

;; "a//b" -> "a/b": what an empty rename-segment side leaves behind.
(define (collapse-slashes s)
  (let loop ([cs (string->list s)] [acc '()] [prev-slash #f])
    (cond
      [(null? cs) (list->string (reverse acc))]
      [(and prev-slash (char=? (car cs) #\/)) (loop (cdr cs) acc #t)]
      [else (loop (cdr cs) (cons (car cs) acc) (char=? (car cs) #\/))])))

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
         [full (and (hash-contains? opts 'full) (hash-ref opts 'full))]
         ;; The explicit revset wins; else `full` escapes jj's curated default
         ;; revset to the whole ancestry of the working copy; else jj's default.
         [rev (if revset revset (if full "::@" #f))]
         [args (append
                (list "log" "--no-graph" "-T" JJ-LOG-TEMPLATE
                  "-n"
                  (number->string limit))
                (if rev (list "-r" rev) '()))]
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
;;;
;;; Served through the 'blame query op (jj-blame-at), not a named interface
;;; field: the view drives blame via backend-query.

;;@doc
;; Parse `jj file annotate -T JJ-ANNOTATE-TEMPLATE` output into blame-line
;; records: one line per record, fields unit-separated. Stray 0x1f bytes in the
;; content itself are joined back into the text.
(define (parse-jj-annotate text)
  (filter-map parse-jj-annotate-line (split-lines text)))

(define (parse-jj-annotate-line line)
  (let ([fields (field-split line)])
    (if (>= (length fields) 4)
      (make-blame-line (list-ref fields 0)
        (list-ref fields 1)
        (let ([n (string->number (list-ref fields 2))]) (if n n 0))
        (rejoin-fields (cdr (cdr (cdr fields)))))
      #f)))

(define (rejoin-fields fields)
  (let loop ([fs (cdr fields)] [acc (car fields)])
    (if (null? fs)
      acc
      (loop (cdr fs) (string-append acc (string (integer->char 31)) (car fs))))))

;; The 'blame query (see the git counterpart for the spec shape). jj resolves
;; path arguments against the process cwd, and `file annotate` takes a plain
;; path rather than a fileset, so the repo-relative path is anchored as an
;; absolute one instead of the usual `root:"..."` wrap. `before?` uses the `-`
;; parent suffix, kept here so the view never forms a jj-specific revset.
(define (jj-blame-at root spec)
  (let* ([file (hash-ref spec 'file)]
         [rev (opt spec 'rev #f)]
         [before? (opt spec 'before #f)]
         [rev-args (if rev (list "-r" (if before? (string-append rev "-") rev)) '())]
         [args (append (list "file" "annotate" "-T" JJ-ANNOTATE-TEMPLATE)
                rev-args
                (list (path-join root file)))]
         [res (run-vcs root "jj" args)])
    (if (vcs-ok? res) (parse-jj-annotate (vcs-stdout res)) '())))

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
      [(eq? op 'rebase-interactive) (jj-rebase-interactive root (car args))]
      [(eq? op 'revert) (jj-revert root (car args))]
      [(eq? op 'switch) (jj-switch root (car args))]
      [(eq? op 'edit) (jj-edit root (car args))]
      [(eq? op 'branch-create) (jj-bookmark-create root (car args) (cadr args))]
      [(eq? op 'branch-set) (jj-bookmark-set root (car args) (cadr args))]
      [(eq? op 'branch-rename) (jj-bookmark-rename root (car args) (cadr args))]
      [(eq? op 'branch-delete) (jj-bookmark-delete root (car args) (cadr args))]
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

;; opts may carry 'remote (string) and 'all-remotes (#t, fetch only). jj uses
;; its configured default remote otherwise.
(define (jj-network root subcmd opts success-msg)
  (let* ([remote (if (and (hash? opts) (hash-contains? opts 'remote)) (hash-ref opts 'remote) #f)]
         [all-remotes (opt opts 'all-remotes #f)]
         [args (append (list "git" subcmd)
                (if (and all-remotes (string=? subcmd "fetch")) (list "--all-remotes") '())
                (if remote (list "--remote" remote) '()))]
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

;; Rebase the branch containing @ onto `onto`. With 'skip-emptied, commits
;; that become empty are abandoned rather than kept.
(define (jj-rebase root rebase-opts)
  (let ([onto (opt rebase-opts 'onto #f)]
        [skip-emptied (opt rebase-opts 'skip-emptied #f)])
    (if (blank? onto)
      (err-result "rebase needs a destination rev" #f)
      (jj-run* root (append (list "rebase" "-b" "@" "--onto" onto)
                     (if skip-emptied (list "--skip-emptied") '()))
        (string-append "Rebased onto " onto)))))

;;; Interactive rebase ;;;
;;;
;;; jj has no todo file: the same backend-neutral plan becomes an ordered
;;; sequence of jj commands keyed on stable change-ids (see rebase-todo's
;;; todo->jj-steps - folds, then drops, then rewords, then a relinearise into
;;; plan order, then parking @ on the edit target). Steps run synchronously;
;;; jj never pauses, so the first failure stops the run and reports it, and the
;;; whole batch is reversible with `jj undo`. Reorder steps may run as no-ops
;;; when the order is unchanged, which jj handles harmlessly.
(define (jj-rebase-interactive root plan)
  (let* ([entries (hash-ref plan 'entries)]
         [invalid (todo-validate entries)])
    (if invalid
      (err-result (string-append "invalid rebase plan: " invalid) #f)
      (jj-run-steps root (todo->jj-steps entries)))))

(define (jj-run-steps root steps)
  (if (null? steps)
    (ok-result "Rebase: nothing to do" #f)
    (let loop ([ss steps] [done 0] [last #f])
      (if (null? ss)
        (ok-result
          (string-append "Rebased: " (number->string done)
            (if (= done 1) " step" " steps")
            " (jj undo to revert)")
          last)
        (let ([res (run-vcs root "jj" (car ss))])
          (if (vcs-ok? res)
            (loop (cdr ss) (+ done 1) res)
            (err-result
              (string-append "jj rebase step failed: " (result-tail res) " (jj undo to revert)")
              res)))))))

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

;; Edit makes an existing change the working copy in place (descendants are
;; auto-rebased as it changes). jj itself refuses immutable commits; that error
;; surfaces through the result. Reversible via jj undo.
(define (jj-edit root rev)
  (if (blank? rev)
    (err-result "edit needs a rev" #f)
    (jj-run* root (list "edit" rev) (string-append "Editing " rev))))

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

;; opts is accepted for interface uniformity; bookmark delete has no
;; safe/force distinction, so it is ignored.
(define (jj-bookmark-delete root name opts)
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
      [(eq? op 'rebase-range) (jj-rebase-range root (car args))]
      [(eq? op 'blame) (jj-blame-at root (car args))]
      [else '()]))) ; 'reflog/'submodules have no jj analogue

;; The commit range for the interactive rebase editor (see the git counterpart).
;; jj edits by change-id, so 'base is informational only (the apply step ignores
;; it); what matters is `commits`, newest first. `spec` carries 'from (edit from
;; this change inclusive) or 'base (commits after this change); with neither it
;; defaults to the mutable ancestors of @ (the commits jj will let us rewrite).
(define (jj-rebase-range root spec)
  (let* ([from (opt spec 'from #f)]
         [given (opt spec 'base #f)]
         [revset (cond
                  [from (string-append "(" from "::@) & mutable()")]
                  [given (string-append given "..@")]
                  [else "::@ & mutable()"])])
    (hash 'base given 'commits (jj-log* root revset (hash 'limit 200)))))

;;; Constructor (defined last; see backend-git for the Steel ordering note) ;;;

;;@doc Build a jj backend rooted at `root`. Mutations are dispatched through
;; `jj-mutate`.
(define (make-jj-backend root)
  (make-backend 'jj root jj-capabilities
    jj-status
    jj-diff
    jj-log
    jj-show
    jj-mutate
    jj-query))
