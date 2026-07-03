;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; backend-git.scm - the Git implementation of the backend interface
;;;
;;; Reads status from `git status --porcelain=v2 --branch`, diffs lazily per
;;; file, and builds the model the view renders. Git has an index, so its status
;;; carries the untracked/unstaged/staged split jj lacks. All output is parsed
;;; into structs; no raw text escapes this module.

(require "process.scm")
(require "model.scm")
(require "diff.scm")
(require "string-utils.scm")
(require "backend-interface.scm")
(require "config.scm")
(require "rebase-todo.scm")

(provide make-git-backend
  parse-porcelain-status ; exported for unit tests
  parse-stash-line
  parse-git-blame-porcelain)

;; Features the Git model supports. No 'redo/'squash/'split/'abandon/'describe/
;; 'oplog: those are jj-only (git's squash is interactive rebase, not a single
;; op). 'undo is best-effort here: git has no first-class undo, so it reverses
;; only the last HEAD movement via the reflog (see git-undo), labelled as such.
;; The view consults this set, never the backend name.
(define git-capabilities
  '(status diff log show blame reflog
    stage
    unstage
    discard
    stage-all
    unstage-all
    commit
    amend
    push
    pull
    fetch
    fetch-prune
    pull-rebase
    push-set-upstream
    branch-force-delete
    commit-no-verify
    commit-signoff
    stash
    branch
    switch
    set-upstream
    reset
    rebase
    rebase-interactive
    autosquash
    force-push
    cherry-pick
    revert
    undo))

;;; Status ;;;

(define LOG-TEMPLATE
  ;; %H %h %an %ar %s %D, unit-separated, record-terminated.
  "%H%x1f%h%x1f%an%x1f%ar%x1f%s%x1f%D%x1e")

(define (git-status b)
  (let* ([root (backend-root b)]
         [res (run-vcs root "git" (list "status" "--porcelain=v2" "--branch"))]
         [lines (split-lines (vcs-stdout res))]
         [parsed (parse-porcelain-status lines)]
         [branch (hash-ref parsed 'branch)]
         [head-name (hash-ref branch 'head)]
         [upstream (hash-ref branch 'upstream)]
         [ahead (hash-ref branch 'ahead)]
         [behind (hash-ref branch 'behind)]
         [head-subject (git-head-subject root)]
         [header (git-header-with-rebase root
                  (build-git-header head-name head-subject upstream ahead behind))]
         [sections (build-git-sections root parsed upstream ahead behind)])
    (make-status header sections)))

;; Surface a paused interactive rebase in the status header so the user sees they
;; are mid-rebase and how to drive it. The continue/abort/skip commands are
;; typed (no status-view key is repurposed mid-rebase).
(define (git-header-with-rebase root header)
  (if (rebase-in-progress? root)
    (append header
      (list (cons "Rebase" "paused - :juju-rebase-continue / -abort / -skip")))
    header))

;; Subject line of HEAD, or "" on an unborn/empty repo.
(define (git-head-subject root)
  (let ([res (run-vcs root "git" (list "log" "-1" "--format=%s"))])
    (if (vcs-ok? res) (string-trim (vcs-stdout res)) "")))

(define (build-git-header head-name head-subject upstream ahead behind)
  (let ([head-val (if (string=? head-subject "")
                   head-name
                   (string-append head-name "  " head-subject))])
    (append
      (list (cons "Head" head-val))
      (if upstream
        (list (cons "Push"
               (string-append upstream "  " (format-ahead-behind ahead behind))))
        '()))))

(define (format-ahead-behind ahead behind)
  (string-append "+" (number->string ahead) " -" (number->string behind)))

;; Build the file/commit sections, omitting empties.
(define (build-git-sections root parsed upstream ahead behind)
  (let* ([files (hash-ref parsed 'files)]
         [untracked (filter (lambda (f) (eq? (hash-ref f 'where) 'untracked)) files)]
         [unstaged (filter (lambda (f) (hash-ref f 'unstaged?)) files)]
         [staged (filter (lambda (f) (hash-ref f 'staged?)) files)]
         [conflicts (filter (lambda (f) (eq? (hash-ref f 'where) 'conflict)) files)]
         [stashes (git-stash-list root)]
         [recent (git-log* root #f (hash 'limit (juju-recent-count)))])
    (append
      (maybe-section "untracked" "Untracked files" 'untracked
        (map (lambda (f) (file-from-status f 'untracked)) untracked))
      (maybe-section "unstaged" "Unstaged changes" 'unstaged
        (map (lambda (f) (file-from-status f 'unstaged)) unstaged))
      (maybe-section "staged" "Staged changes" 'staged
        (map (lambda (f) (file-from-status f 'staged)) staged))
      (maybe-section "conflicts" "Conflicts" 'conflicts
        (map (lambda (f) (file-from-status f 'conflict)) conflicts))
      (maybe-section "stashes" "Stashes" 'stashes stashes)
      (if (and upstream (> ahead 0))
        (maybe-section "unpushed" (string-append "Unpushed to " upstream) 'unpushed
          (git-log* root (string-append upstream "..HEAD") (hash 'limit 20)))
        '())
      (if (and upstream (> behind 0))
        (maybe-section "unpulled" (string-append "Unpulled from " upstream) 'unpulled
          (git-log* root (string-append "HEAD.." upstream) (hash 'limit 20)))
        '())
      (maybe-section "recent" "Recent commits" 'recent recent))))

;; Build a file-item from a parsed status entry, using the code relevant to the
;; section it appears in (X for staged, Y for unstaged/untracked).
(define (file-from-status f where)
  (let* ([code-char (cond
                     [(eq? where 'staged) (hash-ref f 'x)]
                     [(eq? where 'untracked) #\?]
                     [(eq? where 'conflict) #\U]
                     [else (hash-ref f 'y)])]
         [status-code (status-code-from-char code-char)]
         [extra (if (hash-ref f 'orig-path)
                 (hash 'orig-path (hash-ref f 'orig-path) 'rename? #t)
                 (hash))])
    (make-file-item (hash-ref f 'path) status-code #f #f extra)))

(define (status-code-from-char c)
  (cond
    [(char=? c #\M) 'modified]
    [(char=? c #\A) 'added]
    [(char=? c #\D) 'deleted]
    [(char=? c #\R) 'renamed]
    [(char=? c #\C) 'copied]
    [(char=? c #\T) 'type-changed]
    [(char=? c #\U) 'conflicted]
    [(char=? c #\?) 'untracked]
    [else 'modified]))

;;@doc
;; Parse `git status --porcelain=v2 --branch` lines into a hash:
;;   'branch (hash 'head 'upstream 'ahead 'behind)
;;   'files  (list of per-file hashes: 'path 'x 'y 'where 'staged? 'unstaged?
;;            'orig-path)
;; Pure: takes the already-split lines, returns data.
(define (parse-porcelain-status lines)
  (let loop ([ls lines]
             [branch (hash 'head "(unknown)" 'upstream #f 'ahead 0 'behind 0)]
             [files '()])
    (cond
      [(null? ls) (hash 'branch branch 'files (reverse files))]
      [else
        (let ([line (car ls)])
          (cond
            [(string-prefix? "# branch." line)
              (loop (cdr ls) (parse-branch-line line branch) files)]
            [(string-prefix? "1 " line)
              (loop (cdr ls) branch (cons (parse-ordinary-entry line) files))]
            [(string-prefix? "2 " line)
              (loop (cdr ls) branch (cons (parse-rename-entry line) files))]
            [(string-prefix? "u " line)
              (loop (cdr ls) branch (cons (parse-unmerged-entry line) files))]
            [(string-prefix? "? " line)
              (loop (cdr ls) branch (cons (parse-untracked-entry line) files))]
            [else (loop (cdr ls) branch files)]))]))) ; ignored '!' etc.

(define (parse-branch-line line branch)
  (cond
    [(string-prefix? "# branch.head " line)
      (hash-insert branch 'head (string-trim (drop-prefix line "# branch.head ")))]
    [(string-prefix? "# branch.upstream " line)
      (hash-insert branch 'upstream (string-trim (drop-prefix line "# branch.upstream ")))]
    [(string-prefix? "# branch.ab " line)
      (let* ([ab (string-trim (drop-prefix line "# branch.ab "))]
             [parts (split-many ab " ")]
             [ahead (parse-signed (find-prefixed parts "+"))]
             [behind (parse-signed (find-prefixed parts "-"))])
        (hash-insert (hash-insert branch 'ahead ahead) 'behind behind))]
    [else branch]))

(define (find-prefixed parts sign)
  (let loop ([ps parts])
    (cond
      [(null? ps) "0"]
      [(string-prefix? sign (car ps)) (car ps)]
      [else (loop (cdr ps))])))

(define (parse-signed tok)
  (let ([n (string->number (string-drop tok 1))]) (if n n 0)))

(define (parse-ordinary-entry line)
  (let* ([xy (substring line 2 4)]
         [x (string-ref xy 0)]
         [y (string-ref xy 1)]
         [path (after-nth-space line 8)])
    (status-entry path x y #f)))

(define (parse-rename-entry line)
  (let* ([xy (substring line 2 4)]
         [x (string-ref xy 0)]
         [y (string-ref xy 1)]
         ;; after the 9th field (which is the Xscore token) comes "path\torig".
         [rest (after-nth-space line 9)]
         [tab-parts (split-many rest "\t")]
         [path (list-ref tab-parts 0)]
         [orig (if (>= (length tab-parts) 2) (list-ref tab-parts 1) #f)])
    (status-entry path x y orig)))

(define (parse-unmerged-entry line)
  (let* ([path (after-nth-space line 10)])
    (status-entry path #\U #\U #f 'conflict)))

(define (parse-untracked-entry line)
  (status-entry (after-nth-space line 1) #\? #\? #f 'untracked))

;; Build a per-file status hash. `where` is the precomputed primary bucket for
;; untracked/conflict entries; ordinary entries derive staged?/unstaged? from XY.
(define (status-entry path x y orig . opt)
  (let* ([forced-where (if (>= (length opt) 1) (list-ref opt 0) #f)]
         [staged? (and (not forced-where) (not (char=? x #\.)))]
         [unstaged? (and (not forced-where) (not (char=? y #\.)))]
         [where (cond
                 [forced-where forced-where]
                 [staged? 'staged]
                 [else 'unstaged])])
    (hash 'path path 'x x 'y y 'orig-path orig
      'where
      where
      'staged?
      staged?
      'unstaged?
      unstaged?)))

;; Substring after the n-th space in `line` (1-based). Porcelain v2 fields are
;; space-free except the trailing path, so this isolates the path exactly,
;; preserving any spaces it contains.
(define (after-nth-space line n)
  (let ([len (string-length line)])
    (let loop ([i 0] [seen 0])
      (cond
        [(>= i len) ""]
        [(char=? (string-ref line i) #\space)
          (if (= (+ seen 1) n)
            (substring line (+ i 1) len)
            (loop (+ i 1) (+ seen 1)))]
        [else (loop (+ i 1) seen)]))))

(define (drop-prefix str prefix) (string-drop str (string-length prefix)))

;;; Stash ;;;

;; Each entry is "stash@{N}: <subject>". The ref before the first ": " is the
;; stable id stash-pop/apply/drop need; the rest is the human subject.
(define (git-stash-list root)
  (let ([lines (run-vcs-lines root "git" (list "stash" "list"))])
    (map parse-stash-line lines)))

(define (parse-stash-line line)
  (let ([idx (find-substring line ": ")])
    (if idx
      (let ([ref (substring line 0 idx)]
            [subject (string-drop line (+ idx 2))])
        (make-commit-record ref ref "" "" subject '()))
      (make-commit-record line line "" "" line '()))))

;; Index of the first occurrence of `needle` in `s`, or #f. (No prelude search.)
(define (find-substring s needle)
  (let ([sl (string-length s)]
        [nl (string-length needle)])
    (if (= nl 0)
      0
      (let loop ([i 0])
        (cond
          [(> (+ i nl) sl) #f]
          [(string=? (substring s i (+ i nl)) needle) i]
          [else (loop (+ i 1))])))))

;;; Diff ;;;
;;;
;;; target is a hash:
;;;   (hash 'type 'file 'section <kind> 'path <path>)   per-file worktree/index
;;;   (hash 'type 'commit 'rev <rev>)                   a commit's full diff
;;; Diffs legitimately exit non-zero (e.g. --no-index), so stdout is parsed
;;; regardless of exit code.

(define (git-diff b target)
  (let ([root (backend-root b)]
        [type (hash-ref target 'type)])
    (cond
      [(eq? type 'file) (git-file-diff root target)]
      [(eq? type 'commit) (git-commit-hunks root (hash-ref target 'rev))]
      [(eq? type 'worktree)
        ;; All tracked changes against HEAD (staged and unstaged).
        (parse-unified-diff (vcs-stdout (run-vcs root "git" (list "diff" "HEAD"))))]
      [else '()])))

(define (git-file-diff root target)
  (let* ([section (hash-ref target 'section)]
         [path (hash-ref target 'path)]
         [args (cond
                [(eq? section 'staged) (list "diff" "--cached" "--" path)]
                [(eq? section 'untracked)
                  ;; Untracked files have no index entry; diff against
                  ;; /dev/null to render them as all-additions.
                  (list "diff" "--no-index" "--" "/dev/null" path)]
                [else (list "diff" "--" path)])]
         [res (run-vcs root "git" args)])
    (parse-unified-diff (vcs-stdout res))))

(define (git-commit-hunks root rev)
  (let ([res (run-vcs root "git" (list "show" "--format=" rev))])
    (parse-unified-diff (vcs-stdout res))))

;;; Log ;;;

(define (git-log b range opts)
  (git-log* (backend-root b) range opts))

(define (git-log* root range opts)
  (let* ([limit (if (hash-contains? opts 'limit) (hash-ref opts 'limit) 50)]
         [base-args (list "log" (string-append "-n" (number->string limit))
                     (string-append "--format=" LOG-TEMPLATE))]
         [args (if range (append base-args (list range)) base-args)]
         [res (run-vcs root "git" args)])
    (if (vcs-ok? res)
      (parse-log-records (vcs-stdout res))
      '())))

;; Records are terminated by RECORD-SEP (0x1e) and may be newline-separated.
(define (parse-log-records text)
  (let* ([records (split-many text (string (integer->char 30)))]
         [trimmed (map (lambda (r) (trim-start r)) records)]
         [non-empty (filter (lambda (r) (not (string=? (string-trim r) ""))) trimmed)])
    (map parse-log-record non-empty)))

(define (parse-log-record rec)
  (let* ([fields (field-split rec)]
         [get (lambda (i) (if (> (length fields) i) (list-ref fields i) ""))]
         [refs (parse-refs (get 5))])
    (make-commit-record (get 0) (get 1) (get 2) (get 3) (get 4) refs)))

;; "%D" -> "HEAD -> main, origin/main, tag: v1" ; split into a name list.
(define (parse-refs s)
  (if (string=? (string-trim s) "")
    '()
    (map string-trim (split-many s ","))))

;;; Show ;;;

(define (git-show b rev)
  (let* ([root (backend-root b)]
         [meta (run-vcs root "git" (list "show" "-s" (string-append "--format=" LOG-TEMPLATE) rev))]
         [records (parse-log-records (vcs-stdout meta))]
         [commit (if (null? records) #f (car records))]
         [hunks (git-commit-hunks root rev)])
    (hash 'commit commit 'hunks hunks)))

;;; Blame ;;;
;;;
;;; Served through the 'blame query op (git-blame-at), not a named interface
;;; field: the view drives blame via backend-query.

;;@doc
;; Parse `git blame --porcelain` output into blame-line records. Porcelain
;; interleaves an entry header `<sha> <orig> <final> [<n>]` (the sha header
;; repeats for every line; author/committer metadata appears only on a sha's
;; first entry), metadata lines, and the content line prefixed with a tab.
;; Only headers and content lines matter here; metadata is skipped.
(define (parse-git-blame-porcelain text)
  (let loop ([lines (split-lines text)] [sha #f] [orig 0] [acc '()])
    (if (null? lines)
      (reverse acc)
      (let ([line (car lines)])
        (cond
          [(string-prefix? "\t" line)
            (loop (cdr lines) sha orig
              (if sha
                (cons (make-blame-line sha (string-take sha 8) orig (string-drop line 1)) acc)
                acc))]
          [else
            (let ([header (parse-blame-header line)])
              (if header
                (loop (cdr lines) (car header) (cdr header) acc)
                (loop (cdr lines) sha orig acc)))])))))

;; (sha . orig-line) when `line` is a porcelain entry header, else #f. An entry
;; header's first token is the bare 40-char sha; no metadata line starts with
;; one ("previous"/"filename"/"author" etc. lead with their keyword).
(define (parse-blame-header line)
  (let ([parts (split-many line " ")])
    (if (and (>= (length parts) 3)
         (= (string-length (car parts)) 40))
      (let ([orig (string->number (cadr parts))])
        (if orig (cons (car parts) orig) #f))
      #f)))

;; The 'blame query. `spec` is (hash 'file f 'rev r 'before before?): `rev` #f
;; blames the working tree; `before?` #t blames at the parent of `rev`. The `^`
;; parent suffix stays here so the view never forms a git-specific rev. '()
;; when blame fails (a root commit's parent, a path absent at that rev).
(define (git-blame-at root spec)
  (let* ([file (hash-ref spec 'file)]
         [rev (opt spec 'rev #f)]
         [before? (opt spec 'before #f)]
         [rev-args (if rev (list (if before? (string-append rev "^") rev)) '())]
         [args (append (list "blame" "--porcelain") rev-args (list "--" file))]
         [res (run-vcs root "git" args)])
    (if (vcs-ok? res) (parse-git-blame-porcelain (vcs-stdout res)) '())))

;;; Mutations ;;;
;;;
;;; All mutating ops are dispatched through `git-mutate`, the backend's mutate-fn.
;;; Mutations run synchronously: git's local index/worktree operations return
;;; promptly.
;;; Network ops (fetch/pull/push) also run synchronously for now; the view shows
;;; a busy line while they run. Each op returns a uniform result hash.
;;;
;;; Stage/unstage/discard take a list of operand specs (see operand.scm). A spec
;;; is either whole-file (scope 'file) or a set of selected diff lines (scope
;;; 'lines). Whole-file ops shell out to plumbing; line ops reconstruct a partial
;;; patch and feed it to `git apply` over stdin.

(define (git-mutate b op args)
  (let ([root (backend-root b)])
    (cond
      [(eq? op 'stage) (git-apply-selection root (car args) 'stage)]
      [(eq? op 'unstage) (git-apply-selection root (car args) 'unstage)]
      [(eq? op 'discard) (git-apply-selection root (car args) 'discard)]
      [(eq? op 'stage-all) (git-run* root (list "add" "-A") "Staged all changes")]
      [(eq? op 'unstage-all) (git-run* root (list "reset" "-q" "HEAD") "Unstaged everything")]
      [(eq? op 'commit) (git-commit root (car args) (cadr args))]
      [(eq? op 'amend) (git-amend root (car args) (cadr args))]
      [(eq? op 'commit-fixup) (git-commit-fixup root (car args))]
      [(eq? op 'extend) (git-extend root)]
      [(eq? op 'fetch) (git-network root "fetch" (car args))]
      [(eq? op 'pull) (git-network root "pull" (car args))]
      [(eq? op 'push) (git-network root "push" (car args))]
      [(eq? op 'rebase) (git-rebase root (car args))]
      [(eq? op 'rebase-interactive) (git-rebase-interactive root (car args))]
      [(eq? op 'rebase-continue) (git-rebase-step root "--continue" "Continued rebase")]
      [(eq? op 'rebase-abort) (git-rebase-step root "--abort" "Aborted rebase")]
      [(eq? op 'rebase-skip) (git-rebase-step root "--skip" "Skipped commit")]
      [(eq? op 'cherry-pick) (git-cherry-pick root (car args))]
      [(eq? op 'revert) (git-revert root (car args))]
      [(eq? op 'reset) (git-reset root (car args) (cadr args))]
      [(eq? op 'undo) (git-undo root)]
      [(eq? op 'switch) (git-switch root (car args))]
      [(eq? op 'branch-create) (git-branch-create root (car args) (cadr args))]
      [(eq? op 'branch-set) (git-branch-set root (car args) (cadr args))]
      [(eq? op 'branch-rename) (git-branch-rename root (car args) (cadr args))]
      [(eq? op 'branch-delete) (git-branch-delete root (car args) (cadr args))]
      [(eq? op 'set-upstream) (git-set-upstream root (car args) (cadr args))]
      [(eq? op 'stash) (git-stash root (car args))]
      [(eq? op 'stash-pop) (git-stash-cmd root "pop" (car args))]
      [(eq? op 'stash-apply) (git-stash-cmd root "apply" (car args))]
      [(eq? op 'stash-drop) (git-stash-cmd root "drop" (car args))]
      [else #f]))) ; -> unsupported-result, reported uniformly by backend-mutate

;; Run git `args`, mapping exit status to a result with `success-msg` on success.
(define (git-run* root args success-msg)
  (let ([res (run-vcs root "git" args)])
    (if (vcs-ok? res)
      (ok-result success-msg res)
      (err-result (string-append "git failed: " (result-tail res)) res))))

;;; Selection-driven stage/unstage/discard ;;;

(define (op-past op)
  (cond [(eq? op 'stage) "Staged"] [(eq? op 'unstage) "Unstaged"] [else "Discarded"]))

;; Apply `op` to each operand spec, accumulating a count and any errors.
(define (git-apply-selection root specs op)
  (if (null? specs)
    (err-result "nothing selected" #f)
    (let loop ([ss specs] [ok 0] [errs '()] [last #f])
      (if (null? ss)
        (if (null? errs)
          (ok-result (string-append (op-past op) " " (count-label ok)) last)
          (err-result (string-join (reverse errs) "; ") last))
        (let ([r (git-apply-one root (car ss) op)])
          (if (result-ok? r)
            (loop (cdr ss) (+ ok 1) errs (result-raw r))
            (loop (cdr ss) ok (cons (result-message r) errs) (result-raw r))))))))

(define (git-apply-one root spec op)
  (if (eq? (hash-ref spec 'scope) 'file)
    (git-file-op root spec op)
    (git-lines-op root spec op)))

;; Whole-file stage/unstage/discard via plumbing.
(define (git-file-op root spec op)
  (let ([path (hash-ref spec 'path)]
        [kind (hash-ref spec 'section-kind)]
        [code (hash-ref spec 'status-code)])
    (cond
      [(eq? op 'stage) (git-run root (list "add" "-A" "--" path))]
      [(eq? op 'unstage) (git-run root (list "reset" "-q" "HEAD" "--" path))]
      [(eq? op 'discard) (git-discard-file root path kind code)]
      [else (unsupported-result op)])))

;; Discard reverts a file to its committed state. Untracked entries are removed
;; outright (-d because porcelain lists an untracked directory as one `dir/`
;; entry, which clean skips without it); a staged-only new file is dropped from
;; the index and disk; anything else is restored from HEAD (clearing both index
;; and worktree changes).
(define (git-discard-file root path kind code)
  (cond
    [(eq? kind 'untracked) (git-run root (list "clean" "-f" "-d" "--" path))]
    [(eq? code 'added) (git-run root (list "rm" "-f" "--" path))]
    [else (git-run root (list "checkout" "HEAD" "--" path))]))

(define (git-run root args)
  (let ([res (run-vcs root "git" args)])
    (if (vcs-ok? res) (ok-result "ok" res) (err-result (result-tail res) res))))

;; Partial-hunk stage/unstage/discard. Re-fetch the relevant diff, refuse
;; binary/renamed files (a constructed patch cannot represent them), build a
;; patch containing only the selected change lines, and feed it to `git apply`.
;;   stage    forward-apply the unstaged diff to the index   (--cached)
;;   unstage  reverse-apply the staged diff in the index     (--cached --reverse)
;;   discard  reverse-apply the unstaged diff to the worktree (--reverse)
(define (git-lines-op root spec op)
  (if (eq? (hash-ref spec 'section-kind) 'untracked)
    ;; An untracked file's displayed diff is synthetic (--no-index); the real
    ;; `git diff` sees nothing, so a line selection can never apply.
    (err-result (string-append "cannot partial-stage untracked file: "
                 (hash-ref spec 'path)
                 " (stage the whole file)")
      #f)
    (let* ([path (hash-ref spec 'path)]
           [src-args (cond
                      [(eq? op 'unstage) (list "diff" "--cached" "--" path)]
                      [else (list "diff" "--" path)])]
           [raw (vcs-stdout (run-vcs root "git" src-args))]
           [flags (parse-diff-flags raw)])
      (cond
        [(hash-ref flags 'binary?)
          (err-result (string-append "cannot partial-stage binary file: " path) #f)]
        [(hash-ref flags 'rename?)
          (err-result (string-append "cannot partial-stage renamed file: " path) #f)]
        [else (git-apply-patch root path (hash-ref spec 'lines) raw op)]))))

(define (git-apply-patch root path lines-map raw op)
  (let* ([headers (diff-header-lines raw)]
         [hunks (parse-unified-diff raw)]
         [include? (lambda (hi bi)
                    (and (hash-contains? lines-map hi)
                      (hash-contains? (hash-ref lines-map hi) bi)))]
         ;; stage applies forward; unstage/discard reverse-apply, which needs the
         ;; mirrored line-selection rule.
         [mode (if (eq? op 'stage) 'forward 'reverse)]
         [patch (build-apply-patch headers hunks include? mode)])
    (if (not patch)
      (err-result (string-append "no selected lines for " path) #f)
      (let* ([apply-args (cond
                          [(eq? op 'stage) (list "apply" "--cached" "--recount" "-")]
                          [(eq? op 'unstage) (list "apply" "--cached" "--reverse" "--recount" "-")]
                          [else (list "apply" "--reverse" "--recount" "-")])]
             [res (run-vcs-input root "git" apply-args patch)])
        (if (vcs-ok? res)
          (ok-result "applied" res)
          (err-result (string-append "git apply failed (" path "): " (result-tail res)) res))))))

;;; Commit family ;;;

;; Message is fed over stdin (`-F -`) so it needs no quoting and may be
;; multi-line. An empty message aborts rather than opening an editor.
(define (git-commit root message opts)
  (if (blank? message)
    (err-result "commit aborted: empty message" #f)
    (let ([res (run-vcs-input root "git"
                (append (list "commit") (commit-flags opts) (list "-F" "-"))
                message)])
      (if (vcs-ok? res)
        (ok-result "Committed" res)
        (err-result (string-append "git commit failed: " (result-tail res)) res)))))

;; Amend with a new message when one is given; otherwise keep the existing
;; message (--no-edit), so amend can be used purely to fold in staged changes.
(define (git-amend root message opts)
  (let ([res (if (blank? message)
              (run-vcs root "git" (append (list "commit" "--amend" "--no-edit") (commit-flags opts)))
              (run-vcs-input root "git"
                (append (list "commit" "--amend") (commit-flags opts) (list "-F" "-"))
                message))])
    (if (vcs-ok? res)
      (ok-result "Amended HEAD" res)
      (err-result (string-append "git amend failed: " (result-tail res)) res))))

;; Optional commit/amend flags read from opts: --no-verify (skip hooks) and
;; --signoff. Both git-only; jj leaves the capabilities off so they never set.
(define (commit-flags opts)
  (append
    (if (opt opts 'no-verify #f) (list "--no-verify") '())
    (if (opt opts 'signoff #f) (list "--signoff") '())))

;; Record a `fixup!` commit targeting `rev`, to be squashed in a later
;; autosquash rebase.
(define (git-commit-fixup root rev)
  (if (blank? rev)
    (err-result "no target commit for fixup" #f)
    (let ([res (run-vcs root "git" (list "commit" (string-append "--fixup=" rev)))])
      (if (vcs-ok? res)
        (ok-result (string-append "Fixup for " rev) res)
        (err-result (string-append "git fixup failed: " (result-tail res)) res)))))

;; Extend HEAD: fold staged changes into the last commit, message unchanged.
(define (git-extend root)
  (let ([res (run-vcs root "git" (list "commit" "--amend" "--no-edit"))])
    (if (vcs-ok? res)
      (ok-result "Extended HEAD" res)
      (err-result (string-append "git extend failed: " (result-tail res)) res))))

;;; Network ;;;
;;;
;;; opts is a hash that may carry 'remote (string), 'force and 'set-upstream
;;; (#t, push only), 'prune and 'all-remotes (#t, fetch only), and 'rebase
;;; (#t, pull only). The remote, when omitted, lets git use its configured
;;; default. Each flag is gated on its subcommand so a switch carried over
;;; from the shared remote menu is inert where it does not apply.

(define (git-network root subcmd opts)
  (let* ([remote (opt opts 'remote #f)]
         [force (opt opts 'force #f)]
         [prune (opt opts 'prune #f)]
         [all-remotes (opt opts 'all-remotes #f)]
         [set-upstream (opt opts 'set-upstream #f)]
         [rebase (opt opts 'rebase #f)]
         [args (append (list subcmd)
                (if (and force (string=? subcmd "push")) (list "--force-with-lease") '())
                (if (and set-upstream (string=? subcmd "push")) (list "-u") '())
                (if (and prune (string=? subcmd "fetch")) (list "--prune") '())
                (if (and all-remotes (string=? subcmd "fetch")) (list "--all") '())
                (if (and rebase (string=? subcmd "pull")) (list "--rebase") '())
                (if remote (list remote) '()))]
         [res (run-vcs root "git" args)])
    (if (vcs-ok? res)
      (ok-result (string-append (network-verb subcmd)
                  (let ([tail (result-tail res)]) (if (string=? tail "") "" (string-append ": " tail))))
        res)
      (err-result (string-append "git " subcmd " failed: " (result-tail res)) res))))

(define (network-verb subcmd)
  (cond [(string=? subcmd "fetch") "Fetched"]
    [(string=? subcmd "pull") "Pulled"]
    [(string=? subcmd "push") "Pushed"]
    [else subcmd]))

;;; History rewriting ;;;
;;;
;;; The non-interactive subset: rebase onto a ref (optionally --autosquash),
;;; cherry-pick, revert, reset, and best-effort undo via the reflog. On conflict
;;; or any failure these report the command's tail; the user resolves in the
;;; worktree and re-runs.

;; Rebase the current branch onto `onto`. With 'autosquash, run the interactive
;; machinery non-interactively (sequence.editor=true accepts the auto-arranged
;; todo; GIT_EDITOR=true, forced in process.scm, accepts squashed messages), so
;; fixup!/squash! commits fold in without opening an editor.
(define (git-rebase root rebase-opts)
  (let ([onto (opt rebase-opts 'onto #f)]
        [autosquash (opt rebase-opts 'autosquash #f)])
    (if (blank? onto)
      (err-result "rebase needs a target ref" #f)
      (let ([args (if autosquash
                   (list "-c" "sequence.editor=true" "rebase" "-i" "--autosquash" onto)
                   (list "rebase" onto))])
        (git-run* root args (string-append "Rebased onto " onto))))))

;;; Interactive rebase ;;;
;;;
;;; The todo editor builds a backend-neutral plan; here it becomes a git rebase
;;; todo. We write the projected todo to a file inside the git dir and point
;;; git's own `sequence.editor` at a `cp` that overwrites the todo git presents
;;; with ours - the no-tty equivalent of editing the todo by hand (the same
;;; `-c sequence.editor=...` lever `--autosquash` uses). With GIT_EDITOR forced
;;; to true (process.scm), pick/reorder/drop/squash/fixup complete in one shot;
;;; an `edit` entry or a conflict leaves the rebase paused, which we detect and
;;; report so the status view can drive continue/abort.

(define (git-rebase-interactive root plan)
  (let ([entries (hash-ref plan 'entries)]
        [base (hash-ref plan 'base)])
    (cond
      [(blank? base) (err-result "interactive rebase needs a base revision" #f)]
      [(null? entries) (err-result "interactive rebase: nothing to do" #f)]
      [else
        (let ([invalid (todo-validate entries)])
          (if invalid
            (err-result (string-append "invalid rebase plan: " invalid) #f)
            (git-run-rebase-todo root base entries)))])))

;; Write the projected todo and run `rebase -i`, pointing git's sequence editor
;; at a `cp` of our todo. Reword entries also need new commit messages fed to
;; GIT_EDITOR; with none, the forced GIT_EDITOR=true handles squash/fixup.
(define (git-run-rebase-todo root base entries)
  (let* ([gd (git-dir root)]
         [todo-path (string-append gd "/juju-rebase-todo")]
         ;; The editor value is a shell command, so the path is quoted to
         ;; survive spaces (paths containing double quotes stay unsupported).
         [seq-editor (string-append "sequence.editor=cp \"" todo-path "\"")]
         [messages (todo-reword-messages entries)])
    (write-file! todo-path (string-append (string-join (todo->git-lines entries) "\n") "\n"))
    (if (null? messages)
      (git-rebase-result root
        (run-vcs root "git" (list "-c" seq-editor "rebase" "-i" base))
        "Rebase complete")
      (git-rebase-with-rewords root base seq-editor gd messages))))

;; Feed reword messages through GIT_EDITOR. git invokes the editor once per
;; reword (and per squash group); a small helper reads the file git hands it,
;; leaves squash/fixup combination messages untouched (they carry git's banner),
;; and overwrites a reword's message with the next pre-collected one in order.
(define (git-rebase-with-rewords root base seq-editor gd messages)
  (let ([script (string-append gd "/juju-reword.sh")]
        [counter (string-append gd "/juju-reword-counter")])
    (write-reword-messages! gd messages)
    (write-file! counter "0\n")
    (write-file! script (reword-script gd))
    (git-rebase-result root
      (run-vcs-env root "git" (list "-c" seq-editor "rebase" "-i" base)
        (hash "GIT_EDITOR" (string-append "sh \"" script "\"")))
      "Rebase complete (reworded)")))

(define (write-reword-messages! gd messages)
  (let loop ([ms messages] [i 0])
    (when (pair? ms)
      (write-file! (string-append gd "/juju-reword-msg-" (number->string i))
        (string-append (car ms) "\n"))
      (loop (cdr ms) (+ i 1)))))

;; POSIX sh: skip the squash banner, else cp the next message over git's file and
;; bump the counter. `sh <script>` runs it without needing the execute bit.
(define (reword-script gd)
  (string-append
    "#!/bin/sh\n"
    "f=\"$1\"\n"
    "if head -n 1 \"$f\" | grep -q '^# This is a combination of'; then exit 0; fi\n"
    "d='"
    gd
    "'\n"
    "i=$(cat \"$d/juju-reword-counter\")\n"
    "m=\"$d/juju-reword-msg-$i\"\n"
    "if [ -f \"$m\" ]; then cp \"$m\" \"$f\"; echo $((i + 1)) > \"$d/juju-reword-counter\"; fi\n"))

;; Map a rebase invocation to a result, distinguishing the paused state (an
;; `edit` stop or an unresolved conflict, both leaving the rebase dir behind)
;; from a clean finish and from an outright failure.
(define (git-rebase-result root res success-msg)
  (cond
    [(rebase-in-progress? root)
      (ok-result "Rebase paused (resolve, then continue/abort)" res)]
    [(vcs-ok? res) (ok-result success-msg res)]
    [else (err-result (string-append "git rebase failed: " (result-tail res)) res)]))

(define (git-rebase-step root flag success-msg)
  (if (rebase-in-progress? root)
    (git-rebase-result root (run-vcs root "git" (list "rebase" flag)) success-msg)
    (err-result "no rebase in progress" #f)))

;;@doc #t when a git rebase is mid-flight (merge or apply backend dir present).
(define (rebase-in-progress? root)
  (let ([gd (git-dir root)])
    (or (path-exists? (string-append gd "/rebase-merge"))
      (path-exists? (string-append gd "/rebase-apply")))))

;; Absolute git dir for `root` (handles worktrees, where .git is a file); falls
;; back to <root>/.git if the query fails.
(define (git-dir root)
  (let ([res (run-vcs root "git" (list "rev-parse" "--absolute-git-dir"))])
    (if (vcs-ok? res) (string-trim (vcs-stdout res)) (string-append root "/.git"))))

(define (write-file! path text)
  (let ([port (open-output-file path)])
    (display text port)
    (close-output-port port)))

(define (git-cherry-pick root rev)
  (if (blank? rev)
    (err-result "cherry-pick needs a rev" #f)
    (git-run* root (list "cherry-pick" rev) (string-append "Cherry-picked " rev))))

(define (git-revert root rev)
  (if (blank? rev)
    (err-result "revert needs a rev" #f)
    (git-run* root (list "revert" "--no-edit" rev) (string-append "Reverted " rev))))

;; reset moves HEAD (and, per mode, index/worktree) to `rev` (HEAD when #f).
;; soft keeps index+worktree; mixed (default) resets the index; hard discards
;; worktree changes too (the destructive case the command confirms first).
(define (git-reset root mode rev)
  (let ([flag (cond [(eq? mode 'soft) "--soft"] [(eq? mode 'hard) "--hard"] [else "--mixed"])]
        [target (if (blank? rev) "HEAD" rev)])
    (git-run* root (list "reset" flag target)
      (string-append "Reset (" (symbol->string mode) ") to " target))))

;; Best-effort undo: git has no first-class undo, so this rewinds HEAD by one
;; reflog entry, keeping index and worktree (--soft). It only reverses the last
;; operation that moved HEAD (commit, reset, rebase tip); other operations are
;; out of reach. Labelled in the message so the user knows it is best-effort.
(define (git-undo root)
  (git-run* root (list "reset" "--soft" "HEAD@{1}")
    "Undid last HEAD change (best-effort, via reflog)"))

;;; Branch management and switch ;;;
;;;
;;; `checkout` (not `switch`) is used to move because the target may be a branch
;;; or a bare commit (from the recent section), and checkout handles both, going
;;; detached for a commit. Branch create/rename/delete use porcelain `git
;;; branch`; delete is the safe `-d` (refuses unmerged) so a stray key cannot
;;; lose commits.

(define (git-switch root rev)
  (if (blank? rev)
    (err-result "switch needs a branch or commit" #f)
    (git-run* root (list "checkout" rev) (string-append "Switched to " rev))))

;; Create a branch (optionally at `rev`, else HEAD) without switching to it.
(define (git-branch-create root name rev)
  (if (blank? name)
    (err-result "branch needs a name" #f)
    (git-run* root (append (list "branch" name) (if (blank? rev) '() (list rev)))
      (string-append "Created branch " name))))

;; Create-or-move a branch to `rev` (HEAD when none given). `branch -f` allows
;; moving backwards; git refuses to force the currently checked-out branch, whose
;; error surfaces verbatim through git-run*.
(define (git-branch-set root name rev)
  (if (blank? name)
    (err-result "branch needs a name" #f)
    (git-run* root (append (list "branch" "-f" name) (if (blank? rev) '() (list rev)))
      (string-append "Set branch " name))))

(define (git-branch-rename root old new)
  (if (or (blank? old) (blank? new))
    (err-result "rename needs old and new names" #f)
    (git-run* root (list "branch" "-m" old new)
      (string-append "Renamed " old " to " new))))

;; opts may carry 'force (#t): -D instead of the safe -d.
(define (git-branch-delete root name opts)
  (if (blank? name)
    (err-result "delete needs a branch name" #f)
    (git-run* root (list "branch" (if (opt opts 'force #f) "-D" "-d") name)
      (string-append "Deleted branch " name))))

(define (git-set-upstream root name upstream)
  (if (or (blank? name) (blank? upstream))
    (err-result "set-upstream needs a branch and an upstream" #f)
    (git-run* root (list "branch" (string-append "--set-upstream-to=" upstream) name)
      (string-append "Set upstream of " name " to " upstream))))

;;; Stash ;;;
;;;
;;; stash saves the worktree+index (optionally with a message); pop/apply/drop
;;; act on a stash ref (stash@{N}); when the ref is blank git defaults to the
;;; latest stash@{0}.

(define (git-stash root opts)
  (let* ([message (opt opts 'message #f)]
         [args (append (list "stash" "push") (if (blank? message) '() (list "-m" message)))])
    (git-run* root args "Stashed working changes")))

(define (git-stash-cmd root sub ref)
  (let ([args (append (list "stash" sub) (if (blank? ref) '() (list ref)))])
    (git-run* root args
      (string-append "Stash " sub (if (blank? ref) "" (string-append " " ref))))))

;;; Read-only listings (query-fn) ;;;

(define (git-query b op args)
  (let ([root (backend-root b)])
    (cond
      [(eq? op 'refs) (git-refs root)]
      [(eq? op 'remotes) (run-vcs-lines root "git" (list "remote" "-v"))]
      [(eq? op 'reflog)
        (run-vcs-lines root "git" (list "reflog" (string-append "-n" (number->string (juju-recent-count)))))]
      [(eq? op 'worktrees) (run-vcs-lines root "git" (list "worktree" "list"))]
      [(eq? op 'submodules) (run-vcs-lines root "git" (list "submodule" "status"))]
      [(eq? op 'rebase-range) (git-rebase-range root (car args))]
      [(eq? op 'blame) (git-blame-at root (car args))]
      [else '()]))) ; 'oplog has no git analogue

;; The commit range for the interactive rebase editor, as a hash:
;;   'base    the exclusive base to pass to `git rebase -i` (string or #f)
;;   'commits the commits base..HEAD, newest first (commit-record list)
;; `spec` carries 'from (edit from this commit inclusive -> base is its parent)
;; or 'base (an explicit exclusive base); with neither it defaults to the
;; upstream. The whole range/parent syntax stays here so command code is neutral.
(define (git-rebase-range root spec)
  (let* ([from (opt spec 'from #f)]
         [given (opt spec 'base #f)]
         [base (cond
                [from (string-append from "^")]
                [given given]
                [else (git-default-rebase-base root)])])
    (if (not base)
      (hash 'base #f 'commits '())
      (hash 'base base
        'commits
        (git-log* root (string-append base "..HEAD") (hash 'limit 200))))))

;; The tracked upstream's short name (e.g. "origin/main"), or #f when HEAD has no
;; upstream - in which case the editor asks the user for an explicit base.
(define (git-default-rebase-base root)
  (let ([res (run-vcs root "git"
              (list "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}"))])
    (if (vcs-ok? res)
      (let ([name (string-trim (vcs-stdout res))])
        (if (string=? name "") #f name))
      #f)))

;; Local branches, then tags, then remote-tracking branches, each as one line.
(define (git-refs root)
  (run-vcs-lines root "git"
    (list "for-each-ref" "--format=%(refname:short)%09%(objectname:short)%09%(subject)"
      "refs/heads"
      "refs/tags"
      "refs/remotes")))

;;; Constructor ;;;
;;;
;;; Defined last so it closes over fully-defined operation functions: Steel
;;; miscompiles a constructor that forward-references module-level closures.

;;@doc Build a Git backend rooted at `root`. Mutations are dispatched through
;; `git-mutate`.
(define (make-git-backend root)
  (make-backend 'git root git-capabilities
    git-status
    git-diff
    git-log
    git-show
    git-mutate
    git-query))
