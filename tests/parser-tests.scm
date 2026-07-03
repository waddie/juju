;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; parser-tests.scm - unit tests for juju's pure functions
;;;
;;; The parsers (diff, porcelain-v2 status, jj summary, log records) plus the
;;; row-flattening, fold-state, scroll, and backend-selection logic are all pure:
;;; data in, data out. They are fed canned input, so no live process or Helix
;;; runtime is needed. Run via the wrapper, which feeds this file on stdin so
;;; relative requires resolve from the repo root:
;;;
;;;   tests/run.sh          (equivalently: steel < tests/parser-tests.scm)
;;;
;;; The stdin form matters: `steel tests/parser-tests.scm` (file argument)
;;; resolves requires from ~/.steel/cogs instead and will not find these modules.

(require "cogs/juju/string-utils.scm")
(require "cogs/juju/model.scm")
(require "cogs/juju/diff.scm")
(require "cogs/juju/view-rows.scm")
(require "cogs/juju/operand.scm")
(require "cogs/juju/scroll.scm")
(require "cogs/juju/menu-model.scm")
(require "cogs/juju/backend-interface.scm")
(require "cogs/juju/backend-git.scm")
(require "cogs/juju/backend-jj.scm")
(require "cogs/juju/backend-detect.scm")
(require "cogs/juju/rebase-todo.scm")

(define failures (box 0))
(define checks (box 0))

(define (check label actual expected)
  (set-box! checks (+ (unbox checks) 1))
  (if (equal? actual expected)
    (displayln (string-append "  ok   " label))
    (begin
      (set-box! failures (+ (unbox failures) 1))
      (displayln (string-append "  FAIL " label))
      (displayln (string-append "         expected: " (to-string expected)))
      (displayln (string-append "         actual:   " (to-string actual))))))

;;; string-utils ;;;

(displayln "string-utils:")
(check "split-lines trailing newline" (split-lines "a\nb\n") '("a" "b"))
(check "split-lines no trailing" (split-lines "a\nb") '("a" "b"))
(check "split-lines empty" (split-lines "") '())
(check "string-trim" (string-trim "  hi  ") "hi")
(check "truncate-string" (truncate-string "abcdef" 4) "abc…")
(check "pad-right" (pad-right "ab" 4) "ab  ")

;;; diff parser ;;;

(displayln "diff parser:")
(define DIFF
  (string-append
    "diff --git a/foo b/foo\n"
    "index 000..fb1 100644\n"
    "--- a/foo\n"
    "+++ b/foo\n"
    "@@ -1,3 +1,4 @@ heading\n"
    " ctx\n"
    "-gone\n"
    "+added\n"
    "+more\n"
    " tail\n"))
(define HS (parse-unified-diff DIFF))
(check "one hunk" (length HS) 1)
(check "old range" (hunk-old-range (car HS)) (cons 1 3))
(check "new range" (hunk-new-range (car HS)) (cons 1 4))
(check "line count" (length (hunk-lines (car HS))) 5)
(check "first line is context" (diff-line-kind (car (hunk-lines (car HS)))) 'context)
(check "del line text strips marker"
  (diff-line-text (list-ref (hunk-lines (car HS)) 1))
  "gone")

(define HDR-DEFAULT (parse-hunk-header "@@ -5 +6,2 @@"))
(check "default old count" (hash-ref HDR-DEFAULT 'old-range) (cons 5 1))
(check "explicit new count" (hash-ref HDR-DEFAULT 'new-range) (cons 6 2))

(define RENAME-DIFF
  (string-append
    "diff --git a/old.png b/new.png\n"
    "similarity index 100%\n"
    "rename from old.png\n"
    "rename to new.png\n"
    "Binary files a/old.png and b/new.png differ\n"))
(define RF (parse-diff-flags RENAME-DIFF))
(check "rename detected" (hash-ref RF 'rename?) #t)
(check "rename orig path" (hash-ref RF 'orig-path) "old.png")
(check "binary detected" (hash-ref RF 'binary?) #t)

;;; partial-hunk patch builder ;;;
;;;
;;; DIFF's hunk body is: 0:ctx 1:-gone 2:+added 3:+more 4:tail. Selecting the
;;; deletion and the first addition (body indices 1 and 2) exercises both the
;;; forward rule (drop unselected +, unselected - to context) and the reverse
;;; rule (drop unselected -, unselected + to context).

(define (sel? hi bi) (and (= hi 0) (or (= bi 1) (= bi 2))))
(define PB-HEADERS (diff-header-lines DIFF))
(check "header lines" (length PB-HEADERS) 4)

(check "forward patch"
  (build-apply-patch PB-HEADERS HS sel? 'forward)
  (string-append
    "diff --git a/foo b/foo\n"
    "index 000..fb1 100644\n"
    "--- a/foo\n"
    "+++ b/foo\n"
    "@@ -1,3 +1,3 @@\n"
    " ctx\n"
    "-gone\n"
    "+added\n"
    " tail\n"))

(check "reverse patch (unselected + becomes context)"
  (build-apply-patch PB-HEADERS HS sel? 'reverse)
  (string-append
    "diff --git a/foo b/foo\n"
    "index 000..fb1 100644\n"
    "--- a/foo\n"
    "+++ b/foo\n"
    "@@ -1,4 +1,4 @@\n"
    " ctx\n"
    "-gone\n"
    "+added\n"
    " more\n"
    " tail\n"))

(check "default mode is forward"
  (build-apply-patch PB-HEADERS HS sel?)
  (build-apply-patch PB-HEADERS HS sel? 'forward))

(check "no selection yields #f"
  (build-apply-patch PB-HEADERS HS (lambda (hi bi) #f))
  #f)

;;; porcelain v2 status parser ;;;

(displayln "git porcelain-v2 parser:")
(define PORCELAIN
  (list
    "# branch.oid abcdef0123"
    "# branch.head main"
    "# branch.upstream origin/main"
    "# branch.ab +2 -1"
    "2 R. N... 100644 100644 100644 dcc dcc R100 renamed.txt\ttorename.txt"
    "1 A. N... 000000 100644 100644 000 2ce staged-new.txt"
    "1 .M N... 100644 100644 100644 4cb 4cb tracked.txt"
    "u UU N... 100644 100644 100644 100644 h1 h2 h3 conflicted.txt"
    "? untracked.txt"
    "! ignored.txt"))
(define P (parse-porcelain-status PORCELAIN))
(define BR (hash-ref P 'branch))
(check "head" (hash-ref BR 'head) "main")
(check "upstream" (hash-ref BR 'upstream) "origin/main")
(check "ahead" (hash-ref BR 'ahead) 2)
(check "behind" (hash-ref BR 'behind) 1)

(define FILES (hash-ref P 'files))
(check "file count (ignored excluded)" (length FILES) 5)

(define (find-file path) (car (filter (lambda (f) (string=? (hash-ref f 'path) path)) FILES)))
(check "rename path" (hash-ref (find-file "renamed.txt") 'orig-path) "torename.txt")
(check "rename is staged" (hash-ref (find-file "renamed.txt") 'staged?) #t)
(check "staged-new staged" (hash-ref (find-file "staged-new.txt") 'staged?) #t)
(check "tracked unstaged" (hash-ref (find-file "tracked.txt") 'unstaged?) #t)
(check "tracked not staged" (hash-ref (find-file "tracked.txt") 'staged?) #f)
(check "untracked where" (hash-ref (find-file "untracked.txt") 'where) 'untracked)
(check "conflict where" (hash-ref (find-file "conflicted.txt") 'where) 'conflict)
(check "path with no spaces preserved"
  (hash-ref (find-file "tracked.txt") 'path)
  "tracked.txt")

;; path containing spaces (after-nth-space must preserve it)
(define SPACED (parse-porcelain-status (list "1 .M N... 100644 100644 100644 4cb 4cb my file.txt")))
(check "spaced path preserved"
  (hash-ref (car (hash-ref SPACED 'files)) 'path)
  "my file.txt")

;;; jj diff --summary parser ;;;

(displayln "jj summary parser:")
(define JJ-SUMMARY (parse-jj-summary (list "A added.txt" "M changed.txt" "D gone.txt")))
(check "jj summary count" (length JJ-SUMMARY) 3)
(check "jj added code" (file-item-status-code (car JJ-SUMMARY)) 'added)
(check "jj modified code" (file-item-status-code (list-ref JJ-SUMMARY 1)) 'modified)
(check "jj deleted code" (file-item-status-code (list-ref JJ-SUMMARY 2)) 'deleted)
(check "jj path" (file-item-path (car JJ-SUMMARY)) "added.txt")
(check "jj plain path has no orig"
  (hash-contains? (file-item-extra (car JJ-SUMMARY)) 'orig-path)
  #f)

;; jj emits rename segments as "{old => new}", whole-path or mid-path; the
;; parser resolves the new path and records the old under 'orig-path.
(define JJ-RENAMED (parse-jj-summary (list "R {a.txt => b.txt}" "R dir/{a => b}/f.txt")))
(check "jj rename resolves new path" (file-item-path (car JJ-RENAMED)) "b.txt")
(check "jj rename code" (file-item-status-code (car JJ-RENAMED)) 'renamed)
(check "jj rename orig path"
  (hash-ref (file-item-extra (car JJ-RENAMED)) 'orig-path)
  "a.txt")
(check "jj mid-path rename new" (file-item-path (cadr JJ-RENAMED)) "dir/b/f.txt")
(check "jj mid-path rename orig"
  (hash-ref (file-item-extra (cadr JJ-RENAMED)) 'orig-path)
  "dir/a/f.txt")
;; An empty segment side drops out; the doubled separator collapses.
(define JJ-MOVED (car (parse-jj-summary (list "R dir/{ => sub}/f.txt"))))
(check "jj empty-side rename new" (file-item-path JJ-MOVED) "dir/sub/f.txt")
(check "jj empty-side rename orig"
  (hash-ref (file-item-extra JJ-MOVED) 'orig-path)
  "dir/f.txt")

;;; view-rows ;;;

(displayln "view-rows:")
(define VR-STATUS
  (make-status
    (list (cons "Head" "main"))
    (list
      (make-section "unstaged" "Unstaged changes" 'unstaged
        (list (make-file-item "a.txt" 'modified))
        #f))))

;; Header, blank, section head, file, trailing blank.
(define VR-ROWS (build-rows VR-STATUS (make-fold-state) (hash)))
(check "rows: count (collapsed file)" (length VR-ROWS) 5)
(check "rows: header first" (row-type (list-ref VR-ROWS 0)) 'header)
(check "rows: section row" (row-type (list-ref VR-ROWS 2)) 'section)
(check "rows: file row" (row-type (list-ref VR-ROWS 3)) 'file)
(check "rows: file section kind" (row-section-kind (list-ref VR-ROWS 3)) 'unstaged)

;; A collapsed section shows only its head row.
(define VR-COLLAPSED
  (make-status (list (cons "Head" "main"))
    (list (make-section "unstaged" "Unstaged changes" 'unstaged
           (list (make-file-item "a.txt" 'modified))
           #t))))
(define VR-ROWS2 (build-rows VR-COLLAPSED (make-fold-state) (hash)))
(check "rows: collapsed hides files" (length VR-ROWS2) 3)
(check "rows: no file row when collapsed"
  (filter (lambda (r) (eq? (row-type r) 'file)) VR-ROWS2)
  '())

;; An expanded file emits its cached diff rows, numbered against the new file.
(define VR-FOLD3 (make-fold-state))
(begin (set-fold-file-expanded! VR-FOLD3 'unstaged "a.txt" #t) (void))
(define VR-HUNK
  (make-hunk "@@ -1,1 +1,2 @@" (cons 1 1) (cons 1 2)
    (list (make-diff-line 'context "x") (make-diff-line 'add "y"))))
(define VR-CACHE (hash (diff-cache-key 'unstaged "a.txt") (list VR-HUNK)))
(define VR-DIFFROWS
  (filter (lambda (r) (eq? (row-type r) 'diff))
    (build-rows VR-STATUS VR-FOLD3 VR-CACHE)))
(check "rows: expanded file shows diff" (length VR-DIFFROWS) 3)
(check "rows: diff line numbered" (row-line (list-ref VR-DIFFROWS 2)) 2)

;;; fold state ;;;

(displayln "fold state:")
(define FS (make-fold-state))
(check "fold: section default open" (fold-section-collapsed? FS 'unstaged) #f)
(check "fold: file default closed" (fold-file-expanded? FS 'unstaged "a.txt") #f)
(begin (toggle-fold-section! FS 'unstaged) (void))
(check "fold: toggle section" (fold-section-collapsed? FS 'unstaged) #t)
(begin (set-fold-file-expanded! FS 'unstaged "a.txt" #t) (void))
(check "fold: set file expanded" (fold-file-expanded? FS 'unstaged "a.txt") #t)

;; apply-fold-state overwrites struct flags from the map.
(define FS2 (make-fold-state))
(begin
  (set-fold-section-collapsed! FS2 'unstaged #t)
  (set-fold-file-expanded! FS2 'unstaged "a.txt" #t)
  (void))
(define APPLIED-SEC (section-by-kind (apply-fold-state FS2 VR-STATUS) 'unstaged))
(check "fold: apply sets section collapsed" (section-collapsed? APPLIED-SEC) #t)
(check "fold: apply sets file expanded"
  (file-item-expanded? (car (section-items APPLIED-SEC)))
  #t)

;;; scroll math ;;;

(displayln "scroll:")
(check "clamp-top: cursor in view" (clamp-top 0 3 10 20) 0)
(check "clamp-top: cursor below view" (clamp-top 0 15 10 20) 6)
(check "clamp-top: cursor above top" (clamp-top 5 2 10 20) 2)
(check "clamp-top: never past end" (clamp-top 100 19 10 20) 10)
(check "clamp-top: total under height" (clamp-top 0 0 10 3) 0)

;;; backend selection ;;;

(displayln "backend selection:")
(check "choose: valid override wins" (choose-backend-name '(git jj) 'git 'jj) 'git)
(check "choose: invalid override ignored" (choose-backend-name '(git) 'jj 'jj) 'git)
(check "choose: colocated default jj" (choose-backend-name '(jj git) #f 'jj) 'jj)
(check "choose: colocated default git" (choose-backend-name '(jj git) #f 'git) 'git)
(check "choose: sole jj" (choose-backend-name '(jj) #f 'jj) 'jj)
(check "choose: sole git" (choose-backend-name '(git) #f 'jj) 'git)
(check "choose: none present" (choose-backend-name '() #f 'jj) #f)

;;; backend interface wording ;;;

(displayln "backend interface:")
;; Reversibility of a discard is capability-gated on 'oplog (jj's first-class
;; operation log), never on the backend name.
(define BI-OPLOG (make-backend 'jj "/" '(oplog discard) #f #f #f #f))
(define BI-PLAIN (make-backend 'git "/" '(discard) #f #f #f #f))
(check "discard note with oplog" (discard-confirm-note BI-OPLOG) "(undo reverses it)")
(check "discard note without oplog" (discard-confirm-note BI-PLAIN) "This cannot be undone")

;;; commit-row operands (resolve-revs) ;;;

(displayln "operand resolve-revs:")
(define RV-STATUS
  (make-status (list (cons "Head" "main"))
    (list
      (make-section "recent" "Recent commits" 'recent
        (list (make-commit-record "abc123" "abc" "me" "now" "first" '())
          (make-commit-record "def456" "def" "me" "now" "second" '()))
        #f))))
;; rows: 0 header, 1 blank, 2 section, 3 commit, 4 commit, 5 blank.
(define RV-ROWS (build-rows RV-STATUS (make-fold-state) (hash)))
(define RV1 (resolve-revs RV-ROWS '(3)))
(check "resolve-revs single commit" (length RV1) 1)
(check "resolve-revs rev id" (hash-ref (car RV1) 'rev) "abc123")
(check "resolve-revs kind" (hash-ref (car RV1) 'kind) 'recent)
(check "resolve-revs section -> all commits" (length (resolve-revs RV-ROWS '(2))) 2)
(check "resolve-revs dedupe section+commit" (length (resolve-revs RV-ROWS '(2 3))) 2)
(check "resolve-revs non-commit row empty" (resolve-revs RV-ROWS '(0)) '())

;;; git stash list parser ;;;

(displayln "git stash parser:")
(define SL (parse-stash-line "stash@{0}: WIP on main: tweak"))
(check "stash ref" (commit-record-id SL) "stash@{0}")
(check "stash subject" (commit-record-subject SL) "WIP on main: tweak")
(check "stash no-colon falls back"
  (commit-record-id (parse-stash-line "weird"))
  "weird")

;;; jj conflict parser ;;;

(displayln "jj conflict parser:")
(define CL (parse-jj-conflict-line "src/a.txt    2-sided conflict"))
(check "conflict path" (file-item-path CL) "src/a.txt")
(check "conflict code" (file-item-status-code CL) 'conflicted)
(check "conflict blank -> #f" (parse-jj-conflict-line "   ") #f)

;;; transient menu model ;;;

(displayln "menu model:")
(define MENU-ENTRIES
  (list
    (menu-info "Rebase")
    (menu-switch #\a 'autosquash "--autosquash" #f)
    (menu-action #\o "onto a ref" (lambda (switches) 'ran))))
(check "initial switches from defaults"
  (hash-ref (initial-switches MENU-ENTRIES) 'autosquash)
  #f)
(define MENU-SW (hash-insert (initial-switches MENU-ENTRIES) 'autosquash #t))
(define MENU-ROWS (menu-rows MENU-ENTRIES MENU-SW))
(check "menu row count" (length MENU-ROWS) 3)
(check "info row tag" (hash-ref (list-ref MENU-ROWS 0) 'tag) 'info)
(check "switch on shows [x]"
  (hash-ref (list-ref MENU-ROWS 1) 'text)
  "  a  [x] --autosquash")
(check "switch on row tag" (hash-ref (list-ref MENU-ROWS 1) 'tag) 'section)
(check "switch off shows [ ]"
  (hash-ref (list-ref (menu-rows MENU-ENTRIES (initial-switches MENU-ENTRIES)) 1) 'text)
  "  a  [ ] --autosquash")
(check "action row text" (hash-ref (list-ref MENU-ROWS 2) 'text) "  o  onto a ref")
(check "entry key of info is #f" (menu-entry-key (car MENU-ENTRIES)) #f)
(check "entry key of switch" (menu-entry-key (list-ref MENU-ENTRIES 1)) #\a)

;;; rebase todo model ;;;

(displayln "rebase todo:")
;; backend-log yields newest-first; make-todo reverses to oldest-first.
(define RC-A (make-commit-record "aaaa1111" "aaaa" "ann" "1d" "add a" '()))
(define RC-B (make-commit-record "bbbb2222" "bbbb" "ben" "1d" "add b" '()))
(define RC-C (make-commit-record "cccc3333" "cccc" "cat" "1d" "add c" '()))
(define TODO (make-todo (list RC-C RC-B RC-A))) ; newest C first
(check "todo length" (length TODO) 3)
(check "todo oldest first" (commit-record-id (todo-entry-commit (car TODO))) "aaaa1111")
(check "todo default pick" (todo-entry-action (car TODO)) 'pick)

(define TODO-SET (todo-set-action TODO 1 'squash))
(check "set-action at 1" (todo-entry-action (list-ref TODO-SET 1)) 'squash)
(check "set-action leaves others" (todo-entry-action (list-ref TODO-SET 0)) 'pick)

(define TODO-MOVED (todo-move-up TODO 2)) ; C up past B
(check "move-up swaps" (commit-record-id (todo-entry-commit (list-ref TODO-MOVED 1))) "cccc3333")
(check "move-up at top no-op" (todo-move-up TODO 0) TODO)
(check "move-down at bottom no-op" (todo-move-down TODO 2) TODO)

(check "validate ok" (todo-validate TODO) #f)
(check "validate all-drop"
  (todo-validate (todo-set-action (todo-set-action (todo-set-action TODO 0 'drop) 1 'drop) 2 'drop))
  "rebase would drop every commit")
(check "validate first squash"
  (todo-validate (todo-set-action TODO 0 'squash))
  "first commit cannot be squash or fixup")
;; A squash with no surviving commit before it is rejected even after a drop.
(check "validate drop then squash rejected"
  (todo-validate (todo-set-action (todo-set-action TODO 0 'drop) 1 'squash))
  "first commit cannot be squash or fixup")
;; pick, drop, squash is valid: the squash folds into the leading pick.
(check "validate pick drop squash ok"
  (todo-validate (todo-set-action (todo-set-action TODO 1 'drop) 2 'squash))
  #f)

;; git lines: oldest-first, "<action> <full-id> <subject>".
(define GL (todo->git-lines (todo-set-action (todo-set-action TODO 1 'squash) 2 'drop)))
(check "git line 0 pick" (list-ref GL 0) "pick aaaa1111 add a")
(check "git line 1 squash" (list-ref GL 1) "squash bbbb2222 add b")
(check "git line 2 drop" (list-ref GL 2) "drop cccc3333 add c")

;; jj steps: fold first, then drop, then reorder.
(define JJ-SQUASH (todo->jj-steps (todo-set-action TODO 1 'squash)))
(check "jj squash --from/--into" (car JJ-SQUASH)
  (list "squash" "--from" "bbbb2222" "--into" "aaaa1111"))
(define JJ-FIXUP (todo->jj-steps (todo-set-action TODO 1 'fixup)))
(check "jj fixup uses destination message" (car JJ-FIXUP)
  (list "squash" "--from" "bbbb2222" "--into" "aaaa1111" "--use-destination-message"))
(check "jj drop abandons" (car (todo->jj-steps (todo-set-action TODO 1 'drop)))
  (list "abandon" "bbbb2222"))
;; reorder a 3-commit pick list (no folds/drops) -> rebase each onto its predecessor.
(define JJ-REORDER (todo->jj-steps TODO))
(check "jj reorder step count" (length JJ-REORDER) 2)
(check "jj reorder 1" (list-ref JJ-REORDER 0)
  (list "rebase" "-r" "bbbb2222" "--insert-after" "aaaa1111"))
(check "jj reorder 2" (list-ref JJ-REORDER 1)
  (list "rebase" "-r" "cccc3333" "--insert-after" "bbbb2222"))
(check "jj reword describes"
  (car (todo->jj-steps (todo-set-message (todo-set-action TODO 1 'reword) 1 "new msg")))
  (list "describe" "bbbb2222" "-m" "new msg"))
;; A reword with no message falls back to pick in the git todo (keeps alignment).
(check "git reword without message -> pick"
  (list-ref (todo->git-lines (todo-set-action TODO 1 'reword)) 1)
  "pick bbbb2222 add b")
(check "git reword with message"
  (list-ref (todo->git-lines (todo-set-message (todo-set-action TODO 1 'reword) 1 "x")) 1)
  "reword bbbb2222 add b")
(check "reword messages in plan order"
  (todo-reword-messages
    (todo-set-message (todo-set-action
                       (todo-set-message (todo-set-action TODO 0 'reword) 0 "first")
                       2
                       'reword)
      2
      "third"))
  (list "first" "third"))
(check "reword messages skips message-less reword"
  (todo-reword-messages (todo-set-action TODO 1 'reword))
  '())
(check "jj edit parks @"
  (last (todo->jj-steps (todo-set-action TODO 2 'edit)))
  (list "edit" "cccc3333"))

;; reword commit collection by index.
(define RW (todo-reword-commits (todo-set-action (todo-set-action TODO 0 'reword) 2 'reword)))
(check "reword pairs count" (length RW) 2)
(check "reword first idx" (car (car RW)) 0)
(check "reword second idx" (car (cadr RW)) 2)

;; display rows.
(define TR (todo-rows (todo-set-action TODO 1 'drop) 0 '()))
(check "row count" (length TR) 3)
(check "row text picks" (hash-ref (list-ref TR 0) 'text) "pick   aaaa         add a")
(check "row tag drop dim" (hash-ref (list-ref TR 1) 'tag) 'info)
(check "row tag pick" (hash-ref (list-ref TR 0) 'tag) 'commit)

;;; menu model: arg infix ;;;

(displayln "menu model:")
(define MENU
  (list (menu-info "T")
    (menu-switch #\a 'auto "Auto" #f)
    (menu-arg #\n 'count "-n count" "10")
    (menu-action #\x "Go" (lambda (s) s))))
(define MENU-SW (initial-switches MENU))
(check "initial-switches seeds switch default" (hash-ref MENU-SW 'auto) #f)
(check "initial-switches seeds arg default" (hash-ref MENU-SW 'count) "10")
(define MENU-ROWS (menu-rows MENU MENU-SW))
(check "switch row off" (hash-ref (list-ref MENU-ROWS 1) 'text) "  a  [ ] Auto")
(check "arg row shows value" (hash-ref (list-ref MENU-ROWS 2) 'text) "  n  -n count: 10")
(check "arg row set tag" (hash-ref (list-ref MENU-ROWS 2) 'tag) 'section)

(define MENU-UNSET (list (menu-arg #\n 'count "-n count" #f)))
(define MENU-UNSET-SW (initial-switches MENU-UNSET))
(check "arg unset default" (hash-ref MENU-UNSET-SW 'count) #f)
(define MENU-UNSET-ROWS (menu-rows MENU-UNSET MENU-UNSET-SW))
(check "arg row unset text"
  (hash-ref (list-ref MENU-UNSET-ROWS 0) 'text)
  "  n  -n count: (unset)")
(check "arg row unset tag" (hash-ref (list-ref MENU-UNSET-ROWS 0) 'tag) 'file)

;;; view-rows: navigation and search ;;;

(displayln "nav / search:")
(define NAV-ROWS
  (list
    (make-row 'header 'header-label "Head:" #f #f #f) ; 0
    (make-row 'blank 'info "" #f #f #f) ; 1
    (make-row 'section 'section "Untracked" #f 'untracked #t) ; 2
    (make-row 'file 'file "  a.txt" #f 'untracked #t) ; 3
    (make-row 'section 'section "Staged" #f 'staged #t) ; 4
    (make-row 'file 'file "  b.txt" #f 'staged #t) ; 5
    (make-row 'commit 'commit "  c1 subject" #f 'recent #t))) ; 6
(check "section indices" (section-row-indices NAV-ROWS) '(2 4))
(check "next section from top" (next-section-index NAV-ROWS 0) 2)
(check "next section from file" (next-section-index NAV-ROWS 3) 4)
(check "next section at last clamps" (next-section-index NAV-ROWS 6) 6)
(check "prev section from end" (prev-section-index NAV-ROWS 6) 4)
(check "prev section from file" (prev-section-index NAV-ROWS 3) 2)
(check "prev section at first clamps" (prev-section-index NAV-ROWS 2) 2)
(check "parent from file" (parent-section-index NAV-ROWS 5) 4)
(check "parent from section is self" (parent-section-index NAV-ROWS 4) 4)
(check "parent before first section clamps" (parent-section-index NAV-ROWS 1) 1)
(check "search by substring" (search-matches NAV-ROWS "txt") '(3 5))
(check "search case-insensitive" (search-matches NAV-ROWS "TXT") '(3 5))
(check "search header text" (search-matches NAV-ROWS "staged") '(4))
(check "search blank query" (search-matches NAV-ROWS "") '())
(check "search no match" (search-matches NAV-ROWS "zzz") '())

;; nearest-selectable-index: rows 0-1 are not selectable, 2-6 are.
(check "nearest: selectable row is itself" (nearest-selectable-index NAV-ROWS 3 1) 3)
(check "nearest: scans forward" (nearest-selectable-index NAV-ROWS 0 1) 2)
(check "nearest: falls back opposite" (nearest-selectable-index NAV-ROWS 1 -1) 2)
(check "nearest: clamps past end" (nearest-selectable-index NAV-ROWS 99 1) 6)
(check "nearest: none selectable"
  (nearest-selectable-index (list (make-row 'blank 'info "" #f #f #f)) 0 1)
  #f)

;;; blame parsers ;;;

(displayln "blame parsers:")
(define SHA-A "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(define SHA-B "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
(define GIT-BLAME
  (string-append
    SHA-A
    " 1 1 2\n"
    "author Alice\n"
    "author-mail <alice@example.com>\n"
    "summary first commit\n"
    "filename f.txt\n"
    "\tline one\n"
    SHA-A
    " 2 2\n"
    "\tline two\n"
    SHA-B
    " 5 3 1\n"
    "author Bob\n"
    "summary second commit\n"
    "previous "
    SHA-A
    " f.txt\n"
    "filename f.txt\n"
    "\tline three\n"))
(define GIT-BLAMED (parse-git-blame-porcelain GIT-BLAME))
(check "git blame count" (length GIT-BLAMED) 3)
(check "git blame first record"
  (car GIT-BLAMED)
  (make-blame-line SHA-A "aaaaaaaa" 1 "line one"))
(check "git blame repeated header keeps sha"
  (blame-line-commit (list-ref GIT-BLAMED 1))
  SHA-A)
(check "git blame repeated header orig line"
  (blame-line-orig-line (list-ref GIT-BLAMED 1))
  2)
(check "git blame second commit"
  (list-ref GIT-BLAMED 2)
  (make-blame-line SHA-B "bbbbbbbb" 5 "line three"))

(define US (string (integer->char 31)))
(define JJ-ANNOTATED
  (parse-jj-annotate
    (string-append
      "changeidone"
      US
      "chgone"
      US
      "1"
      US
      "alpha\n"
      "changeidone"
      US
      "chgone"
      US
      "2"
      US
      "\n"
      "changeidtwo"
      US
      "chgtwo"
      US
      "1"
      US
      "beta"
      US
      "gamma\n")))
(check "jj annotate count" (length JJ-ANNOTATED) 3)
(check "jj annotate first record"
  (car JJ-ANNOTATED)
  (make-blame-line "changeidone" "chgone" 1 "alpha"))
(check "jj annotate empty content" (blame-line-text (list-ref JJ-ANNOTATED 1)) "")
(check "jj annotate rejoins stray separators"
  (blame-line-text (list-ref JJ-ANNOTATED 2))
  (string-append "beta" US "gamma"))

;;; blame rows ;;;

(displayln "blame rows:")
(define BLAME-VIEW-ROWS
  (blame-rows
    (list (make-blame-line "A" "aa" 1 "one")
      (make-blame-line "A" "aa" 2 "two")
      (make-blame-line "B" "bb" 3 "three")
      (make-blame-line "A" "aa" 4 "four"))))
(check "blame row text pads short id"
  (hash-ref (car BLAME-VIEW-ROWS) 'text)
  "aa       one")
(check "blame row tags mark run starts"
  (map (lambda (r) (hash-ref r 'tag)) BLAME-VIEW-ROWS)
  '(commit file commit commit))

;;; log rows ;;;

(displayln "log rows:")
(define LOG-COMMIT
  (make-commit-record "changeidone" "abc123" "A. Author" "3 days ago" "subject one" '()))
(define LOG-COMMIT-REFS
  (make-commit-record "changeidtwo" "def456" "A. Author" "2 days ago" "subject two" '("main" "wip")))
(define LOG-VIEW-ROWS (log-rows (list LOG-COMMIT LOG-COMMIT-REFS)))
(check "log row count" (length LOG-VIEW-ROWS) 2)
(check "log row text pads id and date"
  (row-text (car LOG-VIEW-ROWS))
  "abc123       3 days ago       subject one")
(check "log row appends refs"
  (row-text (cadr LOG-VIEW-ROWS))
  "def456       2 days ago       subject two  (main, wip)")
(check "log row carries the commit-record" (row-object (car LOG-VIEW-ROWS)) LOG-COMMIT)
(check "log row is a selectable commit row"
  (list (row-type (car LOG-VIEW-ROWS)) (row-tag (car LOG-VIEW-ROWS))
    (row-section-kind (car LOG-VIEW-ROWS))
    (row-selectable? (car LOG-VIEW-ROWS)))
  '(commit commit log #t))

;;; capabilities ;;;

(displayln "capabilities:")
(define JJ-B (make-jj-backend "/tmp/x"))
(define GIT-B (make-git-backend "/tmp/x"))
(check "jj supports edit" (backend-supports? JJ-B 'edit) #t)
(check "jj supports switch" (backend-supports? JJ-B 'switch) #t)
(check "git does not support edit" (backend-supports? GIT-B 'edit) #f)
(check "git supports switch" (backend-supports? GIT-B 'switch) #t)
(check "git supports push-set-upstream" (backend-supports? GIT-B 'push-set-upstream) #t)
(check "git supports branch-force-delete" (backend-supports? GIT-B 'branch-force-delete) #t)
(check "jj does not support branch-force-delete" (backend-supports? JJ-B 'branch-force-delete) #f)
(check "jj supports rebase-skip-emptied" (backend-supports? JJ-B 'rebase-skip-emptied) #t)
(check "git does not support rebase-skip-emptied" (backend-supports? GIT-B 'rebase-skip-emptied) #f)

;;; summary ;;;

(newline)
(displayln (string-append "ran " (number->string (unbox checks)) " checks, "
            (number->string (unbox failures))
            " failures"))
(when (> (unbox failures) 0)
  (error "parser tests failed"))
