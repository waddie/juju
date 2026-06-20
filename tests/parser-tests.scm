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
(require "cogs/juju/backend-git.scm")
(require "cogs/juju/backend-jj.scm")
(require "cogs/juju/backend-detect.scm")

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

;;; summary ;;;

(newline)
(displayln (string-append "ran " (number->string (unbox checks)) " checks, "
            (number->string (unbox failures))
            " failures"))
(when (> (unbox failures) 0)
  (error "parser tests failed"))
