;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; test-view-rows.scm - status flattening, fold state, navigation, log rows
;;;
;;; The pure projection from a status struct to the flat row list, the
;;; fold-state accessors that drive it, the section/search navigation, and
;;; the log-row projection.
;;;
;;; Run in file mode from the repo root: steel tests/test-view-rows.scm
;;; Requires the steel-test package in ~/.steel/cogs.

(require "steel-test/test.scm")
(require "../cogs/juju/model.scm")
(require "../cogs/juju/view-rows.scm")

(define VR-STATUS
  (make-status
    (list (cons "Head" "main"))
    (list
      (make-section "unstaged" "Unstaged changes" 'unstaged
        (list (make-file-item "a.txt" 'modified))
        #f))))

;; Header, blank, section head, file, trailing blank.
(define VR-ROWS (build-rows VR-STATUS (make-fold-state) (hash)))

;; A collapsed section shows only its head row.
(define VR-COLLAPSED
  (make-status (list (cons "Head" "main"))
    (list (make-section "unstaged" "Unstaged changes" 'unstaged
           (list (make-file-item "a.txt" 'modified))
           #t))))
(define VR-ROWS2 (build-rows VR-COLLAPSED (make-fold-state) (hash)))

;; An expanded file emits its cached diff rows, numbered against the new file.
(define VR-FOLD3 (make-fold-state))
(set-fold-file-expanded! VR-FOLD3 'unstaged "a.txt" #t)
(define VR-HUNK
  (make-hunk "@@ -1,1 +1,2 @@" (cons 1 1) (cons 1 2)
    (list (make-diff-line 'context "x") (make-diff-line 'add "y"))))
(define VR-CACHE (hash (diff-cache-key 'unstaged "a.txt") (list VR-HUNK)))
(define VR-DIFFROWS
  (filter (lambda (r) (eq? (row-type r) 'diff))
    (build-rows VR-STATUS VR-FOLD3 VR-CACHE)))

(deftest status-flattening
  (testing "expanded (collapsed file) status"
    (is (= 5 (length VR-ROWS)))
    (is (= 'header (row-type (list-ref VR-ROWS 0))))
    (is (= 'section (row-type (list-ref VR-ROWS 2))))
    (is (= 'file (row-type (list-ref VR-ROWS 3))))
    (is (= 'unstaged (row-section-kind (list-ref VR-ROWS 3)))))
  (testing "collapsed section hides its files"
    (is (= 3 (length VR-ROWS2)))
    (is (= '() (filter (lambda (r) (eq? (row-type r) 'file)) VR-ROWS2))))
  (testing "expanded file shows numbered diff rows"
    (is (= 3 (length VR-DIFFROWS)))
    (is (= 2 (row-line (list-ref VR-DIFFROWS 2))))))

;; apply-fold-state overwrites struct flags from the map.
(define FS2 (make-fold-state))
(set-fold-section-collapsed! FS2 'unstaged #t)
(set-fold-file-expanded! FS2 'unstaged "a.txt" #t)
(define APPLIED-SEC (section-by-kind (apply-fold-state FS2 VR-STATUS) 'unstaged))

(deftest fold-state
  (let ([fs (make-fold-state)])
    (testing "defaults"
      (is (= #f (fold-section-collapsed? fs 'unstaged)))
      (is (= #f (fold-file-expanded? fs 'unstaged "a.txt"))))
    (toggle-fold-section! fs 'unstaged)
    (is (= #t (fold-section-collapsed? fs 'unstaged)))
    (set-fold-file-expanded! fs 'unstaged "a.txt" #t)
    (is (= #t (fold-file-expanded? fs 'unstaged "a.txt"))))
  (testing "apply-fold-state writes flags onto the status"
    (is (= #t (section-collapsed? APPLIED-SEC)))
    (is (= #t (file-item-expanded? (car (section-items APPLIED-SEC)))))))

(define NAV-ROWS
  (list
    (make-row 'header 'header-label "Head:" #f #f #f) ; 0
    (make-row 'blank 'info "" #f #f #f) ; 1
    (make-row 'section 'section "Untracked" #f 'untracked #t) ; 2
    (make-row 'file 'file "  a.txt" #f 'untracked #t) ; 3
    (make-row 'section 'section "Staged" #f 'staged #t) ; 4
    (make-row 'file 'file "  b.txt" #f 'staged #t) ; 5
    (make-row 'commit 'commit "  c1 subject" #f 'recent #t))) ; 6

(deftest navigation
  (testing "section indices"
    (is (= '(2 4) (section-row-indices NAV-ROWS))))
  (testing "next-section"
    (is (= 2 (next-section-index NAV-ROWS 0)))
    (is (= 4 (next-section-index NAV-ROWS 3)))
    (is (= 6 (next-section-index NAV-ROWS 6))))
  (testing "prev-section"
    (is (= 4 (prev-section-index NAV-ROWS 6)))
    (is (= 2 (prev-section-index NAV-ROWS 3)))
    (is (= 2 (prev-section-index NAV-ROWS 2))))
  (testing "parent-section"
    (is (= 4 (parent-section-index NAV-ROWS 5)))
    (is (= 4 (parent-section-index NAV-ROWS 4)))
    (is (= 1 (parent-section-index NAV-ROWS 1)))))

(deftest search
  (is (= '(3 5) (search-matches NAV-ROWS "txt")))
  (testing "case-insensitive"
    (is (= '(3 5) (search-matches NAV-ROWS "TXT"))))
  (is (= '(4) (search-matches NAV-ROWS "staged")))
  (testing "blank query and no match"
    (is (= '() (search-matches NAV-ROWS "")))
    (is (= '() (search-matches NAV-ROWS "zzz")))))

(deftest nearest-selectable
  ;; rows 0-1 are not selectable, 2-6 are.
  (is (= 3 (nearest-selectable-index NAV-ROWS 3 1)))
  (testing "scans forward"
    (is (= 2 (nearest-selectable-index NAV-ROWS 0 1))))
  (testing "falls back to the opposite direction"
    (is (= 2 (nearest-selectable-index NAV-ROWS 1 -1))))
  (testing "clamps past the end"
    (is (= 6 (nearest-selectable-index NAV-ROWS 99 1))))
  (testing "none selectable yields #f"
    (is (= #f (nearest-selectable-index (list (make-row 'blank 'info "" #f #f #f)) 0 1)))))

(define LOG-COMMIT
  (make-commit-record "changeidone" "abc123" "A. Author" "3 days ago" "subject one" '()))
(define LOG-COMMIT-REFS
  (make-commit-record "changeidtwo" "def456" "A. Author" "2 days ago" "subject two" '("main" "wip")))
(define LOG-VIEW-ROWS (log-rows (list LOG-COMMIT LOG-COMMIT-REFS)))

(deftest log-row-projection
  (is (= 2 (length LOG-VIEW-ROWS)))
  (testing "text pads id and date"
    (is (= "abc123       3 days ago       subject one"
         (row-text (car LOG-VIEW-ROWS)))))
  (testing "refs appended"
    (is (= "def456       2 days ago       subject two  (main, wip)"
         (row-text (cadr LOG-VIEW-ROWS)))))
  (testing "row carries its commit-record and is selectable"
    (is (= LOG-COMMIT (row-object (car LOG-VIEW-ROWS))))
    (is (= '(commit commit log #t)
         (list (row-type (car LOG-VIEW-ROWS)) (row-tag (car LOG-VIEW-ROWS))
           (row-section-kind (car LOG-VIEW-ROWS))
           (row-selectable? (car LOG-VIEW-ROWS)))))))

(run-tests!)
