;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; test-blame.scm - git blame porcelain / jj annotate parsers and blame rows
;;;
;;; Run in file mode from the repo root: steel tests/test-blame.scm
;;; Requires the steel-test package in ~/.steel/cogs.

(require "steel-test/test.scm")
(require "../cogs/juju/model.scm")
(require "../cogs/juju/backend-git.scm")
(require "../cogs/juju/backend-jj.scm")
(require "../cogs/juju/view-rows.scm")

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

(deftest git-blame-porcelain
  (is (= 3 (length GIT-BLAMED)))
  (is (= (make-blame-line SHA-A "aaaaaaaa" 1 "line one") (car GIT-BLAMED)))
  (testing "a repeated abbreviated header keeps the full sha"
    (is (= SHA-A (blame-line-commit (list-ref GIT-BLAMED 1))))
    (is (= 2 (blame-line-orig-line (list-ref GIT-BLAMED 1)))))
  (is (= (make-blame-line SHA-B "bbbbbbbb" 5 "line three") (list-ref GIT-BLAMED 2))))

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

(deftest jj-annotate
  (is (= 3 (length JJ-ANNOTATED)))
  (is (= (make-blame-line "changeidone" "chgone" 1 "alpha") (car JJ-ANNOTATED)))
  (testing "empty content"
    (is (= "" (blame-line-text (list-ref JJ-ANNOTATED 1)))))
  (testing "stray separators in content are rejoined"
    (is (= (string-append "beta" US "gamma")
         (blame-line-text (list-ref JJ-ANNOTATED 2))))))

(define BLAME-VIEW-ROWS
  (blame-rows
    (list (make-blame-line "A" "aa" 1 "one")
      (make-blame-line "A" "aa" 2 "two")
      (make-blame-line "B" "bb" 3 "three")
      (make-blame-line "A" "aa" 4 "four"))))

(deftest blame-row-projection
  (testing "text pads a short id"
    (is (= "aa       one" (hash-ref (car BLAME-VIEW-ROWS) 'text))))
  (testing "run starts are tagged"
    (is (= '(commit file commit commit)
         (map (lambda (r) (hash-ref r 'tag)) BLAME-VIEW-ROWS)))))

(run-tests!)
