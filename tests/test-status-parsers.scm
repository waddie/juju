;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; test-status-parsers.scm - backend status parsers (data in, data out)
;;;
;;; Git porcelain-v2 status, jj diff --summary, git stash list, and the jj
;;; conflict line, all fed canned output.
;;;
;;; Run in file mode from the repo root: steel tests/test-status-parsers.scm
;;; Requires the steel-test package in ~/.steel/cogs.

(require "steel-test/test.scm")
(require "../cogs/juju/model.scm")
(require "../cogs/juju/backend-git.scm")
(require "../cogs/juju/backend-jj.scm")

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
(define FILES (hash-ref P 'files))
(define (find-file path)
  (car (filter (lambda (f) (string=? (hash-ref f 'path) path)) FILES)))
(define SPACED
  (parse-porcelain-status (list "1 .M N... 100644 100644 100644 4cb 4cb my file.txt")))

(deftest git-porcelain-v2
  (testing "branch header"
    (is (= "main" (hash-ref BR 'head)))
    (is (= "origin/main" (hash-ref BR 'upstream)))
    (is (= 2 (hash-ref BR 'ahead)))
    (is (= 1 (hash-ref BR 'behind))))
  (testing "ignored entries excluded"
    (is (= 5 (length FILES))))
  (testing "rename entry"
    (is (= "torename.txt" (hash-ref (find-file "renamed.txt") 'orig-path)))
    (is (= #t (hash-ref (find-file "renamed.txt") 'staged?))))
  (testing "staged / unstaged split"
    (is (= #t (hash-ref (find-file "staged-new.txt") 'staged?)))
    (is (= #t (hash-ref (find-file "tracked.txt") 'unstaged?)))
    (is (= #f (hash-ref (find-file "tracked.txt") 'staged?))))
  (testing "where classification"
    (is (= 'untracked (hash-ref (find-file "untracked.txt") 'where)))
    (is (= 'conflict (hash-ref (find-file "conflicted.txt") 'where))))
  (testing "paths preserved"
    (is (= "tracked.txt" (hash-ref (find-file "tracked.txt") 'path)))
    ;; after-nth-space must keep an embedded space
    (is (= "my file.txt" (hash-ref (car (hash-ref SPACED 'files)) 'path)))))

(define JJ-SUMMARY (parse-jj-summary (list "A added.txt" "M changed.txt" "D gone.txt")))
;; jj emits rename segments as "{old => new}", whole-path or mid-path; the
;; parser resolves the new path and records the old under 'orig-path.
(define JJ-RENAMED (parse-jj-summary (list "R {a.txt => b.txt}" "R dir/{a => b}/f.txt")))
;; An empty segment side drops out; the doubled separator collapses.
(define JJ-MOVED (car (parse-jj-summary (list "R dir/{ => sub}/f.txt"))))

(deftest jj-summary
  (testing "status codes and path"
    (is (= 3 (length JJ-SUMMARY)))
    (is (= 'added (file-item-status-code (car JJ-SUMMARY))))
    (is (= 'modified (file-item-status-code (list-ref JJ-SUMMARY 1))))
    (is (= 'deleted (file-item-status-code (list-ref JJ-SUMMARY 2))))
    (is (= "added.txt" (file-item-path (car JJ-SUMMARY))))
    (is (= #f (hash-contains? (file-item-extra (car JJ-SUMMARY)) 'orig-path))))
  (testing "rename segments"
    (is (= "b.txt" (file-item-path (car JJ-RENAMED))))
    (is (= 'renamed (file-item-status-code (car JJ-RENAMED))))
    (is (= "a.txt" (hash-ref (file-item-extra (car JJ-RENAMED)) 'orig-path)))
    (is (= "dir/b/f.txt" (file-item-path (cadr JJ-RENAMED))))
    (is (= "dir/a/f.txt" (hash-ref (file-item-extra (cadr JJ-RENAMED)) 'orig-path))))
  (testing "empty rename side collapses the separator"
    (is (= "dir/sub/f.txt" (file-item-path JJ-MOVED)))
    (is (= "dir/f.txt" (hash-ref (file-item-extra JJ-MOVED) 'orig-path)))))

(deftest git-stash-list
  (let ([sl (parse-stash-line "stash@{0}: WIP on main: tweak")])
    (is (= "stash@{0}" (commit-record-id sl)))
    (is (= "WIP on main: tweak" (commit-record-subject sl))))
  (testing "no colon falls back to the whole line"
    (is (= "weird" (commit-record-id (parse-stash-line "weird"))))))

(deftest jj-conflict-line
  (let ([cl (parse-jj-conflict-line "src/a.txt    2-sided conflict")])
    (is (= "src/a.txt" (file-item-path cl)))
    (is (= 'conflicted (file-item-status-code cl))))
  (testing "blank line yields #f"
    (is (= #f (parse-jj-conflict-line "   ")))))

(run-tests!)
