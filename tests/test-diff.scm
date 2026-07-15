;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; test-diff.scm - unified-diff parser and partial-hunk patch builder
;;;
;;; Run in file mode from the repo root: steel tests/test-diff.scm
;;; Requires the steel-test package in ~/.steel/cogs.

(require "steel-test/test.scm")
(require "../cogs/juju/model.scm")
(require "../cogs/juju/diff.scm")

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

(deftest unified-diff
  (is (= 1 (length HS)))
  (is (= (cons 1 3) (hunk-old-range (car HS))))
  (is (= (cons 1 4) (hunk-new-range (car HS))))
  (is (= 5 (length (hunk-lines (car HS)))))
  (is (= 'context (diff-line-kind (car (hunk-lines (car HS))))))
  (testing "deletion line strips its marker"
    (is (= "gone" (diff-line-text (list-ref (hunk-lines (car HS)) 1))))))

(define HDR-DEFAULT (parse-hunk-header "@@ -5 +6,2 @@"))

(deftest hunk-header
  (testing "absent count defaults to 1"
    (is (= (cons 5 1) (hash-ref HDR-DEFAULT 'old-range))))
  (is (= (cons 6 2) (hash-ref HDR-DEFAULT 'new-range))))

(define RENAME-DIFF
  (string-append
    "diff --git a/old.png b/new.png\n"
    "similarity index 100%\n"
    "rename from old.png\n"
    "rename to new.png\n"
    "Binary files a/old.png and b/new.png differ\n"))
(define RF (parse-diff-flags RENAME-DIFF))

(deftest diff-flags
  (is (= #t (hash-ref RF 'rename?)))
  (is (= "old.png" (hash-ref RF 'orig-path)))
  (is (= #t (hash-ref RF 'binary?))))

;; DIFF's hunk body is: 0:ctx 1:-gone 2:+added 3:+more 4:tail. Selecting the
;; deletion and the first addition (body indices 1 and 2) exercises both the
;; forward rule (drop unselected +, unselected - to context) and the reverse
;; rule (drop unselected -, unselected + to context).
(define (sel? hi bi) (and (= hi 0) (or (= bi 1) (= bi 2))))
(define PB-HEADERS (diff-header-lines DIFF))

(deftest partial-hunk-patch
  (is (= 4 (length PB-HEADERS)))
  (testing "forward patch"
    (is (= (string-append
            "diff --git a/foo b/foo\n"
            "index 000..fb1 100644\n"
            "--- a/foo\n"
            "+++ b/foo\n"
            "@@ -1,3 +1,3 @@\n"
            " ctx\n"
            "-gone\n"
            "+added\n"
            " tail\n")
         (build-apply-patch PB-HEADERS HS sel? 'forward))))
  (testing "reverse patch turns the unselected + into context"
    (is (= (string-append
            "diff --git a/foo b/foo\n"
            "index 000..fb1 100644\n"
            "--- a/foo\n"
            "+++ b/foo\n"
            "@@ -1,4 +1,4 @@\n"
            " ctx\n"
            "-gone\n"
            "+added\n"
            " more\n"
            " tail\n")
         (build-apply-patch PB-HEADERS HS sel? 'reverse))))
  (testing "default mode is forward"
    (is (= (build-apply-patch PB-HEADERS HS sel? 'forward)
         (build-apply-patch PB-HEADERS HS sel?))))
  (testing "no selection yields #f"
    (is (= #f (build-apply-patch PB-HEADERS HS (lambda (hi bi) #f))))))

(run-tests!)
