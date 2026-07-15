;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; test-operand.scm - commit-row selection to revs (resolve-revs)
;;;
;;; Run in file mode from the repo root: steel tests/test-operand.scm
;;; Requires the steel-test package in ~/.steel/cogs.

(require "steel-test/test.scm")
(require "../cogs/juju/model.scm")
(require "../cogs/juju/view-rows.scm")
(require "../cogs/juju/operand.scm")

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

(deftest commit-rev-resolution
  (testing "single commit row"
    (is (= 1 (length RV1)))
    (is (= "abc123" (hash-ref (car RV1) 'rev)))
    (is (= 'recent (hash-ref (car RV1) 'kind))))
  (testing "a section row resolves to all its commits"
    (is (= 2 (length (resolve-revs RV-ROWS '(2))))))
  (testing "section plus one of its commits dedupes"
    (is (= 2 (length (resolve-revs RV-ROWS '(2 3))))))
  (testing "a non-commit row resolves to nothing"
    (is (= '() (resolve-revs RV-ROWS '(0))))))

(run-tests!)
