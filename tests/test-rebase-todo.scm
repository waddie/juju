;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; test-rebase-todo.scm - rebase todo model, validation, and backend plans
;;;
;;; The todo list is built oldest-first, edited by index, then lowered to
;;; either a git rebase todo (oldest-first "<action> <id> <subject>") or a
;;; sequence of jj steps.
;;;
;;; Run in file mode from the repo root: steel tests/test-rebase-todo.scm
;;; Requires the steel-test package in ~/.steel/cogs.

(require "steel-test/test.scm")
(require "../cogs/juju/model.scm")
(require "../cogs/juju/rebase-todo.scm")

;; backend-log yields newest-first; make-todo reverses to oldest-first.
(define RC-A (make-commit-record "aaaa1111" "aaaa" "ann" "1d" "add a" '()))
(define RC-B (make-commit-record "bbbb2222" "bbbb" "ben" "1d" "add b" '()))
(define RC-C (make-commit-record "cccc3333" "cccc" "cat" "1d" "add c" '()))
(define TODO (make-todo (list RC-C RC-B RC-A))) ; newest C first

(deftest todo-construction
  (is (= 3 (length TODO)))
  (testing "reversed to oldest-first"
    (is (= "aaaa1111" (commit-record-id (todo-entry-commit (car TODO))))))
  (testing "default action is pick"
    (is (= 'pick (todo-entry-action (car TODO))))))

(deftest todo-editing
  (let ([set (todo-set-action TODO 1 'squash)])
    (is (= 'squash (todo-entry-action (list-ref set 1))))
    (testing "leaves other entries alone"
      (is (= 'pick (todo-entry-action (list-ref set 0))))))
  (testing "move-up swaps with the predecessor"
    (is (= "cccc3333"
         (commit-record-id (todo-entry-commit (list-ref (todo-move-up TODO 2) 1))))))
  (testing "move at the boundary is a no-op"
    (is (= TODO (todo-move-up TODO 0)))
    (is (= TODO (todo-move-down TODO 2)))))

(deftest todo-validation
  (testing "a plain pick list validates"
    (is (= #f (todo-validate TODO))))
  (testing "dropping every commit is rejected"
    (is (= "rebase would drop every commit"
         (todo-validate
           (todo-set-action (todo-set-action (todo-set-action TODO 0 'drop) 1 'drop) 2 'drop)))))
  (testing "a leading squash is rejected"
    (is (= "first commit cannot be squash or fixup"
         (todo-validate (todo-set-action TODO 0 'squash)))))
  (testing "a squash with no surviving predecessor is rejected even after a drop"
    (is (= "first commit cannot be squash or fixup"
         (todo-validate (todo-set-action (todo-set-action TODO 0 'drop) 1 'squash)))))
  (testing "pick, drop, squash is valid (the squash folds into the leading pick)"
    (is (= #f (todo-validate (todo-set-action (todo-set-action TODO 1 'drop) 2 'squash))))))

;; git lines: oldest-first, "<action> <full-id> <subject>".
(define GL (todo->git-lines (todo-set-action (todo-set-action TODO 1 'squash) 2 'drop)))

(deftest todo-git-lines
  (is (= "pick aaaa1111 add a" (list-ref GL 0)))
  (is (= "squash bbbb2222 add b" (list-ref GL 1)))
  (is (= "drop cccc3333 add c" (list-ref GL 2)))
  (testing "a reword without a message falls back to pick (keeps alignment)"
    (is (= "pick bbbb2222 add b"
         (list-ref (todo->git-lines (todo-set-action TODO 1 'reword)) 1))))
  (testing "a reword with a message"
    (is (= "reword bbbb2222 add b"
         (list-ref
           (todo->git-lines (todo-set-message (todo-set-action TODO 1 'reword) 1 "x"))
           1)))))

(deftest todo-jj-steps
  (testing "squash uses --from/--into"
    (is (= (list "squash" "--from" "bbbb2222" "--into" "aaaa1111")
         (car (todo->jj-steps (todo-set-action TODO 1 'squash))))))
  (testing "fixup keeps the destination message"
    (is (= (list "squash" "--from" "bbbb2222" "--into" "aaaa1111" "--use-destination-message")
         (car (todo->jj-steps (todo-set-action TODO 1 'fixup))))))
  (testing "drop abandons"
    (is (= (list "abandon" "bbbb2222")
         (car (todo->jj-steps (todo-set-action TODO 1 'drop))))))
  (testing "a plain reorder rebases each onto its new predecessor"
    (let ([steps (todo->jj-steps TODO)])
      (is (= 2 (length steps)))
      (is (= (list "rebase" "-r" "bbbb2222" "--insert-after" "aaaa1111") (list-ref steps 0)))
      (is (= (list "rebase" "-r" "cccc3333" "--insert-after" "bbbb2222") (list-ref steps 1)))))
  (testing "reword describes"
    (is (= (list "describe" "bbbb2222" "-m" "new msg")
         (car (todo->jj-steps
               (todo-set-message (todo-set-action TODO 1 'reword) 1 "new msg"))))))
  (testing "edit parks @ last"
    (is (= (list "edit" "cccc3333")
         (last (todo->jj-steps (todo-set-action TODO 2 'edit)))))))

(deftest todo-rewords
  (testing "collected in plan order"
    (is (= (list "first" "third")
         (todo-reword-messages
           (todo-set-message (todo-set-action
                              (todo-set-message (todo-set-action TODO 0 'reword) 0 "first")
                              2
                              'reword)
             2
             "third")))))
  (testing "a message-less reword is skipped"
    (is (= '() (todo-reword-messages (todo-set-action TODO 1 'reword)))))
  (testing "reword commits collected by index"
    (let ([rw (todo-reword-commits (todo-set-action (todo-set-action TODO 0 'reword) 2 'reword))])
      (is (= 2 (length rw)))
      (is (= 0 (car (car rw))))
      (is (= 2 (car (cadr rw)))))))

;; display rows.
(define TR (todo-rows (todo-set-action TODO 1 'drop) 0 '()))

(deftest todo-display-rows
  (is (= 3 (length TR)))
  (is (= "pick   aaaa         add a" (hash-ref (list-ref TR 0) 'text)))
  (testing "a dropped entry is dimmed"
    (is (= 'info (hash-ref (list-ref TR 1) 'tag))))
  (testing "a pick is tagged as a commit"
    (is (= 'commit (hash-ref (list-ref TR 0) 'tag)))))

(run-tests!)
