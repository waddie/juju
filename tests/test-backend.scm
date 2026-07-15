;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; test-backend.scm - backend selection, interface wording, capabilities
;;;
;;; Selection never branches on a backend name; feature availability is read
;;; from the capabilities set.
;;;
;;; Run in file mode from the repo root: steel tests/test-backend.scm
;;; Requires the steel-test package in ~/.steel/cogs.

(require "steel-test/test.scm")
(require "../cogs/juju/backend-interface.scm")
(require "../cogs/juju/backend-detect.scm")
(require "../cogs/juju/backend-git.scm")
(require "../cogs/juju/backend-jj.scm")

(deftest backend-selection
  (testing "a valid override wins"
    (is (= 'git (choose-backend-name '(git jj) 'git 'jj))))
  (testing "an invalid override is ignored"
    (is (= 'git (choose-backend-name '(git) 'jj 'jj))))
  (testing "colocated default"
    (is (= 'jj (choose-backend-name '(jj git) #f 'jj)))
    (is (= 'git (choose-backend-name '(jj git) #f 'git))))
  (testing "sole backend present"
    (is (= 'jj (choose-backend-name '(jj) #f 'jj)))
    (is (= 'git (choose-backend-name '(git) #f 'jj))))
  (testing "none present"
    (is (= #f (choose-backend-name '() #f 'jj)))))

;; Reversibility of a discard is capability-gated on 'oplog (jj's first-class
;; operation log), never on the backend name.
(define BI-OPLOG (make-backend 'jj "/" '(oplog discard) #f #f #f #f))
(define BI-PLAIN (make-backend 'git "/" '(discard) #f #f #f #f))

(deftest interface-wording
  (testing "with an operation log"
    (is (= "(undo reverses it)" (discard-confirm-note BI-OPLOG))))
  (testing "without one"
    (is (= "This cannot be undone" (discard-confirm-note BI-PLAIN)))))

(define JJ-B (make-jj-backend "/tmp/x"))
(define GIT-B (make-git-backend "/tmp/x"))

(deftest capabilities
  (testing "jj"
    (is (= #t (backend-supports? JJ-B 'edit)))
    (is (= #t (backend-supports? JJ-B 'switch)))
    (is (= #f (backend-supports? JJ-B 'branch-force-delete)))
    (is (= #t (backend-supports? JJ-B 'rebase-skip-emptied))))
  (testing "git"
    (is (= #f (backend-supports? GIT-B 'edit)))
    (is (= #t (backend-supports? GIT-B 'switch)))
    (is (= #t (backend-supports? GIT-B 'push-set-upstream)))
    (is (= #t (backend-supports? GIT-B 'branch-force-delete)))
    (is (= #f (backend-supports? GIT-B 'rebase-skip-emptied)))))

(run-tests!)
