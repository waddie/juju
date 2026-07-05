;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; rebase-todo.scm - the pure interactive-rebase plan model
;;;
;;; The interactive rebase editor builds a backend-neutral *plan*: an ordered
;;; list of todo-entries, each a commit paired with an action (pick / reword /
;;; edit / squash / fixup / drop) and, for reword, a new message. This module is
;;; that model plus its projections; it knows nothing of components, processes,
;;; or which backend will apply it. Pure: data in, data out, so it carries the
;;; editor's unit tests.
;;;
;;; Order is git's todo order throughout: oldest first, top of the list applied
;;; first. `make-todo` reverses the newest-first list `backend-log` returns so
;;; that everything downstream - the editor display, `todo->git-lines`, and the
;;; jj step translation - works in one consistent direction. squash/fixup folds
;;; into the nearest *preceding* (older, higher) kept entry, exactly as git reads
;;; its todo.

(require "model.scm")
(require "ui-utils.hx/strings.scm")

(provide todo-entry
  todo-entry?
  todo-entry-commit
  todo-entry-action
  todo-entry-message
  make-todo
  todo-actions
  todo-set-action
  todo-set-message
  todo-move-up
  todo-move-down
  todo-validate
  todo->git-lines
  todo->jj-steps
  todo-reword-commits
  todo-reword-messages
  todo-rows)

;; A planned operation on one commit. `action` is one of `todo-actions`;
;; `message` is the replacement commit message for a reword entry (#f otherwise).
(struct todo-entry (commit action message) #:transparent)

;; The actions the editor can assign, in the order the legend lists them.
(define todo-actions '(pick reword edit squash fixup drop))

;;@doc
;; Build a todo plan from `commits` (a commit-record list, newest first as
;; `backend-log` yields it). Returns entries oldest-first, every action `pick`.
(define (make-todo commits)
  (map (lambda (c) (todo-entry c 'pick #f)) (reverse commits)))

;;; Pure edits (each returns a new entry list) ;;;

;;@doc Replace the action of the entry at `idx`, keeping its message.
(define (todo-set-action entries idx action)
  (if (index-ok? entries idx)
    (let ([e (list-ref entries idx)])
      (list-set entries idx (todo-entry (todo-entry-commit e) action (todo-entry-message e))))
    entries))

;;@doc Set the reword `message` of the entry at `idx`, leaving its action.
(define (todo-set-message entries idx message)
  (if (index-ok? entries idx)
    (let ([e (list-ref entries idx)])
      (list-set entries idx (todo-entry (todo-entry-commit e) (todo-entry-action e) message)))
    entries))

;;@doc Swap the entry at `idx` with the one above it (no-op at the top).
(define (todo-move-up entries idx) (list-swap entries idx (- idx 1)))

;;@doc Swap the entry at `idx` with the one below it (no-op at the bottom).
(define (todo-move-down entries idx) (list-swap entries idx (+ idx 1)))

;;; Validation ;;;

;;@doc
;; #f when `entries` is a valid rebase plan, else a one-line reason. Encodes
;; git's two structural rules: at least one commit must survive, and the first
;; surviving commit cannot be squash/fixup (nothing precedes it to fold into).
(define (todo-validate entries)
  (let ([kept (filter (lambda (e) (not (eq? (todo-entry-action e) 'drop))) entries)])
    (cond
      [(null? kept) "rebase would drop every commit"]
      [(memv (todo-entry-action (car kept)) '(squash fixup))
        "first commit cannot be squash or fixup"]
      [else #f])))

;;; Git projection ;;;

;;@doc
;; Project `entries` to git rebase-todo lines ("<action> <full-id> <subject>"),
;; in order. git ignores everything after the id; the subject is a human aid.
(define (todo->git-lines entries) (map todo-entry->git-line entries))

;; A reword with no collected message keeps the commit as-is (emit `pick`), so
;; the todo's `reword` lines stay 1:1 with the messages fed to GIT_EDITOR.
(define (todo-entry->git-line e)
  (let* ([c (todo-entry-commit e)]
         [a (todo-entry-action e)]
         [verb (if (and (eq? a 'reword) (not (todo-entry-message e)))
                "pick"
                (symbol->string a))])
    (string-append verb " " (commit-record-id c) " " (commit-record-subject c))))

;;@doc
;; The reword messages in plan order (only entries actually marked reword with a
;; message). Aligns 1:1 with the `reword` lines `todo->git-lines` emits, so the
;; git reword editor can feed them in sequence.
(define (todo-reword-messages entries)
  (filter-map-indexed entries
    (lambda (e i)
      (and (eq? (todo-entry-action e) 'reword) (todo-entry-message e)))))

;;; jj projection ;;;
;;;
;;; jj has no todo file, so a plan becomes an ordered sequence of jj commands
;;; keyed on stable change-ids. The graph is collapsed first (folds, then
;;; drops), then surviving commits are reworded and linearised into the planned
;;; order, then the working copy is parked on the edit target. Each step is the
;;; argument list passed to `run-vcs` after the leading root args.

;;@doc Ordered list of jj argument-lists realising `entries`.
(define (todo->jj-steps entries)
  (append
    (jj-fold-steps entries)
    (jj-drop-steps entries)
    (jj-reword-steps entries)
    (jj-reorder-steps entries)
    (jj-edit-steps entries)))

(define (kept-action? a) (memv a '(pick reword edit)))

;; Change-id of the nearest entry before `idx` whose action keeps it, or #f.
(define (preceding-kept-id entries idx)
  (let loop ([i (- idx 1)])
    (cond
      [(< i 0) #f]
      [(kept-action? (todo-entry-action (list-ref entries i)))
        (commit-record-id (todo-entry-commit (list-ref entries i)))]
      [else (loop (- i 1))])))

(define (jj-fold-steps entries)
  (filter-map-indexed entries
    (lambda (e i)
      (let ([a (todo-entry-action e)])
        (if (memv a '(squash fixup))
          (let ([from (commit-record-id (todo-entry-commit e))]
                [into (preceding-kept-id entries i)])
            (and into (jj-fold-args from into a)))
          #f)))))

(define (jj-fold-args from into action)
  (append (list "squash" "--from" from "--into" into)
    (if (eq? action 'fixup) (list "--use-destination-message") '())))

(define (jj-drop-steps entries)
  (filter-map-indexed entries
    (lambda (e i)
      (if (eq? (todo-entry-action e) 'drop)
        (list "abandon" (commit-record-id (todo-entry-commit e)))
        #f))))

(define (jj-reword-steps entries)
  (filter-map-indexed entries
    (lambda (e i)
      (if (and (eq? (todo-entry-action e) 'reword) (todo-entry-message e))
        (list "describe" (commit-record-id (todo-entry-commit e))
          "-m"
          (todo-entry-message e))
        #f))))

;; Linearise the surviving commits into plan order: rebase each onto the one
;; before it. The change-ids are stable, so this holds even after the folds and
;; drops above rewrote the graph.
(define (jj-reorder-steps entries)
  (let ([kept-ids (map (lambda (e) (commit-record-id (todo-entry-commit e)))
                   (filter (lambda (e) (kept-action? (todo-entry-action e))) entries))])
    (if (< (length kept-ids) 2)
      '()
      (let loop ([prev (car kept-ids)] [rest (cdr kept-ids)] [acc '()])
        (if (null? rest)
          (reverse acc)
          (loop (car rest) (cdr rest)
            (cons (list "rebase" "-r" (car rest) "--insert-after" prev) acc)))))))

;; Park @ on the last commit marked edit, if any (later edits win, so the last
;; one is the working-copy position the user asked for).
(define (jj-edit-steps entries)
  (let ([edits (filter (lambda (e) (eq? (todo-entry-action e) 'edit)) entries)])
    (if (null? edits)
      '()
      (list (list "edit" (commit-record-id (todo-entry-commit (last edits))))))))

;;@doc
;; The commits the plan rewords, oldest first: (idx . commit-record) pairs, so
;; the editor can prompt for each new message and write it back by index.
(define (todo-reword-commits entries)
  (let loop ([es entries] [i 0] [acc '()])
    (cond
      [(null? es) (reverse acc)]
      [(eq? (todo-entry-action (car es)) 'reword)
        (loop (cdr es) (+ i 1) (cons (cons i (todo-entry-commit (car es))) acc))]
      [else (loop (cdr es) (+ i 1) acc)])))

;;; Display projection ;;;

;;@doc
;; Flatten `entries` to draw-rows hashes (each `'text` and `'tag`), oldest first.
;; `cursor` and `selection` are accepted for symmetry with the status view but do
;; not change the row text; draw-rows highlights them by index.
(define (todo-rows entries cursor selection)
  (map todo-entry->row entries))

(define (todo-entry->row e)
  (let ([c (todo-entry-commit e)]
        [a (todo-entry-action e)])
    (hash 'text (todo-row-text a c (todo-entry-message e))
      'tag
      (action-tag a))))

(define (todo-row-text action commit message)
  (string-append
    (pad-right (symbol->string action) 7)
    (pad-right (commit-record-short-id commit) 12)
    " "
    (if (and (eq? action 'reword) message) message (commit-record-subject commit))))

;; Colour each action distinctly using the existing render tags.
(define (action-tag action)
  (cond
    [(eq? action 'pick) 'commit]
    [(eq? action 'reword) 'diff-add]
    [(eq? action 'edit) 'diff-header]
    [(eq? action 'squash) 'section]
    [(eq? action 'fixup) 'section]
    [(eq? action 'drop) 'info]
    [else 'file]))

;;; List helpers ;;;

(define (index-ok? lst idx) (and (>= idx 0) (< idx (length lst))))

(define (list-set lst idx val)
  (let loop ([l lst] [i 0] [acc '()])
    (cond
      [(null? l) (reverse acc)]
      [(= i idx) (loop (cdr l) (+ i 1) (cons val acc))]
      [else (loop (cdr l) (+ i 1) (cons (car l) acc))])))

(define (list-swap lst i j)
  (if (and (index-ok? lst i) (index-ok? lst j))
    (let ([vi (list-ref lst i)] [vj (list-ref lst j)])
      (list-set (list-set lst i vj) j vi))
    lst))

;; map `f` over (element index) pairs, dropping #f results.
(define (filter-map-indexed lst f)
  (let loop ([l lst] [i 0] [acc '()])
    (if (null? l)
      (reverse acc)
      (let ([r (f (car l) i)])
        (loop (cdr l) (+ i 1) (if r (cons r acc) acc))))))
