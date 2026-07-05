;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; rebase-view.scm - the interactive rebase editor component
;;;
;;; A read-only-style overlay (the shared overlay-view shell, ui-utils.hx) for
;;; editing a rebase plan: a list of commits, each assignable an action and
;;; reorderable, in Helix's selection-first style. It owns its keys and never
;;; touches an editor buffer. It is backend-agnostic: like the transient menu,
;;; it takes an `on-apply` thunk that closes over the backend and refresh, so it
;;; depends on nothing juju-specific beyond the pure plan model. The plan model,
;;; validation, and the action->row projection live in rebase-todo.scm.

(require-builtin helix/components)
(require "helix/misc.scm")
(require "model.scm")
(require "rebase-todo.scm")
(require "render.scm") ; juju-tag->style
(require "ui-utils.hx/keys.scm")
(require "ui-utils.hx/strings.scm")
(require "ui-utils.hx/overlay-view.scm")

(provide open-rebase-view)

(define COMPONENT-NAME "juju-rebase-view")

;; entries: todo-entry list (oldest first). on-apply: (entries -> void), run on
;; confirm after the view closes. selection: marked row indices (selection-first
;; bulk action assignment); reordering clears it since indices then shift.
(struct rv-state (entries cursor top selection message message-tag on-apply)
  #:mutable
  #:transparent)

(define LEGEND
  "j/k move  J/K reorder  p r e s f d set  v mark  Enter apply  q quit")

(define (rv-status state)
  (let ([m (rv-state-message state)])
    (cons (if (string=? m "") LEGEND m) (rv-state-message-tag state))))

;;; State edits ;;;

(define (set-message! state text tag)
  (set-rv-state-message! state text)
  (set-rv-state-message-tag! state tag))

(define (clear-message! state) (set-message! state "" 'info))

(define (entry-count state) (length (rv-state-entries state)))

(define (cursor-to! state idx)
  (let ([n (entry-count state)])
    (when (> n 0)
      (set-rv-state-cursor! state (max 0 (min idx (- n 1)))))))

;; The rows an action applies to: the marked set, or the cursor when none.
(define (action-indices state)
  (let ([sel (rv-state-selection state)])
    (if (null? sel) (list (rv-state-cursor state)) sel)))

(define (toggle-mark! state)
  (let ([c (rv-state-cursor state)] [sel (rv-state-selection state)])
    (set-rv-state-selection! state
      (if (member c sel)
        (filter (lambda (i) (not (= i c))) sel)
        (cons c sel)))))

(define (set-action! state action)
  (clear-message! state)
  (set-rv-state-entries! state
    (foldl (lambda (i es) (todo-set-action es i action))
      (rv-state-entries state)
      (action-indices state))))

;; Reorder the entry under the cursor, following it, and drop marks (their
;; indices no longer line up with the moved list).
(define (move-entry! state dir)
  (clear-message! state)
  (let* ([c (rv-state-cursor state)]
         [entries (rv-state-entries state)]
         [moved (if (eq? dir 'up) (todo-move-up entries c) (todo-move-down entries c))])
    (set-rv-state-entries! state moved)
    (set-rv-state-selection! state '())
    (cursor-to! state (if (eq? dir 'up) (- c 1) (+ c 1)))))

(define (close-rebase-view) (pop-last-component-by-name! COMPONENT-NAME))

(define (apply-rebase! state)
  (let* ([entries (rv-state-entries state)]
         [invalid (todo-validate entries)])
    (if invalid
      (set-message! state invalid 'error)
      (let ([on-apply (rv-state-on-apply state)])
        (close-rebase-view)
        (collect-rewords entries (todo-reword-commits entries) on-apply)))))

;; Prompt for each reword message in turn, then apply. Chained because each
;; prompt is its own modal: the callback for one pushes the next. An empty answer
;; keeps the commit's existing message (it stays an effective pick at apply).
;; Both branches end in `#t`, a real value: with every reachable tail branch
;; yielding void, Steel's TCO miscompiles the prompt callback and the resulting
;; BadSyntax error tears the editor down (same quirk as status-view move-cursor!).
(define (collect-rewords entries rewords on-apply)
  (if (null? rewords)
    (begin
      (on-apply entries)
      #t)
    (let* ([idx (car (car rewords))]
           [commit (cdr (car rewords))])
      (push-component!
        (prompt
          (string-append "Reword " (commit-record-short-id commit)
            " ["
            (commit-record-subject commit)
            "]: ")
          (lambda (input)
            (let ([next (if (blank? input) entries (todo-set-message entries idx input))])
              (collect-rewords next (cdr rewords) on-apply)))))
      #t)))

;;; Action keys (the shell handles movement and close) ;;;

(define (rv-keys state-box event)
  (let ([state (unbox state-box)])
    (cond
      [(char-is? event #\K) (move-entry! state 'up) event-result/consume]
      [(char-is? event #\J) (move-entry! state 'down) event-result/consume]
      [(char-is? event #\p) (set-action! state 'pick) event-result/consume]
      [(char-is? event #\r) (set-action! state 'reword) event-result/consume]
      [(char-is? event #\e) (set-action! state 'edit) event-result/consume]
      [(char-is? event #\s) (set-action! state 'squash) event-result/consume]
      [(char-is? event #\f) (set-action! state 'fixup) event-result/consume]
      [(char-is? event #\d) (set-action! state 'drop) event-result/consume]
      [(char-is? event #\v) (toggle-mark! state) event-result/consume]
      [(key-event-enter? event) (apply-rebase! state) event-result/consume]
      [else #f])))

(define rebase-view-spec
  (make-overlay-view
    #:name
    COMPONENT-NAME
    #:title
    (lambda (state) "Interactive rebase")
    #:rows
    (lambda (state)
      (todo-rows (rv-state-entries state)
        (rv-state-cursor state)
        (rv-state-selection state)))
    #:cursor
    rv-state-cursor
    #:set-cursor!
    set-rv-state-cursor!
    #:top
    rv-state-top
    #:set-top!
    set-rv-state-top!
    #:status
    rv-status
    #:marked
    rv-state-selection
    #:on-key
    rv-keys
    #:page-size
    8
    #:tag->style
    juju-tag->style
    #:overlay-scale
    (lambda () (juju-overlay-scale))))

;;@doc
;; Open the interactive rebase editor over `entries` (a todo-entry list, oldest
;; first). On confirm the view closes and `on-apply` is called with the edited
;; entries; on cancel nothing runs. Echoes instead when there is nothing to edit.
(define (open-rebase-view entries on-apply)
  (if (null? entries)
    (set-status! "juju: no commits to rebase in range")
    (open-overlay-view! rebase-view-spec
      (rv-state entries 0 0 '() "" 'info on-apply))))
