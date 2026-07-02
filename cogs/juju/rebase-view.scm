;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; rebase-view.scm - the interactive rebase editor component
;;;
;;; A read-only-style overlay (the same shell as text-view / status-view) for
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
(require "render.scm")
(require "scroll.scm")
(require "keys.scm")
(require "string-utils.scm")

(provide open-rebase-view)

(define COMPONENT-NAME "juju-rebase-view")

;; entries: todo-entry list (oldest first). on-apply: (entries -> void), run on
;; confirm after the view closes. selection: marked row indices (selection-first
;; bulk action assignment); reordering clears it since indices then shift.
(struct rv-state (entries cursor top selection message message-tag on-apply)
  #:mutable
  #:transparent)

;;; Rendering ;;;

(define (render-rv state-box rect buffer)
  (let* ([state (unbox state-box)]
         [content (draw-frame buffer (overlay-area rect) "Interactive rebase")]
         [rows (todo-rows (rv-state-entries state)
                (rv-state-cursor state)
                (rv-state-selection state))]
         [height (visible-row-count content)]
         [top (clamp-top (rv-state-top state) (rv-state-cursor state) height (length rows))])
    (set-rv-state-top! state top)
    (draw-rows buffer content rows (rv-state-cursor state) top (rv-state-selection state))
    (draw-status-line buffer content (status-text state) (rv-state-message-tag state))))

(define LEGEND
  "j/k move  J/K reorder  p r e s f d set  v mark  Enter apply  q quit")

(define (status-text state)
  (let ([m (rv-state-message state)])
    (if (string=? m "") LEGEND m)))

;;; State edits ;;;

(define (set-message! state text tag)
  (set-rv-state-message! state text)
  (set-rv-state-message-tag! state tag))

(define (clear-message! state) (set-message! state "" 'info))

(define (entry-count state) (length (rv-state-entries state)))

(define (move-cursor! state delta)
  (let ([n (entry-count state)] [c (+ (rv-state-cursor state) delta)])
    (when (> n 0)
      (set-rv-state-cursor! state (max 0 (min c (- n 1)))))))

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

(define (apply-rebase! state-box)
  (let* ([state (unbox state-box)]
         [entries (rv-state-entries state)]
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

;;; Event handling ;;;

(define (handle-rv state-box event)
  (let ([state (unbox state-box)])
    (cond
      [(key-event-escape? event) (close-rebase-view) event-result/close]
      [(char-is? event #\q) (close-rebase-view) event-result/close]

      [(or (key-event-up? event) (char-is? event #\k)) (move-cursor! state -1) event-result/consume]
      [(or (key-event-down? event) (char-is? event #\j)) (move-cursor! state 1) event-result/consume]
      [(ctrl-char? event #\u) (move-cursor! state -8) event-result/consume]
      [(ctrl-char? event #\d) (move-cursor! state 8) event-result/consume]
      [(key-event-home? event) (cursor-to! state 0) event-result/consume]
      [(key-event-end? event) (cursor-to! state (entry-count state)) event-result/consume]

      ;; Reorder.
      [(char-is? event #\K) (move-entry! state 'up) event-result/consume]
      [(char-is? event #\J) (move-entry! state 'down) event-result/consume]

      ;; Assign an action (selection-first).
      [(char-is? event #\p) (set-action! state 'pick) event-result/consume]
      [(char-is? event #\r) (set-action! state 'reword) event-result/consume]
      [(char-is? event #\e) (set-action! state 'edit) event-result/consume]
      [(char-is? event #\s) (set-action! state 'squash) event-result/consume]
      [(char-is? event #\f) (set-action! state 'fixup) event-result/consume]
      [(char-is? event #\d) (set-action! state 'drop) event-result/consume]

      [(char-is? event #\v) (toggle-mark! state) event-result/consume]

      [(key-event-enter? event) (apply-rebase! state-box) event-result/consume]

      [else event-result/consume])))

(define (close-rebase-view) (pop-last-component-by-name! COMPONENT-NAME))

;;@doc
;; Open the interactive rebase editor over `entries` (a todo-entry list, oldest
;; first). On confirm the view closes and `on-apply` is called with the edited
;; entries; on cancel nothing runs. Echoes instead when there is nothing to edit.
(define (open-rebase-view entries on-apply)
  (if (null? entries)
    (set-status! "juju: no commits to rebase in range")
    (let* ([state-box (box (rv-state entries 0 0 '() "" 'info on-apply))]
           [handlers (hash "handle_event" handle-rv
                      "cursor"
                      (lambda (state-box rect) #f)
                      "required_size"
                      (lambda (state-box size) size))]
           [component (new-component! COMPONENT-NAME state-box render-rv handlers)])
      (push-component! component))))
