;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; text-view.scm - a small read-only scrollable text overlay
;;;
;;; A general component for showing captured VCS output that does not warrant
;;; the structured status view: log listings, blame, a commit's show output. It
;;; reuses the status view's drawing helpers and styles diff lines so `show`
;;; reads well. Read-only: it owns its keys and never touches a buffer.

(require-builtin helix/components)
(require "helix/misc.scm")
(require "render.scm")
(require "scroll.scm")
(require "keys.scm")
(require "string-utils.scm")

(provide show-text-view)

(define COMPONENT-NAME "juju-text-view")

(struct tv-state (title rows cursor top) #:mutable #:transparent)

;; Tag a raw line by its leading diff character so log/show output is coloured.
(define (line->row line)
  (let ([tag (cond
              [(string-prefix? "@@" line) 'diff-header]
              [(string-prefix? "+" line) 'diff-add]
              [(string-prefix? "-" line) 'diff-del]
              [(string-prefix? "commit " line) 'commit]
              [(string-prefix? "diff --git" line) 'diff-meta]
              [else 'file])])
    (hash 'text line 'tag tag)))

(define (render-tv state-box rect buffer)
  (let* ([state (unbox state-box)]
         [content (draw-frame buffer (overlay-area rect) (tv-state-title state))]
         [rows (tv-state-rows state)]
         [height (visible-row-count content)]
         [top (clamp-top (tv-state-top state) (tv-state-cursor state) height (length rows))])
    (set-tv-state-top! state top)
    (draw-rows buffer content rows (tv-state-cursor state) top)
    (draw-status-line buffer content "j/k move  q quit" 'info)))

(define (move! state delta)
  (let* ([rows (tv-state-rows state)]
         [n (length rows)]
         [c (+ (tv-state-cursor state) delta)])
    (when (> n 0)
      (set-tv-state-cursor! state (max 0 (min c (- n 1)))))))

(define (handle-tv state-box event)
  (let ([state (unbox state-box)])
    (cond
      [(key-event-escape? event) (close-tv) event-result/close]
      [(char-is? event #\q) (close-tv) event-result/close]
      [(or (key-event-up? event) (char-is? event #\k)) (move! state -1) event-result/consume]
      [(or (key-event-down? event) (char-is? event #\j)) (move! state 1) event-result/consume]
      [(ctrl-char? event #\u) (move! state -10) event-result/consume]
      [(ctrl-char? event #\d) (move! state 10) event-result/consume]
      [(key-event-home? event) (set-tv-state-cursor! state 0) event-result/consume]
      [(key-event-end? event)
        (set-tv-state-cursor! state (max 0 (- (length (tv-state-rows state)) 1)))
        event-result/consume]
      [else event-result/consume])))

(define (close-tv) (pop-last-component-by-name! COMPONENT-NAME))

;;@doc
;; Open a scrollable overlay titled `title` showing `lines` (a list of strings).
;; Echoes instead when `lines` is empty.
(define (show-text-view title lines)
  (if (null? lines)
    (set-status! (string-append "juju: nothing to show (" title ")"))
    (let* ([rows (map line->row lines)]
           [state-box (box (tv-state title rows 0 0))]
           [handlers (hash "handle_event" handle-tv
                      "cursor"
                      (lambda (state-box rect) #f)
                      "required_size"
                      (lambda (state-box size) size))]
           [component (new-component! COMPONENT-NAME state-box render-tv handlers)])
      (push-component! component))))
