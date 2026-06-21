;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; blame-view.scm - the interactive blame component
;;;
;;; Blame as a chase-revisions loop: every line carries its commit, so Enter
;;; shows that commit, `l` reblames at its parent, and `h` walks back. The same
;;; shell as text-view (read-only, owns its keys, never touches a buffer) and
;;; the same decoupling as the rebase editor: it takes closures (`query`,
;;; `show-commit`) that close over the backend and depends on nothing
;;; juju-specific beyond the pure blame-row projection.

(require-builtin helix/components)
(require "helix/misc.scm")
(require "model.scm")
(require "view-rows.scm")
(require "render.scm")
(require "scroll.scm")
(require "keys.scm")
(require "string-utils.scm")

(provide open-blame-view)

(define COMPONENT-NAME "juju-blame-view")

;; lines: blame-line list at the current (rev, before) position. rev #f is the
;; working copy; before #t means "the parent of rev" (the suffix syntax stays in
;; the backend). stack: saved (list rev before cursor) frames for going back.
;; query: (file rev before) -> blame-line list. show-commit: (commit-id) -> void.
(struct bv-state (file rev before lines cursor top stack message message-tag query show-commit)
  #:mutable
  #:transparent)

;;; Rendering ;;;

(define (render-bv state-box rect buffer)
  (let* ([state (unbox state-box)]
         [content (draw-frame buffer (overlay-area rect) (bv-title state))]
         [rows (blame-rows (bv-state-lines state))]
         [height (visible-row-count content)]
         [top (clamp-top (bv-state-top state) (bv-state-cursor state) height (length rows))])
    (set-bv-state-top! state top)
    (draw-rows buffer content rows (bv-state-cursor state) top)
    (draw-status-line buffer content (status-text state) (bv-state-message-tag state))))

(define LEGEND "j/k move  Enter show  l chase  h back  q quit")

(define (status-text state)
  (let ([m (bv-state-message state)])
    (if (string=? m "") LEGEND m)))

(define (bv-title state)
  (string-append " juju blame  " (bv-state-file state) "  @ " (rev-label state) " "))

(define (rev-label state)
  (let ([rev (bv-state-rev state)])
    (cond
      [(not rev) "working copy"]
      [(bv-state-before state) (string-append "parent of " (string-take rev 8))]
      [else (string-take rev 8)])))

;;; State edits ;;;

(define (set-message! state text tag)
  (set-bv-state-message! state text)
  (set-bv-state-message-tag! state tag))

(define (clear-message! state) (set-message! state "" 'info))

(define (line-count state) (length (bv-state-lines state)))

(define (move-cursor! state delta)
  (let ([n (line-count state)] [c (+ (bv-state-cursor state) delta)])
    (when (> n 0)
      (set-bv-state-cursor! state (max 0 (min c (- n 1)))))))

(define (cursor-to! state idx)
  (let ([n (line-count state)])
    (when (> n 0)
      (set-bv-state-cursor! state (max 0 (min idx (- n 1)))))))

(define (cursor-line state)
  (list-ref (bv-state-lines state) (bv-state-cursor state)))

;; Move to a new (rev, before) position with fresh lines and cursor.
(define (set-position! state rev before lines cursor)
  (set-bv-state-rev! state rev)
  (set-bv-state-before! state before)
  (set-bv-state-lines! state lines)
  (set-bv-state-top! state 0)
  (set-bv-state-cursor! state 0)
  (cursor-to! state cursor))

;;; Chase / back / show ;;;

;; Reblame at the parent of the cursor line's commit, saving the current
;; position for `h`. An empty result (a root commit, or the path absent at the
;; parent) leaves the view where it is.
(define (chase! state)
  (clear-message! state)
  (let* ([commit (blame-line-commit (cursor-line state))]
         [lines ((bv-state-query state) (bv-state-file state) commit #t)])
    (if (null? lines)
      (set-message! state "juju: no blame before this line's commit (root commit?)" 'error)
      (begin
        (set-bv-state-stack! state
          (cons (list (bv-state-rev state) (bv-state-before state) (bv-state-cursor state))
            (bv-state-stack state)))
        (set-position! state commit #t lines 0)))))

;; Pop a saved frame and reblame there, restoring its cursor.
(define (go-back! state)
  (clear-message! state)
  (let ([stack (bv-state-stack state)])
    (if (null? stack)
      (set-message! state "juju: already at the newest blame" 'info)
      (let* ([frame (car stack)]
             [rev (list-ref frame 0)]
             [before (list-ref frame 1)]
             [cursor (list-ref frame 2)]
             [lines ((bv-state-query state) (bv-state-file state) rev before)])
        (if (null? lines)
          (set-message! state "juju: blame failed" 'error)
          (begin
            (set-bv-state-stack! state (cdr stack))
            (set-position! state rev before lines cursor)))))))

(define (show-at-cursor! state)
  (clear-message! state)
  ((bv-state-show-commit state) (blame-line-commit (cursor-line state))))

;;; Event handling ;;;

(define (handle-bv state-box event)
  (let ([state (unbox state-box)])
    (cond
      [(key-event-escape? event) (close-bv) event-result/close]
      [(char-is? event #\q) (close-bv) event-result/close]

      [(or (key-event-up? event) (char-is? event #\k)) (move-cursor! state -1) event-result/consume]
      [(or (key-event-down? event) (char-is? event #\j)) (move-cursor! state 1) event-result/consume]
      [(ctrl-char? event #\u) (move-cursor! state -10) event-result/consume]
      [(ctrl-char? event #\d) (move-cursor! state 10) event-result/consume]
      [(key-event-home? event) (cursor-to! state 0) event-result/consume]
      [(key-event-end? event) (cursor-to! state (line-count state)) event-result/consume]

      [(key-event-enter? event) (show-at-cursor! state) event-result/consume]
      [(char-is? event #\l) (chase! state) event-result/consume]
      [(or (char-is? event #\h) (key-event-backspace? event)) (go-back! state) event-result/consume]

      [else event-result/consume])))

(define (close-bv) (pop-last-component-by-name! COMPONENT-NAME))

;;@doc
;; Open the interactive blame view for `file`, starting from the working-copy
;; blame `lines`. `query` re-runs blame ((file rev before) -> blame-line list)
;; for the chase/back keys; `show-commit` shows a commit id in an overlay.
;; Echoes instead when `lines` is empty.
(define (open-blame-view file lines query show-commit)
  (if (null? lines)
    (set-status! (string-append "juju: nothing to blame (" file ")"))
    (let* ([state-box (box (bv-state file #f #f lines 0 0 '() "" 'info query show-commit))]
           [handlers (hash "handle_event" handle-bv
                      "cursor"
                      (lambda (state-box rect) #f)
                      "required_size"
                      (lambda (state-box size) size))]
           [component (new-component! COMPONENT-NAME state-box render-bv handlers)])
      (push-component! component))))
