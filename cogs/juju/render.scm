;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; render.scm - drawing helpers for the status view
;;;
;;; Pure presentation: it knows nothing of backends or the model. The view hands
;;; it a list of "rows" (each a hash with display `text` and a style `tag`) plus
;;; the cursor and scroll position, and these helpers paint them into the
;;; component buffer with a border, a title, and a status line. Keeping styling
;;; here means the view module stays about state and behaviour.

(require-builtin helix/components)
(require "string-utils.scm")
(require "config.scm")

(provide overlay-area
  draw-frame
  draw-rows
  draw-status-line
  visible-row-count)

;; The status/text views float as centred modal overlays rather than filling the
;; screen, so the buffer underneath stays visible (matching the nrepl.hx
;; pickers). The view renders into this sub-rect and leaves the rest of the
;; surface untouched, which shows the editor layer drawn beneath it. The scale is
;; user-tunable (config `juju-overlay-scale`); it is clamped here so a stray
;; value cannot produce a degenerate rect.
(define OVERLAY-BOTTOM-CLIP 2) ; leave the editor status line visible

(define (overlay-scale-percent)
  (max 10 (min 100 (juju-overlay-scale))))

;;@doc
;; Centre a modal overlay within `rect`: `juju-overlay-scale`% of the terminal
;; width/height, clipping `OVERLAY-BOTTOM-CLIP` rows from the bottom for the
;; editor status line, centred with equal margins.
(define (overlay-area rect)
  (let* ([tw (area-width rect)]
         [th (area-height rect)]
         [tx (area-x rect)]
         [ty (area-y rect)]
         [scale (overlay-scale-percent)]
         [clipped-h (max 0 (- th OVERLAY-BOTTOM-CLIP))]
         [w (quotient (* tw scale) 100)]
         [h (quotient (* clipped-h scale) 100)]
         [ox (quotient (- tw w) 2)]
         [oy (quotient (- clipped-h h) 2)])
    (area (+ tx ox) (+ ty oy) w h)))

;; Map a config colour symbol to a concrete helix Color, defaulting to no colour
;; for an unknown name (the caller's base style then applies unchanged).
(define (color-for-name sym)
  (cond
    [(eq? sym 'yellow) Color/Yellow]
    [(eq? sym 'green) Color/Green]
    [(eq? sym 'cyan) Color/Cyan]
    [(eq? sym 'blue) Color/LightBlue]
    [(eq? sym 'magenta) Color/Magenta]
    [(eq? sym 'red) Color/Red]
    [(eq? sym 'white) Color/White]
    [else #f]))

;; Map a row's style tag (and whether it is the cursor row) to a concrete style.
;; Unknown tags fall back to the default style. The cursor row is drawn reversed
;; so it reads clearly under any theme; a marked (but non-cursor) row is bolded.
(define (tag->style tag selected? . opt)
  (let ([marked? (and (pair? opt) (car opt))]
        [base (cond
               [(eq? tag 'title) (style-with-bold (style))]
               [(eq? tag 'header-label) (style-with-bold (style))]
               [(eq? tag 'section)
                 (let ([c (color-for-name (juju-section-color))])
                   (style-with-bold (if c (style-fg (style) c) (style))))]
               [(eq? tag 'file) (style)]
               [(eq? tag 'diff-add) (style-fg (style) Color/Green)]
               [(eq? tag 'diff-del) (style-fg (style) Color/Red)]
               [(eq? tag 'diff-header) (style-fg (style) Color/Cyan)]
               [(eq? tag 'diff-context) (style-with-dim (style))]
               [(eq? tag 'diff-meta) (style-with-dim (style))]
               [(eq? tag 'commit) (style-fg (style) Color/LightBlue)]
               [(eq? tag 'info) (style-with-dim (style))]
               [(eq? tag 'error) (style-fg (style) Color/Red)]
               [else (style)])])
    (cond
      [selected? (style-with-reversed base)]
      [marked? (style-with-bold base)]
      [else base])))

;;@doc
;; Draw a bordered frame over `rect` with `title` centred on the top border.
;; Returns the inner content area (rect minus the one-cell border).
(define (draw-frame buffer rect title)
  (let ([block (make-block (theme->bg *helix.cx*) (theme->fg *helix.cx*) "all" "plain")])
    (buffer/clear buffer rect)
    (block/render buffer rect block)
    (let ([x (area-x rect)]
          [y (area-y rect)]
          [w (area-width rect)])
      (when (> w 4)
        (frame-set-string! buffer (+ x 2) y
          (truncate-string title (- w 4))
          (tag->style 'title #f))))
    (area (+ (area-x rect) 2)
      (+ (area-y rect) 1)
      (max 0 (- (area-width rect) 4))
      (max 0 (- (area-height rect) 2)))))

;;@doc Rows that fit in `area`, reserving the last line for the status line.
(define (visible-row-count area)
  (max 1 (- (area-height area) 1)))

;;@doc
;; Draw the visible slice of `rows` into `area`, starting at scroll `top`,
;; highlighting the `cursor` row. Optional `marked` is a list of marked row
;; indices, drawn bold. Each row is a hash with 'text and 'tag.
(define (draw-rows buffer area rows cursor top . opt)
  (let* ([marked (if (pair? opt) (car opt) '())]
         [x (area-x area)]
         [y (area-y area)]
         [w (area-width area)]
         [height (visible-row-count area)]
         [total (length rows)]
         [end (min total (+ top height))])
    (let loop ([i top])
      (when (< i end)
        (let* ([row (list-ref rows i)]
               [tag (hash-ref row 'tag)]
               [text (hash-ref row 'text)]
               [selected? (= i cursor)]
               [marked? (and (not selected?) (member i marked) #t)]
               [screen-y (+ y (- i top))])
          (frame-set-string! buffer x screen-y
            (pad-right (truncate-string text w) w)
            (tag->style tag selected? marked?))
          (loop (+ i 1)))))))

;;@doc Draw `text` on the bottom line of `area` (the status/echo line).
(define (draw-status-line buffer area text tag)
  (let ([x (area-x area)]
        [y (+ (area-y area) (- (area-height area) 1))]
        [w (area-width area)])
    (when (> w 0)
      (frame-set-string! buffer x y
        (pad-right (truncate-string text w) w)
        (tag->style tag #f)))))
