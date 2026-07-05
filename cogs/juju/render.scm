;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; render.scm - juju's binding of the shared drawing helpers
;;;
;;; The drawing itself lives in the ui-utils.hx library; this module fixes the
;;; two juju-specific parameters and re-exposes the helpers under the same
;;; signatures the views have always used: the overlay scale comes from config
;;; (`juju-overlay-scale`), and `juju-tag->style` overrides the 'section tag
;;; with the configured colour (`juju-section-color`). Both are read per call,
;;; so live config changes keep taking effect.

(require-builtin helix/components)
(require "config.scm")
(require (prefix-in ui. "ui-utils.hx/geometry.scm"))
(require (prefix-in ui. "ui-utils.hx/style.scm"))
(require (prefix-in ui. "ui-utils.hx/draw.scm"))

(provide overlay-area
  draw-frame
  draw-rows
  draw-status-line
  visible-row-count
  juju-tag->style)

;; Section headers carry the configured colour, bold. Unknown colour names
;; fall back to plain bold.
(define (section-style)
  (let ([c (ui.color-for-name (juju-section-color))])
    (style-with-bold (if c (style-fg (style) c) (style)))))

;;@doc juju's tag->style: the shared default table with the 'section override.
(define juju-tag->style (ui.make-tag->style (hash 'section section-style)))

;;@doc
;; Centre a modal overlay within `rect` at `juju-overlay-scale`%.
(define (overlay-area rect)
  (ui.overlay-area rect (juju-overlay-scale)))

;;@doc
;; Draw a bordered frame with `title`; returns the inner content area.
(define (draw-frame buffer rect title)
  (ui.draw-frame buffer rect title juju-tag->style))

;;@doc Rows that fit in `area`, reserving the last line for the status line.
(define (visible-row-count area)
  (ui.visible-row-count area))

;;@doc
;; Draw the visible slice of `rows`, highlighting `cursor`; optional `marked`
;; indices are drawn bold.
(define (draw-rows buffer area rows cursor top . opt)
  (ui.draw-rows buffer area rows cursor top
    (if (pair? opt) (car opt) '())
    juju-tag->style))

;;@doc Draw `text` on the bottom line of `area`, styled by `tag`.
(define (draw-status-line buffer area text tag)
  (ui.draw-status-line buffer area text tag juju-tag->style))
