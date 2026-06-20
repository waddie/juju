;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; scroll.scm - scroll-position math shared by the overlay views
;;;
;;; Pure integer arithmetic, no component or process dependencies, so it is
;;; unit-testable in isolation. Both the status view and the text view keep a
;;; `top` (first visible row) and a `cursor`, and call `clamp-top` each render to
;;; keep the cursor on screen without scrolling past the end of the list.

(provide clamp-top)

;;@doc
;; Clamp the scroll `top` so `cursor` stays visible within `height` rows and the
;; list of `total` rows never scrolls past its end. Returns the new top.
(define (clamp-top top cursor height total)
  (let* ([max-top (max 0 (- total height))]
         [t (cond
             [(< cursor top) cursor]
             [(>= cursor (+ top height)) (+ (- cursor height) 1)]
             [else top])])
    (max 0 (min t max-top))))
