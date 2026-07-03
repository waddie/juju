;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; keys.scm - key-event predicates shared by the overlay views
;;;
;;; Thin helpers layered over the helix/components key primitives so the status
;;; view and the text view test keypresses the same way. `key-event-modifier`
;;; returns the modifier bitset as an integer (0 when no modifier is held), not
;;; #f, and 0 is truthy in Scheme, so a plain keypress must be tested with
;;; `no-modifier?` rather than `not`.

(require-builtin helix/components)

(provide no-modifier?
  char-is?
  ctrl-char?)

;;@doc #t when no modifier key is held.
(define (no-modifier? event)
  (let ([m (key-event-modifier event)])
    (or (not m) (equal? m 0))))

;;@doc #t when `event` is the character `ch` with no modifier beyond shift.
;; Uppercase letters arrive with the shift modifier set, and the char already
;; encodes the case, so shift must be accepted or uppercase keys never match.
(define (char-is? event ch)
  (and (let ([m (key-event-modifier event)])
        (or (not m) (equal? m 0) (equal? m key-modifier-shift)))
    (equal? (key-event-char event) ch)))

;;@doc #t when `event` is Ctrl + `ch`.
(define (ctrl-char? event ch)
  (and (equal? (key-event-modifier event) key-modifier-ctrl)
    (equal? (key-event-char event) ch)))
