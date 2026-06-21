;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; menu-model.scm - the pure model behind the transient menu

(provide menu-info
  menu-switch
  menu-arg
  menu-action
  menu-entry-key
  initial-switches
  menu-rows)

;;; Entry constructors ;;;
;;;
;;; An entry is a hash tagged by 'kind. info: a heading/blank line. switch: a
;;; toggle keyed by `key`, carrying the flag symbol it sets and its default.
;;; arg: a value infix keyed by `key`, prompting for a string stored under its
;;; flag. action: a keyed command whose `action` is (lambda (switches) ...),
;;; switches being a hash of flag-symbol -> bool-or-string.

;;@doc A non-interactive heading/blank line in a menu.
(define (menu-info text) (hash 'kind 'info 'text text))

;;@doc
;; A toggle: pressing `key` flips `flag` in the switch state. `default` is its
;; initial value. `label` describes it.
(define (menu-switch key flag label default)
  (hash 'kind 'switch 'key key 'flag flag 'label label 'default default))

;;@doc
;; A value infix: pressing `key` prompts for a value stored in the switch state
;; under `flag` (a string). `default` is its initial value (a string, or #f for
;; unset). `label` describes it, e.g. "-n count".
(define (menu-arg key flag label default)
  (hash 'kind 'arg 'key key 'flag flag 'label label 'default default))

;;@doc
;; An action: pressing `key` closes the menu and calls `thunk` with the current
;; switch-state hash. `label` describes it.
(define (menu-action key label thunk)
  (hash 'kind 'action 'key key 'label label 'action thunk))

;;@doc The key char of an interactive entry, or #f for an info line.
(define (menu-entry-key e)
  (if (eq? (hash-ref e 'kind) 'info) #f (hash-ref e 'key)))

;;@doc
;; Initial switch state for `entries`: each switch/arg flag mapped to its
;; default.
(define (initial-switches entries)
  (foldl
    (lambda (e acc)
      (let ([kind (hash-ref e 'kind)])
        (if (or (eq? kind 'switch) (eq? kind 'arg))
          (hash-insert acc (hash-ref e 'flag) (hash-ref e 'default))
          acc)))
    (hash)
    entries))

;; A two-space-padded key cell so labels line up regardless of key width.
(define (key-cell key) (string-append "  " (string key) "  "))

;;@doc
;; Render `entries` (with `switches` supplying current toggle states) into row
;; hashes ('text 'tag) the shared drawing helpers paint. Pure.
(define (menu-rows entries switches)
  (map
    (lambda (e)
      (let ([kind (hash-ref e 'kind)])
        (cond
          [(eq? kind 'info) (hash 'text (hash-ref e 'text) 'tag 'info)]
          [(eq? kind 'switch)
            (let ([on (and (hash-contains? switches (hash-ref e 'flag))
                       (hash-ref switches (hash-ref e 'flag)))])
              (hash 'text
                (string-append (key-cell (hash-ref e 'key))
                  (if on "[x] " "[ ] ")
                  (hash-ref e 'label))
                'tag
                (if on 'section 'file)))]
          [(eq? kind 'arg)
            (let* ([raw (and (hash-contains? switches (hash-ref e 'flag))
                         (hash-ref switches (hash-ref e 'flag)))]
                   [set? (and (string? raw) (not (string=? raw "")))])
              (hash 'text
                (string-append (key-cell (hash-ref e 'key))
                  (hash-ref e 'label)
                  ": "
                  (if set? raw "(unset)"))
                'tag
                (if set? 'section 'file)))]
          [else ; action
            (hash 'text
              (string-append (key-cell (hash-ref e 'key)) (hash-ref e 'label))
              'tag
              'file)])))
    entries))
