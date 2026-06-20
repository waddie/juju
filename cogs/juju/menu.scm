;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; menu.scm - a transient-style popup menu (Magit transients, in miniature)
;;;
;;; A key-dispatch popup: it lists switches and actions, each bound to a single
;;; key. Pressing a switch key toggles a flag (the menu stays open); pressing an
;;; action key closes the menu and runs the action with the current switch state.
;;; This is the discoverable counterpart to the status view's direct action keys
;;; and to the typed commands: a `:juju-rebase-menu` pops the rebase transient
;;; rather than forcing the user to remember `:juju-rebase --autosquash <ref>`.
;;;
;;; The component is generic: it knows nothing of backends or the model. Callers
;;; build entries with menu-info / menu-switch / menu-action (menu-model.scm) and
;;; pass action thunks that close over whatever they need (a backend, a refresh
;;; callback). This keeps the dependency direction clean (commands.scm builds
;;; menus; menu.scm depends on nothing juju-specific) and avoids a cycle with the
;;; status view. The pure entries->rows projection lives in menu-model.scm.

(require-builtin helix/components)
(require "helix/misc.scm")
(require "menu-model.scm")
(require "render.scm")
(require "keys.scm")

(provide show-menu)

(define COMPONENT-NAME "juju-menu")

(struct menu-state (title entries switches) #:mutable #:transparent)

;;; Rendering ;;;

(define (render-menu state-box rect buffer)
  (let* ([state (unbox state-box)]
         [content (draw-frame buffer (overlay-area rect) (menu-state-title state))]
         [rows (menu-rows (menu-state-entries state) (menu-state-switches state))])
    ;; A menu is short and never scrolls: draw from the top with no cursor row
    ;; (cursor -1 never matches a row index).
    (draw-rows buffer content rows -1 0)
    (draw-status-line buffer content "press a key   q quit" 'info)))

;;; Event handling ;;;

;; Find the interactive entry whose key matches `event`, or #f.
(define (entry-for-key entries event)
  (let loop ([es entries])
    (cond
      [(null? es) #f]
      [(let ([k (menu-entry-key (car es))]) (and k (char-is? event k))) (car es)]
      [else (loop (cdr es))])))

(define (toggle-switch! state flag)
  (let ([sw (menu-state-switches state)])
    (set-menu-state-switches! state
      (hash-insert sw flag (not (hash-ref sw flag))))))

(define (handle-menu state-box event)
  (let ([state (unbox state-box)])
    (cond
      [(key-event-escape? event) (close-menu) event-result/close]
      [(char-is? event #\q) (close-menu) event-result/close]
      [else
        (let ([e (entry-for-key (menu-state-entries state) event)])
          (cond
            [(not e) event-result/consume]
            [(eq? (hash-ref e 'kind) 'switch)
              (toggle-switch! state (hash-ref e 'flag))
              event-result/consume]
            [else ; action: close, then run with the current switch state
              (let ([thunk (hash-ref e 'action)]
                    [switches (menu-state-switches state)])
                (close-menu)
                (thunk switches)
                event-result/close)]))])))

(define (close-menu) (pop-last-component-by-name! COMPONENT-NAME))

;;@doc
;; Open a transient menu titled `title` listing `entries` (built with menu-info /
;; menu-switch / menu-action). Switch keys toggle in place; an action key closes
;; the menu and runs its thunk with the current switch-state hash.
(define (show-menu title entries)
  (let* ([state-box (box (menu-state title entries (initial-switches entries)))]
         [handlers (hash "handle_event" handle-menu
                    "cursor"
                    (lambda (state-box rect) #f)
                    "required_size"
                    (lambda (state-box size) size))]
         [component (new-component! COMPONENT-NAME state-box render-menu handlers)])
    (push-component! component)))
