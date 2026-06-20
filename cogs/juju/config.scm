;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; config.scm - user-tunable settings and per-workspace state
;;;
;;; Settings are plain module state with setters so a user's init.scm can tune
;;; them. The per-workspace backend override (set by :juju-backend) is
;;; remembered here keyed by workspace root, so a colocated repo stays on the
;;; backend the user picked for the session.

(provide juju-recent-count
  set-juju-recent-count!
  juju-log-count
  set-juju-log-count!
  juju-colocated-default
  set-juju-colocated-default!
  juju-auto-refresh
  set-juju-auto-refresh!
  juju-warn-colocated
  set-juju-warn-colocated!
  juju-overlay-scale
  set-juju-overlay-scale!
  juju-section-color
  set-juju-section-color!
  workspace-backend-override
  set-workspace-backend-override!
  clear-workspace-backend-override!)

;; How many entries the "Recent commits"/"Recent changes" section shows.
(define *recent-count* (box 10))
(define (juju-recent-count) (unbox *recent-count*))
(define (set-juju-recent-count! n) (set-box! *recent-count* n))

;; How many entries :juju-log lists.
(define *log-count* (box 50))
(define (juju-log-count) (unbox *log-count*))
(define (set-juju-log-count! n) (set-box! *log-count* n))

;; Which backend a colocated repo (.git and .jj both present) defaults to.
(define *colocated-default* (box 'jj))
(define (juju-colocated-default) (unbox *colocated-default*))
(define (set-juju-colocated-default! sym) (set-box! *colocated-default* sym))

;; Whether a typed command refreshes an open status view after a mutation. The
;; in-view action keys always reload (you are looking at the result); this gates
;; only the side-channel refresh `refresh-open-view!` does for typed commands.
(define *auto-refresh* (box #t))
(define (juju-auto-refresh) (unbox *auto-refresh*))
(define (set-juju-auto-refresh! on?) (set-box! *auto-refresh* on?))

;; Whether to surface the colocated-repo note: in a repo with both .git and .jj,
;; a git mutation desyncs jj's recorded working copy until the next jj command
;; re-imports it. The note is informational, not a block; turn it off to silence.
(define *warn-colocated* (box #t))
(define (juju-warn-colocated) (unbox *warn-colocated*))
(define (set-juju-warn-colocated! on?) (set-box! *warn-colocated* on?))

;; Overlay size as a percentage of the terminal (view placement). The status and
;; text views float as centred modals at this scale; lower it for a tighter
;; popup, raise it toward 100 for near-fullscreen. Clamped to 10..100 by render.
(define *overlay-scale* (box 90))
(define (juju-overlay-scale) (unbox *overlay-scale*))
(define (set-juju-overlay-scale! pct) (set-box! *overlay-scale* pct))

;; Colour of section headers in the status view, as a symbol render maps to a
;; concrete colour ('yellow 'green 'cyan 'blue 'magenta 'red 'white 'default).
;; The single colour knob for now; the renderer falls back to its default style
;; for an unknown symbol.
(define *section-color* (box 'yellow))
(define (juju-section-color) (unbox *section-color*))
(define (set-juju-section-color! sym) (set-box! *section-color* sym))

;; root path -> 'git | 'jj override chosen via :juju-backend.
(define *overrides* (box (hash)))

;;@doc Backend override for `root`, or #f if none set.
(define (workspace-backend-override root)
  (let ([h (unbox *overrides*)])
    (if (hash-contains? h root) (hash-ref h root) #f)))

;;@doc Remember that `root` should use `backend-sym` ('git | 'jj).
(define (set-workspace-backend-override! root backend-sym)
  (set-box! *overrides* (hash-insert (unbox *overrides*) root backend-sym)))

;;@doc Forget any override for `root`.
(define (clear-workspace-backend-override! root)
  (let ([h (unbox *overrides*)])
    (when (hash-contains? h root)
      (set-box! *overrides* (hash-remove h root)))))
