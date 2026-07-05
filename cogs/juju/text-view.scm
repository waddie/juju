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
;;; is the shared overlay-view shell (ui-utils.hx) with juju's diff-line
;;; colouring; read-only, owns its keys, never touches a buffer.

(require-builtin helix/components)
(require "helix/misc.scm")
(require "config.scm")
(require "render.scm") ; juju-tag->style
(require "ui-utils.hx/strings.scm")
(require "ui-utils.hx/overlay-view.scm")

(provide show-text-view)

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

(define text-view-spec
  (make-overlay-view
    #:name
    "juju-text-view"
    #:title
    tv-state-title
    #:rows
    tv-state-rows
    #:cursor
    tv-state-cursor
    #:set-cursor!
    set-tv-state-cursor!
    #:top
    tv-state-top
    #:set-top!
    set-tv-state-top!
    #:status
    (lambda (state) (cons "j/k move  q quit" 'info))
    #:tag->style
    juju-tag->style
    #:overlay-scale
    (lambda () (juju-overlay-scale))))

;;@doc
;; Open a scrollable overlay titled `title` showing `lines` (a list of strings).
;; Echoes instead when `lines` is empty.
(define (show-text-view title lines)
  (if (null? lines)
    (set-status! (string-append "juju: nothing to show (" title ")"))
    (open-overlay-view! text-view-spec
      (tv-state title (map line->row lines) 0 0))))
