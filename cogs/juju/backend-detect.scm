;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; backend-detect.scm - choose the active backend
;;;
;;; Walks up from a starting directory to the workspace root (the nearest
;;; ancestor holding `.jj` or `.git`), decides which backend to use, and builds
;;; it. A colocated repo (both present) defaults to jj unless the user pinned a
;;; backend for that workspace with `:juju-backend`.

(require "backend-git.scm")
(require "backend-jj.scm")
(require "backend-interface.scm")
(require "config.scm")
(require "string-utils.scm")

(provide workspace-root-from
  detect-backend-name
  choose-backend-name
  available-backends
  colocated?
  active-backend
  make-backend-named)

;; Parent directory of `path`, or #f at the filesystem root.
(define (parent-dir path)
  (if (string=? path "/")
    #f ; the root has no parent; stops the walk
    (let ([trimmed (if (and (> (string-length path) 1) (string-suffix? "/" path))
                    (substring path 0 (- (string-length path) 1))
                    path)])
      (let loop ([i (- (string-length trimmed) 1)])
        (cond
          [(< i 0) #f]
          [(char=? (string-ref trimmed i) #\/)
            (if (= i 0) "/" (substring trimmed 0 i))]
          [else (loop (- i 1))])))))

(define (has-vcs-dir? dir name)
  (let ([p (path-join dir name)])
    (and (path-exists? p) (is-dir? p))))

;;@doc
;; Nearest ancestor of `start` (inclusive) that contains a `.jj` or `.git`
;; directory, or #f if none up to the filesystem root.
(define (workspace-root-from start)
  (let loop ([dir start])
    (cond
      [(not dir) #f]
      [(or (has-vcs-dir? dir ".jj") (has-vcs-dir? dir ".git")) dir]
      [else (loop (parent-dir dir))])))

;;@doc
;; Which backends a workspace root physically supports: a list possibly holding
;; 'jj and/or 'git.
(define (available-backends root)
  (append
    (if (has-vcs-dir? root ".jj") '(jj) '())
    (if (has-vcs-dir? root ".git") '(git) '())))

;;@doc
;; #t when `root` is a colocated repo: both `.git` and `.jj` are present. A git
;; mutation in such a repo desyncs jj's recorded working copy until the next jj
;; command re-imports it, which juju notes (config `juju-warn-colocated`).
(define (colocated? root)
  (and (has-vcs-dir? root ".jj") (has-vcs-dir? root ".git") #t))

;;@doc
;; Pure backend-selection policy: given the physically `avail`able backends, an
;; optional `override`, and the `colocated-default`, pick the backend name (or
;; #f). A valid override wins; then the colocated default when both exist; then
;; whichever single backend is present. Kept separate from the filesystem probe
;; so the precedence is testable without a repository.
(define (choose-backend-name avail override colocated-default)
  (cond
    [(and override (member override avail)) override]
    [(and (member 'jj avail) (member 'git avail)) colocated-default]
    [(member 'jj avail) 'jj]
    [(member 'git avail) 'git]
    [else #f]))

;;@doc
;; The backend to use for `root`: the per-workspace override if set and valid,
;; else the colocated default when both exist, else the only one present, else
;; #f.
(define (detect-backend-name root)
  (choose-backend-name
    (available-backends root)
    (workspace-backend-override root)
    (juju-colocated-default)))

;;@doc Construct the backend named `name` ('git | 'jj) for `root`, or #f.
(define (make-backend-named name root)
  (cond
    [(eq? name 'git) (make-git-backend root)]
    [(eq? name 'jj) (make-jj-backend root)]
    [else #f]))

;;@doc
;; The active backend value for a workspace containing `start`, or #f when
;; `start` is not inside a git/jj repository. This is the single entry point the
;; commands use to resolve "which backend".
(define (active-backend start)
  (let ([root (workspace-root-from start)])
    (if (not root)
      #f
      (let ([name (detect-backend-name root)])
        (make-backend-named name root)))))
