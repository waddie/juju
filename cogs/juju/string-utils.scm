;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; string-utils.scm - VCS-flavoured string helpers
;;;
;;; The parser-facing remnant: separators for templated VCS output and the
;;; helpers that read it. The general-purpose string functions (splitting,
;;; trimming, padding, truncating) live in the shared ui-utils.hx library;
;;; require "ui-utils.hx/strings.scm" for those.

(require "ui-utils.hx/strings.scm")

(provide field-split
  path-join
  last-line
  opt)

;; `split-many` (used here and in the parsers) is a Steel prelude global,
;; always in scope without a require - the same way `map`/`filter` are.

;; Record/field separators used by the VCS log/op templates. ASCII unit (0x1f)
;; and record (0x1e) separators never appear in commit metadata, so they make
;; unambiguous delimiters for templated output.
(define UNIT-SEP (string (integer->char 31)))
(define RECORD-SEP (string (integer->char 30)))

;;@doc
;; Split `str` on the unit separator into its fields. Used by parsers reading
;; templated VCS output (see the *-SEP constants).
(define (field-split str)
  (split-many str UNIT-SEP))

;;@doc
;; Join a directory and a name with a single "/", tolerating a trailing slash on
;; `dir`.
(define (path-join dir name)
  (if (string-suffix? "/" dir)
    (string-append dir name)
    (string-append dir "/" name)))

;;@doc
;; The last non-blank line of `s`, trimmed, or "" when there is none. VCS
;; commands write the salient message (e.g. "nothing to commit", a push refspec)
;; on the last line of stdout/stderr, so this isolates the reportable tail.
(define (last-line s)
  (let ([lines (filter (lambda (l) (not (string-blank? l))) (split-lines s))])
    (if (null? lines) "" (string-trim (list-ref lines (- (length lines) 1))))))

;;@doc
;; Look up `key` in the option hash `opts`, returning `default` when `opts` is
;; not a hash or lacks the key. Used by backends to read optional op arguments
;; ('remote, 'onto, 'into, ...) uniformly.
(define (opt opts key default)
  (if (and (hash? opts) (hash-contains? opts key)) (hash-ref opts key) default))
