;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; string-utils.scm - shared string helpers
;;;
;;; Small pure helpers used across the codebase: splitting lines, trimming,
;;; padding, truncating. No process or component dependencies, so these are
;;; unit-testable in isolation.

(provide split-lines
  non-empty-lines
  string-blank?
  blank?
  trim-end
  trim-start
  string-trim
  pad-right
  truncate-string
  string-prefix?
  string-suffix?
  string-take
  string-drop
  field-split
  path-join
  last-line
  filter-map
  count-label
  confirmed?
  opt)

;; `split-many` (used throughout this module and the parsers) is a Steel prelude
;; global, always in scope without a require - the same way `map`/`filter` are.

;; Record/field separators used by the VCS log/op templates. ASCII unit (0x1f)
;; and record (0x1e) separators never appear in commit metadata, so they make
;; unambiguous delimiters for templated output.
(define UNIT-SEP (string (integer->char 31)))
(define RECORD-SEP (string (integer->char 30)))

;;@doc
;; Split a string into a list of lines on newline. A trailing newline does not
;; produce a final empty line. Returns '() for the empty string.
(define (split-lines str)
  (if (= (string-length str) 0)
    '()
    (let ([parts (split-many str "\n")])
      ;; split-many on "a\nb\n" yields ("a" "b" ""); drop a single trailing
      ;; empty produced by the terminating newline.
      (let ([rev (reverse parts)])
        (if (and (not (null? rev)) (string=? (car rev) ""))
          (reverse (cdr rev))
          parts)))))

;;@doc
;; Lines of `str` with blank lines removed.
(define (non-empty-lines str)
  (filter (lambda (l) (not (string-blank? l))) (split-lines str)))

;;@doc
;; #t when the string is empty or only whitespace.
(define (string-blank? str)
  (string=? "" (string-trim str)))

;;@doc
;; #t when `s` is #f, empty, or only whitespace. Like `string-blank?` but
;; tolerates #f, for optional inputs (a missing commit message, ref, etc.).
(define (blank? s)
  (or (not s) (string=? (string-trim s) "")))

(define (whitespace-char? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline) (char=? c #\return)))

;;@doc
;; Remove trailing whitespace.
(define (trim-end str)
  (let loop ([end (string-length str)])
    (if (and (> end 0) (whitespace-char? (string-ref str (- end 1))))
      (loop (- end 1))
      (substring str 0 end))))

;;@doc
;; Remove leading whitespace.
(define (trim-start str)
  (let ([len (string-length str)])
    (let loop ([start 0])
      (if (and (< start len) (whitespace-char? (string-ref str start)))
        (loop (+ start 1))
        (substring str start len)))))

;;@doc
;; Remove leading and trailing whitespace.
(define (string-trim str)
  (trim-end (trim-start str)))

;;@doc
;; Pad `str` on the right with spaces to at least `width` columns. Strings
;; already at or beyond `width` are returned unchanged (not truncated).
(define (pad-right str width)
  (let ([len (string-length str)])
    (if (>= len width)
      str
      (string-append str (make-string (- width len) #\space)))))

;;@doc
;; Truncate `str` to at most `width` characters, appending a single-character
;; ellipsis when it overflows. Width <= 0 yields the empty string.
(define (truncate-string str width)
  (cond
    [(<= width 0) ""]
    [(<= (string-length str) width) str]
    [(= width 1) "…"]
    [else (string-append (substring str 0 (- width 1)) "…")]))

;;@doc
;; #t when `str` starts with `prefix`.
(define (string-prefix? prefix str)
  (let ([pl (string-length prefix)]
        [sl (string-length str)])
    (and (>= sl pl) (string=? (substring str 0 pl) prefix))))

;;@doc
;; #t when `str` ends with `suffix`.
(define (string-suffix? suffix str)
  (let ([fl (string-length suffix)]
        [sl (string-length str)])
    (and (>= sl fl) (string=? (substring str (- sl fl) sl) suffix))))

;;@doc
;; First `n` characters of `str` (clamped).
(define (string-take str n)
  (substring str 0 (min n (string-length str))))

;;@doc
;; `str` with its first `n` characters removed (clamped).
(define (string-drop str n)
  (let ([len (string-length str)])
    (if (>= n len) "" (substring str n len))))

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
;; Map `f` over `xs`, dropping results that are #f. (Steel has no built-in.)
(define (filter-map f xs)
  (foldr (lambda (x acc) (let ([v (f x)]) (if v (cons v acc) acc))) '() xs))

;;@doc
;; A count with a pluralised noun: "1 item", "3 items".
(define (count-label n)
  (string-append (number->string n) (if (= n 1) " item" " items")))

;;@doc
;; #t when `input` is an affirmative answer to a y/N prompt ("y", "Y", "yes").
(define (confirmed? input)
  (and input
    (let ([s (string-trim input)])
      (or (string=? s "y") (string=? s "Y") (string=? s "yes")))))

;;@doc
;; Look up `key` in the option hash `opts`, returning `default` when `opts` is
;; not a hash or lacks the key. Used by backends to read optional op arguments
;; ('remote, 'onto, 'into, ...) uniformly.
(define (opt opts key default)
  (if (and (hash? opts) (hash-contains? opts key)) (hash-ref opts key) default))
