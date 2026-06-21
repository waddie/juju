;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; view-rows.scm - flatten a status into displayable rows
;;;
;;; The status view renders a flat list of rows that map 1:1 to screen lines.
;;; Each row carries its display text, a style tag, and the model object it
;;; stands for, so an action (or the cursor) can read exactly which section,
;;; file, or diff line it sits on regardless of folding. This is the spine of
;;; the selection-first model. Pure: no component or process dependencies, so it
;;; is unit-testable on its own.

(require "model.scm")
(require "string-utils.scm")

(provide build-rows
  diff-cache-key
  row-type
  row-tag
  row-text
  row-object
  row-section-kind
  row-selectable?
  row-line
  row-path
  row-status-code
  row-hunk-index
  row-body-line-index
  make-row
  ;; navigation and search (pure, over the flat row list)
  section-row-indices
  next-section-index
  prev-section-index
  parent-section-index
  nearest-selectable-index
  search-matches
  ;; projections shared by the overlay views
  blame-rows
  commit-show-lines
  diff-line->display)

;; A row is a hash. Helpers keep call sites readable.
(define (make-row type tag text object section-kind selectable?)
  (hash 'type type 'tag tag 'text text
    'object
    object
    'section-kind
    section-kind
    'selectable?
    selectable?))

(define (row-type r) (hash-ref r 'type))
(define (row-tag r) (hash-ref r 'tag))
(define (row-text r) (hash-ref r 'text))
(define (row-object r) (hash-ref r 'object))
(define (row-section-kind r) (hash-ref r 'section-kind))
(define (row-selectable? r) (hash-ref r 'selectable?))

;; New-file line number a diff row maps to, or #f for rows without one. Set on
;; diff body rows so visiting a diff line can jump to that line in the file.
(define (row-line r) (if (hash-contains? r 'line) (hash-ref r 'line) #f))

;; The path / status-code of the file a row belongs to. Set on file rows and on
;; diff body rows so operand resolution can group a selection by file without
;; walking the row list. #f on rows that do not stand for a file.
(define (row-path r) (if (hash-contains? r 'path) (hash-ref r 'path) #f))
(define (row-status-code r) (if (hash-contains? r 'status-code) (hash-ref r 'status-code) #f))

;; On a diff body row: the 0-based index of its hunk within the file's hunk list,
;; and the 0-based index of the line within that hunk's `hunk-lines`. Together
;; they key the `include?` predicate that builds a partial-staging patch. #f on
;; non-diff rows. hunk-index can legitimately be 0 (truthy in Scheme).
(define (row-hunk-index r) (if (hash-contains? r 'hunk-index) (hash-ref r 'hunk-index) #f))
(define (row-body-line-index r)
  (if (hash-contains? r 'body-line-index) (hash-ref r 'body-line-index) #f))

;;@doc Cache key for a file's fetched diff: section kind + path.
(define (diff-cache-key section-kind path)
  (string-append (symbol->string section-kind) ":" path))

;;@doc
;; Build the row list for `st`, consulting `fold` for collapse/expand state and
;; `diff-cache` (a hash of diff-cache-key -> list of hunk) for already-fetched
;; diffs. Header labels come first, then each section and its items.
(define (build-rows st fold diff-cache)
  (append
    (header-rows (status-header st))
    (list (blank-row))
    (apply append
      (map (lambda (sec) (section-rows sec fold diff-cache))
        (status-sections st)))))

(define (header-rows header)
  (map (lambda (pair)
        (make-row 'header 'header-label
          (string-append (pad-right (string-append (car pair) ":") 14) (cdr pair))
          #f
          #f
          #f))
    header))

(define (blank-row) (make-row 'blank 'info "" #f #f #f))

(define (section-rows sec fold diff-cache)
  (let* ([kind (section-kind sec)]
         [collapsed? (section-collapsed? sec)]
         [items (section-items sec)]
         [caret (if collapsed? "▸" "▾")]
         [head (make-row 'section 'section
                (string-append caret " " (section-title sec)
                  " ("
                  (number->string (length items))
                  ")")
                sec
                kind
                #t)])
    (if collapsed?
      (list head)
      (append (list head)
        (apply append (map (lambda (it) (item-rows it kind fold diff-cache)) items))
        (list (blank-row))))))

(define (item-rows item kind fold diff-cache)
  (if (file-item? item)
    (file-rows item kind fold diff-cache)
    (commit-rows item kind)))

(define (file-rows fi kind fold diff-cache)
  (let* ([path (file-item-path fi)]
         [expanded? (fold-file-expanded? fold kind path)]
         [caret (if expanded? "  ▾ " "  ▸ ")]
         [label (string-append caret
                 (status-code-label (file-item-status-code fi))
                 " "
                 path
                 (rename-suffix fi))]
         [head (hash-insert
                (hash-insert (make-row 'file 'file label fi kind #t) 'path path)
                'status-code
                (file-item-status-code fi))])
    (if (not expanded?)
      (list head)
      (let ([hunks (let ([k (diff-cache-key kind path)])
                    (if (hash-contains? diff-cache k) (hash-ref diff-cache k) #f))])
        (cons head (diff-rows hunks fi kind))))))

(define (rename-suffix fi)
  (let ([e (file-item-extra fi)])
    (if (and (hash-contains? e 'orig-path) (hash-ref e 'orig-path))
      (string-append "  ← " (hash-ref e 'orig-path))
      "")))

(define (status-code-label code)
  (cond
    [(eq? code 'modified) "modified"]
    [(eq? code 'added) "added   "]
    [(eq? code 'deleted) "deleted "]
    [(eq? code 'renamed) "renamed "]
    [(eq? code 'copied) "copied  "]
    [(eq? code 'untracked) "new     "]
    [(eq? code 'conflicted) "conflict"]
    [(eq? code 'type-changed) "typechg "]
    [else "changed "]))

;; Diff rows under an expanded file. #f hunks = not yet fetched; '() = no diff.
;; Each hunk is numbered by its position so a selected diff row can be tied back
;; to the exact hunk and line the partial-staging patch builder needs.
(define (diff-rows hunks fi kind)
  (cond
    [(not hunks) (list (make-row 'info 'info "      (loading diff…)" #f #f #f))]
    [(null? hunks) (list (make-row 'info 'info "      (no diff)" #f #f #f))]
    [else
      (let loop ([hs hunks] [hi 0] [acc '()])
        (if (null? hs)
          (apply append (reverse acc))
          (loop (cdr hs) (+ hi 1) (cons (hunk-rows (car hs) fi kind hi) acc))))]))

;; Thread the new-file line number down each hunk body so a diff row knows which
;; line it lands on. It starts at the hunk's new-range start and advances on
;; context and added lines (which exist in the new file), but not deleted ones.
;; `body-line-index` (j) is the 0-based position in `hunk-lines`, matching the
;; index `build-apply-patch`'s include? predicate expects.
(define (hunk-rows h fi kind hunk-index)
  (cons
    (make-row 'diff 'diff-header (string-append "    " (hunk-header h)) fi kind #f)
    (let loop ([lines (hunk-lines h)] [line (car (hunk-new-range h))] [j 0] [acc '()])
      (if (null? lines)
        (reverse acc)
        (let* ([dl (car lines)]
               [k (diff-line-kind dl)]
               [row (diff-line-row dl fi kind line hunk-index j)]
               [next (if (or (eq? k 'add) (eq? k 'context)) (+ line 1) line)])
          (loop (cdr lines) next (+ j 1) (cons row acc)))))))

(define (diff-line-row dl fi kind line hunk-index body-line-index)
  (let* ([k (diff-line-kind dl)]
         [prefix (cond [(eq? k 'add) "    +"] [(eq? k 'del) "    -"]
                  [(eq? k 'meta) "    "]
                  [else "     "])]
         [tag (cond [(eq? k 'add) 'diff-add] [(eq? k 'del) 'diff-del]
               [(eq? k 'meta) 'diff-meta]
               [else 'diff-context])])
    (hash-insert
      (hash-insert
        (hash-insert
          (hash-insert
            (hash-insert
              (make-row 'diff tag (string-append prefix (diff-line-text dl)) dl kind #t)
              'line
              line)
            'hunk-index
            hunk-index)
          'body-line-index
          body-line-index)
        'path
        (file-item-path fi))
      'status-code
      (file-item-status-code fi))))

(define (commit-rows c kind)
  (list (make-row 'commit 'commit
         (string-append "  " (pad-right (commit-record-short-id c) 12) " "
           (commit-record-subject c)
           (refs-suffix (commit-record-refs c)))
         c
         kind
         #t)))

(define (refs-suffix refs)
  (if (null? refs)
    ""
    (string-append "  (" (string-join refs ", ") ")")))

;;; Navigation and search ;;;
;;;
;;; Pure reads over the flat row list, the spine of the status view's
;;; section-jump and in-buffer search. Indices are positions in the row list, so
;;; the caller can drop the result straight into the cursor.

;;@doc The indices of the section-header rows in `rows`.
(define (section-row-indices rows)
  (let loop ([rs rows] [i 0] [acc '()])
    (cond
      [(null? rs) (reverse acc)]
      [(eq? (row-type (car rs)) 'section) (loop (cdr rs) (+ i 1) (cons i acc))]
      [else (loop (cdr rs) (+ i 1) acc)])))

;;@doc The nearest section-header index after `from`, or `from` when none (no wrap).
(define (next-section-index rows from)
  (let loop ([is (section-row-indices rows)])
    (cond
      [(null? is) from]
      [(> (car is) from) (car is)]
      [else (loop (cdr is))])))

;;@doc The nearest section-header index before `from`, or `from` when none (no wrap).
(define (prev-section-index rows from)
  (let loop ([is (reverse (section-row-indices rows))])
    (cond
      [(null? is) from]
      [(< (car is) from) (car is)]
      [else (loop (cdr is))])))

;;@doc
;; The enclosing section-header index at or before `from` (so a file/diff row
;; jumps up to its section). `from` itself when it precedes the first section.
(define (parent-section-index rows from)
  (let ([n (length rows)])
    (let loop ([i (min from (- n 1))])
      (cond
        [(< i 0) from]
        [(eq? (row-type (list-ref rows i)) 'section) i]
        [else (loop (- i 1))]))))

;;@doc
;; Index of the nearest selectable row scanning from `from` (inclusive, clamped
;; into range) in direction `dir` (+1/-1), falling back to the opposite
;; direction, or #f when no row is selectable. Backs the status view's page
;; movement, which must land on a selectable row even when the jump overshoots
;; the list.
(define (nearest-selectable-index rows from dir)
  (let ([n (length rows)])
    (if (= n 0)
      #f
      (let ([start (max 0 (min from (- n 1)))])
        (let ([hit (scan-selectable rows start dir n)])
          (if hit hit (scan-selectable rows start (- 0 dir) n)))))))

(define (scan-selectable rows i dir n)
  (cond
    [(or (< i 0) (>= i n)) #f]
    [(row-selectable? (list-ref rows i)) i]
    [else (scan-selectable rows (+ i dir) dir n)]))

;;@doc
;; The indices of rows whose text contains `query`, case-insensitive. '() for a
;; blank query.
(define (search-matches rows query)
  (if (or (not query) (string=? (string-trim query) ""))
    '()
    (let ([q (string-downcase query)])
      (let loop ([rs rows] [i 0] [acc '()])
        (cond
          [(null? rs) (reverse acc)]
          [(string-contains? (string-downcase (row-text (car rs))) q)
            (loop (cdr rs) (+ i 1) (cons i acc))]
          [else (loop (cdr rs) (+ i 1) acc)])))))

;;; Blame rows ;;;

;;@doc
;; Project blame-line records into rows for the blame view: "<short-id> <text>",
;; the first row of each commit run tagged 'commit so the run boundary carries
;; the colour (the row grid styles whole rows), the rest 'file.
(define (blame-rows lines)
  (let loop ([ls lines] [prev #f] [acc '()])
    (if (null? ls)
      (reverse acc)
      (let* ([bl (car ls)]
             [commit (blame-line-commit bl)]
             [tag (if (equal? commit prev) 'file 'commit)])
        (loop (cdr ls) commit (cons (blame-row bl tag) acc))))))

(define (blame-row bl tag)
  (hash 'text
    (string-append (pad-right (blame-line-short-id bl) 9) (blame-line-text bl))
    'tag
    tag))

;;; Commit show projection ;;;

;;@doc
;; Render a backend-show result (commit metadata + parsed hunks) as plain lines
;; the text view can colour by leading character.
(define (commit-show-lines shown)
  (let ([commit (hash-ref shown 'commit)]
        [hunks (hash-ref shown 'hunks)])
    (append
      (if commit
        (list
          (string-append "commit " (commit-record-id commit))
          (string-append "Author: " (commit-record-author commit))
          (string-append "Date:   " (commit-record-date commit))
          ""
          (string-append "    " (commit-record-subject commit))
          "")
        '())
      (apply append
        (map (lambda (h) (cons (hunk-header h) (map diff-line->display (hunk-lines h)))) hunks)))))

(define (diff-line->display dl)
  (let ([k (diff-line-kind dl)] [t (diff-line-text dl)])
    (cond
      [(eq? k 'add) (string-append "+" t)]
      [(eq? k 'del) (string-append "-" t)]
      [(eq? k 'meta) t]
      [else (string-append " " t)])))
