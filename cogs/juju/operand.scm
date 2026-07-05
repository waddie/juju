;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; operand.scm - what a selection acts on
;;;
;;; juju is selection-first: the rows under the cursor (or marked) decide the
;;; operand. This module turns a set of selected row indices into a backend-ready
;;; list of operand specs, one per file, each tagged with its scope:
;;;
;;;   scope 'file   - act on the whole file (stage/unstage/discard the path)
;;;   scope 'lines  - act on selected diff lines only (partial-hunk patch)
;;;
;;; A spec is a hash:
;;;   'section-kind  the section the file sits in ('unstaged 'staged ...)
;;;   'path          the file path
;;;   'status-code   the file's status code ('modified 'added 'untracked ...)
;;;   'scope         'file or 'lines
;;;   'lines         (only when scope 'lines) a hash hunk-index -> (hash
;;;                  body-line-index -> #t): exactly the change lines selected
;;;
;;; Rules: a selected section row contributes every file in that section (whole
;;; file); a selected file row contributes that file (whole file); a selected
;;; diff add/del row contributes just that line. When both a whole-file and
;;; line-level selection land on the same file, whole-file wins (the coarser
;;; operand subsumes the finer). Pure: rows in, specs out.

(require "view-rows.scm")
(require "model.scm")
(require "ui-utils.hx/strings.scm")

(provide resolve-operands
  resolve-revs)

;;@doc
;; Resolve selected row `indices` (a non-empty list of integers into `rows`) into
;; a list of per-file operand specs, in first-seen file order. Indices that point
;; at non-actionable rows (headers, blanks, hunk headers, context lines) drop
;; out. Returns '() when nothing actionable is selected.
(define (resolve-operands rows indices)
  (let ([contribs (apply append
                   (map (lambda (i) (row->contribs rows i)) indices))])
    (merge-contribs contribs)))

;; One selected row -> a list of contributions (file-scope or single-line).
(define (row->contribs rows i)
  (if (or (< i 0) (>= i (length rows)))
    '()
    (let ([r (list-ref rows i)])
      (cond
        ;; A section: every file row belonging to it, each whole-file.
        [(eq? (row-type r) 'section)
          (let ([kind (row-section-kind r)])
            (filter-map
              (lambda (rr)
                (and (eq? (row-type rr) 'file)
                  (eq? (row-section-kind rr) kind)
                  (file-contrib rr)))
              rows))]
        ;; A file row: the whole file.
        [(eq? (row-type r) 'file) (list (file-contrib r))]
        ;; A diff add/del body row: just that line. Context/meta/header rows
        ;; carry no change, so they contribute nothing.
        [(and (eq? (row-type r) 'diff)
            (row-hunk-index r)
            (or (eq? (row-tag r) 'diff-add) (eq? (row-tag r) 'diff-del)))
          (list (lines-contrib r))]
        [else '()]))))

(define (file-contrib r)
  (hash 'section-kind (row-section-kind r)
    'path
    (row-path r)
    'status-code
    (row-status-code r)
    'scope
    'file))

(define (lines-contrib r)
  (hash 'section-kind (row-section-kind r)
    'path
    (row-path r)
    'status-code
    (row-status-code r)
    'scope
    'lines
    'hunk-index
    (row-hunk-index r)
    'body-line-index
    (row-body-line-index r)))

;;; Merging ;;;

;; Group contributions by (section-kind, path), preserving first-seen order, and
;; collapse line-level contributions into a per-hunk line set. A whole-file
;; contribution anywhere for a file forces that file to 'file scope.
(define (merge-contribs contribs)
  (let loop ([cs contribs] [order '()] [table (hash)])
    (if (null? cs)
      (map (lambda (k) (finalize (hash-ref table k))) (reverse order))
      (let* ([c (car cs)]
             [k (contrib-key c)]
             [had? (hash-contains? table k)]
             [acc (merge-one (if had? (hash-ref table k) #f) c)])
        (loop (cdr cs) (if had? order (cons k order)) (hash-insert table k acc))))))

(define (contrib-key c)
  (string-append (symbol->string (hash-ref c 'section-kind))
    (string (integer->char 0))
    (hash-ref c 'path)))

;; Fold contribution `c` into accumulator `acc` (#f if first for this file). The
;; accumulator mirrors a spec but always carries a 'lines map; finalize trims it.
(define (fresh-acc c)
  (hash 'section-kind (hash-ref c 'section-kind)
    'path
    (hash-ref c 'path)
    'status-code
    (hash-ref c 'status-code)
    'scope
    'lines
    'lines
    (hash)))

(define (merge-one acc c)
  (let ([base (if acc acc (fresh-acc c))])
    (if (eq? (hash-ref c 'scope) 'file)
      ;; Whole-file dominates: keep scope 'file, line map irrelevant.
      (hash-insert base 'scope 'file)
      ;; Line-level: add (hunk-index, body-line-index) to the set, unless the
      ;; file is already whole-file scoped.
      (if (eq? (hash-ref base 'scope) 'file)
        base
        (let* ([hi (hash-ref c 'hunk-index)]
               [bi (hash-ref c 'body-line-index)]
               [lines (hash-ref base 'lines)]
               [set (if (hash-contains? lines hi) (hash-ref lines hi) (hash))]
               [set2 (hash-insert set bi #t)])
          (hash-insert base 'lines (hash-insert lines hi set2)))))))

;; Produce the public spec: drop the 'lines map for whole-file specs.
(define (finalize acc)
  (if (eq? (hash-ref acc 'scope) 'file)
    (hash 'section-kind (hash-ref acc 'section-kind)
      'path
      (hash-ref acc 'path)
      'status-code
      (hash-ref acc 'status-code)
      'scope
      'file)
    acc))

;;; Commit / ref operands ;;;
;;;
;;; The revision-section counterpart to resolve-operands: history and branch
;;; actions act on commit rows (recent/unpushed/unpulled/operations/bookmarks)
;;; rather than file rows. A selected section contributes its commit rows; a
;;; selected commit row contributes itself. The same selection thus drives a
;;; file action or a commit action depending on which key is pressed.

;;@doc
;; Resolve selected row `indices` into commit/ref operand specs, first-seen
;; order, deduplicated by (kind, rev). Non-commit rows drop out. Each spec is a
;; hash: 'kind (section kind), 'rev (id/change-id/op-id/bookmark name the backend
;; acts on), 'short (display id), 'subject. Returns '() when nothing commit-like
;; is selected.
(define (resolve-revs rows indices)
  (dedupe-revs
    (apply append (map (lambda (i) (row->rev-contribs rows i)) indices))))

(define (row->rev-contribs rows i)
  (if (or (< i 0) (>= i (length rows)))
    '()
    (let ([r (list-ref rows i)])
      (cond
        [(eq? (row-type r) 'commit) (list (rev-contrib r))]
        ;; A section row contributes every commit row beneath it.
        [(eq? (row-type r) 'section)
          (let ([kind (row-section-kind r)])
            (filter-map
              (lambda (rr)
                (and (eq? (row-type rr) 'commit)
                  (eq? (row-section-kind rr) kind)
                  (rev-contrib rr)))
              rows))]
        [else '()]))))

(define (rev-contrib r)
  (let ([c (row-object r)])
    (hash 'kind (row-section-kind r)
      'rev
      (commit-record-id c)
      'short
      (commit-record-short-id c)
      'subject
      (commit-record-subject c))))

(define (dedupe-revs specs)
  (let loop ([ss specs] [seen (hash)] [acc '()])
    (if (null? ss)
      (reverse acc)
      (let* ([s (car ss)]
             [k (string-append (symbol->string (hash-ref s 'kind))
                 (string (integer->char 0))
                 (hash-ref s 'rev))])
        (if (hash-contains? seen k)
          (loop (cdr ss) seen acc)
          (loop (cdr ss) (hash-insert seen k #t) (cons s acc)))))))
