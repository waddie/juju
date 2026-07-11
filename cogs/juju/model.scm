;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; model.scm - the backend-independent data model
;;;
;;; Backends produce a `status` value; the view renders it without knowing which
;;; backend built it. Fold state (which sections are collapsed, which files are
;;; expanded) lives outside the status value in a per-view map keyed by stable
;;; ids, so a refresh can rebuild the status while the user's fold choices and
;;; lazily-fetched diffs survive.

(require "string-utils.scm")

(provide status status? status-header status-sections
  section
  section?
  section-id
  section-title
  section-kind
  section-items
  section-collapsed?
  file-item
  file-item?
  file-item-path
  file-item-status-code
  file-item-hunks
  file-item-expanded?
  file-item-extra
  hunk
  hunk?
  hunk-header
  hunk-old-range
  hunk-new-range
  hunk-lines
  diff-line
  diff-line?
  diff-line-kind
  diff-line-text
  commit-record
  commit-record?
  commit-record-id
  commit-record-short-id
  commit-record-author
  commit-record-date
  commit-record-subject
  commit-record-refs
  blame-line
  blame-line?
  blame-line-commit
  blame-line-short-id
  blame-line-orig-line
  blame-line-text
  make-blame-line
  make-status
  make-section
  maybe-section
  make-file-item
  make-hunk
  make-diff-line
  make-commit-record
  file-item-with-hunks
  file-item-renamed?
  file-item-binary?
  make-fold-state
  fold-section-collapsed?
  fold-file-expanded?
  set-fold-section-collapsed!
  set-fold-file-expanded!
  toggle-fold-section!
  toggle-fold-file!
  apply-fold-state
  section-by-kind
  status-file-count)

;;; Structs ;;;

;; header: alist of (label . value) pairs shown above the sections, e.g.
;;   (("Head" . "main  init") ("Push" . "origin/main +1 -0"))
(struct status (header sections) #:transparent)

;; A collapsible group of items. `kind` is the semantic section type the view
;; and keymap dispatch on; `id` is a stable string for fold state. `items` are
;; file-items or commit-records depending on kind.
;;   kind: 'untracked 'unstaged 'staged 'working-copy 'stashes 'unpushed
;;         'unpulled 'recent 'bookmarks 'conflicts 'operations
(struct section (id title kind items collapsed?) #:transparent)

;; A changed path. `hunks` is #f until the diff is lazily fetched (on expand),
;; '() when fetched and empty. `extra` is a hash carrying optional flags:
;;   'orig-path (rename/copy source), 'binary? , 'rename? , 'submodule?
;;   status-code: 'modified 'added 'deleted 'renamed 'copied 'untracked
;;                'conflicted 'type-changed
(struct file-item (path status-code hunks expanded? extra) #:transparent)

;; One @@ hunk. old-range/new-range are (start . count) pairs; `lines` is a list
;; of diff-line. `header` is the full @@ ... @@ line including any section
;; heading git appends after it.
(struct hunk (header old-range new-range lines) #:transparent)

;; One line of a diff body. kind: 'context 'add 'del 'header 'meta
(struct diff-line (kind text) #:transparent)

;; A commit or change. `refs` is a list of decorating ref names (branches,
;; tags, bookmarks). For jj, `id` is the change id and `short-id` the commit id.
(struct commit-record (id short-id author date subject refs) #:transparent)

;; One line of blame output. `commit` is a full commit sha (git) or change id
;; (jj), usable as a rev argument; `short-id` is its display prefix.
;; `orig-line` is the line's number in `commit`'s version of the file (an
;; integer), kept for future line-following across a chase.
(struct blame-line (commit short-id orig-line text) #:transparent)

;;; Constructors with defaults ;;;

(define (make-status header sections) (status header sections))

(define (make-section id title kind items collapsed?)
  (section id title kind items collapsed?))

;;@doc
;; A one-section list for `items`, or '() when there are none, so backends can
;; append sections and drop empty ones in one pass.
(define (maybe-section id title kind items)
  (if (null? items)
    '()
    (list (make-section id title kind items #f))))

;; extra defaults to an empty hash; callers pass flags as needed.
(define (make-file-item path status-code . opt)
  (let* ([hunks (if (>= (length opt) 1) (list-ref opt 0) #f)]
         [expanded? (if (>= (length opt) 2) (list-ref opt 1) #f)]
         [extra (if (>= (length opt) 3) (list-ref opt 2) (hash))])
    (file-item path status-code hunks expanded? extra)))

(define (make-hunk header old-range new-range lines)
  (hunk header old-range new-range lines))

(define (make-diff-line kind text) (diff-line kind text))

(define (make-commit-record id short-id author date subject refs)
  (commit-record id short-id author date subject refs))

(define (make-blame-line commit short-id orig-line text)
  (blame-line commit short-id orig-line text))

;;@doc
;; Return a copy of `fi` with its hunks (and expanded flag) replaced. Used after
;; a lazy diff fetch.
(define (file-item-with-hunks fi hunks expanded?)
  (file-item (file-item-path fi)
    (file-item-status-code fi)
    hunks
    expanded?
    (file-item-extra fi)))

(define (file-item-renamed? fi)
  (let ([e (file-item-extra fi)])
    (or (eq? (file-item-status-code fi) 'renamed)
      (eq? (file-item-status-code fi) 'copied)
      (and (hash-contains? e 'rename?) (hash-ref e 'rename?)))))

(define (file-item-binary? fi)
  (let ([e (file-item-extra fi)])
    (and (hash-contains? e 'binary?) (hash-ref e 'binary?))))

;;; Fold state ;;;
;;;
;;; A box around a hash from stable string id -> boolean. Sections store their
;;; `collapsed?`; files store their `expanded?`. Absent keys fall back to the
;;; supplied defaults so a fresh view starts with sections open and files shut.

(define (make-fold-state) (box (hash)))

(define (section-fold-key kind) (string-append "section:" (symbol->string kind)))

(define (file-fold-key section-kind path)
  (string-append "file:" (symbol->string section-kind) ":" path))

(define (fold-get fold key default)
  (let ([h (unbox fold)])
    (if (hash-contains? h key) (hash-ref h key) default)))

(define (fold-set! fold key value)
  (set-box! fold (hash-insert (unbox fold) key value)))

;;@doc
;; #t when the section for `kind` is collapsed (default #f).
(define (fold-section-collapsed? fold kind)
  (fold-get fold (section-fold-key kind) #f))

;;@doc
;; #t when the file at `path` under `section-kind` is expanded (default #f).
(define (fold-file-expanded? fold section-kind path)
  (fold-get fold (file-fold-key section-kind path) #f))

(define (set-fold-section-collapsed! fold kind collapsed?)
  (fold-set! fold (section-fold-key kind) collapsed?))

(define (set-fold-file-expanded! fold section-kind path expanded?)
  (fold-set! fold (file-fold-key section-kind path) expanded?))

(define (toggle-fold-section! fold kind)
  (set-fold-section-collapsed! fold kind (not (fold-section-collapsed? fold kind))))

(define (toggle-fold-file! fold section-kind path)
  (set-fold-file-expanded! fold section-kind path
    (not (fold-file-expanded? fold section-kind path))))

;;@doc
;; Return `status` with each section's `collapsed?` and each file-item's
;; `expanded?` overwritten from the fold map, so freshly-fetched data adopts the
;; user's existing fold choices.
(define (apply-fold-state fold st)
  (status (status-header st)
    (map (lambda (sec) (apply-fold-section fold sec)) (status-sections st))))

(define (apply-fold-section fold sec)
  (let ([kind (section-kind sec)])
    (section (section-id sec)
      (section-title sec)
      kind
      (map (lambda (item)
            (if (file-item? item)
              (file-item (file-item-path item)
                (file-item-status-code item)
                (file-item-hunks item)
                (fold-file-expanded? fold kind (file-item-path item))
                (file-item-extra item))
              item))
        (section-items sec))
      (fold-section-collapsed? fold kind))))

;;@doc
;; First section whose kind is `kind`, or #f.
(define (section-by-kind st kind)
  (let loop ([secs (status-sections st)])
    (cond
      [(null? secs) #f]
      [(eq? (section-kind (car secs)) kind) (car secs)]
      [else (loop (cdr secs))])))

;;@doc
;; Total count of file-items across all sections.
(define (status-file-count st)
  (foldl (lambda (sec acc)
          (+ acc (length (filter file-item? (section-items sec)))))
    0
    (status-sections st)))
