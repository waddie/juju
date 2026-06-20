;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; diff.scm - one unified-diff parser for both backends
;;;
;;; Git and jj both emit Git-format unified diffs (`git diff`, `jj diff --git`),
;;; so a single parser serves both. It splits a file's diff into hunks, tagging
;;; each body line by its leading character, and reads the metadata lines
;;; (binary marker, rename headers) the renderer labels and partial-hunk staging
;;; must refuse. Pure functions: text in, structs out.

(require "string-utils.scm")
(require "model.scm")

(provide parse-unified-diff
  parse-diff-flags
  parse-hunk-header
  diff-header-lines
  build-apply-patch)

;;@doc
;; Parse the body of one file's unified diff into a list of `hunk` structs.
;; Lines before the first `@@` (the `diff --git`/`index`/`---`/`+++` headers)
;; are ignored here; `parse-diff-flags` reads them separately.
(define (parse-unified-diff text)
  (let loop ([lines (split-lines text)]
             [current #f] ; (cons header-info reversed-body-lines) or #f
             [acc '()])
    (cond
      [(null? lines)
        (reverse (if current (cons (finish-hunk current) acc) acc))]
      [(hunk-header-line? (car lines))
        (let ([acc2 (if current (cons (finish-hunk current) acc) acc)])
          (loop (cdr lines)
            (cons (parse-hunk-header (car lines)) '())
            acc2))]
      [current
        ;; Inside a hunk: classify the body line, unless it is the start of the
        ;; next file's headers (defensive, for multi-file input).
        (if (file-header-line? (car lines))
          (loop (cdr lines) #f (cons (finish-hunk current) acc))
          (loop (cdr lines)
            (cons (car current) (cons (classify-line (car lines)) (cdr current)))
            acc))]
      [else
        ;; Before the first hunk: skip file-header lines.
        (loop (cdr lines) #f acc)])))

;; current is (cons header-info reversed-lines); build the hunk struct.
(define (finish-hunk current)
  (let ([info (car current)]
        [lines (reverse (cdr current))])
    (make-hunk (hash-ref info 'header)
      (hash-ref info 'old-range)
      (hash-ref info 'new-range)
      lines)))

(define (hunk-header-line? line)
  (string-prefix? "@@ " line))

;; Lines that begin a new file's header block within a diff stream.
(define (file-header-line? line)
  (string-prefix? "diff --git " line))

(define (classify-line line)
  (if (= (string-length line) 0)
    (make-diff-line 'context "")
    (let ([c (string-ref line 0)])
      (cond
        [(char=? c #\+) (make-diff-line 'add (string-drop line 1))]
        [(char=? c #\-) (make-diff-line 'del (string-drop line 1))]
        [(char=? c #\space) (make-diff-line 'context (string-drop line 1))]
        [(char=? c #\\) (make-diff-line 'meta line)] ; "\ No newline at end of file"
        [else (make-diff-line 'context line)]))))

;;@doc
;; Parse one `@@ -a,b +c,d @@ heading` line into a hash with keys 'header (the
;; full line), 'old-range and 'new-range (each a (start . count) pair). A
;; missing count defaults to 1 (git omits `,1`).
(define (parse-hunk-header line)
  (let* ([toks (split-many line "@@")]
         ;; toks: ("" " -a,b +c,d " " heading") ; ranges are in toks[1]
         [ranges (if (>= (length toks) 2) (string-trim (list-ref toks 1)) "")]
         [parts (split-many ranges " ")]
         [old-tok (find-token parts "-")]
         [new-tok (find-token parts "+")])
    (hash 'header line
      'old-range
      (parse-range old-tok)
      'new-range
      (parse-range new-tok))))

;; First token in `parts` that starts with `sign` ("-" or "+"), or #f.
(define (find-token parts sign)
  (let loop ([ps parts])
    (cond
      [(null? ps) #f]
      [(and (> (string-length (car ps)) 0) (string-prefix? sign (car ps))) (car ps)]
      [else (loop (cdr ps))])))

;; "-12,3" / "+5" -> (start . count); #f -> (0 . 0).
(define (parse-range tok)
  (if (not tok)
    (cons 0 0)
    (let* ([body (string-drop tok 1)] ; drop leading - or +
           [nums (split-many body ",")]
           [start (string->number (list-ref nums 0))]
           [count (if (>= (length nums) 2) (string->number (list-ref nums 1)) 1)])
      (cons (if start start 0) (if count count 1)))))

;;; Patch construction (partial-hunk staging) ;;;
;;;
;;; To stage/unstage/discard part of a file, juju re-fetches the file's diff,
;;; keeps the header block, and rebuilds the hunks with only the selected change
;;; lines. The result is fed to `git apply` (with `--recount`, so the @@ counts
;;; need not be exact). The line-selection rule depends on the apply direction,
;;; because the patch's context must match the content git applies against:
;;;
;;;   'forward (git apply --cached, for staging):
;;;     - selected + or - kept as-is;
;;;     - unselected + dropped (not part of this patch);
;;;     - unselected - becomes context (the deletion is not applied).
;;;   'reverse (git apply --reverse, for unstaging/discarding):
;;;     - selected + or - kept as-is (git negates them on apply);
;;;     - unselected - dropped;
;;;     - unselected + becomes context (it is present in the current content the
;;;       reverse patch is checked against).
;;;
;;; Context lines are always kept. Validated against `git apply --cached
;;; --recount` (forward) and `git apply --cached --reverse --recount` (reverse).

;;@doc
;; The header lines of a single file's unified diff: everything before the first
;; `@@` hunk (the `diff --git`, `index`, `---`, `+++`, and any rename/mode
;; lines). Returned as a list of strings, in order. These are required verbatim
;; at the top of a constructed patch.
(define (diff-header-lines text)
  (let loop ([lines (split-lines text)] [acc '()])
    (cond
      [(null? lines) (reverse acc)]
      [(hunk-header-line? (car lines)) (reverse acc)]
      [else (loop (cdr lines) (cons (car lines) acc))])))

;;@doc
;; Build a patch string from `header-lines` (see `diff-header-lines`) and
;; `hunks` (a list of `hunk` structs), including only the change lines for which
;; `(include? hunk-index body-line-index)` is true. Hunks with no included
;; change line are dropped. Returns the patch string (newline-terminated), or #f
;; when nothing is selected. `body-line-index` is the 0-based position of a line
;; within `hunk-lines`. Optional `mode` is 'forward (default) or 'reverse and
;; selects the line-selection rule (see the section comment above). Pure.
(define (build-apply-patch header-lines hunks include? . opt)
  (let* ([mode (if (pair? opt) (car opt) 'forward)]
         [built (let loop ([hs hunks] [i 0] [acc '()])
                 (if (null? hs)
                   (reverse acc)
                   (let ([h (build-hunk-text (car hs) i include? mode)])
                     (loop (cdr hs) (+ i 1) (if h (cons h acc) acc)))))])
    (if (null? built)
      #f
      (string-append
        (lines->text header-lines)
        (apply string-append built)))))

;; Build the text of one filtered hunk, or #f if it has no included change line.
(define (build-hunk-text h hunk-index include? mode)
  (let* ([lines (hunk-lines h)]
         [emitted (emit-hunk-lines lines hunk-index include? mode)]
         [body (car emitted)]
         [old-count (cadr emitted)]
         [new-count (caddr emitted)]
         [any-change? (cadddr emitted)])
    (if (not any-change?)
      #f
      (let ([old-start (car (hunk-old-range h))]
            [new-start (car (hunk-new-range h))])
        (string-append
          "@@ -"
          (number->string old-start)
          ","
          (number->string old-count)
          " +"
          (number->string new-start)
          ","
          (number->string new-count)
          " @@\n"
          (apply string-append body))))))

;; Walk a hunk's body lines, deciding what to emit per the selection rule for
;; `mode`, and tally the resulting old/new line counts. Returns
;;   (list emitted-lines old-count new-count any-change?)
;; where emitted-lines is in order. In 'forward mode an unselected + is dropped
;; and an unselected - becomes context; in 'reverse mode an unselected - is
;; dropped and an unselected + becomes context.
(define (emit-hunk-lines lines hunk-index include? mode)
  (let ([reverse? (eq? mode 'reverse)])
    (let loop ([ls lines] [j 0] [emitted '()] [old 0] [new 0] [changed? #f])
      (if (null? ls)
        (list (reverse emitted) old new changed?)
        (let* ([dl (car ls)]
               [k (diff-line-kind dl)]
               [text (diff-line-text dl)]
               [sel? (include? hunk-index j)])
          (cond
            [(eq? k 'context)
              (loop (cdr ls) (+ j 1)
                (cons (string-append " " text "\n") emitted)
                (+ old 1)
                (+ new 1)
                changed?)]
            [(eq? k 'add)
              (cond
                [sel?
                  (loop (cdr ls) (+ j 1)
                    (cons (string-append "+" text "\n") emitted)
                    old
                    (+ new 1)
                    #t)]
                [reverse?
                  ;; unselected addition is present in current content: context
                  (loop (cdr ls) (+ j 1)
                    (cons (string-append " " text "\n") emitted)
                    (+ old 1)
                    (+ new 1)
                    changed?)]
                [else (loop (cdr ls) (+ j 1) emitted old new changed?)])]
            [(eq? k 'del)
              (cond
                [sel?
                  (loop (cdr ls) (+ j 1)
                    (cons (string-append "-" text "\n") emitted)
                    (+ old 1)
                    new
                    #t)]
                [reverse?
                  ;; unselected deletion is absent from current content: drop it
                  (loop (cdr ls) (+ j 1) emitted old new changed?)]
                [else
                  ;; forward: unselected deletion becomes context
                  (loop (cdr ls) (+ j 1)
                    (cons (string-append " " text "\n") emitted)
                    (+ old 1)
                    (+ new 1)
                    changed?)])]
            ;; meta ("\ No newline at end of file"): emit verbatim, no count
            [(eq? k 'meta)
              (loop (cdr ls) (+ j 1) (cons (string-append text "\n") emitted) old new changed?)]
            [else (loop (cdr ls) (+ j 1) emitted old new changed?)]))))))

(define (lines->text lines)
  (apply string-append (map (lambda (l) (string-append l "\n")) lines)))

;;@doc
;; Read the metadata at the head of a file's diff. Returns a hash:
;;   'binary?   #t when git reported "Binary files ... differ"
;;   'rename?   #t when the diff carries rename headers
;;   'orig-path source path of a rename/copy, or #f
;;   'new-file? / 'deleted? mode-change markers
;; Partial-hunk staging consults these to refuse cases it cannot patch safely.
(define (parse-diff-flags text)
  (let loop ([lines (split-lines text)]
             [flags (hash 'binary? #f
                     'rename?
                     #f
                     'orig-path
                     #f
                     'new-file?
                     #f
                     'deleted?
                     #f)])
    (cond
      [(null? lines) flags]
      ;; Stop scanning at the first hunk; metadata is all above it.
      [(hunk-header-line? (car lines)) flags]
      [else
        (let ([line (car lines)])
          (loop (cdr lines)
            (cond
              [(string-prefix? "Binary files " line) (hash-insert flags 'binary? #t)]
              [(string-prefix? "GIT binary patch" line) (hash-insert flags 'binary? #t)]
              [(string-prefix? "rename from " line)
                (hash-insert (hash-insert flags 'rename? #t)
                  'orig-path
                  (string-trim (string-drop line (string-length "rename from "))))]
              [(string-prefix? "copy from " line)
                (hash-insert (hash-insert flags 'rename? #t)
                  'orig-path
                  (string-trim (string-drop line (string-length "copy from "))))]
              [(string-prefix? "new file mode" line) (hash-insert flags 'new-file? #t)]
              [(string-prefix? "deleted file mode" line) (hash-insert flags 'deleted? #t)]
              [else flags])))])))
