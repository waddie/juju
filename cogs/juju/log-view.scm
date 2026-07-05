;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; log-view.scm - the interactive log component
;;;
;;; The log as a place to act, not just look: every row carries its
;;; commit-record, so the commit actions the status view offers on commit rows
;;; work here too (show, edit, new/switch, revert, cherry-pick, rebase onto).
;;; Built on the shared overlay-view shell (ui-utils.hx). It holds the backend
;;; struct and gates every action through `backend-supports?`; the one
;;; juju-cross-cutting effect (refreshing an open status view after a mutation)
;;; comes in as the `on-change` thunk. Scrolling past the last loaded row grows
;;; the log, wired through the shell's movement hooks.

(require-builtin helix/components)
(require "helix/misc.scm")
(require "backend-interface.scm")
(require "model.scm")
(require "view-rows.scm")
(require "render.scm") ; juju-tag->style
(require "ui-utils.hx/keys.scm")
(require "ui-utils.hx/overlay-view.scm")
(require "text-view.scm")

(provide open-log-view)

;; commits: the commit-record list currently shown, refreshed after a mutation.
;; opts: the backend-log opts hash the view was opened with; `load-more!` grows
;;   its `'limit` (and sets `'full`) so scrolling past the end reveals older
;;   history. Reused on reload.
;; exhausted: #t once a load-more returned no new rows, so we stop trying.
;; on-change: thunk run after any mutation so an open status view stays current.
(struct lv-state (backend opts commits cursor top message message-tag exhausted on-change)
  #:mutable
  #:transparent)

(define LEGEND "j/k move  Enter show  e edit  n new  g refresh  ? keys  q quit")

(define (lv-status state)
  (let ([m (lv-state-message state)])
    (cons (if (string=? m "") LEGEND m) (lv-state-message-tag state))))

(define (lv-title state)
  (string-append " juju log ("
    (symbol->string (backend-name (lv-state-backend state)))
    ") "))

;;; State edits ;;;

(define (set-message! state text tag)
  (set-lv-state-message! state text)
  (set-lv-state-message-tag! state tag))

(define (clear-message! state) (set-message! state "" 'info))

(define (result-tag r) (if (result-ok? r) 'info 'error))

(define (commit-count state) (length (lv-state-commits state)))

(define (move-cursor! state delta)
  (let ([n (commit-count state)] [c (+ (lv-state-cursor state) delta)])
    (when (> n 0)
      (set-lv-state-cursor! state (max 0 (min c (- n 1)))))))

(define (cursor-to! state idx)
  (let ([n (commit-count state)])
    (when (> n 0)
      (set-lv-state-cursor! state (max 0 (min idx (- n 1)))))))

(define (cursor-commit state)
  (let ([commits (lv-state-commits state)])
    (if (null? commits) #f (list-ref commits (lv-state-cursor state)))))

(define (at-last? state)
  (= (lv-state-cursor state) (- (commit-count state) 1)))

;; Fetch the next page when moving down off the last loaded row (and history is
;; not yet exhausted). Called before the move; `load-more!` keeps the cursor on
;; the same commit, so the following move advances into the freshly loaded rows.
(define (maybe-load-more! state)
  (when (and (> (commit-count state) 0)
         (at-last? state)
         (not (lv-state-exhausted state)))
    (load-more! state)))

;; Re-fetch the log with the current opts, keeping the cursor on the same change
;; when it is still listed (a mutation can move or drop it). Clears `exhausted`:
;; a refresh or mutation may have grown history, so load-more is worth retrying.
(define (reload! state)
  (let* ([prev (cursor-commit state)]
         [commits (backend-log (lv-state-backend state) #f (lv-state-opts state))])
    (set-lv-state-commits! state commits)
    (set-lv-state-cursor! state (restore-cursor commits prev (lv-state-cursor state)))
    (set-lv-state-exhausted! state #f)))

;; Grow the log by one page: bump the opts `'limit` and set `'full` (jj then
;; escapes its curated revset to the whole `::@` ancestry; git already lists
;; HEAD ancestry). The page size is seeded as `'page` at open, falling back to
;; the current limit.
(define (grow-opts opts)
  (let* ([limit (if (hash-contains? opts 'limit) (hash-ref opts 'limit) 50)]
         [page (if (hash-contains? opts 'page) (hash-ref opts 'page) limit)])
    (hash-insert (hash-insert opts 'limit (+ limit page)) 'full #t)))

;; Fetch the next page. When the returned count is unchanged there is no more
;; history: mark `exhausted` and say so. The curated->`::@` jj transition can
;; return fewer rows (sibling heads drop out); that is not exhaustion, and a
;; dropped-sibling cursor falls back via `restore-cursor`.
(define (load-more! state)
  (let* ([prev (cursor-commit state)]
         [before (commit-count state)]
         [opts (grow-opts (lv-state-opts state))]
         [commits (backend-log (lv-state-backend state) #f opts)])
    (set-lv-state-opts! state opts)
    (set-lv-state-commits! state commits)
    (set-lv-state-cursor! state (restore-cursor commits prev (lv-state-cursor state)))
    (if (= (length commits) before)
      (begin
        (set-lv-state-exhausted! state #t)
        (set-message! state "juju: end of history" 'info))
      (clear-message! state))))

;; Index of `prev`'s change in `commits` (matched by id), else `fallback`
;; clamped into range.
(define (restore-cursor commits prev fallback)
  (let ([n (length commits)])
    (if (= n 0)
      0
      (let loop ([cs commits] [i 0])
        (cond
          [(null? cs) (max 0 (min fallback (- n 1)))]
          [(and prev (string=? (commit-record-id (car cs)) (commit-record-id prev))) i]
          [else (loop (cdr cs) (+ i 1))])))))

;;; Commit actions ;;;

;; The standard mutation epilogue: refresh the rows, report `r` on the status
;; line (reload! first, so the message survives it), then let juju refresh any
;; open status view.
(define (finish! state r)
  (reload! state)
  (set-message! state (result-message r) (result-tag r))
  ((lv-state-on-change state)))

;; Check `cap`, then run `op-fn` (backend, rev-string -> result) on the change
;; under the cursor. `noun` completes "no commit to <noun>" on an empty log.
(define (do-rev-op! state cap noun op-fn)
  (let ([backend (lv-state-backend state)])
    (cond
      [(not (backend-supports? backend cap))
        (set-message! state (unsupported-message backend cap) 'error)]
      [else
        (let ([c (cursor-commit state)])
          (if (not c)
            (set-message! state (string-append "juju: no commit to " noun) 'error)
            (finish! state (op-fn backend (commit-record-id c)))))])))

(define (do-edit! state)
  (do-rev-op! state 'edit "edit" (lambda (b rev) (backend-edit b rev))))

(define (do-new! state)
  (do-rev-op! state 'switch "start a change on"
    (lambda (b rev) (backend-switch b rev))))

(define (do-revert! state)
  (do-rev-op! state 'revert "revert"
    (lambda (b rev) (backend-revert b rev (hash)))))

(define (do-cherry-pick! state)
  (do-rev-op! state 'cherry-pick "cherry-pick"
    (lambda (b rev) (backend-cherry-pick b rev (hash)))))

(define (do-rebase-onto! state)
  (do-rev-op! state 'rebase "rebase onto"
    (lambda (b rev) (backend-rebase b (hash 'onto rev)))))

;; Enter: the commit's diff in a text view stacked above the log.
(define (show-at-cursor! state)
  (clear-message! state)
  (let ([c (cursor-commit state)])
    (when c
      (let ([shown (backend-show (lv-state-backend state) (commit-record-id c))])
        (show-text-view
          (string-append " " (commit-record-short-id c) "  " (commit-record-subject c) " ")
          (commit-show-lines shown))))))

;; A one-screen key reference, shown on `?`.
(define (help-lines)
  (list
    "juju log - keys"
    ""
    "Movement:  j/k up/down   C-u/C-d page   Home/End ends"
    "           at the bottom, j/C-d/End load older history"
    "           (jj expands to the full ::@ ancestry)"
    ""
    "On the change under the cursor:"
    "           Enter show   e edit, make it the working copy (jj)"
    "           n / b new change on it (jj new / git checkout)"
    "           V revert   y cherry-pick   r rebase onto"
    ""
    "Other:     g refresh   q / Esc close"))

;;; Movement (with pagination) and action keys ;;;

;; Moving down (or paging down) off the last row grows the log first.
(define (lv-move! state delta)
  (when (> delta 0) (maybe-load-more! state))
  (move-cursor! state delta)
  #t)

(define (lv-edge! state which)
  (when (eq? which 'bottom) (maybe-load-more! state))
  (cursor-to! state (if (eq? which 'top) 0 (commit-count state)))
  #t)

(define (lv-keys state-box event)
  (let ([state (unbox state-box)])
    (cond
      [(key-event-enter? event) (show-at-cursor! state) event-result/consume]
      [(char-is? event #\e) (do-edit! state) event-result/consume]
      [(char-is? event #\n) (do-new! state) event-result/consume]
      [(char-is? event #\b) (do-new! state) event-result/consume]
      [(char-is? event #\V) (do-revert! state) event-result/consume]
      [(char-is? event #\y) (do-cherry-pick! state) event-result/consume]
      [(char-is? event #\r) (do-rebase-onto! state) event-result/consume]
      [(char-is? event #\g) (clear-message! state) (reload! state) event-result/consume]
      [(char-is? event #\?) (show-text-view "juju log keys" (help-lines)) event-result/consume]
      [else #f])))

(define log-view-spec
  (make-overlay-view
    #:name
    "juju-log-view"
    #:title
    lv-title
    #:rows
    (lambda (state) (log-rows (lv-state-commits state)))
    #:cursor
    lv-state-cursor
    #:set-cursor!
    set-lv-state-cursor!
    #:top
    lv-state-top
    #:set-top!
    set-lv-state-top!
    #:status
    lv-status
    #:on-key
    lv-keys
    #:move!
    lv-move!
    #:edge!
    lv-edge!
    #:tag->style
    juju-tag->style
    #:overlay-scale
    (lambda () (juju-overlay-scale))))

;;@doc
;; Open the interactive log view on `backend` with `opts` (a backend-log opts
;; hash, reused on refresh). `on-change` runs after every mutation so the caller
;; can refresh an open status view. Echoes instead when the log is empty.
(define (open-log-view backend opts on-change)
  (let ([commits (backend-log backend #f opts)])
    (if (null? commits)
      (set-status! "juju: nothing to show (log)")
      (open-overlay-view! log-view-spec
        (lv-state backend opts commits 0 0 "" 'info #f on-change)))))
