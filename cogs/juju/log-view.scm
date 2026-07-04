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
;;; The same shell as text-view (read-only, owns its keys, never touches a
;;; buffer). It holds the backend struct and gates every action through
;;; `backend-supports?`; the one juju-cross-cutting effect (refreshing an open
;;; status view after a mutation) comes in as the `on-change` thunk.

(require-builtin helix/components)
(require "helix/misc.scm")
(require "backend-interface.scm")
(require "model.scm")
(require "view-rows.scm")
(require "render.scm")
(require "scroll.scm")
(require "keys.scm")
(require "text-view.scm")

(provide open-log-view)

(define COMPONENT-NAME "juju-log-view")

;; commits: the commit-record list currently shown, refreshed after a mutation.
;; opts: the backend-log opts hash the view was opened with; `load-more!` grows
;;   its `'limit` (and sets `'full`) so scrolling past the end reveals older
;;   history. Reused on reload.
;; exhausted: #t once a load-more returned no new rows, so we stop trying.
;; on-change: thunk run after any mutation so an open status view stays current.
(struct lv-state (backend opts commits cursor top message message-tag exhausted on-change)
  #:mutable
  #:transparent)

;;; Rendering ;;;

(define (render-lv state-box rect buffer)
  (let* ([state (unbox state-box)]
         [content (draw-frame buffer (overlay-area rect) (lv-title state))]
         [rows (log-rows (lv-state-commits state))]
         [height (visible-row-count content)]
         [top (clamp-top (lv-state-top state) (lv-state-cursor state) height (length rows))])
    (set-lv-state-top! state top)
    (draw-rows buffer content rows (lv-state-cursor state) top)
    (draw-status-line buffer content (status-text state) (lv-state-message-tag state))))

(define LEGEND "j/k move  Enter show  e edit  n new  g refresh  ? keys  q quit")

(define (status-text state)
  (let ([m (lv-state-message state)])
    (if (string=? m "") LEGEND m)))

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
(define (maybe-load-more! state-box)
  (let ([state (unbox state-box)])
    (when (and (> (commit-count state) 0)
           (at-last? state)
           (not (lv-state-exhausted state)))
      (load-more! state-box))))

;; Re-fetch the log with the current opts, keeping the cursor on the same change
;; when it is still listed (a mutation can move or drop it). Clears `exhausted`:
;; a refresh or mutation may have grown history, so load-more is worth retrying.
(define (reload! state-box)
  (let* ([state (unbox state-box)]
         [prev (cursor-commit state)]
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
(define (load-more! state-box)
  (let* ([state (unbox state-box)]
         [prev (cursor-commit state)]
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
(define (finish! state-box r)
  (reload! state-box)
  (let ([state (unbox state-box)])
    (set-message! state (result-message r) (result-tag r))
    ((lv-state-on-change state))))

;; Check `cap`, then run `op-fn` (backend, rev-string -> result) on the change
;; under the cursor. `noun` completes "no commit to <noun>" on an empty log.
(define (do-rev-op! state-box cap noun op-fn)
  (let* ([state (unbox state-box)]
         [backend (lv-state-backend state)])
    (cond
      [(not (backend-supports? backend cap))
        (set-message! state (unsupported-message backend cap) 'error)]
      [else
        (let ([c (cursor-commit state)])
          (if (not c)
            (set-message! state (string-append "juju: no commit to " noun) 'error)
            (finish! state-box (op-fn backend (commit-record-id c)))))])))

(define (do-edit! state-box)
  (do-rev-op! state-box 'edit "edit" (lambda (b rev) (backend-edit b rev))))

(define (do-new! state-box)
  (do-rev-op! state-box 'switch "start a change on"
    (lambda (b rev) (backend-switch b rev))))

(define (do-revert! state-box)
  (do-rev-op! state-box 'revert "revert"
    (lambda (b rev) (backend-revert b rev (hash)))))

(define (do-cherry-pick! state-box)
  (do-rev-op! state-box 'cherry-pick "cherry-pick"
    (lambda (b rev) (backend-cherry-pick b rev (hash)))))

(define (do-rebase-onto! state-box)
  (do-rev-op! state-box 'rebase "rebase onto"
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

;;; Event handling ;;;

(define (handle-lv state-box event)
  (let ([state (unbox state-box)])
    (cond
      [(key-event-escape? event) (close-lv) event-result/close]
      [(char-is? event #\q) (close-lv) event-result/close]

      [(or (key-event-up? event) (char-is? event #\k)) (move-cursor! state -1) event-result/consume]
      [(or (key-event-down? event) (char-is? event #\j))
        (maybe-load-more! state-box)
        (move-cursor! state 1)
        event-result/consume]
      [(ctrl-char? event #\u) (move-cursor! state -10) event-result/consume]
      [(ctrl-char? event #\d)
        (maybe-load-more! state-box)
        (move-cursor! state 10)
        event-result/consume]
      [(key-event-home? event) (cursor-to! state 0) event-result/consume]
      [(key-event-end? event)
        (maybe-load-more! state-box)
        (cursor-to! state (commit-count state))
        event-result/consume]

      [(key-event-enter? event) (show-at-cursor! state) event-result/consume]
      [(char-is? event #\e) (do-edit! state-box) event-result/consume]
      [(char-is? event #\n) (do-new! state-box) event-result/consume]
      [(char-is? event #\b) (do-new! state-box) event-result/consume]
      [(char-is? event #\V) (do-revert! state-box) event-result/consume]
      [(char-is? event #\y) (do-cherry-pick! state-box) event-result/consume]
      [(char-is? event #\r) (do-rebase-onto! state-box) event-result/consume]

      [(char-is? event #\g)
        (clear-message! state)
        (reload! state-box)
        event-result/consume]
      [(char-is? event #\?) (show-text-view "juju log keys" (help-lines)) event-result/consume]

      [else event-result/consume])))

(define (close-lv) (pop-last-component-by-name! COMPONENT-NAME))

;;@doc
;; Open the interactive log view on `backend` with `opts` (a backend-log opts
;; hash, reused on refresh). `on-change` runs after every mutation so the caller
;; can refresh an open status view. Echoes instead when the log is empty.
(define (open-log-view backend opts on-change)
  (let ([commits (backend-log backend #f opts)])
    (if (null? commits)
      (set-status! "juju: nothing to show (log)")
      (let* ([state-box (box (lv-state backend opts commits 0 0 "" 'info #f on-change))]
             [handlers (hash "handle_event" handle-lv
                        "cursor"
                        (lambda (state-box rect) #f)
                        "required_size"
                        (lambda (state-box size) size))]
             [component (new-component! COMPONENT-NAME state-box render-lv handlers)])
        (push-component! component)))))
