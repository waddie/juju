;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; status-view.scm - the read-only status component
;;;
;;; A helix/components component, built like the nrepl.hx lookup-picker:
;;; new-component! with a state box, a render closure, and an event handler. The
;;; view owns its own keys and never touches an editor buffer, so the read-only
;;; sections cannot be edited and fold state persists across refreshes. Diffs
;;; are fetched lazily when a file is expanded and cached in the view state.

(require-builtin helix/components)
(require (prefix-in helix. "helix/commands.scm"))
(require "helix/misc.scm")

(require "backend-interface.scm")
(require "backend-detect.scm")
(require "config.scm")
(require "model.scm")
(require "view-rows.scm")
(require "operand.scm")
(require "text-view.scm")
(require "rebase-todo.scm")
(require "rebase-view.scm")
(require "render.scm") ; juju-tag->style
(require "ui-utils.hx/keys.scm")
(require "string-utils.scm")
(require "ui-utils.hx/strings.scm")
(require "ui-utils.hx/overlay-view.scm")

(provide open-status-view
  refresh-open-view!)

(define COMPONENT-NAME "juju-status")

;; The state-box of the currently-open status view, or #f when none is open. A
;; typed command (commands.scm) has no handle to the live component, so it reads
;; this to refresh the view after a mutation. Set on open, cleared on close.
(define *open-view* (box #f))

;; message/message-tag drive the bottom status line. cursor/top index the row
;; list; selection is a list of marked row indices (the multi-select operand).
;; When selection is empty an action falls back to the row under the cursor.
;; search-query/search-matches/search-pos hold the in-buffer search: the last
;; query, the matching row indices, and the position within that match list
;; (-1 when no search is active). Cleared on reload.
(struct view-state
  (backend status fold diff-cache cursor top message message-tag selection
    search-query
    search-matches
    search-pos)
  #:mutable
  #:transparent)

;;; Construction and data flow ;;;

;; Build a fresh view-state for `backend`, fetching its status. Fold state is
;; supplied so a refresh keeps the user's collapse/expand choices.
(define (make-view-state backend fold)
  (let* ([raw (backend-status backend)]
         [st (apply-fold-state fold raw)])
    (view-state backend st fold (hash) 0 0 "" 'info '() "" '() -1)))

;; Re-fetch status, drop cached diffs (content may have changed), keep folds.
(define (reload! state-box)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)]
         [fold (view-state-fold state)]
         [raw (backend-status backend)]
         [st (apply-fold-state fold raw)])
    (set-view-state-status! state st)
    (set-view-state-diff-cache! state (hash))
    (set-view-state-selection! state '())
    (clear-search! state)
    (clamp-cursor! state)
    (set-view-state-message! state "Refreshed")
    (set-view-state-message-tag! state 'info)))

;; Ensure the diff for an expanded file is in the cache, fetching if absent.
(define (ensure-diff! state section-kind path)
  (let* ([cache (view-state-diff-cache state)]
         [key (diff-cache-key section-kind path)])
    (unless (hash-contains? cache key)
      (let ([hunks (backend-diff (view-state-backend state)
                    (hash 'type 'file 'section section-kind 'path path))])
        (set-view-state-diff-cache! state (hash-insert cache key hunks))))))

(define (current-rows state)
  (build-rows (view-state-status state)
    (view-state-fold state)
    (view-state-diff-cache state)))

;;; Cursor movement ;;;

(define (clamp-cursor! state)
  (let* ([rows (current-rows state)]
         [n (length rows)]
         [c (view-state-cursor state)])
    (set-view-state-cursor! state (max 0 (min c (max 0 (- n 1)))))))

;; Move the cursor by `delta`, skipping non-selectable rows (blanks, headers).
(define (move-cursor! state delta)
  (let* ([rows (current-rows state)]
         [n (length rows)])
    (when (> n 0)
      ;; Each branch returns a real value (the unused cursor index), never bare
      ;; (void): Steel miscompiles a loop whose every tail branch evaluates to
      ;; void, treating the (void) literal as an application (#<void> applied).
      (let loop ([c (view-state-cursor state)] [steps n])
        (let ([next (+ c delta)])
          (cond
            ;; Ran out of rows to scan, or stepping off either end: stay put.
            [(<= steps 0) c]
            [(or (< next 0) (>= next n)) c]
            [(row-selectable? (list-ref rows next))
              (set-view-state-cursor! state next)
              next]
            [else (loop next (- steps 1))]))))))

;; Page the cursor by `delta` rows, snapping to the nearest selectable row in
;; the direction of travel (falling back the other way), so an overshoot lands
;; on the last selectable row instead of stranding the cursor.
(define (page-cursor! state delta)
  (let* ([rows (current-rows state)]
         [n (length rows)])
    (when (> n 0)
      (let* ([target (max 0 (min (+ (view-state-cursor state) delta) (- n 1)))]
             [idx (nearest-selectable-index rows target (if (< delta 0) -1 1))])
        (when idx (set-view-state-cursor! state idx))))))

(define (cursor-to-edge! state which)
  (let* ([rows (current-rows state)]
         [n (length rows)])
    (when (> n 0)
      (if (eq? which 'top)
        (begin (set-view-state-cursor! state 0) (move-cursor! state 1)
          (when (and (> n 0) (row-selectable? (list-ref rows 0)))
            (set-view-state-cursor! state 0)))
        (begin (set-view-state-cursor! state (- n 1)) (move-cursor! state -1)
          (when (row-selectable? (list-ref rows (- n 1)))
            (set-view-state-cursor! state (- n 1))))))))

(define (current-row state)
  (let ([rows (current-rows state)]
        [c (view-state-cursor state)])
    (if (and (>= c 0) (< c (length rows))) (list-ref rows c) #f)))

;;; Section navigation ;;;
;;;
;;; Jump the cursor between section headers (siblings) or up to the enclosing
;;; section (parent), the Helix-native answer to Magit's M-n/M-p/^ grammar.

(define (cursor-to-section! state which)
  (let* ([rows (current-rows state)]
         [from (view-state-cursor state)]
         [target (cond
                  [(eq? which 'next) (next-section-index rows from)]
                  [(eq? which 'prev) (prev-section-index rows from)]
                  [else (parent-section-index rows from)])])
    (set-view-state-cursor! state target)))

;;; In-buffer search ;;;
;;;
;;; `/` prompts for a query; matching row indices are stored and the cursor jumps
;;; to the first match at or after it. n/N cycle the matches (wrapping). Search
;;; state is cleared on reload so stale indices never drive the cursor.

(define (clear-search! state)
  (set-view-state-search-query! state "")
  (set-view-state-search-matches! state '())
  (set-view-state-search-pos! state -1))

(define (start-search! state-box)
  (push-component!
    (prompt "Search: "
      (lambda (input) (apply-search! (unbox state-box) (or input ""))))))

(define (apply-search! state query)
  (let* ([rows (current-rows state)]
         [matches (search-matches rows query)])
    (set-view-state-search-query! state query)
    (set-view-state-search-matches! state matches)
    (cond
      [(null? matches)
        (set-view-state-search-pos! state -1)
        (set-msg! state (string-append "No match: " query) 'error)]
      [else
        (let ([p (first-match-pos matches (view-state-cursor state))])
          (set-view-state-search-pos! state p)
          (set-view-state-cursor! state (list-ref matches p))
          (set-msg! state (search-status p (length matches)) 'info))])))

;; The position within `matches` of the first index at or after `cursor`,
;; wrapping to 0 when every match precedes it.
(define (first-match-pos matches cursor)
  (let loop ([ms matches] [p 0])
    (cond
      [(null? ms) 0]
      [(>= (car ms) cursor) p]
      [else (loop (cdr ms) (+ p 1))])))

(define (search-step! state dir)
  (let ([matches (view-state-search-matches state)])
    (if (null? matches)
      (set-msg! state "No active search (press /)" 'info)
      (let* ([n (length matches)]
             [p (modulo (+ (view-state-search-pos state) dir n) n)])
        (set-view-state-search-pos! state p)
        (set-view-state-cursor! state (list-ref matches p))
        (set-msg! state (search-status p n) 'info)))))

(define (search-status p n)
  (string-append "Match " (number->string (+ p 1)) "/" (number->string n)))

;;; Actions ;;;

;; Tab / Enter on a foldable row. Sections toggle collapse; files toggle their
;; inline diff, fetching it on first expand. Returns #t if it handled a fold.
(define (toggle-fold-at-cursor! state)
  (let ([row (current-row state)])
    (cond
      [(not row) #f]
      [(eq? (row-type row) 'section)
        (toggle-fold-section! (view-state-fold state) (row-section-kind row))
        (sync-status-folds! state)
        #t]
      [(or (eq? (row-type row) 'file) (eq? (row-type row) 'diff))
        (let* ([fi (if (eq? (row-type row) 'file) (row-object row) #f)]
               [kind (row-section-kind row)])
          (if fi
            (let ([path (file-item-path fi)])
              (let ([fold (view-state-fold state)])
                (when (not (fold-file-expanded? fold kind path))
                  (ensure-diff! state kind path))
                (toggle-fold-file! fold kind path)
                (sync-status-folds! state))
              #t)
            ;; On a diff line, Tab collapses the parent file.
            (let ([kind (row-section-kind row)])
              (collapse-file-of-diff! state kind)
              #t)))]
      [else #f])))

;; Re-apply fold state to the stored status so collapsed flags match the map.
(define (sync-status-folds! state)
  (set-view-state-status! state
    (apply-fold-state (view-state-fold state) (view-state-status state)))
  (clamp-cursor! state))

;; Collapse the file whose diff the cursor sits in, then move the cursor to it.
(define (collapse-file-of-diff! state kind)
  ;; Walk backwards from the cursor to the owning file row.
  (let* ([rows (current-rows state)]
         [c (view-state-cursor state)])
    (let loop ([i c])
      (cond
        [(< i 0) #f]
        [(eq? (row-type (list-ref rows i)) 'file)
          (let* ([fi (row-object (list-ref rows i))]
                 [path (file-item-path fi)])
            (set-fold-file-expanded! (view-state-fold state) kind path #f)
            (sync-status-folds! state)
            (set-view-state-cursor! state i))]
        [else (loop (- i 1))]))))

;; Close the overlay from within a handler (visiting a file): clear the
;; open-view handle and pop the component, matching the shell's on-close.
(define (close-view)
  (set-box! *open-view* #f)
  (pop-last-component-by-name! COMPONENT-NAME))

;; Enter: visit a file (close the view and open it), expand a commit's diff, or
;; fall back to fold toggling on sections.
(define (visit-at-cursor! state-box)
  (let* ([state (unbox state-box)]
         [row (current-row state)])
    (cond
      [(not row) (void)]
      [(eq? (row-type row) 'file)
        (let ([path (file-item-path (row-object row))]
              [root (backend-root (view-state-backend state))])
          (close-view)
          (helix.open (path-join root path)))]
      [(eq? (row-type row) 'diff)
        ;; Visiting a diff line opens the owning file at that line.
        (let ([root (backend-root (view-state-backend state))]
              [file-path (file-of-diff state)]
              [line (row-line row)])
          (when file-path
            (close-view)
            (helix.open (path-join root file-path))
            (when line (helix.goto-line line))))]
      ;; A commit row: show its diff in a text overlay.
      [(eq? (row-type row) 'commit) (show-commit-at! state-box row)]
      ;; Sections (and anything else foldable) toggle their fold.
      [else (toggle-fold-at-cursor! state)])))

(define (file-of-diff state)
  (let* ([rows (current-rows state)]
         [c (view-state-cursor state)])
    (let loop ([i c])
      (cond
        [(< i 0) #f]
        [(eq? (row-type (list-ref rows i)) 'file)
          (file-item-path (row-object (list-ref rows i)))]
        [else (loop (- i 1))]))))

;;; Selection ;;;
;;;
;;; A list of marked row indices. `v` toggles the current row in or out. Actions
;;; operate on the marked rows, or - when nothing is marked - on the row under
;;; the cursor, so the common single-target case needs no marking.

(define (toggle-mark! state)
  (let* ([rows (current-rows state)]
         [c (view-state-cursor state)])
    (when (and (>= c 0) (< c (length rows)) (row-selectable? (list-ref rows c)))
      (let ([sel (view-state-selection state)])
        (set-view-state-selection! state
          (if (member c sel)
            (filter (lambda (i) (not (= i c))) sel)
            (cons c sel)))))))

;; The row indices an action applies to: the marked set, else the cursor.
(define (action-indices state)
  (let ([sel (view-state-selection state)])
    (if (null? sel) (list (view-state-cursor state)) sel)))

;;; Mutations ;;;
;;;
;;; Every action runs synchronously through the active backend, then refreshes.
;;; Capability gating: an action whose feature the backend does not support
;;; reports so and does nothing (the same keys are simply inert under jj for the
;;; index-only operations).

(define (set-msg! state text tag)
  (set-view-state-message! state text)
  (set-view-state-message-tag! state tag))

(define (result-tag r) (if (result-ok? r) 'info 'error))

;; The standard mutation epilogue: refresh the view, then report `r` (a backend
;; result hash) on the status line. reload! mutates the view-state in place, so
;; the message is set afterwards on the same (re-read) state.
(define (reload-and-report! state-box r)
  (reload! state-box)
  (set-msg! (unbox state-box) (result-message r) (result-tag r)))

;; Run a stage/unstage/discard op over the resolved operand, then refresh.
(define (do-selection-op! state-box op cap)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (cond
      [(not (backend-supports? backend cap))
        (set-msg! state (unsupported-message backend cap) 'error)]
      [else
        (let* ([specs (resolve-operands (current-rows state) (action-indices state))])
          (if (null? specs)
            (set-msg! state "juju: nothing to act on here" 'error)
            (let ([r (backend-mutate backend op (list specs))])
              (reload-and-report! state-box r))))])))

;; Discard is destructive, so confirm first.
(define (do-discard! state-box)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (cond
      [(not (backend-supports? backend 'discard))
        (set-msg! state (unsupported-message backend 'discard) 'error)]
      [else
        (let ([specs (resolve-operands (current-rows state) (action-indices state))])
          (if (null? specs)
            (set-msg! state "juju: nothing to discard here" 'error)
            (push-component!
              (prompt
                (string-append "Discard " (number->string (length specs))
                  " item(s)? "
                  (discard-confirm-note backend)
                  " [y/N]: ")
                (lambda (input)
                  (when (confirmed? input)
                    (let ([r (backend-discard backend specs)])
                      (reload-and-report! state-box r))))))))])))

;; stage-all / unstage-all: no operand, whole worktree.
(define (do-bulk! state-box op cap)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (if (not (backend-supports? backend cap))
      (set-msg! state (unsupported-message backend cap) 'error)
      (let ([r (backend-mutate backend op '())])
        (reload-and-report! state-box r)))))

;; Commit / amend: read a message, then run. Amend with an empty message keeps
;; the existing one. The message is gathered with a single-line prompt.
(define (do-commit! state-box amend?)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)]
         [cap (if amend? 'amend 'commit)])
    (if (not (backend-supports? backend cap))
      (set-msg! state (unsupported-message backend cap) 'error)
      (push-component!
        (prompt (if amend? "Amend message (empty keeps existing): " "Commit message: ")
          (lambda (input)
            (let ([r (if amend?
                      (backend-amend backend (or input "") (hash))
                      (backend-commit backend (or input "") (hash)))])
              (reload-and-report! state-box r))))))))

;; Extend: fold staged/working changes into the latest commit, message unchanged.
(define (do-extend! state-box)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (if (not (backend-supports? backend 'amend))
      (set-msg! state (unsupported-message backend 'amend) 'error)
      (let ([r (backend-extend backend (hash))])
        (reload-and-report! state-box r)))))

;; fetch / pull / push. Synchronous: the view briefly blocks, then shows the
;; outcome. opts is empty, so the backend uses its configured default remote.
(define (do-network! state-box op cap)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (if (not (backend-supports? backend cap))
      (set-msg! state (unsupported-message backend cap) 'error)
      (let ([r (backend-mutate backend op (list (hash)))])
        (reload-and-report! state-box r)))))

;;; Commit / ref actions (selection-first over commit rows) ;;;
;;;
;;; History and branch actions read the selected commit rows (recent, unpushed/
;;; unpulled, bookmarks, stashes) via resolve-revs, the commit-row counterpart to
;;; resolve-operands. The same selection drives a file action or a commit action
;;; depending on the key pressed.

;; The commit/ref operand specs the action applies to (marked rows, else cursor).
;; Operation-log rows are display-only: an op id is not a revision (a hex op id
;; could even resolve as a commit-id prefix), so they never become operands.
(define (action-revs state)
  (filter (lambda (s) (not (eq? (hash-ref s 'kind) 'operations)))
    (resolve-revs (current-rows state) (action-indices state))))

(define (first-rev revs) (hash-ref (car revs) 'rev))

(define (stash-spec? spec) (eq? (hash-ref spec 'kind) 'stashes))

;; Showable kinds map to a real commit (so backend-show works); operations and
;; stashes do not.
(define (showable-commit-kind? kind)
  (and (member kind '(recent unpushed unpulled bookmarks)) #t))

;; Apply `op-fn` (backend, rev-string -> result) across `revs`, combining counts
;; and errors into one result so multi-commit revert/cherry-pick report once.
(define (run-over-revs backend revs op-fn)
  (let loop ([rs revs] [ok 0] [errs '()] [last #f])
    (if (null? rs)
      (if (null? errs)
        (ok-result (string-append "Applied to " (count-label ok)) last)
        (err-result (string-join (reverse errs) "; ") last))
      (let ([r (op-fn backend (hash-ref (car rs) 'rev))])
        (if (result-ok? r)
          (loop (cdr rs) (+ ok 1) errs (result-raw r))
          (loop (cdr rs) ok (cons (result-message r) errs) (result-raw r)))))))

;; Resolve the backend, check `cap`, require a commit selection, run `combine`
;; (backend, revs -> result), then refresh. `noun` completes "select a commit to
;; <noun>" when nothing commit-like is selected.
(define (do-rev-op! state-box cap noun combine)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (cond
      [(not (backend-supports? backend cap))
        (set-msg! state (unsupported-message backend cap) 'error)]
      [else
        (let ([revs (action-revs state)])
          (if (null? revs)
            (set-msg! state (string-append "juju: select a commit to " noun) 'error)
            (let ([r (combine backend revs)])
              (reload-and-report! state-box r))))])))

(define (do-revert! state-box)
  (do-rev-op! state-box 'revert "revert"
    (lambda (b revs) (run-over-revs b revs (lambda (bb rev) (backend-revert bb rev (hash)))))))

(define (do-cherry-pick! state-box)
  (do-rev-op! state-box 'cherry-pick "cherry-pick"
    (lambda (b revs) (run-over-revs b revs (lambda (bb rev) (backend-cherry-pick bb rev (hash)))))))

(define (do-rebase-onto! state-box)
  (do-rev-op! state-box 'rebase "rebase onto"
    (lambda (b revs) (backend-rebase b (hash 'onto (first-rev revs))))))

;; i opens the interactive rebase editor over the commits from the selected one
;; up to the tip. The editor floats above the status view; on confirm we apply
;; the plan and reload the status view underneath.
(define (do-rebase-interactive! state-box)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (cond
      [(not (backend-supports? backend 'rebase-interactive))
        (set-msg! state (unsupported-message backend 'rebase-interactive) 'error)]
      [else
        (let ([revs (action-revs state)])
          (if (null? revs)
            (set-msg! state "juju: select a commit to rebase from" 'error)
            (open-rebase-from state-box backend (first-rev revs))))])))

(define (open-rebase-from state-box backend rev)
  (let* ([range (backend-query backend 'rebase-range (list (hash 'from rev)))]
         [commits (hash-ref range 'commits)]
         [base (hash-ref range 'base)])
    (if (null? commits)
      (set-msg! (unbox state-box) "juju: no commits to rebase from here" 'error)
      (open-rebase-view (make-todo commits)
        (lambda (entries)
          (reload-and-report! state-box
            (backend-rebase-interactive backend (hash 'entries entries 'base base))))))))

(define (do-switch! state-box)
  (do-rev-op! state-box 'switch "switch to"
    (lambda (b revs) (backend-switch b (first-rev revs)))))

;; e is operand-typed like x: on a selected commit row under a backend with the
;; edit capability (jj) it makes that change the working copy; anywhere else it
;; extends as before. Git has no edit capability, so its behaviour is unchanged.
(define (do-edit-or-extend! state-box)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (if (and (backend-supports? backend 'edit) (not (null? (action-revs state))))
      (do-rev-op! state-box 'edit "edit"
        (lambda (b revs) (backend-edit b (first-rev revs))))
      (do-extend! state-box))))

;; B sets a bookmark/branch to the selected commit. The ref name can't come from
;; the row, so prompt for it, then set-or-move it onto the first selected rev.
(define (do-set-bookmark! state-box)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (cond
      [(not (backend-supports? backend 'branch))
        (set-msg! state (unsupported-message backend 'branch) 'error)]
      [else
        (let ([revs (action-revs state)])
          (if (null? revs)
            (set-msg! state "juju: select a commit to set a bookmark on" 'error)
            (push-component!
              (prompt "Set which branch/bookmark here: "
                (lambda (input)
                  (when (not (blank? input))
                    (let ([r (backend-branch-set backend input (first-rev revs))])
                      (reload-and-report! state-box r))))))))])))

;; x is operand-typed: discard the selected files, else drop a selected stash,
;; else abandon a selected commit. The destructive cases confirm first.
(define (do-x! state-box)
  (let* ([state (unbox state-box)]
         [specs (resolve-operands (current-rows state) (action-indices state))]
         [revs (action-revs state)])
    (cond
      [(not (null? specs)) (do-discard! state-box)]
      [(null? revs) (set-msg! state "juju: nothing to act on here" 'error)]
      [(stash-spec? (car revs)) (do-stash-drop! state-box revs)]
      [else (do-abandon! state-box revs)])))

(define (do-stash-drop! state-box revs)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (if (not (backend-supports? backend 'stash))
      (set-msg! state (unsupported-message backend 'stash) 'error)
      (push-component!
        (prompt (string-append "Drop " (count-label (length revs)) "? [y/N]: ")
          (lambda (input)
            (when (confirmed? input)
              (let ([r (run-over-revs backend revs (lambda (b rev) (backend-stash-drop b rev)))])
                (reload-and-report! state-box r)))))))))

(define (do-abandon! state-box revs)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)])
    (if (not (backend-supports? backend 'abandon))
      (set-msg! state (unsupported-message backend 'abandon) 'error)
      (push-component!
        (prompt (string-append "Abandon " (count-label (length revs)) "? (jj undo reverses it) [y/N]: ")
          (lambda (input)
            (when (confirmed? input)
              (let ([r (run-over-revs backend revs (lambda (b rev) (backend-abandon b rev (hash))))])
                (reload-and-report! state-box r)))))))))

;; p pops a stash: requires a stash row selected (popping the wrong stash by
;; accident is worse than a "select a stash" nudge).
(define (do-stash-pop! state-box)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)]
         [revs (filter stash-spec? (action-revs state))])
    (cond
      [(not (backend-supports? backend 'stash))
        (set-msg! state (unsupported-message backend 'stash) 'error)]
      [(null? revs) (set-msg! state "juju: select a stash to pop" 'error)]
      [else
        (let ([r (backend-stash-pop backend (first-rev revs))])
          (reload-and-report! state-box r))])))

;; Enter on a commit row: open its diff in a text view. Operations/stashes are
;; not commits, so they have nothing to show.
(define (show-commit-at! state-box row)
  (let* ([state (unbox state-box)]
         [backend (view-state-backend state)]
         [kind (row-section-kind row)]
         [c (row-object row)])
    (if (showable-commit-kind? kind)
      (let ([shown (backend-show backend (commit-record-id c))])
        (show-text-view
          (string-append " " (commit-record-short-id c) "  " (commit-record-subject c) " ")
          (commit-show-lines shown)))
      (set-msg! state "juju: nothing to show for this row" 'info))))

;;; Rendering ;;;

(define (view-title state)
  (let ([backend (view-state-backend state)])
    (string-append " juju (" (symbol->string (backend-name backend)) ")  "
      (backend-root backend)
      (colocated-marker backend)
      " ")))

(define (view-status state)
  (cons (status-line-text state) (view-state-message-tag state)))

;; A persistent title marker for a colocated repo on the git backend: git changes
;; desync jj's recorded working copy until its next command re-imports them. Empty
;; otherwise, or when the warning is disabled (config `juju-warn-colocated`).
;; Reading `backend-name` here is intentional and not a feature gate: the desync
;; is a git-specific display warning, not a capability the backend can advertise.
(define (colocated-marker backend)
  (if (and (juju-warn-colocated)
       (eq? (backend-name backend) 'git)
       (colocated? (backend-root backend)))
    "  [colocated: git->jj sync on next jj]"
    ""))

(define (status-line-text state)
  (let ([msg (view-state-message state)]
        [n (length (view-state-selection state))])
    (cond
      [(not (string=? msg "")) msg]
      [(> n 0)
        (string-append (number->string n) " marked  "
          "s stage  u unstage  x discard/drop/abandon  c commit  v unmark  q quit")]
      [else
        "s stage  u unstage  x del  c commit  V revert  b switch  / search  v mark  ? keys"])))

;; A one-screen key reference, shown on `?`.
(define (help-lines)
  (list
    "juju status - keys"
    ""
    "Movement:  j/k up/down   C-u/C-d page   Home/End ends"
    "Sections:  } next   { previous   ^ enclosing section"
    "Search:    / search   n next match   N previous match"
    "Folding:   Tab toggle fold   Enter visit file / toggle"
    "Select:    v mark/unmark row (acts on marks, else cursor)"
    ""
    "Stage:     s stage   u unstage   S stage all   U unstage all"
    "           x discard files / drop stash / abandon commit"
    "Commit:    c commit   a amend   e extend (on a commit row: edit, jj)"
    "Remote:    f fetch   F pull   P push"
    ""
    "On a commit (recent/bookmarks/...):"
    "           Enter show   V revert   y cherry-pick   r rebase onto"
    "           i rebase interactively from here   e edit change (jj)"
    "           b switch   B set bookmark here   p stash pop"
    "           z undo   Z redo"
    ""
    "Other:     g refresh   q / Esc close"))

;;; Event handling ;;;

;; The action keys. The shared shell handles movement (via the selectable-aware
;; move!/page!/edge! hooks below) and Esc/q close; this runs first for
;; everything else and returns #f to fall through to the shell defaults. The
;; view state is mutable and lives in the box for the component's lifetime; each
;; branch mutates it in place. Helix re-renders any layer that consumed an event
;; and render reads the same, mutated box.
(define (view-keys state-box event)
  (let ([state (unbox state-box)])
    (cond
      ;; Section navigation: }/{ next/prev sibling, ^ enclosing section.
      [(char-is? event #\}) (cursor-to-section! state 'next) event-result/consume]
      [(char-is? event #\{) (cursor-to-section! state 'prev) event-result/consume]
      [(char-is? event #\^) (cursor-to-section! state 'parent) event-result/consume]

      ;; In-buffer search: / prompt, n/N cycle matches.
      [(char-is? event #\/) (start-search! state-box) event-result/consume]
      [(char-is? event #\n) (search-step! state 1) event-result/consume]
      [(char-is? event #\N) (search-step! state -1) event-result/consume]

      [(key-event-tab? event)
        (clear-message! state)
        (toggle-fold-at-cursor! state)
        event-result/consume]
      [(key-event-enter? event)
        (clear-message! state)
        (visit-at-cursor! state-box)
        event-result/consume]
      [(char-is? event #\g)
        (reload! state-box)
        event-result/consume]

      ;; Selection.
      [(char-is? event #\v) (toggle-mark! state) event-result/consume]

      ;; Stage / unstage (selection-first). x is operand-typed: discard files /
      ;; drop a stash / abandon a commit (see do-x!).
      [(char-is? event #\s) (do-selection-op! state-box 'stage 'stage) event-result/consume]
      [(char-is? event #\u) (do-selection-op! state-box 'unstage 'unstage) event-result/consume]
      [(char-is? event #\x) (do-x! state-box) event-result/consume]
      [(char-is? event #\S) (do-bulk! state-box 'stage-all 'stage-all) event-result/consume]
      [(char-is? event #\U) (do-bulk! state-box 'unstage-all 'unstage-all) event-result/consume]

      ;; Commit family.
      [(char-is? event #\c) (do-commit! state-box #f) event-result/consume]
      [(char-is? event #\a) (do-commit! state-box #t) event-result/consume]
      [(char-is? event #\e) (do-edit-or-extend! state-box) event-result/consume]

      ;; Remote.
      [(char-is? event #\f) (do-network! state-box 'fetch 'fetch) event-result/consume]
      [(char-is? event #\F) (do-network! state-box 'pull 'pull) event-result/consume]
      [(char-is? event #\P) (do-network! state-box 'push 'push) event-result/consume]

      ;; History / branch actions on the selected commit(s).
      [(char-is? event #\V) (do-revert! state-box) event-result/consume]
      [(char-is? event #\y) (do-cherry-pick! state-box) event-result/consume]
      [(char-is? event #\r) (do-rebase-onto! state-box) event-result/consume]
      [(char-is? event #\i) (do-rebase-interactive! state-box) event-result/consume]
      [(char-is? event #\b) (do-switch! state-box) event-result/consume]
      [(char-is? event #\B) (do-set-bookmark! state-box) event-result/consume]
      [(char-is? event #\p) (do-stash-pop! state-box) event-result/consume]
      [(char-is? event #\z) (do-bulk! state-box 'undo 'undo) event-result/consume]
      [(char-is? event #\Z) (do-bulk! state-box 'redo 'redo) event-result/consume]

      ;; Help.
      [(char-is? event #\?) (show-text-view "juju keys" (help-lines)) event-result/consume]

      [else #f])))

(define (clear-message! state)
  (set-view-state-message! state "")
  (set-view-state-message-tag! state 'info))

;;; Lifecycle ;;;

(define status-view-spec
  (make-overlay-view
    #:name
    COMPONENT-NAME
    #:title
    view-title
    #:rows
    current-rows
    #:cursor
    view-state-cursor
    #:set-cursor!
    set-view-state-cursor!
    #:top
    view-state-top
    #:set-top!
    set-view-state-top!
    #:status
    view-status
    #:marked
    view-state-selection
    #:on-key
    view-keys
    #:move!
    move-cursor!
    #:page!
    page-cursor!
    #:edge!
    cursor-to-edge!
    #:page-size
    8
    #:on-close
    (lambda (state-box) (set-box! *open-view* #f))
    #:tag->style
    juju-tag->style
    #:overlay-scale
    (lambda () (juju-overlay-scale))))

;;@doc
;; Open the status view for the backend active in `start-dir`. Echoes a message
;; and does nothing when `start-dir` is not inside a git/jj repository.
(define (open-status-view start-dir)
  (let ([backend (active-backend start-dir)])
    (if (not backend)
      (set-status! "juju: not inside a git or jj repository")
      ;; Record the box (for out-of-band refresh) via the on-open callback so
      ;; the command ends in push-component!, not set-box! (which Helix echoes).
      (open-overlay-view! status-view-spec
        (make-view-state backend (make-fold-state))
        (lambda (state-box) (set-box! *open-view* state-box))))))

;;@doc
;; Refresh the currently-open status view, if any, after an external mutation
;; (a typed command). A no-op when no view is open.
(define (refresh-open-view!)
  (let ([sb (unbox *open-view*)])
    (when sb (reload! sb))))
