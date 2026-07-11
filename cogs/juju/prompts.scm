;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; prompts.scm - the "read argument" step for commands
;;;
;;; Commands needing more than a selection (a commit message, a target ref, a
;;; rebase destination) gather it here.

(require "helix/editor.scm")
(require (prefix-in helix.static. "helix/static.scm"))

(provide editor-cwd
  current-file-path)

;;@doc
;; The editor's working directory, or #f if unavailable.
(define (editor-cwd)
  (with-handler (lambda (err) #f) (helix.static.get-helix-cwd)))

;;@doc
;; Absolute path of the focused document, or #f when none (e.g. a scratch).
(define (current-file-path)
  (with-handler (lambda (err) #f)
    (let* ([focus (editor-focus)]
           [doc-id (editor->doc-id focus)])
      (editor-document->path doc-id))))
