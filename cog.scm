;; Copyright (C) 2026 Tom Waddington
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; cog.scm - Forge package manifest for juju
;;;
;;; Installable with Steel's package manager:
;;;
;;;   forge pkg install --git https://github.com/waddie/juju
;;;
;;; then, in ~/.config/helix/init.scm:
;;;
;;;   (require "juju/juju.scm")
;;;
;;; Forge copies this directory to ~/.steel/cogs/juju/.

(define package-name 'juju)
(define version "0.3.9-alpha")

;; Shared library dependencies, installed to ~/.steel/cogs/<name>/ alongside
;; juju (Forge does this; install.sh does the same by hand). One installed copy
;; serves every plugin that depends on it, so upgrade dependent plugins
;; together.
;;   ui-utils.hx  - overlay shell, drawing, string/scroll helpers.
;;   run-command  - the spawn/capture core behind process.scm's run-vcs.
(define dependencies
  '((#:name "ui-utils.hx"
     #:git-url
     "https://github.com/waddie/ui-utils.hx"
     #:sha
     "d8daf89327b7e0431ec0ce66150aac6eda48b026")
    (#:name "run-command"
     #:git-url
     "https://github.com/waddie/run-command.scm"
     #:sha
     "ed42a376c4761e10530981c34797e7dde8e5abef")))
