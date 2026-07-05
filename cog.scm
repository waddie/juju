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
(define version "0.3.4-alpha")

;; The shared UI library (overlay shell, drawing, string/scroll helpers).
;; Forge installs it to ~/.steel/cogs/ui-utils.hx/ alongside juju; install.sh
;; does the same by hand. One installed copy serves every plugin that depends
;; on it, so upgrade dependent plugins together.
(define dependencies
  '((#:name "ui-utils.hx"
     #:git-url
     "https://github.com/waddie/ui-utils.hx"
     #:sha
     "2998d8229330e433e483745fc8750702b8d134e4")))
