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
(define version "0.3.0-alpha")
(define dependencies '())
