;;; flex-compile.el --- Run, evaluate and compile for a many languages and modes.

;; Copyright (C) 2015 - 2019 Paul Landes

;; Version: 0.5
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: compilation integration processes
;; URL: https://github.com/plandes/flex-compile
;; Package-Requires: ((emacs "26") (dash "2.13.0") (buffer-manage "0.8"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Run, evaluate and compile functionality for a variety of different languages
;; and modes.  The specific "compilation" method is different across each
;; add-on library.  For example, for ESS and Clojure you can evaluate a
;; specific file and/or evaluate a specfic expression via a REPL.  For running
;; a script or starting a `make' an async process is started.
;;
;; For more information see https://github.com/plandes/flex-compile

;;; Code:

(require 'flex-compile-manage)
(require 'flex-compile-script)
(require 'flex-compile-make)
(require 'flex-compile-command)
(require 'flex-compile-choice-prog)
(require 'flex-compile-org-export)
(require 'flex-compile-xml-validate)
(require 'flex-compile-python)
(require 'flex-compile-clojure)
(require 'flex-compile-ess)
(require 'flex-compile-comint)

(defun flex-compile-init ()
  "Initialize the flex-compile system."
  (flex-compiler-config-load)
  (flex-compiler-activate "disable"))

(flex-compile-init)

(provide 'flex-compile)

;;; flex-compile.el ends here
