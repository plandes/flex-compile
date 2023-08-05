;;; flex-compile.el --- Run, evaluate and compile across many languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2023 Paul Landes

;; Version: 1.4
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: compilation integration processes
;; URL: https://github.com/plandes/flex-compile
;; Package-Requires: ((emacs "26.1") (dash "2.17.0") (buffer-manage "1.1"))

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
(require 'flex-compile-choice-program)
(require 'flex-compile-org-export)
(require 'flex-compile-xml-validate)
(require 'flex-compile-python)
(require 'flex-compile-clojure)
(require 'flex-compile-lisp)
(require 'flex-compile-ess)
(require 'flex-compile-comint)
(require 'flex-compile-cli)
(require 'flex-compiler)

(defun flex-compile-key-bindings ()
  "Bind keys globally to flex compiler actions.

Note: this clobbers the following bindings:
  `mark-page'
  `delete-blank-lines'.

This binds the following:
  - C-x C-p: `flex-compiler-do-activate'
  - C-x C-u: `flex-compiler-do-compile'
  - C-x C-y: `flex-compiler-do-clean'
  - C-x C-i: `flex-compiler-do-run-or-set-config'
  - C-x C-o: `flex-compiler-do-eval'"
  ;; switch compiler; clobbers `mark-page'
  (global-set-key "\C-x\C-p" 'flex-compiler-do-activate)
  (global-set-key "\C-x\C-u" 'flex-compiler-do-compile)
  (global-set-key "\C-x\C-y" 'flex-compiler-do-clean)
  (global-set-key "\C-x\C-i" 'flex-compiler-do-run-or-set-config)
  ;; clobbers `delete-blank-lines'
  (global-set-key "\C-x\C-o" 'flex-compiler-do-eval))

(defun flex-compile-init ()
  "Initialize the flex-compile system."
  (flex-compiler-config-load)
  (flex-compiler-do-activate "disable"))

(flex-compile-init)

(provide 'flex-compile)

;;; flex-compile.el ends here
