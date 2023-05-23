;;; flex-compile-script.el --- Script compile functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2023 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: script integration compilation processes
;; URL: https://github.com/plandes/flex-compile
;; Package-Requires: ((emacs "26.1"))
;; Package-Version: 0

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

;; Implementation compiler for script integration

;;; Code:

(require 'flex-compile-manage)

;;; script file compiler
(defclass script-flex-compiler (single-buffer-flex-compiler
				conf-file-flex-compiler)
  ((arguments :initarg :arguments
	      :initform nil
	      :documentation "The arguments to give to the script."))
  :method-invocation-order :c3
  :documentation "\
This compiler runs a script with optional arguments in an async buffer.
See [motivation](#motivation).")

(cl-defmethod initialize-instance ((this script-flex-compiler) &optional slots)
  "Initialize THIS instance using SLOTS as initial values."
  (let* ((fn (lambda (this compiler default prompt history)
	       (ignore this prompt compiler)
	       (let ((input (read-string prompt nil history default)))
		 (and input (split-string input)))))
	 (props (list (config-eval-prop :object-name 'arguments
					:prompt "Arguments (SPACE RET for none)"
					:func fn
					:prop-entry this
					:input-type 'last))))
    (setq slots (plist-put slots :object-name "script")
	  slots (plist-put slots :description "Script")
	  slots (plist-put slots :validate-modes
			  '(sh-mode cperl-mode python-mode))
	  slots (plist-put slots :buffer-name "Script Compile")
	  slots (plist-put slots :kill-buffer-clean t)
	  slots (plist-put slots :props
			   (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this script-flex-compiler))
  "Load `compile' and `choice-program' libraries for THIS compiler."
  (ignore this)
  (require 'compile)
  (require 'choice-program))

(cl-defmethod config-prop-set ((this script-flex-compiler) prop val)
  "Set property PROP to VAL on THIS compiler.
In addition, set the `arguments' `config-prop' to nil."
  (setf (slot-value this 'arguments) nil)
  (cl-call-next-method this prop val))

(cl-defmethod flex-compiler-start-buffer ((this script-flex-compiler)
					  start-type)
  "Return a new buffer for THIS compiler with a processing compilation.
See the `single-buffer-flex-compiler' implementation of
`flex-compiler-start-buffer' for more information and START-TYPE."
  (cl-case start-type
    (compile
     (with-slots (config-file start-directory arguments) this
       (let ((default-directory start-directory)
	     (buffer-name (flex-compiler-buffer-name this))
	     (cmd (concat config-file " "
			  (mapconcat #'identity arguments " ")))
	     buf)
	 (with-current-buffer
	     (setq buf
		   (compilation-start cmd nil (lambda (_) buffer-name))))
	 buf)))
    (run (error "No defined run action for scripts"))))

(flex-compile-manager-register flex-compile-manage-inst (script-flex-compiler))

(provide 'flex-compile-script)

;;; flex-compile-script.el ends here
