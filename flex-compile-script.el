;;; flex-compile-script.el --- script compile functions

;; Copyright (C) 2015 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: script integration compilation processes

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
  (let* ((fn '(lambda (this compiler default prompt history)
		(split-string (read-string prompt nil history default))))
	 (props (list (config-eval-prop :object-name 'arguments
					:prompt "Arguments"
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
  (require 'compile)
  (require 'choice-program))

(cl-defmethod config-prop-set ((this script-flex-compiler)
					   prop val)
  (setf (slot-value this 'arguments) nil)
  (cl-call-next-method this prop val))

(cl-defmethod flex-compiler-start-buffer ((this script-flex-compiler)
					  start-type)
  (let ((a (config-prop-by-name this 'start-directory)))
   (cl-case start-type
     (compile
      (with-slots (config-file start-directory arguments) this
	(let ((default-directory start-directory)
	      (buffer-name (flex-compiler-buffer-name this))
	      (cmd (concat config-file " "
			   (mapconcat #'identity arguments " ")))
	      reset-target
	      buf)
	  (with-current-buffer
	      (setq buf (compilation-start cmd nil
					   #'(lambda (mode-name)
					       buffer-name))))
	  buf)))
     (run (error "No defined run action for scripts")))))

(flex-compile-manager-register flex-compile-manage-inst (script-flex-compiler))

(provide 'flex-compile-script)

;;; flex-compile-script.el ends here
