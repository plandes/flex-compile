;;; flex-compile-command.el --- Convenience compiler that evaluates Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2025 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: interactive function command compile flexible processes
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

;; Compiler that evaluates Emacs Lisp.  This is handy for invoking interactive
;; functions (commands) using the `flex-compiler' keybindings.

;;; Code:

(require 'flex-compile-manage)

(defclass config-sexp-prop (config-prop)
  ()
  :method-invocation-order :c3
  :documentation "Property that prompts for a selection of a list of choices.")

(cl-defmethod initialize-instance ((this config-sexp-prop) &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (setq slots (plist-put slots :transient t))
  (cl-call-next-method this slots))

(cl-defmethod config-prop-read ((this config-sexp-prop))
  "Read the user input for the property.
The default reads a string using ‘config-prop-default’ and
‘config-prop-prompt’ with the history slot.
THIS is the instance."
  (list (cl-flet ((read-sexp
		   (prompt history)
		   (let (sexp)
		     (condition-case nil
			 (progn
			   (setq sexp (thing-at-point 'sexp))
			   ;; read as sexp and then back to string to make
			   ;; into one line
			   (if sexp (setq sexp (prin1-to-string (read sexp)))))
		       (t))
		     (read (read-string prompt sexp history)))))
	  (with-slots (history choices prompt) this
	    (let* ((cmd (read-command (format "%s (or RET for sexp): " prompt)))
		   (sexp-prompt (format "%s: " prompt)))
	      (if (eq '## cmd)
		  (let ((func (read-sexp sexp-prompt history)))
		    (eval `(defun flex-compiler-function-invoke-command ()
			     (interactive)
			     ,func)))
		cmd))))))


;;; func file compiler
(defclass command-flex-compiler (conf-flex-compiler)
  ((sexp :initarg :sexp
	 :initform nil
	 :documentation "The symbol expression to evaluate."))
  :method-invocation-order :c3
  :documentation "\
This \"compiler\" is more of a convenience to invoke an Emacs Lisp function or
form.  This is handy for functions that you end up invoking over and over with
`M-x` (i.e. `cider-test-run-ns-tests`).  See [motivation](#motivation).")

(cl-defmethod initialize-instance ((this command-flex-compiler)
				   &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (let ((props (list (config-sexp-prop :object-name 'sexp
				       :prop-entry this
				       :prompt "Expression"
				       :input-type 'last))))
    (setq slots (plist-put slots :object-name "command")
	  slots (plist-put slots
			   :props (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-display-buffer-alist ((this command-flex-compiler))
  "Return a do-nothing configuration to allow the function to display bufferes.
THIS is the object instance."
  (ignore this))

(cl-defmethod flex-compiler-compile ((this command-flex-compiler))
  "Evalute the function or symbolic expression stored in THIS instance.
The result of the evaulation is given to `message' and returned."
  (unless (slot-value this 'sexp)
    (config-prop-entry-configure this nil))
  (with-slots (sexp) this
    (let ((res (apply sexp)))
      (message "%S "res)
      res)))

(flex-compile-manager-register flex-compile-manage-inst
			       (command-flex-compiler))

(provide 'flex-compile-command)

;;; flex-compile-command.el ends here
