;;; flex-compile-command.el --- convenience compiler that evaluates Emacs Lisp

;; Copyright (C) 2015 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: interactive function command compile flexible

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

(defclass flex-conf-sexp-prop (flex-conf-prop) ()
  :documentation "Property that prompts for a selection of a list of choices.")

(cl-defmethod flex-compiler-conf-read ((this flex-conf-sexp-prop))
  (list
   (cl-flet ((read-sexp
	      (prompt history no-read-p)
	      (let (sexp ret)
		(condition-case err
		    (progn
		      (setq sexp (thing-at-point 'sexp))
		      ;; read as sexp and then back to string to make
		      ;; into one line
		      (if sexp (setq sexp (prin1-to-string (read sexp)))))
		  (t))
		(read (read-string prompt sexp history)))))
     (with-slots (history choices prompt) this
       (let ((cmd (read-command (format "%s (or RET for sexp): " prompt)))
	     (sexp-prompt (format "%s: " prompt)))
	 (if (eq '## cmd)
	     (let ((func (read-sexp sexp-prompt history nil)))
	       (eval `(defun flex-compiler-function-invoke-command ()
			(interactive)
			,func)))
	   cmd))))))


;;; func file compiler
(defclass command-flex-compiler (conf-flex-compiler)
  ((sexp :initarg :sexp
	 :initform nil))
  :documentation "Convenience compiler that evaluates Emacs Lisp.")

(cl-defmethod initialize-instance ((this command-flex-compiler) &optional args)
  (let ((props (list (flex-conf-sexp-prop :name 'sexp
					  :compiler this
					  :prompt "Expression"
					  :input-type 'last))))
    (setq args (plist-put args :name "command")
	  args (plist-put args :props (append (plist-get args :props) props))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-compile ((this command-flex-compiler))
  (unless (slot-value this 'sexp)
    (flex-compiler-configure this nil))
  (with-slots (sexp) this
    (let ((res (apply sexp)))
      (message "%S "res)
      res)))

(flex-compile-manager-register the-flex-compile-manager
			       (command-flex-compiler))

(provide 'flex-compile-command)

;;; flex-compile-command.el ends here
