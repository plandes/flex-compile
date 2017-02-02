;;; compile-flex-command.el --- func compile functions

;; Copyright (C) 2015 - 2017 Paul Landes

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

;; Implementation compiler for invoking interactive functions (commands).

;;; Code:

(require 'compile-flex)

(defvar compile-flex-read-sexp-history nil
  "History variable for `read-sexp'.")

(defun compile-flex-read-sexp (&optional prompt history no-read-p)
  "Read a symobl expression from user input.
PROMPT is used to prompt the user interactively.
HISTORY is a symbol with a variable bounded to the listing of the history.
This defaults to `read-sexp-history'.
NO-READ-P, if non-nil, read the expression with `read'."
  (setq prompt (or prompt "Lisp Expression: "))
  (setq history (or history 'read-sexp-history))
  (let (sexp ret)
    (condition-case err
	(progn
	  (setq sexp (thing-at-point 'sexp))
	  ;; read as sexp and then back to string to make
	  ;; into one line
	  (if sexp (setq sexp (prin1-to-string (read sexp)))))
      (t))
    (setq ret (read-string prompt sexp history))
    (if (not no-read-p) (setq ret (read ret)))
    ret))

;;; func file compiler
(defclass command-flex-compiler (optionable-flex-compiler) ())

(defmethod initialize-instance ((this command-flex-compiler) &rest rest)
  (oset this :name "command")
  (apply 'call-next-method this rest))

(defvar flex-compiler-read-options-command-history nil)

(defmethod flex-compiler-read-options ((this command-flex-compiler))
  (list
   (let ((cmd (read-command "Function to invoke (or RET for sexp): ")))
     (if (eq '## cmd)
	 (let ((func (compile-flex-read-sexp
		      nil 'flex-compiler-read-options-command-history)))
	   (eval `(defun flex-compiler-function-invoke-command ()
		    (interactive)
		    ,func)))
       cmd))))

(defmethod flex-compiler-compile ((this command-flex-compiler))
  (let ((res (apply (flex-compiler-options this))))
    (message "%S "res)
    res))

(flex-compile-manager-register the-flex-compile-manager
			       (command-flex-compiler nil))

(provide 'compile-flex-command)

;;; compile-flex-command.el ends here
