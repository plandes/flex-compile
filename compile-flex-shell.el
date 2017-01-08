;;; compile-flex-shell.el --- shell compile functions

;; Copyright (C) 2015 - 2017 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: shell integration compilation

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

;; Implementation compiler for shell integration

;;; Code:

(require 'cl-lib)
(require 'compile-flex)

(defvar compile-flex-shell-finish-success-function nil
  "A function to call (if non-nill) if the compilation is successful.")

(defclass shell-flex-compiler (run-args-flex-compiler)
  ((executable :initarg :executable
	       :type symbol)
   (compile-options-metadata :initarg :compile-options-metadata
			     :type list)
   (finish-success-function :initarg :finish-success-function
			    :initform nil
			    :documentation "\
A function to call (if non-nill) if the compilation is successful.")))

(defmethod initialize-instance ((this shell-flex-compiler) &rest rest)
  (oset this :major-mode 'non-existing-mode-symbol)
  (apply 'call-next-method this rest))

(defmethod flex-compiler-load-libraries ((this shell-flex-compiler))
  (require 'compile)
  (require 'choice-program))

(defmethod flex-compiler-read-options ((this shell-flex-compiler))
  (remove
   nil
   (apply #'append
	  (mapcar
	   #'(lambda (entry)
	       (let ((opt (nth 0 entry))
		     (desc (nth 1 entry))
		     (getter (nth 2 entry))
		     (getter-type (nth 3 entry)))
		 (list opt
		       (cl-case getter-type
			 (function (apply getter (list (format "%s: " desc))))
			 (class-function (apply getter (list this)))
			 (otherwise (error "Unknown type: %s" getter-type))))))
		  (oref this :compile-options-metadata)))))

(defmethod flex-compiler-run-with-args ((this shell-flex-compiler) args)
  (with-slots (executable finish-success-function) this
    (let* ((exec (symbol-value executable))
	   (cmd (concat exec " " (mapconcat #'identity args " "))))
      (with-current-buffer
	  (compilation-start cmd nil
			     #'(lambda (mode-name)
				 (format "*Shell `%s' Compile*" exec))
			     t)
	(if finish-success-function
	      (add-to-list 'compile-flex-shell-finish-success-function
			   finish-success-function))))))

(provide 'compile-flex-shell)

;;; compile-flex-shell.el ends here
