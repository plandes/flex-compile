;;; flex-compile-choice-prog.el --- compile functions

;; Copyright (C) 2015 - 2017 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: choice-prog compile flexible

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

;; Implementation compiler for choice based programs.  See:
;; https://github.com/plandes/choice-program

;;; Code:

(require 'cl-lib)
(require 'flex-compile-manage)
(require 'choice-program-complete)

(defvar flex-compile-choice-prog-inst-history nil
  "History for choice-program instances.")

;;; choice-prog file compiler
(defclass choice-prog-flex-compiler (optionable-flex-compiler)
  ((program :initarg :program
	    :initform nil
	    :documentation "An instance of `choice-prog'."))
  :documentation "Invoke choice-prog on a configured choice-program.")

(cl-defmethod initialize-instance ((this choice-prog-flex-compiler)
				   &optional args)
  (oset this :name "choice-program")
  (oset this :force-set-compile-options-p t)
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this choice-prog-flex-compiler))
  (require 'choice-program))

(cl-defmethod flex-compiler-choice-prog-map ((this choice-prog-flex-compiler))
  (->> (choice-prog-instances)
       (-map '(lambda (this)
		(cons (choice-prog-name this) this)))))

(cl-defmethod flex-compiler-choice-prog-read-program ((this choice-prog-flex-compiler))
  (let ((choices (->> (flex-compiler-choice-prog-map this)
		      (-map #'car))))
    (choice-program-complete "Program" choices t nil nil
			     'flex-compile-choice-prog-inst-history
			     (cl-second flex-compile-choice-prog-inst-history)
			     nil nil t)))

(cl-defmethod flex-compiler-choice-prog-program ((this choice-prog-flex-compiler)
						 &optional force-read-p)
  (with-slots (compile-options program) this
    (let ((old-program program))
      (if (or force-read-p (not program))
	  (setq program (flex-compiler-choice-prog-read-program this)))
      (if (not (equal program old-program))
	  (setq compile-options nil))
      (->> (flex-compiler-choice-prog-map this)
	   (assoc program)
	   cdr))))

(cl-defmethod flex-compiler-compile ((this choice-prog-flex-compiler))
  (unless (slot-value this 'compile-options)
    (flex-compiler-read-set-options this nil))
  (let ((prog (flex-compiler-choice-prog-program this))
	(mnemonic (car (flex-compiler-options this))))
    (choice-prog-exec prog mnemonic)))

(cl-defmethod flex-compiler-read-options ((this choice-prog-flex-compiler))
  (->> (flex-compiler-choice-prog-program this)
       choice-prog-read-option))

;; register the compiler
(flex-compile-manager-register the-flex-compile-manager
			       (choice-prog-flex-compiler))

;; (defun a ()
;;   (interactive)
;;   (flex-compile-manager-register the-flex-compile-manager
;; 				 (choice-prog-flex-compiler))
;;   (flex-compiler-config-load))


(provide 'flex-compile-choice-prog)

;;; flex-compile-choice-prog.el ends here
