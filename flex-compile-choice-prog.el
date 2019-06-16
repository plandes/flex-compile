;;; flex-compile-choice-prog.el --- compile functions

;; Copyright (C) 2015 - 2019 Paul Landes

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
(require 'dash)
(require 'flex-compile-manage)
(require 'choice-program-complete)

;;; choice-prog file compiler
(defclass choice-prog-flex-compiler (single-buffer-flex-compiler
				     conf-flex-compiler)
  ((program :initarg :program
	    :initform nil
	    :documentation "An instance of `choice-prog'.")
   (action :initarg :action
	   :initform nil
	   :documentation "The action to invoke on the program."))
  :documentation "Invoke choice-prog on a configured choice-program.")

(cl-defmethod initialize-instance ((this choice-prog-flex-compiler) &optional args)
  (let* ((read-prog '(lambda (this compiler default prompt history)
		       (flex-compiler-choice-prog-read-program
			compiler default prompt history)))
	 (read-action '(lambda (this compiler default prompt history)
			 (-> (flex-compiler-choice-prog-program compiler t)
			     (choice-prog-read-option default history))))
	 (props (list (flex-conf-eval-prop :name 'program
					   :prompt "Program"
					   :func read-prog
					   :compiler this
					   :required t
					   :input-type 'last
					   :order 0)
		      (flex-conf-eval-prop :name 'action
					   :prompt "Action"
					   :func read-action
					   :compiler this
					   :required t
					   :order 1
					   :input-type 'last))))
    (setq args (plist-put args :name "choice-program")
	  args (plist-put args :description "Choice program")
	  args (plist-put args :buffer-name "Choice Program")
	  args (plist-put args :kill-buffer-clean t)
	  args (plist-put args :props (append (plist-get args :props) props))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this choice-prog-flex-compiler))
  (require 'choice-program))

(cl-defmethod flex-compiler-choice-prog-map ((this choice-prog-flex-compiler))
  (->> (choice-prog-instances)
       (-map '(lambda (this)
       		(cons (choice-prog-name this) this)))))

(cl-defmethod flex-compiler-choice-prog-read-program ((this choice-prog-flex-compiler)
						      default prompt history)
  "Read a `choice-prog' from the user.
DEFAULT, PROMPT and HISTORY are used for user input and come from
the `flex-compile' framework."
  (let ((choices (->> (flex-compiler-choice-prog-map this)
		      (-map #'car)
		      (-map #'intern))))
    (choice-program-complete prompt choices t t nil history default)))

(cl-defmethod flex-compiler-choice-prog-program ((this choice-prog-flex-compiler)
						 &optional expectp)
  "Read an action for the \(already) selected `choice-prog'"
  (with-slots (program) this
    (if (and (null program) expectp)
	(error "No program set"))
    (when program
      (let ((ret (->> (flex-compiler-choice-prog-map this)
		      (assoc program)
		      cdr)))
	(unless ret
	  (error "No such program: `%S'" program))
	ret))))

(cl-defmethod flex-compiler-conf-set-prop ((this choice-prog-flex-compiler)
					   prop val)
  (setf (slot-value this 'action) nil)
  (flex-compile-clear (flex-compiler-conf-prop-by-name this 'action))
  (cl-call-next-method this prop val))

(cl-defmethod flex-compiler-buffer-name ((this choice-prog-flex-compiler))
  (let ((prog (flex-compiler-choice-prog-program this)))
    (if prog
	(slot-value prog 'buffer-name)
      (cl-call-next-method this))))

(cl-defmethod flex-compiler-start-buffer ((this choice-prog-flex-compiler)
					  start-type)
  (cl-case start-type
    (compile (let ((prog (flex-compiler-choice-prog-program this))
		   (action (slot-value this 'action)))
	       (choice-prog-exec prog action)))
    (run (flex-compiler-show-configuration this))))

;; register the compiler
(flex-compile-manager-register the-flex-compile-manager
			       (choice-prog-flex-compiler))

(provide 'flex-compile-choice-prog)

;;; flex-compile-choice-prog.el ends here
