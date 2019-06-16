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
(require 'choice-program-complete)
(require 'flex-compile-manage)

;;; choice-prog file compiler
(defclass choice-prog-flex-compiler (single-buffer-flex-compiler
				     conf-flex-compiler)
  ((program :initarg :program
	    :initform nil
	    :documentation "An instance of `choice-prog'.")
   (action :initarg :action
	   :initform nil
	   :documentation "The action to invoke on the program."))
  :documentation "\
Prompt and more easily invoke choice/action based programs using the
\[Choice Program](https://github.com/plandes/choice-program) Emacs library.")

(cl-defmethod initialize-instance ((this choice-prog-flex-compiler) &optional slots)
  (let* ((read-prog '(lambda (this compiler default prompt history)
		       (flex-compiler-choice-prog-read-program
			compiler default prompt history)))
	 (read-action '(lambda (this compiler default prompt history)
			 (-> (flex-compiler-choice-prog-program compiler t)
			     (choice-prog-read-option default history))))
	 (props (list (config-eval-prop :object-name 'program
					:prompt "Program"
					:func read-prog
					:prop-entry this
					:required t
					:input-type 'last
					:order 0)
		      (config-eval-prop :object-name 'action
					:prompt "Action"
					:func read-action
					:prop-entry this
					:required t
					:order 1
					:input-type 'last))))
    (setq slots (plist-put slots :object-name "choice-program")
	  slots (plist-put slots :description "Choice program")
	  slots (plist-put slots :buffer-name "Choice Program")
	  slots (plist-put slots :kill-buffer-clean t)
	  slots (plist-put slots
			   :props (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

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

(cl-defmethod config-prop-set ((this choice-prog-flex-compiler)
				    prop val)
  (when (eq (config-prop-name prop) 'program)
    (setf (slot-value this 'action) nil)
    (config-persistent-reset (config-prop-by-name this 'action)))
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
    (run (config-prop-entry-show-configuration this))))

;; register the compiler
(flex-compile-manager-register the-flex-compile-manager
			       (choice-prog-flex-compiler))

(provide 'flex-compile-choice-prog)

;;; flex-compile-choice-prog.el ends here
