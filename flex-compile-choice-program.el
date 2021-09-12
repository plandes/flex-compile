;;; flex-compile-choice-program.el --- Compile functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2020 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: choice-program compile flexible processes
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

;; Implementation compiler for choice based programs.  See:
;; https://github.com/plandes/choice-program

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'choice-program-complete)
(require 'flex-compile-manage)

;;; choice-program file compiler
(defclass choice-program-flex-compiler (single-buffer-flex-compiler
					conf-flex-compiler)
  ((program :initarg :program
	    :initform nil
	    :documentation "An instance of `choice-program'.")
   (action :initarg :action
	   :initform nil
	   :documentation "The action to invoke on the program."))
  :method-invocation-order :c3
  :documentation "\
Prompt and more easily invoke choice/action based programs using the
\[Choice Program](https://github.com/plandes/choice-program) Emacs library.")

(cl-defmethod initialize-instance ((this choice-program-flex-compiler)
				   &optional slots)
  "Initialize the THIS instance with SLOTS."
  (let* ((read-prog (lambda (this compiler default prompt history)
		      (flex-compiler-choice-program-read-program
		       compiler default prompt history)))
	 (read-action (lambda (this compiler default prompt history)
			(-> (flex-compiler-choice-program-program compiler t)
			    (choice-program-read-option default history))))
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
					:input-type 'last
					:order 1))))
    (setq slots (plist-put slots :object-name "choice-program")
	  slots (plist-put slots :description "Choice program")
	  slots (plist-put slots :buffer-name "Choice Program")
	  slots (plist-put slots :kill-buffer-clean t)
	  slots (plist-put slots
			   :props (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries
  ((this choice-program-flex-compiler))
  "Load the `choice-program' library for THIS compiler."
  (ignore this)
  (require 'choice-program))

(cl-defmethod flex-compiler-choice-program-map
  ((this choice-program-flex-compiler))
  "Return an alist of name to registered to `choice-program' instances.
THIS is the instance."
  (ignore this)
  (->> (choice-program-instances)
       (-map '(lambda (this)
		(cons (choice-program-name this) this)))))

(cl-defmethod flex-compiler-choice-program-read-program
  ((this choice-program-flex-compiler) default prompt history)
  "Read a `choice-program' from the user.
DEFAULT, PROMPT and HISTORY are used for user input and come from
the `flex-compile' framework.
THIS is the instance."
  (let ((choices (->> (flex-compiler-choice-program-map this)
		      (-map #'car)
		      (-map #'intern))))
    (choice-program-complete prompt choices t t nil history default)))

(cl-defmethod flex-compiler-choice-program-program
  ((this choice-program-flex-compiler) &optional expectp)
  "Read an action for the \(already) selected `choice-program'.
THIS is the instance.
EXPECTP, if non-nil, raise an exception if the program slot is nil."
  (with-slots (program) this
    (if (and (null program) expectp)
	(error "No program set"))
    (when program
      (let ((ret (->> (flex-compiler-choice-program-map this)
		      (assoc program)
		      cdr)))
	(unless ret
	  (error "No such program: `%S'" program))
	ret))))

(cl-defmethod config-prop-set ((this choice-program-flex-compiler) prop val)
  "Set property PROP to VAL for THIS compiler.

This also resets the `action' property when setting the `program' property."
  (when (eq (config-prop-name prop) 'program)
    (setf (slot-value this 'action) nil)
    ;; we need to some how be able to nil out the history when changing the
    ;; program as currently the history carries forward to other programs
    ;(config-persistent-reset (config-prop-by-name this 'mnemonic))
    (config-persistent-reset (config-prop-by-name this 'action)))
  (cl-call-next-method this prop val))

(cl-defmethod flex-compiler-buffer-name ((this choice-program-flex-compiler))
  "Return the buffer name of THIS compiler.

Use the `buffer-name' slot of the `flex-compiler-choice-program-program' if
non-nil, otherwise use the default `buffer-name' with syntax of the super
class."
  (let ((prog (flex-compiler-choice-program-program this)))
    (if prog
	(slot-value prog 'buffer-name)
      (cl-call-next-method this))))

(cl-defmethod flex-compiler-start-buffer ((this choice-program-flex-compiler)
					  start-type)
  "Return a new buffer for THIS compiler with a processing compilation.
See the `single-buffer-flex-compiler' implementation of
`flex-compiler-start-buffer' for more information and START-TYPE."
  (cl-case start-type
    (compile (let ((prog (flex-compiler-choice-program-program this))
		   (action (slot-value this 'action)))
	       (choice-program-exec prog action)))
    (run (config-prop-entry-show-configuration this))))

;; register the compiler
(flex-compile-manager-register flex-compile-manage-inst
			       (choice-program-flex-compiler))

(provide 'flex-compile-choice-program)

;;; flex-compile-choice-program.el ends here
