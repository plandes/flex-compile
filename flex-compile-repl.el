;;; flex-compile-repl.el --- a REPL based compiler  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2020 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: compilation integration processes

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

;; Run, evaluate and compile functionality for a variety of different languages
;; and modes.  The specific "compilation" method is different across each
;; add-on library.  For example, for ESS and Clojure you can evaluate a
;; specific file and/or evaluate a specfic expression via a REPL.  For running
;; a script or starting a `make` an async process is started.
;;
;; For more information see https://github.com/plandes/flex-compile

;;; Code:

(require 'comint)
(require 'eieio)
(require 'flex-compile-config)

(defclass repl-flex-compiler (single-buffer-flex-compiler
			      conf-file-flex-compiler)
  ((repl-buffer-regexp :initarg :repl-buffer-regexp
		       :type string
		       :documentation "\
Regular expression to match buffers for functions like killing the session.")
   (derived-buffer-names :initarg :derived-buffer-names
			 :initform nil
			 :type list
			 :documentation "\
List of buffers for functions (like killing a buffer) when session ends.")
   (repl-buffer-start-timeout :initarg :repl-buffer-start-timeout
			      :initform 1
			      :type integer
			      :documentation "\
Number of seconds to wait to start before giving up (and not displaying).
If this is 0, don't wait or display the buffer when it comes up.")
   (prompt-kill-repl-buffer :initarg :prompt-kill-repl-buffer
			    :initform t
			    :type boolean
			    :documentation "\
If non-`nil' then prompt to kill a REPL buffer on clean.")
   (output-clear :initarg :output-clear
		 :initform nil
		 :type boolean
		 :documentation "\
Whether or not to clear comint buffer after a compilation.")
   (form-history :initarg :form-history
		 :initform (gensym "config-repl-form-history")
		 :type symbol
		 :documentation "\
The history variable for the eval form history."))
  :method-invocation-order :c3
  :documentation "Compiles by evaluating expressions in the REPL.")

(cl-defmethod initialize-instance ((this repl-flex-compiler) &optional slots)
  (let* ((fn '(lambda (this default prmopt history)
		(split-string (read-string prompt nil history default))))
	 (props (list (config-boolean-prop :object-name 'output-clear
					   :prop-entry this
					   :prompt "Clear output on compile"
					   :input-type 'toggle)
		      (config-boolean-prop :object-name 'prompt-kill-repl-buffer
					   :prop-entry this
					   :prompt "Confirm REPL buffer kills"
					   :input-type 'toggle))))
    (setq slots (plist-put slots
			   :props (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-repl-start ((this repl-flex-compiler))
  "Start the REPL."
  (config-persistent--unimplemented this "start-repl"))

(cl-defmethod flex-compiler-repl-compile ((this repl-flex-compiler) file)
  "Invoked by `compile' type messages from the compiler manager.

FILE gets evaluated by the compiler either as a IPC communication or by direct
insertion in the REPL buffer.

This method is meant to allow for REPL compiles \(really some kind of
evaluation), while allowing base class compilation features.."
  (config-persistent--unimplemented this "repl-compile"))

(cl-defmethod flex-compiler-wait-for-buffer ((this repl-flex-compiler))
  "Wait for the compilation to start.

The caller raises and error if it doesn't start in time."
  (with-slots (repl-buffer-start-timeout) this
    (let ((count-down repl-buffer-start-timeout)
	  buf)
      (cl-block wfb
       (dotimes (i count-down)
	 (setq buf (flex-compiler-buffer this))
	 (if buf
	     (cl-return-from wfb buf)
	   (message "Waiting for buffer to start... (%d)"
		    (- count-down i))
	   (sit-for 1)))))))

(cl-defmethod flex-compiler-repl-running-p ((this repl-flex-compiler))
  "Return whether or not the REPL is currently running."
  (not (null (flex-compiler-buffer this))))

(cl-defmethod flex-compiler-repl-assert-running ((this repl-flex-compiler))
  "Raise an error if the REPL IS running."
  (unless (flex-compiler-repl-running-p this)
    (error "The REPL for %s isn't started" (config-entry-name this))))

(cl-defmethod flex-compiler-repl-assert-not-running ((this repl-flex-compiler))
  "Raise an error if the REPL isn't running."
  (if (flex-compiler-repl-running-p this)
      (error "Compiler %s is already running"
	     (config-entry-name this))))

(cl-defmethod flex-compiler-repl--run-start ((this repl-flex-compiler))
  (with-slots (repl-buffer-start-timeout start-directory) this
    (let ((timeout repl-buffer-start-timeout)
	  buf)
      (unless (flex-compiler-repl-running-p this)
	(config-prop-entry-set-required this)
	(let ((default-directory (or start-directory default-directory)))
	  (flex-compiler-repl-start this))
	(when (> timeout 0)
	  (setq buf (flex-compiler-wait-for-buffer this))
	  (unless buf
	    (error "Couldn't create REPL for compiler %s"
		   (config-entry-name this))))))))

(cl-defmethod flex-compiler-send-input ((this repl-flex-compiler)
					&optional command)
  "Send input/commands to the REPL."
  (goto-char (point-max))
  (insert command)
  (comint-send-input))

(cl-defmethod flex-compiler-run-command ((this repl-flex-compiler)
					 &optional command)
  "Send commands to the REPL to evaluate an expression or start a process."
  (flex-compiler-repl-assert-running this)
  (let ((buf (flex-compiler-buffer this))
	pos win)
    (with-current-buffer buf
      (if command
	  (flex-compiler-send-input this command)))))

(cl-defmethod flex-compiler-buffer ((this repl-flex-compiler))
  "Find the first REPL buffer found in the buffer list."
  (with-slots (repl-buffer-regexp) this
    (cl-block found-buf
      (dolist (buf (buffer-list))
	(let ((buf-name (buffer-name buf)))
	  (when (string-match repl-buffer-regexp buf-name)
	    (cl-return-from found-buf buf)))))))

(cl-defmethod flex-compiler-kill-repl ((this repl-flex-compiler))
  "Kill the compiler's REPL."
  (with-slots (derived-buffer-names prompt-kill-repl-buffer) this
    (let ((bufs (append (mapcar 'get-buffer derived-buffer-names)
			(cons (flex-compiler-buffer this) nil)))
	  (count 0))
      (dolist (buf bufs)
	(when (buffer-live-p buf)
	  (let ((kill-buffer-query-functions
		 (if prompt-kill-repl-buffer
		     kill-buffer-query-functions
		   nil)))
	    (kill-buffer buf))
	  (cl-incf count)))
      (message "%s killed %d buffer(s)"
	       (capitalize (config-entry-name this)) count))))

(cl-defmethod flex-compiler-start-buffer ((this repl-flex-compiler) start-type)
  (config-prop-entry-set-required this)
  (with-slots (config-file output-clear) this
    (let ((runningp (flex-compiler-repl-running-p this)))
      (cl-case start-type
	(compile (progn
		   (unless runningp
		     (flex-compiler-run this))
		   (if (flex-compiler-repl-running-p this)
		       (progn
			 (when output-clear
			   (with-current-buffer (flex-compiler-buffer this)
			     (comint-clear-buffer)))
			 (flex-compiler-repl-compile this config-file))
		     (if runningp
			 (error "REPL hasn't started")
		       (message "REPL still starting, please wait")))
		   (flex-compiler-buffer this)))
	(run (progn
	       (flex-compiler-repl--run-start this)
	       (flex-compiler-buffer this)))
	(clean (progn
		 (flex-compiler-kill-repl this)
		 'killed-buffer))))))

(cl-defmethod flex-compiler-eval-initial-at-point ((this repl-flex-compiler))
  "Return the expression at or right behind the current point."
  nil)

(cl-defmethod flex-compiler-eval-form-impl ((this repl-flex-compiler) form)
  "Evaluate the FORM and return the response of the REPL."
  (config-persistent--unimplemented this "eval-form-impl"))

(cl-defmethod flex-compiler-query-read-form ((this repl-flex-compiler)
					     no-input-p)
  "Read a form, meaningful for the compiler, from the user."
  (with-slots (form-history) this
    (let ((init (flex-compiler-eval-initial-at-point this)))
      (if no-input-p
	  init
	(read-string "Form: " init form-history)))))

(cl-defmethod flex-compiler-evaluate-form ((this repl-flex-compiler)
					   &optional form)
  "Return the evaluation form.

See the `:eval-form' slot."
  (let ((res (flex-compiler-eval-form-impl this form)))
    (if (stringp res)
	res
      (prin1-to-string res))))

(provide 'flex-compile-repl)

;;; flex-compile-repl.el ends here
