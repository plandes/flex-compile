;;; flex-compile-repl.el --- a REPL based compiler

;; Copyright (C) 2015 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: compilation integration

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

(defvar flex-compiler-query-eval-mode nil
  "History variable for `flex-compiler-query-eval'.")

(defvar flex-compiler-query-eval-form-history nil
  "History variable for `flex-compiler-query-read-form'.")

(defclass repl-flex-compiler
  (conf-file-flex-compiler single-buffer-flex-compiler)
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
   (no-prompt-kill-repl-buffer :initarg :no-prompt-kill-repl-buffer
			       :initform nil
			       :type boolean
			       :documentation "\
If non-`nil' then don't prompt to kill a REPL buffer on clean.")
   (output-clear :initarg :output-clear
		 :initform 'no
		 :type symbol
		 :documentation "\
Whether or not to clear comint buffer after a compilation."))
  :documentation "Compiles by evaluating expressions in the REPL.")

(cl-defmethod initialize-instance ((this repl-flex-compiler) &optional args)
  (let* ((fn '(lambda (this default prmopt history)
		(split-string (read-string prompt nil history default))))
	 (props (list (flex-conf-choice-prop :name 'output-clear
					     :compiler this
					     :prompt "Clear output on compile"
					     :choices '(yes no)
					     :input-type 'toggle))))
    (setq args (plist-put args :props (append (plist-get args :props) props))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-repl-start ((this repl-flex-compiler))
  "Start the REPL."
  (flex-compiler--unimplemented this "start-repl"))

(cl-defmethod flex-compiler-repl-compile ((this repl-flex-compiler) file)
  "Invoked by `compile' type messages from the compiler manager.

FILE gets evaluated by the compiler either as a IPC communication or by direct
insertion in the REPL buffer.

This method is meant to allow for REPL compiles \(really some kind of
evaluation), while allowing base class compilation features.."
  (flex-compiler--unimplemented this "repl-compile"))

(cl-defmethod flex-compiler-wait-for-buffer ((this repl-flex-compiler))
  "Wait for the compilation to start.

The caller raises and error if it doesn't start in time."
  (with-slots (repl-buffer-start-timeout) this
    (let ((count-down repl-buffer-start-timeout)
	  buf)
      (dotimes (i count-down)
	(setq buf (flex-compiler-buffer this))
	(if buf
	    (cl-return buf)
	  (message "Waiting for buffer to start... (%d)"
		   (- count-down i))
	  (sit-for 1))))))

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
	(flex-compiler-set-required this)
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
    (dolist (buf (buffer-list))
      (let ((buf-name (buffer-name buf)))
	(when (string-match repl-buffer-regexp buf-name)
	  (cl-return buf))))))

(cl-defmethod flex-compiler-kill-repl ((this repl-flex-compiler))
  "Kill the compiler's REPL."
  (with-slots (name derived-buffer-names no-prompt-kill-repl-buffer) this
    (let ((bufs (append (mapcar 'get-buffer derived-buffer-names)
			(cons (flex-compiler-buffer this) nil)))
	  (count 0))
      (dolist (buf bufs)
	(when (buffer-live-p buf)
	  (let ((kill-buffer-query-functions
		 (if no-prompt-kill-repl-buffer
		     nil
		   kill-buffer-query-functions)))
	    (kill-buffer buf))
	  (cl-incf count)))
      (message "%s killed %d buffer(s)" (capitalize name) count))))

(cl-defmethod flex-compiler-start-buffer ((this repl-flex-compiler) start-type)
  (flex-compiler-set-required this)
  (with-slots (config-file output-clear) this
    (let ((runningp (flex-compiler-repl-running-p this)))
      (cl-case start-type
	(compile (progn
		   (unless runningp
		     (flex-compiler-run this))
		   (if (flex-compiler-repl-running-p this)
		       (progn
			 (when (eq output-clear 'yes)
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
  nil)

(cl-defmethod flex-compiler-query-read-form ((this repl-flex-compiler)
					     no-input-p)
  "Read a form, meaningful for the compiler, from the user."
  (let ((init (flex-compiler-eval-initial-at-point this)))
    (if no-input-p
	init
      (read-string "Form: " init 'flex-compiler-query-eval-form-history))))

(cl-defmethod flex-compiler-evaluate-form ((this repl-flex-compiler)
					   &optional form)
  "Return the evaluation form.

See the `:eval-form' slot."
  (if (and (null form) (not (slot-boundp this :form)))
      (flex-compiler-query-eval this nil))
  (let ((res (flex-compiler-eval-form-impl
	      this (or form (slot-value this 'form)))))
    (if (stringp res)
	res
      (prin1-to-string res))))

(provide 'flex-compile-repl)

;;; flex-compile-repl.el ends here
