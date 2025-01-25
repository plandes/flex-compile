;;; flex-compile-lisp.el --- Lisp compile functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2025 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: lisp integration compilation processes
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

;;; Implementation compiler for Lisp (Emacs slime) integration

;;; Code:

(require 'flex-compile-manage)
(require 'flex-compile-repl)

(config-manage-declare-functions
 slime
 slime-setup
 slime-interactive-eval
 slime-load-file
 slime-compile-and-load-file
 slime-last-expression
 slime-repl-clear-buffer
 slime-quit-lisp)

(defvar flex-compile-lisp-window-context nil
  "Used to restore windows in `flex-compile-lisp-connected'.")

(defclass lisp-flex-compiler (repl-flex-compiler)
  ((compile-on-load :initarg :compile-on-load
		    :initform nil
		    :type boolean
		    :documentation "\
Whether to also compile when loading the source file.")
   (mode :initarg :mode
	 :initform 'file
	 :type symbol
	 :documentation "\
Whether to run the test suite using the `slite' library (must be installed)
instead of evaluate the `config-file'.  Available at:
https://github.com/tdrhq/slite")
   (test-file :initarg :test-file
	      :initform nil
	      :type (or null string)))
  :method-invocation-order :c3
  :documentation "\
This is a REPL based compiler that allows for evaluation Lisp buffers and
expressions using [slime](https://github.com/slime/slime).")

(cl-defmethod initialize-instance ((this lisp-flex-compiler) &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (let ((props
	 (list
	  (config-boolean-prop :object-name 'compile-on-load
			       :prop-entry this
			       :prompt "Compile when loading"
			       :input-type 'toggle)
	  (config-file-prop :object-name 'test-file
			    :prompt "Test file"
			    :prop-entry this
			    :validate-modes (plist-get slots :validate-modes)
			    :input-type 'last
			    :required nil
			    :order 0)
	  (config-choice-prop :object-name 'mode
			      :prompt "Run tests"
			      :prop-entry this
			      :input-type 'toggle
			      :choices '(file test)))))
    (setq slots (plist-put slots :object-name "lisp")
	  slots (plist-put slots :validate-modes '(lisp-mode))
	  slots (plist-put slots :repl-buffer-regexp "^\\**slime-repl .+\\*$")
	  slots (plist-put slots :derived-buffer-names '("*inferior-lisp*"))
	  slots (plist-put slots :repl-buffer-start-timeout 0)
	  slots (plist-put slots
			   :props (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this lisp-flex-compiler))
  "Load the `slime' library for THIS compiler."
  (ignore this)
  (require 'slime)
  (condition-case nil
      (require 'slite)
    (error
     (message "Package `slite' is not found--skipping test functionality")))
  (slime-setup))

(cl-defmethod flex-compiler-eval-form-impl ((this lisp-flex-compiler) form)
  "Evaluate the FORM and return the response of the REPL for THIS compiler."
  (ignore this)
  (slime-interactive-eval form))

(cl-defmethod flex-compiler-repl-compile ((this lisp-flex-compiler) file)
  "Send the contents of FILE to the Slime REPL buffer of THIS compiler."
  (ignore this)
  (with-slots (compile-on-load) this
    (save-excursion
      (apply #'set-buffer (list (find-file-noselect file)))
      (if compile-on-load
	  (slime-compile-and-load-file)
	(slime-load-file file)))))

(cl-defmethod flex-compiler-eval-initial-at-point ((this lisp-flex-compiler))
  "Return the Lisp form at the current point to the REPL for THIS compiler."
  (ignore this)
  (slime-last-expression))

(cl-defmethod flex-compiler-clear-buffer ((this lisp-flex-compiler))
  "Clear THIS compiler's REPL buffer."
  (with-current-buffer (flex-compiler-buffer this)
    ;; temporarily redefine `recenter' to avoid current window warnings
    (cl-letf (((symbol-function 'recenter)
	       (lambda (&rest args)
		 (ignore args)
		 (goto-char (point-max)))))
      (slime-repl-clear-buffer))))

(cl-defmethod flex-compiler-repl-start ((this lisp-flex-compiler))
  "Start the REPL using THIS compiler."
  (ignore this)
  (setq flex-compile-lisp-window-context
	`((this . ,this)
	  (win-cfg . ,(current-window-configuration))))
  (slime))

(cl-defmethod flex-compiler-start-buffer ((this lisp-flex-compiler)
					  start-type)
  "Return a new buffer for THIS compiler with a processing compilation.
START-TYPE is either symbols `compile', `run', `clean' depending
if invoked by `flex-compiler-compile' or `flex-compiler-run'."
  (with-slots (mode test-file) this
    (let ((needs-run-p (and (eq start-type 'compile)
			    (not (flex-compiler-repl-running-p this))))
	  (ret (cl-call-next-method this start-type)))
      ;; tell `flex-compile-lisp-connected' to invoke compilation after the
      ;; REPL has started if started as a compile rather than a run
      (if needs-run-p
	  (setq flex-compile-lisp-window-context
		(append flex-compile-lisp-window-context
			'((needs-compile-p . t)))))
      ret)))

(cl-defmethod flex-compiler-single-buffer--flex-comp-def
  ((this lisp-flex-compiler) start-type startp)
"Return a default compilation definition for THIS compiler.

START-TYPE is either symbols `compile', `run', `clean' depending if invoked by
`flex-compiler-compile' or `flex-compiler-run'.

If STARTP is non-nil, start the buffer using `flex-compiler-start-buffer'.

Return an alist in the following form:
  ((newp . <t if the buffer is new and jsut created, nil otherwise>
   (buffer . <the of single buffer object>)))"
  (with-slots (mode test-file) this
    ;; override for testing functionality
    (if (and (eq start-type 'compile) (eq mode 'test))
	(when (fboundp 'slite-run)
	  (message "Running tests...")
	  (unless test-file
	    (let ((prop (config-prop-by-name this 'test-file)))
	      (config-prop-set this prop (config-prop-read prop))))
	  (flex-compiler-evaluate-form this (format "(load \"%s\")" test-file))
	  (call-interactively 'slite-run)
	  '((newp . nil)
	    (buffer . nil))))
    (cl-call-next-method this start-type startp)))

(cl-defmethod flex-compiler-kill-repl ((this lisp-flex-compiler))
  "Use `cider-quit' to stop the Cider REPL for THIS compiler."
  (condition-case err
      (slime-quit-lisp t)
    (error "Warning: %S" err))
  (dolist (buf (buffer-list))
    (when (string-match "^\\**\\(?:slime-\\|sldb \\).+\\*$" (buffer-name buf))
      (message "killing buffer %S" buf)
      (flex-compiler--kill-buffer this buf)))
  (cl-call-next-method this))

(defun flex-compile-lisp-connected ()
  "Called by `slime-connected-hook' after the REPL has started.
Because slime pops a new buffer after the REPL starts, the default buffer
display logic isn't called after `flex-compiler-repl-start'.  This is called
by `slime-connected-hook' to execute the default buffer display behavior by
calling `flex-compiler-display-buffer'."
  (unwind-protect
      (let* ((this (cdr (assq 'this flex-compile-lisp-window-context)))
	     (cfg (cdr (assq 'win-cfg flex-compile-lisp-window-context)))
	     (needs-compile-p (cdr (assq 'needs-compile-p
					 flex-compile-lisp-window-context)))
	     (compile-def `((newp . t)
			    (buffer . ,(flex-compiler-buffer this)))))
	(set-window-configuration cfg)
	(flex-compiler-display-buffer this compile-def)
	(when needs-compile-p
	  ;; put any output on a separate (new)line
	  (flex-compiler-eval-form-impl this "(format t \"~%\")")
	  (with-slots (config-file) this
	    (flex-compiler-repl-compile this config-file))))
    (setq flex-compile-lisp-window-context nil)))

(add-hook 'slime-connected-hook #'flex-compile-lisp-connected 100)

(flex-compile-manager-register flex-compile-manage-inst (lisp-flex-compiler))

(provide 'flex-compile-lisp)

;;; flex-compile-lisp.el ends here
