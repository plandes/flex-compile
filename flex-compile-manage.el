;;; flex-compile-manage.el --- manager for flexible compilers

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

;; This file contains the manager object and all requires for concrete compiler
;; implementations.  For this reason, each concreate compiler should need to
;; only require this library for implementations.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'eieio)
(require 'eieio-base)
(require 'dash)
(require 'time-stamp)
(require 'config-manage)
(require 'choice-program-complete)
(require 'flex-compile-base)
(require 'flex-compile-config)
(require 'flex-compile-single-buffer)
(require 'flex-compile-repl)

;;; compiler manager/orchestration
(defclass flex-compile-manager (config-manager config-persistable)
  ()
  :documentation "Manages flexible compiler instances.")

(cl-defmethod initialize-instance ((this flex-compile-manager) &optional args)
  (with-slots (entries) this
    (setq entries (list (no-op-flex-compiler nil))))
  (cl-call-next-method this args))

(cl-defmethod config-persistable-load ((this flex-compile-manager))
  (with-slots (entries) this
    (let ((old-entries entries)
	  (old-emap (mapcar #'(lambda (entry)
				(cons (config-entry-name entry) entry))
			    entries))
	  new-entries)
      (cl-call-next-method this)
      (let ((new-emap (mapcar #'(lambda (entry)
				  (cons (config-entry-name entry) entry))
			      entries)))
	(setq entries
	      (remove nil
		      (mapcar #'(lambda (entry)
				  (let* ((name (config-entry-name entry))
					 (new-entry (cdr (assoc name new-emap))))
				    (if new-entry
					(if (assoc name old-emap)
					    new-entry)
				      entry)))
			      old-entries))))
      (dolist (compiler entries)
	;; since all compilers are persistable (via `config-entry inhertiance)
	;; set the manager so we can save the state
	(setf (slot-value compiler 'manager) this)))))

(cl-defmethod config-manager-entry-default-name ((this flex-compile-manager))
  "flexible-compiler")

(cl-defmethod flex-compile-manager-register ((this flex-compile-manager)
					     compiler)
  "Register a compiler instance with the manager \(compilation framework)."
  (with-slots (entries) this
    (setq entries
	  (cl-delete compiler entries
		     :test #'(lambda (a b)
			       (equal (config-entry-name a)
				      (config-entry-name b)))))
    (setq entries (append entries (cons compiler nil)))
    (setf (slot-value compiler 'manager) this)
    (message "Registered %s compiler" (config-entry-name compiler))))

(cl-defmethod config-manager-entry-names ((this flex-compile-manager))
  "Return the names of all registered compilers."
  (with-slots (entries) this
    (mapcar #'config-entry-name entries)))

(cl-defmethod flex-compile-manager-active ((this flex-compile-manager))
  "Return the currently selected or active manager."
  (car (slot-value this 'entries)))

(cl-defmethod config-manager-activate ((this flex-compile-manager) name)
  (let ((compiler (cl-call-next-method this name)))
    (message "Active compiler is now %s" (config-entry-name compiler))
    compiler))

(cl-defmethod flex-compile-manager-assert-ready ((this flex-compile-manager))
  "Make sure the active/selected compiler is ready and libraries loaded."
  (let ((active (flex-compile-manager-active this)))
    (flex-compiler-load-libraries active)))

(cl-defmethod flex-compile-clear ((this flex-compile-manager))
  "Clear all compiler's state.
This is done by simply re-instantiating all current registered compilers."
  (let ((entries (slot-value this 'entries)))
    (setf (slot-value this 'entries) nil)
    (dolist (compiler entries)
      (->> (eieio-object-class compiler)
	   funcall
	   (flex-compile-manager-register this)))))

(cl-defmethod config-prop-doc ((this flex-compile-manager) level)
  "Create markdown documentation on all compilers and their meta data.
STREAM is where the output of the documentation goes."
  (insert (format "%s Compilers\n" (make-string level ?#)))
  (dolist (compiler (config-manager--entries this nil nil 'lexical))
    (unless (equal (config-entry-name compiler) "disable")
      (flex-compile-doc compiler (1+ level)))))



;; library configuration
(defvar the-flex-compile-manager
  (flex-compile-manager)
  "The singleton manager instance.")

(defcustom flex-compile-persistency-file-name
  (expand-file-name "flex-compile" user-emacs-directory)
  "File containing the Flex compile configuration data."
  :type 'file
  :group 'flex-compile
  :set (lambda (sym val)
	 (set-default sym val)
	 (if (and (boundp 'the-flex-compile-manager)
		  the-flex-compile-manager)
	     (setf (slot-value the-flex-compile-manager 'file) val))))


;; functions
(defun flex-compiler-by-name (name)
  "Convenience function to get a compiler by it's NAME."
  (config-manager-entry the-flex-compile-manager name))

;;;###autoload
(defun flex-compiler-config-save ()
  "Save all compiler and manager configuration."
  (interactive)
  (config-persistable-save the-flex-compile-manager))

;;;###autoload
(defun flex-compiler-config-load ()
  "Load all compiler and manager configuration."
  (interactive)
  (config-persistable-load the-flex-compile-manager))

;;;###autoload
(defun flex-compiler-list ()
  "Display the flex compiler list."
  (interactive)
  (config-manager-list-entries-buffer the-flex-compile-manager))

(defun flex-compiler-reset-configuration ()
  "Reset every compiler's configuration."
  (interactive)
  (when (y-or-n-p "This will wipe all compiler configuration.  Are you sure? ")
    (if (file-exists-p flex-compile-persistency-file-name)
	(delete-file flex-compile-persistency-file-name))
    (flex-compile-clear the-flex-compile-manager)
    (flex-compiler-config-save)
    (message "Configuration reset")))



;;; interactive functions
(defvar flex-compiler-read-history nil)

(defun flex-compiler-read (last-compiler-p)
  "Read a flexible compiler to use.

LAST-COMPILER-P, if non-nil, use the last chosen compiler."
  (or (if last-compiler-p
	  (let ((arg (cl-second flex-compiler-read-history)))
	    (setq flex-compiler-read-history
		  (append (cons arg nil) flex-compiler-read-history))
	    arg))
      (let* ((this the-flex-compile-manager)
	     (names (config-manager-entry-names this)))
	(choice-program-complete "Compiler" names t t nil
				 'flex-compiler-read-history
				 (cl-second flex-compiler-read-history)
				 nil t t))))

;;;###autoload
(defun flex-compiler-activate (compiler-name)
  "Activate/select a compiler.

COMPILER-NAME the name of the compiler to activate."
  (interactive (list (flex-compiler-read current-prefix-arg)))
  (let ((this the-flex-compile-manager))
    (config-manager-activate this compiler-name)))

;;;###autoload
(defun flex-compile-compile (config-options)
  "Invoke compilation polymorphically.

CONFIG-OPTIONS, if non-nil invoke the configuration options for the compiler
before invoking the compilation.  By default CONFIG-OPTIONS is only detected
config-options by the \\[universal-argument] but some compilers use the numeric
argument as well.  This creates the need for an awkward key combination of:

  \\[digital-argument] \\[universal-argument] \\[flex-compile-compile]

to invoke this command with full configuration support."
  (interactive
   (list (cond ((null current-prefix-arg) 'compile)
	       ;; universal arg
	       ((equal '(4) current-prefix-arg) nil)
	       (t (1- current-prefix-arg)))))
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (flex-compile-manager-assert-ready this)
    (if (eq config-options 'compile)
	(let (comp-def)
	  (let ((display-buffer-alist
		 (flex-compiler-display-buffer-alist active)))
	    (setq comp-def (flex-compiler-compile active)))
	  (flex-compiler-display-buffer active comp-def))
      (condition-case nil
	  (config-prop-entry-configure active config-options)
	(cl-no-applicable-method
	 (message "Compiler %s is not configurable"
		  (config-entry-name active)))))))

;;;###autoload
(defun flex-compile-run-or-set-config (action)
  "This either invokes the `run' compilation functionality or it configures it.

ACTION is the interactive argument given by the read function."
  (interactive
   (list (cond ((null current-prefix-arg) 'run)
	       ;; universal arg
	       ((equal '(4) current-prefix-arg) 'find)
	       ((eq 1 current-prefix-arg) 'set-config)
	       (t (error "Unknown prefix state: %s" current-prefix-arg)))))
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (flex-compile-manager-assert-ready this)
    (condition-case err
	(cl-case action
	  (run (let (buf)
		 (let ((display-buffer-alist
			(flex-compiler-display-buffer-alist active)))
		   (setq buf (flex-compiler-run active)))
		 (flex-compiler-display-buffer active buf)))
	  (find (if (child-of-class-p (eieio-object-class active)
				      'conf-file-flex-compiler)
		    (flex-compiler-conf-file-display active)))
	  (set-config (config-prop-entry-configure active 'immediate))
	  (t (error "Unknown action: %S" action)))
      (cl-no-applicable-method
       (message "Unsupported action `%S' on compiler %s: no method %S"
		action
		(config-entry-name active)
		(cl-second err))))))

(defun flex-compile-read-form (no-input-p)
  "Read the compilation query form from the user.

If NO-INPUT-P is t, use the default witout getting it from the user.

This invokes the `flex-compiler-query-read-form method' on the
currently activated compiler."
  (let* ((mgr the-flex-compile-manager)
	 (this (flex-compile-manager-active mgr)))
    (flex-compile-manager-assert-ready mgr)
    (flex-compiler-query-read-form this no-input-p)))

;;;###autoload
(defun flex-compile-eval (&optional form)
  "Evaluate the current form for the \(usually REPL based compiler).
FORM is the form to evaluate \(if implemented).  If called with
\\[universal-argument] then prompt the user with the from to evaluation."
  (interactive (list (flex-compile-read-form (not current-prefix-arg))))
  (let* ((mgr the-flex-compile-manager)
	 (this (flex-compile-manager-active mgr)))
    (flex-compile-manager-assert-ready mgr)
    (let ((res (flex-compiler-evaluate-form this form)))
      (when (and res (called-interactively-p 'interactive))
	(kill-new res)
	(message "%s" res))
      res)))

;;;###autoload
(defun flex-compile-clean ()
  "Invoke the clean functionality of the compiler."
  (interactive)
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (condition-case nil
	(progn
	  (flex-compile-manager-assert-ready this)
	  (let (compile-def)
	   (let ((display-buffer-alist
		  (flex-compiler-display-buffer-alist active)))
	     (setq compile-def (flex-compiler-clean active)))
	   (flex-compiler-display-buffer active compile-def)))
      (cl-no-applicable-method
       (message "Compiler %s has no ability to clean"
		(config-entry-name active))))))

(defun flex-compile-doc-show ()
  "Create markdown documentation on all compilers and their meta data."
  (interactive)
  (let ((buf (get-buffer-create "*Compiler Documentation*")))
    (with-current-buffer buf
      (erase-buffer)
      (config-prop-doc the-flex-compile-manager 2)
      (and (fboundp 'markdown-mode) (markdown-mode)))
    (display-buffer buf)
    buf))

;;;###autoload
(defun flex-compile-show-configuration ()
  "Create a buffer with the configuration of the current compiler."
  (interactive)
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (config-prop-entry-show-configuration active)))

(provide 'flex-compile-manage)

;;; flex-compile-manage.el ends here
