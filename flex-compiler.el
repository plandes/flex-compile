;;; flex-compiler.el --- User interactive interface to compilers  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2020 Paul Landes

;; Version: 0.7
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: compilation integration processes
;; URL: https://github.com/plandes/flex-compile
;; Package-Requires: ((emacs "26") (dash "2.13.0") (buffer-manage "0.8"))

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
;; a script or starting a `make' an async process is started.
;;
;; For more information see https://github.com/plandes/flex-compile

;;; Code:

(require 'flex-compile-manage)

(defvar flex-compiler-read-history nil
  "History variable for `flex-compiler-read'.")

(defun flex-compiler-by-name (name)
  "Convenience function to get a compiler by it's NAME."
  (config-manager-entry flex-compile-manage-inst name))

(defun flex-compiler-active ()
  "Return the currently activated compiler."
  (flex-compile-manager-active flex-compile-manage-inst))

(defun flex-compiler-read (last-compiler-p)
  "Read a flexible compiler to use.

LAST-COMPILER-P, if non-nil, use the last chosen compiler."
  (or (if last-compiler-p
	  (let ((arg (cl-second flex-compiler-read-history)))
	    (setq flex-compiler-read-history
		  (append (cons arg nil)
			  flex-compiler-read-history))
	    arg))
      (let* ((this flex-compile-manage-inst)
	     (names (config-manager-entry-names this)))
	(choice-program-complete
	 "Compiler" names t t nil
	 'flex-compiler-read-history
	 (cl-second flex-compiler-read-history)
	 nil t t))))

(defun flex-compiler-manage-read-form (no-input-p)
  "Read the compilation query form from the user.

If NO-INPUT-P is t, use the default witout getting it from the user.

This invokes the `flex-compiler-query-read-form method' on the
currently activated compiler."
  (let* ((mgr flex-compile-manage-inst)
	 (this (flex-compile-manager-active mgr)))
    (if (not (child-of-class-p (eieio-object-class this)
			       'repl-flex-compiler))
	(error "Compiler `%s' has no ability evaluation expressions"
	       (config-entry-name this))
      (flex-compile-manager-assert-ready mgr)
      (flex-compiler-query-read-form this no-input-p))))

;;;###autoload
(defun flex-compiler-config-save ()
  "Save all compiler and manager configuration."
  (interactive)
  (config-persistable-save flex-compile-manage-inst))

;;;###autoload
(defun flex-compiler-config-load ()
  "Load all compiler and manager configuration."
  (interactive)
  (config-persistable-load flex-compile-manage-inst))

;;;###autoload
(defun flex-compiler-list ()
  "Display the flex compiler list."
  (interactive)
  (let ((this flex-compile-manage-inst))
    (->> (format "*%s*" (capitalize (config-manager-name this)))
	 (config-manager-list-entries-buffer this))))

;;;###autoload
(defun flex-compiler-reset-configuration ()
  "Reset every compiler's configuration."
  (interactive)
  (when (y-or-n-p "This will wipe all compiler configuration.  Are you sure? ")
    (if (file-exists-p flex-compile-manage-persistency-file-name)
	(delete-file flex-compile-manage-persistency-file-name))
    (flex-compile-clear flex-compile-manage-inst)
    (flex-compiler-config-save)
    (message "Configuration reset")))

;;;###autoload
(defun flex-compiler-doc-show ()
  "Create markdown documentation on all compilers and their meta data."
  (interactive)
  (config-persistent-doc flex-compile-manage-inst))

;;;###autoload
(defun flex-compiler-show-configuration ()
  "Create a buffer with the configuration of the current compiler."
  (interactive)
  (let* ((this flex-compile-manage-inst)
	 (active (flex-compile-manager-active this)))
    (config-prop-entry-show-configuration active)))

;;;###autoload
(defun flex-compiler-do-activate (compiler-name)
  "Activate/select a compiler.

COMPILER-NAME the name of the compiler to activate."
  (interactive (list (flex-compiler-read current-prefix-arg)))
  (let ((this flex-compile-manage-inst))
    (config-manager-activate this compiler-name)))

;;;###autoload
(defun flex-compiler-do-compile (config-options)
  "Invoke compilation polymorphically.

CONFIG-OPTIONS, if non-nil invoke the configuration options for the compiler
before invoking the compilation.  By default CONFIG-OPTIONS is only detected
config-options by the \\[universal-argument] but some compilers use the numeric
argument as well.  This creates the need for an awkward key combination of:

  \\[digital-argument] \\[universal-argument] \\[flex-compiler-compile]

to invoke this command with full configuration support."
  (interactive
   (list (cond ((null current-prefix-arg) 'compile)
	       ;; universal arg
	       ((equal '(4) current-prefix-arg) nil)
	       (t (1- current-prefix-arg)))))
  (let* ((this flex-compile-manage-inst)
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
(defun flex-compiler-do-run-or-set-config (action)
  "This either invokes the `run' compilation functionality or it configures it.

ACTION is the interactive argument given by the read function."
  (interactive
   (list (cond ((null current-prefix-arg) 'run)
	       ((eq 2 current-prefix-arg) 'force-show)
	       ;; universal arg
	       ((equal '(4) current-prefix-arg) 'find)
	       ((eq 1 current-prefix-arg) 'set-config)
	       (t (error "Unknown prefix state: %s" current-prefix-arg)))))
  (let* ((this flex-compile-manage-inst)
	 (active (flex-compile-manager-active this)))
    (flex-compile-manager-assert-ready this)
    (condition-case err
	(cl-case action
	  (run (let (compile-def)
		 (let ((display-buffer-alist
			(flex-compiler-display-buffer-alist active)))
		   (setq compile-def (flex-compiler-run active)))
		 (flex-compiler-display-buffer active compile-def)))
	  (force-show (let (compile-def)
			(let ((display-buffer-alist
			       (flex-compiler-display-buffer-alist active)))
			  (setq compile-def (append (flex-compiler-run active)
						    '((force-show . t)))))
		 (flex-compiler-display-buffer active compile-def)))
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

;;;###autoload
(defun flex-compiler-do-eval (&optional form)
  "Evaluate the current form for the \(usually REPL based compiler).
FORM is the form to evaluate \(if implemented).  If called with
\\[universal-argument] then prompt the user with the from to evaluation."
  (interactive (list (flex-compiler-manage-read-form (not current-prefix-arg))))
  (let* ((mgr flex-compile-manage-inst)
	 (this (flex-compile-manager-active mgr)))
    (flex-compile-manager-assert-ready mgr)
    (let ((res (flex-compiler-evaluate-form this form)))
      (when (and res (called-interactively-p 'interactive))
	(kill-new res)
	(message "%s" res))
      res)))

;;;###autoload
(defun flex-compiler-do-clean ()
  "Invoke the clean functionality of the compiler."
  (interactive)
  (let* ((this flex-compile-manage-inst)
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
       (message "Compiler `%s' has no ability to clean"
		(config-entry-name active))))))

(provide 'flex-compiler)

;;; flex-compiler.el ends here
