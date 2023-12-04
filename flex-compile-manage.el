;;; flex-compile-manage.el --- Manager for flexible compilers  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2023 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: compilation integration processes
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

;;; compiler shared configuration
(defcustom flex-compile-manage-load-libraries-entry 'activate
  "When to load the compiler's libraries.

This invokes a method that `require's all the libraries needed for the compiler
to run."
  :type '(choice (const :tag "When the compiler is activated" activate)
		 (const :tag "When the compiler is accessed" assert-ready))
  :group 'flex-compile)

;;; compiler manager/orchestration
(defclass flex-compile-manager (config-manager config-persistable)
  ()
  :documentation "\
Concrete instances of *flexible* compilers that provide a common interface.
Each is an implementation of glue code to the respective compilation method.

Note that all compilers that extend from `conf-file-flex-compiler', which
include `make', `script', `xml-validate', `org-mode', `python', `clojure', and
`ess' have their `start-directory' property unset each time the `config-file'
is set.")

(cl-defmethod initialize-instance ((this flex-compile-manager) &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (with-slots (entries) this
    (setq entries (list (no-op-flex-compiler nil))))
  (cl-call-next-method this slots))

(cl-defmethod config-persistable-load ((this flex-compile-manager))
  "Restore the state of THIS instance persistable object."
  (with-slots (entries) this
    (let ((old-entries entries)
	  (old-emap (mapcar #'(lambda (entry)
				(cons (config-entry-name entry) entry))
			    entries)))
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
  "Return `flexible-compiler'.
THIS is the object instance."
  (ignore this)
  "flexible-compiler")

(cl-defmethod flex-compile-manager-register ((this flex-compile-manager)
					     compiler)
  "Register COMPILER instance with the manager \(compilation framework).
THIS is the object instance."
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
  "Return the names of all registered compilers.
THIS is the object instance."
  (with-slots (entries) this
    (mapcar #'config-entry-name entries)))

(cl-defmethod flex-compile-manager-active ((this flex-compile-manager))
  "Return the currently selected or active manager.
THIS is the object instance."
  (car (slot-value this 'entries)))

(cl-defmethod config-manager-activate ((this flex-compile-manager) criteria)
  "Switch to a `flex-compiler’ in THIS manager.

CRITERIA, see the `config-manager’ method ‘config-manager-activate’."
  (let ((compiler (cl-call-next-method this criteria)))
    (message "Active compiler is now %s" (config-entry-name compiler))
    (when (eq flex-compile-manage-load-libraries-entry 'activate)
      (flex-compiler-load-libraries compiler))
    compiler))

(cl-defmethod flex-compile-manager-assert-ready ((this flex-compile-manager))
  "Make sure the active/selected compiler is ready and libraries loaded.
THIS is the object instance."
  (let ((active (flex-compile-manager-active this)))
    (when (eq flex-compile-manage-load-libraries-entry 'assert-ready)
      (flex-compiler-load-libraries active))))

(cl-defmethod config-manager-remove-entry ((this flex-compile-manager) entry)
  "Disallow ENTRY deletion since it is nonsensical for this implementation.
THIS is the object instance."
  (ignore this entry)
  (config-persistent--unimplemented this "remove-entry"))

(cl-defmethod flex-compile-clear ((this flex-compile-manager))
  "Clear all compiler's state.
This is done by simply re-instantiating all current registered compilers.
THIS is the object instance."
  (let ((entries (slot-value this 'entries)))
    (setf (slot-value this 'entries) nil)
    (dolist (compiler entries)
      (->> (eieio-object-class compiler)
	   funcall
	   (flex-compile-manager-register this)))))

;; library configuration
(defvar flex-compile-manage-inst
  (flex-compile-manager :object-name "compiler")
  "The singleton manager instance.")

(defcustom flex-compile-manage-persistency-file-name
  (expand-file-name "flex-compile" user-emacs-directory)
  "File containing the Flex compile configuration data."
  :type 'file
  :group 'flex-compile
  :set (lambda (sym val)
	 (set-default sym val)
	 (if (and (boundp 'flex-compile-manage-inst)
		  flex-compile-manage-inst)
	     (setf (slot-value flex-compile-manage-inst 'file) val))))

(provide 'flex-compile-manage)

;;; flex-compile-manage.el ends here
