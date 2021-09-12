;;; flex-compile-make.el --- Compile functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2021 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: make compile flexible processes
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

;; Implementation compiler for make(files).
;; Not customizing `compilation-always-kill' to t will result in windows
;; disappearing on a compilation interruption.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'compile)
(require 'choice-program-complete)
(require 'flex-compile-manage)

;;; make file compiler
(defclass make-flex-compiler (single-buffer-flex-compiler
			      conf-file-flex-compiler)
  ((target :initarg :target
	   :initform nil
	   :type (or null string)
	   :documentation "The make file target to satisfy.")
   (run-target :initarg :run-target
	       :initform nil
	       :type (or null string)
	       :documentation "\
The target used to run or test as the secondary compilation functionality."))
  ;; see `flex-compiler::config-persistent-doc'
  :method-invocation-order :c3
  :documentation "\
This compiler invokes make as an asynchronous process in a
buffer.  The first target, `run' target, and `clean' target are
invoked respectfully with *compile*, *run* and *clean* Emacs
commands (see [usage](#usage)).

This is a special compiler in it's configuration.  Instead of
setting properties, the default configuration mechanism is to set
the make target instead.  If you want to set a flex compiler
property, use `\\C-u 0 \\C-u'.

When setting the configuration file the target property is unset.")

(cl-defmethod initialize-instance ((this make-flex-compiler) &optional slots)
  "Initialize THIS instance using SLOTS as initial values."
  (let* ((fn #'(lambda (this compiler &rest slots)
		 (ignore slots)
		 (flex-compiler-makefile-read compiler this)))
	 (props (list (config-eval-prop :object-name 'target
					:prompt "Target"
					:func fn
					:prop-entry this
					:input-type 'last
					:order 1)
		      (config-eval-prop :object-name 'run-target
					:prompt "Run target"
					:func fn
					:prop-entry this
					:input-type 'last
					:order 2))))
    (setq slots (plist-put slots :object-name "make")
	  slots (plist-put slots :description "Make")
	  slots (plist-put slots :validate-modes '(makefile-gmake-mode))
	  slots (plist-put slots :buffer-name "compilation")
	  slots (plist-put slots :kill-buffer-clean nil)
	  slots (plist-put slots
			   :props (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this make-flex-compiler))
  "Load library `compile' for THIS compiler."
  (ignore this)
  (require 'compile))

(cl-defmethod flex-compiler-run-make ((this make-flex-compiler)
				      &optional target)
  "Invoke a make compilation in an async inferior buffer for THIS compiler.
TARGET is the make file target to run/satisfy.
This is done by creating a command with `make' found in the executable path."
  (let* ((makefile (slot-value this 'config-file))
	 (dir (file-name-directory makefile))
	 (dir-switch (if dir (format "-C %s" dir)))
	 (command (concat "make -k " dir-switch " -f "
			  (file-name-nondirectory makefile)
			  (if target " ") target)))
    (setenv "EMACS" "emacs")
    ;; ignore annoying 'A compilation process is running; kill it? (yes or no)'
    ;; in latex override code eliminated in favor of `compilation-always-kill'
    (message "Compile command: %s" command)
    (compile command)))

(cl-defmethod flex-compiler-makefile-targets ((this make-flex-compiler))
  "Return all targets supported by the set makefile on THIS compiler."
  (let* ((makefile (slot-value this 'config-file))
	 (dir (file-name-directory makefile))
	 targets)
    (with-temp-buffer
      (insert (shell-command-to-string (format "make -prRn -C %s" dir)))
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-zA-Z0-9-]+\\):" nil t)
	(setq targets
	      (->> (match-string 1)
		   substring-no-properties
		   list
		   (append targets)))))
    (->> targets
	 (cl-remove-if #'(lambda (elt)
			   (member elt '("run" "clean")))))))

(cl-defmethod flex-compiler-makefile-read ((this make-flex-compiler) prop)
  "Read target used invoke a make compilation using THIS compiler.

PROP is the `config-eval-prop' instance used to configure the run target.

See the class's `initialize-instance', which is how this method is gets
invoked."
  (config-prop-entry-set-required this)
  (let ((targets (flex-compiler-makefile-targets this))
	(history (slot-value prop 'history))
	(none "<none>"))
    (->> (choice-program-complete
	  "Target" targets t nil nil history none nil nil t)
	 (funcall #'(lambda (elt)
		      (if (equal none elt) nil elt))))))

(cl-defmethod config-prop-set ((this make-flex-compiler) prop val)
  "Set property PROP to VAL on THIS compiler.

This resets the target when changing the file."
  (when (eq (config-prop-name prop) 'config-file)
    (setf (slot-value this 'target) nil))
  (cl-call-next-method this prop val))

(cl-defmethod config-prop-entry-configure ((this make-flex-compiler)
					   config-options)
  "Special behavior when configuring THIS compiler.

CONFIG-OPTIONS:
  - when -1 (using \\[universal-argument] with 0) prompt for the target
  - when nil short cut to setting the make target"
  (->> (cond ((eq config-options -1) nil) ; unversal arg with 0
	     ;; shortcut to setting the make target
	     ((null config-options) '(prop-name target))
	     (t config-options))
       (cl-call-next-method this)))

(cl-defmethod flex-compiler-start-buffer ((this make-flex-compiler)
					  start-type)
  "Return a new buffer for THIS compiler with a processing compilation.
See the `single-buffer-flex-compiler' implementation of
`flex-compiler-start-buffer' for more information and START-TYPE."
  (with-slots (target run-target start-directory) this
    (let ((default-directory start-directory))
      (cl-case start-type
	(compile (flex-compiler-run-make this target))
	(run (flex-compiler-run-make this (or run-target "run")))
	(clean (flex-compiler-run-make this "clean"))))))

;; register the compiler
(flex-compile-manager-register flex-compile-manage-inst (make-flex-compiler))

(provide 'flex-compile-make)

;;; flex-compile-make.el ends here
