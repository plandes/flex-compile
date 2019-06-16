;;; flex-compile-make.el --- compile functions

;; Copyright (C) 2015 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: make compile flexible

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
(require 'flex-compile-manage)
(require 'choice-program-complete)

(defvar flex-compile-make-target-history nil
  "History for makefile targets on compile.")

;;; make file compiler
(defclass make-flex-compiler (single-buffer-flex-compiler
			      conf-file-flex-compiler)
  ((target :initarg :target
	   :initform nil
	   :type (or null string)))
  :documentation "Invoke make on a configured makefile.")

(cl-defmethod initialize-instance ((this make-flex-compiler) &optional args)
  (let* ((fn #'(lambda (this compiler &rest args)
		 (flex-compiler-makefile-read compiler)))
	 (props (list (flex-conf-eval-prop :name 'target
					   :prompt "Target"
					   :func fn
					   :compiler this
					   :input-type 'last
					   :order 1))))
    (setq args (plist-put args :name "make")
	  args (plist-put args :description "Make")
	  args (plist-put args :validate-modes '(makefile-gmake-mode))
	  args (plist-put args :buffer-name "compilation")
	  args (plist-put args :kill-buffer-clean 2)
	  args (plist-put args :props (append (plist-get args :props) props))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this make-flex-compiler))
  (require 'compile))

(cl-defmethod flex-compiler-run-make ((this make-flex-compiler) &optional target)
  "Invoke a make compilation in an async inferior buffer.

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

(cl-defmethod flex-compiler-makefile-read ((this make-flex-compiler))
  (flex-compiler-set-required this)
  (let ((targets (flex-compiler-makefile-targets this))
	(none "<none>"))
    (->> (choice-program-complete "Target" targets t nil nil
				  'flex-compile-make-target-history
				  none
				  nil nil t)
	 (funcall #'(lambda (elt)
		      (if (equal none elt) nil elt))))))

(cl-defmethod flex-compiler-configure ((this make-flex-compiler)
				       config-options)
  (unless (eq config-options 'immediate)
    (setq config-options '(prop-name target)))
  (cl-call-next-method this config-options))

(cl-defmethod flex-compiler-start-buffer ((this make-flex-compiler)
					  start-type)
  (with-slots (target) this
    (cl-case start-type
      (compile (flex-compiler-run-make this target))
      (run (flex-compiler-run-make this "run"))
      (clean (flex-compiler-run-make this "clean")))))

;; register the compiler
(flex-compile-manager-register the-flex-compile-manager (make-flex-compiler))

(provide 'flex-compile-make)

;;; flex-compile-make.el ends here
