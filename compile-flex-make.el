;;; compile-flex-make.el --- compile functions

;; Copyright (C) 2015 - 2017 Paul Landes

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

;; Implementation compiler for make(files)

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'compile-flex)
(require 'choice-program-complete)

(defcustom compile-flex-make-display-compile-buffer t
  "Whether or not to display the compile buffer when activated."
  :group 'compile-flex
  :type 'boolean)

(defun compile-flex-make-display-compile-buffer-toggle ()
  "Toggle variable `compile-flex-make-display-compile-buffer'."
  (interactive)
  (setq compile-flex-make-display-compile-buffer
	(not compile-flex-make-display-compile-buffer)))

(defvar compile-flex-make-target-history nil
  "History for makefile targets on compile.")

;;; make file compiler
(defclass make-flex-compiler (run-args-flex-compiler)
  ()
  :documentation "Invoke make on a configured makefile.")

(defmethod initialize-instance ((this make-flex-compiler) &rest rest)
  (oset this :name "make")
  (oset this :major-mode 'makefile-gmake-mode)
  (oset this :mode-desc "make")
  (oset this :config-file-desc "makefile")
  (apply 'call-next-method this rest))

(defmethod flex-compiler-load-libraries ((this make-flex-compiler))
  (require 'compile))

(defmethod flex-compiler-run-make ((this make-flex-compiler) &optional target)
  "Invoke a make compilation in an async inferior buffer.

This is done by creating a command with `make' found in the executable path."
  (let* ((makefile (flex-compiler-config this))
	 (dir (file-name-directory makefile))
	 (dir-switch (if dir (format "-C %s" dir)))
	 (command (concat "make -k " dir-switch " -f "
			  (file-name-nondirectory makefile)
			  (if target " ") target))
	 (process-environment (copy-tree process-environment)))
    (setenv "EMACS" "emacs")
    ;; ignore annoying 'A compilation process is running; kill it? (yes or no)'
    ;; in latex
    (ignore-errors
      (kill-compilation)
      (let ((kill-buffer-query-functions nil))
	(kill-buffer (get-buffer "*compilation*"))))
    (message "Compile command: %s" command)
    (if compile-flex-make-display-compile-buffer
	(compile command)
      (save-window-excursion (compile command)))))

(defmethod flex-compiler-makefile-targets ((this make-flex-compiler))
  (let* ((this (flex-compiler-by-name "make"))
	 (makefile (flex-compiler-config this))
	 (dir (file-name-directory makefile))
	 (targets))
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

(defmethod flex-compiler-run-with-args ((this make-flex-compiler) args)
  (flex-compiler-run-make this (car args)))

(defmethod flex-compiler-read-options ((this make-flex-compiler))
  (let ((targets (flex-compiler-makefile-targets this))
	(none "<none>"))
    (->> (choice-program-complete "Target" targets t nil nil
				  'compile-flex-make-target-history
				  none
				  nil nil t)
	 (funcall #'(lambda (elt)
		      (if (equal none elt) nil elt))))))

(defmethod flex-compiler-set-config ((this make-flex-compiler) &optional file)
  (with-slots (compile-options) this
    (setq compile-options nil))
  (call-next-method this file))

(defmethod flex-compiler-run ((this make-flex-compiler))
  (flex-compiler-run-make this "run"))

(defmethod flex-compiler-clean ((this make-flex-compiler))
  (flex-compiler-run-make this "clean"))

;; register the compiler
(flex-compile-manager-register the-flex-compile-manager
			       (make-flex-compiler nil))

(provide 'compile-flex-make)

;;; compile-flex-make.el ends here
