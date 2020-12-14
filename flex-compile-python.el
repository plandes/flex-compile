;;; flex-compile-python.el --- python compile functions

;; Copyright (C) 2015 - 2020 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: python integration compilation processes

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

;;; Implementation compiler for python integration

;;; Code:

(require 'subr-x)
(require 'flex-compile-manage)
(require 'flex-compile-repl)

(config-manage-declare-functions
 python-nav-backward-statement
 python-nav-forward-statement
 python-shell-calculate-command
 python-shell-completion-native-setup
 python-shell-parse-command
 python-shell-send-buffer
 python-shell-send-string
 run-python)
(config-manage-declare-variables
 python-shell-completion-native-enable)

(defun flex-compile-python-path ()
  "Return the PYTHONPATH environment to be used when creating the REPL."
  (getenv "PYTHONPATH"))

(defclass python-flex-compiler (repl-flex-compiler)
  ()
  :method-invocation-order :c3
  :documentation "\
This is a REPL based compiler that allows for evaluation Python buffers and
expressions using [python mode](https://github.com/fgallina/python.el).")

(cl-defmethod initialize-instance ((this python-flex-compiler) &optional slots)
  (setq slots (plist-put slots :object-name "python")
	slots (plist-put slots :validate-modes '(python-mode))
	slots (plist-put slots :repl-buffer-regexp "^\\*Python\\*$")
	slots (plist-put slots :repl-buffer-start-timeout 0))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this python-flex-compiler))
  (require 'python))

(cl-defmethod flex-compiler-eval-form-impl ((this python-flex-compiler) form)
  (python-shell-send-string form))

(cl-defmethod flex-compiler-start-buffer ((this python-flex-compiler)
					  start-type)
  (let (ret)
    (if (eq start-type 'compile)
	(if (flex-compiler-repl-running-p this)
	    (setq ret (cl-call-next-method this start-type))
	  (let ((do-native-p python-shell-completion-native-enable)
		(python-shell-completion-native-enable nil))
	    (flex-compiler-run this)
	    (if do-native-p
		(python-shell-completion-native-setup)))))
    (or ret (cl-call-next-method this start-type))))

(cl-defmethod flex-compiler-repl-compile ((this python-flex-compiler) file)
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (python-shell-send-buffer))))

(cl-defmethod flex-compiler-eval-initial-at-point ((this python-flex-compiler))
  (let ((forward-fn #'python-nav-forward-statement)
	(backward-fn #'python-nav-backward-statement))
    (save-excursion
      (->> (buffer-substring
	    (progn
	      (end-of-line 1)
	      (while (and (or (funcall backward-fn)
			      (beginning-of-line 1))
			  (> (current-indentation) 0)))
	      (forward-line 1)
	      (point-marker))
	    (progn
	      (or (funcall forward-fn)
		  (end-of-line 1))
	      (point-marker)))
	   string-trim))))

(cl-defmethod flex-compiler-repl-start ((this python-flex-compiler))
  (let ((old-path (getenv "PYTHONPATH")))
    (unwind-protect
	(let ((new-path (flex-compile-python-path)))
	  (setenv "PYTHONPATH" new-path)
	  (run-python (python-shell-calculate-command) nil 4))
      (setenv "PYTHONPATH" old-path))))

(flex-compile-manager-register flex-compile-manage-inst (python-flex-compiler))

(provide 'flex-compile-python)

;;; flex-compile-python.el ends here
