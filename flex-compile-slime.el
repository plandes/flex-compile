;;; flex-compile-slime.el --- Lisp compile functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2021 Paul Landes

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
 slime-interactive-eval
 slime-compile-and-load-file
 slime-last-expression
 slime-repl-clear-buffer
 slime-quit-lisp)

(defclass slime-flex-compiler (repl-flex-compiler)
  ()
  :method-invocation-order :c3
  :documentation "\
This is a REPL based compiler that allows for evaluation Lisp buffers and
expressions using [slime](https://github.com/slime/slime).")

(cl-defmethod initialize-instance ((this slime-flex-compiler) &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (setq slots (plist-put slots :object-name "slime")
	slots (plist-put slots :validate-modes '(lisp-mode))
	slots (plist-put slots :repl-buffer-regexp "^\\**slime-repl .+\\*$")
	slots (plist-put slots :repl-buffer-start-timeout 0))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this slime-flex-compiler))
  "Load the `slime' library for THIS compiler."
  (ignore this)
  (require 'slime))

(cl-defmethod flex-compiler-eval-form-impl ((this slime-flex-compiler) form)
  "Evaluate the FORM and return the response of the REPL for THIS compiler."
  (ignore this)
  (slime-interactive-eval form))

(cl-defmethod flex-compiler-repl-compile ((this slime-flex-compiler) file)
  "Send the contents of FILE to the Slime REPL buffer of THIS compiler."
  (ignore this)
  (save-excursion
    (apply #'set-buffer (list (find-file-noselect file)))
    (slime-compile-and-load-file)))

(cl-defmethod flex-compiler-eval-initial-at-point ((this slime-flex-compiler))
  "Return the Lisp form at the current point to the REPL for THIS compiler."
  (ignore this)
  (slime-last-expression))

(cl-defmethod flex-compiler-clear-buffer ((this slime-flex-compiler))
  "Clear THIS compiler's REPL buffer."
  (with-current-buffer (flex-compiler-buffer this)
    ;; temporarily redefine `recenter' to avoid current window warnings
    (cl-letf (((symbol-function 'recenter)
	       (lambda (&rest args)
		 (ignore args)
		 (goto-char (point-max)))))
      (slime-repl-clear-buffer))))

(cl-defmethod flex-compiler-repl-start ((this slime-flex-compiler))
  "Start the REPL using THIS compiler."
  (ignore this)
  (slime))

(cl-defmethod flex-compiler-kill-repl ((this slime-flex-compiler))
  "Use `cider-quit' to stop the Cider REPL for THIS compiler."
  (condition-case err
      (progn
	(slime-quit-lisp t)
	(dolist (buf-name '("*inferior-lisp*" "*slime-events*"))
	  (let ((buf (get-buffer buf-name)))
	    (flex-compiler--kill-buffer this buf))))
    (error "Warning: %S" err))
  (cl-call-next-method this))

(flex-compile-manager-register flex-compile-manage-inst (slime-flex-compiler))

(provide 'flex-compile-slime)

;;; flex-compile-slime.el ends here
