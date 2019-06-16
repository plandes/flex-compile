;;; flex-compile-ess.el --- ess compile functions

;; Copyright (C) 2015 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: ess Emacs Speaks Statistics compilation

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

;; Implementation compiler for ess integration

;;; Code:

(require 'flex-compile-manage)

(flex-compile-declare-functions
 ess-eval-region R)

(defclass ess-flex-compiler (repl-flex-compiler)
  ()
  :method-invocation-order :c3
  :documentation "\
This is a REPL based compiler to evaluate R code with
[Emacs Speaks Statistics](https://ess.r-project.org) .")

(cl-defmethod initialize-instance ((this ess-flex-compiler) &optional args)
  (setq args (plist-put args :object-name "ess")
	args (plist-put args :description "Emacs speaks statistics")
	args (plist-put args :validate-modes '(ess-r-mode))
	args (plist-put args :repl-buffer-regexp "^\\*R:.+\\*$")
	args (plist-put args :repl-buffer-start-timeout 5))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this ess-flex-compiler))
  (require 'ess-site))

(cl-defmethod flex-compiler-repl-start ((this ess-flex-compiler))
  (let ((ess-ask-for-ess-directory nil))
    (with-current-buffer (find-file-noselect (slot-value this 'config-file))
      (R))))

(cl-defmethod flex-compiler-repl-compile ((this ess-flex-compiler) file)
  (with-current-buffer (find-file-noselect file)
    (ess-eval-region (point-min) (point-max) nil)))

(flex-compile-manager-register the-flex-compile-manager (ess-flex-compiler))

(provide 'flex-compile-ess)

;;; flex-compile-ess.el ends here
