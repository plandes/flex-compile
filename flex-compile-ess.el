;;; flex-compile-ess.el --- ess compile functions

;; Copyright (C) 2015 - 2017 Paul Landes

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

(defclass ess-flex-compiler
  (config-flex-compiler repl-flex-compiler single-buffer-flex-compiler)
  ())

(cl-defmethod initialize-instance ((this ess-flex-compiler) &optional args)
  (oset this :name "ess")
  (oset this :major-mode 'ess-mode)
  (oset this :mode-desc "ess")
  (oset this :config-file-desc "ess file")
  (oset this :repl-buffer-regexp "^\\*R\\*$")
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this ess-flex-compiler))
  (require 'ess-site))

(cl-defmethod flex-compiler-repl-start ((this ess-flex-compiler))
  (let ((ess-ask-for-ess-directory nil))
    (with-current-buffer (flex-compiler-config-buffer this)
      (R))))

(cl-defmethod flex-compiler-repl-compile-source ((this ess-flex-compiler))
  (let ((file (flex-compiler-config this)))
    (flex-compiler-run-command this (format "source('%s')" file))))

(cl-defmethod flex-compiler-display-buffer-alist ((this ess-flex-compiler))
  "Return default nil, otherwise prompt reading doesn't play well
with `display-buffer'."
  nil)

(cl-defmethod flex-compiler-repl-compile ((this ess-flex-compiler))
  (let ((buf (flex-compiler-config-buffer this)))
    (with-current-buffer buf
      (ess-eval-region (point-min) (point-max) nil))))

(flex-compile-manager-register the-flex-compile-manager (ess-flex-compiler))

(provide 'flex-compile-ess)

;;; flex-compile-ess.el ends here
