;;; flex-compile-python.el --- python compile functions

;; Copyright (C) 2015 - 2017 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: python integration compilation

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

(require 'flex-compile-manage)

(flex-compile-declare
 python-shell-send-string
 python-shell-send-buffer
 python-shell-parse-command
 run-python)

(defclass python-flex-compiler (evaluate-flex-compiler) ())

(cl-defmethod initialize-instance ((this python-flex-compiler) &optional args)
  (oset this :name "python")
  (oset this :major-mode 'python-mode)
  (oset this :mode-desc "python file")
  (oset this :config-file-desc "python file")
  (oset this :repl-buffer-regexp "^\\*Python")
  (oset this :repl-buffer-start-timeout 0)
  (oset this :show-repl-after-eval-p t)
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this python-flex-compiler))
  (require 'python))

(cl-defmethod flex-compiler-eval-form-impl ((this python-flex-compiler) form)
  (python-shell-send-string form))

(cl-defmethod flex-compiler-eval-config ((this python-flex-compiler) file)
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (python-shell-send-buffer))))

(cl-defmethod flex-compiler-eval-initial-at-point ((this python-flex-compiler))
  (thing-at-point 'symbol))

(cl-defmethod flex-compiler-repl-start ((this python-flex-compiler))
  (run-python (python-shell-parse-command)))

(flex-compile-manager-register the-flex-compile-manager (python-flex-compiler))

(provide 'flex-compile-python)

;;; flex-compile-python.el ends here
