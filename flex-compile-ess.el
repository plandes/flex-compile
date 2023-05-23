;;; flex-compile-ess.el --- Ess compile functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2023 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: ess Emacs Speaks Statistics compilation processes
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

;; Implementation compiler for ess integration

;;; Code:

(require 'flex-compile-manage)

(config-manage-declare-functions ess-eval-region R)

(defclass ess-flex-compiler (repl-flex-compiler)
  ()
  :method-invocation-order :c3
  :documentation "\
This is a REPL based compiler to evaluate R code with
[Emacs Speaks Statistics](https://ess.r-project.org) .")

(cl-defmethod initialize-instance ((this ess-flex-compiler) &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (setq slots (plist-put slots :object-name "ess")
	slots (plist-put slots :description "Emacs speaks statistics")
	slots (plist-put slots :validate-modes '(ess-r-mode))
	slots (plist-put slots :repl-buffer-regexp "^\\*R.*\\*$")
	slots (plist-put slots :repl-buffer-start-timeout 5))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this ess-flex-compiler))
  "Load library `ess-site' for THIS compielr."
  (ignore this)
  (require 'ess-site))

(cl-defmethod flex-compiler-repl-start ((this ess-flex-compiler))
  "Start the REPL using THIS compiler."
  (let ((ess-ask-for-ess-directory nil))
    (ignore ess-ask-for-ess-directory)
    (with-current-buffer (find-file-noselect (slot-value this 'config-file))
      (R))))

(cl-defmethod flex-compiler-repl-compile ((this ess-flex-compiler) file)
  "Send the contents of source code FILE to the REPL buffer of THIS compiler."
  (ignore this)
  (with-current-buffer (find-file-noselect file)
    (ess-eval-region (point-min) (point-max) nil)))

(flex-compile-manager-register flex-compile-manage-inst (ess-flex-compiler))

(provide 'flex-compile-ess)

;;; flex-compile-ess.el ends here
