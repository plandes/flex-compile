;;; flex-compile-script.el --- script compile functions

;; Copyright (C) 2015 - 2017 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: script integration compilation

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

;; Implementation compiler for script integration

;;; Code:

(require 'flex-compile-manage)

(defvar flex-compile-script-args-history nil
  "History variable for `flex-compile-script' arg.")

(defvar flex-compile-script-finish-success-function nil
  "A function to call (if non-nill) if the compilation is successful.")

;;; script file compiler
(defclass script-flex-compiler (run-args-flex-compiler)
  ((buffer-name :initarg :buffer-name
		:initform "*Script Compile*"
		:type string)
   (finish-success-function :initarg :finish-success-function
			    :initform nil
			    :documentation "\
A function to call (if non-nill) if the compilation is successful.")))

(cl-defmethod initialize-instance ((this script-flex-compiler) &optional args)
  (oset this :name "script")
  (oset this :mode-desc "script")
  (oset this :config-file-desc "script file")
  (oset this :major-mode 'non-existing-mode-symbol)
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this script-flex-compiler))
  (require 'compile)
  (require 'choice-program))

(cl-defmethod flex-compiler-validate-buffer-file ((this script-flex-compiler))
  (unless (memq major-mode '(sh-mode cperl-mode python-mode))
    (cl-call-next-method this)))

(cl-defmethod flex-compiler-read-options ((this script-flex-compiler))
  (read-string "Script arguments: "
	       (car flex-compile-script-args-history)
	       'flex-compile-script-args-history))

(cl-defmethod flex-compiler-run-with-args ((this script-flex-compiler) args)
  (let ((config-file (flex-compiler-config this))
	reset-target)
    (with-slots (buffer-name finish-success-function) this
      (let ((cmd (concat config-file " " (mapconcat #'identity args " "))))
	(with-current-buffer
	    (compilation-start cmd nil
			       #'(lambda (mode-name)
				   buffer-name))
	  (if finish-success-function
	      (add-to-list 'flex-compile-script-finish-success-function
			   finish-success-function)))))))

(flex-compile-manager-register the-flex-compile-manager (script-flex-compiler))

(provide 'flex-compile-script)

;;; flex-compile-script.el ends here
