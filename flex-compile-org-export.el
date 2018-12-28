;;; flex-compile-org-export.el --- convenience compiler that evaluates Emacs Lisp

;; Copyright (C) 2015 - 2018 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: interactive function command compile flexible

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

;; Compiler that exports Org mode to external formats and then shows the
;; output in the browser.  Only HTML is currently supported.

;;; Code:

(require 'flex-compile-manage)

;;; func file compiler
(defclass org-export-flex-compiler (optionable-flex-compiler)
  ()
  :documentation "Convenience compiler that exports an Org buffer to a file.")

(cl-defmethod initialize-instance ((this org-export-flex-compiler) &optional args)
  (oset this :name "org-export")
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this org-export-flex-compiler))
  (require 'ox-twbs)
  (require 'choice-program-complete))

(defvar flex-compiler-org-export-function-history nil)

(cl-defmethod flex-compiler-org-export-read-method ((this org-export-flex-compiler))
  "Read the export method function from the user"
  (let* ((methods '(("Plain HTML" . org-html-export-to-html)
		    ("Bootstrap HTML" . org-twbs-export-to-html)))
	 (method (choice-program-complete
		  "Export method"
		  (mapcar 'first methods)
		  t t nil
		  'flex-compiler-org-export-function-history
		  (or (second flex-compiler-org-export-function-history)
		      "Bootstrap HTML")
		  nil nil t)))
    (cdr (assoc method methods))))

(cl-defmethod flex-compiler-read-options ((this org-export-flex-compiler))
  (list (flex-compiler-org-export-read-method this)))

(cl-defmethod flex-compiler-compile ((this org-export-flex-compiler))
  (unless (flex-compiler-options this)
    (flex-compiler-read-set-options this nil))
  ;; (shell-command "osascript -e 'tell application \"Emacs\" to activate'")
  (org-open-file (eval (flex-compiler-options this))))

(flex-compile-manager-register the-flex-compile-manager
			       (org-export-flex-compiler))

(provide 'flex-compile-org-export)

;;; flex-compile-org-export.el ends here
