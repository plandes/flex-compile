;;; flex-compile-org-export.el --- convenience compiler that evaluates Emacs Lisp

;; Copyright (C) 2015 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: interactive function command compile flexible processes

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

(config-manage-declare-functions
 org-open-file
 org-twbs-export-to-html)

;;; func file compiler
(defclass org-export-flex-compiler (conf-file-flex-compiler)
  ((export-fn :initarg :export-fn
	      :initform org-twbs-export-to-html
	      :documentation "The Org mode export function."))
  :method-invocation-order :c3
  :documentation "\
This compiler exports [Org mode](https://orgmode.org) to external formats and
then shows the output in the browser.  Only HTML is currently supported.")

(cl-defmethod initialize-instance ((this org-export-flex-compiler)
				   &optional slots)
  (let* ((fn '(lambda (this compiler default prompt history)
		(split-string (read-string prompt nil history default))))
	 (choices '(("Plain HTML" . org-html-export-to-html)
		    ("Bootstrap HTML" . org-twbs-export-to-html)))
	 (props (list (config-choice-description-prop
		       :object-name 'export-fn
		       :prompt "Export format"
		       :prop-entry this
		       :choices choices
		       :required t
		       :input-type 'toggle))))
    (setq slots (plist-put slots :object-name "org-export")
	  slots (plist-put slots :description "Org mode")
	  slots (plist-put slots :validate-modes '(org-mode))
	  slots (plist-put slots :props
			   (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this org-export-flex-compiler))
  (require 'ox-twbs)
  (require 'choice-program-complete))

(cl-defmethod flex-compiler-compile ((this org-export-flex-compiler))
  (config-prop-entry-set-required this)
  (if nil
      (shell-command "osascript -e 'tell application \"Emacs\" to activate'"))
  (with-slots (export-fn config-file) this
    (with-current-buffer (flex-compiler-conf-file-buffer this)
      (org-open-file (funcall export-fn)))))

(cl-defmethod flex-compiler-clean ((this org-export-flex-compiler))
  (config-prop-entry-set-required this)
  (with-slots (config-file) this
    (let ((html-file (replace-regexp-in-string "\.org$" ".html" config-file)))
      (if (not (file-exists-p html-file))
	  (message "File %s doesn't exist" html-file)
	(delete-file html-file t)
	(message "Deleted %s" html-file)))))

(flex-compile-manager-register flex-compile-manage-inst
			       (org-export-flex-compiler))

(provide 'flex-compile-org-export)

;;; flex-compile-org-export.el ends here
