;;; flex-compile-org-export.el --- convenience compiler that evaluates Emacs Lisp

;; Copyright (C) 2015 - 2020 Paul Landes

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
	      :documentation "The Org mode export function.")
   (open-file :initarg :open-file
	      :initform t
	      :type boolean
	      :documentation "Whether to open the file after exported.")
   (output-directory :initarg :start-directory
		     :initform nil
		     :type (or null string)
		     :documentation "The output directory."))
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
		       :input-type 'toggle)
		      (config-boolean-prop
		       :object-name 'open-file
		       :prop-entry this
		       :prompt "Open file after generated"
		       :input-type 'toggle)
		      (config-directory-prop
		       :object-name 'output-directory
		       :prop-entry this
		       :prompt "Output directory"
		       :input-type 'last))))
    (setq slots (plist-put slots :object-name "org-export")
	  slots (plist-put slots :description "Org mode")
	  slots (plist-put slots :validate-modes '(org-mode))
	  slots (plist-put slots :props
			   (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this org-export-flex-compiler))
  (require 'ox-twbs)
  (require 'choice-program-complete))

(cl-defmethod config-prop-set ((this org-export-flex-compiler) prop val)
  (if (eq (config-prop-name prop) 'config-file)
      ;; when the file configuration file (Org mode file) changes, we must
      ;; mirror the output directory for default behavior; then allow the user
      ;; to change the output directory to export some where elese
      (with-slots (output-directory) this
	(config-prop-validate prop val)
	(setq output-directory (file-name-directory val))))
  (cl-call-next-method this prop val))

(cl-defmethod flex-compiler-org-export-source ((this org-export-flex-compiler))
  "Return the generated target file by the Org system."
  (with-slots (config-file) this
    (replace-regexp-in-string "\.org$" ".html" config-file)))

(cl-defmethod flex-compiler-org-export-dest ((this org-export-flex-compiler))
  "Return the user desired location of the output file."
  (with-slots (config-file output-directory) this
    (let* ((src (flex-compiler-org-export-source this))
	   (dst (file-name-nondirectory src)))
      (concat (file-name-as-directory output-directory) dst))))

(cl-defmethod flex-compiler-compile ((this org-export-flex-compiler))
  (config-prop-entry-set-required this)
  (if nil
      (shell-command "osascript -e 'tell application \"Emacs\" to activate'"))
  (with-slots (export-fn config-file open-file) this
    (with-current-buffer (flex-compiler-conf-file-buffer this)
      (let* ((open-fn (if open-file
			 'org-open-file
		       'identity))
	     (src (funcall export-fn))
	     (dst (flex-compiler-org-export-dest this)))
	(rename-file src dst t)
	(message "Exported file to %s" dst)
	;; call the function to open the file if configured
	(funcall open-fn dst)))))

(cl-defmethod flex-compiler-clean ((this org-export-flex-compiler))
  (config-prop-entry-set-required this)
  (with-slots (config-file) this
    (let ((html-file (flex-compiler-org-export-dest this)))
      (if (not (file-exists-p html-file))
	  (message "File %s doesn't exist" html-file)
	(delete-file html-file t)
	(message "Deleted %s" html-file)))))

(flex-compile-manager-register flex-compile-manage-inst
			       (org-export-flex-compiler))

(provide 'flex-compile-org-export)

;;; flex-compile-org-export.el ends here
