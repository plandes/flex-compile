;;; flex-compile-xml-validate.el --- xml validation  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2020 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: xml validation compilation processes

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

;; Implementation compiler for XML validation using command line `xmllint'.

;;; Code:

(require 'flex-compile-manage)

(eval-when-compile (require 'xml))

(defclass config-schema-file-prop (config-file-prop)
  ()
  :method-invocation-order :c3
  :documentation "A schema file property")

(cl-defmethod initialize-instance ((this config-schema-file-prop)
				   &optional slots)
  (setq slots (plist-put slots :prompt "Schema file")
	slots (plist-put slots :validate-modes '(nxml-mode))
	slots (plist-put slots :input-type 'last))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-guess-schema-file ((this config-schema-file-prop))
  "Try to determine where the XSD is by the location "
  (with-temp-buffer
    (-> (slot-value this 'prop-entry)
	(slot-value 'config-file)
	insert-file-contents)
    (condition-case nil
	(->> (xml-parse-region (point-min) (point-max))
	     car
	     xml-node-attributes
	     (assq 'xsi:schemaLocation)
	     cdr
	     (funcall #'(lambda (xsi)
			  (if (string-match "file://\\(.*\\)$" xsi)
			      (match-string 1 xsi)))))
      (error))))

(cl-defmethod config-prop-read ((this config-schema-file-prop))
  (let* ((schema-guess (flex-compiler-guess-schema-file this))
	 (initial (and schema-guess (file-name-nondirectory schema-guess)))
	 (dir (and schema-guess (file-name-directory schema-guess))))
    (read-file-name "Schema XSD: " dir schema-guess t initial)))



(defclass xml-validate-flex-compiler (single-buffer-flex-compiler
				      conf-file-flex-compiler)
  ((xmllint-program :initarg :xmllint-program
		    :initform "xmllint")
   (schema-file :initarg :schema-file
		:initform nil
		:documentation "\
Location of the schema file to validate against."))
  :method-invocation-order :c3
  :documentation "\
Implementation compiler for XML validation using command line
\[xmllint](http://xmlsoft.org/xmllint.html) command line tool.")

(cl-defmethod initialize-instance ((this xml-validate-flex-compiler)
				   &optional slots)
  (let ((props (list (config-schema-file-prop :object-name 'schema-file
					      :prop-entry this
					      :required t
					      :order 1))))
    (setq slots (plist-put slots :object-name "xml-validate")
	  slots (plist-put slots :description "XML")
	  slots (plist-put slots :validate-modes '(nxml-mode))
	  slots (plist-put slots :buffer-name "XML Validation")
	  slots (plist-put slots :props (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this xml-validate-flex-compiler))
  (require 'xml))

(cl-defmethod config-prop-set ((this xml-validate-flex-compiler)
				    prop val)
  (setf (slot-value this 'schema-file) nil)
  (cl-call-next-method this prop val))

(cl-defmethod flex-compiler-start-buffer ((this xml-validate-flex-compiler)
					  start-type)
  (with-slots (xmllint-program schema-file config-file) this
    (let* ((cmd (mapconcat #'identity
			   `(,xmllint-program "--noout" "--schema"
					      ,schema-file ,config-file)
			   " "))
	   (buffer-name (flex-compiler-buffer-name this)))
      (compilation-start cmd nil #'(lambda (mode-name)
				     buffer-name)))))

(flex-compile-manager-register flex-compile-manage-inst
			       (xml-validate-flex-compiler))

(provide 'flex-compile-xml-validate)

;;; flex-compile-xml-validate.el ends here
