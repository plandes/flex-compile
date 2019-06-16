;;; flex-compile-xml-validate.el --- xml validation

;; Copyright (C) 2015 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: xml validation compilation

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

(defclass flex-conf-schema-file-prop (flex-conf-file-prop)
  ()
  :documentation "A schema file property")

(cl-defmethod initialize-instance ((this flex-conf-schema-file-prop)
				   &optional args)
  (setq args (plist-put args :prompt "Schema file")
	args (plist-put args :validate-modes '(nxml-mode))
	args (plist-put args :input-type 'last))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-guess-schema-file ((this flex-conf-schema-file-prop))
  "Try to determine where the XSD is by the location "
  (with-temp-buffer
    (-> (slot-value this 'compiler)
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

(cl-defmethod flex-compiler-conf-read ((this flex-conf-schema-file-prop))
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
				   &optional args)
  (let ((props (list (flex-conf-schema-file-prop :object-name 'schema-file
						 :compiler this
						 :required t
						 :order 1))))
    (setq args (plist-put args :object-name "xml-validate")
	  args (plist-put args :description "XML")
	  args (plist-put args :validate-modes '(nxml-mode))
	  args (plist-put args :buffer-name "XML Validation")
	  args (plist-put args :props (append (plist-get args :props) props))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this xml-validate-flex-compiler))
  (require 'xml))

(cl-defmethod flex-compiler-conf-set-prop ((this xml-validate-flex-compiler)
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

(flex-compile-manager-register the-flex-compile-manager
			       (xml-validate-flex-compiler))

(provide 'flex-compile-xml-validate)

;;; flex-compile-xml-validate.el ends here
