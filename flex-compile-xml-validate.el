;;; flex-compile-xml-validate.el --- xml validation

;; Copyright (C) 2015 - 2017 Paul Landes

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

(defclass xml-validate-flex-compiler (run-args-flex-compiler)
  ((xmllint-program :initarg :xmllint-program
		    :initform "xmllint")
   (schema-file :initarg :schema-file
		:initform nil
		:documentation "\
Location of the schema file to validate against.")))

(cl-defmethod initialize-instance ((this xml-validate-flex-compiler)
				   &optional args)
  (oset this :name "xml-validate")
  (oset this :config-file-desc "XML instance file")
  (oset this :major-mode 'nxml-mode)
  (oset this :mode-desc "xml-validate")
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this xml-validate-flex-compiler))
  (require 'xml))

(cl-defmethod flex-compiler-guess-schema-file ((this xml-validate-flex-compiler))
  "Try to determine where the XSD is by the location "
  (with-temp-buffer
    (->> (flex-compiler-config this)
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

(cl-defmethod flex-compiler-read-options ((this xml-validate-flex-compiler))
  (let* ((schema-guess (flex-compiler-guess-schema-file this))
	 (initial (and schema-guess (file-name-nondirectory schema-guess)))
	 (dir (and schema-guess (file-name-directory schema-guess)))
	 (schema (read-file-name "Schema XSD: " dir schema-guess t initial)))
    (and schema (oset this :schema-file schema))
    nil))

(cl-defmethod flex-compiler-buffer-name ((this xml-validate-flex-compiler))
  "*XML Validation*")

(cl-defmethod flex-compiler-buffer ((this xml-validate-flex-compiler))
  (get-buffer (flex-compiler-buffer-name this)))

(cl-defmethod flex-compiler-xml-validate-schema ((this xml-validate-flex-compiler))
  (with-slots (schema-file) this
    (if (not schema-file)
	(error "No schema file set"))
    schema-file))

(cl-defmethod flex-compiler-config-persist ((this xml-validate-flex-compiler))
  (append `((schema-file . ,(slot-value this 'schema-file)))
	  (cl-call-next-method this)))

(cl-defmethod flex-compiler-config-unpersist ((this xml-validate-flex-compiler) config)
  (oset this :schema-file (cdr (assq 'schema-file config)))
  (cl-call-next-method this config))

(cl-defmethod flex-compiler-run-with-args ((this xml-validate-flex-compiler)
					   args start-type)
  (with-slots (buffer-name xmllint-program) this
    (let* ((config-file (flex-compiler-config this))
	   (schema (flex-compiler-xml-validate-schema this))
	   (cmd (mapconcat #'identity
			   `(,xmllint-program "--noout" "--schema"
					      ,schema ,config-file)
			   " "))
	   buf)
      (with-current-buffer
	  (setq buf (compilation-start cmd nil
				       #'(lambda (mode-name)
					   buffer-name)))
	(pop-to-buffer (current-buffer)))
      buf)))

(flex-compile-manager-register the-flex-compile-manager (xml-validate-flex-compiler))

(provide 'flex-compile-xml-validate)

;;; flex-compile-xml-validate.el ends here
