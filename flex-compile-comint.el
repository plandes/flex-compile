;;; flex-compile-comint.el --- comint compile functions

;; Copyright (C) 2015 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: comint integration compilation

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

;;; Implementation compiler for comint integration.  See class
;;; `comint-flex-compiler' documentation for more information and motivation.

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'flex-compile-manage)

(defclass comint-flex-compiler (conf-file-flex-compiler)
  ((buffer :initarg :buffer
	   :initform nil
	   :type (or null buffer)
	   :documentation "The buffer to insert the `content' slot.")
   (content :initarg :content
	    :initform nil
	    :type (or null string)
	    :documentation "\
The string to insert in the buffer referred by the `buffer' slot."))
  :method-invocation-order :c3
  :documentation "Send text to any running `comint' buffer.
This is useful for entering a command in a shell, SQL etc buffer that otherwise
requires switching back and forth between buffers, which is a hassle.")

(cl-defmethod initialize-instance ((this comint-flex-compiler) &optional slots)
  (let ((props (list (config-buffer-prop :object-name 'buffer
					 :prop-entry this
					 :required t
					 :transient t
					 :prompt "Buffer"
					 :input-type 'last)
		     (config-prop :object-name 'content
				  :prop-entry this
				  :prompt "Input string"
				  :input-type 'last))))
    (setq slots (plist-put slots :object-name "comint")
	  slots (plist-put slots
			   :props (append (plist-get slots :props) props)))
    (cl-call-next-method this slots)
    ;; TODO: pass these as properties to config-file-flex-compiler instead of
    ;; having to clobber its defaults
    (let ((prop (config-prop-by-name this 'config-file)))
      (setf (slot-value prop 'prompt) "Insert file"
	    (slot-value prop 'required) nil))))

(cl-defmethod flex-compiler-load-libraries ((this comint-flex-compiler))
  (require 'comint))

(cl-defmethod flex-compiler-eval-form-impl ((this comint-flex-compiler) form)
  (goto-char (point-max))
  (insert form)
  (comint-send-input))

(cl-defmethod flex-compiler-compile ((this comint-flex-compiler))
  (with-slots (buffer) this
    (when (not (buffer-live-p buffer))
      (setq buffer nil))
    (config-prop-entry-set-required this)
    (with-slots (config-file content) this
      (if (and (null config-file) (null content))
	  (error "Either property `config-file' or `content' needs to be set"))
      (let ((contents (->> (or content
			       (with-temp-buffer
				 (insert-file-contents config-file)
				 (buffer-string)))
			   string-trim)))
	(with-current-buffer buffer
	  (goto-char (point-max))
	  (insert contents)
	  (comint-send-input))))))

(flex-compile-manager-register the-flex-compile-manager (comint-flex-compiler))

(provide 'flex-compile-comint)

;;; flex-compile-comint.el ends here
