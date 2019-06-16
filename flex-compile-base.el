;;; flex-compile-manage.el --- manager for flexible compilers

;; Copyright (C) 2015 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: compilation integration

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

;; This file contains the library Emacs group definitions and the base class
;; compiler.

;;; Code:

(require 'eieio)
(require 'config-manage)

(defgroup flex-compile nil
  "Compile Helper Functions"
  :group 'tools
  :group 'compilation
  :prefix '"flex-compile")

(defclass flex-compiler (config-entry)
  ()
  :abstract true
  :documentation "Base class for compilation executors (do the work).
Instances of this class are also persistable and their state is stored in a
configuration file.")

(define-error 'flex-compiler-un-implemented
  "Un-implemented method flex-compiler method"
  'cl-no-applicable-method)

(cl-defmethod initialize-instance ((this flex-compiler) &optional args)
  (if (null (plist-get args :description))
      (setq args (plist-put args :description
			    (capitalize (plist-get args :name)))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler--unimplemented ((this flex-compiler) method)
  (with-temp-buffer
    (set-buffer (get-buffer-create "*flex-compiler-backtrace*"))
    (erase-buffer)
    (let ((standard-output (current-buffer)))
      (backtrace)))
  (signal 'flex-compiler-un-implemented
	  (list method (with-temp-buffer
			 (cl-print-object this (current-buffer))))))

(cl-defmethod flex-compiler-load-libraries ((this flex-compiler))
  "Call back for to load and require libraries needed by the compiler.")

(cl-defmethod flex-compiler-save-config ((this flex-compiler))
  "Tell the compiler manager to persist the configuration of all compilers."
  (with-slots (manager) this
    (unless manager
      (error "No manager set in compiler: %S"
	     (with-temp-buffer
	       (cl-print-object this (current-buffer)))))
    (config-persistable-save manager)))

(cl-defmethod flex-compiler-reset-state ((this flex-compiler))
  "Reset all persistable slots to initial state.
This implementation sets all slots to nil."
  (config-persistent-reset this))

(cl-defmethod flex-compiler-run ((this flex-compiler))
  "Invoke the run functionality of the compiler."
  (flex-compiler--unimplemented this "run"))

(cl-defmethod flex-compiler-compile ((this flex-compiler))
  "Invoke the compile functionality of the compiler."
  (flex-compiler--unimplemented this "compile"))

(cl-defmethod flex-compiler-clean ((this flex-compiler))
  "Invoke the clean functionality of the compiler."
  (flex-compiler--unimplemented this "clean"))

(cl-defmethod flex-compiler-configure ((this flex-compiler) config-options)
  "Configure the compiler.

CONFIG-OPTIONS is the numeric argument (if any) passed in the iteractive mode
with \\[universal-argument]."
  (flex-compiler--unimplemented this "configure"))

(cl-defmethod flex-compiler-display-buffer ((this flex-compiler)
					    &optional compile-def)
  "Called to display the compilation buffer \(if any).

COMPILE-DEF is the compilation defition, which is usually an alist of having
an alist with `newp' indicating if the buffer is new and `buffer' of the buffer
just created.  This is also called for clean invocations, in which case the
value is nil.  The value (when non-nil) is dependent on the flex-compiler.")

(cl-defmethod flex-compiler-display-buffer-alist ((this flex-compiler))
  "Return a value that will be bound to `display-buffer-alist', which suggests
to Emacs libraries to not display buffers (via `display-buffer').  This is so
a `flex-compiler' can explictly control buffer display with
`flex-compiler-display-buffer' \(if it chooses).."
  ;; `list' takes any number of arguments and has no side effects
  '((list . (list))))



(defclass no-op-flex-compiler (flex-compiler)
  ()
  :documentation "A no-op compiler for the disabled state.")

(cl-defmethod initialize-instance ((this no-op-flex-compiler) &optional args)
  (setq args (plist-put args :name "disable")
	args (plist-put args :description "Do nothing"))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler--unimplemented ((this no-op-flex-compiler) method)
  (message "Compiler is disabled"))

(provide 'flex-compile-base)

;;; flex-compile-base.el ends here
