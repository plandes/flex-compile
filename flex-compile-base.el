;;; flex-compile-base.el --- Manager for flexible compilers  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2023 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: compilation integration processes
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

;; This file contains the library Emacs group definitions and the base class
;; compiler.

;;; Code:

(require 'eieio)
(require 'config-manage)
(require 'config-manage-prop)

(defgroup flex-compile nil
  "Compile Helper Functions."
  :group 'tools
  :group 'compilation
  :prefix '"flex-compile")

(defclass flex-compiler (config-prop-entry)
  ()
  :abstract true
  :method-invocation-order :c3
  :documentation "Base class for compilation executors (do the work).
Instances of this class are also persistable and their state is stored in a
configuration file.")

(cl-defmethod initialize-instance ((this flex-compiler) &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (if (null (plist-get slots :description))
      (setq slots (plist-put slots :description
			    (capitalize (plist-get slots :object-name)))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this flex-compiler))
  "Call back for to load and require libraries needed by THIS compiler."
  (ignore this))

(cl-defmethod flex-compiler-save-config ((this flex-compiler))
  "Tell the compiler manager to persist the configuration of all compilers.
THIS is the object instance."
  (with-slots (manager) this
    (unless manager
      (error "No manager set in compiler: %S"
	     (with-temp-buffer
	       (cl-print-object this (current-buffer)))))
    (config-persistable-save manager)))

(cl-defmethod flex-compiler-reset-state ((this flex-compiler))
  "Reset all persistable slots to initial state.
This implementation sets all slots to nil.
THIS is the object instance."
  (config-persistent-reset this))

(cl-defmethod flex-compiler-run ((this flex-compiler))
  "Invoke the run functionality of the compiler.
THIS is the object instance."
  (config-persistent--unimplemented this "run"))

(cl-defmethod flex-compiler-compile ((this flex-compiler))
  "Invoke the compile functionality of the compiler.
THIS is the object instance."
  (config-persistent--unimplemented this "compile"))

(cl-defmethod flex-compiler-clean ((this flex-compiler) &optional allp)
  "Invoke the clean functionality of the compiler.
if ALLP is non-nil, then invoke a more destructive cleaning when supported.
THIS is the object instance."
  (ignore allp)
  (config-persistent--unimplemented this "clean"))

(cl-defmethod config-entry-set-name ((this flex-compiler) name)
  "Disallow renaming with NAME from `config-manage-mode' as it is nonsensical.
THIS is the object instance."
  (ignore this name)
  (config-persistent--unimplemented this "set-name"))

(cl-defmethod flex-compiler-display-buffer ((this flex-compiler)
					    &optional compile-def)
  "Called to display the compilation buffer \(if any) for THIS compiler.

COMPILE-DEF is the compilation definition, which is usually an
alist of having an alist with `newp' indicating if the buffer is
new and `buffer' of the buffer just created.  This is also called
for clean invocations, in which case the value is nil.  The
value (when non-nil) is dependent on the flex-compiler."
  (ignore this compile-def))

(cl-defmethod flex-compiler-display-buffer-alist ((this flex-compiler))
  "Return a value that will be bound to `display-buffer-alist' in THIS compiler.

This suggests to Emacs libraries to not display buffers via
`display-buffer'.  This is so a `flex-compiler' can explictly
control buffer display with `flex-compiler-display-buffer' \(if
it chooses).."
  (ignore this)
  ;; `list' takes any number of arguments and has no side effects
  '((list . (list))))



(defclass no-op-flex-compiler (flex-compiler)
  ()
  :documentation "A no-op compiler for the disabled state.")

(cl-defmethod initialize-instance ((this no-op-flex-compiler) &optional slots)
  "Initialize instance THIS with arguments SLOTS."
  (setq slots (plist-put slots :object-name "disable")
	slots (plist-put slots :description "Do nothing"))
  (cl-call-next-method this slots))

(cl-defmethod config-persistent--unimplemented ((this no-op-flex-compiler)
						method)
  "Message that THIS compiler is disabled for persistance \(if tried).
METHOD is the EIEIO method called that has no implementation."
  (ignore this)
  (message "Compiler is disabled for %S" method))

(cl-defmethod config-prop-entry-configure ((this no-op-flex-compiler) _)
  "Raise error for THIS no-op implementation."
  (config-persistent--unimplemented this "configure"))

(provide 'flex-compile-base)

;;; flex-compile-base.el ends here
