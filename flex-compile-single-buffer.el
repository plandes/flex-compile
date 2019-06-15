;;; flex-compile-single-buffer.el --- compiler for single buffer management

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

;; This file contains an abstract utility class that provides other compilers
;; the ability to display the output of the compilation across buffers and
;; windows.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'flex-compile-base)
(require 'flex-compile-config)

(defconst flex-compile-display-mode-options
  '(choice (const :tag "This Window" switch)
	   (const :tag "Other Window" display)
	   (const :tag "Next Frame Otherwise Switch" next-frame-switch)
	   (const :tag "Next Frame Otherwise Display" next-frame-display)
	   (const :tag "Next Frame Skip Switch" next-frame-skip-switch)
	   (const :tag "Next Frame Skip Display" next-frame-skip-display)
	   (const :tag "Never" never)))

(defcustom flex-compile-display-buffer-new-mode 'next-frame-display
  "How to show/switch a new \(not yet created) compilation buffer.
`Switch to Buffer' means to first pop then switch to the buffer.
`Display Buffer' means to show the buffer in a different
window (see `display-buffer').
`Next Frame Otherwise Switch' means to use the next frame if
there are multiple frames, otherwise pop and switch to the buffer.
`Next Frame Otherwise Display' means to use the next frame if
there are multiple frames, otherwise show buffer.
`Next Frame Skip Switch' means to do nothing there are
multiple frames, otherwise pop and switch to the buffer.
`Next Frame Skip Display' means to do nothing there are multiple
frames, otherwise display the buffer.
`Never' means to never show the buffer."
  :type flex-compile-display-mode-options
  :group 'flex-compile)

(defcustom flex-compile-display-buffer-exists-mode 'next-frame-skip-display
  "Like `flex-compile-display-buffer-new-mode', but use when buffer exists."
  :type flex-compile-display-mode-options
  :group 'flex-compile)

(defvar flex-compiler-set-buffer-exists
  (delete-dups (list flex-compile-display-buffer-exists-mode 'never))
  "History variable for `flex-compiler-set-buffer-exists-mode'.")

(defclass single-buffer-flex-compiler (flex-compiler)
  ((buffer-name :initarg :buffer-name
		:type string
		:documentation "The default name of the single buffer."))
  :abstract t
  :documentation "A flex compiler that has a single buffer.")

(cl-defmethod flex-compiler-buffer-name ((this single-buffer-flex-compiler))
  "Return the name of the single buffer."
  (format "*%s*" (slot-value this 'buffer-name)))

(cl-defmethod flex-compiler-buffer ((this single-buffer-flex-compiler))
  "Return non-nil if there exists buffer for this compiler and is live."
  (get-buffer (flex-compiler-buffer-name this)))

(cl-defmethod flex-compiler-start-buffer ((this single-buffer-flex-compiler)
					  start-type)
  "Return a new buffer with a processing compilation.
START-TYPE is either symbols `compile', `run', `clean' depending
if invoked by `flex-compiler-compile' or `flex-compiler-run'."
  (flex-compiler--unimplemented this "start-buffer"))

(cl-defmethod flex-compiler-display-modes ((this single-buffer-flex-compiler))
  "Return an alist with keys `new' and `exists'.
This implementation returns `flex-compile-display-buffer-new-mode' and
`flex-compile-display-buffer-exists-mode' respectfully."
  `((new . ,flex-compile-display-buffer-new-mode)
    (exists . ,flex-compile-display-buffer-exists-mode)))

(defun flex-compiler-display-function (mode)
  "Return a function used for displaying a buffer using MODE.
MODE is either `flex-compile-display-buffer-new-mode' or
`flex-compile-display-buffer-exists-mode'."
  (cl-flet ((display-nf
	     (single-frame-fn multi-frame-fn)
	     (if (> (length (visible-frame-list)) 1)
		 (if multi-frame-fn
		     multi-frame-fn
		   '(lambda (buf)
		      (-> (window-list (next-frame))
			  car
			  (set-window-buffer buf))))
	       single-frame-fn)))
    (let ((void-fn 'list)
	  (display-fn
	   '(lambda (buf)
	      (let ((display-buffer-fallback-action nil))
		(if (= 1 (length (window-list)))
		    (split-window))
		(or (display-buffer buf '(display-buffer-reuse-window
					  ((inhibit-switch-frame . t)
					   (allow-no-window . t))))
		    (display-buffer buf '(display-buffer-use-some-window
					  ((inhibit-switch-frame . t)))))))))
      (cl-case mode
	(never void-fn)
	(switch 'pop-to-buffer)
	(display display-fn)
	(next-frame-switch (display-nf 'pop-to-buffer nil))
	(next-frame-display (display-nf display-fn nil))
	(next-frame-skip-switch (display-nf 'pop-to-buffer void-fn))
	(next-frame-skip-display (display-nf display-fn void-fn))
	(t (error "No mode: %S" mode))))))

(cl-defmethod flex-compiler-display-buffer ((this single-buffer-flex-compiler)
					    &optional compile-def)
  "Display buffer based on values returned from `flex-compiler-display-modes'."
  (if (null compile-def)
      (setq compile-def
	    (flex-compiler-single-buffer--flex-comp-def this 'compile nil)))
  (if (not (consp compile-def))
      (error "Unknown compile-def: %S" compile-def))
  (let* ((modes (flex-compiler-display-modes this))
	 (fn (flex-compiler-display-function
	      (if (cdr (assq 'newp compile-def))
		  (cdr (assq 'new modes))
		(cdr (assq 'exists modes)))))
	 (buf (cdr (assq 'buffer compile-def))))
    (unless (eq buf 'killed-buffer)
      (if (and buf (not (bufferp buf)))
	  (error "Unknown buffer object: %S" buf))
      (and fn (buffer-live-p buf) (funcall fn buf)))))

(cl-defmethod flex-compiler-single-buffer--flex-comp-def
  ((this single-buffer-flex-compiler) start-type startp)
  (let* ((has-buffer-p (flex-compiler-buffer this))
	 (buf (flex-compiler-buffer this)))
    (when (or startp (null buf))
	(if (child-of-class-p (eieio-object-class this) 'conf-flex-compiler)
	    (flex-compiler-set-required this))
	(setq buf (flex-compiler-start-buffer this start-type)))
    `((newp . ,(not has-buffer-p))
      (buffer . ,buf))))

(cl-defmethod flex-compiler-compile ((this single-buffer-flex-compiler))
  (flex-compiler-single-buffer--flex-comp-def this 'compile t))

(cl-defmethod flex-compiler-run ((this single-buffer-flex-compiler))
  (flex-compiler-single-buffer--flex-comp-def this 'run t))

(cl-defmethod flex-compiler-clean ((this single-buffer-flex-compiler))
  (flex-compiler-single-buffer--flex-comp-def this 'clean t))

(provide 'flex-compile-single-buffer)

;;; flex-compile-single-buffer.el ends here
