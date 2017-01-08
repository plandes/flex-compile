;;; compile-flex-clojure.el --- clojure compile functions

;; Copyright (C) 2015 - 2017 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: clojure cider compilation

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

;; Implementation compiler for Clojure with Cider 0.12 integration

;;; Code:

(require 'compile-flex)

;; silence the compiler
(eval-when-compile
  (let ((fns '(cider-repl-return
	       nrepl-dict-get nrepl-sync-request:eval
	       cider-current-connection cider-current-session cider-current-ns
	       cider-load-file cider-last-sexp cider-quit cider-jack-in)))
    (mapcar #'(lambda (sym)
		(eval `(defun ,sym (&rest x))))
	    fns)))

(defclass clojure-flex-compiler (evaluate-flex-compiler) ())

(defmethod initialize-instance ((this clojure-flex-compiler) &rest rest)
  (oset this :name "clojure")
  (oset this :major-mode 'clojure-mode)
  (oset this :mode-desc "clojure file")
  (oset this :config-file-desc "clojure file")
  (oset this :repl-buffer-regexp "^\\*cider-repl ")
  (oset this :derived-buffer-names '(" *nrepl-server*"))
  (oset this :repl-buffer-start-timeout 0)
  (apply 'call-next-method this rest))

(defmethod flex-compiler-load-libraries ((this clojure-flex-compiler))
  (require 'cider))

(defmethod flex-compiler-send-input ((this clojure-flex-compiler)
				     &optional command)
  (goto-char (point-max))
  (insert command)
  (cider-repl-return))

(defmethod flex-compiler-eval-form-impl ((this clojure-flex-compiler) form)
  (nrepl-dict-get (nrepl-sync-request:eval
		   form
		   (cider-current-connection)
		   (cider-current-session)
		   (cider-current-ns))
		  "value"))

(defmethod flex-compiler-eval-config ((this clojure-flex-compiler) file)
  (save-excursion
    (eval-and-compile
      (let ((msg "save-excursion needed when repl is current buffer"))
	(display-warning 'buffer-manage msg :debug)))
    (set-buffer (find-file-noselect file))
    (cider-load-file file)))

(defmethod flex-compiler-eval-initial-at-point ((this clojure-flex-compiler))
  (if t
      (cider-last-sexp)
    (let ((sexp (sexp-at-point)))
      (cond ((null sexp) nil)
	    ((symbolp sexp) (format "(%S)" sexp))
	    (t (prin1-to-string sexp))))))

(defmethod flex-compiler-kill-repl ((this clojure-flex-compiler))
  (cider-quit t)
  (sit-for 1)
  (apply 'call-next-method this nil))

(defmethod flex-compiler-repl-start ((this clojure-flex-compiler))
  (with-current-buffer (flex-compiler-config-buffer this)
    (cider-jack-in)))

(flex-compile-manager-register the-flex-compile-manager
			       (clojure-flex-compiler nil))

(provide 'compile-flex-clojure)

;;; compile-flex-clojure.el ends here
