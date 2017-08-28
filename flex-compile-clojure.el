;;; flex-compile-clojure.el --- clojure compile functions

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

;; Implementation compiler for Clojure with Cider integration

;;; Code:

(require 'flex-compile-manage)

;; silence the compiler
(eval-when-compile
  (let ((fns '(cider-repl-return cider-connect
	       nrepl-dict-get nrepl-sync-request:eval
	       cider-current-connection cider-current-session cider-current-ns
	       cider-load-file cider-last-sexp cider-quit cider-jack-in)))
    (mapcar #'(lambda (sym)
		(eval `(defun ,sym (&rest x))))
	    fns)))

(defvar flex-compiler-clojure-connect-history nil
  "History for connection mode prompt read in `flex-compiler-query-eval'.")

(defclass clojure-flex-compiler (evaluate-flex-compiler)
  ((repl-host :initarg :repl-host
	      :type string
	      :initform "localhost"
	      :documentation "The host running the REPL; default: localhost")
   (repl-port :initarg :repl-port
	      :type integer
	      :initform 32345
	      :documentation "The port running the REPL; default: 32345")
   (connect-mode :initarg :connect-mode
		 :type symbol
		 :initform 'jack-in
		 :documentation "\
The conection mode, which is either:
- 'jack-in local mode, which invokes `cider-jack-in'
- 'connect remote mode, which invokes `cider-connect' using slots:
  `repl-host' and `repl-port'")))

(cl-defmethod initialize-instance ((this clojure-flex-compiler) &optional args)
  (oset this :name "clojure")
  (oset this :major-mode 'clojure-mode)
  (oset this :mode-desc "clojure file")
  (oset this :config-file-desc "clojure file")
  (oset this :repl-buffer-regexp "^\\*cider-repl ")
  (oset this :derived-buffer-names '(" *nrepl-server*"))
  (oset this :repl-buffer-start-timeout 0)
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this clojure-flex-compiler))
  (require 'cider))

(cl-defmethod flex-compiler-send-input ((this clojure-flex-compiler)
					&optional command)
  (goto-char (point-max))
  (insert command)
  (cider-repl-return))

(cl-defmethod flex-compiler-eval-form-impl ((this clojure-flex-compiler) form)
  (nrepl-dict-get (nrepl-sync-request:eval
		   form
		   (cider-current-connection)
		   (cider-current-session)
		   (cider-current-ns))
		  "value"))

(cl-defmethod flex-compiler-eval-config ((this clojure-flex-compiler) file)
  (save-excursion
    (eval-and-compile
      (let ((msg "save-excursion needed when REPL is current buffer"))
	(display-warning 'buffer-manage msg :debug)))
    ;; silence compiler
    (apply #'set-buffer (find-file-noselect file))
    (cider-load-file file)))

(cl-defmethod flex-compiler-eval-initial-at-point ((this clojure-flex-compiler))
  (if t
      (cider-last-sexp)
    (let ((sexp (sexp-at-point)))
      (cond ((null sexp) nil)
	    ((symbolp sexp) (format "(%S)" sexp))
	    (t (prin1-to-string sexp))))))

(cl-defmethod flex-compiler-kill-repl ((this clojure-flex-compiler))
  (cider-quit t)
  (sit-for 1)
  (cl-call-next-method this))

(cl-defmethod flex-compiler-query-eval ((this clojure-flex-compiler)
					config-options)
  "If invoked with interactively with from `flex-compile-compile':

   \\[digital-argument] \\[universal-argument] \\[flex-compile-compile]

with `digital-argument' of `1' the compiler prompts to switch between local
`cider-jack-in' mode or remote `cider-connect' mode."
  (if (and (not (consp config-options))
	   (= config-options 1))
      (with-slots (connect-mode) this
	(->> (choice-program-complete "Connection mode" '(jack-in connect) nil t nil
				      'flex-compiler-clojure-connect-history
				      (or (cl-second flex-compiler-clojure-connect-history)
					  'connect)
				      nil t t)
	     (setq connect-mode))))
  (cl-call-next-method this (list config-options)))

(cl-defmethod flex-compiler-repl-start ((this clojure-flex-compiler))
  (with-slots (connect-mode repl-host repl-port) this
    (with-current-buffer (flex-compiler-config-buffer this)
      (cl-case connect-mode
	(connect (cider-connect repl-host repl-port))
	(jack-in (cider-jack-in))
	(t (error "No such connection mode supported: %S" connect-mode))))))

(flex-compile-manager-register the-flex-compile-manager
			       (clojure-flex-compiler))

(provide 'flex-compile-clojure)

;;; flex-compile-clojure.el ends here