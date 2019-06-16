;;; flex-compile-clojure.el --- clojure compile functions

;; Copyright (C) 2015 - 2019 Paul Landes

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

(require 'dash)
(require 'flex-compile-manage)

(config-manage-declare-functions
 cider-repl-return cider-connect
 nrepl-dict-get nrepl-sync-request:eval
 cider-current-connection cider-current-session cider-current-ns
 cider-load-file cider-last-sexp cider-quit cider-jack-in)

(config-manage-declare-variables
 cider-repl-display-in-current-window)

(defclass clojure-flex-compiler (repl-flex-compiler)
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
  `repl-host' and `repl-port'"))
  :method-invocation-order :c3
  :documentation "\
This is a REPL based compiler that allows for evaluation Clojure
buffers, expressions and starting the REPL using
\[Cider](https://github.com/clojure-emacs/cider).

The Clojure compiler connects using two Cider modes: the default `jack-in'
mode or connecting to a host and port remotely with `cider-connect'.  You can
switch betwee these two methods with the [given keybindings](#key-bindings):

  `M-x 1 C-u C-x C-u'
  
See documetation with `M-h f flex-compiler-query-eval' method for more
inforamtion (and current binding).")

(cl-defmethod initialize-instance ((this clojure-flex-compiler) &optional args)
  (let ((props (list (flex-conf-choice-prop :object-name 'connect-mode
					    :compiler this
					    :prompt "Connection mode"
					    :choices '(jack-in connect)
					    :input-type 'toggle))))
    (setq args (plist-put args :object-name "clojure")
	  args (plist-put args :validate-modes '(clojure-mode))
	  args (plist-put args :repl-buffer-regexp "^\\*cider-repl ")
	  args (plist-put args :repl-buffer-start-timeout 0)
	  args (plist-put args :props (append (plist-get args :props) props))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this clojure-flex-compiler))
  (require 'cider)
  ;; allow flex-compile to manage windows and frame
  (setq cider-repl-display-in-current-window nil))

(cl-defmethod flex-compiler-send-input ((this clojure-flex-compiler)
					&optional command)
  (goto-char (point-max))
  (insert command)
  (cider-repl-return))

(cl-defmethod flex-compiler-eval-form-impl ((this clojure-flex-compiler) form)
  (let* ((res (nrepl-sync-request:eval
		   form
		   (cider-current-connection)
		   (cider-current-ns))))
    (or (nrepl-dict-get res "err")
	(nrepl-dict-get res "value"))))

(cl-defmethod flex-compiler-repl-compile ((this repl-flex-compiler) file)
  (save-excursion
    (apply #'set-buffer (list (find-file-noselect file)))
    (cider-load-file file)))

(cl-defmethod flex-compiler-eval-initial-at-point ((this clojure-flex-compiler))
  (cider-last-sexp))

(cl-defmethod flex-compiler-kill-repl ((this clojure-flex-compiler))
  (condition-case err
      (cider-quit t)
    (error "Warning: %S" err))
  (sit-for 1)
  (->> (process-list)
       (-filter #'(lambda (elt)
		    (string-match "^nrepl-server" (process-name elt))))
       (-map #'kill-process))
  (cl-call-next-method this))

(cl-defmethod flex-compiler-repl-start ((this clojure-flex-compiler))
  (with-slots (connect-mode repl-host repl-port) this
    (with-current-buffer (flex-compiler-conf-file-buffer this)
      (cl-case connect-mode
	(connect (cider-connect (list :host repl-host :port repl-port)))
	(jack-in (cider-jack-in nil))
	(t (error "No such connection mode supported: %S" connect-mode))))))

(flex-compile-manager-register the-flex-compile-manager
			       (clojure-flex-compiler))

(provide 'flex-compile-clojure)

;;; flex-compile-clojure.el ends here
