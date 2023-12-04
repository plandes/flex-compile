;;; flex-compile-clojure.el --- Clojure compile functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2023 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: clojure cider compilation processes
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

;; Implementation compiler for Clojure with Cider integration

;;; Code:

(require 'dash)
(require 'flex-compile-manage)

(declare-function cider-repl-return "cider")
(declare-function cider-connect "cider")
(declare-function cider-current-connection "cider")
(declare-function cider-current-session "cider")
(declare-function cider-current-ns "cider")
(declare-function cider-load-file "cider")
(declare-function cider-last-sexp "cider")
(declare-function cider-quit "cider")
(declare-function cider-jack-in "cider")
(declare-function nrepl-dict-get "nrepl-dict")
(declare-function nrepl-sync-request:eval "nrepl-client")

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
Defines how to connect to a Clojure REPL.
The connection modes include:
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
inforamtion (and current binding).

Todo: support multiple Cider buffers as this implementation currently does
not.")

(cl-defmethod initialize-instance ((this clojure-flex-compiler) &optional slots)
  "Initialize THIS instance using SLOTS as initial values."
  (let ((props (list (config-choice-prop :object-name 'connect-mode
					 :prop-entry this
					 :prompt "Connection mode"
					 :choices '(jack-in connect)
					 :input-type 'toggle)
		     (config-number-prop :object-name 'repl-port
					 :prop-entry this
					 :prompt "REPL port"
					 :input-type 'last))))
    (setq slots (plist-put slots :object-name "clojure")
	  slots (plist-put slots :validate-modes '(clojure-mode))
	  slots (plist-put slots :repl-buffer-regexp
			   "^\\(\\*cider-repl \\|nrepl-server\\)")
	  ;; also see `nrepl-sync-request-timeout'
	  slots (plist-put slots :repl-buffer-start-timeout 0)
	  slots (plist-put slots
			   :props (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries ((this clojure-flex-compiler))
  "Load the `cider' library for THIS compiler.

This also sets `cider-repl-display-in-current-window' to nil"
  (ignore this)
  (require 'cider)
  ;; allow flex-compile to manage windows and frame
  (setq cider-repl-display-in-current-window nil))

(cl-defmethod flex-compiler-send-input ((this clojure-flex-compiler)
					&optional command)
  "Send a COMMAND (input) to THIS compiler’s Cider REPL."
  (ignore this)
  (goto-char (point-max))
  (insert command)
  (cider-repl-return))

(cl-defmethod flex-compiler-eval-form-impl ((this clojure-flex-compiler) form)
  "Evaluate the FORM and return the Cider REPL's response for THIS compiler."
  (ignore this)
  (let* ((res (nrepl-sync-request:eval
	       form
	       (cider-current-connection)
	       (cider-current-ns))))
    (or (nrepl-dict-get res "err")
	(nrepl-dict-get res "value"))))

(cl-defmethod flex-compiler-repl-compile ((this clojure-flex-compiler) file)
  "Send the contents of FILE to the Cider REPL buffer of THIS compiler."
  (ignore this)
  (save-excursion
    (set-buffer (list (find-file-noselect file)))
    (cider-load-file file)))

(cl-defmethod flex-compiler-eval-initial-at-point
  ((this clojure-flex-compiler))
  "Return the Clojure form at the current point to the REPL for THIS compiler."
  (ignore this)
  (cider-last-sexp))

(cl-defmethod flex-compiler-kill-repl ((this clojure-flex-compiler))
  "Use `cider-quit' to stop the Cider REPL for THIS compiler."
  (condition-case err
      (cider-quit t)
    (error "Warning: %S" err))
  (cl-call-next-method this))

(cl-defmethod flex-compiler-repl-start ((this clojure-flex-compiler))
  "Start the Cider REPL using THIS compiler."
  (with-slots (connect-mode repl-host repl-port) this
    (with-current-buffer (flex-compiler-conf-file-buffer this)
      (cl-case connect-mode
	;; hack to fix some pathology with nrepl window management and custom
	;; `display-buffer-alists' settings
	(connect (let ((wc (current-window-configuration)))
		   (save-excursion
		     (cider-connect (list :host repl-host :port repl-port)))
		   (set-window-configuration wc)))
	(jack-in (cider-jack-in nil))
	(t (error "No such connection mode supported: %S" connect-mode))))))

(flex-compile-manager-register flex-compile-manage-inst
			       (clojure-flex-compiler))

(provide 'flex-compile-clojure)

;;; flex-compile-clojure.el ends here
