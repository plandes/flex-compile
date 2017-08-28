;;; flex-compile-scala.el --- scala compile functions

;; Copyright (C) 2015 - 2017 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: scala integration compilation

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

;; Implementation compiler for scala and Spark 2.0.2 integration

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'choice-program-complete)
(require 'flex-compile-manage)

;; silence the compiler
(eval-when-compile
  (defvar bshell-manager-singleton)
  (defvar ensime-inf-buffer-name))
(flex-compile-declare
 buffer-manager-new-entry
 buffer-entry-buffer buffer-entry-insert
 ensime-config-find sbt-command ensime-inf-eval-buffer
 ensime-inf-send-string buffer-manager-entry sbt:find-root
 ensime-shutdown ensime-inf-quit-interpreter)

(defvar compiler-flex-scala-query-eval-mode-history nil
  "History for interactive read for Scala/SBT/REPL evaluations.")

(defun flex-compiler-scala-trim (str)
  "Trim whitespace on both ends of STR."
  (string-match "^\s*\\(.*?\\)\s*$" str)
  (replace-match "\\1" nil nil str))

(defclass scala-flex-compiler (evaluate-flex-compiler)
  ((sbt-eval-mode :initarg :sbt-eval-mode
		  :initform run
		  :type symbol)))

(cl-defmethod initialize-instance ((this scala-flex-compiler) &optional args)
  (oset this :name "scala")
  (oset this :major-mode 'scala-mode)
  (oset this :mode-desc "scala file")
  (oset this :config-file-desc "scala file")
  (oset this :repl-buffer-regexp "\\*sbt\\*.*")
  (oset this :repl-buffer-start-timeout 0)
  (oset this :no-prompt-kill-repl-buffer t)
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-load-libraries ((this scala-flex-compiler))
  (require 'ensime))

(cl-defmethod flex-compiler-config-directory ((this scala-flex-compiler))
  (->> (flex-compiler-config this)
       file-name-directory
       ensime-config-find
       file-name-directory))

(cl-defmethod flex-compiler-scala-repl-buffer-name ((this scala-flex-compiler))
  "*spark-shell*")

(cl-defmethod flex-compiler-scala-repl-buffer ((this scala-flex-compiler))
  (get-buffer (flex-compiler-scala-repl-buffer-name this)))

(cl-defmethod flex-compiler-send-input ((this scala-flex-compiler)
					&optional command)
  (goto-char (point-max))
  (insert command)
					;(sbt-clear)
  )

(cl-defmethod flex-compiler-eval-form-impl ((this scala-flex-compiler) form)
  (error "Not implemented"))

(cl-defmethod flex-compiler-sbt-command ((this scala-flex-compiler) command)
  (with-current-buffer (find-file-noselect (flex-compiler-config this))
    (sbt-command command)))

(cl-defmethod flex-compiler-scala-eval ((this scala-flex-compiler))
  (with-current-buffer (find-file-noselect (flex-compiler-config this))
    (ensime-inf-eval-buffer)
    (display-buffer (flex-compiler-scala-repl-buffer this))))

(cl-defmethod flex-compiler-eval-initial-at-point ((this scala-flex-compiler))
  (error "Not implemented"))

(cl-defmethod flex-compiler-query-read-form ((this scala-flex-compiler))
  (flex-compiler-scala-trim (thing-at-point 'paragraph)))

(cl-defmethod flex-compiler-evaluate-form ((this scala-flex-compiler)
					   &optional form)
  (let ((ensime-inf-buffer-name (flex-compiler-scala-repl-buffer-name this))
	(buf (flex-compiler-scala-repl-buffer this))
	(form (or form (with-current-buffer (flex-compiler-config-buffer this)
			 (buffer-string)))))
    (with-current-buffer buf
      (goto-char (point-max))
      (ensime-inf-send-string form)
      (display-buffer buf))
    nil))

(cl-defmethod flex-compiler-eval-spark ((this scala-flex-compiler))
  (require 'bshell)
  (let* ((mng bshell-manager-singleton)
	 (bname "spark-shell")
	 (bentry (buffer-manager-entry mng bname))
	 (newp (not bentry))
	 (bentry (or bentry
		     (with-current-buffer (flex-compiler-config-buffer this)
		       (let ((start-dir (sbt:find-root)))
			 (buffer-manager-new-entry mng bname start-dir)))))
	 (buf (buffer-entry-buffer bentry)))
					;(display-buffer buf)
    (and newp (buffer-entry-insert bentry "sbt console" t)))
  (flex-compiler-evaluate-form this))

(cl-defmethod flex-compiler-eval-config ((this scala-flex-compiler) file)
  (let ((ensime-inf-buffer-name (flex-compiler-scala-repl-buffer-name this)))
    (let ((mode (slot-value this 'sbt-eval-mode)))
      (cl-case mode
	(eval (flex-compiler-evaluate-form this))
	(eval-spark (flex-compiler-eval-spark this))
	(run (flex-compiler-sbt-command this (concat "run " file)))
	(compile (flex-compiler-sbt-command this "compile"))
	(test (flex-compiler-sbt-command this "test"))))))

(cl-defmethod flex-compiler-query-eval ((this scala-flex-compiler)
					config-option)
  (with-slots (sbt-eval-mode) this
    (setq sbt-eval-mode
	  (choice-program-complete
	   "Evaluation mode" '(run compile test eval eval-spark)
	   nil t nil 'compiler-flex-scala-query-eval-mode-history
	   (cl-second compiler-flex-scala-query-eval-mode-history)
	   nil nil t))))

(cl-defmethod flex-compiler-kill-repl ((this scala-flex-compiler))
  (condition-case err
      (ensime-shutdown)
    (error))
  (condition-case err
      (ensime-inf-quit-interpreter)
    (error))
  (sit-for 1)
  (condition-case nil
      (let ((buf (flex-compiler-scala-repl-buffer)))
	(and buf (kill-buffer buf)))
    (error))
  (->> (process-list)
       (-map (-lambda (proc)
	       (and (string-match "^\\*sbt\\*" (process-name proc))
		    proc)))
       (-filter #'identity)
       (-map #'kill-process))
  (cl-call-next-method this))

(cl-defmethod flex-compiler-rename-repl-buffer ((this scala-flex-compiler))
  (let ((buf (get-buffer ensime-inf-buffer-name))
	(new-name (flex-compiler-scala-repl-buffer-name this)))
    (when buf
      (with-current-buffer buf
	(rename-buffer new-name)))))

(cl-defmethod flex-compiler-repl-start ((this scala-flex-compiler))
  (flex-compiler-sbt-command this "run")
  (when nil
    (with-current-buffer (flex-compiler-config-buffer this)
      (ensime-inf-switch))
    (flex-compiler-rename-repl-buffer this)))


(flex-compile-manager-register the-flex-compile-manager (scala-flex-compiler))

(provide 'flex-compile-scala)

;;; flex-compile-scala.el ends here
