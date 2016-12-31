;;; compile-flex.el --- flexible compile

;; Copyright (C) 2015 - 2017 Paul Landes

;; Version: 0.1
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: compile interpret evaluation
;; URL: https://github.com/plandes/compile-flex
;; Package-Requires: ((choice-program "0.1"))

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

;; Run, evaluate and compile functionality for a variety of different languages
;; and modes.  The specific "compilation" method is different across each
;; add-on library.  For example, for ESS and Clojure you can evaluate a
;; specific file and/or evaluate a specfic expression via a REPL.  For running
;; a script or starting a `make` an async process is started.
;;
;; For more information see https://github.com/plandes/compile-flex

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'eieio-core)
(require 'choice-program-complete)

(defgroup compile-flex nil
  "Compile Helper Functions"
  :group 'tools
  :group 'compilation
  :prefix '"compile-flex")

(defcustom compile-flex-show-repl-mode 'display
  "How to show/switch to the REPL buffer.
`Switch to buffer' means to first pop then switch to the buffer.
`Display buffer' means to show the buffer in a different window."
  :type '(choice (const :tag "Switch to buffer" switch)
		 (const :tag "Display buffer" display))
  :group 'compile-flex)

(defcustom compile-flex-persistency-file-name
  (expand-file-name "compile-flex" user-emacs-directory)
  "File containing the Flex compile configuration data."
  :type 'file
  :group 'compile-flex)



(defclass flex-compiler ()
  ((name :initarg :name
	 :type string)
   (manager :initarg :manager))
  :abstract true
  :documentation "Base class for compilation executors (do the work).")

(defmethod flex-compiler--unimplemented ((this flex-compiler) method)
  (error "Un-implemented method: `flex-compiler-%s' for class `%S'"
	 method (eieio-object-class this)))

(defmethod flex-compiler-load-libraries ((this flex-compiler))
  "Call back for to load and require libraries needed by the compiler.")

(defmethod flex-compiler-name ((this flex-compiler))
  "Return the name of the compiler, which defaults to the `:name` slot."
  (oref this :name))

(defmethod flex-compiler-run ((this flex-compiler))
  "Invoke the run functionality of the compiler."
  (flex-compiler--unimplemented this "run"))

(defmethod flex-compiler-compile ((this flex-compiler))
  "Invoke the compile functionality of the compiler."
  (flex-compiler--unimplemented this "compile"))

(defmethod flex-compiler-clean ((this flex-compiler))
  "Invoke the clean functionality of the compiler."
  (flex-compiler--unimplemented this "clean"))


(defclass no-op-flex-compiler (flex-compiler)
  ()
  :documentation "A no-op compiler For disabled state.")

(defmethod initialize-instance ((this no-op-flex-compiler) &rest rest)
  (oset this :name "disable")
  (apply 'call-next-method this rest))

(defmethod flex-compiler--unimplemented ((this no-op-flex-compiler) method)
  (message "Disabled compiler skipping method `%s'" method))



(defclass config-flex-compiler (flex-compiler)
  ((config-file :initarg :config-file
		:type string
		:documentation "\
The file to use for `configuring' the compiler.")
   (config-file-desc :initarg :config-file-desc
		     :initform "config file"
		     :type string
		     :documentation "\
Description of the configuration file and used in user input prompts.")
   (major-mode :initarg :major-mode "\
The major mode to use to validate/select `config-file` buffers.")
   (mode-desc :initarg :mode-desc
	      :type string))
  :documentation "A configurable compiler with a configuration file.
Instances of this class are also persistable and their state is stored in a
configuration file.")

(defmethod flex-compiler-validate-buffer-file ((this config-flex-compiler))
  "Return an error string if the current buffer isn't the right type of config."
  (if (not (eq major-mode (oref this :major-mode)))
      (format "This doesn't seem like a %s file" (oref this :mode-desc))))

(defmethod flex-compiler--config-variable ((this config-flex-compiler))
  (intern (format "flex-compiler-config-file-%s"
		  (flex-compiler-name this))))

(defmethod flex-compiler-config-persist ((this config-flex-compiler))
  "Persist the state of the compiler instance."
  (let ((conf-file (and (slot-boundp this :config-file)
			(oref this :config-file))))
    (if conf-file `((config-file . ,conf-file)))))

(defmethod flex-compiler-config-unpersist ((this config-flex-compiler) config)
  "Restore the previous state of the compiler instance."
  (oset this :config-file (cdr (assq 'config-file config))))

(defmethod flex-compiler-save-config ((this config-flex-compiler))
  "Tell the compiler manager to persist the configuration of all compilers."
  (with-slots (manager) this
    (flex-compile-manager-config-persist manager)))

(defmethod flex-compiler-set-config ((this config-flex-compiler) &optional file)
  "Set the file for the configuration compiler."
  (unless file
    (let ((errmsg (flex-compiler-validate-buffer-file this)))
      (if errmsg (error errmsg))))
  (setq file (or file (buffer-file-name)))
  (oset this :config-file file)
  (flex-compiler-save-config this)
  (message "Set %s to `%s'" (oref this mode-desc) file))

(defmethod flex-compiler-read-config ((this config-flex-compiler))
  "Read the configuration file from the user."
  (let (file)
    (if (flex-compiler-validate-buffer-file this)
	(let ((conf-desc (oref active :config-file-desc)))
	  (setq file (read-file-name
		      (format "%s: " (capitalize conf-desc))
		      nil (buffer-file-name))))
      file)))

(defmethod flex-compiler-config ((this config-flex-compiler))
  "Validate and return the configuration file."
  (if (not (slot-boundp this :config-file))
      (let ((conf-var (flex-compiler--config-variable this)))
	(if (boundp conf-var)
	    (oset this :config-file (symbol-value conf-var))
	  (error "The %s for compiler %s has not yet been set"
		 (oref this :config-file-desc)
		 (flex-compiler-name this)))))
  (with-slots (config-file config-file-desc) this
    (unless (file-exists-p config-file)
      (error "No such %s: %s" config-file-desc config-file))
    config-file))

(defmethod flex-compiler-config-buffer ((this config-flex-compiler))
  "Return a (new) buffer of the configuration file."
  (let ((file (flex-compiler-config this)))
    (find-file-noselect file)))



(defclass optionable-flex-compiler (flex-compiler)
  ((compile-options :initarg :compile-options
		    :initform nil
		    :type list
		    :documentation "\
A list of options used to configure the run of the compiler.")
   (force-set-compile-options-p :initarg :force-set-compile-options-p
				:initform nil
				:type boolean
				:documentation "\
If non-nil force the user to set this before the compilation run.")))

(defmethod flex-compiler-read-set-options ((this optionable-flex-compiler))
  "Set the `:compile-options' slot of the compiler."
  (let ((args (flex-compiler-read-options this)))
    (if (stringp args)
	(setq args (split-string args)))
    (oset this :compile-options args)))

(defmethod flex-compiler-options ((this optionable-flex-compiler))
  "Validate and return the compiler options."
  (with-slots (compile-options force-set-compile-options-p) this
    (if (and force-set-compile-options-p (null compile-options))
	(error "No options set, use `flex-compile-compile' with argument (C-u)"))
    compile-options))

(defmethod flex-compiler-read-options ((this optionable-flex-compiler))
  "Read the options of the compiler."
  (flex-compiler--unimplemented this "read-options"))


(defclass run-args-flex-compiler (config-flex-compiler optionable-flex-compiler)
  ()
  :documentation "A configurable and optionable compiler.")

(defmethod flex-compiler-run-with-args ((this run-args-flex-compiler) args)
  "Run the compilation with given arguments."
  (flex-compiler--unimplemented this "run-with-args"))

(defmethod flex-compiler--post-config ((this run-args-flex-compiler)))

(defmethod flex-compiler-compile ((this run-args-flex-compiler))
  "Run the compiler with user provided arguments."
  (flex-compiler-run-with-args this (flex-compiler-options this)))

(defmethod flex-compiler-run ((this run-args-flex-compiler))
  "Configure compiler by prompting for user given arguments."
  (flex-compiler-set-config this)
  (flex-compiler--post-config this))


(defclass repl-flex-compiler (flex-compiler)
  ((repl-buffer-regexp :initarg :repl-buffer-regexp
		       :type string
		       :documentation "\
Regular expression to match buffers for functions like killing the session.")
   (derived-buffer-names :initarg :derived-buffer-names
			 :initform nil
			 :type list
			 :documentation "\
List of buffers for functions (like killing a buffer) when session ends.")
   (repl-buffer-start-timeout :initarg :repl-buffer-start-timeout
			      :initform 1
			      :type integer
			      :documentation "\
Number of seconds to wait to start before giving up (and not displaying).
If this is 0, don't wait or display the buffer when it comes up.")
   (no-prompt-kill-repl-buffer :initarg :no-prompt-kill-repl-buffer
			       :initform nil
			       :type boolean
			       :documentation "\
If non-`nil' then don't prompt to kill a REPL buffer on clean.")))

(defmethod flex-compiler-repl-start ((this repl-flex-compiler))
  "Start the REPL."
  (flex-compiler--unimplemented this "start-repl"))

(defmethod flex-compiler-repl-compile ((this repl-flex-compiler))
  "Invoked by `compile' type messages from the compiler manager.

This method is meant to allow for REPL compiles \(really some kind of
evaluation), while allowing base class compilation features.."
  (flex-compiler--unimplemented this "repl-compile"))

(defmethod flex-compiler-wait-for-buffer ((this repl-flex-compiler))
  "Wait for the compilation to start.

The caller raises and error if it doesn't start in time."
  (let ((count-down (oref this :repl-buffer-start-timeout))
	buf)
    (dotimes (i count-down)
      (setq buf (flex-compiler-repl-buffer this))
      (if buf
	(return buf)
	(message "Waiting for buffer to start... (%d)"
		 (- count-down i))
	(sit-for 1)))))

(defmethod flex-compiler-repl-running-p ((this repl-flex-compiler))
  "Return whether or not the REPL is currently running."
  (not (null (flex-compiler-repl-buffer this))))

(defmethod flex-compiler-repl-assert-running ((this repl-flex-compiler))
  "Raise an error if the REPL IS running."
  (unless (flex-compiler-repl-running-p this)
    (error "The REPL for %s isn't started" (flex-compiler-name this))))

(defmethod flex-compiler-repl-assert-not-running ((this repl-flex-compiler))
  "Raise an error if the REPL isn't running."
  (if (flex-compiler-repl-running-p this)
      (error "Compiler %s is already running"
	     (flex-compiler-name this))))

(defmethod flex-compiler-repl--run-start ((this repl-flex-compiler))
  (let ((timeout (oref this :repl-buffer-start-timeout))
	buf)
    (unless (flex-compiler-repl-running-p this)
      (flex-compiler-repl-start this)
      (when (> timeout 0)
	(setq buf (flex-compiler-wait-for-buffer this))
	(unless buf
	  (error "Couldn't create REPL for compiler %s"
		 (flex-compiler-name this)))))))

(defmethod flex-compiler-run ((this repl-flex-compiler))
  "Start the REPL and display it."
  (flex-compiler-repl--run-start this)
  (flex-compiler-repl-display this))

(defmethod flex-compiler-repl-display ((this repl-flex-compiler))
  "Show the REPL in an adjacent buffer or the current buffer."
  (let ((buf (flex-compiler-repl-buffer this))
	fn)
    (when buf
      (setq fn (cl-case compile-flex-show-repl-mode
		 (switch 'pop-to-buffer)
		 (display 'display-buffer)))
      (funcall fn buf)
      buf)))

(defmethod flex-compiler-send-input ((this repl-flex-compiler)
				     &optional command)
  "Send input/commands to the REPL."
  (goto-char (point-max))
  (insert command)
  (comint-send-input))

(defmethod flex-compiler-run-command ((this repl-flex-compiler)
				      &optional command)
  "Send commands to the REPL to evaluate an expression or start a process."
  (flex-compiler-repl-assert-running this)
  (let ((buf (flex-compiler-repl-buffer this))
	pos win)
    (with-current-buffer buf
      (if command
	  (flex-compiler-send-input this command)))))

(defmethod flex-compiler-compile ((this repl-flex-compiler))
  "Start the REPL (if not alrady started) and invoke the compile callback."
  (let ((file (flex-compiler-config this))
	(runningp (flex-compiler-repl-running-p this)))
    (unless runningp
      (flex-compiler-run this))
    (if (flex-compiler-repl-running-p this)
	(flex-compiler-repl-compile this)
      (if runningp
	  (error "REPL hasn't started")
	(message "REPL still starting, please wait")))))

(defmethod flex-compiler-clean ((this repl-flex-compiler))
  "`Clean' by killing the REPL."
  (flex-compiler-kill-repl this))

(defmethod flex-compiler-repl-buffer ((this repl-flex-compiler))
  "Find the first REPL buffer found in the buffer list."
  (dolist (buf (buffer-list))
    (let ((buf-name (buffer-name buf)))
      (when (string-match (oref this :repl-buffer-regexp) buf-name)
	(return buf)))))

(defmethod flex-compiler-kill-repl ((this repl-flex-compiler))
  "Kill the compiler's REPL."
  (let ((bufs (append (mapcar 'get-buffer (oref this :derived-buffer-names))
		      (cons (flex-compiler-repl-buffer this) nil)))
	(count 0))
    (dolist (buf bufs)
      (when (buffer-live-p buf)
	(let ((kill-buffer-query-functions
	       (if (oref this :no-prompt-kill-repl-buffer)
		   nil
		 kill-buffer-query-functions)))
	  (kill-buffer buf))
	(incf count)))
    (message "%s killed %d buffer(s)"
	     (capitalize (oref this :name)) count)))


(defvar flex-compiler-query-eval-mode nil)
(defvar flex-compiler-query-eval-form nil)

(defclass evaluate-flex-compiler (config-flex-compiler repl-flex-compiler)
  ((eval-mode :initarg :eval-mode
	      :initform eval-config
	      :type symbol
	      :documentation "
Mode of evaluating expressions with the REPL.
One of:
- `eval-config': evaluate the configuration file
- `buffer': evaluate in the REPL buffer
- `minibuffer': evaluate form with results to minibuffer
- `copy': like `minibuffer' but the results also go to the kill ring
- `eval-repl': evaluate a form in the repl
- `both-eval-minibuffer': invokes both `eval-config' and `minibuffer' in that order
- `both-eval-repl': invokes both `eval-config' and `eval-repl' in that order")
   (form :initarg :form
	 :type string
	 :documentation "The form to send to the REPL to evaluate.")
   (last-eval :initarg :last-eval
	      :documentation "The value of the last evaluation.")
   (show-repl-after-eval-p :initarg :show-repl-after-eval-p
			   :initform nil
			   :type boolean
			   :documentation "\
Whether or not to show the buffer after the file is evlauated or not.")))

(defmethod flex-compiler-eval-form-impl ((this evaluate-flex-compiler) form)
  (flex-compiler--unimplemented this "eval-form-impl"))

(defmethod flex-compiler-eval-config ((this evaluate-flex-compiler) file)
  (flex-compiler--unimplemented this "eval-config"))

(defmethod flex-compiler-eval-initial-at-point ((this evaluate-flex-compiler))
  nil)

(defmethod flex-compiler-query-read-form ((this evaluate-flex-compiler))
  "Read a form, meaningful for the compiler, from the user."
  (let ((init (flex-compiler-eval-initial-at-point this)))
    (read-string "Form: " init 'flex-compiler-query-eval-form)))

(defmethod flex-compiler-query-eval ((this evaluate-flex-compiler))
  "Prompt the user for the evaluation mode \(see the `:eval-mode' slot)."
  (with-slots (eval-mode form) this
    (setq eval-mode
	  (choice-program-complete
	   "Evaluation mode"
	   '(eval-config buffer minibuffer
			 copy eval-repl both-eval-repl both-eval-minibuffer)
	   nil t nil 'flex-compiler-query-eval-mode
	   (or (cl-first flex-compiler-query-eval-mode) 'minibuffer) nil nil t))
    (when (memq eval-mode '(minibuffer copy eval-repl
				       both-eval-repl both-eval-minibuffer))
      (let ((init (flex-compiler-eval-initial-at-point this)))
	(setq form
	      (read-string "Form: " init 'flex-compiler-query-eval-form))))))

(defmethod flex-compiler-evaluate-form ((this evaluate-flex-compiler)
					&optional form)
  "Return the evaluation form.

See the `:eval-form' slot."
  (if (and (null form) (not (slot-boundp this :form)))
      (flex-compiler-query-eval this))
  (let ((res (flex-compiler-eval-form-impl this (or form (oref this :form)))))
    (oset this :last-eval res)
    (if (stringp res)
	res
      (prin1-to-string res))))

(defmethod flex-comiler-repl--eval-config-and-show ((this evaluate-flex-compiler))
  (flex-compiler-repl--run-start this)
  (flex-compiler-eval-config this (flex-compiler-config this))
  (when (oref this :show-repl-after-eval-p)
    (flex-compiler-repl-display this)))

(defmethod flex-compiler-repl-compile ((this evaluate-flex-compiler))
  "Invoke the compilation based on the `eval-mode' slot."
  (noflet
    ((invoke-mode
      (mode)
      (case mode
	(eval-config (flex-comiler-repl--eval-config-and-show this))
	(buffer (flex-compiler-repl-display this))
	(minibuffer (let ((res (flex-compiler-evaluate-form this)))
		      (message res)
		      res))
	(copy (let ((res (flex-compiler-evaluate-form this)))
		(kill-new res)
		(message res)
		res))
	(eval-repl (progn
		     (flex-compiler-run-command this (oref this :form))
		     (flex-compiler-repl-display this)))
	(both-eval-repl (invoke-mode 'eval-config)
			(invoke-mode 'eval-repl))
	(both-eval-minibuffer (invoke-mode 'eval-config)
			      (invoke-mode 'minibuffer)))))
    (invoke-mode (oref this :eval-mode))))


;;; compiler manager/orchestration

(defclass flex-compile-manager ()
  ((active :initarg :active
	   :initform (no-op-flex-compiler nil)
	   :type flex-compiler
	   :documentation "The currently active/selected compiler.")
   (compilers :initarg :compilers
	      :initform (list (no-op-flex-compiler nil))
	      :type list
	      :documentation "The list of compiler instances."))
  :documentation "Manages flexible compiler instances.")

(defmethod flex-compile-manager-register ((this flex-compile-manager) compiler)
  "Register a compiler instance with the manager \(compilation framework)."
  (with-slots (compilers) this
    (setq compilers
	  (cl-delete compiler compilers
		     :test #'(lambda (a b)
			       (equal (flex-compiler-name a)
				      (flex-compiler-name b)))))
    (setq compilers (append compilers (cons compiler nil)))
    (oset compiler :manager this)
    (let* ((name (flex-compiler-name compiler))
	   (config (flex-compile-manager-compiler-config this name)))
      (when config
	(flex-compiler-config-unpersist compiler config)))))

(defmethod flex-compile-manager-compiler-names ((this flex-compile-manager))
  "Return the names of all registered compilers."
  (mapcar #'flex-compiler-name (oref this :compilers)))

(defmethod flex-compile-manager-compiler ((this flex-compile-manager) name)
  "Return the compiler instance with NAME or nil if not registered."
  (dolist (compiler (oref this :compilers))
    (if (equal name (flex-compiler-name compiler))
	(return compiler))))

(defmethod flex-compile-manager-active ((this flex-compile-manager))
  "Return the currently selected or active manager."
  (oref this :active))

(defmethod flex-compile-manager-activate ((this flex-compile-manager) name)
  "Activate/select the registered compiler with NAME."
  (let ((compiler (flex-compile-manager-compiler this name)))
    (if (null compiler)
	(error "Compiler `%s' not found" name))
    (oset this :active compiler)
    (message "Active compiler is now %s" (flex-compiler-name compiler))
    compiler))

(defmethod flex-compile-manager-settable ((this flex-compile-manager))
  "Return whether the currently active/selected compiler is configurable.
In other words, if it extends `config-flex-compiler'."
  (with-slots (active) this
    (child-of-class-p (eieio-object-class active) 'config-flex-compiler)))

(defmethod flex-compile-manager-set-config ((this flex-compile-manager)
					    &optional file)
  "Set the configuration file of the currently active/selected compiler.

If it isn't settable, warn the user with a message and do nothing."
  (with-slots (active) this
    (if (flex-compile-manager-settable this)
	(flex-compiler-set-config active file)
      (message "Compiler `%s' not settable" (flex-compiler-name active)))))

(defmethod flex-compile-manager-assert-ready ((this flex-compile-manager))
  "Make sure the active/selected compiler is ready and libraries loaded."
  (with-slots (active) this
    (flex-compiler-load-libraries active)))

(defmethod flex-compile-manager-config ((this flex-compile-manager))
  "Return all configurable compilers."
  (let ((this the-flex-compile-manager))
    (->> (oref this :compilers)
	 (remove-if-not #'(lambda (comp)
			    (-> (eieio-object-class comp)
				(child-of-class-p 'config-flex-compiler))))
	 (mapcar #'(lambda (comp)
		     (let ((conf (flex-compiler-config-persist comp)))
		       (and conf (cons (flex-compiler-name comp) conf)))))
	 (remove nil))))

(defmethod flex-compile-manager-config-persist ((this flex-compile-manager))
  "Persist manager and compiler configuration."
  (let ((fname compile-flex-persistency-file-name))
    (with-temp-buffer
      (insert
       ";; -*- emacs-lisp -*-"
       (condition-case nil
	   (progn
	     (format
	      " <%s %s>\n"
	      (time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
	      fname))
	 (error "\n"))
       ";; Flex compiler configuration.  Don't change this file.\n"
       (with-output-to-string
	 ;; save manager configuration for future iterations
	 (pp (append (list (cons 'manager nil))
		     (list (cons 'compilers
				 (flex-compile-manager-config this)))))))
      (write-region (point-min) (point-max) fname)
      (message "Wrote %s" compile-flex-persistency-file-name))))

(defmethod flex-compile-manager-persisted-config ((this flex-compile-manager))
  "Persist the configuration of all configurable managers."
  (let ((this the-flex-compile-manager)
	(fname compile-flex-persistency-file-name))
    (when (file-exists-p fname)
      (with-temp-buffer
	(insert-file-contents fname)
	(read (current-buffer))))))

(defmethod flex-compile-manager-compiler-config ((this flex-compile-manager)
						 name)
  "Read and parse the compilers configuration from the config file."
  (->> (flex-compile-manager-persisted-config this)
       (assq 'compilers) cdr (assoc name) cdr))

(defmethod flex-compile-manager-config-unpersist ((this flex-compile-manager))
  "Unpersist manager and compiler configuration."
  (->> (flex-compile-manager-persisted-config the-flex-compile-manager)
       (assq 'compilers)
       cdr
       (mapcar #'(lambda (conf)
		   (let ((comp (flex-compile-manager-compiler
				this (car conf))))
		     (when comp
		       (flex-compiler-config-unpersist comp conf)))))))

(defun flex-compiler-config-save ()
  "Save all compiler and manager configuration."
  (interactive)
  (flex-compile-manager-config-persist the-flex-compile-manager))

(defun flex-compiler-config-load ()
  "Load all compiler and manager configuration."
  (interactive)
  (flex-compile-manager-config-unpersist the-flex-compile-manager))

(defun flex-compiler-by-name (name)
  "Convenience function to get a compiler by it's NAME."
  (flex-compile-manager-compiler the-flex-compile-manager name))

(defvar the-flex-compile-manager
  (flex-compile-manager "singleton")
  "The singleton manager instance.")



;;; interactive functions
(defvar flex-compiler-read-history nil)

(defun flex-compiler-read (last-compiler-p)
  "Read a flexible compiler to use.

LAST-COMPILER-P, if non-nil, use the last chosen compiler."
  (or (if last-compiler-p
	  (let ((arg (cl-second flex-compiler-read-history)))
	    (setq flex-compiler-read-history
		  (append (cons arg nil) flex-compiler-read-history))
	    arg))
      (let* ((this the-flex-compile-manager)
	     (names (flex-compile-manager-compiler-names this)))
	(choice-program-complete "Compiler" names t t nil
				'flex-compiler-read-history
				(cl-second flex-compiler-read-history)
				nil t t))))

;;;###autoload
(defun flex-compiler-activate (compiler-name)
  "Activate/select a compiler.

COMPILER-NAME the name of the compiler to activate."
  (interactive (list (flex-compiler-read current-prefix-arg)))
  (let ((this the-flex-compile-manager))
    (flex-compile-manager-activate this compiler-name)))

;;;###autoload
(defun flex-compile-compile (config-options-p)
  "Invoke compilation polymorphically.

CONFIG-OPTIONS-P, if non-nil invoke the configuration options for the compiler
before invoking the compilation."
  (interactive "P")
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (flex-compile-manager-assert-ready this)
    (when config-options-p
      (cond ((child-of-class-p (eieio-object-class active)
			       'optionable-flex-compiler)
	     (flex-compiler-read-set-options active))
	    ((child-of-class-p (eieio-object-class active)
			       'evaluate-flex-compiler)
	     (flex-compiler-query-eval active))))
    (flex-compiler-compile active)))

;;;###autoload
(defun flex-compile-run-or-set-config (action)
  "This either invokes the `run' compilation functionality or it configures it.

ACTION is the interactive argument given by the read function."
  (interactive
   (list (cond ((null current-prefix-arg) 'run)
	       ;; universal arg
	       ((equal '(4) current-prefix-arg) 'find)
	       ((eq 1 current-prefix-arg) 'set-config)
	       (t (error "Unknown prefix state: %s" current-prefix-arg)))))
  (cl-flet ((assert-settable
	     ()
	     (if (not (flex-compile-manager-settable this))
		 (error "Not settable compiler: %s"
			(flex-compiler-name active)))))
    (let* ((this the-flex-compile-manager)
	   (active (flex-compile-manager-active this)))
      (flex-compile-manager-assert-ready this)
      (cl-case action
	(run (flex-compiler-run active))
	(find (assert-settable)
	      (pop-to-buffer (flex-compiler-config-buffer active)))
	(set-config (let ((file (flex-compiler-read-config active)))
		      (flex-compile-manager-set-config this file)))
	(t (error "Unknown action: %S" action))))))

(defun flex-compile-read-form ()
  "Read the compilation query form from the user.

This invokes the `flex-compiler-query-read-form method' on the
currently activated compiler."
  (let* ((mgr the-flex-compile-manager)
	 (this (flex-compile-manager-active mgr)))
    (flex-compile-manager-assert-ready mgr)
    (flex-compiler-query-read-form this)))

;;;###autoload
(defun flex-compile-eval (&optional form)
  "Evaluate the current form for the \(usually REPL based compiler).
FORM is the form to evaluate \(if implemented)."
  (interactive (list (flex-compile-read-form)))
  (let* ((mgr the-flex-compile-manager)
	 (this (flex-compile-manager-active mgr))
	 (form (or form (flex-compile-read-form))))
    (flex-compile-manager-assert-ready mgr)
    (let ((res (flex-compiler-evaluate-form this form)))
      (when (and res (interactive-p))
	(kill-new res)
	(message "%s" res))
      res)))

;;;###autoload
(defun flex-compile-clean ()
  "Invoke the clean functionality of the compiler."
  (interactive)
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (flex-compile-manager-assert-ready this)
    (flex-compiler-clean active)))

(provide 'compile-flex)

;;; compile-flex.el ends here
