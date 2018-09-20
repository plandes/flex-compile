;;; flex-compile-manage.el --- manager for flexible compilers

;; Copyright (C) 2015 - 2017 Paul Landes

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

;; Run, evaluate and compile functionality for a variety of different languages
;; and modes.  The specific "compilation" method is different across each
;; add-on library.  For example, for ESS and Clojure you can evaluate a
;; specific file and/or evaluate a specfic expression via a REPL.  For running
;; a script or starting a `make` an async process is started.
;;
;; For more information see https://github.com/plandes/flex-compile

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'eieio)
(require 'time-stamp)
(require 'config-manage)
(require 'choice-program-complete)

(defgroup flex-compile nil
  "Compile Helper Functions"
  :group 'tools
  :group 'compilation
  :prefix '"flex-compile")

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

(defvar flex-compiler-query-eval-mode nil
  "History variable for `flex-compiler-query-eval'.")

(defvar flex-compiler-query-eval-form nil
  "History variable for `flex-compiler-query-read-form'.")



(defclass flex-compiler (config-entry)
  ()
  :abstract true
  :documentation "Base class for compilation executors (do the work).
Instances of this class are also persistable and their state is stored in a
configuration file.")

(define-error 'flex-compiler-un-implemented
  "Un-implemented method flex-compiler method"
  'cl-no-applicable-method)

(cl-defmethod flex-compiler--unimplemented ((this flex-compiler) method)
  (with-temp-buffer
    (set-buffer (get-buffer-create "*flex-compiler-backtrace*"))
    (erase-buffer)
    (let ((standard-output (current-buffer)))
      (backtrace)))
  (signal 'flex-compiler-un-implemented
	  (list method this)))

(cl-defmethod flex-compiler-load-libraries ((this flex-compiler))
  "Call back for to load and require libraries needed by the compiler.")

(cl-defmethod flex-compiler-save-config ((this flex-compiler))
  "Tell the compiler manager to persist the configuration of all compilers."
  (with-slots (manager) this
    (unless manager
      (error "No manager set in compiler: %S" (object-print this)))
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
  :documentation "A no-op compiler For disabled state.")

(cl-defmethod initialize-instance ((this no-op-flex-compiler) &optional args)
  (oset this :name "disable")
  (cl-call-next-method this args))

(cl-defmethod flex-compiler--unimplemented ((this no-op-flex-compiler) method)
  (message "Disabled compiler skipping method `%s'" method))



(defclass directory-start-flex-compiler (flex-compiler)
  ((start-directory :initarg :start-directory
		    :initform nil
		    :type (or null string)
		    :documentation "\
The directory for starting the compilation.  The `default-directory' is set to
this when the compile starts"))
  :abstract true
  :documentation "Compiles starting in a directory")

(cl-defmethod initialize-instance ((this directory-start-flex-compiler)
				   &optional args)
  (with-slots (pslots) this
    (setq pslots (append pslots '(start-directory))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-read-start-directory ((this directory-start-flex-compiler))
  "Read a directory from the user and set it to slot `start-directory'."
  (with-slots (start-directory) this
    (let* ((last start-directory)
	   (dir (read-directory-name "Start directory: " last nil t)))
      (setq start-directory dir)))
  (flex-compiler-save-config this))



(defclass config-flex-compiler (flex-compiler)
  ((config-file :initarg :config-file
		:initform nil
		:type (or null string)
		:documentation "\
The file to use for `configuring' the compiler.")
   (config-file-desc :initarg :config-file-desc
		     :initform "config file"
		     :type string
		     :documentation "\
Description of the configuration file and used in user input prompts.")
   (major-mode :initarg :major-mode
	       :type (or null symbol)
	       :documentation "\
The major mode to use to validate/select `config-file` buffers.")
   (mode-desc :initarg :mode-desc
	      :type string))
  :documentation "A configurable compiler with a configuration file.")

(cl-defmethod initialize-instance ((this config-flex-compiler) &optional args)
  (with-slots (pslots) this
    (setq pslots (append pslots '(config-file))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-validate-buffer-file ((this config-flex-compiler))
  "Return an error string if the current buffer isn't the right type of config."
  (let ((buffer-major-mode major-mode))
    (with-slots (mode-desc major-mode) this
      (if (not (eq buffer-major-mode major-mode))
	  (format "This doesn't seem like a %s file" mode-desc)))))

(cl-defmethod flex-compiler--config-variable ((this config-flex-compiler))
  (intern (format "flex-compiler-config-file-%s"
		  (config-entry-name this))))

(cl-defmethod flex-compiler-set-config ((this config-flex-compiler)
					&optional file)
  "Set the file for the configuration compiler."
  (unless file
    (let ((errmsg (flex-compiler-validate-buffer-file this)))
      (if errmsg (error errmsg))))
  (setq file (or file (buffer-file-name)))
  (flex-compiler-reset-state this)
  (oset this :config-file file)
  (flex-compiler-save-config this)
  (message "Set %s to `%s'" (oref this mode-desc) file))

(cl-defmethod flex-compiler-read-config ((this config-flex-compiler))
  "Read the configuration file from the user."
  (let (file)
    (with-slots (config-file-desc) this
      (if (flex-compiler-validate-buffer-file this)
	  (let ((conf-desc config-file-desc))
	    (setq file (read-file-name
			(format "%s: " (capitalize conf-desc))
			nil (buffer-file-name))))
	file))))

(cl-defmethod flex-compiler-config ((this config-flex-compiler))
  "Validate and return the configuration file."
  (with-slots (config-file config-file-desc) this
    (if (not config-file)
	(let ((conf-var (flex-compiler--config-variable this)))
	  (if (boundp conf-var)
	      (setq config-file (symbol-value conf-var))
	    (error "The %s for compiler %s has not yet been set"
		   config-file-desc
		   (config-entry-name this))))))
  (with-slots (config-file config-file-desc) this
    (unless (file-exists-p config-file)
      (error "No such %s: %s" config-file-desc config-file))
    config-file))

(cl-defmethod flex-compiler-config-buffer ((this config-flex-compiler))
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

(cl-defmethod initialize-instance ((this optionable-flex-compiler)
				   &optional args)
  (with-slots (pslots) this
    (setq pslots (append pslots '(compile-options))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-read-set-options ((this optionable-flex-compiler)
					      config-options)
  "Set the `:compile-options' slot of the compiler.

CONFIG-OPTIONS is the numeric argument (if any) passed in the iteractive mode
with \\[universal-argument]."
  (let ((args (flex-compiler-read-options this)))
    (if (stringp args)
	(setq args (split-string args)))
    (oset this :compile-options args)
    (flex-compiler-save-config this)))

(cl-defmethod flex-compiler-options ((this optionable-flex-compiler))
  "Validate and return the compiler options."
  (with-slots (compile-options force-set-compile-options-p) this
    (if (and force-set-compile-options-p (null compile-options))
	(error "No options set, use `flex-compile-compile' with argument (C-u)"))
    compile-options))

(cl-defmethod flex-compiler-read-options ((this optionable-flex-compiler))
  "Read the options of the compiler."
  (flex-compiler--unimplemented this "read-options"))



(defclass single-buffer-flex-compiler (flex-compiler)
  ()
  :documentation "A flex compiler that has a single buffer.")

(cl-defmethod flex-compiler-buffer ((this single-buffer-flex-compiler))
  "Return non-nil if there exists buffer for this compiler and is live."
  (flex-compiler--unimplemented this "buffer"))

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
  (flet ((display-nf
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
	      (let ((display-buffer-fallback-action nil)
		    (action ))
		(if (= 1 (length (window-list)))
		    (split-window))
		(or (display-buffer buf '(display-buffer-reuse-window
					  ((inhibit-switch-frame . t)
					   (allow-no-window . t))))
		    (display-buffer buf '(display-buffer-use-some-window
					  ((inhibit-switch-frame . t)))))))))
     (case mode
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

(cl-defmethod flex-compiler-single-buffer--flex-start
    ((this single-buffer-flex-compiler) start-type)
  (let ((has-buffer-p (flex-compiler-buffer this))
	(buf (flex-compiler-start-buffer this start-type)))
    `((newp . ,(not has-buffer-p))
      (buffer . ,buf))))

(cl-defmethod flex-compiler-compile ((this single-buffer-flex-compiler))
  (flex-compiler-single-buffer--flex-start this 'compile))

(cl-defmethod flex-compiler-run ((this single-buffer-flex-compiler))
  (flex-compiler-single-buffer--flex-start this 'run))

(cl-defmethod flex-compiler-clean ((this single-buffer-flex-compiler))
  (flex-compiler-single-buffer--flex-start this 'clean))



(defclass run-args-flex-compiler
  (config-flex-compiler optionable-flex-compiler single-buffer-flex-compiler)
  ()
  :documentation "A configurable and optionable compiler.")

(cl-defmethod flex-compiler-run-with-args ((this run-args-flex-compiler)
					   args start-type)
  "Run the compilation with given arguments."
  (flex-compiler--unimplemented this "run-with-args"))

(cl-defmethod flex-compiler--post-config ((this run-args-flex-compiler)))

(cl-defmethod flex-compiler-start-buffer ((this run-args-flex-compiler)
					  start-type)
  (let* ((args (flex-compiler-options this))
	 (buf (flex-compiler-run-with-args this args start-type)))
    ;; find a better way to do this: PL 9/19/2018
    ;; (when (eq start-type 'run)
    ;;   (flex-compiler-set-config this)
    ;;   (flex-compiler--post-config this))
    buf))


(defclass directory-run-flex-compiler (run-args-flex-compiler
				       directory-start-flex-compiler)
  ()
  :documentation "Run arguments compilation starting in directory.")

(cl-defmethod flex-compiler-read-set-options ((this directory-run-flex-compiler)
					      config-options)
  "Set the `:compile-options' slot of the compiler.

CONFIG-OPTIONS is the numeric argument (if any) passed in the iteractive mode
with \\[universal-argument]."
  (flex-compiler-read-start-directory this)
  (cl-call-next-method this config-options))



(defclass repl-flex-compiler
  (directory-start-flex-compiler single-buffer-flex-compiler)
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
If non-`nil' then don't prompt to kill a REPL buffer on clean."))
  :documentation "Compiles by evaluating expressions in the REPL.")

(cl-defmethod flex-compiler-repl-start ((this repl-flex-compiler))
  "Start the REPL."
  (flex-compiler--unimplemented this "start-repl"))

(cl-defmethod flex-compiler-repl-compile ((this repl-flex-compiler))
  "Invoked by `compile' type messages from the compiler manager.

This method is meant to allow for REPL compiles \(really some kind of
evaluation), while allowing base class compilation features.."
  (flex-compiler--unimplemented this "repl-compile"))

(cl-defmethod flex-compiler-wait-for-buffer ((this repl-flex-compiler))
  "Wait for the compilation to start.

The caller raises and error if it doesn't start in time."
  (with-slots (repl-buffer-start-timeout) this
    (let ((count-down repl-buffer-start-timeout)
	  buf)
      (dotimes (i count-down)
	(setq buf (flex-compiler-buffer this))
	(if buf
	    (cl-return buf)
	  (message "Waiting for buffer to start... (%d)"
		   (- count-down i))
	  (sit-for 1))))))

(cl-defmethod flex-compiler-repl-running-p ((this repl-flex-compiler))
  "Return whether or not the REPL is currently running."
  (not (null (flex-compiler-buffer this))))

(cl-defmethod flex-compiler-repl-assert-running ((this repl-flex-compiler))
  "Raise an error if the REPL IS running."
  (unless (flex-compiler-repl-running-p this)
    (error "The REPL for %s isn't started" (config-entry-name this))))

(cl-defmethod flex-compiler-repl-assert-not-running ((this repl-flex-compiler))
  "Raise an error if the REPL isn't running."
  (if (flex-compiler-repl-running-p this)
      (error "Compiler %s is already running"
	     (config-entry-name this))))

(cl-defmethod flex-compiler-repl--run-start ((this repl-flex-compiler))
  (with-slots (repl-buffer-start-timeout start-directory) this
    (let ((timeout repl-buffer-start-timeout)
	  buf)
      (unless (flex-compiler-repl-running-p this)
	(let ((default-directory (or start-directory default-directory)))
	  (flex-compiler-repl-start this))
	(when (> timeout 0)
	  (setq buf (flex-compiler-wait-for-buffer this))
	  (unless buf
	    (error "Couldn't create REPL for compiler %s"
		   (config-entry-name this))))))))

(cl-defmethod flex-compiler-send-input ((this repl-flex-compiler)
					&optional command)
  "Send input/commands to the REPL."
  (goto-char (point-max))
  (insert command)
  (comint-send-input))

(cl-defmethod flex-compiler-run-command ((this repl-flex-compiler)
					 &optional command)
  "Send commands to the REPL to evaluate an expression or start a process."
  (flex-compiler-repl-assert-running this)
  (let ((buf (flex-compiler-buffer this))
	pos win)
    (with-current-buffer buf
      (if command
	  (flex-compiler-send-input this command)))))

(cl-defmethod flex-compiler-buffer ((this repl-flex-compiler))
  "Find the first REPL buffer found in the buffer list."
  (with-slots (repl-buffer-regexp) this
    (dolist (buf (buffer-list))
      (let ((buf-name (buffer-name buf)))
	(when (string-match repl-buffer-regexp buf-name)
	  (cl-return buf))))))

(cl-defmethod flex-compiler-kill-repl ((this repl-flex-compiler))
  "Kill the compiler's REPL."
  (with-slots (name derived-buffer-names no-prompt-kill-repl-buffer) this
    (let ((bufs (append (mapcar 'get-buffer derived-buffer-names)
			(cons (flex-compiler-buffer this) nil)))
	  (count 0))
      (dolist (buf bufs)
	(when (buffer-live-p buf)
	  (let ((kill-buffer-query-functions
		 (if no-prompt-kill-repl-buffer
		     nil
		   kill-buffer-query-functions)))
	    (kill-buffer buf))
	  (cl-incf count)))
      (message "%s killed %d buffer(s)" (capitalize name) count))))

(cl-defmethod flex-compiler-start-buffer ((this repl-flex-compiler) start-type)
  (let ((file (flex-compiler-config this))
	(runningp (flex-compiler-repl-running-p this)))
    (case start-type
      (compile (progn
		 (unless runningp
		   (flex-compiler-run this))
		 (if (flex-compiler-repl-running-p this)
		     (flex-compiler-repl-compile this)
		   (if runningp
		       (error "REPL hasn't started")
		     (message "REPL still starting, please wait")))
		 (flex-compiler-buffer this)))
      (run (progn
	     (flex-compiler-repl--run-start this)
	     (flex-compiler-buffer this)))
      (clean (progn
	       (flex-compiler-kill-repl this)
	       'killed-buffer)))))



(defvar flex-compiler-query-eval-mode nil)
(defvar flex-compiler-query-eval-form nil)

(defclass evaluate-flex-compiler
  (config-flex-compiler repl-flex-compiler single-buffer-flex-compiler)
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
	      :documentation "The value of the last evaluation.")))

(cl-defmethod flex-compiler-eval-form-impl ((this evaluate-flex-compiler) form)
  (flex-compiler--unimplemented this "eval-form-impl"))

(cl-defmethod flex-compiler-eval-config ((this evaluate-flex-compiler) file)
  (flex-compiler--unimplemented this "eval-config"))

(cl-defmethod flex-compiler-eval-initial-at-point ((this evaluate-flex-compiler))
  nil)

(cl-defmethod flex-compiler-query-read-form ((this evaluate-flex-compiler)
					     no-input-p)
  "Read a form, meaningful for the compiler, from the user."
  (let ((init (flex-compiler-eval-initial-at-point this)))
    (if no-input-p
	init
      (read-string "Form: " init 'flex-compiler-query-eval-form))))

(cl-defmethod flex-compiler-query-eval ((this evaluate-flex-compiler)
					config-options)
  "Prompt the user for the evaluation mode \(see the `:eval-mode' slot).

CONFIG-OPTIONS is the numeric argument (if any) passed in the iteractive mode
with \\[universal-argument].

For this implementation, if 1 is given, then prompt for a start directory.
This directory is used to set the `default-directory', which is inherated in
the compilation process \(if any)."
  (if (equal config-options 1)
      (flex-compiler-read-start-directory this))
  (when (or (not config-options) (equal config-options '(4)))
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
		(read-string "Form: " init 'flex-compiler-query-eval-form)))))))

(cl-defmethod flex-compiler-evaluate-form ((this evaluate-flex-compiler)
					   &optional form)
  "Return the evaluation form.

See the `:eval-form' slot."
  (if (and (null form) (not (slot-boundp this :form)))
      (flex-compiler-query-eval this nil))
  (let ((res (flex-compiler-eval-form-impl this (or form (slot-value this 'form)))))
    (oset this :last-eval res)
    (if (stringp res)
	res
      (prin1-to-string res))))

(cl-defmethod flex-compiler-repl--eval-config-and-show ((this evaluate-flex-compiler))
  (flex-compiler-repl--run-start this)
  (flex-compiler-eval-config this (flex-compiler-config this)))

(cl-defmethod flex-compiler-repl--invoke-mode ((this evaluate-flex-compiler) mode)
  "Invoke a flex compilation by mnemonic.

MODE is the compilation mnemonic, which can range from evaluating a buffer,
form from a minibuffer and from the REPL directly."
  (cl-case mode
    (eval-config (flex-compiler-repl--eval-config-and-show this))
    (buffer (error "No longer implemented--no flex-compiler-repl-display"))
    (minibuffer (let ((res (flex-compiler-evaluate-form this)))
		  (message res)
		  res))
    (copy (let ((res (flex-compiler-evaluate-form this)))
	    (kill-new res)
	    (message res)
	    res))
    (eval-repl (flex-compiler-run-command this (slot-value this 'form)))
    (both-eval-repl (flex-compiler-repl--invoke-mode this 'eval-config)
		    (flex-compiler-repl--invoke-mode this 'eval-repl))
    (both-eval-minibuffer (flex-compiler-repl--invoke-mode this 'eval-config)
			  (flex-compiler-repl--invoke-mode this 'minibuffer))))

(cl-defmethod flex-compiler-repl-compile ((this evaluate-flex-compiler))
  "Invoke the compilation based on the `eval-mode' slot."
  (flex-compiler-repl--invoke-mode this (slot-value this 'eval-mode)))


;;; compiler manager/orchestration

(defclass flex-compile-manager (config-manager config-persistable)
  ()
  :documentation "Manages flexible compiler instances.")

(cl-defmethod initialize-instance ((this flex-compile-manager) &optional args)
  (with-slots (entries) this
    (setq entries (list (no-op-flex-compiler nil))))
  (cl-call-next-method this args))

(cl-defmethod config-persistable-load ((this flex-compile-manager))
  (with-slots (entries) this
    (let ((old-entries entries)
	  (old-emap (mapcar #'(lambda (entry)
				(cons (config-entry-name entry) entry))
			    entries))
	  new-entries)
      (cl-call-next-method this)
      (let ((new-emap (mapcar #'(lambda (entry)
				  (cons (config-entry-name entry) entry))
			      entries)))
	(setq entries
	      (remove nil
		      (mapcar #'(lambda (entry)
				  (let* ((name (config-entry-name entry))
					 (new-entry (cdr (assoc name new-emap))))
				    (if new-entry
					(if (assoc name old-emap)
					    new-entry)
				      entry)))
			      old-entries))))
      (dolist (compiler entries)
	;; since all compilers are persistable (via `config-entry inhertiance)
	;; set the manager so we can save the state
	(oset compiler :manager this)))))

(cl-defmethod config-manager-entry-default-name ((this flex-compile-manager))
  "flexible-compiler")

(cl-defmethod flex-compile-manager-register ((this flex-compile-manager)
					     compiler)
  "Register a compiler instance with the manager \(compilation framework)."
  (with-slots (entries) this
    (setq entries
	  (cl-delete compiler entries
		     :test #'(lambda (a b)
			       (equal (config-entry-name a)
				      (config-entry-name b)))))
    (setq entries (append entries (cons compiler nil)))
    (oset compiler :manager this)
    (message "Registered %s compiler" (oref compiler :name))))

(cl-defmethod config-manager-entry-names ((this flex-compile-manager))
  "Return the names of all registered compilers."
  (with-slots (entries) this
    (mapcar #'config-entry-name entries)))

(cl-defmethod flex-compile-manager-active ((this flex-compile-manager))
  "Return the currently selected or active manager."
  (car (slot-value this 'entries)))

(cl-defmethod config-manager-activate ((this flex-compile-manager) name)
  (let ((compiler (cl-call-next-method this name)))
    (message "Active compiler is now %s" (config-entry-name compiler))
    compiler))

(cl-defmethod flex-compile-manager-settable ((this flex-compile-manager)
					     &optional compiler)
  "Return whether the currently active/selected compiler is configurable.
In other words, if it extends `config-flex-compiler'."
  (let ((compiler (or compiler (flex-compile-manager-active this))))
    (child-of-class-p (eieio-object-class compiler) 'config-flex-compiler)))

(cl-defmethod flex-compile-manager-set-config ((this flex-compile-manager)
					       &optional file)
  "Set the configuration file of the currently active/selected compiler.

If it isn't settable, warn the user with a message and do nothing."
  (let ((active (flex-compile-manager-active this)))
    (if (flex-compile-manager-settable this)
	(flex-compiler-set-config active file)
      (message "Compiler `%s' not settable" (config-entry-name active)))))

(cl-defmethod flex-compile-manager-assert-ready ((this flex-compile-manager))
  "Make sure the active/selected compiler is ready and libraries loaded."
  (let ((active (flex-compile-manager-active this)))
    (flex-compiler-load-libraries active)))



;; functions
(defun flex-compiler-by-name (name)
  "Convenience function to get a compiler by it's NAME."
  (config-manager-entry the-flex-compile-manager name))

(defun flex-compiler-config-save ()
  "Save all compiler and manager configuration."
  (interactive)
  (config-persistable-save the-flex-compile-manager))

(defun flex-compiler-config-load ()
  "Load all compiler and manager configuration."
  (interactive)
  (config-persistable-load the-flex-compile-manager))



(defvar the-flex-compile-manager
  (flex-compile-manager)
  "The singleton manager instance.")

(defcustom flex-compile-persistency-file-name
  (expand-file-name "flex-compile" user-emacs-directory)
  "File containing the Flex compile configuration data."
  :type 'file
  :group 'flex-compile
  :set (lambda (sym val)
	 (set-default sym val)
	 (if (and (boundp 'the-flex-compile-manager)
		  the-flex-compile-manager)
	     (oset the-flex-compile-manager :file val))))



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
	     (names (config-manager-entry-names this)))
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
    (config-manager-activate this compiler-name)))

;;;###autoload
(defun flex-compile-compile (config-options)
  "Invoke compilation polymorphically.

CONFIG-OPTIONS, if non-nil invoke the configuration options for the compiler
before invoking the compilation.  By default CONFIG-OPTIONS is only detected
config-options by the \\[universal-argument] but some compilers use the numeric
argument as well.  This creates the need for an awkward key combination of:

  \\[digital-argument] \\[universal-argument] \\[flex-compile-compile]

to invoke this command with full configuration support."
  (interactive "P")
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (flex-compile-manager-assert-ready this)
    (when config-options
      (cond ((child-of-class-p (eieio-object-class active)
			       'optionable-flex-compiler)
	     (flex-compiler-read-set-options active config-options))
	    ((child-of-class-p (eieio-object-class active)
			       'evaluate-flex-compiler)
	     (flex-compiler-query-eval active config-options))))
    (let (compile-def)
      (let ((display-buffer-alist
	     (flex-compiler-display-buffer-alist active)))
	(setq compile-def (flex-compiler-compile active)))
      (flex-compiler-display-buffer active compile-def))))

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
	     (fcm active)
	     (if (not (flex-compile-manager-settable fcm))
		 (error "Not settable compiler: %s"
			(config-entry-name active)))))
    (let* ((this the-flex-compile-manager)
	   (active (flex-compile-manager-active this)))
      (flex-compile-manager-assert-ready this)
      (condition-case err
	  (cl-case action
	    (run (let (buf)
		   (let ((display-buffer-alist
			  (flex-compiler-display-buffer-alist active)))
		     (setq buf (flex-compiler-run active)))
		   (flex-compiler-display-buffer active buf)))
	    (find (assert-settable this active)
		  (pop-to-buffer (flex-compiler-config-buffer active)))
	    (set-config (let ((file (flex-compiler-read-config active)))
			  (flex-compile-manager-set-config this file)))
	    (t (error "Unknown action: %S" action)))
	(cl-no-applicable-method
	 (message "Unsupported action `%S' on compiler %s: no method %S"
		  action
		  (config-entry-name active)
		  (cl-second err)))))))

(defun flex-compile-read-form (no-input-p)
  "Read the compilation query form from the user.

If NO-INPUT-P is t, use the default witout getting it from the user.
1
This invokes the `flex-compiler-query-read-form method' on the
currently activated compiler."
  (let* ((mgr the-flex-compile-manager)
	 (this (flex-compile-manager-active mgr)))
    (flex-compile-manager-assert-ready mgr)
    (flex-compiler-query-read-form this no-input-p)))

;;;###autoload
(defun flex-compile-eval (&optional form)
  "Evaluate the current form for the \(usually REPL based compiler).
FORM is the form to evaluate \(if implemented).  If called with
\\[universal-argument] then prompt the user with the from to evaluation."
  (interactive (list (flex-compile-read-form (not current-prefix-arg))))
  (let* ((mgr the-flex-compile-manager)
	 (this (flex-compile-manager-active mgr)))
    (flex-compile-manager-assert-ready mgr)
    (let ((res (flex-compiler-evaluate-form this form)))
      (when (and res (called-interactively-p 'interactive))
	(kill-new res)
	(message "%s" res))
      res)))

;;;###autoload
(defun flex-compile-clean ()
  "Invoke the clean functionality of the compiler."
  (interactive)
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (condition-case nil
	(progn
	  (flex-compile-manager-assert-ready this)
	  (let (compile-def)
	   (let ((display-buffer-alist
		  (flex-compiler-display-buffer-alist active)))
	     (setq compile-def (flex-compiler-clean active)))
	   (flex-compiler-display-buffer active compile-def)))
      (cl-no-applicable-method
       (message "Compiler %s has no ability to clean"
		(config-entry-name active))))))

;;;###autoload
(defmacro flex-compile-declare-functions (&rest fns)
  "Declare functions in list FNS for the purposes of silencing the compiler.

This is used in the compiler module libraries to silence the compiler in
`eval-when-compile' scopes."
  `(eval-when-compile
     (mapcar #'(lambda (sym)
		 (unless (fboundp sym)
		   (eval `(defun ,sym (&rest x)
			    (error "Bad declare order")))))
	     (quote ,fns))))

;;;###autoload
(defmacro flex-compile-declare-variables (&rest vars)
  "Declare variables in list VARS for the purposes of silencing the compiler.

This is used in the compiler module libraries to silence the compiler in
`eval-when-compile' scopes."
  `(eval-when-compile
     (mapcar #'(lambda (sym)
		 (unless (fboundp sym)
		   (eval `(defvar ,sym nil))))
	     (quote ,vars))))

(provide 'flex-compile-manage)

;;; flex-compile-manage.el ends here
