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
(require 'eieio-base)
(require 'dash)
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

(defvar flex-compiler-query-eval-form-history nil
  "History variable for `flex-compiler-query-read-form'.")

(defvar flex-compiler-set-buffer-exists
  (delete-dups (list flex-compile-display-buffer-exists-mode 'never))
  "History variable for `flex-compiler-set-buffer-exists-mode'.")


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
	  (list method (object-print this))))

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
  (setq args (plist-put args :name "disable"))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler--unimplemented ((this no-op-flex-compiler) method)
  (message "Disabled compiler skipping method `%s'" method))



(defclass flex-conf-prop (eieio-named)
  ((name :initarg :name
	 :type symbol
	 :documentation "Name of the property")
   (compiler :initarg :compiler
	     :type flex-compiler
	     :documentation "The compiler that `owns' this property")
   (prompt :initarg :prompt
	   :type string
	   :documentation "Used to prompt the user for input.")
   (history :initarg :history
	    :initform (gensym "flex-conf-prop-history")
	    :type symbol
	    :documentation "The history variable for the input.")
   (required :initarg :required
	     :initform nil
	     :type boolean
	     :documentation "\
Whether or not the property is needed for compilation, run, or clean")
   (input-type :initarg :input-type
	       :initform 'last
	       :type symbol
	       :documentation "One of last toggle.")
   (order :initarg :order
	  :initform 100
	  :type integer
	  :documentation "The order of importance of setting the property."))
  :documentation "A property of the `conf-flex-compiler' meant to be saved.")

(cl-defmethod initialize-instance ((this flex-conf-prop) &optional args)
  (dolist (elt (list :compiler :name :prompt))
    (unless (plist-get args elt)
      (error "Missing initarg: %S in %s" elt this)))
  (setq args (plist-put args :object-name (plist-get args :name)))
  (cl-call-next-method this args))

(cl-defmethod object-print ((this flex-conf-prop) &rest strings)
  (with-slots (name order) this
    (apply #'cl-call-next-method this
	   (format ": %s (%d)" name order)
	   strings)))

(cl-defmethod flex-compiler-conf-default-input ((this flex-conf-prop))
  "Return the default string value for the default when prompting user input."
  (with-slots (history input-type) this
    (if (boundp history)
	(let ((val (symbol-value history)))
	  (case input-type
	    (toggle (or (second val) (first val)))
	    (last (first val)))))))

(cl-defmethod flex-compiler-conf-prompt ((this flex-conf-prop))
  "Return the prompt to use for user input."
  (with-slots (prompt) this
    (let ((default (flex-compiler-conf-default-input this)))
      (format "%s%s: " prompt (if default (format " (%s)" default) "")))))

(cl-defmethod flex-compiler-conf-read ((this flex-conf-prop))
  "Read the user input for the property.
The default reads a string using `flex-compiler-conf-default' and
`flex-compiler-conf-prompt' with the history slot."
  (with-slots (history) this
    (let* ((default (flex-compiler-conf-default-input this))
	   (prompt (flex-compiler-conf-prompt this)))
      (read-string prompt nil history default))))

(cl-defmethod flex-compiler-conf-validate ((this flex-conf-prop) val)
  "Raise an error if user input VAL is not not valid data."
  nil)

(cl-defmethod flex-compile-clear ((this flex-conf-prop))
  "Clear any state \(i.e. history) from the property."
  (with-slots (history) this
    (setq (symbol-value history) nil)))


(defclass flex-conf-choice-prop (flex-conf-prop)
  ((choices :initarg :choices
	    :type list
	    :documentation "A list of symbols or strings for the choices \
to prompt the user"))
  :documentation "Property that prompts for a selection of a list of choices.")

(cl-defmethod flex-compiler-conf-read ((this flex-conf-choice-prop))
  (with-slots (history choices) this
    (if (= 1 (length choices))
	(car choices)
      (let* ((default (flex-compiler-conf-default-input this))
	     (prompt (flex-compiler-conf-prompt this)))
	(choice-program-complete prompt choices nil t nil history default)))))


(defclass flex-conf-choice-description-prop (flex-conf-prop)
  ((methods :initarg :methods
	   :type list
	   :documentation "A list of symbols or strings for the choices \
to prompt the user"))
  :documentation "Property that prompts for a selection of a list of choices.")

(cl-defmethod flex-compiler-conf-read ((this flex-conf-choice-description-prop))
  (with-slots (history methods) this
    (if (= 1 (length methods))
	(cadr methods)
      (let* ((default (flex-compiler-conf-default-input this))
	     (prompt (flex-compiler-conf-prompt this))
	     (choices (mapcar 'first methods))
	     (method (choice-program-complete
		      prompt choices t t nil history default)))
	(cdr (assoc method methods))))))


(defclass flex-conf-file-prop (flex-conf-prop)
  ((validate-modes :initarg :validate-modes
		   :initform nil
		   :type list
		   :documentation "\
The major mode to use to validate/select `config-file` buffers.")))

(cl-defmethod initialize-instance ((this flex-conf-file-prop) &optional args)
  (dolist (elt (list :validate-modes))
    (unless (plist-get args elt)
      (error "Missing initarg: %S" elt)))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-conf-default-input ((this flex-conf-file-prop))
  nil)

(cl-defmethod flex-compiler-conf-read ((this flex-conf-file-prop))
  (with-slots (history description) this
    (let* ((prompt (flex-compiler-conf-prompt this))
	   (fname (buffer-file-name))
	   (initial (and fname (file-name-nondirectory fname))))
      (read-file-name prompt nil nil t initial))))

(cl-defmethod flex-compiler-conf-validate ((this flex-conf-file-prop) val)
  (with-slots (validate-modes compiler) this
    (let ((description (slot-value compiler 'description)))
      (save-excursion
	(set-buffer (find-file-noselect val))
	(if (and validate-modes (not (memq major-mode validate-modes)))
	    (error (format "This doesn't look like a %s file, got mode: %S"
			   description major-mode)))))))


(defclass flex-conf-directory-prop (flex-conf-prop)
  ()
  :documentation "Directory property")

(cl-defmethod flex-compiler-conf-read ((this flex-conf-directory-prop))
  "Read a directory from the user and set it to slot `start-directory'."
  (with-slots (history choices) this
    (let ((default (flex-compiler-conf-default-input this))
	  (prompt (flex-compiler-conf-prompt this)))
      (read-directory-name prompt default nil t))))


(defclass flex-conf-eval-prop (flex-conf-prop)
  ((func :initarg :func
	 :initform '(lambda (&rest) "Unimplemented")
	 :type function
	 :documentation "The function to invoke when reading the config.")))

(cl-defmethod flex-compiler-conf-read ((this flex-conf-eval-prop))
  (with-slots (func history compiler) this
    (let ((default (flex-compiler-conf-default-input this))
	  (prompt (flex-compiler-conf-prompt this)))
      (funcall func this compiler default prompt history))))



(defclass conf-flex-compiler (flex-compiler)
  ((props :initarg :props
	  :initform nil
	  :documentation "The list of metadata configurations")
   (last-selection :initarg :last-selection))
  :abstract true
  :documentation "A property based configurable compiler.")

(cl-defmethod initialize-instance ((this conf-flex-compiler) &optional args)
  (let* ((props (plist-get args :props))
	 (choices (mapcar #'(lambda (prop)
			      (slot-value prop 'name))
			  props)))
    (setq args (plist-put args :last-selection
			  (flex-conf-choice-prop :name 'last-selection
						 :prompt "Property"
						 :compiler this
						 :choices choices
						 :input-type 'last))
	  args (plist-put args :pslots
			  (append (plist-get args :pslots) choices))
	  args (plist-put args :props props)))
  (cl-call-next-method this args))

(cl-defmethod object-print ((this conf-flex-compiler) &rest strings)
  (apply #'cl-call-next-method this
	 (concat ", props: ("
	 	 (mapconcat #'object-print (slot-value this 'props) ", ")
	 	 ")")
	 strings))

(cl-defmethod flex-compiler-conf-set-prop ((this conf-flex-compiler) prop val)
  "Set a property with name \(symbol) PROP to VAL."
  (flex-compiler-conf-validate prop val)
  (setf (slot-value this (slot-value prop 'name)) val)
  (flex-compiler-save-config this)
  (message "Set %S to %s" (slot-value prop 'name)
	   (if (stringp val)
	       val
	     (prin1-to-string val))))

(cl-defmethod flex-compiler-conf-prop-by-order ((this conf-flex-compiler))
  "Get all properties sorted by their order values."
  (with-slots (props) this
    (setq props (sort props #'(lambda (a b)
				(< (slot-value a 'order)
				   (slot-value b 'order)))))
    props))

(cl-defmethod flex-compiler-conf-prop-by-name ((this conf-flex-compiler) name)
  "Get a property by \(symbol) NAME."
  (with-slots (props) this
    (let ((prop-map (mapcar #'(lambda (prop)
				`(,(slot-value prop 'name) . ,prop))
			    props)))
      (cdr (assq name prop-map)))))

(cl-defmethod flex-compiler-configure ((this conf-flex-compiler)
				       config-options)
  "Configure the compiler.

CONFIG-OPTIONS is the numeric argument (if any) passed in the iteractive mode
with \\[universal-argument]."
  (let (prop val)
    (cond ((null config-options)
	   (with-slots (props last-selection) this
	     (let ((prop-name (flex-compiler-conf-read last-selection)))
	       (setq prop (flex-compiler-conf-prop-by-name this prop-name)))))
	  ((consp config-options)
	   (case (car config-options)
	     (prop-name
	      (setq prop (flex-compiler-conf-prop-by-name
			  this (second config-options))))
	     (t (error "Unknown type: %S" (car config-options))))
	   (if (> (length config-options) 2)
	       (setq val (nth 2 config-options))))
	  (t (let ((props (flex-compiler-conf-prop-by-order this)))
	       (setq prop (nth (min config-options (length props)) props)))))
    (setq val (or val (flex-compiler-conf-read prop)))
    (flex-compiler-conf-set-prop this prop val)))

(cl-defmethod flex-compiler-set-required ((this conf-flex-compiler))
  "Set all required properties for the compiler."
  (dolist (prop (flex-compiler-conf-prop-by-order this))
    (let* ((name (slot-value prop 'name))
	   (val (slot-value this name))
	   ;; protect completing-read
	   (display-buffer-alist nil))
      (when (and (null val) (slot-value prop 'required))
	(setq val (flex-compiler-conf-read prop))
	(flex-compiler-conf-set-prop this prop val)))))

(cl-defmethod flex-compile-clear ((this conf-flex-compiler))
  "Wipe all values for the compiler."
  (dolist (prop (flex-compiler-conf-prop-by-order this))
    (flex-compiler-conf-set-prop this prop nil))
  (dolist (prop (flex-compiler-conf-prop-by-order this))
    (flex-compile-clear prop))
  (message "Cleared %s configuration" (slot-value this 'name)))

(cl-defmethod flex-compiler-show-configuration ((this conf-flex-compiler))
  "Create a buffer with the configuration of the compiler."
  (with-slots (description) this
    (save-excursion
      (-> (format "*%s Compiler Configuration*" description)
	  get-buffer-create
	  set-buffer)
      (read-only-mode 0)
      (erase-buffer)
      (insert (format "%s configuration:\n" description))
      (dolist (prop (flex-compiler-conf-prop-by-order this))
	(let* ((name (slot-value prop 'name))
	       (val (or (slot-value this name) "<not set>")))
	  (insert (format "%S: %s\n" name val))))
      (read-only-mode 1)
      (display-buffer (current-buffer)))))



(defclass conf-file-flex-compiler (conf-flex-compiler)
  ((config-file :initarg :config-file
		:initform nil
		:type (or null string)
		:documentation "\
The file to use for `configuring' the compiler.")
   (start-directory :initarg :start-directory
		    :initform nil
		    :type (or null string)
		    :documentation "\
The directory for starting the compilation.  The `default-directory' is set to
this when the compile starts"))
  :abstract true
  :documentation "A configurable compiler with a configuration file.")

(cl-defmethod initialize-instance ((this conf-file-flex-compiler)
				   &optional args)
  (let* ((modes (plist-get args :validate-modes))
	 (desc (plist-get args :description))
	 (name (plist-get args :name))
	 (prompt (format "%s file" (or desc (capitalize name))))
	 (props (list (flex-conf-directory-prop :name 'start-directory
						:compiler this
						:prompt "Start directory"
						:input-type 'last)
		      (flex-conf-file-prop :name 'config-file
					   :prompt prompt
					   :compiler this
					   :validate-modes modes
					   :input-type 'last
					   :required t
					   :order 0))))
    (setq args (plist-put args :props
			  (append (plist-get args :props) props))))
  (cl-remf args :validate-modes)
  (cl-call-next-method this args))

(cl-defmethod object-print ((this conf-file-flex-compiler) &rest strings)
  (apply #'cl-call-next-method this
	 (format " config-file: %s" (slot-value this 'config-file))
	 strings))

(cl-defmethod flex-compiler-configure ((this conf-file-flex-compiler)
				       config-options)
  (if (eq config-options 'immediate)
      (setq config-options (list 'prop-name 'config-file (buffer-file-name))))
  (cl-call-next-method this config-options))

(cl-defmethod flex-compiler-conf-set-prop ((this conf-file-flex-compiler)
					   prop val)
  (if (eq (slot-value prop 'name) 'config-file)
      (with-slots (start-directory) this
	(flex-compiler-conf-validate prop val)
	(setq start-directory (file-name-directory val))))
  (cl-call-next-method this prop val))

(cl-defmethod flex-compiler-conf-file-buffer ((this conf-file-flex-compiler))
  "Return a (new) buffer of the configuration file."
  (flex-compiler-set-required this)
  (find-file-noselect (slot-value this 'config-file)))

(cl-defmethod flex-compiler-conf-file-display ((this conf-file-flex-compiler))
  "Pop the configuration file buffer to the current buffer/window."
  (pop-to-buffer (flex-compiler-conf-file-buffer this)))



(defclass single-buffer-flex-compiler (flex-compiler)
  ((buffer-name :initarg :buffer-name
		:type string
		:documentation "The default name of the single buffer."))
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



(defclass repl-flex-compiler
  (conf-file-flex-compiler single-buffer-flex-compiler)
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
If non-`nil' then don't prompt to kill a REPL buffer on clean.")
   (output-clear :initarg :output-clear
		 :initform 'no
		 :type symbol
		 :documentation "\
Whether or not to clear comint buffer after a compilation."))
  :documentation "Compiles by evaluating expressions in the REPL.")

(cl-defmethod initialize-instance ((this repl-flex-compiler) &optional args)
  (let* ((fn '(lambda (this default prmopt history)
		(split-string (read-string prompt nil history default))))
	 (props (list (flex-conf-choice-prop :name 'output-clear
					     :compiler this
					     :prompt "Clear output on compile"
					     :choices '(yes no)
					     :input-type 'toggle))))
    (setq args (plist-put args :props (append (plist-get args :props) props))))
  (cl-call-next-method this args))

(cl-defmethod flex-compiler-repl-start ((this repl-flex-compiler))
  "Start the REPL."
  (flex-compiler--unimplemented this "start-repl"))

(cl-defmethod flex-compiler-repl-compile ((this repl-flex-compiler) file)
  "Invoked by `compile' type messages from the compiler manager.

FILE gets evaluated by the compiler either as a IPC communication or by direct
insertion in the REPL buffer.

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
	(flex-compiler-set-required this)
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
  (flex-compiler-set-required this)
  (with-slots (config-file output-clear) this
    (let ((runningp (flex-compiler-repl-running-p this)))
      (case start-type
	(compile (progn
		   (unless runningp
		     (flex-compiler-run this))
		   (if (flex-compiler-repl-running-p this)
		       (progn
			 (when (eq output-clear 'yes)
			   (with-current-buffer (flex-compiler-buffer this)
			     (comint-clear-buffer)))
			 (flex-compiler-repl-compile this config-file))
		     (if runningp
			 (error "REPL hasn't started")
		       (message "REPL still starting, please wait")))
		   (flex-compiler-buffer this)))
	(run (progn
	       (flex-compiler-repl--run-start this)
	       (flex-compiler-buffer this)))
	(clean (progn
		 (flex-compiler-kill-repl this)
		 'killed-buffer))))))

(cl-defmethod flex-compiler-eval-initial-at-point ((this repl-flex-compiler))
  nil)

(cl-defmethod flex-compiler-query-read-form ((this repl-flex-compiler)
					     no-input-p)
  "Read a form, meaningful for the compiler, from the user."
  (let ((init (flex-compiler-eval-initial-at-point this)))
    (if no-input-p
	init
      (read-string "Form: " init 'flex-compiler-query-eval-form-history))))

(cl-defmethod flex-compiler-evaluate-form ((this repl-flex-compiler)
					   &optional form)
  "Return the evaluation form.

See the `:eval-form' slot."
  (if (and (null form) (not (slot-boundp this :form)))
      (flex-compiler-query-eval this nil))
  (let ((res (flex-compiler-eval-form-impl
	      this (or form (slot-value this 'form)))))
    (if (stringp res)
	res
      (prin1-to-string res))))


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
	(setf (slot-value compiler 'manager) this)))))

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
    (setf (slot-value compiler 'manager) this)
    (message "Registered %s compiler" (slot-value compiler 'name))))

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

(cl-defmethod flex-compile-manager-assert-ready ((this flex-compile-manager))
  "Make sure the active/selected compiler is ready and libraries loaded."
  (let ((active (flex-compile-manager-active this)))
    (flex-compiler-load-libraries active)))

(cl-defmethod flex-compile-clear ((this flex-compile-manager))
  "Clear all compiler's state.
This is done by simply re-instantiating all current registered compilers."
  (let ((entries (slot-value this 'entries)))
    (setf (slot-value this 'entries) nil)
    (dolist (compiler entries)
      (-> (eieio-object-class compiler)
	  funcall
	  (flex-compile-manager-register this)))))



;; library configuration
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
	     (setf (slot-value the-flex-compile-manager 'file) val))))


;; functions
(defun flex-compiler-by-name (name)
  "Convenience function to get a compiler by it's NAME."
  (config-manager-entry the-flex-compile-manager name))

;;;###autoload
(defun flex-compiler-config-save ()
  "Save all compiler and manager configuration."
  (interactive)
  (config-persistable-save the-flex-compile-manager))

;;;###autoload
(defun flex-compiler-config-load ()
  "Load all compiler and manager configuration."
  (interactive)
  (config-persistable-load the-flex-compile-manager))

;;;###autoload
(defun flex-compiler-list ()
  "Display the flex compiler list."
  (interactive)
  (config-manager-list-entries-buffer the-flex-compile-manager))

(defun flex-compiler-reset-configuration ()
  "Reset every compiler's configuration."
  (interactive)
  (when (y-or-n-p "This will wipe all compiler configuration.  Are you sure? ")
    (if (file-exists-p flex-compile-persistency-file-name)
	(delete-file flex-compile-persistency-file-name))
    (flex-compile-clear the-flex-compile-manager)
    (flex-compiler-config-save)
    (message "Configuration reset")))



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
(defun flex-compiler-set-buffer-exists-mode ()
  "Query and set the value for the display mode for existing buffers.
This sets but doesn't configure
`flex-compile-display-buffer-exists-mode'."
  (interactive)
  (let ((choices (->> (cdr flex-compile-display-mode-options)
		      (-map 'last)
		      (-map 'first)))
	(def (or (second flex-compiler-set-buffer-exists)
		 (car flex-compiler-set-buffer-exists))))
    (setq flex-compile-display-buffer-exists-mode
	  (choice-program-complete "Buffer Exists Mode"
				   choices
				   nil t ; return string, require match
				   nil	 ; initial
				   'flex-compiler-set-buffer-exists ; history
				   def				    ; def
				   nil	; allow-empty
				   t t))))

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
  (interactive
   (list (cond ((null current-prefix-arg) 'compile)
	       ;; universal arg
	       ((equal '(4) current-prefix-arg) nil)
	       (t (1- current-prefix-arg)))))
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (flex-compile-manager-assert-ready this)
    (if (eq config-options 'compile)
	(let (comp-def)
	  (let ((display-buffer-alist
		 (flex-compiler-display-buffer-alist active)))
	    (setq comp-def (flex-compiler-compile active)))
	  (flex-compiler-display-buffer active comp-def))
      (flex-compiler-configure active config-options))))

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
	  (find (if (child-of-class-p (eieio-object-class active)
				      'conf-file-flex-compiler)
		    (flex-compiler-conf-file-display active)))
	  (set-config (flex-compiler-configure active 'immediate))
	  (t (error "Unknown action: %S" action)))
      (cl-no-applicable-method
       (message "Unsupported action `%S' on compiler %s: no method %S"
		action
		(config-entry-name active)
		(cl-second err))))))

(defun flex-compile-read-form (no-input-p)
  "Read the compilation query form from the user.

If NO-INPUT-P is t, use the default witout getting it from the user.

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
(defun flex-compile-show-configuration ()
  "Create a buffer with the configuration of the current compiler."
  (interactive)
  (let* ((this the-flex-compile-manager)
	 (active (flex-compile-manager-active this)))
    (flex-compiler-show-configuration active)))

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
