;;; flex-compile-cli.el --- Compile functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 - 2021 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: command line compile flexible processes
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

;; Implementation compiler for Zensols action based command line.
;; See: https://plandes.github.io/util/doc/command-line.html

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'dash)
(require 'choice-program-complete)
(require 'flex-compile-manage)

(defclass flex-compile-cli-arg (config-prop-entry)
  ((value :initarg :value
	  :initform nil
	  :documentation "The value of the argument from user input.")
   (arg-name :initarg :arg-name
	     :type string
	     :documentation "The long argument name.")
   (type :initarg :type
	 :type symbol
	 :documentation "Either `option' or `positional'."))
  :documentation "\
Contains user provided arguments that is given to the command line.")

(cl-defmethod config-prop-save-config ((this flex-compile-cli-arg))
  "Does nothing as all data is in-memory ephemeral for THIS compiler."
  (ignore this))

;;; cli file compiler
(defclass cli-flex-compiler (single-buffer-flex-compiler
			     conf-file-flex-compiler)
  ((cli-metadata :initarg :cli-metadata
		 :initform nil
		 :documentation "The CLI action metadata.")
   (action :initarg :action
	   :initform nil
	   :documentation "The action to invoke on the program.")
   (arg-properties :initarg :arg-properties
		   :initform nil
		   :documentation
		   "Properties used to read command line arguments.")
   (arguments :initarg :arguments
	      :initform nil
	      :documentation "The arguments to give to the script.")
   (cache-metadata :initarg :cache-metadata
		   :initform t
		   :documentation "
Whether or not to cache the Python program's CLI metadata."))
  :method-invocation-order :c3
  :documentation "\
Provides support for user input for action mnemonics and options using Python
programs that use the
\[Zensols action CLI]\(https://plandes.github.io/util/doc/command-line.html).

This compiler gets the command line metadata as a list of actions and their
respective positional and option arguments.  It this prompts the user with
documentation associated, first the action, then the action's arguments.
Finally, it constructs the command line and executes the Python program with
the arguments.")

(cl-defmethod initialize-instance ((this cli-flex-compiler)
				   &optional slots)
  "Initialize the THIS instance with SLOTS."
  (let* ((read-action (lambda (this compiler default prompt history)
			(ignore this)
			(flex-compiler-cli-read-action
			 compiler default prompt history)))
	 (read-args (lambda (this compiler &rest args)
		      (ignore this args)
		      (flex-compiler-cli-read-arguments compiler t)))
	 (props (list (config-eval-prop :object-name 'action
					:prompt "Action"
					:func read-action
					:prop-entry this
					:required t
					:input-type 'last
					:order 0)
		      (config-eval-prop :object-name 'arguments
					:prompt "Arguments"
					:func read-args
					:prop-entry this
					:required nil
					:input-type 'last
					:order 1)
		      (config-boolean-prop :object-name 'cache-metadata
					   :prompt "Cache metadata"
					   :prop-entry this
					   :initial-value t
					   :required nil
					   :input-type 'toggle
					   :order 2))))
    (setq slots (plist-put slots :object-name "cli")
	  slots (plist-put slots :description "CLI Python")
	  slots (plist-put slots :buffer-name "Command Line Interface")
	  slots (plist-put slots :kill-buffer-clean t)
	  slots (plist-put slots :validate-modes '(python-mode))
	  slots (plist-put slots
			   :props (append (plist-get slots :props) props))))
  (cl-call-next-method this slots))

(cl-defmethod flex-compiler-load-libraries
  ((this cli-flex-compiler))
  "Load the `json' library for THIS compiler."
  (ignore this)
  (require 'json))

(cl-defmethod config-prop-set ((this cli-flex-compiler) prop val)
  "Set property PROP to VAL on THIS compiler."
  (message "CLI PROP SET %S %S" prop val)
  (let (wipes)
    (cond ((eq (config-prop-name prop) 'config-file)
	   (config-persistent-reset (config-prop-by-name this 'action))
	   (setq wipes '(cli-metadata arg-properties arguments action)))
	  ((eq (config-prop-name prop) 'action)
	   (setq wipes
		 (append wipes '(cli-metadata arg-properties arguments)))))
   (dolist (slot wipes)
     (setf (slot-value this slot) nil)))
  (cl-call-next-method this prop val))

(cl-defmethod flex-compiler-cli-metadata ((this cli-flex-compiler)
					  &optional action)
  "Read the CLI metadata from the command line on THIS compiler.
If ACTION is non-nil, then return only the metadata for the \(symbol) action."
  ;; make sure the action is set first
  (config-prop-entry-set-required this 'config-file)
  (with-slots (config-file cli-metadata cache-metadata) this
    (when (null config-file)
      (error "Must first set the configuration file"))
    (setq cli-metadata
	  (or (and cache-metadata cli-metadata)
	      (let* ((json (->> (format "%s list --lstfmt json" config-file)
				   shell-command-to-string)))
		(condition-case err
		    (json-read-from-string json)
		  (error "Could not parse <%s>: %s" json err)))))
    (if action
	(cdr (assq action cli-metadata))
      cli-metadata)))

(cl-defmethod flex-compiler-cli-has-positional-p ((this cli-flex-compiler)
						  action)
  "Return if THIS compiler's ACTION program has positional arguments."
  (->> (flex-compiler-cli-metadata this)
       (assq action)
       cdr
       (assq 'positional)
       cdr
       length
       (< 0)))

(cl-defmethod flex-compiler-cli-read-action ((this cli-flex-compiler)
					     default prompt history)
  "Read the action for THIS compiler using DEFAULT, PROMPT and HISTORY."
  (let ((action
	 (->> (flex-compiler-cli-metadata this)
	      (-map (lambda (action)
		      (let ((name (cdr (assq 'name action)))
			    (doc (cdr (assq 'doc action))))
			(format "%s: %s" name doc))))
	      (funcall (lambda (actions)
			 (choice-program-complete
			  prompt actions t nil nil history default t))))))
    (unless (string-match "^\\([^:]+\\)" action)
      (error "Could not parse action from desc: %s" action))
    (let ((action (intern (match-string 1 action)))
	  (arg-prop (config-prop-by-name this 'arguments)))
      (oset arg-prop :required
	    (flex-compiler-cli-has-positional-p this action))
      action)))

(cl-defmethod flex-compiler-cli--arg-properties ((this cli-flex-compiler))
  "Create the argument properties for THIS compiler."
  ;; make sure the action is set first
  (config-prop-entry-set-required this 'action)
  (cl-flet ((to-config-class
	     (elt)
	     (let ((regex "\\(?:directory\\|folder\\)")
		   (type (intern (cdr (assq 'dtype elt)))))
	       (cl-case type
		 (bool 'config-boolean-prop)
		 (int 'config-number-prop)
		 (float 'config-number-prop)
		 (str 'config-prop)
		 (path (let* ((doc (cdr (assq 'doc elt)))
			      (is-dir (string-match regex doc)))
			 (if is-dir
			     'config-directory-prop
			   'config-file-prop)))
		 (t (error "Unknown type: %s" type)))))
	    (to-arg
	     (key elt)
	     (cdr (assq key elt)))
	    (to-doc
	     (elt)
	     (let* ((doc (cdr (assq 'doc elt)))
		    (first-char (substring doc nil 1))
		    (rest-str (substring doc 1)))
	       (concat (capitalize first-char) rest-str))))
    (with-slots (action) this
      ;; return a config-prop for each command line argument/option
      (let* ((meta (flex-compiler-cli-metadata this action))
	     ;; collect required positional arguments for the action
	     (positional (->> (cdr (assq 'positional meta))
			      (-map (lambda (elt)
				      `((prop . (,(to-config-class elt)
						 :required t
						 :object-name 'value
						 :prompt ,(to-doc elt)))
					(arg-name . ,(to-arg 'name elt))
					(type . position))))))
	     ;; collect optional arguments for the action
	     (options (->> (cdr (assq 'options meta))
			   (-map (lambda (elt)
				   `((prop . (,(to-config-class elt)
					      :required t
					      :initial-value ,(assq 'defautl elt)
					      :object-name 'value
					      :prompt ,(to-doc elt)))
				     (arg-name . ,(to-arg 'long_name elt))
				     (type . option))))))
	     (order 0))
	;; instantiate a new config-prop by type for each
	(->> (append positional options)
	     (-map (lambda (elt)
		     (let ((prop (cdr (assq 'prop elt)))
			   (arg-name (cdr (assq 'arg-name elt))))
		       (append
			prop
			`(:prop-entry
			  ,(flex-compile-cli-arg :arg-name arg-name
						 :type (cdr (assq 'type elt)))
			  :order ,(cl-incf order)
			  :input-type 'last)))))
	     (-map #'eval)
	     (-map (lambda (prop)
		     (let ((container (slot-value prop 'prop-entry)))
		       (oset container :props (list prop))
		       prop))))))))

(cl-defmethod flex-compiler-cli-argument-plist ((this cli-flex-compiler)
						&optional resetp)
  "Read the arguments and return them as a list of strings for THIS compiler.
If RESETP is non-nil, reset all previously set configuration to force the user
to add again."
  (with-slots (arg-properties) this
    (setq arg-properties
	  (or arg-properties (flex-compiler-cli--arg-properties this)))
    (when resetp
      (dolist (prop arg-properties)
	(oset (slot-value prop 'prop-entry) :value nil)))
    ;; create a string command line parameter for each argument (except false
    ;; booleans as they are set as flags/store true)
    (->> arg-properties
	 (-map (lambda (prop)
		 (let ((container (slot-value prop 'prop-entry))
		       (value-type (config-prop-type prop)))
		   ;; get the user input now
		   (config-prop-entry-set-required container)
		   (with-slots (arg-name type value) container
		     (list :value-type value-type
			   :arg-name arg-name
			   :arg-type type
			   :value value
			   :str-value
			   (cond ((eq value-type 'boolean)
				  (if value "true" "false"))
				 ((or (null value) (stringp value)) value)
				 (t (prin1-to-string value)))))))))))

(cl-defmethod flex-compiler-cli-read-arguments ((this cli-flex-compiler)
						&optional resetp)
  "Read the arguments and return them as a list of strings for THIS compiler.
If RESETP is non-nil, reset all previously set configuration to force the user
to add again."
  ;; create a string command line parameter for each argument (except false
  ;; booleans as they are set as flags/store true)
  (->> (flex-compiler-cli-argument-plist this resetp)
       (-map (lambda (pl)
	       (let ((arg-name (plist-get pl :arg-name))
		     (value (plist-get pl :value))
		     (value-type (plist-get pl :value-type))
		     (arg-type (plist-get pl :arg-type)))
		 (setq value (if (or (null value) (stringp value))
				 value
			       (prin1-to-string value)))
		 ;; positional arguments have no (option) long name
		 (if (eq arg-type 'position)
		     (cons value nil)
		   (if (eq value-type 'boolean)
		       ;; add just the optiona name as flags for booleans
		       (if value
			   (list (format "--%s" arg-name)))
		     ;; add the option name and value
		     (list (format "--%s" arg-name) value))))))
       ;; aggregate the list of argument lists in to a single list
       (apply #'append)))

(cl-defmethod config-prop-entry-write-configuration ((this cli-flex-compiler)
						     &optional level header)
  "Add the command line argument metadata and values to the output for THIS.
LEVEL is the indentation level.
HEADER is a string written to describe the property, otherise the description
is used."
  (cl-call-next-method this level header)
  (setq level (or level 0))
  (with-slots (props) this
    (let ((space (make-string (* 2 level) ? ))
	  (space2 (make-string (* 2 (1+ level)) ? )))
      (insert (format "%sarguments:\n" space))
      (->> (flex-compiler-cli-argument-plist this)
	   (-map (lambda (plist)
		   (apply
		    #'format "%s%s: \"%s\" (%s, %S)" space2
		    (-map (lambda (k)
			    (plist-get plist k))
			  '(:arg-name :str-value :value-type :arg-type)))))
	   (funcall (lambda (args)
		      (mapconcat #'identity args "\n")))
	   insert))
    (newline)))

(cl-defmethod flex-compiler-start-buffer ((this cli-flex-compiler)
					  start-type)
  "Return a new buffer for THIS compiler with a processing compilation.
See the `single-buffer-flex-compiler' implementation of
`flex-compiler-start-buffer' for more information and START-TYPE."
  (cl-case start-type
    (compile
     (with-slots (config-file start-directory action arguments) this
       (let ((default-directory start-directory)
	     (buffer-name (flex-compiler-buffer-name this))
	     (cmd (concat
		   ;; the python entry point script file name
		   config-file " "
		   ;; action mnemonic
		   (symbol-name action) " "
		   ;; any arguments separated with a space
		   (mapconcat #'identity arguments " ")))
	     buf)
	 (with-current-buffer
	     (setq buf
		   (compilation-start cmd nil (lambda (_) buffer-name))))
	 buf)))
    (run (config-prop-entry-show-configuration this))))

;; register the compiler
(flex-compile-manager-register flex-compile-manage-inst
			       (cli-flex-compiler))

(provide 'flex-compile-cli)

;;; flex-compile-cli.el ends here
