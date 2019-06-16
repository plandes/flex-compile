;;; flex-compile-config.el --- configuration based compiler for flex-compile

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

;; This file contains an abstract compiler that provides configuration
;; functionality (minibuffer and persistance to the configuraiton file) and
;; basic meta data properties for configuration.

;;; Code:

(require 'dash)
(require 'eieio)
(require 'choice-program-complete)
(require 'flex-compile-base)

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
  :documentation "A property of the `conf-flex-compiler' meant to be saved.
This is a container class that provides the meta data for the compiler.")

(cl-defmethod initialize-instance ((this flex-conf-prop) &optional args)
  (dolist (elt (list :compiler :name :prompt))
    (unless (plist-get args elt)
      (error "Missing initarg: %S in %s" elt this)))
  (setq args (plist-put args :object-name (plist-get args :name)))
  (cl-call-next-method this args)
  (set (slot-value this 'history) nil))

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
	  (cl-case input-type
	    (toggle (or (cl-second val) (cl-first val)))
	    (last (cl-first val)))))))

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



;; properties
(defclass flex-conf-choice-prop (flex-conf-prop)
  ((choices :initarg :choices
	    :initform nil
	    :type (or null list)
	    :documentation "\
A list of symbols or strings for the choices to prompt the user.
Either this is non-nil or :choices-fn is, but not both.")
   (choices-fn :initarg :choices-fn
	       :initform nil
	       :type (or null function)
	       :documentation "\
A function used to generate choices in places of :choices.
The function takes this class instance as the single parameter.
Either this is non-nil or :choices is, but not both.")
   (ignore-case :initarg :ignore-case
		:initform t
		:type boolean
		:documentation "\
This is always used for `completion-ignore-case'."))
  :documentation "Property that prompts for a selection of a list of choices.")

(cl-defmethod flex-compiler-choices ((this flex-conf-choice-prop))
  (with-slots (choices choices-fn) this
    (or choices (funcall choices-fn this))))

(cl-defmethod flex-compiler-conf-read ((this flex-conf-choice-prop))
  (with-slots (history ignore-case) this
    (let ((choices (flex-compiler-choices this)))
     (if (= 1 (length choices))
	 (car choices)
       (let ((default (flex-compiler-conf-default-input this))
	     (prompt (flex-compiler-conf-prompt this))
	     (completion-ignore-case ignore-case))
	 (choice-program-complete prompt choices nil t nil history default))))))


(defclass flex-conf-choice-description-prop (flex-conf-choice-prop)
  ()
  :documentation "Property that prompts for a selection of a list of choices.")

(cl-defmethod flex-compiler-conf-read ((this flex-conf-choice-description-prop))
  (with-slots (history ignore-case) this
    (let ((choices (flex-compiler-choices this)))
     (if (= 1 (length choices))
	 (cadr choices)
       (let ((default (flex-compiler-conf-default-input this))
	     (prompt (flex-compiler-conf-prompt this))
	     (completion-ignore-case ignore-case))
	 (choice-program-complete prompt choices t t nil history default))))))


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
      (let* ((file-name-history (symbol-value history)))
	(prog1
	    (read-file-name prompt nil nil t initial)
	  (set history file-name-history))))))

(cl-defmethod flex-compiler-conf-validate ((this flex-conf-file-prop) val)
  (with-slots (validate-modes compiler) this
    (let ((description (slot-value compiler 'description)))
      (with-current-buffer (find-file-noselect val)
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
  :documentation "A property based configurable compiler.
All properties are added in each sub class's `initialize-instance' method as
the :props plist argument in ARGS.

Important: Extend from this class _last_ so that it captures all proprties
since this class sets :pslots in the `config-persistent' subclass.")

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
	 	 (mapconcat #'(lambda (obj)
				(with-temp-buffer
				  (cl-print-object obj (current-buffer))))
			    (slot-value this 'props)
			    ", ")
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
	   (cl-case (car config-options)
	     (prop-name
	      (setq prop (flex-compiler-conf-prop-by-name
			  this (cl-second config-options))))
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
	   ;; minibuffer reading has odd behavior when this isn't nil
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
    (with-current-buffer (-> (format "*%s Compiler Configuration*" description)
			     get-buffer-create)
      (read-only-mode 0)
      (erase-buffer)
      (insert (format "%s configuration:\n" description))
      (dolist (prop (flex-compiler-conf-prop-by-order this))
	(let* ((name (slot-value prop 'name))
	       (val (or (slot-value this name) "<not set>")))
	  (insert (format "%S: %s\n" name val))))
      (read-only-mode 1)
      (display-buffer (current-buffer))
      (current-buffer))))



;; configuration based compiler
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

(provide 'flex-compile-config)

;;; flex-compile-config.el ends here
