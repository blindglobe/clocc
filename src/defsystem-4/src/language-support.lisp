;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; language-support.lisp --

(in-package "MK4")


;;; ********************************
;;; Language-Dependent Characteristics
;;; ********************************
;;; This section is used for defining language-specific behavior of
;;; defsystem. If the user changes a language definition, it should
;;; take effect immediately -- they shouldn't have to reload the
;;; system definition file for the changes to take effect. 

(defvar *language-table* (make-hash-table :test #'equal)
  "Hash table that maps from languages to language structures.")

(defvar *language-processors* (make-hash-table)
  "Table containing language processor instances.
It is keyed by keywords.")

(defun find-language (name)
  (gethash name *language-table* nil))


(defstruct (language-processor)
  (key nil :type symbol)
  (name "" :type string))

(defstruct (language-compiler (:include language-processor))
  )

(defstruct (language-loader (:include language-processor))
  )

(defstruct (language-interpreter (:include language-processor))
  )


(defun find-language-processor (kwd)
  (declare (type symbol kwd))		; Maybe `keyword' would do.
  (gethash kwd *language-processors*))

(defun (setf find-language-processor) (kwd lp)
  (declare (type symbol kwd)
	   (type language-processor))
  (setf (gethash kwd *language-processors*) lp))


;;; defmacro define-language-processor would be helpful.


  
(defstruct (language (:print-function print-language))
  (name nil :type symbol)		; The name of the language (a
					; keyword).
  (compiler nil				; Either a function or an
	    :type (or null		; instance of type
		      function		; `language-processor',
		      language-compiler))
  (processor nil			; As above.
	     :type (or null
		       function
		       language-processor))
	     
  (loader nil				; As above.
	  :type (or null
		    function
		    language-loader))
  (traditional-source-extension "" :type string)	; Filename
							; extensions
							; for source
							; files. 
  (traditional-binary-extension "" :type string)	; Filename
							; extensions
							; for binary
							; files.
  )

(defun print-language (language stream depth)
  (declare (ignore depth))
  (print-unreadable-object (language stream)
     (format stream "~:@(~A~) ~A ~A"
	     (language-name language)
	     (language-source-extension language)
	     (language-binary-extension language))))


(defclass component-language-mixin ()
  ((language :accessor component-language
	     :initarg :language
	     :type (or null symbol))
   (compiler :accessor component-compiler
	     :initarg :compiler
	     :type (or null function))
   (loader   :accessor component-loader
	     :initarg :loader
	     :type (or null function))
   )
  (:default-initargs :language :common-lisp)
  (:documentation
   "A 'mixin' class used to specify a component language other than CL."))


(defun compile-function (component)
  (or (component-compiler component)
      (let ((language (find-language (or (component-language component)
					 :lisp))))
	(when language (language-compiler language)))
      #'compile-file))


(defun load-function (component)
  (or (component-loader component)
      (let ((language (find-language (or (component-language component)
					 :lisp))))
	(when language (language-loader language)))
      #'load))


(defmacro define-language (name &key compiler loader processor
				source-extension binary-extension)
  (let ((language (gensym "LANGUAGE")))
    `(let ((,language (make-language :name ,name 
				     :compiler ,compiler
				     :loader ,loader
				     :processor ,processor
				     :traditional-source-extension ,source-extension
				     :traditional-binary-extension ,binary-extension)))
       (setf (gethash ,name *language-table*) ,language)
       ,name)))

;;; Common Lisp Language Definition (really useless, here for completenenss).

(define-language :common-lisp
  :compiler #'compile-file
  :loader #'load
  :source-extension "lisp"
  :binary-extension (cl.env:compiled-file-extension))


#||
;;; Test System for verifying multi-language capabilities.
(defsystem foo
  :language :lisp
  :components ((:module c :language :c :components ("foo" "bar")) 
	       (:module lisp :components ("baz" "barf"))))

||#


;;; end of file -- language-support.lisp --
