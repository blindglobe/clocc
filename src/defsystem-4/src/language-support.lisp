;;; -*- Mode: Lisp -*-

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




;;; Notes.
;;;
;;; 2002-05-22 Marco Antoniotti
;;; Let's bite the bullet.  CLOS is easier to deal with.

(defclass language-processor ()
  ((tag :reader language-processor-tag
	:initarg :tag
	:type symbol)
   (name :reader language-processor-name
	 :initarg :name
	 :type string))
  (:default-initargs :name "" :tag nil))


(defgeneric language-processor-p (x)
  (:method ((x language-processor)) t)
  (:method ((x t)) nil))

(defvar *language-processors* (make-hash-table)
  "Table containing language processor instances.
It is keyed by keywords.")


(defun find-language-processor (kwd)
  (declare (type symbol kwd))		; Maybe `keyword' would do.
  (gethash kwd *language-processors*))

(defun (setf find-language-processor) (kwd lp)
  (declare (type symbol kwd)
	   (type language-processor lp))
  (setf (gethash kwd *language-processors*) lp))


(defclass language-compiler (language-processor))

(defclass language-loader (language-processor))

(defclass language-interpreter (language-processor))


;;; Language Processors initialization methods.

(defmethod initialize-instance :after ((lp language-processor)
				       &key tag
				       &allow-other-keys)
  (setf (find-language-processor tag) lp))



;;; define-language-processor --
;;; This is essentially a "singleton" class definition form.
;;; The implementation is incorrect since I am not really using a
;;; SINGLETON :METACLASS.  Will fix this later.

(defmacro define-language-processor (name (&optional
					   (superclass 'language-processor))
					  slots
					  &rest
					  options)
  (let ((language-tag (or (find :tag options :key #'first)
			  (intern (symbol-name name)
				  (find-package "KEYWORD"))))
	)
    `(progn
       (defclass ,name (,superclass) ,slots ,@options)
       (setf ,name (make-instance ',name :tag ',language-tag)))))


;;; language class.
  
(defclass language ()
  ((name :reader language-name
	 :type symbol
	 :initarg :name)		; The name of the language (a
					; keyword).
   (compiler :accessor language-compiler ; Either a function or an
	     :type (or null		 ; instance of type
		       function		 ; `language-processor',
		       language-compiler)
	     :initarg :compiler)
   (processor :accessor language-processor ; As above.
	      :type (or null
			function
			language-processor)
	      :initarg :processor)
	     
   (loader :accessor language-loader	; As above.
	   :type (or null
		     function
		     language-loader)
	   :initarg :loader)

   (source-extension
    :accessor language-source-extension
    :type (or null string)
    :initarg :source-extension) ; Filename extensions for
					    ; source files. 
   (binary-extension
    :accessor language-binary-extension
    :type (or null string)
    :initarg :binary-extension) ; Filename extensions for
					    ; binary files.
   )
  (:default-initargs :compiler nil :loader nil :processor nil
		     :source-extension nil
		     :binary-extension nil)
  )


(defmethod print-object ((l language) stream)
  (print-unreadable-object (l stream)
     (format stream "MK4:LANGUAGE ~:@(~A~)~@[:~*~]~@[ source ext. ~S~]~
                     ~@[ binary ext. ~S~]"
	     (language-name l)
	     (or (language-source-extension l)
		 (language-binary-extension l))
	     (language-source-extension l)
	     (language-binary-extension l))))


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


(defvar *language-table* (make-hash-table :test #'equal)
  "Hash table that maps from languages to language instances.")

(defun find-language (name)
  (gethash name *language-table* nil))

(defun (setf find-language) (language-instance name)
  (declare (type symbol name)
	   (type language language-instance))
  (setf (gethash name *language-table*) language-instance))





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
  (assert (symbolp name))
  (let ((language (gensym "LANGUAGE"))
	(language-tag (intern (symbol-name name) (find-package "KEYWORD")))
	)
    `(let ((,language (make-instance 'language
				     :name ,language-tag
				     :compiler ,compiler
				     :loader ,loader
				     :processor ,processor
				     :source-extension ,source-extension
				     :binary-extension ,binary-extension)))
       (setf (find-language ',language-tag) ,language)
       ,language)))


;;; Common Lisp Language Definition (really useless, here for completenenss).

(define-language :common-lisp
  :compiler #'compile-file
  :loader #'load
  :source-extension "lisp"
  :binary-extension (cl.env:compiled-file-extension))

(define-language scheme
  :source-extension "scm"
  )


#||
;;; Test System for verifying multi-language capabilities.
(defsystem foo
  :language :lisp
  :components ((:module c :language :c :components ("foo" "bar")) 
	       (:module lisp :components ("baz" "barf"))))

||#


;;; end of file -- language-support.lisp --
