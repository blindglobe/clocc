;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; c.lisp -- Language related definitions for C/C++.

;;; This is very basic. Somebody else who needs it can add in support
;;; for header files, libraries, different C compilers, etc. For example,
;;; we might add a COMPILER-OPTIONS slot to the component defstruct.

(in-package "MK4")


;;; C language definition.

(defclass c-language-loader (language-loader object-loader)
  ()
  (:default-initargs :name "CL external C object file loader"))


(defclass c-language-compiler (language-compiler external-language-processor)
  ()
  (:default-initargs :name "Generic C Compiler" :command "cc" :tag :cc))


;;; The following classes should be singleton ones.

(defclass gcc-language-compiler (c-language-compiler
				 gnu-language-compiler)
  ()
  (:default-initargs :name "GNU C Compiler" :command "gcc" :tag :gcc))
  
(defvar *gcc-language-compiler*
  (make-instance 'gcc-language-compiler))


(defclass sunpro-cc-language-compiler (c-language-compiler)
  ()
  (:default-initargs :name "SunPRO C Compiler" :command "cc" :tag :sunpro-cc))
  
(defvar *sunpro-cc-language-compiler*
  (make-instance 'sunpro-cc-language-compiler))


(defclass microsoft-c-language-compiler (c-language-compiler)
  ()
  (:default-initargs :name "Microsoft C Compiler"
    :command "cl"
    :tag :msc))

(defvar *microsoft-c-language-compiler*
  (make-instance 'microsoft-c-language-compiler))


(defparameter *c-compiler*  *gcc-language-compiler*)

(defparameter *c-loader* *cl-foreign-object-loader*)


;;; The set of functions below may or may not be useful.  It is here
;;; as an example of how to extend the code.

(defgeneric  c-file-os-default-source-extension (os)
  (:method ((os cl.env:operating-system)) "c"))

(defun c-file-default-source-extension ()
  (c-file-os-default-source-extension cl.env:*os*))


(defgeneric  c-file-os-default-binary-extension (os)
  (:method ((os cl.env:unix)) "o")
  (:method ((os cl.env:ms-windows)) "obj"))

(defun c-file-default-binary-extension ()
  (c-file-os-default-binary-extension cl.env:*os*))


(defgeneric static-library-os-extension (os)
  (:method ((os cl.env:unix)) "a")
  (:method ((os cl.env:ms-windows)) "lib"))

(defun static-library-extension ()
  (static-library-os-extension cl.env:*os*))


(defgeneric shared-library-os-extension (os)
  (:method ((os cl.env:unix)) "so")
  (:method ((os cl.env:ms-windows)) "dll"))

(defun shared-library-extension ()
  (shared-library-os-extension cl.env:*os*))


(defun c-file-default-object-pathanme ()
  (make-pathname :type (c-file-default-binary-extension)))

(defun c-file-default-error-pathanme ()
  (make-pathname :type *compile-error-file-type*))

(defun c-file-default-log-pathanme ()
  (make-pathname :type "log"))
  

;;; invoke-compiler --

(defmethod invoke-compiler ((c-compiler c-language-compiler)
			    (file pathname)
			    &rest args
			    &key
			    (output-pathname
			     (output-file-pathname
			      file
			      :output-file
			      (c-file-default-object-pathanme)))
			    &allow-other-keys)
  (apply #'invoke-processor-external c-compiler file args))


#|| Old code
(defmethod invoke-compiler ((c-compiler gcc-language-compiler)
			    (file pathname)
			    &rest args
			    &key
			    (output-file t)
			    (error-file t)
			    (error-output t)
			    (verbose *compile-verbose*)
			    debug
			    link
			    optimize
			    cflags
			    definitions
			    include-paths
			    library-paths
			    libraries
			    (error t))
  (declare (ignore args))

  (flet ((map-options (flag options &optional (func #'identity))
	   (mapcar #'(lambda (option)
		       (format nil "~A~A" flag (funcall func option)))
		   options))
	 )
    (let* ((output-file (default-output-pathname output-file filename "o"))
	   (arguments
	    `(,@(when (not link) '("-c"))
		,@(when debug '("-g"))
		,@(when optimize (list (format nil "-O~D" optimize)))
		,@cflags
		,@(map-options
		   "-D" definitions
		   #'(lambda (definition)
		       (if (atom definition)
			   definition
			   (apply #'format nil "~A=~A" definition))))
		,@(map-options "-I" include-paths #'truename)
		,(namestring (truename filename))
		"-o"
		,(namestring (translate-logical-pathname output-file))
		,@(map-options "-L" library-paths #'truename)
		,@(map-options "-l" libraries))))

      (multiple-value-bind (output-file warnings fatal-errors)
	  (run-processor-command (c-language-compiler-command-line c-compiler)
				 arguments
				 output-file
				 error-file
				 error-output
				 verbose)
	(if (and error (or (not output-file) fatal-errors))
	    (error "Compilation failed")
	    (values output-file warnings fatal-errors))))))
||#


;;; run-c-compiler --

#|| Old code
(defun run-c-compiler (program
		       arguments
		       output-file
		       error-file
		       error-output
		       verbose)
  (flet ((make-useable-stream (&rest streams)
	   (apply #'make-broadcast-stream (delete nil streams)))
	 )
    (let ((error-file error-file)
	  (error-file-stream nil)
	  (verbose-stream nil)
	  (old-timestamp (file-write-date output-file))
	  (fatal-error nil)
	  (output-file-written nil)
	  )
      (unwind-protect
	   (progn
	     (setf error-file
		   (when error-file
		     (default-output-pathname error-file
 			                      output-file
		                              *compile-error-file-type*))

		   error-file-stream
		   (and error-file
			(open error-file
			      :direction :output
			      :if-exists :supersede)))

	     (setf verbose-stream
		   (make-useable-stream error-file-stream
					(and verbose *trace-output*)))

	     (format verbose-stream "Running ~A~@[ ~{~A~^ ~}~]~%"
		     program
		     arguments)

	     (setf fatal-error
		   (let* ((error-output
			   (make-useable-stream error-file-stream
						(if (eq error-output t)
						    *error-output*
						    error-output)))
			  (process-status
			   (run-os-program program
					   :arguments arguments
					   :error error-output)))
		     (not (zerop process-status))))

	     (setf output-file-written-p
		   (and (probe-file output-file)
			(not (= old-timestamp
				(file-write-date output-file)))))


	     (when output-file-written
	       (format verbose-stream "~A written~%" output-file))
	     
	     (format verbose-stream "Running of ~A finished~%"
		     program)

	     (values (and output-file-written output-file)
		     fatal-error
		     fatal-error))
	

	(when error-file
	  (close error-file-stream)
	  (unless (or fatal-error (not output-file-written))
	    (delete-file error-file)))

	(values (and output-file-written-p (truename output-file))
		fatal-error
		fatal-error)))))
||#


;;; Loader.
;;; No loader defined. See INVOKE-LOADER on OBJECT-LOADER in
;;; "MAKE-DEFSYSTEM:;language-support.lisp".


;;; C Language definition.

(define-language :c
    :compiler *c-compiler*
    :loader *c-loader*
    :source-extension "c"
    :binary-extension "o")


#| Old code

(defclass c-language-mixin (component-language-mixin)
  ((compiler :accessor c-file-compiler)
   (compilation-error-p :accessor c-file-compilation-error-p
			:initarg :compilation-error-p)
   (debug :accessor c-file-compile-debug
	  :initarg :debug)
   (link :accessor c-file-compile-link
	 :initarg :link)
   (optimize :accessor c-file-compile-optimize
	     :initarg :optimize)
   (cflags :accessor c-file-compile-cflags
	   :initarg :cflags
	   :type string)
   (cppflags :accessor c-file-compile-cppflags
	     :initarg :cppflags
	     :type string)
   (include-paths :accessor c-file-compile-include-paths
		  :initarg :include-paths
		  :type (or string
			    pathname
			    list ; (satisfies (every #'(lambda (x)
				 ;                        (or (stringp x)
				 ;                            (pathnamep x)))
				 ;                   list)
			    ))
   (library-paths :accessor c-file-compile-library-paths
		  :initarg :library-paths
		  :type (or string pathname list))
   (libraries :accessor c-file-compile-libraries
	      :initarg :libraries
	      :type list)		; (list string).
   )
  (:default-initargs
    :language :c
    :source-extension (c-file-default-source-extension)
    :binary-extension (c-file-default-binary-extension)
    :compiler *c-compiler*
    :loader #'load-c-file
    :compilation-error-p nil
    :debug t
    :link nil
    :optimize nil
    :cflags ""
    :cppflags ""
    :include-paths '()
    :library-paths '()
    :libraries '("c")
    ))

|#


;;; end of file -- c.lisp --
