;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; c.lisp -- Language related definitions for C/C++.

;;; This is very basic. Somebody else who needs it can add in support
;;; for header files, libraries, different C compilers, etc. For example,
;;; we might add a COMPILER-OPTIONS slot to the component defstruct.

(in-package "MK4")


(defstruct (c-language-compiler (:include language-compiler))
  (command-line "cc" :type string))

(defstruct (gcc-language-compiler (:include c-language-compiler)))
(defvar *gcc-language-compiler*
  (setf (find-language-processor :gcc)
	(make-gcc-language-compiler :name "gcc"
				    :command-line "gcc")))

(defstruct (sunpro-cc-language-compiler (:include c-language-compiler)))
(defvar *sunpro-cc-language-compiler*
  (setf (find-language-processor :sunpro-cc)
	(make-sunpro-cc-language-compiler :name "SunPRO cc")))


(defstruct (microsoft-c-language-compiler (:include c-language-compiler)))
(defvar *microsoft-c-language-compiler*
  (setf (find-language-processor :msc)
	(make-microsoft-c-language-compiler :name "Microsoft C"
					    :command-line "cl")))


(defparameter *c-compiler*  *gcc-language-compiler*)


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


;;; Generic interface to the C Compiler.
;;; Add other compilers as appropriate.

(defgeneric c-compile-file (compiler file-pathname
				     &rest args
				     &key output-file
				     &allow-other-keys))

(defmethod c-compile-file ((c-compiler gcc-language-compiler)
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
	  (run-c-compiler (c-language-compiler-command-line c-compiler)
			  arguments
			  output-file
			  error-file
			  error-output
			  verbose)
	(if (and error (or (not output-file) fatal-errors))
	    (error "Compilation failed")
	    (values output-file warnings fatal-errors))))))



;;; run-c-compiler --

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



;;; loader --

(defgeneric load-c-file (loadable-c-pathname
			 &key
			 (print *load-print*)
			 (verbose *load-verbose*)
			 (libraries '("c"))
			 )
  (:documentation
   "Loads a C object file (or similar) into the CL environment."))


(defmethod load-c-file ((pathname t) &rest args)
  (error "MK4: LOAD-C-FILE undefined for pathname ~S and arguments ~S."
	 pathname args))
  


;;; DEFINE-LANGUAGE and the mixin really provide the same
;;; functionality.
;;; Probably the mixin is better and DEFINE-LANGUAGE and stuff will
;;; just go away.

(define-language :c
  :compiler #'c-compile-file
  :loader #+:lucid #'load-foreign-files 
          #+:allegro #'load
          #-(or :lucid :allegro) #'load
  :source-extension "c"
  :binary-extension "o")


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


;;; end of file -- c.lisp --
