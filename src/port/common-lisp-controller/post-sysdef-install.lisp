;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-

(in-package :common-lisp-controller)


(eval-when (:load-toplevel :execute :compile-toplevel)
  (unless (find-package :mk)
    (error "You need to load the mk defsystem before loading or compiling this file!"))
  (unless (find-package :asdf)
    (error "You need to load the asdf system before loading or compiling this file!")))


;; load-only-compiled-op is an ASDF operation that only loads
;; compiled code

(in-package :asdf)
(define-condition load-compiled-error (error)
  ((output-file :initform nil :initarg :output-file :reader output-file))
  (:report (lambda (c s)
	       (format s "Error when performing load-compiled-op with output file ~A"
		       (output-file c)))))

(define-condition load-compiled-error-not-exist (load-compiled-error)
  ()
  (:report (lambda (c s)
	       (format s "During load-compiled-op, compiled file ~A does not exist"
		       (output-file c)))))

(define-condition load-compiled-error-out-dated (load-compiled-error)
  ((source-file :initform nil :initarg :source-file :reader source-file))
  (:report (lambda (c s)
	     (format s "During load-compiled-op, compiled file ~A is older than source file ~A"
		     (output-file c) (source-file c)))))

;; Remove the :cltl2 that mk-defsystem3 pushes onto features
(setq cl:*features* (delete :cltl2 *features*))

(defclass load-compiled-op (load-op)
  ()
  (:documentation "This operation loads each compiled file for a component. If a binary component does not exist then the condition load-compiled-error-not-exist will be signaled. If both the source file and binary components exists, and if the file-write-date of the binary is earlier than the source, then the condition load-compiled-error-out-dated will be signaled."))

(defmethod output-files ((operation load-compiled-op)
			 (c cl-source-file))
  (list (compile-file-pathname (component-pathname c))))

(defmethod operation-done-p ((o load-compiled-op) (c source-file))
  "Compare the :last-loaded time of the source-file to the file-write-date of
the output files. If :last-loaded is not NIL and is greater
than the maximum file-write-date of output-files, return T."
  (let ((load-date (component-property c 'last-loaded))
	(max-binary-date (maximum-file-write-date (output-files o c))))
    (when (and load-date max-binary-date (>= load-date max-binary-date))
      t)))
  
(defun maximum-file-write-date (files)
  "Returns NIL if some files don't exist"
  (let ((max nil))
    (dolist (f files)
      (unless (probe-file f)
	(return nil))
      (let ((date (file-write-date f)))
	(if max
	    (when (< max date)
	      (setq max date))
	  (setq max date))))
    max))

(defmethod perform ((o load-compiled-op) (c source-file))
  (let* ((co (make-sub-operation o 'compile-op))
	 (output-files (output-files co c))
	 (source-date (when (probe-file (component-pathname c))
			(file-write-date (component-pathname c)))))
    (dolist (of output-files)
      (unless (probe-file of)
	(error 'load-compiled-error-not-exist
	       :output-file of))
      (when (and source-date
		 (> source-date (file-write-date of)))
	(error 'load-compiled-error-out-dated
	       :output-file of
	       :source-file (component-pathname c)))
      (load of))
    (setf (component-property c ':last-loaded)
	  (maximum-file-write-date output-files))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'load-compiled-op))

(in-package :common-lisp-controller)

;;
(defmethod asdf:output-files :around ((op asdf:operation) (c asdf:component))
  "Method to rewrite output files to fasl-root"
  (let ((orig (call-next-method)))
    (if (beneath-source-root? c)
	(mapcar #'(lambda (y)
		    (merge-pathnames 
		     (enough-namestring y (asdf::resolve-symlinks *source-root*))
		     *fasl-root*))
		orig)
      orig)))


(defun beneath-source-root? (c)
  "Returns T if component's directory below *source-root*"
  (let ((root-dir (pathname-directory (asdf::resolve-symlinks *source-root*))))
    (and c
	 (equalp root-dir
		 (subseq (pathname-directory (asdf:component-pathname c))
			 0 (length root-dir))))))

  
(defun system-in-source-root? (c)
  "Returns T if component's directory is the same as *source-root* + component's name"
  (and c
       (equalp (pathname-directory (asdf:component-pathname c))
	       (pathname-directory (asdf::resolve-symlinks
				    (merge-pathnames
				     (make-pathname
				      :directory (list :relative
						       (asdf:component-name c)))
				     *source-root*))))))
  
(defun find-system (module-name)
  "Looks for name of system. Returns :asdf if found asdf file or
:defsystem3 if found defsystem file."
  (cond
   ;; probing is for weenies: just do
   ;; it and ignore the errors :-)
   ((ignore-errors
      (let ((system (asdf:find-system module-name)))
	(when (system-in-source-root? system)
	  :asdf))))
   ((ignore-errors
      (equalp
       (pathname-host
	(make::component-source-root-dir
	 (mk:find-system module-name
			 :load-or-nil)))
       (pathname-host
	(pathname
	 "cl-library:"))))
    :defsystem3)
   (t
    nil)))

     

(defun require-defsystem3 (module-name)
  ;; if in the clc root:
  (let ((system (mk:find-system module-name
				:load-or-nil)))
    (or
     ;; try to load it
     (mk:oos  module-name
	      :load
	      :load-source-instead-of-binary nil
	      :load-source-if-no-binary nil
	      :bother-user-if-no-binary nil
	      :compile-during-load nil)
     (progn
       (format t "~&;;;Please wait, recompiling library...")
       ;; first compile the sub-components!
       (dolist (sub-system (make::component-depends-on system))
	 (when (stringp sub-system)
	   (setf sub-system (intern sub-system (find-package :keyword))))
	 (clc-require sub-system))
       (common-lisp-controller:send-clc-command :recompile
						(if (stringp module-name)
						    module-name
						  (string-downcase
						   (symbol-name
						    module-name))))
       (terpri)
       (mk:oos  module-name
		:load
		:load-source-instead-of-binary nil
		:load-source-if-no-binary nil
		:bother-user-if-no-binary nil
		:compile-during-load nil)
       t))))


(defun require-asdf (module-name)
  (let ((system (asdf:find-system module-name)))
    (when system
       (handler-case
	(asdf:oos 'asdf:load-compiled-op module-name)	; try to load it
	(error ()
	       (format t "~&;;;Please wait, recompiling library...")
	       ;; first compile the depends-on
	       (dolist (sub-system
			;; skip asdf:load-op at beginning of first list
			(cdar (asdf:component-depends-on
			      (make-instance 'asdf:compile-op) system)))
		 (clc-require sub-system))
	       (let ((module-name-str
		      (if (stringp module-name)
			  module-name
			(string-downcase (symbol-name module-name)))))
		 (common-lisp-controller:send-clc-command :recompile module-name-str))
	       (terpri)
	       (asdf:oos 'asdf:load-compiled-op module-name) 
	       t)
	(:no-error (res)
		   (declare (ignore res))
		   t)))))

;; we need to hack the require to
;; call clc-send-command on load failure...
(defun clc-require (module-name &optional (pathname 'c-l-c::unspecified))
  (if (not (eq pathname 'c-l-c::unspecified))
      (original-require module-name pathname)
    (let ((system-type (find-system module-name)))
      (case system-type
	(:defsystem3
	 (require-defsystem3 module-name))
	(:asdf
	 (require-asdf module-name))
	(otherwise
	 (original-require module-name))))))

(defun compile-library (library)
  "Recompiles the given library"
  (let ((system (find-system library)))
    (case system
      (:defsystem3
       (mk:oos library :compile :verbose nil))
      (:asdf
       (asdf:oos 'asdf:compile-op library))
      (t
       (format t "~%Package ~S not found... ignoring~%"
	       library))))
  (values))


;; override the standard require with this:
;; ripped from mk:defsystem:
(eval-when (:load-toplevel :execute)
  #-(or (and allegro-version>= (version>= 4 1)) :lispworks :openmcl)
  (setf (symbol-function
         #-(or (and :excl :allegro-v4.0) :mcl :sbcl :lispworks) 'lisp:require
         #+(and :excl :allegro-v4.0) 'cltl1:require
         #+:lispworks3.1 'common-lisp::require
         #+:sbcl 'cl:require
         #+(and :lispworks (not :lispworks3.1)) 'system::require
         #+:mcl 'ccl:require)
        (symbol-function 'clc-require))

  #+:openmcl
  (let ((ccl::*warn-if-redefine-kernel* nil))
    (setf (symbol-function 'ccl:require) (symbol-function 'clc-require)))
  
  #+:lispworks
  (let ((warn-packs system::*packages-for-warn-on-redefinition*))
    (declare (special system::*packages-for-warn-on-redefinition*))
    (setq system::*packages-for-warn-on-redefinition* nil)
    (setf (symbol-function
           #+:lispworks3.1 'common-lisp::require
           #-:lispworks3.1 'system::require
           )
          (symbol-function 'clc-require))
    (setq system::*packages-for-warn-on-redefinition* warn-packs))
  #+(and allegro-version>= (version>= 4 1))
  (excl:without-package-locks
   (setf (symbol-function 'lisp:require)
	 (symbol-function 'clc-require))))

