;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-

(eval-when (:load-toplevel :execute :compile-toplevel)
  (unless (find-package :mk)
    (error "You need to load the mk defsystem before loading or compiling this file!"))
  (unless (find-package :asdf)
    (error "You need to load the asdf system before loading or compiling this file!")))


;; Make specialized operator to load only binary files
;; Signal error if a binary component is missing
(in-package :asdf)

(defclass load-only-compiled-op (operation)
  ((forced-p :initform t)))
(export 'load-only-compiled-op)

(defclass clc-compile-op (compile-op) ())
(export 'clc-compile-op)

(defmethod output-files ((operation load-only-compiled-op) (c cl-source-file))
  (list (compile-file-pathname (component-pathname c))))

(defmethod perform ((o load-only-compiled-op) (c cl-source-file))
  (let* ((co (make-sub-operation o 'clc-compile-op))
	 (output-files (output-files co c)))
    (setf (component-property c 'last-loaded)
	  (if (some #'not (map 'list #'load output-files))
	      nil
	    (file-write-date (car output-files))))))

(defmethod output-files :around ((c load-only-compiled-op) x)
  (declare (ignore x))
  (let ((orig (call-next-method)))
    (mapcar #'(lambda (y)
		(merge-pathnames 
		 (enough-namestring y c-l-c::*source-root*) c-l-c::*fasl-root*))
	    orig)))

(defmethod output-files :around ((c clc-compile-op) x)
  (declare (ignore x))
  (let ((orig (call-next-method)))
    (mapcar #'(lambda (y)
		(merge-pathnames 
		 (enough-namestring y c-l-c::*source-root*) c-l-c::*fasl-root*))
	    orig)))

(in-package :common-lisp-controller)

(defun find-system (module-name)
  "Looks for name of system. Returns :defsystem3 if found defsystem
file or :asdf if found asdf file"
  (cond
   ;; probing is for weenies: just do
   ;; it and ignore the errors :-)
   ((ignore-errors
      (equalp
       (pathname-directory
	(asdf:component-pathname
	 (asdf:find-system module-name)))
       (pathname-directory *systems-root*)))
    :asdf)
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
       (dolist (sub-system (make::component-depends-on
			    system))
	 (when (stringp sub-system)
	   (setf sub-system
		 (intern sub-system
			 (find-package :keyword)))
	   (clc-require sub-system)))
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


(defun clc-asdf-oos (op-class system &rest args)
  (let ((op (apply #'make-instance op-class
		   :original-initargs args args))
	(system (if (typep system 'asdf:component) system (asdf:find-system system))))
    (unless (slot-value system 'asdf::relative-pathname)
      (setf (slot-value system 'asdf::relative-pathname)
	    (merge-pathnames
	     (make-pathname :directory (list :relative
					     (asdf:component-name system)))
	     *source-root*)))
    (with-compilation-unit ()
      (asdf::traverse op system 'asdf::perform))))

    
(defun require-asdf (module-name)
  ;; if in the clc root:
  (let ((system (asdf:find-system module-name)))
    (or
     ;; try to load it
     (ignore-errors (clc-asdf-oos 'asdf::load-only-compiled-op module-name))
     ;; if not: try to compile it
     (progn
       (format t "~&;;;Please wait, recompiling library...")
       ;; first compile the sub-components!
       (dolist (sub-system (asdf:module-components system))
	 (when (typep sub-system 'asdf:system)
	   (setf sub-system
		 (intern sub-system
			 (find-package :keyword)))
	   (clc-require sub-system)))
       (common-lisp-controller:send-clc-command :recompile
						(if (stringp module-name)
						    module-name
						  (string-downcase
						   (symbol-name
						    module-name))))
       (terpri)
       (clc-asdf-oos 'asdf::load-only-compiled-op module-name) 
       t))))


;; we need to hack the require to
;; call clc-send-command on load failure...
(defun clc-require (module-name &optional pathname definition-pname
				default-action (version mk::*version*))
  (let ((system-type (find-system module-name)))
    (case system-type
     (:defsystem3
      (if (not (or pathname
		   definition-pname
		   default-action ))
	  ;; no advanced stuff
	  (require-defsystem3 module-name)
	;; ifnot, let original require deal with it..
	(original-require module-name pathname)))
     (:asdf
      (if (not (or pathname
		   definition-pname
		   default-action ))
	  ;; no advanced stuff
	  (require-asdf module-name)
	;; ifnot, let original require deal with it..
	(original-require module-name pathname)))
     ;; Call original require if can't find system file
     (otherwise
      (original-require module-name pathname)))))


(defun compile-library (library)
  "Recompiles the given library"
  (let ((system (find-system library)))
    (case system
      (:defsystem3
       (mk:oos library :compile :verbose nil))
      (:asdf
       (clc-asdf-oos 'asdf:clc-compile-op library))
      (t
       (format t "~%Package ~S not found... ignoring~%"
	       library))))
  (values))


;; override the standard require with this:
;; ripped from mk:defsystem:
(eval-when (:load-toplevel :execute)
  #-(or (and allegro-version>= (version>= 4 1)) :lispworks)
  (setf (symbol-function
         #-(or (and :excl :allegro-v4.0) :mcl :sbcl :lispworks) 'lisp:require
         #+(and :excl :allegro-v4.0) 'cltl1:require
         #+:lispworks3.1 'common-lisp::require
         #+:sbcl 'cl:require
         #+(and :lispworks (not :lispworks3.1)) 'system::require
         #+:mcl 'ccl:require)
        (symbol-function 'clc-require))

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

