;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-

(in-package #:common-lisp-controller)


(eval-when (:load-toplevel :execute :compile-toplevel)
  (unless (find-package :mk)
    (error "You need to load the mk defsystem before loading or compiling this file!"))
  (unless (find-package :asdf)
    (error "You need to load the asdf system before loading or compiling this file!")))


;; Remove the :cltl2 that mk-defsystem3 pushes onto features
(setq cl:*features* (delete :cltl2 *features*))

;; Take from CLOCC's GPL'd Port package
(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (sys::getenv (string var))
  #+(or cmu scl) (cdr (assoc (string var) ext:*environment-list* :test #'equalp
                    :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+mcl (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)
  #-(or allegro clisp cmu scl gcl lispworks lucid mcl sbcl) "")

(defun append-dir-terminator-if-needed (path)
  (check-type path string)
  (if (char= #\/ (char path (1- (length path))))
      path
      (concatenate 'string path "/")))

(defun user-clc-path ()
  "Returns the path of the user's local clc directory, NIL if directory does not exist."
  (let* ((home-dir (getenv "HOME"))
	 (len (when (stringp home-dir)
		(length home-dir))))
    (when (and len (plusp len))
      (setq home-dir (append-dir-terminator-if-needed home-dir))
      (setq home-dir (parse-namestring home-dir))
      (merge-pathnames
       (make-pathname :directory '(:relative ".clc"))
       home-dir))))


;; Function
(defun is-user-package-system (system)
  "Return pathname of system file if this is a user package"
  (let ((path (make-pathname :name (asdf::coerce-name system)
			     :type "asd"
			     :directory (append
					 (pathname-directory (user-clc-path))
					 '("systems")))))
    (when (probe-file path)
      path)))

(push #' is-user-package-system asdf::*system-definition-search-functions*)

(defun asdf-system-compiled-p (system)
  "Returns T is an ASDF system is already compiled" 
  (notany #'(lambda (op) (and (typep (car op) 'asdf:compile-op)
			      (not (asdf:operation-done-p (car op) (cdr op)))))
	  (asdf::traverse (make-instance 'asdf:compile-op) system)))

(defun user-packages-path ()
  (make-pathname :defaults (user-clc-path)
		 :name "user-packages"
		 :type "db"))

(defvar *cached-user-packages* nil
  "Cache list of user packages")
(defvar *cached-user-packages-date* nil
  "Cached file-write-date of user packages")

(defun load-user-package-list ()
  "Return list of user packages (pathnames)"
  (let ((pkgs-path (user-packages-path)))
    (when (probe-file pkgs-path)
      (let ((pkgs '()))
	(with-open-file (strm pkgs-path :direction :input :if-does-not-exist nil)
  	  (when strm
	    (do ((line (read-line strm nil 'eof) (read-line strm nil 'eof)))
		((eq line 'eof))
	      (let ((path (ignore-errors (parse-namestring line))))
		(when path
		  (push path pkgs))))))
	pkgs))))


(defun user-package-list ()
  "Returns user package list from cache, updates cached version if needed."
  (let* ((path (user-packages-path))
	 (exists (probe-file path))
	 (file-date (when exists
		      (file-write-date path))))
    (when exists
      (if file-date
	  (if (or (null *cached-user-packages-date*)
		  (> file-date *cached-user-packages-date*))
	      (progn
		(setq *cached-user-packages-date* file-date)
		(setq *cached-user-packages* (load-user-package-list)))
	      *cached-user-packages*)
	  (load-user-packages-list)))))

(defun in-user-package (c)
  "Returns T if the ADSF component is in a component of a registered user package."
  (check-type c asdf:component)
  (let* ((system (asdf::component-system c))
	 (system-path (asdf:component-pathname system))
	 (system-name (asdf:component-name system))
	 (asdf-path-string (namestring
			    (make-pathname :defaults system-path
					   :name system-name
					   :type "asd"))))
    (dolist (pkg (user-package-list))
      (when (string= asdf-path-string (namestring pkg))
	(return-from in-user-package t)))
    nil))

(defun user-package-root (c)
  "Return the output file root for a user-package asdf component"
  (let* ((system (asdf::component-system c))
	 (system-name (asdf:component-name system)))
    (merge-pathnames
     (make-pathname :directory (list :relative system-name))
     (user-lib-path))))

(defun source-root-path-to-fasl-path (source)
  "Converts a path in the source root into the equivalent path in the fasl root"
  (merge-pathnames 
   (enough-namestring source (asdf::resolve-symlinks *source-root*))
   *fasl-root*))

(defmethod asdf:output-files :around ((op asdf:operation) (c asdf:component))
  "Method to rewrite output files to fasl-root"
  (let ((orig (call-next-method)))
    (cond
     ((beneath-source-root? c)
      (mapcar #'source-root-path-to-fasl-path orig))
     ((in-user-package c)
      (let ((user-package-root (user-package-root c)))
	(mapcar #'(lambda (y)
		    (merge-pathnames 
		     (enough-namestring y (asdf:component-pathname
					   (asdf::component-system c)))
		     user-package-root))
		orig)))
     (t
      orig))))

(defun beneath-source-root? (c)
  "Returns T if component's directory below *source-root*"
  (when c
    (let ((root-dir (pathname-directory (asdf::resolve-symlinks *source-root*)))
	  (comp-dir (pathname-directory (asdf:component-pathname c))))
      (and (>= (length comp-dir)
	       (length root-dir))
	   (equalp root-dir (subseq comp-dir 0 (length root-dir)))))))

  
(defun system-in-source-root? (c)
  "Returns T if component's directory is the same as *source-root* + component's name"
  ;; asdf::resolve-symlinks gives an error for non-existent pathnames
  ;; on lispworks
  (ignore-errors
    (and c
	 (equalp (pathname-directory (asdf:component-pathname c))
		 (pathname-directory
		  (asdf::resolve-symlinks
		   (merge-pathnames
		    (make-pathname
		     :directory (list :relative (asdf:component-name c)))
		    *source-root*)))))))

(defun ensure-user-packages-added ()
  (dolist (pkg (user-package-list))
    (let ((dir (namestring (asdf::pathname-sans-name+type pkg))))
      (unless (find-if
	       #'(lambda (entry)
		   (setq entry (eval entry))
		   (when (typep entry 'pathname)
		     (setq entry (namestring entry)))
		   (when (string= entry dir)
		     t))
		   asdf:*central-registry*)
	(push dir asdf:*central-registry*)))))

(defun find-system-def (module-name)
  "Looks for name of system. Returns :asdf if found asdf file or
:defsystem3 if found defsystem file."
  (ensure-user-packages-added)
  (cond
   ;; probing is for weenies: just do
   ;; it and ignore the errors :-)
   ((ignore-errors
      (let ((system (asdf:find-system module-name)))
	(when (or (system-in-source-root? system)
		  (in-user-package system))
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


(defvar *recompiling-from-daemon* nil
  "T if we are recompiling on orders of the clc-build-daemon")

(defun require-defsystem3 (module-name)
  ;; if in the clc root:
  (let ((system (mk:find-system module-name
				:load-or-nil)))
    (or
     ;; try to load it
     (progn
       (mk:oos  module-name
                :load
                :load-source-instead-of-binary nil
                :load-source-if-no-binary nil
                :bother-user-if-no-binary nil
                :compile-during-load nil)
       ;; did we load it?
       (find module-name mk::*modules* :test #'string=))
     (progn
       (format t "~&;;;Please wait, recompiling library...")
       (cond
         (*recompiling-from-daemon*
          (mk:oos module-name
                  :compile
                  :verbose nil))
         (t
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
                                                         module-name))))))
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
      (if (asdf-system-compiled-p system)
	  (asdf:oos 'asdf:load-op module-name)
	(progn
	  (format t "~&;;; Please wait, recompiling library...")
	  (cond
	   (*recompiling-from-daemon*
	    (asdf:oos 'asdf:compile-op module-name))
	   (t
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
	      ;; clc-build-daemon will report pkg built unless all components are removed
	      (common-lisp-controller:send-clc-command :remove module-name-str)
	      (common-lisp-controller:send-clc-command :recompile module-name-str))))
	  (terpri)
	  (asdf:oos 'asdf:load-op module-name)))
      t)))

;; we need to hack the require to
;; call clc-send-command on load failure...
(defun clc-require (module-name &optional (pathname 'c-l-c::unspecified))
  (if (not (eq pathname 'c-l-c::unspecified))
      (original-require module-name pathname)
    (let ((system-type (find-system-def module-name)))
      (case system-type
	(:defsystem3
	 (require-defsystem3 module-name))
	(:asdf
	 (require-asdf module-name))
	;; Don't call original-require with SBCL since we are called by that function
	#-sbcl 
	(otherwise
	 (original-require module-name))))))

(defun compile-library (library)
  "Recompiles the given library"
  (let* (;; this is because of clc-build-daemon
         (*recompiling-from-daemon* t)
	 (system (find-system-def library)))
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
  #-(or (and allegro-version>= (version>= 4 1)) :lispworks :openmcl :sbcl)
  (setf (symbol-function
         #-(or (and :excl :allegro-v4.0) :mcl :lispworks) 'lisp:require
         #+(and :excl :allegro-v4.0) 'cltl1:require
         #+:lispworks3.1 'common-lisp::require
         #+(and :lispworks (not :lispworks3.1)) 'system::require
         #+:mcl 'ccl:require)
        (symbol-function 'clc-require))

  #+:openmcl
  (let ((ccl::*warn-if-redefine-kernel* nil))
    (setf (symbol-function 'cl:require) (symbol-function 'clc-require)))
  
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
	 (symbol-function 'clc-require)))

  #+sbcl
  (progn
    (push 'clc-require sb-ext:*module-provider-functions*)
    ;; Overcome the effect of mk-defsystem3's override of 'cl:require
    (if (symbol-function 'sb-ext:without-package-locks)
	(sb-ext:without-package-locks
	 (setf (symbol-function 'cl:require) *original-require-function*))
	(setf (symbol-function 'cl:require) *original-require-function*)))
  )


(defun user-lib-path ()
  (merge-pathnames
   (make-pathname :directory
		  (list :relative "bin" (car (last (pathname-directory *fasl-root*)))))
   (user-clc-path)))

  

