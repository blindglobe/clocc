;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-

(in-package :cl-user)

(defpackage :common-lisp-controller
    (:use :common-lisp)
    (:export #:init-common-lisp-controller
             #:add-project-directory
             #:add-translation
             #:send-clc-command
	     #:make-clc-send-command-string
	     #:*clc-send-command-filename*
             #:compile-library
	     #:clc-require
	     #:source-root-path-to-fasl-path)
    (:nicknames :c-l-c))

(in-package :common-lisp-controller)

;; Some general utilities to make the
;; descriptions shorter

(defparameter *source-extentions* (list "cl" "lisp"
					"l" "lsp"
					"c" "h"))

(defvar *fasl-type*
  (load-time-value
   (pathname-type
    (compile-file-pathname "foo.lisp")))
  "This is the type of compiled lisp files.")


(defun add-translation (for new-root new-part)
  "Adds a translation to the logical pathname named by FOR (:cl-library or :cl-systems)
NEW-ROOT is the new root for this translation, NEW-PART is the part below the
root that should be added.
For example
 (add-translation :cl-library \"/home/pvaneynd/junk-pile/\"
                  (make-pathname :directory '(:RELATIVE \"HEMLOCK\" :wild-inferiors)
                             :type *fasl-type*
                                 :case :common))
should add a translation (in CMUCL) that
cl-library:hemlock;**;*.x86f.* -> /home/pvaneynd/junk-pile/hemlock/**/*.x86f

This function returns nothing."
  (let ((lp-host (ecase for
                   (:cl-library
                    "cl-library")
                   (:cl-systems
                    "cl-systems")))
        ;; force to pathnames
        (new-root (pathname new-root))
        (new-part (pathname new-part)))
    
    (assert (eq (first (pathname-directory new-part))
                :relative)
            (new-part)
            "The NEW-PART parameter ~S is not relative to something, it has to be"
            new-part)
    
    (let ((new-source
	   ;; construct based on new-part but in the right logical pathname
	   (namestring (make-pathname :defaults new-part
				      :name :wild
				      :host lp-host)))
          ;; construct the destination, based on all this
          (new-dest
	   (make-pathname :defaults new-part
			  :name :wild
			  ;; but under new-root
			  :directory (append (pathname-directory new-root)
					     ;; skip the relative
					     (rest (pathname-directory new-part))))))
      (push (list new-source
		  new-dest)
	    (logical-pathname-translations lp-host)))
    ;; also support the old way
    (let ((new-source
	   ;; construct based on new-part but in the right logical pathname
	   (namestring (make-pathname :defaults new-part
				      :directory (cons :absolute
						       (rest
							(pathname-directory new-part)))
				      :host lp-host)))
	  ;; construct the destination, based on all this
	  (new-dest
	   (make-pathname :defaults new-part
			  ;; but under new-root
			  :directory (append (pathname-directory new-root)
					     ;; skip the relative
					     (rest (pathname-directory new-part))))))
	  (push (list new-source
                  new-dest)
            (logical-pathname-translations lp-host)))
    (values)))


(defvar *fasl-root* nil "Root of implementation's directories of binary files")
(defvar *source-root* nil "Root of source directories")
(defvar *systems-root* nil "Root of systems directory")

(defun pathname-dir-only (path)
  (when (stringp path)
    (setq path (parse-namestring path)))
  (make-pathname :host (pathname-host path)
		 :device (pathname-device path)
		 :directory (pathname-directory path)))

(defun init-common-lisp-controller (fasl-root
                                    &key
                                    (source-root "/usr/share/common-lisp/")
                                    (version 2))
  "configures FASL-ROOT as the base of the fasl tree and optionally
SOURCE-ROOT as the root of the source tree.
NOTE: NUKES the cl-library and cl-systems LOGICAL PATHNAMES

The version argument selects which behaviour the implementation requires:
if (>= version 3): load defsystem and patch require.
                   implementation needs to do nothing else.

Returns nothing"
  ;; force both parameters to directories...
  (let* ((fasl-root (pathname-dir-only fasl-root))
         (s-root (pathname-dir-only source-root))
         (source-root (make-pathname
		       :type :wild
		       :name :wild
                       :directory (append (pathname-directory s-root)
                                          '("source"))))
         (system-root (make-pathname
		       :type :wild
		       :name :wild
                       :directory (append (pathname-directory s-root)
                                          '("systems")))))
    (setq *fasl-root* fasl-root)
    (setq *source-root* (pathname-dir-only source-root))
    (setq *systems-root* (pathname-dir-only system-root))
    
    (setf (logical-pathname-translations "cl-library")
          nil)
    (setf (logical-pathname-translations "cl-systems")
          nil)
        ;;; by default everything is in the fasl tree...
    (setf (logical-pathname-translations "cl-library")
	  `(("cl-library:;**;*.*.*" ,(make-pathname :directory (append (pathname-directory fasl-root)
								       (list :wild-inferiors))
						    :name :wild
						    :type :wild))
	    ("cl-library:**;*.*.*" ,(make-pathname :directory (append (pathname-directory fasl-root)
								      (list :wild-inferiors))
						   :name :wild
						   :type :wild))))
    ;;; add common source extentions:
    (loop for extention in *source-extentions*
	do
          (add-translation :cl-library
                           source-root
                           (make-pathname :directory '(:relative :wild-inferiors)
					  :name :wild
                                          :type extention)))
    ;; now cl-systems:
    ;; by default everything is in the fasl tree...
    (setf (logical-pathname-translations "cl-systems")
	  `(("cl-systems:;**;*.system" ,(make-pathname :directory (append (pathname-directory system-root)
									  (list :wild-inferiors))
						       :name :wild
						       :type "system"))
	    ("cl-systems:**;*.system" ,(make-pathname :directory (append (pathname-directory system-root)
									 (list :wild-inferiors))
						      :name :wild
						      :type "system"))))
    (when (>= version 3)
      (flet ((compile-and-load (pkg filename)
	       (let* ((file (parse-namestring filename))
		      (file-path
		       (merge-pathnames
			(make-pathname :name (pathname-name file)
				       :type (pathname-type file)
				       :directory (list :relative pkg))
			source-root))
		      (output-path
		       (merge-pathnames
			(make-pathname :name (pathname-name file)
				       :type (pathname-type file)
				       :directory (list :relative pkg))
			fasl-root))
		      (compiled-file-pathname
		       (compile-file-pathname output-path)))
		  ;; first make the target directory:
		  (ensure-directories-exist compiled-file-pathname)
		  ;; now compile it:
		  (compile-file file-path
				:output-file compiled-file-pathname
				:print nil
				:verbose nil)
		  ;; then load it:
		  (load compiled-file-pathname))))
        ;; first ourselves:
        (compile-and-load  "common-lisp-controller"
			   "common-lisp-controller.lisp")
        ;; then the patches before defsystem:
        (compile-and-load  "common-lisp-controller"
			   "pre-sysdef-install.lisp")
        ;; then defsystem:
        (compile-and-load  "defsystem" "defsystem.lisp")
	;; then asdf:
	;; For SBCL, take advantage of it's REQUIRE/contrib directories integration
	#+sbcl (when (boundp 'sb-ext::*module-provider-functions*)
		 (pushnew :sbcl-hooks-require cl:*features*))
        (compile-and-load  "asdf" "asdf.lisp")
        (compile-and-load  "asdf" "wild-modules.lisp")
	#+sbcl (pushnew
		  '(merge-pathnames #P".sbcl/systems/" (user-homedir-pathname))
		  (symbol-value (intern (symbol-name '#:*central-registry*)
					(find-package :asdf))))
	#+sbcl (setq cl:*features* (delete :sbcl-hooks-require  cl:*features*))
        ;; then the patches:
        (compile-and-load  "common-lisp-controller" "post-sysdef-install.lisp")
        ;; register ourselves:
	(push *systems-root*
	      (symbol-value (intern (symbol-name :*central-registry*)
				    (find-package :make))))
	(push *systems-root*
	      (symbol-value (intern (symbol-name :*central-registry*)
				    (find-package :asdf))))))
    (values)))

(defun add-project-directory (source-root fasl-root projects &optional system-directory)
  "This registers a SOURCE-ROOT and FASL-ROOT translation for the subdirectory
project for all PROJECTS.
Optionally you can also register SYSTEM-DIRECTORY.
Returns nothing"
  (declare (type pathname source-root fasl-root)
  	   (type (or null pathname) system-directory))
  (loop for project in projects
        do
        (let ((project project))
          (add-translation
           :cl-library fasl-root
           (make-pathname :directory (list :relative project :wild-inferiors)
			  :name :wild
                          :type :wild))
          (loop for extention in *source-extentions* do
                (add-translation
                 :cl-library source-root
                 (make-pathname :directory (list :relative project :wild-inferiors)
				:name :wild
                                :type extention)))))
  (when system-directory
    (pushnew  system-directory
              (symbol-value
               (intern (symbol-name :*central-registry*)
                       (find-package :make)))
              :test #'equalp))
  (values))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (pushnew :common-lisp-controller *features*))


(defvar *clc-send-command-filename* "/usr/bin/clc-send-command"
 "Filename of clc-send-command executable")

(defun ensure-lower (obj)
  (etypecase obj
    (string
     (string-downcase obj))
    (symbol
     (string-downcase (symbol-name obj)))))
    
(defun make-clc-send-command-string (command package impl)
  "Function returns a string suitable to pass to the operating
system to execute the clc-send-command program"
  (format nil "~A --quiet ~A ~A ~A" 
	  *clc-send-command-filename* 
	  (ensure-lower command)
	  (ensure-lower package)
	  (ensure-lower impl)))

(unless (fboundp 'send-clc-command)
  (defun send-clc-command (command package)
    "Function to be overrided by the implementation.
Should execute:
/usr/bin/clc-send-command command package <implementation-name> --quiet

with command either :recompile or :remove"
    (cerror "Done"
	    "This implementation has not yet implemented common-lisp-controller:send-clc-command.
Please run /usr/bin/clc-send-command --quiet ~A ~A <implementation-name>
and continue.

In the case of cmucl, please check that the /etc/common-lisp/cmucl/site-init.lisp file does
NOT load /usr/share/common-lisp/source/common-lisp-controller/common-lisp-controller.lisp 
file as this will undo the custom make-clc-send-command-string function that was
installed during the core-building process"
	    command package)))
