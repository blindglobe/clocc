;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-

(in-package :cl-user)

(defpackage "COMMON-LISP-CONTROLLER"
    (:use "COMMON-LISP")
    (:export "INIT-COMMON-LISP-CONTROLLER"
             "ADD-PROJECT-DIRECTORY"
             "ADD-TRANSLATION"
             "SEND-CLC-COMMAND")
    (:nicknames "C-L-C"))

(in-package :common-lisp-controller)

;; Some general utilities to make the
;; descriptions shorter

(defvar *source-extentions* (list "CL" "LISP"
                                  "L" "LSP"
                                  "C" "H"
             			  #+openmcl "cl" #+openmcl "lisp"
                                  #+openmcl "l" #+openmcl "lsp"
                                  #+openmcl "c" #+openmcl "h"))

(defvar *fasl-type*
  (load-time-value
   (pathname-type
    (compile-file-pathname "foo.lisp")
    :case :common))
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
cl-library:;hemlock;**;*.x86f.* -> /home/pvaneynd/junk-pile/hemlock/**/*.x86f

This function returns nothing."
  (let ((lp-host (ecase for
                   (:cl-library
                    "CL-LIBRARY")
                   (:cl-systems
                    "CL-SYSTEMS")))
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
           (make-pathname :defaults new-part
                          :host lp-host
                          :case :common))
          ;; construct the destination, based on all this
          (new-dest
           (make-pathname :defaults new-part
                          ;; but under new-root
                          :directory (append (pathname-directory new-root
                                                                 :case :common)
                                             ;; skip the relative
                                             (rest (pathname-directory new-part
                                                                       :case :common)))
                          :case :common)))
      (push (list new-source
                  new-dest)
            (logical-pathname-translations lp-host)))
    ;; also support the old way
    (let ((new-source
           ;; construct based on new-part but in the right logical pathname
           (make-pathname :defaults new-part
                          :directory (cons :absolute
                                           (rest
                                            (pathname-directory new-part
                                                                :case :common)))
                          :case :common
                          :host lp-host))
          ;; construct the destination, based on all this
          (new-dest
           (make-pathname :defaults new-part
                          ;; but under new-root
                          :directory (append (pathname-directory new-root
                                                                 :case :common)
                                             ;; skip the relative
                                             (rest (pathname-directory new-part
                                                                       :case :common)))
                          :case :common)))
      (push (list new-source
                  new-dest)
            (logical-pathname-translations lp-host)))
    (values)))


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
  (let* ((fasl-root (make-pathname :name nil :type nil :version nil
                                   :defaults (pathname fasl-root)))
         (s-root (pathname source-root))
         (source-root (make-pathname
                       :defaults s-root
                       :case :common
                       :directory (append (pathname-directory s-root
                                                              :case :common)
                                          '("SOURCE"))))
         (system-root (make-pathname
                       :defaults s-root
                       :case :common
                       :directory (append (pathname-directory s-root
                                                              :case :common)
                                          '("SYSTEMS")))))
    (setf (logical-pathname-translations "CL-LIBRARY")
          nil)
    (setf (logical-pathname-translations "CL-SYSTEMS")
          nil)
        ;;; by default everything is in the fasl tree...
    (setf (logical-pathname-translations "CL-LIBRARY")
          (list
           (list (make-pathname :directory '(:relative :wild-inferiors)
                                :host (pathname-host (logical-pathname "CL-LIBRARY:"))
                                :case :common)
                 ;; ;**;*.*.*
                 ;; to
                 (make-pathname :directory (append (pathname-directory fasl-root)
                                                   (list :wild-inferiors))
                                :defaults fasl-root))
           (list (make-pathname :directory '(:absolute :wild-inferiors)
                                :host (pathname-host (logical-pathname "CL-LIBRARY:"))
                                :case :common)
                 ;; ;**;*.*.*
                 ;; to
                 (make-pathname :directory (append (pathname-directory fasl-root)
                                                   (list :wild-inferiors))
                                :defaults fasl-root))))
    ;;; add common source extentions:
    (loop for extention in *source-extentions*
          do
          (add-translation :cl-library
                           source-root
                           (make-pathname :directory '(:relative :wild-inferiors)
                                          :type extention
                                          :case :common)))
    ;; now cl-systems:
    ;; by default everything is in the fasl tree...
    (setf (logical-pathname-translations "CL-SYSTEMS")
         (list
           (list (make-pathname :directory '(:relative :wild-inferiors)
                                :host (pathname-host (logical-pathname "CL-SYSTEMS:"))
                                :type "SYSTEM"
                                :case :common)
                 ;; ;**;*.*.*
                 ;; to
                 (make-pathname :directory (append (pathname-directory system-root
                                                                       :case :common)
                                                   (list :wild-inferiors))
                                :type "SYSTEM"
                                :case :common
                                :defaults system-root))
           (list (make-pathname :directory '(:absolute :wild-inferiors)
                                :host (pathname-host (logical-pathname "CL-SYSTEMS:"))
                                :type "SYSTEM"
                                :case :common)
                 ;; ;**;*.*.*
                 ;; to
                 (make-pathname :directory (append (pathname-directory system-root
                                                                       :case :common)
                                                   (list :wild-inferiors))
                                :type "SYSTEM"
                                :case :common
                                :defaults system-root))))
    (when (>= version 3)
      (flet ((compile-and-load (file)
               ;; first make the target directory:
               (ensure-directories-exist 
                (make-pathname 
                 :directory
                 (pathname-directory    
                  (translate-logical-pathname 
                   (compile-file-pathname file)))))
               ;; now compile it:
               (compile-file file
                             :print nil
                             :verbose nil)
               ;; then load it:
               (load (compile-file-pathname file))))
        ;; first ourselves:
        (compile-and-load  "cl-library:;common-lisp-controller;common-lisp-controller.lisp")
        ;; then defsystem:
        (compile-and-load  "cl-library:;defsystem;defsystem.lisp")
        ;; then the patches:
        (compile-and-load  "cl-library:;common-lisp-controller;common-lisp-controller-post-defsystem.lisp")
        ;; register ourselves:
        (push "cl-systems:"
              (symbol-value (intern "*CENTRAL-REGISTRY*"
                                    (find-package :make))))))
    (values)))

(defun add-project-directory (source-root fasl-root projects &optional system-directory)
  "This registers a SOURCE-ROOT and FASL-ROOT translation for the subdirectory
project for all PROJECTS.
Optionally you can also register SYSTEM-DIRECTORY.
Returns nothing"
  (declare (type pathname source-root fasl-root system-directory))
  (loop for project in projects
        do
        (let ((project (string-upcase project)))
          (add-translation
           :cl-library fasl-root
           (make-pathname :directory (list :relative project :wild-inferiors)
                          :type :wild
                          :case :common))
          (loop for extention in *source-extentions* do
                (add-translation
                 :cl-library source-root
                 (make-pathname :directory (list :relative project :wild-inferiors)
                                :type extention
                                :case :common)))))
  (when system-directory
    (pushnew  system-directory
              (symbol-value
               (intern "*CENTRAL-REGISTRY*"
                       (find-package :make)))
              :test #'equalp))
  (values))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (pushnew :common-lisp-controller *features*))

