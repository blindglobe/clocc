;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-
(in-package :common-lisp-controller)

(eval-when (:load-toplevel :execute :compile-toplevel)
  (unless (find-package :mk)
    (error "You need to load the mk defsystem before loading or compiling this file!")))

;; we need to hack the require to
;; call clc-send-command on load failure...
(defun clc-require (module-name &optional pathname definition-pname
				default-action (version mk::*version*))
  (if (and (not (or pathname
                    definition-pname
                    default-action ))
           ;; no advanced stuff
           ;; is the system in the clc root?
           (ignore-errors
             ;; probing is for weenies: just do
             ;; it and ignore the errors :-)
             (equalp
              (pathname-host
               (make::component-source-root-dir
                (mk:find-system module-name
                                :load-or-nil)))
              ;; the clc root:
              (pathname-host
               (pathname
                "cl-library:")))))
      ;; if in the clc root:
      (or
       ;; try to load it
       (mk:oos  module-name
                :load
                :load-source-instead-of-binary nil
                :load-source-if-no-binary nil
                :bother-user-if-no-binary nil
                :compile-during-load nil)
       ;; if not: try to compile it
       (progn
         (format t "~&;;;Please wait, recompiling library...")
         (common-lisp-controller:send-clc-command :recompile
                                                  (if (stringp module-name)
                                                      module-name
                                                      (string-downcase
                                                       (symbol-name
                                                        module-name))))
         (terpi)
         (mk:oos  module-name
                  :load
                  :load-source-instead-of-binary nil
                  :load-source-if-no-binary nil
                  :bother-user-if-no-binary nil
                  :compile-during-load nil)
         t)
       ;; otherwise fail with a meaningful message:
       (error "I could not load the common-lisp-controller package ~A, please report a bug to the debian BTS"
              module-name))
      ;; ifnot, let mk deal with it..
      (mk::new-require module-name
                       pathname
                       definition-pname
                       default-action
                       version)))

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
