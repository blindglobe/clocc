;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-
;;; File Function: define variables and functions before defsystem is loaded.

(in-package :common-lisp-controller)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *original-require-function* nil
    "Holds vendor's original require function")

  (unless *original-require-function*
    (setq *original-require-function*
	  #-(or (and :excl :allegro-v4.0) :mcl :sbcl :lispworks)
	  (symbol-function 'lisp:require)
	  #+(and :excl :allegro-v4.0)
	  (symbol-function 'cltl1:require)
	  #+:lispworks3.1
	  (symbol-function 'common-lisp::require)
	  #+:sbcl
	  (symbol-function 'cl:require)
	  #+(and :lispworks (not :lispworks3.1))
	  (symbol-function 'system::require)
	  #+(and :lispworks :lispworks3.1)
	  (symbol-function 'common-lisp::require)
	  #+(and :mcl (not :openmcl))
	  (symbol-function 'ccl:require)
	  #+:openmcl
	  (symbol-function 'cl:require))))

(defun original-require (&rest args)
  (if *original-require-function*
      (apply *original-require-function* args)
    (format t "~&;; Original require function not found~%")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'original-require))
