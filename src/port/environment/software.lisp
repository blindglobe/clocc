;;; -*- Mode: Lisp -*-

;;; software.lisp --
;;;
;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defclass software ()
  ((type :initarg :type :reader software-type)
   (version :initarg :version :reader software-version)
   )
  (:documentation "The CL.ENVIRONMENT Software Class."))


;;; Known CL implementations.

(defclass generic-common-lisp-implementation (software
					      feature-tagged-type-class)
  ((type :reader common-lisp-implementation-type)
   (version :reader common-lisp-implementation-version)
   (tag :reader common-lisp-implementation-feature-tag
	:reader cl-feature-tag
	:type symbol)
   )
  (:documentation "The CL.ENVIRONMENT Common Lisp Implementation Class.")
  (:default-initargs :type (lisp-implementation-type)
                     :version (lisp-implementation-version)))


;;; Tags for CL implementations
;;; CMUCL	:cmucl
;;; ACL		:allegro
;;; CLISP       :clisp
;;; LW		:lispworks
;;; Corman Lisp	:corman-lisp
;;; etc etc
;;; Add them!!!

(defclass cmucl (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :cmu))

(defclass sbcl (cmucl)
  ()
  (:default-initargs :feature-tag :sbcl))

(defclass allegro (generic-common-lisp-implementation)
  ((case-sensitive :type boolean :initarg :case-sensitive
		   :reader software-case-sensitive)
   (characters-16bits :type boolean :initarg :characters-16bits
		      :reader software-characters-16bits))
  (:default-initargs :feature-tag :allegro))

(defclass lispworks (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :lispworks))

(defclass clisp (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :clisp))

(defclass kcl (generic-common-lisp-implementation) ())

(defclass ibcl (kcl) ())

(defclass akcl (kcl) ())

(defclass ecolisp (kcl) ())

(defclass gcl (kcl) ())

(defclass mcl (generic-common-lisp-implementation) ())

(defclass lucid (generic-common-lisp-implementation) ())

(defclass scl (generic-common-lisp-implementation) ()) ; Symbolics Genera.

(defclass corman (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :corman-lisp))

(defclass eclipse (generic-common-lisp-implementation) ())


;; The default binary directory name is the lowercase of the feature tag

(defgeneric software-binary-directory-name (software))

(defmethod software-binary-directory-name ((software software))
  (string-downcase (symbol-name (cl-feature-tag software))))


;;; end of file -- software.lisp
