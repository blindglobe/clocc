;;; -*- Mode: CLtL -*-

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
	:type keyword)
   )
  (:documentation "The CL.ENVIRONMENT Common Lisp Implementation Class.")
  (:default-initargs :type (lisp-implementation-type)
                     :version (lisp-implementation-version)))


;;; Tags for CL implementations
;;; CMUCL	:cmucl
;;; ACL		:acl
;;; LW		:lispworks
;;; Corman Lisp	:corman-lisp
;;; etc etc
;;; Adde them!!!

(defclass cmucl (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :cmucl))

(defclass sbcl (cmucl)
  ()
  (:default-initargs :feature-tag :sbcl))

(defclass allegro (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :acl))

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


;;; end of file -- software.lisp
