;;; -*- Mode: CLtL -*-

;;; machine.lisp --
;;; This is provided mostly for symmetry.  No special treatment for
;;; this class is really provided.
;;;
;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defclass machine ()
  ((type :initarg :type :reader machine-type)
   (version :initarg :version :reader machine-version)
   (instance :initarg :instance :reader machine-instance)
   )
  (:documentation "The CL.ENVIRONMENT Machine Class.")
  (:default-initargs :type (common-lisp:machine-type)
                     :version (common-lisp:machine-version)
		     :instance (common-lisp:machine-instance)))


;;; end of file -- software.lisp
