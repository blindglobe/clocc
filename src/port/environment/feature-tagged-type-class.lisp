;;; -*- Mode: CLtL -*-

;;; feature-tagged-type-class.lisp --
;;; A `mixin' class providing a single TAG slot.
;;;
;;; Copyright (c) 2000, 2001 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defclass feature-tagged-type-class ()
  ((tag :reader feature-tag
	:initarg :feature-tag
	:type symbol)
   )
  (:documentation "The CL.ENVIRONMENT Feature Tag Classes."))


;;; end of file -- feature-tagged-type-class.lisp --
