;;; -*- Mode: CLtL -*-

;;; pcl-defsys.lisp --
;;; PCL defsys dependencies.

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (find-package "PCL")
    (error "CL.EXT.CONFIGURATION: the PCL package is required.")))

;;; DEFSYSTEM utilities
;;; PCL defsys unfortunatedly seems to be defined in the "USER" package.

(defmethod find-system ((sys symbol)
			(cl cl.env:generic-common-lisp-implementation)
			(defsys-tag (eql :pcl)))
  (user:get-system sys))

(defmethod load-system ((sys symbol)
			(cl cl.env:generic-common-lisp-implementation)
			(defsys-tag (eql :pcl))
			&rest
			keys
			&key
			arguments
			print-only)
  (declare (ignore keys))
  (user:operate-on-system sys :load arguments print-only))


;;; end of file -- pcl-defsys.lisp --
