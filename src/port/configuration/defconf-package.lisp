;;; -*- Mode: CLtL -*-

;;; defconf-package.lisp --
;;; A 'configure' for Common Lisp.

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).
;;; The Package definition.
;;;
;;; Notes:
;;; 20000203 Marco Antoniotti
;;; I decided to make this package dependent on the CL.ENVIRONMENT
;;; package.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (find-package "CL.ENVIRONMENT")
    (error "CL.EXT.CONFIGURATION requires the CL.ENVIRONMENT package.")))

(defpackage "CL.EXT.CONFIGURATION" (:use "COMMON-LISP")
  (:nicknames "CONF")
  (:shadow "CONFIGURE" "SETUP")		; This 'shadow' clause is a
					; little paranoid. Better safe
					; than sorry.
  (:shadow "FIND-SYSTEM" "LOAD-SYSTEM")	; Again, a safety measure.

  (:export "DEFCONFIGURATION"
	   "CONFIGURATION"
	   "SETUP"
	   "CONFIGURE-FORMAT"
	   )

  (:export "PARSE-CONF-CLAUSE"
	   "BUILD-CONF-CLAUSE"
	   )
  )

;;; end of file -- defconf-package.lisp --
