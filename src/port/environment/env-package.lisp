;;; -*- Mode: CLtL -*-

;;; env-package.lisp --
;;;
;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(defpackage "CL.ENVIRONMENT" (:use "COMMON-LISP")
  (:nicknames "CL.ENV"
	      "ORG.CONS.CLOCC/MARCOXA/CL-ENVIRONMENT")

  ;; Shadow symbols from Chapter 25 of the CLHS.
  (:shadow "SOFTWARE-TYPE"
	   "SOFTWARE-VERSION"
	   "MACHINE-TYPE"
	   "MACHINE-VERSION"
	   "MACHINE-INSTANCE"
	   )
  
  ;; Basic Classes.
  (:export "SOFTWARE"
	   "MACHINE"
	   "OPERATING-SYSTEM"
	   "COMMON-LISP-IMPLEMENTATION"
	   )

  ;; Basic Interface.
  (:export "SOFTWARE-TYPE"
	   "SOFTWARE-VERSION"
	   "MACHINE-TYPE"
	   "MACHINE-VERSION"
	   "MACHINE-INSTANCE"

	   "OPERATING-SYSTEM-TYPE"
	   "OPERATING-SYSTEM-VERSION"
	   "OPERATING-SYSTEM-FEATURE-TAG"
	   "OS-TYPE"		; Abbreviation.
	   "OS-VERSION"		; Abbreviation.
	   "OS-FEATURE-TAG"     ; Abbreviation.

	   "COMMON-LISP-IMPLEMENTATION-TYPE"
	   "COMMON-LISP-IMPLEMENTATION-VERSION"

	   "*COMMON-LISP-IMPLEMENTATION*"
	   "*OPERATING-SYSTEM*"
	   "*MACHINE*"

	   "VERSION"
	   "VERSION-CASE"

	   "SYSTEM-INFO"
	   )

  ;;---------------------------------------------
  ;; Exports related to known CL implementations.

  (:export "GENERIC-COMMON-LISP-IMPLEMENTATION")
  
  ;; Franz Inc. Allegro.
  (:export "ALLEGRO")

  ;; Harlequin LispWorks.
  (:export "LISPWORKS")

  ;; MCL.
  (:export "MCL")

  ;; CMUCL and SBCL.
  (:export "CMUCL" "SBCL")

  ;; CLisp.
  (:export "CLISP")

  ;; Kcl and derivatives.
  (:export "KCL" "IBCL" "AKCL" "GCL" "ECOLISP")

  ;; ECLipse
  (:export "ECLIPSE")

  ;; Lucid
  (:export "LUCID")

  ;; Corman
  (:export "CORMAN")

  ;; Genera Symbolics Common Lisp
  (:export "SCL")

  ;;--------------------------------------------
  ;; Exports related to known Operating Systems.

  ;; UNIX (generic).
  (:export "UNIX")

  ;; SunOS and Solaris.
  (:export "SUNOS" "SOLARIS")

  ;; HP-UX.
  (:export "HP-UX")

  ;; IRIX.
  (:export "IRIX")

  ;; Linux.
  (:export "LINUX")

  ;; MS-DOS and Windows.
  (:export "MS-DOS"
	   "WINDOWS"
	   "W32"
	   "WINDOWS-95"
	   "WINDOWS-98"
	   "WNT"
	   "WNT-TSE"
	   "WINDOWS-2000"
	   )

  ;; Mac.
  (:export "MACOS")

  ;; Genera.
  (:export "GENERA")
  )
	   


;;; end of file -- env-package.lisp
