;;; -*- Mode: CLtL -*-

;;; env-package.lisp --
;;;
;;; Copyright (c) 2000-2002 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

;;; New version that placates Franz's Allegro kludgy case sensitivity modes.

(defpackage "CL.ENVIRONMENT" (:use "COMMON-LISP")
  (:nicknames "CL.ENV"
	      "ORG.CONS.CLOCC/MARCOXA/CL-ENVIRONMENT"
	      "cl.env")

  ;; Shadow symbols from Chapter 25 of the CLHS.
  (:shadow #:software-type
	   #:software-version
	   #:machine-type
	   #:machine-version
	   #:machine-instance
	   )
  
  ;; Basic Classes.
  (:export #:software
	   #:machine
	   #:operating-system
	   #:common-lisp-implementation
	   )

  ;; Basic Interface.
  (:export #:feature-tag

	   #:software-type
	   #:software-version
	   #:machine-type
	   #:machine-version
	   #:machine-instance

	   #:operating-system-type
	   #:operating-system-version
	   #:operating-system-feature-tag
	   #:os-type		; Abbreviation.
	   #:os-version		; Abbreviation.
	   #:os-feature-tag     ; Abbreviation.

	   #:find-operating-system-class
	   #:find-os-class	; Abbreviation.

	   #:operating-system-tag-compatible-p
	   #:os-tag-compatible-p	; Abbreviation

	   #:os-file-system-directory-separator
	   #:current-directory-pathname

	   
	   #:common-lisp-implementation-type
	   #:common-lisp-implementation-version

	   #:*common-lisp-implementation*
	   #:*cl*
	   #:*operating-system*
	   #:*os*
	   #:*machine*

	   #:version
	   #:version-case
	   )

  ;;---------------------------------------------
  ;; Exports related to known CL implementations.

  (:export #:generic-common-lisp-implementation)
  
  ;; Franz Inc. Allegro.
  (:export #:allegro)

  ;; Harlequin LispWorks.
  (:export #:lispworks)

  ;; MCL.
  (:export #:mcl)

  ;; CMUCL and SBCL.
  (:export #:cmucl #:sbcl)

  ;; CLisp.
  (:export #:clisp)

  ;; Kcl and derivatives.
  (:export #:kcl #:ibcl #:akcl #:gcl #:ecolisp)

  ;; ECLipse
  (:export #:eclipse)

  ;; Lucid
  (:export #:lucid)

  ;; Corman
  (:export #:corman)

  ;; Genera Symbolics Common Lisp
  (:export #:scl)

  ;;--------------------------------------------
  ;; Exports related to known Operating Systems.

  ;; UNIX (generic).
  (:export #:Unix)

  ;; SunOS and Solaris.
  (:export #:Sun-OS #:Solaris)

  ;; HP-UX.
  (:export #:HP-UX)

  ;; IRIX.
  (:export #:Irix)

  ;; Linux.
  (:export #:Linux)

  ;; MS-DOS and Windows.
  (:export #:MS-DOS
	   #:MS-Windows
	   #:MS-Windows-32
	   #:MS-Windows-95
	   #:MS-Windows-98
	   #:MS-Windows-ME
	   #:MS-Windows-NT
	   #:MS-Windows-NT-TSE
	   #:MS-Windows-2000
	   #:MS-Windows-XP
	   )

  ;; Mac.
  (:export #:Mac-OS #:Mac-OS-X)

  ;; Genera.
  (:export #:Genera)

  ;; Amiga.
  (:export #:Amiga)

  ;;--------------------------------------------
  ;; Exports related to known Machine Architectures.
  (:export #:intel-x86-machine
	   #:sparc-machine
	   #:ppc-machine
	   #:mips-machine
	   #:alpha-machine)

  ;;--------------------------------------------
  ;; Across the board utilities.
  (:export #:compiled-file-extension
	   #:file-system-directory-separator
	   #:current-working-directory
	   #:cwd
	   
	   #:system-info)

  )

;;; end of file -- env-package.lisp
