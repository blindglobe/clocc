;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; defsystem-pkg.lisp --
;;; Package definition and other package related settings for
;;; MK:DEFSYSTEM.

;;;===========================================================================
;;; Prerequisites

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (find-package "CL.ENV")
    (error "MK:DEFSYSTEM requires the package CL.ENVIRONMENT to run.~@
            You can find it in the CLOCC at http://www.sourceforge.net."))

  (unless (find-package "CL.EXT.FILESYSTEM")
    (error "MK:DEFSYSTEM requires the package CL.EXT.FILESYSTEM to run.~@
            It should have been distributed with MK:DEFSYSTEM (check ~@
            file named `adjoin-dirs'.)"))
  )


(pushnew :mk-defsystem *features*)


;;;===========================================================================
;;; Package definition.

;;; Unfortunately, lots of lisps have their own defsystems, some more
;;; primitive than others, all uncompatible, and all in the DEFSYSTEM
;;; package. To avoid name conflicts, we've decided to name this the
;;; MAKE package. A nice side-effect is that the short nickname
;;; MK is my initials.

;;; NOTE:  providing "MAKE" should be done at the end of the loading
;;; operation.

(defpackage "MAKE-4" (:use "COMMON-LISP")
  (:nicknames "MK4")
  ;; (:nicknames "MK" "make")           ; Commented for the time being.
  ;; (:nicknames "MA")			; Ok. I could do this, but it
					; would make the system messier!
					; 19991211 Marco Antoniotti

  (:import-from "CL.EXT.FILESYSTEM" "ADJOIN-DIRECTORIES")
  
  (:export "AFS-BINARY-DIRECTORY"	; Do I still need these?
	   "AFS-SOURCE-DIRECTORY")

  (:export "DEFSYSTEM"
	   "FIND-SYSTEM"
	   "COMPILE-SYSTEM"
	   "LOAD-SYSTEM"
	   "UNDEFSYSTEM"
	   "DEFINED-SYSTEMS"
	   "DESCRIBE-SYSTEM"
	   "CLEAN-SYSTEM"
	   "EDIT-SYSTEM"
	   "HARDCOPY-SYSTEM"
	   "OPERATE-ON-SYSTEM"
	   "OOS"
	   "FILES-IN-SYSTEM"
	   "GET-COMPONENT"
	   "EXECUTE-ACTION"
  	   )

  (:export "*CENTRAL-REGISTRY*"
	   "*BIN-SUBDIR*"
	   "MACHINE-TYPE-TRANSLATION"
	   "SOFTWARE-TYPE-TRANSLATION"
	   "COMPILER-TYPE-TRANSLATION"
	   ;; "REQUIRE"
	   "ALLEGRO-MAKE-SYSTEM-FASL"
	   "FILES-WHICH-NEED-COMPILATION"
	   "SYSTEM-SOURCE-SIZE" "MAKE-SYSTEM-TAG-TABLE"
	   "*DEFSYSTEM-VERSION*"
	   "*COMPILE-DURING-LOAD*"
	   "*MINIMAL-LOAD*"
	   "*DONT-REDEFINE-REQUIRE*"
	   "*FILES-MISSING-IS-AN-ERROR*"
	   "*RELOAD-SYSTEMS-FROM-DISK*"
	   "*SOURCE-PATHNAME-DEFAULT*"
	   "*BINARY-PATHNAME-DEFAULT*"
	   "*MULTIPLE-LISP-SUPPORT*"
	   )

  (:export "DEFINE-LANGUAGE"
	   "SAVE-WORKING-IMAGE")
  )

;;;---------------------------------------------------------------------------
;;; Extra package related settings.


;;; end of file -- defsystem-pkg.lisp --
