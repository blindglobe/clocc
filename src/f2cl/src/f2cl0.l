; f2cl0.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; edit here so that the root of the f2cl directory tree is appropriate
; for your installation and the filename extension is valid
;;#+(or sparc sun4)(defvar *f2cl_dir* "/d/sen/f2cl/wrk/")
;;#+vms (defvar *f2cl_dir* "d:[sen.f2cl.wrk]")
;;(defvar *ext* 
;;          #+clisp ".fas" #+allegro ".fasl" #+vms ".fas" #+lucid ".sbin")
;;
;;(defun load-f2cl (x) 
;;(load (concatenate 'string *f2cl_dir* x *ext*) :print nil :verbose t))
;;
;;(load-f2cl "f2cl1" )
;;(load-f2cl "f2cl2" )
;;(load-f2cl "f2cl3" )
;;(load-f2cl "f2cl4" )
;;(load-f2cl "f2cl5" )
;;(load-f2cl "f2cl6" )
;;(load-f2cl "f2cl7" )
;;(load-f2cl "macros" )
;;
;;(format t "~&The f2cl software has been loaded.~%")

(defpackage "FORTRAN-TO-LISP"
    (:use "CL")
  (:documentation "The package holding all symbols need by the Fortran to Lisp converter")
  (:nicknames "F2CL")
  (:export
   ;; Main routines
   "F2CL"
   ;; Constants
   "%FALSE%" "%TRUE%"
   ;; Types
   "INTEGER4" "INTEGER2" "INTEGER1" "REAL8" "REAL4" "COMPLEX8" "COMPLEX16"
   "ARRAY-DOUBLE-FLOAT" "ARRAY-SINGLE-FLOAT" "ARRAY-INTEGER4" "LOGICAL"
   ;; Macros
   "FREF" "FSET"
   "F2CL-INIT-STRING" "FREF-STRING" "FSET-STRING" "F2CL-SET-STRING"
   "F2CL-//" "FSTRING-/=" "FSTRING-=" "FSTRING->" "FSTRING->=" "FSTRING-<" "FSTRING-<="
   "FORTRAN_COMMENT" "FDO" "F2CL/" "ARITHMETIC-IF" "COMPUTED-GOTO"
   "ASSIGNED-GOTO"
   "FFORMAT"
   "DATA-IMPLIED-DO"
   ;; Utilities
   "ARRAY-SLICE" "ARRAY-INITIALIZE"
   ;; Intrinsic functions
   "ABS" "ACOS" "AIMAG" "AINT" "ALOG" "ALOG10" "AMAX0" "AMAX1"
   "AMAX1" "AMIN1" "AMOD" "ANINT" "ASIN" "ATAN" "ATAN2"
   "CABS" "CEXP" "FCHAR" "CLOG" "CMPLX" "CONJG" "CCOS"
   "CSIN" "CSQRT" "DABS" "DACOS" "DASIN"
   "DATAN" "DATAN2" "DBLE" "DCOS" "DCOSH" "DEXP" "DIM"
   "DIM" "DINT" "DLOG" "DLOG10" "DMAX1" "DMIN1" "DMOD"
   "DNINT" "DPROD" "DSIGN" "DSIN" "DSINH" "DSQRT" "DTAN"
   "DTANH" "FFLOAT" "IABS" "ICHAR" "IDIM" "IDINT"
   "IDNINT" "IFIX" "INDEX" "INT" "ISIGN" "LE" "LEN"
   "LGE" "LGT" "FLOG" "LOG10" "LT" "MAX" "MAX0"
   "MAX1" "MIN0" "MIN1" "NINT" "FREAL"
   "SIGN" "SNGL" "FSQRT"
   ))

;;;-------------------------------------------------------------------------
;;; end of f2cl0.l
;;;
;;; $Id: f2cl0.l,v 1.4 2000/07/28 22:07:44 rtoy Exp $
;;; $Log: f2cl0.l,v $
;;; Revision 1.4  2000/07/28 22:07:44  rtoy
;;; It's FORTRAN, not FORTAN!
;;;
;;; Revision 1.3  2000/07/28 16:56:48  rtoy
;;; f2cl0.l isn't the (unused) f2cl loader anymore.  Use it to define the
;;; package used by f2cl.
;;;
;;; Revision 1.2  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-------------------------------------------------------------------------
