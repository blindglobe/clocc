; f2cl0.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; edit here so that the root of the f2cl directory tree is appropriate
; for your installation and the filename extension is valid
#+(or sparc sun4)(defvar *f2cl_dir* "/d/sen/f2cl/wrk/")
#+vms (defvar *f2cl_dir* "d:[sen.f2cl.wrk]")
(defvar *ext* 
          #+clisp ".fas" #+allegro ".fasl" #+vms ".fas" #+lucid ".sbin")

(defun load-f2cl (x) 
(load (concatenate 'string *f2cl_dir* x *ext*) :print nil :verbose t))

(load-f2cl "f2cl1" )
(load-f2cl "f2cl2" )
(load-f2cl "f2cl3" )
(load-f2cl "f2cl4" )
(load-f2cl "f2cl5" )
(load-f2cl "f2cl6" )
(load-f2cl "f2cl7" )
(load-f2cl "macros" )

(format t "~&The f2cl software has been loaded.~%")
;-------------------------------------------------------------------------
; end of f2cl0.l
