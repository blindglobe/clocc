; macros.l - all the basic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Hamilton, New Zeland 1992-95 - all rights reserved;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :f2cl)
#+nil
(eval-when (compile load eval)
  (export '(do1 do! map-defvar double-cdr double-mapcar push 
	    fexport fproclaim fuse-package fin-package fdo arithmetic-if
	    computed-goto assigned-goto
	    fref fset f2cl/
	    int ifix idint real ffloat sngl dble cmplx ichar char aint dint
	    anint dnint nint idnint iabs abs dabs cabs mod amod dmod isign sign dsign
	    idim dim ddim dprod max max0 amax1 dmax1 amax0 amax1 min min0 amin1 dmin1
	    min1 len index lge lgt lle llt aimag conjg sqrt dsqrt fsqrt csqrt 
	    exp dexp cexp log alog dlog flog clog log10 alog10 dlog10 sin dsin csin
	    cos dcos ccos tan dtan asin dasin acos dacos atan datan atan2 datan2
	    sinh dsinh cosh dcosh tanh dtanh
	    data-implied-do
	    %false% %true%
	    fformat)))

; macros:
;	rexpt
;	fexport
;	fproclaim
;	fuse-package 
;	fin-package
;	map-defvar
;	do1 
;	do!
;	double-cdr
;	putproperty
;	defprop
;	array-cl
;	store-cl
;	apply!

;	rfref
;	rfset
;	fref
;	fset

;	while
;       fdo
;	reset-vble - a defun
;       arithmetic-if
;	computed-goto
;	assigned-goto
;	eqv
;	constant-list
;       Fortran intrinsic functions imax, dabs,...
;----------------------------------------------------------------------------

(eval-when (compile load eval) (proclaim '(special *verbose*)))
;----------------------------------------------------------------------------
#+aclpc (defmacro rexpt (x y) `(realpart (expt ,x ,y)))
#-aclpc (defmacro rexpt (x y) `(expt ,x ,y))

(deftype logical ()
  `(member t nil))

;; Decide what you want integer*4 to be.  Good choices are fixnum or
;; (signed-byte 32).  The latter is good only if your compiler does a
;; good job with this type.  If you aren't sure, use fixnum.  CMUCL
;; does a good job with (signed-byte 32).
;;
;; If you change this, you may need to change some of the macros
;; below, such as INT and AINT!

#+cmu
(deftype integer4 ()
  `(signed-byte 32))
#-cmu
(deftype integer4 ()
  'fixnum)

(deftype integer2 ()
  `(signed-byte 16))
(deftype integer1 ()
  `(signed-byte 8))
(deftype real8 ()
  'double-float)
(deftype real4 ()
  'single-float)
(deftype complex8 ()
  `(complex single-float))
(deftype complex16 ()
  `(complex double-float))

(deftype array-double-float ()
    `(array double-float (*)))
(deftype array-integer4 ()
    `(array integer4 (*)))
(deftype array-single-float ()
    `(array single-float (*)))

(defconstant %false% nil)
(defconstant %true% t)

(defmacro fexport (x) `(eval-when (compile load eval) (export ,x)))

(defmacro fproclaim (x) `(eval-when (compile load eval) (proclaim ,x)))

(defmacro fin-package (x)
  `(prog nil 
     (defpackage ,x)
     (in-package ,x)))

(defmacro fuse-package (x) `(eval-when (compile load eval) ,x))
;-------------------------------------------------------------------------

(defmacro apply! (fun args) (eval `(cons ,fun ,args)))

;-----------------------------------------------------------------------------

(defmacro do! (var init step end &rest body)
   `(do ((,var ,init ,step)) (,end) ,@body))

; the body is an unquoted list of the terms of the actual body
(defmacro do1 (var end body)
   `(do ((,var 1 (1+ i))) ((> ,var ,end)) ,@body))

(defmacro double-cdr (lis)
   `(mapcar #'cdr (cdr ,lis)))

(defun putproperty (a b c) (setf (get a c) b))

(defmacro defprop (sym prop ind)
  `(putproperty ',sym ',prop ',ind))

(defmacro def (name body) `(defun ,name ,(cadr body) ,(caddr body)))

(defmacro array-cl (name type &rest dims)
 `(set ',name 
        (make-array ',(mapcar #'eval dims) 
           :element-type ,(cond ((equal type 'fixnum-block) ''integer)
                                ((equal type 'flonum-block)  ''flonum) ;###
                                ((equal type t) t)))))

(defmacro store-cl (name-indices val)
  `(setf (aref ,(car name-indices) ,@(cdr name-indices)) ,val))
;-----------------------------------------------------------------------------

#+nil
(defmacro fref (arr indices &optional low-bnds)
  (if low-bnds
      `(aref ,arr ,@(mapcar #'(lambda (k lim)
				`(- ,k ,lim))
			    indices low-bnds))
      `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) indices))))

;; Array dimensions are (d1, d2, d3, ...)
;;
;; Then x(n1, n2, n3, ...) means index is
;;
;; n1 + d1*(n2 + d2*(n3 + d3*(n4 + d4*(n5))))

;; Return an expression that computes the column major index given the
;; indices and the bounds on each dimension.  The bounds are a list of
;; the upper and lower bounds for each dimension.
(defun col-major-index (indices dims)
  (flet ((get-offset (n bound)
	   (let ((lo (first bound)))
	     (if (and (numberp lo) (zerop lo))
		 n
		 `(- ,n ,lo))))
	 (get-size (bound)
	   (destructuring-bind (lo hi)
	       bound
	     (cond ((numberp lo)
		    (cond ((numberp hi)
			   (1+ (- hi lo)))
			  ((= lo 1)
			   hi)
			  (t
			   `(- ,hi ,(- lo 1)))))
		   (t
		    `(- ,hi (- ,lo 1)))))))
    (let* ((rev-idx (reverse indices))
	   (rev-dim (reverse dims))
	   (idx (get-offset (first rev-idx) (first rev-dim))))
      (do ((d (rest rev-dim) (rest d))
	   (n (rest rev-idx) (rest n)))
	  ((endp d)
	   idx)
	(setf idx `(+ ,(get-offset (first n) (first d))
		    (* ,(get-size (first d)) ,idx)))))))

(defun check-array-bounds (indices bounds)
  `(and ,@(mapcar #'(lambda (idx dim)
		     `(<= ,(first dim) ,idx ,(second dim)))
		 indices bounds)))

(defmacro fref (arr indices bounds)
  (if *check-array-bounds*
      `(aref ,arr (if ,(check-array-bounds indices bounds)
		      ,(col-major-index indices bounds)
		      (error "Out of bounds index for array ~S"
			     ',arr)))
      `(aref ,arr ,(col-major-index indices bounds))))

(defmacro fset (a b) 
  `(setf (fref ,(second a) ,@(cddr a)) ,b))

(defmacro fref-string (s range)
  `(subseq ,s (1- ,(first range)) ,(second range)))

(defmacro fset-string (a b)
  `(setf (fref-string ,(second a) ,(third a)) ,b))

(defmacro f2cl-// (a b)
  `(concatenate 'string ,a ,b))

;; Create an array slice for the array named VNAME whose elements are
;; of type TYPE.  The slice starts at the indices INDICES and the
;; original array has dimensions given by BOUND.
;;
;; This is done by making a displaced array to VNAME with the
;; appropriate offset.
(defmacro array-slice (vname type indices bounds)
  (let ((dims `(* ,@(mapcar #'(lambda (idx bnd)
				(if (and (numberp idx)
					 (numberp (second bnd)))
				    (+ (- (second bnd) idx) 1)
				    `(+ (- ,(second bnd) ,idx) 1)))
			    indices bounds))))
    `(make-array ,dims
      :element-type ',type
      :displaced-to ,vname
      :displaced-index-offset ,(col-major-index indices bounds))))  

;; Compute an initializer for make-array given the data in the list
;; DATA.  The array has en element type of TYPE and has dimensions of
;; DIMS.
(defmacro array-initialize (type dims data)
  (let ((data-list (gensym))
	(data-len (length data))
	(total-length (gensym)))
    `(let* ((,data-list ',data)
	    (,total-length (reduce #'* (list ,@dims))))
       (cond ((< ,data-len ,total-length)
	      ;; Need to append some data.
	      (append ,data-list (make-list (- ,total-length ,data-len)
					    :initial-element (coerce 0 ',type))))
	     ((> ,data-len ,total-length)
	      ;; Need to truncate some data
	      (subseq ,data-list 0 ,total-length))
	     (t
	      ,data-list)))))  

;----------------------------------------------------------------------------

#-aclpc (defmacro while (con &rest body)
            `(loop (if (not ,con) (return t)) ,@body))
;------------------------------------------------------------------

(defmacro fortran_comment (&rest args)
  (declare (ignore args)))

;----------------------------------------------------------------------------
(defun reset-vble (pred-clause) ; ((> i 4) nil)
   `(,(first pred-clause)
          (setq ,(second (first pred-clause)) ,(third (first pred-clause)))
          ,(second pred-clause)))

; fdo has similar syntax as do except there will only be one do_vble

(defmacro fdo (do_vble_clause predicate_clause &rest body)
  (let ((step (gensym))
	(iteration_count (gensym)))
    `(prog* ((,step ,(third (third do_vble_clause)))
	     (,iteration_count 
	      (max 0 (truncate (+ (- ,(third (first predicate_clause))
				     ,(second do_vble_clause))
				  ,step)
			       ,step)
		   )))
      (declare (type integer4 ,step ,iteration_count))
      ;; initialise loop variable
      (setq ,(first do_vble_clause) ,(second do_vble_clause))
      loop
      (return
	(cond				; all iterations done
	  ((zerop ,iteration_count) nil)
	  ;; execute loop, in/de-crement loop vble and decrement cntr
	  ,(cons 't 
		 (append 
		  (append body
			  `((setq ,(first do_vble_clause) 
			     ,(third do_vble_clause)
			     ,iteration_count 
			     (1- ,iteration_count))))
		  '((go loop)))))))))

;(defmacro fdo (do-vbles predicate-clause &rest body)
;   `(prog nil
;          (setq ,(caar do-vbles) ,(cadar do-vbles)) 
;          loop
;          (return
;          (cond ,(reset-vble predicate-clause)
;                ,(cons 't 
;                       (append 
;                        (append body `((setq ,(caar do-vbles) ,(caddar do-vbles))))
;                        '((go loop))))))))
;(defmacro fdo (do-vbles predicate-clause &rest body)
;   `(prog (iteration-count)
;          ,(append '(psetq) 
;                   (do ((do-vars do-vbles (cdr do-vars))
;                        (ret nil (append ret (list (caar do-vars) (cadar do-vars)))))
;                       ((null do-vars) ret)))
;          loop
;          (return
;          (cond ,predicate-clause
;                ,(cons 't 
;                       (append 
;                        (append body
;                                (list
;                                (append '(psetq)
;                                (do ((do-vars do-vbles (cdr do-vars))
;                                     (ret nil (append ret (if (null (caddar do-vars)) 
;                                                              nil 
;                                                              (list (caar do-vars) 
;                                                                    (caddar do-vars))))))
;                                    ((null do-vars) ret)))))
;                        '((go loop))))))))

;----------------------------------------------------------------------------
(defun constant-list (x n)
  (do  ((i 1 (1+ i)) 
        (ret nil (cons x ret))) 
       ((> i n) ret)))
     

;----------------------------------------------------------------------------
;; macro for division 

(defmacro f2cl/ (x y)
  (let ((top (gensym))
	(bot (gensym)))
    `(let ((,top ,x)
	   (,bot ,y))
      (if (and (typep ,top 'integer)
	       (typep ,bot 'integer))
	  (values (the integer4 (truncate ,top ,bot)))
	  (/ ,top ,bot)))))

;; macro for a lisp equivalent of Fortran arithmetic IFs
(defmacro arithmetic-if (pred s1 s2 s3)
  (let ((tst (gensym)))
    `(let ((,tst ,pred))
      (cond ((< ,tst 0) ,s1)
	    ((= ,tst 0) ,s2)
	    (t ,s3)))))

;; macro for a lisp equivalent of Fortran computed GOTOs
(defun computed-goto-aux (tags)
  (let ((idx 0)
	(result '()))
    (dolist (tag tags (nreverse result))
      (incf idx)
      (push `(,idx (go ,tag)) result))))

(defmacro computed-goto (tag-lst i)
  `(case ,i
    ,@(computed-goto-aux tag-lst)))

;; macro for a lisp equivalent of Fortran assigned GOTOs
(defmacro assigned-goto (i &optional tag-lst)
   `(if ,tag-lst
        (if (member ,i ,tag-lst) 
            (go ,i)
            (error "bad statement number in assigned goto"))
        (go ,i)))

;-----------------------------------------------------------------------------       
; set up a list of intrinsic function names
;real xxx
(defvar *intrinsic_function_names*
  '(int ifix idint real float sngl dble cmplx ichar char aint dint
    anint dnint nint idnint iabs abs dabs cabs mod amod dmod isign sign dsign
    idim dim ddim dprod max max0 max1 amax1 dmax1 amax0 amax1 min min0 amin1 dmin1
    min1 len index lge lgt lle llt aimag conjg sqrt dsqrt csqrt 
    exp dexp cexp log alog dlog clog log10 alog10 dlog10 sin dsin csin
    cos dcos ccos tan dtan asin dasin acos dacos atan datan atan2 datan2
    sinh dsinh cosh dcosh tanh dtanh))

(declaim (inline truncate-towards-zero ftruncate-towards-zero))

(defun truncate-towards-zero (x)
  ;; Fortran 77 standard says INT functions round towards zero:
  ;;
  ;; int(a) is the integer whose magnitude is the largest integer that
  ;; does not exceed the magnitude of a and whose sign is the same as
  ;; the sign of a.
  ;;
  (if (minusp x)
      (ceiling x)
      (floor x)))

(defun ftruncate-towards-zero (x)
  ;; Floating-point version of the above.
  (if (minusp x)
      (fceiling x)
      (ffloor x)))

(declaim (inline int ifix idfix))

;; Define the intrinsic functions.
#-cmu
(defun int (x)
  ;; We use fixnum here because f2cl thinks Fortran integers are
  ;; fixnums.  If this should change, we need to change the ranges
  ;; here as well.
  (etypecase x
    (integer
     x)
    (single-float
     (truncate-towards-zero (the (single-float #.(float most-negative-fixnum)
					       #.(float most-positive-fixnum))
			      x)))
    (double-float
     (truncate-towards-zero (the (double-float #.(float most-negative-fixnum 1d0)
					       #.(float most-positive-fixnum 1d0))
			      x)))))

#+cmu
(defun int (x)
  ;; We use fixnum here because f2cl thinks Fortran integers are
  ;; fixnums.  If this should change, we need to change the ranges
  ;; here as well.
  (etypecase x
    (integer
     x)
    (single-float
     (truncate-towards-zero (the (single-float #.(float (- (ash 1 31)))
					       #.(float (1- (ash 1 31))))
			      x)))
    (double-float
     (truncate-towards-zero (the (double-float #.(float (- (ash 1 31)) 1d0)
					       #.(float (1- (ash 1 31)) 1d0))
			      x)))))
  
(defun ifix (x)
  (int x))
(defun idfix (x)
  (int x))

(declaim (inline aint dint anint dnint nint idnint))

#-cmu
(defun aint (x)
  (etypecase x
    (single-float
     (ftruncate-towards-zero (the (single-float #.(float most-negative-fixnum)
						#.(float most-positive-fixnum))
			       x)))
    (double-float
     (ftruncate-towards-zero (the (double-float #.(float most-negative-fixnum 1d0)
						#.(float most-positive-fixnum 1d0))
			       x)))))
#+cmu
(defun aint (x)
  (etypecase x
    (single-float
     (ftruncate-towards-zero (the (single-float #.(float (- (ash 1 31)))
					     #.(float (1- (ash 1 31))))
			    x)))
    (double-float
     (ftruncate-towards-zero (the (double-float #.(float (- (ash 1 31)) 1d0)
					     #.(float (1- (ash 1 31)) 1d0))
			    x)))))

(defun dint (x)
  (aint x))

(defun anint (x)
  (coerce (round x) (type-of x)))
(defun dnint (x)
  (values (fround x)))
(defun nint (x)
  (values (round x)))
(defun idnint (x)
  (values (round x)))

(declaim (inline freal sngl dble cmplx))
(defun freal (x)
  (coerce (realpart x) 'single-float))

(defun sngl (x)
  (coerce (realpart x) 'single-float))

(defun dble (x)
  (coerce (realpart x) 'double-float))

(defun cmplx (x &optional y)
  (complex x (if y y 0)))

(defun ichar (c)
  (char-int c))
(defun fchar (i)			;intrinsic function char
  (char-int i))

(declaim (inline iabs dabs cabs amod dmod))
#-aclpc
(defun iabs (x)
  (declare (type integer4 x))
  (abs x))
(defun dabs (x)
  (declare (type double-float x))
  (abs x))
(defun cabs (x)
  (declare (type complex x))
  (abs x))

(defun amod (x y)
  (declare (type single-float x y))
  (mod x y))
(defun dmod (x y)
  (declare (type double-float x y))
  (mod x y))


(declaim (inline isign sign dsign))

(defun isign (x y)
  (declare (type integer4 x y))
  (if (>= y 0)
      (abs x)
      (- (abs x))))
(defun sign (x y)
  (declare (type single-float x y))
  (float-sign y x))
(defun dsign (x y)
  (declare (type double-float x y))
  (float-sign y x))

(declaim (inline idim dim ddim))
(defun idim (x y)
  (declare (type integer4 x y))
  (abs (- x y)))
(defun dim (x y)
  (declare (type single-float x y))
  (abs (- x y)))
(defun ddim (x y)
  (declare (type double-float x y))
  (abs (- x y)))

(defun dprod (x y)
  (coerce (* x y) `double-float))

(defun max0 (&rest x)
  (apply #'max x))
(defun amax1 (&rest x)
  (apply #'max x))
(defun dmax1 (&rest x)
  (apply #'max x))

(defun min0 (&rest x)
  (apply #'min x))
(defun amin1 (&rest x)
  (apply #'min x))
(defun dmin1 (&rest x)
  (let ((result (first x)))
    (declare (double-float result))
    (dolist (val (rest x))
      (declare (double-float val))
      (setf result (min result val)))
    result))

(defun amax0 (&rest x)
  (float (apply #'max x)))
(defun max1 (&rest x)
  (nint (apply #'max x)))
(defun amin0 (&rest x)
  (float (apply #'min x)))
(defun min1 (&rest x)
  (nint (apply #'min x)))

;; Define some compile macros for these max/min functions.
#+cmu
(progn
(define-compiler-macro max0 (&rest args)
  `(max ,@args))
(define-compiler-macro amax1 (&rest args)
  `(max ,@args))
(define-compiler-macro dmax1 (&rest args)
  `(max ,@args))
(define-compiler-macro min0 (&rest args)
  `(min ,@args))
(define-compiler-macro amin1 (&rest args)
  `(min ,@args))
(define-compiler-macro dmin1 (&rest args)
  `(min ,@args))
(define-compiler-macro min1 (&rest args)
  `(nint (min ,@args)))

(define-compiler-macro amax0 (&rest args)
  `(float (max ,@args)))
(define-compiler-macro max1 (&rest args)
  `(nint (max ,@args)))

(define-compiler-macro amin0 (&rest args)
  `(float (min ,@args)))
(define-compiler-macro min1 (&rest args)
  `(nint (min ,@args)))
) ; end progn

(defun len (s)
  (length s))

(defun index (s1 s2)
  (or (search s1 s2) 0))

;; These string operations need some work!
(defun lge (s1 s2)
  (string>= s1 s2))
(defun lgt (s1 s2)
  (string> s1 s2))
(defun lle (s1 s2)
  (string<= s1 s2))
(defun llt (s1 s2)
  (string< s1 s2))

(defun fstring-/= (s1 s2)
  (not (string= s1 s2)))
(defun fstring-= (s1 s2)
  (string= s1 s2))
(defun fstring-> (s1 s2)
  (string> s1 s2))
(defun fstring->= (s1 s2)
  (string>= s1 s2))
(defun fstring-< (s1 s2)
  (string< s1 s2))
(defun fstring-<= (s1 s2)
  (string<= s1 s2))


(declaim (inline aimag conjg))
(defun aimag (c)
  (imagpart c))
(defun conjg (c)
  (conjugate c))

(declaim (inline fsqrt flog))
(defun fsqrt (x)
  (typecase x
    (single-float
     (sqrt (the (single-float 0f0) x)))
    (double-float
     (sqrt (the (double-float 0d0) x)))
    (t
     (sqrt x))))

(defun flog (x)
  (typecase x
    (single-float
     (log (the (or (single-float (0f0)) (member 0f0)) x)))
    (double-float
     (log (the (or (double-float (0d0)) (member 0d0)) x)))
    (t
     (log x))))
  
;; Tell Lisp that the arguments always have the correct range.  If
;; this is not true, the original Fortran code was broken anyway, so
;; GIGO (garbage in, garbage out).

(declaim (inline dsqrt csqrt alog dlog clog alog10 dlog10))
(defun dsqrt (x)
  (declare (type (double-float 0d0) x))
  (sqrt  x))
(defun csqrt (x)
  (sqrt x))
(defun alog (x)
  (declare (type (or (single-float (0f0)) (member 0f0)) x))
  (log x))
(defun dlog (x)
  (declare (type (or (double-float (0d0)) (member 0d0)) x))
  (log x))
(defun clog (x)
  (log x))
(defun alog10 (x)
  (declare (type (or (single-float (0f0)) (member 0f0)) x))
  (log x 10f0))
(defun dlog10 (x)
  (declare (type (or (double-float (0d0)) (member 0d0)) x))
  (log x 10.0d0))

(declaim (inline log10))
(defun log10 (x)
  (typecase x
    (single-float
     (log (the (single-float 0.0f0) x) 10f0))
    (double-float
     (log (the (double-float 0.0d0) x) 10d0))
    (t
     (/ (log x)
	(typecase x
	  ((complex double-float)
	   10d0)
	  ((complex single-float)
	   10f0)
	  (t
	   (coerce 10 (type-of (realpart x)))))))))

(declaim (inline dexp cexp))
(defun dexp (x)
  (declare (type double-float x))
  (exp x))
(defun cexp (x)
  (declare (type complex x))
  (exp x))

(declaim (inline dsin csin dcos ccos dtan ctan dasin dacos datan atan2 datan2 dsinh dcosh dtanh))
(defun dsin (x)
  (declare (type double-float x))
  (sin x))
(defun csin (x)
  (declare (type complex x))
  (sin x))

(defun dcos (x)
  (declare (type double-float x))
  (cos x))
(defun ccos (x)
  (declare (type complex x))
  (cos x))

(defun dtan (x)
  (declare (type double-float x))
  (tan x))
(defun ctan (x)
  (declare (type complex x))
  (tan x))

(defun dasin (x)
  (declare (type double-float x))
  (asin x))
(defun dacos (x)
  (declare (type double-float x))
  (acos x))
(defun datan (x)
  (declare (type double-float x))
  (atan x))
(defun atan2 (x y)
  (declare (type double-float x))
  (atan x y))
(defun datan2 (x y)
  (declare (type double-float x y))
  (atan x y))

(defun dsinh (x)
  (declare (type double-float x))
  (sinh x))
(defun dcosh (x)
  (declare (type double-float x))
  (cosh x))
(defun dtanh (x)
  (declare (type double-float x))
  (tanh x))

(declaim (inline ffloat))
(defun ffloat (x)
  (coerce x 'single-float))

(defun process-implied-do (ido low-bnds init)
  (let* ((implied-do (remove '|,| ido))
	 (array (first implied-do))
	 (do-var (elt implied-do (1- (position '= implied-do))))
	 (limits (rest (member '= implied-do)))
	 (start (first limits))
	 (end (second limits))
	 (step (if (>= (length limits) 3)
		   (third limits)
		   1)))
    (cond ((atom array)
	   `(do ((,do-var ,start (+ ,do-var ,step)))
	     ((> ,do-var ,end))
	     (fset (fref ,array ,(remove '|,| (second implied-do)) ,low-bnds) (pop ,init))))
	  (t
	   `(do ((,do-var ,start (+ ,do-var ,step)))
	     ((> ,do-var ,end))
	     ,(process-implied-do (remove '|,| array) low-bnds init))))))


;; Process implied do loops for data statements
(defmacro data-implied-do (implied-do low-bnds vals)
  (let ((v (gensym)))
    `(let ((,v ',vals))
      ,(process-implied-do implied-do low-bnds v))))
;-----------------------------------------------------------------------------  ; end of macros.l
   
;; Map Fortran logical unit numbers to Lisp streams

(defvar *lun-hash*
  (let ((table (make-hash-table)))
    (setf (gethash 6 table) *standard-output*)
    (setf (gethash t table) *standard-output*)
    table))

(defun lun->stream (lun)
  (let ((stream (gethash lun *lun-hash*)))
    (if stream
	stream
	(setf (gethash lun *lun-hash*)
	      (open (format nil "fort~d.dat" lun)
		    :direction :output
		    :if-exists :rename)))))

(declaim (ftype (function (t) stream) lun->stream))

(defmacro fformat (dest-lun format-cilist &rest args)
  (let ((stream (gensym)))
    `(let ((,stream (lun->stream ,dest-lun)))
      ,@(expand-format stream format-cilist args))))

#+nil
(defmacro fformat1 (dest directive arg)
  (let ((val (gensym)))
    `(let ((,val ,arg))
      (if (and (arrayp ,val)
	       (not (stringp ,val)))
	  (print ,val)
	  (format ,dest ,directive ,val)))))

(defmacro fformat1 (dest directive arg)
  (let ((val (gensym)))
    `(let ((,val ,arg))
      (cond ((and (arrayp ,val)
		  (not (stringp ,val)))
	     (dotimes (k (array-total-size ,val))
	       (format ,dest ,directive (row-major-aref ,val k))
	       (terpri ,dest)))
	    ((listp ,val)
	     (dolist (item ,val)
	       (format ,dest ,directive item)
	       (terpri ,dest)))
	    (t
	     (format ,dest ,directive ,val))))))

(defun expand-format (dest cilist args)
  (if (equal cilist '("~A~%"))
      (append (mapcar #'(lambda (arg) `(fformat1 ,dest "~A " ,arg)) args)
	   `((format ,dest "~%")))

      ;loop through directives, consume arguments
      (do ((res '())
	   (directives cilist (cdr directives))
	   (arglist args arglist))
	  ((null directives)
	   (nreverse res))
	(cond ((stringp (first directives))
	       ;;(format t "~a~%" (first directives))
	       (push `(format ,dest ,(first directives))
		     res))
	      (t
	       (push `(fformat1 ,dest
		       ,(car (first directives)) 
		       ,(first arglist))
		     res)
	       (setq arglist (cdr arglist)))))))

;; Initialize a multi-dimensional array of character strings. I think
;; we need to do it this way to appease some picky compilers (like
;; CMUCL).  The initial-element is needed to get rid of a warning
;; about the default initial element not being a simple
;; string. However, this initializes all elements of the array to
;; exactly the same string, so we loop over the entire array contents
;; and initialize each element with a string of the appropriate
;; length.  The string is initialized with #\Space because it seems
;; that's what Fortran initializes it to.
(defmacro f2cl-init-string (dims len)
  (let ((init (gensym))
	(new-dims (if (every #'numberp dims)
		      `',dims
		      `(list ,@dims))))
    `(let ((,init (make-array ,new-dims
			      :element-type `(simple-array base-char (,',@len))
			      :initial-element (make-string ,@len))))
       (dotimes (k (array-total-size ,init))
	 (setf (row-major-aref ,init k)
	       (make-string ,@len :initial-element #\Space)))
       ,init)))

;; This macro is supposed to set LHS to the RHS assuming that the LHS
;; is a Fortran CHARACTER type of length LEN.
;;
;; Currently, converts the RHS to the appropriate length string and
;; assigns it to the LHS.  However, this can generate quite a bit of
;; garbage.  We might want to be a bit smarter and use loops to
;; replace the individual characters of the LHS with the appropriate
;; characters from the RHS.
(defmacro f2cl-set-string (lhs rhs (string len))
  (declare (ignore string))
  (etypecase lhs
    (symbol
     ;; Assignment to a simple string.
     `(setf ,lhs (f2cl-string ,rhs ,len)))
    (list
     ;; Assignment to an array
     `(fset ,lhs (f2cl-string ,rhs ,len)))))

(defun f2cl-string (string len)
  ;; Create a string of the desired length by either appending spaces
  ;; or truncating the string.
  (let ((slen (length string)))
    (cond ((= slen len)
	   ;; Need to make a copy of the string, so we don't have
	   ;; duplicated structure.
	   (copy-seq string))
	  ((> slen len)
	   ;; Truncate the string
	   (subseq string 0 len))
	  (t
	   ;; String is too short, so append some spaces
	   (concatenate 'string string (make-string (- len slen) :initial-element #\Space))))))
  

;;;-------------------------------------------------------------------------
;;; end of macros.l
;;;
;;; $Id: macros.l,v 1.27 2000/08/03 13:45:53 rtoy Exp $
;;; $Log: macros.l,v $
;;; Revision 1.27  2000/08/03 13:45:53  rtoy
;;; Make FFORMAT1 handle lists that we get from implied do loops.
;;;
;;; The whole FFORMAT stuff needs to be rethought if we really want to
;;; support Fortran output.
;;;
;;; Revision 1.26  2000/08/01 22:10:41  rtoy
;;; o Try to make the Fortran functions to convert to integers work the
;;;   way Fortran says they should.
;;; o Declaim most of the intrinsics as inline so we don't have an
;;;   additional function call for simple things.
;;; o Add some compiler macros for Fortran max/min functions to call the
;;;   Lisp max/min functions withouth using #'apply.
;;; o Try to declare the args to functions with branchs appropriately,
;;;   even in the face of signed zeroes.
;;;
;;; Revision 1.25  2000/07/28 22:10:05  rtoy
;;; Remove unused var from ARRAY-SLICE.
;;;
;;; Revision 1.24  2000/07/28 17:09:13  rtoy
;;; o We are in the f2cl package now.
;;; o Remove the export expression.
;;; o // is now called F2CL-//, to prevent problems with the lisp variable
;;;   //.
;;; o REAL is now called FREAL, to prevent problems with the lisp type
;;;   REAL.
;;;
;;; Revision 1.23  2000/07/27 16:39:17  rtoy
;;; We want to be in the CL-USER package, not the USER package.
;;;
;;; Revision 1.22  2000/07/20 13:44:38  rtoy
;;; o Remove old fref macro
;;; o Add some comments
;;; o Add macro ARRAY-INITIALIZE to handle creating the appropriate for to
;;;   give to make-array :initial-contents.
;;;
;;; Revision 1.21  2000/07/19 13:54:27  rtoy
;;; o Add the types ARRAY-DOUBLE-FLOAT, ARRAY-SINGLE-FLOAT, and
;;;   ARRAY-INTEGER4.
;;; o All arrays are 1-D now to support slicing of Fortran arrays
;;;   correctly.
;;; o All arrays are in column-major order just like Fortran (and the
;;;   opposite of Lisp).  This is to support slicing of arrays.  Modified
;;;   FREF to support this by taking an extra arg for the dimensions of
;;;   the array.
;;; o Added macro ARRAY-SLICE to slice the array properly.
;;; o Optimized routine DMIN1 a bit.   (Need to do that for more routines.)
;;;
;;; Revision 1.20  2000/07/14 15:50:59  rtoy
;;; Get rid of *dummy_var*.  It's not used anymore.
;;;
;;; Revision 1.19  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-----------------------------------------------------------------------------
