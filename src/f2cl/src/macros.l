; macros.l - all the basic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Hamilton, New Zeland 1992-95 - all rights reserved;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :user)
(eval-when (compile load eval)
  (export '(do1 do! map-defvar double-cdr double-mapcar push 
	    fexport fproclaim fuse-package fin-package fdo arithmetic-if
	    computed-goto assigned-goto
	    fref fset f2cl/
	    int ifix idint real float sngl dble cmplx ichar char aint dint
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

(defvar *dummy_var* nil
  "Used by f2cl for dummy return values")

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

(defmacro fref (arr indices &optional low-bnds)
  (if low-bnds
      `(aref ,arr ,@(mapcar #'(lambda (k lim)
				`(- ,k ,lim))
			    indices low-bnds))
      `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) indices))))

(defmacro fset (a b) 
  `(setf (fref ,(second a) ,@(cddr a)) ,b))

(defmacro rfref (arr &rest indices)
  `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) (reverse indices))))

(defmacro rfset (a b) 
  `(setf (rfref ,(second a) ,@(cddr a)) ,b))


;----------------------------------------------------------------------------

#-aclpc (defmacro while (con &rest body)
            `(loop (if (not ,con) (return t)) ,@body))
;------------------------------------------------------------------

(defun comment (s) (when *verbose* (princ s) (terpri)))

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
      (declare (fixnum ,step ,iteration_count))
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
	  (values (truncate ,top ,bot))
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
    idim dim ddim dprod max max0 amax1 dmax1 amax0 amax1 min min0 amin1 dmin1
    min1 len index lge lgt lle llt aimag conjg sqrt dsqrt csqrt 
    exp dexp cexp log alog dlog clog log10 alog10 dlog10 sin dsin csin
    cos dcos ccos tan dtan asin dasin acos dacos atan datan atan2 datan2
    sinh dsinh cosh dcosh tanh dtanh))

; some macros for intrinsic functions
(defmacro int (x)
  (let ((xval (gensym)))
    ;; We use fixnum here because f2cl thinks Fortran integers are
    ;; fixnums.  If this should change, we need to change the ranges
    ;; here as well.
    `(let ((,xval ,x))
      (etypecase ,xval
	(integer
	 ,xval)
	(single-float
	 (truncate (the (single-float #.(float most-negative-fixnum)
				       #.(float most-positive-fixnum))
		      ,xval)))
	(double-float
	 (truncate (the (double-float #.(float most-negative-fixnum 1d0)
				       #.(float most-positive-fixnum 1d0))
		      ,xval)))))))
  
(defmacro ifix (x)
  `(int ,x))
(defmacro idfix (x)
  `(int ,x))

(defmacro aint (x)
  (let ((xval (gensym)))
    `(let ((,xval ,x))
      (etypecase ,xval
	(single-float
	 (ftruncate (the (single-float #.(float most-negative-fixnum)
				       #.(float most-positive-fixnum))
		      ,xval)))
	(double-float
	 (ftruncate (the (double-float #.(float most-negative-fixnum 1d0)
				       #.(float most-positive-fixnum 1d0))
		      ,xval)))))))

(defmacro dint (x)
  `(aint ,x))

(defmacro anint (x)
  (let ((xval (gensym)))
    `(let ((,xval ,x))
      (coerce (round ,xval) (type-of ,xval)))))
(defmacro dnint (x)
   `(values (fround ,x)))
(defmacro nint (x)
   `(values (round ,x)))
(defmacro idnint (x)
   `(values (round ,x)))

(defmacro real_ (x)
   `(coerce (realpart ,x) 'single-float))

(defmacro sngl (x)
   `(coerce (realpart ,x) 'single-float))

(defmacro dble (x)
  `(coerce (realpart ,x) 'double-float))

(defmacro cmplx (x &optional y)
   `(complex ,x ,(if y y 0)))

(defmacro ichar (c)
   `(char-int ,c))
(defmacro fchar (i)  ;intrinsic function char
   `(char-int ,i))

#-aclpc
(defmacro iabs (x)
  `(abs ,x))
(defmacro dabs (x)
   `(abs ,x))
(defmacro cabs (x)
   `(abs ,x))

(defmacro amod (x y)
  `(mod ,x ,y))
(defmacro dmod (x y)
  `(mod ,x ,y))


(defmacro isign (x y)
  `(if (>= ,y 0) (abs ,x) (- (abs ,x))))
(defmacro sign (x y)
  `(float-sign ,y ,x))
(defmacro dsign (x y)
  `(float-sign ,y ,x))


(defmacro idim (x y)
  `(abs (- ,x ,y)))
(defmacro dim (x y)
  `(abs (- ,x ,y)))
(defmacro ddim (x y)
  `(abs (- ,x ,y)))

(defmacro dprod (x y)
  `(coerce (* ,x ,y) `double-float))

(defmacro max0 (&rest x)
  `(max ,@x))
(defmacro amax1 (&rest x)
  `(max ,@x))
(defmacro dmax1 (&rest x)
  `(max ,@x))
(defmacro amax0 (&rest x)
  `(values (float (max ,@x))))
(defmacro max1 (&rest x)
  `(values (float (max ,@x))))

(defmacro min0 (&rest x)
  `(min ,@x))
(defmacro amin1 (&rest x)
  `(min ,@x))
(defmacro dmin1 (&rest x)
  `(min ,@x))
(defmacro amin0 (&rest x)
  `(values (float (min ,@x))))
(defmacro min1 (&rest x)
  `(values (float (min ,@x))))

(defmacro len (s)
   `(length ,s))

(defmacro index (s1 s2)
 (declare (ignore s1 s2))
   `(error "macro for intrinsic INDEX not yet implemented"))

(defmacro lge (s1 s2)
   `(string>= ,s1 ,s2))
(defmacro lgt (s1 s2)
   `(string> ,s1 ,s2))
(defmacro lle (s1 s2)
   `(string<= ,s1 ,s2))
(defmacro llt (s1 s2)
   `(string< ,s1 ,s2))

(defmacro aimag (c)
   `(imagpart ,c))
(defmacro conjg (c)
   `(conjugate ,c))

;; This creates macros for fsqrt and flog.  These macros essentially
;; tell Lisp that the arguments are always non-negative so that the
;; results are always real numbers.  Thus, for smart compilers (like
;; CMUCL), we don't have to use the generic function.  Also, for smart
;; compilers, the COND expression can be collapsed into just the
;; correct function call.  CMUCL can do this because it knows the type
;; of the argument at compile time.  Other compilers may not be this
;; smart.

(macrolet ((frob (ftn lsp)
	     `(defmacro ,ftn (x)
	       (let ((xval (gensym)))
		 `(let ((,xval ,x))
		   (cond ((typep ,xval 'single-float)
			  (,',lsp (the (single-float 0.0f0) ,xval)))
			 ((typep ,xval 'double-float)
			  (,',lsp (the (double-float 0.0d0) ,xval)))
			 (t
			  (,',lsp ,xval))))))))
  (frob fsqrt sqrt)
  (frob flog log))

;; Tell Lisp that the arguments always have the correct range.  If
;; this is not true, the original Fortran code was broken anyway, so
;; GIGO (garbage in, garbage out).

(defmacro dsqrt (x)
   `(sqrt (the (double-float 0d0) ,x)))
(defmacro csqrt (x)
   `(sqrt ,x))
(defmacro alog (x)
   `(log (the (single-float 0f0) ,x)))
(defmacro dlog (x)
   `(log (the (double-float 0d0) ,x)))
(defmacro clog (x)
   `(log ,x))
(defmacro alog10 (x)
   `(log (the (single-float 0f0),x) 10.0))
(defmacro dlog10 (x)
   `(log (the (double-float 0d0) ,x) 10.0d0))
(defmacro log10 (x)
  (let ((xval (gensym)))
    `(let ((,xval ,x))
      (cond ((typep ,xval 'single-float)
	     (log (the (single-float 0.0f0)) ,xval 10.0))
	    ((typep ,xval 'double-float)
	     (log (the (double-float 0.0d0)) ,xval 10d0))
	    (t
	     (/ (log ,xval)
		(float 10 (type-of (realpart ,xval)))))))))

(defmacro dexp (x)
   `(exp ,x))
(defmacro cexp (x)
   `(exp ,x))

(defmacro dsin (x)
   `(sin ,x))
(defmacro csin (x)
   `(sin ,x))

(defmacro dcos (x)
   `(cos ,x))
(defmacro ccos (x)
   `(cos ,x))

(defmacro dtan (x)
   `(tan ,x))
(defmacro ctan (x)
   `(tan ,x))

(defmacro dasin (x)
   `(asin ,x))
(defmacro dacos (x)
   `(acos ,x))
(defmacro datan (x)
   `(atan ,x))
(defmacro atan2 (x y)
  `(atan ,x ,y))
(defmacro datan2 (x y)
  `(atan ,x ,y))

(defmacro dsinh (x)
   `(sinh ,x))
(defmacro dcosh (x)
   `(cosh ,x))
(defmacro dtanh (x)
   `(tanh ,x))

(defmacro ffloat (x)
  `(coerce ,x 'single-float))

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

(defmacro fformat1 (dest directive arg)
  (let ((val (gensym)))
    `(let ((,val ,arg))
      (if (arrayp ,val)
	  (print ,val)
	  (format ,dest ,directive ,val)))))

(defun expand-format (dest cilist args)
  (if (equal cilist '("~A~%"))
      (append (mapcar #'(lambda (arg) `(fformat1 ,dest "~A " ,arg)) args)
	   `((format ,dest "~%")))

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
