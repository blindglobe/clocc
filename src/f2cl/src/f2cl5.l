; -*- Mode: lisp; package: F2CL -*-
; f2cl5.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;functions:
; declaration insertion
;      check_new_vbles
;      insert-declarations
;      make-initialisation
;      make-declaration
;      make-char-decl
;      make-char-init
;      get-implicit-type
;      get_array_type
;      default-int-p
;      vble-is-array-p
;      sym-is-fun-name
;      make-special-proclamation
;      make-special-var-decl
;      check-reserved-lisp-names
; structured stmt fix
;      fix-structure
;      fix-ifthen
;      end-do-p
;      label-matches-dolabel-p
;      fix-do
;      fix-tagbodies
;      remove-prefix 
;      multiple-do-labelp
; FORMAT parsing
;      parse-format
;      parse-format1
;      parse-write
;      parse-format-descriptor-list
;      fixnum-string
;      parse-format-descriptor-I
;      parse-format-descriptor-F
;      parse-format-descriptor-E
;      parse-format-descriptor-G
;      parse-format-descriptor-/
;      parse-format-descriptor-TR
;      parse-format-descriptor-X
;      parse-format-descriptor-S
;      parse-default-format-descriptor
;      fix-slashes

;;-----------------------------------------------------------------------------
(in-package :f2cl)

;; functions for setting up varaible declarations and initialisations
(eval-when (compile load eval)
  (proclaim '(special *intrinsic-function-names* *external-function-names*
	      *declared_vbles* *undeclared_vbles* *key_params* *save_vbles*
	      *explicit_vble_decls* *implicit_vble_decls* *common_array_dims*
	      *subprog_common_vars* *program-flag* 
	      *subprog-stmt-fns* *subprog_stmt_fns_bodies*
	      *auto-save-data*
	      *functions-used*
	      *vble-declaration-done*))
)

(defvar *fortran-keywords*
  '(ASSIGN IF THEN GO TO GOTO CALL CONTINUE DO FDO ELSE ELSEIF RETURN
    STOP PRINT READ WRITE BACKSPACE CLOSE ENDFILE INQUIRE
    OPEN REWIND FORMAT AND OR NOT))



(defun check_new_vbles (exprs)
  (cond ((or (null exprs)
	     (numberp exprs)
	     (typep exprs 'string))
	 nil)
	((symbolp exprs)
	 (cond ((or (member exprs '(\, \' + - * / ** // ^ = EQUAL > < >= <= >< |:|
				    FREF FREF-STRING F2CL/
				    FLOG FSQRT FFLOAT EXPT
				    %FALSE% %TRUE%
				    FUNCALL))
		    (member exprs *fortran-keywords*)
		    (member exprs *key_params* :key #'car)
		    (member exprs *external-function-names*)
		    (member exprs *declared_vbles*)
		    (member exprs *undeclared_vbles*)
		    (sym-is-fun-name (list exprs))
		    (sym-is-number-p exprs))
		nil)
	       ((eq exprs 'PAUSE)
		)
	       (t
		(pushnew (check-reserved-lisp-names exprs) *undeclared_vbles*))))
	((listp exprs)
	 ;; Function calls get turned into a MULTIPLE-VALUE-BIND, so
	 ;; the only possible place for variables is the arg list for
	 ;; the function.
	 (if (eq (first exprs) 'multiple-value-bind)
	     (mapc #'check_new_vbles (cddr (third exprs)))
	     (mapc #'check_new_vbles exprs)))
	(t
	 (Error "What happened?")))
  exprs)

;; This takes a list of individual Lisp declarations and combines them
;; into one based on the declared type.  Thus
;;
;;  (declare (type fixnum x))
;;  (declare (type fixnum y))
;;  (declare (type single-float z))
;;
;; becomes
;;
;;  (declare (type fixnum x y)
;;           (type single-float z))
(defun pretty-decls (decls)
  (let ((type-hash (make-hash-table :test 'equal)))
    (dolist (decl decls)
      (destructuring-bind (dcl (type v-type var))
	  decl
	(declare (ignore dcl type))
	(let ((val (gethash v-type type-hash)))
	  (setf (gethash v-type type-hash)
		(push var val)))))
    (let ((dec '()))
      (maphash #'(lambda (key val)
		   (push `(type ,key ,@val) dec))
	       type-hash)
      `((declare ,@dec)))))

(defun lookup-vble-type (vble &optional (decls *explicit_vble_decls*))
  ;;(format t "lookup-vble: ~a~%" vble)
  (cond ((or (member vble *declared_vbles*)
	     (member vble *subprog-arglist*))
	 ;;(format t "  is declared var~%")
	 ;;(format t "  explicit_vbld_decls = ~A~%" *explicit_vble_decls*)
	 
	 ;; First for declared variables
	 (do* ((type-clauses decls
			     (rest type-clauses))
	       (type (member vble (cdar type-clauses) :key #'car)
		     (member vble (cdar type-clauses) :key #'car)))
	      ((or type
		   (null type-clauses))
	       (let ((v-type (caar type-clauses)))
		 (cond ((eq v-type 'array)
			;; Hmm, this is probably an array declared
			;; with a dimension statement, and either the
			;; type is declared later or implicit typing
			;; is used.
			;;
			;; Try to lookup the type again but skip over
			;; array types.  If this works, the array type
			;; was explicitly given so use it. Otherwise,
			;; use Fortran typing rules.
			(cond ((lookup-vble-type vble (remove 'array decls :key #'first)))
			      ((get-implicit-type vble))
			      ((default-int-p vble)
			       'integer4)
			      (t 'single-float)))
		       ((and (listp v-type)
			     (eq (first v-type) 'character))
			;; Hmm, a Fortran character string.  Make it a Lisp string
			(if (numberp (second v-type))
			    `(string ,(second v-type))
			    'string))
		       (t
			;; If type is NIL, use implicit type
			(if (null v-type)
			    (cond ((get-implicit-type vble))
				  ((default-int-p vble)
				   'integer4)
				  (t
				   'single-float))
			    v-type))))
	       ;;(format t  "type-clause = ~S~%" type-clauses)
	       )))
	(t
	 ;;(format t "  is not declared. Implicit type = ~a~%" (get-implicit-type vble))
	 ;;(format t "implicit ~a~%" *implicit_vble_decls*)
	 (cond ((get-implicit-type vble))
	       ((default-int-p vble)
		'integer4)
	       (t
		'single-float)))))

;; A simple implementation of Fortran contagion.
;;
;; Coerce 0 to the types T1 and T2 and add the result.  The type of
;; the sum is the desired type.  However, we convert some of the types
;; to ones that f2cl understands.
(defun fortran-contagion (t1 t2)
  (let ((sum (+ (coerce 0 t1) (coerce 0 t2))))
    (typecase sum
      ((complex double-float)
       'complex16)
      ((complex single-float)
       'complex8)
      (integer
       'integer4)
      (t
       (type-of sum)))))

(defun get-fun-arg-type (arg)
  (cond ((symbolp arg)
	 (case arg
	   ((%false% %true%)
	    'logical)
	   (t
	    ;; Lookup the variable type and return it.
	    (let ((var-type (lookup-vble-type arg)))
	      (cond ((eq var-type 'array)
		     ;; Look up the type of the array
		     (destructuring-bind (&optional decl1 decl2)
			 (vble-declared-twice-p arg *explicit_vble_decls*)
		       (declare (ignorable decl2))
		       (values (first decl1) t)))
		    ((vble-is-array-p arg)
		     (values var-type t))
		    (t
		     var-type)))
	    )))
	((listp arg)
	 ;; We have an expression of some type
	 (let ((fun (first arg)))
	   (cond ((eq 'make-array fun)
		  ;; Some array slicing here.
		  (values (second (second (member :element-type arg))) t))
		 ((eq 'array-slice fun)
		  (values (get-fun-arg-type (second arg)) t))
		 ((eq 'funcall fun)
		  ;; Look up function type
		  (get-fun-arg-type (second arg)))
		 ((eq 'fref fun)
		  ;; Look up array references.
		  (let ((result (get-fun-arg-type (second arg))))
		    (values (get-fun-arg-type (second arg)) nil)))
		 ((member fun '(+ -))
		  ;; Basic arithmetic operations that might be unary
		  ;; operators.
		  (if (third arg)
		      (fortran-contagion (get-fun-arg-type (second arg))
					 (get-fun-arg-type (third arg)))
		      (get-fun-arg-type (second arg))))
		 ((member fun '(* f2cl/ expt))
		  ;; Basic arithmetic operations.
		  (fortran-contagion (get-fun-arg-type (second arg))
				     (get-fun-arg-type (third arg))))
		 ((member fun '(conjg))
		  ;; Complex conjugate
		  (fortran-contagion (get-fun-arg-type (second arg))
				     'complex8))
		 ((member fun '(cmplx))
		  ;; Complex.  Figure out the type of complex we
		  ;; should return.
		  (fortran-contagion
		   (fortran-contagion (get-fun-arg-type (second arg))
				      (get-fun-arg-type (third arg)))
		   'complex8))
		 ((member fun '(sin cos tan
				asin acos atan atan2
				exp flog alog10
				fsqrt))
		  ;; Generic functions
		  (get-fun-arg-type (second arg)))
		 ((member fun '(dsin dcos dtan
				dasin dacos datan datan2
				dexp dlog dlog10
				dabs dmax1 dmin1
				dble
				dsqrt))
		  ;; Double-precision functions
		  'double-float)
		 ((member fun '(and or not > >= equal <= < logeqv logxor))
		  ;; Logical operations
		  'logical)
		 (t
		  (get-fun-arg-type fun)))))
	(t
	 ;; If we have a fixnum, return integer4 instead.
	 (if (typep arg 'integer)
	     'integer4
	     (type-of arg)))))

(defun get-upgraded-fun-arg-type (args)
  (let ((res '()))
    ;;(format t "args = ~a~%" args)
    (dolist (arg args
	     (nreverse res))
      ;; Handle the simple cases, and then Run down the list and
      ;; lookup the argument types of each.
      (cond ((numberp arg)
	     (push (type-of arg) res))
	    ((symbolp arg)
	     (push (get-fun-arg-type arg) res))
	    ((and (listp arg)
		  (eq 'fref (first arg)))
	     (push (get-fun-arg-type (second arg)) res))
	    (t
	     (let ((arg-types (mapcar #'get-fun-arg-type arg)))
	       (push (cond ((every #'(lambda (x)
				       (eq x (first arg-types)))
				   arg-types)
			    ;; All items are the same, so that's our type.
			    (first arg-types))
			   (t
			    ;; Some items are different, so we can't
			    ;; really say. (Although I think Fortran
			    ;; says they should all have been the
			    ;; same.)
			    `(or ,@arg-types)))
		     res)))))))

;; This needs to be reworked!
(defun get-arg-decl (list-of-arglists)
  (let (res)
    ;; Given a list of arglists, we derive the type for each of the arglists.
    (dolist (arglist list-of-arglists)
      (push (mapcar #'(lambda (arg)
			;; Literal strings (which are of type (string
			;; <len>)) confuse the mapcar below, so change
			;; them into just type 'string.
			(multiple-value-bind (type arrayp)
			    (get-fun-arg-type arg)
			  (cond (arrayp
				 (if (subtypep type 'string)
				     ;; An array of strings
				     (intern (concatenate 'string "ARRAY-STRINGS"))
				     ;; Some other type of array
				     (intern (concatenate 'string "ARRAY-" (string type)))))
				((subtypep type 'string)
				 'string)
				((subtypep type 'integer4)
				 'integer4)
				(t
				 type))))
		    arglist)
	    res))
    ;; Now make a list of each of the types of the arguments
    (let ((types
	   (mapcar #'(lambda (z)
		       (if (atom z)
			   z
			   (let ((nodup (remove-duplicates z :test #'equalp)))
			     (if (rest nodup)
				 `(or ,@nodup)
				 (first nodup)))))
		   (reduce #'(lambda (x accum)
			       (mapcar #'(lambda (a b)
					   (if (listp a)
					       (cons b a)
					       (list b a)))
				       x accum))
			   res))))
      ;; Remove an extra set of parens if needed.
      (if (and (= (length types) 1) (listp (first types)))
	  (first types)
	  types))))

;; Create declarations for all the functions used in the subprogram.
;; We return three items: statement functions, other functions, and
;; functions in the parameter list of the subprogram.
(defun make-fcn-decl (fcn-list arglist)
  (let (stmt-fcns other-fcns arg-fcns)
    (dolist (fcn fcn-list)
      (destructuring-bind ((fname &optional ftype) args)
	  fcn
	;;(format t "declaring fcn ~S~%" fcn)
	;;(format t "fname = ~A~%" fname)
	;;(format t "args = ~A~%" args)
	(let* ((type (get-fun-arg-type fcn))
	       ;; If the function is an intrinsic, there is exactly
	       ;; one return value.  If it's not an intrinsic but it
	       ;; is a function, the return values are the function
	       ;; value and the list of arguments.  (This is how f2cl
	       ;; works).  If it's not an intrinsic and is a
	       ;; subroutine (ftype non-NIL), the return type is the
	       ;; list of arguments.  (Should we derive the type of
	       ;; these and put them in the values list?)
	       (decl `(function ,(get-arg-decl args)
		       (values
			,@(if ftype nil (list type))
			,@(if (member fname *intrinsic-function-names*)
			      nil
			      '(&rest t))))))
	  ;;(format t "type = ~A~%" type)
	  ;;(format t "decl = ~A~%" decl)
	  ;;(format t "get-arg-decl = ~A~%" (get-arg-decl args))
	  (cond ((member fname *subprog-stmt-fns*)
		 (push `(declare (ftype ,decl ,fname)) stmt-fcns))
		((member fname arglist)
		 ;;(format t "arglist fcn = ~A~%" `(declare (type ,decl ,fname)))
		 (push `(declare (type ,decl ,fname)) arg-fcns))
		((member fname *intrinsic-function-names*)
		 ;; We don't need declarations for intrinsic functions
		 nil)
		(t
		 (push `(declare (ftype ,decl ,fname)) other-fcns))))))
    (values stmt-fcns other-fcns arg-fcns)))

;; Given the program list P, look through it and try to convert all
;; occurrences of f2cl/ with either / or TRUNCATE.  We use TRUNCATE if
;; both parameters are integers; / if both parameters are float or
;; complex.  If we can't determine the types of the parameters, we
;; leave it alone.
;;
;; A new tree with the (possibly) modified code is returned.
;;
;; This is an attempt to optimize out the calls to the f2cl/ macro.
;; This is for the benefit of Lisp compilers that aren't smart enough
;; to optimize unused parts of the macros out. (We should probably do
;; a similar thing for sqrt, log, log10.)
;;
(defun optimize-f2cl/ (p)
  (cond ((or (atom p) (null p))
	 p)
	((eq (first p) 'f2cl/)
	 ;; Let's try to optimize f2cl/ to either / or truncate
	 ;; appropriately.  If both parameters to f2cl/ are integers,
	 ;; we use truncate; if either parameter is a float or
	 ;; complex, use /.  If we can't determine the type, leave the
	 ;; macro in.

	 (let* ((left (get-fun-arg-type (list (second p))))
		(right (get-fun-arg-type (list (third p))))
		(fun (cond ((and (subtypep left 'integer)
				 (subtypep right 'integer))
			    'truncate)
			   ((or (subtypep left 'number)
				(subtypep right 'number))
			    '/)
			   (t
			    'f2cl/))))
	   `(,fun ,@(mapcar #'optimize-f2cl/ (rest p)))))
	(t
	 `(,(first p) ,@(mapcar #'optimize-f2cl/ (rest p))))))

;; Given the program list P, look through it and try to convert all
;; occurrences of external functions with #'<func>.  Do this only if
;; the function is not first element of a list.
;;
;; A new tree with the (possibly) modified code is returned.
;;
(defun fixup-external-function-refs (p externs)
  (cond ((null p)
	 p)
	((atom p)
	 (if (member p externs)
	     `(function ,p)
	     p))
	((eq (first p) 'declare)
	 ;; Skip over declarations
	 p)
	((eq (first p) 'function)
	 ;; Skip over functions that we have already done
	 p)
	((eq (first p) 'funcall)
	 ;; Skip over funcalls!
	 `(,(first p) ,(second p) ,@(mapcar #'(lambda (pp)
						(fixup-external-function-refs pp externs))
					    (cddr p))))
	(t
	 `(,(first p) ,@(mapcar #'(lambda (pp)
				    (fixup-external-function-refs pp externs))
				(rest p))))))

;; In the following functions, we are trying to simplify some of the
;; expressions produced by f2cl.  We should probably use some real
;; pattern matching algorithm instead of these hand-written matchers.
;;
;; Fix up the expression by destructively modifying it. This tries to
;; convert some expressions produced by f2cl into something more
;; readable:

;; f2cl always converts (- X) to (* -1 X).  This function tries to
;; undo that.  A new tree is returned with the result.
(defun fixup-expr-mul (expr)
  (cond ((or (atom expr) (null expr))
	 expr)
	((and (eq (first expr) '*)
	      (numberp (second expr))
	      (= (second expr) -1))
	 ;; Convert (* -1 X) to (- X)
	 `(- ,@(fixup-expr-mul (rest (rest expr)))))
	(t
	 `(,(first expr) ,@(mapcar #'fixup-expr-mul (rest expr))))))

;; We try to handle some conversions that make the resulting
;; expressions a bit easier to read and more natural.  Also, some of
;; the transformations reduce the number of operations needed.  (These
;; may not be needed if the compiler is smart enough.)
;;
;; F2CL                            RESULT
;; (- N)                           -N        (for numeric N)
;; (+ (- Z) X)                     (- X Z)
;; (op (- Z) X)                    (op -Z X) (for number N, and any function F, except +
;; (+ X (- Z))                     (- X Z)
;; (F X (- Z))                     (F X -Z)  (for F /= + and Z a number)
;; (+ X (op (- Z) Y))              (- X (op Z Y))  (for op = * or /)
(defun fixup-expression (expr) 
  (flet ((is-neg (e)
	   ;; Return T if the e looks something like '(- Z)
	   (and (listp e)
		(eq (first e) '-)
		(= (length e) 2))))
    (cond ((or (atom expr) (null expr))
	   expr)
	  ((eq (first expr) 'fref)
	   ;; Fixup the expressions for the index.
	   ;;(format t "expr = ~S: ~S~%" expr (mapcar #'fixup-expression (third expr)))
	   `(fref ,(second expr)
	     ,(mapcar #'fixup-expression (third expr))
	     ,@(if (fourth expr)
		   (list (fourth expr)))))
	  ((eq (first expr) 'fdo)
	   ;; Handle fdo: the fdo macro depends on a certain format
	   ;; for the loop (yuck!  Need to change that!)
	   `(fdo ,(second expr) ,(third expr) ,@(mapcar #'fixup-expression (nthcdr 3 expr))))
	  ((and (eq (first expr) '-)
		(numberp (second expr)))
	   ;; Convert (- N) to just -N
	   (- (second expr)))
	  ((is-neg (second expr))
	   ;; We have (F (- Z) X ...).  If F is '+,
	   ;; make it (- X Z).  If Z is a number, make it (F -Z X ...)
	   (cond ((eq (first expr) '+)
		  ;; (+ (- Z) X ...) => (- X Z)
		  `(- ,@(mapcar #'fixup-expression (rest (rest expr)))
		      ,@(mapcar #'fixup-expression (rest (second expr)))))
		 ((numberp (second (second expr)))
		  `(,(first expr) ,(- (second (second expr)))
		    ,@(mapcar #'fixup-expression (rest (rest expr)))))
		 (t
		  `(,@(mapcar #'fixup-expression expr)))))
	  ((is-neg (third expr))
	   ;; We have (F X (- Z) ...).  If F is '+, convert to (- X Z).
	   ;; If not, but Z is a number, convert to (F X -Z ...).
	   (cond ((eq '+ (first expr))
		  ;; We have '(+ X (- Z)).  Make that '(- X Z)
		  `(- ,@(mapcar #'fixup-expression (list (second expr)))
		      ,@(mapcar #'fixup-expression (list (second (third expr))))))
		 ((numberp (second (third expr)))
		  ;; We have (F X (- N) ...).  Make that (F X -N ...)
		  `(,(first expr) ,@(mapcar #'fixup-expression (list (second expr)))
		                  ,(- (second (third expr)))
		                  ,@(mapcar #'fixup-expression (nthcdr 3 expr))))
		 (t
		  ;; Process the X and (- Z) parts now
		  ;;(format t "T case (F X (- Z) ...): ~S~%" expr)
		  `(,(first expr) ,(fixup-expression (second expr))
		                  ,@(mapcar #'fixup-expression (rest (rest expr)))))))
	  ((and (eq (first expr) '+)
		(listp (third expr))
		(member (first (third expr)) '(* /))
		(listp (second (third expr)))
		(eq (first (second (third expr))) '-))
	   ;; We have (+ X (op (- Z) Y)).  Convert to (- X (op Z Y)), for op = * or /.

	   ;;(format t "got ~S~%" expr)
	   `(- ,(fixup-expression (second expr))
	     (,(first (third expr))
	      ,@(mapcar #'fixup-expression (rest (second (third expr))))
	      ,@(mapcar #'fixup-expression (rest (rest (third expr)))))))
	  (t
	   ;; Didn't match, so descend into the remaining args
	   `(,(first expr) ,@(mapcar #'fixup-expression (rest expr)))))))

(defun fixup-relop (expr)
  (cond ((or (null expr)
	     (atom expr)
	     (typep expr 'string))
	 expr)
	((member (first expr) '(|>=| |<=| EQUAL |/=| < > =))
	 ;; If one arguments is a string, replace operation with the
	 ;; appropriate string operation.
	 (let ((lhs (get-fun-arg-type (list (second expr))))
	       (rhs (get-fun-arg-type (list (third expr)))))
	   (cond ((or (subtypep lhs 'string)
		      (subtypep rhs 'string))
		  (let ((op (intern (concatenate 'string
						 "FSTRING-"
						 (symbol-name (first expr))))))
		    `(,op ,(second expr) ,(third expr))))
		 (t
		  expr))))
	(t
	 `(,@(mapcar #'fixup-relop expr)))))

(defun merge-ops (expr)
  (cond ((or (null expr)
	     (atom expr)
	     (typep expr 'string))
	 expr)
	((and (third expr)
	      (member (first expr) '(+ * - /)))
	 ;; Try to merge a bunch of +, *, -, / operations into one.
	 (destructuring-bind (op next &rest args)
	     expr
	   ;;(setf args (list args))
	   (when (and (listp next) (third next))
	     (loop while (and (listp next) (eq op (first next)))
	       do
	       (push (third next) args)
	       (setf next (second next))))
	   `(,op ,(merge-ops next) ,@(merge-ops args))))
	(t
	 `(,@(mapcar #'merge-ops expr)))))
	 
 

(defun insert-declarations (fort-fun) 
  (prog (defun-bit arglist prog-bit formal-arg-decls common_var_decls
		   local-vbles vble-decls body special-proclamation
		   saved-decls save-inits
		   other-fcn-decls
		   stmt-fcn-decls
		   arg-fcn-decls
		   common-block-structs
		   key-params
		   key-params-decls
		   all-decls)

     (setq defun-bit (list (car fort-fun) (cadr fort-fun))
	   arglist (caddr fort-fun)
	   body (cdddr fort-fun))
     (setq *undeclared_vbles* 
	   (set-difference *undeclared_vbles* *subprog_common_vars*))
     (when (member :insert-declaration *f2cl-trace*)
       (format t "~&")
       (format t "declared_vbles   = ~S~%" *declared_vbles*)
       (format t "undeclared_vbles = ~S~%" *undeclared_vbles*)
       (format t "implicit_vbles   = ~S~%" *implicit_vble_decls*)
       (format t "*functions used* = ~S~%" *functions-used*)
       (format t "external func    = ~S~%" *external-function-names*)
       (format t "*subprog_common_vars* = ~S~%" *subprog_common_vars*)
       (format t "*common_array_dims*   = ~S~%" *common_array_dims*)
       (format t "*explicit_vble_decls* = ~A~%" *explicit_vble_decls*)
       (maphash #'(lambda (key val)
		    (format t "~A => ~A~%" key val))
		*common-blocks*))
     (setq special-proclamation
	   (make-special-proclamation *subprog_common_vars*))
     (when (member :insert-declaration *f2cl-trace*)
       (format t "special-proclamation = ~a~%" special-proclamation))
     (setq common_var_decls 
	   (mapcar #'make-special-var-decl *subprog_common_vars*))
     ;; Clean up the declarations by merging them into one
     (setq common_var_decls
	   (append '(declaim)
		   (mapcar #'(lambda (decl)
			       (second decl))
			   common_var_decls)))

     ;;(format t "*subprog_common_vars* = ~S~%" *subprog_common_vars*)
     ;;(format t "declared = ~S~%" common_var_decls)
     (setf common-block-structs (make-common-block-structure common_var_decls))
     (when (member :insert-declaration *f2cl-trace*)
       (format t "struct = ~S~%" common-block-structs))

     (setq special-proclamation common-block-structs)
     ;;(format t "special-proclamation = ~a~%" special-proclamation)

     ;; Get the declarations for all functions
     (multiple-value-setq (stmt-fcn-decls other-fcn-decls arg-fcn-decls)
       (make-fcn-decl *functions-used* arglist))

     (setq formal-arg-decls
	   (pretty-decls
	    (append
	     ;; Declare any function arguments
	     (if (equalp arg-fcn-decls '((declare)))
		 nil
		 arg-fcn-decls)
	     ;; Declare variables
	     (mapcar #'(lambda (vble)
			 (let ((decl
				(make-declaration vble :vble-is-formal-arg t)))
			   (setf *declared_vbles*
				 (remove vble *declared_vbles*))
			   (setf *undeclared_vbles*
				 (remove vble *undeclared_vbles*))
			   decl))
		     (set-difference arglist *external-function-names*)))))

     (when *relaxed-array-decls*
       ;; Old Fortran practice often declared arrays to functions to
       ;; have length 1.  Since Fortran didn't check array bounds,
       ;; this basically meant such arrays could have any length.
       ;; This bit of code converts any such explicit declarations of
       ;; sizes to '*, meaning anything goes.
       (setf formal-arg-decls
	     (list 
	      (mapcar #'(lambda (decl)
			  (cond ((and (listp decl)
				      (eq 'type (first decl))
				      (subtypep (second decl) 'array))
				 (destructuring-bind (a n l)
				     (second decl)
				   `(type (,a ,n ,(mapcar #'(lambda (x)
							      (declare (ignore x))
							      '*)
							  l))
				     ,@(rest (rest decl)))))
				(t
				 decl)))
		      (first formal-arg-decls)))))

     ;; Clean up other-fcn-decls.  If there weren't any, make it so,
     ;; instead of leaving it as an empty declare.
     (if (equal other-fcn-decls '((declare)))
	 (setf other-fcn-decls nil)
	 (setf other-fcn-decls (pretty-decls other-fcn-decls)))
     
     ;; If we are auto-SAVE'ing variables initialized in DATA
     ;; statements, we add all of the variables in the *data-init* list
     ;; to the *save_vbles* list, removing duplicates.

     (flet ((extract-var-name (setter)
	      ;; From the setting form, we extract the variable name.
	      ;; Currently setters look something like this:
	      ;;
	      ;; (setq var val)
	      ;;
	      ;; (replace array '(a b c ...))
	      ;;
	      ;; (fset (fref array n bounds) v)
	      ;;
	      ;; (data-implied-do do-loop var val)

	      ;;(format t "e-v-n:  ~S~%" setter)
	      (when (listp setter)
		(cond ((eq 'setq (first setter))
		       (second setter))
		      ((eq 'replace (first setter))
		       (second setter))
		      ((eq 'fset (first setter))
		       (second (second setter)))
		      ((eq 'data-implied-do (first setter))
		       (third setter))))))
       (when (and *auto-save-data* *data-init*)
	 (setf *save_vbles*
	       (remove-duplicates
		(append *save_vbles*
			(remove nil 
				(mapcar #'extract-var-name
					*data-init*)))))))

     ;; If a variable names a function used or an external function,
     ;; delete the variable.
     (setf *declared_vbles*
	   (remove-if #'(lambda (v)
			  (or (member v *functions-used* :key #'caar)
			      (member v *external-function-names*)))
		      *declared_vbles*))
     ;; If a variable names a function used or an external function or
     ;; delete the variable.
     (setf *undeclared_vbles*
	   (remove-if #'(lambda (v)
			  (or (member v *functions-used* :key #'caar)
			      (member v *external-function-names*)))
		      *undeclared_vbles*))

     ;; Initialize local variables
     (setq local-vbles
	   (remove-duplicates
	    (remove nil
		    (mapcar #'make-initialisation 
			    (remove-if 
			     #'(lambda (x)
				 (or (member x *save_vbles*) 
				     (member x *key_params* :key #'car)))
			     (set-difference (append *declared_vbles*
						     *undeclared_vbles*)
					     *subprog_common_vars*))))
	    :test #'(lambda (a b)
		      (eq (first a) (first b)))))

     ;; Remove %false% and %true% from the initialization lists.
     (setf local-vbles (remove-if #'(lambda (x)
				      (member (first x) '(%false% %true%)))
				  local-vbles))

     ;; Declare local variables, but remove any variables explicitly
     ;; declared as SAVE'd.
     (setq vble-decls
	   (pretty-decls
	    (remove-if
	     #'(lambda (x)
		 (member (third (second x)) '(%false% %true%)))
	     (remove-duplicates
	      (remove nil
		      (mapcar #'(lambda (vble)
				  (make-declaration vble
						    :vble-is-formal-arg nil))
			      (remove-if
			       #'(lambda (x)
				   (or (member x *save_vbles*)))
			       (set-difference
				(append
				 (set-difference *declared_vbles* 
						 (mapcar #'car *key_params*))
				 *undeclared_vbles*)
				*subprog_common_vars*))))
	      :test #'(lambda (a b)
			(eq (third (second a))
			    (third (second b))))))))

     ;; If we have saved variables, setup their declarations too.
     ;;(format t "*save_vbles* = ~S~%" *save_vbles*)
     (setq saved-decls
	   (pretty-decls
	    (remove-if
	     #'(lambda (x)
		 (member (third (second x)) '(%false% %true%)))
	     (remove-duplicates
	      (remove nil
		      (mapcar #'(lambda (vble)
				  (make-declaration vble
						    :vble-is-formal-arg nil))
			      *save_vbles*))
	      :test #'(lambda (a b)
			(eq (third (second a))
			    (third (second b))))))))

     ;; Initialize SAVE'd variables appropriately
     (setq save-inits
	   (remove-duplicates
	    (remove nil
		    (mapcar #'make-initialisation 
			    (remove-if 
			     #'(lambda (x)
				 (member x *key_params* :key #'car))
			     *save_vbles*)))
	    :test #'(lambda (a b)
		      (eq (first a) (first b)))))

     ;; Clean up data inits: handle the fset inits and the
     ;; data-implied-do inits.

     ;;(format t "*data-init* before: ~S~%" *data-init*)
     (setq *data-init*
	   (mapcar #'(lambda (init)
		       (flet ((get-dims (var)
				(mapcar #'first (lookup-array-bounds var))))
				  
		       (cond #+nil
			     ((eq 'fset (first init))
			      ;; We need to get the dimensions for this array
			      (destructuring-bind (fset (fref var idx) val)
				  init
				(declare (ignore fset fref))
				`(fset (fref ,var ,idx ,(get-dims var)) ,val)))
			     ((eq 'data-implied-do (first init))
			      ;; We need to get the dimensions for this array
			      (destructuring-bind (ido loop var vals)
				  init
				(declare (ignore ido))
				`(data-implied-do ,loop ,(get-dims var) ,vals)))
			     (t
			      init))))
		   *data-init*))
     ;;(format t "*data-init* after : ~S~%" *data-init*)
     
     (setq arglist (mapcar #'check-reserved-lisp-names arglist))

     ;;(format t "*key_params* = ~S~%" *key_params*)
     ;;(format t "key-params = ~S~%" key-params)
     (setq key-params (mapcar #'(lambda (x) 
                                  (list (check-reserved-lisp-names (car x)) (cadr x))) 
			      *key_params*))
     (setq key-params-decls (mapcar #'(lambda (param)
				       (make-declaration (car param)))
				   *key_params*))
     ;;(format t "key-params-decl = ~S~%" key-params-decls)
     
     ;;(format t "local-vbles     = ~S~%" local-vbles)
     ;;(format t "vbles-decls     = ~S~%" vble-decls)
     ;;(format t "other-fcn-decls = ~S~%" other-fcn-decls)
     ;;(format t "body            = ~S~%" body)
     (setf all-decls (append (rest (first vble-decls))
			     (rest (first other-fcn-decls))))
     (setf all-decls `((declare ,@all-decls)))
     ;;(format t "all-decls = ~A~%" all-decls)
     (setq prog-bit
	   (if (or *save_vbles* *auto-save-data*)
	       ;; If we have SAVE'd variables, don't put their inits
	       ;; into the function.  Put them in the let outside the function
	       ;; where they belong.
	       (if *subprog-stmt-fns*
		   `(labels ,*subprog_stmt_fns_bodies* ,@stmt-fcn-decls
		     (prog ,local-vbles ,@all-decls ,@body))
		   `(prog ,local-vbles ,@all-decls ,@body))
	       (if *subprog-stmt-fns*
		   `(labels ,*subprog_stmt_fns_bodies* ,@stmt-fcn-decls
		     (prog ,local-vbles ,@all-decls ,@*data-init* ,@body))
		   `(prog ,local-vbles ,@all-decls ,@*data-init* ,@body)))
	       )
     ;;(format t "prog-bit = ~%~S~%" prog-bit)

     ;; Do some common fixups to make the code faster (for compilers
     ;; not sufficiently smart) and prettier

     (setf prog-bit (fixup-expression (fixup-expr-mul prog-bit)))

     (setf prog-bit (fixup-relop prog-bit))

     (setf prog-bit (merge-ops prog-bit))

     ;; Try to optimize out calls to f2cl/.
     ;;(format t "fixed-up prog-bit = ~%~S~%" prog-bit)
     (setf prog-bit (optimize-f2cl/ prog-bit))
     ;;(format t "opt prog = ~%~S~%" prog-bit)
     ;;(format t "*save_vbles* = ~a~%" *save_vbles*)
     ;;(format t "*data-init*  = ~a~%" *data-init*)
     ;;(format t "save-inits = ~a~%" save-inits)
     ;;(format t "prog-bit = ~a~%" prog-bit)

     ;; Replace all references to external functions with #'.
     ;; However, if the external function was on the parameter list,
     ;; we don't need to do that.
     (setf prog-bit (fixup-external-function-refs
		     prog-bit
		     (set-difference *external-function-names* arglist)))
     ;; Do the same for intrinsic function names, but be sure to
     ;; remove any variables whose name might match an intrinsic
     ;; function name.

     (when (member :insert-declaration *f2cl-trace*)
       (format t "key-params            = ~A~%" key-params)
       (format t "*declared_vbles*      = ~A~%" *declared_vbles*)
       (format t "*undeclared_vbles*    = ~A~%" *undeclared_vbles*)
       (format t "*subprog_common_vars* = ~A~%" *subprog_common_vars*)
       (format t "arglist               = ~A~%" arglist))
     (setf prog-bit
	   (fixup-external-function-refs
	    prog-bit
	    (set-difference *intrinsic-function-names*
			    (append *declared_vbles*
				    *undeclared_vbles*
				    arglist
				    (mapcar #'first key-params)))))


     ;; Add symbol-macrolets to access the common block variables
     (when *subprog_common_vars*
       (let ((sym-lets '()))
	 (maphash
	  #'(lambda (key varlist)
	      (mapc #'(lambda (var)
			(let ((accessor-name
			       (concatenate 'string
					    (symbol-name key)
					    "-"
					    (symbol-name var)))
			      (name
			       (concatenate 'string
					    "*"
					    (symbol-name key)
					    "-COMMON-BLOCK*")))
			  (push `(,var (,(intern accessor-name) ,(intern name)))
				sym-lets)))
		    varlist))
	  *common-blocks*)
	 (setf prog-bit `(symbol-macrolet ,sym-lets ,prog-bit))))

     
     ;; Return 3 pieces: Any proclamations for special variables, the
     ;; declarations for the special variables, and, finally, the
     ;; function itself.
     (let* ((defun-stuff `(defun ,(cadr defun-bit) ,arglist
			   ;; Remove empty declaration
			   ,@(unless (equal formal-arg-decls '((declare)))
				     formal-arg-decls)
			   ,prog-bit))
	    (save-stuff (if (and (or *save_vbles* *auto-save-data*)
				 save-inits)
			    `(let ,save-inits
			      ,@saved-decls
			      ,@*data-init*
			      ,defun-stuff)
			    defun-stuff))
	    (param-stuff (if key-params
			     `(let* ,key-params
			       ,@key-params-decls
			       ,save-stuff)
			     save-stuff)))
       
       (return 
	 (values special-proclamation
		 (when special-proclamation
		   (make-common-block-init *common-blocks* common_var_decls key-params key-params-decls))
		 param-stuff)))
     ))



;; given vble return (vble init-value)
;; for prog arglist

(defun vble-declared-twice-p (vble vble_decls)
  (let ((ndecls 0)
	v-type v-name)
    (dolist (type-list vble_decls)
      (let ((found (member vble (rest type-list) :key #'car)))
	(when found
	  (incf ndecls)
	  (push (first type-list) v-type)
	  (push (car found) v-name))))
    (when (> ndecls 1)
      (mapcar #'list v-type v-name))))


(defun make-initialisation (vble)
  ;; Look up variable in *data-init* to see if it has an
  ;; initialized value from a data statement
  (flet ((lookup-data-init (v)
	   (let ((val (find-if #'(lambda (name)
				   (eq v (second name)))
			       *data-init*)))
	     ;;(format t "lookup-data-init for ~S = ~S~%" v val)
	     (when val
	       (cond ((eq 'fill (first val))
		      (list 'fill (third val)))
		     ((eq 'setq (first val))
		      (third val))
		     ((eq 'replace (first val))
		      (third val)))))))
    (let* ((vble_name (check-reserved-lisp-names vble))
	   (init-val (lookup-data-init vble))
	   type decl1)
      ;;(format t "*data-init* = ~A~%" *data-init*)
      ;;(format t "*explicit_vble_decls* = ~a~%" *explicit_vble_decls*)
      ;;(format t "vble = ~a~%" vble)
      ;;(format t "init-val = ~S~%" init-val)
      (cond
	;;check for vble with two declarations i.e. an array
	((setf decl1 (vble-declared-twice-p vble *explicit_vble_decls*))
	 ;; If we've already processed this variable, we don't need to
	 ;; do it again.
	 (unless (member vble *vble-declaration-done*)
	   (destructuring-bind (decl1 decl2)
	       decl1
	     ;; Remember that we have done this already
	     (pushnew vble *vble-declaration-done*)
	     (if (eq (car decl1) 'ARRAY)
		 `(,vble_name ,(make_make-array_stmt (cdadr decl1)
						     (car decl2)
						     init-val vble_name))
		 `(,vble_name ,(make_make-array_stmt (cdadr decl2)
						     (car decl1)
						     init-val vble_name))))))
	   
	;; Don't need any initialization for statement functions or
	;; external functions.  (But perhaps we should put one in to
	;; say these are actually functions?)
	((or (member vble *external-function-names*)
	     (member vble *subprog-stmt-fns*))
	 nil
	 )
	;; check for declared variable
	((member vble *declared_vbles*)
	 ;;(format t "declared var:  ~A = ~A~%" vble init-val)
	 (do ((type-clauses *explicit_vble_decls* (cdr type-clauses))
	      (decl nil))
	     ((null type-clauses) )
	   (setq type (caar type-clauses))
	   ;;(format t "type-clauses = ~S~%" type-clauses)
	   ;;(format t "looping: type = ~A~%" type)
	   (cond ((and (listp type)
		       (eq (car type) 'CHARACTER))
		  (if (setq decl (member vble (cdar type-clauses) :key #'car))
		      (return `(,vble_name ,
				(make-char-init (car decl) type)))))
		 ((eq type 'ARRAY)
		  ;;(format t "array type-clauses = ~S~%" (cdar type-clauses))
		  ;;(format t "array decl = ~S~%" (member vble (cdar type-clauses) :key #'car))
		  (if (setq decl (member vble (cdar type-clauses) :key #'car))
		      (return 
			`(,vble_name 
			  ,(make_make-array_stmt
			    (cdar decl)
			    (get_array_type (caar decl) nil)
			    init-val
			    vble_name)))))
		 ((eq type 'LOGICAL)
		  (if (setq decl (member vble (cdar type-clauses) :key #'car))
		      (return
			(if (cdar decl) 
			    `(,vble_name
			      ,(make_make-array_stmt (cdar decl) t))
			    `(,vble_name nil)))))
		 (t
		  ;;(format t "default decl = ~S~%" (member vble (cdar type-clauses) :key #'car))
		  ;;(format t "default init-val = ~S~%" init-val)
		  (if (setq decl (member vble (cdar type-clauses) :key #'car))
		      (return 
			(if (cdar decl) 
			    `(,vble_name
			      ,(make_make-array_stmt (cdar decl) type init-val vble_name))
			    `(,vble_name
			      ,(cond (init-val
				      (remove-*data-init*-var vble_name)
				      init-val)
				     (t
				      (ecase type
					((fixnum integer4 integer2 integer1)
					 0)
					(single-float 0f0)
					(double-float 0d0)
					((complex complex8) #c(0f0 0f0))
					(complex16 #c(0d0 0d0)))))))))))))
	(t
	 (if (setq type (get-implicit-type vble))
	     `(,vble_name ,(cond (init-val
				  (remove-*data-init*-var vble_name)
				  init-val)
				 ((eq type 'fixnum) 0)
				 ((eq type 'single-float) 0f0)
				 ((eq type 'double-float) 0d0)
				 ((eq type 'complex) '(complex 0f0 0f0))
				 ((and (listp type) 
				       (eq (car type) 'character))
				  (make-char-init (list vble) type))
				 (t nil)))
	     (if (default-int-p vble) 
		 `(,vble_name 0)
		 `(,vble_name 0f0))))))))


(defun remove-*data-init*-var (vble)
  (setf *data-init* (remove-if #'(lambda (x)
				 (equal vble (second x)))
			     *data-init*)))

;; DIMS is a list of the dimensions of an array.  DATA is a list of
;; data to use for initializing an array.
(defun fortran-data-init (type dims data)
  (cond ((every #'numberp dims)
	 ;; We can compute everything we need at compile time so let's
	 ;; do it.
	 (let ((data-len (length data))
	       (total-length (reduce #'* dims)))
	   (cond ((< data-len total-length)
		  ;; Need to append some data.
		  `(',(append data (make-list (- total-length data-len)
					      :initial-element (coerce 0 type)))))
		 ((> data-len total-length)
		  ;; Need to truncate some data
		  `(',(subseq data 0 total-length)))
		 (t
		  `(',data)))))
	(t
	 ;; Dispatch the creation to a macro to be compiled later by
	 ;; Lisp.
	 `((array-initialize ,type ,dims ,data))
	 )))

(defun make_make-array_stmt (dimens type &optional init vble)
  (let ((dims (mapcar #'(lambda (x) 
                           (cond ((member '* x) '*)
                                 ((eq (car x) 1) (cadr x))
                                 ((not (member nil (mapcar #'numberp x)))
                                  (1+ (- (cadr x) (car x))))
                                 (t `(1+ (- ,(cadr x) ,(car x))))))
                       dimens)))
    ;; Don't need to initialize this variable separately, so kill it.
    (setf *data-init* (remove-*data-init*-var vble))
    
    (let ((init-val
	   (when init
	     (if (eq 'fill (first init))
		 `(:initial-element ,(second init))
		 `(:initial-contents
		   ,@(fortran-data-init type dims
					(first (rest init))))))))
      (cond ((every #'numberp dims)
	     ;; If all of the dimensions are numbers, we don't have to
	     ;; make a list out of them.  This can speed up array
	     ;; creation for smart compilers.
	     (if (= (length dims) 1)
		 `(make-array ,(first dims) :element-type ',type ,@init-val)
		 `(make-array ,(reduce #'* dims) :element-type ',type ,@init-val)))
	    (t
	     (if (= (length dims) 1)
		 `(make-array ,@dims :element-type ',type ,@init-val)
		 `(make-array (reduce #'* (list ,@dims)) :element-type ',type ,@init-val)))))))

;; create a labels form for when statement functions are present

(defun make-labels-form (local-vbles vble-decls *data-init* body)
    `(labels ,*subprog_stmt_fns_bodies*
      (prog ,local-vbles ,@vble-decls ,@*data-init* ,@body)))


;; Take the bounds of an array and compute the total size of the array
;; if possible.  If not, return '*.  Useful for getting the
;; appropriate type declaration for an array.
(defun f2cl-array-total-size (bounds)
  (list (reduce #'(lambda (accum y)
		    (if (and (numberp accum) (numberp y))
			(* accum y)
			'*)
		    )
		(mapcar #'(lambda (x)
			    (if (every #'realp x)
				(1+ (reduce #'- (reverse x)))
				'*))
			bounds))))

;; make a declaration for vble which may or may not be a formal arg
;; vble_name is used to replace vbles called T with T_

(defun make-declaration (vble &key vble-is-formal-arg)
 (prog (vble_name type decl1)
   (setq vble_name (check-reserved-lisp-names vble))
  (return
   (cond				;check for vble with two declarations i.e. an array
     ((setf decl1 (vble-declared-twice-p vble *explicit_vble_decls*))
      (destructuring-bind (decl1 decl2)
	  decl1
	(when (eq (car decl2) 'array)
	  (rotatef decl1 decl2))
	(return `(declare (type (,*array-type*
				 ,(car decl2)
                                 ,(f2cl-array-total-size (cdadr decl1))
				 )
				 ,vble_name)))))
	  
     ;; Don't need any initialization for statement functions or
     ;; external functions
     ((or (member vble *external-function-names*)
	  (member vble *subprog-stmt-fns*))
      ;;(format t "no declaration needed for ~A~%" vble)
      ;;(format t "stmt fns = ~A~%" *subprog-stmt-fns*)
      ;;(format t "ext fns  = ~A~%" *external-function-names*)
      ;;(format t "arglist  = ~A~%" *subprog-arglist*)
      nil
      )
					; check for declared variable
     ((member vble *declared_vbles*)
      (do ((type-clauses *explicit_vble_decls* (cdr type-clauses))
	   (decl nil))
	  ((null type-clauses) )
	(setq type (caar type-clauses))
	(cond ((and (listp type)
		    (eq (car type) 'CHARACTER))
	       (if (setq decl (member vble (cdar type-clauses) :key #'car))
		   (return (make-char-decl (car decl) type))))
	      ((eq type 'ARRAY)
	       (when (setq decl (member vble (cdar type-clauses) :key #'car))
		   (return `(declare (type (,*array-type*
					    ,(get_array_type (caar decl) 
							     vble-is-formal-arg)
					    ,(f2cl-array-total-size (cdar decl))
					    )
				      ,vble_name)))
		   ))
	      ((eq type 'LOGICAL)
	       (if (setq decl (member vble (cdar type-clauses) :key #'car))
		   (return
		     (if (cdar decl) 
			 `(declare (type (,*array-type* logical ,(f2cl-array-total-size (cadar decl)))
				    ,vble_name))
			 `(declare (type logical ,vble))))))
	      (t (when (setq decl (member vble (cdar type-clauses) :key #'car))
		   (return 
		       (if (cdar decl) 
			   `(declare (type (,*array-type*
					    ,type 
					    ,(f2cl-array-total-size (cdar decl)))
				      ,vble_name))
			   `(declare (type ,type ,vble_name)))))))))
					; check implicitly declared variable
     ((and (not vble-is-formal-arg)
	   (setq type (get-implicit-type vble)))
      (if (and (listp type) (eq (car type) 'CHARACTER))
	  (make-char-decl (list vble_name) type)
	  (progn
	    (when (eq type :none)
	      (warn "Undeclared variable `~A' with no implicit type!" vble))
	    `(declare (type ,type ,vble_name)))))
     ;; otherwise use default declaration
     (t (if (default-int-p vble)
	    `(declare (type integer4 ,vble_name))
	    `(declare (type single-float ,vble_name))))))))

(defun make-char-decl (decl type)
  (flet ((decl-bounds (bounds)
	   (mapcar #'(lambda (b)
		       (destructuring-bind (lo hi)
			   b
			 (if (and (numberp lo) (numberp hi))
			     (+ 1 (- hi lo))
			     '*)))
		   bounds)))
    (cond ((null (cdr decl))
	   ;; scalar, no length spec.
	   ;;(format t "scalar, no length spec = ~A~%" decl)
	   (if (equal (cadr type) '(*))
	       `(declare (type (simple-array base-char (*)) ,(car decl)))    
	       `(declare (type (simple-array base-char (,(cadr type))) ,(car decl)))))
	  ((atom (cadr decl))
	   ;; scalar, length spec.
	   ;;(format t "scalar, length spec = ~A~%" decl)
	   (error "shouldn't happen!")
	   ;;`(declare (type (simple-array base-char (,(cadr decl))) ,(car decl)))
	   )
	  ((equal (cadr decl) '(*))
	   ;; unspecified length spec
	   ;;(format t "unspecified length spec = ~A~%" decl)
           `(declare (type (simple-array base-char (*)) ,(car decl))))
	  (t
	   ;; array, no length spec.
	   ;;(format t "array, no length spec = ~A~%" decl)
           `(declare (type (,*array-type*
			    (simple-array base-char ,(if (second type)
							 `(,(second type))
							 '(*)))
			    ,(decl-bounds (rest decl))) ,(car decl)))))))

(defun make-char-init (decl type)
  (cond ((equal (cadr type) '(*))	; unspecified length spec
	 ;;(format t "make-char-init unspecified length spec (*)~%")
	 "")
	((null (cdr decl))		; scalar, no length spec.      
	 `(make-array '(,(cadr type)) :element-type 'base-char :initial-element #\Space))
	((atom (cadr decl))		; scalar, length spec.
	 (error "shouldn't happen!")
	 ;;`(make-array '(,(cadr decl)) :element-type 'base-char :initial-element #\Space)
	 )
	(t				; array, no length spec.
	 ;;(format t "make-char-init array, no length spec~%")
	 (let ((dims (mapcar #'(lambda (bounds)
				 (destructuring-bind (lo hi)
				     bounds
				   (if (and (numberp lo) (numberp hi))
				       (+ 1 (- hi lo))
				       `(+ 1 (- ,hi ,lo)))))
			     (remove '|,| (rest decl)))))
	   ;;(format t "dims = ~A~%" dims)
	   `(f2cl-init-string ,dims ,(cdr type))))))


(defun get-implicit-type (vble)
   (do ((decls *implicit_vble_decls* (cdr decls))
        (vble-str (string vble)))
       ((null decls) nil)
       (if
        (do ((vble-ranges (cdar decls) (cdr vble-ranges)))
           ((null vble-ranges) nil)
           (if (or (and (cdar vble-ranges)
                        (string>= vble-str (string (caar vble-ranges)))
                        (string<= vble-str (string (cadar vble-ranges))))
                   (string= vble-str (string (caar vble-ranges))))
               (return t)))
        (return (caar decls)))))


(defun get_array_type (decl vble-is-formal-arg) 
   (prog (type)
      (return
       (cond ((and (member decl *common_array_dims*)
                   (setq type (car (member decl *explicit_vble_decls* :key #'caadr)))
                   (not (eq (car type) 'array)))
              (car type))
             ((and (not vble-is-formal-arg)
                   (setq type (get-implicit-type decl)))
              type)
             ((default-int-p decl) 'integer4)
             (t  'single-float)))))

(defun default-int-p (vble)
   (and (string>= (string vble) "I")
        (string< (string vble) "O")))

;; test whether or not a given symbol has been defined as an array
(defun vble-is-array-p (v)
  ;; check to see if v is explicitly declared and dimensioned
  (if (member v *declared_vbles*)
      (do ((decls *explicit_vble_decls* (cdr decls)))
	  ((null decls) nil)
	(if (do ((vbles (cdar decls) (cdr vbles)))
		((null vbles) nil)
	      (if (and (eq v (caar vbles)) (cdar vbles))
		  (return t)))
	    (return t)))
      ;; else check if v is a common variable and an array
      (member v *common_array_dims*)))

; test if a symbol is a function call rather than an undeclared vble
; used in check_new_vbles
(defun sym-is-fun-name (expr)
   (and (listp (cadr expr)) (not (null (cdr expr)))))

;; Test if a symbol is really an f2cl hacked number.  This only
;; happens when the number is in exponential form with a negative
;; exponent.
(defun sym-is-number-p (expr)
  (let* ((s (string expr))
	 (neg (position #\% s)))
    (when neg
      ;; Ok, it might be a number.
      (and (member (aref s (1- neg)) '(#\d #\e) :test #'char-equal)
	   (numberp (read-from-string (subseq s 0 (- neg 2))))
	   (numberp (read-from-string (subseq s (1+ neg))))))))
      
; given a list of variables names proclaim them special
(defun make-special-proclamation (vars)
   (if vars `(proclaim '(special ,@vars)) nil))

; declare a special (common) variable
(defun make-special-var-decl (v)
  (let ((dim (member v *common_array_dims*)))
    (cond				; check if v is an array 
      (dim
       `(declare (type (,*array-type* ,(get_array_type v nil)
			,(f2cl-array-total-size (cadr dim))) 
		  ,(check-reserved-lisp-names v))))
					; else make ordinary declaration
      (t (make-declaration v)))))

(defparameter +reserved-lisp-names+
  '(t pi nil funcall prog prog1 progn mapcar map mapc))

;; Check if the Fortran name would collide with Lisp names like T, PI,
;; NIL, FUNCALL, PROG, etc.  If it does, replace it a new name with a
;; trailing underscore.

(defun check-reserved-lisp-names (x)
  (if (member x +reserved-lisp-names+)
      (intern (concatenate 'string (string x) "_"))
      x))

;-----------------------------------------------------------------------------     
; functions for fixing DO and IF structures

; sort out do loops and if-then-else structures
(defun fix-structure (checklist labels) ; labels is the do labels
   (do ((retlist (list (third checklist) (second checklist) (first checklist))
                 (if clause (cons clause retlist) retlist))
        (exprs (cdddr checklist))
        (clause))
       ((null exprs) (reverse retlist))
       (cond ((listp (car exprs))
              (cond ((eq (caar exprs) 'if-then)
                     (multiple-value-setq (clause exprs) 
                                          (fix-ifthen exprs labels)))
                    ((eq (caar exprs) 'fdo)
                     (multiple-value-setq (clause exprs) (fix-do exprs labels)))
                    ((eq (caar exprs) 'continue_place_holder)
                     (setq clause nil
                           exprs (cdr exprs)))
                    (t (setq clause (car exprs)
                             exprs (cdr exprs)))))
            ((eq (car exprs) 'continue_place_holder)
              (setq clause nil
                    exprs (cdr exprs)))
            (t (setq clause (car exprs)
                     exprs (cdr exprs))))))

(defun fix-ifthen (checklist labels)
    (do ((clause (cdar checklist))
         (exprs (cdr checklist))
         (ret nil) (subclause))
        ((eq (car exprs) 'endif_place_holder)
         (setq ret (cons (reverse clause) ret))
         (values (append '(cond) (fix-tagbodies (reverse ret))) (cdr exprs)))
        (cond ((listp (car exprs))
               (cond ((eq (caar exprs) 'fdo)
                      (multiple-value-setq (subclause exprs) 
                                           (fix-do exprs labels))
                      (setq clause (cons subclause clause)))
                     ((eq (caar exprs) 'if-then)
                      (multiple-value-setq (subclause exprs) 
                                           (fix-ifthen exprs labels))
                      (setq clause (cons subclause clause)))
                     ((eq (caar exprs) 'elseif_place_holder)
                      (setq ret (cons (reverse clause) ret)
                            clause (list (cadar exprs))
                            exprs (cdr exprs)))
                     (t (setq clause (cons (car exprs) clause)
                              exprs (cdr exprs)))))
              ((eq (car exprs) 'continue_place_holder)
               (setq exprs (cdr exprs)))
              (t (setq clause (cons (car exprs) clause)
                       exprs (cdr exprs))))))


; compare FDO_BODY_LABELx with LABELy
; return t if x = y
(defun end-do-p (checklist do-label)
  (and (not (listp (car checklist)))
       (label-matches-dolabel-p (car checklist) do-label)))

; given (go labelx) does it match with current fdo_body_labely
(defun label-matches-dolabel-p (label do_label)
   (and (eq 5 (string-lessp "LABEL" (string label)))
        (equal (string-left-trim "LABEL" label)
               (string-left-trim "FDO_BODY_LABEL" do_label))))

;------------------------------------------------------------------------------

(defun fix-do (checklist labels)
  (do ((do-expr (ldiff (car checklist) (last (car checklist))) )
       (loop-body '(tagbody) 
		  (if subclause (cons subclause loop-body) loop-body))
       (exprs (cdr checklist))
       (subclause nil)
       (do-label (car (last (car checklist)))))

      ((or (null exprs)
	   (end-do-p exprs do-label))
       (cond
	 ((null exprs)
	  (error "A DO statement without a matching label?!"))
	 ((multiple-do-labelp (car exprs) labels)
	  (values (append do-expr 
			  (list (reverse loop-body))) exprs))

	 ((not (eq (second exprs) 'continue_place_holder)) 
	  (values (append do-expr 
			  (list 
			   (reverse (cons (second exprs) 
					  (cons (intern (remove-prefix do-label)) loop-body))))) 
		  (cddr exprs))) 
	 (t 
	  (values (append do-expr 
			  (list 
			   (reverse (cons (intern (remove-prefix do-label)) loop-body)))) 
		  (cddr exprs))) ))
    ;; body
    (cond ((listp (car exprs))
	   (cond ((eq (caar exprs) 'if-then)
		  (multiple-value-setq (subclause exprs) 
		    (fix-ifthen exprs labels)))
		 ((eq (caar exprs) 'fdo)
		  (multiple-value-setq (subclause exprs) 
		    (fix-do exprs labels)))
		 ((and (eq (caar exprs) 'go)
		       (label-matches-dolabel-p (cadar exprs) do-label))
		  ;; Why do we treat a go to to the continue
		  ;; statement of the do loop differently?  Why
		  ;; "(return)" instead of "(go label)"
		  #+nil
		  (setq subclause '(return)
			exprs (cdr exprs))
		  (setq subclause (car exprs)
			exprs (cdr exprs)))
		 (t (setq subclause (car exprs)
			  exprs (cdr exprs)))))
	  ((eq (car exprs) 'continue_place_holder)
	   (setq subclause nil
		 exprs (cdr exprs)))
	  (t (setq subclause (car exprs)
		   exprs (cdr exprs))))))

(defun remove-prefix (x) (string-left-trim "FDO_BODY_" x))

(defun multiple-do-labelp (labelnnn labels)
 (< 1 (length (remove-if-not #'(lambda (x) (label-matches-dolabel-p labelnnn 
          (princ-to-string x))) labels))))

;------------------------------------------------------------------------------

; if a cond clause contains a jump (from within to within) make a tagbody
(defun fix-tagbodies (cond-expr)
   (do ((clauses cond-expr (cdr clauses))
        (new-clauses nil (cons clause new-clauses))
        (clause nil))
       ((null clauses) (reverse new-clauses))
       (setq clause (car clauses)) ;(<pred> expr expr ....)
       (if (member-if #'symbolp (cdr clause))
           (setq clause `(,(car clause) (tagbody ,@(cdr clause)))))))

;----------------------------------------------------------------------------- 

(eval-when (compile load eval)  
  (proclaim '(special *format_stmts* *current_label* *SP* *dlist-flag*)))

(defun parse-format (x)
  (prog (*SP*)
   (declare (special *SP*))
   (setq *SP* nil)
   (setq *format_stmts* (cons (list *current_label*
                                 (parse-format1 (cadr x)))
                            *format_stmts*)))
)

;; x is of form: '(WRITE (* |,| 8000) |,| J |,| K)
;;           or  '(WRITE (*) |,| J |,| K)
;;           or  '(WRITE (* |,| *) |,| J |,| K)
;;
;; Note that the unit can be any arbitrary expression, so be careful.

(defun parse-write (x)
  ;; check for comma before arguments
  (if (and (third x) (not (eq (third x) '|,|)))
      (setq x (append (list (first x) (second x) '|,|) (cddr x))))
  ;; As shown above, (second x) will contain the unit number and the
  ;; format number, if any.  Split this at the comma to get the unit
  ;; part and the format part.
  (destructuring-bind (lun-part fmt-part)
      (list-split '|,| (second x))
    ;;(format t "lun-part = ~A~%" lun-part)
    ;;(format t "fmt-part = ~A~%" fmt-part)
    
    `((fformat ,(parse_format_dest lun-part)
       ,(if (null fmt-part) 
	    '(("~A~%"))
	    (get_format_stmt fmt-part))
       ,@(mapcar #'parse-output-argument 
		 (list-split '|,| (cdddr x)))))))

;; x is of the form: '(PRINT * |,| X |,| Y)
;;               or: '(PRINT 9000 |,| X |,| Y)

(defun parse-print (x)
  `((fformat t
     ,(get_format_stmt (list (second x)))
     ,@(mapcar #'parse-output-argument 
		       (list-split '|,| (cdddr x))))))

;; x is of the form '(read (lun |,| format) var |,| var)
;;
;; We're not trying to implement full Fortran read functionality.  We
;; just want to read data into the variables.  Implied do loops are
;; not supported.
(defun parse-read (x)
  (append (list '(fortran_comment "***WARNING:  READ statement may not be translated correctly!"))
	  (mapcar #'(lambda (var)
		      `(setf ,(if (rest var)
				  var
				  (first var))
			(read)))
		  (remove nil (list-split '|,| (cddr x))))
	  (list '(fortran_comment "***WARNING: Preceding READ statements may not be correct!"))))
  
     
;; Handle things like FMT = number
(defun get_format_stmt (label)
  (let ((fmt-num (first label)))
    (cond ((eq fmt-num 'fmt)
	   ;; We have something like "FMT = number"
	   (get_format_stmt (list (third label))))
	  ((or (eq fmt-num '*)
	       (not (numberp fmt-num)))
	   '(("~A~%")))
	  (t
	   (do ((lis *format_stmts* (cdr lis)))
	       ((null lis)
		(error "Format statement ~A not found" fmt-num))
	     (if (equal fmt-num (caar lis))
		 (return (cadar lis))))))))

;; Figure out where we're trying to WRITE to.
;;
;; DEST can be any arbitrary expression, so we need to parse it. 
(defun parse_format_dest (dest)
  (cond ((null (rest dest))
	 (if (eq (first dest) '*) t (first dest)))
	(t
	 (id-expression dest))))

(defun parse-output-argument (arg) 
   (cond ((null arg) nil)
         ((and (listp arg) (listp (car arg)) (member '= (car arg)))  ;implied-do
          (parse-implied-DO (list-split '|,| (car arg))))
         ((and (listp arg) (every #'listp arg)) ; dlist
          (parse-dlist arg))
         (t (id-expression arg))))

(defun parse-implied-DO (do-list)
   (prog (ctrl-vars dlist ivar e1 e2 e3 (*dlist-flag* nil))
      (setq ctrl-vars (member-if #'(lambda (x) (eq (second x) '=)) do-list)
            dlist (parse-dlist (ldiff do-list ctrl-vars))
            ivar (first (first ctrl-vars))
            e1 (id-expression (cdr (member '= (first ctrl-vars))))
            e2 (id-expression (second ctrl-vars))
            e3 (if (third ctrl-vars) (third ctrl-vars) 1))
      (return
      (if *dlist-flag*
      `(do ((,ivar ,e1 (+ ,ivar ,e3))
            (ret nil (append ret ,dlist)))
           ((> ,ivar ,e2) ret))

      `(do ((,ivar ,e1 (+ ,ivar ,e3))
            (ret nil (cons ,dlist ret)))
           ((> ,ivar ,e2) (reverse ret)))))))

(defun parse-dlist (x)
   (setq *dlist-flag* t)
   `(list ,@(mapcar #'parse-output-argument x)))

;--------------------------------------------------------------------------------

; convert an array to a list
; this is possibly implementation dependent
; but is not actually used by the translator
(defun array-list (array)
  (prog (str i dim old-print-length old-print-level old-print-array)
   (setq old-print-array *print-array*
         old-print-length *print-length*
         old-print-level *print-level*
         *print-array* t
         *print-length* nil
         *print-level* nil)
   (setq str (princ-to-string array)
         dim (length str))
   (setq i (do ((i 0 (1+ i)))
               ((eq (schar str i) #\() i)))
   (setq str
         (read-from-string (make-array (- dim i) :displaced-to str 
                                                 :displaced-index-offset i
                                                 :element-type 'base-char)))
   (setq *print-array* old-print-array
         *print-length* old-print-length
         *print-level* old-print-level)
   (return str)))

;--------------------------------------------------------------------------------

(defun parse-format1 (format &optional (newlinep t))
  (let ((x (list-split '|,| (fix-slashes format))))
    (do ((desc-lists x (cdr desc-lists))
	 (directive) (dl)
	 (directive-list nil (append directive-list directive)))
	((null desc-lists)
	 (if newlinep
	     (append directive-list '("~%"))
	     directive-list))
      (setq dl (car desc-lists))
      (setq directive 
	    (cond ((null dl) nil)	;(list nil))
		  ((stringp (car dl))
		   (list (car dl)))
		  ((listp (car dl))
		   ;; Repeat group forever, as signaled by the rep factor of T.
		   (append  '(T) (list (parse-format1 (car dl) nil))))
		  ((and (numberp (car dl))
			(listp (cadr dl)))
		   ;; Repetition of a group
		   (append (list (car dl))
			   (list (parse-format1 (cadr dl) nil))))
		  ((numberp (car dl))
		   ;; Simple repeated format descriptor (Handles the
		   ;; case where the repetition factor is separated
		   ;; from the format descriptor.  The case when the
		   ;; repetition factor is NOT separated is handled
		   ;; below.
		   (parse-format-descriptor-list (car dl) 
						 (destruct-descriptor-list (cdr dl))))
		  (t (parse-format-descriptor-list 1 
						   (destruct-descriptor-list dl))))))))

;--------------------------------------------------------------------------------

; given a symbol e.g I4 or P2G10.4E2 this function seperates it into numbers and
; symbols eg (#\I 4) or (#\P 2 #\G 10 4 #\E 2)

(defun destruct-descriptor-list (x)
  (let (descriptor-list char atm)
    (cond ((listp x)
	   (setq x (make-string-input-stream
		    (reduce #'(lambda (r s)
				(concatenate 'string r " " s))
			    (mapcar #'symbol-name x)))))
	  ((symbolp x)
	   (setq x (make-string-input-stream (symbol-name x))))
	  (t
	   (return-from destruct-descriptor-list x)))
    (loop
	(setq char (peek-char nil x nil :eof nil))
	(when (equal char :eof)
	  (return-from destruct-descriptor-list (reverse descriptor-list)))
      (cond ((or (digit-char-p char)
		 (member char '(#\+ #\-)))
	     (multiple-value-setq (atm x)
	       (read-number-from-stream x)))
	    ((eq char #\.)
	     (read-char x)
	     (setq atm nil))
	    (t
	     (setq atm (read-char x))))
      (when atm
	(setq descriptor-list (cons atm descriptor-list))))))

(defun read-number-from-stream (x)
   (prog ((number-str "") char)
     loop
   (setq char (peek-char nil x nil 'EOF nil))
   (if (equal char 'EOF) (return (values (read-from-string number-str) x)))
   (if (or (digit-char-p char) (member char '(#\+ #\-)))
       (setq number-str (concatenate `string number-str (string (read-char x))))
       (return (values (read-from-string number-str) x)))
   (go loop)))

;--------------------------------------------------------------------------------
; pattern matching functions adapted from those in f3.l

; bind matched atoms to A etc if the pattern has (> A)
; bind matched groups of atoms to B if the pattern has (+ B)
; the eval obtains the list of atoms seen so far and the cons adds the atom
; matched at the current level
; e.g. > (setq x  '(#\P 2 #\G 10 4 #\E 2))
;      > (pattern-match-and-bind '((+ left) #\G (+ right)) x)
;      binds (#\P 2) to left and (10 4 #\E 2) to right
;      > (pattern-match-and-bind '(#\P (> A) #\G (> B) (> C) (+ D)) x)
;      binds 2 to A, 10 to B, 4 to C and '(#\E 2) to D

(defun pattern-match-and-bind (p d)
      (cond ((and (null p) (null d)) t)
            ((or (null p) (null d)) nil)   
            ((or (equal (car p) '>)       
                 (equal (car p) (car d)))
             (pattern-match-and-bind (cdr p) (cdr d)))
            
            ((and (listp (car p))
                  (equal (caar p) '>)  
                  (pattern-match-and-bind (cdr p) (cdr d)))
             (set (cadar p) (car d)) t) 
            ((equal (car p) '+)                 
             (cond ((pattern-match-and-bind (cdr p) (cdr d)))   
                   ((pattern-match-and-bind p (cdr d)))))
            ((and (listp (car p))
                  (equal (caar p) '+))
             (cond ((pattern-match-and-bind (cdr p) (cdr d))
                    (set (cadar p) (list (car d))) t) ; building begins
                   ((pattern-match-and-bind p (cdr d))
                    (set (cadar p)
                         (cons (car d) (eval (cadar p)))) t)))))

;--------------------------------------------------------------------------------
; functions for parsing format stmts which are stored with their labels
; on a global list (referenced by calls to WRITE)

(defun parse-format-descriptor-list (a x)
  (prog (left right)
   (declare (special left right))
   (if (numberp (car x)) nil (setq x (cons a x)))
   ; find most significant descriptor and parse
   (return 
   (cond ;((typep (cdr x) 'string)
         ; (cdr x))
         ((pattern-match-and-bind '((+ left) #\H (+ right)) x)
          (parse-format-descriptor-H left right))
         ((pattern-match-and-bind '((> left) #\I (+ right)) x)
          (parse-format-descriptor-I left right))
         ((pattern-match-and-bind '((+ left) #\F (+ right)) x)
          (parse-format-descriptor-F left right))
         ((pattern-match-and-bind '((+ left) #\G (+ right)) x)
          (parse-format-descriptor-G left right))
         ((pattern-match-and-bind '((+ left) #\E (+ right)) x)
          (parse-format-descriptor-E left right))
         ((pattern-match-and-bind '((+ left) #\D (+ right)) x)
          (parse-format-descriptor-D left right))
         ((pattern-match-and-bind '((> left) #\T #\R (+ right)) x)
          (parse-format-descriptor-TR right))
         ((pattern-match-and-bind '((> left) #\X) x)
          (parse-format-descriptor-X left))
         ((pattern-match-and-bind '((> left) #\S (> right)) x)
          (parse-format-descriptor-S right))
	 ((pattern-match-and-bind '((> left) #\A (+ right)) x)
	  (parse-format-descriptor-A left right))
         ((equal (cadr x) '#\/)
          (parse-format-descriptor-/))
         (t (parse-default-format-descriptor x))
         )))) 

(defun fixnum-string (x)
   (princ-to-string x))

(defun parse-format-descriptor-A (a w)
  (let* ((width (if (listp w)
		   (car w)
		   w))
	(directive (list (format nil "~~~DA" width))))
    (list a (list directive))))

(defun parse-format-descriptor-I (a w)
  (let* ((width (if (listp w)
		   (car w)
		   w))
	(directive (list (format nil "~~~DD" width))))
    (list a (list directive))))

;<kP><a>Fw.d -> ~a{~w,d,k,,,[@]F}
(defun parse-format-descriptor-F (left right)
  (let ((k 0)
	(a 1))
    (declare (special k a))
    (or (pattern-match-and-bind '((> k) #\P (> a)) left)
	(pattern-match-and-bind '((> k) #\P) left)
	(pattern-match-and-bind '((> a)) left))
    (let ((directive
	   (list (concatenate 'string 
			      "~" (fixnum-string (car right)) "," ;w
			      (fixnum-string (cadr right)) "," ;d
			      (fixnum-string k) "," ;k
			      "'*,"
			      (if *SP* "@F" "F")))))
      (list a (list directive)))))

;; Note: The Fortran standard says that, for a format like Ew.d, if
;; the the exponent is three digits long, the exponent indicator (E or
;; is not printed.  Thus instead of xxxE+ddd, we get xxx+ddd.  We
;; don't handle that here!
(defun parse-format-descriptor-E (left right)
  (let ((k 0) (a 1) w d (e 2))
     (declare (special k a w d e))
     (or (pattern-match-and-bind '((> k) #\P (> a)) left)
         (pattern-match-and-bind '((> k) #\P) left)
         (pattern-match-and-bind '((> a)) left))
     (or (pattern-match-and-bind '((> w) (> d)) right)
         (pattern-match-and-bind '((> w) (> d) #\E (> e)) right))
     (let ((directive 
	    (list (concatenate 'string
			       "~" (fixnum-string w) ","
			       (fixnum-string d) ","
			       (fixnum-string e) ","
			       (fixnum-string k) ",'*,,'E"
			       (if *SP* "@E" "E")))))
       (list a (list directive)))))

(defun parse-format-descriptor-D (left right)
  (let ((k 0) (a 1) w d (e 2))
     (declare (special k a w d e))
     (or (pattern-match-and-bind '((> k) #\P (> a)) left)
         (pattern-match-and-bind '((> k) #\P) left)
         (pattern-match-and-bind '((> a)) left))
     (or (pattern-match-and-bind '((> w) (> d)) right)
         (pattern-match-and-bind '((> w) (> d) #\E (> e)) right))
     (let ((directive 
	    (list (concatenate 'string
			       "~" (fixnum-string w) ","
			       (fixnum-string d) ","
			       (fixnum-string e) ","
			       (fixnum-string k) ",'*,,'D"
			       (if *SP* "@E" "E")))))
       (list a (list directive)))))

(defun parse-format-descriptor-G (left right)
  (let ((k 0) (a 1) w d (e 2))
     (declare (special k a w d e))
     (or (pattern-match-and-bind '((> k) #\P (> a)) left)
         (pattern-match-and-bind '((> k) #\P) left)
         (pattern-match-and-bind '((> a)) left))
     (or (pattern-match-and-bind '((> w) (> d)) right)
         (pattern-match-and-bind '((> w) (> d) #\E (> e)) right))
     (let ((directive 
	    (list (concatenate 'string
			       "~" (fixnum-string w) ","
			       (fixnum-string d) ","
			       (fixnum-string e) ","
			       (fixnum-string k) ",'*"
			       (if *SP* "@G" "G")))))
       (list a (list directive)))))

(defun parse-format-descriptor-/ ()
   '("~%"))

(defun parse-format-descriptor-TR (s)
   (list (concatenate 'string "~" (fixnum-string s) "@T")))


(defun parse-format-descriptor-X (w)
   (list (concatenate 'string "~" (fixnum-string w) "@T")))

(defun parse-format-descriptor-S (right)
   (setq *SP* (if (eq right #\P) t nil))
   "")

(defun parse-format-descriptor-H (width string)
  (declare (ignore width))
  (list (coerce (mapcar #'(lambda (char-or-digit)
			    (cond ((numberp char-or-digit)
				   (aref (princ-to-string char-or-digit) 0))
				  ((symbolp char-or-digit)
				   (aref (symbol-name char-or-digit) 0))
				  (t
				   char-or-digit)))
			string)
		'string)))

(defun parse-default-format-descriptor (x)
   (list (do ((i 1 (1+ i))
              (ret nil (append ret '("~A"))))
             ((> i (car x)) ret))))

;; given a list containing /'s put commas on either side
;; e.g. (x / y) -> ( |,| / |,| y) 
;;      (/ x) -> (/ |,| x)
;;
;; Note that f2cl will convert // to f2cl-//, so if we find that, we
;; need to insert the appropriate number of commas and slashes.
(defun fix-slashes (x)
  (do ((lis x (cdr lis))
       (ret nil 
	    (append ret (cond ((eq (car lis) 'f2cl-//)
			       (cond ((and ret (cdr lis))
				      '(|,| / |,| / |,|))
				     ((cdr lis)
				      '(/ |,| / |,|))
				     (t
				      '(|,| / |,| /))))
			      ((eq (car lis) '/) 
			       (cond ((and ret (cdr lis))
				      '(|,| / |,|))
				     ((cdr lis)
				      '(/ |,|))
				     (t '(|,| /))))
			      (t
			       (list (car lis)))))))
      ((null lis) ret)))


;; Creates the slots for the common block.  Each variable in the
;; common block is a slot.  We initialize the slot with the
;; appropriate value and type.  Even if the array dimensions are
;; known, we don't use them here, just in case the dimensions contain
;; Fortran PARAMETER values.
(defun make-common-block-vars (varlist common_var_decls)
  ;; VARLIST is the list of variables in the common block.
  ;; COMMON_VAR_DECLS is a set of declaims computed in
  ;; INSERT-DECLARATIONS.  We use that to determine the appropriate
  ;; initalizations for the variables.
  (mapcar #'(lambda (var)
	      (let* ((decl (find var (rest common_var_decls)
				 :key #'third))
		     (var-type (if decl (second decl) nil))
		     (dims (cond ((and (listp var-type)
				       (subtypep var-type 'array)
				       (not (subtypep var-type 'string)))
				  ;;(format t "array var-type = ~A ~A~%" var var-type)
				  ;;(format t "lookup-array-bounds ~A = ~A~%" var (lookup-array-bounds var))

				  ;; If the dimension of the array is
				  ;; a number, use the actual
				  ;; dimension.  Otherwise use 0.
				  (if (every #'numberp (third var-type))
				      (third var-type)
				      0))
				 (t nil)))
		     (var-init (cond ((and (listp var-type)
					   (subtypep (first var-type) 'array))
				      `(make-array ',dims :element-type ',(second var-type)
					:initial-element ,(if (subtypep (second var-type) 'logical)
							      nil
							      (coerce 0 (second var-type)))))
				     ((subtypep var-type 'logical)
				      nil)
				     (t
				      (coerce 0 var-type)))))
		;; Initialize the slot with a 0 of the appropriate
		;; type (for scalars) or a zero element array of the
		;; appropriate dimensions for array slots.
		(if dims
		    `(,var ,var-init :type (,*array-type* ,(second var-type) ,(subst '(*) 0 dims)))
		    `(,var ,var-init :type ,var-type))))
	  varlist))

			     
;; Create a structure for the given common blocks.
(defun make-common-block-structure (common_var_decls)
  (let ((res '()))
    (maphash #'(lambda (key varlist)
		 ;;(format t "key varlist = ~S ~S~%" key varlist)
		 ;;(format t "res = ~S~%" res)
		 ;; The varlist looks something like (v1 v2 v3
		 ;; (v3-dims) v4 (v4-dims)).  That is if the variable
		 ;; is an array, the following tiem in varlist gives
		 ;; the dimensions of the array.
		 (push `(defstruct (,key (:predicate ,(intern (concatenate 'string
									   "IS-"
									   (symbol-name key)
									   "-P"))))
			 ,@(make-common-block-vars varlist common_var_decls))
		       res))
	     *common-blocks*)
    (nreverse res)))

(defun make-common-block-var-init (varlist common_var_decls)
  (flet ((fixup-bounds (bounds)
	   (mapcar #'(lambda (bound)
		       (if (every #'numberp bound)
			   (1+ (- (second bound) (first bound)))
			   `(1+ (- ,(second bound) ,(first bound)))))
		   bounds)))
    (let ((initializer '()))
      (dolist (var varlist)
	(let* ((decl (find var (rest common_var_decls)
			   :key #'third))
	       (var-type (if decl (second decl) nil))
	       (dims (cond ((and (listp var-type)
				 (subtypep (first var-type) 'array)
				 (not (subtypep var-type 'string)))
			    (fixup-bounds (lookup-array-bounds var)))
			   (t nil)))
	       (var-init (cond ((and (listp var-type)
				     (subtypep (first var-type) 'array))
				;; If the dimensions are numbers,
				;; we've already initialized the array
				;; in the structure definition.
				(unless (every #'numberp dims)
				  `(make-array (list ,@dims) :element-type ',(second var-type)
				  :initial-element ,(cond ((subtypep (second var-type) 'logical)
							   nil)
							  (t
							   (coerce 0 (second var-type)))))))
			       ((subtypep var-type 'logical)
				nil)
			       (t
				(coerce 0 var-type)))))
	  (when (and dims var-init)
	    (setf initializer (append initializer
				      `(,(intern (symbol-name var) "KEYWORD") ,var-init))))))
      initializer)))

;; This assigns the common block structure to a global variable.  The
;; common block is initialized with reasonable defaults.  The
;; KEY-PARAMS is needed, just in case arrays in the common block are
;; dimensioned from PARAMETER values.
(defun make-common-block-init (comm-blocks comm-decls key-params key-param-decls)
  ;; COMM-BLOCKS is the hash table of all common blocks.  COMM-DECLS
  ;; is the list of declaims for the variables in the common block.
  ;; KEY-PARAMS is the list of Fortran PARAMETER values.
  ;; KEY-PARAM-DECLS is the appropriate set of declarations for the
  ;; PARAMTERs.
  (let ((var-inits '()))
    (maphash
     #'(lambda (key val)
	 (push `(let* ,key-params
		 ,@key-param-decls
		 (defparameter ,(intern (concatenate 'string
						     "*"
						     (symbol-name key)
						     "-COMMON-BLOCK*"))
		   (,(intern (concatenate 'string
					  "MAKE-"
					  (symbol-name key)))
		     ,@(make-common-block-var-init val comm-decls))))
	       var-inits))
     comm-blocks)
    (nreverse var-inits)))
;;;-----------------------------------------------------------------------------
;;; end of f2cl5.l
;;;
;;; $Id: f2cl5.l,v 1.77 2002/02/07 03:58:45 rtoy Exp $
;;; $Log: f2cl5.l,v $
;;; Revision 1.77  2002/02/07 03:58:45  rtoy
;;; o The previous change messed up the parsing of the format number of a
;;;   write statement.  Fix it.
;;; o The previous change also messed up the unit number for
;;;   PARSE_FORMAT_DEST. Fix it.
;;; o Add a parser for Fortran's A format descriptor.
;;;
;;; Revision 1.76  2002/02/04 03:22:31  rtoy
;;; Handle the case where the unit number for a write statement can be an
;;; arbitrary expression.
;;;
;;; Revision 1.75  2002/01/13 16:57:52  rtoy
;;; When looking up variables in the declared variables list or the
;;; subprog arglist, we still need to handle them via Fortran's implicit
;;; typing rules.
;;;
;;; Revision 1.74  2002/01/09 15:31:08  rtoy
;;; o In GET-FUN-ARG-TYPE, we weren't correctly handling the case of unary
;;;   + and -.
;;; o When looking up the type of a variable, we need to check for the
;;;   variable in *subprog-arglist* as well as *declared_vbles*!
;;;
;;; Thanks to Mike Koerber for sending sample code where this fails.
;;;
;;; Revision 1.73  2002/01/08 03:24:44  rtoy
;;; o Correct the previous change about looking up the type of array
;;;   references.  If we have an fref, it can't be an array slice because
;;;   we would have already sliced it before we get here.
;;; o Try to pretty up the declarations for other functions by merging
;;;   them into just one declaration.
;;; o Group the declaration statements for variables and other functions
;;;   into just a single declaration.
;;;
;;; Revision 1.72  2002/01/07 20:55:40  rtoy
;;; In GET-FUN-ARG-TYPE, we were always returning array types for FREF.  This
;;; is true if array-slicing is enabled.
;;;
;;; Revision 1.71  2002/01/06 23:10:12  rtoy
;;; Rename *intrinsic_function_names*, *external_function_names* and
;;; *subprog_stmt_fns* to use dashes.
;;;
;;; Revision 1.70  2001/06/04 17:16:24  rtoy
;;; Print a warning if there is no implicit type given and the variable
;;; was not declared.  This is invalid Fortran anyway.
;;;
;;; Revision 1.69  2001/06/03 20:46:08  rtoy
;;; Changes to FIX-DO:
;;; o Add a test and code to keep FIX-DO from looping forever searching
;;;   for a non-existent DO label.  We cause an error now if this happens.
;;; o Gratuitously re-indented code.
;;;
;;; Revision 1.68  2000/09/03 02:33:39  rtoy
;;; FORTRAN-CONTAGION returned (complex double-float) and (complex
;;; single-float) which confuses f2cl.  Return complex16 and complex8
;;; instead, respectively.
;;;
;;; Revision 1.67  2000/08/30 16:56:24  rtoy
;;; In PARSE-FORMAT1, if there's no repetition factor for a group, then
;;; the group is supposed to be repeated forever until all the data has
;;; been printed.  (I think)
;;;
;;; Revision 1.66  2000/08/29 15:55:04  rtoy
;;; o In GET-ARG-DECL, return type INTEGER4 if the arg is a subtype of
;;;   INTEGER4. (So fixnum args are treated as INTEGER4 args for
;;;   declaration purposes instead of a union of fixnum and integer4
;;;   types.)
;;; o In PARSE-READ, don't use literal strings; make them FORTRAN_COMMENT's.
;;;
;;; Revision 1.65  2000/08/29 14:41:41  rtoy
;;; o Remove lots of unused code
;;; o Fix a typo in the Fortra D format parser: the equivalent Lisp format
;;;   is E not D!  Also, explicitly specify the exponent character of "D".
;;; o For the E format, specify an explicit exponent character of "E".
;;;
;;; Revision 1.64  2000/08/27 16:36:07  rtoy
;;; Clean up handling of format statements.  Should handle many more
;;; formats correctly now.
;;;
;;; Revision 1.63  2000/08/10 18:00:37  rtoy
;;; Declarations of the array type for arrays in common blocks were
;;; wrong.  (Oops!)
;;;
;;; Revision 1.62  2000/08/09 22:45:17  rtoy
;;; o In MAKE-DECLARATION, use the LOGICAL type instead of (MEMBER T NIL)
;;;   in declaring arrays.
;;; o MAKE-SPECIAL-VAR-DECL was not returning the right dimensions.
;;;   (After the conversion to 1-D arrays.)
;;; o In MAKE-COMMON-BLOCK-VARS, try to initialize arrays to the right
;;;   size and values.  (Particularly for logical arrays.)
;;; o In MAKE-COMMON-BLOCK-VAR-INIT, we didn't compute the array bounds
;;;   correctly.  Also, if we know the initializer for the common block
;;;   structure element initialized the array (because the dimension was a
;;;   number), don't do it when creating the structure.
;;;
;;; Revision 1.61  2000/08/07 18:55:03  rtoy
;;; GET-ARG-DECL was confused by arrays of strings.  We return the type
;;; ARRAY-STRINGS now.  (Need better names for these types or need to fix
;;; the code so it handles these better!)
;;;
;;; Revision 1.60  2000/08/05 19:23:16  rtoy
;;; Comment out some unreachable code.
;;;
;;; Revision 1.59  2000/08/04 14:20:31  rtoy
;;; Add very rudimentary support for Fortran READ statements.  This means
;;; we just basically call read and assign the result to the (simple)
;;; variable.  We don't even bother to look at the format number or check
;;; the variable type.
;;;
;;; Revision 1.58  2000/08/03 03:39:49  rtoy
;;; The string passed to PARSE-FORMAT-DESCRIPTOR-H can be a character,
;;; digit, or symbol.  These all need to be converted to character.  Make
;;; it so.
;;;
;;; Revision 1.57  2000/08/02 16:26:20  rtoy
;;; The D and E format descriptors should print out a D and E,
;;; respectively.  Thus add PARSE-FORMAT-DESCRIPTOR-D.
;;;
;;; Revision 1.56  2000/08/02 14:42:20  rtoy
;;; o Add support for parsing Hollerith strings in format statements.  Not
;;;   perfect because the preprocessor mangles spaces within the string,
;;;   but we can at least print out the non-space parts of the string.
;;;   Changed PARSE-FORMAT1, DESTRUCT-DESCRIPTOR-LIST, and
;;;   PARES-FORMAT-DESCRIPTOR-H for this.
;;;
;;; o PARSE-FORMAT-DESCRIPTOR-LIST treated D format descriptor as F
;;;   instead of as E.  (Should we distinguish between D and E?)
;;;
;;; o PARSE-FORMAT-DESCRIPTOR-F didn't add the fill character when the
;;;   number is too large to fit in the desired field.
;;;
;;; Revision 1.55  2000/07/30 05:58:48  rtoy
;;; Don't check for new variables inside of multiple-value-bind's that
;;; were created for function calls.  We only need to check in the
;;; arguments of the function call.
;;;
;;; Revision 1.54  2000/07/28 17:05:01  rtoy
;;; o We are in the f2cl package now.
;;; o We convert // to f2cl-//, even in format statements so fix
;;;   FIX-SLASHES to handle this case by replacing f2cl-// with 2 slashes,
;;;   as appropriate.
;;;
;;; Revision 1.53  2000/07/27 16:42:01  rtoy
;;; o We want to be in the CL-USER package, not the USER package.
;;; o Use (typep x 'integer) instead of (fixnump x) in GET-FUN-ARG-TYPE.
;;;
;;; Revision 1.52  2000/07/21 17:47:20  rtoy
;;; o FIXUP-EXPRESSION:  add a case to convert (- N) to just -N, when N is
;;;   a number.
;;;
;;; o MERGE-OPS: incorrectly merged (- (- 3) IT) to (- 3 IT).  I think
;;;   this is fixed now.
;;;
;;; o INSERT-DECLARATIONS: Don't fixup external function refs if an
;;;   intrinsic was actually a variable in the arglist.  (Missed this case
;;;   from before.)
;;;
;;; Revision 1.51  2000/07/20 13:43:00  rtoy
;;; Since all arrays are now actually stored in column-major order in a
;;; 1-dimensional vector, we don't need to transpose the data initializers
;;; anymore.  Replace fortran-transpose with fortran-data-init to
;;; correctly initialize the array.
;;;
;;; Revision 1.50  2000/07/19 22:17:13  rtoy
;;; Remove a print statement inadvertently left in.
;;;
;;; Revision 1.49  2000/07/19 14:04:28  rtoy
;;; o GET-FUN-ARG-TYPE returns a second value to indicate if the arg is an
;;;   array or not.
;;; o More hacking on GET-ARG-DECL.  Should now correctly identify if an
;;;   array is used as a parameter.  Still needs work.
;;; o MAKE_MAKE-ARRAY_STMT creates 1-D arrays for all arrays to support
;;;   Fortran array slicing.
;;; o In MAKE-DECLARATION, declare all arrays as 1-D even if
;;;   multi-dimensional, for supporting Fortran array slicing.
;;;
;;; Revision 1.48  2000/07/18 13:59:23  rtoy
;;; o Left out some double precision intrinsics for getting function
;;;   types.
;;; o Declarations for functions were not quite right.  Make it better, but
;;;   still needs some work.
;;; o The format of *functions-used* has changed.  Do the right thing in
;;;   insert-declarations.
;;;
;;; Revision 1.47  2000/07/14 21:23:37  rtoy
;;; o In GET-FUN-ARG-TYPE, when looking up the type of an expression,
;;;   handle (funcall f ...) by looking up the type of "f" instead of
;;;   looking up the type of "funcall"!
;;;
;;; o In FIX-DO, a goto to the end of the loop would get translated into a
;;;   (return).  This seems wrong, and I don't know why it wants to do
;;;   this.
;;;
;;; Revision 1.46  2000/07/14 15:50:25  rtoy
;;; o When getting the type of an arg, handle the case when the arg is
;;;   actually a call to make-array for array slicing.  Get the type form
;;;   the :element-type.
;;; o If the arg has type fixnum, return integer4 instead.
;;;
;;; Revision 1.45  2000/07/14 14:08:26  rtoy
;;; Honor the user's choice of declaring arrays as array or simple-array.
;;; Except we leave Fortran character strings still declared as
;;; simple-array.
;;;
;;; Revision 1.44  2000/07/14 13:33:26  rtoy
;;; Don't apply external ref fixups if the external function was also a
;;; parameter to the routine.  If we do, then we referring to the wrong
;;; thing!
;;;
;;; Revision 1.43  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-----------------------------------------------------------------------------
