; -*- Mode: lisp; package: CL-USER -*-
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
;      check-t-and-pi
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
;;(in-package :user)   

;; functions for setting up varaible declarations and initialisations
(eval-when (compile load eval)
  (proclaim '(special *intrinsic_function_names* *external_function_names*
	      *declared_vbles* *undeclared_vbles* *key_params* *save_vbles*
	      *explicit_vble_decls* *implicit_vble_decls* *common_array_dims*
	      *subprog_common_vars* *program-flag* 
	      *subprog_stmt_fns* *subprog_stmt_fns_bodies*
	      *auto-save-data*
	      *functions-used*
	      *vble-declaration-done*))
)

(defvar *fortran-keywords*
  '(ASSIGN IF THEN GO TO GOTO CALL CONTINUE DO FDO ELSE ELSEIF RETURN
    STOP PRINT READ WRITE BACKSPACE CLOSE ENDFILE INQUIRE
    OPEN REWIND FORMAT AND OR NOT))



#+nil
(defun check_new_vbles (prog_list)
  (format t "check_new ~S~%" prog_list)
  (do ((exprs prog_list (cdr exprs))
       (vble_list nil
		  (append vble_list
			  (cond ((listp (car exprs))
				 (format t "c-n-v 1 = ~S~%" (car exprs))
				 (check_new_vbles (car exprs)))
				((or (not (symbolp (car exprs)))
				     (numberp (car exprs))
				     #+nil(member (car exprs) 
					     '(\, \' + - * / ** // ^ = EQUAL > < >= <= >< FREF F2CL/))
				     (member (car exprs) *fortran-keywords*)
				     (member (car exprs) *intrinsic_function_names*)
				     (member (car exprs) *key_params* :key #'car)
				     (member (car exprs) *external_function_names*)
				     (member (car exprs) *declared_vbles*)
				     (member (car exprs) *undeclared_vbles*)
				     (sym-is-fun-name exprs)
				     (numberp (id-expression (list (car exprs)))))
				 (format t "c-n-v 2 = ~S~%" (car exprs))
				 nil)
				;; special case of error messages
				((eq (car exprs) 'PAUSE)
				 (return prog_list))
				((member (car exprs) 
					     '(\, \' + - * / ** // ^ = EQUAL > < >= <= >< FREF F2CL/))
				 nil)
				(t
				 (format t "adding unknown ~S~%" (car exprs))
				 (setq *undeclared_vbles*
				       (cons (check-t-and-pi (car exprs))
					     *undeclared_vbles*)))))))
      ((null exprs)
       (format t "check-new returns ~S~%" prog_list)
       prog_list)
    (format t "expr = ~S~%" exprs)))

(defun check_new_vbles (exprs)
  (cond ((or (null exprs)
	     (numberp exprs)
	     (typep exprs 'string))
	 nil)
	((symbolp exprs)
	 (cond ((or (member exprs '(\, \' + - * / ** // ^ = EQUAL > < >= <= >< |:|
				    FREF FREF-STRING F2CL/
				    FLOG FSQRT FFLOAT EXPT))
		    (member exprs *fortran-keywords*)
		    (member exprs *intrinsic_function_names*)
		    (member exprs *key_params* :key #'car)
		    (member exprs *external_function_names*)
		    (member exprs *declared_vbles*)
		    (member exprs *undeclared_vbles*)
		    (sym-is-fun-name (list exprs)))
		nil)
	       ((eq exprs 'PAUSE)
		)
	       (t
		(pushnew (check-t-and-pi exprs) *undeclared_vbles*))))
	((listp exprs)
	 (mapc #'check_new_vbles exprs)
	 )
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
#+nil
(defun pretty-decls (decls)
  (let ((type-alist '()))
    (map nil #'(lambda (d)
		 (destructuring-bind (decl (type v-type var))
		     d
		   (declare (ignore decl type))
		   (let ((known (assoc-if #'(lambda (x)
					      (equal x v-type))
					  type-alist)))
		     (if known
			 (setf (cdr known) (cons var (cdr known)))
			 (setf type-alist (cons (list v-type var)
						type-alist))))))
	 decls)
    (when type-alist
      `((declare ,@(mapcar #'(lambda (v)
			       `(type ,@v))
			   type-alist))))))

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

(defun lookup-vble-type (vble)
  ;;(format t "lookup-vble: ~a~%" vble)
  (cond ((member vble *declared_vbles*)
	 ;;(format t "  is declared var~%")
	 ;; First for declared variables
	 (do* ((type-clauses *explicit_vble_decls*
			     (rest type-clauses))
	       (type (member vble (cdar type-clauses) :key #'car)
		     (member vble (cdar type-clauses) :key #'car)))
	      ((or type
		   (null type-clauses))
	       (let ((v-type (caar type-clauses)))
		 (cond ((eq v-type 'array)
			;; Hmm, an array declaration, but no type?
			(cond ((get-implicit-type vble))
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
			v-type)))
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

(defun fortran-contagion (t1 t2)
  (type-of (+ (coerce 0 t1) (coerce 0 t2))))

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
		       (first decl1)))
		    (t
		     var-type)))
	    )))
	((listp arg)
	 ;; We have an expression of some type
	 (let ((fun (first arg)))
	   (cond ((eq 'fref fun)
		  ;; Look up array references
		  (get-fun-arg-type (second arg)))
		 ((member fun '(+ - * f2cl/ expt))
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
	 (type-of arg))))

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

;; Create declarations for all the functions used in the subprogram.
;; We return three items: statement functions, other functions, and
;; functions in the parameter list of the subprogram.
(defun make-fcn-decl (fcn-list arglist)
  (let (stmt-fcns other-fcns arg-fcns)
    (dolist (fcn fcn-list)
      (let* ((fname (first fcn))
	     (type (lookup-vble-type fname))
	     (args (second fcn))
	     (decl `(function ,(get-upgraded-fun-arg-type args) ,type)))
	(cond ((member fname *subprog_stmt_fns*)
	       (push `(ftype ,decl ,fname) stmt-fcns))
	      ((member fname arglist)
	       (push `(type ,decl ,fname) arg-fcns))
	      (t
	       (push `(ftype ,decl ,fname) other-fcns)))))
    (values `((declare ,@(nreverse stmt-fcns)))
	    `((declare ,@(nreverse other-fcns)))
	    `((declare ,@(nreverse arg-fcns))))))

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
	 (destructuring-bind (op next args)
	     expr
	   (setf args (list args))
	   (loop while (and (listp next) (eq op (first next)))
	     do
	     (push (third next) args)
	     (setf next (second next)))
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
		   key-params-decls)

     (setq defun-bit (list (car fort-fun) (cadr fort-fun))
	   arglist (caddr fort-fun)
	   body (cdddr fort-fun))
     (setq *undeclared_vbles* 
	   (set-difference *undeclared_vbles* *subprog_common_vars*))
     ;;(format t "declared_vbles   = ~S~%" *declared_vbles*)
     ;;(format t "undeclared_vbles = ~S~%" *undeclared_vbles*)
     ;;(format t "*functions used  = ~S~%" *functions-used*)
     ;;(format t "external func    = ~S~%" *external_function_names*)
     ;;(format t "*subprog_common_vars* = ~a~%" *subprog_common_vars*)
     (setq special-proclamation
	   (make-special-proclamation *subprog_common_vars*))
     ;;(format t "special-proclamation = ~a~%" special-proclamation)
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
     ;;(format t "struct = ~S~%" common-block-structs)

     (setq special-proclamation common-block-structs)
     ;;(format t "special-proclamation = ~a~%" special-proclamation)

     ;; Get the declarations for all functions
     (multiple-value-setq (stmt-fcn-decls other-fcn-decls arg-fcn-decls)
       (make-fcn-decl *functions-used* arglist))

     (setq formal-arg-decls
	   (pretty-decls
	    (append
	     ;; Declare the any function arguments
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
		     (set-difference arglist *external_function_names*)))))

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
     (when (equal other-fcn-decls '((declare)))
	 (setf other-fcn-decls nil))
     
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
	      ;; (fset (fref array n) v)
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

     ;; If a variable names an function used, delete the variable
     (setf *declared_vbles*
	   (remove-if #'(lambda (v)
			  (member v *functions-used* :key #'car))
		      *declared_vbles*))
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
				  
		       (cond ((eq 'fset (first init))
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
     
     (setq arglist (mapcar #'check-t-and-pi arglist))

     ;;(format t "*key_params* = ~S~%" *key_params*)
     ;;(format t "key-params = ~S~%" key-params)
     (setq key-params (mapcar #'(lambda (x) 
                                  (list (check-t-and-pi (car x)) (cadr x))) 
			      *key_params*))
     (setq key-params-decls (mapcar #'(lambda (param)
				       (make-declaration (car param)))
				   *key_params*))
     ;;(format t "key-params-decl = ~S~%" key-params-decls)
     
     ;;(format t "local-vbles     = ~S~%" local-vbles)
     ;;(format t "vbles-decls     = ~S~%" vble-decls)
     ;;(format t "other-fcn-decls = ~S~%" other-fcn-decls)
     ;;(format t "body            = ~S~%" body)
     (setq prog-bit
	   (if (or *save_vbles* *auto-save-data*)
	       ;; If we have SAVE'd variables, don't put their inits
	       ;; into the function.  Put them in the let outside the function
	       ;; where they belong.
	       (if *subprog_stmt_fns*
		   `(labels ,*subprog_stmt_fns_bodies* ,@stmt-fcn-decls
		     (prog ,local-vbles ,@vble-decls ,@other-fcn-decls ,@body))
		   `(prog ,local-vbles ,@vble-decls ,@other-fcn-decls ,@body))
	       (if *subprog_stmt_fns*
		   `(labels ,*subprog_stmt_fns_bodies* ,@stmt-fcn-decls
		     (prog ,local-vbles ,@vble-decls ,@other-fcn-decls ,@*data-init* ,@body))
		   `(prog ,local-vbles ,@vble-decls ,@other-fcn-decls ,@*data-init* ,@body)))
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
     (return
       (values special-proclamation
	       (when special-proclamation
		   (make-common-block-init *common-blocks* common_var_decls key-params key-params-decls))
	       (if (and (or *save_vbles* *auto-save-data*)
			save-inits)
		   (append
		    `(let* ,key-params
		      ,@key-params-decls
		      (let ,save-inits
			,@saved-decls
			,@*data-init*
			(defun ,(cadr defun-bit) ,arglist
			  ,@formal-arg-decls 
			  ,prog-bit))))
		   (append
		    `(let* ,key-params
		      ,@key-params-decls
		      (defun ,(cadr defun-bit) ,arglist
			,@formal-arg-decls 
			,prog-bit))))))))



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
    (let* ((vble_name (check-t-and-pi vble))
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
	((or (member vble *external_function_names*)
	     (member vble *subprog_stmt_fns*))
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

;; DATA is a sequence of numbers in Fortran column-major order.
;; Create a Lisp array (row-major order) with the data in the correct
;; order.
(defun fortran-transpose (dims data)
  (if (= (length dims) 1)
      data
      (let* ((ndims (length dims))
	     (limits (make-array ndims :initial-contents dims))
	     (cnt (make-array ndims :initial-element 0))
	     (array-result (make-array dims))
	     result)
	(map nil #'(lambda (x)
		     (let ((idx (apply #'array-row-major-index
				       array-result (coerce cnt 'list))))
		       ;; Set the correct element
		       (setf (row-major-aref array-result idx) x)
		       ;; Update the indices in column major order
		       ;; (first index varies fastest)
		       (do ((carry 1)
			    (k 0 (1+ k)))
			   ((or (>= k ndims)
				(zerop carry)))
			 (incf (aref cnt k) carry)
			 (if (>= (aref cnt k) (aref limits k))
			     (setf (aref cnt k) 0)
			     (setf carry 0)))))
	     data)
	;; Now we need to convert the array into a nested structure of
	;; sequences.  We are lazy here.  This does what we want
	(setf result (format nil "~A~%" array-result))
	(values (read-from-string (subseq result (position #\( result))))
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
    
    (cond ((every #'numberp dims)
	   ;; If all of the dimensions are numbers, we don't have to
	   ;; make a list out of them.  This can speed up array
	   ;; creation for smart compilers.
	   (let ((init-val
		  (when init
		    (if (eq 'fill (first init))
			`(:initial-element ,(second init))
			`(:initial-contents
			  ',(fortran-transpose dims
					       (first (rest init))))))))
	     (if (= (length dims) 1)
		 `(make-array ,(first dims) :element-type ',type ,@init-val)
		 `(make-array '(,@dims) :element-type ',type ,@init-val))))
	  (t
	   (list 'make-array 
		 (append '(list) dims)
		 ':element-type (list 'quote type))))))

;; create a labels form for when statement functions are present

(defun make-labels-form (local-vbles vble-decls *data-init* body)
    `(labels ,*subprog_stmt_fns_bodies*
      (prog ,local-vbles ,@vble-decls ,@*data-init* ,@body)))


;; make a declaration for vble which may or may not be a formal arg
;; vble_name is used to replace vbles called T with T_

(defun make-declaration (vble &key vble-is-formal-arg)
 (prog (vble_name type decl1)
   (setq vble_name (check-t-and-pi vble))
  (return
   (cond				;check for vble with two declarations i.e. an array
     ((setf decl1 (vble-declared-twice-p vble *explicit_vble_decls*))
      (destructuring-bind (decl1 decl2)
	  decl1
	(when (eq (car decl2) 'array)
	  (rotatef decl1 decl2))
	(return `(declare (type (simple-array
				 ,(car decl2)
                                 ,(mapcar #'(lambda (x)
                                              (if (every #'realp x)
                                                  (1+ (reduce #'- (reverse x)))
                                                  '*))
                                   (cdadr decl1))
				 )
				 ,vble_name)))))
	  
     ;; Don't need any initialization for statement functions or
     ;; external functions
     ((or (member vble *external_function_names*)
	  (member vble *subprog_stmt_fns*))
      ;;(format t "no declaration needed for ~A~%" vble)
      ;;(format t "stmt fns = ~A~%" *subprog_stmt_fns*)
      ;;(format t "ext fns  = ~A~%" *external_function_names*)
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
	       (if (setq decl (member vble (cdar type-clauses) :key #'car))
                         
		   (return `(declare (type (simple-array 
					    ,(get_array_type (caar decl) 
							     vble-is-formal-arg)
					    ,(mapcar #'(lambda (x) 
							 (if (every #'realp x)
							     (1+ (reduce #'- (reverse x)))
							     '*))
						     (cdar decl))
					    )
				      ,vble_name)))))
	      ((eq type 'LOGICAL)
	       (if (setq decl (member vble (cdar type-clauses) :key #'car))
		   (return
		     (if (cdar decl) 
			 `(declare (type (simple-array (member t nil) ,(cadar decl)) 
				    ,vble_name))
			 `(declare (type (member t nil) ,vble))))))
	      (t (when (setq decl (member vble (cdar type-clauses) :key #'car))
		   (return 
		       (if (cdar decl) 
			   `(declare (type (simple-array 
					    ,type 
					    ,(mapcar #'(lambda (x) 
							 (if (every #'realp x)
							     (1+ (reduce #'- (reverse x)))
							     '*))
						     (cdar decl)))
				      ,vble_name))
			   `(declare (type ,type ,vble_name)))))))))
					; check implicitly declared variable
     ((and (not vble-is-formal-arg) (setq type (get-implicit-type vble)))
      (if (and (listp type) (eq (car type) 'CHARACTER))
	  (make-char-decl (list vble_name) type)
	  `(declare (type ,type ,vble_name))))
					; otherwise use default declaration
     (t (if (default-int-p vble)
	    `(declare (type integer4 ,vble_name))
	    `(declare (type single-float ,vble_name))))))))

(defun make-char-decl (decl type)
   (cond ((null (cdr decl)) ; scalar, no length spec.
          (if (equal (cadr type) '(*))
          `(declare (type (simple-array base-char (*)) ,(car decl)))    
          `(declare (type (simple-array base-char (,(cadr type))) ,(car decl)))))
         ((atom (cadr decl)) ; scalr, length spec.
          `(declare (type (simple-array base-char (,(cadr decl))) ,(car decl))))
         ((cddr decl) ; array with length spec
          `(declare (type (simple-array t (,(caadr decl))) ,(car decl))))
         ((equal (cadr decl) '(*)) ; unspecified length spec
           `(declare (type (simple-array base-char (*)) ,(car decl))))
         (t ; array, no length spec.
           `(declare (type (simple-array t (1)) ,(car decl))))))

(defun make-char-init (decl type)
   (cond ((null (cdr decl)) ; scalar, no length spec.      
          `(make-array '(,(cadr type)) :element-type 'base-char :initial-element #\Space))
         ((atom (cadr decl)) ; scalar, length spec.
          `(make-array '(,(cadr decl)) :element-type 'base-char :initial-element #\Space))
         ((cddr decl) ; array with length spec
	  (let ((dims (remove '|,| (cadr decl))))
	    (if (every #'numberp dims)
		`(make-array ',dims :element-type '(simple-array base-char (,(caddr decl))))
		`(make-array (list ,@dims) :element-type '(simple-array base-char (,(caddr decl)))
		  :initial-element #\Space))))
         ((equal (cadr decl) '(*)) ; unspecified length spec
           "")
         (t ; array, no length spec.
	  (let ((dims (remove '|,| (cadr decl))))
	    (if (every #'numberp dims)
		`(make-array ',dims :element-type '(simple-array base-char ,(cdr type))
		  :initial-element #\Space)
		`(make-array (list ,@dims) :element-type '(simple-array base-char ,(cdr type))))))))


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

; test whether or not a given symbol has been defined as an array
(defun vble-is-array-p (v)
   ; check to see if v is explicitly declared and dimensioned
   (if (member v *declared_vbles*)
          (do ((decls *explicit_vble_decls* (cdr decls)))
              ((null decls) nil)
              (if (do ((vbles (cdar decls) (cdr vbles)))
                      ((null vbles) nil)
                      (if (and (eq v (caar vbles)) (cdar vbles))
                          (return t)))
                  (return t)))
     ; else check if v is a common variable and an array
     (member v *common_array_dims*)))

; test if a symbol is a function call rather than an undeclared vble
; used in check_new_vbles
(defun sym-is-fun-name (expr)
   (and (listp (cadr expr)) (not (null (cdr expr)))))

; given a list of variables names proclaim them special
(defun make-special-proclamation (vars)
   (if vars `(proclaim '(special ,@vars)) nil))

; declare a special (common) variable
(defun make-special-var-decl (v)
   (prog (dim)
    (return
     (cond ; check if v is an array 
           ((setq dim (member v *common_array_dims*))
            `(declare (type (simple-array ,(get_array_type v nil) ',(cadr dim)) 
                            ,(check-t-and-pi v))))
           ; else make ordinary declaration
           (t (make-declaration v))))))

(defun check-t-and-pi (x)
   (cond ((eq x 'T) 't_)
         ((eq x 'PI) 'pi_)
         ((eq x 'NIL) 'nil_)
         (t x)))

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

       ((end-do-p exprs do-label) 
        (cond
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
;body
       (cond ((listp (car exprs))
              (cond ((eq (caar exprs) 'if-then)
                     (multiple-value-setq (subclause exprs) 
                                          (fix-ifthen exprs labels)))
                    ((eq (caar exprs) 'fdo)
                     (multiple-value-setq (subclause exprs) 
                                          (fix-do exprs labels)))
                    ((and (eq (caar exprs) 'go)
                          (label-matches-dolabel-p (cadar exprs) do-label))
                     (setq subclause '(return)
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

; x is of form: '(WRITE (* |,| 8000) |,| J |,| K)
;           or  '(WRITE (*) |,| J |,| K)
;           or  '(WRITE (* |,| *) |,| J |,| K)
(defun parse-write (x)
  ;; check for comma before arguments
  (if (and (third x) (not (eq (third x) '|,|)))
      (setq x (append (list (first x) (second x) '|,|) (cddr x))))
  `((fformat ,(parse_format_dest (first (second x)))
     ,(if (null (second (second x))) 
	  '("~A~%")
	  (get_format_stmt (nthcdr 2 (second x))))
     ,@(mapcar #'parse-output-argument 
	       (list-split '|,| (cdddr x))))))

;; x is of the form: '(PRINT * |,| X |,| Y)
;;               or: '(PRINT 9000 |,| X |,| Y)

(defun parse-print (x)
  `((fformat t
     ,(get_format_stmt (list (second x)))
     ,@(mapcar #'parse-output-argument 
		       (list-split '|,| (cdddr x))))))
     
#+nil
(defmacro fformat (dest cilist args)
   `(if (equal ,cilist '("~A~%")) 
        (and (mapcar #'(lambda (arg) (fformat1 ,dest "~A " arg)) ,args)
             (format ,dest "~%"))
        ;loop through directives, consume arguments
        (do ((directives ,cilist (cdr directives))
             (arglist ,args arglist))
            ((null directives) )
            (cond ((stringp (first directives))
                   (format ,dest (first directives)))
                  (t
                   (fformat1 ,dest
                             (car (first directives)) 
                             (first arglist))
                   (setq arglist (cdr arglist)))))))

#+nil
(progn
(defmacro fformat (dest-lun format-cilist args)
  (let ((dest (gensym))
	(directives (gensym))
	(cilist (gensym))
	(arglist (gensym)))
    `(let ((,dest (lun->stream ,dest-lun))
	   (,cilist ,format-cilist))
      (if (equal ,cilist '("~A~%")) 
	  (and (mapcar #'(lambda (arg) (fformat1 ,dest "~A " arg)) ,args)
	       (format ,dest "~%"))
	  ;; loop through directives, consume arguments
	  (do ((,directives ,cilist (cdr ,directives))
	       (,arglist ,args ,arglist))
	      ((null ,directives) )
            (cond ((stringp (first ,directives))
                   (format ,dest (first ,directives)))
                  (t
                   (fformat1 ,dest
                             (car (first ,directives)) 
                             (first ,arglist))
                   (setq ,arglist (cdr ,arglist)))))))))

(defun fformat1 (dest directive arg)
   (if (arrayp arg)
        (print arg)
        (format dest directive arg)))
)

#+nil
(progn
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
)

;; Handle things like FMT = number
(defun get_format_stmt (label)
  (let ((fmt-num (first label)))
    (cond ((eq fmt-num 'fmt)
	   ;; We have something like "FMT = number"
	   (get_format_stmt (list (third label))))
	  ((or (eq fmt-num '*)
	       (not (numberp fmt-num)))
	   '("~A~%"))
	  (t
	   (do ((lis *format_stmts* (cdr lis)))
	       ((null lis) (error "format stmt ~A not found" fmt-num))
	     (if (equal fmt-num (caar lis))
		 (return (cadar lis))))))))
          
(defun parse_format_dest (dest) 
    (if (eq  dest '*) t dest))

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

(defun parse-format1 (x)
   (setq x (list-split '|,| (fix-slashes x)))
   (do ((desc-lists x (cdr desc-lists))
        (directive) (dl)
        (directive-list nil (append directive-list directive)))
       ((null desc-lists) (append directive-list '("~%")))
      (setq dl (car desc-lists))
      (setq directive 
         (cond ((null dl) nil) ;(list nil))
               ((stringp (car dl)) (list (car dl)))
               ((listp (car dl))
                (append  '("~{") (parse-format1 (car dl)) '("~}")))
               ((and (numberp (car dl)) (listp (cadr dl)))
                (append '("~") (list (car dl)) '("{")
                                 (parse-format1 (cadr dl)) '("~}")))
               ((numberp (car dl)) 
                (parse-format-descriptor-list (car dl) 
                                (destruct-descriptor-list (cadr dl))))
               (t (parse-format-descriptor-list 1 
                          (destruct-descriptor-list (car dl))))))))

;--------------------------------------------------------------------------------

; given a symbol e.g I4 or P2G10.4E2 this function seperates it into numbers and
; symbols eg (#\I 4) or (#\P 2 #\G 10 4 #\E 2)

(defun destruct-descriptor-list (x)
  (prog (descriptor-list char atm)
   (if (not (symbolp x)) (return x))
   (setq x (make-string-input-stream (symbol-name x)))
    loop
   (setq char (peek-char nil x nil 'EOF nil))
   (if (equal char 'EOF) (return (reverse descriptor-list)))
   (cond ((or (digit-char-p char) (member char '(#\+ #\-)))
          (multiple-value-setq (atm x) (read-number-from-stream x)))
         ((eq char #\.) (read-char x) (setq atm nil))
         (t (setq atm (read-char x))))
   (if atm (setq descriptor-list (cons atm descriptor-list)))
   (go loop)))

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
          (parse-format-descriptor-F left right))
         ((pattern-match-and-bind '((> left) #\T #\R (+ right)) x)
          (parse-format-descriptor-TR right))
         ((pattern-match-and-bind '((> left) #\X) x)
          (parse-format-descriptor-X left))
         ((pattern-match-and-bind '((> left) #\S (> right)) x)
          (parse-format-descriptor-S right))
         ((equal (cadr x) '#\/)
          (parse-format-descriptor-/))
         (t (parse-default-format-descriptor x))
         )))) 

(defun fixnum-string (x)
   (princ-to-string x))

(defun parse-format-descriptor-I (a w)
   (prog (directive)
   (if (listp w) (setq w (car w)))  ;ignore .m
   (setq directive (list (concatenate 'string "~" (fixnum-string w) "D")))
   (return 
        (do ((i 1 (1+ i))
             (ret nil (append ret (list directive))))
            ((> i a) ret)))))

;<kP><a>Fw.d -> ~a{~w,d,k,,,[@]F}
(defun parse-format-descriptor-F (left right)
  (prog ((k 0) (a 1) directive)
     (declare (special k a))
     (or (pattern-match-and-bind '((> k) #\P (> a)) left)
         (pattern-match-and-bind '((> k) #\P) left)
         (pattern-match-and-bind '((> a)) left))
     (setq directive
         (list (concatenate 'string 
                 "~" (fixnum-string (car right)) ","     ;w
                     (fixnum-string (cadr right)) ","    ;d
                     (fixnum-string k) ",,"              ;k
                     (if *SP* "@F" "F"))))
    (return
        (do ((i 1 (1+ i))
             (ret nil (append ret (list directive))))
            ((> i a) ret)))))

(defun parse-format-descriptor-E (left right)
  (prog ((k 0) (a 1) w d (e 2) directive)
     (declare (special k a w d e))
     (or (pattern-match-and-bind '((> k) #\P (> a)) left)
         (pattern-match-and-bind '((> k) #\P) left)
         (pattern-match-and-bind '((> a)) left))
     (or (pattern-match-and-bind '((> w) (> d)) right)
         (pattern-match-and-bind '((> w) (> d) #\E (> e)) right))
     (setq directive 
         (list (concatenate 'string
			    "~" (fixnum-string w) ","
			    (fixnum-string d) ","
			    (fixnum-string e) ","
			    (fixnum-string k) ",'*"
			    (if *SP* "@E" "E"))))
    (return
      (do ((i 1 (1+ i))
	   (ret nil (append ret (list directive))))
	  ((> i a) ret)))))

(defun parse-format-descriptor-G (left right)
  (prog ((k 0) (a 1) w d (e 2) directive)
     (declare (special k a w d e))
     (or (pattern-match-and-bind '((> k) #\P (> a)) left)
         (pattern-match-and-bind '((> k) #\P) left)
         (pattern-match-and-bind '((> a)) left))
     (or (pattern-match-and-bind '((> w) (> d)) right)
         (pattern-match-and-bind '((> w) (> d) #\E (> e)) right))
     (setq directive 
         (list (concatenate 'string
                            "~" (fixnum-string w) ","
                            (fixnum-string d) ","
                            (fixnum-string e) ","
                            (fixnum-string k) ",'*"
                            (if *SP* "@G" "G"))))
    (return
        (do ((i 1 (1+ i))
             (ret nil (append ret (list directive))))
            ((> i a) ret)))))

(defun parse-format-descriptor-/ ()
   '("~%"))

(defun parse-format-descriptor-TR (s)
   (list (concatenate 'string "~" (fixnum-string s) "@T")))


(defun parse-format-descriptor-X (w)
   (list (concatenate 'string "~" (fixnum-string w) "@T")))

(defun parse-format-descriptor-S (right)
   (setq *SP* (if (eq right #\P) t nil))
   "")

#+nil
(defun parse-format-descriptor-H (width string)
   (declare (ignore width string))
   (list "*Hollerith*"))

(defun parse-format-descriptor-H (width string)
  (format t "Hollerith:  ~a ~a~%" width string)
  (list "*Hollerith*"))

(defun parse-default-format-descriptor (x)
   (list (do ((i 1 (1+ i))
              (ret nil (append ret '("~A"))))
             ((> i (car x)) ret))))

; given a list containing /'s put commas on either side
; e.g. (x / y) -> ( |,| / |,| y) 
;      (/ x) -> (/ |,| x)
(defun fix-slashes (x)
   (do ((lis x (cdr lis))
        (ret nil 
             (append ret (if (eq (car lis) '/) 
                             (cond ((and ret (cdr lis)) '(|,| / |,|))
                                   ((cdr lis) '(/ |,|))
                                   (t '(|,| /)))
                             (list (car lis))))))
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
				       (subtypep (first var-type) 'array))
				  (mapcar #'(lambda (x)
					      (declare (ignore x))
					      0)
					  (lookup-array-bounds var)))
				 (t nil)))
		     (var-init (cond ((and (listp var-type)
					   (subtypep (first var-type) 'array))
				      `(make-array ',dims :element-type ',(second var-type)))
				     (t
				      (coerce 0 var-type)))))
		;; Initialize the slot with a 0 of the appropriate
		;; type (for scalars) or a zero element array of the
		;; appropriate dimensions for array slots.
		(if dims
		    `(,var ,var-init :type (simple-array ,(second var-type) ,(subst '* 0 dims)))
		    `(,var ,var-init :type ,var-type))))
	  varlist))

			     
;; Create a structure for the given common blocks.
(defun make-common-block-structure (common_var_decls)
  (let ((res '()))
    (maphash #'(lambda (key varlist)
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
			   (- (second bound) (first bound))
			   `(- ,(second bound) ,(first bound))))
		   bounds)))
    (let ((initializer '()))
      (dolist (var varlist)
	(let* ((decl (find var (rest common_var_decls)
			   :key #'third))
	       (var-type (if decl (second decl) nil))
	       (dims (cond ((and (listp var-type)
				 (subtypep (first var-type) 'array))
			    (fixup-bounds (lookup-array-bounds var)))
			   (t nil)))
	       (var-init (cond ((and (listp var-type)
				     (subtypep (first var-type) 'array))
				`(make-array (list ,@dims) :element-type ',(second var-type)))
			       (t
				(coerce 0 var-type)))))
	  (when dims
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
;-----------------------------------------------------------------------------
; end of f2cl5.l
