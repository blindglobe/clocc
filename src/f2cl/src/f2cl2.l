; f2cl2.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 92-95 - all rights reserved;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; functions
;	id-expression 
;	parse
;	expression-preparse
;	inner-preparse
;	id-assignment
;	comma-sep-listp  - within scope of restrict non-local
;	symbol-listp
;	id-term
;	id-factor 
;       id-factor1
;	gen-list-split
;       id-expression-list-ifthen
;	id-array-store - check args eval to integers in range at eval time
;	restore-negative-numbers
;------------------------------------------------------------------------------
(in-package :user)
(eval-when (compile load eval)
  (proclaim '(optimize (speed 3) (safety 1)))

  (proclaim '(special *intrinsic_function_names* *external_function_names*
	      *subprog_name* *subprog_stmt_fns*
	      *functions-used*)))
;-----------------------------------------------------------------------------
; subst-splice substitutes the list b for the atom a in the list c via splicing
(defun subst-splice (b a c) 
  (cond ((atom c) c)  
        ((equal (car c) a) (append b (subst-splice b a (cdr c))))
        (t (cons (subst-splice b a (car c)) (subst-splice b a (cdr c))))))
;-----------------------------------------------------------------------------

(defun extract (alpha beta)
  (prog (retlist)
    (setq retlist nil)
    loop
    (cond ((null beta) (return retlist))
          ((member (car beta) alpha)
           (setq retlist (append1 retlist (car beta))
                 beta (cdr beta))
           (go loop))
          (t (setq beta (cdr beta))
           (go loop)))))

(defun list-itp (lis signs)
 (cond 
      ((member nil lis)
       (princ-reset '|f2cl error: failure to parse an expression|)))
  (prog (ret)
    (cond ((equal signs nil) (return (car lis))))
    (setq signs (subst 'f2cl/ '/ signs))
    (setq ret (list (car signs) (car lis) (cadr lis)))
    (setq lis (cdr lis))
    loop
    (setq lis (cdr lis))
    (setq signs (cdr signs))
    (cond ((null (car signs)) (return ret))
          (t (setq ret (list (car signs) ret (car lis)))
             (go loop)))))

(defun tpl-subst (x y lis)
   (cond 
         ((null lis) nil)
         ((equal y (car lis))
           `(,x ,@(tpl-subst x y (cdr lis))))
         (t `(,(car lis) ,@(tpl-subst x y (cdr lis))))))

(defun tpl-subpair (lx ly lis)
   (prog (elx ely restx resty ret)
    (setq elx (car lx) ely (car ly) restx (cdr lx) resty (cdr ly) ret lis)
    loop
    (setq ret (tpl-subst elx ely ret))
    (cond ((null restx) (return ret)))
    (setq elx (car restx) ely (car resty) restx (cdr restx) resty (cdr resty))
    (go loop)))


;----------------------------------------------------------------------------- 
(defun symbol-listp (x) (and (listp x) (symbolp (car x)) (null (cdr x))))
;------------------------------------------------------------------------------
(defun check-tags-go (x)
  (prog (tags gos)
   (setq tags (mapcar #'car (delete nil (mapcar #'(lambda (y) (cond ((symbol-listp y) y)
                                                     (t nil))) x))))
   (setq gos (mapcar #'car (find-gos x)))

   (return
    (cond ((eq (length tags) (length (unique-elements (append gos tags)))) t)
          (t nil))))) 
;------------------------------------------------------------------------------
(defun find-gos (x)
 (cond ((atom x) nil)
       ((eq (car x) 'go) 
        (cond ((null (symbol-listp (cadr x))) 
               (princ-reset
'|f2cl error: the argument to a program "go" must be a valid tag.
Tag being parsed:| (cadr x))))
        (list (cadr x)))
       (t (append (find-gos (car x)) (find-gos (cdr x))))))
    
;------------------------------------------------------------------------------

; this didn't solve the fortify problem:
 (defun restore-negative-numbers (x)
  (prog (rest ret next)
   (cond ((not (memberq '- x)) (return x)))
   (setq rest x)
   loop
   (cond ((null rest) (return (reverse ret))))
   (setq next (car rest) rest (cdr rest))
   (cond ((and (eq '- next) (numberp (car rest)))
          (setq ret (cons (minus (car rest)) (cons '+ ret))
                    rest (cdr rest)))
         (t (setq ret (cons next ret))))
     (go loop)))
;----------------------------------------------------------------------------- 

(defun id-expression (larg)
  (prog (x y l)
     (setq l (subst-splice '(+ -1 *) '- larg))
     (cond ((eq (car l) '+) (setq l (cdr l)))
           ((eq (car l) '-) (setq l (cons 0 l))))

     (setq x (list-split-multi '+ l))
     (setq x (list x (extract '(+ -) l)))
     (setq y (list-itp (mapcar #'id-term (car x)) 
		       (cadr x)))
     (return y)))

;-----------------------------------------------------------------------------

(defun id-term (l)
 (prog (x)
     (setq x (apply #'append (mapcar #'(lambda (x) (list-split-bin '/ x))
                                    (list-split-multi '* l))))
     (setq x (list x (extract '(* /) l)))
     (if (and (equal (caar x) '(-1)) (equal (cadr x) '(*)))
         (return `(- ,(id-factor1 (cadar x)))))
     (return (list-itp (mapcar #'id-factor1 (car x)) 
                      (cadr x)))))

;-----------------------------------------------------------------------------
(defun id-factor1 (l)
 (prog (x)
     (setq x (list-split-bin '^ l))
     (setq x (list x (extract '(^) l)))
     (return (list-itp (mapcar #'id-factor (car x)) 
                      (subst 'expt  '^ (cadr x))))))

;------------------------------------------------------------------------------
(defun lookup-array-bounds (v)
  (map nil #'(lambda (e)
	       (let ((res (find v (rest e) :key #'car)))
		 (when (and res
			    (not (null (rest res))))
		   ;; If this entry has dimensions, we're done!
		   (return-from lookup-array-bounds (rest res)))))
       *explicit_vble_decls*)
  (error "Cannot find array bounds for ~S!" v))


(defun update-called-functions-list (name args)
  (let ((fcn (member name *functions-used* :key #'car)))
    (cond (fcn
	   ;; Found it.  Let's add the arguments to the arg list
	   (setf (second (first fcn))
		 (mapcar #'cons
			 args (second (first fcn)))))
	  (t
	   ;; A new function.  Let's add it and the args to the list.
	   (push (list name (mapcar #'list args)) *functions-used*)))))

(defun id-factor (l)
  (cond 
    ;;         ((null l) nil)

    ;; number:
    ((numberp l) l)
    ;; string:
    ((stringp l) l)
    ;; x.xE%x (scientific notation) number - % is used to represent a minus sign:
    ((and (symbolp l)
	  (string-equal 
	   (string-trim '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.) 
			(string l))
	   "E%"))
     (setq l
	   (read-from-string
	    (concatenate 'string
			 (string-right-trim 
			  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\%) l)
			 "-"
			 (string-left-trim '(#\%)
					   (string-left-trim 
					    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\E) l)))))
     l)
    ;; x.xD%x, like above
    ((and (symbolp l)
	  (string-equal 
	   (string-trim '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.) 
			(string l))
	   "D%"))
     (setq l
	   (read-from-string
	    (concatenate 'string
			 (string-right-trim 
			  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\%) l)
			 "-"
			 (string-left-trim '(#\%)
					   (string-left-trim 
					    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\D) l)))))
     l)

					; symbol:
    ((symbolp l)
     (check-t-and-pi l))
     
					;number: 
    ((and (null (cdr l)) (numberp (car l)))
     (car l))


					; bracketed symbol or string:
    ((and (null (cdr l)) (or (symbolp (car l)) (stringp (car l))))
     (id-factor (car l)))

					; function of no variables:
    ((and (symbolp (car l)) (eq nil (cadr l)))
     (list (car l)))

					; boolean expression:
    ((intersection l '(NOT EQUAL AND OR < > <= >= ><))
     (id-logical l))

					; intrinsic function call:
    ((and (symbolp (car l)) 
	  (listp (cadr l))
	  (member (car l) *intrinsic_function_names*))
     (cond ((not (null (cddr l)))
	    (princ-reset
	     '|f2cl error: missing +, *, /, or ^ operator following a function call.| l)))
     (cons (case (car l)
	     ;; Handle some special cases for intrinsic functions
	     (char 'fchar)
	     (sqrt 'fsqrt)
	     (log 'flog)
	     (float 'ffloat)
	     (t (car l)))
	   (mapcar #'id-expression
		   (list-split '|,| (cadr l)))))
    ;; array reference:
    ((and (symbolp (car l))
	  (listp (cadr l))
	  (vble-is-array-p (check-t-and-pi (car l))))
     (when (cddr l)
       (princ-reset
	'|f2cl error: missing +, *, /, or ^ operator following a function call.| l))

     ;; Given the array name and indices, look up the the bounds for
     ;; this array and construct the appropriate expression to access
     ;; the array correctly.
     (let* ((vname (check-t-and-pi (car l)))
	    (indices (mapcar #'id-expression
			     (list-split '|,| (cadr l))))
	    (bounds (lookup-array-bounds vname)))

       ;; If all of the lower bounds are 1, don't need to pass that
       ;; on.  The fref macro already handles this situation.  Doesn't
       ;; affect the behavior, but makes the Lisp code look a bit
       ;; cleaner.
       (append (list 'fref vname)
	       (list indices)
	       (let ((blist (mapcar #'car bounds)))
		 (unless (and blist
			      (every #'(lambda (x)
					 (= x 1))
				     blist))
		   (list (mapcar #'car bounds)))))))
    
    ;; function call:
    ((and (symbolp (car l))
	  (listp (cadr l)))
     ;; Should we check for T and PI here?
     (let ((fname (car l))
	   (args (mapcar #'id-expression (list-split '|,| (cadr l)))))
       ;; Save the function name and number of arguments so we can
       ;; declare the function appropriately.  We also save the
       ;; argument list so we can determine later the types of each
       ;; parameter, if we really want to.

       (update-called-functions-list fname args)
       (cond ((and (member fname *external_function_names*)
		   (member fname *subprog-arglist*))
	      ;; We want to use funcall only if the external function
	      ;; was passed in as a parameter.  If not, then we don't
	      ;; need to funcall it.  The user was just telling us that
	      ;; it was external function instead an intrinsic.
	      `(funcall ,fname ,@args)) 
	     (t 
	      `(,fname ,@args)))))

					; expression:
    ((and (listp (car l)) (null (cdr l)))
     (id-expression (car l)))

					; complex number:
    ((and (eq (length l) 3) (eq (second l) '|,|))
     (list 'complex (id-factor (first l)) (id-factor (third l))))

					; error dropping out the bottom:
    (t 
     (princ-reset '|f2cl syntax error| l))
    ))

;-------------------------------------------------------------------------------

; (a0 |,| a1 |,| a2 ..... |,| an) is checked:
(defun comma-sep-symb-listp (l)
 (prog (rest)
  (cond
   ((and 
         (listp l) 
         (neq '|,| (car l))
         (every #'symbolp l) 
         (evenp (length (cdr l)))))
   (t (return nil)))
  (setq rest (cdr l))
  loop
  (cond ((null rest) (return t)))
  (cond ((or (neq (car rest) '|,|)
             (eq (cadr rest) '|,|))
          (princ-reset
'|f2cl syntax error: failure to parse the parameter list of a function 
definition.| l) ))
  (setq rest (cddr rest))
  (go loop)))

;------------------------------------------------------------------------------

; (a0 |,| a1 |,| a2 ..... |,| an) is checked where a0 are anything :
(defun comma-sep-listp (l)
 (prog (rest)
  (cond
   ((and 
         (listp l) 
         (neq (car l) '|,|)
         (evenp (length (cdr l)))))
   (t (return nil)))
  (setq rest (cdr l))
  loop
  (cond ((null rest) (return t)))
  (cond ((or (neq (car rest) '|,|)
             (eq (cadr rest) '|,|))
          (princ-reset 
'|f2cl syntax error: failure to parse a list.| l) ))
  (setq rest (cddr rest))
  (go loop)))

;------------------------------------------------------------------------------
; end of f2cl2.l

