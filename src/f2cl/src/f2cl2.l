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

 (proclaim '(special f-store arg-store exp-store var-store init-store
                  next-user-function-address lis-store end-store step-store 
                  body-store intrinsic_function_names external_function_names
                  subprog_name subprog_stmt_fns)))
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
(defun id-factor (l)
   (cond 
;         ((null l) nil)

; number:
         ((numberp l) l)
; string:
         ((stringp l) l)
; x.xE%x (scientific notation) number - % is used to represent a minus sign:
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

; symbol:
         ((symbolp l) (check-t-and-pi l))
     
;number: 
         ((and (null (cdr l)) (numberp (car l))) (car l))


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
               (member (car l) intrinsic_function_names))
          (cond ((not (null (cddr l))) (princ-reset
'|f2cl error: missing +, *, /, or ^ operator following a function call.| l)))
          (cons (if (eq (car l) 'char) 'fchar (car l)) ;special case intrinsic "char"
                (mapcar #'id-expression
                   (list-split '|,| (cadr l)))))

; array reference:
         ((and (symbolp (car l)) (listp (cadr l)) (vble-is-array-p (car l)))
          (cond ((not (null (cddr l))) (princ-reset
'|f2cl error: missing +, *, /, or ^ operator following a function call.| l)))
          (append (list 'fref (car l)) 
                (mapcar #'id-expression
                   (list-split '|,| (cadr l)))))

; function call:
         ((and (symbolp (car l)) (listp (cadr l)))
          (cond ((member (car l) external_function_names)
                 `(funcall ,(car l) 
                         ,@(mapcar #'id-expression (list-split '|,| (cadr l)))))
;                ((member (car l) subprog_stmt_fns)
;                 `(,(read-from-string
;                       (concatenate 'string (princ-to-string subprog_name)
;                                            (princ-to-string (car l))))
;                   ,@(mapcar #'id-expression (list-split '|,| (cadr l)))))
                (t 
                 `(,(car l) ,@(mapcar #'id-expression (list-split '|,| (cadr l)))))))

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

