;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools-*-
(in-package :ytools)

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.


(cl:eval-when (:compile-toplevel :load-toplevel :execute)
   (shadow '(#:defun #:defmacro #:eval-when)))

;;; These have to be separate because if the following is
;;; *read* before the 'shadow' is *evaluated*, then there will
;;; be no symbol with name "defun" internal to, or importable into,
;;; the :ytools package.
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
   (export '(ytools-readtable* _ defun defmacro \\
	     make-eq-hash-table table-entry href walk-table clear-table
	     make-Array make-Symbol
	     Array-dimension Array-dimensions
	     is-Vector is-Array is-Symbol Symbol-name Symbol-plist
	     is-Keyword is-String memq assq nodup =<
	     is-Pair is-cons list-copy tuple is-Char is-Integer is-Number
	     is-Float is-Single-float is-Double-float
	     is-Fixnum is-Ratio is-sublist is-whitespace
	     is-Stream list->values values->list lastelt len string-length
	     build-symbol symno* true false keyword-package*
	     eval-when alist-entry alist-entry-set alref. alref condense
	     include-if series car-eq take drop occurs-in empty-list
	     on-list off-list --)))

;;;;(eval-when (:compile-toplevel)
;;;;   (format t "shadow-export done"))

#-:excl
(cl:defmacro with-packages-unlocked (&body body)
  `(progn ,@body))
#+:excl
(cl:defmacro with-packages-unlocked (&body body)
  `(excl:without-package-locks ,@body))

(cl:defmacro eval-when (situations &body b)
   (cond ((member ':slurp-toplevel situations)
	  `(progn (eval-when-slurping ,@b)
		  (cl:eval-when ,(remove ':slurp-toplevel situations)
		     ,@b)))
	 (t
	  `(cl:eval-when ,situations ,@b))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)

(cl:defun ignore-smooth (args body)
   (multiple-value-bind (newargs to-be-ignored)
			(underscores-elim args)
      (let ((realbod (ignore-convert body)))
	 (cond ((null to-be-ignored)
		(values args realbod))
	       (t
		(values newargs `((declare (cl:ignore ,@to-be-ignored))
				  ,@realbod)))))))

(cl:defun underscores-elim (args)
   (let ((realargs '())
	 (new-ignores '()))
      (dolist (a args)
	 (cond ((eq a '_)
		(let ((new (gensym)))
		   (push new new-ignores)
		   (push new realargs)))
	       (t
		(push a realargs))))
      (values (reverse realargs)
	      new-ignores)))

(cl:defun ignore-convert (body)
   (cond ((and (not (null body))
	       (consp (car body))
	       (eq (caar body) 'ignore))
	  `((declare (cl:ignore ,@(cdar body))) ,@(cdr body)))
	 (t body)   ))

)

(cl:defmacro defun (name args &rest body)
   (multiple-value-bind (args body)
			(ignore-smooth args body)
      `(cl:defun ,name ,args ,@body)))

(cl:defmacro defmacro (name args &rest body)
   (multiple-value-bind (args body)
			(ignore-smooth args body)
      `(cl:defmacro ,name ,args ,@body)   ))

(cl:defmacro \\ (args &rest body)
   (multiple-value-bind (args body)
			(ignore-smooth args body)
      `(function (lambda ,args ,@body))   ))

(defmacro eval-when-slurping (&rest _) ''no-op)

(defconstant true t)
(defconstant false nil)
(defconstant nil* 'nil)

(defparameter keyword-package* (find-package 'keyword))

;;; Each entry is of form (char global-handler -local-handlers-)
;;; The current readtable is looked up in the local-handlers (itself an
;;; alist).  If no entry, use global.  If no global, '!' should be taken
;;; as an ordinary character.
(defvar excl-handlers* '())

(defun excl-reader (srm ch)
   (setq ch (peek-char nil srm nil nil))
   (cond ((not ch)
	  ;; end of file
	  (intern "!"))
	 (t
	  (labels ((nonmacro ()
		      (cond ((member ch '(#\space #\tab #\newline #\return #\linefeed
					  #\page #\( #\))
				     :test #'char=)
			     (intern "!"))
			    (t
			     ;; if any problems here, could try UNREAD-CHAR + (VALUES)
			     (values
			      (intern (concatenate 'string
					    "!" (string (read srm t nil t)))))))))
	     (let ((e (assoc ch excl-handlers* :test #'eq)))
		(cond (e
		       (let ((r (assq *readtable* (cddr e))))
			  (cond (r
				 (read-char srm)
				 (funcall (cadr r) srm ch))
				((cadr e)
				 (read-char srm)
				 (funcall (cadr e) srm ch))
				(t (nonmacro)))))
		      (t (nonmacro))))))))

(set-macro-character #\! #'excl-reader ytools-readtable*)

;;;;(cl:eval-when (:compile-toplevel :load-toplevel)
;;;;   #+cmu (ignore-errors
;;;;		(make-dispatch-macro-character #\! t ytools-readtable*))
;;;;   #-cmu (make-dispatch-macro-character #\! t ytools-readtable*))

;;;;(defun treat-excl-as-char (srm ch param)
;;;;   (let ((pc (peek-char nil srm)))
;;;;      (cond ((or (is-whitespace pc)
;;;;		 (eq pc '#\()
;;;;		 (eq pc '#\)))
;;;;	     (intern (format nil "!~a~a" (or param "") ch)))
;;;;	    ((eq pc '#\?)
;;;;	     (read-char srm)
;;;;	     '|!=?|)
;;;;	    (t
;;;;	     ;;;(unread-char ch)
;;;;	     (let ((sym (read srm)))
;;;;		(cond ((symbolp sym)
;;;;		       (intern (format nil "!~a~a~a"
;;;;				       (or param "") ch (symbol-name sym))))
;;;;		      (t
;;;;		       (error "Illegal '!' construct !~a~a~s"
;;;;			      (or param sym) ch sym))))))))

(eval-when (:compile-toplevel :load-toplevel)

; Useful macro in backquote. ,@(INCLUDE-IF test -stuff-) includes stuff if
; test is non-false.  If stuff is omitted, it defaults to test itself.
(defmacro include-if (tst &rest stuff)
   (cond ((null stuff)
	  `(list-if-not-false ,tst))
	 (t
	  `(cond (,tst (list ,@stuff)) (t '())   ))))
)

(cl:defmacro subr-synonym (syn subr &optional setfable)
   `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
       (cond ((not (eq ',syn ',subr))
	      ;; If they're eq, it's probably because they 
	      ;; looked different when input, as:
	      ;; (subr-synonym 'make-Pathname 'make-pathname)
	      (define-subr-synonym ',syn ',subr)
	      (define-compiler-macro ,syn (&rest args)
		 `(,',subr ,@args))
	      ,@(include-if setfable
		   `(defun (setf ,syn) (v x) (setf (,subr x) v)))))))
(cl:eval-when (:compile-toplevel :load-toplevel)

(defvar subr-synonyms* '())
		  
(cl:defun define-subr-synonym (syn subr)
	  (setf (symbol-function syn) (symbol-function subr))
	  (let ((p (assoc syn subr-synonyms* :test #'eq)))
	     (cond ((not p)
		    (setq subr-synonyms*
		          (cons (list syn subr) subr-synonyms*)))
		   (t
		    (setf (cadr p) subr)))))

)

(cl:defun make-eq-hash-table (&rest args)
   (apply #'make-hash-table :test #'eq args))

(cl:defun table-entry (htab key) (gethash key htab))

(defsetf table-entry (htab key) (x) `(setf (gethash ,key ,htab) ,x))

(defmacro href (htab^ key^ &optional (default^ 'false))
   (let ((valvar (gensym)) (presvar (gensym)))
      `(multiple-value-bind (,valvar ,presvar)
			    (gethash ,key^ ,htab^)
	  (cond (,presvar ,valvar)
		(t ,default^)))))

(defsetf href (htab^ key^ &optional default^) (newval^)
   (declare (ignore default^))
   `(setf (gethash ,key^ ,htab^) ,newval^))

(subr-synonym is-Hash-table hash-table-p)
(subr-synonym walk-table    maphash)
(subr-synonym clear-table  clrhash)

(subr-synonym is-Symbol symbolp)
(subr-synonym Symbol-name symbol-name)
(subr-synonym Symbol-plist symbol-plist)

(subr-synonym is-Keyword keywordp)

(subr-synonym is-String stringp)

(cl:defun memq (x l) (member x l :test #'eq))
(cl:defun assq (k al) (assoc k al :test #'eq)   )

(defun nodup (l &key (test #'eql))
   (declare (type list l))
   (do ((tl (reverse l) (cdr tl))
	(res '()))
       ((null tl) res)
     (cond ((not (member (car tl) (cdr tl) :test test))
	    (setq res (cons (car tl) res))))))

(subr-synonym =< <=)

(subr-synonym make-Array make-array)
(subr-synonym make-Symbol make-symbol)

(subr-synonym is-Array arrayp)
(subr-synonym is-Pair consp)
(subr-synonym is-Vector vectorp)
(subr-synonym is-cons consp)
(subr-synonym is-sublist subsetp)
(subr-synonym list-copy copy-list)
(subr-synonym tuple list)
(subr-synonym is-Char characterp)
(subr-synonym is-Integer integerp)
(subr-synonym is-Number numberp)
(subr-synonym is-Float floatp)

(defun is-Single-float (x) (typep x 'single-float))
(defun is-Double-float (x) (typep x 'double-float))

(defun is-Fixnum (n)
   (and (integerp n)
	(<= n most-positive-fixnum)
	(>= n most-negative-fixnum)))	

(defun is-Ratio (x) (typep x 'ratio))

(subr-synonym is-Stream streamp)
(subr-synonym list->values values-list)
(subr-synonym values->list multiple-value-list)

(subr-synonym Array-dimension array-dimension)
(subr-synonym Array-dimensions array-dimensions)

(defun lastelt (l) (car (last l)))

(defun len (l) (length (the list l))   )
(defun string-length (s) (length (the string s)))

;; Macro for building new symbols.
;; (BUILD-SYMBOL [(:package <p>)] -specs-) creates a symbol
;; whose print name is built out of specs.  An atomic or string spec is
;; concatenated in.  A spec of the form (:< e1 e2 ...) has each eI evaluated
;; and its characters concatenated in.  A spec of the form (:++ e) increments e
;; and concatenates in its characters.  Anything else is supposed to
;; evaluate to a list of characters, which are concatenated in.
;; Example: If A = FOO, B = (1 2 3), and C = (#\B \#A \#R), then
;; (build-symbol (:< A) (:++ (CAR B)) (CDR C) "< >") is FOO2AR</ >, and
;; B becomes (2 2 3).
(defmacro build-symbol (&rest l)
   (let ((p (find-if #'(lambda (x) (and (consp x) (eq (car x) ':package)))
		     l)))
      (cond (p
	     (setq l (remove p l))))
      (let ((pkg (cond ((memq (cadr p) '(false nil))
			false)
		       (t `(find-package ',(cadr p))))))
	 (cond (p
		(cond (pkg
		       `(values (intern ,(symstuff l) ,pkg)))
		      (t
		       `(make-symbol ,(symstuff l)))))
	       (t
		`(values (intern ,(symstuff l))))))))

(cl:eval-when (:compile-toplevel :load-toplevel)
(defun symstuff (l)
   `(concatenate 'string
     ,@(mapcan
           (\\ (x)  (cond ((stringp x)
			   (cond ((and (find-if #'alpha-char-p x)
				       (not (find-if #'is-whitespace
						     x)))
				  (format *error-output*
				     "Warning: alphabetic string ~s in ~
                                      build-symbol;~
                                    ~%  more portable to use symbols.~
                                    ~%  to make this warning go away ~
                                      replace with :~a)"
				     x x)))
			   (list `',x))
                          ((symbolp x)
                           (list `',(symbol-name x)))
                          ((atom x)
                           (list `(coerce-to-string ,x)))
                          ((memq (car x) '(:< <))
			   (mapcar (\\ (y) `(coerce-to-string ,y)   )
				   (cdr x)))
                          ((memq (car x) '(:++ ++))
                           (list `(princ-to-string (incf ,(cadr x)))))
                          (t (list `(coerce-to-string ,x)))   ))
	   l)   ))

(defvar symno* 0) ;; General-purpose counter for built symbols

(defun is-whitespace (ch)
     (or (char= ch #\space)
	 (not (graphic-char-p ch)))   )

)

(defun coerce-to-string (x)
   (cond ((stringp x) x)
	 ((characterp x) (string x))
	 ((numberp x)
	  (princ-to-string x))
	 ((symbolp x)
	  (symbol-name x))
	 (t (format nil "~a" x))   ))

(defmacro alist-entry (^x ^l &optional (^initial 'false))
   `(alist-entry-def ,^x ,^l
		     ,(cond ((eq ^initial 'false) 'false)
			    (t `(\\ () ,^initial)))))


(defun alist-entry-def (x a initializer)
   (let ((p (assq x a)))
      (cond (p (cadr p))
	    (t (and initializer (funcall initializer))))))

(define-setf-expander alist-entry-def (x l initializer)
   (multiple-value-bind (ltemps lvals lstores lset lacc)
                        (get-setf-expansion l)
      (let ((xvar (gensym)) (newvar (gensym)) (lvar (gensym))
            (storevar (car lstores)))
         (values `(,xvar ,@ltemps ,lvar)
                 `(,x ,@lvals ,lacc)
                 `(,newvar)
                 `(let ((p (assq ,xvar ,lvar)))
                     (cond ((not p)
                            (let ((,storevar (cons (tuple ,xvar ,newvar)
                                                   ,lvar)))
                                ,lset
				,newvar))
	                   (t
	                    (setf (cadr p) ,newvar)))
                      ,newvar)
                 `(alist-entry-def ,xvar ,lvar ,initializer)))))

(defmacro alref. (alist^ key^ &optional (default^ 'false))
   (let ((entryvar (gensym)))
      `(let ((,entryvar (assq ,key^ ,alist^)))
	  (cond (,entryvar (cdr ,entryvar))
		(t ,default^)))))

(define-setf-expander alref. (alist^ key^ &optional default^)
   (multiple-value-bind (altemps alvals alstores alist-set alist-acc)
                        (get-setf-expansion alist^)
      (let ((keyvar (gensym)) (newvar (gensym)) (alist-var (gensym))
            (storevar (car alstores)))
         (values `(,keyvar ,@altemps ,alist-var)
                 `(,key^ ,@alvals ,alist-acc)
                 `(,newvar)
                 `(let ((p (assq ,keyvar ,alist-var)))
                     (cond ((not p)
                            (let ((,storevar (cons (cons ,keyvar ,newvar)
                                                   ,alist-var)))
                                ,alist-set
				,newvar))
	                   (t
	                    (setf (cdr p) ,newvar)))
                      ,newvar)
                 `(alref. ,alist-var ,keyvar ,default^)))))

(defmacro alref (alist^ key^ &optional (default^ 'false))
   (let ((entryvar (gensym)))
      `(let ((,entryvar (assq ,key^ ,alist^)))
	  (cond (,entryvar (cadr ,entryvar))
		(t ,default^)))))

(define-setf-expander alref (alist^ key^ &optional default^)
   (multiple-value-bind (altemps alvals alstores alist-set alist-acc)
                        (get-setf-expansion alist^)
      (let ((keyvar (gensym)) (newvar (gensym)) (alist-var (gensym))
            (storevar (car alstores)))
         (values `(,keyvar ,@altemps ,alist-var)
                 `(,key^ ,@alvals ,alist-acc)
                 `(,newvar)
                 `(let ((p (assq ,keyvar ,alist-var)))
                     (cond ((not p)
                            (let ((,storevar (cons (tuple ,keyvar ,newvar)
                                                   ,alist-var)))
                                ,alist-set
				,newvar))
	                   (t
	                    (setf (cadr p) ,newvar)))
                      ,newvar)
                 `(alref ,alist-var ,keyvar ,default^)))))

;;;;(set-dispatch-macro-character #\! #\( #'lpar-exclmac ytools-readtable*)

(defmacro def-excl-dispatch (char args &body b)
   (multiple-value-bind (b rt)
                        (let ((tl (member ':readtable b)))
			   (cond (tl
				  (values `(,@(ldiff b tl) ,@(cddr tl))
					  (cadr tl)))
				 (t
				  (values b nil))))
      (let ((fun-name (intern (format nil "excl-handler-~a" char))))
	 `(progn
	     (defun ,fun-name ,args
		,@b)
	     (let ((e (assq ',char excl-handlers*)))
		(cond ((not e)
		       (setq e (tuple ',char nil))
		       (setq excl-handlers* (cons e excl-handlers*))))
		,(cond (rt
			`(let ((rte (assq ,rt (cddr e))))
			    (cond ((not rte)
				   (setq rte (tuple ,rt nil))
				   (setf (cddr e) (cons rte (cddr e)))))
			    (setf (cadr rte) #',fun-name)))
		       (t
			`(setf (cadr e) #',fun-name))))))))

(def-excl-dispatch #\( (srm ch)
   (setq ch (peek-char t srm))
   (cond ((char= ch '#\) )
	  (read-char srm)
	  (list 'empty-list false))
	 (t
	  (let ((thing (read srm)))
	     (setq ch (peek-char t srm))
	     (cond ((eq ch '#\) )
		    (read-char srm)
		    (list 'empty-list thing))
		   (t
		    (cerror "!(~s...) has too much stuff before close paren"
			    thing)))))))


(set-pprint-dispatch
    '(cons (eql empty-list))
    (\\ (srm el)
       (cond ((= (length el) 2)
	      (cond ((cadr el) (format srm "!(~s)" (cadr el)))
		    (t (format srm "!()"))))
	     (t
	      (pprint-fill srm el true))))
    2)

(defmacro empty-list (&rest _)
  ''())

(def-excl-dispatch #\" (srm _)
   (unread-char #\" srm)
   (let ((fstr (read srm)))
      (format nil fstr)))

;;;;(set-dispatch-macro-character #\! #\" #'quote-funmac ytools-readtable*)


(defun print-spaces (number stream)
    (dotimes (n number) (write-char #\Space stream))   )

(defun list-if-not-false (x)
   (cond (x (list x))
	 (t '())))

(declaim (inline car-eq))
(defun car-eq (x y) (and (consp x) (eq (car x) y)))

(defun series (l &optional h i)
   (cond ((null i)
          (setf i 1)
          (cond ((null h) (setf h l) (setf l 1))   ))   )
   (cond ((> l h) nil)
         (t (make-series l h i))   ))

(defun make-series (l h i)
   (declare (fixnum l h i))
   (let ((ans (list l)))
      (do ((tail ans)
           (l (+ l i) (+ l i)))
          ((> l h) ans)
         (setf (cdr tail) (list l))
         (setf tail (cdr tail))   )))

(defun symbol-is-non-function (x)
  (or (macro-function x)
      (special-operator-p x)))

(defgeneric condense (x))

(defmethod condense ((x t))
   (cond ((or (symbolp x) (simple-string-p x) (numberp x))
	  x)
	 ((consp x)
	  (cons (condense (car x)) (and (cdr x) '(--))))
	 ((simple-vector-p x)
	  (cond ((= (array-dimension x 0) 0) '#())
		((= (array-dimension x 0) 1)
		 (vector (condense (svref x 0))))
		(t
		 (vector (condense (svref x 0)) '--))))
	 (t (format nil "--~s--" (type-of x)))))

(defun take (n l)
   (declare (type fixnum n)
            (type list l))
   (cond ((< n 0)
          (let ((g (length l)))  (subseq l (+ g n) g)   ))
         (t (subseq l 0 n))   ))

(defun drop (n l)
   (declare (type fixnum n)
            (type list l))
   (cond ((< n 0) (subseq l 0 (+ (length l) n)))
         (t (subseq l n (length l)))   ))

(defun occurs-in (x tr)
   (cond ((eql x tr) true)
	 ((atom tr) false)
	 (t
	  (or (occurs-in x (car tr))
	      (occurs-in x (cdr tr))))))

(def-excl-dispatch #\? (srm _)
  (labels ((read-like-sym (chars)
	      (let ((*package* keyword-package*))
		 (symbol-name
		    (read-from-string (coerce chars 'string))))))
     (do ((ch (read-char srm) (read-char srm))
	  (chars-to-colon '() (cons ch chars-to-colon)))
	 ((char= ch #\:)
	  (progn
	     (cond ((char= (peek-char false srm) #\:)
		    (read-char srm)))
	     (let ((pkg-name (read-like-sym (reverse chars-to-colon))))
		(let ((pkg (find-package pkg-name)))
		   (cond (pkg
			  (do ((ch (peek-char false srm) (peek-char false srm))
			       (chars-to-delim '() (cons ch chars-to-delim)))
			      ((or (is-whitespace ch)
				   (char= ch #\()
				   (char= ch #\)))
			       (let ((symname (read-like-sym (reverse chars-to-delim))))
				  (let ((sym (find-symbol symname pkg)))
				     (or sym
					 (progn
					    (cerror "I will create it"
						"Symbol ~s not found in package ~s"
						 symname pkg)
					    (intern symname pkg))))))
			     (read-char srm)))
			 (t
			  (error "Package not found: ~s" pkg-name))))))))))

(defmacro on-list (x^ l^) `(push ,x^ ,l^))

(defmacro off-list (l^) `(pop ,l^))

(declaim (special constant-condtests-silent*))

;;; Used to turn off complaints about unreachable code.
(cond (constant-condtests-silent*
       (defmacro -- (x) x))
      (t
       (defmacro -- (x) `(rv ,x))))

(defun rv (v) v)