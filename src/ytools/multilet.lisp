;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on %ytools/ setter signal)

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(multi-let)))

;;;;(needed-by-macros
;;;;   (export '(multi-let multi-let-careful*)))

;;;;(needed-by-macros
;;;;   (defvar multi-let-careful* true))

;;; (multi-let (((v v v ...) form)
;;;                   ((v v v ...) form)
;;;                   ...)
;;;     -body-)
(defmacro multi-let (bindspecs &body b)
   (let ((bindspecs (multi-let-bindspecs-analyze bindspecs true b)))
      (let ((bvars
	       (<# car bindspecs))
	    (explicit-ignores '())
	    b1)
	 (cond ((matchq ((ignore ?@explicit-ignores) ?@b1)
			b)
		(setq b b1)))
	 (cond ((or (>= debuggability* 0) (= (len bindspecs) 1))
		(simple-multi-let bvars bindspecs explicit-ignores b))
	       (t
		(hairy-multi-let bvars bindspecs explicit-ignores b))))))

(needed-by-macros

;;; This needs to be augmented with positions at some point
;;; 'check-syntax' is false if we're in decl mode and it will be checked
;;; by someone else.
(defun multi-let-bindspecs-analyze (bindspecs check-syntax body)
   (let ((standardized  
	    (<# (\\ (bs)
		   (match-cond bs
		      ((atom bs)
		       `((,bs) nil))
		      ?( (() ?_)
			bs)
		      ?( ((?_ ?@_) ?_)
			bs)
		      ?( (?(:+ ?v atom) ?@(:& ?val ?(:\| () (?_))))
			`((,v) ,@val))
		      (t
		       (signal-problem multi-let
			  "Ill-formed: " bs
			  " in: "
			  `(multi-let ,bindspecs ,@body)))))
		bindspecs)))
      (cond (check-syntax
	     (repeat :for ((bs :in standardized :tail bsl))
		(repeat :for ((v :in (car bs)))
		   (cond ((not (is-Symbol v))
			  (signal-problem multi-let
			     "Illegal variable " v " in " t
			     `(multi-let ,bindspecs ,@body)))
			 ((and (not (eq v '_))
			       (exists (other :in (cdr bsl))
				  (memq v (car other))))
			  (signal-problem multi-let
			     "Variable " v " is bound twice in" t
			     `(multi-let ,bindspecs ,@body))))))))
      standardized))

(defun simple-multi-let (bvars-with-underscores bindspecs explicit-ignores b)
   (multiple-value-let
          (bvars ign)
	  (let-fun ((bvars-elim-underscores (bll)
		      (cond ((null bll)
			     (values '() '()))
			    (t
			     (multiple-value-let (bla iga)
						 (underscores-elim (car bll))
				(multiple-value-let (bld igd)
						    (bvars-elim-underscores
						       (cdr bll))
				   (values (cons bla bld)
					   (cons iga igd))))))))
	     (bvars-elim-underscores bvars-with-underscores))
      (cond ((>= debuggability* 0)
	     `(<< (\\ ,(<< append bvars) 
		      ,@(let ((all-ign (append (<< nconc ign)
					       explicit-ignores)))
			   (ignore-if-not-null all-ign))
		      ,@b)
		  (nconc ,@(<# (\\ (m vl)
				  `(value-list-check
				      (multiple-value-list ,m)
				      ',vl ',m))
			       (<# cadr bindspecs)
			       bvars-with-underscores))))
	    ;; Go for efficiency.  (We know there's just one bindspec.)
	    (t
	     (let ((vl (car bvars))
		   (arg (cadar bindspecs))
		   (ign1 (ignore-if-not-null
			    (append explicit-ignores (car ign)))))
		(cond ((= (len vl) 1)
		       `(let ((,(car vl)
			       ,arg))
			     ,@ign1
			   ,@b))
		      (t
		       `(multiple-value-let ,vl ,arg
			      ,@ign1
			   ,@b))))))))

(defun hairy-multi-let (bvars-with-underscores bindspecs explicit-ignores b)
   (let ((auxvars (<# (\\ (vl)
			 (<# (\\ (_) (gensym))
			    vl))
		      bvars-with-underscores)))
      (let (;; Each element of auxvars is split into
	    ;; the corresponding element of used-auxvars
	    ;; and ign-auxvars
	    (used-auxvars
	       (<# (\\ (auxl bvl_)
		      (<! (\\ (av bv_)
			     (cond ((eq bv_ '_)
				    '())
				   (t (list av))))
			  auxl bvl_))
		   auxvars
		   bvars-with-underscores))
	    (ign-auxvars
	       (<# (\\ (avl bvl_)
		      (nconc (<! (\\ (av bv_)
				    (cond ((eq bv_ '_)
					   (list av))
					  (t '())))
				 avl bvl_)
			     ;; ... but we also include the auxvars
			     ;; corresponding to explicit-ignores
			     (<! (\\ (av bv_)
				    (cond ((memq bv_ explicit-ignores)
					   (list av))
					  (t '())))
				 avl bvl_)))
		   auxvars
		   bvars-with-underscores))
	    (used-bvars
	       (<# (\\ (bvl_)
		      (<? (\\ (bv) (not (eq bv '_)))
			  bvl_))
		   bvars-with-underscores)))
	 (let-fun ((nest (auxl ign-auxl bl)
		     (cond ((null bl)
			    `(let ,(<! (\\ (vl auxl)
					  (<! (\\ (v av)
						 (cond ((memq
							   v explicit-ignores)
							'())
						       (t
							(list (tuple v av)))))
					      vl auxl))
				       used-bvars used-auxvars)
					  ,@b))
			   (t
			    `(multiple-value-let ,(car auxl)
						 ,(cadar bl)
						 ,@(ignore-if-not-null
						      (car ign-auxl))
				,(nest (cdr auxl)
				       (cdr ign-auxl)
				       (cdr bl)))))))
	    (nest auxvars ign-auxvars bindspecs)))))
      
(defun ignore-if-not-null (ign)
   (include-if (not (null ign)) `(ignore ,@ign)))

(defun value-list-check (vals vars form)
   (cond ((= (len vals) (len vars))
	  vals)
	 (t
	  (error-break value-list-check
	     "Wrong number of arguments.  Wanted " vars
	     :% " got " vals
	     :% " as value of " form
	     (:novalue "I will ignore some vars or some vals"))
	  (<# (\\ (v _)
		 v)
	      vals vars))))

;;;;(def-op name-lookup (env name)
;;;;   (error-break name-lookup
;;;;      "Can't find " name " in environment " env
;;;;      :fatal))

)