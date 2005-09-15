;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: multilet.lisp,v 1.3.2.2 2005/09/15 02:18:10 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on %ytools/ setter signal)

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(multi-let with-open-files gen-var 
	     keyword-args-extract
	     control-nest track-extra-vals extra-vals)))

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
   (include-if (not (null ign)) `(declare (ignore ,@ign))))

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

;;; (with-open-files ((srmvar1 ...)
;;;                   (srmvar2 ...)
;;;                   ...)
;;;     -body-)
;;; is an abbreviation for
;;; (with-open-file (srmvar1 ...)
;;;    (with-open-file (srmvar2 ...)
;;;        (...
;;;          -body-)))
(defmacro with-open-files (bdgs &body body)
   (let-fun ((build-it (bdgs)
		(cond ((null bdgs) body)
		      (t
		       (let ((inner (build-it (cdr bdgs))))
			  `((with-open-file ,(car bdgs)
			       ,@inner)))))))
      (let ((e (build-it bdgs)))
	 (cond ((= (len e) 1)
		(car e))
	       (t `(progn ,@e))))))


;;; The form (track-extra-vals :extra <bdgs>
;;;			       [:principal <vars>] 
;;;			       [:values <exps>])
;;;            -body-)
;;; where <bdgs> is a list ((var1 val1) ... (varK valK))
;;; <vars> is a list of variables (default: a new variable)
;;; and <exps> is a list of expressions (default: (<vars> <vars-from-bdgs>))
;;; binds the <bdgs>, evaluates the body, binds <vars> to the results,
;;; and returns <exps> as multiple values.
;;; It is expected that the -body- will contain occurrences of
;;; 'extra-vals' (see below).
;;; The keywords can come anywhere; in particular, :values can come
;;; after the body.
(defmacro track-extra-vals (&rest stuff)
   (multi-let (((remainder alist)
		(keyword-args-extract stuff '(:principal :extra :values))))
      (let ((p-vars (alref alist ':principal (list (gen-var 'prin))))
	    (e-bdgs (alref alist ':extra
			   (signal-problem track-extra-vals
			       "Required :extra arg missing in "
			       `(track-extra-vals ,@stuff)))))
	 (cond ((and (atom p-vars)
		     (not (eq p-vars 'nil)))
		(!= p-vars (list *-*))))
	 (let ((vals (alref alist ':values `(,@p-vars ,@(<# car e-bdgs)))))
	    `(let ,e-bdgs
		(multi-let ((,p-vars (progn ,@remainder)))
		   (values ,@vals)))))))

;;;;(defmacro track-extra-vals (&whole tev-exp
;;;;			    resvar\(s\) k init-bindings expr^ &body body^)
;;;;   (cond ((and (eq k ':extra)
;;;;	       (or (is-Symbol resvar\(s\))
;;;;		   (is-list-of resvar\(s\) #'is-Symbol)))
;;;;	  `(multi-let ,init-bindings
;;;;	      (multi-let ((,resvar\(s\) ,expr^)))
;;;;		 ,@body^))
;;;;	 (t
;;;;	  (signal-problem track-extra-vals
;;;;	     "Ill-formed: " tev-exp
;;;;	     :% " Should be of form (track-extra-vals <vars> :extra <bdgs>"
;;;;	        " expression --body--)"))))

;;; (extra-vals <vars> e (v1 e1) ... (vK eK))
;;; evaluates 'e', and binds the <vars> to all but the first value
;;; returned.  (These are the "extra" values.)  All the vI are then
;;; updated by reassigning them to eI.  The first value returned by 'e'
;;; is then the value returned by the 'extra-vals' expression.
(defmacro extra-vals (extra-vars exp^ &rest accums)
   (let ((main-res-var (gen-var 'r)))
      (!= accums (<? neg is-Keyword *-*))
      `(multi-let (((,main-res-var ,@extra-vars)
		    ,exp^))
	  ,@(<# (\\ (acc)
		   (match-cond acc
		      (:? (!= ?@_) acc)
		      (:? (?(:+ ?v is-Symbol) ?e)
			 `(!= ,v ,e))
		      (t (signal-problem extra-vals
			    "Ill-formed 'extra-vals' clause " acc))))
		accums)
	  ,main-res-var)))

;;;;   (cond ((forall (a :in accums) (= (len a) 3))
;;;;	  ;; -- Each is of form (newval accumval accumulator)
;;;;	  (let ((main-res-var (gen-var 'r)))
;;;;	     `(multi-let (((,main-res-var ,@(<# car accums))
;;;;			   ,exp^))
;;;;		 ,@(<# (\\ (acc) `(!= ,(cadr acc) ,(caddr acc)))
;;;;		       accums)
;;;;		 ,gen-var)))
;;;;	 (t
;;;;	  (signal-problem extra-vals
;;;;	     "extra-vals expects a list of (newval accumval accumulator) triples,"
;;;;	     :% " not: " accums))))

;;;;     (control-nest :okay
;;;;                   (bind-something (what-to-bind-1)
;;;;		          (check-something-1 ...
;;;;		    	 :okay
;;;;		    	 ...))
;;;;		       (bind-something (what-to-bind-2)
;;;;		          (check-something-2 ...
;;;;		    	 :okay
;;;;		    	 ...))
;;;;                   done)
;;;;
;;;;is short for
;;;;
;;;;(bind-something (what-to-bind-1)
;;;;   (check-something-1 ...
;;;;      (bind-something (what-to-bind-1)
;;;;	     (check-something-2 ..
;;;;	         done ...))))


;;; Each elements of 'labeled-binding-exps' (except the first)
;;; is preceded by a keyword.  The binding-exp immediately before
;;; that keyword must contain exactly one occurrence of it.
;;; The macro substitutes for the keyword the binding exp labeled
;;; with it.  (Purpose: avoid deeply nexted expressions, especially
;;; when it isn't necessary for variable bindings to be vividly
;;; apparent.)
(defmacro control-nest (&rest labeled-binding-exps)
   (let-fun ((fold (lbes)
		;; -- lbes is tail of 'labeled-binding-exps'
	        ;; starting with a non-label.
		(cond ((null (cdr lbes))
		       (car lbes))
		      ((or (null (cddr lbes))
			   (not (is-Symbol (cadr lbes))))
		       (signal-problem control-nest
			  "Ill-formed labeled-binding-exp in control-nest: "
			  :% 1 (cdr lbes)
			  (:proceed
			     "I'll drop remaining labeled-binding-exps"))
		       (car lbes))
		      (t
		       (cond ((not (= (count-occs (cadr lbes) (car lbes))
				      1))
			      (signal-problem control-nest
				 "Expression should have exactly one"
				 " occurrence of " (cadr lbes) ": "
				 (car lbes)
				 (:proceed "I'll replace all occurrences"))))
		       (subst (fold (cddr lbes))
			      (cadr lbes)
			      (car lbes))))))
      (cond ((null labeled-binding-exps)
	     (signal-problem control-next :fatal
		"control-nest has no forms to combine"))
	    (t (fold labeled-binding-exps)))
    :where
       (:def count-occs (sym e)
	  (cond ((atom e)
		 (cond ((eq e sym) 1)
		       (t 0)))
		(t
		 (</ (\\ (tot x)
			(+ tot (count-occs sym x)))
		     0 e))))))

;;; Returns alist of keywords and vals, plus what's left over
;;; In order: < leftovers, alist >
(defun keyword-args-extract (args keywords)
   (repeat :for ((al args)
		 :collector pairs remainder)
    :until (null al)
    :result (values remainder pairs)
    :within
      (let ((a (car al)))
	 (cond ((memq a keywords)
		(:continue
		 :collect (:into pairs (list a (cadr al)))
		   (!= al (cddr al))))
	       (t
		(:continue
		 :collect (:into remainder a)
		   (!= al (cdr al))))))))

(defun gen-var (sym)
   (build-symbol (:package false) (< sym) - (++ symno*)))

