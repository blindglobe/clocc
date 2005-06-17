;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(depends-on %module/ ytools)

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(control-nest track-extra-vals extra-vals)))

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
   (multi-let (((alist remainder)
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
(defun keyword-args-extract (args keywords)
   (repeat :for ((al args)
		 :collector pairs remainder)
    :until (null al)
    :result (values pairs remainder)
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
