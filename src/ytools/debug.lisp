;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: debug.lisp,v 1.4 2005/02/21 14:10:02 airfoyle Exp $

(depends-on %module/  ytools
	    :at-run-time %ytools/ nilscompat)

(end-header :continue-slurping)

(eval-when (:slurp-toplevel :load-toplevel)
   (export '(s sv ps ss dbg-stack* dbg-save st g gty package seek ev ev-ugly
	     get-frame-args
	     symshow =g htab-show file-show test check
	     condition-display-string)))

;; Each entry is of the form (flag form type), where flag is a symbol,
;; often *.  The idea is to make it easy to munge the forms further,
;; while debugging macros or for some other purpose.
(def-class Dbg-entry
   (:options (:medium :list))
   label
   object
   type)

(defvar dbg-stack* '())

;; Macro expand arg or arg applied to something on dbg-stack*
(defmacro s (&optional (form nil form-supp) (flag '*))
   `(sv ,(cond (form-supp `',form)
	       (t '(g *)))
	,flag))
	
;; Same, but always evaluates arg.
(defmacro sv (&optional (form '(g *)) (flag '*))
   `(dbg-push ',flag
	      (macroexpand-1 ,form)
	      'Sexp false))

;;;;	          ,(cond ((atom f) `(,f (g ,@stuff)))
;;;;				       (t `',f)))))



;;;;(defun sv (v &optional )
;;;;   (dbg-push '* (macroexpand-1 v) 'Sexp false))

;; evalute form and push on stack if not atomic.
(defmacro ev (form &optional (label '*) (type false))
   (let ((vvar (gensym)) (tyvar (gensym)))
      `(multi-let (((,vvar ,tyvar)
		    (ev-process ',form ,(cond (type `',type) (t 'false)))))
	  (dbg-push ',label ,vvar ,tyvar))))

;; Like ev, but print nonprettily and don't put on stack
(defmacro ev-ugly (form)
   `(ev-process '(bind ((*print-pretty* false) (out ,form :%) (values)))
		'Void))

;;; If * (value of last thing printed) is an Allegro-style function-call 
;;; display (i.e., (function-name val-of-arg-1 ... val-of-arg-N)),
;;; extract the args, give them the relevant names, and push them
;;; onto the debug-stack.  If a name is '_', the corresponding arg is
;;; skipped.  If there are fewer names than args, the excess args are
;;; not taken.
(defmacro get-frame-args (&rest names)
   `(progn
       ,@(repeat :for ((name :in names)
		       (i = 1 :by 1))
	  :when (not (eq name '_))
	  :collect `(ev (nth ,i *) ,name))
       ',names))

(needed-by-macros

;;; alist of special handlers
(defvar ev-process-tab* !())

(datafun attach-datafun ev-process
   (defun :^ (_ sym fname)
      (!= (alist-entry sym ev-process-tab*)
	  (symbol-function fname))))

(defun ev-process (form td)
   (let ((h (and (consp form) (alist-entry (car form) ev-process-tab*))))
      (cond (h
;;;;	     (out "Calling handler for " form " & " td :%)
	     (funcall h form td))
	    (t
	     (values (eval (subst-with-stack-vars form)) td)))))

(defun subst-with-stack-vars (exp)
   (cond ((and (is-Symbol exp)
	       (not (eq exp '*)))
	  ;; '*' still means "just typed," not (g *), by default
	  (let ((ent (nth-dbg-entry dbg-stack* exp 0)))
	     (cond ((eq ent absent-dbg-entry*)
		    exp)
		   (t `(g ,exp)))))
	 ((atom exp) exp)
	 ((memq (car exp) '(g quote)) exp)
	 (t
	  (<# subst-with-stack-vars exp))))

(defvar dbg-stack-max-len* 200)

(defvar dbg-stack-trap-labels* '())

(defun dbg-push (label x &optional (type false) (even-trivia true))
   (cond ((or even-trivia
	      (not (or (is-Number x)
		       (not x)
		       (is-Symbol x))))
	  (cond ((memq label dbg-stack-trap-labels*)
		 (signal-problem dbg-push
		    "Saving under label: " label
		    t " Object: " x
		    (:continue "I'll go ahead and save it"))))
	  (push (make-Dbg-entry label x type) dbg-stack*)
	  (cond ((> (len dbg-stack*) dbg-stack-max-len*)
		 (let ((newlen
			  (signal-problem dbg-push
			     "Dbg-stack exceeds " dbg-stack-max-len*
			     " elements: "
			     (<# Dbg-entry-label dbg-stack*)
			     (:prompt-for "New stack length (default: no change): "
					  dbg-stack-max-len*))))
		    (cond ((> newlen (len dbg-stack*))
			   (!= dbg-stack-max-len* newlen))
			  (t
			   (ps (- (len dbg-stack*) newlen)))))))))
   (values x type))
 
)

;;;; dbg-save prints a message on *query-io* when it is expanded, so
;;;; the user doesn't forget the call to dbg-save is there.
;;;; If dbg-save-msg-postpone* is non-false, we wait and print the message
;;;; when the expanded code runs.
;;;;(needed-by-macros
;;;;(defvar dbg-save-msg-postpone* false)
;;;;)

;; (dbg-save [(:package p)] <announce> (v1 e1 [t1]) (v2 e2 [t2]) ...) 
;;   puts entries flagged vI with value
;;   eI and type tI on stack.  
;; (v v false) may be abbreviated v.  Each vI is reinterned
;; in package p, if supplied, else in the package in effect when dbg-save 
;; is executed.  (Big convenience during debugging.)
;; Generally the programmer wants to be reminded about an occurrence of 
;; dbg-save, either when it is expanded (typically at compile time), when 
;; its expansion is executed, or both.
;; The <announce> fields determine when the reminder is produced.
;; Their format is
;;    [| :comp-quiet | :comp-loud]   [| :run-quiet | :run-loud]
;; :run-loud is useful for reminding you of what's been saved on the dbg-stack
;; just before a breakpoint.
;; :comp-loud is useful for reminding you that there are stray debugging
;; statements in the program, that might be wasting space by growing the
;; dbg-stack for no reason.
;; Defaults: If nothing is supplied, the default is :comp-loud :run-quiet.
;; If just one is supplied, the other defaults to the opposite.
;; (:silent is a synonym for :comp-quiet.)
(defmacro dbg-save (&rest vars-n-vals)
   (multi-let (((pkg-name comp-loud run-loud real-vars-n-vals)
		(dbg-save-analyze vars-n-vals)))
      (let ((pkg-exp
	       `(maybe-package
		   ,(cond (pkg-name `',pkg-name)
			  (t 'false)))))
	 (cond (comp-loud 
		(out (:to *error-output*) "Dbg-save: " real-vars-n-vals :%)))
	 `(progn
	     ,@(include-if run-loud
		  `(out (:to *query-io*)
		      "Dbg-saving: "
		      (list ,@(<# (\\ (vv)
				     `(intern
					 ',(symbol-name
					      (dbg-save-var-val-analyze
						 vv))
					 ,pkg-exp))
				  real-vars-n-vals))
		      t))
	     ,@(repeat :for ((l :in real-vars-n-vals))
		:collect (multi-let (((var val type)
				      (dbg-save-var-val-analyze l)))
			    (!= var `(intern ',(symbol-name var)
					     ,pkg-exp))
			    `(dbg-push ,var ,val ,(cond (type `',type)
							(t 'false)))))))))

(needed-by-macros

(defun dbg-save-analyze (vars-n-vals)
   (let ((pkg-name false)
	 (real-vars-n-vals '())
	 (comp-explicit false)
	 (comp-loud true)
	 (run-explicit false)
	 (run-loud false))
      (repeat :for ((vv :in vars-n-vals))
       :result (!= real-vars-n-vals (nreverse *-*))
         (match-cond vv
	    ?( ?(:\| :comp-loud :comp-quiet :silent)
	      (cond (comp-explicit
		     (out (to *error-output*)
			"More than one :comp-X instruction in "
			`(dbg-save ,@vars-n-vals)
			t "  Ignoring all but first" t))
		    (t
		     (!= comp-explicit true)
		     (!= comp-loud (eq vv ':comp-loud)))))
	    ?( ?(:\| :run-loud :run-quiet)
	      (cond (run-explicit
		     (out (to *error-output*)
			"More than one :run-X instruction in "
			`(dbg-save ,@vars-n-vals)
			t "  Ignoring all but first" t))
		    (t
		     (!= run-explicit true)
		     (!= run-loud (eq vv ':run-loud)))))
	    ?( (?(:\| :package package) ?pkg)
	      (!= pkg-name pkg))
	    (t 
	     (!= real-vars-n-vals (cons vv *-*)))))
      (cond (comp-explicit
	     (cond ((not run-explicit)
		    (!= run-loud (not comp-loud)))))
	    (run-explicit
	     (!= comp-loud (not run-loud))))
      (values pkg-name comp-loud run-loud real-vars-n-vals)))

(defun dbg-save-var-val-analyze (l)
   (cond ((atom l) (values l l false))
	 (t
	  (values (car l)
		  (cadr l)
		  (cond ((cddr l) (caddr l))
			(t false))))))

)

(defun maybe-package (pkg-name)
   (let ((pkg (cond (pkg-name (find-package pkg-name))
		    (t *package*))))
      (cond (pkg pkg)
	    (t
	     (out (to *error-output*) "dbg-save can't find package " pkg-name
		  ", using " *package* t)
	     *package*))))

;; Get the nth entry on the stack associated with flag sym.
(defmacro g (&optional (sym '*) (n 0))
   (cond ((and (numberp sym)
	       (= n 0))
	  (!= n sym)
	  (!= sym '*)))
   `(Dbg-entry-object (nth-dbg-entry dbg-stack* ',sym ,n)))

;; Get the type of the nth entry
(defmacro gty (&optional (sym '*) (n 0))
   (cond ((and (numberp sym)
	       (= n 0))
	  (!= n sym)
	  (!= sym '*)))
   `(Dbg-entry-type (nth-dbg-entry dbg-stack* ',sym ,n)))

(defvar absent-dbg-entry* (make-Dbg-entry nil "?" nil))

(defsetf g-set (&optional (sym '*) (n 0)) (val)
   `(set-dbg-entry-object ',sym ',n ,val))

(defun set-dbg-entry-object (sym n val)
   (let ((e (nth-dbg-entry dbg-stack* sym n)))
      (cond ((eq e absent-dbg-entry*)
	     "?")
	    (t
	     (!= (Dbg-entry-object e) val)))))

(defun nth-dbg-entry (a sym n)
   (cond ((null a) absent-dbg-entry*)
	 ((eq (Dbg-entry-label (car a)) sym)
	  (cond ((= n 0)
		 (car a))
		(t (nth-dbg-entry (cdr a) sym (- n 1)))))
	 (t
	  (nth-dbg-entry (cdr a) sym n))))

(defvar dbg-stack-show* 3)

(defmacro =g (newlabel &optional (oldlabel '*) (n 0))
   `(dbg-entry-label-change ',oldlabel ,n ',newlabel))

(defun dbg-entry-label-change (oldlabel n newlabel)
  (let ((e (nth-dbg-entry dbg-stack* oldlabel n)))
     (cond ((eq e absent-dbg-entry*)
	    "?")
	   (t (!= (Dbg-entry-label e) newlabel)))))

;; Pop n things off *end* of dbg-stack and display prefix of result.
;; If n negative, pop off beginning
(defun ps (&optional (n 0) (display dbg-stack-show*))
   (!= dbg-stack* (cond ((> (abs n) (length dbg-stack*))
			 '())
			((< n 0)
			 (nthcdr (- n) *-*))
			(t
			 (drop (- n) *-*))))
   (format t "~s~%" (<# (\\ (e) `(,(Dbg-entry-label e)
				  ,(condense (Dbg-entry-object e))
				  ,(Dbg-entry-type e)))
			(take (min display (len dbg-stack*))
			      dbg-stack*)))
;;;   (bind ((*print-level* dbg-stack-show*)
;;;	  (*print-length* dbg-stack-show*)
;;;	  (*print-pretty* true))
;;;      (format t "~s~%" (cdr dbg-stack*)))
   (length dbg-stack*))

;; Display n things on top of stack
(defun ss (n)
  (bind ((dbg-stack-show* n))
     (ps 0)))

;; Display nth element of dbg-stack.
(defun st (&optional (n 0))
   (elt dbg-stack* n))

;; !^pkg sym returns pkg::sym, but imports sym to current package.
(!= (get '\^ 'excl-reader)
    (\\ (srm _)
       (let ((pkgname (read srm)))
	  (let ((x (let ((*package* (find-package (symbol-name pkgname))))
		      (read srm))))
	     (cond ((is-Symbol x)
		    (cond ((eq (find-symbol (symbol-name x)
					    *package*)
			       x)
			   (out x " already in " t 3 *package* t))
			  (t
			   (out "Importing " x " into " t 3 *package* t)
			   (import x))))
		   (t
		    (out "???" t)))
	     x))))

;;; Push first subform of exp beginning with sym onto dbg-stack*
(defun seek (sym &optional (num 0) (exp (g *)) (label '*))
   (let-fun ((sk (e)
	       (cond ((atom e) false)
		     ((eq (car e) sym)
		      (cond ((= num 0)
			     e)
			    (t
			     (!= num (- num 1))
			     (skl (cdr e)))))
		     (t
		      (skl e))))
	    (skl (e)
	       (repeat :for (x)
		  (!= x (sk (car e)))
		:until x
		:result x
		  (!= e (cdr e))
		:until (atom e)
		:result false)))
      (let ((sub (sk exp)))
	 (cond (sub (dbg-push label sub))))))
				 
(defun symshow (str &optional (pkg1 *package*) (pkg2 *package*))
   (cond ((is-Symbol str)
	  (!= str (symbol-name str))))
   (cond ((eq pkg1 pkg2)
	  (!= pkg2 false)))
   (multiple-value-let (sym status)
		       (find-symbol str pkg1)
      (cond (sym
	     (let ((sympkg (symbol-package sym)))
		(out sym " home " sympkg)
		(let-fun ((also-show (pkg)
			    (multiple-value-let (sym-in-pkg stat)
						(find-symbol str pkg)
			       (cond (sym-in-pkg
				      (cond ((eq sym-in-pkg sym)
					     (out "  Also visible in "
						  pkg " [" stat "]" t))
					    (t
					     (out "  Shadowed in "
						  pkg " [" stat "]" t))))
				     (t
				      (out "  Not visible in " pkg))))))
		   (cond ((eq sympkg (find-package pkg1))
			  (out "[" status "]" t))
			 (sympkg
			  (multiple-value-let (_ stat)
					      (find-symbol str sympkg)
			     (out "[" stat "]" t)
			     (also-show pkg1))))
		   (cond ((and pkg2 (not (eq sympkg (find-package pkg2))))
			  (also-show pkg2))))))
	    (t
	     (out "No such symbol in " pkg1 t)))))

(defun htab-show (htab)
   (with-hash-table-iterator (ht-iter htab)
      (repeat 
       :within
	 (multiple-value-let (found key value)
			     (ht-iter)
	    (:continue
	     :while found
	     (out key " -- " (condense value) :%))))))

(defun file-show (filespecs)
   (with-open-file (srm (car (filespecs->pathnames filespecs)) :direction ':input)
      (repeat :for ((r = (in (:from srm) :obj)
		       :then :again))
       :until (eq r eof*)
          (out (:to *query-io*) r :% :%))))

(define-condition Test-failure (error)
   ((description :reader Test-failure-description
		 :initarg :description))
   (:report (lambda (tf srm)
	       (out (:to srm) "TEST FAILURE " (Test-failure-description tf)))))

(defvar break-on-test-failure* true)

(defvar check-count*)

;;; (test id-string  -forms-)
;;; The forms include calls to the 'check' macro, below
(defmacro test (string &rest body)
   (let ((block-label (gensym)))
      `(block ,block-label
	  (let ((check-count* 0))
	     (handler-bind ((Test-failure
			       (\\ (tf)
				  (cond ((not break-on-test-failure*)
					 ;; We have to do this because
					 ;; the :report clause doesn't seem
					 ;; to work right --
					 (bind ((*print-escape* false))
					    (out (:to *error-output*)
					       "TEST FAILURE"
					       (Test-failure-description tf)
					       :% " *** " ,string " test FAILED ***" :%))
					 (return-from ,block-label))))))
		  (progn (out (:to *error-output*) "Beginning " ,string " test" :%)
			 ,@body
			 (out (:to *error-output*) 5 ,string " test succeeded " :%)))))))

;;; (check form -out-stuff-) runs form.  If it returns false, 'out' the
;;; out-stuff and break.
(defmacro check (form &rest msgstuff)
   (!= msgstuff (remove ':else *-*))
   (cond ((not (null msgstuff))
	  (!= msgstuff `(": " ,@*-*))))
   (let ((srmvar (gensym)))
      `(progn
	  (!= check-count* (+ *-* 1))
;;;;	  (out "check-count* = " check-count* :%)
	  (cond ((not ,form)
		 (let ((cc check-count*))
		    (signal-problem :noplace
		       :class Test-failure
		       :description
			   (make-Printable
			       (\\ (,srmvar)
				  (out (:to ,srmvar)
				      1 cc ,@msgstuff)))
		       :proceed)))))))

#+allegro
(defun condition-display-string (c)
   (with-output-to-string (srm)
      (cond ((forall (s :in '(excl::format-control excl::format-arguments))
		(and (slot-exists-p c s)
		     (slot-boundp c s)))
	     (apply #'format srm (slot-value c 'excl::format-control)
			     (slot-value c 'excl::format-arguments)))
	    (t
	     (out (:to srm) "Undisplayable " (:a c) :%)))))
