;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved.
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(fslurp in-readtable in-regime slurp-whole-file needed-by-macros
	     find-lprec with-post-file-transduction-hooks after-file-transduction
	     during-file-transduction setf-during-file-transduction end-header
	     to-slurp fload-verbose* in-header eval-when-slurping
	     make-Printable printable-as-string eof*)))

(defvar object-suffixes* `(,lisp-object-extn*))

(eval-when (:compile-toplevel :load-toplevel)

;;; Useful for constructing "marker" objects that have nothing but their
;;; identity: none of them is EQ to anything encountered in an ordinary
;;; S-expression.
(defstruct (Printable (:print-function
		         (lambda (me str d)
			         (declare (ignore d))
			    (funcall (Printable-printer me) str)))
	              (:constructor make-Printable (printer)))
   printer)

(defun printable-as-string (s)
   (make-Printable (\\ (srm) (format srm "~a" s))))
)

(defvar eof* (printable-as-string "#<End of file>"))
;;;;(make-Printable (\\ (srm) (format srm "~a" "#<End of file>")))

(defun place-load-progress-rec (pn)
   (setq pn (pathname-resolve pn true))
   (let ((lpr (pathname-prop 'load-progress-rec pn)))
      (or lpr
	  (let ((lpr (make-Load-progress-rec
			:pathname (make-Pathname
				     :type (cond ((member (Pathname-type pn)
							  source-suffixes*
							  :test #'string=)
						  false)
						 (t (Pathname-type pn)))
				     :defaults pn)
			:status false
			:whether-compile false
			:when-reached (get-universal-time)
			:when-loaded false
			:status-timestamp file-op-count*)))
	     (setf (pathname-prop 'load-progress-rec pn)
		   lpr)
	     lpr))))
	     
(defmacro find-lprec (&rest filespecs)
   `(do-find-lprec ',filespecs))

(defun do-find-lprec (filespecs)
   (let ((pnl (filespecs->ytools-pathnames filespecs)))
      (cond ((null pnl) "?")
	    (t
	     (prog1 (pathname-prop 'load-progress-rec (car pnl))
	            (cond ((not (null (cdr pnl)))
			   (format *error-output* "Ignoring ~s" (cdr pnl)))))))))

;;;;(defun Load-progress-rec-src-pathname (lpr)
;;;;   (pathname-source-version (Load-progress-rec-pathname lpr)))

;;;;(defun load-progress-rec-src-write-time (lpr)
;;;;   (let ((src-pn (load-progress-rec-src-pathname lpr)))
;;;;      (or (and src-pn (pathname-write-time src-pn))
;;;;	  (get-universal-time))))

(defvar fload-verbose*            true)		
;;; -- true for message during FLOAD and related ops.

(defvar fload-indent*     0)

(defvar post-file-transduce-hooks* '*not-transducing
  "A list of things to do (i.e., functions of no arguments to call) after YTools file transducers finish.")

(defmacro with-post-file-transduction-hooks (&body b)
   `(let ((post-file-transduce-hooks* '()))
       (cleanup-after-file-transduction ,@b)))

(defmacro after-file-transduction (&body b)
   `(cond ((check-file-transduction ',b)
	   (push (\\ () ,@b) post-file-transduce-hooks*))))

(defun during-file-transduction ()
   (not (eq post-file-transduce-hooks* '*not-transducing)))

(defmacro setf-during-file-transduction (place val^)
   (let ((save-var (gensym)))
      `(let ((,save-var ,place))
	  (after-file-transduction (\\ () (setf ,place ,save-var)))
	  (setf ,place ,val^))))

(defun check-file-transduction (b)
   (cond ((during-file-transduction) true)
	 (t
	  (cerror "I will skip setting the hook"
	     "Attempt to set post-file-transduction hook when not transducing a file ~%   (~s)"
	     b)
	  false)))

;; --Pathname of file ...
(defvar now-loading*     nil)  ; ... being loaded by 'fload'
(defvar now-compiling* false)  ; ... being compiled by 'fcompl'
(defvar now-slurping*   '())   ; ... being slurped by 'fslurp'

(defvar slurping-stack* '())
;;;; (defvar previous-slurp-speclist* '())

(defmacro file-op-defaults-update (specs possible-flags
				   acc-defaults set-defaults)
   (let ((default-var (gensym)))
      `(let ((,default-var (,acc-defaults)))
	  (multiple-value-bind (files flags readtab)
	                       (flags-separate ,specs ,possible-flags)
	     (,set-defaults
		 (tuple (cond ((null files)
			       (car ,default-var))
			      (t files))
			(cond ((and (null flags) (null files))
			       (cadr ,default-var))
			      (t flags))
			(decipher-readtable readtab ,default-var)))))))

(defun decipher-readtable (readtab defaults)
			(cond ((eq readtab '*missing)
			       (let ((default-readtab (caddr defaults)))
				  (cond ((and default-readtab
					      (not (eq default-readtab
						       *readtable*)))
					 (format t "Readtable ~s will be used for this file operation~%"
						 default-readtab)
					 default-readtab)
					(t
					 *readtable*))))
			      (t
			       (name->readtable readtab))))


(defmacro fslurp (&rest specs)
  `(do-fslurp ',specs))

(defvar fslurp-flags* '(-f))

(defvar default-fslurp-args* (tuple !() !() nil))

;;; True if we're in the midst of an 'fload', 'fcompl', or 'fslurp'.
(defvar file-op-in-progress* false)

(defun do-fslurp (specs)
  (cond ((not file-op-in-progress*)
	 (setq file-op-count* (+ file-op-count* 1))))
  (let ((file-op-in-progress* true))
     (file-op-defaults-update specs fslurp-flags*
			      (lambda () default-fslurp-args*)
			      (lambda (a)
				 (setq default-fslurp-args* a)))
     (apply #'filespecs-slurp default-fslurp-args*)))

(defun filespecs-slurp (specs flags *readtable*)
  (let ((force-flag false))
    (dolist (flag flags)
       (cond ((eq flag '-f)
	      (setq force-flag true))))
    (dolist (pn (filespecs->ytools-pathnames specs))
      (pathname-slurp pn force-flag ':whole-file))))

(defun pathname-slurp (pn force-flag howmuch)
;;;;   (breakpoint pathname-slurp "Hup!")
   (cond ((is-Pseudo-pathname pn)
	  (funcall (Pseudo-pathname-slurper pn)
		   pn
		   force-flag howmuch))
	 (t
	  (let ((v (pathname-prop 'version pn)))
	     (cond (v
		    (pathname-slurp v force-flag howmuch))
		   (t
		    (let ((lprec (place-load-progress-rec pn)))
		       (lprec-slurp lprec force-flag howmuch))))))))

;;; Important fact about 'lprec-slurp': When it slurps the 'depends-on's in 
;;; the header of a source file, it sets 'run-time-depends-on' and
;;; 'compile-time-depends-on' fields of 'lprec'.  The code that does
;;; this won't be defined until the file depend.lisp is loaded.

(defun lprec-slurp (lprec force-flag howmuch)
   (let ((pn (Load-progress-rec-pathname lprec)))
      (let ((src-version (pathname-source-version pn)))
	 (cond (src-version
		(let ((up-to-date
			 (achieved-load-status
			    lprec
			    (ecase howmuch
			       (:header-only ':header-checked)
			       (:at-least-header ':slurped)
			       (:whole-file ':slurped-all)))))
;;;;		   (out "lprec-slurp finds up-to-date status " up-to-date
;;;;			" for amount " howmuch " of "
;;;;			:% lprec :%)
		   (cond ((or force-flag (not up-to-date))
			  (pathname-really-slurp
			     src-version lprec howmuch)
			  false)
			 (t true))))
	       (t
		(cerror "I will ignore this file"
			"No source file to slurp for ~s"
			pn))))))

;;;;(defvar slurping-modes* '(:all :dependencies))

(defun pathname-really-slurp (pn lprec howmuch)
  (if (member pn slurping-stack* :test #'pathname-equal)
      (if fload-verbose*
	  (format *error-output*
	     "Warning -- ~s already being slurped~%" pn))
      (let ((real-pn (pathname-resolve pn false)))
         (let  ((fload-indent*  0)
		(now-loading*  false)
		(now-compiling* false)
		(now-loading-lprec* false)
		(now-slurping*   pn)
		(slurping-stack* (cons pn slurping-stack*))
		#+:excl (excl:*source-pathname* real-pn)
		#+:excl (excl:*record-source-file-info* nil)
		(*package* *package*)
		(*readtable* *readtable*))
	    (let ((slurp-status 
		     (pathname-finally-slurp real-pn lprec pn howmuch)))
	       (cond ((not (achieved-load-status lprec slurp-status))
		      (note-load-status   ;;;; -if-not-achieved
		         lprec slurp-status))))))))

(defmacro cleanup-after-file-transduction (&body b)
   `(unwind-protect (progn ,@b)
       (dolist (h post-file-transduce-hooks*)
	  (funcall h))))

(defvar slurp-whole-file* true)
(defvar slurping-lprec* false)
(defvar slurping-how-much* nil)

(defvar end-header-dbg* false)

;;; Returns :header-checked, :slurped, or :slurped-all
;;; May have side effect on lprec, in particular, setting
;;; the 'depends-on' fields.
(defun pathname-finally-slurp (pn lprec ytools-pn howmuch)
;;;;   (cond ((equal (Pathname-name pn) "trecycle")
;;;;	  (dbg-save pn lprec)
;;;;	  (breakpoint pathname-finally-slurp
;;;;	     "Slurping " lprec)))
   (let ((out-of-header false)
	 (slurp-whole-file* (eq howmuch ':whole-file))
	 (slurping-how-much* howmuch)
	 (slurping-lprec* lprec))
      (setf (Load-progress-rec-run-time-depends-on lprec)
	    !())
      (setf (Load-progress-rec-compile-time-depends-on lprec)
	    !())
      (labels ((end-header-note (form)
		  (cond (end-header-dbg*
			 (format *error-output*
			    "End of header on file ~s~%" pn)
			 (cond ((eq form eof*)
				(cond ((not slurp-whole-file*)
				       (format *error-output*
					       "  -- Header is entire file~%"))))
			       ((and (not slurp-whole-file*)
				     (not (car-eq form 'end-header)))
				(format *error-output*
					"  -- Header ends at ~s~%"
					form)))))))
	 (let ((post-file-transduce-hooks* !()))
	    (with-open-file (s pn :direction ':input)
	       (cleanup-after-file-transduction
		  (do ((form (read s false eof*) (read s false eof*)))
		      ((or (eq form eof*)
			   (and out-of-header
				(case howmuch
				   (:header-only true)
				   (:at-least-header 
   ;;;;				 (breakpoint pathname-finally-slurp
   ;;;;				    "leaving header, slurp-whole-file* = "
   ;;;;				    slurp-whole-file*)
				    (not slurp-whole-file*))
				   (t false))))
		       (cond ((eq form eof*)
			      (end-header-note form))))
		     (cond ((form-slurp form slurp-whole-file*)
			    ;; Substantive
			    (cond ((not out-of-header)
				   (setq out-of-header true)
				   (end-header-note form)
				   (cond (slurp-whole-file*
					  (fload-op-message
					       "Slurping"
					       ytools-pn pn "...")))))))))))
	 (cond ((and out-of-header slurp-whole-file*)
		(fload-op-message "...slurped" pn false "")))
	 (case howmuch
	    (:header-only ':header-checked)
	    (:at-least-header
	     (cond (slurp-whole-file*
		    ':slurped-all)
		   (t
		    (supporting-files-in)
		    ':slurped)))
	    (t ':slurped-all)))))

(datafun-table slurp-handlers* to-slurp :size 200)

(defvar file-headers* '(in-package depends-on export))

;;; Returns true if form is "substantive," i.e., part of the file's
;;; content.  Returns false if form is "header material," such as 
;;; 'in-package', 'depends-on', and such.
;;; 'slurp-if-substantive' might better be called 'past-header'.
(defun form-slurp (form slurp-if-substantive)
  (cond ((atom form)
	 false)
	((and (is-Symbol (car form))
	      (table-entry slurp-handlers* (car form)))
;;;;	 (format t "Found handler for ~s~%" form)
	 (funcall (table-entry slurp-handlers* (car form))
		  form slurp-if-substantive))
	((macro-function (car form))
;;;;	 (format t "Macro ~s being expanded to ~s~%" form (macroexpand-1 form))
	 (prog1 (form-slurp (macroexpand-1 form)
			    slurp-if-substantive)
	     ;;;;(format t "Mission accomplished~%")
	     ))
	(t true)))

(defvar fload-show-actual-pathnames* false)

#|
NOT USED BY ANYONE (oddly enough)
(defun ytools-module-slurp (name force-flag)
   (cond ((or force-flag (not (memq name loaded-ytools-modules*)))
	  (let ((ytm (find-YT-module name)))
	     (cond (ytm
		    (form-slurp (YT-module-action ytm) false))
		   (t
		    (cerror "I will ignore it"
			    "Attempt to slurp nonexistent module '~s'"
			    name)))))
	 (t
	  (let ((ytm (place-YT-module name)))
	     (let ((p (YT-module-postponed ytm)))
	        (cond ((not (null p))
		       (setf (YT-module-postponed ytm) !())
		       (let ((module-now-loading* name))
			  (dolist (e (reverse p))
			     (form-slurp e false)))
		       (cond ((null (YT-module-postponed ytm))
			      `("Finished slurping module" ,name))
			     (t
			      (note-module-postponement
				 `(check-module-postponed ',name))
			      `("Postponed slurping module " ,name))))
		      (t `("Load blocks slurp" ,name))))))))
|#

;;; Returns three values: filespecs, flags, and readtable
;;; If readtable is missing, the value is *missing.
(defun flags-separate (args possible-flags)
   (let ((flags !())
	 (readtab (memq ':readtable args)))
      (cond (readtab
	     (setq args (nconc (ldiff args readtab)
			       (cddr readtab)))
	     (setq readtab (cadr readtab)))
	    (t (setq readtab '*missing)))
      (do ((al args (cond (flags-done al) (t (cdr al))))
	   (flags-done false)
	   fname interned-flag)
	  ((or flags-done (null al))
	   (values al (reverse flags) readtab))
         (cond ((is-Symbol (car al))
		(setq fname
		      (symbol-name (car al)))
		(setq flags-done (not (char= (elt fname 0) #\-))))
	       (t
		(setq flags-done true)))
	 (cond ((not flags-done)
		(setq interned-flag
		      (intern fname ytools-package*))
		(cond ((memq interned-flag possible-flags)
		       (setq flags (cons interned-flag flags)))
		      (t
		       (error "Unexpected flag ~s; expected one of ~a"
			      (car al)
			      (mapcar (lambda (flag)
					 (intern (symbol-name flag)
						 *package*))
				      possible-flags)))))))))

(defun fload-op-message (beg-message pn real-pn end-message)
   (if fload-verbose*
       (progn
	  (print-spaces fload-indent* *query-io*)
	  (format *query-io* "~a " beg-message)
	  (cond (pn
		 (format *query-io* "~a" (namestring pn))))
	  (cond ((and fload-show-actual-pathnames*
		     real-pn
		     (not (equal real-pn pn)))
		 (format *query-io* "~%")
		 (print-spaces
		     (+ fload-indent* (length beg-message) -2)
		     *query-io*)))
	  (format *query-io* "~a~%" end-message))))

(defmacro in-readtable (name)
  `(eval-when (:compile-toplevel :load-toplevel :slurp-toplevel :execute)
      (setq *readtable* (name->readtable ',name))))

(defun name->readtable (name)
   (cond ((not name) lisp-readtable*)
	 ((symbolp name)
	  (let ((rt (or (named-readtable name)
			(eval name))))
	     (cond ((typep rt 'readtable)
		    rt)
		   (t (error "in-readtable: ~s is not a readtable" rt)))))
	 (t
	  (error "in-readtable: ~s is not the name of a readtable"
		 name))))

(defmacro in-regime (pkg &optional rt)
   `(progn (in-package ,pkg)
	   (in-readtable ,(or rt pkg))))

;;;======================================================================
;;; TO-SLURP datafuns for basic forms.  to-slurp's for other forms
;;; are introduced when the forms are.
;;;======================================================================

(datafun to-slurp in-package
   (defun :^ (e _)
      (eval e)
      false))

(datafun to-slurp in-readtable
   (defun :^ (e _)
      (eval e)
      false))

(datafun to-slurp defpackage in-package)
(datafun to-slurp declaim in-package)

(datafun to-slurp eval-when-slurping
   (defun :^ (form _)
      (dolist (e (cdr form))
	 (eval e))
      false))

(defmacro needed-by-macros (&rest l)
   `(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
      ,@l))

(datafun to-slurp needed-by-macros eval-when-slurping)

(datafun to-slurp eval-when
   (defun :^ (form _)
      (cond ((memq ':slurp-toplevel (cadr form))
	     (dolist (e (cddr form))
	        (eval e))
	     false)
	    (t true))))

(defmacro in-header (&body b)
  `(progn ,@b))

;;; Evaluate if encountered in header, without terminating header.
(datafun to-slurp in-header eval-when-slurping)

(defmacro end-header (&rest _)
   '(values))
	 
(datafun to-slurp end-header
   (defun :^ (form _)
      (cond ((and (memq ':continue-slurping (cdr form))
		  (eq slurping-how-much* ':at-least-header))
	     (setq slurp-whole-file* true)))
      (supporting-files-in)
      (cond ((memq ':no-compile (cdr form))
	     (setf (Load-progress-rec-whether-compile slurping-lprec*)
		   ':source)))
      (cond (end-header-dbg*
	     (format *error-output*
		     "Executing ~s~%  slurp-whole-file* = ~s whether-compile = ~s~%"
		     form
		     slurp-whole-file*
		     (Load-progress-rec-whether-compile slurping-lprec*))))
      true))

(defmacro slurp-whole-file ()
   (format nil "Whole file ~s will be slurped" now-slurping*))

(datafun to-slurp slurp-whole-file
   (defun :^ (_ _)
;;;;     (breakpoint slurp-whole-file-to-slurp
;;;;	"slurping-how-much* = " slurping-how-much*)
     (cond ((not (eq slurping-how-much* ':header-only))
	    (setq slurp-whole-file* true)))
     (supporting-files-in)
     false))

(defun supporting-files-in ()
   (values)
;;;;	    (dolist (rtsupp
;;;;			(Load-progress-rec-run-time-depends-on
;;;;			 slurping-lprec*))
;;;;	       (format *error-output*
;;;;		       "Not slurping ~s~%" rtsupp)
;;;;	       (pathname-slurp rtsupp false ':whole-file)
;;;;	      )
;;;;	    (dolist (ctsupp
;;;;			(Load-progress-rec-compile-time-depends-on
;;;;			     slurping-lprec*))
;;;;	       (pathname-fload ctsupp false false))
	    )

(datafun to-slurp datafun
  (defun :^ (e slurp-if-substantive)
    (cond ((and slurp-if-substantive
		(eq (cadr e) 'attach-datafun))
	   (eval e)))
    true))
;;;;	  (t
;;;;	   (form-slurp (macroexpand-1 e) slurp-if-substantive)))))

(defun forms-slurp (explist slurp-if-substantive)
     (let ((substantive false))
        (block through-forms
	   (dolist (exp explist)
	      (cond ((form-slurp exp slurp-if-substantive)
		     (setq substantive true)))
	      (cond ((and substantive (not slurp-if-substantive))
		     (return-from through-forms)))))
	substantive))

(datafun to-slurp progn
  (defun (e slurp-if-substantive)
     (forms-slurp (cdr e) slurp-if-substantive)))

(datafun to-slurp prog1    progn)
(datafun to-slurp prog2    progn)

;;; Maybe evaluate 'e' and report that it was substantive.
(eval-when (:compile-toplevel :load-toplevel :execute)
   (defun slurp-eval (e slurp-if-substantive)
      (cond (slurp-if-substantive
	     (eval e)))
      true))

(datafun to-slurp defmacro #'slurp-eval)
(datafun to-slurp cl:defmacro #'slurp-eval)

(datafun to-slurp defsetf  #'slurp-eval)

(datafun to-slurp defstruct #'slurp-eval)

(datafun to-slurp defvar
   (defun :^ (form _)
      ;;;;(format *error-output* "defvar-ing ~s~%" (cadr form))
      (eval `(defvar ,(cadr form)))
      true))

(datafun to-slurp defconstant
   (defun :^ (form slurp-if-substantive)
      (cond (slurp-if-substantive
	     (cond ((not (boundp (cadr form)))
		    (eval form)))))
      true))

(datafun to-slurp with-packages-unlocked
   (defun :^ (form slurp-if-substantive)
      (with-packages-unlocked
	 (forms-slurp (cdr form) slurp-if-substantive))))

(datafun to-slurp subr-synonym #'slurp-eval)

;;; For debugging:

(defun filespec-lprec (fs)
   (place-load-progress-rec (car (filespecs->ytools-pathnames fs))))