;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
	     
;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(fload fcompl fload-compile* fcompl-reload* always-slurp
	     debuggable debuggability*)))

(defvar fload-flags* '(- -f -a -c))

(defmacro fload (&rest specs)
  `(do-fload ',specs))

(defvar default-fload-args* (tuple !() !() nil))

(defun do-fload (specs)
   (cond ((not file-op-in-progress*)
	  (setq file-op-count* (+ file-op-count* 1))))
   (let ((file-op-in-progress* true))
      (catch 'fload-abort
	 (with-compilation-unit ()
	    (file-op-defaults-update specs fload-flags*
				   (lambda () default-fload-args*)
				   (lambda (a)
				      (setq default-fload-args* a)))
	    (apply #'filespecs-fload default-fload-args*)))))      

(defun filespecs-fload (specs &optional (flags !()) (*readtable* *readtable*))
   (let ((*load-verbose* false))
      (let ((force-flag nil) (force-compile false) (must-ask false))
	(dolist (flag flags)
	    (case flag
	       (- (setq force-flag false)
		  (setq force-compile false))
	       (-a (setq must-ask true))
	       (-c (setq force-compile true)
		   (setq force-flag true))
	       (-f (setq force-flag true))
	       (t (cerror "I will ignore it"
			  "Illegal flag to 'fload': ~s" flag))))
	(dolist (pn (filespecs->ytools-pathnames specs))
  	   (pathname-fload pn force-flag force-compile must-ask)
;;;;	   (setf (Load-progress-rec-whether-compile
;;;;		    (place-load-progress-rec pn))
;;;;	         false)
	  ))))

(defvar final-load* false)
; When building systems, bind to true to minimize soul-searching later.

(defun pathname-fload (pn force-flag force-compile must-ask)
   (cond ((is-Pseudo-pathname pn)
	  (pseudo-pathname-fload pn force-flag force-compile))
	 (t
	  (let ((v (pathname-prop 'version pn)))
	     (cond (v
		    (pathname-fload v force-flag force-compile must-ask))
		   (t
		    (let ((lprec (place-load-progress-rec pn)))
		       (lprec-load lprec
				   force-flag
				   force-compile
				   must-ask))))))))

;;;;(defun fload-obj-or-source (pn force-flag force-compile always-ask)
;;;;   (let ((lprec (place-load-progress-rec pn)))
;;;;      (lprec-load-obj-or-source lprec force-flag force-compile always-ask)))

(defun lprec-load (lprec force-flag force-compile must-ask)
   (let ()   ;;;;(lprec-pn (Load-progress-rec-pathname lprec))
;;;;      (setq lprecs* (cons lprec lprecs*))
      (let ((supporters (lprec-find-supporters lprec))
	    (when-reached-status (Load-progress-rec-when-reached lprec))
	    )
	 (multiple-value-bind (ur-mod-time ldble-mod-time)
			      (lprec-find-version-modtimes lprec)
	    (let ((changed-supporters
		     (cond (ldble-mod-time
			    (files-changed-since supporters ldble-mod-time))
			   (t !()))))
	       (cond ((or force-flag
			  (not (achieved-load-status lprec ':loaded))
			  (> ur-mod-time when-reached-status)
		      ;;; This is not necessary, because calling 'achieved-load-status'
		      ;;; above did the check already.--
;;;;			  (or (> ur-mod-time ldble-mod-time)
;;;;			      (> ur-mod-time when-reached-status))
			  (and (not (null changed-supporters))
			       (or (some (\\ (cs) (> (cadr cs) when-reached-status))
					 changed-supporters)
				   (not (memq (Load-progress-rec-whether-compile
					       lprec)
					      '(:object :source))))))
;;;;		      (dbg-save ur-mod-time ldble-mod-time changed-supporters
;;;;				(wc (Load-progress-rec-whether-compile lprec))
;;;;				lprec)
;;;;		      (breakpoint lprec-load "Loading!!?")
		      ;; There's a reason to load or reload, possibly
		      ;; after (re)compilation.
		      (let ((src-version (lprec-find-source-pathname lprec))
			    (obj-version (lprec-find-object-pathname lprec)))
			 (multiple-value-bind (version-to-load src-or-obj)
					      (lprec-check-compile
						 lprec
						 src-version obj-version
						 (cond (force-compile
							':just-do-it)
						       ((not (probe-file obj-version))
							':no-object)
						       ((> ur-mod-time ldble-mod-time)
							':out-of-date)
						       ((not (null changed-supporters))
							':supporter-out-of-date)
						       (must-ask
							':have-to-ask)
						       (t
							':no-reason))
						 changed-supporters
						 must-ask)
			    (cond (version-to-load
				   (pathname-really-fload
				      version-to-load src-or-obj lprec))
				  (t
				   (format *error-output*
					   "Nothing to load~%"))))))
		     (t
		      (dolist (pn changed-supporters)
			 (fload-if-recompile (car pn))))
		      ))))))

(defun lprec-find-supporters (lprec)
   (labels ((walk-through-supporters (lpr)
;;;;	       (trace-around walk-through
;;;;		  (:> "(walk-through: " lpr ")")
	       (list-copy
		  (cond ((= (Load-progress-rec-supp-timestamp lpr)
			    file-op-count*)
			 (Load-progress-rec-supporters lpr))
			(t
			 (setf (Load-progress-rec-supp-timestamp lpr)
			       file-op-count*)
			 ;; Slurp to set immediate supporters --
			 (setf (Load-progress-rec-run-time-depends-on lpr) !())
			 (setf (Load-progress-rec-compile-time-depends-on lpr)
			       !())
			 (lprec-slurp lprec false ':header-only)
			 (let ((curr-supps (Load-progress-rec-supporters lpr)))
			    (setf (Load-progress-rec-supporters lpr) !())
			    ;; -- Set to harmless value in case we come across
			    ;; this same lprec during the recursive walk.
			    (setf (Load-progress-rec-supporters lpr)
				  (pns-nodups
				     (nconc
					 (walk-down
					    (Load-progress-rec-run-time-depends-on
						lpr))
					 (walk-down
					    (Load-progress-rec-compile-time-depends-on
						lpr))
					 curr-supps)))))))
;;;;		  (:< (val &rest _) "walk-through: " val))
	       )
	    (walk-down (immediate-supporters)
	       (mapcan (\\ (s)
			  (cond ((is-Pseudo-pathname s)
				 !())
				(t
				 (cons s (walk-through-supporters
					    (place-load-progress-rec s))))))
		       immediate-supporters))
	    (pns-nodups (pnl)
;;;;	       (trace-around pns-nodups
;;;;		  (:> "(pns-nodups: " pnl ")")
	       (remove-duplicates pnl :test #'pn-equal)
;;;;		  (:< (val &rest _) "pns-nodups: " val))
	       ))
      (let ((l (walk-through-supporters lprec)))
;;;;	 (format t "Before nodups: supporters = ~s~%" l)
;;;;	 (pns-nodups l)
	 l
	 )))

;;; Presupposing that 'lprec' represents a loaded file whose loadable-version
;;; is up to date, find supporters that have changed since the last time
;;; the loadable-version changed.
;;; Returns list of pairs (supporter when-it-last-changed)
(defun files-changed-since (supps loadable-mod-time)
   (mapcan (\\ (supp)
	      (multiple-value-bind (supp-ur-time ign)
				   (lprec-find-version-modtimes
				      (place-load-progress-rec supp))
				   (declare (ignore ign))
		 (cond ((> supp-ur-time loadable-mod-time)
			(list (tuple supp supp-ur-time)))
		       (t !()))))
	   supps))

(defun fload-if-recompile (ytpn)
   (let ((src-version (pathname-source-version ytpn))
	 (lprec (place-load-progress-rec ytpn)))
      (cond ((and (achieved-load-status lprec ':loaded)
		  (> (lprec-find-version-modtimes lprec)
		     (Load-progress-rec-when-reached lprec)))
	     (loop
		(format *query-io*
		   "Source file older than loaded version of ~s~% Reload it now (recompiling if appropriate)? (y/n, \\\\ to abort) "
		   src-version)
		(let ((ans (keyword-if-sym (read *query-io*))))
		   (labels (

	  ;; Indentation for local functions
	  (compile-and-load ()
	   (let ((obj-version
		    (lprec-find-supporters-and-compile lprec true)))
	      (cond (obj-version
		     (pathname-really-fload obj-version ':object lprec)))))

	  (ask-if-compile ()
	  ;;;;    (trace-around ask-if-compile
	  ;;;;       (:> "(ask-if-compile: " ")")
	   (loop
	      (format *query-io* "Should I recompile ~s (y/n, \\\\ to abort)?"
		      src-version)
	      (let ((ans (keyword-if-sym (read *query-io*))))
		 (case ans
		   ((:y :yes :t)
		    (compile-and-load)
		    (return ':compile))
		   ((:n :no :nil)
		    (pathname-really-fload src-version ':source lprec)
		    (return ':source))
		   (:\\
		    (throw 'fload-abort 'fload-aborted))
		   (t
		    (format *query-io* "???~%")))))
	  ;;;;       (:< (val &rest _) "ask-if-compile: " val))
	   )

	  (ask-what-this-means ()
	   (let ((obj-version (pathname-object-version src-version true)))
	      (cond (obj-version
		     (loop 
		       (format *query-io*
			       "Previously the object version of ~s was loaded;~% shall I reload it, recompile and reload it, or skip? (Type l, c, or /; \\\\ to abort): "
			       src-version)
			(case (keyword-if-sym (read *query-io*))
			   ((:l :load)
			    (pathname-really-fload obj-version ':object lprec)
			    (return))
			   ((:c :compile)
			    (compile-and-load)
			    (return))
			   ((:/ :s :skip)
			    (return))
			   (:\\ (throw 'fload-abort 'fload-aborted))
			   (t (format *query-io* "???~%")))))
		    (t
		     (format *query-io*
			     "The object version of has apparently been deleted~%"
			     src-version)
		     (ask-if-compile))))))

	  ;; Resume normal indentation
	  ;; We're in the middle of a loop here, waiting for a valid input to the question
	  ;; "Reload it now?"	     
	  ;; so we have to make sure to exit if we get a valid input.

		      (case ans
			 ((:y :yes :+)
			  (ecase (Load-progress-rec-whether-compile lprec)
			     (:compile
			      (compile-and-load))
			     (:source
			       (pathname-really-fload src-version ':source lprec))
			     ((:ask :unknown nil)
			      (ask-if-compile))
			     (:object
			      (ask-what-this-means)))
			  (return))
			 ((:\\)
			  (throw 'fload-abort 'fload-aborted))
			 ((:n :no :-)
			  (return))
			 (t
			  (format *query-io* "???~%"))))))))))

(defun pseudo-pathname-fload (pn force-flag force-compile)
	  (funcall (Pseudo-pathname-loader pn)
		   pn force-flag force-compile false))

;;; Return obj-version, if obj-version is now up-to-date (or is preferred
;;; to the source for some reason, rational or perverse);
;;; src-version, if should ignore object version and its possible existence.
;;; + second arg, :source or :object, specifying what the first
;;; arg refers to.
(defun lprec-check-compile (lprec src-version obj-version
			    why changed-supporters must-ask)
   (let ((whether-compile (Load-progress-rec-whether-compile
			     lprec))
	 (obj-version-exists (probe-file obj-version)))
      (let ((action
	       (cond (src-version
		      (cond ((and (not must-ask)
				  (or (memq why '(:just-do-it :no-reason))
				      (cond ((eq whether-compile ':ask)
					     false)
					    ((eq whether-compile ':unknown)
					     (not (eq fload-compile* ':ask)))
					    (t true))))
			     ;; No need to ask
			     (cond ((eq why ':no-reason)
				    (cond (obj-version-exists
					   ':object)
					  (t
					   ':source)))
				   ((eq why ':just-do-it)
				    (ask-next-time lprec)
				    ':compile)
				   ((eq whether-compile ':unknown)
				    (setf (Load-progress-rec-whether-compile
					     lprec)
				          fload-compile*)
				    fload-compile*)
				   (t whether-compile)))
			    (t
			     ;; Ask
			     (ask-whether-compile
			        lprec src-version obj-version-exists
				why changed-supporters))))
		     (t ':object))))
	 (multiple-value-bind (file-version which)
			      (ecase action
				 (:compile 
				  (lprec-compile lprec true changed-supporters))
				 (:object
				  (cond (obj-version-exists
					 (values obj-version ':object))
					(t
					 (values src-version ':source))))
				 (:source
				  (values src-version ':source)))
	     (cond (file-version
		    (note-load-status lprec ':maybe-compiled)))
	     (values file-version which)))))

;;; Returns :compile, :source, :object
(defun ask-whether-compile (lprec src-version obj-version-exists
			    why changed-supporters)
   (cond ((eq why ':supporter-out-of-date)
	  (format *query-io*
		  "~&The files ~s~%have changed, so that ~% ~s~%may have to be recompiled;~% compile it now? (y/n, or: + = always, - = never, \\\\ to abort) "
		  (mapcar #'car changed-supporters)
		  src-version))
	 ((eq why ':have-to-ask)
	  (format *query-io*
		  "~&Should I compile ~s? (y/n, or: + = always, - = never, \\\\ to abort) "))
	 (t
	  (format *query-io*
		   "~&~a~% ~s~%Compile it now? (y/n, or: + = always, - = never, \\\\ to abort) "
		   (ecase why
		      (:no-object "No object file for ")
		      (:out-of-date "Object file older than source file "))
		   src-version)))
   (let ((ans (keyword-if-sym (read *query-io*))))
      (case ans
	((:y :yes :t) ':compile)
	((:+) (setq fload-compile* ':compile) ':compile)
	((:n :no :nil :-)
	 ;; If supporter out of date, "don't recompile" means "load object if
	 ;; possible"; if file itself out of date, "don't recompile" requires
	 ;; further questioning.
	 (cond ((eq why ':supporter-out-of-date)
		(setf (Load-progress-rec-when-reached lprec)
		      (let ((time (Load-progress-rec-when-reached lprec)))
			 (dolist (cs changed-supporters)
			    (let ((ptime (cadr cs)))
			       (cond ((> ptime time)
				      (setq time ptime)))))
			 time))
		(maybe-remember-disp
		    (cond (obj-version-exists ':object)
			  (t ':compile))
		    lprec))
	       (t
		(let ((obj-or-source (ask-obj-or-source
				        src-version obj-version-exists lprec)))
		   (cond ((eq ans ':-)
			  (cond ((y-or-n-p
				  "Because you typed \"-\" I will now set fload-compile* 
	to ~s, ~%  thus setting a global policy to avoid recompilation and~%  favor files of that type.~%  Are you sure that's what you want?")
				 (setq fload-compile* obj-or-source))
				(t (format *query-io* "Not setting fload-compile*~%")))))
		   obj-or-source))))
	((:\\)
	 (throw 'fload-abort 'fload-aborted))
	(t
	 (format *query-io* "???~%")
	 (ask-whether-compile lprec src-version obj-version-exists
			      false changed-supporters)))))

(defun ask-obj-or-source (src-version obj-version-exists lprec)
   (let ((obj-or-source
	    (progn
	       (format *query-io*
		    "Load object or source (obj/source, \\\\ to abort) ")
	       (case (keyword-if-sym (read *query-io* false eof*))
		  ((:o :obj :object)
		   (maybe-remember-disp ':object lprec))
		  ((:s :sou :src :sour :source)
		   (maybe-remember-disp ':source lprec))
		  (:\\ (throw 'fload-abort 'fload-aborted))
		  (t
		   (format *query-io* "???~%")
		   (ask-obj-or-source src-version obj-version-exists lprec))))))
      (cond ((and (eq obj-or-source ':object)
		  (not obj-version-exists))
	     (format *query-io*
		"Because this file has no object file, I will load the source file~%")))
      obj-or-source))

(defun maybe-remember-disp (disp lprec)
  (format *query-io*
	  "Remember this action for future encounters with this file (y/n, \\\\ to abort)? ")
  (case (keyword-if-sym (read *query-io*))
     ((:y :yes :t)
      (setf (Load-progress-rec-whether-compile lprec)
	    disp))
     ((:n :no)
      (setf (Load-progress-rec-whether-compile lprec)
	    ':ask))
     ((:\\)
      (throw 'fload-abort 'fload-aborted))
     (t
      (format *query-io* "???~%")
      (maybe-remember-disp disp lprec)))
  disp)

(defvar loading-stack*   nil)
   ;; -- stack of Pathnames of files currently being loaded

(defvar loading-src-or-obj* false) ; :source, :object, or false

(defun pathname-really-fload (pn src-or-obj lprec)
  (if (member pn loading-stack* :test #'pathname-equal)
      (if fload-verbose*
	  (format *query-io* "~&Warning -- ~s already being loaded~%" pn))
      (let ((real-pn (pathname-resolve pn true)))
	 (let  ((fload-indent* (+ 3 fload-indent*))
		(now-loading*   pn)
		(loading-src-or-obj* src-or-obj)
		(now-compiling* false)
		(now-slurping* false)
		(now-loading-lprec* lprec)
		(loading-stack* (cons pn loading-stack*))
;;;;		(post-file-transduce-hooks* !())
		#+:excl (excl:*record-source-file-info* true)
		#+:excl (excl:*load-source-file-info* true))
	    (fload-op-message "Loading" pn real-pn "...")
	    (with-post-file-transduction-hooks
	       (cleanup-after-file-transduction
	           (let ((*package* *package*)
			 (*readtable* *readtable*))
		      (load real-pn))))
	    (fload-op-message "...loaded" pn false "")
	    (note-load-status
	       lprec (cond (final-load* ':frozen)
			   (t ':loaded)))))))

(defun filespecs->lprecs (specs)
   (let ((pnl (filespecs->pathnames specs)))
     (mapcar (lambda (pn)
	        (setq pn (make-Pathname
			    :type false
			    :defaults pn))
		(place-load-progress-rec pn))
	     pnl)))

(defvar fcompl-flags* '(- -f))

(defmacro fcompl (&rest specs)
  `(do-fcompl ',specs))

(defvar default-fcompl-args* '())

(defun do-fcompl (specs)
  (cond ((not file-op-in-progress*)
	 (setq file-op-count* (+ file-op-count* 1))))
  (let ((file-op-in-progress* true))
     (with-compilation-unit ()
	(file-op-defaults-update specs fcompl-flags*
				 (lambda () default-fcompl-args*)
				 (lambda (a)
				   (setq default-fcompl-args* a)))
	(apply #'filespecs-fcompl default-fcompl-args*))))

(defun filespecs-fcompl (specs flags *readtable*)
   (let ((*load-verbose* false))
      (let ((force-flag nil))
	(dolist (flag flags)
	    (case flag
	       (-f (setq force-flag true))
	       (- (setq force-flag false))
	       (t (cerror "I will ignore it"
			  "Illegal flag to 'fcompl': ~s" flag))))
	(dolist (pn (filespecs->ytools-pathnames specs))
	   (pathname-fcompl pn force-flag)
	   (setf (Load-progress-rec-whether-compile
		    (place-load-progress-rec pn))
	         false)))))

(defvar fcompl-reload* ':ask)

(defun pathname-fcompl (pn force-flag)
   (cond ((is-Pseudo-pathname pn)
	  (pseudo-pathname-fcompl pn force-flag))
	 (t
	  (let ((v (pathname-prop 'version pn)))
	     (cond (v
		    (pathname-fcompl v force-flag))
		   (t
		    (let ((lprec (place-load-progress-rec pn)))
		       (let ((maybe-obj-version
				   (lprec-find-supporters-and-compile
				       lprec force-flag)))
			  (cond ((and maybe-obj-version
				      (load-after-compile))
				 (pathname-really-fload
				    maybe-obj-version ':object lprec)))))))))))

(defun lprec-find-supporters-and-compile (lprec force-flag)
   (multiple-value-bind (ign ldble-mod-time)
			(lprec-find-version-modtimes lprec)
			(declare (ignore ign))
      (lprec-compile lprec force-flag
	 (cond (ldble-mod-time
		(files-changed-since
		       (lprec-find-supporters lprec)
		       ldble-mod-time))
	       (t !())))))

;;; Returns object version if object version is now up to date.
(defun lprec-compile (lprec force-flag changed-supporters)
   (let ((pn (Load-progress-rec-pathname lprec)))
      (let ((src-version (lprec-find-source-pathname lprec)))
	 (multiple-value-bind (ur-time ldble-time)
			      (lprec-find-version-modtimes lprec)
	    (cond ((or force-flag
		       (or (not ur-time)
			   (not ldble-time)
			   (> ur-time ldble-time))
		       (not (null changed-supporters))
		       (not (achieved-load-status lprec ':maybe-compiled)))
		   (cond ((null src-version)
			  (error "Fcompl couldn't find file ~s" pn))
			 (t
			  (pathname-slurp pn false ':at-least-header)
			  ;; Make sure slurping didn't encounter
			  ;; a :no-compile
			  (cond ((and (not force-flag)
				      (eq (Load-progress-rec-whether-compile
						  lprec)
					  ':source))
				 (lprec-recompute-version-modtimes lprec)
				 src-version)
				(t
				 (dolist (rtsupp
					     (Load-progress-rec-run-time-depends-on
						lprec))
				    (pathname-slurp rtsupp false ':at-least-header))
				 (dolist (chsupp changed-supporters)
				    (pathname-slurp (car chsupp) false
						    ':at-least-header))
				 (dolist (ctsupp
					   (Load-progress-rec-compile-time-depends-on
						lprec))
				    (pathname-fload ctsupp false false false))
				 (pathname-with-suffix-fcompl
				    src-version lprec))))))
		   (t
		    (format *query-io* "~s~% up to date, not compiling~%"
				       (Load-progress-rec-pathname lprec))
		    false))))))

(defun pseudo-pathname-fcompl (pn force-flag)
	  (funcall (Pseudo-pathname-compiler pn)
		   pn force-flag))

(defvar fcompl-always* false)

(defun pathname-with-suffix-fcompl (src-version lprec)
      (let ((old-obj-write-time
	       (let ((old-obj-version (pathname-object-version src-version true)))
		  (and old-obj-version
		       (pathname-write-time old-obj-version)))))
	 (pathname-really-fcompl src-version)
	 (let ((new-obj-version (pathname-object-version src-version true)))
	    (let ((success
		      (and new-obj-version
			   (or (not old-obj-write-time)
			       (> (pathname-write-time new-obj-version)
				  old-obj-write-time)))))
	       (fcompl-log src-version (and success new-obj-version))
	       (cond (success
		      (note-load-status lprec ':maybe-compiled)
		      (lprec-recompute-version-modtimes lprec)
		      (ask-next-time lprec)
		      new-obj-version)
		     (t
		      (format *error-output*
			      "Recompiling failed to produce object file~%")
;;;;		      (dbg-save old-obj-version new-obj-version src-version lprec)
;;;;		      (breakpoint pathname-with-suffix-fcompl
;;;;			 "Recompilation failure "
;;;;			 old-obj-version " => " new-obj-version)
		      false))))))

(defun ask-next-time (lprec)
   (cond ((not (eq (Load-progress-rec-whether-compile lprec)
		   ':compile))
	  (setf (Load-progress-rec-whether-compile lprec)
	        ':ask))))

(defvar fcompl-log* true)
(defvar debuggability* 0)

(defun pathname-really-fcompl (pn)
  (let ((ov (pathname-object-version pn false)))
     (let ((real-pn (pathname-resolve pn true))
	   (real-ov (pathname-resolve ov false)))
	(let  ((now-compiling*    pn)
	       (now-loading* false)
	       (now-slurping* false)
	       (now-loading-lprec* nil)
	       (debuggability* debuggability*)
	       (fload-indent* (+ 3 fload-indent*)))
	   (fload-op-message "Beginning compilation on" pn real-pn "...")
	   (with-post-file-transduction-hooks
	      (cleanup-after-file-transduction
		 (let ((*compile-verbose* false))
;;;;		    (out :% " ov = " ov :% :%)
		    (cond (ov 
;;;;			   (trace-around compile-it
;;;;			      (:> "(compile-file: output: " real-ov ")")
			   (compile-file real-pn :output-file real-ov)
;;;;			      (:< (val &rest _) "compile-file: " val))
			   )
			  (t (compile-file real-pn)))))))
	(fload-op-message "...compiled to" real-ov false ""))))

(defun fcompl-log (src-pn obj-pn-if-succeeded)
  (let ((log-pn (pathname-resolve
		   (make-Pathname :host (Pathname-host src-pn)
				  :device (Pathname-device src-pn)
				  :directory (Pathname-directory src-pn)
				  :name "Niscom_Log")
		   true)))
    (let ((oldlog
	   (cond ((probe-file log-pn)
		  (with-open-file (ss log-pn :direction ':input)
                     (read ss)))
		 (t !())))
	  (fname (build-symbol (Pathname-name src-pn))))
       (let ((newlog
		(cond (obj-pn-if-succeeded
                       (cons (list fname
                                   (current-time-string)
                                   ;;;;(compile-opt-lev)
				   )
                             (mapcan (lambda (x)
					(cond ((eq (car x) fname)
					       '())
					      (t (list x))))
				     oldlog)))
                      (t
		       (cons (list fname
                                   (current-time-string)
                                   "Compilation failed")
                             oldlog)))))
          (with-open-file (ss log-pn :direction ':output
			             :if-exists ':supersede)
             (let  ((*print-level* nil)
                    (*print-length* nil)
		    (*print-pretty* t))
		(prin1 newlog ss)
		))))))

;;;;(defun compile-opt-lev () `((compile-opt ,compile-opt*)))

(defun current-time-string ()
   (multiple-value-bind (sec mnt hr day mo yr wkday dst tz)
                       (get-decoded-time)
                       (declare (ignore sec wkday dst tz))
      (with-output-to-string (ss)
         (format ss "~s:~s ~a ~s ~s"
              hr mnt
	      (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
		      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
		    (- mo 1))
	      day yr))))

(defun load-after-compile ()
   (and fcompl-reload*
	(or (not (eq fcompl-reload* ':ask))
	    (progn
	       (format *query-io*
		    "Load newly compiled file? ")
	       (memq (keyword-if-sym
			(read *query-io* false false))
		     '(:y :yes :t))))))				

#|
(defun times-for-a-change (load-time ur-change-time obj-change-time must-load-source)
   (or (and load-time
	    (> ur-change-time load-time))
       (and ur-change-time
	    (not must-load-source)
	    (or (not obj-change-time)
		(> ur-change-time obj-change-time)))))

;;; Returns write time of source version if has changed.
(defun changed-since-loaded (supp)
   (let ((lprec (place-load-progress-rec supp)))
      (cond ((achieved-load-status lprec ':loaded)
	     (multiple-value-bind (ur-write-time ldbl-write-time)
				  (lprec-find-version-modtimes lprec)
				  (declare (ignore ldbl-write-time))
                (> ur-write-time
		   (Load-progress-rec-when-reached lprec))))
	    (t false))))
|#

(defun needs-recompile (lprec)
  (and (not (eq (Load-progress-rec-whether-compile lprec)
		':source))
       (let ((lprec-pn (pathname-resolve (Load-progress-rec-pathname lprec)
					 false)))
	  (let ((obj-version (pathname-object-version lprec-pn true)))
	     (and obj-version
		  (>= (pathname-write-time obj-version)
		      (pathname-write-time
			 (pathname-source-version lprec-pn))))))))

(defun always-slurp ()
;;;;   (trace-around always-slurp
;;;;      (:> "(always-slurp: now-loading* = " now-loading*
;;;;	  " loading-src-or-obj* = " loading-src-or-obj* ")")
   (cond ((and now-loading* (eq loading-src-or-obj* ':source))
	  (pathname-slurp now-loading* false ':whole-file)))
;;;;      (:< (val &rest _) "always-slurp: " val))
   )

;;;;(defmacro always-slurp ()
;;;;   (breakpoint always-slurp "okay")
;;;;   `(progn (eval-when (:execute)
;;;;	      (format t "~%Evaluated!~%")
;;;;	      (cond (now-loading*
;;;;		     (pathname-slurp now-loading* false ':whole-file))))
;;;;	   (eval-when (:compile-toplevel)
;;;;	      (cond (now-compiling*
;;;;		     (pathname-slurp now-compiling* false ':whole-file))))
;;;;	   ))
	  
(datafun to-slurp always-slurp
   (defun :^ (_ _)
;;;;      (trace-around always-slurp-to-slurp
;;;;	 (:> "(always-slurp-to-slurp: " ")")
      (cond ((not (eq slurping-how-much* ':header-only))
	     (setq slurp-whole-file* true)))
;;;;      (format t "slurp-whole-file* = ~s // slurping-how-much* = ~s~%"
;;;;	      slurp-whole-file* slurping-how-much*)
      false
;;;;	 (:< (val &rest _) "always-slurp-to-slurp: " val))
      ))

(defun keyword-if-sym (x)
   (cond ((is-Symbol x)
	  (intern (symbol-name x) keyword-package*))
	 (t x)))

(defmacro debuggable (n)
   (multiple-value-bind (speed safety space debug)
			(cond ((> n 0)
			       (values 2 3 0 3))
			      ((= n 0)
			       (values 2 1 1 2))
			      (t
			       (values 3 1 2 0)))
;;;;      (out (:to *error-output*) "debug = " debug :%)
      `(eval-when (:compile-toplevel :execute)

	  (declaim (optimize (speed ,speed) (safety ,safety)
			     (space ,space) (debug ,debug)))
	  (setq debuggability* ,n))))
	 