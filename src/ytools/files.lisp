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
      (let ((force-flag nil) (force-compile false))
	(dolist (flag flags)
	    (case flag
	       (- (setq force-flag false)
		  (setq force-compile false))
	       (-a (setq fload-compile* ':ask))
	       (-c (setq force-compile true)
		   (setq force-flag true))
	       (-f (setq force-flag true))
	       (t (cerror "I will ignore it"
			  "Illegal flag to 'fload': ~s" flag))))
	(dolist (pn (filespecs->ytools-pathnames specs))
  	   (pathname-fload pn force-flag force-compile)
;;;;	   (setf (Load-progress-rec-whether-compile
;;;;		    (place-load-progress-rec pn))
;;;;	         false)
	  ))))

(defvar final-load* false)
; When building systems, bind to true to minimize soul-searching later.

(defun pathname-fload (pn force-flag force-compile)
   (cond ((is-Pseudo-pathname pn)
	  (pseudo-pathname-fload pn force-flag force-compile))
	 (t
	  (let ((v (pathname-prop 'version pn)))
	     (cond (v
		    (pathname-fload v force-flag force-compile))
		   (t
		    (let ((lprec (place-load-progress-rec pn)))
		       (lprec-load lprec
				   force-flag
				   force-compile
				   false))))))))

;;;;(defun fload-obj-or-source (pn force-flag force-compile always-ask)
;;;;   (let ((lprec (place-load-progress-rec pn)))
;;;;      (lprec-load-obj-or-source lprec force-flag force-compile always-ask)))

;;; Returns true if already up to date.
(defun lprec-load (lprec force-flag force-compile always-ask)
   (let ((lprec-pn (pathname-resolve (Load-progress-rec-pathname lprec)
				     false)))
      (cond ((is-Pseudo-pathname lprec-pn)
	     (pseudo-pathname-fload lprec-pn force-flag force-compile))
	    (t
;;;;	     (setq load-pn* lprec-pn)
	     (let ((src-version (pathname-source-version lprec-pn))
		   (obj-version (pathname-object-version lprec-pn true)))
		(cond (src-version
		       (let ((already-loaded (achieved-load-status
					        lprec ':loaded))
			     (changed-supporters
				(find-changed-supporters
				   lprec)))
			  (let ((up-to-date
				   (or (eq (Load-progress-rec-whether-compile lprec)
					    ':source)
				       (and obj-version
					    (>= (pathname-write-time
						   obj-version)
						(pathname-write-time
						   src-version))))))
			     (cond ((or force-flag
					force-compile
					(not already-loaded)
					(not up-to-date)
					(not (null changed-supporters)))
				    (multiple-value-bind
					     (version-to-load src-or-obj)
					     (lprec-check-compile
						 lprec src-version
						 (cond (force-compile
							':just-do-it)
						       ((not up-to-date)
							':out-of-date)
						       ((not (null changed-supporters))
							':supporter-out-of-date)
						       (t false))
						 changed-supporters
						 force-compile
						 always-ask)
;;;;				       (out "changed-supporters = "
;;;;					    changed-supporters :%)
				       (cond (version-to-load
					      (pathname-really-fload
						 version-to-load src-or-obj lprec))
					     ((not (null changed-supporters))
					      (dolist (pn changed-supporters)
;;;;						 (out "Reloading " (car pn) :%)
						 (fload-if-recompile (car pn))))))))
			     (and already-loaded up-to-date))))
		      (obj-version
		       (cond (force-compile
			      (cerror "I will try loading the object version"
				      "Can't compile ~s" lprec-pn
				      " -- no source file")))
		       (pathname-really-fload obj-version ':object lprec))
		      (t
		       (error "No source or object file for ~s"
			      lprec-pn)))
		       true)))))

(defun fload-if-recompile (ytpn)
   (cond ((changed-since-loaded ytpn)
	  (let ((src-version (pathname-source-version ytpn)))
	     (loop
		(format *query-io*
		   "Source file older than loaded version of ~s~% Reload it now (recompiling if appropriate)? (y/n, \\\\ to abort) "
		   src-version)
		(let ((ans (keyword-if-sym (read *query-io*)))
		      (lprec (place-load-progress-rec ytpn)))
		   (labels (

 ;; Indentation for local functions
 (compile-and-load ()
    (let ((obj-version
	     (lprec-compile lprec true !())))
       (cond (obj-version
	      (pathname-really-fload obj-version ':object lprec)))))

 (ask-whether-compile ()
;;;;    (trace-around ask-whether-compile
;;;;       (:> "(ask-whether-compile: " ")")
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
	     (throw 'fload-abort 'fload-aborted)))))
;;;;       (:< (val &rest _) "ask-whether-compile: " val))
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
	      (ask-whether-compile))))))
 
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
			     ((:ask nil)
			      (ask-whether-compile))
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

;;; Return obj-version if obj-version is now up-to-date;
;;; src-version if should ignore object version; else false
;;; + second arg, :source, :object, or false, specifying which.
(defun lprec-check-compile (lprec src-version why changed-supporters
			    force-compile always-ask)
   (let ((obj-version (pathname-object-version src-version true)))
      (cond ((or force-compile
		 (not obj-version)
		 (memq why '(:out-of-date :supporter-out-of-date)))
	     (let ((action
		      (should-compile
			 lprec src-version obj-version
			 always-ask why changed-supporters)))
		(ecase action
		   (:compile 
		    (lprec-compile lprec true changed-supporters))
		   (:object
		    (values (pathname-object-version src-version false)
			    ':object))
		   (:source
		    ;;;;(lprec-slurp-all-supporters lprec)
		    (values src-version ':source))
		   (:noload
		    (values false false)))))
	    (t
	     (note-load-status-if-not-achieved lprec ':maybe-compiled)
	     (values obj-version ':object)))))

;;; Returns :compile, :source, :object, or :noload
(defun should-compile (lprec src-version obj-version-exists
		       always-ask why changed-supporters)
  (cond ((not (eq (symbol-package fload-compile*)
		  keyword-package*))
	 (setq fload-compile* (intern (symbol-name fload-compile*)
				       keyword-package*))))
  (let ((prev (and (not always-ask)
		   (not (eq why ':just-do-it))
                   (eq fload-compile* ':ask)
		   (Load-progress-rec-whether-compile lprec))))
    (cond ((eq why ':just-do-it) ':compile)
	  ((and prev (not (eq prev ':ask)))
	   (cond ((not (is-Symbol prev))
		  (error "Illegal whether-compile field ~s"
			 prev)))
	   prev)
	  ((or always-ask
	       (eq fload-compile* ':ask)
	       (eq prev ':ask))
	   (cond ((eq why ':supporter-out-of-date)
		  (format *query-io*
			  "~&The files ~s~%have changed, so that ~% ~s~%may have to be recompiled;~% compile it now? (y/n, or: + = always, - = never, \\\\ to abort) "
			  (mapcar #'car changed-supporters)
			  src-version))
		 (t
		   (format *query-io*
			   "~&~a~% ~s~%Compile it now? (y/n, or: + = always, - = never, \\\\ to abort) "
			   (ccase why
			      (:out-of-date
			       (cond (obj-version-exists
				      "Object file older than source file ")
				     (t "No object file for ")))
			      (:supporter-out-of-date
			       "Change in some file depended on by "))
			   src-version)))
           (let ((ans (keyword-if-sym (read *query-io*))))
              (case ans
                ((:y :yes :t) ':compile)
                ((:+) (setq fload-compile* ':compile) ':compile)
                ((:n :no :nil :-)
		 (cond ((eq why ':supporter-out-of-date)
			(setf (Load-progress-rec-when-reached lprec)
			      (let ((time (Load-progress-rec-when-reached lprec)))
				 (dolist (cs changed-supporters)
				    (let ((ptime (cadr cs))) ;;;;(pathname-write-time cs)
				       (cond ((> ptime time)
					      (setq time ptime)))))
				 time))
			':noload)
		       (t
			(ask-obj-or-source src-version obj-version-exists lprec))))
		((:\\)
		 (throw 'fload-abort 'fload-aborted))
                (t
                 (should-compile lprec src-version obj-version-exists
				       false always-ask changed-supporters)))))
	  ((eq fload-compile* true) ':compile)
	  ((not fload-compile*)
           ':source)
	  (t fload-compile*))))

(defun ask-obj-or-source (src-version obj-version-exists lprec)
   (cond (obj-version-exists
	  (format *query-io*
	       "Load object or source (obj/source, or: + = always source, - = always object, \\\\ to abort) ")
	  (case (keyword-if-sym (read *query-io* false eof*))
	     ((:o :obj :object)
	      (maybe-remember-disp ':object lprec))
	     ((:s :sou :src :sour :source)
	      (maybe-remember-disp ':source lprec))
	     (:+ (setq fload-compile* ':source) ':source)
	     (:- (setq fload-compile* ':object) ':object)
	     (:\\ (throw 'fload-abort 'fload-aborted))
	     (t (ask-obj-or-source src-version obj-version-exists lprec))))
	 (t
	  (maybe-remember-disp ':source lprec))))

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
;;;;	    (setf (Load-progress-rec-status lprec) ':loaded)
;;;;	    (setf (Load-progress-rec-when-reached lprec)
;;;;	          (get-universal-time))
	    (fload-op-message "...loaded" pn false "")
	    (note-load-status-if-not-achieved
	       lprec (cond (final-load* ':frozen)
			   (t ':loaded)))))))

(defun filespecs->lprecs (specs)
   (let ((pnl (filespecs->ytools-pathnames specs)))
     (mapcar (lambda (pn)
	        (setq pn (make-Pathname
			    :type false
			    :defaults pn))
		(place-load-progress-rec pn))
	     pnl)))

(defun was-updated-since-loaded (pn)
  (let ((when-loaded (pathname-prop 'loaded pn)))
    (cond ((null when-loaded) true)
	  ((eq when-loaded true) false)
	  (t (let ((write-time (pathname-write-time pn)))
	       (and pn (> write-time when-loaded)))))))

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
				   (lprec-compile
				      lprec force-flag
				      (find-changed-supporters
					 lprec))))
			  (cond ((and maybe-obj-version
				      (load-after-compile))
				 (pathname-really-fload
				    maybe-obj-version ':object lprec)))))))))))

;;; Returns object version if object version is now up to date.
(defun lprec-compile (lprec force-flag changed-supporters)
   (let ((pn (pathname-resolve (Load-progress-rec-pathname lprec)
			       false)))
      (cond ((is-Pseudo-pathname pn)
	     (pseudo-pathname-fcompl pn force-flag))
	    (t
	     (let ((src-version (pathname-source-version pn)))
		(cond ((or force-flag
			   (not (null changed-supporters))
			   (not (achieved-load-status lprec ':maybe-compiled)))
		       (cond ((null src-version)
			      (error "Fcompl couldn't find file ~s"
				     pn))
			     (t
			      (pathname-slurp pn false ':at-least-header)
			      ;; Make sure slurping didn't encounter
			      ;; a :no-compile
			      (cond ((and (not force-flag)
					  (eq (Load-progress-rec-whether-compile
						      lprec)
					      ':source))
				     src-version)
				    (t
;;;;				     (out "huh?? force-flag = " force-flag
;;;;					  " whether = "
;;;;					  (Load-progress-rec-whether-compile
;;;;						      lprec)
;;;;					  :%)
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
					(pathname-fload ctsupp false false))
				     (pathname-with-suffix-fcompl
					src-version lprec))))))
		      (t
		       (format *query-io* "~s up to date, not compiling~%"
					  (Load-progress-rec-pathname lprec))
		       false
		       ;;;;(pathname-object-version src-version false)
		       )))))))

(defun pseudo-pathname-fcompl (pn force-flag)
	  (funcall (Pseudo-pathname-compiler pn)
		   pn force-flag))

(defvar fcompl-always* false)

(defun pathname-with-suffix-fcompl (src-version lprec)
;;;;     (cond (obj-version
;;;;	    (setq obj-version (maybe-delete-stale-file obj-version))))
   (pathname-really-fcompl src-version)
   (incf file-op-count*)
   (let ((obj-version (pathname-object-version src-version true)))
      (let ((success
	        (and obj-version
		     (> (pathname-write-time obj-version)
			(pathname-write-time src-version)))))
	 (fcompl-log src-version (and success obj-version))
	 (cond (success
		(note-load-status-if-not-achieved lprec ':maybe-compiled)
		obj-version)
	       (t
		(format *error-output*
			"Recompiling failed to produce object file~%")
		false)))))

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
		    (cond (ov 
			   (compile-file real-pn :output-file real-ov))
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

;;; Returns a list of pairs (pathname source-write-time),
;;; for each files that has been loaded but has since
;;; changed.
(defun find-changed-supporters (lprec)
   (cond ((return-val can-get-write-times*)
	  (let ((changed !()))
	     (labels ((findem (lprec)
;;;;			 (trace-around findem
;;;;			    (:> "(findem: "
;;;;				(Pathname-name (Load-progress-rec-pathname lprec))
;;;;				")")
			 (cond ((= (Load-progress-rec-supp-timestamp lprec)
				   file-op-count*)
				(dolist (pair (Load-progress-rec-changed-supporters
					          lprec))
				   (cond ((not (assoc (first pair) changed
						      :test #'pn-equal))
					  (on-list pair changed)))))
			       (t
				(setf (Load-progress-rec-supp-timestamp lprec)
				      file-op-count*)
				(setf (Load-progress-rec-changed-supporters lprec)
				      !())
				;; Executed to set depends-on
				;; slots of lprec --
				(lprec-slurp lprec false ':header-only)
				(dolist (ctdep
					 (Load-progress-rec-compile-time-depends-on
						  lprec))
				   (pathname-check ctdep lprec))
				(dolist (rtdep
					 (Load-progress-rec-run-time-depends-on
						  lprec))
				   (pathname-check rtdep lprec))))
;;;;			    (:< (&rest _) "findem: exit"))
			 )
		      (pathname-check (supporter from-lprec)
			 (cond ((not (is-Pseudo-pathname supporter))
				(setq supporter (pathname-resolve
						   supporter true))
				(cond ((not (assoc supporter changed :test #'pn-equal))
				       (let ((v (pathname-prop 'version supporter)))
					  (cond (v
						 (pathname-check v from-lprec))
						(t
						 (supporter-check
						    supporter from-lprec)))))))))
		      (supporter-check (supp from-lprec)
;;;;			 (trace-around supporter-check
;;;;			    (:> "(supporter-check: " supp ")")
			 (let ((supp-lprec (place-load-progress-rec supp)))
			    (let ((wt (changed-since-loaded supp)))
			       (cond (wt
				      (let ((pair (tuple supp wt)))
					 (on-list pair changed)
					 (on-list pair
						  (Load-progress-rec-changed-supporters
							from-lprec)))))
			       (findem supp-lprec)))
;;;;			    (:< (&rest _) "supporter-check: exit"))
			 ))
	     (findem lprec)
	     changed)))
	    (t !())))

;;; Returns write time of source version if has changed.
(defun changed-since-loaded (supp)
   (let ((sv (or (pathname-source-version supp)
		 (pathname-object-version supp true))))
      (cond ((not sv)
	     (error "Can't find source (or object) version of file ~s"
		    supp)))
      (let ((supp-lprec (place-load-progress-rec supp)))
	 (let ((wt (pathname-write-time sv))
	       (lt (Load-progress-rec-when-loaded supp-lprec)))
	   (cond ((and wt lt (> wt lt))
		  wt)
		 (t false))))))

;;; The only reason for this is to avoid the compiler noticing that the last cond
;;; clause in 'find-changed-supporters' is unreachable in most implementations.
(defun return-val (v) v)

(defun always-slurp ()
   (cond ((and now-loading* (eq loading-src-or-obj* ':source))
	  (pathname-slurp now-loading* false ':whole-file))))

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
      (cond ((not (eq slurping-how-much* ':header-only))
	     (setq slurp-whole-file* true)))
;;;;      (format t "slurp-whole-file* = ~s // slurping-how-much* = ~s~%"
;;;;	      slurp-whole-file* slurping-how-much*)
      false))

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
	 