;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: fload.lisp,v 1.1.2.1 2005/03/18 15:19:33 airfoyle Exp $

;;; Copyright (C) 1976-2005
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(fload filespecs-load fcompl filespecs-compile
	     fload-compile* bind-fload-compile* fcompl-reload*
	     fload-versions postponed-files-update
	     debuggable debuggability*)))

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

(defvar file-op-count* 0)

(defparameter fload-flags* '(- -f -c -a -s -o -x -z))
(defparameter filespecs-load-flags* '(-c -s -o -z))
;;; -  -> "Clear sticky flags"
;;; -f -> "Force load even if apparently up to date"
;;; -c -> "Compile and load each file even if apparently up to date"
;;; -a -> "Ask whether to compile each file"
;;; -s -> "Load source, ignoring possibility of loading object"
;;; -o -> "Load object, without recompiling"
;;; -x -> "Stop managing loaded file (i.e., stop loading)"
;;; -z -> "Postpone update of chunks for files supported by this one"

(defmacro fload (&rest specs)
  `(do-fload ',specs))

;;; files, flags, readtable
(defvar default-fload-args* (vector !() !() nil))

(defun do-fload (specs)
   (labels ((do-it ()
	       (with-compilation-unit ()
		  (file-op-defaults-update 'fload specs fload-flags*
					 (lambda () default-fload-args*)
					 (lambda (a)
					    (setq default-fload-args* a)))
		  (apply #'filespecs-do-load
			 (coerce default-fload-args* 'list)))))
      (cond (file-op-in-progress*
	     (do-it))
	    (t
	     (setq file-op-count* (+ file-op-count* 1))
	     (let ((file-op-in-progress* true))
		(catch 'fload-abort
		   (do-it)))))))

(defun filespecs-load (specs &optional (flags !()) (rt *readtable*))
   (filespecs-do-load
       specs
       (flags-check flags filespecs-load-flags*)
       rt))

(defun filespecs-do-load (specs flags *readtable*)
   (let ((*load-verbose* false))
      (let ((force-flag false)
	    (file-manip false)
	    (postpone-derivees false))
	 (labels ((set-manip (v force-too flag)
		     (cond (file-manip
			    (format *error-output*
			       "Ignoring redundant flag ~s to fload"
			       flag))
			   (t
			    (setq file-manip v)
			    (cond (force-too
				   (setq force-flag v)))))))
	    (dolist (flag flags)
	       (cond ((eq flag '-z)
		      (setq postpone-derivees true))
		     (file-manip
		      (format *error-output*
			 "Ignoring redundant flag to fload: ~s~%"
			 flag))
		     (t
		      (case flag
			 (-x
			  (set-manip ':noload false '-x))
			 (-s
			  (set-manip ':source true '-s))
			 (-o
			  (set-manip ':object true '-o))
			 (-f
			  (setq force-flag ':load))
			 (-c
			  (set-manip ':compile true '-c))
			 (-a
			  (set-manip ':ask-ask false '-a))
			 (t
			  (cerror "I will ignore it"
				  "Illegal flag to 'fload': ~s" flag)))))))
	(dolist (pn (filespecs->ytools-pathnames specs))
  	   (filoid-fload pn :force-load force-flag
			    :manip file-manip
			    :postpone-derivees postpone-derivees)
	  ))))

;;; Possible values: 
;;; :compile -- always compile when object file is missing or out of date)
;;; :source -- always load source without compiling (if missing, load object)
;;; :object -- compile if object file is missing, else load existing object
;;; :ask-every -- ask user what to do every time the file is looked at
;;;         (this gets tedious!)
;;; :ask-once -- ask user once and file the answer
;;; :ask-ask -- ask, then ask the user whether to keep asking
(defvar fload-compile-flag* ':ask)

;;; A class with just one instance, representing the fload-compile-flag*'s
;;; being set to the user's desired value --
(defclass Fload-compile-flag-set (Chunk)
   ((value :accessor Fload-compile-flag-value
	   :initform fload-compile-flag*)))

(defmethod derive ((fcf Fload-compile-flag-set))
   (cond ((eq (Fload-compile-flag-value fcf) fload-compile-flag*)
	  false)
	 (t
	  (setf (Fload-compile-flag-value fcf) fload-compile-flag*)
	  true)))

(defun default-fload-manip () fload-compile-flag*)

(defun (setf default-fload-manip) (v)
   (cond ((memq v '(:ask :source :object :compile
		    :ask-ask :ask-once :ask-every))
	  (cond ((not (eq v fload-compile-flag*))
		 (setq fload-compile-flag* v)
		 (loadeds-check-bases))))
	 (t
	  (cerror "I will leave the value unchanged"
		  !"Illegal value ~s for fload-compile*"
		  v)))
  v)

(define-symbol-macro fload-compile* (default-fload-manip))

;; Must use this to "bind" fload-compile* --
(defmacro bind-fload-compile* (newval &body b)
   `(let ((fload-compile-flag* fload-compile-flag*))
       (setf fload-compile* ,newval)
       ,@b))

(defvar fload-compile-flag-chunk* (make-instance 'Fload-compile-flag-set))

;;; 'filoid-fload' is the entry point to declare that a filoid
;;; should be loaded, figure out its basis, and tell the chunk
;;; network to load it.  
;;; It should never be called _by_ a filoid (Loaded-) chunk deriver, 
;;; because it alters the bases of such chunks. 
(defgeneric filoid-fload (pn &key force-load manip postpone-derivees))

(defmethod filoid-fload ((ytpn YTools-pathname)
			 &key force-load manip postpone-derivees)
   (filoid-fload (pathname-resolve ytpn false)
		 :force-load force-load
		 :manip manip
		 :postpone-derivees postpone-derivees))

(defmethod filoid-fload ((pn pathname)
			 &key force-load manip postpone-derivees)
   (let* ((pchunk (pathname-denotation-chunk pn))
	  (lpchunk (place-Loaded-chunk pchunk manip)))
      (cond ((not (eq manip ':noload))
	     (loaded-chunk-fload lpchunk force-load postpone-derivees)))))

(defvar postponed-file-chunks* !())

(defun loaded-chunk-fload (loaded-chunk force-load postpone-derivees)
      (monitor-filoid-basis loaded-chunk)
      (loaded-chunk-set-basis loaded-chunk)
      (chunk-request-mgt loaded-chunk)
      (let ((d (Chunk-date loaded-chunk)))
;;;;	 (format t "Updating ~s~%"
;;;;		 loaded-chunk)
	 (file-ops-maybe-postpone
	    (chunk-update loaded-chunk false postpone-derivees))
	 (cond ((and force-load
		     (= (Chunk-date loaded-chunk)
			d))
		;; It was apparently already up to date,
		;; so forcing makes sense
		(loaded-chunk-force
		   loaded-chunk force-load postpone-derivees)))))

(defgeneric loaded-chunk-force (loaded-chunk force postpone-derivees))

(defmethod loaded-chunk-force ((loaded-chunk Loaded-file-chunk)
			       force postpone-derivees)
   (let ((obj-file-chunk (Loaded-file-chunk-object loaded-chunk))
	 (src-file-chunk (Loaded-file-chunk-source loaded-chunk)))
      ;; If force = :load, load object if it exists --
      (cond ((eq force ':load)
	     (setq force
		   (cond ((probe-file (File-chunk-pathname obj-file-chunk))
			  ':object)
			 (t
			  ':source)))))
      (let ((operative-chunk
	       (ecase force
		  ((:source :object)
		   (place-Loaded-source-chunk
		      (cond ((eq force ':object)
			     obj-file-chunk)
			    (t
			     src-file-chunk))))
		  ((:compile)
		   (Loaded-file-chunk-compiled loaded-chunk)))))
	(file-ops-maybe-postpone
	   (chunk-update operative-chunk true postpone-derivees)))))

;;;;   (pathname-is-source (File-chunk-pathname file-ch))

(defvar final-load* false)
; When building systems, bind to true to minimize soul-searching later.

(defmethod derive-date ((lc Loaded-file-chunk))
   false)

(defmethod derive ((lc Loaded-file-chunk))
   (let ((loaded-ch lc) file-ch)
      (loop 
	 (setq file-ch
	       (Loaded-file-chunk-selection loaded-ch))
	 (cond ((not file-ch)
		(loaded-chunk-set-basis loaded-ch)
		(setq file-ch
		      (Loaded-file-chunk-selection loaded-ch))))
	 (cond ((typep file-ch 'Loaded-file-chunk)
		(setq loaded-ch file-ch)
		;; Indirection; go around again looking
		;; for an actual file.
	        )
	       (t
		(return))))
      (cond ((not (typep file-ch 'File-chunk))
	     (error "Can't extract file to load from ~s"
		    lc)))
;;;;      (dbg-save lc file-ch)
;;;;      (breakpoint file-chunk-load
;;;;	 "About to load " lc)
      (file-chunk-load file-ch)))

(defmethod derive ((l-source Loaded-source-chunk))
   (file-chunk-load (Loaded-source-chunk-file l-source)))

(defun file-chunk-load (file-ch)
      (with-post-file-transduction-hooks
	 (cleanup-after-file-transduction
	    (let ((*package* *package*)
		  (*readtable* *readtable*)
		  (fload-indent* ;;;;(+ 3 fload-indent*)
		      (* 3 (Chunk-depth file-ch))))
	       (file-op-message "Loading"
				 (File-chunk-pn
				     file-ch)
				 (File-chunk-pathname file-ch)
				 "...")
	       (load (File-chunk-pathname file-ch))
	       (file-op-message "...loaded"
				 (File-chunk-pn
				     file-ch)
				 false
				 ""))))
      (get-universal-time))

;;; This must be called whenever the 'manip' field changes.
(defmethod loaded-chunk-set-basis ((loaded-ch Loaded-file-chunk))
   (let* ((file-ch (Loaded-chunk-loadee loaded-ch))
	  (manip (Loaded-file-chunk-manip loaded-ch))
	  (source-exists (Loaded-file-chunk-source loaded-ch))
	  (obj-file-chunk (Loaded-file-chunk-object loaded-ch))
	  (object-exists
	     (and obj-file-chunk
		  (probe-file (File-chunk-pathname obj-file-chunk)))))
      (cond (loaded-manip-dbg*
	     (format t !"Setting loaded chunk basis, ~
                         manip initially = ~s~%" manip)))
      (cond ((not (or source-exists object-exists))
	     (error "No source or object file can be found for ~s"
		    loaded-ch)))
      (cond ((File-chunk-alt-version file-ch)
	     (setf (Chunk-basis loaded-ch)
		   (list (place-Loaded-file-chunk
			    (File-chunk-alt-version file-ch)
			    ':nochoice))))
	    ((not source-exists)
	     (setq manip ':object))
	    (t
	     (cond ((memq manip '(:defer :follow))
		    (let ((prev-manip manip))
		       (setq manip fload-compile*)
		       (cond ((eq manip ':ask)
			      ;; Old style doesn't make enough distinctions
			      (setq manip ':ask-ask)))
		       (cond ((eq prev-manip ':follow)
			      (setf (Loaded-file-chunk-manip loaded-ch)
				    manip))))))
	     (cond ((memq manip '(:ask-once :ask-every :ask-ask))
		    (setq manip
		          (ask-user-for-manip loaded-ch
					      object-exists)))
		   ((and (eq manip ':object)
			 (not object-exists))
;;;;		    (cerror "I will compile the source file"
;;;;			    !"Request to load nonexistent ~
;;;;			      object file for ~s"
;;;;			    (File-chunk-pathname file-ch))
		    (setq manip ':compile)))
	     ;; At this point manip is either :object, :source, or
	     ;; :compile
	     (cond (loaded-manip-dbg*
		    (format t !"Now manip = ~s ~
                                [should be :object, :source, or :compile]~
                                ~%  loaded-ch = ~s~%"
			    manip loaded-ch)))
	     (let ((first-time (null (Loaded-chunk-principal-bases
				         loaded-ch))))
		(setf (Loaded-chunk-principal-bases loaded-ch)
		      (list (ecase manip
			       (:source
				(Loaded-file-chunk-source loaded-ch))
			       (:compile
				(place-compiled-chunk
				   (Loaded-file-chunk-source loaded-ch)))
			       (:object
				(Loaded-file-chunk-object loaded-ch)))))
		(cond (first-time
		       (setf (Chunk-basis loaded-ch)
			     (append (Loaded-chunk-principal-bases loaded-ch)
				     (mapcar
				         (\\ (callee)
					    (place-Loaded-chunk callee false))
					 (File-chunk-callees file-ch)))))
		      (t
		       (loaded-chunk-change-basis
			   loaded-ch 
			   (mapcar (\\ (callee)
				      (place-Loaded-chunk callee false))
				   (File-chunk-callees file-ch))))))))
      (let ((selection 
	       (cond ((eq manip ':compile)
		      (place-File-chunk
			 (File-chunk-pathname
			    (head (Chunk-basis loaded-ch)))))
		     (t
		      (head (Chunk-basis loaded-ch))))))
	 (cond (loaded-manip-dbg*
		(format t "Setting selection of ~s~% to ~s~%"
			loaded-ch selection)))
	 (setf (Loaded-file-chunk-selection loaded-ch)
	       selection))))

;;; 'loaded-ch' is a Loaded-file-chunk
(defun ask-user-for-manip (loaded-ch obj-exists)
   (let ()
      (loop 
	 (format *query-io*
	    !"Do you want to load the object or source version of ~
              ~% of ~s~% (o[bject]/s[ource]/+/-, \\\\ to abort)? "
	      (File-chunk-pn (Loaded-chunk-loadee loaded-ch)))
	 (multiple-value-bind
	          (manip general)
		  (case (keyword-if-sym (read *query-io*))
		     ((:s :source)
		      (values ':source false))
		     ((:o :object)
		      (values (cond (obj-exists
				     ':object)
				    (t ':compile))
			      false))
		     (:+
		      (setq fload-compile* ':compile)
		      (values ':compile true))
		     (:-
		      (setq fload-compile* ':object)
		      (values ':object true))
		     ((:c :compile)
		      (values ':compile false))
		     ((:\\)
		      (throw 'fload-abort 'fload-aborted))
		     (t
		      (values false nil)))
;;;;	    (format *query-io* "manip = ~s general = ~s~%"
;;;;		    manip general)
	    (cond (manip
		   (cond (general 
			  (setf (Loaded-file-chunk-manip loaded-ch)
				manip)
			  (return manip))
			 (t
			  (return (manip-refine-remember manip loaded-ch)))))
		  (t
		   (format *query-io*
			 !"Type 's' or 'source' to load source file;~%~
                           type 'o' or 'object' to load object file;~%~
                           type '+' to recompile this and every subsequent ~
                           file from now on;~%~
                           type '-' to load object files without recompiling ~
                           whenever possible, from now on.~%")))))))

(defun manip-refine-remember (manip loaded-ch)
   (cond ((eq manip ':object)
	  (cond ((y-or-n-p 
		    !"Do you want to check file write times and recompile ~
		      if necessary? ")
		 (setq manip ':compile)))))
   (block dialogue
      (let ((old-manip (Loaded-file-chunk-manip loaded-ch)))
	 (cond ((eq old-manip ':ask-once)
		(setf (Loaded-file-chunk-manip loaded-ch)
		       manip))
	       ((eq old-manip ':ask-ask)
		(loop
		   (format *query-io*
			   !"Record this choice for future encounters ~
                             with this file (y/n/d, \\\\ to abort)? ")
		   (case (keyword-if-sym (read *query-io*))
		      ((:y :yes :t)
		       (setf (Loaded-file-chunk-manip loaded-ch)
			     manip)
		       (ask-if-generalize manip)
		       (return-from dialogue))
		      ((:n :no)
		       (loop
			  (format *query-io*
				  !"Ask again next time whether to record ~
				    (y/n, \\\\ to abort)? ")
			  (let ((q (keyword-if-sym (read *query-io*))))
			     (case q
			        ((:n :no)
				 (setf (Loaded-file-chunk-manip loaded-ch)
				       ':ask-every)
				 (return-from dialogue))
				((:y :yes)
				 (return-from dialogue))
				((:\\)
				 (throw 'fload-abort 'fload-aborted))
				(t
				 (format *query-io*
				    !"Type 'y' to ensure that every question ~
                                      about what to load will be followed by ~
                                      a~%  ~
				      question about whether to record;~%~
				      type 'n' to suppress that second ~
				      annoying question.~%"))))))
		      ((:d :defer)
		       (setf (Loaded-file-chunk-manip loaded-ch)
			     ':defer)
		       (return-from dialogue))
		      ((:\\)
		       (throw 'fload-abort 'fload-aborted))
		      (t
		       (format *query-io*
			  "Type 'y' or 'yes' to record ~s as form of ~s;~%~
                           type 'n' or 'no' to use that value once, ~
                           ask again next;~%~
                           type 'd' or 'defer' to use value of ~
                           'fload-compile* to decide each time.~%"))))))))
  manip)

(defun ask-if-generalize (manip)
   (cond ((and (not (eq fload-compile* manip))
	       (y-or-n-p
		  "Should the same decision apply to all other files? "))
	  (setq fload-compile* manip))))

;;; We can assume that derive-date has already set the date to the old write
;;; time, and that some supporter has a more recent time.
(defmethod derive ((cf-ch Compiled-file-chunk))
   (let* ((pn (File-chunk-pn
		 (Compiled-file-chunk-source-file cf-ch)))
	  (real-pn (pathname-resolve pn true))
	  (real-ov (File-chunk-pathname
		      (Compiled-file-chunk-object-file cf-ch)))
	  (old-obj-write-time
		   (and real-ov
			(probe-file real-ov)
			(pathname-write-time real-ov))))
      (let ((now-compiling* pn)
	    (now-loading* false)
	    (now-slurping* false)
	    (debuggability* debuggability*)
	    (fload-indent*
	       (* 3 (Chunk-depth cf-ch))))
	 (compile-and-record pn real-pn real-ov
			     cf-ch old-obj-write-time))))

;;;;(defmethod derive ((cf-ch Compiled-file-chunk))
;;;;   (let ((pn (File-chunk-pn
;;;;		(Compiled-file-chunk-source-file cf-ch))))
;;;;      (let ((ov (pathname-object-version pn false)))
;;;;	 (let ((real-pn (pathname-resolve pn true))
;;;;	       (real-ov (and (not (eq ov ':none))
;;;;			     (pathname-resolve ov false)))
;;;;	       (prev-time (Compiled-file-chunk-last-compile-time cf-ch)))
;;;;	    (let  ((old-obj-write-time
;;;;		      (and real-ov
;;;;			   (probe-file real-ov)
;;;;			   (pathname-write-time real-ov))))
;;;;	       (cond ((and (> prev-time 0)
;;;;			   (< prev-time old-obj-write-time))
;;;;		      (format *error-output*
;;;;			 !"Warning -- file ~s apparently compiled outside ~
;;;;                           control of ~s~%"
;;;;			 real-ov cf-ch)))
;;;;	       (cond ((or (not old-obj-write-time)
;;;;			  (< old-obj-write-time
;;;;			     (Chunk-latest-supporter-date cf-ch)))
;;;;		      (setq cf-ch* cf-ch
;;;;			    owt* old-obj-write-time)
;;;;		      (break "Compiling ~s"
;;;;			     pn)
;;;;		      (let ((now-compiling*    pn)
;;;;			    (now-loading* false)
;;;;			    (now-slurping* false)
;;;;			    (debuggability* debuggability*)
;;;;			    (fload-indent* (+ 3 fload-indent*)))
;;;;			 (compile-and-record pn real-pn real-ov
;;;;					     cf-ch old-obj-write-time)))
;;;;		     (t
;;;;		      ;; Up to date
;;;;		      false)))))))

(defun compile-and-record (pn real-pn object-file cf-ch old-obj-write-time)
   (prog2
      (file-op-message "Beginning compilation of" pn real-pn "...")
      (with-post-file-transduction-hooks
	 (cleanup-after-file-transduction
	    (let ((*compile-verbose* false))
	       (cond (object-file
		      (compile-file real-pn :output-file object-file))
		     (t
		      (compile-file real-pn)
		      (setq object-file (pathname-object-version
					   real-pn true))))
	       (let* ((new-compile-time
			 (and object-file
			      (probe-file object-file)
			      (pathname-write-time object-file)))
		      (success
			 (and new-compile-time
			      (or (not old-obj-write-time)
				  (> new-compile-time
				     old-obj-write-time)))))
		  (fcompl-log real-pn (and success object-file))
		  (cond (success
			 (setf (Compiled-file-chunk-last-compile-time
				  cf-ch)
			       new-compile-time)
			 new-compile-time)
			(t 
			 (let ((comp-time (get-universal-time)))
			    (format *error-output*
			       "Compilation failed [time ~s]: ~s~%"
			       comp-time real-pn))))))))
      (file-op-message "...compiled to" object-file false "")))

(defparameter fcompl-flags* '(- -f -x -l -z))
(defparameter filespecs-compile-flags* '(-l -z))
;;; -x -> "Stop managing compiled file (i.e., stop compiling)"
;;; -l -> "Load after compile" (from now on, unless -x)
;;; -f -> "Force compile even if apparently up to date"
;;; -z -> "Postpone update of chunks for files supported by this one"

(defmacro fcompl (&rest specs)
  `(do-fcompl ',specs))

;;; files, flags, readtable
(defvar default-fcompl-args* (vector !() !() false))

(defun do-fcompl (specs)
   (labels ((do-it ()
	       (with-compilation-unit ()
		  (file-op-defaults-update 'fcompl specs fcompl-flags*
					   (lambda () default-fcompl-args*)
					   (lambda (a)
					     (setq default-fcompl-args* a)))
		  (apply #'filespecs-do-compile
			 (coerce default-fcompl-args* 'list)))))
      (cond (file-op-in-progress*
	     (do-it))
	    (t
	     (setq file-op-count* (+ file-op-count* 1))
	     (let ((file-op-in-progress* true))
		(catch 'fload-abort
		   (do-it)))))))

(defun filespecs-compile (specs flags rt)
   (filespecs-do-compile
      specs
      (flags-check flags flags filespecs-compile-flags*)
      rt))

(defun filespecs-do-compile (specs flags *readtable*)
   (let ((*load-verbose* false))
      (let ((force-flag false)
	    (load-flag false)
	    (cease-mgt false)
	    (postpone-derivees false))
	(dolist (flag flags)
	    (case flag
	       (-f (setq force-flag true))
	       (-l (setq load-flag true))
	       (-x (setq cease-mgt true))
	       (-z (setq postpone-derivees true))
	       (t (cerror "I will ignore it"
			  "Illegal flag to 'fcompl': ~s" flag))))
	(dolist (pn (filespecs->ytools-pathnames specs))
	   (filoid-fcompl pn
			    :force-compile force-flag
			    :load load-flag
			    :cease-mgt cease-mgt
			    :postpone-derivees postpone-derivees)))))

(defvar fcompl-reload* ':ask)

(defgeneric filoid-fcompl (pn &key force-compile
				     load
				     cease-mgt
				     postpone-derivees))

(defmethod filoid-fcompl ((ytpn YTools-pathname)
			  &key force-compile load cease-mgt postpone-derivees)
   (filoid-fcompl (pathname-resolve ytpn false)
		  :force-compile force-compile
		  :load load
		  :cease-mgt cease-mgt
		  :postpone-derivees postpone-derivees))

(defmethod filoid-fcompl ((pn pathname)
			    &key force-compile
				 load
				 cease-mgt
				 postpone-derivees)
   (let* ((file-chunk
	     (pathname-denotation-chunk pn))
	  (lpchunk (place-Loaded-chunk
		      file-chunk
		      false))
	  (compiled-chunk
	     (place-compiled-chunk file-chunk))
	  (object-file-chunk
	     (Compiled-file-chunk-object-file compiled-chunk))
	  (object-pathname
	     (File-chunk-pathname object-file-chunk))
	  (object-file-date
	     (and (probe-file object-pathname)
		  (file-write-date object-pathname)))
;;;;	  (comp-date
;;;;	     (pathname-write-time
;;;;	        (File-chunk-pathname compiled-chunk)))
	  )
      (labels ((force-compile ()
		  (file-ops-maybe-postpone
		     (chunks-update (list compiled-chunk)
				    true postpone-derivees)))
	       (consider-loading ()
		  (cond ((and (not (chunk-up-to-date lpchunk))
			      (probe-file object-pathname)
			      (or (not object-file-date)
				  (> (file-write-date object-pathname)
				     object-file-date))
			      (or load (load-after-compile)))
			 (file-ops-maybe-postpone
			    (chunks-update (list lpchunk)
					   true postpone-derivees))))))
	 (monitor-filoid-basis lpchunk)
	 (cond (cease-mgt
		(chunk-terminate-mgt compiled-chunk ':ask)
		(cond (force-compile
		       ;; One last fling --
		       (force-compile)
		       (consider-loading))))
	       (t
		(let ((comp-date
			 (Chunk-date compiled-chunk)))
		   (chunk-request-mgt compiled-chunk)
		   (file-ops-maybe-postpone
		      (chunk-update compiled-chunk false postpone-derivees))
		   (cond ((and force-compile
			       (= (Chunk-date compiled-chunk)
				  comp-date))
			  ;; Hasn't been compiled yet
			  (force-compile)
			  (chunk-derive-and-record compiled-chunk)))
		   (consider-loading)))))))

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

(defvar loaded-filoids-to-be-monitored* ':global)

(defun monitor-filoid-basis (loaded-filoid-ch)
   (cond ((not (typep loaded-filoid-ch 'Loaded-chunk))
	  (error "Attempt to monitor basis of non-Loaded-chunk: ~s"
		 loaded-filoid-ch)))
   (cond ((eq loaded-filoids-to-be-monitored* ':global)
	  (let ((controllers (list (Loaded-chunk-controller loaded-filoid-ch)))
		(loaded-filoids-to-be-monitored* !()))
	     (loop
	        (dolist (loadable-ch controllers)
		   (chunk-request-mgt loadable-ch))
	        (chunks-update controllers false false)
	        (cond ((null loaded-filoids-to-be-monitored*)
		       (return))
		      (t
		       (setq controllers 
			     (mapcar #'Loaded-chunk-controller
				     loaded-filoids-to-be-monitored*))
		       (setq loaded-filoids-to-be-monitored* !()))))))
	 (t
	  (on-list loaded-filoid-ch loaded-filoids-to-be-monitored*))))

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

(defvar fload-version-suffix* ':-new)

(defmacro fload-versions (&rest specs)
   (let ((olds (mapcar (lambda (x) 
			  (cond ((consp x) (car x))
				(t x)))
		       specs))
	 (news (mapcar (lambda (x)
			  (cond ((consp x)
                                 (cond ((null (cdr x))
                                        (build-symbol
					   (:< (car x))
					   (:< fload-version-suffix*)))
                                       ((or (is-String (cadr x))
					    (is-Keyword (cadr x)))
                                        (build-symbol
					   (:< (car x)) (:< (cadr x))))
                                       (t (cadr x))))
				(t x)))
		       specs)))
      `(fload-versions-setup ',olds ',news)))

(defun fload-versions-setup (olds news)
   (multiple-value-bind (set-olds set-news reset-olds)
                        (labels ((segregate (olds news)
                                    (cond ((null news)
                                           (values !() !() !()))
                                          (t
                                           (multiple-value-bind
                                                   (so sn rso)
                                                   (segregate (cdr olds)
                                                              (cdr news))
                                              (cond ((eq (car news) '-)
                                                     (values so sn
                                                             (cons (car olds)
                                                                   rso)))
                                                    ((eq (car news)
							 (car olds))
                                                     (values
                                                        (cons (car olds) so)
                                                        (cons (car news) sn)
                                                        (cons (car olds) rso)))
                                                    (t
                                                     (values
                                                        (cons (car olds) so)
                                                        (cons (car news) sn)
                                                        rso))))))))
                            (segregate olds news))
      (let ((changing-chunks !()))
	 (do ((oldl (filespecs->ytools-pathnames set-olds) (cdr oldl))
	      (newl (filespecs->ytools-pathnames set-news) (cdr newl)))
	     ((null oldl))
	    (let ((fc (place-File-chunk (car oldl))))
	       (setf (File-chunk-alt-version fc)
		     (place-File-chunk (car newl)))
	       (on-list fc changing-chunks)))
	 (do ((oldl (filespecs->ytools-pathnames reset-olds) (cdr oldl)))
	     ((null oldl))
	    (let ((fc (place-File-chunk (car oldl))))
	       (setf (File-chunk-alt-version fc)
		     false)
	       (on-list fc changing-chunks)))
	 (chunks-update changing-chunks false false)
	 (nconc reset-olds (mapcar #'list set-olds set-news)))))

;;; The "maybe" is because 'chunks' can be the empty list if the
;;; appropriate "postpone" argument was false in the call that 
;;; produced it.--
(defun file-ops-maybe-postpone (chunks)
   (setq postponed-file-chunks*
	 (append chunks postponed-file-chunks*)))

(defun postponed-files-update ()
   (chunks-update postponed-file-chunks* false false))

(defun keyword-if-sym (x)
   (cond ((is-Symbol x)
	  (intern (symbol-name x) keyword-package*))
	 (t x)))

