;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: depend-new.lisp,v 1.1.2.4 2005/01/28 13:24:26 airfoyle Exp $

;;; Copyright (C) 1976-2005 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:compile-toplevel :load-toplevel)
   (export '(depends-on module funktion)))

(eval-when (:compile-toplevel :load-toplevel)

(defstruct (Scan-depends-on-state (:conc-name Sds-))
   file-chunk  ; or the symbol *, meaning the whose basis is being found
   sub-file-types)
;; -- A list of sub-file types L such that all supportive children found
;; after this will be slurped with respect to every element of L.

;;;;   slurp-trawlers
;;;; -- trawlers are objects (functions?) that interact with file
;;;; dependencies to produce slurp tasks for files this one depends on

;;; State for task is a file chunk, whose basis (and callees)
;;; we are computing.
(def-slurp-task scan-depends-on
   :default (\\ (_ _) true)
;;; -- The idea is that anything we didn't anticipate takes us
;;; out of the header.
   :file->state-fcn     ;;;; #'place-file-chunk
		    (\\ (pn)
		       (make-Scan-depends-on-state
			  :file-chunk (place-file-chunk pn)
			  :sub-file-types (list macro-sub-file-type*)
;;;;			  :slurp-trawlers !())
		        )))
 )
   
(defclass Loadable-chunk-from-file-scan (Loadable-chunk) ())

(defmethod derive ((fb Loadable-chunk-from-file-scan))
   (cond ((= (Chunk-date fb) file-op-count*)
	  false)
	 (t
	  (let* ((loaded-ch (Loadable-chunk-controllee fb))
		 (file-ch (Loaded-chunk-file loaded-ch))
		 (comp-ch (place-compiled-chunk file-ch)))
	     (setf (File-chunk-callees file-ch) !())
	     (setf (File-chunk-self-compile-dep file-ch)
		   false)
	     (file-slurp (File-chunk-pathname file-ch)
			 (list scan-depends-on*)
			 (\\ (srm)
			    (let ((readtab
				     (modeline-extract-readtab srm)))
			       (cond (readtab
				      (setf (File-chunk-readtable
					       file-ch)
					    readtab))))))
	     file-op-count*))))

;;; 'srm' is stream of freshly opened file.  Try to get readtable name
;;; from first line, returning false if it can't be found.
(defun modeline-extract-readtab (srm)
   (let ((c (peek-char false srm false eof*)))
      (cond ((and (not (eq c eof*))
		  (char= #\;))
	     ;; Got comment.  Try to parse as
	     ;; mode line with readtable spec
	     (string-extract-readtab (read-line srm)))
	    (t false))))

(defun string-extract-readtab (str)
   (let ((rpos (search "Readtable: " str))
	 (strlen (length str)))
      (cond (rpos
	     (let ((pos (+ rpos (length "Readtable: "))))
		(loop (cond ((and (< pos strlen)
				  (is-whitespace (elt str pos)))
			     (setq pos (+ pos 1)))
			    (t (return))))
		(cond ((< pos strlen)
		       (let ((end (position #\; str :start pos)))
			  (cond (end
				 ;; Finally!  A readtable name
				 (let* ((readtab-name
					     (intern (subseq str pos end)
						     keyword-package*))
					(readtab (named-readtable
						     readtab-name)))
				    (or readtab
					(progn
					   (format *error-output*
					      "Undefined readtable ~s~%"
					      readtab-name)
					   false))))
				(t
				 (format *error-output*
				    !"Can't find end of readtable name in ~
                                      mode line~
                                      ~%   \"...~a\"~%"
				    (subseq str rpos))))))
		      (t
		       (format *error-output*
			  "Can't find readtable name in mode line ~
                           ~%   \"...~a\"~%"
			  (subseq str rpos))))))
	    (t false))))

(defmethod create-Loaded-Controller ((file-ch File-chunk)
				     (loaded-ch Loaded-chunk))
   (chunk-with-name
       `(:loadable ,(Chunk-name file-ch))
       (\\ (name)
	  (make-instance 'Loadable-chunk-from-file-scan
	     :name name
	     :controllee loaded-ch))))

(defvar depends-on-enabled* true)

(defun cl-user::depends-on-disable () (setq depends-on-enabled* false))

;;; Syntax (depends-on <filegroup>*)
;;; where <filegroup> is [<times>] --floadable-filespecs--
;;; <times> is (:at <time>*)
;;; where each <time> is either :run-time, :compile-time, :slurp-time
;;; (:at :run-time) may be abbreviated :at-run-time; similarly for 
;;; :at-compile-time and :at-slurp-time
;;; (:at :run-time :compile-time :slurp-time) may be abbreviated
;;; :always.
;;; Let C be the file containing the 'depends-on' form, F be a file
;;; mentioned in the 'depends-on'.
;;; 
;;; Note that :read-time (= :slurp-time) means that F will be loaded as soon
;;; as the 'depends-on' form is seen.  :run-time means that F will be
;;; loaded when C or its compiled form is loaded.
;;; :compile-time means that F will be loaded when C is compiled.

(defmacro depends-on (&rest stuff)
   (let ((dgroups (depends-on-args-group stuff)))
      (let ((read-time-deps
	       (mapcan (\\ (g)
			  (cond ((memq ':read-time (first g))
				 (list-copy (rest g)))
				(t !())))
		       dgroups))
	    (run-time-deps
	       (mapcan (\\ (g)
			  (cond ((memq ':run-time (first g))
				 (list-copy (rest g)))
				(t !())))
		       dgroups)))
	 `(progn
	      ,@(include-if (not (null read-time-deps))
		   `(eval-when (:compile-toplevel :execute)
		       (cond (depends-on-enabled*
			      (fload ,@read-time-deps)))))
	      ,@(include-if (not (null run-time-deps))
		   `(eval-when (:load-toplevel :execute)
		       (cond (depends-on-enabled*
			      (fload ,@run-time-deps)))))))))

(defvar end-header-dbg* false)

(datafun scan-depends-on in-readtable
   (defun :^ (form file-ch)
      (let ((readtab (name->readtable (cadr form)))
	    (prev-readtab (File-chunk-readtable file-ch)))
	 (cond ((not (eq readtab prev-readtab))
		(format *error-output*
		   "Changing readtable of ~s from ~s to ~s = ~s~%"
		   file-ch prev-readtab (cadr form) readtab)))
		(setf (File-chunk-readtable file-ch)
		      readtab)
		;; for the duration of the slurp --
		(setq *readtable* readtab))))

(datafun scan-depends-on end-header
   (defun :^ (form file-ch) ;; -- of file being slurped
      (cond ((memq ':no-compile (cdr form))
	     (place-loaded-chunk file-ch ':source)))
      (cond (end-header-dbg*
	     (format *error-output*
		     "Executing ~s~% "
		     form)))
      (cond ((memq ':continue-slurping (cdr form))
	     (format *error-output*
		!"Warning -- ':continue-slurping' encountered in file ~
                  ~s; this declaration is~
                  ~%  no longer needed~%")))
      true))

(datafun scan-depends-on depends-on
  (defun :^ (e sdo-state)
     (cond (depends-on-enabled*
	    (let* ((file-ch (Sds-file-chunk sdo-state))
		   (groups (depends-on-args-group (cdr e)))
		   (compiled-ch (place-compiled-chunk file-ch))
		   (loaded-file-ch (place-loaded-chunk file-ch false)))
	       (labels ()
		  (dolist (g groups)
		     (let* ((file-chunks (<# place-file-chunk
					     (filespecs->ytools-pathnames
						(cdr g))))
			    (loaded-file-chunks
			       (<# (\\ (fc) (place-loaded-chunk fc false))
				   file-chunks)))
			(cond ((memq ':compile-time (first g))
			       (setf (Chunk-basis compiled-ch)
				     (union
					file-chunks
					(Chunk-basis compiled-ch)))
			       (setf (Chunk-update-basis compiled-ch)
				     (union loaded-file-chunks
					    (Chunk-update-basis
					       compiled-ch)))))
			(cond ((memq ':run-time (first g))
			       (dolist (fch file-chunks)
				  (setf (alref (File-chunk-callees file-ch)
					       fch)
					(union (alref (File-chunk-callees
						         file-ch)
						      fch
						      !())
					       (Sds-sub-file-types
						   sdo-state))))))
			(cond ((or (memq ':read-time (first g))
				   (memq ':slurp-time (first g)))
			       (setf (File-chunk-read-basis file-ch)
				     (union file-chunks
					    (File-chunk-read-basis
						file-ch)))))))))))
    false))

;;; Return a list of groups, each of the form
;;; ((<time>*)
;;;  --filespecs--)
;;; where each <time> is either :run-time, :compile-time, :read-time
(defun depends-on-args-group (args)
   (cond ((and (= (len args) 1)
	       (is-Symbol (car args)))
	  ;;; Compatibility with old Nisp
	  (list `((:run-time :compile-time) %module/ ,(car args))))
	 (t
	  (let ((this-mode '(:run-time :compile-time :read-time :slurp-time))
		(this-group '())
		(groups '()))
	     (labels ((end-group ()
			 (cond ((not (null this-group))
				(setq groups
				      (cons (cons this-mode
						  (reverse this-group))
					    groups))))))
		(dolist (arg args)
		   (let ((next-mode
			    (cond ((memq arg '(:at-run-time at-run-time
					       :at-compile-time at-compile-time
					       :at-read-time :at-slurp-time
					       :always :at-run-and-compile-time))
				   (case arg
				      ((:at-run-time at-run-time)
				       '(:run-time))
				      ((:at-compile-time at-compile-time)
				       '(:compile-time))
				      ((:at-read-time :at-slurp-time)
				       '(:read-time))
				      (t
				       '(:compile-time :run-time :read-time))))
				  ((car-eq arg ':at)
				   (cond ((null (cdr arg))
					  (cerror !"The next block ~
                                                    will be ignored"
						  "depends-on with (:at): ~s"
						  `(depends-on ,@args))))
				   (cdr arg))
				  (t ':no-next-group))))
		      (cond ((eq next-mode ':no-next-group)
			     (setq this-group (cons arg this-group)))
			    (t
			     (end-group)
			     (setq this-mode next-mode)
			     (setq this-group '())))))
	     (end-group)
	     (reverse groups))))))

(defun module-slurp (mpn _ howmuch)
   (let ((modname (Module-pseudo-pn-module mpn)))
      (let ((module (find-YT-module modname)))
	 (cond (module
		(let ((slurping-lprec*
		         (YT-module-rec module)))
		   (cond ((memq modname module-trace*)
			  (format *error-output*
			      "Considering slurping module ~s... ~%" modname)))
		   (cond ((achieved-load-status
			     slurping-lprec* ':slurped-all)
			  (cond ((memq modname module-trace*)
				 (format *error-output*
				    "Module ~s already slurped~%" modname))))
			 ((eq howmuch ':header-only)
			  (cond ((memq modname module-trace*)
				 (format *error-output*
				    "Not looking inside module ~s for supporters"
				    modname))))
			 (t
			  (setf (Load-progress-rec-run-time-depends-on
				   slurping-lprec*)
				!())
			  (setf (Load-progress-rec-compile-time-depends-on
				   slurping-lprec*)
				!())
			  (cond ((memq modname module-trace*)
				 (format *error-output*
				    "Slurping module ~s, ~% form = ~s"
				    modname (YT-module-contents module))))
			  (form-slurp (YT-module-contents module)
				      (eq howmuch ':whole-file))
;;;;			  (format t "Doing dependents of ~s~%"
;;;;				  slurping-lprec*)
			  ;; The following two forms were lifted from
			  ;; 'lprec-compile'
			  (dolist (rtsupp
				      (Load-progress-rec-run-time-depends-on
					 slurping-lprec*))
			     (pathname-slurp rtsupp false ':at-least-header))
			  (dolist (ctsupp
				    (Load-progress-rec-compile-time-depends-on
					 slurping-lprec*))
			     (pathname-fload ctsupp false false false))
			  (note-load-status slurping-lprec* ':slurped-all)))))
	       (t
		(error "Attempt to slurp non-module ~s"
		       modname))))))

(defun module-load (mod-pspn force-flag force-compile always-ask)
                   (ignore always-ask)
   (let ((modname (Module-pseudo-pn-module mod-pspn)))
      (let ((fload-compile* (or force-compile fload-compile*)))
	 (ytools-module-load modname force-flag))))

(defun module-compile (mod-pspn force-flag)
   (let ((modname (Module-pseudo-pn-module mod-pspn)))
      (let ((fload-compile* ':compile))
	 (ytools-module-load modname force-flag))))

(defun module-expansion (mod-pspn)
   (let ((modname (Module-pseudo-pn-module mod-pspn)))
      (let ((mod (find-YT-module modname)))
	 (cond (mod
		(YT-module-expansion mod))
	       (t
		(error "Undefined YTools module ~s" modname))))))


#|
;;; Example: 
(def-ytools-module nisp
   (:run-support
      (depends-on %ytools/ nilscompat)
      (depends-on %module/ ydecl-kernel))
   (:compile-support (depends-on %ydecl/ compnisp))
   (:expansion
       (self-compile-dep :macros)
       (callees-get-nisp-type-slurp))
   )

(datafun scan-depends-on callees-get-nisp-type-slurp
   (defun :^ (d sdo-state)
      (setf (Sds-sub-file-types sdo-state)
	    (adjoin nisp-type-slurp*
		    (Sds-sub-file-types sdo-state)))))

|#

(defmacro self-compile-dep (&rest sorts)
   `("self-dependency for compilation " ',sorts))

(datafun scan-depends-on self-compile-dep
   (defun :^ (e sdo-state)
      (let ((file-ch (Sds-file-chunk sdo-state)))
	 (setf (File-chunk-self-compile-dep file-ch)
	       (cond ((eq (cadr e) ':load-source)
		      ':load-source)
		     (t (mapcar #'lookup-sub-file-type
				(cdr e))))))))

(defvar patch-files* '())

(defun load-patch-file-spec (pf)
       (let ((files (filespecs->pathnames (cdr pf))))
	  (dolist (file files)
	     (cond ((probe-file file)
		    (pathname-fload file false false false))))))

(defmacro load-patch-file (filename^ label^)
   `(probe-and-load-patch-file ,filename^ ,label^))

(defun probe-and-load-patch-file (name label)
   (let ((pname (->pathname
		    (concatenate 'string
		        name "-"
			(substitute #\- #\. label)))))
      (let ((rname (or (get-pathname-with-suffixes pname object-suffixes*)
		       (get-pathname-with-suffixes pname source-suffixes*))))
	 (cond (rname
		(lprec-load (place-load-progress-rec rname)
			    false false false))))))

(declaim (special final-load*))

(defun note-patch-files (module-name files)
   (let ((p (assq module-name patch-files*)))
      (cond ((not p)
	     (setf p (list module-name))
	     (setf patch-files* (cons p patch-files*)))   )
      (setf (cdr p) files)
      (cond ((not final-load*)
             (load-patch-file-spec p)))))

(def-ytools-module ytools
	 (eval-when (:execute :compile-toplevel :load-toplevel)
	    (setq *readtable* ytools-readtable*))
;;;;         #+allegro
;;;;	 (depends-on :at-run-time %ytools/ prompthack)
	 (depends-on %ytools/ multilet)
	 (depends-on :at-run-time %ytools/ signal misc)
	 (depends-on (:at :slurp-time :compile-time) %ytools/ setter)
	 (depends-on :at-compile-time 
		     %ytools/ object mapper)
	 (note-patch-files 'all-ytools '(%ytools/ "ytools-patches.nsp")))

(def-excl-dispatch #\' (srm _)
   (list 'funktion (read srm true nil true)))

;;;;  (let ((f (read srm)))
;;;;     (cond ((atom f) (list 'funktion f))
;;;;	   (t f))))

(defmacro funktion (f)
   (cond ((and (atom f) (> debuggability* 0))
	  `',f)
	 (t `#',f)))