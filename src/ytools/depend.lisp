;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: depend.lisp,v 1.7.2.23 2005/03/28 03:23:56 airfoyle Exp $

;;; Copyright (C) 1976-2005 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:compile-toplevel :load-toplevel)
   (export '(depends-on module scan-depends-on self-compile-dep
	     end-header)))

(eval-when (:compile-toplevel :load-toplevel)

(defstruct (Scan-depends-on-state (:conc-name Sds-))
   file-chunk  ; for the file whose basis is being found
;;;;   expect-only-run-time-dependencies
   sub-file-types)
;; -- A list of sub-file types L such that all antecedents found
;; after this will be slurped with respect to every element of L.

;;; State for task is an Sdo-state for the file chunk we're scanning.
;;; We're scanning it to figure out its basis (and callees)
(def-slurp-task scan-depends-on
   :default (\\ (_ _) true)
;;; -- The idea is that anything we didn't anticipate takes us
;;; out of the header.
   :file->state-fcn     
		    (\\ (pn)
		       (make-Scan-depends-on-state
			  :file-chunk (place-Code-file-chunk pn)
;;;;			  :expect-only-run-time-dependencies false
			  :sub-file-types (list macros-sub-file-type*)
		        )))
 )
   
(setq hidden-slurp-tasks* (adjoin 'scan-depends-on hidden-slurp-tasks*))

(defclass Loadable-file-chunk-from-scan (Loadable-chunk) ())

(defmethod derive-date ((fb Loadable-file-chunk-from-scan))
;;;;   (format *error-output*
;;;;      "[D]Loadable chunk date = ~s file-op-count* = ~s~%"
;;;;      (Chunk-date fb) file-op-count*)
   (cond ((= (Chunk-date fb) file-op-count*)
	  false)
	 (t +no-info-date+)))

(defmethod derive ((fb Loadable-file-chunk-from-scan))
;;;;   (format *error-output*
;;;;      "Loadable chunk date = ~s file-op-count* = ~s~%  file: ~s~%"
;;;;      (Chunk-date fb)
;;;;      file-op-count*
;;;;      (Loaded-chunk-loadee (Loadable-chunk-controllee fb)))
   (cond ((= (Chunk-date fb) file-op-count*)
	  false)
	 (t
	  (multiple-value-bind (file-ch loaded-ch)
	                       (loaded-file-chunk-current-version
				   (Loadable-chunk-controllee fb))
	                       (declare (ignore loaded-ch))
	     (cond ((typep file-ch 'Compiled-file-chunk)
		    (setq file-ch (Compiled-file-chunk-source-file file-ch))))
	     (setf (Code-chunk-callees file-ch) !())
;;;;	     (setf (File-chunk-self-compile-dep file-ch)
;;;;		   false)
	     (file-slurp (Code-file-chunk-pathname file-ch)
			 (list scan-depends-on*)
			 (\\ (srm)
			    (let ((readtab
				     (modeline-extract-readtab srm)))
			       (cond (readtab
				      (setf (Code-file-chunk-readtable
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

(defmethod create-loaded-controller ((file-ch Code-file-chunk)
				     (loaded-ch Loaded-file-chunk))
   (chunk-with-name
       `(:loadable ,(Chunk-name file-ch))
       (\\ (name)
	  (make-instance 'Loadable-file-chunk-from-scan
	     :name name
	     :controllee loaded-ch))))

(defvar depends-on-enabled* true)
(defvar depends-on-loads-files* false)

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
      (let ((run-time-deps
	       (mapcan (\\ (g)
			  (cond ((memq ':run-time (first g))
				 (filespecs->ytools-pathnames (rest g)))
				(t !())))
		       dgroups)))
	 `(progn
	     ,@(mapcan (\\ (pn) (list-copy (pathname-expansion pn)))
		       run-time-deps)
	     ,@(include-if (not (null run-time-deps))
		  `(cond ((and depends-on-enabled*
			       depends-on-loads-files*)
			  ,@(mapcar (\\ (pn)
				       `(filoid-fload
				           ',pn
					   :manip ':follow))
				    run-time-deps))))))))

(defvar end-header-dbg* false)

(datafun scan-depends-on in-readtable
   (defun :^ (form sdo-state)
      (let* ((readtab (name->readtable (cadr form)))
	     (file-ch (Sds-file-chunk sdo-state))
	     (prev-readtab (Code-file-chunk-readtable file-ch)))
	 (cond ((not (eq readtab prev-readtab))
		(format *error-output*
		   "Changing readtable of ~s from ~s to ~s = ~s~%"
		   file-ch prev-readtab (cadr form) readtab)
		(setf (Code-file-chunk-readtable file-ch)
		      readtab)))
	 ;; for the duration of the slurp --
	 (setq *readtable* readtab)
	 false)))

(datafun scan-depends-on in-package
   (defun :^ (e _)
      (let ((pkg (find-package (cadr e))))
	 (cond (pkg
		(setq *package* pkg))
	       (t
		(format *error-output*
		   "Ignoring declaration ~s -- undefined package"
		   e)))
	 false)))

;;; During debugging, may hold a bogus group gleaned from
;;; a module definition.--
(defvar bad-group*)

(datafun scan-depends-on depends-on
  (defun :^ (e sdo-state)
     (cond (depends-on-enabled*
	    (let* ((file-ch (Sds-file-chunk sdo-state))
		   ;; -- If it's not a real Code-file-chunk, it's something
		   ;; like a module chunk.
		   (groups (depends-on-args-group (cdr e)))
		   (compiled-ch (place-compiled-chunk file-ch))
		   (loaded-file-ch (place-Loaded-chunk file-ch false))
		   )
	       (labels ()
		  (dolist (g groups)
		     (let* ((pnl (filespecs->ytools-pathnames (cdr g))))
			(cond ((memq ':compile-time (first g))
			       (cond (compiled-ch
				      (pathnames-note-compile-support
					 pnl compiled-ch))
				     (t
				      (setq bad-group* g)
				      (cerror "Forge on!"
					 !"Warning: ~
					   ':compile-time' dependency in ~
					   unexpected context ~s~%"
					 e)))))
			(cond ((memq ':run-time (first g))
			       (let ((sub-file-types
				        (Sds-sub-file-types sdo-state)))
					 ;; -- Not clear why _these_
					 ;; sub-file-types should be the
					 ;; crucial ones.
				  (pathnames-note-run-support
				     pnl file-ch
				     loaded-file-ch)
				  (pathnames-note-sub-file-deps
				     pnl file-ch compiled-ch sub-file-types))))
			(multiple-value-bind (slurp-dep which-slurp-types)
			                     (extract-slurp-types (first g))
			   (cond (slurp-dep
				  (pathnames-note-slurp-support
					  pnl file-ch which-slurp-types)
				  (setf (Code-file-chunk-read-basis file-ch)
					(union
					  (mapcar
					     #'pathname-denotation-chunk
					     pnl)
					  (Code-file-chunk-read-basis
					     file-ch))))))
			(dolist (pn pnl)
			   (let* ((pchunk (pathname-denotation-chunk pn))
				  (lpchunk (place-Loaded-chunk pchunk false)))
			      (monitor-filoid-basis lpchunk)
			      (loaded-chunk-set-basis lpchunk)
			      (let ((expansion (pathname-expansion pn)))
				 (forms-slurp expansion
					      (list scan-depends-on*)
					      (list sdo-state)))))))))))
    false))

;;; Returns < read-or-slurp-dependency, slurp-types >
;;; If all slurp-types are involved, second val is !().
(defun extract-slurp-types (times)
   (cond ((or (memq ':read-time times)
	      (memq ':slurp-time times))
	  (values true !()))
	 (t
	  (let ((slurp-types !())
		(slurp-time-dependency false))
	     (dolist (time times)
	        (cond ((car-eq time ':slurp-time)
		       (setq slurp-time-dependency true)
		       (setq slurp-types (union (cdr time) slurp-types)))))
	     (values slurp-time-dependency slurp-types)))))

;;; Return a list of groups, each of the form
;;; ((<time>*)
;;;  --filespecs--)
;;; where each <time> is either :run-time, :compile-time, 
;;; :slurp-time, :read-time
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

(defun pathnames-note-compile-support (pnl compiled-ch)
   (dolist (pn pnl)
      (multiple-value-bind (bl ubl)
			   (pathname-compile-support pn)
	 (setf (Chunk-basis compiled-ch)
	       (union bl (Chunk-basis compiled-ch)))
	 (setf (Chunk-update-basis compiled-ch)
	       (union ubl (Chunk-update-basis compiled-ch))))))

(defun pathnames-note-run-support (pnl filoid-ch loaded-filoid-ch)
   (dolist (pn pnl)
      (multiple-value-bind (bl lbl)
			   (pathname-run-support pn)
	 (cond (filoid-ch
		(let ((fbl (retain-if (\\ (b) (typep b 'Code-file-chunk))
				      bl)))
		   (setf (Code-chunk-callees filoid-ch)
			 (union fbl
				(Code-chunk-callees filoid-ch))))))
	 (loaded-chunk-augment-basis loaded-filoid-ch lbl))))

;;; Create link that requires sub-files of all pathnames in 'pnl'
;;; to be loaded or slurped before file handled by 'compiled-ch'
;;; is compiled;
;;; PLUS link that requires any sub-file of a pathname in 'pnl'
;;; to be loaded before the corresponding sub-file of 'file-ch'.
(defun pathnames-note-sub-file-deps (pnl file-ch compiled-ch sub-file-types)
   (cond ((typep file-ch 'Code-file-chunk)
	  (let ((filename (Code-file-chunk-pathname file-ch)))
	     (dolist (pn pnl)
	        (let ((code-chunk
		         (pathname-denotation-chunk pn)))
		   (cond ((typep code-chunk 'Code-file-chunk)
			  (dolist (sfty sub-file-types)
			     (let* ((sf-load-chunker
				       (Sub-file-type-load-chunker sfty))
				    (sf-slurp-chunker
				       (Sub-file-type-slurp-chunker sfty))
				    (slurped-sub-file-chunk
				       (funcall sf-slurp-chunker
					        filename))
				    (loaded-sub-pn-chunk
				       (funcall sf-load-chunker
						(Code-file-chunk-pathname
						   code-chunk))))
			       (pushnew loaded-sub-pn-chunk
					(Chunk-basis
					   slurped-sub-file-chunk)))))))))))
  (cond (compiled-ch
	 (dolist (pn pnl)
	    (multiple-value-bind (bl lbl)
				 (pathname-run-support pn)
				 (declare (ignore lbl))
	       (dolist (callee-ch bl)
		  (cond ((typep callee-ch 'Code-file-chunk)
			 (dolist (sfty sub-file-types)
			    (compiled-ch-sub-file-link
			       compiled-ch callee-ch sfty true))))))))))

(defun pathnames-note-slurp-support (pnl file-ch sub-file-type-names)
   (dolist (pn pnl)
      (let* ((dep-ch (pathname-denotation-chunk pn))
	     (loaded-dep-ch (place-Loaded-chunk dep-ch false))
	     (filename (Code-file-chunk-pathname file-ch)))
	 (dolist (sfty (cond ((null sub-file-type-names)
			      ;; Use 'em all
			      sub-file-types*)
			     (t
			      (mapcar #'lookup-sub-file-type
				      sub-file-type-names))))
	    (let* ((slurped-sub-file-ch
		     (funcall (Sub-file-type-slurp-chunker sfty)
			      filename)))
;;;;	       (out "Introducing dependency: "
;;;;		    :% 2 slurped-sub-file-ch
;;;;		    :% 2 "has basis augmented with"
;;;;		    :% 2 loaded-dep-ch :%)
	       (cond ((not (memq loaded-dep-ch
				 (Chunk-basis slurped-sub-file-ch)))
		      (setf (Chunk-basis slurped-sub-file-ch)
			    (cons loaded-dep-ch
				  (Chunk-basis slurped-sub-file-ch))))))))))

;;; Returns two lists of chunks that should be part of the 
;;; basis and update-basis [respectively] for (:compiled ...).
(defgeneric pathname-compile-support (xpn))

;;; Analogously, returns two lists of chunks that should be part of the 
;;; basis for the filoid denoted by xpn, and of the basis for
;;; (:loaded <filoid>).	   
(defgeneric pathname-run-support (xpn))

(defmethod pathname-compile-support ((ytp YTools-pathname))
   (pathname-compile-support (pathname-resolve ytp false)))

(defmethod pathname-run-support  ((ytp YTools-pathname))
   (pathname-run-support (pathname-resolve ytp false)))

;;; For files, you get the same two lists for compile-support as
;;; for run-support, although with slightly different meanings.--
(defmethod pathname-compile-support ((pn pathname))
   (pathname-run-support pn))

(defmethod pathname-run-support ((pn pathname))
   (let ((file-ch (pathname-denotation-chunk pn)))
      (values (list file-ch)
	      (list (place-Loaded-chunk file-ch false)))))

(defmacro self-compile-dep (&rest sorts)
   `'("self-dependency for compilation " ',sorts))

(datafun scan-depends-on self-compile-dep
   (defun :^ (e sdo-state)
      (let* ((file-ch (Sds-file-chunk sdo-state))
	     (compiled-ch (place-compiled-chunk file-ch)))
;;;;	 (cond ((equal (Pathname-name (Code-file-chunk-pathname file-ch))
;;;;		       "intypes")
;;;;		(dbg-save e sdo-state file-ch compiled-ch)
;;;;		(breakpoint self-compile-dep-scan-depends-on
;;;;		   "Self-compile dependency of sort(s) " e)))
	 (cond ((eq (cadr e) ':load-source)
		(pushnew (place-Loaded-source-chunk file-ch)
			 (Chunk-update-basis compiled-ch)))
	       (t
		(dolist (sub-file-type-name (cdr e))
		   (let ((sftype (lookup-sub-file-type sub-file-type-name)))
		      (pushnew sftype (Sds-sub-file-types sdo-state))
		      (compiled-ch-sub-file-link
		         compiled-ch file-ch sftype false))))))
     false))

(defmacro end-header (&rest _)
   '(values))

;;;;(datafun scan-depends-on filespecs-load 
;;;;;; If scanning 
;;;;
;;;;   (defun :^ (form sdo-state)
;;;;      ()))


(datafun scan-depends-on end-header
   (defun :^ (form sdo-state)
      (let ((flags (cdr form)))
	 (cond ((memq ':no-compile flags)
		(place-Loaded-chunk (Sds-file-chunk sdo-state)
				    ':source)
		(setq flags (remove ':no-compile flags))))
	 (cond (end-header-dbg*
		(format *error-output*
			"Executing ~s~% "
			form)))
	 (cond ((memq ':continue-slurping flags)
		(let* ((file-ch (Sds-file-chunk sdo-state))
		       (compiled-ch (place-compiled-chunk file-ch)))
		   (dolist (sfty (Sds-sub-file-types sdo-state))
		      (compiled-ch-sub-file-link
		         compiled-ch file-ch sfty false)))
;;;;            (self-compile-dep-scan-depends-on form sdo-state)
		))
	 true)))

;;; Actual pathnames don't have expansions, but some pseudo-pathnames
;;; do.
(defgeneric pathname-expansion (xpn)
   (:method ((xpn t)) !()))