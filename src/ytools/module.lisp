;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: module.lisp,v 1.9.2.24 2005/03/24 12:58:58 airfoyle Exp $

;;; Copyright (C) 1976-2004
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(def-ytools-module import-export module-trace*)))

(defstruct (YT-module
	      (:constructor make-YT-module
			    (name contents
			     &aux (chunk false) (loaded-chunk false)))
	      (:predicate is-YT-module)
	      (:print-object 
	          (lambda (mod srm)
		     (format srm "#<YT-module ~s>" (YT-module-name mod)))))
   name
   ;; A list of lists, each of the form
   ;;    ((-time-specs-) -acts-)
   ;; meaning, insert or execute the -acts- at times matching one of
   ;; the time-specs.  A time-spec is :run-support, :compile-support,
   ;; or :expansion. -- 
   contents     
   chunk
   loaded-chunk)

;;; Note that YT-module-chunk is the name of the accessor of the 'chunk'
;;; slot of 'YT-module', and also the name of a class.  Have fun keeping
;;; track of them.

;;; An alist of <name, yt-module> pairs.
(defvar ytools-modules* !())

(defstruct (Module-pseudo-pn (:include Pseudo-pathname))
   module)

(defmethod make-load-form ((mpspn Module-pseudo-pn) &optional env)
   (declare (ignore env))
   (let ((modname (YT-module-name
			   (Module-pseudo-pn-module mpspn))))
      `(make-Module-pseudo-pn
	  :name ',(Module-pseudo-pn-name mpspn)
	  :module (or (lookup-YT-module
			 ',modname)
		      (error "Can't find YT-module named ~s"
			     ',modname)))))

(defmethod pathname-expansion ((mpspn Module-pseudo-pn))
   (mapcan (\\ (c)
	      (cond ((memq ':expansion (first c))
		     (list-copy (rest c)))
		    (t !())))
	   (YT-module-contents
	      (Module-pseudo-pn-module mpspn))))

;;; (def-ytools-module name 
;;;     -act-specs-)
;;; Where act-spec is (timespec -acts-)
;;; timespec is a list of symbols drawn from 
;;; :run-support, :compile-support, :expansion,

;;; Let F be a file that depends-on this module.
;;; :expansion: The acts are (as it were) inserted in the
;;;    top level of F.
;;; :run-support: The acts are executed when F is loaded
;;; :compile-support: The acts are executed when F is compiled
;;; If act-spec is not of the form (timespec -acts-), it is treated 
;;; as ((:run-support :compile-support) act-spec).
;;; If timespec is one the three symbols above, it is treated as a 
;;; singleton list of that symbol.
;;; Note that this is orthogonal to the :at-run-time/:at-compile-time
;;; distinction.  If F depends-on a module :at-compile-time, that
;;; means that the :run-support actions of F are executed when F is
;;; compiled.
(defmacro def-ytools-module (name &rest actions &whole dym-exp)
   (labels ((is-timespec (x)
	        (memq x '(:run-support :compile-support :expansion))))
      (setq actions
	    (mapcan (\\ (a)
		       (cond ((atom a)
			      (format *error-output*
				 !"Ignoring meaningless actspec ~s in ~
				   'def-ytools-module': ~s"
				 a dym-exp)
			      (list))
			     ((listp (car a))
			      (cond ((every #'is-timespec (car a))
				     (list a))
				    (t
				     (list `((:run-support :compile-support)
					     ,@a)))))
			     ((is-timespec (car a))
			      (list `((,(car a)) ,@(cdr a))))
			     (t
			      (list `((:run-support :compile-support)
				      ,a)))))
		    actions))
      `(let ((mod (place-YT-module ',name)))
	  (setf (YT-module-contents mod)
		',actions))))

(defun place-YT-module (name)
   (or (lookup-YT-module name)
       (let ((mod (make-YT-module name false)))
	  (on-list (tuple name mod)
		   ytools-modules*)
	  mod)))

(defun lookup-YT-module (name)
   (let ((e (assq name ytools-modules*)))
      (and e (second e))))

(defclass YT-module-chunk (Code-chunk)
   ((module :reader YT-module-chunk-module
	    :initarg :module
	    :type YT-module)))

(defmethod derive ((yt-mod YT-module-chunk))
   false)

;;; loadee is a YT-module
(defclass Loaded-module-chunk (Loaded-chunk)
   ())

(defclass Compiled-module-chunk (Chunk)
   ((module :reader Compiled-module-chunk-module
	    :initarg :module)))

;;; List of names of modules we're interested in --
(defvar module-trace* !())

(def-ytools-pathname-control module
   (defun :^ (operands _)
      (do ((opl operands (cdr opl))
	   mod
	   (mods !()))
	  ((or (null opl)
	       (not (setq mod (lookup-YT-module (car opl)))))
	   (let ((remainder opl))
	      (cond ((not (null module-trace*))
		     (format *error-output*
			 "Preparing to process modules ~s~%"
			 mods)))
	      (values (mapcar (\\ (mod)
				 (make-Module-pseudo-pn
				    :name (YT-module-name mod)
				    :module mod))
			      mods)
		      false
		      remainder)))
         (on-list mod mods))))

(defmethod pathname-denotation-chunk ((mod-pspn Module-pseudo-pn))
   (place-YT-module-chunk (Module-pseudo-pn-module mod-pspn)))

(defun place-YT-module-chunk (mod)
   (or (YT-module-chunk mod)
       (let ((mod-ch 
		(chunk-with-name `(:YT-module ,(YT-module-name mod))
		   (\\ (name)
		      (make-instance 'YT-module-chunk
			 :name name
			 :module mod)))))
	  (setf (YT-module-chunk mod)
		mod-ch)
	  mod-ch)))

;;; The key hack in the following two items is that a YT-module's
;;; dependencies are a subset of the files it will load, possibly
;;; an empty subset.
;;; But if there's some other reason to reload or recompile,
;;; all the relevant forms from its contents must be updated,
;;; and in general they will load several files-- 

(defmethod pathname-compile-support ((pspn Module-pseudo-pn))
   (let ((mod (Module-pseudo-pn-module pspn)))
      (values
           (module-loaded-prereqs mod)
	   (module-form-chunks mod ':compile-support))))

(defmethod pathname-run-support ((pspn Module-pseudo-pn))
   (let ((mod (Module-pseudo-pn-module pspn)))
      (values
           (module-loaded-prereqs mod)
	   (list (place-Loaded-module-chunk
		    (place-YT-module-chunk mod)
		    false)))))

;;; A Form-chunk that is clocked using file-op-count*.
(defclass Form-timed-file-op-chunk (Form-chunk)
   ())

(defmethod derive-date ((fop-ch Form-timed-file-op-chunk))
   (cond ((> (Chunk-date fop-ch) 0)
	  (Chunk-date fop-ch))
	 (t +no-info-date+)))

(defmethod derive :around ((fop-ch Form-timed-file-op-chunk))
   (cond ((not file-op-in-progress*)
	  (setq file-op-count* (+ file-op-count* 1))))
   (let ((file-op-in-progress* true)
	 (initial-count file-op-count*)
	 (initial-chunk-event-count chunk-event-num*))
      ;; Binding file-op-in-progress* keeps file-op-count* from
      ;; being incremented.
      (call-next-method fop-ch)
      (cond ((and chunk-update-dbg*
		  (not (= initial-chunk-event-count
			  chunk-event-num*)))
	     (format *error-output*
		"Chunk event count changed during form evaluation, from ~s to ~s~%"
		initial-chunk-event-count chunk-event-num*)))
      (cond ((not (= file-op-count* initial-count))
	     (error "file-op-count* changed unexpectedly while evaluating ~s~%"
		    (Form-chunk-form fop-ch))))
      file-op-count*))

(defun place-Form-timed-file-op-chunk (form)
   (chunk-with-name `(:form ,form :timed-by-file-ops)
      (\\ (name)
	 (make-instance 'Form-timed-file-op-chunk
	    :name name
	    :form form))))

(defun module-form-chunks (mod which)
	   (let ((rforms
		    (retain-if
		       (\\ (e) (memq which (first e)))
		       (YT-module-contents mod))))
	      (cond ((null rforms)
		     !())
		    (t
		     (list (place-Form-timed-file-op-chunk
			      `(progn ,@(mapcan (\\ (e)
						   (list-copy (rest e)))
						rforms))))))))

(defun module-loaded-prereqs (mod)
   (let ((loaded-ch (place-Loaded-module-chunk
		        (place-YT-module-chunk mod)
			false)))
      (loaded-chunk-verify-basis loaded-ch)))

(defvar loaded-ytools-modules* !())

(defmethod filoid-fload ((yt-mod Module-pseudo-pn)
			 &key force-load manip postpone-derivees)
   (let* ((module (Module-pseudo-pn-module yt-mod))
	  (mod-chunk (place-YT-module-chunk module))
	  (lmod-chunk (place-Loaded-module-chunk
		         mod-chunk manip)))
;;;;      (setq lmod-ch* lmod-chunk)
;;;;      (break "In filoid-fload, got module ~s" module)
      (cond ((not (eq manip ':noload))
	     (loaded-chunk-fload lmod-chunk force-load postpone-derivees)))))

(defmethod place-Loaded-chunk ((mod-ch YT-module-chunk) mod-manip)
   (place-Loaded-module-chunk mod-ch mod-manip))

(defun place-Loaded-module-chunk (mod-chunk mod-manip)
   (let ((module (YT-module-chunk-module mod-chunk)))
      (or (YT-module-loaded-chunk module)
	  (let ((lc (chunk-with-name `(:loaded ,module)
		       (\\ (name)
			  (let ((new-lc
				   (make-instance 'Loaded-module-chunk
				      :name name
				      :loadee mod-chunk)))
			     new-lc))
		       :initializer
			  (\\ (new-lc)
			     (cond ((not (slot-truly-filled
					      new-lc 'controller))
				    (setf (Loaded-chunk-controller new-lc)
					  (create-loaded-controller
					     mod-chunk new-lc))))))))
	     (cond ((eq mod-manip ':noload)
		    (chunk-terminate-mgt lc ':ask))
       ;;;;	    ((and mod-manip
       ;;;;		  (not (eq mod-manip (Loaded-chunk-manip lc))))
       ;;;;	     (setf (Loaded-chunk-manip lc) mod-manip))
	      )
	     (setf (YT-module-loaded-chunk module)
		   lc)
	     lc))))

(defclass Loadable-module-chunk (Loadable-chunk)
   ())

(defmethod create-loaded-controller ((mod-ch YT-module-chunk)
				     (loaded-ch Loaded-module-chunk))
   (chunk-with-name `(:loadable ,(Chunk-name mod-ch))
      (\\ (name)
	 (make-instance 'Loadable-module-chunk
	    :controllee loaded-ch
	    :name name))))

#|
Not needed, because it's essentially identical to scan-depends-on
(def-slurp-task module-scan
   :default (\\ (form state)
	       (let ((dos-handler
		        (href
		           (Slurp-task-handler-table
			      scan-depends-on*)
			   (car form))))
		  (cond (dos-handler
			 (funcall dos-handler form state))
			(t true))))
   :file->state-fcn (\\ (mod)
		       (cond ((is-YT-module mod)
			      (make-Scan-depends-on-state
				 :file-chunk (place-YT-module-chunk mod)
;;;;				 :expect-only-run-time-dependencies true
				 :sub-file-types (list macros-sub-file-type*)))
			     (t
			      (error "Attempt to scan non-module ~s"
				     mod)))))
|#

(defmethod derive ((mod-controller Loadable-module-chunk))
   (let* ((loaded-mod-chunk
		(Loadable-chunk-controllee mod-controller))
	  (mod-chunk
	     (Loaded-chunk-loadee loaded-mod-chunk))
	  (module (YT-module-chunk-module mod-chunk))
	  (clal (YT-module-contents module)))
      (dolist (al clal)
	 (cond ((not (equal (car al)
			    '(:expansion)))
		(let ((acts (rest al)))
		   (forms-slurp
		      acts
		      (list scan-depends-on*)     ;;;;module-scan*
		      (list (make-Scan-depends-on-state
			       :file-chunk mod-chunk
			       :sub-file-types !()))))))))
   file-op-count*)

(defvar module-now-loading* false)

(defmethod derive ((lmod-ch Loaded-module-chunk))
   (let ((module
	    (YT-module-chunk-module
	       (Loaded-chunk-loadee lmod-ch))))
      (dolist (c (YT-module-contents module))
	 (cond ((memq ':run-support (first c))
;;;;		(setq mod* module)
;;;;		(break "In Loaded-module-chunk/derive, evaluating ~s"
;;;;		       c)
		(dolist (e (rest c))
		   (eval e)))))))

(defmethod loaded-chunk-set-basis ((mod-loaded-ch Loaded-module-chunk))
   !())

(defmethod loaded-chunk-force ((loaded-chunk Loaded-module-chunk)
			       _ _)
   (cerror "Proceed without forced compilation"
	   "Not implemented: Forcing compilation of module ~s"
	   (Loaded-chunk-loadee loaded-chunk)))

(defmethod filoid-fcompl ((mod-pspn Module-pseudo-pn)
			  &key force-compile
			       load
			       cease-mgt
			       postpone-derivees)
   (let* ((mod-ch (pathname-denotation-chunk mod-pspn))
	  (loaded-mod-ch (place-Loaded-chunk mod-ch false))
	  (compiled-mod-ch (place-compiled-chunk mod-ch)))
      (monitor-filoid-basis loaded-mod-ch)
      (cond (force-compile
	     (format *error-output*
		!"Warning: Ignoring :force-compile argument to 'filoid-fcompl ~
                  ~% for module chunk ~s~%"
		mod-ch)))
      (cond (cease-mgt
	     (chunk-terminate-mgt compiled-mod-ch  ':ask))
	    (t
	     (let (;;;;(comp-date
		   ;;;;   (Chunk-date compiled-mod-ch))
		   )
		(chunk-request-mgt compiled-mod-ch)
		(file-ops-maybe-postpone
		   (chunk-update compiled-mod-ch false postpone-derivees))
		(cond ((and (not (chunk-up-to-date loaded-mod-ch))
			    (or load (load-after-compile)))
		       (file-ops-maybe-postpone
			     (chunks-update (list loaded-mod-ch)
					    true postpone-derivees)))))))))

(defmethod place-compiled-chunk ((source-mod-ch YT-module-chunk))
   (chunk-with-name `(:compiled ,(YT-module-chunk-module source-mod-ch))
      (\\ (name)
	 (make-instance 'Compiled-module-chunk
	    :module (YT-module-chunk-module source-mod-ch)
	    :name name))))

(eval-when (:load-toplevel :compile-toplevel :execute)

(def-slurp-task module-compile
   :default (\\ (_ _) false))
)

(defmethod derive ((comp-mod Compiled-module-chunk))
   (let ((module
	    (Compiled-module-chunk-module comp-mod)))
      (dolist (c (YT-module-contents module))
	 (cond ((not (equal (first c) '(:expansion)))
		(forms-slurp (rest c)
			     (list module-compile*)
			     (list false)))))))

(defmacro module-elements (&rest specs)
      (multiple-value-bind (files flags readtab)
	                   (flags-separate specs filespecs-load-flags*)
	 `(module-elements-load
	      ',files ',flags
	      (decipher-readtable ,readtab *readtable*))))

(defun module-elements-load (specs flags readtab)
   (labels ((do-it ()
	       (filespecs-do-load specs flags readtab)))
      (cond (file-op-in-progress*
	     (do-it))
	    (t
	     (setq file-op-count* (+ file-op-count* 1))
	     (let ((file-op-in-progress* true))
		(catch 'fload-abort
		   (do-it)))))))

(datafun module-compile fload
   (defun :^ (form _)
      (multiple-value-bind (files flags readtab)
	                   (flags-separate (cdr form) filespecs-load-flags*)
	 (compile-if-flags-say-so files flags readtab))))

(datafun module-compile module-elements fload)

(defun compile-if-flags-say-so (files flags readtab)
      (let ((flags (flags-check flags filespecs-load-flags*)))
	 (cond ((or (memq '-c flags) (memq '-o flags))
		(filespecs-do-compile
		   files
		   (retain-if (\\ (g) (eq g '-z))
			      flags)
		   readtab)))))

(defun import-export (from-pkg-desig strings
		      &optional (exporting-pkg-desig *package*))
   (let ((from-pkg 
	    (cond ((or (is-Symbol from-pkg-desig)
		       (is-String from-pkg-desig))
		   (find-package from-pkg-desig))
		  (t
		   from-pkg-desig)))
	 (exporting-pkg
	     (cond ((or (is-Symbol exporting-pkg-desig)
			(is-String exporting-pkg-desig))
		    (find-package exporting-pkg-desig))
		   (t
		    exporting-pkg-desig))))
   (cond ((and (packagep from-pkg)
	       (packagep exporting-pkg))
	  (dolist (str strings)
	     (cond ((is-Symbol str)
		    (setq str (symbol-name str))))
	     (let ((sym (find-symbol str from-pkg)))
	        (cond ((not sym)
		       (cerror (format nil "I'll put symbol ~a into ~s"
				       str from-pkg)
			       "Symbol with name ~s does not exist in ~s, and so can't ~
                                be exported from ~s"
			 str from-pkg exporting-pkg)
		       (setq sym (intern str from-pkg))))
		(import (list sym) exporting-pkg)
		(export (list sym) exporting-pkg))))
	 ((not (packagep from-pkg))
	  (error "~s does not designate a package"
		 from-pkg-desig))
	 (t
	  (error "~s does not designate a package"
		 exporting-pkg-desig)))))

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


when scanning a sub-file for nisp types, the scan *dies* if you don't see
(depends-on %module/ nisp) before the end of the header

|#

(def-ytools-module ytools
   ((:run-support :compile-support)
    (module-elements %ytools/ multilet signal misc setter object mapper
		     :readtable ytools-readtable*)))

(defun ytools-module-load (name)
   (let ((yt-mod (lookup-YT-module name)))
      (cond (yt-mod
	     (let ((chl (module-form-chunks yt-mod ':run-support)))
	        (dolist (ch chl)
		   (chunk-request-mgt ch))
		(chunks-update chl false false)))
	    (t
	     (error "Can't load YTools module -- undefined")))))
