;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: files.lisp,v 1.14.2.8 2004/12/13 03:26:36 airfoyle Exp $
	     
;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(fload fcompl fload-compile* fcompl-reload*
	     fload-versions
	     always-slurp end-header
	     debuggable debuggability*)))

(defvar fload-flags* '(- -f -a -c -s -o))

(defmacro fload (&rest specs)
  `(do-fload ',specs))

(defvar default-fload-args* (vector !() !() nil))

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
      (let ((force-flag false)
	    (file-manip false))
	(dolist (flag flags)
	    (cond ((memq flag '(-s -o -f -c))
		   (setq force-flag true)))
	    (cond ((not (eq flag '-f))
		   (cond (file-manip
			  (format *error-output*
				  "Ignoring redundant flag to fload: ~s~%"
				  flag))
			 (t
			  (setq file-manip
			     (case flag
				(- ':default)
				(-a ':ask)
				(-c ':compile)
				(-s ':source)
				(-o ':object)
				(t
				 (cerror "I will ignore it"
					 "Illegal flag to 'fload': ~s" flag)
				 false))))))))
	(dolist (pn (filespecs->ytools-pathnames specs))
  	   (pathname-fload pn :force-load force-flag
			      :file-manip file-manip)
	  ))))

(defgeneric pathname-fload (pn &key force-load file-manip))

(defmethod pathname-fload ((pn pathname)
			   &key force-load file-manip)
   (let ((pchunk (place-file-chunk pn)))
      (let ((lpchunk (place-loaded-chunk pchunk file-manip)))
	 (cond ((Chunk-managed lpchunk)
		(cond (force-load
		       ;; Already managed, so forcing makes sense
		       (setf (Chunk-date lpchunk)
			     (derive lpchunk))
		       (chunks-update (Chunk-derivees lpchunk)))))
	       (t
		(chunk-request-mgt lpchunk))))))

;;; The name of a File-chunk is always its yt-pathname if non-nil,
;;; else its pathname.
;;; This represents a primitive file, not one whose contents are
;;; managed by the file/chunk system itself.
(defclass File-chunk (Chunk)
  ((pathname :reader File-chunk-pathname
	     :initarg :pathname
	     :type pathname)
   (yt-pathname :reader File-chunk-yt-pathname
		:initarg :yt-pathname
		:initform false)
   ;;  -- Either false or a YTools pathname, in which case
   ;; 'pathname' is its resolution
   (readtable :accessor File-chunk-readtable
	      :initarg :readtable
	      :initform false)
   (alt-version nil))
   ;; - If not nil, the File-chunk for the alternative version.
   ;; Files that must be loaded when this one is --
   (callees :accessor File-chunk-callees
	    :initform !())
   ;; Inverse of 'callees' --
   (callers :accessor File-chunk-callers
	    :initform !())
   ;; Files that must be loaded when this one is read --
   (read-basis :accessor File-chunk-read-basis
	       :initform false)
)
;; -- Files are the finest grain we allow.  If a file is out of date,
;; then every chunk associated with the file is assumed to be out of
;; date.  More than one chunk can be associated with a file.  For now,
;; don't allow a chunk to be associated with more than one file.

;; These two methods ensure that 'callers' is the inverse of 'callees' --
(defmethod (setf File-chunk-callees) :before (new-callees file-ch)
   (dolist (clee (File-chunk-callees file-ch))
      (setf (File-chunk-callers clee)
	    (remove file-ch (File-chunk-callers clee)))))

(defmethod (setf File-chunk-callees) :after (new-callees file-ch)
   (dolist (clee (File-chunk-callees file-ch))
      (setf (File-chunk-callers clee)
	    (adjoin file-ch (File-chunk-callers clee)))))

(defmethod derive ((fc File-chunk))
   (let ((v (File-chunk-alt-version fc)))
      (cond (v (derive v))
	    (t (file-write-date (File-chunk-pathname fc))))))

(defun File-chunk-pn (file-ch)
   (or (File-chunk-yt-pathname file-ch)
       (File-chunk-pathname file-ch)))

(defun file-chunk-is-source (file-ch)
   (pathname-is-source (File-chunk-pathname file-ch)))

(defvar final-load* false)
; When building systems, bind to true to minimize soul-searching later.

(defun place-file-chunk (pn)
   (multiple-value-bind (yt-pathname l-pathname)
			(cond ((is-YTools-pathname pn)
			       (values pn (pathname-resolve pn)))
			      (t
			       (values false pn)))
      (chunk-with-name
	 l-pathname
	 (\\ (_)
	    (make-instance 'File-chunk
	       :name `(:file ,(or yt-pathname l-pathname))
	       :yt-pathname yt-pathname
	       :pathname l-pathname
	       :alt-version false)))))

;;; Represents a source file having an up-to-date compiled version.
(defclass Compiled-file-chunk (File-chunk)
  ((source-file :initarg :source-file
		:reader Compiled-file-chunk-source-file
		:type File-chunk)))

;;; Represents the decision having been made which variant of a
;;; file to load, source, object, or alternative
;;; version of file (see fload-versions, below).
(defclass Loadable-chunk (Chunk)
   ((file :reader Loadable-chunk-file
	  :initarg :file
	  :type File-chunk)
    ;; The variant currently selected --
    (selection :accessor Loadable-chunk-selection
	  :initform false)
    ;; The criterion (supplied by user) for how to make the selection --
    (manip :accessor Loadable-chunk-manip
	   :initarg :manip
	   :type (member :compile :source :object
			 :ask-once :ask-every :ask-ask :defer))
    ;; -- Meanings:
    ;;   :source -> don't compile, load :source;
    ;;   :object -> don't compile, load :object;
    ;;   :compile -> compile and then load object;
    ;;   :ask-once -> ask user which to do and record result;
    ;;   :ask-every -> ask user every time basis is recomputed;
    ;;   :ask-ask -> ask once and ask if it should keep asking;
    ;;   :defer -> refer to value of fload-compile*;
    ;;   :follow -> replace 'manip' value with value of fload-compile*

    (source :reader Loadable-chunk-source
	    :initarg :source)
    (compiled :reader Loadble-chunk-compiled
	      :initarg :compiled)
    (object :reader Loadable-chunk-object
	    :initarg :object)))

;;; A file (compiled or source) as having the appropriate
;;; variant loaded into primary memory --
(defclass Loaded-chunk (Chunk)
   ((loadable :reader Loaded-chunk-loadable
	      :initarg :loadable
	      :type (or null Loadable-chunk))
    ;; -- loadable is false (typically) if this is an object file
    ;; that we don't want to consider compiling.
    ;; The File-chunk this one governs --
    (file :accessor Loaded-chunk-basis-chunk
	  :type File-chunk)))

;;; Corresponds to information gleaned from file header about what
;;; other files (and modules, ...) are necessary in order to load or
;;; compile and load, the given file.  This information forms the basis
;;; of the corresponding Loaded-chunk.
(defclass Loaded-basis-chunk (Chunk)
  ((file :reader Loaded-basis-chunk-file
	:initarg :file
	:type File-chunk)))
;;; It would be a reasonable idea to have alternative versions of this
;;; chunk that use 'defsystem' information to figure out the basis.


(defmethod initialize-instance :after ((fb-ch Loaded-basis-chunk)
				       &rest initargs)
  (setf (Chunk-basis fb-ch) !()))

;;; State for task is a file chunk, whose basis (and callees)
;;; we are computing.
(def-slurp-task :compute-file-basis
   :default (\\ (_ _) true)
;;; -- The idea is that anything we didn't anticipate takes us
;;; out of the header.
   :file->state-fcn (\\ (pn) 
		       (place-file-chunk pn)))

(defun place-loaded-basis-chunk (fc)
   (chunk-with-name `(:loaded-basis ,(File-chunk-pathname fc))
      (\\ (name)
	 (make-instance 'Loaded-basis-chunk
	    :file fc
	    :basis !()))))
;;; Though this seems rational --
;;;;(cond ((file-chunk-is-source fc)
;;;;			  (list fc))
;;;;			 (t !()))
;;; -- we simply can't connect a basis-computing chunk to the
;;; chunks whose basis it might affect.  That's because 'chunks-update'
;;; is based on the assumption that the bases it sees are not in the
;;; process of changing.  This comment applies to 
;;; 'loaded-chunk-compute-basis' as well.

(defmethod derive ((fb Loaded-basis-chunk))
   (cond ((= (Chunk-date fb) file-op-count*)
	  false)
	 (t
	  (let ((file-ch (Loaded-basis-chunk-file fb)))
	     (setf (File-chunk-callees ch) !())
	     (setf (Chunk-basis
		      (place-compiled-chunk file-ch))
		   (list file-ch))
	     (file-slurp (File-chunk-pathname file-ch)
			 (list compute-file-basis*)
;;;;			 (list file-ch)
	     )
	     file-op-count*))))

(defmacro end-header (&rest _)
   '(values))

(datafun :compute-file-basis end-header
   (defun :^ (form file-ch) ;; -- of file being slurped
      (cond ((memq ':no-compile (cdr form))
	     (place-loadable-chunk file-ch ':source)))
      (cond ((memq ':continue-slurping (cdr form))
,better idea: manage (:slurped (:macros <file>))
	     (format *error-output*
		     !"Warning-- ':continue-slurping' found in 'end-header' ~
                       form for ~%   ~s~%"
		     file-ch)))
   ;;;;      (cond ((and (memq ':continue-slurping (cdr form))
   ;;;;		  (eq slurping-how-much* ':at-least-header))
      (cond (end-header-dbg*
	     (format *error-output*
		     "Executing ~s~% "
		     form)))
      true))

(defun place-loaded-chunk (file-chunk file-manip)
   (let ((file-choice
	    (cond ((eq file-manip ':nochoice) false)
		  (t
		   (place-loadable-chunk file-chunk file-manip)))))
      (chunk-with-name `(:loaded ,(File-chunk-pathname file-chunk))
	   (\\ (name-exp)
	      (make-instance 'Loaded-chunk
		 :name name-exp
		 :loadable file-choice
		 :file file-chunk)))))

(defmethod derive ((lc Loaded-chunk))
   (let ((loadable (Loaded-chunk-loadable lc)))
      (let ((chunk-of-file-to-load
	       (cond (loadable
		      (Loadable-chunk-selection loadable))
		     (t
		      (Loaded-chunk-file lc)))))
	 (with-post-file-transduction-hooks
	    (cleanup-after-file-transduction
	       (let ((*package* *package*)
		     (*readtable* *readtable*)
		     (fload-indent* (+ 3 fload-indent*)))
		  (fload-op-message "Loading"
				    (File-chunk-yt-pathname
				        chunk-of-file-to-load)
				    (File-chunk-pathname chunk-of-file-to-load)
				    "...")

		  (load (File-chunk-pathname chunk-of-file-to-load))
		  (fload-op-message "...loaded"
				    (File-chunk-yt-pathname
				        chunk-of-file-to-load)
				    false
				    ""))))
	 (get-universal-time))))

;;; Whoever changes the alt-version of a Loadable-chunk (e.g.,
;;; 'fload-versions') must change its basis accordingly and
;;; update it if it's managed.      
;;; There's really nothing left for the 'deriver' to do, because
;;; once the basis is determined, you're done.  The derivees
;;; of the Loadable-chunk just load its basis.
(defun place-loadable-chunk (file-chunk manip)
   (let ((ld-chunk
	    (chunk-with-name
	       `(:loadable ,(File-chunk-pathname file-chunk))
	       (\\ (name)
		  (let ((is-source (file-chunk-is-source file-chunk))
			(alt-version
			   (let ((v (File-chunk-alt-version file-chunk)))
			      (and v (place-loadable-chunk v)))))
		     (let ((compiled-chunk
			    (and is-source
				 (place-compiled-chunk file-chunk))))
			(let ((new-lc
				 (make-instance 'Loadable-chunk
				    :name name
				    :file file-chunk
				    :manip (or manip ':follow)
				    :source (cond (is-source file-chunk)
						  (t false))
				    :compiled (cond (is-source compiled-chunk)
						    (t file-chunk))
				    :object (cond (is-source
						   (place-file-chunk
						      (File-chunk-pathname
							 compiled-chunk)))
						  (t file-chunk)))))
			   (push new-lc all-loadable-chunks*)
			   new-lc)))))))
      (cond (manip
	     (cond ((not (eq (Loadable-chunk-manip ld-chunk)
			     manip))
		    (format *error-output*
			    "Changing manip of ~s from ~s to ~s~%"
			    ld-chunk (Loadable-chunk-manip ld-chunk) manip)
		    (setf (Loadable-chunk-manip ld-chunk)
		          manip)))))
      (loaded-chunk-compute-basis ld-chunk)
      ld-chunk))

;;; Possible values: 
;;; :compile -- always compile when object file is missing or out of date)
;;; :object, :source -- always load object or source without compiling
;;;             (makes no sense if file is missing)
;;; :ask-every -- ask user what to do every time the file is looked at
;;;         (this gets tedious!)
;;; :ask-once -- ask user once and file the answer
;;; :ask-ask -- ask, then ask the user whether to keep asking
(defvar fload-compile-flag* ':ask)

(defun default-fload-manip () fload-compile-flag*)

(defun (setf default-fload-manip) (v)
   (cond ((memq v '(:ask :source :object :compile
		    :ask-ask :ask-once :ask-every))
	  (cond ((not (eq v fload-compile-flag*))
		 (setq fload-compile-flag* v)
		 (loadables-check-bases))))
	 (t
	  (cerror "I will leave the value unchanged"
		  !"Illegal value ~s for fload-compile*"
		  v))))

(define-symbol-macro fload-compile* (default-fload-manip))

(defvar all-loadable-chunks* !())

(defun loadables-check-bases ()
   (let ((loadables-needing-checking !())
	 (loadables-needing-update !()))
      (dolist (lc all-loadable-chunks*)
	 (cond ((memq (Loadable-chunk-manip lc)
		      '(:defer :follow))
		(push lc loadables-needing-checking))))
      (dolist (lc loadables-needing-checking)
	 (loaded-chunk-compute-basis lc)
	 (cond ((not (chunk-up-to-date lc))
		(push lc loadables-needing-update))))
      (chunks-update loadables-needing-update)))

;;; This must be called whenever the 'manip' field changes.
;;; The field is changed only by functions defined in this file.
;;; Make sure that that remains the case, and that changes in the
;;; 'manip' field are carefully synchronized with calls to this function. --
(defun loaded-chunk-compute-basis (loadable-ch)
   (let* ((file-ch (Loadable-chunk-file loadable-ch))
	  (manip (Loadable-chunk-manip loadable-ch))
	  (source-exists (Loadable-chunk-source file-ch))
	  (object-exists
	     (probe-file
		(File-chunk-pathname
		   (Loadable-chunk-object loadable-ch)))))
      (cond ((not (or source-exists object-exists))
	     (error "No source or object file can be found for ~s"
		    loadable-ch)))
      (cond ((File-chunk-alt-version file-ch)
	     (setf (Chunk-basis loaded-ch)
		   (list (place-loaded-chunk
			    (File-chunk-alt-version file-ch)
			    ':nochoice))))
	    (t
	     (cond (source-exists
		    (let ((lb (place-loaded-basis-chunk file-ch)))
		       (chunk-request-mgt lb)
		       (chunk-update lb)))
		   (t
		    (setq manip ':object)))
	     (cond ((memq manip '(:defer :follow))
		    (let ((prev-manip manip))
		       (setq manip fload-compile*)
		       (cond ((eq manip ':ask)
			      ;; Old style doesn't make enough distinctions
			      (setq manip ':ask-ask)))
		       (cond ((eq prev-manip ':follow)
			      (setf (Loadable-chunk-manip loadable-ch)
				    manip))))))
	     (cond ((memq manip '(:ask-once :ask-every :ask-ask))
		    (setq manip
		          (ask-user-for-manip loadable-ch
					      object-exists)))
		   ((and (eq manip ':object)
			 (not object-exists))
		    (cerror "I will compile the source file"
			    !"Request to load nonexistent ~
			      object file for ~s"
			    (File-chunk-pathname file-ch))
		    (setq manip ':compile)))
	     ;; At this point manip is either :object, :source, or
	     ;; :compile
	     (setf (Chunk-basis loaded-ch)
		   (list (ecase manip
			    (:source
			     (Loadable-chunk-source loadable-ch))
			    (:compile
			     (place-compiled-chunk
				(Loadable-chunk-source loadable-ch)))
			    (:object
			     (Loadable-chunk-object loadable-ch)))))
	     (setf (Loadable-chunk-selection loadable-ch)
		   (cond ((eq manip ':compile)
			  (place-file-chunk
			     (Compiled-file-chunk-pathname
				(head (Chunk-basis loaded-ch)))))
			 (t
			  (head (Chunk-basis loaded-ch)))))))))

(defun ask-user-for-manip (loadable-ch obj-exists)
   (let ()
      (loop 
	 (format *query-io*
	    !"Do you want to use the freshly compiled, ~a version ~
              ~% of ~s (~a/+/-, \\\\ to abort) ?"
	    (cond (obj-exists
		   "source, or object")
		  (t "or source"))
	    (File-chunk-pn file-ch)
	    (cond (obj-exists
		   "c/s/o")
		  (t
		   "c/s")))
	 (let ((manip 
		  (case (keyword-if-sym (read *query-io*))
		     ((:c :compile)
		      ':compile)
		     ((:s :source)
		      ':source)
		     ((:o :object)
		      (cond (obj-exists
			     ':object)
			    (t false)))
		     (:+
		      (setq fload-compile* ':compile)
		      ':defer)
		     (:-
		      (setq fload-compile* ':object)
		      ':defer)
		     ((:\\)
		      (throw 'fload-abort 'fload-aborted))
		     (t
		      false))))
	    (cond (manip
		   (manip-maybe-remember manip loadable-ch)
		   (return manip))
		  (t
		   (format *query-io*
			 !"Type 'c' to compile, then load object file;~%~
			   type 's' to load source file without compiling;~%~
			   ~atype '+' to make compiling be the default for ~
                           this and subsequent files;~%~
			   type '-' to make object versions be the default for ~
			   this and subsequent files.~%"
			 (cond (obj-exists
				!"type 'o' to load existing object file;~%")
			       (t "")))))))))

(defun manip-maybe-remember (manip loadable-ch)
   (block dialogue
      (let ((old-manip (Loadable-chunk-manip loadable-ch)))
	 (cond ((eq old-manip ':ask-once)
		(setf (Loadable-chunk-manip loadable ch)
		       manip))
	       ((eq old-manip ':ask-ask)
		(loop
		   (format *query-io*
			   !"Record this choice for future encounters ~
                             with this file (y/n/d, \\\\ to abort)? ")
		   (case (keyword-if-sym (read *query-io*))
		      ((:y :yes :t)
		       (setf (Loadable-chunk-manip loadable-ch)
			     manip)
		       (return-from dialogue))
		      ((:n :no)
		       (loop
			  (format *query-io*
				  !"Ask again next time whether to record ~
				    (y/n, \\\\ to abort)? ")
			  (let ((q (keyword-if-sym (read *query-io*))))
			     (case q
			        ((:n :no)
				 (setf (Loadable-chunk-manip loadable-ch)
				       ':ask-every)
				 (return-from dialogue))
				((:y :yes))
				((:\\)
				 (throw 'fload-abort 'fload-aborted))
				(t
				 (format *query-io*
				    !"Type 'y' so that every question about ~
				      what to load will be followed by a~%  ~
				      question about whether to record;~%~
				      type 'n' to suppress that second ~
				      annoying question.~%"))))))
		      ((:d :defer)
		       (setf (Loadable-chunk-manip loadable-ch)
			     ':defer)
		       (return-from dialogue))
		      ((:\\)
		       (throw 'fload-abort 'fload-aborted))
		      (t
		       (format *query-io*
			  "Type 'y' to record ~s as form of ~s;~%~
                           type 'n' to use that value once, ask again next;~%~
                           type 'd' to use value of 'fload-compile* ~
                                     to decide.~%"))))))))
  manip)

(defmethod derive ((ldbl Loadable-chunk))
   false)

(defun place-compiled-chunk (source-file-chunk)
   (let ((filename (File-chunk-pathname source-file-chunk)))
      (let ((compiled-chunk
	       (chunk-with-name
		   `(:compiled ,filename)
		   (\\ (exp)
		      (let ((new-chunk
			       (make-instance 'Compiled-file-chunk
				  :source-file source-file-chunk
				  :pathname (pathname-object-version
					       filename
					       false)
				  :name exp
				  ;; This will be changed when we
				  ;; derive the Loaded-basis-chunk
				  ;; for the source file, but
				  ;; it must always include the
				  ;; source-file chunk --
				  :basis (list source-file-chunk))))
			 new-chunk)))))
	 (setf (Chunk-basis compiled-chunk)
	       (list source-file-chunk))
	 compiled-chunk)))
	    
(defmethod derive ((cf-ch Compiled-file-chunk))
   (let ((pn (File-chunk-pn
		(Compiled-file-chunk-source-file cf-ch))))
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
			       (compile-file real-pn :output-file real-ov)
			       )
			      (t (compile-file real-pn)))))))
	    (fload-op-message "...compiled to" real-ov false "")))))

;;; A "sub-file" is a chunk of data that is a subset (usually proper)
;;; of a file, with the property that when the file is loaded the subset
;;; is sure to be loaded, so that (:loaded f) obviates (:slurped (S f)),
;;; where S is the kind of subfile.  (S is :macros, :nisp-type-decls, or
;;; the like.)
;;; We need 3 Chunk subclasses for sub-file-type N:
;;; Class name: N-class         Chunk name: (N f)            - the contents 
;;;                                                            of the sub-file
;;;             Slurped-N-class             (:slurped (N f)) - as slurped
;;;             Loaded-N-class              (:loaded (N f))  - Or-chunk
;;;							       (see below)
;;; Plus one or more existing classes L, typically (:loaded f), that trump
;;; (:slurped (N f))
;;; The Or-chunk has disjuncts (union L {(:slurped (N f))}), default
;;; (:slurped (N f)).
(defmacro def-sub-file-type (sym
			     &key
			     ((:default default-handler^)
			      'nil)
			     ((:file->state-fcn file->state-fcn^)
			      '(\\ (_) nil)))
   (let  ((sym-name (Symbol-name name)))
      (let ((subfile-class-name (build-symbol (:< (string-capitalize sym-name))
					      -chunk))
	    (slurped-class-name
	       (build-symbol Slurped- (:< sym-name) -chunk))
	    (loaded-sym-class-name
	       (build-symbol Loaded- (:< sym-name) -chunk)))
      (let* ((sub-pathname-read-fcn (build-symbol (:< subfile-class-name)
						  -pathname))
	     (slurped-subfile-read-fcn (build-symbol (:< slurped-class-name)
						     -subfile))
	     (slurped-subfile-placer-fcn
		(build-symbol place-
			      (:< slurped-subfile-read-fcn)
			      -chunk))
	     (ld-pathname-read-fcn (build-symbol (:< loaded-sym-class-name)
						 -pathname))
	     (loaded-class-loaded-file-acc
		(build-symbol (:< loaded-sym-class-name)
			      -loaded-file))
	     (slurp-task-name (build-symbol (:package :keyword)
				 slurp- (:< sym-name))))
         `(progn
	     (defclass ,subfile-class-name (Chunk)
		((pathname :reader ,sub-pathname-read-fcn
			   :initarg :pathname
			   :type pathname)))

	     (defmethod initialize-instance :after
				  ((ch ,subfile-class-name)
				   &rest _)
		(setf (Chunk-basis ch)
		      (list (place-file-chunk (,sub-pathname-read-fcn ch)))))

	     (defclass ,slurped-class-name (Chunk)
	       ((subfile :reader ,slurped-subfile-read-fcn
			 :initarg :subfile
			 :type Chunk)))

	     (defmethod initialize-instance :after
					    ((ch ,slurped-class-name)
					     &rest _)
		(setf (Chunk-basis ch)
		      (list (,slurped-subfile-read-fcn ch))))

	     (defun ,slurped-subfile-placer-fcn (pn)
		(chunk-with-name
		   `(:slurped (,',sub-file-type pn))
		   (\\ (name-exp)
		      (make-instance ',slurped-class-name
			 :name name-exp
			 :subfile (chunk-with-name
				     (cadr exp)
				     (\\ (sub-name-exp)
					(make-instance
					      ',sub-file-class-name
					   :name sub-name-exp
					   :pathname pn)))))))

	     (defclass ,loaded-sym-class-name (Or-chunk)
		((pathname :reader
			      (build-symbol (:< loaded-sym-class-name)
					    -pathname)
			   :initarg :pathname
			   :type pathname)
		 (loaded-file :accessor ,loaded-class-loaded-file-acc
			      :type Loaded-chunk)))

	     (defmethod initialize-instance :after
					    ((ch ,loaded-sym-class-name)
					     &key sub-file-type file
						  &allow-other-keys)
		(setq file (pathname file))
		(let ((file-chunk (place-loaded-chunk file false))
		      (slurped-chunk 
			 (,slurped-subfile-placer-fcn file)))
		   (setf (Chunk-basis ch)
			 (list file-chunk))
		   (setf (Or-chunk-disjuncts ch)
			 (list file-chunk slurped-chunk))))

	     (def-slurp-task ,slurp-task-name
		:default ,default-handler^
		:file->state-fcn ,file->state-fcn^)

	     (defmethod derive ((x ,subfile-class-name))
		(let ((file (,sub-pathname-read-fcn x)))
		   (file-slurp file
			       (list ,(build-symbol (:< slurp-task-name)
						    *))))))))))

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

;;;;(defvar save* nil)

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
	    (let ((fc (place-file-chunk (car oldl))))
	       (setf (File-chunk-alt-version fc)
		     (place-file-chunk (car newl)))
	       (push fc changing-chunks)))
	 (do ((oldl (filespecs->ytools-pathnames reset-olds) (cdr oldl)))
	     ((null oldl))
	    (let ((fc (place-file-chunk (car oldl))))
	       (setf (File-chunk-alt-version fc)
		     false)
	       (push fc changing-chunks)))
	 (chunks-update changing-chunks)
	 (nconc reset-olds (mapcar #'list set-olds set-news)))))


(def-sub-file-type :macros)

(defun macros-slurp-eval (e _) (eval e) false)

(datafun :slurp-macros defmacro #'macros-slurp-eval)

(defmacro needed-by-macros (&body l)
   `(progn ,@l))

(datafun :slurp-macros needed-by-macros
   (defun :^ (exp _)
      (dolist (e exp) (eval e))
      false))

(datafun :slurp-macros defsetf  #'macros-slurp-eval)

(datafun :slurp-macros defstruct #'macros-slurp-eval)

(datafun :slurp-macros defvar
   (defun :^ (form _)
      (eval `(defvar ,(cadr form)))
      false))

(datafun :slurp-macros defconstant
   (defun :^ (form _)
      (cond ((not (boundp (cadr form)))
	     (eval form)))
      false))

(datafun :slurp-macros subr-synonym #'macros-slurp-eval)

(datafun :slurp-macros datafun
  (defun :^ (e _)
    (cond ((eq (cadr e) 'attach-datafun)
	   (eval e)))
    false))



;;; Old stuff from here on --

*************************
(defvar loading-src-or-obj* false) ; :source, :object, or false

(defvar fcompl-flags* '(- -f))

(defmacro fcompl (&rest specs)
  `(do-fcompl ',specs))

(defvar default-fcompl-args* (vector false))

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
	    (cond ((null src-version)
		   (error "Fcompl couldn't find file ~s" pn))
		  ((or force-flag
		       (and (compilable src-version)
			    (or (not ur-time)
				(not ldble-time)
				(> ur-time ldble-time)
				(not (null changed-supporters))
				(not (achieved-load-status
					 lprec ':maybe-compiled)))))
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
				    src-version lprec))))
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

(defun compilable (pathname)
   (member (Pathname-type pathname) source-suffixes* :test #'equal))

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
