;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: files-new.lisp,v 1.1.2.1 2004/12/21 17:36:32 airfoyle Exp $
	     
;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(fload fcompl fload-compile* fcompl-reload*
	     fload-versions
	     always-slurp end-header
	     debuggable debuggability*)))

(defvar file-op-count* 0)

(defvar fload-flags* '(- -f -a -c -s -o -x))

(defmacro fload (&rest specs)
  `(do-fload ',specs))

;;; files, flags, readtable
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
				(-a ':ask)
				(-c ':compile)
				(-s ':source)
				(-o ':object)
				(-x ':noload)
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
	 (monitor-file-basis pchunk)
	 (cond ((Chunk-managed lpchunk)
		(cond (force-load
		       ;; Already managed, so forcing makes sense
		       (chunk-derive-and-record lpchunk)
		       (chunks-update (Chunk-derivees lpchunk)))))
	       (t
		(chunk-request-mgt lpchunk))))))

;;; The name of a File-chunk is always its yt-pathname if non-nil,
;;; else its pathname.
;;; This represents a primitive file, not one whose contents are
;;; managed by the file/chunk system itself (e.g., an object file
;;; whose compilation is handled by a Compiled-file-chunk).
(defclass File-chunk (Chunk)
  ((pathname :reader File-chunk-pathname
	     :initarg :pathname
	     :type pathname)
   (yt-pathname :reader File-chunk-yt-pathname
		:initarg :yt-pathname
		:initform false)
   ;;  -- Either false or a YTools pathname, in which case
   ;; 'pathname' is its resolution
   (kind :reader File-chunk-kind
	 :initarg :kind)
   ;; -- :source, :object, :data
   (readtable :accessor File-chunk-readtable
	      :initarg :readtable
	      :initform false)
   (alt-version :accessor File-chunk-alt-version
		:initarg :alt-version
		:initform false)
   ;; - If not nil, the File-chunk for the alternative version.
   ;; Files that must be loaded when this one is --
   (callees :accessor File-chunk-callees
	    :initform !())
   ;; Inverse of 'callees' --
   (callers :accessor File-chunk-callers
	    :initform !())
   ;; Files that must be loaded when this one is read --
   (read-basis :accessor File-chunk-read-basis
	       :initform false))
)
;; -- Files are the finest grain we allow.  If a file is out of date,
;; then every chunk associated with the file is assumed to be out of
;; date.  More than one chunk can be associated with a file.  For now,
;; don't allow a chunk to be associated with more than one file.

;; These two methods ensure that 'callers' is the inverse of 'callees' --
(defmethod (setf File-chunk-callees) :before (_ file-ch)
   (dolist (clee (File-chunk-callees file-ch))
      (setf (File-chunk-callers clee)
	    (remove file-ch (File-chunk-callers clee)))))

(defmethod (setf File-chunk-callees) :after (_ file-ch)
   (dolist (clee (File-chunk-callees file-ch))
      (setf (File-chunk-callers clee)
	    (adjoin file-ch (File-chunk-callers clee)))))

(defmethod derive ((fc File-chunk))
   (let ((v (File-chunk-alt-version fc)))
      (cond (v (derive v))
	    (t
	     (let ((pn (File-chunk-pathname fc)))
	        (cond ((probe-file pn)
		       (file-write-date pn))
		      (t
		       (error "File-chunk corresponds to nonexistent file: ~s"
			      fc))))))))

(defun File-chunk-pn (file-ch)
   (or (File-chunk-yt-pathname file-ch)
       (File-chunk-pathname file-ch)))

(defun file-chunk-is-source (file-ch)
   (eq (File-chunk-kind file-ch) ':source))

;;;;   (pathname-is-source (File-chunk-pathname file-ch))

(defvar final-load* false)
; When building systems, bind to true to minimize soul-searching later.

(defun place-file-chunk (pn &key kind)
   (multiple-value-bind (yt-pathname l-pathname)
			(cond ((is-YTools-pathname pn)
			       (values pn (pathname-resolve pn true)))
			      (t
			       (values false (pathname pn))))
      (cond ((not kind)
	     (setq kind
	       (cond ((pathname-is-source l-pathname)
		      ':source)
		     ((pathname-is-object l-pathname)
		      ':object)
		     (t
		      ':data)))))
      (chunk-with-name
	 `(:file ,(or yt-pathname l-pathname))
	 (\\ (e)
	    (make-instance 'File-chunk
	       :name e
	       :kind kind
	       :yt-pathname yt-pathname
	       :pathname l-pathname
	       :alt-version false)))))

;;; Represents a source file having an up-to-date compiled version.
;;; Actually, an attempt to compile is good enough, because we wouldn't
;;; want to try to recompile before a basis chunk changes.
(defclass Compiled-file-chunk (File-chunk)
  ((source-file :initarg :source-file
		:reader Compiled-file-chunk-source-file
		:type File-chunk)))

;;; A file as having the appropriate variant loaded into primary
;;; memory --
(defclass Loaded-chunk (Chunk)
   (;; The File-chunk this one governs --
    (file :accessor Loaded-chunk-file
	  :initarg :file
	  :type File-chunk)
    ;; The variant currently selected, either a File-chunk, or
    ;; a Loaded-chunk if we're indirecting through an alt-version --
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
    (compiled :reader Loadable-chunk-compiled
	      :initarg :compiled)
    (object :reader Loadable-chunk-object
	    :initarg :object)))

(defmethod initialize-instance :after ((lc Loaded-chunk)
				       &key &allow-other-keys)
   (setf (Chunk-basis lc)
         (list (Loaded-chunk-file lc))))

;;; This is a "meta-chunk," which keeps the network of Loaded-chunks 
;;; up to date.
;;; Corresponds to information gleaned from file header about what
;;; other files (and modules, ...) are necessary in order to load or
;;; compile and load, the given file.  This information forms the basis
;;; of the corresponding Loaded-chunk.
;;; Represents the decision having been made which variant of a
;;; file to load, source, object, or alternative
;;; version of file (see fload-versions, below).
(defclass Loadable-chunk (Chunk)
   ((controllee :reader Loadable-chunk-controllee
	  :initarg :controllee
	  :type Loaded-chunk))
   (:default-initargs :basis !()))
;;; -- The basis of this chunk is the empty list because it's a
;;; meta-chunk, which can't be connected to any chunk whose basis it
;;; might affect.  That's because 'chunks-update' is based on the
;;; assumption that the bases it sees are not in the process of
;;; changing.

(defvar all-loadable-chunks* !())

;;; Whoever changes the alt-version of a Loadable-chunk (e.g.,
;;; 'fload-versions') must change its basis accordingly and
;;; update it if it's managed.      
(defun place-loadable-chunk (file-chunk manip)
   (let ((lfc (place-loaded-chunk file-chunk manip)))
      (let ((ld-chunk
	       (chunk-with-name
		  `(:loadable ,(File-chunk-pathname file-chunk))
		  (\\ (name-exp)
		     (let ((new-lc (make-instance 'Loadable-chunk
				      :controllee lfc)))
			(push new-lc all-loadable-chunks*)
			new-lc))
		  :initializer #'loadable-chunk-compute-basis)))
      (cond (manip
	     (cond ((not (eq (Loadable-chunk-manip ld-chunk)
			     manip))
		    (format *error-output*
			    "Changing manip of ~s from ~s to ~s~%"
			    ld-chunk (Loadable-chunk-manip ld-chunk) manip)
		    (setf (Loadable-chunk-manip ld-chunk)
		          manip)
		    (loadable-chunk-compute-basis ld-chunk)))))
      ld-chunk))

(defmethod derive ((lc Loadable-chunk))
   (error "No method supplied for figuring out what files ~s depends on"
	  (File-chunk-file (Loadable-chunk-file lc))))
;;; -- We must subclass Loadable-chunk to supply methods for figuring
;;; out file dependencies.  See depend.lisp for the YTFM approach.

(defmacro end-header (&rest _)
   '(values))

(defun place-loaded-chunk (file-chunk file-manip)
   (let ((lc (chunk-with-name `(:loaded ,(File-chunk-pathname file-chunk))
		(\\ (name)
		  (let ((is-source (file-chunk-is-source file-chunk)))
		     (let ((compiled-chunk
			    (and is-source
				 (place-compiled-chunk file-chunk))))
			(let ((new-lc
				 (make-instance 'Loaded-chunk
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
			   new-lc))))


		   (\\ (name-exp)
		      (make-instance 'Loaded-chunk
			 :name name-exp
			 :file file-chunk)))))
	 (cond ((eq file-manip ':noload)
		(chunk-terminate-mgt lc ':ask)))
	 lc)))
   (let ((file-choice
	    (cond ((eq file-manip ':nochoice) false)
		  (t
		   (place-loadable-chunk
		      file-chunk
		      (cond ((eq file-manip ':noload)
			     false)
			    (t file-manip)))))))






(defmethod derive ((lc Loaded-chunk))
   (let ((chunk-of-file-to-load lc))
      (loop 
	 (let ((loadable (Loaded-chunk-loadable
			     chunk-of-file-to-load)))
	    (setq chunk-of-file-to-load
		  (cond (loadable
			 (Loadable-chunk-selection loadable))
			(t
			 (Loaded-chunk-file lc))))
	    (cond ((not (typep chunk-of-file-to-load
			       'Loaded-chunk))
		   (return)))
	    ;; Indirection; go around again looking
	    ;; for an actual file.
	  ))
      (cond ((not (typep chunk-of-file-to-load 'File-chunk))
	     (error "Can't extract file to load from ~s"
		    lc)))
      (with-post-file-transduction-hooks
	 (cleanup-after-file-transduction
	    (let ((*package* *package*)
		  (*readtable* *readtable*)
		  (fload-indent* (+ 3 fload-indent*)))
	       (fload-op-message "Loading"
				 (File-chunk-pn
				     chunk-of-file-to-load)
				 (File-chunk-pathname chunk-of-file-to-load)
				 "...")

	       (load (File-chunk-pathname chunk-of-file-to-load))
	       (fload-op-message "...loaded"
				 (File-chunk-pn
				     chunk-of-file-to-load)
				 false
				 ""))))
      true))



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

;;; Not clear what this is supposed to do after the reorganization --
(defun loadables-check-bases ()
   (let ((loadables-needing-checking !())
	 (loadables-needing-update !()))
      (dolist (lc all-loadable-chunks*)
	 (cond ((memq (Loadable-chunk-manip lc)
		      '(:defer :follow))
		(push lc loadables-needing-checking))))
      (dolist (lc loadables-needing-checking)
	 (loadable-chunk-compute-basis lc)
	 (cond ((not (chunk-up-to-date lc))
		(push lc loadables-needing-update))))
      (chunks-update loadables-needing-update)))

;;; This must be called whenever the 'manip' field changes.
;;; The field is changed only by functions defined in this file.
;;; Make sure that that remains the case, and that changes in the
;;; 'manip' field are carefully synchronized with calls to this function. --
(defun loadable-chunk-compute-basis (loadable-ch)
   (let* ((file-ch (Loadable-chunk-file loadable-ch))
	  (manip (Loadable-chunk-manip loadable-ch))
	  (source-exists (Loadable-chunk-source loadable-ch))
	  (object-exists
	     (probe-file
		(File-chunk-pathname
		   (Loadable-chunk-object loadable-ch)))))
      (format t "At start, manip = ~s~%" manip)
      (cond ((not (or source-exists object-exists))
	     (error "No source or object file can be found for ~s"
		    loadable-ch)))
      (cond ((File-chunk-alt-version file-ch)
	     (setf (Chunk-basis loadable-ch)
		   (list (place-loaded-chunk
			    (File-chunk-alt-version file-ch)
			    ':nochoice))))
	    (t
	     (cond (source-exists
		    (let ((lb (place-loaded-basis-chunk file-ch)))
		       (chunk-request-mgt lb)
		       (chunk-update lb)))
;;;;		   (t
;;;;		    (setq manip ':object))
		   )
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
	     (format t "manip = ~s loadable-ch = ~s~%"
		     manip loadable-ch)
	     (setf (Chunk-basis loadable-ch)
		   (list (ecase manip
			    (:source
			     (Loadable-chunk-source loadable-ch))
			    (:compile
			     (place-compiled-chunk
				(Loadable-chunk-source loadable-ch)))
			    (:object
			     (Loadable-chunk-object loadable-ch)))))))
;;;;      (format t "Ready to set selection~%")
      (setf (Loadable-chunk-selection loadable-ch)
	    (cond ((eq manip ':compile)
		   (place-file-chunk
		      (File-chunk-pathname
			 (head (Chunk-basis loadable-ch)))))
		  (t
		   (head (Chunk-basis loadable-ch)))))))

(defun ask-user-for-manip (loadable-ch obj-exists)
   (let ()
      (loop 
	 (format *query-io*
	    !"Do you want to use the freshly compiled, ~a version ~
              ~% of ~s (~a/+/-, \\\\ to abort) ?"
	    (cond (obj-exists
		   "source, or object")
		  (t "or source"))
	    (File-chunk-pn (Loadable-chunk-file loadable-ch))
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
		(setf (Loadable-chunk-manip loadable-ch)
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
			 (setf (Chunk-basis new-chunk)
			       (list source-file-chunk))
			 new-chunk)))))
	 compiled-chunk)))
	    
(defvar debuggability* 1)

(defmethod derive ((cf-ch Compiled-file-chunk))
   (let ((pn (File-chunk-pn
		(Compiled-file-chunk-source-file cf-ch))))
      (let ((ov (pathname-object-version pn false)))
	 (let ((real-pn (pathname-resolve pn true))
	       (real-ov (and (not (eq ov ':none))
			     (pathname-resolve ov false))))
	    (let  ((old-obj-write-time
		      (and real-ov (pathname-write-time real-ov)))
		   (now-compiling*    pn)
		   (now-loading* false)
		   (now-slurping* false)
		   (debuggability* debuggability*)
		   (fload-indent* (+ 3 fload-indent*)))
	       (fload-op-message "Beginning compilation on" pn real-pn "...")
	       (with-post-file-transduction-hooks
		  (cleanup-after-file-transduction
		     (let ((*compile-verbose* false))
			(cond (real-ov
			       (compile-file real-pn :output-file real-ov))
			      (t
			       (compile-file real-pn)
			       (setq real-ov (pathname-object-version
					         real-pn true))))
			(let* ((success
				  (and real-ov
				       (or (not old-obj-write-time)
					   (> (pathname-write-time real-ov)
					      old-obj-write-time)))))
			   (fcompl-log
			      real-pn
			      (and success real-ov))))))
	       (fload-op-message "...compiled to" real-ov false "")
	       (get-universal-time))))))

(eval-when (:compile-toplevel :load-toplevel)

;;; A "sub-file" is a chunk of data that is a subset (usually proper)
;;; of a file, with the property that when the file is loaded the subset
;;; is sure to be loaded, so that (:loaded f) obviates (:slurped (S f)),
;;; where S is the kind of subfile.  (S is :macros, :nisp-type-decls, or
;;; the like.)
;;; We need 3 Chunk subclasses for sub-file-type N:
;;; Class name: N-chunk         Chunk name: (N f)            - the contents 
;;;                                                            of the sub-file
;;;             Slurped-N-chunk             (slurped (N f)) - as slurped
;;;             Loaded-N-chunk              (loaded (N f))  - Or-chunk
;;;							       (see below)
;;; Plus one or more existing classes L, typically (loaded f), that trump
;;; (slurped (N f))
;;; The Or-chunk has disjuncts (union L {(slurped (N f))}), default
;;; (slurped (N f)).

(defstruct (Sub-file-type)
   name
   chunker
   ;; -- Function to produce chunk (N F) for file F (N= name of Sub-file-type)
   load-chunker
   ;; -- Function to produce chunk (:loaded (N F)) 
)

(defmacro def-sub-file-type (sym
			     &key
			     ((:default default-handler^)
			      'nil)
			     ((:file->state-fcn file->state-fcn^)
			      '(\\ (_) nil)))
   (let  ((sym-name (Symbol-name sym)))
      (let ((subfile-class-name (build-symbol (:< (string-capitalize sym-name))
					      -chunk))
	    (slurped-sym-class-name
	       (build-symbol Slurped- (:< sym-name) -chunk))
	    (loaded-sym-class-name
	       (build-symbol Loaded- (:< sym-name) -chunk)))
      (let* ((sub-pathname-read-fcn (build-symbol (:< subfile-class-name)
						  -pathname))
	     (slurped-subfile-read-fcn (build-symbol (:< slurped-sym-class-name)
						     -subfile))
	     (subfile-placer-fcn (build-symbol place- (:< sym-name) -chunk))
	     (slurped-subfile-placer-fcn
		(build-symbol place-slurped- (:< sym-name) -chunk))
	     (ld-pathname-read-fcn (build-symbol (:< loaded-sym-class-name)
						 -pathname))
	     (loaded-class-loaded-file-acc
		(build-symbol (:< loaded-sym-class-name)
			      -loaded-file))
	     (loaded-subfile-placer-fcn
	        (build-symbol place-loaded- (:< sym-name) -chunk))
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

	     (defclass ,slurped-sym-class-name (Chunk)
	       ((subfile :reader ,slurped-subfile-read-fcn
			 :initarg :subfile
			 :type Chunk)))

	     (defmethod initialize-instance :after
					    ((ch ,slurped-sym-class-name)
					     &rest _)
		(setf (Chunk-basis ch)
		      (list (,slurped-subfile-read-fcn ch))))

	     (defun ,subfile-placer-fcn (pn)
		(chunk-with-name
		   `(,',sym ,pn)
		   (\\ (sub-name-exp)
		      (make-instance
			 ',subfile-class-name
			:name sub-name-exp
			:pathname pn))))

	     (defun ,slurped-subfile-placer-fcn (pn)
		(chunk-with-name
		   `(:slurped (,',sym ,pn))
		   (\\ (name-exp)
		      (make-instance ',slurped-sym-class-name
			 :name name-exp
			 :subfile (,subfile-placer-fcn pn)))))

	     (defclass ,loaded-sym-class-name (Or-chunk)
		((pathname :reader ,ld-pathname-read-fcn
			   :initarg :pathname
			   :type pathname)
		 (loaded-file :accessor ,loaded-class-loaded-file-acc
			      :type Loaded-chunk)))

	     (defun ,loaded-subfile-placer-fcn (pn)
		(format t "Placing new loaded chunk for ~s subfile chunk of ~s~%"
			',sym pn)
		(chunk-with-name
		    `(:loaded (,',sym ,pn))
		    (\\ (name-exp)
		       (format t "Creating new loaded chunk for file chunk of ~s~%"
			       pn)
		       (let ()
			  (make-instance ',loaded-sym-class-name
			      :name name-exp
			      :pathname pn)))
		    :initializer
		       (\\ (new-ch)
			  (setf (,loaded-class-loaded-file-acc new-ch)
				(place-loaded-chunk
				   (place-file-chunk pn)
				   false))
			  (setf (Or-chunk-disjuncts new-ch)
			        (list new-ch
				      (,slurped-subfile-placer-fcn pn))))))

	     (def-slurp-task ,slurp-task-name
		:default ,default-handler^
		:file->state-fcn ,file->state-fcn^)

	     (defmethod derive ((x ,slurped-sym-class-name))
		(let ((file (,sub-pathname-read-fcn
			       (,slurped-subfile-read-fcn
				  x))))
		   (file-slurp file
			       (list ,(build-symbol (:< slurp-task-name)
						    *))
			       false)
		   (get-universal-time)))

	     (defparameter ,(build-symbol (:< sym-name)
					  -sub-file-type*)
		(make-Sub-file-type
		   :name ',sym-name
		   :chunker #',subfile-placer-fcn
		   :load-chunker #',loaded-subfile-placer-fcn)))))))
)

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

(eval-when (:compile-toplevel :load-toplevel)
   (def-sub-file-type :macros)

   (defvar standard-sub-file-types* (list macros-sub-file-type*))

   (defun macros-slurp-eval (e _) (eval e) false))

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

(defvar end-header-dbg* false)

(datafun compute-file-basis in-readtable
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

(datafun compute-file-basis end-header
   (defun :^ (form file-ch) ;; -- of file being slurped
      (cond ((memq ':no-compile (cdr form))
	     (place-loadable-chunk file-ch ':source)))
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

(defvar fcompl-flags* '(- -f -x -l))

(defmacro fcompl (&rest specs)
  `(do-fcompl ',specs))

;;; files, flags, readtable
(defvar default-fcompl-args* (vector false ))

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
      (let ((force-flag false)
	    (load-flag false)
	    (cease-mgt false))
	(dolist (flag flags)
	    (case flag
	       (-f (setq force-flag true))
	       (-l (setq load-flag true))
	       (-x (setq cease-mgt true))
	       (t (cerror "I will ignore it"
			  "Illegal flag to 'fcompl': ~s" flag))))
	(dolist (pn (filespecs->ytools-pathnames specs))
	   (pathname-fcompl pn force-flag load-flag cease-mgt)))))

(defvar fcompl-reload* ':ask)

(defgeneric pathname-fcompl (pn &key force-compile
				     load
				     cease-mgt))

(defmethod pathname-fcompl ((pn pathname)
			    &key force-compile
				 load
				 cease-mgt)
   (let* ((file-chunk
	     (place-file-chunk pn))
	  (compiled-chunk
	     (place-compiled-chunk file-chunk))
;;;;	  (comp-date
;;;;	     (pathname-write-time
;;;;	        (File-chunk-pathname compiled-chunk)))
	  )
      (monitor-file-basis file-chunk)
      (cond (cease-mgt
	     (cond (force-compile
		    ;; One last fling --
		    (derive compiled-chunk)))
	     (chunk-terminate-mgt compiled-chunk ':ask)
	     (cond (load
		    (cerror "I will ignore the load request"
			    !"Request to load ~s contradicts request to ~
                              cease managing it"))))
	    (t
	     (let ((comp-date
		      (Chunk-date compiled-chunk)))
		(chunk-request-mgt compiled-chunk)
		(cond ((and force-compile
			    (= (Chunk-date compiled-chunk)
			       comp-date))
		       ;; Hasn't been compiled yet
		       (derive compiled-chunk)))
		(cond ((or load
			   (load-after-compile))
		       (chunk-request-mgt
			  (place-loaded-chunk pn ':compiled)))))))))

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

(defun monitor-file-basis (file-ch)
   (let ((lb-ch (place-loadable-chunk file-ch false)))
      (chunk-request-mgt lb-ch)
      (chunk-update lb-ch)))

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
