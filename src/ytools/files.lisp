;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: files.lisp,v 1.14.2.29 2005/03/06 22:51:17 airfoyle Exp $
	     
;;; Copyright (C) 1976-2004
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(fload fcompl fload-compile* bind-fload-compile* fcompl-reload*
	     fload-versions postponed-files-update
	     always-slurp end-header
	     debuggable debuggability*)))

(defvar file-op-count* 0)

(defvar fload-flags* '(- -f -c -a -s -o -x -z))
;;; -  -> "Clear sticky flags"
;;; -f -> "Force load even if apparently up to date"
;;; -c -> "Compile each file"
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
   (cond ((not file-op-in-progress*)
	  (setq file-op-count* (+ file-op-count* 1))))
   (let ((file-op-in-progress* true))
      (catch 'fload-abort
	 (with-compilation-unit ()
	    (file-op-defaults-update 'fload specs fload-flags*
				   (lambda () default-fload-args*)
				   (lambda (a)
				      (setq default-fload-args* a)))
	    (apply #'filespecs-fload (coerce default-fload-args* 'list))))))

(defun filespecs-fload (specs &optional (flags !()) (*readtable* *readtable*))
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
      (loaded-chunk-fload lpchunk force-load postpone-derivees)))

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
   ;; Perhaps the next two should be associated with this file's
   ;; Loaded-chunk, because they don't make sense for a data file.--
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

(defmethod derive-date ((fc File-chunk))
   (let ((v (File-chunk-alt-version fc)))
      (cond (v (derive-date v))
	    (t
	     (let ((pn (File-chunk-pathname fc)))
	        (cond ((probe-file pn)
		       (file-write-date pn))
		      (t
		       (error "File-chunk corresponds to nonexistent file: ~s"
			      fc))))))))

(defmethod derive ((fc File-chunk))
   false)

(defun File-chunk-pn (file-ch)
   (or (File-chunk-yt-pathname file-ch)
       (File-chunk-pathname file-ch)))

(defun file-chunk-is-source (file-ch)
   (eq (File-chunk-kind file-ch) ':source))

(defun loaded-chunk-force (loaded-chunk force postpone-derivees)
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

;;; 'pn' could be a Pseudo-pathname (see module.lisp)
(defgeneric pathname-denotation-chunk (pn))

(defmethod pathname-denotation-chunk ((ytpn YTools-pathname))
   (pathname-denotation-chunk (pathname-resolve ytpn false)))

(defmethod pathname-denotation-chunk ((pn pathname))
   (cond ((null (Pathname-type pn))
	  (let ((source-pn (pathname-source-version pn)))
	  (cond ((not source-pn)
		 (error "No source file corresponding to ~s"
			pn)))
	  (setq pn source-pn))))
   (place-File-chunk pn))

(defun place-File-chunk (pn &key kind)
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
		:type File-chunk)
   (object-file :initarg :object-file
		 :accessor Compiled-file-chunk-object-file
		 :type File-chunk)
   (last-compile-time :accessor Compiled-file-chunk-last-compile-time
		      :initform -1)))

;;; There are two sorts of strings attached to loadable files: what files
;;; they depend on in order to be compiled and loaded; and what version of
;;; is to be loaded (source, object, or some other file entirely).  The 
;;; latter is controlled directly by user requests, using 'fload' and 'fcompl',
;;; or some other facility (simple 'load', for instance).  The former is
;;; knottier.  YTFM wants to figure out dependencies by slurping, but 
;;; 'fload'/'fcompl' should be able to decipher dependencies declared by
;;; some version of 'defsystem.'  That's where Loadable-chunks come in (qv).

;;; An entity as (having the appropriate variant) loaded into primary
;;; memory --
(defclass Loaded-chunk (Chunk)
   (;; The entity this one governs --
    (loadee :accessor Loaded-chunk-loadee
	    :initarg :loadee)
    ;; The final basis is these plus those obtained by scanning
    ;; dependencies from whatever places are appropriate --
    (principal-bases :accessor Loaded-chunk-principal-bases
		     :initform !())
    ;; The Loadable-chunk that computes the basis of this one --
    (controller :accessor Loaded-chunk-controller
		:initarg :controller)))

(defgeneric place-Loaded-chunk (ch variant)
   (:method ((ch t) variant)
	    (declare (ignore variant))
      (error "No way to make a Loaded-chunk for ~s" ch)))

(defclass Loaded-file-chunk (Loaded-chunk)
   (;; The variant currently selected, either a File-chunk, or
    ;; a Loaded-file-chunk if we're indirecting through an alt-version --
    (selection :accessor Loaded-file-chunk-selection
	  :initform false)
    ;; The criterion (supplied by user) for how to make the selection --
    (manip :accessor Loaded-file-chunk-manip
	   :initarg :manip
	   :type (member :compile :source :object
			 :ask-once :ask-every :ask-ask :defer))
    ;; -- Meanings:
    ;;   :source -> don't compile, load source;
    ;;   :object -> compile only if no object; load object
    ;;   :compile -> compile and then load object;
    ;;   :ask-once -> ask user which to do and record result;
    ;;   :ask-every -> ask user every time basis is recomputed;
    ;;   :ask-ask -> ask once and ask if it should keep asking;
    ;;   :defer -> refer to value of fload-compile*;
    ;;   :follow -> replace 'manip' value with value of fload-compile*

    (source :reader Loaded-file-chunk-source
	    :initarg :source)
    (compiled :reader Loaded-file-chunk-compiled
	      :initarg :compiled)
    (object :reader Loaded-file-chunk-object
	    :initarg :object)))

(defmethod initialize-instance :after ((lc Loaded-chunk )
				       &key &allow-other-keys)
   (setf (Chunk-basis lc)
         (list (Loaded-chunk-loadee lc))))

(defun loaded-chunk-augment-basis (loaded-ch added-bases)
  (let ((non-prin-bases (loaded-chunk-verify-basis loaded-ch)))
     (setf (Chunk-basis loaded-ch)
	   (append (Loaded-chunk-principal-bases loaded-ch)
		   (union non-prin-bases
			  added-bases)))))

;;; The first elements of the basis are always the principal-bases.
;;; Use this to change the rest --
(defun loaded-chunk-change-basis (loaded-ch revised-basis)
  (loaded-chunk-verify-basis loaded-ch)
  (setf (Chunk-basis loaded-ch)
	(append (Loaded-chunk-principal-bases loaded-ch)
		revised-basis)))

;;; Returns excess basis elements beyond the principal ones --
(defun loaded-chunk-verify-basis (loaded-ch)
   (let ((loaded-basis (Chunk-basis loaded-ch)))
      (do ((pl (Loaded-chunk-principal-bases loaded-ch)
	       (cdr pl))
	   (bl loaded-basis (cdr bl)))
	  ((or (null pl)
	       (null bl)
	       (not (eq (car bl) (car pl))))
	   (cond ((null pl) bl)
		 (t
		  (error !"Chunk for loaded entity ~s~%~
                           has bogus basis ~s~%~
                           which differs from principal basis ~s"
			 loaded-ch loaded-basis)))))))

;;; This is a "meta-chunk," which keeps the network of Loaded-chunks 
;;; up to date.
;;; Corresponds to information gleaned from file header (or elsewhere)
;;; about what
;;; other files (and modules, ...) are necessary in order to load or
;;; compile and load, the given file.  This information forms the basis
;;; of the corresponding Loaded-chunk.
;;; This is a rudimentary chunk, which has no basis or derivees.  The only
;;; reason to use the chunk apparatus is to keep track of the date, which
;;; uses the file-op-count* instead of actual time.
(defclass Loadable-chunk (Chunk)
   ((controllee :reader Loadable-chunk-controllee
	  :initarg :controllee
	  :type Loaded-chunk))
   (:default-initargs :basis !()))
;;; -- The basis of this chunk is the empty list because it's a
;;; meta-chunk, which can't be connected to any chunk whose basis it
;;; might affect.

;;; Creates a Loadable-chunk to act as a controller for this filoid
;;; and the chunk corresponding to its being loaded.
;;; In depend.lisp we'll supply the standard controller for ordinary files
;;; following YTools rules.  
(defgeneric create-loaded-controller (filoid loaded)
  (:method ((x t) y)
     (declare (ignore y))
     (error "No way supplied to create controllers for objects of type ~s~%"
	    (type-of x))))

(defvar all-loaded-file-chunks* !())

(defmethod derive ((lc Loadable-chunk))
   (error "No method supplied for figuring out what entities ~s depends on"
	  (Loadable-chunk-controllee lc)))
;;; -- We must subclass Loadable-chunk to supply methods for figuring
;;; out file dependencies.  See depend.lisp for the YTFM approach.

;;;;(defmethod derive :after ((lc Loadable-chunk))
;;;;   (loaded-chunk-set-basis (Loadable-chunk-controllee lc))
;;;;;;;;   (compiled-chunk-basis-set (Loadable-chunk-controllee lc))
;;;;   )

(defmacro end-header (&rest _)
   '(values))

(defmethod place-Loaded-chunk ((file-ch File-chunk) manip)
   (place-Loaded-file-chunk file-ch manip))

(defun place-Loaded-file-chunk (file-chunk file-manip)
   (let ((lc (chunk-with-name `(:loaded ,(File-chunk-pathname file-chunk))
		(\\ (name)
		  (let ((is-source (file-chunk-is-source file-chunk)))
		     (let ((compiled-chunk
			      (and is-source
				   (place-compiled-chunk file-chunk))))
			(let ((new-lc
				 (make-instance 'Loaded-file-chunk 
				    :name name
				    :loadee file-chunk
				    :manip (or file-manip ':follow)
				    :source (cond (is-source file-chunk)
						  (t false))
				    :compiled (cond (is-source compiled-chunk)
						    (t file-chunk))
				    :object (cond (is-source
						   (place-File-chunk
						      (File-chunk-pathname
							 compiled-chunk)))
						  (t file-chunk)))))
;;;;			   (setq cc* compiled-chunk)
;;;;			   (loaded-file-chunk-set-basis new-lc)
			   (push new-lc all-loaded-file-chunks*)
			   new-lc))))
		:initializer
		   (\\ (new-lc)
		      (cond ((not (slot-truly-filled new-lc 'controller))
			     (setf (Loaded-chunk-controller new-lc)
				   (create-loaded-controller
				      file-chunk new-lc))))))))
      (cond ((eq file-manip ':noload)
	     (chunk-terminate-mgt lc ':ask))
	    ((and file-manip
		  (not (eq file-manip (Loaded-file-chunk-manip lc))))
	     (setf (Loaded-file-chunk-manip lc) file-manip)))
      lc))

(defmethod derive-date ((lc Loaded-file-chunk))
   false)

(defmethod derive ((lc Loaded-file-chunk))
   (let ((loaded-ch lc) file-ch)
      (loop 
	 (setq file-ch
	       (Loaded-file-chunk-selection loaded-ch))
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

;;; Given the names, one might think this should be a subclass of 
;;; Loaded-file-chunk.  However, Loaded-file-chunks are complex entities
;;; that can map into different versions depending on user decisions.
;;; This class is just a simple loaded-or-not-loaded affair.
;;; The file doesn't have to be a source file (!?)
(defclass Loaded-source-chunk (Chunk)
   ((file-ch :accessor Loaded-source-chunk-file
	     :initarg :file)))
	  
(defun place-Loaded-source-chunk (file-ch)
   (chunk-with-name `(:loaded ,(Chunk-name file-ch))
      (\\ (name)
	 (make-instance 'Loaded-source-chunk
	    :name name
	    :file file-ch))))

(defmethod derive-date ((l-source Loaded-source-chunk))
   false)

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

(defun loadeds-check-bases ()
   (let ((loadeds-needing-checking !()))
      (dolist (lc all-loaded-file-chunks*)
	 (cond ((memq (Loaded-file-chunk-manip lc)
		      '(:defer :follow))
		(on-list lc loadeds-needing-checking))))
;;;;      (dolist (lc loadeds-needing-checking)
;;;;	 (monitor-filoid-basis lc)
;;;;	 (cond ((not (chunk-up-to-date lc))
;;;;		(on-list lc loadeds-needing-update))))
      (chunks-update (cons fload-compile-flag-chunk*
			   loadeds-needing-checking)
		     false false)))

(defvar loaded-manip-dbg* false)

(defgeneric loaded-chunk-set-basis (loaded-ch)
   (:method ((lch t)) nil))

;;; This must be called whenever the 'manip' field changes.
(defmethod loaded-chunk-set-basis ((loaded-ch Loaded-file-chunk))
   (let* ((file-ch (Loaded-chunk-loadee loaded-ch))
	  (manip (Loaded-file-chunk-manip loaded-ch))
	  (source-exists (Loaded-file-chunk-source loaded-ch))
	  (object-exists
	     (probe-file
		(File-chunk-pathname
		   (Loaded-file-chunk-object loaded-ch)))))
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
	     (setf (Loaded-chunk-principal-bases loaded-ch)
		   (list (ecase manip
			    (:source
			     (Loaded-file-chunk-source loaded-ch))
			    (:compile
			     (place-compiled-chunk
				(Loaded-file-chunk-source loaded-ch)))
			    (:object
			     (Loaded-file-chunk-object loaded-ch)))))
	     (setf (Chunk-basis loaded-ch)
	           (append (Loaded-chunk-principal-bases loaded-ch)
			   (mapcar (\\ (callee)
				      (place-Loaded-chunk callee false))
				   (File-chunk-callees file-ch))))))
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
              ~% of ~s (o/s, \\\\ to abort)? "
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

(defun place-compiled-chunk (source-file-chunk)
   (let* ((filename (File-chunk-pathname source-file-chunk))
	  (ov (pathname-object-version filename false))
	  (real-ov (and (not (eq ov ':none))
			(pathname-resolve ov false))))
      (let ((compiled-chunk
	       (chunk-with-name
		   `(:compiled ,filename)
		   (\\ (exp)
		      (let ((new-chunk
			       (make-instance 'Compiled-file-chunk
				  :source-file source-file-chunk
				  :object-file (place-File-chunk
						  real-ov
						  :kind ':object)
				  :pathname (pathname-object-version
					       filename
					       false)
				  :kind ':object
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

(defmethod derive-date ((cf-ch Compiled-file-chunk))
   (let ((pn (File-chunk-pn
		(Compiled-file-chunk-source-file cf-ch))))
      (let ((ov (pathname-object-version pn false)))
	 (let ((real-ov (and (not (eq ov ':none))
			     (pathname-resolve ov false)))
	       (prev-time (Compiled-file-chunk-last-compile-time cf-ch)))
	    (let  ((old-obj-write-time
		      (and real-ov
			   (probe-file real-ov)
			   (pathname-write-time real-ov))))
	       (cond ((and old-obj-write-time
			   (>= prev-time 0)
			   (< prev-time old-obj-write-time))
		      (format *error-output*
			 !"Warning -- file ~s apparently compiled outside ~
                           control of ~s~%"
			 real-ov cf-ch)))
	       (or old-obj-write-time
		   prev-time))))))

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
	    (fload-indent* (+ 3 fload-indent*)))
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
      (file-op-message "Beginning compilation on" pn real-pn "...")
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

(eval-when (:compile-toplevel :load-toplevel)

(defvar sub-file-types* !())

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
   slurp-chunker
   ;; -- Function to produce chunk (:slurped (N F))
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
		(slurped-subfile-read-fcn
		   (build-symbol (:< slurped-sym-class-name)
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
				    slurp- (:< sym-name)))
		(sub-file-type-name (build-symbol (:< sym-name)
					     -sub-file-type*)))
	    `(progn
		(defclass ,subfile-class-name (Chunk)
		   ((pathname :reader ,sub-pathname-read-fcn
			      :initarg :pathname
			      :type pathname)))

		(defmethod derive-date ((ch ,subfile-class-name))
		   (file-write-date (,sub-pathname-read-fcn ch)))

		;; There's nothing to derive for the subfile; as long as the
		;; underlying file is up to date, so is the subfile.	     
		(defmethod derive ((ch ,subfile-class-name))
		   false)

		(defmethod initialize-instance :after
				     ((ch ,subfile-class-name)
				      &rest _)
		   (setf (Chunk-basis ch)
			 (list (place-File-chunk
				  (,sub-pathname-read-fcn ch)))))

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
   ;;;;		(format t "Placing new loaded chunk for ~s subfile chunk of ~s~%"
   ;;;;			',sym pn)
		   (chunk-with-name
		       `(:loaded (,',sym ,pn))
		       (\\ (name-exp)
   ;;;;		       (format t !"Creating new loaded chunk for ~
   ;;;;                                   ~s subfile chunk of ~s~%"
   ;;;;			       ',sym pn)
			  (let ()
			     (make-instance ',loaded-sym-class-name
				 :name name-exp
				 :pathname pn)))
		       :initializer
			  (\\ (new-ch)
			     (let* ((file-ch (place-File-chunk pn))
				    (loaded-file-chunk
				       (place-Loaded-chunk file-ch false)))
				(setf (,loaded-class-loaded-file-acc new-ch)
				      loaded-file-chunk)
				(setf (Chunk-basis new-ch)
				      (list file-ch))
				(setf (Or-chunk-disjuncts new-ch)
				      (list loaded-file-chunk
					    (,slurped-subfile-placer-fcn
					        pn)))))))

		(def-slurp-task ,slurp-task-name
		   :default ,default-handler^
		   :file->state-fcn ,file->state-fcn^)

 		(defmethod derive ((x ,slurped-sym-class-name))
		   (let ((file (,sub-pathname-read-fcn
				  (,slurped-subfile-read-fcn
				     x))))
   ;;;;		   (setq slurp-ch* x)
   ;;;;		   (break "About to slurp ~s~%" file)
		      (file-slurp file
				  (list ,(build-symbol (:< slurp-task-name)
						       *))
				  false)
		      (get-universal-time)))

		(defparameter ,sub-file-type-name
		   (make-Sub-file-type
		      :name ',sym
		      :chunker #',subfile-placer-fcn
		      :slurp-chunker #',slurped-subfile-placer-fcn
		      :load-chunker #',loaded-subfile-placer-fcn))

		(setq sub-file-types*
		      (cons ,sub-file-type-name
			    (remove-if
			       (\\ (sfty)
				  (eq (Sub-file-type-name sfty)
				      ',sym))
			       sub-file-types*))))))))
)

(defun lookup-sub-file-type (name)
   (dolist (sfty
	    sub-file-types*
	    (error "Undefined sub-file-type ~s" name))
      (cond ((eq (Sub-file-type-name sfty) name)
	     (return sfty)))))

(defun compiled-ch-sub-file-link (compiled-ch callee-ch sub-file-type load)
      (pushnew (funcall
		  (Sub-file-type-chunker sub-file-type)
		  (File-chunk-pathname callee-ch))
	       (Chunk-basis compiled-ch))
      (pushnew (funcall (cond (load
			       (Sub-file-type-load-chunker
				  sub-file-type))
			      (t
			       (Sub-file-type-slurp-chunker
				  sub-file-type)))
			 (File-chunk-pathname callee-ch))
	       (Chunk-update-basis compiled-ch)))

(eval-when (:compile-toplevel :load-toplevel)
   (def-sub-file-type :macros)

;;;;   (defparameter standard-sub-file-types* (list macros-sub-file-type*))

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

(datafun :slurp-macros eval-when
   (defun :^ (form _)
      (cond ((memq ':slurp-toplevel (cadr form))
	     (dolist (e (cddr form))
	        (eval e))))
      false))

(datafun :slurp-macros eval-when-slurping
   (defun :^ (form _)
      (dolist (e (cdr form))
	 (eval e))
      false))

(datafun :slurp-macros needed-by-macros eval-when-slurping)

(defvar fcompl-flags* '(- -f -x -l -z))
;;; -x -> "Stop managing compiled file (i.e., stop compiling)"
;;; -l -> "Load after compile" (from now on, unless -x)
;;; -f -> "Force compile even if apparently up to date"
;;; -z -> "Postpone update of chunks for files supported by this one"

(defmacro fcompl (&rest specs)
  `(do-fcompl ',specs))

;;; files, flags, readtable
(defvar default-fcompl-args* (vector !() !() false))

(defun do-fcompl (specs)
  (cond ((not file-op-in-progress*)
	 (setq file-op-count* (+ file-op-count* 1))))
  (let ((file-op-in-progress* true))
     (with-compilation-unit ()
	(file-op-defaults-update 'fcompl specs fcompl-flags*
				 (lambda () default-fcompl-args*)
				 (lambda (a)
				   (setq default-fcompl-args* a)))
	(apply #'filespecs-fcompl (coerce default-fcompl-args* 'list)))))

(defun filespecs-fcompl (specs flags *readtable*)
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

(defun file-ops-maybe-postpone (chunks)
   (setq postponed-file-chunks*
	 (append chunks postponed-file-chunks*)))

(defun postponed-files-update ()
   (chunks-update postponed-file-chunks* false false))

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

;;; For debugging --
(defun flchunk (pn)
   (let ((pn (->pathname pn)))
      (place-Loaded-file-chunk (place-File-chunk pn)
			       false)))