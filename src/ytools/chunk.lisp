;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;; $Id: chunk.lisp,v 1.1 2004/11/09 14:38:26 airfoyle Exp $

;;; This file depends on nothing but the facilities introduced
;;; in base.lisp and datafun.lisp

;;; A Chunk represents a piece of information, or a form of a piece of
;;; information, or perhaps merely a piece of information copied to a
;;; particular place.  We can't, and don't need to, describe the
;;; information in the Chunk.  We just have to ensure that anyone who
;;; needs it has a pointer to the Chunk so it can tell whether the
;;; information is up to date and if not recompute it.
(defclass Chunk ()
   ((name :reader Chunk-name
	  :initarg :name
	  :initform ""
	  :type string)
   (managed :accessor Chunk-managed
	    :initform false
	    :type boolean)
    ;; -- A chunk is being kept up to date if and only its 'managed'
    ;; field is true.  It makes no sense for a managed chunk to be
    ;; derived from an unmanaged one, so if this one is unmanaged so
    ;; are its derivees.
   (height :accessor Chunk-height
	   :type integer)
   ;; -- 0 if not derived from anything, else 1 + max height
   ;; of chunks in 'basis'. 
   (date :accessor Chunk-date
	 :initform -1
	 :type number)
   ;; -- date when updated or -1 if unknown
   (basis :accessor Chunk-basis
	  :initarg :basis
	  :initform !()
	  :type list)
   ;; -- list of Chunks this one is derived from
   (update-basis :accessor Chunk-update-basis
		 :initarg :update-basis
		 :initform false
		 :type list)
   ;; -- A list of Chunks governing the derivation of this one.
   ;; Think of it as keeping track of the "derivability" of this one.
   ;; Example: The chunk for a compiled file M might have in its
   ;; basis (:macros B) for a file containing macros it uses.  But if it
   ;; is necessary to recompile M, then the chunk (:loaded (:macros B))
   ;; must be up to date; i.e., the macros must actually be loaded.
   ;; So this chunk is in its update-basis.
   (derivees :accessor Chunk-derivees
	     :initform !()
	     :type list)
   ;; -- back pointers to chunks this one is a member of the basis 
   ;; or update-basis of
   (update-mark :accessor Chunk-update-mark
		:initform  0
		:type fixnum)
   ;; - Marked with number of update in progress
   )
)
;;; We sort of assume that update-basis is "derivative" in the sense
;;; that if the chunk is up to date wrt the basis, there are no "surprises" 
;;; in the update-basis.  Every chunk in the update basis is a function
;;; of a subset of the basis, so if we updated it it wouldn't change the
;;; the output of 'derive'.

(defgeneric derive (chunk))
;;; Recomputes chunk
;;;   and returns time when it changed (usually current time)
;;;   or false if it's up to date.
;;; Important: 'derive' works purely locally, and in particular
;;; never calls 'chunk-update'; someone else is assumed
;;; to have done that.

(defmethod initialize-instance :before ((ch Chunk) &rest initargs)
   (declare (ignore initargs))
   (setf (Chunk-height ch)
         (cond ((null (Chunk-basis ch)) 0)
	       (t
		(+ (reduce #'max (Chunk-basis ch)
			   :key #'Chunk-height)
		   1))))
   (dolist (b (Chunk-basis ch))
      (pushnew ch (Chunk-derivees b) :test #'eq))
   (dolist (b (Chunk-update-basis ch))
      (pushnew ch (Chunk-derivees b) :test #'eq)))

(defun chunk-manage (chunk)
   (cond ((Chunk-managed chunk)
	  (cond ((eq (Chunk-managed chunk)
		     ':in-transition)
		 (format *error-output*
		    "Chunk basis cycle detected at ~S~%" chunk))))
	 (t
	  (setf (Chunk-managed chunk) ':in-transition)
	  (dolist (b (Chunk-basis chunk))
	     (chunk-manage b))
	  (setf (Chunk-managed chunk) true))))

(defun chunk-non-manage (chunk)
   (labels ((chunk-gather-derivees (ch dvl)
	       (cond ((or (not (Chunk-managed ch))
			  (member ch dvl))
		      dvl)
		     (t
		      (on-list ch dvl)
		      (dolist (d (Chunk-derivees ch))
			 (cond ((memq ch (Chunk-basis d))
				(setf dvl
				      (chunk-gather-derivees d dvl)))))
		      dvl))))
      (let ((all-supported (chunk-gather-derivees chunk)))
	 (cond ((or (< (length all-supported) 2)
		    (yes-or-no-p
		       "Should I really stop managing (keeping up to date) all of the following chunks? --~%  ~S?"
		       all-supported))
		(dolist (d (Chunk-derivees chunk))
		   (setf (Chunk-managed chunk) false)))))))

(defvar update-no* 0)

(defun chunk-update (ch)
   (chunks-update (list ch)))

(defun chunks-update (chunks)
   ;; We have two mechanisms for keeping track of updates in progress.
   ;; The 'in-progress' stack is used to detect a situation where a
   ;; chunk is its own input, which would cause an infinite recursion
   ;; if undetected (and may indicate an impossible update goal).  The
   ;; mark mechanism is used to avoid updating a chunk which has
   ;; already been updated during this call to 'chunk-update'.  This
   ;; is "merely" for efficiency, but it's not a case of premature
   ;; optimization, because it's very easy for the derivation graph to
   ;; have exponentially many occurrences of a chunk if the graph is
   ;; expanded to a tree (which is what eliminating this optimization
   ;; would amount to).
   (let (chunk)
      (labels ((update-recursively (ch in-progress)
		  (cond ((member ch in-progress)
			 (format *error-output*
			    "Attempt to update ~s in order to update itself~%"
			    ch))
			((not (= (Chunk-update-mark ch) update-no*))
			 (let ((in-progress
				     (cons ch in-progress)))
;;; Fallacy -- a chunk can perfectly well be updated without being
;;; managed.			   
			    (dolist (b (Chunk-basis ch))
;;;;			       (cond ((not (Chunk-managed ch))
;;;;				      (cerror "I will try to manage it"
;;;;					      "Unmanaged chunk ~S detected ~% as supporter of ~S"
;;;;					      ch chunk)
;;;;				      (chunk-manage ch)))
			       (update-recursively
				   b in-progress))
			    (cond ((not (chunk-up-to-date ch))
				   (dolist (ub (Chunk-update-basis ch))
;;; Same fallacy --
;;;;				      (chunk-manage ub)
				      (update-recursively
				         ub in-progress))
				   ;; The deriver has not yet been called if
				   ;; 'chunk-up-to-date' returns false
				   (let ((new-date (derive ch)))
				      (cond ((> new-date (Chunk-date ch))
					     (setf (Chunk-date ch) new-date)
					     (dolist (d (Chunk-derivees ch))
						(cond ((Chunk-managed d)
						       (update-recursively
							  d))))))))))
			 (setf (Chunk-update-mark ch) update-no*)))))
	 (cond ((some #'Chunk-managed chunks)
		(setf update-no* (+ update-no* 1))
		(dolist (ch chunks)
		   (cond ((Chunk-managed ch)
			  (setq chunk ch)
			  (update-recursively chunk)))))))))

;;; If 'basis' is non-!(), then it's up to date
;;; if and only if its date >= the dates of its bases.
;;; -- If 'basis' is !(), then you have to call 'derive' for every
;;; inquiry, but the deriver makes it up to date.
(defun chunk-up-to-date (ch)
   (let ((ch-date (Chunk-date ch)))
      (cond ((null (Chunk-basis ch))
	     (let ((d (derive ch)))
		(cond (d
		       (cond ((< d ch-date)
			      (cerror "I will ignore the new date"
				      "Chunk-deriver returned date ~s, which is before current date ~s"
				      d (Chunk-date ch))
			      true)
			     (t
			      (setf (Chunk-date ch) d)
			      true)))
		      (t true))))
	    (t
	     ;; We're assuming here that the basis chunks have been
	     ;; updated.  Keep an eye on this assumption.
	     (every (\\ (b)
		       (=< (Chunk-date b) ch-date))
		    (Chunk-basis b))))))

(defvar chunk-table* (make-hash-table :test #'equal :size 300))

(defgeneric chunk-name->list-structure (name)

  (:method ((x t))
     x)

  (:method ((l consp))
     (<# chunk-name->list-structure l)))

;;; Index chunks by first pathname in their names, if any
;;; If 'creator' is non-false, it's a function that creates
;;; a new chunk, which is placed in the table.
(defun chunk-with-name (exp creator)
       ;; The kernel is the first atom in a non-car position,
       ;; which is often a pathname, but need not be.
   (labels ((chunk-name-kernel (e)
	       (dolist (x (cdr e) false)
		  (cond ((atom x) (return x))
			(t
			 (let ((k (chunk-name-kernel x)))
			    (cond (k (return k)))))))))
      (let ((name-kernel
	       (let ((list-version
		        (chunk-name->list-structure exp)))
		  (cond ((atom list-version) list-version)
			(t (chunk-name-kernel list-version))))))
	 (let ((bucket (href chunk-table* name-kernel)))
	    (do ((bl bucket (cdr bl))
		 (c nil))
		((or (null bl)
		     (equal (Chunk-name (setq c (car bl)))
			    exp))
		 (cond ((null bl)
			(cond (creator
			       (let ((new-chunk (funcall creator exp)))
				  (on-list new-chunk
				           (href chunk-table* pn))
				  (cond ((Chunk-managed new-chunk)
					 (chunk-update new-chunk)))
				  new-chunk))
			      (t false)))
		       (t c))))))))
