;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;; $Id: chunk.lisp,v 1.1.2.4 2004/11/27 20:03:02 airfoyle Exp $

;;; This file depends on nothing but the facilities introduced
;;; in base.lisp and datafun.lisp

;;; The date assigned to chunks with no basis, i.e., leaves.
(defconstant +no-supporters-date+ -1000)

;;; A Chunk represents a piece of information, or a form of a piece of
;;; information, or perhaps merely a piece of information copied to a
;;; particular place.  We can't, and don't need to, describe the
;;; information in the Chunk.  We just have to ensure that anyone who
;;; needs it has a pointer to the Chunk so it can tell whether the
;;; information is up to date and if not recompute it.
(defclass Chunk ()
  ((name :reader Chunk-name  ; -- An S-expression
	  :initarg :name
	  :initform "")
   (manage-request :accessor Chunk-manage-request
		   :initform false
		   :type boolean)
   ;; -- true if user has declared that this chunk should be
   ;; managed.
   (managed :accessor Chunk-managed
	    :initform false
	    :type boolean)
    ;; -- A chunk is being kept up to date if and only if its 'managed'
    ;; field is true.  
    ;; Global invariant: c is managed if and only if either its
    ;; manage-request is t or some derivee is managed.
   (height :accessor Chunk-height
	   :type integer)
   ;; -- 0 if not derived from anything, else 1 + max height
   ;; of chunks in 'basis'. 
   (date :accessor Chunk-date
	 :initform -1
	 :type number)
   ;; -- date when updated or -1 if unknown
   (latest-supporter-date
         :accessor Chunk-latest-supporter-date
	 :initform +no-supporters-date+
	 :type number)
   ;; -- date of latest element of basis, or basis of some element of basis,
   ;;  or .....  Value = -1 if unknown
   ;; More precisely, this is the latest value any supporter has ever had.
   ;; If the basis of a chunk changes, that will not ever cause its
   ;; 'latest-supporter-date' to decrease.  In particular, a chunk can
   ;; go from begin out of date to being up to date only by a call
   ;; to 'chunks-update'.
   (basis :accessor Chunk-basis
	  :initarg :basis
	  :initform !()
	  :type list)
   ;; -- list of Chunks this one is derived from
   (derivees :accessor Chunk-derivees
	     :initform !()
	     :type list)
   ;; -- back pointers to chunks this one is a member of the basis of
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
   (update-derivees :accessor Chunk-update-derivees
		    :initform !()
		    :type list)
   ;; -- back pointers to chunks this one is a member of the
   ;; update basis of
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

(defgeneric print-innards (x srm)
  (:method ((x t) srm)
     (declare (ignore srm))
     (values)))

(defmethod print-object ((c Chunk) srm)
   (format srm "#<Chunk~a~s"
	   (cond ((Chunk-managed c) "=")
		 (t "_"))
	   (Chunk-name c))
   (print-innards c srm)
   (format srm "~a>"
	   (cond ((Chunk-is-leaf c)
		  "*")
		 ((and (slot-boundp c 'date)
		       (slot-boundp c 'latest-supporter-date)
		       (>= (Chunk-date c)
			   (Chunk-latest-supporter-date c)))
		  "!")
		 (t "?"))))

(defgeneric derive (chunk))
;;; Recomputes chunk
;;;   and returns time when it changed (usually current time)
;;;   or false if it's up to date.
;;; Important: 'derive' works purely locally, and in particular
;;; never calls 'chunk-update'; someone else is assumed
;;; to have done that.

(defmethod initialize-instance :after ((ch Chunk) &rest initargs)
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
      (pushnew ch (Chunk-update-derivees b) :test #'eq))
   (cond ((null (Chunk-basis ch))
	  (leaf-chunk-update ch)))
;;; There is no point in doing this, because 'ch' has no derivees
;;; immediately after being created.   
;;;;   (set-latest-support-date ch)
   )

(defun Chunk-is-leaf (ch) (null (Chunk-basis ch)))

(defmethod (setf Chunk-basis) :before (new-basis ch)
                                     (declare (ignore new-basis))
   (dolist (b (Chunk-basis ch))
      (setf (Chunk-derivees b)
	    (remove ch (Chunk-derivees b)))))

(defmethod (setf Chunk-basis) :after (new-basis ch)
                                     (declare (ignore new-basis))
   (dolist (b (Chunk-basis ch))
      (setf (Chunk-derivees b)
	    (adjoin ch (Chunk-derivees b))))
   (set-latest-support-date ch))

;;; Returns a list of all chunks whose 'latest-supporter-date' get
;;; updated.
(defun set-latest-support-date (ch)
   (let ((latest-supporter-date +no-supporters-date+))
      (dolist (b (Chunk-basis ch))
	 (let ((late (max (Chunk-date b)
			  (Chunk-latest-supporter-date b))))
	    (cond ((> late latest-supporter-date)
		   (setf latest-supporter-date late)))))
      (cond ((or (> latest-supporter-date
		    (Chunk-latest-supporter-date ch))
		 (= latest-supporter-date +no-supporters-date+))
	     (setf (Chunk-latest-supporter-date ch) latest-supporter-date)
	     (cons ch
		   (mapcan (\\ (d) (set-latest-support-date d))
			   (Chunk-derivees ch))))
	    (t !()))))

(defmethod (setf Chunk-update-basis) :before (new-update-basis ch)
                                     (declare (ignore new-update-basis))

   (dolist (b (Chunk-update-basis ch))
      (setf (Chunk-update-derivees b)
	    (remove ch (Chunk-update-derivees b)))))

(defmethod (setf Chunk-update-basis) :after (new-update-basis ch)
                                     (declare (ignore new-update-basis))
   (dolist (b (Chunk-update-basis ch))
      (setf (Chunk-update-derivees b)
	    (adjoin ch (Chunk-update-derivees b)))))

(defun chunk-request-mgt (c)
   (cond ((not (Chunk-manage-request c))
	  (setf (Chunk-manage-request c) true)
	  (chunk-manage c))))

;;; This doesn't call chunk-update, but presumably everyone who calls
;;; this will call chunk-update immediately thereafter.
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

;;; This terminates the explicit request for management.
;;; If 'propagate' is false, then the chunk will remain managed unless
;;; none of its derivees are managed.
;;; If 'propagate' is :ask, then the user will be asked if its
;;; derivees should become unmanaged.
;;; Any other value will cause any derivees to become unmanaged.
(defun chunk-terminate-mgt (c propagate)
   (cond ((Chunk-manage-request c)
	  (setf (Chunk-manage-request c) false)
	  (labels ((chunk-gather-derivees (ch dvl)
		      (cond ((and (Chunk-managed ch)
				  (not (member ch dvl)))
			     (on-list ch dvl)
			     (dolist (d (Chunk-derivees ch))
				(setf dvl
				      (chunk-gather-derivees d dvl)))
			     dvl)
			    (t dvl))))
	     (cond (propagate
		    (let ((all-derivees
			     (chunk-gather-derivees c !())))
		       (cond ((or (not (eq propagate ':ask))
				  (< (length all-derivees) 2)
				  (yes-or-no-p
				     !"Should I really stop managing (keeping ~
                                       up to date) all of the following chunks?~
                                       --~%  ~s?"
				     all-derivees))
			      (dolist (d all-derivees)
				 (setf (Chunk-manage-request d) false)
				 (setf (Chunk-managed d) false))))
		       all-derivees))
		   (t
		    (setf (Chunk-managed c)
		          (some #'Chunk-managed
				(Chunk-derivees c)))))))
	 (t false)))

(defvar update-no* 0)

(defun chunk-update (ch)
   (chunks-update (list ch)))

(defun chunks-update (chunks)
   ;; We have two mechanisms for keeping track of updates in progress.
   ;; The 'in-progress' stack is used to detect a situation where a
   ;; chunk feeds its own input, which would cause an infinite recursion
   ;; if undetected (and may indicate an impossible update goal).  
   ;; The mark mechanism is used to avoid processing a chunk which has
   ;; already been processed. during this call to 'chunk-update'.  This
   ;; is "merely" for efficiency, but it's not a case of premature
   ;; optimization, because it's very easy for the derivation graph to
   ;; have exponentially many occurrences of a chunk if the graph is
   ;; expanded to a tree (which is what eliminating this optimization
   ;; would amount to).
   ;; The algorithm is hairy because of the need to handle "update bases,"
   ;; the chunks needed to run a chunk's deriver, but not to test whether
   ;; it is up to date.  We first propagate down to leaves ('check-leaves-
   ;; up-to-date'), then back up resetting the 'latest-supporter-date'
   ;; of all the chunks we passed.  If that allows us to detect that a chunk
   ;; is out of date, we must go down and up again through its update-basis.
   ;; The process stops when no further updates can be found.
   ;; Now we call 'derivees-update' to call 'derive' on all out-of-date 
   ;; chunks that can be reached from the marked leaves.
   ;; Note: The procedure calls the deriver of a chunk at most once.
   ;; Derivers of leaves are called in 'check-leaves-up-to-date'; other
   ;; derivers are called in 'derivees-update'.
   (let ((down-mark (+ update-no* 1))
	 (up-mark (+ update-no* 2)))
      (setq update-no* (+ update-no* 2))
      (labels ((check-leaves-up-to-date (ch in-progress)
		  ;; Returns list of leaf chunks supporting ch.
		  ;; Also marks all derivees* of those leaves with proper
		  ;; latest-supporter-date.
		  (cond ((= (Chunk-update-mark ch) down-mark)
			 !())
			((member ch in-progress)
			 (error
			     !"Path to leaf chunks from ~s apparently goes ~
                               through itself"))
			(t
			 (let ((in-progress (cons ch in-progress)))
;;;;			    (format t "Setting down-mark of ~s~%" ch)
			    (setf (Chunk-update-mark ch) down-mark)
			    (cond ((null (Chunk-basis ch))
				   ;; Reached a leaf
				   (derive ch)
;;;;				   (format t "Leaf derived: ~s~%" ch)
				   (cons ch
					 (check-from-new-starting-points
					    (set-latest-support-date ch)
					    in-progress)))
				  (t
				   (flet ((recur (b)
					     (check-leaves-up-to-date
						b in-progress)))
				      (nconc
					 (mapcan #'recur
						 (Chunk-basis ch))
					 ;; The call to 'chunk-up-to-date'
					 ;; works because the leaves supporting
					 ;; ch have passed their dates up
					 ;; via 'set-latest-support-date' --
					 (cond ((not (chunk-up-to-date ch))
						(mapcan #'recur
							(Chunk-update-basis
							    ch)))
					       (t !()))))))))))

	       (check-from-new-starting-points (updatees in-progress)
		  ;; Sweep up from leaves may have found new chunks that
		  ;; to be checked.
		  (let ((nsp (mapcan (\\ (ch)
					(check-leaves-up-to-date
					   ch in-progress))
				     (remove-if-not  ; = keep-if
					(\\ (c)
					   (and (not (chunk-up-to-date c))
						(or (Chunk-managed c)
						    (= (Chunk-update-mark c)
						       down-mark))))
					(set-difference updatees
							in-progress)))))
;;;;		     (format t "New starting points from ~s~%   = ~s~%"
;;;;			       updatees nsp)
		     nsp))

	       (derivees-update (ch in-progress)
;;;;		  (format t "Considering ~s~%" ch)
		  (cond ((member ch in-progress)
			 (error
			    !"Cycle in derivation links from ~s"
			    ch))
			((and (not (= (Chunk-update-mark ch) up-mark))
			      ;; Run the deriver when and only when
			      ;; its basis is up to date --
			      (every (\\ (b)
					(or (Chunk-is-leaf b)
					    (chunk-up-to-date b)))
				     (Chunk-basis ch))
			      ;; Ditto for update-basis --
			      (every (\\ (b)
					(or (Chunk-is-leaf b)
					    (chunk-up-to-date b)))
				     (Chunk-update-basis ch)))
			 (let ((in-progress
				     (cons ch in-progress))
			       (down-marked (= (Chunk-update-mark ch)
					       down-mark)))
			    (setf (Chunk-update-mark ch) up-mark)
			    (cond ((and (or down-marked
					    (Chunk-managed ch))
					(not (chunk-up-to-date ch)))
				   (let ((new-date (derive ch)))
;;;;				      (format t "Derived ~s~%" ch)
				      (cond ((and new-date
						  (> new-date
						     (Chunk-date ch)))
;;;;					     (setq ch* ch)
;;;;					     (break
;;;;					        "[A]About to set date of ~s, old = ~s, new = ~s~%"
;;;;						ch (Chunk-date ch) new-date)
					     (setf (Chunk-date ch)
					           new-date)
					     (dolist (d (Chunk-derivees ch))
						(derivees-update d in-progress))
					     (dolist (d (Chunk-update-derivees ch))
					        (derivees-update d in-progress)
					     ))
					    (t
					     (cond ((or (not new-date)
							(< new-date
							   (Chunk-date ch)))
;;;;						    (setq ch* ch)
;;;;						    (break
;;;;						       "[B]About to set date of ~s, old = ~s, new = ~s~%"
;;;;						       ch (Chunk-date ch)
;;;;						       (Chunk-latest-supporter-date
;;;;							   ch))
						    (setf (Chunk-date ch)
							  (Chunk-latest-supporter-date
							     ch))))
;;;;					     (format t "new-date ~s falls to ~s~%"
;;;;						     new-date (Chunk-date ch))
					     ))))
				  (t
;;;;				   (format t "Skipping ~s, because ~a~%"
;;;;					   ch (cond ((or down-marked
;;;;							 (Chunk-managed ch))
;;;;						     "already up to date")
;;;;						    (t
;;;;						     "not managed or marked")))
				  ))))
			(t
;;;;			 (format t "Rejecting ~s, because ~a~%"
;;;;				 ch (cond ((= (Chunk-update-mark ch) up-mark)
;;;;					   "already done")
;;;;					  (t
;;;;					   "bases not ready")))
			))))
	 (cond ((some #'Chunk-managed chunks)
;;;;		(setf update-no* (+ update-no* 1))
;;;;		(format t "update-no* = ~s~%" update-no*)
		(let ((leaves
		         (mapcan (\\ (ch)
				    (cond ((Chunk-managed ch)
					   (check-leaves-up-to-date ch !()))
					  (t !())))
				 chunks)))
;;;;		   (format t "Deriving from ~s~%" leaves)
;;;;		   (break "Ready to rederive")
		   (dolist (leaf leaves)
		      (dolist (d (Chunk-derivees leaf))
			 (derivees-update d !())))))))))

;;; If 'basis' is non-!(), then it's up to date
;;; if and only if its date >= the dates of its bases.
;;; -- If 'basis' is !(), then you have to call 'derive' for every
;;; inquiry, but the deriver makes it up to date.
(defun chunk-up-to-date (ch)
   (let ()
      (cond ((null (Chunk-basis ch))
	     (leaf-chunk-update ch))
	    (t
	     (>= (Chunk-date ch)
		 (Chunk-latest-supporter-date ch))))))

(defun leaf-chunk-update (leaf-ch)
	     (let ((d (derive leaf-ch)))
		(cond (d
		       (cond ((< d (Chunk-date leaf-ch))
			      (cerror "I will ignore the new date"
				      !"Chunk-deriver returned date ~s, ~
					which is before current date ~s"
				      d (Chunk-date leaf-ch))
			      true)
			     (t
;;;;			      (setf lch* leaf-ch)
;;;;			      (format t "[C]Setting date of ~s; old = ~s; new = ~s~%"
;;;;				      leaf-ch (Chunk-date leaf-ch) d)
			      (setf (Chunk-date leaf-ch) d)
			      true)))
		      (t true))))

(defvar chunk-table* (make-hash-table :test #'equal :size 300))

(defgeneric chunk-name->list-structure (name)

  (:method ((x t))
     x)

  (:method ((l cons))
     (mapcar #'chunk-name->list-structure l)))

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
				           (href chunk-table* name-kernel))
				  (cond ((Chunk-managed new-chunk)
					 (chunk-update new-chunk)))
				  new-chunk))
			      (t false)))
		       (t c))))))))

;;; For debugging --
(defun chunk-zap-dates (c)
;; Zap all chunks supporting c, except inputs.
;; Doesn't check whether already zapped, so it's sure of getting
;; them all.
   (cond ((not (null (Chunk-basis c)))
	  (setf (Chunk-date c) 0)
	  (dolist (b (Chunk-basis c))
	     (chunk-zap-dates b))
	  (dolist (u (Chunk-update-basis c))
	     (chunk-zap-dates u)))))
