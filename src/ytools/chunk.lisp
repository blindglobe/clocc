;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;; $Id: chunk.lisp,v 1.1.2.14 2004/12/29 20:15:03 airfoyle Exp $

;;; This file depends on nothing but the facilities introduced
;;; in base.lisp and datafun.lisp

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(Chunk Or-chunk Chunk-basis derive print-innards
	     chunk-with-name chunk-destroy
	     chunk-request-mgt chunk-terminate-mgt
	     chunk-up-to-date chunk-update chunks-update)))


;;; The date assigned to chunks with no basis, i.e., leaves.
(defconstant +no-supporters-date+ -1000)

;;; A Chunk represents a piece of information, or a form of a piece of
;;; information, or perhaps merely a piece of information copied to a
;;; particular place.  We can't, and don't need to, describe the
;;; information in the Chunk.  We just have to ensure that anyone who
;;; needs it has a pointer to the Chunk so it can tell whether the
;;; information is up to date and if not recompute it.
(defclass Chunk ()
  ((name :accessor Chunk-name  ; -- An S-expression
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
		 :initform !()
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
   ))
;;; We sort of assume that update-basis is "derivative" in the sense
;;; that if the chunk is up to date wrt the basis, there are no "surprises" 
;;; in the update-basis.  Every chunk in the update basis is a function
;;; of a subset of the basis, so if we updated it it wouldn't change the
;;; the output of 'derive'.

;;; These are used purely temporarily, during chunk construction.
;;; Finding one is an error.
(defclass Transient-chunk (Chunk) ())

;;; Key fact about an Or-chunk is that it supplies no reason for any of its
;;; disjuncts to be managed, except its default.
;;; This class is handled specially by 'chunk-manage', 'chunk-terminate-mgt',
;;; and 'chunks-update'. --
(defclass Or-chunk (Chunk)
   ((disjuncts :accessor Or-chunk-disjuncts
	       :initarg :disjuncts)
    (default :accessor Or-chunk-default
	     :initarg :default
	     :initform false)))

;;; We use the 'update-basis' slot for this now --
;;;;    ;; This is the disjunct from which 'chunk' is currently derived --
;;;;    (active :accessor Or-chunk-active
;;;;	    :initform false)

(defgeneric print-innards (x srm)
  (:method ((x t) srm)
     (declare (ignore srm))
     (values)))

(defmethod print-object ((c Chunk) srm)
   (print-unreadable-object (c srm)
      (format srm "Chunk~a~s"
	      (cond ((eq (Chunk-managed c) ':in-transition)
		     "~")
		    ((Chunk-managed c) "=")
		    (t "_"))
	      (Chunk-name c))
      (print-innards c srm)
      (format srm "~a"
	      (cond ((and (slot-boundp c 'basis)
			  (chunk-is-leaf c))
		     "*")
		    ((and (slot-boundp c 'date)
			  (slot-boundp c 'latest-supporter-date)
			  (chunk-date-up-to-date c))
		     "!")
		    (t "?")))))

(defmethod print-innards :before ((orch Or-chunk) srm)
   (cond ((slot-boundp orch 'disjuncts)
	  (format srm "|~s|" (len (Or-chunk-disjuncts orch))))
	 (t (format srm "<no disjuncts>"))))

(defgeneric derive (chunk))
;;; Recomputes chunk
;;;   and returns time when it changed (usually current universal time)
;;;   or false if it's up to date.
;;;   If it returns t, that's equivalent to returning (get-universal-time).
;;; Important: 'derive' works purely locally, and in particular
;;; never calls 'chunk-update'; someone else is assumed
;;; to have done that.
;;; Subtlety: What I mean is, that it should never alter any chunk
;;; _in the same network_, i.e., one that might be connected to this
;;; one.  It's possible to have one network (N0) of chunks whose purpose
;;; is to manage another network (N1) of chunks.  A 'derive' in N0 
;;; can change the topology of N1, update a chunk in N1, etc.

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
   (cond ((and (null (Chunk-basis ch))
	       (Chunk-managed ch))
	  (leaf-chunk-update ch)))
;;; There is no point in doing this, because 'ch' has no derivees
;;; immediately after being created.   
;;;;   (set-latest-support-date ch)
   )

(defmethod initialize-instance :after ((orch Or-chunk) &rest _)
   (cond ((slot-boundp orch 'disjuncts)
	  (or-chunk-set-update-basis orch)
	  (dolist (b (Or-chunk-disjuncts orch))
	     (pushnew orch (Chunk-derivees b)))))
   orch)

(defmethod (setf Or-chunk-disjuncts) :before (_ (orch Or-chunk))
   (cond ((slot-boundp orch 'disjuncts)
	  (dolist (d (Or-chunk-disjuncts orch))
	  (setf (Chunk-derivees d)
		(remove orch (Chunk-derivees d))))))
   (setf (Chunk-update-basis orch) !())
   orch)

;;; All of this is mainly to make the internals of Or-chunks
;;; look reasonably consistent.  The final decision about
;;; the default and update-basis are actually made in 'chunk-manage'.
(defmethod (setf Or-chunk-disjuncts) :after (_ (orch Or-chunk))
   (dolist (d (Or-chunk-disjuncts orch))
      (setf (Chunk-derivees d)
	    (adjoin orch (Chunk-derivees d))))
   (or-chunk-set-update-basis orch)
   orch)

(defun chunk-is-leaf (ch)
   (null (Chunk-basis ch)))

(defvar basis-inverters-dbg* false)

(defmethod (setf Chunk-basis) :before (new-basis ch)
                                     ;;;;(declare (ignore new-basis))
   (cond (basis-inverters-dbg*
	  (format t "Before setting basis of ~s to ~s~%"
		  ch new-basis)))
   (dolist (b (Chunk-basis ch))
      (setf (Chunk-derivees b)
	    (remove ch (Chunk-derivees b)))))

(defmethod (setf Chunk-basis) :after (new-basis ch)
                                     ;;;;(declare (ignore new-basis))
   (cond (basis-inverters-dbg*
	  (format t "After setting basis of ~s to ~s~%"
		  ch new-basis)))
   (dolist (b (Chunk-basis ch))
      (setf (Chunk-derivees b)
	    (adjoin ch (Chunk-derivees b))))
   (cond ((Chunk-managed ch)
	  (dolist (b (Chunk-basis ch))
	     (chunk-manage b))))
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
;;;;	     (cond ((and (>= (Chunk-date ch) 0)
;;;;			 (>= latest-supporter-date 0)
;;;;			 (>= (Chunk-date ch)
;;;;			     latest-supporter-date))
;;;;		    (setq ch* ch)
;;;;		    (format t !"Chunk ~s up to date in unlikely circumstances ~
;;;;                             ~%  date = ~s latest-supporter-date = ~s~%"
;;;;			   ch (Chunk-date ch) latest-supporter-date)))
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

;;; Returns management state of 'c' (normally true after this runs).
(defun chunk-request-mgt (c)
   (cond ((or (not (Chunk-manage-request c))
	      (not (Chunk-managed c)))
	  (setf (Chunk-manage-request c) true)
	  (chunk-manage c))
	 (t true)))

;;; This doesn't call chunk-update, but presumably everyone who calls
;;; this will call chunk-update immediately thereafter.
(defun chunk-manage (chunk)
   (cond ((Chunk-managed chunk)
	  (cond ((eq (Chunk-managed chunk)
		     ':in-transition)
		 (error
		    !"Chunk basis cycle detected at ~S" chunk 
                     "~%     [-- chunk-manage]")))
	  true)
	 (t
	  (let ((its-an-or (typep chunk 'Or-chunk)))
	     (unwind-protect
		(progn
		   (setf (Chunk-managed chunk) ':in-transition)
		   (dolist (b (Chunk-basis chunk))
		      (chunk-manage b))
		   (cond (its-an-or
			  (or-chunk-set-update-basis chunk)
			  (chunk-manage (first (Chunk-update-basis chunk)))))
		   ;; If 'chunk' is a non-default disjunct
		   ;; of an Or-chunk, then the default disjunct no
		   ;; longer gets a reason to be managed from the Or.
		   (dolist (d (Chunk-derivees chunk))
		      (cond ((and (Chunk-managed d)
				  (typep d 'Or-chunk))
			     (let ((d-disjuncts (Or-chunk-disjuncts d))
				   (d-default (Or-chunk-default d)))
				(cond ((and (not (eq chunk d-default))
					    (memq chunk d-disjuncts)
					    (not (reason-to-manage d-default)))
				       (setf (Chunk-update-basis d)
					     (list chunk))
				       (chunk-unmanage d-default))))))))
	        ;; Normally this just sets (chunk-managed chunk)
	        ;; to true, but not if an interrupt occurred --
		(progn
		    (setf (Chunk-managed chunk)
			  (and (every #'Chunk-managed
				      (Chunk-basis chunk))
			       (or (not its-an-or)
				   (some #'Chunk-managed
					 (Or-chunk-disjuncts chunk)))))
;;;;		    (format t "chunk-managed? ~s : ~s~%"
;;;;			    chunk (Chunk-managed chunk))
;;;;		    (cond ((not (Chunk-managed chunk))
;;;;			   (setq ch* chunk)
;;;;			   (break "Bogosity for ~s" ch*)))
		 ))
	     (Chunk-managed chunk)))))

;;; This terminates the explicit request for management.
;;; If 'propagate' is false, then the chunk will remain managed unless
;;; none of its derivees are managed.
;;; If 'propagate' is :ask, then the user will be asked if its
;;; derivees should become unmanaged.
;;; Any other value will cause any derivees to become unmanaged.
;;; Returns management state of 'c'.
(defun chunk-terminate-mgt (c propagate)
   (setf (Chunk-manage-request c) false)
   (cond ((Chunk-managed c)  ;;;;(Chunk-manage-request c)
	  (labels (;; Returns true if should unmanage derivees --
		   (propagate-to-derivees (all-derivees)
			 (or (null all-derivees)
			     (and propagate
				  (or (not (eq propagate ':ask))
				      (yes-or-no-p
					 !"Should I really stop managing ~
					   (keeping up to date) all of ~
					   the following chunks?~
					   --~%  ~s?"
					 all-derivees)))))
		   (chunk-gather-derivees (ch dvl)
		      (dolist (d (Chunk-derivees ch))
			 (cond ((and (Chunk-managed d)
				     (not (member d dvl))
				     (or (member ch (Chunk-basis d))
					 (cond ((typep d 'Or-chunk)
						(or-dflt-reason-to-manage
						   d ch))
					       (t
						(error "Missing back ptr")))))
				(setq dvl
				      (chunk-gather-derivees
					  d (cons d dvl))))))
		      dvl))
	     (let ((all-derivees
		      (chunk-gather-derivees c !())))
;;;;		(format t "All-derivees = ~s~%" all-derivees)
		(cond ((propagate-to-derivees all-derivees)
		       (cond ((null all-derivees)
			      (chunk-unmanage c))
			     (t
			      (dolist (d all-derivees)
				 (cond ((chunk-has-no-managed-derivees d)
					(chunk-unmanage d))))
			      (cond ((Chunk-managed c)
				     (cerror !"I'll proceed with chunk in ~
                                               unexpected state"
					     !"Chunk ~s still managed after ~
                                               request to terminate ~
                                               management"
					     c)
				     true)
				    (t false)))))
		      (t true)))))
	 (t false)))

(defun chunk-has-no-managed-derivees (ch)
   (let ((found-one false))
      (dolist (d (Chunk-derivees ch))
	 (cond ((Chunk-managed d)
		(cond ((typep d 'Or-chunk)
		       (cond ((or (member ch (Chunk-basis d))
				  (or-dflt-reason-to-manage d ch))
			      (setq found-one true)
			      (return))))
		      (t
		       (setq found-one true)
		       (return))))))
      (not found-one)))

;;; Returns management state of 'c' --
(defun chunk-unmanage (c)
   ;; This is called only by 'chunk-terminate-mgt' and 'chunk-manage',
   ;; and only after verifying that 'c' has no managed derivees --
   (cond ((Chunk-managed c)
	  (cond ((eq (Chunk-managed c)
		     ':in-transition)
		 (error
		    "Chunk basis cycle detected at ~S" c
		    "~%     [-- chunk-unmanage]")))
	  (unwind-protect
	     (progn
		(setf (Chunk-managed c)
		      (reason-to-manage c))
		(cond ((Chunk-managed c)
		       (cerror "I'll proceed with chunk in unexpected state"
			       !"Chunk ~s has unexpected reason to be ~
                                 managed"
			       c)
		       true)
		      (t
		       ;; If c is the last disjunct that supplied
		       ;; an independent way of deriving a managed
		       ;; Or-chunk d, then the default disjunct of
		       ;; d must be managed.
		       (dolist (d (Chunk-derivees c))
			  (cond ((and (Chunk-managed d)
				      (typep d 'Or-chunk))
				 (let ((d-default (Or-chunk-default d)))
				    (cond ((every (\\ (j)
						     (or (eq j d-default)
							 (not (Chunk-managed
							         j))))
						  (Or-chunk-disjuncts d))
					   (setf (Chunk-update-basis d)
						 (list d-default))
					   (chunk-manage d-default)))))))
		       (cond ((typep c 'Or-chunk)
			      ;; If 'c' is an Or-chunk,
			      ;; check if now there is no reason to
			      ;; keep managing its default
			      (setf (Chunk-update-basis c) !())
			      (let ((c-default (Or-chunk-default c)))
				 (cond ((not (reason-to-manage c-default))
					(chunk-unmanage c-default))))))
		       (dolist (b (Chunk-basis c))
			  (cond ((and (Chunk-managed b)
				      (not (reason-to-manage b)))
				 ;; No further reason to manage b
				 (chunk-unmanage b)))))))
	       ;; This is normally redundant, but we need it in the
	       ;; call to 'unwind-protect' to ensure the invariant
	       ;; that if a chunk is managed then its basis is --
	       (setf (Chunk-managed c)
		     (reason-to-manage c))))
	 (t false)))

;;; Returns "the" chunk that gives a reason for 'ch' to be managed, either
;;; 'ch' itself if there's a request to manage it, or an appropriate
;;; derivee.  Returns false if there's no reason for it to be managed. --
(defun reason-to-manage (ch)
   (cond ((Chunk-manage-request ch) ch)
	 (t
	  (dolist (d (Chunk-derivees ch) false)
	     (cond ((Chunk-managed d)
		    (cond ((typep d 'Or-chunk)
			   (cond ((memq ch (Chunk-basis d))
				  (return-from reason-to-manage d))
				 ((or-dflt-reason-to-manage d ch)
				  (return-from reason-to-manage d))))
			  (t
			   (return-from reason-to-manage d)))))))))

;;; 'orch' is an Or-chunk, and is a derivee of 'ch'.  Does 'orch'
;;; supply a reason to manage 'ch' by virtue of 'ch' being the default
;;; for 'orch'?
(defun or-dflt-reason-to-manage (orch ch)
   (and (eq (Or-chunk-default orch)
	    ch)
	(every (\\ (j)
		  (or (eq j ch)
		      (not (Chunk-managed j))))
	       (Or-chunk-disjuncts orch))))

(defvar update-no* 0)

(defvar temp-mgt-dbg* false)

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
   ;; We temporarily make the update-basis chunks managed (using 
   ;; 'unwind-protect' to avoid leaving them managed afterward).
   ;; The process stops when no further updates can be found.
   ;; Now we call 'derivees-update' to call 'derive' on all out-of-date 
   ;; chunks that can be reached from the marked leaves.
   ;; Note: The procedure calls the deriver of a chunk at most once.
   ;; Derivers of leaves are called in 'check-leaves-up-to-date'; other
   ;; derivers are called in 'derivees-update'.
   ;; Note: 'chunk-up-to-date' is called only on non-leaves in
   ;; 'check-from-new-starting-points' and 'derivees-update'.  This is
   ;; valid because all leaves reachable have already been checked,
   ;; and useful because we don't know how expensive a call to a leaf
   ;; deriver is.
   (let ((down-mark (+ update-no* 1))
	 (up-mark (+ update-no* 2))
	 (temporarily-managed !()))
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
			    (cond ((chunk-is-leaf ch)
				   (chunk-derive-and-record ch)
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
						(temporarily-manage
						   (Chunk-update-basis ch))
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

	       (temporarily-manage (update-chunks)
		  (dolist (ud-ch update-chunks)
		     (cond ((not (Chunk-managed ud-ch))
			    (cond (temp-mgt-dbg*
				   (format t "Temporarily managing ~s~%"
					   ud-ch)))
			    (on-list ud-ch temporarily-managed)
			    (chunk-request-mgt ud-ch)))
		    (loaded-c-check)))

	       (derivees-update (ch in-progress)
;;;;		  (format t "Considering ~s~%" ch)
		  (cond ((member ch in-progress)
			 (error
			    !"Cycle in derivation links from ~s"
			    ch))
			((and (Chunk-managed ch)
			      (not (= (Chunk-update-mark ch) up-mark))
			      ;; Run the deriver when and only when
			      ;; its basis is up to date --
			      (every (\\ (b)
					(or (chunk-is-leaf b)
					    (chunk-up-to-date b)))
				     (Chunk-basis ch))
			      ;; Ditto for update-basis --
			      (every (\\ (b)
					(or (chunk-is-leaf b)
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
				   (cond ((chunk-derive-and-record ch)
					  (dolist (d (Chunk-derivees ch))
					     (derivees-update
						d in-progress))
					  (dolist (d (Chunk-update-derivees
							ch))
					     (derivees-update
					        d in-progress)))))
;;;;				  ((not (or down-marked (Chunk-managed ch)))
;;;;				   (format t "Not down marked or managed~%"))
;;;;				  (t
;;;;				   (format t "Already up to date~%"))
			    )))
;;;;			((= (Chunk-update-mark ch) up-mark)
;;;;			 (format t "Already up-marked ~s~%"
;;;;				 up-mark))
;;;;			(t
;;;;			 (dolist (b (Chunk-basis ch))
;;;;			    (cond ((and (not (chunk-is-leaf b))
;;;;					(not (chunk-up-to-date b)))
;;;;				   (format t "Basis ~s not up to date~%"
;;;;					   b))))
;;;;			 (dolist (b (Chunk-update-basis ch))
;;;;			    (cond ((and (not (chunk-is-leaf b))
;;;;					(not (chunk-up-to-date b)))
;;;;				   (format t "U-Basis ~s not up to date~%"
;;;;					   b)))))
		   )))
	 (cond ((some #'Chunk-managed chunks)
		(setq update-no* up-mark)
;;;;		(setf update-no* (+ update-no* 1))
;;;;		(format t "update-no* = ~s~%" update-no*)
		(unwind-protect
		   (let ((leaves
			    (mapcan (\\ (ch)
				       (cond ((Chunk-managed ch)
					      (check-leaves-up-to-date ch !()))
					     (t !())))
				    chunks)))
		      (dolist (leaf leaves)
			 (dolist (d (Chunk-derivees leaf))
			    (derivees-update d !()))))
		  (dolist (ud-ch temporarily-managed)
		     (cond (temp-mgt-dbg*
			    (format t "No longer managing ~s~%" ud-ch)))
		     (chunk-terminate-mgt ud-ch false))))))))

(defun or-chunk-set-update-basis (orch)
   (cond ((null (Or-chunk-disjuncts orch))
	  (error !"Attempting to maintain Or-chunk with ~
                   no disjuncts: ~s"
		 orch)))
   (cond ((or (null (Chunk-update-basis orch))
	      (not (Chunk-managed (first (Chunk-update-basis orch)))))
	  (dolist (b (Or-chunk-disjuncts orch))
	     (cond ((Chunk-managed b)
		    (setf (Chunk-update-basis orch)
			  (list b))
		    (return-from or-chunk-set-update-basis))))
	  (cond ((not (Or-chunk-default orch))
		 (setf (Or-chunk-default orch)
		       (lastelt (Or-chunk-disjuncts orch)))))
	  (setf (Chunk-update-basis orch)
	        (list (Or-chunk-default orch))))))

;;; Run 'derive', update ch's date, and return t if date has moved 
;;; forward -- 
(defmethod derive ((orch Or-chunk))
   (cond ((slot-is-empty orch 'disjuncts)
	  (error "Attempt to derive Or-chunk with no disjuncts: ~s"
		 orch))
	 ((slot-is-empty orch 'default)
	  (cerror "I will set it to the last disjunct"
		  "Deriving Or-chunk with no default disjunct: ~s"
		  orch)
	  (setf (Or-chunk-default orch)
	        (lastelt (Or-chunk-disjuncts orch)))))

   (dolist (d (Or-chunk-disjuncts orch)
	      (Chunk-date (Or-chunk-default orch)))
;;; Alarmist --
;;;;	      (error "Attempt to derive Or-chunk with no managed disjunct: ~s"
;;;;		     orch)
;;;   -- It's perfectly okay to update an unmanaged chunk, and an unmanaged
;;;   Or-chunk can have unmanaged disjuncts.     
      (cond ((and (Chunk-managed d)
		  (or (chunk-is-leaf d)
		      (chunk-up-to-date d)))
	     (return-from derive (Chunk-date d))))))

;;; If 'basis' is non-!(), then it's up to date
;;; if and only if its date >= the dates of its bases.
;;; -- If 'basis' is !(), then you have to call 'derive' for every
;;; inquiry, but the deriver makes it up to date.
(defun chunk-up-to-date (ch)
   (let ()
      (cond ((null (Chunk-basis ch))
	     (leaf-chunk-update ch))
	    (t
	     (chunk-date-up-to-date ch)))))

(defun chunk-date-up-to-date (ch)
   (and (>= (Chunk-date ch)
	    (Chunk-latest-supporter-date ch))
	(>= (Chunk-date ch) 0)))

(defun leaf-chunk-update (leaf-ch)
   (chunk-derive-and-record leaf-ch)
   true)

;;; Call 'derive', update date, and return true if date advanced (else
;;; false if already up to date).
(defun chunk-derive-and-record (ch)
   (let ((new-date (derive ch))
	 (old-date (Chunk-date ch)))
      (cond ((and new-date (not (is-Number new-date)))
	     (setq new-date (get-universal-time))))
      (cond ((or (not new-date)
		 (= new-date old-date)
		 (and (< new-date old-date)
		      (progn 
			 (cerror "I will ignore the new date"
				 !"Chunk-deriver returned date ~s, ~
				   which is before current date ~s"
				   new-date (Chunk-date ch))
			 true)))
	     (setf (Chunk-date ch)
		   (max (Chunk-date ch)
			(Chunk-latest-supporter-date ch)))
	     false)
	    (t
	     (setf (Chunk-date ch) new-date)
	     true))))

(defvar chunk-table* (make-hash-table :test #'equal :size 300))

(defgeneric chunk-name->list-structure (name)

  (:method ((x t))
     x)

  (:method ((l cons))
     (mapcar #'chunk-name->list-structure l)))

;;; IMPORTANT: All chunks should be created using the following function --

;;; Index chunks by first pathname in their names, if any
;;; If 'creator' is non-false, it's a function that creates
;;; a new chunk, which is placed in the table.
(defun chunk-with-name (exp creator &key initializer)
       ;; The kernel is the first atom in a non-car position,
       ;; which is often a pathname, but need not be.
   (labels ((chunk-name-kernel (e)
	       (dolist (x (cdr e) false)
		  (cond ((atom x) (return x))
			(t
			 (let ((k (chunk-name-kernel x)))
			    (cond (k (return k))))))))
	    (create (exp)
	       (let ((new-chunk (funcall creator exp)))
		  (cond ((or (slot-is-empty new-chunk 'name)
			     (not (Chunk-name new-chunk))
			     (cond ((equal (Chunk-name new-chunk)
					   exp)
				    false)
				   (t
				    (cerror "I will change the name"
					    !"Name of new chunk is ~s; ~
					      it should be ~s~%"
					    (Chunk-name new-chunk)
					    exp)
				    true)))
			 (setf (Chunk-name new-chunk)
			       exp)))
		  new-chunk)))
      (let ((name-kernel
	       (let ((list-version
		        (chunk-name->list-structure exp)))
		  (cond ((atom list-version) list-version)
			(t (chunk-name-kernel list-version))))))
	 (let ((bucket (href chunk-table* name-kernel)))
;;;;	    (format t "Looking for ~s in~%  bucket ~s~%"
;;;;		    exp bucket)
;;;;	    (cond ((is-Pathname exp)
;;;;		   (break "Chunk with pathname exp")))
	    (do ((bl bucket (cdr bl))
		 (c nil))
		((or (null bl)
		     (equal (Chunk-name (setq c (car bl)))
			    exp))
		 (cond ((null bl)
			(cond (creator
			       (let ((temp-chunk
				        (make-instance 'Transient-chunk
					   :name exp)))
				  (on-list temp-chunk
					   (href chunk-table* name-kernel))
				  (let ((new-chunk (create exp)))
				     (setf (href chunk-table* name-kernel)
					   (cons new-chunk
						 (delete temp-chunk
							 (href chunk-table*
							       name-kernel))))
				     (cond (initializer
					    (funcall initializer new-chunk)))
				     (cond ((Chunk-managed new-chunk)
					    (chunk-update new-chunk)))
				     new-chunk)))
			      (t false)))
		       ((typep c 'Transient-chunk)
			(error !"Circularity during chunk construction detected at ~
                                 ~s"
			       c))
		       (t c)))
;;;;	      (format t "Skipped ~s~%" c)
	      )))))

(defun chunk-destroy (ch)
   (walk-table
      (\\ (k ch1)
	 (cond ((eq ch1 ch)
		(remhash k chunk-table*))))
      chunk-table*)
  (dolist (b (Chunk-basis ch))
     (setf (Chunk-derivees b) (delete ch (Chunk-derivees b))))
  (dolist (d (Chunk-derivees ch))
     (setf (Chunk-basis d) (delete ch (Chunk-basis d))))
  (dolist (b (Chunk-update-basis ch))
     (setf (Chunk-update-derivees b) (delete ch (Chunk-update-derivees b))))
  (dolist (d (Chunk-update-derivees ch))
     (setf (Chunk-update-basis d) (delete ch (Chunk-update-basis d)))))	  

(defun slot-is-empty (obj slot)
   (or (not (slot-boundp obj slot))
       (null (slot-value obj slot))))

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
