;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;; $Id: chunk.lisp,v 1.1.2.25 2005/02/28 13:55:52 airfoyle Exp $

;;; This file depends on nothing but the facilities introduced
;;; in base.lisp and datafun.lisp

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(Chunk Or-chunk Form-chunk Chunk-basis derive print-innards
	     chunk-with-name chunk-destroy
	     chunk-request-mgt chunk-terminate-mgt
	     chunk-up-to-date chunk-update chunks-update)))

;;;;; Some of the code in this file is referred to in the following
;;;;; paper:
#| <<<< :eval
    (referring-paper "McDermott05"
                      ~/UNIfied/word/pub/Lisp2005/lisp05.txl 
                      :default true)
   >>>> :eval
|#

;;; The date assigned to chunks with no basis, i.e., leaves.
(defconstant +no-supporters-date+ -1000)

;;; A Chunk represents a piece of information, or a form of a piece of
;;; information, or perhaps merely a piece of information copied to a
;;; particular place.  We can't, and don't need to, describe the
;;; information in the Chunk.  We just have to ensure that anyone who
;;; needs it has a pointer to the Chunk so it can tell whether the
;;; information is up to date and if not recompute it.
;;;;; <<<< Chunk-defn 
(defclass Chunk ()
  ((name :accessor Chunk-name  ; -- An S-expression
	  :initarg :name
	  :initform "")
                    ;;;;; <<<< Mgt-stuff
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
                    ;;;;; >>>> Mgt-stuff
   (height :accessor Chunk-height
	   :type integer)
   ;; -- 0 if not derived from anything, else 1 + max height
   ;; of chunks in 'basis' (but different for Or-chunks; see below).
                    ;;;;; <<<< Update-stuff
   (date :accessor Chunk-date
	 :initform -1
	 :type number)
   ;; -- date when last updated or -1 if never updated
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
                    ;;;;; <<<< Bases-derivees
   (basis :accessor Chunk-basis
	  :initarg :basis
	  :initform !()
	  :type list)
   ;; -- list of Chunks this one is derived from
   (derivees :accessor Chunk-derivees
	     :initform !()
	     :type list)
   ;; -- back pointers to chunks this one is a member of the basis of
                    ;;;;; >>>> Bases-derivees
		    ;;;;; <<<< update-basis
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
		    ;;;;; >>>> update-basis
   ;; -- back pointers to chunks this one is a member of the
   ;; update basis of
   (update-marks :accessor Chunk-update-marks
		 :initform  !())
   ;; -- Marked with various numbers 
   ;; to keep track of progress of chunk-update process.
                    ;;;;; >>>> Update-stuff
   ))
;;;;; >>>> Chunk-defn
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
;;;;; <<<< Or-chunk
(defclass Or-chunk (Chunk)
   ((disjuncts :accessor Or-chunk-disjuncts
	       :initarg :disjuncts)
    (default :accessor Or-chunk-default
	     :initarg :default
	     :initform false)))
;;;;; >>>> Or-chunk

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

;;;;; <<<< derive
(defgeneric derive (chunk))
;;; Recomputes chunk
;;;   and returns time when it changed (usually current universal time)
;;;   or false if it's up to date.
;;;   If it returns t, that's equivalent to returning (get-universal-time).
;;;;; >>>> derive
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
		(+ (chunks-max-height (Chunk-basis ch))
		   1))))
   (dolist (b (Chunk-basis ch))
      (pushnew ch (Chunk-derivees b) :test #'eq))
   (dolist (b (Chunk-update-basis ch))
      (pushnew ch (Chunk-update-derivees b) :test #'eq))
   (cond ((and (null (Chunk-basis ch))
	       (Chunk-managed ch))
	  (leaf-chunk-update ch)))
;;;;; <<<< _
;;; There is no point in doing this, because 'ch' has no derivees
;;; immediately after being created.   
;;;;   (set-latest-support-date ch)
;;;;; >>>> _
   )

(defmethod initialize-instance :after ((orch Or-chunk) &rest _)
   (cond ((slot-truly-filled orch 'disjuncts)
	  (or-chunk-set-default-and-update orch)
	  (dolist (b (Or-chunk-disjuncts orch))
	     (pushnew orch (Chunk-derivees b)))))
   (cond ((slot-truly-filled orch 'default)
	  (or-chunk-modify-height orch (Or-chunk-default orch))))
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
(defmethod (setf Or-chunk-disjuncts) :after (disjuncts (orch Or-chunk))
   (dolist (d disjuncts)
      (setf (Chunk-derivees d)
	    (adjoin orch (Chunk-derivees d))))
   (or-chunk-set-default-and-update orch)
   orch)

(defun or-chunk-set-default-and-update (orch)
	  (cond ((not (slot-truly-filled orch 'default))
		 (setf (Or-chunk-default orch)
		       (lastelt (Or-chunk-disjuncts orch)))))
	  (or-chunk-set-update-basis orch))

(defmethod (setf Or-chunk-default) :after (d (orch Or-chunk))
   (or-chunk-modify-height orch d))

(defun or-chunk-modify-height (orch default)
   (let ((h-from-d (+ (Chunk-height default) 1)))
      (cond ((> h-from-d (Chunk-height orch))
	     (setf (Chunk-height orch) h-from-d)))))

(defun chunk-is-leaf (ch)
   (null (Chunk-basis ch)))

(defvar basis-inverters-dbg* false)

;;; Don't ever, ever modify the basis destructively.  Always setf the 
;;; whole basis, so that these demons run --

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

;;; The number of the next chunk event (management request or update) .
(defvar chunk-event-num* 1)

;;; A list recording events that are finished.  Format: First element
;;; is smallest number that is greater than all finished events.
;;; Remaining elements, if any, are finished events with numbers greater
;;; than that, in ascending order.
;;; Corollary: (rest active-chunk-events*) is either empty or starts with a
;;; number > (+ (first active-chunk-events*) 1).  
(defvar active-chunk-events* (list 1))

(defun chunk-event-discard (evnum)
;;;;   (format *error-output*
;;;;      "Before discarding ~s, active-chunk-events* = ~s~%"
;;;;     evnum active-chunk-events*)
;;;;   (cond ((= evnum 5)
;;;;	  (break "Discarding event 5")))
   (cond ((< evnum (first active-chunk-events*))
	  (error "Discarding ~s when chunk-event-num* = ~s"
		 evnum chunk-event-num*))
	 ((= evnum (first active-chunk-events*))
	  ;; Opportunity to condense the list of done events
	  (let ((evnum evnum))
	     (loop
		;; In this loop, 'evnum' = event number for smallest
		;; event known to be >= all discarded events except
		;; those on 'active-chunk-events*'.
		(setq active-chunk-events* (rest active-chunk-events*))
		;; At this point, 'active-chunk-events*' is the list of numbers
		;; of all discarded events > 'evnum'.
	        ;; 'evnum' is the number that occurred just before
	        ;; those numbers.
	        (setq evnum (+ evnum 1))
	        ;; Now 'evnum' is > all discarded events except those
	        ;; on 'active-chunk-events*'.
		(cond ((and (not (null active-chunk-events*))
			    (= (first active-chunk-events*)
			       evnum))
		       (setq evnum (first active-chunk-events*)))
		      (t
		       (on-list evnum active-chunk-events*)
		       ;; At this point the global invariant (above the 'defvar'
		       ;; for active-chunk-events*) is restored.
		       (return))))))
	 (t
	  ;; If not discarding the first, just put it in the proper
	  ;; order on (rest active-chunk-events*)
	  (let ((done active-chunk-events*))
	     (loop
		(cond ((null (tail done))
		       (on-list evnum (tail done)))
		      ((= (first (tail done))
			  evnum)
		       (return))
		      ((> (first (tail done))
			  evnum)
		       (on-list evnum (tail done))
		       (return))
		      (t
		       (setq done (tail done))))))))
;;;;  (format *error-output*
;;;;     "After discarding ~s, active-chunk-events* = ~s~%"
;;;;     evnum active-chunk-events*)
  )

(defun chunk-mark (ch evnum)
   (on-list evnum (Chunk-update-marks ch)))

(defun chunk-is-marked (ch evnum)
   (do ((marks (Chunk-update-marks ch) (rest marks))
	(prev false marks))
       ((or (null marks)
	    (= (first marks) evnum))
	(not (null marks)))
      (cond ((< (first marks) (first active-chunk-events*))
	     (cond (prev
		    (setf (rest prev) (rest marks)))
		   (t
		    (setf (Chunk-update-marks ch)
			  (rest marks))))))))

;;;;; <<<< chunk-requesters
;;; Returns management state of 'c' (normally true after this runs).
(defun chunk-request-mgt (c)
   (cond ((or (not (Chunk-manage-request c))
	      (not (Chunk-managed c)))
	  (let ((evnum chunk-event-num*))
	     (unwind-protect
		(progn
		   (setq chunk-event-num* (+ evnum 1))
		   (chunk-internal-req-mgt c))
	        (chunk-event-discard evnum))))
	 (t true)))

(defun chunk-internal-req-mgt (c)
	  (setf (Chunk-manage-request c) true)
	  (chunk-manage c))

;;; This terminates the explicit request for management.
;;; If 'propagate' is false, then the chunk will remain managed unless
;;; none of its derivees are managed.
;;; If 'propagate' is :ask, then the user will be asked if its
;;; derivees should become unmanaged.
;;; Any other value will cause any derivees to become unmanaged.
;;; Returns management state of 'c'.
(defun chunk-terminate-mgt (c propagate)
   (let ((evnum chunk-event-num*))
      (unwind-protect
	 (progn
	    (setq chunk-event-num* (+ evnum 1))
	    (chunk-internal-term-mgt c propagate))
	 (chunk-event-discard evnum))))

(defun chunk-internal-term-mgt (c propagate)
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
;;;;; >>>> chunk-requesters

;;; This doesn't call chunk-update, but presumably everyone who calls
;;; this will call chunk-update immediately thereafter.
;;;;; <<<< chunk-managers
;;; Returns management state of 'chunk' --
(defun chunk-manage (chunk)
   (cond ((Chunk-managed chunk)
	  ;;;;; <<<< _ 
	  ;; Omitted because in the paper we prove the check unnecessary!
	  (cond ((eq (Chunk-managed chunk)
		     ':in-transition)
		 (error
		    !"Chunk basis cycle detected at ~S" chunk 
                     "~%     [-- chunk-manage]")))
	  ;;;;; >>>> _
	  true)
	 (t
	  (let ((its-an-or (typep chunk 'Or-chunk)))
	     (unwind-protect
		(progn
		   ;;;;; <<<< temporarily-in-transition
		   (setf (Chunk-managed chunk) ':in-transition)
		   ;;;;; >>>> temporarily-in-transition
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

;;; Returns management state of 'c' --
(defun chunk-unmanage (c)
   ;; This is called only by 'chunk-terminate-mgt' and 'chunk-manage',
   ;; and only after verifying that 'c' has no managed derivees --
   (cond ((Chunk-managed c)
	  ;;;;; <<<< cycle-check
	  (cond ((eq (Chunk-managed c)
		     ':in-transition)
		 (error
		    "Chunk basis cycle detected at ~S" c
		    "~%     [-- chunk-unmanage]")))
	  ;;;;; >>>> cycle-check
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
;;;; >>>> chunk-managers

;;; Returns "the" chunk that gives a reason for 'ch' to be managed, either
;;; 'ch' itself if there's a request to manage it, or an appropriate
;;; derivee.  Returns false if there's no reason for it to be managed. --
;;;;; <<<< reason-to-manage
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
;;;;; >>>> reason-to-manage

;;;;(defvar update-no* 0)

(defvar temp-mgt-dbg* false)
(defvar bad-bases*)
(defvar bad-ch*)

;;; Debugging instrumentation in 'chunks-update-now' (see below);
;;; normally commented out.
(defmacro report-reason-to-skip-chunk ()
  '(format t " ...Skipping because ~s~%"
	   (cond ((not (Chunk-managed ch))
		  "not managed")
		 ((chunk-is-marked ch up-mark)
		  "marked")
		 ((chunk-date-up-to-date ch)
		  "up to date")
		 ((not
		      (every (\\ (b)
				(or (chunk-is-leaf b)
				    (chunk-up-to-date b)))
			     (Chunk-basis ch)))
		  (setq bad-bases*
		     (remove-if
			 (\\ (b)
			     (or (chunk-is-leaf b)
				 (chunk-up-to-date b)))
			 (Chunk-basis ch)))
		  (setq bad-ch* ch)
		  (format nil
		     "Out-of-date bases ~%~s"
		     bad-bases*)
;;;;		  (break "bases not up to date: ~s for chunk ~s"
;;;;			 bad-bases* bad-ch*)
		  )
		 ((not
		     (every (\\ (b)
			       (or (chunk-is-leaf b)
				   (chunk-up-to-date b)))
			    (Chunk-update-basis ch)))
		  (setq bad-bases*
		     (remove-if
			 (\\ (b)
			     (or (chunk-is-leaf b)
				 (chunk-up-to-date b)))
			 (Chunk-update-basis ch)))
		  (format nil "Out-of-date update bases~%~s"
		              bad-bases*))
		 (t "of a mystery"))))

;;;;(defvar postpone-updates* ':do-now)


(defun chunk-update (ch postpone-derivees)
    (chunks-update (list ch) postpone-derivees))
	 
(defvar chunk-update-dbg* false)

;;; 'postpone-derivees' is true if the only chunks to be updated are the
;;; supportive descendents of 'chunks'.
;;; Returns list of postponed derivees, 
;;; which will be () if postpone-derivees=false.
(defun chunks-update (chunks postpone-derivees)
   (let* (derive-mark down-mark up-mark
	  max-here temporarily-managed
	  (postponed !()))
      (labels ((chunks-leaves-up-to-date (chunkl in-progress)
		  (let ((supporting-leaves !()))
		     (dolist (ch chunkl supporting-leaves)
			(let ((sl (check-leaves-up-to-date ch in-progress)))
			   (cond ((update-interrupted)
				  (return !()))
				 (t
				  (setq supporting-leaves
					(nconc sl supporting-leaves))))))))

	       ;; Return all derivees that need to be updated (or at least
	       ;; supporters* of those derivees) --
	       (check-leaves-up-to-date (ch in-progress)
		  ;; Returns list of leaf chunks supporting ch.
		  ;; Also marks all derivees* of those leaves with proper
		  ;; latest-supporter-date.
		  (cond ((chunk-is-marked ch down-mark)
			 !())
			((member ch in-progress)
;;;;			 (setq ch* ch)
;;;;			 (setq inp* in-progress)
			 (error
			     !"Path to leaf chunks from ~s apparently goes ~
                               through itself"
			     ch))
			(t
			 (let ((in-progress (cons ch in-progress)))
			    (cond (chunk-update-dbg*
				   (format *error-output*
					   "Setting down-mark of ~s to ~s~%"
					   ch down-mark)))
			    (chunk-mark ch down-mark)
			    (cond ((chunk-is-leaf ch)
				   (cond ((not (chunk-is-marked
						  ch derive-mark))
					  (cond (chunk-update-dbg*
						 (format *error-output*
						    "Marking [~s] and deriving leaf ~s~%  [Current marks: ~s]~%"
						    derive-mark ch
						    (Chunk-update-marks ch))))
					  (chunk-mark ch derive-mark)
					  (chunk-derive-and-record ch)
					  (cond (chunk-update-dbg*
						 (format *error-output*
						    "Leaf derived: ~s~%"
						    ch)))))
				   (cond ((update-interrupted)
					  !())
					 (t
					  (cons ch
						(check-from-new-starting-points
						   (set-latest-support-date ch)
						   in-progress)))))
				  (t
				   (flet ()
				      (nconc
					 (chunks-leaves-up-to-date
					    (Chunk-basis ch)
					    in-progress)
					 ;; The call to 'chunk-up-to-date'
					 ;; works because the leaves supporting
					 ;; ch have passed their dates up
					 ;; via 'set-latest-support-date' --
					 (cond ((chunk-up-to-date ch) !())
					       (t
						(temporarily-manage
						   (Chunk-update-basis ch))
						(chunks-leaves-up-to-date
						   (Chunk-update-basis ch)
						   in-progress)))))))))))

	       (check-from-new-starting-points (updatees in-progress)
		  ;; Sweep up from leaves may have found new chunks that
		  ;; to be checked.
		  (let ((new-points
			   (retain-if
				    (\\ (c)
				       (and (not (chunk-up-to-date c))
					    (or (Chunk-managed c)
						(chunk-is-marked
						   c down-mark))))
				    (set-difference updatees
						    in-progress))))
		     ;; But if postponing, these are precisely the
		     ;; chunks we want to save for later.
		     (cond (postpone-derivees
			    (cond (chunk-update-dbg*
				   (format *error-output*
				      "Postponing chunks ~s~%"
				      new-points)))
			    (setq postponed
				  (nconc new-points postponed))
			    !())
			   (t
			    ;; Otherwise, start from those chunks as if new-- 
			    (let ((nsp (chunks-leaves-up-to-date
					  new-points !())))
			       (cond (chunk-update-dbg*
				      (format *error-output*
					      "New starting points from ~s~%   = ~s~%"
					      updatees nsp)))
			       nsp)))))

	       (temporarily-manage (update-chunks)
		  (dolist (ud-ch update-chunks)
		     (cond ((not (Chunk-managed ud-ch))
			    (cond (temp-mgt-dbg*
				   (format t "Temporarily managing ~s~%"
					   ud-ch)))
			    (on-list ud-ch temporarily-managed)
			    (chunk-internal-req-mgt ud-ch)))
;;;;		    (loaded-c-check)
		    ))

	       (derivees-update (ch in-progress)
		  (cond (chunk-update-dbg*
			 (format *error-output* "Considering ~s~%" ch)))
		  (cond ((member ch in-progress)
			 (error
			    !"Cycle in derivation links from ~s"
			    ch))
			((and (Chunk-managed ch)
			      (not (chunk-is-marked ch up-mark))
			      (not (chunk-date-up-to-date ch))
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
			 (cond ((and postpone-derivees
				     (not (chunk-is-marked ch down-mark)))
				;; We're about to go to far; put it on the
				;; postponed list.
				(cond (chunk-update-dbg*
				       (format *error-output*
					  "Postponing ~s~%"
					  ch)))
				(on-list ch postponed))
			       (t
				(cond ((not (chunk-is-marked ch derive-mark))
				       (cond (chunk-update-dbg*
					      (format *error-output*
						      " ...Deriving!~%")))
				       (chunk-mark ch derive-mark)
				       (chunk-derive-and-record ch)))
				(cond ((not (update-interrupted))
				       (let ((in-progress
						(cons ch in-progress)))
					  (chunk-mark ch up-mark)
					  (dolist (d (Chunk-derivees ch))
					     (derivees-update d in-progress))
					  (dolist (d (Chunk-update-derivees
							ch))
					     (derivees-update d in-progress))))))))
			(chunk-update-dbg*
			 (report-reason-to-skip-chunk))))

	       (update-interrupted ()
		  (cond ((> chunk-event-num* max-here)
			 (cond (chunk-update-dbg*
				(format *error-output*
				   "Chunk update interrupted~%")))
			 true)
			(t false))))

	 ;; The following comment describes the inner recursions 
	 ;; of 'chunks-update' --

	 ;; We have two mechanisms for keeping track of updates in
	 ;; progress.  The 'in-progress' stack is used to detect a
	 ;; situation where a chunk feeds its own input, which would
	 ;; cause an infinite recursion if undetected (and may
	 ;; indicate an impossible update goal).  The mark mechanism
	 ;; is used to avoid processing a chunk which has already been
	 ;; processed. during this call to 'chunk-update'.  This is
	 ;; "merely" for efficiency, but it's not a case of premature
	 ;; optimization, because it's very easy for the derivation
	 ;; graph to have exponentially many occurrences of a chunk if
	 ;; the graph is expanded to a tree (which is what eliminating
	 ;; this optimization would amount to).  The algorithm is
	 ;; hairy because of the need to handle "update bases," the
	 ;; chunks needed to run a chunk's deriver, but not to test
	 ;; whether it is up to date.  We first propagate down to
	 ;; leaves ('check-leaves- up-to-date'), then back up
	 ;; resetting the 'latest-supporter-date' of all the chunks we
	 ;; passed.  If that allows us to detect that a chunk is out
	 ;; of date, we must go down and up again through its
	 ;; update-basis.  We temporarily make the update-basis chunks
	 ;; managed (using 'unwind-protect' to avoid leaving them
	 ;; managed afterward).  The process stops when no further
	 ;; updates can be found.  Now we call 'derivees-update' to
	 ;; call 'derive' on all out-of-date chunks that can be
	 ;; reached from the marked leaves.  Note: The procedure calls
	 ;; the deriver of a chunk at most once.  Derivers of leaves
	 ;; are called in 'check-leaves-up-to-date'; other derivers
	 ;; are called in 'derivees-update'.  Note: 'chunk-up-to-date'
	 ;; is called only on non-leaves in
	 ;; 'check-from-new-starting-points' and 'derivees-update'.
	 ;; This is valid because all leaves reachable have already
	 ;; been checked, and useful because we don't know how
	 ;; expensive a call to a leaf deriver is.

	 ;; Around the inner recursions is wrapped a restart mechanism
	 ;; to handle the (not uncommon) situation where deriving a chunk
	 ;; has an effect on the chunk network.  We detect this by
	 ;; noting when chunk-event-num* is incremented when we're
	 ;; not looking.  When 'chunks-update' is restarted, we do everything
	 ;; all over again, but 'derive' is _not_ rerun on any chunk it
	 ;; was previously run on.

	 (cond ((some #'Chunk-managed chunks)
		;; Every chunk derived is marked with this mark,
		;; to avoid deriving it twice (even if the update
		;; process is restarted) --
		(setq derive-mark chunk-event-num*)
;;;;		(setq min-here chunk-event-num*)
		(setq max-here (+ chunk-event-num* 1))
		(setq chunk-event-num* max-here)
		(unwind-protect
		   (loop 
		      (setq down-mark chunk-event-num*)
		      (setq up-mark (+ chunk-event-num* 1))
		      ;; If a chunk event occurs while we're updating,
		      ;; we must restart.  We detect that if
		      ;; chunk-event-num* ever exceeds 'max-here' --
		      (setq max-here (+ up-mark 1))
		      (setq chunk-event-num* max-here)
		      (setq temporarily-managed !())
		      (unwind-protect
			 (let ((leaves
				   (chunks-leaves-up-to-date chunks !())))
			     (dolist (leaf leaves)
				(dolist (d (Chunk-derivees leaf))
				   (derivees-update d !()))))
			 (dolist (ud-ch temporarily-managed)
			    (cond (temp-mgt-dbg*
				   (format t "No longer managing ~s~%" ud-ch)))
			    (chunk-internal-term-mgt ud-ch false))
			 (chunk-event-discard down-mark)
			 (chunk-event-discard up-mark))
		      (cond ((not (update-interrupted))
			     (return)))
		      (cond (chunk-update-dbg*
			     (format *error-output*
				"Restarting update of ~s~%"
				chunks))))
;;;;		   (format *error-output*
;;;;		      "Discarding ~s through ~s~%"
;;;;		      min-here (- max-here 1))
		   (chunk-event-discard derive-mark)
;;;;		   (do ((ev min-here (+ ev 1)))
;;;;		       ((= ev max-here))
;;;;		     (chunk-event-discard ev))
		   )))
	 (nodup postponed))))

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
	  (setf (Chunk-update-basis orch)
	        (list (Or-chunk-default orch))))))

;;; Run 'derive', update ch's date, and return t if date has moved 
;;; forward -- 
;;;;; <<<< Or-chunk/derive
(defmethod derive ((orch Or-chunk))
   (cond ((not (slot-truly-filled orch 'disjuncts))
	  (error "Attempt to derive Or-chunk with no disjuncts: ~s"
		 orch))
	 ((not (slot-truly-filled orch 'default))
	  (error "Deriving Or-chunk with no default disjunct: ~s"
		  orch)))
   (let ((date nil))
      (dolist (d (Or-chunk-disjuncts orch)
		(or date
		    (error "No disjunct of or-chunk ~s is managed" orch)))
	 (cond ((and (Chunk-managed d)
		     (or (chunk-is-leaf d)
			 (chunk-up-to-date d)))
		(cond ((or (not date)
			   (> date (Chunk-date d)))
		       (setq date (Chunk-date d)))))))))
;;;;; >>>> Or-chunk/derive

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
   (let ((old-date (Chunk-date ch)))
;;;;     (cond ((and (car-eq (Chunk-name ch) ':loadable)
;;;;		 (> old-date 1000))
;;;;	    (setq l-ch* ch)
;;;;	    (break "Strange date ~s" old-date)
;;;;	    (setq old-date file-op-count*)
;;;;	    (setf (Chunk-date ch) file-op-count*)))
     (cond ((not (chunk-is-leaf ch))
	    ;; Mark it provisionally with a date that will prevent
	    ;; it from being updated by "inner" calls to chunks-update --
	    (setf (Chunk-date ch)
		  (reduce #'max (Chunk-basis ch)
			  :key #'Chunk-date
			  :initial-value (Chunk-latest-supporter-date ch)))))
     (let ((new-date (derive ch)))
	 (cond ((and new-date (not (is-Number new-date)))
		(setq new-date (get-universal-time))))
	 (cond ((or (not new-date)
		    (= new-date old-date)
		    (and (< new-date old-date)
			 (progn 
			    (cerror "I will ignore the new date"
				    !"Chunk-deriver returned date ~s, ~
				      which is before current date ~s"
				      new-date old-date)
			    true)))
		(setf (Chunk-date ch)
		      (max old-date
			   (Chunk-latest-supporter-date ch)))
		false)
	       (t
		(setf (Chunk-date ch) new-date)
		true)))))

(defvar chunk-table* (make-hash-table :test #'equal :size 300))

(defun chunk-system-clear ()
   (chunk-table-clear)
   (setq chunk-event-num* 1)
   (setq active-chunk-events* (list 1)))

(defun chunk-table-clear ()
   (clrhash chunk-table*))

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
   (labels ((chunk-name-kernel (e piecefn)
	       (dolist (x (funcall piecefn e) false)
		  (cond ((atom x) (return x))
			(t
			 (let ((k (chunk-name-kernel x piecefn)))
			    (cond (k (return k))))))))

	    (create (exp)
	       (let ((new-chunk (funcall creator exp)))
		  (cond ((or (not (slot-truly-filled new-chunk 'name))
;;;;			     (not (Chunk-name new-chunk))
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
			(t (or (chunk-name-kernel list-version
						  #'cdr)
			       (chunk-name-kernel list-version
						  #'identity)))))))
	 (let ((bucket (href chunk-table* name-kernel)))
;;;;	    (format t "Looking for ~s in~%  bucket ~s~%"
;;;;		    exp bucket)
;;;;	    (cond ((is-Pathname exp)
;;;;		   (break "Chunk with pathname exp")))
	    (do ((bl bucket (cdr bl))
		 (c false))
		((or (null bl)
		     (equal (Chunk-name (setq c (car bl)))
			    exp))
		 (let ((found-chunk (and (not (null bl))
					 c)))
		    (cond ((and found-chunk
				(not (typep found-chunk 'Transient-chunk)))
			   found-chunk)
			  ((not creator)
			   false)
			  (t
			   ;; 'found-chunk' is either false or transient
			   ;; 'creator' has been supplied
			   (cond (found-chunk
				  (cerror !"I will delete evidence of ~
					    circularity and proceed"
					  !"Circularity during chunk ~
					    construction detected at ~
					    ~s"
					  found-chunk)))
			   (let ((temp-chunk
				    (or found-chunk
					(make-instance 'Transient-chunk
					   :name exp))))
			      (cond ((not found-chunk)
				     (on-list temp-chunk
					      (href chunk-table*
						    name-kernel))))
			      (let ((new-chunk (create exp)))
				 (setf (href chunk-table* name-kernel)
				       (cons new-chunk
					     (delete temp-chunk
						     (href chunk-table*
							   name-kernel))))
				 (cond (initializer
					(funcall initializer new-chunk)))
				 (cond ((Chunk-managed new-chunk)
					(chunk-update new-chunk false)))
				 new-chunk))))))
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

(defun chunks-max-height (chunks)
   (reduce #'max chunks :key #'Chunk-height :initial-value 0))

(defun slot-is-empty (obj slot)
   (not (slot-truly-filled obj slot)))

(defun slot-truly-filled (ob sl)
   (and (slot-boundp ob sl)
	(slot-value ob sl)))

;;; A chunk the derivation of which consists of evaluating 'form' --
(defclass Form-chunk (Chunk)
   ((form :reader Form-chunk-form
	  :initarg :form)))

(defun place-Form-chunk (form)
   (chunk-with-name `(:form ,form)
      (\\ (name)
	 (make-instance 'Form-chunk
	    :name name
	    :form form))))

(defmethod derive ((fc Form-chunk))
   (eval (Form-chunk-form fc))
   true)

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
