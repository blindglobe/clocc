;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

;;; $Id: chunktest.lisp,v 1.1.2.3 2004/12/06 15:09:54 airfoyle Exp $

(defvar datum-a*)
(defvar datum-b*)
(defvar datum-c*)

;; For ease of reading, we don't use universal time, but the number
;; of numerical ops, as our clock.

(defvar num-num-ops* 0)

(defclass Num-reg (Chunk)
   ((cont :accessor Num-reg-contents
	  :initarg :contents
	  :type number)
    (fcn :reader Num-reg-fcn
	 :initarg :fcn)))

(defmethod print-innards ((n Num-reg) srm)
   (format srm "(= ~a)"
	   (cond ((slot-boundp n 'cont)
		  (Num-reg-contents n))
		 (t "??"))))

(defmethod derive ((r Num-reg))
   (cond ((not (slot-boundp r 'fcn))
	  num-num-ops*)
	 (t
	  (let ((new-val 
		   (funcall (Num-reg-fcn r)
			 (mapcar #'Num-reg-contents (Chunk-basis r))
			 (mapcar #'Num-reg-contents (Chunk-update-basis r)))))
	     (setq num-num-ops* (+ num-num-ops* 1))
	     (cond ((or (not (slot-boundp r 'cont))
			(not (= new-val (Num-reg-contents r))))
		    (setf (Num-reg-contents r)
			  new-val)
		    num-num-ops*)
		   (t false))))))

(defparameter
    chunk-a* 
    (make-instance 'Num-reg
	  :name 'a
	  :basis '()))

(defparameter
    chunk-b*
    (make-instance 'Num-reg
	  :name 'b
	  :basis '()))

(defparameter
    chunk-c*
    (make-instance 'Num-reg
	  :name 'c
	  :basis '()))

(defclass K-A-aux (Num-reg)
   ((delta :accessor K-A-aux-s
	   :initarg :delta
	   :type number)))
  
;;; Create a new Num-reg whose value is (v+1)^2, where v is the 
;;; contents of reg.
(defun setup-fcn1 (reg delta)
   (let* ((aux-chunk (make-instance 'K-A-aux
		        :name (build-symbol aux- (:++ symno*))
			:delta delta
			:fcn (\\ (bn _) (+ (first bn) delta ))
			:basis (list reg)))
	  (res-chunk (make-instance 'Num-reg
		       :name (build-symbol res- (:++ symno*))
		       :basis (list reg)
		       :update-basis (list aux-chunk)
		       :fcn (\\ (_ xn) (* (first xn)
					  (first xn))))))
      res-chunk))

(defparameter nr1*
    (setup-fcn1 chunk-a* 1))


;; Numbers (these should all be nonnegative to avoid possibility
;; of dividing by zero below)--    
(defvar ivec*)

;; Chunks that just read those numbers
(defvar input-chunks*)

;; Add 1 to each input but the first.
;; (aref inter-chunks* i) contains the chunk for
;; (+ (ivec (+ i 1)) 1)
(defvar inter-chunks*)

(defvar numer*)

(defvar denom*)

(defvar top-chunk*)

(defun build-test-net (k)
   (setq ivec* (make-array k))
   (do ((i 0 (+ i 1)))
       ((= i k))
      (setf (aref ivec* i) i))
   (setq input-chunks* (make-array k))
   (do ((i 0 (+ i 1)))
       ((= i k))
      (setf (aref input-chunks* i)
	    (make-instance 'Num-reg
	       :name `(input ,i)
	       :basis !()
	       :update-basis !()
	       :fcn (let ((i i))
		      ;; Avoid classic bug (i clobbered by 'do')
		      (\\ (_ _)
;;;;			 (format t "Calculating (aref ivec* ~s)~%"
;;;;				 i)
			 (aref ivec* i))))))
   (setq inter-chunks* (make-array (- k 1)))
   (do ((i 0 (+ i 1)))
       ((= i (- k 1)))
      (setf (aref inter-chunks* i)
	    (make-instance 'Num-reg
	       :name `(inter ,(+ i 1))
	       :basis (list (aref input-chunks* (+ i 1)))
	       :update-basis !()
	       :fcn (\\ (b _) (+ (first b) 1)))))
   (setq numer*
	 (make-instance 'Num-reg
	    :name '"Numer"
	    :basis (concatenate 'list input-chunks*)
	    :update-basis (concatenate 'list inter-chunks*)
	    :fcn (\\ (b u)
		    (do ((bl b (cdr bl))
			 (ul u (cdr ul)) ; one shorter than bl
			 (accum 0)
			 (sign 1 (- sign)))
		        ((null ul)
			 (+ accum (first bl)))
		       (setq accum
			     (+ accum
				(* sign (first bl) (first ul))))))))
   (setq denom*
	 (labels ((build-denom (j)
		     (cond ((= j (- k 2))
			    (aref inter-chunks* j))
			   (t
			    (let ((den (build-denom (+ j 1))))
			       (make-instance 'Num-reg
				  :name (format nil "(i[~a]+1)/~a"
						    (+ j 1) (Chunk-name den))
				  :basis (concatenate 'list (subseq inter-chunks* j))
				  :update-basis (list (aref inter-chunks* j)
						      den)
				  :fcn (\\ (_ ub)
					  (/ (first ub) (second ub)))))))))
	    (build-denom 0)))
   (setq top-chunk*
	 (make-instance 'Num-reg
	     :name "numer/denom"
	     :basis (list numer* denom*)
	     :update-basis !()
	     :fcn (\\ (b _) (/ (first b) (second b))))))

;;; Independent computer of value of top-chunk*
(defun compute ()
   (let ((k (length ivec*)))
      (let ((n (do ((i 0 (+ i 1))
		     (accum 0)
		     (sign 1 (- sign)))
		    ((= i (- k 1))
		     (+ accum (aref ivec* i)))
		  (setq accum
			(+ accum (* sign
				    (aref ivec* i)
				    (+ (aref ivec* (+ i 1))
				       1))))
;;;;		  (format t "i = ~s   accum = ~s~%" i accum)
	       ))
	     (d (do ((i (- k 2) (- i 1))
		     (quo (+ (aref ivec* (- k 1))
			     1)))
		    ((= i 0)
		     quo)
		  (setq quo (/ (+ (aref ivec* i) 1)
			       quo))
;;;;		  (format t "i = ~s   quo = ~s~%" i quo)
		)))
	  (format t "n = ~s   d = ~s~%" n d)
	  (/ n d))))
	   
;;; Build net before starting this --
(defun net-direct-compare (which-input start-val)
   (chunk-request-mgt top-chunk*)
   (do ((i start-val (+ i 1))
	(j which-input (+ j 1))
	(k (length ivec*)))
       ()
      (cond ((>= j k)
	     (setq j 0)))
      (setf (aref ivec* j) i)
      (incf num-num-ops*)
      (chunk-up-to-date (aref input-chunks* j))
      (chunk-update top-chunk*)
      (let ((direct (compute))
	    (via-chunks (Num-reg-contents top-chunk*)))
	 (format t "~s: ~s <?> ~s~%"
		   i direct via-chunks)
	 (cond ((not (= direct via-chunks))
		(return))))))

