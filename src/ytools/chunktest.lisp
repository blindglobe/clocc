;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

;;; $Id: chunktest.lisp,v 1.1.2.1 2004/11/19 15:32:41 airfoyle Exp $

(defvar datum-a*)
(defvar datum-b*)
(defvar datum-c*)

(defclass Num-reg (Chunk)
   ((cont :accessor Num-reg-contents
	  :initarg :contents
	  :type number)
    (fcn :reader Num-reg-fcn
	 :initarg :fcn)))

(defmethod derive ((r Num-reg))
   (cond ((null (Chunk-basis r))
	  false)
	 (t
	  (let ((new-val 
		   (funcall (Num-reg-fcn r)
		         (mapcar #'Num-reg-contents (Chunk-basis r))
			 (mapcar #'Num-reg-contents (Chunk-update-basis r)))))
	     (cond ((not (= new-val (Num-reg-contents r)))
		    (setf (Num-reg-contents r)
		          new-val)
		    (get-universal-time))
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




