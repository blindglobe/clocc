;;;
;;; Simple tests for selected LAPACK routines
;;;
;;; $Id: lapack-tests.lisp,v 1.1 2006/11/26 04:51:05 rtoy Exp $
;;;

(defun make-eigval (wr wi)
  (let ((e-val (make-array (length wr))))
    (map-into e-val #'(lambda (r i)
		       ;; Do we really want to do this?  Should we
		       ;; just make all of the eigenvalues complex?
		       (if (zerop i)
			   r
			   (complex r i)))
	      wr wi)
    e-val))

(defun make-eigvec (n vr wi)
  (let ((evec (make-array (list n n))))
    (do ((col 0 (incf col))
	 (posn 0))
	((>= col n))
      (cond ((zerop (aref wi col))
	     (dotimes (row n)
	       (setf (aref evec row col) (aref vr posn))
	       (incf posn)))
	    (t
	     (dotimes (row n)
	       (let* ((next-posn (+ posn n))
		      (val+ (complex (aref vr posn) (aref vr next-posn)))
		      (val- (conjugate val+)))
		 (setf (aref evec row col) val+)
		 (setf (aref evec row (1+ col)) val-)
		 (incf posn)))
	     ;; Skip over the next column, which we've already used
	     (incf col)
	     (incf posn n))))
    evec))

(defun print-dgeev-results (e-val e-vec)
  (let ((n (length e-val)))
    (dotimes (k n)
      (format t "Eigenvalue(~D) = ~A~%" k (aref e-val k))
      (format t "~%Eigenvector(~D)~%" k)
      (dotimes (row n)
	(format t "~A~%" (aref e-vec row k)))
      (terpri))))

(defun test-dgeev ()
  ;; The matrix is
  ;;
  ;;  0.35  0.45 -0.14 -0.17
  ;;  0.09  0.07 -0.54  0.35
  ;; -0.44 -0.33 -0.03  0.17
  ;;  0.25 -0.32 -0.13  0.11
  ;;
  ;; Recall that Fortran arrays are column-major order!
  (let* ((n 4)
	 (a-mat (make-array (* n n) :element-type 'double-float
			    :initial-contents '(0.35d0 0.09d0 -0.44d0 0.25d0
						0.45d0 0.07d0 -0.33d0 -0.32d0
						-0.14d0 -0.54d0 -0.03d0 -0.13d0
						-0.17d0 0.35d0 0.17d0 0.11d0)))
	 (wr (make-array n :element-type 'double-float))
	 (wi (make-array n :element-type 'double-float))
	 (vl (make-array 0 :element-type 'double-float))
	 (vr (make-array (* n n) :element-type 'double-float))
	 (lwork 660)
	 (work (make-array lwork :element-type 'double-float)))
    (multiple-value-bind (z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl z-ldvl z-vr
					z-ldvr z-work z-lwork info)
	(dgeev "N" "V" n a-mat n wr wi vl n vr n work lwork 0)
      ;; Display solution
      (cond ((zerop info)
	     (print-dgeev-results (make-eigval wr wi)
				  (make-eigvec n vr wi)))
	    (t
	     (format t "Failure in DGEEV.  INFO = ~D~%" info)))
      ;; Display workspace info
      (format t "Optimum workspace required = ~D~%" (truncate (aref work 0)))
      (format t "Workspace provided = ~D~%" lwork))))

    
;;; $Log: lapack-tests.lisp,v $
;;; Revision 1.1  2006/11/26 04:51:05  rtoy
;;; packages/lapack.system:
;;; o Add defsystem for LAPACK tests
;;;
;;; packages/lapack/lapack-tests.lisp:
;;; o Add simple tests for LAPACK.  (Currently only DGEEV).
;;;