;;;
;;; Simple tests for selected LAPACK routines.
;;;
;;; $Id: lapack-tests.lisp,v 1.3 2006/11/26 05:31:16 rtoy Exp $
;;;

;; Convert the eigenvalues returned by DGEEV into an array
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

;; Convert the eigenvalues returned by DGEEV into a more typical
;; matrix form.
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

;; Expected results from http://www.nag.co.uk/lapack-ex/examples/results/dgeev-ex.r
;;
;; DGEEV Example Program Results
;; 
;;  Eigenvalue( 1) =  7.9948E-01
;; 
;;  Eigenvector( 1)
;;  -6.5509E-01
;;  -5.2363E-01
;;   5.3622E-01
;;  -9.5607E-02
;; 
;;  Eigenvalue( 2) = (-9.9412E-02, 4.0079E-01)
;; 
;;  Eigenvector( 2)
;;  (-1.9330E-01, 2.5463E-01)
;;  ( 2.5186E-01,-5.2240E-01)
;;  ( 9.7182E-02,-3.0838E-01)
;;  ( 6.7595E-01, 0.0000E+00)
;; 
;;  Eigenvalue( 3) = (-9.9412E-02,-4.0079E-01)
;; 
;;  Eigenvector( 3)
;;  (-1.9330E-01,-2.5463E-01)
;;  ( 2.5186E-01, 5.2240E-01)
;;  ( 9.7182E-02, 3.0838E-01)
;;  ( 6.7595E-01,-0.0000E+00)
;; 
;;  Eigenvalue( 4) = -1.0066E-01
;; 
;;  Eigenvector( 4)
;;   1.2533E-01
;;   3.3202E-01
;;   5.9384E-01
;;   7.2209E-01
;; 
(defun print-dgeev-results (e-val e-vec)
  (let ((n (length e-val)))
    (dotimes (k n)
      (format t "Eigenvalue(~D) = ~A~%" k (aref e-val k))
      (format t "~%Eigenvector(~D)~%" k)
      (dotimes (row n)
	(format t "~A~%" (aref e-vec row k)))
      (terpri))))

;; DGEEV example based on the example from
;; http://www.nag.co.uk/lapack-ex/node87.html
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
      (declare (ignore z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl z-ldvl z-vr
		       z-ldvr z-work z-lwork))
      ;; Display solution
      (cond ((zerop info)
	     (print-dgeev-results (make-eigval wr wi)
				  (make-eigvec n vr wi)))
	    (t
	     (format t "Failure in DGEEV.  INFO = ~D~%" info)))
      ;; Display workspace info
      (format t "Optimum workspace required = ~D~%" (truncate (aref work 0)))
      (format t "Workspace provided = ~D~%" lwork))))

;; Expected results http://www.nag.co.uk/lapack-ex/examples/results/dgeevx-ex.r
;;
;; DGEEVX Example Program Results
;; 
;;  Eigenvalue( 1) =  7.9948E-01
;; 
;;  Reciprocal condition number =  9.9E-01
;;  Error bound                 =  1.3E-16
;; 
;;  Eigenvector( 1)
;;  -6.5509E-01
;;  -5.2363E-01
;;   5.3622E-01
;;  -9.5607E-02
;; 
;;  Reciprocal condition number =  8.2E-01
;;  Error bound                 =  1.6E-16
;; 
;;  Eigenvalue( 2) = (-9.9412E-02, 4.0079E-01)
;; 
;;  Reciprocal condition number =  7.0E-01
;;  Error bound                 =  1.8E-16
;; 
;;  Eigenvector( 2)
;;  (-1.9330E-01, 2.5463E-01)
;;  ( 2.5186E-01,-5.2240E-01)
;;  ( 9.7182E-02,-3.0838E-01)
;;  ( 6.7595E-01, 0.0000E+00)
;; 
;;  Reciprocal condition number =  4.0E-01
;;  Error bound                 =  3.3E-16
;; 
;;  Eigenvalue( 3) = (-9.9412E-02,-4.0079E-01)
;; 
;;  Reciprocal condition number =  7.0E-01
;;  Error bound                 =  1.8E-16
;; 
;;  Eigenvector( 3)
;;  (-1.9330E-01,-2.5463E-01)
;;  ( 2.5186E-01, 5.2240E-01)
;;  ( 9.7182E-02, 3.0838E-01)
;;  ( 6.7595E-01,-0.0000E+00)
;; 
;;  Reciprocal condition number =  4.0E-01
;;  Error bound                 =  3.3E-16
;; 
;;  Eigenvalue( 4) = -1.0066E-01
;; 
;;  Reciprocal condition number =  5.7E-01
;;  Error bound                 =  2.3E-16
;; 
;;  Eigenvector( 4)
;;   1.2533E-01
;;   3.3202E-01
;;   5.9384E-01
;;   7.2209E-01
;; 
;;  Reciprocal condition number =  3.1E-01
;;  Error bound                 =  4.2E-16
;; 
(defun print-dgeevx-results (tol e-val e-vec rconde rcondv)
  (let ((n (length e-val)))
    (dotimes (k n)
      (format t "Eigenvalue(~D) = ~A~%" k (aref e-val k))
      (let ((rcnd (aref rconde k)))
	(format t "Reciprocal condition number = ~A~%" rcnd)
	(if (plusp rcnd)
	    (format t "Error bound = ~A~%" (/ tol rcnd))
	    (format t "Error bound is infinite~%")))
      
      (format t "~%Eigenvector(~D)~%" k)
      (dotimes (row n)
	(format t "~A~%" (aref e-vec row k)))
      (let ((rcnd (aref rcondv k)))
	(format t "Reciprocal condition number = ~A~%" rcnd)
	(if (plusp rcnd)
	    (format t "Error bound = ~A~%" (/ tol rcnd))
	    (format t "Error bound is infinity~%")))
      (terpri))))

(defun test-dgeevx ()
  (let* ((n 4)
	 (a-mat (make-array (* n n) :element-type 'double-float
			    :initial-contents '(0.35d0 0.09d0 -0.44d0 0.25d0
						0.45d0 0.07d0 -0.33d0 -0.32d0
						-0.14d0 -0.54d0 -0.03d0 -0.13d0
						-0.17d0 0.35d0 0.17d0 0.11d0)))
	 (wr (make-array n :element-type 'double-float))
	 (wi (make-array n :element-type 'double-float))
	 (vl (make-array (* n n) :element-type 'double-float))
	 (vr (make-array (* n n) :element-type 'double-float))
	 (scale (make-array n :element-type 'double-float))
	 (rconde (make-array n :element-type 'double-float))
	 (rcondv (make-array n :element-type 'double-float))
	 (lwork 660)
	 (work (make-array lwork :element-type 'double-float))
	 (iwork (make-array (- (* n 2) 2) :element-type 'f2cl-lib::integer4)))
    (multiple-value-bind (z-balanc z-jobvl z-jobvr z-sense z-n z-a z-lda z-wr z-wi z-vl z-ldvl z-vr
				   z-ldvr ilo ihi z-scale abnrm z-rconde z-rcondv z-work z-lwork z-iwork
				   info)
	(dgeevx "Balance" "Vectors (left)" "Vectors (right)"
		"Both reciprocal condition numbers"
		n a-mat n wr wi vl n vr n 0 0 scale 0d0 rconde rcondv
		work lwork iwork 0)
      (declare (ignore z-balanc z-jobvl z-jobvr z-sense z-n z-a z-lda z-wr z-wi z-vl z-ldvl z-vr
				   z-ldvr z-scale z-rconde z-rcondv z-work z-lwork z-iwork))
      ;; Display solution
      (cond ((zerop info)
	     (let* ((eps (dlamch "Eps"))
		    (tol (* eps abnrm)))
	       (print-dgeevx-results tol
				     (make-eigval wr wi)
				     (make-eigvec n vr wi)
				     rconde rcondv)))
	    (t
	     (format t "Failure in DGEEV.  INFO = ~D~%" info)))
      ;; Display workspace info
      (format t "Optimum workspace required = ~D~%" (truncate (aref work 0)))
      (format t "Workspace provided = ~D~%" lwork))))  
    
;;; $Log: lapack-tests.lisp,v $
;;; Revision 1.3  2006/11/26 05:31:16  rtoy
;;; packages/lapack.system:
;;; o Add DGEEVX and dependencies
;;;
;;; packages/lapack/lapack-tests.lisp:
;;; o Add test for DGEEVX
;;; o Add comments
;;;
;;; packages/lapack/dgeevx.f:
;;; packages/lapack/dlacon.f:
;;; packages/lapack/dlaexc.f:
;;; packages/lapack/dlaqtr.f:
;;; packages/lapack/dlasy2.f:
;;; packages/lapack/dtrexc.f:
;;; packages/lapack/dtrsna.f:
;;; o New files for DGEEVX and dependencies.
;;;
;;; Revision 1.2  2006/11/26 04:53:22  rtoy
;;; Add comments
;;;
;;; Revision 1.1  2006/11/26 04:51:05  rtoy
;;; packages/lapack.system:
;;; o Add defsystem for LAPACK tests
;;;
;;; packages/lapack/lapack-tests.lisp:
;;; o Add simple tests for LAPACK.  (Currently only DGEEV).
;;;