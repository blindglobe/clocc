;;; Matrix Routines
;;;
;;; The main advantage of this file is that is can work with matrices
;;; with elements of arbitrary types (bignums, ratios).
;;; The main disadvantages are performance and limited functionality.
;;;
;;; If you need fast floating point matrix operations,
;;; you should use MatLisp (http://matlisp.sourceforge.net/),
;;; which relies on BLAS (http://www.netlib.org/blas) and
;;; LAPACK (http://www.netlib.org/lapack) for heavy-duty computations.
;;;
;;; Copyright (C) 2000-2003 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: matrix.lisp,v 2.8 2004/12/18 17:55:30 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/matrix.lisp,v $

(in-package :cllib)

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-type', `index-t'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `divf'
  (require :cllib-math (translate-logical-pathname "cllib:math")))

(export '(matrix-print matrix-multiply array-copy array-lin-comb dimension
          matrix-id matrix-id-p matrix-transpose matrix-symmetric-p bilinear
          matrix-solve-lower matrix-solve-upper matrix-solve-lu
          matrix-solve matrix-inverse))
(import '(matrix-print) :cl-user) ; format ~//

;;;
;;; printing
;;;

(defun matrix-print (out aa colp atp &optional fmt-arg)
  "Print a matrix.  Suitable for `format' ~//.
By default prints the contents.
@ means print the rank and dimensions too.
: means print just the rank and the dimensions."
  (declare (stream out) (type array aa))
  (let ((rank (array-rank aa))
        (fmt (case fmt-arg
               ((#\,) (formatter " ~:d"))      ; 123,456,789
               ((#\$) (formatter " ~5,1,10$")) ; Matlab-style
               ((nil) (formatter " ~S"))
               (t fmt-arg))))
    (declare (type index-t rank))
    (when atp
      (format out ";; Matrix: rank: ~d; dimension~:p: ~{~d~^x~}~%"
              rank (array-dimensions aa)))
    (unless colp
      (case rank
        (1 (loop :for elt :across aa :do (format out fmt elt) (terpri out)))
        (2 (dotimes (ii (array-dimension aa 0))
             (dotimes (jj (array-dimension aa 1))
               (format out fmt (aref aa ii jj)))
             (terpri out)))
        (t (error 'case-error :proc 'matrix-print :args
                  (list 'matrix aa 1 2)))))
    aa))

;;;
;;; unity &c
;;;

(define-condition dimension (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "dimension mismatch:~@{ ~{~d~^x~}~}")))

(defun matrix-id (nn)
  "Generate an NxN identity matrix"
  (declare (type index-t nn))
  (loop :with matrix = (make-array (list nn nn) :initial-element 0)
    :for ii :from 0 :below nn :do (setf (aref matrix ii ii) 1)
    :finally (return matrix)))

(defun array-copy (aa &optional bb)
  "Copy an arbitrary array."
  (declare (type array aa))
  (case (array-rank aa)
    (1 (if bb (replace bb aa) (copy-seq aa)))
    (t (let* ((res (or bb (make-array (array-dimensions aa)
                                      :element-type (array-element-type aa))))
              (tot (array-total-size aa))
              (v0 (make-array tot :displaced-to aa))
              (v1 (make-array tot :displaced-to res)))
         (declare (dynamic-extent v0 v1))
         (replace v1 v0)
         res))))

(defun matrix-symmetric-p (mx)
  "Check for the matrix being symmetric."
  (declare (type array mx))
  (let ((dim (array-dimension mx 0)))
    (and (= dim (array-dimension mx 1))
         (loop :for ii :of-type index-t :from 1 :below dim
           :unless (loop :for jj :of-type index-t :from 0 :below ii
                     :unless (= (aref mx ii jj) (aref mx jj ii))
                     :return nil :finally (return t))
           :return nil :finally (return t)))))

(defun matrix-id-p (mx)
  "Check the matrix for being identity."
  (declare (type array mx))
  (let ((dim (array-dimension mx 0)))
    (and (= dim (array-dimension mx 1))
         (loop :for ii :of-type index-t :from 1 :below dim
           :unless (and (loop :for jj :of-type index-t :from 0 :below ii
                          :unless (and (zerop (aref mx ii jj))
                                       (zerop (aref mx jj ii)))
                          :return nil :finally (return t))
                        (= 1 (aref mx ii ii)))
           :return nil :finally (return t)))))

(defun matrix-transpose (mx)
  "Transpose the matrix."
  (declare (type array mx))
  (let ((mxt (make-array (reverse (array-dimensions mx)))))
    (dotimes (ii (array-dimension mx 0) mxt)
      (dotimes (jj (array-dimension mx 1))
        (setf (aref mxt jj ii) (aref mx ii jj))))))

;;;
;;; linear combinations
;;;

(defun array-check-return (arr dims)
  "Make the value to be returned."
  (if arr
      (if (equal (array-dimensions arr) dims) arr
          (error 'dimension :proc 'array-check-return :args
                 (list (array-dimensions arr) dims)))
      (make-array dims :initial-element 0)))

(defun array-lin-comb (l0 a0 l1 a1 &optional res)
  "Linear combination of arbitrary arrays."
  (declare (type array a0 a1) (number l0 l1))
  (let ((dims (array-dimensions a0)))
    (unless (equal dims (array-dimensions a1))
      (error 'dimension :proc 'array-check-return
             :args (list dims (array-dimensions a1))))
    (let* ((ret (array-check-return res dims))
           (tot (array-total-size ret))
           (v0 (make-array tot :displaced-to a0))
           (v1 (make-array tot :displaced-to a1))
           (v2 (make-array tot :displaced-to ret)))
      (declare (dynamic-extent v0 v1 v2))
      (dotimes (ii tot ret)
        (setf (aref v2 ii) (+ (* l0 (aref v0 ii)) (* l1 (aref v1 ii))))))))

;;;
;;; Bilinear form
;;;

(defun bilinear (mx v0 v1)
  "Compute the bilinear form (Ax,y)."
  (declare (type (array * (* *)) mx) (type (array * (*)) v0 v1))
  (unless (and (= (array-dimension v0 0) (array-dimension mx 0))
               (= (array-dimension v0 0) (array-dimension mx 0)))
    (error 'dimension :proc 'bilinear :args
           (list (array-dimensions mx) (array-dimensions v0)
                 (array-dimensions v1))))
  (loop :for ii :of-type index-t :from 0 :below (array-dimension mx 0)
    :sum (* (aref v0 ii)
            (loop :for jj :of-type index-t :from 0 :below (array-dimension mx 1)
              :sum (* (aref mx ii jj) (aref v1 jj))))))

;;;
;;; Matrix Multiplication
;;;

(defun matrix-multiply (aa bb &optional re)
  "Multiply two matrixes.
The optional third argument is the place where to put the return value."
  (flet ((num-arr (num input dim)   ; scalar X any array
           (loop :with res :of-type array = (array-check-return re dim)
             :with tot :of-type index-t = (array-total-size res)
             :with to = (make-array tot :displaced-to res)
             :with fr = (make-array tot :displaced-to input)
             :for ii :of-type index-t :from 0 :below tot
             :do (setf (aref to ii) (* num (aref fr ii)))
             :finally (return res))))
    (cond ((numberp aa) (num-arr aa bb (array-dimensions bb)))
          ((numberp bb) (num-arr bb aa (array-dimensions aa)))
          ((and (= 2 (array-rank aa)) ; matrix X matrix
                (= 2 (array-rank bb)))
           (if (= (array-dimension aa 1)
                  (array-dimension bb 0))
               (loop :with res :of-type array =
                 (array-check-return re (list (array-dimension aa 0)
                                              (array-dimension bb 1)))
                 :for ii :from 0 :below (array-dimension aa 0) :do
                 (loop :for jj :from 0 :below (array-dimension bb 1) :do
                   (loop :for kk :from 0 :below (array-dimension aa 1) :do
                     (incf (aref res ii jj)
                           (* (aref aa ii kk) (aref bb kk jj)))))
                 :finally (return res))
               (error 'dimension :proc 'matrix-multiply :args
                      (list (array-dimensions aa) (array-dimensions bb)))))
          ((and (= 1 (array-rank aa)) ; row X matrix
                (= 2 (array-rank bb)))
           (if (= (array-dimension aa 0)
                  (array-dimension bb 0))
               (loop :with res :of-type array =
                 (array-check-return re (list (array-dimension bb 1)))
                 :for ii :from 0 :below (array-dimension bb 1) :do
                 (loop :for jj :from 0 :below (array-dimension aa 0) :do
                   (incf (aref res ii)
                         (* (aref aa jj) (aref bb jj ii))))
                 :finally (return res))
               (error 'dimension :proc 'matrix-multiply :args
                      (list (array-dimensions aa) (array-dimensions bb)))))
          ((and (= 2 (array-rank aa)) ; matrix X column
                (= 1 (array-rank bb)))
           (if (= (array-dimension aa 1)
                  (array-dimension bb 0))
               (loop :with res :of-type array =
                 (array-check-return re (list (array-dimension aa 0)))
                 :for ii :from 0 :below (array-dimension aa 0) :do
                 (loop :for jj :from 0 :below (array-dimension bb 0) :do
                   (incf (aref res ii) (* (aref aa ii jj) (aref bb jj))))
                 :finally (return res))
               (error 'dimension :proc 'matrix-multiply :args
                      (list (array-dimensions aa) (array-dimensions bb)))))
          ((and (= 1 (array-rank aa)) ; column X row (row X col is dot)
                (= 1 (array-rank bb)))
           (loop :with res :of-type array =
                 (array-check-return
                  re (list (array-dimension aa 0) (array-dimension bb 0)))
                 :for ii :from 0 :below (array-dimension aa 0) :do
                 (loop :for jj :from 0 :below (array-dimension bb 0) :do
                   (setf (aref res ii jj) (* (aref aa ii) (aref bb jj))))
                 :finally (return res)))
          (t (error 'code :proc 'matrix-multiply :args (list aa bb) :mesg
                    "cannot multiply a matrix <~:/matrix-print/> ~
                     by a matrix <~:/matrix-print/>")))))

;;;
;;; solving linear systems
;;;

(defun mx-solve-check (aa bb)
  (unless (and (= 2 (array-rank aa)) (= 1 (array-rank bb))
               (= (array-dimension aa 0) (array-dimension aa 1)
                  (array-dimension bb 0)))
    (error 'dimension :proc 'matrix-solve :args
           (list (array-dimensions aa) (array-dimensions bb)))))

(defun matrix-solve-lower (aa bb &optional diag1)
  "Solve Ax=b, put the result in b.
A is assumed to be a lower-triangular matrix."
  (declare (type (array * (* *)) aa) (type (array * (*)) bb))
  (mx-solve-check aa bb)
  (loop :with nn :of-type index-t = (1- (array-dimension aa 0))
    :for ii :of-type index-t :from 0 :to nn :do
    (unless diag1 (divf (aref bb ii) (aref aa ii ii)))
    (loop :for jj :of-type index-t :from (1+ ii) :to nn :do
      (decf (aref bb jj) (* (aref aa jj ii) (aref bb ii))))
    :finally (return bb)))

(defun matrix-solve-upper (aa bb &optional diag1)
  "Solve Ax=b, put the result in b.
A is assumed to be an upper-triangular matrix."
  (declare (type (array * (* *)) aa) (type (array * (*)) bb))
  (mx-solve-check aa bb)
  (loop :with nn :of-type index-t = (1- (array-dimension aa 0))
    :for ii :of-type index-t :downfrom nn :to 1 :do
    (unless diag1 (divf (aref bb ii) (aref aa ii ii)))
    (loop :for jj :of-type index-t :downfrom (1- ii) :to 1 :do
      (decf (aref bb jj) (* (aref aa jj ii) (aref bb ii))))
    :finally (return bb)))

(defun matrix-lu (mx lu)
  "Decompose the matrix MX into Lower*Upper.
The diagonal elements of the upper-triangular part are 1.
MX and LU can be the same matrix.
See Horn/Johnson 'Matrix Analysis' 3.5.2."
  (declare (type (array * (* *)) mx lu))
  (unless (and (= 2 (array-rank mx) (array-rank lu))
               (equal (array-dimensions mx) (array-dimensions lu))
               (= (array-dimension mx 0) (array-dimension mx 1)))
    (error 'dimension :proc 'matrix-lu :args
           (list (array-dimensions mx) (array-dimensions lu))))
  (let ((nn (1- (array-dimension mx 0))))
    (flet ((lu-sum (ii jj)
             (loop :for kk :of-type index-t :downfrom (1- (min ii jj)) :to 0
               :sum (* (aref lu ii kk) (aref lu kk jj)))))
      (loop :for ii :of-type index-t :from 0 :to nn :do
        (loop :for jj :of-type index-t :from ii :to nn
          :do (setf (aref lu jj ii)
                    (- (aref mx jj ii) (lu-sum jj ii))))
        (loop :for jj :of-type index-t :from (1+ ii) :to nn
          :do (setf (aref lu ii jj)
                    (/ (- (aref mx ii jj) (lu-sum ii jj))
                       (aref lu ii ii))))
        :finally (return lu)))))

(defun matrix-solve-lu (mx bb &optional (lu mx) (xx bb))
  "Solve the linear system Ax=b.
The result is placed in the 4th (optional, default - b) arg.
The 3rd (optional, default - A) arg, is destructively modified,
 it now contains the Lower*Upper decomposition on A.
If one of the principal minors of A is 0, `matrix-solve-lu'
 will signal the `division-by-zero' error."
  (declare (type (array * (* *)) mx) (type (array * (*)) bb))
  (mx-solve-check mx bb)
  (setq lu (array-check-return lu (array-dimensions mx))
        xx (array-check-return xx (array-dimensions bb)))
  (unless (eq xx bb) (replace xx bb))
  (matrix-lu mx lu)
  (matrix-solve-lower lu xx nil)
  (matrix-solve-upper lu xx t))

;;;
;;; matrix inversion
;;;

(defsubst mx-swap-rows (mx nn ii jj)
  (declare (type simple-array mx) (type index-t nn ii jj))
  (dotimes (kk nn)
    (declare (type index-t kk))
    (rotatef (aref mx ii kk) (aref mx jj kk))))

(defsubst mx-swap-cols (mx nn ii jj)
  (declare (type simple-array mx) (type index-t nn ii jj))
  (dotimes (kk nn)
    (declare (type index-t kk))
    (rotatef (aref mx kk ii) (aref mx kk jj))))

(defun matrix-inverse (mx)
  "Invert the matrix in-place.
Return the determinant of the matrix."
  (unless (and (= 2 (array-rank mx))
               (= (array-dimension mx 0) (array-dimension mx 1)))
    (error 'dimension :proc 'matrix-inverse :args
           (list (array-dimensions mx))))
  (labels ((pivot (aa nn kk pivot)
             (declare (type (array * (* *)) aa)
                      (type index-t nn kk) (type number pivot))
             (dotimes (ii nn)   ; divide column by minus pivot
               (declare (type index-t ii))
               (divf (aref aa kk ii) (- pivot)))
             (setf (aref aa kk kk) pivot)
             (dotimes (ii nn)   ; reduce matrix
               (declare (type index-t ii))
               (unless (= ii kk)
                 (loop :with ki = (aref aa kk ii)
                   :for jj :of-type index-t :from 0 :below nn
                   :unless (= jj kk) :do
                   (setf (aref aa jj ii)
                         (+ (* ki (aref aa jj kk)) (aref aa jj ii))))))
             (dotimes (jj nn)   ; divide row by pivot
               (declare (type index-t jj))
               (divf (aref aa jj kk) pivot))
             ;; replace pivot by reciprocal
             (setf (aref aa kk kk) (/ pivot))
             (values))
           (inv (mx nn kk)
             (declare (type (array * (* *)) mx) (type index-t nn kk))
             (if (>= kk nn) 1
                 (let ((ii kk) (jj kk) (max (abs (aref mx kk kk))) pivot)
                   (declare (type index-t ii jj))
                   (loop :for i0 :of-type index-t :from kk :below nn :do
                     (loop :for j0 :of-type index-t :from kk :below nn
                       :when (> (abs (aref mx i0 j0)) max) :do
                       (setq ii i0 jj j0 max (abs (aref mx i0 j0)))))
                   (setq pivot (aref mx ii jj))
                   (when (= pivot 0) (return-from matrix-inverse 0))
                   (mx-swap-rows mx nn ii kk)
                   (mx-swap-cols mx nn jj kk)
                   (pivot mx nn kk pivot)
                   ;; determinant is the recursive product of pivots
                   (prog1 (* pivot (inv mx nn (1+ kk)))
                     (mx-swap-rows mx nn jj kk)
                     (mx-swap-cols mx nn ii kk))))))
    (inv mx (array-dimension mx 0) 0)))

;;;
;;; `matrix-solve' - all in one
;;;

(defun matrix-solve (mx bb &optional (tm mx) (xx bb))
  "Solve the linear system Ax=b.
The optional 3rd and 4th arguments have the same meaning
 as in `matrix-solve-lu'.
If `matrix-solve-lu' fails, the matrix is inverted.  In this case,
 the 3rd argument contains the inverse, and the determinant
 is returned as the second value."
  (declare (type (array * (* *)) mx) (type (array * (*)) bb))
  (mx-solve-check mx bb)
  (setq tm (array-check-return tm (array-dimensions mx))
        xx (array-check-return xx (array-dimensions bb)))
  (handler-case (matrix-solve-lu mx bb tm xx)
    (division-by-zero (co)
      (declare (ignore co))
      (let ((det (matrix-inverse (array-copy mx tm))))
        (when (zerop det)
          (error "the matrix is degenerate (det = 0):~%~/matrix-print/" mx))
        (values (matrix-multiply tm bb xx) det)))))

(provide :cllib-matrix)
;;; matrix.lisp ends here
