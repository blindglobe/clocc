;;; Munkres' Assignment Algorithm (AKA "Hungarian Algorithm")
;;; <http://www.public.iastate.edu/~ddoty/HungarianAlgorithm.html>
;;; <http://216.249.163.93/bob.pilgrim/445/munkres.html>
;;; <http://www.math.uwo.ca/~mdawes/courses/344/kuhn-munkres.html>
;;; <http://netlib.bell-labs.com/netlib/toms/548.gz>
;;; Handbook Of Graph Theory, Algorithm 11.3.2, p. 1110
;;;
;;; Copyright (C) 2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: munkres.lisp,v 2.2 2004/06/07 21:39:08 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/munkres.lisp,v $

(defpackage :cllib)
(in-package :cllib)
(export '(assignment))

;;;###autoload
(defun assignment (cost-mx &key (out *standard-output*))
  "Solve the assignment problem using Munkres' algorithm
\(AKA Hungarian Algorithm, AKA Bipartite Minimum-Weight Matching).
Returns the total cost and two assignment vectors: X->Y and Y->X."
  (loop :with x-count = (array-dimension cost-mx 0)
    :and y-count = (array-dimension cost-mx 1)
    :with x-tlv = (make-array x-count) ; tentative least weight
    :and y-tlv = (make-array y-count) ; tentative least weight
    :and x-matching = (make-array x-count)
    :and y-matching = (make-array y-count)
    :and s-x = (make-array x-count :element-type 'bit)
    :and s-y = (make-array y-count :element-type 'bit)
    :and x-next = (make-array x-count)
    :and y-next = (make-array y-count)
    :and cost = 0
    :for iteration-count :upfrom 1
    :do (when out
          (format out "~S(~:D): ~S~%   ~S~%   ~S~%" 'assignment
                  iteration-count cost x-matching y-matching))
    ;; s-x == all free X vertices
    (dotimes (i x-count)
      (if (svref x-matching i)
          (setf (sbit s-x i) 0
                (svref x-tlv i) nil)
          (setf (sbit s-x i) 1
                (svref x-tlv i) 0)))
    (fill y-tlv nil)
    (fill x-next nil) (fill y-next nil)
    (loop :while (find 1 s-x) :do
      (fill s-y 0)
      (dotimes (i x-count)           ; for all X
        (unless (zerop (sbit s-x i)) ; in Sx
          (let ((dx (svref x-tlv i)))
            (dotimes (j y-count)     ; for all Y
              (let ((dy (svref y-tlv j))
                    (ndy (+ dx (aref cost-mx i j))))
                (when (or (null dy) (< ndy dy))
                  (setf (svref y-tlv j) ndy
                        (sbit s-y j) 1
                        (svref y-next j) i)))))))
      (fill s-x 0)
      (dotimes (j y-count)           ; for all Y
        (unless (zerop (sbit s-y j)) ; in Sy
          (let ((i (svref y-matching j)))
            (when i
              (let ((dx (svref x-tlv i))
                    (ndx (- (svref y-tlv j) (aref cost-mx i j))))
                (when (or (null dx) (< ndx dx))
                  (setf (svref x-tlv i) ndx
                        (sbit s-x i) 1
                        (svref x-next i) j))))))))
    ;; free vertex with min TLV
    (let ((min nil) (pos nil) (weight 0) (len 0))
      (dotimes (j y-count)
        (unless (svref y-matching j)
          (let ((dy (svref y-tlv j)))
            (when (or (null min) (< dy min))
              (setq min dy pos j)))))
      (unless min
        (return-from assignment (values cost x-matching y-matching)))
      ;; augment M with the path associated with POS
      (loop :for i = (svref y-next pos) :for j = (svref x-next i)
        :do (setf (svref y-matching pos) i
                  (svref x-matching i) pos)
        (incf weight (aref cost-mx i pos))
        (incf len)
        :unless j :return nil
        :do (setq pos j) (decf weight (aref cost-mx i j)) (incf len))
      (assert (= min weight) (min weight)
              "~S: bug: min label=~S  /=  augmenting path weight=~S"
              'assignment min weight)
      (when out
        (format out "   => augmenting path: len=~:D weight=~S~%"
                len weight))
      (incf cost weight))))

(provide :cllib-munkres)
;;; file munkres.lisp ends here
