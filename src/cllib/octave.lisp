;;; File: <octave.lisp - 1998-04-21 Tue 14:27:13 EDT sds@mute.eaglets.com>
;;;
;;; Octave interface
;;;
;;; Copyright (C) 1997 by Sam Steingold.
;;; This is free software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: octave.lisp,v 1.5 1998/04/21 18:27:32 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/octave.lisp,v $
;;; $Log: octave.lisp,v $
;;; Revision 1.5  1998/04/21 18:27:32  sds
;;; Replaced `get-currency' with `find-currency'.
;;;
;;; Revision 1.4  1998/02/12 21:43:09  sds
;;; Switched to `defgeneric' and `require'.
;;;
;;; Revision 1.3  1997/10/08 15:46:16  sds
;;; Moved make-dx-mx here.
;;;
;;; Revision 1.2  1997/09/12 20:58:24  sds
;;; flush-stream is not used anymore.
;;;

(in-package "CL-USER")

(eval-when (load compile eval)
  (sds-require "base") (sds-require "date") (sds-require "currency"))

(defun dot0 (l0 l1 &key (key #'value) key0 key1)
  "Compute the dot-product of the two sequences,
presumed to be of the same size."
  (declare (sequence l0 l1))
  (setq key0 (or key0 key) key1 (or key1 key))
  (reduce #'+ (map 'list (lambda (r0 r1)
			   (* (funcall key0 r0) (funcall key1 r1))) l0 l1)
	  :initial-value 0.0))

(defcustom *octave-program* simple-string
  #+win32 "c:/bin/octave.exe" #+unix "/usr/local/bin/octave"
  "*The octave executable.")

(defun flush-stream (in-str &optional (out-str t))
  "Flush the stream IN-STR, dumping the stuff to the stream OUT-STR."
  (if out-str
      (do ((nn 0 (1+ nn))) ((null (listen in-str)))
	(format out-str "octave -~2d-> ~a~%" nn (read-line in-str)))
      (do () ((null (listen in-str)))
	(read-line in-str))))

(defun solve-lin (mx vec &optional dump)
  "Given a matrix N x N and an N vector, return the solution of the system.
Send the data to Octave, get the answer."
  (declare (array double-float (* *) mx)
	   (array double-float (*) vec))
  (unless (= (array-dimension mx 0) (array-dimension mx 1)
	     (array-dimension vec 0))
    (error "solve-lin: the matrix must be N x N, and vector - N"))
  (multiple-value-bind (oc-io oc-in oc-ou dim ans endstr les)
      (make-pipe-io-stream *octave-program*)
    (setq dim (array-dimension mx 0)
	  ans (make-array dim :element-type 'double-float
			  :initial-element 0.0 :adjustable nil)
	  endstr "ans = 579" les (length endstr))
    (format oc-ou "format long~%page_screen_output = 0~%
output_precision = 20~%AA=[")
    (dotimes (ii dim)
      (dotimes (jj dim)
	(format oc-ou "~f, " (aref mx ii jj)))
      (terpri oc-ou))
    (format oc-ou "]~%BB=[")
    (dotimes (ii dim)
      (format oc-ou "~f, " (aref vec ii)))
    (format oc-ou "]~%XX=BB/AA~%123+456~%")
    (do ((nn 0 (1+ nn)) (str (read-line oc-in) (read-line oc-in)))
	((and (>= (length str) les) (string= endstr str :end2 les)))
      (when dump (format t "octave --~3d--> ~a~%" nn str)))
    (dotimes (ii dim)
      (format oc-ou "XX(~d)~%" (1+ ii))
      (setf (aref ans ii) (read-from-string (read-line oc-in)
					    nil nil :start 5)))
    (close oc-in)
    (close oc-ou)
    (close oc-io)
    ans))

(defcustom *dx-matrix* array (make-array nil)
  "The matrix of the currencies' dot products.")
(defcustom *dx-vector* array (make-array nil)
  "The vector of the currencies' dot products.")

(defun make-dx-mx ()
  "Make the matrix for DX."
  (let (hists (dim 0) (dx-h (currency-hist (find-currency 'dx))))
    (dolist (cr *currencies-table*)
      (unless (zerop (currency-wt cr))
	(push (currency-hist cr) hists)
	(incf dim)))
    (setq *dx-matrix* (make-array (list dim dim) :element-type 'double-float
				  :initial-element 0.0 :adjustable nil))
    (setq *dx-vector* (make-array dim :element-type 'double-float
				  :initial-element 0.0 :adjustable nil))
    (dotimes (ii dim)
      (dotimes (jj ii)
	(setf (aref *dx-matrix* ii jj)
	      (dot (elt hists ii) (elt hists jj) :key #'currency-rec-avg)
	      (aref *dx-matrix* jj ii) (aref *dx-matrix* ii jj)))
      (setf (aref *dx-matrix* ii ii)
	    (dot (elt hists ii) (elt hists ii) :key #'currency-rec-avg)
	    (aref *dx-vector* ii)
	    (dot (elt hists ii) dx-h :key #'currency-rec-avg)))))

(provide "octave")
;;; octave.lisp ends here
