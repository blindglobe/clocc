(in-package :donlp2)

(defun o8cpu (dum)
  (declare (ignore dum))
  (float (/ (get-internal-run-time)
	    internal-time-units-per-second)
	 1f0))

(defun o8tida (chan)
  ;; Print out time and date to channel chan
  #+cmu
  (let ((stream (f2cl-lib::lun->stream chan)))
    (ext:format-universal-time stream (get-universal-time))
    (terpri stream)))

;; Tell f2cl the function signatures
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(progn
  (setf (gethash 'f2cl::o8cpu f2cl::*f2cl-function-info*)
	(f2cl::make-f2cl-finfo
	 :arg-types '((f2cl::integer4))
	 :return-values '(nil)
	 :calls nil))

  (setf (gethash 'f2cl::o8tida f2cl::*f2cl-function-info*)
	(f2cl::make-f2cl-finfo
	 :arg-types '((f2cl::integer4))
	 :return-values '(nil)
	 :calls nil))

  (setf (gethash 'f2cl::ef f2cl::*f2cl-function-info*)
	(f2cl::make-f2cl-finfo
	 :arg-types '((array double-float (*))
		      (double-float))
	 :return-values '(nil f2cl::fx)
	 :calls nil))
  (setf (gethash 'f2cl::egradf f2cl::*f2cl-function-info*)
	(f2cl::make-f2cl-finfo
	 :arg-types '((array double-float (*))
		      (array double-float (*)))
	 :return-values '(nil nil)
	 :calls nil))
  (setf (gethash 'f2cl::eh f2cl::*f2cl-function-info*)
	(f2cl::make-f2cl-finfo
	 :arg-types '((f2cl::integer4)
		      (array double-float (*))
		      (double-float))
	 :return-values '(nil nil fortran-to-lisp::hxi)
	 :calls nil))
  (setf (gethash 'f2cl::egradh f2cl::*f2cl-function-info*)
	(f2cl::make-f2cl-finfo
	 :arg-types '((f2cl::integer4)
		      (array double-float (*))
		      (array double-float (*)))
	 :return-values '(nil nil nil)
	 :calls nil))
  (setf (gethash 'f2cl::eg f2cl::*f2cl-function-info*)
	(f2cl::make-f2cl-finfo
	 :arg-types '((f2cl::integer4)
		      (array double-float (*))
		      (double-float))
	 :return-values '(nil nil f2cl::gxi)
	 :calls nil))
  (setf (gethash 'f2cl::egradg f2cl::*f2cl-function-info*)
	(f2cl::make-f2cl-finfo
	 :arg-types '((f2cl::integer4)
		      (array double-float (*))
		      (array double-float (*)))
	 :return-values '(nil nil nil)
	 :calls nil))

  )					; end progn
