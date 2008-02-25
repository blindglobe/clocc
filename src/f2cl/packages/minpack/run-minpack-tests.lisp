(in-package :minpack)

;; After running the tests, compare the output files (tlmdif.txt,
;; tlmder.txt) with the reference Fortran results (lmdif-ref.txt,
;; lmder-ref.txt.)
(defun run-minpack-tests ()
  (flet
      ((run-test (input-file f)
	 (with-open-file (input input-file :direction :input)
	   (with-open-file (output (make-pathname :host "minpack"
						  :name (string-downcase (string f))
						  :type "txt")
				   :direction :output
				   :if-exists :supersede)
	     (let ((old-in-lun (gethash 5 f2cl-lib::*lun-hash*))
		   (old-out-lun (gethash 6 f2cl-lib::*lun-hash*)))
	       (unwind-protect
		    (progn
		      (setf (gethash 5 f2cl-lib::*lun-hash*) input)
		      (setf (gethash 6 f2cl-lib::*lun-hash*)
			    (make-broadcast-stream output *standard-output*))
		      (funcall f))
		 (setf (gethash 5 f2cl-lib::*lun-hash*) old-in-lun)
		 (setf (gethash 6 f2cl-lib::*lun-hash*) old-out-lun)))))))
    (dolist (f '(tlmdif tlmder))
      (run-test "minpack:lmdif-input.dat" f))))

(defun run-minpack-test-hybrd ()
  (flet
      ((run-test (input-file f)
	 (with-open-file (input input-file :direction :input)
	   (with-open-file (output (make-pathname :host "minpack"
						  :name (string-downcase (string f))
						  :type "txt")
				   :direction :output
				   :if-exists :supersede)
	     (let ((old-in-lun (gethash 5 f2cl-lib::*lun-hash*))
		   (old-out-lun (gethash 6 f2cl-lib::*lun-hash*)))
	       (unwind-protect
		    (progn
		      (setf (gethash 5 f2cl-lib::*lun-hash*) input)
		      (setf (gethash 6 f2cl-lib::*lun-hash*)
			    (make-broadcast-stream output *standard-output*))
		      (funcall f))
		 (setf (gethash 5 f2cl-lib::*lun-hash*) old-in-lun)
		 (setf (gethash 6 f2cl-lib::*lun-hash*) old-out-lun)))))))
    (dolist (f '(thybrd))
      (run-test "minpack:hybrd-input.dat" f))))
  

  
