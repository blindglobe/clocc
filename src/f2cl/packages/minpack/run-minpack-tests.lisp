(in-package :minpack)

(defun run-minpack-tests ()
  (dolist (f '(tlmdif tlmder))
    (with-open-file (input "clocc:src;f2cl;packages;minpack;lmdif-input.dat"
			   :direction :input)
      (let ((*standard-input* input))
	(funcall f)))))
  
