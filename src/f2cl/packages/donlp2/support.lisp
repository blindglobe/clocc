(defun o8cpu (dum)
  (declare (ignore dum))
  (float (/ (get-internal-run-time)
	    internal-time-units-per-second)
	 1f0))

(defun o8tida (chan)
  (declare (ignore chan))
  ;; Print out time and date to channel chan
  (format-universal-time *standard-output* (get-universal-time)))
