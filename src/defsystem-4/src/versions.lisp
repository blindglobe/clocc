;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; versions.lisp --

(in-package "MK4")

(defstruct (version (:print-function print-version))
  (symbolic-tag 'nil :type symbol :read-only t)
  (numbers '() :type list :read-only t)
  (documentation "" :type string))

(defun version-major-number (v)
  (if (version-numbers v)
      (first (version-numbers v))
      0))

(defun version-minor-number (v)
  (if (rest (version-numbers v))
      (second (version-numbers v))
      0))


(defun print-version (version-structure stream level)
  (if (or (zerop level) (null *print-level*) (< level *print-level*))
      (print-unreadable-object (version-structure stream :type t)
	 (format stream "~@[~A-~]~D~{.~D~}~:[~; ~S~]"
		 (version-symbolic-tag version-structure)
		 (version-major-number version-structure)
		 (rest (version-numbers version-structure))
		 (string/= "" (version-documentation version-structure))
		 (version-documentation version-structure)))
      (prin1 version-structure stream)))		 


(defun version= (v1 v2)
  (declare (type version v1 v2))
  (and (eq (version-symbolic-tag v1) (version-symbolic-tag v2))
       (= (length (version-numbers v1)) (length (version-numbers v2)))
       (every #'= (version-numbers v1) (version-numbers v2))))


;;; Helper function
(defun list< (l1 l2 &optional (previous nil))
  "Returns T if L1 is lexicographically smaller than L2.
L1 and L2 are assumed to be lists of numbers."
  ;; Of course this could be generalized to SEQUENCEs etc. etc.
  (cond ((null l1)
	 (if (null l2)
	     previous
	     t))
	((null l2) previous)
	(t
	 (if (> (first l1) (first l2))
	     nil
	     (list< (rest l1) (rest l2)
		    (or previous (< (first l1) (first l2))))))))


(defun version< (v1 v2)
  (declare (type version v1 v2))
  (and (eq (version-symbolic-tag v1) (version-symbolic-tag v2))
       (list< (version-numbers v1)
	      (version-numbers v2))))
		     
       

(defun version<= (v1 v2)
  (declare (type version v1 v2))
  (or (version< v1 v2)
      (version= v1 v2)))

(defun version> (v1 v2)
  (declare (type version v1 v2))
  (not (version<= v1 v2)))

(defun version>= (v1 v2)
  (not (version< v1 v2)))
       

;;; end of file -- versions.lisp --
