;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; registry.lisp --
;;; Registry and PROVIDE/REQUIRE utilities.

(in-package "MK4")

(defparameter *central-registry*
  (make-pathname :directory '(:absolute
			      "usr"
			      "local"
			      "common-lisp"
			      "systems")))

(defvar *system-file-extension* "system")


(defun system-registry-paths (&optional paths-list)
  (append (mapcar #'pathname paths-list)
	  (list *central-registry*)
	  (list (cl.env:cwd))))


(define-condition system-definition-file-not-found (file-error)
  ((name :reader file-not-found-system-name
	 :initarg :system-name)
   (search-paths :reader file-not-found-system-search-paths
		 :initarg :system-search-paths)
   )
  (:report (lambda (cnd stream)
	     (format stream
		     "MK4: system ~S file definition ~S not found in~{~&~S~}."
		     (file-not-found-system-name cnd)
		     (file-error-pathname cnd)
		     (file-not-found-system-search-paths cnd)))))


(defun compute-system-definition-file (system-name
				       &key
				       definition-pathname
				       alternative-locations)
  (declare (type (or symbol string) system-name)
	   (type (or null string pathname) definition-pathname))
  (let* ((system-file-name
	  (etypecase system-name
	    (symbol (string-downcase (symbol-name system-name)))
	    (string system-name)))
	 (system-file-name-pathname
	  (make-pathname :name system-file-name
			 :type *system-file-extension*))
	 )
    (when (and definition-pathname (probe-file definition-pathname))
      (return-from compute-system-definition-file (pathname definition-pathname)))

    (dolist (registry-path (system-registry-paths alternative-locations))
      (let ((system-file-pathname (adjoin-directories registry-path
						      system-file-name-pathname))
	    )
	(when (probe-file system-file-pathname)
	  (return-from compute-system-definition-file system-file-pathname))))

    (error 'system-definition-file-not-found
	   :system-name system-name
	   :system-search-paths (system-registry-paths alternative-locations)
	   :pathname (or definition-pathname system-file-name-pathname))))

;;; end of file -- registry.lisp --
