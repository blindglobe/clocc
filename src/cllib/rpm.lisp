;;; File: <rpm.lisp - 1998-11-19 Thu 12:02:40 EST sds@eho.eaglets.com>
;;;
;;; Copyright (C) 1998-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; Copyright (C) 1998 by Sam Steingold
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Id: rpm.lisp,v 1.2 1998/11/19 17:02:52 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Log: rpm.lisp,v $
;;; Revision 1.2  1998/11/19 17:02:52  sds
;;; Moved `binary-pos' to util.lisp.
;;;
;;; Revision 1.1  1998/11/18 01:01:56  sds
;;; Initial revision
;;;
;;;
          rpm-get-new-rpms rpm-list-rpm rpm-clean-up))
(in-package :cl-user)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))
(eval-when (load compile eval)
  (sds-require "base") (sds-require "url")
;;;
;;; download data
(defcustom *rpm-locations* list
  (macrolet ((mk (&rest args) `(make-download-data :url (make-url ,@args))))
(defcustom *rpm-urls* list
  (list (list (make-url :prot "ftp" :host "ftp.redhat.com"
                        :path "/redhat/redhat-5.2/i386/RedHat/RPMS/"))
        (list (make-url :prot "ftp" :host "ftp.redhat.com"
                        :path "/redhat/updates/current/i386/"))
        (list (make-url :prot "ftp" :host "ftp.redhat.com"
                        :path "/redhat/updates/current/noarch/"))
        (list (make-url :prot "ftp" :host "contrib.redhat.com"
                        :path "/libc6/i386/"))
        (list (make-url :prot "ftp" :host "contrib.redhat.com"
                        :path "/noarch/")))
  "The list of URLs where to look for RPMs.")
(defcustom *rpm-local-path* pathname #p"/var/tmp/o/RedHat/RPMS/"
  "The path to the local RPMs.")
(defcustom *rpm-local-target* pathname #p"/var/tmp"
  "rpm --queryformat \"%{NAME}-%{VERSION}-%{RELEASE}.%{ARCH} %{BUILDTIME} %{INSTALLTIME} %{SIZE}\\n\""
  "The rpm(1) command line to print all the installed packages.
  "rpm -qa --queryformat \"%{NAME}-%{VERSION}-%{RELEASE}.%{ARCH}\\n\""
  "The rpm(1) command line to print all the installed packages.")
;;; RPM
(defstruct (rpm)
  (rels "rels?" :type simple-string)
  (arch "arch?" :type simple-string)
  (btime nil :type (or null integer)) ; build time
  (arch "arch?" :type simple-string))

(defmethod print-object ((rpm rpm) (stream stream))
              (rpm-name rpm) (rpm-vers rpm) (rpm-rels rpm) (rpm-arch rpm)
      (format stream "~a-~a-~a.~a" (rpm-name rpm) (rpm-vers rpm)
              (rpm-rels rpm) (rpm-arch rpm))))
  "Find the position of the RPM in `*rpm-present*'."
(fmakunbound 'rpm)
(defgeneric rpm (obj) (:documentation "Convert to RPM."))
  ;; should work with strings like:
(defmethod rpm ((obj symbol)) (rpm (symbol-name obj)))
(defmethod rpm ((obj rpm)) obj)
(defmethod rpm ((obj pathname)) (rpm (pathname-name obj)))
(defmethod rpm ((obj string))
  (let* ((line (string-trim +whitespace+ obj))
         (p0 (position #\- line :from-end t)) (len (length line))
         (p1 (position #\- line :from-end t :end (1- p0)))
         (p2 (if (string= ".rpm" line :start2 (- len 4)) (- len 4) len))
         (p3 (or (position #\. line :from-end t :start p0 :end p2) p2)))
    (make-rpm :name (subseq line 0 p1) :vers (subseq line (1+ p1) p0)
              :rels (subseq line (1+ p0) p3) :arch (subseq line (1+ p3) p2))))
  "Sorting order on RPMs."
  (declare (type rpm r0 r1))
  (or (string< (rpm-name r0) (rpm-name r1))
      (and (string= (rpm-name r0) (rpm-name r1))
           (or (version< (rpm-vers r0) (rpm-vers r1))
               (and (string= (rpm-vers r0) (rpm-vers r1))
           (or (string< (rpm-vers r0) (rpm-vers r1))
                        (and (string= (rpm-rels r0) (rpm-rels r1))
                    (string< (rpm-rels r0) (rpm-rels r1)))))))
  "Put the notes from r1 into r0."
  (declare (stream in))
  "Read RPMs from the stream."
              (read-line in nil nil)
  (sort (loop :for line :of-type simple-string = (read-line in nil nil)
              :while line :when (< 4 (length line)) :collect (rpm line))
(defun rpm-installed ()
  "Return the list of RPMs installed."
(defun rpm-present (&optional force)
  "Update `*rpm-present*' and return it."
  (setq *rpm-present*
        (if (and (null force) *rpm-present*) *rpm-present*
            (coerce
             (delete-duplicates
              (merge 'list
                     (sort (mapcar #'rpm (directory
                                          (merge-pathnames
                                           "*.rpm" *rpm-local-path*)))
                           #'rpm<)
                     (rpm-read (pipe-input *rpm-command-line*))
                     #'rpm<)
              :test #'string= :key #'rpm-name)
             'simple-vector))))
                      (retry 2))
(defun rpm-available (url)
  (handler-case
  (declare (type url url))
  (with-open-url (sock url :err t)
    (let ((data (ftp-get-passive-socket sock t)))
      (format sock "nlst~%")
      (rpm-read data))))
  "Print RPM in a full form."
(defun rpm-new-packages ()
  "Get the list of all RPMs in `*rpm-urls*' and put it there.
  (when (or force (notany #'dld-all *rpm-locations*)
  (dolist (url *rpm-urls*)
    (format t "Processing `~a'...~%" (car url))
    (setf (cdr url)
          (mapcan (lambda (rpm)
                    (let ((pos (binary-pos rpm *rpm-present* :test #'string<
                                           :key #'rpm-name)))
                      (when (and pos (rpm< (svref *rpm-present* pos) rpm))
                        (list rpm))))
                  (rpm-available (car url))))))
(defun show-rpms (&optional (what "") (local t))
(defun rpm-get-new-rpms ()
  "Download the RPMs from `*rpm-urls*'."
  (dolist (url *rpm-urls*)
    (when (cdr url)
      (with-open-url (sock (car url) :err t)
        (dolist (rpm (cdr url))
          (format t "getting `~a'..." rpm) (force-output)
          (let ((bt (get-float-time))
                (tot (ftp-get-file sock (format nil "~a.rpm" rpm)
                                   *rpm-local-target* :log t)))
            (multiple-value-bind (el st) (elapsed bt t)
              (format t "done [~:d bytes, ~a, ~f bytes/sec]~%" tot st
                      (/ tot el)))))))))
;;; file rpm.lisp ends here
