;;; File: <rpm.lisp - 1998-11-19 Thu 14:37:00 EST sds@eho.eaglets.com>
;;;
;;; Copyright (C) 1998-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; Copyright (C) 1998 by Sam Steingold
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Id: rpm.lisp,v 1.3 1998/11/19 19:37:41 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Log: rpm.lisp,v $
;;; Revision 1.3  1998/11/19 19:37:41  sds
;;; Replaced `*rpm-local-path*' with a list - `*rpm-local-paths*'.
;;;
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
(defcustom *rpm-local-paths* list
  '(#p"/var/tmp/o/RedHat/RPMS/" #p"/var/tmp/")
  "The local path where to download stuff.")
(defcustom *rpm-command-line* simple-string
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

(defmethod print-object ((rpm rpm) (out stream))
  (if *print-readably* (call-next-method)
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
              :for rr :of-type rpm = (ignore-errors (rpm line))
              :while line :when rr :collect rr)
(defun rpm-installed ()
  "Return the list of RPMs installed."
  (delete-duplicates
  "Return the list of RPMs installed and already downloaded."
          (lambda (r0 r1)
   (merge 'simple-vector
          (sort (mapcar #'rpm
                        (mapcan (lambda (pp)
                                  (directory (merge-pathnames "*.rpm" pp)))
                                *rpm-local-paths*))
                #'rpm<)
          (rpm-read (pipe-input *rpm-command-line*))
          #'rpm<)
(defun rpm-available (url &key (out *standard-output*) (err *error-output*)
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
    (format t " *** processing `~a'...~%" (car url))
    (setf (cdr url)
          (mapcan (lambda (rpm)
                    (let ((pos (binary-pos rpm *rpm-present* :test #'string<
                                           :key #'rpm-name)))
                      (when (and pos (rpm< (svref *rpm-present* pos) rpm))
                        (list rpm))))
                  (rpm-available (car url))))
    (format t " *** ~d new RPMs~%" (length (cdr url)))))
(defun show-rpms (&optional (what "") (local t))
(defun rpm-get-new-rpms ()
  "Download the RPMs from `*rpm-urls*'."
  (when (zerop (length *rpm-present*))
    (setq *rpm-present* (rpm-present)))
  (format t " *** ~d packages present
 *** Getting the list of new packages...~%"
          (length *rpm-present*))
  (when (notany #'cdr *rpm-urls*) (rpm-new-packages))
  (dolist (url *rpm-urls*)
    (when (cdr url)
      (format t " *** getting ~d files from `~a'~%"
              (length (cdr url)) (car url))
      (with-open-url (sock (car url) :err t)
        (dolist (rpm (cdr url))
          (format t " *** getting `~a'...~%" rpm)
          (let ((bt (dfloat (get-internal-real-time)))
                (tot (ftp-get-file sock (format nil "~a.rpm" rpm)
                                   *rpm-local-target* :log t)))
            (multiple-value-bind (el st) (elapsed bt t)
              (format t "~& *** done [~:d bytes, ~a, ~:d bytes/sec]~%" tot st
                      (round tot el)))))))))
;;; file rpm.lisp ends here
