;;; File: <rpm.lisp - 1998-11-21 Sat 16:00:01 EST sds@eho.eaglets.com>
;;;
;;; Copyright (C) 1998-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; Copyright (C) 1998 by Sam Steingold
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Id: rpm.lisp,v 1.4 1998/11/21 21:00:44 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Log: rpm.lisp,v $
;;; Revision 1.4  1998/11/21 21:00:44  sds
;;; Added `timeout' catch/throw.
;;;
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
  (list (list (make-url :prot "ftp" :host "rawhide.redhat.com"
                        :path "/i386/RedHat/RPMS/"))
        (list (make-url :prot "ftp" :host "updates.redhat.com"
                        :path "/current/i386/"))
        (list (make-url :prot "ftp" :host "updates.redhat.com"
                        :path "/current/noarch/"))
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
(defmethod rpm ((obj symbol)) (unintern obj) (rpm (symbol-name obj)))
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
                    (release< (rpm-rels r0) (rpm-rels r1)))))))
  "Sorting order for versions."
  (declare (simple-string v0 v1))
  (let ((l0 (or (position-if-not #'digit-char-p v0) (length v0)))
        (l1 (or (position-if-not #'digit-char-p v1) (length v1))))
  (let ((l0 (length v0)) (l1 (length v1)))
    (cond ((> l0 l1) (string< v0 (format nil "~v,,,'0@a" l0 v1)))

(defun rpm-merge-notes (r0 r1)
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
                        (delete-if
                         (lambda (pa)
                           (let ((st (pipe-input "rpm" "-qp" pa)))
                             (prog1 (not (read-line st nil nil)) (close st))))
                         (mapcan (lambda (pp)
                                   (directory (merge-pathnames "*.rpm" pp)))
                                 *rpm-local-paths*)
                         :key #'namestring))
                #'rpm<)
          (rpm-read (pipe-input *rpm-command-line*))
          #'rpm<)
(defun rpm-available (url &key (out *standard-output*) (err *error-output*)
                      (retry 2))
(defun rpm-available (url &optional (log *standard-output*))
  (handler-case
  (declare (type url url) (type (or null stream) log))
  (catch 'timeout
    (with-open-url (sock url :err log :timeout 600)
      (let ((data (ftp-get-passive-socket sock t)))
        (ftp-ask sock log 150 "nlst")
        (prog1 (rpm-read data)
          (ftp-ask sock log 226))))))
  "Remove the elements in the list which are better in `*rpm-present*'.
The elements matching `*rpm-skip*' are removed, too."
  "Remove the elements in the list which are better in `*rpm-present*'."
  (mapcan (lambda (rpm)
            (let ((pos (binary-pos rpm *rpm-present* :test #'string<
                                   :key #'rpm-name)))
              (when (and pos (rpm< (svref *rpm-present* pos) rpm))
                (list rpm))))
          rpms))
  "Print RPM in a full form."
(defun rpm-new-packages (&optional (log *standard-output*))
  "Get the list of all RPMs in `*rpm-urls*' and put it there.
  (when (or force (notany #'dld-all *rpm-locations*)
  (declare (type (or null stream) log))
  (let ((bt (get-float-time-real)) avail)
    (declare (double-float bt) (list avail))
    (dolist (url *rpm-urls*)
      ;; (declare (type (cons url list) url))
      (format t " *** processing `~a'...~%" (car url))
      (setq avail (rpm-available (car url) log))
      (setf (cdr url) (rpm-prune-list avail))
      (format t " *** ~d new RPMs (out of ~d) [~a]~%" (length (cdr url))
              (length avail) (elapsed-1 bt nil)))))

; (setq sock (open-url (url "ftp://ftp.gnu.org/pub/")))
; (ftp-get-file sock "ls-lR" "/var/tmp/" )
; (ftp-list sock)
; (ftp-ask sock t 257 "pwd")
; (ftp-ask sock t 214 "help ~a" "type")
; (ftp-ask sock t 214 "help")
; (close sock)
(defun show-rpms (&optional (what "") (local t))
(defun rpm-get-new-rpms (&optional (log *standard-output*))
  "Download the RPMs from `*rpm-urls*'."
  (declare (type (or null stream) log))
  (format t " *** [~a] `rpm-get-new-rpms' started.~%" (current-time nil))
  (when (zerop (length *rpm-present*))
    (setq *rpm-present* (rpm-present)))
  (format t " *** ~d packages present~%" (length *rpm-present*))
  (when (notany #'cdr *rpm-urls*)
    (format t " *** Getting the list of new packages...~%")
    (rpm-new-packages log))
  (let ((bt (get-float-time-real)) (glob 0))
    (declare (double-float bt) (type (unsigned-byte 64) glob))
    (dolist (url *rpm-urls*)
      ;; (declare (type (cons url list) url))
      (when (cdr url)
        (let ((len (length (cdr url))))
          (format t " *** getting ~d file~:p from `~a'~%" len (car url))
          (catch 'timeout
            (with-open-url (sock (car url) :err log :timeout 600)
              (ftp-ask sock log 200 "type i")
              (loop :for rpm :in (cdr url)
                    :and ii :of-type (unsigned-byte 20) :upfrom 1
                    :for pos = (binary-pos rpm *rpm-present* :test #'string<
                                           :key #'rpm-name)
                    :do (format t " *** [~d/~d] getting~25t`~a'...~%"
                                ii len rpm)
                    :if (rpm< (svref *rpm-present* pos) rpm) :do
                    (multiple-value-bind (tot el st)
                        (ftp-get-file sock (format nil "~a.rpm" rpm)
                                      *rpm-local-target* :log log :bin t)
                      (declare (type (unsigned-byte 64) tot) (double-float el)
                               (simple-string st))
                      (incf glob tot)
                      (format t " *** done [~:d bytes, ~a, ~:d bytes/sec]~%"
                              tot st (round tot el)))
                    (format t " *** replacing ~a with ~a in `*rpm-present*'~%"
                            (svref *rpm-present* pos) rpm)
                    (setf (svref *rpm-present* pos) rpm)
                    :else :do (format t " *** already have~25t`~a'.~%" rpm)
                    :end)))
          (format t " *** Pruning [~d]..." len) (force-output)
          (setf (cdr url) (rpm-prune-list (cdr url)))
          (format t "done [~d left]~%" (length (cdr url))))))
    (multiple-value-bind (el st) (elapsed bt nil t)
      (format t " *** [~a] done [~:d bytes, ~a, ~:d bytes/sec]~%" glob st
              (current-time nil) (round glob el)))))
;;; file rpm.lisp ends here
