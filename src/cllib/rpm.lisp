;;; File: <rpm.lisp - 1998-12-03 Thu 10:54:34 EST sds@eho.eaglets.com>
;;;
;;; Copyright (C) 1998-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; Copyright (C) 1998 by Sam Steingold
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Id: rpm.lisp,v 1.5 1998/12/03 15:56:33 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Log: rpm.lisp,v $
;;; Revision 1.5  1998/12/03 15:56:33  sds
;;; Added a `note' field to the `rpm' structure.
;;; Added `rpm-get-present', `show-rpms', `rpm-current'.
;;;
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
  (sds-require "base") (sds-require "url") (sds-require "date")
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
  (ecase (read-from-string (machine-instance))
    (mute.eaglets.com
     '(#p"/var/tmp/o/RedHat/RPMS/" #p"/var/tmp/" #p"/mnt/n/var/tmp/"
       #p"/mnt/n/var/tmp/o/RedHat/RPMS/"))
    (eho.eaglets.com
     '(#p"/var/tmp/o/RedHat/RPMS/" #p"/var/tmp/" #p"/mnt/n/var/tmp/"
       #p"/mnt/n/mnt/c/tmp/linux/RedHat/RPMS/")))
  "The local path where to download stuff.")
(defcustom *rpm-command-line* simple-string
  "rpm --queryformat \"%{NAME}-%{VERSION}-%{RELEASE}.%{ARCH} %{BUILDTIME} %{INSTALLTIME} %{SIZE}\\n\""
  "The rpm(1) command line to print all the installed packages.
  "rpm --queryformat \"%{NAME}-%{VERSION}-%{RELEASE}.%{ARCH} %{BUILDTIME} %{INSTALLTIME}\\n\""
(defcustom *rpm-timeout* index-t 600
  "*The default timeout (in seconds) for RPM network operations.")
;;; RPM
(defstruct (rpm)
  (rels "rels?" :type simple-string)
  (arch "arch?" :type simple-string)
  (btime nil :type (or null integer)) ; build time
  (itime nil :type (or null integer)) ; install time


(defmethod print-object ((rpm rpm) (out stream))
  (if *print-readably* (call-next-method)
(defmethod print-object ((rpm rpm) (stream stream))
              (rpm-name rpm) (rpm-vers rpm) (rpm-rels rpm) (rpm-arch rpm)
      (format stream "~a-~a-~a.~a" (rpm-name rpm) (rpm-vers rpm)
              (rpm-rels rpm) (rpm-arch rpm))))
  "Find the position of the RPM in `*rpm-present*'."
(defcustom +unix-epoch+ integer (date2time (mk-date :ye 1970 :mo 1 :da 1))
  "The start of the UNIX epoch - 1970-01-01.")
(defun unix-date (nn)
  "Convert UNIX time_t number to date."
  (declare (type (unsigned-byte 32) nn) (values date))
  (time2date (+ nn +unix-epoch+)))

(fmakunbound 'rpm)
(defgeneric rpm (obj) (:documentation "Convert to RPM."))
  ;; should work with strings like:
(defmethod rpm ((obj symbol)) (unintern obj) (rpm (symbol-name obj)))
(defmethod rpm ((obj rpm)) obj)
(defmethod rpm ((path pathname))
  (let ((rr (rpm (pathname-name path))))
    (setf (rpm-note rr) (list path))
    rr))
(defmethod rpm ((obj string))
  (let* ((line (string-trim +whitespace+ obj))
         (pc (position #\Space line :test #'char=)) (name (subseq line 0 pc))
         (p0 (position #\- name :from-end t)) (len (length name))
         (p1 (position #\- name :from-end t :end (1- p0)))
         (p2 (if (string= ".rpm" name :start2 (- len 4)) (- len 4) len))
         (p3 (or (position #\. name :from-end t :start p0 :end p2) p2)))
    (multiple-value-bind (bd ne) (read-from-string line nil nil :start len)
      (let ((id (read-from-string line nil nil :start ne)))
        (make-rpm :name (subseq name 0 p1) :vers (subseq name (1+ p1) p0)
                  :rels (subseq name (1+ p0) p3) :arch (subseq name (1+ p3) p2)
                  :note (when bd (list (cons (unix-date bd)
                                             (when id (unix-date id))))))))))
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
  (declare (type rpm r0 r1) (values rpm))
  (setf (rpm-note r0) (nconc (rpm-note r0) (rpm-note r1)
                             (when (rpm-itime r1)
  (setf (rpm-note r0) (nconc (rpm-note r0) (rpm-note r1)))
(defun rpm-read (in)
  "Read RPMs from the stream till EOF.  Close the stream."
  (declare (stream in))
  "Read RPMs from the stream."
              (read-line in nil nil)
  (sort (loop :for line :of-type simple-string = (read-line in nil nil)
        #'rpm<))
              :while line :when rr :collect rr)
(defun rpm-installed ()
  "Return the list of RPMs installed."
  (with-open-pipe (st (pipe-input *rpm-command-line* "-qa")) (rpm-read st)))
  "Return the list of RPMs instelled."
  (rpm-read (pipe-input *rpm-command-line* "-qa")))
  "Check for validity of the RPM."
  (declare (type pathname path))
  (with-open-pipe (st (pipe-input *rpm-command-line* "-qp" path))
    (read-line st nil nil)))
  (let ((st (pipe-input *rpm-command-line* "-qp" path)))
    (prog1 (read-line st nil nil) (close st))))
  "Return the list of RPMs already downloaded."
  (sort (mapcar
         #'rpm (delete-if-not
                #'rpm-path-valid-p
                (mapcan (lambda (pp) (directory (merge-pathnames "*.rpm" pp)))
                        (adjoin *rpm-local-target* *rpm-local-paths*
                                :test #'equalp))))
                        *rpm-local-paths*)
                :key #'namestring))
(defun rpm-present ()
  "Return the simple vector of RPMs installed and already downloaded."
  (delete-duplicates
   (merge 'simple-vector (rpm-downloaded) (rpm-installed)
          (lambda (r0 r1)
            (let ((res (rpm< r0 r1)))
              (when (string= (rpm-name r0) (rpm-name r1))
                (if res (rpm-merge-notes r1 r0) (rpm-merge-notes r0 r1)))
              res)))
   :test #'string= :key #'rpm-name))

(defun rpm-available (url &key (out *standard-output*) (err *error-output*)
                      (retry 2))
(defun rpm-available (url &optional (log *standard-output*))
  (handler-case
  (declare (type url url) (type (or null stream) log))
  (catch 'timeout
    (with-open-url (sock url :err log :timeout 600)
      (let ((data (ftp-get-passive-socket sock t)))
        (ftp-ask sock log 150 "nlst")
        (prog1 (map-in (lambda (rr) (setf (rpm-note rr) (list url)) rr)
                       (rpm-read data))
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
(defun rpm-new-packages (&optional force (log *standard-output*))
  "Get the list of all RPMs in `*rpm-urls*' and put it there.
  (when (or force (notany #'dld-all *rpm-locations*)
  (declare (type (or null stream) log))
  (when (or force (notany #'cdr *rpm-urls*))
    (format t " *** Getting the list of new packages...~%")
    (let ((bt (get-float-time nil)) avail (na 0) (le 0))
      (declare (double-float bt) (list avail) (type (unsigned-byte 20) na le))
      (dolist (url *rpm-urls*)
        ;; (declare (type (cons url list) url))
        (format t " *** processing `~a'...~%" (car url))
        (setq avail (rpm-available (car url) log))
        (setf (cdr url) (rpm-prune-list avail) le (length (cdr url)))
        (incf na le)
        (format t " *** ~d new RPMs (out of ~d) [~a]~%" le
                (length avail) (elapsed-1 bt nil)))
      (format t " *** ~d new RPMs in ~r URLs.~%" na (length *rpm-urls*)))))

; (setq sock (open-url (url "ftp://ftp.gnu.org/pub/")))
; (ftp-get-file sock "ls-lR" "/var/tmp/" )
; (ftp-list sock)
; (ftp-ask sock t 257 "pwd")
; (ftp-ask sock t 214 "help ~a" "type")
; (ftp-ask sock t 214 "help")
; (close sock)
; (show-rpms "free")
(defun show-rpms (&optional (what "") (local t))
(defun show-rpms (&optional (what ""))
  (if local (rpm-get-present) (rpm-get-available))
  (let ((nrpm (if local (length *rpm-present*)
  (rpm-get-present)
  (if (zerop (length what))
      (format t "~& *** ~d RPMs~%" (length *rpm-present*))
      (format t "~&Searching ~d RPMs for `~a'~%" (length *rpm-present*) what))
  (loop :for rpm :of-type rpm :across *rpm-present*
        :with ii :of-type (unsigned-byte 20) = 0
        :when (search what (rpm-name rpm) :test #'char-equal)
        :do (format t "~3d. ~a:~{~%~10t~a~}~%" (incf ii) rpm (rpm-note rpm))))
  "Return the rpm corresponding to this name."
  (declare (simple-string name) (values (or null rpm)))
  (with-open-pipe (st (pipe-input *rpm-command-line* "-q" name))
    (let ((ln (read-line st nil nil))) (when ln (rpm ln)))))
  (let* ((st (pipe-input *rpm-command-line* "-q" name))
         (ln (read-line st nil nil)))
    (prog1 (when ln (rpm ln)) (close st))))
  "Print RPMs which are downloaded but not yet installed."
  (loop :for rpm :of-type rpm :across *rpm-present*
        :with ii :of-type index-t = 0
        :unless (rpm-itime rpm)
        :with ii :of-type (unsigned-byte 20) = 0
        :unless (consp (car (rpm-note rpm)))
        :do (format t "~3d. ~a:~{~%~10t~a~}~%" (incf ii) rpm (rpm-note rpm))))
  "Make sure `*rpm-present*' is initialized."
(defun rpm-get-present (&optional force (out *standard-output*))
  (when (or force (zerop (length *rpm-present*)))
  (declare (type (or null stream) out))
      (mesg t out " * Finding the RPMs...")
      (setq *rpm-present* (rpm-present))
      (when out (princ " * Finding the RPMs..." out) (force-output out))
            (length *rpm-present*)
      (when out
        (format out "done [~d; run: ~a; real: ~a]~%" (length *rpm-present*)
                (elapsed-1 bt0 t) (elapsed-1 bt1 nil))))))
(defun rpm-get-list (url rpms &key (out *standard-output*) (err *error-output*)
(defun rpm-get-new-rpms (&optional force (log *standard-output*))
  "Download the RPMs from `*rpm-urls*'."
  (declare (type (or null stream) log))
  (format t "~& *** [~a] `rpm-get-new-rpms' started.~%" (current-time nil))
  (rpm-get-present force log)
  (format t " *** ~d packages present~%" (length *rpm-present*))
  (rpm-new-packages force log)
  (let ((bt (get-float-time nil)) (glob 0))
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
                    (setf (svref *rpm-present* pos)
                          (rpm-merge-notes rpm (svref *rpm-present* pos)))
                    :else :do (format t " *** already have~25t`~a'.~%" rpm)
                    :end)))
          (format t " *** Pruning [~d]..." len) (force-output)
          (setf (cdr url) (rpm-prune-list (cdr url)))
          (format t "done [~d left]~%" (length (cdr url))))))
    (multiple-value-bind (el st) (elapsed bt nil t)
      (format t " *** [~a] done [~:d bytes, ~a, ~:d bytes/sec]~%"
              (current-time nil) glob st (round glob el)))
;;;###autoload
;;; file rpm.lisp ends here
