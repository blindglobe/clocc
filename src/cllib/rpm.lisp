;;; File: <rpm.lisp - 1999-02-09 Tue 17:46:11 EST sds@eho.eaglets.com>
;;;
;;; Copyright (C) 1998-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; Copyright (C) 1998 by Sam Steingold
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Id: rpm.lisp,v 1.8 1999/02/09 23:21:26 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Log: rpm.lisp,v $
;;; Revision 1.8  1999/02/09 23:21:26  sds
;;; Removed `catch timeout'.
;;;
;;; Revision 1.7  1999/01/24 20:30:38  sds
;;; Added `rpm-skip-p', `*rpm-skip*', `rpm-print-rpm-url'.
;;; Use `with-open-pipe'.
;;;
;;; Revision 1.6  1999/01/07 03:56:34  sds
;;; Use `close-pipe', `index-t' instead of (unsigned-byte 20).
;;; Added `version<' and a test suite for it.
;;;
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
  (list (list (make-url :prot :ftp :host "rawhide.redhat.com"
                        :path "/i386/RedHat/RPMS/"))
        (list (make-url :prot :ftp :host "updates.redhat.com"
                        :path "/current/i386/"))
        (list (make-url :prot :ftp :host "updates.redhat.com"
                        :path "/current/noarch/"))
        (list (make-url :prot :ftp :host "contrib.redhat.com"
                        :path "/libc6/i386/"))
        (list (make-url :prot :ftp :host "contrib.redhat.com"
                        :path "/noarch/"))
        (list (make-url :prot :ftp :host "developer.redhat.com"
                        :path "/pub/rhcn/RPMS/i386/"))
        (list (make-url :prot :ftp :host "developer.redhat.com"
                        :path "/pub/rhcn/RPMS/noarch/"))
        (list (make-url :prot :ftp :host "ftp.suse.com" :path ; X server
                        "/pub/suse_update/XFree86-3.3.3.1-SuSE/glibc2/"))
        ;;(make-url :prot :ftp :host "ftp.cc.gatech.edu" :path
        ;;          "/pub/linux/distributions/suse/suse_update/XFree86-3.3.3.1-SuSE/glibc2/")
        (list (make-url :prot :ftp :host "ftp.dosemu.org" ; dosemu
                        :path "/dosemu/")))
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
(defcustom *rpm-max-retry* (or null index-t) 10
  "*The default timeout for RPM network operations.")
;;; RPM
(eval-when (load compile eval)
  (vers "vers?" :type simple-string)
  (rels "rels?" :type simple-string)
  (arch "arch?" :type simple-string)
  (btime nil :type (or null integer)) ; build time
  (itime nil :type (or null integer)) ; install time

(defconst +bad-rpm+ rpm (make-rpm) "*The convenient constant for init.")

(defmethod print-object ((rpm rpm) (out stream))
  (if *print-readably* (call-next-method)
(defmethod print-object ((rpm rpm) (stream stream))
              (rpm-name rpm) (rpm-vers rpm) (rpm-rels rpm) (rpm-arch rpm)
      (format stream "~:[~;\"~]~a-~a-~a.~a~:[~;\"~]" *print-escape*

(defsubst rpm-pos (name)
  "Find the position of the RPM in `*rpm-present*'."
  (declare (type rpm rpm))
  (or (string-beg-with "pre" (rpm-vers rpm))
      (and (search "kernel" (rpm-name rpm) :test #'char=)
  (and (search "kernel" (rpm-name rpm) :test #'char=)
       (notevery #'digit-char-p (rpm-vers rpm))))

(defun short-string-to-rpm (name)
  "Convert the short string like `pack-1.2-3.i386.rpm' to RPM.
(defcustom +unix-epoch+ integer (date2time (mk-date :ye 1970 :mo 1 :da 1))
  "The start of the UNIX epoch - 1970-01-01.")
(defun unix-date (nn)
  "Convert UNIX time_t number to date."
  (declare (type (unsigned-byte 32) nn) (values date))
  (time2date (+ nn +unix-epoch+)))

(fmakunbound 'rpm)
  (:method ((obj rpm)) obj)
  (:method ((obj symbol))
    (unintern obj)
  (:method ((obj symbol)) (unintern obj) (rpm (symbol-name obj)))
      (setf (rpm-note rr) (list path)
           (let ((rr (rpm (pathname-name path))))
             (setf (rpm-note rr) (list path))
             rr)))
  ;; should work with strings like:
(defmethod rpm ((obj string))
  (let* ((line (string-trim +whitespace+ obj))
         (pc (position #\Space line :test #'char=)) (name (subseq line 0 pc))
         (p0 (position #\- name :from-end t)) (len (length name))
         (p1 (position #\- name :from-end t :end (1- p0)))
         (p2 (if (string= ".rpm" name :start2 (- len 4)) (- len 4) len))
         (p3 (or (position #\. name :from-end t :start p0 :end p2) p2)))
    (multiple-value-bind (bd id)
        (values-list (string-tokens line :start len :max 2))
      (make-rpm :name (subseq name 0 p1) :vers (subseq name (1+ p1) p0)
                :rels (subseq name (1+ p0) p3) :arch (subseq name (1+ p3) p2)
                :note (when bd (list (cons (unix-date bd)
                                           (when id (unix-date id)))))))))
  "Sorting order on RPMs."
  (declare (type rpm r0 r1))
  (or (string< (rpm-name r0) (rpm-name r1))
      (and (string= (rpm-name r0) (rpm-name r1))
           (or (version< (rpm-vers r0) (rpm-vers r1))
               (and (string= (rpm-vers r0) (rpm-vers r1))
                    (or (release< (rpm-rels r0) (rpm-rels r1))
                        (and (string= (rpm-rels r0) (rpm-rels r1))
                    (release< (rpm-rels r0) (rpm-rels r1)))))))
  "Sorting order for versions."
  (declare (simple-string v0 v1))
  (do* ((b0 0 (1+ e0)) (b1 0 (1+ e1))
        (e0 (position #\. v0) (position #\. v0 :start b0))
        (e1 (position #\. v1) (position #\. v1 :start b1)))
       ((or (null e0) (null e1)
            (string/= v0 v1 :start1 b0 :start2 b1 :end1 e0 :end2 e1))
        (or (release< (subseq v0 b0 e0) (subseq v1 b1 e1))
            (and (string= v0 v1 :start1 b0 :start2 b1 :end1 e0 :end2 e1)
                 (null e0) e1)))))

(defun release< (v0 v1)
  "Sorting order for releases."
  (declare (simple-string v0 v1))
  (let ((l0 (or (position-if-not #'digit-char-p v0) (length v0)))
        (l1 (or (position-if-not #'digit-char-p v1) (length v1))))
    (declare (type index-t l0 l1))
    (cond ((and (= l0 (length v0)) (< l1 (length v1)) ; v1 - beta
                (char-equal #\b (schar v1 l1)))
           nil)
          ((and (= l1 (length v1)) (< l0 (length v0)) ; v0 - beta
                (char-equal #\b (schar v0 l0)))
           t)
          ((> l0 l1) (string< v0 (format nil "~v,,,'0@a" l0 v1)))
          ((< l0 l1) (string< (format nil "~v,,,'0@a" l1 v0) v1))
          ((string< v0 v1)))))

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
              :for rr :of-type (or null rpm) = (ignore-errors (rpm line))
              :while line :when rr :collect rr :finally (close in))
        #'rpm<))
              :while line :when rr :collect rr)
(defun rpm-installed ()
  "Return the list of RPMs installed."
  (with-open-pipe (st (pipe-input *rpm-command-line* "-qa")) (rpm-read st)))
  "Return the list of RPMs instelled."
(defun rpm-path-valid-p (path)
  "Check for validity of the RPM."
  (declare (type pathname path))
  (with-open-pipe (st (pipe-input *rpm-command-line* "-qp" path))
    (read-line st nil nil)))
  (with-open-pipe (st (pipe-input *rpm-command-line* "-qp" path)) ; "-K" ?
(defun rpm-downloaded ()
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
(defun rpm-available (url &optional (log *standard-output*) (retry 2)
                      (timeout *rpm-timeout*))
  (handler-case
  (declare (type url url) (type (or null stream) log) (type index-t retry))
                           :max-retry *rpm-max-retry*)
      (with-open-url (sock url :err log :timeout timeout)
        (let ((data (ftp-get-passive-socket sock t nil timeout)))
          (url-ask sock log 150 "nlst")
          (prog1 (map-in (lambda (rr) (setf (rpm-note rr) (list url)) rr)
                         (rpm-read data))
            (url-ask sock log 226))))
    (error (co)
      (when log (format log "rpm-available [~a]: ~a~%" url co))
             (rpm-available url :out out :err err :retry (1- retry)))
             (when log
               (format log "rpm-available: ~r more attempt~:p~%" retry))
             (rpm-available url log (1- retry)))
            (co)))))
  "Remove the elements in the list which are better in `*rpm-present*'.
The elements matching `*rpm-skip*' are removed, too."
  (declare (list rpms))
  (remove-if-not
   (lambda (rpm)
  (mapcan (lambda (rpm)
            (let ((pos (binary-pos (rpm-name rpm) *rpm-present*
                                   :test #'string< :key #'rpm-name)))
              (when (and pos (rpm< (svref *rpm-present* pos) rpm)
                         (not (typecase *rpm-skip*
                                (string (search *rpm-skip* (rpm-name rpm)
                                                :test #'char=))
                                (function (funcall *rpm-skip* rpm)))))
                (list rpm))))
          rpms))
  "Print RPM in a full form."
(defun rpm-print-rpm-url (rpmurl)
  "Print the cons (URL . (list RPMs))."
  (declare (type (cons url list) rpmurl))
  (format t " * ~a:~{~<~%~10t ~1,74:; ~a~>~^,~}.~%"
          (car rpmurl) (cdr rpmurl)))

(defun rpm-new-packages (&optional force (log *standard-output*))
  "Get the list of all RPMs in `*rpm-urls*' and put it there.
  (when (or force (notany #'dld-all *rpm-locations*)
  (declare (type (or null stream) log))
  (when (or force (notany #'cdr *rpm-urls*))
    (format t " *** Getting the list of new packages...~%")
    (let ((bt (get-float-time nil)) avail (na 0) (le 0) (bt0 0.0d0))
      (declare (double-float bt bt0) (list avail) (type index-t na le))
      (dolist (url *rpm-urls*)
        (declare (type (cons url list) url))
        (format t " *** processing `~a'...~%" (car url))
        (setq bt0 (get-float-time nil) avail (rpm-available (car url) log))
        (cond ((listp avail)
               (setf (cdr url) (rpm-prune-list avail) le (length (cdr url)))
               (incf na le)
               (format t " *** ~d new RPM~:p (out of ~d in `~a') [~a]~%" le
                       (length avail) (car url) (elapsed-1 bt0 nil)))
              (t (setf (cdr url) (list avail))
                 (format t " *** `~a': failed! [~a]~%"
                         (car url) (elapsed-1 bt0 nil)))))
      (format t " *** ~d new RPM~:p in ~r URL~:p [~a]~%" na
              (length *rpm-urls*) (elapsed-1 bt nil))
      (dolist (ll *rpm-urls*)
        (when (cdr ll) (rpm-print-rpm-url ll))))))
(defun show-rpms (&optional (what "") (local t))
(defun show-rpms (&optional (what ""))
  (if local (rpm-get-present) (rpm-get-available))
  (let ((nrpm (if local (length *rpm-present*)
  (rpm-get-present)
  (if (zerop (length what))
      (format t "~& *** ~d RPM~:p~%" (length *rpm-present*))
      (format t "~&Searching ~d RPM~:p for `~a'~%" (length *rpm-present*)
              what))
  (loop :for rpm :of-type rpm :across *rpm-present*
        :with ii :of-type index-t = 0
        :when (search what (rpm-name rpm) :test #'char-equal)
        :do (format t "~3d. ~a:~{~%~10t~a~}~%" (incf ii) rpm (rpm-note rpm))
        :finally (format t "~[None~:;~:*~d~] found.~%" ii)))
  "Delete some RPMs from the vector `*rpm-present*'."
  (declare (simple-string what))
  (flet ((killrpm (what where)
           (delete what where :key #'rpm-name
  (show-rpms what)
  (when (yes-or-no-p "Delete all these? ")
    (setq *rpm-present*
          (delete what *rpm-present* :key #'rpm-name
                  :test (lambda (wh nm) (search wh nm :test #'char-equal))))
    (show-rpms what)))
  "Return the rpm corresponding to this name."
  (declare (simple-string name) (values (or null rpm)))
  (with-open-pipe (st (pipe-input *rpm-command-line* "-q" name))
    (let ((ln (read-line st nil nil))) (when ln (rpm ln)))))

(defun rpm-to-be-installed ()
  "Print RPMs which are downloaded but not yet installed."
  (loop :for rpm :of-type rpm :across *rpm-present*
        :with ii :of-type index-t = 0
        :unless (rpm-itime rpm)
        :do (rpm-print rpm (incf ii))))
        :unless (consp (car (rpm-note rpm)))
        :do (format t "~3d. ~a:~{~%~10t~a~}~%" (incf ii) rpm (rpm-note rpm))))
  "Make sure `*rpm-present*' is initialized."
(defun rpm-get-present (&optional force (log *standard-output*))
  (when (or force (zerop (length *rpm-present*)))
  (declare (type (or null stream) log))
      (mesg t out " * Finding the RPMs...")
      (setq *rpm-present* (rpm-present))
      (when log (princ " * Finding the RPMs..." log) (force-output log))
            (length *rpm-present*)
      (when log
        (format log "done [~d package~:p; run: ~a; real: ~a]~%"
                (length *rpm-present*)
                (elapsed-1 bt0 t) (elapsed-1 bt1 nil))))))
(defun rpm-get-list (url rpms &key (out *standard-output*) (err *error-output*)
(defun rpm-get-list (url rpms &key (log *standard-output*)
                     (timeout *rpm-timeout*) (len (length rpms)) (retry 2))
           (type index-t len retry))
  (declare (type url url) (list rpms) (type (or null stream) log)
           (type index-t timeout len retry))
                           :max-retry *rpm-max-retry*)
      (with-open-url (sock url :err log :timeout timeout)
        (url-ask sock log 200 "type i")
              :for pos = (rpm-pos (rpm-name rpm))
              :and all :of-type file-size-t = (reduce #'+ rpms :key #'rpm-size)
              :for pos = (binary-pos (rpm-name rpm) *rpm-present*
                                     :test #'string< :key #'rpm-name)
              :with glob :of-type file-size-t = 0
              :do (format t " *** [~d/~d] getting~25t`~a'...~%" ii len rpm)
                  (ftp-get-file sock (format nil "~a.rpm" rpm)
              (multiple-value-bind (tot el st)
                (declare (type file-size-t tot) (double-float el)
                                *rpm-local-target* :log log :bin t
                                :timeout timeout)
                (cond ((rpm-path-valid-p path)
                         (simple-string st))
                (incf glob tot)
                (format t " *** done [~:d bytes, ~a, ~:d bytes/sec]~%"
                        tot st (round tot el)))
              (format t " *** replacing ~a with ~a in `*rpm-present*'~%"
                      (svref *rpm-present* pos) rpm)
              (setf (svref *rpm-present* pos)
                    (rpm-merge-notes rpm (svref *rpm-present* pos)))
              :else :do (format t " *** already have~25t`~a'.~%" rpm)
              :end :finally (return glob)))
    (error (co)
      (when log (format log "rpm-get-list [~a]: ~a~%" url co))
             (rpm-get-list url rpms :out out :err err :retry (1- retry)
             (when log (format log "rpm-get-list: ~r more attempt~:p~%" retry))
             (rpm-get-list url rpms :log log :retry (1- retry)
                           :timeout timeout :len len))
;;;###autoload
(defun rpm-get-new-rpms (&key force (out *standard-output*)
(defun rpm-get-new-rpms (&key force (log *standard-output*)
                         (timeout *rpm-timeout*))
  (declare (type (or null stream) err out))
  "Download the RPMs from `*rpm-urls*'."
  (declare (type (or null stream) log) (type index-t timeout))
  (format t "~& *** [~a] `rpm-get-new-rpms' started.~%" (current-time nil))
  (rpm-get-present force log)
  (format t " *** ~d package~:p present~%" (length *rpm-present*))
  (rpm-new-packages force log)
  (let ((bt (get-float-time nil)) (glob 0))
      (declare (type download-data dld))
    (dolist (url *rpm-urls*)
      (declare (type (cons url list) url))
      (when (cdr url)
        (let ((len (length (cdr url))))
          (format t " *** getting ~d file~:p from `~a':~%" len (car url))
          (rpm-print-rpm-url url)
          (incf glob (rpm-get-list (car url) (cdr url) :log log
                                   :timeout timeout :len len))
          (format t " *** Pruning [~d]..." len) (force-output)
          (setf (cdr url) (rpm-prune-list (cdr url)))
          (format t "done [~d left]~%" (length (cdr url))))))
    (format t " *** [~a] done" (current-time nil))
    (if (zerop glob) (format t ".~%")
                  glob st (round glob el))))
          (format t " [~:d bytes, ~a, ~:d bytes/sec]~%"

;;;###autoload
(defun rpm-list-rpm (name &key (out *standard-output*) (err *error-output*)
;; (rpm-clean-up "/var/tmp/")
;; (rpm-clean-up)
;; (rpm-clean-up "/var/tmp/o/")

                                   :test #'equalp))
                     (out *standard-output*))
  "Remove old RPM files."
  (declare (stream out))
  (etypecase dirs
    ((or string pathname)
     (format out " ***** Cleaning up `~a'~%" dirs)
     (flet ((to-file (rpm) (merge-pathnames (format nil "~a.rpm" rpm) dirs)))
       (do* ((fl (sort (map-in #'rpm (directory (merge-pathnames
     (do* ((fl (sort (map-in #'rpm (directory (merge-pathnames
                                                      "*.rpm" dirs)))
                     #'rpm<) (cdr fl))
           (nn 0) (tot 0 (1+ tot)) (fs 0) (tfs 0) (sz 0) file)
          ((null fl)
           (format out " ***** ~:d/~:d file~:p (~:d/~:d byte~:p) deleted~%"
                   nn tot fs tfs)
           (values nn fs tot tfs))
       (declare (type index-t nn tot) (type file-size-t sz fs tfs))
       (setq file (merge-pathnames (format nil "~a.rpm" (car fl)) dirs)
             sz (file-size file) tfs (+ sz tfs))
       (when (and (cdr fl) (string= (rpm-name (car fl)) (rpm-name (cadr fl))))
         (format out " ~3d * removing ~a (~:d bytes) because of ~a..."
                 (incf nn) (car fl) sz (cadr fl))
         (multiple-value-bind (tt co) (ignore-errors (delete-file file))
           (format out "~:[failed: ~a~;done~*~]~%" tt co))
         (incf fs sz))))
       (declare (type index-t nnt tott) (type file-size-t fst tfst))
       (map nil (lambda (dd)
                  (multiple-value-bind (nn fs tot tfs) (rpm-clean-up dd out)
                    (declare (type index-t nn tot) (type file-size-t fs tfs))
                    (incf nnt nn) (incf fst fs)
                    (incf tott tot) (incf tfst tfs)))
            dirs)
       (format out " Total ~:d/~:d file~:p (~:d/~:d byte~:p) deleted~%"
               nnt tott fst tfst)
       (values nnt fst tott tfst)))))

;;; active mode ftp - doesn't work - why?!

#+(or clisp allegro)
(progn

(defun local-host (sock)
  #+clisp (let ((ho (lisp:socket-stream-local sock)))
            (subseq ho 0 (position #\Space ho :test #'char=)))
  #+clisp (lisp:socket-stream-local sock)
(defun local-port (serv)

(defun ftp-port-command (sock serv &optional (out *standard-output*))
    (declare (type index-t port))
    (url-ask sock out :port "port ~a,~d,~d" ; 200
             (substitute #\, #\. (local-host sock))
    (url-ask sock out 200 "port ~a,~d,~d"
)

(defun socket-accept (serv)
  #+clisp (lisp:socket-accept serv)
  #+allegro (socket:accept-connection serv))

#+nil
(progn

(setq sock (open-url (url "ftp://ftp.gnu.org/pub/gnu/")))
(setq sock (open-url (url "ftp://ftp.nyu.edu/pub/")))
(setq serv (open-socket-server sock))
(ftp-port-command sock serv)
(read-line sock)
(progn
  ;; (format sock "retr clisp.README~%")
  (format sock "list~%")
  (setq sk (socket-accept serv)))
(read-line sk)
(close sock)
(close serv)
(close sk)
(setq serv (lisp:socket-server 3288))
(lisp:socket-server-close serv)
(lisp:socket-stream-handle sock)
(lisp:socket-stream-host sock)
(lisp:socket-stream-port sock)
(lisp:socket-stream-peer sock)
(lisp:socket-stream-local sock)

(ftp-get-file sock "wn.README" "/var/tmp/" )
(ftp-list sock)
(url-ask sock *standard-output* :pwd "pwd") ; 257
(url-ask sock *standard-output* :help "help ~a" "type") ; 214
(url-ask sock *standard-output* 257 "pwd")
(url-ask sock *standard-output* 214 "help ~a" "type")
(url-ask sock *standard-output* 214 "help")
(url-ask sock *standard-output* 250 "cwd gnu")

(setq serv (lisp:socket-server 0))
(lisp:socket-server-close serv)
(setq sock (lisp:socket-connect 21 "ftp.gnu.org"))
(setq serv (lisp:socket-server sock))
(setq sock (lisp:socket-connect 21 "mute"))

;socket.d:fill_hostname
;    printf ("#(%lu \"%s\" \"%s\" %u)\n", hd->host, hd->hostname,
;            hd->truename, hd->port);

)

(provide "rpm")
;;; file rpm.lisp ends here
