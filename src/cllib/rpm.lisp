;;; File: <rpm.lisp - 1999-05-03 Mon 14:12:14 EDT sds@goems.com>
;;;
;;; Copyright (C) 1998-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; Copyright (C) 1998-1999 by Sam Steingold
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Id: rpm.lisp,v 1.13 1999/05/03 18:14:30 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Log: rpm.lisp,v $
;;; Revision 1.13  1999/05/03 18:14:30  sds
;;; (*rpm-max-retry*): new variable.  Replaced the ubiquitous use of
;;; `timeout' keyword argument with `*rpm-timeout*'.
;;; (rpm-pos): new function.
;;; (rpm-skip-p): skip "pre" releases.
;;;
;;; Revision 1.12  1999/04/17 23:22:31  sds
;;; (download-data): new structure (AKA dld).
;;; (*rpm-locations*): a list of `download-data', replaces `*rpm-urls*'.
;;; (rpm-print-rpm-url): removed (replaced with `download-data').
;;; (rpm-new-packages, rpm-list-rpm, rpm-get-new-rpms &c): use `dld'.
;;;
;;; Revision 1.11  1999/04/09 19:19:14  sds
;;; Added `short-string-to-rpm'.  Cleaned up the `rpm' generic function.
;;;
;;; Revision 1.10  1999/03/24 17:04:24  sds
;;; 4 new fields in the RPM structure: `btime', `itime', `ftime' and `size'.
;;; Removed `+unix-epoch+'.
;;; Handle long `ls' output in (rpm string) method.
;;; Added `rpm-list-rpm'.
;;;
;;; Revision 1.9  1999/02/19 14:29:53  sds
;;; `rpm-get-new-rpms': fix for timeout condition.
;;;
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
;;;

(defstruct (download-data (:conc-name dld-)
                          #+cmu (:print-function print-struct-object))
(defstruct (download-data (:conc-name dld-) (:include url)
                           #+cmu (:print-function print-struct-object))
  (fls nil :type list))         ; files found at the location
  (fls nil :type list))        ; files found at the location

  (write (dld-url dld) :stream out)
  (cond ((dld-err dld) (format out "~% - ~a" (dld-err dld)))
  (call-next-method)
  (cond ((dld-err dld) (format out "~% * ~a" (dld-err dld)))
        ((dld-fls dld)
         (let ((sz (reduce #'+ (dld-fls dld) :key
                           (safe-fun1 rpm-size rpm-p 0))))
           (declare (type file-size-t sz))
           (format out "~:[~*~; [~:d bytes]~]: ~{~<~%~10t ~1,74:; ~a~>~^,~}."
                   (zerop sz) sz (dld-fls dld))))))
  "Reset ERR and FLS to NIL."
  (declare (type download-data dld))
  (setf (dld-err dld) nil (dld-fls dld) nil (dld-all dld) nil))

  (setf (dld-err dld) nil (dld-fls dld) nil))
  "The list of the present packages.")
(defcustom *rpm-locations* list
  (macrolet ((mk (&rest args) `(make-download-data :url (make-url ,@args))))
    (list (mk :prot :ftp :host "rawhide.redhat.com"
  (list (make-download-data :prot :ftp :host "rawhide.redhat.com"
                            :path "/i386/RedHat/RPMS/")
        (make-download-data
         :prot :ftp :host "ftp.rge.com"
         :path "/pub/systems/linux/redhat/rawhide/i386/RedHat/RPMS/")
        (make-download-data :prot :ftp :host "ftp.redhat.com"
                            :path "/pub/starbuck/i386/RedHat/RPMS/")
        (make-download-data
         :prot :ftp :host "ftp.rge.com"
         :path "/pub/systems/linux/redhat/current/i386/RedHat/RPMS/")
        (make-download-data :prot :ftp :host "updates.redhat.com"
                            :path "/current/i386/")
        (make-download-data :prot :ftp :host "updates.redhat.com"
                            :path "/current/noarch/")
        (make-download-data :prot :ftp :host "contrib.redhat.com"
                            :path "/libc6/i386/")
        (make-download-data :prot :ftp :host "contrib.redhat.com"
                            :path "/noarch/")
        (make-download-data :prot :ftp :host "developer.redhat.com"
                            :path "/pub/rhcn/RPMS/i386/")
        (make-download-data :prot :ftp :host "developer.redhat.com"
                            :path "/pub/rhcn/RPMS/noarch/"))
  ;; (make-download-data :prot :ftp :host "ftp.suse.com" :path;Xserver
  ;;                 "/pub/suse_update/XFree86-3.3.3.1-SuSE/glibc2/")
  ;; (make-download-data :prot :ftp :host "ftp.cc.gatech.edu" :path
  "*The list of `download-data' structs where to look for RPMs.")
  ;; (make-download-data :prot :ftp :host "ftp.dosemu.org" ; dosemu
  ;;                 :path "/dosemu/"))
  "The list of `download-data' structs where to look for RPMs.")
(defcustom *rpm-local-paths* list
  (case (read-from-string (machine-instance))
    (mute.eaglets.com
     '(#p"/var/tmp/o/RedHat/RPMS/" #p"/var/tmp/" #p"/mnt/n/var/tmp/"
       #p"/mnt/n/var/tmp/o/RedHat/RPMS/"))
    (eho.eaglets.com
     '(#p"/var/tmp/o/RedHat/RPMS/" #p"/var/tmp/" #p"/mnt/n/var/tmp/"
       #p"/mnt/n/mnt/c/tmp/linux/RedHat/RPMS/"))
    (t '(#p"/var/tmp/")))
  "The local path where to download stuff.")
(defcustom *rpm-command-line* simple-string
  "rpm --queryformat \"%{NAME}-%{VERSION}-%{RELEASE}.%{ARCH} %{BUILDTIME} %{INSTALLTIME} %{SIZE}\\n\""
  "The rpm(1) command line to print all the installed packages.
See `rpm --querytags' for more tags")
(defcustom *rpm-timeout* index-t 600
  "*The default timeout (in seconds) for RPM network operations.")
(defcustom *rpm-max-retry* (or null index-t) 10
  "*The default value of max-retry for RPM.
If nil, retry ad infinitum, otherwise a positive fixnum.")

;;;
;;; RPM
;;;

(eval-when (compile load eval)
(defstruct (rpm #+cmu (:print-function print-struct-object))
(eval-when (load compile eval)
  (vers "vers?" :type simple-string)
  (rels "rels?" :type simple-string)
  (arch "arch?" :type simple-string)
  (btime nil :type (or null integer)) ; build time
  (itime nil :type (or null integer)) ; install time
  (ftime nil :type (or null integer)) ; file time
  (size nil :type (or null integer))
  (note nil))
)

(defconst +bad-rpm+ rpm (make-rpm) "*The convenient constant for init.")

(defmethod print-object ((rpm rpm) (out stream))
  (if *print-readably* (call-next-method)
(defmethod print-object ((rpm rpm) (stream stream))
              (rpm-name rpm) (rpm-vers rpm) (rpm-rels rpm) (rpm-arch rpm)
      (format stream "~:[~;\"~]~a-~a-~a.~a~:[~;\"~]" *print-escape*

(defsubst rpm-pos (name)
  "Find the position of the RPM in `*rpm-present*'."
  (declare (simple-string name))
  (binary-pos name *rpm-present* :test #'string< :key #'rpm-name))

(defun rpm-skip-p (rpm)
  "Do not download such an RPM."
  (declare (type rpm rpm))
  (or (string-beg-with "pre" (rpm-vers rpm))
      (and (search "kernel" (rpm-name rpm) :test #'char=)
           (or (notevery #'digit-char-p (rpm-rels rpm))
               (string= (rpm-vers rpm)
                        (rpm-vers (svref *rpm-present*
                                         (rpm-pos (rpm-name rpm)))))))))
(defcustom *rpm-skip* (or (function (rpm) boolean) string null) #'rpm-skip-p
  "Skip these RPMs.")

(defun short-string-to-rpm (name)
  "Convert the short string like `pack-1.2-3.i386.rpm' to RPM.
Do not use it!!!  Use the generic function `rpm' instead!!!"
  (declare (simple-string name) (values rpm))
  (let* ((len (length name)) (p0 (position #\- name :from-end t))
         (p1 (position #\- name :from-end t :end (1- p0)))
         (p2 (if (string= ".rpm" name :start2 (- len 4)) (- len 4) len))
         (p3 (or (position #\. name :from-end t :start p0 :end p2) p2)))
    (make-rpm :name (subseq name 0 p1) :vers (subseq name (1+ p1) p0)
              :rels (subseq name (1+ p0) p3) :arch (subseq name (1+ p3) p2))))

(eval-when (compile load eval) (fmakunbound 'rpm))
(defgeneric rpm (obj)
(eval-when (load compile eval) (fmakunbound 'rpm))
  (:method ((obj rpm)) obj)
  (:method ((obj symbol))
    (unintern obj)
    (short-string-to-rpm (symbol-name obj)))
  (:method ((path pathname))
    (let ((rr (short-string-to-rpm (pathname-name path))))
      (setf (rpm-note rr) (list path)
            (rpm-size rr) (file-size path)
            (rpm-ftime rr) (file-write-date path))
      rr)))
(declaim (ftype (function (t) rpm) rpm))
(defmethod rpm ((obj string))
  ;; should work with strings like:
  ;; "pack-1.2-3.i386.rpm"
  ;; "-rw-r--r-- 1 sds sds 220876 Mar 20 19:29 mgetty-voice-1.1.20-1.i386.rpm"
  ;; "rpm-2.92-14.i386 922075412 922585615 1667226"
  (let* ((toks (string-tokens
                (nsubstitute #\- #\: (string-trim +whitespace+ obj))))
         (rpm (ignore-errors (rpm (car toks)))))
    (assert (symbolp (car toks)) ()
            "The first token in ~s is not a symbol: cannot convert to RPM" obj)
    (cond (rpm
           (setf (rpm-btime rpm)
                 (if (cadr toks) (+ +unix-epoch+ (cadr toks)))
                 (rpm-itime rpm)
                 (if (caddr toks) (+ +unix-epoch+ (caddr toks)))
                 (rpm-size rpm) (cadddr toks))
           (return-from rpm rpm))
          ((char/= #\- (schar (symbol-name (car toks)) 0))
           (error "special file cannot be an RPM: ~s" obj))
          ((numberp (fourth toks)) (setq toks (cdddr toks)))
          ((numberp (fifth toks)) (setq toks (cddddr toks))))
    (assert (symbolp (fifth toks)) ()
            "~s: ~s is not a symbol: cannot convert to RPM" obj (fifth toks))
    (setf rpm (rpm (fifth toks)) (rpm-size rpm) (first toks)
          (rpm-ftime rpm)
          (let ((day (third toks)) (mon (infer-month (second toks))))
            (if (numberp (fourth toks))
                (encode-universal-time 0 0 0 day mon (fourth toks))
                (let* ((str (symbol-name (fourth toks)))
                       (mi (parse-integer str :start 3))
                       (ho (parse-integer str :end 2))
                       (now (get-universal-time))
                       (ye (nth-value 5 (decode-universal-time now)))
                       (ft (encode-universal-time 0 mi ho day mon ye)))
                  (if (< ft now) ft
                      (encode-universal-time  0 mi ho day mon (1- ye)))))))
    rpm))

(defun rpm< (r0 r1)
  "Sorting order on RPMs."
  (declare (type rpm r0 r1))
  (or (string< (rpm-name r0) (rpm-name r1))
      (and (string= (rpm-name r0) (rpm-name r1))
           (or (version< (rpm-vers r0) (rpm-vers r1))
               (and (string= (rpm-vers r0) (rpm-vers r1))
                    (or (release< (rpm-rels r0) (rpm-rels r1))
                        (and (string= (rpm-rels r0) (rpm-rels r1))
                             (string< (rpm-arch r0) (rpm-arch r1)))))))))

(defun version< (v0 v1)
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
                               (list (cons (time2date (rpm-itime r1))
                                           (time2date (rpm-btime r1)))))))
  r0)

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

(defun rpm-downloaded ()
  "Return the list of RPMs already downloaded."
  (sort (mapcar
         #'rpm (delete-if-not
                #'rpm-path-valid-p
                (mapcan (lambda (pp) (directory (merge-pathnames "*.rpm" pp)))
                        (adjoin *rpm-local-target* *rpm-local-paths*
                                :test #'equalp))))
                        *rpm-local-paths*)))
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
  "Return the list of all available RPMs."
  (declare (type url url) (type (or null stream) err out) (type index-t retry))
  (handler-case
      (with-open-url (sock url :err err :timeout *rpm-timeout*
                           :max-retry *rpm-max-retry*)
        (let ((data (ftp-get-passive-socket sock err nil *rpm-timeout*)))
          (when (ignore-errors (url-ask sock err :list "list *.rpm")) ; 150
        (let ((data (ftp-get-passive-socket sock t nil *rpm-timeout*)))
          (when (ignore-errors (url-ask sock err 150 "list")) ; nlst
              (url-ask sock err :list))))) ; 226
    (login (co)
              (url-ask sock err 226)))))
      (cond ((plusp retry)
      (mesg :log err " * rpm-available [~a]:~% * ~a~%" url co)
             (rpm-available url :out out :err err :retry (1- retry)))
            (t (error co))))))

            (co)))))
  "Remove the elements in the list which are better in `*rpm-present*'.
The elements matching `*rpm-skip*' are removed, too."
  (declare (list rpms))
  (remove-if-not
   (lambda (rpm)
  (mapcan
       (and pos (rpm< (svref *rpm-present* pos) rpm)
            (not (typecase *rpm-skip*
       (when (and pos (rpm< (svref *rpm-present* pos) rpm)
                  (not (typecase *rpm-skip*
                         (string (search *rpm-skip* (rpm-name rpm)
                                         :test #'char=))
                         (function (funcall *rpm-skip* rpm))
                         (null nil)
                         (t (error 'case-error :proc 'rpm-prune-list
                                   :args (list '*rpm-skip* *rpm-skip*
                                               'function 'string nil))))))
         (list rpm))))
(defun rpm-print (rpm &optional idx (out *standard-output*))
  "Print RPM in a full form."
  (declare (type rpm rpm) (stream out))
  (format out "~@[~3d. ~]~a [~@[~:d bytes, ~]~a/~a/~a]:~{~%~10t~a~}~%"
          idx rpm (rpm-size rpm)
          (if (rpm-btime rpm) (time2date (rpm-btime rpm)))
          (if (rpm-itime rpm) (time2date (rpm-itime rpm)))
          (if (rpm-ftime rpm) (time2date (rpm-ftime rpm)))
          (rpm-note rpm)))

(defun rpm-get-available (&key force (out *standard-output*)
                          (err *error-output*))
(defun rpm-new-packages (&optional force (out *standard-output*))
  (declare (type (or null stream) out err))
  (when (or force (notany #'dld-all *rpm-locations*)
  (declare (type (or null stream) out))
  (when (or force (notany #'dld-fls *rpm-locations*))
    (format t " *** Getting the list of new packages...~%")
    (let ((bt (get-float-time nil)) avail (na 0) (le 0) (bt0 0.0d0)
      (declare (double-float bt bt0) (type index-t na le))
      (dolist (dld *rpm-locations*)
      (declare (double-float bt bt0) (type (or list condition) avail)
               (type index-t na le))
        (dld-reset dld)
        (format out " *** processing `~a'...~%" dld)
        (handler-case
        (format t " *** processing `~a'...~%" dld)
        (setq bt0 (get-float-time nil) avail (rpm-available dld :out out))
        (etypecase avail
          (list (setf (dld-fls dld) (rpm-prune-list avail)
                      le (length (dld-fls dld)))
                (incf na le)
                (format t " *** ~d new RPM~:p (out of ~d) [~a]:~% --> ~a~%"
                        le (length avail) (elapsed-1 bt0 nil) dld))
          (condition (setf (dld-err dld) avail)
                     (format t " *** failed: ~a [~a]~%"
                             (dld-err dld) (elapsed-1 bt0 nil)))))
      (format t " *** ~d new RPM~:p in ~r URL~:p [~a]~%" na
      (dolist (dld *rpm-locations*)
          (format out " * ~a~%" dld))))))

          (format t " * ~a~%" dld))))))
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
        :do (rpm-print rpm (incf ii))
        :finally (format t "~[None~:;~:*~d~] found.~%" ii)))
  "Delete some RPMs from the vector `*rpm-present*'."
  (declare (simple-string what))
  (flet ((killrpm (what where)
           (delete what where :key #'rpm-name
  (show-rpms what)
                   :test (lambda (wh nm) (search wh nm :test #'char-equal)))))
    (when (and (/= 0 (show-rpms what)) (yes-or-no-p "Delete all these? "))
      (setq *rpm-present* (killrpm what *rpm-present*))
    (when (yes-or-no-p "Delete all these? ")
        (setf (dld-fls dld) (killrpm what (dld-fls dld)))
        (format t " -> ~a~%" dld))
      (show-rpms what))))

(defun rpm-current (name)
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

(defun rpm-get-present (&optional force (out *standard-output*))
  "Make sure `*rpm-present*' is initialized."
  (declare (type (or null stream) out))
  (when (or force (zerop (length *rpm-present*)))
    (let ((bt0 (get-float-time t)) (bt1 (get-float-time nil)))
      (mesg t out " * Finding the RPMs...")
      (setq *rpm-present* (rpm-present))
      (mesg t out "done [~d package~:p; run: ~a; real: ~a]~%"
            (length *rpm-present*)
            (elapsed-1 bt0 t) (elapsed-1 bt1 nil)))))

;;;###autoload
(defun rpm-get-list (url rpms &key (out *standard-output*) (err *error-output*)
  "Get the list of RPMS from URL."
  (declare (type url url) (list rpms) (type (or null stream) out err)
           (type index-t len retry))
  (handler-case
      (with-open-url (sock url :err err :timeout *rpm-timeout*
                           :max-retry *rpm-max-retry*)
        (url-ask sock err :type "type i") ; 200
        (loop :for rpm :in rpms
        (url-ask sock err 200 "type i")
              :for pos = (rpm-pos (rpm-name rpm))
              :and all :of-type file-size-t = (reduce #'+ rpms :key #'rpm-size)
              :then (- all (rpm-size rpm))
              :with got :of-type file-size-t = 0
              :do (format out " *** [~d/~d] ~a~@[ [~:d bytes~@[, ~/pr-secs/~]~
~@[/~/pr-secs/~]]~]~%"
              :do (format t " *** [~d/~d] ~a~@[ [~:d bytes~@[, ~/pr-secs/~]~
                          (url-eta all))
              :if (rpm< (svref *rpm-present* pos) rpm) :do
              (multiple-value-bind (tot el st path)
                  (ftp-get-file sock (format nil "~a.rpm" rpm)
                                *rpm-local-target* :err err :bin t :out out)
                (declare (type file-size-t tot) (double-float el)
                         (simple-string st) (type pathname path))
                (cond ((rpm-path-valid-p path)
                       (incf got tot)
                       (format out " *** done [~:d bytes, ~a, ~:d bytes/sec]
 *** replacing ~a with ~a in `*rpm-present*'~%"
                       (format t " *** done [~:d bytes, ~a, ~:d bytes/sec]
                               (svref *rpm-present* pos) rpm)
                       (setf (svref *rpm-present* pos)
                             (rpm-merge-notes rpm (svref *rpm-present* pos))))
                      (t (format out " *** file `~a' is corrupted:~%" path)
                         (with-open-pipe (st (pipe-input "rpm -K" path)))
                      (t (format t " *** file `~a' is corrupted:~%" path)
              :else :do (format out " *** already have~25t`~a'.~%" rpm)
              :end :finally (return got)))
              :else :do (format t " *** already have~25t`~a'.~%" rpm)
      (mesg :log err "rpm-get-list [~a]:~% - ~a~%" url co)
      (cond ((plusp retry)
      (mesg :log err "rpm-get-list [~a]:~% * ~a~%" url co)
             (rpm-get-list url rpms :out out :err err :retry (1- retry)
                           :len len))
            (0)))))

;;;###autoload
(defun rpm-get-new-rpms (&key force (out *standard-output*)
  ;; (rpm-get-new-rpms)
  "Download the RPMs from `*rpm-locations*'."
  (declare (type (or null stream) err out))
  (format out "~& *** [~a] `rpm-get-new-rpms' started.~%" (current-time nil))
  (rpm-get-present force out)
  (format t "~& *** [~a] `rpm-get-new-rpms' started.~%" (current-time nil))
  (rpm-get-available :force force :out out :err err)
  (format t " *** ~d package~:p present~%" (length *rpm-present*))
  (rpm-new-packages force out)
        (*url-default-timeout* *rpm-timeout*)
    (declare (double-float bt) (type file-size-t glob))
    (dolist (dld *rpm-locations*)
      (declare (type download-data dld))
      (cond ((dld-fls dld)
             (let ((len (length (dld-fls dld))))
               (format out " *** getting ~d file~:p: ~a~%" len dld)
               (incf glob (rpm-get-list (dld-url dld) (dld-fls dld) :out out
               (format t " *** getting ~d file~:p: ~a~%" len dld)
               (incf glob (rpm-get-list dld (dld-fls dld) :out out :err err
                                        :len len))
               (format t " *** Pruning [~d]..." len) (force-output)
            ((dld-err dld) (format out " *** ~a~%" dld))
               (format t "done [~d left]~%" (length (dld-fls dld)))))
            ((dld-err dld) (format t " *** ~a~%" dld))
            (t (format t " *** ~a:~60tno new packages~%" dld))))
    (format t " *** [~a] done" (current-time nil))
    (if (zerop glob) (format t ".~%")
                  glob st (round glob el))))
          (format t " [~:d bytes, ~a, ~:d bytes/sec]~%"

;;;###autoload
(defun rpm-list-rpm (name &key (out *standard-output*) (err *error-output*)
(defun rpm-list-rpm (name &key (out *standard-output*) (err *error-output*))
  "Look for the RPM on all sites."
        (loop :initially (format out " *** ~a:~%" dld)
    (with-open-url (sock dld :err err :timeout *rpm-timeout*
                         :max-retry *rpm-max-retry*)
      (format out " *** ~a:~%" dld)
      (ftp-list sock :name name :err err :out out))))
;;  (:documentation "Remove old RPM files."))
;; (rpm-clean-up "/var/tmp/")
;; (rpm-clean-up)
;; (rpm-clean-up "/var/tmp/o/")

;; (rpm-clean-up "/var/tmp/o/RedHat/RPMS/")
                                   :test #'equalp))
                     (out *standard-output*))
  "Remove old RPM files."
  (declare (stream out))
  (etypecase dirs
    ((or string pathname)
     (format out " ***** Cleaning up `~a'~%" dirs)
     (flet ((to-file (rpm) (merge-pathnames (format nil "~a.rpm" rpm) dirs)))
       (do* ((fl (sort (map-in #'rpm (directory (merge-pathnames
                                                 "*.rpm" dirs))) #'rpm<)
                 (cdr fl))
             (nn 0) (tot 0 (1+ tot)) (fs 0) (tfs 0) (sz 0) nfile (rm nil nil)
             (file (to-file (car fl)) nfile) (good (rpm-path-valid-p file)))
            ((null fl)
             (format out " ***** ~:d/~:d file~:p (~:d/~:d byte~:p) deleted~%"
                     nn tot fs tfs)
             (values nn fs tot tfs))
         (declare (type index-t nn tot) (type file-size-t sz fs tfs))
         (setq sz (file-size file) tfs (+ sz tfs))
         (unless good (setq rm "corruption"))
         (when (cdr fl)
           (setq nfile (to-file (cadr fl)) good (rpm-path-valid-p nfile))
           (unless good (with-open-pipe (st (pipe-input "rpm -K" nfile))))
           (when (and good (string= (rpm-name (car fl)) (rpm-name (cadr fl))))
             (setq rm (cadr fl))))
         (when rm
           (format out " ~3d * removing ~a (~:d bytes) because of ~a..."
                   (incf nn) (car fl) sz rm)
           (multiple-value-bind (tt co) (ignore-errors (delete-file file))
             (format out "~:[failed: ~a~;done~*~]~%" tt co))
           (incf fs sz)))))
    (sequence
     (let ((nnt 0) (fst 0) (tott 0) (tfst 0))
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
  #+allegro (socket:ipaddr-to-dotted (socket:local-host sock)))

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
