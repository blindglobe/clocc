;;; File: <rpm.lisp - 1998-11-17 Tue 20:01:41 EST sds@eho.eaglets.com>
;;;
;;; Copyright (C) 1998-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; Copyright (C) 1998 by Sam Steingold
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Id: rpm.lisp,v 1.1 1998/11/18 01:01:56 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $
;;; $Log: rpm.lisp,v $
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
(defcustom *rpm-installed* list nil
  "The list of installed packages.")
;;; RPM
(defstruct (rpm)
  (name "" :type simple-string)
  (vers "" :type simple-string)
  (bld "" :type simple-string))

(defmethod print-object ((rpm rpm) (stream stream))
              (rpm-name rpm) (rpm-vers rpm) (rpm-rels rpm) (rpm-arch rpm)
      (format stream "~a-~a-~d" (rpm-name rpm) (rpm-vers rpm) (rpm-bld rpm))))
  "Find the position of the RPM in `*rpm-present*'."
(fmakunbound 'rpm)
(defgeneric rpm (obj) (:documentation "Convert to RPM."))
  ;; should work with strings like:
(defmethod rpm ((obj symbol)) (rpm (symbol-name obj)))
(defmethod rpm ((obj rpm)) obj)
(defmethod rpm ((obj string))
  (let* ((p0 (position #\- obj :from-end t))
         (p1 (position #\- obj :from-end t :end (1- p0))))
    (make-rpm :name (subseq obj 0 p1) :vers (subseq obj (1+ p1) p0)
              :bld (subseq obj (1+ p0)))))

(defun rpm-installed (&optional force)
  "Update `*rpm-installed*' and return it."
  (setq *rpm-installed*
        (if (and (null force) *rpm-installed*) *rpm-installed*
            (do* ((st (pipe-input "rpm -qa")) res
                  (line (read-line st nil nil) (read-line st nil nil)))
                 ((null line) (sort res #'string< :key #'rpm-name))
              (declare (stream st) (simple-string line))
              (push (rpm line) res)))))
                      (retry 2))
(defun rpm-available (url)
  (handler-case
  (with-open-url (sock (url url) *readtable* t)
    (format sock "ls~%")
    (loop :for line = (read-line sock nil nil) :while line :do
          (format t "~a~%" line))))

; (rpm-available "ftp://www.redhat.com/redhat/redhat-5.2/")
;;; file rpm.lisp ends here
