;;; Regression Testing
;;;
;;; Copyright (C) 1999-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: tests.lisp,v 2.2 2000/05/19 19:14:22 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/tests.lisp,v $

(eval-when (load compile eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `mesg'
  (require :log (translate-logical-pathname "cllib:log"))
  ;; these files will be tested:
  (require :string (translate-logical-pathname "cllib:string"))
  (require :date (translate-logical-pathname "cllib:date"))
  (require :url (translate-logical-pathname "cllib:url"))
  (require :rpm (translate-logical-pathname "cllib:rpm"))
  (require :elisp (translate-logical-pathname "cllib:elisp")))

(in-package :cllib)

(export '(test-all))

(defun test-string (&key (out *standard-output*))
  (mesg :test out " ** ~s...~%" 'test-string)
  (let ((num-err 0))
    (flet ((ts (res seq from to &rest keys)
             (mesg :test out " * ~s ~s ~s~{ ~s~} -> ~s~%" seq from to keys res)
             (let ((r1 (apply #'substitute-subseq seq from to keys)))
               (unless (string= r1 res)
                 (incf num-err)
                 (format
                  out " ### FAILED: ~s ~s ~s~{ ~s~}~% ->~10t~s~% /=~10t~s~%"
                  seq from to keys r1 res)))))
      (ts "ab123efghcda" "abcdefghcda" "cd" "123" :start 1 :end 6)
      (ts "ab123efgh123a" "abcdefghcda" "cd" "123" :start 1)
      (ts "ab123efghcda" "abcdefghcda" "cd" "123" :end 6)
      (ts "ab123efgh123a" "abcdefghcda" "cd" "123")
      (ts "abcdefghcda" "abcdefghcda" "cd" "123" :start 5 :end 6))
    (mesg :test out " ** ~s: ~:d error~:p~%" 'test-string num-err)
    num-err))

(defun test-date (&key (out *standard-output*))
  (mesg :test out " ** ~s...~%" 'test-date)
  (let ((num-err 0))
    (loop :repeat 10 :do
          (let* ((n0 (random 100000)) (dd (days2date n0)) (n1 (date2days dd)))
            (mesg :test out "~6:d --> ~a --> ~6:d~%" n0 dd n1)
            (unless (= n0 n1)
              (incf num-err)
              (format t " ### FAILED: ~6:d --> ~a --> ~6:d~2%" n0 dd n1))))
    (flet ((ts (nn st)
             (mesg :test out "~30s --> ~d --> ~a~%" st nn (dttm->string nn))
             (unless (= nn (string->dttm st))
               (incf num-err)
               (format t " ### FAILED: ~s --> ~d, not ~d~2%"
                       st (string->dttm st) nn))))
      (ts 3126896578 "Mon Feb  1 17:22:58 1999 EST")
      (ts 3126896578 "Mon Feb  1 17:22:58 1999 EST")
      (ts 3126878578 "Feb  1 Mon 17:22:58 1999 GMT")
      (ts 3126878578 "Feb  1 Mon 17:22:58 1999")
      (ts 3126878578 "1999 Feb  1 Mon 17:22:58")
      (ts 3126896578 "1999-02-01 Mon 17:22:58 EST")
      (ts 3126878578 "1999-02-01 17:22:58")
      (ts 3126878578 "1999-02-01 17:22:58 GMT")
      (ts 3126878578 "1999 Feb  1 17:22:58")
      (ts 3126896578 "Feb  1 17:22:58 1999 EST"))
    (mesg :test out " ** ~s: ~:d error~:p~%" 'test-date num-err)
    num-err))

(defun test-rpm (&key (out *standard-output*))
  (mesg :test out " ** ~s...~%" 'test-rpm)
  (let ((num-err 0))
    (flet ((av (v0 v1)
             (mesg :test out " ~a < ~a~%" v0 v1)
             (unless (version< v0 v1)
               (incf num-err)
               (format t " ### FAILED: ~a < ~a~2%" v0 v1))))
      (av "3.3.2" "3.3.11")
      (av "4.2b980516" "4.2")
      (av "3.3.2pl2" "3.3.3")
      (av "1.1b" "1.1.1")
      (av "3.0" "3.0.3"))
    (mesg :test out " ** ~s: ~:d error~:p~%" 'test-rpm num-err)
    num-err))

(defun test-url (&key (out *standard-output*))
  (mesg :test out " ** ~s...~%" 'test-url)
  (let ((num-err 0))
    (flet ((ts (st url pr)
             (mesg :test out " * ~s~% - ~s~% - ~s~%" st url pr)
             (let ((uu (url st)))
               (unless (equalp uu url)
                 (incf num-err)
                 (format t " ### PARSING FAILED:~% ### ")
                 (terpri) (pr uu) (terpri)
                 (princ " ### ") (pr url) (terpri) (terpri))
               (unless (string= (princ-to-string uu) pr)
                 (incf num-err)
                 (format
                  t " ### PRINTING FAILED:~%~5t~s -->~%~5t~s~% not~5t~s~2%"
                  st uu st)))))
      (ts "news://nntp.gnu.org/gnu.discuss"
          (make-url :prot :news :user "" :pass "" :host "nntp.gnu.org" :port 0
                    :path "/gnu.discuss")
          "news://nntp.gnu.org/gnu.discuss")
      (ts "news:gnu.discuss"
          (make-url :prot :news :user "" :pass "" :host "" :port 0
                    :path "gnu.discuss")
          "news:gnu.discuss")
      (ts "ftp://user#password@host.domain/path/to/file"
          (make-url :prot :ftp :user "user" :pass "password" :host
                    "host.domain" :port 0 :path "/path/to/file")
          "ftp://user#password@host.domain/path/to/file")
      (ts "www.gnu.org/gpl.html"
          (make-url :prot :http :user "" :pass "" :host "www.gnu.org" :port 0
                    :path "/gpl.html")
          "http://www.gnu.org/gpl.html"))
    (mesg :test out " ** ~s: ~:d error~:p~%" 'test-url num-err)
    num-err))

(defun test-elisp (&key (out *standard-output*))
  (mesg :test out " ** ~s...~%" 'test-elisp)
  (let ((*readtable* +elisp-readtable+) (num-err 0))
    (flet ((ts (str obj)
             (mesg :test out " * ~s --> ~s~%" str obj)
             (handler-case
                 (let ((o1 (read-from-string str)))
                   (unless (equalp o1 obj)
                     (format t " ### READING FAILED: ~s != ~s~%" o1 obj)
                     (incf num-err)))
               (error (err)
                 (format t " ### READ ERROR: ~a~%" err)
                 (incf num-err)))))
      (ts "[a ?\\C-a ?c #\\z]" #(a (:control #\a) #\c #\z))
      (ts "[?Z ?\\^M ?\\n]" #(#\Z (:control #\M) #\Newline)))
    (mesg :test out " ** ~s: ~:d error~:p~%" 'test-elisp num-err)
    num-err))

(defun test-all (&key (out *standard-output*))
  (mesg :test out "~& *** ~s: regression testing...~%" 'test-all)
  (let ((num-err (+ (test-string :out out)
                    (test-date :out out)
                    (test-rpm :out out)
                    (test-url :out out)
                    (test-elisp :out out))))
    (mesg :test out " *** ~s: ~:d error~:p~%" 'test-all num-err)))

(provide :tests)
;;; file tests.lisp ends here
