;;; read/write comma-separated values
;;;
;;; Copyright (C) 2003-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: csv.lisp,v 2.15 2005/03/08 20:06:34 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/csv.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-collect'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `with-timing', `log'
  (require :cllib-log (translate-logical-pathname "cllib:log")))

(in-package :cllib)

(export '(csv-print-vector csv-parse-string csv-read-file with-csv
          *csv-separator* *csv-whitespace* *csv-progress* *csv-progress-1*))

(defcustom *csv-separator* character #\,
  "The separator in the CSV file, normally the comma.")

(defun csv-print-vector (vec &optional (out *standard-output*))
  "Print a vector as a comma-separated line."
  (declare (type vector vec) (stream out))
  (loop :with len = (length vec) :for val :across vec :and ii :from 1
        :when val :do (write val :stream out :escape nil)
        :unless (= ii len) :do (write-char *csv-separator* out))
  (terpri out))

(defcustom *csv-whitespace* (or null string) +whitespace+
  "The string of characters to trim from the values.")
(defcustom *csv-progress* integer 1000
  "*How often the progress report should be made")
(defcustom *csv-progress-1* integer 10
  "*How often the secondary progress report should be made")

(defun csv-trim (whitespace string)
  "Trim the string argument from the whitespace."
  (let ((clean (string-trim whitespace string)))
    (if (zerop (length clean)) nil clean)))

(defun csv-parse-string (string &key
                         ((:separator *csv-separator*) *csv-separator*)
                         ((:whitespace *csv-whitespace*) *csv-whitespace*))
  "Parse a string, returning a vector of strings."
  (loop :with num = (count *csv-separator* string :test #'char=)
    :with res = (make-array (1+ num))
    :for ii :from 0 :to num
    :for beg = 0 :then (1+ end)
    :for end = (or (position *csv-separator* string :test #'char= :start beg)
                   (length string))
    :do (setf (aref res ii)
              (when (> end beg) ; otherwise NIL = missing
                (csv-trim *csv-whitespace* (subseq string beg end))))
    :finally (return res)))

(defmacro with-csv ((vec file &key (progress '*csv-progress*)
                         skip-first-line junk-allowed
                         (progress-1 '*csv-progress-1*) limit
                         (out '*standard-output*) columns)
                    &body body)
  "Open FILE and set VEC to successive vectors in it.
Return 3 values:
  number of records (lines) read,
  number of bytes in the file,
  fraction of bytes read"
  (with-gensyms ("WITH-CSV-" in fn fsize ln len cols pro pro1 pro1-count lim)
    `(with-timing (:out ,out :count ,len :units "records")
       (let* ((,fn ,file) (,pro ,progress) (,pro1 ,progress-1) ,fsize
              ,@(when limit `((,lim ,limit))))
         (with-open-file (,in ,fn :direction :input)
           (format ,out "~&Reading `~a' [~:d bytes]..."
                   ,fn (setq ,fsize (file-length ,in)))
           (force-output ,out)
           (when ,skip-first-line (read-line ,in))
           (loop :with ,vec :and ,cols = ,columns :and ,pro1-count = 0
             :for ,ln = (read-line ,in nil nil) :while ,ln
             ,@(when limit
                 `(:when (and ,lim (= ,len ,lim))
                    :do (warn "reached the limit of ~:D record~:P ~
                               at ~:D byte~:P (~4F%), aborted~%"
                              ,len (file-position ,in)
                              (/ (file-position ,in) ,fsize 1d-2))
                        (loop-finish)))
             :unless (and ,junk-allowed
                          (zerop (length (setq ,ln (string-trim
                                                    *csv-whitespace* ,ln)))))
             :do (setq ,vec (csv-parse-string ,ln)) (incf ,len)
             (if ,cols
                 (assert (= ,cols (length ,vec)) (,cols ,vec)
                         "~&~s:~:d: Wrong column count ~:d instead of ~:d:~%~s"
                         ,fn ,len (length ,vec) ,cols ,vec)
                 (setq ,cols (length ,vec)))
             ,@body
             (when (and ,pro ,out (zerop (mod ,len ,pro)))
               (princ "." ,out) (force-output ,out)
               (when (and ,pro1 (= ,pro1 (incf ,pro1-count)))
                 (let* ((pos (/ (file-position ,in) ,fsize)) (eta (eta pos))
                        (incomplete
                         (if ,(when limit `(and ,lim (> ,len (* ,lim pos))))
                             "*" "")))
                   (format ,out "<~A~:D: ~4F% ETA: ~/pr-secs/~A>"
                           incomplete ,len (* pos 1d2) eta incomplete))
                 (force-output ,out) (setq ,pro1-count 0)))
             :end
             :finally (format ,out "done [~:d record~:p~@[, ~:d column~:p~]]"
                              ,len ,cols)
             :finally (return
                        (values ,len (file-length ,in)
                                (if (zerop ,fsize) 1
                                    (/ (file-position ,in) ,fsize))))))))))

;;;###autoload
(defun csv-read-file (inf)
  "Read comma-separated values into a list of vectors."
  (let (len size)
    (values (with-collect (coll)
              (setf (values len size) (with-csv (vec inf) (coll vec))))
            len size)))

(provide :cllib-csv)
;;; file csv.lisp ends here
