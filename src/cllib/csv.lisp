;;; read/write comma-separated values
;;;
;;; Copyright (C) 2003 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: csv.lisp,v 2.7 2003/07/01 21:51:22 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/csv.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-collect'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `with-timing', `log'
  (require :cllib-log (translate-logical-pathname "cllib:log")))

(in-package :cllib)

(export '(csv-print-vector csv-parse-string csv-read-file with-csv
          *csv-separator* *csv-whitespace* *csv-progress*))

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
                         (out '*standard-output*))
                    &body body)
  "Open FILE and set VEC to successive vectors in it."
  (with-gensyms ("WITH-CSV-" in fn ln len cols)
    `(with-timing (:out ,out)
      (let* ((,fn ,file))
        (with-open-file (,in ,fn :direction :input)
          (format ,out "~&Reading `~a' [~:d bytes]..."
                  ,fn (file-length ,in))
          (force-output ,out)
          (loop :with ,vec :and ,cols
            :for ,ln = (read-line ,in nil nil) :and ,len :upfrom 1
            :while ,ln :do (setq ,vec (csv-parse-string ,ln))
            (if ,cols
                (assert (= ,cols (length ,vec)) (,cols ,vec)
                        "~&~s:~:d: Wrong column count ~:d instead of ~:d:~%~s"
                        ,fn ,len (length ,vec) ,cols ,vec)
                (setq ,cols (length ,vec)))
            ,@body
            (when (and ,progress ,out (zerop (mod ,len ,progress)))
              (princ "." ,out) (force-output ,out))
            :finally (progn (format ,out "done [~:d record~:p, ~:d column~:p]"
                                    (decf ,len) ,cols)
                            (return ,len))))))))

;;;###autoload
(defun csv-read-file (inf)
  "Read comma-separated values into a list of vectors."
  (let (len)
    (values (with-collect (coll)
              (setq len (with-csv (vec inf) (coll vec))))
            len)))

(provide :cllib-csv)
;;; file csv.lisp ends here
