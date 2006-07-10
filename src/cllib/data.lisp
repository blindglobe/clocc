;;; Data Analysis and Visualization
;;;
;;; Copyright (C) 2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: data.lisp,v 1.3 2006/07/10 17:57:53 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/data.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `mdl'
  (require :cllib-math (translate-logical-pathname "cllib:math"))
  ;; `with-csv'
  (require :cllib-csv (translate-logical-pathname "cllib:csv"))
  ;; `fill-buckets'
  (require :cllib-lift (translate-logical-pathname "cllib:lift"))
  ;; `plot-histogram'
  (require :cllib-gnuplot (translate-logical-pathname "cllib:gnuplot")))

(in-package :cllib)

(export '(analyse-csv *buckets* *columns*))

(defcustom *buckets* (list lift:bucket) ()
  "The list of buckets to fill in `analyse-csv'.")
(defcustom *columns* (or (eql t) (list (or fixnum string symbol))) t
  "The list of column specs to study in `analyse-csv'.")

(defun unroll-column-specs (col-specs names ncol)
  "Turn `columns' into a list of `interesting' indexes."
  (etypecase col-specs
    (null (error "empty column selection"))
    ((eql t) (loop :for i :from 0 :below ncol :collect i))
    (cons (delete
           nil (mapcar (lambda (spec)
                         (etypecase spec
                           ((or string symbol)
                            (assert names (names)
                                    "column spec ~S requires a names line" spec)
                            (or (position spec names :test #'string-equal)
                                (cerror "drop it" "no ~S in ~S" spec names)))
                           (fixnum
                            (if (< -1 spec ncol) spec
                                (cerror "drop it" "~S is out of range [0:~D]"
                                        spec (1- ncol))))))
                       col-specs)))))

(defun strings-to-nums (lines col-specs &optional (len (length lines))
                        (out *standard-output*))
  "Convert some strings to numbers, in place."
  (with-timing (:out out)
    (let ((drop 0))
      (mesg :log out "Converting strings to numbers...")
      (setq lines
            (delete-if-not
             (lambda (v)
               (loop :for i :in col-specs
                 :for num = (read-from-string (aref v i))
                 :always (if (numberp num) (setf (aref v i) num)
                             (warn "[~D] dropped ~S: non-number ~S at ~D"
                                   (incf drop) v num i))))
             lines))
      (mesg :log out "dropped ~:D lines (out of ~:D, ~4F%)"
            drop len (/ (* 1d2 drop) len))
      (values lines (- len drop)))))

(defun stat-column (lines col buckets names plot file
                    &optional (len (length lines)) (out *standard-output*))
  (let* ((name (if names (aref names col) (format nil "C~D")))
         (key (lambda (v) (aref v col)))
         (mdl (standard-deviation-mdl lines :key key)))
    (mesg :log out "~3D ~15@A" col name)
    (if plot
        (plot-histogram lines 100 :key key :plot plot :mdl mdl
                        :xlabel (format nil "~A -- ~A(~D)" file name col))
        (mesg :log out " ~A~%" mdl))
    (let ((bl (mapcar (port:compose lift:bucket-empty lift:copy-bucket)
                      buckets)))
      (when buckets
        (lift:fill-buckets lines bl :key key)
        (dolist (b bl)
          (mesg :log out "  -  ~A  ~4F%~%"
                b (/ (* 1d2 (lift:bucket-size b)) len))))
      (list* col name mdl bl))))

;;;###autoload
(defun analyse-csv (file &key plot (first-line-names t) (out *standard-output*)
                    ((:columns *columns*) *columns*)
                    ((:buckets *buckets*) *buckets*))
  "Analyse columns in the CSV file."
  (multiple-value-bind (lines len file-size names)
      (csv-read-file file :first-line-names first-line-names)
    (declare (ignore file-size))
    (let ((columns (unroll-column-specs *columns* names
                                        (length (or names (car lines))))))
      (assert columns (columns) "no interesting columns left")
      (setf (values lines len) (strings-to-nums lines columns len out))
      (values lines
              (mapcar (lambda (i)
                        (stat-column lines i *buckets* names plot file len out))
                      columns)))))


(provide :data)
;;; file data.lisp ends here
