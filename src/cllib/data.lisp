;;; Data Analysis and Visualization
;;;
;;; Copyright (C) 2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: data.lisp,v 1.8 2006/07/12 20:58:44 sds Exp $
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

(export '(analyse-csv *buckets* *columns* evaluate-predictor))

(defcustom *buckets* (or null (cons lift:bucket)) ()
  "The list of buckets to fill in `analyse-csv'.")
(defcustom *columns* (or (eql t) (cons (or fixnum string symbol))) t
  "The list of column specs to study in `analyse-csv'.")

(defun column-spec-list (col-specs names ncol)
  "Return the list of positions corresponding to the column specifiers."
  (delete
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
               col-specs)))

(defun unroll-column-specs (col-specs names ncol)
  "Turn `columns' into a list of `interesting' indexes."
  (etypecase col-specs
    (null (error "empty column selection"))
    ((eql t) (loop :for i :from 0 :below ncol :collect i))
    ((cons (eql not))           ; exclude specified columns
     (set-difference (loop :for i :from 0 :below ncol :collect i)
                     (column-spec-list col-specs names ncol)))
    (cons                       ; include specified columns
     (column-spec-list col-specs names ncol))))

(defun numeric (v i &optional names
                &aux (*read-default-float-format* 'double-float))
  (let ((n (read-from-string (aref v i))))
    (if (numberp n)
        (if (< (abs n) (/ *num-tolerance*)) n
            (cerror "drop the whole line"
                    "extreme value ~S in ~S at ~:D~@[ (~A)~]"
                    n v i (and names (aref names i))))
        (cerror "drop the whole line" "non-number ~S in ~S at ~:D~@[ (~A)~]"
                n v i (and names (aref names i))))))

(defun strings-to-nums (lines col-specs &key names (len (length lines))
                        (out *standard-output*))
  "Convert some strings to numbers, in place."
  (with-timing (:out out)
    (let ((drop 0))
      (mesg :log out "Converting strings to numbers...")
      (setq lines
            (delete-if-not
             (lambda (v)
               (dolist (i col-specs t)
                 (handler-bind ((error (lambda (c)
                                         (warn "~A -- line dropped" c)
                                         (incf drop) (return nil))))
                   (setf (aref v i) (numeric v i names)))))
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
      (setf (values lines len) (strings-to-nums lines columns :names names
                                                :len len :out out))
      (values lines
              (mapcar (lambda (i)
                        (stat-column lines i *buckets* names plot file len out))
                      columns)))))

;;;###autoload
(defun evaluate-predictor (file &optional (out *standard-output*))
  "Evaluate the quality of the predictor the produced the file.
File: CSV, 1st column: actuals, 2nd column: predicted."
  (let ((data (with-collect (coll)
                (with-csv (vec file)
                  (coll (cons (numeric vec 0) (numeric vec 1)))))))
    (mesg :log out "actual: ~a~%" (standard-deviation-mdl data :key #'car))
    (mesg :log out "pred  : ~a~%" (standard-deviation-mdl data :key #'cdr))
    (multiple-value-bind (co m0 m1 d0 d1 n) (cov data)
      (let ((s0 (sqrt d0)) (s1 (sqrt d1)))
        (mesg :log out "actual: mean=~9f std=~9f n=~9:d~%pred  : mean=~9f std=~9f corr=~f~%"
              m0 s0 n m1 s1 (/ co (* s0 s1)))
        (let* ((deviation (mapcar (lambda (x) (- (car x) (cdr x))) data))
               (mdl (standard-deviation-mdl deviation)))
          (mesg :log out "diff  : ~a  r2=~f~%"
                mdl (/ (- s1 (mdl-sd mdl)) s1)))))))

(provide :data)
;;; file data.lisp ends here
