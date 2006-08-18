;;; Data Analysis and Visualization
;;;
;;; Copyright (C) 2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: data.lisp,v 1.20 2006/08/18 02:29:59 sds Exp $
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

(export '(analyse-csv *buckets* *columns* evaluate-predictor ensure-buckets
          stat-column sc-pos sc-name sc-mdl sc-median sc-buckets sc-table
          table table-path table-lines table-stats table-names aref-i
          compress-tables *tables* table-lines$ show-sc show-sc-list
          column-name-sc
          table-stat-column ensure-table-stat-column table-column-pos
          table-stats-refresh column-histogram add-column plot-columns))

(defcustom *buckets* (or null (cons lift:bucket)) ()
  "The list of buckets to fill in `analyse-csv'.")
(defcustom *columns* (or (eql t) (cons (or fixnum string (eql NOT)))) t
  "The list of column specs to study in `analyse-csv'.")

(defun column-spec-list (col-specs names ncol)
  "Return the list of positions corresponding to the column specifiers."
  (delete
   nil (mapcar (lambda (spec)
                 (etypecase spec
                   (string
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
                     (column-spec-list (rest col-specs) names ncol)))
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

(defvar *min-name-length* 5)
(defun max-name-length (names)
  (reduce #'max names :key #'length :initial-value *min-name-length*))
(defun column-name (names col)
  (if names (aref names col) (format nil "C~D" col)))

(defun strings-to-nums (lines col-specs &key names (len (length lines))
                        (max-name-length (max-name-length names))
                        (out *standard-output*))
  "Convert some strings to numbers, in place."
  (let ((dnum (make-array (1+ (reduce #'max col-specs)) :initial-element 0))
        (drop 0) (total (length lines)))
    (with-timing (:out out :count line :units "records"
                  :progress *csv-progress* :progress-1 *csv-progress-1*)
      (mesg :log out "Converting strings to numbers...")
      (setq lines
            (delete-if-not
             (lambda (v)
               (incf line) (progress (/ line total))
               (dolist (i col-specs t)
                 (handler-bind ((error (lambda (c)
                                         (warn "~A -- line dropped" c)
                                         (incf (aref dnum i))
                                         (incf drop) (return nil))))
                   (setf (aref v i) (numeric v i names)))))
             lines))
      (if (zerop drop) (mesg :log out "done")
          (loop :for i :in col-specs :for d = (aref dnum i) :unless (zerop d)
            :do (mesg :log out "~%~3D ~V@A:  ~:D lines dropped"
                      i max-name-length (column-name names i) d)
            :finally (mesg :log out "~%...dropped ~:D lines (out of ~:D, ~4F%)"
                           drop len (/ (* 1d2 drop) len))))
      (values lines (- len drop)))))

(defstruct (stat-column (:conc-name sc-))
  (table nil :type (or nil table))
  (pos 0 :type index-t)
  (name "" :type string)
  (mdl +bad-mdl+ :type mdl)
  (buckets nil :type list)      ; of buckets
  (median nil :type (or null real)))
(defmethod print-object ((sc stat-column) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (sc out :type t)
        (format out "~W ~:D~@[ ~W~]" (sc-name sc) (sc-pos sc)
                (let ((tab (sc-table sc))) (and tab (table-path tab)))))))
(defun show-sc (sc &key (width 0) (out *standard-output*))
  "Print a STAT-COLUMN nicely, on its own line or to string."
  (format out "~:[~;~&;; ~]~3D ~V@A ~A~@[  ~F~]~:[~;~%~]"
          out (sc-pos sc) width (sc-name sc) (sc-mdl sc) (sc-median sc) out))
(defun show-sc-list (sc-list &key (out *standard-output*))
  "Print a list of STAT-COLUMNs an as aligned table."
  (let ((width (reduce #'max sc-list :key (port:compose length sc-name)
                       :initial-value *min-name-length*)))
    (dolist (r sc-list) (show-sc r :width width :out out))))
(defun aref-i (i) (lambda (v) (aref v i)))
(defun stat-column (lines col names &key (out *standard-output*)
                    ((:buckets *buckets*) *buckets*)
                    (max-name-length (max-name-length names)) table)
  (let* ((name (column-name names col)) (key (aref-i col))
         (mdl (standard-deviation-mdl lines :key key)))
    (mesg :log out "~3D ~V@A ~A~%" col max-name-length name mdl)
    (make-stat-column :pos col :name name :mdl mdl :table table
                      :buckets (lift:bucketize lines *buckets*
                                               :key key :out out))))
(defun ensure-buckets (sc lines &key (out *standard-output*)
                       ((:buckets *buckets*) *buckets*))
  "When buckets are not present in a STAT-COLUMN, add it."
  (unless (sc-buckets sc) ; no buckets - create them!
    (show-sc sc :out out)
    (setf (sc-buckets sc) (lift:bucketize lines *buckets* :out out
                                          :key (aref-i (sc-pos sc))))))

(defstruct table
  (path "" :type (or string pathname)) ; file containing the table
  (lines () :type (or list integer)) ; lines or line count
  (stats () :type list)              ; of stat-column
  (names #() :type vector))          ; of column names
(defun table-lines$ (table)
  "Return the number of lines in the TABLE."
  (let ((lines (table-lines table)))
    (etypecase lines
      (integer lines)
      (list (length lines)))))
(defun table-column-pos (name table)
  "Return the position of the column NAME in the TABLE."
  (or (position name (table-names table) :test #'string=)
      (error "~S: no ~S in ~S" 'table-column-pos name table)))

(defmethod print-object ((tab table) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (tab out :type t)
        (format out "~W ~:Dx~:D" (table-path tab) (table-lines$ tab)
                (length (table-names tab))))))
(defvar *tables* nil "The list of currently loaded tables")
(defun compress-tables ()
  (dolist (tab *tables* (port:gc))
    (etypecase (table-lines tab)
      (integer (mesg :log t "~&no lines in <~S> - was ~:D~%"
                     (table-path tab) (table-lines tab)))
      (list (let ((len (length (table-lines tab))))
              (mesg :log t "~&removed ~:D lines from <~S>~%"
                    len (table-path tab))
              (setf (table-lines tab) len))))))

(defun table-stat-column (pos-or-name table &key (out *standard-output*)
                          (buckets
                           (let ((stats (table-stats table)))
                             (if stats (sc-buckets (car stats)) *buckets*)))
                          (max-name-length
                           (max-name-length (table-names table))))
  "Return a freshly computed STAT-COLUMN."
  (stat-column (table-lines table)
               (etypecase pos-or-name
                 (integer pos-or-name)
                 (string (table-column-pos pos-or-name table)))
               (table-names table) :buckets buckets
               :out out :max-name-length max-name-length :table table))
(defun ensure-table-stat-column (name table)
  "Make shure that there is a STAT-COLUMN for NAME in TABLE and return it."
  (or (find name (table-stats table) :test #'string= :key #'sc-name)
      (car (push (table-stat-column name table) (table-stats table)))))

(defun column-histogram (sc nbins &rest plot-opts)
  (apply #'plot-histogram (table-lines (sc-table sc)) nbins
         :key (aref-i (sc-pos sc)) :mdl (sc-mdl sc)
         :xlabel (format nil "~A -- ~A(~D)" (table-path (sc-table sc))
                         (sc-name sc) (sc-pos sc))
         plot-opts))

;;;###autoload
(defun analyse-csv (file &key (first-line-names :default)
                    (out *standard-output*)
                    ((:columns *columns*) *columns*)
                    ((:buckets *buckets*) *buckets*))
  "Analyse columns in the CSV file."
  (multiple-value-bind (lines len file-size names)
      (csv-read-file file :first-line-names first-line-names)
    (declare (ignore file-size))
    (let* ((columns (unroll-column-specs *columns* names
                                         (length (or names (car lines)))))
           (max-name-length
            (if names
                (reduce #'max columns :key (lambda (i) (length (aref names i)))
                        :initial-value *min-name-length*)
                *min-name-length*))
           (tab (make-table :path file :names names)))
      (assert columns (columns) "no interesting columns left")
      (setf (values (table-lines tab) len)
            (strings-to-nums lines columns :names names
                             :max-name-length max-name-length
                             :len len :out out))
      (push tab *tables*)
      (setf (table-stats tab)
            (mapcar (lambda (i)
                      (table-stat-column tab i :out out
                                         :max-name-length max-name-length))
                    columns))
      tab)))

(defun table-stats-refresh (table)
  "Update TABLE-STATS."
  (let ((stats (table-stats table)))
    (map-into stats (lambda (sc) (table-stat-column table (sc-pos sc)))
              stats)))

(defun add-column (table1 function name)
  "Add a new column named NAME to the table.
The value is computed by calling FUNCTION on the existing row vector.
Everything is allocated anew."
  (let* ((table2 (copy-table table1))
         (names (table-names table2)) (len (length names))
         (stats (table-stats table2)))
    (setf (table-names table2) (concatenate 'vector names (vector name))
          (table-lines table2)
          (mapcar (lambda (v)
                    (concatenate 'vector v (vector (funcall function v))))
                  (table-lines table2))
          (table-stats table2)
          (nconc (mapcar (lambda (sc)
                           (let ((cs (copy-stat-column sc)))
                             (setf (sc-table cs) table2)
                             cs))
                         stats)
                 (list (stat-column
                        (table-lines table2) len
                        (table-names table2) :table table2
                        :buckets (and stats (sc-buckets (car stats)))))))
    (push table2 *tables*)
    table2))

(defun column-name-sc (obj table)
  "Return two values: column position and column name in table."
  (etypecase obj
    (integer
     (let ((name (aref (table-names table) obj)))
       (values obj name (ensure-table-stat-column name table))))
    (string
     (values (position obj (table-names table) :test #'string=) obj
             (ensure-table-stat-column obj table)))
    (stat-column
     (unless (eq table (sc-table obj))
       (cerror "ignore and proceed" "~S(~S): ~S /= ~S"
               'column-name-sc obj (sc-table obj) table)
       (let ((pos (column-name-sc (sc-name obj) table)))
         (unless (= (sc-pos obj) pos)
           (cerror "ignore and proceed" "~S(~S): ~D /= ~D"
                   'column-name-sc obj (sc-pos obj) pos)))
       (let ((name (aref (table-names table) (sc-pos obj))))
         (unless (string= (sc-name obj) name)
           (cerror "ignore and proceed" "~S(~S): ~S /= ~S"
                   'column-name-sc obj (sc-name obj) name))))
     (values (sc-pos obj) (sc-name obj) obj))))

(defun plot-columns (table col1 col2 &rest options)
  "Plot one column vs the other."
  (multiple-value-bind (c1 n1) (column-name-sc col1 table)
    (multiple-value-bind (c2 n2) (column-name-sc col2 table)
      (apply #'plot-lists-arg
             (list (cons (format nil "~A vs ~A" n1 n2)
                         (mapcar (lambda (v)
                                   (cons (aref v c1) (aref v c2)))
                                 (table-lines table))))
             (append options
                     (list :xlabel n1 :ylabel n2
                           :data-style :points
                           :title (format nil "~A (~:D)" (table-path table)
                                          (table-lines$ table))))))))

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
