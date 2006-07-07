;;; Data Analysis and Visualization
;;;
;;; Copyright (C) 2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: data.lisp,v 1.2 2006/07/07 20:11:58 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/data.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `mdl'
  (require :cllib-math (translate-logical-pathname "cllib:math"))
  ;; `with-csv'
  (require :cllib-csv (translate-logical-pathname "cllib:csv"))
  ;; `plot-histogram'
  (require :cllib-gnuplot (translate-logical-pathname "cllib:gnuplot")))

(in-package :cllib)

(export '(analyse-csv))

;;;###autoload
(defun analyse-csv (file &key plot (first-line-names t) (out *standard-output*))
  "Analyse columns in the CSV file."
  (multiple-value-bind (lines len file-size names)
      (csv-read-file file :first-line-names first-line-names)
    (declare (ignore file-size))
    (let ((ncol (length  (or names (car lines)))))
      (with-timing (:out out)
        (let ((drop 0))
          (mesg :log out "Converting strings to numbers...")
          (setq lines
                (delete-if-not
                 (lambda (v)
                   (loop :for i :from 0 :below ncol
                     :for num = (read-from-string (aref v i))
                     :always (if (numberp num) (setf (aref v i) num)
                                 (warn "~D dropped ~S" (incf drop) v))))
                 lines))
          (mesg :log out "dropped ~:D lines (out of ~:D, ~G%)"
                drop len (/ (* 1d2 drop) len))))
      (dotimes (i ncol)
        (let ((name (if names (aref names i) (format nil "C~D")))
              (key (lambda (v) (aref v i))))
          (mesg :log out "~3D ~15@A" i name)
          (if plot
              (plot-histogram lines 100 :key key :plot plot
                              :xlabel (format nil "~A -- ~A(~D)" file name i))
              (mesg :log out " ~A~%"
                    (standard-deviation-mdl lines :key key))))))))


(provide :data)
;;; file data.lisp ends here
