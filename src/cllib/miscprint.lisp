;;; print misc special opjects
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: miscprint.lisp,v 1.5 2000/05/02 15:39:14 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/miscprint.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-collect'
  (require :simple (translate-logical-pathname "cllib:simple")))

(in-package :cllib)

(export '(hash-table-keys hash-table->alist alist->hash-table print-hash-table
          print-all-ascii print-all-packages))

;;;
;;; characters
;;;

(defun ascii-char (ch &optional (str *standard-output*))
  (declare (character ch) (stream str))
  (let ((co (char-code ch)))
    (format str "\"~c\"~5t~@c~17t~:@c~27t~:c~35t ~9:o ~9:d ~9:x  ~a"
            ch ch ch ch co co co (get-macro-character ch))))

(defun print-all-ascii (&optional (str *standard-output*))
  (declare (type (or null stream) str))
  (loop :with *print-pretty* = nil
        :with st :of-type stream = (or str (make-string-output-stream))
        :initially
        (format
         st "~&char (~~c, ~~@c, ~~:@c, ~~:c)~42toct~52tdec~62thex  macro~%")
        :for co :from 0 :to char-code-limit
        :for ch :of-type (or null character) = (code-char co)
        :when ch :do (ascii-char ch st) (terpri st)
        :finally (unless str (return (get-output-stream-string st)))))

;;;
;;; packages
;;;

(defun print-package (pp &optional (out *standard-output*) verbose)
  "Print the package information.
Return 3 values: the number of accessible symbols,
the number of external symbols, and
the number of symbols without any bindings."
  (declare (type (or symbol string package) pp) (stream out))
  (let ((pa (if (packagep pp) pp
                (or (find-package pp)
                    (find-package (string-upcase (string pp))))))
        (ns 0) (ne 0) (nw 0))
    (declare (type index-t ns ne nw))
    (assert (packagep pa) (pa) " *** Package not found: `~s'" pp)
    (do-symbols (sy pa)
      (when (and (eq :internal (nth-value 1 (find-symbol (symbol-name sy) pa)))
                 (not (or (boundp sy) (fboundp sy) (symbol-plist sy))))
        (when verbose (format out " ~a" sy))
        (incf nw))
      (incf ns))
    (do-external-symbols (sy pa) (declare (ignore sy)) (incf ne))
    (format out "~& *** ~a~@[ [~{~a~^ ~}]~]:~%~@[~5tuses:~{ ~a~}~%~]~
~@[~5tused by:~{ ~a~}~%~]~5tsymbols: ~:d [external: ~:d] [wasted: ~:d]~%"
            (package-name pa) (package-nicknames pa)
            (mapcar #'package-name (package-use-list pa))
            (mapcar #'package-name (package-used-by-list pa)) ns ne nw)
    (values ns ne nw)))

(defun print-all-packages (&optional (out *standard-output*))
  "Print all packages."
  (declare (stream out))
  (let ((ns 0) (ne 0) (tn 0))
    (declare (type index-t ns ne tn))
    (dolist (pp (list-all-packages))
      (multiple-value-bind (n0 n1) (print-package pp out)
        (incf ns n0) (incf ne n1)))
    (format out " === `*package*' ===~%")
    (print-package *package* out)
    (do-all-symbols (sy) (declare (ignore sy)) (incf tn))
    (format out " === Total number of symbols: ~:d [~:d ~:d]~%" tn ns ne)))

;;;
;;; hash tables
;;;

(defun hash-table-keys (ht)
  "Return the list of all the keys in the hashtable."
  (declare (hash-table ht))
  ;; (loop :for kk :being :each :hash-key :of ht :collect kk)
  (with-collect (co)
    (with-hash-table-iterator (iter ht)
      (loop (multiple-value-bind (re kk) (iter)
              (unless re (return))
              (co kk))))))

(defun hash-table->alist (ht)
  "Return the alist with the same data as the hash-table.
Actually, the first element is the test: '(eql (key0 . val0) (key1 . val1)).
The inverse is `alist->hash-table'."
  (declare (hash-table ht))
  (cons (hash-table-test ht)
        (with-collect (co)
          (with-hash-table-iterator (iter ht)
            (loop (multiple-value-bind (re kk vv) (iter)
                    (unless re (return))
                    (co (cons kk vv))))))))

(defun alist->hash-table (alist)
  "Return the new hash-table based on this alist.
This is the inverse of `hash-table->alist'."
  (declare (list alist))
  (let ((ht (make-hash-table :test (car alist))))
    (dolist (co (cdr alist) ht)
      (setf (gethash (car co) ht) (cdr co)))))

;;;###autoload
(defun print-hash-table (ht &optional (out t))
  "Print the hash table with contents."
  (declare (hash-table ht))
  (format out "#S(hash-table :test '~s :size ~d ~s)" (hash-table-test ht)
          (hash-table-count ht) (cdr (hash-table->alist ht))))

(provide :miscprint)
;;; file miscprint.lisp ends here
