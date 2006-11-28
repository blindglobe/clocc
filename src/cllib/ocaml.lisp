;;; Read Ocaml Sexp
;;; http://www.ocaml.info/home/ocaml_sources.html#toc8
;;; http://www.janestcapital.com/ocaml/index.html
;;;
;;; Copyright (C) 2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: ocaml.lisp,v 2.3 2006/11/28 05:21:03 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/ocaml.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  (require :port-mop (translate-logical-pathname "port:mop")))

(in-package :cllib)

;;;
;;; sexp parsing
;;;

(defmacro def-sexp-to (class-name)
  "Define a function `sexp-to-...'."
  (let* ((class (find-class class-name))
         (slots (port:class-slots class))
         (names (mapcar #'port:slot-definition-name slots)))
    (labels ((parser-name (name) (intern (format nil "SEXP-TO-~S" name)))
             (parser-form (name arg)
               (let ((parser (parser-name name)))
                 (if (fboundp parser) `(,parser ,arg) arg)))
             (parser-function (name)
               (let ((parser (parser-name name)))
                 (if (fboundp parser) (fdefinition parser) #'identity))))
      `(defun ,(parser-name class-name) (sexp)
         (let ,(mapcar #'port:slot-definition-name slots)
           (dolist (pair sexp)
             (ecase (car pair)
               ,@(mapcar
                  (lambda (slot name)
                    `(,name
                      (setq ,name
                            ,(let ((type (port:slot-definition-type slot)))
                               (if (symbolp type)
                                   (parser-form type '(second pair))
                                   (ecase (first type)
                                     (array `(map 'vector ,(parser-function
                                                            (second type))
                                                  (second pair)))))))))
                  slots names)))
           (,(sys::structure-kconstructor class)
             ,@(mapcan (lambda (name) (list (cllib:kwd name) name)) names)))))))

(defun fix-slot (name line &key (start 0) (end (length line)))
  "Convert a `list' representation to a `string' one;
\(NAME(.....)) --> (NAME\".....\")"
  (loop :with len = (length name)
    :for pos = (search name line :start2 start)
    :while (and pos (< 0 pos (- end len 1))
                (char= #\( (char line (1- pos))) ; must be '(foo('
                (char= #\( (char line (+ pos len)))) :do
    (incf pos len)
    (setf (aref line pos) #\"
          pos (position #\) line :start pos)
          (aref line pos) #\"
          start pos))
  line)

(provide :ocaml)
;;; file ocaml.lisp ends here
