;;; File: <autoload.lisp - 2000-03-03 Fri 12:31:55 EST sds@ksp.com>
;;;
;;; generate and use autoloads
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;;
;;; $Id: autoload.lisp,v 1.3 2000/03/03 17:34:09 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/autoload.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `skip-to-line', `timestamp'
  (require :fileio (translate-logical-pathname "cllib:fileio")))

(in-package :cllib)

(export '(autoload autoload-generate))

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;;
;;; autoload
;;;

(defparameter *autoload-defun* '
  ;; this will be written into `auto.lisp'
(defun autoload (symb file &optional comment)
  "Declare symbol SYMB to call a function defined in FILE."
  (declare (symbol symb) (type (or simple-string null) comment))
  (export (list symb))
  (unless (fboundp symb)
    (setf (fdefinition symb)
          (lambda (&rest args)
            (setf (documentation symb 'function) nil)
            (fmakunbound symb)
            (format t "; ~s is being autoloaded from `~a'~%" symb file)
            (require file (translate-logical-pathname
                           (concatenate 'string "cllib:" file)))
            (apply (fdefinition symb) args))
          (documentation symb 'function)
          (format nil "Autoloaded (from ~a)~@[:~%~a~]" file comment))))

)

(defgeneric autoload-stream (in out log)
  (:documentation "Generate the autoloads writing into a stream.")
  (:method ((in string) (out stream) (log t))
    (autoload-stream (pathname in) out log))
  (:method ((in sequence) (out stream) (log t))
    (reduce #'+ in :key (lambda (i0) (autoload-stream i0 out log))))
  (:method ((in pathname) (out stream) (log t))
    (with-open-file (ins in :direction :input)
      (format out "~&;;; file ~s, ~:d bytes~%" in (file-length ins))
      (format log "~&~s [~:d bytes] --> " in (file-length ins)) (force-output)
      (loop :while (ignore-errors (skip-to-line ins ";;;###autoload"))
            :count t :into total :do
            (let* ((*package* (find-package :cllib))
                   (form (read ins)) (name (second form))
                   (doc (case (car form)
                          ((defun defsubst)
                           (when (stringp (fourth form)) (fourth form)))
                          (defgeneric
                              (cadr (assoc :documentation (cdddr form))))
                          (t (error 'case-error :proc 'autoload-stream :args
                                    (list 'form (car form) 'defun 'defsubst
                                          'defgeneric))))))
              (format out "(autoload '~s ~s ~s)~%"
                      name (pathname-name in) doc))
            :finally (progn (format log "~d autoload~:p~%" total) (terpri out)
                            (return total))))))

(defun autoload-generate (in out &optional (log t))
  "Generate the autoloads, indicated by ';;;###autoload'."
  (with-open-file (outs out :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
    (format outs ";;; autoloads generated on ~s~%(in-package :cllib)~2%~s~2%"
            (timestamp) *autoload-defun*)
    (let ((tot (autoload-stream in outs log)))
      (format log "wrote ~d autoload~:p to ~s (~:d bytes)~%"
              tot out (file-length outs))
      tot)))

(provide "autoload")
;;; file autoload.lisp ends here
