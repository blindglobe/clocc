;;; Shell Access
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: shell.lisp,v 1.6 2000/04/10 18:35:43 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/shell.lisp,v $

(eval-when (compile load eval)
  (require :ext (translate-logical-pathname "clocc:src;port;ext")))

(in-package :port)

(export '(run-prog pipe-output pipe-input close-pipe with-open-pipe))

;;;
;;; Shell interface
;;;

(defun run-prog (prog &rest opts &key args wait &allow-other-keys)
  "Common interface to shell. Doesn't return anything useful."
  #+(or clisp gcl lispworks) (declare (ignore wait))
  (remf opts :args) (remf opts :wait)
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+clisp (apply #'lisp:run-program prog :arguments args opts)
  #+cmu (ext:run-program prog args :wait wait)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (sys::call-system (format nil "~a~{ ~a~}" prog args))
  #-(or allegro clisp cmu gcl liquid lispworks)
  (error 'not-implemented :proc (list 'run-prog prog opts)))

(defun pipe-output (prog &rest args)
  "Return an output stream which will go to the command."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :input :stream :wait nil)
  #+clisp (lisp:make-pipe-output-stream (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-input (ext:run-program prog args :input :stream
                                            :output t :wait nil))
  #+gcl (si::fp-input-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :output)
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'pipe-output prog args)))

(defun pipe-input (prog &rest args)
  "Return an input stream from which the command output will be read."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :output :stream :wait nil)
  #+clisp (lisp:make-pipe-input-stream (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-output (ext:run-program prog args :output :stream
                                             :input t :wait nil))
  #+gcl (si::fp-output-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :input)
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'pipe-input prog args)))

;;; Allegro CL: a simple `close' does NOT get rid of the process.
;;; The right way, of course, is to define a Gray stream `pipe-stream',
;;; define the `close' method and use `with-open-stream'.
;;; Unfortunately, not every implementation supports Gray streams, so we
;;; have to stick with this to further the portability.

(defun close-pipe (stream)
  "Close the pipe stream."
  (declare (stream stream))
  (close stream)
  #+allegro (sys:reap-os-subprocess))

(defmacro with-open-pipe ((pipe open) &body body)
  "Open the pipe, do something, then close it."
  `(let ((,pipe ,open))
    (declare (stream ,pipe))
    (unwind-protect (progn ,@body)
      (close-pipe ,pipe))))

(provide :shell)
;;; file shell.lisp ends here
