(in-package :cl-user)


(defclass sub1 (super1)())

(defun fooey ()
  (make-instance 'sub1))

