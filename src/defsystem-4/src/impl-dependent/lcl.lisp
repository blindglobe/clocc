;;; -*- Mode: CLtL -*-

(in-package "MK4")

(shadow '(lcl:run-program))

(defun run-program (program &rest arguments)
  (lcl:run-program program :arguments arguments))

;;; end of file -- lcl.lisp --
