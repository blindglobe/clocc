;;; -*- Mode: CLtL -*-

(in-package "MK4")

(defun run-program (program &rest arguments)
  (system (format nil "~A~@[~{ ~A~}~]" program arguments)))

;;; end of file -- kcl-derivatives.lisp --
