;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :symbols-legacy-4
  (symbol-name (quote xyz))
  "XYZ")

(check-for-bug :symbols-legacy-8
  (let ((*gensym-counter* 32))
    (gensym)
    (prin1-to-string (gensym "FOO-")))
  "#:FOO-33")

(check-for-bug :symbols-legacy-14
  (let ((*gensym-counter* 32))
    (gensym)
    (prin1-to-string (gensym "garbage-")))
  #+xcl "#:|garbage|-33"
  #+(or clisp akcl allegro cmu sbcl ecls) "#:|garbage-33|"
  #-(or xcl clisp akcl allegro cmu sbcl ecls) UNKNOWN)

