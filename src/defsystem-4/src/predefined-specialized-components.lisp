;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; predefined-specialized-components.lisp --

(in-package "MK4")


;;; Derived and specialized components.

(defclass c-file (file c-language-mixin)
  ()
  )

(defclass c-header-file (c-file c-language-mixin)
  ()
  (:default-initargs :source-extension "h"))

(defclass c-executable (c-file)
  ()
  (:default-initargs :link t))


(defclass statically-linked-library (library c-language-mixin)
  ()
  (:default-initargs
    :language :c
    :binary-extension (static-library-extension))
  )

(defclass dynamically-linked-library (library c-language-mixin)
  ()
  (:default-initargs
    :language :c
    :binary-extension (shared-library-extension))
  )

;;; end of file -- predefined-specialized-components.lisp --
