;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; predefined-specialized-components.lisp --

(in-package "MK4")


;;; Derived and specialized components.

(defclass c-file (file c-language-mixin)
  ()
  )

(defclass c-header-file (c-file)
  ()
  (:default-initargs :source-extension "h"))

(defclass c-executable (c-file executable-file)
  ()
  (:default-initargs :link t))

(defclass c-object (c-file object-file)
  ()
  (:default-initargs :link t))


;;; Notes:
;;;
;;; 2002-05-22 Marco Antoniotti
;;; Originally these components were defined to have :language :c.
;;; This is not necessarily the case.
;;;
;;; This realization is a good thing, because it settles the dilemma of
;;; the inherited language mixin vs. the language slot.  The language
;;; slot is what I want.

(defclass statically-linked-library (library)
  ()
  (:default-initargs :binary-extension (static-library-extension))
  )

(defclass dynamically-linked-library (library)
  ()
  (:default-initargs :binary-extension (shared-library-extension))
  )

;;; end of file -- predefined-specialized-components.lisp --
