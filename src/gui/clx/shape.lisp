;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-
;;; Copyright BRIAN SPILSBURY <zhivago@iglou.com>
;;; Placed in the public domain, no warranty

;;; CLX interface for Shape Extension

(in-package "XLIB" :use '(lisp))

(define-extension "SHAPE")

(export '(
  query-shape-version
  shape-mask
  shape-combine
  shape-offset
  shape-query-extents
  shape-set
  shape-union
  shape-invert
  shape-bounding
  shape-clip
  shape-notify-mask
  shape-notify

  shape-combine-mask
  shape-combine-shape
  shape-offset-shape
  shape-query-extents))

(defconstant query-shape-version  0) ; not sure about this one
(defconstant shape-rectangles     1) ; unimplemented
(defconstant shape-mask           2)
(defconstant shape-combine        3)
(defconstant shape-offset         4)
(defconstant shape-query-extents  5)
(defconstant shape-select-input   6) ; unimplemented - need to figure inputs
(defconstant shape-input-selected 7) ; unimplemented -       " "
(defconstant shape-get-rectangles 8) ; unimplemented

(defconstant shape-set              0)
(defconstant shape-union            1)
(defconstant shape-intersect        2)
(defconstant shape-subtract         3)
(defconstant shape-invert           4)

(defconstant shape-bounding         0)
(defconstant shape-clip             1)

(defconstant shape-notify-mask      0)
(defconstant shape-notify           1)

; kinds are shape-bounding, or shape-clipped
(defun shape-combine-mask (drawable dst-kind x-off y-off bitmap op)
  (let* ((display (drawable-display drawable))
         (opcode (extension-opcode display "SHAPE")))
    (with-buffer-request (display opcode :length 5)
      ((data card8) shape-mask)
      (card8  op dst-kind 0 0)
      (drawable drawable)
      (int16  x-off y-off)
      (drawable bitmap))))

; kinds are shape-bounding, or shape-clipped
(defun shape-combine-shape (drawable dst-kind x-off y-off bitmap src-kind op)
  (let* ((display (drawable-display drawable))
         (opcode (extension-opcode display "SHAPE")))
    (with-buffer-request (display opcode :length 5)
      ((data card8) shape-combine)
      (card8  op dst-kind src-kind 0)
      (drawable drawable)
      (int16  x-off y-off)
      (drawable bitmap))))

; kinds are shape-bounding or shape-clip
(defun shape-offset-shape (drawable dst-kind x-off y-off)
  (let* ((display (drawable-display drawable))
         (opcode (extension-opcode display "SHAPE")))
    (with-buffer-request (display opcode :length 4)
      ((data card8) shape-combine)
      (card8  dst-kind 0)
      (card16 0)
      (drawable drawable)
      (int16  x-off y-off))))

(defun shape-query-extents (drawable)
  (let* ((display (drawable-display drawable))
         (opcode (extension-opcode display "SHAPE")))
    (with-buffer-request-and-reply (display opcode 2 :sizes (8 16 32))
         (((data card8) shape-query-extents)
          (drawable drawable))
      (values
        (card8-get 8) ; bounding shape?
        (int16-get 12) ; bounding x
        (int16-get 14) ; bounding y
        (card16-get 16) ; bounding width
        (card16-get 18) ; bounding height

        (card8-get 9) ; bounding shape?
        (int16-get 20) ; bounding x
        (int16-get 22) ; bounding y
        (card16-get 24) ; bounding width
        (card16-get 26) ; bounding height
        ))))
