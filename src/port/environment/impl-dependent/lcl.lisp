;;; -*- Mode: CLtL -*-

;;; lcl.lisp --
;;; Liquid (nee Lucid) implementation dependencies.

;;; Copyright (c) 2000, 2001 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

;;; Directory utilities

(defmethod current-directory-pathname ((cl-implementation cl.env:cmucl))
  (pathname (working-directory)))

;;; end of file -- cmucl.lisp --
