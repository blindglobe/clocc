;;; -*- Mode: CLtL -*-

;;; lcl.lisp --
;;; Liquid (nee Lucid) implementation dependencies.

;;; Copyright (c) 2000-2004 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

;;; Directory utilities

(defmethod current-directory-pathname ((cl-implementation cl.env:lucid))
  (pathname (lcl:working-directory)))


(defmethod change-current-directory ((cl-implementation cl.env:lucid)
				     (new-dir string))
  (lcl:working-directory new-dir))


(defmethod change-current-directory ((cl-implementation cl.env:lucid)
				     (new-dir pathname))
  (lcl:working-directory new-dir))


;;; end of file -- lcl.lisp --
