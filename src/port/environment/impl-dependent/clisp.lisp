;;; -*- Mode: CLtL -*-

;;; clisp.lisp --
;;; Haible's CLisp implementation dependencies.

;;; Copyright (c) 2000, 2001 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

(defmethod current-directory-pathname ((cl-implementation cl.env:clisp))
  (pathname (lisp:default-directory)))

;;; DEFSYSTEM utilities

;;; Nothing define for CLisp, which relies on third parties DEFSYSTEMs.

;;; end of file -- clisp.lisp --
