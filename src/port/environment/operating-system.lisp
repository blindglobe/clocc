;;; -*- Mode: CLtL -*-

;;; operating-system.lisp --
;;;
;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defclass operating-system (software)
  ((type :reader operating-system-type :reader os-type)
   (version :reader operating-system-version :reader os-version)
   (tag :reader operating-system-feature-tag :reader os-feature-tag
	:initarg :feature-tag
	:type symbol)
   )
  (:documentation "The CL.ENVIRONMENT Operating System Class."))
	   

;;; Known Operating Systems.

(defclass unix (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT Unix Operating System Class.")
  (:default-initargs :type "UNIX" :version "" :feature-tag :unix))


(defclass SunOS (unix)
  ()
  (:documentation "The CL.ENVIRONMENT SunOS Operating System Class.")
  (:default-initargs :type "SunOS" :version "4.1.1" :feature-tag :sun-os))


(defclass Solaris (unix)
  ()
  (:documentation "The CL.ENVIRONMENT Solaris Operating System Class.")
  (:default-initargs :type "Solaris" :version "7.x" :feature-tag :solaris))

(defclass HP-UX (unix)
  ()
  (:documentation "The CL.ENVIRONMENT HP-UX Operating System Class.")
  (:default-initargs :type "HP-UX" :version "10.x" :feature-tag :hp-ux))

(defclass Irix (unix)
  ()
  (:documentation "The CL.ENVIRONMENT Irix Operating System Class.")
  (:default-initargs :type "Irix" :version "" :feature-tag :irix))

(defclass Linux (unix)
  ()
  (:documentation "The CL.ENVIRONMENT Linux Operating System Class.")
  (:default-initargs :type "Linux" :version "" :feature-tag :linux))


(defclass MacOS (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT MacOS Operating System Class.")
  (:default-initargs :type "MacOS" :version "8.x" :feature-tag :mac-os))

(defclass Genera (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT Genera Operating System Class.")
  (:default-initargs :type "Genera" :version "8.x" :feature-tag :genera))

(defclass MS-DOS (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT MS-DOS Operating System Class.")
  (:default-initargs :type "MS-DOS" :version "" :feature-tag :ms-dos))

(defclass Windows (operating-system)	; Maybe (MS-DOS) would be better!
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows Operating System Class.")
  (:default-initargs :type "Windows (generic)"
                     :version ""
		     :feature-tag :ms-windows))


(defclass W32 (windows)	; Maybe (MS-DOS) would be better!
  ()
  (:documentation
   "The CL.ENVIRONMENT Generic MS Windows (32 bits) Operating System Class.")
  (:default-initargs :type "Windows (generic, 32 bits)"
                     :version ""
		     :feature-tag :ms-windows-32))


(defclass Windows-95 (W32)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows 95 Operating System Class.")
  (:default-initargs :type "Windows 95"
                     :version ""
		     :feature-tag :windows-95))

(defclass Windows-98 (Windows-95)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows 95 Operating System Class.")
  (:default-initargs :type "Windows 98" :version "" :feature-tag :windows-98))

(defclass WNT (W32)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows NT Operating System Class.")
  (:default-initargs :type "Windows NT"
                     :version "4.1"
		     :feature-tag :windows-nt))

(defclass WNT-TSE (WNT)
  ()
  (:documentation
   "The CL.ENVIRONMENT MS Windows NT Terminal Server Operating System Class.")
  (:default-initargs :type "Windows NT"
                     :version "4.1"
		     :feature-tag :windows-nt-tse))

(defclass Windows-2000 (WNT Windows-98)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows NT Operating System Class.")
  (:default-initargs :type "Windows 2000"
                     :version "4.1"
		     :feature-tag :windows-2000))



;;; end of file -- operating-system.lisp
