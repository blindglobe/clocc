;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :streamslong-legacy-4
 (read-from-string "123")
 123)

(check-for-bug :streamslong-legacy-8
 (prin1-to-string 123)
 "123")

(check-for-bug :streamslong-legacy-12
 (let ((*a*
	(make-array 10. :element-type 'character
		    :fill-pointer 0)))
   (format *a* "XXX"))
 nil)

(check-for-bug :streamslong-legacy-19
 (let ((*a*
	(make-array 10. :element-type 'character
		    :fill-pointer 0)))
   (format *a* "XXX")   
   *a*)
 "XXX")

#+xcl
(check-for-bug :streamslong-legacy-28
 (sys::check-stream-system)
 #+xcl t)

