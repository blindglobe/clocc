;;; -*- Mode: Lisp; Package: CLX-VM;  -*-


;; This file contains the MP support for CLX VM
;; copyright 2003, Peter Van Eynde (BSD like license)
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.
;; 3. The name of the author may not be used to endorse or promote
;;    products derived from this software without specific prior
;;    written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
#+cmu
(ext:file-comment
 "$Header: /cvsroot/clocc/clocc/src/gui/clx/clx-vm/threading.lisp,v 1.1 2003/02/24 11:05:33 pvaneynd Exp $")

(in-package :clx-vm)

;; The basic idea is very simple:
;; we don't do without-scheduling anymore...
;; we should support the non-MP cmucl system serve-event in a
;; portable way
;;
;; so: what do we need:
;;
;; creating a X message handler
;;
;; creating mutex
;; take mutex
;; release mutex
;; wait for mutex
;;

;; That's all :-)


;;; CLX Event handling
;;
;; We use an abstraction of MP to describe event handing in X:
;; the application can push an event handler on a queue, when an
;; X event arrives all the handlers on the queue are called in order
;; until one returns true. The handlers are called in a wrapper
;; that flushes all errors.
;;
;; We use a queue so the programmer doesn't have to worry if s/he
;; needs to restore a handler another piece of the program installed
;; before.

(defun push-clx-event-handler (display handler)
  "This function pushes HANDLER on the wake up queue for activity from DISPLAY.
The system will call the HANDLERs in the queue when there is input from the
X windows stream related to DISPLAY. If a handler returns true
the event is considered handled.

The handler is called with the object of the event ??? XXX 

All errors during the execution of HANDLER will be flushed.

This function returns nothing."
  (values))

(defun pop-clx-event-handling (display handler) 
  "This function remove HANDLER from the event queue  for DISPLAY.
Is this HANDLER was not installed nothing happens.

This function returns nothing."
  (values))

(defun with-clx-event-handling ((display handler) &body body)
  "This macro evaluates BODY in a block where the HANDLER gets woken up with activity on DISPLAY.
It is just an easy wrapper of enable-clx-event-handling."
  (let ((handler-symbol (make-symbol "clx-event-handler")))
    `(let ((,handler-symbol ,handler))
      (unwind-protect
	   (progn
	     (push-clx-event-handling ,display ,handler-symbol)
	     ,@body)
	(pop-clx-event-handling ,display ,handler-symbol)))))


;; now we define a mutex:

(defclass mutex ()
  ((name :reader mutex-name
	 :initarg :name)
   (value :reader mutex-value
	  :initarg :value)
   (taken-p :reader mutex-taken-p
	   :initform nil)))

(defun make-mutex (name &optional (value nil))
  "Makes a mutex with the given NAME and optional VALUE.
Returns the new mutex")

(defun take-mutex (mutex &optional
			 (put-value t put-value-p)
			 (timeout 0 timeout-p))
  "Tries to take the MUTEX, putting PUT-VALUE as value and waiting for the mutex TIMEOUT seconds.
Returns the mutex on success, nil on failure.")

(defun release-mutex (mutex &optional (put-value t put-value-p))
  "Releases the MUTEX, putting PUT-VALUE as new value")

(define-condition Mutex-Timeout ()
  ((mutex :reader mutex-timeout-mutex
	  :initarg :mutex))
  (:report
   (lambda (condition stream)
     (format stream "Got a timeout while trying to take the mutex ~A"
	     (mutex-timeout-mutex condition)))))

(defmacro with-mutex-taken ((mutex &optional
				   (put-value t put-value-p)
				   (timeout 0 timeout-p))
			    &body body)
  "Evaluates BODY with MUTEX held.
Will put PUT-VALUE into the value field during the execution, setting it to
NIL on exit. Will wait TIMEOUT time for the lock, throwing an
error of type MUTEX-TIMEOUT on timeout.")


				   


  




