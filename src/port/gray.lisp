;;; Gray streams
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: gray.lisp,v 1.1 2000/03/09 20:27:33 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/gray.lisp,v $
;;; $Log: gray.lisp,v $
;;; Revision 1.1  2000/03/09 20:27:33  sds
;;; Gray streams unification
;;;
;;;

(eval-when (compile load eval)
  #-(or allegro clisp cmu)
  (error 'not-implemented :proc "Gray streams")
  (require :ext (translate-logical-pathname "clocc:src;port;ext")))

(in-package #+allegro :excl #+clisp :lisp #+cmu :ext)

(let ((gray-symbols
       ;; Classes
       '(FUNDAMENTAL-STREAM FUNDAMENTAL-INPUT-STREAM FUNDAMENTAL-OUTPUT-STREAM
         FUNDAMENTAL-CHARACTER-STREAM FUNDAMENTAL-BINARY-STREAM
         FUNDAMENTAL-CHARACTER-INPUT-STREAM FUNDAMENTAL-CHARACTER-OUTPUT-STREAM
         FUNDAMENTAL-BINARY-INPUT-STREAM FUNDAMENTAL-BINARY-OUTPUT-STREAM
         ;; Character input
         STREAM-READ-CHAR STREAM-UNREAD-CHAR STREAM-READ-CHAR-NO-HANG
         STREAM-PEEK-CHAR STREAM-LISTEN STREAM-READ-LINE STREAM-CLEAR-INPUT
         ;; Character output
         STREAM-WRITE-CHAR STREAM-LINE-COLUMN STREAM-START-LINE-P
         STREAM-WRITE-STRING STREAM-TERPRI STREAM-FRESH-LINE
         STREAM-FINISH-OUTPUT STREAM-FORCE-OUTPUT STREAM-CLEAR-OUTPUT
         STREAM-ADVANCE-TO-COLUMN
         ;; Binary streams
         STREAM-READ-BYTE STREAM-WRITE-BYTE)))
  (import gray-symbols :port)
  (export gray-symbols :port))

(provide :gray)
;;; file gray.lisp ends here
