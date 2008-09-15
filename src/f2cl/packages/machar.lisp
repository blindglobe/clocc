(defun machar (&optional ibeta it irnd ngrd machep negep iexp minexp 
	       maxexp eps epsneg xmin xmax)
  "Lisp equivalent of machar.  Arguments are optional

      IBETA   - the radix for the floating-point representation
      IT      - the number of base IBETA digits in the floating-point
                significand
      IRND    - 0 if floating-point addition chops
                1 if floating-point addition rounds, but not in the
                  IEEE style
                2 if floating-point addition rounds in the IEEE style
                3 if floating-point addition chops, and there is
                  partial underflow
                4 if floating-point addition rounds, but not in the
                  IEEE style, and there is partial underflow
                5 if floating-point addition rounds in the IEEE style,
                  and there is partial underflow
      NGRD    - the number of guard digits for multiplication with
                truncating arithmetic.  It is
                0 if floating-point arithmetic rounds, or if it
                  truncates and only  IT  base  IBETA digits
                  participate in the post-normalization shift of the
                  floating-point significand in multiplication;
                1 if floating-point arithmetic truncates and more
                  than  IT  base  IBETA  digits participate in the
                  post-normalization shift of the floating-point
                  significand in multiplication.
      MACHEP  - the largest negative integer such that
                1.0+FLOAT(IBETA)**MACHEP .NE. 1.0, except that
                MACHEP is bounded below by  -(IT+3)
      NEGEPS  - the largest negative integer such that
                1.0-FLOAT(IBETA)**NEGEPS .NE. 1.0, except that
                NEGEPS is bounded below by  -(IT+3)
      IEXP    - the number of bits (decimal places if IBETA = 10)
                reserved for the representation of the exponent
                (including the bias or sign) of a floating-point
                number
      MINEXP  - the largest in magnitude negative integer such that
                FLOAT(IBETA)**MINEXP is positive and normalized
      MAXEXP  - the smallest positive power of  BETA  that overflows
      EPS     - FLOAT(IBETA)**MACHEP.
      EPSNEG  - FLOAT(IBETA)**NEGEPS.
      XMIN    - the smallest non-vanishing normalized floating-point
                power of the radix, i.e.,  XMIN = FLOAT(IBETA)**MINEXP
      XMAX    - the largest finite floating-point number.  In
                particular  XMAX = (1.0-EPSNEG)*FLOAT(IBETA)**MAXEXP
                Note - on some machines  XMAX  will be only the
                second, or perhaps third, largest number, being
                too small by 1 or 2 units in the last digit of
                the significand.
"
  (let ((ibeta (float-radix 1d0))
	(it (float-digits 1d0))
	;; IEEE style rounding assumed, with partial underflow
	(irnd 5)
	(ngrd 0)
	(machep (1- (float-digits 1d0)))
	(negeps (- (float-digits 1d0)))
	(iexp 11)
	(minexp -1022)
	(maxexp 1024)
	(eps double-float-epsilon)
	(epsneg double-float-negative-epsilon)
	(xmin least-positive-normalized-double-float)
	(xmax most-positive-double-float))
    ;; Determine irnd
    (setf irnd 0)
    (let ((a 1d0)
	  (temp 0d0)
	  (temp1 0d0)
	  (tempa 0d0)
	  (beta (float ibeta 1d0)))
      (loop
	 (setf a (+ a a))
	 (setf temp (+ a 1d0))
	 (setf temp1 (- temp a))
	 (unless (zerop (- temp1 1d0))
	   (return)))
      (let* ((betah (/ beta 2))
	     (temp (+ a betah)))
	(when (not (zerop (- temp a)))
	  (setf irnd 1))
	(setf tempa (+ a beta))
	(setf temp (+ tempa betah))
	(when (and (zerop irnd)
		   (not (zerop (- temp tempa))))
	  (setf irnd 2))))
    (values ibeta it irnd ngrd machep negep iexp minexp 
                        maxexp eps epsneg xmin xmax)))
	
	  

  