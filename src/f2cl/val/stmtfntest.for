C test for translation of various sub programs
C
C a function sub program
      FUNCTION ifun(arg1, arg2)
      INTEGER arg1, arg2
      ifun = arg1 + arg2
      END
C
C a typed FUNCTION
      DOUBLE FUNCTION jfun(arg1)
      DOUBLE arg1
      jfun = SQRT(arg1)
      END
C
C subroutine with a stmt function
      SUBROUTINE subr1(a,b)
      DIMENSION arrayA(3,4,3)
      stmtfn(x, y, z) = max(x, max(y, z))
      a = stmtfn(b,a,a)
      arrayA(1,1,1) = a
      write(*,9000),a
9000  format("Value is "I3/)
      RETURN
      END

c subroutine with no args and which calls the functions
      SUBROUTINE subr2
      IMPLICIT DOUBLE (i,l-n)
      write(*, 9030)
      i=1.
      j=2
      m=3
      l=4.5
      write(*,9010), ifun(j,m)
      write(*,9020), jfun(i+l)
      STOP
9010  FORMAT("IFUN RETURNS "I3) 
9020  FORMAT("JFUN RETURNS "D3.2)
9030  FORMAT("inside subroutine subr2"/)
      END
C
C main
      PROGRAM TEST
      CALL subr1(3, 4)
      CALL subr2
      STOP
      END
C
