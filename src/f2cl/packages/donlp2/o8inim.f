C****************************************************************
      SUBROUTINE O8INIM
C***  INITIALIZE THE QUASI NEWTON UPDATE WITH A MULTIPLE OF THE
C     IDENTITY
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      INTEGER I,J
      SAVE
          DO I=1,N
            DO J=1,N
              A(J,I)=ZERO
            ENDDO
            A(I,I)=MATSC
            DIAG0(I)=MATSC
          ENDDO
          ACCINF(ITSTEP,27)=-ONE
          ACCINF(ITSTEP,14)=ONE
          IF ( .NOT. SILENT ) CALL O8INFO(20)
          RETURN
      END
