C*******************************************************************
      SUBROUTINE O8SHMS
C WRITE SHORT INFORMATION ON STANDARD OUT
      INCLUDE 'O8COMM.INC'
      DOUBLE PRECISION UMIN
      SAVE
      IF ( TE0 .AND. .NOT. SILENT ) THEN
        UMIN=ACCINF(ITSTEP,11)
        WRITE(*,FMT='(I5,'' FX='',D15.7,'' UPSI='',D9.2,
     F  '' B2N='',D9.2,'' UMI='',D9.2,'' NR'',I4,'' SI'',I2)')
     F   ITSTEP,FX,UPSI,B2N,UMIN,NR,INT(ACCINF(ITSTEP,10))
        WRITE(PROU,FMT='(I5,'' FX='',D15.7,'' UPSI='',D9.2,
     F  '' B2N='',D9.2,'' UMI='',D9.2,'' NR'',I4,'' SI'',I2)')
     F   ITSTEP,FX,UPSI,B2N,UMIN,NR,INT(ACCINF(ITSTEP,10))

      ENDIF
      RETURN
      END
