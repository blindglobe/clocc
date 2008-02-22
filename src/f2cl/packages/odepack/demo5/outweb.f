
      subroutine outweb (t, c, ns, mx, my, lun)
c-----------------------------------------------------------------------
c This routine prints the values of the individual species densities
c at the current time t.  The write statements use unit lun.
c-----------------------------------------------------------------------
      integer ns, mx, my, lun
      double precision t, c
      dimension c(ns,mx,my)
      integer i, jx, jy
c
      write(lun,10) t
 10   format(/80('-')/30x,'At time t = ',d16.8/80('-') )
c
      do 40 i = 1,ns
        write(lun,20) i
 20     format(' the species c(',i2,') values are:')
        do 30 jy = my,1,-1
          write(lun,25) (c(i,jx,jy),jx=1,mx)
 25       format(6(1x,g12.6))
 30       continue
        write(lun,35)
 35     format(80('-'),/)
 40     continue
c
      return
c------------  end of subroutine outweb  -------------------------------
      end
