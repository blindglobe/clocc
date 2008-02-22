
      subroutine edit (y, mb, nip, lout)
c This routine prints output.  For each of the mb PDE components, the
c values at the nip points are printed.  All output is on unit lout.
c
      integer mb, nip, lout,  i, k
      double precision y
      dimension y(mb,nip)
c
      do 10 i = 1,mb
 10      write (lout,20) i, (y(i,k),k=1,nip)
c
 20   format(' Values of PDE component i =',i3/15(7d12.4/))
c
      return
c end of subroutine edit
      end
