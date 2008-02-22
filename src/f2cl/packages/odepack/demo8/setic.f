
      subroutine setic (nint, mb, y)
c This routine loads the y array with initial data based on a
c square wave profile for each of the mb PDE variables.
c
      integer nint, mb, i, k, nip, n14, n34
      double precision y,  amp, half, zero
      dimension y(mb,*), amp(3)
      data zero/0.0d0/, half/0.5d0/, amp/0.2d0,0.3d0,0.5d0/
c
      nip = nint - 1
      n14 = nint/4
      n34 = 3*n14
c
      do 15 k = 1,n14-1
        do 10 i = 1,mb
 10       y(i,k) = zero
 15     continue
c
      do 20 i = 1,mb
 20     y(i,n14) = half*amp(i)
c
      do 35 k = n14+1,n34-1
        do 30 i = 1,mb
 30       y(i,k) = amp(i)
 35     continue
c
      do 40 i = 1,mb
 40     y(i,n34) = half*amp(i)
c
      do 55 k = n34+1,nip
        do 50 i = 1,mb
 50       y(i,k) = zero
 55     continue
c
      return
c end of subroutine setic
      end
