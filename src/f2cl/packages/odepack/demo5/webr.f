
      subroutine webr (x, y, t, c, rate)
c-----------------------------------------------------------------------
c This routine computes the interaction rates for the species
c c(1),...,c(ns), at one spatial point and at time t.
c-----------------------------------------------------------------------
      double precision x, y, t, c, rate
      dimension c(*), rate(*)
      integer ns, mx, my, mxns
      integer i
      double precision ax,ay,acoef,bcoef,dx,dy,alph,diff,cox,coy
      double precision fac
      common /pcom1/ ax, ay, acoef(20,20), bcoef(20), dx, dy, alph,
     1               diff(20), cox(20), coy(20), ns, mx, my, mxns
c
      do 10 i = 1,ns
 10     rate(i) = 0.0d0
      do 15 i = 1,ns
        call daxpy (ns, c(i), acoef(1,i), 1, rate, 1)
 15     continue
      fac = 1.0d0 + alph*x*y
      do 20 i = 1,ns
 20     rate(i) = c(i)*(bcoef(i)*fac + rate(i))
      return
c------------  end of subroutine webr  ---------------------------------
      end
