
      subroutine cinit (cc)
c-----------------------------------------------------------------------
c This routine computes and loads the vector of initial values.
c-----------------------------------------------------------------------
      double precision cc
      dimension cc(*)
      integer ns, mx, my, mxns
      integer i, ici, ioff, iyoff, jx, jy
      double precision ax,ay,acoef,bcoef,dx,dy,alph,diff,cox,coy
      double precision argx, argy, x, y
      common /pcom1/ ax, ay, acoef(20,20), bcoef(20), dx, dy, alph,
     1               diff(20), cox(20), coy(20), ns, mx, my, mxns
c
        do 20 jy = 1,my
          y = (jy-1)*dy
          argy = 16.0d0*y*y*(ay-y)*(ay-y)
          iyoff = mxns*(jy-1)
          do 10 jx = 1,mx
            x = (jx-1)*dx
            argx = 16.0d0*x*x*(ax-x)*(ax-x)
            ioff = iyoff + ns*(jx-1)
            do 5 i = 1,ns
              ici = ioff + i
              cc(ici) = 10.0d0 + i*argx*argy
  5           continue
 10         continue
 20       continue
      return
c------------  end of subroutine cinit  --------------------------------
      end
