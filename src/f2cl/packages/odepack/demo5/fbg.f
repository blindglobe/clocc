
      subroutine fbg (neq, t, cc, jx, jy, cdot)
c-----------------------------------------------------------------------
c This routine computes one block of the interaction terms of the
c system, namely block (jx,jy), for use in preconditioning.
c-----------------------------------------------------------------------
      integer neq, jx, jy
      double precision t, cc, cdot
      dimension cc(neq), cdot(neq)
      integer ns, mx, my, mxns
      integer iblok, ic
      double precision ax,ay,acoef,bcoef,dx,dy,alph,diff,cox,coy
      double precision x, y
c
      common /pcom1/ ax, ay, acoef(20,20), bcoef(20), dx, dy, alph,
     1               diff(20), cox(20), coy(20), ns, mx, my, mxns
c
      iblok = jx + (jy-1)*mx
        y = (jy-1)*dy
          x = (jx-1)*dx
          ic = ns*(iblok-1) + 1
          call webr (x, y, t, cc(ic), cdot)
      return
c------------  end of subroutine fbg  ----------------------------------
      end
