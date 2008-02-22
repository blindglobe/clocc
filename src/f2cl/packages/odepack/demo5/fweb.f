
      subroutine fweb (neq, t, cc, cdot)
c-----------------------------------------------------------------------
c This routine computes the derivative of cc and returns it in cdot.
c The interaction rates are computed by calls to webr, and these are
c saved in cc(neq+1),...,cc(2*neq) for use in preconditioning.
c-----------------------------------------------------------------------
      integer neq(1)
      double precision t, cc, cdot
      dimension cc(*), cdot(*)
      integer ns, mx, my, mxns
      integer i, ic, ici, idxl, idxu, idyl, idyu, iyoff, jx, jy
      double precision ax,ay,acoef,bcoef,dx,dy,alph,diff,cox,coy
      double precision dcxli, dcxui, dcyli, dcyui, x, y
      common /pcom1/ ax, ay, acoef(20,20), bcoef(20), dx, dy, alph,
     1               diff(20), cox(20), coy(20), ns, mx, my, mxns
c
      do 100 jy = 1,my
        y = (jy-1)*dy
        iyoff = mxns*(jy-1)
        idyu = mxns
        if (jy .eq. my) idyu = -mxns
        idyl = mxns
        if (jy .eq. 1) idyl = -mxns
        do 90 jx = 1,mx
          x = (jx-1)*dx
          ic = iyoff + ns*(jx-1) + 1
c Get interaction rates at one point (x,y).
          call webr (x, y, t, cc(ic), cc(neq(1)+ic))
          idxu = ns
          if (jx .eq. mx) idxu = -ns
          idxl = ns
          if (jx .eq. 1) idxl = -ns
          do 80 i = 1,ns
            ici = ic + i - 1
c Do differencing in y.
            dcyli = cc(ici) - cc(ici-idyl)
            dcyui = cc(ici+idyu) - cc(ici)
c Do differencing in x.
            dcxli = cc(ici) - cc(ici-idxl)
            dcxui = cc(ici+idxu) - cc(ici)
c Collect terms and load cdot elements.
            cdot(ici) = coy(i)*(dcyui - dcyli) + cox(i)*(dcxui - dcxli)
     1                  + cc(neq(1)+ici)
 80         continue
 90       continue
 100    continue
      return
c------------  end of subroutine fweb  ---------------------------------
      end
