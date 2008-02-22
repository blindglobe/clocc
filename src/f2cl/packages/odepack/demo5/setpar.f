
      subroutine setpar
c-----------------------------------------------------------------------
c This routine sets the problem parameters.
c It set ns, mx, my, and problem coefficients acoef, bcoef, diff, alph,
c using parameters np, aa, ee, gg, bb, dprey, dpred.
c-----------------------------------------------------------------------
      integer ns, mx, my, mxns
      integer i, j, np
      double precision aa, ee, gg, bb, dprey, dpred,
     1     ax, ay, acoef, bcoef, dx, dy, alph, diff, cox, coy
      common /pcom0/ aa, ee, gg, bb, dprey, dpred
      common /pcom1/ ax, ay, acoef(20,20), bcoef(20), dx, dy, alph,
     1               diff(20), cox(20), coy(20), ns, mx, my, mxns
c
      np = 3
      mx = 6
      my = 6
      aa = 1.0d0
      ee = 1.0d4
      gg = 0.5d-6
      bb = 1.0d0
      dprey = 1.0d0
      dpred = 0.5d0
      alph = 1.0d0
      ns = 2*np
      do 70 j = 1,np
        do 60 i = 1,np
          acoef(np+i,j) = ee
          acoef(i,np+j) = -gg
 60       continue
        acoef(j,j) = -aa
        acoef(np+j,np+j) = -aa
        bcoef(j) = bb
        bcoef(np+j) = -bb
        diff(j) = dprey
        diff(np+j) = dpred
 70     continue
c
      return
c------------  end of subroutine setpar  -------------------------------
      end
