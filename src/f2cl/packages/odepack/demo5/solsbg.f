
      subroutine solsbg (n, t, cc, f0, wk, hl0, bd, ipbd, v, lr, ier)
c-----------------------------------------------------------------------
c This routine applies one or two inverse preconditioner matrices
c to the array v, using the interaction-only block-diagonal Jacobian
c with block-grouping, and Gauss-Seidel applied to the diffusion terms.
c When lr = 1 or 3, it calls gs for a Gauss-Seidel approximation
c to ((I-hl0*Jd)-inverse)*v, and stores the result in v.
c When lr = 2 or 3, it computes ((I-hl0*dg/dc)-inverse)*v, using LU
c factors of the blocks in bd, and pivot information in ipbd.
c In both cases, the array v is overwritten with the solution.
c-----------------------------------------------------------------------
      integer n, ipbd, lr, ier
      double precision t, cc, f0, wk, hl0, bd, v
      dimension cc(*), f0(*), wk(*), bd(*), ipbd(*), v(*)
      dimension n(1)
      integer mp, mq, mpsq, itmax,
     2        meshx,meshy,ngx,ngy,ngrp,mxmp,jgx,jgy,jigx,jigy,jxr,jyr
      integer ibd, ig0, igm1, igx, igy, iip, iv, jx, jy
      double precision uround, srur
c
      common /pcom2/ uround, srur, mp, mq, mpsq, itmax
      common /pcom3/ meshx, meshy, ngx, ngy, ngrp, mxmp,
     1   jgx(21), jgy(21), jigx(50), jigy(50), jxr(20), jyr(20)
c
      ier = 0
c
      if (lr.eq.0 .or. lr.eq.1 .or. lr.eq.3) call gs (n, hl0, v, wk)
      if (lr.eq.0 .or. lr.eq.2 .or. lr.eq.3) then
        iv = 1
        do 20 jy = 1,meshy
          igy = jigy(jy)
          ig0 = (igy - 1)*ngx
          do 10 jx = 1,meshx
            igx = jigx(jx)
            igm1 = igx - 1 + ig0
            ibd = 1 + igm1*mpsq
            iip = 1 + igm1*mp
            call dgesl (bd(ibd), mp, mp, ipbd(iip), v(iv), 0)
            iv = iv + mp
 10         continue
 20       continue
        endif
c
      return
c------------  end of subroutine solsbg  -------------------------------
      end
