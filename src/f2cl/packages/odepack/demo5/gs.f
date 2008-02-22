
      subroutine gs (n, hl0, z, x)
c-----------------------------------------------------------------------
c This routine performs itmax Gauss-Seidel iterations
c to compute an approximation to P-inverse*z, where P = I - hl0*Jd, and
c Jd represents the diffusion contributions to the Jacobian.
c z contains the answer on return.
c The dimensions below assume ns .le. 20.
c-----------------------------------------------------------------------
      integer n
      double precision hl0, z, x
      dimension z(*), x(*)
      dimension n(1)
      integer ns, mx, my, mxns,
     1        mp, mq, mpsq, itmax
      integer i, ic, ici, iter, iyoff, jx, jy
      double precision ax,ay,acoef,bcoef,dx,dy,alph,diff,cox,coy,
     2     uround, srur
      double precision beta,beta2,cof1,elamda,gamma,gamma2
      dimension beta(20), gamma(20), beta2(20), gamma2(20), cof1(20)
      common /pcom1/ ax, ay, acoef(20,20), bcoef(20), dx, dy, alph,
     1               diff(20), cox(20), coy(20), ns, mx, my, mxns
      common /pcom2/ uround, srur, mp, mq, mpsq, itmax
c
c-----------------------------------------------------------------------
c Write matrix as P = D - L - U.
c Load local arrays beta, beta2, gamma, gamma2, and cof1.
c-----------------------------------------------------------------------
      do 10 i = 1,ns
        elamda = 1.d0/(1.d0 + 2.d0*hl0*(cox(i) + coy(i)))
        beta(i) = hl0*cox(i)*elamda
        beta2(i) = 2.d0*beta(i)
        gamma(i) = hl0*coy(i)*elamda
        gamma2(i) = 2.d0*gamma(i)
        cof1(i) = elamda
 10     continue
c-----------------------------------------------------------------------
c Begin iteration loop.
c Load array x with (D-inverse)*z for first iteration.
c-----------------------------------------------------------------------
      iter = 1
c
      do 50 jy = 1,my
        iyoff = mxns*(jy-1)
        do 40 jx = 1,mx
          ic = iyoff + ns*(jx-1)
          do 30 i = 1,ns
            ici = ic + i
            x(ici) = cof1(i)*z(ici)
            z(ici) = 0.d0
 30         continue
 40       continue
 50     continue
      go to 160
c-----------------------------------------------------------------------
c Calculate (D-inverse)*U*x.
c-----------------------------------------------------------------------
 70   continue
      iter = iter + 1
      jy = 1
        jx = 1
        ic = ns*(jx-1)
        do 75 i = 1,ns
          ici = ic + i
 75       x(ici) = beta2(i)*x(ici+ns) + gamma2(i)*x(ici+mxns)
        do 85 jx = 2,mx-1
          ic = ns*(jx-1)
          do 80 i = 1,ns
            ici = ic + i
 80         x(ici) = beta(i)*x(ici+ns) + gamma2(i)*x(ici+mxns)
 85       continue
        jx = mx
        ic = ns*(jx-1)
        do 90 i = 1,ns
          ici = ic + i
 90       x(ici) = gamma2(i)*x(ici+mxns)
      do 115 jy = 2,my-1
        iyoff = mxns*(jy-1)
          jx = 1
          ic = iyoff
          do 95 i = 1,ns
            ici = ic + i
 95         x(ici) = beta2(i)*x(ici+ns) + gamma(i)*x(ici+mxns)
          do 105 jx = 2,mx-1
            ic = iyoff + ns*(jx-1)
            do 100 i = 1,ns
              ici = ic + i
 100          x(ici) = beta(i)*x(ici+ns) + gamma(i)*x(ici+mxns)
 105        continue
          jx = mx
          ic = iyoff + ns*(jx-1)
          do 110 i = 1,ns
            ici = ic + i
 110        x(ici) = gamma(i)*x(ici+mxns)
 115      continue
      jy = my
      iyoff = mxns*(jy-1)
        jx = 1
        ic = iyoff
        do 120 i = 1,ns
          ici = ic + i
 120      x(ici) = beta2(i)*x(ici+ns)
        do 130 jx = 2,mx-1
          ic = iyoff + ns*(jx-1)
          do 125 i = 1,ns
            ici = ic + i
 125      x(ici) = beta(i)*x(ici+ns)
 130      continue
        jx = mx
        ic = iyoff + ns*(jx-1)
        do 135 i = 1,ns
          ici = ic + i
 135      x(ici) = 0.0d0
c-----------------------------------------------------------------------
c Calculate (I - (D-inverse)*L)-inverse * x.
c-----------------------------------------------------------------------
 160  continue
      jy = 1
        do 175 jx = 2,mx-1
          ic = ns*(jx-1)
          do 170 i = 1,ns
            ici = ic + i
 170        x(ici) = x(ici) + beta(i)*x(ici-ns)
 175      continue
        jx = mx
        ic = ns*(jx-1)
        do 180 i = 1,ns
          ici = ic + i
 180      x(ici) = x(ici) + beta2(i)*x(ici-ns)
      do 210 jy = 2,my-1
        iyoff = mxns*(jy-1)
          jx = 1
          ic = iyoff
          do 185 i = 1,ns
            ici = ic + i
 185        x(ici) = x(ici) + gamma(i)*x(ici-mxns)
          do 200 jx = 2,mx-1
            ic = iyoff + ns*(jx-1)
            do 195 i = 1,ns
              ici = ic + i
              x(ici) = (x(ici) + beta(i)*x(ici-ns))
     1             + gamma(i)*x(ici-mxns)
 195          continue
 200        continue
            jx = mx
            ic = iyoff + ns*(jx-1)
            do 205 i = 1,ns
              ici = ic + i
              x(ici) = (x(ici) + beta2(i)*x(ici-ns))
     1             + gamma(i)*x(ici-mxns)
 205          continue
 210        continue
      jy = my
      iyoff = mxns*(jy-1)
        jx = 1
        ic = iyoff
        do 215 i = 1,ns
          ici = ic + i
 215      x(ici) = x(ici) + gamma2(i)*x(ici-mxns)
        do 225 jx = 2,mx-1
          ic = iyoff + ns*(jx-1)
          do 220 i = 1,ns
            ici = ic + i
            x(ici) = (x(ici) + beta(i)*x(ici-ns))
     1           + gamma2(i)*x(ici-mxns)
 220        continue
 225      continue
        jx = mx
        ic = iyoff + ns*(jx-1)
        do 230 i = 1,ns
          ici = ic + i
          x(ici) = (x(ici) + beta2(i)*x(ici-ns))
     1         + gamma2(i)*x(ici-mxns)
 230      continue
c-----------------------------------------------------------------------
c Add increment x to z.
c-----------------------------------------------------------------------
      do 300 i = 1,n(1)
 300    z(i) = z(i) + x(i)
c
      if (iter .lt. itmax) go to 70
      return
c------------  end of subroutine gs  -----------------------------------
      end
