
      subroutine jac2 (neq, t, y, ml, mu, pd, nrowpd)
      integer neq, ml, mu, nrowpd, j, mband, mu1, mu2, ng
      double precision t, y, pd, alph1, alph2
      dimension y(*), pd(nrowpd,*)
      dimension neq(*)
      data alph1/1.0d0/, alph2/1.0d0/, ng/5/
      mband = ml + mu + 1
      mu1 = mu + 1
      mu2 = mu + 2
      do 10 j = 1,neq(1)
        pd(mu1,j) = -2.0d0
        pd(mu2,j) = alph1
 10     pd(mband,j) = alph2
      do 20 j = ng,neq(1),ng
 20     pd(mu2,j) = 0.0d0
      return
      end
