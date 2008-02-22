
      subroutine jacbt (n, t, y, s, mb, nb, pa, pb, pc)
c This subroutine computes the Jacobian dg/dy = d(g-A*s)/dy
c which has block-tridiagonal structure.  The main, upper, and
c lower diagonals are stored in pa, pb, and pc, respectively.
c
      integer n, mb, nb,  ncomp, nip, nm1, i, j, k
      double precision t, y, s, pa, pb, pc,  r6d, eodsq,  cc, cl, cr,
     1   dlj, drj, paij, pbij, pcij, terma, termb, termc, two
      dimension y(mb,nb),s(n),pa(mb,mb,nb),pb(mb,mb,nb),pc(mb,mb,nb)
      common /par/ r6d, eodsq(3), ncomp, nip, nm1
      data two/2.0d0/
c
c left-most interior point (k = 1)
      cc = y(1,1) + y(2,1) + y(3,1)
      cr = y(1,2) + y(2,2) + y(3,2)
      terma = r6d*cr
      termb = -r6d*(two*cc + cr)
      do 20 j = 1,mb
        drj = y(j,2) - y(j,1)
        paij = -r6d*two*y(j,2)
        pbij = -r6d*drj
        do 10 i = 1,mb
          pa(i,j,1) = paij
 10       pb(i,j,1) = pbij
        pa(j,j,1) = pa(j,j,1) + terma - two*eodsq(j)
        pb(j,j,1) = pb(j,j,1) + termb + eodsq(j)
 20     continue
c
c interior points k = 2 to nip-1
      do 50 k = 2,nm1
        cl = y(1,k-1) + y(2,k-1) + y(3,k-1)
        cc = y(1,k) + y(2,k) + y(3,k)
        cr = y(1,k+1) + y(2,k+1) + y(3,k+1)
        terma = r6d*(cr - cl)
        termb = -r6d*(two*cc + cr)
        termc = r6d*(two*cc + cl)
        do 40 j = 1,mb
          dlj = y(j,k) - y(j,k-1)
          drj = y(j,k+1) - y(j,k)
          paij = -r6d*two*(dlj + drj)
          pbij = -r6d*drj
          pcij = -r6d*dlj
          do 30 i = 1,mb
            pa(i,j,k) = paij
            pb(i,j,k) = pbij
 30         pc(i,j,k) = pcij
          pa(j,j,k) = pa(j,j,k) + terma - two*eodsq(j)
          pb(j,j,k) = pb(j,j,k) + termb + eodsq(j)
          pc(j,j,k) = pc(j,j,k) + termc + eodsq(j)
 40       continue
 50     continue
c
c right-most interior point (k = nip)
      cl = y(1,nm1) + y(2,nm1) + y(3,nm1)
      cc = y(1,nip) + y(2,nip) + y(3,nip)
      terma = -r6d*cl
      termc = r6d*(two*cc + cl)
      do 70 j = 1,mb
        dlj = y(j,nip) - y(j,nm1)
        paij = r6d*two*y(j,nm1)
        pcij = -r6d*dlj
        do 60 i = 1,mb
          pa(i,j,nip) = paij
 60       pc(i,j,nip) = pcij
        pa(j,j,nip) = pa(j,j,nip) + terma - two*eodsq(j)
        pc(j,j,nip) = pc(j,j,nip) + termc + eodsq(j)
 70     continue
c
      return
c end of subroutine jacbt
      end
