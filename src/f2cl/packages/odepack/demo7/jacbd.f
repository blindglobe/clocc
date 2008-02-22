
      subroutine jacbd (n, t, y, s, ml, mu, pa, m0)
c This subroutine computes the Jacobian dg/dy = d(g-a*s)/dy
c and stores elements
c   i   j
c dg /dy   in  pa(i-j+mu+1,j)  in band matrix format.
c It uses r4d = 1/(4*delta), eodsq = eta/delta**2, and nm1 = n - 1
c from the Common block test1.
c
      integer i, n, m0, ml, mu, mup1, mup2, nm1
      double precision t, y, s, pa, diag, r4d, eodsq, two, r2d
      dimension y(*), s(*), pa(m0,*)
      dimension n(*)
      common /test1/ r4d, eodsq, nm1
      data two/2.0d0/
c
      mup1 = mu + 1
      mup2 = mu + 2
      diag = -two*eodsq
      r2d = two*r4d
c                     1   1
c Compute and store dg /dy
      pa(mup1,1) = diag
c
c                     1   2
c Compute and store dg /dy
      pa(mu,2) = -r2d*y(2) + eodsq
c
      do 20 i = 2,nm1
c
c                     i   i-1
c Compute and store dg /dy
        pa(mup2,i-1) = r2d*y(i-1) + eodsq
c
c                     i   i
c Compute and store dg /dy
      pa(mup1,i) = diag
c
c                     i   i+1
c Compute and store dg /dy
        pa(mu,i+1) = -r2d*y(i+1) + eodsq
   20   continue
c
c                     n   n-1
c Compute and store dg /dy
      pa(mup2,nm1) = r2d*y(nm1) + eodsq
c
c                     n   n
c Compute and store dg /dy
      pa(mup1,n(1)) = diag
c
      return
c end of subroutine jacbd for the DLSODI demonstration problem.
      end
