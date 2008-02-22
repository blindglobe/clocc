
      subroutine gfun (n, t, y, g)
c This subroutine computes the right-hand side function g(y,t).
c It uses r4d = 1/(4*delta), eodsq = eta/delta**2, and nm1 = n - 1
c from the Common block test1.
c
      integer i, n, nm1
      double precision t, y, g, r4d, eodsq, two
      dimension g(n), y(n)
      common /test1/ r4d, eodsq, nm1
      data two/2.0d0/
c
      g(1) = -r4d*y(2)**2 + eodsq*(y(2) - two*y(1))
c
      do 20 i = 2,nm1
        g(i) = r4d*(y(i-1)**2 - y(i+1)**2)
     1        + eodsq*(y(i+1) - two*y(i) + y(i-1))
   20   continue
c
      g(n) = r4d*y(nm1)**2 + eodsq*(y(nm1) - two*y(n))
c
      return
c end of subroutine gfun for the DLSODI demonstration problem.
      end
