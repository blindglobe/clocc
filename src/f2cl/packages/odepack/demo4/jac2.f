
      subroutine jac2 (neq, t, y, ml, mu, pd, nrowpd)
      integer neq, ml, mu, nrowpd
      double precision t, y, pd
      dimension y(2), pd(nrowpd,2)
      dimension neq(*)
      pd(1,1) = 0.0d0
      pd(1,2) = 1.0d0
      pd(2,1) = -200.0d0*y(1)*y(2) - 1.0d0
      pd(2,2) = 100.0d0*(1.0d0 - y(1)*y(1))
      return
      end
