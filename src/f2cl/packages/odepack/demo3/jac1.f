
      subroutine jac1 (neq, t, y, ml, mu, pd, nrowpd)
      integer neq, ml, mu, nrowpd
      double precision t, y, pd
      dimension y(*), pd(nrowpd,*)
      dimension neq(*)
      pd(1,1) = 0.0d0
      pd(1,2) = 1.0d0
      pd(2,1) = -40.0d0*y(1)*y(2) - 1.0d0
      pd(2,2) = 20.0d0*(1.0d0 - y(1)*y(1))
      return
      end
