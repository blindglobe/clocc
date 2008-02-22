
      subroutine gr1 (neq, t, y, ng, groot)
      integer neq, ng
      double precision t, y, groot
      dimension y(1), groot(2)
      dimension neq(*)
      groot(1) = ((2.0d0*log(y(1)) + 8.0d0)/t - 5.0d0)*y(1)
      groot(2) = log(y(1)) - 2.2491d0
      return
      end
