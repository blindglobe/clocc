
      subroutine gr2 (neq, t, y, ng, groot)
      integer neq, ng
      double precision t, y, groot
      dimension y(2), groot(1)
      dimension neq(*)
      groot(1) = y(1)
      return
      end
