
      subroutine f1 (neq, t, y, ydot)
      integer neq
      double precision t, y, ydot
      dimension y(1), ydot(1)
      dimension neq(*)
      ydot(1) = ((2.0d0*log(y(1)) + 8.0d0)/t - 5.0d0)*y(1)
      return
      end
