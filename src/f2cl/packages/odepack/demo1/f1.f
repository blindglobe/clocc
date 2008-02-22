
      subroutine f1 (neq, t, y, ydot)
      integer neq
      double precision t, y, ydot
      dimension y(*), ydot(*)
      dimension neq(*)
      ydot(1) = y(2)
      ydot(2) = 3.0d0*(1.0d0 - y(1)*y(1))*y(2) - y(1)
      return
      end
