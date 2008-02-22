
      subroutine res (n, t, y, v, r, ires)
c This subroutine computes the residual vector
c   r = g(t,y) - A(t,y)*v .
c It uses nm1 = n - 1 from Common.
c If ires = -1, only g(t,y) is returned in r, since A(t,y) does
c not depend on y.
c
      integer i, ires, n, nm1
      double precision t, y, v, r, r4d, eodsq, one, four, six,
     1   fact1, fact4
      dimension y(*), v(*), r(*)
      dimension n(*)
      integer itemp
      common /test1/ r4d, eodsq, nm1
      data one /1.0d0/, four /4.0d0/, six /6.0d0/
c
      itemp = n(1)
      call gfun (itemp, t, y, r)
      if (ires .eq. -1) return
c
      fact1 = one/six
      fact4 = four/six
      r(1) = r(1) - (fact4*v(1) + fact1*v(2))
      do 10 i = 2, nm1
  10   r(i) = r(i) - (fact1*v(i-1) + fact4*v(i) + fact1*v(i+1))
      r(n(1)) = r(n(1)) - (fact1*v(nm1) + fact4*v(n(1)))
      return
c end of subroutine res for the DLSODI demonstration problem.
      end
