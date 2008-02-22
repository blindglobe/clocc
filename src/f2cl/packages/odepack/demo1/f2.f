
      subroutine f2 (neq, t, y, ydot)
      integer neq, i, j, k, ng
      double precision t, y, ydot, alph1, alph2, d
      dimension y(*), ydot(*)
      dimension neq(*)
      data alph1/1.0d0/, alph2/1.0d0/, ng/5/
      do 10 j = 1,ng
      do 10 i = 1,ng
        k = i + (j - 1)*ng
        d = -2.0d0*y(k)
        if (i .ne. 1) d = d + y(k-1)*alph1
        if (j .ne. 1) d = d + y(k-ng)*alph2
 10     ydot(k) = d
      return
      end
