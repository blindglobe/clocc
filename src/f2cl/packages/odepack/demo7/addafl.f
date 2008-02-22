
      subroutine addafl (n, t, y, ml, mu, pa, m0)
c This subroutine computes the matrix A in full form, adds it to
c pa, and returns the sum in pa.
c It uses nm1 = n - 1 from Common.
c The matrix A is tridiagonal, of order n, with nonzero elements
c (reading across) of  1/6, 4/6, 1/6.
c
      integer i, n, m0, ml, mu, nm1
      double precision t, y, pa, r4d, eodsq, one, four, six,
     1   fact1, fact4
      dimension y(*), pa(m0,8)
      dimension n(1)
      common /test1/ r4d, eodsq, nm1
      data one/1.0d0/, four/4.0d0/, six/6.0d0/
c
c Compute the elements of A.
      fact1 = one/six
      fact4 = four/six
c
c Add the matrix A to the matrix pa (full).
c
      do 110  i = 2, nm1
         pa(i,i+1) = pa(i,i+1) + fact1
         pa(i,i) = pa(i,i) + fact4
         pa(i,i-1) = pa(i,i-1) + fact1
  110    continue
      pa(1,2) = pa(1,2) + fact1
      pa(1,1) = pa(1,1) + fact4
      pa(n(1),n(1)) = pa(n(1),n(1)) + fact4
      pa(n(1),nm1) = pa(n(1),nm1) + fact1
      return
c end of subroutine addafl for the DLSODI demonstration problem.
      end
