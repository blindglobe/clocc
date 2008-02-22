
      subroutine addabd (n, t, y, ml, mu, pa, m0)
c This subroutine computes the matrix A in band form, adds it to pa,
c and returns the sum in pa.   The matrix A is tridiagonal, of order n,
c with nonzero elements (reading across) of  1/6, 4/6, 1/6.
c
      integer i, n, m0, ml, mu, mup1, mup2
      double precision t, y, pa, fact1, fact4, one, four, six
      dimension y(*), pa(m0,*)
      dimension n(*)
      data one/1.0d0/, four/4.0d0/, six/6.0d0/
c
c Set the pointers.
      mup1 = mu + 1
      mup2 = mu + 2
c Compute the elements of A.
      fact1 = one/six
      fact4 = four/six
c Add the matrix A to the matrix pa (banded).
      do 10 i = 1,n(1)
        pa(mu,i) = pa(mu,i) + fact1
        pa(mup1,i) = pa(mup1,i) + fact4
        pa(mup2,i) = pa(mup2,i) + fact1
   10   continue
      return
c end of subroutine addabd for the DLSODI demonstration problem.
      end
