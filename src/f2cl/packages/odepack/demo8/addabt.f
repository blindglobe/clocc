
      subroutine addabt (n, t, y, mb, nb, pa, pb, pc)
c This subroutine computes the elements of the matrix A,
c and adds them to pa, pb, and pc in the appropriate manner.
c The matrix A is tridiagonal, of order n, with
c nonzero elements (reading across) of 1/6, 4/6, 1/6.
c
      integer n, mb, nb, i, k
      double precision pa, pb, pc, t, y,  aa1, aa4, four, one, six
      dimension y(mb,nb),pa(mb,mb,nb),pb(mb,mb,nb),pc(mb,mb,nb)
      data one/1.0d0/, four/4.0d0/, six/6.0d0/
c
      aa1 = one/six
      aa4 = four/six
      do 50 k = 1,nb
        do 10 i = 1,mb
 10       pa(i,i,k) = pa(i,i,k) + aa4
        if (k .ne. nb) then
          do 20 i = 1,mb
 20         pb(i,i,k) = pb(i,i,k) + aa1
        endif
        if (k .ne. 1) then
          do 30 i = 1,mb
 30         pc(i,i,k) = pc(i,i,k) + aa1
        endif
 50     continue
c
      return
c end of subroutine addabt
      end
