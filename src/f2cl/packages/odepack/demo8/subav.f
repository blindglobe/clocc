
      subroutine subav (r, v, mb)
c This routine subtracts the matrix a time the vector v from r,
c in order to form the residual vector, stored in r.
c
      integer mb,  ncomp, nip, nm1,  i, k
      double precision r, v,  r6d, eodsq,  aa1, aa4, four, one, six
      dimension r(mb,*), v(mb,*)
      common /par/ r6d, eodsq(3), ncomp, nip, nm1
      data one /1.0d0/, four /4.0d0/, six /6.0d0/
c
      aa1 = one/six
      aa4 = four/six
c
      do 10 i = 1,mb
 10     r(i,1) = r(i,1) - (aa4*v(i,1) + aa1*v(i,2))
c
      do 20 k = 2,nm1
        do 15 i = 1,mb
 15       r(i,k) = r(i,k) - (aa1*v(i,k-1) + aa4*v(i,k) + aa1*v(i,k+1))
 20     continue
c
      do 30 i = 1,mb
 30     r(i,nip) = r(i,nip) - (aa1*v(i,nm1) + aa4*v(i,nip))
c
      return
c end of subroutine subav
      end
