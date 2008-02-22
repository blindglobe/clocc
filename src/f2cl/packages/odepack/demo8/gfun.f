
      subroutine gfun (t, y, g, mb)
c This subroutine computes the right-hand side function g(y,t).
c It uses r6d = 1/(6*dx), eodsq(*) = eta(*)/dx**2, nip,
c and nm1 = nip - 1 from the Common block par.
c
      integer mb,  ncomp, nip, nm1,  i, k
      double precision t, y, g,  r6d, eodsq,  cc, cl, cr, dli, dri, two
      dimension g(mb,*), y(mb,*)
      common /par/ r6d, eodsq(3), ncomp, nip, nm1
      data two/2.0d0/
c
c left-most interior point (k = 1)
      cc = y(1,1) + y(2,1) + y(3,1)
      cr = y(1,2) + y(2,2) + y(3,2)
      do 10 i = 1,mb
        dri = y(i,2) - y(i,1)
        g(i,1) = -r6d*(two*cc*y(i,2) + cr*dri)
     1         + eodsq(i)*(dri - y(i,1))
 10     continue
c
c interior points k = 2 to nip-1
      do 20 k = 2,nm1
        cl = y(1,k-1) + y(2,k-1) + y(3,k-1)
        cc = y(1,k) + y(2,k) + y(3,k)
        cr = y(1,k+1) + y(2,k+1) + y(3,k+1)
        do 15 i = 1,mb
          dli = y(i,k) - y(i,k-1)
          dri = y(i,k+1) - y(i,k)
          g(i,k) = -r6d*(cl*dli + two*cc*(dli + dri) + cr*dri)
     1           + eodsq(i)*(dri - dli)
 15       continue
 20     continue
c
c right-most interior point (k = nip)
      cl = y(1,nm1) + y(2,nm1) + y(3,nm1)
      cc = y(1,nip) + y(2,nip) + y(3,nip)
      do 30 i = 1,mb
        dli = y(i,nip) - y(i,nm1)
        g(i,nip) = -r6d*(cl*dli - two*cc*y(i,nm1))
     1           - eodsq(i)*(y(i,nip) + dli)
 30     continue
c
        t = t
      return
c end of subroutine gfun
      end
