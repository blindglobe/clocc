c     Fortran translation (Heh!) of test14 in quadpack-tests.lisp.
      program test14
      implicit none
      double precision alpha
      integer key

      print *, 'Test 14'
      print *
      print *, ' integrate(x^(-1/2)*exp(-2^(-alpha)*x)*cos(x)'
      print 9001
 9001 format('   = (1+eps)*sqrt(pi)*(1-4^(-alpha))^(-1/4)*cos(atan(2^alp
     $ha)/2)')

      print *
      print *
      print *, 'QAWF'
      print 9000 
      do 20 alpha = 1d0,6d0
         call qqawf(alpha)
   20 continue
      print *
      print *, 'QAWO'
      print 9000 
      do 50 alpha = 1d0, 6d0
         call qqawo(alpha)
   50 continue
 9000 format('alpha   est result              est abserr             nev
     &al    ier   true answer            true abs err')
      end
      double precision function soln(alpha)
      double precision pi
      parameter (pi = 3.141592653589793d0)
      double precision alpha
      soln = sqrt(pi)*(1+4**(-alpha))**(-.25d0)*cos(atan(2**alpha)/2)
      return
      end

      double precision function f(x)
      double precision x, tmp
      double precision alfa
      
      common /param/ alfa
      f = exp(-(2**(-alfa))*x)/sqrt(x)
      return
      end
      
      subroutine qqawf(alpha)
      integer limit, lenw
      parameter (limit = 200, maxp1 = 100, leniw = limit, limlst = 10)
      parameter (lenw = 2*leniw + maxp1*25)
      
      double precision alpha
      double precision alfa
      common /param/alfa

      integer iwork(leniw)
      double precision work(lenw)
      double precision f, soln
      external f

      double precision result, abserr
      integer neval, ier, last
      
      alfa = alpha
      last = 0
      call dqawf(f, 1d-10, 1d0, 1, 1d-8, result, abserr, neval, ier,
     $     limlst, last, leniw, maxp1, lenw, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

      subroutine qqawo(alpha)
      integer leniw, limit, lenw, maxp1
      parameter (limit = 200, leniw = limit, maxp1 = 100)
      parameter (lenw = 2*leniw+25*maxp1)
      
      double precision alpha
      double precision alfa
      common /param/alfa

      integer iwork(leniw)
      double precision work(lenw)
      double precision f, soln
      external f

      double precision result, abserr
      integer neval, ier, last
      
      alfa = alpha
      
      call dqawo(f, 1d-10, 20*2**alpha, 1d0, 1, 0d0, 1d-8,
     $     result, abserr, neval, ier,
     $     leniw, maxp1, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

