c     Fortran translation (Heh!) of test15 in quadpack-tests.lisp.
      program test15
      implicit none
      double precision alpha
      integer key

      print *, 'Test 15'
      print *
      print *, ' integrate(x^2*exp(-2^(-alpha)*x),x,0,b)'
      print *, '  = (1+eps)*2^(3*alpha+1)'

      print *
      print *
      print *, 'QAG'
      print 9000 
      do 20 alpha = 0d0,6d0
         call qqag(alpha)
   20 continue
      print *
      print *, 'QAGI'
      print 9000 
      do 50 alpha = 0d0, 6d0
         call qqagi(alpha)
   50 continue
 9000 format('alpha   est result              est abserr             nev
     &al    ier   true answer            true abs err')
      end
      double precision function soln(alpha, limit)
      double precision pi
      parameter (pi = 3.141592653589793d0)
      double precision alpha, a2, tmp, limit

      if (limit .gt. 0) then
         a2 = 2**alpha
         tmp = exp(-limit/a2)*(limit**2 + 2*a2*limit + 2*a2)
      else
         tmp = 0
      endif
      soln = 2**(1+3*alpha) - tmp
      return
      end

      double precision function f(x)
      double precision x, tmp
      double precision alfa
      
      common /param/ alfa
      f = x*x*exp(-x*2**(-alfa))
      return
      end
      
      subroutine qqag(alpha)
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
      call dqag(f, 0d0, 40*2**alpha, 0d0, 1d-8, 6,
     $     result, abserr, neval, ier,
     $     limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier,
     $     soln(alpha, 40*2**alpha),
     $     abs(soln(alpha,40*2**alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

      subroutine qqagi(alpha)
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
      
      call dqagi(f, 0d0, 1, 0d0, 1d-8,
     $     result, abserr, neval, ier,
     $     limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha,0d0),
     $     abs(soln(alpha,0d0) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

