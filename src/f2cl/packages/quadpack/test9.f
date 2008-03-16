c     Fortran translation (Heh!) of test9 in quadpack-tests.lisp.
      program test9
      implicit none
      double precision alpha
      integer key

      print *, 'Test 4, same integral as test 1'
      print *
      print *, ' integrate(x^alpha*log(1/x), x, 0, 1)'
      print *, '   = (1+alpha)^(-2)'
      print *
      print *
      print *, 'QAGS'
      print 9000 
      do 20 alpha = 1d0, 20d0
         call qqags(alpha)
   20 continue
      print *
      print *, 'QAWS'
      print 9000 
      do 50 alpha = 1d0, 20d0
         call qqaws(alpha)
   50 continue
 9000 format('alpha   est result              est abserr             nev
     &al    ier   true answer            true abs err')
      end
      double precision function soln(alpha)
      double precision pi
      parameter (pi = 3.141592653589793d0)
      double precision alpha, a2
      a2 = 2**(-alpha)
      soln = pi/sqrt(a2 * (2 + a2))
      return
      end

      double precision function f(x)
      double precision x, tmp
      double precision alfa
      common /param/ alfa
      tmp = 1/sqrt(1-x*x)
      tmp = tmp / (1 + x + 2**(-alfa))
      f = tmp
      return
      end
      
      subroutine qqags(alpha)
      integer limit, lenw
      parameter (limit = 200, lenw = 4*limit)
      
      double precision alpha
      double precision alfa
      common /param/alfa

      integer iwork(limit)
      double precision work(lenw)
      double precision f, soln
      external f

      double precision result, abserr
      integer neval, ier, last
      
      alfa = alpha
      
      call dqags(f, -1d0, 1d0, 0d0, 1d-8, result, abserr, neval, ier,
     $     limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

      double precision function f2(x)
      double precision x
      
      double precision alfa
      common /param/alfa

      f2 = 1/(1+x+2**(-alfa))
      return
      end
      
      subroutine qqaws(alpha)
      integer limit, lenw
      parameter (limit = 200, lenw = 4*limit)
      
      double precision alpha
      double precision alfa
      common /param/alfa

      integer iwork(limit)
      double precision work(lenw)
      double precision f2, soln
      external f2

      double precision result, abserr
      integer neval, ier, last
      
      alfa = alpha
      
      call dqaws(f2, -1d0, 1d0, -0.5d0, -0.5d0, 1, 0d0, 1d-8,
     $     result, abserr, neval, ier,
     $     limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

