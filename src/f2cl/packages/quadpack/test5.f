c     Fortran translation (Heh!) of test5 in quadpack-tests.lisp.
      program test5
      implicit none
      double precision alpha
      integer key, keyval(3)
      data keyval/1,3,6/

      print *, 'Test 5, same integral as test 2'
      print *
      print *, ' integrate(1/((x-pi/4)^2+1/alpha, x, 0, 1)'
      print *, '   = atan((4-pi)*4^(alpha-1))+atan(pi*4^(alpha-1))'
      print *
      print *, 'QNG'
      print 9000 
      do 10 alpha = 0d0, 20d0
         call qqng(alpha)
   10 continue
      print *
      print *, 'QAGS'
      print 9000 
      do 30 alpha = 0d0, 20d0
         call qqags(alpha)
   30 continue
      print *
      print *, 'QAG'
      print 9000 
      do 50 alpha = 0d0, 20d0
         call qqag(alpha)
   50 continue
 9000 format('alpha   est result              est abserr             nev
     &al    ier   true answer            true abs err')
      end
      double precision function soln(alpha)
      implicit none
      double precision alpha
      double precision z, pi
      parameter (pi = 3.141592653589793d0)

      z = 4**(alpha-1)
      soln = atan((4-pi)*z) + atan(pi*z)
      return
      end

      double precision function f(x)
      implicit none
      double precision pi
      parameter (pi = 3.141592653589793d0)
      double precision x
      double precision alfa
      common /param/ alfa
      f = 4d0**(-alfa)/((x-pi/4)**2 + 16d0**(-alfa))
      return
      end
      
      subroutine qqng(alpha)
      integer limit, lenw
      parameter (limit = 200, lenw = 4*limit)
      
      double precision alpha
      integer key
      double precision alfa
      common /param/alfa

      integer iwork(limit)
      double precision work(lenw)
      double precision f, soln
      external f

      double precision result, abserr
      integer neval, ier, last
      
      alfa = alpha
      
      call dqng(f, 0d0, 1d0, 0d0, 1d-8, result, abserr, neval, ier)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

      subroutine qqags(alpha)
      integer limit, lenw
      parameter (limit = 200, lenw = 4*limit)
      
      double precision alpha
      integer key
      double precision alfa
      common /param/alfa

      integer iwork(limit)
      double precision work(lenw)
      double precision f, soln
      external f

      double precision result, abserr
      integer neval, ier, last
      
      alfa = alpha
      
      call dqags(f, 0d0, 1d0, 0d0, 1d-8, result, abserr, neval, ier,
     $     limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

      subroutine qqag(alpha)
      integer limit, lenw
      parameter (limit = 200, lenw = 4*limit)
      
      double precision alpha
      integer key
      double precision alfa
      common /param/alfa

      integer iwork(limit)
      double precision work(lenw)
      double precision f, soln
      external f

      double precision result, abserr
      integer neval, ier, last
      
      alfa = alpha
      
      call dqag(f, 0d0, 1d0, 0d0, 1d-8, 1, result, abserr, neval, ier,
     $     limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end
      
