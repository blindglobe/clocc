c     Fortran translation (Heh!) of test4 in quadpack-tests.lisp.
      program test4
      implicit none
      double precision alpha
      integer key

      print *, 'Test 4, same integral as test 1'
      print *
      print *, ' integrate(x^alpha*log(1/x), x, 0, 1)'
      print *, '   = (1+alpha)^(-2)'
      print *
      print *, 'QNG'
      print 9000 
      do 10 alpha = -0.9d0, 0d0, 0.1d0
         call qqng(alpha)
   10 continue
      do 20 alpha = 0.2d0, 2.6d0, 0.2d0
         call qqng(alpha)
   20 continue
      print *
      print *, 'QAGS'
      print 9000 
      do 30 alpha = -0.9d0, 0d0, 0.1d0
         call qqags(alpha)
   30 continue
      do 40 alpha = 0.2d0, 2.6d0, 0.2d0
         call qqags(alpha)
   40 continue
      print *
      print *, 'QAG'
      print 9000 
      do 50 alpha = -0.9d0, 0d0, 0.1d0
         call qqag(alpha)
   50 continue
      do 60 alpha = 0.2d0, 2.6d0, 0.2d0
         call qqag(alpha)
   60 continue
 9000 format('alpha   est result              est abserr             nev
     &al    ier   true answer            true abs err')
      end
      double precision function soln(alpha)
      double precision alpha
      soln = (1 + alpha)**(-2)
      return
      end

      double precision function f(x)
      double precision x
      double precision alfa
      common /param/ alfa
      f = x**alfa * log(1/x)
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
      
