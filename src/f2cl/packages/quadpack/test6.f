c     Fortran translation (Heh!) of test6 in quadpack-tests.lisp.
      program test6
      implicit none
      double precision alpha

      print *, 'Test 6, same integral as test 3'
      print *
      print *, ' integrate(cos(2^alpha*sin(x)), x, pi)'
      print *, '   = pi * J0(2^alpha)'
      print *
      print *, 'QNG'
      print 9000 
      do 10 alpha = 0d0, 10d0
         call qqng(alpha)
   10 continue
      print *
      print *, 'QAGS'
      print 9000 
      do 30 alpha = 0d0, 10d0
         call qqags(alpha)
   30 continue
      print *
      print *, 'QAG'
      print 9000 
      do 50 alpha = 0d0, 10d0
         call qqag(alpha)
   50 continue
 9000 format('alpha   est result              est abserr             nev
     &al    ier   true answer            true abs err')
      end
      double precision function soln(alpha)
      implicit none
      double precision alpha
      double precision z
c
c     If we have Bessel J0, we should use that.
c      
      soln = 0d0
      return
      end

      double precision function f(x)
      implicit none
      double precision x
      double precision alfa
      common /param/ alfa
      f = cos(2**alfa*sin(x))
      return
      end
      
      subroutine qqng(alpha)
      integer limit, lenw
      double precision pi
      parameter (pi = 3.141592653589793d0)
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
      
      call dqng(f, 0d0, pi, 0d0, 1d-8, result, abserr, neval, ier)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

      subroutine qqags(alpha)
      integer limit, lenw
      double precision pi
      parameter (pi = 3.141592653589793d0)
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
      
      call dqags(f, 0d0, pi, 0d0, 1d-8, result, abserr, neval, ier,
     $     limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

      subroutine qqag(alpha)
      integer limit, lenw
      double precision pi
      parameter (pi = 3.141592653589793d0)
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
      
      call dqag(f, 0d0, pi, 0d0, 1d-8, 6, result, abserr, neval, ier,
     $     limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end
      
