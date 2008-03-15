c     Fortran translation (Heh!) of test3 in quadpack-tests.lisp.
      program test3
      implicit none
      double precision alpha
      integer key, keyval(3)
      data keyval/1,3,6/

      print *, 'Test integral 3'
      print *
      print *, ' integrate(cos(2^alpha*sin(x)), x, pi)'
      print *, '   = pi * J0(2^alpha)'
      print *
      do 10 key = 1,3
         print *, 'QAG, key = ', keyval(key)
         print 9000 
         do 20 alpha = 0.0d0, 10d0, 1d0
            call quad(alpha, keyval(key))
   20    continue
   10 continue
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
      
      subroutine quad(alpha, key)
      implicit none
      integer limit, lenw
      parameter (limit = 200, lenw = 4*limit)
      
      double precision pi
      parameter (pi = 3.141592653589793d0)

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
      
      call dqag(f, 0d0, pi, 0d0, 1d-8, key, result, abserr, neval,
     $     ier, limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end
      
