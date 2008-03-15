c     Fortran translation (Heh!) of test2 in quadpack-tests.lisp.
      program test2
      implicit none
      double precision alpha
      integer key, keyval(3)
      data keyval/1,3,6/

      print *, 'Test integral 2'
      print *
      print *, ' integrate(1/((x-pi/4)^2+1/alpha, x, 0, 1)'
      print *, '   = atan((4-pi)*4^(alpha-1))+atan(pi*4^(alpha-1))'
      print *
      do 10 key = 1,3
         print *, 'QAG, key = ', keyval(key)
         print 9000 
         do 20 alpha = 0.0d0, 20d0, 1d0
            call quad(alpha, keyval(key))
   20    continue
   10 continue
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
      
      subroutine quad(alpha, key)
      implicit none
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
      
      call dqag(f, 0d0, 1d0, 0d0, 1d-8, key, result, abserr, neval,
     $     ier, limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end
      
