c     Fortran translation (Heh!) of test1 in quadpack-tests.lisp.
      program test1
      double precision alpha
      integer keyval(3)
      data keyval/1,3,6/

      print *, 'Test integral 1'
      print *
      print *, ' integrate(x^alpha*log(1/x), x, 0, 1)'
      print *, '   = (1+alpha)^(-2)'
      print *
      do 10 key = 1,3
         print *, 'QAG, key = ', keyval(key)
         print 9000 
         do 20 alpha = -0.9d0, 0d0, 0.1d0
            call quad(alpha, keyval(key))
   20    continue
         do 30 alpha = 0.2d0, 2.6d0, 0.2d0
            call quad(alpha, keyval(key))
   30    continue
   10 continue
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
      
      subroutine quad(alpha, key)
      parameter (limit = 200, lenw = 4*limit)
      integer limit, lenw
      
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
      
