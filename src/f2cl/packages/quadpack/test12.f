c     Fortran translation (Heh!) of test12 in quadpack-tests.lisp.
      program test12
      implicit none
      double precision alpha
      integer key

      print *, 'Test 12'
      print *
      print *, ' integrate(exp(20*(x-1))*sin(2^alpha*x),x,0,1)'
      print 9001
 9001 format('   = (20*sin(2^alpha) - 2^alpha*cos(2^alpha) + 2^alpha*exp
     $(-20))/(400 + 4^alpha)')
      print *
      print *
      print *, 'QAG'
      print 9000 
      do 20 alpha = 0d0,9d0
         call qqag(alpha)
   20 continue
      print *
      print *, 'QAWS'
      print 9000 
      do 50 alpha = 0d0, 9d0
         call qqawo(alpha)
   50 continue
 9000 format('alpha   est result              est abserr             nev
     &al    ier   true answer            true abs err')
      end
      double precision function soln(alpha)
      double precision pi
      parameter (pi = 3.141592653589793d0)
      double precision alpha, a2
      a2 = 2**(alpha)
      soln = (20*sin(a2)-a2*cos(a2)+a2*exp(-20d0))/(400+4**alpha)
      return
      end

      double precision function f(x)
      double precision x, tmp
      double precision alfa
      common /param/ alfa
      f = exp(20*(x-1))*sin(x*2**alfa)
      return
      end
      
      subroutine qqag(alpha)
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
      
      call dqag(f, 0d0, 1d0, 0d0, 1d-8, 6, result, abserr, neval, ier,
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

      f2 = exp(20*(x-1))
      return
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
      double precision f2, soln
      external f2

      double precision result, abserr
      integer neval, ier, last
      
      alfa = alpha
      
      call dqawo(f2, 0d0, 1d0, 2**alfa, 2, 0d0, 1d-8,
     $     result, abserr, neval, ier,
     $     leniw, maxp1, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end

