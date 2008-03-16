c     Fortran translation (Heh!) of test7 in quadpack-tests.lisp.
      program test7
      double precision alpha
      integer keyval(3)
      data keyval/1,3,6/

      print *, 'Test integral 7'
      print *
      print *, ' integrate(abs(x-1/3)^alpha, x, 0, 1)'
      print *, '   = ((2/3)^(alpha+1)+(1/3)^(alpha+1))/(alpha+1)'
      print *
      print *, 'QAGS'
      do 10 alpha = -0.8d0, 2.1d0, 0.1d0
         call qqags(alpha)
   10 continue
      print *
      print *, 'QAGP'
      do 20 alpha = -0.8d0, 2.1d0, 0.1d0
         call qqagp(alpha)
   20 continue
 9000 format('alpha   est result              est abserr             nev
     &al    ier   true answer            true abs err')
      end
      double precision function soln(alpha)
      double precision alpha
      double precision a1
      a1 = 1 + alpha
      soln = ((2d0/3d0)**a1 + (1d0/3d0)**a1)/a1
      return
      end

      double precision function f(x)
      double precision x
      double precision alfa
      common /param/ alfa
      f = abs(x - 1d0/3d0)**alfa
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
      
      call dqags(f, 0d0, 1d0, 0d0, 1d-8, result, abserr, neval,
     $     ier, limit, lenw, last, iwork, work)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end
      
      subroutine qqagp(alpha)
      integer limit, lenw
      parameter (limit = 200, lenw = 4*limit)
      
      double precision alpha
      double precision alfa
      common /param/alfa

      double precision points(3), alist(limit), blist(limit)
      double precision rlist(limit), pts(3)
      integer level(limit)
      integer ndin(3), iord(limit)
      data points/0.3333333333333333d0,
     $     0.3333333333333333d0,
     $     0.3333333333333333d0/
      
      integer iwork(limit)
      double precision work(lenw)
      double precision f, soln
      external f

      double precision result, abserr
      integer neval, ier, last
      
      alfa = alpha

      call dqagpe(f, 0d0, 1d0, 3, points, 0d0, 1d-8, limit,
     $     result, abserr, neval, ier,
     $     alist, blist, rlist, elist, pts, iord, level, ndin, last)
      print 9000, alpha, result, abserr, neval, ier, soln(alpha),
     $     abs(soln(alpha) - result)
      return
 9000 format(f5.1, 2(2x, 1pe22.15), 2(2x,i5), 2x, 1pe22.15, 1x, 1pe10.3)
      end
      
