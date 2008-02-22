      program opkdemo8
c-----------------------------------------------------------------------
c Demonstration program for the DLSOIBT package.
c This is the version of 14 June 2001.
c
c This version is in double precision.
c
c This program solves a semi-discretized form of the following system
c Of three PDEs (each similar to a Burgers equation):
c
c   u(i)   =  -(u(1)+u(2)+u(3)) u(i)   +  eta(i) u(i)    (i=1,2,3),
c       t                           x                xx
c
c on the interval  -1 .le. x .le. 1, and with time t .ge. 0.
c The diffusion coefficients are eta(*) = .1, .02, .01.
c The boundary conditions are u(i) = 0 at x = -1 and x = 1 for all i.
c The initial profile for each u(i) is a square wave:
c     u(i) = 0         on 1/2 .lt. abs(x) .le. 1
c     u(i) = amp(i)/2  on abs(x) = 1/2
c     u(i) = amp(i)    on 0 .le. abs(x) .lt. 1/2
c where the amplitudes are amp(*) = .2, .3, .5.
c
c A simplified Galerkin treatment of the spatial variable x is used,
c with piecewise linear basis functions on a uniform mesh of 100
c intervals.  The result is a system of ODEs in the discrete values
c u(i,k) approximating u(i)  (i=1,2,3) at the interior points
c (k = 1,...,99).  The ODEs are:
c
c    .            .        .
c   (u(i,k-1) + 4 u(i,k) + u(i,k+1))/6  =
c
c     -(1/6dx) (c(k-1)dul(i) + 2c(k)(dul(i)+dur(i)) + c(k+1)dur(i))
c
c     + (eta(i)/dx**2) (dur(i) - dul(i))     (i=1,2,3,  k=1,...,99),
c
c where
c     c(j) = u(1,j)+u(2,j)+u(3,j),   dx = .02 = the interval size,
c     dul(i) = u(i,k) - u(i,k-1),   dur(i) = u(i,k+1) - u(i,k).
c Terms involving boundary values (subscripts 0 or 100) are dropped
c from the equations for k = 1 and k = 99 above.
c
c The problem is run for each of the 4 values of mf, and for two values
c of the tolerances.  Output is taken at t = .1, .2, .3, .4.
c Output is on unit lout, set to 6 in a data statement below.
c-----------------------------------------------------------------------
      external res, addabt, jacbt
      integer ncomp, nip, nm1
      integer i, io, istate, itol, iwork, jtol, lout, liw, lrw,
     1   meth, miter, mf, neq, nerr, nint, nout
      double precision eodsq, r6d
      double precision abermx, atol, dx, errfac, eta, hun, one,
     1   rtol, rwork, six, t, tinit, tlast, tout, tols, two, y, ydoti
      dimension eta(3), y(297), ydoti(297), tout(4), tols(2)
      dimension rwork(7447), iwork(317)
      dimension neq(1), rtol(1), atol(1)
c Pass problem parameters in the common block par.
      common /par/ r6d, eodsq(3), ncomp, nip, nm1
c
c Set problem parameters and run parameters
      data eta/0.1d0,0.02d0,0.01d0/, tinit/0.0d0/, tlast/0.4d0/
      data one/1.0d0/, two/2.0d0/, six/6.0d0/, hun/100.0d0/
      data tout/.10d0,.20d0,.30d0,.40d0/
      data lout/6/, nout/4/, lrw/7447/, liw/317/
      data itol/1/, tols/1.0d-3, 1.0d-6/
c
c Set mesh parameters nint, dxc etc.
      nint = 100
      ncomp = 3
      dx = two/nint
      r6d = one/(six*dx)
      do 10 i = 1,ncomp
 10     eodsq(i) = eta(i)/dx**2
      nip = nint - 1
      neq(1) = ncomp*nip
      nm1 = nip - 1
      iwork(1) = ncomp
      iwork(2) = nip
c
      nerr = 0
c
c Set the initial conditions (for output purposes only).
      call setic (nint, ncomp, y)
c
      write (lout,1000)
      write (lout,1100) (eta(i),i=1,ncomp), tinit, tlast, nint,
     1      ncomp, nip, neq(1)
      write (lout,1200)
      call edit (y, ncomp, nip, lout)
c
c The jtol loop is over error tolerances.
      do 200 jtol = 1,2
      rtol(1) = tols(jtol)
      atol(1) = rtol(1)
c
c The meth/miter loops cover 4 values of method flag mf.
      do 100 meth = 1,2
       do 100 miter = 1,2
        mf = 10*meth + miter
c
c Set the initial conditions.
        call setic (nint, ncomp, y)
        t = tinit
        istate = 0
c
        write (lout,1500)  rtol(1), atol(1), mf
c
c Loop over output times for each case
        do 80 io = 1,nout
c
          call dlsoibt (res, addabt,jacbt, neq, y, ydoti, t, tout(io),
     1     itol,rtol,atol, 1, istate, 0, rwork,lrw,iwork,liw, mf)
c
          write (lout,2000) t, rwork(11), iwork(14), iwork(11)
          if (io .eq. nout) call edit (y, ncomp, nip, lout)
c
c If istate is not 2 on return, print message and go to next case.
          if (istate .ne. 2) then
            write (lout,4000) mf, t, istate
            nerr = nerr + 1
            go to 100
            endif
c
 80       continue
c
c Print final statistics.
        write (lout,3000) mf, iwork(11), iwork(12), iwork(13),
     1         iwork(17), iwork(18)
c
c Estimate final error and print result.
        call maxerr (y, ncomp, nip, abermx)
        errfac = abermx/tols(jtol)
        if (errfac .lt. hun) then
          write (lout,5000) errfac
        else  
          write (lout,5100) errfac
          nerr = nerr + 1
          endif
 100    continue
 200  continue
c
      write (lout,6000) nerr
c      stop
c
 1000 format(/20x,' Demonstration Problem for DLSOIBT'//
     1   10x,'Galerkin method solution of system of 3 PDEs:'//
     2   10x,'  u(i)   =  -(u(1)+u(2)+u(3)) u(i)   +  eta(i) u(i)',
     3   5x,'(i=1,2,3)',/16x,'t',27x,'x',16x,'xx'//
     4   10x,'x interval is -1 to 1,  zero boundary conditions'/
     5   10x,'x discretized using piecewise linear basis functions')
 1100 format(/10x,'Fixed parameters are as follows:'/
     1       13x,'Diffusion coefficients are eta =',3d10.2/
     2       13x,'t0 = ',d12.5/13x,'tlast = ',d12.5/
     3       13x,'Uniform mesh, number of intervals =',i4/
     4       13x,'Block size mb =',i2/13x,'Number of blocks nb =',i4/
     5       13x,'ODE system size neq =',i5//)
c
 1200 format(/'Initial profiles:'/)
c
 1500 format(////90('*')//'Run with rtol =',d9.1,'  atol =',d9.1,
     1       '   mf =',i3///)
c
 2000 format(' At time t =',d12.5,'  current h =',d12.5,
     1       '  current order =',i2,'  current nst =',i5/)
c
 3000 format(//'Final statistics for mf = ',i2,':',
     1       i5,' steps,',i6,' res,',i6,' jacobians,'/
     2       30x,       'rwork size =',i6,',  iwork size =',i6)
c
 4000 format(//20x,'Final time reached for mf = ',i2,
     1       ' was t = ',d12.5/25x,'at which istate = ',i2//)
 5000 format('Final output is correct to within ',d9.2,
     1       '  times local error tolerance. ')
 5100 format('Final output is wrong by ',d9.2,
     1       '  times local error tolerance.')
 6000 format(//90('*')//'Run completed: ',i3,' errors encountered')
c
c end of main program for the DLSOIBT demonstration problem
      end
