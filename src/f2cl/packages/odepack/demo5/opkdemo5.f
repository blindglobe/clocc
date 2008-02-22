      program opkdemo5
c-----------------------------------------------------------------------
c Demonstration program for DLSODPK.
c ODE system from ns-species interaction pde in 2 dimensions.
c This is the version of 14 June 2001.
c
c This version is in double precision.
c-----------------------------------------------------------------------
c This program solves a stiff ODE system that arises from a system
c of partial differential equations.  The PDE system is a food web
c population model, with predator-prey interaction and diffusion on
c the unit square in two dimensions.  The dependent variable vector is
c
c         1   2        ns
c   c = (c , c , ..., c  )
c
c and the PDEs are as follows:
c
c     i               i      i
c   dc /dt  =  d(i)*(c    + c   )  +  f (x,y,c)  (i=1,...,ns)
c                     xx     yy        i
c
c where
c                  i          ns         j
c   f (x,y,c)  =  c *(b(i) + sum a(i,j)*c )
c    i                       j=1
c
c The number of species is ns = 2*np, with the first np being prey and
c the last np being predators.  The coefficients a(i,j), b(i), d(i) are:
c
c   a(i,i) = -a  (all i)
c   a(i,j) = -g  (i .le. np, j .gt. np)
c   a(i,j) =  e  (i .gt. np, j .le. np)
c   b(i) =  b*(1 + alpha*x*y)  (i .le. np)
c   b(i) = -b*(1 + alpha*x*y)  (i .gt. np)
c   d(i) = dprey  (i .le. np)
c   d(i) = dpred  (i .gt. np)
c
c The various scalar parameters are set in subroutine setpar.
c
c The boundary conditions are: normal derivative = 0.
c A polynomial in x and y is used to set the initial conditions.
c
c The PDEs are discretized by central differencing on a mx by my mesh.
c
c The ODE system is solved by DLSODPK using method flag values
c mf = 10, 21, 22, 23, 24, 29.  The final time is tmax = 10, except
c that for mf = 10 it is tmax = 1.0d-3 because the problem is stiff,
c and for mf = 23 and 24 it is tmax = 2 because the lack of symmetry
c in the problem makes these methods more costly.
c
c Two preconditioner matrices are used.  One uses a fixed number of
c Gauss-Seidel iterations based on the diffusion terms only.
c The other preconditioner is a block-diagonal matrix based on
c the partial derivatives of the interaction terms f only, using
c block-grouping (computing only a subset of the ns by ns blocks).
c For mf = 21 and 22, these two preconditioners are applied on
c the left and right, respectively, and for mf = 23 and 24 the product
c of the two is used as the one preconditioner matrix.
c For mf = 29, the inverse of the product is applied.
c
c Two output files are written: one with the problem description and
c and performance statistics on unit 6, and one with solution profiles
c at selected output times (for mf = 22 only) on unit 8.
c-----------------------------------------------------------------------
c Note: In addition to the main program and 10 subroutines
c given below, this program requires the LINPACK subroutines
c DGEFA and DGESL, and the BLAS routine DAXPY.
c-----------------------------------------------------------------------
c Reference:
c     Peter N. Brown and Alan C. Hindmarsh,
c     Reduced Storage Matrix Methods in Stiff ODE Systems,
c     J. Appl. Math. & Comp., 31 (1989), pp. 40-91;
c     Also LLNL Report UCRL-95088, Rev. 1, June 1987.
c-----------------------------------------------------------------------
      external fweb, jacbg, solsbg
      integer ns, mx, my, mxns,
     1        mp, mq, mpsq, itmax,
     2        meshx,meshy,ngx,ngy,ngrp,mxmp,jgx,jgy,jigx,jigy,jxr,jyr
      integer i, imod3, iopt, iout, istate, itask, itol, iwork,
     1   jacflg, jpre, leniw, lenrw, liw, lrw, mf,
     2   ncfl, ncfn, neq, nfe, nfldif, nfndif, nli, nlidif, nni, nnidif,
     3   nout, npe, nps, nqu, nsdif, nst
      double precision aa, ee, gg, bb, dprey, dpred,
     1     ax, ay, acoef, bcoef, dx, dy, alph, diff, cox, coy,
     2     uround, srur
      double precision avdim, atol, cc, hu, rcfl, rcfn, dumach,
     1   rtol, rwork, t, tout
      dimension neq(1), atol(1), rtol(1)
c
c The problem Common blocks below allow for up to 20 species,
c up to a 50x50 mesh, and up to a 20x20 group structure.
      common /pcom0/ aa, ee, gg, bb, dprey, dpred
      common /pcom1/ ax, ay, acoef(20,20), bcoef(20), dx, dy, alph,
     1               diff(20), cox(20), coy(20), ns, mx, my, mxns
      common /pcom2/ uround, srur, mp, mq, mpsq, itmax
      common /pcom3/ meshx, meshy, ngx, ngy, ngrp, mxmp,
     2   jgx(21), jgy(21), jigx(50), jigy(50), jxr(20), jyr(20)
c
c The dimension of cc below must be .ge. 2*neq, where neq = ns*mx*my.
c The dimension lrw of rwork must be .ge. 17*neq + ns*ns*ngrp + 61,
c and the dimension liw of iwork must be .ge. 35 + ns*ngrp.
      dimension cc(576), rwork(5213), iwork(67)
      data lrw/5213/, liw/67/
c
      open (unit=6, file='demout', status='new')
      open (unit=8, file='ccout', status='new')
c
      ax = 1.0d0
      ay = 1.0d0
c
c Call setpar to set problem parameters.
      call setpar
c
c Set remaining problem parameters.
      neq(1) = ns*mx*my
      mxns = mx*ns
      dx = ax/(mx-1)
      dy = ay/(my-1)
      do 10 i = 1,ns
        cox(i) = diff(i)/dx**2
 10     coy(i) = diff(i)/dy**2
c
c Write heading.
      write(6,20)ns, mx,my,neq(1)
 20   format(' Demonstration program for DLSODPK package'//
     1   ' Food web problem with ns species, ns =',i4/
     2   ' Predator-prey interaction and diffusion on a 2-d square'//
     3   ' Mesh dimensions (mx,my) =',2i4/
     4   ' Total system size is neq =',i7//)
      write(6,25) aa,ee,gg,bb,dprey,dpred,alph
 25   format(' Matrix parameters:  a =',d12.4,'   e =',d12.4,
     1   '   g =',d12.4/20x,' b =',d12.4//
     2   ' Diffusion coefficients: dprey =',d12.4,'   dpred =',d12.4/
     3   ' Rate parameter alpha =',d12.4//)
c
c Set remaining method parameters.
      jpre = 3
      jacflg = 1
      iwork(3) = jpre
      iwork(4) = jacflg
      iopt = 0
      mp = ns
      mq = mx*my
      mpsq = ns*ns
      uround = dumach()
      srur = sqrt(uround)
      meshx = mx
      meshy = my
      mxmp = meshx*mp
      ngx = 2
      ngy = 2
      ngrp = ngx*ngy
      call gset (meshx, ngx, jgx, jigx, jxr)
      call gset (meshy, ngy, jgy, jigy, jyr)
      iwork(1) = mpsq*ngrp
      iwork(2) = mp*ngrp
      itmax = 5
      itol = 1
      rtol(1) = 1.0d-5
      atol(1) = rtol(1)
      itask = 1
      write(6,30)ngrp,ngx,ngy,itmax,rtol(1),atol(1)
 30   format(' Preconditioning uses interaction-only block-diagonal',
     1   ' matrix'/' with block-grouping, and Gauss-Seidel iterations'//
     2   ' Number of diagonal block groups = ngrp =',i4,
     3   '   (ngx by ngy, ngx =',i2,'  ngy =',i2,' )'//
     4   ' G-S preconditioner uses itmax iterations, itmax =',i3//
     5   ' Tolerance parameters: rtol =',d10.2,'   atol =',d10.2)
c
c
c Loop over mf values 10, 21, ..., 24, 29.
c
      do 90 mf = 10,29
      if (mf .gt. 10 .and. mf .lt. 21) go to 90
      if (mf .gt. 24 .and. mf .lt. 29) go to 90
      write(6,40)mf
 40   format(//80('-')//' Solution with mf =',i3//
     1   '   t       nstep  nfe  nni  nli  npe  nq',
     2   4x,'h          avdim    ncf rate    lcf rate')
c
      t = 0.0d0
      tout = 1.0d-8
      nout = 18
      if (mf .eq. 10) nout = 6
      if (mf .eq. 23 .or. mf .eq. 24) nout = 10
      call cinit (cc)
      if (mf .eq. 22) call outweb (t, cc, ns, mx, my, 8)
      istate = 1
      nli = 0
      nni = 0
      ncfn = 0
      ncfl = 0
      nst = 0
c
c Loop over output times and call DLSODPK.
c
      do 70 iout = 1,nout
        call dlsodpk (fweb, neq, cc, t, tout, itol, rtol, atol, itask,
     1         istate, iopt, rwork, lrw, iwork, liw, jacbg, solsbg, mf)
        nsdif = iwork(11) - nst
        nst = iwork(11)
        nfe = iwork(12)
        npe = iwork(13)
        nnidif = iwork(19) - nni
        nni = iwork(19)
        nlidif = iwork(20) - nli
        nli = iwork(20)
        nfndif = iwork(22) - ncfn
        ncfn = iwork(22)
        nfldif = iwork(23) - ncfl
        ncfl = iwork(23)
        nqu = iwork(14)
        hu = rwork(11)
        avdim = 0.0d0
        rcfn = 0.0d0
        rcfl = 0.0d0
        if (nnidif .gt. 0) avdim = real(nlidif)/real(nnidif)
        if (nsdif .gt. 0) rcfn = real(nfndif)/real(nsdif)
        if (nnidif .gt. 0) rcfl = real(nfldif)/real(nnidif)
        write(6,50)t,nst,nfe,nni,nli,npe,nqu,hu,avdim,rcfn,rcfl
 50     format(d10.2,i5,i6,3i5,i4,2d11.2,d10.2,d12.2)
        imod3 = iout - 3*(iout/3)
        if (mf .eq. 22 .and. imod3 .eq. 0) call outweb (t,cc,ns,mx,my,8)
        if (istate .eq. 2) go to 65
        write(6,60)t
 60     format(//' final time reached =',d12.4//)
        go to 75
 65     continue
        if (tout .gt. 0.9d0) tout = tout + 1.0d0
        if (tout .lt. 0.9d0) tout = tout*10.0d0
 70     continue
c
 75   continue
      nst = iwork(11)
      nfe = iwork(12)
      npe = iwork(13)
      lenrw = iwork(17)
      leniw = iwork(18)
      nni = iwork(19)
      nli = iwork(20)
      nps = iwork(21)
      if (nni .gt. 0) avdim = real(nli)/real(nni)
      ncfn = iwork(22)
      ncfl = iwork(23)
      write (6,80) lenrw,leniw,nst,nfe,npe,nps,nni,nli,avdim,
     1               ncfn,ncfl
 80   format(//' Final statistics for this run:'/
     1   ' rwork size =',i8,'   iwork size =',i6/
     2   ' number of time steps            =',i5/
     3   ' number of f evaluations         =',i5/
     4   ' number of preconditioner evals. =',i5/
     4   ' number of preconditioner solves =',i5/
     5   ' number of nonlinear iterations  =',i5/
     5   ' number of linear iterations     =',i5/
     6   ' average subspace dimension  =',f8.4/
     7   i5,' nonlinear conv. failures,',i5,' linear conv. failures')
c
 90   continue
c      stop
c------  end of main program for DLSODPK demonstration program ----------
      end
