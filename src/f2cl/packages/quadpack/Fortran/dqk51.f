      subroutine dqk51(f,a,b,result,abserr,resabs,resasc)
c***begin prologue  dqk51
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a1a2
c***keywords  51-point gauss-kronrod rules
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math & progr. div. - k.u.leuven
c***purpose  to compute i = integral of f over (a,b) with error
c                           estimate
c                       j = integral of abs(f) over (a,b)
c***description
c
c           integration rules
c           standard fortran subroutine
c           double precision version
c
c           parameters
c            on entry
c              f      - double precision
c                       function subroutine defining the integrand
c                       function f(x). the actual name for f needs to be
c                       declared e x t e r n a l in the calling program.
c
c              a      - double precision
c                       lower limit of integration
c
c              b      - double precision
c                       upper limit of integration
c
c            on return
c              result - double precision
c                       approximation to the integral i
c                       result is computed by applying the 51-point
c                       kronrod rule (resk) obtained by optimal addition
c                       of abscissae to the 25-point gauss rule (resg).
c
c              abserr - double precision
c                       estimate of the modulus of the absolute error,
c                       which should not exceed abs(i-result)
c
c              resabs - double precision
c                       approximation to the integral j
c
c              resasc - double precision
c                       approximation to the integral of abs(f-i/(b-a))
c                       over (a,b)
c
c***references  (none)
c***routines called  d1mach
c***end prologue  dqk51
c
      double precision a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,
     *  d1mach,epmach,f,fc,fsum,fval1,fval2,fv1,fv2,hlgth,resabs,resasc,
     *  resg,resk,reskh,result,uflow,wg,wgk,xgk
      integer j,jtw,jtwm1
      external f
c
      dimension fv1(25),fv2(25),xgk(26),wgk(26),wg(13)
c
c           the abscissae and weights are given for the interval (-1,1).
c           because of symmetry only the positive abscissae and their
c           corresponding weights are given.
c
c           xgk    - abscissae of the 51-point kronrod rule
c                    xgk(2), xgk(4), ...  abscissae of the 25-point
c                    gauss rule
c                    xgk(1), xgk(3), ...  abscissae which are optimally
c                    added to the 25-point gauss rule
c
c           wgk    - weights of the 51-point kronrod rule
c
c           wg     - weights of the 25-point gauss rule
c
c
c gauss quadrature weights and kronron quadrature abscissae and weights
c as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
c bell labs, nov. 1981.
c
      data wg  (  1) / 0.011393798501026287947902964113235d0 /
      data wg  (  2) / 0.026354986615032137261901815295299d0 /
      data wg  (  3) / 0.040939156701306312655623487711646d0 /
      data wg  (  4) / 0.054904695975835191925936891540473d0 /
      data wg  (  5) / 0.068038333812356917207187185656708d0 /
      data wg  (  6) / 0.080140700335001018013234959669111d0 /
      data wg  (  7) / 0.091028261982963649811497220702892d0 /
      data wg  (  8) / 0.100535949067050644202206890392686d0 /
      data wg  (  9) / 0.108519624474263653116093957050117d0 /
      data wg  ( 10) / 0.114858259145711648339325545869556d0 /
      data wg  ( 11) / 0.119455763535784772228178126512901d0 /
      data wg  ( 12) / 0.122242442990310041688959518945852d0 /
      data wg  ( 13) / 0.123176053726715451203902873079050d0 /
c
      data xgk (  1) / 0.999262104992609834193457486540341d0 /
      data xgk (  2) / 0.995556969790498097908784946893902d0 /
      data xgk (  3) / 0.988035794534077247637331014577406d0 /
      data xgk (  4) / 0.976663921459517511498315386479594d0 /
      data xgk (  5) / 0.961614986425842512418130033660167d0 /
      data xgk (  6) / 0.942974571228974339414011169658471d0 /
      data xgk (  7) / 0.920747115281701561746346084546331d0 /
      data xgk (  8) / 0.894991997878275368851042006782805d0 /
      data xgk (  9) / 0.865847065293275595448996969588340d0 /
      data xgk ( 10) / 0.833442628760834001421021108693570d0 /
      data xgk ( 11) / 0.797873797998500059410410904994307d0 /
      data xgk ( 12) / 0.759259263037357630577282865204361d0 /
      data xgk ( 13) / 0.717766406813084388186654079773298d0 /
      data xgk ( 14) / 0.673566368473468364485120633247622d0 /
      data xgk ( 15) / 0.626810099010317412788122681624518d0 /
      data xgk ( 16) / 0.577662930241222967723689841612654d0 /
      data xgk ( 17) / 0.526325284334719182599623778158010d0 /
      data xgk ( 18) / 0.473002731445714960522182115009192d0 /
      data xgk ( 19) / 0.417885382193037748851814394594572d0 /
      data xgk ( 20) / 0.361172305809387837735821730127641d0 /
      data xgk ( 21) / 0.303089538931107830167478909980339d0 /
      data xgk ( 22) / 0.243866883720988432045190362797452d0 /
      data xgk ( 23) / 0.183718939421048892015969888759528d0 /
      data xgk ( 24) / 0.122864692610710396387359818808037d0 /
      data xgk ( 25) / 0.061544483005685078886546392366797d0 /
      data xgk ( 26) / 0.000000000000000000000000000000000d0 /
c
      data wgk (  1) / 0.001987383892330315926507851882843d0 /
      data wgk (  2) / 0.005561932135356713758040236901066d0 /
      data wgk (  3) / 0.009473973386174151607207710523655d0 /
      data wgk (  4) / 0.013236229195571674813656405846976d0 /
      data wgk (  5) / 0.016847817709128298231516667536336d0 /
      data wgk (  6) / 0.020435371145882835456568292235939d0 /
      data wgk (  7) / 0.024009945606953216220092489164881d0 /
      data wgk (  8) / 0.027475317587851737802948455517811d0 /
      data wgk (  9) / 0.030792300167387488891109020215229d0 /
      data wgk ( 10) / 0.034002130274329337836748795229551d0 /
      data wgk ( 11) / 0.037116271483415543560330625367620d0 /
      data wgk ( 12) / 0.040083825504032382074839284467076d0 /
      data wgk ( 13) / 0.042872845020170049476895792439495d0 /
      data wgk ( 14) / 0.045502913049921788909870584752660d0 /
      data wgk ( 15) / 0.047982537138836713906392255756915d0 /
      data wgk ( 16) / 0.050277679080715671963325259433440d0 /
      data wgk ( 17) / 0.052362885806407475864366712137873d0 /
      data wgk ( 18) / 0.054251129888545490144543370459876d0 /
      data wgk ( 19) / 0.055950811220412317308240686382747d0 /
      data wgk ( 20) / 0.057437116361567832853582693939506d0 /
      data wgk ( 21) / 0.058689680022394207961974175856788d0 /
      data wgk ( 22) / 0.059720340324174059979099291932562d0 /
      data wgk ( 23) / 0.060539455376045862945360267517565d0 /
      data wgk ( 24) / 0.061128509717053048305859030416293d0 /
      data wgk ( 25) / 0.061471189871425316661544131965264d0 /
c       note: wgk (26) was calculated from the values of wgk(1..25)
      data wgk ( 26) / 0.061580818067832935078759824240066d0 /
c
c
c           list of major variables
c           -----------------------
c
c           centr  - mid point of the interval
c           hlgth  - half-length of the interval
c           absc   - abscissa
c           fval*  - function value
c           resg   - result of the 25-point gauss formula
c           resk   - result of the 51-point kronrod formula
c           reskh  - approximation to the mean value of f over (a,b),
c                    i.e. to i/(b-a)
c
c           machine dependent constants
c           ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c
c***first executable statement  dqk51
      epmach = d1mach(4)
      uflow = d1mach(1)
c
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
c
c           compute the 51-point kronrod approximation to
c           the integral, and estimate the absolute error.
c
      fc = f(centr)
      resg = wg(13)*fc
      resk = wgk(26)*fc
      resabs = dabs(resk)
      do 10 j=1,12
        jtw = j*2
        absc = hlgth*xgk(jtw)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j = 1,13
        jtwm1 = j*2-1
        absc = hlgth*xgk(jtwm1)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15 continue
      reskh = resk*0.5d+00
      resasc = wgk(26)*dabs(fc-reskh)
      do 20 j=1,25
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if(resasc.ne.0.0d+00.and.abserr.ne.0.0d+00)
     *  abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if(resabs.gt.uflow/(0.5d+02*epmach)) abserr = dmax1
     *  ((epmach*0.5d+02)*resabs,abserr)
      return
      end
