      subroutine dqng(f,a,b,epsabs,epsrel,result,abserr,neval,ier)
c***begin prologue  dqng
c***date written   800101   (yymmdd)
c***revision date  810101   (yymmdd)
c***category no.  h2a1a1
c***keywords  automatic integrator, smooth integrand,
c             non-adaptive, gauss-kronrod(patterson)
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl math & progr. div. - k.u.leuven
c           kahaner,david,nbs - modified (2/82)
c***purpose  the routine calculates an approximation result to a
c            given definite integral i = integral of f over (a,b),
c            hopefully satisfying following claim for accuracy
c            abs(i-result).le.max(epsabs,epsrel*abs(i)).
c***description
c
c non-adaptive integration
c standard fortran subroutine
c double precision version
c
c           f      - double precision
c                    function subprogram defining the integrand function
c                    f(x). the actual name for f needs to be declared
c                    e x t e r n a l in the driver program.
c
c           a      - double precision
c                    lower limit of integration
c
c           b      - double precision
c                    upper limit of integration
c
c           epsabs - double precision
c                    absolute accuracy requested
c           epsrel - double precision
c                    relative accuracy requested
c                    if  epsabs.le.0
c                    and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
c                    the routine will end with ier = 6.
c
c         on return
c           result - double precision
c                    approximation to the integral i
c                    result is obtained by applying the 21-point
c                    gauss-kronrod rule (res21) obtained by optimal
c                    addition of abscissae to the 10-point gauss rule
c                    (res10), or by applying the 43-point rule (res43)
c                    obtained by optimal addition of abscissae to the
c                    21-point gauss-kronrod rule, or by applying the
c                    87-point rule (res87) obtained by optimal addition
c                    of abscissae to the 43-point rule.
c
c           abserr - double precision
c                    estimate of the modulus of the absolute error,
c                    which should equal or exceed abs(i-result)
c
c           neval  - integer
c                    number of integrand evaluations
c
c           ier    - ier = 0 normal and reliable termination of the
c                            routine. it is assumed that the requested
c                            accuracy has been achieved.
c                    ier.gt.0 abnormal termination of the routine. it is
c                            assumed that the requested accuracy has
c                            not been achieved.
c           error messages
c                    ier = 1 the maximum number of steps has been
c                            executed. the integral is probably too
c                            difficult to be calculated by dqng.
c                        = 6 the input is invalid, because
c                            epsabs.le.0 and
c                            epsrel.lt.max(50*rel.mach.acc.,0.5d-28).
c                            result, abserr and neval are set to zero.
c
c***references  (none)
c***routines called  d1mach,xerror
c***end prologue  dqng
c
      double precision a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,
     *  d1mach,epmach,epsabs,epsrel,f,fcentr,fval,fval1,fval2,fv1,fv2,
     *  fv3,fv4,hlgth,result,res10,res21,res43,res87,resabs,resasc,
     *  reskh,savfun,uflow,w10,w21a,w21b,w43a,w43b,w87a,w87b,x1,x2,x3,x4
      integer ier,ipx,k,l,neval
      external f
c
      dimension fv1(5),fv2(5),fv3(5),fv4(5),x1(5),x2(5),x3(11),x4(22),
     *  w10(5),w21a(5),w21b(6),w43a(10),w43b(12),w87a(21),w87b(23),
     *  savfun(21)
c
c           the following data statements contain the
c           abscissae and weights of the integration rules used.
c
c           x1      abscissae common to the 10-, 21-, 43- and 87-
c                   point rule
c           x2      abscissae common to the 21-, 43- and 87-point rule
c           x3      abscissae common to the 43- and 87-point rule
c           x4      abscissae of the 87-point rule
c           w10     weights of the 10-point formula
c           w21a    weights of the 21-point formula for abscissae x1
c           w21b    weights of the 21-point formula for abscissae x2
c           w43a    weights of the 43-point formula for abscissae x1, x3
c           w43b    weights of the 43-point formula for abscissae x3
c           w87a    weights of the 87-point formula for abscissae x1,
c                   x2, x3
c           w87b    weights of the 87-point formula for abscissae x4
c
c
c gauss-kronrod-patterson quadrature coefficients for use in
c quadpack routine qng.  these coefficients were calculated with
c 101 decimal digit arithmetic by l. w. fullerton, bell labs, nov 1981.
c
      data x1    (  1) / 0.973906528517171720077964012084452d0 /
      data x1    (  2) / 0.865063366688984510732096688423493d0 /
      data x1    (  3) / 0.679409568299024406234327365114874d0 /
      data x1    (  4) / 0.433395394129247190799265943165784d0 /
      data x1    (  5) / 0.148874338981631210884826001129720d0 /
      data w10   (  1) / 0.066671344308688137593568809893332d0 /
      data w10   (  2) / 0.149451349150580593145776339657697d0 /
      data w10   (  3) / 0.219086362515982043995534934228163d0 /
      data w10   (  4) / 0.269266719309996355091226921569469d0 /
      data w10   (  5) / 0.295524224714752870173892994651338d0 /
c
      data x2    (  1) / 0.995657163025808080735527280689003d0 /
      data x2    (  2) / 0.930157491355708226001207180059508d0 /
      data x2    (  3) / 0.780817726586416897063717578345042d0 /
      data x2    (  4) / 0.562757134668604683339000099272694d0 /
      data x2    (  5) / 0.294392862701460198131126603103866d0 /
      data w21a  (  1) / 0.032558162307964727478818972459390d0 /
      data w21a  (  2) / 0.075039674810919952767043140916190d0 /
      data w21a  (  3) / 0.109387158802297641899210590325805d0 /
      data w21a  (  4) / 0.134709217311473325928054001771707d0 /
      data w21a  (  5) / 0.147739104901338491374841515972068d0 /
      data w21b  (  1) / 0.011694638867371874278064396062192d0 /
      data w21b  (  2) / 0.054755896574351996031381300244580d0 /
      data w21b  (  3) / 0.093125454583697605535065465083366d0 /
      data w21b  (  4) / 0.123491976262065851077958109831074d0 /
      data w21b  (  5) / 0.142775938577060080797094273138717d0 /
      data w21b  (  6) / 0.149445554002916905664936468389821d0 /
c
      data x3    (  1) / 0.999333360901932081394099323919911d0 /
      data x3    (  2) / 0.987433402908088869795961478381209d0 /
      data x3    (  3) / 0.954807934814266299257919200290473d0 /
      data x3    (  4) / 0.900148695748328293625099494069092d0 /
      data x3    (  5) / 0.825198314983114150847066732588520d0 /
      data x3    (  6) / 0.732148388989304982612354848755461d0 /
      data x3    (  7) / 0.622847970537725238641159120344323d0 /
      data x3    (  8) / 0.499479574071056499952214885499755d0 /
      data x3    (  9) / 0.364901661346580768043989548502644d0 /
      data x3    ( 10) / 0.222254919776601296498260928066212d0 /
      data x3    ( 11) / 0.074650617461383322043914435796506d0 /
      data w43a  (  1) / 0.016296734289666564924281974617663d0 /
      data w43a  (  2) / 0.037522876120869501461613795898115d0 /
      data w43a  (  3) / 0.054694902058255442147212685465005d0 /
      data w43a  (  4) / 0.067355414609478086075553166302174d0 /
      data w43a  (  5) / 0.073870199632393953432140695251367d0 /
      data w43a  (  6) / 0.005768556059769796184184327908655d0 /
      data w43a  (  7) / 0.027371890593248842081276069289151d0 /
      data w43a  (  8) / 0.046560826910428830743339154433824d0 /
      data w43a  (  9) / 0.061744995201442564496240336030883d0 /
      data w43a  ( 10) / 0.071387267268693397768559114425516d0 /
      data w43b  (  1) / 0.001844477640212414100389106552965d0 /
      data w43b  (  2) / 0.010798689585891651740465406741293d0 /
      data w43b  (  3) / 0.021895363867795428102523123075149d0 /
      data w43b  (  4) / 0.032597463975345689443882222526137d0 /
      data w43b  (  5) / 0.042163137935191811847627924327955d0 /
      data w43b  (  6) / 0.050741939600184577780189020092084d0 /
      data w43b  (  7) / 0.058379395542619248375475369330206d0 /
      data w43b  (  8) / 0.064746404951445885544689259517511d0 /
      data w43b  (  9) / 0.069566197912356484528633315038405d0 /
      data w43b  ( 10) / 0.072824441471833208150939535192842d0 /
      data w43b  ( 11) / 0.074507751014175118273571813842889d0 /
      data w43b  ( 12) / 0.074722147517403005594425168280423d0 /
c
      data x4    (  1) / 0.999902977262729234490529830591582d0 /
      data x4    (  2) / 0.997989895986678745427496322365960d0 /
      data x4    (  3) / 0.992175497860687222808523352251425d0 /
      data x4    (  4) / 0.981358163572712773571916941623894d0 /
      data x4    (  5) / 0.965057623858384619128284110607926d0 /
      data x4    (  6) / 0.943167613133670596816416634507426d0 /
      data x4    (  7) / 0.915806414685507209591826430720050d0 /
      data x4    (  8) / 0.883221657771316501372117548744163d0 /
      data x4    (  9) / 0.845710748462415666605902011504855d0 /
      data x4    ( 10) / 0.803557658035230982788739474980964d0 /
      data x4    ( 11) / 0.757005730685495558328942793432020d0 /
      data x4    ( 12) / 0.706273209787321819824094274740840d0 /
      data x4    ( 13) / 0.651589466501177922534422205016736d0 /
      data x4    ( 14) / 0.593223374057961088875273770349144d0 /
      data x4    ( 15) / 0.531493605970831932285268948562671d0 /
      data x4    ( 16) / 0.466763623042022844871966781659270d0 /
      data x4    ( 17) / 0.399424847859218804732101665817923d0 /
      data x4    ( 18) / 0.329874877106188288265053371824597d0 /
      data x4    ( 19) / 0.258503559202161551802280975429025d0 /
      data x4    ( 20) / 0.185695396568346652015917141167606d0 /
      data x4    ( 21) / 0.111842213179907468172398359241362d0 /
      data x4    ( 22) / 0.037352123394619870814998165437704d0 /
      data w87a  (  1) / 0.008148377384149172900002878448190d0 /
      data w87a  (  2) / 0.018761438201562822243935059003794d0 /
      data w87a  (  3) / 0.027347451050052286161582829741283d0 /
      data w87a  (  4) / 0.033677707311637930046581056957588d0 /
      data w87a  (  5) / 0.036935099820427907614589586742499d0 /
      data w87a  (  6) / 0.002884872430211530501334156248695d0 /
      data w87a  (  7) / 0.013685946022712701888950035273128d0 /
      data w87a  (  8) / 0.023280413502888311123409291030404d0 /
      data w87a  (  9) / 0.030872497611713358675466394126442d0 /
      data w87a  ( 10) / 0.035693633639418770719351355457044d0 /
      data w87a  ( 11) / 0.000915283345202241360843392549948d0 /
      data w87a  ( 12) / 0.005399280219300471367738743391053d0 /
      data w87a  ( 13) / 0.010947679601118931134327826856808d0 /
      data w87a  ( 14) / 0.016298731696787335262665703223280d0 /
      data w87a  ( 15) / 0.021081568889203835112433060188190d0 /
      data w87a  ( 16) / 0.025370969769253827243467999831710d0 /
      data w87a  ( 17) / 0.029189697756475752501446154084920d0 /
      data w87a  ( 18) / 0.032373202467202789685788194889595d0 /
      data w87a  ( 19) / 0.034783098950365142750781997949596d0 /
      data w87a  ( 20) / 0.036412220731351787562801163687577d0 /
      data w87a  ( 21) / 0.037253875503047708539592001191226d0 /
      data w87b  (  1) / 0.000274145563762072350016527092881d0 /
      data w87b  (  2) / 0.001807124155057942948341311753254d0 /
      data w87b  (  3) / 0.004096869282759164864458070683480d0 /
      data w87b  (  4) / 0.006758290051847378699816577897424d0 /
      data w87b  (  5) / 0.009549957672201646536053581325377d0 /
      data w87b  (  6) / 0.012329447652244853694626639963780d0 /
      data w87b  (  7) / 0.015010447346388952376697286041943d0 /
      data w87b  (  8) / 0.017548967986243191099665352925900d0 /
      data w87b  (  9) / 0.019938037786440888202278192730714d0 /
      data w87b  ( 10) / 0.022194935961012286796332102959499d0 /
      data w87b  ( 11) / 0.024339147126000805470360647041454d0 /
      data w87b  ( 12) / 0.026374505414839207241503786552615d0 /
      data w87b  ( 13) / 0.028286910788771200659968002987960d0 /
      data w87b  ( 14) / 0.030052581128092695322521110347341d0 /
      data w87b  ( 15) / 0.031646751371439929404586051078883d0 /
      data w87b  ( 16) / 0.033050413419978503290785944862689d0 /
      data w87b  ( 17) / 0.034255099704226061787082821046821d0 /
      data w87b  ( 18) / 0.035262412660156681033782717998428d0 /
      data w87b  ( 19) / 0.036076989622888701185500318003895d0 /
      data w87b  ( 20) / 0.036698604498456094498018047441094d0 /
      data w87b  ( 21) / 0.037120549269832576114119958413599d0 /
      data w87b  ( 22) / 0.037334228751935040321235449094698d0 /
      data w87b  ( 23) / 0.037361073762679023410321241766599d0 /
c
c           list of major variables
c           -----------------------
c
c           centr  - mid point of the integration interval
c           hlgth  - half-length of the integration interval
c           fcentr - function value at mid point
c           absc   - abscissa
c           fval   - function value
c           savfun - array of function values which have already been
c                    computed
c           res10  - 10-point gauss result
c           res21  - 21-point kronrod result
c           res43  - 43-point result
c           res87  - 87-point result
c           resabs - approximation to the integral of abs(f)
c           resasc - approximation to the integral of abs(f-i/(b-a))
c
c           machine dependent constants
c           ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c
c***first executable statement  dqng
      epmach = d1mach(4)
      uflow = d1mach(1)
c
c           test on validity of parameters
c           ------------------------------
c
      result = 0.0d+00
      abserr = 0.0d+00
      neval = 0
      ier = 6
      if(epsabs.le.0.0d+00.and.epsrel.lt.dmax1(0.5d+02*epmach,0.5d-28))
     *  go to 80
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
      centr = 0.5d+00*(b+a)
      fcentr = f(centr)
      neval = 21
      ier = 1
c
c          compute the integral using the 10- and 21-point formula.
c
      do 70 l = 1,3
      go to (5,25,45),l
    5 res10 = 0.0d+00
      res21 = w21b(6)*fcentr
      resabs = w21b(6)*dabs(fcentr)
      do 10 k=1,5
        absc = hlgth*x1(k)
        fval1 = f(centr+absc)
        fval2 = f(centr-absc)
        fval = fval1+fval2
        res10 = res10+w10(k)*fval
        res21 = res21+w21a(k)*fval
        resabs = resabs+w21a(k)*(dabs(fval1)+dabs(fval2))
        savfun(k) = fval
        fv1(k) = fval1
        fv2(k) = fval2
   10 continue
      ipx = 5
      do 15 k=1,5
        ipx = ipx+1
        absc = hlgth*x2(k)
        fval1 = f(centr+absc)
        fval2 = f(centr-absc)
        fval = fval1+fval2
        res21 = res21+w21b(k)*fval
        resabs = resabs+w21b(k)*(dabs(fval1)+dabs(fval2))
        savfun(ipx) = fval
        fv3(k) = fval1
        fv4(k) = fval2
   15 continue
c
c          test for convergence.
c
      result = res21*hlgth
      resabs = resabs*dhlgth
      reskh = 0.5d+00*res21
      resasc = w21b(6)*dabs(fcentr-reskh)
      do 20 k = 1,5
        resasc = resasc+w21a(k)*(dabs(fv1(k)-reskh)+dabs(fv2(k)-reskh))
     *                  +w21b(k)*(dabs(fv3(k)-reskh)+dabs(fv4(k)-reskh))
   20 continue
      abserr = dabs((res21-res10)*hlgth)
      resasc = resasc*dhlgth
      go to 65
c
c          compute the integral using the 43-point formula.
c
   25 res43 = w43b(12)*fcentr
      neval = 43
      do 30 k=1,10
        res43 = res43+savfun(k)*w43a(k)
   30 continue
      do 40 k=1,11
        ipx = ipx+1
        absc = hlgth*x3(k)
        fval = f(absc+centr)+f(centr-absc)
        res43 = res43+fval*w43b(k)
        savfun(ipx) = fval
   40 continue
c
c          test for convergence.
c
      result = res43*hlgth
      abserr = dabs((res43-res21)*hlgth)
      go to 65
c
c          compute the integral using the 87-point formula.
c
   45 res87 = w87b(23)*fcentr
      neval = 87
      do 50 k=1,21
        res87 = res87+savfun(k)*w87a(k)
   50 continue
      do 60 k=1,22
        absc = hlgth*x4(k)
        res87 = res87+w87b(k)*(f(absc+centr)+f(centr-absc))
   60 continue
      result = res87*hlgth
      abserr = dabs((res87-res43)*hlgth)
   65 if(resasc.ne.0.0d+00.and.abserr.ne.0.0d+00)
     *  abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs.gt.uflow/(0.5d+02*epmach)) abserr = dmax1
     *  ((epmach*0.5d+02)*resabs,abserr)
      if (abserr.le.dmax1(epsabs,epsrel*dabs(result))) ier = 0
c ***jump out of do-loop
      if (ier.eq.0) go to 999
   70 continue
   80 call xerror('abnormal return from dqng ',26,ier,0)
  999 return
      end
