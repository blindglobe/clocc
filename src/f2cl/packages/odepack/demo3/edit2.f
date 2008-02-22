
      subroutine edit2 (y, t, erm)
      integer i, j, k, ng
      double precision y, t, erm, alph1, alph2, a1, a2, er, ex, yt
      dimension y(*)
      data alph1/1.0d0/, alph2/1.0d0/, ng/5/
      erm = 0.0d0
      if (t .eq. 0.0d0) return
      ex = 0.0d0
      if (t .le. 30.0d0) ex = exp(-2.0d0*t)
      a2 = 1.0d0
      do 60 j = 1,ng
        a1 = 1.0d0
        do 50 i = 1,ng
          k = i + (j - 1)*ng
          yt = t**(i+j-2)*ex*a1*a2
          er = abs(y(k)-yt)
          erm = max(erm,er)
          a1 = a1*alph1/i
 50       continue
        a2 = a2*alph2/j
 60     continue
      return
      end
