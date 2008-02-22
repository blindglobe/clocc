
      subroutine gset (m, ng, jg, jig, jr)
c-----------------------------------------------------------------------
c This routine sets arrays jg, jig, and jr describing
c a uniform partition of (1,2,...,m) into ng groups.
c-----------------------------------------------------------------------
      integer m, ng, jg, jig, jr
      dimension jg(*), jig(*), jr(*)
      integer ig, j, len1, mper, ngm1
c
      mper = m/ng
      do 10 ig = 1,ng
 10     jg(ig) = 1 + (ig - 1)*mper
      jg(ng+1) = m + 1
c
      ngm1 = ng - 1
      len1 = ngm1*mper
      do 20 j = 1,len1
 20     jig(j) = 1 + (j-1)/mper
      len1 = len1 + 1
      do 25 j = len1,m
 25     jig(j) = ng
c
      do 30 ig = 1,ngm1
 30     jr(ig) = 0.5d0 + (ig - 0.5d0)*mper
      jr(ng) = 0.5d0*(1 + ngm1*mper + m)
c
      return
c------------  end of subroutine gset  ---------------------------------
      end
