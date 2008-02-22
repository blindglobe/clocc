
      subroutine res (n, t, y, v, r, ires)
c This subroutine computes the residual vector
c   r = g(t,y) - A(t,y)*v
c using routines gfun and subav.
c If ires = -1, only g(t,y) is returned in r, since A(t,y) does
c not depend on y.
c No changes need to be made to this routine if nip is changed.
c
      integer ires, n,  ncomp, nip, nm1
      double precision t, y, v, r, r6d, eodsq
      dimension y(*), v(*), r(*)
      dimension n(*)
      common /par/ r6d, eodsq(3), ncomp, nip, nm1
c
      call gfun (t, y, r, ncomp)
      if (ires .eq. -1) return
c
      call subav (r, v, ncomp)
c
      return
c end of subroutine res
      end
