TOMS419_F = calct.f \
	cauchy.f \
	cdivid.f \
	cmod.f \
	cpoly.f \
	cpolydr.f \
	errev.f \
	fxshft.f \
	mcon.f \
	nexth.f \
	noshft.f \
	polyev.f \
	prtc.f \
	prtz.f \
	scale.f \
	vrshft.f

cpolydr : cpolydr.o prtc.o prtz.o $(TOMS419_F:.f=.o)
	$(F77) -o $@ $^

cpolydr.out : cpolydr
	cpolydr | tee $@
