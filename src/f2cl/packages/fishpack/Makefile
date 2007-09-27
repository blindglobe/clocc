FISHPACK_SRC = \
	blktr1.f \
	blktri.f \
	bsrh.f \
	chkpr4.f \
	chksn4.f \
	cofx.f \
	compb.f \
	cosgen.f \
	cprod.f \
	cprodp.f \
	defe4.f \
	dx4.f \
	dy4.f \
	epmach.f \
	genbun.f \
	hstcrt.f \
	hstcs1.f \
	hstcsp.f \
	hstcyl.f \
	hstplr.f \
	hstssp.f \
	hw3crt.f \
	hwscrt.f \
	hwscs1.f \
	hwscsp.f \
	hwscyl.f \
	hwsplr.f \
	hwsss1.f \
	hwsssp.f \
	indxa.f \
	indxb.f \
	indxc.f \
	junk.f \
	merge.f \
	minso4.f \
	ortho4.f \
	pimach.f \
	pois3d.f \
	poisd2.f \
	poisn2.f \
	poisp2.f \
	poistg.f \
	pos3d1.f \
	postg2.f \
	ppadd.f \
	ppsgf.f \
	ppspf.f \
	prod.f \
	prodp.f \
	psgf.f \
	sepx4.f \
	speli4.f \
	store.f \
	tevls.f \
	tri3.f \
	trid.f \
	tris4.f \
	trix.f

all : tsthwscrt tsthwscyl tsthwsplr tsthwsssp tsthwscsp tsthstplr tsthstcyl tsthstssp tsthstcsp tstsepx4

tsthwscrt : ex/tsthwscrt.o libfishpack.a
	$(F77) -o $@ $^

tsthwscyl : ex/tsthwscyl.o libfishpack.a
	$(F77) -o $@ $^

tsthwsplr : ex/tsthwsplr.o libfishpack.a
	$(F77) -o $@ $^

tsthwsssp : ex/tsthwsssp.o libfishpack.a
	$(F77) -o $@ $^

tsthwscsp : ex/tsthwscsp.o libfishpack.a
	$(F77) -o $@ $^

tsthstcrt : ex/tsthstcrt.o libfishpack.a
	$(F77) -o $@ $^

tsthstplr : ex/tsthstplr.o libfishpack.a
	$(F77) -o $@ $^

tsthstcyl : ex/tsthstcyl.o libfishpack.a
	$(F77) -o $@ $^

tsthstssp : ex/tsthstssp.o libfishpack.a
	$(F77) -o $@ $^

tsthstcsp : ex/tsthstcsp.o libfishpack.a
	$(F77) -o $@ $^

tsthw3crt : ex/tsthw3crt.o libfishpack.a
	$(F77) -o $@ $^

tstsepx4 : ex/tstsepx4.o ex/cofx4.o libfishpack.a
	$(F77) -o $@ $^

libfishpack.a : $(FISHPACK_SRC:.f=.o)
	ar r $@ $^
