ODEPACK = \
	adjlr.f \
	cdrv.f \
	cntnzu.f \
	daigbt.f \
	dainvg.f \
	dainvgs.f \
	datp.f \
	datv.f \
	daxpy.f \
	dbnorm.f \
	dcfode.f \
	dcopy.f \
	ddecbt.f \
	ddot.f \
	dewset.f \
	dfnorm.f \
	dgbfa.f \
	dgbsl.f \
	dgefa.f \
	dgesl.f \
	dhefa.f \
	dhels.f \
	dheqr.f \
	dhesl.f \
	dintdy.f \
	diprep.f \
	diprepi.f \
	dlhin.f \
	dlsoda.f \
	dlsodar.f \
	dlsode.f \
	dlsodes.f \
	dlsodi.f \
	dlsodis.f \
	dlsodkr.f \
	dlsodpk.f \
	dlsoibt.f \
	dmnorm.f \
	dnrm2.f \
	dorthog.f \
	dpcg.f \
	dpcgs.f \
	dpjibt.f \
	dpkset.f \
	dprep.f \
	dprepi.f \
	dprepj.f \
	dprepji.f \
	dprja.f \
	dprjis.f \
	dprjs.f \
	drchek.f \
	droots.f \
	dscal.f \
	dsetpk.f \
	dslsbt.f \
	dsolbt.f \
	dsolpk.f \
	dsolss.f \
	dsolsy.f \
	dspigmr.f \
	dspiom.f \
	dsrcar.f \
	dsrckr.f \
	dsrcma.f \
	dsrcms.f \
	dsrcom.f \
	dsrcpk.f \
	dstoda.f \
	dstode.f \
	dstodi.f \
	dstodpk.f \
	dstoka.f \
	dumach.f \
	dumsum.f \
	dusol.f \
	dvnorm.f \
	idamax.f \
	iumach.f \
	ixsav.f \
	jgroup.f \
	md.f \
	mdi.f \
	mdm.f \
	mdp.f \
	mdu.f \
	nnfc.f \
	nnsc.f \
	nntc.f \
	nroc.f \
	nsfc.f \
	odrv.f \
	sro.f \
	xerrwd.f \
	xsetf.f \
	xsetun.f

FFLAGS = -g
F77FLAGS = -g

libodepack.a : $(ODEPACK:.f=.o)
	ar -r libodepack.a $^
	ranlib libodepack.a

demo-lsode : demo1/opkdemo1.o demo1/f1.o demo1/f2.o demo1/jac1.o demo1/jac2.o demo1/edit2.o libodepack.a
	$(F77) -o $@ $^

demo-lsode.out : demo-lsode
	demo-lsode | tee demo-lsode.out

demo-lsoda : demo3/opkdemo3.o demo3/f1.o demo3/f2.o demo3/jac1.o demo3/jac2.o demo3/edit2.o libodepack.a
	$(F77) -o $@ $^

demo-lsoda.out : demo-lsoda
	demo-lsoda | tee $@

demo-lsodar : demo4/opkdemo4.o demo4/f1.o demo4/f2.o demo4/gr1.o demo4/gr2.o demo4/jac2.o libodepack.a
	$(F77) -o $@ $^

demo-lsodar.out : demo-lsodar
	demo-lsodar | tee $@

demo-lsodpk : demo5/opkdemo5.o demo5/fweb.o demo5/gset.o demo5/cinit.o demo5/outweb.o demo5/jacbg.o demo5/setpar.o demo5/solsbg.o demo5/gs.o demo5/fbg.o demo5/webr.o libodepack.a
	$(F77) -o $@ $^

demo-lsodpk.out : demo-lsodpk
	$(RM) -f demout ccout
	demo-lsodpk
	mv demout $@
	mv ccout ccout-lsodpk.out

demo-lsodkr : opkdemo6.o libodepack.a
	$(F77) -o $@ $^

demo-lsodkr.out : demo-lsodkr
	demo-lsodkr | tee $@

demo-lsodi : demo7/opkdemo7.o demo7/res.o demo7/elkup.o demo7/jacbd.o demo7/jacfl.o demo7/addabd.o demo7/addafl.o demo7/gfun.o libodepack.a
	$(F77) -o $@ $^

demo-lsodi.out : demo-lsodi
	demo-lsodi | tee $@

demo-lsoibt : demo8/opkdemo8.o demo8/res.o demo8/edit.o demo8/maxerr.o demo8/jacbt.o demo8/setic.o demo8/addabt.o demo8/gfun.o demo8/subav.o libodepack.a
	$(F77) -o $@ $^

demo-lsoibt.out : demo-lsoibt
	demo-lsoibt | tee $@

demo-lsodis : opkdemo9.o libodepack.a
	$(F77) -o $@ $^

demo-lsodis.out : demo-lsodis
	demo-lsodis | tee $@

tests : demo-lsode.out demo-lsoda.out demo-lsodar.out demo-lsodpk.out demo-lsodkr.out demo-lsodi.out demo-lsoibt.out demo-lsodis.out

clean :
	$(RM) *.o
	$(RM) demo-lsode demo-lsoda demo-lsodar demo-lsodpk demo-lsodkr demo-lsodi demo-lsoibt demo-lsodis
