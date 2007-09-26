# This is for GNU make.

DONLP2_SRC = \
	donlp2.f \
	dummy.f \
	esf.f \
	esg.f \
	esgradf.f \
	esgradg.f \
	esgradh.f \
	esh.f \
	eval_extern.f \
	o8adcd.f \
	o8bfgs.f \
	o8cutd.f \
	o8dec.f \
	o8dird.f \
	o8dlcd.f \
	o8dsq1.f \
	o8egph.f \
	o8eval.f \
	o8fin.f \
	o8ht.f \
	o8info.f \
	o8inim.f \
	o8left.f \
	o8mdru.f \
	o8msg.f \
	o8opti.f \
	o8qpdu.f \
	o8rest.f \
	o8rght.f \
	o8rinv.f \
	o8rup.f \
	o8save.f \
	o8sc1.f \
	o8sc2.f \
	o8sc3.f \
	o8sce.f \
	o8shms.f \
	o8smax.f \
	o8sol.f \
	o8solt.f \
	o8st.f \
	o8unim.f \
	o8upd.f \
	o8vecn.f \
	o8zup.f \
	solchk.f \
	user_eval.f

antenna1 : donlp2main.o antenna1.o libdonlp2.a
	$(F77) -o $@ $^

hs101 : donlp2main.o hs101.o libdonlp2.a
	$(F77) -o $@ $^

hs102 : donlp2main.o hs102.o libdonlp2.a
	$(F77) -o $@ $^

hs103 : donlp2main.o hs103.o libdonlp2.a
	$(F77) -o $@ $^

betting : donlp2main.o betting.o libdonlp2.a
	$(F77) -o $@ $^

chemequi : donlp2main.o chemequi.o libdonlp2.a
	$(F77) -o $@ $^

alkylati : donlp2main.o alkylati.o libdonlp2.a
	$(F77) -o $@ $^

amplqp :  donlp2main.o amplqp.o libdonlp2.a
	$(F77) -o $@ $^

libdonlp2.a : $(DONLP2_SRC:.f=.o)
	ar r $@ $^

