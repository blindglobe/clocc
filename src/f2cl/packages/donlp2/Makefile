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

fractf77new : donlp2main.o fractf77new.o libdonlp2.a
	$(F77) -o $@ $^

girder : donlp2main.o girder.o libdonlp2.a
	$(F77) -o $@ $^

irreg1 : donlp2main.o irreg1.o libdonlp2.a
	$(F77) -o $@ $^

irreg2 : donlp2main.o irreg2.o libdonlp2.a
	$(F77) -o $@ $^

net1 : donlp2main.o net1.o libdonlp2.a
	$(F77) -o $@ $^

boxparam : donlp2main.o boxparam.o libdonlp2.a
	$(F77) -o $@ $^

barnes: donlp2main.o barnes.o libdonlp2.a
	$(F77) -o $@ $^

dembo1as: donlp2main.o dembo1as.o libdonlp2.a
	$(F77) -o $@ $^

dembo4: donlp2main.o dembo4.o libdonlp2.a
	$(F77) -o $@ $^

dembo7: donlp2main.o dembo7.o libdonlp2.a
	$(F77) -o $@ $^

hs109: donlp2main.o hs109.o libdonlp2.a
	$(F77) -o $@ $^

maratos: donlp2main.o maratos.o libdonlp2.a
	$(F77) -o $@ $^

paviani2: donlp2main.o paviani2.o libdonlp2.a
	$(F77) -o $@ $^

tp383mod: donlp2main.o tp383mod.o libdonlp2.a
	$(F77) -o $@ $^

undefexa: donlp2main.o undefexa.o libdonlp2.a
	$(F77) -o $@ $^

weaponas: donlp2main.o weaponas.o libdonlp2.a
	$(F77) -o $@ $^

robota: donlp2main.o robota.o libdonlp2.a
	$(F77) -o $@ $^

libdonlp2.a : $(DONLP2_SRC:.f=.o)
	ar r $@ $^


PROGS = \
	antenna1  \
	hs101  \
	hs102  \
	hs103  \
	betting  \
	chemequi  \
	alkylati  \
	amplqp  \
	fractf77new  \
	girder  \
	irreg1  \
	irreg2  \
	net1  \
	boxparam  \
	barnes \
	dembo1as \
	dembo4 \
	dembo7 \
	hs109 \
	maratos \
	paviani2 \
	tp383mod \
	undefexa \
	weaponas \
	robota

tests : $(PROGS)
	for f in $^; do $$f; done

clean :
	$(RM) *.o
	$(RM) $(PROGS)
