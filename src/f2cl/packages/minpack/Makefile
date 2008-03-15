MINPACK = chkder.f \
	dogleg.f \
	dpmpar.f \
	enorm.f \
	fdjac1.f \
	fdjac2.f \
	hybrd.f \
	hybrd1.f \
	hybrj.f \
	hybrj1.f \
	lmder.f \
	lmder1.f \
	lmdif.f \
	lmdif1.f \
	lmpar.f \
	lmstr.f \
	lmstr1.f \
	qform.f \
	qrfac.f \
	qrsolv.f \
	r1mpyq.f \
	r1updt.f \
	rwupdt.f

tst-hybrd : tst-hybrd.o $(MINPACK:.f=.o)
	$(F77) -o $@ $^
tst-lmder : tst-lmder.o $(MINPACK:.f=.o)
	$(F77) -o $@ $^
tst-lmdif : tst-lmdif.o  $(MINPACK:.f=.o)
	$(F77) -o $@ $^

lmdif-ref.txt : tst-lmdif
	tst-lmdif < lmdif-input.dat | tee lmdif-ref.txt

lmder-ref.txt : tst-lmder
	tst-lmder < lmdif-input.dat | tee lmder-ref.txt

hybrd-ref.txt : tst-hybrd
	tst-hybrd < hybrd-input.dat | tee hybrd-ref.txt

tests : lmdif-ref.txt lmder-ref.txt hybrd-ref.txt

clean :
	$(RM) *.o tst-hybrd tst-lmder tst-lmdif 
	$(RM) lmdif-ref.txt lmder-ref.txt hybrd-ref.txt
