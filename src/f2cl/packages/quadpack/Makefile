QUADPACK_SRC = \
	Fortran/d1mach.f \
	Fortran/dgtsl.f \
	Fortran/dqag.f \
	Fortran/dqage.f \
	Fortran/dqagi.f \
	Fortran/dqagie.f \
	Fortran/dqagpe.f \
	Fortran/dqags.f \
	Fortran/dqagse.f \
	Fortran/dqawc.f \
	Fortran/dqawce.f \
	Fortran/dqawf.f \
	Fortran/dqawfe.f \
	Fortran/dqawo.f \
	Fortran/dqawoe.f \
	Fortran/dqaws.f \
	Fortran/dqawse.f \
	Fortran/dqc25c.f \
	Fortran/dqc25f.f \
	Fortran/dqc25s.f \
	Fortran/dqcheb.f \
	Fortran/dqelg.f \
	Fortran/dqk15.f \
	Fortran/dqk15i.f \
	Fortran/dqk15w.f \
	Fortran/dqk21.f \
	Fortran/dqk31.f \
	Fortran/dqk41.f \
	Fortran/dqk51.f \
	Fortran/dqk61.f \
	Fortran/dqmomo.f \
	Fortran/dqng.f \
	Fortran/dqpsrt.f \
	Fortran/dqwgtc.f \
	Fortran/dqwgtf.f \
	Fortran/dqwgts.f \
	Fortran/xerror.f \
	Fortran/i1mach.f

libquadpack.a : $(QUADPACK_SRC:.f=.o)
	ar r $@ $^

PROGS = test1 test2 test3 test4 test5 test6 test7 test9 test12 test14 test15

test1 : test1.f libquadpack.a
	$(F77) -o $@ $^

test2 : test2.f libquadpack.a
	$(F77) -o $@ $^

test3 : test3.f libquadpack.a
	$(F77) -o $@ $^

test4 : test4.f libquadpack.a
	$(F77) -o $@ $^

test5 : test5.f libquadpack.a
	$(F77) -o $@ $^

test6 : test6.f libquadpack.a
	$(F77) -o $@ $^

test7 : test7.f libquadpack.a
	$(F77) -o $@ $^

test9 : test9.f libquadpack.a
	$(F77) -o $@ $^

test12 : test12.f libquadpack.a
	$(F77) -o $@ $^

test14 : test14.f libquadpack.a
	$(F77) -o $@ $^

test15 : test15.f libquadpack.a
	$(F77) -o $@ $^


tests : $(PROGS)
	for f in $^; do $$f | tee $$f.out; done