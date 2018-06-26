sac_io.mod sac_io.o: sac_io.f90
	$(FC) -c $(FFLAGS) $<

test: test.f90 sac_io.o
	$(FC) -o $@ $^

