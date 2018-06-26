.DEFAULT_GOAL := test

FC=gfortran
FFLAGS=-g -fcheck=all

sac_io.mod sac_io.o: sac_io.f90
	$(FC) -c $(FFLAGS) $<

test: test.f90 sac_io.o
	$(FC) -o $@ $^

.PHONY: clean
clean:
	rm -rf sac_io.o sac_io.mod test
