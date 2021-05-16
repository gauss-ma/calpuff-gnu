FC=gfortran
FCFLAGS=-c -O2 -static -std=legacy -mtune=native -fbacktrace -fcheck=bounds
LDFLAGS=
OBJECTS=calpost.o

all: $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o ../../exe/CALPOSTv6.221.EXE

%.o: %.for
	$(FC) $(FCFLAGS) $< -o $@

clean:
	rm -rf *.o *.mod *.exe *.EXE

