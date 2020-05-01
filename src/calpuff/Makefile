FC=gfortran-9
FCFLAGS=-c -O2 -static -std=legacy -mtune=native -fbacktrace -fcheck=bounds #-mtune=native#-fcheck=bounds
LDFLAGS=
OBJECTS=calpuff.o

all: $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o ../../exe/CALPUFF.EXE

%.o: %.for
	$(FC) $(FCFLAGS) $< -o $@

clean:
	rm -rf *.o *.mod *.exe *.EXE

LDFLAGS=

#BATCH ORIGINAL
#gfortran -static -o calmet.exe calmet.for 2>compile-calmet-gfortran.log



