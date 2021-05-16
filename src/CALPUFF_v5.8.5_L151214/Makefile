FC=gfortran
FCFLAGS=-c -O2 -static -std=legacy -mtune=native -fbacktrace -fcheck=bounds #-mtune=native#-fcheck=bounds
LDFLAGS=
OBJECTS=calpuff.o

all: $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o ../../exe/CALPUFFv5.8.5.EXE

%.o: %.for
	$(FC) $(FCFLAGS) $< -o $@

clean:
	rm -rf *.o *.mod *.exe *.EXE
