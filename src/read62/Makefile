FC=gfortran
FCFLAGS=-c -O2 -static -std=legacy -mtune=native -fbacktrace -fcheck=bounds #-mtune=native#-fcheck=bounds
LDFLAGS=
OBJECTS=read62.o
all: $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o ../../exe/READ62.EXE

%.o: %.for
	$(FC) $(FCFLAGS) $< -o $@

clean:
	rm -rf *.o *.mod *.exe *.EXE
