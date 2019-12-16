FC=gfortran
FCFLAGS=-c -O2 -static #-mtune=native#-fcheck=bounds
LDFLAGS=

#OBJECTS=calmet.o calutils.o coordlibs.o
#OBJECTS=calutils.o coordlibs.o calmet.o
OBJECTS=calmet.o
all: $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o ../../exe/CALMET.EXE

%.o: %.for
	$(FC) $(FCFLAGS) $< -o $@

clean:
	rm -rf *.o *.mod *.EXE


:
#BATCH ORIGINAL
#gfortran -static -o calmet.exe calmet.for 2>compile-calmet-gfortran.log
