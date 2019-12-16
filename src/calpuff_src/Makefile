FC= gfortran
FCFLAGS=-c -O2 -static
LDFLAGS=
OBJECTS=calpuff.o

all: $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) -o ../../CALPUFF.EXE

%.o: %.for
	$(FC) $(FCFLAGS) $< -o $@

clean:
	rm -rf *.o *.mod *.EXE

##BATCH ORIGINAL:
#gfortran -static -o calpuff.exe calpuff.for 2>compile-calpuff-gfortran.log
