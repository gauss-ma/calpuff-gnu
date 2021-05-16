#!/bin/bash

#Build CALWRF v2.0.3 (L190426)

#Dependencias:
#	- gfortran
#	- netcdf-4.4.0.tar.gz
#	- netcdf-fortran-4.4.3.tar.gz

DIR=/home/rama/git/hub/calpuff
LIBS_DIR=$DIR/lib

# -----------------------------------------------------------------------------
#Si el compilador fue instalado local
#export PATH=/path/to/compiler/bin:$PATH
# -----------------------------------------------------------------------------
# NetCDF-C (netcdf-4.4.0)
#tar -xzvf netcdf-4.4.0.tar.gz
#cd netcdf-4.0.0
./configure --prefix=$LIBS_DIR/netcdf --enable-fortran --enable-static --enable-shared 
make; make install

export NCDIR=$LIBS_DIR/netcdf
export PATH=$PATH:$NCDIR/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$NCDIR/lib
export CPPFLAGS=-I$NCDIR/include
export CFLAGS=-I$NCDIR/include
export FCFLAGS=-I$NCDIR/include
export LDFLAGS="-I$NCDIR/include -L$NCDIR/lib"
## -----------------------------------------------------------------------------
## NetCDF-Fortran (netcdf-fortran-4.4.3)
##tar -xzvf netcdf-fortran-4.4.3.tar.gz
##cd netcdf-fortran-4.4.3
./configure --prefix=$LIBS_DIR/netcdf --enable-static --enable-shared
make
make install
## -----------------------------------------------------------------------------
## CALWRF
export NETLIB=$LIBS_DIR/netcdf/lib
export NETINC=$LIBS_DIR/netcdf/inc
export NETINC=$LIBS_DIR/netcdf/include

cd CALWRF_v2.0.3_L190426
gfortran -I$NETINC calwrf.f -L$NETLIB -lnetcdf -lnetcdff -o $DIR/exe/CALWRFv2.0.3.EXE



# CALMET:
cd CALMET_v5.8.5_L151214/  
#rename 'y/A-Z/a-z/' *
make

# CALPOST:
cd CALPOST_v6.221_L080724/ 
#rename 'y/A-Z/a-z/' *
make

# CALPUFF:
cd CALPUFF_v5.8.5_L151214/
#rename 'y/A-Z/a-z/' *
make

