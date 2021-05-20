#!/bin/bash
#Armar binarios y librerias para CALPUFF
#-------------------------------------------------------------------------------
# Si bien está estructurado como un bashscript la idea es que sea una guía para compilarlo, y no un programa para correr directamente.
#-------------------------------------------------------------------------------
#Preprocesadores:
#      - CALMET
#      - CALWRF
#      - read62
#Postprocesador:
#	- CALPOST
#Dependencias:
#	- gfortran (u otro compilador fortran)
#	- netcdf-4.4.0.tar.gz
#	- netcdf-fortran-4.4.3.tar.gz

#Defino directorio base, que contiene las carpetas "src", "exe" y "lib" (carpetas con codigo fuente, carpeta para guardar binarios y carpeta para guardar librerias, respectivamente).
DIR=/home/rama/git/hub/calpuff
# -----------------------------------------------------------------------------
#Si el compilador fue instalado local
#export PATH=/path/to/compiler/bin:$PATH
# -----------------------------------------------------------------------------
# NetCDF-C (netcdf-4.4.0)
cd $DIR/src
tar -xzvf netcdf-4.4.0.tar.gz
cd netcdf-4.0.0
./configure --prefix=$DIR/lib/netcdf --enable-fortran --enable-static --enable-shared 
make; make install

export PATH=$PATH:$DIR/lib/netcdf/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$DIR/lib/netcdf/lib
## -----------------------------------------------------------------------------
## NetCDF-Fortran (netcdf-fortran-4.4.3)
cd $DIR/src
tar -xzvf netcdf-fortran-4.4.3.tar.gz
cd netcdf-fortran-4.4.3
./configure --prefix=$DIR/lib/netcdf --enable-static --enable-shared
make
make install
## -----------------------------------------------------------------------------
# CALWRF:
cd $DIR/src
cd CALWRF_v2.0.3_L190426
#(!) Tuve que comentar las lineas: 2000-20010 (aprox) por que no me dejan leer el wrfout si en global attributes del netcdf no dice exactamente "OUTPUT FROM WRF VX"
#      !if(INDEX(value_chr,'OUTPUT FROM WRF V2') == 0)then
#También cambie:
#<       vattnames(3)='DYN_OPT'
#>       vattnames(3)='FEEDBACK'
#Esto último sólo por sugeerenicia de un post en internet, pero no sé si es necesario realmente.

gfortran -I$DIR/lib/netcdf/include calwrf.f -L$DIR/lib/netcdf/lib -lnetcdf -lnetcdff -o $DIR/exe/CALWRFv2.0.3.EXE
## -----------------------------------------------------------------------------
# CALMET:
cd $DIR/src
cd CALMET_v5.8.5_L151214/  
rename 'y/A-Z/a-z/' *	#El compilador no encuentra algunos modulos si están en uppercase, asi que transformo todo a lower.
make

# CALPOST:
cd $DIR/src
cd CALPOST_v6.221_L080724/ 
rename 'y/A-Z/a-z/' *
make

# CALPUFF:
cd $DIR/src
cd CALPUFF_v5.8.5_L151214/
rename 'y/A-Z/a-z/' *
make

