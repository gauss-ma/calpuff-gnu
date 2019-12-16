C v5df.h

C Include file for using v5d functions from FORTRAN programs


C Function prototypes.  See the README file for details.  These are
C the functions you'll want to use for writing v5d file converters.

      integer v5dcreate

      integer v5dcreatesimple

      integer v5dwrite

      integer v5dmcfile

      integer v5dclose

C 5-D grid limits, must match those in v5d.h!!!
C Also make sure - maxcolumns >= mxnx in calmm5.par
C                  maxrows    >= mxny in calmm5.par
C                  maxlevels  >= mxnz in calmm5.par

      integer MAXVARS, MAXTIMES, MAXROWS, MAXCOLUMNS, MAXLEVELS

      parameter (MAXVARS=100)
C      parameter (MAXVARS=30)
      parameter (MAXTIMES=8760)
      parameter (MAXROWS=150)
      parameter (MAXCOLUMNS=150)
c      parameter (MAXLEVELS=30)
      parameter (MAXLEVELS=50)
 
C Missing values
      real MISSING
      integer IMISSING

      parameter (MISSING=1.0E35)
      parameter (IMISSING=-987654)



