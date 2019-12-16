c----------------------------------------------------------------------
c --- PARAMETER statements -- MAKEGEO Preprocessor
c----------------------------------------------------------------------
c --- Specify model version
      character*12 mver, mlevel
      parameter(mver='2.29',mlevel='070327')
c
c --- Specify parameters
      parameter(mxnx=265,mxny=265,mxcat=52,mxocat=52)
      parameter(ncell=mxnx*mxny)
      parameter(iomesg=0)
      parameter(io2=2,io3=3,io4=4,io5=5,io6=16,io7=7,io8=8)
      parameter(io9=9,io12=12,io53=13,io52=15)
c
c --- Parameter definitions:
c       MVER    - version number of MAKEGEO
c       MLEVEL  - level (e.g. release date code) of MAKEGEO
c       MXNX    - maximum number of x grid cells in domain
c       MXNY    - maximum number of y grid cells in domain
c       MXCAT   - maximum number of input land use categories
c       MXOCAT  - maximum number of output land use categories
c
c --- FORTRAN I/O unit numbers:
c       IOMESG  - unit number for screen         - output
c       IO2     - Fractional land use data file  - input  - formatted
c                  (user-specified file name)
c       IO3     - Supplemental Fractional LU file- input  - formatted
c                  (user-specified file name)
c       IO4     - Preprocessed Terrain Data file - input  - formatted
c                  (user-specified file name)
c       IO5     - Control file (MAKEGEO.INP)     - input  - formatted
c       IO6     - Run list File (MAKEGEO.LST)    - output - formatted
c       IO7     - Output GEO.DAT file            - output - formatted
c       IO8     - Output plot file (QALUSE.GRD)  - output - formatted
c       IO9     - Output plot file (QATERR.GRD)  - output - formatted
c       IO12    - UAM Terrain File               - output - unformatted
c                  (user-specified file name)
c       IO53    - Surfer 'CLR' file for LU plots - output - formatted
c       IO52    - UAM output file (deprecated)
 