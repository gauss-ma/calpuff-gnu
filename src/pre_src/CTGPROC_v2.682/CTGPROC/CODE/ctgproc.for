c----------------------------------------------------------------------
c --- CTGPROC -- LULC (CTG) Preprocessor
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681        Level: 070327                 MAIN
c              E. Insley,   EARTH TECH, Inc.
c
c     Copyright (c) 1996,1997,1999,2001-2007
c              by Exponent, Inc.
c
c --- PURPOSE: Reads a compressed USGS Composite Theme Grid (CTG) land
c              use and land cover data file (1 quad/per run), or USGS
c              Global land cover characteristics data file (Data in 
c              Lambert Azimuthal Equal Area Projection), or New
c              Zealand Generic Format (comma-delimited), or National
c              Land Cover Dataset (Data in Albers Conical Equal Area
c              Projection) and determines the fractional land use for
c              each grid cell in the user-specified gridded domain.
c              The program may need to run iteratively to cover the entire
c              modeling domain.  The output created from a previous
c              run of CTGPROC can be used as input.
c
c --- Updates
c     Ver 2.67 Level 070430 from Ver 2.66 Level 060202       IWL
c              - For NLCD92 data, code modified to read in data 
c                filename regardless of whether file was unzipped using
c                WinZip (e.g."alabama_NLCD_flat_031600.bin"
c                or GZIP (e.g. "alabama.nlcd.bin")
c                Modified:  COMP
c
c --- Ver 2.681 Level 070320 from Ver 2.68 Level 061013
c              - CALUTILS from v2.52 Level 060519 to v2.55 Level 070327
c                Move GLOBE1 to COORDLIB
c                Allow negative increments in INCRS
c                Fixed format bug in subroutine BASRUTC for the case of
c                time zone zero (output string was 'UTC+0  0' instead
c                of 'UTC+0000'
c                Modified:  INCRS, UTCBASR, BASRUTC
c                Removed:   GLOBE1
c              - COORDLIB from v1.96 Level 051010 to v1.98 Level 060911
c                Changes in COORDS that allow a higher level of FORTRAN error
c                checking.  Compiler checks had identified constant arguments
c                and 2 uninitialized variables.  None of these is known to
c                have produced errors in cooerdinate conversions.
c                Add GLOBE1 (from CALUTILS)
c                Modified:  COORDS
c
c --- Ver. 2.68 Level 061013 from Ver. 2.67 Level 060519
c              - NLCD processing fails if output datum is same as NLCD
c                datum (NAR-C).  Add logic for NLCD (LULC type 4) in
c                CTRANS subroutine.
c                Modified:  CONTROL.CTG
c                           BLOCK DATA, CTRANS
c
c --- Ver. 2.67 Level 060519 from 2.66 Level 060202
c              - CALUTILS from v2.51 Level 051019 to v2.52 Level 060519
c                Variable names in control file are not processed
c                correctly if there are too many characters (including
c                blanks) to the left of the "=" sign (run stops in
c                setup phase).
c                Modified:  READIN
c
c --- Ver. 2.66 Level 060202 from Ver. 2.65 Level 051201
c                                                       (D. Strimaitis)
c              - Add LQACELL to control output of QA files of input
c                land use cell coordinates for QA plots. 
c                Modified:  PARAMS.CTG, CONTROL.CTG
c                           BLOCK DATA, READCF, COMP
c              - Change filename strings from c*70 to c*132
c                This is required by CALUTILS 2.3 and later which uses
c                c*132 in FILCASE and COMLINE
c                Modified:  FILNAM.CTG
c                           SETUP, READCF, COMP
c
c --- Ver. 2.65 Level 051201 from Ver. 2.64 Level 050428
c                                                       (K. Morrison)
c              - Add processing of NLCD 1992 data.  This required changes
c                to SETUP, READCF, COMP, CTRANS, SETTRANO, and BLOCK DATA,
c                changes to PARAMS.CTG, CONTROL.CTG, and LUTABS.CTG,
c                as well as changes to GLOBE1 in CALUTILS.FOR and to COORDS
c                in COORDLIB.FOR to support the Albers Conical Equal Area
c                projection.
c              - Correction in COASTEXT when there is only a single
c                surrounding polygon (i.e. coastline processing was
c                not needed)
c
c --- Ver. 2.64 Level 050428 from Ver. 2.63 Level 050128
c                                                       (K. Morrison)
c              - Correction in COASTEXT to catch GSHHS polygons that
c                straddle Greenwich because GLOBE changes negative input
c                longitudes to positive output longitudes, not desired
c                when the domain straddles or is East of Greenwich.
c
c     Ver 2.63 Level 050128 from Ver 2.62 Level 041215
c                                                         D. Strimaitis
c              - Add mesh density option for spreading LU in an input
c                grid cell to other points within the cell to improve
c                the sampling density.  This assumes that LU is at the
c                cell center, and it is characteristic of all points
c                within the cell (limit of LU resolution).  Control
c                variables MESHCTG and MESHGLAZ are added to the inputs.
c                The option is not implemented for the NZGEN data.
c                Modified:  CONTROL.CTG
c                           BLOCK DATA, SETUP, READCF, COMP, SVGRID
c              - When processing USGS Global LU, keep all points that
c                are within the output grid plus one Global LU grid cell
c                diagonal.  This buffer had been set to one output grid
c                cell side.
c                Modified:  COMP
c              - Add QA output files for locations of LU data points
c                Modified:  COMP
c              - Disable use of IDEFTYP to replace ocean LU at onshore
c                points (was not operational), and add marine LU control
c                variables LMARSWAP, LMARFILL, and IOCEAN for the
c                coastline processing option.
c                Modified:  CONTROL.CTG
c                           BLOCK DATA, SETUP, READCF, COMP, LUGRID
c              - COORDLIB updated to stop UTM conversions with a DATUM
c                that is not mapped to the list in the USGS UTM subr.
c                An example is the sphere datum NWS-84 (Earth radius
c                6370km), since only the sphere datum ESR-S (Earth
c                radius 6371km) is available.  Unmapped datums had
c                defaulted to the Clarke 1866 spheroid.
c                LAZA Projection:  removed assignment of 6370 km earth
c                radius (NWS-84 datum) when a value less than 6000 km is
c                found.  This assignment can override a requested radius
c                of 6371 (ESR-S datum) if the NWS-84 datum is used with
c                any valid projection prior to the request for ESR-S.
c                LAZA(NWS-84) coordinate distances from the projection
c                origin are about 0.016% smaller than LAZA(ESR-S).
c                Error message and version strings added to COORDS calls
c                and new subroutine COORDSVER to report COORDS version
c                documentation.
c                (Version 1.95, Level 050126)
c              - Added call to COORDSVER to access the COORDS version
c                info and passed string to list file and comment section
c                of output files.
c                Modified: READCF, WRTHEAD
c              - Correct spelling of NDBF in BLOCK DATA and remove
c                unused LPROC argument from GET4CNR
c                Modified: BLOCK DATA, GET4CNR
c              - Initialize LSURRSIDE as a logical array rather than as
c                a logical.
c                Modified: COASTEXT.
c
c     Ver 2.62 Level 041215 from Ver 2.61 Level 041013
c                                                         D. Strimaitis
c              - Remove screen on transformed (x,y) points before call
c                to LUGRID in subroutine COMP.  The screen, which was
c                added in version 2.6 with the coastline option, removed
c                points near the edges of the grid domain that should
c                have been included.  Also, move the cell index (i,j)
c                calculation in LUGRID to the top of the subroutine so
c                that points outside of the domain are rejected before
c                the IPTYPE(x,y) function is called (shoreline option).
c                Modified:  COMP
c              - Add a column to the right end of the LU.DAT output file
c                that flags cells for which land use remains missing.
c                Modified:  SVGRID
c              - Make format for LU.DAT records respond to MXCAT
c                in the range 1 to 999.
c                Modified:  RDGRID, SVGRID
c              - Fixed assignment of the reference latitude and
c                longitude in SETUP calls to GLOBE1 (added in v2.6).
c                Variables are REFLAT, REFLON.  Also removed /GRID/
c                from COASTEXT and passed needed variables as arguments.
c                These changes are needed for the coastline processing
c                option when the output grid projection requires a 
c                reference latitude/longitude.
c                Modified:  SETUP, COASTEXT
c              - Add array bound checks against MXCOAST, MXCOASTP
c                Modified:  COASTEXT
c              - Restrict global landuse processing to the first CTGPROC
c                application because of the logic required to overcome
c                spurious water results when land is on "another
c                continent".  Global files cannot be added to a
c                continuation run, and files in a run are re-ordered to
c                process global files before any others.
c                Modified:  READCF, COMP
c              - Recast LU counts reported to list file to characterize
c                the number of data points read and the number used.
c                Modified:  COMP
c
c     Ver 2.61 Level 041013 from Ver 2.6 Level 040921
c                                                         K. Morrison
c              - Modify COASTEXT and BREADH to allow both old and new
c                versions of GSHHS
c              - Modify COASTEXT to eventually be able to process WDBII
c                with empirical fixup (hardwired out for now)
c              - Correct error introduced for global data set in version
c                2.6 (wrong variable in call to LUGRID)
c                                                         D. Strimaitis
c              - COORDLIB updated to fix UTM conversion in the S.
c                hemisphere when the ouput UTM zone is forced as N. 
c                hemisphere, and the DATUM changes (completes changes
c                started in version 1.93).
c                (Version: 1.94, Level: 041007)
c              - Remove screen on global data files when only 1 file is
c                processed.
c
c     Ver 2.6 Level 040921 from Ver 2.51 Level 040907 K. Morrison
c              - Correct coastline processing (COMP)
c              - Add default land cover variable IDEFTYP for points
c                incorrectly identified as ocean (READCF, LUGRID, GRID.CTG)
c              - Check empty cells to see if they are in the ocean (SVGRID)
c              - Add coastline processing and associated files and logicals
c              - Consolidate output translations into SETTRANO
c              - Harmonize numerous variables in commons to be consistent
c                with TERREL
c
c     Ver 2.51, Level 040907 from Ver 2.5, Level 040512  D. Strimaitis          
c              - COORDLIB updated to respond to UTM conversion across
c                the equator from S. hemisphere to N. hemisphere, when
c                the S. hemisphere zone is forced.  Also fixed a problem
c                with the conversion to/from spherical NWS-84 datum when
c                using UTM projection (USGS program input array
c                conflicts).
c                (Version: 1.93, Level: 040713)
c
c     Ver 2.5, Level 040512 from Ver 2.45, Level 040109  D. Strimaitis          
c              - GET4CNR modified to limit computed beginning/ending
c                GLOBAL DB ind. to actual range.  Without this mod the
c                random access counter can point outside the range,
c                generating a runtime error.
c              - GET4CNR modified to add a buffer to the computed
c                range that is 10% of the modeling domain scale rather
c                than 200km.
c              - Add logic to identify case where no data in global DB
c                file covers the current grid (skip this file).
c              - Add loop structure to process multiple input land use
c                files.
c              - Count of global records used for LU updates is made 
c                before splitting Global cell into 16ths
c              - Remove LGLOBAL from code
c              - Create temporary grid for USGS Global data as it is 
c                processed and add combinatorial logic to exclude
c                grid cells that are coded as water in place of land
c                when that land is on "another continent".
c
c     Ver 2.45, Level 040109 from Ver 2.44, Level 031017  D. Strimaitis          
c              - COORDLIB updated to respond to projection parameter
c                changes when both the projection type and datum do not
c                change
c                (Version: 1.92, Level: 031201)
c 
c     Ver 2.44, Level 031017 from Ver 2.43, Level 030905  D. Strimaitis          
c              - WGS-72 DATUM bug for UTM calls fixed in COORDLIB
c                (Version: 1.91, Level: 031017) 
c
c     Ver 2.43, Level 030905 from Ver 2.42, Level 030709  D. Strimaitis          
c              - DATUMs updated COORDLIB (Version: 1.9, Level: 030905)
c              - Default DATUMs reset
c
c     Ver 2.42, Level 030709 from Ver 2.41, Level 030528  D. Strimaitis          
c              - Fix variable type assignment of LCFILES in READCF
c
c     Ver 2.41, Level 030528 from Ver 2.4, Level 030402   D. Strimaitis          
c              - Add read for false easting/northing for LCC,LAZA,TTM
c                in continuation file
c              - COORDLIB (Version: 1.15, Level: 030528) 
c              - CALUTILS (Version: 2.2, Level: 030528) 
c
c     Ver 2.4, Level 030402 from Ver 2.3, Level 021028   D. Strimaitis          
c              - Incorporate revised COORDS implementation in
c                COORDLIB (Version: 1.14, Level: 030402) 
c              - Updated CALUTILS (Version: 2.1, Level: 030402)
c              - New header for output data file (LU.DAT)
c              - Add false Easting and Northing (GLOBE1)
c              - Add TYPE argument to XTRACTLL
c              - Replace IOMESG with iolst in 2nd JULDAY call in FIN
c
c     Ver 2.3, Level 021028 from Ver 2.1, Level 020729   K. Morrison          
c              - Add New Zealand Generic data treatment by:
c                passing the variable lulc through common
c                allowing lulc to have 3 values instead of 2
c                changing certain tests from lglobal to lulc
c                adding a section in subroutine comp to handle the New
c                   Zealand data
c                adding a translation table for New Zealand codes to
c                   USGS codes.
c
c     Ver 2.2 Level 020828 from Ver 2.1, Level 020729   D. Strimaitis
c              - Updated CALUTILS (Version 1.1, Level 020828)
c
c     Ver 2.1, Level 020729 from Ver 2.0, Level 020226   D. Strimaitis
c              - Place OPEN attributes for binary files in PARAMETERs
c                to allow code to be configured for various compilers
c
c     Ver 2.0, Level 020226 from Ver 2.0, Level 011003   D. Strimaitis          
c              - Split Global data cell into 16 to get ~250m coverage
c              - Revise Global cell location algorithm to fix bug
c
c     Ver 2.0, Level 011003 from Ver 1.2, Level 010206   D. Strimaitis          
c              - Restructure inputs for CALPUFF system control file
c              - Restructure main program as subroutine COMP
c              - Place system-wide utilities into an include module
c                (calutil.for)
c
c     Ver 1.2, Level 010206 from Ver 1.1, Level 000218   D. Strimaitis          
c              (1) Add land use index 55, thereby increasing the
c                  number of land use types from 37 to 38
c              (2) Add IMAPFAC (map projection) to control file inputs
c                      1:UTM    2:LCC
c              (3) Add header records that identify the data file
c                  properties -- these changes require the use of
c                  the corresponding MAKEGEO!
c
c     Ver 1.1, Level 000218 from Ver 1.1, Level 000112   S. Du          
c              (1) Modify MAIN to correct index for excluding data
c                  from analysis (corrects a left/bottom boundary
c                  problem).
c
c     Ver 1.1, Level 000112 from Ver 1.0, Level 990729   S. Du, J. Scire
c              (1) Add the capability to process USGS Global land 
c                  cover data (in Lambert Azimuthal projection) file. 
c                  Added a subroutine called GET4CNR which determines 
c                  the position of the output domain in the whole input 
c                  data domain.
c              (2) Modify Subr. LUGRID when using LCC projection with
c                  Global data (UTM is not is not needed (and not 
c                  defined) when using LCC with Global data)
c              (3) Modify list file outputs and misc. code comments
c     Ver 1.0, Level 990729 from 990130      B. de Foy
c              (1) Subroutines MAPL2G, MAPG2L, LL2UTM, UTM2LL taken 
c                  from COORDS, Level 990729 to deal with Southern 
c                  Hemisphere + crossing meridian 180
c     Ver 1.0, Level 990130 from 961113/970729      DGS, J. Chang
c                  OUT - changed to be similar to PRTMET version
c                        allowing subgrid output, and up to 999 cells
c                  Add capability to use Lambert Conformal projection
c
c----------------------------------------------------------------------
      Program CTGPROC
c
c --- Include parameters
      include 'params.ctg'
c --- Include common blocks
      include 'qa.ctg'
c
c --- Set version and level number of program (stored in /QA/ and
c --- checked against values set in PARAMS.CTG)
      ver='2.682'
      level='070430'
c
c --- SETUP PHASE -- read control file information
      call SETUP
c
c --- COMPUTATIONAL PHASE -- process data files
      call COMP
c
c --- TERMINATION PHASE -- program termination functions
      call FIN
c
      stop
      end
c----------------------------------------------------------------------
      BLOCK DATA
c----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682          Level: 061013        BLOCK DATA
c               D. Strimaitis
c
c --- Updates
c     Ver 2.68, Level 061013 from Ver 2.66, Level 060202  DGS
c               Add LACEA logical in /CONTROL/
c     Ver 2.66, Level 060202 from Ver 2.65, Level 051201  DGS
c               Add LQACELL logical
c     Ver 2.65, Level 051201 from Ver 2.64, Level 050128  KAM
c               Add mapping tabled for NLCD92 and NLCD01
c     Ver 2.63, Level 050128 from Ver 2.6, Level 040921  DGS
c               Add MESHCTG, MESHGLAZ, LMARSWAP, LMARFILL, IOCEAN
c               Correct spelling of NDBF
c     Ver 2.6, Level 040921 from Ver 2.5, Level 040512   KAM
c               Add variables for coastline processing
c     Ver 2.5, Level 040512 from Ver 2.4, Level 030402   DGS
c               Allow multiple land use data files (input)
c     Ver 2.4, Level 030402 from Ver 2.0, Level 011003   DGS
c               New COORDS/DATUM variables
c     Ver 2.0, Level 011003 from Ver 1.2, Level 010206   DGS
c               Set defaults for new control file inputs
c     Ver 1.2, Level 010206 from Ver 1.1, Level 960901   DGS
c               Add land use index 55, thereby increasing the
c               number of land use types from 37 to 38
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.ctg'
c
c --- Include common blocks
      include 'filnam.ctg'
      include 'control.ctg'
      include 'grid.ctg'
      include 'lutabs.ctg'

c --- FILNAM common block
      data runinp/'ctgproc.inp'/,runlst/'ctgproc.lst'/,
     1     ludat/'lu.dat'/,prevdat/'prev.dat'/,
     2     gshhsin/'gshhs_f.b'/, coastbln/'coast.bln'/
      data luindat(1)/'luin.dat'/,justname(1)/'luin.dat'/
c --- FILLOG common block
      data lcfiles/.true./

c --- CONTROL common block
c --- Primary variables
      data lprev/.false./, lfinal/.true./
      data lcoast/.false./, lblnread/.false./
      data lmarswap/.false./, lmarfill/.true./
      data iocean/55/
      data ithres/75/,iglazr/mxfil*0/
      data pmap/'UTM     '/
      data dctg/'NAS-C   '/, dusgsla/'ESR-S   '/, dnzgen/'WGS-84  '/
      data dnlcd/'NAR-C   '/, dwvs/'WGS-84  '/, dwdbii/'WGS-72  '/
      data lulc/mxfil*1/
      data ndbf/0/
      data meshctg/1/, meshglaz/1/
      data lqacell/.false./
c --- Derived variables
      data lutm/.false./, llcc/.false./, lps/.false./
      data lem/.false./, llaza/.false./, lttm/.false./
      data lacea/.false./

c --- GRID common block
      data lzone/.false./, ldatum/.false./
      data utmhem/'N   '/, datum/'WGS-84  '/
      data reflat/-999./,reflon/-999./,xlat1/-999./,xlat2/-999./
      data feast/0.0/, fnorth/0.0/

c --- LUTABS common block
      data lumap/10*0,
     1           1,2,3,4,5,6,7,3*0,
     2           8,9,10,11,6*0,
     3           12,13,14,7*0,
     4           15,16,17,7*0,
     5           18,19,20,21,22,5*0,
     6           23,24,8*0,
     7           25,26,27,28,29,30,31,3*0,
     8           32,33,34,35,36,5*0,
     9           37,38/
      data itab /16,21,21,21,21,21,31,32,33,33,
     1           41,41,42,42,43,52,62,61,
     2           77,82,81,85,83,91/
      data nzcat /55,8*0,16,
     1           14,8*0,21,
     2           22,8*0,43,
     3           43,8*0,32,
     4           2*0,61,6*0,52,
     5           9*0,62,
     6           62,8*0,75,
     7           72,74/
      data nlcd92/10*0,
     1            52,91,8*0,
     2            11,11,13,7*0,
     3            74,75,76,7*0,
     4            41,42,43,7*0,
     5            32,9*0,
     6            22,9*0,
     7            31,9*0,
     8            5*21,5*0,
     9            61,62,7*0/
      data nlcd01/10*0,
     1            52,91,8*0,
     2            30,11,11,13,6*0,
     3            74,72,8*0,
     4            41,42,43,7*0,
     5            32,32,8*0,
     6            10*0,
     7            31,82,82,82,6*0,
     8            2*21,7*0,
     9            5*61,5*62/
      data ideftyp /4/

      end
c----------------------------------------------------------------------
c --- BRING IN CALPUFF SYSTEM UTILITY SUBROUTINES
      include 'calutils.for'
      include 'coordlib.for'
c----------------------------------------------------------------------
c-----------------------------------------------------------------------
      subroutine setup
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682           Level: 060202             SETUP
c               D. Strimaitis, Earth Tech, Inc.
c
c PURPOSE:     SETUP reads and checks the control data provided, sets
c              logicals, echoes the control data, and opens the data
c              files if inputs are valid.
c
c --- Updates
c     Ver 2.66 Level 060202 from Ver 2.65 Level 051201       DGS
c              Change filename strings from c*70 to c*132
c              This is required by CALUTILS 2.3 and later which uses
c              c*132 in FILCASE and COMLINE
c     Ver 2.65 Level 051201 from Ver 2.64 Level 050128       KAM
c              Add NLCD data types - hide 2001 for now
c     Ver 2.63 Level 050128 from Ver 2.62 Level 041215       DGS
c              Add MESHCTG, MESHGLAZ, LMARSWAP, LMARFILL, IOCEAN
c              Remove IDEFTYP
c     Ver 2.62 Level 041215 from Ver 2.6 Level 040921        DGS
c              RLAT/RLON changed to REFLAT/REFLON in GLOBE1 calls
c              added with coastline processing option, and /GRID/
c              data passed as arguments to COASTEXT
c     Ver 2.6 Level 040921 from Ver 2.51  Level 040512       KAM
c              Add coastline processing
c     Ver 2.5 Level 040512 from Ver 2.41 Level 030429        DGS
c              Allow multiple input land use data files
c     Ver 2.41 Level 030429 from Ver 2.4  Level 030402       DGS
c              Report false Easting/Northing
c     Ver 2.4  Level 030402 from Ver 2.1  Level 020729       DGS
c              New COORDS/DATUM
c     Ver 2.1  Level 020729 from Ver 2.0  Level 011003       DGS
c              Use file attributes from PARAMS file for Global Data
c
c ARGUMENTS:
c    PASSED:  none
c
c  RETURNED:  /CONTROL/   logicals and flags
c             /GRID/      data
c
c CALLING ROUTINES:   MAIN
c
c EXTERNAL ROUTINES:  DATETM, COMLINE, READCF
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.ctg'
      include 'control.ctg'
      include 'filnam.ctg'
      include 'lutabs.ctg'
      include 'grid.ctg'
      include 'gspan.ctg'
      include 'qa.ctg'

c --- Local Variables
      character*32 cglobal(6)
      character*12 caction
      character*4 c4dum
      real*8 vecti(9),vecto(9)

c --- Set text for reporting input options
      data cglobal/'North America)','South America)',
     &             'Eurasia, Optimized for Europe)',
     &             'Eurasia, Optimized for Asia)',
     &             'Africa)','Australia Pacific)'/

c --- Get date and time from system
      call DATETM(rdate,rtime,rcpu)

c --- Get the name of the control file from the command line
      call COMLINE(runinp)

c --- Open the control file
      open(iocnt,file=runinp,status='old')

c --- Report progress
      write(iomesg,*)'SETUP PHASE'

c --- Check that the version and level number in the parameter
c --- file matches those in the code itself
      if(ver.ne.mver.or.level.ne.mlevel)then
         write(iomesg,10) ver,level,mver,mlevel
10       format(/1x,'ERROR in SUBR. SETUP -- The CTGPROC version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open list file)
      call READCF

c --- Open output data file
      open(ioout,file=ludat)

c --- Open file from previous run, if any
      if(LPREV) then
         open(ioprev,file=prevdat,status='old')
      endif

c --- Write header lines to list-file

      write(iolst,*)
      write(iolst,*) '--------------------------'
      write(iolst,*) '    SETUP Information'
      write(iolst,*) '--------------------------'
      write(iolst,*)

c -----------------------------
c --- Report control data
c -----------------------------

      write(iolst,*)
      write(iolst,*) 'Control File Used -----'
      write(iolst,*) runinp

      write(iolst,*)
      write(iolst,*) 'Processing Options -----'
      write(iolst,*) 'Continues Previous Run? :  ',lprev
      write(iolst,*) 'Final Run in Series?    :  ',lfinal
      write(iolst,*)
      write(iolst,*) 'Coastline processing?   =  ',lcoast
      if(lcoast) then
c        write(iolst,*) 'Default LU for points misidentified'
c        write(iolst,*) ' as ocean (*10)          =  ',ideftyp
        if(LMARFILL) write(iolst,*) 'Fill empty marine grid cells ',
     &                    'at end of run with land use type ',iocean
        if(LMARSWAP) write(iolst,*) 'At offshore points, replace land',
     &               ' use type read from data file with type ',iocean
      endif
      write(iolst,*)
      write(iolst,*) 'Mesh Density for CTG files  =  ',meshctg
      write(iolst,*) 'Mesh Density for GLAZ files =  ',meshglaz
      write(iolst,*)
      write(iolst,*) 'QA Threshold (%)        =  ',ithres

      write(iolst,*)
      write(iolst,*) 'Input Land Use File(s) -----'
      do k=1,ndbf
      write(iolst,*)
        write(iolst,'(a10,a132)') ' luindat: ' ,justname(k)
        if(lulc(k).eq.2) then
           write(iolst,*) '(USGS Global - Lambert Azimuthal: ',
     &                  cglobal(iglazr(k)),')'
        elseif(lulc(k).eq.1) then
           write(iolst,*) '(USGS CTG - compressed)'
        elseif(lulc(k).eq.3) then
           write(iolst,*) '(New Zealand Generic)'
        elseif(lulc(k).eq.4) then
           write(iolst,*) '(USGS NLCD 1992)'
c        elseif(lulc(k).eq.5) then
c           write(iolst,*) '(USGS NLCD 2001)'
        endif
      enddo
      write(iolst,*)
      write(iolst,*) 'Default Datum-Region for each File Type --------'
      write(iolst,*) 'CTG    : ',dctg
      write(iolst,*) 'USGSLA : ',dusgsla
      write(iolst,*) 'NZGEN  : ',dnzgen
      write(iolst,*) 'NLCD92 : ',dnlcd
c      write(iolst,*) 'NLCD01 : ',dnlcd
      write(iolst,*) 'GSHHS  : ',dwvs
c      write(iolst,*) 'WDBII  : ',dwdbii

      if(LPREV) then
      write(iolst,*)
         write(iolst,'(a10,a132)') 'prevdat : ' ,prevdat
         write(iolst,*) '(Previous LUDAT file read as input)'
      endif

      write(iolst,*)
      write(iolst,*) 'Output File Names -------------'
      write(iolst,'(a10,a132)') 'runlst  : ' ,runlst
      write(iolst,'(a10,a132)') 'ludat   : ' ,ludat
      if(LFINAL) then
         write(iolst,*) '(LUDAT written in fractional land use format)'
      else
         write(iolst,*) '(LUDAT written in continuation format)'
      endif

      write(iolst,*)
      write(iolst,*) 'Grid data (for output) ---------------------'
      write(iolst,*) 'datum  : ' ,datum
      write(iolst,*) 'pmap   : ' ,pmap
      if(LUTM) then
         write(iolst,*) 'Hemisphere : ',utmhem
         write(iolst,*) 'UTM zone   : ' ,izone
      endif
      write(iolst,*) 'xorigin: ' ,xorigin
      write(iolst,*) 'yorigin: ' ,yorigin
      write(iolst,*) 'izone  : ' ,izone
      write(iolst,*) 'dgrid  : ' ,dgrid
      write(iolst,*) 'nx     : ' ,nx
      write(iolst,*) 'ny     : ' ,ny
      if(LLCC.or.LPS.or.LEM.or.LLAZA.or.LTTM) then
         write(iolst,*) 'rlat(N): ' ,reflat
         write(iolst,*) 'rlon(E): ' ,reflon
         if(LLCC.or.LPS)write(iolst,*) 'xlat1  : ' ,xlat1
         if(LLCC)write(iolst,*) 'xlat2  : ' ,xlat2
      endif
      if(LLCC.or.LLAZA.or.LTTM) then
         write(iolst,*) 'feast  : ' ,feast
         write(iolst,*) 'fnorth : ' ,fnorth
      endif
      write(iolst,*)
      write(iolst,*)

c --- Set utm zone for coordinate calls
	iutm=izone
	if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

c --- Set translation vectors from the output grid to LL
	if(lutm) then
c ---    Using UTM grid
	   call GLOBE1('UTM     ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
	elseif(llcc) then
c ---    Using Lambert Conformal grid
	   call GLOBE1('LCC     ',idum,xdum,xlat1,xlat2,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
	elseif(lps) then
c ---    Using Polar Stereographic grid
	   call GLOBE1('PS      ',idum,xdum,xlat1,xdum,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
	elseif(lem) then
c ---    Using Equatorial Mercator grid
	   call GLOBE1('EM      ',idum,xdum,xlat1,-xlat1,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
	elseif(llaza) then
c ---    Using Lambert Azimuthal Equal Area grid
	   call GLOBE1('LAZA    ',idum,xdum,xdum,xdum,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
	elseif(lttm) then
c ---    Using Tangential TM grid
	   call GLOBE1('TM      ',idum,tmsone,xdum,xdum,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
	endif

c --- Map corners of grid to LAT,LON(east)

	call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xllk,yllk,xlonsw,ylatsw,idum,c4dum)
	call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xllk,yurk,xlonnw,ylatnw,idum,c4dum)
	call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xurk,yurk,xlonne,ylatne,idum,c4dum)
	call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xurk,yllk,xlonse,ylatse,idum,c4dum)
c --- Convert to West Longitude
	xlonsw=-xlonsw
	xlonnw=-xlonnw
	xlonne=-xlonne
	xlonse=-xlonse

c --- Get the coastline data
      if(LCOAST) then
        if(LBLNREAD) then
c         Read in the already-extracted .BLN file      --- call COASTRD
          write(*,*) 'Reading in pre-processed coastal data'
          call COASTRD
        else
c         Extract the data                            --- call COASTEXT
          call COASTEXT(xllk,yllk,xurk,yurk,izone,reflat,reflon,
     &                  xlat1,xlat2,feast,fnorth,utmhem,datum)
        endif
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682          Level: 060202            READCF
c               J. Scire, D. Strimaitis   Earth Tech, Inc.
c
c --- PURPOSE:  Read the control file containing the file names of
c               the input and output files of the run, and associated
c               control variables, place into program variables,
c               and QA information.
c
c --- UPDATES:
c --- Ver 2.66 Level 060202 from Ver 2.65 Level 051201       DGS
c              - Add LQACELL for QA plot file output
c              - Change filename strings from c*70 to c*132
c                This is required by CALUTILS 2.3 and later which uses
c                c*132 in FILCASE and COMLINE
c --- Ver 2.65 Level 051201 from Ver 2.64 Level 050128       KAM
c              - Add NLCD data types - hide 2001 for now
c --- Ver 2.63 Level 050128 from Ver 2.62 Level 041215       DGS
c              - Add MESHCTG, MESHGLAZ, LMARSWAP, LMARFILL, IOCEAN
c              - Add COORDSVER call; write documentation to list file
c --- Ver 2.62 Level 041215 from Ver 2.6 Level 040921        DGS
c              - Restrict Global files to the first application of
c                a continuation application
c --- Ver 2.6 Level 040921 from Ver 2.5 Level 040512   - K. Morrison
c              - Add coastline processing
c              - Add default LU type for points misidentified as ocean
c --- Ver 2.5 Level 040512 from Ver 2.41 Level 030429        DGS
c              - Allow multiple input land use data files
c --- Ver 2.41  Level 030402 to Ver 2.42 Level 030709  - D. Strimaitis
c              - Fix type assignment for LCFILES
c --- Ver 2.4  Level 011003 to Level 030402    - D. Strimaitis
c              - Add DATUM for input and output files
c              - PMAP = UTM, TTM, LCC, PS, EM, or LAZA (imap removed)
c              - LSOHEM (L: T/F) replaced by UTMHEM (C*4: N/S)
c              - Add false easting/northing
c              - Add type string to XTRACTLL calls
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c
c        Parameters: iocnt, iolst, IOMESG, MXVAR, MXFIL
c
c --- OUTPUT:
c
c ---    Common block /FILNAM/ variables:
c           runinp,runlst,luindat(mxfil),ludat,prevdat,
c           gshhsin,coastbln,
c           justname(mxfil),
c           lcfiles
c ---    Common block /CONTROL/ variables:
c           lfinal,lprev,lcoast,lblnread,
c           lutm,llcc,lps,lem,llaza,lttm,
c           ithres,iglazr(mxfil),lulc(mxfil),
c           pmap,dctg,dusgsla,dnzgen,dwvs,dwdbii,
c           ndbf,meshctg,meshglaz,lmarswap,lmarfill,iocean,
c           lqacell
c ---    Common block /GRID/ variables:
c           nx,ny,dgrid,xorigin,yorigin,xllk,yllk,xurk,yurk,
c           izone,reflat,reflon,xlat1,xlat2,feast,fnorth,
c           utmhem,datum,clat0,clon0,clat1,clat2
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE, XTRACTLL, COORDSVER
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.ctg'
      include 'params.cal'
c
c --- Include common blocks
      include 'control.ctg'
      include 'grid.ctg'
      include 'filnam.ctg'
      include 'lutabs.ctg'
      include 'qa.ctg'
c
c --- Local variables
      character*4 ctemp(132,10)
      character*4 clatlon(16,4)
      character*4 cpmap(8),cdatum(8)
      character*12 cvdic(mxvar,4)
      character*50 verdoc
      character*132 dbfil
      integer ivleng(mxvar,4),ivtype(mxvar,4)
      integer lulcold,iglazrold
      logical lecho,lerrcf
      logical lzerob

c --- Initialize local variables
      data lecho/.false./, lerrcf/.false./
      data names/6/, ntypes/6/ , nfiles/10/

c --- Set Dictionary

      data cvdic/
     a  'LUINDAT','LUDAT','RUNLST','PREVDAT','GSHHSIN','COASTBLN',
     a  'NDBF','LCFILES',  52*' ',
     2  'CTG','NZGEN','GLAZNA','GLAZSA','GLAZEU','GLAZAS','GLAZAF',
     2  'GLAZAP','NLCD92','NLCD01', 50*' ',
     b  'LFINAL','LPREV','LULC','IGLAZR','ITHRESH','LCOAST',
     b  'LBLNREAD','IDEFTYP','MESHCTG','MESHGLAZ','LQACELL',
     b  'IOCEAN','LMARSWAP','LMARFILL',
     b  'DCTG','DUSGSLA','DNZGEN','DNLCD','DWVS','DWDBII',  40*' ',
     c  'PMAP','IUTMZN','UTMHEM','RLAT0','RLON0','RLAT1','RLAT2',
     c  'DATUM','XREFKM','YREFKM','NX','NY','DGRIDKM',
     c  'FEAST','FNORTH',  45*' '/

      data ivleng/
     a  6*132, 2*1, 52*0,
     2  10*132, 50*0,
     b  14*1, 6*132, 40*0,
     c  8, 2*1, 4*16, 8, 7*1, 45*0/

c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
      data ivtype/
     a  6*4, 2, 3, 52*0,
     2  10*4, 50*0,
     b  2*3, 3*2, 2*3, 3*2, 3, 2, 2*3, 6*4, 40*0,
     c  4, 2, 6*4, 1, 1, 2, 2, 3*1, 45*0/

c ------------------
c --- Input Group 0
c ------------------

c --- Initialize the temporary arrays
      do i=1,names
         do j=1,132
            ctemp(j,i)(1:1)=' '
         enddo
      enddo

c --- Read the group data
       call READIN(cvdic(1,1),ivleng(1,1),ivtype(1,1),iocnt,iomesg,
     & lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),ctemp(1,5),
     2 ctemp(1,6),ndbf,lcfiles,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum)

      if(ndbf.EQ.0) then
         lzerob=.FALSE.
      else
         lzerob=.TRUE.
      endif

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')luindat(1)=' '
      if(ctemp(1,2)(1:1).ne.' ')ludat=' '
      if(ctemp(1,3)(1:1).ne.' ')runlst=' '
      if(ctemp(1,4)(1:1).ne.' ')prevdat=' '
      if(ctemp(1,5)(1:1).ne.' ')gshhsin=' '
      if(ctemp(1,6)(1:1).ne.' ')coastbln=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')luindat(1)(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')ludat(j:j)=ctemp(j,2)(1:1)
         if(ctemp(j,3)(1:1).ne.' ')runlst(j:j)=ctemp(j,3)(1:1)
         if(ctemp(j,4)(1:1).ne.' ')prevdat(j:j)=ctemp(j,4)(1:1)
         if(ctemp(j,5)(1:1).ne.' ')gshhsin(j:j)=ctemp(j,5)(1:1)
         if(ctemp(j,6)(1:1).ne.' ')coastbln(j:j)=ctemp(j,6)(1:1)
      enddo

c --- Convert the file names to the proper case
      call FILCASE(lcfiles,luindat(1))
      call FILCASE(lcfiles,ludat)
      call FILCASE(lcfiles,runlst)
      call FILCASE(lcfiles,prevdat)
      call FILCASE(lcfiles,gshhsin)
      call FILCASE(lcfiles,coastbln)

c --- Open listfile
      open(iolst,file=runlst,status='unknown')

c --- Write banner to list file
      write(iolst,5) ver,level
5     format(///,26x,'CTGPROC OUTPUT SUMMARY',/,19x,'VERSION:  ',A12,
     1       ' LEVEL:  ',A12///)

c --- Obtain COORDS version information
      call COORDSVER(iolst,verdoc)
      write(iolst,*)'Internal Coordinate Transformations by ',verdoc
      write(iolst,*)
      write(iolst,*)
      write(iolst,*)

c --------------------
c --- Input Group 0(b)
c --------------------

c --- Make array entries for 1 file if multiple land use DB files
c --- are not provided (old control file)
      if(.NOT.LZEROB) then
        ndbf=1
      else
c ---   Process group 0(b)

        do k=1,ndbf
c ---     Initialize the temporary arrays for the file names
          do i=1,nfiles
             do j=1,132
                ctemp(j,i)(1:1)=' '
             enddo
          enddo
          do j=1,132
             dbfil(j:j)=' '
          enddo

c ---     Read one DB filename entry (one of 10 DB types)
      call READIN(cvdic(1,2),ivleng(1,2),ivtype(1,2),iocnt,iomesg,
     1 lecho,
     2 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),ctemp(1,5),
     3 ctemp(1,6),ctemp(1,7),ctemp(1,8),ctemp(1,9),ctemp(1,10),
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     8 idum,idum)

c ---     Process if filename array has room for this entry
          if(k.LE.mxfil) then

c ---        Identify type of DB file by first character
             ii=0
             j=1
             if(ctemp(j,1)(1:1).ne.' ') then
                ii=1
c ---           CTG
                lulc(k)=1
                iglazr(k)=0
             elseif(ctemp(j,2)(1:1).ne.' ') then
                ii=2
c ---           NZGEN
                lulc(k)=3
                iglazr(k)=0
             elseif(ctemp(j,3)(1:1).ne.' ') then
                ii=3
c ---           GLAZNA
                lulc(k)=2
                iglazr(k)=1
             elseif(ctemp(j,4)(1:1).ne.' ') then
                ii=4
c ---           GLAZSA
                lulc(k)=2
                iglazr(k)=2
             elseif(ctemp(j,5)(1:1).ne.' ') then
                ii=5
c ---           GLAZEU
                lulc(k)=2
                iglazr(k)=3
             elseif(ctemp(j,6)(1:1).ne.' ') then
                ii=6
c ---           GLAZAS
                lulc(k)=2
                iglazr(k)=4
             elseif(ctemp(j,7)(1:1).ne.' ') then
                ii=7
c ---           GLAZAF
                lulc(k)=2
                iglazr(k)=5
             elseif(ctemp(j,8)(1:1).ne.' ') then
                ii=8
c ---           GLAZAP
                lulc(k)=2
                iglazr(k)=6
             elseif(ctemp(j,9)(1:1).ne.' ') then
                ii=9
c ---           NLCD92
                lulc(k)=4
                iglazr(k)=0
             elseif(ctemp(j,10)(1:1).ne.' ') then
                ii=10
c ---           NLCD01
                lulc(k)=5
                iglazr(k)=0
             elseif(ii.EQ.0) then
                stop 'READCF: DB file type index = 0'
             endif


c ---        Transfer the char*4 data into the char*132 variable
             do j=1,132
               if(ctemp(j,ii)(1:1).ne.' ')dbfil(j:j)=ctemp(j,ii)(1:1)
             enddo

c ---        Convert the file name to the proper case
             call FILCASE(lcfiles,dbfil)         

c ---        Place information in DB array, up to MXFIL
             luindat(k)=dbfil

c ---        Extract just file name without path info.
             ipath=0
             ilen=0
             do n=1,132
                 if(dbfil(n:n).NE.' ') ilen=n
                 if(dbfil(n:n).EQ.'/' .OR. dbfil(n:n).EQ.'\') ipath=n
             enddo
             read(dbfil(ipath+1:ilen),'(a)') justname(k)

          endif
        enddo

      endif      

c -----------------
c --- Input Group 1
c -----------------

c --- Initialize the temporary arrays for the Datum-Region names
      do i=1,ntypes
         do j=1,132
            ctemp(j,i)(1:1)=' '
         enddo
      enddo

c --- Initialize old control file variables to the current values
      if(.NOT.LZEROB) then
         lulcold=lulc(1)
         iglazrold=iglazr(1)
      endif

      call READIN(cvdic(1,3),ivleng(1,3),ivtype(1,3),iocnt,iolst,lecho,
     1 LFINAL,LPREV,LULCOLD,IGLAZROLD,ITHRES,LCOAST,LBLNREAD,IDEFTYP,
     2 meshctg,meshglaz,lqacell,iocean,lmarswap,lmarfill,
     3 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),ctemp(1,5),
     4 ctemp(1,6),
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     8 idum,idum,idum,idum)

c --- Transfer the char*4 data into the char*8 variables
      if(ctemp(1,1)(1:1).ne.' ') then
         do j=1,8
            dctg(j:j)=ctemp(j,1)(1:1)
         enddo
      endif
      if(ctemp(1,2)(1:1).ne.' ') then
         do j=1,8
            dusgsla(j:j)=ctemp(j,2)(1:1)
         enddo
      endif
      if(ctemp(1,3)(1:1).ne.' ') then
         do j=1,8
            dnzgen(j:j)=ctemp(j,3)(1:1)
         enddo
      endif
      if(ctemp(1,4)(1:1).ne.' ') then
         do j=1,8
            dwvs(j:j)=ctemp(j,4)(1:1)
         enddo
      endif
      if(ctemp(1,5)(1:1).ne.' ') then
         do j=1,8
            dwdbii(j:j)=ctemp(j,5)(1:1)
         enddo
      endif
      if(ctemp(1,6)(1:1).ne.' ') then
         do j=1,8
            dwdbii(j:j)=ctemp(j,6)(1:1)
         enddo
      endif

c --- Make array entries for 1 file if multiple land use DB files
c --- are not provided (old control file)
      if(.NOT.LZEROB) then
         lulc(1)=lulcold
         iglazr(1)=iglazrold
      endif

c -----------------
c --- Input Group 2
c -----------------

c --- Initialize the temporary arrays for the character lat/lon fields
      do i=1,4
         do j=1,16
            clatlon(j,i)(1:1)=' '
         enddo
      enddo
      do j=1,16
         clat0(j:j)=' '
         clon0(j:j)=' '
         clat1(j:j)=' '
         clat2(j:j)=' '
      enddo

c --- Initialize the temporary array for the Datum-Region name and 
c --- map projection
      do j=1,8
         cpmap(j)(1:1)=' '
         cdatum(j)(1:1)=' '
      enddo

c --- Initialize input false easting and northing with defaults
      feastin=feast
      fnorthin=fnorth

      call READIN(cvdic(1,4),ivleng(1,4),ivtype(1,4),iocnt,iolst,lecho,
     1 CPMAP,IUTMZN,UTMHEM,
     2 CLATLON(1,1),CLATLON(1,2),CLATLON(1,3),CLATLON(1,4),
     3 CDATUM,XREFKM,YREFKM,NX,NY,DGRIDKM,FEASTIN,FNORTHIN,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum)

c --- Transfer the char*4 data into the char*16 variables
      do j=1,16
         if(clatlon(j,1)(1:1).ne.' ')clat0(j:j)=clatlon(j,1)(1:1)
         if(clatlon(j,2)(1:1).ne.' ')clon0(j:j)=clatlon(j,2)(1:1)
         if(clatlon(j,3)(1:1).ne.' ')clat1(j:j)=clatlon(j,3)(1:1)
         if(clatlon(j,4)(1:1).ne.' ')clat2(j:j)=clatlon(j,4)(1:1)
      enddo

c --- Transfer the char*4 data into the char*8 variables
      if(cpmap(1)(1:1).ne.' ') then
         do j=1,8
            pmap(j:j)=cpmap(j)(1:1)
         enddo
      endif
      if(cdatum(1)(1:1).ne.' ') then
         do j=1,8
            datum(j:j)=cdatum(j)(1:1)
         enddo
      endif

c --- Pad the char*4 UTM Hemisphere
      utmhem(2:4)='   '

c --------------------------------------------------
c --- Translate selected inputs to CTGPROC variables
c --------------------------------------------------

c --- Grid variables
      xorigin=xrefkm
      yorigin=yrefkm
      dgrid=dgridkm
      xllk=xorigin
      yllk=yorigin
      xurk=xllk+real(nx)*dgridkm
      yurk=yllk+real(ny)*dgridkm
      izone=iutmzn

c --- Translate character lat/lon to real NLat/ELon
      if(clat0(1:1).NE.' ') call XTRACTLL(iolst,'LAT ',clat0,reflat)
      if(clon0(1:1).NE.' ') call XTRACTLL(iolst,'LON ',clon0,reflon)
      if(clat1(1:1).NE.' ') call XTRACTLL(iolst,'LAT ',clat1,xlat1)
      if(clat2(1:1).NE.' ') call XTRACTLL(iolst,'LAT ',clat2,xlat2)

c --- Set logicals for map projection PMAP
      if(pmap.EQ.'UTM     ')  lutm =.TRUE.
      if(pmap.EQ.'LCC     ')  llcc =.TRUE.
      if(pmap.EQ.'PS      ')   lps  =.TRUE.
      if(pmap.EQ.'EM      ')   lem  =.TRUE.
      if(pmap.EQ.'LAZA    ') llaza=.TRUE.
      if(pmap.EQ.'TTM     ')  lttm =.TRUE.

c --- Adjust projection information if needed
      if(LEM) then
c ---    Equatorial Mercator projection matches at 0.0N, 
c ---    and places the northing origin at 0.0N
         reflat=0.0
         xlat1=0.0
         xlat2=0.0
      endif

c --- Transfer input false easting and northing if the projection
c --- can use it
      if(LLCC.or.LTTM.or.LLAZA) then
         feast=feastin
         fnorth=fnorthin
      endif

c ---------------------
c --- Perform QA checks
c ---------------------

      do k=1,ndbf

c ---   Test for valid LULC
        if(lulc(k).LT.1 .OR. lulc(k).GT.5) then
           write(iolst,*)
           write(iolst,*) 'READCF:  Error in Input Group 0/1'
           write(iolst,*) 'LULC out of range      = ',lulc(k)
           write(iolst,*) 'LULC should be 1, 2, 3, 4, or 5'
           lerrcf=.TRUE.
        endif
      
c ---   Test for valid IGLAZR
        if(lulc(k).EQ.2 .AND. (iglazr(k).LT.1 .OR. 
     &     iglazr(k).GT.6)) then
           write(iolst,*)
           write(iolst,*) 'READCF:  Error in Input Group 0/1'
           write(iolst,*) 'IGLAZR out of range      = ',iglazr(k)
           write(iolst,*) 'IGLAZR should be 1 - 6'
           lerrcf=.TRUE.
        endif

c ---   Restrict Global LU processing to first application
        if(lulc(k).EQ.2 .AND. LPREV) then
           write(iolst,*)
           write(iolst,*) 'READCF:  Error in Input Group 0/1'
           write(iolst,*) 'USGS Global LU files must be processed'
           write(iolst,*) 'before any other type.  Place these files'
           write(iolst,*) 'in the first CTGPROC application of a'
           write(iolst,*) 'continuation run.'
           lerrcf=.TRUE.
        endif

      enddo

c --- Test for valid ITHRES
      if(ithres.LT.0 .OR. ithres.GT.100) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'ITHRES out of range      = ',ithres
         write(iolst,*) 'ITHRES should be 0 to 100'
         lerrcf=.TRUE.
      endif

c --- Drop IDEFTYP
cc --- Test for valid IDEFTYP
c      if(lcoast.and.(ideftyp.lt.0.or.ideftyp.gt.9)) then
c         write(iolst,*)
c         write(iolst,*) 'READCF:  Error in Input Group 1'
c         write(iolst,*) 'IDEFTYP out of range     = ',ideftyp
c         write(iolst,*) 'IDEFTYP should be 0 to 9'
c         lerrcf=.TRUE.
c      endif

c --- Test for valid Coastal Processing
      if(lcoast .and. .not.lmarswap .and. .not.lmarfill) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'Coastal processing REQUIRES a method'
         write(iolst,*) 'LCOAST, LMARSWAP, LMARFILL = ',lcoast,
     &                   lmarswap,lmarfill
         lerrcf=.TRUE.
      endif

c --- Test for valid IOCEAN (must be in LU list)
      if(lcoast) then
         if(iocean.LT.1 .OR. iocean.GT.92) then
           write(iolst,*)
           write(iolst,*) 'READCF:  Error in Input Group 1'
           write(iolst,*) 'IOCEAN out of range      = ',iocean
           write(iolst,*) 'IOCEAN should be 1 to 92'
           lerrcf=.TRUE.
         elseif(lumap(iocean).EQ.0) then
           write(iolst,*)
           write(iolst,*) 'READCF:  Error in Input Group 1'
           write(iolst,*) 'IOCEAN is not valid      = ',iocean
           write(iolst,*) 'IOCEAN must be in USGS LU table'
           lerrcf=.TRUE.
         endif
      endif

c --- Test for valid MESHCTG,MESHGLAZ
      if(meshctg.lt.1.or.meshglaz.lt.1) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'MESH density must be greater than 0'
         write(iolst,*) 'MESHCTG, MESHGLAZ       = ',meshctg,meshglaz
         lerrcf=.TRUE.
      endif

c --- Test for valid PMAP
      if(lutm.OR.llcc.OR.lps.OR.lem.OR.llaza.OR.lttm) then
c        OK
      else
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'Unknown PMAP             = ',pmap
         write(iolst,*) 'PMAP must be UTM,LCC,PS,EM,LAZA, or TTM'
         lerrcf=.TRUE.
      endif

c --- Test for valid IUTMZN
      if((iutmzn.LT.1 .OR. iutmzn.GT.60) .AND. LUTM) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'IUTMZN out of range      = ',iutmzn
         write(iolst,*) 'IUTMZN should be 1 to 60'
         lerrcf=.TRUE.
      endif

c --- Test for valid UTMHEM
      if((utmhem.NE.'N   '.AND.utmhem.NE.'S   ') .AND. LUTM) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'UTMHEM out of range      = ',utmhem
         write(iolst,*) 'UTMHEM should be N or S'
         lerrcf=.TRUE.
      endif

c --- Test for lat/lon of origin for LCC/PS/EM/LAZA/TTM map projection
      if(LLCC .or. LPS .or. LEM .or. LLAZA .or. LTTM) then
         if(reflat .LT. -900.0 .AND. reflon .LT. -900.0) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 2'
            write(iolst,*) 'Missing lat/lon of origin for ',pmap
            lerrcf=.TRUE.
         endif
      endif

c --- Test for matching latitudes for LCC map projection
      if(LLCC) then
         if(xlat1 .LT. -900.0 .OR. xlat2 .LT. -900.0) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 2'
            write(iolst,*) 'Missing matching lats for ',pmap
            lerrcf=.TRUE.
         endif
      endif

c --- Test for matching latitudes for PS/EM map projection
      if(LPS .or. LEM) then
         if(xlat1 .LT. -900.0) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 2'
            write(iolst,*) 'Missing matching lats for ',pmap
            lerrcf=.TRUE.
         endif
      endif

c --- Test for valid NX,NY
      if(nx.GT.mxnx) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'NX exceeds the parameter MXNX '
         write(iolst,*) 'NX, MXNX = ',nx,mxnx
         write(iolst,*) 'Increase MXNX in PARAMS.ctg and recompile'
         write(iolst,*) 'or reduce the number of X-grid cells'
         lerrcf=.TRUE.
      endif
      if(ny.GT.mxny) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'NY exceeds the parameter MXNY '
         write(iolst,*) 'NY, MXNY = ',ny,mxny
         write(iolst,*) 'Increase MXNY in PARAMS.ctg and recompile'
         write(iolst,*) 'or reduce the number of Y-grid cells'
         lerrcf=.TRUE.
      endif

c --- Test for valid DGRIDKM
      if(dgridkm.LE.0.0) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'DGRIDKM must be positive '
         write(iolst,*) 'DGRIDKM = ',dgridkm
         lerrcf=.TRUE.
      endif

c --- STOP now if error exists in the control file
      if(LERRCF) then
         write(*,*)'ERRORS are found in the CONTROL file'
         write(*,*)'Review messages written to the LIST file'
         stop
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine comp
c----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682         Level: 070430               COMP
c ---           J. Scire, D. Strimaitis  Earth Tech, Inc.
c
c --- PURPOSE:  Main computational routine
c
c --- Updates
c     Ver 2.67 Level 070430 from Ver 2.66 Level 060202       IWL
c              For NLCD92 data, code modified to read in data 
c              filename regardless of whether file was unzipped using
c              WinZip (e.g."alabama_NLCD_flat_031600.bin"
c              or GZIP (e.g. "alabama.nlcd.bin")
c     Ver 2.66 Level 060202 from Ver 2.65 Level 051201       DGS
c              Set LQA from LQACELL in /control/
c              Change filename strings from c*70 to c*132
c              This is required by CALUTILS 2.3 and later which uses
c              c*132 in FILCASE and COMLINE
c     Ver 2.65 Level 051201 from Ver 2.64 Level 050128       KAM
c              Add NLCD92 data type.
c     Ver 2.63 Level 050128 from Ver 2.62 Level 041215       DGS
c              Add mesh density option for spreading LU in an input
c              grid cell to other points within the cell to improve
c              the sampling density.  This assumes that LU is at the
c              cell center, and it is characteristic of all points
c              within the cell (limit of LU resolution).  This replaces
c              the 16-fold split of the global LU dataset with
c              MESHGLAZ=4, and introduces splitting of CTG cells.
c              Add LMARFILL coastal processing control with IOCEAN.
c     Ver 2.62 Level 041215 from Ver 2.61 Level 041013       DGS
c              Remove screen on transformed (x,y) points before call
c              to LUGRID in subroutine COMP.  The screen removed some
c              points near the edges of the grid domain that should
c              have been included.
c              Process global landuse files first because of the logic
c              required to overcome spurious water results when land
c              is on "another continent".
c              Characterize number of points read and used in list
c              file output.
c     Ver 2.61 Level 041013 from Ver 2.6 Level 040921
c              Fix bug (introduced in version 2.6) in LUGRID call for
c              Global dataset temporary array                KAM
c              Remove screen on Global land use files if only 1 global
c              file is specified.                            DGS
c     Ver 2.6 Level 040921 from Ver 2.51 Level 040512 KAM
c              Check global data cells identified as water to
c              classify as ocean if coastline processing invoked
c     Ver 2.5  Level 040512 from Ver 2.4  Level 030402       DGS
c              Add screen to skip Global files based on grid location
c              Add loop over multiple land use data files
c              Count of global LU updates made before splitting
c              Exclude cells with all water when land is seen in
c              another dataset (USGS Global)
c     Ver 2.4  Level 030402 from Ver 2.3  Level 021028       DGS
c              Implement new COORDS with DATUMs
c              All coordinate transformations brought into COMP
c     Ver 2.3  Level 021028 from Ver 2.0  Level 020226       KAM
c              Add NZ generic data format.
c     Ver 2.0  Level 020226 from Ver 2.0  Level 011003       DGS
c              Split Global cell into 16 to distribute property from
c              1km cell into 250m cells to improve sampling, and 
c              interpret coordinate location as center of Global cell.
c
c --- INPUTS:
c       Parameters: iolst, IOMESG, mxcat,mxnx,mxny
c
c --- OUTPUT:  none
c
c --- COMP called by:  MAIN
c --- COMP calls:      GET4CNR, LUGRID, CTRANS, GLOBE, SVGRID,
c                      GSELECT, GMERGE
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.ctg'

c --- include common blocks
      include 'control.ctg'
      include 'filnam.ctg'
      include 'grid.ctg'
      include 'lutabs.ctg'

c --- Local variables
      character*80 cline
      character*4 c4dum
      character*1 kbuf
      integer ngrid(mxcat,mxnx,mxny)
      integer ngtmp(mxcat,mxnx,mxny)
      integer igkeep(6)
      integer luftype(4)
c --- For coordinate transformations
      real*8 vecti(9),vecto(9)
      character*12 caction
      logical ltrans

c --- For QA output
      logical lqa

c --- Local arrays for Global Lambert Azimuthal grid parameters
      integer nxia(6),nyia(6),nxoffa(6),nyoffa(6)
      real dxia(6),dyia(6),xorga(6),yorga(6),rlata(6),rlona(6)

c --- Data for Global Lambert Azimuthal grid parameters
c                   1 = North America
c                   2 = South America
c                   3 = Eurasia (Optimized for Europe) 
c                   4 = Eurasia (Optimized for Asia) 
c                   5 = Africa
c                   6 = Australia Pacific
c                     1       2       3       4       5       6
      data nxia/    9223,   6000,  13000,  13000,   8350,   9300 /
      data nyia/    8996,   8000,  13000,  12000,   9276,   8000 /
      data dxia/     1.0,    1.0,    1.0,    1.0,    1.0,    1.0 /
      data dyia/     1.0,    1.0,    1.0,    1.0,    1.0,    1.0 /
      data xorga/ -4487., -3000., -3000., -8000., -4458., -5000. /
      data yorga/ -4515., -4899., -4999., -5499., -4795., -3944.891/
      data rlata/   50.0,  -15.0,   55.0,   45.0,    5.0,  -15.0 /
      data rlona/  -100.,  -60.0,   20.0,  100.0,   20.0,  135.0 /
      data nxoffa/     0,      0,      0,      0,      0,      0 /
      data nyoffa/     0,      0,      0,      0,      0,      0 /

c
c --- Local arrays and data for NLCD 1992 data files
c
      character*16 statenm(52),state
      character*132 tstate
      real xorgnlcd(52),yorgnlcd(52)
      integer*2 nrnlcd(52),ncnlcd(52)
      integer*1 lcc1
      data statenm(1),nrnlcd(1),ncnlcd(1),xorgnlcd(1),yorgnlcd(1)
     & /"alabama         ",19175,11864,  696270.,1386300./
      data statenm(2),nrnlcd(2),ncnlcd(2),xorgnlcd(2),yorgnlcd(2)
     & /"arizona         ",23328,20036,-1747230.,1701780./
      data statenm(3),nrnlcd(3),ncnlcd(3),xorgnlcd(3),yorgnlcd(3)
     & /"arkansas        ",13461,14869,  122340.,1510800./
      data statenm(4),nrnlcd(4),ncnlcd(4),xorgnlcd(4),yorgnlcd(4)
     & /"california_north",20260,14971,-2361900.,2455830./
      data statenm(5),nrnlcd(5),ncnlcd(5),xorgnlcd(5),yorgnlcd(5)
     & /"california_south",20260,23851,-2361900.,1849830./
      data statenm(6),nrnlcd(6),ncnlcd(6),xorgnlcd(6),yorgnlcd(6)
     & /"colorado        ",16915,21418,-1146810.,2074020./
      data statenm(7),nrnlcd(7),ncnlcd(7),xorgnlcd(7),yorgnlcd(7)
     & /"connecticut     ", 5264, 5248, 1831260.,2368230./
      data statenm(8),nrnlcd(8),ncnlcd(8),xorgnlcd(8),yorgnlcd(8)
     & /"delaware        ", 5170, 3300, 1703490.,2055690./
      data statenm(9),nrnlcd(9),ncnlcd(9),xorgnlcd(9),yorgnlcd(9)
     & /"florida         ",23393,27314,  783810., 969660./
      data statenm(10),nrnlcd(10),ncnlcd(10),xorgnlcd(10),yorgnlcd(10)
     & /"georgia         ",17648,17074,  926910.,1417050./
      data statenm(11),nrnlcd(11),ncnlcd(11),xorgnlcd(11),yorgnlcd(11)
     & /"idaho           ",28387,17567,-1715970.,3060150./
      data statenm(12),nrnlcd(12),ncnlcd(12),xorgnlcd(12),yorgnlcd(12)
     & /"illinois        ",21096,11835,  378030.,2201310./
      data statenm(13),nrnlcd(13),ncnlcd(13),xorgnlcd(13),yorgnlcd(13)
     & /"indiana         ",15757, 9184,  686850.,2139030./
      data statenm(14),nrnlcd(14),ncnlcd(14),xorgnlcd(14),yorgnlcd(14)
     & /"iowa            ",11712,17834,  -52620.,2288880./
      data statenm(15),nrnlcd(15),ncnlcd(15),xorgnlcd(15),yorgnlcd(15)
     & /"kansas          ",11788,21829, -532680.,1903920./
      data statenm(16),nrnlcd(16),ncnlcd(16),xorgnlcd(16),yorgnlcd(16)
     & /"kentucky        ",11189,21849,  569070.,1849050./
      data statenm(17),nrnlcd(17),ncnlcd(17),xorgnlcd(17),yorgnlcd(17)
     & /"louisiana       ",14956,17234,  181380.,1115490./
      data statenm(18),nrnlcd(18),ncnlcd(18),xorgnlcd(18),yorgnlcd(18)
     & /"maine           ",17729,11408, 1923780.,3016020./
      data statenm(19),nrnlcd(19),ncnlcd(19),xorgnlcd(19),yorgnlcd(19)
     & /"maryland        ", 7014,13575, 1396050.,2038290./
      data statenm(20),nrnlcd(20),ncnlcd(20),xorgnlcd(20),yorgnlcd(20)
     & /"massachusetts   ", 6312,10672, 1826460.,2483430./
      data statenm(21),nrnlcd(21),ncnlcd(21),xorgnlcd(21),yorgnlcd(21)
     & /"michigan        ",23944,22795,  428490.,2835390./
      data statenm(22),nrnlcd(22),ncnlcd(22),xorgnlcd(22),yorgnlcd(22)
     & /"minnesota       ",21780,19430,  -92400.,2931270./
      data statenm(23),nrnlcd(23),ncnlcd(23),xorgnlcd(23),yorgnlcd(23)
     & /"mississippi     ",18639,11058,  407970.,1361550./
      data statenm(24),nrnlcd(24),ncnlcd(24),xorgnlcd(24),yorgnlcd(24)
     & /"missouri        ",17045,19665,   18600.,1964100./
      data statenm(25),nrnlcd(25),ncnlcd(25),xorgnlcd(25),yorgnlcd(25)
     & /"montana         ",19094,30100,-1498260.,3044730./
      data statenm(26),nrnlcd(26),ncnlcd(26),xorgnlcd(26),yorgnlcd(26)
     & /"nebraska        ",12153,24363, -671730.,2250990./
      data statenm(27),nrnlcd(27),ncnlcd(27),xorgnlcd(27),yorgnlcd(27)
     & /"nevada          ",28903,18756,-2037840.,2358600./
      data statenm(28),nrnlcd(28),ncnlcd(28),xorgnlcd(28),yorgnlcd(28)
     & /"new_hampshire   ",10237, 5364, 1879380.,2734830./
      data statenm(29),nrnlcd(29),ncnlcd(29),xorgnlcd(29),yorgnlcd(29)
     & /"new_jersey      ", 9932, 4297, 1721790.,2242860./
      data statenm(30),nrnlcd(30),ncnlcd(30),xorgnlcd(30),yorgnlcd(30)
     & /"new_mexico      ",21277,20583,-1233900.,1629780./
      data statenm(31),nrnlcd(31),ncnlcd(31),xorgnlcd(31),yorgnlcd(31)
     & /"new_york        ",17455,23005, 1317210.,2663820./
      data statenm(32),nrnlcd(32),ncnlcd(32),xorgnlcd(32),yorgnlcd(32)
     & /"north_carolina  ",12094,26378, 1047690.,1699530./
      data statenm(33),nrnlcd(33),ncnlcd(33),xorgnlcd(33),yorgnlcd(33)
     & /"north_dakota    ",12143,19386, -624210.,2913750./
      data statenm(34),nrnlcd(34),ncnlcd(34),xorgnlcd(34),yorgnlcd(34)
     & /"ohio            ",15385,12371,  922170.,2251080./
      data statenm(35),nrnlcd(35),ncnlcd(35),xorgnlcd(35),yorgnlcd(35)
     & /"oklahoma        ",13368,25428, -621210.,1573800./
      data statenm(36),nrnlcd(36),ncnlcd(36),xorgnlcd(36),yorgnlcd(36)
     & /"oregon          ",20221,23924,-2301360.,2907510./
      data statenm(37),nrnlcd(37),ncnlcd(37),xorgnlcd(37),yorgnlcd(37)
     & /"pennsylvania    ",11105,17370, 1261380.,2295150./
      data statenm(38),nrnlcd(38),ncnlcd(38),xorgnlcd(38),yorgnlcd(38)
     & /"rhode_island    ", 3507, 2463, 1968930.,2374110./
      data statenm(39),nrnlcd(39),ncnlcd(39),xorgnlcd(39),yorgnlcd(39)
     & /"south_carolina  ",11915,15620, 1138290.,1458540./
      data statenm(40),nrnlcd(40),ncnlcd(40),xorgnlcd(40),yorgnlcd(40)
     & /"south_dakota    ",13774,20585, -652560.,2577510./
      data statenm(41),nrnlcd(41),ncnlcd(41),xorgnlcd(41),yorgnlcd(41)
     & /"tennessee       ", 9396,26006,  498660.,1613760./
      data statenm(42),nrnlcd(42),ncnlcd(42),xorgnlcd(42),yorgnlcd(42)
     & /"texas_n         ",16717,28285, -664320.,1518240./
      data statenm(43),nrnlcd(43),ncnlcd(43),xorgnlcd(43),yorgnlcd(43)
     & /"texas_se        ",24577,20078, -364860.,1047900./
      data statenm(44),nrnlcd(44),ncnlcd(44),xorgnlcd(44),yorgnlcd(44)
     & /"texas_sw        ",20435,22199,-1000650.,1047900./
      data statenm(45),nrnlcd(45),ncnlcd(45),xorgnlcd(45),yorgnlcd(45)
     & /"utah            ",20729,16563,-1582080.,2250990./
      data statenm(46),nrnlcd(46),ncnlcd(46),xorgnlcd(46),yorgnlcd(46)
     & /"vermont         ",10140, 5659, 1758930.,2707020./
      data statenm(47),nrnlcd(47),ncnlcd(47),xorgnlcd(47),yorgnlcd(47)
     & /"virginia        ",13032,23630, 1088700.,1967100./
      data statenm(48),nrnlcd(48),ncnlcd(48),xorgnlcd(48),yorgnlcd(48)
     & /"washington_dc   ",  784,  662, 1610190.,1936680./
      data statenm(49),nrnlcd(49),ncnlcd(49),xorgnlcd(49),yorgnlcd(49)
     & /"washington      ",14795,19975,-2144460.,3177720./
      data statenm(50),nrnlcd(50),ncnlcd(50),xorgnlcd(50),yorgnlcd(50)
     & /"west_virginia   ",13240,13265, 1154670.,2064600./
      data statenm(51),nrnlcd(51),ncnlcd(51),xorgnlcd(51),yorgnlcd(51)
     & /"wisconsin       ",18513,17414,  241980.,2715090./
      data statenm(52),nrnlcd(52),ncnlcd(52),xorgnlcd(52),yorgnlcd(52)
     & /"wyoming         ",17088,20575,-1250670.,2539890./

c --- Set array that determines the processing order by file type
c --- Order is: GLOBAL, CTG, NZGEN, NLCD92
      data luftype/2,1,3,4/

c --- Set cell dimensions for CTG data (km)
      data dxctg/0.2/, dyctg/0.2/

c --- QA output
      lqa=lqacell
      if(LQA) then
         open(40,file='qamesh.dat')
         open(41,file='qactg.dat')
         open(42,file='qaglaz.dat')
         open(43,file='qanzgen.dat')
         open(44,file='qanlcd92.dat')
      endif

c --- Report progress
      write(iomesg,*)'COMPUTATIONAL PHASE'
      write(iolst,*)
      write(iolst,*) '--------------------------'
      write(iolst,*) '    PROCESS Information'
      write(iolst,*) '--------------------------'

c --- Initialize the land use and missing grid arrays
      do 5 i=1,mxcat
      do 5 j=1,mxnx
      do 5 k=1,mxny
        ngrid(i,j,k) = 0
5     continue

c --- Set screening information for USGS GLOBAL files
c --- Check for number of files listed
      numglob=0
      do k=1,ndbf
         if(lulc(k).EQ.2) numglob=numglob+1
      enddo
      if(numglob.GT.1) then
c ---    Screen
         call GSELECT(igkeep)
      else
c ---    No Screen
         do k=1,6
            igkeep(k)=1
         enddo
      endif

c --- Set up data from previous run, if any
      if(LPREV) then
c ---    Read the grid from the previous run
         call RDGRID(ngrid,nx,ny)
      endif

c --- Set the coordinate (km) info for splitting a CTG cell
      dxctgby2=0.5*dxctg
      dxctgmesh=dxctg/FLOAT(meshctg)
      dyctgby2=0.5*dyctg
      dyctgmesh=dyctg/FLOAT(meshctg)

c --- Loop over land use data files
c ---------------------------------

c --- Loop over file type (Global files first)
      do 1010 kk=1,4
      kftype=luftype(kk)

      do 1000 kf=1,ndbf

c --- Screen for proper file type
      if(lulc(kf).NE.kftype) goto 1000

c --- Initialize Counter for each file
      nrec = 0
      nmiss = 0
      nread = 0
      ncount = 0
      ncount2 = 0

c --- Open Input Data File
      if(lulc(kf).eq.2.or.lulc(kf).eq.4) then
         open(ioinp,file=luindat(kf), status='old',form=cform,
     1        access=trim(caccess), recl=irecl)
      else
         open(ioinp,file=luindat(kf), status='old')
      endif

c --- Report progress to list file
      write(iolst,*)
      write(iolst,*) 'Land use data file: ',justname(kf)
      write(iolst,*)

c --- Set DATUM translation and branch to proper LULC
      ldatum=.FALSE.
      if(lulc(kf).EQ.1) then
         if(dctg.NE.datum) ldatum=.TRUE.
         goto 100
      elseif(lulc(kf).EQ.2) then
         if(dusgsla.NE.datum) ldatum=.TRUE.
         goto 200
      elseif(lulc(kf).EQ.3) then
         if(dnzgen.NE.datum) ldatum=.TRUE.
         goto 300
      elseif(lulc(kf).EQ.4) then
         if(dnlcd.NE.datum) ldatum=.TRUE.
         goto 400
      else
         write(iomesg,*)
         write(iomesg,*)'COMP: Invalid DB type'
         write(iomesg,*)'      Expected 1,2,3,4'
         write(iomesg,*)'      Found ',lulc(kf)
         stop
      endif


c -------------------
c --- CTG Format Data
c -------------------
100   continue
c --- Read First Header Record of CTG data
      read(ioinp,30) cline
30    format(a80)
      nrec = nrec + 1
c --- Extract UTM zone from CTG header record
      read(cline(51:55),40) ictgzone
40    format(i5)

c --- Compare UTM zones (CTG and user input) and set UTM zone
c --- conversion flag (lzone)
      lzone = .false.
      if(LUTM) then
        if(ictgzone.NE.izone)then
          lzone = .true.
          write(iolst,*) 'WARNING: UTM Zones Do NOT Agree: ',
     1                 '(ICTGZONE,IZONE)= ',ictgzone,izone
        elseif(utmhem.NE.'N   ') then
          lzone = .true.
          write(iolst,*) 'WARNING: UTMs are translated to Southern Hem.'
        endif
      endif

c --- Read and Write the CTG header
      write(iolst,*) '  Header of Compressed CTG data file: '
      write(iolst,30) cline
      do i=2,5
        read(ioinp,30) cline
        nrec = nrec + 1
        write(iolst,30) cline
      enddo

c --- Prepare for coordinate transformation
      call CTRANS(kf,xdum,xdum,xdum,xdum,ltrans,caction,vecti,vecto)

c --- Read Data Records
1     read(ioinp,50,end=999) num,lucat,ixutm,iyutm
50    format(i4,i3,i8,i9)
      nrec = nrec + 1
c --- Check to see if this line is a new coordinate or data
      if(num.EQ.0)then
c ---   A new coordinate
        ix = ixutm - 200
        iy = iyutm
c ---   Read next line
        read(ioinp,50,end=999) num,lucat,ixutm,iyutm
        nrec = nrec + 1
      endif
      if(ixutm.EQ.0)then
c ---   Data Record
        nread = nread + num
c ---   Update the land use grid array (count) for the land use 'hits'
c ---   if it is a valid land use category
        if(lucat.GE.1 .AND. lucat.LE.92)then
c ---     Loop over grid cells in a row to update land use 'hit' count
          do k=1,num
            ncount0=ncount2
            lastmesh=ncount2
c ---       Increment x coordinate by 200 m (the spacing of CTG data)
            ix = ix + 200
c ---       Convert to KM
            xc = float(ix)*0.001
            yc = float(iy)*0.001
c ---       Loop over sub-mesh centered on (xc,yc)
            xx=xc-dxctgby2
            yy=yc-dyctgby2
            do imesh=1,meshctg
               xmesh=xx+(FLOAT(imesh)-0.5)*dxctgmesh
               do jmesh=1,meshctg
                  ymesh=yy+(FLOAT(jmesh)-0.5)*dyctgmesh
c ---             Translate CTG to grid coordinates if needed
                  if(LTRANS) then
                    call GLOBE(iolst,caction,dctg,vecti,datum,vecto,
     &                         xmesh,ymesh,xgrid,ygrid,idum,c4dum)
                  else
                    xgrid=xmesh
                    ygrid=ymesh
                  endif
c ---             Update grid
                  lastmesh=ncount2
                  call lugrid(ngrid,xgrid,ygrid,lucat,ncount2)

                  if(LQA .AND. lastmesh.LT.ncount2) then
                     write(40,*) xgrid,ygrid
                     lastmesh=ncount2
                  endif

               enddo
            enddo
            if(ncount2.GT.ncount0) then
               ncount=ncount+1

               if(LQA) then
c ---             Translate CTG to grid coordinates if needed
                  if(LTRANS) then
                     call GLOBE(iolst,caction,dctg,vecti,datum,vecto,
     &                          xc,yc,xgrid,ygrid,idum,c4dum)
                  else
                     xgrid=xc
                     ygrid=yc
                  endif
                  write(41,*) xgrid,ygrid
               endif

            endif
          enddo
        else
c ---     update the x coord. value
          ix = ix + (num*200)
          nmiss = nmiss + num
        endif
      endif

c --- Loop back
      go to 1


c ----------------------
c --- GLOBAL Format Data
c ----------------------
200   continue

c --- Skip processing if not on the selection list
      nbegx=0
      nendx=0
      nbegy=0
      nendy=0
      if(igkeep(iglazr(kf)).EQ.0) goto 999

csd   Global data: Set input data file specifics
      nxi=nxia(iglazr(kf))
      nyi=nyia(iglazr(kf))
      dxi=dxia(iglazr(kf))
      dyi=dyia(iglazr(kf))
      xorg=xorga(iglazr(kf))
      yorg=yorga(iglazr(kf))
      rlat=rlata(iglazr(kf))
      rlon=rlona(iglazr(kf))

c --- Coordinate translation offsets are not currently used
c      nxoff=nxoffa(iglazr(kf))
c      nyoff=nyoffa(iglazr(kf))

c --- Set the coordinate (km) info for splitting a GLOBAL cell
      dxglazby2=0.5*dxi
      dxglazmesh=dxi/FLOAT(meshglaz)
      dyglazby2=0.5*dyi
      dyglazmesh=dyi/FLOAT(meshglaz)

c --- Set 'fuzz' margin to diagonal of square based on longer side
      fuzz=SQRT(2.)*AMAX1(dxi,dyi)

c --- Initialize temporary land use grid array
      do i=1,mxcat
        do j=1,mxnx
          do k=1,mxny
            ngtmp(i,j,k) = 0
          enddo
        enddo
      enddo

c --- Prepare for coordinate transformation
      call CTRANS(kf,rlat,rlon,xdum,xdum,ltrans,caction,vecti,vecto)

csd   Calculate the relative positions of the 4 corners of output
csd   domain in the  domain of global data  
c --- Computes a window to assist in speeding up the processing
c --- LLC,LRC,URC,ULC
      call GET4CNR(dxi,dyi,xorg,yorg,nxi,nyi,
     &             ltrans,caction,vecti,vecto,
     &             nbegx,nendx,nbegy,nendy)

c --- Range lies outside of DB file if ANY index is zero
      if(nbegx.EQ.0 .OR. nendx.EQ.0 .OR. 
     &   nbegy.EQ.0 .OR. nendy.EQ.0) goto 999

csd   processing global data
c --- Center of first DB cell is at (xorg,yorg)
      mxy = nxi*nyi
      Do j = nbegy,nendy
         yc = yorg + dyi*(j-1)
         Do i = nbegx,nendx
            xc = xorg + dxi*(i-1)

c ---       Translate GLOBAL to grid coordinates if needed
            if(LTRANS) then
              call GLOBE(iolst,caction,dusgsla,vecti,datum,vecto,
     &                   xc,yc,xdist,ydist,idum,c4dum)
            else
              xdist=xc
              ydist=yc
            endif

c ---       Drop input cells beyond the output grid (including fuzz)
            if(xdist.lt.(xorigin-fuzz).or.
     &         ydist.lt.(yorigin-fuzz).or.
     &         xdist.gt.(xorigin+dgrid*nx+fuzz).or.
     &         ydist.gt.(yorigin+dgrid*ny+fuzz))  goto 555

c ---       Read record for this cell
            kcnt = mxy -((j-1)*nxi + nxi - i + 1) + 1
            Read(ioinp,rec=kcnt)kbuf
            nrec = nrec + 1
            nread = nread + 1
            lcc = ichar(kbuf)
csd --- Check if lcc is out of valid range. If yes, flag this in the  
csd ---   list file.
            if(lcc.lt.1.or.lcc.gt.mc) then
              nmiss=nmiss+1
              if(nmiss.lt.50)then
                 if(nmiss.eq.1)then
                    write(iolst,*)
                    write(iolst,*)' Missing data messages - first ',
     1              '50 points listed'
                 endif
                 write(iolst,440)nmiss,i,j,lcc
 440             format(1x,'Point: ',i5,1x,'No valid datum at I,J = ',
     1           2(i7,1x),' LU category = ',i7)
              endif
              goto 555
            endif
csd --- End of the check
            lucat = itab(lcc)

            if(LQA) write(42,*) xdist,ydist

c ---       Loop over sub-mesh centered on (xc,yc)
            xx=xc-dxglazby2
            yy=yc-dyglazby2
            do imesh=1,meshglaz
               xmesh=xx+(FLOAT(imesh)-0.5)*dxglazmesh
               do jmesh=1,meshglaz
                  ymesh=yy+(FLOAT(jmesh)-0.5)*dyglazmesh
c ---             Translate GLOBAL to grid coordinates if needed
                  if(LTRANS) then
                    call GLOBE(iolst,caction,dusgsla,vecti,datum,vecto,
     &                         xmesh,ymesh,xgrid,ygrid,idum,c4dum)
                  else
                    xgrid=xmesh
                    ygrid=ymesh
                  endif

                  if(LQA) write(40,*) xgrid,ygrid

c ---             Update grid
                  if(lucat.GE.1 .AND. lucat.LE.92)then
                    call LUGRID(ngtmp,xgrid,ygrid,lucat,ncount2)
                  else
                    print *,'Land use catagory is out of range:',lucat
                    stop
                  endif
               enddo
            enddo
            if(ncount2.GT.ncount0) ncount=ncount+1

 555       continue
         Enddo
      Enddo

c --- Merge temporary LU grid with the full grid, with special treatment
c --- for the water category that may be used for excluded land masses
      lucatx=52
      call GMERGE(ngrid,ngtmp,lucatx)
      goto 999


c -----------------------------------
c --- New Zealand Generic Format Data
c -----------------------------------
300   continue
c --- Prepare for coordinate transformation
      call CTRANS(kf,xdum,xdum,xdum,xdum,ltrans,caction,vecti,vecto)

c --- Read each record
3     read(ioinp,*,end=999) nobs,nzlu,rnlat,relon
      nrec=nrec+1
      nread=nread+1

c --- Convert the NZ landuse class,
c --- catching '0' at edge of domain and resetting to '1'
      if(nzlu.eq.0) nzlu=1
      lucat=nzcat(nzlu)

c --- Convert Lat/Lon to grid km
      call GLOBE(iolst,caction,dnzgen,vecti,datum,vecto,
     &           relon,rnlat,xgrid,ygrid,idum,c4dum)

      if(LQA) write(43,*) xgrid,ygrid

c --- Check for valid land use
      if(lucat.GE.1 .AND. lucat.LE.92)then
        call LUGRID(ngrid,xgrid,ygrid,lucat,ncount)
      else
        print *,'Land use catagory is out of range:',lucat
        stop
      endif

c --- Loop back
      go to 3

c ----------------------
c --- NLCD92 Format Data
c ----------------------
400   continue

c     NLCD92 data: Set input data file specifics
c     First, extract the state name
      state=' '
      do k=132,1,-1
        ipos=k
        if(justname(kf)(k:k+3).eq.'NLCD' .or. 
     &     justname(kf)(k:k+3).eq.'nlcd') exit
      enddo
      ipos=ipos-2
      if(ipos.lt.4) then
        Write(*,*)'Invalid state file name for NLCD92 : ',justname(kf)
        stop
      endif
      tstate=justname(kf)
      call filcase(.true.,tstate)
      do k=1,ipos
        state(k:k)=tstate(k:k)
      enddo

c     Scan through the 52 state names to find the characteristics
      do k=1,53
        if(k.eq.53) then
          Write(*,*)'Invalid state file name for NLCD92 : ',justname(kf)
          stop
        endif
        ipos=k
        if(state.eq.statenm(k)) exit
      enddo
      nxi=ncnlcd(ipos)
      nyi=nrnlcd(ipos)
      dxi=0.03
      dyi=-0.03
      xorg=xorgnlcd(ipos)*1.e-3
      yorg=yorgnlcd(ipos)*1.e-3
      rlat=23.
      rlon=-96.
      xlat1i=29.5
      xlat2i=45.5

c --- Set 'fuzz' margin to diagonal of 1 square
      fuzz=SQRT(0.0018)

c --- Prepare for coordinate transformation
      call CTRANS(kf,rlat,rlon,xlat1i,xlat2i,ltrans,caction,vecti,vecto)

c     Calculate the relative positions of the 4 corners of output
c     domain in the  domain of NLCD data  
c --- Computes a window to assist in speeding up the processing
c --- LLC,LRC,URC,ULC
      call GET4CNR(dxi,dyi,xorg,yorg,nxi,nyi,
     &             ltrans,caction,vecti,vecto,
     &             nbegx,nendx,nbegy,nendy)


c --- Report info to list file
      write(iolst,*)
      write(iolst,*)'NLCD92 Data Info:'
      write(iolst,*)'State, ID   = ',state,ipos
      write(iolst,*)'nx,ny       = ',nxi,nyi
      write(iolst,*)'dx,dy       = ',dxi,dyi
      write(iolst,*)'x0,y0       = ',xorg,yorg
      write(iolst,*)'GET4CNR     :',ltrans,caction
      write(iolst,*)'nbegx,nendx = ',nbegx,nendx
      write(iolst,*)'nbegy,nendy = ',nbegy,nendy
      write(iolst,*)




c --- Range lies outside of DB file if ANY index is zero
      if(nbegx.EQ.0 .OR. nendx.EQ.0 .OR. 
     &   nbegy.EQ.0 .OR. nendy.EQ.0) goto 999

c --- Flip begining and ending y and y origin since NLCD is top to bottom
       if(nendy.lt.nbegy) then
         nbegyt = nendy
         nendy = nbegy 
         nbegy = nbegyt
       endif

c     processing NLCD data
c --- Center of first DB cell is at (xorg,yorg)
      mxy = nxi*nyi
      Do j = nbegy,nendy
         yc = yorg + dyi*(j-1)
         Do i = nbegx,nendx
            xc = xorg + dxi*(i-1)

c ---       Translate GLOBAL to grid coordinates if needed
            if(LTRANS) then
              call GLOBE(iolst,caction,dnlcd,vecti,datum,vecto,
     &                   xc,yc,xdist,ydist,idum,c4dum)
            else
              xdist=xc
              ydist=yc
            endif

c ---       Drop input cells beyond the output grid (including fuzz)
            if(xdist.lt.(xorigin-fuzz).or.
     &         ydist.lt.(yorigin-fuzz).or.
     &         xdist.gt.(xorigin+dgrid*nx+fuzz).or.
     &         ydist.gt.(yorigin+dgrid*ny+fuzz))  goto 556

c ---       Read record for this cell
            kcnt = (j-1)*nxi +i

            Read(ioinp,rec=kcnt) lcc1
            nrec = nrec + 1
            nread = nread + 1
            lucat=0
            if(lcc1.gt.0) lucat = nlcd92(lcc1)

            if(LQA) write(44,*) xdist,ydist

c ---       Check for valid land use
            if(lucat.GE.1 .AND. lucat.LE.92)then
              call LUGRID(ngrid,xdist,ydist,lucat,ncount)
            elseif(lcc1.NE.0) then
              print *,'NLCD Land use catagory is out of range:',lcc1
              stop
            else
              nmiss = nmiss + 1
            endif

 556       continue
         Enddo
      Enddo

      goto 999

c --- End of File
999   continue
      close(ioinp)
      write(iolst,*)
      write(iolst,*) 'Number of records read: ',nrec
      write(iolst,*) 'Number of data points read: ',nread
      write(iolst,*) 'Number of data points used to update grid: ',
     1                ncount
      write(iolst,*) 'Number of data points with missing LU: ',nmiss
      write(iolst,*)


c --- End of loop over land use data files
c ----------------------------------------
1000  continue
1010  continue


c --- Finished land use quad -- save grid
      call svgrid(ngrid)

      write(iolst,*)
      write(iolst,*) 'Land Use Processing Complete.'
      write(*,*) 'Land Use Processing Complete -- Check LIST file for',
     1           ' Run Summary and QA Warning Messages.'

      return
      end
c----------------------------------------------------------------------
      subroutine ctrans(k,rlat,rlon,xlat1i,xlat2i,ltrans,caction,
     & vecti,vecto)
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681         Level: 061013              CTRANS
c ---          D. Strimaitis, EARTH TECH, Inc.
c
c --- PURPOSE:  Sets the translation vectors for coordinate
c               transformations
c
c --- Updates
c     Ver 2.68 Level 061013 from Ver 2.65 Level 051201       DGS
c              Add LTRANS logic for NLCD DBs (LULC type=4)
c     Ver 2.65 Level 051201 from Ver 2.64 Level 040921       KAM
c              Add support for NLCD DBs, adding the matching latitudes
c              (also required change to SETTRANO)
c     Ver 2.6  Level 040921 from Ver 2.4  Level 040512       KAM
c              Consolidate calls to GLOBE1 into SETTRANO
c     Ver 2.5  Level 040512 from Ver 2.4  Level 030402       DGS
c              Add argument K for multiple land use data files
c
c --- INPUTS:
c                K - integer    - Index for current land use file
c             RLAT - real       - Ref. N.Latitude (deg) for GLOBAL & NLCD DB
c             RLON - real       - Ref. E.Longitude (deg) for GLOBAL & NLCD DB
c           XLAT1I - real       - Matching Equator-ward N.Latitude for NLCD DB
c           XLAT2I - real       - Matching Pole-ward N.Latitude for NLCD DB
c
c
c --- OUTPUT:
c           LTRANS - logical    - Coordinate translation needed?
c                                 (T:yes, F:no)
c          CACTION - char*12    - Map conversion string (e.g., UTM2LCC)
c         VECTI(9) - real*8 arr - Input Coordinate description vector
c                                 (see GLOBE1 for definition)
c         VECTO(9) - real*8 arr - Output Coordinate description vector:
c                                 (see GLOBE1 for definition)
c
c ---  CTRANS called by:  COMP
c ---  CTRANS calls:      SETTRANO
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.ctg'
c --- include commons
      include 'control.ctg'
      include 'grid.ctg'

      real*8 vecti(9),vecto(9)
      character*12 caction
      logical ltrans

c --- Set Scale Factor of Tangential TM projection (1.0)
      tmsone=1.00000

c --- Set input false Easting/Northing to zero
      feasti=0.0
      fnorti=0.0

      ltrans=.FALSE.
      if(LDATUM) then
         ltrans=.TRUE.
      elseif(lulc(k).EQ.1) then
         if(.not.LUTM) then
            ltrans=.TRUE.
         elseif(LZONE) then
            ltrans=.TRUE.
         endif
      elseif(lulc(k).EQ.2) then
         if(.not.LLAZA) then
            ltrans=.TRUE.
         elseif(rlat.NE.reflat .OR. rlon.NE.reflon) then
            ltrans=.TRUE.
         endif
      elseif(lulc(k).EQ.3) then
         ltrans=.TRUE.
      elseif(lulc(k).EQ.4) then
         if(.not.LACEA) ltrans=.TRUE.
      endif

c --- Done if no transformation is needed
      if(.not.LTRANS) return

c --- Set utm zone for coordinate calls 
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

      if(lulc(k).EQ.1) then
c --- Translation from CTG coordinates to grid coordinates
c --------------------------------------------------------
         call SETTRANO('UTM     ',ictgzone,xdum,xdum,xdum,xdum,
     &                 feasti,fnorti,
     &                 iutm,tmsone,xlat1,xlat2,reflat,reflon,
     &                 feast,fnorth,
     &                 caction,vecti,vecto)


      elseif(lulc(k).EQ.2) then
c --- Translation from GLOBAL coordinates to grid coordinates
c -----------------------------------------------------------
         call SETTRANO('LAZA    ',idum,xdum,xdum,rlat,rlon,
     &                feasti,fnorti,
     &                iutm,tmsone,xlat1,xlat2,reflat,reflon,
     &                feast,fnorth,
     &                caction,vecti,vecto)

      elseif(lulc(k).EQ.3) then
c --- Translation from NZ Gen coordinates to grid coordinates
c -----------------------------------------------------------
         call SETTRANO('LL      ',idum,xdum,xdum,xdum,xdum,
     &               feasti,fnorti,
     &               iutm,tmsone,xlat1,xlat2,reflat,reflon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

      elseif(lulc(k).EQ.4) then
c --- Translation from NLCD coordinates to grid coordinates
c -----------------------------------------------------------
         call SETTRANO('ACEA    ',idum,xlat1i,xlat2i,rlat,rlon,
     &               feasti,fnorti,
     &               iutm,tmsone,xlat1,xlat2,reflat,reflon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

      endif

      return
      end
c----------------------------------------------------------------------
      subroutine gselect(igkeep)
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681    Level: 040512                 GSELECT
c ---          D. Strimaitis, EARTH TECH, Inc.
c
c --- PURPOSE:  Tests modeling grid coordinates against USGS Global data 
c               file coverage to identify which files can be skipped
c
c --- INPUTS:
c             none
c
c --- OUTPUT:
c        IGKEEP(6) - int array  - Keep flag for each Global file
c                                 (0: Skip,    1: Keep)
c
c ---  GSELECT called by:  COMP
c ---  GSELECT calls:      GLOBE1, GLOBE
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.ctg'
c --- include commons
      include 'control.ctg'
      include 'grid.ctg'

      real xlimN(6),xlimS(6),xlimE(6),xlimW(6)
      integer igkeep(6), igselect(6,2)
      real*8 vecti(9),vecto(9)
      character*12 caction
      character*5 aname(6)

      logical ldb

      data ldb/.TRUE./

c --- Data for Global Lambert Azimuthal grid parameters
c --- Approximate NSEW limits of coverage (NLat or ELon)
c                   1 = North America
c                   2 = South America
c                   3 = Eurasia (Optimized for Europe) 
c                   4 = Eurasia (Optimized for Asia) 
c                   5 = Africa
c                   6 = Australia Pacific
c                     1       2       3       4       5       6
      data aname/'NAmer','SAmer','Europ','Asia ','Afric','AusPa' /
      data xlimN/   90.0,   13.0,   90.0,   90.0,   46.0,   22.0 /
      data xlimS/    8.5,  -60.0,    8.8,   -6.0,  -39.0,  -51.0 /
      data xlimE/  -20.0,  -32.0,   60.0,  200.0,   60.0,  175.0 /
      data xlimW/ -180.0, -100.0,  -25.0,   60.0,  -30.0,   88.0 /

c      data xlimE/  -44.0,  -32.0,   60.0,  156.0,   56.0,  175.0 /
c      data xlimW/ -154.0,  -88.0,  -22.0,   60.0,  -21.0,   88.0 /

c --- Set dummy values
      data xdum/0.0/,rdum/0.0/
      data idum/0/

c --- Set Scale Factor of Tangential TM projection (1.0)
      data tmsone/1.00000/

c --- Set output false Easting/Northing to zero
      data feasto/0.0/
      data fnorto/0.0/

c --- Translation from grid coordinates to Lat/Lon
      if(LUTM) then
c ---    Using UTM grid
c ---    Set utm zone for coordinate calls
         iutm=izone
         if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm
         call GLOBE1('UTM     ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasto,fnorto,
     &               caction,vecti,vecto)
      elseif(LLCC) then
c ---    Using Lambert Conformal grid
         call GLOBE1('LCC     ',idum,xdum,xlat1,xlat2,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasto,fnorto,
     &               caction,vecti,vecto)
      elseif(LPS) then
c ---    Using Polar Stereographic grid
         call GLOBE1('PS      ',idum,xdum,xlat1,xdum,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasto,fnorto,
     &               caction,vecti,vecto)
      elseif(LEM) then
c ---    Using Equatorial Mercator grid
         call GLOBE1('EM      ',idum,xdum,xlat1,-xlat1,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasto,fnorto,
     &               caction,vecti,vecto)
      elseif(LLAZA) then
c ---    Using Lambert Azimuthal Equal Area grid
         call GLOBE1('LAZA    ',idum,xdum,xdum,xdum,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasto,fnorto,
     &               caction,vecti,vecto)
      elseif(LTTM) then
c ---    Using Tangential TM grid
         call GLOBE1('TM      ',idum,tmsone,xdum,xdum,reflat,reflon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasto,fnorto,
     &               caction,vecti,vecto)
      endif

c --- Locate output grid corners and center in lat/lon
      xL = xorigin
      yL = yorigin
      xR = xorigin+dgrid*nx 
      yU = yorigin+dgrid*ny
      xC = 0.5*(xL+xR)
      yC = 0.5*(yU+yL)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xC,yC,xelonC,ynlatC,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xL,yL,xelonLL,ynlatLL,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xL,yU,xelonUL,ynlatUL,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xR,yU,xelonUR,ynlatUR,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xR,yL,xelonLR,ynlatLR,idum,c4dum)

c --- If domain crosses 180 deg branch, set longs of the 4
c --- corners consistent with the center
      if((xelonC-xelonLL).LT.0.0) xelonLL=xelonLL-360.
      if((xelonC-xelonUL).LT.0.0) xelonUL=xelonUL-360.
      if((xelonLR-xelonC).LT.0.0) xelonLR=xelonLR+360.
      if((xelonUR-xelonC).LT.0.0) xelonUR=xelonUR+360.
c --- Set test markers
      ynlatU=AMAX1(ynlatUL,yNlatUR)
      ynlatL=AMIN1(ynlatLL,yNlatLR)
      xelonR=AMAX1(xelonUR,xelonLR)
      xelonL=AMIN1(xelonUL,xelonLL)
c --- Reset longitudes to -180:180 range
      if(xelonR.LT.-180.) xelonR=xelonR+360.
      if(xelonR.GT.180.) xelonR=xelonR-360.
      if(xelonL.LT.-180.) xelonL=xelonL+360.
      if(xelonL.GT.180.) xelonL=xelonL-360.

c --- Test overlap with each Global file
      do i=1,6
         igselect(i,1)=0
         igselect(i,2)=0

         if(ynlatC.LE.xlimN(i) .AND. ynlatC.GE.xlimS(i)) 
     &      igselect(i,1)=igselect(i,1)+1
         if(xelonC.LE.xlimE(i) .AND. xelonC.GE.xlimW(i)) 
     &      igselect(i,1)=igselect(i,1)+1

         if(ynlatU.LE.xlimN(i) .AND. ynlatU.GE.xlimS(i)) 
     &      igselect(i,2)=igselect(i,2)+1
         if(ynlatL.LE.xlimN(i) .AND. ynlatL.GE.xlimS(i)) 
     &      igselect(i,2)=igselect(i,2)+1
         if(xelonL.LE.xlimE(i) .AND. xelonL.GT.xlimW(i)) 
     &      igselect(i,2)=igselect(i,2)+1
         if(xelonR.LE.xlimE(i) .AND. xelonR.GT.xlimW(i)) 
     &      igselect(i,2)=igselect(i,2)+1
      enddo

c --- Repeat longitude test for corners with +360 shift
      xelonL2=xelonL+360.
      xelonR2=xelonR+360.
      do i=1,6
         if(xelonL2.LE.xlimE(i) .AND. xelonL2.GT.xlimW(i)) 
     &      igselect(i,2)=igselect(i,2)+1
         if(xelonR2.LE.xlimE(i) .AND. xelonR2.GT.xlimW(i)) 
     &      igselect(i,2)=igselect(i,2)+1
      enddo

c --- Repeat longitude test for corners with -360 shift
      xelonL2=xelonL-360.
      xelonR2=xelonR-360.
      do i=1,6
         if(xelonL2.LE.xlimE(i) .AND. xelonL2.GT.xlimW(i)) 
     &      igselect(i,2)=igselect(i,2)+1
         if(xelonR2.LE.xlimE(i) .AND. xelonR2.GT.xlimW(i)) 
     &      igselect(i,2)=igselect(i,2)+1
      enddo

c --- Set flag
      ncenter3=0
      ncenter4=0

c --- Center and all 4 range markers within file:
      do i=1,6
         igkeep(i)=0
         if(igselect(i,1).EQ.2 .AND. igselect(i,2).EQ.4) then
            ncenter4=ncenter4+1
            igkeep(i)=1
         endif
      enddo

c --- Consider some overlap cases
      do i=1,6
         if(igselect(i,1).EQ.2 .OR. igselect(i,2).EQ.3) then
            ncenter3=ncenter3+1
            igkeep(i)=1
         endif
      enddo

c --- Do not keep both Europe and Asia
      if(igkeep(3).EQ.1 .AND. igkeep(4).EQ.1) igkeep(4)=0


      if(LDB) then

      write(iolst,*)
      write(iolst,*)
      write(iolst,*)'NCENTER3,NCENTER4: ',ncenter3,ncenter4
      do k=1,6
         write(iolst,*) 'File:',aname(k),' KEEP:',igkeep(k)
      enddo
      write(iolst,*)
      write(iolst,*)
      write(iolst,*)'C  Elon,Nlat: ',xelonC,ynlatC
      write(iolst,*)'LL Elon,Nlat: ',xelonLL,ynlatLL
      write(iolst,*)'UL Elon,Nlat: ',xelonUL,ynlatUL
      write(iolst,*)'UR Elon,Nlat: ',xelonUR,ynlatUR
      write(iolst,*)'LR Elon,Nlat: ',xelonLR,ynlatLR
      write(iolst,*)
      write(iolst,*)
      write(iolst,*)'Upper  Nlat: ',ynlatU
      write(iolst,*)'Lower  Nlat: ',ynlatL
      write(iolst,*)'Left   Elon: ',xelonL
      write(iolst,*)'Right  Elon: ',xelonR
      write(iolst,*)
      write(iolst,*)
      do k=1,6
         write(iolst,*) 'File:',aname(k),' Center:',igselect(k,1)
      enddo
      write(iolst,*)
      write(iolst,*)
      do k=1,6
         write(iolst,*) 'File:',aname(k),' Range:',igselect(k,2)
      enddo

      endif


      return
      end
c----------------------------------------------------------------------
      subroutine lugrid(ngrid,x,y,ilu,ncount)
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681        Level: 050128               LUGRID
c ---          E. Insley, EARTH TECH, Inc.
c ---          J. Scire - updated (1/2000) to fix bug with LCC 
c                         projection
c
c --- PURPOSE:  Updates the count for the land use category for the grid
c
c --- Updates
c --- Ver 2.62 Level 041215 to Ver 2.63 Level 050128 DGS
c              Disable IDEFTYP and introduce LMARSWAP and IOCEAN
c --- Ver 2.6 Level 040921 to Ver 2.62 Level 041215 DGS
c              Move the cell index (i,j) calculation to the top of the
c              subroutine so that points outside of the domain are
c              rejected at once.
c --- Ver 2.4  Level 030402 to Ver 2.6 Level 040921 KAM
c              If coastline processing invoked, check points to either:
c               - classify as ocean if point is in ocean
c               - reclassify as default type if point is misclassified
c                 as ocean, or skip point
c     Ver 2.4  Level 011003 to 030402       DGS
c              Remove all coordinate transformations
c              Remove loop over repeated CTG entries in 'row'
c     Ver 2.0  Level 011003 from Ver 1.2  Level 000112       DGS
c              Use new /GRID/, /LUTABS/, and /CONTROL/ commons
c
c --- INPUTS:
c       NGRID(mxcat,mxnx,mxny) - integer  - Count array for each LU for
c                                  array      each grid
c                     X  - real     - X Coordinate (km) of LU grid cell
c                     Y  - real     - Y Coordinate (km) of LU grid cell
c                   ILU  - integer  - Land Use category for LU grid cell
c                NCOUNT  - integer  - Number of LU cells which 'hit'
c
c         Parameters: MXCAT,MXNX,MXNY,iolst
c
c --- OUTPUT:
c       NGRID(mxcat,mxnx,mxny) - integer  - Updated count array for each
c                                 array      LU for each grid
c            NCOUNT  - integer - Updated number of CTG cells which 'hit'
c
c ---  LUGRID called by:  COMP
c ---  LUGRID calls:      none
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.ctg'
c --- include commons
      include 'control.ctg'
      include 'grid.ctg'
      include 'lutabs.ctg'

      integer ngrid(mxcat,mxnx,mxny)
      logical lok

c --- Copy ilu to local variable
      ilul=ilu
        
c --- QA land use index
      land = lumap(ilul)
      if(land.EQ.0)then
        write(iolst,20) land,x,y
20      format(1x,'BAD Mapped Land Use Category: LAND = ',i4,
     1         ' at (x,y)=', 2f12.3)
        write(*,*) 'ERROR in SUBR. LUGRID  --  See Run LIST File'
        stop
      endif

c --- Determine modeling grid index of CTG coordinate
c-emi Fixed method of determining grid index because integer truncation
c-emi returns 0 for -1.0<real<1.0 which caused CTG coords just outside
c-emi the grid to be counted as though they were in. (8/26/97)
c-emi i = (x-xorigin)/dgrid + 1
c-emi j = (y-yorigin)/dgrid + 1
      delx = x-xorigin
      dely = y-yorigin
      i = delx/dgrid
      j = dely/dgrid
      if(delx.GE.0.0) i = i + 1
      if(dely.GE.0.0) j = j + 1

c --- In the modeling grid?
      lok=.FALSE.
      if(i.GE.1 .AND. i.LE.nx .AND. j.GE.1 .AND. j.LE.ny) lok=.TRUE.

c --- Done if the point is not in the grid
      if(.not.LOK) return

c --- If coastal processing is invoked, check if the point is marine
c --- or is incorrectly classed as marine.  If the latter, either
c --- reclassify according to the default IDEFTYP, or return
c --- without counting the point since true land use cannot be
c --- determined
      if(lcoast.AND.lmarswap) then
        if(iptype(x,y).eq.0) then
          if(ilu.ne.54) ilul=iocean
        else
c ---      IDEFTYP disabled
c          if(ilu.eq.55) then
c            if(ideftyp.eq.0) then
c              return
c            else
c              ilul=ideftyp*10+2
c            endif
c          endif
        endif
      endif
        
c --- Map LU
      land = lumap(ilul)
      if(land.EQ.0)then
        write(iolst,20) land,x,y
        write(*,*) 'ERROR in SUBR. LUGRID  --  See Run LIST File'
        stop
      endif
      ngrid(land,i,j) = ngrid(land,i,j) + 1
      ncount = ncount + 1

      return
      end
c----------------------------------------------------------------------
      subroutine gmerge(ngrid,ngtmp,ilu)
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681     Level: 040512                  GMERGE
c ---          D. Strimaitis, EARTH TECH, Inc.
c
c --- PURPOSE:  Merges the gridded land use from the current DB file
c               with the accumulated gridded land use.  Because the
c               USGS Global DB files are packaged by continent and
c               replace bordering land masses with water, water-only
c               cell counts are not added to the accumulation grid
c               if any non-water counts for the cell have already
c               been logged.  Similarly, accumulated water-only cells
c               in the accumulation grid are totally replaced if the
c               current grid cell contains some land.
c
c --- INPUTS:
c     NGRID(mxcat,mxnx,mxny) - int array - Count array for each LU for
c                                          each grid cell (accumulated)
c     NGTMP(mxcat,mxnx,mxny) - int array - Count array for each LU for
c                                          each grid cell (current)
c                       ILU  - integer   - Land Use category for water
c
c         Parameters: MXCAT,MXNX,MXNY,iolst
c
c --- OUTPUT:
c     NGRID(mxcat,mxnx,mxny) - int array - Count array for each LU for
c                                          each grid cell (accumulated)
c
c ---  GMERGE called by:  COMP
c ---  GMERGE calls:      none
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.ctg'
c --- include commons
      include 'control.ctg'
      include 'grid.ctg'
      include 'lutabs.ctg'

      integer ngrid(mxcat,mxnx,mxny)
      integer ngtmp(mxcat,mxnx,mxny)

c --- Map LU
      iwater = lumap(ilu)

c --- Loop over cells
      do j=1,ny
      do i=1,nx

c ---    Sum counts for all land use and just for water
         nallgrd=0
         nwatgrd=0
         nalltmp=0
         nwattmp=0
         do k=1,mxcat
            nallgrd=nallgrd+ngrid(k,i,j)
            nalltmp=nalltmp+ngtmp(k,i,j)
            if(k.EQ.iwater) then
               nwatgrd=nwatgrd+ngrid(k,i,j)
               nwattmp=nwattmp+ngtmp(k,i,j)
            endif
         enddo
c ---    Set action switch (kact)
c ---      0: Ignore current cell
c ---      1: Add current cell to accumulation
c ---      2: Replace accumulation with current cell
         if(nallgrd.GT.nwatgrd) then
c ---       Accumulation grid cell has some non-water
            if(nalltmp.GT.nwattmp) then
c ---          Current grid cell has some non-water (add)
               kact=1
            else
c ---          Current grid cell is ALL water (drop)
               kact=0
            endif
         else
c ---       Accumulation grid cell is ALL water
            if(nalltmp.GT.nwattmp) then
c ---          Current grid cell has some non-water (replace)
               kact=2
            else
c ---          Current grid cell is ALL water (add)
               kact=1
            endif
         endif

c ---    Perform action
         if(kact.EQ.1) then
c ---       Add
            do k=1,mxcat
               ngrid(k,i,j)=ngrid(k,i,j)+ngtmp(k,i,j)
            enddo
         elseif(kact.EQ.2) then
c ---       Replace
            do k=1,mxcat
               ngrid(k,i,j)=ngtmp(k,i,j)
            enddo
         endif

      enddo
      enddo

      return
      end
c-----------------------------------------------------------------------
      subroutine get4cnr(dxi,dyi,xorg,yorg,nxi,nyi,
     &                   ltrans,caction,vecti,vecto,
     &                   nbegx,nendx,nbegy,nendy)
c-----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681     Level: 051201                 GET4CNR
c ---          Shuming Du, EARTH TECH, Inc.
c
c --- PURPOSE:  Calculate locations of the 4 corners of output domain 
c               within the USGS Global data set
c
c --- NOTE:    This routine uses the following conventions:
c                 Latitude  - Northern Hemisphere - positive
c                             Southern Hemisphere - negative
c                 Longitude - Eastern Hemisphere  - positive
c                             Western Hemisphere  - negative
c
c --- Updates
c     Ver 2.65 Level 051201 from Ver 2.64 Level 050128       KAM
c              Modify to support NLCD as well as GLOBAL datasets
c     Ver 2.63 Level 050128 from Ver 2.5 Level 040512        DGS
c              Remove LPROC argument (detecting case where there
c              is no overlap in the modeling grid and the database
c              file never implemented).
c     Ver 2.5 Level 040512 from Ver 2.4  Level 030402        DGS
c              Modified to limit computed beginning/ending
c              GLOBAL DB ind. to actual range.  Without this mod the
c              random access counter can point outside the range,
c              generating a runtime error.
c              Modified to detect case where there is no overlap in
c              the modeling grid and the database file.
c              Modified to add a buffer to the computed range
c              that is 10% of the modeling domain scale rather
c              than 200 cells (e.g., 200 km).
c     Ver 2.4  Level 030402 from Ver 2.0  Level 011003       DGS
c              Implement new COORDS with DATUMs
c     Ver 2.0  Level 011003 from Ver 1.2  Level 000112       DGS
c              Use new /GRID/ and /CONTROL/ commons
c
c --- INPUTS:
c
c       DXI   - real     - Grid size (in X direction) of the input image
c                          file (km)
c       DYI   - real     - Grid size (in Y direction) of the input image
c                          file (km)
c       XORG  - real     - X coordinate (Lambert Azimuthal projection) 
c                          of the lower-left corner of the input image
c                          file (km)
c       YORG  - real     - Y coordinate (Lambert Azimuthal projection) 
c                          of the lower-left corner of the input image
c                          file (km)
c       NXI   - integer  - Number of grid cells (in X direction) in the 
c                          input image file
c       NYI   - integer  - Number of grid cells (in Y direction) in the 
c                          input image file
c      LTRANS - logical   - Coordinate translation needed?
c                           (T:yes, F:no)
c     CACTION - char*12   - Map conversion string (e.g., UTM2LCC)
c    VECTI(9) - real*8 arr - Input Coordinate description vector
c                            (see GLOBE1 for definition)
c    VECTO(9) - real*8 arr - Output Coordinate description vector:
c                            (see GLOBE1 for definition)
c
c      Common/CONTROL/ variables:
c          LSOHEM, LLCC
c      Common/GRID/ variables:
c          NX, NY, DGRID, XORIGIN, YORIGIN, IZONE,
c          REFLAT, REFLON, XLAT1, XLAT2
c
c --- OUTPUTS:
c
c       NBEGX - integer  - Location (column) of the lower-left corner
c                          of output domain in the input image file.
c       NENDX - integer  - Location (column) of the upper-right corner
c                          of output domain in the input image file.
c       NBEGY - integer  - Location (row) of the lower-left corner of 
c                            output domain in the input image file.
c       NENDY - integer  - Location (row) of the upper-right corner of 
c                            output domain in the input image file.
c
c ---  GET4CNR called by:  COMP
c ---  GET4CNR calls:      GLOBE
c----------------------------------------------------------------------
c --- Include parameters
      include 'params.ctg'
c --- Include commons
      include 'control.ctg'
      include 'grid.ctg'

      logical ltrans,ldb

c --- FORWARD coordinate transformation variables
      real*8 vecti(9),vecto(9)
      character*12 caction
c --- REVERSE coordinate transformation variables
      real*8 rvecti(9),rvecto(9)
      character*12 rcaction

      character*8 ddatain
      real xkm(4),ykm(4)

      ldb=.FALSE.

      if(dyi.gt.0.) then
        ddatain=dusgsla
      else
        ddatain=dnlcd
      endif

c --- Set coordinates of lower-left & upper-right corners of the
c --- output grid (km)
      xorgo = xorigin
      yorgo = yorigin
      xorge = xorigin+dgrid*nx 
      yorge = yorigin+dgrid*ny

      if(LTRANS) then
c ---   Establish the transformation variables to go from the OUTPUT
c ---   grid to the INPUT grid (i.e., the reverse direction)
        do i=1,9
          rvecti(i)=vecto(i)
          rvecto(i)=vecti(i)
        enddo
c ---   Reverse the action string
        rcaction=caction
        do i=1,12
          if(caction(i:i).EQ.'2') i2=i
          if(caction(i:i).NE.' ') ilast=i
        enddo
        nfrom=i2-1
        nto=ilast-i2
        j2=nto+1
        rcaction(j2:j2)='2'
        do j=1,nto
          i=i2+j
          rcaction(j:j)=caction(i:i)
        enddo
        do i=1,nfrom
          j=j2+i
          rcaction(j:j)=caction(i:i)
        enddo

        if(LDB) then
           write(*,*)'GET4CNR: caction,rcaction = ',caction,rcaction
           write(*,*)'  VECTI = RVECTO?  '
           do k=1,9
              write(*,*)  VECTI(k),RVECTO(k)
           enddo
           write(*,*)'  VECTO = RVECTI?  '
           do k=1,9
              write(*,*)  VECTO(k),RVECTI(k)
           enddo
           write(*,*)'Reverse GLOBE call going from datum ',datum
           write(*,*)'                           to datum ',dusgsla
        endif

c ---   Translate from output grid to GLOBAL grid
        call GLOBE(iolst,rcaction,datum,rvecti,ddatain,rvecto,
     &             xorgo,yorgo,xkm(1),ykm(1),idum,c4dum)
        call GLOBE(iolst,rcaction,datum,rvecti,ddatain,rvecto,
     &             xorge,yorgo,xkm(2),ykm(2),idum,c4dum)
        call GLOBE(iolst,rcaction,datum,rvecti,ddatain,rvecto,
     &             xorge,yorge,xkm(3),ykm(3),idum,c4dum)
        call GLOBE(iolst,rcaction,datum,rvecti,ddatain,rvecto,
     &             xorgo,yorge,xkm(4),ykm(4),idum,c4dum)
      else
        xkm(1)=xorgo
        ykm(1)=yorgo
        xkm(2)=xorge
        ykm(2)=yorgo
        xkm(3)=xorge
        ykm(3)=yorge
        xkm(4)=xorgo
        ykm(4)=yorge
      endif

c --- Write the corner points to the list file
      write(iolst,*)
      write(iolst,*)' Coordinates of corners'
      write(iolst,31)'SW',xorgo,yorgo,xkm(1),ykm(1)
      write(iolst,31)'SE',xorge,yorgo,xkm(2),ykm(2)
      write(iolst,31)'NE',xorge,yorge,xkm(3),ykm(3)
      write(iolst,31)'NW',xorgo,yorge,xkm(4),ykm(4)
31    format(3x,a2,' Grid Corner: X,Y = ',f12.5,1x,f12.5,
     1             '    DATABASE X,Y = ',f12.5,1x,f12.5)

c --- Set index range for x, y
      nbegx = 0
      nbegy = 0
      nendx = 0
      nendy = 0

      do i=1,4
         ix=INT((xkm(i)-xorg)/dxi) + 1
         if(nbegx.EQ.0 .OR. nbegx.GT.ix) nbegx=ix
         if(nendx.EQ.0 .OR. nendx.LT.ix) nendx=ix
         iy=INT((ykm(i)-yorg)/dyi) + 1
         if(nbegy.EQ.0 .OR. nbegy.GT.iy) nbegy=iy
         if(nendy.EQ.0 .OR. nendy.LT.iy) nendy=iy
      enddo

c --- Apply 10% buffer to range
      nbuffx=(nendx-nbegx)/10 + 1
      nbuffy=(nendy-nbegy)/10 + 1
      nbuff=MAX(nbuffx,nbuffy)
      nbegx=nbegx-nbuff
      nendx=nendx+nbuff
      nbegy=nbegy-nbuff
      nendy=nendy+nbuff

c --- Impose absolute limits of the DB on these computed limits
      if(nbegx.GT.nxi .OR. nendx.LT.1) then
         nbegx=0
         nendx=0
      else
         nbegx=MAX(1,nbegx)
         nendx=MIN(nxi,nendx)
      endif
      if(nbegy.GT.nyi .OR. nendy.LT.1) then
         nbegy=0
         nendy=0
      else
         nbegy=MAX(1,nbegy)
         nendy=MIN(nyi,nendy)
      endif

      return   
      end
c----------------------------------------------------------------------
      subroutine rdgrid(ngrid,nx,ny)
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681      Level: 041215                 RDGRID
c ---          E. Insley, EARTH TECH, Inc.
c
c --- PURPOSE:  Reads the gridded land use count data from a previous
c               run of CTGPROC.
c
c --- Updates
c     Ver 2.62 Level 041215 from Ver 2.4  Level 030402       DGS
c              Make format for LU.DAT records respond to MXCAT
c              in the range 1 to 999.
c     Ver 2.4  Level 030402 from Ver 1.2  Level 010206       DGS
c              Add call to READHD for new header record structure
c     Ver 1.2  Level 010206 from Ver 1.0  Level 961113       DGS
c              Increment the read format for 38 LU categories
c              Read over header records that identify the data file
c              (QA to be implemented)
c
c --- INPUTS:
c                    NX  - integer  - Number of grid cells: X direction
c                    NY  - integer  - Number of grid cells: Y direction
c
c        Parameters: MXCAT,MXNX,MXNY,ioprev,iolst
c
c --- OUTPUT:
c       NGRID(mxcat,mxnx,mxny) - integer  - Count array for each LU for
c                                  array      each grid
c
c ---  RDGRID called by:  COMP
c ---  RDGRID calls:      READHD
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.ctg'

      character*18 fmtc
      integer ngrid(mxcat,mxnx,mxny)
      nrd=0

c --- Construct format for input records (consistent with MXCAT)
      i=0
      if(mxcat.LT.1000) i=3
      if(mxcat.LT.100) i=2
      if(mxcat.LT.10) i=1
      if(mxcat.LT.1) i=0
      if(i.EQ.3) then
         fmtc='(2i5,nnni5)   '
         write(fmtc(6:8),'(i3)') mxcat
      elseif(i.EQ.2) then
         fmtc='(2i5,nni5)    '
         write(fmtc(6:7),'(i2)') mxcat
      elseif(i.EQ.1) then
         fmtc='(2i5,ni5)     '
         write(fmtc(6:6),'(i1)') mxcat
      else
         write(*,*)'FATAL Error in RDGRID - MXCAT out of range'
         write(*,*)'Expected MXCAT between 1 and 999'
         write(*,*)'Found    MXCAT = ',mxcat
         stop
      endif

c --- Read & QA header records
      call READHD(ioprev)

c --- Read Data
1     continue
      read(ioprev,fmtc,end=999) i,j,(ngrid(k,i,j),k=1,mxcat)
      nrd = nrd + 1
c --- QA on grid indices
      if(i.LT.1 .OR. i.GT.nx .OR. j.LT.1 .OR. j.GT.ny)then
        write(iolst,*) 'ERROR reading previous data file at record: ',
     &                  nrd
        write(iolst,*) 'I index = ', i,' J index = ',j
        write(*,*) 'ERROR in SUBR. RDGRID  --  See Run LIST File'
        stop
      endif
      goto 1

c --- End of File
999   continue
      write(iolst,*)
      write(iolst,*) 'Previous grid data file processed without error.'
      write(iolst,*)
      write(iolst,*)

      return
      end
c-----------------------------------------------------------------------
      subroutine readhd(io)
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682          Level: 030429            READHD
c               D. Strimaitis, Earth Tech
c
c PURPOSE:     READHD reads the header records of an input data file
c              and screens coordinate and grid parameters
c
c --- Updates
c     Ver 2.41, Level 030429 from Ver 2.4 Level 030402   D. Strimaitis          
c               - Add read for false easting/northing for LCC,LAZA,TTM
c               - Fix reference lat/lon variable names for QA tests
c
c ARGUMENTS:
c    PASSED:  /CONTROL/   logicals
c             /GRID/      data
c
c  RETURNED:  none
c
c CALLING ROUTINES:   RDGRID
c
c EXTERNAL ROUTINES:  ALLCAP, XTRACTLL, LRSAME
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.ctg'
      include 'params.cal'
      include 'control.ctg'
      include 'grid.ctg'

c --- Local Variables
      character*4 xyunitin,utmhemin
      character*8 lutype,pmapin,datumin,datenin
      character*16 dataset,dataver,blank16
      character*16 clat0in,clon0in,clat1in,clat2in
      character*64 datamod
      character*80 comment1

      logical lutmin,llccin,lpsin,lemin,llazain,lttmin
      logical llu
      logical LRSAME
      logical lerror

      data nlim/1/
      data blank16/'                '/

      lutmin =.FALSE.
      llccin =.FALSE.
      lpsin  =.FALSE.
      lemin  =.FALSE.
      llazain=.FALSE.
      lttmin =.FALSE.

      llu =.FALSE.

      lerror=.FALSE.

      clat0in=blank16
      clon0in=blank16
      clat1in=blank16
      clat2in=blank16


c --- Read header information
c ---------------------------

c --- Dataset, Version, Modifier
      read(io,'(2a16,a64)') dataset,dataver,datamod
c --- Convert Dataset to upper case
      do i=1,16
         call ALLCAP(dataset(i:i),nlim)
      enddo

c --- Identify dataset type
      if(dataset.EQ.'LU.DAT') then
         llu=.TRUE.
      else
c ---    FATAL ERROR
         write(iolst,*)
         write(iolst,*)'RDHEAD: Invalid input file DATASET type: ',
     &                  dataset
         write(iolst,*)'        Expected LU.DAT'
         lerror=.TRUE.
         goto 999
      endif

c --- Number of comment records
      read(io,*) ncomment
c --- Comments (optional/repeatable)
      do k=1,ncomment
         read(io,'(a80)') comment1
      enddo

c --- LU File Type
      read(io,'(a8)') lutype
      do i=1,8
         call ALLCAP(lutype(i:i),nlim)
      enddo

c --- Map projection
      read(io,'(a8)') pmapin
      do i=1,8
         call ALLCAP(pmapin(i:i),nlim)
      enddo

      if(pmapin.EQ.'UTM     ')  lutmin =.TRUE.
      if(pmapin.EQ.'LCC     ')  llccin =.TRUE.
      if(pmapin.EQ.'PS      ')  lpsin  =.TRUE.
      if(pmapin.EQ.'EM      ')  lemin  =.TRUE.
      if(pmapin.EQ.'LAZA    ')  llazain=.TRUE.
      if(pmapin.EQ.'TTM     ')  lttmin =.TRUE.

c --- Map projection parameters
      if(LUTMIN) then
         read(io,'(i4,a4)') izonein,utmhemin
      elseif(LLCCIN) then
         read(io,'(4a16)') clat0in,clon0in,clat1in,clat2in
      elseif(LPSIN) then
         read(io,'(3a16)') clat0in,clon0in,clat1in
      elseif(LEMIN.or.LLAZAIN.or.LTTMIN) then
         read(io,'(2a16)') clat0in,clon0in
      endif
c --- Map false Easting/Northing
      if(LLCCIN.or.LLAZAIN.or.LTTMIN) then
         read(io,*) feastin,fnorthin
      else
         feastin=feast
         fnorthin=fnorth
      endif
c --- Map DATUM
      read(io,'(a8,a12)') datumin,datenin
      do i=1,8
         call ALLCAP(datumin(i:i),nlim)
      enddo

c --- Grid
      read(io,*) nxin,nyin,xoin,yoin,dgridin,dgridin,
     &           mxcatin
c --- XYUNIT
      read(io,'(a4)') xyunitin
      do i=1,4
         call ALLCAP(xyunitin(i:i),nlim)
      enddo

c --- QA header information
c -------------------------

c --- Grid information
      if(nx.NE.nxin.OR.ny.NE.nyin) then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         Number of grid cells does not match'
         write(iolst,*)'         Input file NX,NY  : ',nxin,nyin
         write(iolst,*)'         Control file NX,NY: ',nx,ny
         lerror=.TRUE.
      endif
      if(xyunitin.NE.'KM  ') then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         XY units must be KM'
         write(iolst,*)'         Input file: ',xyunitin
         lerror=.TRUE.
      endif
      if(.not.LRSAME(0.0001,dgridin,dgrid))then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         Grid cell size does not match'
         write(iolst,*)'         Input file  : ',dgridin
         write(iolst,*)'         Control file: ',dgrid
         lerror=.TRUE.
      endif
      if((.not.LRSAME(0.0001,xoin,xorigin)).or.
     1   (.not.LRSAME(0.0001,yoin,yorigin)))then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         SW corner coordinates do not match'
         write(iolst,*)'         Input file xkm,ykm  : ',xoin,yoin
         write(iolst,*)'         Control file xkm,ykm: ',xorigin,yorigin
         lerror=.TRUE.
      endif

c --- Land Use Checks
c --- Check for a match of the number of (input) land use categories
      if(LLU .AND. mxcatin.NE.mxcat) then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         Number of land use categories: ',
     &                           mxcatin
         write(iolst,*)'         Control file categories      : ',mxcat
         lerror=.TRUE.
      endif
      if(LLU .AND. lutype.NE.'HITS')then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         Incorrect dataset type: ',lutype
         write(iolst,*)'         Type required         : HITS'
         lerror=.TRUE.
      endif

c --- Projection checks
      if(pmap.NE.pmapin) then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         Map projection PMAP does not match'
         write(iolst,*)'         Input file  : ',pmapin
         write(iolst,*)'         Control file: ',pmap
         lerror=.TRUE.
      endif
      if(LUTMIN .AND. LUTM) then
         if(izonein.NE.izone)then
            write(iolst,*)
            write(iolst,*)'RDHEAD:  Problem in input file type: ',
     &                              dataset
            write(iolst,*)'         UTM zone does not match'
            write(iolst,*)'         Input file  : ',izonein
            write(iolst,*)'         Control file: ',izone
            lerror=.TRUE.
         endif
         if(utmhemin.NE.utmhem)then
            write(iolst,*)
            write(iolst,*)'RDHEAD:  Problem in input file type: ',
     &                              dataset
            write(iolst,*)'         UTM Hemisphere does not match'
            write(iolst,*)'         Input file  : ',utmhemin
            write(iolst,*)'         Control file: ',utmhem
            lerror=.TRUE.
         endif
      endif

c --- Check lat/lon variables
      if(clat0in(1:1).NE.' ') then
        call XTRACTLL(iolst,'LAT ',clat0in,reflatin)
        if(.not.LRSAME(0.0001,reflatin,reflat))then
          write(iolst,*)
          write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
          write(iolst,*)'         REFLAT does not match'
          write(iolst,*)'         Input file  : ',reflatin
          write(iolst,*)'         Control file: ',reflat
          lerror=.TRUE.
        endif
      endif
      if(clon0in(1:1).NE.' ') then
        call XTRACTLL(iolst,'LON ',clon0in,reflonin)
        if(.not.LRSAME(0.0001,reflonin,reflon))then
          write(iolst,*)
          write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
          write(iolst,*)'         REFLON does not match'
          write(iolst,*)'         Input file  : ',reflonin
          write(iolst,*)'         Control file: ',reflon
          lerror=.TRUE.
        endif
      endif
      if(clat1in(1:1).NE.' ') then
        call XTRACTLL(iolst,'LAT ',clat1in,xlat1in)
        if(.not.LRSAME(0.0001,xlat1in,xlat1))then
          write(iolst,*)
          write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
          write(iolst,*)'         XLAT1 does not match'
          write(iolst,*)'         Input file  : ',xlat1in
          write(iolst,*)'         Control file: ',xlat1
          lerror=.TRUE.
        endif
      endif
      if(clat2in(1:1).NE.' ') then
        call XTRACTLL(iolst,'LAT ',clat2in,xlat2in)
        if(.not.LRSAME(0.0001,xlat2in,xlat2))then
          write(iolst,*)
          write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
          write(iolst,*)'         XLAT2 does not match'
          write(iolst,*)'         Input file  : ',xlat2in
          write(iolst,*)'         Control file: ',xlat2
          lerror=.TRUE.
        endif
      endif

c --- Check false Easting/Northing
      if(.not.LRSAME(0.0001,feastin,feast))then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         False Easting does not match'
         write(iolst,*)'         Input file  : ',feastin
         write(iolst,*)'         Control file: ',feast
         lerror=.TRUE.
      endif
      if(.not.LRSAME(0.0001,fnorthin,fnorth))then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         False Northing does not match'
         write(iolst,*)'         Input file  : ',fnorthin
         write(iolst,*)'         Control file: ',fnorth
         lerror=.TRUE.
      endif

c --- DATUM
      if(datumin.NE.datum)then
         write(iolst,*)
         write(iolst,*)'RDHEAD:  Problem in input file type: ',dataset
         write(iolst,*)'         DATUM does not match'
         write(iolst,*)'         Input file  : ',datumin
         write(iolst,*)'         Control file: ',datum
         lerror=.TRUE.
      endif



c --- STOP now if error exists in the input file
999   if(LERROR) then
         write(*,*)'ERRORS are found in an input file'
         write(*,*)'Review messages written to the LIST file'
         stop
      endif

      return
      end
c----------------------------------------------------------------------
      logical function lrsame(r0,r1,r2)
c----------------------------------------------------------------------
c
c --- CTGPROC    Version: 2.681      Level: 030402               LRSAME
c                D. Strimaitis,    Earth Tech
c ---            From CALPOST V5.2, L991104c
c
c --- PURPOSE:  Compare 2 real numbers (r1,r2) to determine if their
c               fractional difference exceeds r0
c
c --- INPUTS:
c            r0 - real       - Fractional difference allowed
c            r1 - real       - Real value 1
c            r2 - real       - Real value 2
c
c
c --- OUTPUT:
c        lrsame - logical    - Key indicating result of test
c                              .TRUE.  -- values are 'same'
c                              .FALSE. -- values are NOT 'same'
c
c
c --- LRSAME called by:   READHD
c --- LRSAME calls:       none
c----------------------------------------------------------------------
c
      data half/0.5/

      lrsame=.TRUE.

c --- Direct comparison
      if(r1.EQ.r2) return

      rdif=ABS(r1-r2)
      ravg=half*ABS(r1+r2)

      if(rdif.GE.ravg) then
c ---    Fractional difference greater than one!
         lrsame=.FALSE.
      else
         ftest=rdif/ravg
         if(ftest.GT.r0) lrsame=.FALSE.
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine svgrid(ngrid)
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681       Level: 050128                SVGRID
c ---          E. Insley, EARTH TECH, Inc.
c
c --- PURPOSE:  Writes the gridded land use count data to an ASCII
c               output file. Also does some QA checks of the data
c               by comparing the total number of CTG land use cell
c               hits in each grid cell of the user-specified modeling
c               domain.  A table is written in the CTGPROC.LST file
c               containing the number of 'hits' for each grid cell
c               and a warning message is written to alert the
c               user to investigate grid cells with the number of
c               'hits' well below the average number for their grid.
c               This could help the user identify missing data areas.
c
c --- Updates
c     Ver 2.63 Level 050128 from Ver 2.62 Level 041215        DGS
c              Configure coastline replacement option using LMARFILL
c              and IOCEAN inputs.
c     Ver 2.62 Level 041215 from Ver 2.6 Level 040921        DGS
c              Add a column to the right end of the LU.DAT output file
c              that flags cells for which land use remains missing, or
c              was changed from missing to ocean based on coastline.
c              Make output format respond to MXCAT(1 to 999).
c     Ver 2.6 Level 040921 from Ver 2.5 Level 040512         KAM
c              If coastline processing invoked, for the final pass
c              check centers of empty cells to see if they can be
c              classified as ocean
c     Ver 2.5  Level 040512 from Ver 2.4  Level 030402       DGS
c              Revise warning message at end of run
c     Ver 2.4  Level 030402 from Ver 2.0  Level 011003       DGS
c              Revise header of data file, and consolidate
c              in WRTHEAD
c     Ver 2.0  Level 011003 from Ver 1.2  Level 010206       DGS
c              Use new /CONTROL/ & /GRID/ commons
c     Ver 1.2  Level 010206 from Ver 1.0  Level 990130       DGS
c              Add header records that identify the data file
c              properties -- these changes require the use of
c              the corresponding MAKEGEO!
c     Ver 1.0  Level 990130 from Ver 1.0 960901              DGS
c              Condense warning messages for cells, and compute number
c              of significant digits to use in grid output to list file
c
c --- INPUTS:
c       NGRID(mxcat,mxnx,mxny) - integer  - Count array for each LU for
c                                  array      each grid
c
c        Parameters: MXCAT,MXNX,MXNY,ioout,iolst
c
c --- OUTPUT:  None
c
c ---  SVGRID called by:  COMP
c ---  SVGRID calls:      WRTHEAD, OUT
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.ctg'
c --- include commons
      include 'control.ctg'
      include 'grid.ctg'
      include 'lutabs.ctg'

      integer ngrid(mxcat,mxnx,mxny), isumgrd(mxnx,mxny)
      real xdum(mxnx,mxny),zero(mxcat)
      logical ldate
      character*18 fmtf,fmtc
      character*20 fmiss,fgood,focean
      character*70 messag

      data zero/mxcat*0.0/
      data nzero/0/, nlow/0/, nocean/0/

c --- Set flags for identifying missing LU cells
      data fmiss /'   Missing          '/
      data fgood /'   .                '/
      data focean/'   Filled as Ocean  '/

c --- Construct format for output records (consistent with MXCAT)
c --- Output MXCAT LU categories plus a column for the Missing flag
      i=0
      if(mxcat.LT.1000) i=3
      if(mxcat.LT.100) i=2
      if(mxcat.LT.10) i=1
      if(mxcat.LT.1) i=0
      if(i.EQ.3) then
         fmtf='(2i5,nnnf6.3,a20) '
         fmtc='(2i5,nnni5,a20)   '
         write(fmtf(6:8),'(i3)') mxcat
         write(fmtc(6:8),'(i3)') mxcat
      elseif(i.EQ.2) then
         fmtf='(2i5,nnf6.3,a20)  '
         fmtc='(2i5,nni5,a20)    '
         write(fmtf(6:7),'(i2)') mxcat
         write(fmtc(6:7),'(i2)') mxcat
      elseif(i.EQ.1) then
         fmtf='(2i5,nf6.3,a20)   '
         fmtc='(2i5,ni5,a20)     '
         write(fmtf(6:6),'(i1)') mxcat
         write(fmtc(6:6),'(i1)') mxcat
      else
         write(*,*)'FATAL Error in SVGRID - MXCAT out of range'
         write(*,*)'Expected MXCAT between 1 and 999'
         write(*,*)'Found    MXCAT = ',mxcat
         stop
      endif

c --- get the index for ocean
      kocean = lumap(iocean)

c --- Prepare and write header records
      call WRTHEAD
c
c --- Initialize sums
      isumall = 0
      do 1 j=1,ny
      do 1 i=1,nx
        isumgrd(i,j) = 0
1     continue
c
c --- Sum the number of 'hits' and write the output file
      isummax = 0
      do 2 j=1,ny
      do 2 i=1,nx
        isum = 0
        do 3 k=1,mxcat
          isum = isum + ngrid(k,i,j)
          isumall = isumall + ngrid(k,i,j)
          isumgrd(i,j) = isumgrd(i,j) + ngrid(k,i,j)
          isummax = MAX(isum,isummax)
3       continue

c ---   If it is the last run, write out data in terms of fractional
c ---   land use based on the number of 'hits', otherwise if the data
c ---   will be used again in a subsequent run then write the number
c ---   of 'hits'
c ---   If it's the final run and coastal processing is in effect, check
c ---   the center of null cells to see if they are ocean and redefine
c ---   land use as ocean if true
        if(lfinal)then
          if(isum.NE.0) then
            write(ioout,fmtf) i,j,(ngrid(k,i,j)/float(isum),k=1,mxcat),
     &                        fgood
          else
            if(lcoast.AND.lmarfill) then
              x=xorigin+dgrid*(float(i)-0.5)
              y=yorigin+dgrid*(float(j)-0.5)
              iptypes=iptype(x,y)
              if(iptypes.eq.0) then
                write(ioout,fmtf) i,j,(zero(k),k=1,kocean-1),1.,
     *                            (zero(m),m=kocean+1,mxcat),focean
                nocean=nocean+1
              else
                write(ioout,fmtf) i,j,(zero(k),k=1,mxcat),fmiss
                nzero=nzero+1
              endif
            else
              write(ioout,fmtf) i,j,(zero(k),k=1,mxcat),fmiss
              nzero=nzero+1
            endif
          endif
        else
          if(isum.EQ.0) then
             write(ioout,fmtc) i,j,(ngrid(k,i,j),k=1,mxcat),fmiss
          else
             write(ioout,fmtc) i,j,(ngrid(k,i,j),k=1,mxcat),fgood
          endif
        endif
2     continue

c --- Compute average number of 'hits' per grid cell
      iavg = isumall/(nx*ny)

c --- Write out table of 'hits' per grid cell
      isd=5
      do i=5,1,-1
         itest=10**i
         if(itest.GT.isummax) isd=i
      enddo
      isd=MIN(5,(isd+1))
      ldate = .false.
      messag = 'Number of CTG land use cell hits'
      call OUT(xdum,isumgrd,2,isd,ldate,messag,1,1,nx,ny)

c --- Warn user if less than user-specified threshold (percent, eg. 75%)
c --- of average number of hits
      ifract = max(1,int(real(iavg * ithres)/100.))
      do 4 j=1,ny
      do 4 i=1,nx
        if(isumgrd(i,j).LT. ifract) nlow=nlow+1
4     continue

c --- Warn user of low number of hits in any cell
      if(nlow.GT.0) then
        write(iolst,*)
        write(iolst,*)
        write(iolst,30) nlow,ifract
30      format(1x,'Number of land use hits low in',i10,' Cells',/1x,
     &     'with fewer than ',i5,' hits per cell.')
        if(lfinal)then
          write(iolst,31)
          if(nzero.gt.0) write(iolst,32) nzero
31        format(1x,'INVESTIGATE cells that are partially filled.')
32        format(///1x,'POTENTIAL ERROR: Number of Grid Cells with no ',
     &          'defined land use = ',i10,/1x,
     &          'This should NOT be your LAST run unless these cells ',
     &          'are PROPERLY filled in',/1x,'with the missing value ',
     &          '(IMISS) used in the next processing step (MAKEGEO).',
     &          /1x,'Consult the gridded table printed above to ',
     &          'identify the cells.')
        endif
      endif

c --- warn user if missing cells were filled with "ocean"
      if(lcoast.and.nocean.gt.0) then
        write(iolst,33) nocean
33      format(/1x,'Coastline processing - empty cells filled with '/
     &           1x,'"OCEAN" in the following number of cells :',
     &           i10//)
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine wrthead
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682          Level: 050128           WRTHEAD
c               D. Strimaitis, Earth Tech
c
c PURPOSE:     WRTHEAD constructs the header records for the output
c              data file LU.DAT
c
c --- Update
c     Ver 2.4  Level 030402 to Ver 2.63 Level 050128         DGS
c              Add call to COORDSVER to access the COORDS version info
c              and pass string to comment section of output file.
c
c ARGUMENTS:
c    PASSED:  /CONTROL/   logicals
c             /GRID/      data
c             /QA/        ver,level
c
c  RETURNED:  none
c
c CALLING ROUTINES:   SVGRID
c
c EXTERNAL ROUTINES:  NIMADATE, COORDSVER
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.ctg'
      include 'control.ctg'
      include 'grid.ctg'
      include 'qa.ctg'

c --- Local Variables
      character*16 dataset,dataver
      character*64 datamod
      character*86 comment1
      character*50 verdoc

c --- Configure output variables
      data dataset/'LU.DAT'/, dataver/'2.0'/
      data datamod/'Header structure with coordinate parameters'/
      data ncomment/2/
      data comment1/'Produced by CTGPROC Version: '/

c --- Construct the version-level comment string
      j=30
      do i=1,12
         if(ver(i:i).NE.' ') then
            comment1(j:j)=ver(i:i)
            j=j+1
         endif
      enddo
      j=j+1
      comment1(j:j+7)=' Level: '
      j=j+8
      do i=1,12
         if(level(i:i).NE.' ') then
            comment1(j:j)=level(i:i)
            j=j+1
         endif
      enddo

c --- Obtain COORDS version information
      call COORDSVER(iolst,verdoc)

c --- Record 1:  Dataset, Version, Modifier
      write(ioout,'(2a16,a64)') dataset,dataver,datamod
c --- Record 2:  Number of comment records
      write(ioout,'(i4)') ncomment
c --- Record 3:  Comment (optional/repeatable)
      write(ioout,'(a86)') comment1
c --- Report COORDS version
      comment1(1:36)='Internal Coordinate Transformations '
      comment1(37:86)=verdoc
      write(ioout,'(a86)') comment1
c --- Record 4:  File Type
c --- Set type of LU output fields
      if(lfinal) then
         write(ioout,'(a8)') 'FRACTION'
      else
         write(ioout,'(a8)') 'HITS    '
      endif
c --- Record 5:  Map projection
      write(ioout,'(a8)') pmap
c --- Record 6:  Map projection parameters
      if(LUTM) then
         write(ioout,'(i4,a4)') izone,utmhem
      elseif(LLCC) then
         write(ioout,'(4a16)') clat0,clon0,clat1,clat2
      elseif(LPS) then
         write(ioout,'(3a16)') clat0,clon0,clat1
      elseif(LEM.or.LLAZA.or.LTTM) then
         write(ioout,'(2a16)') clat0,clon0
      endif
c --- Record 7:  False Easting, Northing
      if(LLCC.or.LLAZA.or.LTTM) then
         write(ioout,*) feast,fnorth
      endif
c --- Record 8:  Map DATUM
      call NIMADATE(daten)
      write(ioout,'(a8,a12)') datum,daten
c --- Record 9:  Grid
      write(ioout,'(2i8,4f12.3,i6)') nx,ny,xorigin,yorigin,
     &                             dgrid,dgrid,mxcat
c --- Record 10:  XYUNIT
      write(ioout,'(a4)') 'KM  '

      return
      end
c----------------------------------------------------------------------
      subroutine out(rarray,iarray,ityp,nsigd,ldate,messag,nbx,nby,
     1 nex,ney)
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681       Level: 990130                   OUT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write a gridded field of real or integer numbers
c
c --- Update
c     Ver 1.0  Level 961113 to 990130       DGS
c              Allow subgrid to be printed, and use (I3) cell index
c
c --- INPUTS:
c     RARRAY(MXNX,MXNY) - Real array  - Array of real numbers to print
c                                       (used only if ITYP = 1)
c     IARRAY(MXNX,MXNY) - Int. array  - Array of integer numbers to
c                                       print (used only if ITYP = 2)
c                  ITYP - Integer     - Array type (1=real, 2=integer)
c                 NSIGD - Integer     - No. digits to print (valid range
c                                       for NSIGD is 1 to 5)
c                 LDATE - Logical     - Control variable for printing
c                                       of date (.true. = print date in
c                                       common /GEN/, .false. = do not
c                                       print date)
c                MESSAG - Char.*70    - Label of table
c                   NBX - Integer     - Starting X cell to print
c                   NBY - Integer     - Starting Y cell to print
c                   NEX - Integer     - Ending X cell to print
c                   NEY - Integer     - Ending Y cell to print
c       Common block /GEN/ variables:
c          NYR, NMO, NDAY, NJUL, NHR  - (Used only if LDATE=.true.)
c       Parameters: MXNX, MXNY, iolst
c
c --- OUTPUT:  none
c
c --- OUT    called by:  SVGRID
c --- OUT    calls:      WRT, WRT2
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.ctg'
c
c --- Set dummy common for "date" variables (not used here)
      common/gen/nyr,nmo,nday,njul,nhr
c
      real rarray(mxnx,mxny)
c
      integer iarray(mxnx,mxny),icol(5)
      integer iout(mxnx)
c
      logical ldate
c
      character*70 messag
      character*1 sign(mxnx),plus,minus
      character*24 form1(5)
      character*21 form2(5)
      character*18 form3(5)
c
c     include 'gen.met'
c
      data icol /40,40,30,25,20/
      data plus,minus /'+','-'/
      data form1 /'(1x,i3,1x,1hI,40(i3,1x))',
     1            '(1x,i3,1x,1hI,40(i3,1x))',
     2            '(1x,i3,1x,1hI,40(i3,1x))',
     3            '(1x,i3,1x,1hI,40(i4,1x))',
     4            '(1x,i3,1x,1hI,40(i5,1x))'/
      data form2 /'(5x,1hI,40(2x,a1,1x))',
     1            '(5x,1hI,40(2x,a1,1x))',
     2            '(5x,1hI,40(2x,a1,1x))',
     3            '(5x,1hI,40(3x,a1,1x))',
     4            '(5x,1hI,40(4x,a1,1x))'/
      data form3 /'(6x,40(i3,1x))',
     1            '(6x,40(i3,1x))',
     2            '(6x,40(i3,1x))',
     3            '(6x,40(i4,1x))',
     4            '(6x,40(i5,1x))'/

c --- check that valid values of array type (ityp) and print digits
c --- (nsigd) have been passed to routine
      if(ityp.ne.1.and.ityp.ne.2)then
         write(iolst,*)'ERROR in SUBR. OUT -- invalid value of ITYP',
     1   ' -- ITYP = ',ityp
         write(*,*) 'ERROR in SUBR. OUT  --  See Run LIST File'
         stop
      endif
      if(nsigd.lt.1.or.nsigd.gt.5)then
         write(iolst,*)'ERROR in SUBR. OUT -- invalid value of NSIGD',
     1   ' -- NSIGD = ',nsigd
         write(*,*) 'ERROR in SUBR. OUT  --  See Run LIST File'
         stop
      endif
c
c --- compute no. X cells to print
      nx=nex-nbx+1
c
      icr=2
      if(nsigd.eq.1)icr=1
      if(mod(nx,icol(nsigd)).eq.0)then
         npass=nx/icol(nsigd)
      else
         npass=nx/icol(nsigd)+1
      endif
c
c --- real array -- find min. & max. values
      if(ityp.ne.1)go to 50
      xmax=-1.e-25
      xmin=1.e25
      do 10 i=nbx,nex
      do 10 j=nby,ney
      if(rarray(i,j).gt.xmax)xmax=rarray(i,j)
      if(rarray(i,j).lt.xmin)xmin=rarray(i,j)
10    continue
      if(xmin.ne.0.0.or.xmax.ne.0.0)go to 12
      if(ldate)write(iolst,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(iolst,95)messag
      write(iolst,11)
11    format(1x,'GRID NOT PRINTED -- all values zero')
      return
c
12    continue
      xexp=xmax
      if(abs(xmin).gt.xmax)xexp=abs(xmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic1=nbx
      ic2=ic1+icol(nsigd)-1
      if(ic2.gt.nex)ic2=nex
c
      do 30 ipass=1,npass
c
      if(ldate)write(iolst,94)messag,nyr,nmo,nday,njul,nhr
94    format(/1x,a70,2x,'year: ',i2,2x,'month: ',i2,2x,'day: ',i2,2x,
     1 'Julian day: ',i3,2x,'hour: ',i2/)
      if(.not.ldate)write(iolst,95)messag
95    format(/1x,a70/)
      write(iolst,109)nexp
109   format(1x,'Multiply all values by 10 ** ',i3/)
c
      do 20 jj=ney,nby,-1
         icnt=0
c
         do 18 i=ic1,ic2
            icnt=icnt+1
            if(rarray(i,jj).lt.0)then
               iout(icnt)=-(rarray(i,jj)*xscale-0.5)
               sign(icnt)=minus
            else
               iout(icnt)=rarray(i,jj)*xscale+0.5
               sign(icnt)=plus
            endif
18       continue
         call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,iolst)
20    continue
c --- Set underline (minimum space per cell is 4 characters)
      minund=4     
      nund=icnt*MAX((nsigd+1),minund)
      write(iolst,101)(minus,n=1,nund)
101   format(5x,160a1)
      call wrt2(form3(nsigd),ic1,ic2,iolst)
c
      ic1=ic1+icol(nsigd)
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nex)ic2=nex
30    continue
      return
c
c --- integer array -- find min. & max. values
50    continue
      kmax=-9999999
      kmin=9999999
      do 110 i=nbx,nex
      do 110 j=nby,ney
      if(iarray(i,j).gt.kmax)kmax=iarray(i,j)
      if(iarray(i,j).lt.kmin)kmin=iarray(i,j)
110   continue
      if(kmin.ne.0.or.kmax.ne.0)go to 102
      if(ldate)write(iolst,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(iolst,95)messag
      write(iolst,11)
      return
c
102   continue
      xexp=kmax
      if(iabs(kmin).gt.kmax)xexp=iabs(kmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic1=nbx
      ic2=ic1+icol(nsigd)-1
      if(ic2.gt.nex)ic2=nex
c
      do 130 ipass=1,npass
c
      if(ldate)write(iolst,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(iolst,95)messag
      write(iolst,109)nexp
c
      do 120 jj=ney,nby,-1
         icnt=0
c
         do 118 i=ic1,ic2
            icnt=icnt+1
            if(iarray(i,jj).lt.0)then
               iout(icnt)=-(iarray(i,jj)*xscale-0.5)
               sign(icnt)=minus
            else
               iout(icnt)=iarray(i,jj)*xscale+0.5
               sign(icnt)=plus
            endif
118      continue
         call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,iolst)
120   continue
c --- Set underline (minimum space per cell is 4 characters)
      minund=4     
      nund=icnt*MAX((nsigd+1),minund)
      write(iolst,101)(minus,n=1,nund)
      call wrt2(form3(nsigd),ic1,ic2,iolst)
c
      ic1=ic1+icol(nsigd)
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nex)ic2=nex
130   continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrt(form1,form2,jj,iout,sign,n,iolst)
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681       Level: 960901                   WRT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write one Y row of gridded data
c
c --- INPUTS:
c              FORM1 - Char.*24    - Format field for Y label and data
c                                    to be printed
c              FORM2 - Char.*21    - Format field for sign of data
c                 JJ - Integer     - Y grid cell number
c            IOUT(N) - Int. array  - Array of data to be printed
c                                    (one Y row)
c            SIGN(N) - Char.*1     - Array containing sign of data
c                                    ('+' or '-')
c                  N - Integer     - Number of cells in this row
c                iolst - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT called by:  OUT
c --- WRT calls:      none
c----------------------------------------------------------------------
      integer iout(n)
c
      character*1 sign(n)
      character*24 form1
      character*21 form2
c
      write(iolst,form1)jj,iout
      write(iolst,form2)sign
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrt2(form,n1,n2,iolst)
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681       Level: 960901                  WRT2
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write a line labeling grid cell numbers
c
c --- INPUTS:
c               FORM - Char.*18    - Format field of data to be printed
c                 N1 - Integer     - Starting grid cell number
c                 N2 - Integer     - Ending grid cell number
c                iolst - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT2 called by:  OUT
c --- WRT2 calls:      none
c----------------------------------------------------------------------
      character*18 form
c
      write(iolst,form)(i,i=n1,n2)
      return
      end
c----------------------------------------------------------------------
      subroutine fin
c----------------------------------------------------------------------
c
c --- CTGPROC  Version: 2.681           Level: 011003               FIN
c ---          J. Scire, Earth Tech
c
c --- PURPOSE:  Run termination routine -- compute runtime
c
c --- INPUTS:
c       Common block /QA/
c          rdate, rtime, rcpu
c       Parameters: iolst, IOMESG
c
c --- OUTPUT:  none
c
c --- FIN called by:  MAIN
c --- FIN calls:      DATETM, JULDAY, DELTT
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.ctg'
      include 'qa.ctg'
c
      character*8 rtime2
      character*10 rdate2
c
      write(iomesg,*)'TERMINATION PHASE'
c
c --- get system date & time at end of run
      call datetm(rdate2,rtime2,rcpu)
c
c --- compute runtime
      read(rtime(1:2),10)ihr1
      read(rtime(4:5),10)imin1
      read(rtime(7:8),10)isec1
10    format(i2)
      t1=ihr1*3600.+imin1*60.+isec1
c
      read(rtime2(1:2),10)ihr2
      read(rtime2(4:5),10)imin2
      read(rtime2(7:8),10)isec2
      t2=ihr2*3600.+imin2*60.+isec2
c
      if(rdate.eq.rdate2)then
         delt=t2-t1
      else
         read(rdate(1:2),10)imo1
         read(rdate(4:5),10)iday1
         read(rdate(7:10),'(i4)')iyr1
         call julday(iolst,iyr1,imo1,iday1,ijul1)
c
         read(rdate2(1:2),10)imo2
         read(rdate2(4:5),10)iday2
         read(rdate2(7:10),'(i4)')iyr2
         call julday(iolst,iyr2,imo2,iday2,ijul2)
c
c ---    compute no. hours from beg. of first hour of run to
c ---    ending hour of ending day of the run
         call deltt(iyr1,ijul1,ihr1,iyr2,ijul2,ihr2,idelhr)
c
c ---    adjust for minutes and seconds
         delt=idelhr*3600.-imin1*60.-isec1+imin2*60.+isec2
      endif

c --- On the PC, the runtime and CPU time are the same
c --- (DATETM provides RCPU = 0.0 on the PC)
      if(rcpu.EQ.0.0)rcpu=delt

c --- Report current date
      write(iolst,1402)rtime2,rdate2,delt,rcpu
1402  format(//2x,'End of run -- Clock time: ',a8/
     1         2x,'                    Date: ',a10//
     2         2x,'      Elapsed Clock Time: ',f10.1,' (seconds)'//
     3         2x,'                CPU Time: ',f10.1,' (seconds)')

c
      return
      end
c
c-----------------------------------------------------------------------
	logical function lbig_end(i4c)
c-----------------------------------------------------------------------
c      
c --- CTGPROC   Version: 2.682          Level: 040921          LBIG_END
c               K. Morrison, Earth Tech
c
c PURPOSE:      LBIG_END tests the endian of the machine on which the
c               program is running.
c
c-----------------------------------------------------------------------

	integer*4 i4b
	integer*2 i2b(2)
	equivalence (i4b,i2b(1))
c
	i4b=1
	if(i2b(1).eq.1) then
		lbig_end=.false.
	elseif(i2b(2).eq.1) then
		lbig_end=.true.
	else
		write(*,*) 'Invalid ENDIAN result'
	endif
	return
	end
c-----------------------------------------------------------------------
      subroutine settrano(cmapi,iutmzni,xlat1i,xlat2i,rlati,rloni,
     &  feasti,fnorti,izone,tmsone,xlat1,xlat2,rlat,rlon,
     &  feast,fnorth,caction,vecti,vecto)
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682        Level: 051201            SETTRANO
c               K. Morrison, Earth Tech
c
c PURPOSE:     SETTRANO consolidates the calls for the translation
c              vectors for output.
c
c Updates:
c         V2.65 (051201) from V2.64 (040921)
c               - Add Albers Conical Equal Area as input projection
c                 (add XLAT1I and XLAT2I to inputs)
c               - Change to GLOBE1 in CALUTILS and COORDS in COORDLIB
c                 to support Albers
c
c         V2.64 (040921) Copied from TERREL Ver. 3.58_draft Level 040831
c
c --- INPUTS:
c         INPUT PROJECTION
c            CMAPI - char*8     - Map projection of input coordinates
c                                  LL  : N.Lat., E.Long.
c                                  UTM : Universal Transverse Mercator
c                                  TM  : Transverse Mercator
c                                  LCC : Lambert Conformal Conic
c                                  PS  : Polar Stereographic
c                                  EM  : Equatorial Mercator
c                                  LAZA: Lambert Azimuthal Equal Area
c                                  ACEA: Albers Conical Equal Area
c          IUTMZNI - integer    - UTM zone of input coords.
c                                  (S. hemisphere is NEGATIVE)
c           XLAT1I - real       - Matching Equator-ward N.Latitude
c           XLAT2I - real       - Matching Pole-ward N.Latitude
c            RLATI - real       - Map origin N.Latitude
c            RLONI - real       - Map origin E.Longitude
c           FEASTI - real       - False Easting (km) at proj. origin
c           FNORTI - real       - False Northing (km) at proj. origin
c         OUTPUT PROJECTION
c            IZONE - integer    - UTM zone of output coords.
c                                  (S. hemisphere is NEGATIVE)
c           TMSONE - real       - Scale Factor for TM projection
c            XLAT1 - real       - Matching Equator-ward N.Latitude
c            XLAT2 - real       - Matching Pole-ward N.Latitude
c             RLAT - real       - Map origin N.Latitude
c             RLON - real       - Map origin E.Longitude
c            FEAST - real       - False Easting (km) at proj. origin
c           FNORTH - real       - False Northing (km) at proj. origin
c            /CONTROL/          - program control logicals
c
c
c --- OUTPUT:
c         VECTI(9) - real*8 arr - Input Coordinate description vector:
c                                 UTM zone or TM Scale Factor
c                                 Reserved
c                                 Reserved
c                                 Matching Equator-ward N.Latitude
c                                 Matching Pole-ward N.Latitude
c                                 Map origin E.Longitude
c                                 Map origin N.Latitude
c                                 False Easting
c                                 False Northing
c         VECTO(9) - real*8 arr - Output Coordinate description vector:
c                                 UTM zone override (ignore if 999.0D0)
c                                     or TM Scale Factor
c                                 Reserved
c                                 Reserved
c                                 Matching Equator-ward N.Latitude
c                                 Matching Pole-ward N.latitude
c                                 Map origin E.Longitude
c                                 Map origin N.Latitude
c                                 False Easting
c                                 False Northing
c          CACTION - char*12    - Map conversion string (e.g., UTM2LCC)
c
c
c --- SETTRANO called by: RDUSGSHD, CORNERS, LOADDMDF, LOADNZGNR,
C                         LOADGNR, LOADLAZA, LOADOTHER, COASTEXT
c --- SETTRANO calls:     GLOBE1
c----------------------------------------------------------------------
c --- Parameters
      include 'params.ctg'
c
c --- Control common 
      include 'control.ctg'

	character*8 cmapi
	character*12 caction
	real*8 vecti(9),vecto(9)

c --- Set translation vectors to the output grid

	if(lutm) then
c ---    Using UTM grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'UTM     ',izone,xdum,xdum,xdum,rdum,rdum,
     &               feast,fnorth,
     &               caction,vecti,vecto)

	elseif(llcc) then
c ---    Using Lambert Conformal grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'LCC     ',idum,xdum,xlat1,xlat2,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

	elseif(lps) then
c ---    Using Polar Stereographic grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'PS      ',idum,xdum,xlat1,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

	elseif(lem) then
c ---    Using Equatorial Mercator grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'EM      ',idum,xdum,xlat1,-xlat1,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

	elseif(llaza) then
c ---    Using Lambert Azimuthal Equal Area grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'LAZA    ',idum,xdum,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

	elseif(lttm) then
c ---    Using Tangential TM grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'TM      ',idum,tmsone,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

	endif
	return
	end
c-----------------------------------------------------------------------
      subroutine coastext(xllk,yllk,xurk,yurk,izone,rlat,rlon,
     &                    xlat1,xlat2,feast,fnorth,utmhem,datum)
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682        Level: 051201            COASTEXT
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     COASTEXT extracts polygons of water/land interfaces
c              within the desired grid area from the GSHHS data set,
c              passing these into the SHORES common for subsequent
c              use to evaluate crude type for points and grid cells.
c              The types are: 0-ocean, 1-mainland and marine islands,
c              2-lakes, 3-islands in lakes, 4-ponds on islands.  It 
c              also outputs a Surfer BLN file for coastline mapping. 
c
c --- Updates
c         Ver. 2.64 Level 050428 to Ver. 2.65 Level 051201      KAM
c              - Change call to SETTRANO to conform to new arguements
c              - Correct test for a surrounding polygon when it
c                is the only polygon (i.e. coastline processing not
c                needed)
c         Ver. 2.63 Level 050128 to Ver. 2.64 Level 050428      KAM
c              - Reset negative longitudes where needed, since GLOBE
c                changes input -long into output +long
c         Ver. 2.62 Level 041215 to Ver. 2.63 Level 050128      DGS
c              - Initialize LSURRSIDE as an array
c         Ver. 2.61 Level 041013 to Ver. 2.62 Level 041215      DGS
c              - Remove /GRID/ inclusion, and pass needed data by
c                arguments (facilitate subroutine use in TERREL and
c                CTGPROC).
c              - Add array bound checks against MXCOAST, MXCOASTP
c
C         Copied from TERREL Ver. 3.61 Level 041013
c         Ver. 3.60 Level 040920 to Ver. 3.61 Level 041013
c              - Reactivate WDBII, but with empirical fix-up for latitudes,
c                actual use of WDBII polygons is controlled by the 
c                variable LWDBII, currently false
c              - Change polygon loop from fixed number to no index,
c                now exiting on GSHHS EOF
c              - Change EOF in BREADH, and move the loop exit on EOF
c              - Correct UTM out zone in call to SETTRANO
c         Ver. 3.57_draft Level 040429 to Ver. 3.59_draft Level 040901
c              - Corrections to polygon closing
c         Ver. 3.56_draft Level 040311 to Ver. 3.57_draft Level 040429
c              - Disable use of WDBII data
c         Ver. 3.54_draft Level 040303 to Ver. 3.56_draft Level 040311
c              - Change points examination to lat-lon
c              - Add treatment for data and grids near Greenwich
c              - Correct additional outside point for end of segment
c              - Add polygon source and id to BLN identifier field
c         Ver. 3.53_draft Level 040302 to Ver. 3.54_draft Level 040303
c              - Fix bug in surrounding polygon detection
c         Ver. 3.52_draft Level 040229 to Ver. 3.53_draft Level 040302
c              - Move min-max assignment to block data
c         Ver. 3.51_draft Level 040228 to Ver. 3.52_draft Level 040229
c              - Explicitly define the surrounding polygon if the zone
c                is surrounded without a coastline intersection
c
c ARGUMENTS:
c    PASSED:  /CONTROL/   input datums, output projection
c             /FILNAM/    file names
c
c  RETURNED:  /SHORES/    shoreline data
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  SETTRANO, GLOBE, BREADH, BREADP, ISIDE, PCORNER,
c                     LBIG_END
c-----------------------------------------------------------------------
c --- Include parameter and common files
c
      include 'params.ctg'
      include 'filnam.ctg'
      include 'control.ctg'
      include 'shores.ctg'
      include 'gspan.ctg'
c
c --- Declare character arguments
      character*4 utmhem
      character*8 datum
c
c --- Local variables
c      
      integer id,npts,itype,ixmin,ixmax,iymin,iymax,iarea
      real uxmax,uxmin,uymax,uymin
      real xp1i,xp1it,xp1,yp1i,yp1it,yp1,xps,yps
      real*8 vecti(9),vecto(9),vectis(9),vectos(9)
      integer*2 igreen,isource
      integer sside,oside,nside,eside
      character*12 caction,cll2ll
      character*4 c4dum
      character*8 cdatum(2)
      logical lgood/.false./,linside/.false./,leof/.false./,lfirst,
     &  lbigendian,lbig_end,lsurrside(4),lstraddleg/.false./,
     &  lwdbii/.false./

c --- Logicals for array bound checks (DGS)
      logical lpnts/.false./, lpoly/.false./

c
      write(*,*)
      write(*,*) ' Processing coastline data'
      write(*,*)
c
c --- Set coast counter to 0, but set first type to ocean in case no
c     polygons are found at all, only possible if the zone is in the
c     ocean with not even any islands
c
      nshore=0
      itypes(1)=0
c
c --- Copy the datums for the 2 data types
c
      cdatum(2)=dwvs
      cdatum(1)=dwdbii
c
c --- Set input dataset false Easting/Northing to zero and TM scale to 1
      feasti=0.0
      fnorti=0.0
      tmsone=1.0

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

      call SETTRANO('LL      ',iutm,xdum,xdum,rdum,rdum,
     &            feasti,fnorti,
     &            iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &            feast,fnorth,
     &            caction,vecti,vecto)
      call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &            feasti,fnorti,
     &            'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &            feasti,fnorti,
     &            cll2ll,vectis,vectos)

c
c     expand the user zone by 1% to avoid border problems
c
      dwide=(xurk-xllk)*.01
      dhigh=(yurk-yllk)*.01
      uxmax=xurk+dwide
      uxmin=xllk-dwide
      uymax=yurk+dhigh
      uymin=yllk-dhigh
c
c     Get the east longitudes and latitudes for the grid limits
c
      dwide=dwide/107.9*cos((ylatnw+ylatse)/2.*.01745393)
      dhigh=dhigh/107.9
      xlonww=min(-xlonsw,-xlonnw)-dwide
      if(xlonww.lt.0.) xlonww=xlonww+360.
      xlonee=max(-xlonse,-xlonne)+dwide
      if(xlonee.lt.0.) xlonee=xlonee+360.
      ylatnn=max(ylatnw,ylatne)+dhigh
      ylatss=min(ylatsw,ylatse)-dhigh
      if(xlonww.gt.xlonee) then
        lstraddleg=.true.
        xlonww=xlonww-360.
      endif

c DGS
      write(iolst,*)
      write(iolst,*)'COASTLINE processing from GSHHS data set'
      write(iolst,*)
      write(iolst,*)'Grid Limits --- Datum ',datum
      write(iolst,*)'E. Longitudes:',xlonww,xlonee
      write(iolst,*)'N. Latitudes :',ylatss,ylatnn
      write(iolst,*)
c DGS

c
c     Open the GSHHS full-resolution file
c
      open(unit=iogshhs,file=gshhsin,access=trim(caccess),form=cform,
     &  status='OLD',recl=irecl)
c
c     Check if the current machine is BIG-ENDIAN
c
      lbigendian=lbig_end(1)
c      
c     Set the point counter for the first point
c
      ipp=1
c      
c     Loop over the polygons in the GSHHS file
c
      Polygons: do
c
c           Clear the surround logicals
c
            do kside=1,4
               lsurrside(kside)=.false.
            enddo
            lfirst=.false.
c
c           Read the header for this GSHHS polygon
c
            call breadh(id,npts,itype,ixmin,ixmax,iymin,iymax,
     *      iarea,igreen,isource,iogshhs,leof,lbigendian)
            if(leof) goto 99
c
c           Convert raw data for limits from millionths to whole degrees
c
            shft=0.
            xmaxit=real(ixmax)/1.e6
            if(xmaxit.lt.0.) xmaxit=xmaxit+360.
            xminit=real(ixmin)/1.e6
            if(xminit.lt.0.) xminit=xminit+360.
            ymaxit=real(iymax)/1.e6
            yminit=real(iymin)/1.e6
c            
c           Increment isource to use the proper datum, transform
c           to output datum latitude-longitude, and adjust for -/+
c           shift at Greenwich
c
            isource=isource+1
            call globe(iolst,cll2ll,cdatum(isource),vectis,
     &        datum,vectos,xminit,yminit,xmini,ymini,idum,c4dum)
            call globe(iolst,cll2ll,cdatum(isource),vectis,
     &        datum,vectos,xmaxit,ymaxit,xmaxi,ymaxi,idum,c4dum)
c
c           Check for Greenwich straddle
c
            if(xmini.gt.xmaxi) then
c
c             If output grid also straddles, keep original range
c
              if(lstraddleg) then
                xmini=xmini-360.
c
c             Otherwise check the side of Greenwich.  If the grid is
c             just west, shift all longitudes further east, otherwise
c             restore original range
c
              else
                if(xlonee.gt.xmini) then
                  shft=360.
                  xmaxi=xmaxi+shft
                else
                  xmini=xmini-360.
                endif
              endif
            endif
c
c           Empirical fix for WDBII
c
            if(isource.eq.1) then
              xav=abs((xmini+xmaxi)/2.)
              xoff=mod(xav,6.)
              if(xoff.gt.3.) xoff=6.-xoff
              ylatmul=1.-(6.006006e-5+1.134468e-4*xoff)
              ymini=ymini*ylatmul
              ymaxi=ymaxi*ylatmul
            endif
c
c           If polygon is outside of grid, or if the polygon is
c           from WDBII and logical not set, simply read through to go
c           to next polygon
c
            if(xlonee.lt.xmini.or.xlonww.gt.xmaxi
     &        .or.ylatnn.lt.ymini.or.ylatss.gt.ymaxi.or.
     &        (isource.eq.1.and..not.lwdbii))
     &        then
               do j=1,npts
                 read(iogshhs) idum1,idum2
               enddo
c
c           Parts or all are in, so extract appropriate parts for a
c           smaller polygon closing sections along the outside of the grid
c
            else
               lgood=.false.
               linside=.false.
               lfirst=.true.
               if(nshore.LT.mxcoast) then
                  nshore=nshore+1
               else
                  lpoly=.true.
                  goto 999
               endif
               nspts(nshore)=0
               istart(nshore)=ipp
               itypes(nshore)=itype
               isourcep(nshore)=isource
               ipolyid(nshore)=id
c
c              Loop through the points on the GSHHS polygon
c
               Points: do j=1,npts
c
                  call breadp(xp1it,yp1it,lbigendian,iogshhs)
                  xp1it=xp1it+shft
c
c                 Empirical fix for WDBII
c
                     if(isource.eq.1) then
                        xoff=mod(abs(xp1it),6.)
                        if(xoff.gt.3.) xoff=6.-xoff
                        ylatmul=1.-(6.006006e-5+1.134468e-4*xoff)
                        yp1it=yp1it*ylatmul
                     endif
c
c                 Transform the coordinates to the output datum
c                 and also the output projection
c
                  call globe(iolst,cll2ll,cdatum(isource),vectis,
     &            datum,vectos,xp1it,yp1it,xp1i,yp1i,idum,c4dum)
                  call globe(iolst,caction,cdatum(isource),vecti,
     &            datum,vecto,xp1it,yp1it,xp1,yp1,idum,c4dum)
c
c                 Check for shift at Greenwich
c
                  if(xmini.le.0.) then
                     if(xp1i.gt.220.) xp1i=xp1i-360.
                  endif
c
c                 Check that point is inside the user zone
c
                  if(xp1i.ge.xlonww.and.xp1i.le.xlonee.and.yp1i.ge.
     *              ylatss.and.yp1i.le.ylatnn) then
c
c                   Check for an arbitrary starting point 
c
                    if(j.eq.1) linside=.true.
c
c                   Start a new segment of the polygon
c
                    if(.not.lgood) then
                      lgood=.true.
c
c                     Check if this is the first good segment
c
c                     yes - set counter and reset flag
c
                      if(lfirst) then
                        lfirst=.false.
                        istart(nshore)=ipp
                        if(linside) then
                          xps=xp1
                          yps=yp1
                        else
                          xp(ipp)=xps
                          yp(ipp)=yps
                          if(ipp.LT.mxcoastp) then
                             ipp=ipp+1
                             nspts(nshore)=nspts(nshore)+1
                          else
                             lpnts=.true.
                             goto 999
                          endif
                        endif
c
c                       Check the side of the zone on which this section starts
c
                        sside=iside(xp1,yp1,uxmax,uxmin,uymax,uymin)
c
                      else
c
c                       Save the previous point
c
                        xp(ipp)=xps
                        yp(ipp)=yps
                        if(ipp.LT.mxcoastp) then
                           ipp=ipp+1
                           nspts(nshore)=nspts(nshore)+1
                        else
                           lpnts=.true.
                           goto 999
                        endif
c
                      endif
                    endif
c
c                   Save the point and increment counters
c
                    xp(ipp)=xp1
                    yp(ipp)=yp1
                    if(ipp.LT.mxcoastp) then
                       ipp=ipp+1
                       nspts(nshore)=nspts(nshore)+1
                    else
                       lpnts=.true.
                       goto 999
                    endif
c
c                 End of a section inside zone
c
                  elseif(lgood) then
c
c                   Check the side of the zone where this section ends
c
                    oside=iside(xp1,yp1,uxmax,uxmin,uymax,uymin)
c
c                   Add this outside point, but first save it in case
c                   it's needed to close the polygon
c
                    xpe=xp1
                    ype=yp1
                    ippe=ipp
                    eside=iside(xp1i,yp1i,xlonee,xlonww,ylatnn,
     *                ylatss)
                    xp(ipp)=xp1
                    yp(ipp)=yp1
                    if(ipp.LT.mxcoastp) then
                       ipp=ipp+1
                       nspts(nshore)=nspts(nshore)+1
                    else
                       lpnts=.true.
                       goto 999
                    endif
                    lgood=.false.
c
                  else
c
c                   Save point in case of need, and add any necessary
c                   corners to the polygon
c
                    xps=xp1
                    yps=yp1
                    if(.not.lfirst) then
                      nside=iside(xp1i,yp1i,xlonee,xlonww,ylatnn,
     *                 ylatss)
                      if(nside.ne.oside) then
                        call pcorner(nside,oside,ipp,xp,yp,nspts,
     *                    nshore,uxmax,uxmin,uymax,uymin,1)
                        oside=nside
                      endif
                    else
                      oside=iside(xp1i,yp1i,xlonee,xlonww,ylatnn,ylatss)
                    endif
                    lsurrside(oside)=.true.
c
                    lgood=.false.
                  endif
c
c              End of loop by points
c
               enddo Points
c
c              If no points were found inside the zone, set a polygon
c              to the extremes of the zone and then goto the next GSHHS
c              polygon - ie the next major outer loop.  This happens if
c              the GSHHS polygon surrounds the zone but doesn't 
c              intersect it.  Set the type for this polygon anyway - it
c              may be used if no shores are found.
c
               if(lfirst) then
c
c                Check that it actually surrounds the grid 
c
                 if((xlonee.le.xmaxi.and.xlonww.ge.xmini.and.ylatnn
     *             .le.ymaxi.and.ylatss.ge.ymini).and.
     *             (lsurrside(1).and.lsurrside(2).and.
     *             lsurrside(3).and.lsurrside(4))) then

                   itypes(nshore)=itype
                   istart(nshore)=ipp
c
c                  Water - go clockwise
c
                   if(mod(itype,2).EQ.0) then
                     icstart=1
                     icend=5
                     iinc=1
c
c                  Land - go counter-clockwise
c
                   else
                     icstart=5
                     icend=1
                     iinc=-1
                   endif
c
                   nspts(nshore)=0
                   do iicc=icstart,icend,iinc
                     iico=iicc
                     if(iico.gt.4) iico=iico-4
                     iicn=iico+iinc
                     if(iicn.le.0) iicn=iicn+4
                     if(iicn.gt.4) iicn=iicn-4
                     call pcorner(iico,iicn,ipp,xp,yp,nspts,
     *               nshore,uxmax,uxmin,uymax,uymin,2)
                   enddo
                   pxmax(nshore)=uxmax
                   pxmin(nshore)=uxmin
                   pymax(nshore)=uymax
                   pymin(nshore)=uymin
c
c                  Check that there are not nested surrounds
c
                   if(nshore.gt.1) then
                     itypes(nshore-1)=itype
                     ipp=istart(nshore)
                     nspts(nshore)=0
                     nshore=nshore-1
                   endif
c
c                If the polygon isn't to keep, decrement nshore
c
                 else
                   ipp=istart(nshore)
                   nspts(nshore)=0
                   nshore=nshore-1
                 endif
c
c                Go to next polygon
c
                 cycle Polygons
               endif
c
c              If the polygon didn't start within the zone, copy the
c              first point to the last to close the polygon after
c              checking corners
c
               if(.not.linside) then
c
c                First, check if the side of the start point and the
c                last outside point are the same
c
c                write(iolst,*) xpe,ype
                 if(ippe.LT.mxcoastp) then
                    ipp=ippe+1
                    nspts(nshore)=ipp-istart(nshore)
                 else
                    lpnts=.true.
                    goto 999
                 endif
                 oside=eside
                 dif=0.
                 if(sside.eq.oside) then
                   if(sside.eq.1) dif=ype-yp(istart(nshore))
                   if(sside.eq.2) dif=xpe-xp(istart(nshore))
                   if(sside.eq.3) dif=yp(istart(nshore))-ype
                   if(sside.eq.4) dif=xp(istart(nshore))-xpe
                 endif
c
c                Add a corner if necessary and reset the last side
c
c                Land or island - counterclockwise
c
                 if((itype.eq.1.or.itype.eq.3).and.dif.lt.0.) then
                   iicn=oside-1
                   if(iicn.eq.0) iicn=4
                   call pcorner(oside,iicn,ipp,xp,yp,nspts,
     *             nshore,uxmax,uxmin,uymax,uymin,3)
                   oside=iicn
c
c                Lake or pond - clockwise
c
                 else
                   if((itype.eq.2.or.itype.eq.4).and.dif.gt.0.) then
                     iicn=oside+1
                     if(iicn.eq.5) iicn=1
                     call pcorner(oside,iicn,ipp,xp,yp,nspts,
     *               nshore,uxmax,uxmin,uymax,uymin,4)
                     oside=iicn
                   endif
                 endif
c
c                if sides are not the same, add corners
c
                 if(sside.ne.oside) then
c
c                  Land or island - counterclockwise
c
                   if(itype.eq.1.or.itype.eq.3) then
                     if(sside.lt.oside) then
                       do iic=oside,sside+1,-1
                         iicn=iic-1
                         call pcorner(iic,iicn,ipp,xp,yp,nspts,
     *                   nshore,uxmax,uxmin,uymax,uymin,5)
                       enddo
                     else
                       nloop=4-sside+oside
                       do iicc=1,nloop
                         iic=oside-iicc+1
                         if(iic.le.0) iic=4+iic
                         iicn=iic-1
                         if(iicn.le.0) iicn=4+iicn
                         call pcorner(iic,iicn,ipp,xp,yp,nspts,
     *                   nshore,uxmax,uxmin,uymax,uymin,6)
                       enddo
                     endif
c
c                  Lake or pond - clockwise
c
                   else
                     if(sside.gt.oside) then
                       do iic=oside,sside-1
                         iicn=iic+1
                         call pcorner(iic,iicn,ipp,xp,yp,nspts,
     *                   nshore,uxmax,uxmin,uymax,uymin,7)
                       enddo
                     else
                       nloop=4-oside+sside
                       do iicc=1,nloop
                         iic=oside+iicc-1
                         if(iic.gt.4) iic=iic-4
                         iicn=iic+1
                         if(iicn.gt.4) iicn=iicn-4
                         call pcorner(iic,iicn,ipp,xp,yp,nspts,
     *                   nshore,uxmax,uxmin,uymax,uymin,8)
                       enddo
                     endif
                   endif
                 endif
               endif
c
c              Starting point to close polygon if needed
c
               if(xp(ipp-1).ne.xp(istart(nshore)).or.yp(ipp-1).ne.
     *           yp(istart(nshore))) then
                 xp(ipp)=xp(istart(nshore))
                 yp(ipp)=yp(istart(nshore))
                 if(ipp.LT.mxcoastp) then
                    ipp=ipp+1
                    nspts(nshore)=nspts(nshore)+1
                 else
                    lpnts=.true.
                    goto 999
                 endif
               endif
c               
c              Now establish mins and maxs
c
               do j=istart(nshore),ipp-1
                 pxmax(nshore)=max(pxmax(nshore),xp(j))
                 pxmin(nshore)=min(pxmin(nshore),xp(j))
                 pymax(nshore)=max(pymax(nshore),yp(j))
                 pymin(nshore)=min(pymin(nshore),yp(j))
               enddo
c
c           End for this GSHHS polygon
c
            endif
c
c     End of polygon loop
c
      enddo Polygons
c
99    continue
c
c     Final sanity check on a surrounding polygon.  If and only if the
c     first polygon is a surrounding polygon it will have 5 vetices.
c     Due to the hierarchical nature of the polygons the type should
c     be 1 less than the next polygon.
c
      if(nshore.gt.1.and.itypes(2)-itypes(1).ne.1.and.nspts(1).eq.5)
     *  itypes(1)=itypes(2)-1
c
c     Open and write the BLN file if coasts were found
c
      if(nshore.eq.0) then
        write(iolst,*)
        write(iolst,*) 'Note: no shorelines found within zone'
        write(iolst,*) '      BLN file not created'
        write(iolst,*)
        write(*,*)
        write(*,*) 'Note: no shorelines found within zone'
        write(*,*) '      BLN file not created'
        write(*,*)
      else
        write(iolst,*)
        write(iolst,*) 'Note: # shoreline polygons found: ',nshore
        write(iolst,*)
        write(*,*)
        write(*,*) 'Note: # shoreline polygons found: ',nshore
        write(*,*)
        open(unit=iobln,file=coastbln)
        do i=1,nshore
          itbnl=0 
          if(mod(itypes(i),2).eq.0) itbnl=1
          write(iobln,87) nspts(i),itbnl,itypes(i),isourcep(i),
     &                    ipolyid(i)
          iend=istart(i)-1+nspts(i)
          do j=istart(i),iend 
            xout=min(max(xp(j),xllk-.0001),xurk+.0001)
            yout=min(max(yp(j),yllk-.0001),yurk+.0001)
            write(iobln,fmt="(f12.6,',',f12.6)") xout,yout
          enddo
        enddo
87      format(i7,',',i1,',"',i1,',',i1,',',i6,'"')
        write(iolst,*)
        write(iolst,*) 'BLN file created: ',coastbln
        write(iolst,*)
        close(iobln)
      endif
      return

c --- Report any problem with array sizes and quit
999   if(LPNTS) then
        write(iolst,*)
        write(iolst,*) 'ERROR: Number of shoreline points exceeds'
        write(iolst,*) '      MXCOASTP = ',mxcoastp
        write(iolst,*) 'Increase parameter MXCOASTP and recompile'
        write(iolst,*)
        write(iolst,*) '      BLN file not created'
        write(iolst,*)
        write(*,*)
        write(*,*) 'ERROR: Number of shoreline points exceeds'
        write(*,*) '      MXCOASTP = ',mxcoastp
        write(*,*) 'Increase parameter MXCOASTP and recompile'
        write(*,*) 'Halted in COASTEXT'
        write(*,*)
        write(*,*) '      BLN file not created'
        write(*,*)
      endif
      if(LPOLY) then
        write(iolst,*)
        write(iolst,*) 'ERROR: Number of shorelines exceeds'
        write(iolst,*) '      MXCOAST = ',mxcoast
        write(iolst,*) 'Increase parameter MXCOAST and recompile'
        write(iolst,*)
        write(iolst,*) '      BLN file not created'
        write(iolst,*)
        write(*,*)
        write(*,*) 'ERROR: Number of shorelines exceeds'
        write(*,*) '      MXCOAST = ',mxcoast
        write(*,*) 'Increase parameter MXCOAST and recompile'
        write(*,*) 'Halted in COASTEXT'
        write(*,*)
        write(*,*) '      BLN file not created'
        write(*,*)
      endif
      stop

      end
c-----------------------------------------------------------------------
      integer function iside(x,y,xmx,xmn,ymx,ymn)
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682           Level: 040921             ISIDE
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     ISIDE decides to which side of the polygon a point is 
c              nearest.  Sides are numbered clockwise 1-4 starting on
c              the west side, going around to south. 
c
c --- Updates
c        Copied from TERREL Ver. 3.58_draft Level 040831
c        Ver. 3.55_draft Level 040305 to Ver. 3.56_draft Level 040311
c              - Check for inside vs outside point
c
c ARGUMENTS:
c    PASSED:  x,y        - point location
c             x,y(mx,mn) - grid limits for the polygon
c
c  RETURNED:  iside      - the side for this point
c
c CALLING ROUTINES:   COASTEXT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Local variables
      real x,y,xmx,xmn,ymx,ymn,difw,dife,difs,difn,difsn,difew
      difw=abs(x-xmn)
      dife=abs(x-xmx)
      difs=abs(y-ymn)
      difn=abs(y-ymx)
      difsn=min(difs,difn)
      difew=min(dife,difw)
c
c --- Check if the point is clearly north or south
c
      if(x.gt.xmn.and.x.lt.xmx.and.(y.le.ymn.or.y.ge.ymx)) then
        iside=2
        if(difs.lt.difn) iside=4
c
c --- Otherwise, check if clearly west or east
c
      elseif(y.gt.ymn.and.y.lt.ymx.and.(x.le.xmn.or.x.ge.xmx)) then
        iside=1
        if(dife.lt.difw) iside=3
c
c --- Off a corner or inside - decide on closest side
c
      else
c
c ---   Inside?
c
        if(x.ge.xmn.and.x.le.xmx.and.y.ge.ymn.and.y.le.ymx) then
          if(difsn.gt.difew) then
            iside=1
            if(dife.lt.difw) iside=3
          else
            iside=2
            if(difs.lt.difn) iside=4
          endif
c
c ---   Outside
c
        else
          if(difsn.lt.difew) then
            iside=1
            if(dife.lt.difw) iside=3
          else
            iside=2
            if(difs.lt.difn) iside=4
          endif
        endif
c
      endif
c
      return
      end
c      
c-----------------------------------------------------------------------
      subroutine pcorner(side1,side2,ipp,xp,yp,nspts,nshore,xmax,
     *  xmin,ymax,ymin,icall)
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682           Level: 040921        PCORNER
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     PCORNER adds the corners of the grid area to a polygon 
c              to facilitate closing the polygon, unless the corner is 
c              redundant, in which case it removes the redundancy.
c
c --- Updates
c             Copied from TERREL Ver. 3.58_draft Level 040831
c
c ARGUMENTS:
c    PASSED:  side1, side2 - sides of the polygon
c             ipp          - counter for total number of points (+1)
c             xp,yp        - arrays containing polygon points
c             nspts        - array of number of points in each polygon
c             nshore       - current number of polygons
c             x,y(max,min) - limits of grid area
c             icall        - calling occurrence in COASTEXT
c
c  RETURNED:  updated ipp, xp, yp, nspts 
c
c CALLING ROUTINES:   COASTEXT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Local variables
      integer side1,side2,ipp,nspts(*),nshore,iprod
      real xp(*),yp(*),xmax,xmin,ymax,ymin
c      IF(ICALL.EQ.2) THEN
c        iolst=16
c        write(iolst,*) ' NSHORE = ',nshore
c        write(iolst,*) ' NSPTS = ',(NSPTS(KKK),KKK=1,NSHORE)
c        WRITE(IOLST,*) (XP(KKK),YP(KKK),KKK=1,IPP-1)
c      ENDIF
c
c     sanity check - sides must be 1-4
c
      if(min(side1,side2).lt.1.or.max(side1,side2).gt.4.or.
     *  side1.eq.side2) then
        write(*,*) ' Corner problem for coastal polygons'
        write(*,*) '  Side out of range'
        write(*,*) ' Side 1 - ',side1,'  Side 2 - ',side2
        write(*,*) '   Call # ',icall
        stop
      endif
c
c     choose the corner to add based on the product of the sides
c
      iprod=side1*side2
      if(iprod.eq.2) then
        xp(ipp)=xmin
        yp(ipp)=ymax
      elseif(iprod.eq.6) then
        xp(ipp)=xmax
        yp(ipp)=ymax
      elseif(iprod.eq.12) then
        xp(ipp)=xmax
        yp(ipp)=ymin
      elseif(iprod.eq.4) then
        xp(ipp)=xmin
        yp(ipp)=ymin
      else
        write(*,*) ' Sides problem in PCORNER'
        write(*,*) ' Side 1 - ',side1,'  Side 2 - ',side2
        write(*,*) '   Call # ',icall
        stop
      endif
c
c     check for redundant corner, and remove if present
c
      if(ipp.gt.1) then
        if(xp(ipp).eq.xp(ipp-1).and.yp(ipp).eq.yp(ipp-1)) then
          ipp=ipp-1
          nspts(nshore)=nspts(nshore)-1
          return
        endif
      endif
c
c     otherwise, add the corner and update counters
c
      ipp=ipp+1
      nspts(nshore)=nspts(nshore)+1
      return
      end
c
c-----------------------------------------------------------------------
      subroutine breadh(il1,il2,il3,il4,il5,il6,il7,il8,is1,is2,
     &  iogshhs,leof,lbigendian)
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682           Level: 041013            BREADH
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     BREADH reads the binary header of a GSHHS polygon and
c              returns the information.  If the current machine is
c              "LITTLE-ENDIAN", it carries out byte-switching to
c              transform the data from the "BIG-ENDIAN" format. 
c
c --- Updates
c        Copied from TERREL Ver. 3.61 Level 041013
c
c        Version 3.61 Level 041013 from Version 3.60 Level 040228
c           - Add variable il9 to test for version of GSHHS file, its
c             value will be 3 for most the recent GSHHS Version 1.3 (040928).
c             The change is limited to this subroutine
c           - Change eof test from count to err condition
c
c ARGUMENTS:
c    PASSED:  lbigendian   - logical flag to indicate if current machine
c                            is "BIG-ENDIAN" or not
c             iogshhs      - unit number for GSHHS file
c
c
c  RETURNED:  il1-il8      - the 8 long (4-byte) integers from the header
c             is1-is2      - the 2 short (2-byte) integers from the header
c             leof         - logical flag indicating end of file
c
c CALLING ROUTINES:   COASTEXT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Local variables
c      
      integer il1,il2,il3,il4,il5,il6,il7,il8,il9,ill1,ill2,ill3,ill4,
     * ill5,ill6,ill7,ill8,ill9
      integer*2 is1,is2,ils1,ils2
      integer*1 isl1(4),isl2(4),isl3(4),isl4(4),isl5(4),isl6(4),
     * isl7(4),isl8(4),isl9(4),iss1(2),iss2(2)
      logical leof,lbigendian,lver3
      data lver3/.true./
      integer ilast/-1/
c      
c     Equivalences to allow byte-by-byte access
c
      equivalence (ils1,iss1(1)),(ils2,iss2(1)),(ill1,isl1(1)),
     * (ill2,isl2(1)),(ill3,isl3(1)),(ill4,isl4(1)),(ill5,isl5(1)),
     * (ill6,isl6(1)),(ill7,isl7(1)),(ill8,isl8(1)),(ill9,isl9(1))
c      
c     If current machine is big-endian, read the data directly.
c     If not (eg. Intel & DEC), read in byte-by-byte to reverse order
c     and copy values to return arguments.
c
1     if(lbigendian) then
c
c       For reads, check the version number of the GSHHS file, and change
c       the read of the variables in the header if the version is not 3.
c
        if(lver3) then
          read(iogshhs,err=99) il1,il2,il3,il4,il5,il6,il7,il8,
     *      il9,is1,is2
        else
          read(iogshhs,err=99) il1,il2,il3,il4,il5,il6,il7,il8,
     *      is1,is2
        endif  
      else
        if(lver3) then
          read(iogshhs,err=99) isl1(4),isl1(3),isl1(2),isl1(1),
     *      isl2(4),isl2(3),isl2(2),isl2(1),
     *      isl3(4),isl3(3),isl3(2),isl3(1),
     *      isl4(4),isl4(3),isl4(2),isl4(1),
     *      isl5(4),isl5(3),isl5(2),isl5(1),
     *      isl6(4),isl6(3),isl6(2),isl6(1),
     *      isl7(4),isl7(3),isl7(2),isl7(1),
     *      isl8(4),isl8(3),isl8(2),isl8(1),
     *      isl9(4),isl9(3),isl9(2),isl9(1),
     *      iss1(2),iss1(1), 
     *      iss2(2),iss2(1) 
        else
          read(iogshhs,err=99) isl1(4),isl1(3),isl1(2),isl1(1),
     *      isl2(4),isl2(3),isl2(2),isl2(1),
     *      isl3(4),isl3(3),isl3(2),isl3(1),
     *      isl4(4),isl4(3),isl4(2),isl4(1),
     *      isl5(4),isl5(3),isl5(2),isl5(1),
     *      isl6(4),isl6(3),isl6(2),isl6(1),
     *      isl7(4),isl7(3),isl7(2),isl7(1),
     *      isl8(4),isl8(3),isl8(2),isl8(1),
     *      iss1(2),iss1(1), 
     *      iss2(2),iss2(1) 
        endif
c       Copy values to dummy arguments
        il1=ill1
        il2=ill2
        il3=ill3
        il4=ill4
        il5=ill5
        il6=ill6
        il7=ill7
        il8=ill8
        il9=ill9
        is1=ils1
        is2=ils2
      endif
      ilast=il1
c
c     Test the GSHHS version number - for Version 1.3, il9 must equal 3
c
      if(il9.ne.3) then
        lver3=.false.
        il9=3
        ill9=3
        write(*,55)
55      format(/' Note : GSHHS version is obsolete - update ASAP'//)
        rewind (unit=iogshhs)
        goto 1
      endif
      return
c
c     Test if this is the last polygon or if there is actually an EOF
c     before any polygons have been read - in this case the filename is
c     wrong
c
99    leof=.true.
      if(ilast.lt.0) then
        write(*,*) ' GSHHS file not found or invalid '
        write(*,*) '  - check path and file name'
        stop
      endif
      return
      end
c      
c-----------------------------------------------------------------------
      subroutine breadp(xp1,yp1,lbigendian,iogshhs)
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682           Level: 040921            BREADP
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     BREADH reads the binary x-y values of a point of a GSHHS
c              polygon, scales the results, and returns the information.
c              If the current machine is "LITTLE-ENDIAN", it carries 
c              out byte-switching to transform the data from the 
c              "BIG-ENDIAN" format prior to scaling. 
c
c --- Updates
c             Copied from TERREL Ver. 3.56_draft Level 040831
c
c ARGUMENTS:
c    PASSED:  lbigendian   - logical flag to indicate if current machine
c                            is "BIG-ENDIAN" or not
c             iogshhs      - unit number for GSHHS file
c
c
c  RETURNED:  xp1, yp1     - the latitude and longitude of the point in
c                            decimal degrees
c
c CALLING ROUTINES:   COASTEXT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Local variables
c      
      logical lbigendian
      real xp1,yp1
      integer il1,il2
      integer*1 isl1(4),isl2(4)
c      
c     Equivalences to allow byte-by-byte access
c
      equivalence (il1,isl1(1)),(il2,isl2(1))
c      
c     If current machine is big-endian, read the data directly.
c     If not (INTEL&DEC), read in byte-by-byte to reverse order.
c
      if(lbigendian) then
        read(iogshhs) il1,il2
      else
        read(iogshhs) isl1(4),isl1(3),isl1(2),isl1(1),
     *    isl2(4),isl2(3),isl2(2),isl2(1)
      endif
c
c     Covert from millionths to whole degrees
c
      xp1=real(il1)/1.e6
      yp1=real(il2)/1.e6
c      
      return
      end
c-----------------------------------------------------------------------
      subroutine coastrd
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682           Level: 040921           COASTRD 
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     COASTRD reads already-extracted polygons of water/land
c              interfaces from an existing BLN file, coming either
c              from a previous TERREL run or a previous CTGPROC run,
c              passing these into the SHORES common for subsequent
c              use to evaluate crude type for points and grid cells.
c              The types are: 0-ocean, 1-mainland and marine islands,
c              2-lakes, 3-islands in lakes, 4-ponds on islands. 
c
c --- Updates
c --- Copied from TERREL Ver. 3.58_draft Level 040831
c --- Ver 3.56_draft Level 040311 to Ver 3.58_draft Level 040831  KAM
c              - Relax testing of BLN limits to be more realistic 
c --- Ver 3.54_draft Level 040303 to Ver 3.56_draft Level 040311  KAM
c              - Add polygon source and id to BLN identifier field
c              - Change name of BLN read logical
c --- Ver 3.53_draft Level 040302 to Ver 3.54_draft Level 040303  KAM
c              - Close the BLN file before exiting
c
c ARGUMENTS:
c    PASSED:  /FILNAM/    file names
c             /GRID/      grid min-max
c            
c  RETURNED:  /SHORES/    shoreline data
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include parameter and common files
c
      include 'params.ctg'
      include 'shores.ctg'
      include 'filnam.ctg'
      include 'grid.ctg'
c
c --- Local variables
c
      character*10 cptype
      integer np,iiside,iend
c
c --- Open the BLN file
c
      open(unit=iobln,file=coastbln)
c
c --- Loop through the polygons
c
      nshore=0
      iend=0
      do
c
c ---   Read the header for the polygon and decode polygon type
c
        read(iobln,*,end=99) np,iiside,cptype
        nshore=nshore+1
        nspts(nshore)=np
        istart(nshore)=iend+1
        iend=iend+np
        read(cptype,*) itypes(nshore),isourcep(nshore),ipolyid(nshore)
c
c ---   Read the points for the polygon into the arrays
c
        do i=istart(nshore),iend
          read(iobln,*) xp(i),yp(i)
          pxmax(nshore)=max(pxmax(nshore),xp(i))
          pymax(nshore)=max(pymax(nshore),yp(i))
          pxmin(nshore)=min(pxmin(nshore),xp(i))
          pymin(nshore)=min(pymin(nshore),yp(i))
        enddo
c
      enddo
c
c --- EOF - Check that data were actually found
c
99    if(nshore.EQ.0) then
        write(*,*) ' No coasts found in ',coastbln
        write(*,*) ' Check path, and if this is really a continuation'
        stop
      endif
c
c --- Check that polygons are within the grid - otherwise
c     the BLN file is not for this grid, so terminate
c
      xxmxx=-1.e30
      xxmnn=1.e30
      yymxx=-1.e30
      yymnn=1.e30
      do i=1,nshore
        xxmxx=max(xxmxx,pxmax(i))
        xxmnn=min(xxmnn,pxmin(i))
        yymxx=max(yymxx,pymax(i))
        yymnn=min(yymnn,pymin(i))
      enddo
      if(xxmxx.LT.xllk .OR. xxmnn.GT.xurk .OR.
     *   yymxx.LT.yllk .OR. yymnn.GT.yurk) then
         write(*,*) ' Data limits in BLN file are not consistent with',
     *              ' the grid limits'
         write(*,*) ' Change LBLNREAD to .FALSE. and re-run'
         stop
      endif

      close(iobln)
      return
      end
c-----------------------------------------------------------------------      
      integer function iptype(px,py)
c-----------------------------------------------------------------------
c
c --- CTGPROC   Version: 2.682           Level: 040921            IPTYPE
c               K. Morrison, Earth Tech
c
c
c PURPOSE:     IPTYPE checks an individual point to see if it falls
c              within polygons extracted from the GSHHS full-resolution
c              data set.  The polygons are hierarchical - i.e. nested.
c              Therefore, the highest value for the type of any
c              polygon containing the point is the value for that
c              point.  IPTYPE returns one of five values:
c              0 - ocean (default value - there are no ocean polygons)
c              1 - mainland or marine islands
c              2 - lakes
c              3 - islands within lakes
c              4 - ponds on islands
c              Note that a default value of 0 is returned if the point
c              doesn't fall within a polygon, unless there are no
c              polygons in which case a value of 1 is returned.
c
c --- Updates
c         Copied from TERREL Ver. 3.58_draft Level 040831
c         Ver. 3.51_draft Level 040228 to Ver. 3.52_draft Level 040229
c              - Remove defaulting to first land type since COASTEXT now
c                explicitly defines the surrounding polygon if the zone
c                is surrounded without a coastline intersection
c              - Add defaulting to land type if no shorelines were
c                processed or found
c
c ARGUMENTS:
c    PASSED:  PX, PY real    - X-Y coordinates of the point
c             /SHORES/       - shoreline data extracted from GSHHS
c
c  RETURNED:  IPTYPE integer - water or land crude type, 0-4
c
c
c CALLING ROUTINES:   TERREL
c
c EXTERNAL ROUTINES:  PNPOLY
c-----------------------------------------------------------------------
c      
c --- Include common
c
      include 'params.ctg'
      include 'control.ctg'
      include 'shores.ctg'
c
c --- Default type to ocean, unless no shoreline processing done in
c     which case default to land
c
      iptype=0
      if(.NOT.lcoast) then
        write(*,*) 'No coastal processing'
        iptype=1
        return
      endif
c      
c --- Loop through all polygons within the study zone
c
      do i=1,nshore
c        
c   --- First, skip any polygons that can't contain the point or that
c       won't change the type of point
c
        if((px.lt.pxmin(i).or.px.gt.pxmax(i).or.py.lt.pymin(i).or.        
     *    py.gt.pymax(i)).or.iptype.ge.itypes(i)) cycle   
c     
c   --- Now check if the point falls within this ploygon
c
        call pnpoly(px,py,xp(istart(i)),yp(istart(i)),nspts(i),inout)
c
c       Get polygon type
c
c   --- Outside of polygon - no change        
c
        if(inout.eq.-1) cycle
c
c   --- On edge of polygon - set to water
c
        if(inout.eq.0) then
          if(itypes(i).eq.1) iptype=0
          if(itypes(i).eq.2) iptype=2
          if(itypes(i).eq.3) iptype=2
          if(itypes(i).eq.4) iptype=4
c        
c   --- Inside of polygon - retain max of current type and polygon type
c
        else
          iptype=max(iptype,itypes(i))
        endif
c
c --- End of loops over polygons
c
      enddo
      return
      end
c-----------------------------------------------------------------------
      SUBROUTINE PNPOLY (PX,PY,X,Y,N,INOUT)
c-----------------------------------------------------------------------
C
C     Copied from ISCST3
c-----------------------------------------------------------------------
C
C     Courtesy: Jay Sandhu
C               email: jsandhu@esri.com
C
C
C Please cite David H. Douglas, COLLECTED ALGORITHMS, Cambridge MA:
C Harvard Laboratory for Computer Graphics, 1974
C
C This is my reinvention buster.
C 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974
C
C>>>PNPY
C     .................................................................
C
C----------------------------------------------------------------------
C
C        SUBROUTINE PNPOLY
C
C        PURPOSE
C           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
C
C        USAGE
C           CALL PNPOLY (PX, PY, X, Y, N, INOUT )
C
C        DESCRIPTION OF THE PARAMETERS
C           PX      - X-COORDINATE OF POINT IN QUESTION.
C           PY      - Y-COORDINATE OF POINT IN QUESTION.
C           X       - N LONG VECTOR CONTAINING X-COORDINATES OF
C                     VERTICES OF POLYGON.
C           Y       - N LONG VECTOR CONTAINING Y-COORDINATES OF
C                     VERTICES OF POLYGON.
C           N       - NUMBER OF VERTICES IN THE POLYGON.
C           INOUT   - THE SIGNAL RETURNED:
C                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
C                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
C                      1 IF THE POINT IS INSIDE OF THE POLYGON.
C
C        REMARKS
C           THE VERTICES MAY BE LISTED IN CLOCKWISE OR ANTICLOCKWISE
C           ORDER.  FOR THIS SUBROUTINE A POINT IS CONSIDERED INSIDE
C           THE POLYGON IF IT IS LOCATED IN THE ENCLOSED AREA DEFINED
C           BY THE LINE FORMING THE POLYGON.
C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
C           PNPOLY CAN HANDLE ANY NUMBER OF VERTICES IN THE POLYGON.
C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 6/72.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           A VERTICAL SEMI-INFINITE LINE IS DRAWN UP FROM THE POINT
C           IN QUESTION. IF IT CROSSES THE POLYGON AN ODD NUMBER OF
C           TIMES, THE POINT IS INSIDE THE POLYGON.
C
C     .................................................................
C

      REAL X(N),Y(N)
      LOGICAL IX,IY,JX,JY,EOR

C     EXCLUSIVE OR FUNCTION.
      EOR(IX,IY)=(IX.OR.IY).AND..NOT.(IX.AND.IY)

      INOUT=-1
      DO 4 I=1,N
         XI=X(I)-PX
         YI=Y(I)-PY
C        CHECK WHETHER THE POINT IN QUESTION IS AT THIS VERTEX.
         IF (XI.EQ.0.0.AND.YI.EQ.0.0) GO TO 2
C        J IS NEXT VERTEX NUMBER OF POLYGON.
         J=1+MOD(I,N)
         XJ=X(J)-PX
         YJ=Y(J)-PY
C        IS THIS LINE OF 0 LENGTH ?
         IF (XI.EQ.XJ.AND.YI.EQ.YJ) GO TO 4
         IX=XI.GE.0.0
         IY=YI.GE.0.0
         JX=XJ.GE.0.0
         JY=YJ.GE.0.0
C        CHECK WHETHER (PX,PY) IS ON VERTICAL SIDE OF POLYGON.
         IF (XI.EQ.0.0.AND.XJ.EQ.0.0.AND.EOR(IY,JY)) GO TO 2
C        CHECK WHETHER (PX,PY) IS ON HORIZONTAL SIDE OF POLYGON.
         IF (YI.EQ.0.0.AND.YJ.EQ.0.0.AND.EOR(IX,JX)) GO TO 2
C        CHECK WHETHER BOTH ENDS OF THIS SIDE ARE COMPLETELY 1) TO RIGHT
C        OF, 2) TO LEFT OF, OR 3) BELOW (PX,PY).
         IF (.NOT.((IY.OR.JY).AND.EOR(IX,JX))) GO TO 4
C        DOES THIS SIDE OBVIOUSLY CROSS LINE RISING VERTICALLY FROM (PX,PY
         IF (.NOT.(IY.AND.JY.AND.EOR(IX,JX))) GO TO 1
         INOUT=-INOUT
         GO TO 4

1        IF ((YI*XJ-XI*YJ)/(XJ-XI)) 4,2,3
2        INOUT=0
         RETURN
3        INOUT=-INOUT
4     CONTINUE

      RETURN
      END

