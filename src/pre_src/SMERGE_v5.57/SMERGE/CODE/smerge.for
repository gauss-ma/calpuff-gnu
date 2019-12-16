c----------------------------------------------------------------------
c --- SMERGE -- Surface Meteorological Preprocessor
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 070627                MAIN
c
c --- Written by:  E. Insley, J. Scire
c 
c     Copyright (c) 1999-2007 by Exponent, Inc.
c
c --- Reads 'N' CD144, SAMSON, HUSWO, ISHWO, TD3505, or TD9956 hourly
c --- surface observations files containing data for one station each and  
C --- reformats the data into 1 file containing 'N' stations sorted by 
C --- time.  Formatted files may also be combined with an already existing
c --- SMERGE (SURF.DAT) file.  The output file may be formatted or
c --- unformatted.  If the output SURF.DAT file is unformatted, it may
c --- be either in a packed (compressed) or unpacked form.
c --- Optionally it can also output a VSRN.DAT file of present weather
c --- codes and visibilty for subsequent use in CALPOST for visibility
c --- calculations using Method 7.
c
c----------------------------------------------------------------------
c --- Model Change Bulletin Updates Included:           MCB-A (040716)
c----------------------------------------------------------------------
c
c --- Updated V5.57(070627) from V5.56(050324) (D. Strimaitis)
c     - Updated CALUTILS (Version 2.55, Level 070327)
c         Modify search for '=' in READIN to allow
c           for blanks between c*12 variable name and the '='
c           sign (internal blanks are not removed after V2.2)
c         Replace filename strings c*70 with c*132
c         Allow for spaces within pathnames by adding new TLEFT
c           and TRIGHT trim subroutines
c     - Filnames changed from c*70 to c*132 (CALUTILS V2.3 and later)
c       Modified:  FILNAM.SMG
c                  READCF
c
c --- Updated V5.56(050324) from V5.55(050311)
c     - Revise time-window in READISH so that only reported times that
c       fall within the hour (HH-1)01 to HH00 are assigned to hour HH.
c       For example, observations at 1701, 1725, 1749, and 1800 are all
c       assigned to "hour 18".  Subsequent processing will use the
c       latest non-missing data for the hour so that "standard" reports
c       on the hour will take precedence.          (D. Strimaitis)
c
c --- Updated V5.55(050311) from V5.54(041102)
c     - Updated logic in READISH that overstated the number of missing
c       hours.                                     (D. Strimaitis)
c
c --- Updated V5.54(041102) from V5.53(041029)
c     - Fixed bug in RDWRITS that assigned the wrong station
c       elevation array element when adding stations to an
c       existing SURF.DAT file.                    (D. Strimaitis)
c
c --- Updated V5.53(041029) from V5.52(041026)
c     - Modified End-of-File response after call to GETISH to continue
c       processing valid data on the last record of a file.  Missing
c       station pressure on the last record was not calculated in 
c       previous version (JDAT=5,6,7).             (D. Strimaitis)
c
c --- Updated V5.52(041026) from V5.51(040922)
c     - Added station ID check (the ID provided in the control file for
c       each station file processed must match the ID found in the
c       corresponding file.                         (D. Strimaitis)
c     - Disable the VSRN output option pending revisions
c                                                   (D. Strimaitis)
c     - Fixed bug in the format for reading TD3505 and TD9956 data
c       (types 6 & 7) in READISH                    (K. Morrison)
c     - Fixed bug in call to GETISH in RDWRITS that used the wrong
c       station time zone array element when adding stations to an
c       existing SURF.DAT file.                     (K. Morrison)
c     - Require time zone XSTZ entered in control file to be 0 for
c       ISHWO, TD3505, and TD9956                   (K. Morrison)
c
c --- Updated V5.51(040922) from V5.5_draft(040709) (K. Morrison)
c     - Modifications in RDWRITS
c       - Correction to empirical calculations for pressure
c       - Remove substitution of total cloud cover for missing opaque
c         for CD144
c       - Correction to station identification for SAMSON and HUSWO
c       - Correction to precip codes for SAMSON
c       - Identify HUSWO as English or metric units based on pressure
c         value, and adjust unit conversions in consequence
c     - Change dimension of ncbdf from 8 to 7 in WRS
c     - Add control file variable IHUSWO to designate either English or
c       metric data units in HUSWO file (default=English).  Keep logic
c       to determine likely units from data ranges, but stop if it
c       appears that units are wrong.                            (DGS)
c
c --- Updated V5.5_draft(040709) from V5.44(040621) (K. Morrison)
c     - Add TD3505 and TD9956 data types (types 6 & 7)
c     - Get present weather codes, and standardize to general manual
c       DATSAV station codes
c     - Read visibility for all data types
c     - Add option to output a VSRN.DAT file
c
c --- Updated V5.44(040621) from V5.43(040322) (K. Morrison)
c     - Utility programs added in separate file (PRESUTIL.FOR V:1.00)
c       for conversions among pressure variables (station, altimeter, 
c       sea-level), called from RDWRITS
c     - Added reading of dew point for all data types and sea-level
c       pressure for CD-144 in RDWRITS
c     - Added ability to calculate pressure from sea-level pressure for
c       CD-144 data in subroutine RDWRITS
c     - Added ability to calculate humidity from dry-bulb and dew point
c       for data types other than ISHWO in RDWRITS (already done for this 
c       type in READISH)
c     - Moved pressure calculation from READISH to RDWRITS for ISHWO data,
c       and added passing of altimeter and sea-level back to RDWRITS
c     - Added new logical to CONTROL common and Input Group 1 (LPCALC) to 
c       choose replacement of missing pressure values with calculated 
c       values based on sea-level pressure - this variable automatically 
c       enabled for ISHWO files
c
c --- Updated V5.43(040322) from V5.42(040318) (K. Morrison)
c     - Correction to pressure estimation for ISHWO data
c
c --- Updated V5.42(040318) from V5.41(040210) (K. Morrison)
c     - Correction to CD-144 treatment of "0" wind direction
c     - Correction to ISHWO variable wind direction
c
c --- Updated V5.41(040210) from V5.4(040205) (K. Morrison)
c     - Corrections and enhancements to processing of ISHWO data
c
c --- Updated V5.4(040205) from V5.31(030528) (K. Morrison)
c     - Added processing of ISHWO data
c
c --- Updated V5.31(030528) from V5.3(030402) (D. Strimaitis)
c     - Updated CALUTILS (Version 2.2, Level 030528)
c
c --- Updated V5.3(030402) from V5.2(020828) (D. Strimaitis)
c     - Updated CALUTILS (Version 2.1, Level 030402)
c     - New header for output data file (SURF.DAT)
c             ENVPOP=0  Revised SURF.DAT header
c             ENVPOP=1  Revised SURF.DAT header with station locations
c
c --- Updated V5.2(020828) from V5.1(020809) (D. Strimaitis)
c     - Updated CALUTILS (Version 1.1, Level 020828)
c
c --- Updated V5.1(020809) from V5.0(020308) (D. Strimaitis)
c     - Add parameter ENVPOP to params.smg and use this to toggle
c       environment configurations (these will evolve in time):
c             ENVPOP=0  SURF.DAT header as in 1/2000 Users Guide
c             ENVPOP=1  SURF.DAT header with draft station locations
c                       introduced in V5.0(020308)
c
c --- Updated V5.0(020308) from V5.0(010630) (D. Strimaitis)
c     - Allow station ID to exceed 5 digits (use 10 for now in format
c       statements)
c     - Initialize data to missing at start of each hour
c     - New header format for SURF.DAT:  include SMERGE version, level,
c       station name, ID, anemometer height, and LAT/LON
c     - Read station information from text file
c     - Modified CD144 Format (JDAT=4): look for extended CD144 record
c       to obtain precip rate, and create a PRECIP.DAT output file
c
c --- Updated V5.0(010630) from V4.0(010315) (D. Strimaitis)
c     - Restructure inputs for CALPUFF system control file
c     - Restructure main program as subroutine COMP
c     - Place parameters and commons into include files
c     - Place system-wide utilities into an include module (calutil.for)
c
c --- Updated 010315 from 991223 (D. Strimaitis)
c     - Y2K treatment of year enforced
c
c --- Updated 991223 from 970718 (Benjamin de Foy)
c     - JDAT = 3: HUSWO format
c     - Count & print no. of hours with data missing for all stations
c     - Some restructuring in subroutine rdwrits
c     - Add 'backspace' command to deal with missing hours in input
c       files for all three formats
c     - Subroutine Clouds replaced - simpler + more robust
c     - Only col 25 of CD144 was used for pcodes, now uses cols 25-29
c     - Changed surf.dat formats statement - fewer digits precision
c
c --- Updated 7/18/97 from V4.0, L961113 to V4.0, L970718 (E. Insley)
c     - Fixed hour handling for CD-ROM data in SUBR. RDWRITS.  It had
c       subtracted one from the hour for all hours which incorrectly
c       shifted the data.  It now only changes hr 24 to hr 0 of the
c       next day without shifting the data.
c
c --- Updated 11/13/96 from V3.1, L961031 to V3.1, L961113 (E. Insley)
c     - Modified input structure to allow the user to name the SURF.DAT
c       output file and the previous SMERGE data file used as input
c     - Changed file names from Char*10 to Char*12
c
c --- Updated 10/31/96 from V3.1, L961014 to V3.1, L961031 (J. Scire)
c     - Add skip of two header records when using SAMSON format
c
c --- Updated 10/14/96 from V3.0, L941215 to V3.1, L961014 (J. Scire)
c     - Added QA checks to input variables,
c     - Added warning message to screen whenever a fatal,
c       program-generated error occurs
c     - Added addition description of inputs,
c     - Changed SURF.DAT status from "NEW" to "UNKNOWN"
c     - Changed SMERGE.LST status from unspecified to "UNKNOWN"
c     - Fixed error in DO 85 loop - checks for blank station ID field
c
c --- Updated by:  E. Insley, SRC  11/18/94
c   - Added option to read an existing 'SURF.DAT' file as either
c   - formatted or unformatted (previously it was only unformatted).
c   - Modified the MAIN program and Subroutines OPENS, RDWRITS and RDS.
c
c --- Updated by:  E. Insley, SRC  3/30/94
c   - Added option to create a formatted 'SURF.DAT' file.
c   - Modified the MAIN program and Subroutines OPENS, RDWRITS and WRHD.
c
c --- Updated by:  R. Mentzer, SRC  9/1/92
c   - Allow SMERGE to process both CD144 and SAMSON files.
c   - Allow missing data to pass through with a missing value indicator.
c   - Modified subroutines RDWRITS and CLOUDS.
c----------------------------------------------------------------------
      Program SMERGE
c
c --- Include parameters
      include 'params.smg'
c --- Include common blocks
      include 'qa.smg'
c
c --- Set version and level number of program
      ver='5.57'
      level='070627'
c
c --- SETUP PHASE -- read control file information
      call SETUP
c
c --- COMPUTATIONAL PHASE -- loop over processing periods
      call COMP
c
c --- TERMINATION PHASE -- program termination functions
      call FIN(2)
c
      stop
      end
c----------------------------------------------------------------------
      BLOCK DATA
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 040922          BLOCK DATA
c                D. Strimaitis, Earth Tech, Inc.
c
c --- Include parameter statements
      include 'params.smg'
c
c --- Include common blocks
      include 'filnam.smg'
      include 'control.smg'
      include 'qa.smg'

c --- FILNAM common block
      data runinp/'smerge.inp'/,runlst/'smerge.lst'/,
     1 prevdat/'prev.dat'/,surfdat/'surf.dat'/,
     2 sfcmet/'none.dat'/,sstatxt/'surf.txt'/,vsrndat/'vsrn.dat'/
c --- FILLOG common block
      data lcfiles/.true./

c --- CONTROL common block
      data nbstn/0/, inform/2/, ioform/2/, iopack/0/, jtxt/1/,
     1 lpcalc/.false./, lvsrnout/.false./
      data ihuswo/1/

c --- QA common block
      data model/'SMERGE      '/

      end

c----------------------------------------------------------------------
c --- BRING IN CALPUFF SYSTEM UTILITY SUBROUTINES
      include 'calutils.for'
      include 'presutil.for'
c----------------------------------------------------------------------

c----------------------------------------------------------------------
      subroutine setup
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57       Level: 020809                SETUP
c                D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:   perform all initialization and one-time setup
c                operations
c
c --- UPDATES:
c     V5.1(020809) from V5.0(010630) (D. Strimaitis)
c     - Use parameter ENVPOP to activate call to RDSSTA
c
c     Common block /FILNAM/
c        RUNINP
c
c --- Parameters used:
c        IO5, IO6, IOMESG, ENVPOP
c
c --- SETUP called by: MAIN
c --- SETUP calls:     DATETM, COMLINE, READCF, WRTHEAD
c                      
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.smg'

c --- Include common blocks
      include 'filnam.smg'
      include 'qa.smg'

c --- Get date and time from system
      call DATETM(rdate,rtime,rcpu)

c --- Get the name of the control file from the command line
      call COMLINE(runinp)

c --- Open the control file
      open(io5,file=runinp,status='old')

c --- Report progress
      write(iomesg,*)'SETUP PHASE'

c --- Check that the version and level number in the parameter
c --- file matches those in the code itself
      if(ver.ne.mver.or.level.ne.mlevel)then
         write(iomesg,10) ver,level,mver,mlevel
10       format(/1x,'ERROR in SUBR. SETUP -- The SMERGE version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open other files)
      call READCF

c --- Read station information file
      if(envpop.EQ.1) call RDSSTA

      return
      end
c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57       Level: 070627               READCF
c                D. Strimaitis
c
c --- PURPOSE:  Read the file containing the file names of the
c               input and output files of the run, and associated
c               control variables, place into program variables,
c               and QA information.
c
c --- UPDATES:
c
c --- V5.57(070627) from V5.52(041026) (D. Strimaitis)
c     - Filenames from c*70 to c*132 for CALUTILS V2.3 and later
c
c     V5.52(041026) from V5.51(040922)
c     - Require time zone XSTZ entered in control file to be 0 for
c       ISHWO, TD3505, and TD9956                   (K. Morrison)
c     - Disable the VSRN output option pending revisions
c                                                   (D. Strimaitis)
c
c     V5.51(040922) from V5.5_draft(040709) (D. Strimaitis)
c     - Add control file variable IHUSWO to designate either English or
c       metric data units in HUSWO file (default=English)
c
c     V5.5_draft(040709) from V5.44(040616) (K. Morrison)
c     - Add VSRN.DAT output option
c     - Add data types 6 & 7 for TD3505 and TD9956
c
c     V5.44(040616) from V5.41(040210) (K. Morrison)
c     - Add variable LPCALC to enable pressure estimation from sea 
c       level values when station pressure is missing for CD144 
c       (Input Group 1).  This variable is forced to be .true. for
c       ISHWO data
c
c     V5.41(040210) from V5.4(040205) (K. Morrison)
c     - Add station elevation for ISHWO processing
c
c     V5.4(040205) from V5.31(020809) (K. Morrison)
c     - Add ISHWO processing (file type 5)
c
c     V5.1(020809) from V5.0(010630) (D. Strimaitis)
c     - Use parameter ENVPOP to control surface station info
c
c     V5.0(020308) from V5.0(010630) (D. Strimaitis)
c     - Add station information file
c     - Use larger integer format when writing station ID
c     - Add JDAT=4 for extended CD144 record
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c
c        Parameters: IO5, IO6, IOMESG, MXVAR
c                    MXFF, MXSS, ENVPOP
c
c --- OUTPUT:
c
c ---    Common block /DATEHR/ variables:
c           ibdathr,iedathr
c ---    Common block /FILNAM/ variables:
c           prevdat,surfdat,runlst,sfcdat,cffiles,lcfiles
c ---    Common block /CONTROL/ variables:
c           iotz,ioform,iopack,inform,
c           jdat,ihuswo,jtxt,nff,istz(mxff),ifstn(mxff),
c           ibstn(mxss),nbstn,lprev          
c ---    Common block /STATION/ variables:
c           istz(mxff),ifstn(mxff)
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE, JULDAY, QAYR4, YR4, OPENS
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.smg'
      include 'params.cal'
c
c --- Include common blocks
      include 'control.smg'
      include 'datehr.smg'
      include 'filnam.smg'
      include 'qa.smg'
      include 'station.smg'
c
c --- Local variables
      real qastz(mxff)
      character*4 ctemp(132,5)
      character*12 cvdic(mxvar,4)
      integer ivleng(mxvar,4),ivtype(mxvar,4)
      logical lecho
      logical lerrcf
      character*44 units(2)

c --- Set control file error logical
      lerrcf=.FALSE.

c --- Set strings for reporting HUSWO data units
      data units/'(ENGLISH data units are expected in HUSWO)  ',
     &           '(METRIC data units are expected in HUSWO)   '/

c --- Set Dictionary
      data lecho/.false./
      data names/5/

      data cvdic/
     a  'PREVDAT','SURFDAT','RUNLST','SSTATXT', 
     a  'VSRNDAT','LCFILES','NFF',53*' ',
     b  'SFCMET','IFSTN','XSTZ','XELEV', 56*' ',
     c  'IBYR','IBMO','IBDY','IBHR','IEYR','IEMO','IEDY','IEHR','XBTZ',
     c  'LPREV','NBSTN','INFORM','IOFORM','IOPACK','JDAT','JTXT',
     c  'LPCALC','LVSRNOUT','IHUSWO',41* ' ',
     d  'IBSTN', 59*' '/

      data ivleng/
     a  5*132,2*1, 53*0,
     b  132,3*1, 56*0,
     c  19*1, 41*0,
     d  1, 59*0/

c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
      data ivtype/
     a  5*4,3,2, 53*0,
     b  4,2,1,1, 56*0,
     c  8*2,1,3,6*2,2*3,2, 41*0,
     d  2, 59*0/

c ------------------
c --- Input Group 0a
c ------------------

c --- Initialize the temporary arrays
      do i=1,names
         do j=1,132
            ctemp(j,i)(1:1)=' '
         enddo
      enddo

c --- Read the group data
       call READIN(cvdic(1,1),ivleng(1,1),ivtype(1,1),io5,iomesg,
     & lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),ctemp(1,5),
     2 lcfiles,nff,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum)

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')prevdat=' '
      if(ctemp(1,2)(1:1).ne.' ')surfdat=' '
      if(ctemp(1,3)(1:1).ne.' ')runlst=' '
      if(ctemp(1,4)(1:1).ne.' ')sstatxt=' '
      if(ctemp(1,5)(1:1).ne.' ')vsrndat=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')prevdat(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')surfdat(j:j)=ctemp(j,2)(1:1)
         if(ctemp(j,3)(1:1).ne.' ')runlst(j:j)=ctemp(j,3)(1:1)
         if(ctemp(j,4)(1:1).ne.' ')sstatxt(j:j)=ctemp(j,4)(1:1)
         if(ctemp(j,5)(1:1).ne.' ')vsrndat(j:j)=ctemp(j,5)(1:1)
      enddo

c --- Convert the file names to the proper case
      call FILCASE(lcfiles,prevdat)
      call FILCASE(lcfiles,surfdat)
      call FILCASE(lcfiles,runlst)
      call FILCASE(lcfiles,sstatxt)
      call FILCASE(lcfiles,vsrndat)

c --- Open listfile
      open(io6,file=runlst,status='unknown')

c --- Write banner to list file
      write(io6,5) ver,level
5     format(///,26x,'SMERGE OUTPUT SUMMARY',/,19x,'VERSION:  ',A8,
     1       ' LEVEL:  ',A8///)

c ------------------
c --- Input Group 0b
c ------------------

      do k=1,nff
c ---    Initialize the temporary arrays for the file name
         do j=1,132
            ctemp(j,1)(1:1)=' '
            sfcmet(j:j)=' '
         enddo

c ---    Set default elevation
         xelev=-9999.

c ---    Read the surface met station information
       call READIN(cvdic(1,2),ivleng(1,2),ivtype(1,2),io5,io6,lecho,
     1 ctemp(1,1),ifstnin,xstz,xelev,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum)

c ---    Transfer the char*4 data into the char*132 variable
         do j=1,132
            if(ctemp(j,1)(1:1).ne.' ')sfcmet(j:j)=ctemp(j,1)(1:1)
         enddo

c ---    Convert the file name to the proper case
         call FILCASE(lcfiles,sfcmet)

c ---    Place information in surface station arrays, up to MXFF
         if(k.LE.MXFF) then
            cffiles(k)=sfcmet
            ifstn(k)=ifstnin
            istz(k)=NINT(xstz)
            qastz(k)=xstz
            elev(k)=xelev
         endif

      enddo

c -----------------
c --- Input Group 1
c -----------------

      call readin(cvdic(1,3),ivleng(1,3),ivtype(1,3),io5,io6,lecho,
     1 IBYR,IBMO,IBDY,IBHR,IEYR,IEMO,IEDY,IEHR,XBTZ,
     2 LPREV,NBSTN,INFORM,IOFORM,IOPACK,JDAT,JTXT,LPCALC,LVSRNOUT,
     3 IHUSWO,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum)

c --- Special Y2K QA on starting year of simulation
      call QAYR4(io6,ibyr,0,ierr)
      if(ierr.NE.0) then
         write(io6,*) 'READCF:  Error in Input Group 1'
         lerrcf=.TRUE.
      endif

c --- Make sure ending year is YYYY (Y2K)
      call YR4(io6,ieyr,ierr)
      if(ierr.NE.0) then
         write(io6,*) 'READCF:  Error in Input Group 1'
         lerrcf=.TRUE.
      endif

c -----------------
c --- Input Group 2
c -----------------

c --- Condition NBSTN to be consistent with LPREV
      if(.not.LPREV) nbstn=0

      do k=1,nbstn
         kk=MIN(k,mxss)
         call readin(cvdic(1,4),ivleng(1,4),ivtype(1,4),io5,io6,lecho,
     1 IBSTN(kk),
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
      enddo

c -------------------------------------------------
c --- Translate selected inputs to SMERGE variables
c -------------------------------------------------

      iotz=NINT(xbtz)

c --- Set date-time variables
      call JULDAY(io6,ibyr,ibmo,ibdy,ibjuldy)
      call JULDAY(io6,ieyr,iemo,iedy,iejuldy)
c --- Create date-time parameters (YYYYJJJHH)
      ibdathr = ibyr * 100000 + ibjuldy * 100 + ibhr
      iedathr = ieyr * 100000 + iejuldy * 100 + iehr

c ---------------------
c --- Perform QA checks
c ---------------------

c --- Test for valid NFF
      if(nff.GT.mxff) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 0a'
         write(io6,*) 'NFF exceeds the parameter MXFF '
         write(io6,*) 'NFF, MXFF = ',nff,mxff
         write(io6,*) 'Increase MXFF in PARAMS.SMG and recompile'
         lerrcf=.TRUE.
      endif

c --- Test for integer time zone (code for half-zones not available)
      kff=MIN(nff,mxff)
      do k=1,kff
         test=istz(k)-qastz(k)
         if(ABS(test).GE.0.1) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 0b'
            write(io6,*) 'Fractional time zone found: XSTZ= ',qastz(k)
            write(io6,*) 'SMERGE is designed to use integer time zones'
            lerrcf=.TRUE.
         endif
         if(istz(k).LT.-12 .OR. istz(k).GT.12) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 0b'
            write(io6,*) 'Invalid station time zone = ',istz(k)
            write(io6,*) ' (-12 <= ISTZ <= +12) '
            lerrcf=.TRUE.
         endif
c ---    Test for time zone other than 0 for UTC files
c        (ISHWO, TD3505, TD9956)
         if(JDAT.GE.5 .AND. JDAT.LE.7 .AND. istz(k).NE.0) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 0b'
            write(io6,*) 'Non-zero time zone found: XSTZ= ',istz(k)
            write(io6,*) 'ISHWO, TD3505, and TD9956 are time zone 0'
            lerrcf=.TRUE.
         endif
      enddo

c --- Test for Time zone of output data (IOTZ)
c --- Test for integer time zone (code for half-zones not available)
      test=iotz-xbtz
      if(ABS(test).GE.0.1) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'Fractional time zone found: XBTZ= ',xbtz
         write(io6,*) 'SMERGE is designed to use integer time zones'
         lerrcf=.TRUE.
      endif
      if(iotz.LT.-12 .OR. iotz.GT.12) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'Invalid value of time zone (IOTZ) = ',iotz
         write(io6,*) ' (-12 <= IOTZ <= +12) '
         lerrcf=.TRUE.
      endif

c --- Test output data formats
      if(ioform.NE.1 .AND. ioform.NE.2) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'Invalid value of IOFORM = ',ioform
         write(io6,*) 'IOFORM must be 1 or 2'
         lerrcf=.TRUE.
      endif
c --- Packing flag of output SURF.DAT file (0=not packed,1=packed)
      if(iopack.NE.0 .AND. iopack.NE.1) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'Invalid value of IOPACK = ',iopack
         write(io6,*) 'IOPACK must be 0 or 1'
         lerrcf=.TRUE.
      endif

c --- Test for valid JDAT
      if(nff.GT.0 .AND. envpop.EQ.1) then
         if(jdat.LT.1 .OR. jdat.GT.7) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 1'
            write(io6,*) 'JDAT out of range      = ',jdat
            write(io6,*) 'JDAT should be between 1 and 7'
            lerrcf=.TRUE.
         endif
      elseif(nff.GT.0) then
         if(jdat.LT.1 .OR. jdat.GT.7 .OR. jdat.EQ.4) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 1'
            write(io6,*) 'JDAT out of range      = ',jdat
            write(io6,*) 'JDAT should be 1,2,3 or 5,6,7'
            lerrcf=.TRUE.
         endif
      endif

c --- Test for valid IHUSWO
      if(jdat.EQ.3) then
         if(ihuswo.LT.1 .OR. ihuswo.GT.2) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 1'
            write(io6,*) 'IHUSWO out of range      = ',ihuswo
            write(io6,*) 'IHUSWO should be between 1 and 2'
            lerrcf=.TRUE.
         endif
      endif

c --- Test for valid JTXT
      if(nff.GT.0 .AND. envpop.EQ.1) then
         if(jtxt.LT.1 .OR. jtxt.GT.1) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 1'
            write(io6,*) 'JTXT out of range      = ',jtxt
            write(io6,*) 'JTXT should be 1'
            lerrcf=.TRUE.
         endif
      endif

c --- Test for valid NBSTN
      if(nbstn.GT.mxss) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'NBSTN exceeds the parameter MXSS '
         write(io6,*) 'NBSTN, MXSS = ',nbstn,mxss
         write(io6,*) 'Increase MXSS in PARAMS.SMG and recompile'
         lerrcf=.TRUE.
      endif

c --- Deactivate VSRN output option
      if(lvsrnout) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'Unsupported option detected:    LVSRNOUT'
         write(io6,*) 'VSRN output file option is under review'
         write(io6,*) 'Set LVSRNOUT = F in the control file'
         lerrcf=.TRUE.
      endif

c --- STOP now if error exists in the control file
      if(LERRCF) then
         write(*,*)'ERRORS are found in the CONTROL file'
         write(*,*)'Review messages written to the LIST file'
         stop
      endif

c ------------------------
c --- Open remaining files
c ------------------------
      call OPENS

c -----------------------------------------------------------
c --- Echo inputs to list file as in previous SMERGE versions
c -----------------------------------------------------------

c --- Echo filenames
      write(io6,*)
      write(io6,*) 'Control file name: ',runinp
      write(io6,*) 'Output list file name: ',runlst
      write(io6,*) 'Output file name: ',surfdat
      if(envpop.EQ.1) write(io6,*) 'Station Info file name: ',sstatxt
      write(io6,*) 'Continuation Run? ',lprev
      if(LPREV)
     1  write(io6,*) 'Previous SMERGE output data file: ', prevdat
      write(io6,*)
      if(LVSRNOUT) write(io6,*) 'Weather-visibility file: ',vsrndat

      if (jdat.EQ.1) write(io6,20)
      if (jdat.EQ.2) write(io6,21)
      if (jdat.EQ.3) write(io6,22)
      if (jdat.EQ.4 .AND. envpop.EQ.1) write(io6,23)
      if (jdat.EQ.5) write(io6,24)
20    format(//1x,'Station ID',2x,'Time Zone ',5x,'Formatted CD144'
     1      ,' Surface Data  ',/35x,'Input Files ',/)
21    format(//1x,'Station ID',2x,'Time Zone ',5x,'SAMSON Surface',
     1       ' Data  ',/35x,'Input Files ',/)
22    format(//1x,'Station ID',2x,'Time Zone ',5x,'HUSWO Surface',
     1       ' Data  ',/35x,'Input Files ',/)
23    format(//1x,'Station ID',2x,'Time Zone ',5x,'Extended CD144'
     1      ,' Surface Data  ',/35x,'Input Files ',/)
24    format(//1x,'Station ID',2x,'Time Zone ',5x,'ISHWO Surface',
     1       ' Data  ',/35x,'Input Files ',/)

      do i=1,nff
         write(io6,'(1x,i10,5x,i3,8x,a)') ifstn(i),istz(i),cffiles(i)
      enddo

c --- Identify units in HUSWO file
      if (jdat.EQ.3) then
         write(io6,*)
         write(io6,*) units(ihuswo)
         write(io6,*)
         write(*,*) units(ihuswo)
      endif

c --- If JDAT=5 to 7 (ISHWO, 3505, 9956), make sure LPCALC enabled
      if(jdat.ge.5.and.jdat.le.7.and..not.lpcalc) then
         lpcalc=.true.
         write(io6,*)
         write(io6,*) 'READCF:  Note in Input Group 1'
         write(io6,*) 'LPCALC reset to .true. for integrated data'
      endif

c --- Echo processing period
      write(io6,80) iotz,ibmo,ibdy,ibyr,ibhr,iemo,iedy,ieyr,iehr
80    format(//,1x,'Period to Extract (in time zone',i3,'):   ',i2,'/',
     1    i2,'/',i4,2x,i2,':00','  to  ',i2,'/',i2,'/',i4,2x,i2,':00'/)


      return
      end
c----------------------------------------------------------------------
      subroutine comp
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 041026                COMP
c                E. Insley, J. Scire,   Earth Tech, Inc.
c
c --- PURPOSE:  Process the surface station data to produce a
c               SURF.DAT file for CALMET
c
c --- UPDATES:
c     V5.52(041026) from V5.3(030402) (D. Strimaitis)
c     - Added station ID check (the ID provided in the control file for
c       each station file processed must match the ID found in the
c       corresponding file.
c     V5.3(030402) from V5.0(020308) (D. Strimaitis)
c     - Move previous SURF.DAT header output to RDHD
c     V5.0(020308) from V5.0(010630) (D. Strimaitis)
c     - Use large integer format when writing station ID instead of 
c       the char*5 variable
c
c --- INPUTS:
c
c        Parameters: IO6, IOFF, IOMESG, MXFF, MXSS
c
c --- OUTPUT:
c           none
c
c --- COMP called by:  MAIN
c --- COMP calls:      GRDAY, DEDAT,
c                      RDWRITS, CHKHUSHD
c ------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.smg'

c --- Include common blocks
      include 'control.smg'
      include 'datehr.smg'
      include 'filnam.smg'
      include 'qa.smg'
      include 'station.smg'

c --- Define common for unused variables
      common/skip/jd,ctext

      integer ishift(3)
      character*80 ctext
      character*120 hushd

c --- Report progress
      write(iomesg,*)'COMPUTATIONAL PHASE'

c --- Process the two header records in the SAMSON files
      if(jdat.eq.2)then
         io=ioff
         do i=1,nff
            read(io,'(1x,i5)')jd
            read(io,'(a80)')ctext
            io=io+1
c ---       Check the ID
            if(jd.NE.ifstn(i)) then
               write(*,*)'FATAL Error in COMP -- Station ID bad'
               write(io6,*)'FATAL Error in COMP -- Station ID bad'
               write(io6,*)'File number (SAMSON): ',i
               write(io6,*)' Expected Station ID: ',ifstn(i)
               write(io6,*)'    Found Station ID: ',jd
               stop
            endif
         enddo
c --- Skip the one header record in the HUSWO files
      elseif(jdat.eq.3)then
         io=ioff
         do i=1,nff
            read(io,'(a120)')hushd
            call chkhushd(io6,hushd)
            io=io+1
         enddo
      endif

C  READ DATA AND WRITE TO OUTPUT FILE
      call rdwrits

      write(io6,81)
81    format(//,26x,'********************',//)

C  WRITE OUT SUMMARY INFORMATION ABOUT OUTPUT FILE
      if(ioform.EQ.1)then
         write(io6,135) iotz, ioform, iopack
135      format(/,1x,'Characteristics of SMERGE Output ',
     1         'Data File:'//3x,
     2         'Time Zone:',i6/3x,'File Format (1=unformatted, ',
     3         '2=formatted):',i3/3x,'Packing Code:',i3)
      elseif(ioform.EQ.2)then
         write(io6,136) iotz, ioform
136      format(/,1x,'Characteristics of SMERGE Output ',
     1         '(SURF.DAT) File:'//3x,
     2         'Time Zone:',i6/3x,'File Format (1=unformatted, ',
     3         '2=formatted):',i3)
      else
         write(io6,137) ioform
137      format(1x,'ERROR-- Invalid input for IOFORM (1 or 2); ',
     1         'IOFORM = ',i3)
         write(*,987)
         goto 99
      endif


C     WRITE TO LIST FILE THE STATION NUMBERS IN THE OUTPUT FILE
C     SET VARIABLES FOR PROPER COLUMNS FOR WRITING OUT
C      J4 IS NO. ROWS IN A "SHORT" COLUMN
C      J5 IS NO. ROWS IN A "LONG" COLUMN
C      J6 IS THE NUMBER OF "LONG" COLUMNS
      j4 = ntstn/4
      j6 = mod(ntstn,4)
      if(j6.EQ.0)then
        j5 = j4
      else
        j5 = j4 + 1
      endif
      ishift(1) = j5
      do i=2,3
        if(i.LE.j6)then
          ishift(i) = ishift(i-1) + j5
        else
          ishift(i) = ishift(i-1) + j4
        endif
      enddo
      ncol = min0(ntstn,4)
      write(io6,150) (' ',k=1,ncol)
150   format( /,1x,'Surface Stations in Output File:  '/
     1       3x,4(a1,'No.',7x,'ID',8x)/)
      do i=1,j4
        i2 = i + ishift(1)
        i3 = i + ishift(2)
        i4 = i + ishift(3)
        write(io6,160) i,idstn(i),i2,idstn(i2),i3,idstn(i3),
     &                 i4,idstn(i4)
160     format(3x,4(i3,2x,I10,6x))
      enddo
      if(j6.GT.0)then
        n1 = j5
        n2 = n1+(j6-1)*j5
        write(io6,160) (k,idstn(k),k=n1,n2,j5)
      endif

      write(io6,*)
      write(io6,*)
      write(io6,*)

      return

99    stop
987   format(1x,'ERROR in SMERGE run - see SMERGE.LST file')

      end
c----------------------------------------------------------------------
      subroutine opens
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 040709               OPENS
c ---            E. Insley, Earth Tech, Inc.

c
c     Opens input and output data files
c
c --- UPDATES:
c     V5.5_draft(040709) from V5.1(020809) (K. Morrison)
c     - Open VSRN.DAT if used
c     V5.1(020809) from V5.0(020308) (D. Strimaitis)
c     - Use parameter ENVPOP to activate SSTA file
c
c --- OPENS called by:  READCF
c --- OPENS calls:      none
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.smg'
c --- Include common blocks
      include 'control.smg'
      include 'filnam.smg'

C     OPEN FORMATTED INPUT FILES
      io = ioff
      do i=1,nff
         open(io,file=cffiles(i),status='old')
         io = io + 1
      enddo

C     OPEN EXISTING SURF.DAT INPUT FILE
      if(LPREV)then
         if(inform.EQ.1)then
c ---       BINARY INPUT FILE
            open(ioprev,file=prevdat,status='old',form='unformatted')
         elseif(inform.EQ.2)then
c ---       FORMATTED INPUT FILE
            open(ioprev,file=prevdat,status='old')
         endif
      endif

c --- Open the station information file
      if(envpop.EQ.1) open(iossta,file=sstatxt,status='old')

c     OPEN OUTPUT SURF.DAT FILE
      if(ioform.EQ.1)then
c ---    BINARY OUTPUT FILE
         open(iosurf,file=surfdat,status='unknown',form='unformatted')
      elseif(ioform.EQ.2)then
c ---    FORMATTED OUTPUT FILE
         open(iosurf,file=surfdat,status='unknown')
      endif

c     OPEN OUTPUT PRECIP.DAT FILE (name is fixed)
      if(jdat.EQ.4 .AND. envpop.EQ.1)then
c ---    FORMATTED OUTPUT FILE
         open(ioprec,file='precip.dat',status='unknown')
      endif

c     Open the VSRN.DAT file
      if(lvsrnout) then
c ---    if a continuation run, position to end of file
         if(lprev) then
            open(iovsrn,file=vsrndat,position='append')
         else
            open(iovsrn,file=vsrndat)
         endif
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine rdwrits
c----------------------------------------------------------------------
c     
c --- SMERGE     Version: 5.57        Level: 041102             RDWRITS
c ---            E. Insley, Earth Tech, Inc.
c
c --- Updated V5.54(041102) from V5.53(041029)
c     - Fixed bug in RDWRITS that assigned the wrong station
c       elevation array element when adding stations to an
c       existing SURF.DAT file.                    (D. Strimaitis)
c
c --- Updated V5.53(041029) from V5.52(041026)
c     - Modified End-of-File response after call to GETISH to continue
c       processing valid data on the last record of a file.  Missing
c       station pressure on the last record was not calculated in 
c       previous version (JDAT=5,6,7).             (D. Strimaitis)
c
c --- Updated V5.52(041026) from V5.51(040922) (D. Strimaitis)
c     - Added station ID check (the ID provided in the control file for
c       each station file processed must match the ID found in the
c       corresponding file.
c     - Fixed bug in call to GETISH in RDWRITS that used the wrong
c       station time zone array element when adding stations to an
c       existing SURF.DAT file.                     (K. Morrison)
c
c --- Updated V5.51(040922) from V5.5_draft(040709) (K. Morrison)
c     - Correction to empirical calculations for pressure
c     - Remove substitution of total cloud cover for opaque for
c       CD144 if opaque is missing
c     - Correct reference to station id for SAMSON and HUSWO
c     - Correct use of present weather for precipitation codes for
c       SAMSON (undo an error)
c     - Correct identification of missing precipitation codes for
c       SAMSON
c     - Correct identification of missing visibility values for
c       SAMSON and HUSWO
c     - Use pressure value to identify HUSWO as English or metric
c       units and STOP if units are not those expected.
c     - Use temperature value to identify HUSWO as English or metric
c       units and STOP if units are not those expected.          (DGS)
c --- Updated V5.5_draft(040709) from V5.44(040621) (K. Morrison)
c     - Add TD3505 and TD9956 file types, and for these types
c       override elevation if valid value read from data file
c     - Handle present weather codes and visibility, and output to
c       VSRN.DAT if needed
c --- Updated V5.44(040621) from V5.42(040318) (K. Morrison)
c     - Read dew-point for all file types, and sea-level pressure
c       for CD-144
c     - Add ability to calculate surface pressure from sea-level
c       pressure for CD-144 via either empirical relation or
c       call to SLP2STP
c     - Get altimeter setting and sea-level pressure from GETISH
c       for ISHWO files
c     - Move pressure calculations for ISHWO to here from READISH,
c       allowing empirical relations or by calling subroutines ALP2STP 
c       and SLP2STP
c     - Enable calculation of RH if dry-bulb and dew-point are 
c       present for all data types
c
c --- Updated V5.42(040318) from V5.41(040210) (K. Morrison)
c     - Enforce CD-144 0 WD with non-0 WS to both be missing
c
c --- Updated V5.41(040210) from V5.4(040205) (K. Morrison)
c     - Add station elevation to ISHWO call to permit station pressure
c       calculation based on altimeter setting or sea level pressure 
c       if station pressure missing
c
c --- Updated V5.4(040205) from V5.3(030402) (K. Morrison)
c     - Add ISHWO file type
c
c --- Updated V5.3(030402) from V5.2(020809) (D. Strimaitis)
c     - Drop parameter ENVPOP from RDHD
c
c --- Updated V5.1(020809) from V5.0(020308) (D. Strimaitis)
c     - Pass parameter ENVPOP to header subroutines
c
c --- Updated V5.0(020308) from V5.0(010630) (D. Strimaitis)
c     - Initialize hourly data to missing before reading file
c     - Introduce station location information arrays
c     - Add JDAT=4 for extended CD144 record (precip rate)
c
c --- Updated V5.0(010630) from V4.0(010315) (D. Strimaitis)
c     - Place parameters and commons into include files
c     - NBSTN .LE. 0 triggers use of all stations in previous file
c
c --- Updated 010315 from 991223 (D. Strimaitis)
c     - Y2K treatment of year enforced
c
c --- Updated 991223 from 970718 (Benjamin de Foy)
c     - Add HUSWO format
c
c
C     READS INPUT FILES AND FILLS THE OUTPUT ARRAYS AND WRITES TO
C     THE OUTPUT FILE
c     
c --- DATE VARIABLES:
c     IBDATHR  - integer  - Beginning date of requested time period
c                           in output time zone YYYYJJJHH
c     IEDATHR  - integer  - Ending date of requested time period in
c                           output time zone YYYYJJJHH
c     KBTZ     - integer  - Time zone of existing sfc input data
c     NDATE    - integer  - Time currently wanted in output time zone
c                           YYYYJJJHH
c     KDATE    - integer  - Time currently wanted in existing sfc time
c                           zone YYYYJJJHH
c     JDATE    - integer  - Time currently wanted in output time zone
c                           for a particular formatted station YYYYJJJHH
c     KBDATHR  - integer  - Beginning date of available existing sfc
c                           data in output time zone YYYYJJJHH
c     KEDATHR  - integer  - Ending date of available existing sfc data
c                           in output time zone YYYYJJJHH
c     IKBDATHR - integer  - Beginning date of available existing sfc
c                           data in existing sfc input time zone
c                           YYYYJJJHH
c     IKEDATHR - integer  - Ending date of available existing sfc data
c                           YYYYJJJHH
c     IDATHR   - integer  - Time of record actually read in existing
c                           sfc time zone YYYYJJJHH
c     
c --- OTHER VARIABLES AND FLAGS:
c     IBIN     - integer  - Flag indicating date status of existing sfc
c                           input file vs requested time period;
c                           IBIN = 0  no data in existing sfc input
c                           file for requested time period
c                           IBIN = 1  existing sfc input file contains
c                           data for requested time period
c     
C     RDWRITS called by: COMP
C     RDWRITS calls: DEDAT, DELTT, RDHD, WRHD, WRHDP, RDS,
C                    PCODES, PCODES2, CLOUDS, WRS, INDECR, JULDAY, YR4,
c                    GETISH, ALP2STP, SLP2STP, SCAN144, SCANISHWO
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.smg'
c --- Include common blocks
      include 'control.smg'
      include 'datehr.smg'
      include 'qa.smg'
      include 'station.smg'

      integer ixref(mxss),iceil(mxss),icc(mxss),irh(mxss),
     1     ipcode(mxss),ibuf(3,mxss),ixceil(mxss),ixcc(mxss),
     2     ixrh(mxss),ixpcode(mxss),jprecip(5)

c --- Arrays for empirical pressure calculations
      real xintercep(mxss)/mxss*-9999./
      real xslope(mxss)/mxss*-9999./
      real altrat(mxss)/mxss*-9999./
      logical lfirstrd(mxss)/mxss*.true./

c --- Array for counting missing hours for each variable in surf.dat:
      integer ncbdf(7)

c --- Flag for printing missing:
      logical lfprint

c --- Logical to identify extended CD144 format
      logical lx144

c --- Logical for HUSWO as metric
      logical lhmetric

c --- HUSWO: total and opaque sky cover:
      integer itskc,ioskc     
c --- HUSWO: present weather code:
      character*8 pwth
c --- HUSWO: precipitation flag:
      character*1 precfl
c --- HUSWO: snow fall:
      integer isnow
c --- HUSWO: count hours with multiple weather events:
      integer npwth

c --- Conversion inches Hg to milliBars:
      real inHgtomb             
      parameter(inHgtomb = 33.864)

      real xws(mxss),xwd(mxss),xtempk(mxss),xpres(mxss),ws(mxss),
     1     wd(mxss),tempk(mxss),pres(mxss)

c --- Add array for precipitation rate in mm/hr (extended CD144 format)
      real xprate(mxss)

      character*1 cceil(3),tcc,ccc,jcover1
      character*8 cprecip
      character cwd*2, cws*2, cpres*4, ctemp*3, crh*3, jceilc*7,
     * cslp*4, cdewp*3, cvis*3

c --- Logicals for ISHWO

      logical licc/.true./,leof

c --- Add data for present weather code conversions

      integer*2 hus2dats(10:99)
      integer*2 sam2dats(0:9,9)
      data sam2dats/
     1 95,99,19,18,18,18,19,19,19,999,
     2 61,63,65,80,81,81,66,67,67,999,
     3 60,62,64,51,53,55,56,57,57,999,
     4 71,73,75,87,88,88,78,78,78,999,
     5 85,87,87,70,72,74,77,77,77,999,
     6 79,79,79,89,90,90,87,88,88,999,
     7 45,45,44,31,31,45,49,45,44,999,
     8 4,5,4,31,38,999,31,4,999,999,
     9 79,79,79,999,999,999,999,999,999,999/
      equivalence (hus2dats,sam2dats)
      integer*1 isampw(9)
      integer*2 ipwout(3)

c --- Set flag for extended CD144 format
      lx144=.FALSE.
      if(jdat.EQ.4) lx144=.TRUE.
    
c --- Initialize ncbdf,lfprint: - bdf
      lfprint = .true.
      do j = 1,7
         ncbdf(j) = 0
      enddo
      npwth = 0
c     
C     SET FLAG FOR READING BINARY DATA
      ibin = 1
C     DETERMINE THE NUMBER OF HOURS BETWEEN THE BEGINNING AND
C     ENDING DATES OF OUTPUT FILE
      call dedat(ibdathr,ibyr,ibjul,ibhr)
      call dedat(iedathr,ieyr,iejul,iehr)
      call deltt(ibyr,ibjul,ibhr,ieyr,iejul,iehr,idiff)
C     NREC IS THE NUMBER OF DATA RECORDS IN THE OUTPUT FILE
      nrec = idiff + 1
C     READ HEADER INFORMATION FOR EXISTING SURFACE DATA INPUT FILE
      if(LPREV) then
         call rdhd(io6,inform,ioprev,kbyr,kbjul,kbhr,
     1        keyr,kejul,kehr,kbtz,nstnb,ispack,mxss,idbstn,banem,
     2        cbname,cblat,cblon)
         ikbdathr = kbyr * 100000 + kbjul * 100 + kbhr
         ikedathr = keyr * 100000 + kejul * 100 + kehr
C     SET UP CROSS REFERENCE OF STATION IDS WITH THEIR CORRESPONDING
C     POSITION IN THE EXISTING SURFACE DATA FILE ARRAY
         if(nbstn.LE.0) then
            nbstn = nstnb
            do j=1,nstnb
               ixref(j) = j
               ibstn(j) = idbstn(j)
            enddo
            go to 16
         endif
         do 5 i = 1,nbstn
            do 15 k = 1,nstnb
               if(ibstn(i).EQ.idbstn(k))then
                  ixref(i) = k
                  go to 5
               endif
 15         continue
 5       continue
C     CONVERT TO IOTZ (OUTPUT) TIME ZONE
 16      if(iotz.EQ.kbtz)then
            kbdathr = ikbdathr
            kedathr = ikedathr
         else
C     CONVERT DATE/HR TO OUTPUT TIME ZONE
            idtz = kbtz - iotz
            call indecr(io6,kbyr,kbjul,kbhr,idtz,0,23)
            call indecr(io6,keyr,kejul,kehr,idtz,0,23)
            kbdathr = kbyr * 100000 + kbjul * 100 + kbhr
            kedathr = keyr * 100000 + kejul * 100 + kehr
         endif
         if(iedathr.LT. kbdathr .OR. ibdathr.GT.kedathr)then
            write(io6,100)
 100        format('  WARNING: The existing surface data input ',
     1           'file has no data for the time period of this run.')
            ibin = 0
         endif
      else
         nbstn = 0
         kbtz = iotz
c        Write header to VSRN.DAT
         if(lvsrnout) write(iovsrn,*) 'STN---'
      endif
C     FILL STATION ARRAYS FOR OUTPUT FILE
      ntstn = nff + nbstn
C     EXISTING SURFACE STATIONS
      do 25 i = 1,nbstn
         idstn(i) = ibstn(i)
         anem(i)  = banem(i)
         cname(i) = cbname(i)
         clat(i)  = cblat(i)
         clon(i)  = cblon(i)
 25   continue
C     FORMATTED STATIONS
      do 35 i = nbstn+1,ntstn
         idstn(i) = ifstn(i-nbstn)
         anem(i)  = fanem(i-nbstn)
         cname(i) = cfname(i-nbstn)
         clat(i)  = cflat(i-nbstn)
         clon(i)  = cflon(i-nbstn)
 35   continue
c     --- Check for repeating station IDs
      do 360 j=1,ntstn
         icheck = idstn(j)
         icnt = 0
         do 370 k=1,ntstn
c     ---     Keep count of matches (it should only match once)
            if(idstn(k).EQ.icheck) icnt = icnt + 1
 370     continue
         if(icnt.GT.1)then
            write(io6,*) ' ERROR: Duplicate Station ID. ID = ',icheck
            write(*,*) ' ERROR: Invalid Input --- See Run LIST file'
            stop
         endif
 360  continue
C     WRITE OUT THE HEADER TO THE SURFACE DATA OUTPUT FILE
      call wrhd(envpop,io6,ioform,iosurf,ibyr,ibjul,ibhr,
     1          ieyr,iejul,iehr,iotz,ntstn,iopack,idstn,anem,
     2          cname,clat,clon,ver,level)

      if(envpop.EQ.1) then
c ---    Write header to PRECIP.DAT file
         if(LX144) call wrhdp(envpop,ioprec,ibyr,ibjul,ibhr,
     1                        ieyr,iejul,iehr,iotz,ntstn,idstn,
     2                        cname,clat,clon,ver,level)
      endif

C     FIRST DATE TO WRITE TO THE OUTPUT FILE
      ndate = ibyr * 100000 + ibjul * 100 + ibhr
      if(iotz.EQ.kbtz)then
         kdate = ndate
      else
C     KDATE IS TIME IN "KBTZ" TIME ZONE WHICH IS EQUIVALENT TO NDATE IN
C     "IOTZ" TIME ZONE
         idtz = iotz - kbtz
         call dedat(ndate,jyr,jjul,jhr)
         call indecr(io6,jyr,jjul,jhr,idtz,0,23)
         kdate = jyr * 100000 + jjul * 100 + jhr
      endif
C     SKIP TO FIRST DATE IN EXISTING SURFACE DATA INPUT FILE IF THERE
C     ARE DATA FOR THE REQUESTED PERIOD OF THIS RUN
      if(ibdathr.GT.kbdathr)then
         if(LPREV .AND. ibin.NE.0)then
            call deltt(kbyr,kbjul,kbhr,ibyr,ibjul,ibhr,iskip)
            do i = 1,iskip
               call rds(io6,inform,nstnb,ispack,ioprev,1,ibuf,myr,mjul,
     1                  mhr,ws,wd,iceil,icc,tempk,irh,pres,ipcode)
            enddo
         endif
      endif

c ----------------------
C     MAIN LOOP: (HOURS)
c ----------------------
      do 55 i = 1,nrec

C     READ DATA FOR 1 HOUR FROM EXISTING SURFACE DATA INPUT FILE
c     ----------------------------------------------------------
         if(LPREV .AND. ibin.NE.0)then
            if(ndate.LE.kedathr.AND.ndate.GE.kbdathr)then
               call rds(io6,inform,nstnb,ispack,ioprev,0,ibuf,iyr,ijul,
     1                  ihr,ws,wd,iceil,icc,tempk,irh,pres,ipcode)
               idathr = iyr * 100000 + ijul * 100 + ihr
               if(kdate.NE.idathr) then
                  write(io6,200) idathr, kdate
 200  format(1x,'Error in Subr. RDWRITS: Next date and hour ',
     1       'from the existing surface data input file, ',i10/1x,
     2       'does not match the expected date and hour, ',i10)
                  write(*,987)
 987  format(1x,'ERROR in SMERGE run - see SMERGE.LST file')
                  go to 99
               endif
C     TRANSFER STATIONS OF INTEREST FROM READ BUFFER TO OUTPUT ARRAY
               do 65 j = 1,nbstn
                  index = ixref(j)
                  xws(j) = ws(index)
                  xwd(j) = wd(index)
                  ixceil(j) = iceil(index)
                  ixcc(j) = icc(index)
                  xtempk(j) = tempk(index)
                  ixrh(j) = irh(index)
                  xpres(j) = pres(index)
                  ixpcode(j) = ipcode(index)
                  xprate(j) = 9999.
 65            continue
            else
               do 75 j = 1,nbstn
                  xws(j) = 9999.
                  xwd(j) = 9999.
                  ixceil(j) = 9999
                  ixcc(j) = 9999
                  xtempk(j) = 9999.
                  ixrh(j) = 9999
                  xpres(j) = 9999.
                  ixpcode(j) = 9999
                  xprate(j) = 9999.
 75            continue
            endif
         else
            do 85 j = 1,nbstn
               xws(j) = 9999.
               xwd(j) = 9999.
               ixceil(j) = 9999
               ixcc(j) = 9999
               xtempk(j) = 9999.
               ixrh(j) = 9999
               xpres(j) = 9999.
               ixpcode(j) = 9999
               xprate(j) = 9999.
 85         continue
         endif
C     SET ARRAY INDEX COUNTER FOR FORMATTED STATIONS
         index = nbstn

C     LOOP OVER FORMATTED FILES (STATIONS)
c     ------------------------------------
         do 95 k = 1,nff
            index = index + 1
            iofor = k + (ioff-1)

c ---       Initialize data to missing
            xws(index) = 9999.
            xwd(index) = 9999.
            ixceil(index) = 9999
            ixcc(index) = 9999
            xtempk(index) = 9999.
            ixrh(index) = 9999
            xpres(index) = 9999.
            ixpcode(index) = 9999
            xprate(index) = 9999.
            ipwout=999
            visb=99999.

C     READ A RECORD OF FORMATTED DATA
            if(iotz.EQ.istz(k))then
               jdate = ndate
            else
C     CORRECT REQUESTED LOCAL TIME TO OUTPUT TIME ZONE
               idtz = iotz - istz(k)
               call dedat(ndate,jyr,jjul,jhr)
               call indecr(io6,jyr,jjul,jhr,idtz,0,23)
               jdate = jyr * 100000 + jjul * 100 + jhr
            endif
c ------
c CD144:
c ------
 17   if (jdat.eq.1 .OR. jdat.eq.4) then
c        Check if pressure calculation was selected, and attempt empirical
c          calculations
         if(LPCALC.and.LFIRSTRD(index)) then
            lfirstrd(index)=.false.
            call scan144(iofor,xintercep(index),xslope(index))
         endif
         if(LX144 .AND. envpop.EQ.1) then
            read(iofor,10,end=95) id,jyr,jmo,jday,jhr,cceil,cvis,
     1                            cprecip,cslp,cdewp,cwd,cws,cpres,
     2                            ctemp,crh,tcc,ccc,pmmhr
         else
            read(iofor,10,end=95) id,jyr,jmo,jday,jhr,cceil,cvis,
     1                            cprecip,cslp,cdewp,cwd,cws,cpres,
     2                            ctemp,crh,tcc,ccc
         endif
c 10      format(i5,4i2,3a1,4x,a3,a6,9x,2a2,a4,a3,3x,a3,a1,22x,a1)
 10      format(i5,4i2,3a1,4x,a3,a8,a4,a3,2a2,a4,a3,3x,a3,a1,
     *          22x,a1,f3.0)
         ichkws = 0
c ---    Make sure year is YYYY (Y2K)
         call YR4(io6,jyr,ierr)
         if(ierr.NE.0) stop 'Halted in RDWRITS - see list file'
C     CONVERT DATE READ FROM FORMATTED FILE AND COMPARE TO DATE WANTED
         call julday(io6,jyr,jmo,jday,jjul)
         idathr = jyr * 100000 + jjul * 100 + jhr
c if jdate not reached, read new record:
         if(idathr.LT.jdate) go to 17 
         if(idathr.EQ.jdate)then

c ---       Check station ID
            if(id.NE.ifstn(k)) then
               write(*,*)'FATAL Error in RDWRITS -- Station ID bad'
               write(io6,*)'FATAL Error in RDWRITS -- Station ID bad'
               write(io6,*)' File number (CD144): ',k
               write(io6,*)' Expected Station ID: ',ifstn(k)
               write(io6,*)'    Found Station ID: ',id
               stop
            endif

c     Substitute total cloud cover if opaque is missing
c            if(ccc.eq.' ') ccc=tcc
C     CONVERT WD (tens of degrees) TO WIND DIRECTION (degrees)
c     Check for missing data
            if (cwd.eq.'  ')then
               xwd(index) = 9999.
               xws(index) = 9999.
               ichkws = 1
            else
               read (cwd,390)jwd
               read (cws,390)jws
 390           format (i2)
               if(jws.GT.0)then
                  if(jwd.GT.0) then
                     xwd(index) = jwd * 10.
                  else
                     xwd(index) = 9999.
                     xws(index) = 9999.
                     ichkws = 1
                  endif
               else
                  xwd(index) = 0.
               endif
               if (xwd(index).lt.0.0.or.xwd(index).gt.360.0) then
                  xwd(index) = 9999.
               endif
            endif
C     CONVERT TEMPERATURE FROM DEG F TO DEG K
c     Check for missing data
            if (ctemp.eq.'   ')then
               xtempk(index) = 9999.
            else
               read (ctemp,391)jtemp
 391           format (i3)
               xtempk(index) = (jtemp - 32) * 5./9. + 273.15
            endif
C     GET DEW POINT TEMPERATURE 
c     Check for missing data
            if (cdewp.eq.'   ')then
               dewp = 9999.
            else
               if(cdewp(1:1).eq.'X'.or.cdewp(1:1).eq.'x') then
                 read(cdewp,fmt='(1x,f2.0)') dewp
                 dewp=-dewp
               elseif(cdewp(1:1).eq.'0') then
                 read(cdewp,fmt='(1x,f2.0)') dewp
               else  
                 read (cdewp,fmt='(f3.0)') dewp
               endif
            endif
C     CONVERT WS FROM KNOTS TO M/S
            if (ichkws.ne.1) xws(index) = jws * 0.51444
C     CONVERT CD144 PRECIPITATION DATA TO PRECIP CODES AND
C     DATSAV2 WEATHER CODES
c     Check for missing data
            if (cprecip.eq.'        ')then
               ixpcode(index)=9999
               ipwout=999
            else
               read(cprecip,394)(jprecip(kkk),kkk=1,5)
 394           format (1x,5i1)
               call pcodes(io6,idathr,id,jprecip,ixpcode(index))
               ipwout=0
               idatsvi=1
               do isami=1,8
                  if(cprecip(isami:isami).ne.'0'.and.
     &               cprecip(isami:isami).ne.' ') then
                     read(cprecip(isami:isami),fmt='(i1)') ipw144
                     ipw144=ipw144-1
                     ipwout(idatsvi)=sam2dats(ipw144,isami)
                     idatsvi=idatsvi+1
                     if(idatsvi.gt.3) exit
                  endif
               enddo
            endif
C     CONVERT CD144 VISIBILITY CODE TO MILES
            if(cvis.eq.'   ') then
               visb=99999.
            else
               read(cvis,fmt='(i3)') ivisb
               visb=real(ivisb/10)
               ivisb=mod(ivisb,10)
               if(ivisb.le.6) then
                  visb=visb+real(ivisb)/16.
               else
                  visb=visb+.375+real(ivisb-6)*.125
               endif
            endif                         
C     CONVERT CD144 ALPHANUMERIC CLOUD DATA TO NUMERIC DATA
            call clouds(io6,cceil,ccc,ixcc(index),ixceil(index))
C     GET SEA-LEVEL PRESSURE
c     Check for missing data
            if (cslp.eq.'    ') then
               slp = 9999.
            else
               read (cslp,fmt='(f4.1)') slp
               if(slp.lt.400.) slp=slp+1000.
            endif

C     CONVERT SURFACE PRESSURE FROM INCHES HG TO MB
C     33.864 MB = 1 INCH HG
c     Check for missing data
            if (cpres.eq.'    ') then
c             If lpcalc selected, try estimating pressure from 
c               sea-level value first empirically, and then by
c               equation               
              if(lpcalc.and.slp.lt.9998.) then
                if(abs(xintercep(index)).lt.999..and.
     *             abs(xintercep(index)).gt.1.e-10) then
                   xpres(index)=(xintercep(index)+xslope(index)*
     *                          xtempk(index))*slp
                 else 
                  call slp2stp(slp,elev(k),xtempk(index),9999.,
     &                         xpres(index))
                 endif
              else
                 xpres(index) = 9999.
              endif
            else
               read (cpres,392) jpres
 392           format (i4)
               xpres(index) = jpres * 0.01* inHgtomb
            endif
C     FILL RELATIVE HUMIDITY OUTPUT ARRAY
c     Check for missing data
            if (crh.eq.'   ')then
c              if dry-bulb and dew point available, calculate
               if(tempk(index).lt.9998..and.dewp.lt.9998.) then
                 ixrh(index)=max(1,min(nint(100.*((173.-real(jtemp)
     &                 *.1+dewp)/(173.+real(jtemp)*.9))**8),100))
               else
                 ixrh(index) = 9999
               endif
            else
               read (crh,391) jrh
               ixrh(index) = jrh
            endif
c ---       Get precipitation rate from extended CD144 record
            if(LX144) then
c ---          Identify missing data (-99)
               if(pmmhr.LT.-90.0) then
                  xprate(index) = 9999.
               else
                  xprate(index) = pmmhr
               endif
            else
               xprate(index) = 9999.
            endif
         else
c     this is used if the whole record is missing.
c     backspace to recover at next hour (bdf)
            call dedat(ndate,nyr,njul,nhr)
            write(io6,1007) nyr,njul,nhr,id
            backspace(iofor)
            xws(index) = 9999.
            xwd(index) = 9999.
            ixceil(index) = 9999
            ixcc(index) = 9999
            xtempk(index) = 9999.
            ixrh(index) = 9999
            xpres(index) = 9999.
            ixpcode(index) = 9999
            xprate(index) = 9999.
            ipwout=999
            visb=99999.
         endif

c -------
c SAMSON:
c -------
      elseif (jdat.eq.2) then
         read(iofor,422,end=95)jyr,jmo,jday,jhr,iio,jcover1,ccc,
     >        temp,dewpt,jrh,jpres,jdir,spd,visb,jceilb,
     >        isampw
 422     format(1x,i2,3(1x,i2),1x,i1,38x,a1,a1,f6.1,f6.1,i4,i5,
     >        i4,f6.1,f7.1,i7,1x,9i1,23x,5i1)
         id=ifstn(iofor-ioff+1)
c ---    Copy present weather codes 2-6 to jprecip for precip codes
         do kkk=1,5
           jprecip(kkk)=isampw(kkk+1)
         enddo
c ---    Make sure year is YYYY (Y2K)
         call YR4(io6,jyr,ierr)
         if(ierr.NE.0) stop 'Halted in RDWRITS - see list file'
c     *-*-* EMI Modification (7/18/97)
c     ****  Change Hour 24 to Hour 0, next day instead of shifting
c     ****  data down by one hour
c     ****  jhr=jhr-1
         call julday(io6,jyr,jmo,jday,jjul)
         if(jhr.EQ.24) then
            jhr = 23
            call indecr(io6,jyr,jjul,jhr,1,0,23)
            call grday(io6,jyr,jjul,jmo,jday)
         endif
c     *-*-* End of EMI Modification (7/18/97)
         idathr = jyr * 100000 + jjul * 100 + jhr
         if(idathr.LT.jdate) then
c read new record
            goto 17 
         elseif(idathr.EQ.jdate)then
            ichkws=0
            if (jcover1.eq.'1') ccc='-'
            if (jcover1.eq.'9') ccc=' '
            xpres(index)=jpres
            if (jdir.eq.999) then
               xwd(index)=9999.
               xws(index)=9999.
               ichkws=1
            else
               if(spd.gt.0.) then
                  xwd(index)=jdir
               else
                  xwd(index)=0.
               endif
            endif
c SAMSON wind speed is in m/s
            if (ichkws.ne.1) xws(index)=spd
            if (temp.eq.9999.) then
               xtempk(index)=temp
            else
               xtempk(index)=temp+273.15
            endif
            if (jrh.eq.999) then
c           if dry-bulb and dew point available, calculate rh
              if(tempk(index).lt.9998..and.dewpt.lt.9998.) then
                temp=temp*1.8+32.
                dewpt=dewpt*1.8+32.
                ixrh(index)=max(1,min(nint(((173.-temp*.1
     &                 +dewpt)/(173.+temp*.9))**8),100))
              else
                ixrh(index)=9999
              endif
            else
               ixrh(index)=jrh
            endif
            if (jceilb.eq.77777) then
               cceil(1)='-'
               cceil(2)='-'
               cceil(3)='-'
            elseif (jceilb.eq.88888) then
               cceil(1)='8'
               cceil(2)='8'
               cceil(3)='8'
            elseif (jceilb.eq.99999) then
               cceil(1)=' '
               cceil(2)=' '
               cceil(3)=' '
            else
               jceilb=jceilb*3.2808399/100
               write(jceilc,425) jceilb
 425           format(I7)
               if (jceilc(5:5).eq.' ') jceilc(5:5)='0'
               if (jceilc(6:6).eq.' ') jceilc(6:6)='0'
               if (jceilc(7:7).eq.' ') jceilc(7:7)='0'
               cceil(3)=jceilc(7:7)
               cceil(2)=jceilc(6:6)
               cceil(1)=jceilc(5:5)
            endif
            call clouds(io6,cceil,ccc,ixcc(index),ixceil(index))
            do ii=1,5
               jprecip(ii)=jprecip(ii)+1
               if (jprecip(ii).eq.10) jprecip(ii)=0
            enddo
            if (iio.eq.9.and.jprecip(1).eq.0.and.jprecip(2).eq.0
     1           .and.jprecip(3).eq.0.and.jprecip(4).eq.0.and.
     2           jprecip(5).eq.0) then
               ixpcode(index)=9999
            else
               call pcodes(io6,idathr,id,jprecip,ixpcode(index))
            endif
c     decode present weather into DATSAV2 codes
            ipwout=0
            if(minval(isampw).eq.9) then
               if(iio.eq.9) ipwout=999
            else
               idatsvi=1
               do isami=1,9
                  if(isampw(isami).ne.9) then
                     ipwout(idatsvi)=sam2dats(isampw(isami),isami)
                     idatsvi=idatsvi+1
                     if(idatsvi.gt.3) exit
                  endif
               enddo
            endif
c      visibilty km==>mi
            if(visb.lt.800.) then
              visb=min(visb/1.6093,99.9)
            else
              visb=99999.
            endif
         elseif(idathr.gt.jdate)then
c     this is used if the whole record is missing.
c     backspace to recover at next hour (bdf)
            call dedat(ndate,nyr,njul,nhr)
            write(io6,1007) nyr,njul,nhr,id
            backspace(iofor)
            xws(index) = 9999.
            xwd(index) = 9999.
            ixceil(index) = 9999
            ixcc(index) = 9999
            xtempk(index) = 9999.
            ixrh(index) = 9999
            xpres(index) = 9999.
            ixpcode(index) = 9999
            ipwout=999
            visb=99999.
         endif

c ------
c HUSWO:
c ------
      elseif (jdat.eq.3) then
         read(iofor,423,end=95) id,jyr,jmo,jday,jhr,
     &        itskc,ioskc,
     &        temp,dewp,jrh,jpresr,jdir,spd,
     &        visb,jceilb,pwth,
     &        iprec,
     &        precfl,isnow
 423     format(I5,1X,I4,3I2,1X,4X,1X,4X,1X,I2,I2,1X,F5.1,
     &        1X,1X,f5.1,1X,I3,1X,I4,1X,1X,I3,1X,F4.1,1X,
     &        f6.1,1X,I5,1X,A8,
     &        1X,2X,3X,1X,2X,3X,1X,2X,3X,2X,I3,A1,2X,I3)

c ---    Set units conversion
         lhmetric=.true.
         if(ihuswo.NE.2) lhmetric=.false.

c ---    Test temperature to see if data are in expected units
c        Metric  - temperature is in degrees C (must be < 61.0)
         if(temp.GT.60.0 .AND. temp.LT.999.0) then
            if(lhmetric) then
               write(io6,*)'FATAL Error in RDWRITS: HUSWO data units'
               write(io6,*)'Expected METRIC data units'
               write(io6,*)'Found ENGLISH temperature units, T = ',temp
               stop 'HALTED in RDWRITS: unexpected HUSWO data units'
            endif
         endif
         if(dewp.GT.60.0 .AND. dewp.LT.999.0) then
            if(lhmetric) then
               write(io6,*)'FATAL Error in RDWRITS: HUSWO data units'
               write(io6,*)'Expected METRIC data units'
               write(io6,*)'Found ENGLISH dew pt units, T = ',dewp
               stop 'HALTED in RDWRITS: unexpected HUSWO data units'
            endif
         endif

c ---    Test pressure to see if data are English or metric units
c ---    and compare with expected units
c        English - pressure is in hundredths of inches (>2000)
c        Metric  - pressure is in bars (<1101)
         if(jpresr.lt.1101) then
            if(.not.lhmetric) then
               write(io6,*)'FATAL Error in RDWRITS: HUSWO data units'
               write(io6,*)'Expected ENGLISH data units'
               write(io6,*)'Found METRIC pressure units, P = ',jpresr
               stop 'HALTED in RDWRITS: unexpected HUSWO data units'
            endif
         elseif(jpresr.lt.9990) then
            if(lhmetric) then
               write(io6,*)'FATAL Error in RDWRITS: HUSWO data units'
               write(io6,*)'Expected METRIC data units'
               write(io6,*)'Found ENGLISH pressure units, P = ',jpresr
               stop 'HALTED in RDWRITS: unexpected HUSWO data units'
            endif
         endif

c         id=ifstn(iofor-ioff+1)
c ---    Make sure year is YYYY (Y2K)
         call YR4(io6,jyr,ierr)
         if(ierr.NE.0) stop 'Halted in RDWRITS - see list file'
         call julday(io6,jyr,jmo,jday,jjul)
         if(jhr.EQ.24) then
            jhr = 23
            call indecr(io6,jyr,jjul,jhr,1,0,23)
            call grday(io6,jyr,jjul,jmo,jday)
         endif
         idathr = jyr * 100000 + jjul * 100 + jhr
         if(idathr.LT.jdate) then
c read new record
            goto 17             
         elseif(idathr.EQ.jdate)then

c ---       Check station ID
            if(id.NE.ifstn(k)) then
               write(*,*)'FATAL Error in RDWRITS -- Station ID bad'
               write(io6,*)'FATAL Error in RDWRITS -- Station ID bad'
               write(io6,*)' File number (HUSWO): ',k
               write(io6,*)' Expected Station ID: ',ifstn(k)
               write(io6,*)'    Found Station ID: ',id
               stop
            endif

c     process data:
c     process wind speed and direction:
            if (jdir.eq.999) then
               xwd(index) = 9999.0
               xws(index) = 9999.0
            else
               if (spd.eq.99.9) then
                  xwd(index) = 9999.0
                  xws(index) = 9999.0
               else
                  xwd(index) = jdir
                  if(lhmetric) then
c     wind speed already in m/s
                    xws(index) = spd
                  else
c     convert wind speed from mph to m/s:
                    xws(index) = spd*0.44704
                  endif
               endif
            endif
c     process ceiling height:
            if (jceilb.eq.99999) then
c     missing:
               ixceil(index) = 9999
            elseif (jceilb.eq.88888) then
c     cirroform:
               ixceil(index) = 888
            elseif (jceilb.eq.77777) then
c     unlimited:
               ixceil(index) = 999
            else
               if(lhmetric) then
c     convert from meters to hundreds of feet
                 ixceil(index) = nint(float(jceilb)/30.48)
               else
c     convert from feet to hundreds of feet
                 ixceil(index) = nint(float(jceilb)/100.0)
               endif
            endif
c     process sky cover (use total if opaque is missing):
            if (ioskc.ne.99) then
               ixcc(index) = ioskc
            elseif (itskc.ne.99) then
               ixcc(index) = itskc
            else
               ixcc(index) = 9999
            endif
c     process temperature:
            if (temp.eq.999.9) then
               xtempk(index) = 9999.0
            else
c     convert temperature from Celsius to Kelvin:
               if(lhmetric) then
                 xtempk(index) = temp+273.15
               else
c     convert temperature from Fahrenheit to Kelvin:
                 xtempk(index) = (temp-32)*5./9.+273.15
               endif
            endif
c     process relative humidity
            if (jrh.eq.999) then
c              if dry-bulb and dew point available, calculate
               if(temp.lt.998..and.dewp.lt.998.) then
c           if metric, convert temp and dewp to F
                 if(lhmetric) then
                   temp=temp*9./5. + 32.
                   dewp=dewp*9./5. + 32.
                 endif
                 ixrh(index)=max(1,min(nint(((173.-temp*.1
     &                  +dewp)/(173.+temp*.9))**8),100))
               else
                 ixrh(index) = 9999
               endif
            else
               ixrh(index) = jrh
            endif
c     process pressure:
            if (jpresr.eq.9999) then
               xpres(index) = 9999.0
            else
               if(lhmetric) then
c     pressure already in mBar
                 xpres(index) = float(jpresr)
               else
c     convert from hundreths of inches of Mercury to mBar:
                 xpres(index) = jpresr * 0.01 *inHgtomb
               endif
            endif
c     process precipitation data:
            call pcodes2(io6,idathr,id,pwth,ixpcode(index),nc)
c     count multiple weather occurences
            if (nc.gt.1) npwth = npwth + 1
c     generate weather codes
            ipwout=999
            if(pwth.ne.'99999999') then
               ipwout=0
               do idatsvi=1,3
                  isami=idatsvi*2-1
                  read(pwth(isami:isami+1),fmt='(i2)') ipwtemp
                  if(ipwtemp.ge.10) ipwout(idatsvi)=
     &               hus2dats(ipwtemp)
               enddo
            endif
            if(visb.lt.800.) then
c     convert visibility to miles if metric
              if(lhmetric) visb=min(visb/1.6093,99.9)
            else
              visb=99999.
            endif
         elseif(idathr.gt.jdate) then
c     no data, flag as missing and backspace
            call dedat(ndate,nyr,njul,nhr)
            write(io6,1007) nyr,njul,nhr,id
 1007       format(' No data at:',i4,1x,i3,1x,i2,', Station: ',i10)
            backspace(iofor)
            xws(index) = 9999.0
            xwd(index) = 9999.0
            ixceil(index) = 9999
            ixcc(index) = 9999
            xtempk(index) = 9999.0
            ixrh(index) = 9999
            xpres(index) = 9999.0
            ixpcode(index) = 9999
            ipwout=999
            visb=99999.
         endif
      
c ----------------------
C ISHWO, TD3505, TD9956:      
c ----------------------
      elseif (jdat.ge.5.and.jdat.le.7) then
c        Check if pressure calculation was selected, and attempt empirical
c          calculations
         if(LPCALC.and.LFIRSTRD(index)) then
            lfirstrd(index)=.false.
            call scanishwo(iofor,jdat,xintercep(index),xslope(index),
     *                     altrat(index))
c ---       Report results to list file
            write(io6,*)'Pressure analysis for station ',ifstn(k)
            write(io6,*)'   P(Stn)/P(MSL) = ',xintercep(index),
     &                  ' + T(Stn) * ',xslope(index)
            write(io6,*)'   P(Stn)/P(Altimeter) = ',altrat(index)
            write(io6,*)
         endif
         call GETISH(iofor,id,jyr,jmo,jday,jhr,xwd(index),xws(index),
     *               ixceil(index),ixcc(index),xtempk(index),
     *               ixrh(index),xpres(index),altim,slp,xprate(index),
     *               ixpcode(index),dtow,ipwout,visb,elevx,licc,
     *               istz(k),leof,1,jdat)
         if(leof .AND. altim.GT.9998. .AND. slp.GT.9998.) goto 95
         call julday(io6,jyr,jmo,jday,jjul)
         if(jhr.EQ.24) then
            jhr = 23
            call indecr(io6,jyr,jjul,jhr,1,0,23)
            call grday(io6,jyr,jjul,jmo,jday)
         endif
         idathr = jyr * 100000 + jjul * 100 + jhr
         if(idathr.LT.jdate) then
c     read new record
            goto 17             
         elseif(idathr.EQ.jdate)then

c ---       Check station ID
            if(id.NE.ifstn(k)) then
               write(*,*)'FATAL Error in RDWRITS -- Station ID bad'
               write(io6,*)'FATAL Error in RDWRITS -- Station ID bad'
               if(jdat.EQ.5) then
                  write(io6,*)' File number (ISHWO): ',k
               elseif(jdat.EQ.6) then
                  write(io6,*)'File number (TD3505): ',k
               else
                  write(io6,*)'File number (TD9956): ',k
               endif
               write(io6,*)' Expected Station ID: ',ifstn(k)
               write(io6,*)'    Found Station ID: ',id
               stop
            endif

c           if station pressure is missing, try to calculate it based
c           on altimeter setting first, and then on sea-level pressure.
c           for both, try the empirical calculation first
            if(xpres(index).gt.9998.) then
              if(elevx.lt.-400..or.elevx.gt.9998.) elevx=
     *           elev(k)
              if(altim.lt.9998.) then
                 if(altrat(index).gt.0.) then
                    xpres(index)=altim*altrat(index)
                 else
                    call alp2stp(altim,elevx,xpres(index))
                 endif
              endif
            endif
            if(xpres(index).gt.9998..and.slp.lt.9998.) then
                if(abs(xintercep(index)).lt.999..and.
     *             abs(xintercep(index)).gt.1.e-10) then
                  xpres(index)=(xintercep(index)+xslope(index)*
     *                         xtempk(index))*slp
               else 
                  call slp2stp(slp,elevx,xtempk(index),9999.,
     *                      xpres(index))
               endif
            endif
         else
c     this is used if the whole record is missing.
c     backspace to recover at next hour (bdf)
            call dedat(ndate,nyr,njul,nhr)
            write(io6,1007) nyr,njul,nhr,id
            backspace(iofor)
            xws(index) = 9999.
            xwd(index) = 9999.
            ixceil(index) = 9999
            ixcc(index) = 9999
            xtempk(index) = 9999.
            ixrh(index) = 9999
            xpres(index) = 9999.
            ixpcode(index) = 9999
            xprate(index) = 9999.
            ipwout=999
            visb=99999.
         endif
         
      endif

c     Output the VSRN.DAT line
      if(lvsrnout) then
        if(visb.lt.10.) then
          write(iovsrn,fmt="(i6,6x,i4,4i2,t52,f4.2,
     *      3(2x,i2))") id,jyr,jmo,jday,jhr,0,visb,ipwout
        elseif(visb.lt.100.) then
          write(iovsrn,fmt="(i6,6x,i4,4i2,t52,f4.1,
     *      3(2x,i2))") id,jyr,jmo,jday,jhr,0,visb,ipwout
        else
          write(iovsrn,fmt="(i6,6x,i4,4i2,t52,f4.0,
     *      3(2x,i2))") id,jyr,jmo,jday,jhr,0,visb,ipwout
        endif
      endif

c     END LOOP OVER FORMATTED FILES (STATIONS)
c     ----------------------------------------
 95   continue

C     DECODE CURRENT DATE
      call dedat(ndate,jyr,jjul,jhr)
      call dedat(kdate,iyr,ijul,ihr)
C     WRITE SURFACE DATA TO OUTPUT FILE FOR THIS HOUR
         call wrs(io6,ioform,ntstn,iopack,iosurf,ibuf,jyr,jjul,jhr,
     &            xws,xwd,ixceil,ixcc,xtempk,ixrh,xpres,ixpcode,
     &            ncbdf,lfprint)
c --- Write precipitation rate data to PRECIP.DAT file
         if(LX144 .AND. envpop.EQ.1) call wrp(io6,ntstn,ioprec,
     &                         jyr,jjul,jhr,xprate,ncp,lfprint)
         if(ndate.LT.iedathr)then
C     INCREMENT CURRENT DATE/HR BY ONE HOUR
            call indecr(io6,jyr,jjul,jhr,1,0,23)
            ndate = jyr * 100000 + jjul * 100 + jhr
C     INCREMENT CURRENT DATE/HR FOR EXISTING SURFACE INPUT DATA BY ONE HOUR
            call indecr(io6,iyr,ijul,ihr,1,0,23)
            kdate = iyr * 100000 + ijul * 100 + ihr
         endif
         if(ndate.GT.iedathr) then
            write(io6,300) ndate, iedathr
 300  format(1x,'ERROR in Subr. RDWRITS: NDATE, ',i8,' is greater ',
     1       'than the requested end date (IEDATHR), ',i8)
            write(*,987)
            go to 99
         endif
 55   continue

 99   continue

      if (npwth.gt.0) then
         write(io6,309) npwth
      endif
 309  format (1x,i5,' hours with multiple present weather codes,'
     &     ,/,' priority list is inverse of precipitation code list'
     &     ,' for surf.dat file')

c --- Report counts of missings
      if(LX144 .AND. envpop.EQ.1) then
         write(*,312)
         write(*,311) ncbdf(1),ncbdf(2),ncbdf(3),ncbdf(4),ncbdf(5),
     &                ncbdf(6),ncbdf(7),ncp
         write(io6,312)
         write(io6,311) ncbdf(1),ncbdf(2),ncbdf(3),ncbdf(4),ncbdf(5),
     &                  ncbdf(6),ncbdf(7),ncp
      else
         write(*,310)
         write(*,311) ncbdf(1),ncbdf(2),ncbdf(3),ncbdf(4),ncbdf(5),
     &                ncbdf(6),ncbdf(7)
         write(io6,310)
         write(io6,311) ncbdf(1),ncbdf(2),ncbdf(3),ncbdf(4),ncbdf(5),
     &                  ncbdf(6),ncbdf(7)
      endif

 310  format(' No. Missing Values for',
     &     '    WS    WD ICEIL   ICC TEMPK   IRH  PRES')
 311  format('                        ',8(I5,1X))
 312  format(' No. Missing Values for',
     &     '    WS    WD ICEIL   ICC TEMPK   IRH  PRES  PRATE')

      return
      end
c----------------------------------------------------------------------
      subroutine clouds(io,aceil,acc,jcc,jceil)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 010630              CLOUDS
c ---            Benjamin de Foy
c                Simplified version, does not need leading zeroes.
c
c --- Convert CD144 alphanumeric cloud cover and ceiling height data
c --- to numeric data
c
c --- UPDATES:
c     V5.0(010630) from V4.0(991223) (D. Strimaitis)
c     - Add listfile unit to arguments (remove reference to parameter)
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of list file
c             ACEIL - char*1     - Array (3) of CD144 ceiling ht data
c                                  for this hour
c               ACC - char*1     - Opaque cloud cover for this hour
c --- OUTPUT:
c               JCC - integer    - Numeric cloud cover for this station
c                                  this hour (tenths)
c             JCEIL - integer    - Numeric ceiling height for this
c                                  station this hour (hundreds of feet)
c
c --- CLOUDS called by:  RDWRITS
c --- CLOUDS calls:      none
c----------------------------------------------------------------------
      integer jcc, jceil
      character*1 aceil(3),acc
c use aceil2 to phase out aceil(3)
      character*3 aceil2

c write array of ceiling height characters to a 3 character string
      write(aceil2,'(3A1)') aceil(1),aceil(2),aceil(3)

c --- decode cloud cover
      if (acc.eq.' ')then
         jcc = 9999
      else
c use opaque sky cover
         if (acc.eq.'-') then
            jcc = 10
         else
            read(acc,'(I1)',err=21) jcc
         endif
      endif
      goto 22
 21   continue
      write(io,988) acc
 988  format(/,1x,'Error in subr. CLOUDS2: --- Unallowable character ',
     1     'in opaque sky cover field:  ACC = ',a1)
      write(*,987)
      stop
 22   continue

c --- decode ceiling height
      if (aceil2.eq.'   ') then
c missing ceiling height
         jceil = 9999
      elseif (aceil2.eq.'---') then
c if dashes, assume unlimited
         jceil = 999
      elseif (aceil2.eq.'888') then
c cirroform, avoid dealing with it for now, flag as missing
         jceil = 888
      else
         read(aceil2,'(I3)',err=25) jceil
      endif

      return

 25   continue
      write(io,100) aceil2
 100  format(/,1x,'Error in subr. CLOUDS: --- Unallowable character ',
     1     'in ceiling height field:  ACEIL = ',a3)
      write(*,987)
 987  format(1x,'ERROR in SMERGE run - see SMERGE.LST file')
      stop
      
      end
c----------------------------------------------------------------------
      subroutine pcodes(io,idate,id,iprec,ipcode)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 020308              PCODES
c ---            E. Insley, Earth Tech, Inc.

c                941215->991223: rewritten by B. de Foy
c
c --- Convert CD144 precipitation type data to two-digit precipitation
c     codes.  Multiple Codes are counted and reported.
c     For these cases, ipcode(1) has highest priority, ipcode(5) lowest.
c
c --- UPDATES:
c
c     V5.0(020308) from V5.0(010630) (D. Strimaitis)
c     - Use larger integer format when writing station ID
c
c     V5.0(010630) from V4.0(991223) (D. Strimaitis)
c     - Add listfile unit to arguments (remove reference to parameter)
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of list file
c             IDATE - integer    - Date for this hour YYYYJJJHH
c                ID - integer    - Station id
c             IPREC - integer(5) - Present Weather Codes.
c                                  (CD144: columns 25-29)
c
c --- OUTPUT:
c            IPCODE - integer    - Two-digit precipitation code
c
c --- PCODES called by:  RDWRITS
c --- PCODES calls:  DEDAT
c----------------------------------------------------------------------
      integer idate,id,iprec(5),ipcode
c
      ipcode = 0
      nc = 0
      do i = 5,1,-1
         ip = iprec(i)
         if (ip.ne.0) then
            ipcode = (i-1)*9+ip
            nc = nc + 1
         endif
      enddo

c CHECK FOR MORE THAN 1 TYPE OF PRECIPITATION REPORTED FOR SAME HOUR
      if (nc.gt.1) then
         call dedat(idate,nyr,njul,nhr)
         write(io,1000) nc,iprec,nyr,njul,nhr,id
 1000    format(' Multiple (',i2,') weather codes (',5I1,') at:',
     &        i4,1x,i3,1x,i2,', Station: ',i10)
      endif

      end
c----------------------------------------------------------------------
      subroutine pcodes2(io,idate,id,pwth,ipcode,nc)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 020308             PCODES2
c ---            B. de Foy, Earth Tech, Inc.
c
c
c --- Convert HUSWO weather type to two-digit precipitation codes
c     Priority scheme for multiple codes follows CD-144 convention -
c     lowest ipcode has highest priority
c     Note: there may be some difference between HUSWO and CD-144
c           ipcodes because HUSWO enables combinations of weather codes
c           that are not possible in CD-144, eg: light snow + light
c           snow pellets
c
c --- UPDATES:
c
c     V5.0(020308) from V5.0(010630) (D. Strimaitis)
c     - Use larger integer format when writing station ID
c
c     V5.0(010630) from V4.0(991223) (D. Strimaitis)
c     - Add listfile unit to arguments (remove reference to parameter)
c
c --- INPUTS:
c             IDATE - integer    - Date for this hour YYYYJJJHH
c                ID - integer    - Station id
c              PWTH - char*8     - Weather code (column 15 of HUSWO)
c
c --- OUTPUT:
c            IPCODE - integer    - Two-digit precipitation code
c
c --- OTHER VARIABLES:
c            ITABLE? - integer(10) - ipcode for HUSWO weather types
c
c --- PCODES called by:  RDWRITS
c --- PCODES calls:  DEDAT
c----------------------------------------------------------------------
      integer idate,id,iprec(4),ipcode,ipcoden
      integer itable2(10),itable3(10),itable4(10),itable5(10)
      integer itable6(10),itable9(10)
      character*8 pwth
      data itable2/ 1, 2, 3, 4, 5, 6, 7, 8, 9,99/
      data itable3/99,99,99,13,14,15,16,17,18,99/
      data itable4/19,20,21,22,23,24,26,26,26,99/
      data itable5/28,29,30,99,99,99,34,35,36,99/
      data itable6/37,38,39,41,41,41,44,44,44,99/
      data itable9/37,38,39,99,99,99,99,99,99,99/
c
      read(pwth,100) iprec(1),iprec(2),iprec(3),iprec(4)
 100  format(4I2)

c number of valid codes in pwth:
      nc = 0
c initialise ipcode:
      ipcode = 99

      do nf = 1,4
         ipcoden = 99
         if (iprec(nf).lt.20) then
c
         elseif (iprec(nf).lt.30) then
            ipcoden = itable2(iprec(nf)-19)
            nc = nc + 1
         elseif (iprec(nf).lt.40) then
            ipcoden = itable3(iprec(nf)-29)
            nc = nc + 1
         elseif (iprec(nf).lt.50) then
            ipcoden = itable4(iprec(nf)-39)
            nc = nc + 1
         elseif (iprec(nf).lt.60) then
            ipcoden = itable5(iprec(nf)-49)
            nc = nc + 1
         elseif (iprec(nf).lt.70) then
            ipcoden = itable6(iprec(nf)-59)
            nc = nc + 1
         elseif (iprec(nf).ge.90.and.iprec(nf).le.92) then
            ipcoden = itable9(iprec(nf)-89)
            nc = nc + 1
         endif
c copy ipcoden to ipcode if it has priority over ipcode (in cases of
c multiple codes)
         if (ipcoden.ne.99.and.(ipcoden.lt.ipcode.or.
     &        ipcode.eq.99)) then
            ipcode = ipcoden
         endif
      enddo

c Correct missing flag:
      if (ipcode.eq.99) then
         if (pwth.eq.'99999999') then
            ipcode = 9999
         else
            ipcode = 0
         endif
      endif

c check for multiple weather codes:
c (NB: weather codes < 20 and =>70 do not count for this)
      if (nc.gt.1) then
         call dedat(idate,nyr,njul,nhr)
         write(io,1000) nc,pwth,nyr,njul,nhr,id
 1000    format(' Multiple (',i2,') weather codes (',a8,
     &        ') at:',i4,1x,i3,1x,i2,', Station: ',i10)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine rdhd(iolst,iform,io,ibyr,ibjul,ibhr,
     1                ieyr,iejul,iehr,ibtz,nsta,ipack,maxs,idsta,anem,
     2                cname,clat,clon)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57       Level: 030402                 RDHD
c ---            J. Scire, Earth Tech, Inc.
c
c --- Read the header records from the SURF.DAT file
c
c --- UPDATES:
c     V5.3(030402) from V5.1(020809) (D. Strimaitis)
c     - New header record structure
c     - IENVPOP toggle removed
c     - Moved list-file output from COMP to here
c     V5.1(020809) from V5.0(020308) (D. Strimaitis)
c     - Add IENVPOP to toggle station info records
c     V5.0(020308) from V5.0(010630) (D. Strimaitis)
c     - Add new records for more station information
c     V5.0(010630) from V4.0(010315) (D. Strimaitis)
c     - Add listfile unit to arguments (remove reference to parameter)
c
c --- Updated 010315 from 961014 (D. Strimaitis)
c     - Y2K treatment of year enforced
c
c --- INPUTS:
c             IOLST - integer    - Fortran unit no. of list file
c             IFORM - integer    - Data format flag (0=data not used,
c                                  1=unformatted, 2=formatted)
c                IO - integer    - Fortran unit no. of input file
c              MAXS - integer    - Maximum number of stations
c --- OUTPUT:
c              IBYR - integer    - Beginning year of data (4 digits)
c             IBJUL - integer    - Beginning Julian day number
c              IBHR - integer    - Beginning hour
c              IEYR - integer    - Ending year of data (4 digits)
c             IEJUL - integer    - Ending Julian day number
c              IEHR - integer    - Ending hour
c              IBTZ - integer    - Base time zone (8 = PST, 7 = MST,
c                                  6 = CST, 5 = EST)
c              NSTA - integer    - Number of stations
c             IPACK - integer    - Data packing code (0=unpacked,
c                                  1=packed)
c       IDSTA(MAXS) - int. array - Array of station identification
c                                  codes
c     ANEM(MAXS) - real array    - Anemometer height at each station (m)
c    CNAME(MAXS) - char*4 array  - Name of each station (----)
c     CLAT(MAXS) - char*16 array - Latitude of each station (deg[N/S])
c     CLON(MAXS) - char*16 array - Longitude of each station (deg[E/W])
c
c --- RDHD called by:  RDWRITS
c --- RDHD calls:      DEDAT, YR4, GRDAY
c----------------------------------------------------------------------
c
      real anem(maxs)
      integer idsta(maxs)
      character*4 cname(maxs)
      character*16 clat(maxs),clon(maxs)

      character*4 xyunit
      character*8 datum, pmap
      character*12 daten
      character*16 dataset,dataver
      character*64 datamod
      character*80 comment1

      integer ishift(3)

      write(iolst,81)
81    format(//,26x,'********************',//)
      write(iolst,*)'Data Read from Existing Surface Data Input File:'
      write(iolst,*)
      write(iolst,*)

      if(iform.eq.0)then
c ---    data not used
         nsta=0
         return
c
      else if(iform.eq.1)then
c
c ---    data unformatted
         read(io) dataset,dataver,datamod
c ---    Check for valid dataset name
         if(dataset.NE.'SURF.DAT') then
            write(iolst,*)
            write(iolst,*) 'RDHD:  Invalid previous SURF.DAT file'
            write(iolst,*) 'Dataset name found = ',dataset
            write(iolst,*) 'Dataset name expected = ','SURF.DAT'
            write(*,987)
            stop
         endif
         read(io) ncomment
         write(iolst,'(2a16,a64)') dataset,dataver,datamod
         write(iolst,'(i4)') ncomment
         do i=1,ncomment
            read(io) comment1
            write(iolst,'(a80)') comment1
         enddo
         read(io) pmap
         write(iolst,'(a8)') pmap
         if(pmap.EQ.'NONE   ') then
            read(io) ibeg,iend,ibtz,nsta,ipack
            read(io) (idsta(n),n=1,nsta)
         else
            read(io) datum,daten
            read(io) xyunit
            write(iolst,'(a8,a12)') datum,daten
            write(iolst,'(a4)') xyunit
            read(io) ibeg,iend,ibtz,nsta,ipack
            read(io) (idsta(n),n=1,nsta)
            read(io) (cname(n),n=1,nsta)
            read(io) (clat(n),n=1,nsta)
            read(io) (clon(n),n=1,nsta)
            read(io) (anem(n),n=1,nsta)
         endif
c
c ---    decode starting and ending dates
         call dedat(ibeg,ibyr,ibjul,ibhr)
         call dedat(iend,ieyr,iejul,iehr)
c
      else if(iform.eq.2)then
         ipack=0
         read(io,'(2a16,a64)') dataset,dataver,datamod
         if(dataset.NE.'SURF.DAT') then
            write(iolst,*)
            write(iolst,*) 'RDHD:  Invalid previous SURF.DAT file'
            write(iolst,*) 'Dataset name found = ',dataset
            write(iolst,*) 'Dataset name expected = ','SURF.DAT'
            write(*,987)
            stop
         endif
         read(io,*) ncomment
         write(iolst,'(2a16,a64)') dataset,dataver,datamod
         write(iolst,'(i4)') ncomment
         do i=1,ncomment
            read(io,'(a80)') comment1
            write(iolst,'(a80)') comment1
         enddo
         read(io,'(a8)') pmap
         write(iolst,'(a8)') pmap
         if(pmap.EQ.'NONE   ') then
            read(io,*)ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,nsta
            read(io,*)(idsta(n),n=1,nsta)
         else
            read(io,'(a8,a12)') datum,daten
            read(io,'(a4)') xyunit
            write(iolst,'(a8,a12)') datum,daten
            write(iolst,'(a4)') xyunit
            read(io,*)ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,nsta
            do n=1,nsta
               read(io,*)idsta(n),cname(n),clat(n),clon(n),anem(n)
            enddo
         endif
      else
         write(iolst,12)iform
12       format(//2x,'ERROR IN SUBR. RDHD -- invalid value of IFORM'/
     1   5x,'IFORM = ',i10)
         write(*,987)
         stop
      endif

c --- Make sure years are YYYY (Y2K)
      call YR4(iolst,ibyr,ierr)
      if(ierr.NE.0) stop 'Halted in RDHD - see list file'
      call YR4(iolst,ieyr,ierr)
      if(ierr.NE.0) stop 'Halted in RDHD - see list file'

C  WRITE OUT REMAINING SURFACE DATA INPUT FILE HEADER INFORMATION
        write(iolst,90) ibtz, iform, ipack
90      format(3x,'Time Zone:', i6,/,3x,'File Format',
     2         ' (1=unformatted,2=formatted):',i3,/,3x,'Packing Code:',
     3         i3)
        call GRDAY(iolst,ibyr,ibjul,ibmo,ibday)
        call GRDAY(iolst,ieyr,iejul,iemo,ieday)
        write(iolst,100) ibtz,ibmo,ibday,ibyr,ibhr,iemo,ieday,ieyr,iehr
100     format(/,3x,'Period (in time zone',i2,'):   ',i2,'/',i2,'/',
     1         i4,2x,i2,':00','  to  ',i2,'/',i2,'/',i4,2x,i2,':00')

C       WRITE THE AVAILABLE STATION NUMBERS IN THE EXISTING SURFACE
C       DATA INPUT FILE
C       SET VARIABLES FOR WRITING OUT IN 4 COLUMNS
C        J4 IS NO. ROWS IN A "SHORT" COLUMN
C        J5 IS NO. ROWS IN A "LONG" COLUMN
C        J6 IS THE NUMBER OF "LONG" COLUMNS
        j4 = nsta/4
        j6 = mod(nsta,4)
        if(j6.EQ.0)then
           j5 = j4
        else
           j5 = j4 + 1
        endif
        ishift(1) = j5
        do i=2,3
           if(i.LE.j6)then
              ishift(i) = ishift(i-1) + j5
           else
              ishift(i) = ishift(i-1) + j4
           endif
        enddo
        ncol = min0(nsta,4)
        write(iolst,120) (' ',k=1,ncol)
120     format( /,3x,'Stations Available in Existing Surface Data ',
     1         'Input File:  '/ 3x,4(a1,'No.',7x,'ID',8x)/)
        do i=1,j4
           i2 = i + ishift(1)
           i3 = i + ishift(2)
           i4 = i + ishift(3)
           write(iolst,130) i,idsta(i),i2,idsta(i2),i3,idsta(i3),i4,
     1                    idsta(i4)
130        format(3x,4(i3,2x,i10,6x))
        enddo
        if(j6.GT.0)then
           n1 = j5
           n2 = n1+(j6-1)*j5
           write(iolst,130) (k,idsta(k),k=n1,n2,j5)
        endif
      write(iolst,81)

      return
987   format(1x,'ERROR in SMERGE run - see SMERGE.LST file')
      end
c----------------------------------------------------------------------
      subroutine wrhd(ienvpop,iolst,iform,io,ibyr,ibjul,ibhr,
     1                ieyr,iejul,iehr,ibtz,nsta,ipack,idsta,anem,
     2                cname,clat,clon,cver,clev)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 030402                WRHD
c ---            J. Scire, Earth Tech, Inc.

c
c --- Write the header records for a meteorological data file
c
c --- UPDATES:
c     V5.3(030402) from V5.1(020809) (D. Strimaitis)
c     - New header record structure
c     V5.1(020809) from V5.0(020308) (D. Strimaitis)
c     - Add IENVPOP to toggle station info records
c     V5.0(020308) from V5.0(010630) (D. Strimaitis)
c     - Add new records for more station information
c     V5.0(010630) from V4.0(010315) (D. Strimaitis)
c     - Add listfile unit to arguments (remove reference to parameter)
c
c --- Updated 010315 from 961014 (D. Strimaitis)
c     - YYYY format for year
c
c --- INPUTS:
c           IENVPOP - integer    - Environment toggle for headers
c             IOLST - integer    - Fortran unit no. of list file
c             IFORM - integer    - Data format flag (1=unformatted,
c                                  2=formatted)
c                IO - integer    - Fortran unit no. of data file
c              IBYR - integer    - Beginning year of data (4 digits)
c             IBJUL - integer    - Beginning Julian day number
c              IBHR - integer    - Beginning hour
c              IEYR - integer    - Ending year of data (4 digits)
c             IEJUL - integer    - Ending Julian day number
c              IEHR - integer    - Ending hour
c              IBTZ - integer    - Base time zone (8 = PST, 7 = MST,
c                                  6 = CST, 5 = EST)
c              NSTA - integer    - Number of stations
c             IPACK - integer    - Data packing code (0=unpacked,
c                                  1=packed)
c       IDSTA(NSTA) - int. array - Array of station identification
c                                  codes
c     ANEM(NSTA) - real array    - Anemometer height at each station (m)
c    CNAME(NSTA) - char*4 array  - Name of each station (----)
c     CLAT(NSTA) - char*16 array - Latitude of each station (deg[N/S])
c     CLON(NSTA) - char*16 array - Longitude of each station (deg[E/W])
c           CVER - character*12  - Version of processor
c           CLEV - character*12  - Level of processor
c
c --- OUTPUT:  none
c
c --- WRHD called by:  RDWRITS
c --- WRHD calls:      none
c----------------------------------------------------------------------
c --- Local Variables
      real anem(nsta)
      integer idsta(nsta)
      character*4 cname(nsta)
      character*16 clat(nsta),clon(nsta)
      character*12 cver,clev
      character*1 q

      character*4 xyunit
      character*8 datum, pmap
      character*12 daten
      character*16 dataset,dataver
      character*64 datamod
      character*80 comment1

c --- Configure output variables
      data dataset/'SURF.DAT'/, dataver/'2.0'/
      data datamod/'Header structure with coordinate parameters'/
      data ncomment/1/
      data comment1/'Produced by SMERGE Version: '/

c --- Set single quote character
      data q/''''/

c --- Construct the version-level comment string
      j=29
      do i=1,12
         if(cver(i:i).NE.' ') then
            comment1(j:j)=cver(i:i)
            j=j+1
         endif
      enddo
      j=j+1
      comment1(j:j+7)=' Level: '
      j=j+8
      do i=1,12
         if(clev(i:i).NE.' ') then
            comment1(j:j)=clev(i:i)
            j=j+1
         endif
      enddo

c --- Set map projection information
      if(ienvpop.EQ.0) then
         pmap='NONE    '
      elseif(ienvpop.EQ.1) then
         pmap='LL      '
         datum='WGS-G   '
         daten='10-10-2002  '
         xyunit='DEG '
      endif

c --- Write records
      if(iform.eq.1)then
c ---    Unformatted file
c ---    code beginning and ending date/hour
         ibeg=ibyr*100000+ibjul*100+ibhr
         iend=ieyr*100000+iejul*100+iehr
         write(io) dataset,dataver,datamod
         write(io) ncomment
         write(io) comment1
         write(io) pmap
         if(ienvpop.EQ.0) then
            write(io)ibeg,iend,ibtz,nsta,ipack
            write(io)idsta
         elseif(ienvpop.EQ.1) then
            write(io) datum,daten
            write(io) xyunit
            write(io)ibeg,iend,ibtz,nsta,ipack
            write(io)idsta
            write(io)cname
            write(io)clat
            write(io)clon
            write(io)anem
         endif

      else if(iform.eq.2)then
c ---    Formatted file
         write(io,'(2a16,a64)') dataset,dataver,datamod
         write(io,'(i4)') ncomment
         write(io,'(a80)') comment1
         write(io,'(a8)') pmap
         if(ienvpop.EQ.0) then
            write(io,10)ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,nsta
10          format(2(i6,2i4),2i5)
            do i=1,nsta
               write(io,20)idsta(i)
            enddo
20          format(i8)
         elseif(ienvpop.EQ.1) then
            write(io,'(a8,a12)') datum,daten
            write(io,'(a4)') xyunit
            write(io,10)ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,nsta
            do i=1,nsta
               write(io,21)idsta(i),q,cname(i),q,q,clat(i),q,q,
     &                     clon(i),q,anem(i)
            enddo
21          format(i8,2x,a1,a4,a1,2x,2(a1,a16,a1,2x),f10.2)
         endif
      else
         write(iolst,12)iform
12       format(//2x,'ERROR IN SUBR. WRHD -- invalid value of IFORM'/
     1   5x,'IFORM = ',i10)
         write(*,987)
987      format(1x,'ERROR in SMERGE run - see SMERGE.LST file')
         stop
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrhdp(ienvpop,io,ibyr,ibjul,ibhr,ieyr,iejul,iehr,
     1                 ibtz,nsta,idsta,cname,clat,clon,
     2                 cver,clev)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 030402               WRHDP
c ---            Adapted from WRHD
c
c --- Write the header records for a FORMATTED precipitation data file
c
c --- UPDATES:
c     V5.3(030402) from V5.1(020809) (D. Strimaitis)
c     - New header record structure
c     V5.1(020809) from V5.0(020308) (D. Strimaitis)
c     - Add IENVPOP to toggle station info records
c
c --- INPUTS:
c           IENVPOP - integer    - Environment toggle for headers
c                IO - integer    - Fortran unit no. of data file
c              IBYR - integer    - Beginning year of data (4 digits)
c             IBJUL - integer    - Beginning Julian day number
c              IBHR - integer    - Beginning hour
c              IEYR - integer    - Ending year of data (4 digits)
c             IEJUL - integer    - Ending Julian day number
c              IEHR - integer    - Ending hour
c              IBTZ - integer    - Base time zone (8 = PST, 7 = MST,
c                                  6 = CST, 5 = EST)
c              NSTA - integer    - Number of stations
c       IDSTA(NSTA) - int. array - Array of station identification
c                                  codes
c    CNAME(NSTA) - char*4 array  - Name of each station (----)
c     CLAT(NSTA) - char*16 array - Latitude of each station (deg[N/S])
c     CLON(NSTA) - char*16 array - Longitude of each station (deg[E/W])
c           CVER - character*12  - Version of processor
c           CLEV - character*12  - Level of processor
c
c --- OUTPUT:  none
c
c --- WRHDP called by:  RDWRITS
c --- WRHDP calls:      none
c----------------------------------------------------------------------
c --- Local Variables
      integer idsta(nsta)
      character*4 cname(nsta)
      character*16 clat(nsta),clon(nsta)
      character*12 cver,clev
      character*1 q

      character*4 xyunit
      character*8 datum, pmap
      character*12 daten
      character*16 dataset,dataver
      character*64 datamod
      character*80 comment1

c --- Configure output variables
      data dataset/'PRECIP.DAT'/, dataver/'2.0'/
      data datamod/'Header structure with coordinate parameters'/
      data ncomment/1/
      data comment1/'Produced by SMERGE Version '/

c --- Set single quote character
      data q/''''/

c --- Construct the version-level comment string
      j=28
      do i=1,12
         if(cver(i:i).NE.' ') then
            comment1(j:j)=cver(i:i)
            j=j+1
         endif
      enddo
      j=j+1
      comment1(j:j)='('
      j=j+1
      do i=1,12
         if(clev(i:i).NE.' ') then
            comment1(j:j)=clev(i:i)
            j=j+1
         endif
      enddo
      comment1(j:j)=')'

c --- Set map projection information
      if(ienvpop.EQ.0) then
         pmap='NONE    '
      elseif(ienvpop.EQ.1) then
         pmap='LL      '
         datum='WGS-G   '
         daten='10-10-2002  '
         xyunit='DEG '
      endif

c --- Write records

c ---    Formatted file
         write(io,'(2a16,a64)') dataset,dataver,datamod
         write(io,'(i4)') ncomment
         write(io,'(a80)') comment1
         write(io,'(a8)') pmap
         if(ienvpop.EQ.0) then
            write(io,10)ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,nsta
10          format(2(i6,2i4),2i5)
            do i=1,nsta
               write(io,20)idsta(i)
            enddo
20          format(i8)
         elseif(ienvpop.EQ.1) then
            write(io,'(a8,a12)') datum,daten
            write(io,'(a4)') xyunit
            write(io,10)ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,nsta
            do i=1,nsta
              write(io,21)idsta(i),q,cname(i),q,q,clat(i),q,q,clon(i),q
            enddo
21          format(i8,2x,a1,a4,a1,2x,2(a1,a16,a1,2x))
         endif

      return
      end
c----------------------------------------------------------------------
      subroutine rds(iolst,iforms,nssta,ispack,io,iskip,ibuf,
     1    iyr,ijul,ihr,ws,wd,iceil,icc,tempk,irh,pres,ipcode)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 010630                 RDS
c ---            J. Scire, SRC
c
c --- Read a record of surface meteorological data
c --- (missing value indicator = 9999. (real) or 9999 (integer))
c --- If packing is used, data will be unpacked before returning
c
c --- UPDATES:
c     V5.0(010630) from V4.0(010315) (D. Strimaitis)
c     - Add listfile unit to arguments (remove reference to parameter)
c
c --- Updated 010315 from 961014 (D. Strimaitis)
c     - Y2K treatment of year enforced
c
c --- INPUTS:
c             IOLST - integer    - Fortran unit no. of list file
c            IFORMS - integer    - Data format flag (1=unformatted,
c                                  2=formatted)
c             NSSTA - integer    - Number of surface stations
c            ISPACK - integer    - Data packing code (0=unpacked,
c                                  1=packed)
c                IO - integer    - Fortran unit no. of surface data
c             ISKIP - integer    - Flag controlling unpacking of data
c                                  if ISKIP=0 data are unpacked, if
c                                  ISKIP=1, data are read but not
c                                  unpacked (used only if ISPACK=1)
c     IBUF(3,NSSTA) - int. array - Buffer to temporarily store packed
c                                  data (used only if ISPACK = 1)
c
c --- OUTPUT:
c               IYR - integer    - Year of surface data (4 digits)
c              IJUL - integer    - Julian day of data
c               IHR - integer    - Hour of data
c         WS(NSSTA) - real array - Wind speed (m/s)
c         WD(NSSTA) - real array - Wind direction (degrees)
c      ICEIL(NSSTA) - int. array - Ceiling height (hundreds of ft)
c        ICC(NSSTA) - int. array - Opaque sky cover (tenths)
c      TEMPK(NSSTA) - real array - Temperature (deg. K)
c        IRH(NSSTA) - int. array - Relative humidity (percent)
c       PRES(NSSTA) - real array - Surface station pressure (mb)
c     IPCODE(NSSTA) - int. array - Precipitation code
c
c --- RDS called by:  RDWRITS
c --- RDS calls:    UNPCKS
c                   DEDAT
c                   YR4
c----------------------------------------------------------------------
c
      real ws(nssta),wd(nssta),tempk(nssta),pres(nssta)
c
      integer iceil(nssta),icc(nssta),irh(nssta),ipcode(nssta)
      integer ibuf(3,nssta)

      if(iforms.eq.1)then
c
c ---    unformatted data
         if(ispack.eq.0)then
c ---       read unpacked, unformatted data
            read(io)idathr,ws,wd,iceil,icc,tempk,irh,pres,ipcode
         else
c ---       read packed, unformatted data --
c ---       date/hr word + 3 words/station
            read(io)idathr,ibuf
c ---       unpack data (unless in skip mode)
            if(iskip.eq.0)call unpcks(nssta,ibuf,ws,wd,iceil,icc,
     1      tempk,irh,pres,ipcode)
         endif
c ---    decode date and time
         call dedat(idathr,iyr,ijul,ihr)
c
      else if(iforms.eq.2)then
c
c ---    formatted data
c-emi **** Modified by EMI 11/18/94 ****
c dgs -- Removed EMI change for the more general SURF.DAT record 3/1/99
         read(io,*)iyr,ijul,ihr,(ws(n),wd(n),iceil(n),icc(n),tempk(n),
     1   irh(n),pres(n),ipcode(n),n=1,nssta)
c         read(io,*)iyr,ijul,ihr
c         read(io,*) (ws(n),wd(n),iceil(n),icc(n),tempk(n),
c     1               irh(n),pres(n),ipcode(n),n=1,nssta)
c-emi **** End of EMI Modification ****
c
      else
         write(iolst,12)iforms
12       format(//2x,'ERROR IN SUBR. RDS -- invalid value of IFORMS'/
     1   5x,'IFORMS = ',i10)
         write(*,987)
987      format(1x,'ERROR in SMERGE run - see SMERGE.LST file')
         stop
      endif

c --- Make sure year is YYYY (Y2K)
      call YR4(iolst,iyr,ierr)
      if(ierr.NE.0) stop 'Halted in RDS - see list file'

      return
      end
c----------------------------------------------------------------------
      subroutine unpcks(nssta,ibuf,ws,wd,iceil,icc,tempk,irh,pres,
     1 ipcode)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 941215              UNPCKS
c ---            J. Scire, SRC
c
c --- Unpack an array of surface meteorological data using integer
c --- packing (3 words/station)
c        Word 1:  TTTTPCRRR --  TTTT = temp. (XXX.X deg, K)
c                                 PC = precipitation code (XX)
c                                RRR = relative humidity (XXX. %)
c        Word 2: pPPPPCCWWW -- pPPPP = station pressure (pXXX.X mb,
c                                      with p = 0 or 1 only)
c                                 CC = opaque sky cover (XX tenths)
c                                WWW = wind direction (XXX. deg.)
c        Word 3:   HHHHSSSS --  HHHH = ceiling height (XXXX. hundreds
c                                      of feet)
c                               SSSS = wind speed (XX.XX m/s)
c
c --- INPUTS:
c             NSSTA - integer    - Number of surface stations
c     IBUF(3,NSSTA) - int. array - Array of packed data
c
c --- OUTPUT:
c         WS(NSSTA) - real array - Wind speed (m/s)
c         WD(NSSTA) - real array - Wind direction (degrees)
c      ICEIL(NSSTA) - int. array - Ceiling height (hundreds of ft)
c        ICC(NSSTA) - int. array - Opaque sky cover (tenths)
c      TEMPK(NSSTA) - real array - Temperature (deg. K)
c        IRH(NSSTA) - int. array - Relative humidity (percent)
c       PRES(NSSTA) - real array - Surface station pressure (mb)
c     IPCODE(NSSTA) - int. array - Precipitation code
c
c --- UNPCKS called by:  RDS
c --- UNPCKS calls:      none
c----------------------------------------------------------------------
c
      real ws(nssta),wd(nssta),tempk(nssta),pres(nssta)
c
      integer iceil(nssta),icc(nssta),irh(nssta),ipcode(nssta)
      integer ibuf(3,nssta)
c
      do 100 i=1,nssta
c
      iword1=ibuf(1,i)
      iword2=ibuf(2,i)
      iword3=ibuf(3,i)
c
c --- unpack temperature, precip. code, relative humidity
      it=iword1/100000
      ipc=iword1/1000-it*100
      ir=iword1-it*100000-ipc*1000
c
c --- use a standard missing value indicator of 9999 for all variables
      if(it.eq.9999)then
         tempk(i)=9999.
      else
         tempk(i)=float(it)/10.
      endif
c
      if(ipc.eq.99)then
         ipcode(i)=9999
      else
         ipcode(i)=ipc
      endif
c
      if(ir.eq.999)then
         irh(i)=9999
      else
         irh(i)=ir
      endif
c
c --- unpack station pressure, cloud cover, wind direction
      ip=iword2/100000
      ic=iword2/1000-ip*100
      iw=iword2-ip*100000-ic*1000
c --- NOTE: 1XXXX is allowed for station pressure and converts to
c ---       1XXX.X mb
      if(ip.eq.9999)then
         pres(i)=9999.
      else
         pres(i)=float(ip)/10.
      endif
c
      if(ic.eq.99)then
         icc(i)=9999
      else
         icc(i)=ic
      endif
c
      if(iw.eq.999)then
         wd(i)=9999.
      else
         wd(i)=iw
      endif
c
c --- unpack ceiling height, wind speed
      ih=iword3/10000
      is=iword3-ih*10000
c
      iceil(i)=ih
      if(is.eq.9999)then
         ws(i)=9999.
      else
         ws(i)=float(is)/100.
      endif
100   continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrs(iolst,iforms,nssta,ispack,io,ibuf,iyr,ijul,ihr,
     1          ws,wd,iceil,icc,tempk,irh,pres,ipcode,ncbdf,lfprint)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 040922                 WRS
c ---            J. Scire, SRC
c ---            Modified by E. Insley, SRC   3/30/94
c
c --- UPDATES:
c     V5.51(040922) from V5.0(010630) (K. Morrison)
c     - Change dimension of ncbdf from 8 to 7 to be consistent with
c       RDWRITS
c     V5.0(010630) from V4.0(010315) (D. Strimaitis)
c     - Add listfile unit to arguments (remove reference to parameter)
c
c --- Updated 010315 from 991223 (D. Strimaitis)
c     - YYYY format for year (formats do not change!)
c
c --- Updated 991223 from 940330 (B. de Foy)
c     - Count hours where a variable is missing at all stations
c
c --- Write a record of surface meteorological data (one hour of data)
c --- (missing value indicator = 9999. (real) or 9999 (integer))
c --- Option to pack data if output data file is unformatted
c
c --- INPUTS:
c             IOLST - integer    - Fortran unit no. list file
c            IFORMS - integer    - Data format flag (1=unformatted,
c                                  2=formatted)
c             NSSTA - integer    - Number of surface stations
c            ISPACK - integer    - Data packing code (0=unpacked,
c                                  1=packed)
c                IO - integer    - Fortran unit no. of surface data
c     IBUF(3,NSSTA) - int. array - Buffer to temporarily store packed
c               IYR - integer    - Year of surface data (4 digits)
c              IJUL - integer    - Julian day of data
c               IHR - integer    - Hour of data
c         WS(NSSTA) - real array - Wind speed (m/s)
c         WD(NSSTA) - real array - Wind direction (degrees)
c      ICEIL(NSSTA) - int. array - Ceiling height (hundreds of ft)
c        ICC(NSSTA) - int. array - Opaque sky cover (tenths)
c      TEMPK(NSSTA) - real array - Temperature (deg. K)
c        IRH(NSSTA) - int. array - Relative humidity (percent)
c       PRES(NSSTA) - real array - Surface station pressure (mb)
c     IPCODE(NSSTA) - int. array - Precipitation code
c          ncbdf(7) - int. array - count no. of missing or replaced hrs
c            lfprint - logical - .true. : print changes to screen
c
c --- OUTPUT:  none
c
c --- WRS called by:  RDWRITS
c --- WRS calls:      PACKS
c----------------------------------------------------------------------
c
      real ws(nssta),wd(nssta),tempk(nssta),pres(nssta)
c
      integer iceil(nssta),icc(nssta),irh(nssta),ipcode(nssta)
      integer ibuf(3,nssta)
      integer ncbdf(7)

      logical lfprint

c if fill in of gaps disabled, count the number of gaps
c
c check for missing wind speed data - bdf
      do i = 1,nssta
         if (ws(i).ne.9999.0) goto 300
      enddo
      ncbdf(1) = ncbdf(1)+1
      if (lfprint.and.ncbdf(1).le.100) then
         write(iolst,40) 'wind speed',iyr,ijul,ihr
      endif
 40   format('Missing ',a15,' at: ',i4,1x,i3,1x,i2)
 300  continue
c check for missing wind direction data - bdf
      do i = 1,nssta
         if (wd(i).ne.9999.0) goto 301
      enddo
      ncbdf(2) = ncbdf(2)+1
      if (lfprint.and.ncbdf(2).le.100) then
         write(iolst,40) 'wind direction',iyr,ijul,ihr
      endif
 301  continue
c check for missing temperature data - bdf
      do i = 1,nssta
         if (tempk(i).ne.9999.0) goto 302
      enddo
      ncbdf(5) = ncbdf(5)+1
      if (lfprint.and.ncbdf(5).le.100) then
         write(iolst,40) 'temperature',iyr,ijul,ihr
      endif
 302  continue
c check for missing pressure data - bdf
      do i = 1,nssta
         if (pres(i).ne.9999.0) goto 303
      enddo
      ncbdf(7) = ncbdf(7)+1
      if (lfprint.and.ncbdf(7).le.100) then
         write(iolst,40) 'pressure',iyr,ijul,ihr
      endif
 303  continue
c check for missing sky ceiling data - bdf
      do i = 1,nssta
         if (iceil(i).ne.9999) goto 304
      enddo
      ncbdf(3) = ncbdf(3)+1
      if (lfprint.and.ncbdf(3).le.100) then
         write(iolst,40) 'sky ceiling',iyr,ijul,ihr
      endif
 304  continue
c check for missing sky cover data - bdf
      do i = 1,nssta
         if (icc(i).ne.9999) goto 305
      enddo
      ncbdf(4) = ncbdf(4)+1
      if (lfprint.and.ncbdf(4).le.100) then
         write(iolst,40) 'sky cover',iyr,ijul,ihr
      endif
 305  continue
c check for missing RH data - bdf
      do i = 1,nssta
         if (irh(i).ne.9999) goto 306
      enddo
      ncbdf(6) = ncbdf(6)+1
      if (lfprint.and.ncbdf(6).le.100) then
         write(iolst,40) 'RH',iyr,ijul,ihr
      endif
 306  continue


      if(iforms.eq.1)then
c ---    Unformatted option
c ---    code date and time into a single integer variable
         idathr=iyr*100000+ijul*100+ihr
c
         if(ispack.eq.0)then
c
c ---       write unpacked data
            write(io)idathr,ws,wd,iceil,icc,tempk,irh,pres,ipcode
         else
c
c ---       pack and write data
            call packs(nssta,ws,wd,iceil,icc,tempk,irh,pres,ipcode,
     1      ibuf)
            write(io)idathr,ibuf
         endif
      else if(iforms.eq.2)then
c ---    Formatted option
c-emi **** Modified by EMI 3/30/94 ****
c-emi    write(io,*)iyr,ijul,ihr,(ws(n),wd(n),iceil(n),icc(n),
c-emi1   tempk(n),irh(n),pres(n),ipcode(n),n=1,nssta)
         write(io,10)iyr,ijul,ihr
10       format(3i4)
         write(io,20) (ws(n),wd(n),iceil(n),icc(n),tempk(n),irh(n),
     1                 pres(n),ipcode(n),n=1,nssta)
 20      format(1x,f8.3,1x,f8.3,1x,i4,1x,i4,1x,f8.3,1x,i4,1x,
     &          f8.3,1x,i4)
c 20      format((2f12.6,2i5,f12.6,i5,f12.6,i5))
c-emi **** End of EMI Modification ****
      else
         write(iolst,30)iforms
30       format(//2x,'ERROR IN SUBR. WRS -- invalid value of IFORMS'/
     1   5x,'IFORMS = ',i10)
         write(*,987)
987      format(1x,'ERROR in SMERGE run - see SMERGE.LST file')
         stop
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrp(iolst,nssta,io,iyr,ijul,ihr,prate,ncbdf,lfprint)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 020308                 WRP
c ---            Adapted from WRS
c
c --- INPUTS:
c             IOLST - integer    - Fortran unit no. list file
c             NSSTA - integer    - Number of surface stations
c                IO - integer    - Fortran unit no. of surface data
c               IYR - integer    - Year of surface data (4 digits)
c              IJUL - integer    - Julian day of data
c               IHR - integer    - Hour of data
c      PRATE(NSSTA) - real array - Precipitation rate (mm/hr)
c             ncbdf - integer    - count no. of missing or replaced hrs
c            lfprint - logical - .true. : print changes to screen
c
c --- OUTPUT:  none
c
c --- WRP called by:  RDWRITS
c --- WRP calls:      none
c----------------------------------------------------------------------
c
      real prate(nssta)
      integer ncbdf
      logical lfprint

c --- Check for missing precip data
      do i = 1,nssta
         if (prate(i).ne.9999.0) goto 300
      enddo
      ncbdf = ncbdf+1
      if (lfprint.and.ncbdf.le.100) then
         write(iolst,40) 'Precip',iyr,ijul,ihr
      endif
40    format('Missing ',a15,' at: ',i4,1x,i3,1x,i2)
300   continue

c --- Write record
      write(io,10)iyr,ijul,ihr,prate
10    format(3i4,1x,10f9.3,24(/,13x,10f9.3))

      return
      end
c----------------------------------------------------------------------
      subroutine packs(nssta,ws,wd,iceil,icc,tempk,irh,pres,ipcode,
     1 ibuf)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 941215               PACKS
c ---            J. Scire, SRC
c
c --- Pack a set of surface meteorological variables using integer
c --- packing (3 words/station)
c        Word 1:  TTTTPCRRR --  TTTT = temp. (XXX.X deg, K)
c                                 PC = precipitation code (XX)
c                                RRR = relative humidity (XXX. %)
c        Word 2: pPPPPCCWWW -- pPPPP = station pressure (pXXX.X mb,
c                                      with p = 0 or 1 only)
c                                 CC = opaque sky cover (XX tenths)
c                                WWW = wind direction (XXX. deg.)
c        Word 3:   HHHHSSSS --  HHHH = ceiling height (XXXX. hundreds
c                                      of feet)
c                               SSSS = wind speed (XX.XX m/s)
c
c --- INPUTS:
c             NSSTA - integer    - Number of surface stations
c         WS(NSSTA) - real array - Wind speed (m/s)
c         WD(NSSTA) - real array - Wind direction (degrees)
c      ICEIL(NSSTA) - int. array - Ceiling height (hundreds of ft)
c        ICC(NSSTA) - int. array - Opaque sky cover (tenths)
c      TEMPK(NSSTA) - real array - Temperature (deg. K)
c        IRH(NSSTA) - int. array - Relative humidity (percent)
c       PRES(NSSTA) - real array - Surface station pressure (mb)
c     IPCODE(NSSTA) - int. array - Precipitation code
c
c --- OUTPUT:
c     IBUF(3,NSSTA) - int. array - Array of packed data
c
c --- PACKS called by:  WRS
c --- PACKS calls:      none
c----------------------------------------------------------------------
      real ws(nssta),wd(nssta),tempk(nssta),pres(nssta)
c
      integer iceil(nssta),icc(nssta),irh(nssta),ipcode(nssta)
      integer ibuf(3,nssta)
c
      do 100 i=1,nssta
c
c --- pack temperature, precip. code, relative humidity into word 1
      if(tempk(i).eq.9999.)then
         it=9999
      else
         it=10.*tempk(i)+0.5
      endif
c
      if(ipcode(i).eq.9999)then
         ipc=99
      else
         ipc=ipcode(i)
      endif
c
      if(irh(i).eq.9999)then
         ir=999
      else
         ir=irh(i)
      endif
      ibuf(1,i)=it*100000+ipc*1000+ir
c
c --- pack station pressure, cloud cover, wind direction into
c --- word 2
      if(pres(i).eq.9999.)then
         ip=9999
      else
c ---    NOTE: 1XXX.X mb allowed for station pressure and converts to
c ---          1XXXX
         ip=10.*pres(i)+0.5
      endif
c
      if(icc(i).eq.9999)then
         ic=99
      else
         ic=icc(i)
      endif
c
      if(wd(i).eq.9999.)then
         iw=999
      else
         iw=wd(i)+0.5
      endif
      ibuf(2,i)=ip*100000+ic*1000+iw
c
c --- pack ceiling height, wind speed into word 3
      ih=iceil(i)
c
      if(ws(i).eq.9999.)then
         is=9999
      else
         is=100.*ws(i)+0.5
      endif
      ibuf(3,i)=ih*10000+is
100   continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine chkhushd(io,hushd)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 010630            CHKHUSHD
c ---            Benjamin de Foy, Earth Tech, Inc.
c
c --- Checks that HUSWO file contains all data records.
c     Read statement in rdwrits does not allow for missing data fields.
c     Stops if check failed.
c
c --- UPDATES:
c     V5.0(010630) from V4.0(991223) (D. Strimaitis)
c     - Add listfile unit to arguments (remove reference to parameter)
c 
c --- INPUT:
c     hushd    - char*120 - Header of HUSWO file
c
c --- OTHER VARIABLES:
c     nmax     - integer  - length of header in characters
c     indexmax - integer  - maximum number of fields in HUSWO file
c
c     CHKHUSHD called by: COMP
c----------------------------------------------------------------------
c
      character*120 hushd
      integer j,n,nmax,index,indexref,indexmax
      parameter(nmax=120,indexmax=20)

      n = 1
      indexref = 1

 100  continue
c skip blanks
      if (hushd(n:n).eq.' ') then
         n = n + 1
         if (n.gt.nmax) goto 999
         goto 100
      endif
c find a number
      j = 1
 101  continue
      if (hushd(n+j:n+j).ne.' ') then
         j = j + 1
         if (n+j.gt.nmax) goto 999
         goto 101
      endif
c read the number
      read(hushd(n:n+j),*) index
      if (index.eq.indexref) then
         if (index.eq.indexmax) then
c we are done and may return
            return
         else
c look for next index
            indexref = indexref+1
            n = n+j+1
            goto 100
         endif
      endif

 999  continue

      write(io,*) 'HUSWO file should contain all fields'
      write(io,*) hushd
      write(*,*) 'HUSWO file should contain all fields'
      write(*,*) hushd

      stop
      end
c----------------------------------------------------------------------
      subroutine getish(ioin,id,iyr,imo,ida,ihr,wd,ws,iceil,icc,
     *  tempk,irh,pres,altim,slp,xprec,ipcode,dtow,ipwout,visb,
     *  elevx,licc,ixtz,leof,maxap,jdat)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 040709              GETISH
c ---            K. Morrison, Earth Tech
c
c --- PURPOSE:  Get a single hour of ISHWO data, passing back variables
c               that could be used for SURF.DAT, PRECIP.DAT, VSRN.DAT, and 
c               SEA.DAT files.  Since there may be multiple records for an 
c               individual hour, the program always carries out at least 
c               2 reads per hour, backspacing as necessary.
c               Note that station ID is the 6-digit USAF/WMO ID.
c
c --- UPDATES:
c --- Ver. 5.5_draft lev. 040709 from Ver. 5.44 lev. 040616 (K. Morrison)
c     - Add processing for TD3505 and TD9956 and obtain station
c       elevation
c     - Obtain and return present weather codes and visibility
c --- Ver. 5.44 lev. 040616 from Ver. 5.41 lev. 040205 (K. Morrison)
c     - Return altimeter and sea-level pressure to RDWRITS
c     - Drop passing of elevation since all pressure calculations now
c       in RDWRITS
c --- Ver. 5.41 lev. 040210 from Ver. 5.4 lev. 0040205 (K. Morrison)
c     - Add array for precip over last 6 hours for allocation of 
c       accumulated precipitation
c     - Correct identification and output of missing hours
c
c
c --- INPUTS:
c          IOIN  - integer - Input unit number for current file
c          LICC  - logical - Switch to use total sky cover if opaque
c                            sky cover is missing
c          IXTZ  - integer - Time zone for LST of current station
c         MAXAP  - integer - Maximum precip accumulation period
c          JDAT  - integer - File type (5=ISHWO, 6=TD3505, 7=TD9956)
c
c --- OUTPUTS:   
c          ID    - integer - Identifier of current station
c          IYR   - integer - Year of current observation
c          IMO   - integer - Month of current observation
c          IDA   - integer - Day of current observation
c          IHR   - integer - Hour of current observation
c          WS    - real    - Wind speed for current observation
c          WD    - real    - Wind direction for current observation
c          ICEIL - integer - Ceiling height for current observation
c          ICC   - integer - Cloud cover for current observation 
c          TEMPK - real    - Air temperature for current observation
c          IRH   - integer - Relative humidity for current observation
c          PRES  - real    - Station pressure for current observation 
c          ALTIM - real    - Altimeter setting for current observation
c          SLP   - real    - Sea-level pressure for current observation
c          XPREC - real    - Liquid precipitation for current observation 
c          IPCODE- integer - Precipitation code for current observation 
c          DTOW  - real    - Air-sea temperature difference for current 
c                            observation
c          IPWOUT- integer - Array of 3 present weather codes for current 
c                            observation
c          VISB  - real    - Visibility for current observation
c          ELEVX - real    - Station elevation (TD3505 & TD9956)
c          LEOF  - logical - Flag for EOF on current file
c
c --- LOCAL VARIABLES: 
c          IYRK - Most recent year
c         JDAYK - Most recent julian day
c          IHRK - Most recent hour
c          PREC - Precipitation over the previous 6 hours
c
c --- GETISH called by:  RDWRITS
c --- GETISH calls:      READISH, DELTT, INCR, GRDAY
c----------------------------------------------------------------------
c
c
      logical licc,leof
      integer iyrk(300),ijdayk(300),ihrk(300)
      integer*2 ipwout(3),ipwout1(3)
      real prec(6,300)/1800*0./
      data iyrk,ijdayk,ihrk/900*-1/
c      
c     first call
c
      k=ioin-6
      call readish(ioin,id,iyr,imo,ida,ihr,jday,wd,ws,iceil,
     *  icc,tempk,irh,pres,altim,slp,prec(6,k),ipdur,ipcode,dtow,
     *  ipwout,visb,elevx,licc,ixtz,leof,maxap,jdat)
      if(leof) goto 9999
c
c     check for missing periods
c
c     check if first read for this station
      if(iyrk(k).lt.0) then
        iyrk(k)=iyr
        ijdayk(k)=jday
        ihrk(k)=ihr      
c     check if gap in hours
      else
        call deltt(iyrk(k),ijdayk(k),ihrk(k),iyr,jday,ihr,jdiff)
        if(jdiff.gt.1) then
c       gap found - return missing values  
          backspace ioin
          call incr(6,iyrk(k),ijdayk(k),ihrk(k),1)
          iyr=iyrk(k)
          jday=ijdayk(k)
          ihr=ihrk(k)
          call grday(6,iyr,jday,imo,ida)
          ws=9999.
          wd=9999.
          iceil=9999
          icc=9999
          tempk=9999.
          irh=9999
          pres=9999.
          xprec=9999.
          ipcode=9999
          dtow=9999.
          prec(6,k)=9999.
          ipwout=999
          visb=99999.
          elevx=9999.
          goto 9999
        else
c         no gap
          iyrk(k)=iyr
          ijdayk(k)=jday
          ihrk(k)=ihr
          if(ipdur.gt.1) then
            ifhr=6-ipdur+1
            do ii=5,ifhr
              if(prec(ii,k).lt.9998.) prec(6,k)=prec(6,k)-prec(ii,k)
            enddo
          endif
          prec(6,k)=max(prec(6,k),0.)
          xprec=prec(6,k)      
        endif
      endif 
c
c     subsequent call(s)
c
100   call readish(ioin,id,iyr1,imo1,ida1,ihr1,jday1,wd1,ws1,iceil1,
     *  icc1,tempk1,irh1,pres1,altim1,slp1,xprec1,ipdur1,ipcode1,
     *  dtow1,ipwout1,visb1,elevx1,licc,ixtz,leof,maxap,jdat)
      iyrk(k)=iyr
      ijdayk(k)=jday
      ihrk(k)=ihr      
      if(leof) goto 9999
c
c     first, check if same hour
c     if not, backspace unit and return
c
      if(iyr1.ne.iyr.or.jday1.ne.jday.or.ihr1.ne.ihr) then
        backspace ioin
        goto 9999
      endif
c
c     check variable by variable, retaining the most recent or most valid
c

c     wind speed
      if(ws1.lt.9998.) ws=ws1
c     wind direction
      if(wd1.lt.9998.) wd=wd1
c     ceiling height
      if(iceil1.lt.9999) iceil=iceil1
c     cloud cover
      if(icc1.lt.9999) icc=icc1
c     temperature
      if(tempk1.lt.9998.) tempk=tempk1 
c     humidity      
      if(irh1.lt.9999) irh=irh1
c     pressure
      if(pres1.lt.9998.) pres=pres1
c     altimeter
      if(altim1.lt.9998.) altim=altim1
c     sea-level pressure      
      if(slp1.lt.9998.) slp=slp1
c     precipitation
      if(xprec1.lt.9998.) then
        if(ipdur1.gt.1) then
          ifhr=6-ipdur1+1
          do ii=5,ifhr
            if(prec(ii,k).lt.9998.) xprec1=xprec1
     *        -prec(ii,k)
          enddo
          xprec1=max(0.,xprec1)
        endif
        if(prec(6,k).lt.9999.) then
          if(xprec1.gt.prec(6,k)) then
            prec(6,k)=xprec1
            ipdur=ipdur1
          endif
        else
          prec(6,k)=xprec1
          ipdur=ipdur1
        endif
        xprec=prec(6,k)
      endif
c     precipitation code
      if(ipcode1.lt.9999) ipcode=ipcode1
c     air-sea Tdiff
      if(dtow1.lt.9998.) dtow=dtow1
c     weather codes
      if(minval(ipwout1).lt.998) ipwout=ipwout1
c     visibility
      if(visb1.lt.99998.) visb=visb1
c     elevation
      if(elevx1.lt.9998.) elevx=elevx1
c
c     repeat the read
c
      goto 100
c
c     before returning, wind down the precipitation array
9999  do ii=1,5
        prec(ii,k)=prec(ii+1,k)
      enddo
      return
      end
c
c----------------------------------------------------------------------
      subroutine readish(ioin,id,iyr,imo,ida,ihr,jday,wd,ws,
     * iceil,icc,tempk,irh,pres,altim,slp,xprec,ipdur,ipcode,
     * dtow,ipwout,visb,elevx,licc,ixtz,leof,maxap,jdat)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 050324             READISH
c ---            K. Morrison, Earth Tech
c
c --- PURPOSE:  Read a single record of full ISHWO, TD3505, or TD9956  
c     data and extract variables that are needed for SURF.DAT, 
c     PRECIP.DAT, VSRN.DAT, and SEA.DAT files.
c     The subroutine adjust the date and hour to LST, and returns as the 
c     station ID the 6-digit USAF/WMO ID.
c
c --- UPDATES:
c
c --- Ver. 5.56 lev. 050324 from Ver. 5.55 lev. 050311 (D. Strimaitis)
c     - Revise time-window in READISH so that only reported times that
c       fall within the hour (HH-1)01 to HH00 are assigned to hour HH.
c       For example, observations at 1701, 1725, 1749, and 1800 are all
c       assigned to "hour 18".
c
c --- Ver. 5.55 lev. 050311 from Ver. 5.52 lev. 041026 (D. Strimaitis)
c     - Updated logic that overstated the number of missing hours.
c
c --- Ver. 5.52 lev. 041026 from Ver. 5.5_draft lev. 040709 (K. Morrison)
c     - Add TD3505 and TD9956 handling, and extract station elevation for
c
c --- Ver. 5.5_draft lev. 040709 from Ver. 5.44 lev. 040621 (K. Morrison)
c     - Add TD3505 and TD9956 handling, and extract station elevation for
c       these types
c     - Translate automated present weather codes to manual codes, return
c       these codes, and modify precipitation code generation
c
c --- Ver. 5.44 lev. 040621 from Ver. 5.43 lev. 040322 (K. Morrison)
c     - Return altimeter setting and sea-level pressure to GETISH
c     - Remove surface pressure estimation, moving it to RDWRITS
c     - Add call from SCANISHWO to allow empirical evaluations of
c       station pressure from altimeter setting or from sea-level
c       pressure and temperature
c
c --- Ver. 5.43 lev. 040322 from Ver. 5.42 lev. 040318 (K. Morrison)
c     - Corrections to pressure calculation from altimeter setting and
c       sea level pressure
c
c --- Ver. 5.42 lev. 040318 from Ver. 5.41 lev. 040210 (K. Morrison)
c     - Force direction to be missing for variable winds
c
c --- Ver. 5.41 lev. 040210 from Ver. 5.4 lev. 040204 (K. Morrison)
c     - Get TSKC from cover summation if direct value missing
c     - Correct present weather handling for code differences between
c       automatic and manual measurement
c     - Calculate station pressure from altimeter setting or sea level
c       pressure when station value is missing (requires station elevation)
c
c
c --- INPUTS:
c          IOIN  - integer - Input unit number for current file
c          LICC  - logical - Switch to use total sky cover if opaque
c                            sky cover is missing
c          IXTZ  - integer - Time zone for LST of current station
c         MAXAP  - integer - Maximum precip accumulation period
c          JDAT  - integer - File type (ISHWO=5, TD3505=6, TD9956=7)
c
c --- OUTPUTS:   
c          ID    - integer - Identifier of current station
c          IYR   - integer - Year of current observation
c          IMO   - integer - Month of current observation
c          IDA   - integer - Day of current observation
c          IHR   - integer - Hour of current observation
c          JDAY  - integer - Julian day
c          WS    - real    - Wind speed for current observation
c          WD    - real    - Wind direction for current observation
c          ICEIL - integer - Ceiling height for current observation
c          ICC   - integer - Cloud cover for current observation 
c          TEMPK - real    - Air temperature for current observation
c          IRH   - integer - Relative humidity for current observation
c          PRES  - real    - Station pressure for current observation 
c          ALTIM - real    - Altimeter setting for current observation
c          SLP   - real    - Sea-level pressure for current observation
c          XPREC - real    - Liquid precipitation for current observation 
c          LPDUR - integer - Duration of precipitation for current observation 
c          IPCODE- integer - Precipitation code for current observation 
c          DTOW  - real    - Air-sea temperature difference for current 
c                            observation
c          IPWOUT- integer - Array of 3 present weather codes for current 
c                            observation
c          VISB  - real    - Visibility for current observation
c          ELEVX - real    - Station elevation for TD3505 and TD9956
c          LEOF  - logical - Flag for EOF on current file
c          
c
c --- READISH called by:  GETISH, SCANISHWO 
c --- READISH calls:      ISHQC, JULDAY
c----------------------------------------------------------------------
c
      character*2866 recin
      character*2 addtype
      character*1 addn,wdtype
      logical licc,leof
      integer ileap,mdays(12)/31,28,31,30,31,30,31,31,30,31,30,31/
      integer*2 ipwout(3)
      integer*2 isa2ism(100)/0,1,2,3,4,4,999,999,999,999,
     1 10,76,13,999,999,999,999,999,18,999,
     2 28,21,20,21,22,24,29,38,36,38,
     3 45,41,43,45,47,49,999,999,999,999,
     4 63,63,65,63,63,73,75,66,67,999,
     5 53,51,53,55,56,57,57,58,59,999,
     6 63,61,63,65,66,67,67,68,69,999,
     7 73,71,73,75,79,79,79,999,999,999,
     8 81,80,81,81,82,85,86,86,999,999,
     9 95,17,95,96,17,97,99,999,999,19/
      leof=.false.
      npw=0
c
c     set all variables to missing
c
      wd=9999.
      ws=9999.
      iceil=9999
      icc=9999
      tempk=9999.
      irh=9999
      pres=9999.
      altim=9999.
      slp=9999.
      ipcode=0
      xprec=0.
      dtow=9999.
      ipdur=1
      visb=99999.
      ipwout=999
      elevx=9999.
c
c     read the record
c
      read(ioin,fmt='(a2866)',end=9999) recin
c
c     read mandatory section
c
      if(jdat.eq.5) then
c
c     ISHWO section
c
      read(recin,1001) nchar,idwmo,iwban,iyr,imo,ida,ihr,imin,
     *  wd,iwdqc,wdtype,ws,iwsqc,iceil,iceilqc,vis,ivisqc,
     *  tempk,itempqc,dp,idpqc,slp,islpqc
1001  format(i4,i6,i5,i4,i2,i2,i2,i2,6x,f3.0,i1,a1,
     *  f4.1,i1,i5,i1,2x,f6.3,i1,2x,f5.1,i1,f5.1,i1,f5.1,i1)
      nchar=nchar+77
      ipstart=82
c
      elseif(jdat.eq.6) then
c
c     3505 section
c
      read(recin,1002) nchar,idwmo,iwban,iyr,imo,ida,ihr,imin,slat,
     *  slong,elevx,wd,iwdqc,wdtype,ws,iwsqc,iceil,iceilqc,vis,
     *  ivisqc,tempk,itempqc,dp,idpqc,slp,islpqc
1002  format(i4,i6,i5,i4,i2,i2,i2,i2,1x,f6.3,f7.3,5x,f5.0,
     *  9x,f3.0,i1,a1,f4.1,i1,i5,i1,2x,f6.3,i1,2x,f5.1,i1,f5.1,i1,
     *  f5.1,i1)
      nchar=nchar+104
      ipstart=109
c
      elseif(jdat.eq.7) then
c
c     9956 section
c
      read(recin,1003) nchar,idwmo,iyr,imo,ida,ihr,imin,slat,
     *  slong,elevx,wd,iwdqc,wdtype,ws,iwsqc,iceil,iceilqc,vis,
     *  ivisqc,tempk,itempqc,dp,idpqc,slp,islpqc
1003  format(i4,i6,i4,i2,i2,i2,i2,f6.3,f7.3,5x,f5.0,
     *  9x,f3.0,i1,a1,f4.1,i1,i5,i1,2x,f6.3,i1,2x,f5.1,i1,f5.1,i1,
     *  f5.1,i1)
      nchar=nchar+98
      ipstart=103
c
      endif
c
c      
c     set station id to WMO
c
      id=idwmo
c
c     adjust hour and day from UTC to local
c
      ihr=ihr-ixtz
c --- Accept all minutes within the hour ENDING at HH00 as hour HH
c --- (e.g., 1701, 1726, and 1800 are assigned to hour 18;
c ---  1801 goes to hour 19)
c     if(imin.gt.15) ihr=ihr+1
      if(imin.gt.0) ihr=ihr+1
      if(ihr.gt.23) then
        ida=ida+1
        ihr=ihr-24
        ileap=0
        if(mod(iyr,4).eq.0.and.imo.eq.2) ileap=1
        if(ida.gt.mdays(imo)+ileap) then
          ida=1
          imo=imo+1
          if(imo.eq.13) then
            imo=1
            iyr=iyr+1
          endif
        endif
      else
        if(ihr.lt.0) then
          ihr=ihr+24
          ida=ida-1
          if(ida.lt.1) then
            imo=imo-1
            if(imo.lt.1) then
              iyr=iyr-1
              imo=12
              ida=31
            else
              ida=mdays(imo)
              if(imo.eq.2.and.mod(iyr,4).eq.0) ida=ida+1
            endif
          endif
        endif
      endif
c
c     calculate julian day
c
      call julday(2,iyr,imo,ida,jday)
c      
c     check winds
c
c --- Revise logic for wind observations
c --------------------
c      if(wd.gt.998..or.ishqc(iwdqc).eq.9) then
c        wd=9999.
c      else
c        if(wdtype.eq.'C') wd=0.
c        if(wdtype.eq.'V') wd=9999.
c      endif
c      if(ws.gt.999.8.or.ishqc(iwsqc).eq.9.or.wd.gt.9998.) then
c        ws=9999.
c      else
c        if(wdtype.eq.'C') ws=0.
c      endif
c      if(ws.lt.0.1) wd=0.
c --------------------

c -------------------------------------------------
c --- Hierarchy for interpreting wind observations
c ---    1.  Use type code C        --> calm
c ---    2.  Use type code V        --> missing
c ---    3.  Use valid WS < 0.1     --> calm
c ---    4.  Invalid WD             --> missing
c ---    5.  Invalid WS             --> missing
c ---    6.  Use type code '9'      --> missing
c -------------------------------------------------

      if(wdtype.EQ.'C' .OR. wdtype.EQ.'c') then
c ---    CALM (Accept wind observation type code C)
         ws=0.
         wd=0.
      elseif(wdtype.EQ.'V' .OR. wdtype.EQ.'v') then
c ---    Accept wind observation type code V
c ---    Set variable winds to missing
         ws=9999.
         wd=9999.
      elseif(ws.LT.0.1 .AND. ishqc(iwsqc).NE.9) then
c ---    Valid calm speed
         ws=0.
         wd=0.
      elseif(wd.gt.998..or.ishqc(iwdqc).eq.9) then
c ---    Invalid wind direction
         ws=9999.
         wd=9999.
      elseif(ws.gt.998..or.ishqc(iwsqc).eq.9) then
c ---    Invalid wind speed
         ws=9999.
         wd=9999.
      elseif(wdtype.EQ.'9') then
c ---    Accept MISSING type code
         ws=9999.
         wd=9999.
      endif
c --------------------

c
c     convert ceiling height from m to 100s of ft
c
      if(iceil.gt.99998.or.ishqc(iceilqc).eq.9) then
        iceil=9999
      else
        if(iceil.eq.22000) then
          iceil=999
        else
          iceil=nint(float(iceil)/30.48)
        endif
      endif
c
c     convert visibility from km to miles
c
      if(vis.gt.999.98.or.ishqc(ivisqc).eq.9) then
        visb=99999.
      else
        visb=vis/1.6093
      endif
c
c     check temperature and calculate humidity if possible
c
      if(tempk.gt.9999.8.or.ishqc(itempqc).eq.9) then
        tempk=9999.
        irh=9999
      else
        tempk=tempk+273.15
        if(dp.gt.9999.8.or.ishqc(idpqc).eq.9) then
          irh=9999
        else
          tempf=(tempk-273.15)*1.8+32.
          tdewf=dp*1.8+32.
          irh=nint(100.*(((173.-0.1*tempf+tdewf)/(173.+0.9*tempf))**8))
          irh=max(min(irh,100),1)
        endif
      endif
c
c     check sea-level pressure
c
      if(slp.gt.9999.8.or.ishqc(islpqc).eq.9) slp=9999.
c
c     read additional variable section
c     first, check that additional variables are present
c
      read(recin(ipstart-3:ipstart-2),fmt='(a2)') addtype
      if(addtype.ne.'AD') return
c
100   if(ipstart.ge.nchar) return
      read(recin(ipstart:ipstart+1),fmt='(a2)') addtype
      read(recin(ipstart+2:ipstart+2),fmt='(a1)') addn
      ipstart=ipstart+3
c
c     check for remarks or general qc remarks and return
c
      if(addtype.eq.'RE'.or.addtype.eq.'EQ'.or.addtype.eq.'QN') 
     *  return
c
c     check for unused variables and skip the associated fields
c
c     precipitation history
      if(addtype.eq.'AC') then
        ipstart=ipstart+3
        goto 100
      endif
c     precipitation bogus
      if(addtype.eq.'AG') then
        ipstart=ipstart+4
        goto 100
      endif
c     snow depth
      if(addtype.eq.'AJ') then
        ipstart=ipstart+14
        goto 100
      endif
c     snow accumulation
      if(addtype.eq.'AL') then
        ipstart=ipstart+7
        goto 100
      endif
c     past weather
      if(addtype.eq.'AY'.or.addtype.eq.'AZ') then
        ipstart=ipstart+5
        goto 100
      endif
c     runway visibility
      if(addtype.eq.'ED') then
        ipstart=ipstart+8
        goto 100
      endif
c     sky cover layer
      if(addtype.eq.'GA') then
        ipstart=ipstart+13
        goto 100
      endif
c     below-station cloud
      if(addtype.eq.'GG') then
        ipstart=ipstart+15
        goto 100
      endif
c     sunshine
      if(addtype.eq.'GJ') then
        ipstart=ipstart+5
        goto 100
      endif
c     hail size
      if(addtype.eq.'HL') then
        ipstart=ipstart+4
        goto 100
      endif
c     ground observation
      if(addtype.eq.'IA') then
        if(addn.eq.'1') then
          ipstart=ipstart+3
        else
          ipstart=ipstart+9
        endif
        goto 100
      endif
c     extreme air temperature
      if(addtype.eq.'KA') then
        ipstart=ipstart+10
        goto 100
      endif
c     pressure change
      if(addtype.eq.'MD') then
        ipstart=ipstart+11
        goto 100
      endif
c     geopotential height isobaric level
      if(addtype.eq.'ME') then
        ipstart=ipstart+6
        goto 100
      endif
c     present weather in vicinity
      if(addtype.eq.'MV') then
        ipstart=ipstart+3
        goto 100
      endif
c     supplemental wind
      if(addtype.eq.'OA') then
        ipstart=ipstart+8
        goto 100
      endif
c     gusts
      if(addtype.eq.'OC') then
        ipstart=ipstart+5
        goto 100
      endif
c     waves
      if(addtype.eq.'UA') then
        ipstart=ipstart+10
        goto 100
      endif
c     swells
      if(addtype.eq.'UG') then
        ipstart=ipstart+9
        goto 100
      endif
c     ice accretion
      if(addtype.eq.'WA') then
        ipstart=ipstart+6
        goto 100
      endif
c     water surface ice
      if(addtype.eq.'WD') then
        ipstart=ipstart+20
        goto 100
      endif
c     ice history
      if(addtype.eq.'WG') then
        ipstart=ipstart+11
        goto 100
      endif
c
c     useful variables
c
c
c     liquid precipitation, retain 1-hr value if available, otherwise
c     return accumulated value
      if(addtype.eq.'AA') then
        read(recin(ipstart:ipstart+7),fmt='(i2,f4.1,2i1)') lpdur,prec,
     *    itrace,ilpqc
        if(lpdur.eq.1) then
          ipdur=lpdur
          if(prec.gt.999.8.or.ishqc(ilpqc).eq.9) then
            xprec=9999.
          else
            xprec=prec
          endif
        else
          if(lpdur.le.maxap.and.xprec.gt.9998.) then
            ipdur=lpdur
            if(prec.gt.999.8.or.ishqc(ilpqc).eq.9) then
              xprec=9999.
            else
              xprec=prec
            endif
          endif
        endif
        if(xprec.lt.0.01) ipcode=0
        if(xprec.gt.0..and.xprec.lt.9998..and.tempk.lt.9998.) then
          if(tempk.ge.273.15) then 
            ipcode=1
          else
            ipcode=20
          endif
        endif
        ipstart=ipstart+8
        goto 100
      endif
c
c     automated present weather for ipcode and check precip,
c     converting automated pw code to manual pw code first
      if(addtype.eq.'AW') then
        read(recin(ipstart:ipstart+2),fmt='(i2,i1)') ipw,ipwqc
        if(ishqc(ipwqc).ne.9.and.npw.lt.3) then
          ipw=isa2ism(ipw+1)
          npw=npw+1
          ipwout(npw)=ipw
          if(((ipw.le.19).or.(ipw.ge.30.and.ipw.le.35).or.
     *      (ipw.ge.40.and.ipw.le.49)).and.
     *      (xprec.lt.0.01.or.xprec.gt.9998.)) then
            ipcode=0
            xprec=0.
          else
c           predominantly liquid            
            if(ipw.eq.20.or.ipw.eq.21.or.(ipw.ge.23.and.ipw.le.25).or.
     *        (ipw.ge.50.and.ipw.le.69).or.(ipw.ge.80.and.ipw.le.84)
     *        .or.ipw.eq.91.or.ipw.eq.92) then
              ipcode=1
c           predominantly solid 
            elseif(ipw.eq.22.or.(ipw.ge.36.and.ipw.le.39).or.
     *        (ipw.ge.70.and.ipw.le.79).or.ipw.eq.85.or.ipw.eq.86.or.
     *        ipw.eq.96.or.ipw.eq.99) then
              if(ipcode.ne.1) ipcode=20
c           indeterminate, use temperature
            else
              if(tempk.lt.9998.) then
                if(tempk.ge.273.15) then
                  ipcode=1
                else
                  if(ipcode.ne.1) ipcode=20
                endif
              endif
            endif
          endif
        endif
        if(ipcode.eq.0.and.ipdur.gt.1) xprec=0.
        ipstart=ipstart+3
        goto 100
      endif
c
c     sky cover summation - overridden if sky condition present
      if(addtype.eq.'GD') then
        read(recin(ipstart:ipstart+3),fmt='(i1,i2,i1)') iskcs1,
     *    iskcs2,iskcsqc
        if(licc.and.icc.eq.9999) then
          if(iskcs2.eq.99) then
            if(iskcs1.eq.9.or.ishqc(iskcsqc).eq.9) then
              icc=9999
            else
              if(iskcs1.eq.0) icc=0
              if(iskcs1.eq.1) icc=2
              if(iskcs1.eq.2) icc=4
              if(iskcs1.eq.3) icc=8
              if(iskcs1.ge.4) icc=10
            endif
          else
            if(iskcs2.eq.0) icc=0
            if(iskcs2.eq.1) icc=1
            if(iskcs2.eq.2) icc=3
            if(iskcs2.eq.3) icc=4
            if(iskcs2.eq.4) icc=5
            if(iskcs2.eq.5) icc=6
            if(iskcs2.eq.6) icc=8
            if(iskcs2.eq.7) icc=9
            if(iskcs2.eq.8) icc=10
          endif
        endif
        ipstart=ipstart+12
        goto 100
      endif
c
c     sky condition for opaque or total cloud cover
      if(addtype.eq.'GF') then
        read(recin(ipstart:ipstart+4),fmt='(2i2,i1)') itskc,ioskc,
     *    iskcqc
        if(ioskc.eq.99) then
          if(licc) then
            if(itskc.eq.99.or.ishqc(iskcqc).eq.9) then
              itskc=99
            endif
            ioskc=itskc
          endif
        endif  
        if(ioskc.eq.0) icc=0
        if(ioskc.eq.1) icc=1
        if(ioskc.eq.2) icc=3
        if(ioskc.eq.3) icc=4
        if(ioskc.eq.4) icc=5
        if(ioskc.eq.5) icc=6
        if(ioskc.eq.6) icc=8
        if(ioskc.eq.7) icc=9
        if(ioskc.eq.8) icc=10
        if(ioskc.eq.99) icc=9999
        ipstart=ipstart+23
        goto 100
      endif
c
c     altimeter setting and station pressure  
      if(addtype.eq.'MA') then
        read(recin(ipstart:ipstart+11),fmt='(f5.1,i1,f5.1,i1)') altim,
     *    ialtqc,pres,ipresqc
        if(altim.gt.9999.8.or.ishqc(ialtqc).eq.9) altim=9999.
        if(pres.gt.9999.8.or.ishqc(ipresqc).eq.9) pres=9999.
        ipstart=ipstart+12
        goto 100
      endif
c
c     manual present weather for ipcode and check precip
      if(addtype.eq.'MW') then
        read(recin(ipstart:ipstart+2),fmt='(i2,i1)') ipw,ipwqc
        if(ishqc(ipwqc).ne.9.and.npw.lt.3) then
          npw=npw+1
          ipwout(npw)=ipw
          if(((ipw.le.19).or.(ipw.ge.30.and.ipw.le.35).or.
     *      (ipw.ge.40.and.ipw.le.49)).and.
     *      (xprec.lt.0.01.or.xprec.gt.9998.)) then
            ipcode=0
            xprec=0.
          else
c           predominantly liquid            
            if(ipw.eq.20.or.ipw.eq.21.or.(ipw.ge.23.and.ipw.le.25).or.
     *        (ipw.ge.50.and.ipw.le.69).or.(ipw.ge.80.and.ipw.le.84)
     *        .or.ipw.eq.91.or.ipw.eq.92) then
              ipcode=1
c           predominantly solid 
            elseif(ipw.eq.22.or.(ipw.ge.36.and.ipw.le.39).or.
     *        (ipw.ge.70.and.ipw.le.79).or.ipw.eq.85.or.ipw.eq.86.or.
     *        ipw.eq.96.or.ipw.eq.99) then
              if(ipcode.ne.1) ipcode=20
c           indeterminate, use temperature
            else
              if(tempk.lt.9998.) then
                if(tempk.ge.273.15) then
                  ipcode=1
                else
                  if(ipcode.ne.1) ipcode=20
                endif
              endif
            endif
          endif
        endif
        if(ipcode.eq.0.and.ipdur.gt.1) xprec=0.
        ipstart=ipstart+3
        goto 100
      endif
c
c     sea surface temperature
      if(addtype.eq.'SA') then
        read(recin(ipstart:ipstart+4),fmt='(f4.1,i1)') tw,itwqc
        if(tempk.gt.9998..or.tw.gt.999.8.or.ishqc(itwqc).eq.9) then
          dtow=9999.
        else
          dtow=tempk-273.15-tw
        endif
        ipstart=ipstart+5
        goto 100
      endif
c
c     otherwise, there has been an error
        open(unit=600,file='isherror.txt')
        write(*,*) ' Error decoding ISH code'
        write(*,*) ' Check isherror.txt for message'
        write(600,*) ' Error in decoding at column ',ipstart
        write(600,*) ' Bad code : ',addtype,' in record :'
        write(600,*) recin(1:nchar)
        stop
c
c     end of data extraction
c
9999  leof=.true.
      return
      end
c----------------------------------------------------------------------
      integer function ishqc(iqc)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 040204               ISHQC
c ---            K. Morrison, Earth Tech
c
c --- PURPOSE:  Decode a QC flag for ISHWO data
c
c --- UPDATE:
c --- Ver. 5.41 lev. 040210 from Ver. 5.4 lev. 040204
c     - Accept suspect ISH values (codes 2 and 6)
c
c --- INPUTS:
c          IQC   - integer - QC flag
c
c --- OUTPUTS:   
c          ISHQC - integer - Decoded QC
c                            1 - Accept current value
c                            9 - Reject current value
c          
c
c --- ISHQC   called by:  READISH 
c --- ISHQC   calls:      none 
c----------------------------------------------------------------------
c          
      if(iqc.eq.3.or.iqc.eq.7.or.iqc.eq.9) then
        ishqc=9
      else
        ishqc=1
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine rdssta
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 020308              RDSSTA
c ---            D. Strimaitis, Earth Tech, Inc.

c
c --- Read the station information for the input suface data files
c
c --- INPUTS:
c       Common block /CONTROL/
c          nff, 
c       Parameters: IO6, IOSSTA, MXFF
c --- OUTPUT:
c
c --- RDSSTA called by:  SETUP
c --- RDSSTA calls:      DEBLNK, ALLCAP
c----------------------------------------------------------------------
c
c --- Include parameters and commons
      include 'params.smg'
      include 'params.cal'
      include 'control.smg'
      include 'station.smg'

      integer num(mxff)

      character*70 line,blank70,break
      character*1 cstor1(mxcol),cstor2(mxcol)

      character*4 cname4
      character*16 clatNS, clonEW

      data num /mxff*0/

      data blank70(1:35)  /'                                   '/
      data blank70(36:70) /'                                   '/

c --- Read and check first line of file
      read(iossta,'(a70)') Line
      if(line(1:7).NE.'SURFACE') then
         write(io6,*)
         write(io6,*)
         write(io6,*) 'RDSSTA:  Invalid file content'
         write(io6,*) 'First line should begin with SURFACE = '
         write(io6,*) 'First line found: '
         write(io6,*) line
         write(io6,*)
         write(io6,*)
         stop
      endif

c --- Initilize output arrays
      do i=1,mxff
         fanem(i)=10.0
         cfname(i)='----'
         cflat(i)='----------------'
         cflon(i)='----------------'        
      enddo

c --- Set the section break text string
      read(iossta,'(a70)') break

c --- Process data from each section
10    line=blank70
      read(iossta,'(a70)',end=999) Line

      if(line.NE.break) then
c ---    Pass to character*1 array for processing
         do i=1,70
            cstor1(i)=line(i:i)
            cstor2(i)=' '
         enddo
c ---    Remove blank characters from string
         call DEBLNK(cstor1,1,70,cstor2,nlim)
c ---    Find the location of '=' and '!' (if present)
         neqs=0
         nend=nlim
         do i=nlim,1,-1
            if(cstor2(i).EQ.'=') neqs=i
            if(cstor2(i).EQ.'!') nend=i-1
         enddo
         nlim=nend
         if(neqs.EQ.0) then
            write(io6,*)
            write(io6,*)
            write(io6,*) 'RDSSTA:  Invalid file content'
            write(io6,*) 'Assignment line must include an = '
            write(io6,*) 'Assignment line found: '
            write(io6,*) line
            write(io6,*)
            write(io6,*)
            stop
         endif
c ---    Convert lower case letters to upper case
         call ALLCAP(cstor2,nlim)
c ---    Pass back to line
         line=blank70
         do i=1,nlim
            line(i:i)=cstor2(i)
         enddo
c ---    Assign to variable
         i1=neqs+1
         if(line(1:12).EQ.'STATION_NAME') then
            i2=i1+3
            cname4=line(i1:i2)
         elseif(line(1:10).EQ.'STATION_ID') then
            read(line(i1:nlim),'(i10)') istation
         elseif(line(1:13).EQ.'ANEMOMETER_HT') then
            if(i1.LT.nlim) read(line(i1:nlim),'(f10.2)') anemht
         elseif(line(1:8).EQ.'LATITUDE') then
            i2=MIN(16,nlim)
            clatNS=line(i1:nlim)
         elseif(line(1:9).EQ.'LONGITUDE') then
            i2=MIN(16,nlim)
            clonEW=line(i1:nlim)
         endif

      else      
c ---    End of section reached, so pass into storage arrays
c ---    Match to station ID
         do i=1,nff
            if(istation.EQ.ifstn(i)) then
               num(i)=num(i)+1
               if(anemht.GT.0.) fanem(i)=anemht
               cfname(i)=cname4
               cflat(i)=clatNS
               cflon(i)=clonEW
            endif
         enddo
c ---    Reset variables
         anemht=0.
         cname4='    '
         clatNS='                '
         clonEW='                '
      endif

c --- Get next line
      goto 10

999   continue
c --- Report results to list file
      write(io6,*)
      write(io6,*)'RDSSTA:  Information extracted from station file'
      write(io6,*)
      write(io6,*)'   ID     Name  Anem(m)     Lat               Lon'
      do i=1,nff
         write(io6,101)ifstn(i),cfname(i),fanem(i),cflat(i),
     &                 cflon(i),num(i)
      enddo
101   format(i8,2x,a4,f8.2,6x,2(a16,2x),i4)
      write(io6,*)
      write(io6,*)

      return
      end
c----------------------------------------------------------------------
      subroutine fin(itest)
c----------------------------------------------------------------------
c
c --- SMERGE     Version: 5.57        Level: 030402                 FIN
c ---            J. Scire, Earth Tech, Inc.

c
c --- PURPOSE:  Run termination routine -- compute runtime,
c               write last day processed
c
c --- UPDATE
c --- V5.2-V5.3    030402  (DGS): Add list file unit number to JULDAY
c                                 (CALUTILS version), and fix unit
c                                 number given to GRDAY
c --- V5.1-V5.2    020828  (DGS): rdate, rdate2 changed to include
c                                 YYYY format for year (MM-DD-YYYY)
c
c --- INPUTS:
c          ITEST - integer - Flag indicating if execution is to
c                            include COMPUTATIONAL phase
c                            (ITEST = 1 to STOP program and skip
c                                       the COMPUTATIONAL phase
c                             ITEST = 2 to CONTINUE execution to
c                                       include computations)
c       Common block /DATEHR/
c          jyr, jjul, jhr
c       Common block /QA/
c          rdate, rtime, rcpu
c       Parameters: IO6, IOMESG
c
c --- OUTPUT:  none
c
c --- FIN called by:  MAIN
c --- FIN calls:      DATETM, JULDAY, DELTT, YR4C
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.smg'
c
      character*8 rtime2
      character*10 rdate2
c
      include 'datehr.smg'
      include 'qa.smg'
c
      write(iomesg,*)'TERMINATION PHASE'
c
c --- Write last day/hour processed
      if(ITEST.eq.2)then
c ---    Compute month & day from Julian day
         call GRDAY(io6,jyr,jjul,jmo,jday)
         write(io6,5)jyr,jmo,jday,jjul,jhr
5        format(//2x,'LAST DAY/HOUR PROCESSED:'/5x,'Year: ',i4,2x,
     1   'Month: ',i2,3x,'Day: ',i2,3x,'Julian day: ',i3,3x,'Hour: ',
     2   i2)
      else
c
c ---    TEST mode -- COMPUTATIONAL phase skipped
         write(io6,12)
12       format(/1x,13('----------')//1x,
     1   'Completion of test mode run -- run terminating ',
     2   'normally'//1x,13('----------'))
      endif
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
         call julday(io6,iyr1,imo1,iday1,ijul1)

         read(rdate2(1:2),10)imo2
         read(rdate2(4:5),10)iday2
         read(rdate2(7:10),'(i4)')iyr2
         call julday(io6,iyr2,imo2,iday2,ijul2)

c ---    compute no. hours from beg. of first hour of run to
c ---    ending hour of ending day of the run
         call deltt(iyr1,ijul1,ihr1,iyr2,ijul2,ihr2,idelhr)

c ---    adjust for minutes and seconds
         delt=idelhr*3600.-imin1*60.-isec1+imin2*60.+isec2
      endif

c --- On the PC, the runtime and CPU time are the same
c --- (DATETM provides RCPU = 0.0 on the PC)
      if(rcpu.EQ.0.0)rcpu=delt

c --- Report current date
      write(io6,1402)rtime2,rdate2,delt,rcpu
1402  format(//2x,'End of run -- Clock time: ',a8/
     1         2x,'                    Date: ',a10//
     2         2x,'      Elapsed clock time: ',f10.1,' (seconds)'//
     3         2x,'                CPU time: ',f10.1,' (seconds)')
c
      return
      end
