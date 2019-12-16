c----------------------------------------------------------------------
c --- READ62 -- TD-6201 Upper Air Data Preprocessor
c----------------------------------------------------------------------
c
c --- READ62   Version: 5.54      Level: 070627                    MAIN
c
c     Copyright (c) 1995-2007 by Exponent, Inc.
c
c --- PURPOSE:
c      THIS PROGRAM READS A TD-6201 UPPER AIR FILE or an NCDC CD-ROM
c      upper air data file (FSL format), EXTRACTS DATA FOR PRESSURE 
c      LEVELS REQUESTED, AND CREATES A FORMATTED FILE FOR EDITING AND 
c      INPUT TO THE CALMET MODEL
c
c----------------------------------------------------------------------
c --- Model Change Bulletin Updates Included:           MCB-A (040716)
c----------------------------------------------------------------------
c
c --- UPDATES
c
c --- V 5.53 Level 040109  ====> V 5.54 Level 070627 (D. Strimaitis)
c     - Updated CALUTILS (Version 2.55, Level 070327)
c         Modify search for '=' in READIN to allow
c           for blanks between c*12 variable name and the '='
c           sign (internal blanks are not removed after V2.2)
c         Replace filename strings c*70 with c*132
c         Allow for spaces within pathnames by adding new TLEFT
c           and TRIGHT trim subroutines
c     - Filnames changed from c*70 to c*132 (CALUTILS V2.3 and later)
c       Modified:  FILNAM.R62
c                  READCF, SETUP
c
c --- V 5.52 Level 030709  ====> V 5.53 Level 040109 (D. Strimaitis)
c     1)  Remove lines using old limit of 79 levels from the TD-6201
c         section.  These lines caused 1 or 2 dummy reads leading to
c         "skipped soundings" in the output file. 
c         This does not affect FSL data.
c
c --- V 5.51 Level 030528  ====> V 5.52 Level 030709 (D. Strimaitis)
c     1)  Fix type assignment for LCFILES in READCF
c
c --- V 5.5 Level 030402  ====> V 5.51 Level 030528 (D. Strimaitis)
c     1)  Updated CALUTILS (Version 2.2, Level 030528)
c
c --- V 5.4 Level 021024  ====> V 5.5 Level 030402 (D. Strimaitis)
c     1)  Updated CALUTILS (Version 2.1, Level 030402)
c     2)  New header for output data file (UP.DAT)
c             ENVPOP=0  Revised UP.DAT header
c             ENVPOP=1  Revised UP.DAT header with station locations
c     3)  Use WBAN ID in FSL file if available, otherwise use WMO
c     4)  Fix N,S,E,W logic for WEB FSL format
c
c --- V 5.3 Level 020828  ====> V 5.4 Level 021024 (D. Strimaitis)
c     1)  Fixed bug in extrapolation repair code (bug had allowed
c         extrapolation from levels below user's minimum level)
c
c --- V 5.2 Level 020805  ====> V 5.3 Level 020828 (D. Strimaitis)
c     1)  Updated CALUTILS (Version 1.1, Level 020828)
c
c --- V 5.1 Level 020614  ====> V 5.2 Level 020805 (D. Strimaitis)
c     1)  Change FSL header format for new WEB structure
c     2)  Add parameter ENVPOP to params.r62 and use this to toggle
c         environment configurations (these will evolve in time):
c             ENVPOP=0  UP.DAT header as in 1/2000 Users Guide
c             ENVPOP=1  UP.DAT header with draft station locations
c                       introduced in V 5.1 Level 020330
c
c --- V 5.1 Level 020330  ====> V 5.1 Level 020614 (D. Strimaitis)
c     1)  Allow station ID to be up to 8 characters
c
c --- V 5.0 Level 011003  ====> V 5.1 Level 020330 (D. Strimaitis)
c     1)  Accept variable record length version of TD6201 format
c     2)  New header format for UP.DAT:  include READ62 version, level,
c         station name, ID, and LAT/LON
c         (Station information is read from file headers)
c     3)  Add repair option to extrapolate missing data to either the
c         surface and/or the top pressure level
c     4)  Add option to substitute alternate sounding profiles when
c         observed sounding is deficient (from UP.DAT format file)
c
c --- V 4.0 Level 010315  ====> V 5.0 Level 011003 (D. Strimaitis)
c     1)  Restructure inputs for CALPUFF system control file
c     2)  Restructure main program as subroutine COMP
c     3)  Place system-wide utilities into an include module
c           (calutil.for)
c
c --- V 4.0 Level 990228  ====> V 4.0 Level 010315 (D. Strimaitis)
c     1)  Read full year from input records and enforce YYYY format
c         for all years (Y2k)
c
c --- V 4.0 Level 980304  ====> V 4.0 Level 990228 (J. Scire)
c     1) Modified format statements 6060/6200/6065/6205 to allow more 
c        than 99 levels to be written in data header record
c     2) Allow wind speed to be written in xxx.x format in m/s when
c        using comma-delimited output file.  Add comma after wind
c        speed field in data record (except after last value).
c        Eliminate truncation of wind speed data when using FSL data 
c        input.
c     3) Perform check on no. levels to ensure array dimensions are
c        not exceeded (FSL formatted data).
c     4) Fix bug preventing elimination of levels when LTEMP=T (caused by
c        change of missing value indicator from 99.9 to 999.9).
c     5) Fix bug allowing conversion of temperature to deg. K when
c        using td-6201 data.
c     6) Update version number in function GOOD.
c     7) Add check for header entry with no data records (FSL)
c     8) Perform QA checks & produce warnings relevent for CALMET
c        - flag data missing at bottom or top of sounding
c        - check that first layer is at ground
c        - flag if pressure increases with decreasing level number
c        - flag if elevation decreases with increasing level number
c        - flag WD < 0 or WD > 360 degrees
c        - flag WS < 0
c        - flag T < TMIN, T > TMAX (TMIN set at 175 K (-99 C, -146 F),
c                                   TMAX set at 322 K (120 F))
c        - flag P < PMIN, P > PMAX in final sounding
c        - check for missing height (FSL format uses 32767 as missing
c                                    value indicator)
c        - NOTE: delta pressure & elevation checks exclude layers with
c                missing data
c     9) Use parameter to dimension max. number levels & unit numbers
c    10) Write correct number of levels in original sounding (MLEV)
c    11) Fix bug where missing height was assigned to -990 instead of 9999.
c    12) Use UP.DAT convention of -99.9 for missing pressures on output
c
c --- V 4.0 Level 970730  ====> V 4.0 Level 980304 (R. O'Neal, E. Insley)
c     1) Modified WRITE statement 6010 to reflect the current version
c        (was still writing out version 970131).
c     2) Corrected check for missing days which then get sent to
c        subroutine DELTT.  First julian day argument (JDAY2) passed in
c        DELTT changed to JDAY1.  This corrects problem of READ62
c        failing to pick up missing sounding time under certain
c        circumstances.
c     3) Added a check for soundings which get written with missing or invalid
c        heights.  They are still written with missing heights, but a
c        message is written to the list file to alert the user.
c
c --- V 4.0 Level 970131  ====> V 4.0 Level 970730 (E. Insley)
c     1) Modified to allow the data to span from one year to the next
c        without getting a "MISSING DAYS" message (unless, of course
c        there really is a missing day.
c     2) Corrected check for missing/duplicate soundings.  It now uses
c        Subr. DELTT to compute the delta time between soundings and only
c        writes messages when appropriate.
c
c --- V 4.0 Level 961113  ====> V 4.0 Level 970131 (E. Insley, V. Tino)
c     1) Modified to use up to 24 user-specified observation hours,
c        thereby allowing any upper air soundings in raw data file be
c        retrieved by READ62 instead of just 00 and 12 GMT.
c        (EMI - 12/13/96)
c
c     2) Added check if all levels bad (only 0 or 1 good level), skip
c        sounding.
c
c     3) Modified check for missing hours to reflect acceptable
c        user-input observation times.  Uses an algorithm based on
c        12 hour difference.  Difference between 2 consecutive obs.
c        must be less than or equal to 12 hours.
c
c --- V 4.0 Level 961025  ====> V 4.0 Level 961113 (E. Insley, J. Scire)
c     1) Changed STATUS to 'UNKNOWN' in opens for output files
c        (READ62.LST, UP.DAT)
c     2) Modified UP.DAT to include a flag indicating the file type
c        on the first header record (i.e., IFMT=1 for slash-delimited
c        file, IFMT=2 for comma-delimited file)
c     3) Added option to read filenames from the control file
c        (READ62.INP) -- Filenames read: READ62.LST, TD6201.DAT or
c        NCDC_U.DAT, and UP.DAT.
c     4) Added QA checks on values of control file input parameters
c     5) Updated comments in MAIN program
c     6) Eliminated some old, inactive code
c
c --- V 3.0 Level 941215  ======> V 4.0 Level 961025   (E. Insley)
c     1.) Added option to output comma delimited UP.DAT file which can
c         be read using a free format, or / delimited soundings as
c         originally output
c     2.) Fixed location of where the 'MISSING DAYS' and 'MISSING/DUPLICATE
c         SOUNDING' messages are printed. Now they are printed where the
c         missing sounding should be located instead of after the next
c         valid sounding.
c     3.) Changed format at end of program to read 'LAST YR,DAY PROCESSED'
c         and separated the YR and DAY by a comma (e.g., 83,365)
c
c   Modified - VRT - 8/15/95 -
c     Error in processing CD-ROM data.  The
c     1000mb level was included in the sounding even if the surface
c     level was above 1000mb (i.e. 990mb).  In this case, we want any
c     sounding level that is out of order (p increases with Z) to be
c     deleted from the sounding.
c
c     Error in processing missing temperature data.  Conversion of temp
c     from Celsius to Kelvin occurs within the write statement.  This
c     produces an error when the temp in C is missing (99.9). The temp
c     output to the UP.DAT file should stay at 99.9 but all nonmissing
c     temps should be converted to Kelvin.
c
c     Error in reading CD-ROM data.  Calmet needs the 5 digit WBAN number
c     to be compared to the number given in the CALMET.INP file (Group 8)
c     Changed to read and write the WBAN # instead of the 3-letter WMO code.
C                                                                       R6200380
C DETAILS OF TD-6201 CONTENT:                                           R6200390
C                                                                       R6200400
C       HEADER INFORMATION FOR EACH SOUNDING TIME:                      R6200410
C                                                                       R6200420
C      STNID          STATION IDENTIFICATION                            R6200430
C      LAT            LATITUDE -- THE STATION LATITUDE IN DEG AND MIN,  R6200440
C                       FOLLOWED BY 'N' OR 'S'                          R6200450
C      LON            LONGITUDE-- THE STATION LONGITUDE IN DEG AND MIN, R6200460
C                       FOLLOWED BY 'E' OR 'W'                          R6200470
C      YEAR, MONTH, DAY, HOUR  -- THE SCHEDULED TIME OF THE OBSERVATION R6200480
C      NUMLEV         NUMBER OF REPEATING GROUPS -- THIS REPRESENTS     R6200490
C                       THE NUMBER OF DATA LEVELS FOUND IN THE CURRENT  R6200500
C                       OBSERVATION (79 IS THE MAXIMUM NUMBER STORED)   R6200510
C                                                                       R6200520
C       DATA FOR EACH NUMLEV PRESSURE LEVEL:                            R6200530
C                                                                       R6200540
C      QIND           LEVEL-QUALITY-INDICATOR -- DENOTES THE RESULTS OF R6200550
C                       ANY QUALITY CONTROLS APPLIED TO THIS LEVEL (THISR6200560
C                       IS USED IN THIS PROGRAM)                        R6200570
C      ETIME          THE ELAPSED TIME SINCE THE RELEASE OF THE SOUNDINGR6200580
C                       IN MINUTES AND TENTHS (IGNORED HERE)            R6200590
C      PRES           ATMOSPHERIC PRESSURE AT THE CURRENT LEVEL (READ INR6200600
C                       AS MILLIBARS)                                   R6200610
C      HGT            GEOPOTENTIAL HEIGHT OF THE CURRENT LEVEL IN METERSR6200620
C      TEMP           THE FREE AIR TEMPERATURE AT THE CURRENT LEVEL IN  R6200630
C                       DEGREES AND TENTHS CELSIUS.                     R6200640
C      RH             THE RELATIVE HUMIDITY AT THE CURRENT LEVEL IN %   R6200650
C      WD             DIRECTION OF THE WIND AT THE CURRENT LEVEL IN DEG R6200660
C      WS             SPEED OF THE WIND IN WHOLE METERS PER SECOND.     R6200670
C      TIMEF,PRESF,HGTF,TEMPF,RHF,WINDF  --  QUALITY CONTROL FLAGS      R6200680
C                       (USED HERE)                                     R6200690
C      TYPLEV         TYPE OF LEVEL FLAG (IGNORED HERE)                 R6200700
C                                                                       R6200710
C                                                                       R6200720
C      EXTERNAL FUNCTION: GOOD (INTEGER)                                R6200730
C                                                                       R6200740
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.r62'
c --- Include common blocks
      include 'qa.r62'
c
c --- Set version and level number of program (stored in /QA/ and
c --- checked against values set in PARAMS.R62)
      ver='5.54'
      level='070627'
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
c --- READ62    Version: 5.54           Level: 011003        BLOCK DATA
c               D. Strimaitis, Earth Tech, Inc.
c
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.r62'
c
c --- Include common blocks
      include 'control.r62'
      include 'filnam.r62'
      include 'qa.r62'
      include 'station.r62'

c --- FILNAM common block
      data runinp/'read62.inp'/,runlst/'read62.lst'/,
     1     indat/'sounding.dat'/,updat/'up.dat'/,subdat/'subsound.dat'/
c --- FILLOG common block
      data lcfiles/.true./

c --- CONTROL common block
      data lht/.false./, ltemp/.false./, lwd/.false./, lws/.false./
      data ifmt/2/
      data pstop/700.0/
      data isub/0/
      data lxtop/.false./, lxsfc/.false./
      data pvtop/700./, zvsfc/200./

c --- QA common block
      data model/'READ62      '/

c --- STATION common block
      data istz/0/
      data cname/'----'/

      end

c----------------------------------------------------------------------
c --- BRING IN CALPUFF SYSTEM UTILITY SUBROUTINES
      include 'calutils.for'
c----------------------------------------------------------------------

c-----------------------------------------------------------------------
      subroutine setup
c-----------------------------------------------------------------------
c
c --- READ62    Version: 5.54           Level: 070627             SETUP
c               D. Strimaitis
c
c PURPOSE:     SETUP calls routines to read and check the control data
c              provided, it reports the control data to the list file,
c              and it opens the data files if inputs are valid.
c
c --- UPDATES:
c
c --- V 5.1 Level 020330 ====> V5.54 (070627)  (D.Strimaitis)
c     - Change filenames from c*70 to c*132
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c ---    Common block /FILNAM/ variables:
c           runlst,indat,subdat,updat,
c           lcfiles
c ---    Common block /CONTROL/ variables:
c           IBYR,IBMO,IBDY,IBHR,IEYR,IEMO,IEDY,IEHR,ibjul,iejul,
c           lht,ltemp,lwd,lws,lxtop,lxsfc,
c           jdat,ifmt,isub,
c           pstop,pvtop,zvtop
c
c        Parameters: IO5, IO6, IO8, IO9, IO18 IOMESG, MVER, MLEVEL
c
c --- OUTPUT:
c
c ---    Common block /QA/ variables:
c           rcpu,rtime,rdate
c
c ---    Common block /FILNAM/ variables:
c           runinp
c
c CALLING ROUTINES:   MAIN
c
c EXTERNAL ROUTINES:  DATETM, COMLINE, READCF, QAINP, RDSTA
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.r62'
      include 'control.r62'
      include 'filnam.r62'
      include 'qa.r62'

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
10       format(/1x,'ERROR in SUBR. SETUP -- The READ62 version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open list file)
      call READCF

c --- Write control data to list-file
      WRITE(io6,6020)IBYR,IEYR,ibjul,iejul,IBHR,IEHR
6020  FORMAT(/1x,'STARTING DATE:',16X,'ENDING DATE:'//1x,15X,'YEAR = ',
     1 I4,18X,'YEAR = ',I4/10X,'JULIAN DAY = ',I4,12X,'JULIAN DAY = ',
     2 I4/16X,'HOUR = ',I4,' (GMT)',12X,'HOUR = ',I4,' (GMT)')
      WRITE(io6,6030)PSTOP
6030  FORMAT(//1x,'PRESSURE LEVELS EXTRACTED:'//1x,20X,'SURFACE',
     1 ' TO  ',F5.0,' MB')
      write(io6,6031)jdat,ifmt
6031  format(//1x,'INPUT FILE FORMAT (1=TD6201,2=NCDC CD-ROM): ',i3,
     1 /1x,'OUTPUT FILE FORMAT (1=/ DELIMITED,2=COMMA DELIMITED): ',i3)

      if(isub.EQ.0) then
         write(io6,6032)
      else
         write(io6,6033) isub
      endif
6032  format(//1x,'ALT. SOUNDING FILE FOR SUBSTITUTIONS IS NOT USED ')
6033  format(//1x,'ALT. SOUNDING FILE (1=/ DELIMITED,2=COMMA',
     &            ' DELIMITED): ',i3)

      WRITE(io6,6040)LHT,LTEMP,LWD,LWS
6040  FORMAT(//1X,'DATA LEVEL ELIMINATED IF HEIGHT MISSING ? ',8X,L1/
     1 /1X,'DATA LEVEL ELIMINATED IF TEMPERATURE MISSING ? ',3X,L1/
     2 /1X,'DATA LEVEL ELIMINATED IF WIND DIRECTION MISSING ? ',L1/
     3 /1X,'DATA LEVEL ELIMINATED IF WIND SPEED MISSING ? ',4X,L1)

      WRITE(io6,6041)LXTOP,PVTOP,LXSFC,ZVSFC
6041  FORMAT(//1X,'MISSING PROFILE DATA EXTRAPOLATED TO TOP ? ',8X,L1/
     1 /1X,'Last valid data must be above pressure (mb): ',3X,f7.1/
     2 /1X,'MISSING PROFILE DATA EXTRAPOLATED TO SURFACE ? ',4X,L1/
     3 /1X,'First valid data must be below height (m AGL): ',3X,f7.1)

      write(io6,6042) runinp,indat,updat,runlst
6042  format(//1x,'FILENAMES: '/
     1 5x,'Control file:          ',a132/
     2 5x,'Input upper air file:  ',a132/
     3 5x,'Output upper air file: ',a132/
     4 5x,'Output list file:      ',a132)

c --- Substitution sounding file (conditional)
      if(isub.GT.0) then
         write(io6,6043) subdat
6043     format(5x,'Substitution UP.DAT:   ',a132)
     1    
      endif

c --- Apply QA checks
      call QAINP

c --- Open the upper air input data file (TD6201.DAT or NCDC-UP.DAT)
      if(jdat.EQ.1) then
c         open(io8,FILE=indat,status='old',form='formatted',
c     1        recl=2876)
         open(io8,FILE=indat,status='old',form='formatted',
     1        recl=7232)
      else
         open(io8,FILE=indat,status='old')
      endif

c --- Open a second upper air input data file for soundings used as
c --- substitutions (UP.DAT format) and read the header
c --- of the substitution file
      if(isub.GT.0) then
         open(io18,FILE=subdat,status='old')
         call RDHDSUB(io18)
      endif

c --- Get station information from input data file
      call RDSTA

c --- Open the output UP.DAT file
      open(io9,file=updat,status='unknown')

      return
      end
c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- READ62    Version: 5.54           Level: 070627            READCF
c               D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  Read the control file containing the file names of
c               the input and output files of the run, and associated
c               control variables.  Open list file for output.
c
c --- UPDATES:
c --- V 5.52 Level 030709 ====> V 5.54 Level 070627  (D.Strimaitis)
c     - Filenames from c*70 to c*132 for CALUTILS V2.3 and later
c --- V 5.5 Level 020330  ====> V 5.52 Level 030709 (D. Strimaitis)
c           - Fix type assignment for LCFILES
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c
c ---    Common block /FILNAM/ variables:
c           runinp
c
c        Parameters: IO5, IO6, IOMESG, MXLEV
c
c --- OUTPUT:
c
c ---    Common block /FILNAM/ variables:
c           runlst,indat,subdat,updat,
c           lcfiles
c ---    Common block /CONTROL/ variables:
c           IBYR,IBMO,IBDY,IBHR,IEYR,IEMO,IEDY,IEHR,ibjul,iejul,
c           lht,ltemp,lwd,lws,lxtop,lxsfc,
c           jdat,ifmt,isub,
c           pstop,pvtop,zvtop
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE, JULDAY
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.r62'
      include 'params.cal'
c
c --- Include common blocks
      include 'control.r62'
      include 'filnam.r62'
      include 'qa.r62'
c
c --- Local variables
      character*4 ctemp(132,4)
      character*12 cvdic(mxvar,2)
      integer ivleng(mxvar,2),ivtype(mxvar,2)
      logical lecho

c --- Initialize local variables
      data lecho/.false./
      data names/4/

c --- Set Dictionary

      data cvdic/
     a  'RUNLST','INDAT','SUBDAT','UPDAT','LCFILES', 55*' ',
     b  'IBYR','IBMO','IBDY','IBHR','IEYR','IEMO','IEDY','IEHR',
     b  'JDAT','IFMT','PSTOP','LHT','LTEMP','LWD','LWS',
     b  'ISUB','PVTOP','ZVSFC','LXTOP','LXSFC', 40* ' '/

      data ivleng/
     a  4*132,1, 55*0,
     b  20*1, 40*0/

c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
      data ivtype/
     a  4*4,3, 55*0,
     b  10*2,1,4*3,2,2*1,2*3, 40*0/

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
       call READIN(cvdic(1,1),ivleng(1,1),ivtype(1,1),io5,iomesg,
     & lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),lcfiles,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum)

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')runlst=' '
      if(ctemp(1,2)(1:1).ne.' ')indat=' '
      if(ctemp(1,3)(1:1).ne.' ')subdat=' '
      if(ctemp(1,4)(1:1).ne.' ')updat=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')runlst(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')indat(j:j)=ctemp(j,2)(1:1)
         if(ctemp(j,3)(1:1).ne.' ')subdat(j:j)=ctemp(j,3)(1:1)
         if(ctemp(j,4)(1:1).ne.' ')updat(j:j)=ctemp(j,4)(1:1)
      enddo

c --- Convert the file names to the proper case
      call FILCASE(lcfiles,runlst)
      call FILCASE(lcfiles,indat)
      call FILCASE(lcfiles,subdat)
      call FILCASE(lcfiles,updat)

c --- Open listfile
      open(io6,file=runlst,status='unknown')

c --- Write banner to list file
      write(io6,5) ver,level
5     format(///,26x,'READ62 OUTPUT SUMMARY',/,19x,'VERSION:  ',A12,
     1       ' LEVEL:  ',A12///)

c -----------------
c --- Input Group 1
c -----------------

      call readin(cvdic(1,2),ivleng(1,2),ivtype(1,2),io5,io6,lecho,
     1 IBYR,IBMO,IBDY,IBHR,IEYR,IEMO,IEDY,IEHR,
     2 JDAT,IFMT,PSTOP,LHT,LTEMP,LWD,LWS,
     3 ISUB,PVTOP,ZVSFC,LXTOP,LXSFC,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum)

c --- Compute Julian day for each date
      call JULDAY(io6,ibyr,ibmo,ibdy,ibjul)
      call JULDAY(io6,ieyr,iemo,iedy,iejul)

      return
      end
c----------------------------------------------------------------------
      subroutine qainp
c----------------------------------------------------------------------
c
c --- READ62    Version: 5.54           Level: 020330             QAINP
c               D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  QA the control file information
c
c --- INPUTS:
c
c ---    Common block /CONTROL/ variables:
c           IBYR,IBMO,IBDY,IBHR,IEYR,IEMO,IEDY,IEHR,ibjul,iejul,
c           jdat,ifmt,pstop
c
c        Parameters: IO6
c
c --- OUTPUT:   none
c
c --- QAINP called by:  SETUP
c --- QAINP calls:      QAYR4, YR4
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.r62'
c
c --- Include common blocks
      include 'control.r62'
c
c --- Local variables
      logical lerrcf

c --- Initialize local variables
      data lerrcf/.false./

c --- Special Y2K QA on starting year of simulation
      call QAYR4(io6,ibyr,0,ierr)
      if(ierr.NE.0) then
         write(io6,*) 'QAINP:  Error in Input Group 1'
         lerrcf=.TRUE.
      endif

c --- Make sure ending year is YYYY (Y2K)
      call YR4(io6,ieyr,ierr)
      if(ierr.NE.0) then
         write(io6,*) 'QAINP:  Error in Input Group 1'
         lerrcf=.TRUE.
      endif

c --- Check that ending date is after starting date
      iedathr=ieyr*100000+iejul*100+iehr
      ibdathr=ibyr*100000+ibjul*100+ibhr
      if(ibdathr.GT.iedathr)then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 1'
         write(io6,*) 'Starting date/hour is after ending date/hr'
         write(io6,*) 'IBDATHR = ',ibdathr
         write(io6,*) 'IEDATHR = ',iedathr
         lerrcf=.TRUE.
      endif

c --- Check for an invalid value of JDAT (input data type)
      if(jdat.NE.1 .AND. jdat.NE.2)then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 1'
         write(io6,*) 'JDAT out of range       = ',jdat
         write(io6,*) 'JDAT must be 1 (for TD-6201 file) or ',
     1                '2 (for NCDC FSL file)'
         lerrcf=.TRUE.
      endif

c --- Check for an invalid value of IFMT (input data format)
      if(ifmt.NE.1 .AND. ifmt.NE.2)then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 1'
         write(io6,*) 'IFMT out of range       = ',ifmt
         write(io6,*) 'IFMT must be 1 (for slash-delimited file) or ',
     1                '2 (for comma-delimited file)'
         lerrcf=.TRUE.
      endif

c --- Check for an invalid value of PSTOP (mb)
      if(pstop.GT.1500.0 .OR. pstop.LT.0.0)then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 1'
         write(io6,*) 'PSTOP(mb) out of range = ',pstop
         lerrcf=.TRUE.
      endif

c --- Check for an invalid value of ISUB (substitute option file)
      if(isub.NE.0 .AND. isub.NE.1 .AND. isub.NE.2)then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 1'
         write(io6,*) 'ISUB out of range       = ',isub
         write(io6,*) 'ISUB must be 0 (NOT USED) or ',
     1                '1 (for TD-6201 file) or ',
     2                '2 (for NCDC FSL file)'
         lerrcf=.TRUE.
      endif
c --- Conditional checks on repair parameters
      if(LXTOP) then
c ---    Check for an invalid value of PVTOP (mb)
         if(pvtop.GT.1500.0 .OR. pvtop.LT.pstop)then
            write(io6,*)
            write(io6,*) 'QAINP:  Error in Input Group 1'
            write(io6,*) 'PVTOP(mb) out of range = ',pvtop
            lerrcf=.TRUE.
         endif
      endif
      if(LXSFC) then
c ---    Check for an invalid value of ZVSFC (m AGL)
         if(zvsfc.GT.5000.0 .OR. zvsfc.LE.10.0)then
            write(io6,*)
            write(io6,*) 'QAINP:  Error in Input Group 1'
            write(io6,*) 'ZVSFC (m) out of range = ',zvsfc
            lerrcf=.TRUE.
         endif
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
c --- READ62    Version: 5.54           Level: 030402              COMP
c ---           J. Scire, D. Strimaitis  Earth Tech, Inc.
c
c --- PURPOSE:  Main computational routine that reads soundings,
c               reports QA failures, and writes ouput data file.
c
c --- UPDATES:
c --- V 5.52 Level 030709  ====> V 5.53 Level 040109 (D. Strimaitis)
c           - Remove old limit of 79 levels from TD-6201 section
c --- V 5.4 Level 021024  ====> V 5.5 Level 030402 (D. Strimaitis)
c           - Update WRHD arguments (new header)
c           - Use WBAN ID in FSL file if available, otherwise use WMO
c --- V 5.2 Level 020805  ====> V 5.4 Level 021024 (D. Strimaitis)
c           - Fix bug in setting minimum level at which valid data
c             are found (bug resulted in using only temperature)
c --- V 5.1 Level 020614  ====> V 5.2 Level 020805 (D. Strimaitis)
c           - Change FSL header format for new WEB structure
c           - Remove io9 from WRHD call (params.r62 added to WRHD)
c --- V 5.1 Level 020330  ====> V 5.1 Level 020614 (D. Strimaitis)
c           - Allow station ID to be up to 8 characters
c --- V 5.0 Level 011003  ====> V 5.1 Level 020330 (D. Strimaitis)
c           - Allow either fixed or variable-length record TD6201 file
c           - Allow for profile repair and substitution
c
c --- INPUTS:
c       Parameters: IO6, IO8, IO9, IOMESG, MXLEV
c
c --- OUTPUT:  none
c
c --- COMP called by:  MAIN
c --- COMP calls:      YR4, DELTT, GOOD, WRHD, SUBMISS, SUBREP, RDSUB
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.r62'

c --- Include common blocks
      include 'control.r62'
      include 'qa.r62'
      include 'station.r62'
c
      REAL HEIGHT(mxlev),HIGHT(mxlev)
      REAL APRES(mxlev),ATEMP(mxlev),PRES(mxlev),TEMP(mxlev)
      INTEGER MON(12),LMON(12),YEAR,MONTH,DAY,HOUR,GOOD,yearm1
      INTEGER WS(mxlev),AWS(mxlev),WD(mxlev),AWD(mxlev),RH,NCDCMON2(12)
      INTEGER WS1(mxlev)
c
      CHARACTER*32 JUNK
      CHARACTER*3 NCDCMON1(12),MONTHC
      CHARACTER*2 WSUNIT
      CHARACTER*1 LATA,LONA,QIND(mxlev),TIMEF(mxlev),PRESF(mxlev),
     1  HGTF(mxlev),TEMPF(mxlev),RHF,WINDF(mxlev),TYPLEV
      character*1 ccomma
c
      real xPRES(mxlev),xHEIGHT(mxlev),xTEMP(mxlev)
      real xWS1(mxlev),xaws1(mxlev)
      integer xWD(mxlev)
      integer iws1(mxlev)

c --- Declare local logicals for identifying problems with soundings
      logical lqamb,lqabot,lqatop,lqafirst,lqapht,lqazht,lqaelev
      logical lqawd,lqaws,lqat,lqap
      logical lrmb,lrbot,lrtop,lrepair

c
      common/tmp1/etime,rh,alat,alon
      common/tmp2/junk,lata,lona,timef,presf,rhf,typlev

C                                                                       R6200860
      DATA MON/0,31,59,90,120,151,181,212,243,273,304,334/              R6200870
      DATA LMON/0,31,60,91,121,152,182,213,244,274,305,335/             R6200880
      DATA NCDCMON1/'JAN','FEB','MAR','APR','MAY','JUN',
     > 'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA NCDCMON2/1,2,3,4,5,6,7,8,9,10,11,12/
      data xws1/mxlev*0.0/,igrd/9999/
      data ccomma/','/
      data tmin/175./,tmax/322./
      data pmin/0.0/,pmax/1040./

c --- Report progress
      write(iomesg,*)'COMPUTATIONAL PHASE'

c --- Pass the starting/ending Julian days to local variables
      ibday=ibjul
      ieday=iejul
c
c --- Write the temperature values used in the QA range checks
      write(io6,6044)tmin,tmax
6044  format(/1x,'Temperature values used in range checks: '/
     1  5x,'TMIN = ',f5.1/
     2  5x,'TMAX = ',f5.1)
c
c --- Write the pressure values used in the QA range checks
      write(io6,6046)pmin,pmax
6046  format(/1x,'Pressure values used in range checks: '/
     1  5x,'PMIN = ',f6.1/
     2  5x,'PMAX = ',f6.1)

c --- Write the heading for the list file table
      WRITE(io6,6050)
6050  FORMAT(//1x,'THE FOLLOWING SOUNDINGS HAVE BEEN PROCESSED:'//1x,
     1     6X,'YEAR',3X,'MONTH',3X,'DAY',3X,'JULIAN DAY',3X,
     2 'HOUR (GMT)',3X,'NO. LEVELS EXTRACTED'/)
C                                                                       R6201130
C       INITIALIZE PREVIOUS GOOD SOUNDING TIME                          R6201140
C                                                                       R6201150
      isavyr=ibyr
      IF(IBHR.EQ.0)GO TO 100                                            R6201160
C-----STARTING HOUR = 12                                                R6201170
      JDAY2=IBDAY                                                       R6201180
      ISAV2=0                                                           R6201190
      GO TO 200                                                         R6201200
100   CONTINUE                                                          R6201210
C-----STARTING HOUR = 00                                                R6201220
      JDAY2=IBDAY-1                                                     R6201230
      ISAV2=12                                                          R6201240
200   CONTINUE                                                          R6201250

      isub0=isub
      if(isub.GT.0) then
c ---    Acquire the next available substitute sounding
         call NEXTSUB(isavyr,jday2,isav2,isub0)
      endif
      if(isub0.EQ.0) isub=0

C                                                                       R6201260
C *********************************************************************
C
C-----WRITE HEADER RECORDS
      call WRHD(ifmt,ibyr,ibday,ibhr,ieyr,ieday,iehr,
     1          pstop,jdat,LHT,LTEMP,LWD,LWS,
     2          ielevm,stnid,cname,clat,clon,ver,level)
C
C *********************************************************************
C

c --- Top of LOOP over soundings
c ---------------------------------------------------------------------
1000  CONTINUE                                                          R6201270
      IF (JDAT.EQ.1) THEN
C                                                                       R6201280
C-----READ TD-6201 SOUNDING FROM UNIT ITD                               R6201290
C                                                                       R6201300
c        READ(io8,6100,END=2000) STNID,LAT,LATA,LON,LONA,YEAR,MONTH,    R6201310
c     1  DAY,HOUR,NUMLEV,(QIND(I),ETIME,PRES(I),HEIGHT(I),              R6201320
c     2  TEMP(I),RH,WD(I),WS(I),TIMEF(I),PRESF(I),HGTF(I),TEMPF(I),     R6201330
c     3  RHF,WINDF(I),TYPLEV,I=1,79)                                    R6201340
c ---   Read the full 4-digit year
c6100  FORMAT(3X,A5,I4,A1,I5,A1,2X,4(I2),I3,
c     1      (79(A1,F4.1,F5.1,F6.0,F4.1,3(I3),7A1)))
c6100    FORMAT(3X,A5,I4,A1,I5,A1,i4,3(I2),I3,
c     1        (79(A1,F4.1,F5.1,F6.0,F4.1,3(I3),7A1)))
c
c ---   Allow variable-length record
        READ(io8,6100,END=2000) STNID,LAT,LATA,LON,LONA,YEAR,MONTH,
     1  DAY,HOUR,NUMLEV,(QIND(I),ETIME,PRES(I),HEIGHT(I),
     2  TEMP(I),RH,WD(I),WS(I),TIMEF(I),PRESF(I),HGTF(I),TEMPF(I),
     3  RHF,WINDF(I),TYPLEV,I=1,numlev)
6100    FORMAT(A8,I4,A1,I5,A1,i4,3(I2),I3,
     1        (200(A1,F4.1,F5.1,F6.0,F4.1,3(I3),7A1)))
c
c ---   Make sure year is YYYY (Y2K)
        call YR4(io6,yearr,ierr)
        if(ierr.NE.0) stop 'Halted in MAIN (TD-6201) - see list file'
c
        ALAT = LAT/100 + (LAT-(LAT/100*100))/60.                        R6201360
        ALON = LON/100 + (LON-(LON/100*100))/60.                        R6201370

c ---   Check for array dimensioning problems
        if(numlev.gt.mxlev)then
           write(io6,*)'ERROR -- too many levels in upper air data ',
     1     'for array dimensions'
           write(io6,*)'YEAR: ',year,' MONTH: ',month,' DAY: ',day,
     1     ' HOUR: ',hour
           write(io6,*)'NUMLEV in file = ',numlev
           write(io6,*)'MXLEV in array = ',mxlev
           stop
        endif
c
c ---   Save original number of levels for output
        mlev=numlev
c
c ---   Use 9999. for missing value indicator for height
        do i=1,numlev
          if(height(i).lt.-99998.or.height(i).gt.99998.)height(i)=9999.
        enddo
c
c ---   Set ground elevation as first sounding level (lowest one
c ---   encountered) that is non-missing
        if(height(1).lt.float(igrd).and.height(1).lt.9998.)
     &     igrd=height(1)
c
c1050  IF(NUMLEV.LE.79) GO TO 1100                                       R6201410
c      READ(io8,6150,END=2000) JUNK                                      R6201420
c6150  FORMAT(A32,79(36X))
c      NUMLEV = MAX(NUMLEV-79,79)                                        R6201430
c      GO TO 1050                                                        R6201440
      ELSEIF (JDAT.EQ.2) THEN
C
C-----READ NCDC CD-ROM Data (FSL format)
C
c ---   (DGS) Set point to read next period if there are no data recs
7099    continue
c ---   CD-ROM FSL Header #1
        READ(io8,7100,END=2000) HOUR,DAY,MONTHC,YEAR
c ---   Read full 4-digit year
c7100    FORMAT(7X,2I7,6X,A3,6X,I2)
7100    FORMAT(7X,2I7,6X,A3,4X,I4)
c
c ---   Make sure year is YYYY (Y2K)
        call YR4(io6,year,ierr)
        if(ierr.NE.0) stop 'Halted in MAIN (FSL) - see list file'
c
        DO II=1,12
        IF (NCDCMON1(II).EQ.MONTHC) THEN
          MONTH=NCDCMON2(II)
          GOTO 1051
        ENDIF
        ENDDO
c ---   CD-ROM FSL Header #2
c ---   CD format is (3i7,2F7.2,2i7), read as (9x,a5,7X,2F7.2,i7) [OLD]
c ---   WEB format is now (3i7,F7.2,a1,f6.2,a1,i6,i7), where the new A1
c ---   variables are N,S,E, or W for the lat/lon
c ---   Use the new format for both, but do not interpret the N,S,E,W
c ---   (Take STNID from /STATION/ common)
1051    READ(io8,7120,END=2000) ALAT,LATA,ALON,LONA,igrd
c7120    FORMAT(7x,a7,7X,F7.2,a1,f6.2,a1,i6)
7120    FORMAT(21x,F7.2,a1,f6.2,a1,i6)
c ---   CD-ROM FSL Header #3
        READ(io8,7130,END=2000)nlines
7130    FORMAT(28X,I7)
c
c ---   Number of levels = number of lines of data - 4 ID lines
        numlev=nlines-4
        if(numlev.gt.mxlev)then
           write(io6,*)'ERROR -- too many levels in upper air data ',
     1     'for array dimensions'
           write(io6,*)'YEAR: ',year,' MONTHC: ',monthc,' DAY: ',day,
     1     ' HOUR: ',hour
           write(io6,*)'NLINES = ',nlines,' NUMLEV = ',numlev
           write(io6,*)'MXLEV = ',mxlev
           stop
        endif
c
c ---   Save original number of levels for output
        mlev=numlev
c
c ---   CD-ROM FSL Header #4
        READ(io8,7140,END=2000) WSUNIT
7140    FORMAT(16X,5x,26X,A2)

c ---   (DGS) Get next sounding now if there are no data records
        if(numlev.EQ.0) goto 7099

Cvrt -- Changed to check if levels are out of order (by pressure). If so
cvrt -- then delete that record and get the next record with lower pressure.
cvrt    READ(io8,7150,END=2000) (PRES(I),HEIGHT(I),TEMP(I),
cvrt >   WD(I),WS1(I),I=1,NUMLEV)

        READ(io8,7150,END=2000) (xPRES(I),xHEIGHT(I),xTEMP(I),
     >   xWD(I),iWS1(I),I=1,NUMLEV)
7150    FORMAT(7X,F7.0,F7.0,F7.1,7X,2I7)
         ic = 1
         pres(1) = xpres(1)
         height(1) = xheight(1)
         temp(1) = xtemp(1)
         wd(1) = xwd(1)
         ws1(1) = iws1(1)
         do 700 i=2,numlev
           if (xpres(i).lt.pres(ic)) then
               ic = ic + 1
               pres(ic) = xpres(i)
               height(ic) = xheight(i)
               temp(ic) = xtemp(i)
               wd(ic) = xwd(i)
               ws1(ic) = iws1(i)
           end if
700      continue
         numlev = ic

        DO II=1,NUMLEV
           IF (HEIGHT(II).GT.32766.) HEIGHT(II)=9999.

cvrt missing temps should be 999.9 not 99.9
cvrt       IF (TEMP(II).EQ.3276.7) TEMP(II)=99.9
           IF (TEMP(II).EQ.3276.7) TEMP(II)=999.9
cvrtend

           IF (WD(II).EQ.32767) WD(II)=999
           IF (WS1(II).EQ.32767)xws1(II)=999.9
        ENDDO
        IF (WSUNIT.EQ.'kt') THEN
         DO II=1,NUMLEV
            IF (WS1(II).NE.32767)xws1(II)=WS1(II)*0.514791
         ENDDO
        ELSEIF (WSUNIT.EQ.'ms') THEN
         DO II=1,NUMLEV
            IF (WS1(II).NE.32767)xws1(II)=WS1(II)/10.
         ENDDO
        ENDIF
c
c ---   Compute integer equivalent (WS) of real wind speed (XWS1) -- for
c ---   use in missing value checks
        do ii=1,numlev
           ws(ii)=xws1(ii)
        enddo
      ENDIF
C                                                                       R6201380
C     IF CONTINUATION OF LAST SOUNDING, IGNORE AND READ NEXT SOUNDING   R6201390
C                                                                       R6201400
1100  CONTINUE                                                          R6201450
C                                                                       R6201460
C*******ROUTINE TO CONVERT DATES TO JULIAN                              R6201470
C                                                                       R6201480
        JDAY=MON(MONTH)+DAY                                             R6201490
        IF(MOD(YEAR,4).EQ.0)JDAY=LMON(MONTH)+DAY                        R6201500
C                                                                       R6201530
C       CHECK FOR BEGINNING AND ENDING TIMES                            R6201540
C                                                                       R6201550
      IF(YEAR.LT.IBYR) GO TO 1000                                       R6201560
      IF(YEAR.GT.IEYR) GO TO 5000                                       R6201570
      IF(YEAR.EQ.IBYR.AND.JDAY.LT.IBDAY) GO TO 1000                     R6201580
      IF(YEAR.EQ.IEYR.AND.JDAY.GT.IEDAY) GO TO 5000                     R6201590
      IF(YEAR.EQ.IBYR.AND.JDAY.EQ.IBDAY.AND.HOUR.LT.IBHR) GO TO 1000    R6201600
      IF(YEAR.EQ.IEYR.AND.JDAY.EQ.IEDAY.AND.HOUR.GT.IEHR) GO TO 5000    R6201610
C                                                                       R6201620
C       COMPRESS ARRAYS IF MISSING VALUES ARE FOUND                     R6201630
C                                                                       R6201640
      KK=0                                                              R6201650
      DO 1200 JJ=1,NUMLEV                                               R6201660
      IF (JDAT.EQ.1) THEN
        IF(GOOD(QIND(JJ)).EQ.0) GO TO 1200                              
        IF(LHT .AND. (HEIGHT(JJ).gt.9998. .OR. GOOD(HGTF).EQ.0))
     1   GO TO 1200                                                     
        IF(LTEMP .AND. (ABS(TEMP(JJ)).GE.99. .OR. GOOD(TEMPF).EQ.0))
     1   GO TO 1200                                                    
        IF(LWD .AND. (WD(JJ).GE.999 .OR. GOOD(WINDF).EQ.0))GO TO 1200
        IF(LWS .AND. (WS(JJ).ge.999 .OR. GOOD(WINDF).EQ.0))GO TO 1200
      ELSEIF (JDAT.EQ.2) THEN
        IF(LHT .AND. HEIGHT(JJ).gt.9998) GO TO 1200                    
        IF(LTEMP .AND. TEMP(JJ).gt.998.) GO TO 1200
        IF(LWD .AND. WD(JJ).ge.999) GO TO 1200
        IF(LWS .AND. WS(JJ).ge.999) GO TO 1200
      ENDIF
      KK=KK+1                                                           R6201740
      APRES(KK)=PRES(JJ)                                                R6201750
      ATEMP(KK)=TEMP(JJ)                                                R6201760
      AWS(KK)=WS(JJ)                                                    R6201770
      xaws1(kk)=xws1(jj)
      AWD(KK)=WD(JJ)                                                    R6201780
      HIGHT(KK)=HEIGHT(JJ)                                              R6201790
1200  CONTINUE                                                          R6201800

      if (kk.eq.0) goto 1000

      NLEV=KK                                                           R6201810
      DO 1300 LL=1,NLEV                                                 R6201820
      PRES(LL)=APRES(LL)                                                R6201830
      TEMP(LL)=ATEMP(LL)                                                R6201840
      WD(LL)=AWD(LL)                                                    R6201850
      WS(LL)=AWS(LL)                                                    R6201860
      xws1(LL)=xaws1(LL)
      HEIGHT(LL)=HIGHT(LL)                                              R6201870
1300  CONTINUE                                                          R6201880
c
c --- Change missing value indicator for temperature to 999.9
c --- when using TD-6201 data (JDAT=1).  Already converted to 999.9
c --- when JDAT=2
      if(jdat.eq.1)then
         do LL=1,nlev
c ---       TD-6201 format uses -99.9 as missing temperature flag
            if(temp(LL).le.-99.)temp(LL)=999.9
         enddo
      endif
C                                                                       R6201890
C-----DETERMINE LEVELS UP TO PSTOP                                      R6201900
C                                                                       R6201910
      KSTOP = 0                                                         R6201920
      DO 1500 I = 1,NLEV                                                R6201930
      IF(PRES(I).LE.PSTOP) THEN                                         R6201940
          ISTOP = I                                                     R6201950
          GO TO 1600                                                    R6201960
      ENDIF                                                             R6201970
1500  CONTINUE                                                          R6201980
      ISTOP = NLEV                                                      R6201990
      IF(ABS(PRES(NLEV)-PSTOP).GT.1.0) KSTOP = 1                        R6202000
1600  CONTINUE                                                          R6202010

      if (istop.eq.1) goto 1000

c --- SUBSTITION for missing soundings
1610  isub0=isub
      more=0
      if(isub.GT.0) then
         call SUBMISS(isavyr,jday2,isav2,year,jday,hour,more,stnid)
         if(more.GT.0) call NEXTSUB(isavyr,jday2,isav2,isub0)
      endif
      isub=isub0
      if(more.GT.0) goto 1610

C                                                                       R6202140
C-----CHECK FOR MISSING DAYS                                            R6202150
C                                                                       R6202160
      IF(JDAY.EQ.JDAY2) THEN
         JDAY1=JDAY
         GO TO 1700
      ELSE                                                              R6202170
         JDAY1=JDAY2
         JDAY2=JDAY
      ENDIF
c*-*-* EMI Modification (7/29/97)
c-emi Allow data to span from one year to the next without a missing
c     day message unless there really is a missing day.
c-emi IF(JDAY1.EQ.(JDAY2-1)) GO TO 1700                                 R6202200
      if(isavyr.EQ.year .and. JDAY1.EQ.(JDAY2-1))then
        go to 1700
      else
        yearm1 = year-1
        if(MOD(isavyr,4).EQ.0)then
          idaysv = 366
        else
          idaysv = 365
        endif
        if(isavyr.EQ.yearm1 .AND. jday1.EQ.idaysv .AND. jday.EQ.1)
     +    go to 1700
      endif

      WRITE(io6,6070)
      WRITE(io9,6070)
6070  FORMAT(1X,'->->->Missing day(s)')
1700  CONTINUE                                                          R6202230
c*-*-* EMI Modification (7/29/97)
      isavyr = year
c*-*-* End of EMI Modification (7/29/97)
C                                                                       R6202240
C-----CHECK FOR MISSING/DUPLICATE SOUNDINGS                             R6202250
C                                                                       R6202260
      ISAV1=ISAV2                                                       R6202270
      ISAV2=HOUR                                                        R6202280

      call deltt(isavyr,jday1,isav1,year,jday,hour,idelhr)
      if(idelhr.NE.0 .AND. idelhr.LE.12) go to 1900

c --- When substituting soundings simply drop duplicate soundings
      if(isub.GT.0 .AND. idelhr.EQ.0) goto 1000

      WRITE(io6,6080)
      WRITE(io9,6080)
6080  FORMAT(1X,'->->->Missing/duplicate sounding or time > 12 hours')
      GO TO 1900                                                        R6202330
1900  CONTINUE                                                          R6202380
c
c --- Convert to Kelvin if temp. is not missing (avoid real equivalence
c --- check)
      do 1650 i=1,istop
        if (temp(i).lt.999.) temp(i) = temp(i)+273.2
1650  continue
c
c --- Fill the real WS array if using TD-6201 data (JDAT=1) with a
c --- comma-delimited file
      if(jdat.eq.1.and.ifmt.eq.2)then
         do i=1,istop
            xws1(i)=ws(i)
            if(ws(i).eq.999)xws1(i)=999.9
         enddo
      endif      
c
c -----------------------------------
c --- Perform QA checks of final data
c -----------------------------------
      lrmb=.FALSE.
      lrbot=.FALSE.
      lrtop=.FALSE.

      lqamb=.FALSE.
      lqabot=.FALSE.
      lqatop=.FALSE.
      lqafirst=.FALSE.
      lqapht=.FALSE.
      lqazht=.FALSE.
      lqaelev=.FALSE.
      lqawd=.FALSE.
      lqaws=.FALSE.
      lqat=.FALSE.
      lqap=.FALSE.

c --- Check for non-repairable problems
c -------------------------------------

c --- Check that first level is at the ground
      iel=height(1)
      if(iel.gt.igrd) lqafirst=.TRUE.

c --- Check that the pressure is decreasing with height
      do i=2,istop
         if(pres(i).ge.pres(i-1))lqapht=.TRUE.
      enddo

c --- Check that the elevation is increasing with height
      do i=2,istop
         im1=i-1
         if(height(i).le.height(im1).and.
     1   height(i).le.9998..and.height(im1).le.9998.) lqazht=.TRUE.
      enddo

c --- Check for missing height values
      do i=1,istop
        if(height(i).gt.9998.) lqaelev=.TRUE.
      enddo

c --- Check range of non-missing wind directions
      do i=1,istop
        if(wd(i).lt.999.and.(wd(i).lt.0.or.wd(i).gt.360)) lqawd=.TRUE.
      enddo

c --- Check range of non-missing wind speeds
      do i=1,istop
        if(xws1(i).lt.0.0) lqaws=.TRUE.
      enddo

c --- Check range of non-missing temperatures
      do i=1,istop
        if(temp(i).lt.999.8.and.(temp(i).lt.tmin.or.temp(i).gt.tmax))
     1    lqat=.TRUE.
      enddo

c --- Check range of non-missing pressures (missing value = -99.9)
      do i=1,istop
        if(pres(i).lt.pmin.or.pres(i).gt.pmax) lqap=.TRUE.
      enddo

c --- Check for 'repairable' problems
c -----------------------------------

c --- Check for short sounding
      if(kstop.NE.0) lqamb=.TRUE.

c --- Check for missing data at top of sounding
      if(height(istop).gt.9998..or.temp(istop).gt.999.8.or.
     1 wd(istop).ge.999.or.xws1(istop).gt.998.) lqatop=.TRUE.

c --- Check for missing data at bottom of sounding
      if(height(1).gt.9998..or.temp(1).gt.999.8.or.
     1 wd(1).ge.999.or.xws1(1).gt.998.) lqabot=.TRUE.


c --- Skip any repair attempts if other problems exist
c ----------------------------------------------------
      lrepair=.TRUE.
      if(lqafirst .OR.  lqapht .OR. lqazht .OR. lqaelev .OR.
     &   lqawd .OR. lqaws .OR. lqat .OR. lqap) lrepair=.FALSE.

c --- Repair QA problems at top of sounding if possible
      if(lrepair .AND. lxtop .AND. (lqamb.OR.lqatop)) then
c ---    Extrapolate upwards
         ntop=istop
         if(lqamb) ntop=istop+1
         if(ntop.LE.mxlev) then
c ---       Get valid data levels for extrapolation
            ilevs=1
            ilevd=1
            ilevt1=1
            ilevt2=1
            ilevz1=1
            ilevz2=1
            do i=1,istop
               if(xws1(i).LE.998.) ilevs=i
               if(wd(i).LE.998) ilevd=i
               if(height(i).LE.9998. .AND. pres(i).GT.-98.) ilevz2=i
               if(temp(i).LE.999.8 .AND. height(i).LE.9998.) ilevt2=i
            enddo
            do i=1,(ilevz2-1)
               if(height(i).LE.9998. .AND. pres(i).GT.-98.) ilevz1=i
            enddo
            do i=1,(ilevt2-1)
               if(temp(i).LE.999.8 .AND. height(i).LE.9998.) ilevt1=i
            enddo
c ---       Check against user's constraint
            ii=ilevs
            ii=MIN(ii,ilevd)
            ii=MIN(ii,ilevz2)
            ii=MIN(ii,ilevt2)
            if(pvtop.GE.pres(ii)) then
               if(lqamb) then
c ---             Fill everything at PSTOP level
                  pres(ntop)=pstop
                  wd(ntop)=wd(ilevd)
                  xws1(ntop)=xws1(ilevs)
                  ws(ntop)=ws(ilevs)
                  zslope=(height(ilevz2)-height(ilevz1))/
     &                   (pres(ilevz2)-pres(ilevz1))
                  height(ntop)=height(ilevz2)+zslope*
     &                        (pres(ntop)-pres(ilevz2))
                  tslope=(temp(ilevt2)-temp(ilevt1))/
     &                   (height(ilevt2)-height(ilevt1))
                  temp(ntop)=temp(ilevt2)+tslope*
     &                       (height(ntop)-height(ilevt2))
                  lrmb=.TRUE.
                  istop=ntop
               elseif(lqatop) then
c ---             Fill missing variables at top level
                  if(wd(istop).ge.999) wd(istop)=wd(ilevd)
                  if(xws1(istop).gt.998.) then
                     xws1(istop)=xws1(ilevs)
                     ws(istop)=ws(ilevs)
                  endif
                  if(height(istop).gt.9998.) then
                     zslope=(height(ilevz2)-height(ilevz1))/
     &                      (pres(ilevz2)-pres(ilevz1))
                     height(istop)=height(ilevz2)+zslope*
     &                             (pres(istop)-pres(ilevz2))
                  endif
                  if(temp(istop).gt.999.8) then
                     tslope=(temp(ilevt2)-temp(ilevt1))/
     &                      (height(ilevt2)-height(ilevt1))
                     temp(istop)=temp(ilevt2)+tslope*
     &                           (height(istop)-height(ilevt2))
                  endif
                  lrtop=.TRUE.
               endif
c ---          Reset QA logicals
               lqamb=.FALSE.
               lqatop=.FALSE.
            endif
         endif
      endif

c --- Repair QA problems at bottom of sounding if possible
      if(lrepair .AND. lxsfc .AND. lqabot) then
c ---    Temperature and height cannot be missing
         if(temp(1).LT.999.8 .AND. height(1).LT.9998.) then
c ---       Extrapolate wind to surface               
c ---       Get valid data levels for extrapolation
            ilevs=istop
            ilevd=istop
            do i=istop,1,-1
               if(xws1(i).LE.998. .AND. height(i).LE.9998.) ilevs=i
               if(wd(i).LE.998 .AND. height(i).LE.9998.) ilevd=i
            enddo
c ---       Check against user's constraint
            ii=ilevs
            ii=MAX(i,ilevd)
            zagl=height(ii)-height(1)
            if(zvsfc.GE.zagl) then
c ---          Fill missing variables at surface
               if(wd(1).ge.999) wd(1)=wd(ilevd)
               if(xws1(1).gt.998.) then
                  zagl=height(ilevs)-height(1)
                  if(xws1(ilevs).LE.1.1) then
                     xws1(1)=xws1(ilevs)
                     ws(1)=ws(ilevs)
                  elseif(zagl.LE.11.) then
                     xws1(1)=xws1(ilevs)
                     ws(1)=ws(ilevs)
                  else
                     xws1(1)=xws1(ilevs)*(0.1*zagl)**(-0.15)
                     ws(1)=NINT(xws1(1))
                  endif
               endif
               lrbot=.TRUE.
c ---          Reset QA logical
               lqabot=.FALSE.
            endif
         endif
      endif

c --- Try to replace this sounding with a substitute if any problems
c --- remain
1660  isub0=isub
      more=0
      ireplace=0
      if(isub.GT.0 .AND. (lqamb .OR. lqabot .OR. lqatop .OR. lqafirst
     &                    .OR.  lqapht .OR. lqazht .OR. lqaelev .OR.
     &                    lqawd .OR. lqaws .OR. lqat .OR. lqap)) then
         call SUBREP(isavyr,jday2,isav2,more,ireplace,stnid)
         if(more.GT.0) call NEXTSUB(isavyr,jday2,isav2,isub0)
      endif
      isub=isub0
      if(more.GT.0 .AND. ireplace.EQ.0) then
c ---    Try to replace sounding again
         goto 1660
      elseif(ireplace.EQ.1) then
c ---    Sounding has been replaced, so go on to next period
         goto 1000
      endif

C                                                                       R6202390
C-----WRITE TO LINE PRINTER AND UPPER AIR OUTPUT FILE                   R6202030
C                                                                       R6202040
      WRITE(io6,6060)YEAR,MONTH,DAY,JDAY,HOUR,ISTOP
6060   FORMAT(7X,I4,4X,I2,6X,I2,7X,I3,9X,I2,14X,I5)
       WRITE(io9,6200) STNID,YEAR,MONTH,DAY,HOUR,mlev,ISTOP
6200   FORMAT(3X,'6201',2X,A8,3X,i4,3I2,2X,I5,T66,I5)

      if(lrmb.OR.lrtop) then
         write(io6,*)' ->->->Data at top of sounding repaired'
      endif
      if(lrbot) then
         write(io6,*)' ->->->Data at bottom of sounding repaired'
      endif

      if(lqamb) then
          WRITE(io6,6065) PSTOP
          WRITE(io9,6065) PSTOP
6065      FORMAT(1X,'->->->Top ','of sounding is below the ',F6.1,
     1              '-mb level')
      endif
      if(lqabot) then
         write(io6,*)' ->->->Data at bottom of sounding is missing'
         write(io9,*)' ->->->Data at bottom of sounding is missing'
      endif
      if(lqatop) then
         write(io6,*)' ->->->Data at top of sounding is missing'
         write(io9,*)' ->->->Data at top of sounding is missing'
      endif
      if(lqafirst)then
         write(io6,*)' ->->->First level in sounding is above surface'
         write(io9,*)' ->->->First level in sounding is above surface'
      endif
      if(lqapht)then
         write(io6,*)' ->->->Pressure increasing with height'
         write(io9,*)' ->->->Pressure increasing with height'
      endif
      if(lqazht)then
         write(io6,*)' ->->->Elevation is decreasing with height'
         write(io9,*)' ->->->Elevation is decreasing with height'
      endif      
      if(lqaelev)then
         write(io6,*)' ->->->Elevation missing'
         write(io9,*)' ->->->Elevation missing'
      endif  
      if(lqawd)then
         write(io6,*)' ->->->Invalid wind direction (< 0 or > 360)'
         write(io9,*)' ->->->Invalid wind direction (< 0 or > 360)'
      endif  
      if(lqaws)then
         write(io6,*)' ->->->Invalid wind speed (< 0)'
         write(io9,*)' ->->->Invalid wind speed (< 0)'
      endif   
      if(lqat)then
         write(io6,*)' ->->->Invalid temperature (< TMIN or > TMAX)'
         write(io9,*)' ->->->Invalid temperature (< TMIN or > TMAX)'
      endif
      if(lqap)then
         write(io6,*)' ->->->Invalid or missing pressure ',
     1    '(< PMIN or > PMAX)'
         write(io9,*)' ->->->Invalid or missing pressure ',
     1    '(< PMIN or > PMAX)'
      endif                                  

c -------------------------------------
c --- Write the data to the UP.DAT file
c -------------------------------------
      if(ifmt.EQ.1)then
c ---   Write a slash-delimited file (original format)
        WRITE(io9,6210) (PRES(I),HEIGHT(I),TEMP(I),WD(I),WS(I),
     1   I=1,ISTOP)
6210    FORMAT(4(3X,F6.1,'/',F5.0,'/',F5.1,'/',I3,'/',I3))
      else
c ---   Write a comma-delimited file
        istopm1=istop-1
        WRITE(io9,6211) (PRES(I),HEIGHT(I),TEMP(I),WD(I),xws1(I),
     1   ccomma,I=1,istopm1),
     2   PRES(istop),HEIGHT(istop),TEMP(istop),WD(istop),xws1(istop)
6211    FORMAT(4(3X,F6.1,',',F5.0,',',F5.1,',',I3,',',f5.1,a1))
      endif
C                                                                       R6202140
      GO TO 1000                                                        R6202400
2000  WRITE(io6,6090)YEAR,JDAY
6090  FORMAT(/20X,'EOF ON INPUT'/20X,'LAST YR,DAY READ =  ',I4,',',I3)
C                                                                       R6202420
5000  CONTINUE                                                          R6202430

      return                                                            R6202440
      END                                                               R6202770
c-----------------------------------------------------------------------
      INTEGER FUNCTION GOOD(IQUAL)
c-----------------------------------------------------------------------
C                                                                       GOO00030
c --- READ62   Version: 5.54      Level: 941215                    GOOD
C
C PURPOSE: CHECKS QUALITY INDICATOR TO DETERMINE WHETHER OR NOT TO      GOO00040
C           ACCEPT THE UPPER AIR OBSERVATION AS VALID                   GOO00050
C                                                                       GOO00060
C ASSUMPTIONS: TDF6201 FORMAT                                           GOO00070
C                                                                       GOO00080
C LIMITATIONS: NMC INDICATORS A-Z ARE NOT TESTED FOR BAD DATA           GOO00090
C                                                                       GOO00100
C ARGUMENTS                                                             GOO00110
C  PASSED:                                                              GOO00120
C       IQUAL   CHAR    QUALITY INDICATOR: 0-9 OR A-Z                   GOO00130
C                                                                       GOO00140
C  RETURNED:                                                            GOO00150
C       FUNCTION GOOD: 0 IF BAD DATA; 1 IF GOOD DATA                    GOO00160
C                                                                       GOO00170
C  MEANING OF QUALITY INDICATORS FOR TDF6201 DATA:                      GOO00180
C                                                                       GOO00190
C      0       ORIGINAL VALUES ARE CORRECT                              GOO00200
C      1       ORIGINAL VALUES ARE MISSING                              GOO00210
C      2       ORIGINAL VALUES ARE DOUBTFUL, A CORRECTED LEVEL FOLLOWS  GOO00220
C      3       ORIGINAL VALUES ARE DOUBTFUL, UNCORRECTED                GOO00230
C      4       ORIGINAL VALUES ARE IN ERROR, A CORRECTED LEVEL FOLLOWS  GOO00240
C      5       ORIGINAL VALUES ARE IN ERROR, UNCORRECTED                GOO00250
C      6       CORRRECTED LEVEL                                         GOO00260
C      9       LEVEL NOT CHECKED                                        GOO00270
C     A-Z      SUPPLIED BY NMC, HAVE CHANGED MANY TIMES OVER THE YEARS  GOO00280
C                                                                       GOO00290
C       GOOD RETURNS 0 IF CODE IS 1, 2, 3, 4, OR 5; 1 OTHERWISE         GOO00300
C                                                                       GOO00310
C                                                                       GOO00320
C CALLING ROUTINES: READ62                                              GOO00330
C                                                                       GOO00340
C-----------------------------------------------------------------------GOO00350
C                                                                       GOO00360
        CHARACTER*1 IQUAL                                               GOO00380
C                                                                       GOO00390
        GOOD = 1                                                        GOO00400
        IF(IQUAL.EQ.'1' .OR. IQUAL.EQ.'2' .OR. IQUAL.EQ.'3' .OR.        GOO00410
     1   IQUAL.EQ.'4' .OR. IQUAL.EQ.'5') GOOD = 0                       GOO00420
        RETURN                                                          GOO00430
        END                                                             GOO00440
c----------------------------------------------------------------------
      subroutine wrhd(iform,ibyr,ibjul,ibhr,ieyr,iejul,iehr,
     1                pstop,jdat,LHT,LTEMP,LWD,LWS,
     2                ielevm,stnid,cname,clat,clon,cver,clev)
c----------------------------------------------------------------------
c
c --- READ62     Version: 5.54     Level: 030402                   WRHD
c ---            J. Scire, D. Strimaitis, Earth Tech, Inc.
c
c --- Write the header records for the UP.DAT file
c
c --- UPDATES:
c --- V 5.2 Level 020805  ====> V 5.5 Level 030402 (D. Strimaitis)
c           - New header record structure
c
c --- V 5.1 Level 020614  ====> V 5.2 Level 020805 (D. Strimaitis)
c           - Toggle header format using ENVPOP parameter
c           - Include PARAMS.R62 and remove IO unit from arg list
c
c --- V 5.1 Level 020330  ====> V 5.1 Level 020614 (D. Strimaitis)
c           - Station ID as character variable
c
c
c --- INPUTS:
c             IFORM - integer    - Data format flag (1=slash delimiter,
c                                  2=comma delimiter - more precision)
c              IBYR - integer    - Beginning year of data (4 digits)
c             IBJUL - integer    - Beginning Julian day number
c              IBHR - integer    - Beginning hour
c              IEYR - integer    - Ending year of data (4 digits)
c             IEJUL - integer    - Ending Julian day number
c              IEHR - integer    - Ending hour
c             PSTOP - integer    - Top pressure level (mb) of data
c              JDAT - integer    - Original data format (1=TD-6201,
c                                  2=NCDC FSL)
c               LHT - logical    - Drop Sounding level if no height
c             LTEMP - logical    - Drop Sounding level if no temperature
c               LWD - logical    - Drop Sounding level if no direction
c               LWS - logical    - Drop Sounding level if no speed
c            IELEVM - integer    - station elevation (m MSL)
c          STNID - character*8   - station identification code
c          CNAME - character*4   - Name of each station (----)
c           CLAT - character*16  - Latitude of each station (deg[N/S])
c           CLON - character*16  - Longitude of each station (deg[E/W])
c           CVER - character*12  - Version of processor
c           CLEV - character*12  - Level of processor
c
c --- OUTPUT:  none
c
c --- WRHD called by:  COMP
c --- WRHD calls:      none
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.r62'
c
      logical lht,ltemp,lwd,lws

      character*4 cname
      character*8 stnid
      character*16 clat,clon
      character*12 cver,clev
      character*1 q

      character*4 xyunit
      character*8 datum, pmap
      character*12 daten
      character*16 dataset,dataver
      character*64 datamod
      character*80 comment1

c --- Configure output variables
      data dataset/'UP.DAT'/, dataver/'2.0'/
      data datamod/'Header structure with coordinate parameters'/
      data ncomment/1/
      data comment1/'Produced by READ62 Version: '/

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
      if(envpop.EQ.0) then
         pmap='NONE    '
      elseif(envpop.EQ.1) then
         pmap='LL      '
         datum='WGS-G   '
         daten='10-10-2002  '
         xyunit='DEG '
      endif

c --- Write header
      write(io9,'(2a16,a64)') dataset,dataver,datamod
      write(io9,'(i4)') ncomment
      write(io9,'(a80)') comment1
      write(io9,'(a8)') pmap
      if(envpop.EQ.0) then
c ---    Header witout location data
         WRITE (io9,'(1X,6I5,F5.0,2I5)')IBYR,IBJUL,IBHR,
     &                         IEYR,IEJUL,IEHR,PSTOP,JDAT,iform
         WRITE (io9,'(1X,4(4X,L1))')LHT,LTEMP,LWD,LWS
      elseif(envpop.EQ.1) then
c ---    Header with location data
         write(io9,'(a8,a12)') datum,daten
         write(io9,'(a4)') xyunit
         WRITE (io9,'(1X,6I5,F5.0,2I5)')IBYR,IBJUL,IBHR,
     &                         IEYR,IEJUL,IEHR,PSTOP,JDAT,iform
         WRITE (io9,'(1X,4(4X,L1))')LHT,LTEMP,LWD,LWS
         write(io9,20)stnid,q,cname,q,q,clat,q,q,clon,q,ielevm
      else
         write(*,*)'WRHD:  invalid parameter ENVPOP in PARAMS.R62'
         write(*,*)'       Expected 0 or 1'
         write(*,*)'       Found ',envpop
         stop
      endif

20    format(a8,2x,a1,a4,a1,2x,2(a1,a16,a1,2x),i7)

      return
      end
c----------------------------------------------------------------------
      subroutine rdhdsub(io)
c----------------------------------------------------------------------
c
c --- READ62     Version: 5.54     Level: 030402                RDHDSUB
c ---            D. Strimaitis, Earth Tech, Inc.
c
c --- Skip the header records for the UP.DAT file (for substitutions)
c
c --- UPDATES:
c --- V 5.4 Level 020330  ====> V 5.5 Level 030402 (D. Strimaitis)
c           - New header record structure
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of data file
c
c --- OUTPUT:  none
c
c --- RDHDSUB called by:  SETUP
c --- RDHDSUB calls:      none
c----------------------------------------------------------------------
c
      character*8 pmap
      character*16 dataset,dataver
      character*64 datamod
      character*80 comment1

      read(io,'(2a16,a64)') dataset,dataver,datamod
c --- Check first field of record 1
      if(dataset.EQ.'UP.DAT') then
         read(io,*) ncomment
         do i=1,ncomment
            read(io,'(a80)') comment1
         enddo
         read(io,'(a8)') pmap
         if(pmap.EQ.'NONE   ') then
c ---       Skip remaining 2 header records
            read(io,*)
            read(io,*)
         else
c ---       Skip remaining 5 header records
            read(io,*)
            read(io,*)
            read(io,*)
            read(io,*)
            read(io,*)
         endif
      elseif(dataset.EQ.'UPPER-AIR-MET') then
c ---    Skip remaining 3 header records of older header
         read(io,*)
         read(io,*)
         read(io,*)
      else
c ---    Must be oldest header with 2 records
c ---    Skip remaining header record
         read(io,*)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine nextsub(lastyr,lastjul,lasthr,isub0)
c----------------------------------------------------------------------
c
c --- READ62   Version: 5.54      Level: 020330                 NEXTSUB
c ---          D. Strimaitis,  Earth Tech, Inc.
c
c --- PURPOSE:  Access data in the substitution UP.DAT file
c               to obtain the next available sounding arrays
c
c --- INPUTS:
c               LASTYR - integer    - Year of last sounding (GMT)
c              LASTJUL - integer    - Julian day (GMT) of last sounding
c               LASTHR - integer    - Time (GMT) of last sounding
c
c        Common block /submet/ variables
c            NTZSUB
c
c        Parameters: IO6
c
c --- OUTPUT:
c                ISUB0 - integer    - ISUB (altered if EOF reached)
c
c
c --- NEXTSUB called by:  COMP
c --- NEXTSUB calls:      RDSUB
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.r62'
      include 'control.r62'
      include 'submet.r62'

c --- Set ISUB0
      isub0=isub

c --- Set last date-time
      ilast=lastyr*100000+lastjul*100+lasthr

c --- Get substitute UP.DAT data
10    call RDSUB(ierr)
      if(ierr.NE.0) then
         isub0=0
         write(io6,*)'       No further substitutions are made'
         write(io6,*)'       after YYYYJJJHH = ',ilast
         write(io6,*)
         write(io6,*)
         return
      endif

c --- Check for date-time later than last
      if(ilast.GE.NTZSUB) goto 10

      return
      end
c----------------------------------------------------------------------
      subroutine rdsub(ierr)
c----------------------------------------------------------------------
c
c --- READ62   Version: 5.54      Level: 020330                   RDSUB
c ---          J. Scire, D. Strimaitis,  Earth Tech, Inc.
c
c --- PURPOSE:  Read a set of upper air soundings -- input data
c               include:  pressure (mb), height (m above MSL),
c               temp (deg. C), wind direction (deg.), and
c               wind speed (m/s)
c
c --- INPUTS:
c
c        Common block /CONTROL/ variables
c            ISUB
c        Parameters: MXLEV, IO6, IO18
c
c --- OUTPUT:
c                 IERR - integer       - Error flag for read access
c                                        0 = No Error
c                                        1 = Error
c
c        Common block /submet/ variables
c            PSUB(mxlev), ZLSUB(mxlev), TZSUB(mxlev),
c            IWDSUB(mxlev), WSSUB(mxlev), IWSSUB(mxlev),
c            NLSUB,NLORI,NLSUB,NTZSUB,iyrsub,imosub,idysub,julsub,
c            ihrsub
c
c
c --- RDSUB called by:  NEXTSUB
c --- RDSUB calls:      JULDAY, YR4
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.r62'
      include 'control.r62'
      include 'submet.r62'

      ierr=0

c --- Read data header
      read(io18,3,end=999) iyr,imo,idy,ihr,nlori,nlsub
3     format(20x,i4,3i2,2x,i5,T66,i5)

      call YR4(io6,iyr,ierr)
      if(ierr.NE.0) stop 'Halted in RDSUB'

      call JULDAY(io6,iyr,imo,idy,ijul)

c --- Pass date-time to /SUBMET/
      iyrsub=iyr
      imosub=imo
      idysub=idy
      julsub=ijul
      ihrsub=ihr
      ntzsub = iyr*100000 + ijul*100 + ihr

c --- Error checks
      if(nlsub.lt.1)then
         write(io6,81) iyr,imo,idy,ihr
81       format(//1x,'ERROR IN SUBR. RDSUB -- no upper air data ',
     1   'for substitution '//1x,'(year,month,day) = (',
     2   i4,',',i2,',',i2,')',2x,'hour = ',i2,' (GMT)')
         stop
      endif

c --- Check that no. levels does not exceed array dimension
      if(nlsub.gt.mxlev)then
         write(io6,86)iyr,imo,idy,ihr,nlsub,mxlev
86       format(//1x,'ERROR IN SUBR. RDSUB -- too many levels for ',
     1   'substitution array dimension'//1x,
     2   '(year,month,day) = (',i4,',',i2,',',i2,')',2x,'hour = ',i2,
     3   ' GMT'//1x,'No. levels = ',i5,3x,'Current array dimension = ',
     4   i5)
         stop
      endif
c
c --- Read data records
      if(isub.eq.1)then
c ---    Original slash-delimited format
         read(io18,4)(psub(ii),zlsub(ii),tzsub(ii),
     1   iwdsub(ii),iwssub(ii),ii=1,nlsub)
4        format(4(3x,f6.1,1x,f5.0,1x,f5.1,1x,i3,1x,i3))
      elseif(isub.eq.2)then
c ---    Comma-delimited data format
         read(io18,*)(psub(ii),zlsub(ii),tzsub(ii),
     1   iwdsub(ii),wssub(ii),ii=1,nlsub)
      else
         write(io6,*)'ERROR in SUBR. RDSUB - Invalid format type - ',
     1   'ISUB = ',isub
         stop
      endif

c --- Set real/integer versions of speed
      if(isub.EQ.1) then
         do ii=1,nlsub
            wssub(ii)=iwssub(ii)
            if(iwssub(ii).EQ.999) wssub(ii)=999.9
         enddo
      else
         do ii=1,nlsub
            iwssub(ii)=wssub(ii)
         enddo
      endif

      return

999   ierr=1
      write(io6,*)
      write(io6,*)
      write(io6,*)
      write(io6,*)'-------------------------------------------------'
      write(io6,*)'RDSUB: EOF reached in substitute UP.DAT file'
      write(io6,*)'-------------------------------------------------'
      write(io6,*)
      return

      end
c----------------------------------------------------------------------
      subroutine submiss(lastyr,lastjul,lasthr,nowyr,nowjul,nowhr,
     &                   more,stnid)
c----------------------------------------------------------------------
c
c --- READ62   Version: 5.54      Level: 020614                 SUBMISS
c ---          D. Strimaitis,  Earth Tech, Inc.
c
c --- PURPOSE:  Use data in the substitution UP.DAT file to fill in
c               missing soundings (does one sounding per call)
c
c --- UPDATES:
c --- V 5.1 Level 020330  ====> V 5.1 Level 020614 (D. Strimaitis)
c           - Allow station ID to be up to 8 characters
c
c --- INPUTS:
c               LASTYR - integer    - Year of last sounding (GMT)
c              LASTJUL - integer    - Julian day (GMT) of last sounding
c               LASTHR - integer    - Time (GMT) of last sounding
c                NOWYR - integer    - Year of current sounding (GMT)
c               NOWJUL - integer    - Julian day (GMT) of current
c                                     sounding
c                NOWHR - integer    - Time (GMT) of current sounding
c                STNID - character*8- Station ID
c
c        Common block /submet/ variables
c            (all)
c
c        Parameters: IO6
c
c --- OUTPUT:
c               LASTYR - integer    - Year of last sounding (GMT)
c              LASTJUL - integer    - Julian day (GMT) of last sounding
c               LASTHR - integer    - Time (GMT) of last sounding
c                 MORE - integer    - Processing flag
c                                       0: processing complete
c                                       1: get next substitute sounding
c                                          and call again
c
c --- SUBMISS called by:  COMP
c --- SUBMISS calls:      DELTT
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.r62'
      include 'control.r62'
      include 'submet.r62'

      character*8 stnid
      character*1 ccomma
      data ccomma/','/

c --- Set last and current date-time
      last=lastyr*100000+lastjul*100+lasthr
      now=nowyr*100000+nowjul*100+nowhr

      if(ntzsub.LE.last) then
c ---    Get next substitute sounding when current substitution time is
c ---    older than last good sounding
         more=1
      elseif(ntzsub.GE.now) then
c ---    No substitution is possible when current substitution time is
c ---    newer than current sounding
         more=0
      elseif(ntzsub.LT.now) then
c ---    Substitute sounding is between last and current soundings
         call DELTT(lastyr,lastjul,lasthr,iyrsub,julsub,ihrsub,idelhr)
         if(idelhr.LE.6) then
c ---       Substitution time is too near last, so get the next one
            more=1
         else
c ---       Use substitute sounding
c ---       Report message if any soundings are still missing
            if(idelhr.GT.18) then
               WRITE(io6,6080)
               WRITE(io9,6080)
6080           FORMAT(1X,'->->->Missing sounding(s)')
            endif
c ---       Data header and list file log record
            WRITE(io6,6060)iyrsub,imosub,idysub,julsub,ihrsub,nlsub
            write(io6,*)' ->->->Data SUBSTITUTED from alternate file'
6060        FORMAT(7X,I4,4X,I2,6X,I2,7X,I3,9X,I2,14X,I5)
            WRITE(io9,6200) STNID,iyrsub,imosub,idysub,ihrsub,nlori,
     &                      nlsub
6200        FORMAT(3X,'6201',2X,A8,3X,i4,3I2,2X,I5,T66,I5)
c ---       Data record
            if(ifmt.EQ.1)then
c ---         Write a slash-delimited file (original format)
              WRITE(io9,6210) (psub(I),zlsub(I),tzsub(I),iwdsub(I),
     1                         iwssub(I),I=1,nlsub)
6210          FORMAT(4(3X,F6.1,'/',F5.0,'/',F5.1,'/',I3,'/',I3))
            else
c ---         Write a comma-delimited file
              nlsubm1=nlsub-1
              WRITE(io9,6211) (psub(I),zlsub(I),tzsub(I),iwdsub(I),
     1                         wssub(I),ccomma,I=1,nlsubm1),
     2         psub(nlsub),zlsub(nlsub),tzsub(nlsub),iwdsub(nlsub),
     3         wssub(nlsub)
6211          FORMAT(4(3X,F6.1,',',F5.0,',',F5.1,',',I3,',',f5.1,a1))
            endif
c ---       Update LAST sounding time
            lastyr=iyrsub
            lastjul=julsub
            lasthr=ihrsub
            more=1
         endif
      endif         

      return
      end
c----------------------------------------------------------------------
      subroutine subrep(nowyr,nowjul,nowhr,more,ireplace,stnid)
c----------------------------------------------------------------------
c
c --- READ62   Version: 5.54      Level: 020614                  SUBREP
c ---          D. Strimaitis,  Earth Tech, Inc.
c
c --- PURPOSE:  Use data in the substitution UP.DAT file to replace
c               a bad sounding
c
c --- UPDATES:
c --- V 5.1 Level 020330  ====> V 5.1 Level 020614 (D. Strimaitis)
c           - Allow station ID to be up to 8 characters
c
c --- INPUTS:
c                NOWYR - integer    - Year of current sounding (GMT)
c               NOWJUL - integer    - Julian day (GMT) of current
c                                     sounding
c                NOWHR - integer    - Time (GMT) of current sounding
c                STNID - character*8- Station ID
c
c        Common block /submet/ variables
c            (all)
c
c        Parameters: IO6
c
c --- OUTPUT:
c                NOWYR - integer    - Year of current sounding (GMT)
c               NOWJUL - integer    - Julian day (GMT) of current
c                                     sounding
c                NOWHR - integer    - Time (GMT) of current sounding
c                 MORE - integer    - Next sounding needed flag
c                                       0: retain current sounding
c                                       1: get next substitute sounding
c             IREPLACE - integer    - Processing result
c                                       0: sounding NOT replaced
c                                       1: sounding replaced
c
c --- SUBREP called by:  COMP
c --- SUBREP calls:      DELTT
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.r62'
      include 'control.r62'
      include 'submet.r62'

      character*8 stnid
      character*1 ccomma
      data ccomma/','/

c --- Check sounding times 
      call DELTT(nowyr,nowjul,nowhr,iyrsub,julsub,ihrsub,idelhr)

      if(idelhr.LT.-6) then
c ---    Get next substitute sounding when current substitution time is
c ---    significantly older than current sounding
         more=1
         ireplace=0
      elseif(idelhr.GT.6) then
c ---    No substitution is possible when current substitution time is
c ---    significantly newer than current sounding
         more=0
         ireplace=0
      else
c ---    Use substitute sounding
c ---    Data header and list file log record
         WRITE(io6,6060)iyrsub,imosub,idysub,julsub,ihrsub,nlsub
         write(io6,*)' ->->->Data SUBSTITUTED from alternate file'
6060     FORMAT(7X,I4,4X,I2,6X,I2,7X,I3,9X,I2,14X,I5)
         WRITE(io9,6200) STNID,iyrsub,imosub,idysub,ihrsub,nlori,
     &                   nlsub
6200     FORMAT(3X,'6201',2X,A8,3X,i4,3I2,2X,I5,T66,I5)
c ---    Data record
         if(ifmt.EQ.1)then
c ---      Write a slash-delimited file (original format)
           WRITE(io9,6210) (psub(I),zlsub(I),tzsub(I),iwdsub(I),
     1                      iwssub(I),I=1,nlsub)
6210       FORMAT(4(3X,F6.1,'/',F5.0,'/',F5.1,'/',I3,'/',I3))
         else
c ---      Write a comma-delimited file
           nlsubm1=nlsub-1
           WRITE(io9,6211) (psub(I),zlsub(I),tzsub(I),iwdsub(I),
     1                      wssub(I),ccomma,I=1,nlsubm1),
     2      psub(nlsub),zlsub(nlsub),tzsub(nlsub),iwdsub(nlsub),
     3      wssub(nlsub)
6211       FORMAT(4(3X,F6.1,',',F5.0,',',F5.1,',',I3,',',f5.1,a1))
         endif
c ---    Update sounding time
         nowyr=iyrsub
         nowjul=julsub
         nowhr=ihrsub
         more=1
         ireplace=1
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine RDSTA
c----------------------------------------------------------------------
c
c --- READ62     Version: 5.54     Level: 030402                  RDSTA
c ---            D. Strimaitis, Earth Tech, Inc.
c
c --- Read station information from the top of the upper-air data file
c
c --- UPDATES:
c --- V 5.2 Level 020805  ====> V 5.5 Level 030402 (D. Strimaitis)
c           - Always use WMO ID in FSL file, not the WBAN ID
c           - Fix N,S,E,W logic for WEB FSL format
c           - Add list-file output of Station ID
c --- V 5.1 Level 020614  ====> V 5.2 Level 020805 (D. Strimaitis)
c           - Change FSL header format for new WEB structure
c --- V 5.1 Level 020330  ====> V 5.1 Level 020614 (D. Strimaitis)
c           - Station ID as character variable
c
c --- INPUTS:
c
c     Common block /CONTROL/
c        JDAT
c
c --- Parameters used:
c        IO6, IO8
c
c --- OUTPUT:
c
c     Common block /STATION/
c        ielevm,stnid,cname,clat,clon
c        
c
c --- RDSTA called by:  SETUP
c --- RDSTA calls:      none
c----------------------------------------------------------------------
c
      include 'params.r62'
      include 'control.r62'
      include 'station.r62'

      character*1 cNS,cEW
      character*12 clat12,clon12

      if(jdat.EQ.1) then
c ---    TD-6201 format file
c ---    Read part of first record for info
         read(io8,10) stnid,ilat,cNS,ilon,cEW,ielevm
10       format(a8,i4,a1,i5,a1,23x,i6)
c ---    Convert latitude to degrees
         ideg=ilat/100
         imin=ilat-100*ideg
         xlat=ideg+imin/60.0
c ---    Convert longitude to degrees
         ideg=ilon/100
         imin=ilon-100*ideg
         xlon=ideg+imin/60.0
         write(io6,*)
         write(io6,*)'TD-6201 Station ID:      ',stnid
         write(io6,*)

      elseif(jdat.EQ.2) then
c ---    NCDC FSL format file
c ---    Header #1
         READ(io8,'(i7)') itype
c ---    Header #2
c ---    CD format is (3i7,2F7.2,2i7),
c ---    WEB format is now (3i7,F7.2,a1,f6.2,a1,i6,i7), where the new A1
c ---    variables are N,S,E, or W for the lat/lon
c ---    Use the new format for both, and interpret the N,S,E,W as
c ---    N or W if missing (CD convention is N Lat and W Lon)
         READ(io8,'(3i7,F7.2,a1,f6.2,a1,i6)') itype,iwban,iwmo,
     &                                        xlat,cNS,xlon,cEW,ielevm
c ---    Set lat/lon convention
         if(cNS.EQ.'s') cNS='S'
         if(cNS.NE.'S') cNS='N'
         if(cEW.EQ.'e') cEW='E'
         if(cEW.NE.'E') cEW='W'
c ---    Header #3
         READ(io8,'(i7)') itype
c ---    Header #4
         READ(io8,'(i7,10x,a4)') itype,cname
         if(iwban.NE.99999) then
c ---       Use WBAN ID
            write(stnid,'(i8)') iwban
         else
c ---       Use WMO ID
            write(stnid,'(i8)') iwmo
         endif
         write(io6,*)
         write(io6,*)'FSL Station ID used:      ',stnid
         write(io6,*)'            WBAN ID:      ',iwban
         write(io6,*)'             WMO ID:      ',iwmo
         write(io6,*)

      else
         write(io6,*)
         write(io6,*)'RDSTA:  FATAL ERROR!'
         write(io6,*)' Invalid value of JDAT (Upper-Air File format)'
         write(io6,*)' Expected JDAT = 1 or 2'
         write(io6,*)' Found    JDAT = ',jdat

         write(*,*)
         write(*,*)'RDSTA:  FATAL ERROR!'
         write(*,*)' Invalid value of JDAT (Upper-Air File format)'
         write(*,*)' Expected JDAT = 1 or 2'
         write(*,*)' Found    JDAT = ',jdat

         stop
      endif

c --- Form lat/lon character variables
      write(clat12,'(f12.7)') xlat
      clat(1:12)=clat12
      clat(13:13)=cNS
      write(clon12,'(f12.7)') xlon
      clon(1:12)=clon12
      clon(14:14)=cEW

c --- Reset position to start of data file
      REWIND(io8)

      return
      end
c----------------------------------------------------------------------
      subroutine fin
c----------------------------------------------------------------------
c
c --- READ62   Version: 5.54            Level: 011003               FIN
c ---          J. Scire, Earth Tech
c
c --- PURPOSE:  Run termination routine -- compute runtime
c
c --- INPUTS:
c       Common block /QA/
c          rdate, rtime, rcpu
c       Parameters: IO6, IOMESG
c
c --- OUTPUT:  none
c
c --- FIN called by:  MAIN
c --- FIN calls:      DATETM, JULDAY, DELTT
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.r62'
      include 'qa.r62'
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
         call julday(io6,iyr1,imo1,iday1,ijul1)
c
         read(rdate2(1:2),10)imo2
         read(rdate2(4:5),10)iday2
         read(rdate2(7:10),'(i4)')iyr2
         call julday(io6,iyr2,imo2,iday2,ijul2)
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
      write(io6,1402)rtime2,rdate2,delt,rcpu
1402  format(//2x,'End of run -- Clock time: ',a8/
     1         2x,'                    Date: ',a10//
     2         2x,'      Elapsed Clock Time: ',f10.1,' (seconds)'//
     3         2x,'                CPU Time: ',f10.1,' (seconds)')

c
      return
      end
