c----------------------------------------------------------------------
c --- PMERGE --Precipitation Data Preprocessor
c----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 070627                    MAIN
c
c --- Written by:  E. Insley, J. Scire
c 
c     Copyright (c) 1998-2007 by Exponent, Inc.
c
c----------------------------------------------------------------------
c --- Reads 'N' TD3240 precipitation files containing data for one
c --- station each and reformats the data into one binary file that
c --- contains 'N' stations sorted by time.  A formatted TD3240 file
c --- may also be combined with an already existing PMERGE (binary)
c --- file.  Accumulation periods are resolved and bad or suspicious
c --- data are eliminated.  Output files may be in the packed or
c --- unpacked format.
c----------------------------------------------------------------------
c
c --- UPDATES:
c --- Updated V5.32(070627) from V5.31(030528) (D. Strimaitis)
c     - Updated CALUTILS (Version 2.55, Level 070327)
c         Modify search for '=' in READIN to allow
c           for blanks between c*12 variable name and the '='
c           sign (internal blanks are not removed after V2.2)
c         Replace filename strings c*70 with c*132
c         Allow for spaces within pathnames by adding new TLEFT
c           and TRIGHT trim subroutines
c     - Filnames changed from c*70 to c*132 (CALUTILS V2.3 and later)
c       Modified:  FILNAM.PMG
c                  READCF
c
c --- Updated V5.31(030528) from V5.3(030402) (D. Strimaitis)
c     - Updated CALUTILS (Version 2.2, Level 030528)
c
c --- Updated V5.3(030402) from V5.2(020828) (D. Strimaitis)
c     - Updated CALUTILS (Version 2.1, Level 030402)
c     - New header for output data file (PRECIP.DAT)
c     - User selection of previous PRECIP.DAT format
c     - Replace NBSTN=-999 convention with NBSTN<=0 convention
c       to indicate that ALL stations in a previous PRECIP.DAT
c       file are used
c     - Update defaults in BLOCK DATA
c     - Report accumulation period to list file
c     - FIN:  list file unit number added to JULDAY
c     - Report an error if the first precipitation flag in the data
c       file is a closed end-period bracket such as } or ]
c       (J. Popovic)
c
c --- Updated V5.2(020828) from V5.1(020722) (D. Strimaitis)
c     - Updated CALUTILS (Version 1.1, Level 020828)
c     - Updated FIN to use the full YYYY system year (RDATE2)
c
c --- Updated V5.1(020722) from 5.0(011003) (D. Strimaitis)
c     - Fix hour array dimension (24 --> 25) for input record in GETPREC
c       so that full day of precip (plus total) can be read
c     - Reset max number of stations to 300
c
c --- Updated V5.0(011003) from V4.0(010608) (D. Strimaitis)
c     - Restructure inputs for CALPUFF system control file
c     - Restructure main program as subroutine COMP
c     - Place parameters and commons into include files
c     - Place system-wide utilities into an include module (calutil.for)
c
c --- Updated V4.0(010608) from V4.0(010315) (D. Strimaitis)
c     - Allow variable record-length precip data files
c     - Interpret past and current hourly data flags (FLAG1)
c        A/A  ,   D/D   M/M   E              (past)
c        a/A  ,   {/}   [/]   E    g    T    (current)
c       Daily flags are not interpreted
c     - Interpret some current data quality flags (FLAG2)
c        Q (value treated as bad)  q (value accepted)
c
c --- Updated V4.0(010315) from V4.0(981025) (D. Strimaitis)
c     - Y2K treatment of year enforced
c
c --- Updated V4.0(981025) from V4.0(961113) (D. Strimaitis)
c     - Convert time-mark used in output file from a (01-24)
c       time-ending to a (00-23) time-ending convention
c 
c --- INPUTS:
c         NPF - integer    - Number of formatted TD3240 precipitation
c                            input files (stations)
c       MAXAP - integer    - Maximum accumulation period length (hrs)
c        IOTZ - integer    - Requested time zone of data for binary
c                            output file
c      IOFORM - integer    - Output format(1 = binary, 2 = formatted
c                            MESOPAC input format)
c      IOPACK - integer    - Data packing code for output file
c                            (0 = unpacked, 1 = packed)
c      OUTFIL - char*70    - Output data filename
c     PREVFIL - char*70    - Previous PMERGE run output data filename
c     CFFILES - char*70    - Array of names of formatted TD3240 precip
c               array        files 'stn#.dat' (USED ONLY IF NPF > 0)
c       IFSTN - int. array - Array of station ids corresponding to each
c                            formatted TD3240 precipitation input file
c        ISTZ - int. array - Array of time zone corresponding to each
c                            formatted TD3240 precipitation input file
c       NBSTN - integer    - Number of stations to extract from the
c                            previous input file (.LE.0 = all stations
c                            in file) (USED ONLY IF LPREV = Y)
c       IBSTN - int. array - Array of 6-digit station numbers of the
c                            previous stations to extract (USED ONLY IF:
c                            LPREV=T, NBSTN .GT. 0)
c        IBYR - integer    - Beginning year of period to extract
c        IBMO - integer    - Beginning month of period to extract
c       IBDAY - integer    - Beginning day of period to extract
c        IBHR - integer    - Beginning hour of period to extract
c        IEYR - integer    - Ending year of period to extract
c        IEMO - integer    - Ending month of period to extract
c       IEDAY - integer    - Ending day of period to extract
c        IEHR - integer    - Ending hour of period to extract
c
c --- OUTPUT FILES:
c
c        PMERGE.LST     -  summary table of data selected
c        User specified - binary or formatted file of precip data for a
c         file name       number of stations for a given time period
c
c --- DATE VARIABLES:
c     IBDATHR - integer     - Beginning date of requested time period
c                             in output time zone YYYYJJJHH
c     IEDATHR - integer     - Ending date of requested time period in
c                             output time zone YYYYJJJHH
c        KBTZ - integer     - Time zone of binary input data
c       NDATE - integer     - Time currently wanted in output time zone
c                             YYYYJJJHH
c       KDATE - integer     - Time currently wanted in binary time zone
c                             YYYYJJJHH
c       JDATE - integer     - Time currently wanted in output time zone
c                             for a particular formatted station
c                             YYYYJJJHH
c     KBDATHR - integer     - Beginning date of available binary data
c                             in output time zone YYYYJJJHH
c     KEDATHR - integer     - Ending date of available binary data
c                             in output time zone YYYYJJJHH
c    IKBDATHR - integer     - Beginning date of available binary data
c                             in binary input time zone YYYYJJJHH
c    IKEDATHR - integer     - Ending date of available binary data
c                             YYYYJJJHH
c      IDATHR - integer     - Time of record actually read in binary
c                             time zone YYYYJJJHH
c
c -----------------------------------------------------------------------------
      Program PMERGE
c
c --- Include parameters
      include 'params.pmg'
c --- Include common blocks
      include 'qa.pmg'
c
c --- Set version and level number of program
      ver='5.32'
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
c --- PMERGE     Version: 5.32     Level: 030402             BLOCK DATA
c                D. Strimaitis, Earth Tech, Inc.
c
c --- Include parameter statements
      include 'params.pmg'
c
c --- Include common blocks
      include 'control.pmg'
      include 'filnam.pmg'

c --- CONTROL common block
      data maxap/6/, nbstn/0/, ioform/2/, iopack/0/, ipform/2/

c --- FILNAM common block
      data runinp/'pmerge.inp'/,runlst/'pmerge.lst'/,
     1 prevdat/'prev.dat'/,precdat/'precip.dat'/,
     2 stnfil/'none.dat'/
c --- FILLOG common block
      data lcfiles/.true./

      end

c----------------------------------------------------------------------
c --- BRING IN CALPUFF SYSTEM UTILITY SUBROUTINES
      include 'calutils.for'
c----------------------------------------------------------------------

c----------------------------------------------------------------------
      subroutine setup
c----------------------------------------------------------------------
c
c --- PMERGE     Version: 5.32     Level: 011003                  SETUP
c                D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:   perform all initialization and one-time setup
c                operations
c
c     Common block /FILNAM/
c        RUNINP
c
c --- Parameters used:
c        IO5, IO6, IOMESG
c
c --- SETUP called by: MAIN
c --- SETUP calls:     DATETM, COMLINE, READCF
c                      
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'

c --- Include common blocks
      include 'filnam.pmg'
      include 'qa.pmg'

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
10       format(/1x,'ERROR in SUBR. SETUP -- The PMERGE version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open other files)
      call READCF

      return
      end
c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- PMERGE     Version: 5.32     Level: 070627                 READCF
c                D. Strimaitis
c
c --- PURPOSE:  Read the file containing the file names of the
c               input and output files of the run, and associated
c               control variables, place into program variables,
c               and QA information.
c
c --- UPDATES:
c     V5.32(070627) from V5.3(030402) (D. Strimaitis)
c     - Filenames from c*70 to c*132 for CALUTILS V2.3 and later
c     V5.3(030402) from V5.2(011003) (D. Strimaitis)
c     - Previous PRECIP.DAT file either binary or ASCII
c     - Report max accumulation period to list file
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c
c        Parameters: IO5, IO6, IOMESG, MXVAR
c                    MXPF, MXPS
c
c --- OUTPUT:
c
c ---    Common block /DATEHR/ variables:
c           ibdathr,iedathr
c ---    Common block /FILNAM/ variables:
c           prevdat,precdat,runlst,stnfil,cffiles,lcfiles
c ---    Common block /CONTROL/ variables:
c           iotz,ioform,iopack,ipform,
c           npf,istz(mxpf),ifstn(mxpf),
c           ibstn(mxps),nbstn,lprev,
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE, JULDAY
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.pmg'
      include 'params.cal'
c
c --- Include common blocks
      include 'control.pmg'
      include 'datehr.pmg'
      include 'filnam.pmg'
      include 'qa.pmg'
c
c --- Local variables
      real qastz(mxpf)
      character*4 ctemp(132,4)
      character*12 cvdic(mxvar,4)
      integer ivleng(mxvar,4),ivtype(mxvar,4)
      logical lecho
      logical lerrcf

c --- Set control file error logical
      lerrcf=.FALSE.

c --- Set Dictionary
      data lecho/.false./
      data names/3/

      data cvdic/
     a  'PREVDAT','PRECDAT','RUNLST','LCFILES','NPF', 55*' ',
     b  'STNFIL','IFSTN','XSTZ', 57*' ',
     c  'IBYR','IBMO','IBDY','IBHR','IEYR','IEMO','IEDY','IEHR','XBTZ',
     c  'LPREV','NBSTN','IOFORM','IOPACK','IPFORM','MAXAP', 45* ' ',
     d  'IBSTN', 59*' '/

      data ivleng/
     a  3*132,2*1, 55*0,
     b  132,2*1, 57*0,
     c  15*1, 45*0,
     d  1, 59*0/

c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
      data ivtype/
     a  3*4,3,2, 55*0,
     b  4,2,1, 57*0,
     c  8*2,1,3,5*2, 45*0,
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
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),lcfiles,npf,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum)

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')prevdat=' '
      if(ctemp(1,2)(1:1).ne.' ')precdat=' '
      if(ctemp(1,3)(1:1).ne.' ')runlst=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')prevdat(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')precdat(j:j)=ctemp(j,2)(1:1)
         if(ctemp(j,3)(1:1).ne.' ')runlst(j:j)=ctemp(j,3)(1:1)
      enddo

c --- Convert the file names to the proper case
      call FILCASE(lcfiles,prevdat)
      call FILCASE(lcfiles,precdat)
      call FILCASE(lcfiles,runlst)

c --- Open listfile
      open(io6,file=runlst,status='unknown')

c --- Write banner to list file
      write(io6,5) ver,level
5     format(///,26x,'PMERGE OUTPUT SUMMARY',/,19x,'VERSION:  ',A8,
     1       ' LEVEL:  ',A8///)

c ------------------
c --- Input Group 0b
c ------------------

      do k=1,npf
c ---    Initialize the temporary arrays for the file name
         do j=1,132
            ctemp(j,1)(1:1)=' '
            stnfil(j:j)=' '
         enddo

c ---    Read the surface met station information
       call READIN(cvdic(1,2),ivleng(1,2),ivtype(1,2),io5,io6,lecho,
     1 ctemp(1,1),ifstnin,xstz,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum)

c ---    Transfer the char*4 data into the char*132 variable
         do j=1,132
            if(ctemp(j,1)(1:1).ne.' ')stnfil(j:j)=ctemp(j,1)(1:1)
         enddo

c ---    Convert the file name to the proper case
         call FILCASE(lcfiles,stnfil)

c ---    Place information in surface station arrays, up to MXPF
         if(k.LE.MXPF) then
            cffiles(k)=stnfil
            ifstn(k)=ifstnin
            istz(k)=NINT(xstz)
            qastz(k)=xstz
         endif

      enddo

c -----------------
c --- Input Group 1
c -----------------

      call readin(cvdic(1,3),ivleng(1,3),ivtype(1,3),io5,io6,lecho,
     1 IBYR,IBMO,IBDY,IBHR,IEYR,IEMO,IEDY,IEHR,XBTZ,
     2 LPREV,NBSTN,IOFORM,IOPACK,IPFORM,MAXAP,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum)

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
         kk=MIN(k,mxps)
         call readin(cvdic(1,4),ivleng(1,4),ivtype(1,4),io5,io6,lecho,
     1 IBSTN(kk),
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
      enddo

c -------------------------------------------------
c --- Translate selected inputs to PMERGE variables
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

c --- Test for valid NPF
      if(npf.GT.mxpf) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 0a'
         write(io6,*) 'NPF exceeds the parameter MXPF '
         write(io6,*) 'NPF, MXPF = ',npf,mxpf
         write(io6,*) 'Increase MXPF in PARAMS.PMG and recompile'
         lerrcf=.TRUE.
      endif

c --- Test for integer time zone (code for half-zones not available)
      kff=MIN(npf,mxpf)
      do k=1,kff
         test=istz(k)-qastz(k)
         if(ABS(test).GE.0.1) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 0b'
            write(io6,*) 'Fractional time zone found: XSTZ= ',qastz(k)
            write(io6,*) 'PMERGE is designed to use integer time zones'
            lerrcf=.TRUE.
         endif
         if(istz(k).LT.-12 .OR. istz(k).GT.12) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 0b'
            write(io6,*) 'Invalid station time zone = ',istz(k)
            write(io6,*) ' (-12 <= ISTZ <= +12) '
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
         write(io6,*) 'PMERGE is designed to use integer time zones'
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
c --- Packing flag of output PRECIP.DAT file (0=not packed,1=packed)
      if(iopack.NE.0 .AND. iopack.NE.1) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'Invalid value of IOPACK = ',iopack
         write(io6,*) 'IOPACK must be 0 or 1'
         lerrcf=.TRUE.
      endif

c --- Test input data format
      if(ipform.NE.1 .AND. ipform.NE.2) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'Invalid value of IPFORM = ',ipform
         write(io6,*) 'IPFORM must be 1 or 2'
         lerrcf=.TRUE.
      endif

c --- Test for valid MAXAP
      if(maxap.LT.0) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'MAXAP out of range      = ',maxap
         write(io6,*) 'MAXAP should be positive'
         lerrcf=.TRUE.
      endif

c --- Test for valid NBSTN
      if(nbstn.GT.mxps) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'NBSTN exceeds the parameter MXPS '
         write(io6,*) 'NBSTN, MXPS = ',nbstn,mxps
         write(io6,*) 'Increase MXPS in PARAMS.PMG and recompile'
         lerrcf=.TRUE.
      endif

c --- STOP now if error exists in the control file
      if(LERRCF) then
         write(*,*)'ERRORS are found in the CONTROL file'
         write(*,*)'Review messages written to the LIST file'
         stop
      endif

c -----------------------------------------------------------
c --- Echo inputs to list file as in previous PMERGE versions
c -----------------------------------------------------------

c --- Echo filenames
      write(io6,*)
      write(io6,10) 'Control file name     : ',runinp
      write(io6,10) 'Output list file name : ',runlst
      write(io6,10) 'Output file name      : ',precdat
      write(io6,*)  'Continuation Run?     : ',lprev
      if(LPREV)
     1  write(io6,10) 'Previous PMERGE output: ', prevdat
10    format(a24,a132)

      write(io6,*)
      write(io6,20)
20      format(//,1x,'Time Zone ',5x,'Station ID',5x,
     1         'Formatted TD3240 Precipitation',/39x,'Input Files ',/)

      do i=1,npf
         write(io6,'(4x,i2,11x,i6,7x,a)') istz(i),ifstn(i),cffiles(i)
      enddo

c --- Echo processing period
      write(io6,80) iotz,ibmo,ibdy,ibyr,ibhr,iemo,iedy,ieyr,iehr
80    format(//,1x,'Period to Extract (in time zone',i3,'):   ',i2,'/',
     1    i2,'/',i4,2x,i2,':00','  to  ',i2,'/',i2,'/',i4,2x,i2,':00'/)

c --- Echo maximum accumulation period
      write(io6,90) maxap
90    format(//,1x,'Maximum Accumulation Period (hours):   ',i4,/)


c ------------------------
c --- Open remaining files
c ------------------------
      call OPENIT

      return
      end
c----------------------------------------------------------------------
      subroutine comp
c----------------------------------------------------------------------
c
c --- PMERGE     Version: 5.32     Level: 030402                   COMP
c                E. Insley, J. Scire,   Earth Tech, Inc.
c
c --- PURPOSE:  Process the precipitation station data to produce a
c               PRECIP.DAT file for CALMET
c
c --- Updated V5.3(030402) from V5.2(011003) (D. Strimaitis)
c     - Previous PRECIP.DAT either binary or ASCII
c
c --- INPUTS:
c
c        Parameters: IO6, IOMESG, MXPF, MXPS
c
c --- OUTPUT:
c           none
c
c --- COMP called by:  MAIN
c --- COMP calls:      GRDAY, DEDAT,
c                      RDWRIT
c ------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.pmg'

c --- Include common blocks
      include 'control.pmg'
      include 'datehr.pmg'

      integer ishift(3),isuminv(mxpf),isumtot(mxpf)
      character*6 cstn(mxps),cbstn(mxps)

c --- Report progress
      write(iomesg,*)'COMPUTATIONAL PHASE'

c --- Read data and write to output file
      call RDWRIT

C  WRITE OUT INPUT FILE HEADER INFORMATION
      if(LPREV)then
        write(io6,90) kbtz, ipform, ippack
90      format(1x,'Data from Previous Output File:'//,3x,'Time Zone:',
     1         i6,/,3x,'Format Code:',i4,/,3x,'Packing Code:',i3)
        call dedat(kbdathr,jbyr,jbday,jbhr)
        call dedat(kedathr,jeyr,jeday,jehr)
        call grday(io6,jbyr,jbday,kbmo,kbday)
        call grday(io6,jeyr,jeday,kemo,keday)
        if(iotz.EQ.kbtz)then
          write(io6,100) iotz,kbmo,kbday,jbyr,jbhr,kemo,keday,jeyr,jehr
100       format(/,3x,'Period (in time zone',i2,'):   ',i2,'/',i2,'/',
     1           i4,2x,i2,':00','  to  ',i2,'/',i2,'/',i4,2x,i2,':00')
        else
          call dedat(ikbdathr,ijbyr,ijbday,ijbhr)
          call dedat(ikedathr,ijeyr,ijeday,ijehr)
          call grday(io6,ijbyr,ijbday,ikbmo,ikbday)
          call grday(io6,ijeyr,ijeday,ikemo,ikeday)
          write(io6,100) kbtz,ikbmo,ikbday,ijbyr,ijbhr,ikemo,ikeday,
     1                   ijeyr,ijehr
          write(io6,100) iotz,kbmo,kbday,jbyr,jbhr,kemo,keday,jeyr,jehr
        endif
C       CONVERT INTEGER STATION IDS IN INPUT FILE TO CHARACTER*6
        do 45 j= 1,nstnb
          write(cbstn(j),'(i6)') idbstn(j)
          do 55 i=1,6
            if(cbstn(j)(i:i).EQ.' ')then
              cbstn(j)(i:i) = '0'
              if(i.NE.1)then
                write(io6,110) idbstn(j)
110             format(2x,'Incorrect station id in previous file: ',
     1                 i6)
                go to 99
              endif
            endif
55        continue
45      continue

C       WRITE THE AVAILABLE STATION NUMBERS IN THE INPUT FILE
C       SET VARIABLES FOR WRITING OUT IN 4 COLUMNS
C        J4 IS NO. ROWS IN A "SHORT" COLUMN
C        J5 IS NO. ROWS IN A "LONG" COLUMN
C        J6 IS THE NUMBER OF "LONG" COLUMNS
        j4 = nstnb/4
        j6 = mod(nstnb,4)
        if(j6.EQ.0)then
          j5 = j4
        else
          j5 = j4 + 1
        endif
        ishift(1) = j5
        do 65 i=2,3
          if(i.LE.j6)then
            ishift(i) = ishift(i-1) + j5
          else
            ishift(i) = ishift(i-1) + j4
          endif
65      continue
        ncol = min0(nstnb,4)
        write(io6,120) (' ',k=1,ncol)
120     format( /,3x,'Stations Available in Previous Output File:  '/
     1         3x,4(a1,'No.',4x,'ID',11x)/)
        do 75 i=1,j4
          i2 = i + ishift(1)
          i3 = i + ishift(2)
          i4 = i + ishift(3)
          write(io6,130) i,cbstn(i),i2,cbstn(i2),i3,cbstn(i3),i4,
     1                   cbstn(i4)
130       format(3x,4(i3,3x,a6,9x))
75      continue
        if(j6.GT.0)then
          n1 = j5
          n2 = n1+(j6-1)*j5
          write(io6,130) (k,cbstn(k),k=n1,n2,j5)
        endif
      endif
      if(LPREV) write(io6,81)
81    format(//,26x,'********************',//)

C  WRITE OUT SUMMARY INFORMATION ABOUT OUTPUT FILE
C     CONVERT INTEGER STATION IDS TO CHARACTER*6
      do 85 j= 1,ntstn
        write(cstn(j),'(i6)') idstn(j)
        do 95 i=1,6
          if(cstn(j)(i:i).EQ.' ')then
            cstn(j)(i:i) = '0'
            if(i.NE.1)then
              write(io6,140) idstn(j)
140           format(2x,'Incorrect station id input: ',i6)
              go to 99
            endif
          endif
95      continue
85    continue

C     WRITE TO LIST FILE THE STATION NUMBERS IN THE BINARY OUTPUT FILE
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
      do 105 i=2,3
        if(i.LE.j6)then
          ishift(i) = ishift(i-1) + j5
        else
          ishift(i) = ishift(i-1) + j4
        endif
105   continue
      ncol = min0(ntstn,4)
      write(io6,150) (' ',k=1,ncol)
c150   format( /,1x,'PMERGE Stations in Binary Output File:  '/
150   format( /,1x,'PMERGE Stations in Output File:  '/
     1       3x,4(a1,'No.',4x,'ID',11x)/)
      do 115 i=1,j4
        i2 = i + ishift(1)
        i3 = i + ishift(2)
        i4 = i + ishift(3)
        write(io6,160) i,cstn(i),i2,cstn(i2),i3,cstn(i3),i4,cstn(i4)
160     format(3x,4(i3,3x,a6,9x))
115   continue
      if(j6.GT.0)then
        n1 = j5
        n2 = n1+(j6-1)*j5
        write(io6,160) (k,cstn(k),k=n1,n2,j5)
      endif
C     WRITE DATA STATUS SUMMARY TO LIST FILE
      write(io6,170)
170   format(///,1x,'Summary of Data from Formatted TD3240 ',
     1       'Precipitation Files: ',///,18x,
     2       'Valid Hours:',//,1x,'Station',3x,'Zero',2x,'Nonzero',2x,
     3       'Accum',4x,'Total',5x,'%',/,3x,'IDs',20x,'Period',
     4       2(3x,'Valid'),/,35x,'Hours',3x,'Hours')
C     WRITE SUMMARY OF DATA STATUS FOR VALID HOURS
      do 125 i = 1,npf
        isumv = icount(i,1) + icount(i,2) + icount(i,3)
        isuminv(i) = icount(i,4) +icount(i,5) +icount(i,6) +icount(i,7)
        isumtot(i) = isumv + isuminv(i)
        pctv = float(isumv)/float(isumtot(i)) * 100.0
        write(io6,180)cstn(nbstn+i),icount(i,1),icount(i,2),icount(i,3),
     1                isumv,pctv
180     format(2x,a6,2x,2(i5,3x),i5,4x,i5,3x,f5.1)
125   continue
      write(io6,190)
190   format(//,18x,'Invalid Hours:',//,1x,'Station',3x,'Flagged',3x,
     1       'Excessive',2(3x,'Missing Data'),4x,'Total',7x,'%',/,3x,
     2       'IDs',5x,'Missing',5x,'Accum',5x,'Before First',4x,
     3       'After Last',4x,'Invalid',3x,'Invalid',/,23x,'Period',
     4       1x,2(3x,'Valid Record'),4x,'Hours',5x,'Hours')
C     WRITE SUMMARY OF DATA STATUS FOR INVALID HOURS
      do 135 i = 1,npf
        pctinv = float(isuminv(i))/float(isumtot(i)) * 100.0
        write(io6,200) cstn(nbstn+i),icount(i,4),icount(i,5),
     1                icount(i,6),icount(i,7),isuminv(i),pctinv
200     format(2x,a6,4x,i5,6x,i5,8x,i5,9x,i5,8x,i5,6x,f5.1)
135   continue
      write(io6,*)
      write(io6,*)
      write(io6,*)

99    return
      end
c----------------------------------------------------------------------
      subroutine openit
c----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 030402                  OPENIT
c ---          E. Insley, Earth Tech, Inc.
c
c --- Updated V5.3(030402) from V5.0(011003) (D. Strimaitis)
c     - Previous PRECIP.DAT either binary or ASCII
c --- Updated V5.0(011003) from V4.0(961113) (D. Strimaitis)
c     - Place parameters and commons into include files
c
c --- OPENIT called by:  READCF
c --- OPENIT calls:  none
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
c --- Include common blocks
      include 'control.pmg'
      include 'filnam.pmg'

C     OPEN FORMATTED INPUT FILES
      io = 7
      do i=1,npf
         open(io,file=cffiles(i),status='old')
         io = io + 1
      enddo

C     OPEN EXISTING PRECIP.DAT FILE
      if(LPREV) then
        if(ipform.EQ.1)then
          open(ioprev,file=prevdat,status='old',form='unformatted')
        elseif(ipform.EQ.2)then
          open(ioprev,file=prevdat,status='unknown')
        endif
      endif

C     OPEN OUTPUT PRECIP.DAT FILE
      if(ioform.EQ.1)then
        open(ioprec,file=precdat,status='unknown',form='unformatted')
      elseif(ioform.EQ.2)then
        open(ioprec,file=precdat,status='unknown')
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine rdwrit
c----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 030402                  RDWRIT
c ---          E. Insley, Earth Tech, Inc.
c
C     READS INPUT FILES AND FILLS THE OUTPUT ARRAYS AND WRITES TO
C     THE OUTPUT FILE
c
c --- UPDATES:
c
c --- Updated V5.3(030402) from V5.2(981025) (D. Strimaitis)
c     - Previous PRECIP.DAT either binary or ASCII
c     - Replace NBSTN=-999 with NBSTN.LE.0 as flag for using ALL
c       stations in previous file
c --- Updated 981025 from 961113 (D. Strimaitis)
c     - Convert INDECR time-mark range from  (01-24) to (00-23) 
c
c --- DATE VARIABLES:
c           IBDATHR - integer     - Beginning date of requested time period
c                                   in output time zone YYYYJJJHH
c           IEDATHR - integer     - Ending date of requested time period in
c                                   output time zone YYYYJJJHH
c              KBTZ - integer     - Time zone of binary input data
c             NDATE - integer     - Time currently wanted in output time zone
c                                   YYYYJJJHH
c             KDATE - integer     - Time currently wanted in binary time zone
c                                   YYYYJJJHH
c             JDATE - integer     - Time currently wanted in output time zone
c                                   for a particular formatted station
c                                   YYYYJJJHH
c           KBDATHR - integer     - Beginning date of available binary data
c                                   in output time zone YYYYJJJHH
c           KEDATHR - integer     - Ending date of available binary data
c                                   in output time zone YYYYJJJHH
c          IKBDATHR - integer     - Beginning date of available binary data
c                                   in binary input time zone YYYYJJJHH
c          IKEDATHR - integer     - Ending date of available binary data
c                                   YYYYJJJHH
c            IDATHR - integer     - Time of record actually read in binary time
c                                   zone YYYYJJJHH
c
c --- OTHER VARIABLES AND FLAGS:
c             NSTNB - integer     - Number of stations available in previous
c                                   input file
c             NBSTN - integer     - Number of stations to extract from 
c                                   previous input file
c            ICOUNT - 2 dim int.  - Formatted data status code count for each
c                     array         hour
c              IBIN - integer     - Flag indicating date status of binary
c                                   input file vs requested time period;
c                                   IBIN = 0  no data in binary input file for
c                                             requested time period
c                                   IBIN = 1  binary input file contains data
c                                             for requested time period
c
C     RDWRIT called by: PMERGE
C     RDWRIT calls: DEDAT
C                   DELTT
C                   RDHD
C                   WRHD
C                   RDP
C                   UNCDP
C                   WRP
C                   INDECR
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
c
c --- Include common blocks
      include 'control.pmg'
      include 'datehr.pmg'
c
      integer ixref(mxps)
      real xprecip(mxps), precip(mxps)

C     INITIALIZE ARRAY OF DATA STATUS CODES
      do 1 j = 1,7
        do 2 i = 1,mxpf
          icount(i,j) = 0
2       continue
1     continue
C     SET FLAG FOR READING BINARY DATA
      ibin = 1
C     DETERMINE THE NUMBER OF HOURS BETWEEN THE BEGINNING AND
C     ENDING DATES OF OUTPUT FILE
      call dedat(ibdathr,ibyr,ibjul,ibhr)
      call dedat(iedathr,ieyr,iejul,iehr)
      call deltt(ibyr,ibjul,ibhr,ieyr,iejul,iehr,idiff)
C     NREC IS THE NUMBER OF DATA RECORDS IN THE OUTPUT FILE
      nrec = idiff + 1
C     READ HEADER INFORMATION FOR INPUT FILE
      if(LPREV)then
        call rdhd(ipform,ioprev,kbyr,kbjul,kbhr,keyr,kejul,kehr,
     &            kbtz,nstnb,ippack,idbstn,mxps)
        ikbdathr = kbyr * 100000 + kbjul * 100 + kbhr
        ikedathr = keyr * 100000 + kejul * 100 + kehr
C     SET UP CROSS REFERENCE OF STATION IDS WITH THEIR CORRESPONDING
C     POSITION IN THE PREVIOUS OUTPUT ARRAY
        if(nbstn.LE.0)then
          nbstn = nstnb
          do 3 j = 1,nstnb
            ixref(j) = j
            ibstn(j) = idbstn(j)
3         continue
          go to 17
        endif
        do 5 i = 1,nbstn
          do 15 k = 1,nstnb
            if(ibstn(i).EQ.idbstn(k))then
              ixref(i) = k
              go to 5
            endif
15        continue
 5      continue
C     CONVERT TO IOTZ (OUTPUT) TIME ZONE
17      if(iotz.EQ.kbtz)then
          kbdathr = ikbdathr
          kedathr = ikedathr
        else
C       CONVERT DATE/HR TO OUTPUT TIME ZONE
          idtz = kbtz - iotz
          call indecr(io6,kbyr,kbjul,kbhr,idtz,0,23)
          call indecr(io6,keyr,kejul,kehr,idtz,0,23)
          kbdathr = kbyr * 100000 + kbjul * 100 + kbhr
          kedathr = keyr * 100000 + kejul * 100 + kehr
        endif
        if(iedathr.LT. kbdathr .OR. ibdathr.GT.kedathr)then
          write(io6,100)
100       format('  WARNING: The binary input file has no data for ',
     1           'the time period of this run.')
          ibin = 0
        endif
      else
        nbstn = 0
        kbtz = iotz
      endif
C    FILL STATION ID ARRAY FOR OUTPUT FILE
      ntstn = npf + nbstn
C     PREVIOUS STATIONS
      do 25 i = 1,nbstn
        idstn(i) = ibstn(i)
25    continue
C     FORMATTED STATIONS
      do 35 i = nbstn+1,ntstn
        idstn(i) = ifstn(i-nbstn)
35    continue

c --- Check for repeating station IDs
      do 360 j=1,ntstn
        icheck = idstn(j)
        icnt = 0
        do 370 k=1,ntstn
c ---     Keep count of matches (it should only match once)
          if(idstn(k).EQ.icheck) icnt = icnt + 1
370     continue
        if(icnt.GT.1)then
          write(io6,*) ' ERROR: Duplicate Station ID. ID = ',icheck
          write(*,*) ' ERROR: Invalid Input --- See Run LIST file'
          stop
        endif
360   continue

C     WRITE OUT THE HEADER TO THE PRECIPITATION OUTPUT FILE
      call wrhd(ioform,ioprec,ibyr,ibjul,ibhr,ieyr,iejul,iehr,iotz,
     1           ntstn,iopack,idstn)
C     FIRST DATE TO WRITE TO THE OUTPUT FILE
      ndate = ibyr * 100000 + ibjul * 100 + ibhr
      if(iotz.EQ.kbtz)then
        kdate = ndate
      else
C       KDATE IS TIME IN "KBTZ" TIME ZONE WHICH IS EQUIVALENT TO NDATE
C       IN "IOTZ" TIME ZONE
        idtz = iotz - kbtz
        call dedat(ndate,jyr,jjul,jhr)
        call indecr(io6,jyr,jjul,jhr,idtz,0,23)
        kdate = jyr * 100000 + jjul * 100 + jhr
      endif
C     SKIP TO FIRST DATE IN BINARY INPUT FILE IF THERE IS DATA FOR THE
C     REQUESTED PERIOD OF THIS RUN
      if(ibdathr.GT.kbdathr)then
        if(LPREV .AND. ibin.NE.0)then
          call deltt(kbyr,kbjul,kbhr,ibyr,ibjul,ibhr,iskip)
          do 45 i = 1,iskip
            call rdp(ipform,nstnb,ippack,ioprev,1,myr,mjul,mhr,precip)
45        continue
        endif
      endif
C   MAIN LOOP: (HOURS)
      do 55 i = 1,nrec
C     READ DATA FOR 1 HOUR FROM PREVIOUS OUTPUT FILE
        if(LPREV .AND. ibin.NE.0)then
          if(ndate.LE.kedathr.AND.ndate.GE.kbdathr)then
            call rdp(ipform,nstnb,ippack,ioprev,0,iyr,ijul,ihr,precip)
            idathr = iyr * 100000 + ijul * 100 + ihr
            if(kdate.NE.idathr) then
              write(io6,200) idathr, kdate
              go to 99
            endif
C     TRANSFER STATIONS OF INTEREST FROM READ BUFFER TO OUTPUT ARRAY
            do 65 j = 1,nbstn
              index = ixref(j)
              xprecip(j) = precip(index)
65          continue
          else
              do 75 j = 1,nbstn
                xprecip(j) = 9999.
75            continue
          endif
        else
            do 85 j = 1,nbstn
              xprecip(j) = 9999.
85          continue
        endif
C     SET ARRAY INDEX COUNTER FOR FORMATTED STATIONS
        index = nbstn
C     LOOP OVER FORMATTED FILES (STATIONS)
        do 95 k = 1,npf
          index = index + 1
          iofor = k + 6
C         READ ONE OR TWO RECORDS OF FORMATTED DATA AND DETERMINE THE
C         PRECIPITATION RATE
          if(iotz.EQ.istz(k))then
            jdate = ndate
          else
C           CORRECT REQUESTED LOCAL TIME TO OUTPUT TIME ZONE
            idtz = iotz - istz(k)
            call dedat(ndate,jyr,jjul,jhr)
            call indecr(io6,jyr,jjul,jhr,idtz,0,23)
            jdate = jyr * 100000 + jjul * 100 + jhr
          endif
          call uncdp(iofor,jdate,idstn(nbstn+k),maxap,index,pmm,icode)
          xprecip(index) = pmm
C         KEEP COUNT OF THE STATUS OF EACH HOUR OF DATA
          if(icode.EQ.1)then
            if(pmm.EQ.0)then
              icount(k,1) = icount(k,1) + 1
            else
              icount(k,2) = icount(k,2) + 1
            endif
          else
            icount(k,icode+1) = icount(k,icode+1) + 1
          endif
95      continue
C       DECODE CURRENT DATE
        call dedat(ndate,jyr,jjul,jhr)
        call dedat(kdate,iyr,ijul,ihr)
C       WRITE PRECIPITATION DATA TO OUTPUT FILE FOR THIS HOUR
        call wrp(ioform,ntstn,iopack,ioprec,jyr,jjul,jhr,xprecip)
        if(ndate.LT.iedathr)then
C         INCREMENT CURRENT DATE/HR BY ONE HOUR
          call indecr(io6,jyr,jjul,jhr,1,0,23)
          ndate = jyr * 100000 + jjul * 100 + jhr
C         INCREMENT CURRENT DATE/HR FOR PREVIOUS DATA BY ONE HOUR
          call indecr(io6,iyr,ijul,ihr,1,0,23)
          kdate = iyr * 100000 + ijul * 100 + ihr
        endif
        if(ndate.GT.iedathr) then
          write(io6,300) ndate, iedathr
          go to 99
        endif
55    continue
200   format('  Error in Subr. RDWRIT: Next date and hour from binary ',
     1       'input file, ',i8, /, ' does not match the expected date',
     2      ' and hour, ',i8)
300   format('  ERROR in Subr. RDWRIT: NDATE, ',i8,' is greater than ',
     1       'the requested end date (IEDATHR), ',i8)
99    return
      end

c ------------------------------------------------------------------------------
      subroutine uncdp(io,ndate,idexp,maxap,k,pmm,icode)
c ------------------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 010608                    UNCDP
c ---          J. Scire, SRC
c
c --- Determine the precipitation rate for a given date/hour by:
c        (a) reading a character variable storing a previously read
c            record
c        (b) reading a precipitation record from a TD-3240 file
c        (c) resolving an accumulation period
c        (d) resolving a missing data period
c
c --- UPDATES:
c
c --- Updated 010608 from 941215 (D. Strimaitis)
c     - Interpret icode returned from PREAD to recognize EOF (avoids
c       endless loop when data ends before processing period)
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c             NDATE - integer    - Coded date/time field (YYYYJJJHH) of
c                                  current hour
c             IDEXP - integer    - ID of current precip. station
c             MAXAP - integer    - Maximum allowed length (hrs) of an
c                                  accumulation period
c                 K - integer    - Array index of precip. arrays
c                                  (K = station no.)
c
c --- Parameters used:
c                MXPS
c
c --- OUTPUT:
c               PMM - real       - Precipitation rate (mm/hr)
c                                  (missing value indicator = 9999.)
c             ICODE - integer    - Data status code:
c                                  1 = valid hourly value,
c                                  2 = valid accumulation period,
c                                  3 = missing data flag (labeled
c                                      missing),
c                                  4 = missing due to excessive length
c                                      of accumulation period,
c                                  5 = missing data before first valid
c                                      record in file
c                                  6 = missing data after last valid
c                                      record in file
c
c --- UNCDP called by:  RDWRIT
c --- UNCDP calls:      PREAD
c
c --- IFLAG -- Flag indicating precip. data status:
c              IFLAG = -99 if this is the first pass for this station
c              IFLAG =   0 if date/hr of first precip. record has not
c                          been reached yet (data is assumed missing
c                          up to date/hr of first valid record)
c              IFLAG =  +1 if current date/hr > date/hr of first record
c                          (precip. rate is assumed = 0.0 between time
c                          of valid records)
c ------------------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
c
      real pmmsav(mxps)
      integer icodsv(mxps),ibdat(mxps),iedat(mxps),iflag(mxps)
      integer iprev(mxps)
      character*42 cdat(mxps)
c
      data icodsv/mxps*0/,ibdat/mxps*0/,iedat/mxps*0/
      data iflag/mxps*-99/,cdat/mxps*' '/
      data iprev/mxps*0/
c
c --- determine if current date/hr is within range previously stored
c --- pmmsav array
10    continue
      if(ndate.lt.ibdat(k))then
c
c ---    Date/hr between valid records -- precip. rate = 0.0
         pmm=0.0
         icode=1
         return
      else if(ndate.le.iedat(k))then
c
c ---    Current date/hr is within period of validity of pmmsav
         pmm=pmmsav(k)
         icode=icodsv(k)
         return
      endif
c
c --- Current date/hr is after end of period of validity of pmmsav
      call pread(io,ndate,idexp,maxap,iflag(k),cdat(k),iprev(k),
     1           icodsv(k),ibdat(k),iedat(k),pmmsav(k))
      icode=icodsv(k)
      if(icode.EQ.6) then
c ---    EOF encountered; done with this station
         pmm=pmmsav(k)
         return
      else
         go to 10
      endif
      end
c ----------------------------------------------------------------------
      subroutine pread(io,ndate,idexp,maxap,iflag,cdat,iprev,
     1 icode,ibdat,iedat,pmmsav)
c ----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 030402                    PREAD
c ---          J. Scire, SRC
c
c --- Read a precipitation record -- if necessary, read a second record
c --- to resolve a missing data or accumulation period
c
c --- UPDATES:
c
c --- Updated 030402 from 010608 (J. Popovic)
c     - Report an error if the first precipitation flag in the data
c       file is a closed end-period bracket such as } or ]
c
c --- Updated 010608 from 010315 (D. Strimaitis)
c     - Allow variable record-length precip file by placing reads in
c       new subroutine GETPREC
c     - Interpret past and current hourly data flags (FLAG-1)
c        A/A  ,   D/D   M/M   E              (past)
c        a/A  ,   {/}   [/]   E    g    T    (current)
c       Daily flags are not interpreted
c     - Interpret some current data quality flags (FLAG-2)
c        Q (value treated as bad)  q (value accepted)
c
c --- Updated 010315 from 981025 (D. Strimaitis)
c     - Y2K treatment of year enforced
c
c --- Updated 981025 from 961113 (D. Strimaitis)
c     - Convert time-mark used in TD3240 file from a (01-24)
c       time-ending to a (00-23) time-ending convention
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c             NDATE - integer    - Coded date/time field (YYYYJJJHH) of
c                                  current hour
c             IDEXP - integer    - ID of current precip. station
c             MAXAP - integer    - Maximum allowed length (hrs) of an
c                                  accumulation period
c             IFLAG - integer    - Flag indicating precip. data status:
c                                  IFLAG = -99 if this is the first pass
c                                              for this station
c                                  IFLAG =   0 if date/hr of first
c                                              precip. record has not
c                                              been reached yet (data
c                                              are assumed missing up to
c                                              date/hr of first valid
c                                              record)
c                                  IFLAG =  +1 if current date/hr >
c                                              date/hr of first record
c                                              (precip. rate is assumed
c                                              = 0.0 between time of
c                                              valid records)
c              CDAT - char.*42   - A character string to store a TD-3240
c                                  data record (used only if IFLAG=0)
c             IPREV - integer    - Coded date/time field (YYYYJJJHH) of
c                                  previously read TD-3240 record
c
c --- Parameters used:
c                IO6
c
c --- OUTPUT:
c             ICODE - integer    - Data status code:
c                                  1 = valid hourly value,
c                                  2 = valid accumulation period,
c                                  3 = flagged as missing
c                                  4 = missing due to excessive length
c                                      of accumulation period
c                                  5 = missing data before first valid
c                                      record in file
c                                  6 = missing data after last valid
c                                      record in file
c             IBDAT - integer    - Beginning date/time of data
c                                  (YYYYJJJHH)
c             IEDAT - integer    - Ending date/time of data (YYYYJJJHH)
c            PMMSAV - real       - Precipitation rate (mm/hr)
c                                  (missing value indicator = 9999.)
c              CDAT - char.*42   - A character string storing a TD-3240
c                                  data record (an output only if
c                                  IFLAG = -99)
c             IPREV - integer    - Updated coded date/time field of
c                                  last TD-3240 record read
c
c --- PREAD called by:  UNCDP
c --- PREAD calls:   JULDAY
c                    INDECR
c                    YR4
c                    GETPREC
c ----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
c
      character*42 cdat,cdatn
      character*1 cflag,cflag2,cflag_2,cflag2_2
      logical leof

c --- Hold initial iflag value for missing period screen
      integer iflagx

      cflag=' '
      cflag2=' '
c
      if(iflag.le.0)then
c
c ---    If first time through for this station, read TD-3240 record &
c ---    store in character string (CDAT)
         if(iflag.eq.-99)then
            call GETPREC(io,cdat,leof)
            if(LEOF) goto 995
            iflagx=iflag
            iflag=0
c
c ---       extract station id, date/hr
            read(cdat,20)idsta,iyr,imo,iday,ihr
20          format(3x,i6,8x,i4,i2,i4,3x,i2,2x,i6,2a1)

c ---       Make sure year is YYYY (Y2K)
            call YR4(io6,iyr,ierr)
            if(ierr.NE.0) stop 'Halted in PREAD - see list file'

            call julday(io6,iyr,imo,iday,ijul)
c ---       convert hour 24 to hour 00 of next day
            if(ihr.EQ.24) then
               ihr=23
               call indecr(io6,iyr,ijul,ihr,1,0,23)
            endif
            idate=iyr*100000+ijul*100+ihr
            iprev=idate
c
c ---       check station id vs. expected station id
            if(idsta.ne.idexp)go to 1010
c
c ---       all data up to time of first record in file is considered
c ---       missing
            icode=5
            ibdat=0
c ---       subtract one hour from yr/Julian day/hr
            call indecr(io6,iyr,ijul,ihr,-1,0,23)
            iedat=iyr*100000+ijul*100+ihr
            pmmsav=9999.
            return
         endif
c
c ---    extract data from a previously read character string
         read(cdat,20,end=995)idsta,iyr,imo,iday,ihr,ihinch,
     &                        cflag,cflag_2

c ---    Make sure year is YYYY (Y2K)
         call YR4(io6,iyr,ierr)
         if(ierr.NE.0) stop 'Halted in PREAD - see list file'

         iflag=1
         call julday(io6,iyr,imo,iday,ijul)
c ---    convert hour 24 to hour 00 of next day
         if(ihr.EQ.24) then
            ihr=23
            call indecr(io6,iyr,ijul,ihr,1,0,23)
         endif
         idate=iyr*100000+ijul*100+ihr
c
c ---    check station id vs. expected station id
         if(idsta.ne.idexp)go to 1010
      else
c
c ---    read a new record
         call GETPREC(io,cdatn,leof)
         if(LEOF) goto 995
c ---    Extract data from character variable
         read(cdatn,20,end=995)idsta,iyr,imo,iday,ihr,ihinch,
     &                         cflag,cflag_2

c ---    Make sure year is YYYY (Y2K)
         call YR4(io6,iyr,ierr)
         if(ierr.NE.0) stop 'Halted in PREAD - see list file'

         call julday(io6,iyr,imo,iday,ijul)
c ---    convert hour 24 to hour 00 of next day
         if(ihr.EQ.24) then
            ihr=23
            call indecr(io6,iyr,ijul,ihr,1,0,23)
         endif
         idate=iyr*100000+ijul*100+ihr
c
c ---    check station id vs. expected station id
         if(idsta.ne.idexp)go to 1010
c
c ---    check if date/hr of record is out of order
         if(idate.gt.iprev)then
            iprev=idate
         else
            go to 1040
         endif
      endif

c --- Interpret flags from this hour
c ----------------------------------

c --- First translate some flags to 'old' values to simplify tests
      if(cflag.EQ.',') cflag='A'

c --- Test for an initial end-of-period marker
      if(iflagx.eq.-99)then
         if(cflag.eq.'}'.or.cflag.eq.']') goto 1001
      endif

      if(cflag.EQ.'{' .OR. cflag.EQ.'}') cflag='D'
      if(cflag.EQ.'[' .OR. cflag.EQ.']') cflag='M'

      if(cflag.eq.' ' .or. cflag.eq.'E' .or.
     &   cflag.eq.'g' .or. cflag.eq.'T')then
c ---    Need to check FLAG-2 as final screen
         if(cflag_2 .NE. 'Q') then
c ---       Valid hourly data value -- convert to mm/hr
            icode=1
            ibdat=idate
            iedat=idate
            pmmsav=0.254*float(ihinch)
         else
c ---       Invalid hourly data value (report as missing)
            icode=3
            ibdat=idate
            iedat=idate
            pmmsav=9999.
         endif
         return

      else if(cflag.eq.'A' .OR. cflag.eq.'a')then
c ---    Should indicate beginning of an accumulation period
         if(ihinch .EQ. 99999) then
c ---       Looks OK -- read next record to obtain the
c ---       ending date/time of accumulation period & accum. amount
110         call GETPREC(io,cdatn,leof)
            if(LEOF) goto 996
c ---       Extract data from character variable
            read(cdatn,20,end=996)jdsta,jyr,jmo,jday,jhr,jhinch,
     &                            cflag2,cflag2_2
            if(cflag2.EQ.',') cflag2='A'

c ---       Discard if this is an accumulation continuation record
            if(cflag2.EQ.'A' .AND. jhinch.EQ.99999) goto 110

c ---       Make sure year is YYYY (Y2K)
            call YR4(io6,jyr,ierr)
            if(ierr.NE.0) stop 'Halted in PREAD - see list file'
            call julday(io6,jyr,jmo,jday,jjul)
c ---       convert hour 24 to hour 00 of next day
            if(jhr.EQ.24) then
               jhr=23
               call indecr(io6,jyr,jjul,jhr,1,0,23)
            endif
            jdate=jyr*100000+jjul*100+jhr
            if(jdsta.ne.idexp)go to 1020
            if(jdate.gt.iprev)then
               iprev=jdate
            else
               go to 1050
            endif

c ---       Expect this to be the close of the accumulation period
c ---       with a valid precip amount
            if(cflag2.ne.'A' .OR. jhinch.EQ.99999)then
c
c ---          ERROR -- unpaired accumulation period
               go to 1000
            else
c
c ---          paired accumulation records -- resolve accumulation
c ---          period precip. rate
               call deltt(iyr,ijul,ihr,jyr,jjul,jhr,idelt)
               nhrs=idelt+1
c
c ---          if length of the accumulation period exceeds max.
c ---          allowed, consider data as missing
               if(nhrs.gt.maxap)then
                  icode=4
                  ibdat=idate
                  iedat=jdate
                  pmmsav=9999.
                  return
               endif
c
c ---          valid accumulation period -- check FLAG-2
               if(cflag2_2 .NE. 'Q') then
c ---             Value is OK -- resolve & save results
                  icode=2
                  ibdat=idate
                  iedat=jdate
                  pmmsav=0.254*float(jhinch)/float(nhrs)
                  return
               else
c ---             Invalid accumulation value (report as missing)
                  icode=3
                  ibdat=idate
                  iedat=jdate
                  pmmsav=9999.
               endif
            endif

         else
c ---       The starting accumulation flag is not associated with a
c ---       missing value, so something is wrong here!
            go to 993
         endif
c
      else if(cflag.eq.'M' .or. cflag.eq.'D')then
c
c ---    beginning of missing or deleted data period -- read next record
c ---    with ending date/time of missing or deleted data period
         call GETPREC(io,cdatn,leof)
         if(LEOF) goto 998
c ---    Extract data from character variable
         read(cdatn,20,end=998)jdsta,jyr,jmo,jday,jhr,jhinch,
     &                         cflag2,cflag2_2
         if(cflag2.EQ.',') cflag2='A'
         if(cflag2.EQ.'{' .OR. cflag2.EQ.'}') cflag2='D'
         if(cflag2.EQ.'[' .OR. cflag2.EQ.']') cflag2='M'

c ---    Make sure year is YYYY (Y2K)
         call YR4(io6,jyr,ierr)
         if(ierr.NE.0) stop 'Halted in PREAD - see list file'

         call julday(io6,jyr,jmo,jday,jjul)
c ---    convert hour 24 to hour 00 of next day
         if(jhr.EQ.24) then
            jhr=23
            call indecr(io6,jyr,jjul,jhr,1,0,23)
         endif
         jdate=jyr*100000+jjul*100+jhr
         if(jdsta.ne.idexp)go to 1020
         if(jdate.gt.iprev)then
            iprev=jdate
         else
            go to 1050
         endif
c
         if(cflag2.ne.cflag)then
c
c ---       ERROR -- unpaired missing or deleted data records
            go to 1030
         else
c
c ---       paired missing data records
            icode=3
            ibdat=idate
            iedat=jdate
            pmmsav=9999.
            return
         endif
      endif
c
c --- invalid precipitation flag encountered
      write(io6,990)idsta,ndate,io,iyr,imo,iday,ihr,cflag,cflag2
990   format(/1x,'Error in subr. PREAD -- invalid ',
     1 'precipitation flag encountered'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYYYJJJHH) = ',i9,3x,'io = ',i5
     3 //1x,'Yr: ',i4,2x,'Month: ',i2,2x,'Day: ',i2,
     4 2x,'Hr: ',i2//1x,'CFLAG = ',a1,3x,'CFLAG2 = ',a1)
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
c --- Invalid first flag encountered
1001  continue
      write(io6,1003)idsta,ndate,io,iyr,imo,iday,ihr,cflag
1003  format(/1x,'Error in subr. PREAD -- invalid first ',
     1 'precipitation flag in the data file encountered'
     2 //1x,'ID = ',i6,3x,
     3 //1x,'Requested date/hr (YYYYJJJHH) = ',i9,3x,'io = ',i5
     4 //1x,'Yr: ',i4,2x,'Month: ',i2,2x,'Day: ',i2,
     5 2x,'Hr: ',i2//1x,'CFLAG = ',a1)
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
c --- invalid start for accumulation period
993   continue
      write(io6,994)idsta,ndate,io,iyr,imo,iday,ihr,cflag,ihinch
994   format(/1x,'Error in subr. PREAD -- invalid start for ',
     1 'accumulation period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYYYJJJHH) = ',i9,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i4,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'Precip is not 99999: ',i6)
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
c --- end of file encountered
995   continue
      icode=6
      ibdat=0
      iedat=9999999
      pmmsav=9999.
      return
c
c --- end of file encountered -- unpaired accumulation period
996   continue
      write(io6,997)idsta,ndate,io,iyr,imo,iday,ihr,cflag
997   format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'accumulation period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYYYJJJHH) = ',i9,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i4,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'END OF FILE REACHED')
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
c --- end of file encountered -- unpaired missing or deleted period
998   continue
      write(io6,999)idsta,ndate,io,iyr,imo,iday,ihr,cflag
999   format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'missing or deleted data period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYYYJJJHH) = ',i9,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i4,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'END OF FILE REACHED')
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
c --- write error message -- unpaired accumulation period
1000  continue
      write(io6,1002)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 jyr,jmo,jday,jhr,cflag2
1002  format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'accumulation period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYYYJJJHH) = ',i9,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i4,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'Yr: ',i4,2x,'Month: ',i2,2x,
     6 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1)
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
c --- write error message -- unexpected station ID
1010  continue
      write(io6,1012)idsta,idexp,ndate,io,iyr,imo,iday,ihr
1012  format(/1x,'Error in subr. PREAD -- station ID does not',
     1 ' match expected station ID'//1x,'ID = ',i6,3x,
     2 'Expected ID = ',i6//1x,'Requested date/hr (YYYYJJJHH) = ',
     3 i9,3x,'io = ',i5//1x,'Yr: ',i4,2x,'Month: ',i2,2x,'Day: ',i2,
     4 2x,'Hr: ',i2)
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
1020  continue
      write(io6,1012)jdsta,idexp,ndate,io,jyr,jmo,jday,jhr
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
c --- write error message -- unpaired missing data period
1030  continue
      write(io6,1032)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 jyr,jmo,jday,jhr,cflag2
1032  format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'missing or deleted data period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYYYJJJHH) = ',i9,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i4,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'Yr: ',i4,2x,'Month: ',i2,2x,
     6 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1)
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
c --- write error message -- invalid date/hr ( <= previous value)
1040  continue
      write(io6,1042)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 idate,iprev
1042  format(/1x,'Error in subr. PREAD -- invalid date/hr ( <= ',
     1 'previous value)'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYYYJJJHH) = ',i9,3x,'io = ',i5
     3 //1x,'Yr: ',i4,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'Date/hr (YYYYJJJHH) = ',i10,'  (Current record)'
     6 //1x,'Date/hr (YYYYJJJHH) = ',i10,' (Previous record)')
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
1050  continue
      write(io6,1042)jdsta,ndate,io,jyr,jmo,jday,jhr,cflag,
     1 jdate,iprev
      write(*,*) 'ERROR in SUBR. PREAD -- See Run LIST file'
      stop
c
      end
c ------------------------------------------------------------------------------
      subroutine rdp(iformp,npsta,ippack,io,iskip,iyr,ijul,ihr,xprecp)
c ------------------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 010315                    RDP
c ---          J. Scire, SRC
c
c --- Read and process precipitation data for one hour
c        If data are packed,   2 records/hour -- read, unpack, & decode
c        If data are unpacked, 1 record/hour  -- read and decode
c
c --- UPDATES:
c
c --- Updated 010315 from 961113 (D. Strimaitis)
c     - Y2K treatment of year enforced
c
c
c --- INPUTS:
c            IFORMP - integer    - Data format flag (0=data not used,
c                                  1=unformatted, 2=formatted)
c             NPSTA - integer    - Number of precipitation stations
c            IPPACK - integer    - Data packing code (0=unpacked,
c                                  1=packed)
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c             ISKIP - integer    - Unpacking flag (used only if IPPACK=1)
c                                  (If ISKIP=0, data are unpacked, if
c                                  ISKIP=1, data are read but not
c                                  unpacked)
c
c --- Parameters used:
c                IO6, MXPS
c
c --- OUTPUT:
c               IYR - integer    - Year of precip. data (4 digits)
c              IJUL - integer    - Julian day number of precip. data
c               IHR - integer    - Ending hour (1-24) of precip. data
c     XPRECP(NPSTA) - real array - Unpacked Precip. rates (mm/hr) for
c                                  each station (9999. indicates
c                                  missing value)
c
c --- RDP called by:  RDWRIT
c --- RDP calls:   RDNWD
c                  UNPACK
c                  DEDAT
c                  YR4
c ------------------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
c
      real xprecp(npsta),xbuf(mxps)
c
      if(iformp.eq.0)then
c ---    precip data not used
         do 5 i=1,npsta
         xprecp(i)=9999.
5        continue
         return
c
      else if(iformp.eq.1)then
c
c ---    precip. data unformatted
         if(ippack.eq.0)then
c
c ---       read unpacked data
            read(io)idathr,xprecp
         else
c
c ---       read first record of packed data
            read(io)idathr,nwords
c
c ---       read second record of packed data (if necessary)
            if(nwords.eq.0)then
c
c ---          second record is omitted if all values are zero
               if(iskip.eq.0)then
                  do 10 i=1,npsta
                  xprecp(i)=0.0
10                continue
               endif
            else
c
c ---          read second record with "nwords"
               call rdnwd(io,nwords,xbuf)
c
c ---          unpack precipitation rates
               if(iskip.eq.0)call unpack(nwords,xbuf,npsta,xprecp)
            endif
         endif
c
c ---    decode date and time
         call dedat(idathr,iyr,ijul,ihr)
c
      else if(iformp.eq.2)then
c
c ---    precip. data formatted
         read(io,*)iyr,ijul,ihr,xprecp
      else
         write(io6,12)iformp
12       format(//2x,'ERROR IN SUBR. RDP -- invalid value of IFORMP'/
     1   5x,'IFORMP = ',i10)
         write(*,*) 'ERROR in SUBR. RDP -- See Run LIST file'
         stop
      endif

c --- Make sure year is YYYY (Y2K)
      call YR4(io6,iyr,ierr)
      if(ierr.NE.0) stop 'Halted in RDP - see list file'

c
      return
      end
c-----------------------------------------------------------------------
      subroutine wrp(iformp,npsta,ippack,io,iyr,ijul,ihr,xprecp)
c-----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 010315                    WRP
c ---          J. Scire, SRC
c
c --- Write precipitation data for one hour -- formatted or unformatted
c --- if unformatted, option to pack (compress) data
c        If data are packed,   2 records/hour
c        If data are unpacked, 1 record/hour
c
c --- UPDATES:
c
c --- Updated 010315 from 961113 (D. Strimaitis)
c     - YYYY format of year
c
c
c --- INPUTS:
c            IFORMP - integer    - Data format flag (0=data not used,
c                                  1=unformatted, 2=formatted)
c             NPSTA - integer    - Number of precipitation stations
c            IPPACK - integer    - Data packing code (0=unpacked,
c                                  1=packed)
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c               IYR - integer    - Year of precip. data (4 digits)
c              IJUL - integer    - Julian day number of precip. data
c               IHR - integer    - Ending hour (1-24) of precip. data
c     XPRECP(NPSTA) - real array - Unpacked Precip. rates (mm/hr) for
c                                  each station (9999. indicates
c                                  missing value)
c
c --- Parameters used:
c                IO6, MXPS
c
c --- OUTPUT:  none
c
c --- WRP called by:  RDWRIT
c --- WRP calls:   PACK
c                  WRNWD
c-----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
c
      real xprecp(npsta),xbuf(mxps)

      if(iformp.eq.1)then
c ---    Unformatted option
c ---    code date and time into a single integer variable
         idathr=iyr*100000+ijul*100+ihr
c
         if(ippack.eq.0)then
c
c ---       write unpacked data
            write(io)idathr,xprecp
         else
c
c ---       pack and write data
            call pack(npsta,xprecp,nwords,xbuf)
            write(io)idathr,nwords
            if(nwords.gt.0)call wrnwd(io,nwords,xbuf)
         endif
      else if(iformp.eq.2)then
c ---    Formatted option
c***     write(io,*)iyr,ijul,ihr,xprecp
         write(io,10)iyr,ijul,ihr,xprecp
10       format(3i4,1x,10f9.3,24(/,13x,10f9.3))
      else
         write(io6,12)iformp
12       format(//2x,'ERROR IN SUBR. WRP -- invalid value of IFORMP'/
     1   5x,'IFORMP = ',i10)
         write(*,*) 'ERROR in SUBR. WRP -- See Run LIST file'
         stop
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine wrhd(iform,io,ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,
     1 nsta,ipack,idsta)
c-----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 030402                    WRHD
c ---          J. Scire, Earth Tech, Inc.
c
c --- Write the header records for a meteorological data file
c
c --- UPDATES:
c     V5.3(030402) from V5.2(961113) (D. Strimaitis)
c     - New header record structure
c
c --- INPUTS:
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
c
c --- Parameters used:
c                IO6
c
c --- OUTPUT:  none
c
c --- WRHD called by:  RDWRIT
c --- WRHD calls:      none
c-----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
      include 'qa.pmg'
c
      integer idsta(nsta)
      character*8 pmap
      character*16 dataset,dataver
      character*64 datamod
      character*80 comment1

c --- Configure output variables
      data dataset/'PRECIP.DAT'/, dataver/'2.0'/
      data datamod/'Header structure with coordinate parameters'/
      data ncomment/1/
      data comment1/'Produced by PMERGE Version: '/

c --- Construct the version-level comment string
      j=29
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

c --- Set map projection information
      pmap='NONE    '

      if(iform.eq.1)then
c ---    Unformatted file
c ---    code beginning and ending date/hour
         ibeg=ibyr*100000+ibjul*100+ibhr
         iend=ieyr*100000+iejul*100+iehr
c ---    write header records
         write(io) dataset,dataver,datamod
         write(io) ncomment
         write(io) comment1
         write(io) pmap
         write(io)ibeg,iend,ibtz,nsta,ipack
         write(io)idsta
      else if(iform.eq.2)then
c ---    Formatted file
c ---    check that packing code = 0 (unpacked) with formatted file
         if(ipack.ne.0)then
            write(io6,11)ipack,iform
11          format(//2x,'ERROR IN SUBR. WRHD -- IPACK must be zero ',
     1      'when IFORM = 2 (formatted file)'/5x,'IPACK = ',i10,5x,
     2      ' IFORM = ',i10)
            write(*,*) 'ERROR in SUBR. WRHD -- See Run LIST file'
            stop
         endif
c ---    write header records
         write(io,'(2a16,a64)') dataset,dataver,datamod
         write(io,'(i4)') ncomment
         write(io,'(a80)') comment1
         write(io,'(a8)') pmap
         write(io,10)ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,nsta
10       format(2(i6,2i4),2i5)
         do i=1,nsta
            write(io,20)idsta(i)
         enddo
20       format(i8)
      else
         write(io6,12)iform
12       format(//2x,'ERROR IN SUBR. WRHD -- invalid value of IFORM'/
     1   5x,'IFORM = ',i10)
         write(*,*) 'ERROR in SUBR. WRHD -- See Run LIST file'
         stop
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine rdhd(iform,io,ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,
     1 nsta,ipack,idsta,maxs)
c-----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 030402                    RDHD
c ---          J. Scire, Earth Tech, Inc.
c
c --- Read the header records from the unformatted precipitation data
c     file
c
c --- UPDATES:
c     V5.3(030402) from V5.2(010315) (D. Strimaitis)
c     - New header record structure
c
c --- Updated 010315 from 961113 (D. Strimaitis)
c     - Y2K treatment of year enforced
c
c
c --- INPUTS:
c             IFORM - integer    - Data format flag (0=data not used,
c                                  1=unformatted, 2=formatted)
c                IO - integer    - Fortran unit no. of input file
c              MAXS - integer    - Maximum number of stations
c
c --- Parameters used:
c                IO6
c
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
c
c --- RDHD called by:  RDWRIT
c --- RDHD calls:   DEDAT
c-----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
c
      integer idsta(maxs)
      character*8 pmap
      character*16 dataset,dataver
      character*64 datamod
      character*80 comment1
c
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
         if(dataset.NE.'PRECIP.DAT') then
            write(io6,*)
            write(io6,*) 'RDHD:  Invalid previous PRECIP.DAT file'
            write(io6,*) 'Dataset name found = ',dataset
            write(io6,*) 'Dataset name expected = ','PRECIP.DAT'
            write(*,987)
            stop
         endif
         read(io) ncomment
         do i=1,ncomment
            read(io) comment1
         enddo
         read(io) pmap
         if(pmap.NE.'NONE   ') then
            write(io6,*)
            write(io6,*) 'RDHD:  Invalid previous PRECIP.DAT file'
            write(io6,*) 'Coordinate type found = ',pmap
            write(io6,*) 'Coordinate type expected = ','NONE'
            write(*,987)
            stop
         endif
         read(io)ibeg,iend,ibtz,nsta,ipack
         read(io)(idsta(n),n=1,nsta)
c
c ---    decode starting and ending dates
         call dedat(ibeg,ibyr,ibjul,ibhr)
         call dedat(iend,ieyr,iejul,iehr)
c
      else if(iform.eq.2)then
         ipack=0
         read(io,'(2a16,a64)') dataset,dataver,datamod
c ---    Check for valid dataset name
         if(dataset.NE.'PRECIP.DAT') then
            write(io6,*)
            write(io6,*) 'RDHD:  Invalid previous PRECIP.DAT file'
            write(io6,*) 'Dataset name found = ',dataset
            write(io6,*) 'Dataset name expected = ','PRECIP.DAT'
            write(*,987)
            stop
         endif
         read(io,*) ncomment
         do i=1,ncomment
            read(io,'(a80)') comment1
         enddo
         read(io,'(a8)') pmap
         if(pmap.NE.'NONE   ') then
            write(io6,*)
            write(io6,*) 'RDHD:  Invalid previous PRECIP.DAT file'
            write(io6,*) 'Coordinate type found = ',pmap
            write(io6,*) 'Coordinate type expected = ','NONE'
            write(*,987)
            stop
         endif
         read(io,*)ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,nsta
         read(io,*)(idsta(n),n=1,nsta)
      else
         write(io6,12)iform
12       format(//2x,'ERROR IN SUBR. RDHD -- invalid value of IFORM'/
     1   5x,'IFORM = ',i10)
         write(*,*) 'ERROR in SUBR. RDHD -- See Run LIST file'
         stop
      endif

c --- Make sure year is YYYY (Y2K)
      call YR4(io6,ibyr,ierr)
      if(ierr.NE.0) stop 'Halted in RDHD - see list file'
      call YR4(io6,ieyr,ierr)
      if(ierr.NE.0) stop 'Halted in RDHD - see list file'

      return
987   format(1x,'ERROR in PMERGE run - see PMERGE.LST file')
      end
c ------------------------------------------------------------------------------
      subroutine pack(nvals,xdata,nwords,xbuf)
c ------------------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 941215                    PACK
c ---          J. Scire, SRC
c
c --- Pack a packed array of data by eliminating zeroes
c --- (assumes all data values >= 0.0)
c
c --- INPUTS:
c       XDATA(NVALS) - real array - Array of unpacked data
c              NVALS - integer    - No. of unpacked values stored in
c                                   packed array
c
c --- OUTPUT:
c             NWORDS - integer    - Number of packed words
c       XBUF(NWORDS) - real array - Array of packed data
c
c --- PACK called by:  WRP
c --- PACK calls:      none
c ------------------------------------------------------------------------------
      real xbuf(nvals),xdata(nvals)
c
c --- pack the data
      nzero=0
      nwords=0
      do 100 i=1,nvals
c
      if(xdata(i).eq.0.)then
         nzero=nzero+1
         go to 100
      endif
c
      if(nzero.gt.0)then
         nwords=nwords+1
         xbuf(nwords)=-nzero
         nzero=0
      endif
c
      nwords=nwords+1
      xbuf(nwords)=xdata(i)
100   continue
c
c --- account for case when last value in unpacked array is a zero
      if(nzero.gt.0)then
         nwords=nwords+1
         xbuf(nwords)=-nzero
      endif
c
c --- if all values are zero, set nwords = 0
      nzero=-xbuf(1)+0.001
      if(nzero.eq.nvals)nwords=0
c
      return
      end
c-----------------------------------------------------------------------
      subroutine unpack(nwords,xbuf,nvals,xdata)
c-----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 961113                    UNPACK
c ---          J. Scire, SRC
c
c --- Unpack a packed array of data
c
c --- INPUTS:
c             NWORDS - integer    - Number of packed words
c       XBUF(NWORDS) - real array - Array of packed data
c              NVALS - integer    - No. of unpacked values stored in
c                                   packed array
c
c --- Parameters used:
c                IO6
c
c --- OUTPUT:
c       XDATA(NVALS) - real array - Array of unpacked data
c
c --- UNPACK called by:  RDP
c --- UNPACK calls:      none
c-----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
c
      real xbuf(nwords),xdata(nvals)
c
      ii=0
      do 100 i=1,nwords
c
      if(xbuf(i).ge.0.)then
c
c ---    transfer actual value to unpacked array
         ii=ii+1
         xdata(ii)=xbuf(i)
      else
c
c ---    insert "jj" zero values into unpacked array
         jj=abs(xbuf(i))+0.1
         do 50 j=1,jj
         ii=ii+1
         xdata(ii)=0.0
50       continue
      endif
100   continue
c
c --- check that size of unpacked array matches expected value
      if(ii.ne.nvals)then
         write(io6,102)ii,nvals
102      format(//5x,'ERROR IN SUBR. UNPACK -- no. values unpacked ',
     1   'does not match expected value'//8x,'ii (no. unpacked) = ',
     2   i10//8x,'nvals (no. expected) = ',i10)
         write(*,*) 'ERROR in SUBR. UNPACK -- See Run LIST file'
         stop
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine rdnwd(io,n,xbuf)
c-----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 941215                    RDNWD
c ---          J. Scire, SRC
c
c --- Read "N" words from an unformatted data file
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of input file
c                 N - integer    - Number of words to read
c
c --- OUTPUT:
c           XBUF(N) - real array - Array of values from data file
c
c --- RDNWD called by:  RDP
c --- RDNWD calls:      none
c-----------------------------------------------------------------------
      real xbuf(n)
c
      read(io)xbuf
c
      return
      end
c-----------------------------------------------------------------------
      subroutine wrnwd(io,n,xbuf)
c-----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 941215                    WRNWD
c ---          J. Scire, SRC
c
c --- Write "N" words to an unformatted data file
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of input file
c                 N - integer    - Number of words to write
c           XBUF(N) - real array - Array of values to write
c
c --- OUTPUT:  none
c
c --- WRNWD called by:  WRP
c --- WRNWD calls:      none
c-----------------------------------------------------------------------
      real xbuf(n)
c
      write(io)xbuf
c
      return
      end
c----------------------------------------------------------------------
      subroutine getprec(io,cdat,leof)
c----------------------------------------------------------------------
c
c --- PMERGE   Version: 5.32      Level: 020722                 GETPREC
c ---          D. Strimaitis, Earth Tech
c
c --- PURPOSE: Get precipitation data for a single hour.  If the input
c              data are in the fixed record-length format, each hour is
c              written as a single record.  If the input data are in the
c              variable record-length format, each record contains data
c              for a day, and the last hour of the day (25) is the total
c              amount for the day.  Note that hours with zero precip
c              are typically not included.
c
c --- UPDATES:
c
c --- Updated 020722 from 010608 (D. Strimaitis)
c     - Fix hour array dimension (24 --> 25) for input record so that
c       full day of precip (plus total) is processed
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of current precip.
c                                  input file
c
c --- Parameters used:
c                IO6, MXPF
c
c --- OUTPUT:
c              CDAT - char*42    - A character string storing a TD-3240
c                                  data record for 1 hour
c              LEOF - logical    - End-of-File flag
c
c --- GETPREC called by:  PREAD
c --- GETPREC calls:      none
c-------------------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pmg'
c
      integer index(mxpf),nhr(mxpf)
      character*42 cdat,cdatv(24,mxpf)
      character*12 chour(25)
      character*27 chead
      character*3 cnhr

      logical leof

c --- Set initial values
      data index/mxpf*0/, nhr/mxpf*0/, cnhr/'001'/

c --- Local variables must be remembered
      SAVE

c --- Set pointer for this file from IO unit (see call to UNCDP)
      k=io-6

c --- Increment index for data to be 'read' from arrays this call
      index(k)=index(k)+1

      if(index(k) .GT. nhr(k)) then
c ---    Must read another record from file
         read(io,'(a27,i3,25(a12))',end=999) chead,nhr(k),
     &                                       (chour(i),i=1,nhr(k))
         index(k)=1

c ---    Do not include hour '25' in variable-length records
         if(nhr(k).GT.1) nhr(k)=nhr(k)-1

c ---    Pass information to fixed-record images
         do i=1,nhr(k)
            cdatv(i,k)=chead//cnhr//chour(i)
         enddo
      endif

c --- Return character string for 1 hour
      cdat=cdatv(index(k),k)

      leof=.FALSE.
      return

999   leof=.TRUE.
      return

      end
c----------------------------------------------------------------------
      subroutine fin(itest)
c----------------------------------------------------------------------
c
c --- PMERGE     Version: 5.32     Level: 030402                    FIN
c ---            J. Scire, SRC
c
c --- PURPOSE:  Run termination routine -- compute runtime,
c               write last day processed
c
c --- UPDATE
c --- V5.2-V5.3    030402  (DGS): Add list file unit number to JULDAY
c                                 call
c
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
      include 'params.pmg'
c
      character*8 rtime2
      character*10 rdate2
c
      include 'datehr.pmg'
      include 'qa.pmg'
c
      write(iomesg,*)'TERMINATION PHASE'
c
c --- Write last day/hour processed
      if(ITEST.eq.2)then
c ---    Compute month & day from Julian day
         call GRDAY(io,jyr,jjul,jmo,jday)
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
