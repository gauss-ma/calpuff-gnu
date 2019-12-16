c----------------------------------------------------------------------
c --- PXTRACT -- Precipitation Data Extraction Program
c----------------------------------------------------------------------
c
c --- PXTRACT  Version: 4.25      Level: 070327                    MAIN
c
c --- Original Program Written by:
c                  E. Insley, J. Scire
c
c     Copyright (c) 1996-2007 by Exponent, Inc.
c
c --- PURPOSE:
c      Extracts precipitation data for stations and time periods of
c      interest from a fixed length TD3240 formatted precip file.
c
c      * breaks one large file into a number of smaller station files.
c      * allows stations to be selected based on:
c         1) state code
c         2) station id (state code and station id)
c         3) all stations ( up to 200)
c      * produces a summary table indicating what data are available
c         for the time period selected
c      * produces one output file for each station requested
c
c --- UPDATES:
c
c     V4.24 (060519) to V4.25 (070327) --- (DGS)
c            - CALUTILS from v2.52 Level 060519 to v2.55 Level 070327
c              Move GLOBE1 to COORDLIB
c              Allow negative increments in INCRS
c              Fixed format bug in subroutine BASRUTC for the case of time
c              zone zero (output string was 'UTC+0  0' instead of 'UTC+0000'
c              Modified:  INCRS, UTCBASR, BASRUTC
c              Removed:   GLOBE1
c
c     V4.23 (060309) to V4.24 (060519) --- (DGS)
c            - CALUTILS from v2.51 Level 051019 to v2.52 Level 060519
c              Variable names in control file are not processed
c              correctly if there are too many characters (including
c              blanks) to the left of the "=" sign (run stops in setup
c              phase).
c              Modified:  READIN
c
c     V4.22 (030709) to V4.23 (060309) --- (DGS)
c            - Updated to CALUTILS V2.51 (051019) from V2.2 (0030528)
c            - Filnames changed from c*70 to c*132 (for CALUTILS V2.3
c              and later)
c              Modified:  FILNAM.PXT
c                         READCF, ECHOCF
c
c     V4.21 (030528) to V4.22 (030709) --- (DGS)
c            - Fix type assignment for LCFILES in READCF
c
c     V4.2 (030402) to V4.21 (030528) --- (DGS)
c            - Updated CALUTILS (Version 2.2, Level 030528)
c
c     V4.1 (020828) to V4.2 (030402) --- (DGS)
c            - Updated CALUTILS (Version 2.1, Level 030402)
c            - Replace IOMESG with IO6 in 2nd JULDAY call in FIN
c
c     V4.0 (011003) to V4.1 (020828) --- (DGS)
c            - Updated CALUTILS (Version 1.1, Level 020828)
c
c     V3.0 (010608) to V4.0 (011003) --- (DGS)
c            - Restructure inputs for CALPUFF system control file
c            - Restructure main program as subroutine COMP
c              (subroutine GETDATA is removed)
c            - Place system-wide utilities into an include module
c              (calutil.for)
c
c     V3.0 (010315) to V3.0 (010608) --- (DGS)
c            - Extraction period is by day and variable-length
c              record format is accepted automatically
c
c     V3.0 (981025) to V3.0 (010315) --- (DGS)
c            - Y2K treatment of year enforced
c
c     V3.0 (961113) to V3.0 (981025) --- (DGS)
c            - Remove implicit requirement that states/stations are
c              sorted by ID number
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pxt'
c --- Include common blocks
      include 'qa.pxt'
c
c --- Set version and level number of program (stored in /QA/ and
c --- checked against values set in PARAMS.PXT)
      ver='4.25'
      level='070327'
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
c --- PXTRACT   Version: 4.25           Level: 011003        BLOCK DATA
c               D. Strimaitis, Earth Tech, Inc.
c
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.pxt'
c
c --- Include common blocks
      include 'filnam.pxt'
      include 'control.pxt'

c --- FILNAM common block
      data runinp/'pxtract.inp'/,runlst/'pxtract.lst'/,
     1     precdat/'td3240.dat'/
c --- FILLOG common block
      data lcfiles/.true./

c --- CONTROL common block
      data nsta/0/

      end

c----------------------------------------------------------------------
c --- BRING IN CALPUFF SYSTEM UTILITY SUBROUTINES
      include 'calutils.for'
c----------------------------------------------------------------------

c-----------------------------------------------------------------------
      subroutine setup
c-----------------------------------------------------------------------
c
c --- PXTRACT   Version: 4.25           Level: 011003             SETUP
c               D. Strimaitis, Earth Tech, Inc.
c
c PURPOSE:     SETUP calls routines to read and check the control data
c              provided, it reports the control data to the list file,
c              and it opens the data files if inputs are valid.
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c ---    Common block /FILNAM/ variables:
c           runlst,precdat
c           lcfiles
c ---    Common block /CONTROL/ variables:
c           IBYR,IBMO,IBDY,IEYR,IEMO,IEDY,ibjul,iejul,
c           icode,nsta,istate,istn
c
c        Parameters: IO5, IO6, IOMESG, MVER, MLEVEL
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
c EXTERNAL ROUTINES:  DATETM, COMLINE, READCF, QAINP
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.pxt'
      include 'control.pxt'
      include 'filnam.pxt'
      include 'qa.pxt'

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
10       format(/1x,'ERROR in SUBR. SETUP -- The PXTRACT version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open list file)
      call READCF

c --- Apply QA checks (transfer state/station IDs to character vars)
      call QAINP

c --- Write control data to list-file
      call ECHOCF

c --- Open the TD3240 input data file
      open(io2,FILE=precdat,status='old')

      return
      end
c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- PXTRACT   Version: 4.25           Level: 030709            READCF
c               D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  Read the control file containing the file names of
c               the input and output files of the run, and associated
c               control variables.  Open list file for output.
c
c --- UPDATES:
c
c --- V4.22 (030709) to V4.23 (060309) --- (DGS)
c            - Filenames from c*70 to c*132 for CALUTILS V2.3 and later
c
c --- V4.0 (011003) to V4.22 (030709) --- (DGS)
c            - Fix type assignment for LCFILES
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c ---    Common block /FILNAM/ variables:
c           runinp
c
c        Parameters: IO5, IO6, IOMESG, MXS, MXSTN
c
c --- OUTPUT:
c
c ---    Common block /FILNAM/ variables:
c           runlst,precdat
c           lcfiles
c ---    Common block /CONTROL/ variables:
c           IBYR,IBMO,IBDY,IEYR,IEMO,IEDY,ibjul,iejul,
c           icode,nsta,istate,istn
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE, JULDAY
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.pxt'
      include 'params.cal'
c
c --- Include common blocks
      include 'control.pxt'
      include 'filnam.pxt'
      include 'qa.pxt'
c
c --- Local variables
      character*4 ctemp(132,2)
      character*12 cvdic(mxvar,4)
      integer ivleng(mxvar,4),ivtype(mxvar,4)
      logical lecho

c --- Initialize local variables
      data lecho/.false./
      data names/2/

c --- Set Dictionary

      data cvdic/
     a  'RUNLST','PRECDAT','LCFILES', 57*' ',
     b  'IBYR','IBMO','IBDY','IEYR','IEMO','IEDY',
     b  'ICODE','NSTA', 52* ' ',
     c  'IDSTATE', 59* ' ',
     d  'IDSTN', 59* ' '/

      data ivleng/
     a  2*132,1, 57*0,
     b  8*1, 52*0,
     c  1, 59*0,
     d  1, 59*0/

c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
      data ivtype/
     a  2*4,3, 57*0,
     b  8*2, 52*0,
     c  2, 59*0,
     d  2, 59*0/

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
     1 ctemp(1,1),ctemp(1,2),lcfiles,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum)

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')runlst=' '
      if(ctemp(1,2)(1:1).ne.' ')precdat=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')runlst(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')precdat(j:j)=ctemp(j,2)(1:1)
      enddo

c --- Convert the file names to the proper case
      call FILCASE(lcfiles,runlst)
      call FILCASE(lcfiles,precdat)

c --- Open listfile
      open(io6,file=runlst,status='unknown')

c --- Write banner to list file
      write(io6,5) ver,level
5     format(///,26x,'PXTRACT OUTPUT SUMMARY',/,19x,'VERSION:  ',A12,
     1       ' LEVEL:  ',A12///)

c -----------------
c --- Input Group 1
c -----------------

      call readin(cvdic(1,2),ivleng(1,2),ivtype(1,2),io5,io6,lecho,
     1 IBYR,IBMO,IBDY,IEYR,IEMO,IEDY,ICODE,NSTA,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum)

c --- Compute Julian day for each date
      call JULDAY(io6,ibyr,ibmo,ibdy,ibjul)
      call JULDAY(io6,ieyr,iemo,iedy,iejul)

c --- Set local variable for number of stations/states to read,
c --- conditioned to comply with array dimensions
      ns=MAX(0,nsta)
      if(icode.EQ.1) then
         ns=MIN(mxs,ns)
      elseif(icode.EQ.2) then
         ns=MIN(mxstn,ns)
      endif

c -----------------
c --- Input Group 2
c -----------------

c --- Group should be present only when ICODE=1

      if(icode.EQ.1) then
         do i=1,ns
         call readin(cvdic(1,3),ivleng(1,3),ivtype(1,3),io5,io6,lecho,
     1    ISTATE(i),
     2    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
         enddo
      endif

c -----------------
c --- Input Group 3
c -----------------

c --- Group should be present only when ICODE=2

      if(icode.EQ.2) then
         do i=1,ns
         call readin(cvdic(1,4),ivleng(1,4),ivtype(1,4),io5,io6,lecho,
     1    ISTN(i),
     2    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
         enddo
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine qainp
c----------------------------------------------------------------------
c
c --- PXTRACT   Version: 4.25           Level: 011003             QAINP
c               D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  QA the control file information, and also transfer
c               state/station IDs from integer to character arrays
c               while checking for bad entries.
c
c --- INPUTS:
c
c ---    Common block /CONTROL/ variables:
c           IBYR,IBMO,IBDY,IEYR,IEMO,IEDY,ibjul,iejul,
c           icode,nsta,istate(mxs),istn(mxs)
c
c        Parameters: IO6, MXS, MXSTN
c
c --- OUTPUT:
c
c ---    Common block /CONTROL/ variables:
c           ibeg,iend,state(mxs),stn(mxs)
c
c --- QAINP called by:  SETUP
c --- QAINP calls:      QAYR4, YR4
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.pxt'
c
c --- Include common blocks
      include 'control.pxt'
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
c --- MERGE YEAR AND JULIAN DAY INTO ONE PARAMETER
      ibeg = ibyr * 1000 + ibjul
      iend = ieyr * 1000 + iejul
      if(ibeg.GT.iend)then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 1'
         write(io6,*) 'Starting date is after ending date'
         write(io6,*) 'IBEG = ',ibeg
         write(io6,*) 'IEND = ',iend
         lerrcf=.TRUE.
      endif

c --- Check for an invalid value of ICODE
      if(icode.LT.1 .OR. icode.GT.3)then
         write(io6,*)
         write(io6,*) 'QAINP:  Error in Input Group 1'
         write(io6,*) 'ICODE out of range     = ',icode
         write(io6,*) 'ICODE must be 1, 2, or 3 '
         lerrcf=.TRUE.
      endif

c --- Check for an invalid value of NSTA
      if(icode.EQ.1) then
         if(nsta.LT.1 .OR. nsta.GT.MXS)then
            write(io6,*)
            write(io6,*) 'QAINP:  Error in Input Group 1'
            write(io6,*) 'NSTA out of range       = ',nsta
            write(io6,*) 'Number of states allowed is 1 to MXS'
            write(io6,*) 'MXS                     = ',mxs
            lerrcf=.TRUE.
            nsta=MIN(mxs,nsta)
         endif
      elseif(icode.EQ.2) then
         if(nsta.LT.1 .OR. nsta.GT.MXSTN)then
            write(io6,*)
            write(io6,*) 'QAINP:  Error in Input Group 1'
            write(io6,*) 'NSTA out of range       = ',nsta
            write(io6,*) 'Number of stations allowed is 1 to MXSTN'
            write(io6,*) 'MXSTN                   = ',mxstn
            lerrcf=.TRUE.
            nsta=MIN(mxstn,nsta)
         endif
      endif

c --- Check for invalid codes (translate to characters)
      if(icode.EQ.1)then
c ---   STATE CODES
        do j=1,nsta
          write(state(j),'(i2)') istate(j)
          do i=1,2
            if(state(j)(i:i) .EQ. ' ')then
              state(j)(i:i) = '0'
              if(i.NE.1) then
                write(io6,*)
                write(io6,*) 'QAINP:  Error in Input Group 2'
                write(io6,*) 'Incorrect state code    = ',istate(j)
                lerrcf=.TRUE.
              endif
            endif
          enddo
        enddo
      elseif(icode.EQ.2)then
c ---   STATION CODES
        do j=1,nsta
          write(stn(j),'(i6)') istn(j)
          do i=1,6
            if(stn(j)(i:i).EQ.' ')then
              stn(j)(i:i) = '0'
              if(i.NE.1) then
                write(io6,*)
                write(io6,*) 'QAINP:  Error in Input Group 3'
                write(io6,*) 'Incorrect station code  = ',istn(j)
                lerrcf=.TRUE.
              endif
            endif
          enddo
        enddo
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
      subroutine echocf
c----------------------------------------------------------------------
c
c --- PXTRACT   Version: 4.25           Level: 060309            ECHOCF
c               D. Strimaitis, Earth Tech, Inc.
c
c     Portions extracted from subroutine GETDATA
c               E. Insley, SRC
c
c --- PURPOSE:  Echo the control file information to the list file
c
c --- UPDATES:
c
c --- V4.0 (011003) to V4.23 (060309) --- (DGS)
c            - Filenames from c*70 to c*132 for CALUTILS V2.3 and later
c
c --- INPUTS:
c
c ---    Common block /CONTROL/ variables:
c           IBYR,IBMO,IBDY,IEYR,IEMO,IEDY,ibjul,iejul,
c           icode,nsta,
c           state(mxs),stn(mxstn)
c ---    Common block /FILNAM/ variables:
c           runinp,runlst,precdat
c
c        Parameters: IO6, MXS, MXSTN
c
c --- OUTPUT:   none
c
c
c --- ECHOCF called by:  SETUP
c --- ECHOCF calls:      
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.pxt'
c
c --- Include common blocks
      include 'control.pxt'
      include 'filnam.pxt'

c --- Local variables
      integer ishift(3)

      write(io6,6042) runinp,precdat,runlst
6042  format(//1x,'FILENAMES: '/
     1 5x,'Control file:          ',a132/
     2 5x,'Input TD3240 file:     ',a132/
     3 5x,'Output list file:      ',a132)

      if(icode.EQ.1)then
        write(io6,50) ibmo,ibdy,ibyr,iemo,iedy,ieyr
50      format(//2x,'Data Requested ','by State Code',//,2x,
     1         'Period to Extract:   ',i2,'/',i2,'/',i4,'  to  ',i2,
     2         '/',i2,'/',i4)
      elseif(icode.EQ.2) then
        write(io6,51) ibmo,ibdy,ibyr,iemo,iedy,ieyr
51      format(//,2x,'Data Requested ','by Station ID',//,2x,
     1         'Period to Extract:   ',i2,'/',i2,'/',i4,'  to  ',i2,
     2         '/',i2,'/',i4)
      else
        write(io6,53) ibmo,ibdy,ibyr,iemo,iedy,ieyr
53      format(//,2x,'Data Requested for all Stations',//,2x,
     1         'Period to Extract:   ',i2,'/',i2,'/',i4,'  to  ',i2,
     2         '/',i2,'/',i4)
        return
      endif
C  WRITE OUT SORTED DATA IN COLUMNS
C  J4 IS NO. ROWS IN A "SHORT" COLUMN
C  J5 IS NO. ROWS IN A "LONG" COLUMN
C  J6 IS THE NUMBER OF "LONG" COLUMNS
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
C  WRITE OUT EITHER STATE CODES OR STATION CODES
      if(icode.EQ.1)then
       write(io6,60) (' ',k=1,ncol)
60     format( //2x,'Requested State Codes --- :',
     1         //,3x,4(a1,'No.',6x,'ID',9x)/)
       do i=1,j4
         i2 = i + ishift(1)
         i3 = i + ishift(2)
         i4 = i + ishift(3)
         write(io6,70) i,state(i),i2,state(i2),i3,state(i3),i4,
     1               state(i4)
70       format(3x,4(i3,3x,a6,9x))
       enddo
       if(j6.GT.0)then
         n1 = j5
         n2 = n1+(j6-1)*j5
         write(io6,70) (k,state(k),k=n1,n2,j5)
       endif
      else
       if(icode.EQ.2)then
        write(io6,61) (' ',k=1,ncol)
61      format( //2x,'Requested Precipitation Station ID Numbers -- ',
     1        ':',//,3x,4(a1,'No.',4x,'ID',11x)/)
        do i=1,j4
          i2 = i + ishift(1)
          i3 = i + ishift(2)
          i4 = i + ishift(3)
          write(io6,71) i,stn(i),i2,stn(i2),i3,stn(i3),i4,stn(i4)
71        format(3x,4(i3,3x,a6,9x))
        enddo
        if(j6.GT.0)then
          n1 = j5
          n2 = n1+(j6-1)*j5
          write(io6,71) (k,stn(k),k=n1,n2,j5)
        endif
       endif
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine comp
c----------------------------------------------------------------------
c
c --- PXTRACT   Version: 4.25           Level: 011003              COMP
c               D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  Main computational routine
c
c --- INPUTS:
c
c       Common block /CONTROL/ variables:
c           icode
c
c       Parameters: IOMESG
c
c --- OUTPUT:  none
c
c --- COMP called by:  MAIN
c --- COMP calls:      RDSTATE, RDSTN, RDALL
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pxt'

c --- Include common blocks
      include 'control.pxt'

c --- Report progress
      write(iomesg,*)'COMPUTATIONAL PHASE'

      if(icode.EQ.1) then
         call rdstate
      elseif(icode.EQ.2) then
         call rdstn
      else
         call rdall
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine rdstn
c----------------------------------------------------------------------
c
c --- PXTRACT  Version: 4.25      Level: 011003                   RDSTN
c              E. Insley, SRC
c
c --- Reads the large precipitation data file and matches station
c --- numbers and time periods to the user inputs and writes out the
c --- records that match. Data for each station is written to a
c --- separate output file.
c
c --- UPDATES:
c     V3.0 (010608) to V4.0 (011003) --- (DGS)
c            - Inputs provided by included params and /control/ files
c     V3.0 (010315) to V3.0 (010608) --- (DGS)
c            - Extraction period is by day and variable-length
c              record format is accepted automatically
c     V3.0 (981025) to V3.0 (010315) --- (DGS)
c            - Read and keep 4-digit year
c     V3.0 (961113) to V3.0 (981025) --- (DGS)
c            - Remove implicit requirement that stations are sorted
c              by ID number
c
c --- INPUTS:
c       Common block /CONTROL/ variables:
c           nsta,stn(mxstn),
c           ibyr,ibeg,ieyr,iend
c
c       Parameters: IO6, IO2, MXS, MXSTN
c
c --- INDEXES AND FLAGS:
c         I - integer     - Station id count (index)
c         K - integer     - Output file unit numbers
c      INEW - integer     - Flag for new station (new output file)
c    IDATEF - integer     - Flag indicating start date status for
c                           new station
c                           IDATEF = 0 a start date for this station
c                                      has not been determined
c                           IDATEF = 1 a start date for this station
c                                      has already been determined
c      LREC - integer     - Flag indicating status of next record after
c                           the requested time period for each station
c                           LREC = 0 not written yet
c                           LREC = 1 already written for this station
c     NFLAG - integer     - Indicator for nonblank flag at end of data
c                           NFLAG = 0 blank flag
c                           NFLAG = 1 nonblank flag
c
c --- RDSTN called by:  COMP
c --- RDSTN calls:   JULDAY
c                    GRDAY
c                    FLAGCHK
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pxt'

c --- Include common blocks
      include 'control.pxt'

      character*2 bflag, eflag, oldflag
      character*6 stn1, isave(mxstn), oldstn, stn0
      character*10 cfile
      character*12 chour(25),chourold(25)
      character*27 chead,cheadold
      character*3 cnhr,cnhrold

      integer nrec(mxstn),isdate(mxstn),iedate(mxstn)
      logical lstn, lcurrent

C   INITIALIZE COUNTERS FOR NUMBER OF RECORDS, DATES, AND DAYS/STATION
      do j=1,nsta
        nrec(j) = 0
        isdate(j) = 0
        iedate(j) = 0
      enddo

C   I = STATION INDEX
C   K = FILE UNIT NUMBER
C   K0 = Increment added to I to obtain file unit number K
      i = 0
      k0 = 6
      ku = k0 + i
      inew = 0
      idatef = 0
      jolddate = 0
      oldflag = '  '
      oldstn = '      '
      stn0 = '      '

C   READ IN THE PRECIPITATION DATA RECORD
1     read(io2,'(a27,i3,25(a12))',end=99) chead,nhr,(chour(n),n=1,nhr)
      read(chead(4:9),'(a6)') stn1

c --- Place number of hours into character A3 variable
      cnhr='000'
      if(nhr.GT.9) then
        write(cnhr(2:3),'(i2)') nhr
      else
        write(cnhr(3:3),'(i1)') nhr
      endif

c --- Transfer flags at beginning and end of record
      read(chour(1)(11:12),'(a2)') bflag
      call FLAGCHK(bflag)
      eflag=bflag
      if(nhr.GT.1) then
         read(chour(nhr-1)(11:12),'(a2)') eflag
         call FLAGCHK(eflag)
      endif

c --- Set date for this record
      read(chead(18:21),'(i4)') jyr
      read(chead(22:23),'(i2)') jmo
      read(chead(26:27),'(i2)') jday
      call julday(io6,jyr,jmo,jday,juldy)
      jdate = jyr * 1000 + juldy

C   CHECK THE STATION ID
c --- Use LCURRENT to signal if the station just read is the current
c --- station being processed
29    lcurrent=.FALSE.
      if(i.GT.0) then
         if(stn1.EQ.stn0) lcurrent=.TRUE.
      endif
      if(.not.LCURRENT) then
c ---    Station on this record is NOT the current station
c ---    Check to see if this station is on the list
         lstn=.FALSE.
         do is=1,nsta
            if(stn1.EQ.stn(is)) then
               lstn=.TRUE.
               i=is
               stn0=stn1
            endif
         enddo
         if(LSTN) then
c ---       Check for date before or within the extraction date-range
            if(jdate.LE.iend) then
c ---          New station is OK
c ---          Initialize 'new station' flags
               idatef = 0
               inew = 0
               lrec = 0
               oldflag = '  '
c ---          Close file for last station
               if(ku.GT.k0) close(ku)
c ---          Open output file for new station
               ku=k0+i
               cfile(1:6) = stn(i)
               cfile(7:10) = '.dat'
               open(ku,file=cfile,status='unknown')
            else
c ---          Reject this record (Date beyond range)
               goto 1
            endif
         else
c ---       Reject this record (Station not on list)
            goto 1
         endif
      endif

c --- Check for date within the extraction date-range
      if(jdate.LT.ibeg) then
c ---    Date is before range; save content for potential use
         nhrold=nhr
         cheadold=chead
         cnhrold=cnhr
         do n=1,nhr
            chourold(n)=chour(n)
         enddo
         oldflag = eflag
      elseif(jdate.GT.iend) then
c ---    Date is after range
c ---    Check to see if it is the next record for this station
c ---    If so, write record and set new end-date
         if(lrec.EQ.0 .AND. LCURRENT)then
            write(ku,'(a27,a3,25(a12))') chead,cnhr,
     &                                  (chour(n),n=1,nhr)
            nrec(i) = nrec(i) + 1
            iedate(i) = jdate
            if(eflag.EQ.'  ') lrec = 1
         endif
      else
c ---    Record within period
         if(nrec(i).EQ.0) then
c ---       No records have been written yet for this station
            if(inew.GT.0) then
               write(*,*)'FATAL ERROR in Subr. RDSTN'
               write(*,*)'Bad internal information about new station'
               write(*,*)'Both NREC(i) and INEW should be 0'
               write(*,*)'i, NREC(i), INEW = ',i,nrec(i),inew
               write(*,'(a27,a3,25(a12))') chead,cnhr,
     &                                    (chour(n),n=1,nhr)
               stop
            endif

c ---       First record may start 'late', or may have a flag that
c ---       needs to be paired with one in a previous record; so
c ---       always write out record that preceeds start of period
c ---       if one exists
            read(cheadold(4:9),'(a6)') oldstn
            if(oldstn.EQ.stn(i))then
               nrec(i) = nrec(i) + 1
               write(ku,'(a27,a3,25(a12))') cheadold,cnhrold,
     &                                     (chourold(n),n=1,nhrold)
               inew = 1
               read(cheadold(18:21),'(i4)') iyr
               read(cheadold(22:23),'(i2)') imo
               read(cheadold(26:27),'(i2)') iday
               call julday(io6,iyr,imo,iday,ijuldy)
               jolddate = iyr * 1000 + ijuldy
               isdate(i) = jolddate
               idatef = 1
            endif
         endif
c ---    Now, process current record
         write(ku,'(a27,a3,25(a12))') chead,cnhr,
     &                               (chour(n),n=1,nhr)
         nrec(i) = nrec(i) + 1
         iedate(i) = jdate
         oldstn = stn1
         nhrold=nhr
         cheadold=chead
         cnhrold=cnhr
         do n=1,nhr
            chourold(n)=chour(n)
         enddo
         jolddate = jdate
         oldflag = eflag
C  SAVE THE START DATE, UNLESS A START DATE HAS ALREADY BEEN DETERMINED
C  FOR THIS STATION (idatef = 1) AND AN END DATE FOR EACH STATION
         if(idatef.EQ.0)then
            isdate(i) = jdate
            iedate(i) = isdate(i)
            idatef = 1
         else
            iedate(i) = jdate
         endif
      endif
      go to 1

99    write(io6,40)
40    format(///,2x,'Station',6x,'Starting',5x,'Ending',7x,
     1       'No. of',/,4x,'Code',9x,'Date',8x,'Date',8x,'Records',/)
      nf = 0
      m = 0
      do j=1,nsta
         if(isdate(j).EQ.0 .AND.iedate(j).EQ.0)then
            m = m + 1
            isave(m) = stn(j)
            nf = 1
         else
            jsyr = isdate(j)/1000
            jeyr = iedate(j)/1000
            jsjul = MOD(isdate(j),1000)
            jejul = MOD(iedate(j),1000)
            call GRDAY(io6,jsyr,jsjul,jsmo,jsday)
            call GRDAY(io6,jeyr,jejul,jemo,jeday)
            write(io6,50) stn(j),jsmo,jsday,jsyr,jemo,jeday,jeyr,
     1                    nrec(j)
50          format(2x,a6,7x,2(i2,'/'),i4,2x,2(i2,'/'),i4,2x,i5)
         endif
      enddo
      if(nf.EQ.1) then
         write(io6,60)
         write(io6,70) (isave(n),n=1,m)
      endif
60    format(///2x,'The following stations were not found in the ',
     1      'precipitation data file',/ '  for the requested time ',
     2       'period: ')
70    format(2x,a6)

      return
      end
c----------------------------------------------------------------------
      subroutine rdstate
c----------------------------------------------------------------------
c
c --- PXTRACT  Version: 4.25      Level: 011003                 RDSTATE
c              E. Insley, SRC
c
c --- Reads the large precipitation data file and matches the state
c --- codes and time periods to the user inputs and writes out the
c --- records that match.  Data for each station is written out to
c --- a separate output file.
c
c --- UPDATES:
c     V3.0 (010608) to V4.0 (011003) --- (DGS)
c            - Inputs provided by included params and /control/ files
c     V3.0 (010315) to V3.0 (010608) --- (DGS)
c            - Extraction period is by day and variable-length
c              record format is accepted automatically
c     V3.0 (981025) to V3.0 (010315) --- (DGS)
c            - Read and keep 4-digit year
c     V3.0 (961113) to V3.0 (981025) --- (DGS)
c            - Remove implicit requirement that states are sorted
c              by ID number
c
c --- INPUTS:
c       Common block /CONTROL/ variables:
c           nsta,state(mxstn)
c           ibyr,ibeg,ieyr,iend
c
c       Parameters: IO6, IO2, MXS, MXSTN
c
c --- INDEXES AND FLAGS:
c               I - integer     - State code index
c               K - integer     - Station code index
c              K0 - integer     - Increment added to station code index
c                                 to identify file unit
c            INEW - integer     - Flag for new station (new output file)
c          IDATEF - integer     - Flag indicating start date status for
c                                 new station
c                                 IDATEF = 0 a start date for this station
c                                            has not been determined
c                                 IDATEF = 1 a start date for this station
c                                            has already been determined
c            LREC - integer     - Flag indicating status of next record after
c                                 the requested time period for each station
c                                 LREC = 0 not written yet
c                                 LREC = 1 already written for this station
c           NFLAG - integer     - Indicator for nonblank flag at end of data
c                                 NFLAG = 0 blank flag
c                                 NFLAG = 1 nonblank flag
c
c --- VARIABLE NAMES FOR DATES:
c          ISDATE - integer     - Array of start dates for each station
c          IEDATE - integer     - Array of end dates for each station
c          IBDATE - integer     - Array of start dates for each state
c          ILDATE - integer     - Array of end dates for each state
c
c --- RDSTATE called by:  COMP
c --- RDSTATE calls:  GRDAY
c                     JULDAY
c                     FLAGCHK
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pxt'

c --- Include common blocks
      include 'control.pxt'
c
      character*2 isave(mxs), bflag, eflag, oldflag
      character*6 oldstn
      character*10 cfile

      character*12 chour(25),chourold(25)
      character*27 chead,cheadold
      character*3 cnhr,cnhrold

      integer nrec(mxstn),isdate(mxstn),iedate(mxstn),
     1        nstn(mxs),ibdate(mxs),ldate(mxs)

      logical lstate, lcurrent


C   INITIALIZE COUNTERS FOR NUMBER OF STATIONS, RECORDS AND DATES
      do j=1,nsta
        nstn(j) = 0
        ibdate(j) = 0
        ldate(j) = 0
      enddo
      do j=1,mxstn
        nrec(j) = 0
        isdate(j) = 0
        iedate(j) = 0
      enddo

C   I = STATE CODE INDEX
C   K = STATION CODE INDEX
C   K0 = Increment added to K to obtain file unit number
      i = 1
      k = 0
      k0 = 6
      inew = 0
      iflg = 0
      idatef = 0
      jolddate = 0
      oldstn = '      '
      oldflag = '  '

C   READ IN A PRECIPITATION DATA RECORD
1     read(io2,'(a27,i3,25(a12))',end=99) chead,nhr,(chour(n),n=1,nhr)

c --- Place number of hours into character A3 variable
      cnhr='000'
      if(nhr.GT.9) then
        write(cnhr(2:3),'(i2)') nhr
      else
        write(cnhr(3:3),'(i1)') nhr
      endif

c --- Transfer flags at beginning and end of record
      read(chour(1)(11:12),'(a2)') bflag
      call FLAGCHK(bflag)
      eflag=bflag
      if(nhr.GT.1) then
         read(chour(nhr-1)(11:12),'(a2)') eflag
         call FLAGCHK(eflag)
      endif

c --- Set the date
      read(chead(18:21),'(i4)') jyr
      read(chead(22:23),'(i2)') jmo
      read(chead(26:27),'(i2)') jday
      call julday(io6,jyr,jmo,jday,juldy)
      jdate = jyr * 1000 + juldy

C   CHECK THE STATE/STATION ID
c --- Use LCURRENT to signal if the station just read is the current
c --- station being processed
29    lcurrent=.FALSE.
      if(k.GT.0) then
         if(chead(4:9).EQ.stn(k)) lcurrent=.TRUE.
      endif
      if(.not.LCURRENT) then
c ---    Station on this record is NOT the current station
c ---    Check to see if state for this record is on the list
         lstate=.FALSE.
         do is=1,nsta
            if(chead(4:5).EQ.state(is)) then
               lstate=.TRUE.
               i=is
            endif
         enddo
         if(LSTATE) then
c ---       Check for date before or within the extraction date-range
            if(jdate.LE.iend) then
c ---          Add station
               nstn(i) = nstn(i) + 1
               k=k+1
               ku=k0+k
               read(chead(4:9),'(a6)') stn(k)
c ---          Initialize 'new station' flags
               idatef = 0
               inew = 0
               lrec = 0
               iflg = 0
               oldflag = '  '
c ---          Open output file (close old file)
               cfile(1:6) = stn(k)
               cfile(7:10) = '.dat'
               open(ku,file=cfile,status='unknown')
               if(k.GT.1) close(ku-1)
            else
c ---          Reject this record
               goto 1
            endif
         else
c ---       Reject this record
            goto 1
         endif
      endif

c --- Check for date within the extraction date-range
      if(jdate.LT.ibeg) then
c ---    Date is before range; save content for potential use
         nhrold=nhr
         cheadold=chead
         cnhrold=cnhr
         do n=1,nhr
            chourold(n)=chour(n)
         enddo
         oldflag = eflag
      elseif(jdate.GT.iend) then
c ---    Date is after range
c ---    Check to see if it is the next record for this station
c ---    If so, write record and set new end-date
         if(lrec.EQ.0 .AND. LCURRENT)then
            write(ku,'(a27,a3,25(a12))') chead,cnhr,
     &                                  (chour(n),n=1,nhr)
            nrec(k) = nrec(k) + 1
            iedate(k) = jdate
            if(ldate(i).LE.iedate(k)) ldate(i) = iedate(k)
            if(eflag.EQ.'  ') lrec = 1
         endif
      else
c ---    Record within period
         if(nrec(k).EQ.0) then
c ---       No records have been written yet for this station
            if(inew.GT.0) then
               write(*,*)'FATAL ERROR in Subr. RDSTATE'
               write(*,*)'Bad internal information about new station'
               write(*,*)'Both NREC(k) and INEW should be 0'
               write(*,*)'k, NREC(k), INEW = ',k,nrec(k),inew
               write(*,'(a27,a3,25(a12))') chead,cnhr,
     &                                    (chour(n),n=1,nhr)
               stop
            endif

c ---       First record may start 'late', or may have a flag that
c ---       needs to be paired with one in a previous record; so
c ---       always write out record that preceeds start of period
c ---       if one exists
            read(cheadold(4:9),'(a6)') oldstn
            if(oldstn.EQ.stn(k))then
               nrec(k) = nrec(k) + 1
               write(ku,'(a27,a3,25(a12))') cheadold,cnhrold,
     &                                     (chourold(n),n=1,nhrold)
               inew = 1
               read(cheadold(18:21),'(i4)') iyr
               read(cheadold(22:23),'(i2)') imo
               read(cheadold(26:27),'(i2)') iday
               call julday(io6,iyr,imo,iday,ijuldy)
               jolddate = iyr * 1000 + ijuldy
               isdate(k) = jolddate
               ibdate(i) = isdate(k)
               idatef = 1
            endif
         endif
c ---    Now, process current record
         write(ku,'(a27,a3,25(a12))') chead,cnhr,
     &                               (chour(n),n=1,nhr)
         nrec(k) = nrec(k) + 1
         iedate(k) = jdate
         oldstn = stn(k)
         nhrold=nhr
         cheadold=chead
         cnhrold=cnhr
         do n=1,nhr
            chourold(n)=chour(n)
         enddo
         jolddate = jdate
         oldflag = eflag
C  SAVE THE START DATE, UNLESS A START DATE HAS ALREADY BEEN DETERMINED
C  FOR THIS STATION (idatef = 1) AND AN END DATE FOR EACH STATION
         if(idatef.EQ.0)then
            isdate(k) = jdate
            iedate(k) = isdate(k)
            idatef = 1
         else
            iedate(k) = jdate
         endif
         if(iflg.EQ.0)then
            ibdate(i) = isdate(k)
            ldate(i) = iedate(k)
            iflg = 1
         endif
         if(isdate(k).LE.ibdate(i)) ibdate(i) = isdate(k)
         if(ldate(i).LE.iedate(k)) ldate(i) = iedate(k)
      endif
      go to 1

99    write(io6,40)
40    format(///,2x,'  State',6x,'Starting',5x,'Ending',7x,
     1       'No. of',/,4x,'Code',9x,'Date',8x,'Date',8x,'Stations',/)
      nf = 0
      nstntot =0
      m = 0
      do j=1,nsta
         if(ibdate(j).EQ.0 .AND.ldate(j).EQ.0)then
            m = m + 1
            isave(m) = state(j)
            nf = 1
         else
            jbyr = ibdate(j)/1000
            jlyr = ldate(j)/1000
            jbjul = MOD(ibdate(j),1000)
            jljul = MOD(ldate(j),1000)
            call GRDAY(io6,jbyr,jbjul,jbmo,jbday)
            call GRDAY(io6,jlyr,jljul,jlmo,jlday)
            write(io6,50) state(j),jbmo,jbday,jbyr,jlmo,jlday,jlyr,
     1                    nstn(j)
50          format(4x,a2,9x,2(i2,'/'),i4,2x,2(i2,'/'),i4,2x,i5)
            nstntot = nstntot + nstn(j)
         endif
      enddo
      if(nf.EQ.1) then
         write(io6,60)
         write(io6,70) (isave(n),n=1,m)
      endif
60    format(///2x,'The following state codes were not found in the ',
     1      'precipitation data file',/ '  for the requested time ',
     2       'period: ')
70    format(2x,a2)
      if(nstntot.GT.0)then
         write(io6,80)
80       format(///,2x,'Station',6x,'Starting',5x,'Ending',7x,
     1         'No. of',/,4x,'ID',11x,'Date',8x,'Date',8x,'Records',/)
         do j=1, nstntot
            jsyr = isdate(j)/1000
            jeyr = iedate(j)/1000
            jsjul = MOD(isdate(j),1000)
            jejul = MOD(iedate(j),1000)
            call GRDAY(io6,jsyr,jsjul,jsmo,jsday)
            call GRDAY(io6,jeyr,jejul,jemo,jeday)
            write(io6,90) stn(j),jsmo,jsday,jsyr,jemo,jeday,jeyr,
     1                    nrec(j)
90          format(2x,a6,7x,2(i2,'/'),i4,2x,2(i2,'/'),i4,2x,i5)
         enddo
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine rdall
c----------------------------------------------------------------------
c
c --- PXTRACT  Version: 4.25      Level: 010608                   RDALL
c ---          E. Insley, SRC
c
c --- Reads the large precipitation data file and writes out all the
c --- data for a given time period.  There is one output file per
c --- station. Missing data and Accumulation period data is always
c --- matched, regardless of the time period requested. The record
c --- prior to and immediately after the time period requested is
c --- included.
c
c --- UPDATES:
c     V3.0 (010608) to V4.0 (011003) --- (DGS)
c            - Inputs provided by included params and /control/ files
c     V3.0 (010315) to V3.0 (010608) --- (DGS)
c            - Extraction period is by day and variable-length
c              record format is accepted automatically
c     V3.0 (961113) to V3.0 (010315) --- (DGS)
c            - Y2K treatment of year enforced
c
c --- INPUTS:
c       Common block /CONTROL/ variables:
c           ibeg,iend
c
c       Parameters: IO6, IO2, MXSTN
c
c --- INDEXES AND FLAGS:
c          I - integer    - Station number count (index)
c          K - integer    - Output file unit numbers
c       INEW - integer    - Flag for new station (new output file)
c     IDATEF - integer    - Flag indicating start date status for
c                           a new station
c                           IDATEF = 0  a start date for this station
c                                       has not been determined yet
c                           IDATEF = 1  a start date for this station
c                                       has already been determined
c       LREC - integer    - Flag indicating status of next record after
c                           the requested time period for each station
c                           LREC = 0  not written yet
c                           LREC - 1  already written for this station
c      NFLAG - integer    - Indicator for nonblank flag at end of data
c                           NFLAG = 0 blank flag
c                           NFLAG = 1 nonblank flag
c
c --- RDALL called by:  COMP
c --- RDALL calls:  JULDAY
c                   GRDAY
c                   YR4
c                   QAYR4
c                   FLAGCHK
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pxt'

c --- Include common blocks
      include 'control.pxt'
c
      character*2 bflag, eflag, oldflag
      character*6 stn1(mxstn), oldstn1
      character*10 cfile

      character*12 chour(25),chourold(25)
      character*27 chead,cheadold
      character*3 cnhr,cnhrold

      integer icount(mxstn),isdate(mxstn),iedate(mxstn)
      logical lcurrent

C  INITIALIZE COUNTERS FOR NUMBER OF RECORDS AND DATES PER STATION
      do 3, j=1,mxstn
        icount(j) = 0
        isdate(j) = 0
        iedate(j) = 0
3     continue

C   I = STATION INDEX
C   K = FILE UNIT NUMBER
C   K0 = Increment added to I to obtain file unit number K
      i = 0
      k0 = 6
      inew = 0
      idatef = 0
      jolddate = 0
      oldflag = '  '
      oldstn1 = '      '

C   READ IN THE PRECIPITATION DATA
1     read(io2,'(a27,i3,25(a12))',end=99) chead,nhr,(chour(n),n=1,nhr)

c --- Place number of hours into character A3 variable
      cnhr='000'
      if(nhr.GT.9) then
        write(cnhr(2:3),'(i2)') nhr
      else
        write(cnhr(3:3),'(i1)') nhr
      endif

c --- Transfer flags at beginning and end of record
      read(chour(1)(11:12),'(a2)') bflag
      call FLAGCHK(bflag)
      eflag=bflag
      if(nhr.GT.1) then
         read(chour(nhr-1)(11:12),'(a2)') eflag
         call FLAGCHK(eflag)
      endif

c --- Set the date
      read(chead(18:21),'(i4)') jyr
      read(chead(22:23),'(i2)') jmo
      read(chead(26:27),'(i2)') jday
      call julday(io6,jyr,jmo,jday,juldy)
      jdate = jyr * 1000 + juldy

C   CHECK THE STATION ID
c --- Use LCURRENT to signal if the station just read is the current
c --- station being processed
29    lcurrent=.FALSE.
      if(i.GT.0) then
         if(chead(4:9).EQ.stn1(i)) lcurrent=.TRUE.
      endif
      if(.not.LCURRENT) then
c ---    Station on this record is NOT the current station
c ---    Check for date before or within the extraction date-range
         if(jdate.LE.iend) then
c ---       Add station
            i=i+1
            ku=k0+i
            read(chead(4:9),'(a6)') stn1(i)
c ---       Initialize 'new station' flags
            idatef = 0
            inew = 0
            lrec = 0
            oldflag = '  '
c ---       Open output file (close old file)
            cfile(1:6) = stn1(i)
            cfile(7:10) = '.dat'
            open(ku,file=cfile,status='unknown')
            if(i.GT.1) close(ku-1)
         else
c ---       Reject this record
            goto 1
         endif
      endif

c --- Check for date within the extraction date-range
      if(jdate.LT.ibeg) then
c ---    Date is before range; save content for potential use
         nhrold=nhr
         cheadold=chead
         cnhrold=cnhr
         do n=1,nhr
            chourold(n)=chour(n)
         enddo
         oldflag = eflag
      elseif(jdate.GT.iend) then
c ---    Date is after range
c ---    Check to see if it is the next record for this station
c ---    If so, write record and set new end-date
         if(lrec.EQ.0 .AND. LCURRENT)then
            write(ku,'(a27,a3,25(a12))') chead,cnhr,
     &                                  (chour(n),n=1,nhr)
            icount(i) = icount(i) + 1
            iedate(i) = jdate
            if(eflag.EQ.'  ') lrec = 1
         endif
      else
c ---    Record within period
         if(icount(i).EQ.0) then
c ---       No records have been written yet for this station
            if(inew.GT.0) then
               write(*,*)'FATAL ERROR in Subr. RDALL'
               write(*,*)'Bad internal information about new station'
               write(*,*)'Both ICOUNT(i) and INEW should be 0'
               write(*,*)'i, ICOUNT(i), INEW = ',i,icount(i),inew
               write(*,'(a27,a3,25(a12))') chead,cnhr,
     &                                    (chour(n),n=1,nhr)
               stop
            endif

c ---       First record may start 'late', or may have a flag that
c ---       needs to be paired with one in a previous record; so
c ---       always write out record that preceeds start of period
c ---       if one exists
            read(cheadold(4:9),'(a6)') oldstn1
            if(oldstn1.EQ.stn1(i))then
               icount(i) = icount(i) + 1
               write(ku,'(a27,a3,25(a12))') cheadold,cnhrold,
     &                                     (chourold(n),n=1,nhrold)
               inew = 1
               read(cheadold(18:21),'(i4)') iyr
               read(cheadold(22:23),'(i2)') imo
               read(cheadold(26:27),'(i2)') iday
               call julday(io6,iyr,imo,iday,ijuldy)
               jolddate = iyr * 1000 + ijuldy
               isdate(i) = jolddate
               idatef = 1
            endif
         endif
c ---    Now, process current record
         write(ku,'(a27,a3,25(a12))') chead,cnhr,
     &                               (chour(n),n=1,nhr)
         icount(i) = icount(i) + 1
         iedate(i) = jdate
         oldstn1 = stn1(i)
         nhrold=nhr
         cheadold=chead
         cnhrold=cnhr
         do n=1,nhr
            chourold(n)=chour(n)
         enddo
         jolddate = jdate
         oldflag = eflag
C  SAVE THE START DATE, UNLESS A START DATE HAS ALREADY BEEN DETERMINED
C  FOR THIS STATION (idatef = 1) AND AN END DATE FOR EACH STATION
         if(idatef.EQ.0)then
            isdate(i) = jdate
            iedate(i) = isdate(i)
            idatef = 1
         else
            iedate(i) = jdate
         endif
      endif
      go to 1

99    write(io6,60)
60    format(///,2x,'Station',6x,'Starting',5x,'Ending',7x,
     1       'No. of',/,4x,'ID',11x,'Date',8x,'Date',8x,'Records',/)
C  WRITE OUT SUMMARY TABLE INFORMATION
      do 35 j=1,i-1
        jsyr = isdate(j)/1000
        jeyr = iedate(j)/1000
        jsjul = MOD(isdate(j),1000)
        jejul = MOD(iedate(j),1000)
        call GRDAY(io6,jsyr,jsjul,jsmo,jsday)
        call GRDAY(io6,jeyr,jejul,jemo,jeday)
        write(io6,70) stn1(j),jsmo,jsday,jsyr,jemo,jeday,jeyr,
     1              icount(j)
70      format(2x,a6,7x,2(i2,'/'),i4,2x,2(i2,'/'),i4,2x,i5)
35    continue
      return
      end
c----------------------------------------------------------------------
      subroutine flagchk(flag)
c----------------------------------------------------------------------
c
c --- PXTRACT   Version: 4.25      Level: 010608                FLAGCHK
c               D. Strimaitis, Earth Tech, Inc.
c
c --- Checks 2-character flag code from precipitation record for values
c --- that should be interpreted is 'good' when deciding which records
c --- may be written to the output files
c
c --- INPUTS:
c            FLAG - character*2 - Precipitation flag
c
c --- OUTPUTS:
c            FLAG - character*2 - Precipitation flag (possibly changed)
c
c
c --- FLAGCHK called by:  RDSTN
c                         RDSTATE
c                         RDALL
c --- FLAGCHK calls:      none
c----------------------------------------------------------------------
      character*2 flag

c --- Keep Q in FLAG-2 (anything else is accepted)
      if(flag(2:2).NE.'Q') then
         flag(2:2)=' '
      endif

c --- Pass FLAG-1 as blank if g, E, or T is found in record
      if(flag(1:1).EQ.'g' .OR.
     &   flag(1:1).EQ.'T' .OR.
     &   flag(1:1).EQ.'E') then
         flag(1:1)=' '
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine fin
c----------------------------------------------------------------------
c
c --- PXTRACT   Version: 4.25            Level: 030402              FIN
c               J. Scire, Earth Tech, Inc.
c
c --- PURPOSE:  Run termination routine -- compute runtime
c
c --- UPDATE:
c
c     V4.1 (011003) to V4.2 (030402) --- (DGS)
c            - Replace IOMESG with IO6 in 2nd JULDAY
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
      include 'params.pxt'
      include 'qa.pxt'
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
