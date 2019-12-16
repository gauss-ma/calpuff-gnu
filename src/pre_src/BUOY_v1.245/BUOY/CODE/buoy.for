c----------------------------------------------------------------------
c --- BUOY -- Buoy Data Preprocessor Program
c----------------------------------------------------------------------
c
c --- BUOY   Version: 1.245      Level: 070327                  MAIN
c --- Developed by: 	C. DesAutels
c
c     Copyright (c) 2005-2007 by Exponent, Inc.
c
c --- PURPOSE: Read NODC and NDBC buoy data into SEA.DAT format
c
c --- UPDATES
c
c --- Version 1.244, Level 060519 to Version 1.245, Level 070327
c     (1) CALUTILS from v2.52 Level 060519 to v2.55 Level 070327
c         Move GLOBE1 to COORDLIB
c         Allow negative increments in INCRS
c         Fixed format bug in subroutine BASRUTC for the case of time
c         zone zero (output string was 'UTC+0  0' instead of 'UTC+0000'
c         Modified:  INCRS, UTCBASR, BASRUTC
c         Removed:   GLOBE1
c     (2) COORDLIB from v1.96 Level 051010 to v1.98 Level 060911
c         Changes in COORDS that allow a higher level of FORTRAN error
c         checking.  Compiler checks had identified constant arguments
c         and 2 uninitialized variables.  None of these is known to
c         have produced errors in cooerdinate conversions.
c         Add GLOBE1 (from CALUTILS)
c         Modified:  COORDS
c
c --- Version 1.243, Level 060323 to Version 1.244, Level 060519
c     (1) CALUTILS from v2.51 Level 051019 to v2.52 Level 060519
c         Variable names in control file are not processed correctly
c         if there are too many characters (including blanks) to the
c         left of the "=" sign (run stops in setup phase).
c         Modified:  READIN
c
c --- Version 1.242, Level 060317 to Version 1.243, Level 060323
c     (1) Revised X-Y location format to allow for locations up to
c         -9999.999 km
c
c --- Version 1.241, Level 060210 to Version 1.242, Level 060317
c     (1) COORDLIB from v1.95 Level 050126 to v1.96 Level 051010
c     (2) CALUTILS from v2.5 Level 041123 to v2.51 Level 051019
c
c --- Version 1.24, Level 060206 to Version 1.241, Level 060210
c     (1) Version update performed to remove duplicate Version 1.24
c         distributions.  Version 1.241 is identical to the FINAL
c         Version 1.24 code.  An earlier version 1.24 made on the
c         same day contains an implementation error, and will not
c         run.
c         The local timestamp on the Version 1.24 code files are:
c         Early Version 1.24 (bad)   Feb 6, 2006 at 01:54:18 PM
c           file size:               88,578 bytes
c         FINAL Version 1.24 (good)  Feb 6, 2006 at 07:10:40 PM
c           file size:               88,624 bytes
c
c --- Version 1.23, Level 060119 to Version 1.24, Level 060206
c     (1) Remove the number of stations from header of output file
c         for Dataset 2.2 (only 1 station supported)
c         Modified:  WRTHEAD
c     (2) Change filename strings from c*70 to c*132
c         This is required by CALUTILS 2.3 and later which uses
c         c*132 in FILCASE and COMLINE
c         Modified:  FILNAM.BUY
c                    SETUP, READCF
c
c --- Version 1.22, Level 051220 to Version 1.23, Level 060119
c     (1) Correct convention for longitude in version 2.0 output
c         the expected convention is for west longitude to be
c         positive.
c
c --- Version 1.21, Level 051220 to Version 1.22, Level 060105
c     (1) Correct convention of hour-ending for MOD5 output
c         from NODC data and hour beginning for MOD6 output
c         from NDBC data
c     (2) Allowed NODC stations to move during the requested
c         period.  Location is read every hour.  Location
c         for missing hours is forward looking to the next
c         available period.
c
c --- Version 1.2, Level 051109 to Version 1.21, Level 051220
c     (1) update READCF to read in time zonein format: UTC-0500
c     (2) Create output version 2.11 which includes time zone
c         in the last record of the header.
c     (3) Updated version of calutils.for to bring in calls
c         for converting time zone from string to integer.
c     (4) Reclassified output formats:
c                           2.0 pre-10/2005 format
c                             with no wave parameters
c                           2.1 Based on 2.0 with
c                             2 wave parameters for wave 
c                             height and period
c                           2.11 Revision of 2.1 to add time zone
c                             and other format corrections
c                           2.2 MOD6 version of 2.11 with sub-hourly
c                               time steps.
c                           3.0 Full wave data with 
c                             sub-hourly time steps.
c     (5) Corrected bug which would produce 1 extra hour of output
c           at the beginning of the run.
c     (6) Revised format statements for output in both RDNDBC and RDF291
c
c
c --- Version 1.1, Level 050826 to Version 1.2, Level 051109
c     (1) Add routines to write version 2.1 sea.dat files
c     (2) Reclassified data formats: 
c                           2.0 pre-10/2005 format
c                             with no wave parameters
c                           2.1 Based on 2.0 with
c                             2 wave parameters for wave 
c                             height and period
c                           2.2 Full wave data with 
c                             sub-hourly time steps.
c
c --- Version 1.0, Level 050117 to Version 1.1, Level 050826
c     (1) Add routines to write version 2.0 sea.dat files
c
c----------------------------------------------------------------------

      Program buoy
c
c
c --- Include parameters
      include 'params.buy'

      include 'control.buy'
      include 'qa.buy'
      include 'station.buy'
c
c --- Set version and level number of program (stored in /QA/ and
c --- checked against values set in PARAMS.BUY)
      ver='1.245'
      level='070327'
c
c --- SETUP PHASE -- read control file information
      call SETUP
c
c --- COMPUTATIONAL PHASE -- process data files
      call COMP
c
c --- TERMINATION PHASE -- program termination functions
c      call FIN
c
      stop
      end

c----------------------------------------------------------------------
      BLOCK DATA
c----------------------------------------------------------------------
c
c --- BUOY   Version: 1.245      Level: 050117             BLOCK DATA
c                C. DesAutels Earth Tech.
c
c --- Include parameter statements
      include 'params.buy'
c
c --- Include common blocks
      include 'datehr.buy'
      include 'filnam.buy'
      include 'control.buy'
      include 'qa.buy'
      include 'grid.buy'
      include 'station.buy'

c --- FILNAM common block
      data runinp/'buoy.inp'/,lstfil/'buoy.lst'/,
     1 outfil/'sea.dat'/,
     4     nbdf/1/
c --- FILLOG common block
      data lcfiles/.true./
      data dataver/'2.11'/

c --- CONTROL common block

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
c --- BUOY   Version: 1.245      Level: 060206             SETUP
c               C. DesAutels, Earth Tech, Inc.
c
c --- Version 1.22, Level 060105 to Version 1.24, Level 060206
c       1) Change filename strings from c*70 to c*132
c          This is required by CALUTILS 2.3 and later which uses
c          c*132 in FILCASE and COMLINE
c
c --- Version 1.21, Level 051220 to Version 1.22, Level 060105
c
c       1) Corrected error for processing LCC output data from NODC input
c          which caused errors in the location. rlat and rlon were redundantly
c          used.
c
c PURPOSE:     SETUP reads and checks the control data provided, sets
c              logicals, echoes the control data, and opens the data
c              files if inputs are valid.
c
c
c ARGUMENTS:
c    PASSED:  none
c
c  RETURNED:  /CONTROL/   logicals and flags
c             /GRID/      data
c
c CALLING ROUTINES:   MAIN
c
c EXTERNAL ROUTINES:  DATETM, COMLINE, READCF, GLOBE1, GLOBE, XTRACTLL
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.buy'
      include 'control.buy'
      include 'filnam.buy'
      include 'qa.buy'
      include 'grid.buy'
      include 'datehr.buy'
      include 'station.buy'

      character*40 hdline1,ndbc11,ndbc21,ndbc31,ndbc41
      character*50 hdline2,ndbc12,ndbc22,ndbc32,ndbc42


      character*1 rlath,rlonh
      character*12 caction
      character*4 c4dum

      real*8 vecti(9),vecto(9)
      real depth,lat,lon,rlatc,rlonc
      real rlatd,rlatm,rlats,rlond,rlonm,rlons,rdepth

c --- Get date and time from system
      call DATETM(rdate,rtime,rcpu)

c --- Get the name of the control file from the command line
      call COMLINE(runinp)

c --- Open the control file
      open(ioinp,file=runinp,status='old')

c --- Report progress
      write(iomesg,*)'SETUP PHASE'

c --- Check that the version and level number in the parameter
c --- file matches those in the code itself
      if(ver.ne.mver.or.level.ne.mlevel)then
         write(iomesg,10) ver,level,mver,mlevel
10       format(/1x,'ERROR in SUBR. SETUP -- The BUOY version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open list file)
      call READCF

      if (lprev) call RDPREVHD

c --- READ INPUT PARAMETERS FROM BUOY.INP

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
      write(iolst,*) 'BUOY.INP = ',runinp

      write(iolst,*)
      write(iolst,*) 'Buoy Data Input File Names -------------'
      if (lprev) then
      write(iolst,*) 'prev.dat : ',prevdat
      endif
      write(iolst,*) 'data files:'
      write(iolst,*) 'File Type: ',datatyp(1)
      do i=1,nbdf
        write(iolst,'(a10,3x,a132)')
     &       'dbfile : ',datafil(i)
      enddo   

      write(iolst,*)
      write(iolst,*) 'Input Datum-Region used --------'
      if(datatyp(1).eq.'NDBC') then
        write(iolst,*) 'Datum : ', dndbc
      elseif(datatyp(1).eq.'NODC') then
        write(iolst,*) 'Datum : ', dnodc
      endif

      write(iolst,*)
      write(iolst,*) 'Output File Names -------------'
      write(iolst,*) 'lstfil   : ' ,lstfil
      write(iolst,*) 'outfil   : ' ,outfil

      write(iolst,*)
      write(iolst,*) 'Location Info (for output) ---------------------'
      write(iolst,*) 'datum   : ' ,datum
      write(iolst,*) 'pmap    : ' ,pmap
      if(LUTM) then
         write(iolst,*) 'Hemisphere : ',utmhem
         write(iolst,*) 'UTM zone   : ' ,iutmzn
      endif
      if(LLCC.or.LLAZA.or.LTTM) then
         write(iolst,*) 'fEast  : ' ,feast
         write(iolst,*) 'fNorth : ' ,fnorth
      endif
      if(LLCC.or.LPS.or.LEM.or.LLAZA.or.LTTM) then
         write(iolst,*) 'rlat(N): ' ,rlat
         write(iolst,*) 'rlon(E): ' ,rlon
         if(LLCC.or.LPS)write(iolst,*) 'xlat1  : ' ,xlat1
         if(LLCC)write(iolst,*) 'xlat2  : ' ,xlat2
      endif
      write(iolst,*) ' '
      write(iolst,*) 'Time Period -------'
      write(iolst,*) 'Period to Extract:   ',
     1    ibmo,'/',ibdy,'/',ibyr,ibhr,':00','  to  ',iemo,'/',iedy,
     1    '/',ieyr,iehr,':00'

c---------------------------------------------------
c--- CONFIRM STATION LOCATION
c---------------------------------------------------
c--- IF FORMAT IS NODC OPEN FILE AND READ FORMAT
c---------------------------------------------------


      if(datatyp(1).eq.'NODC') then
         open(iobuoy,file=datafil(1),form='formatted',status='old')

      read(iobuoy,101)btyp,idnum,rlatd,rlatm,rlats,
     &  rlath,rlond,rlonm,rlons,rlonh,rdepth
      close(iobuoy)
      bnum=bnam

      depth=rdepth/10
      rlatc=rlatd+(rlatm/60)+(rlats/3600)
      write(blat,102)rlatc,rlath
      rlonc=rlond+(rlonm/60)+(rlons/3600)
      write(blon,102)rlonc,rlonh

101   format(a3,7x,i5,11x,3(f2.0),a1,f3.0,2(f2.0),a1,f5.0)
102   format(f7.3,a1)

      endif

      write(iolst,*)
      write(iolst,*) 'Buoy Specifications ---------'
      write(iolst,*) 'Buoy Name   : ' ,bnam
      write(iolst,*) 'Buoy Number : ' ,idnum
      write(iolst,*) 'Lat(N)      : ' ,blat
      write(iolst,*) 'Lon(E)      : ' ,blon
      write(iolst,*)
      write(iolst,*)
      write(iolst,*) 'Record of missing or replaced data'
      write(iolst,*) '------------------------------------------'

c----------------------------------------------------
c--- IF FORMAT IS NDBC OPEN FILE TO CONFIRM VERSION
c----------------------------------------------------
      if(datatyp(1).eq.'NDBC') then

        ndbc11='YY MM DD hh WD   WSPD GST  WVHT  DPD   A'
        ndbc12='PD  MWD  BAR    ATMP  WTMP  DEWP  VIS'

        ndbc21='YYYY MM DD hh WD   WSPD GST  WVHT  DPD  '
        ndbc22=' APD  MWD  BAR    ATMP  WTMP  DEWP  VIS'

        ndbc31='YYYY MM DD hh WD   WSPD GST  WVHT  DPD  '
        ndbc32=' APD  MWD  BAR    ATMP  WTMP  DEWP  VIS  TIDE' 

        ndbc41='YYYY MM DD hh  WD WSPD  GST  WVHT   DPD  '
        ndbc42='  APD MWD  BARO   ATMP  WTMP  DEWP  VIS  TIDE'


        open(iobuoy,file=datafil(1),form='formatted',status='old')
 
        read(iobuoy,1301)hdline1,hdline2

1301    format(40a,50a)
        if (hdline1.eq.ndbc11.and.hdline2.eq.ndbc12) then
          btyp='A'
        elseif (hdline1.eq.ndbc21.and.hdline2.eq.ndbc22) then
          btyp='B'
        elseif (hdline1.eq.ndbc31.and.hdline2.eq.ndbc32) then
          btyp='C'
        elseif (hdline1.eq.ndbc41.and.hdline2.eq.ndbc42) then
          btyp='D'
        else
          write(iomesg,*)'ERROR in read NDBC data'
          write(iomesg,*)'Undocumented header format'
          write(iomesg,1301)hdline1,hdline2
          stop
        endif
        close(iobuoy)

      endif

c --- Set utm zone for coordinate calls
	iutm=iutmzn
	if(utmhem.EQ.'S   ' .AND. xbtz.LT.900) iutm=-iutm

c --- Convert Location to Local coordinate system

c --- Set station location from LL to the output grid 
	if(lutm) then
c ---    Using UTM grid
	   call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'UTM     ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feast,fnorth,
     &               caction,vecti,vecto)

	elseif(llcc) then
c ---    Using Lambert Conformal grid
	   call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'LCC     ',idum,xdum,xlat1,xlat2,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
	elseif(lps) then
c ---    Using Polar Stereographic grid
	   call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'PS      ',idum,xdum,xlat1,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
	elseif(lem) then
c ---    Using Equatorial Mercator grid
	   call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'EM      ',idum,xdum,xlat1,-xlat1,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
	elseif(llaza) then
c ---    Using Lambert Azimuthal Equal Area grid
	   call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'LAZA    ',idum,xdum,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
	elseif(lttm) then
c ---    Using Tangential TM grid
	   call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'TM      ',idum,tmsone,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
	endif


c----------------------
c --- Map LL to output
c----------------------
 
      call XTRACTLL(iolst,'LAT ',BLAT,RLATc)
      call XTRACTLL(iolst,'LON ',BLON,RLONc)

      call GLOBE(iolst,caction,dndbc,vecti,datum,vecto,
     &           RLONc,RLATc,bx,by,idum,c4dum)

      return
      end


c-----------------------------------------------------------------------
      subroutine comp
c-----------------------------------------------------------------------
c
c --- BUOY   Version: 1.245      Level: 050117             SETUP
c               C. DesAutels, Earth Tech, Inc.
c
c PURPOSE:     COMP reads and formats the buoy data from original source
c
c --- Updates
c
c ARGUMENTS:
c    PASSED:  none
c
c  RETURNED:  /CONTROL/   logicals and flags
c             /GRID/      data
c
c CALLING ROUTINES:   MAIN
c
c EXTERNAL ROUTINES:  DATETM, COMLINE, WRTHEAD, RDNDBC, RDF291
c-----------------------------------------------------------------------

c --- Include file of parameters and commons

      include 'params.buy'
      include 'control.buy'
      include 'filnam.buy'
      include 'qa.buy'
      include 'grid.buy'
      include 'datehr.buy'
      include 'station.buy'

      character title1*4,title2*4,title3*4
      character title4*4,title5*4,title6*4,title7*4

c
      write(iomesg,*)'COMP PHASE'
c
      open(ioout,file=OUTFIL,form='formatted',status='unknown')

      call wrthead

c----------------------------------------------------
c--- SET READ FORMAT BASED ON NODC or NDBC FILES
c----------------------------------------------------
       if (datatyp(1).eq.'NDBC') then
         call rdndbc
       else if (datatyp(1).eq.'NODC') then
         call rdf291
       endif

       end

c----------------------------------------------------------------------
      subroutine rdprevhd
c
c --- BUOY   Version: 1.245      Level: 050117             SETUP
c               C. DesAutels, Earth Tech, Inc.
c
c PURPOSE:     RDPREVHD reads the header records of a previous sea.dat
c              file only if LPREV is set to True.
c
c --- Updates
c
c ARGUMENTS:
c    PASSED:  none
c
c  RETURNED:  
c             
c
c CALLING ROUTINES:  SETUP 
c
c EXTERNAL ROUTINES:  NONE
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.buy'
      include 'control.buy'
      include 'filnam.buy'
      include 'qa.buy'
      include 'grid.buy'
      include 'datehr.buy'
      include 'station.buy'


      character*12 pdaten
      character*16 pdataset,pdataver
      character*64 pdatamod
      character*80 pcomment

      integer*4 pncomment
      integer piutmzn, ifatal
      character*4 putmhem
      character*8 pdatum

      character*16 pclat0,pclon0,pclat1,pclat2
      character*8 ppmap

      open (ioprev,file=prevdat,status='old')
c --- Record 1:  Dataset, Version, Modifier
      read(ioprev,'(2a16,a64)') pdataset,pdataver,pdatamod
c --- Record 2:  Number of comment records
      read(ioprev,'(i4)') pncomment
c --- Record 3:  Comment (optional/repeatable)
      do i=1,pncomment
      read(ioprev,'(a80)') pcomment
      enddo

c --- Record 4:  Map projecption
      read(ioprev,'(a8)') ppmap
      if(ppmap.ne.pmap)ifatal=1

c --- Record 5:  Map projection parameters
      if(ppmap.eq.'UTM') then
         read(ioprev,'(i4,a4)') piutmzn,putmhem
         if(piutmzn.ne.iutmzn)ifatal=.1
         if(putmhem.ne.utmhem)ifatal=1
      elseif(ppmap.eq.'LCC') then
         read(ioprev,'(4a16)') pclat0,pclon0,pclat1,pclat2
         if(pclat0.ne.clat0)ifatal=1
         if(pclon0.ne.clon0)ifatal=1
         if(pclat1.ne.clat1)ifatal=1
         if(pclat2.ne.clat2)ifatal=1
      elseif(ppmap.eq.'PS') then
         read(ioprev,'(3a16)') pclat0,pclon0,pclat1
         if(pclat0.ne.clat0)ifatal=1
         if(pclon0.ne.clon0)ifatal=1
         if(pclat1.ne.clat1)ifatal=1
      elseif(ppmap.eq.'EM'.or.ppmap.eq.'LAZA'.or.ppmap.eq.'TTM') then
         read(ioprev,'(2a16)') pclat0,pclon0
         if(pclat0.ne.clat0)ifatal=1
         if(pclon0.ne.clon0)ifatal=1
      endif

c --- Record 6:  False Easting, Northing
      if(ppmap.eq.'LCC'.or.ppmap.eq.'LAZA'.or.ppmap.eq.'TTM') then
         read(ioprev,*) pfeast,pfnorth
         if(pfeast.ne.feast)ifatal=1
         if(pfnorth.ne.fnorth)ifatal=1
      endif

c --- Record 7:  Map DATUM
      read(ioprev,'(a8,a12)') pdatum,pdaten
         if(pdatum.ne.datum)ifatal=1

c --- Record 9:  XYUNIT,ZUNIT
      read(ioprev,*)  

      read (ioprev,'(3i6,6x,3i6,6x,f4.0,i5)')ipbyr,ipbmo,ipbdy,
     &           ipeyr,ipemo,ipedy,xpbtz, ipnumb

      do i=1,ipnumb
      read (ioprev,'(a5)')pbnum(i)
      enddo
      if(ifatal.eq.1) then
        write(iomesg,*)'ERROR in RDPREVHD, Data projections'
        write(iomesg,*)'do not match with PREV.DAT file'
        stop
      endif
      if(ibyr.ne.ipbyr.or.ibmo.ne.ipbmo.or.ibdy.ne.ipbdy.or.
     &   ieyr.ne.ipeyr.or.iemo.ne.ipemo.or.iedy.ne.ipedy)then
         write(iomesg,*)'ERROR in RDPREVHD, Beginning-Ending'
         write(iomesg,*)'Dates do not match with PREV.DAT file'
         stop
      endif



      return
      end

c-----------------------------------------------------------------------
      subroutine wrthead
c-----------------------------------------------------------------------
c
c --- BUOY   Version: 1.245      Level: 060206           WRTHEAD
c               C. DesAutels, Earth Tech
c
c PURPOSE:     WRTHEAD constructs the header records for the output
c              data file BUOY.DAT
c
c UPDATES:
c
c --- Version 1.21, Level 051220 to Version 1.23, Level 060206
c     (1) Remove the number of stations from header of output file
c         for Dataset 2.2 (only 1 station supported)
c
c --- Version 1.2, Level 051109 to Version 1.21, Level 051220
c
c     (1) update READCF to read in time zonein format: UTC-0500
c     (2) Create output version 2.11 which includes time zone
c         in the last record of the header.
c
c --- Version 1.0, Level 050826 to Version 1.0, Level 051109
c
c     (1) Add routines to write version 2.1 sea.dat files
c     (2) Reclassified data formats: 
c                           2.0 pre-10/2005 format
c                             with no wave parameters
c                           2.1 Based on 2.0 with
c                             2 wave parameters for wave 
c                             height and period
c                           2.2 Full wave data with 
c                             sub-hourly time steps.
c
c --- Version 1.0, Level 050117 to Version 1.0, Level 050826
c
c     (1) Add routines to write version 2.0 or 2.1 sea.dat header files
c
c ARGUMENTS:
c    PASSED:  /CONTROL/   logicals
c             /GRID/      data
c             /QA/        ver,level
c
c  RETURNED:  none
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  NIMADATE
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.buy'
      include 'control.buy'
      include 'filnam.buy'
      include 'qa.buy'
      include 'grid.buy'
      include 'datehr.buy'
      include 'station.buy'

c --- Local Variables
      character*12 daten
      character*16 dataset
      character*64 datamod
      character*80 comment1, comment2

c --- Configure output variables
      data dataset/'SEA.DAT'/
      data datamod/'Header structure with coordinate parameters'/
      data ncomment/2/
      data comment1/'Produced by BUOY Version: '/

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
      write(comment2,*)'Data values taken from ', datatyp(1), 
     &   'Data Format ', btyp      

c --- Record 1:  Dataset, Version, Modifier
      write(ioout,'(2a16,a64)') dataset,dataver,datamod
c --- Record 2:  Number of comment records
      write(ioout,'(i4)') ncomment
c --- Record 3:  Comment (optional/repeatable)
      write(ioout,'(a80)') comment1
      write(ioout,'(a80)') comment2
c --- Record 4:  Map projection
      write(ioout,'(a8)') pmap
c --- Record 5:  Map projection parameters
      if(LUTM) then
         write(ioout,'(i4,a4)') iutmzn,utmhem
      elseif(LLCC) then
         write(ioout,'(4a16)') clat0,clon0,clat1,clat2
      elseif(LPS) then
         write(ioout,'(3a16)') clat0,clon0,clat1
      elseif(LEM.or.LLAZA.or.LTTM) then
         write(ioout,'(2a16)') clat0,clon0
      endif
c --- Record 6:  False Easting, Northing
      if(LLCC.or.LLAZA.or.LTTM) then
         write(ioout,*) feast,fnorth
      endif
c --- Record 7:  Map DATUM
      call NIMADATE(daten)
      write(ioout,'(a8,a12)') datum,daten
c --- Record 9:  XYUNIT,ZUNIT
      write(ioout,'(a2)') 'KM'        

c --- Record 10: Time Zone
      if(dataver.eq.'2.11'.or.dataver.eq.'2.2'
     &                   .or.dataver.eq.'3.0') then
         write(ioout,'(a8)')axtz
      endif

      inumb=1
      if (lprev) then
         inumb=1+ipnumb
      endif
      if(dataver.eq.'2.0'.or.dataver.eq.'2.1') then
         write(ioout,'(i9,1x,a6)')idnum,bnam

      elseif(dataver.eq.'2.11') then
         write(ioout,'(6i6)')ibyr,ibjdy,ibhr,
     &              ieyr,iejdy,iehr
         write(ioout,'(i9,1x,a132)')idnum,bnam

      elseif(dataver.eq.'2.2') then
         write(ioout,'(8i6)')ibyr,ibjdy,ibhr,0000,
     &              ieyr,iejdy,iehr,3600
         write(ioout,'(i9,1x,a132)')idnum,bnam

      elseif(dataver.eq.'3.0') then
         write(ioout,'(8i6,i5)')ibyr,ibjdy,ibhr,0000,
     &              ieyr,iejdy,iehr,3600, inumb
         do i=1,ipnumb
            write(ioout,'(i5)')pbnum(i)
         enddo
         write(ioout,'(i9,1x,a132)')idnum,bnam
   
      endif


      return
      end

c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- BUOY   Version: 1.245      Level: 060206            READCF
c               C. DesAutels   Earth Tech
c
c --- PURPOSE:  Read the control file containing the file names of
c               the input and output files of the run, and associated
c               control variables, place into program variables,
c               and QA information.
c
c --- UPDATES:
c
c --- Version 1.21, Level 051220 to Version 1.24, Level 060206
c     (1) Change filename strings from c*70 to c*132
c         This is required by CALUTILS 2.3 and later which uses
c         c*132 in FILCASE and COMLINE
c
c --- Version 1.2, Level 051109 to Version 1.21, Level 051220
c
c     (1) update READCF to read in time zonein format: UTC-0500
c     (2) Create output version 2.11 which includes time zone
c         in the last record of the header.
c      
c --- Version 1.0, Level 050117 to Version 1.1, Level 050826
c
c     (1) Add variable DATAVER to allow for writing of version 2.0 or 
c         version 2.1 sea.dat files
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c
c        Parameters: IOCNT, IOLST, IOMESG, IOINP, MXVAR, MXFIL
c
c --- OUTPUT:
c
c ---    Common block /FILNAM/ variables:
c           nbdf,lstfil,outfil,pltfil,prevfil,savefil,xyinp,xyout,
c           datafil(mxfil),justname(mxfil),datatyp(mxfil),
c           lcfiles
c ---    Common block /CELL/ variables:
c           ithresh
c ---    Common block /CONTROL/ variables:
c
c ---    Common block /GRID/ variables:
c
c ---    Common block /XY/ variables:
c
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE, XTRACTLL
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.buy'
      include 'params.cal'
c
c --- Include common blocks
      include 'control.buy'
      include 'filnam.buy'
      include 'qa.buy'
      include 'datehr.buy'
      include 'grid.buy'
      include 'station.buy'

c

c --- Local variables
c --- Dimension CTEMP(132, MAX(names,ntypes)), DBTYPE(ntypes)
      character*4 ctemp(132,5)
      character*6 dbtype(2)
      character*132 dbfil
      character*4 ctime(70)
      character*4 clatlon(16,4)
      character*4 cpmap(8),cdatum(8)
      character*4 cnam(132),cnum(8)
      character*12 cvdic(mxvar,6)
      integer ivleng(mxvar,6),ivtype(mxvar,6)
      logical lecho, lerrcf


      character*12 caction
      character*4 c4dum
      real*8 vecti(9),vecto(9)
      real lat, lon
      character*1 iq, cblnk


c --- Initialize local variables
      data lecho/.false./, lerrcf/.false./
      data names/4/, ntypes/2/
      data dbtype/'NDBC','NODC'/
      data cblnk/' '/
      iq=''''

c --- Set Dictionary
      data cvdic/
     a  'PREVDAT','OUTFIL','LSTFIL','PLTFIL','LCFILES','NBDF',
     a  'DATAVER', 53*' ',
     b  'NDBC','NODC', 58* ' ',
     c  'DNDBC','DNODC', 58* ' ',
     1  'IBYR','IBMO','IBDY','IBHR','IEYR','IEMO','IEDY',
     1  'IEHR','ABTZ', 'LPREV', 50*' ',
     2  'PMAP','IUTMZN','UTMHEM','RLAT0','RLON0','RLAT1','RLAT2',
     2  'DATUM','FEAST','FNORTH',  50*' ',
     3  'BNAM','IDNUM','BLAT','BLON','BANM', 'RASN', 'RWSN', 53*' '/

      data ivleng/
     a  4*132,2*1, 1*16, 53*0,
     b  2*132, 58*0,
     c  2*132, 58*0,
     1  8*1, 70, 1, 50*0,
     2  8, 2*1, 4*16, 8, 2*1, 50*0,
     3  132, 1, 16, 16, 1, 1, 1,  53*0/

c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
      data ivtype/
     a  4*4, 3, 2, 4, 53*0,
     b  2*4, 58*0,
     c  2*4, 58*0,
     1  8*2, 1*4, 1*3, 50*0,
     2  4, 2, 6*4, 1, 1, 50*0,
     3  4, 2, 4, 4, 1, 1, 1, 53*0/


c ------------------
c --- Input Group 0a
c ------------------
c --- Initialize the temporary arrays
      do i=1,5
         do j=1,132
            ctemp(j,i)(1:1)=' '
         enddo
      enddo
c --- Read the group data
       call READIN(cvdic(1,1),ivleng(1,1),ivtype(1,1),ioinp,iolst,
     & lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),lcfiles,nbdf,
     2 ctemp(1,5),idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum)

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')prevdat=' '
      if(ctemp(1,2)(1:1).ne.' ')outfil=' '
      if(ctemp(1,3)(1:1).ne.' ')lstfil=' '
      if(ctemp(1,4)(1:1).ne.' ')pltfil=' '
      if(ctemp(1,5)(1:1).ne.' ')dataver=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')prevdat(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')outfil(j:j)=ctemp(j,2)(1:1)
         if(ctemp(j,3)(1:1).ne.' ')lstfil(j:j)=ctemp(j,3)(1:1)
         if(ctemp(j,4)(1:1).ne.' ')pltfil(j:j)=ctemp(j,4)(1:1)
      enddo
      do j=1,16
         if(ctemp(j,5)(1:1).ne.' ')dataver(j:j)=ctemp(j,5)(1:1)
      enddo


c --- Convert the file names to the proper case
      call FILCASE(lcfiles,outfil)
      call FILCASE(lcfiles,lstfil)
      call FILCASE(lcfiles,pltfil)

c --- Open listfile
      open(iolst,file=lstfil,status='unknown')

c --- Write banner to list file
      write(iolst,5) ver,level
5     format(///,26x,'BUOY OUTPUT SUMMARY',/,19x,'VERSION:  ',A8,
     1       ' LEVEL:  ',A8///)

c ------------------
c --- Input Group 0b
c ------------------

      do k=1,nbdf
c ---    Initialize the temporary arrays for the file names
         do i=1,ntypes
            do j=1,132
               ctemp(j,i)(1:1)=' '
            enddo
         enddo
         do j=1,132
            dbfil(j:j)=' '
         enddo
c ---    Read one DB filename entry (one of 2 DB types)
      call READIN(cvdic(1,2),ivleng(1,2),ivtype(1,2),ioinp,iolst,
     & lecho,
     1 ctemp(1,1),ctemp(1,2),idum,idum,idum,
     2 idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum)

c ---    Process if filename array has room for this entry
         if(k.LE.mxfil) then

            ii=0
            j=1
            if(ctemp(j,1)(1:1).ne.' ') then
             ii=1
            else if(ctemp(j,2)(1:1).ne.' ') then
             ii=2
            endif
            datatyp(k)=dbtype(ii)

c ---       Transfer the char*4 data into the char*132 variable
            do j=1,132
               if(ctemp(j,ii)(1:1).ne.' ')dbfil(j:j)=ctemp(j,ii)(1:1)
            enddo

c ---       Convert the file name to the proper case
            call FILCASE(lcfiles,dbfil)         

c ---       Place information in DB array, up to MXFIL
            datafil(k)=dbfil

c ---       Extract just file name without path info.
            ipath=0
            ilen=0
            do n=1,132
                if(dbfil(n:n).NE.' ') ilen=n
                if(dbfil(n:n).EQ.'/' .OR. dbfil(n:n).EQ.'\') ipath=n
            enddo
            read(dbfil(ipath+1:ilen),'(a)') justname(k)
         endif
      enddo

c ------------------
c --- Input Group 0c
c ------------------

c --- Initialize the temporary arrays for the Datum-Region names
      do i=1,2
         do j=1,132
            ctemp(j,i)(1:1)=' '
         enddo
      enddo

      do j=1,8
         dndbc(j:j)=' '
         dnodc(j:j)=' '
      enddo

c --- Read Datum-Region for each DB type
      call READIN(cvdic(1,3),ivleng(1,3),ivtype(1,3),ioinp,iolst,lecho,
     1 ctemp(1,1),ctemp(1,2),idum,idum,idum,
     2 idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum)

c --- Transfer the char*4 data into the char*8 variables
         do j=1,8
            if(ctemp(j,1)(1:1).ne.' ')dndbc(j:j)=ctemp(j,1)(1:1)
            if(ctemp(j,2)(1:1).ne.' ')dnodc(j:j)=ctemp(j,2)(1:1)
         enddo

c -----------------
c --- Input Group 1
c -----------------

c --- Initialize the temporary array for the time zone
      do j=1,70
         ctime(j)(1:1)=' '
         abtz(j:j)=' '
      enddo

      call readin(cvdic(1,4),ivleng(1,4),ivtype(1,4),ioinp,iolst,lecho,
     1 IBYR,IBMO,IBDY,IBHR,IEYR,IEMO,IEDY,IEHR,CTIME(1),LPREV,
     2 idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum)

c ---  transfer the char*4 data into the char*70 variable
       do j=1,70
          if(ctime(j)(1:1).ne.' ')abtz(j:j)=ctime(j)(1:1)
       enddo

c ---  SEA.DAT version 2.11
c ---  Strip charct time zone from blanks and take 8 characters
       ij=0
       do j=1,70
          if (abtz(j:j).ne.' ') then
             ij=ij+1
             if (ij.gt.8) then
                write(io6,*)'wrong format for UTC time zone ABTZ'
                write(io6,*)'J, abtz(j) = ',J, abtz(j:j)
                STOP 'wrong format for input UTC time zone - STOP'
             endif
             axtz(ij:ij)=abtz(j:j)
          endif
       end do

c ---  transform Char*8 UTC time zone to real time zone
       call utcbasr(axtz,xbtz)


      if (lprev.and.(dataver.eq.'2.0')) then
         write(iomesg,*) 'ERROR in SUBR. READCF',
     1   'Previous SEA.DAT file may not be in 2.0 format'
         stop
      endif
      if (lprev.and.(dataver.eq.'2.1')) then
         write(iomesg,*) 'ERROR in SUBR. READCF',
     1   'Previous SEA.DAT file may not be in 2.1 format'
         stop
      endif


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

      call julday(iolst,IBYR,IBMO,IBDY,ibjdy)
      call julday(iolst,IEYR,IEMO,IEDY,iejdy)

      ibdathr = ibyr * 100000 + ibjdy * 100 + ibhr
      iedathr = ieyr * 100000 + iejdy * 100 + iehr

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

      call READIN(cvdic(1,5),ivleng(1,5),ivtype(1,5),ioinp,iolst,lecho,
     1 CPMAP,iutmzn,utmhem,
     2 CLATLON(1,1),CLATLON(1,2),CLATLON(1,3),CLATLON(1,4),
     3 CDATUM,FEASTIN,FNORTHIN,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     8 idum,idum,idum,idum,idum)

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

c -----------------
c --- Input Group 3
c -----------------

c --- Initialize the temporary arrays for the character lat/lon fields
      do i=1,4
         do j=1,16
            clatlon(j,i)(1:1)=' '
         enddo
      enddo
      do j=1,132
         cnam(j:j)=' '
         bnam(j:j)=' '
      enddo


      do j=1,16
         blat(j:j)=' '
         blon(j:j)=' '
      enddo
      do j=1,5
         bnum(j:j)=' '
         cnum(j:j)=' '
      enddo

c --- Initialize input false easting and northing with defaults
      feastin=feast
      fnorthin=fnorth

      call READIN(cvdic(1,6),ivleng(1,6),ivtype(1,6),ioinp,iolst,lecho,
     1 CNAM,idnum,CLATLON(1,1),CLATLON(1,2),banm,rasn,rwsn,
     2 idum,idum,idum,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum)


c --- Transfer the char*4 data into the char*16 variables
      do j=1,16
         if(clatlon(j,1)(1:1).ne.' ')blat(j:j)=clatlon(j,1)(1:1)
         if(clatlon(j,2)(1:1).ne.' ')blon(j:j)=clatlon(j,2)(1:1)
      enddo
c --- Transfer the char*4 data into the char*5 variables
      bnam(1:1)=iq
      if(CNAM(1)(1:1).ne.' ') then
         do j=2,132
            bnam(j:j)=CNAM(j-1)(1:1)
            if (bnam(j:j).NE.cblnk) then
                ipos=j+1
            endif
         enddo
      endif
      bnam(ipos:ipos)=iq
      if (dataver.eq.'2.0'.or.dataver.eq.'2.1') then
           bnam(6:6)=iq
      endif
 


c -----------------------------
c --- Translate selected inputs
c -----------------------------

c --- Translate character lat/lon to real NLat/ELon
      if(clat0(1:1).NE.' ') call XTRACTLL(iolst,'LAT ',clat0,rlat)
      if(clon0(1:1).NE.' ') call XTRACTLL(iolst,'LON ',clon0,rlon)
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
         rlat=0.0
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

c --- Test for valid NBDF
      if(nbdf.GT.mxfil) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 0a'
         write(iolst,*) 'NBDF exceeds the parameter MXFIL '
         write(iolst,*) 'NBDF, MXFIL = ',nbdf,mxfil
         write(iolst,*) 'Increase MXFIL in PARAMS.BUY and recompile'
         write(iolst,*) 'or reduce the number of Buoy input files'
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
      if((utmhem.NE.'N   ' .AND. utmhem.NE.'S   ') .AND. LUTM) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'UTMHEM out of range      = ',utmhem
         write(iolst,*) 'UTMHEM should be N or S'
         lerrcf=.TRUE.
      endif



      return
      end

c----------------------------------------------------------------------
      subroutine rdndbc
c----------------------------------------------------------------------
c
c --- BUOY   Version: 1.245      Level: 060323            RDNDBC
c               C. DesAutels   Earth Tech
c
c --- PURPOSE:  Read data from the NDBC website format and outputs
c               it into BUOY.DAT format
c
c --- UPDATES:
c
c --- Version 1.242, Level 060317 to Version 1.243, Level 060323
c
c     (1) Revised X-Y location format to allow for locations up to
c         -9999.999 km
c
c --- Version 1.22, Level 051220 to Version 1.23, Level 060119
c
c     (1) Correct convention for longitude in version 2.0 output
c         the expected convention is for west longitude to be
c         positive.
c
c --- Version 1.21, Level 051220 to Version 1.22, Level 060105
c
c     (1) Correct MOD 6 output to be the start hour and end time.
c
c
c --- Version 1.2, Level 051109 to Version 1.21, Level 051220
c
c     (1) update READCF to read in time zonein format: UTC-0500
c     (2) Create output version 2.11 which includes time zone
c         in the last record of the header.
c
c --- Version 1.1, Level 050826 to Version 1.2, Level 051109
c
c     (1) Add routines to write version 2.1 sea.dat files
c     (2) Reclassified data formats: 
c                           2.0 pre-10/2005 format
c                             with no wave parameters
c                           2.1 Based on 2.0 with
c                             2 wave parameters for wave 
c                             height and period
c                           2.2 Full wave data with 
c                             sub-hourly time steps.
c     (3) Reclassify 0.0 wave period to 99.0
cc
c --- Version 1.0, Level 050117 to Version 1.1, Level 050826
c
c     (1) Add routines to write version 2.0 sea.dat files
c     (2) Shift read statement for reading from NDBC data file to
c         second line to allow for all variables to be read.
c     (3) Change initial starting point to be back 2 to start at
c         proper requested hour.
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c
c        Parameters: 
c
c --- OUTPUT:
c
c
c --- RDNDBC called by:  COMP
c --- RDNDBC calls:      DEDAT, INCR, XTRACTLL, JULDAY
c----------------------------------------------------------------------
c --- Include file of parameters and commons

      include 'params.buy'
      include 'control.buy'
      include 'filnam.buy'
      include 'qa.buy'
      include 'grid.buy'
      include 'datehr.buy'
      include 'station.buy'

      integer ipro

      real depth,lat,lon,anm,tempa,pres,ws,wd,prec,rnyr
      real sr1,sr2,swh,awp,mwd,temps,dwp,mwh,mws,dp
      real rlatd,rlatm,rlats,rlond,rlonm,rlons,rdepth,rdwp
      real rsam, sam

      character*40 hdline1,ndbc11,ndbc21,ndbc31,ndbc41
      character*50 hdline2,ndbc12,ndbc22,ndbc32,ndbc42

      character*180 dateline, pline
      character*50 pline2
      character*1 iq

      iq=''''

      call dedat(ibdathr,ibyr,ibjulday,ibhr)
      call dedat(iedathr,ieyr,iejulday,iehr)

      myr =ibyr
      mjulday =ibjulday
      mhr = ibhr
      call incr(iolst,myr,mjulday,mhr,-1)

c----------------------------------------------------
c--- SET FORMATS OF HEADER RECORDS TO CONFIRM VERSION
c----------------------------------------------------
        ndbc11='YY MM DD hh WD   WSPD GST  WVHT  DPD   A'
        ndbc12='PD  MWD  BAR    ATMP  WTMP  DEWP  VIS'

        ndbc21='YYYY MM DD hh WD   WSPD GST  WVHT  DPD  '
        ndbc22=' APD  MWD  BAR    ATMP  WTMP  DEWP  VIS'

        ndbc31='YYYY MM DD hh WD   WSPD GST  WVHT  DPD  '
        ndbc32=' APD  MWD  BAR    ATMP  WTMP  DEWP  VIS  TIDE' 

        ndbc41='YYYY MM DD hh  WD WSPD  GST  WVHT   DPD  '
        ndbc42='  APD MWD  BARO   ATMP  WTMP  DEWP  VIS  TIDE'

      dp=9999.

       call XTRACTLL(iolst,'LON ',BLON,RLON)

C-----------------------------------------------
C---  WRITE HOURLY RECORDS TO THE BUOY.DAT FILE
C-----------------------------------------------
      do i=1,nbdf
        open(iobuoy,file=datafil(i),form='formatted',status='old')
 
        read(iobuoy,101)hdline1,hdline2

c----------------------------------------------------
c--- READ NDBC HEADER AND CONFIRM VERSION
c----------------------------------------------------
 101    format(40a,50a)
        if (hdline1.eq.ndbc11.and.hdline2.eq.ndbc12) then
          btyp='A'
        elseif (hdline1.eq.ndbc21.and.hdline2.eq.ndbc22) then
          btyp='B'
        elseif (hdline1.eq.ndbc31.and.hdline2.eq.ndbc32) then
          btyp='C'
        elseif (hdline1.eq.ndbc41.and.hdline2.eq.ndbc42) then
          btyp='D'
        else
          write(iolst,*) 'RDNDBC:  Unknown NDBC header structure' 
        endif
    
 111    continue
        call incr(iolst,myr,mjulday,mhr,1)
 2      continue 
        if (btyp.eq.'A') then
            read(iobuoy,3,end = 99) nyr,nmo,ndy,nhr,wd,ws,swh,
     &  dwp,pres,tempa,temps,dp
            nyr=nyr+1900
        elseif (btyp.eq.'B') then
            read(iobuoy,4,end = 99) nyr,nmo,ndy,nhr,wd,ws,swh,
     &  dwp,pres,tempa,temps,dp
        elseif (btyp.eq.'C') then
            read(iobuoy,5,end = 99) nyr,nmo,ndy,nhr,wd,ws,swh,
     &  dwp,pres,tempa,temps,dp
        elseif (btyp.eq.'D') then
            read(iobuoy,5,end = 99) nyr,nmo,ndy,nhr,wd,ws,swh,
     &  dwp,pres,tempa,temps,dp
        endif



        call julday(iolst,nyr,nmo,ndy,njulday)

 3      format(i2,1x,3(i2,1x),f3.0,f5.1,5x,f6.2,f6.2,11x,
     & f6.1,f6.1,f6.1,f6.1)
 4      format(i4,1x,3(i2,1x),f3.0,f5.1,5x,f6.2,f6.2,11x,
     & f6.1,f6.1,f6.1,f6.1)
 5      format(i4,1x,3(i2,1x),f3.0,f5.1,5x,f6.2,f6.2,11x,
     & f6.1,f6.1,f6.1,f6.1)


c---------------------------------------
c--- CONVERT TIME FROM GMT TO LST
c---------------------------------------
        ibtz=-real(xbtz)

        call incr(iolst,nyr,njulday,nhr,ibtz)

c----------------------------------------------------------------
c--- Convert to hour starting for MOD6.  Data from NDBC is assumed
c--- to come in as hour ending.  For MOD6 output the need is to
c--- back up one hour to define what the hour beginning would be.
c--- Final output will come out as [hour starting 0000] to 
c--- [hour starting 3600] for hourly records.
c----------------------------------------------------------------

        if(dataver.eq.'2.2'.or.dataver.eq.'3.0') then
           call incr(iolst,nyr,njulday,nhr,-1)
        endif

c---------------------------------------
c--- SKIP HOURS BEFORE REQUESTED PERIOD
c---------------------------------------
        if (nyr .lt. myr) goto 2
        if (njulday .lt. mjulday) goto 2
        if (nhr .lt. mhr) goto 2

c--------------------------------------
c--- EXIT AND END IF HOUR IS LATER THAN
c--- THE REQUESTED TIME PERIOD 
c--------------------------------------
 9      continue
        if (mhr .gt. iehr) then
           if (mjulday .gt. iejulday .or. mjulday .eq. iejulday) then
              if (myr .gt. ieyr .or. myr .eq. ieyr) then
                 goto 99
              endif
           endif
        endif
        if (mjulday .gt. iejulday) then
           if (myr .gt. ieyr .or. myr .eq. ieyr) then
              goto 99
           endif
        endif
        if (myr .gt. ieyr) then
            goto 99
        endif

c-----------------------------------
c--- CHECK FOR MISSING TIMES
c-----------------------------------
        if (myr .ne. nyr .or. mjulday .ne. njulday
     &      .or. mhr .ne. nhr) then
          write(iolst,*) 'missing data record ', myr,
     *     mjulday, mhr

c-----------------------------------
c--- WRITE DATA FOR MISSING HOURS
c-----------------------------------
      if (dataver.eq.'2.2'.or.dataver.eq.'3.0') then
          write(ioout,1200) myr, mjulday, mhr, 0000, 
     &                      myr, mjulday, mhr, 3600

      endif
c-----------------------------------
c--- write data from prev.dat stations
c-----------------------------------

          if (lprev) then
            read(ioprev,1198)dateline
            do k=1,ipnumb
              read(ioprev,1198)pline
              write(ioout,1198)pline
              read(ioprev,1199)pline2,ipp
              write(ioout,1199)pline2,ipp
              if(ipp.gt.0) then
                do l=1,ipp
                  read(ioprev,1198)pline
                  write(ioout,1198)pline
                enddo
              endif
            enddo
          endif

c------------------------------------
c--- write missing data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(ioout,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999.
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, banm,rasn,rwsn, myr, mjulday, 
     *     mhr, myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, banm,rasn,rwsn,
     *     9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'3.0') then
           write(ioout,1202) idnum, bx, by, banm, rasn,rwsn,9999.0,
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.  
           write(ioout,1203) idnum, 9999., 9999., 9999., 9999., 9999., 
     &                       9999., 0
          endif

          call incr(iolst,myr,mjulday,mhr,1)
          goto 9
        endif



c-----------------------------------
c--- FILL AND/OR FLAG MISSING VALUES
c-----------------------------------
        if (tempa .gt. 998.9) then
          write(iolst,*) 'temperature missing', myr, mjulday, mhr
          tempa = 9999.
          rh = 9999.
        else
          if (dp .gt. 998.9) then
            rh = 9999.
          else
            ewd = 6.1094 * exp(17.625 * dp / (243.04 + dp))
            ewt = 6.1094 * exp(17.625 * tempa / (243.04 + tempa))
            rh = 100 * (ewd / ewt)
            if (rh .gt. 100.) rh = 100.
            if (rh .lt. 0.) rh = 0.
          endif
          tempa = tempa + 273.15
        endif

c-----------------------------------
c--- IF EITHER WIND SPEED OR DIRECTION 
c--- ARE MISSING SET BOTH TO MISSING
c-----------------------------------
        if (wd .gt. 998. .or. ws .gt. 98.9) then
c--- missing winds not to list file as they are not required
c          write(iolst,*) 'winds missing', myr, mjulday, mhr
          wd = 9999.
          ws = 9999.
        endif

c-----------------------------------
c--- MISSING TEMPERATURE
c-----------------------------------
        if (temps .gt. 998.9) then
          temps = 9999.
        else
          temps = 273.15 + temps
        endif

        if (tempa .lt. 9000 .and. temps .lt. 9000.) then
          diff = tempa - temps
        else
          diff = 9999.
          write(iolst,*) 'delta temperature missing', myr, mjulday, mhr
        endif
c-----------------------------------
c--- MISSING WAVE PERIOD
c--- ALSO MAP 0.0 WAVE PERIOD to 99.0
c-----------------------------------
        if (dwp .gt. 98.9) then
          dwp = 9999.
        elseif(dwp.eq.0.0) then
          dwp = 99.0
        endif

c-----------------------------------
c--- MISSING WAVE PERIOD
c--- ALSO MAP 0.0 WAVE PERIOD to 99.0
c-----------------------------------
        if (swh .gt. 98.9) then
          swh = 9999.
        endif

c-----------------------------------
c--- COMPUTE DT
c-----------------------------------

        if (tempa .lt. 9000 .and. temps .lt. 9000.) then
          diff = tempa - temps
        else
          diff = 9999.
        endif


c-----------------------------------
c--- WIRTE OUT HOURLY RECORD
c-----------------------------------
      if (dataver.eq.'2.2'.or.dataver.eq.'3.0') then
          write(ioout,1200) myr, mjulday, mhr, 0000, 
     &                      myr, mjulday, mhr, 3600
      endif

c-----------------------------------
c--- write data from prev.dat stations
c-----------------------------------

          if (lprev) then
            read(ioprev,1198)dateline
            do k=1,ipnumb
              read(ioprev,1198)pline
              write(ioout,1198)pline
              read(ioprev,1199)pline2,ipp
              write(ioout,1199)pline2,ipp
              if(ipp.gt.0) then
                do l=1,ipp
                  read(ioprev,1198)pline
                  write(ioout,1198)pline
                enddo
              endif
            enddo
          endif
c------------------------------------
c--- write data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(*,*)rlonneg, rlon
           write(*,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, diff, tempa, rh,
     *     9999., 9999., 9999., ws, wd

           write(ioout,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, diff, tempa, rh,
     *     9999., 9999., 9999., ws, wd
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, diff, tempa, rh,
     *     9999., 9999., 9999., ws, wd, dwp, swh
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, banm,rasn,rwsn, myr, mjulday, mhr,
     *     myr, mjulday, mhr, diff, tempa, rh,
     *     9999., 9999., 9999., ws, wd, dwp, swh
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, banm,rasn,rwsn, diff, tempa, rh,
     *     9999., 9999., 9999., ws, wd, dwp, swh
          elseif (dataver.eq.'3.0') then
          write(ioout,1202)idnum, bx, by, banm, rasn,rwsn,9999.0,  
     &                   diff, tempa, rh, 9999.,
     &                   9999., 9999., ws, wd, 
     &                   9999., 9999.0, 9999.0, pres
          write(ioout,1203) idnum,swh, 9999.0, dwp, 
     &                      9999.0, 9999.0, 9999.0, 0
          endif

 1198   format(a180)
 1199   format(a50,i5)
 1200   format(2(i5,i4,i3,i5))
 1201   format(2(i5,i4,i3))
 1202   format(3x,i5,2f10.3,4(f7.1),f8.2,12f7.1)
 1203   format(3x,i5,6f7.1,i5)
 1204   format(4(f5.0,f4.0,f5.0))
 1205   format(2f9.2, f8.3, f5.1, 2(i5,i4,i3), 8f7.1)
 1206   format(2f9.2, f5.1, 2(i5,i4,i3), 10f7.1)
 1207   format(2f10.3, 3(f7.1), 2(i5,i4,i3), f8.2, 9f7.1)
 1208   format(2f10.3, 3(f7.1), f8.2, 9f7.1)
        goto 111

 99     continue
c----------------------------------------
c--- Back up to get the last hour 
c--- which was not in the previous file.
c----------------------------------------

        call incr(iolst,myr,mjulday,mhr,-1)
      enddo
998   continue

      call incr(iolst,myr,mjulday,mhr,1)
      if (myr.lt.ieyr.or.myr.eq.ieyr) then
         if(mjulday.lt.iejulday.or.mjulday.eq.iejulday) then
            if(mhr.lt.iehr.or.mhr.eq.iehr) then

c------------------------------------
c--- write missing data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(ioout,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999.
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, banm,rasn,rwsn, myr, mjulday, 
     *     mhr, myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, banm,rasn,rwsn,
     *     9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'3.0') then
           write(ioout,1202) idnum, bx, by, banm, rasn,rwsn,9999.0,
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.  
           write(ioout,1203) idnum, 9999., 9999., 9999., 9999., 9999., 
     &                       9999., 0
          endif
c--------------------------------------
c--- End of missing data write statement
c--------------------------------------
              goto 998
             endif
           endif
         endif   

      if (myr.lt.ieyr.or.myr.eq.ieyr) then
         if(mjulday.lt.iejulday) then

c------------------------------------
c--- write missing data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(ioout,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999.
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, banm,rasn,rwsn, myr, mjulday, 
     *     mhr, myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, banm,rasn,rwsn,
     *     9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'3.0') then
           write(ioout,1202) idnum, bx, by, banm, rasn,rwsn,9999.0,
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.  
           write(ioout,1203) idnum, 9999., 9999., 9999., 9999., 9999., 
     &                       9999., 0
          endif
c--------------------------------------
c--- End of missing data write statement
c--------------------------------------
              goto 998
             endif 
            endif

      if (myr.lt.ieyr) then

c------------------------------------
c--- write missing data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(ioout,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999.
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, banm,rasn,rwsn, myr, mjulday, 
     *     mhr, myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, banm,rasn,rwsn,
     *     9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'3.0') then
           write(ioout,1202) idnum, bx, by, banm, rasn,rwsn,9999.0,
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.  
           write(ioout,1203) idnum, 9999., 9999., 9999., 9999., 9999., 
     &                       9999., 0
          endif
c--------------------------------------
c--- End of missing data write statement
c--------------------------------------
              goto 998
         endif   

      return
      end

c
c----------------------------------------------------------------------
      subroutine rdf291
c----------------------------------------------------------------------
c
c --- BUOY   Version: 1.245      Level: 060323            RDF291
c               C. DesAutels   Earth Tech
c
c --- PURPOSE:  Read data from the NODC F291 web format and outputs
c               it in BUOY.DAT format
c
c --- UPDATES:
c
c --- Version 1.242, Level 060317 to Version 1.243, Level 060323
c
c     (1) Revised X-Y location format to allow for locations up to
c         -9999.999 km
c
c --- Version 1.22, Level 051220 to Version 1.23, Level 060119
c
c     (1) Correct convention for longitude in version 2.0 output
c         the expected convention is for west longitude to be
c         positive.
c
c --- Version 1.21, Level 051220 to Version 1.22, Level 060105
c
c     (1) Correct MOD5 output to be ending.
c     (2) Added routines to extract and convert station location
c         every hour.
c     (3) add check for "calm sea" conditions.  wave period set to 99.
c         wave height left at 0.0 m.
c
c --- Version 1.2, Level 051109 to Version 1.21, Level 051220
c
c     (1) update READCF to read in time zonein format: UTC-0500
c     (2) Create output version 2.11 which includes time zone
c         in the last record of the header.
c
c --- Version 1.0, Level 050826 to Version 1.0, Level 051109
c
c     (1) Add routines to write version 2.1 sea.dat files
c     (2) Reclassified data formats: 
c                           2.0 pre-10/2005 format
c                             with no wave parameters
c                           2.1 Based on 2.0 with
c                             2 wave parameters for wave 
c                             height and period
c                           2.2 Full wave data with 
c                             sub-hourly time steps.
c     (3) Reclassify 0.0 wave period to 99.0
c
c --- Version 1.0, Level 050117 to Version 1.1, Level 050826
c
c     (1) Add variable routines to allow for writing of version 2.0 or 
c         version 2.1 sea.dat files
c
c
c --- INPUTS:
c
c
c --- OUTPUT:
c
c
c --- RDF291 called by:  COMP
c --- RDF291 calls:      DEDAT, INCR, XTRACTLL, JULDAY
c----------------------------------------------------------------------
c --- Include file of parameters and commons

      include 'params.buy'
      include 'station.buy'
      include 'control.buy'
      include 'filnam.buy'
      include 'qa.buy'
      include 'grid.buy'
      include 'datehr.buy'


      character*160 line
      character*180 dateline, pline
      character*50 pline2
      character*11 fields
      character rec
      character*1 rlath,rlonh,fd
      character*12 caction

c--- new fields
      character*4 c4dum

      real*8 vecti(9),vecto(9)
      real rlatc,rlonc

      integer ipro, ipp

      real depth,lat,lon,anm,tempa,pres,ws,wd,prec,rnyr
      real sr1,sr2,swh,awp,mwd,temps,dwp,mwh,mws
      real rlatd,rlatm,rlats,rlond,rlonm,rlons,rdepth,rdwp
      real rsam, sam
      real rpd1, rpd2, rpd3, rpd4
      real rpt1, rpt2, rpt3, rpt4
      real rps1, rps2, rps3, rps4

      call dedat(ibdathr,ibyr,ibjulday,ibhr)
      call dedat(iedathr,ieyr,iejulday,iehr)

c-------------------------------------------------
c--- OPEN THE FIRST NODC FILE TO READ STATION DATA
c--- FROM FIRST 'A' RECORD AND THEN CLOSE
c-------------------------------------------------
      open(iobuoy,file=datafil(1),form='formatted',status='old')

      read(iobuoy,101)btyp,idnum,rhr,rmin,rlatd,rlatm,rlats,
     &  rlath,rlond,rlonm,rlons,rlonh,rdepth,rsam,fd
      close(iobuoy)

      depth=rdepth/10
      sam=rsam/10
      lat=rlatd+(rlatm/60)+(rlats/3600)
      write(blat,102)lat,rlath
      lon=rlond+(rlonm/60)+(rlons/3600)
      write(blon,102)lon,rlonh


c--------------------------------------
c--- INITIALIZE THE READ STATEMENT
c--------------------------------------
      myr =ibyr
      mjulday =ibjulday
      mhr = ibhr
      call incr(iolst,myr,mjulday,mhr,-1)
      
c--------------------------------------
c--- READ EACH NODC BUOY FILE
c--------------------------------------
      do i=1,nbdf
        open(iobuoy,file=datafil(i),form='formatted',status='old')

111    continue

       ipro=0
c---------------------------------------
c--- INCREMENT COUNTER TO THE NEXT HOUR
c--- READ IN A FULL LINE OF DATA AND EXTRACT
c--- THE RECORD LABEL
c---------------------------------------
       call incr(iolst,myr,mjulday,mhr,1)
2      continue 
       read(iobuoy,201,end = 99)line
       read(line,205)rec

201   format(a160)
205   format(9x,a1)

c-----------------------------------
c--- READ RECORD 'A' IF PRESENT
c-----------------------------------   
c
       if (rec.eq.'A') then

C------------------------------------------------
c--- Read physical location parameters and update
c--- if station has moved.
c------------------------------------------------
          read(line,101)btyp2,idnum2,rhr2,rmin,rlatd,rlatm,rlats,
     &        rlath,rlond,rlonm,rlons,rlonh,rdepth,rsam,fd

          depth=rdepth/10
          rlatc=rlatd+(rlatm/60)+(rlats/3600)
          write(blat,102)rlatc,rlath
          rlonc=rlond+(rlonm/60)+(rlons/3600)
          write(blon,102)rlonc,rlonh

          call XTRACTLL(iolst,'LAT ',BLAT,RLATc)
          call XTRACTLL(iolst,'LON ',BLON,RLONc)

c ---    Set utm zone for coordinate calls
	   iutm=iutmzn
	   if(utmhem.EQ.'S   ' .AND. xbtz.LT.900) iutm=-iutm

c ---    Convert Location to Local coordinate system
C
c ---    Set station location from LL to the output grid 
	   if(lutm) then
c ---    Using UTM grid
	      call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'UTM     ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feast,fnorth,
     &               caction,vecti,vecto)

	   elseif(llcc) then
c ---    Using Lambert Conformal grid
	      call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'LCC     ',idum,xdum,xlat1,xlat2,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)


	   elseif(lps) then
c ---    Using Polar Stereographic grid
	      call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'PS      ',idum,xdum,xlat1,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
	   elseif(lem) then
c ---    Using Equatorial Mercator grid
	      call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'EM      ',idum,xdum,xlat1,-xlat1,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
	   elseif(llaza) then
c ---    Using Lambert Azimuthal Equal Area grid
	      Call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'LAZA    ',idum,xdum,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
	   elseif(lttm) then
c ---    Using Tangential TM grid
	      call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               'TM      ',idum,tmsone,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
	   endif

c----------------------
c --- Map LL to output
c----------------------
           call GLOBE(iolst,caction,dndbc,vecti,datum,vecto,
     &           RLONc,RLATc,bx,by,idum,c4dum)
c--------------------------------------------------------------

           read(line,224)nyr,nmo,ndy,nhr
          goto 2

224       format(3x,i4,3x,5x,1x,2x,i2,i2,i2)
101   format(a3,7x,i5,7x,2(f2.0),3(f2.0),a1,f3.0,2(f2.0),
     &   a1,f5.0,58x,f3.0,2x,a1)
102   format(f7.3,a1)
103   format(2x,a1)

c-----------------------------------
c--- READ RECORD 'B' IF PRESENT
c-----------------------------------    
       else if (rec.eq.'B') then

          read(line,230)rbanm,rtempa,rdp,rpres,rws,rwd,rprec,
     &            rsr1,rsr2,rswh,rawp,rmwd,rtemps,rdwp,rmwh,rmws

          dp=rdp/10
          banm=rbanm/10
          tempa=rtempa/10
          pres=rpres/10
          ws=rws/100
          wd=rwd/10
          prec=rprec/10
          sr1=rsr1/100
          sr2=rsr2/100
          swh=rswh/10
          awp=rawp/10
          mwd=rmwd/10
          temps=rtemps/100
          dwp=rdwp/10
          mwh=rmwh/10
          mws=rmws/10

c-----------------------------------
c--- MISSING WAVE PERIOD
c--- ALSO MAP 0.0 WAVE PERIOD to 99.0
c-----------------------------------

        if(dwp.eq.0.0) then
          dwp = 99.0
        endif         

c-----------------------------------
c    FILL MISSING VALUES
c-----------------------------------
          if (line(33:33).eq.' ') tempa=9999.0
          if (line(37:37).eq.' ') dp=9999.0
          if (line(42:42).eq.' ') pres=9999.0
          if (line(46:46).eq.' ') ws=9999.0
          if (line(50:50).eq.' ') wd=9999.0
          if (line(58:58).eq.' ') prec=9999.0
          if (line(61:61).eq.' ') sr1=9999.0
          if (line(64:64).eq.' ') sr2=9999.0
          if (line(70:70).eq.' ') awp=9999.0
          if (line(77:77).eq.' ') mwd=9999.0
          if (line(83:83).eq.' ') temps=9999.0
          if (line(96:96).eq.' ') dwp=9999.0
          if (line(99:99).eq.' ') mwh=9999.0
          if (line(102:102).eq.' ') mws=9999.0

          if (line(29:29).eq.' ') then
             if ((ws.eq.9999.0).and.(wd.eq.9999.0)) then
                 banm=9999.0
             else
                 banm=ranmp
                 write(iolst,*) 'anemometer height missing', myr, 
     &                     mjulday, mhr
             endif
          else
             ranmp=banm
          endif

          if (fd.eq.'Y') goto2


230   format(26x,f3.0,f4.0,f4.0,f5.0,
     & 2(f4.0),4x,f4.0,5(f3.0),6x,f4.0,10x,3(f3.0))


c-----------------------------------
c--- READ RECORD 'D' IF PRESENT
c-----------------------------------    
       else if (rec.eq.'D') then
          ipro=4
          read(line,240)rpd1,rpt1,rps1
     &                  rpd2,rpt2,rps2
     &                  rpd3,rpt3,rps3
     &                  rpd4,rpt4,rps4

c-------------------------------
c---  FILL MISSING VALUES
c-------------------------------

          if (line(49:49).eq.' ') rpd1=9999.0
          if (line(67:67).eq.' ') rpd2=9999.0
          if (line(85:85).eq.' ') rpd3=9999.0
          if (line(103:103).eq.' ') rpd4=9999.0

          if (line(53:53).eq.' ') rpt1=9999.0
          if (line(71:71).eq.' ') rpt2=9999.0
          if (line(89:89).eq.' ') rpt3=9999.0
          if (line(107:107).eq.' ') rpt4=9999.0

          if (line(58:58).eq.' ') rps1=9999.0
          if (line(76:76).eq.' ') rps2=9999.0
          if (line(94:94).eq.' ') rps3=9999.0
          if (line(112:112).eq.' ') rps4=9999.0

240   format(44x,4(f5.0,f4.0,f5.0,4x))


c-----------------------------------
c    READ AND IGNORE OTHER RECORDS
c-----------------------------------
      else
c          read(iobuoy,201,end=99)line
          goto 2
      endif


      call julday(iolst,nyr,nmo,ndy,njulday)

c-----------------------------------------------------
c---  Determine the midpoint of the sampling period
c---  and back up if within previous hour.
c-----------------------------------------------------
      rmid=rmin-(sam/2)
      if (rmid.lt.0) then
         call incr(iolst,nyr,njulday,nhr,-1)
      endif

c-----------------------------------------------------
c---  If producing MOD5 output move time ahead to define
c---  time as hour-ending.  Time read in is hour + minute
c---  if minute is > 0 then readings within the hour would
c---  define an hour ending of the next hour.
c------------------------------------------------------

      if (dataver.eq.'2.0'.or.dataver.eq.'2.1'.or.
     & dataver.eq.'2.11') then
         call incr(iolst,nyr,njulday,nhr,1)
      endif

c---------------------------------------
c--- CONVERT TIME FROM GMT TO LST
c---------------------------------------
        ibtz=-real(xbtz)
        call incr(iolst,nyr,njulday,nhr,ibtz)

c---------------------------------------
c--- SKIP HOURS BEFORE REQUESTED PERIOD
c---------------------------------------
        if (nyr .lt. myr) goto 2
        if (njulday .lt. mjulday) goto 2
        if (nhr .lt. mhr) goto 2

c--------------------------------------
c--- EXIT AND END IF HOUR IS LATER THAN
c--- THE REQUESTED TIME PERIOD 
c--------------------------------------
 9      continue
        if (mhr .gt. iehr) then
           if (mjulday .gt. iejulday .or. mjulday .eq. iejulday) then
              if (myr .gt. ieyr .or. myr .eq. ieyr) then
                 goto 99
              endif
           endif
        endif
        if (mjulday .gt. iejulday) then
           if (myr .gt. ieyr .or. myr .eq. ieyr) then
              goto 99
           endif
        endif
        if (myr .gt. ieyr) then
            goto 99
        endif

c-----------------------------------
c--- CHECK FOR MISSING TIMES
c-----------------------------------
        if (myr .ne. nyr .or. mjulday .ne. njulday
     &      .or. mhr .ne. nhr) then
          write(iolst,*) ' missing data record ', myr,
     *     mjulday, mhr

c-----------------------------------
c--- WRITE DATA FOR MISSING HOURS
c-----------------------------------
      if (dataver.eq.'2.2'.or.dataver.eq.'3.0') then
          write(ioout,1200) myr, mjulday, mhr, 0000, 
     &                      myr, mjulday, mhr, 3600

      endif
c-----------------------------------
c--- write data from prev.dat stations
c-----------------------------------

          if (lprev) then
            read(ioprev,1198)dateline
            do k=1,ipnumb
              read(ioprev,1198)pline
              write(ioout,1198)pline
              read(ioprev,1199)pline2,ipp
              write(ioout,1199)pline2,ipp
              if(ipp.gt.0) then
                do l=1,ipp
                  read(ioprev,1198)pline
                  write(ioout,1198)pline
                enddo
              endif
            enddo
          endif

c------------------------------------
c--- write missing data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(ioout,1205) bx, by, rlonneg, 9999., myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999.
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, 9999., myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, 9999.,9999.,9999., myr, mjulday, 
     *     mhr, myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, 9999.,9999.,9999.,
     *     9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'3.0') then
           write(ioout,1202) idnum, bx, by, 9999., 9999.,9999.,9999.,
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.  
           write(ioout,1203) idnum, 9999., 9999., 9999., 9999., 9999., 
     &                       9999., 0
          endif

          call incr(iolst,myr,mjulday,mhr,1)
          goto 9
        endif


c-----------------------------------
c--- FILL AND/OR FLAG MISSING VALUES
c-----------------------------------
        if (tempa .gt. 998.9) then
          write(iolst,*) 'temperature missing', myr, mjulday, mhr
          tempa = 9999.
          rh = 9999.
        else
          if (dp .gt. 998.9) then
            rh = 9999.
          else
            ewd = 6.1094 * exp(17.625 * dp / (243.04 + dp))
            ewt = 6.1094 * exp(17.625 * tempa / (243.04 + tempa))
            rh = 100 * (ewd / ewt)
            if (rh .gt. 100.) rh = 100.
            if (rh .lt. 0.) rh = 0.
          endif
          tempa = tempa + 273.15
        endif

c-----------------------------------
c--- IF EITHER WIND SPEED OR DIRECTION 
c--- ARE MISSING SET BOTH TO MISSING
c-----------------------------------
        if (wd .gt. 998. .or. ws .gt. 98.9) then
c--- missing wind not written to data file as it's not required
c          write(iolst,*) 'winds missing', myr, mjulday, mhr
          wd = 9999.
          ws = 9999.
        endif

c-----------------------------------
c--- MISSING TEMPERATURE
c-----------------------------------
        if (temps .gt. 998.9) then
          temps = 9999.
        else
          temps = 273.15 + temps
        endif

        if (tempa .lt. 9000 .and. temps .lt. 9000.) then
          diff = tempa - temps
        else
          write(iolst,*) 'delta temperature missing',
     &    myr, mjulday, mhr
          diff = 9999.
        endif

c-----------------------------------
c--- MISSING WAVE PERIOD
c--- ALSO MAP 0.0 WAVE PERIOD to 99.0
c-----------------------------------
        if (dwp .gt. 98.9) then
          dwp = 9999.
        elseif(dwp.eq.0.0) then
          dwp = 99.0
        endif
c--------------------------------------
c--- MISSING SIGNIFICANT WAVE HEIGHT
c--------------------------------------
        if (swh .gt. 98.9) then
          swh = 9999.
        endif
c-----------------------------------
c--- WIRTE OUT HOURLY RECORD
c-----------------------------------
      if (dataver.eq.'2.2'.or.dataver.eq.'3.0') then
          write(ioout,1200) myr, mjulday, mhr, 0000, 
     &                      myr, mjulday, mhr, 3600
      endif

c-----------------------------------
c--- write data from prev.dat stations
c-----------------------------------

          if (lprev) then
            read(ioprev,1198)dateline
            do k=1,ipnumb
              read(ioprev,1198)pline
              write(ioout,1198)pline
              read(ioprev,1199)pline2,ipp
              write(ioout,1199)pline2,ipp
              if(ipp.gt.0) then
                do l=1,ipp
                  read(ioprev,1198)pline
                  write(ioout,1198)pline
                enddo
              endif
            enddo
          endif
c------------------------------------
c--- write data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(ioout,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, diff, tempa, rh,
     *     9999., 9999., 9999., ws, wd
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, diff, tempa, rh,
     *     9999., 9999., 9999., ws, wd, dwp, swh
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, banm,rasn,rwsn, myr, mjulday, mhr,
     *     myr, mjulday, mhr, diff, tempa, rh,
     *     9999., 9999., 9999., ws, wd, dwp, swh
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, banm,rasn,rwsn, diff, tempa, rh,
     *     9999., 9999., 9999., ws, wd, dwp, swh
          elseif (dataver.eq.'3.0') then
          write(ioout,1202)idnum, bx, by, banm, rasn,rwsn,depth,  
     &                   diff, tempa, rh, 9999.,
     &                   9999., 9999., ws, wd, 
     &                   9999., 9999.0, 9999.0, pres
          write(ioout,1203) idnum,swh, awp, dwp, 
     &                      mwd, mwh, mws, 0
          endif

 1198   format(a180)
 1199   format(a50,i5)
 1200   format(2(i5,i4,i3,i5))
 1201   format(2(i5,i4,i3))
 1202   format(3x,i5,2f10.3,4(f7.1),f8.2,12f7.1)
 1203   format(3x,i5,6f7.1,i5)
 1204   format(4(f5.0,f4.0,f5.0))
 1205   format(2f9.2, f8.3, f5.1, 2(i5,i4,i3), 8f7.1)
 1206   format(2f9.2, f5.1, 2(i5,i4,i3), 10f7.1)
 1207   format(2f10.3, 3(f7.1), 2(i5,i4,i3), f8.2, 9f7.1)
 1208   format(2f10.3, 3(f7.1), f8.2, 9f7.1)
        goto 111

 99     continue
c----------------------------------------
c--- Back up to get the last hour 
c--- which was not in the previous file.
c----------------------------------------

        call incr(iolst,myr,mjulday,mhr,-1)
      enddo
998   continue

      call incr(iolst,myr,mjulday,mhr,1)
      if (myr.lt.ieyr.or.myr.eq.ieyr) then
         if(mjulday.lt.iejulday.or.mjulday.eq.iejulday) then
            if(mhr.lt.iehr.or.mhr.eq.iehr) then

c------------------------------------
c--- write missing data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(ioout,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999.
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, banm,rasn,rwsn, myr, mjulday, 
     *     mhr, myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, banm,rasn,rwsn,
     *     9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'3.0') then
           write(ioout,1202) idnum, bx, by, banm, rasn,rwsn,9999.0,
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.  
           write(ioout,1203) idnum, 9999., 9999., 9999., 9999., 9999., 
     &                       9999., 0
          endif
c--------------------------------------
c--- End of missing data write statement
c--------------------------------------
              goto 998
             endif
           endif
         endif   

      if (myr.lt.ieyr.or.myr.eq.ieyr) then
         if(mjulday.lt.iejulday) then

c------------------------------------
c--- write missing data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(ioout,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999.
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, banm,rasn,rwsn, myr, mjulday, 
     *     mhr, myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, banm,rasn,rwsn,
     *     9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'3.0') then
           write(ioout,1202) idnum, bx, by, banm, rasn,rwsn,9999.0,
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.  
           write(ioout,1203) idnum, 9999., 9999., 9999., 9999., 9999., 
     &                       9999., 0
          endif
c--------------------------------------
c--- End of missing data write statement
c--------------------------------------
              goto 998
             endif 
            endif

      if (myr.lt.ieyr) then

c------------------------------------
c--- write missing data from new station
c------------------------------------
          if (dataver.eq.'2.0') then
           rlonneg=-rlon
           write(ioout,1205) bx, by, rlonneg, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999.
          elseif (dataver.eq.'2.1') then
           write(ioout,1206) bx, by, banm, myr, mjulday, mhr,
     *     myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.11') then
           write(ioout,1207) bx, by, banm,rasn,rwsn, myr, mjulday, 
     *     mhr, myr, mjulday, mhr, 9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'2.2') then
           write(ioout,1208) bx, by, banm,rasn,rwsn,
     *     9999., 9999., 9999.,
     *     9999., 9999., 9999., 9999.,9999., 9999., 9999.
          elseif (dataver.eq.'3.0') then
           write(ioout,1202) idnum, bx, by, banm, rasn,rwsn,9999.0,
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.,  
     &                     9999., 9999., 9999., 9999.  
           write(ioout,1203) idnum, 9999., 9999., 9999., 9999., 9999., 
     &                       9999., 0
          endif
c--------------------------------------
c--- End of missing data write statement
c--------------------------------------
              goto 998
         endif   


      return
      end
