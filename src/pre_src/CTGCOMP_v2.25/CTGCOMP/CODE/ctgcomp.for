c ---------------------------------------------------------------------
c --- CTGCOMP - Composite Theme Grid (CTG) land use file compressor
c----------------------------------------------------------------------
c
c --- CTGCOMP  Version: 2.25      Level: 070327                    MAIN
c              E. Insley
c
c     Copyright (c) 1996-2007 by Exponent, Inc.         
c
c --- PURPOSE: Compresses a USGS Composite Theme Grid (CTG) land use
c              and land cover data file.  Packs the data across a row.
c              Both the input and output data files are ASCII.
c
c --- UPDATES
c
c --- Ver 2.24 Level 060519 to Ver 2.25 Level 070327    - D. Strimaitis
c              - CALUTILS from v2.52 Level 060519 to v2.55 Level 070327
c                Move GLOBE1 to COORDLIB
c                Allow negative increments in INCRS
c                Fixed format bug in subroutine BASRUTC for the case of
c                time zone zero (output string was 'UTC+0  0' instead
c                of 'UTC+0000'
c                Modified:  INCRS, UTCBASR, BASRUTC
c                Removed:   GLOBE1
c
c --- Ver 2.23 Level 060309 to Ver 2.24 Level 060519    - D. Strimaitis
c              - CALUTILS from v2.51 Level 051019 to v2.52 Level 060519
c                Variable names in control file are not processed
c                correctly if there are too many characters (including
c                blanks) to the left of the "=" sign (run stops in setup
c                phase).
c                Modified:  READIN
c
c --- Ver 2.22 Level 030528 to Ver 2.23 Level 060309    - D. Strimaitis
c              - Updated to CALUTILS V2.51 (051019) from V2.2 (0030528)
c              - Filnames changed from c*70 to c*132 (for CALUTILS V2.3
c                and later)
c                Modified:  FILNAM.CMP
c                           READCF, SETUP
c
c --- ver 2.21 Level 030402 to Ver 2.22 Level 030528    - D. Strimaitis
c              - Updated CALUTILS (Version 2.2, Level 030528)
c
c --- Ver 2.2  Level 020828 to Ver 2.21 Level 030402    - D. Strimaitis
c              - Fixed bug in writing out 'gap' missing indicators
c              - Updated CALUTILS (Version 2.1, Level 030402)
c              - Replace IOMESG with IO6 in 2nd JULDAY call in FIN
c
c --- Ver 2.1  Level 020424 to Ver 2.2 Level 020828    - D. Strimaitis
c              - Updated CALUTILS (Version 1.1, Level 020828)
c
c --- Ver 2.1  Level 011003 to Level 020424    - M. Borissova
c              - Allow for gaps in land use along a strip
c
c --- Ver 2.0  Level 961113 to Level 011003    - D. Strimaitis
c              - Restructure inputs for CALPUFF system control file
c              - Restructure main program as subroutine COMP
c              - Place system-wide utilities into an include module
c                (calutil.for)
c----------------------------------------------------------------------
      program CTGCOMP
c
c --- Include parameters
      include 'params.cmp'
c --- Include common blocks
      include 'qa.cmp'
c
c --- Set version and level number of program (stored in /QA/ and
c --- checked against values set in PARAMS.CMP)
      ver='2.25'
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
c --- CTGCOMP   Version: 2.25           Level: 011003        BLOCK DATA
c               D. Strimaitis, Earth Tech, Inc.
c
c --- Include parameter statements
      include 'params.cmp'
c
c --- Include common blocks
      include 'filnam.cmp'

c --- FILNAM common block
      data runinp/'ctgcomp.inp'/,complst/'ctgcomp.lst'/,
     1     ctgfil/'ctg.dat'/,compfil/'ctgcomp.dat'/
c --- FILLOG common block
      data lcfiles/.true./

      end

c----------------------------------------------------------------------
c --- BRING IN CALPUFF SYSTEM UTILITY SUBROUTINES
      include 'calutils.for'
c----------------------------------------------------------------------

c-----------------------------------------------------------------------
      subroutine setup
c-----------------------------------------------------------------------
c
c --- CTGCOMP   Version: 2.25           Level: 060309             SETUP
c               D. Strimaitis, EarthTech, Inc.
c
c
c PURPOSE:     SETUP reads and checks the control data provided, sets
c              logicals, echoes the control data, and opens the data
c              files if inputs are valid.
c
c --- Updates
c     Ver 2.23 Level 060309 from Ver 2.0 Level 011003       DGS
c              Filenames from c*70 to c*132 for CALUTILS V2.3 and later
c
c ARGUMENTS:
c    PASSED:  /QA/       ver,level
c
c  RETURNED:  /FILNAM/   file names
c             /QA/       rdate,rtime,rcpu
c
c CALLING ROUTINES:   MAIN
c
c EXTERNAL ROUTINES:  READCF
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.cmp'
      include 'filnam.cmp'
      include 'qa.cmp'

c --- Get date and time from system
      call DATETM(rdate,rtime,rcpu)

c --- Get the name of the control file from the
c --- command line
      call COMLINE(runinp)

c --- Open the control file
      open(iocnt,file=runinp,status='old')

c --- Report progress
      write(iomesg,*)'SETUP PHASE'

c --- Check that the version and level number in the parameter
c --- file matches those in the code itself
      if(ver.ne.mver.or.level.ne.mlevel)then
         write(iomesg,10) ver,level,mver,mlevel
10       format(/1x,'ERROR in SUBR. SETUP -- The CTGCOMP version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open list file)
      call READCF

c --- Open remaining files
      open(ioctg,file=ctgfil)
      open(iocomp,file=compfil)

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
      write(iolst,*) 'File Names ------------'
      write(iolst,'(a19,a132)') 'List File        : ' ,complst
      write(iolst,'(a19,a132)') 'Input CTG File   : ' ,ctgfil
      write(iolst,'(a19,a132)') 'COMPRESSED FILE  : ' ,compfil
      write(iolst,*)

      return
      end
c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- CTGCOMP   Version: 2.25           Level: 060309            READCF
c               D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  Read the control file containing the file names of
c               the input and output files of the run
c
c --- Updates
c     Ver 2.23 Level 060309 from Ver 2.0 Level 011003       DGS
c              Filenames from c*70 to c*132 for CALUTILS V2.3 and later
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c
c        Parameters: IOCNT, IOLST, IOMESG, MXVAR
c
c --- OUTPUT:
c
c ---    Common block /FILNAM/ variables:
c           complst,ctgfil,compfil,
c           lcfiles
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.cmp'
      include 'params.cal'
c
c --- Include common blocks
      include 'filnam.cmp'
      include 'qa.cmp'
c
c --- Local variables (currently set for 1 input group)
      character*4 ctemp(132,3)
      character*12 cvdic(mxvar,1)
      integer ivleng(mxvar,1),ivtype(mxvar,1)
      logical lecho

c --- Initialize local variables
      data lecho/.false./

c --- Set Dictionary
      data cvdic/'CTGFIL','COMPFIL','COMPLST','LCFILES',  56*' '/
      data ivleng/3*132,1, 56*0/
      data ivtype/3*4,3, 56*0/
c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character

c ------------------
c --- Input Group 0
c ------------------

c --- Initialize the temporary arrays
      do i=1,3
         do j=1,132
            ctemp(j,i)(1:1)=' '
         enddo
      enddo

c --- Read the group data
       call READIN(cvdic(1,1),ivleng(1,1),ivtype(1,1),iocnt,iomesg,
     & lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),lcfiles,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum)

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')ctgfil=' '
      if(ctemp(1,2)(1:1).ne.' ')compfil=' '
      if(ctemp(1,3)(1:1).ne.' ')complst=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')ctgfil(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')compfil(j:j)=ctemp(j,2)(1:1)
         if(ctemp(j,3)(1:1).ne.' ')complst(j:j)=ctemp(j,3)(1:1)
      enddo

c --- Convert the file names to the proper case
      call FILCASE(lcfiles,ctgfil)
      call FILCASE(lcfiles,compfil)
      call FILCASE(lcfiles,complst)

c --- Open listfile
      open(iolst,file=complst,status='unknown')

c --- Write banner to list file
      write(iolst,5) ver,level
5     format(///,26x,'CTGCOMP OUTPUT SUMMARY',/,19x,'VERSION:  ',A12,
     1       ' LEVEL:  ',A12///)

      return
      end
c----------------------------------------------------------------------
      subroutine comp
c----------------------------------------------------------------------
c
c --- CTGCOMP   Version: 2.25           Level: 030402              COMP
c               E. Insley,  EARTH TECH, Inc.
c
c --- PURPOSE:  Main computational routine
c
c --- UPDATES
c
c --- Ver 2.21 Level 020424 to Level 030402    - D. Strimaitis
c              - Activate 'write(2,40)' output lines, using
c                unit iocomp
c --- Ver 2.1  Level 011003 to Level 020424    - M. Borissova
c              - Allow for gaps in land use along a strip
c
c --- INPUTS:
c       Parameters: IOLST, IOCTG, IOCOMP
c
c --- OUTPUT:  none
c
c --- COMP called by:  MAIN
c --- COMP calls:      none
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.cmp'

      character*80 cline

c --- Initialize Counters
      nrec = 0
      nmiss = 0

c --- Read and Write Data File Header Records
      write(iolst,*) 'CTG Data File Header Records'
      do i=1,5
        read(ioctg,10) cline
10      format(a80)
        if(i.EQ.1) then
          read(cline,'(35x,i5)') nx
        endif
        nrec = nrec + 1
        write(iocomp,10) cline
        write(iolst,10) cline
      enddo
      
      nxstep = abs(nx)
c     write(*,*)'nxstep', nxstep

c --- Set flag for first record read
      ifirst = 1

c --- Read data records
1     read(ioctg,20,end=999) ixutm,iyutm,lucat
20    format(3x,2i8,1x,i10)
      nrec = nrec + 1
c --- Check for valid Land Use category
      if(lucat.LE.0 .OR. lucat.GE.93)then
        lucat = 999
        nmiss = nmiss + 1
      endif

      if(ifirst.EQ.1)then
c ---   Write new coordinate to output file
        write(iocomp,30) 0,0,ixutm,iyutm
30      format(i4,i3,i8,i9)
c ---   Save data used for comparison in compressing algorithm
        iyold = iyutm
        ixold = ixutm
        luold = lucat
c ---   Initialize count
        ncount = 0
      endif

c --- Check for same row, same land use
      if(iyutm.EQ.iyold)then
c ---   Same Row
c ---   Check for skipped cells in x
        idx = abs(ixutm - ixold)
        ixold = ixutm
        if(idx.GT.nxstep) then
          nxmiss = int(idx/200) - 1
          write(iocomp,40) ncount,luold
40        format(i4,i3)
          luold = lucat
          ncount = 0
          write(iocomp,40) nxmiss,999
          nmiss = nmiss + nxmiss
        else
          continue
        endif
        if(lucat.NE.luold)then
c ---     New Land Use, write count for old land use and reset count
          write(iocomp,40) ncount,luold
          luold = lucat
          ncount = 1
        else
c---      Same Land Use Category, same row, increment count
          ncount = ncount + 1
        endif
      else
c ---   New Row, write last land use count info.
        write(iocomp,40) ncount,luold
c ---   Write new coordinate to output file
        write(iocomp,30) 0,0,ixutm,iyutm
c ---   Save new info.
        ixold = ixutm
        iyold = iyutm
        luold = lucat
        ncount = 1
      endif

c --- Reset first record flag
      ifirst = 0

c --- Loop back to read next record
      go to 1

c --- End of File
999   continue
c --- Write last count info.
      write(iocomp,40) ncount,luold
c --- Write summary info. to list file
      write(iolst,*)
      write(iolst,*) 'Compression Complete !'
      write(iolst,*) '  Total Number of Records Read: ',nrec
      write(iolst,*) '  Total Number of Missing LU Categories: ',nmiss
      write(iomesg,*) 'Compression Complete !'
      write(iomesg,*) '  Total Number of Records Read: ',nrec
      write(iomesg,*) '  Total Number of Missing LU Categories: ',nmiss

      return
      end
c----------------------------------------------------------------------
      subroutine fin
c----------------------------------------------------------------------
c
c --- CTGCOMP  Version: 2.25            Level: 030402               FIN
c ---          J. Scire, Earth Tech, Inc.
c
c --- PURPOSE:  Run termination routine -- compute runtime
c
c --- UPDATES
c
c --- Ver 2.21 Level 011003 to Level 030402    - D. Strimaitis
c              - Replace IOMESG with IO6 in 2nd JULDAY call in FIN
c
c --- INPUTS:
c       Common block /QA/
c          rdate, rtime, rcpu
c       Parameters: IOLST, IOMESG
c
c --- OUTPUT:  none
c
c --- FIN called by:  MAIN
c --- FIN calls:      DATETM, JULDAY, DELTT
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.cmp'
      include 'qa.cmp'
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
      write(iolst,1402)rtime2,rdate2,delt,rcpu
1402  format(//2x,'End of run -- Clock time: ',a8/
     1         2x,'                    Date: ',a10//
     2         2x,'      Elapsed Clock Time: ',f10.1,' (seconds)'//
     3         2x,'                CPU Time: ',f10.1,' (seconds)')

c
      return
      end
