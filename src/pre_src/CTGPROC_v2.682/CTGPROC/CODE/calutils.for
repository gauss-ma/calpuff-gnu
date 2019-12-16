c------------------------------------------------------------------------------
c --- CALUTILS -- CALPUFF SYSTEM UTILITIES
c------------------------------------------------------------------------------
c
c --- CALUTILS   Version: 5.8.4    Level: 130731
C                EPA
C
c ---            Version: 2.55     Level: 070327
c                TRC
c -----------------------------
c --- CONTENT:
c -----------------------------
c --- Coordinates
c      subroutine xtractll
c --- Year 2000
c      subroutine yr4
c      subroutine yr4c
c      subroutine qayr4
c --- Date/Time
c      subroutine julday
c      subroutine grday
c      subroutine dedat
c      subroutine deltt
c      subroutine incr
c      subroutine indecr
c      subroutine incrs
c      subroutine deltsec
c      subroutine midnite
c      subroutine basrutc
c      subroutine utcbasr
c --- Control file
c      subroutine filcase
c      subroutine readin
c      subroutine altonu
c      subroutine deblnk
c      subroutine deplus
c      subroutine tright
c      subroutine tleft
c      subroutine setvar
c      subroutine allcap
c --- System
c      subroutine datetm
c      subroutine etime
c      subroutine undrflw
c      subroutine comline
c -----------------------------
c
c --- UPDATE
C --- V5.8.4        130731   : EPA released version
C                              No changes to version 080407
C
c --- v2.55 - v2.56 080407   : Exponential notation processing in ALTONU 
c                              did not properly interpret an entry without 
c                              a decimal point.
c
c --- V2.54-V2.55 070327(DGS): Format for output time zone stringin BASRUTC
c                              wrote zone zero as 'UTC+0  0' instead of
c                              'UTC+0000'
c                              Add RETURN statement to BASRUTC and UTCBASR
c --- V2.53-V2.54 061020(DGS): Allow negative increments in INCRS
c --- V2.52-V2.53 060626(DGS): Remove routine GLOBE1 (move to COORDLIB)
c --- V2.51-V2.52 060519(DGS): Modify search for '=' in READIN to allow
c                              for blanks between c*12 variable name and
c                              the '=' sign (internal blanks are not removed
c                              after V2.2)
c --- V2.5-V2.51 051019 (KAM): Add Albers Conical Equal Area projection
c                              in GLOBE1
c --- V2.4-V2.5  041123 (FRR): add subroutine BASRUTC to convert real
c                              base time zone to character UTC time zone
c                              and UTCBASR for the backward conversion
c --- V2.3-V2.4  041029 (DGS): Add routine INCRS to change time by a
c                              number of seconds
c                              Add routine MIDNITE - converts timestamp
c                              from day N, time 0000
c                              to day N-1, time 2400
c --- V2.2-V2.3  040330 (DGS): Replace filename strings c*70 with c*132
c                              (FILCASE, COMLINE)
c                              Allow for spaces within pathnames by adding
c                              new TLEFT and TRIGHT trim subroutines
c --- V2.1-V2.2  030528 (DGS): Screen for valid UTM zone using
c                              absolute value (S. Hem. zones are
c                              negative) in GLOBE1
c --- V2.0-V2.1  030402 (DGS): Remove routine GLOBE
c                              Split DEBLNK action (removes ' ', '+')
c                              into DEBLNK and DEPLUS
c                              Add routine UNDRFLW
c                              Add false Easting and Northing (GLOBE1)
c                              Add TYPE argument to XTRACTLL
c                              Change format XTRACTLL (f16) to (f16.0)
c --- V1.1-V2.0  021018 (DGS): Add routines for new COORDS
c --- V1.0-V1.1  020828 (DGS): Add check for YYYY on input   (YR4C)
c
c
c----------------------------------------------------------------------
      subroutine xtractll(io,type,clatlon,rlatlon)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 030402               XTRACTLL
c                D. Strimaitis   EarthTech
c
c --- PURPOSE:  Extract the real latitude or longitude from a character
c               string that contains the N/S or E/W convention
c               character, and express result as either North Latitude
c               or East Longitude
c
c --- UPDATE
c --- V2.1 (030402) from V2.0 (010713) (DGS)
c               - Add TYPE argument for QA
c               - Change format (f16) to (f16.0) to satisfy different
c                 compilers
c
c
c --- INPUTS:
c               IO - integer    - Unit number for list file output
c             TYPE - char*4     - LAT or LON
c          CLATLON - char*16    - Latitude or longitude (degrees), with
c                                 1 character that denotes convention
c                                 (e.g. 'N  45.222' or  '-35.999s')
c
c --- OUTPUT:
c          RLATLON - real       - North Latitude or East Longitude
c                                 (degrees)
c
c --- XTRACTLL called by: (utility)
c --- XTRACTLL calls:     DEBLNK, ALLCAP
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cal'

      character*1 cstor1(mxcol),cstor2(mxcol)
      character*16 clatlon, clatlon2
      character*4 type
      logical ltype

      ltype=.FALSE.

c --- Initialize character variables for output
      clatlon2='                '
      do i=1,20
         cstor2(i)=' '
      enddo

c --- Was valid type provided?
      if(type.NE.'LAT ' .AND. type.NE.'LON ') then
         write(io,*) 'XTRACTLL:  FATAL ERROR reported when ',
     &               'extracting Latitude/Longitude'
         write(io,*) 'Invalid type:  ',type
         write(io,*) 'Expected LAT or LON'
         write(*,*)
         stop 'Halted in XTRACTLL -- see list file'
      endif

c --- Pass c*16 string into storage array 1
      do i=1,16
         cstor1(i)=clatlon(i:i)
      enddo
c --- Pad out to position 20
      do i=17,20
         cstor1(i)=' '
      enddo

c --- Remove blank characters from string, place in storage array 2
c --- (Use a 20-character field here for a margin at end of string)
      call DEBLNK(cstor1,1,20,cstor2,nlim)
c
c --- Convert lower case letters to upper case
      call ALLCAP(cstor2,nlim)

c --- Interpret valid convention character (N,S,E,W)
      nchar=0
      ichar=0
      ilat=0
      ilon=0
   
      do i=1,nlim
         if(cstor2(i).EQ.'N') then
            ilat=1
            ichar=i
            nchar=nchar+1
         elseif(cstor2(i).EQ.'S') then
            ilat=2
            ichar=i
            nchar=nchar+1
         elseif(cstor2(i).EQ.'W') then
            ilon=1
            ichar=i
            nchar=nchar+1
         elseif(cstor2(i).EQ.'E') then
            ilon=2
            ichar=i
            nchar=nchar+1
         endif
      enddo

c --- Was 1 valid character found?
      if(nchar.NE.1) then
         write(io,*) 'XTRACTLL:  FATAL ERROR reported when ',
     &               'extracting Latitude/Longitude'
         write(io,*) 'N,S,E,W character is missing or repeated'
         write(io,*) 'Lat/Lon = ',clatlon
         write(*,*)
         stop 'Halted in XTRACTLL -- see list file'
      endif

c --- Was valid character the right type?
      if(type.EQ.'LAT ' .AND. ilat.EQ.0) ltype=.TRUE.
      if(type.EQ.'LON ' .AND. ilon.EQ.0) ltype=.TRUE.
      if(LTYPE) then
         write(io,*) 'XTRACTLL:  FATAL ERROR reported when ',
     &               'extracting Latitude/Longitude'
         write(io,*) 'N,S,E,W character does not match type'
         write(io,*) 'Lat/Lon = ',clatlon
         write(io,*) 'type    = ',type
         write(*,*)
         stop 'Halted in XTRACTLL -- see list file'
      endif

c --- Remove character from string
      do i=ichar,nlim
         cstor2(i)=cstor2(i+1)
      enddo

c --- Search for position of decimal point
      ipt=0
      do i=1,nlim
         if(cstor2(i).EQ.'.') ipt=i
      enddo

c --- Add a decimal point if needed
      if(ipt.EQ.0) then
         cstor2(nlim)='.'
      endif

c --- Pass resulting "number" back into c*16 variable
      do i=1,nlim
         clatlon2(i:i)=cstor2(i)
      enddo

c --- Get real part
      read(clatlon2,'(f16.0)') rlatlon

c --- Convert to either N. Lat. or E. Lon., if needed
      if(ilat.EQ.2) then
         rlatlon=-rlatlon
      elseif(ilon.EQ.1) then
         rlatlon=-rlatlon
      endif

c --- Condition longitude to be -180 to +180
      if(ilon.GT.0) then
         if(rlatlon.GT.180.) then
            rlatlon=rlatlon-360.
         elseif(rlatlon.LT.-180.) then
            rlatlon=rlatlon+360.
         endif
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine yr4(io,iyr,ierr)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 991104                    YR4
c ---            D. Strimaitis,   Earth Tech
c
c --- PURPOSE: Checks/converts 2-digit year to 4-digit year
c
c --- INPUTS:
c                IO - integer    - Unit number for list file output
c               IYR - integer    - Year (YYYY or YY)
c
c     Common block /Y2K/:
c             IYYLO - integer    - Smallest 2-digit year for which
c                                  'old' century marker is used
c             ICCLO - integer    - 2-digit ('old') century
c
c --- OUTPUT:
c               IYR - integer    - Year (YYYY)
c              IERR - integer    - Error code: 0=OK, 1=FATAL
c
c --- YR4 called by:  Input routines reading 'year' data
c --- YR4 calls:      none
c----------------------------------------------------------------------
c
      common/y2k/iyylo,icclo

      ierr=0

c --- Test for 4-digit year (must exceed 1000)
      if(iyr.GT.1000) then
c ---    Passes 11th Century test (large year not trapped)
         return
      elseif(iyr.LT.100 .AND. iyr.GE.0) then
c ---    2-digit year
c ---    Construct 4-digit year
         if(iyr.LT.iyylo) then
            iyr=(icclo+1)*100+iyr
         else
            iyr=icclo*100+iyr
         endif
      else
c ---    Year not recognized
         ierr=1
         write(io,*)'ERROR in YR4 --- Year not recognized: ',iyr
         write(*,*)'ERROR in YR4 --- Year not recognized: ',iyr
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine yr4c(iyr)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 020828                   YR4C
c ---            D. Strimaitis,  Earth Tech
c
c --- PURPOSE: Checks/converts 2-digit year to 4-digit year (CURRENT)
c
c --- UPDATE
c --- V1.0-V1.1     020828  (DGS): Add check for YYYY on input
c
c --- INPUTS:
c               IYR - integer    - Year (YYYY or YY)
c
c --- OUTPUT:
c               IYR - integer    - Year (YYYY)
c
c --- YR4C called by:  host subroutines
c --- YR4C calls:      none
c----------------------------------------------------------------------
c --- Set parameters for converting a current year (1999 - 2098)
c --- Use KCCLO as century digits for years GE KYYLO
      data kyylo/99/, kcclo/19/

c --- Test for 4-digit year (must exceed 1000)
      if(iyr.GT.1000) then
c ---    Passes 11th Century test (large year not trapped)
         return
      elseif(iyr.LT.100 .AND. iyr.GE.0) then
c ---    2-digit year
c ---    Construct 4-digit year
         if(iyr.LT.kyylo) then
            iyr=(kcclo+1)*100+iyr
         else
            iyr=kcclo*100+iyr
         endif
      else
c ---    Year not recognized
         write(*,*)'ERROR in YR4C --- Year not recognized: ',iyr
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine qayr4(io,iyr,metrun,ierr)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 991104                  QAYR4
c ---            D. Strimaitis,   Earth Tech
c
c --- PURPOSE: Defines century and year markers to use in converting
c ---          2-digit year to 4-digit year
c ---          The IBYR (YYYY) must be provided in the control file
c
c --- INPUTS:
c                IO - integer    - Unit number for list file output
c               IYR - integer    - Year provided for start of run
c            METRUN - integer    - Flag to run period in met file
c                                  0 = do not run period
c                                  1 = run period
c
c --- OUTPUT:
c              IERR - integer    - Error code: 0=OK, 1=FATAL
c
c     Common block /Y2K/:
c             IYYLO - integer    - Smallest 2-digit year for which
c                                  'old' century marker is used
c             ICCLO - integer    - 2-digit ('old') century
c
c --- QAYR4 called by:  host subroutines
c --- QAYR4 calls:      none
c----------------------------------------------------------------------
c
      common/y2k/iyylo,icclo

c --- Sets parameters for the starting century marker (CC) and the
c --- 2-digit year (YY) used as the marker between the starting century
c --- and the next century.  For example, if CC=19 and YY=30, then a
c --- year less than 30 (say 15) is assumed to be 2015.  Any year
c --- greater than or equal to 30 (say 56) is assumed to be 1956.

c --- Set number of years prior to start of simulation that must not
c --- be placed in the next century
      data ibackyr/50/

      ierr=0

c --- Expect explicit starting year (YYYY)
c --- Test for 4-digit year (must exceed 1000)
      if(iyr.GT.1000) then
c ---    Passes 11th Century test (large year not trapped)
c ---    Back up IBACKYR years to set IYYLO
         kyr=iyr-ibackyr
c ---    Extract starting 2-digit century and 2-digit year
         icclo=kyr/100
         iyylo=kyr-icclo*100

c ---    Warn user that control file input is used to convert to YYYY
         iyr1=icclo*100+iyylo
         iyr2=(icclo+1)*100+iyylo-1
         write(io,*)
         write(io,*)'-------------------------------------------------'
         write(io,*)'NOTICE: Starting year in control file sets the'
         write(io,*)'        expected century for the simulation.  All'
         write(io,*)'        YY years are converted to YYYY years in'
         write(io,*)'        the range: ',iyr1,iyr2
         write(io,*)'-------------------------------------------------'
         write(io,*)
      else
         ierr=1
         write(*,*)
         write(*,*)'--------------------------------------------'
         write(*,*)'QAYR4 -- Start year must be 4-digits!: ',iyr
         if(metrun.EQ.1) then
            write(*,*)'         and must always be provided'
         endif
         write(*,*)'--------------------------------------------'
         write(*,*)
         write(io,*)
         write(io,*)'-------------------------------------------'
         write(io,*)'QAYR4 -- Start year must be 4-digits!: ',iyr
         if(metrun.EQ.1) then
            write(io,*)'         and must always be provided'
         endif
         write(io,*)'-------------------------------------------'
         write(io,*)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine julday(io,iyr,imo,iday,ijuldy)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 000602                 JULDAY
c ---            J. Scire, SRC
c
c --- PURPOSE:  Compute the Julian day number from the Gregorian
c               date (month, day)
c
c --- UPDATE
c ---               000602  (DGS): YYYY format for year
c
c --- INPUTS:
c            IO - integer      - Unit number for list file output
c           IYR - integer      - Year
c           IMO - integer      - Month
c          IDAY - integer      - Day
c
c --- OUTPUT:
c          IJUL - integer      - Julian day
c
c --- JULDAY called by:  host subroutines
c --- JULDAY calls:      none
c----------------------------------------------------------------------
c
      integer kday(12)
      data kday/0,31,59,90,120,151,181,212,243,273,304,334/
c
c --- Check for valid input data
      ierr=0
c --- Check for valid month
      if(imo.lt.1.or.imo.gt.12)ierr=1
c --- Check for valid day in 30-day months
      if(imo.eq.4.or.imo.eq.6.or.imo.eq.9.or.imo.eq.11)then
         if(iday.gt.30)ierr=1
      else if(imo.eq.2)then
         if(mod(iyr,4).eq.0)then
c ---       February in a leap year
            if(iday.gt.29)ierr=1
         else
c ---       February in a non-leap year
            if(iday.gt.28)ierr=1
         endif
      else
c ---    Check for valid day in 31-day months
         if(iday.gt.31)ierr=1
      endif
c
      if(ierr.eq.1)then
         write(io,*)
         write(io,*)'ERROR in SUBR. JULDAY'
         write(io,*)'Invalid date - IYR = ',iyr,' IMO = ',
     1    imo,' IDAY = ',iday
         write(*,*)
         stop 'Halted in JULDAY -- see list file.'
      endif
c
c --- Compute the Julian day
      ijuldy=kday(imo)+iday
      if(imo.le.2)return
      if(mod(iyr,4).EQ.0)ijuldy=ijuldy+1
c
      return
      end
c----------------------------------------------------------------------
      subroutine grday(io,iyr,ijul,imo,iday)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 000602                  GRDAY
c                J. Scire, SRC
c
c --- PURPOSE:  Compute the Gregorian date (month, day) from the
c               Julian day
c
c --- UPDATE
c ---               000602  (DGS): YYYY format for year
c
c --- INPUTS:
c            IO - integer      - Unit number for list file output
c           IYR - integer      - Year
c          IJUL - integer      - Julian day
c
c --- OUTPUT:
c           IMO - integer      - Month
c          IDAY - integer      - Day
c
c --- GRDAY called by:  host subroutines
c --- GRDAY calls:      none
c----------------------------------------------------------------------
c
      integer kday(12,2)
      data kday/31,59,90,120,151,181,212,243,273,304,334,365,
     1          31,60,91,121,152,182,213,244,274,305,335,366/
c
c
      ileap=1
      if(mod(iyr,4).eq.0)ileap=2
      if(ijul.lt.1.or.ijul.gt.kday(12,ileap))go to 11
c
      do 10 i=1,12
      if(ijul.gt.kday(i,ileap))go to 10
      imo=i
      iday=ijul
      if(imo.ne.1)iday=ijul-kday(imo-1,ileap)
      return
10    continue
c
11    continue
      write(io,12)iyr,ijul
12    format(//2x,'ERROR in SUBR. GRDAY -- invalid Julian day '//2x,
     1 'iyr = ',i5,3x,'ijul = ',i5)
      write(*,*)
      stop 'Halted in GRDAY -- see list file.'
      end
c------------------------------------------------------------------------------
      subroutine dedat(idathr,iyr,ijul,ihr)
c------------------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 941215                  DEDAT
c ---            J. Scire, SRC
c
c --- Decode a date-time variable
c
c --- INPUTS:
c            IDATHR - integer    - Date-time variable (YYYYJJJHH)
c
c --- OUTPUT:
c               IYR - integer    - Year of precip. data (4 digits)
c              IJUL - integer    - Julian day number of precip. data
c               IHR - integer    - Ending hour (1-24) of precip. data
c
c --- DEDAT called by:  host subroutines
c --- DEDAT calls:      none
c------------------------------------------------------------------------------
c
c --- decode date and time
      iyr=idathr/100000
      ijul=idathr/100-iyr*1000
      ihr=idathr-iyr*100000-ijul*100
c
      return
      end
c------------------------------------------------------------------------------
      subroutine deltt(j1yr,j1jul,j1hr,j2yr,j2jul,j2hr,jleng)
c------------------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 941215                  DELTT
c ---            J. Scire, SRC
c
c --- Compute the difference (in hours) between two dates & times
c ---    (time #2 - time #1)
c
c --- INPUTS:
c              J1YR - integer    - Year of date/time #1
c             J1JUL - integer    - Julian day of date/time #1
c              J1HR - integer    - Hour of date/time #1
c              J2YR - integer    - Year of date/time #2
c             J2JUL - integer    - Julian day of date/time #2
c              J2HR - integer    - Hour of date/time #2
c
c --- OUTPUT:
c             JLENG - integer    - Difference (#2 - #1) in hours
c
c --- DELTT called by:  host subroutines
c --- DELTT calls:      none
c------------------------------------------------------------------------------
c
      jmin=min0(j1yr,j2yr)
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the first date/hour
      if(j1yr.eq.jmin)then
         j1=0
      else
         j1=0
         j1yrm1=j1yr-1
         do 10 i=jmin,j1yrm1
         if(mod(i,4).eq.0)then
            j1=j1+8784
         else
            j1=j1+8760
         endif
10       continue
      endif
      j1=j1+(j1jul-1)*24+j1hr
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the second date/hour
      if(j2yr.eq.jmin)then
         j2=0
      else
         j2=0
         j2yrm1=j2yr-1
         do 20 i=jmin,j2yrm1
         if(mod(i,4).eq.0)then
            j2=j2+8784
         else
            j2=j2+8760
         endif
20       continue
      endif
      j2=j2+(j2jul-1)*24+j2hr
c
c --- compute the time difference (in hours)
      jleng=j2-j1
c
      return
      end
c----------------------------------------------------------------------
      subroutine incr(io,iyr,ijul,ihr,nhrinc)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 000602                   INCR
c                J. Scire, SRC
c
c --- PURPOSE:  Increment the time and date by "NHRINC" hours
c
c --- UPDATE
c ---               000602  (DGS): add message to "stop"
c ---               980304  (DGS): Allow for a negative "increment" of
c                                  up to 24 hours
c ---               980304  (DGS): Allow for arbitrarily large nhrinc
c
c --- INPUTS:
c       IO     - integer - Unit number for list file output
c       IYR    - integer - Current year
c       IJUL   - integer - Current Julian day
c       IHR    - integer - Current hour (00-23)
c       NHRINC - integer - Time increment (hours)
c
c               NOTE: "NHRINC" must >= -24
c                      Hour is between 00-23
c
c --- OUTPUT:
c       IYR    - integer - Updated year
c       IJUL   - integer - Updated Julian day
c       IHR    - integer - Updated hour (00-23)
c
c --- INCR called by: host subroutines
c --- INCR calls:     none
c----------------------------------------------------------------------
c
c --- Check nhrinc
      if(nhrinc.lt.-24) then
         write(io,*)'ERROR IN SUBR. INCR -- Invalid value of NHRINC ',
     1   '-- NHRINC = ',nhrinc
         write(*,*)
         stop 'Halted in INCR -- see list file.'
      endif

c --- Save increment remaining (needed if nhrinc > 8760)
      nleft=nhrinc
c
c --- Process change in hour
      if(nhrinc.gt.0)then
c
10       ninc=MIN0(nleft,8760)
         nleft=nleft-ninc
c
c ---    Increment time
         ihr=ihr+ninc
         if(ihr.le.23)return
c
c ---    Increment day
         ijul=ijul+ihr/24
         ihr=mod(ihr,24)
c
c ---    ILEAP = 0 (non-leap year) or 1 (leap year)
         if(mod(iyr,4).eq.0)then
            ileap=1
         else
            ileap=0
         endif
c
         if(ijul.gt.365+ileap) then
c ---       Update year
            iyr=iyr+1
            ijul=ijul-(365+ileap)
         endif
c
c ---    Repeat if more hours need to be added
         if(nleft.GT.0) goto 10
c
      elseif(nhrinc.lt.0)then
c ---    Decrement time
         ihr=ihr+nhrinc
         if(ihr.lt.0)then
            ihr=ihr+24
            ijul=ijul-1
            if(ijul.lt.1)then
               iyr=iyr-1
               if(mod(iyr,4).eq.0)then
                  ijul=366
               else
                  ijul=365
               endif
            endif
         endif
      endif
c
      return
      end
c------------------------------------------------------------------------------
      subroutine indecr(io,iyr,ijul,ihr,idelt,ihrmin,ihrmax)
c------------------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 961014                 INDECR
c ---            J. Scire, SRC
c
c --- Increment or decrement a date/time by "IDELT" hours
c --- (-24 <= IDELT <= 24)
c --- Allows specification of 0-23 or 1-24 hour clock
c
c --- INPUTS:
c                IO - integer    - Unit number for list file output
c               IYR - integer    - Input Year
c              IJUL - integer    - Input Julian day
c               IHR - integer    - Input hour (ihrmin <= IHR <= ihrmax)
c             IDELT - integer    - Change in time (hours) -- must be
c                                  between -24 to +24, inclusive
c            IHRMIN - integer    - Minimum hour (i.e., either  0 or  1)
c            IHRMAX - integer    - Maximum hour (i.e., either 23 or 24)
c
c --- OUTPUT:
c               IYR - integer    - Year after change of "IDELT" hours
c              IJUL - integer    - Julian day after change of "IDELT" hours
c               IHR - integer    - Hour after change of "IDELT" hours
c
c --- INDECR called by:  host subroutines
c --- INDECR calls:      none
c------------------------------------------------------------------------------
c
      if(iabs(idelt).gt.24)then
         write(io,10)'IDELT',iyr,ijul,ihr,idelt,ihrmin,ihrmax
10       format(/1x,'ERROR in subr. INDECR -- invalid "',a,'" -- ',
     1   ' iyr,ijul,ihr,idelt,ihrmin,ihrmax = ',6i10)
         write(*,987)
987      format(1x,'ERROR in run - see the .LST file')
         stop
      endif
      if(ihr.lt.ihrmin.or.ihr.gt.ihrmax)then
         write(io,10)'IHR',iyr,ijul,ihr,idelt,ihrmin,ihrmax
         write(*,987)
         stop
      endif
c
      if(idelt.lt.0)then
c ---    idelt is negative
         ihr=ihr+idelt
         if(ihr.lt.ihrmin)then
            ihr=ihr+24
            ijul=ijul-1
            if(ijul.lt.1)then
               iyr=iyr-1
               if(mod(iyr,4).eq.0)then
                  ijul=366
               else
                  ijul=365
               endif
            endif
         endif
      else
c ---    idelt is positive or zero
         ihr=ihr+idelt
         if(ihr.gt.ihrmax)then
            ihr=ihr-24
            ijul=ijul+1
            if(mod(iyr,4).eq.0)then
               ndays=366
            else
               ndays=365
            endif
            if(ijul.gt.ndays)then
               ijul=1
               iyr=iyr+1
            endif
         endif
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine incrs(io,iyr,ijul,ihr,isec,nsec)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 061020                  INCRS
c                D. Strimaitis, EARTH TECH
c
c --- PURPOSE:  Increment the time and date by "NSEC" seconds
c
c --- UPDATE
c --- V2.54 (061020) from V2.4 (041029) (DGS)
c               - Allow negative increment
c
c --- INPUTS:
c       IO     - integer - Unit number for list file output
c       IYR    - integer - Current year (YYYY)
c       IJUL   - integer - Current Julian day (JJJ)
c       IHR    - integer - Current hour (00-23)
c       ISEC   - integer - Current second (0000-3599)
c       NSEC   - integer - Time increment (seconds)
c       Parameters: IO6
c
c --- OUTPUT:
c       IYR    - integer - Updated year
c       IJUL   - integer - Updated Julian day
c       IHR    - integer - Updated hour (00-23)
c       ISEC   - integer - Updated seconds (0000-3599)
c
c --- INCRS called by: host subroutines
c --- INCRS calls:     INCR
c----------------------------------------------------------------------

      if(nsec.GE.0) then
c ---    Increment seconds
         isec=isec+nsec
         if(isec.GE.3600) then
            nhrinc=isec/3600
            isec=MOD(isec,3600)
            call INCR(io,iyr,ijul,ihr,nhrinc)
         endif

      else
c ---   Decrement seconds
         isec=isec+nsec
         if(isec.LT.0) then
c ---       Earlier hour
            ksec=-isec
            if(ksec.GE.3600) then
c ---          Back up at least 1 hour
               nhrinc=ksec/3600
               ksec=MOD(ksec,3600)
               nhrinc=-nhrinc
               call INCR(io,iyr,ijul,ihr,nhrinc)
            endif
            isec=-ksec
            if(isec.LT.0) then
c ---          Back up 1 more hour
               nhrinc=-1
               isec=3600+isec
               call INCR(io,iyr,ijul,ihr,nhrinc)
            endif
         endif

      endif

      return
      end
c----------------------------------------------------------------------
      subroutine deltsec(ndhrb,nsecb,ndhre,nsece,ndelsec)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 041029                DELTSEC
c ---            D. Strimaitis, Earth Tech
c
c --- PURPOSE: Compute the difference (in seconds) between two dates &
c              times (timeE - timeB)
c
c --- INPUTS:
c             NDHRB - integer    - Beginning year & hour (YYYYJJJHH)
c             NSECB - integer    - Beginning second (SSSS)
c             NDHRE - integer    - Ending year & hour (YYYYJJJHH)
c             NSECE - integer    - Ending second (SSSS)
c
c --- OUTPUT:
c           NDELSEC - integer    - Length of interval (seconds)
c
c --- DELTSEC called by: host subroutines
c --- DELTSEC calls:     DELTT
c----------------------------------------------------------------------
c
c --- Extract year, Julian day, and hour from date-time variables
c --- Beginning
      j1yr=ndhrb/100000
      iyyjjj=ndhrb/100
      j1jul=iyyjjj-j1yr*1000
      j1hr=ndhrb-iyyjjj*100
c --- Ending
      j2yr=ndhre/100000
      iyyjjj=ndhre/100
      j2jul=iyyjjj-j2yr*1000
      j2hr=ndhre-iyyjjj*100

c --- Find difference between hours (in seconds)
      call DELTT(j1yr,j1jul,j1hr,j2yr,j2jul,j2hr,jdelhr)
      ndelsec=jdelhr*3600

c --- Add difference between seconds
      ndelsec=ndelsec+(nsece-nsecb)

      return
      end
c----------------------------------------------------------------------
      subroutine midnite(io,ctrans,iyr,imo,iday,ijul,
     &                             kyr,kmo,kday,kjul)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 041029                MIDNITE
c ---            D. Strimaitis, Earth Tech
c
c --- PURPOSE:  Converts date/time at midnight between day N, 0000
c               and day N-1, 2400.  Direction is determined by the
c               CTRANS instruction.
c
c --- INPUTS:
c            IO - integer      - Unit number for list file output
c        CTRANS - character    - Instruction 'TO 24h' or 'TO 00h'
c           IYR - integer      - Year
c           IMO - integer      - Month
c          IDAY - integer      - Day
c          IJUL - integer      - Julian day
c
c --- OUTPUT:
c           KYR - integer      - Year
c           KMO - integer      - Month
c          KDAY - integer      - Day
c          KJUL - integer      - Julian day
c
c --- MIDNITE called by:  host subroutines
c --- MIDNITE calls:      JULDAY, INCR, GRDAY
c----------------------------------------------------------------------
      character*6 ctrans

      ierr =0

c --- Get Julian day from month/day if needed
      if(ijul.LE.0) call JULDAY(io,iyr,imo,iday,ijul)

      kyr=iyr
      kmo=imo
      kday=iday
      kjul=ijul

      if(ctrans.EQ.'TO 24h') then
c ---    Convert from 0000 on ijul to 2400 on kjul
         ihr=0
         nhr=-1
         call INCR(io,kyr,kjul,ihr,nhr)
         call GRDAY(io,kyr,kjul,kmo,kday)
      elseif(ctrans.EQ.'TO 00h') then
c ---    Convert from 2400 on ijul to 0000 on kjul
         ihr=23
         nhr=1
         call INCR(io,kyr,kjul,ihr,nhr)
         call GRDAY(io,kyr,kjul,kmo,kday)
      else
         ierr=1
      endif

      if(ierr.eq.1)then
         write(io,*)
         write(io,*)'ERROR in SUBR. MIDNITE'
         write(io,*)'Invalid instruction: ',ctrans
         write(io,*)'           Expected: TO 24h'
         write(io,*)'              OR   : TO 00h'
         write(*,*)
         stop 'Halted in MIDNITE -- see list file.'
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine utcbasr(axtz,xbtz)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.55     Level: 070327                UTCBASR
c ---            F.Robe, Earth Tech
c
c --- PURPOSE:  Converts character string UTC time zone 
c               to real base time zone
c
c --- V2.55 (070327) from V2.5 (041123) (DGS)
c               - Add RETURN statement
c
c --- INPUT:
c          AXTZ - char*8    - time zone (international convention: 
c                             relative to UTC/GMT)UTC-HHMM
c --- OUTPUT:
c          XBTZ - real      - base time zone (old convention: positive
c                             in North America i.e. opposite to UTC)
c
c --- UTCBASR called by:  host subroutines
c --- UTCBASR calls:      none
c----------------------------------------------------------------------
      character*8 axtz

      read(axtz(4:6),'(i3)')ihr
      read(axtz(7:8),'(i2)')imin
      if(ihr.lt.0)imin=-imin

      xbtz=ihr+imin/60.

c --- Flip sign as base time convention is opposite UTC/GMT
      xbtz=-xbtz

      return
      end
c----------------------------------------------------------------------
      subroutine basrutc(xbtz,axtz)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.55     Level: 070327                BASRUTC
c ---            F.Robe, Earth Tech
c
c --- PURPOSE:  Converts real base time zone  to character string
c               UTC time zone
c
c --- UPDATE
c --- V2.55 (070327) from V2.5 (041123) (DGS)
c               - Fix output format of time zone string for zone=0
c               - Add RETURN statement
c
c --- INPUT:
c          XBTZ - real      - base time zone (old convention: positive
c                             in North America i.e. opposite to UTC)

c --- OUTPUT:
c          AXTZ - real      - time zone (international convention: 
c                             relative to UTC/GMT)UTC-HHMM
c
c --- BASRUTC called by:  host subroutines
c --- BASRUTC calls:      none
c----------------------------------------------------------------------
      character*8 axtz

      ixbtz=int(xbtz) 
c     convert fractional real to minutes
      imin=(xbtz-ixbtz)*60
      ixbtz=ixbtz*100+imin

c --- Define time as "UTC-HHMM" (hours/minutes)
      axtz(1:3)="UTC"

c --- Flip sign as base time zone is minus UTC zone
      if (xbtz.gt.0.) then
         axtz(4:4)="-"
      else
         axtz(4:4)="+"
      endif
c --- Make sure time zone is written as 4 digits
      write(axtz(5:8),'(i4.4)')abs(ixbtz)

      return
      end
c----------------------------------------------------------------------
      subroutine filcase(lcfiles,cfile)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 040330                FILCASE
c ---            J. Scire, SRC
c
c --- PURPOSE:  Convert all characters within a file name to lower
c               case (if LCFILES=T) or UPPER CASE (if LCFILES=F).
c
c --- UPDATE
c --- V2.2 (950610) to V2.3 (040330)  DGS
c               - Replace filename strings c*70 with c*132
c
c --- INPUTS:
c
c         LCFILES - logical - Switch indicating if all characters in the
c                             filenames are to be converted to lower case
c                             letters (LCFILES=T) or converted to UPPER
c                             CASE letters (LCFILES=F).
c           CFILE - char*132- Input character string
c
c --- OUTPUT:
c
c           CFILE - char*132- Output character string with
c                             letters converted
c
c --- FILCASE called by:  READFN
c --- FILCASE calls:      none
c----------------------------------------------------------------------
c
      character*132 cfile
      character*1 cchar,clc(29),cuc(29)
      logical lcfiles
c
      data clc/'i','n','x','a','e','o','u','b','c','d','f','g','h',
     1 'j','k','l','m','p','q','r','s','t','v','w','y','z','-','.',
     2 '*'/
      data cuc/'I','N','X','A','E','O','U','B','C','D','F','G','H',
     1 'J','K','L','M','P','Q','R','S','T','V','W','Y','Z','-','.',
     2 '*'/
c
      if(lcfiles)then
c
c ---    Convert file name to lower case letters
         do i=1,132
            cchar=cfile(i:i)
c
            do j=1,29
               if(cchar.eq.cuc(j))then
                  cfile(i:i)=clc(j)
                  go to 52
               endif
            enddo
52          continue
         enddo
      else
c
c ---    Convert file name to UPPER CASE letters
         do i=1,132
            cchar=cfile(i:i)
c
            do j=1,29
               if(cchar.eq.clc(j))then
                  cfile(i:i)=cuc(j)
                  go to 62
               endif
            enddo
62          continue
         enddo
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine readin(cvdic,ivleng,ivtype,ioin,ioout,lecho,
     1 i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,
     2 i19,i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,i30,i31,i32,i33,i34,
     3 i35,i36,i37,i38,i39,i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,i50,
     4 i51,i52,i53,i54,i55,i56,i57,i58,i59,i60)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 060519                 READIN
c                J. Scire, Earth Tech, Inc.
c
c --- PURPOSE:  Read one input group of the free formatted control
c               file -- allows comments within the input file --
c               ignores all text except that within delimiters
c
c ---           NOTE:  All variables (real, integer, logical,
c                      or character) must be 4 bytes
c ---           NOTE:  Character*4 array uses only one character
c                      per word -- it must be dimensioned large
c                      enough to accommodate the number of characters
c                      in the variable field
c
c --- UPDATE
c --- V2.52 (060519) from V2.3 (040330) (DGS)
c               - Search for '=' beyond position 14 because blanks are
c                 not automatically removed within string
c --- V2.3 (040330) from V2.1 (030402) (DGS)
c               - Preserve spaces within character variables
c --- V2.1 (030402) from V2.0 (000602) (DGS)
c               - Split DEBLNK action (removes ' ', '+') into 
c                 DEBLNK and DEPLUS(new)
c
c
c --- INPUTS:
c
c     CVDIC(mxvar) - character*12 array - Variable dictionary
c                                         containing up to "MXVAR"
c                                         variable names
c    IVLENG(mxvar) - integer array      - Dimension of each variable
c                                         (dim. of scalars = 1)
c    IVTYPE(mxvar) - integer array      - Type of each variable
c                                           1 = real,
c                                           2 = integer,
c                                           3 = logical,
c                                           4 = character*4
c             IOIN - integer            - Fortran unit of control file
c                                         input
c            IOOUT - integer            - Fortran unit of list file
c                                         output
c            LECHO - logical            - Control variable determining
c                                         if input data are echoed to
c                                         list file (IOOUT)
c        Parameters: MXVAR, MXCOL
c
c --- OUTPUT:
c
c      I1, I2, ... - integer arrays     - Variables being read
c                    (integer array locally, but can be a real,
c                     integer, logical, or character*4 array in
c                     the calling routine)
c
c --- READIN called by:  host subroutines
c --- READIN calls:      DEBLNK, ALTONU, SETVAR, ALLCAP, DEPLUS,
c                        TRIGHT, TLEFT
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cal'
c
      integer*4 i1(*),i2(*),i3(*),i4(*),i5(*),i6(*),i7(*),i8(*),i9(*),
     1 i10(*),i11(*),i12(*),i13(*),i14(*),i15(*),i16(*),i17(*),i18(*),
     2 i19(*),i20(*),i21(*),i22(*),i23(*),i24(*),i25(*),i26(*),i27(*),
     3 i28(*),i29(*),i30(*),i31(*),i32(*),i33(*),i34(*),i35(*),i36(*),
     4 i37(*),i38(*),i39(*),i40(*),i41(*),i42(*),i43(*),i44(*),i45(*),
     5 i46(*),i47(*),i48(*),i49(*),i50(*),i51(*),i52(*),i53(*),i54(*),
     6 i55(*),i56(*),i57(*),i58(*),i59(*),i60(*)
      integer*4 ivleng(mxvar),jdex(mxvar),ivtype(mxvar)
c
      logical*4 lv
      logical lecho
c
      character*12 cvdic(mxvar),cvar,cblank
      character*4 cv(mxcol)
      character*1 cstor1(mxcol),cstor2(mxcol)
c --- Intermediate scratch arrays
      character*1 cstor3(mxcol),cstor4(mxcol)
      character*1 cdelim,ceqls,ce,cn,cd,comma,cblnk
c
      data cblank/'            '/
      data cdelim/'!'/,ceqls/'='/,ce/'E'/,cn/'N'/,cd/'D'/,comma/','/
      data cblnk/' '/
c
      ilim2=99
      do 2 i=1,mxvar
      jdex(i)=1
2     continue
c
c --- begin loop over lines
c
c --- read a line of input 
5     continue 
      read(ioin,10)cstor1 
10    format(200a1) 
      if(lecho)write(ioout,7)cstor1 
7     format(1x,200a1) 
c
c --- check if this is a continuation line
      if(ilim2.gt.0)go to 16
c
c --- continuation line -- find the second delimiter
      do 12 i=1,mxcol
      if(cstor1(i).eq.cdelim)then
         ilim2=i
         go to 14
      endif
12    continue
14    continue
      il2=ilim2
      if(il2.eq.0)il2=mxcol
c
c --- Trim blanks from left and right sides of string within delimiters
c -----------------------
cc --- remove blank characters from string within delimiters
c      call deblnk(cstor1,1,il2,cstor2,nlim)
cc --- Remove '+' characters as well (is this needed?)
c      if(nlim.gt.0) then
c         do k=1,mxcol
c            cstor3(k)=cstor2(k)
c         enddo
c         il3=nlim
c         call deplus(cstor3,1,il3,cstor2,nlim)
c      endif
c -----------------------
c --- Remove blank characters on right side
      call TRIGHT(cstor1,1,il2,cstor2,nlim)
c --- Remove blank characters on left side
      if(nlim.gt.0) then
         do k=1,mxcol
            cstor3(k)=cstor2(k)
         enddo
         il3=nlim
         call TLEFT(cstor3,1,il3,cstor2,nlim)
      endif
c -----------------------
      icom=0
c
c --- convert lower case letters to upper case
      call allcap(cstor2,nlim)
      go to 55
c
16    continue
      ibs=1
c
c --- begin loop over delimiter pairs
17    continue
      if(ibs.ge.mxcol)go to 5
c
c --- find location of delimiters
      do 20 i=ibs,mxcol
      if(cstor1(i).eq.cdelim)then
         ilim1=i
         if(ilim1.eq.mxcol)go to 22
         ip1=ilim1+1
         do 18 j=ip1,mxcol
         if(cstor1(j).eq.cdelim)then
            ilim2=j
            go to 22
         endif
18       continue
c
c ---    second delimiter not on this line
         ilim2=0
         go to 22
      endif
20    continue
c
c --- no delimiters found -- skip line and read next line of text
      go to 5
22    continue
      ibs=ilim2+1
      if(ilim2.eq.0)ibs=mxcol+1
c
c --- Trim blanks from left and right sides of string within delimiters
c -----------------------
cc --- remove blanks from string within delimiters
c      il2=ilim2
c      if(il2.eq.0)il2=mxcol
c      call deblnk(cstor1,ilim1,il2,cstor2,nlim)
cc --- Remove '+' characters as well (is this needed?)
c      if(nlim.gt.0) then
c         do k=1,mxcol
c            cstor3(k)=cstor2(k)
c         enddo
c         il3=nlim
c         call deplus(cstor3,1,il3,cstor2,nlim)
c      endif
c -----------------------
      il2=ilim2
      if(il2.eq.0)il2=mxcol
c --- Remove blank characters on right side
      call TRIGHT(cstor1,ilim1,il2,cstor2,nlim)
c --- Remove blank characters on left side
      if(nlim.gt.0) then
         do k=1,mxcol
            cstor3(k)=cstor2(k)
         enddo
         il3=nlim
         call TLEFT(cstor3,1,il3,cstor2,nlim)
      endif
c -----------------------
c
c --- convert lower case letters to upper case
      call allcap(cstor2,nlim)
c
c --- search for equals sign (cstor2(1) is delimiter; cstor2(2) is
c --- first letter of variable; cstor2(3) is earliest '=' can occur)
c --- (060519)  Search entire string as now there may be blanks before '=' 
c      do 30 i=3,14
      do 30 i=3,nlim
      if(cstor2(i).eq.ceqls)then
         ieq=i
         go to 32
      endif
30    continue
c
c --- "END" within delimiters signifies the end of the read for
c --- this input group
      if(cstor2(2).eq.ce.and.cstor2(3).eq.cn.and.cstor2(4).eq.cd)return
      write(ioout,31)(cstor2(n),n=1,nlim)
31    format(/1x,'ERROR IN SUBR. READIN -- Error in input data -- '/
     1 1x,'Variable too long (Equals sign not found in string) -- ',
     2 'CSTOR2 = ',200a1)
      write(*,*)
      stop 'Halted in READIN -- see list file.'
c
c --- CVAR is character*12 variable name
32    continue
      cvar=cblank
      ieqm1=ieq-1
c --- Grab string to left of '=', and remove blanks
      call deblnk(cstor2,1,ieqm1,cstor3,keqm1)
c --- Pass string to variable name
      do 40 i=2,keqm1
      il=i-1
      cvar(il:il)=cstor3(i)
40    continue
c
c --- find the variable name in the variable dictionary
      do 50 i=1,mxvar
      if(cvar.eq.cvdic(i))then
         nvar=i
         go to 52
      endif
50    continue
      write(ioout,51)cvar,(cvdic(n),n=1,mxvar)
51    format(/1x,'ERROR IN SUBR. READIN -- Error in input data -- '/
     1 1x,'Variable not found in variable dictionary'/
     2 1x,'Variable: ',a12/
     3 1x,'Variable Dictionary: ',9(a12,1x)/
     4 10(22x,9(a12,1x)/))
      write(*,*)
      stop 'Halted in READIN -- see list file.'
c
c --- search for comma
52    continue
      icom=ieq
c
c --- beginning of loop over values within delimiters
55    continue
      ivb=icom+1
c
c --- if reaches end of line, read next line
      if(ivb.gt.nlim)go to 5
      do 60 i=ivb,nlim
      if(cstor2(i).eq.comma)then
         icom=i
         go to 64
      endif
60    continue
c
c --- no comma found
      icom=0
      ive=nlim-1
c
c --- comma between last value and delimiter is allowed
      if(cstor2(ivb).eq.cdelim.and.cstor2(ive).eq.comma)go to 17
c
c --- if no comma & last non-blank character is not a delimiter,
c --- then the input is in error
      if(cstor2(nlim).eq.cdelim)go to 66
      write(ioout,63)cstor1
63    format(/1x,'ERROR IN SUBR. READIN -- Error in input data -- '/
     1 1x,'If a string within delimiters covers more than one line, ',
     2 'the last character in the line must be a comma'/
     3 1x,'Input line: ',200a1)
      write(*,*)
      stop 'Halted in READIN -- see list file.'
64    continue
c
c --- value of variable is contained in elements IVB to IVE of
c --- CSTOR2 array
      ive=icom-1
66    continue
c      ncar=ive-ivb+1
      index=jdex(nvar)
      itype=ivtype(nvar)
c
c --- Check for invalid value of variable type
      if(itype.le.0.or.itype.ge.5)then
         write(ioout,68)itype,nvar,ivtype(nvar),cvdic(nvar)
68       format(/1x,'ERROR IN SUBR. READIN -- Error in input data -- '/
     1   1x,'Invalid value of variable type -- ITYPE must be 1, 2, 3, ',
     2   'or 4'/1x,'ITYPE = ',i10/1x,'NVAR = ',i10/1x,'IVTYPE(nvar) = ',
     3   i10/1x,'CVDIC(nvar) = ',a12)
      write(*,*)
      stop 'Halted in READIN -- see list file.'
      endif
c
c --- Convert character string to numeric or logical value
c     (if ITYPE = 1,2, or 3 -- If 4, transfer characters to the
c     work array CV)

c --- Remove all blanks from variable string if type is numeric or
c --- logical;  otherwise, trim left and right side of string
      if(itype.NE.4) then
         call deblnk(cstor2,ivb,ive,cstor4,nv)
c ---    Remove '+' characters as well (is this needed?)
         if(nv.gt.0) then
            do k=1,mxcol
               cstor3(k)=cstor4(k)
            enddo
            il3=nv
            call deplus(cstor3,1,il3,cstor4,nv)
         endif
         call altonu(ioout,cstor4(1),nv,itype,irep,rlno,ino,lv,cv)
      else
c ---    Pass variable string into cstor4
         nv=ive-ivb+1
         do k=1,nv
            cstor4(k)=cstor2(ivb+k-1)
         enddo
         do k=nv+1,mxcol
            cstor4(k)=cblnk
         enddo
c ---    Remove blank characters on right side of character variable
         if(cstor4(nv).EQ.cblnk) call TRIGHT(cstor2,ivb,ive,cstor4,nv)
c ---    Remove blank characters on left side of character variable
         if(nv.GT.0 .AND. cstor4(1).EQ.cblnk) then
            do k=1,mxcol
               cstor3(k)=cstor4(k)
            enddo
            il3=nv
            call TLEFT(cstor3,1,il3,cstor4,nv)
         endif
         call altonu(ioout,cstor4(1),nv,itype,irep,rlno,ino,lv,cv)
      endif

c
c --- check that array bounds are not exceeded
      if(index+irep-1.gt.ivleng(nvar))go to 201
c
      go to (101,102,103,104,105,106,107,108,109,110,111,112,113,114,
     1 115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,
     2 131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,
     3 147,148,149,150,151,152,153,154,155,156,157,158,159,160),nvar
c
c --- code currently set up to handle up to 60 variables/source group
      write(ioout,71)nvar,(cstor2(n),n=1,nlim)
71    format(/1x,'ERROR IN SUBR. READIN -- Current code ',
     1 'configuration allows up to 60 variables per source group'/
     2 1x,'No. variables (NVAR) = ',i10/
     3 1x,'Input data (CSTOR2)  = ',200a1)
      write(*,*)
      stop 'Halted in READIN -- see list file.'
c
c --- transfer value into output variable
101   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i1(index),i1(index),
     1 i1(index),i1(index))
      go to 161
102   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i2(index),i2(index),
     1 i2(index),i2(index))
      go to 161
103   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i3(index),i3(index),
     1 i3(index),i3(index))
      go to 161
104   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i4(index),i4(index),
     1 i4(index),i4(index))
      go to 161
105   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i5(index),i5(index),
     1 i5(index),i5(index))
      go to 161
106   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i6(index),i6(index),
     1 i6(index),i6(index))
      go to 161
107   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i7(index),i7(index),
     1 i7(index),i7(index))
      go to 161
108   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i8(index),i8(index),
     1 i8(index),i8(index))
      go to 161
109   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i9(index),i9(index),
     1 i9(index),i9(index))
      go to 161
110   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i10(index),i10(index),
     1 i10(index),i10(index))
      go to 161
111   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i11(index),i11(index),
     1 i11(index),i11(index))
      go to 161
112   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i12(index),i12(index),
     1 i12(index),i12(index))
      go to 161
113   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i13(index),i13(index),
     1 i13(index),i13(index))
      go to 161
114   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i14(index),i14(index),
     1 i14(index),i14(index))
      go to 161
115   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i15(index),i15(index),
     1 i15(index),i15(index))
      go to 161
116   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i16(index),i16(index),
     1 i16(index),i16(index))
      go to 161
117   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i17(index),i17(index),
     1 i17(index),i17(index))
      go to 161
118   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i18(index),i18(index),
     1 i18(index),i18(index))
      go to 161
119   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i19(index),i19(index),
     1 i19(index),i19(index))
      go to 161
120   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i20(index),i20(index),
     1 i20(index),i20(index))
      go to 161
121   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i21(index),i21(index),
     1 i21(index),i21(index))
      go to 161
122   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i22(index),i22(index),
     1 i22(index),i22(index))
      go to 161
123   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i23(index),i23(index),
     1 i23(index),i23(index))
      go to 161
124   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i24(index),i24(index),
     1 i24(index),i24(index))
      go to 161
125   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i25(index),i25(index),
     1 i25(index),i25(index))
      go to 161
126   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i26(index),i26(index),
     1 i26(index),i26(index))
      go to 161
127   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i27(index),i27(index),
     1 i27(index),i27(index))
      go to 161
128   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i28(index),i28(index),
     1 i28(index),i28(index))
      go to 161
129   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i29(index),i29(index),
     1 i29(index),i29(index))
      go to 161
130   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i30(index),i30(index),
     1 i30(index),i30(index))
      go to 161
131   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i31(index),i31(index),
     1 i31(index),i31(index))
      go to 161
132   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i32(index),i32(index),
     1 i32(index),i32(index))
      go to 161
133   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i33(index),i33(index),
     1 i33(index),i33(index))
      go to 161
134   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i34(index),i34(index),
     1 i34(index),i34(index))
      go to 161
135   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i35(index),i35(index),
     1 i35(index),i35(index))
      go to 161
136   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i36(index),i36(index),
     1 i36(index),i36(index))
      go to 161
137   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i37(index),i37(index),
     1 i37(index),i37(index))
      go to 161
138   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i38(index),i38(index),
     1 i38(index),i38(index))
      go to 161
139   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i39(index),i39(index),
     1 i39(index),i39(index))
      go to 161
140   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i40(index),i40(index),
     1 i40(index),i40(index))
      go to 161
141   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i41(index),i41(index),
     1 i41(index),i41(index))
      go to 161
142   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i42(index),i42(index),
     1 i42(index),i42(index))
      go to 161
143   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i43(index),i43(index),
     1 i43(index),i43(index))
      go to 161
144   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i44(index),i44(index),
     1 i44(index),i44(index))
      go to 161
145   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i45(index),i45(index),
     1 i45(index),i45(index))
      go to 161
146   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i46(index),i46(index),
     1 i46(index),i46(index))
      go to 161
147   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i47(index),i47(index),
     1 i47(index),i47(index))
      go to 161
148   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i48(index),i48(index),
     1 i48(index),i48(index))
      go to 161
149   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i49(index),i49(index),
     1 i49(index),i49(index))
      go to 161
150   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i50(index),i50(index),
     1 i50(index),i50(index))
      go to 161
151   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i51(index),i51(index),
     1 i51(index),i51(index))
      go to 161
152   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i52(index),i52(index),
     1 i52(index),i52(index))
      go to 161
153   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i53(index),i53(index),
     1 i53(index),i53(index))
      go to 161
154   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i54(index),i54(index),
     1 i54(index),i54(index))
      go to 161
155   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i55(index),i55(index),
     1 i55(index),i55(index))
      go to 161
156   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i56(index),i56(index),
     1 i56(index),i56(index))
      go to 161
157   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i57(index),i57(index),
     1 i57(index),i57(index))
      go to 161
158   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i58(index),i58(index),
     1 i58(index),i58(index))
      go to 161
159   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i59(index),i59(index),
     1 i59(index),i59(index))
      go to 161
160   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i60(index),i60(index),
     1 i60(index),i60(index))
c
161   continue
      jdex(nvar)=jdex(nvar)+irep
c
c --- continue reading values for this array until array is filled
c --- or delimiter is reached
      if(icom.ne.0.and.jdex(nvar).le.ivleng(nvar))go to 55
      go to 17
201   continue
      iatt=index+irep-1
      write(ioout,202)cvdic(nvar),ivleng(nvar),iatt,cstor1
202   format(/1x,'ERROR IN SUBR. READIN -- Error in input data',
     1 1x,'Array bounds exceeded -- Variable: ',a12,3x,' Declared ',
     2 'dimension = ',i8/1x,'Input attempted to element ',i8/1x,
     3 'Input line: ',200a1)
      write(*,*)
      stop 'Halted in READIN -- see list file.'
      end
c----------------------------------------------------------------------
      subroutine altonu(ioout,alp,ncar,itype,irep,rlno,ino,lv,cv)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 000602                 ALTONU
c ---            J. Scire, SRC
c
c --- PURPOSE:  Convert a character string into a real, integer or
c               logical variable -- also compute the repetition factor
c               for the variable
c
c --- UPDATES
c ---               000602  (DGS): add message to "stop"
c
c --- INPUTS:
c            IOOUT - integer           - Fortran unit of list file
c                                        output
c        ALP(ncar) - character*1 array - Characters to be converted
c             NCAR - integer           - Number of characters
c            ITYPE - integer           - Type of each variable
c                                           1 = real,
c                                           2 = integer,
c                                           3 = logical,
c                                           4 = character*4
c
c       Parameter:   MXCOL
c
c --- OUTPUT:
c             IREP - integer           - Repetition factor for value
c             RLNO - real              - Real variable produced from
c                                        character string
c              INO - integer           - Integer variable produced from
c                                        character string
c               LV - logical*4         - Logical variable produced from
c                                        character string
c        CV(mxcol) - character*4       - Character*4 variable produced
c                                        from character string
c                                        (NOTE: Only 1 (NOT 4)
c                                        character(s) per word)
c
c --- ALTONU called by:  READIN
c --- ALTONU calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cal'
c
      real*8 rno,xmult,ten
      integer num2(mxcol)
      logical*4 lv
      character*4 cv(mxcol)
      character*1 alp(ncar),alpsv,ad(17),astar,adec
c
      data ad/'0','1','2','3','4','5','6','7','8','9','-',
c ---   num2 = 0   1   2   3   4   5   6   7   8   9  11
     1        '*','.','E','D','T','F'/
c ---   num2 = 12  13  14  15  16  17
      data astar/'*'/,adec/'.'/,ten/10.0d0/
c
c --- If dealing with a character*4 variable, transfer characters
c     into the work array CV (ONE character per 4-byte word)
      if(itype.eq.4)then
         do 5 i=1,ncar
         cv(i)(1:1)=alp(i)
5        continue
c
c ---    NOTE: Repetition factor refers to the number of
c              characters in the field, if ITYPE = 4
         irep=ncar
         return
      endif
c
c --- Convert character array elements into numeric codes
      do 30 i=1,ncar
      alpsv=alp(i)
      do 20 j=1,17
      if(alpsv.eq.ad(j))then
         num2(i)=j
         if(j.lt.11)num2(i)=j-1
         go to 30
      endif
20    continue
      write(ioout,21)(alp(n),n=1,ncar)
21    format(/1x,'ERROR IN SUBR. ALTONU -- Unrecognizable character ',
     1 'in input -- Character string (ALP) = ',15a1)
      write(*,*)
      stop 'Halted in ALTONU -- see list file.'
30    continue
c
c --- Locally classify variable type (1=real, 2=integer, 3=logical)
      do 40 i=1,ncar
      if(num2(i).le.12)go to 40
      if(num2(i).ge.16)then
c
c ---    logical variable ("T", "F")
         jtype=3
         go to 41
      else
c
c ---    real variable (".", "E", "D")
         jtype=1
         go to 41
      endif
40    continue
c
c --- integer variable
      jtype=2
41    continue
c
c --- determine if repetition factor "*" is used
      do 50 i=1,ncar
      if(alp(i).eq.astar)then
         istar=i
         go to 51
      endif
50    continue
      istar=0
51    continue
      if(istar.ne.0)go to 400
      irep=1
      go to (101,201,301),jtype
      write(ioout,55)jtype,(alp(n),n=1,ncar)
55    format(/1x,'ERROR IN SUBR. ALTONU -- JTYPE must be 1, 2, or 3 ',
     1 '-- JTYPE = ',i3/3x,'Text string (ALP) = ',15a1)
      write(*,*)
      stop 'Halted in ALTONU -- see list file.'
c
c --------------------------------------------------------------------
c --- REAL number w/o "*"
c --------------------------------------------------------------------
c --- Determine sign -- ISTAR is position of array containing "*"
c                       (ISTAR = 0 if no repetition factor)
101   continue
      if(num2(1+istar).eq.11)then
         isgn=-1
         istart=istar+2
      else
         isgn=1
         istart=istar+1
      endif
c
c --- Locate decimal point
      idec=0
      do 109 i=istart,ncar
      if(alp(i).eq.adec)then
         if(idec.eq.0)then
            idec=i
            go to 109
         endif
c
c ---    More than one decimal point found
         write(ioout,120)(alp(n),n=1,ncar)
120      format(/1x,'ERROR IN SUBR. ALTONU -- Invalid real variable ',
     1   'entry'/5x,'Input text (ALP) = ',15a1)
         write(*,*)
         stop 'Halted in ALTONU -- see list file.'
      endif
109   continue
c
c --- Search for E or D
      do 110 i=istart,ncar
      if(num2(i).eq.14.or.num2(i).eq.15)then
         istop=i-1
         go to 111
      endif
110   continue
      istop=ncar
111   continue

c --- 080407 Update:
c --- Correct for missing decimal point before decoding
      if(idec.EQ.0) idec=istop+1
c --- Trap missing number in front of E,D
      if(istop.LT.1 .OR. istart.GT.istop) then
         write(ioout,120)(alp(n),n=1,ncar)
         write(*,*)
         write(*,*)'Missing number!'
         stop 'Halted in ALTONU -- see list file.'
      endif
c
c --- Convert integer numerics to real number
      rno=0.0
      do 130 i=istart,istop
      if(i.eq.idec)go to 130
      if(num2(i).ge.10)then
         write(ioout,120)(alp(n),n=1,ncar)
         write(*,*)
         stop 'Halted in ALTONU -- see list file.'
      endif
      iexp=idec-i
      if(iexp.gt.0)iexp=iexp-1
      xmult=1.0
      if(iexp.ne.0)xmult=ten**iexp
      rno=rno+xmult*num2(i)
130   continue
c
c --- Account for minus sign (if present)
      rno=isgn*rno
      rlno=rno
c --- Also set integer variable in case of improper input
      if(rlno.lt.0.0)then
         ino=rlno-0.0001
      else
         ino=rlno+0.0001
      endif
      if(istop.eq.ncar)return
c
c --- Find exponent (istop+1 is position in array containing E or D)
      isgn=1
      istart=istop+2
      if(num2(istart).ne.11)go to 135
      isgn=-1
      istart=istart+1
135   continue
      if(istart.gt.ncar)then
         write(ioout,120)(alp(n),n=1,ncar)
         write(*,*)
         stop 'Halted in ALTONU -- see list file.'
      endif
      rexp=0.0
      do 140 i=istart,ncar
      if(num2(i).ge.10)then
         write(ioout,120)(alp(n),n=1,ncar)
         write(*,*)
         stop 'Halted in ALTONU -- see list file.'
      endif
      iexp=ncar-i
      xmult=1.0
      if(iexp.ne.0)xmult=ten**iexp
      rexp=rexp+xmult*num2(i)
140   continue
      xmult=1.0
      if(rexp.ne.0.0)xmult=ten**(isgn*rexp)
      rno=rno*xmult
      rlno=rno
c
c --- Also set integer variable in case of improper input
      if(rlno.lt.0.0)then
         ino=rlno-0.0001
      else
         ino=rlno+0.0001
      endif
      return
c
c --------------------------------------------------------------------
c --- INTEGER variables
c --------------------------------------------------------------------
201   continue
      if(num2(1+istar).ne.11)go to 228
      isgn=-1
      istart=istar+2
      go to 229
228   continue
      isgn=1
      istart=istar+1
229   continue
      ino=0
      do 230 i=istart,ncar
      if(num2(i).ge.10)go to 208
      iexp=ncar-i
      xmult=1.0
      if(iexp.ne.10)xmult=ten**iexp
      ino=ino+xmult*num2(i)+0.5
230   continue
      ino=isgn*ino
c
c --- Also set real variable in case of improper input
      rlno=ino
      return
208   continue
      write(ioout,220)(alp(n),n=1,ncar)
220   format(/1x,'ERROR IN SUBR. ALTONU -- Invalid integer variable ',
     1 'entry'/5x,'Input text (ALP) = ',15a1)
      write(*,*)
      stop 'Halted in ALTONU -- see list file.'
c
c --------------------------------------------------------------------
c --- LOGICAL variables
c --------------------------------------------------------------------
301   continue
      if(ncar-istar.ne.1)go to 308
      if(num2(istar+1).eq.16)then
c
c ---    Variable = T
         lv=.true.
         return
      else if(num2(istar+1).eq.17)then
c
c ---    Variable = F
         lv=.false.
         return
      endif
308   continue
      write(ioout,320)(alp(n),n=1,ncar)
320   format(/1x,'ERROR IN SUBR. ALTONU -- Invalid logical variable ',
     1 'entry'/5x,'Input text (ALP) = ',15a1)
      write(*,*)
      stop 'Halted in ALTONU -- see list file.'
c
c --- Determine repetition factor
400   continue
      irep=0
c
c --- ISTAR is the position of array containing "*"
      istrm1=istar-1
      do 430 i=1,istrm1
      if(num2(i).ge.10)go to 408
      iexp=istrm1-i
      xmult=1.0
      if(iexp.ne.0)xmult=ten**iexp
      irep=irep+xmult*num2(i)+0.5
430   continue
      go to(101,201,301),jtype
      write(ioout,55)jtype,(alp(n),n=1,ncar)
      write(*,*)
      stop 'Halted in ALTONU -- see list file.'
408   continue
      write(ioout,420)(alp(n),n=1,ncar)
420   format(/1x,'ERROR IN SUBR. ALTONU -- Invalid repetition factor ',
     1 'entry'/5x,'Input text (ALP) = ',15a1)
      write(*,*)
      stop 'Halted in ALTONU -- see list file.'
      end
c----------------------------------------------------------------------
      subroutine deblnk(cstor1,ilim1,il2,cstor2,nlim)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 030402                 DEBLNK
c ---            J. Scire, Earth Tech, Inc.
c
c --- PURPOSE:  Remove all blank or "+" characters from the character 
c               string within delimiters
c               Only characters in the range ilim1 to il2 may be
c               written to output array
c
c --- UPDATE
c --- V2.1 (030402) from V2.0 (980918) (DGS)
c               - Split DEBLNK action (removes ' ', '+') into 
c                 DEBLNK and DEPLUS(new)
c
c --- INPUTS:
c
c    CSTOR1(mxcol) - character*1 array - Input character string
c            ILIM1 - integer           - Array element at which search
c                                        for blanks begins
c              IL2 - integer           - Array element at which search
c                                        for blanks ends
c        Parameters: MXCOL
c
c --- OUTPUT:
c
c    CSTOR2(mxcol) - character*1 array - Output character string
c                                        (without blanks within text)
c             NLIM - integer           - Length of output string
c                                        (characters)
c
c --- DEBLNK called by:  (utility)
c --- DEBLNK calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cal'
c
      character*1 cstor1(mxcol),cstor2(mxcol),cblnk
      data cblnk/' '/
c
      ind=0
      do 10 i=ilim1,il2
      if(cstor1(i).eq.cblnk)go to 10
c
c --- transfer non-blank character into output array
      ind=ind+1
      cstor2(ind)=cstor1(i)
10    continue
      nlim=ind
      if(ind.eq.mxcol)return
c
c --- pad rest of output array
      indp1=ind+1
      do 20 i=indp1,mxcol
      cstor2(i)=cblnk
20    continue
      return
      end
c----------------------------------------------------------------------
      subroutine deplus(cstor1,ilim1,il2,cstor2,nlim)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 030402                 DEPLUS
c ---            J. Scire, Earth Tech, Inc.
c
c --- PURPOSE:  Remove all "+" characters from the character 
c               string within delimiters
c               Only characters in the range ilim1 to il2 may be
c               written to output array
c
c --- INPUTS:
c
c    CSTOR1(mxcol) - character*1 array - Input character string
c            ILIM1 - integer           - Array element at which search
c                                        for plus begins
c              IL2 - integer           - Array element at which search
c                                        for plus ends
c        Parameters: MXCOL
c
c --- OUTPUT:
c
c    CSTOR2(mxcol) - character*1 array - Output character string
c                                        (without plus within text)
c             NLIM - integer           - Length of output string
c                                        (characters)
c
c --- DEPLUS called by:  (utility)
c --- DEPLUS calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cal'
c
      character*1 cstor1(mxcol),cstor2(mxcol),cblnk,cplus
      data cblnk/' '/,cplus/'+'/
c
      ind=0
      do 10 i=ilim1,il2
      if(cstor1(i).eq.cplus)go to 10
c
c --- transfer non-plus character into output array
      ind=ind+1
      cstor2(ind)=cstor1(i)
10    continue
      nlim=ind
      if(ind.eq.mxcol)return
c
c --- pad rest of output array
      indp1=ind+1
      do 20 i=indp1,mxcol
      cstor2(i)=cblnk
20    continue
      return
      end
c----------------------------------------------------------------------
      subroutine tright(cstor1,ilim1,il2,cstor2,nlim)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 040330                 TRIGHT
c ---            D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  Remove all blank characters in the range ilim1 to il2
c               that lie to the RIGHT of the last non-blank character
c               in the string before il2. Also remove the character
c               at il2 if it is blank.
c               Only characters in the range ilim1 to il2 may be
c               written to the output array.
c
c               Example --
c               Range    : ilim1=3, il2=21
c               CSTOR1   :  2   for this run   !
c               Position : 000000000111111111122
c                          123456789012345678901
c               CSTOR2   :    for this run!
c
c --- INPUTS:
c
c    CSTOR1(mxcol) - character*1 array - Input character string
c            ILIM1 - integer           - Array element at which search
c                                        for blanks begins
c              IL2 - integer           - Array element at which search
c                                        for blanks ends
c        Parameters: MXCOL
c
c --- OUTPUT:
c
c    CSTOR2(mxcol) - character*1 array - Output character string
c                                        (with right-blanks removed)
c             NLIM - integer           - Length of output string
c                                        (characters)
c
c --- TRIGHT called by:  (utility)
c --- TRIGHT calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cal'
c
      character*1 cstor1(mxcol),cstor2(mxcol),cblnk
      data cblnk/' '/

c --- Position of last non-blank character
      klast=0
      il2m1=il2-1
      do k=ilim1,il2m1
         if(cstor1(k).NE.cblnk) klast=k
      enddo

c --- Transfer all characters in range up to klast
      ind=0
      if(klast.GT.0) then
         do k=ilim1,klast
            ind=ind+1
            cstor2(ind)=cstor1(k)
         enddo
      endif
c --- Add last character in range if non-blank
      if(cstor1(il2).NE.cblnk) then
         ind=ind+1
         cstor2(ind)=cstor1(il2)
      endif
      nlim=ind
      if(ind.EQ.mxcol) return

c --- Pad rest of output array
      indp1=ind+1
      do i=indp1,mxcol
         cstor2(i)=cblnk
      enddo

      return
      end
c----------------------------------------------------------------------
      subroutine tleft(cstor1,ilim1,il2,cstor2,nlim)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 040330                  TLEFT
c ---            D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  Remove all blank characters in the range ilim1 to il2
c               that lie to the LEFT of the first non-blank character
c               in the string after ilim1. Also remove the character
c               at ilim1 if it is blank.
c               Only characters in the range ilim1 to il2 may be
c               written to the output array.
c
c               Example --
c               Range    : ilim1=2, il2=19
c               CSTOR1   :  2   for this run   !
c               Position : 123456789111111111122
c                                   012345678901
c               CSTOR2   : 2for this run
c
c --- INPUTS:
c
c    CSTOR1(mxcol) - character*1 array - Input character string
c            ILIM1 - integer           - Array element at which search
c                                        for blanks begins
c              IL2 - integer           - Array element at which search
c                                        for blanks ends
c        Parameters: MXCOL
c
c --- OUTPUT:
c
c    CSTOR2(mxcol) - character*1 array - Output character string
c                                        (with left-blanks removed)
c             NLIM - integer           - Length of output string
c                                        (characters)
c
c --- TLEFT called by:  (utility)
c --- TLEFT calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cal'
c
      character*1 cstor1(mxcol),cstor2(mxcol),cblnk
      data cblnk/' '/

c --- Position of first non-blank character
      kfrst=0
      ilim1p1=ilim1+1
      do k=il2,ilim1p1,-1
         if(cstor1(k).NE.cblnk) kfrst=k
      enddo

      ind=0
c --- Pass first character in range if non-blank
      if(cstor1(ilim1).NE.cblnk) then
         ind=ind+1
         cstor2(ind)=cstor1(ilim1)
      endif

c --- Transfer all characters in range from kfrst
      if(kfrst.GT.0) then
         do k=kfrst,il2
            ind=ind+1
            cstor2(ind)=cstor1(k)
         enddo
      endif
      nlim=ind
      if(ind.EQ.mxcol) return

c --- Pad rest of output array
      indp1=ind+1
      do i=indp1,mxcol
         cstor2(i)=cblnk
      enddo

      return
      end
c----------------------------------------------------------------------
      subroutine setvar(itype,irep,xx,jj,ll,cv,xarr,jarr,larr,carr)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 950122                 SETVAR
c ---            J. Scire, SRC
c
c --- PURPOSE:  Fill the output variable or array with the value read
c               from the input file
c
c --- INPUTS:
c
c            ITYPE - integer        - Variable type (1=real, 2=integer,
c                                     3=logical, 4=character*4)
c             IREP - integer        - Repetition factor
c                                     If ITYPE = 4, IREP refers to the
c                                     number of characters in the field)
c               XX - real           - Real value read from input
c                                     file (Used only if ITYPE=1)
c               JJ - integer        - Integer value read from input
c                                     file (Used only if ITYPE=2)
c               LL - logical*4      - Logical value read from input
c                                     file (Used only if ITYPE=3)
c        CV(mxcol) - character*4    - Character*4 values read from input
c                                     file (Used only if ITYPE=4)
c
c         PARAMETER:  MXCOL
c
c --- OUTPUT:
c
c          XARR(*) - real array     - Output real array (or scalar if
c                                     IREP=1) -- Used only if ITYPE=1
c          JARR(*) - integer array  - Output integer array (or scalar if
c                                     IREP=1) -- Used only if ITYPE=2
c          LARR(*) - logical array  - Output logical array (or scalar if
c                                     IREP=1) -- Used only if ITYPE=3
c          CARR(*) - character*4    - Output character*4 array (or
c                                     scalar if IREP=1) -- Used only if
c                                     ITYPE=4
c
c --- SETVAR called by:  READIN
c --- SETVAR calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cal'
c
      real xarr(*)
      integer jarr(*)
      logical*4 larr(*),ll
      character*4 carr(*),cv(mxcol)
c
      go to(10,20,30,40),itype
c
c --- real variable
10    continue
      do 15 i=1,irep
      xarr(i)=xx
15    continue
      return
c
c --- integer variable
20    continue
      do 25 i=1,irep
      jarr(i)=jj
25    continue
      return
c
c --- logical variable
30    continue
      do 35 i=1,irep
      larr(i)=ll
35    continue
      return
c
c --- character*4 variable string
40    continue
      do 45 i=1,irep
      carr(i)=cv(i)
45    continue
      return
      end
c----------------------------------------------------------------------
      subroutine allcap(cstor2,nlim)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 950122                 ALLCAP
c ---            J. Scire, SRC
c
c --- PURPOSE:  Convert all lower case letters within a character
c               string to upper case
c
c --- INPUTS:
c
c    CSTOR2(mxcol) - character*1 array - Input character string
c             NLIM - integer           - Length of string (characters)
c        Parameters: MXCOL
c
c --- OUTPUT:
c
c    CSTOR2(mxcol) - character*1 array - Output character string with
c                                        lower case letters converted
c                                        to upper case
c
c --- ALLCAP called by:  READIN
c --- ALLCAP calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cal'
c
      character*1 cstor2(mxcol),cchar,clc(29),cuc(29)
c
      data clc/'i','n','x','a','e','o','u','b','c','d','f','g','h',
     1 'j','k','l','m','p','q','r','s','t','v','w','y','z','-','.',
     2 '*'/
      data cuc/'I','N','X','A','E','O','U','B','C','D','F','G','H',
     1 'J','K','L','M','P','Q','R','S','T','V','W','Y','Z','-','.',
     2 '*'/
c
      do 100 i=1,nlim
      cchar=cstor2(i)
c
      do 50 j=1,29
      if(cchar.eq.clc(j))then
         cstor2(i)=cuc(j)
         go to 52
      endif
50    continue
52    continue
100   continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine datetm(rdate,rtime,rcpu)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 010731                 DATETM
c ---            J. Scire, SRC
c
c --- PURPOSE:  Get system date and time from system clock
c
c --- INPUTS:  none
c
c --- OUTPUT:  rdate  - C*10 - Current system date (MM-DD-YYYY)
c              rtime  - C*8  - Current system time (HH:MM:SS)
c               rcpu  - real - CPU time (sec) from system utility
c
c --- DATETM called by:  SETUP, FIN
c --- DATETM calls:      DATE, TIME (Lahey F77 compiler utilities) 
c                        ETIME (SUN F77 CPU utility program) 
c                        DATE_AND_TIME (COMPAQ, LF95 compilers) 
c                        CPU_TIME (F95) 
c                        YR4C 
c----------------------------------------------------------------------
      character*8  rtime
      character*10 rdate

c --- Local store
      character*11 stime
      character*8 sdate

c --- Set initial base CPU time to -1. 
      data rcpu0/-1./ 
      SAVE rcpu0 
c --- Get system date and time [COMPILER-SPECIFIC!] 

cc --- Lahey F77L Compiler (begin)
cc -------------------------------
cc --- System date in MM-DD-YY
cc --- System clock in HH:MM:SS.ss, where ss = hundredths of seconds
c      call DATE(sdate)
c      call TIME(stime)
cc --- Pass to output formats (MM-DD-YYYY) and (HH:MM:SS)
c      rdate='  -  -    '
c      rdate(1:2)=sdate(1:2)
c      rdate(4:5)=sdate(4:5)
c      rdate(7:8)='00'
c      rdate(9:10)=sdate(7:8)
c      do i=1,8
c         rtime(i:i)=stime(i:i)
c      enddo
cc --- Get CPU time from SUN system utility (or PC dummy) 
c      call etime(rcpu1) 
cc --- Lahey F77L Compiler (end) 

c --- Get CPU time from LF95 system utility 
      call CPU_TIME(rcpu1) 
c --- COMPAQ DF90/95 and Lahey LF95 Compilers (end) 
c ---------------------------------------------------
c --- System date in CCYYMMDD
c --- System clock in HHMMSS.sss, where sss = thousandths of seconds
      call DATE_AND_TIME(sdate,stime)
c --- Pass to output formats (MM-DD-YYYY) and (HH:MM:SS)
      rdate='  -  -    '
      rdate(1:2)=sdate(5:6)
      rdate(4:5)=sdate(7:8)
      rdate(7:10)=sdate(1:4)
      rtime='  :  :  '
      rtime(1:2)=stime(1:2)
      rtime(4:5)=stime(3:4)
      rtime(7:8)=stime(5:6)
c --- COMPAQ DF90/95 and Lahey LF95 Compilers (end)

c --- Construct 4-digit year from current 2-digit year (if found)
      read(rdate(7:10),'(i4)') iyr
      call YR4C(iyr)
      write(rdate(7:10),'(i4)') iyr

c --- Update base CPU time on first call 
      if(rcpu0.LT.0.0) rcpu0=rcpu1 
c --- Return CPU time difference from base 
      rcpu=rcpu1-rcpu0 

      return 
      end 
c----------------------------------------------------------------------
      subroutine etime(rcpu)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 941215                  ETIME
c ---            J. Scire, SRC
c
c --- PURPOSE:  Dummy system CPU time routine for PC
c               DO NOT USE THIS ROUTINE ON SUNs
c
c --- INPUTS:  none
c
c --- OUTPUT:  RCPU  - real - CPU time (sec) -- set to zero for PC
c
c --- ETIME called by:  DATETM
c --- ETIME calls:      none
c----------------------------------------------------------------------
      rcpu=0.0
c
      return
      end
c----------------------------------------------------------------------
      subroutine undrflw(lflag)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 030402                UNDRFLW
c                D. Strimaitis,  Earth Tech Inc.
c
c --- PURPOSE:  This routine takes advantage of the Lahey F77L routine
c               UNDER0 to set underflows to zero.  When other compilers
c               are used, there may be a similar routine.  If none
c               exists, place a dummy statement here and use compiler
c               switches to configure the NDP response to an underflow.
c
c               This routine contains calls for several different
c               compilers, but only one should be active at any one
c               time.
c
c----------------------------------------------------------------------
      logical lflag

cc --- Lahey F77L Compiler (begin)
cc -------------------------------
cc --- Lahey F77 compiler -- set underflows ( < 10**-38 ) to zero
c      call UNDER0(lflag)
cc --- Lahey F77L Compiler (end)

c --- Dummy (no action on underflows)
c -----------------------------------
      lflag=.TRUE.
c --- Dummy (end)

      return
      end
c----------------------------------------------------------------------
      subroutine comline(ctext)
c----------------------------------------------------------------------
c
c --- CALUTILS   Version: 2.54     Level: 040330                COMLINE
c                J. Scire, SRC
c
c --- PURPOSE:  Call the compiler-specific system routine that will
c               pass back the command line argument after the text
c               that executed the program
c
c               This routine contains calls for several different
c               compilers, but only one should be active at any one
c               time.
c
c --- UPDATE
c --- V2.2 (960521) to V2.3 (040330)  DGS
c               - Replace strings c*70 with c*132
c
c --- INPUTS:
c
c          CTEXT - character*132 - Default command line argument #1
c
c --- OUTPUT:
c
c          CTEXT - character*132 - Command line argument #1
c                                  If command line argument is
c                                  missing, CTEXT is not changed
c
c --- COMLINE called by:  SETUP
c --- COMLINE calls:      GETCL - Lahey system routine
c                         NARGS, GETARG - Microsoft routines
c                         NARGS, GETARG - Compaq DF routines
c                         IARGC, GETARG - Sun routines
c                         IARGC, GETARG - HP routines
c----------------------------------------------------------------------
c
c ----------------------------------------
c --- COMPAQ DF compiler library directive
c *** USE DFLIB
c ----------------------------------------
c
      character*132 ctext,cdeflt
c
c --- Microsoft variables
c *** integer*2 iarg,istat
c
c --- HP declaration
c *** external getarg
c *** external iargc
c
c --- The following is for any system without a command line routine
c --- and is also used as a default
      cdeflt=ctext
c
c ----------------------
c --- Intel ifort and Lahey lf95 compilers:
c     NARGS is an intrinsic function that returns the number of command-line
c         arguments INCLUDING the command.
c ----------------------
c***       numargs=NARGS()
c***       if(numargs.ge.1)then
c***          call GETARG(1,ctext)
c***       endif
c
c --------------------------------------------------------
c --- Sun compiler, GNU gfortran, OR
c --- HP compiler -- NOTE: HP needs +U77 switch on compile
c --------------------------------------------------------
      numargs=IARGC()
      if(numargs.ge.1)then
        call GETARG(1,ctext)
      endif
c
c
c --- If no command line arguments, use default
      if(ctext(1:1).eq.' ')ctext=cdeflt

      return
      end
