c----------------------------------------------------------------------
c --- COORDLIB -- COORDINATE SYSTEM UTILITIES
c----------------------------------------------------------------------
c
c --- COORDLIB   Version: 1.99     Level: 070921
c 
c     Copyright (c) 2003-2007 by Exponent, Inc.
c
c -----------------------------
c --- CONTENT:
c -----------------------------
c
c --- Interface routines
c      subroutine GLOBE1
c      subroutine GLOBE
c      subroutine NIMADATE
c      subroutine COORDSVER
c
c --- Coordinate transformation engine
c      subroutine COORDS
c      (and subroutines)
c -----------------------------
c
c --- UPDATE
c
c --- V1.98-V1.99   070921  (DGS): Modify UTM section of PJINIT in
c                                  COORDS to fix erroneous non-zero
c                                  false Northing when converting S.
c                                  hemisphere locations to UTM-N
c                                  coordinates
c                                  Initialize full work arrays DWRK,
c                                  DWRK2, TDUM to zero
c                                  Initialize UTMOUT to zero
c
c --- V1.97-V1.98   060911  (DGS): Changes in COORDS that allow a higher
c                                  level of FORTRAN error checking.
c
c --- V1.96-V1.97   060626  (DGS): Add subroutine GLOBE1 (from CALUTILS)
c                                  after removing link to CALUTILS
c                                  components
c
c --- V1.95-V1.96   051010  (KAM): ADD ALBERS CONICAL EQUAL AREA (ACEA)  
c                                  PROJECTION AS ONE OF THE SUPPORTED
c                                  PROJECTIONS IN SUBROUTINE COORDS.
c
c --- V1.94-V1.95   050126  (GEM): FORBID UTM CONVERSION TO BE DONE
c                                  FOR A NON-USGS SPHEROID. ADDED AN ERROR
c                                  STRING TO THE COORDS CALL BETWEEN IRET
c                                  AND DSTAMPIN. ADDED THE IRET CODE 99
c                                  FOR THE CASE WHEN THE FORBIDDEN UTM
c                                  CONVERSION IS ENCOUNTERED. ALSO FIXED
c                                  THE UTM TO UTM CASE WHEN THE OUTPUT UTM
c                                  ZONE IS NOT SPECIFIED. USES THE INPUT
c                                  (OR NATURAL) ZONE TO AVOID ZEROES.
C                           (GEM): Added IRET=98 error code for a LAZA
c                                  projection with a datum that is not a
c                                  sphere (e.g. not NWS-84 or ESR-S).
c                           (GEM): LAZA Projection:  removed assignment
c                                  of 6370 km earth radius (NWS-84 datum)
c                                  when a value less than 6000 km is
c                                  found.  This assignment can override
c                                  a requested radius of 6371 (ESR-S
c                                  datum) if the NWS-84 datum is used
c                                  with any valid projection prior to the
c                                  request for ESR-S.  LAZA(NWS-84)
c                                  coordinate distances from the
c                                  projection origin are about 0.016%
c                                  smaller than LAZA(ESR-S).
c                           (DGS): Introduce subroutine COORDSVER
c --- V1.93-V1.94   041007  (GEM): CORRECTED CASE WHERE UTM EQUATOR
c                                  CROSSOVER WAS DONE INCORRECTLY WHEN
c                                  MOVING FROM ONE DATUM TO ANOTHER - A
c                                  CONTINUATION OF THE FIX IN THE
c                                  PREVIOUS VERSION.
c --- V1.92-V1.93   040713  (GEM): CORRECTED CASE WHERE UTM EQUATOR
c                                  CROSSOVER WAS DONE INCORRECTLY AND
c                                  FIXED THE CASE WHERE NWS-84 UNDER
c                                  UTM USE DID NOT HAVE A VALID ELLIPSE
c                                  MODEL INPUT
c --- V1.91-V1.92   031201  (GEM): CORRECTED CASE WHERE ONLY A CHANGE
C                                  IN THE SAME PROJECTION IS DESIRED
c --- V1.9-V1.91    031017  (GEM): CORRECTED WGS 72 AND FIXED ELLIPSOID
c                                  INITIALIZATION
c --- V1.15-V1.9    030905  (GEM): MAPLIB VERSION 1.9     030905
c                                  Rename MAPLIB system to COORDLIB
c --- V1.14-V1.15   030528  (DGS): MAPLIB VERSION 1.85    030528
c --- V1.13-V1.14   030402  (DGS): MAPLIB VERSION 1.84    030402
c --- V1.12-V1.13   030307  (DGS): MAPLIB VERSION 1.83    030307
c                                  NIMA Date now C*12 (MM-DD-YYYY  )
c --- V1.11-V1.12   030221  (DGS): Add routine to pass NIMA date
c --- V1.1-V1.11    030217  (DGS): Revise COORDS error message
c --- V1.0-V1.1     030117  (DGS): Add date stamp to COORDS call
c                                  MAPLIB VERSION 1.8A    011403
c
c----------------------------------------------------------------------
      subroutine globe1(cmapi,iutmzni,tmsfi,xlat1i,xlat2i,rlati,rloni,
     &                  feasti,fnorti,
     &                  cmapo,iutmzno,tmsfo,xlat1o,xlat2o,rlato,rlono,
     &                  feasto,fnorto,
     &                  caction,vecti,vecto)
c----------------------------------------------------------------------
c
c --- COORDLIB   Version: 1.99     Level: 060626                 GLOBE1
c                D. Strimaitis
c
c --- PURPOSE:  Setup for coordinate transformation routine COORDS
c
c --- UPDATE
c --- V1.97(060626) (DGS)
c               - Transferred from CALUTILS
c               - Remove calls to DEBLNK and ALLCAP to isolate
c --- ...CALUTILS... 
c --- V2.3 (051019) from V2.2 (030528) (KAM)
c               - Add Albers Conical Equal Area projection
c --- V2.2 (030528) from V2.1 (030402) (DGS)
c               - Screen for valid UTM zone using absolute value
c                 (S. Hem. zones are negative)
c --- V2.1 (030402) from V2.0 (021018) (DGS)
c               - Add False Easting & Northing inputs
c
c --- INPUTS:
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
c            TMSFI - real       - Scale Factor for TM projection
c           XLAT1I - real       - Matching Equator-ward N.Latitude
c           XLAT2I - real       - Matching Pole-ward N.Latitude
c            RLATI - real       - Map origin N.Latitude
c            RLONI - real       - Map origin E.Longitude
c           FEASTI - real       - False Easting (km) at proj. origin
c           FNORTI - real       - False Northing (km) at proj. origin
c            CMAPO - char*8     - Map projection of output coordinates
c                                  LL  : N.Lat., E.Long.
c                                  UTM : Universal Transverse Mercator
c                                  TM  : Transverse Mercator
c                                  LCC : Lambert Conformal Conic
c                                  PS  : Polar Stereographic
c                                  EM  : Equatorial Mercator
c                                  LAZA: Lambert Azimuthal Equal Area
c                                  ACEA: Albers Conical Equal Area
c          IUTMZNO - integer    - UTM zone of input coords.
c                                  (S. hemisphere is NEGATIVE)
c            TMSFO - real       - Scale Factor for TM projection
c           XLAT1O - real       - Matching Equator-ward N.Latitude
c           XLAT2O - real       - Matching Pole-ward N.Latitude
c            RLATO - real       - Map origin N.Latitude
c            RLONO - real       - Map origin E.Longitude
c           FEASTO - real       - False Easting (km) at proj. origin
c           FNORTO - real       - False Northing (km) at proj. origin
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
c --- GLOBE1 called by: (utility)
c --- GLOBE1 calls:     none
c----------------------------------------------------------------------

      character*1 cstor1(20),cstor2(20),clc(26),cuc(26)

      real*8 vecti(9),vecto(9)
      character*12 caction
      character*8 cmapi,cmapo

      data clc/'i','n','x','a','e','o','u','b','c','d','f','g','h',
     &         'j','k','l','m','p','q','r','s','t','v','w','y','z'/
      data cuc/'I','N','X','A','E','O','U','B','C','D','F','G','H',
     &         'J','K','L','M','P','Q','R','S','T','V','W','Y','Z'/

c --- Set action string for conversion
c ------------------------------------
c --- Initialize character variables for output
      do i=1,20
         cstor1(i)=' '
         cstor2(i)=' '
      enddo
      do i=1,8
         j=i+9
         cstor1(i)=cmapi(i:i)
         cstor1(j)=cmapo(i:i)
      enddo
      cstor1(9)='2'
c --- Remove blank characters from string, place in storage array 2
      nlim=0
      do i=1,17
         if(cstor1(i).NE.' ') then
c ---       Transfer non-blank character into array 2
            nlim=nlim+1
            cstor2(nlim)=cstor1(i)
         endif
      enddo
c --- Convert lower case letters to upper case
      do i=1,nlim
         do j=1,26
            if(cstor2(i).EQ.clc(j)) then
               cstor2(i)=cuc(j)
               go to 52
            endif
         enddo
52       continue
      enddo
c --- Transfer characters to action string
      do i=1,12
         caction(i:i)=cstor2(i)
      enddo

c --- Set transformation vectors
c ------------------------------
c --- Initialize transformation vectors
      vecti(1)=999.0D0
      vecto(1)=999.0D0
      do i=2,9
         vecti(i)=0.0D0
         vecto(i)=0.0D0
      enddo

c --- Input coords
      if(cmapi.EQ.'UTM') then
c ---    UTM zone
         if(IABS(iutmzni).GT.0 .AND.
     &      IABS(iutmzni).LT.61) vecti(1)=DBLE(iutmzni)
      else
c ---    Matching points / origin
         vecti(4)=DBLE(xlat1i)
         vecti(5)=DBLE(xlat2i)
         vecti(6)=DBLE(rloni)
         vecti(7)=DBLE(rlati)
      endif
      if(cmapi.EQ.'TM') then
c ---    TM Scale Factor
         vecti(1)=DBLE(tmsfi)
      endif
      if(cmapi.EQ.'TM'.or.cmapi.EQ.'LCC'.or.cmapi.EQ.'LAZA'.or.
     &                    cmapi.EQ.'ACEA') then
         vecti(8)=DBLE(feasti)
         vecti(9)=DBLE(fnorti)
      endif

c --- Output coords
      if(cmapo.EQ.'UTM') then
c ---    UTM zone
         if(IABS(iutmzno).GT.0 .AND.
     &      IABS(iutmzno).LT.61) vecto(1)=DBLE(iutmzno)
      else
c ---    Matching points / origin
         vecto(4)=DBLE(xlat1o)
         vecto(5)=DBLE(xlat2o)
         vecto(6)=DBLE(rlono)
         vecto(7)=DBLE(rlato)
      endif
      if(cmapo.EQ.'TM') then
c ---    TM Scale Factor
         vecto(1)=DBLE(tmsfo)
      endif
      if(cmapo.EQ.'TM'.or.cmapo.EQ.'LCC'.or.cmapo.EQ.'LAZA'.or.
     &                    cmapo.EQ.'ACEA') then
         vecto(8)=DBLE(feasto)
         vecto(9)=DBLE(fnorto)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine globe(iolst,caction,cdatumi,vecti,cdatumo,vecto,
     &                 xinp4,yinp4,xout4,yout4,izone,utmhem)
c----------------------------------------------------------------------
c
c --- COORDLIB   Version: 1.99     Level: 050126                  GLOBE
c                D. Strimaitis   EarthTech
c
c --- PURPOSE:  Driver for coordinate transformation routine COORDS
c               translates CALPUFF system information and provides
c               fixed inputs
c
c --- UPDATE
c
c --- V1.13 (030307) to V1.95 (050126)
c               - Added ESTRNG string to COORDS call for error message
c                 text.                                           (GEM)
c               - Added VERDOC string to COORDS call for identification
c                 text                                            (DGS)
c --- V1.12 (030217) to V1.13 (030307)  (DGS)
c               - Change NIMA date from C*10 to C*12
c --- V1.1 (030117) to V1.11 (030217)  (DGS)
c               - Revise return error message
c --- V1.0 () to V1.1 (030117)  (DGS)
c               - Add date stamp to COORDS calls
c
c --- INPUTS:
c            IOLST - integer    - Unit number for list file output
c          CACTION - char*12    - Map conversion string (e.g., UTM2LCC)
c          CDATUMI - char*8     - Datum-region code for input coords
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
c          CDATUMO - char*8     - Datum-region code for output coords
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
c            XINP4 - real*4     - Input Easting(km) (or E.Longitude deg)
c            YINP4 - real*4     - Input Northing(km) (or N.Latitude deg)
c
c
c --- OUTPUT:
c            XOUT4 - real*4     - Output Easting(km) (or E.Longitude deg)
c            YOUT4 - real*4     - Output Northing(km) (or N.Latitude deg)
c            IZONE - integer    - UTM zone of output
c           UTMHEM - char*4     - Hemisphere for UTM projection (N or S)
c
c --- GLOBE called by: (utility)
c --- GLOBE calls:     COORDS
c----------------------------------------------------------------------
      parameter (nc = 3, ndat = 6)

      real*8 vecti(9),vecto(9),xyzin(nc),xyzio(nc),utmout
      real*8 xdatum(ndat)

      logical ldb

      character*4 utmhem
      character*10 iunit
      character*8 cdatumi,cdatumo
      character*12 caction
      character*12 dstamp
      character*50 estrng, verdoc

      data iunit/'KILOMETERS'/
      data imode/0/, iprec/1/, nvec/9/
      data xdatum/0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/

c --- Set debug output logical
      ldb=.FALSE.

c --- Set dstamp to blank string to invoke default in COORDS
      dstamp='            '

c --- Convert input coordinates to double precision
      xyzin(1)=DBLE(xinp4)
      xyzin(2)=DBLE(yinp4)

      mcp=nc
      mdat=ndat
      xyzin(3) = 1.0D0
      xyzio(3) = 1.0D0

      call COORDS(iolst,iunit,imode,caction,cdatumi,cdatumo,iprec,
     &            vecti,vecto,nvec,xyzin,mcp,xdatum,mdat,
     &            xyzio,utmout,iret,estrng,dstamp,verdoc)

      IF(IRET.NE.0)THEN
         write(iolst,*)'GLOBE: COORDS FAILED - ',estrng
         write(iolst,*)
         write(iolst,*)'COORDS arguments -----------'
         write(iolst,*)'iunit   = ',iunit
         write(iolst,*)'imode   = ',imode
         write(iolst,*)'caction = ',caction
         write(iolst,*)'cdatumi = ',cdatumi
         write(iolst,*)'cdatumo = ',cdatumo
         write(iolst,*)'iprec   = ',iprec
         write(iolst,*)'vecti   = ',(vecti(j),j=1,nvec)
         write(iolst,*)'vecto   = ',(vecto(j),j=1,nvec)
         write(iolst,*)'xyzin   = ',(xyzin(j),j=1,mcp)
         write(iolst,*)'xyzio   = ',(xyzio(j),j=1,mcp)
         write(iolst,*)'xdatum  = ',(xdatum(j),j=1,mdat)
         write(iolst,*)'utmout  = ',utmout
         write(iolst,*)'iret    = ',iret
         write(iolst,*)'dstamp  = ',dstamp
         write(iolst,*)'verdoc  = ',verdoc
         write(iolst,*)
         write(*,*)
         write(*,*)'GLOBE: COORDS FAILED - ',estrng
         stop 'Halted in GLOBE - see list file.'
      endif

c --- Convert output coordinates to single precision
      xout4=SNGL(xyzio(1))
      yout4=SNGL(xyzio(2))
      utmzn=SNGL(utmout)
      izone=NINT(utmzn)

c --- Format UTM zone to CALPUFF convention
      utmhem='N'
      if(izone.LT.0) then
         utmhem='S'
         izone=-izone
      endif

      if(LDB) then
         write(iolst,*)
         write(iolst,*)'COORDS arguments -----------'
         write(iolst,*)'iunit   = ',iunit
         write(iolst,*)'imode   = ',imode
         write(iolst,*)'caction = ',caction
         write(iolst,*)'cdatumi = ',cdatumi
         write(iolst,*)'cdatumo = ',cdatumo
         write(iolst,*)'iprec   = ',iprec
         write(iolst,*)'vecti   = ',(vecti(j),j=1,nvec)
         write(iolst,*)'vecto   = ',(vecto(j),j=1,nvec)
         write(iolst,*)'xyzin   = ',(xyzin(j),j=1,mcp)
         write(iolst,*)'xyzio   = ',(xyzio(j),j=1,mcp)
         write(iolst,*)'xdatum  = ',(xdatum(j),j=1,mdat)
         write(iolst,*)'utmout  = ',utmout
         write(iolst,*)'iret    = ',iret
         write(iolst,*)'dstamp  = ',dstamp
         write(iolst,*)'verdoc  = ',verdoc
         write(iolst,*)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine nimadate(date)
c----------------------------------------------------------------------
c
c --- COORDLIB   Version: 1.99     Level: 030905               NIMADATE
c                D. Strimaitis   EarthTech
c
c --- PURPOSE:  Passes the NIMA date from common to calling program
c
c --- UPDATE
c --- V1.13 (030307) to V1.9 (030905)  (GEM)
c               - Change to NIMA.CRD for MAPLIB VERSION 1.9
c --- V1.12 (030221) to V1.13 (030307)  (DGS)
c               - Change NIMA date from C*10 to C*12
c
c --- INPUTS:
c             none
c
c --- OUTPUT:
c             DATE - char*12    - NIMA database date
c
c --- NIMADATE called by: (utility)
c --- NIMADATE calls:     none
c----------------------------------------------------------------------
      include 'nima.crd'
      character*12 date

      date=daten

      return
      end
c----------------------------------------------------------------------
      subroutine coordsver(iolst,verdoc)
c----------------------------------------------------------------------
c
c --- COORDLIB   Version: 1.99     Level: 050126             COORDSVER
c                D. Strimaitis   EarthTech
c
c --- PURPOSE:  Accesses the COORDS version information by making one
c               generic call to COORDS (like GLOBE)
c
c --- INPUTS:
c            IOLST - integer    - Unit number for list file output
c
c --- OUTPUT:
c           VERDOC - char*50    - COORDS version information
c
c --- COORDSVER called by: (utility)
c --- COORDSVER calls:      COORDS
c----------------------------------------------------------------------
      parameter (nc = 3, ndat = 6)

      real*8 vecti(9),vecto(9),xyzin(nc),xyzio(nc),utmout
      real*8 xdatum(ndat)

      character*10 iunit
      character*8 cdatumi,cdatumo
      character*12 caction
      character*12 dstamp
      character*50 estrng, verdoc

      data iunit/'KILOMETERS'/
      data imode/0/, iprec/1/, nvec/9/
      data xdatum/0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      data vecti/0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      data vecto/0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/

c --- Set dstamp to blank string to invoke default in COORDS
      dstamp='            '

c --- Set up converter for a null translation of lat/lon
      xinp4= -90.0
      yinp4=45.0
      caction='LL2LL       '
      cdatumi='WGS-84  '
      cdatumo='WGS-84  '

c --- Convert input coordinates to double precision
      xyzin(1)=DBLE(xinp4)
      xyzin(2)=DBLE(yinp4)

      mcp=nc
      mdat=ndat
      xyzin(3) = 1.0D0
      xyzio(3) = 1.0D0

      call COORDS(iolst,iunit,imode,caction,cdatumi,cdatumo,iprec,
     &            vecti,vecto,nvec,xyzin,mcp,xdatum,mdat,
     &            xyzio,utmout,iret,estrng,dstamp,verdoc)

      IF(IRET.NE.0)THEN
         write(iolst,*)'GLOBE: COORDS FAILED - ',estrng
         write(iolst,*)
         write(iolst,*)'COORDS arguments -----------'
         write(iolst,*)'iunit   = ',iunit
         write(iolst,*)'imode   = ',imode
         write(iolst,*)'caction = ',caction
         write(iolst,*)'cdatumi = ',cdatumi
         write(iolst,*)'cdatumo = ',cdatumo
         write(iolst,*)'iprec   = ',iprec
         write(iolst,*)'vecti   = ',(vecti(j),j=1,nvec)
         write(iolst,*)'vecto   = ',(vecto(j),j=1,nvec)
         write(iolst,*)'xyzin   = ',(xyzin(j),j=1,mcp)
         write(iolst,*)'xyzio   = ',(xyzio(j),j=1,mcp)
         write(iolst,*)'xdatum  = ',(xdatum(j),j=1,mdat)
         write(iolst,*)'utmout  = ',utmout
         write(iolst,*)'iret    = ',iret
         write(iolst,*)'dstamp  = ',dstamp
         write(iolst,*)'verdoc  = ',verdoc
         write(iolst,*)
         write(*,*)
         write(*,*)'GLOBE: COORDS FAILED - ',estrng
         stop 'Halted in GLOBE - see list file.'
      endif

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE COORDS(IO,IUNIT,IMODE,IPROJ,IDATMI,IDATMO,IPREC,
     1 CVECTI,CVECTO,NVEC,XYZIN,NC,XDATUM,NDAT,XYZIO,UTMOUT,IRET,
     2 ESTRNG,DSTAMPIN,VERDOC)
C----------------------------------------------------------------------
C
C --- COORDLIB   Version: 1.99     Level: 070921                 COORDS
C
C --- Program was written by Gary Moore
C
C --- PROGRAM NOTES FOLLOW:
C
C --- Version 1.1 argument change
C
C --- IDATMI(O) - FULL CHARACTER STRING FOR GUI SUPPLIED (IRANK REMOVED)
C --- XDATUM,NDAT - PASS FULL ARRAY OF USER DEFINED DATUM INFO (DP)
C
C --- (1) - MAJOR RADIUS
C --- (2) - INVERSE FLATTENING
C --- (3) - ECCENTRICITY SQUARED
C --- (4) - DX
C --- (5) - DY
C --- (6) - DZ
C
C --- Version 1.2 argument change
C
C --- UTMOUT a double precision output UTM zone is used in the convert
C --- program as output to tell what UTM each point has been translated
C --- TO.
C
C --- Version 1.3 changes
C
C --- Addition of LL2ZONE subroutine for extracting the natural UTM zone
C --- when going FROM LCC TO UTMS - otherwise there is no way of knowing
C --- added extra projection calls in places to retrieve the geodetic
C --- coordinates.
C
C --- Version 1.4 changes
C
C --- Fixed the use of the FROM ellipsoid model for the final projection
C --- and changed to it to the TO ellipsoid model. Fixed the DAT2DAT and
C --- DATSHFT routines so that the proper reverse transformation proceedure
C --- is done (note - changed presentation figures)
C
C --- Version 1.5 changes
C
C --- Added more options for transformation - PS = Polar Stereographic
C --- and EM = Equatorial Mercator. Note - both of these will generally
C --- be used on a spherical earth represented by Datum 220, but can
C --- be projected to an ellipical surface - unlike the azimuthal
C --- projections that can only be done on a sphere. The LAZA was hardwired
C --- to do only a sphere with a radius of 6370 km (before it could float
C --- incorrectly).
C
C --- The block data variables were modified to accomodate the new NIMA
C --- data base. Block data call was moved to the INIT subroutine which
C --- sets up variables for COORDS and outputs several arrays for use with
C --- GUI's
C
C --- The NIMA data base use resulted in a considerable set of code
C --- revisions including (1) 8 Character Datum ID use for selecting the
C --- Datum (2) use of a 21 character ellipsoid string check (3) use of
C --- a revised 118 character region string.
C
C --- An INCLUDE file 'NIMA.CRD' was used to insert the NIMA common
C --- blocks into routines.
C
C --- version 1.6 changes
C
C --- Made several upgrades including:
C
C --- (1) adds a date check to make sure the block data is the right
C ---     version.  This requires adding an extra argument to COORDS
C
C --- (2) adds the Tranverse Mercator projection (TM)
C
C --- (3) add error codes for projections
C
C --- (4) allows the user input 'to' (output) utm zone to work
c
c --- Changed the ordering of CVECTI/CVECTO elements 4-7 to be consistent
c --- across all transformations, rather than following the USGS element
c --- definitions.  Lat/Lon of origin of EM and PS projections is accepted
c --- and the corresponding false Easting/Northing values are computed
c --- and applied.  The elements of the transformation vector are:
c        (1)  UTM Zone (for UTM), or Scale Factor (for TM)
c        (2)  radius of major axis of earth - (used for Azimuthal projections)
c        (3)  not currently used
c        (4)  True N. Latitude #1 (where applicable)
c        (5)  True N. Latitude #2 (where applicable)
c        (6)  E. Longitude of projection origin (where applicable)
c        (7)  N. Latitude of projection origin (where applicable)
c        (8)  False Easting (where applicable)
c        (9)  False Northing (where applicable)
C
C --- Version 1.7 changes
C
C --- Moved false northing determination of TO projections to a point
C --- where they occur AFTER a datum shift
C
C --- Added dummy arrays to keep longitude/latitudes from being written
C --- over.
C
C --- Removed writes to standard output so DLL's can be directly made
C
C --- Removed external date check changes to an internal one
C
C --- Further revisions to PS and EM cases the user cannot input a
C --- false northing and easting - and error is returned if they do
C
C --- Fixed PS2PS and EM2EM cases
C
C --- Version 1.8 changes
C
C --- Dealt with a major issue of projection initialization that is done
C --- with INZONE. Initialization is done when the UTM zone changes. Software
C --- was added to make sure this happens.
C
C --- The PS/EM projections had consistency problems when the offset is
C --- calculated with a 0.0 rather than a true longitude - the true longitude
C --- was used.
C
C --- An error in the PS/EM projection was corrected when the input
C --- parameter vector was found to be using an incorrect latitude of
C --- true scale.
C
C --- Error warnings were included to make sure that no false eastings
C --- or northings are input by the user of the PS and EM projections.
C
C --- Version 1.81 changes
C
C --- Modified USGS routines to force initialization every time by
C --- setting the switch array to zero for all projections on each call
C
C --- Added DGS approach to checking date stamp using DATEN and DATEB
C
C --- Added include for the block data (blockdat.crd)
C
C --- Version 1.82 changes
C
C --- Fixed the TM insertion of scaling factor - moved it from the USGS
C --- Element # 3 (CVECT element #4) to the CVECT element #1 normally
C --- (UTM ZONE) - the UTM zone is now set to 999. There is a mapping
C --- of the UTM ZONE to the USGS element 3 and a resetting of the
C --- UTM zone to 999 before entering the USGS subroutines.
C
C --- Scale false Easting/Northing to METERS
C --- Correct false Easting/Northing assignments after processing
C
C --- Convert main program to the CALPUFF Version/Level designation
C --- where Level is YYMMDD
C
C --- Added date-stamp argument DSTAMPIN to re-assign DSTAMP if the
C --- argument is non-blank
C
C --- Version 1.83 changes
C
C --- NIMA date variables changed from C*10 to C*12
C --- DAT2DAT does not transform to/from WGS84 if input/output datum
C     is for a sphere
C
C --- Version 1.84 changes
C
C --- Recast UTM-to-UTM conversions to properly handle zone overrides
C     by adding IOVUTM:
C     0)  finds native output UTM zone for output UTMs
C     1)  no change to input coordinates when inzone=iozone
C     2)  uses zone override for output UTMs
C
C --- Version 1.85   Level: 030528  changes
C
C --- Fix Polar Stereographic (PS) dummy array initialization which
C     did not include the Earth Radius for spherical datum, and clarify
C     code (remove unneeded dummy arrays)
c
c --- Take absolute value of UTM zone when testing for valid values
c     (UTM is negative in S. Hemisphere)
c
C
C --- Version 1.9   Level: 030905  changes
C
C --- NEW BLOCK DATA!!!! The new block data was created by version 1.3
C     of BUILD.FOR which utilizes new data sources for DATUMs. These new
C     files include:
C
C --- (1) New HEADER.TXT which defines two new global datum and removes
C         one spherical earth datum (based on NAD 27). The two new datums
C         are functionally equivalent and they serve as a placeholder to
C         assure users they have the proper DATUM
C
C --- (2) New Datum data files GEOTRANS_02-21-2003.dat and ellips.dat
C         These new data files are required since the DATUM listing text
C         file produced by NIMA is not available for the latest changes
C         in datum definitions. Instead the user is referred to the data
C         files used by the NIMA GEOTRANS geocalculator. The ellips.dat
C         file contains the parameters defining 23 ellipsoid models used
C         to define the datums. These are matched by two character codes
C         to the differences in geocentric coordinates of each datum
C         relative to WGS-84 found in GEOTRANS_02-21-2003. The
C         GEOTRANS_02-21-2003.dat file contains five new local datums -
C         all which are Hawaian Island local variants.
C
C --- (3) NEWDATUM.TXT is a new file that has been added to allow insertion
C         of new datums into the proper place in the master list of local
C         datums. This file also allows one to add descriptive text (3 lines)
C         describing the valid region or conditions of the datum.
C
C --- (4) Introduced the WGS72 global data and added formulas
C         to deal with the coordinate transformations between WGS84.
C
C --- Version 1.91   Level: 031017  changes
C
C ---     Made a change to TPARIN and TPARIO - Placed ellipsoid
C ---     parameters in locations 14 (major radius) and 15 (eccentricity
C ---     squared). Also forces the first pass initialization of GZTP0
C ---     to use the parameters rather than default to a CLARKE 1866.
C ---     Also fixed a typo so that the USGS WGS 72 ellipsoid model in
C ---     the USGS programs is used.
C
C --- Version 1.93   Level: 041307  changes
C
C --- Made a change to UTM to fix the equator problem (going from southern
C --- to northern hemisphere). Also fixed a problem with NWS-84 and
C --- UTM combination where there is no difference in the results when
C --- going to and from this DATUM from other DATUMS. For UTM the 6371 km
C --- spherical ellipse model must be used when the 6370Km sphere is used
C --- because of USGS program input array conflicts.
C
C --- Version 1.94   Level: 041007  changes
C
C --- Made a change to UTM to fix the equator problem (going from southern
C --- to northern hemisphere) when going from one DATUM to another. This
C --- is a continuation of the change made in version 1.93.
C
C --- Version 1.95   Level: 050126  changes
C
C --- Made it impossible to use a non-USGS earth spheroid when using UTM's
C --- Essentially reversed an attempted fix under version 1.93.
C --- EMG-96 is aliased to GRS 80 ellipsoid model.
C
C---------------
C *** ALERT ***
C---------------
C     - COORDS versions prior to 1.93 used the Clark 1866 spheroid for
C     - UTM conversions when a datum with a non-USGS earth spheroid is
C     - specified. An example of this is the NWS-84 datum.
C     - The UTM/NWS-84 fix implemented in version 1.93 and present in
C     - version 1.94 whould have used a mixture of ESRI and Clarke 1866
C     - owing to the fix being applied only to one side of the
C     - transformation.  One should never mix versions 1.93 and 1.94
C     - with prior versions. ONE SHOULD NOT USE VERSIONS 1.93 and 1.94
C     - owing to the inconsistent nature of the transformation!!!!
C
C --- Added another IRET error code (IRET = 99) for this case. Added an
C --- error string (50 characters) between IRET and DSTAMPIN to the call
C --- to COORDS to return the error message text.
C
C --- Added yet another IRET error code (IRET = 98) for the case when
C --- one tries to use LAZA with a datum that is not a sphere (e.g. not
C --- (NWS-84 or ESR-S).
c
c --- Added VERDOC string to argument list for COORDS identification
c --- text.
c
c --- LAZA Projection:  removed assignment of 6370 km earth radius
c --- (NWS-84 datum) when a value less than 6000 km is found.  This
c --- assignment can override a requested radius of 6371 (ESR-S datum)
c --- if the NWS-84 datum is used with any valid projection prior to the
c --- request for ESR-S.  LAZA(NWS-84) coordinate distances from the
c --- projection origin are about 0.016% smaller than LAZA(ESR-S).
c --- This undoes a change made in version 1.5.
C
C --- Fixed the case for a UTM to UTM transformation when the output UTM
C --- zone is not specified by the user.  The UTM zone is set to the
C --- input UTM zone (or the natural UTM if it estimated) in order that
C --- the proper UTM zone is presented in the output rather than zero.
C --- This fix addresses a situation that arises in the coordinate
C --- conversion GUI.  
C
C --- Version 1.96   Level: 051010  changes
C
C --- Add Albers Conical Equal Area projection as one of the supported
C --- projections.
C
C --- Version 1.98   Level: 060911  changes
C
c --- Changes that allow a higher level of FORTRAN error checking:
c ---    Replace the constant 4 with an I*4 variable (IUNIT4) in
c        calls to GTPZ0 from COORDS (to/from lat-lon).
c ---    Set GTPZ0 argument LENGTH=100 (for direct access files that
c        are not used).
c ---    Replace constant 0 with I*4 variable (INSPHZERO) in argument 1
c        of SPHDZ0 call in GTPZ0
c ---    Change FUNCTION ADJLZ0 argument name and reassign to LON within
c        (sub is called with a computed argument that should not be
c        changed within subroutine)
c ---    SAVE9 is undefined first time in PJINIT; set to zero in DATA

C
C --- Version 1.99   Level: 070921  changes
C
c --- Modify UTM section of PJINIT to fix erroneous non-zero false
c --- Northing when converting S. hemisphere locations to UTM-N
c --- coordinates.  Main subroutine also changed to remove patches
c --- that had corrected this problem when converting from lat/lon to
c --- UTM-N.  The bug only affected conversions to N. hemisphere UTM
c --- coordinates when the location was in the S. hemisphere.  The
c --- coordinates returned were actually in UTM-S.
c
c --- Initialize full real*8 work arrays DWRK, DWRK2, TDUM to zero.
c
c --- Initialize UTMOUT to zero.
C
C----------------------------------------------------------------------
C
C --- PROGRAM FUNCTION:
C
C --- THIS IS THE MAIN DRIVER PROGRAM FOR THE MOLODENSKY DATUM
C --- CONVERSION AND THE USGS GCTP PROJECTION CONVERSION SOFTWARE.
C
C --- INPUT VARIABLES
C
C --- IO = LOGICAL FORTRAN UNIT FOR OUTPUT
C --- IUNIT = 10 CHARACTER UNITS STRING - 'METERS   ' OR 'KILOMETERS'
C --- IMODE = 0 - USES DATA IN BLOCK DATA
C ---         1 - USER DEFINED DATUM INFORMATION (FROM)
C ---         2 - USER DEFINED DATUM INFORMATION (TO)
C ---         3 - USER DEFINED DATUM INFORMATION (FROM-TO)
C --- IPROJ = 12 CHARACTER PROJECTION ACTION STRING EG 'LL2UTM     '
C --- IDATMI = 8 CHARACTER INPUT DATUM ID STRING
C ---          PPP-GGXX WHERE PPP IS THE PRIMARY ID, GG IS THE
C ---          GEOGRAPHIC REGION INDICATOR AND XX ARE PRESENLTY BLANK
C --- IDATMO = 8 CHARACTER OUTPUT DATUM ID STRING
C ---          PPP-GGXX WHERE PPP IS THE PRIMARY ID, GG IS THE
C ---          GEOGRAPHIC REGION INDICATOR AND XX ARE PRESENLTY BLANK
C --- IPREC = 0 - SINGLE PRECISION COORDINATES FOR XYZIN(O),CVECTI(O)
C ---         1 - DOUBLE PRECISION COORDINATES FOR XYZIN(O),CVECTI(O)
C --- CVECTI = 1-D VECTOR OF INPUT PROJECTION PARAMETERS (DP)
C --- CVECTO = 1-D VECTOR OF OUTPUT PROJECTION PARAMETERS (DP)
C --- NVEC = NUMBER OF PARAMETERS IN THE CVECT ARRAYS
C --- XYZIN = 1-D ARRAY OF INPUT COORDINATES (X,Y,Z)  (DP)
C --- NC = NUMBER OF VALID ELEMENTS IN XYZIN(O) (2 OR 3) (X,Y) OR (X,Y,Z)
C --- XDATUM = 1-D VECTOR OF DATUM DEFINITION PARAMETERS
C --- NDAT = NUMBER OF DATUM DEFINITION PARAMETERS (NORMALLY = 6)
C --- DSTAMPIN = 12 CHARACTER DATE STRING (MM-DD-YYYY  ) FOR CHECKING
C ---            NIMA PARAMS AND BLOCKDATA (Leave blank for default)

C
C --- OUTPUT VARIABLES
C
C --- XYZIO = 1-D ARRAY OF OUTPUT COORDINATES (X,Y,Z) (DP)
C --- UTMOUT = UTM ZONE OF THE OUTPUT TO TRANSFORMATION (DP)
C --- IRET = RETURN FLAG (0) - SUCCESSFUL
C --- ESTRNG = 50 CHARACTER STRNG CONTAINING ERROR MESSAGE
C --- VERDOC = 50 character string containing COORDS version and level
C
C --- THIS PROGRAM CALLS:
C
C --- GTPZ0 - USGS GCTP MAIN SUBROUTINE
C --- ERRFLG - ERROR PRINTS FOR GTPZ0
C --- DAT2DAT - MOLODENSKY DATUM SHIFT
C
C --- All NIMA BASED COMMON BLOCKS AND SUPPORTIVE DECLARATIVE
C --- STATEMENTS HAVE BEEN LUMPED INTO A SINGLE INCLUDE FILE
C --- CALLED 'NIMA.CRD'
C
c----------------------------------------------------------------------
C
      PARAMETER (MP = 64)
C
      CHARACTER*128 FN27,FN83
      CHARACTER*50 IERR(12)
      CHARACTER*50 ESTRNG, VERDOC
      CHARACTER*12 JPROJ(MP),IPROJ
      CHARACTER*8 IDATMI,IDATMO
      CHARACTER*7 IPATH1,IPATH2
      CHARACTER*10 IUNIT
      CHARACTER*21 ELIPSI,ELIPSO
      CHARACTER*52 IDSTRNG
      CHARACTER*12 DSTAMPIN
C
      INTEGER*4 INSYS,INZONE,INUNIT,INSPH,IPR,JPR,LEMSG,LPARM,LN27,
     1 LN83,LENGTH,IOSYS,IOZONE,IOUNIT,IFLG

c --- V1.98 (060911)
      INTEGER*4 IUNIT4

      INTEGER*4 SYSFLG(2,MP)
      Integer*4 irnkin,irnkio
      Integer*4 io,lpr, iret
C
      Real*4 xdum,dxshft,dyshft,dzshft
C
      REAL*8 CRDIN(2),TPARIN(15),CRDIO(2),TPARIO(15),DWRK(15),
     1 DWRK2(15)
      REAL*8 XYZIN(NC), XYZIO(NC), CVECTI(NVEC), CVECTO(NVEC)
      REAL*8 TDUM(15),XDATUM(NDAT)
      Real*8 xlonin,xlatin,xlonio,xlatio
      Real*8 flonin,flatin,flonio,flatio
      Real*8 dd,dms,drad,dflt
      Real*8 utmout
      Real*8 TCRDIN(2),TCRDIO(2)
C
C --- Include the NIMA database
      INCLUDE 'nima.crd'
C
      common /xdatm/ drad,dflt,dxshft,dyshft,dzshft
C
C --- DEFAULT CONTROL SETTINGS AND ALLOWED PROJECTIONS
      DATA IPATH1,IPATH2 /'NAD27SP','NAD83SP'/
      DATA LEMSG,LPARM,LN27,LN83 /16,17,18,19/
c      DATA IPR,JPR /0,0/
      DATA IPR,JPR /1,1/
      DATA JPROJ/
     *        'LL2LL      ','LL2UTM     ','LL2LCC     ','LL2LAZA    ',
     *        'LL2PS      ','LL2EM      ','LL2TM      ','LL2ACEA    ',
     *        'UTM2LL     ','UTM2UTM    ','UTM2LCC    ','UTM2LAZA   ',
     *        'UTM2PS     ','UTM2EM     ','UTM2TM     ','UTM2ACEA   ',
     *        'LCC2LL     ','LCC2UTM    ','LCC2LCC    ','LCC2LAZA   ',
     *        'LCC2PS     ','LCC2EM     ','LCC2TM     ','LCC2ACEA   ',
     *        'LAZA2LL    ','LAZA2UTM   ','LAZA2LCC   ','LAZA2LAZA  ',
     *        'LAZA2PS    ','LAZA2EM    ','LAZA2TM    ','LAZA2ACEA  ',
     *        'PS2LL      ','PS2UTM     ','PS2LCC     ','PS2LAZA    ',
     *        'PS2PS      ','PS2EM      ','PS2TM      ','PS2ACEA    ',
     *        'EM2LL      ','EM2UTM     ','EM2LCC     ','EM2LAZA    ',
     *        'EM2PS      ','EM2EM      ','EM2TM      ','EM2ACEA    ',
     *        'TM2LL      ','TM2UTM     ','TM2LCC     ','TM2LAZA    ',
     *        'TM2PS      ','TM2EM      ','TM2TM      ','TM2ACEA    ',
     *        'ACEA2LL    ','ACEA2UTM   ','ACEA2LCC   ','ACEA2LAZA  ',
     *        'ACEA2PS    ','ACEA2EM    ','ACEA2TM    ','ACEA2ACEA  '/
      DATA SYSFLG/0,0,0,1,0,4,0,11,0,6,0,5,0,9,0,3,
     *            1,0,1,1,1,4,1,11,1,6,1,5,1,9,1,3,
     *            4,0,4,1,4,4,4,11,4,6,4,5,4,9,4,3,
     *            11,0,11,1,11,4,11,11,11,6,11,5,11,9,11,3,
     *            6,0,6,1,6,4,6,11,6,6,6,5,6,9,6,3,
     *            5,0,5,1,5,4,5,11,5,6,5,5,5,9,5,3,
     *            9,0,9,1,9,4,9,11,9,6,9,5,9,9,9,3,
     *            3,0,3,1,3,4,3,11,3,6,3,5,3,9,3,3/
      DATA TDUM /15*1.0D0/
C
      FN27(1:7) = IPATH1
      FN83(1:7) = IPATH2
      LPR = IO

c --- V1.98 (060911)
c --- Set units variable for steps with conversion to/from lat-lon
      iunit4=4
c --- Define record-length argument for GTPZ0
      length=100

c----------------------------------------------------------------------
c --- Set the COORDS version and level string

      verdoc=' ---  COORDLIB   Version: 1.99   Level: 070921 '

c----------------------------------------------------------------------

C
C --- SET IRET TO ZERO
      IRET = 0
C
C --- PROPERLY INITIALIZE ESTRNG to BLANKS (NOT NULLS)
      DO K = 1,50
         ESTRNG(K:K) = ' '
      ENDDO
C
C --- SPECIAL CHECK FOR NWS-84 SPHERE JUST IN CASE A LAZA PROJECTION
C --- IS DESIRED.  A SPHERE FLAG IS INITIALIZED HERE (TO ZERO). IT IS
C --- SET TO 1 IF THE ELLIPSOID MODEL IS A SPHERE.
      IBALLI = 0
      IBALLO = 0
      IF(IDATMI.EQ.'NWS-84')IBALLI = 1
      IF(IDATMO.EQ.'NWS-84')IBALLO = 1
C
C --- Establish the date-stamp value
      if(dstampin(1:1).NE.' ') dstamp=dstampin
C
C --- NOW FINDS OUT IF THE USER EXPECTED DATE STRING MATCHES THE
C --- ONE FOUND IN THE NIMA TEXT FILE
      IF(DSTAMP.NE.DATEN)THEN
         IRET = 10
         IERR(1)='DATE STAMP FAILURE FOR NIMA.CRD!                 '
         ESTRNG = IERR(1)
         RETURN
      ENDIF
C
C --- NOW FINDS OUT IF WE HAVE THE RIGHT BLOCK DATA FILE
      IF(DSTAMP.NE.DATEB)THEN
         IRET = 20
         IERR(2)='DATE STAMP FAILURE FOR BLOCKDATA!                '
         ESTRNG = IERR(2)
         RETURN
      ENDIF
C
C --- IMMEDIATELY FINDS THE PROPER DATUM FROM THE PRESTORED SET
      IRNKIN = 0
      IRNKIO = 0
      IF(IMODE.EQ.0)THEN
         DO K = 1,ND
            IF(IDATMI.EQ.DATCOD(K))THEN
               IRNKIN = K
               GO TO 222
            ENDIF
         ENDDO
222      CONTINUE
         DO K = 1,ND
            IF(IDATMO.EQ.DATCOD(K))THEN
               IRNKIO = K
               GO TO 232
            ENDIF
         ENDDO
232      CONTINUE
      ENDIF
      IF(IMODE.EQ.1)THEN
         DO K = 1,ND
            IF(IDATMO.EQ.DATCOD(K))THEN
               IRNKIO = K
               GO TO 332
            ENDIF
         ENDDO
332      CONTINUE
      ENDIF
      IF(IMODE.EQ.2)THEN
         DO K = 1,ND
            IF(IDATMI.EQ.DATCOD(K))THEN
               IRNKIN = K
               GO TO 322
            ENDIF
         ENDDO
322      CONTINUE
      ENDIF
C
C --- IMMEDIATE CHECK FOR ILLEGAL DATUM POINTER
      IF(IMODE.EQ.0)THEN
      IF(IRNKIN.LT.1.OR.IRNKIN.GT.ND)THEN
         IRET = 60
         IERR(6)='INPUT DATUM POINTER IS ILLEGAL!                  '
         ESTRNG = IERR(6)
         RETURN
      ENDIF
      IF(IRNKIO.LT.1.OR.IRNKIO.GT.ND)THEN
         IRET = 70
         IERR(7)='OUTPUT DATUM POINTER IS ILLEGAL!                 '
         ESTRNG = IERR(7)
         RETURN
      ENDIF
      ENDIF
C
C --- CHECKS OPERATION MODE
      IF(IMODE.LT.0.OR.IMODE.GT.3)THEN
         IRET = 30
         IERR(3) = 'THE INPUT OPERATION MODE IS ILLEGAL!              '
         ESTRNG = IERR(3)
      ENDIF
C
C --- NOW ESTABLISHES THE TRANSFORMATION TYPE
      DO K = 1,MP
         IF(JPROJ(K).EQ.IPROJ)THEN
            INSYS = SYSFLG(1,K)
            IOSYS = SYSFLG(2,K)
            GOTO 101
         ENDIF
      ENDDO
      IRET = 40
      IERR(4) = 'THE PROJECTION PAIR IS UNDEFINED OR NOT ALLOWED!  '
      ESTRNG = IERR(4)
      RETURN
 101  CONTINUE
C
C --- NOW CHECKS FOR IMPROPER EASTING AND NORTHING OFFSETS FOR PS AND EM
C --- PROJECTIONS
      IF((INSYS.EQ.5.OR.INSYS.EQ.6).AND.CVECTI(9).NE.0.0D+00)THEN
         IRET = 80
         IERR(8) = 'ILLEGAL INPUT OF (FROM) EASTING/NORTHING OFFSET   '
         ESTRNG = IERR(8)
      ENDIF
      IF((IOSYS.EQ.5.OR.IOSYS.EQ.6).AND.CVECTO(9).NE.0.0D+00)THEN
         IRET = 90
         IERR(9) = 'ILLEGAL INPUT OF (TO) EASTING/NORTHING OFFSET     '
         ESTRNG = IERR(9)
      ENDIF
C
C --- NOW ESTABLISHES THE PROPER UNITS
C --- LL = DECIMAL DEGREES
C --- UTM,LCC,LAZA,PS,MC,ACEA = METERS OR KILOMETERS
      XMULTI = 1.0
      IF(INSYS.EQ.0)THEN
         INUNIT = 4
      ELSE
         INUNIT = 2
         IF(IUNIT.EQ.'KILOMETERS ')THEN
            XMULTI = 1000.0
         ELSE
            XMULTI = 1.0
         ENDIF
      ENDIF
      XMULTO = 1.0
      IF(IOSYS.EQ.0)THEN
         IOUNIT = 4
      ELSE
         IOUNIT = 2
         IF(IUNIT.EQ.'KILOMETERS ')THEN
            XMULTO = 0.001
         ELSE
            XMULTO = 1.0
         ENDIF
      ENDIF
C
C --- SINGLE PRECISION CHECK - SINGLE PRECISION IS NOT YET SUPPORTED
      IF(IPREC.EQ.0)THEN
         IRET = 50
         IERR(5) = 'NICE TRY - SINGLE PRECISION COORDS ARE ILLEGAL!  '
         ESTRNG = IERR(5)
         RETURN
      ENDIF
c
c --- Store the ELon and NLat of the projection origin (DD)
      FLONIN=CVECTI(6)
      FLONIO=CVECTO(6)
      FLATIN=CVECTI(7)
      FLATIO=CVECTO(7)
C
C --- FILLS THE INPUT COORDINATES ARRAY CRDIN AND THE TPARIN ARRAY
      CRDIN(1) = XYZIN(1)*DBLE(XMULTI)
      CRDIN(2) = XYZIN(2)*DBLE(XMULTI)
      IF(NVEC.GT.16)THEN
         IRET = 60
         IERR(6) = 'TRUNCATED PROJECTION PARAMETER VECTOR!           '
         ESTRNG = IERR(6)
         NVEC = 16
      ENDIF
      DO K = 1,15
         TPARIN(K) = 0.0D+00
      ENDDO
      XDUM = SNGL(CVECTI(1))
      INZONE = NINT(XDUM)
      DO K = 2,NVEC
         IF(K.EQ.8 .OR. K.EQ.9) THEN
C ---       SCALE FALSE EASTING/NORTHING TO METERS
            TPARIN(K-1) = CVECTI(K)*DBLE(XMULTI)
         ELSE
C ---       ASSIGN DIRECTLY FROM INPUT VECTOR
            TPARIN(K-1) = CVECTI(K)
         ENDIF
      ENDDO
C
C --- FILLS THE TPARIO ARRAY (ALSO NEEDED)
      DO K = 1,15
         TPARIO(K) = 0.0D+00
      ENDDO
      XDUM = SNGL(CVECTO(1))
      IOZONE = NINT(XDUM)
      DO K = 2,NVEC
         IF(K.EQ.8 .OR. K.EQ.9) THEN
C ---       SCALE FALSE EASTING/NORTHING TO METERS
            TPARIO(K-1) = CVECTO(K)/DBLE(XMULTO)
         ELSE
C ---       ASSIGN DIRECTLY FROM OUTPUT VECTOR
            TPARIO(K-1) = CVECTO(K)
         ENDIF
      ENDDO

c --- Initialize full work arrays
      do k = 1,15
         dwrk(k)  = 0.0D+00
         dwrk2(k) = 0.0D+00
         tdum(k)  = 0.0D+00
      enddo

c --- Initialize output variable UTMOUT
      utmout = 0.0D+00
C
C --- Now converts the TPARIN, TPARIO FROM DD to DDDMMMSSS.SS
C --- UTM's
      IF(INSYS.EQ.1)THEN
         DD = TPARIN(1)
         CALL DD2DMS(DD,DMS)
         TPARIN(1) = DMS
         DD = TPARIN(2)
         CALL DD2DMS(DD,DMS)
         TPARIN(2) = DMS
      ENDIF
C --- LCC's and ACEA's
      IF(INSYS.EQ.4.OR.INSYS.EQ.3)THEN
         DD = TPARIN(3)
         CALL DD2DMS(DD,DMS)
         TPARIN(3) = DMS
         DD = TPARIN(4)
         CALL DD2DMS(DD,DMS)
         TPARIN(4) = DMS
         DD = TPARIN(5)
         CALL DD2DMS(DD,DMS)
         TPARIN(5) = DMS
         DD = TPARIN(6)
         CALL DD2DMS(DD,DMS)
         TPARIN(6) = DMS
      ENDIF
C --- EM & PS's (Note shift of arguments)
      IF(INSYS.EQ.5.OR.INSYS.EQ.6)THEN
         DD = TPARIN(5)
         CALL DD2DMS(DD,DMS)
         TPARIN(5) = DMS
         DD = TPARIN(3)
         CALL DD2DMS(DD,DMS)
         TPARIN(6) = DMS
         TPARIN(3) = 0.0D0
      ENDIF
C --- TRANSVERSE MERCATOR (TM)
      IF(INSYS.EQ.9)THEN
         DD = TPARIN(5)
         CALL DD2DMS(DD,DMS)
         TPARIN(5) = DMS
         DD = TPARIN(6)
         CALL DD2DMS(DD,DMS)
         TPARIN(6) = DMS
C --- NOW SWAP FROM CVECTI ELEMENT 1 TO USGS ELEMENT 3
         TPARIN(3) = CVECTI(1)
         INZONE = 999
      ENDIF
C --- LAZA's
      IF(INSYS.EQ.11)THEN
C --- MAKES SURE A LEGAL SPHERE RADIUS IS PRESENT
C         IF(TPARIN(1).LT.6000000.0D+00)THEN
C            TPARIN(1) = 6370000.0D+00
C         ENDIF
         DD = TPARIN(5)
         CALL DD2DMS(DD,DMS)
         TPARIN(5) = DMS
         DD = TPARIN(6)
         CALL DD2DMS(DD,DMS)
         TPARIN(6) = DMS
      ENDIF
C --- UTM's
      IF(IOSYS.EQ.1)THEN
         DD = TPARIO(1)
         CALL DD2DMS(DD,DMS)
         TPARIO(1) = DMS
         DD = TPARIO(2)
         CALL DD2DMS(DD,DMS)
         TPARIO(2) = DMS
      ENDIF
C --- LCC's and ACEA's
      IF(IOSYS.EQ.4.OR.IOSYS.EQ.3)THEN
         DD = TPARIO(3)
         CALL DD2DMS(DD,DMS)
         TPARIO(3) = DMS
         DD = TPARIO(4)
         CALL DD2DMS(DD,DMS)
         TPARIO(4) = DMS
         DD = TPARIO(5)
         CALL DD2DMS(DD,DMS)
         TPARIO(5) = DMS
         DD = TPARIO(6)
         CALL DD2DMS(DD,DMS)
         TPARIO(6) = DMS
      ENDIF
C --- EM AND PS's (Note shift of arguments)
      IF(IOSYS.EQ.5.OR.IOSYS.EQ.6)THEN
         DD = TPARIO(5)
         CALL DD2DMS(DD,DMS)
         TPARIO(5) = DMS
         DD = TPARIO(3)
         CALL DD2DMS(DD,DMS)
         TPARIO(6) = DMS
         TPARIO(3) = 0.0D0
      ENDIF
C --- TRANSVERSE MERCATOR (TM)
      IF(IOSYS.EQ.9)THEN
         DD = TPARIO(5)
         CALL DD2DMS(DD,DMS)
         TPARIO(5) = DMS
         DD = TPARIO(6)
         CALL DD2DMS(DD,DMS)
         TPARIO(6) = DMS
C --- NOW SWAP FROM CVECTO ELEMENT 1 TO USGS ELEMENT 3
         TPARIO(3) = CVECTO(1)
         IOZONE = 999
      ENDIF
C --- LAZA's
      IF(IOSYS.EQ.11)THEN
C --- MAKES SURE A LEGAL SPHERE RADIUS IS PRESENT
C         IF(TPARIO(1).LT.6000000.0D+00)THEN
C            TPARIO(1) = 6370000.0D+00
C         ENDIF
         DD = TPARIO(5)
         CALL DD2DMS(DD,DMS)
         TPARIO(5) = DMS
         DD = TPARIO(6)
         CALL DD2DMS(DD,DMS)
         TPARIO(6) = DMS
      ENDIF
C
C --- NOW ESTABLISHES THE PROPER ELLIPSOID MODEL PARAMETERS
      IF(IMODE.EQ.0.OR.IMODE.EQ.2)THEN
         IDSTRNG = DATUM(DATTYP(IRNKIN))
         ELIPSI = IDSTRNG(32:52)
         INSPH = -1
c
c --- Special alias for EMG 96
         if(elipsi.eq.'EMG 96               ')INSPH = 8
         IF(ELIPSI.EQ.'Clarke 1866          ')INSPH = 0
         IF(ELIPSI.EQ.'Clarke 1880          ')INSPH = 1
         IF(ELIPSI.EQ.'Bessel 1841          ')INSPH = 2
         IF(ELIPSI.EQ.'International 1967   ')INSPH = 3
         IF(ELIPSI.EQ.'International 1909   ')INSPH = 4
         IF(ELIPSI.EQ.'WGS 72               ')INSPH = 5
         IF(ELIPSI.EQ.'Everest (1830)       ')INSPH = 6
         IF(ELIPSI.EQ.'WGS 66               ')INSPH = 7
         IF(ELIPSI.EQ.'GRS 80               ')INSPH = 8
         IF(ELIPSI.EQ.'Airy                 ')INSPH = 9
         IF(ELIPSI.EQ.'Everest (1956)       ')INSPH = 10
         IF(ELIPSI.EQ.'Modified Airy        ')INSPH = 11
         IF(ELIPSI.EQ.'WGS 84               ')INSPH = 12
         IF(ELIPSI.EQ.'Modified Fischer 1960')INSPH = 13
         IF(ELIPSI.EQ.'Australian National  ')INSPH = 14
         IF(ELIPSI.EQ.'Krassovsky 1940      ')INSPH = 15
         IF(ELIPSI.EQ.'Hough                ')INSPH = 16
         IF(ELIPSI.EQ.'Mercury 1960         ')INSPH = 17
         IF(ELIPSI.EQ.'Modified Mercury 1968')INSPH = 18
         IF(ELIPSI.EQ.'Normal Sphere (6371) ')INSPH = 19
         IF(ELIPSI.EQ.'International 1924   ')INSPH = 20
      ENDIF
C
C --- DOES NOT ALLOW UTM WITHOUT USGS SPHEROID MODEL TO
C --- BE USED (IRET ERROR CODE OF 99 IS GIVEN). PRESENTLY
C --- NWS-84 DATUM FITS THIS CONDITION AS DOES A NUMBER OF
C --- OTHER EXOTICS.
C      IJSYS = 0
C      IF(INSYS.EQ.1.OR.IOSYS.EQ.1)IJSYS = 1
      IF(INSPH.LT.0.AND.INSYS.EQ.1)THEN
          IRET = 99
          write(IERR(11),'(a26,a8)')'CANNOT USE UTM WITH DATUM ',
     &                               idatmi
c         IERR(11) = 'CANNOT USE UTM WITH NON-USGS SPHERE'
          ESTRNG = IERR(11)
          RETURN
      ENDIF
C
C --- DOES NOT ALLOW LAZA TO BE USED WITH A NON-SPHERE SPHEROID
C --- (IRET ERROR CODE OF 98 IS GIVEN)
      IF(INSPH.EQ.19)IBALLI = 1
      IF(INSYS.EQ.11.AND.IBALLI.NE.1)THEN
          IRET = 98
          write(IERR(12),'(a27,a8)')'CANNOT USE LAZA WITH DATUM ',
     &                               idatmi
c         IERR(12) = 'CANNOT USE LAZA WITH NON-SPHERE'
          ESTRNG = IERR(12)
          RETURN
      ENDIF
      IF(IMODE.EQ.0.OR.IMODE.EQ.1)THEN
         IDSTRNG = DATUM(DATTYP(IRNKIO))
         ELIPSO = IDSTRNG(32:52)
         IOSPH = -1
c
c --- Special alias for EMG 96
         if(elipso.eq.'EMG 96               ')IOSPH = 8
         IF(ELIPSO.EQ.'Clarke 1866          ')IOSPH = 0
         IF(ELIPSO.EQ.'Clarke 1880          ')IOSPH = 1
         IF(ELIPSO.EQ.'Bessel 1841          ')IOSPH = 2
         IF(ELIPSO.EQ.'International 1967   ')IOSPH = 3
         IF(ELIPSO.EQ.'International 1909   ')IOSPH = 4
         IF(ELIPSO.EQ.'WGS 72               ')IOSPH = 5
         IF(ELIPSO.EQ.'Everest (1830)       ')IOSPH = 6
         IF(ELIPSO.EQ.'WGS 66               ')IOSPH = 7
         IF(ELIPSO.EQ.'GRS 80               ')IOSPH = 8
         IF(ELIPSO.EQ.'Airy                 ')IOSPH = 9
         IF(ELIPSO.EQ.'Everest (1956)       ')IOSPH = 10
         IF(ELIPSO.EQ.'Modified Airy        ')IOSPH = 11
         IF(ELIPSO.EQ.'WGS 84               ')IOSPH = 12
         IF(ELIPSO.EQ.'Modified Fischer 1960')IOSPH = 13
         IF(ELIPSO.EQ.'Australian National  ')IOSPH = 14
         IF(ELIPSO.EQ.'Krassovsky 1940      ')IOSPH = 15
         IF(ELIPSO.EQ.'Hough                ')IOSPH = 16
         IF(ELIPSO.EQ.'Mercury 1960         ')IOSPH = 17
         IF(ELIPSO.EQ.'Modified Mercury 1968')IOSPH = 18
         IF(ELIPSO.EQ.'Normal Sphere (6371) ')IOSPH = 19
         IF(ELIPSO.EQ.'International 1924   ')IOSPH = 20
      ENDIF
C
C --- DOES NOT ALLOW UTM WITHOUT USGS SPHEROID MODEL TO
C --- BE USED (IRET ERROR CODE OF 99 IS GIVEN). PRESENTLY
C --- NWS-84 DATUM FITS THIS CONDITION AS DOES A NUMBER OF
C --- OTHER EXOTICS.
C      IJSYS = 0
C      IF(INSYS.EQ.1.OR.IOSYS.EQ.1)IJSYS = 1
      IF(IOSPH.LT.0.AND.IOSYS.EQ.1)THEN
          IRET = 99
          write(IERR(11),'(a26,a8)')'CANNOT USE UTM WITH DATUM ',
     &                               idatmo
c          IERR(11) = 'CANNOT USE UTM WITH NON-USGS SPHERE'
          ESTRNG = IERR(11)
          RETURN
      ENDIF
C
C --- DOES NOT ALLOW LAZA TO BE USED WITH A NON-SPHERE SPHEROID
C --- (IRET ERROR CODE OF 98 IS GIVEN)
      IF(IOSPH.EQ.19)IBALLO = 1
      IF(IOSYS.EQ.11.AND.IBALLO.NE.1)THEN
          IRET = 98
          write(IERR(12),'(a27,a8)')'CANNOT USE LAZA WITH DATUM ',
     &                               idatmo
c         IERR(12) = 'CANNOT USE LAZA WITH NON-SPHERE'
          ESTRNG = IERR(12)
          RETURN
      ENDIF
C
C --- STICKS THE ELLIPSOID PARAMETERS INTO ELEMENTS 1,2 OF
C --- TPARIN, TPARIO
      IF(INSPH.LT.0.AND.IMODE.EQ.0)THEN
C      IF(IMODE.EQ.0)THEN
         TPARIN(1) = DRADIM(IRNKIN)
         TPARIN(2) = DEC2(IRNKIN)
      ENDIF
      IF(IOSPH.LT.0.AND.IMODE.EQ.0)THEN
C      IF(IMODE.EQ.0)THEN
         TPARIO(1) = DRADIM(IRNKIO)
         TPARIO(2) = DEC2(IRNKIO)
      ENDIF
C
C --- SPECIAL SET FOR ELLIPSOID PARAMETERS IN TPARIN AND TPARIO ELEMENTS 14,15
      TPARIN(14) = DRADIM(IRNKIN)
      TPARIN(15) = DEC2(IRNKIN)
      TPARIO(14) = DRADIM(IRNKIO)
      TPARIO(15) = DEC2(IRNKIO)
C
C--------------------------------------------------------------------
C --- CRDIN = COORDINATES IN INPUT SYSTEM (2 DP WORDS ARRAY).
C --- INSYS = CODE NUMBER OF INPUT COORDINATE SYSTEM (INTEGER).
C            =  0 , GEOGRAPHIC
C            =  1 , U T M
C            =  2 , STATE PLANE
C            =  3 , ALBERS CONICAL EQUAL-AREA
C            =  4 , LAMBERT CONFORMAL CONIC
C            =  5 , MERCATOR
C            =  6 , POLAR STEREOGRAPHIC
C            =  7 , POLYCONIC
C            =  8 , EQUIDISTANT CONIC
C            =  9 , TRANSVERSE MERCATOR
C            = 10 , STEREOGRAPHIC
C            = 11 , LAMBERT AZIMUTHAL EQUAL-AREA
C            = 12 , AZIMUTHAL EQUIDISTANT
C            = 13 , GNOMONIC
C            = 14 , ORTHOGRAPHIC
C            = 15 , GENERAL VERTICAL NEAR-SIDE PERSPECTIVE
C            = 16 , SINUSOIDAL
C            = 17 , EQUIRECTANGULAR (PLATE CARREE)
C            = 18 , MILLER CYLINDRICAL
C            = 19 , VAN DER GRINTEN I
C            = 20 , OBLIQUE MERCATOR (HOTINE)
C            = 21 , ROBINSON
C            = 22 , SPACE OBLIQUE MERCATOR
C            = 23 , MODIFIED-STEREOGRAPHIC CONFORMAL (ALASKA)
C --- INZONE = CODE NUMBER OF INPUT COORDINATE ZONE (INTEGER).
C --- TPARIN = PARAMETERS OF INPUT REFERENCE SYSTEM (15 DP WORDS ARRAY).
C --- INUNIT = CODE NUMBER OF UNITS OF MEASURE FOR INPUT COORDINATES (I*
C            = 0 , RADIANS.
C            = 1 , U.S. FEET.
C            = 2 , METERS.
C            = 3 , SECONDS OF ARC.
C            = 4 , DEGREES OF ARC.
C            = 5 , INTERNATIONAL FEET.
C            = 6 , USE LEGISLATED DISTANCE UNITS FROM NADUT TABLE
C
C --- INSPH  = INPUT SPHEROID CODE.  SEE SPHDZ0 FOR PROPER CODES.
C --- 0 = CLARKE 1866           1 = CLARKE 1880
C --- 2 = BESSEL                3 = NEW INTERNATIONAL 1967
C --- 4 = INTERNATIONAL 1909    5 = WGS 72
C --- 6 = EVEREST               7 = WGS 66
C --- 8 = GRS 1980              9 = AIRY
C --- 10 = MODIFIED EVEREST     11 = MODIFIED AIRY
C --- 12 = WGS 84               13 = SOUTHEAST ASIA
C --- 14 = AUSTRALIAN NATIONAL  15 = KRASSOVSKY
C --- 16 = HOUGH                17 = MERCURY 1960
C --- 18 = MODIFIED MERC 1968   19 = SPHERE OF RADIUS 6370997 M
C --- 20 = INTERNATIONAL 1924
C
C --- IPR    = PRINTOUT FLAG FOR ERROR MESSAGES. 0=YES, 1=NO
C --- JPR    = PRINTOUT FLAG FOR PROJECTION PARAMETERS 0=YES, 1=NO
C --- LEMSG  = LOGICAL UNIT FOR LISTING ERROR MESSAGES IF IPR = 0
C --- LPARM  = LOGICAL UNIT FOR LISTING PROJECTION PARAMETERS IF JPR = 0
C --- LN27   = LOGICAL UNIT FOR NAD 1927 SPCS PARAMETER FILE
C --- FN27   = FILE NAME OF NAD 1927 SPCS PARAMETERS
C --- LN83   = LOGICAL UNIT FOR NAD 1983 SPCS PARAMETER FILE
C --- FN83   = FILE NAME OF NAD 1983 SPCS PARAMETERS
C --- LENGTH = RECORD LENGTH OF NAD1927 AND NAD1983 PARAMETER FILES
C
C---------------------------------------------------------------------
C
C --- SETS IN NEW DATUM PARAMETERS AND CHECK FOR BAD MODE FLAG
      IF(IMODE.EQ.1)THEN
         INSPH = -1
         TPARIN(1) = XDATUM(1)
         TPARIN(2) = XDATUM(3)
         IRNKIN = 9999
         DRAD = XDATUM(1)
         DFLT = XDATUM(2)
         DXSHFT = SNGL(XDATUM(4))
         DYSHFT = SNGL(XDATUM(5))
         DZSHFT = SNGL(XDATUM(6))
      ENDIF
      IF(IMODE.EQ.2)THEN
         IOSPH = -1
         TPARIO(1) = XDATUM(1)
         TPARIO(2) = XDATUM(3)
         IRNKIO = 9999
         DRAD = XDATUM(1)
         DFLT = XDATUM(2)
         DXSHFT = SNGL(XDATUM(4))
         DYSHFT = SNGL(XDATUM(5))
         DZSHFT = SNGL(XDATUM(6))
      ENDIF
      IF(IMODE.EQ.3)THEN
         INSPH = -1
         TPARIN(1) = XDATUM(1)
         TPARIN(2) = XDATUM(3)
         IRNKIN = 9999
         IOSPH = -1
         TPARIO(1) = XDATUM(1)
         TPARIO(2) = XDATUM(3)
         IRNKIO = 9999
         DRAD = XDATUM(1)
         DFLT = XDATUM(2)
         DXSHFT = SNGL(XDATUM(4))
         DYSHFT = SNGL(XDATUM(5))
         DZSHFT = SNGL(XDATUM(6))
      ENDIF
      IF(IMODE.LT.0.OR.IMODE.GT.3)THEN
         IRET = 30
         IERR(3) = 'THE INPUT OPERATION MODE IS ILLEGAL!              '
         ESTRNG = IERR(3)
      ENDIF
C
C**********************************************************************
C
C --- Now converts TLAT1 for EM,PS to LATITUDE OF TRUE SCALE
C --- and takes the Latitude of origin of projection and changes
C --- it to a false northing
C
C**********************************************************************
C
C --- (FROM) INPUT DATUM SIDE - POLAR STEREOGRAPHIC + EQUATORIAL MERCATOR
      IF(INSYS.EQ.6.OR.INSYS.EQ.5)THEN
C
C --- SET COORDINATE ORIGIN AS THE PS POINT DESIRED
         TCRDIN(1) = FLONIN
         TCRDIN(2) = FLATIN
C
C --- CREATE A DUMMY WORKING PROJECTION VECTOR (DWRK2) FOR
C --- CONVERTING TO PS/EM
         DO KK = 1,NVEC
            DWRK2(KK) = TPARIN(KK)
         ENDDO
C
C --- CLEAN TEMPORARY OUTPUT ARRAY FOR FALSE EASTING, NORTHING AND
C --- SET PROPER UNITS FOR A LL2PS/EM TRANSFORMATION
         TCRDIO(1) = 0.0D0
         TCRDIO(2) = 0.0D0
         JNUNIT = 4
         JOUNIT = 2
C
C --- DOES CALL FOR THE FALSE EASTING AND NORTHING TO BE ADDED TO THE
C --- PROJECTION
         CALL GTPZ0(TCRDIN,0,0,TDUM,JNUNIT,INSPH,IPR,
     .              JPR,LEMSG,LPARM,TCRDIO,INSYS,INZONE,DWRK2,JOUNIT,
     .              LN27,LN83,FN27,FN83,LENGTH,IFLG)
         CALL ERRPRT(IFLG,LPR,1)
C
C --- ERROR PROCESSING
         IF(IFLG.NE.0)THEN
            IRET = IRET + IFLG
            RETURN
         ENDIF
C
C --- NOW SHIFTS THE INPUT COORDS FROM DOMAIN CENTER TO THE POLE
C --- ASSUMES SINCE ONE IS NOT PUTTING IN OFFSETS THAT THE DATA
C --- COMING IN IS ALREADY OFFSET AND MUST BE SET TO THE POLE
         CRDIN(1) = CRDIN(1) + TCRDIO(1)
         CRDIN(2) = CRDIN(2) + TCRDIO(2)
      ENDIF
C**********************************************************************
C
C --- OUTPUT (TO) DATUM SIDE - POLAR STEREOGRAPHIC + EQUATORIAL MERCATOR
      IF(IOSYS.EQ.6.OR.IOSYS.EQ.5)THEN
C
C --- SET DOMAIN CENTER AS THE PS POINT DESIRED
         TCRDIN(1) = FLONIO
         TCRDIN(2) = FLATIO
C
C --- CREATE A DUMMY WORKING PROJECTION VECTOR (DWRK2) FOR
C --- CONVERTING TO PS/EM
         DO KK = 1,NVEC
            DWRK2(KK) = TPARIO(KK)
         ENDDO
C
C --- CLEAN TEMPORARY OUTPUT ARRAY FOR FALSE EASTING, NORTHING AND
C --- SET PROPER UNITS FOR A LL2PS/EM TRANSFORMATION
         TCRDIO(1) = 0.0D0
         TCRDIO(2) = 0.0D0
         JNUNIT = 4
         JOUNIT = 2
C
C --- DOES CALL FOR THE FALSE EASTING AND NORTHING TO BE SUBTRACTED
C --- FROM THE PROJECTION
         CALL GTPZ0(TCRDIN,0,0,TDUM,JNUNIT,IOSPH,IPR,
     .              JPR,LEMSG,LPARM,TCRDIO,IOSYS,IOZONE,DWRK2,JOUNIT,
     .              LN27,LN83,FN27,FN83,LENGTH,IFLG)
C
C --- ERROR PROCESSING
         CALL ERRPRT(IFLG,LPR,1)
         IF(IFLG.NE.0)THEN
            IRET = IRET + IFLG
            RETURN
         ENDIF
      ENDIF
C**********************************************************************
C
C --- DOES A COMPLETE CYCLE PROJ/DATUM/PROJ
      IF(IRNKIN.NE.IRNKIO.AND.INSYS.NE.0.AND.IOSYS.NE.0)THEN
C
C --- STEP 1 PROJECTION TO LAT-LON
         IOVUTM = 0
         IF(IABS(IOZONE).GT.0.AND.IABS(IOZONE).LT.61)IOVUTM = 1
         IF(IOZONE.NE.INZONE.AND.IOVUTM.EQ.1)IOVUTM = 2
C
c --- V1.98 (060911)
c         CALL GTPZ0(CRDIN,INSYS,INZONE,TPARIN,INUNIT,INSPH,IPR,JPR,
c     .        LEMSG,LPARM,CRDIO,0,0,TDUM,4,LN27,LN83,
c     .        FN27,FN83,LENGTH,IFLG)
         CALL GTPZ0(CRDIN,INSYS,INZONE,TPARIN,INUNIT,INSPH,IPR,JPR,
     .        LEMSG,LPARM,CRDIO,0,0,TDUM,IUNIT4,LN27,LN83,
     .        FN27,FN83,LENGTH,IFLG)
         CALL ERRPRT(IFLG,LPR,1)
         IF(IFLG.NE.0)RETURN
C
C --- STEP 2 DATUM TRANSFORMATION
         XLONIN = CRDIO(1)
         XLATIN = CRDIO(2)
         ZLEVIN = SNGL(XYZIN(3))
         CALL DAT2DAT(LPR,IPR,XLONIN,XLATIN,ZLEVIN,IRNKIN,
     1    IRNKIO,XLONIO,XLATIO,ZLEVIO)
         CRDIN(1) = XLONIO
         CRDIN(2) = XLATIO
         XYZIO(3) = DBLE(ZLEVIO)
C
C --- GETS THE TO UTM ZONE
         IF(IOSYS.EQ.1.AND.IOVUTM.EQ.0)THEN
            CALL LL2ZON(XLONIO,XLATIO,IOZONE,IRET)
         ENDIF
C
C --- STEP 3 PROJECTION FROM LAT-LON
c --- V1.98 (060911)
c         CALL GTPZ0(CRDIN,0,0,TDUM,4,IOSPH,IPR,JPR,
c     .        LEMSG,LPARM,CRDIO,IOSYS,IOZONE,TPARIO,IOUNIT,LN27,LN83,
c     .        FN27,FN83,LENGTH,IFLG)
         CALL GTPZ0(CRDIN,0,0,TDUM,IUNIT4,IOSPH,IPR,JPR,
     .        LEMSG,LPARM,CRDIO,IOSYS,IOZONE,TPARIO,IOUNIT,LN27,LN83,
     .        FN27,FN83,LENGTH,IFLG)
         CALL ERRPRT(IFLG,LPR,2)
         IF(IFLG.NE.0)RETURN
         UTMOUT = DBLE(IOZONE)
      ENDIF
C**********************************************************************
C
C --- DOES ONLY A DATUM SHIFT
      IF(INSYS.EQ.0.AND.IOSYS.EQ.0)THEN
         XLONIN = CRDIN(1)
         XLATIN = CRDIN(2)
         ZLEVIN = SNGL(XYZIN(3))
         CALL DAT2DAT(LPR,IPR,XLONIN,XLATIN,ZLEVIN,IRNKIN,
     1    IRNKIO,XLONIO,XLATIO,ZLEVIO)
         CRDIO(1) = XLONIO
         CRDIO(2) = XLATIO
         XYZIO(3) = DBLE(ZLEVIO)
         UTMOUT = DBLE(INZONE)
      ENDIF
C**********************************************************************
C
C --- DOES A PARTIAL CYCLE - FROM PROJ/DATUM TO LL (GEODETIC)
      IF(IRNKIN.NE.IRNKIO.AND.INSYS.NE.0.AND.IOSYS.EQ.0)THEN
C
C --- STEP 1 PROJECTION TO LAT-LON
c --- V1.98 (060911)
c         CALL GTPZ0(CRDIN,INSYS,INZONE,TPARIN,INUNIT,INSPH,IPR,JPR,
c     .        LEMSG,LPARM,CRDIO,0,0,TDUM,4,LN27,LN83,
c     .        FN27,FN83,LENGTH,IFLG)
         CALL GTPZ0(CRDIN,INSYS,INZONE,TPARIN,INUNIT,INSPH,IPR,JPR,
     .        LEMSG,LPARM,CRDIO,0,0,TDUM,IUNIT4,LN27,LN83,
     .        FN27,FN83,LENGTH,IFLG)
         CALL ERRPRT(IFLG,LPR,1)
         IF(IFLG.NE.0)RETURN
C
C --- STEP 2 DATUM TRANSFORMATION
         XLONIN = CRDIO(1)
         XLATIN = CRDIO(2)
         ZLEVIN = SNGL(XYZIN(3))
         CALL DAT2DAT(LPR,IPR,XLONIN,XLATIN,ZLEVIN,IRNKIN,
     1    IRNKIO,XLONIO,XLATIO,ZLEVIO)
         CRDIO(1) = XLONIO
         CRDIO(2) = XLATIO
         XYZIO(3) = DBLE(ZLEVIO)
      ENDIF
C**********************************************************************
C
C --- DOES A PARTIAL CYCLE FROM LL (GEODETIC) TO DATUM/PROJ
      IF(IRNKIN.NE.IRNKIO.AND.INSYS.EQ.0.AND.IOSYS.NE.0)THEN
C
C --- STEP 1 DATUM TRANSFORMATION
         XLONIN = CRDIN(1)
         XLATIN = CRDIN(2)
         ZLEVIN = SNGL(XYZIN(3))
         CALL DAT2DAT(LPR,IPR,XLONIN,XLATIN,ZLEVIN,IRNKIN,
     1    IRNKIO,XLONIO,XLATIO,ZLEVIO)
         CRDIN(1) = XLONIO
         CRDIN(2) = XLATIO
         XYZIO(3) = DBLE(ZLEVIO)
C
C --- GETS THE TO UTM ZONE
         IF(IOSYS.EQ.1.AND.IABS(IOZONE).GT.60)THEN
            CALL LL2ZON(XLONIO,XLATIO,IOZONE,IRET)
         ENDIF
C
C --- STEP 2 PROJECTION FROM LAT-LON
         CALL GTPZ0(CRDIN,0,INZONE,TPARIN,INUNIT,IOSPH,IPR,JPR,
     .        LEMSG,LPARM,CRDIO,IOSYS,IOZONE,TPARIO,IOUNIT,LN27,LN83,
     .        FN27,FN83,LENGTH,IFLG)
         CALL ERRPRT(IFLG,LPR,1)

c --- Fix moved into PJINIT (070921)
cC --- SPECIAL FIX FOR NH CROSS OVER OF ZONE [IOZONE > 0 crdin(2) <0.0]
c            IF(INSYS.EQ.0.AND.IOSYS.EQ.1.AND.IOZONE.GT.0.AND.CRDIN(2).
c     1       LT.0.0)THEN
c                CRDIO(2) = CRDIO(2)-10000000.0
c            ENDIF

         IF(IFLG.NE.0)RETURN
         UTMOUT = DBLE(IOZONE)
      ENDIF
C**********************************************************************
C
C --- DOES A PARTIAL CYCLE - PROJ ONLY - NO DATUM CHANGE
      IF(IRNKIN.EQ.IRNKIO)THEN
C
C --- GOES TO LL (GEODETIC IF IOSYS = 1) TO GET UTM ZONE FOR OUTPUT
         IF(IOSYS.EQ.1)THEN
            IF(INSYS.NE.0)THEN
               DO KK = 1,NVEC
                  DWRK(KK) = 0.0D0
                  DWRK2(KK) = TPARIN(KK)
               ENDDO
               CRDIO(1) = 0.0D0
               CRDIO(2) = 0.0D0
               IDUM = INZONE
               JDUM = IOZONE
               JOUNIT = 4
               JOSYS = 0
               CALL GTPZ0(CRDIN,INSYS,IDUM,DWRK2,INUNIT,INSPH,IPR,
     .              JPR,LEMSG,LPARM,CRDIO,JOSYS,JDUM,DWRK,JOUNIT,LN27,
     .              LN83,FN27,FN83,LENGTH,IFLG)
               CALL ERRPRT(IFLG,LPR,1)
               IF(IFLG.NE.0)THEN
                  IRET = IRET + IFLG
                  RETURN
               ENDIF
               XLONIO = CRDIO(1)
               XLATIO = CRDIO(2)
            ELSE
               XLONIO = CRDIN(1)
               XLATIO = CRDIN(2)
            ENDIF
C
C --- DETERMINE IF A VALID OUTPUT ZONE IS GIVEN
            IOVUTM = 0
            IF(IABS(IOZONE).GT.0.AND.IABS(IOZONE).LT.61)IOVUTM = 1
            IF(IOZONE.NE.INZONE.AND.IOVUTM.EQ.1)IOVUTM = 2
C
C --- MAKE SURE WE GET A DECENT ZONE IF WE ENTERED A BOGUS ONE INITIALLY
            IF(IOVUTM.EQ.0)THEN
               CALL LL2ZON(XLONIO,XLATIO,IOZONE,IRET)
            ENDIF
c            PRINT *,'HEY - LATITUDE - UTM OUT: ',XLATIO,IOZONE
         ENDIF
C
C --- SPECIAL CASE UTM2UTM WHERE OVERRIDE IS DESIRED
         IF(INSYS.EQ.1.AND.IOSYS.EQ.1)THEN
            CRDIN(1) = CRDIO(1)
            CRDIN(2) = CRDIO(2)
            JNUNIT = 4
            JNSYS = 0
            IF(IOVUTM.EQ.0)THEN
               CALL GTPZ0(CRDIN,JNSYS,IDUM,DWRK,JNUNIT,INSPH,IPR,JPR,
     1          LEMSG,LPARM,CRDIO,IOSYS,IOZONE,TPARIO,IOUNIT,LN27,LN83,
     2          FN27,FN83,LENGTH,IFLG)
               CALL ERRPRT(IFLG,LPR,1)
            ELSE
               IF(IOVUTM.EQ.2)THEN
                  CALL GTPZ0(CRDIN,JNSYS,IDUM,DWRK,JNUNIT,INSPH,IPR,
     1             JPR,LEMSG,LPARM,CRDIO,IOSYS,IOZONE,TPARIO,IOUNIT,
     2             LN27,LN83,FN27,FN83,LENGTH,IFLG)
                  CALL ERRPRT(IFLG,LPR,1)
               ELSE
C
C --- DO NOTHING EXCEPT UNITS CHANGE
                  XYZIO(1) = XYZIN(1)
                  XYZIO(2) = XYZIN(2)
                  RETURN
               ENDIF
            ENDIF

C
C --- SPECIAL CASE WHERE INZONE IS PROVIDED BUT IOZONE IS NOT
            IF(IABS(INZONE).GT.0.AND.IABS(INZONE).LT.61)THEN
               IOZONE = INZONE
            ENDIF
            UTMOUT = DBLE(IOZONE)
         ELSE
C
C --- REGULAR CASES
         IF(INSYS.NE.IOSYS)THEN
            CALL GTPZ0(CRDIN,INSYS,INZONE,TPARIN,INUNIT,INSPH,IPR,JPR,
     .        LEMSG,LPARM,CRDIO,IOSYS,IOZONE,TPARIO,IOUNIT,LN27,LN83,
     .        FN27,FN83,LENGTH,IFLG)
              CALL ERRPRT(IFLG,LPR,1)

c --- Fix moved into PJINIT (070921)
cC --- SPECIAL FIX FOR NH CROSS OVER OF ZONE [IOZONE > 0 crdin(2) <0.0]
c            IF(INSYS.EQ.0.AND.IOSYS.EQ.1.AND.IOZONE.GT.0.AND.CRDIN(2).
c     1       LT.0.0)THEN
c                CRDIO(2) = CRDIO(2)-10000000.0
c            ENDIF

            ELSE    ! CASE FROM ONE PROJECTION SETTING TO ANOTHER
C
C --- STEP 1 PROJECTION TO LAT-LON
C
c --- V1.98 (060911)
c               CALL GTPZ0(CRDIN,INSYS,INZONE,TPARIN,INUNIT,INSPH,IPR,
c     1          JPR,LEMSG,LPARM,CRDIO,0,IOZONE,TDUM,4,LN27,LN83,
c     2          FN27,FN83,LENGTH,IFLG)
               CALL GTPZ0(CRDIN,INSYS,INZONE,TPARIN,INUNIT,INSPH,IPR,
     1          JPR,LEMSG,LPARM,CRDIO,0,IOZONE,TDUM,IUNIT4,LN27,LN83,
     2          FN27,FN83,LENGTH,IFLG)
               CALL ERRPRT(IFLG,LPR,1)
               IF(IFLG.NE.0)RETURN
C
C --- STEP 2 FEED CHANGE BACK TO PROJECTION
               XLONIN = CRDIO(1)
               XLATIN = CRDIO(2)
               CRDIN(1) = XLONIN
               CRDIN(2) = XLATIN
C
C --- STEP 3 PROJECTION FROM LAT-LON
c --- V1.98 (060911)
c               CALL GTPZ0(CRDIN,0,IOZONE,TDUM,4,IOSPH,IPR,JPR,
c     1          LEMSG,LPARM,CRDIO,IOSYS,IOZONE,TPARIO,IOUNIT,LN27,LN83,
c     2          FN27,FN83,LENGTH,IFLG)
               CALL GTPZ0(CRDIN,0,IOZONE,TDUM,IUNIT4,IOSPH,IPR,JPR,
     1          LEMSG,LPARM,CRDIO,IOSYS,IOZONE,TPARIO,IOUNIT,LN27,LN83,
     2          FN27,FN83,LENGTH,IFLG)
               CALL ERRPRT(IFLG,LPR,2)
               IF(IFLG.NE.0)RETURN
            ENDIF
            CALL ERRPRT(IFLG,LPR,1)
            IF(IFLG.NE.0)THEN
               IRET = IRET + IFLG
               RETURN
            ENDIF
            XYZIO(3) = XYZIN(3)
            UTMOUT = DBLE(IOZONE)
         ENDIF
      ENDIF
C
C---------------------------------------------------------------------
C
C --- IOSYS  = CODE NUMBER OF OUTPUT COORDINATE SYSTEM (INTEGER).
C --- IOZONE = CODE NUMBER OF OUTPUT COORDINATE ZONE (INTEGER).
C --- TPARIO = PARAMETERS OF OUTPUT REFERENCE SYSTEM (15 DP WORDS ARRAY)
C --- IOUNIT = CODE NUMBER OF UNITS OF MEASURE FOR OUTPUT COORDINATES (I
C --- CRDIO  = COORDINATES IN OUTPUT REFERENCE SYSTEM (2 DP WORDS ARRAY)
C --- IFLG   = RETURN FLAG (INTEGER).
C            = 0 , SUCCESSFUL TRANSFORMATION.
C            = 1 , ILLEGAL INPUT SYSTEM CODE.
C            = 2 , ILLEGAL OUTPUT SYSTEM CODE.
C            = 3 , ILLEGAL INPUT UNIT CODE.
C            = 4 , ILLEGAL OUTPUT UNIT CODE.
C            = 5 , INCONSISTENT UNIT AND SYSTEM CODES FOR INPUT.
C            = 6 , INCONSISTENT UNIT AND SYSTEM CODES FOR OUTPUT.
C            = 7 , ILLEGAL INPUT ZONE CODE.
C            = 8 , ILLEGAL OUTPUT ZONE CODE.
C
C----------------------------------------------------------------------
C
C --- PUTS THE OUTPUT INFORMATION INTO XYZIO AND SCALES
C --- NOTE THAT TCRDIO ARRAY HAS BEEN FILLED APPROPRIATELY WHEN AN
C --- OFFSET IS COMPUTED FOR PS AND EM
      IF(IOSYS.EQ.5.OR.IOSYS.EQ.6)THEN
         XYZIO(1) = (CRDIO(1) - TCRDIO(1))*DBLE(XMULTO)
         XYZIO(2) = (CRDIO(2) - TCRDIO(2))*DBLE(XMULTO)
      ELSE
         XYZIO(1) = CRDIO(1)*DBLE(XMULTO)
         XYZIO(2) = CRDIO(2)*DBLE(XMULTO)
      ENDIF
C
C --- NOW DOES A 'TO' (OUTPUT) PROJECTION CHECK
      JFLG = 1
C      IF(FLONIO.NE.0.0.AND.FLATIO.NE.0.0)THEN
C         CALL PRJCHK(LPR,IOSYS,FLONIO,FLATIO,JFLG,IRET)
C      ELSE
C        IF(FLONIN.NE.0.0.AND.FLATIN.NE.0.0)THEN
C            CALL PRJCHK(LPR,IOSYS,FLONIN,FLATIN,JFLG,IRET)
C         ENDIF
C      ENDIF
C
  999 CONTINUE
C 999  PRINT *,'FINISHED NORMALLY'
      RETURN
      END
c
c-----------------------------------------------------------------------
c --- Bring in BLOCK DATA as an include file
c-----------------------------------------------------------------------
      include 'blockdat.crd'
c
c----------------------------------------------------------------------
      SUBROUTINE PRJCHK(IO,INSYS,XLON,XLAT,IFLG,IRET)
c----------------------------------------------------------------------
c
C --- COORDLIB   Version: 1.99     Level: 021024                 PRJCHK
c
c --- Program was written by Gary Moore
c                            Earth Tech @2002  all rights
c                            Atmospheric Studies Group (ASG)
c                            Concord,MA 01742
c
c --- Program notes follow:
c
c----------------------------------------------------------------------
c
c --- Program function:
c
c --- This program writes out the errors associated with the mapping
c --- to various projections when the longitude and latitude are set to
c --- some values that are outside the bounds of the various projections.
c
c --- Program inputs are:
c
c --- io = FORTRAN logical unit for output
c --- insys = projection type
c --- xlon = double precision longitude
c --- xlat = double precision latitude
c --- iflg = error print flag
c
c --- Program outputs are:
c
c --- iret = error number
c
c----------------------------------------------------------------------
c
      Real*8 xlon, xlat
c
      Real*4 xlono,xlato
c
      Integer*4 iflg,io,iret,ichk
c
      ichk = 0
      xlato = sngl(xlat)
      xlono = sngl(xlon)
c
c --- Test for polar stereographic mapping
      if(insys.eq.6.and.abs(xlato).le.45.0)iret = iret + 100
c
c --- Test for mercator mapping
      if(insys.eq.5.and.abs(xlato).ge.45.0)iret = iret + 200
c
c --- Test for utm mapping
      if(insys.eq.1.and.(xlato.ge.84.0.or.xlato.le.-80.0))iret=iret
     1  + 300
c
c --- Test for transverse mercator mapping
      if(insys.eq.9.and.(xlato.ge.84.0.or.xlato.le.-80.0))iret=iret+
     1  400
c
c --- Print out
      IF(ICHK.GT.0)THEN
c         PRINT *,' WARNING INAPPROPIATE LATITUDE '
         WRITE(IO,'(A29)')'WARNING INAPPROPIATE LATITUDE'
      ENDIF
      Return
      End
c----------------------------------------------------------------------
      SUBROUTINE ERRPRT(IFLG,IO,IAPP)
c----------------------------------------------------------------------
c
C --- COORDLIB   Version: 1.99     Level: 020623                 ERRPRT
c
c --- Program was written by Gary Moore
c                            Earth Tech @2002  all rights
c                            Atmospheric Studies Group (ASG)
c                            Concord,MA 01742
c
c --- Program notes follow:
c----------------------------------------------------------------------
c
c --- Program function:
c
c --- This program writes out the errors associated with the USGS GCTP
c --- software.
c
c --- Program inputs are:
c
c --- io = FORTRAN logical unit for output
c --- iflg = error flag
c --- iapp = application number
c----------------------------------------------------------------------
C
C --- PRINT ERROR MESSAGES
      IF(IFLG.NE.0)THEN
c         PRINT *,' PROBLEMS WITH APPLICATION NUMBER: ',IAPP
         WRITE(IO,'(A35,I5)')' PROBLEMS WITH APPLICATION NUMBER: ',IAPP
         IF(IFLG.EQ.1)THEN
c            PRINT *,' ILLEGAL INPUT SYSTEM CODE.'
            WRITE(IO,'(A25)')'ILLEGAL INPUT SYSTEM CODE'
         ENDIF
         IF(IFLG.EQ.2)THEN
c            PRINT *,' ILLEGAL OUTPUT SYSTEM CODE.'
            WRITE(IO,'(A26)')'ILLEGAL OUTPUT SYSTEM CODE'
         ENDIF
         IF(IFLG.EQ.3)THEN
c            PRINT *,' ILLEGAL INPUT UNIT CODE.'
            WRITE(IO,'(A23)')'ILLEGAL INPUT UNIT CODE'
         ENDIF
         IF(IFLG.EQ.4)THEN
c            PRINT *,' ILLEGAL OUTPUT UNIT CODE.'
            WRITE(IO,'(A24)')'ILLEGAL OUTPUT UNIT CODE'
         ENDIF
         IF(IFLG.EQ.5)THEN
c           PRINT *,' INCONSISTENT UNIT/SYSTEM CODES FOR INPUT.'
           WRITE(IO,'(A40)')'INCONSISTENT UNIT/SYSTEM CODES FOR INPUT'
         ENDIF
         IF(IFLG.EQ.6)THEN
c           PRINT *,' INCONSISTENT UNIT/SYSTEM CODES FOR OUTPUT.'
           WRITE(IO,'(A41)')'INCONSISTENT UNIT/SYSTEM CODES FOR OUTPUT'
         ENDIF
         IF(IFLG.EQ.7)THEN
c            PRINT *,' ILLEGAL INPUT ZONE CODE.'
            WRITE(IO,'(A23)')'ILLEGAL INPUT ZONE CODE'
         ENDIF
         IF(IFLG.EQ.8)THEN
c            PRINT *,' ILLEGAL OUTPUT ZONE CODE.'
            WRITE(IO,'(A24)')'ILLEGAL OUTPUT ZONE CODE'
         ENDIF
         IF(IFLG.GT.8)THEN
c            PRINT *,' REALLY BAD UNDETERMINED ERROR! '
            WRITE(IO,'(A30)')'REALLY BAD UNDETERMINED ERROR!'
            STOP
         ENDIF
c         PRINT *,' WILL TRY NEXT COORDINATE SET: '
      ENDIF
      RETURN
      END
c----------------------------------------------------------------------
      Subroutine ll2zon(dxlon,dxlat,izone,iret)
c----------------------------------------------------------------------
c
C --- COORDLIB   Version: 1.99     Level: 020710                 LL2ZON
c
c --- Program was written by Gary Moore
c                            Earth Tech @2002  all rights
c                            Atmospheric Studies Group (ASG)
c                            Concord,MA 01742
c
c --- Program notes follow:
c
c----------------------------------------------------------------------
c
c --- Program function:
c
c --- This program converts longitude,latitude in to UTM zone for use
c --- in estimating the UTM zone of any given latitude and longitude.
c
c --- Program inputs are:
c
c --- dxlon = longitude in decimal degrees (DP)
c --- dxlat = latitude in decimal degrees (DP)
c
c --- Program outputs are:
c
c --- izone = utm zone in the range -60 < -1 and 1 < 60
c --- iret = a return code = 100 if the longitude is funky
c
c----------------------------------------------------------------------
c
      Real*8 dxlon,dxlat
c
      iret = 0
      if(dabs(dxlon).gt.180.0D0)then
         iret = 100
c         Print *,'magnitude of longitude is > 180 degrees!!!!'
         Return
      Endif
c
c --- NH E Quad
      If(dxlon.ge.0.0D0.and.dxlat.ge.0.0D0)then
      izone = dint(dabs(dxlon)/6.0D0) + 1
      izone = 30 + izone
      endif
c
c --- NH W Quad
      If(dxlon.le.0.0D0.and.dxlat.ge.0.0D0)then
      izone = dint(dabs(dxlon)/6.0D0) + 1
      izone = 31 - izone
      endif
c
c --- SH E Quad
      If(dxlon.ge.0.0D0.and.dxlat.le.0.0D0)then
      izone = dint(dabs(dxlon)/6.0D0) + 1
      izone = -(30 + izone)
      endif
c
c --- SH W Quad
      If(dxlon.le.0.0D0.and.dxlat.le.0.0D0)then
      izone = dint(dabs(dxlon)/6.0D0) + 1
      izone = -(31 - izone)
      endif
      if(izone.gt.60)izone = 60
      if(izone.lt.-60)izone = -60
      Return
      End
c----------------------------------------------------------------------
      Subroutine dd2dms(dd,dms)
c----------------------------------------------------------------------
c
C --- COORDLIB   Version: 1.99     Level: 020624                 DD2DMS
c
c
c --- Program was written by Gary Moore
c                            Earth Tech @2002  all rights
c                            Atmospheric Studies Group (ASG)
c                            Concord,MA 01742
c
c --- PROGRAM NOTES FOLLOW:
c
c----------------------------------------------------------------------
c
c --- Program function:
c
c --- Convert decimal degrees to packed degrees,mintues,econds format
c
c --- dd.ddddd to dddmmmsss.ss
c
c --- Program Inputs
c
c --- dd = decimal degrees (dp)
c
c --- Program Outputs
c
c --- dms = packed degrees minutes seconds format (dp)
c
c----------------------------------------------------------------------
c
      real*8 dd,dms
      real*4 sdd
c
      sdd = sngl(dd)
      ideg = int(sdd)
      xminit = (sdd - ideg)*60.0
      iminit = int(xminit)
      xsec = (xminit - iminit)*60.0
      dms = 1000000.D0*ideg + 1000.D0*iminit + 1.0D0*xsec
      return
      end

c----------------------------------------------------------------------
      Subroutine dat2dat(lpr,ipr,xlonin,xlatin,zlevin,irnkin,
     1 irnkio,xlonio,xlatio,zlevio)
c----------------------------------------------------------------------
c
C --- COORDLIB   Version: 1.99     Level: 030905                DAT2DAT
c
c --- Program was written by Gary Moore
c                            Earth Tech @2002  all rights
c                            Atmospheric Studies Group (ASG)
c                            Concord,MA 01742
c
c --- Program notes follow:
c
c --- Added a 9999 datum designamtion to do a manual datum trasformation
c --- using user input information in the common block XDATM (version 1.1
c --- 062002)
c
c --- Version 1.2 (071102)
c
c --- Changed calls to DATSHFT by adding IFLG so that a proper paired
c --- set of FROM-TO transformations could be made.
c
c --- Added the NIMA.CRD include. Use the new strings and pointers for
c --- handling the NIMA dataset.
c
c --- Version 1.3 102802
c
c --- Corrected the ao,fo - ai,fi used (switched order) on from ref to
c --- output datum
c
c --- Version 1.4 030703
c
c --- Blocked datum conversion to/from WGS84 lat-lon for sphere datums
c
c --- Version 1.9 Level: 030905
c
c --- Add iflg values 2 and 3 to datshft calls to go to and from WGS-72
c
c----------------------------------------------------------------------
c
c --- Program function:
c
c --- This program converts longitude,latitude in one datum to the
c --- longitude,latitude in another.  The program also does a shift in
c --- elevation due to a change in the geoid.
c
c --- Program inputs are:
c
c --- lpr = FORTRAN logical unit for output
c --- ipr = print flag => 0 to avoid printing
c --- xlonin = input longitude in decimal degrees (dp)
c --- xlatin = input latitude in decimal degrees  (dp)
c --- zlevin = elevation of the input point of interest in meters
c --- irnkin = input datum pointer
c --- irnkio = output datum pointer
c
c --- Program outputs are:
c
c --- xlonio = output longitude in decimal degrees (dp)
c --- xlatio = output latitude in decimal degrees  (dp)
c --- zlevio = revised elevation output of the input point in meters
c
c --- subroutine calls:
c
c --- DATSHFT
c
c----------------------------------------------------------------------
c
      Real*8 ai, ao, fi, fo, dx, dy, dz, xlonin, xlatin, zhti,
     1       xlonio, xlatio, zhto
      Real*8 xlato,xlono,drad,dflt
c
      Integer*4 iposi,iposo
c
      common /xdatm/ drad,dflt,dxshft,dyshft,dzshft
c
c --- NIMA data base include
      Include 'nima.crd'
c
c --- reference definition - the convention will be it is always 1!
      iref = 1
c
c --- asigns the positions
      if(irnkin.ne.9999)then
         iposi = irnkin
      else
         iposi = 0
      endif
      if(irnkio.ne.9999)then
         iposo = irnkio
      else
         iposo = 0
      endif
c
c --- Print out information
      if(ipr.ne.1.and.iposi.ne.0)then
         Write(lpr,'(a12,a8,1x,a50,1x,a60)')'From datum: ',
     1    datcod(iposi),datum(dattyp(iposi)),geodat1(iposi)
      endif
      if(ipr.ne.1.and.iposo.ne.0)then
         Write(lpr,'(a10,a8,1x,a50,1x,a60)')'To datum: ',
     1    datcod(iposo),datum(dattyp(iposo)),geodat1(iposo)
      endif
c
c --- datum to reference shift (i= input o = output)
      if(iposi.ne.0)then
         ai = dradim(iposi)
         fi = 1.0/dflat(iposi)
         ao = dradim(iref)
         fo = 1.0/dflat(iref)
         dx = dble(dxmod(iposi))
         dy = dble(dymod(iposi))
         dz = dble(dzmod(iposi))
         zhti = dble(zlevin)
      else
         ai = drad
         fi = 1.0/dflt
         ao = dradim(iref)
         fo = 1.0/dflat(iref)
         dx = dble(dxshft)
         dy = dble(dyshft)
         dz = dble(dzshft)
         zhti = dble(zlevin)
      endif
c --- Transform to WGS84 only if input datum is NOT a sphere
      if(fi.GT.1.0D-19) then
         if(datcod(iposi).eq.'WGS-72  ')then
            iiflag = 2
         else
            iiflag = 0
         endif
         Call datshft(xlonin,xlatin,zhti,ai,fi,fo,ao,dx,dy,dz,iiflag,
     1                xlono,xlato,zhto)
      else
         xlono=xlonin
         xlato=xlatin
         zhto=zhti
      endif
c
c --- reference to datum shift (i = input o = output) note same diffierence
c --- but a negative sign is used - this insures we get back to where
c --- we started!!!!
      if(iposo.ne.0)then
         ao = dradim(iref)
         fo = 1.0/dflat(iref)
         ai = dradim(iposo)
         fi = 1.0/dflat(iposo)
         dx = dble(dxmod(iposo))
         dy = dble(dymod(iposo))
         dz = dble(dzmod(iposo))
      else
         ai = drad
         fi = 1.0/dflt
         ao = dradim(iref)
         fo = 1.0/dflat(iref)
         dx = dble(dxshft)
         dy = dble(dyshft)
         dz = dble(dzshft)
      endif
c --- Transform from WGS84 only if output datum is NOT a sphere
      if(fi.GT.1.0D-19) then
         if(datcod(iposo).eq.'WGS-72  ')then
            iiflag = 3
         else
            iiflag = 1
         endif
         Call datshft(xlono,xlato,zhto,ai,fi,fo,ao,dx,dy,dz,iiflag,
     1                xlonio,xlatio,zhti)
      else
         xlonio=xlono
         xlatio=xlato
         zhti=zhto
      endif
      zlevio = sngl(zhti)
c
      Return
      End
c---------------------------------------------------------------------
      subroutine datshft(xloni,xlati,zhti,ai,fi,fo,ao,dx,dy,dz,iflg,
     1 xlono,xlato,zhto)
c----------------------------------------------------------------------
c
C --- COORDLIB   Version: 1.99     Level: 030905                DATSHFT
c
c --- Program was written by Gary Moore at Earth Tech - Concord MA
c
c --- Standard Modolensky Datum Transformation
c
c
c----------------------------------------------------------------------
c
c --- Program notes
c --- Added the IFLG argument for proper FROM - TO conversions
c
c
c --- Version 1.1
c --- Modified code constants to insure everything is DP
c --- Modified calculation of the reverse transformation. The reverse
c --- is done by subtracting the geodetics rather than inputing negative
c --- delta X,Y,Z.
c
c --- Version 1.9 Level: 030905
c
c --- Add equations and special option to go to and from WGS-72
c----------------------------------------------------------------------
c
c --- Program function:
c
c --- This program converts the lat/lon/height of one datum to another
c --- assuming an earth center shift of dx,dy,dz (geoid specific) and the
c --- ellipsoid major axis and flattening of each datum.
c
c --- Input arguments - double precision
c
c --- xlati = input latitude in decimal dgrees
c --- xloni = input longitude in decimal degrees
c --- zhti = input elevation in meters
c --- ai = input major radius in meters
c --- fi = input flattening factor
c --- fo = output flattening factor
c --- ao = output major radius
c --- dx = datum to reference earth center shift in meters
c --- dy = datum to reference earth center shift in meters
c --- dz = datum to reference earth center shift in meters
c --- iflg = 0 FROM datum A TO WGS84 = 1 TO datum B FROM WGS84
c --- iflg = 2 FROM datum A to WGS72 = 3 TO datum B FROM WGS72
c
c --- Output arguments - double precision
c
c --- xlato = output longitude in decimal degrees
c --- xlono = output longitude in decimal degrees
c --- zhto = output elevation in meters
c
c --- Subroutine calls:
c
c --- None
c
c----------------------------------------------------------------------
c
      real*8 xlati,xloni,zhti,ai,fi,fo,ao,dx,dy,dz,xlato,xlono,zhto
      real*8 deg2rad,rad2deg,da,df,sithet,siphi,cithet,ciphi,siphi2
      real*8 rn,rm,dlat,dlon,dh,one,two,dlat72,dh72
      real*8 es,bda,c1,c2,c3,c4,d1,d2,e1,e2,e3,e4,e5
c
c --- compute some double precision constants
      deg2rad = 0.01745329252D0
      rad2deg = 57.295779513D0
      one = 1.0D0
      two = 2.0D0
c
c --- compute delta radius/flattening - double precision
      da = ao - ai
      df = fo - fi
      es = two*fi - fi*fi                  ! eccentricity squared
      bda = one - fi                       !pole/equator radius ratio
c
c --- compute sin,cos of theta and phi - double precision
      siphi = dsin(xlati*deg2rad)
      siphi2 = dsin(xlati*2.0*deg2rad)
      ciphi = dcos(xlati*deg2rad)
      sithet = dsin(xloni*deg2rad)
      cithet = dcos(xloni*deg2rad)
c
c --- radius of curvature - prime vertical
      rn = ai/dsqrt(one - es*siphi**2)
c
c --- radius of curvature - prime meridian
      rm = ai*(one - es)/(one - es*siphi**2)**1.5
c
c --- shift in latitude
      c1 = -dx*siphi*cithet - dy*siphi*sithet + dz*ciphi
      c2 = da*(rn*es*siphi*ciphi)/ai
      c3 = df*(rm/bda + rn*bda)*siphi*ciphi
      c4 = rm + zhti
      dlat = (c1 + c2 + c3)/c4
      dlat72 = 4.5D0*ciphi/(ai*sin(1.0*deg2rad/3600.0)) +
     1         df*siphi2/(sin(1.0*deg2rad/3600.0))
c
c --- shift in longitude
      d1 = -dx*sithet + dy*cithet
      d2 = (rn + zhti)*ciphi
      dlon = d1/d2
c
c --- shift in height
      e1 = dx*ciphi*cithet
      e2 = dy*ciphi*sithet
      e3 = dz*siphi
      e4 = da*ai/rn
      e5 = df*bda*rn*siphi*siphi
      dh = e1 + e2 + e3 - e4 + e5
      dh72 = 4.5D0*siphi + ai*df*siphi*siphi - da + 1.4D0
c
c --- estimate the output arguments
      if(iflg.eq.0)then
         xlato = xlati + dlat*rad2deg
         xlono = xloni + dlon*rad2deg
         zhto = zhti + dh
      endif
      if(iflg.eq.1)then
         xlato = xlati - dlat*rad2deg
         xlono = xloni - dlon*rad2deg
         zhto = zhti - dh
      endif
c
c --- Special WGS-72 change 030905
      if(iflg.eq.2)then
         xlato = xlati + dlat72/3600.0D0
         xlono = xloni + 0.554D0/3600.0D0
         zhto = zhti + dh72
      endif
      if(iflg.eq.3)then
         xlato = xlati - dlat72/3600.0D0
         xlono = xloni - 0.554D0/3600.0D0
         zhto = zhti - dh72
      endif
c
      return
      end
c----------------------------------------------------------------------
      Subroutine init(datloc,datnam,datid,datreg1,datreg2,datreg3,
     1 max,maxd)
c----------------------------------------------------------------------
c
c --- COORDLIB   Version: 1.99     Level: 021016                   INIT
c
c --- Program was written by Gary Moore at Earth Tech - Concord MA
c
c --- Initializes the NIMA data label arrays
c
c----------------------------------------------------------------------
c
c --- Program notes
c
c----------------------------------------------------------------------
c
c --- Program function:
c
c --- This program does some string housekeeping and outputs the strings
c --- for use by a GUI or some other management routines. It starts
c --- with the NIMA common blocks that are input via the NIMA.CRD include
c --- block.
c
c --- Input arguments:
c
c --- MAX = maximum number of datums in the data base
c
c --- Output arguments - double precision
c
c --- DATID = 8 character ID code array for each datum
c --- DATLOC = 20 character Atlas location string array
c --- DATNAM = 50 character Datum name string array
c --- DATREG1 = 60 character Region descriptor string array - line 1
c --- DATREG2 = 60 character Region descriptor string array - line 2
c --- DATREG3 = 60 character Region descriptor string array - line 3
c
c --- Subroutine calls:
c
c --- None
c
c----------------------------------------------------------------------
c
      CHARACTER*8 DATID(MAX)
      CHARACTER*20 DATLOC(MAX)
      CHARACTER*50 ISTRNG, DATNAM(MAX)
      CHARACTER*60 DATREG1(MAX), DATREG2(MAX), DATREG3(MAX)
c
c --- Calls the include
      Include 'nima.crd'
c
c --- First maps the DATLOC and DATNAM arrays
      maxd = kmax
      Do i = 1,kmax
         DATLOC(i) = Atlas(dattyp(i))
         DATNAM(i) = Datum(dattyp(i))
         DATID(i) = Datcod(i)
         DATREG1(i) = Geodat1(i)
         DATREG2(i) = Geodat2(i)
         DATREG3(i) = Geodat3(i)
      Enddo
c
c --- Now compresses the Datum name string
      Do k = 1,kmax
         istrng = datnam(k)
         Do j = 1,29
            jj = 29 - j + 1
            if(istrng(jj:jj).ne.' ')then
               jbeg = jj + 2
               go to 444
            endif
         Enddo
444      continue
         jend = jbeg + 20
         if(jend.gt.50)jend = 50
         istrng(jbeg:jend) = istrng(30:50)
         if(jend.lt.50)then
            Do j = jend+1,50
               istrng(j:j) = ' '
            Enddo
         endif
         datnam(k) = istrng
      Enddo
      Return
      End
C-----------------------------------------------------------------------
C     GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE - VERSION 2.0.2
C     FORTRAN 77 LANGUAGE FOR IBM, AMDAHL, ENCORE, VAX, CONCURRENT, AND
C     DATA GENERAL COMPUTERS
C                   ADJLZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION ADJLZ0 (LONIN)

c --- V1.98 (060911)
c --- Change argument name and reassign (sub is called with a computed
c --- argument that should not be changed within subroutine)

C
C FUNCTION TO ADJUST LONGITUDE ANGLE TO MODULO 180 DEGREES.
C
      IMPLICIT REAL*8 (A-Z)
      DATA TWO,PI /2.0D0,3.14159265358979323846D0/

c --- V1.98 (060911)
      LON=LONIN
C
  020 ADJLZ0 = LON
      IF (DABS(LON) .LE. PI) RETURN
      TWOPI = TWO * PI
      LON = LON - DSIGN (TWOPI,LON)
      GO TO 020
C
      END
C                   ASINZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION ASINZ0 (CON)
C
C THIS FUNCTION ADJUSTS FOR ROUND-OFF ERRORS IN COMPUTING ARCSINE
C
      IMPLICIT REAL*8 (A-Z)
      DATA ONE /1.0D0/
C
      IF (DABS(CON) .GT. ONE) THEN
       CON = DSIGN (ONE,CON)
       ENDIF
      ASINZ0 = DASIN (CON)
      RETURN
C
      END
C                   DMSPZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION DMSPZ0 (SGNA,DEGS,MINS,SECS)
C
C SUBROUTINE TO CONVERT UNPACKED DMS TO PACKED DMS ANGLE
C SGNA : SIGN OF ANGLE
C DEGS : DEGREES PORTION OF ANGLE
C MINS : MINUTES PORTION OF ANGLE
C SECS : SECONDS PORTION OF ANGLE
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 SECS
      INTEGER*4 DEGS,MINS
      CHARACTER*1 SGNA,NEG
      DATA CON1,CON2 /1000000.0D0,1000.0D0/
      DATA NEG /'-'/
C
      CON = DBLE (DEGS) * CON1 + DBLE (MINS) * CON2 + DBLE (SECS)
      IF (SGNA .EQ. NEG) CON = - CON
      DMSPZ0 = CON
      RETURN
C
      END
C                   E0FNZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION E0FNZ0 (ECCNTS)
C
C FUNCTION TO COMPUTE CONSTANT (E0).
C
      IMPLICIT REAL*8 (A-Z)
      DATA QUART,ONE,ONEQ,THREE,SIXT /0.25D0,1.0D0,1.25D0,3.0D0,16.0D0/
C
      E0FNZ0 = ONE - QUART * ECCNTS * (ONE + ECCNTS / SIXT *
     .         (THREE + ONEQ * ECCNTS))
C
      RETURN
      END
C                   E1FNZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION E1FNZ0 (ECCNTS)
C
C FUNCTION TO COMPUTE CONSTANT (E1).
C
      IMPLICIT REAL*8 (A-Z)
      DATA CON1,CON2,CON3 /0.375D0,0.25D0,0.46875D0/
      DATA ONE /1.0D0/
C
      E1FNZ0 = CON1 * ECCNTS * (ONE + CON2 * ECCNTS *
     .         (ONE + CON3 * ECCNTS))
C
      RETURN
      END
C                   E2FNZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION E2FNZ0 (ECCNTS)
C
C FUNCTION TO COMPUTE CONSTANT (E2).
C
      IMPLICIT REAL*8 (A-Z)
      DATA CON1,CON2 /0.05859375D0,0.75D0/
      DATA ONE /1.0D0/
C
      E2FNZ0 = CON1 * ECCNTS * ECCNTS * (ONE + CON2 * ECCNTS)
C
      RETURN
      END
C                   E3FNZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION E3FNZ0 (ECCNTS)
C
C FUNCTION TO COMPUTE CONSTANT (E3).
C
      IMPLICIT REAL*8 (A-Z)
C
      E3FNZ0 = ECCNTS*ECCNTS*ECCNTS*(35.D0/3072.D0)
C
      RETURN
      END
C                   E4FNZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION E4FNZ0 (ECCENT)
C
C FUNCTION TO COMPUTE CONSTANT (E4).
C
      IMPLICIT REAL*8 (A-Z)
      DATA ONE /1.0D0/
C
      CON = ONE + ECCENT
      COM = ONE - ECCENT
      E4FNZ0 = DSQRT ((CON ** CON) * (COM ** COM))
C
      RETURN
      END
C                   GTPZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      SUBROUTINE GTPZ0(CRDIN,INSYS,INZONE,TPARIN,INUNIT,INSPH,IPR,JPR,
     .     LEMSG,LPARM,CRDIO,IOSYS,IOZONE,TPARIO,IOUNIT,LN27,LN83,
     .     FN27,FN83,LENGTH,IFLG)
C
C **********************************************************************
C
C INPUT ****************************************************************
C CRDIN  : COORDINATES IN INPUT SYSTEM (2 DP WORDS ARRAY).
C INSYS  : CODE NUMBER OF INPUT COORDINATE SYSTEM (INTEGER).
C            =  0 , GEOGRAPHIC
C            =  1 , U T M
C            =  2 , STATE PLANE
C            =  3 , ALBERS CONICAL EQUAL-AREA
C            =  4 , LAMBERT CONFORMAL CONIC
C            =  5 , MERCATOR
C            =  6 , POLAR STEREOGRAPHIC
C            =  7 , POLYCONIC
C            =  8 , EQUIDISTANT CONIC
C            =  9 , TRANSVERSE MERCATOR
C            = 10 , STEREOGRAPHIC
C            = 11 , LAMBERT AZIMUTHAL EQUAL-AREA
C            = 12 , AZIMUTHAL EQUIDISTANT
C            = 13 , GNOMONIC
C            = 14 , ORTHOGRAPHIC
C            = 15 , GENERAL VERTICAL NEAR-SIDE PERSPECTIVE
C            = 16 , SINUSOIDAL
C            = 17 , EQUIRECTANGULAR (PLATE CARREE)
C            = 18 , MILLER CYLINDRICAL
C            = 19 , VAN DER GRINTEN I
C            = 20 , OBLIQUE MERCATOR (HOTINE)
C            = 21 , ROBINSON
C            = 22 , SPACE OBLIQUE MERCATOR
C            = 23 , MODIFIED-STEREOGRAPHIC CONFORMAL (ALASKA)
C INZONE : CODE NUMBER OF INPUT COORDINATE ZONE (INTEGER).
C TPARIN : PARAMETERS OF INPUT REFERENCE SYSTEM (15 DP WORDS ARRAY).
C INUNIT : CODE NUMBER OF UNITS OF MEASURE FOR INPUT COORDINATES (I*4)
C            = 0 , RADIANS.
C            = 1 , U.S. FEET.
C            = 2 , METERS.
C            = 3 , SECONDS OF ARC.
C            = 4 , DEGREES OF ARC.
C            = 5 , INTERNATIONAL FEET.
C            = 6 , USE LEGISLATED DISTANCE UNITS FROM NADUT TABLE
C INSPH  : INPUT SPHEROID CODE.  SEE SPHDZ0 FOR PROPER CODES.
C IPR    : PRINTOUT FLAG FOR ERROR MESSAGES. 0=YES, 1=NO
C JPR    : PRINTOUT FLAG FOR PROJECTION PARAMETERS 0=YES, 1=NO
C LEMSG  : LOGICAL UNIT FOR LISTING ERROR MESSAGES IF IPR = 0
C LPARM  : LOGICAL UNIT FOR LISTING PROJECTION PARAMETERS IF JPR = 0
C LN27   : LOGICAL UNIT FOR NAD 1927 SPCS PARAMETER FILE
C FN27   : FILE NAME OF NAD 1927 SPCS PARAMETERS
C LN83   : LOGICAL UNIT FOR NAD 1983 SPCS PARAMETER FILE
C FN83   : FILE NAME OF NAD 1983 SPCS PARAMETERS
C LENGTH : RECORD LENGTH OF NAD1927 AND NAD1983 PARAMETER FILES
C OUTPUT ***                                                       *****
C IOSYS  : CODE NUMBER OF OUTPUT COORDINATE SYSTEM (INTEGER).
C IOZONE : CODE NUMBER OF OUTPUT COORDINATE ZONE (INTEGER).
C TPARIO : PARAMETERS OF OUTPUT REFERENCE SYSTEM (15 DP WORDS ARRAY).
C IOUNIT : CODE NUMBER OF UNITS OF MEASURE FOR OUTPUT COORDINATES (I*4)
C CRDIO  : COORDINATES IN OUTPUT REFERENCE SYSTEM (2 DP WORDS ARRAY).
C IFLG   : RETURN FLAG (INTEGER).
C            = 0 , SUCCESSFUL TRANSFORMATION.
C            = 1 , ILLEGAL INPUT SYSTEM CODE.
C            = 2 , ILLEGAL OUTPUT SYSTEM CODE.
C            = 3 , ILLEGAL INPUT UNIT CODE.
C            = 4 , ILLEGAL OUTPUT UNIT CODE.
C            = 5 , INCONSISTENT UNIT AND SYSTEM CODES FOR INPUT.
C            = 6 , INCONSISTENT UNIT AND SYSTEM CODES FOR OUTPUT.
C            = 7 , ILLEGAL INPUT ZONE CODE.
C            = 8 , ILLEGAL OUTPUT ZONE CODE.
C      OTHERWISE , ERROR CODE FROM PROJECTION COMPUTATIONAL MODULE.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 NAD27(134), NAD83(134), NADUT(54), SPTYPE(134)
      INTEGER*4 SYSUNT(24), SWITCH(23), ITER

c --- V1.98 (060911)
      INTEGER*4 INSPHZERO

      INTEGER*2 INMOD, IOMOD, FWD, INV
      CHARACTER*128 FN27, FN83, FILE27, FILE83
      DIMENSION CRDIN(2),CRDIO(2),TPARIN(15),TPARIO(15),COORD(2)
      DIMENSION DUMMY(15), PDIN(15), PDIO(15)
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /ELLPZ0/ AZ,EZ,ESZ,E0Z,E1Z,E2Z,E3Z,E4Z
      COMMON /PROJZ0/ IPROJ
      COMMON /SPCS/ ISPHER,LU27,LU83,LEN,MSYS,FILE27,FILE83
      COMMON /TOGGLE/ SWITCH
C
      PARAMETER (MAXUNT=6, MAXSYS=23)
      PARAMETER (FWD=0, INV=1)
      DATA SYSUNT / 0 , 23*2 /
      DATA PDIN/15*0.0D0/, PDIO/15*0.0D0/
      DATA INSP/999/, INPJ/999/, INZN/99999/
      DATA IOSP/999/, IOPJ/999/, IOZN/99999/
      DATA ITER /0/
      DATA JFLAG/0/
C
      DATA NAD27/0101,0102,5010,5300,0201,0202,0203,0301,0302,0401,0402,
     .           0403,0404,0405,0406,0407,0501,0502,0503,0600,0700,0901,
     .           0902,0903,1001,1002,5101,5102,5103,5104,5105,1101,1102,
     .           1103,1201,1202,1301,1302,1401,1402,1501,1502,1601,1602,
     .           1701,1702,1703,1801,1802,1900,2001,2002,2101,2102,2103,
     .           2111,2112,2113,2201,2202,2203,2301,2302,2401,2402,2403,
     .           2501,2502,2503,2601,2602,2701,2702,2703,2800,2900,3001,
     .           3002,3003,3101,3102,3103,3104,3200,3301,3302,3401,3402,
     .           3501,3502,3601,3602,3701,3702,3800,3901,3902,4001,4002,
     .           4100,4201,4202,4203,4204,4205,4301,4302,4303,4400,4501,
     .           4502,4601,4602,4701,4702,4801,4802,4803,4901,4902,4903,
     .           4904,5001,5002,5003,5004,5005,5006,5007,5008,5009,5201,
     .           5202,5400/
C
      DATA NAD83/0101,0102,5010,5300,0201,0202,0203,0301,0302,0401,0402,
     .           0403,0404,0405,0406,0000,0501,0502,0503,0600,0700,0901,
     .           0902,0903,1001,1002,5101,5102,5103,5104,5105,1101,1102,
     .           1103,1201,1202,1301,1302,1401,1402,1501,1502,1601,1602,
     .           1701,1702,1703,1801,1802,1900,2001,2002,2101,2102,2103,
     .           2111,2112,2113,2201,2202,2203,2301,2302,2401,2402,2403,
     .           2500,0000,0000,2600,0000,2701,2702,2703,2800,2900,3001,
     .           3002,3003,3101,3102,3103,3104,3200,3301,3302,3401,3402,
     .           3501,3502,3601,3602,3701,3702,3800,3900,0000,4001,4002,
     .           4100,4201,4202,4203,4204,4205,4301,4302,4303,4400,4501,
     .           4502,4601,4602,4701,4702,4801,4802,4803,4901,4902,4903,
     .           4904,5001,5002,5003,5004,5005,5006,5007,5008,5009,5200,
     .           0000,5400/
C
C     TABLE OF UNIT CODES AS SPECIFIED BY STATE LAWS AS OF 2/1/92
C     FOR NAD 1983 SPCS - 1 = U.S. SURVEY FEET, 2 = METERS,
C                         5 = INTERNATIONAL FEET
C
C     NADUT - UNIT CODES FOR THE STATES ARRANGED IN STATE NUMBER ORDER
C              (FIRST TWO DIGITS OF ZONE NUMBER)
C
      DATA NADUT /1, 5, 1, 1, 5, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2,
     .            1, 1, 5, 2, 1, 2, 5, 1, 2, 2, 2, 1, 1, 1, 5, 2, 1, 5,
     .            2, 2, 5, 2, 1, 1, 5, 2, 2, 1, 2, 1, 2, 2, 1, 2, 2, 2/
C
C     TABLE OF STATE PLANE ZONE TYPES:  4 = LAMBERT, 7 = POLYCONIC,
C     9 = TRANSVERSE MERCATOR, AND 20 = OBLIQUE MERCATOR
C
      DATA SPTYPE / 9, 9, 4, 4, 9, 9, 9, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
     .              4, 4, 4, 9, 9, 9, 4, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
     .              9, 9, 9, 9, 4, 4, 4, 4, 4, 4, 4, 4, 4, 9, 9, 4, 4,
     .              4, 9, 9, 9, 4, 4, 4, 4, 4, 4, 9, 9, 9, 9, 9, 4, 4,
     .              4, 4, 4, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 4, 4, 4,
     .              4, 4, 4, 4, 4, 4, 4, 4, 4, 9, 4, 4, 4, 4, 4, 4, 4,
     .              4, 4, 4, 4, 4, 4, 9, 4, 4, 4, 4, 4, 4, 4, 4, 4, 9,
     .              9, 9, 9,20, 9, 9, 9, 9, 9, 9, 9, 9, 4, 4, 7/
C
C     SETUP
C
      IOSPH = INSPH
      IPEMSG = IPR
      IPPARM = JPR
      IPELUN = LEMSG
      IPPLUN = LPARM
      IPROJ = INSYS
      LU27 = LN27
      FILE27 = FN27
      LU83 = LN83
      FILE83 = FN83
      LEN = LENGTH
C
C     INITIALIZE SWITCH FOR EACH PROJECTION TO ZERO
C
      ITER = ITER + 1
      IF (ITER .LE. 1) THEN
         DO 5 I=1,15
            DUMMY(I) = 0.0D0
    5    CONTINUE
         MSYS = 2
      END IF
      INSPCS = 2
      IOSPCS = 2
      IF (JFLAG.NE.0) GO TO 10
      EZ = 0.0D0
      ESZ = 0.0D0

c --- V1.98 (060911)
c     CALL SPHDZ0(0,DUMMY)
c --- Set sphere as a variable instead of a constant
      insphzero=0
      CALL SPHDZ0(insphzero,DUMMY)
C
C --- SPECIAL TREATMENT FOR STARTUP
      IF(TPARIO(14).NE.0D0.AND.TPARIO(15).NE.0D0)THEN
         DUMMY(1) = TPARIO(14)
         DUMMY(2) = TPARIO(15)
      ENDIF
      JFLAG = 1
C
C CHECK VALIDITY OF CODES FOR REFERENCE SYSTEMS.
C
   10 IF (INSYS.LT.0 .OR. INSYS.GT.MAXSYS) THEN
         IF (IPEMSG .NE. 0) WRITE (IPELUN,2000) INSYS
 2000    FORMAT (' ILLEGAL SOURCE REFERENCE SYSTEM CODE = ',I6)
         IFLG = 1
         RETURN
      END IF
C
      IF (IOSYS.LT.0 .OR. IOSYS.GT.MAXSYS) THEN
         IF (IPEMSG .NE. 0) WRITE (IPELUN,2010) IOSYS
 2010    FORMAT (' ILLEGAL TARGET REFERENCE SYSTEM CODE = ',I6)
         IFLG = 2
         RETURN
      END IF
C
C     FORCE INITIALIZATION OF PROJECTIONS IF SPHEROID OR PROJECTION
C     HAS CHANGED FROM PREVIOUS INPUT - OUTPUT SET
C
C
C----------------------------------------------------------------------
C
C --- THIS SECTION IS TO BE PLACED IN ALL VERSIONS OF USGS CODE TO FORCE
C --- REINITIALIZATION EACH TIME.
C
C----------------------------------------------------------------------
      DO I = 1,MAXSYS
         SWITCH(I) = 0
      ENDDO
C----------------------------------------------------------------------
C
      IF (INSPH .NE. INSP) THEN
         DO 11 I = 1,MAXSYS
            SWITCH(I) = 0
   11    CONTINUE
      END IF
C
      IF (INSYS .GT. 0) THEN
         IF (INSYS .NE. INPJ .AND. INSYS .NE. IOPJ) SWITCH(INSYS) = 0
         IF (SWITCH(INSYS) .NE. INZONE .AND. SWITCH(INSYS) .NE. IOZONE)
     .      SWITCH(INSYS) = 0
      END IF
C
      IF (IOSYS .GT. 0) THEN
         IF (IOSYS .NE. INPJ .AND. IOSYS .NE. IOPJ) SWITCH(IOSYS) = 0
         IF (SWITCH(IOSYS) .NE. INZONE .AND. SWITCH(IOSYS) .NE. IOZONE)
     .      SWITCH(IOSYS) = 0
      END IF
C
C     CHECK FOR REPEAT OF INPUT SYSTEM
C
      INMOD = 1
      IF (INSYS .EQ. 2) THEN
         IF (INZONE .GT. 0) THEN
            ID = 0
            IF (INSPH .EQ. 0) THEN
               DO 12 I = 1,134
                  IF (INZONE .EQ. NAD27(I)) ID = I
   12          CONTINUE
            END IF
            IF (INSPH .EQ. 8) THEN
               DO 13 I = 1,134
                  IF (INZONE .EQ. NAD83(I)) ID = I
   13          CONTINUE
            END IF
            IF (ID .NE. 0) INSPCS = SPTYPE(ID)
            IF (INZONE .NE. SWITCH(INSPCS)) GO TO 15
         END IF
      END IF
      IF (INSP .NE. INSPH) GO TO 15
      IF (INPJ .NE. INSYS) GO TO 15
      IF (INZN .NE. INZONE) GO TO 15
      IF (INSYS .GE. 3) THEN
         DO 14 I=1,15
            IF (TPARIN(I) .NE. PDIN(I)) GO TO 15
   14    CONTINUE
      END IF
      INMOD = 0
      GO TO 30
C
C     SAVE INPUT SYSTEM PARAMETERS
C
   15 INSP = INSPH
      INPJ = INSYS
      INZN = INZONE
      DO 16 I=1,15
   16 PDIN(I) = TPARIN(I)
C
C CHECK CONSISTENCY BETWEEN UNITS OF MEASURE
C
      IF (INUNIT.LT.0 .OR. INUNIT.GT.MAXUNT) THEN
         IF (IPEMSG .NE. 0) WRITE (IPELUN,2020) INUNIT
 2020    FORMAT (' ILLEGAL SOURCE UNIT CODE = ',I6)
         IFLG = 3
         RETURN
      END IF
C
C     CHECK FOR REPEAT OF OUTPUT SYSTEM
C
   30 IOMOD = 1
      IF (IOSYS .EQ. 2) THEN
         IF (IOZONE .GT. 0) THEN
            ID = 0
            IF (IOSPH .EQ. 0) THEN
               DO 32 I = 1,134
                  IF (IOZONE .EQ. NAD27(I)) ID = I
   32          CONTINUE
            END IF
            IF (IOSPH .EQ. 8) THEN
               DO 33 I = 1,134
                  IF (IOZONE .EQ. NAD83(I)) ID = I
   33          CONTINUE
            END IF
            IF (ID .NE. 0) IOSPCS = SPTYPE(ID)
            IF (IOZONE .NE. SWITCH(INSPCS)) GO TO 35
         END IF
      END IF
      IF (IOSP .NE. INSPH) GO TO 35
      IF (IOSP .NE. IOSPH) GO TO 35
      IF (IOPJ .NE. IOSYS) GO TO 35
      IF (IOZN .NE. IOZONE) GO TO 35
      IF (IOSYS .GE. 3) THEN
         DO 34 I=1,15
            IF (TPARIO(I) .NE. PDIO(I)) GO TO 35
   34    CONTINUE
      END IF
      IOMOD = 0
      GO TO 80
C
C     SAVE OUTPUT SYSTEM PARAMETERS
C
   35 IOSP = INSPH
      IOPJ = IOSYS
      IOZN = IOZONE
      DO 36 I=1,15
   36 PDIO(I) = TPARIO(I)
C
C CHECK CONSISTENCY BETWEEN UNITS OF MEASURE
C
      IF (IOUNIT.LT.0 .OR. IOUNIT.GT.MAXUNT) THEN
         IF (IPEMSG .NE. 0) WRITE (IPELUN,2030) IOUNIT
 2030    FORMAT (' ILLEGAL TARGET UNIT CODE = ',I6)
         IFLG = 4
         RETURN
      END IF
C
   80 IUNIT = SYSUNT(INSYS + 1)
C
C     CHANGE UNITS TO LEGISLATED UNITS USING TABLE
C
      IF (INSPH .EQ. 0 .AND. INSYS .EQ. 2 .AND. INUNIT .EQ. 6) INUNIT=1
      IF (INSPH .EQ. 8 .AND. INSYS .EQ. 2 .AND. INUNIT .EQ. 6) THEN
         IND = 0
         DO 90 I = 1,134
            IF (INZONE .EQ. NAD83(I)) IND = I
   90    CONTINUE
         IF (IND .NE. 0) INUNIT = NADUT( INT(INZONE/100))
      END IF
      CALL UNTFZ0 (INUNIT,IUNIT,FACTOR,IFLG)
      IF (IFLG .EQ. 0) GO TO 100
      IFLG = 5
      RETURN
  100 COORD(1) = FACTOR * CRDIN(1)
      COORD(2) = FACTOR * CRDIN(2)
      IUNIT = SYSUNT(IOSYS + 1)
C
C     CHANGE UNITS TO LEGISLATED UNITS USING TABLE
C
      IF (INSPH .EQ. 0 .AND. IOSYS .EQ. 2 .AND. IOUNIT .EQ. 6) IOUNIT=1
      IF (INSPH .EQ. 8 .AND. IOSYS .EQ. 2 .AND. IOUNIT .EQ. 6) THEN
         IND = 0
         DO 110 I = 1,134
            IF (IOZONE .EQ. NAD83(I)) IND = I
  110    CONTINUE
         IF (IND .NE. 0) IOUNIT = NADUT( INT(IOZONE/100))
      END IF
      CALL UNTFZ0 (IUNIT,IOUNIT,FACTOR,IFLG)
      IF (IFLG .EQ. 0) GO TO 120
      IFLG = 6
      RETURN
  120 IF (INSYS.NE.IOSYS.OR.INZONE.NE.IOZONE.OR.INZONE.LE.0) GO TO 140
      CRDIO(1) = FACTOR * COORD(1)
      CRDIO(2) = FACTOR * COORD(2)
      RETURN
C
C COMPUTE TRANSFORMED COORDINATES AND ADJUST THEIR UNITS.
C
  140 IF (INSYS .EQ. 0) GO TO 520
      IF (INZONE.GT.60 .OR. INSYS.EQ.1) GO TO 200
      IF (IPEMSG .NE. 0) WRITE (IPELUN,2040) INZONE
 2040 FORMAT (' ILLEGAL SOURCE ZONE NUMBER = ',I6)
      IFLG = 7
      RETURN
C
C INVERSE TRANSFORMATION.
C
  200 IPROJ=INSYS
      ISPHER = INSPH
      IF (INSYS.GE.3) CALL SPHDZ0(INSPH,TPARIN)
C
C     CHECK FOR CHANGE IN ZONE FROM LAST USE OF THE INPUT PROJECTION
C
      IF (INSYS .EQ. 1 .AND. INZONE .NE. SWITCH(9)) THEN
         SWITCH(1) = 0
         INMOD = 1
      END IF
      IF (INSYS .EQ. 2 .AND. INZONE .NE. SWITCH(INSPCS)) THEN
         SWITCH(2) = 0
         INMOD = 1
      END IF
      IF (INZONE .NE. SWITCH(INSYS)) THEN
         SWITCH(INSYS) = 0
         INMOD = 1
      END IF
C
      IF (INSYS .EQ. 1) THEN
         IF (INZONE.EQ.0.AND.TPARIN(1).NE.0.0D0) GO TO 211
         TPARIN(1) = 1.0D6*DBLE(6*INZONE-183)
         TPARIN(2) = DSIGN(4.0D7,DBLE(INZONE))
  211    CALL SPHDZ0(INSPH,DUMMY)
         TPARIN(14) = DUMMY(1)
         TPARIN(15) = DUMMY(2)
         IF (INMOD .NE. 0) THEN
            CALL PJINIT (INSYS,INZONE,TPARIN)
            IF (IERROR .NE. 0) INZN = 99999
            IF (IERROR .NE. 0) GO TO 500
         END IF
         CALL PJ01Z0 (COORD,CRDIO,INV)
      END IF
C
      IF (INSYS .GT. 1) THEN
         IF (INMOD .NE. 0) THEN
            MSYS = INSPCS
            CALL PJINIT (INSYS,INZONE,TPARIN)
            IF (IERROR .NE. 0) INZN = 99999
            IF (IERROR .NE. 0) GO TO 500
         END IF
         IF (INSYS .EQ. 2) CALL PJ02Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 3) CALL PJ03Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 4) CALL PJ04Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 5) CALL PJ05Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 6) CALL PJ06Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 7) CALL PJ07Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 8) CALL PJ08Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 9) CALL PJ09Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 10) CALL PJ10Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 11) CALL PJ11Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 12) CALL PJ12Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 13) CALL PJ13Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 14) CALL PJ14Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 15) CALL PJ15Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 16) CALL PJ16Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 17) CALL PJ17Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 18) CALL PJ18Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 19) CALL PJ19Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 20) CALL PJ20Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 21) CALL PJ21Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 22) CALL PJ22Z0 (COORD,CRDIO,INV)
         IF (INSYS .EQ. 23) CALL PJ23Z0 (COORD,CRDIO,INV)
      END IF
C
  500 IFLG = IERROR
      DO 510 I = 1,15
  510 TPARIN(I) = PDIN(I)
      IF (IFLG .NE. 0) RETURN
      CRDIO(1) = ADJLZ0(CRDIO(1))
      IF (IOSYS .EQ. 0) GO TO 920
      COORD(1) = CRDIO(1)
      COORD(2) = CRDIO(2)
  520 IF (INSYS .EQ. 0 .AND. IOSYS .EQ. 0) THEN
         CRDIO(1) = COORD(1)
         CRDIO(2) = COORD(2)
         GO TO 920
      END IF
      IF (IOZONE.GT.60 .OR. IOSYS.EQ.1) GO TO 540
      IF (IPEMSG .NE. 0) WRITE (IPELUN,2050) IOSYS
 2050 FORMAT (' ILLEGAL TARGET ZONE NUMBER = ',I6)
      IFLG = 8
      RETURN
C
C FORWARD TRANSFORMATION.
C
  540 IPROJ=IOSYS
      ISPHER = INSPH
      IF (IOSYS.GE.3) CALL SPHDZ0(INSPH,TPARIO)
C
C     CHECK FOR CHANGE IN ZONE FROM LAST USE OF THE OUTPUT PROJECTION
C
      IF (IOSYS .EQ. 1 .AND. IOZONE .NE. SWITCH(9)) THEN
         SWITCH(1) = 0
         IOMOD = 1
      END IF
      IF (IOSYS .EQ. 2 .AND. IOZONE .NE. SWITCH(IOSPCS)) THEN
         SWITCH(2) = 0
         IOMOD = 1
      END IF
      IF (IOZONE .NE. SWITCH(IOSYS)) THEN
         SWITCH(IOSYS) = 0
         IOMOD = 1
      END IF
C
      IF (IOSYS .EQ. 1) THEN
         TPARIO(1) = COORD(1)
         TPARIO(2) = COORD(2)
         CALL SPHDZ0(INSPH,DUMMY)
         TPARIO(14) = DUMMY(1)
         TPARIO(15) = DUMMY(2)
         IF (IOMOD .NE. 0) THEN
            CALL PJINIT (IOSYS,IOZONE,TPARIO)
            IF (IERROR .NE. 0) IOZN = 99999
            IF (IERROR .NE. 0) GO TO 900
         END IF
         CALL PJ01Z0 (COORD,CRDIO,FWD)
      END IF
C
      IF (IOSYS .GT. 1) THEN
         IF (IOMOD .NE. 0) THEN
            MSYS = IOSPCS
            CALL PJINIT (IOSYS,IOZONE,TPARIO)
            IF (IERROR .NE. 0) IOZN = 99999
            IF (IERROR .NE. 0) GO TO 900
         END IF
         IF (IOSYS .EQ. 2) CALL PJ02Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 3) CALL PJ03Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 4) CALL PJ04Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 5) CALL PJ05Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 6) CALL PJ06Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 7) CALL PJ07Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 8) CALL PJ08Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 9) CALL PJ09Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 10) CALL PJ10Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 11) CALL PJ11Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 12) CALL PJ12Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 13) CALL PJ13Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 14) CALL PJ14Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 15) CALL PJ15Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 16) CALL PJ16Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 17) CALL PJ17Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 18) CALL PJ18Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 19) CALL PJ19Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 20) CALL PJ20Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 21) CALL PJ21Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 22) CALL PJ22Z0 (COORD,CRDIO,FWD)
         IF (IOSYS .EQ. 23) CALL PJ23Z0 (COORD,CRDIO,FWD)
      END IF
C
  900 IFLG = IERROR
      DO 910 I = 1,15
  910 TPARIO(I) = PDIO(I)
  920 CRDIO(1) = FACTOR * CRDIO(1)
      CRDIO(2) = FACTOR * CRDIO(2)
      RETURN
C
      END
C                   MLFNZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION MLFNZ0 (E0,E1,E2,E3,PHI)
C
C FUNCTION TO COMPUTE CONSTANT (M).
C
      IMPLICIT REAL*8 (A-Z)
      DATA TWO,FOUR,SIX /2.0D0,4.0D0,6.0D0/
C
      MLFNZ0 = E0 * PHI - E1 * DSIN (TWO * PHI) + E2 * DSIN (FOUR * PHI)
     * - E3 * DSIN (SIX * PHI)
C
      RETURN
      END
C                   MSFNZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION MSFNZ0 (ECCENT,SINPHI,COSPHI)
C
C FUNCTION TO COMPUTE CONSTANT (SMALL M).
C
      IMPLICIT REAL*8 (A-Z)
      DATA ONE /1.0D0/
C
      CON = ECCENT * SINPHI
      MSFNZ0 = COSPHI / DSQRT (ONE - CON * CON)
C
      RETURN
      END
C                   PAKCZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION PAKCZ0 (PAK)
C
C SUBROUTINE TO CONVERT 2 DIGIT PACKED DMS TO 3 DIGIT PACKED DMS ANGLE.
C
C SGNA : SIGN OF ANGLE
C DEGS : DEGREES PORTION OF ANGLE
C MINS : MINUTES PORTION OF ANGLE
C SECS : SECONDS PORTION OF ANGLE
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 DEGS,MINS
      CHARACTER*1 SGNA,IBLANK,NEG
      DATA ZERO,CON1,CON2 /0.0D0,10000.0D0,100.0D0/
      DATA CON3,CON4 /1000000.0D0,1000.0D0/
      DATA TOL /1.0D-3/
      DATA IBLANK,NEG /' ','-'/
C
      SGNA = IBLANK
      IF (PAK .LT. ZERO) SGNA = NEG
      CON = DABS (PAK)
      DEGS = IDINT ((CON / CON1) + TOL)
      CON = DMOD ( CON , CON1)
      MINS = IDINT ((CON / CON2) + TOL)
      SECS = DMOD (CON , CON2)
C
      CON = DBLE (DEGS) * CON3 + DBLE (MINS) * CON4 + SECS
      IF (SGNA .EQ. NEG) CON = - CON
      PAKCZ0 = CON
      RETURN
C
      END
C                   PAKDZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      SUBROUTINE PAKDZ0 (PAK,SGNA,DEGS,MINS,SECS)
C
C SUBROUTINE TO CONVERT PACKED DMS TO UNPACKED DMS ANGLE.
C
C SGNA : SIGN OF ANGLE
C DEGS : DEGREES PORTION OF ANGLE
C MINS : MINUTES PORTION OF ANGLE
C SECS : SECONDS PORTION OF ANGLE
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 SECS
      INTEGER*4 DEGS,MINS
      CHARACTER*1 SGNA,IBLANK,NEG
      DATA ZERO,CON1,CON2 /0.0D0,1000000.0D0,1000.0D0/
      DATA TOL /1.0D-4/
      DATA IBLANK,NEG /' ','-'/
C
      SGNA = IBLANK
      IF (PAK .LT. ZERO) SGNA = NEG
      CON = DABS (PAK)
      DEGS = IDINT ((CON / CON1) + TOL)
      CON = DMOD ( CON , CON1)
      MINS = IDINT ((CON / CON2) + TOL)
      SECS = SNGL ( DMOD (CON , CON2))
      RETURN
C
      END
C                   PAKRZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION PAKRZ0 (ANG)
C
C FUNCTION TO CONVERT DMS PACKED ANGLE INTO RADIANS.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DATA SECRAD /0.4848136811095359D-5/
C
C CONVERT ANGLE TO SECONDS OF ARC
C
      SEC = PAKSZ0 (ANG)
C
C CONVERT ANGLE TO RADIANS.
C
      PAKRZ0 = SEC * SECRAD
C
      RETURN
      END
C                   PAKSZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION PAKSZ0 (ANG)
C
C FUNCTION TO CONVERT DMS PACKED ANGLE INTO SECONDS OF ARC.
C
      IMPLICIT REAL*8 (A-H,M-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      DIMENSION CODE(2)
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      DATA CODE /1000000.0D0,1000.0D0/
      DATA ZERO,ONE /0.0D0,1.0D0/
      DATA C1,C2 /3600.0D0,60.0D0/
      DATA TOL /1.0D-4/
C
C SEPARATE DEGREE FIELD.
C
      FACTOR = ONE
      IF (ANG .LT. ZERO) FACTOR = - ONE
      SEC = DABS(ANG)
      TMP = CODE(1)
      I = IDINT ((SEC / TMP) + TOL)
      IF (I .GT. 360) GO TO 020
      DEG = DBLE (I)
C
C SEPARATE MINUTES FIELD.
C
      SEC = SEC - DEG * TMP
      TMP = CODE(2)
      I = IDINT ((SEC / TMP) + TOL)
      IF (I .GT. 60) GO TO 020
      MIN = DBLE (I)
C
C SEPARATE SECONDS FIELD.
C
      SEC = SEC - MIN * TMP
      IF (SEC .GT. C2) GO TO 020
      SEC = FACTOR * (DEG * C1 + MIN * C2 + SEC)
      GO TO 040
C
C ERROR DETECTED IN DMS FORM.
C
  020 WRITE (IPELUN,2000) ANG
 2000 FORMAT ('0ERROR PAKSZ0'/
     .        ' ILLEGAL DMS FIELD =',F15.3)
      STOP 16
C
  040 PAKSZ0 = SEC
C
      RETURN
      END
C                   PHI1Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION PHI1Z0 (ECCENT,QS)
C
C FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-1).
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 II,NIT
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      DATA HALF,ONE /0.5D0,1.0D0/
      DATA EPSLN,TOL,NIT /1.0D-7,1.0D-10,15/
C
      PHI1Z0 = ASINZ0 (HALF * QS)
      IF (ECCENT .LT. EPSLN) RETURN
C
      ECCNTS = ECCENT * ECCENT
      PHI = PHI1Z0
      DO 020 II = 1,NIT
      SINPI = DSIN (PHI)
      COSPI = DCOS (PHI)
      CON = ECCENT * SINPI
      COM = ONE - CON * CON
      DPHI = HALF * COM * COM / COSPI * (QS / (ONE - ECCNTS) -
     .       SINPI / COM + HALF / ECCENT * DLOG ((ONE - CON) /
     .       (ONE + CON)))
      PHI = PHI + DPHI
      IF (DABS(DPHI) .GT. TOL) GO TO 020
      PHI1Z0 = PHI
      RETURN
  020 CONTINUE
C
      IF (IPEMSG .EQ. 0) WRITE (IPELUN,2000) NIT,ECCENT,QS
 2000 FORMAT ('0ERROR PHI1Z0' /
     .        ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS'/
     .        ' ECCENTRICITY =',D25.16,'   QS =',D25.16)
      IERROR = 001
      RETURN
C
      END
C                   PHI2Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION PHI2Z0 (ECCENT,TS)
C
C FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-2).
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 II,NIT
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      DATA HALF,ONE,TWO /0.5D0,1.0D0,2.0D0/
      DATA TOL,NIT /1.0D-10,15/
      DATA HALFPI /1.5707963267948966D0/
C
      ECCNTH = HALF * ECCENT
      PHI = HALFPI - TWO * DATAN (TS)
      DO 020 II = 1,NIT
      SINPI = DSIN (PHI)
      CON = ECCENT * SINPI
      DPHI = HALFPI - TWO * DATAN (TS * ((ONE - CON) /
     .       (ONE + CON)) ** ECCNTH) - PHI
      PHI = PHI + DPHI
      IF (DABS(DPHI) .GT. TOL) GO TO 020
      PHI2Z0 = PHI
      RETURN
  020 CONTINUE
C
      IF (IPEMSG .EQ. 0) WRITE (IPELUN,2000) NIT,ECCENT,TS
 2000 FORMAT ('0ERROR PHI2Z0' /
     .        ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS'/
     .        ' ECCENTRICITY =',D25.16,'   TS =',D25.16)
      IERROR = 002
      RETURN
C
      END
C                   PHI3Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION PHI3Z0 (ML,E0,E1,E2,E3)
C
C FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-3).
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 II,NIT
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      DATA TWO,FOUR,SIX /2.0D0,4.0D0,6.0D0/
      DATA TOL,NIT /1.0D-10,15/
C
      PHI = ML
      DO 020 II = 1,NIT
      DPHI = (ML + E1 * DSIN (TWO * PHI) - E2 * DSIN (FOUR * PHI)
     .       + E3 * DSIN (SIX * PHI)) / E0 - PHI
      PHI = PHI + DPHI
      IF (DABS(DPHI) .GT. TOL) GO TO 020
      PHI3Z0 = PHI
      RETURN
  020 CONTINUE
C
      IF (IPEMSG .EQ. 0) WRITE (IPELUN,2000) NIT,ML,E0,E1,E2,E3
 2000 FORMAT ('0ERROR PHI3Z0' /
     .        ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS'/
     .        ' ML =',D25.16,'   E0 =',D25.16/
     .        ' E1 =',D25.16,'   E2 =',D25.16,'   E3=',D25.16)
      IERROR = 003
      RETURN
C
      END
C                   PHI4Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      SUBROUTINE PHI4Z0 (ECCNTS,E0,E1,E2,E3,A,B,C,PHI)
C
C FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-4).
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 II,NIT
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      DATA ONE,TWO,FOUR,SIX /1.0D0,2.0D0,4.0D0,6.0D0/
      DATA TOL,NIT /1.0D-10,15/
C
      PHI = A
      DO 020 II = 1,NIT
      SINPHI = DSIN (PHI)
      TANPHI = DTAN (PHI)
      C = TANPHI * DSQRT (ONE - ECCNTS * SINPHI * SINPHI)
      SIN2PH = DSIN (TWO * PHI)
      ML = E0 * PHI - E1 * SIN2PH + E2 * DSIN (FOUR * PHI)
     .      - E3 * DSIN (SIX * PHI)
      MLP = E0 - TWO * E1 * DCOS (TWO * PHI) + FOUR * E2 *
     .      DCOS (FOUR * PHI) - SIX * E3 * DCOS (SIX * PHI)
      CON1 = TWO * ML + C * (ML * ML + B) - TWO * A *
     .       (C * ML + ONE)
      CON2 = ECCNTS * SIN2PH * (ML * ML + B - TWO * A * ML) / (TWO * C)
      CON3 = TWO * (A - ML) * (C * MLP - TWO / SIN2PH) - TWO * MLP
      DPHI = CON1 / (CON2 + CON3)
      PHI = PHI + DPHI
      IF (DABS(DPHI) .GT. TOL) GO TO 020
      RETURN
  020 CONTINUE
C
      IF (IPEMSG .EQ. 0) WRITE (IPELUN,2000) NIT,E0,E1,E2,E3,A,B,C,
     . ECCNTS
 2000 FORMAT ('0ERROR PHI4Z0' /
     .        ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS'/
     .        ' E0 =',D25.16,'   E1 =',D25.16/
     .        ' E2 =',D25.16,'   E3 =',D25.16/
     .        ' A  =',D25.16,'   B  =',D25.16/
     .        ' C  =',D25.16/
     .        ' ECCENTRICITY SQUARE =',D25.16)
      IERROR = 004
      RETURN
C
      END
C                   PJINIT
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      SUBROUTINE PJINIT (ISYS,ZONE,DATA)
cc ----------------------------------------------------------------------
c --- UPDATE (for use in COORDS)
c
c --- V1.98-V1.99   070921  (DGS)
c     Modify UTM section of PJINIT in to fix erroneous non-zero
c     false Northing when converting S. hemisphere locations to UTM-N
c     coordinates.  Calls from COORDS to GTPZ0 manage the UTM zone
c     (negative for S. hemisphere) so the zone alone should be used to
c     set the false Northing for UTM in the S. hemisphere.  Calls made
c     with a positive zone MUST result in UTM-N coordinates, which are
c     negative in the S. hemisphere.
c ----------------------------------------------------------------------
C
      IMPLICIT REAL*8 (A-Z)
      REAL*4 SECS(5)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN,ITEMP
      INTEGER*4 LAND, PATH, LIMIT, IND02, IND06, IND09, ISYS, KEEPZN
      INTEGER*4 SWITCH(23),I,ZONE,DEGS(5),MINS(5)
      INTEGER*4 ID, IND, ITEM, ITYPE, MODE, N, MSYS
      INTEGER*4 ISPHER, LUNIT, LU27, LU83, LEN, NAD27(134), NAD83(134)
      CHARACTER*128 DATUM, FILE27, FILE83
      CHARACTER*32 PNAME
      CHARACTER*1 SGNA(5)
C
      DIMENSION DATA(15),BUFFL(15)
      DIMENSION TABLE(9)
      DIMENSION PR(20),XLR(20)
      DIMENSION ACOEF(6),BCOEF(6)
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /ELLPZ0/ AZ,EZ,ESZ,E0Z,E1Z,E2Z,E3Z,E4Z
      COMMON /SPHRZ0/ AZZ
      COMMON /NORM/ Q,T,U,W,ES22,P22,SA,CA,XJ
      COMMON /SPCS/ ISPHER,LU27,LU83,LEN,MSYS,FILE27,FILE83
      COMMON /PJ02/ ITYPE
      COMMON /PJ03/ A03,LON003,X003,Y003,C,E03,ES03,NS03,RH003
      COMMON /PJ04/ A04,LON004,X004,Y004,E04,F04,NS04,RH004
      COMMON /PJ05/ A05,LON005,X005,Y005,E05,M1
      COMMON /PJ06/ A06,LON006,X006,Y006,E06,E4,FAC,MCS,TCS,IND06
      COMMON /PJ07/ A07,LON007,X007,Y007,E07,E007,E107,E207,E307,ES07,
     .              ML007
      COMMON /PJ08/ A08,LON008,X008,Y008,E008,E108,E208,E308,GL,NS08,
     .              RH008
      COMMON /PJ09/ A09,LON009,X009,Y009,ES09,ESP,E009,E109,E209,E309,
     .              KS009,LAT009,ML009,IND09
      COMMON /PJ10/ A10,LON010,X010,Y010,COSP10,LAT010,SINP10
      COMMON /PJ11/ A11,LON011,X011,Y011,COSP11,LAT011,SINP11
      COMMON /PJ12/ A12,LON012,X012,Y012,COSP12,LAT012,SINP12
      COMMON /PJ13/ A13,LON013,X013,Y013,COSP13,LAT013,SINP13
      COMMON /PJ14/ A14,LON014,X014,Y014,COSP14,LAT014,SINP14
      COMMON /PJ15/ A15,LON015,X015,Y015,COSP15,LAT015,P,SINP15
      COMMON /PJ16/ A16,LON016,X016,Y016
      COMMON /PJ17/ A17,LON017,X017,Y017,LAT1
      COMMON /PJ18/ A18,LON018,X018,Y018
      COMMON /PJ19/ A19,LON019,X019,Y019
      COMMON /PJ20/ LON020,X020,Y020,AL,BL,COSALF,COSGAM,E20,EL,SINALF,
     .              SINGAM,U0
      COMMON /PJ21/ A21,LON021,X021,Y021,PR,XLR
      COMMON /PJ22/ A22,X022,Y022,A2,A4,B,C1,C3,LAND,PATH
      COMMON /PJ23/ A23,LON023,X023,Y023,ACOEF,BCOEF,EC,LAT023,
     .              CCHIO,SCHIO,N
      COMMON /TOGGLE/ SWITCH
C
      DATA PI /3.14159265358979323846D0/
      DATA HALFPI /1.5707963267948966D0/
      DATA ZERO,HALF,ONE,TWO /0.0D0,0.5D0,1.0D0,2.0D0/
      DATA EPSLN /1.0D-10/
      DATA TOL /1.0D-7/
      DATA TOL09 /1.0D-5/
      DATA NINTYD /90000000.0D0/
      DATA DG1 /0.01745329252D0/

c --- V1.98 (060911)
c --- Set initial value of SAVE9
      data SAVE9/0.0D0/
C
      DATA NAD27/0101,0102,5010,5300,0201,0202,0203,0301,0302,0401,0402,
     .           0403,0404,0405,0406,0407,0501,0502,0503,0600,0700,0901,
     .           0902,0903,1001,1002,5101,5102,5103,5104,5105,1101,1102,
     .           1103,1201,1202,1301,1302,1401,1402,1501,1502,1601,1602,
     .           1701,1702,1703,1801,1802,1900,2001,2002,2101,2102,2103,
     .           2111,2112,2113,2201,2202,2203,2301,2302,2401,2402,2403,
     .           2501,2502,2503,2601,2602,2701,2702,2703,2800,2900,3001,
     .           3002,3003,3101,3102,3103,3104,3200,3301,3302,3401,3402,
     .           3501,3502,3601,3602,3701,3702,3800,3901,3902,4001,4002,
     .           4100,4201,4202,4203,4204,4205,4301,4302,4303,4400,4501,
     .           4502,4601,4602,4701,4702,4801,4802,4803,4901,4902,4903,
     .           4904,5001,5002,5003,5004,5005,5006,5007,5008,5009,5201,
     .           5202,5400/
C
      DATA NAD83/0101,0102,5010,5300,0201,0202,0203,0301,0302,0401,0402,
     .           0403,0404,0405,0406,0000,0501,0502,0503,0600,0700,0901,
     .           0902,0903,1001,1002,5101,5102,5103,5104,5105,1101,1102,
     .           1103,1201,1202,1301,1302,1401,1402,1501,1502,1601,1602,
     .           1701,1702,1703,1801,1802,1900,2001,2002,2101,2102,2103,
     .           2111,2112,2113,2201,2202,2203,2301,2302,2401,2402,2403,
     .           2500,0000,0000,2600,0000,2701,2702,2703,2800,2900,3001,
     .           3002,3003,3101,3102,3103,3104,3200,3301,3302,3401,3402,
     .           3501,3502,3601,3602,3701,3702,3800,3900,0000,4001,4002,
     .           4100,4201,4202,4203,4204,4205,4301,4302,4303,4400,4501,
     .           4502,4601,4602,4701,4702,4801,4802,4803,4901,4902,4903,
     .           4904,5001,5002,5003,5004,5005,5006,5007,5008,5009,5200,
     .           0000,5400/
C ....................................................................
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                              .  U T M  .
C ......................................................................
C
      KSYS = 0
      IF (ISYS .EQ. 1) THEN
C
         IERROR = 0
         IF (SWITCH(1).NE.0 .AND. SWITCH(1).EQ.ZONE) RETURN
         SWITCH(1) = ZONE
         IF (SWITCH(9).NE.0.AND.SWITCH(9).EQ.ZONE.AND.DATA(14).EQ.SAVE)
     .   RETURN
         KEEPZN = ZONE
         ZONE = IABS(ZONE)
         SAVE = DATA(1)
         IF (ZONE .EQ. 0) THEN
            ZONE = IDINT( ( (DATA(1) * 180.0D0 / PI)
     .             + (TOL09 / 3600.D0) ) / 6.D0 )
            IND = 1
            IF (DATA(1) .LT. ZERO) IND = 0
            ZONE = MOD ((ZONE + 30), 60) + IND
            KEEPZN = ZONE
            IF (DATA(2) .LT. ZERO) KEEPZN = -ZONE
         ENDIF
         IF (ZONE.LT.1 .OR. ZONE.GT.60) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,140) KEEPZN
  140       FORMAT ('0ERROR PJ01Z0'/
     .              ' ILLEGAL ZONE NO. : ',I10)
            IERROR = 011
            RETURN
         ENDIF
         BUFFL(1) = DATA(14)
         BUFFL(2) = DATA(15)
         BUFFL(3) = 0.9996D0
         BUFFL(4) = ZERO
         BUFFL(5) = DBLE (6 * ZONE - 183) * 1.0D6
         BUFFL(6) = ZERO
         BUFFL(7) = 500000.0D0
         BUFFL(8) = ZERO

c --- COORDS
c --- Use just the ZONE provided when setting the false Northing
c         IF (DATA(2) .LT. ZERO) BUFFL(8) = 10000000.0D0

         IF (KEEPZN .LT. 0) BUFFL(8) = 10000000.0D0
         IF (BUFFL(1).NE.0.0D0.AND.BUFFL(1).NE.SAVE9) SWITCH(9) = 0
         SAVE9 = BUFFL(1)
         ITEMP = IPPARM
         IPPARM = 1
         DO 145 I=1,8
            DATA(I) = BUFFL(I)
  145    CONTINUE
         AZ = DATA(14)
         EZ = DATA(15)
         SWITCH(9) = 0
         KSYS = 9
         GO TO 900
      ENDIF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                           .  STATE PLANE  .
C ......................................................................
C
      KSYS = 0
      IF (ISYS .EQ. 2) THEN
C
         IERROR = 0
         IF (SWITCH(2).NE.0 .AND. SWITCH(2).EQ.ZONE) RETURN
         IF (ISPHER .NE. 0 .AND. ISPHER .NE. 8) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,205) ISPHER
  205       FORMAT('0ERROR PJ02Z0'/
     .             ' SPHEROID NO. ',I4,' IS INVALID FOR STATE PLANE',
     .             ' TRANSFORMATIONS')
            IERROR = 020
            RETURN
         ENDIF
         IF (ZONE .GT. 0) THEN
            IND02 = 0
            IF (ISPHER .EQ. 0) THEN
               DO 210 I = 1,134
                  IF (ZONE .EQ. NAD27(I)) IND02 = I
  210          CONTINUE
            ENDIF
            IF (ISPHER .EQ. 8) THEN
               DO 220 I = 1,134
                  IF (ZONE .EQ. NAD83(I)) IND02 = I
  220          CONTINUE
            ENDIF
            IF (IND02 .EQ. 0) THEN
               IF (IPEMSG .EQ. 0)WRITE (IPELUN,240) ZONE, ISPHER
               IERROR = 021
               RETURN
            ENDIF
         ELSE
            IF (IPEMSG .EQ. 0)WRITE (IPELUN,240) ZONE, ISPHER
            IERROR = 021
            RETURN
         ENDIF
         IF (ISPHER .EQ. 0) THEN
            LUNIT = LU27
            DATUM = FILE27
         ENDIF
         IF (ISPHER .EQ. 8) THEN
            LUNIT = LU83
            DATUM = FILE83
         ENDIF
         OPEN (UNIT=LUNIT,FILE=DATUM,STATUS='OLD',ACCESS='DIRECT',
     .   RECL=LEN)
         READ (UNIT=LUNIT,REC=IND02) PNAME,ID,TABLE
         CLOSE (UNIT=LUNIT,STATUS='KEEP')
         IF (ID .LE. 0) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,240) ZONE, ISPHER
  240       FORMAT('0ERROR PJ02Z0'/
     .             ' ILLEGAL ZONE NO. : ',I8,' FOR SPHEROID NO. : ',I4)
            IERROR = 021
            RETURN
         ENDIF
         ITYPE = ID
         AZ = TABLE(1)
         ES = TABLE(2)
         ESZ = ES
         EZ  = DSQRT(ES)
         E0Z = E0FNZ0(ES)
         E1Z = E1FNZ0(ES)
         E2Z = E2FNZ0(ES)
         E3Z = E3FNZ0(ES)
         E4Z = E4FNZ0(EZ)
         ITEMP = IPPARM
         IPPARM = 1
C
C     TRANSVERSE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 1) THEN
            DATA(3) = TABLE(4)
            DATA(5) = PAKCZ0(TABLE(3))
            DATA(6) = PAKCZ0(TABLE(7))
            DATA(7) = TABLE(8)
            DATA(8) = TABLE(9)
            MSYS = 9
            SWITCH(MSYS) = 0
            KSYS = 9
            GO TO 900
         ENDIF
C
C     LAMBERT CONFORMAL PROJECTION
C
         IF (ITYPE .EQ. 2) THEN
            DATA(3) = PAKCZ0(TABLE(6))
            DATA(4) = PAKCZ0(TABLE(5))
            DATA(5) = PAKCZ0(TABLE(3))
            DATA(6) = PAKCZ0(TABLE(7))
            DATA(7) = TABLE(8)
            DATA(8) = TABLE(9)
            MSYS = 4
            SWITCH(MSYS) = 0
            KSYS = 4
            GO TO 400
         ENDIF
C
C     POLYCONIC PROJECTION
C
         IF (ITYPE .EQ. 3) THEN
            DATA(5) = PAKCZ0(TABLE(3))
            DATA(6) = PAKCZ0(TABLE(4))
            DATA(7) = TABLE(5)
            DATA(8) = TABLE(6)
            MSYS = 7
            SWITCH(MSYS) = 0
            KSYS = 7
            GO TO 700
         ENDIF
C
C     OBLIQUE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 4) THEN
            DATA(3) = TABLE(4)
            DATA(4) = PAKCZ0(TABLE(6))
            DATA(5) = PAKCZ0(TABLE(3))
            DATA(6) = PAKCZ0(TABLE(7))
            DATA(7) = TABLE(8)
            DATA(8) = TABLE(9)
            DATA(13) = ONE
            MSYS = 20
            SWITCH(MSYS) = 0
            KSYS = 20
            GO TO 2000
         ENDIF
C
      ENDIF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                    .  ALBERS CONICAL EQUAL AREA  .
C ......................................................................
C
      IF (ISYS .EQ. 3) THEN
C
         IERROR = 0
         IF (SWITCH(3).NE.0 .AND. SWITCH(3).EQ.ZONE) RETURN
         SWITCH(3) = 0
         A03 = AZ
         E03 = EZ
         ES03 = ESZ
         LAT1 = PAKRZ0 (DATA(3))
         LAT2 = PAKRZ0 (DATA(4))
         IF (DABS(LAT1+LAT2) .LT. EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,340)
  340       FORMAT ('0ERROR PJ03Z0'/
     .              ' EQUAL LATITUDES FOR ST. PARALLELS ON OPPOSITE',
     .              ' SIDES OF EQUATOR')
            IERROR = 031
            RETURN
         END IF
         LON003 = PAKRZ0 (DATA(5))
         LAT003 = PAKRZ0 (DATA(6))
         X003 = DATA(7)
         Y003 = DATA(8)
         SINP03 = DSIN (LAT1)
         CON = SINP03
         COSP03 = DCOS (LAT1)
         MS1 = MSFNZ0 (E03,SINP03,COSP03)
         QS1 = QSFNZ0 (E03,SINP03,COSP03)
         SINP03 = DSIN (LAT2)
         COSP03 = DCOS (LAT2)
         MS2 = MSFNZ0 (E03,SINP03,COSP03)
         QS2 = QSFNZ0 (E03,SINP03,COSP03)
         SINP03 = DSIN (LAT003)
         COSP03 = DCOS (LAT003)
         QS0 = QSFNZ0 (E03,SINP03,COSP03)
         IF (DABS(LAT1-LAT2) .GE. EPSLN) THEN
            NS03 = (MS1 * MS1 - MS2 * MS2) / (QS2 - QS1)
         ELSE
            NS03 = CON
         END IF
         C = MS1 * MS1 + NS03 * QS1
         RH003 = A03 * DSQRT (C - NS03 * QS0) / NS03
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT2,SGNA(2),DEGS(2),MINS(2),SECS(2))
         CALL RADDZ0 (LON003,SGNA(3),DEGS(3),MINS(3),SECS(3))
         CALL RADDZ0 (LAT003,SGNA(4),DEGS(4),MINS(4),SECS(4))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,350) A03,ES03,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,4),
     .            X003,Y003
  350   FORMAT ('0INITIALIZATION PARAMETERS (ALBERS CONICAL EQUAL-AREA',
     .           ' PROJECTION)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' LATITUDE OF 1ST ST. PARALLEL = ',A1,2I3,F7.3/
     .           ' LATITUDE OF 2ND ST. PARALLEL = ',A1,2I3,F7.3/
     .           ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .           ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A03
         DATA(2) = ES03
         SWITCH(3) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                     .  LAMBERT CONFORMAL CONIC  .
C ......................................................................
C
400   CONTINUE
      IF (KSYS.EQ.4.OR.ISYS .EQ. 4) THEN
C
         IERROR = 0
         IF (SWITCH(4).NE.0 .AND. SWITCH(4).EQ.ZONE) RETURN
         SWITCH(4) = 0
         A04 = AZ
         E04 = EZ
         ES = ESZ
         LAT1 = PAKRZ0 (DATA(3))
         LAT2 = PAKRZ0 (DATA(4))
         IF (DABS(LAT1+LAT2) .LT. EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,440)
  440       FORMAT ('0ERROR PJ04Z0'/
     .              ' EQUAL LATITUDES FOR ST. PARALLELS ON OPPOSITE',
     .              ' SIDES OF EQUATOR')
            IERROR = 041
            RETURN
         END IF
         LON004 = PAKRZ0 (DATA(5))
         LAT004 = PAKRZ0 (DATA(6))
         X004 = DATA(7)
         Y004 = DATA(8)
         SINP04 = DSIN (LAT1)
         CON = SINP04
         COSP04 = DCOS (LAT1)
         MS1 = MSFNZ0 (E04,SINP04,COSP04)
         TS1 = TSFNZ0 (E04,LAT1,SINP04)
         SINP04 = DSIN (LAT2)
         COSP04 = DCOS (LAT2)
         MS2 = MSFNZ0 (E04,SINP04,COSP04)
         TS2 = TSFNZ0 (E04,LAT2,SINP04)
         SINP04 = DSIN (LAT004)
         TS0 = TSFNZ0 (E04,LAT004,SINP04)
         IF (DABS(LAT1-LAT2) .GE. EPSLN) THEN
            NS04 = DLOG (MS1 / MS2) / DLOG (TS1 / TS2)
         ELSE
            NS04 = CON
         END IF
         F04 = MS1 / (NS04 * TS1 ** NS04)
         RH004 = A04 * F04 * TS0 ** NS04
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT2,SGNA(2),DEGS(2),MINS(2),SECS(2))
         CALL RADDZ0 (LON004,SGNA(3),DEGS(3),MINS(3),SECS(3))
         CALL RADDZ0 (LAT004,SGNA(4),DEGS(4),MINS(4),SECS(4))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,450) A04,ES,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,4),
     .            X004,Y004
  450    FORMAT ('0INITIALIZATION PARAMETERS (LAMBERT CONFORMAL CONIC',
     .           ' PROJECTION)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' LATITUDE OF 1ST ST. PARALLEL = ',A1,2I3,F7.3/
     .           ' LATITUDE OF 2ND ST. PARALLEL = ',A1,2I3,F7.3/
     .           ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .           ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A04
         DATA(2) = ES
         SWITCH(4) = ZONE
C
C     LIST STATE PLANE INITIALIZATION PARAMETERS IF NECESSARY
C
         IF (ISYS .EQ. 2) THEN
            IPPARM = ITEMP
            IF (IERROR .NE. 0) RETURN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,470) ZONE, PNAME
  470    FORMAT (' INITIALIZATION PARAMETERS (STATE PLANE PROJECTION)'/
     .           ' ZONE NUMBER = ',I4,5X,' ZONE NAME = ',A32)
            SWITCH(2) = ZONE
            RETURN
         END IF
C
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                            .  MERCATOR  .
C ......................................................................
C
      IF (ISYS .EQ. 5) THEN
C
         IERROR = 0
         IF (SWITCH(5).NE.0 .AND. SWITCH(5).EQ.ZONE) RETURN
         SWITCH(5) = 0
         A05 = AZ
         E05 = EZ
         ES = ESZ
         LON005 = PAKRZ0 (DATA(5))
         LAT1 = PAKRZ0 (DATA(6))
         M1 = DCOS(LAT1) / (DSQRT( ONE - ES * DSIN(LAT1) **2))
         X005 = DATA(7)
         Y005 = DATA(8)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LON005,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,550) A05,ES,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X005,Y005
  550    FORMAT ('0INITIALIZATION PARAMETERS (MERCATOR',
     .           ' PROJECTION)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' LATITUDE OF TRUE SCALE       = ',A1,2I3,F7.3/
     .           ' CENTRAL LONGITUDE            = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A05
         DATA(2) = ES
         SWITCH(5) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                       .  POLAR STEREOGRAPHIC  .
C ......................................................................
C
      IF (ISYS .EQ. 6) THEN
C
         IERROR = 0
         IF (SWITCH(6).NE.0 .AND. SWITCH(6).EQ.ZONE) RETURN
         SWITCH(6) = 0
         A06 = AZ
         E06 = EZ
         ES = ESZ
         E4 = E4Z
         LON006 = PAKRZ0 (DATA(5))
         SAVE = DATA(6)
         LATC = PAKRZ0 (SAVE)
         X006 = DATA(7)
         Y006 = DATA(8)
         FAC = ONE
         IF (SAVE .LT. ZERO) FAC =-ONE
         IND06 = 0
         IF (DABS(SAVE) .NE. NINTYD) THEN
            IND06 = 1
            CON1 = FAC * LATC
            SINPHI = DSIN (CON1)
            COSPHI = DCOS (CON1)
            MCS = MSFNZ0 (E06,SINPHI,COSPHI)
            TCS = TSFNZ0 (E06,CON1,SINPHI)
         END IF
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON006,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LATC,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,650) A06,ES,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X006,Y006
  650    FORMAT ('0INITIALIZATION PARAMETERS (POLAR STEREOGRAPHIC',
     .           ' PROJECTION)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' LONGITUDE OF Y-AXIS          = ',A1,2I3,F7.3/
     .           ' LATITUDE OF TRUE SCALE       = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A06
         DATA(2) = ES
         SWITCH(6) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                            .  POLYCONIC  .
C ......................................................................
C
  700 CONTINUE
      IF (KSYS.EQ.7.OR.ISYS .EQ. 7) THEN
C
         IERROR = 0
         IF (SWITCH(7).NE.0 .AND. SWITCH(7).EQ.ZONE) RETURN
         SWITCH(7) = 0
         A07 = AZ
         E07 = EZ
         ES07 = ESZ
         E007 = E0Z
         E107 = E1Z
         E207 = E2Z
         E307 = E3Z
         LON007 = PAKRZ0 (DATA(5))
         LAT007 = PAKRZ0 (DATA(6))
         X007 = DATA(7)
         Y007 = DATA(8)
         ML007 = MLFNZ0 (E007,E107,E207,E307,LAT007)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON007,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT007,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,750) A07,ES07,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X007,Y007
  750    FORMAT ('0INITIALIZATION PARAMETERS (POLYCONIC',
     .           ' PROJECTION)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .           ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A07
         DATA(2) = ES07
         SWITCH(7) = ZONE
C
C     LIST STATE PLANE INITIALIZATION PARAMETERS IF NECESSARY
C
         IF (ISYS .EQ. 2) THEN
            IPPARM = ITEMP
            IF (IERROR .NE. 0) RETURN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,470) ZONE, PNAME
            SWITCH(2) = ZONE
            RETURN
         END IF
C
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                        .  EQUIDISTANT CONIC  .
C ......................................................................
C
      IF (ISYS .EQ. 8) THEN
C
         IERROR = 0
         IF (SWITCH(8).NE.0 .AND. SWITCH(8).EQ.ZONE) RETURN
         SWITCH(8) = 0
         A08 = AZ
         E = EZ
         ES = ESZ
         E008 = E0Z
         E108 = E1Z
         E208 = E2Z
         E308 = E3Z
         LAT1 = PAKRZ0 (DATA(3))
         LAT2 = PAKRZ0 (DATA(4))
         IF (DABS(LAT1+LAT2) .LT. EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,840)
  840       FORMAT ('0ERROR PJ08Z0'/
     .              ' EQUAL LATITUDES FOR ST. PARALLELS ON OPPOSITE',
     .              ' SIDES OF EQUATOR')
            IERROR = 081
            RETURN
         END IF
         LON008 = PAKRZ0 (DATA(5))
         LAT0 = PAKRZ0 (DATA(6))
         X008 = DATA(7)
         Y008 = DATA(8)
         SINPHI = DSIN (LAT1)
         COSPHI = DCOS (LAT1)
         MS1 = MSFNZ0 (E,SINPHI,COSPHI)
         ML1 = MLFNZ0 (E008,E108,E208,E308,LAT1)
         IND = 0
         IF (DATA(9) .NE. ZERO) THEN
            IND = 1
            SINPHI = DSIN (LAT2)
            COSPHI = DCOS (LAT2)
            MS2 = MSFNZ0 (E,SINPHI,COSPHI)
            ML2 = MLFNZ0 (E008,E108,E208,E308,LAT2)
            IF (DABS(LAT1-LAT2) .GE. EPSLN) THEN
               NS08 = (MS1 - MS2) / (ML2 - ML1)
            ELSE
               NS08 = SINPHI
            END IF
         ELSE
            NS08 = SINPHI
         END IF
         GL = ML1 + MS1 / NS08
         ML0 = MLFNZ0 (E008,E108,E208,E308,LAT0)
         RH008 = A08 * (GL - ML0)
C
C    LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT2,SGNA(2),DEGS(2),MINS(2),SECS(2))
         CALL RADDZ0 (LON008,SGNA(3),DEGS(3),MINS(3),SECS(3))
         CALL RADDZ0 (LAT0,SGNA(4),DEGS(4),MINS(4),SECS(4))
         IF (IND .NE. 0) THEN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,850) A08,ES,
     .               (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,4),
     .               X008,Y008
  850       FORMAT ('0INITIALIZATION PARAMETERS (EQUIDISTANT CONIC',
     .              ' PROJECTION)'/
     .              ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .              ' ECCENTRICITY SQUARED         =',F12.9/
     .              ' LATITUDE OF 1ST ST. PARALLEL = ',A1,2I3,F7.3/
     .              ' LATITUDE OF 2ND ST. PARALLEL = ',A1,2I3,F7.3/
     .              ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .              ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .              ' FALSE EASTING                =',F12.2,' METERS'/
     .              ' FALSE NORTHING               =',F12.2,' METERS')
         ELSE
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,860) A08,ES,
     .                SGNA(1),DEGS(1),MINS(1),SECS(1),
     .               (SGNA(I),DEGS(I),MINS(I),SECS(I),I=3,4),
     .               X008,Y008
  860       FORMAT ('0INITIALIZATION PARAMETERS (EQUIDISTANT CONIC',
     .              ' PROJECTION)'/
     .              ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .              ' ECCENTRICITY SQUARED         =',F12.9/
     .              ' LATITUDE OF ST. PARALLEL     = ',A1,2I3,F7.3/
     .              ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .              ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .              ' FALSE EASTING                =',F12.2,' METERS'/
     .              ' FALSE NORTHING               =',F12.2,' METERS')
         END IF
         DATA(1) = A08
         DATA(2) = ES
         SWITCH(8) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                       .  TRANSVERSE MERCATOR  .
C ......................................................................
C
  900 CONTINUE
      IF (KSYS.EQ.9.OR.ISYS .EQ. 9) THEN
C
         IERROR = 0
         IF (DATA(1).NE.0.0D0.AND.DATA(1).NE.SAVE) SWITCH(9) = 0
         IF (SWITCH(9).NE.0 .AND. SWITCH(9).EQ.ZONE) RETURN
         SWITCH(9) = 0
         SAVE = DATA(1)
         A09 = AZ
         E09 = EZ
         ES09 = ESZ
         E009 = E0Z
         E109 = E1Z
         E209 = E2Z
         E309 = E3Z
         KS009 = DATA(3)
         LON009 = PAKRZ0 (DATA(5))
         LAT009 = PAKRZ0 (DATA(6))
         X009 = DATA(7)
         Y009 = DATA(8)
         ML009 = A09 * MLFNZ0 (E009,E109,E209,E309,LAT009)
         IND09 = 1
         ESP = ES09
         IF (E09 .GE. TOL09) THEN
            IND09 = 0
            ESP = ES09 / (ONE - ES09)
         END IF
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON009,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT009,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,950) A09,ES09,KS009,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X009,Y009
  950    FORMAT ('0INITIALIZATION PARAMETERS (TRANSVERSE MERCATOR',
     .           ' PROJECTION)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' SCALE FACTOR AT C. MERIDIAN  =',F9.6/
     .           ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .           ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A09
         DATA(2) = ES09
         SWITCH(9) = ZONE
C
C     LIST UTM PROJECTION INITIALIZATION PARAMETERS IF NECESSARY
C
         IF (ISYS .EQ. 1) THEN
            IPPARM = ITEMP
            BUFFL(1) = A09
            BUFFL(2) = ES09
            ZONE = KEEPZN
            SWITCH(9) = ZONE
            IF (IERROR .NE. 0) RETURN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,960) ZONE,BUFFL(1),
     .            BUFFL(2),BUFFL(3),
     .            SGNA(1),DEGS(1),MINS(1),SECS(1),
     .            BUFFL(7),BUFFL(8)
  960          FORMAT ('0INITIALIZATION PARAMETERS (U T M PROJECTION)'/
     .            ' ZONE = ',I3/
     .            ' SEMI-MAJOR AXIS OF ELLIPSOID = ',F12.2,' METERS'/
     .            ' ECCENTRICITY SQUARED         = ',F18.15/
     .            ' SCALE FACTOR AT C. MERIDIAN  = ',F9.6/
     .            ' LONGITUDE OF CENTRAL MERIDIAN= ',A1,2I3,F7.3/
     .            ' FALSE EASTING                = ',F12.2,' METERS'/
     .            ' FALSE NORTHING               = ',F12.2,' METERS')
            SWITCH(1) = ZONE
            RETURN
         END IF
C
C     LIST STATE PLANE INITIALIZATION PARAMETERS IF NECESSARY
C
         IF (ISYS .EQ. 2) THEN
            IPPARM = ITEMP
            IF (IERROR .NE. 0) RETURN
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,470) ZONE, PNAME
            SWITCH(2) = ZONE
            RETURN
         END IF
C
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                          .  STEREOGRAPHIC  .
C ......................................................................
C
      IF (ISYS .EQ. 10) THEN
C
         IERROR = 0
         IF (SWITCH(10).NE.0 .AND. SWITCH(10).EQ.ZONE) RETURN
         SWITCH(10) = 0
         A10 = AZZ
         LON010 = PAKRZ0 (DATA(5))
         LAT010 = PAKRZ0 (DATA(6))
         X010 = DATA(7)
         Y010 = DATA(8)
         SINP10 = DSIN (LAT010)
         COSP10 = DCOS (LAT010)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON010,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT010,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1050) A10,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X010,Y010
 1050    FORMAT ('0INITIALIZATION PARAMETERS (STEREOGRAPHIC',
     .           ' PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .           ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A10
         SWITCH(10) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                   .  LAMBERT AZIMUTHAL EQUAL-AREA  .
C ......................................................................
C
      IF (ISYS .EQ. 11) THEN
C
         IERROR = 0
         IF (SWITCH(11).NE.0 .AND. SWITCH(11).EQ.ZONE) RETURN
         SWITCH(11) = 0
         A11 = AZZ
         LON011 = PAKRZ0 (DATA(5))
         LAT011 = PAKRZ0 (DATA(6))
         X011 = DATA(7)
         Y011 = DATA(8)
         SINP11 = DSIN (LAT011)
         COSP11 = DCOS (LAT011)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON011,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT011,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1150) A11,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X011,Y011
 1150 FORMAT ('0INITIALIZATION PARAMETERS (LAMBERT AZIMUTHAL EQUAL-AREA'
     .          ,' PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .           ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A11
         SWITCH(11) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                      .  AZIMUTHAL EQUIDISTANT  .
C ......................................................................
C
      IF (ISYS .EQ. 12) THEN
C
         IERROR = 0
         IF (SWITCH(12).NE.0 .AND. SWITCH(12).EQ.ZONE) RETURN
         SWITCH(12) = 0
         A12 = AZZ
         LON012 = PAKRZ0 (DATA(5))
         LAT012 = PAKRZ0 (DATA(6))
         X012 = DATA(7)
         Y012 = DATA(8)
         SINP12 = DSIN (LAT012)
         COSP12 = DCOS (LAT012)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON012,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT012,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1250) A12,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X012,Y012
 1250    FORMAT ('0INITIALIZATION PARAMETERS (AZIMUTHAL EQUIDISTANT',
     .           ' PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .           ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A12
         SWITCH(12) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                            .  GNOMONIC  .
C ......................................................................
C
      IF (ISYS .EQ. 13) THEN
C
         IERROR = 0
         IF (SWITCH(13).NE.0 .AND. SWITCH(13).EQ.ZONE) RETURN
         SWITCH(13) = 0
         A13 = AZZ
         LON013 = PAKRZ0 (DATA(5))
         LAT013 = PAKRZ0 (DATA(6))
         X013 = DATA(7)
         Y013 = DATA(8)
         SINP13 = DSIN (LAT013)
         COSP13 = DCOS (LAT013)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON013,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT013,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1350) A13,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X013,Y013
 1350    FORMAT ('0INITIALIZATION PARAMETERS (GNOMONIC',
     .           ' PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .           ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A13
         SWITCH(13) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                          .  ORTHOGRAPHIC  .
C ......................................................................
C
      IF (ISYS .EQ. 14) THEN
C
         IERROR = 0
         IF (SWITCH(14).NE.0 .AND. SWITCH(14).EQ.ZONE) RETURN
         SWITCH(14) = 0
         A14 = AZZ
         LON014 = PAKRZ0 (DATA(5))
         LAT014 = PAKRZ0 (DATA(6))
         X014 = DATA(7)
         Y014 = DATA(8)
         SINP14 = DSIN (LAT014)
         COSP14 = DCOS (LAT014)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON014,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT014,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1450) A14,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X014,Y014
 1450    FORMAT ('0INITIALIZATION PARAMETERS (ORTHOGRAPHIC',
     .           ' PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .           ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A14
         SWITCH(14) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C             .  GENERAL VERTICAL NEAR-SIDE PERSPECTIVE  .
C ......................................................................
C
      IF (ISYS .EQ. 15) THEN
C
         IERROR = 0
         IF (SWITCH(15).NE.0 .AND. SWITCH(15).EQ.ZONE) RETURN
         SWITCH(15) = 0
         A15 = AZZ
         P = ONE + DATA(3) / A15
         LON015 = PAKRZ0 (DATA(5))
         LAT015 = PAKRZ0 (DATA(6))
         X015 = DATA(7)
         Y015 = DATA(8)
         SINP15 = DSIN (LAT015)
         COSP15 = DCOS (LAT015)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON015,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT015,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1550) A15,DATA(3),
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X015,Y015
 1550 FORMAT ('0INITIALIZATION PARAMETERS (GENERAL VERTICAL NEAR-SIDE',
     .           ' PERSPECTIVE PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' HEIGHT OF PERSPECTIVE POINT'/
     .           ' ABOVE SPHERE                 =',F12.2,' METERS'/
     .           ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .           ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A15
         SWITCH(15) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                           .  SINUSOIDAL  .
C ......................................................................
C
      IF (ISYS .EQ. 16) THEN
C
         IERROR = 0
         IF (SWITCH(16).NE.0 .AND. SWITCH(16).EQ.ZONE) RETURN
         SWITCH(16) = 0
         A16 = AZZ
         LON016 = PAKRZ0 (DATA(5))
         X016 = DATA(7)
         Y016 = DATA(8)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON016,SGNA(1),DEGS(1),MINS(1),SECS(1))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1650) A16,
     .            SGNA(1),DEGS(1),MINS(1),SECS(1),
     .            X016,Y016
 1650    FORMAT ('0INITIALIZATION PARAMETERS (SINUSOIDAL',
     .           ' PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A16
         SWITCH(16) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                         .  EQUIRECTANGULAR  .
C ......................................................................
C
      IF (ISYS .EQ. 17) THEN
C
         IERROR = 0
         IF (SWITCH(17).NE.0 .AND. SWITCH(17).EQ.ZONE) RETURN
         SWITCH(17) = 0
         A17 = AZZ
         LAT1 = PAKRZ0 (DATA(6))
         LON017 = PAKRZ0 (DATA(5))
         X017 = DATA(7)
         Y017 = DATA(8)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LAT1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LON017,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1750) A17,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X017,Y017
 1750 FORMAT ('0INITIALIZATION PARAMETERS (EQUIRECTANGULAR PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LATITUDE OF TRUE SCALE       = ',A1,2I2,F7.3/
     .           ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A17
         SWITCH(17) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                       .  MILLER CYLINDRICAL  .
C ......................................................................
C
      IF (ISYS .EQ. 18) THEN
C
         IERROR = 0
         IF (SWITCH(18).NE.0 .AND. SWITCH(18).EQ.ZONE) RETURN
         SWITCH(18) = 0
         A18 = AZZ
         LON018 = PAKRZ0 (DATA(5))
         X018 = DATA(7)
         Y018 = DATA(8)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON018,SGNA(1),DEGS(1),MINS(1),SECS(1))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1850) A18,
     .             SGNA(1),DEGS(1),MINS(1),SECS(1),
     .             X018,Y018
 1850    FORMAT ('0INITIALIZATION PARAMETERS (MILLER CYLINDRICAL',
     .           ' PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A18
         SWITCH(18) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                        .  VAN DER GRINTEN I  .
C ......................................................................
C
      IF (ISYS .EQ. 19) THEN
C
         IERROR = 0
         IF (SWITCH(19).NE.0 .AND. SWITCH(19).EQ.ZONE) RETURN
         SWITCH(19) = 0
         A19 = AZZ
         LON019 = PAKRZ0 (DATA(5))
         X019 = DATA(7)
         Y019 = DATA(8)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON019,SGNA(1),DEGS(1),MINS(1),SECS(1))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,1950) A19,
     .             SGNA(1),DEGS(1),MINS(1),SECS(1),
     .             X019,Y019
 1950    FORMAT ('0INITIALIZATION PARAMETERS (VAN DER GRINTEN I',
     .           ' PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A19
         SWITCH(19) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                    .  OBLIQUE MERCATOR (HOTINE)  .
C ......................................................................
C
 2000 CONTINUE
      IF (KSYS.EQ.20.OR.ISYS .EQ. 20) THEN
C
         IERROR = 0
         IF (SWITCH(20).NE.0 .AND. SWITCH(20).EQ.ZONE) RETURN
         SWITCH(20) = 0
         MODE = 0
         IF (DATA(13) .NE. ZERO) MODE = 1
         A = AZ
         E20 = EZ
         ES = ESZ
         KS0 = DATA(3)
         LAT0 = PAKRZ0 (DATA(6))
         X020 = DATA(7)
         Y020 = DATA(8)
         SINPH0 = DSIN (LAT0)
         COSPH0 = DCOS (LAT0)
         CON = ONE - ES * SINPH0 * SINPH0
         COM = DSQRT (ONE - ES)
         BL = DSQRT (ONE + ES * COSPH0 ** 4 / (ONE - ES))
         AL = A * BL * KS0 * COM / CON
         IF (DABS(LAT0).LT.EPSLN) TS0 = 1.0D0
         IF (DABS(LAT0).LT.EPSLN) D=1.0D0
         IF (DABS(LAT0).LT.EPSLN) EL=1.0D0
         IF (DABS(LAT0).GE.EPSLN) THEN
            TS0 = TSFNZ0 (E20,LAT0,SINPH0)
            CON = DSQRT (CON)
            D = BL * COM / (COSPH0 * CON)
            F = D + DSIGN (DSQRT (DMAX1 ((D * D - ONE), 0.0D0)) , LAT0)
            EL = F * TS0 ** BL
         END IF
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2050) A,ES,KS0
 2050 FORMAT ('0INITIALIZATION PARAMETERS (OBLIQUE MERCATOR ''HOTINE''',
     .           ' PROJECTION)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' SCALE AT CENTER              =',F12.9)
         IF (MODE .NE. 0) THEN
            ALPHA = PAKRZ0 (DATA(4))
            LONC = PAKRZ0 (DATA(5))
            G = HALF * (F - ONE / F)
            GAMMA = ASINZ0 (DSIN (ALPHA) / D)
            LON020 = LONC - ASINZ0 (G * DTAN (GAMMA)) / BL
C
C     LIST INITIALIZATION PARAMETERS (CASE B).
C
            CALL RADDZ0 (ALPHA,SGNA(1),DEGS(1),MINS(1),SECS(1))
            CALL RADDZ0 (LONC,SGNA(2),DEGS(2),MINS(2),SECS(2))
            CALL RADDZ0 (LAT0,SGNA(3),DEGS(3),MINS(3),SECS(3))
            IF (IPPARM .EQ. 0) WRITE (IPPLUN,2060)
     .               (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,3)
 2060       FORMAT (' AZIMUTH OF CENTRAL LINE      = ',A1,2I3,F7.3/
     .              ' LONGITUDE OF ORIGIN          = ',A1,2I3,F7.3/
     .              ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3)
            CON = DABS (LAT0)
            IF (CON.GT.EPSLN .AND. DABS(CON - HALFPI).GT.EPSLN) THEN
               SINGAM = DSIN (GAMMA)
               COSGAM = DCOS (GAMMA)
               SINALF = DSIN (ALPHA)
               COSALF = DCOS (ALPHA)
               U0 = DSIGN((AL/BL)*DATAN(DSQRT(D*D-ONE)/COSALF),LAT0)
               IF (IPPARM .EQ. 0) WRITE (IPPLUN,2080) X020,Y020
               DATA(1) = A
               DATA(2) = ES
               SWITCH(20) = ZONE
C
C     LIST STATE PLANE INITIALIZATION PARAMETERS IF NECESSARY
C
               IF (ISYS .EQ. 2) THEN
                  IPPARM = ITEMP
                  IF (IERROR .NE. 0) RETURN
                  IF (IPPARM .EQ. 0) WRITE (IPPLUN,470) ZONE, PNAME
                  SWITCH(2) = ZONE
                  RETURN
               END IF
C
               RETURN
            ELSE
               IF (IPEMSG .EQ. 0) WRITE (IPELUN,2040)
 2040          FORMAT ('0ERROR PJ20Z0'/
     .                 ' INPUT DATA ERROR')
               IERROR = 201
               RETURN
            END IF
         END IF
         LON1 = PAKRZ0 (DATA(9))
         LAT1 = PAKRZ0 (DATA(10))
         LON2 = PAKRZ0 (DATA(11))
         LAT2 = PAKRZ0 (DATA(12))
         SINPHI = DSIN (LAT1)
         TS1 = TSFNZ0 (E20,LAT1,SINPHI)
         SINPHI = DSIN (LAT2)
         TS2 = TSFNZ0 (E20,LAT2,SINPHI)
         H = TS1 ** BL
         L = TS2 ** BL
         F = EL / H
         G = HALF * (F - ONE / F)
         J = (EL * EL - L * H) / (EL * EL + L * H)
         P = (L - H) / (L + H)
         CALL RADDZ0 (LON2,SGNA(3),DEGS(3),MINS(3),SECS(3))
         DLON = LON1 - LON2
         IF (DLON .LT. -PI) LON2 = LON2 - 2.D0 * PI
         IF (DLON .GT.  PI) LON2 = LON2 + 2.D0 * PI
         DLON = LON1 - LON2
         LON020 = HALF * (LON1 + LON2) - DATAN (J * DTAN (HALF * BL *
     .          DLON) / P) / BL
         DLON = ADJLZ0 (LON1 - LON020)
         GAMMA = DATAN (DSIN (BL * DLON) / G)
         ALPHA = ASINZ0 (D * DSIN (GAMMA))
         CALL RADDZ0 (LON1,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT1,SGNA(2),DEGS(2),MINS(2),SECS(2))
         CALL RADDZ0 (LAT2,SGNA(4),DEGS(4),MINS(4),SECS(4))
         CALL RADDZ0 (LAT0,SGNA(5),DEGS(5),MINS(5),SECS(5))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2070)
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,5)
 2070    FORMAT (' LONGITUDE OF 1ST POINT       = ',A1,2I3,F7.3/
     .           ' LATITUDE OF 1ST POINT        = ',A1,2I3,F7.3/
     .           ' LONGITUDE OF 2ND POINT       = ',A1,2I3,F7.3/
     .           ' LATITUDE OF 2ND POINT        = ',A1,2I3,F7.3/
     .           ' LATITUDE OF ORIGIN           = ',A1,2I3,F7.3)
         IF (DABS(LAT1 - LAT2) .LE. EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2040)
            IERROR = 202
            RETURN
        ELSE
            CON = DABS (LAT1)
         END IF
         IF (CON.LE.EPSLN .OR. DABS(CON - HALFPI).LE.EPSLN) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2040)
            IERROR = 202
            RETURN
         ELSE
            IF (DABS(DABS(LAT0) - HALFPI) .LE. EPSLN) THEN
               IF (IPEMSG .EQ. 0) WRITE (IPELUN,2040)
               IERROR = 202
               RETURN
            END IF
         END IF
         SINGAM = DSIN (GAMMA)
         COSGAM = DCOS (GAMMA)
         SINALF = DSIN (ALPHA)
         COSALF = DCOS (ALPHA)
         U0 = DSIGN((AL/BL)*DATAN(DSQRT(D*D-ONE)/COSALF),LAT0)
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2080) X020,Y020
 2080    FORMAT (' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A
         DATA(2) = ES
         SWITCH(20) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                       .       ROBINSON       .
C ......................................................................
C
      IF (ISYS .EQ. 21) THEN
C
         IERROR = 0
         IF (SWITCH(21).NE.0 .AND. SWITCH(21).EQ.ZONE) RETURN
         SWITCH(21) = 0
         A21 = AZZ
         LON021 = PAKRZ0 (DATA(5))
         X021 = DATA(7)
         Y021 = DATA(8)
         PR(1)=-0.062D0
         XLR(1)=0.9986D0
         PR(2)=0.D0
         XLR(2)=1.D0
         PR(3)=0.062D0
         XLR(3)=0.9986D0
         PR(4)=0.124D0
         XLR(4)=0.9954D0
         PR(5)=0.186D0
         XLR(5)=0.99D0
         PR(6)=0.248D0
         XLR(6)=0.9822D0
         PR(7)=0.31D0
         XLR(7)=0.973D0
         PR(8)=0.372D0
         XLR(8)=0.96D0
         PR(9)=0.434D0
         XLR(9)=0.9427D0
         PR(10)=0.4958D0
         XLR(10)=0.9216D0
         PR(11)=0.5571D0
         XLR(11)=0.8962D0
         PR(12)=0.6176D0
         XLR(12)=0.8679D0
         PR(13)=0.6769D0
         XLR(13)=0.835D0
         PR(14)=0.7346D0
         XLR(14)=0.7986D0
         PR(15)=0.7903D0
         XLR(15)=0.7597D0
         PR(16)=0.8435D0
         XLR(16)=0.7186D0
         PR(17)=0.8936D0
         XLR(17)=0.6732D0
         PR(18)=0.9394D0
         XLR(18)=0.6213D0
         PR(19)=0.9761D0
         XLR(19)=0.5722D0
         PR(20)=1.0D0
         XLR(20)=0.5322D0
         DO 2110 I=1,20
 2110    XLR(I)=XLR(I) * 0.9858D0
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON021,SGNA(1),DEGS(1),MINS(1),SECS(1))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2150) A21,
     .             SGNA(1),DEGS(1),MINS(1),SECS(1),
     .             X021,Y021
 2150    FORMAT ('0INITIALIZATION PARAMETERS (ROBINSON',
     .           ' PROJECTION)'/
     .           ' RADIUS OF SPHERE             =',F12.2,' METERS'/
     .           ' LONGITUDE OF C. MERIDIAN     = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A21
         SWITCH(21) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C                      .  SPACE OBLIQUE MERCATOR  .
C ......................................................................
C
      IF (ISYS .EQ. 22) THEN
C
         IERROR = 0
         IF (SWITCH(22).NE.0 .AND. SWITCH(22).EQ.ZONE) RETURN
         SWITCH(22) = 0
         A22 = AZ
         E = EZ
         ES22 = ESZ
         X022 = DATA(7)
         Y022 = DATA(8)
         LAND = IDINT(DATA(3)+TOL)
         PATH = IDINT(DATA(4)+TOL)
C
C        CHECK IF LANDSAT NUMBER IS WITHIN RANGE 1 - 5
C
         IF (LAND .GT. 0 .AND. LAND .LE. 5) THEN
            IF (LAND .LE. 3) LIMIT = 251
            IF (LAND .GE. 4) LIMIT = 233
         ELSE
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2240) LAND, PATH
            IERROR = 221
            RETURN
         END IF
C
C        CHECK IF PATH NUMBER IS WITHIN RANGE 1 - 251 FOR LANDSATS 1 - 3
C        OR RANGE 1 - 233 FOR LANDSATS 4 - 5
C
         IF (PATH .LE. 0 .OR. PATH .GT. LIMIT) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2240) LAND, PATH
 2240       FORMAT ('0ERROR PJ22Z0'/
     .              ' LANDSAT NUMBER ',I2,' AND / OR PATH NUMBER ',I4,
     .              ' ARE OUT OF RANGE')
            IERROR = 221
            RETURN
         END IF
         P1=1440.0D0
         IF (LAND.LE.3) THEN
            P2=103.2669323D0
            ALF=99.092D0*DG1
         ELSE
            P2=98.8841202D0
            ALF=98.20D0*DG1
         END IF
         SA=DSIN(ALF)
         CA=DCOS(ALF)
         IF (DABS(CA).LT.1.D-9) CA=1.D-9
         ESC=ES22*CA*CA
         ESS=ES22*SA*SA
         W=((ONE-ESC)/(ONE-ES22))**TWO-ONE
         Q=ESS/(ONE-ES22)
         T=(ESS*(TWO-ES22))/(ONE-ES22)**TWO
         U=ESC/(ONE-ES22)
         XJ=(ONE-ES22)**3
         P22=P2/P1
C
C        COMPUTE FOURIER COEFFICIENTS.  LAM IS CURRENT VALUE OF
C        LAMBDA DOUBLE-PRIME.
C
         LAM=0
         CALL SERAZ0 (FB,FA2,FA4,FC1,FC3,LAM)
         SUMA2=FA2
         SUMA4=FA4
         SUMB=FB
         SUMC1=FC1
         SUMC3=FC3
         DO 2210 I=9,81,18
         LAM=DBLE(I)
         CALL SERAZ0 (FB,FA2,FA4,FC1,FC3,LAM)
         SUMA2=SUMA2+4.0D0*FA2
         SUMA4=SUMA4+4.0D0*FA4
         SUMB=SUMB+4.0D0*FB
         SUMC1=SUMC1+4.0D0*FC1
         SUMC3=SUMC3+4.0D0*FC3
 2210    CONTINUE
         DO 2220 I=18,72,18
         LAM=DBLE(I)
         CALL SERAZ0 (FB,FA2,FA4,FC1,FC3,LAM)
         SUMA2=SUMA2+TWO*FA2
         SUMA4=SUMA4+TWO*FA4
         SUMB=SUMB+TWO*FB
         SUMC1=SUMC1+TWO*FC1
         SUMC3=SUMC3+TWO*FC3
 2220    CONTINUE
         LAM=90.0D0
         CALL SERAZ0 (FB,FA2,FA4,FC1,FC3,LAM)
         SUMA2=SUMA2+FA2
         SUMA4=SUMA4+FA4
         SUMB=SUMB+FB
         SUMC1=SUMC1+FC1
         SUMC3=SUMC3+FC3
C
C        THESE ARE THE VALUES OF FOURIER CONSTANTS.
C
         A2=SUMA2/30.D0
         A4=SUMA4/60.D0
         B=SUMB/30.D0
         C1=SUMC1/15.D0
         C3=SUMC3/45.D0
C
C        LIST RESULTS OF PARAMETER INITIALIZATION.
C
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2250) A22,ES22,LAND,PATH,
     .                                          X022,Y022
 2250    FORMAT ('0INITIALIZATION PARAMETERS (SPACE OBL. MERCATOR',
     .           ' PROJECTION)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' LANDSAT NO.                  = ',I3/
     .           ' PATH                         = ',I5/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS'/)
         DATA(1) = A22
         DATA(2) = ES22
         SWITCH(22) = ZONE
         RETURN
      END IF
C
C ......................................................................
C             .  INITIALIZATION OF PROJECTION PARAMETERS  .
C
C          .  MODIFIED-STEREOGRAPHIC CONFORMAL (FOR ALASKA)  .
C ......................................................................
C
      IF (ISYS .EQ. 23) THEN
C
         IERROR = 0
         IF (SWITCH(23).NE.0 .AND. SWITCH(23).EQ.ZONE) RETURN
         SWITCH(23) = 0
         A23 = AZ
         EC2 = 0.6768657997291094D-02
         EC  = DSQRT (EC2)
         N=6
         LON023 = -152.0D0*DG1
         LAT023 = 64.0D0*DG1
         X023 = DATA(7)
         Y023 = DATA(8)
         ACOEF(1)=0.9945303D0
         ACOEF(2)=0.0052083D0
         ACOEF(3)=0.0072721D0
         ACOEF(4)=-0.0151089D0
         ACOEF(5)=0.0642675D0
         ACOEF(6)=0.3582802D0
         BCOEF(1)=0.0D0
         BCOEF(2)=-.0027404D0
         BCOEF(3)=0.0048181D0
         BCOEF(4)=-0.1932526D0
         BCOEF(5)=-0.1381226D0
         BCOEF(6)=-0.2884586D0
         ESPHI=EC*DSIN(LAT023)
         CHIO=TWO*DATAN(DTAN((HALFPI+LAT023)/TWO)*((ONE-ESPHI)/
     .       (ONE+ESPHI))**(EC/TWO)) - HALFPI
         SCHIO=DSIN(CHIO)
         CCHIO=DCOS(CHIO)
C
C     LIST RESULTS OF PARAMETER INITIALIZATION.
C
         CALL RADDZ0 (LON023,SGNA(1),DEGS(1),MINS(1),SECS(1))
         CALL RADDZ0 (LAT023,SGNA(2),DEGS(2),MINS(2),SECS(2))
         IF (IPPARM .EQ. 0) WRITE (IPPLUN,2350) A23,EC2,
     .            (SGNA(I),DEGS(I),MINS(I),SECS(I),I=1,2),
     .            X023,Y023
 2350    FORMAT ('0INITIALIZATION PARAMETERS (MOD. STEREOGRAPHIC',
     .           ' CONFORMAL PROJECTION, ALASKA)'/
     .           ' SEMI-MAJOR AXIS OF ELLIPSOID =',F12.2,' METERS'/
     .           ' ECCENTRICITY SQUARED         =',F12.9/
     .           ' LONGITUDE OF CENTER          = ',A1,2I3,F7.3/
     .           ' LATITUDE  OF CENTER          = ',A1,2I3,F7.3/
     .           ' FALSE EASTING                =',F12.2,' METERS'/
     .           ' FALSE NORTHING               =',F12.2,' METERS')
         DATA(1) = A23
         SWITCH(23) = ZONE
         RETURN
      END IF
C
C     INITIALIZATION OF PROJECTION COMPLETED
C
      END
C                   PJ01Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C                              *  U T M  *
C **********************************************************************
C
      SUBROUTINE PJ01Z0 (COORD,CRDIO,INDIC)
C
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC, FWD, INV
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /TOGGLE/ SWITCH
      PARAMETER (FWD=0, INV=1)
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(1) .NE. 0) GO TO 140
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT ('0ERROR PJ01Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 013
         RETURN
  140    CALL PJ09Z0 (GEOG,PROJ,FWD)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(1) .NE. 0) GO TO 160
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
         IERROR = 014
         RETURN
  160    CALL PJ09Z0 (PROJ,GEOG,INV)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ02Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C                           *  STATE PLANE  *
C **********************************************************************
C
      SUBROUTINE PJ02Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23), ITYPE
      INTEGER*2 INDIC, FWD, INV
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ02/ ITYPE
      COMMON /TOGGLE/ SWITCH
C
      PARAMETER (FWD=0, INV=1)
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(2) .EQ. 0) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,250)
  250       FORMAT ('0ERROR PJ02Z0'/
     .              ' PROJECTION WAS NOT INITIALIZED')
            IERROR = 023
            RETURN
         END IF
C
C     TRANSVERSE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 1) THEN
            CALL PJ09Z0 (GEOG,PROJ,FWD)
         END IF
C
C     LAMBERT CONFORMAL PROJECTION
C
         IF (ITYPE .EQ. 2) THEN
            CALL PJ04Z0 (GEOG,PROJ,FWD)
         END IF
C
C     POLYCONIC PROJECTION
C
         IF (ITYPE .EQ. 3) THEN
            CALL PJ07Z0 (GEOG,PROJ,FWD)
         END IF
C
C     OBLIQUE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 4) THEN
            CALL PJ20Z0 (GEOG,PROJ,FWD)
         END IF
C
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(2) .EQ. 0) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,250)
            IERROR = 025
            RETURN
         END IF
C
C     TRANSVERSE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 1) THEN
            CALL PJ09Z0 (PROJ,GEOG,INV)
         END IF
C
C     LAMBERT CONFORMAL PROJECTION
C
         IF (ITYPE .EQ. 2) THEN
            CALL PJ04Z0 (PROJ,GEOG,INV)
         END IF
C
C     POLYCONIC PROJECTION
C
         IF (ITYPE .EQ. 3) THEN
            CALL PJ07Z0 (PROJ,GEOG,INV)
         END IF
C
C     OBLIQUE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 4) THEN
            CALL PJ20Z0 (PROJ,GEOG,INV)
         END IF
C
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ03Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                    *  ALBERS CONICAL EQUAL AREA  *
C **********************************************************************
C
      SUBROUTINE PJ03Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,NS,C,RH0 *******
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ03/ A,LON0,X0,Y0,C,E,ES,NS,RH0
      COMMON /TOGGLE/ SWITCH
      DATA TOL /1.0D-7/
      DATA HALFPI /1.5707963267948966D0/
      DATA ZERO,HALF,ONE /0.0D0,0.5D0,1.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(3) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT ('0ERROR PJ03Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 033
         RETURN
  220    SINPHI = DSIN (GEOG(2))
         COSPHI = DCOS (GEOG(2))
         QS = QSFNZ0 (E,SINPHI,COSPHI)
         RH = A * DSQRT (C - NS * QS) / NS
         THETA = NS * ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + RH * DSIN (THETA)
         PROJ(2) = Y0 + RH0 - RH * DCOS (THETA)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(3) .NE. 0) GO TO 240
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
         IERROR = 034
         RETURN
  240    X = PROJ(1) - X0
         Y = RH0 - PROJ(2) + Y0
         RH = DSIGN (DSQRT (X * X + Y * Y) , NS)
         THETA = ZERO
         CON = DSIGN (ONE , NS)
         IF (RH .NE. ZERO) THETA = DATAN2 (CON * X , CON * Y)
         CON = RH * NS / A
         QS = (C - CON * CON) / NS
         IF (E .LT. TOL) GO TO 260
         CON = ONE - HALF * (ONE - ES) * DLOG ((ONE - E) /
     .         (ONE + E)) / E
         IF ((DABS(CON) - DABS(QS)) .GT. TOL) GO TO 260
         GEOG(2) = DSIGN (HALFPI , QS)
         GO TO 280
  260    GEOG(2) = PHI1Z0 (E,QS)
         IF (IERROR .EQ. 0) GO TO 280
         IERROR = 035
         RETURN
  280    GEOG(1) = ADJLZ0 (THETA / NS + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ04Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                     *  LAMBERT CONFORMAL CONIC  *
C **********************************************************************
C
      SUBROUTINE PJ04Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,NS,F,RH0 *******
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ04/ A,LON0,X0,Y0,E,F,NS,RH0
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE /0.0D0,1.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(4) .NE. 0) GO TO 200
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT ('0ERROR PJ04Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 043
         RETURN
  200    CON = DABS (DABS (GEOG(2)) - HALFPI)
         IF (CON .GT. EPSLN) GO TO 220
         CON = GEOG(2) * NS
         IF (CON .GT. ZERO) GO TO 210
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT ('0ERROR PJ04Z0'/
     .           ' POINT CANNOT BE PROJECTED')
         IERROR = 044
         RETURN
  210    RH = ZERO
         GO TO 230
  220    SINPHI = DSIN (GEOG(2))
         TS = TSFNZ0 (E,GEOG(2),SINPHI)
         RH = A * F * TS ** NS
  230    THETA = NS * ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + RH * DSIN (THETA)
         PROJ(2) = Y0 + RH0 - RH * DCOS (THETA)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(4) .NE. 0) GO TO 240
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
         IERROR = 045
         RETURN
  240    X = PROJ(1) - X0
         Y = RH0 - PROJ(2) + Y0
         RH = DSIGN (DSQRT (X*X + Y*Y) , NS)
         THETA = ZERO
         CON = DSIGN (ONE , NS)
         IF (RH .NE. ZERO) THETA = DATAN2 (CON * X , CON * Y)
         IF (RH.NE.ZERO .OR. NS.GT.ZERO) GO TO 250
         GEOG(2) = - HALFPI
         GO TO 260
  250    CON = ONE / NS
         TS = (RH / (A * F)) ** CON
         GEOG(2) = PHI2Z0 (E,TS)
         IF (IERROR .EQ. 0) GO TO 260
         IERROR = 046
         RETURN
  260    GEOG(1) = ADJLZ0 (THETA / NS + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ05Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                            *  MERCATOR  *
C **********************************************************************
C
      SUBROUTINE PJ05Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,X0,Y0,NS,F,RH0,LAT1,M1 **************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ05/ A,LON0,X0,Y0,E,M1
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(5) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ05Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 052
         RETURN
  220    IF (DABS(DABS(GEOG(2)) - HALFPI) .GT. EPSLN) GO TO 240
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT ('0ERROR PJ05Z0'/
     .           ' TRANSFORMATION CANNOT BE COMPUTED AT THE POLES')
         IERROR = 053
         RETURN
  240    SINPHI = DSIN (GEOG(2))
         TS = TSFNZ0 (E,GEOG(2),SINPHI)
         PROJ(1) = X0 + A * M1 * ADJLZ0 (GEOG(1) - LON0)
         PROJ(2) = Y0 - A * M1 * DLOG (TS)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(5) .NE. 0) GO TO 260
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 054
         RETURN
  260    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         TS = DEXP (- Y / (A * M1))
         GEOG(2) = PHI2Z0 (E,TS)
         IF (IERROR .EQ. 0) GO TO 280
         IERROR = 055
         RETURN
  280    GEOG(1) = ADJLZ0 (LON0 + X / (A * M1))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ06Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                       *  POLAR STEREOGRAPHIC  *
C **********************************************************************
C
      SUBROUTINE PJ06Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23),IND
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,LATC,X0,Y0,E4,MCS,TCS,FAC,IND *******
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ06/ A,LON0,X0,Y0,E,E4,FAC,MCS,TCS,IND
      COMMON /TOGGLE/ SWITCH
      DATA ZERO,ONE,TWO /0.0D0,1.0D0,2.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(6) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ06Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 062
         RETURN
  220    CON1 = FAC * ADJLZ0 (GEOG(1) - LON0)
         CON2 = FAC * GEOG(2)
         SINPHI = DSIN (CON2)
         TS = TSFNZ0 (E,CON2,SINPHI)
         IF (IND .EQ. 0) GO TO 240
         RH = A * MCS * TS / TCS
         GO TO 260
  240    RH = TWO * A * TS / E4
  260    PROJ(1) = X0 + FAC * RH * DSIN (CON1)
         PROJ(2) = Y0 - FAC * RH * DCOS (CON1)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(6) .NE. 0) GO TO 320
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 063
         RETURN
  320    X = FAC * (PROJ(1) - X0)
         Y = FAC * (PROJ(2) - Y0)
         RH = DSQRT (X * X + Y * Y)
         IF (IND .EQ. 0) GO TO 340
         TS = RH * TCS / (A * MCS)
         GO TO 360
  340    TS = RH * E4 / (TWO * A)
  360    GEOG(2) = FAC * PHI2Z0 (E,TS)
         IF (IERROR .EQ. 0) GO TO 380
         IERROR = 064
         RETURN
  380    IF (RH .NE. ZERO) GO TO 400
         GEOG(1) = FAC * LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  400    GEOG(1) = ADJLZ0 (FAC * DATAN2 (X , -Y) + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ07Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                            *  POLYCONIC  *
C **********************************************************************
C
      SUBROUTINE PJ07Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,LAT0,X0,Y0,E0,E1,E2,ML0 *************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ07/ A,LON0,X0,Y0,E,E0,E1,E2,E3,ES,ML0
      COMMON /TOGGLE/ SWITCH
      DATA TOL /1.0D-7/
      DATA ZERO,ONE /0.0D0,1.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(7) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ07Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 072
         RETURN
  220    CON = ADJLZ0 (GEOG(1) - LON0)
         IF (DABS(GEOG(2)) .GT. TOL) GO TO 240
         PROJ(1) = X0 + A * CON
         PROJ(2) = Y0 - A * ML0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
  240    SINPHI = DSIN (GEOG(2))
         COSPHI = DCOS (GEOG(2))
         ML = MLFNZ0 (E0,E1,E2,E3,GEOG(2))
         MS = MSFNZ0 (E,SINPHI,COSPHI)
         CON = CON * SINPHI
         PROJ(1) = X0 + A * MS * DSIN (CON) / SINPHI
         PROJ(2) = Y0 + A * (ML - ML0 + MS * (ONE - DCOS(CON)) / SINPHI)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(7) .NE. 0) GO TO 320
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 073
         RETURN
  320    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         AL = ML0 + Y / A
         IF (DABS (AL) .GT. TOL) GO TO 340
         GEOG(1) = X / A + LON0
         GEOG(2) = ZERO
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  340    B = AL * AL + (X / A) ** 2
         CALL PHI4Z0 (ES,E0,E1,E2,E3,AL,B,C,GEOG(2))
         IF (IERROR .EQ. 0) GO TO 360
         IERROR = 074
         RETURN
  360    GEOG(1) = ADJLZ0 (ASINZ0 (X * C / A) / DSIN (GEOG(2)) + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ08Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                        *  EQUIDISTANT CONIC  *
C **********************************************************************
C
      SUBROUTINE PJ08Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C ** PARAMETERS * A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,E0,E1,E2,E3,NS,GL,RH0
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ08/ A,LON0,X0,Y0,E0,E1,E2,E3,GL,NS,RH0
      COMMON /TOGGLE/ SWITCH
      DATA ZERO,ONE /0.0D0,1.0D0/
      DATA EPSLN /1.0D-10/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(8) .NE. 0) GO TO 300
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT ('0ERROR PJ08Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 083
         RETURN
  300    ML = MLFNZ0 (E0,E1,E2,E3,GEOG(2))
         RH = A * (GL - ML)
         THETA = NS * ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + RH * DSIN (THETA)
         PROJ(2) = Y0 + RH0 - RH * DCOS (THETA)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(8) .NE. 0) GO TO 320
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
         IERROR = 084
         RETURN
  320    X = PROJ(1) - X0
         Y = RH0 - PROJ(2) + Y0
         RH = DSIGN (DSQRT (X * X + Y * Y) , NS)
         THETA = ZERO
         CON = DSIGN (ONE , NS)
         IF (RH .NE. ZERO) THETA = DATAN2 (CON * X , CON * Y)
         ML = GL - RH / A
         GEOG(2) = PHI3Z0 (ML,E0,E1,E2,E3)
         IF (IERROR .EQ. 0) GO TO 340
         IERROR = 085
         RETURN
  340    GEOG(1) = ADJLZ0 (LON0 + THETA / NS)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ09Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                       *  TRANSVERSE MERCATOR  *
C **********************************************************************
C
      SUBROUTINE PJ09Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23),I,IND,NIT
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS ** A,E,ES,KS0,LON0,LAT0,X0,Y0,E0,E1,E2,E3,ESP,ML0,IND
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ09/ A,LON0,X0,Y0,ES,ESP,E0,E1,E2,E3,KS0,LAT0,ML0,IND
      COMMON /TOGGLE/ SWITCH
      DATA ZERO,HALF,ONE,TWO,THREE /0.0D0,0.5D0,1.0D0,2.0D0,3.0D0/
      DATA FOUR,FIVE,SIX,EIGHT,NINE /4.0D0,5.0D0,6.0D0,8.0D0,9.0D0/
      DATA HALFPI /1.5707963267948966D0/
      DATA TEN /10.0D0/
      DATA EPSLN,NIT /1.0D-10,6/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(9) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ09Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 092
         RETURN
  220    DLON = ADJLZ0 (GEOG(1) - LON0)
         LAT = GEOG(2)
         IF (IND .EQ. 0) GO TO 240
         COSPHI = DCOS (LAT)
         B = COSPHI * DSIN (DLON)
         IF (DABS(DABS(B) - ONE) .GT. EPSLN) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT ('0ERROR PJ09Z0'/
     .           ' POINT PROJECTS INTO INFINITY')
         IERROR = 093
         RETURN
  230    PROJ(1) = HALF * A * KS0 * DLOG ((ONE + B) / (ONE - B)) + X0
         CON = DACOS (COSPHI * DCOS (DLON) / DSQRT (ONE - B * B))
         IF (LAT .LT. ZERO) CON =-CON
         PROJ(2) = A * KS0 * (CON - LAT0) + Y0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
C
  240    SINPHI = DSIN (LAT)
         COSPHI = DCOS (LAT)
         AL = COSPHI * DLON
         ALS = AL * AL
         C = ESP * COSPHI * COSPHI
         TQ = DTAN (LAT)
         T = TQ * TQ
         N = A / DSQRT (ONE - ES * SINPHI * SINPHI)
         ML = A * MLFNZ0 (E0,E1,E2,E3,LAT)
         PROJ(1) = KS0 * N * AL * (ONE + ALS / SIX * (ONE - T + C +
     .             ALS / 20.0D0 * (FIVE - 18.0D0 * T + T * T + 72.0D0 *
     .             C - 58.0D0 * ESP))) + X0
         PROJ(2) = KS0 *(ML - ML0 + N * TQ *(ALS *(HALF + ALS / 24.0D0 *
     .             (FIVE - T + NINE * C + FOUR * C * C + ALS / 30.0D0 *
     .             (61.0D0 - 58.0D0 * T + T * T + 600.0D0 * C -
     .             330.0D0 * ESP))))) + Y0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(9) .NE. 0) GO TO 320
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 094
         RETURN
  320    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         IF (IND .EQ. 0) GO TO 340
         F = DEXP (X / (A * KS0))
         G = HALF * (F - ONE / F)
         TEMP = LAT0 + Y / (A * KS0)
         H = DCOS (TEMP)
         CON = DSQRT ((ONE - H * H) / (ONE + G * G))
         GEOG(2) = ASINZ0 (CON)
         IF (TEMP .LT. ZERO) GEOG(2) =-GEOG(2)
         IF (G.NE.ZERO .OR. H.NE.ZERO) GO TO 330
         GEOG(1) = LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  330    GEOG(1) = ADJLZ0 (DATAN2 (G,H) + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
C
  340    CON = (ML0 + Y / KS0) / A
         PHI = CON
         DO 360 I = 1,NIT
         DPHI = ((CON + E1 * DSIN (TWO * PHI) - E2 * DSIN (FOUR * PHI)
     .          + E3 * DSIN (SIX * PHI)) / E0) - PHI
         PHI = PHI + DPHI
         IF (DABS(DPHI) .LE. EPSLN) GO TO 380
  360    CONTINUE
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030) NIT
 2030    FORMAT ('0ERROR PI09Z0' /
     .           ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS')
         IERROR = 095
         RETURN
  380    IF (DABS(PHI) .LT. HALFPI) GO TO 400
         GEOG(2) = DSIGN (HALFPI , Y)
         GEOG(1) = LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  400    SINPHI = DSIN (PHI)
         COSPHI = DCOS (PHI)
         TANPHI = DTAN (PHI)
         C = ESP * COSPHI * COSPHI
         CS = C * C
         T = TANPHI * TANPHI
         TS = T * T
         CON = ONE - ES * SINPHI * SINPHI
         N = A / DSQRT (CON)
         R = N * (ONE - ES) / CON
         D = X / (N * KS0)
         DS = D * D
         GEOG(2) = PHI - (N * TANPHI * DS / R) * (HALF - DS / 24.0D0 *
     .             (FIVE + THREE * T + TEN * C - FOUR * CS - NINE * ESP
     .             - DS / 30.0D0 * (61.0D0 + 90.0D0 * T + 298.0D0 * C +
     .             45.0D0 * TS - 252.0D0 * ESP - THREE * CS)))
         GEOG(1) = ADJLZ0 (LON0 + (D * (ONE - DS / SIX * (ONE + TWO *
     .             T + C - DS / 20.0D0 * (FIVE - TWO * C + 28.0D0 * T -
     .             THREE * CS + EIGHT * ESP + 24.0D0 * TS))) / COSPHI))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ10Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                          *  STEREOGRAPHIC  *
C **********************************************************************
C
      SUBROUTINE PJ10Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,LAT0,X0,Y0,SINPH0,COSPH0 *****************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ10/ A,LON0,X0,Y0,COSPH0,LAT0,SINPH0
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE,TWO /0.0D0,1.0D0,2.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(10) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ10Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 102
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         SINPHI = DSIN (GEOG(2))
         COSPHI = DCOS (GEOG(2))
         COSLON = DCOS (LON)
         G = SINPH0 * SINPHI + COSPH0 * COSPHI * COSLON
         IF (DABS(G + ONE) .GT. EPSLN) GO TO 140
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT ('0ERROR PJ10Z0'/
     .           ' POINT PROJECTS INTO INFINITY')
         IERROR = 103
         RETURN
  140    KSP = TWO / (ONE + G)
         PROJ(1) = X0 + A * KSP * COSPHI * DSIN (LON)
         PROJ(2) = Y0 + A * KSP * (COSPH0 * SINPHI - SINPH0 * COSPHI *
     .             COSLON)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(10) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 104
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         RH = DSQRT (X * X + Y * Y)
         Z = TWO * DATAN (RH / (TWO * A))
         SINZ = DSIN (Z)
         COSZ = DCOS (Z)
         GEOG(1) = LON0
         IF (DABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(2) = ASINZ0 (COSZ * SINPH0 + Y * SINZ * COSPH0 / RH)
         CON = DABS (LAT0) - HALFPI
         IF (DABS (CON) .GT. EPSLN) GO TO 260
         IF (LAT0 .LT. ZERO) GO TO 250
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 (X , -Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  250    GEOG(1) = ADJLZ0 (LON0 - DATAN2 (-X , Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  260    CON = COSZ - SINPH0 * DSIN (GEOG(2))
         IF (DABS(CON).LT.EPSLN.AND.DABS(X).LT.EPSLN) RETURN
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 ((X*SINZ*COSPH0) , (CON*RH)))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ11Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                   *  LAMBERT AZIMUTHAL EQUAL-AREA  *
C **********************************************************************
C
      SUBROUTINE PJ11Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,LAT0,X0,Y0,SINPH0,COSPH0 *****************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ11/ A,LON0,X0,Y0,COSPH0,LAT0,SINPH0
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE,TWO /0.0D0,1.0D0,2.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(11) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ11Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 112
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         SINPHI = DSIN (GEOG(2))
         COSPHI = DCOS (GEOG(2))
         COSLON = DCOS (LON)
         G = SINPH0 * SINPHI + COSPH0 * COSPHI * COSLON
         IF (G .NE. -ONE) GO TO 140
         CON = TWO * A
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020) CON
 2020    FORMAT (' POINT PROJECTS INTO A CIRCLE OF RADIUS =',F12.2,
     .           ' METERS')
         IERROR = 113
         RETURN
  140    KSP = DSQRT (TWO / (ONE + G))
         PROJ(1) = X0 + A * KSP * COSPHI * DSIN (LON)
         PROJ(2) = Y0 + A * KSP * (COSPH0 * SINPHI - SINPH0 * COSPHI *
     .             COSLON)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(11) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 114
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         RH = DSQRT (X * X + Y * Y)
         CON = RH / (TWO * A)
         IF (CON .LE. ONE) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT ('0ERROR PJ11Z0'/
     .           ' INPUT DATA ERROR')
         IERROR = 115
         RETURN
  230    Z = TWO * ASINZ0 (CON)
         SINZ = DSIN (Z)
         COSZ = DCOS (Z)
         GEOG(1) = LON0
         IF (DABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(2) = ASINZ0 (COSZ * SINPH0 + Y * SINZ * COSPH0 / RH)
         CON = DABS (LAT0) - HALFPI
         IF (DABS (CON) .GT. EPSLN) GO TO 260
         IF (LAT0 .LT. ZERO) GO TO 250
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 (X , -Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  250    GEOG(1) = ADJLZ0 (LON0 - DATAN2 (-X , Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  260    CON = COSZ - SINPH0 * DSIN (GEOG(2))
         IF (CON .EQ. ZERO) RETURN
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 ((X*SINZ*COSPH0) , (CON*RH)))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ12Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                      *  AZIMUTHAL EQUIDISTANT  *
C **********************************************************************
C
      SUBROUTINE PJ12Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,LAT0,X0,Y0,SINPH0,COSPH0 *****************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ12/ A,LON0,X0,Y0,COSPH0,LAT0,SINPH0
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE,TWO /0.0D0,1.0D0,2.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(12) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ12Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 122
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         SINPHI = DSIN (GEOG(2))
         COSPHI = DCOS (GEOG(2))
         COSLON = DCOS (LON)
         G = SINPH0 * SINPHI + COSPH0 * COSPHI * COSLON
         IF (DABS(DABS(G) - ONE) .GE. EPSLN) GO TO 140
         KSP = ONE
         IF (G .GE. ZERO) GO TO 160
         CON = TWO * HALFPI * A
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020) CON
 2020    FORMAT (' POINT PROJECTS INTO CIRCLE OF RADIUS =',F12.2,
     .           ' METERS')
         IERROR = 123
         RETURN
  140    Z = DACOS (G)
         KSP = Z / DSIN (Z)
  160    PROJ(1) = X0 + A * KSP * COSPHI * DSIN (LON)
         PROJ(2) = Y0 + A * KSP * (COSPH0 * SINPHI - SINPH0 * COSPHI *
     .             COSLON)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(12) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 124
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         RH = DSQRT (X * X + Y * Y)
         IF (RH .LE. (TWO * HALFPI * A)) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT ('0ERROR PJ12Z0'/
     .           ' INPUT DATA ERROR')
         IERROR = 125
         RETURN
  230    Z = RH / A
         SINZ = DSIN (Z)
         COSZ = DCOS (Z)
         GEOG(1) = LON0
         IF (DABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(2) = ASINZ0 (COSZ * SINPH0 + Y * SINZ * COSPH0 / RH)
         CON = DABS (LAT0) - HALFPI
         IF (DABS (CON) .GT. EPSLN) GO TO 260
         IF (LAT0 .LT. ZERO) GO TO 250
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 (X , -Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  250    GEOG(1) = ADJLZ0 (LON0 - DATAN2 (-X , Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  260    CON = COSZ - SINPH0 * DSIN (GEOG(2))
         IF (DABS(CON).LT.EPSLN.AND.DABS(X).LT.EPSLN) RETURN
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 ((X*SINZ*COSPH0) , (CON*RH)))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ13Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                            *  GNOMONIC  *
C **********************************************************************
C
      SUBROUTINE PJ13Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,LAT0,X0,Y0,SINPH0,COSPH0 *****************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ13/ A,LON0,X0,Y0,COSPH0,LAT0,SINPH0
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE /0.0D0,1.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(13) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ13Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 132
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         SINPHI = DSIN (GEOG(2))
         COSPHI = DCOS (GEOG(2))
         COSLON = DCOS (LON)
         G = SINPH0 * SINPHI + COSPH0 * COSPHI * COSLON
         IF (G .GT. ZERO) GO TO 140
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT (' POINT PROJECTS INTO INFINITY')
         IERROR = 133
         RETURN
  140    KSP = ONE / G
         PROJ(1) = X0 + A * KSP * COSPHI * DSIN (LON)
         PROJ(2) = Y0 + A * KSP * (COSPH0 * SINPHI - SINPH0 * COSPHI *
     .             COSLON)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(13) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 134
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         RH = DSQRT (X * X + Y * Y)
         Z = DATAN (RH / A)
         SINZ = DSIN (Z)
         COSZ = DCOS (Z)
         GEOG(1) = LON0
         IF (DABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(2) = ASINZ0 (COSZ * SINPH0 + Y * SINZ * COSPH0 / RH)
         CON = DABS (LAT0) - HALFPI
         IF (DABS (CON) .GT. EPSLN) GO TO 260
         IF (LAT0 .LT. ZERO) GO TO 250
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 (X , -Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  250    GEOG(1) = ADJLZ0 (LON0 - DATAN2 (-X , Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  260    CON = COSZ - SINPH0 * DSIN (GEOG(2))
         IF (DABS(CON).LT.EPSLN.AND.DABS(X).LT.EPSLN) RETURN
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 ((X*SINZ*COSPH0) , (CON*RH)))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ14Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                          *  ORTHOGRAPHIC  *
C **********************************************************************
C
      SUBROUTINE PJ14Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,LAT0,X0,Y0,SINPH0,COSPH0 *****************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ14/ A,LON0,X0,Y0,COSPH0,LAT0,SINPH0
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE /0.0D0,1.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(14) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ14Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 142
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         SINPHI = DSIN (GEOG(2))
         COSPHI = DCOS (GEOG(2))
         COSLON = DCOS (LON)
         G = SINPH0 * SINPHI + COSPH0 * COSPHI * COSLON
         KSP = ONE
         IF (G.GT.ZERO .OR. DABS(G).LE.EPSLN) GO TO 140
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT (' POINT CANNOT BE PROJECTED')
         IERROR = 143
         RETURN
  140    PROJ(1) = X0 + A * KSP * COSPHI * DSIN (LON)
         PROJ(2) = Y0 + A * KSP * (COSPH0 * SINPHI - SINPH0 * COSPHI *
     .             COSLON)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(14) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 144
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         RH = DSQRT (X * X + Y * Y)
         IF (RH .LE. A) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT ('0ERROR PJ14Z0'/
     .           ' INPUT DATA ERROR')
         IERROR = 145
         RETURN
  230    Z = ASINZ0 (RH / A)
         SINZ = DSIN (Z)
         COSZ = DCOS (Z)
         GEOG(1) = LON0
         IF (DABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(2) = ASINZ0 (COSZ * SINPH0 + Y * SINZ * COSPH0 / RH)
         CON = DABS (LAT0) - HALFPI
         IF (DABS (CON) .GT. EPSLN) GO TO 260
         IF (LAT0 .LT. ZERO) GO TO 250
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 (X , -Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  250    GEOG(1) = ADJLZ0 (LON0 - DATAN2 (-X , Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  260    CON = COSZ - SINPH0 * DSIN (GEOG(2))
         IF (DABS(CON).LT.EPSLN.AND.DABS(X).LT.EPSLN) RETURN
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 ((X*SINZ*COSPH0) , (CON*RH)))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ15Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C              *  GENERAL VERTICAL NEAR-SIDE PERSPECTIVE  *
C **********************************************************************
C
      SUBROUTINE PJ15Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,P,LON0,LAT0,X0,Y0,SINPH0,COSPH0 ***************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ15/ A,LON0,X0,Y0,COSPH0,LAT0,P,SINPH0
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE /0.0D0,1.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(15) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ15Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 152
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         SINPHI = DSIN (GEOG(2))
         COSPHI = DCOS (GEOG(2))
         COSLON = DCOS (LON)
         G = SINPH0 * SINPHI + COSPH0 * COSPHI * COSLON
         IF (G .GE. (ONE / P)) GO TO 140
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT (' POINT CANNOT BE PROJECTED')
         IERROR = 153
         RETURN
  140    KSP = (P - ONE) / (P - G)
         PROJ(1) = X0 + A * KSP * COSPHI * DSIN (LON)
         PROJ(2) = Y0 + A * KSP * (COSPH0 * SINPHI - SINPH0 * COSPHI *
     .             COSLON)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(15) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 154
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         RH = DSQRT (X * X + Y * Y)
         R = RH / A
         CON = P - ONE
         COM = P + ONE
         IF (R .LE. DSQRT (CON / COM)) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT ('0ERROR PJ15Z0'/
     .           ' INPUT DATA ERROR')
         IERROR = 155
         RETURN
  230    SINZ = (P - DSQRT (ONE - R * R * COM / CON)) /
     .          (CON / R + R / CON)
         Z = ASINZ0 (SINZ)
         SINZ = DSIN (Z)
         COSZ = DCOS (Z)
         GEOG(1) = LON0
         IF (DABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(2) = ASINZ0 (COSZ * SINPH0 + Y * SINZ * COSPH0 / RH)
         CON = DABS (LAT0) - HALFPI
         IF (DABS (CON) .GT. EPSLN) GO TO 260
         IF (LAT0 .LT. ZERO) GO TO 250
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 (X , -Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  250    GEOG(1) = ADJLZ0 (LON0 - DATAN2 (-X , Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  260    CON = COSZ - SINPH0 * DSIN (GEOG(2))
         IF (DABS(CON).LT.EPSLN.AND.DABS(X).LT.EPSLN) RETURN
         GEOG(1) = ADJLZ0 (LON0 + DATAN2 ((X*SINZ*COSPH0) , (CON*RH)))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ16Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                           *  SINUSOIDAL  *
C **********************************************************************
C
      SUBROUTINE PJ16Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,X0,Y0 ************************************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ16/ A,LON0,X0,Y0
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(16) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ16Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 162
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + A * LON * DCOS (GEOG(2))
         PROJ(2) = Y0 + A * GEOG(2)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(16) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 163
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         GEOG(2) = Y / A
         IF (DABS(GEOG(2)) .LE. HALFPI) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT ('0ERROR PJ16Z0'/
     .           ' INPUT DATA ERROR')
         IERROR = 164
         RETURN
  230    CON = DABS (GEOG(2)) - HALFPI
         IF (DABS (CON) .GT. EPSLN) GO TO 240
         GEOG(1) = LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(1) = ADJLZ0 (LON0 + X / (A * DCOS (GEOG(2))))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ17Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                  *  EQUIRECTANGULAR   *
C **********************************************************************
C
      SUBROUTINE PJ17Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,X0,Y0,LAT1 *******************************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ17/ A,LON0,X0,Y0,LAT1
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(17) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ17Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 172
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + A * LON * DCOS(LAT1)
         PROJ(2) = Y0 + A * GEOG(2)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(17) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 173
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         GEOG(2) = Y / A
         IF (DABS(GEOG(2)) .LE. HALFPI) GO TO 240
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT ('0ERROR PJ17Z0'/
     .           ' INPUT DATA ERROR')
         IERROR = 174
         RETURN
  240    GEOG(1) = ADJLZ0 (LON0 + X / (A * DCOS(LAT1) ))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ18Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                       *  MILLER CYLINDRICAL  *
C **********************************************************************
C
      SUBROUTINE PJ18Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,X0,Y0 ************************************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ18/ A,LON0,X0,Y0
      COMMON /TOGGLE/ SWITCH
      DATA FORTPI /0.78539816339744833D0/
      DATA ZERO,ONEQ,TWOH /0.0D0,1.25D0,2.5D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(18) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ18Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 182
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + A * LON
         PROJ(2) = Y0 + A * DLOG (DTAN (FORTPI + GEOG(2) / TWOH)) * ONEQ
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(18) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 183
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         GEOG(1) = ADJLZ0 (LON0 + X / A)
         GEOG(2) = TWOH * DATAN (DEXP (Y / A / ONEQ)) - FORTPI * TWOH
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ19Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                        *  VAN DER GRINTEN I  *
C **********************************************************************
C
      SUBROUTINE PJ19Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,X0,Y0 ************************************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ19/ A,LON0,X0,Y0
      COMMON /TOGGLE/ SWITCH
      DATA PI /3.14159265358979323846D0/
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN/1.0D-10/
      DATA ZERO,HALF,ONE,TWO,THREE/0.0D0,0.5D0,1.0D0,2.0D0,3.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(19) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ19Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 192
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         LAT = GEOG(2)
         IF (DABS(LAT) .GT. EPSLN) GO TO 140
         PROJ(1) = X0 + A * LON
         PROJ(2) = Y0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
  140    THETA = ASINZ0 (DMIN1(DABS (LAT /HALFPI),ONE))
         IF (DABS(LON).GT.EPSLN.AND.DABS(DABS(LAT)-HALFPI).GT.EPSLN)
     .       GO TO 160
         PROJ(1) = X0
         PROJ(2) = Y0 + PI * A * DSIGN( DTAN (HALF * THETA), LAT)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
  160    AL = HALF * DABS (PI / LON - LON / PI)
         ASQ = AL * AL
         SINTHT = DSIN (THETA)
         COSTHT = DCOS (THETA)
         G = COSTHT / (SINTHT + COSTHT - ONE)
         GSQ = G * G
         M = G * (TWO / SINTHT - ONE)
         MSQ = M * M
         CON = PI * A * (AL * (G - MSQ) + DSQRT (ASQ * (G - MSQ)**2 -
     .         (MSQ + ASQ) * (GSQ - MSQ))) / (MSQ + ASQ)
         CON = DSIGN (CON , LON)
         PROJ(1) = X0 + CON
         CON = DABS (CON / (PI * A))
         PROJ(2) = Y0 + DSIGN (PI * A * DSQRT (ONE - CON * CON -
     .             TWO * AL * CON) , LAT)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C     ALGORITHM DEVELOPED BY D.P. RUBINCAM, THE AMERICAN CARTOGRAPHER,
C                1981, V. 8, NO. 2, P. 177-180.
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(19) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 193
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         CON = PI * A
         XX = X / CON
         YY = Y / CON
         XYS = XX * XX + YY * YY
         C1 = -DABS(YY) * (ONE + XYS)
         C2 = C1 - TWO * YY * YY + XX * XX
         C3 = -TWO * C1 + ONE + TWO * YY * YY + XYS*XYS
         D = YY * YY / C3 + (TWO * C2 * C2 * C2/ C3/ C3/ C3 - 9.0D0 * C1
     .       * C2/ C3/ C3) / 27.0D0
         A1 = (C1 - C2 * C2/ THREE/ C3)/ C3
         M1 = TWO * DSQRT(-A1/ THREE)
         CON = ((THREE * D) / A1) / M1
         IF (DABS(CON).GT.ONE) CON = DSIGN(ONE,CON)
         TH1 = DACOS(CON)/THREE
         GEOG(2) = (-M1 * DCOS(TH1 + PI/ THREE) - C2/ THREE/ C3)
     .   * DSIGN(PI,Y)
         IF (DABS(XX).GE.EPSLN) GO TO 230
         GEOG(1) = LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  230    CONTINUE
         GEOG(1) = LON0 + PI * (XYS - ONE + DSQRT(ONE + TWO * (XX * XX
     .      - YY * YY) + XYS * XYS))/ TWO/ XX
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ20Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                    *  OBLIQUE MERCATOR (HOTINE)  *
C **********************************************************************
C
      SUBROUTINE PJ20Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,KS0,ALPHA,LONC,LON1,LAT1,LON2,LAT2,LAT0 **
C ********************** X0,Y0,GAMMA,LON0,AL,BL,EL *********************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ20/ LON0,X0,Y0,AL,BL,COSALF,COSGAM,E,EL,SINALF,SINGAM,U0
      COMMON /TOGGLE/ SWITCH
      DATA PI /3.14159265358979323846D0/
      DATA HALFPI /1.5707963267948966D0/
      DATA TOL,EPSLN /1.0D-7,1.0D-10/
      DATA ZERO,HALF,ONE /0.0D0,0.5D0,1.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(20) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2050)
 2050    FORMAT ('0ERROR PJ20Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 204
         RETURN
  220    SINPHI = DSIN (GEOG(2))
         DLON = ADJLZ0 (GEOG(1) - LON0)
         VL = DSIN (BL * DLON)
         IF (DABS(DABS(GEOG(2)) - HALFPI) .GT. EPSLN) GO TO 230
         UL = SINGAM * DSIGN (ONE , GEOG(2))
         US = AL * GEOG(2) / BL
         GO TO 250
  230    TS = TSFNZ0 (E,GEOG(2),SINPHI)
         Q = EL / TS ** BL
         S = HALF * (Q - ONE / Q)
         T = HALF * (Q + ONE / Q)
         UL = (S * SINGAM - VL * COSGAM) / T
         CON = DCOS (BL * DLON)
         IF (DABS(CON) .LT. TOL) GO TO 240
         US = AL * DATAN ((S * COSGAM + VL * SINGAM) / CON) / BL
         IF (CON .LT. ZERO) US = US + PI * AL / BL
         GO TO 250
  240    US = AL * BL * DLON
  250    IF (DABS(DABS(UL) - ONE) .GT. EPSLN) GO TO 255
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2060)
 2060    FORMAT ('0ERROR PJ20Z0'/
     .           ' POINT PROJECTS INTO INFINITY')
         IERROR = 205
         RETURN
  255    VS = HALF * AL * DLOG ((ONE - UL) / (ONE + UL)) / BL
         US = US - U0
         PROJ(1) = X0 + VS * COSALF + US * SINALF
         PROJ(2) = Y0 + US * COSALF - VS * SINALF
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(20) .NE. 0) GO TO 280
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2050)
         IERROR = 206
         RETURN
  280    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         VS = X * COSALF - Y * SINALF
         US = Y * COSALF + X * SINALF
         US = US + U0
         Q = DEXP (- BL * VS / AL)
         S = HALF * (Q - ONE / Q)
         T = HALF * (Q + ONE / Q)
         VL = DSIN (BL * US / AL)
         UL = (VL * COSGAM + S * SINGAM) / T
         IF (DABS (DABS (UL) - ONE) .GE. EPSLN) GO TO 300
         GEOG(1) = LON0
         GEOG(2) = DSIGN (HALFPI , UL)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  300    CON = ONE / BL
         TS = (EL / DSQRT ((ONE + UL) / (ONE - UL))) ** CON
         GEOG(2) = PHI2Z0 (E,TS)
         CON = DCOS (BL * US / AL)
         LON = LON0 - DATAN2 ((S * COSGAM - VL * SINGAM) , CON) / BL
         GEOG(1) = ADJLZ0 (LON)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ21Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                       *       ROBINSON       *
C **********************************************************************
C
      SUBROUTINE PJ21Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN,IP1,NN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2),
     . PR(20),XLR(20)
C **** PARAMETERS **** A,LON0,X0,Y0 ************************************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ21/ A,LON0,X0,Y0,PR,XLR
      COMMON /TOGGLE/ SWITCH
      DATA DG1 /0.01745329252D0/
      DATA PI /3.14159265358979323846D0/
      DATA EPSLN /1.0D-10/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(21) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ21Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 212
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
         P2=DABS(GEOG(2)/5.0D0/DG1)
         IP1=IDINT(P2-EPSLN)
C
C        STIRLING'S INTERPOLATION FORMULA (USING 2ND DIFF.)
C            USED WITH LOOKUP TABLE TO COMPUTE RECTANGULAR COORDINATES
C            FROM LAT/LONG.
C
         P2=P2-DBLE(IP1)
         X=A*(XLR(IP1+2)+P2*(XLR(IP1+3)-XLR(IP1+1))/2.0D0
     .     +P2*P2*(XLR(IP1+3)-2.0D0*XLR(IP1+2)+XLR(IP1+1))/2.0D0)*LON
         Y=A*(PR(IP1+2)+P2*(PR(IP1+3)-PR(IP1+1))/2.0D0
     .     +P2*P2*(PR(IP1+3)-2.0D0*PR(IP1+2)+PR(IP1+1))/2.0D0)*PI/2.0D0
     .     *DSIGN(1.0D0,GEOG(2))
         PROJ(1) = X0 + X
         PROJ(2) = Y0 + Y
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(21) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 213
         RETURN
  220    X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         YY = 2.0D0 * Y / PI / A
         PHID = YY * 90.0D0
         P2 = DABS(PHID / 5.0D0)
         IP1 = IDINT(P2 - EPSLN)
         IF (IP1.EQ.0) IP1 = 1
         NN = 0
C
C     STIRLING'S INTERPOLATION FORMULA AS USED IN FORWARD TRANSFORMATION
C     IS REVERSED FOR FIRST ESTIMATION OF LAT. FROM RECTANGULAR
C     COORDINATES.  LAT. IS THEN ADJUSTED BY ITERATION UNTIL USE OF
C     FORWARD SERIES PROVIDES CORRECT VALUE OF Y WITHIN TOLERANCE.
C
  230    U = PR(IP1 + 3) - PR(IP1 + 1)
         V = PR(IP1 + 3) - 2.0D0 * PR(IP1 + 2) + PR(IP1 + 1)
         T = 2.0D0 * (DABS(YY) - PR(IP1 + 2))/ U
         C = V / U
         P2 = T * (1.0D0 - C * T * (1.0D0 - 2.0D0 * C * T))
         IF (P2.LT.0.0D0.AND.IP1.NE.1) GO TO 240
         PHID = DSIGN((P2 + DBLE(IP1)) * 5.0D0, Y)
  235    P2 = DABS(PHID / 5.0D0)
         IP1 = IDINT(P2 - EPSLN)
         P2 = P2 - DBLE(IP1)
         Y1=A*(PR(IP1+2)+P2*(PR(IP1+3)-PR(IP1+1))/2.0D0
     .     +P2*P2*(PR(IP1+3)-2.0D0*PR(IP1+2)+PR(IP1+1))/2.0D0)*PI/2.0D0
     .     * DSIGN(1.0D0,Y)
         PHID = PHID - 180.0D0* (Y1 - Y) / PI / A
         NN = NN + 1
         IF (NN.LE.20) GO TO 237
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,245)
         IERROR = 214
         RETURN
  237    IF (DABS(Y1 - Y).GT.0.00001D0) GO TO 235
         GO TO 250
  240    IP1 = IP1 - 1
         GO TO 230
  245    FORMAT ('0ERROR PJ21Z0'/
     .           ' TOO MANY ITERATIONS FOR INVERSE ROBINSON')
  250    GEOG(2) = PHID * DG1
C
C        CALCULATE LONG. USING FINAL LAT. WITH TRANSPOSED FORWARD
C        STIRLING'S INTERPOLATION FORMULA.
C
         GEOG(1)=LON0+X/A/(XLR(IP1+2)+P2*(XLR(IP1+3)-XLR(IP1+1))/2.0D0
     .     +P2*P2*(XLR(IP1+3)-2.0D0*XLR(IP1+2)+XLR(IP1+1))/2.0D0)
         GEOG(1) = ADJLZ0(GEOG(1))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ22Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C                      *  SPACE OBLIQUE MERCATOR  *
C **********************************************************************
C
      SUBROUTINE PJ22Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN,PATH,LAND,NN,L
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,LATC,X0,Y0,MCS,TCS,FAC,IND **********
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /NORM/ Q,T,U,W,ES,P22,SA,CA,XJ
      COMMON /PJ22/ A,X0,Y0,A2,A4,B,C1,C3,LAND,PATH
      COMMON /TOGGLE/ SWITCH
      DATA TOL /1.0D-7/
      DATA DG1 /0.01745329252D0/
      DATA PI /3.14159265358979323846D0/
      DATA ZERO,ONE,TWO /0.0D0,1.0D0,2.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(22) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ22Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 222
         RETURN
  220    IF (LAND.GE.4) GO TO 225
         LON=GEOG(1)-128.87D0*DG1+PI*TWO/251.D0*DBLE(PATH)
         GO TO 230
  225    LON=GEOG(1)-129.30D0*DG1+PI*TWO/233.D0*DBLE(PATH)
  230    LAT=GEOG(2)
C
C        TEST FOR LAT. AND LONG. APPROACHING 90 DEGREES.
C
         IF (LAT.GT.1.570796D0) LAT=1.570796D0
         IF (LAT.LT.-1.570796D0) LAT =-1.570796D0
         IF (LAT.GE.0) LAMPP=PI/TWO
         IF (LAT.LT.0) LAMPP=1.5D0*PI
         NN=0
  231    SAV=LAMPP
         L=0
         LAMTP=LON+P22*LAMPP
         CL=DCOS(LAMTP)
         IF (DABS(CL).LT.TOL) LAMTP=LAMTP-TOL
         FAC=LAMPP-(DSIGN(ONE,CL))*DSIN(LAMPP)*PI/TWO
  232    LAMT=LON+P22*SAV
         C=DCOS(LAMT)
         IF (DABS(C).LT.TOL) THEN
            LAMDP = SAV
            GO TO 233
         END IF
         XLAM=((ONE-ES)*DTAN(LAT)*SA+DSIN(LAMT)*CA)/C
         LAMDP=DATAN(XLAM)
         LAMDP=LAMDP+FAC
         DIF=DABS(SAV)-DABS(LAMDP)
         IF (DABS(DIF).LT.TOL) GO TO 233
         SAV=LAMDP
         L=L+1
         IF (L.GT.50) GO TO 234
         GO TO 232
C
C        ADJUST FOR LANDSAT ORIGIN.
C
  233    RLM=PI*(16.D0/31.D0+ONE/248.D0)
         RLM2=RLM+TWO*PI
         NN=NN+1
         IF (NN.GE.3) GO TO 236
         IF (LAMDP.GT.RLM.AND.LAMDP.LT.RLM2) GO TO 236
         IF (LAMDP.LE.RLM) LAMPP=2.5D0*PI
         IF (LAMDP.GE.RLM2) LAMPP=PI/TWO
         GO TO 231
  234    IF (IPEMSG .EQ. 0) WRITE (IPELUN,235)
  235    FORMAT ('0ERROR PJ22Z0'/
     .           ' 50 ITERATIONS WITHOUT CONVERGENCE.')
         IERROR = 223
  236    CONTINUE
C
C        LAMDP COMPUTED.  NOW COMPUTE PHIDP.
C
         SP=DSIN(LAT)
         PHIDP=ASINZ0(((ONE-ES)*CA*SP-SA*DCOS(LAT)*DSIN(LAMT))/DSQRT(ONE
     .     -ES*SP*SP))
C
C        COMPUTE X AND Y
C
         TANPH=DLOG(DTAN(PI/4.0D0+PHIDP/TWO))
         SD=DSIN(LAMDP)
         SDSQ=SD*SD
         S=P22*SA*DCOS(LAMDP)*DSQRT((ONE+T*SDSQ)/((ONE+W*SDSQ)*(ONE
     .     +Q*SDSQ)))
         D=DSQRT(XJ*XJ+S*S)
         X=B*LAMDP+A2*DSIN(TWO*LAMDP)+A4*DSIN(4.0D0*LAMDP)-TANPH*S/D
         X=A*X
         Y=C1*SD+C3*DSIN(3.0D0*LAMDP)+TANPH*XJ/D
         Y=A*Y
         PROJ(1)=X+X0
         PROJ(2)=Y+Y0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(22) .NE. 0) GO TO 320
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 224
         RETURN
  320    X = PROJ(1) -X0
         Y = PROJ(2) -Y0
C
C        COMPUTE TRANSFORMED LAT/LONG AND GEODETIC LAT/LONG, GIVEN X,Y.
C
C        BEGIN INVERSE COMPUTATION WITH APPROXIMATION FOR LAMDP.  SOLVE
C        FOR TRANSFORMED LONG.
C
         LAMDP=X/A/B
         NN=0
  325    SAV=LAMDP
         SD=DSIN(LAMDP)
         SDSQ=SD*SD
         S=P22*SA*DCOS(LAMDP)*DSQRT((ONE+T*SDSQ)/((ONE+W*SDSQ)*(ONE+Q
     .     *SDSQ)))
         LAMDP=X/A+Y/A*S/XJ-A2*DSIN(TWO*LAMDP)-A4*DSIN(4.0D0*LAMDP)
     .     -(S/XJ)*(C1*DSIN(LAMDP)+C3*DSIN(3.0D0*LAMDP))
         LAMDP=LAMDP/B
         DIF=LAMDP-SAV
         IF (DABS(DIF).LT.TOL) GO TO 330
         NN=NN+1
         IF (NN.EQ.50) GO TO 330
         GO TO 325
C
C        COMPUTE TRANSFORMED LAT.
C
  330    SL=DSIN(LAMDP)
         FAC=DEXP(DSQRT(ONE+S*S/XJ/XJ)*(Y/A-C1*SL-C3*DSIN(3.0D0*LAMDP)))
         ACTAN=DATAN(FAC)
         PHIDP=TWO*(ACTAN-PI/4.0D0)
C
C        COMPUTE GEODETIC LATITUDE.
C
         DD=SL*SL
         IF (DABS(DCOS(LAMDP)).LT.TOL) LAMDP=LAMDP-TOL
         SPP=DSIN(PHIDP)
         SPPSQ=SPP*SPP
         LAMT=DATAN(((ONE-SPPSQ/(ONE-ES))*DTAN(LAMDP)*CA-SPP*SA*DSQRT((
     .   ONE+Q*DD)*(ONE-SPPSQ)-SPPSQ*U)/DCOS(LAMDP))/(ONE-SPPSQ*(ONE+U))
     .   )
C
C        CORRECT INVERSE QUADRANT.
C
         IF (LAMT.GE.0) SL=ONE
         IF (LAMT.LT.0) SL=-ONE
         IF (DCOS(LAMDP).GE.0) SCL=ONE
         IF (DCOS(LAMDP).LT.0) SCL=-ONE
         LAMT=LAMT-PI/TWO*(ONE-SCL)*SL
         LON=LAMT-P22*LAMDP
C
C        COMPUTE GEODETIC LATITUDE.
C
         IF (DABS(SA).LT.TOL) LAT=ASINZ0(SPP/DSQRT((ONE-ES)*(ONE-ES)
     .      +ES*SPPSQ))
         IF (DABS(SA).LT.TOL) GO TO 335
         LAT=DATAN((DTAN(LAMDP)*DCOS(LAMT)-CA*DSIN(LAMT))/((ONE-ES)*SA))
  335    CONTINUE
         IF (LAND.GE.4) GO TO 370
         GEOG(1)=LON+128.87D0*DG1-PI*TWO/251.D0*DBLE(PATH)
         GO TO 380
  370    GEOG(1)=LON+129.30D0*DG1-PI*TWO/233.D0*DBLE(PATH)
  380    GEOG(2)=LAT
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   PJ23Z0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **              MATHEMATICAL ANALYSIS BY JOHN SNYDER                **
C **********************************************************************
C            * MODIFIED-STEREOGRAPHIC CONFORMAL (FOR ALASKA) *
C **********************************************************************
C
      SUBROUTINE PJ23Z0 (COORD,CRDIO,INDIC)
C
      IMPLICIT REAL*8 (A-Z)
      INTEGER*4 IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN,N,J,NN
      INTEGER*4 SWITCH(23)
      INTEGER*2 INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2),
     . ACOEF(6),BCOEF(6)
C **** PARAMETERS **** A,E,ES,LON0,LAT0,X0,Y0,SINPH0,COSPH0 ************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ23/ A,LON0,X0,Y0,ACOEF,BCOEF,EC,LAT0,CCHIO,SCHIO,N
      COMMON /TOGGLE/ SWITCH
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE,TWO /0.0D0,1.0D0,2.0D0/
C
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(23) .NE. 0) GO TO 120
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
 2010    FORMAT ('0ERROR PJ23Z0'/
     .           ' PROJECTION WAS NOT INITIALIZED')
         IERROR = 232
         RETURN
  120    LON = ADJLZ0 (GEOG(1) - LON0)
C
C     CALCULATE X-PRIME AND Y-PRIME FOR OBLIQUE STEREOGRAPHIC PROJ.
C          FROM LAT/LONG.
C
         SINLON = DSIN (LON)
         COSLON = DCOS (LON)
         ESPHI = EC *DSIN(GEOG(2))
         CHI=TWO*DATAN(DTAN((HALFPI+GEOG(2))/TWO)*((ONE-ESPHI)/(ONE
     .      +ESPHI))**(EC/TWO)) - HALFPI
         SCHI=DSIN(CHI)
         CCHI=DCOS(CHI)
         G=SCHIO*SCHI+CCHIO*CCHI*COSLON
         S=TWO/(ONE+G)
         XP=S*CCHI*SINLON
         YP=S*(CCHIO*SCHI-SCHIO*CCHI*COSLON)
C
C     USE KNUTH ALGORITHM FOR SUMMING COMPLEX TERMS, TO CONVERT
C     OBLIQUE STEREOGRAPHIC TO MODIFIED-STEREOGRAPHIC COORD.
C
         R=XP+XP
         S=XP*XP+YP*YP
         AR=ACOEF(N)
         AI=BCOEF(N)
         BR=ACOEF(N-1)
         BI=BCOEF(N-1)
         DO 140 J=2,N
         ARN=BR+R*AR
         AIN=BI+R*AI
         IF (J.EQ.N) GO TO 140
         BR=ACOEF(N-J)-S*AR
         BI=BCOEF(N-J)-S*AI
         AR=ARN
         AI=AIN
  140    CONTINUE
         BR=-S*AR
         BI=-S*AI
         AR=ARN
         AI=AIN
         X=XP*AR-YP*AI+BR
         Y=YP*AR+XP*AI+BI
         PROJ(1)=X*A+X0
         PROJ(2)=Y*A+Y0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .
C ......................................................................
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         IF (SWITCH(23) .NE. 0) GO TO 220
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010)
         IERROR = 234
         RETURN
  220    X = (PROJ(1) - X0)/A
         Y = (PROJ(2) - Y0)/A
         XP=X
         YP=Y
         NN=0
C
C     USE KNUTH ALGORITHM FOR SUMMING COMPLEX TERMS, TO CONVERT
C     MODIFIED-STEREOGRAPHIC CONFORMAL TO OBLIQUE STEREOGRAPHIC
C     COORDINATES (XP,YP).
C
  225    R=XP+XP
         S=XP*XP+YP*YP
         AR=ACOEF(N)
         AI=BCOEF(N)
         BR=ACOEF(N-1)
         BI=BCOEF(N-1)
         CR=DBLE(N)*AR
         CI=DBLE(N)*AI
         DR=(DBLE(N-1))*BR
         DI=(DBLE(N-1))*BI
         DO 230 J=2,N
         ARN=BR+R*AR
         AIN=BI+R*AI
         IF (J.EQ.N) GO TO 230
         BR=ACOEF(N-J)-S*AR
         BI=BCOEF(N-J)-S*AI
         AR=ARN
         AI=AIN
         CRN=DR+R*CR
         CIN=DI+R*CI
         DR=DBLE(N-J)*ACOEF(N-J)-S*CR
         DI=DBLE(N-J)*BCOEF(N-J)-S*CI
         CR=CRN
         CI=CIN
  230    CONTINUE
         BR=-S*AR
         BI=-S*AI
         AR=ARN
         AI=AIN
         FXYR=XP*AR-YP*AI+BR-X
         FXYI=YP*AR+XP*AI+BI-Y
         FPXYR=XP*CR-YP*CI+DR
         FPXYI=YP*CR+XP*CI+DI
         DEN=FPXYR*FPXYR+FPXYI*FPXYI
         DXP=-(FXYR*FPXYR+FXYI*FPXYI)/DEN
         DYP=-(FXYI*FPXYR-FXYR*FPXYI)/DEN
         XP=XP+DXP
         YP=YP+DYP
         DS=DABS(DXP)+DABS(DYP)
         NN=NN+1
         IF (NN.LE.20) GO TO 237
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,235)
  235    FORMAT ('0ERROR PJ23Z0'/
     .           ' TOO MANY ITERATIONS IN ITERATING INVERSE')
         IERROR = 235
         GO TO 238
  237    IF (DS.GT.EPSLN) GO TO 225
C
C     CONVERT OBLIQUE STEREOGRAPHIC COORDINATES TO LAT/LONG.
C
  238    RH = DSQRT (XP * XP + YP * YP)
         Z = TWO * DATAN (RH / TWO)
         SINZ = DSIN (Z)
         COSZ = DCOS (Z)
         GEOG(1) = LON0
         IF (DABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    CHI = ASINZ0 (COSZ * SCHIO + YP *SINZ * CCHIO / RH)
         NN=0
         PHI=CHI
  250    ESPHI=EC*DSIN(PHI)
         DPHI=TWO*DATAN(DTAN((HALFPI+CHI)/TWO)*((ONE+ESPHI)/(ONE-ESPHI))
     .      **(EC/TWO)) - HALFPI - PHI
         PHI = PHI + DPHI
         NN = NN + 1
         IF (NN.LE.20) GO TO 257
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,255)
  255    FORMAT ('0ERROR PJ23Z0'/
     .           ' TOO MANY ITERATIONS IN CALCULATING PHI FROM CHI')
         IERROR = 236
         GO TO 260
  257    IF (DABS(DPHI).GT.EPSLN) GO TO 250
  260    GEOG(2)=PHI
         GEOG(1) = ADJLZ0 (LON0 + DATAN2(XP*SINZ, RH*CCHIO*COSZ-YP*SCHIO
     .     *SINZ))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
C                   QSFNZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION QSFNZ0 (ECCENT,SINPHI,COSPHI)
C
C FUNCTION TO COMPUTE CONSTANT (SMALL Q).
C
      IMPLICIT REAL*8 (A-Z)
      DATA HALF,ONE,TWO /0.5D0,1.0D0,2.0D0/
      DATA EPSLN /1.0D-7/
C
      IF (ECCENT .LT. EPSLN) GO TO 020
      CON = ECCENT * SINPHI
      QSFNZ0 = (ONE - ECCENT * ECCENT) * (SINPHI / (ONE - CON * CON) -
     .         (HALF / ECCENT) * DLOG ((ONE - CON) / (ONE + CON)))
      RETURN
C
  020 QSFNZ0 = TWO * SINPHI
      RETURN
      END
C                   RADDZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      SUBROUTINE RADDZ0 (RAD,SGNA,DEGS,MINS,SECS)
C
C SUBROUTINE TO CONVERT ANGLE FROM RADIANS TO SIGNED DMS
C SGNA : SIGN OF ANGLE
C DEGS : DEGREES PORTION OF ANGLE
C MINS : MINUTES PORTION OF ANGLE
C SECS : SECONDS PORTION OF ANGLE
C
      REAL*8 RAD,CON,RADSEC,ZERO,TOL
      REAL*4 SECS
      INTEGER*4 DEGS,MINS
      CHARACTER*1 SGNA,BLANK,NEG
      DATA RADSEC /206264.806247D0/
      DATA ZERO,TOL /0.0D0,1.0D-4/
      DATA BLANK,NEG /' ','-'/
C
C CONVERT THE ANGLE TO SECONDS.
C
      CON = DABS(RAD) * RADSEC
      ISEC = IDINT(CON + TOL)
C
C DETERMINE THE SIGN OF THE ANGLE.
C
      SGNA = BLANK
      IF (RAD .LT. ZERO .AND. CON .GE. 0.00005D0) SGNA = NEG
      IF (CON .LT. 0.00005D0) CON = ZERO
C
C COMPUTE DEGREES PART OF THE ANGLE.
C
      INTG = ISEC / 3600
      DEGS = INTG
      ISEC = INTG * 3600
      CON = CON - DBLE(ISEC)
      ISEC = IDINT(CON + TOL)
C
C COMPUTE MINUTES PART OF THE ANGLE.
C
      MINS = ISEC / 60
      ISEC = MINS * 60
      CON = CON - DBLE(ISEC)
C
C COMPUTE SECONDS PART OF THE ANGLE.
C
      SECS = SNGL(CON)
C
C     INCREASE MINS IF SECS CLOSE TO 60.000
C
      IF(SECS .LT. 59.9995D0) RETURN
      MINS = MINS + 1
      SECS = 0.0
C
C     INCREASE DEGS IF MINS EQUAL 60
C
      IF(MINS .LE. 59) RETURN
      MINS = 0
      DEGS = DEGS + 1
C
      RETURN
      END
C                   SERAZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      SUBROUTINE SERAZ0 (FB,FA2,FA4,FC1,FC3,LAM)
C
C     COMPUTES INTEGRAL FUNCTION OF TRANSFORMED LONG. FOR FOURIER
C     CONSTANTS A2, A4, B, C1, AND C3.
C     LAM IS INTEGRAL VALUE OF TRANSFORMED LONG.
C
      IMPLICIT REAL*8 (A-Z)
      COMMON /NORM/ Q,T,U,W,ES,P22,SA,CA,XJ
      DATA DG1 /0.01745329252D0/
      DATA ONE,TWO /1.0D0,2.0D0/
      LAM=LAM*DG1
      SD=DSIN(LAM)
      SDSQ=SD*SD
      S=P22*SA*DCOS(LAM)*DSQRT((ONE+T*SDSQ)/((ONE+W*SDSQ)
     .  *(ONE+Q*SDSQ)))
      H=DSQRT((ONE+Q*SDSQ)/(ONE+W*SDSQ))*(((ONE+W*SDSQ)/
     .   ((ONE+Q*SDSQ)**TWO))-P22*CA)
      SQ=DSQRT(XJ*XJ+S*S)
      FB=(H*XJ-S*S)/SQ
      FA2=FB*DCOS(TWO*LAM)
      FA4=FB*DCOS(4.0D0*LAM)
      FC=S*(H+XJ)/SQ
      FC1=FC*DCOS(LAM)
      FC3=FC*DCOS(3.0D0*LAM)
      RETURN
      END
C                   SPHDZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      SUBROUTINE SPHDZ0(ISPH,PARM)
C
C     SUBROUTINE TO COMPUTE SPHEROID PARAMETERS
C
C     ISPH IS THE SPHEROID CODE FROM THE FOLLOWING LIST:
C     0 = CLARKE 1866           1 = CLARKE 1880
C     2 = BESSEL                3 = NEW INTERNATIONAL 1967
C     4 = INTERNATIONAL 1909    5 = WGS 72
C     6 = EVEREST               7 = WGS 66
C     8 = GRS 1980              9 = AIRY
C    10 = MODIFIED EVEREST     11 = MODIFIED AIRY
C    12 = WGS 84               13 = SOUTHEAST ASIA
C    14 = AUSTRALIAN NATIONAL  15 = KRASSOVSKY
C    16 = HOUGH                17 = MERCURY 1960
C    18 = MODIFIED MERC 1968   19 = SPHERE OF RADIUS 6370997 M
C    20 = INTERNATIONAL 1924
C
C    PARM IS ARRAY OF PROJECTION PARAMETERS:
C       PARM(1) IS THE SEMI-MAJOR AXIS
C       PARM(2) IS THE ECCENTRICITY SQUARED
C
C     IF ISPH IS NEGATIVE, USER SPECIFIED PROJECTION PARAMETERS ARE TO
C     DEFINE THE RADIUS OF SPHERE OR ELLIPSOID CONSTANTS AS APPROPRIATE
C
C     IF ISPH = 0 , THE DEFAULT IS RESET TO CLARKE 1866
C
C ****                                                             *****
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION PARM(15),AXIS(21),BXIS(21)
C
      COMMON /ELLPZ0/ AZ,EZ,ESZ,E0Z,E1Z,E2Z,E3Z,E4Z
      COMMON /SPHRZ0/ AZZ
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PROJZ0/ IPROJ
C
      DATA ZERO,ONE /0.0D0,1.0D0/
C
      DATA AXIS/6378206.4D0,6378249.145D0,6377397.155D0,6378157.5D0,
     . 6378388.0D0,6378135.0D0,6377276.3452D0,6378145.0D0,6378137.0D0,
     . 6377563.396D0,6377304.063D0,6377340.189D0,6378137.0D0,6378155.D0,
     . 6378160.0D0,6378245.0D0,6378270.0D0,6378166.0D0,6378150.0D0,
     . 6370997.0D0,6378388.0D0/
C
      DATA BXIS/6356583.8D0,6356514.86955D0,6356078.96284D0,
     . 6356772.2D0,6356911.94613D0,6356750.519915D0,6356075.4133D0,
     . 6356759.769356D0,6356752.314140D0,6356256.91D0,6356103.039D0,
     . 6356034.448D0,6356752.314245D0,6356773.3205D0,6356774.719D0,
     . 6356863.0188D0,6356794.343479D0,6356784.283666D0,6356768.337303D0
     . ,6370997.0D0,6356911.95D0/
C
      IF (ISPH.GE.0) GO TO 5
C
C     INITIALIZE USER SPECIFIED SPHERE AND ELLIPSOID PARAMETERS
C
      AZZ = ZERO
      AZ = ZERO
      EZ = ZERO
      ESZ = ZERO
      E0Z = ZERO
      E1Z = ZERO
      E2Z = ZERO
      E3Z = ZERO
      E4Z = ZERO
C
C     FETCH FIRST TWO USER SPECIFIED PROJECTION PARAMETERS
C
      A = DABS(PARM(1))
      B = DABS(PARM(2))
      IF (A .GT. ZERO .AND. B .GT. ZERO) GO TO 13
      IF (A .GT. ZERO .AND. B .LE. ZERO) GO TO 12
      IF (A .LE. ZERO .AND. B .GT. ZERO) GO TO 11
C
C     DEFAULT NORMAL SPHERE AND CLARKE 1866 ELLIPSOID
C
      JSPH = 1
      GO TO 10
C
C     DEFAULT CLARKE 1866 ELLIPSOID
C
   11 A = AXIS(1)
      B = BXIS(1)
      GO TO 14
C
C     USER SPECIFIED RADIUS OF SPHERE
C
   12 AZZ = A
      GO TO 15
C
C     USER SPECIFIED SEMI-MAJOR AND SEMI-MINOR AXES OF ELLIPSOID
C
   13 IF (B .LE. ONE) GO TO 15
   14 ES = ONE - (B / A)**2
      GO TO 16
C
C     USER SPECIFIED SEMI-MAJOR AXIS AND ECCENTRICITY SQUARED
C
   15 ES = B
   16 AZ = A
      ESZ = ES
      EZ  = DSQRT(ES)
      E0Z = E0FNZ0(ES)
      E1Z = E1FNZ0(ES)
      E2Z = E2FNZ0(ES)
      E3Z = E3FNZ0(ES)
      E4Z = E4FNZ0(EZ)
      PARM(1) = A
      PARM(2) = ES
      RETURN
C
C     CHECK FOR VALID SPHEROID SELECTION
C
    5 IF (PARM(1).NE.ZERO.AND.IPROJ.NE.1) RETURN
      JSPH = IABS(ISPH) + 1
      IF (JSPH.LE.21) GO TO 10
      IERROR = 999
      IF (IPEMSG .EQ. 0) WRITE (IPELUN,1) ISPH
    1 FORMAT('0ERROR SPHDZ0:  SPHEROID CODE OF ',I5,' RESET TO 0')
      ISPH = 0
      JSPH = 1
C
C     RETRIEVE A AND B AXES FOR SELECTED SPHEROID
C
   10 A = AXIS(JSPH)
      B = BXIS(JSPH)
      ES = ONE - (B / A)**2
C
C     SET COMMON BLOCK PARAMETERS FOR SELECTED SPHEROID
C
      AZZ = 6370997.0D0
      EZ  = DSQRT(ES)
      E0Z = E0FNZ0(ES)
      E1Z = E1FNZ0(ES)
      E2Z = E2FNZ0(ES)
      E3Z = E3FNZ0(ES)
      E4Z = E4FNZ0(EZ)
      AZ  = A
      ESZ = ES
      IF (ES.EQ.ZERO) AZZ=A
C
      PARM(1) = A
      PARM(2) = ES
      RETURN
      END
C                   TSFNZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION TSFNZ0 (ECCENT,PHI,SINPHI)
C
C FUNCTION TO COMPUTE CONSTANT (SMALL T).
C
      IMPLICIT REAL*8 (A-Z)
      DATA HALF,ONE /0.5D0,1.0D0/
      DATA HALFPI /1.5707963267948966D0/
C
      CON = ECCENT * SINPHI
      COM = HALF * ECCENT
      CON = ((ONE - CON) / (ONE + CON)) ** COM
      TSFNZ0 = DTAN (HALF * (HALFPI - PHI)) / CON
C
      RETURN
      END
C                   UNTFZ0
C **********************************************************************
C ** GENERAL CARTOGRAPHIC TRANSFORMATION PACKAGE (GCTP) VERSION 2.0.2 **
C ** U. S. GEOLOGICAL SURVEY - SNYDER, ELASSAL, AND LINCK    06/08/94 **
C **********************************************************************
C
      SUBROUTINE UNTFZ0 (INUNIT,IOUNIT,FACTOR,IFLG)
C
C SUBROUTINE TO DETERMINE CONVERGENCE FACTOR BETWEEN TWO LINEAL UNITS
C
C * INPUT ........
C * INUNIT * UNIT CODE OF SOURCE.
C * IOUNIT * UNIT CODE OF TARGET.
C
C * OUTPUT .......
C * FACTOR * CONVERGENCE FACTOR FROM SOURCE TO TARGET.
C * IFLG   * RETURN FLAG .EQ. 0 , NORMAL RETURN.
C            RETURN FLAG .NE. 0 , ABNORMAL RETURN.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION FACTRS(6,6)
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      PARAMETER (ZERO = 0.0D0, MAXUNT = 6)
      DATA FACTRS /0.1000000000000000D01 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.2062648062470963D06 ,
     .             0.5729577951308231D02 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.1000000000000000D01 ,
     .             0.3048006096012192D00 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.1000002000004000D01 ,
     .             0.0000000000000000D00 , 0.3280833333333333D01 ,
     .             0.1000000000000000D01 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.3280839895013124D01 ,
     .             0.4848136811095360D-5 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.1000000000000000D01 ,
     .             0.2777777777777778D-3 , 0.0000000000000000D00 ,
     .             0.1745329251994330D-1 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.3600000000000000D04 ,
     .             0.1000000000000000D01 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.9999980000000000D00 ,
     .             0.3048000000000000D00 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.1000000000000000D01 /
C
      IF (INUNIT .GE. 0 .AND. INUNIT .LT. MAXUNT .AND.
     .    IOUNIT .GE. 0 .AND. IOUNIT .LT. MAXUNT) THEN
         FACTOR = FACTRS(IOUNIT+1 , INUNIT+1)
         IF (FACTOR .NE. ZERO) THEN
            IFLG = 0
            RETURN
         ELSE
            IF (IPEMSG .NE. 0) WRITE (IPELUN,2000) INUNIT,IOUNIT
 2000       FORMAT (' INCONSISTENT UNIT CODES = ',I6,' / ',I6)
            IFLG = 12
            RETURN
         END IF
      ELSE
         IF (INUNIT.LT.0 .OR. INUNIT.GE.MAXUNT) THEN
            IF (IPEMSG .NE. 0) WRITE (IPELUN,2010) INUNIT,IOUNIT
 2010       FORMAT (' ILLEGAL SOURCE OR TARGET UNIT CODE = ',I6,' / ',
     .              I6)
         END IF
         IF (IOUNIT.LT.0 .OR. IOUNIT.GE.MAXUNT) THEN
            IF (IPEMSG .NE. 0) WRITE (IPELUN,2010) IOUNIT,IOUNIT
         END IF
         IFLG = 11
         RETURN
      END IF
C
      END
