c-----------------------------------------------------------------------
c --- COMMON BLOCK /GRID/ -- Grid for output data                MAKEGEO
c-----------------------------------------------------------------------
      character*4 utmhem
      character*8 datum,pmap
      character*12 daten
      character*16 clat0,clon0,clat1,clat2

      common /GRID/ nx,ny,delx,xorg,yorg,
     &              izone,reflat,reflon,xlat1,xlat2,feast,fnorth,
     &              pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2

c-----------------------------------------------------------------------
c     DEFINITIONS  [i]=integer   [r]=real   [l]=logical   [c]=character
c-----------------------------------------------------------------------
c nx,ny            actual number of cells in x,y (or r,theta)        [i]
c delx        (km) length of side of output grid Cartesian grid cell [r]
c (x,y)origin (km) reference coordinates of grid origin              [r]
c izone            base zone for UTM grid                            [i]
c reflat     (deg) latitude of x=0 and y=0 of Lambert grid           [r]
c reflon     (deg) longitude of x=0 and y=0 of Lambert grid          [r]
c                  NOTE: longitude is East Longitude (neg in west hem)
c xlat1,xlat2(deg) standard latitudes used for Lambert conf. proj.   [r]
c feast  (km)      False Easting at projection origin                [r]
c fnorth (km)      False Northing at projection origin               [r]
c pmap             character code for output map projection          [c]
c                  UTM :  Universal Transverse Mercator
c                  LCC :  Lambert Conformal Conic
c                  PS  :  Polar Stereographic
c                  EM  :  Equatorial Mercator
c                  LAZA:  Lambert Azimuthal Equal Area
c                  TTM :  Tangential Transverse Mercator
c utmhem           base hemisphere for output UTM projection         [c]
c                  (S=southern, N=northern)
c datum            Datum-Region for grid coordinates                 [c]
c daten            NIMA date for datum parameters                    [c]
c clat0            character version of RLAT                         [c]
c clon0            character version of RLON                         [c]
c clat1            character version of XLAT1                        [c]
c clat2            character version of XLAT2                        [c]
c-----------------------------------------------------------------------
