c-----------------------------------------------------------------------
c --- COMMON BLOCK /CONTROL/ -- Program control data             MAKEGEO
c-----------------------------------------------------------------------
      logical lqacell,lterr,llu2,lqamiss
      logical lutm,llcc,lps,lem,llaza,lttm
      character*80 ctitle

      common /CONTROL/ lqacell,lterr,lutm,llcc,lps,lem,llaza,lttm,llu2,
     &                 lqamiss,iflip,nlx,nly,htfac,image,flumin,
     &                 ctitle

c-----------------------------------------------------------------------
c     DEFINITIONS  [i]=integer   [r]=real   [l]=logical   [c]=character
c-----------------------------------------------------------------------
c lqacell        flag indicating QA output for 1 grid cell           [l]
c lterr          flag indicating reading terrain data from file      [l]
c llu2           flag indicating supplental LU data file             [l]
c lutm           flag indicating Universal Transverse Mercator       [l]
c llcc           flag indicating Lambert Conformal Conic             [l]
c lps            flag indicating Polar Stereographic                 [l]
c lem            flag indicating Equatorial Mercator                 [l]
c llaza          flag indicating Lambert Azimuthal Equal Area        [l]
c lttm           flag indicating Tangential Transverse Mercator      [l]
c lqamiss        flag indicating QA output for missing cells         [l]
c iflip          location of first point in TERR.DAT file            [i]
c                   0 = SW corner of grid
c                   1 = NW corner of grid (TERREL format)
c nlx,nly        location of grid cell for QA output                 [i]
c htfac          factor to convert terrain elevations to meters (MSL)[r]
c IMAGE          Output GRD file format for SURFER IMAGE maps        [i]
c                  0 = Standard GRD (for SURFER 8)
c                  1 = GRD ranges shifted from cell-centers to
c                      cell-edges (for SURFER 7)
c flumin         Minimum total land use fraction for a valid cell    [r]
c                that is not missing.  Cells with fractional LU
c                totalling FLUMIN or more are renormalized to 1.0.
c ctitle         1-line title written to GEO.DAT file                [c]
c-----------------------------------------------------------------------
