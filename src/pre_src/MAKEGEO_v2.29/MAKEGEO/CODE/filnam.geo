c-----------------------------------------------------------------------
c --- COMMON BLOCK /FILNAM/ -- File names                       MAKEGEO
c-----------------------------------------------------------------------
      character*132 runinp,runlst,ludat,lu2dat,terrdat,geodat,
     &             lugrd,tegrd
      logical lcfiles

      common /FILNAM/ runinp,runlst,ludat,lu2dat,terrdat,
     &                geodat,lugrd,tegrd
      common /FILLOG/ lcfiles

c-----------------------------------------------------------------------
c     DEFINITIONS  [i]=integer   [r]=real   [l]=logical   [c]=character
c-----------------------------------------------------------------------
c runinp         Path & filename for the control file                [c]
c                (default: MAKEGEO.INP)
c runlst         Path & filename for the output MAKEGEO list file    [c]
c                (default: MAKEGEO.LST)
c ludat          Path & filename for the input LULC data file        [c]
c                (default: LU.DAT)
c lu2dat         Path & filename for the supplemental LULC data file [c]
c                (default: LU2.DAT)
c terrdat        Path & filename for terrain data file               [c]
c                (default: TERR.DAT)
c geodat         Path & filename for the output GEO.DAT file         [c]
c                (default: GEO.DAT)
c lugrd          Path & filename for the output Land Use GRD file    [c]
c                (default: QALUSE.GRD)
c tegrd          Path & filename for the output Terrain GRD file     [c]
c                (default: QATERR.GRD)
c lcfiles        Switch indicating if all characters in the          [l]
c                filenames are to be converted to lower case
c                letters (LCFILES=T) or converted to UPPER
c                case letters (LCFILES=F).
c-----------------------------------------------------------------------
