c-----------------------------------------------------------------------
c --- TERREL -- Terrain Elevation Preprocessor
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684           Level: 070327              MAIN
c 
c     Copyright (c) 1994-2007 by Exponent, Inc.
c
c
c --- PURPOSE: TERREL coordinates the allocation of terrain elevation
c              data (m MSL) from several digitized data bases to a
c              user-supplied modeling grid.
c
c --- Updates
c
c --- Ver. 3.683 Level 061113 to Ver. 3.684 Level 070327
c                                                       (D. Strimaitis)
c              - CALUTILS from v2.54 Level 061020 to v2.55 Level 070327
c                Fixed format bug in subroutine BASRUTC for the case of
c                time zone zero (output string was 'UTC+0  0' instead of
c                'UTC+0000'
c                (no change for TERREL -- no time zones used)
c                Modified:  UTCBASR, BASRUTC
c              - COORDLIB from v1.97 Level 060626 to v1.98 Level 060911
c                Changes in COORDS that allow a higher level of FORTRAN error
c                checking.  Compiler checks had identified constant arguments
c                and 2 uninitialized variables.  None of these is known to
c                have produced errors in cooerdinate conversions.
c                Modified:  COORDS
c
c --- Ver. 3.682 Level 060519 to Ver. 3.683 Level 061113
c                                                       (D. Strimaitis)
c              - UTM zone values passed into GLOBE1 were set to the same
c                variable name, so that no UTM zone translation was done
c                when input DEM data are in UTM map projection
c                Note: code assumes that utm zone read from DEM header
c                is a signed integer (negative in S. hemispere)
c                Modified:  RDUSGSHD
c              - Output UTM zone value passed into GLOBE1 when
c                processing the Canadian DMDF dataset should be signed
c                integer, where negatives denote the S. hemisphere
c                (since this dataset format is N. hemisphere, this
c                change should have no effect)
c                Modified:  LOADDMDF
c              - Output UTM zone value passed into GLOBE1 when
c                processing the New Zealand generic dataset was not
c                assigned (zero in many compilers)
c                Modified:  LOADNZGNR
c              - CALUTILS from v2.52 Level 060519 to v2.54 Level 061020
c                Move GLOBE1 to COORDLIB
c                Allow negative increments in INCRS
c                Modified:  INCRS
c                Removed:   GLOBE1
c              - COORDLIB from v1.95 Level 050126 to v1.97 Level 060626
c                Add Albers Conical Equal Area projection
c                Add GLOBE1 (from CALUTILS)
c
c --- Ver. 3.681 Level 060202 to Ver. 3.682 Level 060519
c                                                       (D. Strimaitis)
c              - CALUTILS from v2.51 Level 051019 to v2.52 Level 060519
c                Variable names in control file are not processed
c                correctly if there are too many characters (including
c                blanks) to the left of the "=" sign (run stops in
c                setup phase).
c                Modified:  READIN
c
c --- Ver. 3.68 Level 051201 to Ver. 3.681 Level 060202
c                                                       (D. Strimaitis)
c              - Change filename strings from c*70 to c*132
c                This is required by CALUTILS 2.3 and later which uses
c                c*132 in FILCASE and COMLINE
c                Modified:  FILNAM.TRL
c                           SETUP, READCF
c --- Ver. 3.67 Level 050824 to Ver. 3.68 Level 051201
c                                                       (K. Morrison)
c              - Correction in COASTEXT when there is only a single
c                surrounding polygon (i.e. coastline processing was
c                not needed)
c              - Argument list for SETTRANO changed to be consistent 
c                CTGPROC Ver. 2.65 051201
c
c --- Ver. 3.66 Level 050817 to Ver. 3.67 Level 050824
c                                                       (K. Morrison)
c              - Add option to echo data points in (X,Y) grid units to
c                an output datafile RAWECHO.DAT, affecting:
c                PARAMS.TRL, CONTROL.TRL, FILNAM.TRL,
c                SETUP, READCF, WRTHEAD, SAVEELEV, BLOCK DATA
c
c --- Ver. 3.65 Level 050815 to Ver. 3.66 Level 050817
c                                                       (K. Morrison)
c              - Change unused variables to avoid compiler warnings in:
c                SETSRTM, LOADNZGNR, LOADGNR, LOADLAZA, LOADSTRP,
c                LOADSRTM, XYIN, LBIG_END, COASTRD, COASTEXT
c              - Remove test of coastline processing in SAVEELEV, since
c                type defaults to land if no processing done and noise
c                and default testing may still be desired
c              - Modify error message in COASTRD in case polygon headers
c                are incomplete, manually-produced BLN file only
c              - Set cell counts to 0 in HEIGHTS if noise processing
c                resets height to void 
c
c --- Ver. 3.64 Level 050526 to Ver. 3.65 Level 050815
c                                                       (K. Morrison)
c              - Corrections in case domain crosses the international
c                date line, affecting:
c                GRID common, SPAN, SETARM3, SET3SEC, SETGLOB, SETSRTM,
c                SETUSGSG (GRID common also added), RDUSGSHD, LOADNZGNR,
c                LOADGNR, LOADSTRP, GET4CNR
c              - Add a note in SETUP if a BLN file is being read in
c              - Corrections to HEIGHTS to catch properly voids if
c                void and/or noise correction is not used
c              - Correct the UTM type test in LOADGNR from 3 (LL) to 2 (Km)
c              - Correct the call to OUT in HCOPY so that the first
c                argument is the HT array instead of XDUM even though
c                not used (allows debugging)
c              - Change the final loop in LOADSTRP to NPT (actual number
c                of points) instead of MXNP (maximum number) to avoid
c                undefined values (allows debugging)
c              - Constrain main loop in LOADLAZA to actual points
c                (allows debugging)
c              - Constrain main loop in GET4CNR to actual points
c                (allows debugging)
c
c --- Ver. 3.63 Level 050428 to Ver. 3.64 Level 050526
c                                                       (K. Morrison)
c              - Correction in CELLFILL to use Nearest Neighbour for
c                void cells on the outside boundries of the domain
c              - Change default ITERREP for oceans from 2 to 3 (always
c                replace) in BLOCK DATA
c              - Add check and reset for oceans in HEIGHTS when ITERREP
c                value equals 3
c
c --- Ver. 3.62 Level 050128 to Ver. 3.63 Level 050428
c                                                       (K. Morrison)
c              - Correction in COASTEXT to catch GSHHS polygons that
c                straddle Greenwich because GLOBE changes negative input
c                longitudes to positive output longitudes, not desired
c                when the domain straddles or is East of Greenwich.
c
c --- Ver. 3.61 Level 041013 to Ver. 3.62 Level 050128
c                                                       (D. Strimaitis)
c              - COORDLIB updated to stop UTM conversions with a DATUM
c                that is not mapped to the list in the USGS UTM subr.
c                An example is the sphere datum NWS-84 (Earth radius
c                6370km), since only the sphere datum ESR-S (Earth
c                radius 6371km) is available.  Unmapped datums had
c                defaulted to the Clarke 1866 spheroid.
c                LAZA Projection:  removed assignment of 6370 km earth
c                radius (NWS-84 datum) when a value less than 6000 km is
c                found.  This assignment can override a requested radius
c                of 6371 (ESR-S datum) if the NWS-84 datum is used with
c                any valid projection prior to the request for ESR-S.
c                LAZA(NWS-84) coordinate distances from the projection
c                origin are about 0.016% smaller than LAZA(ESR-S).
c                Error message and version strings added to COORDS calls
c                and new subroutine COORDSVER to report COORDS version
c                documentation.
c                (Version 1.95, Level 050126)
c              - Added call to COORDSVER to access the COORDS version
c                info and passed string to list file and comment section
c                of output file.
c                Modified: READCF, WRTHEAD
c              - Initialize LSURRSIDE as an array, and add array bound
c                checks against MXCOAST, MXCOASTP
c              - Use only the first 2 land characterization types for
c                INOISEREP, ZNOISE, ITERREP, TERDEF arrays (currently
c                only distinguishing marine water and land).
c                Modified: READCF, SETUP
c
c --- Ver. 3.6 Level 040920 to Ver. 3.61 Level 041013
c                                                       (K. Morrison)
c              - Bug fix:  change all UTM zone values passed into GLOBE1
c                to signed integers, where negatives denote the S.
c                hemisphere.  This bug could cause terrain extraction
c                to fail in the S. hemisphere when COORDLIB version 1.93
c                or later is used.
c              - Modifications to COASTEXT and BREADH to accept both
c                old and new GSHHS.
c              - Modify COASTEXT to allow WDBII processing via a logical,
c                but hardwire it to .false. for now
c                                                       (D. Strimaitis)
c              - COORDLIB updated to fix UTM conversion in the S.
c                hemisphere when the ouput UTM zone is forced as N. 
c                hemisphere, and the DATUM changes (completes changes
c                started in version 1.93).
c                (Version: 1.94, Level: 041007)
c              - Change default Datum for the SRTM data from WGS-84 to
c                WGS-96 (matches control file default).
c              - Fix subroutine SETUSGS format statement 1001 to write
c                integers instead of reals.
c
c --- Ver. 3.59 Level 040901 to Ver. 3.6 Level 040920
c              - Final corrections to polygon closing in COASTEXT
c                                                       (K. Morrison)
c              - Redo logic for "3 arc-second" USGS DEM so that header
c                information is fully interpreted (will handle any true
c                USGS DEM format file that is based on lat/lon)
c                (LOAD, RDUSGSHD)                       (D. Strimaitis)
c              - COORDLIB updated to respond to UTM conversion across
c                the equator from S. hemisphere to N. hemisphere, when
c                the S. hemisphere zone is forced.  Also fixed a problem
c                with the conversion to/from spherical NWS-84 datum when
c                using UTM projection (USGS program input array
c                conflicts).                            (D. Strimaitis)
c                (Version: 1.93, Level: 040713)
c
c --- Ver. 3.57 Level 040429 to Ver. 3.59 Level 040901
c              - Corrections to polygon closing in COASTEXT (K. Morrison)
c              - Relaxing polygon limits in BLN file COASTRD to be more
c                realistic and to allow BLNs that may come from elsewhere
c --- Ver. 3.56 Level 040311 to Ver. 3.57 Level 040429
c              - Disable use of WDBII data in COASTEXT      (K. Morrison)
c --- Ver. 3.55 Level 040305 to Ver. 3.56 Level 040311
c              - Corrections to coastal point treatment     (K. Morrison)
c --- Ver. 3.54 Level 040303 to Ver. 3.55 Level 040305
c              - Add noisy point processing
c              - Add point counters                         (K. Morrison)
c --- Ver. 3.53 Level 040302 to Ver. 3.54 Level 040303
c              - Correct bug for the surrounding polygon in COASTEXT
c              - Add simple test in SAVEELEV to skip outside points
c                (K. Morrison)
c --- Ver. 3.52 Level 040229 to Ver. 3.53 Level 040302
c              - Corrections in READCF to Group (1) dictionary
c              - Add option to read already-existing BLN coast file (K. Morrison)
c --- Ver. 3.51 Level 040228 to Ver. 3.52 Level 040229
c              - Explicitly define the surrounding polygon in COASTEXT
c                if the zone is surrounded without a coastline intersection
c              - Change IPTYPE to reflect the explicit polygon (K. Morrison)
c --- Ver 3.5 Level 040131 to Ver 3.51 Level 040228
c              - Add option for coastline processing based on GSHHS
c                dataset
c              - Add output of BLN file if coastline processing is chosen
c              - Add radius variable for cell interpolation
c              - Change TERDEF to array by water/land type, and allow 
c                different application of default by type      (K. Morrison)
c --- Ver 3.5 Level 031031 to Level 040131
c              - Correct treatment in eastern hemisphere       (K. Morrison)
c --- Ver 3.4 Level 031023 to Ver 3.5 Level 031031
c              - Add option to fill cells by interpolation
c              - General simplification and modularization of COMP (K. Morrison)
c --- Ver 3.4 Level 031017 to Level 031023
c              - Add default value TERDEF for missing cell and discrete
c                receptors to replace -999              (K. Morrison)
c                Modified SETUP, READCF, HEIGHTS, XYOUTPUT, /CELL/  
c --- Ver 3.312 Level 030905 to Ver 3.4 Level 031017
c              - Treat missing value indicator in USGS DEM files
c                Modified LOAD, CELLDAT, XYELEV         (D. Strimaitis)
c              - Initialize elevations for the XY-file option with -999.
c                Modified XYIN                          (D. Strimaitis)
c              - Added SRTM 1-sec and 3-sec processing  (K. Morrison)
c              - Test for ENDIAN of machine
c                Added LBIG_END, Modified SETGLOB
c              - Added interpolation option for discrete receptors
c                Added INTERP2D, Modified CELLIN, CELLOUT, XYELEV, XYOUTPUT
c              - Check number of discrete receptors in XYIN, and
c                check number of input columns
c              - Grid cells with no data have elevations of -999.
c
c --- Ver 3.311 Level 030709 to Ver 3.312 Level 030905  D. Strimaitis
c              - DATUMs updated COORDLIB (Version: 1.9, Level: 030905)
c              - Default DATUMs reset
c
c --- Ver 3.31 Level 030528 to Ver 3.311 Level 030709  D. Strimaitis
c              - Remove restriction on using LCC projection with 7.5
c                minute DEM data and Canadiam 100m data
c
c --- Ver 3.3 Level 030402 to Ver 3.31 Level 030528 - D. Strimaitis
c              - CALUTILS (Version: 2.2, Level: 030528)
c              - COORDLIB (Version: 1.15, Level: 030528)
c
c --- Ver 3.2 Level 021018 to Ver 3.3 Level 030402    - D. Strimaitis
c              - Updated CALUTILS (Version: 2.1, Level: 030402)
c              - COORDS95 --> COORDLIB (Version: 1.14, Level: 030402)
c              - New header for output data file (TERREL.DAT)
c              - Add false Easting and Northing (GLOBE1)
c              - Add TYPE argument to XTRACTLL
c              - FIN:  list file unit number added to JULDAY
c              - Default plot file name changed to QATERR.GRD
c
c --- Ver 3.1 Level 020730 to Ver 3.2 Level 021018    - D. Strimaitis
c              - Incorporate revised COORDS implementation:
c                   full DATUM conversion
c                   UTM, LCC, PS, EM, TTM, and LAZA map projections
c              - Updated CALUTILS (Version 2.0, Level 021018)
c
c --- Ver 3.0 Level 020627 to Ver 3.1 Level 020730    - D. Strimaitis
c              - Place OPEN attributes for binary files in PARAMETERs
c                to allow code to be configured for various compilers
c              - Use explicit format for 1st header of output GRD files
c              - Fixed DEM-2 modification to subroutine CORNERS
c
c --- Ver 3.0 Level 020513 to Ver 3.0 Level 020627    - D. Strimaitis
c              - Fixed assignment of LPOLR based on IGRID value
c
c --- Ver 3.0 Level 020318 to Ver 3.0 Level 020513    - D. Strimaitis
c              - Combined the 'DEM-2' (~90m ; 3 arc-sec) processing
c                structure with the standard DEM structure
c
c --- Ver 3.0 Level 020315 to Ver 3.0 Level 020318    - C. Czaja
c              - Revised the 'DEM-2' (~90m ; 3 arc-sec) input parameters
c                for variable numbers of strips and number of points in
c                each strip. Changes made in RDUSGSHD; in LOAD and a few
c                lines after calling LOAD.
c
c --- Ver 3.0 Level 010115 to Ver 3.0 Level 020315    - D. Strimaitis
c              - Use NINT function when converting corner lat/lons from
c                seconds to integer degrees
c
c --- Ver 2.1 Level 010115 to Ver 3.0 Level 010713    - D. Strimaitis
c              - Restructure inputs for CALPUFF system control file
c              - Restructure main program as subroutine COMP
c              - Place system-wide utilities into an include module
c                (calutil.for)
c
c --- Ver 2.1 Level 001207 to Ver 2.1 Level 010115    - S. Du
c              - Modified to allow different discrete receptor fields 
c                (two columns: x,y) 
c                or (four columns: x,y,elevation,receptor_height).
c
c --- Ver 2.1 Level 001204 to Ver 2.1 Level 001207    - S. Du
c              - Add support for the DEM-2 (~90 m) terrain data
c                DEM-2 can be considered as a special form of 
c                USGS 1-deg (~90m), but it has irregular 4 corners
c                and probably irregular number of data point on 
c                each strip
c
c --- Ver 2.1 Level 001004 to Ver 2.1 Level 001204    - S. Du
c              - Add support for the New Zealand generic terrain data
c                which contain four columns of data:
c                row #,elevation,lat,long
c              - To move the generic terrain data to the last of all
c                acceptable input data
c
c --- Ver 2.1 Level 000707 to Ver 2.1 Level 001004    - S. Du
c              - Add support for the Generic terrain data
c                A header must be added to each of 
c                of the generic data files to indicate the type 
c                of coordinates and the unit of elation.
c                The header should have three numbers, the first one 
c                is the type of coordinates 
c                (1=UTM in meters, 2=UTM in kilometers,
c                 3=Lat_Long);
c                The second one is the unit for elevation
c                (1=meter, 2=foot) 
c                The third one is the zone number of UTM coordinates
c                (omitted if Lat_Long is used) 
c      
c                The generic terrain data file is in the format of 
c                  X(eastern),Y(northern),Z(elevation)
c
c --- Ver 2.1 Level 000411 to Ver 2.1 Level 000707    - S. Du
c              - Add support for the USGS global 1000-m data in 
c                Lambert Azimuthal Eqaul Area Projection 
c                (image file)
c
c --- Ver 2.1 Level 000320 to Ver 2.1 Level 000411    - D. Strimaitis
c              - Use sampling info from header of USGS DEM 1-deg
c                files to treat 'non-standard' structure at high
c                latitudes (e.g. Alaska)
c              - Define independent step-size along and between strips
c              - Replace 3CD filenames with new DEM filenames
c              - Revise filename logic to treat branch at 180 deg.
c              - Change USGS '30m' references to USGS '7_5' for 7.5
c                minute quadrangle files
c
c --- Ver 2.1 Level 000112 to Ver 2.1 Level 000320    - D. Strimaitis
c              - Introduce scale factor (ZFAC) for heights read from
c                USGS DEM files (usually 1.0)
c
c --- Ver 2.1 Level 000107 to Ver 2.1 Level 000112    - D. Strimaitis
c              - Enlarge format field for reporting 3CD file names
c
c --- Ver 2.0 Level 990729 to Ver 2.1 Level 000107    - D. Strimaitis
c              - Add option to process discrete locations whereby the
c                peak elevation within a user-provided search radius
c                is assigned to the locations read from a new file type
c
c --- Ver 2.0 Level 990528 to Ver 2.0 Level 990729    - B. de Foy
c              - Use subroutine MAPL2G & MAPG2L taken from CALMET 990228
c                For correct LCC - UTM conversion in Southern Hemisphere
c              - Changes to MAPG2L, LL2UTM, UTM2LL
c
c --- Ver 2.0 Level 990130 to Ver 2.0 Level 990528   - J. Chang
c              - Add support for the USGS global 30 arc-sec DEM data
c                (binary in big endian)
c --- Ver 2.0 Level 981025 to Ver 2.0 Level 990130   - J. Scire
c              - Integrate DGS changes (981025) with SD changes (8/26/98)
c              - Remove Level 3 warning on USGS data
c              - Remove code producing compiler warnings
c              - Remove unused code (general clean-up)
c              - Modify OUT to allow NX, NY >= 100
c              - Restructure list-file QA reporting (DGS)
c
c --- Ver 2.0 Modification is based on level 980304  - S. Du
c              Modified to allow Canadian DMDF (~100m) terrain data. 
c              UTM coordinate is used for the reference system.
c                
c --- Ver 2.0 Level 971029 to Ver 2.0 Level980304        EMI
c              Modified to use real arrays instead of integer*2 arrays
c                to store the strip data and process it.  Feet to meters
c   (bug fixed)  conversions (if necessary) are also done in Subr. LOAD
c                before the data is processed.  All output data are in
c                f8.1 format in meters.  The following subroutines were
c                modified to handle the processing using real arrays:
c                MAIN, CELLIN, CELLOUT, CELLDAT, HEIGHTS, FILLANG, HCOPY,
c                MODOUT, and PLTOUT
c              PARAMS.TRL was also modified. CELL common was updated to
c                include real array HTMAX instead of integer*2 IHMAX.
c
c --- Ver 2.0 Level 970505 to Ver 2.0 Level 971029        EMI
c              SETUP   - convert corner coordinates to meters for polar grids
c
c --- Ver 2.0  Level 961113 to Ver 2.0 Level 970505        DGS
c              CELLIND - integer truncation returns 0 for -1.0<real<1.0
c                        so that first row/column of cells in the grid
c                        contained data just outside the grid in the
c                        reported average; problem fixed.
c
c               E. Insley -- update from V1.1 to V2.0
c                 * allows USGS data to be read
c                 * identifies USGS file coverage necessary for domain
c                 * loops over all sheets for 30m, 90m and 900m data
c                 * allows 30m, 90m and 900m data to be processed together
c
c-----------------------------------------------------------------------
      Program TERREL
c
c --- Include parameters
      include 'params.trl'
c --- Include common blocks
      include 'qa.trl'
c
c --- Set version and level number of program (stored in /QA/ and
c --- checked against values set in PARAMS.TRL)
      ver='3.684'
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
c --- TERREL    Version: 3.684          Level: 050824       BLOCK DATA
c               D. Strimaitis, Earth Tech
c
c --- Include parameter statements
      include 'params.trl'
c
c --- Include common blocks
      include 'filnam.trl'
      include 'cell.trl'
      include 'control.trl'
      include 'grid.trl'
      include 'xy.trl'
      include 'shores.trl'

c --- FILNAM common block
      data runinp/'terrel.inp'/,outfil/'terrel.dat'/,
     1     lstfil/'terrel.lst'/,pltfil/'qaterr.grd'/,
     2     prevfil/'prev.sav'/,savefil/'terrel.sav'/,
     3     xyinp/'xyinp.dat'/,xyout/'xyout.dat'/,
     4     gshhsin/'gshhs_f.b'/coastbln/'coast.bln'/,
     5     ntdf/0/

c --- FILLOG common block
      data lcfiles/.true./

c --- CELL common block
      data ithres/75/,terdef/5*0./,iterrep/3,0,0,0,0/
      data inoiserep/5*0/,znoise/0.,1.,1.,1.,1./

c --- CONTROL common block
c --- Primary variables
      data lprev/.false./, lvoidfil/.false./, lxy/.false./
      data lintxy/.false./, lcoast/.false./, lblnread/.false./
      data lrawecho/.false./
      data imodel/1/, igrid/1/, iproc/2/
      data pmap/'UTM     '/
      data datum/'WGS-84'/
      data dusgs90/'WGS-72'/, dusgs30/'NAS-C'/, darm3/'NAS-C'/
      data d3cd/'WGS-72'/, ddmdf/'NAS-C'/, dgtopo30/'WGS-84'/
      data dusgsla/'ESR-S'/, dnzgen/'WGS-84'/, dgen/'WGS-84'/
      data dsrtm1/'WGS-96'/, dsrtm3/'WGS-96'/
      data dwvs/'WGS-84'/, dwdbii/'WGS-72'/
c --- Derived variables
      data lutm/.false./, llcc/.false./, lps/.false./
      data lem/.false./, llaza/.false./, lttm/.false./
      data l30sec/.false./, l3sec/.false./, l7_5min/.false./
      data l100met/.false./, l3cd/.false./, lgtopo30/.false./
      data lnzgnr/.false./, lgnr/.false./
      data lusgsglb/.false./, lsrtm1/.false./, lsrtm3/.false./
      data lnuat/.false./, lcmet/.false./, lmeso/.false./
      data liscc/.false./, liscp/.false./, lgenr/.false./
      data lfeet/.false./, lpeak/.false./, lpolr/.false./
      data lcent/.false./, lpush/.false./

c --- GRID common block
      data utmhem/'N   '/
      data rlat/-999./,rlon/-999./,xlat1/-999./,xlat2/-999./
      data feast/0.0/, fnorth/0.0/
      data npextr/0/,npnoise/0/,npmiss/0/

c --- XY common block
      data nxycol/4/

c --- SHORES common block
      data nshore/0/
      data pxmax,pxmin/mxcoast*-1.e30,mxcoast*1.e30/
      data pymax,pymin/mxcoast*-1.e30,mxcoast*1.e30/

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
c --- TERREL    Version: 3.684          Level: 060202             SETUP
c               D. Strimaitis, SRC
c
c
c PURPOSE:     SETUP reads and checks the control data provided, sets
c              logicals, echoes the control data, and opens the data
c              files if inputs are valid.
c
c --- Updates
c     Ver 3.67 Level 050824 to Ver 3.681 Level 060202       DGS
c              - Change filename strings from c*70 to c*132
c              - This is required by CALUTILS 2.3 and later which uses
c              - c*132 in FILCASE and COMLINE
c --- Ver. 3.65 Level 050815 to Ver. 3.67 Level 050824  KAM
c              - Add opening of raw data echo file, and echo file name
c                to list file as needed
c              - For coastline processing, in the input file list echoed
c                to the list file, indicate either the GSHHS file or the
c                BLN file depending on which is being read, and if the
c                BLN file is being read in, do not list it under output
c                files
c --- Ver. 3.63 Level 050128 to Ver. 3.65 Level 050815  KAM
c              - Add a note to the screen if a BLN file is being read in
c --- Ver. 3.55 Level 040305 to Ver. 3.62 Level 050128  DGS
c              - Use only the first 2 land characterization types for
c                INOISEREP, ZNOISE, ITERREP, TERDEF arrays (currently
c                only distinguishing marine water and land)
c --- Ver. 3.53 Level 040302 to Ver. 3.55 Level 040305  KAM
c              - Add noisy data processing
c --- Ver. 3.51 Level 040228 to Ver. 3.53 Level 040302  KAM
c              - Add option to read already-existing BLN coast file
c --- Ver 3.4 Level 031017 to Ver 3.51 Level 040228  KAM
c              - Add coastline processing
c              - Change TERDEF to array by water/land type
c --- Ver 3.3 Level 030402 to Ver 3.4 Level 031017  KAM
c              - Add SRTM 1-sec and 3-sec processing
c              - Read discrete locations before reading save file
c              - Add default value TERDEF for missing elevations at
c                end of processing
c --- Ver 3.3  Level 021018 to 030402       DGS
c              New output file header (WRTHEAD)
c              False Easting/Northing reported to list file
c --- Ver 3.2  Level 010713 to 021018       DGS
c              Incorporate full COORDS implementation:
c                -  full DATUM conversion
c                -  UTM, LCC, PS, EM, TTM, and LAZA map projections
c     Ver 3.0  Level 000107 to 010713       DGS
c              Add control file reader (READCF)
c              Move in calls to SPAN, FINDFILE, CELLIN, XYIN
c              
c     Ver 2.0  Level 990528 to 000107       DGS
c              Add discrete location processing option
c              Change USGS '30m' references to USGS '7_5' for 7.5
c              minute quadrangle files
c
c ARGUMENTS:
c    PASSED:  none
c
c  RETURNED:  /CONTROL/   logicals
c             /GRID/      data
c
c CALLING ROUTINES:   MAIN
c
c EXTERNAL ROUTINES:  READCF, SPAN, FINDFILE, CELLIN, XYIN, WRTHEAD,
c                     COASTEXT
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'cell.trl'
      include 'control.trl'
      include 'filnam.trl'
      include 'grid.trl'
      include 'qa.trl'
      include 'xy.trl'

c --- Local Variables
      character*16 cgrid(3),cmodel(6),cproc(2)
      real cbdisk(mxnx)

c --- Set text for reporting input options
      data cgrid/'Cartesian-Corner','Cartesian-Center','Polar'/
      data cmodel/'CALMET','MESOPAC','ISC Polar','ISC Cartesian',
     &            'NUATMOS','Generic'/
      data cproc/'Normal','Screen'/

c --- Get date and time from system                     --- call DATETM
      call DATETM(rdate,rtime,rcpu)

c --- Get the name of the control file from the
c --- command line                                     --- call COMLINE
      call COMLINE(runinp)

c --- Open the control file
      open(iocnt,file=runinp,status='old')

c --- Report progress
      write(iomesg,*)'SETUP PHASE'

c --- Check that the version and level number in the parameter
c --- file matches those in the code itself
      if(ver.ne.mver.or.level.ne.mlevel)then
         write(iomesg,10) ver,level,mver,mlevel
10       format(/1x,'ERROR in SUBR. SETUP -- The TERREL version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open list file)                --- call READCF
      call READCF

c --- Open remaining files (not terrain DB files)
      open(ioout,file=outfil)
      open(ioplt,file=pltfil)
      open(iosav,file=savefil,form='unformatted')
      if(LPREV) then
         open(ioprev,file=prevfil,form='unformatted',status='old')
      endif
      if(LXY) then
         open(ioxyi,file=xyinp,status='old')
         open(ioxyo,file=xyout)
      endif
      if(LRAWECHO) then
         open(iorawo,file=rawecho)
      endif

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
      write(iolst,*) 'Processing Options -----'
      write(iolst,*) 'igrid   : ' ,cgrid(igrid)
      write(iolst,*) 'imodel  : ' ,cmodel(imodel)
      write(iolst,*) 'LXY     : ' ,lxy
      if(LXY) then
         write(iolst,*) 'LINTXY  : ' ,lintxy
         write(iolst,*) 'nxycol  : ' ,nxycol
         write(iolst,*) 'xyradkm : ' ,xyradkm
      endif

      write(iolst,*)
      write(iolst,*) 'Continuation Run?   :  ',lprev
      write(iolst,*)
      write(iolst,*) 'Interpolate voids?  :  ',lvoidfil
      write(iolst,*)
      write(iolst,*) 'QA Threshold (%)    =  ',ithres
      write(iolst,*)

      write(iolst,*) 'Coastal processing? :  ',lcoast
      write(iolst,*)
      write(iolst,*) 'Read BLN coast data?:  ',lblnread

      write(iolst,*)
      write(iolst,*) 'Minimum elevation for noise & replacement option'
      write(iolst,*) '  - ocean           = ',znoise(1),', ',
     &                                        inoiserep(1)
      write(iolst,*) '  - mainland        = ',znoise(2),', ',
     &                                        inoiserep(2)

c --- Using just the first 2 for now ...
c      write(iolst,*) '  - lake            = ',znoise(3),', ',
c     &                                        inoiserep(3)
c      write(iolst,*) '  - island in lake  = ',znoise(4),', ',
c     &                                        inoiserep(4)
c      write(iolst,*) '  - pond on island  = ',znoise(5),', ',
c     &                                        inoiserep(5)

      write(iolst,*)
      write(iolst,*) 'Default elevation & replacement option'
      write(iolst,*) '  - ocean           = ',terdef(1),', ',iterrep(1)
      write(iolst,*) '  - mainland        = ',terdef(2),', ',iterrep(2)

c --- Using just the first 2 for now ...
c      write(iolst,*) '  - lake            = ',terdef(3),', ',iterrep(3)
c      write(iolst,*) '  - island in lake  = ',terdef(4),', ',iterrep(4)
c      write(iolst,*) '  - pond on island  = ',terdef(5),', ',iterrep(5)

      write(iolst,*)
      write(iolst,*) 'Terrain Data Input File Names -------------'
      if(LXY) write(iolst,*) 'xyinp    : ' ,xyinp
      do i=1,ntdf
        write(iolst,'(1x,a10,a6,3x,a132)')
     &       'dbfile   : ',datatyp(i),datafil(i)
      enddo   
      if(LCOAST) then
        if(.NOT.LBLNREAD) then
          write(iolst,*) 'gshhsin  : ' ,gshhsin
        else
          write(iolst,*) 'coastbln : ' ,coastbln
        endif
      endif
      write(iolst,*)
      write(iolst,*) 'Default Datum-Region for each File Type --------'
      write(iolst,*) 'USGS90 : ',dusgs90
      write(iolst,*) 'USGS30 : ',dusgs30
      write(iolst,*) 'ARM3   : ',darm3
      write(iolst,*) '3CD    : ',d3cd
      write(iolst,*) 'DMDF   : ',ddmdf
      write(iolst,*) 'GTOPO30: ',dgtopo30
      write(iolst,*) 'USGSLA : ',dusgsla
      write(iolst,*) 'SRTM1  : ',dsrtm1
      write(iolst,*) 'SRTM3  : ',dsrtm3
      write(iolst,*) 'NZGEN  : ',dnzgen
      write(iolst,*) 'GEN    : ',dgen
      write(iolst,*) 'Coastal Data'
      write(iolst,*) 'WVS    : ',dwvs
      write(iolst,*) 'WDBII  : ',dwdbii

      if(LPREV) then
         write(iolst,*)
         write(iolst,*) 'Previous save-file read as input: ',prevfil
      endif

      write(iolst,*)
      write(iolst,*) 'Output File Names -------------'
      write(iolst,*) 'lstfil   : ' ,lstfil
      write(iolst,*) 'pltfil   : ' ,pltfil
      write(iolst,*) 'outfil   : ' ,outfil
      write(iolst,*) 'savefil  : ' ,savefil
      if(LRAWECHO) write(iolst,*) 'rawecho  : ',rawecho
      if(LXY) write(iolst,*) 'xyout    : ' ,xyout
      if(LCOAST.AND..NOT.LBLNREAD) write(iolst,*) 'blnfil   : ' ,
     &    coastbln

      write(iolst,*)
      write(iolst,*) 'Grid Info (for output) ---------------------'
      write(iolst,*) 'datum      : ' ,datum
      write(iolst,*) 'pmap       : ' ,pmap
      if(LUTM) then
         write(iolst,*) 'Hemisphere : ',utmhem
         write(iolst,*) 'UTM zone   : ' ,izone
      endif
      write(iolst,*) 'xorgk      : ' ,xorgk
      write(iolst,*) 'yorgk      : ' ,yorgk
      write(iolst,*) 'sizek      : ' ,sizek
      write(iolst,*) 'nx         : ' ,nx
      write(iolst,*) 'ny         : ' ,ny
      if(LLCC.or.LLAZA.or.LTTM) then
         write(iolst,*) 'fEast      : ' ,feast
         write(iolst,*) 'fNorth     : ' ,fnorth
      endif
      if(LLCC.or.LPS.or.LEM.or.LLAZA.or.LTTM) then
         write(iolst,*) 'rlat(N)    : ' ,rlat
         write(iolst,*) 'rlon(E)    : ' ,rlon
         if(LLCC.or.LPS)write(iolst,*) 'xlat1      : ' ,xlat1
         if(LLCC)write(iolst,*) 'xlat2      : ' ,xlat2
      endif
      if(LPOLR)then
         write(iolst,*) 'Polar Grid ---'
         write(iolst,*) 'iproc      : ' ,cproc(iproc)
         write(iolst,*) 'disk       : ' ,(disk(i),i=1,nx)
         write(iolst,*) 'ang        : ' ,(ang(i),i=1,ny)
      endif

c -----------------------------
c --- Process control data
c -----------------------------

c --- Set cell boundaries (cbdis,cbang) for polar grid
      if(LPOLR) then
c ---    Rings
         do i=1,nx-1
            if(LPUSH) then
               cbdisk(i)=disk(i+1)                 ! Screening Method
               cbdisk(nx)=2.*disk(nx)-disk(nx-1)
            else
               cbdisk(i)=0.5*(disk(i)+disk(i+1))   ! Normal Method
               cbdisk(nx)=2.*disk(nx)-cbdisk(nx-1)
            endif
         enddo
c ---    Radials
c ---    Subtract ang(1) from the boundaries so that sector 1 is
c ---    centered on 0/360 -- data points will also be shifted
         do i=1,ny-1
            cbang(i)=0.5*(ang(i)+ang(i+1))-ang(1)
         enddo
         cbang(ny)=0.5*(ang(ny)+ang(1)+360.)-ang(1)
      endif

c --- Establish the domain covered by the grid;  for polar grid, the
c --- domain is the square that contains the outer cell boundary ring

      if(LPOLR) then
c ---    POLAR grid
c ---    Set the lower left (SW) corner of domain in km
         xllk=xorgk-cbdisk(nx)
         yllk=yorgk-cbdisk(nx)

c ---    Set the upper right (NE) corner of domain in km
         xurk=xorgk+cbdisk(nx)
         yurk=yorgk+cbdisk(nx)

c ---    Convert xorgk,yorgk, and cbdisk to METERS for later use
         xorgm=xorgk*1000.0
         yorgm=yorgk*1000.0
         do i=1,nx
            cbdism(i)=1000.0*cbdisk(i)
         enddo
c-emi **** Modification 10/29/97  Convert corners to METERS
         xllm=xllk*1000.0
         yllm=yllk*1000.0
         xurm=xurk*1000.0
         yurm=yurk*1000.0

      else
c ---    CARTESIAN grid
c ---    Set the lower left (SW) corner of cell (1,1) in km
         if(LCENT) then
            xllk=xorgk-.5*sizek
            yllk=yorgk-.5*sizek
         else
            xllk=xorgk
            yllk=yorgk
         endif

c ---    Calculate upper right corner of output grid in km
         yurk=yllk+FLOAT(ny)*sizek
         xurk=xllk+FLOAT(nx)*sizek

c ---    Convert xllk,yllk,xurk,yurk,sizek to METERS.
         xllm=xllk*1000.0
         yllm=yllk*1000.0
         xurm=xurk*1000.0
         yurm=yurk*1000.0
         sizem=sizek*1000.0

c ---    Establish unit scale-factor for cell in output grid
         scale = 1./sizem

      endif

c --- Set the total number of cells
      nxy = nx*ny

c --- Compute the span of the output grid                ---  call SPAN
      call SPAN

c --- Get the coastline data                     
      if(LCOAST) then
        if(LBLNREAD) then
c         Read in the already-extracted .BLN file      --- call COASTRD
          write(*,*) 'Reading in pre-processed coastal data'
          call COASTRD                                
        else
c         Extract the data                            --- call COASTEXT
          call COASTEXT                               
        endif
      endif

c --- Determine how many data files are required &   ---  call FINDFILE
c --- report to user in LIST file
      call FINDFILE(ntdf)

c --- Retrieve discrete location information             ---  call XYIN
      if(LXY) call XYIN

c --- Retrieve partially completed arrays, if any      ---  call CELLIN
      call CELLIN

c --- Set header of TERREL.DAT                        ---  call WRTHEAD
      call WRTHEAD

      return
      end

c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 060202            READCF
c               J. Scire, D. Strimaitis   Earth Tech
c
c --- PURPOSE:  Read the control file containing the file names of
c               the input and output files of the run, and associated
c               control variables, place into program variables,
c               and QA information.
c
c --- UPDATES:
c     Ver Ver 3.67 Level 050824 to Ver Ver 3.681 Level 060202      DGS
c              - Change filename strings from c*70 to c*132
c              - This is required by CALUTILS 2.3 and later which uses
c              - c*132 in FILCASE and COMLINE
c --- Ver 3.62 Level 050128 to Ver 3.67 Level 050824  KAM
c              - Add variables LRAWECHO and RAWECHO to allow echoing
c                of raw data in grid units to an ASCII file
c --- Ver 3.55 Level 040305 to Ver 3.62 Level 050128  DGS
c              - Add call to COORDSVER and write info to list file
c              - Use only the first 2 land characterization types for
c                INOISEREP, ZNOISE, ITERREP, TERDEF arrays (currently
c                only distinguishing marine water and land)
c --- Ver 3.53 Level 040302 to Ver 3.55 Level 040305  KAM
c              - Add noisy data processing to Group (1)
c              - Change logical name for reading BLN fom LCOASTCON to
c                LBLNREAD
c --- Ver 3.51 Level 040228 to Ver 3.53 Level 040302  KAM
c              - Corrections to Group (1) dictionary
c              - Add option to read already-existing BLN coast file
c --- Ver 3.5 Level 031031 to Ver 3.51 Level 040228  K. Morrison
c              - Add variable for radius of cell interpolation
c              - Add coast processing with crude water/land use types
c                - New input file - GSHHSIN
c                - New output file - COASTBLN
c                - New datums in list for 2 data sources in GSHHS -
c                  DWVS and DWDBII
c              - Change TERDEF to be a 5-element vector, with a 
c                different default value by water/land use (ocean, land,
c                lake, island, pond)
c              - New array ITERREP(5) to control use of related TERDEF
c                value: 0 - no replacement, -1 - replace upon output only,
c                1 - replace voids during DB point processing, 2 - always
c                replace(only allowed for ocean and lakes)
c --- Ver 3.4 Level 031023 to Ver 3.5 Level 031031  K. Morrison
c              - Add interpolation option for empty cells
c --- Ver 3.4 Level 031017 to Level 031023  K. Morrison
c              - Add default value TERDEF for missing elevations at
c                end of processing
c --- Ver 3.312 Level 030905 to Ver 3.4 Level 031017  K. Morrison
c              - Add SRTM 1-sec and 3-sec processing
c              - Add interpolation option for discrete receptors
c --- Ver 3.3 Level 030402 to Ver 3.311 Level 030709  D. Strimaitis
c              - Remove restriction on using LCC projection with 7.5
c                minute DEM data and Canadiam 100m data
c --- Ver 3.3  Level 021018 to Level 030402    - D. Strimaitis
c              - Move character lat/lon variables into /GRID/
c              - Add false easting/northing
c              - Add type string to XTRACTLL calls
c --- Ver 3.2  Level 020627 to Level 021018    - D. Strimaitis
c              - Add DATUM (new subgroup 0c, new DATUM in Group 2)
c              - PMAP = UTM, TTM, LCC, PS, EM, or LAZA (imap removed)
c              - LSOHEM (L: T/F) replaced by UTMHEM (C*4: N/S)
c --- Ver 3.0  Level 020513 to Level 020627    - D. Strimaitis
c              - Fixed assignment of LPOLR based on IGRID value
c --- Ver 3.0  Level 010713 to Level 020513    - D. Strimaitis
c              - Made the 'DEM-2' (~90m ; 3 arc-sec) code applicable
c                to the standard DEM processing (retired DEM-2)
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
c           ntdf,lstfil,outfil,pltfil,prevfil,savefil,xyinp,xyout,
c           gshhsin,coastbln,rawecho,datafil(mxfil),justname(mxfil),
c           datatyp(mxfil),datatyp(mxfil),lcfiles
c ---    Common block /CELL/ variables:
c           ithresh,znoise(5),inoiserep(5),terdef(5),iterrep(5),cellradkm
c ---    Common block /CONTROL/ variables:
c           imodel,igrid,iproc,
c           lprev,lxy,lintxy,lvoidfil,lcoast,lblnread,lrawecho,
c           l30sec,l3sec,l7_5min,l100met,l3cd,lgtopo30,
c           lnzgnr,lgnr,lusgsglb,lsrtm1,lsrtm3,
c           lnuat,lcmet,lmeso,liscc,liscp,lgenr,
c           lutm,llcc,lps,lem,llaza,lttm,
c           lfeet,lpeak,lpolr,lcent,lpush,
c           pmap,dusgs90,dusgs30,darm3,d3cd,ddmdf,dgtopo30,
c           dusgsla,dnzgen,dgen,dsrtm1,dsrtm3,dwvs,dwdbii
c ---    Common block /GRID/ variables:
c           nx,ny,sizek,disk(mxnx),ang(mxny),xorgk,yorgk,feast,fnorth,
c           izone,rlat,rlon,xlat1,xlat2,icode(4),
c           utmhem,datum,clat0,clon0,clat1,clat2
c ---    Common block /XY/ variables:
c           nxycol,xyradkm
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE, XTRACTLL, COORDSVER
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.trl'
      include 'params.cal'
c
c --- Include common blocks
      include 'cell.trl'
      include 'control.trl'
      include 'grid.trl'
      include 'filnam.trl'
      include 'qa.trl'
      include 'xy.trl'
c
c --- Local variables
c --- Dimension CTEMP(132, MAX(names,ntypes)), DBTYPE(ntypes)
      character*4 ctemp(132,14)
      character*6 dbtype(11)
      character*132 dbfil
      character*50 verdoc
      character*4 clatlon(16,4)
      character*4 cpmap(8),cdatum(8)
      character*12 cvdic(mxvar,7)
      integer ivleng(mxvar,7),ivtype(mxvar,7)
      logical lecho, lerrcf

c --- Work arrays for land use type arrays
      real znoisew(5),terdefw(5)
      integer inoiserepw(5),iterrepw(5)

c --- Initialize local variables
      data lecho/.false./, lerrcf/.false./
      data names/10/, ntypes/13/
      data dbtype/'USGS90','USGSQD','ARM3  ',
     &            '3CD   ','DD1   ','GLOB30',
     &            'USGSGL','NZGNR ','GNR   ',
     &            'SRTM1 ','SRTM3 '/

c --- Set Dictionary

      data cvdic/
     a  'OUTFIL','LSTFIL','PLTFIL','PREVFIL','SAVEFIL',
     a  'XYINP','XYOUT','GSHHSIN','COASTBLN','LCFILES','NTDF',
     a  'LRAWECHO','RAWECHO', 47*' ',
     2  'USGS90','USGS30','ARM3','3CD','DMDF','GTOPO30','USGSLA',
     2  'NZGEN','GEN','SRTM1','SRTM3',  49*' ',
     3  'DUSGS90','DUSGS30','DARM3','D3CD','DDMDF','DGTOPO30',
     3  'DUSGSLA','DNZGEN','DGEN','DSRTM1','DSRTM3','DWVS','DWDBII',
     3   47*' ',
     b  'LPREV','LVOIDFIL','LXY','LINTXY','NXYCOL','XYRADKM','IMODEL',  
     b  'ITHRES','ZNOISE','INOISEREP','TERDEF','ITERREP','LCOAST',
     b  'LBLNREAD','CELLRADKM',45* ' ',
     c  'PMAP','IUTMZN','UTMHEM','RLAT0','RLON0','RLAT1','RLAT2',
     c  'DATUM','IGRID','XREFKM','YREFKM','NX','NY','DGRIDKM',
     c  'NRING','NRAYS','IPROC','FEAST','FNORTH',  41*' ',
     d  'DISKM',  59*' ',
     2  'ANGDEG',  59*' '/

      data ivleng/
     a  9*132, 3*1, 132, 47*0,
     2  11*132, 49*0,
     3  13*132, 47*0,
     b  8*1, 4*5, 3*1, 45*0,
     c  8,2*1,4*16,8,11*1, 41*0,
     d  1, 59*0,
     2  1, 59*0/

c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
      data ivtype/
     a  9*4,3,2,3,4, 47*0,
     2  11*4, 49*0,
     3  13*4, 47*0,
     b  3,3,3,3,2,1,2,2,1,2,1,2,3,3,1, 45*0,
     c  4,2,6*4,2,1,1,2,2,1,3*2,2*1, 41*0,
     d  1, 59*0,
     2  1, 59*0/

c --- Pass initial land type array values into work arrays
      do k=1,5
         znoisew(k)=znoise(k)
         terdefw(k)=terdef(k)
         inoiserepw(k)=inoiserep(k)
         iterrepw(k)=iterrep(k)
      enddo

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
       call READIN(cvdic(1,1),ivleng(1,1),ivtype(1,1),iocnt,iomesg,
     & lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),ctemp(1,5),
     2 ctemp(1,6),ctemp(1,7),ctemp(1,8),ctemp(1,9),lcfiles,ntdf,
     3 lrawecho,ctemp(1,10),idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum)

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')outfil=' '
      if(ctemp(1,2)(1:1).ne.' ')lstfil=' '
      if(ctemp(1,3)(1:1).ne.' ')pltfil=' '
      if(ctemp(1,4)(1:1).ne.' ')prevfil=' '
      if(ctemp(1,5)(1:1).ne.' ')savefil=' '
      if(ctemp(1,6)(1:1).ne.' ')xyinp=' '
      if(ctemp(1,7)(1:1).ne.' ')xyout=' '
      if(ctemp(1,8)(1:1).ne.' ')gshhsin=' '
      if(ctemp(1,9)(1:1).ne.' ')coastbln=' '
      if(ctemp(1,10)(1:1).ne.' ')rawecho=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')outfil(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')lstfil(j:j)=ctemp(j,2)(1:1)
         if(ctemp(j,3)(1:1).ne.' ')pltfil(j:j)=ctemp(j,3)(1:1)
         if(ctemp(j,4)(1:1).ne.' ')prevfil(j:j)=ctemp(j,4)(1:1)
         if(ctemp(j,5)(1:1).ne.' ')savefil(j:j)=ctemp(j,5)(1:1)
         if(ctemp(j,6)(1:1).ne.' ')xyinp(j:j)=ctemp(j,6)(1:1)
         if(ctemp(j,7)(1:1).ne.' ')xyout(j:j)=ctemp(j,7)(1:1)
         if(ctemp(j,8)(1:1).ne.' ')gshhsin(j:j)=ctemp(j,8)(1:1)
         if(ctemp(j,9)(1:1).ne.' ')coastbln(j:j)=ctemp(j,9)(1:1)
         if(ctemp(j,10)(1:1).ne.' ')rawecho(j:j)=ctemp(j,10)(1:1)
      enddo

c --- Convert the file names to the proper case
      call FILCASE(lcfiles,outfil)
      call FILCASE(lcfiles,lstfil)
      call FILCASE(lcfiles,pltfil)
      call FILCASE(lcfiles,prevfil)
      call FILCASE(lcfiles,savefil)
      call FILCASE(lcfiles,xyinp)
      call FILCASE(lcfiles,xyout)
      call FILCASE(lcfiles,gshhsin)
      call FILCASE(lcfiles,coastbln)
      call FILCASE(lcfiles,rawecho)

c --- Open listfile
      open(iolst,file=lstfil,status='unknown')

c --- Write banner to list file
      write(iolst,5) ver,level
5     format(///,26x,'TERREL OUTPUT SUMMARY',/,17x,'VERSION:  ',A12,
     1       ' LEVEL:  ',A8///)

c --- Obtain COORDS version information
      call COORDSVER(io6,verdoc)
      write(iolst,*)'Internal Coordinate Transformations by ',verdoc
      write(iolst,*)
      write(iolst,*)
      write(iolst,*)

c ------------------
c --- Input Group 0b
c ------------------

      do k=1,ntdf
c ---    Initialize the temporary arrays for the file names
         do i=1,ntypes
            do j=1,132
               ctemp(j,i)(1:1)=' '
            enddo
         enddo
         do j=1,132
            dbfil(j:j)=' '
         enddo

c ---    Read one DB filename entry (one of 11 DB types)
      call READIN(cvdic(1,2),ivleng(1,2),ivtype(1,2),iocnt,iolst,lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),ctemp(1,5),
     2 ctemp(1,6),ctemp(1,7),ctemp(1,8),ctemp(1,9),ctemp(1,10),
     3 ctemp(1,11),
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     8 idum)

c ---    Process if filename array has room for this entry
         if(k.LE.mxfil) then

c ---       Identify type of DB file by first character
            ii=0
            j=1
            if(ctemp(j,1)(1:1).ne.' ') then
               ii=1
               l3sec=.TRUE.
            elseif(ctemp(j,2)(1:1).ne.' ') then
               ii=2
               l7_5min=.TRUE.
            elseif(ctemp(j,3)(1:1).ne.' ') then
               ii=3
               l30sec=.TRUE.
            elseif(ctemp(j,4)(1:1).ne.' ') then
               ii=4
               l3cd=.TRUE.
            elseif(ctemp(j,5)(1:1).ne.' ') then
               ii=5
               l100met=.TRUE.
            elseif(ctemp(j,6)(1:1).ne.' ') then
               ii=6
               lgtopo30=.TRUE.
            elseif(ctemp(j,7)(1:1).ne.' ') then
               ii=7
               lusgsglb=.TRUE.
            elseif(ctemp(j,8)(1:1).ne.' ') then
               ii=8
               lnzgnr=.TRUE.
            elseif(ctemp(j,9)(1:1).ne.' ') then
               ii=9
               lgnr=.TRUE.
            elseif(ctemp(j,10)(1:1).ne.' ') then
               ii=10
               lsrtm1=.TRUE.
            elseif(ctemp(j,11)(1:1).ne.' ') then
               ii=11
               lsrtm3=.TRUE.
            elseif(ii.EQ.0) then
               stop 'READCF: DB file type index = 0'
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

cc ---       USGS90 files may have alternate format - read ICODE(4)
cc ---       According to the USGS manual, icode(1) must be equal to 3
cc ---       for USGS 1-deg DEM data sheets, however, this is not true:
cc ---       many 1-deg USGS DEM files have icode(1) .ne.3
cc ---                                          Shuming Du 12/07/2000.
c            if(ii .EQ. 1) then
c               open(ioinp,file=datafil(k),status='old')
c               read(ioinp,'(12(12x),4i6)') icode
c               close(ioinp)
c               if(icode(1) .EQ. 2) then
c                  datatyp(k) = 'USGSD2'
c                  l3sec2=.TRUE.
c               endif
c            endif

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
      do i=1,ntypes
         do j=1,132
            ctemp(j,i)(1:1)=' '
         enddo
      enddo

c --- Read Datum-Region for each DB type
      call READIN(cvdic(1,3),ivleng(1,3),ivtype(1,3),iocnt,iolst,lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),ctemp(1,5),
     2 ctemp(1,6),ctemp(1,7),ctemp(1,8),ctemp(1,9),ctemp(1,10),
     3 ctemp(1,11),ctemp(1,12),ctemp(1,13),
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)

c --- Transfer the char*4 data into the char*8 variables
      if(ctemp(1,1)(1:1).ne.' ') then
         do j=1,8
            dusgs90(j:j)=ctemp(j,1)(1:1)
         enddo
      endif
      if(ctemp(1,2)(1:1).ne.' ') then
         do j=1,8
            dusgs30(j:j)=ctemp(j,2)(1:1)
         enddo
      endif
      if(ctemp(1,3)(1:1).ne.' ') then
         do j=1,8
            darm3(j:j)=ctemp(j,3)(1:1)
         enddo
      endif
      if(ctemp(1,4)(1:1).ne.' ') then
         do j=1,8
            d3cd(j:j)=ctemp(j,4)(1:1)
         enddo
      endif
      if(ctemp(1,5)(1:1).ne.' ') then
         do j=1,8
            ddmdf(j:j)=ctemp(j,5)(1:1)
         enddo
      endif
      if(ctemp(1,6)(1:1).ne.' ') then
         do j=1,8
            dgtopo30(j:j)=ctemp(j,6)(1:1)
         enddo
      endif
      if(ctemp(1,7)(1:1).ne.' ') then
         do j=1,8
            dusgsla(j:j)=ctemp(j,7)(1:1)
         enddo
      endif
      if(ctemp(1,8)(1:1).ne.' ') then
         do j=1,8
            dnzgen(j:j)=ctemp(j,8)(1:1)
         enddo
      endif
      if(ctemp(1,9)(1:1).ne.' ') then
         do j=1,8
            dgen(j:j)=ctemp(j,9)(1:1)
         enddo
      endif
      if(ctemp(1,10)(1:1).ne.' ') then
         do j=1,8
            dsrtm1(j:j)=ctemp(j,10)(1:1)
         enddo
      endif
      if(ctemp(1,11)(1:1).ne.' ') then
         do j=1,8
            dsrtm3(j:j)=ctemp(j,11)(1:1)
         enddo
      endif
      if(ctemp(1,12)(1:1).ne.' ') then
         do j=1,8
            dwvs(j:j)=ctemp(j,12)(1:1)
         enddo
      endif
      if(ctemp(1,13)(1:1).ne.' ') then
         do j=1,8
            dwdbii(j:j)=ctemp(j,13)(1:1)
         enddo
      endif

c -----------------
c --- Input Group 1
c -----------------

      call READIN(cvdic(1,4),ivleng(1,4),ivtype(1,4),iocnt,iolst,lecho,
     1 LPREV,LVOIDFIL,LXY,LINTXY,NXYCOL,XYRADKM,IMODEL,ITHRES,ZNOISEW,
     2 INOISEREPW,TERDEFW,ITERREPW,LCOAST,LBLNREAD,CELLRADKM,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum)

c --- Use first 2 land type array values from work arrays
      do k=1,2
         znoise(k)=znoisew(k)
         terdef(k)=terdefw(k)
         inoiserep(k)=inoiserepw(k)
         iterrep(k)=iterrepw(k)
      enddo

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

      call READIN(cvdic(1,5),ivleng(1,5),ivtype(1,5),iocnt,iolst,lecho,
     1 CPMAP,IUTMZN,UTMHEM,
     2 CLATLON(1,1),CLATLON(1,2),CLATLON(1,3),CLATLON(1,4),
     3 CDATUM,IGRID,XREFKM,YREFKM,NX,NY,DGRIDKM,NRING,NRAYS,IPROC,
     4 FEASTIN,FNORTHIN,
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

      do k=1,nring
         kk=MIN(k,mxnx)
         call READIN(cvdic(1,6),ivleng(1,6),ivtype(1,6),iocnt,iolst,
     & lecho,
     1 DISK(kk),
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
      enddo

      do k=1,nrays
         kk=MIN(k,mxny)
         call READIN(cvdic(1,7),ivleng(1,7),ivtype(1,7),iocnt,iolst,
     & lecho,
     1 ANG(kk),
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
      enddo

c -------------------------------------------------
c --- Translate selected inputs to TERREL variables
c -------------------------------------------------

c --- Translate character lat/lon to real NLat/ELon
      if(clat0(1:1).NE.' ') call XTRACTLL(iolst,'LAT ',clat0,rlat)
      if(clon0(1:1).NE.' ') call XTRACTLL(iolst,'LON ',clon0,rlon)
      if(clat1(1:1).NE.' ') call XTRACTLL(iolst,'LAT ',clat1,xlat1)
      if(clat2(1:1).NE.' ') call XTRACTLL(iolst,'LAT ',clat2,xlat2)

c --- Grid variables
      xorgk=xrefkm
      yorgk=yrefkm
      sizek=dgridkm
      izone=iutmzn

c --- Set logicals for IMODEL choice
      if(imodel.EQ.1) then
         lcmet=.TRUE.
      elseif(imodel.EQ.2) then
         lmeso=.TRUE.
      elseif(imodel.EQ.3) then
         liscp=.TRUE.
         lpeak=.TRUE.
         lpolr=.TRUE.
c ---    Enforce POLAR grid type
         if(igrid.NE.3) then
            igrid=3
            write(iolst,*)
            write(iolst,*) 'IGRID changed to 3 (POLAR) for ISC-polar'
         endif
c ---    Set method for identifying distance ranges
         if(iproc.EQ.2) then
            lpush=.TRUE.
         endif
      elseif(imodel.EQ.4) then
         liscc=.TRUE.
         lpeak=.TRUE.
      elseif(imodel.EQ.5) then
         lnuat=.TRUE.
      elseif(imodel.EQ.6) then
         lgenr=.TRUE.
      endif

c --- Assign NX,NY if polar option is selected
      if(igrid.EQ.3) then
         nx=nring
         ny=nrays
      endif

c --- Set logicals for map projection PMAP
      if(pmap.EQ.'UTM     ')  lutm =.TRUE.
      if(pmap.EQ.'LCC     ')  llcc =.TRUE.
      if(pmap.EQ.'PS      ')   lps  =.TRUE.
      if(pmap.EQ.'EM      ')   lem  =.TRUE.
      if(pmap.EQ.'LAZA    ') llaza=.TRUE.
      if(pmap.EQ.'TTM     ')  lttm =.TRUE.

c --- Set logical for cell-centered reference point
      if(igrid.NE.1) lcent=.TRUE.

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

c --- Test for valid NTDF
      if(ntdf.GT.mxfil) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 0a'
         write(iolst,*) 'NTDF exceeds the parameter MXFIL '
         write(iolst,*) 'NTDF, MXFIL = ',ntdf,mxfil
         write(iolst,*) 'Increase MXFIL in PARAMS.TRL and recompile'
         write(iolst,*) 'or reduce the number of terrain DB files'
         lerrcf=.TRUE.
      endif

c --- Test for valid NXYCOL
      if((nxycol.NE.2 .AND. nxycol.NE.4) .AND. LXY) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'NXYCOL is not valid      = ',nxycol
         write(iolst,*) 'NXYCOL should be 2 or 4'
         lerrcf=.TRUE.
      endif

c --- Test for valid XYRADKM
      if(xyradkm.LE.0.0 .AND. LXY .AND. .NOT.LINTXY) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'XYRADKM is not valid     = ',xyradkm
         write(iolst,*) 'XYRADKM should be positive'
         lerrcf=.TRUE.
      endif

c --- Test for valid CELLRADKM
      if(cellradkm.LE.DGRIDKM .AND. LVOIDFIL) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'CELLRADKM is not valid     = ',
     &                   cellradkm
         write(iolst,*) 'CELLRADKM should be at least',
     &                  ' as large as DGRIDKM' 
         lerrcf=.TRUE.
      endif

c --- Test for valid IMODEL
      if(imodel.LT.1 .OR. imodel.GT.6) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'IMODEL out of range      = ',imodel
         write(iolst,*) 'IMODEL should be 1 to 6'
         lerrcf=.TRUE.
      endif

c --- Test for valid INOISEREP
      if(MINVAL(inoiserep).LT.0 .OR. MAXVAL(inoiserep).GT.3 .OR.
     &   MAX(ABS(inoiserep(2)),ABS(inoiserep(4)),ABS(inoiserep(5)))
     &   .GT.2) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'INOISEREP out of range      = ',inoiserep
         write(iolst,*) 'INOISEREP should be between 0,0,0,0,0 ',
     &                  ' and 3,3,3,3,3'
         lerrcf=.TRUE.
      endif

c --- Test for valid ITERREP
      if(MINVAL(iterrep).LT.0 .OR. MAXVAL(iterrep).GT.3 .OR.
     &   MAX(ABS(iterrep(2)),ABS(iterrep(4)),ABS(iterrep(5))).GT.2) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'ITERREP out of range      = ',iterrep
         write(iolst,*) 'ITERREP should be between 0,0,0,0,0 ',
     &                  ' and 3,2,3,2,2'
         lerrcf=.TRUE.
      endif

c --- Test for valid ITHRES
      if(ithres.LT.0 .OR. ithres.GT.100) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 1'
         write(iolst,*) 'ITHRES out of range      = ',ithres
         write(iolst,*) 'ITHRES should be 0 to 100'
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

c --- Test for valid IGRID
      if(igrid.LT.1 .OR. igrid.GT.3) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'IGRID out of range      = ',igrid
         write(iolst,*) 'IGRID should be 1,2, or 3'
         lerrcf=.TRUE.
      endif

c --- Test for 'CORNER' when 'CALMET' is selected
      if(LCMET) then
         if(LCENT) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 2'
            write(iolst,*) 'CALMET requires origin specified at '
            write(iolst,*) '       SW corner of grid cell (1,1)'
            lerrcf=.TRUE.
         endif
      endif

c --- Allow these combinations now
cc --- Reject combination of 7.5 minute data and LCC map projection
c      if(LLCC .AND. L7_5MIN) then
c         write(iolst,*)
c         write(iolst,*) 'READCF:  Error in Input Group 2'
c         write(iolst,*) 'Use of LCC map projection with UTM-based'
c         write(iolst,*) '7.5 minute terrain data is NOT implemented'
c         lerrcf=.TRUE.
c      endif
c
cc --- Reject combination of 100m Canadian data and LCC map projection -SD
c      if(LLCC .AND. L100MET) then
c         write(iolst,*)
c         write(iolst,*) 'READCF:  Error in Input Group 2'
c         write(iolst,*) 'Use of LCC map projection with 100-m'
c         write(iolst,*) 'Canadian DMDF terrain data is NOT implemented'
c         lerrcf=.TRUE.
c      endif

c --- Reject combination POLAR grid with models other than ISC
      if(LPOLR) then
         if(.not.LISCP .AND. .not.LISCC) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 2'
            write(iolst,*) 'Use of POLAR grid with models other than'
            write(iolst,*) 'ISC is NOT implemented'
            lerrcf=.TRUE.
         endif
      endif

c --- Test for lat/lon of origin for LCC/PS/EM/LAZA/TTM map projection
      if(LLCC .or. LPS .or. LEM .or. LLAZA .or. LTTM) then
         if(rlat .LT. -900.0 .AND. rlon .LT. -900.0) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 2'
            write(iolst,*) 'Missing lat/lon of origin for ',pmap
            lerrcf=.TRUE.
         endif
      endif

c --- Test for matching latitudes for LCC map projection
      if(LLCC) then
         if(xlat1 .LT. -900.0 .OR. xlat2 .LT. -900.0) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 2'
            write(iolst,*) 'Missing matching lats for ',pmap
            lerrcf=.TRUE.
         endif
      endif

c --- Test for matching latitudes for PS/EM map projection
      if(LPS .or. LEM) then
         if(xlat1 .LT. -900.0) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 2'
            write(iolst,*) 'Missing matching lats for ',pmap
            lerrcf=.TRUE.
         endif
      endif

c --- Test for valid NX,NY
      if(nx.GT.mxnx .AND. (igrid.EQ.1 .OR. igrid.EQ.2)) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'NX exceeds the parameter MXNX '
         write(iolst,*) 'NX, MXNX = ',nx,mxnx
         write(iolst,*) 'Increase MXNX in PARAMS.TRL and recompile'
         write(iolst,*) 'or reduce the number of X-grid cells'
         lerrcf=.TRUE.
      endif
      if(ny.GT.mxny .AND. (igrid.EQ.1 .OR. igrid.EQ.2)) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'NY exceeds the parameter MXNY '
         write(iolst,*) 'NY, MXNY = ',ny,mxny
         write(iolst,*) 'Increase MXNY in PARAMS.TRL and recompile'
         write(iolst,*) 'or reduce the number of Y-grid cells'
         lerrcf=.TRUE.
      endif

c --- Test for valid DGRIDKM
      if(dgridkm.LE.0.0 .AND. (igrid.EQ.1 .OR. igrid.EQ.2)) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'DGRIDKM must be positive '
         write(iolst,*) 'DGRIDKM = ',dgridkm
         lerrcf=.TRUE.
      endif

c --- Test for valid IPROC
      if(igrid.EQ.3) then
         if(iproc.LT.1 .OR. iproc.GT.2) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 2'
            write(iolst,*) 'IPROC out of range      = ',iproc
            write(iolst,*) 'IPROC should be 1 or 2'
            lerrcf=.TRUE.
         endif
      endif

c --- Test for valid NRING
      if(nring.GT.mxnx .AND. igrid.EQ.3) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'NRING exceeds the parameter MXNX '
         write(iolst,*) 'NRING, MXNX = ',nring,mxnx
         write(iolst,*) 'Increase MXNX in PARAMS.TRL and recompile'
         write(iolst,*) 'or reduce the number of rings'
         lerrcf=.TRUE.
      endif

c --- Test for valid NRAYS
      if(nrays.GT.mxny .AND. igrid.EQ.3) then
         write(iolst,*)
         write(iolst,*) 'READCF:  Error in Input Group 2'
         write(iolst,*) 'NRAYS exceeds the parameter MXNY '
         write(iolst,*) 'NRAYS, MXNY = ',nrays,mxny
         write(iolst,*) 'Increase MXNY in PARAMS.TRL and recompile'
         write(iolst,*) 'or reduce the number of rays'
         lerrcf=.TRUE.
      endif

c --- Make sure that distances and radials are given in right order
c --- for polar grid option
      if(igrid.EQ.3) then
         if(ang(1).LT.1.) then
            write(iolst,*)
            write(iolst,*) 'READCF:  Error in Input Group 3'
            write(iolst,*) 'First angle must be GE 1; Found ',ang(1)
            lerrcf=.TRUE.
         endif
         do i=2,nx
            if(disk(i).LE.disk(i-1)) then
               write(iolst,*)
               write(iolst,*) 'READCF:  Error in Input Group 3'
               write(iolst,*) 'Ring not in order :',i
               lerrcf=.TRUE.
            endif
         enddo
         do i=2,ny
            if(ang(i).LE.ang(i-1)) then
               write(iolst,*)
               write(iolst,*) 'READCF:  Error in Input Group 3'
               write(iolst,*) 'Angle not in order :',i
               lerrcf=.TRUE.
            endif
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

c-----------------------------------------------------------------------
      subroutine span
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050817              SPAN
c               D. Strimaitis, E. Insley SRC
c
c ADAPTED from program ELEV in the ARM3 model
c               G. Lundberg, SAI
c         with enhancements by
c               E. Chang, G. Moore, SRC
c
c
c PURPOSE:     SPAN computes the minimum and maximum coordinates of
c              the output grid in the units of the terrain data file.
c              Use the DATUM of the output grid for this conversion.
c
c --- Update
c     Ver 3.65 Level 050815 to Ver 3.66 Level 050817 KAM
c              Modify warning message for crossing dateline
c     Ver 3.63 Level 040131 to Ver 3.65 Level 050815 KAM
c              Re-use LBRANCH and add to GRID common so that dateline
c              crossing can be corrected for elsewhere
c     Ver 3.3  Level 030402 to Ver 3.5 Level 040131 KAM
c              Correct treatment of eastern hemisphere
c     Ver 3.3  Level 021018 to 030402       DGS
c              Add false easting/northing to GLOBE1 calls
c     Ver 3.2  Level 000411 to 021018       DGS
c              Use full COORDS conversion (GLOBE1, GLOBE)
c     Ver 2.1  Level 990528 to 000411       DGS
c              Revise logic to treat branch at 180 deg.
c
c
c ARGUMENTS:
c    PASSED:  /CONTROL/ program control logicals
c             /GRID/    output grid specification
c
c  RETURNED:  /GSPAN/   range in grid coordinates
c
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:   GLOBE1, GLOBE
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'grid.trl'
      include 'gspan.trl'

c --- For coordinate transformations
      character*12 caction
      character*4 c4dum
      real*8 vecti(9),vecto(9)

c --- Set Scale Factor of Tangential TM projection (1.0)
      tmsone=1.00000

c --- Set input dataset false Easting/Northing to zero
      feasti=0.0
      fnorti=0.0

      write(iolst,*)
      write(iolst,*) '--------------------------'
      write(iolst,*) '      SPAN Results '
      write(iolst,*) '--------------------------'
      write(iolst,*)

c ---    Get the LON/LAT span that covers the grid
c ---    Note:   The one degree U.S. sheets are defined from
c ---            the SE corner and WEST Longitude has the
c ---            opposite orientation of UTMX
c ---    !! x & y are WEST Longitude & Latitude here !!

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

c --- Set translation vectors from the output grid to LL
      if(lutm) then
c ---    Using UTM grid
         call GLOBE1('UTM     ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(llcc) then
c ---    Using Lambert Conformal grid
         call GLOBE1('LCC     ',idum,xdum,xlat1,xlat2,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(lps) then
c ---    Using Polar Stereographic grid
         call GLOBE1('PS      ',idum,xdum,xlat1,xdum,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(lem) then
c ---    Using Equatorial Mercator grid
         call GLOBE1('EM      ',idum,xdum,xlat1,-xlat1,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(llaza) then
c ---    Using Lambert Azimuthal Equal Area grid
         call GLOBE1('LAZA    ',idum,xdum,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(lttm) then
c ---    Using Tangential TM grid
         call GLOBE1('TM      ',idum,tmsone,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      endif

c --- Map corners of grid to LAT,LON(east)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xllk,yllk,xlonsw,ylatsw,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xllk,yurk,xlonnw,ylatnw,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xurk,yurk,xlonne,ylatne,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xurk,yllk,xlonse,ylatse,idum,c4dum)
c --- Also map middle of north edge
      xmidk=0.5*(xllk+xurk)
      call GLOBE(iolst,caction,datum,vecti,datum,vecto,
     &           xmidk,yurk,xlonnm,ylatnm,idum,c4dum)
c --- Convert to West Longitude
      xlonsw=-xlonsw
      xlonnw=-xlonnw
      xlonne=-xlonne
      xlonse=-xlonse
      xlonnm=-xlonnm

c --- Get the span (m) that covers the grid for use with 30 meter data
c --- !! x & y are Easting & Northing m here !!
c --- !! convert to nearest whole meter here !!
      min30x = INT(xllm)
      min30y = INT(yllm)
      max30x = INT(xurm)+1
      max30y = INT(yurm)+1

c ---    Remove branch at 180 degrees if it is within grid
         lbranch=.FALSE.
         difn=xlonnw-xlonne
         difs=xlonsw-xlonse
         dife=ABS(xlonne-xlonse)
         difw=ABS(xlonnw-xlonsw)
         if(difn.LT.0. .OR. difs.LT.0.) then
c ---       Longitude must increase from E to W along N and S borders
            lbranch=.TRUE.
         elseif(dife.GT.90. .OR. difw.GT.90.) then
c ---       Longitude change along E and W borders must be 'reasonable'
            lbranch=.TRUE.
         endif
         if(LBRANCH) then
            if(xlonse.LT.0.) xlonse=xlonse+360.
            if(xlonsw.LT.0.) xlonsw=xlonsw+360.
            if(xlonne.LT.0.) xlonne=xlonne+360.
            if(xlonnw.LT.0.) xlonnw=xlonnw+360.
                lbranch=.false.
         endif

c ---    Print Warning message if necessary
           if (max(xlonsw,xlonnw).ge.180.and.min(xlonse,xlonne).le.
     &      180.) then
            lbranch=.TRUE.
            write (*,*) 'Current output grid might cross International',
     &                  ' Date Line.  There may be'
            write (*,*) 'ARM3, 3CD, DMDF, Generic, and NZ Generic',
     &                  ' data set problems.'
            write (*,*) 'Please review the input file and results',
     &                  ' carefully.'
         end if

c ---    Report to list-file
         write(iolst,*) 'NLat,WLon  [SE] = ',ylatse,xlonse
         write(iolst,*) 'NLat,WLon  [SW] = ',ylatsw,xlonsw
         write(iolst,*) 'NLat,WLon  [NW] = ',ylatnw,xlonnw
         write(iolst,*) 'NLat,WLon  [NE] = ',ylatne,xlonne
         write(iolst,*) 'Output Grid Coordinates (km) '
         if(LUTM) write(iolst,*) ' -- UTM Zone = ',izone
         write(iolst,*) ' [SE] = ',xurk,yllk
         write(iolst,*) ' [SW] = ',xllk,yllk
         write(iolst,*) ' [NW] = ',xllk,yurk
         write(iolst,*) ' [NE] = ',xurk,yurk

c --- Set Max and Min values     
      minx = FLOOR(AMIN1(xlonne,xlonse))
      maxx = FLOOR(AMAX1(xlonnw,xlonsw))
      miny = FLOOR(AMIN1(ylatse,ylatsw))
c --- Special treatment for MAXY:
c --- Adjust for possible dip in meridian lines across north edge
c --- by adding the absolute difference in latitude between the
c --- middle of the north edge and each corner, to the max value.
      del = ABS(ylatnm-ylatnw)+ABS(ylatnm-ylatne)
      rmaxy = AMAX1(ylatne,ylatnw)
      maxy = FLOOR(AMAX1(rmaxy,ylatnm)+del)

c --- Compute number of 7.5 min quads to eliminate from 1 degree DEMs
c --- There are 8 quads (7.5 min.) in the x-direction and y-direction
c --- of a 1 degree DEM.  Total = 64
c --- X-direction
      degfractmn = AMIN1(xlonne,xlonse) - minx
      degfractmx = AMIN1(xlonnw,xlonsw) - maxx
      nquadmnx = degfractmn/0.125
      nquadmxx = (1.0-degfractmx)/0.125
c --- Y-direction
      degfractmn = AMIN1(ylatse,ylatsw) - miny
      degfractmx = AMIN1(ylatne,ylatnw) - maxy
      nquadmny = degfractmn/0.125
      nquadmxy = (1.0-degfractmx)/0.125

c      write(iolst,1234) minx,miny,maxx,maxy
c1234  format('MINX=',i10,/,'MINY=',i10,/,'MAXX=',i10,/,'MAXY=',i10)

      return
      end
c-----------------------------------------------------------------------
      subroutine findfile(ntdf)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 040311          FINDFILE
c               E. Insley, M. Spitzak, D. Strimaitis, SRC
c
c
c PURPOSE:     FINDFILE reports to the LIST file the range of the domain
c              in terms of latitude and longitude and tells the user how
c              many data files they need to cover the domain.  This
c              information is provided for USGS 3-sec data, USGS 7.5 minute
c              data, and for the '3CD' data (Rocky Mtn Communications
c              distribution of 3-sec digitized data).
c              In addition, filenames that are based on the latitude
c              and longitude of the SE corner of each degree-square (e.g.
c              the 3-sec datasets), the SW corners (SRTM datasets), and the NW
c              corner (30-sec datasets) are generated for the list file.
c
c --- Update
c     Ver 3.5   Level 040131 to Ver 3.56 Level 040311       KAM
c               Re-instate termination if nothing is being done
c               Add cell common to test for no action
c     Ver 3.4   Level 031017 to Ver 3.5 Level 040131       KAM
c               Correct treatment of eastern hemisphere
c     Ver 3.313 Level 000112 to Ver 3.4 Level 031017       KAM
c               Add filenames for 1-sec and 3-sec SRTM data
c     Ver 2.1   Level 000112 to 000411       DGS
c               Replace 3CD filenames with new 1-degree DEM filenames
c               that explicitly reference N/S lat. and E/W long.
c               Revise logic to treat branch at 180 deg.
c     Ver 2.0   Level 990528 to 000112       DGS
c               Expand format used to create 3CD filenames
c
c
c ARGUMENTS:
c
c    PASSED:
c        NTDF   - integer     - Number of terrain DB files provided
c                               in control file
c             /CONTROL/ program control logicals
c             /GRID/    output grid specification
c             /GSPAN/   range in grid coordinates
c             /CELL/    switches for data repair/filling
c
c  RETURNED:  /DBINF/   3SEC terrain data base information
c
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'grid.trl'
      include 'gspan.trl'
      include 'cell.trl'

c --- Local variables
c     logical lwarn,lexist
      integer lonse(indmax),latse(indmax)
      character filenm1*2,filenm2*3,filenm3*4,globtmp*7
      character cn*1,cs*1,ce*1,cw*1

      data cn/'n'/, cs/'s'/, ce/'e'/, cw/'w'/
c     data lwarn/.FALSE./

      write(iolst,*)
      write(iolst,*) '--------------------------'
      write(iolst,*) '  Data File Information'
      write(iolst,*) '--------------------------'
      write(iolst,*)

c --- Assign DB values
c --- Note: x,y here refer to WEST Longitude, and NORTH Latitude.
c --- The DB "origin" is set at the SE corner of the SE-most
c --- degree-square needed to cover the output grid
      ndx = 0
      lonsedb = minx
      latsedb = miny

c --- Compute the number of 1-degree squares needed to cover grid
      nxdb=maxx-minx+1
      nydb=maxy-miny+1
      nxydb=nxdb*nydb
      nd=2*nxydb
      n30x = (nxdb*8)-nquadmnx-nquadmxx
      n30y = (nydb*8)-nquadmny-nquadmxy

c --- Report results to list-file
      write(iolst,*) 'SE sheet starts at LONSEDB = ',lonsedb
      write(iolst,*) '                   LATSEDB = ',latsedb
      write(iolst,*) 'Number of 1 Degree DEMs (NXDB) = ',nxdb
      write(iolst,*) '                        (NYDB) = ',nydb
      write(iolst,*) 'Number of 30 Meter DEMs (N30X) = ',n30x
      write(iolst,*) '                        (N30Y) = ',n30y

c --- Check against array dimensions
      if(nxydb .GT. indmax) then
        write(iolst,*) 'WARNING ------'
        write(iolst,*) 'Grid requires too many files, over: ',indmax
        write(iolst,*) 'Final grid needs to be built sequentially'
        goto 100
      endif

c --- Determine file names for 1-degree sheets.  Start numbering
c --- them at the SE corner, and proceed west and north.
      is=1
c --- Determine latitude and longitude for each 1 degree square
c --- Save min & max to specify range for output grid coverage
      do iy=1,nydb
         do ix=1,nxdb
            lonse(is)=minx+ix-1
            if(lonse(is).GE.180) lonse(is)=lonse(is)-360
            latse(is)=miny+iy-1
            is=is+1
         enddo
      enddo
c --- Write filenames for 1-degee data to list file
      write(iolst,*)
      write(iolst,*) 'The following 1-degree data files are needed:'

      filenm3='.DEM'
      do is=1,nxydb
c        lexist=.FALSE.
         write(filenm1,'(i2)') IABS(latse(is))
         if(IABS(lonse(is)) .LT. 100) then
            write(filenm2(2:3),'(i2)') IABS(lonse(is))
            filenm2(1:1)='0'
         else
            write(filenm2,'(i3)') IABS(lonse(is))
         endif
         if(latse(is).LT.0) then
            if(lonse(is).LT.0) then
               filenm(is)=filenm1//cs//filenm2//ce//filenm3
            else
               filenm(is)=filenm1//cs//filenm2//cw//filenm3
            endif
         else
            if(lonse(is).LT.0) then
               filenm(is)=filenm1//cn//filenm2//ce//filenm3
            else
               filenm(is)=filenm1//cn//filenm2//cw//filenm3
            endif
         endif

c ---    Write 3CD filename to LIST file
         write(iolst,*) filenm(is)

      enddo

c --- Write filenames for SRTM data to list file
      write(iolst,*)
      write(iolst,*) 'The following SRTM data files are needed:'

      filenm3='.HGT'
      do is=1,nxydb
c        lexist=.FALSE.
         write(filenm1,'(i2.2)') IABS(latse(is))

c ---    For SRTM, the name is the SW corner    
         write(filenm2,'(i3.3)') IABS(lonse(is)+1)
         if(latse(is).LT.0) then
            if((lonse(is)+1).LT.0) then
               srtmnm(is)=cs//filenm1//ce//filenm2//filenm3
            else
               srtmnm(is)=cs//filenm1//cw//filenm2//filenm3
            endif
         else
            if((lonse(is)+1).LT.0) then
               srtmnm(is)=cn//filenm1//ce//filenm2//filenm3
            else
               srtmnm(is)=cn//filenm1//cw//filenm2//filenm3
            endif
         endif

c ---    Write SRTM filename to LIST file
         write(iolst,*) srtmnm(is)

      enddo

100   continue
c --- Determine file names for the GTOPO30 globat data set.  The following naming
c     convention is used for the data files.
c
c     The following scheme assumes that the output grid will not expand
c     beyond non-adjacent terrain files.  In other words, the output grid
c     must be smaller than ~ 40 degrees.
c
      is=1
c
c --- The SE corner of the output grid
c
      call findglob (xlonse,ylatse,globnm(1))
c
c --- The NE corner of the output grid
c
      call findglob (xlonne,ylatne,globtmp)
      do i=1,is
        if (globtmp.eq.globnm(i)) goto 10
      end do
      is=is+1
      globnm(is)=globtmp
10    continue
c
c --- The NW corner of the output grid
c
      call findglob (xlonnw,ylatnw,globtmp)
      do i=1,is
        if (globtmp.eq.globnm(i)) goto 20
      end do
      is=is+1
      globnm(is)=globtmp
20    continue
c
c --- The SW corner of the output grid
c
      call findglob (xlonsw,ylatsw,globtmp)
      do i=1,is
        if (globtmp.eq.globnm(i)) goto 30
      end do
      is=is+1
      globnm(is)=globtmp
30    continue
c
c --- Write filenames for GTOPO30 data to list file
c
      write(iolst,*)
      write(iolst,*) 'The following GTOPO30 data files are needed:'
c
      do i=1,is
        write (iolst,*) globnm(i)//'.DEM'
      end do

c --- Stop processing if no input data files have been specified by the user
c --- and no further processing (void filling of previous data) is to be
c --- done
      if(ntdf.EQ.0 .AND. .NOT.(lprev .AND. (lvoidfil .OR.
     &   (maxval(inoiserep).GT.0) .OR. (maxval(iterrep).GT.0)))) then
        write(iolst,*)
        write(iolst,*) '--------------------------'
        write(iolst,*) '    Processing Stopped   '
        write(iolst,*) '--------------------------'
        write(iolst,*) 'No Input Data Files Found'
        write(iolst,*) ' -- Enter number of input files and filenames '
        write(iolst,*) '    in TERREL.INP and run again.'
        write(*,*)' No Input Data Files Found -- See Run LIST file'
        stop
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine comp
c----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 031031              COMP
c               D. Strimaitis   Earth Tech
c
c --- PURPOSE:  Process one or more terrain DB files to produce a
c               terrain elevation file for the specified domain
c
c --- Updates
c --- Ver 3.4  Level 031017 to Ver 3.5  Level 031031       KAM
c              Split out setups to DB-specific subroutines
c               - Adds SETDMDF, SETGENER, SETSRTM
c               - Move most logical for DB types to associated
c                 setup routines
c              Split out reads to DB-specific subroutines
c               - Adds LOADDMDF, LOADNZGNR, LOADGNR, LOADOTHER, LOADSRTM
c               - Rename LOAD to LOADSTRP, now called from LOADOTHER
c              Move intermediate data processing for cells and
c              receptors to SAVEELEV, now called from the read subroutines
c              Add call for cell interpolation, subroutine CELLFILL
c --- Ver 3.4  Level 030402 to 031017       KAM
c              Add 1-sec and 3-sec SRTM data
c              Fix discrete processing for all DB types 
c --- Ver 3.3  Level 021018 to 030402       DGS
c              Add false easting/northing to GLOBE1 calls
c --- Ver 3.2  Level 020730 to 021018       DGS
c               Incorporate full COORDS implementation:
c                -  identify UTM zone for DMDF data points
c                -  full DATUM conversion
c                -  UTM, LCC, PS, EM, and LAZA map projections
c --- Ver 3.1  Level 020513 to Level 020730    - D. Strimaitis
c              - Place OPEN attributes for binary files in PARAMETERs
c                to allow code to be configured for various compilers
c --- Ver 3.0  Level 020318 to Level 020513    - D. Strimaitis
c              - Made the 'DEM-2' (~90m ; 3 arc-sec) code applicable
c                to the standard DEM processing (retired DEM-2)
c --- Ver 3.0  Level 010713 to Level 020318    - C. Czaja
c              - Revised the 'DEM-2' (~90m ; 3 arc-sec) steps
c                for strips and points
c
c --- INPUTS:
c
c --- OUTPUT:
c           none
c
c --- COMP called by:  MAIN
c --- COMP calls:    SETDMDF, SETARM3, SET3SEC, RDUSGSHD, SETGLOB,
c                    SETUSGSG, SETGENER, SETSRTM,
c                    LOADDMDF, LOADNZGNR, LOADGNR, LOADOTHER
c                    CELLFILL, CELLOUT, HEIGHTS, 
c                    FILLANG, HCOPY, MODOUT, PLTOUT,
c                    OUT, XYOUTPUT
c ------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'filnam.trl'
      include 'grid.trl'
      include 'gspan.trl'

c --- Local variables
      real ht(mxnxy)

c --- Initialize the count for the number of input data files not used
      notused = 0

c --- Report progress
      write(iomesg,*)'COMPUTATIONAL PHASE'
      write(iomesg,*)

c --- Loop over all terrain DB files specified by user
      do 500 k = 1,ntdf

        write(*,*)
        write(*,*) 'Processing data file: ',datafil(k)


c ---   Initialize vertical scaling factor for file
        zfac=1.0

c ---   Determine what format the data is and open file
c ---   Collect information on the terrain data base
c -----------------------------------------------------

        if(datatyp(k) .EQ.'ARM3'.OR. datatyp(k) .EQ. 'arm3') then
c ---     ARM3 file                                     ---  call SETARM3
          call SETARM3(k)

        elseif(datatyp(k) .EQ.'3CD'.OR. datatyp(k) .EQ. '3cd') then
c ---     3-sec file Rocky Mtn. Comm. CD-ROM data       ---  call SET3SEC
          call SET3SEC(k)

        elseif(datatyp(k) .EQ.'USGS90'.OR. datatyp(k) .EQ. 'usgs90')then
c ---     USGS 30-sec(90m)                             ---  call RDUSGSHD
          l3sec = .TRUE.
          l7_5min = .FALSE.
          call RDUSGSHD(k)

        elseif(datatyp(k) .EQ.'USGSQD'.OR. datatyp(k) .EQ. 'usgsqd')then
c ---     USGS 7.5 minute file                         ---  call RDUSGSHD
          l7_5min = .TRUE.
          l3sec = .FALSE.
          call RDUSGSHD(k)

        elseif(datatyp(k) .EQ.'DD1'.OR. datatyp(k) .EQ. 'dd1')then
c ---     CANADA DMDF (~100m) file                     ---  call SETGENER 
          l100met = .TRUE.
          lnzgnr = .FALSE.
          lgnr = .FALSE.
          call SETGENER(k)
          dbdatum=ddmdf

        elseif(datatyp(k) .EQ.'GLOB30' .OR. datatyp(k) .EQ. 'glob30')
     &  then
c ---     USGS 30-sec global terrain dataset (DEM)     ---  call SETGLOB
          call SETGLOB(k)

        elseif(datatyp(k) .EQ.'USGSGL' .OR. datatyp(k) .EQ. 'usgsgl')
     &  then
c ---     USGS Lambert Azimuthal Equal Area Projection global 
c ---     terrain image dataset                        ---  call SETUSGSG
          call SETUSGSG(k)
          dbdatum=dusgsla

        elseif(datatyp(k) .EQ.'NZGNR'.OR. datatyp(k) .EQ. 'nzgnr')then
c ---     New Zealand Generic data file                ---  call SETGENER      
          lnzgnr = .TRUE.
          lgnr = .FALSE.
          l100met = .FALSE.
          call SETGENER(k)
          dbdatum=dnzgen

        elseif(datatyp(k) .EQ.'GNR'.OR. datatyp(k) .EQ. 'gnr')then
c ---     Generic data file                            ---  call SETGENER
          lgnr = .TRUE.
          lnzgnr = .FALSE.
          l100met = .FALSE.
          call SETGENER(k)
          dbdatum=dgen

        elseif(datatyp(k) .EQ.'SRTM1' .OR. datatyp(k) .EQ. 'srtm1')
     &  then
c ---     SRTM 1-sec terrain dataset (DEM)             ---  call SETSRTM
          call SETSRTM(k,1)

        elseif(datatyp(k) .EQ.'SRTM3' .OR. datatyp(k) .EQ. 'srtm3')
     &  then
c ---     SRTM 3-sec terrain dataset (DEM)             ---  call SETSRTM
          call SETSRTM(k,3)

        endif
c ---   If data from this file is not on the grid - go on to next file
        if(lnotgrid) go to 500

c ---   Process DB File
c -------------------------

c --------------------------------------------------------------------
c ---   Canadian DMDF terrain file
c --------------------------------------------------------------------
        if(l100met) then
           call loaddmdf

c --------------------------------------------------------------------
c ---   New Zealand generic data file
c --------------------------------------------------------------------
        elseif(lnzgnr) then
           call loadnzgnr

c --------------------------------------------------------------------
c ---   Generic data file
c --------------------------------------------------------------------
        elseif(lgnr) then
           call loadgnr

c --------------------------------------------------------------------
c ---   USGS Lambert Azimuthal projection image data
c --------------------------------------------------------------------
        elseif(lusgsglb) then
           call loadlaza


c --------------------------------------------------------------------
c ---   Other files 
c --------------------------------------------------------------------
        else
           call loadother

        endif
        close(ioinp)
  500 continue
c --- End of loop over all terrain DB files specified by user

c --- Write summary of number of input data files used
      write(iolst,*)
      write(iolst,*) '*** TERREL RUN SUMMARY ***'
      write(iolst,*) 'Number of input data files = ',ntdf
      write(iolst,*) 'Number of input data files NOT used = ',notused

c --- Check if empty cells should be filled             ---  call CELLFILL
      
      if(LVOIDFIL) call cellfill

c --- Finished with this set of sheets                  ---  call CELLOUT
      call CELLOUT

c --- Process cell data for "model" input files         ---  call HEIGHTS
      call HEIGHTS(ht,zhi,zlo)

c --- Fill gaps that may exist in POLAR grid            ---  call FILLANG
      if(LPOLR) call FILLANG(ht)

c --- Write out terrain heights to hard copy            ---  call HCOPY
      call HCOPY(ht)

c --- Write out the model input files                   ---  call MODOUT
      call MODOUT(ht)

c --- Write out the plot-file                           ---  call PLTOUT
      if(.not.LPOLR) then
         call PLTOUT(ht,zhi,zlo)
      endif

c --- Write out the discrete location file              ---  call XYOUTPUT
      if(LXY) call XYOUTPUT

      return
      end
c-----------------------------------------------------------------------
      subroutine saveelev(x,y,dels,z)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050824          SAVEELEV
c               K. Morrison. Earth Tech
c
c
c PURPOSE:     SAVEELEV saves the information on the current point
c              to discrete receptors and grid cells by calls
c              to the appropriate subroutines, consolidating these
c              calls previously done in COMP.
c
c --- Update
c     Ver. 3.66 Level 050817 to Ver. 3.67 Level 050824 - KAM
c              - Add echoing output of raw input data, in X,Y grid units 
c     Ver. 3.55 Level 040305 to Ver. 3.66 Level 050817 - KAM
c              - Remove test for coastline processing, because type 
c                to land if processing not done, but noise and 
c                defaults can still apply
c     Ver. 3.54 Level 040303 to Ver. 3.55 Level 040305 - KAM
c              - Add noisy point processing
c              - Add point counters
c     Ver. 3.51 Level 040228 to Ver. 3.54 Level 040303 - KAM
c              - Add simple test to skip outside points
c     Ver 3.5  Level 031031 to Ver 3.51 Level 040228 - KAM
c              - Add shoreline processing
c              - Add TERDEF processing point-by-point 
c
c ARGUMENTS:
c    PASSED:  /CONTROL/ program control logicals
c             /GRID/    grid data, point counts
c             /CELL/    TERDEF, ITERREP
c             x    - real x coordinate of current point
c             y    - real y coordinate of current point
c             dels - real amount to perturb x,y for peak processing
c             z    - real z elevation of current point
c
c  RETURNED:  none (called subroutines carry out changes)
c
c CALLING ROUTINES:  LOADDMDF, LOADNZGNR, LOADGNR, LOADLAZA, LOADOTHER
c
c EXTERNAL ROUTINES: IPTYPE, XYELEV, CELLIND, CELLDAT
c-----------------------------------------------------------------------
c --- Include parameters and file commons
      include 'params.trl'
      include 'control.trl'
      include 'grid.trl'
      include 'cell.trl'

c --- Local variables
      real void/-32000./
      integer itype/2/

c --- Skip if point is out of grid
c
      if(x.lt.xllm.or.x.gt.xurm.or.y.lt.yllm.or.y.gt.yurm) return

c --- Increment total and missing counters
      npextr=npextr+1
      if(z.LT.void) npmiss=npmiss+1

      xkm=x*.001
      ykm=y*.001
c --- If echoing is selected, write data to file before filtering
      if(lrawecho) write(iorawo,fmt='(2f11.3,f12.3)') xkm,ykm,z

c --- If coastal processing invoked, find water/land type for point
c     and process as needed
c
c     Disabled in V. 3.67 L. 050824 so that filtering can still occur,
c     land type defaulting to land if coastline processing not invoked
c
c      if(LCOAST) then
        itype=iptype(xkm,ykm)+1

        if(iterrep(itype).EQ.3) then
            z=terdef(itype)
        else
            if(inoiserep(itype).gt.0) then
              if(z.lt.znoise(itype).and.z.gt.void) then
                if(inoiserep(itype).eq.1) z=void-1.
                if(inoiserep(itype).eq.2) z=znoise(itype)
                if(inoiserep(itype).eq.3) z=terdef(itype)
c ---         Increment noisy counter
                npnoise=npnoise+1
              endif
            endif
            if(z.LT.void) then
              if(iterrep(itype).EQ.2) then
                z=terdef(itype)
              else
                return
              endif
            endif
        endif
c      endif

c --- Sweep through discrete points and update elevations
c --- when option is ON                       ---  call XYELEV
      if(LXY) call XYELEV(x,y,z)

c --- Determine cell index (ix,iy) and perturbed cell
c --- index (ixp,iyp)                         ---  call CELLIND
      call CELLIND(x,y,dels,ix,iy,ixp,iyp)
c --- Process if computed cell is in grid
      if(ix .GE. 1 .AND. ix .LE. nx) then
        if(iy .GE. 1 .AND. iy .LE. ny) then
c ---     Looks OK -- add to cell data      ---  call CELLDAT
          call CELLDAT(ix,iy,z)
        endif
      endif
c --- For PEAK processing, perturb point by "dels" about
c --- (xi,yi) and process elevation data in each cell in
c --- which any of these point positions fall (only the max
c --- elevation in a cell is retained, so duplicate entries
c --- are allowed).  This perturbation seeks to capture peak
c --- elevations that occur near the boundary of a cell.
      if(LPEAK) then
        if(ixp .NE. ix .OR. iyp .NE. iy) then
          if(ixp .GE. 1 .AND. ixp .LE. nx) then
            if(iyp .GE. 1 .AND. iyp .LE. ny) then
              call CELLDAT(ixp,iyp,z)
              if(ix .GE. 1 .AND. ix .LE. nx) then
                call CELLDAT(ix,iyp,z)
              endif
              if(iy .GE. 1 .AND. iy .LE. ny) then
                call CELLDAT(ixp,iy,z)
              endif
            endif
          endif
        endif
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine findglob (xlonwest,ylat,globname)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 000411          FINDGLOB
c               J. Chang, Earth Tech
c
c PURPOSE:     FINDGLOB decides the name of the GTOPO30 terrain file
c              based on the longitude and latitude specified.  The
c              GTOPO30 terrain files follow the naming convention below.
c
c --- Update
c     Ver 2.1  Level 990528 to 000411       DGS
c              Revise logic to treat branch at 180 deg. and use real
c              longitudes as provided in argument list to place point.
c
c INPUT:
c   xlonwest  real  west longitude
c   ylat      real  north latitude
c
c OUTPUT:
c   globname char*7   GTOPO30 file name
c
c                Latitude          Longitude                  Elevation
c    Tile    Minimum  Maximum   Minimum  Maximum   Minimum  Maximum  Mean  Std.Dev.
c   -------  ----------------   ----------------   --------------------------------
c   
c   W180N90     40       90       -180    -140         1      6098    448     482
c   W140N90     40       90       -140    -100         1      4635    730     596
c   W100N90     40       90       -100     -60         1      2416    333     280
c   W060N90     40       90        -60     -20         1      3940   1624     933
c   W020N90     40       90        -20      20       -30      4536    399     425
c   E020N90     40       90         20      60      -137      5483    213     312
c   E060N90     40       90         60     100      -152      7169    509     698
c   E100N90     40       90        100     140         1      3877    597     455
c   E140N90     40       90        140     180         1      4588    414     401
c   W180N40    -10       40       -180    -140         1      4148    827     862
c   W140N40    -10       40       -140    -100       -79      4328   1321     744
c   W100N40    -10       40       -100     -60         1      6710    375     610
c   W060N40    -10       40        -60     -20         1      2843    212     168
c   W020N40    -10       40        -20      20      -103      4059    445     298
c   E020N40    -10       40         20      60      -407      5825    727     561
c   E060N40    -10       40         60     100         1      8752   1804    1892
c   E100N40    -10       40        100     140       -40      7213    692     910
c   E140N40    -10       40        140     180         1      4628    549     715
c   W180S10    -60      -10       -180    -140         1      2732    188     297
c   W140S10    -60      -10       -140    -100         1       910     65     124
c   W100S10    -60      -10       -100     -60         1      6795   1076    1356
c   W060S10    -60      -10        -60     -20         1      2863    412     292
c   W020S10    -60      -10        -20      20         1      2590   1085     403
c   E020S10    -60      -10         20      60         1      3484    893     450
c   E060S10    -60      -10         60     100         1      2687    246     303
c   E100S10    -60      -10        100     140         1      1499    313     182
c   E140S10    -60      -10        140     180         1      3405    282     252
c   W180S60    -90      -60       -180    -120         1      4009   1616    1043
c   W120S60    -90      -60       -120     -60         1      4743   1616     774
c   W060S60    -90      -60        -60       0         1      2916   1866     732
c   W000S60    -90      -60          0      60         1      3839   2867     689
c   E060S60    -90      -60         60     120         1      4039   2951     781
c   E120S60    -90      -60        120     180         1      4363   2450     665
c
c
c CALLING ROUTINES:   FINDFILE
c
c-----------------------------------------------------------------------
c
      character*7 globname
c
c *** Convert from west to east longitude, to be consistent with the 
c     convention used in the GTOPO30 data set.
c
      xlon=-xlonwest

c --- Condition E. Longitude to lie in range -180 to +180
      if(xlon.LT.-180.) xlon=xlon+360.
      if(xlon.GE.180.) xlon=xlon-360.
c
c *** Find the appropriate degree square, as indicated by the north
c     latitude (LAT) and east longitude (LON).
c
      if (ylat.ge.0.) then
        lat=nint(ylat)
      else
        lat=nint(ylat)-1
      end if
c
c *** Construct file name
c
      if (lat.ge.40) then
        globname(5:7)='N90'
      else if (lat.ge.-10) then
        globname(5:7)='N40'
      else if (lat.ge.-60) then
        globname(5:7)='S10'
      else
        globname(5:7)='S60'
      end if
c
      if (globname(5:7).ne.'S60') then
c
        if (xlon.lt.-140.) then
          globname(1:4)='W180'
        else if (xlon.lt.-100.) then
          globname(1:4)='W140'
        else if (xlon.lt.-60.) then
          globname(1:4)='W100'
        else if (xlon.lt.-20.) then
          globname(1:4)='W060'
        else if (xlon.lt.20.) then
          globname(1:4)='W020'
        else if (xlon.lt.60.) then
          globname(1:4)='E020'
        else if (xlon.lt.100.) then
          globname(1:4)='E060'
        else if (xlon.lt.140.) then
          globname(1:4)='E100'
        else
          globname(1:4)='E140'
        end if
c
      else ! Antarctica files
c
        if (xlon.lt.-120.) then
          globname(1:4)='W180'
        else if (xlon.lt.-60.) then
          globname(1:4)='W120'
        else if (xlon.lt.0.) then
          globname(1:4)='W060'
        else if (xlon.lt.60.) then
          globname(1:4)='W000'
        else if (xlon.lt.120.) then
          globname(1:4)='E060'
        else
          globname(1:4)='E120'
        end if
c
      end if
c
      return
      end
c-----------------------------------------------------------------------
      subroutine setarm3(k)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050815           SETARM3
c               D. Strimaitis, E. Insley SRC
c
c ADAPTED from program ELEV in the ARM3 model
c               G. Lundberg, E. Chang  SAI
c
c
c PURPOSE:     SETARM3 prepares the information required to read
c              in a "sheet" of terrain data from files associated
c              with the ARM3 model.  Each file contains a number
c              of 1-degree sheets.  Terrain elevations (m) in each
c              sheet are posted at 30-sec intervals (less than 1000m)
c              in longitude and latitude (30-sec USGS digitized data).
c              Each sheet is referenced by the latitude and longitude of
c              the SE corner.  An indexing system locates the sheet
c              within the file.  Data for the continental U.S. are
c              placed in 4 files, with the following zonal boundaries:
c
c              ZONE  EAST       66-86 DEGREES W LONGITUDE
c                    CENTRAL    86-100
c                    MOUNTAIN  100-109
c                    WEST      109-125
c
c              All sheets which span the output grid and which are also
c              found in the specified ARM3 file are processed.  TERREL
c              must be applied to more than one ARM3 file if the grid
c              crosses the boundary between zones.
c
c --- Update
c     Ver 3.63 Level 031031 to Ver 3.65 Level 0050815   KAM
c              Add check for dateline crossing
c     Ver 3.5  Level 021018 to 031031       KAM
c              Move DB-type logicals from COMP
c     Ver 3.2  Level 000411 to 021018       DGS
c              Set datum string
c     Ver 2.1  Level 961004 to 000411       DGS
c              Define independent step size Along and Between strips
c
c
c ARGUMENTS:
c    PASSED:  /CONTROL/ program control logicals
c             /GRID/    output grid specification
c
c  RETURNED:  /DBINF/   ARM3 terrain data base information
c             /GSPAN/   revised span data
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'filnam.trl'
      include 'grid.trl'
      include 'gspan.trl'

c --- Local variables
      integer header(5)
      integer limit(4)
      data limit /19,13,8,15/

      lnotgrid = .FALSE.

      l30sec = .TRUE.
      l3cd = .FALSE.
      l3sec = .FALSE.
      l7_5min = .FALSE.
      l100met = .FALSE.
      lgtopo30 = .FALSE.
      lusgsglb = .FALSE.
      lnzgnr = .FALSE.
      lsrtm1 = .FALSE.
      lsrtm3 = .FALSE.
      lgnr = .FALSE.
      lfeet=.FALSE.

      open(ioinp,file=datafil(k),status='OLD')

      write(iolst,*)
      write(iolst,*) '--------------------------'
      write(iolst,*) 'ARM3 Data File Information'
      write(iolst,*) '--------------------------'
      write(iolst,*)

c --- Read DB header
      read(ioinp,1000) header
1000  format(5i10)
      write(iolst,330) header
330   format('HEADER READ IN AS:',5i10)

c --- Assign DB values
c --- Note: x,y here refer to WEST Longitude, and Latitude
c --- and the DB origin is set at the SE corner of the SE-most
c --- degree-square in the file
      ndx = header(1)
      lonsedb = header(2)
c ---   check for dateline crossing, and adjust - lons
        if(lbranch) then
          if(lonsedb.lt.0.) lonsedb=360.-lonsedb
        endif
      latsedb = header(3)
      nxdb = header(4)
      nydb = header(5)
      nxydb = nxdb*nydb
      nd=2*nxydb

      write(iolst,*) 'LONSEDB = ',lonsedb
      write(iolst,*) 'LATSEDB = ',latsedb

c --- Set the datum string
      dbdatum=darm3
      write(iolst,*) 'DBDATUM = ',dbdatum

c --- Read file index values
      read(ioinp,1100) (index(i),i=1,nd)
1100  format(6i20)
      write(iolst,*) 'ARM3 DATA INDEX READ IN O.K.'
      write(iolst,*)

c --- Check if the assigned DB zone includes the grid area
c --- Reset limit -- it is wrong on the ARM3 files
      limx = limit(ndx)

      if(maxx .LT. lonsedb)        goto 900
      if(minx .GT. lonsedb + limx) goto 900

c --- RESET MINX AND MAXX TO ZONE BOUNDARIES IF NECESSARY
c     if(minx .LT. lonsedb)      minx = lonsedb
c     if(maxx .GT. lonsedb+limx) maxx = lonsedb+limx

c --- Set the interval-step and number of points and strips (profiles)
c --- of terrain data in file
      npt=120
      nstr=120
c --- 30 second step = 120 steps/degree
      stepa = 1./120.
      stepb = stepa

      return

900   continue
      WRITE(iolst,*) 'ARM3 DATA BASE DOES NOT CONTAIN GRID AREA'
      lnotgrid = .TRUE.
      if(lnotgrid) notused = notused + 1

      return
      end
c-----------------------------------------------------------------------
      subroutine set3sec(k)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050815           SET3SEC
c               M. Spitzak, D. Strimaitis, SRC
c
c
c PURPOSE:     SET3SEC assigns terrain data base parameters for
c              the 3CD 3-second digitized terrain data files.
c
c --- Update
c     Ver 3.63 Level 031031 to Ver 3.65 Level 0050815   KAM
c              Add check for dateline crossing
c     Ver 3.5  Level 021018 to 031031       KAM
c              Move DB-type logicals from COMP
c     Ver 3.2  Level 000411 to 021018       DGS
c              Set datum string
c     Ver 2.1  Level 961004 to 000411       DGS
c              Define independent step size Along and Between strips
c
c ARGUMENTS:
c    PASSED:  /CONTROL/ program control logicals
c             /GRID/    output grid specification
c             /GSPAN/   range in grid coordinates
c
c  RETURNED:  /DBINF/   3SEC terrain data base information
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  CORNERS
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'grid.trl'
      include 'filnam.trl'
      include 'gspan.trl'

      lnotgrid=.FALSE.

      l3cd = .TRUE.
      l30sec = .FALSE.
      l3sec = .FALSE.
      l7_5min = .FALSE.
      l100met = .FALSE.
      lgtopo30 = .FALSE.
      lnzgnr = .FALSE.
      lsrtm1 = .FALSE.
      lsrtm3 = .FALSE.
      lgnr = .FALSE.
      lfeet=.FALSE.
c     open(ioinp,file=datafil(k),form='UNFORMATTED',status='OLD',
c    &             access=caccess)
      open(ioinp,file=datafil(k),status='OLD',form=cform,
     &           access=trim(cd3access))

c --- Get lat,lon of SE corner for this 1-deg DEM
      do n=6,40
        if(datafil(k)(n:n+3) .EQ. '.3cd' .OR.
     &     datafil(k)(n:n+3) .EQ. '.3CD') then
             read(datafil(k)(n-5:n-4),10) latsedb
             read(datafil(k)(n-3:n-1),20) lonsedb
10           format(i2)
20           format(i3)
             exit
         endif
      enddo

c ---   check for dateline crossing, and adjust - lons
        if(lbranch) then
          if(lonsedb.lt.0.) lonsedb=360.-lonsedb
        endif

c --- Set corners of Quad
      xse=lonsedb
      yse=latsedb
      xsw=xse+1.
      ysw=yse
      xnw=xse+1.
      ynw=yse+1.
      xne=xse
      yne=yse+1.

c --- Set the datum string
      dbdatum=d3cd

c --- Check if the terrain data in file overlaps the grid area
      maxxs=MAX(xnw,xsw)
      minxs=MIN(xne,xse)
      maxys=MAX(yne,ynw)
      minys=MIN(yse,ysw)
      if(maxx .LT. minxs .OR. minx .GT. maxxs) then
        write(iolst,*)'SET3SEC: 3CD Data file NOT in designated grid'
        lnotgrid=.TRUE.
      elseif(maxy .LT. minys .OR. miny .GT. maxys) then
        write(iolst,*)'SET3SEC: 3CD Data file NOT in designated grid'
        lnotgrid=.TRUE.
      endif
      if(lnotgrid) notused = notused + 1

c --- Set the interval-step and number of points and strips (profiles)
c --- of terrain data in file
      npt=1201
      nstr=1201

c --- Set variables so that 1 sheet is processed at a time
      nxdb = 1
      nydb = 1

c --- 3 second step = 1200 steps/degree
      stepa = 1./1200.
      stepb = stepa
      return
      end

c-----------------------------------------------------------------------
      subroutine setgener(k)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 031031          SETGENER
c               K. Morrison, Earth Tech
c
c PURPOSE:     SETGENER sets the logicals and opens the database file
c              for Canadian DMDF, New Zealand Generic, and Generic data
c              types.
c
c --- Update
c
c
c INPUT:
c   k  integer  The number of the terrain data file
c
c OUTPUT:
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'filnam.trl'

      l7_5min = .FALSE.
      l3sec = .FALSE.
      l3cd = .FALSE.
      l30sec = .FALSE.
      lgtopo30 = .FALSE.
      lusgsglb = .FALSE.
      lsrtm1 = .FALSE.
      lsrtm3 = .FALSE.
      lfeet=.FALSE.
      open(ioinp,file=datafil(k),status='OLD')

      return
      end
c-----------------------------------------------------------------------
      subroutine setglob(k)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050815           SETGLOB
c               J. Chang, Earth Tech
c
c PURPOSE:     SETGLOB prepares the information required to read the
c              USGS GTOPO30 global terrain data with 30 arc-second (~900 m)
c              resolution.  Each terrain (DEM) file covers 50 degrees of
c              latitude and 40 degrees of longitude, except for 60S to 90S
c              (antarctic region), where each terrain file covers 30 degrees
c              of latitude and 60 degrees of longitude.  Each degree-square
c              contains 120x120 pixels where elevation, in m, is defined.
c              Elevation is provided as 16-bit signed integer in a simple
c              binary (big endian) raster.  There are no header or trailer
c              bytes imbedded.  Therefore, for a 40 by 50 deg file, the DEM
c              file size is 40*50*120*120*2 = 57,600,000 bytes.  Each file
c              stores the data from the northwest corner first in row major
c              order (all the data for row 1, followed by all the data for
c              row 2, etc.)  The file name refers to the longitude and latitude
c              of the northwest corner of the file.  The following is a list of
c              DEM files and their associated borders:
c
c --- Update
c     Ver 3.63 Level 031031 to Ver 3.65 Level 050815   KAM
c              Add check for dateline crossing
c     Ver 3.5  Level 021018 to 031031       KAM
c              Move DB-type logicals from COMP
c     Ver 3.2  Level 000411 to 021018       DGS
c              Set datum string
c     Ver 2.1  Level 990528 to 000411       DGS
c              Define independent step size Along and Between strips
c
c INPUT:
c   k  integer  The ordinal number of terrain data files (need to figure out
c               lat/long from file name)
c
c                Latitude          Longitude                  Elevation
c    Tile    Minimum  Maximum   Minimum  Maximum   Minimum  Maximum  Mean  Std.Dev.
c   -------  ----------------   ----------------   --------------------------------
c   
c   W180N90     40       90       -180    -140         1      6098    448     482
c   W140N90     40       90       -140    -100         1      4635    730     596
c   W100N90     40       90       -100     -60         1      2416    333     280
c   W060N90     40       90        -60     -20         1      3940   1624     933
c   W020N90     40       90        -20      20       -30      4536    399     425
c   E020N90     40       90         20      60      -137      5483    213     312
c   E060N90     40       90         60     100      -152      7169    509     698
c   E100N90     40       90        100     140         1      3877    597     455
c   E140N90     40       90        140     180         1      4588    414     401
c   W180N40    -10       40       -180    -140         1      4148    827     862
c   W140N40    -10       40       -140    -100       -79      4328   1321     744
c   W100N40    -10       40       -100     -60         1      6710    375     610
c   W060N40    -10       40        -60     -20         1      2843    212     168
c   W020N40    -10       40        -20      20      -103      4059    445     298
c   E020N40    -10       40         20      60      -407      5825    727     561
c   E060N40    -10       40         60     100         1      8752   1804    1892
c   E100N40    -10       40        100     140       -40      7213    692     910
c   E140N40    -10       40        140     180         1      4628    549     715
c   W180S10    -60      -10       -180    -140         1      2732    188     297
c   W140S10    -60      -10       -140    -100         1       910     65     124
c   W100S10    -60      -10       -100     -60         1      6795   1076    1356
c   W060S10    -60      -10        -60     -20         1      2863    412     292
c   W020S10    -60      -10        -20      20         1      2590   1085     403
c   E020S10    -60      -10         20      60         1      3484    893     450
c   E060S10    -60      -10         60     100         1      2687    246     303
c   E100S10    -60      -10        100     140         1      1499    313     182
c   E140S10    -60      -10        140     180         1      3405    282     252
c   W180S60    -90      -60       -180    -120         1      4009   1616    1043
c   W120S60    -90      -60       -120     -60         1      4743   1616     774
c   W060S60    -90      -60        -60       0         1      2916   1866     732
c   W000S60    -90      -60          0      60         1      3839   2867     689
c   E060S60    -90      -60         60     120         1      4039   2951     781
c   E120S60    -90      -60        120     180         1      4363   2450     665
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'filnam.trl'
      include 'grid.trl'
      include 'gspan.trl'

      logical lbig_end
      lnotgrid = .FALSE.

      l3cd = .FALSE.
      l30sec = .FALSE.
      l3sec = .FALSE.
      l7_5min = .FALSE.
      l100met = .FALSE.
      lgtopo30 = .TRUE.
      lusgsglb = .FALSE.
      lnzgnr = .FALSE.
      lsrtm1 = .FALSE.
      lsrtm3 = .FALSE.
      lgnr = .FALSE.
      lfeet=.FALSE.

c     open(ioinp,file=datafil(k),form='UNFORMATTED',status='OLD',
c    &           access='direct',recl=240)
      open(ioinp,file=datafil(k),form=cform,status='OLD',
     &           access='direct',recl=240)
c                Read the binary data with "direct access" with
c                a record length of 240 bytes, i.e., 120 16-bit
c                signed integers.
c      ===================================================================
c                Note that some UNIX Fortran compilers use 4-byte
c                word by default as the unit for the record length
c                for unformatted direct I/O, as a result, recl=240
c                could mean that record length is 960 bytes.  In
c                this case, either change recl=240 to recl=60, or if
c                appropriate, leave recl=240 and compile the program
c                with the "-bytereclen" switch.
c      ===================================================================
c
c -----------------------------------------------------------------
c *** Specify computer platform, as the GTOPO30 data are stored in
c     16-bit signed integers in "big endian" byte order.  It is
c     necessary to swap bytes if the machine platform is "little
c     endian," as indicated by LBIGENDIAN=.FALSE.
c -----------------------------------------------------------------
c
c      lbigendian=.FALSE. ! for Intel and DEC platforms
c      lbigendian=.TRUE. ! for Motorola, SUN, HP, and SGI platforms
      lbigendian=lbig_end(1)
      
      write(iolst,*)
      write(iolst,*) '-----------------------------'
      write(iolst,*) 'GTOPO30 Data File Information'
      write(iolst,*) '-----------------------------'
      write(iolst,*)

c --- Assign DB values
      if (justname(k)(5:7).eq.'S60' .or. justname(k)(5:7).eq.'s60') then 
c        ! antarctica files (30 degrees of latitude and 60 degrees of longitude)
        l50by40=.FALSE.
        nxdb=60 ! longitude
        nydb=30 ! latitude
      else if (justname(k)(5:7).eq.'N90' .or. justname(k)(5:7).eq.'n90' 
     &    .or. justname(k)(5:7).eq.'N40' .or. justname(k)(5:7).eq.'n40' 
     &    .or. justname(k)(5:7).eq.'S10' .or. justname(k)(5:7).eq.'s10')
     &    then
        l50by40=.TRUE.
        nxdb=40 ! longitude
        nydb=50 ! latitude
      else
        print *,'Error in SETGLOB'
        print *,'Data file name not recognized: ',justname(k)
        stop
      end if
c
c *** Get lat/long for the NW corner of the data file
c
      read (justname(k)(2:4),*) lonnwdb
      read (justname(k)(6:7),*) latnwdb
      if (justname(k)(1:1).eq.'W' .or. justname(k)(1:1).eq.'w') then
        lonnwdb=-lonnwdb ! still negative for western hemisphere here
c ---   crosses dateline
        elseif (lbranch) then
          lonnwdb=lonnwdb-360.
      end if
      if (justname(k)(5:5).eq.'S' .or. justname(k)(5:5).eq.'s') then
        latnwdb=-latnwdb
      end if
c
c *** Get lat/long for the SE corner of the data file
c
      if (l50by40) then
        lonsedb=lonnwdb+40 ! still negative for western hemisphere here
        latsedb=latnwdb-50
      else
        lonsedb=lonnwdb+60 ! still negative for western hemisphere here
        latsedb=latnwdb-30
      end if
c
c *** Change convention from eastern longitude to western longitude
c
      lonsedb=-lonsedb
      lonnwdb=-lonnwdb
c        
      nxydb = nxdb*nydb
      nd=2*nxydb
c
      write(iolst,*) 'LONSEDB = ',lonsedb
      write(iolst,*) 'LATSEDB = ',latsedb
      write(iolst,*) 'Is the machine platform "big endian?" ',lbigendian
c
c --- Check if the grid area is completely outside of the assigned
c     terrain file.
c     Note that MAXX and MINX are western longitudes.
c
      if(maxx .LT. lonsedb) goto 900
      if(minx .GT. lonnwdb) goto 900
      if(maxy .LT. latsedb) goto 900
      if(miny .GT. latnwdb) goto 900

c --- Set the datum string
      dbdatum=dgtopo30
c
c --- Set the interval-step and number of points and strips of terrain data
c     in file
c
      npt=120
      nstr=120
c --- 30 second step = 120 steps/degree
      stepa = 1./120.
      stepb = stepa
c
      return
c
900   continue
      WRITE(iolst,*) 'GTOPO30 DATA BASE DOES NOT CONTAIN GRID AREA'
      lnotgrid = .TRUE.
      if(lnotgrid) notused = notused + 1
c
      return
      end
c-----------------------------------------------------------------------
      subroutine setsrtm(k,ispan)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050817           SETSRTM
c               K. Morrison, Earth Tech
c
c PURPOSE:     SETSRTM prepares the information required to read the
c              NASA SRTM  terrain data with either 1 arc-second (~30 m) or
c              3 arc-second (~90 m) resolution.  Each terrain (HGT) file covers
c              1 degree of latitude and longitude.  Each file contains either 
c              3601x3601 pixels (1 arc-sec) or 1201x1201 pixels (3 arc-second) 
c              where elevation, in m, is defined.  Elevation is provided as 
c              16-bit signed integer in a simple binary (big endian) raster.
c              There are no header or trailer bytes imbedded.  Therefore, for 
c              a 1 arc-second file, the HGT file size is 3601x3601*2 = 25,934,402
c              bytes, while for a 3 arc-second file the file size is 1201x1201x2
c              = 2,884,802 bytes.  Each file stores the data from the northwest 
c              corner first in row major order (all the data for row 1, followed
c              by all the data for row 2, etc.)  The file name refers to the 
c              longitude and latitude of the southwest corner of the file.  Due to
c              overlaps, the first line of the file is skipped, since it should 
c              also be the last line of the sheet to the north.
c
c --- Updates
c --- Ver. 3.65 Level 050815 to Ver. 3.66 Level 050817   KAM
c              Remove compiler warning about DATA array not being used
c --- Ver. 3.63 Level 040305 to Ver. 3.65 Level 050815   KAM
c              Add check for dateline crossing
c --- Ver. 3.5 Level 040131 to Ver. 3.55 Level 040305  KAM
c              Correct detection of unused files
c --- Ver. 3.5 Level 031017 to Level 040131  KAM
c              Correct treatment of eastern hemisphere
c
c INPUTS:
c   k      integer  The ordinal number of terrain data files (need to figure 
c                   out lat/long from file name)
c   ispan  integer  The arc-second type of SRTM file (1 or 3)
c
c
c  RETURNED:  /DBINF/   Terrain data base information
c             /CONTROL/ Program control information
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'filnam.trl'
      include 'grid.trl'
      include 'gspan.trl'

      integer*2 data(3601)
      
      logical lexist,lbig_end
      lnotgrid = .FALSE.

      l3cd = .FALSE.
      l30sec = .FALSE.
      l3sec = .FALSE.
      l7_5min = .FALSE.
      l100met = .FALSE.
      lgtopo30 = .FALSE.
      lusgsglb = .FALSE.
      lnzgnr = .FALSE.
      lsrtm1 = .FALSE.
      lgnr = .FALSE.
      lfeet=.FALSE.

      idist=ispan
c
c -----------------------------------------------------------------
c *** Specify computer platform, as the SRTM data are stored in
c     16-bit signed integers in "big endian" byte order.  It is
c     necessary to swap bytes if the machine platform is "little
c     endian," as indicated by LBIGENDIAN=.FALSE.
c -----------------------------------------------------------------
c
c      lbigendian=.FALSE. ! for Intel and DEC platforms
c      lbigendian=.TRUE. ! for Motorola, SUN, HP, and SGI platforms
      lbigendian=lbig_end(1)
      
      write(iolst,*)
      write(iolst,*) '--------------------------'
      write(iolst,*) 'SRTM Data File Information'
      write(iolst,*) '--------------------------'
      write(iolst,*)

c ----------------------
c --- Lahey F95 Compiler
c ----------------------
      inquire(file=datafil(k),exist=lexist,recl=isize)

c --- Check that file exists
      if ((justname(k)(1:1).eq.'S' .or. justname(k)(1:1).eq.'s' .or.
     &   justname(k)(1:1).eq.'N' .or. justname(k)(1:1).eq.'n') .and. 
     &   (justname(k)(4:4).eq.'E' .or. justname(k)(4:4).eq.'e' .or.
     &   justname(k)(4:4).eq.'W' .or. justname(k)(4:4).eq.'w') .and.
     &   lexist) then 
        nxdb=1 ! longitude
        nydb=1 ! latitude
      else
        print *,'Error in SETSRTM'
        print *,'Data file name not recognized: ',justname(k)
        stop
      end if
c
c --- check that the file is the right type, since both file types
c     have the same nomenclature
c
      if((isize.lt.3000000.and.ispan.eq.1).or.
     &   (isize.gt.3000000.and.ispan.eq.3)) then
         idist=4-ispan
         write(iolst,99) ispan,idist
99    format(' WARNING in SETSRTM'/' Wrong file type'/' Requested - ',
     &  i1,' arc-second'/'   Found - ',i1,' arc-second'//
     &  'Processing according to actual type'/)
      endif         
c
c *** Get lat/long for the NW corner of the data file
c
      read (justname(k)(5:7),*) lonnwdb
      read (justname(k)(2:3),*) latsedb
c
c ---  change east longitude to west, and if domain crosses dateline
c      make the longitude positive if east
c
      if (justname(k)(4:4).eq.'E' .or. justname(k)(4:4).eq.'e') then
          if(lbranch) then
            lonnwdb=360-lonnwdb
          else
            lonnwdb=-lonnwdb
          endif
      end if
      if (justname(k)(1:1).eq.'S' .or. justname(k)(1:1).eq.'s') then
        latsedb=-latsedb
      end if
      lonsedb=lonnwdb-1 
      latnwdb=latsedb+1
c
      nxydb = nxdb*nydb
      nd=2*nxydb
c
      write(iolst,*) 'LONSWDB = ',lonnwdb
      write(iolst,*) 'LATSWDB = ',latsedb
      write(iolst,*) 'Is the machine platform "big endian?" ',lbigendian
c
c --- Check if the grid area is completely outside of the assigned
c     terrain file, allowing for overlaps.
c     Note that longitudes are western (eastern hemisphere is negative).
c
      if(max(xlonsw,xlonnw) .LT. float(lonsedb)) goto 900
      if(min(xlonse,xlonne) .GT. float(lonnwdb)+8.34e-4) goto 900
      if(max(ylatne,ylatnw) .LT. float(latsedb)-8.33e-4) goto 900
      if(min(ylatse,ylatsw) .GT. float(latnwdb)) goto 900
c
c --- Open the file
c       open(ioinp,file=datafil(k),form='UNFORMATTED',status='OLD',
c    &               access='TRANSPARENT',recl=1)
        open(ioinp,file=datafil(k),form=cform,status='OLD',
     &               access=trim(caccess),recl=irecl)
c                    Read the binary data with "transparent access".
c
c --- Set the datum string, interval step, and number of points and strips
c     of terrain data in file
c
      if(idist.eq.1) then
c ---    1 second step = 3600 steps/degree
         lsrtm1 = .TRUE.
         lsrtm3 = .FALSE.
         dbdatum=dsrtm1
         npt=3600
         nstr=3600
         stepa = 1./3600.
         stepb = stepa
      elseif(idist.eq.3) then
c ---    3 second step = 1200 steps/degree
         lsrtm1 = .FALSE.
         lsrtm3 = .TRUE.
         dbdatum=dsrtm3
         npt=1200
         nstr=1200
         stepa = 1./1200.
         stepb = stepa
      endif
c
c --- Skip the first line of the file
c
      read(ioinp) (data(i),i=1,npt),data(npt+1)
c 
c --- Remove compiler warning
        data(1)=data(1)

      return
c
900   continue
      WRITE(iolst,*) 'SRTM DATA BASE DOES NOT CONTAIN GRID AREA'
      lnotgrid = .TRUE.
      if(lnotgrid) notused = notused + 1
c
      return
      end
c-----------------------------------------------------------------------
      subroutine setusgsg(k)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050815          SETUSGSG
c               S. Du, D. Strimaitis, Earth Tech
c
c
c PURPOSE:     SETUSGSG assigns terrain data base parameters for
c              the USGS Global Lambert Azimuthal terrain data files
c
c
c --- UPDATES
c     Ver 3.63 Level 041013 to Ver 3.65 Level 050815   KAM
c              Add check for dateline crossing, and add GRID common
c              for LBRANCH logical
c     Ver 3.61 Level 031031 to 041013       DGS
c              Fixed format 1001 to write integers instead of reals
c     Ver 3.5  Level 021018 to 031031       KAM
c              Moved logicals and file opening from COMP
c              Replaced local variable FILNAM with JUSTNAME
c     Ver 3.2  Level 010713 to 021018       DGS
c              Revised GET4CNR argument list; removed /GRID/
c
c ARGUMENTS:
c    PASSED:  
c              k -  integer  The ordinal number of terrain data file  
c                            (need to figureout file name)
c             /FILNAM/  Database file names
c
c  RETURNED:  /USGSLA/  Terrain data base information
c             /CONTROL/ Program control data
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  GET4CNR
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'filnam.trl'
      include 'control.trl'
        include 'grid.trl'

      common /USGSLA/ nxi,nyi,dxi,dyi,xorgi,yorgi,
     &                rlati,rloni,nxoff,nyoff

      l3cd = .FALSE.
      l30sec = .FALSE.
      l3sec = .FALSE.
      l100met = .FALSE.
      lgtopo30 = .FALSE.
      lusgsglb = .TRUE.
      lnzgnr = .FALSE.
      lsrtm1 = .FALSE.
      lsrtm3 = .FALSE.
      lgnr = .FALSE.
      lfeet=.FALSE.
c     open(ioinp,file=datafil(k),form='UNFORMATTED',status='OLD',
c    &           access='transparent')
      open(ioinp,file=datafil(k),status='old',form=cform,
     &           access=trim(caccess), recl=irecl)

c --- Hardwire a set of parameters that specify the
c --- USGS Lambert Azimuthal image data files

c --- North America
      if(justname(k)(1:2).eq.'na'
     1 .or.justname(k)(1:2).eq.'Na'
     1 .or.justname(k)(1:2).eq.'nA'
     1 .or.justname(k)(1:2).eq.'NA') then
        nxi = 9223
        nyi = 8996
        dxi = 1.0
        dyi = 1.0 
        xorgi = -4487.
        yorgi = -4515.
        rlati = 50.0
        rloni = -100.              
        nxoff = 0
        nyoff = 0

c --- South America
      else if(justname(k)(1:2).eq.'sa'
     1 .or.justname(k)(1:2).eq.'Sa'
     1 .or.justname(k)(1:2).eq.'sA'
     1 .or.justname(k)(1:2).eq.'SA') then
        nxi = 6000
        nyi = 8000
        dxi = 1.0
        dyi = 1.0 
        xorgi = -3000.
        yorgi = -4899.
        rlati = -15.
        rloni = -60.              
        nxoff = 0
        nyoff = 0

c --- Australia and Pacific 
      else if(justname(k)(1:2).eq.'ap'
     1 .or.justname(k)(1:2).eq.'Ap'
     1 .or.justname(k)(1:2).eq.'aP'
     1 .or.justname(k)(1:2).eq.'AP') then
        nxi = 9300
        nyi = 8000
        dxi = 1.0
        dyi = 1.0 
        xorgi = -5000.
        yorgi = -3944.891
        rlati = -15.
        rloni = 135.              
        nxoff = 0
        nyoff = 0

c --- Africa
      else if(justname(k)(1:2).eq.'af'
     1 .or.justname(k)(1:2).eq.'Af'
     1 .or.justname(k)(1:2).eq.'aF'
     1 .or.justname(k)(1:2).eq.'AF') then
        nxi = 8350
        nyi = 9276
        dxi = 1.0
        dyi = 1.0 
        xorgi = -4458.
        yorgi = -4795.
        rlati = 5.
        rloni = 20.              
        nxoff = 0
        nyoff = 0

c --- Asia-Europe Continent 
      else if(justname(k)(1:2).eq.'ea'
     1 .or.justname(k)(1:2).eq.'Ea'
     1 .or.justname(k)(1:2).eq.'eA'
     1 .or.justname(k)(1:2).eq.'EA') then
c --- Europe
        if(justname(k)(7:7).eq.'e'
     1     .or.justname(k)(7:7).eq.'E') then
          nxi = 13000
          nyi = 13000
          dxi = 1.0
          dyi = 1.0 
          xorgi = -3000.
          yorgi = -4999.
          rlati = 55.
          rloni = 20.              
          nxoff = 0
          nyoff = 0
c --- Asia
        else if(justname(k)(7:7).eq.'a'
     1     .or.justname(k)(7:7).eq.'A') then
          nxi = 13000
          nyi = 12000
          dxi = 1.0
          dyi = 1.0 
          xorgi = -8000.
          yorgi = -5499.
          rlati = 45.
          rloni = 100.              
          nxoff = 0
          nyoff = 0
        endif

      else
c ---   Need to rename input data file
        write(*,*) 'Sorry, but the input data file needs a new name'
        write(*,*)
        write(*,*) 'Suggested input data file name for each continent'
        write(*,*)
        write(*,*) 'Continent:          file name:'
        write(*,*)
        write(*,*) 'NORTH AMERICA     : NADEM.IM'
        write(*,*) 'SOUTH AMERICA     : SADEM.IM'
        write(*,*) 'AFRICA            : AFDEM.IM'
        write(*,*) 'AUSTRALIA&PACIFIC : APDEM.IM'
        write(*,*) 'ASIA              : EADEM_A.IM'
        write(*,*) 'EUROPE            : EADEM_E.IM'
        stop
      endif

c ---   check for dateline crossing, and adjust + lons
        if(lbranch.and.rloni.gt.0.) rloni=rloni-360.

csd   Calculate the relative positions of the 4 corners of output
csd   domain in the domain of global lcc data  
c --- Computes a window to assist in speeding up the processing
c --- LLC,LRC,URC,ULC
      call GET4CNR(dxi,dyi,xorgi,yorgi,nxi,nyi,rlati,rloni)
      
      write(iolst,*)
      write(iolst,*) '-----------------------------------------'
      write(iolst,*) 'USGS Global Lambert Azimuthal Information'
      write(iolst,*) '-----------------------------------------'
      write(iolst,*)

      write(iolst,1000)
1000  format(/1x,'Global landuse dataset variables:')
      write(iolst,1001) nxi,nyi
1001  format(3x,'Number of elements in X input:',2i8)
      write(iolst,1002) dxi,dyi
1002  format(3x,'Cell size in X & Y input (km):',2f10.3)
      write(iolst,1003) xorgi,yorgi
1003  format(3x,'The input LZ origin X & Y (km):',2f10.3)
      write(iolst,1004) rlati,rloni
1004  format(3x,'Reference lat/lon of the input dataset:',2f10.3)
      write(iolst,1005) nxoff,nyoff 
1005  format(3x,'False easting and northing:',2i5)


      return
      end
c-----------------------------------------------------------------------
      subroutine rdusgshd(k)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 061109          RDUSGSHD
c               D. Strimaitis, SRC
c               updated by E. Insley
c
c --- Update
c     Ver 3.68 Level 051201 to Ver 3.683 Level 061109   DGS
c              Correct output UTM zone name (was same as input zone)
c              and change to signed integer before GLOBE1 call;
c              recognize that output map projection may not be UTM
c              when reporting messages
c              Note: code assumes that utm zone read from DEM header is
c              a signed integer (negative in S. hemispere)
c     Ver 3.67 Level 050815 to Ver 3.68 Level 051201   KAM
c              Add additional dummy arguments in calls to SETTRANO to
c              be consistent with CTGPROC V. 2.65 Lev. 051201
c     Ver 3.63 Level 041013 to Ver 3.65 Level 050815   KAM
c              Add check for dateline crossing
c     Ver 3.61 Level 040920 to 041013       KAM
c              Correct output UTM zone in call to SETTRANO
c     Ver 3.6  Level 031031 to 040920       DGS
c              Define stepa and stepb as fractional step size for the
c              USGS "3-sec" DEM format (allows use of non 3-sec data)
c     Ver 3.5  Level 030402 to 031031       KAM
c              Move logicals and file opening from COMP
c     Ver 3.3  Level 021018 to 030402       DGS
c              Add false easting/northing to GLOBE1 calls
c     Ver 3.2  Level 020513 to 021018       DGS
c              Set datum string with GLOBE1/GLOBE calls
c     Ver 3.0  Level 020318 to Level 020513    - D. Strimaitis
c              Made the 'DEM-2' (~90m ; 3 arc-sec) code applicable
c              to the standard DEM processing (retired DEM-2)
c     Ver 3.0  Level 020315 to 020318   - C. Czaja
c              Compute stepb and nstr in USGS90 DEM-2 format.
c              Stepa and npt are computed in LOAD.
c     Ver 3.0  Level 001207 to 020315       DGS
c              Use NINT when converting corners from seconds to integer
c              degrees
c     Ver 2.1  Level 000411 to 001207       S. Du
c              Read in lat/lon of 4 corners of DEM-2 (~90m) sheet
c     Ver 2.1  Level 000320 to 000411       DGS
c              Allow number of strips to be different from number of
c              points along strip (for 1-deg DEM)
c              Change USGS '30m' references to USGS '7_5' for 7.5
c              minute quadrangle files
c     Ver 2.1  Level 990130 to 000320       DGS
c              Pass elevation factor to new common variable ZFAC
c     Ver 2.0  Level 981025 to 990130       JSS, DGS
c              Remove USGS warning about Level 3 consistency
c              Note when UTM coordinates are remapped to grid zones
c     Ver 2.0  Level 961004 to 981025       DGS
c              Change USGS warning about Level 3 consistency  
c
c PURPOSE:     Reads the USGS terrain data base header record and assigns
c              terrain data base parameters for 1-Deg DEM (~90m) or
c              7.5 min (usually 30m) DEM digitized terrain data files.
c              Each file contains data for a single quad sheet.
c
c
c ARGUMENTS:
c    PASSED:  /CNTRL/   program control logicals
c             /GRID/    output grid specification
c             /GSPAN/   range in grid coordinates
c
c  RETURNED:  /DBINF/   terrain data base information
c             /CORNR/   UTM coord. at 4 corners of sheet
c             /SEC3CNR/ lat/lon of the 4 corners of the DEM-2 sheet
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  SETTRANO, GLOBE
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'cornr.trl'
      include 'filnam.trl'
      include 'dbinf.trl'
      include 'grid.trl'
      include 'gspan.trl'

c --- Local variables
      character title*144
      integer*2 ixunit,izunit,nside,iacc,nprof(2)
      real rdelta(3)
      real*8 adum(15),qcornr(8),zmnmx(2),rdum

c --- For coordinate transformations
      character*12 caction
      character*4 c4dum
      real*8 vecti(9),vecto(9)
      logical lconvert

      common/tmp2/adum,zmnmx,rdum
      common/tmp3/nside,iacc
      common /SEC3CNR/ cnrlatse,cnrlonse,cnrlatne,cnrlonne,
     &          cnrlatsw,cnrlonsw,cnrlatnw,cnrlonnw

      data xdum/0.0/

      lnotgrid=.FALSE.
      lzone=.FALSE.
      lconvert=.FALSE.

      l3cd = .FALSE.
      l30sec = .FALSE.
      l100met = .FALSE.
      lgtopo30 = .FALSE.
      lsrtm1 = .FALSE.
      lsrtm3 = .FALSE.
      lnzgnr = .FALSE.
      lgnr = .FALSE.
      lfeet=.FALSE.
      open(ioinp,file=datafil(k),status='OLD')

c --- Set Scale Factor of Tangential TM projection (1.0)
      tmsone=1.00000

c --- Set output utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

c --- Set input dataset false Easting/Northing to zero
      feasti=0.0
      fnorti=0.0

      write(iolst,*)
      write(iolst,*) '--------------------------'
      write(iolst,*) 'USGS Data File Information'
      write(iolst,*) '--------------------------'
      write(iolst,*)

c --- Read header record of DB terrain file
      read(ioinp,1000) title,icode,adum,ixunit,izunit,nside,
     &                 qcornr,zmnmx,rdum,iacc,rdelta,nprof
1000  format(a144,4i6,15d24.15,3i6,11d24.15,i6,3e12.6,2i6)

c --- Report title to list-file
      write(iolst,*) 'Identification of DB file:'
      write(iolst,*) title
      write(iolst,*)

      if(l7_5min) then
c ---   Make sure that the data are 7.5 minute DEM data in standard format
        if(icode(1) .NE. 1) then
           write(iolst,*) 'RDUSGSHD:  Not a 7.5 min Quad: icode(1) = ',
     &                   icode(1)
        endif
        if(icode(3) .NE. 1) then
           write(iolst,*) 'RDUSGSHD:  Not a UTM system: icode(3) = ',
     &                    icode(3)
        endif
        if(icode(4) .NE. iutm .AND. LUTM) then
          write(iolst,*) 'RDUSGSHD:  UTM coordinates in zone ',icode(4)
          write(iolst,*) 'will be converted to grid UTM zone ',izone
          lzone =.TRUE.
        endif
        if(ixunit .NE. 2) then
          write(iolst,*) 'RDUSGSHD:  UTM units not meters: ixunit = ',
     &                     ixunit
        endif

c ---   Assign data to internal variables

c ---   Corners of Quad (assigned into a single precision variable)
        xsw=qcornr(1)
        ysw=qcornr(2)
        xnw=qcornr(3)
        ynw=qcornr(4)
        xne=qcornr(5)
        yne=qcornr(6)
        xse=qcornr(7)
        yse=qcornr(8)
c ---   Convert corner coordinates from meters to km
        xswkm = xsw/1000.
        yswkm = ysw/1000.
        xnwkm = xnw/1000.
        ynwkm = ynw/1000.
        xnekm = xne/1000.
        ynekm = yne/1000.
        xsekm = xse/1000.
        ysekm = yse/1000.
c ---   Strip information (profiles)
        nstr=nprof(2)
        delx=0.0
        dely=rdelta(2)
c ---   Vertical scaling factor
        zfac=rdelta(3)
c ---   Units logical
        if(izunit .EQ. 1) then
          lfeet=.TRUE.
        else
          lfeet=.FALSE.
        endif

c ---   Assign values to common variables NOT specified in DB
        lonsedb=1
        latsedb=1
        nxdb=1
        nydb=1
        nxydb=1
        nd=2

c ---   Set the datum string for the quad map
        dbdatum=dusgs30

c ---   Coordinate conversion needed for output grid?
        if(LUTM.and.LZONE) lconvert=.TRUE.
        if(dbdatum.NE.datum) lconvert=.TRUE.
        if(.not.LUTM) lconvert=.TRUE.

        if(LCONVERT) then

c ---     Set translation vectors to the output grid
            call SETTRANO('UTM     ',icode(4),xdum,xdum,xdum,xdum,
     &                  feasti,fnorti,
     &                  iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &                  feast,fnorth,
     &                  caction,vecti,vecto)

c ---     Translate corner coordinates
          call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &               xswkm,yswkm,xkmo,ykmo,idum,c4dum)
          xswkm=xkmo
          yswkm=ykmo
          call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &               xnwkm,ynwkm,xkmo,ykmo,idum,c4dum)
          xnwkm=xkmo
          ynwkm=ykmo
          call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &               xnekm,ynekm,xkmo,ykmo,idum,c4dum)
          xnekm=xkmo
          ynekm=ykmo
          call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &               xsekm,ysekm,xkmo,ykmo,idum,c4dum)
          xsekm=xkmo
          ysekm=ykmo

c ---     Convert corner coordinates from km to meters
          xsw = xswkm*1000.
          ysw = yswkm*1000.
          xnw = xnwkm*1000.
          ynw = ynwkm*1000.
          xne = xnekm*1000.
          yne = ynekm*1000.
          xse = xsekm*1000.
          yse = ysekm*1000.

        endif

c ---   Report coordinates of corners of sheet
        write(iolst,*)
        write(iolst,*) 'Corners of Sheet (km) in Ouput Projection:'
        write(iolst,*) 'SW (x,y) = ',xswkm,yswkm
        write(iolst,*) 'NW (x,y) = ',xnwkm,ynwkm
        write(iolst,*) 'NE (x,y) = ',xnekm,ynekm
        write(iolst,*) 'SE (x,y) = ',xsekm,ysekm
        write(iolst,*) 'DATUM    = ',datum

c ---   Check if the terrain data in file overlaps the grid area
        maxxs=MAX(xne,xse)
        minxs=MIN(xnw,xsw)
        maxys=MAX(yne,ynw)
        minys=MIN(yse,ysw)
        if(max30x .LT. minxs .OR. min30x .GT. maxxs) then
          write(iolst,*)'RDUSGSHD: ',
     &                  '7.5 minute Data file NOT in designated grid'
          lnotgrid=.TRUE.
        elseif(max30y .LT. minys .OR. min30y .GT. maxys) then
          write(iolst,*)'RDUSGSHD: ',
     &                  '7.5 minute Data file NOT in designated grid'
          lnotgrid=.TRUE.
        endif
        if(lnotgrid) notused = notused + 1

      else


c -------------------------------------------
c ---   *** USGS lat/lon degree-based DEM ***
c -------------------------------------------
c ---   Make sure that the data are 1-deg DEM data in standard format
c ---   The following check is not needed (1/99)
c ***   if(icode(1) .NE. 3) then
c ***      write(iolst,*) 'WARNING:  Not a Level 3 DEM: icode(1) = ',
c ***&                   icode(1)
c ***   endif
        if(icode(1) .EQ. 2) then
          write(iolst,*) 'This is a DEM-2 file: icode(1) = ', icode(1)
        endif
        if(icode(3) .NE. 0) then
           write(iolst,*) 'WARNING:  Not a lat\lon system: icode(3) = ',
     &                    icode(3)
        endif
        if(icode(4) .NE. 0) then
          write(iolst,*) 'WARNING:  Code defining zone not 0: icode(4)',
     &                ' = ', icode(4)
        endif
        if(ixunit .NE. 3) then
          write(iolst,*) 'WARNING: Coordinates not arc-seconds: ixunit',
     &                    ' = ',ixunit
        endif

c ---   Assign data to internal variables
c ---   SE Corner of Quad, Convert from arc-seconds to degrees
c ---   Make Western Hemisphere Longitude positive
c ---   Assign into single precision variables
c ---   (assign integer lat/lon using NINT)
c ---   Check for dateline crossing, and reset + east lons to the
c       - east lons corresponding
          if(lbranch) then
            do icornn=1,7,2
              if(qcornr(icornn).gt.0.d0)
     *           qcornr(icornn)=qcornr(icornn)-1.296d6
            enddo
          endif
        lonsedb=-NINT(qcornr(7)/3600.)
        latsedb=NINT(qcornr(8)/3600.)
        xsw=-qcornr(1)/3600.
        ysw=qcornr(2)/3600.
        xnw=-qcornr(3)/3600.
        ynw=qcornr(4)/3600.
        xne=-qcornr(5)/3600.
        yne=qcornr(6)/3600.
        xse=-qcornr(7)/3600.
        yse=qcornr(8)/3600.
          cnrlonse=xse
          cnrlatse=yse
          cnrlonsw=xsw
          cnrlatsw=ysw
          cnrlonne=xne
          cnrlatne=yne
          cnrlonnw=xnw
          cnrlatnw=ynw

c ---   Strip information (number of profiles and points/profile)
        nstr=nprof(2)
        npt=nprof(2)
        stepb=1./FLOAT(nstr-1)
        stepa=1./FLOAT(npt-1)
cc ---   3-secstep = 1200 steps/degree  (use RDELTA info)
c        stepa = rdelta(2)/3600.
c        stepb = rdelta(1)/3600.
cc ---   Change made on 3/18/02 - cec - different step for UTM-x and UTM-y
cc       stepb and nstr are defined here. stepa and npt will be defined
cc       in the subroutine LOAD.
cc ---   (DEM-2 format)
c        stepb = rdelta(1)/(3*(nprof(2)-1))
cc ---   Strip information (profiles)
c        nstr=nprof(2)
c        npt = 1+NINT(1./stepa)

c ---   Units logical
        if(izunit .EQ. 1) then
          lfeet=.TRUE.
        else
          lfeet=.FALSE.
        endif
c ---   Set variables so that 1 sheet is processed at a time
        nxdb = 1
        nydb = 1
c ---   Report corners of sheet
        write(iolst,*)
        write(iolst,*) 'Corners of 3-sec (1-deg DEM) Sheet:'
        write(iolst,*) 'SW (W.lon,N.lat) = ',xsw,ysw
        write(iolst,*) 'NW (W.lon,N.lat) = ',xnw,ynw
        write(iolst,*) 'NE (W.lon,N.lat) = ',xne,yne
        write(iolst,*) 'SE (W.lon,N.lat) = ',xse,yse

c ---   Set the datum string
        dbdatum=dusgs90
        write(iolst,*) 'DBDATUM = ',dbdatum

c ---   Check if the terrain data in file overlaps the grid area
        maxxs=MAX(xnw,xsw)
        minxs=MIN(xne,xse)
        maxys=MAX(yne,ynw)
        minys=MIN(yse,ysw)
        if(maxx .LT. minxs .OR. minx .GT. maxxs) then
          write(iolst,*)'RDUSGSHD: 90m Data file NOT in designated grid'
          lnotgrid=.TRUE.
        elseif(maxy .LT. minys .OR. miny .GT. maxys) then
          write(iolst,*)'RDUSGSHD: 90m Data file NOT in designated grid'
          lnotgrid=.TRUE.
        endif
        if(lnotgrid) notused = notused + 1
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine corners(alat,alon)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 051201           CORNERS
c               D. Strimaitis, SRC
c
c PURPOSE:     CORNERS provides the coordinates of the 4 corners of
c              the DB sheet, in the cartesian coordinates used to
c              specify the output grid.
c
c Updates:
c --- Ver 3.67 Level 050815 to Ver 3.68 Level 051201   KAM
c              Add additional dummy arguments in calls to SETTRANO to
c              be consistent with CTGPROC V. 2.65 Lev. 051201
c --- Ver 3.61 Level 031031 to 041013 
c              Correction to output UTM zone in call to SETTRANO
c --- Ver 3.5  Level 021018 to 031031       KAM
c              Replace calls to GLOBE1 with call to SETTRANO
c --- Ver 3.3  Level 021018 to 030402       DGS
c              Add false easting/northing to GLOBE1 calls
c --- Ver 3.2  Level 020730 to 021018       DGS
c              - Use full COORDS (GLOBE1, GLOBE)
c --- Ver 3.1  Level 020513 to Level 020730    - D. Strimaitis
c              - Revised the 'DEM-2' (~90m ; 3 arc-sec) change
c                to work with other datasets
c --- Ver 3.0  Level 001207 to Level 020513    - D. Strimaitis
c              - Made the 'DEM-2' (~90m ; 3 arc-sec) code applicable
c                to the standard DEM processing (retired DEM-2)
c --- Ver 2.1  Level 931201 to Level 001207    - S. Du
c              - Add support for DEM-2 (~90m) data 
c
c
c ARGUMENTS:
c    PASSED:  alat      latitude of the SE corner of the sheet
c             alon      West longitude of the SE corner of the sheet
c             /CONTROL/ program control data
c             /DBINF/   input DB file datum (DBDATUM)
c             /GRID/    ouput grid specifications
c             /SEC3CNR/ lat/lon of the 4 corners of the DEM-2 sheet
c
c  RETURNED:  /CORNR/   corners of sheet of terrain data (Cartesian C)
c                       in meters
c
c
c CALLING ROUTINES:   SETOTHER
c
c EXTERNAL ROUTINES:  SETTRANO, GLOBE
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'cornr.trl'
      include 'grid.trl'
      include 'dbinf.trl'

      real*8 vecti(9),vecto(9)
      character*12 caction
      character*4 c4dum

      common /SEC3CNR/ cnrlatse,cnrlonse,cnrlatne,cnrlonne,
     &                 cnrlatsw,cnrlonsw,cnrlatnw,cnrlonnw

      data idum/0/, xdum/0.0/

c --- Set Scale Factor of Tangential TM projection (1.0)
      tmsone=1.00000

c --- Set input dataset false Easting/Northing to zero
      feasti=0.0
      fnorti=0.0

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

c --- Assign N.lat/E.lon at the corners
      if(L3SEC) then
         rlatse=cnrlatse
         rlonse=-cnrlonse
         rlatne=cnrlatne
         rlonne=-cnrlonne
         rlatsw=cnrlatsw
         rlonsw=-cnrlonsw
         rlatnw=cnrlatnw
         rlonnw=-cnrlonnw
      else
         rlatse=alat
         rlonse=-alon
         rlatne=alat+1.
         rlonne=-alon
         rlatsw=alat
         rlonsw=-alon-1.
         rlatnw=alat+1.
         rlonnw=-alon-1.
      endif

c --- Set translation vectors
      call SETTRANO('LL      ',idum,xdum,xdum,xdum,xdum,
     &              feasti,fnorti,
     &              iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &              feast,fnorth,
     &              caction,vecti,vecto)

c --- Datum for Grid coordinates is /GRID/datum
c --- Datum for DB coordinates is /DBINF/dbdatum

c --- Map corners of degree square
      call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &           rlonse,rlatse,x,y,idum,c4dum)
      xse=x*1000.0
      yse=y*1000.0
      call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &           rlonne,rlatne,x,y,idum,c4dum)
      xne=x*1000.0
      yne=y*1000.0
      call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &           rlonnw,rlatnw,x,y,idum,c4dum)
      xnw=x*1000.0
      ynw=y*1000.0
      call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &           rlonsw,rlatsw,x,y,idum,c4dum)
      xsw=x*1000.0
      ysw=y*1000.0

c --- Report corners to list file
      write(iolst,*)
      write(iolst,*)'CORNERS (m) of DB sheet'
      write(iolst,*)'xsw,ysw = ',xsw,ysw
      write(iolst,*)'xnw,ynw = ',xnw,ynw
      write(iolst,*)'xne,yne = ',xne,yne
      write(iolst,*)'xse,yse = ',xse,yse
      write(iolst,*)'For SE corner Nlat,Wlon = ',alat,alon
      write(iolst,*)
 
      return
      end
c-----------------------------------------------------------------------
      subroutine loaddmdf
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 061113          LOADDMDF 
c               K. Morrison, Earth Tech
c
c
c PURPOSE:     LOADDMDF reads the terrain data from the Canadian DMDF
c              data base, and passes to SAVEELEV the
c              intermediate data for the discrete receptors and grid
c              cells to which they apply.
c
c --- Update
c     Ver 3.68 Level 051201 to Ver 3.683 Level 061113   DGS
c              Output UTM zone value passed into GLOBE1 when
c              processing the Canadian DMDF dataset should be signed
c              integer, where negatives denote the S. hemisphere
c              (since this dataset format is for the N. hemisphere,
c              this change should have no effect)
c     Ver 3.67 Level 031031 to Ver 3.68 Level 051201   KAM
c              Add additional dummy arguments in calls to SETTRANO to
c              be consistent with CTGPROC V. 2.65 Lev. 051201
c
c
c --- INPUTS  /CONTROL/ program control data
c             /CORNR/   corners of sheet of terrain data
c             /DBINF/   terrain data base information
c             /GRID/    output grid specification
c
c  RETURNED:  NONE (data passed to SAVEELEV)
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  SETTRANO, GLOBE, SAVEELEV
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'grid.trl'

c --- Local variables
      real*8 vecti11(9),vecto11(9),vecti12(9),vecto12(9)
      character*12 caction
      character*4 c4dum
      character*80 acnddmdf
      character*2 aoldtype
      real feasti/0./,fnorti/0./,tmsone/1.0/

      data xdum/0.0/

c --- For Canadian DMDF terrain data, delx = del = 100 m
      delx = 100.
      dely = 100.
c --- For iscpolar
      dels = (sqrt(delx*delx + dely*dely))*0.5
c --- non-iscpolar
      if(.NOT.LPOLR) dels = dels*scale

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

c --- Set translation vectors to the output grid
      iutm11=11
      iutm12=12
      call SETTRANO('UTM     ',iutm11,xdum,xdum,xdum,xdum,
     &             feasti,fnorti,
     &             iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &             feast,fnorth,
     &             caction,vecti11,vecto11)
      call SETTRANO('UTM     ',iutm12,xdum,xdum,xdum,xdum,
     &             feasti,fnorti,
     &             iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &             feast,fnorth,
     &             caction,vecti12,vecto12)
c
c --- Processing loop
c
      do
        npoint=0
        read(ioinp,fmt='(a80)',end=599) acnddmdf
c ---     
c ---   '07' is the header 
        if(acnddmdf(1:2).eq.'07') then
          write(*,*) 'Reading Canadian DMDF data'
          aoldtype='07'
          cycle

c ---   '01' is a single point
        elseif(acnddmdf(1:2).eq.'01') then
c ---     get the point
          read(acnddmdf(15:18),fmt='(i4)') icndx1
          read(acnddmdf(19:24),fmt='(i6)') icndx2
          read(acnddmdf(26:29),fmt='(i4)') icndy1
          read(acnddmdf(30:35),fmt='(i6)') icndy2
          read(acnddmdf(37:44),fmt='(i8)') icndz
          x=icndx1*1000.+icndx2/1000.
          y=icndy1*1000.+icndy2/1000.
          z=icndz/1000.
          npoint = 1
          aoldtype='01'

c ---   '02' is a line string: each line contains 2 locations.
        elseif(acnddmdf(1:2).eq.'02') then
c ---     get first point
          read(acnddmdf(15:18),fmt='(i4)') icndx1
          read(acnddmdf(19:24),fmt='(i6)') icndx2
          read(acnddmdf(26:29),fmt='(i4)') icndy1
          read(acnddmdf(30:35),fmt='(i6)') icndy2
          read(acnddmdf(37:44),fmt='(i8)') icndz
          x=icndx1*1000.+icndx2/1000.
          y=icndy1*1000.+icndy2/1000.
          z=icndz/1000.
c ---     next point
          read(acnddmdf(46:49),fmt='(i4)') icndx1
          read(acnddmdf(50:55),fmt='(i6)') icndx2
          read(acnddmdf(57:60),fmt='(i4)') icndy1
          read(acnddmdf(61:66),fmt='(i6)') icndy2
          read(acnddmdf(68:75),fmt='(i8)') icndz
          xpoint2 = icndx1*1000.+icndx2/1000.
          ypoint2 = icndy1*1000.+icndy2/1000.
          zpoint2 = icndz/1000.
          npoint = 2
          aoldtype='02'

c ---   '03' is a curve: each line contains 2 locations.
        elseif(acnddmdf(1:2).eq.'03') then
c ---     get first point
          read(acnddmdf(15:18),fmt='(i4)') icndx1
          read(acnddmdf(19:24),fmt='(i6)') icndx2
          read(acnddmdf(26:29),fmt='(i4)') icndy1
          read(acnddmdf(30:35),fmt='(i6)') icndy2
          read(acnddmdf(37:44),fmt='(i8)') icndz
          x=icndx1*1000.+icndx2/1000.
          y=icndy1*1000.+icndy2/1000.
          z=icndz/1000.
c ---     next point
          read(acnddmdf(46:49),fmt='(i4)') icndx1
          read(acnddmdf(50:55),fmt='(i6)') icndx2
          read(acnddmdf(57:60),fmt='(i4)') icndy1
          read(acnddmdf(61:66),fmt='(i6)') icndy2
          read(acnddmdf(68:75),fmt='(i8)') icndz
          xpoint2 = icndx1*1000.+icndx2/1000.
          ypoint2 = icndy1*1000.+icndy2/1000.
          zpoint2 = icndz/1000.
          npoint = 2
          aoldtype='03'

c ---   '06' is textual map feature, skip this line
        elseif(acnddmdf(1:2).eq.'06') then
          aoldtype='06'

c ---   '00' is a continuation: each line can have either 1 or 2 locations.
        elseif(acnddmdf(1:2).eq.'00') then
          if(aoldtype.ne.'06') then
c ---       get first point
            read(acnddmdf(15:18),fmt='(i4)') icndx1
            read(acnddmdf(19:24),fmt='(i6)') icndx2
            read(acnddmdf(26:29),fmt='(i4)') icndy1
            read(acnddmdf(30:35),fmt='(i6)') icndy2
            read(acnddmdf(37:44),fmt='(i8)') icndz
            x=icndx1*1000.+icndx2/1000.
            y=icndy1*1000.+icndy2/1000.
            z=icndz/1000.
            npoint = 1
c ---       next point
            if(acnddmdf(46:55).ne.'          ') then
              read(acnddmdf(46:49),fmt='(i4)') icndx1
              read(acnddmdf(50:55),fmt='(i6)') icndx2
              read(acnddmdf(57:60),fmt='(i4)') icndy1
              read(acnddmdf(61:66),fmt='(i6)') icndy2
              read(acnddmdf(68:75),fmt='(i8)') icndz
              xpoint2 = icndx1*1000.+icndx2/1000.
              ypoint2 = icndy1*1000.+icndy2/1000.
              zpoint2 = icndz/1000.
              npoint = 2
            endif
          endif

c ---   '99' is the last line of the data file: no other info useful.
        elseif(acnddmdf(1:2).eq.'99') then
          write(*,*) 'End of reading Canadian DMDF file'
          exit
        endif          

c ---   Process the point(s)
        do i=1,npoint

c DGS     Set local DB UTM zone, and adjust false easting if zone 12
          kzone=11
          if(x.GT.1000000.) kzone=12
c ---     As 1000000 meter is added to UTM x in zone 12, this shift should 
c ---     be deducted from x so that x is the true UTM X 
          if (kzone .eq. 12) x = x - 1000000.

c ---     Transform to grid datum and projection if needed
          if(dbdatum.NE.datum .OR. .not.LUTM .OR. 
     &        (LUTM .AND. kzone.NE.izone)) then
            xkm=x*0.001
            ykm=y*0.001
            if(kzone.EQ.11) then
              call GLOBE(iolst,caction,dbdatum,vecti11,datum,vecto11,
     &                 xkm,ykm,xkmo,ykmo,idum,c4dum)
            else
              call GLOBE(iolst,caction,dbdatum,vecti12,datum,vecto12,
     &                 xkm,ykm,xkmo,ykmo,idum,c4dum)
            endif
            x=xkmo*1000.
            y=ykmo*1000.
          endif

c ---     Save discrete and cell data
          call saveelev(x,y,dels,z)

c ---     Process 2nd point where needed
          if(npoint.eq.2) then
            x=xpoint2
            y=ypoint2
            z=zpoint2
          endif
        enddo

c --- End of processing of current line
      enddo


c --- Finished the file
599   return
      end
c-----------------------------------------------------------------------
      subroutine loadnzgnr
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 061113         LOADNZGNR
c               K. Morrison, Earth Tech
c
c
c PURPOSE:     LOADNZGNR reads the terrain data from the New Zealand
c              generic data base, and passes to SAVEELEV the
c              intermediate data for the discrete receptors and grid
c              cells to which they apply.
c
c --- Update
c --- Ver 3.68 Level 051201 to Ver 3.683 Level 061113   DGS
c              Output UTM map projection zone is not assigned
c              (set iutm from the unsigned izone variable)
c --- Ver. 3.67 Level 050817 to Ver 3.68 Level 051201   KAM
c              Add additional dummy arguments in calls to SETTRANO to
c              be consistent with CTGPROC V. 2.65 Lev. 051201
c --- Ver. 3.65 Level 050815 to Ver. 3.66 Level 050817   KAM
c              Remove compiler warning about IJUNK not being used
c --- Ver. 3.63 Level 041013 to Ver. 3.65 Level 050815   KAM
c              Add check for dateline crossing
c --- Ver 3.61 Level 031031 to 041013
c              Correction to output UTM zone in call to SETTRANO
c
c
c --- INPUTS  /CONTROL/ program control data
c             /CORNR/   corners of sheet of terrain data
c             /DBINF/   terrain data base information
c             /GRID/    output grid specification
c
c  RETURNED:  NONE (data passed to SAVEELEV)
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  SETTRANO, GLOBE, SAVEELEV
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'grid.trl'

c --- Local variables
      real*8 vecti(9),vecto(9)
      character*12 caction
      character*4 c4dum
      real feasti/0./,fnorti/0./,tmsone/1.0/

      data idum/0/, xdum/0.0/

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

c --- Set translation vectors to the output grid
      call SETTRANO('LL      ',idum,xdum,xdum,xdum,xdum,
     &              feasti,fnorti,
     &              iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &              feast,fnorth,
     &              caction,vecti,vecto)

c --- Calculate dels from the first two data points
      read(ioinp,*,end=7990) ijunk,z,alon,alat
c --- check for dateline crossing, and if so, make sure alon is positive
      if(lbranch.and.alon.lt.0.) alon=alon+360.
      call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &           alon,alat,xkmo,ykmo,idum,c4dum)
      x = xkmo*1000.
      y = ykmo*1000.
      xold = x
      yold = y
      read(ioinp,*,end=7990) ijunk,z,alon,alat
c --- check for dateline crossing, and if so, make sure alon is positive
      if(lbranch.and.alon.lt.0.) alon=alon+360.
      call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &           alon,alat,xkmo,ykmo,idum,c4dum)
      x = xkmo*1000.
      y = ykmo*1000.
      dels = 0.5*sqrt((x-xold)**2+(y-yold)**2)
c --- non-iscpolar
      if(.NOT.LPOLR) dels = dels*scale
      backspace ioinp
      backspace ioinp                  
 7010 continue
      read(ioinp,*,end=7990) ijunk,z,alon,alat
c --- check for dateline crossing, and if so, make sure alon is positive
      if(lbranch.and.alon.lt.0.) alon=alon+360.
      call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &           alon,alat,xkmo,ykmo,idum,c4dum)
      x = xkmo*1000.
      y = ykmo*1000.

c --- Save discrete and cell data
      call saveelev(x,y,dels,z)

      goto 7010
 7990 continue

c --- Remove compiler warning about ijunk
      ijunk=ijunk

      return
      end
c-----------------------------------------------------------------------
      subroutine loadgnr
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 051201           LOADGNR
c               K. Morrison, Earth Tech
c
c
c PURPOSE:     LOADGNR reads the terrain data from the generic data
c              base, and passes to SAVEELEV the intermediate data for
c              the discrete receptors and grid cells to which they
c              apply.
c
c --- Update
c --- Ver 3.67 Level 050817 to Ver 3.68 Level 051201   KAM
c              Add additional dummy arguments in calls to SETTRANO to
c              be consistent with CTGPROC V. 2.65 Lev. 051201
c --- Ver 3.65 Level 050815 to Ver. 3.66 Level 050817
c              Change ZGNR in first 4 reads to Z to remove compiler warning
c --- Ver 3.63 Level 041013 to Ver. 3.65 Level 050815
c              Correct UTM zone test from type 3 to type 2
c              Add check for domain crossing the dateline
c --- Ver 3.61 Level 031031 to 041013
c              Correction to output UTM zone in call to SETTRANO
c
c
c --- INPUTS  /CONTROL/ program control data
c             /CORNR/   corners of sheet of terrain data
c             /DBINF/   terrain data base information
c             /GRID/    output grid specification
c
c  RETURNED:  NONE (data passed to SAVEELEV)
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  SETTRANO, GLOBE, SAVEELEV
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'grid.trl'

c --- Local variables
      real*8 vecti(9),vecto(9)
      character*12 caction
      character*4 c4dum
      logical lconvert
      real feasti/0./,fnorti/0./,tmsone/1.0/

      data idum/0/, xdum/0.0/

c --- HEADER RECORD ---
c --- ignrtype: Type of (x,y) coordinates:
c ---    ignrtype = 1: UTM in meter
c ---    ignrtype = 2: UTM in km
c ---    ignrtype = 3: NLat,ELong,Zelev
c --- ignrunit: Unit of elevation:
c ---    ignrunit = 1: meter
c ---    ignrunit = 2: foot
c --- izoneipt: UTM zone used in the input data file
c ---    neglected if Lat_Long is used
c --------------------------------------------------------------------
      read(ioinp,*,end=799) ignrtype,ignrunit,izoneipt
      if(ignrtype.lt.1.or.ignrtype.gt.3) then
        write(*,*) 'Error in the header:'
        write(*,*) 'Input type must be 1,2 or 3'
        write(*,*) 'ignrtype = 1: UTM in meter'
        write(*,*) 'ignrtype = 2: UTM in km'
        write(*,*) 'ignrtype = 3: NLat,ELong'
        stop
      endif
      if(ignrunit.ne.1.and.ignrunit.ne.2) then
        write(*,*) 'Error in the header:'
        write(*,*) 'Elevation unit must be 1 or 2'
        write(*,*) 'ignrunit = 1: meter'
        write(*,*) 'ignrunit = 2: foot'
        stop
      endif
        if(ignrtype.eq.1.or.ignrtype.eq.2) then
        if(izoneipt.lt.1.or.izoneipt.gt.60)then
          write(*,*) 'Error in the header:'
          write(*,*) 'UTM zone must be in the range of (1,60)'
          stop
        endif
      endif

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

c --- Set translation vectors to the output grid
      lconvert=.TRUE.
      if(ignrtype.EQ.3) then
         call SETTRANO('LL      ',idum,xdum,xdum,xdum,xdum,
     &               feasti,fnorti,
     &               iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
      else
         call SETTRANO('UTM     ',izoneipt,xdum,xdum,xdum,xdum,
     &               feasti,fnorti,
     &               iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)
         if(izone.EQ.izoneipt) lconvert=.FALSE.
      endif
      if(dbdatum.NE.datum) lconvert=.TRUE.
      
c --- Calculate dels from the first two data points
      if(ignrtype.EQ.3) then 
           read(ioinp,*,end=799) y_lat,x_lon,z
c ---      Check for dateline crossing, and if so, make sure x_lon
c ---      is positive
           if(lbranch.and.x_lon.lt.0.) x_lon=x_lon+360.
      else
           read(ioinp,*,end=799) x_lon,y_lat,z
      endif
      if(LCONVERT) then
         if(ignrtype.eq.1) then
c ---       Convert m to km
            x_lon = x_lon*0.001
            y_lat = y_lat*0.001
         endif
         call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &              x_lon,y_lat,xkmo,ykmo,idum,c4dum)
         x = xkmo*1000.
         y = ykmo*1000.
      elseif(ignrtype.eq.1) then
         x = x_lon
         y = y_lat
      elseif(ignrtype.eq.2) then
         x = x_lon*1000.
         y = y_lat*1000.
      else
         stop 'ERROR in LOADGNR:  Bad Generic Conversion'
      endif
      xold = x
      yold = y

      if(ignrtype.EQ.3) then
           read(ioinp,*,end=799) y_lat,x_lon,z
c ---      Check for dateline crossing, and if so, make sure x_lon
c ---      is positive
           if(lbranch.and.x_lon.lt.0.) x_lon=x_lon+360.
      else
           read(ioinp,*,end=799) x_lon,y_lat,z
      endif
      if(LCONVERT) then
         if(ignrtype.eq.1) then
c---        Convert m to km
            x_lon = x_lon*0.001
            y_lat = y_lat*0.001
         endif
         call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &              x_lon,y_lat,xkmo,ykmo,idum,c4dum)
         x = xkmo*1000.
         y = ykmo*1000.
      elseif(ignrtype.eq.1) then
         x = x_lon
         y = y_lat
      elseif(ignrtype.eq.2) then
         x = x_lon*1000.
         y = y_lat*1000.
      else
         stop 'ERROR in LOADGNR:  Bad Generic Conversion'
      endif

      dels = 0.5*sqrt((x-xold)**2+(y-yold)**2)
c --- non-iscpolar
      if(.NOT.LPOLR) dels = dels*scale

      backspace ioinp
      backspace ioinp                  

c --- Loop over records
 701  continue
      if(ignrtype.EQ.3) then 
         read(ioinp,*,end=799) y_lat,x_lon,z
c ---      Check for dateline crossing, and if so, make sure x_lon
c ---      is positive
           if(lbranch.and.x_lon.lt.0.) x_lon=x_lon+360.
      else
         read(ioinp,*,end=799) x_lon,y_lat,z
      endif
      if(ignrunit.eq.2) z = z*0.3048
      if(LCONVERT) then
         if(ignrtype.eq.1) then
c ---       Convert m to km
            x_lon = x_lon*0.001
            y_lat = y_lat*0.001
         endif
         call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &              x_lon,y_lat,xkmo,ykmo,idum,c4dum)
         x = xkmo*1000.
         y = ykmo*1000.
      elseif(ignrtype.eq.1) then
         x = x_lon
         y = y_lat
      elseif(ignrtype.eq.2) then
         x = x_lon*1000.
         y = y_lat*1000.
      else
         stop 'ERROR in LOADGNR:  Bad Generic Conversion'
      endif

c --- Save discrete and cell data
      call saveelev(x,y,dels,z)

      goto 701
 799  continue

      return
      end

c-----------------------------------------------------------------------
      subroutine loadlaza
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 051201          LOADLAZA 
c               K. Morrison, Earth Tech
c
c
c PURPOSE:     LOADLAZA reads the terrain data from the Lambert Azimuthal
c              Global data base, and passes to SAVEELEV the
c              intermediate data for the discrete receptors and grid
c              cells to which they apply.
c
c --- Update
c --- Ver 3.67 Level 050817 to Ver 3.68 Level 051201   KAM
c              Add additional dummy arguments in calls to SETTRANO to
c              be consistent with CTGPROC V. 2.65 Lev. 051201
c --- Ver 3.65 Level 050815 to Ver 3.66 Level 050817
c              Comment out references to XXX and YYY (not used)
c --- Ver 3.63 Level 041013 to Ver 3.65 Level 050815
c              Constrain the main loop to only cover actual points
c              (facilitates debugging)
c --- Ver 3.61 Level 031031 to 041013
c              Correction to output UTM zone in call to SETTRANO
c
c
c --- INPUTS  /CONTROL/ program control data
c             /CORNR/   corners of sheet of terrain data
c             /DBINF/   terrain data base information
c             /GRID/    output grid specification
c
c  RETURNED:  NONE (data passed to SAVEELEV)
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  SETTRANO, GLOBE, SAVEELEV
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'grid.trl'

      common /GCORNR/ nbegx,nendx,nbegy,nendy
c
      common /USGSLA/ nxi,nyi,dxi,dyi,xorgi,yorgi,
     &                rlati,rloni,nxoff,nyoff

c --- Local variables
      real*8 vecti(9),vecto(9)
      character*12 caction
      character*4 c4dum
      character*1 ibuf,jbuf
      real feasti/0./,fnorti/0./,tmsone/1.0/

      data idum/0/, xdum/0.0/

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

c --- Set translation vectors to the output grid
      call SETTRANO('LAZA    ',idum,rlati,rloni,xdum,xdum,
     &             feasti,fnorti,
     &             iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &             feast,fnorth,
     &             caction,vecti,vecto)

 2    continue
      mxy = nxi*nyi

c --- determine absolute spacing, in meters divided by 2
      dels = (SQRT(dxi*dxi + dyi*dyi)*1000.)*0.5

c --- non-iscpolar
      if(.NOT.LPOLR) dels = dels*scale

      OUTER: Do j = nbegy,nendy
          if(j.lt.1.or.j.gt.nyi) cycle OUTER
         yy = yorgi + dyi*j
         INNER: Do i = nbegx,nendx
             if(i.lt.1.or.i.gt.nxi) cycle INNER
c
c --- estimate the geodesiacs
            xx = xorgi + dxi*i
            call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &                 xx,yy,xdist,ydist,idum,c4dum)
            x = xdist*1000
            y = ydist*1000
c (not used)    xxx = (xdist - xorgk)/dxi
c (not used)    yyy = (ydist - yorgk)/dyi
c (not used)    ix = int(xxx + 0.5) + nxoff
c (not used)    iy = int(yyy + 0.5) + nyoff
            kcnt = mxy -((j-1)*nxi + nxi - i + 1) + 1
            jcnt = kcnt*2 - 1
            jcntp1 = jcnt + 1
            Read(ioinp,rec=jcnt)ibuf
            Read(ioinp,rec=jcntp1)jbuf
            nrec = nrec + 1
            k1 = ichar(ibuf)
            k2 = ichar(jbuf)

c ---       z is the terrain elevation in meters
            z = 1.0*(k2 + 255*k1)

c ---       Save discrete and cell data
            call saveelev(x,y,dels,z)

         Enddo INNER
      Enddo OUTER

      return
      end
c-----------------------------------------------------------------------
      subroutine loadother
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 051201         LOADOTHER
c               K. Morrison, Earth Tech
c
c
c PURPOSE:     LOADOTHER reads the terrain data from the USGS 7.5 min,
c              USGS 30-sec, ARM3, Rocky Mountain 3-sec, GTOPO30 and SRTM
c              database files, and passes to SAVEELEV the intermediate
c              data for the discrete receptors and grid cells to which
c              they apply.
c
c --- Update
c --- Ver 3.67 Level 041013 to Ver 3.68 Level 051201   KAM
c              Add additional dummy arguments in calls to SETTRANO to
c              be consistent with CTGPROC V. 2.65 Lev. 051201
c --- Ver 3.61 Level 031031 to 041013 
c              Correction to output UTM zone in call to SETTRANO
c
c
c --- INPUTS  /CONTROL/ program control data
c             /CORNR/   corners of sheet of terrain data
c             /DBINF/   terrain data base information
c             /GRID/    output grid specification
c             /GSPAN/   range in grid coordinates
c
c  RETURNED:  NONE (data passed to SAVEELEV)
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  SETTRANO, CORNERS, GLOBE, LOADSTRP, SAVEELEV
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'dbinf.trl'
      include 'grid.trl'
      include 'gspan.trl'

      real*8 vecti(9),vecto(9)
      character*12 caction
      character*4 c4dum
      real rdata(mxnp),feasti/0./,fnorti/0./,tmsone/1.0/

      data xdum/0.0/

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

      if(L7_5MIN) then
c ---    Set (quad map) translation vectors to the output grid
         call SETTRANO('UTM     ',icode(4),xdum,xdum,xdum,xdum,
     &                 feasti,fnorti,
     &                 iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &                 feast,fnorth,
     &                 caction,vecti,vecto)
      endif

c --- Loop over all sheets required to cover grid
      do 400 jj=1,nydb
        do 300 ii=1,nxdb

c ---    Calculate the DB index
         lon = ii - 1 + lonsedb
         lat = jj - 1 + latsedb
         ndx = (jj-1)*nxdb + ii
c
c ==========================================================================
c        For GTOPO30 data, each file typically covers 2000 degree-squares.
c        Hence, it is extremely inefficient to loop over each degree-square
c        Furthermore, doing UTM conversions over such a large area produces
c        unpredictable results.  Therefore, for the sake of model efficiency
c        and accuracy, the following checks are performed to simply skip
c        degree-squares that are too far away from the output grid.
c        This reduces the number of degree-squares that need to be processed
c        from 2000 to less than 10, a speed-up by a factor 200!
c ==========================================================================
c
         if (lgtopo30) then
c
c          Note that LON and LAT are west longitude and north latitude of 
c          the SE corner for the current degree-square
c
           if (lon.le.minx-2 .or. lon.ge.maxx+2 .or.
     &         lat.le.miny-2 .or .lat.ge.maxy+2) 
     &     goto 300
c
         end if

         if(L30SEC) then
c ---       ARM3 1x1 degree sheet where (LON,LAT) is s.e. corner
            ipos = index(ndx)
            if(ipos .EQ. 0) goto 300
         endif

c ---    Get CARTESIAN coordinates (m) of four corners of sheet
c ---    if terrain data are NOT already in UTM's  ---  call CORNERS
         if(.not.L7_5MIN) then
            alon = FLOAT(lon)
            alat = FLOAT(lat)
            call CORNERS(alat,alon)
         endif

c ---    Loop over all strips of data in sheet
         do 200 j=1,nstr

c ---       Load strip and calculate near and far edge grid(m) coord
c ---       (strip goes from near to far edge of sheet)  ---  call LOAD
            call LOADSTRP(j,lat,lon,rdata,xnear,ynear,xfar,yfar)

c ---       Determine spacing between points along strip
            if(.not.L7_5MIN) then
               delx = (xfar-xnear)*stepb
               dely = (yfar-ynear)*stepa
            endif

c ---       Determine absolute spacing, in meters divided by 2
            dels = (SQRT(delx*delx + dely*dely))*0.5
            if(LPOLR) then
c ---          POLAR - spacing in meters
            else
c ---          CARTESIAN - spacing in grid units
               dels = dels*scale
            endif

c ---       Loop over data points in strip (im1=i-1)
            do 100 i=1,npt

c ---          Interpolate UTM/LC coordinates of this point from
c ---          the near edge to the far edge of strip
               im1 = i-1
               x = xnear + delx * im1
               y = ynear + dely * im1
             
               if(L7_5MIN) then
c ---             Translate UTM coordinates to grid
                  xkm=x*0.001
                  ykm=y*0.001
                  call GLOBE(iolst,caction,dbdatum,vecti,datum,vecto,
     &                       xkm,ykm,xkmo,ykmo,idum,c4dum)
                  x=xkmo*1000.
                  y=ykmo*1000.
               endif

c ---          Save discrete and cell data
               call SAVEELEV(x,y,dels,rdata(i))

  100       continue
  200    continue
         write(*,*) 'Finished Sheet # ',ii
  300   continue
        write(*,*) 'Finished Sheets in Panel # ',jj
  400 continue

      return
      end
c-----------------------------------------------------------------------
      subroutine loadstrp(j,lat,lon,rdata,xnear,ynear,xfar,yfar)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050817          LOADSTRP
c               D. Strimaitis, SRC
c               J. Chang, Earth Tech
c
c
c PURPOSE:     LOADSTRP controls reading the terrain data from the data
c              base, and setting interpolation factors for calculating
c              the grid cell to which it applies.
c
c --- Update
c     Ver 3.65 Level 050815 to Ver 3.66 Level 050817   KAM
c              Remove LAT LON from call to LOADSRTM - not used
c              Remove compiler warning about ELMNMX
c     Ver 3.63 Level 040920 to Ver 3.65 Level 050815   KAM
c              Add check for dateline crossing
c              Change final loop to only cover actual points (NPT), not
c              maximum number (MXNP) to avoid undefined values when
c              debugging
c     Ver 3.6  Level 031031 to Level 040920    - D. Strimaitis
c              Set NPTS to the number of points listed on this data
c              record for USGS "3-sec" format
c     Ver 3.5  Level 031017 to 031031       KAM
c              Now called from LOADOTHER instead of from COMP
c     Ver 3.4  Level 021018 to Level 031017    - D. Strimaitis
c              Change to formatted read of DEM data records and
c              recognize the missing data value (-32767)
C              Change name from LOAD to LOADSTRP - K. Morrison
c              Add SRTM processing via LOADSRTM
c     Ver 3.2  Level 020513 to Level 021018    - D. Strimaitis
c              Remove UTM zone translation for L7_5MIN quad
c              (full datum/projection translation done in COMP)
c     Ver 3.0  Level 020318 to Level 020513    - D. Strimaitis
c              Made the 'DEM-2' (~90m ; 3 arc-sec) code applicable
c              to the standard DEM processing (retired DEM-2)
c     Ver 3.0  Level 001207 to 020318       C. Czaja
c              Compute stepa and npt, step and number of points in the
c              strip for USGS90 DEM-2 (~90m; 3 arc-sec)
c     Ver 2.1  Level 000411 to 001207       S Du
c              Add the capability to deal with DEM-2 (~90 m) data
c     Ver 2.1  Level 000320 to 000411       DGS
c              Define independent step size Along and Between strips
c              Change USGS '30m' references to USGS '7_5' for 7.5
c              minute quadrangle files
c     Ver 2.1  Level 990528 to 000320       DGS
c              Add vertical scaling factor ZFAC (value other than 1.0
c              may have been read from header of USGS file)
c
c
c ARGUMENTS:
c    PASSED:  j         index for current strip within sheet
c             lat       northern latitude of the current degree-square
c                       (only used for GTOPO30 and SRTM data)
c             lon       western longitude of the current degree-square
c                       (only used for GTOPO30 and SRTM data)
c             /CONTROL/ program control data
c             /CORNR/   corners of sheet of terrain data
c             /DBINF/   terrain data base information
c             /GRID/    output grid specification
c
c  RETURNED:  rdata     real array of terrain data in strip
c             xnear     x-coordinate of point at start of strip
c             ynear     y-coordinate of point at start of strip
c             xfar      x-coordinate of point at end of strip
c             yfar      y-coordinate of point at end of strip
c
c
c CALLING ROUTINES:   LOADOTHER
c
c EXTERNAL ROUTINES:  LOADSRTM
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'cornr.trl'
      include 'dbinf.trl'
      include 'grid.trl'

c --- Local variables
      integer*2 data(mxnp)
      integer*1 data1(2*mxnp)
      equivalence (data,data1) ! to convert from "big endian" to "little endian,"
c                                when necessary
      real rdata(mxnp)
      data feet2m/0.3048/

c --- Set missing data integer to trap -32767 with some "slop"
      data imiss/-32000/

c --- Local variables for the USGS DEM data format
      integer*2 iprof(2),npoints(2)
      real*8 utm0(2),elbase,elmnmx(2)

c --- Set the position of the current strip
      stepj = (j-1) * stepb

      if(L30SEC) then

c ---    ARM3 1x1 degree sheet   ---
c ---    Data are ordered in strips of constant latitude,
c ---    starting with SE corner.
c ---    Load data for one strip of one sheet into data(np), where the
c ---    strip is oriented east-west (constant latitude), and the first
c ---    point in the strip lies on the eastern boundary of the sheet.
         xnear = xse + (xne-xse) * stepj
         ynear = yse + (yne-yse) * stepj
         xfar  = xsw + (xnw-xsw) * stepj
         yfar  = ysw + (ynw-ysw) * stepj
         read(ioinp,1200) (data(ilon),ilon=1,npt)
1200     format(30i4)

      elseif(L3CD) then

c ---    3-sec 1x1 degree sheet of binary data
c ---    Data are ordered in strips of constant longitude,
c ---    starting with SW corner.
c ---    Load data for one strip of one sheet into data(np), where the
c ---    strip is oriented south-north (constant longitude), and the 1st
c ---    point in the strip lies on the southern boundary of the sheet.
         xnear = xsw + (xse-xsw) * stepj
         ynear = ysw + (yse-ysw) * stepj
         xfar  = xnw + (xne-xnw) * stepj
         yfar  = ynw + (yne-ynw) * stepj
         read(ioinp) (data(ilat),ilat=1,npt)

      elseif(L7_5MIN) then

c ---   7.5 minute USGS quadrangle sheet.
c ---   Data are ordered in strips of constant UTM-Easting,
c ---   starting with SW corner.
c ---   Load data for one strip of one sheet into data(np), where the
c ---   strip is oriented south-north, and the first point in the strip
c ---   lies near the southern boundary of the sheet.

c ---   Read full data record, using formatted read
c ---   First physical record (1024 bytes, has up to 146 heights)
        read(ioinp,1300) iprof,npoints,utm0,elbase,elmnmx,
     1                   (data(iy),iy=1,146)
1300    format(4i6,5d24.15,146i6)
        npt=npoints(1)
c ---   Remove compiler warning
          elmnmx(1)=elmnmx(1)

c ---   Convert from double precision to single precision variable
        xnear = utm0(1)
        ynear = utm0(2)
        xfar=xnear
        yfar=ynear+dely*(npt-1)

c ---   Check strip (profile) number
        if(iprof(2) .NE. j) then
          write(iolst,*) 'LOAD:  Strip (profile) index does not match'
          write(iolst,*) '       j,iprof(2) = ',j,iprof(2)
          write(*,*) 'Error in Subr. LOAD:  See .LST file'
          stop
        endif

c ---   Remaining physical records (up to 170 heights each), if needed
        if(npoints(1).GT.146) then
c ---      Compute the number needed to be read
           nhtleft=npoints(1)-146
           nrcleft=nhtleft/170
           nhttest=146+nrcleft*170
           if(nhttest.LT.npoints(1)) nrcleft=nrcleft+1
c ---      Loop over records
           do irc=1,nrcleft
              iy1=146+170*(irc-1)+1
              iy2=146+170*irc
              iy2=MIN(iy2,npoints(1))
              read(ioinp,'(170i6)') (data(iy),iy=iy1,iy2)
           enddo
        endif

c ---   Add base elevation to heights (skip voids)
        do iy=1,npt
           if(data(iy).GT.imiss) data(iy)=NINT(elbase+FLOAT(data(iy)))
        enddo

      elseif(L3SEC) then

c ---   USGS DEM-2 quadrangle sheet.
c ---   Read the elevation data in this record, using formatted read
c ---   First physical record (1024 bytes, has up to 146 heights)
        read(ioinp,1300) iprof,npoints,utm0,elbase,elmnmx,
     1                   (data(iy),iy=1,146)
c ---     check for dateline crossing, and adjust +lons
          if(lbranch) then
            if(utm0(1).gt.0.d0) utm0(1)=utm0(1)-1.296d6
          endif

c ---   Check strip (profile) number
        if(iprof(2) .NE. j) then
           write(iolst,*) 'LOAD:  Strip (profile) index does not match'
           write(iolst,*) '       j,iprof(2) = ',j,iprof(2)
           write(*,*) 'Error in Subr. LOAD:  See .LST file'
           stop
        endif

c ---   Remaining physical records (up to 170 heights each), if needed
        if(npoints(1).GT.146) then
c ---      Compute the number needed to be read
           nhtleft=npoints(1)-146
           nrcleft=nhtleft/170
           nhttest=146+nrcleft*170
           if(nhttest.LT.npoints(1)) nrcleft=nrcleft+1
c ---      Loop over records
           do irc=1,nrcleft
              iy1=146+170*(irc-1)+1
              iy2=146+170*irc
              iy2=MIN(iy2,npoints(1))
              read(ioinp,'(170i6)') (data(iy),iy=iy1,iy2)
           enddo
        endif

c ---   Add 3/18/02 - c.Czaja - defined stepa and npt from information
c        read here for the step and number of points in this
c        strip.- npoints(1).
        stepa = 1./(npoints(1)-1)
c         npt = 1+nint(1./stepa)
          npt=npoints(1)

c ---   Add base elevation to heights (skip voids)
        do iy=1,npoints(1)
           if(data(iy).GT.imiss) data(iy)=NINT(elbase+FLOAT(data(iy)))
        enddo
c ---   Define start and stop points of strip
        xnear = xsw + (xse-xsw) * stepj
        ynear = ysw + (yse-ysw) * stepj
        xfar  = xnw + (xne-xnw) * stepj
        yfar  = ynw + (yne-ynw) * stepj

      elseif(LGTOPO30) then

c ---    USGS GTOPO30 30 arc-second data 1x1 degree sheet.  Note that
c        Each GTOPO30 file typically covers 50 degress of latitude and
c        40 degrees of longitude.  The 40x50 degree area is then divided 
c        into 2000 degree-squares.  Each degree-square is processed
c        at a time (the do 400 and do 300 loops in the main program), 
c        and each strip (the do 200 loop in the main program) is then
c        processed at a time.  Because the data file are "direct accessed,"
c        the I/O time required should be minimal.
c
c ---    Data are ordered in strips of constant latitude, starting with
c        NW corner.  Load data for one strip of one sheet into data(np),
c        where the strip is oriented east-west (constant latitude), and
c        the first point in the strip lies on the western boundary of
c        the sheet.  Strips are proceeding southward, i.e., in decreasing
c        latitude.
c
c        As described above, each GTOPO30 file is a flat (6000,4800)
c        or (3600,7200) array, where each degree-square occupies a
c        (120,120) patch.  The data are stored in row major order, i.e.,
c        along the same latitude.  Assume that the (western) longitude and
c        latitude of the northwest corner of the GTOPO30 file is 
c        (lonnwdb,latnwdb).  Then, for the (lon,lat) degree-square (referring
c        to the SE corner):
c        the range in i (north-south) should be
c            (latnwdb-lat-1)*120+1 to (latnwdb-lat)*120;
c        and the range in j (west-east) should be
c            (lonnwdb-lon-1)*120+1 to (lonnwdb-lon)*120;
c        Because the data are stored in row major order, (i,j) correspond 
c        to (i-1)*120*(lonnwdb-lonsedb)+j
c        See more below.
c
         xnear = xnw + (xsw-xnw) * stepj
         ynear = ynw + (ysw-ynw) * stepj
         xfar  = xne + (xse-xne) * stepj
         yfar  = yne + (yse-yne) * stepj
c
c ***    Calculate the starting location for the first point in the strip
c        Note that data are accessed 240 bytes at a time.
c
         iii=(latnwdb-lat-1)*nstr+j
         jjj=lonnwdb-lon
         istart1d=(iii-1)*(lonnwdb-lonsedb)+jjj
c          j   is the counter for data strips in current degree-square
c              (from north to south)
c          iii is the counter for data strips in current file
c              (from north to south)
c          jjj is the counter for 240-byte data record in a data strip
c              (from west to east)
c          istart1d is the 1-D counter corresponding to (iii,jjj)
c
         if (.not.lbigendian) then
           read (ioinp,rec=istart1d) (data1(i+1),data1(i),i=1,npt*2,2)
c          To change data from "big endian" to "little endian" by
c          swapping bytes
         else
           read (ioinp,rec=istart1d) (data(i),i=1,npt) ! no need to swap
         end if
c
c ***    Convert elevation for ocean areas from -9999 to 0
c
         do i=1,npt
           if (data(i).eq.-9999) data(i)=0
         end do
c
      elseif(LSRTM1 .or. LSRTM3) then

        call loadsrtm(j,data,data1,xnear,ynear,xfar,yfar)

      endif

c-emi Convert data in strip from integer*2 to real
c-emi also convert from feet to meters if necessary
c --- Apply the vertical scale factor ZFAC
        do i=1,npt
         if(data(i).GT.imiss) then
            rdata(i) = zfac*data(i)
            if(LFEET) rdata(i) = rdata(i) * feet2m
         else
c ---       Pass voids without scaling
            rdata(i) = data(i)
         endif
      enddo

      return
      end
c-----------------------------------------------------------------------
        subroutine loadsrtm(j,data,data1,xnear,ynear,xfar,yfar)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050817          LOADSRTM
c               K. Morrison, Earth Tech
c
c
c PURPOSE:     LOADSRTM controls reading the terrain data from the
c              SRTM data base, and setting interpolation factors for 
c              calculating the grid cell to which it applies.  It uses
c              the 1-byte and 2-byte integer arrays created in LOAD
c              to handle the ENDIAN problem. 
c
c --- Update
c     Ver. 3.4 Level 031017 to Ver. 3.66 Level 050817
c              - Remove LAT LON from argument list - not used
c              - Remove declaration for IMISS - not used
c
c ARGUMENTS:
c    PASSED:  j         index for current strip within sheet
c             /CONTROL/ program control data
c             /CORNR/   corners of sheet of terrain data
c             /DBINF/   terrain data base information
c             /GRID/    output grid specification
c
c  RETURNED:  data      integer array of terrain data in strip
c             xnear     x-coordinate of point at start of strip
c             ynear     y-coordinate of point at start of strip
c             xfar      x-coordinate of point at end of strip
c             yfar      y-coordinate of point at end of strip
c
c
c CALLING ROUTINES:   LOADSTRP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'cornr.trl'
      include 'dbinf.trl'
      include 'grid.trl'

c --- Local variables
        integer*2 data(mxnp)
      integer*1 data1(2*mxnp)

c --- Find coordinates of strip, accounting for first skippped strip
      stepj = (j-1) * stepb
      stepj2 = stepj + stepb
      xnear = xnw + (xsw-xnw) * stepj
      ynear = ynw + (ysw-ynw) * stepj2
      xfar  = xne + (xse-xne) * stepj
      yfar  = yne + (yse-yne) * stepj2

c
c --- Data are ordered in strips of constant latitude, starting with
c     NW corner.  Load data for one strip of the sheet into data(np),
c     where the strip is oriented east-west (constant latitude), and
c     the first point in the strip lies on the western boundary of
c     the sheet.  Strips are proceeding southward, i.e., in decreasing
c     latitude.  Note that an extra 2-byte value is read at the end of
c     the strip, which is the overlap with the next (eastern) sheet. 
c
      if (.not.lbigendian) then
        read (ioinp) (data1(i+1),data1(i),i=1,npt*2,2),data(npt+1)
c       To change data from "big endian" to "little endian" by
c       swapping bytes
      else
        read (ioinp) (data(i),i=1,npt+1) ! no need to swap
      end if
c
      return
      end
c-----------------------------------------------------------------------
      subroutine cellind(x,y,dels,ix,iy,ixp,iyp)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 970505           CELLIND
c               D. Strimaitis, SRC
c
c
c PURPOSE:     CELLIND computes cell index for point x,y and it computes
c              the cell index when the point is perturbed by +/- dels
c
c --- Updates
c     Ver 2.0  Level 940107 to 970505
c                 DGS  - integer truncation returns 0 for -1.0<real<1.0
c                        so that first row/column of cells in the grid
c                        contained data just outside the grid in the
c                        reported average; problem fixed.
c
c
c ARGUMENTS:
c    PASSED:  x,y       coordinates of point (m)
c             dels      spatial perturbation (m for ISC-POLAR,
c                       grid units for CARTESIAN)
c             /CONTROL/ program control logicals
c             /GRID/    output grid information
c
c  RETURNED:  ix,iy     cell index
c             ixp,iyp   perturbed cell index
c
c CALLING ROUTINES:   SAVEELEV
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'grid.trl'

      data rad2deg/57.29578/


c --- Cell Index Section

      if(LPOLR) then

c ---    POLAR grid
c ---    Radial distance is xi(m), angular distance is yi(deg)
         ix = 1
         iy = 1
         xm = x-xorgm
         ym = y-yorgm
         xi = SQRT(xm**2+ym**2)
         do i=1,nx
            if(xi .GE. cbdism(i)) ix=i+1
         enddo
         if(ix .GT. nx) then
c ---       Point lies off polar grid, skip angular index
            iy=ny+1
         else
            yi = ATAN2(xm,ym)*rad2deg
c ---       Shift radial by -ang(1) [cbang are already shifted]
            yi = yi-ang(1)
            if(yi .LT. 0.) yi=yi+360.
            do i=1,ny
               if(yi .GE. cbang(i)) iy=i+1
            enddo
c ---       Account for 0/360 jump in center of shifted sector 1
            if(iy .GT. ny) iy=1
         endif

      else

c ---    CARTESIAN grid
c ---    Coordinates of point are xi,yi in grid units
         xi = (x-xllm)*scale
         yi = (y-yllm)*scale
c DGS    ix = INT(xi) + 1
c DGS    iy = INT(yi) + 1
         ix = INT(xi)
         iy = INT(yi)
         if(xi.GE.0.0) ix=ix+1
         if(yi.GE.0.0) iy=iy+1

      endif


c --- Perturbed Index Section

c --- For PEAK processing, perturb point by "dels" about
c --- (xi,yi) and identify each cell in which any of these point
c --- positions fall.  This perturbation seeks to capture peak
c --- elevations that occur near the boundary of a cell.
      ixp = ix
      iyp = iy
      if(LPEAK) then
         if(LPOLR) then
c ---       POLAR grid
c ---       Check radial distance range
            if(xi+dels .GT. cbdism(ix)) ixp=ix+1
            if(ix .GE. 2) then
               if(xi-dels .LT. cbdism(ix-1)) ixp=ix-1
            endif
c ---       Check angular range (account for 0/360 jump)
            if(iy .EQ. 1) then
               iym1=ny
            else
               iym1=iy-1
            endif
            if(iy .EQ. ny) then
               iyp1=1
            else
               iyp1=iy+1
            endif
            upedge = cbang(iy)-yi
            if(upedge .LT. 0.) upedge=upedge+360.
            dnedge = yi-cbang(iym1)
            if(dnedge .LT. 0.) dnedge=dnedge+360.
c --- SD    The next line is added to avoid "divided by zero"
            if(xi.eq.0.0) xi = 0.001
            dels2 = dels*rad2deg/xi
            if(upedge .LT. dels2) then
               iyp=iyp1
            elseif(dnedge .LT. dels2) then
               iyp=iym1
            endif
         else
c ---       CARTESIAN grid
            ixp = INT(xi+dels) +1
            if(ixp .EQ. ix) ixp = INT(xi-dels) +1
            iyp = INT(yi+dels) +1
            if(iyp .EQ. iy) iyp = INT(yi-dels) +1
         endif
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine celldat (ix,iy,ht)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 031017           CELLDAT
c               D. Strimaitis, Earth Tech
c               E. Insley -- Modified to convert from ft to meters
c
c PURPOSE:     CELLDAT sums and counts the elevation for the particular
c              grid cell, and tracks the max/min values for cell.
c               E. Insley - updated to use real HTMAX array
c
c --- Updates
c     Ver 3.4  Level 980304 to Level 031017    - D. Strimaitis
c              Skip void points with data value < -32000.
c              Reset cell sum to zero upon first valid value - KAM
c
c ARGUMENTS:
c    PASSED:  ix,iy     grid-cell index values
c             ht        elevation (real)
c             /GRID/    output grid specification
c             /CELL/    stored data for cell including units identifier
c
c  RETURNED:  /CELL/    stored data for cell
c                       SUM array is always in meters
c                       HTMAX array is always in meters
c
c
c CALLING ROUTINES:   SAVEELEV
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'cell.trl'
      include 'grid.trl'

c --- Set data value used to identify voids
      data void/-32000./

      if(ht.LT.void) return

      i = ix + (iy-1)*nx
      if(knt(i).eq.0) sum(i)=0.
      sum(i) = sum(i) + ht
c     sums(i) = sums(i) + ht*ht
      knt(i) = knt(i) + 1

      if(ht .GT. htmax(i)) htmax(i) = ht

c --- SUMS and IHMIN are not currently used in output, so they are
c --- commented here to reduce memory requirements

      return
      end
c-----------------------------------------------------------------------
      subroutine cellin
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 031017            CELLIN
c
c ADAPTED from program ELEV in the ARM3 model
c               G. Lundberg, SAI
c         with enhancements by
c               E. Chang, G. Moore, SRC
c               M. Fernau, ET, made save file universal
c               E. Insley -updated to use real HTMAX array
c
c PURPOSE:     CELLIN initializes the SUM, MAX/MIN, and KNT arrays,
c              and retrieves partial arrays, if any.
c
c --- Updates
c     Ver 3.4  Level 980304 to Level 031017    - K. Morrison
c              Read in saved information for discrete receptors
c              Change initial sum value from 0 to -999
c
c
c ARGUMENTS:
c    PASSED:  /GRID/    output grid specification
c             /CONTROL/ program control logicals
c
c  RETURNED:  /CELL/    stored data for cells
c             /XY/      stored data for discrete receptors
c
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'cell.trl'
      include 'control.trl'
      include 'grid.trl'
      include 'xy.trl'

c --- NOTE !!! ---  Disable SUMS and IHMIN to conserve memory

c --- Initialize SUM,SUMS,IHMAX,IHMIN, and KNT arrays
      do i=1,nxy
         sum(i) = -999.
c        sums(i) = 0.
         knt(i) = 0
         htmax(i) = -999.
      enddo

c --- Read Previous save file if the user wants to
      if(LPREV)then
c       read (ioprev,end=83,err=85) ihdr
        read (ioprev,err=85) ihdr
c ---   End of MEF change

c ---   Check if this is the file wanted
        if(ihdr(1) .NE. INT(xllm)  .OR.
     &     ihdr(2) .NE. INT(yllm)  .OR.
     &     ihdr(3) .NE. izone      .OR.
     &     ihdr(4) .NE. INT(sizem) .OR.
     &     ihdr(5) .NE. nx         .OR.
     &     ihdr(6) .NE. ny         .OR.
     &     ihdr(7) .NE. nxyrec)             goto 920

c ---   OK - Retrieve partial arrays from earlier run(s)
        read (ioprev,err=910) (sum(i),i=1,nxy)
        read (ioprev,err=910) (knt(i),i=1,nxy)
        read (ioprev,err=910) (htmax(i),i=1,nxy)
        if (LXY) then
          read (ioprev,err=910) (elrecm(i),i=1,nxyrec)
          read (ioprev,err=910) ((xx(j,i),j=1,4),i=1,nxyrec)
          read (ioprev,err=910) ((yy(j,i),j=1,4),i=1,nxyrec)
          read (ioprev,err=910) ((zz(j,i),j=1,4),i=1,nxyrec)
          read (ioprev,err=910) ((distxy(j,i),j=1,4),i=1,nxyrec)
        endif


        write(iolst,*)
        write(iolst,*) 'CELLIN:  Partial Grid Recovered from .SAV file'
        if(LXY) write(iolst,*)
     &    'CELLIN:  Discrete Info Recovered from .SAV file'

c ---   MEF change: always look for save file 4/11/96

c ---   Save file found and read successfully
        goto 80
      else
c ---   No save file is wanted
c  83   write(iolst,*)
        write(iolst,*)
        write(iolst,*) 'CELLIN:  No Previous Save File is Used'
        goto 80
      endif

c --- Error reading save file
   85 write(iolst,*) 'CELLIN:  Warning -- Error Reading Save File'
c --- End of MEF change

c --- Set header and close file
   80 ihdr(1) = INT(xllm)
      ihdr(2) = INT(yllm)
      ihdr(3) = izone
      ihdr(4) = INT(sizem)
      ihdr(5) = nx
      ihdr(6) = ny
      ihdr(7) = nxyrec
      close (ioprev)
      return

910   continue
      write(iolst,*) 'CELLIN:  Read-ERROR on Save-File'

920   continue
      write(iolst,*) 'CELLIN:  Header of Save-File does not match',
     1               ' grid for this run!'
      write(iolst,*) '  Save-File Header vs Expected Data: '
      write(iolst,*) '    ihdr(1)= ',ihdr(1),' INT(xllm)= ',INT(xllm)
      write(iolst,*) '    ihdr(2)= ',ihdr(2),' INT(yllm)= ',INT(yllm)
      write(iolst,*) '    ihdr(3)= ',ihdr(3),' IZONE= ',izone
      write(iolst,*) '    ihdr(4)= ',ihdr(4),' INT(sizem)= ',INT(sizem)
      write(iolst,*) '    ihdr(5)= ',ihdr(5),' NX= ',nx
      write(iolst,*) '    ihdr(6)= ',ihdr(6),' NY= ',ny
      write(iolst,*) '    ihdr(7)= ',ihdr(7),' NXYREC= ',nxyrec

      write(*,*)' ERROR occurs in Subr. CELLIN  -- See Run LIST file'
      stop
      end
c-----------------------------------------------------------------------
      subroutine cellout
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 010713           CELLOUT
c
c ADAPTED from program ELEV in the ARM3 model
c               G. Lundberg, SAI
c         with enhancements by
c               G. Moore, SRC
c               E. Insley - updated to use real HTMAX array
c
c
c PURPOSE:     CELLOUT saves the partial (or complete) SUM, KNT
c              arrays and the partial (or complete) average ht grid
c
c --- Updates
c     Ver 3.0  Level 010713 to Ver 3.4 Level 031017    - K. Morrison
c              Write saved information for discrete receptors
c     Ver 3.0  Level 980304 to 010713       DGS
c            - SAVE file opened elsewhere
c
c
c ARGUMENTS:
c    PASSED:  /GRID/    output grid specification
c             /XY/      stored data for discrete receptors
c             /CONTROL/ program control logicals
c
c  RETURNED:  /CELL/    stored data for cells
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'cell.trl'
      include 'grid.trl'
      include 'control.trl'
      include 'xy.trl'

c --- NOTE !!! ---  Disable SUMS and IHMIN to conserve memory

c --- Write the header
      write (iosav,err=910) ihdr

c --- Write the temporary arrays
      write (iosav,err=910) (sum(i),i=1,nxy)
      write (iosav,err=910) (knt(i),i=1,nxy)
      write (iosav,err=910) (htmax(i),i=1,nxy)
      if (LXY) then
        write (iosav,err=910) (elrecm(i),i=1,nxyrec)
        write (iosav,err=910) ((xx(j,i),j=1,4),i=1,nxyrec)
        write (iosav,err=910) ((yy(j,i),j=1,4),i=1,nxyrec)
        write (iosav,err=910) ((zz(j,i),j=1,4),i=1,nxyrec)
        write (iosav,err=910) ((distxy(j,i),j=1,4),i=1,nxyrec)
      endif
      return

910   write(iolst,*) 'CELLOUT: Error Writing to Save-File'
      write(*,*)'ERROR occurs writing to SAVE-File -- See Run LIST file'
      stop

      end
c-----------------------------------------------------------------------
      subroutine heights(ht,zhi,zlo)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050815           HEIGHTS
c               D. Strimaitis, SRC
c
c
c PURPOSE:     Transfers terrain height data for each cell to array
c              used in output file.
c               E. Insley - updated to use real HT and HTMAX arrays
c
c --- Updates
c     Ver 3.65 Level 050815 to Ver 3.66 Level 050817 - KAM
c              If noise processing results in a void output, reset
c              the count for that cell to 0
c     Ver 3.64 Level 050526 to Ver 3.65 Level 050815 - KAM
c              Catch voids if void/noise processing not invoked
c     Ver 3.55 Level 040305 to Ver 3.64 Level 050526 - KAM
c              Add default ITYPE as land
c              Force early replacement of ocean cells if options allow
c              (i.e. LREPLACE = .TRUE.)
c     Ver 3.53 Level 040302 to Ver 3.55 Level 040305 - KAM
c              Add noisy data processing
c              Write out point counts
c     Ver 3.51 Level 040228 to Ver 3.53 Level 040302 - KAM
c              Corrected bug in indices to generate X-Y for type check
c     Ver 3.4  Level 031023 to Ver 3.51 Level 040228 - K. Morrison
c              Replace single value of TERDEF with array by crude
c              water/land type as derived from GSHHS.
c              If no coastal processing was done, type defaults to land.
c     Ver 3.4  Level 031017 to Ver 3.4 Level 031023    - K. Morrison
c              Replace -999 with default value TERDEF
c     Ver 3.0  Level 980304 to Ver 4.4 Level 031017    - K. Morrison
c              Cells with no data output -999 as height
c
c ARGUMENTS:
c    PASSED:  /CONTROL/ program control logicals
c             /CELL/    terrain data for each cell
c             /GRID/    output grid specification
c
c  RETURNED:  ht (m)    terrain elevation data
c             zhi (m)   highest elevation in iht array
c             zlo (m)   lowest elevation in iht array
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'cell.trl'
      include 'control.trl'
      include 'grid.trl'

c --- Local variables
      real ht(mxnxy),void/-999./,htnoise(5)/5*0./
      integer nmiss(5)/5*0/,nmissr(5)/5*0/,nmisst/0/,nnoise(5)/5*0/,
     &        nnoiser(5)/5*0/,nnoiset/0/,itype/2/
      logical lreplace

c --- Wite out final point counts to list file
      write(iolst,*)
      write(iolst,fmt="(i8,' Total points extracted within grid')")
     &  npextr
      write(iolst,fmt="(i8,' Points had missing values')") npmiss
      if(LCOAST)
     &  write(iolst,fmt="(i8,' Points were considered noise')") npnoise
      write(iolst,*)

c --- Initialize hi and lo terrain values
      zhi = -9999.
      zlo = 9999.
      lreplace=(MAXVAL(iterrep).GT.0 .OR. MAXVAL(inoiserep).GT.0)

c --- Calculate the elevations needed for model selected
c --- (currently uses either the max elevation in cell for "receptor"
c --- output, or average elevation)
c
      do i=1,nxy
         if(lreplace) then
           ixxr=mod(i,nx)
           if(ixxr.EQ.0) ixxr=nx
           iyyr=i/nx+1
           if(ixxr.EQ.nx) iyyr=iyyr-1
           xxr=(float(ixxr-1)+0.5)*sizek+xllk
           yyr=(float(iyyr-1)+0.5)*sizek+yllk
           itype=iptype(xxr,yyr)+1
           if(iterrep(itype).eq.3) then
             sum(i)=terdef(itype)*FLOAT(knt(i))
             htmax(i)=max(htmax(i),h)
           endif
         endif
         if(LPEAK) then
           if(htmax(i).lt.znoise(itype).and.htmax(i).gt.void) then
             if(inoiserep(itype).gt.0) then
               if(inoiserep(itype).eq.1) then
                 htmax(i)=void
                 knt(i)=0
               endif
               if(inoiserep(itype).eq.2) htmax(i)=znoise(itype)
               if(inoiserep(itype).eq.3) htmax(i)=terdef(itype)
               nnoiser(itype)=nnoiser(itype)+1
             endif
             nnoise(itype)=nnoise(itype)+1
           endif
           if(htmax(i).GE.-998.)then
             h = htmax(i)
           else
             if(iterrep(itype).NE.0) then
               h = terdef(itype)
               nmissr(itype) = nmissr(itype)+1
             else
               h = void
             endif
             nmiss(itype)=nmiss(itype)+1
           endif
         else
           if(knt(i) .GT. 0) then
             h = sum(i)/FLOAT(knt(i))
             if(h.lt.znoise(itype).and.h.gt.void) then
               if(inoiserep(itype).gt.0) then
                 if(inoiserep(itype).eq.1) then
                   h=void
                   knt(i)=0
                 endif
                 if(inoiserep(itype).eq.2) h=znoise(itype)
                 if(inoiserep(itype).eq.3) h=terdef(itype)
                 nnoiser(itype)=nnoiser(itype)+1
               endif
               nnoise(itype)=nnoise(itype)+1
             endif
           else
             if(iterrep(itype).NE.0) then
               h = terdef(itype)
               nmissr(itype) = nmissr(itype)+1
             else
               h = void
             endif
             nmiss(itype)=nmiss(itype)+1
           endif
         endif
c-emi    The following line is commented out because the feet to meters
c-emi    is now taken care of in Subr. LOAD
c        if(LFEET) h = feet2m*h
c ---    Keep track of extremes in the array
         if(zhi .LT. h) zhi = h
         if(zlo .GT. h .AND. h .GE. -998.) zlo = h
c ---    Store elevation data as integers
         ht(i) = h
      enddo

c --- If substitutions were done, write out how many to the list file
      do i=1,5
        nmisst=nmisst+nmiss(i)
        nnoiset=nnoiset+nnoise(i)
        if(inoiserep(i).eq.1) htnoise(i)=-999.
        if(inoiserep(i).eq.2) htnoise(i)=znoise(i)
        if(inoiserep(i).eq.3) htnoise(i)=terdef(i)
      enddo
19    format(24x,'Type   Value-m  #noisy    #replaced')
20    format(24x,'Type   Value-m  #missing  #replaced')
21    format(a30,f8.1,4x,i5,6x,i5)
      if(nnoiset.gt.0) then
        write(iolst,*)
        write(iolst,*) nnoiset,' cells had noisy data'
        if(maxval(inoiserep).gt.0) then
          write(iolst,*) 'Elevations were set to following values:'
          write(iolst,19)
          write(iolst,21) '                       ocean: ',
     &                    htnoise(1),nnoise(1),nnoiser(1)
          write(iolst,21) ' mainland and marine islands: ',
     &                    htnoise(2),nnoise(2),nnoiser(2)
          write(iolst,21) '                       lakes: ',
     &                    htnoise(3),nnoise(3),nnoiser(3)
          write(iolst,21) '            islands in lakes: ',
     &                    htnoise(4),nnoise(4),nnoiser(4)
          write(iolst,21) '            ponds on islands: ',
     &                    htnoise(5),nnoise(5),nnoiser(5)
        endif
        write(iolst,*)
      endif
      if(nmisst.gt.0) then
        write(iolst,*)
        write(iolst,*) nmisst,' cells had no data'
        if(lreplace) then
          write(iolst,*) 'Elevations were set to following values:'
          write(iolst,20)
          write(iolst,21) '                       ocean: ',
     &                    terdef(1),nmiss(1),nmissr(1)
          write(iolst,21) ' mainland and marine islands: ',
     &                    terdef(2),nmiss(2),nmissr(2)
          write(iolst,21) '                       lakes: ',
     &                    terdef(3),nmiss(3),nmissr(3)
          write(iolst,21) '            islands in lakes: ',
     &                    terdef(4),nmiss(4),nmissr(4)
          write(iolst,21) '            ponds on islands: ',
     &                    terdef(5),nmiss(5),nmissr(5)
        endif
        write(iolst,*)
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine fillang(ht)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 980304           FILLANG
c               D. Strimaitis, SRC
c
c
c PURPOSE:     Fills in gaps resulting from using an output grid with
c              greater resolution than terrain data.  This is generally
c              a problem for POLAR grids, so the algorithm only uses
c              data in iy-columns (theta direction) to fill gaps.  It
c              also assumes that few contiguous cells are "empty", so
c              that a nearest-neighbor swap is used.
c      Updated to use real HT array instead of integer*2 IHT array - EMI
c
c
c ARGUMENTS:   ht (m)   terrain elevation data
c    PASSED:  /CONTROL/ program control logicals
c             /CELL/    terrain data for each cell
c             /GRID/    output grid specification
c
c  RETURNED:   ht (m)   terrain elevation data (with no gaps)
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'cell.trl'
      include 'control.trl'
      include 'grid.trl'

c --- Local variables
      real ht(mxnxy)
      logical lagain

      if(LPOLR) then
         do ix=1,nx
            ilast=ix+(ny-1)*nx
            rlast=ht(ilast)
1           lagain=.FALSE.
            do iy=1,ny
               i=ix+(iy-1)*nx
               if(ht(i) .LE. -9999.) then
                  iyp1=iy+1
                  if(iyp1 .GT. ny) iyp1=1
                  inext=ix+(iyp1-1)*nx
                  rnext=ht(inext)
                  if(rlast .LE. -9999.) then
                     if(rnext .LE. -9999.) then
c ---                   Can't fill this gap yet, move on
                        lagain=.TRUE.
                     else
c ---                   Must use next cell ht to fill this gap
                        ht(i)=rnext
                     endif
                  elseif(rnext .LE. -9999.) then
c ---                Must use last cell ht to fill this gap
                     ht(i)=rlast
                  else
c ---                Only 1 cell is missing -- average adjacent hts
                     ht(i)=(rlast+rnext)/2.
                  endif
               endif
               rlast=ht(i)
            enddo
c ---       Repeat loop over iy to until all gaps are filled
            if(LAGAIN) goto 1
         enddo
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine hcopy(ht)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 990130             HCOPY
c               E. Insley, E. Chang, D. Strimaitis, SRC
c
c
c PURPOSE:     Writes terrain elevation data (heights, m MSL) to the
c              list-file to provide a "hard-copy" of the data. Also
c              writes out a table of the number of 'hits' of data
c              per grid cell.
c      Updates:
c      Ver 2.0 Level 990130 to Ver 3.66 Level 050817   KAM
c              Change 1st argument in call to OUT from dummy variable
c              to HT to avoid compiler message
c      Ver 2.0 Level 981025 to Level 990130
c              Use real HT array instead of integer*2 IHT array - EMI
c
c ARGUMENTS:
c    PASSED:   ht  (m)  terrain elevation data
c             /CELL/    terrain data for each cell
c             /GRID/    output grid specification
c
c  RETURNED:  none
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  OUT
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'cell.trl'
      include 'grid.trl'

c --- Local variables
      real ht(mxnxy), dum(mxnx,mxny)
      integer ihits(mxnx,mxny)
      logical ldate
      character*70 messag
      character*5 cdash

      data cdash/' ----'/

c --- Print elevation grid header
      write(iolst,*)
      write(iolst,*)
      write(iolst,*)
      write(iolst,*) '--------------------------'
      write(iolst,*) '     HCOPY:  Results'
      write(iolst,*) '--------------------------'
      write(iolst,*)
      write(iolst,*)
      write(iolst,1000) ihdr(1),ihdr(2),ihdr(3),ihdr(4),ihdr(5),ihdr(6)
 1000 format (T10,'XLLM =',T30,I10.0/
     $        T10,'YLLM =',T30,I10.0/
     $        T10,'IZONE =',T30,I10/
     $        T10,'CELL SIZE (m) =',T30,I10.0/
     $        T10,'NX CELLS =',T30,I10/
     $        T10,'NY CELLS =',T30,I10//)

c --- Write elevation data
      npan=nx/26+1
      do ipan=1,npan
         i1=25*(ipan-1)+1
         i2=i1+24
         if(i2 .GT. nx) i2=nx
         write(iolst,41) (i,i=i1,i2)
         write(iolst,45) (cdash, i=i1,i2)
         do 1101 j=1,ny
         do 1101 i=i1,i2
            ii=(j-1)*nx+i
            dum(i,j)=ht(ii)
1101     continue
         do jj=1,ny
            j=ny-jj+1
            write(iolst,42) j,(dum(i,j),i=i1,i2)
         enddo
      enddo

c --- Write # of 'hits' per grid cell table
      write(iolst,*)
      write(iolst,*)
      write(iolst,*)
      write(iolst,*)
c --- Move 1-D knt array into 2-D ihits array
      do k=1,nxy
        if(mod(k,nx).NE.0)then
          j = (k/nx)+1
        else
          j = (k/nx)
        endif
        i = k-(j-1)*nx
        ihits(i,j) = knt(k)
      enddo
      ldate = .false.
      messag = 'NUMBER OF TERRAIN DATA "HITS" PER CELL'
        call out(ht,ihits,2,5,ldate,messag)
c     do ipan=1,npan
c        i1=25*(ipan-1)+1
c        i2=i1+24
c        if(i2 .GT. nx) i2=nx
c        write(iolst,51) (i,i=i1,i2)
c        do 1201 j=1,ny
c        do 1201 i=i1,i2
c           ii=(j-1)*nx+i
c           idum(i,j)=knt(ii)
c1201     continue
c         do jj=1,ny
c            j=ny-jj+1
c            write(iolst,42) j,(idum(i,j),i=i1,i2)
c         enddo
c      enddo

c --- Compute overall sum for the grid to determine avg 'hits' per cell
c --- Compare each grid cell with the user specified threshold of the average
      write(iolst,*)
      write(iolst,*)
      ngrid = nx*ny
      isumall = 0
      nsum=0
      do k=1,ngrid
c ---    Exclude cells with zero from average
         if(knt(k).GT.0) then
            nsum=nsum+1
            isumall=isumall+knt(k)
         endif
      enddo
      iavg=0
      if(nsum.GT.0) iavg=isumall/nsum
      ifractlo = NINT(0.01*FLOAT(iavg*ithres))
      ifracthi = NINT(0.01*FLOAT(iavg*(200-ithres)))
c --- Count number of cells with low or high number of hits
      n0=0
      nlo=0
      nhi=0
      do k=1,ngrid
        if(knt(k) .LE. 0) then
           n0=n0+1
        elseif(knt(k) .LT. ifractlo) then
           nlo=nlo+1
        elseif(knt(k) .GT. ifracthi) then
          nhi=nhi+1
        endif
      enddo

c --- Report results to list file
      if(n0.GT.0) then
         write(iolst,*)
         write(iolst,*)
         write(iolst,*)
         write(iolst,*)'**********************************************'
         write(iolst,'(a,i10,a)')
     &        'WARNING! There are NO DATA in',n0,' cells'
         write(iolst,'(a,i10,a,i4)')
     &        ' -- Check the coverage of your DEM files'
         write(iolst,*)'**********************************************'
         write(iolst,*)
         write(iolst,*)
         write(iolst,*)
      endif
      ntot=nxy-n0
      if(ntot.GT.0) then
         write(iolst,'(a)')'Summary Information for cells with data:'
         write(iolst,'(a,5x,a,i10)')
     &        ' -- ','       Number of cells with data ',ntot
         write(iolst,'(a,5x,a,i10)')
     &        ' -- ',' Average number of hits per cell ',iavg
         write(iolst,'(a,i10,a,i10)')
     &        ' -- ',nlo,' cells have fewer hits than ',ifractlo
         write(iolst,'(a,i10,a,i10)')
     &        ' -- ',nhi,' cells have  more hits than ',ifracthi
      endif

      return

   41 format(/,' TERRAIN HEIGHTS (METERS MSL) FOR',
     & ' MODELING DOMAIN',/,7x,25(1x,i7))
   45 format(7x,25a8)
   42 format(i4,' : ',25f8.1)

      end
c-----------------------------------------------------------------------
      subroutine modout(ht)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 980304            MODOUT
c               G. Moore, M. Spitzak, D. Strimaitis   SRC
c
c
c PURPOSE:     Writes terrain elevation data (heights, m MSL) to
c              the model input file in the required grid format.
c              Modified the ISC POLAR output to write heights in groups
c              of 5 instead of 10 so that it fits in an ISC input file.
c              (EMI 960329)
c      Updated to use real HT array instead of integer*2 IHT array - EMI
c
c ARGUMENTS:
c    PASSED:   ht  (m)  terrain elevation data
c             /CONTROL/ program control logicals
c             /GRID/    output grid specification
c
c  RETURNED:  none
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'grid.trl'

c --- Local variables
      real ht(mxnxy)
      data deg2rad/.0174533/

      if(LNUAT) then
         do j=1,ny
            ilo = (j-1)*nx + 1
            ihi = ilo + nx - 1
            write(ioout,10) (ht(i),i=ilo,ihi)
10          format(10f8.1)
         enddo

      elseif(LCMET) then
         do j=ny,1,-1
            ilo = (j-1)*nx + 1
            ihi = ilo + nx - 1
            write(ioout,10) (ht(i),i=ilo,ihi)
         enddo

      elseif(LMESO) then
         do j=1,ny
            ilo = (j-1)*nx + 1
            ihi = ilo + nx - 1
            write(ioout,100) (ht(i),i=ilo,ihi)
         enddo
100      format(16f5.0)

      elseif(LISCC) then
         if(LPOLR) then
c ---       Need to create discrete Cartesian output from a polar grid
c ---       Loop over radials
            do j=1,ny
               klo = (j-1)*nx + 1
               angrad = ang(j)*deg2rad
               sinang = SIN(angrad)
               cosang = COS(angrad)
               do i=1,nx
                  k = klo + i -1
                  dism = disk(i)*1000.
                  xrecm = xorgm + dism*sinang
                  yrecm = yorgm + dism*cosang
                  write(ioout,200) xrecm,yrecm,ht(k)
200               format('RE DISCCART',2f10.1,f8.1)
               enddo
            enddo
         else
c ---       Regular Cartesian grid
            yrecm=yllm+(0.5*sizem)
            do j=1,ny
               ibase = (j-1)*nx
               xrecm=xllm+(0.5*sizem)
               do i=1,nx
                  ii=ibase + i
                  write(ioout,200) xrecm,yrecm,ht(ii)
                  xrecm=xrecm+sizem
               enddo
               yrecm=yrecm+sizem
            enddo
         endif

      elseif(LISCP) then
         write(ioout,210)
         write(ioout,211) xorgm,yorgm
         write(ioout,212) ((1000.*disk(i)),i=1,nx)
         write(ioout,213) (ang(i),i=1,ny)
c ---    Write elevation data in rows corresponding to radials
c ---    Break into groups of 5 distances
         do j=1,ny
            ilo = (j-1)*nx + 1
            ihi = ilo + nx - 1
            inum = ihi - ilo
            igrp = inum/5
            do ig=1,igrp
               iglo=ilo+5*(ig-1)
               ighi=MIN(iglo+4,ihi)
               write(ioout,214) ang(j),(ht(i),i=iglo,ighi)
            enddo
            if(ihi .GT. ighi) then
               iglo=ighi+1
               write(ioout,214) ang(j),(ht(i),i=iglo,ihi)
            endif
         enddo
         write(ioout,215)
210      format('RE GRIDPOLR  XXXXX  STA')
211      format('                    ORIG',2f15.1)
212      format('                    DIST',5f8.1)
213      format('                    DDIR',5f6.0)
214      format('                    ELEV',f6.0,5f8.1)
215      format('RE GRIDPOLR  XXXXX  END')

      elseif(LGENR) then
c ---    Define grid corners as CENTER of corner cells
         xlo = xllm+0.5*sizem
         ylo = yllm+0.5*sizem
         xhi = xurm-0.5*sizem
         yhi = yurm-0.5*sizem

c ---    Write header information
         write(ioout,*) nx,ny,xlo,ylo,xhi,yhi,sizem

c ---    Write data, with "rows", starting at lower-left corner of grid
         do j=1,ny
            ilo = (j-1)*nx + 1
            ihi = ilo + nx - 1
            write(ioout,300) (ht(i),i=ilo,ihi)
         enddo
300      format(16f5.0)

      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine pltout(ht,zhi,zlo)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 020730            PLTOUT
c               D. Strimaitis   SRC
c
c
c PURPOSE:     Writes terrain elevation data (heights, m MSL) to
c              the plot-file as a SURFER ".GRD" file
c      Updated to use real HT array instead of integer*2 IHT array - EMI
c
c --- Updates
c --- Ver 3.1  Level 980304 to Level 020730    - D. Strimaitis
c              - Use explicit format for 1st header of output GRD files
c
c ARGUMENTS:
c    PASSED:  ht   (m)  terrain elevation data
c             zhi  (m)  highest elevation in iht array
c             zlo  (m)  lowest elevation in iht array
c             /GRID/    output grid specification
c
c  RETURNED:  none
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'grid.trl'

c --- Local variables
      real ht(mxnxy)

c --- Grid definition
      xlo = xllk+0.5*sizek
      ylo = yllk+0.5*sizek
      xhi = xurk-0.5*sizek
      yhi = yurk-0.5*sizek

c --- Header information
      write(ioplt,'(a4)') 'DSAA'
      write(ioplt,*) nx,ny
      write(ioplt,*) xlo,xhi
      write(ioplt,*) ylo,yhi
      write(ioplt,*) zlo,zhi

c --- Data, "rows", starting at lower-left corner of grid
      do irow = 1,ny
         ilo = (irow-1)*nx+1
         ihi = ilo+nx-1
         write(ioplt,10) (ht(i), i=ilo,ihi)
10       format(10f8.1)
      enddo

      return
      end
c----------------------------------------------------------------------
      subroutine out(rarray,iarray,ityp,nsigd,ldate,messag)
c----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 990130               OUT
c ---           J. Scire, SRC
c
c --- PURPOSE:  Write a gridded field of real or integer numbers
c
c --- Modified 1/99 to accommodate NX, NY > 99 (JSS)
c
c --- INPUTS:
c     RARRAY(MXNX,MXNY) - Real array  - Array of real numbers to print
c                                       (used only if ITYP = 1)
c     IARRAY(MXNX,MXNY) - Int. array  - Array of integer numbers to
c                                       print (used only if ITYP = 2)
c                  ITYP - Integer     - Array type (1=real, 2=integer)
c                 NSIGD - Integer     - No. digits to print (valid range
c                                       for NSIGD is 1 to 5)
c                 LDATE - Logical     - Control variable for printing
c                                       of date (.true. = print date in
c                                       common /GEN/, .false. = do not
c                                       print date)
c                MESSAG - Char.*70    - Label of table
c                    NX - Integer     - No. X grid cells being used in
c                                       array
c                    NY - Integer     - No. Y grid cells being used in
c                                       array
c       Common block /GEN/ variables:
c          NYR, NMO, NDAY, NJUL, NHR  - (Used only if LDATE=.true.)
c       Parameters: NX, NY, MXNX, MXNY, IOLST
c
c --- OUTPUT:  none
c
c --- OUT    called by:  HCOPY
c --- OUT    calls:      WRT, WRT2
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.trl'
      include 'grid.trl'
c
      real rarray(mxnx,mxny)
c
      integer iarray(mxnx,mxny),icol(5)
      integer iout(mxnx)
c
      logical ldate
c
      character*70 messag
      character*1 sign(mxnx),plus,minus
      character*24 form1(5)
      character*21 form2(5)
      character*18 form3(5)
c
c***  include 'gen.met'
c
      data icol /40,40,30,25,20/
      data plus,minus /'+','-'/
      data form1 /'(1x,i3,1x,1hI,40(i3,1x))',
     1            '(1x,i3,1x,1hI,40(i3,1x))',
     2            '(1x,i3,1x,1hI,40(i3,1x))',
     3            '(1x,i3,1x,1hI,40(i4,1x))',
     4            '(1x,i3,1x,1hI,40(i5,1x))'/
      data form2 /'(5x,1hI,40(2x,a1,1x))',
     1            '(5x,1hI,40(2x,a1,1x))',
     2            '(5x,1hI,40(2x,a1,1x))',
     3            '(5x,1hI,40(3x,a1,1x))',
     4            '(5x,1hI,40(4x,a1,1x))'/
      data form3 /'(6x,40(i3,1x))',
     1            '(6x,40(i3,1x))',
     2            '(6x,40(i3,1x))',
     3            '(6x,40(i4,1x))',
     4            '(6x,40(i5,1x))'/
c
c --- check that valid values of array type (ityp) and print digits
c --- (nsigd) have been passed to routine
      if(ityp.ne.1.and.ityp.ne.2)then
        write(iolst,*)'ERROR in SUBR. OUT -- invalid value of ITYP -- ',
     1   'ITYP = ',ityp
        write(*,*)'ERROR in SUBR. OUT -- See Run LIST file'
         stop
      endif
      if(nsigd.lt.1.or.nsigd.gt.5)then
        write(iolst,*)'ERROR in SUBR. OUT -- invalid value of NSIGD -- '
     1  ,'NSIGD = ',nsigd
        write(*,*)'ERROR in SUBR. OUT -- See Run LIST file'
         stop
      endif
c
      icr=2
      if(nsigd.eq.1)icr=1
      if(mod(nx,icol(nsigd)).eq.0)then
         npass=nx/icol(nsigd)
      else
         npass=nx/icol(nsigd)+1
      endif
c
c --- real array -- find min. & max. values
      if(ityp.ne.1)go to 50
      xmax=-1.e-25
      xmin=1.e25
      do 10 i=1,nx
      do 10 j=1,ny
      if(rarray(i,j).gt.xmax)xmax=rarray(i,j)
      if(rarray(i,j).lt.xmin)xmin=rarray(i,j)
10    continue
      if(xmin.ne.0.0.or.xmax.ne.0.0)go to 12
c *** if(ldate)write(iolst,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(iolst,95)messag
      write(iolst,11)
11    format(1x,'GRID NOT PRINTED -- all values zero')
      return
c
12    continue
      xexp=xmax
      if(abs(xmin).gt.xmax)xexp=abs(xmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic2=0
      do 30 ipass=1,npass
c
c *** if(ldate)write(iolst,94)messag,nyr,nmo,nday,njul,nhr
c94    format(/1x,a70,2x,'year: ',i2,2x,'month: ',i2,2x,'day: ',i2,2x,
c ***1 'Julian day: ',i3,2x,'hour: ',i2/)
      if(.not.ldate)write(iolst,95)messag
95    format(/1x,a70/)
      write(iolst,109)nexp
109   format(1x,'Multiply all values by 10 ** ',i3/)
c
      ic1=ic2+1
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nx)ic2=nx
c
         do 20 jj=ny,1,-1
         icnt=0
c
            do 18 i=ic1,ic2
            icnt=icnt+1
            if(rarray(i,jj).lt.0)then
               iout(icnt)=-(rarray(i,jj)*xscale-0.5)
               sign(icnt)=minus
            else
               iout(icnt)=rarray(i,jj)*xscale+0.5
               sign(icnt)=plus
            endif
18          continue
         call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,iolst)
20       continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(iolst,101)(minus,n=1,nund)
101   format(5x,128a1)
      call wrt2(form3(nsigd),ic1,ic2,iolst)
30    continue
      return
c
c --- integer array -- find min. & max. values
50    continue
      kmax=-9999999
      kmin=9999999
      do 110 i=1,nx
      do 110 j=1,ny
      if(iarray(i,j).gt.kmax)kmax=iarray(i,j)
      if(iarray(i,j).lt.kmin)kmin=iarray(i,j)
110   continue
      if(kmin.ne.0.or.kmax.ne.0)go to 102
c *** if(ldate)write(iolst,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(iolst,95)messag
      write(iolst,11)
      return
c
102   continue
      xexp=kmax
      if(iabs(kmin).gt.kmax)xexp=iabs(kmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic2=0
      do 130 ipass=1,npass
c
c *** if(ldate)write(iolst,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(iolst,95)messag
      write(iolst,109)nexp
c
      ic1=ic2+1
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nx)ic2=nx
c
         do 120 jj=ny,1,-1
         icnt=0
c
            do 118 i=ic1,ic2
            icnt=icnt+1
            if(iarray(i,jj).lt.0)then
               iout(icnt)=-(iarray(i,jj)*xscale-0.5)
               sign(icnt)=minus
            else
               iout(icnt)=iarray(i,jj)*xscale+0.5
               sign(icnt)=plus
            endif
118         continue
         call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,iolst)
120      continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(iolst,101)(minus,n=1,nund)
      call wrt2(form3(nsigd),ic1,ic2,iolst)
130   continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrt(form1,form2,jj,iout,sign,n,iolst)
c----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 901130               WRT
c ---           J. Scire, SRC
c
c --- PURPOSE:  Write one Y row of gridded data
c
c --- INPUTS:
c              FORM1 - Char.*24    - Format field for Y label and data
c                                    to be printed
c              FORM2 - Char.*21    - Format field for sign of data
c                 JJ - Integer     - Y grid cell number
c            IOUT(N) - Int. array  - Array of data to be printed
c                                    (one Y row)
c            SIGN(N) - Char.*1     - Array containing sign of data
c                                    ('+' or '-')
c                  N - Integer     - Number of cells in this row
c                IO6 - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT called by:  OUT
c --- WRT calls:      none
c----------------------------------------------------------------------
      integer iout(n)
c
      character*1 sign(n)
      character*24 form1
      character*21 form2
c
      write(iolst,form1)jj,iout
      write(iolst,form2)sign
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrt2(form,n1,n2,iolst)
c----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 901130              WRT2
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write a line labeling grid cell numbers
c
c --- INPUTS:
c               FORM - Char.*18    - Format field of data to be printed
c                 N1 - Integer     - Starting grid cell number
c                 N2 - Integer     - Ending grid cell number
c                IO6 - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT2 called by:  OUT
c --- WRT2 calls:      none
c----------------------------------------------------------------------
      character*18 form
c
      write(iolst,form)(i,i=n1,n2)
      return
      end
c-----------------------------------------------------------------------
      subroutine xyelev(xm,ym,elevm)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 031017            XYELEV
c               D. Strimaitis, SRC
c
c PURPOSE:     XYELEV checks for discrete locations within xyradkm
c              of the data point (x,y) and updates the stored elevation
c              if ELEV exceeds the stored value.  Alternatively, it 
c              updates stored data for eventual interpolation.
c
c --- Updates
c     Ver 3.4  Level 000107 to Level 031017    - D. Strimaitis
c              Skip void points with data value < -32000.
c              Add storage for interpolation   - K. Morrison
c
c ARGUMENTS:
c    PASSED:  xm,ym     terrain data location (m)
c             elevm     elevation (m)
c             /CONTROL/ logical LINTXY for interpolation 
c             /XY/      stored data for location
c
c  RETURNED:  /XY/      updated stored data for location
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  IPTYPE
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'cell.trl'
      include 'xy.trl'

c --- Set data value used to identify voids
      data void/-32000./

c --- Convert data point location from meters to kilometers
      xkm=.001*xm
      ykm=.001*ym

c --- Return if value is void
      if(elevm.LT.void) return

      if(LINTXY) then
c --- Interpolation
c ---   Find the nearest 4 points, keeping the simplex from
c       being degenerate
        do i=1,nxyrec
          diff=SQRT((xkm-xreckm(i))**2+(ykm-yreckm(i))**2)
          if(xkm.le.xreckm(i)) then
            if(ykm.le.yreckm(i)) then
              if(diff.lt.distxy(1,i)) then
                xx(1,i)=xkm
                yy(1,i)=ykm
                zz(1,i)=elevm
                distxy(1,i)=diff
              endif
            else
              if(diff.lt.distxy(4,i)) then
                xx(4,i)=xkm
                yy(4,i)=ykm
                zz(4,i)=elevm
                distxy(4,i)=diff
              endif
            endif
          else
            if(ykm.le.yreckm(i)) then
              if(diff.lt.distxy(2,i)) then
                xx(2,i)=xkm
                yy(2,i)=ykm
                zz(2,i)=elevm
                distxy(2,i)=diff
              endif
            else
              if(diff.lt.distxy(3,i)) then
                xx(3,i)=xkm
                yy(3,i)=ykm
                zz(3,i)=elevm
                distxy(3,i)=diff
              endif
            endif
          endif
        enddo
                
      else      
c ---   Search for discrete locations within xyradkm of data 
c       point
        do i=1,nxyrec
           diff=SQRT((xkm-xreckm(i))**2+(ykm-yreckm(i))**2)
           if(diff.LE.xyradkm) then
              if(elevm .GT. elrecm(i)) elrecm(i) = elevm
           endif
        enddo
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine xyin
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050817              XYIN
c               D. Strimaitis, SRC
c
c PURPOSE:     XYIN reads data for all discrete locations and stores
c              data in arrays.  These data may be from a previous
c              application of TERREL.
c
c --- Updates
c     Ver 3.4  Level 031017 to Ver 3.66 Level 050817
c              Remove  ELJNK, ZJNK - not needed
c              Remove compiler warnings about XJNK, YJNK
c     Ver 3.4  Level 010115 to Level 031017    - D. Strimaitis
c              Initialize elevation field with -999. instead of 0.
c              Initialize interpolation arrays - K. Morrison
c              Stop execution if too many receptors or number
c              of columns is incorrect
c
c     Level 000107 to 010115               -Shuming Du
c              Modified to allow different discrete receptor fields 
c              (two columns: x,y) 
c              or (four columns: x,y,elevation,receptor_height).
c
c ARGUMENTS:
c    PASSED:  
c             none
c
c  RETURNED:  /XY/      stored data for discrete locations
c
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'xy.trl'

      character*80 discin4

c --- File is free-format, providing x,y,elev,and z (AGL) data
c --- Repeat reads until the EOF is reached
      i=1
      if(nxycol.eq.4) then
c
c ---   Check that there are really 4 columns
c
        read(ioxyi,fmt='(a80)') discin4
        read(discin4,*,end=88) xreckm(i),yreckm(i),elrecm(i),zrecm(i)
        rewind ioxyi
1       read(ioxyi,*,end=999) xreckm(i),yreckm(i),elrecm(i),zrecm(i)
        i=i+1
c
c ---   Check number of receptors, and stop if too many
c
        if(i .LE. mxrecxy) goto 1
          read(ioxyi,*,end=999) xjnk,yjnk
        goto 89

      elseif(nxycol.eq.2) then
2       read(ioxyi,*,end=999) xreckm(i),yreckm(i)
        i=i+1
c
c ---   Check number of receptors, and stop if too many
c
        if(i .LE. mxrecxy) goto 2
        read(ioxyi,*,end=999) xjnk,yjnk
        goto 89

      else
        print *, ' The number of fields in the input receptor file'
        print *, ' must be 2 or 4'
        stop
      endif
c
c --- The number of columns specified was 4, but 4 columns were not found
c
88    print *,' There are not 4 columns in the discrete receptor file'
      print *,' Change the number of columns in the input file or'
      print *,' check the XY file'
      write(iolst,*)
      write(iolst,*) 'XYIN:  Error in Discrete Receptors'
      write(iolst,*) '4 columns were specified but not found'
      write(iolst,*) 'Change number of columns or check XY file'
      stop
c
c --- The number of discrete receptors exceeds the maximum allowed
c
89    print *,' There are too many discrete receptors'
      write(iolst,*)
      write(iolst,*) 'XYIN:  Error in Discrete Receptors'
      write(iolst,*) 'NXYREC exceeds the parameter MXRECXY '
      write(iolst,*) 'MXRECXY = ',mxrecxy
      write(iolst,*) 'Increase MXRECXY in PARAMS.TRL and recompile'
      write(iolst,*) 'or reduce the number of discrete receptors'
c ---   Remove compiler warnings
        xjnk=xjnk
        yjnk=yjnk
      stop
c
999   nxyrec=i-1
      if(nxycol.eq.2) then
        do i=1,nxyrec
          elrecm(i)=-999.
          zrecm(i)=0.
        enddo
      endif
      do i=1,nxyrec
        xx(1,i)=-1.e15
        yy(1,i)=-1.e15
        xx(2,i)=1.e15
        yy(2,i)=-1.e15
        xx(3,i)=1.e15
        yy(3,i)=1.e15
        xx(4,i)=-1.e15
        yy(4,i)=1.e15
        do j=1,4
          zz(j,i)=-999.
          distxy(j,i)=1.e15
        enddo
      enddo
      return
      end

c-----------------------------------------------------------------------
      subroutine xyoutput
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 031031          XYOUTPUT
c               D. Strimaitis, SRC
c
c PURPOSE:     XYOUT writes data for all discrete locations
c
c --- Updates
c     Ver 3.4  Level 031023 to Ver 3.51 Level 040228 - K. Morrison
c              Replace single value of TERDEF with array by crude
c              water/land type as derived from GSHHS.
c              If no coastal processing was done, type defaults to land.
c     Ver 3.4  Level 031017 to Level 031023    - K. Morrison
c              Replace missing values with TERDEF upon output
c     Ver 3.4  Level 000107 to Level 031017    - K. Morrison
c              Add interpolation option
c              
c ARGUMENTS:
c    PASSED:  
c             /XY/      stored data for discrete locations
c             /CELL/    stored data for cells
c
c  RETURNED:  none
c
c
c CALLING ROUTINES:   COMP
c
c EXTERNAL ROUTINES:  IPTYPE, INTERP2D
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'xy.trl'
      include 'cell.trl'

      integer nmiss(5)/5*0/,nmissr(5)/5*0/,nmisst/0/
      integer nnoise(5)/5*0/,nnoiser(5)/5*0/,nnoiset/0/
      real void/-999./,htnoise(5)/5*0./
      logical lreplace

      lreplace=(MAXVAL(iterrep).GT.0 .OR. MAXVAL(inoiserep).GT.0)

c --- Check if interpolation needed
c
      if(LINTXY) then
        do i=1,nxyrec
          call interp2d(xx(1,i),yy(1,i),zz(1,i),distxy(1,i),xreckm(i),
     &      yreckm(i),xyradkm,elrecm(i),zerr)
        enddo
      endif
          
c --- File is free-format, providing x,y,elev,and z (AGL) data
      do i=1,nxyrec
         itype=iptype(xreckm(i),yreckm(i))+1
         if(elrecm(i).lt.znoise(itype).and.elrecm(i).gt.void) then
           if(inoiserep(itype).gt.0) then
             if(inoiserep(itype).eq.1) elrecm(i)=void
             if(inoiserep(itype).eq.2) elrecm(i)=znoise(itype)
             if(inoiserep(itype).eq.3) elrecm(i)=terdef(itype)
             nnoiser(itype)=nnoiser(itype)+1
           endif
           nnoise(itype)=nnoise(itype)+1
         endif
         if(elrecm(i).lt.-998.) then
           if(iterrep(itype).ne.0) then
             elrecm(i)=terdef(itype)
             nmissr(itype)=nmissr(itype)+1
           endif
           nmiss(itype)=nmiss(itype)+1
         endif
         write(ioxyo,10) xreckm(i),yreckm(i),elrecm(i),zrecm(i)
      enddo

10    format(1x,2f12.5,2f10.2)

c --- If substitutions were done, write out how many to the list file
      do i=1,5
        nmisst=nmisst+nmiss(i)
        nnoiset=nnoiset+nnoise(i)
        if(inoiserep(i).eq.1) htnoise(i)=-999.
        if(inoiserep(i).eq.2) htnoise(i)=znoise(i)
        if(inoiserep(i).eq.3) htnoise(i)=terdef(i)
      enddo
19    format(24x,'Type   Value-m  #noisy    #replaced')
20    format(24x,'Type   Value-m  #missing  #replaced')
21    format(a30,f8.1,4x,i5,6x,i5)
      if(nnoiset.gt.0) then
        write(iolst,*)
        write(iolst,*) nnoiset,' receptors had noisy data'
        if(maxval(inoiserep).gt.0) then
          write(iolst,*) 'Elevations were set to following values:'
          write(iolst,19)
          write(iolst,21) '                       ocean: ',
     &                    htnoise(1),nnoise(1),nnoiser(1)
          write(iolst,21) ' mainland and marine islands: ',
     &                    htnoise(2),nnoise(2),nnoiser(2)
          write(iolst,21) '                       lakes: ',
     &                    htnoise(3),nnoise(3),nnoiser(3)
          write(iolst,21) '            islands in lakes: ',
     &                    htnoise(4),nnoise(4),nnoiser(4)
          write(iolst,21) '            ponds on islands: ',
     &                    htnoise(5),nnoise(5),nnoiser(5)
        endif
        write(iolst,*)
      endif
      if(nmisst.gt.0) then
        write(iolst,*)
        write(iolst,*) nmisst,' receptors had no data'
        if(lreplace) then
          write(iolst,*) 'Elevations were set to following values:'
          write(iolst,20)
          write(iolst,21) '                       ocean: ',
     &                    terdef(1),nmiss(1),nmissr(1)
          write(iolst,21) ' mainland and marine islands: ',
     &                    terdef(2),nmiss(2),nmissr(2)
          write(iolst,21) '                       lakes: ',
     &                    terdef(3),nmiss(3),nmissr(3)
          write(iolst,21) '            islands in lakes: ',
     &                    terdef(4),nmiss(4),nmissr(4)
          write(iolst,21) '            ponds on islands: ',
     &                    terdef(5),nmiss(5),nmissr(5)
        endif
        write(iolst,*)
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine get4cnr(dxi,dyi,xorg,yorg,nxi,nyi,rlati,rloni)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050817           GET4CNR
c ---           Shuming Du, EARTH TECH, Inc.
c
c --- PURPOSE:  Calculate locations of the 4 corners of output domain 
c               within the USGS Global data set
c
c --- NOTE:    This routine uses the following conventions:
c                 Latitude  - Northern Hemisphere - positive
c                             Southern Hemisphere - negative
c                 Longitude - Eastern Hemisphere  - positive
c                             Western Hemisphere  - negative
c
c --- UPDATES
c     Ver 3.65 Level 050815 to Ver 3.66 Level 050817   KAM
c              Modify +/-200 adjustment to NBEGX, NBEGY, NENDX, NENDY 
c     Ver 3.63 Level 030402 to Ver 3.65 Level 050815   KAM
c              Add check for dateline crossing
c              Constrain NENDX and NENDY to maximum actual number of
c              points (allows debugging)
c     Ver 3.3  Level 021018 to 030402       DGS
c              - Add false easting/northing to GLOBE1 calls
c     Ver 3.2  Level 010713 to 021018       DGS
c              - Add /CONTROL/ for output projection logicals
c              - Add /GRID/ for output projection parameters
c              - Use full COORDS (GLOBE1, GLOBE)
c     Ver 3.0  Level 000428 to 010713       DGS
c              - Longitude in arg list changed to EAST Long
c              - LSHEMI and LLCC explicitly passed/declared
c              - IZONE added to arg list
c
c --- INPUTS:
c
c       DXI   - real     - Grid size (in X direction) of the input image file (km)
c       DYI   - real     - Grid size (in Y direction) of the input image file (km)
c       XORG  - real     - X coordinate (in Lambert Azimuthal projection) 
c                          of the lower-left corner of the input image file (km)
c       YORG  - real     - Y coordinate (in Lambert Azimuthal projection) 
c                          of the lower-left corner of the input image file (km)
c       NXI   - integer  - Number of grid cells (in X direction) in the 
c                          input image file
c       NYI   - integer  - Number of grid cells (in Y direction) in the 
c                          input image file
c       RLATI - real     - Reference latitude (deg.) of the origin of
c                          the Lambert Azimuthal projection of the input image file
c       RLONI - real     - Reference longitude (deg.) of the origin of
c                          the Lambert Azimuthal projection of the input image file
c
c --- OUTPUTS:
c
c       NBEGX - integer  - Location (column) of the lower-left corner of 
c                            output domain in the input image file.
c       NENDX - integer  - Location (column) of the upper-right corner of 
c                            output domain in the input image file.
c       NBEGY - integer  - Location (row) of the lower-left corner of 
c                            output domain in the input image file.
c       NENDY - integer  - Location (row) of the upper-right corner of 
c                            output domain in the input image file.
c
c ---  GET4CNR called by:  SETUSGSG
c ---  GET4CNR calls:      GLOBE1, GLOBE
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.trl'

c --- Include commons
      include 'control.trl'
      include 'grid.trl'

      dimension ixx(4), iyy(4)
      real*8 vecti(9),vecto(9)
      character*12 caction
      character*4 c4dum

      common /GCORNR/ nbegx,nendx,nbegy,nendy

c --- Set Scale Factor of Tangential TM projection (1.0)
      tmsone=1.00000

c --- Set input dataset false Easting/Northing to zero
      feasti=0.0
      fnorti=0.0

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

c --- Translate GRID corners to Global DB lat/lon
c -----------------------------------------------

c --- Set translation vectors
      if(lutm) then
c ---    Using UTM grid
         write(iolst,*)
         write(iolst,*)' UTM coordinates of corners'
         write(iolst,*)' (Universal convention for longitude)'
         write(iolst,*)' IZONE = ',izone,' HEMISPHERE = ',utmhem
c ---    Set conversion vectors
         call GLOBE1('UTM     ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feast,fnorth,
     &               'LL      ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(llcc) then
c ---    Using Lambert Conformal grid
         write(iolst,*)
         write(iolst,*)' Lambert Conformal coordinates of corners'
         write(iolst,*)' (Universal convention for longitude)'
c ---    Set conversion vectors
         call GLOBE1('LCC     ',iutm,xdum,xlat1,xlat2,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(lps) then
c ---    Using Polar Stereographic grid
         write(iolst,*)
         write(iolst,*)' Polar Stereographic coordinates of corners'
         write(iolst,*)' (Universal convention for longitude)'
c ---    Set conversion vectors
         call GLOBE1('PS      ',iutm,xdum,xlat1,xdum,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(lem) then
c ---    Using Equatorial Mercator grid
         write(iolst,*)
         write(iolst,*)' Equatorial Mercator coordinates of corners'
         write(iolst,*)' (Universal convention for longitude)'
c ---    Set conversion vectors
         call GLOBE1('EM      ',iutm,xdum,xlat1,-xlat1,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(llaza) then
c ---    Using Lambert Azimuthal Equal Area grid
         write(iolst,*)
         write(iolst,*)' Lambert Azimuthal coordinates of corners'
         write(iolst,*)' (Universal convention for longitude)'
c ---    Set conversion vectors
         call GLOBE1('LAZA    ',iutm,xdum,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      elseif(lttm) then
c ---    Using Tangential TM grid
         write(iolst,*)
         write(iolst,*)' Tangential TM coordinates of corners'
         write(iolst,*)' (Universal convention for longitude)'
c ---    Set conversion vectors
         call GLOBE1('TM      ',iutm,tmsone,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               'LL      ',iutm,xdum,xdum,xdum,rdum,rdum,
     &               feasti,fnorti,
     &               caction,vecti,vecto)
      endif

c --- Obtain Lat/Lon for the corners of the grid
c --- Lower-left (SW) corner coordinates of domain are (x,y)orgk (km)
c --- Upper-right (NE) corner coordinates of domain are (x,y)urk (km)
c
c --- Datum for Grid coordinates is /GRID/datum
c --- Datum for Global DB coordinates is /CONTROL/dusgsla

      call GLOBE(iolst,caction,datum,vecti,dusgsla,vecto,
     &           xorgk,yorgk,rlon1,rlat1,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,dusgsla,vecto,
     &           xurk,yorgk,rlon2,rlat2,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,dusgsla,vecto,
     &           xurk,yurk,rlon3,rlat3,idum,c4dum)
      call GLOBE(iolst,caction,datum,vecti,dusgsla,vecto,
     &           xorgk,yurk,rlon4,rlat4,idum,c4dum)
c --- Check for dateline crossing
        if(lbranch) then
          if(rlon1.lt.0.) rlon1=rlon1+360.
          if(rlon2.lt.0.) rlon2=rlon2+360.
          if(rlon3.lt.0.) rlon3=rlon3+360.
          if(rlon4.lt.0.) rlon4=rlon4+360.
        endif
c --- Write the corner points to the list file
      write(iolst,31)'SW',xorgk,yorgk,rlat1,rlon1
      write(iolst,31)'SE',xurk,yorgk,rlat2,rlon2
      write(iolst,31)'NE',xurk,yurk,rlat3,rlon3
      write(iolst,31)'NW',xorgk,yurk,rlat4,rlon4
31    format(3x,a2,' Corner: X,Y = ',f12.5,1x,f12.5,' NLat./ELon. = ',
     1   f12.5,1x,f12.5)

c --- Convert Lat/Long to Lambert Azimuthal
c --- Set conversion vectors
      call GLOBE1('LL      ',iutm,xdum,xdum,xdum,rdum,rdum,
     &            feasti,fnorti,
     &            'LAZA    ',iutm,xdum,xdum,xdum,rlati,rloni,
     &            feast,fnorth,
     &            caction,vecti,vecto)
      call GLOBE(iolst,caction,dusgsla,vecti,dusgsla,vecto,
     &           rlon1,rlat1,x1,y1,idum,c4dum)
      call GLOBE(iolst,caction,dusgsla,vecti,dusgsla,vecto,
     &           rlon2,rlat2,x2,y2,idum,c4dum)
      call GLOBE(iolst,caction,dusgsla,vecti,dusgsla,vecto,
     &           rlon3,rlat3,x3,y3,idum,c4dum)
      call GLOBE(iolst,caction,dusgsla,vecti,dusgsla,vecto,
     &           rlon4,rlat4,x4,y4,idum,c4dum)
c --- Write the corner points in Lambert Azimuthal coordinates
      write(iolst,*)
      write(iolst,*)' Lambert Azimuthal coordinates of corners'
      write(iolst,*)' (Universal convention for longitude)'
      write(iolst,31)'SW',x1,y1,rlat1,rlon1
      write(iolst,31)'SE',x2,y2,rlat2,rlon2
      write(iolst,31)'NE',x3,y3,rlat3,rlon3
      write(iolst,31)'NW',x4,y4,rlat4,rlon4
c
      ixx(1) = int((x1 - xorg)/dxi) + 1
      iyy(1) = int((y1 - yorg)/dyi) + 1
      ixx(2) = int((x2 - xorg)/dxi) + 1
      iyy(2) = int((y2 - yorg)/dyi) + 1
      ixx(3) = int((x3 - xorg)/dxi) + 1
      iyy(3) = int((y3 - yorg)/dyi) + 1
      ixx(4) = int((x4 - xorg)/dxi) + 1
      iyy(4) = int((y4 - yorg)/dyi) + 1
      nbegx = nxi
      nbegy = nyi
      nendx = 1
      nendy = 1
      Do i = 1,4
         if(ixx(i).lt.nbegx)nbegx = ixx(i)
         if(iyy(i).lt.nbegy)nbegy = iyy(i)
         if(ixx(i).gt.nendx)nendx = ixx(i)
         if(iyy(i).gt.nendy)nendy = iyy(i)
      Enddo
c      if(nbegy.gt.200)nbegy = nbegy - 200
c      if(nbegx.gt.200)nbegx = nbegx - 200
c      if(nendx+200.lt.nxi)nendx = nendx + 200
c      if(nendy+200.lt.nyi)nendy = nendy + 200
c       nendx=min(nendx,nxi)
c       nendy=min(nendy,nyi)
        nbegx=max(nbegx-200,1)
        nbegy=max(nbegy-200,1)
        nendx=min(nendx+200,nxi)
        nendy=min(nendy+200,nyi)
c
      write(iolst,*)
      write(iolst,*)' nbegx, nendx: ',nbegx,nendx
      write(iolst,*)' nbegy, nendy: ',nbegy,nendy

      return   
      end
c----------------------------------------------------------------------
      subroutine fin
c----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 030402               FIN
c ---          J. Scire, Earth Tech
c
c --- PURPOSE:  Run termination routine -- compute runtime
c
c --- UPDATES
c     Ver 3.3  Level 021018 to 030402       DGS
c              - Add list file unit number to JULDAY call
c
c --- INPUTS:
c       Common block /QA/
c          rdate, rtime, rcpu
c       Parameters: IOLST, IOMESG
c
c --- OUTPUT:  none
c
c --- FIN called by:  MAIN
c --- FIN calls:      DATETM, JULDAY, DELTT, YR4C
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.trl'
      include 'qa.trl'
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
         call julday(iolst,iyr1,imo1,iday1,ijul1)

         read(rdate2(1:2),10)imo2
         read(rdate2(4:5),10)iday2
         read(rdate2(7:10),'(i4)')iyr2
         call julday(iolst,iyr2,imo2,iday2,ijul2)

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
     2         2x,'      Elapsed clock time: ',f10.1,' (seconds)'//
     3         2x,'                CPU time: ',f10.1,' (seconds)')
c
      return
      end
c-----------------------------------------------------------------------
      subroutine wrthead
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050824           WRTHEAD
c               D. Strimaitis, Earth Tech
c
c PURPOSE:     WRTHEAD constructs the header records for the output
c              data file TERREL.DAT and RAWECHO.DAT
c
c --- UPDATE
c --- V3.62-V3.67   050824  (KAM): Add header output for raw data echo
c                                  file, resulting in changing datatype
c                                  and dataver to arrays with 2 elements,
c                                  one for the processed output and one
c                                  for the raw data
c --- V3.3-V3.62    050128  (DGS): Add call to COORDSVER and write info
c                                  to output file headers
c
c ARGUMENTS:
c    PASSED:  /CONTROL/   logicals
c             /GRID/      data
c             /QA/        ver,level
c
c  RETURNED:  none
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  NIMADATE, COORDSVER
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.trl'
      include 'control.trl'
      include 'grid.trl'
      include 'qa.trl'

c --- Local Variables
      character*12 daten
      character*16 dataset(2),dataver(2)
      character*50 verdoc
      character*64 datamod
      character*86 comment1

c --- Configure output variables
      data dataset/'TERREL.DAT','RAWECHO.DAT'/
      data dataver/'2.0','1.0'/
      data datamod/'Header structure with coordinate parameters'/
      data ncomment/2/
      data comment1/'Produced by TERREL Version: '/

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

c --- Obtain COORDS version information
      call COORDSVER(iolst,verdoc)

c --- Record 1:  Dataset, Version, Modifier
      write(ioout,'(2a16,a64)') dataset(1),dataver(1),datamod
      if(lrawecho) write(iorawo,'(2a16,a64)') dataset(2),dataver(2),
     &    datamod
c --- Record 2:  Number of comment records
      write(ioout,'(i4)') ncomment
      if(lrawecho) write(iorawo,'(i4)') ncomment
c --- Record 3:  Comment (optional/repeatable)
      write(ioout,'(a86)') comment1
      if(lrawecho) write(iorawo,'(a86)') comment1
c --- Report COORDS version
      comment1(1:36)='Internal Coordinate Transformations '
      comment1(37:86)=verdoc
      write(ioout,'(a86)') comment1
      if(lrawecho) write(iorawo,'(a86)') comment1
c --- Record 4:  Map projection
      write(ioout,'(a8)') pmap
      if(lrawecho) write(iorawo,'(a8)') pmap
c --- Record 5:  Map projection parameters
      if(LUTM) then
         write(ioout,'(i4,a4)') izone,utmhem
         if(lrawecho) write(iorawo,'(i4,a4)') izone,utmhem
      elseif(LLCC) then
         write(ioout,'(4a16)') clat0,clon0,clat1,clat2
         if(lrawecho) write(iorawo,'(4a16)') clat0,clon0,clat1,clat2
      elseif(LPS) then
         write(ioout,'(3a16)') clat0,clon0,clat1
         if(lrawecho) write(iorawo,'(3a16)') clat0,clon0,clat1
      elseif(LEM.or.LLAZA.or.LTTM) then
         write(ioout,'(2a16)') clat0,clon0
         if(lrawecho) write(iorawo,'(2a16)') clat0,clon0
      endif
c --- Record 6:  False Easting, Northing
      if(LLCC.or.LLAZA.or.LTTM) then
         write(ioout,*) feast,fnorth
         if(lrawecho) write(iorawo,*) feast,fnorth
      endif
c --- Record 7:  Map DATUM
      call NIMADATE(daten)
      write(ioout,'(a8,a12)') datum,daten
      if(lrawecho) write(iorawo,'(a8,a12)') datum,daten
c --- Record 8:  Grid
      write(ioout,'(2i8,4f12.3)') nx,ny,xllk,yllk,sizek,sizek
c --- Record 9:  XYUNIT,ZUNIT
      write(ioout,'(2a4)') 'KM  ','M   '
      if(lrawecho) write(iorawo,'(2a4)') 'KM  ','M   '
c --- Record 10:  Record structure
      write(ioout,'(2a4)') 'W_E ','N_S '

      return
      end
c 
c-----------------------------------------------------------------------
      logical function lbig_end(i4c)
c-----------------------------------------------------------------------
c      
c --- TERREL    Version: 3.684          Level: 050817          LBIG_END
c               K. Morrison, Earth Tech
c
c PURPOSE:      LBIG_END tests the endian of the machine on which the
c               program is running.
c
c --- Updates
c     Ver 3.4 Level 031017 to Ver 3.66 Level 050817
c               Remove compiler warning about I4C not being used
c
c-----------------------------------------------------------------------

      integer*4 i4b
      integer*2 i2b(2)
      equivalence (i4b,i2b(1))
c
c       Remove compiler warning
        i4d=i4c
        i4d=i4d
c
      i4b=1
      if(i2b(1).eq.1) then
            lbig_end=.false.
      elseif(i2b(2).eq.1) then
            lbig_end=.true.
      else
            write(*,*) 'Invalid ENDIAN result'
      endif
      return
      end
c-----------------------------------------------------------------------      
      subroutine interp2d(xo,yo,zo,dist,x,y,distmax,z,zerr)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 031017           INTERP2D
c               K. Morrison, Earth Tech
c
c
c PURPOSE:     INTERP2D handles 2-dimensional interpolation for discrete
c              receptors.  If the input data is a rectangular grid, the 
c              interpolation is true 2-d regardless of the actual grid
c              orientation.  If the grid is not truly rectangular, the 
c              potential error indicates half the difference between
c              the 2-d estimates.  
c
c --- Updates
c
c ARGUMENTS:
c    PASSED:  
c        XO      - real array    - x-values for the 4 known vertices
c        YO      - real array    - y-values for the 4 known vertices
c        ZO      - real array    - z-values for the 4 known vertices
c        DIST    - real array    - distances from the desired point to
c                                  the 4 known vertices
c        X       - real scalar   - x-value for the desired point
c        Y       - real scalar   - y-value for the desired point
c        DISTMAX - real scalar   - maximum allowed distance for 
c                                  interpolation
c
c  RETURNED:  
c        Z    - real scalar   - estimated z-value for the desired point
c        ZERR - real scalar   - potential error in the estimated z-value 
c
c CALLING ROUTINES:   XYOUTPUT, CELLFILL
c
c EXTERNAL ROUTINES:  NONE
c-----------------------------------------------------------------------
      real xo(4),yo(4),zo(4),dist(4),side(4),ango(4),
     &     angp(4),pdist(4),ztemp(4)
      integer indx(5)/5*0/
      pi=abs(atan2(0.,-1.))
c      
c     calculate all distances and angles from the point to the vertices
c
      do i=1,4
        dx=x-xo(i)
        dy=y-yo(i)
        angp(i)=0.
        if(dx.ne.0..or.dy.ne.0.) angp(i)=atan2(dy,dx)
      enddo
c      
c     check if the point is at one of the vertices
c
      if(minval(dist).le.1.e-3) then
        do i=1,4
          if(dist(i).eq.minval(dist)) then
            z=zo(i)
            zerr=0.
            return
          endif
        enddo
      endif
c
c     check if one of the vertices is either missing or too far away,
c     and if so, return a missing value
c
      if(minval(zo).lt.-998. .or. maxval(dist).gt.distmax) then
        z=-999.
        zerr=0.
        return
      endif
c
c     check the order of the vertices, to start at "lowleft" and go 
c     around counter-clockwise
c
      ymn=minval(yo)
      xmn=1.e15
      imn=1
      do i=1,4
        if(yo(i).eq.ymn.and.xo(i).lt.xmn) then
          xmn=xo(i)
          imn=i
        endif
      enddo
      indx(1)=imn
      angomn=1.e15
      angomx=-1.e15
      do i=1,4
        if(i.eq.indx(1)) cycle
        ango(i)=atan2(yo(i)-yo(indx(1)),xo(i)-xo(indx(1))
     &         +1.e-30)
        if(ango(i).lt.(-pi/2.)) ango(i)=ango(i)+(2.*pi) 
        if(ango(i).lt.angomn) then
          angomn=ango(i)
          indx(2)=i
        endif
        if(ango(i).gt.angomx) then
          angomx=ango(i)
          indx(4)=i
        endif
      enddo
      indx(3)=10-(indx(1)+indx(2)+indx(4))
      indx(5)=indx(1)
c      
c     calculate the lengths and angles of the sides of the observed simplex
c
      do i=1,4
        dxs=xo(indx(i))-xo(indx(i+1))
        dys=yo(indx(i))-yo(indx(i+1))
        side(i)=sqrt(dxs**2+dys**2)
        if(dys.ne.0..or.dxs.ne.0.) then 
          ango(i)=atan2(dys,dxs)
        else
          ango(i)=0.
        endif
      enddo
c
c     do linear interpolation on all 4 sides of the simplex, checking
c     if the point is actually on a side
c
      do i=1,4
        angr=angp(indx(i))-ango(i)
        ds=abs(dist(indx(i))*cos(angr))
        pdist(i)=abs(dist(indx(i))*sin(angr))
        ztemp(i)=(ds*zo(indx(i+1))+(side(i)-ds)*zo(indx(i)))
     &        /side(i)
        if(pdist(i).lt.1.e-3) then
          z=ztemp(i)
          zerr=0.
          return
        endif
      enddo
c
c     interpolate in from the 4 sides
c
      z1=(ztemp(1)*pdist(3)+ztemp(3)*pdist(1))/(pdist(1)+pdist(3))
      z2=(ztemp(2)*pdist(4)+ztemp(4)*pdist(2))/(pdist(2)+pdist(4))
c
c     final interpolation based on the two estimates
c
      z=(z1*(pdist(2)+pdist(4))+z2*(pdist(1)+pdist(3)))/
     & (pdist(1)+pdist(2)+pdist(3)+pdist(4))
      zerr=abs(z1-z2)/2.
c
c     finished
c
      return
      end
c-----------------------------------------------------------------------
      subroutine cellfill
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 050526          CELLFILL
c               K. Morrison, Earth Tech
c
c PURPOSE:     CELLFILL fills in void cells by interpolating from the
c              nearest grid cells that have data.  This will normally
c              be carried out in a continuation run using GTOPO30 data
c              after the higher resolution data has been exploited.
c
c --- Updates
c         Ver. 3.51 Level 040228 to Ver. 3.64 Level 050526
c              - Change filling of void cells on outer boundries to
c                Nearest Neighbour
c         Ver. 3.5 Level 031031 to Ver. 3.51 Level 040228
c              - Change for explicit search radius from input file
c
c ARGUMENTS:
c    PASSED:  /GRID/    output grid specification
c             /CELL/    stored data for cell and search radius 
c
c  RETURNED:  /CELL/    stored data for cell
c                       SUM array is always in meters
c                       HTMAX array is always in meters
c
c
c CALLING ROUTINES:   TERREL
c
c EXTERNAL ROUTINES:  INTERP2D
c-----------------------------------------------------------------------
c --- Include files of parameters and commons
      include 'params.trl'
      include 'cell.trl'
      include 'grid.trl'
      real xx(4),yy(4),zza(4),zzmx(4),x,y,z,zerr,dist(4)
      logical lvoid/.false./
      integer nvoids/0/

c --- Set the search range in numbers of cells
      cellradkm=max(cellradkm,sizek)
      numcell=max(int(cellradkm/sizek+0.1),1)

c --- First, fill the outer rows/columns with averages
c     of valid cell elevations and with maximum valid heights
c     but reset the counts to -1 for the outer cells so that the
c     estimated values are not re-used for adjacent outer cells
c
c     Changed 050526 to Nearest Neighbour with ties averaged
c
c     Top and bottom rows
c
      do ixtop=1,nx
        ixbottom = ixtop + (ny-1)*nx
        if(knt(ixtop).eq.0) then
          sum(ixtop)=0.
          htmax(ixtop)=0.
          odist=1.e32
          ixstart=max(ixtop-numcell,1)
          ixend=min(ixtop+numcell,nx)
          do ixind=ixstart,ixend
            do iyind=1,numcell
              cdist=sqrt(float((ixind-ixtop)**2+(iyind-1)**2))
              i=ixind + (iyind-1)*nx 
              if(knt(i).gt.0) then
                if(cdist.le.odist) then
                  if(cdist.lt.odist) then
                    odist=cdist
                    sum(ixtop)=sum(i)/real(knt(i))
                    knt(ixtop)=1
                    htmax(ixtop)=htmax(i)
                  else
                    sum(ixtop) = sum(ixtop) + sum(i)/real(knt(i))
                    knt(ixtop) = knt(ixtop) + 1
                    htmax(ixtop)=max(htmax(ixtop),htmax(i))
                  endif
                endif
              endif
            enddo
          enddo
          if(knt(ixtop).gt.0) then
            sum(ixtop)=sum(ixtop)/real(knt(ixtop))
            knt(ixtop)=-1
            nvoids=nvoids+1
          endif
        endif
        if(knt(ixbottom).eq.0) then
          sum(ixbottom)=0.
          htmax(ixbottom)=0.
          odist=1.e32
          ixstart=max(ixtop-numcell,1)
          ixend=min(ixtop+numcell,nx)
          do ixind=ixstart,ixend
            do iyind=ny-numcell+1,ny
              cdist=sqrt(float((ixind-ixtop)**2+(iyind-ny)**2))
              i=ixind + (iyind-1)*nx 
              if(knt(i).gt.0) then
                if(cdist.le.odist) then
                  if(cdist.lt.odist) then
                    odist=cdist
                    sum(ixbottom)=sum(i)/real(knt(i))
                    knt(ixbottom)=1
                    htmax(ixbottom)=htmax(i)
                  else
                    sum(ixbottom) = sum(ixbottom) + sum(i)/real(knt(i))
                    knt(ixbottom) = knt(ixbottom) + 1
                    htmax(ixbottom)=max(htmax(ixbottom),htmax(i))
                  endif
                endif
              endif
            enddo
          enddo
          if(knt(ixbottom).gt.0) then
            sum(ixbottom)=sum(ixbottom)/real(knt(ixbottom))
            knt(ixbottom)=-1
            nvoids=nvoids+1
          endif
        endif
      enddo
c
c     Left and right columns
c
      do iy=1,ny
        iyleft=1 + (iy-1)*nx
        iyright=iy*nx
        if(knt(iyleft).eq.0) then
          sum(iyleft)=0.
          htmax(iyleft)=0.
          odist=1.e32
          iystart=max(iy-numcell,1)
          iyend=min(iy+numcell,ny)
          do iyind=iystart,iyend
            do ixind=1,numcell
              i=ixind + (iyind-1)*nx 
              cdist=sqrt(float((ixind-1)**2+(iyind-iy)**2))
              if(knt(i).gt.0) then
                if(cdist.le.odist) then
                  if(cdist.lt.odist) then
                    odist=cdist
                    sum(iyleft)=sum(i)/real(knt(i))
                    knt(iyleft)=1
                    htmax(iyleft)=htmax(i)
                  else
                    sum(iyleft) = sum(iyleft) + sum(i)/real(knt(i))
                    knt(iyleft) = knt(iyleft) + 1
                    htmax(iyleft)=max(htmax(iyleft),htmax(i))
                  endif
                endif
              endif
            enddo
          enddo
          if(knt(iyleft).gt.0) then
            sum(iyleft)=sum(iyleft)/real(knt(iyleft))
            knt(iyleft)=-1
            nvoids=nvoids+1
          endif
        endif
        if(knt(iyright).eq.0) then
          sum(iyright)=0.
          htmax(iyright)=0.
          odist=1.e32
          iystart=max(iy-numcell,1)
          iyend=min(iy+numcell,ny)
          do iyind=iystart,iyend
            do ixind=nx-numcell+1,nx
              i=ixind + (iyind-1)*nx 
              cdist=sqrt(float((ixind-nx)**2+(iyind-iy)**2))
              if(knt(i).gt.0) then
                if(cdist.le.odist) then
                  if(cdist.lt.odist) then
                    odist=cdist
                    sum(iyright)=sum(i)/real(knt(i))
                    knt(iyright)=1
                    htmax(iyright)=htmax(i)
                  else
                    sum(iyright) = sum(iyright) + sum(i)/real(knt(i))
                    knt(iyright) = knt(iyright) + 1
                    htmax(iyright)=max(htmax(iyright),htmax(i))
                  endif
                endif
              endif
            enddo
          enddo
          if(knt(iyright).gt.0) then
            sum(iyright)=sum(iyright)/real(knt(iyright))
            knt(iyright)=-1
            nvoids=nvoids+1
          endif
        endif
      enddo
c
c --- Change the counts on the filled outer cells to 1 so that they can
c     be used to fill the inner cells
c
      do ixtop=1,nx
        ixbottom=ixtop+(ny-1)*nx
        if(knt(ixtop).eq.-1) knt(ixtop)=1
        if(knt(ixbottom).eq.-1) knt(ixbottom)=1
      enddo
      do iy=2,ny-1
        iyleft=1+(iy-1)*nx
        iyright=iy*nx
        if(knt(iyleft).eq.-1) knt(iyleft)=1
        if(knt(iyright).eq.-1) knt(iyright)=1
      enddo
c
c --- Now find the nearest valid cells for each missing cell and
c     carry out the interpolation, but set the counts to -1 so that
c     estimated cells other than outside cells are not re-used to
c     estimate other cells
c
      do iii=1,nx
        do jjj=1,ny
          i1=iii + (jjj-1)*nx
c
c         Check if this cell is void, and initialize the search
c         for good values
c
          if(knt(i1).eq.0) then
            iystart=max(jjj-numcell,1)
            iyend=min(jjj+numcell,ny)
            ixstart=max(iii-numcell,1)
            ixend=min(iii+numcell,nx)
            x=real(iii)
            y=real(jjj)
            xx(1)=real(ixstart-1)
            xx(2)=real(ixend+1)
            xx(3)=xx(2)
            xx(4)=xx(1)
            yy(1)=real(iystart-1)
            yy(2)=yy(1)
            yy(3)=real(iyend+1)
            yy(4)=yy(3)
            do idd=1,4
              dist(idd)=sqrt((x-xx(idd))**2+(y-yy(idd))**2)
              zza(idd)=-999.
              zzmx(idd)=-999.
            enddo
c
c           Scan the surrounding cells for valid values
c
            do iyind=iystart,iyend
              do ixind=ixstart,ixend
                if(jjj.eq.iyind.and.iii.eq.ixind) cycle
                distxy=sqrt(real((iii-ixind)**2)+real((jjj-iyind)**2))
                i=ixind + (iyind-1)*nx
                if(knt(i).gt.0) then
                  distxy=sqrt(real((iii-ixind)**2)+real((jjj-iyind)**2))
                  if(ixind.lt.iii.or.(ixind.eq.1.and.iii.eq.1)) then
c
c                   Upper left
c
                    if((iyind.gt.jjj.or.(iyind.eq.ny.and.jjj.eq.ny))
     &                 .and.distxy.lt.dist(4)) then
                      xx(4)=real(ixind)
                      yy(4)=real(iyind)
                      zza(4)=sum(i)/real(knt(i))
                      zzmx(4)=htmax(i)
                      dist(4)=distxy
c
c                   Lower left
c
                    elseif(iyind.le.jjj.and.jjj.ne.ny.and.distxy.lt.
     &                 dist(1)) then
                      xx(1)=real(ixind)
                      yy(1)=real(iyind)
                      zza(1)=sum(i)/real(knt(i))
                      zzmx(1)=htmax(i)
                      dist(1)=distxy
                    endif
                  elseif(ixind.ge.iii.and.iii.ne.nx) then
c
c                   Upper right
c
                    if((iyind.gt.jjj.or.(iyind.eq.ny.and.jjj.eq.ny))
     &                 .and.distxy.lt.dist(3)) then
                      xx(3)=real(ixind)
                      yy(3)=real(iyind)
                      zza(3)=sum(i)/real(knt(i))
                      zzmx(3)=htmax(i)
                      dist(3)=distxy
c
c                   Lower right
c
                    elseif(iyind.le.jjj.and.jjj.ne.ny.and.distxy.lt.
     &                 dist(2)) then
                      xx(2)=real(ixind)
                      yy(2)=real(iyind)
                      zza(2)=sum(i)/real(knt(i))
                      zzmx(2)=htmax(i)
                      dist(2)=distxy
                    endif
                  endif
                endif
              enddo
            enddo
c
c           Interpolate the average and maximum heights
c
            call interp2d(xx,yy,zza,dist,x,y,cellradkm,z,zerr)
            sum(i1)=z
            call interp2d(xx,yy,zzmx,dist,x,y,cellradkm,z,zerr)
            htmax(i1)=z
c
c           Check that the void was actually filled
c
            if(z.gt.-998.) then
              knt(i1)=-1
              nvoids=nvoids+1
            endif

          endif
        enddo
      enddo
c
c --- Change the count of the filled cells to 1, and check that
c     all cells were filled
c 
      do iii=1,nx
        do jjj=1,ny
          i=iii+(jjj-1)*nx
          if(knt(i).lt.0) knt(i)=1
          if(knt(i).eq.0) lvoid=.TRUE.
        enddo
      enddo
      write(iolst,*)
      write(iolst,*) 'CELLFILL '
      write(iolst,*) nvoids,' cells were filled'
      write(iolst,*)
      if(lvoid) then
        write(iolst,*)
        write(iolst,*) 'CELLFILL - WARNING'
        write(iolst,*) 'Not all cells were filled'
        write(iolst,*) 'Do a continuation run using the appropriate'
        write(iolst,*) 'GTOPO30 file(s) and re-select filling'
        write(iolst,*)
        write(*,*)
        write(*,*) 'CELLFILL - WARNING'
        write(*,*) 'Not all cells were filled'
        write(*,*) 'Do a continuation run using the appropriate'
        write(*,*) 'GTOPO30 file(s) and re-select filling'
        write(*,*)
      endif    

      return
      end

c-----------------------------------------------------------------------
      subroutine settrano(cmapi,iutmzni,xlat1i,xlat2i,rlati,rloni,
     &  feasti,fnorti,iutmzno,tmsone,xlat1,xlat2,rlat,rlon,
     &  feast,fnorth,caction,vecti,vecto)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684          Level: 051201          SETTRANO
c               K. Morrison, Earth Tech
c
c PURPOSE:     SETTRANO consolidates the calls for the translation
c              vectors for output that were previously in COMP.
c
c Updates:
c         Version 3.68 Level 051201 from Version 3.67 level 041013
c           - Add Albers Conical Equal Area as input projection
c             (add XLAT1I and XLAT2I to inputs) for consitency with
c             CTGPROC Ver. 2.65 Lev. 051201 
c           - Change to GLOBE1 in CALUTILS and COORDS in COORDLIB
c             to support Albers
c         Version 3.61 Level 041013 from Version 3.6 Level 031031
c           - Change output UTM zone variable name to IUTMZNO to avoid 
c             confusion with unsigned zone variable name (IZONE)
c
c --- INPUTS:
c         INPUT PROJECTION
c            CMAPI - char*8     - Map projection of input coordinates
c                                  LL  : N.Lat., E.Long.
c                                  UTM : Universal Transverse Mercator
c                                  TM  : Transverse Mercator
c                                  LCC : Lambert Conformal Conic
c                                  PS  : Polar Stereographic
c                                  EM  : Equatorial Mercator
c                                  LAZA: Lambert Azimuthal Equal Area
c          IUTMZNI - integer    - UTM zone of input coords.
c                                  (S. hemisphere is NEGATIVE)
c           XLAT1I - real       - Matching Equator-ward N.Latitude
c           XLAT2I - real       - Matching Pole-ward N.Latitude
c            RLATI - real       - Map origin N.Latitude
c            RLONI - real       - Map origin E.Longitude
c           FEASTI - real       - False Easting (km) at proj. origin
c           FNORTI - real       - False Northing (km) at proj. origin
c         OUTPUT PROJECTION
c          IUTMZNO - integer    - UTM zone of output coords.
c                                  (S. hemisphere is NEGATIVE)
c           TMSONE - real       - Scale Factor for TM projection
c            XLAT1 - real       - Matching Equator-ward N.Latitude
c            XLAT2 - real       - Matching Pole-ward N.Latitude
c             RLAT - real       - Map origin N.Latitude
c             RLON - real       - Map origin E.Longitude
c            FEAST - real       - False Easting (km) at proj. origin
c           FNORTH - real       - False Northing (km) at proj. origin
c            /CONTROL/          - program control logicals
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
c --- SETTRANO called by: RDUSGSHD, CORNERS, LOADDMDF, LOADNZGNR,
C                         LOADGNR, LOADLAZA, LOADOTHER, COASTEXT
c --- SETTRANO calls:     GLOBE1
c----------------------------------------------------------------------
c
c --- Control common 
      include 'control.trl'

      character*8 cmapi
      character*12 caction
      real*8 vecti(9),vecto(9)

c --- Set translation vectors to the output grid

      if(lutm) then
c ---    Using UTM grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'UTM     ',iutmzno,xdum,xdum,xdum,rdum,rdum,
     &               feast,fnorth,
     &               caction,vecti,vecto)

      elseif(llcc) then
c ---    Using Lambert Conformal grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'LCC     ',idum,xdum,xlat1,xlat2,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

      elseif(lps) then
c ---    Using Polar Stereographic grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'PS      ',idum,xdum,xlat1,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

      elseif(lem) then
c ---    Using Equatorial Mercator grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'EM      ',idum,xdum,xlat1,-xlat1,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

      elseif(llaza) then
c ---    Using Lambert Azimuthal Equal Area grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'LAZA    ',idum,xdum,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

      elseif(lttm) then
c ---    Using Tangential TM grid
         call GLOBE1(cmapi,iutmzni,xdum,xlat1i,xlat2i,rlati,rloni,
     &               feasti,fnorti,
     &               'TM      ',idum,tmsone,xdum,xdum,rlat,rlon,
     &               feast,fnorth,
     &               caction,vecti,vecto)

      endif
      return
      end
c-----------------------------------------------------------------------
      subroutine coastrd
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684           Level: 050817           COASTRD 
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     COASTRD reads already-extracted polygons of water/land
c              interfaces from an existing BLN file, coming either
c              from a previous TERREL run or a previous CTGPROC run,
c              passing these into the SHORES common for subsequent
c              use to evaluate crude type for points and grid cells.
c              The types are: 0-ocean, 1-mainland and marine islands,
c              2-lakes, 3-islands in lakes, 4-ponds on islands. 
c
c --- Updates
c --- Ver 3.59 Level 040901 to Ver 3.66 Level 050817  KAM
c              - Remove compiler warning about IISIDE
c              - Modify error message in case headers are incomplete
c                (manually-produced BLN file)
c --- Ver 3.56 Level 040311 to Ver 3.59 Level 040901  KAM
c              - Relax testing of BLN limits to be more realistic 
c --- Ver 3.54 Level 040303 to Ver 3.56 Level 040311  KAM
c              - Add polygon source and id to BLN identifier field
c              - Change name of BLN read logical
c --- Ver 3.53 Level 040302 to Ver 3.54 Level 040303  KAM
c              - Close the BLN file before exiting
c
c ARGUMENTS:
c    PASSED:  /FILNAM/    file names
c             /GRID/      grid min-max
c            
c  RETURNED:  /SHORES/    shoreline data
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include parameter and common files
c
      include 'params.trl'
      include 'shores.trl'
      include 'filnam.trl'
      include 'grid.trl'
c
c --- Local variables
c
      character*10 cptype
      integer np,iiside,iend
c
c --- Open the BLN file
c
      open(unit=iobln,file=coastbln)
c
c --- Loop through the polygons
c
      nshore=0
      iend=0
      do
c
c ---   Read the header for the polygon and decode polygon type
c
        read(iobln,*,end=99) np,iiside,cptype
        nshore=nshore+1
        nspts(nshore)=np
        istart(nshore)=iend+1
        iend=iend+np
        read(cptype,*) itypes(nshore),isourcep(nshore),ipolyid(nshore)
c
c ---   Read the points for the polygon into the arrays
c
        do i=istart(nshore),iend
          read(iobln,*) xp(i),yp(i)
          pxmax(nshore)=max(pxmax(nshore),xp(i))
          pymax(nshore)=max(pymax(nshore),yp(i))
          pxmin(nshore)=min(pxmin(nshore),xp(i))
          pymin(nshore)=min(pymin(nshore),yp(i))
        enddo
c
      enddo
c
c --- EOF - Check that data were actually found
c
99    if(nshore.EQ.0) then
        write(*,*) ' No coasts found in ',coastbln
        write(*,*) ' Check path, and if this is really a continuation.'
        write(*,*) ' If file exists, check headers to polygons.  They '
        write(*,*) ' must contain (include commas & double quotes):'
        write(*,*) ' number_of_points_in_polygon,1,"1,2,999"'
        stop
      endif
c
c --- Check that polygons are within the grid - otherwise
c     the BLN file is not for this grid, so terminate
c
      xxmxx=-1.e30
      xxmnn=1.e30
      yymxx=-1.e30
      yymnn=1.e30
      do i=1,nshore
        xxmxx=max(xxmxx,pxmax(i))
        xxmnn=min(xxmnn,pxmin(i))
        yymxx=max(yymxx,pymax(i))
        yymnn=min(yymnn,pymin(i))
      enddo
      if(xxmxx.LT.xllk .OR. xxmnn.GT.xurk .OR.
     *   yymxx.LT.yllk .OR. yymnn.GT.yurk) then
         write(*,*) ' Data limits in BLN file are not consistent with',
     *              ' the grid limits'
         write(*,*) ' Change LBLNREAD to .FALSE. and re-run'
         stop
      endif

      close(iobln)
c     Remove compiler warning
      iiside=iiside
      return
      end
c-----------------------------------------------------------------------      
      integer function iptype(px,py)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684           Level: 040228            IPTYPE
c               K. Morrison, Earth Tech
c
c
c PURPOSE:     IPTYPE checks an individual point to see if it falls
c              within polygons extracted from the GSHHS full-resolution
c              data set.  The polygons are hierarchical - i.e. nested.
c              Therefore, the highest value for the type of any
c              polygon containing the point is the value for that
c              point.  IPTYPE returns one of five values:
c              0 - ocean (default value - there are no ocean polygons)
c              1 - mainland or marine islands
c              2 - lakes
c              3 - islands within lakes
c              4 - ponds on islands
c              Note that a default value of 0 is returned if the point
c              doesn't fall within a polygon, unless there are no
c              polygons in which case a value of 1 is returned.
c
c --- Updates
c         Ver. 3.51 Level 040228 to Ver. 3.52 Level 040229
c              - Remove defaulting to first land type since COASTEXT now
c                explicitly defines the surrounding polygon if the zone
c                is surrounded without a coastline intersection
c              - Add defaulting to land type if no shorelines were
c                processed or found
c
c ARGUMENTS:
c    PASSED:  PX, PY real    - X-Y coordinates of the point
c             /SHORES/       - shoreline data extracted from GSHHS
c
c  RETURNED:  IPTYPE integer - water or land crude type, 0-4
c
c
c CALLING ROUTINES:   TERREL
c
c EXTERNAL ROUTINES:  PNPOLY
c-----------------------------------------------------------------------
c      
c --- Include common
c
      include 'params.trl'
      include 'control.trl'
      include 'shores.trl'
c
c --- Default type to ocean, unless no shoreline processing done in
c     which case default to land
c
      iptype=0
      if(.NOT.lcoast) then
        iptype=1
        return
      endif
c      
c --- Loop through all polygons within the study zone
c
      do i=1,nshore
c        
c   --- First, skip any polygons that can't contain the point or that
c       won't change the type of point
c
        if((px.lt.pxmin(i).or.px.gt.pxmax(i).or.py.lt.pymin(i).or.        
     *    py.gt.pymax(i)).or.iptype.ge.itypes(i)) cycle   
c     
c   --- Now check if the point falls within this ploygon
c
        call pnpoly(px,py,xp(istart(i)),yp(istart(i)),nspts(i),inout)
c
c       Get polygon type
c
c   --- Outside of polygon - no change        
c
        if(inout.eq.-1) cycle
c
c   --- On edge of polygon - set to water
c
        if(inout.eq.0) then
          if(itypes(i).eq.1) iptype=0
          if(itypes(i).eq.2) iptype=2
          if(itypes(i).eq.3) iptype=2
          if(itypes(i).eq.4) iptype=4
c        
c   --- Inside of polygon - retain max of current type and polygon type
c
        else
          iptype=max(iptype,itypes(i))
        endif
c
c --- End of loops over polygons
c
      enddo
      return
      end
c-----------------------------------------------------------------------
      SUBROUTINE PNPOLY (PX,PY,X,Y,N,INOUT)
c-----------------------------------------------------------------------
C
C     Copied from ISCST3
c-----------------------------------------------------------------------
C
C     Courtesy: Jay Sandhu
C               email: jsandhu@esri.com
C
C
C Please cite David H. Douglas, COLLECTED ALGORITHMS, Cambridge MA:
C Harvard Laboratory for Computer Graphics, 1974
C
C This is my reinvention buster.
C 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974
C
C>>>PNPY
C     .................................................................
C
C----------------------------------------------------------------------
C
C        SUBROUTINE PNPOLY
C
C        PURPOSE
C           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
C
C        USAGE
C           CALL PNPOLY (PX, PY, X, Y, N, INOUT )
C
C        DESCRIPTION OF THE PARAMETERS
C           PX      - X-COORDINATE OF POINT IN QUESTION.
C           PY      - Y-COORDINATE OF POINT IN QUESTION.
C           X       - N LONG VECTOR CONTAINING X-COORDINATES OF
C                     VERTICES OF POLYGON.
C           Y       - N LONG VECTOR CONTAINING Y-COORDINATES OF
C                     VERTICES OF POLYGON.
C           N       - NUMBER OF VERTICES IN THE POLYGON.
C           INOUT   - THE SIGNAL RETURNED:
C                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
C                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
C                      1 IF THE POINT IS INSIDE OF THE POLYGON.
C
C        REMARKS
C           THE VERTICES MAY BE LISTED IN CLOCKWISE OR ANTICLOCKWISE
C           ORDER.  FOR THIS SUBROUTINE A POINT IS CONSIDERED INSIDE
C           THE POLYGON IF IT IS LOCATED IN THE ENCLOSED AREA DEFINED
C           BY THE LINE FORMING THE POLYGON.
C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
C           PNPOLY CAN HANDLE ANY NUMBER OF VERTICES IN THE POLYGON.
C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 6/72.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           A VERTICAL SEMI-INFINITE LINE IS DRAWN UP FROM THE POINT
C           IN QUESTION. IF IT CROSSES THE POLYGON AN ODD NUMBER OF
C           TIMES, THE POINT IS INSIDE THE POLYGON.
C
C     .................................................................
C

      REAL X(N),Y(N)
      LOGICAL IX,IY,JX,JY,EOR

C     EXCLUSIVE OR FUNCTION.
      EOR(IX,IY)=(IX.OR.IY).AND..NOT.(IX.AND.IY)

      INOUT=-1
      DO 4 I=1,N
         XI=X(I)-PX
         YI=Y(I)-PY
C        CHECK WHETHER THE POINT IN QUESTION IS AT THIS VERTEX.
         IF (XI.EQ.0.0.AND.YI.EQ.0.0) GO TO 2
C        J IS NEXT VERTEX NUMBER OF POLYGON.
         J=1+MOD(I,N)
         XJ=X(J)-PX
         YJ=Y(J)-PY
C        IS THIS LINE OF 0 LENGTH ?
         IF (XI.EQ.XJ.AND.YI.EQ.YJ) GO TO 4
         IX=XI.GE.0.0
         IY=YI.GE.0.0
         JX=XJ.GE.0.0
         JY=YJ.GE.0.0
C        CHECK WHETHER (PX,PY) IS ON VERTICAL SIDE OF POLYGON.
         IF (XI.EQ.0.0.AND.XJ.EQ.0.0.AND.EOR(IY,JY)) GO TO 2
C        CHECK WHETHER (PX,PY) IS ON HORIZONTAL SIDE OF POLYGON.
         IF (YI.EQ.0.0.AND.YJ.EQ.0.0.AND.EOR(IX,JX)) GO TO 2
C        CHECK WHETHER BOTH ENDS OF THIS SIDE ARE COMPLETELY 1) TO RIGHT
C        OF, 2) TO LEFT OF, OR 3) BELOW (PX,PY).
         IF (.NOT.((IY.OR.JY).AND.EOR(IX,JX))) GO TO 4
C        DOES THIS SIDE OBVIOUSLY CROSS LINE RISING VERTICALLY FROM (PX,PY
         IF (.NOT.(IY.AND.JY.AND.EOR(IX,JX))) GO TO 1
         INOUT=-INOUT
         GO TO 4

1        IF ((YI*XJ-XI*YJ)/(XJ-XI)) 4,2,3
2        INOUT=0
         RETURN
3        INOUT=-INOUT
4     CONTINUE

      RETURN
      END
c      
c-----------------------------------------------------------------------
      subroutine breadh(il1,il2,il3,il4,il5,il6,il7,il8,is1,is2,
     &  iogshhs,leof,lbigendian)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684           Level: 041013            BREADH
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     BREADH reads the binary header of a GSHHS polygon and
c              returns the information.  If the current machine is
c              "LITTLE-ENDIAN", it carries out byte-switching to
c              transform the data from the "BIG-ENDIAN" format. 
c
c --- Updates
c        Version 3.61 Level 041013 from Version 3.60 Level 040228
c           - Add variable il9 to test for version of GSHHS file, its
c             value will be 3 for most the recent GSHHS Version 1.3 (040928).
c             The change is limited to this subroutine
c           - Change eof test from count to err condition
c
c ARGUMENTS:
c    PASSED:  lbigendian   - logical flag to indicate if current machine
c                            is "BIG-ENDIAN" or not
c             iogshhs      - unit number for GSHHS file
c
c
c  RETURNED:  il1-il8      - the 8 long (4-byte) integers from the header
c             is1-is2      - the 2 short (2-byte) integers from the header
c             leof         - logical flag indicating end of file
c
c CALLING ROUTINES:   COASTEXT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Local variables
c      
      integer il1,il2,il3,il4,il5,il6,il7,il8,il9,ill1,ill2,ill3,ill4,
     * ill5,ill6,ill7,ill8,ill9
      integer*2 is1,is2,ils1,ils2
      integer*1 isl1(4),isl2(4),isl3(4),isl4(4),isl5(4),isl6(4),
     * isl7(4),isl8(4),isl9(4),iss1(2),iss2(2)
      logical leof,lbigendian,lver3
      data lver3/.true./
      integer ilast/-1/
c      
c     Equivalences to allow byte-by-byte access
c
      equivalence (ils1,iss1(1)),(ils2,iss2(1)),(ill1,isl1(1)),
     * (ill2,isl2(1)),(ill3,isl3(1)),(ill4,isl4(1)),(ill5,isl5(1)),
     * (ill6,isl6(1)),(ill7,isl7(1)),(ill8,isl8(1)),(ill9,isl9(1))
c      
c     If current machine is big-endian, read the data directly.
c     If not (eg. Intel & DEC), read in byte-by-byte to reverse order
c     and copy values to return arguments.
c
1     if(lbigendian) then
c
c       For reads, check the version number of the GSHHS file, and change
c       the read of the variables in the header if the version is not 3.
c
        if(lver3) then
          read(iogshhs,err=99) il1,il2,il3,il4,il5,il6,il7,il8,
     *      il9,is1,is2
        else
          read(iogshhs,err=99) il1,il2,il3,il4,il5,il6,il7,il8,
     *      is1,is2
        endif  
      else
        if(lver3) then
          read(iogshhs,err=99) isl1(4),isl1(3),isl1(2),isl1(1),
     *      isl2(4),isl2(3),isl2(2),isl2(1),
     *      isl3(4),isl3(3),isl3(2),isl3(1),
     *      isl4(4),isl4(3),isl4(2),isl4(1),
     *      isl5(4),isl5(3),isl5(2),isl5(1),
     *      isl6(4),isl6(3),isl6(2),isl6(1),
     *      isl7(4),isl7(3),isl7(2),isl7(1),
     *      isl8(4),isl8(3),isl8(2),isl8(1),
     *      isl9(4),isl9(3),isl9(2),isl9(1),
     *      iss1(2),iss1(1), 
     *      iss2(2),iss2(1) 
        else
          read(iogshhs,err=99) isl1(4),isl1(3),isl1(2),isl1(1),
     *      isl2(4),isl2(3),isl2(2),isl2(1),
     *      isl3(4),isl3(3),isl3(2),isl3(1),
     *      isl4(4),isl4(3),isl4(2),isl4(1),
     *      isl5(4),isl5(3),isl5(2),isl5(1),
     *      isl6(4),isl6(3),isl6(2),isl6(1),
     *      isl7(4),isl7(3),isl7(2),isl7(1),
     *      isl8(4),isl8(3),isl8(2),isl8(1),
     *      iss1(2),iss1(1), 
     *      iss2(2),iss2(1) 
        endif
c       Copy values to dummy arguments
        il1=ill1
        il2=ill2
        il3=ill3
        il4=ill4
        il5=ill5
        il6=ill6
        il7=ill7
        il8=ill8
        il9=ill9
        is1=ils1
        is2=ils2
      endif
      ilast=il1
c
c     Test the GSHHS version number - for Version 1.3, il9 must equal 3
c
      if(il9.ne.3) then
        lver3=.false.
        il9=3
        ill9=3
        write(*,55)
55      format(/' Note : GSHHS version is obsolete - update ASAP'//)
        rewind (unit=iogshhs)
        goto 1
      endif
      return
c
c     Test if this is the last polygon or if there is actually an EOF
c     before any polygons have been read - in this case the filename is
c     wrong
c
99    leof=.true.
      if(ilast.lt.0) then
        write(*,*) ' GSHHS file not found or invalid '
        write(*,*) '  - check path and file name'
        stop
      endif
      return
      end
c      
c-----------------------------------------------------------------------
      subroutine breadp(xp1,yp1,lbigendian,iogshhs)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684           Level: 040228            BREADP
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     BREADH reads the binary x-y values of a point of a GSHHS
c              polygon, scales the results, and returns the information.
c              If the current machine is "LITTLE-ENDIAN", it carries 
c              out byte-switching to transform the data from the 
c              "BIG-ENDIAN" format prior to scaling. 
c
c --- Updates
c
c ARGUMENTS:
c    PASSED:  lbigendian   - logical flag to indicate if current machine
c                            is "BIG-ENDIAN" or not
c             iogshhs      - unit number for GSHHS file
c
c
c  RETURNED:  xp1, yp1     - the latitude and longitude of the point in
c                            decimal degrees
c
c CALLING ROUTINES:   COASTEXT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Local variables
c      
      logical lbigendian
      real xp1,yp1
      integer il1,il2
      integer*1 isl1(4),isl2(4)
c      
c     Equivalences to allow byte-by-byte access
c
      equivalence (il1,isl1(1)),(il2,isl2(1))
c      
c     If current machine is big-endian, read the data directly.
c     If not (INTEL&DEC), read in byte-by-byte to reverse order.
c
      if(lbigendian) then
        read(iogshhs) il1,il2
      else
        read(iogshhs) isl1(4),isl1(3),isl1(2),isl1(1),
     *    isl2(4),isl2(3),isl2(2),isl2(1)
      endif
c
c     Covert from millionths to whole degrees
c
      xp1=real(il1)/1.e6
      yp1=real(il2)/1.e6
c      
      return
      end
c
c-----------------------------------------------------------------------
      subroutine coastext
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684           Level: 051201          COASTEXT
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     COASTEXT extracts polygons of water/land interfaces
c              within the desired grid area from the GSHHS data set,
c              passing these into the SHORES common for subsequent
c              use to evaluate crude type for points and grid cells.
c              The types are: 0-ocean, 1-mainland and marine islands,
c              2-lakes, 3-islands in lakes, 4-ponds on islands.  It 
c              also outputs a Surfer BLN file for coastline mapping. 
c
c --- Updates
c         Ver. 3.67 Level 050817 to Ver 3.68 Level 051201       KAM
c              - Add additional dummy arguments in calls to SETTRANO to
c                be consistent with CTGPROC V. 2.65 Lev. 051201
c              - Correct test for a surrounding polygon when it
c                is the only polygon (i.e. coastline processing not
c                needed)
c         Ver. 3.63 Level 050428 to Ver. 3.66 Level 050817      KAM
c              - Remove XP1IS declaration - not used
c              - Remove compiler warnings for IDUM1, IDUM2
c         Ver. 3.62 Level 050128 to Ver. 3.63 Level 050428      KAM
c              - Reset negative longitudes where needed, since GLOBE
c                changes input -long into output +long
c         Ver. 3.61 Level 041013 to Ver. 3.62 Level 050128      DGS
c              - Initialize LSURRSIDE as an array
c              - Add array bound checks against MXCOAST, MXCOASTP
c         Ver. 3.60 Level 040920 to Ver. 3.61 Level 041013
c              - Reactivate WDBII, but with empirical fix-up for latitudes,
c                actual use of WDBII polygons is controlled by the 
c                variable LWDBII, currently false
c              - Change polygon loop from fixed number to no index,
c                now exiting on GSHHS EOF
c              - Change EOF in BREADH, and move the loop exit on EOF
c              - Correct UTM out zone in call to SETTRANO
c         Ver. 3.57_draft Level 040429 to Ver. 3.59_draft Level 040901
c              - Corrections to polygon closing
c         Ver. 3.56_draft Level 040311 to Ver. 3.57_draft Level 040429
c              - Disable use of WDBII data
c         Ver. 3.54_draft Level 040303 to Ver. 3.56_draft Level 040311
c              - Change points examination to lat-lon
c              - Add treatment for data and grids near Greenwich
c              - Correct additional outside point for end of segment
c              - Add polygon source and id to BLN identifier field
c         Ver. 3.53_draft Level 040302 to Ver. 3.54_draft Level 040303
c              - Fix bug in surrounding polygon detection
c         Ver. 3.52_draft Level 040229 to Ver. 3.53_draft Level 040302
c              - Move min-max assignment to block data
c         Ver. 3.51_draft Level 040228 to Ver. 3.52_draft Level 040229
c              - Explicitly define the surrounding polygon if the zone
c                is surrounded without a coastline intersection
c
c ARGUMENTS:
c    PASSED:  /CONTROL/   input datums, output projection
c             /GRID/      output datum, grid extremes
c             /FILNAM/    file names
c
c  RETURNED:  /SHORES/    shoreline data
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  SETTRANO, GLOBE, BREADH, BREADP, ISIDE, PCORNER,
c                     LBIG_END
c-----------------------------------------------------------------------
c --- Include parameter and common files
c
      include 'params.trl'
      include 'control.trl'
      include 'shores.trl'
      include 'grid.trl'
      include 'filnam.trl'
      include 'gspan.trl'
c
c --- Local variables
c      
      integer id,npts,itype,ixmin,ixmax,iymin,iymax,iarea
      real uxmax,uxmin,uymax,uymin
      real xp1i,xp1it,xp1,yp1i,yp1it,yp1,xps,yps
      real*8 vecti(9),vecto(9),vectis(9),vectos(9)
      integer*2 igreen,isource
      integer sside,oside,nside,eside
      character*12 caction,cll2ll
      character*4 c4dum
      character*8 cdatum(2)
      logical lgood/.false./,linside/.false./,leof/.false./,lfirst,
     &  lbigendian,lbig_end,lsurrside(4),lstraddleg/.false./,
     &  lwdbii/.false./

c --- Logicals for array bound checks (DGS)
      logical lpnts/.false./, lpoly/.false./

c
      write(*,*)
      write(*,*) ' Processing coastline data'
      write(*,*)
c
c --- Set coast counter to 0, but set first type to ocean in case no
c     polygons are found at all, only possible if the zone is in the
c     ocean with not even any islands
c
      nshore=0
      itypes(1)=0
c
c --- Copy the datums for the 2 data types
c
      cdatum(2)=dwvs
      cdatum(1)=dwdbii
c
c --- Set input dataset false Easting/Northing to zero and TM scale to 1
      feasti=0.0
      fnorti=0.0
      tmsone=1.0

c --- Set utm zone for coordinate calls
      iutm=izone
      if(utmhem.EQ.'S   ' .AND. izone.LT.900) iutm=-iutm

      call SETTRANO('LL      ',iutm,rdum,rdum,rdum,rdum,
     &            feasti,fnorti,
     &            iutm,tmsone,xlat1,xlat2,rlat,rlon,
     &            feast,fnorth,
     &            caction,vecti,vecto)
      call GLOBE1('LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &            feasti,fnorti,
     &            'LL      ',idum,xdum,xdum,xdum,rdum,rdum,
     &            feasti,fnorti,
     &            cll2ll,vectis,vectos)

c
c     expand the user zone by 1% to avoid border problems
c
      dwide=(xurk-xllk)*.01
      dhigh=(yurk-yllk)*.01
      uxmax=xurk+dwide
      uxmin=xllk-dwide
      uymax=yurk+dhigh
      uymin=yllk-dhigh
c
c     Get the east longitudes and latitudes for the grid limits
c
      dwide=dwide/107.9*cos((ylatnw+ylatse)/2.*.01745393)
      dhigh=dhigh/107.9
      xlonww=min(-xlonsw,-xlonnw)-dwide
      if(xlonww.lt.0.) xlonww=xlonww+360.
      xlonee=max(-xlonse,-xlonne)+dwide
      if(xlonee.lt.0.) xlonee=xlonee+360.
      ylatnn=max(ylatnw,ylatne)+dhigh
      ylatss=min(ylatsw,ylatse)-dhigh
      if(xlonww.gt.xlonee) then
        lstraddleg=.true.
        xlonww=xlonww-360.
      endif

c DGS
      write(iolst,*)
      write(iolst,*)'COASTLINE processing from GSHHS data set'
      write(iolst,*)
      write(iolst,*)'Grid Limits --- Datum ',datum
      write(iolst,*)'E. Longitudes:',xlonww,xlonee
      write(iolst,*)'N. Latitudes :',ylatss,ylatnn
      write(iolst,*)
c DGS

c
c     Open the GSHHS full-resolution file
c
      open(unit=iogshhs,file=gshhsin,access=trim(caccess),
     &  form=cform,status='OLD',recl=irecl)
c
c     Check if the current machine is BIG-ENDIAN
c
      lbigendian=lbig_end(1)
c      
c     Set the point counter for the first point
c
      ipp=1
c      
c     Loop over the polygons in the GSHHS file
c
      Polygons: do
c
c           Clear the surround logicals
c
            do kside=1,4
               lsurrside(kside)=.false.
            enddo
            lfirst=.false.
c
c           Read the header for this GSHHS polygon
c
            call breadh(id,npts,itype,ixmin,ixmax,iymin,iymax,
     *      iarea,igreen,isource,iogshhs,leof,lbigendian)
            if(leof) goto 99
c
c           Convert raw data for limits from millionths to whole degrees
c
            shft=0.
            xmaxit=real(ixmax)/1.e6
            if(xmaxit.lt.0.) xmaxit=xmaxit+360.
            xminit=real(ixmin)/1.e6
            if(xminit.lt.0.) xminit=xminit+360.
            ymaxit=real(iymax)/1.e6
            yminit=real(iymin)/1.e6
c            
c           Increment isource to use the proper datum, transform
c           to output datum latitude-longitude, and adjust for -/+
c           shift at Greenwich
c
            isource=isource+1
            call globe(iolst,cll2ll,cdatum(isource),vectis,
     &        datum,vectos,xminit,yminit,xmini,ymini,idum,c4dum)
            call globe(iolst,cll2ll,cdatum(isource),vectis,
     &        datum,vectos,xmaxit,ymaxit,xmaxi,ymaxi,idum,c4dum)
c
c           Check for Greenwich straddle
c
            if(xmini.gt.xmaxi) then
c
c             If output grid also straddles, keep original range
c
              if(lstraddleg) then
                xmini=xmini-360.
c
c             Otherwise check the side of Greenwich.  If the grid is
c             just west, shift all longitudes further east, otherwise
c             restore original range
c
              else
                if(xlonee.gt.xmini) then
                  shft=360.
                  xmaxi=xmaxi+shft
                else
                  xmini=xmini-360.
                endif
              endif
            endif
c
c           Empirical fix for WDBII
c
            if(isource.eq.1) then
              xav=abs((xmini+xmaxi)/2.)
              xoff=mod(xav,6.)
              if(xoff.gt.3.) xoff=6.-xoff
              ylatmul=1.-(6.006006e-5+1.134468e-4*xoff)
              ymini=ymini*ylatmul
              ymaxi=ymaxi*ylatmul
            endif
c
c           If polygon is outside of grid, or if the polygon is
c           from WDBII and logical not set, simply read through to go
c           to next polygon
c
            if(xlonee.lt.xmini.or.xlonww.gt.xmaxi
     &        .or.ylatnn.lt.ymini.or.ylatss.gt.ymaxi.or.
     &        (isource.eq.1.and..not.lwdbii))
     &        then
               do j=1,npts
                 read(iogshhs) idum1,idum2
               enddo
c              Remove compiler warnings
               idum1=idum1
               idum2=idum2
c
c           Parts or all are in, so extract appropriate parts for a
c           smaller polygon closing sections along the outside of the grid
c
            else
               lgood=.false.
               linside=.false.
               lfirst=.true.
               if(nshore.LT.mxcoast) then
                  nshore=nshore+1
               else
                  lpoly=.true.
                  goto 999
               endif
               nspts(nshore)=0
               istart(nshore)=ipp
               itypes(nshore)=itype
               isourcep(nshore)=isource
               ipolyid(nshore)=id
c
c              Loop through the points on the GSHHS polygon
c
               Points: do j=1,npts
c
                  call breadp(xp1it,yp1it,lbigendian,iogshhs)
                  xp1it=xp1it+shft
c
c                 Empirical fix for WDBII
c
                     if(isource.eq.1) then
                        xoff=mod(abs(xp1it),6.)
                        if(xoff.gt.3.) xoff=6.-xoff
                        ylatmul=1.-(6.006006e-5+1.134468e-4*xoff)
                        yp1it=yp1it*ylatmul
                     endif
c
c                 Transform the coordinates to the output datum
c                 and also the output projection
c
                  call globe(iolst,cll2ll,cdatum(isource),vectis,
     &            datum,vectos,xp1it,yp1it,xp1i,yp1i,idum,c4dum)
                  call globe(iolst,caction,cdatum(isource),vecti,
     &            datum,vecto,xp1it,yp1it,xp1,yp1,idum,c4dum)
c
c                 Check for shift at Greenwich
c
                  if(xmini.le.0.) then
                     if(xp1i.gt.220.) xp1i=xp1i-360.
                  endif
c
c                 Check that point is inside the user zone
c
                  if(xp1i.ge.xlonww.and.xp1i.le.xlonee.and.yp1i.ge.
     *              ylatss.and.yp1i.le.ylatnn) then
c
c                   Check for an arbitrary starting point 
c
                    if(j.eq.1) linside=.true.
c
c                   Start a new segment of the polygon
c
                    if(.not.lgood) then
                      lgood=.true.
c
c                     Check if this is the first good segment
c
c                     yes - set counter and reset flag
c
                      if(lfirst) then
                        lfirst=.false.
                        istart(nshore)=ipp
                        if(linside) then
                          xps=xp1
                          yps=yp1
                        else
                          xp(ipp)=xps
                          yp(ipp)=yps
                          if(ipp.LT.mxcoastp) then
                             ipp=ipp+1
                             nspts(nshore)=nspts(nshore)+1
                          else
                             lpnts=.true.
                             goto 999
                          endif
                        endif
c
c                       Check the side of the zone on which this section starts
c
                        sside=iside(xp1,yp1,uxmax,uxmin,uymax,uymin)
c
                      else
c
c                       Save the previous point
c
                        xp(ipp)=xps
                        yp(ipp)=yps
                        if(ipp.LT.mxcoastp) then
                           ipp=ipp+1
                           nspts(nshore)=nspts(nshore)+1
                        else
                           lpnts=.true.
                           goto 999
                        endif
c
                      endif
                    endif
c
c                   Save the point and increment counters
c
                    xp(ipp)=xp1
                    yp(ipp)=yp1
                    if(ipp.LT.mxcoastp) then
                       ipp=ipp+1
                       nspts(nshore)=nspts(nshore)+1
                    else
                       lpnts=.true.
                       goto 999
                    endif
c
c                 End of a section inside zone
c
                  elseif(lgood) then
c
c                   Check the side of the zone where this section ends
c
                    oside=iside(xp1,yp1,uxmax,uxmin,uymax,uymin)
c
c                   Add this outside point, but first save it in case
c                   it's needed to close the polygon
c
                    xpe=xp1
                    ype=yp1
                    ippe=ipp
                    eside=iside(xp1i,yp1i,xlonee,xlonww,ylatnn,
     *                ylatss)
                    xp(ipp)=xp1
                    yp(ipp)=yp1
                    if(ipp.LT.mxcoastp) then
                       ipp=ipp+1
                       nspts(nshore)=nspts(nshore)+1
                    else
                       lpnts=.true.
                       goto 999
                    endif
                    lgood=.false.
c
                  else
c
c                   Save point in case of need, and add any necessary
c                   corners to the polygon
c
                    xps=xp1
                    yps=yp1
                    if(.not.lfirst) then
                      nside=iside(xp1i,yp1i,xlonee,xlonww,ylatnn,
     *                 ylatss)
                      if(nside.ne.oside) then
                        call pcorner(nside,oside,ipp,xp,yp,nspts,
     *                    nshore,uxmax,uxmin,uymax,uymin,1)
                        oside=nside
                      endif
                    else
                      oside=iside(xp1i,yp1i,xlonee,xlonww,ylatnn,ylatss)
                    endif
                    lsurrside(oside)=.true.
c
                    lgood=.false.
                  endif
c
c              End of loop by points
c
               enddo Points
c
c              If no points were found inside the zone, set a polygon
c              to the extremes of the zone and then goto the next GSHHS
c              polygon - ie the next major outer loop.  This happens if
c              the GSHHS polygon surrounds the zone but doesn't 
c              intersect it.  Set the type for this polygon anyway - it
c              may be used if no shores are found.
c
               if(lfirst) then
c
c                Check that it actually surrounds the grid 
c
                 if((xlonee.le.xmaxi.and.xlonww.ge.xmini.and.ylatnn
     *             .le.ymaxi.and.ylatss.ge.ymini).and.
     *             (lsurrside(1).and.lsurrside(2).and.
     *             lsurrside(3).and.lsurrside(4))) then

                   itypes(nshore)=itype
                   istart(nshore)=ipp
c
c                  Water - go clockwise
c
                   if(mod(itype,2).EQ.0) then
                     icstart=1
                     icend=5
                     iinc=1
c
c                  Land - go counter-clockwise
c
                   else
                     icstart=5
                     icend=1
                     iinc=-1
                   endif
c
                   nspts(nshore)=0
                   do iicc=icstart,icend,iinc
                     iico=iicc
                     if(iico.gt.4) iico=iico-4
                     iicn=iico+iinc
                     if(iicn.le.0) iicn=iicn+4
                     if(iicn.gt.4) iicn=iicn-4
                     call pcorner(iico,iicn,ipp,xp,yp,nspts,
     *               nshore,uxmax,uxmin,uymax,uymin,2)
                   enddo
                   pxmax(nshore)=uxmax
                   pxmin(nshore)=uxmin
                   pymax(nshore)=uymax
                   pymin(nshore)=uymin
c
c                  Check that there are not nested surrounds
c
                   if(nshore.gt.1) then
                     itypes(nshore-1)=itype
                     ipp=istart(nshore)
                     nspts(nshore)=0
                     nshore=nshore-1
                   endif
c
c                If the polygon isn't to keep, decrement nshore
c
                 else
                   ipp=istart(nshore)
                   nspts(nshore)=0
                   nshore=nshore-1
                 endif
c
c                Go to next polygon
c
                 cycle Polygons
               endif
c
c              If the polygon didn't start within the zone, copy the
c              first point to the last to close the polygon after
c              checking corners
c
               if(.not.linside) then
c
c                First, check if the side of the start point and the
c                last outside point are the same
c
                 write(iolst,*) xpe,ype
                 if(ippe.LT.mxcoastp) then
                    ipp=ippe+1
                    nspts(nshore)=ipp-istart(nshore)
                 else
                    lpnts=.true.
                    goto 999
                 endif
                 oside=eside
                 dif=0.
                 if(sside.eq.oside) then
                   if(sside.eq.1) dif=ype-yp(istart(nshore))
                   if(sside.eq.2) dif=xpe-xp(istart(nshore))
                   if(sside.eq.3) dif=yp(istart(nshore))-ype
                   if(sside.eq.4) dif=xp(istart(nshore))-xpe
                 endif
c
c                Add a corner if necessary and reset the last side
c
c                Land or island - counterclockwise
c
                 if((itype.eq.1.or.itype.eq.3).and.dif.lt.0.) then
                   iicn=oside-1
                   if(iicn.eq.0) iicn=4
                   call pcorner(oside,iicn,ipp,xp,yp,nspts,
     *             nshore,uxmax,uxmin,uymax,uymin,3)
                   oside=iicn
c
c                Lake or pond - clockwise
c
                 else
                   if((itype.eq.2.or.itype.eq.4).and.dif.gt.0.) then
                     iicn=oside+1
                     if(iicn.eq.5) iicn=1
                     call pcorner(oside,iicn,ipp,xp,yp,nspts,
     *               nshore,uxmax,uxmin,uymax,uymin,4)
                     oside=iicn
                   endif
                 endif
c
c                if sides are not the same, add corners
c
                 if(sside.ne.oside) then
c
c                  Land or island - counterclockwise
c
                   if(itype.eq.1.or.itype.eq.3) then
                     if(sside.lt.oside) then
                       do iic=oside,sside+1,-1
                         iicn=iic-1
                         call pcorner(iic,iicn,ipp,xp,yp,nspts,
     *                   nshore,uxmax,uxmin,uymax,uymin,5)
                       enddo
                     else
                       nloop=4-sside+oside
                       do iicc=1,nloop
                         iic=oside-iicc+1
                         if(iic.le.0) iic=4+iic
                         iicn=iic-1
                         if(iicn.le.0) iicn=4+iicn
                         call pcorner(iic,iicn,ipp,xp,yp,nspts,
     *                   nshore,uxmax,uxmin,uymax,uymin,6)
                       enddo
                     endif
c
c                  Lake or pond - clockwise
c
                   else
                     if(sside.gt.oside) then
                       do iic=oside,sside-1
                         iicn=iic+1
                         call pcorner(iic,iicn,ipp,xp,yp,nspts,
     *                   nshore,uxmax,uxmin,uymax,uymin,7)
                       enddo
                     else
                       nloop=4-oside+sside
                       do iicc=1,nloop
                         iic=oside+iicc-1
                         if(iic.gt.4) iic=iic-4
                         iicn=iic+1
                         if(iicn.gt.4) iicn=iicn-4
                         call pcorner(iic,iicn,ipp,xp,yp,nspts,
     *                   nshore,uxmax,uxmin,uymax,uymin,8)
                       enddo
                     endif
                   endif
                 endif
               endif
c
c              Starting point to close polygon if needed
c
               if(xp(ipp-1).ne.xp(istart(nshore)).or.yp(ipp-1).ne.
     *           yp(istart(nshore))) then
                 xp(ipp)=xp(istart(nshore))
                 yp(ipp)=yp(istart(nshore))
                 if(ipp.LT.mxcoastp) then
                    ipp=ipp+1
                    nspts(nshore)=nspts(nshore)+1
                 else
                    lpnts=.true.
                    goto 999
                 endif
               endif
c               
c              Now establish mins and maxs
c
               do j=istart(nshore),ipp-1
                 pxmax(nshore)=max(pxmax(nshore),xp(j))
                 pxmin(nshore)=min(pxmin(nshore),xp(j))
                 pymax(nshore)=max(pymax(nshore),yp(j))
                 pymin(nshore)=min(pymin(nshore),yp(j))
               enddo
c
c           End for this GSHHS polygon
c
            endif
c
c     End of polygon loop
c
      enddo Polygons
c
99    continue
c
c     Final sanity check on a surrounding polygon.  If and only if the
c     first polygon is a surrounding polygon it will have 5 vetices.
c     Due to the hierarchical nature of the polygons the type should
c     be 1 less than the next polygon.
c
      if((nshore.gt.1).and.(itypes(2)-itypes(1).ne.1).and.
     *  (nspts(1).eq.5)) itypes(1)=itypes(2)-1
c
c     Open and write the BLN file if coasts were found
c
      if(nshore.eq.0) then
        write(iolst,*)
        write(iolst,*) 'Note: no shorelines found within zone'
        write(iolst,*) '      BLN file not created'
        write(iolst,*)
        write(*,*)
        write(*,*) 'Note: no shorelines found within zone'
        write(*,*) '      BLN file not created'
        write(*,*)
      else
        write(iolst,*)
        write(iolst,*) 'Note: # shoreline polygons found: ',nshore
        write(iolst,*)
        write(*,*)
        write(*,*) 'Note: # shoreline polygons found: ',nshore
        write(*,*)
        open(unit=iobln,file=coastbln)
        do i=1,nshore
          itbnl=0 
          if(mod(itypes(i),2).eq.0) itbnl=1
          write(iobln,87) nspts(i),itbnl,itypes(i),isourcep(i),
     &                    ipolyid(i)
          iend=istart(i)-1+nspts(i)
          do j=istart(i),iend 
            xout=min(max(xp(j),xllk-.0001),xurk+.0001)
            yout=min(max(yp(j),yllk-.0001),yurk+.0001)
            write(iobln,fmt="(f12.6,',',f12.6)") xout,yout
          enddo
        enddo
87      format(i7,',',i1,',"',i1,',',i1,',',i6,'"')
        write(iolst,*)
        write(iolst,*) 'BLN file created: ',coastbln
        write(iolst,*)
        close(iobln)
      endif
      return

c --- Report any problem with array sizes and quit
999   if(LPNTS) then
        write(iolst,*)
        write(iolst,*) 'ERROR: Number of shoreline points exceeds'
        write(iolst,*) '      MXCOASTP = ',mxcoastp
        write(iolst,*) 'Increase parameter MXCOASTP and recompile'
        write(iolst,*)
        write(iolst,*) '      BLN file not created'
        write(iolst,*)
        write(*,*)
        write(*,*) 'ERROR: Number of shoreline points exceeds'
        write(*,*) '      MXCOASTP = ',mxcoastp
        write(*,*) 'Increase parameter MXCOASTP and recompile'
        write(*,*) 'Halted in COASTEXT'
        write(*,*)
        write(*,*) '      BLN file not created'
        write(*,*)
      endif
      if(LPOLY) then
        write(iolst,*)
        write(iolst,*) 'ERROR: Number of shorelines exceeds'
        write(iolst,*) '      MXCOAST = ',mxcoast
        write(iolst,*) 'Increase parameter MXCOAST and recompile'
        write(iolst,*)
        write(iolst,*) '      BLN file not created'
        write(iolst,*)
        write(*,*)
        write(*,*) 'ERROR: Number of shorelines exceeds'
        write(*,*) '      MXCOAST = ',mxcoast
        write(*,*) 'Increase parameter MXCOAST and recompile'
        write(*,*) 'Halted in COASTEXT'
        write(*,*)
        write(*,*) '      BLN file not created'
        write(*,*)
      endif
      stop

      end
c-----------------------------------------------------------------------
      integer function iside(x,y,xmx,xmn,ymx,ymn)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684           Level: 040311             ISIDE
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     ISIDE decides to which side of the polygon a point is 
c              nearest.  Sides are numbered clockwise 1-4 starting on
c              the west side, going around to south. 
c
c --- Updates
c        Ver. 3.55 Level 040305 to Ver. 3.56 Level 040311
c              - Check for inside vs outside point
c
c ARGUMENTS:
c    PASSED:  x,y        - point location
c             x,y(mx,mn) - grid limits for the polygon
c
c  RETURNED:  iside      - the side for this point
c
c CALLING ROUTINES:   COASTEXT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Local variables
      real x,y,xmx,xmn,ymx,ymn,difw,dife,difs,difn,difsn,difew
      difw=abs(x-xmn)
      dife=abs(x-xmx)
      difs=abs(y-ymn)
      difn=abs(y-ymx)
      difsn=min(difs,difn)
      difew=min(dife,difw)
c
c --- Check if the point is clearly north or south
c
      if(x.gt.xmn.and.x.lt.xmx.and.(y.le.ymn.or.y.ge.ymx)) then
        iside=2
        if(difs.lt.difn) iside=4
c
c --- Otherwise, check if clearly west or east
c
      elseif(y.gt.ymn.and.y.lt.ymx.and.(x.le.xmn.or.x.ge.xmx)) then
        iside=1
        if(dife.lt.difw) iside=3
c
c --- Off a corner or inside - decide on closest side
c
      else
c
c ---   Inside?
c
        if(x.ge.xmn.and.x.le.xmx.and.y.ge.ymn.and.y.le.ymx) then
          if(difsn.gt.difew) then
            iside=1
            if(dife.lt.difw) iside=3
          else
            iside=2
            if(difs.lt.difn) iside=4
          endif
c
c ---   Outside
c
        else
          if(difsn.lt.difew) then
            iside=1
            if(dife.lt.difw) iside=3
          else
            iside=2
            if(difs.lt.difn) iside=4
          endif
        endif
c
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
      subroutine pcorner(side1,side2,ipp,xp,yp,nspts,nshore,xmax,
     *  xmin,ymax,ymin,icall)
c-----------------------------------------------------------------------
c
c --- TERREL    Version: 3.684           Level: 040228           PCORNER
c               K. Morrison  Earth Tech
c
c
c PURPOSE:     PCORNER adds the corners of the grid area to a polygon 
c              to facilitate closing the polygon, unless the corner is 
c              redundant, in which case it removes the redundancy.
c
c --- Updates
c
c ARGUMENTS:
c    PASSED:  side1, side2 - sides of the polygon
c             ipp          - counter for total number of points (+1)
c             xp,yp        - arrays containing polygon points
c             nspts        - array of number of points in each polygon
c             nshore       - current number of polygons
c             x,y(max,min) - limits of grid area
c             icall        - calling occurrence in COASTEXT
c
c  RETURNED:  updated ipp, xp, yp, nspts 
c
c CALLING ROUTINES:   COASTEXT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Local variables
      integer side1,side2,ipp,nspts(*),nshore,iprod
      real xp(*),yp(*),xmax,xmin,ymax,ymin
c
c     sanity check - sides must be 1-4
c
      if(min(side1,side2).lt.1.or.max(side1,side2).gt.4.or.
     *  side1.eq.side2) then
        write(*,*) ' Corner problem for coastal polygons'
        write(*,*) '  Side out of range'
        write(*,*) ' Side 1 - ',side1,'  Side 2 - ',side2
        write(*,*) '   Call # ',icall
        stop
      endif
c
c     choose the corner to add based on the product of the sides
c
      iprod=side1*side2
      if(iprod.eq.2) then
        xp(ipp)=xmin
        yp(ipp)=ymax
      elseif(iprod.eq.6) then
        xp(ipp)=xmax
        yp(ipp)=ymax
      elseif(iprod.eq.12) then
        xp(ipp)=xmax
        yp(ipp)=ymin
      elseif(iprod.eq.4) then
        xp(ipp)=xmin
        yp(ipp)=ymin
      else
        write(*,*) ' Sides problem in PCORNER'
        write(*,*) ' Side 1 - ',side1,'  Side 2 - ',side2
        write(*,*) '   Call # ',icall
        stop
      endif
c
c     check for redundant corner, and remove if present
c
      if(ipp.gt.1) then
        if(xp(ipp).eq.xp(ipp-1).and.yp(ipp).eq.yp(ipp-1)) then
          ipp=ipp-1
          nspts(nshore)=nspts(nshore)-1
          return
        endif
      endif
c
c     otherwise, add the corner and update counters
c
      ipp=ipp+1
      nspts(nshore)=nspts(nshore)+1
      return
      end

