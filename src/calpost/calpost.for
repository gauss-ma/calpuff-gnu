c-----------------------------------------------------------------------
      program calpost
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221           Level: 080724            MAIN
c
c ---           D. Strimaitis, J. Scire
c
c     Copyright (c) 1998-2008 by Exponent, Inc.         
c
c
c  PURPOSE:     Main program for processing concentration data produced
c               by either CALGRID or CALPUFF.  Summary output tables
c               are printed depending on options and specifications
c               provided by the user.
c
c --- Modifications - V6.22, Level 080709 to V6.221, Level 080724
c
c     1) Expand check to identify missing periods when performing calm
c        processing for files from other plume models produced in
c        CALPUFF format.  Periods with positive heat flux and valid
c        wind (u-star) may have a missing convective mixing height due
c        to missing upper-air data, which makes that period 'missing'.
c        Report number of periods found to list file.
c        Modified: CALMPRO.PST
c                  (main), BLOCK DATA, GETCALM
c     2) Change the default extinction efficiency of NO2 absorbtion from
c        0.17 to 0.1755 (1/Mm per ug/m3)
c        Modified: BLOCK DATA
c     3) Add more digits to extinction efficiency in control file input
c        summary section of list file
c        Modified: READCF
c     4) Add f(RH) columns to list file for small/large/seasalt
c        (Implemented for monthly f(RH) -- M8_MODE=2,3,4,5)
c        and drop RH component from run-length output report
c        Modified: WRIVIS, OUTVIS, VISSUMRY, UPV24
c     5) Report ranked visibility records for all days with change > 5%
c        up to maximum MXV24 set in PARAMS file
c        Modified: PARAMS.PST
c                  VISSUMRY
c     6) Add MVISCHECK option for testing FLAG 2008 configuration, and
c        recognize AREANAME input for Class I area.
c        Do not require MFRH=4 with MVISBK=8 unless M_MODE = 1, 2, or 3
c        Modified: CTRL.PST
c                  BLOCK DATA, READCF, QAINP
c
c --- Modifications - V6.213, Level 080407 to V6.22, Level 080709
c
c     1) Added control-file specification of monthly f(RH) for each of
c        the Method 8 components (small & large hygroscopic, and sea
c        salt), and increased M8_MODE range to 1-5  (4,5 added).
c        Reset visibility defaults as follows:
c             MFRH     = 4
c             MVISBK   = 8
c             M8_MODE  = 5
c        Modified: VISB.PST
c                  (main), BLOCK DATA, READCF, QAINP, WRIVIS,
c                  SETVISBK, GETRCRD, BEXT_24
c
c --- Modifications - V6.212, Level 080404 to V6.213, Level 080407
c
c     1) Updated CALUTILS from v2.55 Level 070327 to v2.56 Level 080407
c        Control file entries in exponential notation were not correct
c        if decimal point was missing (2e-02 was read as 0.2e-02).
c        Modified: ALTONU
c
c --- Modifications - V6.211, Level 080214 to V6.212, Level 080404
c
c     1) Processing NO2 using NO2/NOx ratio was active for only one of 
c        receptor types (gridded, discrete, OR terrain)due to a bug in
c        the logic.  Change to independent if-blocks for each receptor
c        type.
c        Modified: GETRCRD
c        (CEC)
c
c --- Modifications - V6.21, Level 080205 to V6.211, Level 080214
c
c    (1) Correct the variable written to list file to document the
c        fraction of CALPUFF NOx that is used for NO2.  The retired
c        variable (FNO2NOX) was written instead of the current variable
c        (RNO2NOX).  This does not alter results.
c        Modified:  READCF
c
c --- Modifications - V6.2, Level 070712 to V6.21, Level 080205
c
c    (1) COORDLIB from v1.98 Level 060911 to v1.99 Level 070921
c        - Conversion of point in S. hemisphere to UTM-N returned coord.
c          as UTM-S instead, for conversions from all map projections
c          except lat/lon.
c        - Initialization of a few work arrays were missing.  These have
c          no effect on results.
c        Modified:  COORDS
c
c --- Modifications - V6.145, Level 070501 to V6.2, Level 070712
c
c     (1) Add option to assign NO2 from NOx concentrations using factors
c         that are a function of NOx (user-supplied tabulation).  The 
c         single value factor is retained, but entered as a new variable
c         RNO2NOX instead of FNO2NOX because the option is relocated to
c         Group 1 from Group 2 where 'old' control files may specify
c         FNO2NOX.  NO2 derived from NOx is used for both concentration
c         processing (ASPEC=NO2) and in the visibility processing
c         (LVNO2=T).
c         New:      NO2NOX.PST, MAKENO2
c         Modified: BLOCK DATA, CTRL.PST, VISB.PST
c                   READCF, QAINP, GETRCRD, BEXT_24, MAPSPEC
c
c --- Modifications - V6.144, Level 070130 to V6.145, Level 070501
c
c     (1) CALUTILS from v2.54 Level 061020 to v2.55 Level 070327
c         Fixed format bug in subroutine BASRUTC for the case of
c         time zone zero (output string was 'UTC+0  0' instead
c         of 'UTC+0000'
c         Modified:  UTCBASR, BASRUTC
c     (2) COORDLIB from v1.97 Level 060626 to v1.98 Level 060911
c         Changes in COORDS that allow a higher level of FORTRAN error
c         checking.  Compiler checks had identified constant arguments
c         and 2 uninitialized variables.  None of these is known to
c         have produced errors in cooerdinate conversions.
c         Modified:  COORDS
c     (3) Initialize several variables explicitly.  Compilers typically
c         used are configured to set unintialized variables to zero,
c         which allows CALPOST to work as intended.  Explicit
c         initialization provides intended values for those compilers
c         not so configured.
c      -  Initialize exceedence counting arrays to zero.
c         New:       RESETI
c         Modified:  (main)
c      -  Initialize top-N date arrays to zero
c         Modified:  (main)
c      -  Initialize minimum number of periods in averages to zero.
c         Non-zero values for min3, min24, and minN are set for the
c         calm-pro option, and are otherwise zero.
c         This option was introduced in v6.144.
c         Modified:  (main)
c      -  Initialize all top-50 arrays (concentrations were already
c         initialized to vinit).
c         New:       INIT50
c         Modified:  (main)
c     (4) Remove errant assignment: aconcd(isrc,ispec)=tcg(isrec,jsrec)
c         In this line, isrec and jsrec are not defined (typically zero)
c         and isrc=1, so that the first discrete receptor concentration
c         is assigned a zero value before the modeled value is added in.
c         Introduced in v6.141, this bug appears benign for normal
c         applications
c         Modified:  (main)
c
c --- Modifications - V6.143, Level 070117 to V6.144, Level 070130
c
c     (1) Add new option to perform calm processing when averaging
c         results from models that do not produce concentrations for
c         periods associated with 'calm' winds (e.g., AERMOD).  This
c         option requires the SURFACE meteorological data file that
c         was used to run the dispersion model.  New control-file
c         inputs are MET1DAT, MET1FMT, and MCALMPRO.
c         New:       CALMPRO.PST
c                    GETCALM
c         Modified:  FILNAM.PST, CTRL.PST
c                    BLOCK DATA, READCF, READFN, QAINP, WRFILES
c
c     (2) Add new time-checks on the input records to test the start/end
c         times on input data records with those expected each period.
c         New:       TTEST, TUP
c         Modified:  (main)
c
c     (3) Blank RHS of output template string names written to
c         list file
c         Modified:  WRFILES
c
c --- Modifications - V6.142, Level 061218 to V6.143, Level 070117
c
c     (1) Fix bug in dataset 2.2 units processing -- wrong variable in
c         conditional check causes concentration units to be used for
c         flux labels when datasets prior to 2.2 are processed.
c         Modified:  GETHEAD
c
c --- Modifications - V6.141, Level 061120 to V6.142, Level 061218
c
c     (1) Method 8 visibility processing with M8_MODE=3 did not include
c         the LVSO4,LVNO3,LVOC,LVPMC,LVPMF,LVEC,LVNO2 selections.
c         Modified:  BEXT_24
c     (2) Dataset version 2.2 bug:  base time zone string added to
c         header was not declared as character in /head/
c         Modified:  HEAD.PST
c     (3) Add NO2 (Gas) to HLABEL in top-50 listing
c         Modified:  WRIT50
c
c --- Modifications - V6.14, Level 061107 to V6.141, Level 061120
c
c     (1) Revise Method 8 Visibility implementation to operate on daily
c         average concentrations.  Introduce arrays for processing all
c         species in file so that 24-hour visibility extinction can be
c         calculated from individual 24-hour concentrations (make
c         general so that future CALPOST can process more than 1 species
c         in a single application).  This treatment is selected using
c         M8_MODE=3 (replaces RHTYPE control file variable)
c         New:       SARRAYS.PST, SPECOUT.PST,
c                    MAPSPEC, SUMACONC, AVGACONC, BEXT_24, V24_RUNL
c         Modified:  CONC.PST, VISB.PST
c                    main, READCF, QAINP, WRIVIS
c     (2) Prohibit use of scaling and background concentrations
c         when doing visibility
c         Modified:  QAINP
c
c --- Modifications - V6.133, Level 060919 to V6.14, Level 061107
c
c     (1) Add Method 8 Visibility procesing option with extinction
c         efficiencies for sulfate, nitrate and organic carbon that vary
c         with concentration, and with F(RH) curves that vary with
c         particle size class (small/large).
c         Add IRHTYPE to allow "method 2 or method 6" treatment.
c         Includes background sea salt.
c         New:       FRH4, IMPRV06, RH12
c         Modified:  BLOCK DATA, VISB.PST, CONC.PST
c                    READCF, QAINP, GETRCRD, SETVISBK
c     (2) Add NO2 absorption option to visibility processing
c         Modified:  BLOCK DATA, VISB.PST, CONC.PST
c                    READCF, QAINP, GETRCRD, SUMEX24, AVGEX24
c                    WRIVIS, OUTVIS, VISSUMRY, UPV24
c     (3) CALUTILS from v2.52 Level 060519 to v2.54 Level 061020
c         Move GLOBE1 to COORDLIB;
c         Allow negative increments in INCRS
c         Modified:  INCRS
c         Removed:   GLOBE1
c     (4) COORDLIB from v1.95 Level 050126 to v1.97 Level 060626
c         Add Albers Conical Equal Area projection
c         Add GLOBE1 (from CALUTILS)
c
c --- Modifications - V6.132, Level 060519 to V6.133, Level 060919
c
c     (1) Subroutine OUTDVIS corrected to take the start-time of the
c         averaging period rather than the end-time of the period.  Since
c         the call is made with the start-time, the date-times reported
c         in the DVISDAT output file are either 1 hour early (hourly
c         output) or 1 day early (daily output).
c         Modified:  OUTDVIS
c     (2) Change unit number in1 in several calls within GETHEAD to io,
c         which is passed as an argument.  The two unit number variables
c         had been equal so this does not alter results from CALPOST.
c         Modified:  GETHEAD
c     (3) Initialize NHRS to zero since it is only present in "older" 
c         control files (CALPOST version 5)
c         Modified:  READCF
c
c --- Modifications - V6.131, Level 060410 to V6.132, Level 060519
c
c     (1) CALUTILS from v2.51 Level 051019 to v2.52 Level 060519
c         Variable names in control file are not processed correctly
c         if there are too many characters (including blanks) to the
c         left of the "=" sign (run stops in setup phase).
c         Modified:  READIN
c     (2) Identify hour listed in the visibility output as the starting
c         time
c         Modified:  WRIVIS
c         
c
c --- Modifications - V6.13, Level 060324 to V6.131, Level 060410
c
c    (1) Modify test in QAINP for fractional averaging periods to 
c        allow long periods to be processed (selected variables need
c        to be real*8 instead of real*4).  This change contributed by
c        Ken Morrison, HATCH Associates)
c        Modified: QAINP
c
c --- Modifications - V6.12, Level 060309 to V6.13, Level 060324
c
c    (1) Add CALPUFF concentration/deposition dataset 2.2 file format
c        - Add header record after species-level list for units of
c          each species-level
c        - Base time zone read as character*8 (e.g. UTC-0500)
c        Modified: HEAD.PST
c                  MAIN, GETHEAD, GETHDRH, QAINP
c
c --- Modifications - V6.11, Level 051012 to V6.12, Level 060309
c
c    (1) COORDLIB from v1.95 Level 050126 to v1.96 Level 051010
c    (2) CALUTILS from v2.5 Level 041123 to v2.51 Level 051019
c
c    (3) Change mxgx and mxgy to ielmet and jelmet -
c        the actual size of CALMET grid when checking the
c        receptors indices against the CALMET grid.
c        check size of CALMET domain (ielmet, jelmet) against
c        maximum limits of gridded receptors put in PARAMS.PST
c        (C. Escoffier-Czaja)
c        Modified: GETRCRD, GETHEAD, GETHDRH
c
c    (4) Add new tables to the visibility summary in the list file to
c        report the daily visibility peaks in ranked order (same results
c        are currently listed in chronological order)
c        New:      UPV24
c        Modified: PARAMS.PST, CTRL.PST
c                  BLOCK DATA, VISSUMRY
c    (5) Add auxiliary output file option for visibility change at each
c        receptor
c        New:      DVIS_HD, OUTDVIS
c        Modified: CTRL.PST, FILNAM.PST
c                  BLOCK DATA, READFN, READCF, QAINP, WRFILES
c
c --- Modifications - V6.1, Level 050915 to V6.11, Level 051012
c
c    (1) Test for planar GRD (SURFER) output fields, and set min/max
c        in the header slightly different to force SURFER to make a
c        plot with no contours
c        Modified: PLTECHO, PLTTOPN, PLTEXC
c    (2) Switch labeling convention from end-time to start-time
c        Modified: (MAIN), ECHO, PLTECHO, TSER_HD, TSEROUT,
c                  PEAK_HD, PEAKOUT, WRITOPN, WRIT50, WRISUM
c
c --- Modifications - V5.65, Level 050729 to V6.1, Level 050915
c
c     Version 6 introduces sub-hourly timesteps into the CALPUFF system.
c     An early implementation of Version 6 was developed in 1999 for
c     testing and limited application.  CALPOST V6.0, level 990806 was
c     based on CALPOST V5.0, level 990228.  The sub-hourly timestep
c     features have now been fully implemented in the current version
c     to create CALPOST V6.1, level 050915.
c
c    (1) Activate general run-steps, with time resolved to seconds;
c        Update the control file (retain compatibility with previous
c        versions)
c        Modified: CTRL.PST, BLOCKDATA, READCF, QAINP
c    (2) Add the base time zone (BTZONE) input to control file group 1.
c        It may also be in the visibility Method 7 input section of
c        group 2.  If it appears in both groups, the value must be the
c        same.  BTZONE is needed only if CALPUFF Dataset Version is 2.0
c        or earlier.  BTZONE is now stored in /CTRL/, BTZONE2 in /VISB/.
c        Modified: CTRL.PST, VISB.PST, BLOCKDATA, READCF
c    (3) Modify list and plot file labels that identify averaging times.
c        Modified: ECHO_HD, ECHO, PLTECHO, PLTTOPN, PLTEXC,
c                  WRIMAP, WRIMAPR, WRIMAPX, WRIEXC, WRIT50, WRITOPN,
c                  WRIPEAK, TSEROPEN, TSER_HD, TSER_OUT, PEAK_HD,
c                  PEAK_OUT
c    (4) Update CALUTILS (Version 2.4, Level 041029) for new routines
c        INCRS, MIDNITE
c
c --- Modifications - V5.638, Level 050408 to V5.65, Level 050729
c
c    (1) Add SAMPLER processing option that configures the run to
c        produce output directly related to measured sampler data
c        (e.g., sample end-time, averaging period, and species)
c        Several options are configured via the MSAMPLER switch and
c        the sampler data are provided in a SAMP.DAT file.
c        New:      SAMP.PST
c                  RDSAMP, TDATE, SUMSAMP, AVGSAMP, WRISAMP
c        Modified: PARAMS.PST, CTRL.PST, FILNAM.PST
c                  BLOCK DATA, READFN, READCF, WRFILES, QAINP, GETRCRD
c
c --- Modifications - V5.637, Level 050321 to V5.638, Level 050408
c
c    (1) Bug in TSEROPEN:  update NPATH to NTSPATH.  This was partially
c        implemented so the path portion of the name only worked if 
c        npath=ntspath (typically 0).  An invalid pathname resulted.
c        Modified: TSEROPEN
c
c    (2) Add peak value output option.  Produce a timeseries output file
c        that contains just the peak value of the requested species and
c        and averaging time, over the receptors that are selected.  New
c        control variable for this is LPEAK, and the output files have
c        the form PEAKVAL_ASPEC_ttHR_CONC_TSUNAM.DAT.
c        New:      PEAK_HD, PEAKOUT
c        Modified: CTRL.PST
c                  BLOCK DATA, READCF, TSEROPEN, TSERSET
c
c --- Modifications - V5.636, Level 050218 to V5.637, Level 050321
c
c    (1) Typo in GETHEAD creates problem for reading source names for
c        line source type 2 (variables emissions file) in Dataset 2.1
c        format.  CALPOST terminated with error message.
c        Modified: GETHEAD
c
c --- Modifications - V5.635, Level 050128 to V5.636, Level 050218
c
c    (1) Change the output format of Light extinction in the list file
c        to accomodate value larger than 9999. in the TOP50 Tables
c        Modified: WRIT50
c    (2) Information about MM5_LQW file not printed on the screen anymore
c        but in the list file.
c        (C. Escoffier-Czaja)
c
c --- Modifications - V5.634, Level 050114 to V5.635, Level 050128
c
c    (1) COORDLIB updated to stop UTM conversions with a DATUM that is
c        not mapped to the list in the USGS UTM subroutine.  An example
c        is the sphere datum NWS-84 (Earth radius 6370km), since only
c        the sphere datum ESR-S (Earth radius 6371km) is available.
c        Unmapped datums had defaulted to the Clarke 1866 spheroid.
c        LAZA Projection:  removed assignment of 6370 km earth
c        radius (NWS-84 datum) when a value less than 6000 km is
c        found.  This assignment can override a requested radius
c        of 6371 (ESR-S datum) if the NWS-84 datum is used with
c        any valid projection prior to the request for ESR-S.
c        LAZA(NWS-84) coordinate distances from the projection
c        origin are about 0.016% smaller than LAZA(ESR-S).
c        Error message and version strings added to COORDS calls and new
c        subroutine COORDSVER to report COORDS version documentation.
c        (Version 1.95, Level 050126)
c
c    (2) Added call to COORDSVER to access the COORDS version info and
c        passed string to list file.
c        Modified: READCF
c
c    (3) Remove debug argument from PREPWX call (not used).
c
c --- Modifications - V5.633, Level 041202 to V5.634, Level 050114
c
c    (1) Fix the number of dummy arguments in one call to READIN from
c        READCF.  An extra idum had been left in the argument list when
c        BTZONE was added to input group 2.
c        Modified:  READCF
c
c --- Modifications - V5.632, Level 041130 to V5.633, Level 041202
c
c    (1) A modification of averaging the percent change in light extinction
c        is implemented when the logical LAVER is true. The average of the
c        ratio is done instead of doing the ratio of the source average over
c        the background average.
c        Subroutines modified: SUMVISB, AVGVISB, SUMCONC, AVGCONC, OUTVIS
c        ARRAYS.PST is modified (av??pg, av??pd, av??pt were added)
c    (2) modification of READCF to implement the new parameter in CALPOST control
c        input file for visibility - LAVER (logical for 24h averaging of percent
c                     change in light extinction -average the ratios of source
c                     extinction over background extinction)
c                     VISB.PST is modified
c        (C. Escoffier-Czaja)
c
c --- Modifications - V5.631, Level 040917 to V5.632, Level 041130
c
c    (1) Add option to specify the f(RH) curve used to simulate the
c        effect on the scattering coefficient of the growth of
c        hygroscopic particles as a function of the relative humidity.
c        Three choices are provided for f(RH) - the IWAQM (1998) curve,
c        the FLAG (2000) tabulation, and the EPA (2003) tabulation.
c        Modified: VISB.PST,
c                  BLOCK DATA, READCF, QAINP, GROWTH
c
c --- Modifications - V5.63, Level 040623 to V5.631, Level 040917
c --- Christelle Escoffier-Czaja (CEC)
c
c    (1) Add possibility of reading MM5 2D_LQW file to be read for method 7
c        instead of Meteorological Observation station.
c        subroutine PREPWX and GETWX3 was created.
c        RDHD5, RDHD52 and RDHD53 are added to read MM5_2DLQW file and
c        compute the closest MM5 grid point from a receptor.
c        these subroutines were taken from CALMET
c
c --- Modifications - V5.62, Level 040503 to V5.63, Level 040623
c
c    (1) Fix bug in reading source names into array (CALPUFF dataset
c        version 2.1) that caused a QA halt when more than 1 source
c        type is in the file
c        Modified: GETHEAD
c
c --- Modifications - V5.61, Level 040123 to V5.62, Level 040503
c
c    (1) Expand automatic plot-file names, and include UTC+HHmm format
c        in the time part of the name.  For example,
c        1pm EST on March 31, 2004 is 2004_M03_D31_1300(UTC-0500)
c        6pm GMT on March 31, 2004 is 2004_M03_D31_1800(UTC+0000)
c        7pm (Paris) on March 31, 2004 is 2004_M03_D31_1900(UTC+0100)
c        (READFN, PLTECHO, PLTEXC, PLTTOPN, WRIMAP, TSEROPEN, WRFILES)
c    (2) Replace filename strings c*70 with c*132; this also requires
c        changes in CALUTILS (updated to V2.3, Level 040330)
c    (3) Move BTZONE input processing originally needed for Method 7
c        visibility applications outside of the Method 7 construct
c        since it may now be required for the filename time when 
c        a CALPUFF.DAT Version 2.1 file is not processed. (QAINP)
c
c --- Modifications - V5.6, Level 031017 to V5.61, Level 040123
c
c    (1) Fix bug that caused error in reading 'old' CALPUFF (before 
c        dataset version 2.1) output format (iptime replaces irhtime)
c        Modified: GETHEAD
c
c --- Modifications - V5.51, Level 030709 to V5.6, Level 031017
c
c    (1) Implement CALPUFF dataset version 2.1 output format
c        Modified: HEAD.PST, GETHEAD, GETHDRH
c        Created:  SOURCE.PST 
c
c    (2) Add source contribution option (MSOURCE) that allows a 
c        'traceback' analysis to identify sources contributing at a
c        specified receptor.
c        Modified: HEAD.PST, CTRL.PST, PARAMS.PST, TSER.PST,
c                  BLOCK DATA, READCF, QAINP, GETHEAD, SKIPREC, GETRCRD,
c                  SCALE, BACK, SUMCONC, AVECONC, COUNTX, RESET, UPT50,
c                  UPTN, ECHO_HD, ECHO, PLTECHO, PLTTOPN, PLTEXC,
c                  WRIMAP, WRIMAPR, WRIMAPX, WRIEXC, WRIT50, WRISUM,
c                  WRITOPN, WRIPEAK, WRIPEAKR, SUNIT, CNTXDAY, XCDAY,
c                  WRIVIO, TSERSET, TSER_HD, TSEROUT
c        Created:  RDCONC, RDRELH
c
c --- Modifications - V5.5, Level 030627 to V5.51, Level 030709
c
c    (1) Treat RH<1% case in f(RH) lookup table (GROWTH)
c
c --- Modifications - V5.41, Level 030528 to V5.5, Level 030627
c
c    (1) Add Method 7 for visibility processing  (D. Strimaitis)
c        - Computations are the same as Method 2 unless obscuring
c          weather is observed (fog/precipitation), in which case
c          the observed visual range is converted to background
c          extinction
c        - Weather observations are read from a DATSAV format surface
c          station file (abbreviated, space-delimited) containing
c          one or more stations
c        - User identifies a list of station IDs to use, ordered by
c          priority (first station in list with valid data for hour
c          is used).  Time zone for each is required.
c    (2) Add additional debug output for Method 7   (D. Strimaitis)
c        - Activated when LDEBUG=T
c        - DEBUG.WX1: lists available weather records
c        - DEBUG.WX2: lists weather records used
c        - report.hrv: reports hourly extinction components at each
c                     receptor
c    (3) Add output report option for hourly extinctions (D. Strimaitis)
c        - Currently for Method 7
c        - Activated when LVEXTHR=T
c        - REPORT.HRV: reports hourly extinction calculation at each
c                      receptor
c
c --- Modifications - V5.4, Level 030402 to V5.41, Level 030528
c
c    (1) Update CALUTILS (Version 2.2, Level 030528) (J.Scire, 
c        D. Strimaitis)
c    (2) Change format in write statement (READCF) for gridded 
c        receptors subset option (F.Robe)
c
c --- Modifications - V5.2, L991104d to V5.4, L030402
c
c    (1) Allow 2D array of relative humidity in VISB.DAT  (F. Robe)
c        - First CALPUFF header record has i2drh for all types
c          of CALPUFF output files (change in GETHEAD)
c        - No records #5-6-7 in header of VISB.DAT if 2D array of RH 
c
c    (2) Replace common utility subroutines with the CALUTILS group
c        of subroutines configured for LF95 compilation
c        CALUTILS.FOR (Version 2.1, Level 030402)   (D. Strimaitis)
c
c    (3) Implement dataset version 2.0 CALPUFF format, with control
c        file images as comment records and map projection variables,
c        with echo toggled by LDOC control file entry (D. Strimaitis)
c
c --- Modifications - V5.2, L991104c to V5.2, L991104d (D. Strimaitis)
c
c    (1) Reset default visibility method from MVISBK=6 to MVISBK=2
c
c    (2) Add 'FIN' subroutine to close run like CALMET/CALPUFF
c
c --- Modifications - V5.2, L991104b to V5.2, L991104c (D. Strimaitis)
c
c    (1) Implement processing option to report results by ring for
c        Top-N, Exceedence, Echo, and Time Series output options
c        (discrete receptors generated in CALPUFF GUI)
c
c    (2) Implement processing option to report results by ring for
c        Visibility 
c        (discrete receptors generated in CALPUFF GUI)
c
c --- Modifications - V5.2, L991104a to V5.2, L991104b (D. Strimaitis)
c
c    (1) Add processing option to report results by ring (discrete
c        receptors generated in CALPUFF GUI) via LDRING variable
c
c --- Modifications - V5.2, L991104 to V5.2, L991104a (D. Strimaitis)
c
c    (1) Add Elemental Carbon (EC) as a modeled species for visibility
c    (2) Add check for iday > MXDAY in CNTXDAY
c
c --- Modifications - V5.1, L990920 to V5.2, L991104 (D. Strimaitis,
c                     J. Scire)
c
c    (1) Allow multiple time-series files (various averaging times)
c    (2) Add 'TF' as a species-level code to denote TOTAL FLUX (wet &
c        dry)
c    (3) Write error messages to list file as well as screen (GROWTH,
c        OUTVIS, READFN, YR4, QAYR4, TSERSET)
c
c --- Modifications - V5.0, L990228 to V5.1, L990920
c
c  (a1/5.0) Interpret "-1"s provided in control file for range of
c         gridded receptors to process (IBGRID,IEGRID,JBGRID,JEGRID)
c         whenever CALPUFF file contains gridded receptors (LSGRID=T)
c         
c  (a2/5.0) Interpret "-1" provided in control file for discrete receptors
c         to process (NDRECP) whenever CALPUFF file contains discrete 
c         receptors (NDREC>0)
c         
c  (a3/5.0) Add ability to exclude specific gridded receptors from the 
c         processing (NGONOFF, NGXRECP added to control file, and
c         NGRECP array added to /CTRL/)
c         
c  (a4/5.0) Add ability to report exceedances over multi-day running periods
c         (e.g., number of exceedances of 1-hour threshold in a running
c          7-day period)
c
c  (b1/5.0) Initialize "top" value markers with very negative numbers to
c         allow PSD-type analyses
c
c  (b2/5.0) Increase exponential formats to allow space for '-' sign
c
c
c    (1)  Enforce YYYY format for year
c                                                      <v6.0 990709>
c
c    (2)  Allow general run-steps, with time resolved to seconds
c                                                      <v6.0 990709>
c
c    (3)  WRIMAP: use 'nn' in filename when averaging period exceeds 99
c                                                      <v6.0 990709>
c
c    (4)  Use 2-D storage for datetime of topN results to allow
c         YYYYJJJ;HHMM format
c                                                      <v6.0 990806>
c
c    (5)  Switch integer*2 arrays to integer (*4)
c
c    (6)  Revise QA output for excluded receptors
c
c    (7)  Skip excluded receptors when updating TOP50 arrays,
c         and when reporting results; otherwise, compute all values
c         and report 'null' values if field must be written (e.g.,
c         in the case of gridded output).
c
c    (8)  Revise control file (mainly output options and logicals)
c
c    (9)  Add time-series option
c
c   (10)  Add visibility processing option for latest FLAG guidelines
c         as MVISBK=6 and add user-supplied monthly RH factors for
c         this method
c
c
c
c  ARGUMENTS:   none
c
c  CALLING ROUTINES:    none
c
c  EXTERNAL ROUTINES:   COMLINE, READCF, GETHDRH, 
c                       WRIVL, GETCTRL, GETHEAD, QAINP, GETRCRD, ECHO_HD
c                       SCALE, UPT50, UPTN, ECHO, SUMCONC, AVGCONC,
c                       COUNTX, RESET, SUNIT, WRIT50, WRITOPN, WRIEXC,
c                       WRIMAP, PLTECHO, PLTTOPN, PLTEXC, PLTREC,
c                       WRISUM, WRIPEAK, WRIPEAKR, SKIPREC,
c                       SUMVISB, AVGVISB, GETHDVSR, VISSUMRY,
c                       TSERSET, TSEROPEN, TSER_HD, TSEROUT,
c                       PEAKSET, PEAK_HD, PEAKOUT, DVIS_HD, OUTDVIS
c                       RINGSET, AVGRING, AVGSAMP, WRISAMP, FIN, MIDNITE
c                       MAPSPEC, SUMACONC, AVGACONC,
c                       TTEST, TUP, INIT50, RESETI
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'calmpro.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'filnam.pst'
      INCLUDE 'samp.pst'
      INCLUDE 'source.pst'
      INCLUDE 'specout.pst'
      INCLUDE 'tser.pst'
      INCLUDE 'visb.pst'
      INCLUDE 'vispec.pst'
      character*12 version,level
c
c  Declare arrays not passed in common
      INCLUDE 'arrays.pst'
      INCLUDE 'sarrays.pst'
c
c  Declare local logicals
      logical lfull
      logical LT1HR,LT3HR,LT24HR,LTNAVG,LTRUNL
c
c  Set initial value for "top" arrays
      data vinit/-9.99e25/
c
c  Identify version and level of CALPOST:
      version='6.221'
      level='080724'
c
c  Check that the version and level number in the parameter
c  file matches those in the code itself
      if(version.ne.mver.or.level.ne.mlevel)then
         write(*,2) version,level,mver,mlevel
2        format(/1x,'ERROR  -- The version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif
c
c  Get the name of the control file from the command line
      call comline(pstinp)
c
c  Open the control file
      open(in2,file=pstinp,status='old')
c
c  Obtain program control information for run (/ctrl/)
      call READCF(in2,io1,version,level)
c
c  If this is a visibility calculation that requires hourly RH data,
c  open the file with relative humidity data and read the header
      if (LVISIB .AND. .not.LRHFAC) then
        open(in3,file=visdat,status='old',form='unformatted')
        call GETHDRH(in3)
      endif
c
c  Open unformatted input files
      open(in1,file=moddat,status='old',form='unformatted')
c
c  Obtain specifications for modeled concentrations (/head/)
c  (must come after call to GETHDRH)
      call GETHEAD(in1)
c
c  Map input species-levels to those that are needed
      call MAPSPEC
c
c  Adjust start time from UTC to local and set species array
c  for SAMPLER processing option
      if(MSAMPLER.GT.0) call SETSAMP
c
c  Check consistency of information, echo values, & calc. periods to
c  skip to get to first record for processing in "concentration" file
      call QAINP(io1,nskip)
c
c  Open the background data file and read the header record.
c  The variable confact in the header record is a conversion factor
c  that converts the hourly background concentration to g/m**3.
      if(LBACK) then
         open(in4,file=backdat,status='old')
         read(in4,*) confact
      endif
c
c  Set up arrays for RING output (place call right after QAINP)
      if(LDRING) call RINGSET(io1)
c
c  Report field of processed receptors to 'plot' file POSTREC.DAT
      if(LDEBUG .OR. msrc.EQ.2) call PLTREC(ngrecp,ndrecp)
c
c  Set up arrays for time-series output
      if(LTIME .OR. LPEAK) call TSERSET(io1)
c
c  Prepare background visibility data
      if(LVISIB) then
c  Set background extinction coefficients for visibility calculation
         call SETVISBK
         if(LVSR) then
c  Open visual range measurements file for MVISBK=4,5; set type of data
            open(in5,file=vsrdat,status='old')
            call GETHDVSR(in5)
         elseif(mvisbk.EQ.7) then
c  Open weather observation file and preprocess station data
            open(in5,file=vsrdat,status='old')
            call PREPWX
         endif
      endif
c
c  Write the files used in this run to the list file
      call WRFILES

c  Perform setup for CALM processing option
      if(mcalmpro.EQ.1) then
c ---    Open single-point met data file for CALM processing
         open(in7,file=met1dat,status='old')
c ---    Skip header record in AERMOD/AERMET SURFACE file
         if(met1fmt.EQ.1) read(in7,*)
c ---    Set minimum number of values in each averaging period
         min3=3-INT(fcalm*3.0)
         min24=24-INT(fcalm*24.0)
         minN=navg-INT(fcalm*FLOAT(navg))
      else
         min3=0
         min24=0
         minN=0
         icalmflag=0
      endif

c
c  Set minimum dimensions of concentration arrays (for reads in GETRCRD)
      idimx=MAX(1,ngx)
      idimy=MAX(1,ngy)
      idimd=MAX(1,ndrec)
      idimt=MAX(1,nctrec)
c
c  Set similar dimension for sources
      idims=MAX(1,nsrc)
c
c  Initialize end of file flag for reading data for each period
      ieof=0

c  Set the start and end times eXpected for the first period processed
      nsecstep=mavgpd*nsecdt
      ibyrx=isyr
      ibjdx=jsday
      ibhrx=ishr
      ibscx=issec4
      idathrbx=ibyrx*100000+ibjdx*100+ibhrx
      call DEDAT(idathrbx,ieyrx,iejdx,iehrx)
      iescx=ibscx
      call INCRS(io1,ieyrx,iejdx,iehrx,iescx,nsecstep)
      idathrex=ieyrx*100000+iejdx*100+iehrx

c  Skip to starting time for processing concentrations (/conc/)
      if(isyr .NE. msyr .OR. jsday .NE. mjsday .OR.
     *   ishr .NE. mshr .OR. issec4 .NE. mssec) then
         do i=1,nskip
            call SKIPREC(in1,tcg,tcd,tct,idimx,idimy,idimd,idimt,ieof)
            if(ieof.EQ.1) then
               write(io1,*)'          --- <<!!!>> ---                 '
               write(io1,*)'------------------------------------------'
               write(io1,*)'End Of File found when skipping period ',i
               write(io1,*)'------------------------------------------'
               stop
            endif
         enddo
      endif
c
c  Initialize period counter (iper)
      iper=0
c
c  Initialize number of values in each average to zero
      num3=0
      num24=0
      numN=0
      numRL=0
c
c  Set maximum number of periods to process
      mxper=nper
c
c  Initialize logicals for updating start-times to T
      LT1HR=.TRUE.
      LT3HR=.TRUE.
      LT24HR=.TRUE.
      LTNAVG=.TRUE.
      LTRUNL=.TRUE.
c
c  Set initial values for Top-50 tables
      call INIT50(vinit,t50c1,it501,ct501,t50xy1,t50b1,t50h1)
      call INIT50(vinit,t50c3,it503,ct503,t50xy3,t50b3,t50h3)
      call INIT50(vinit,t50c24,it5024,ct5024,t50xy24,t50b24,t50h24)
      call INIT50(vinit,t50cN,it50N,ct50N,t50xyN,t50bN,t50hN)
      call INIT50(vinit,t50cR,it50R,ct50R,t50xyR,t50bR,t50hR)
      call INIT50(vinit,t50cP,it50P,ct50P,t50xyP,t50bP,t50hP)

c  Set initial values for Top-N tables
      if(msrc.EQ.2) then
         idim=idims
      else
         idim=idimd
      endif
      do n=1,mxrnk
         do m=1,idimy
            do l=1,idimx
               tng1(l,m,n)=vinit
               tng3(l,m,n)=vinit
               tng24(l,m,n)=vinit
               tngN(l,m,n)=vinit
               tngP(l,m,n)=vinit
               do k=1,2
                  ng1(k,l,m,n)=0
                  ng3(k,l,m,n)=0
                  ng24(k,l,m,n)=0
                  ngN(k,l,m,n)=0
                  ngP(k,l,m,n)=0
               enddo
            enddo
         enddo
         do m=1,idim
            tnd1(m,n)=vinit
            tnd3(m,n)=vinit
            tnd24(m,n)=vinit
            tndN(m,n)=vinit
            tndP(m,n)=vinit
            do k=1,2
               nd1(k,m,n)=0
               nd3(k,m,n)=0
               nd24(k,m,n)=0
               ndN(k,m,n)=0
               ndP(k,m,n)=0
            enddo
         enddo
         do m=1,idimt
            tnt1(m,n)=vinit
            tnt3(m,n)=vinit
            tnt24(m,n)=vinit
            tntN(m,n)=vinit
            tntP(m,n)=vinit
            do k=1,2
               nt1(k,m,n)=0
               nt3(k,m,n)=0
               nt24(k,m,n)=0
               ntN(k,m,n)=0
               ntP(k,m,n)=0
            enddo
         enddo
      enddo
c
c  Set averaging/summing arrays to zero
      call RESET(avRcg,avRcd,avRct)
      call RESET(avRbg,avRbd,avRbt)
      call RESET(avRhg,avRhd,avRht)
      call RESET(avRpg,avRpd,avRpt)
      call RESET(avRng,avRnd,avRnt)
      call RESET(avNcg,avNcd,avNct)
      call RESET(avNbg,avNbd,avNbt)
      call RESET(avNhg,avNhd,avNht)
      call RESET(avNpg,avNpd,avNpt)
      call RESET(av24cg,av24cd,av24ct)
      call RESET(av24bg,av24bd,av24bt)
      call RESET(av24hg,av24hd,av24ht)
      call RESET(av24pg,av24pd,av24pt)
      call RESET(av24ng,av24nd,av24nt)
      call RESET(av3cg,av3cd,av3ct)
      call RESET(av3bg,av3bd,av3bt)
      call RESET(av3hg,av3hd,av3ht)
      call RESET(av3pg,av3pd,av3pt)
      call RESET(av1cg,av1cd,av1ct)
      call RESET(av1bg,av1bd,av1bt)
      call RESET(av1hg,av1hd,av1ht)
      call RESET(av1pg,av1pd,av1pt)

      call RESET(av24s4g,av24s4d,av24s4t)
      call RESET(av24n3g,av24n3d,av24n3t)
      call RESET(av24n2g,av24n2d,av24n2t)
      call RESET(av24ocg,av24ocd,av24oct)
      call RESET(av24ecg,av24ecd,av24ect)
      call RESET(av24pcg,av24pcd,av24pct)
      call RESET(av24pfg,av24pfd,av24pft)

      do k=1,nospec
         call RESET(avRacg(1,1,k),avRacd(1,k),avRact(1,k))
         call RESET(avRang(1,1,k),avRand(1,k),avRant(1,k))
         call RESET(avNacg(1,1,k),avNacd(1,k),avNact(1,k))
         call RESET(av24acg(1,1,k),av24acd(1,k),av24act(1,k))
         call RESET(av24ang(1,1,k),av24and(1,k),av24ant(1,k))
         call RESET(av3acg(1,1,k),av3acd(1,k),av3act(1,k))
         call RESET(av1acg(1,1,k),av1acd(1,k),av1act(1,k))
      enddo

c  Set counting arrays to zero
      call RESETI(ixc1g,ixc1d,ixc1t)
      call RESETI(ixc3g,ixc3d,ixc3t)
      call RESETI(ixc24g,ixc24d,ixc24t)
      call RESETI(ixcNg,ixcNd,ixcNt)
      call RESETI(ixcPg,ixcPd,ixcPt)

c
c  Write header information for "echo" output to list file
      if(LECHO) then
         call ECHO_HD(io1)
      endif
c
c  Open file(s) and write header information for "time-series" output
      call TSEROPEN
      if(LTIME) then
         if(L1PD) call TSER_HD(iotp,1)
         if(L1HR) call TSER_HD(iot1,iper1)
         if(L3HR) call TSER_HD(iot3,iper3)
         if(L24HR) call TSER_HD(iot24,iper24)
         if(LNAVG) call TSER_HD(iotn,navg)
      endif
      if(LPEAK) then
         if(L1PD) call PEAK_HD(iopp,1)
         if(L1HR) call PEAK_HD(iop1,iper1)
         if(L3HR) call PEAK_HD(iop3,iper3)
         if(L24HR) call PEAK_HD(iop24,iper24)
         if(LNAVG) call PEAK_HD(iopn,navg)
      endif

c  Write header for visibility output to scratch files
      if(LVISIB) then
         open(iox1,status='scratch')
         open(iox2,status='scratch')
         open(iox3,status='scratch')
         open(iox4,status='scratch')
         call WRIVL(iox1,version,level,1)
         call WRIVL(iox2,version,level,1)
         call WRIVL(iox3,version,level,1)
         call WRIVL(iox4,version,level,1)
         call WRIVIS
         if(mdvis.GT.0) then
            open(iodv,file=dvisdat,status='unknown')
            call DVIS_HD
         endif
      endif
c
c  Loop over averaging periods --------------------------
10    iper=iper+1
c
c --- Read modeled data
c
c  Process every "NREP" period of data
      do iev=1,nrep
         call GETRCRD(in1,in3,tcg,tcd,tct,idimx,idimy,idimd,idimt,ieof)
         if(ieof.EQ.1) then
            mxper=iper-1
            goto 11
         endif
c ---    Test timestamp of these data against expected timestamp
         call TTEST(idathrbx,ibscx,idathrex,iescx)
c ---    Increment expected timestamps to next period
         call TUP(io1,idathrbx,ibscx,idathrex,iescx,nsecstep)
      enddo

c  Get corresponding CALM processing flag for this period
      if(mcalmpro.EQ.1) call GETCALM

c  Convert mid-night end-time to 2400 for reporting purposes
      if(mhre.EQ.0 .AND. msece.EQ.0) then
         myr=myre
         mjul=mjdaye
         call MIDNITE(io1,'TO 24h',myr,-99,-99,mjul,
     &                             myre,mmoe,mdaye,mjdaye)
         mhre=24
      endif

c  Make sure mid-night start-time is not 2400
      if(mhrb.EQ.24 .AND. msecb.EQ.0) then
         myr=myrb
         mjul=mjdayb
         call MIDNITE(io1,'TO 00h',myr,-99,-99,mjul,
     &                             myrb,mmob,mdayb,mjdayb)
         mhrb=0
      endif

c  Update starting time for averages when appropriate
      if(LT1HR) then
         myr1HR=myrb
         mjday1HR=mjdayb
         mhr1HR=mhrb
         msec1HR=msecb
      endif
      if(LT3HR) then
         myr3HR=myrb
         mjday3HR=mjdayb
         mhr3HR=mhrb
         msec3HR=msecb
      endif
      if(LT24HR) then
         myr24HR=myrb
         mjday24HR=mjdayb
         mhr24HR=mhrb
         msec24HR=msecb
      endif
      if(LTNAVG) then
         myrNAVG=myrb
         mjdayNAVG=mjdayb
         mhrNAVG=mhrb
         msecNAVG=msecb
      endif
      if(LTRUNL) then
         myrRUNL=myrb
         mjdayRUNL=mjdayb
         mhrRUNL=mhrb
         msecRUNL=msecb
      endif
      LT1HR=.FALSE.
      LT3HR=.FALSE.
      LT24HR=.FALSE.
      LTNAVG=.FALSE.
      LTRUNL=.FALSE.

c
c  Apply user-specified scaling (if required) in internal CALPUFF
c  units (g/m**3 for concentration)
      if(LSCALE) call SCALE
      if(LBACK) call BACK(confact)
c
c  Section for nper*mavg averages ("length-of-Run")
      if(LRUNL .OR. LVISIB) then
         if(icalmflag.EQ.0) numRL=numRL+1
         call SUMACONC(avRacg,avRacd,avRact)
         if(LVSR .OR. (LVISIB .AND. mvisbk.EQ.3)) then
c ---       Visibility processing 3: track number of values in sum
            call SUMVISB(avRcg,avRcd,avRct,avRbg,avRbd,avRbt,
     *                   avRhg,avRhd,avRht,avRng,avRnd,avRnt,
     *                   avRpg,avRpd,avRpt)
         elseif(LVISIB .AND. mvisbk.EQ.8 .AND. m8_mode.EQ.3) then
c ---       NOTHING:  daily extinctions are added in 24-hr section
         elseif(LVISIB .AND. mvisbk.EQ.8 .AND. m8_mode.EQ.5) then
c ---       NOTHING:  daily extinctions are added in 24-hr section
         else
            call SUMCONC(avRcg,avRcd,avRct,avRbg,avRbd,avRbt,
     *                   avRhg,avRhd,avRht,avRpg,avRpd,avRpt)
         endif
      endif
c
c  Section for navg*mavg averages
      if(LNAVG) then
         if(icalmflag.EQ.0) numN=numN+1
         call SUMACONC(avNacg,avNacd,avNact)
         call SUMCONC(avNcg,avNcd,avNct,avNbg,avNbd,avNbt,
     *                avNhg,avNhd,avNht,avNpg,avNpd,avNpt)
         if(MOD(iper,navg) .EQ. 0) then
            numN=MAX(numN,minN)
            call AVGACONC(numN,avNacg,avNacd,avNact)
            call AVGCONC(numN,avNcg,avNcd,avNct,avNbg,avNbd,avNbt,
     *                        avNhg,avNhd,avNht,avNpg,avNpd,avNpt)
            numN=0
            if(LEXCD) call COUNTX(sthreshN,avNcg,avNcd,avNct,
     *                            ixcNg,ixcNd,ixcNt)
            if(LT50) call UPT50(myrNAVG,mjdayNAVG,mhrNAVG,msecNAVG,
     *                          avNcg,avNcd,avNct,t50cN,it50N,ct50N,
     *                          t50xyN,avNbg,avNbd,avNbt,t50bN,
     *                          avNhg,avNhd,avNht,t50hN)
            if(LTOPN) call UPTN(myrNAVG,mjdayNAVG,mhrNAVG,msecNAVG,
     *                          avNcg,avNcd,avNct,tngN,tndN,tntN,
     *                          ngN,ndN,ntN)

            if((LECHO.OR.LTIME.OR.LPEAK).AND.iecho(mjdayNAVG).EQ.1) then
c ---          Scale output to user units before reporting
               call SUNIT(avNcg,avNcd,avNct)
               if(LDRING) then
c ---             Process averages for ring output
                  call AVGRING(io1,avNcd,avNcr)
               endif
               if(LECHO) then
                  call ECHO(io1,navg,avNcg,avNcd,avNct,avNcr,
     *                      myrNAVG,mjdayNAVG,mhrNAVG,msecNAVG)
                  if(LPLT) call PLTECHO(navg,avNcg,avNcd,avNct,
     *                      myrNAVG,mjdayNAVG,mhrNAVG,msecNAVG)
               endif
               if(LTIME) then
                  call TSEROUT(iotn,avNcg,avNcd,avNct,avNcr,
     *                         myrNAVG,mjdayNAVG,mhrNAVG,msecNAVG)
               endif
               if(LPEAK) then
                  call PEAKOUT(iopn,avNcg,avNcd,avNct,avNcr,
     *                         myrNAVG,mjdayNAVG,mhrNAVG,msecNAVG)
               endif
            endif
            call RESET(avNcg,avNcd,avNct)
            call RESET(avNbg,avNbd,avNbt)
            call RESET(avNhg,avNhd,avNht)
            call RESET(avNpg,avNpd,avNpt)
            do i=1,nospec
               call RESET(avNacg(1,1,k),avNacd(1,k),avNact(1,k))
            enddo
            LTNAVG=.TRUE.
         endif
      endif
c
c  Section for 24-hour averages
      if(L24HR .OR. LVISIB) then
         if(icalmflag.EQ.0) num24=num24+1
         call SUMACONC(av24acg,av24acd,av24act)
         if(LVSR .OR. (LVISIB .AND. mvisbk.EQ.3)) then
c ---       Visibility processing 3: track number of values in daily sum
            lfull=.FALSE.
            call SUMVISB(av24cg,av24cd,av24ct,av24bg,av24bd,av24bt,
     *                   av24hg,av24hd,av24ht,av24ng,av24nd,av24nt,
     *                   av24pg,av24pd,av24pt)
c ---       Sum extinction by species for daily averages
            call SUMEX24(lfull)
         elseif(LVISIB .AND. mvisbk.EQ.8 .AND. m8_mode.EQ.3) then
c ---       NOTHING:  24-hr concentration averages done by SUMACONC
         elseif(LVISIB .AND. mvisbk.EQ.8 .AND. m8_mode.EQ.5) then
c ---       NOTHING:  24-hr concentration averages done by SUMACONC
         elseif(LVISIB) then
            lfull=.TRUE.
            call SUMCONC(av24cg,av24cd,av24ct,av24bg,av24bd,av24bt,
     *                   av24hg,av24hd,av24ht,av24pg,av24pd,av24pt)
c ---       Sum extinction by species for daily averages
            call SUMEX24(lfull)
         else
            lfull=.TRUE.
            call SUMCONC(av24cg,av24cd,av24ct,av24bg,av24bd,av24bt,
     *                   av24hg,av24hd,av24ht,av24pg,av24pd,av24pt)
         endif
c
         if(MOD(iper,iper24) .EQ. 0) then
            num24=MAX(num24,min24)
            call AVGACONC(num24,av24acg,av24acd,av24act)
            if(LVSR .OR. (LVISIB .AND. mvisbk.EQ.3)) then
c ---          Visibility processing 3: use number of values in sum
               call AVGVISB(iper24,av24cg,av24cd,av24ct,av24bg,av24bd,
     *                      av24bt,av24hg,av24hd,av24ht,av24ng,av24nd,
     *                      av24nt,av24pg,av24pd,av24pt)
            elseif(LVISIB .AND. mvisbk.EQ.8 .AND. m8_mode.EQ.3) then
c ---          NOTHING:  24-hr concentration averages done by AVGACONC
            elseif(LVISIB .AND. mvisbk.EQ.8 .AND. m8_mode.EQ.5) then
c ---          NOTHING:  24-hr concentration averages done by AVGACONC
            else
               call AVGCONC(num24,av24cg,av24cd,av24ct,av24bg,av24bd,
     *                av24bt,av24hg,av24hd,av24ht,av24pg,av24pd,av24pt)
            endif
            num24=0
            if(LVISIB) then
               if(mvisbk.EQ.8 .AND.
     &           (m8_mode.EQ.3 .OR. m8_mode.EQ.5)) then
c ---             24-hour extinctions from 24-hour concentrations
                  call BEXT_24(av24acg,av24acd,av24act,
     *                 myr24HR,mjday24HR,mhr24HR,msec24HR,
     *                 av24cg,av24cd,av24ct,av24bg,av24bd,av24bt,
     *                 av24hg,av24hd,av24ht,av24pg,av24pd,av24pt)
c ---             Drop 24-hour extinctions into run-length sums
                  call V24_RUNL(av24cg,av24cd,av24ct,av24bg,av24bd,
     *                 av24bt,av24hg,av24hd,av24ht,av24pg,av24pd,av24pt,
     *                 avRcg,avRcd,avRct,avRbg,avRbd,avRbt,
     *                 avRhg,avRhd,avRht,avRng,avRnd,avRnt,
     *                 avRpg,avRpd,avRpt)
               else
                  call AVGEX24(lfull,av24ng,av24nd,av24nt)
               endif
               call OUTVIS(iper24,av24cg,av24cd,av24ct,av24bg,
     *                 av24bd,av24bt,av24hg,av24hd,av24ht,
     *                 av24pg,av24pd,av24pt,
     *                 myr24HR,mjday24HR,mhr24HR,msec24HR)
               if(mdvis.GE.1 .AND. mdvis.LE.2) then
                  if(LAVER) then
                     call OUTDVIS(1,av24pg,av24pd,av24pt,av24bg,
     *                            av24bd,av24bt,
     *                            myr24HR,mjday24HR,mhr24HR,msec24HR)
                  else
                     call OUTDVIS(0,av24cg,av24cd,av24ct,av24bg,
     *                            av24bd,av24bt,
     *                            myr24HR,mjday24HR,mhr24HR,msec24HR)
                  endif
               endif
            endif
            if(LEXCD) call COUNTX(sthresh24,av24cg,av24cd,av24ct,
     *                             ixc24g,ixc24d,ixc24t)
            if(LT50) call UPT50(myr24HR,mjday24HR,mhr24HR,msec24HR,
     *                          av24cg,av24cd,av24ct,t50c24,it5024,
     *                          ct5024,t50xy24,
     *                          av24bg,av24bd,av24bt,t50b24,
     *                          av24hg,av24hd,av24ht,t50h24)
            if(LTOPN) call UPTN(myr24HR,mjday24HR,mhr24HR,msec24HR,
     *                          av24cg,av24cd,av24ct,tng24,tnd24,
     *                          tnt24,ng24,nd24,nt24)
            if((L24ECHO.OR.L24TSER.OR.L24PEAK) .AND.
     &          iecho(mjday24HR) .EQ. 1) then
c ---          Scale ouput to user units before reporting
               call SUNIT(av24cg,av24cd,av24ct)
               if(LDRING) then
c ---             Process averages for ring output
                  call AVGRING(io1,av24cd,av24cr)
               endif
               if(L24ECHO) then
                  call ECHO(io1,iper24,av24cg,av24cd,av24ct,av24cr,
     *                      myr24HR,mjday24HR,mhr24HR,msec24HR)
                  if(LPLT) call PLTECHO(iper24,av24cg,av24cd,av24ct,
     *                      myr24HR,mjday24HR,mhr24HR,msec24HR)
               endif
               if(L24TSER) then
                  call TSEROUT(iot24,av24cg,av24cd,av24ct,av24cr,
     &                         myr24HR,mjday24HR,mhr24HR,msec24HR)
               endif
               if(L24PEAK) then
                  call PEAKOUT(iop24,av24cg,av24cd,av24ct,av24cr,
     &                         myr24HR,mjday24HR,mhr24HR,msec24HR)
               endif
            endif
            call RESET(av24cg,av24cd,av24ct)
            call RESET(av24bg,av24bd,av24bt)
            call RESET(av24hg,av24hd,av24ht)
            call RESET(av24pg,av24pd,av24pt)
            LT24HR=.TRUE.
            if(LVISIB) then
               call RESET(av24s4g,av24s4d,av24s4t)
               call RESET(av24n3g,av24n3d,av24n3t)
               call RESET(av24n2g,av24n2d,av24n2t)
               call RESET(av24ocg,av24ocd,av24oct)
               call RESET(av24ecg,av24ecd,av24ect)
               call RESET(av24pcg,av24pcd,av24pct)
               call RESET(av24pfg,av24pfd,av24pft)
               if(LVSR.OR.mvisbk.EQ.3)
     &           call RESET(av24ng,av24nd,av24nt)
            endif
            do k=1,nospec
               call RESET(av24acg(1,1,k),av24acd(1,k),av24act(1,k))
               call RESET(av24ang(1,1,k),av24and(1,k),av24ant(1,k))
            enddo
         endif
      endif
c
c  Section for 3-hour averages
      if(L3HR) then
         if(icalmflag.EQ.0) num3=num3+1
         call SUMACONC(av3acg,av3acd,av3act)
         call SUMCONC(av3cg,av3cd,av3ct,av3bg,av3bd,av3bt,
     *                av3hg,av3hd,av3ht,av3pg,av3pd,av3pt)
         if(MOD(iper,iper3) .EQ. 0) then
            num3=MAX(num3,min3)
            call AVGACONC(num3,av3acg,av3acd,av3act)
            call AVGCONC(num3,av3cg,av3cd,av3ct,av3bg,av3bd,av3bt,
     *                     av3hg,av3hd,av3ht,av3pg,av3pd,av3pt)
            num3=0
            if(LEXCD) call COUNTX(sthresh3,av3cg,av3cd,av3ct,ixc3g,
     *                            ixc3d,ixc3t)
            if(LT50) call UPT50(myr3HR,mjday3HR,mhr3HR,msec3HR,
     *                          av3cg,av3cd,av3ct,t50c3,it503,ct503,
     *                          t50xy3,av3bg,av3bd,av3bt,t50b3,
     *                          av3hg,av3hd,av3ht,t50h3)
            if(LTOPN) call UPTN(myr3HR,mjday3HR,mhr3HR,msec3HR,
     *                          av3cg,av3cd,av3ct,tng3,tnd3,tnt3,
     *                          ng3,nd3,nt3)
            if((LECHO.OR.LTIME.OR.LPEAK).AND.iecho(mjday3HR).EQ.1) then
c ---          Scale output to user units before reporting
               call SUNIT(av3cg,av3cd,av3ct)
               if(LDRING) then
c ---             Process averages for ring output
                  call AVGRING(io1,av3cd,av3cr)
               endif
               if(LECHO) then
                  call ECHO(io1,iper3,av3cg,av3cd,av3ct,av3cr,
     *                      myr3HR,mjday3HR,mhr3HR,msec3HR)
                  if(LPLT) call PLTECHO(iper3,av3cg,av3cd,av3ct,
     *                      myr3HR,mjday3HR,mhr3HR,msec3HR)
               endif
               if(LTIME) then
                  call TSEROUT(iot3,av3cg,av3cd,av3ct,av3cr,
     *                         myr3HR,mjday3HR,mhr3HR,msec3HR)
               endif
               if(LPEAK) then
                  call PEAKOUT(iop3,av3cg,av3cd,av3ct,av3cr,
     *                         myr3HR,mjday3HR,mhr3HR,msec3HR)
               endif
            endif
            call RESET(av3cg,av3cd,av3ct)
            call RESET(av3bg,av3bd,av3bt)
            call RESET(av3hg,av3hd,av3ht)
            call RESET(av3pg,av3pd,av3pt)
            LT3HR=.TRUE.
            do k=1,nospec
               call RESET(av3acg(1,1,k),av3acd(1,k),av3act(1,k))
            enddo
         endif
      endif
c
c
c  Section for 1-hour averages
      if(L1HR) then
         call SUMACONC(av1acg,av1acd,av1act)
         call SUMCONC(av1cg,av1cd,av1ct,av1bg,av1bd,av1bt,
     *                av1hg,av1hd,av1ht,av1pg,av1pd,av1pt)
         if(MOD(iper,iper1) .EQ. 0) then
            call AVGACONC(iper1,av1acg,av1acd,av1act)
            call AVGCONC(iper1,av1cg,av1cd,av1ct,av1bg,av1bd,av1bt,
     *                     av1hg,av1hd,av1ht,av1pg,av1pd,av1pt)
            if(LEXCD) call COUNTX(sthresh1,av1cg,av1cd,av1ct,ixc1g,
     *                            ixc1d,ixc1t)
            if(LT50) call UPT50(myr1HR,mjday1HR,mhr1HR,msec1HR,
     *                          av1cg,av1cd,av1ct,t50c1,it501,ct501,
     *                          t50xy1,av1bg,av1bd,av1bt,t50b1,
     *                          av1hg,av1hd,av1ht,t50h1)
            if(LTOPN) call UPTN(myr1HR,mjday1HR,mhr1HR,msec1HR,
     *                          av1cg,av1cd,av1ct,tng1,tnd1,tnt1,
     *                          ng1,nd1,nt1)
            if(LVISIB) then
               if(mdvis.GE.3 .AND. mdvis.LE.4) then
                  call OUTDVIS(0,concg,concd,conct,gbext0,dbext0,tbext0,
     *                         myr1HR,mjday1HR,mhr1HR,msec1HR)
               endif
            endif
            if((LECHO.OR.LTIME.OR.LPEAK).AND.iecho(mjday1HR).EQ.1) then
c ---          Scale output to user units before reporting
               call SUNIT(av1cg,av1cd,av1ct)
               if(LDRING) then
c ---             Process averages for ring output
                  call AVGRING(io1,av1cd,av1cr)
               endif
               if(LECHO) then
                  call ECHO(io1,iper1,av1cg,av1cd,av1ct,av1cr,
     *                      myr1HR,mjday1HR,mhr1HR,msec1HR)
                  if(LPLT) call PLTECHO(iper1,av1cg,av1cd,av1ct,
     *                      myr1HR,mjday1HR,mhr1HR,msec1HR)
               endif
               if(LTIME) then
                  call TSEROUT(iot1,av1cg,av1cd,av1ct,av1cr,
     *                         myr1HR,mjday1HR,mhr1HR,msec1HR)
               endif
               if(LPEAK) then
                  call PEAKOUT(iop1,av1cg,av1cd,av1ct,av1cr,
     *                         myr1HR,mjday1HR,mhr1HR,msec1HR)
               endif
            endif
            call RESET(av1cg,av1cd,av1ct)
            call RESET(av1bg,av1bd,av1bt)
            call RESET(av1hg,av1hd,av1ht)
            call RESET(av1pg,av1pd,av1pt)
            LT1HR=.TRUE.
            do k=1,nospec
               call RESET(av1acg(1,1,k),av1acd(1,k),av1act(1,k))
            enddo
         endif
      endif
c
c  Section for 1-period averages (directly from file -- no averaging)
      if(L1PD) then
         if(LEXCD) call COUNTX(sthresh1,concg,concd,conct,ixcPg,
     &                         ixcPd,ixcPt)
         if(LT50) call UPT50(myrb,mjdayb,mhrb,msecb,
     *                       concg,concd,conct,t50cP,it50P,ct50P,
     *                       t50xyP,gbext0,dbext0,tbext0,t50bP,
     *                       gfrh,dfrh,tfrh,t50hP)
         if(LTOPN) call UPTN(myrb,mjdayb,mhrb,msecb,
     *                       concg,concd,conct,tngP,tndP,tntP,
     *                       ngP,ndP,ntP)
         if((LECHO.OR.LTIME.OR.LPEAK).AND.iecho(mjdayb).EQ.1) then
c ---       Scale output to user units before reporting (1-pd arrays
c ---       are not reset, so they must be reported LAST after all
c ---       summing is done)
            call SUNIT(concg,concd,conct)
            if(LDRING) then
c ---          Process averages for ring output
               call AVGRING(io1,concd,avPcr)
            endif
            if(LECHO) then
               call ECHO(io1,1,concg,concd,conct,avPcr,
     *                   myrb,mjdayb,mhrb,msecb)
               if(LPLT) call PLTECHO(1,concg,concd,conct,
     *                               myrb,mjdayb,mhrb,msecb)
            endif
            if(LTIME) then
               call TSEROUT(iotp,concg,concd,conct,avPcr,
     *                      myrb,mjdayb,mhrb,msecb)
            endif
            if(LPEAK) then
               call PEAKOUT(iopp,concg,concd,conct,avPcr,
     *                      myrb,mjdayb,mhrb,msecb)
            endif
         endif
      endif
c
c --- Process daily exceedance counts if this is last hour of day
      if(mhre.EQ.24 .AND. LXDAY) then
         if(LXDAY1) then
            call CNTXDAY(ixc1g,ixc1d,ixc1t,myre,mjdaye,
     *                   ixcday,ixcdayg,ixcdayd,ixcdayt)
         elseif(LXDAY3) then
            call CNTXDAY(ixc3g,ixc3d,ixc3t,myre,mjdaye,
     *                   ixcday,ixcdayg,ixcdayd,ixcdayt)
         elseif(LXDAYN) then
            call CNTXDAY(ixcNg,ixcNd,ixcNt,myre,mjdaye,
     *                   ixcday,ixcdayg,ixcdayd,ixcdayt)
         elseif(LXDAY24) then
            call CNTXDAY(ixc24g,ixc24d,ixc24t,myre,mjdaye,
     *                   ixcday,ixcdayg,ixcdayd,ixcdayt)
         endif
      endif
c
c --- Next period
      if(iper.LT.mxper .OR. metrun.EQ.1) goto 10
11    continue


c  ------------------------Exit period loop --------------------------
c
c  Average/Update length-of-run values
c -------------------------------------
      if(LRUNL .OR. LVISIB) then
c ---    Do not place floor on number of hours in run-length average
c ---    numRL=MAX(numRL,minRL)
         call AVGACONC(numRL,avRacg,avRacd,avRact)
         if(LVSR .OR. (LVISIB .AND. mvisbk.EQ.3)) then
c ---       Visibility Method 3: use number of values in sum
            call AVGVISB(mxper,avRcg,avRcd,avRct,avRbg,avRbd,
     *                   avRbt,avRhg,avRhd,avRht,avRng,avRnd,
     *                   avRnt,avRpg,avRpd,avRpt)
         elseif(LVISIB .AND. mvisbk.EQ.8 .AND.
     &         (m8_mode.EQ.3 .OR. m8_mode.EQ.5)) then
c ---       Visibility Method 8 Mode 3/5: use number of values in sum
c ---       Valid fraction test needs days, not model periods
            nvalid=mxper/iper24
            call AVGVISB(nvalid,avRcg,avRcd,avRct,avRbg,avRbd,
     *                   avRbt,avRhg,avRhd,avRht,avRng,avRnd,
     *                   avRnt,avRpg,avRpd,avRpt)
         else
            call AVGCONC(numRL,avRcg,avRcd,avRct,avRbg,avRbd,
     *                   avRbt,avRhg,avRhd,avRht,avRpg,avRpd,avRpt)
         endif
      endif

      if(LT50.AND.LRUNL) call UPT50(myrRUNL,mjdayRUNL,mhrRUNL,msecRUNL,
     *                              avRcg,avRcd,avRct,
     *                              t50cR,it50R,ct50R,t50xyR,
     *                              avRbg,avRbd,avRbt,t50bR,
     *                              avRhg,avRhd,avRht,t50hR)
c
c  Process run-length visibility results  
c ---------------------------------------
      if(LVISIB) then
         izero=0
         call OUTVIS(mxper,avRcg,avRcd,avRct,avRbg,avRbd,avRbt,
     *               avRhg,avRhd,avRht,avRpg,avRpd,avRpt,
     *               izero,izero,izero,izero)
      endif
c
c  Print out top-50 tables for each averaging period
c --------------------------------------------------
      if(LT50) then
         if(L1PD) then
            call WRIVL(io1,version,level,1)
            call WRIT50(io1,1,t50cP,it50P,ct50P,t50xyP,t50bP,t50hP)
         endif
         if(L1HR) then
            call WRIVL(io1,version,level,1)
            call WRIT50(io1,iper1,t50c1,it501,ct501,t50xy1,t50b1,t50h1)
         endif
         if(L3HR) then
            call WRIVL(io1,version,level,1)
            call WRIT50(io1,iper3,t50c3,it503,ct503,t50xy3,t50b3,t50h3)
         endif
         if(L24HR) then
            call WRIVL(io1,version,level,1)
            call WRIT50(io1,iper24,t50c24,it5024,ct5024,t50xy24,t50b24,
     *                  t50h24)
         endif
         if(LNAVG) then
            call WRIVL(io1,version,level,1)
            call WRIT50(io1,navg,t50cN,it50N,ct50N,t50xyN,t50bN,t50hN)
         endif
         if(LRUNL) then
            call WRIVL(io1,version,level,1)
            call WRIT50(io1,mxper,t50cR,it50R,ct50R,t50xyR,t50bR,t50hR)
         endif
      endif
c
c  Print out top-N tables and plot-files for each averaging period
c ----------------------------------------------------------------
      if(LTOPN) then
         if(L1PD) then
c ---       Scale to user units before reporting
c ---       Loop over requested ranks
            do n=1,ntop
               irnk=itop(n)
               call SUNIT(tngP(1,1,irnk),tndP(1,irnk),tntP(1,irnk))
            enddo
            call WRIVL(io1,version,level,1)
            call WRITOPN(io1,1,tngP,tndP,tntP,ngP,ndP,ntP)
            if(LPLT) then
               if(LGRD) then
c ---             Loop over requested ranks
                  do n=1,ntop
                     irnk=itop(n)
                     call PLTTOPN(irnk,1,tngP(1,1,irnk),tndP(1,irnk),
     &                            tntP(1,irnk))
                  enddo
               else
c ---             Write all ranks to one DATA plot-file
                  call WRIMAP(1,tngP,tndP,tntP)
               endif
            endif
         endif
         if(L1HR) then
c ---       Scale to user units before reporting
c ---       Loop over requested ranks
            do n=1,ntop
               irnk=itop(n)
               call SUNIT(tng1(1,1,irnk),tnd1(1,irnk),tnt1(1,irnk))
            enddo
            call WRIVL(io1,version,level,1)
            call WRITOPN(io1,iper1,tng1,tnd1,tnt1,ng1,nd1,nt1)
            if(LPLT) then
               if(LGRD) then
c ---             Loop over requested ranks
                  do n=1,ntop
                     irnk=itop(n)
                     call PLTTOPN(irnk,iper1,tng1(1,1,irnk),
     &                            tnd1(1,irnk),tnt1(1,irnk))
                  enddo
               else
c ---             Write all ranks to one DATA plot-file
                  call WRIMAP(iper1,tng1,tnd1,tnt1)
               endif
            endif
         endif
         if(L3HR) then
c ---       Scale to user units before reporting
c ---       Loop over requested ranks
            do n=1,ntop
               irnk=itop(n)
               call SUNIT(tng3(1,1,irnk),tnd3(1,irnk),tnt3(1,irnk))
            enddo
            call WRIVL(io1,version,level,1)
            call WRITOPN(io1,iper3,tng3,tnd3,tnt3,ng3,nd3,nt3)
            if(LPLT) then
               if(LGRD) then
c ---             Loop over requested ranks
                  do n=1,ntop
                     irnk=itop(n)
                     call PLTTOPN(irnk,iper3,tng3(1,1,irnk),
     &                            tnd3(1,irnk),tnt3(1,irnk))
                  enddo
               else
c ---             Write all ranks to one DATA plot-file
                  call WRIMAP(iper3,tng3,tnd3,tnt3)
               endif
            endif
         endif
         if(L24HR) then
c ---       Scale to user units before reporting
c ---       Loop over requested ranks
            do n=1,ntop
               irnk=itop(n)
               call SUNIT(tng24(1,1,irnk),tnd24(1,irnk),tnt24(1,irnk))
            enddo
            call WRIVL(io1,version,level,1)
            call WRITOPN(io1,iper24,tng24,tnd24,tnt24,ng24,nd24,nt24)
            if(LPLT) then
               if(LGRD) then
c ---             Loop over requested ranks
                  do n=1,ntop
                     irnk=itop(n)
                     call PLTTOPN(irnk,iper24,tng24(1,1,irnk),
     &                            tnd24(1,irnk),tnt24(1,irnk))
                  enddo
               else
c ---             Write all ranks to one DATA plot-file
                  call WRIMAP(iper24,tng24,tnd24,tnt24)
               endif
            endif
         endif
         if(LNAVG) then
c ---       Scale to user units before reporting
c ---       Loop over requested ranks
            do n=1,ntop
               irnk=itop(n)
               call SUNIT(tngN(1,1,irnk),tndN(1,irnk),tntN(1,irnk))
            enddo
            call WRIVL(io1,version,level,1)
            call WRITOPN(io1,navg,tngN,tndN,tntN,ngN,ndN,ntN)
            if(LPLT) then
               if(LGRD) then
c ---             Loop over requested ranks
                  do n=1,ntop
                     irnk=itop(n)
                     call PLTTOPN(irnk,navg,tngN(1,1,irnk),tndN(1,irnk),
     &                            tntN(1,irnk))
                  enddo
               else
c ---             Write all ranks to one DATA plot-file
                  call WRIMAP(navg,tngN,tndN,tntN)
               endif
            endif
         endif
         if(LRUNL) then
c ---       Scale run-length values to user units before reporting
            call SUNIT(avRcg,avRcd,avRct)
            if(LDRING) then
c ---          Process averages for ring output
               call AVGRING(io1,avRcd,avRcr)
            endif
            call WRIVL(io1,version,level,1)
            call ECHO(io1,mxper,avRcg,avRcd,avRct,avRcr,
     &                myrb,mjdayb,mhrb,msecb)
            if(LPLT) then
               irnk=0
               call PLTTOPN(irnk,mxper,avRcg,avRcd,avRct)
            endif
         endif
      endif
c
c  Print out Exceedance Count tables and map-files for each ave. period
c ---------------------------------------------------------------------
      if(LEXCD) then
         if(L1PD) then
            call WRIVL(io1,version,level,1)
            call WRIEXC(io1,1,thresh1,ixcPg,ixcPd,ixcPt)
            if(LPLT) then
               call PLTEXC(1,thresh1,ixcPg,ixcPd,ixcPt)
            endif
         endif
         if(L1HR) then
            call WRIVL(io1,version,level,1)
            call WRIEXC(io1,iper1,thresh1,ixc1g,ixc1d,ixc1t)
            if(LPLT) then
               call PLTEXC(iper1,thresh1,ixc1g,ixc1d,ixc1t)
            endif
         endif
         if(L3HR) then
            call WRIVL(io1,version,level,1)
            call WRIEXC(io1,iper3,thresh3,ixc3g,ixc3d,ixc3t)
            if(LPLT) then
               call PLTEXC(iper3,thresh3,ixc3g,ixc3d,ixc3t)
            endif
         endif
         if(L24HR) then
            call WRIVL(io1,version,level,1)
            call WRIEXC(io1,iper24,thresh24,ixc24g,ixc24d,ixc24t)
            if(LPLT) then
               call PLTEXC(iper24,thresh24,ixc24g,ixc24d,ixc24t)
            endif
         endif
         if(LNAVG) then
            call WRIVL(io1,version,level,1)
            call WRIEXC(io1,navg,threshN,ixcNg,ixcNd,ixcNt)
            if(LPLT) then
               call PLTEXC(navg,threshN,ixcNg,ixcNd,ixcNt)
            endif
         endif
      endif
c
c  Report Visibility Summary tables from scratch files
c ------------------------------------------------------
      if(LVISIB) call VISSUMRY
c
c  Report Exceedance summary for multiple-day analysis
c -----------------------------------------------------
      if(LXDAY) then
         call WRIVL(io1,version,level,1)
         call XCDAY(ixcday,ixcdayg,ixcdayd,ixcdayt)
      endif
c
c  Print out Peak Value summary from top-N tables for each period
c ---------------------------------------------------------------
      if(LTOPN) then
         call WRIVL(io1,version,level,1)
         call WRISUM(io1)
         if(L1PD) then
            call WRIPEAK(io1,1,tngP,tndP,tntP,ngP,ndP,ntP)
         endif
         if(L1HR) then
            call WRIPEAK(io1,iper1,tng1,tnd1,tnt1,ng1,nd1,nt1)
         endif
         if(L3HR) then
            call WRIPEAK(io1,iper3,tng3,tnd3,tnt3,ng3,nd3,nt3)
         endif
         if(L24HR) then
            call WRIPEAK(io1,iper24,tng24,tnd24,tnt24,ng24,nd24,nt24)
         endif
         if(LNAVG) then
            call WRIPEAK(io1,navg,tngN,tndN,tntN,ngN,ndN,ntN)
         endif
         if(LRUNL) then
            call WRIPEAKR(io1,mxper,avRcg,avRcd,avRct)
         endif
      endif
c
c  Process SAMPLER results  
c ------------------------
      if(msampler.GT.0) then
         call AVGSAMP
         if(msampler.EQ.1) then
c ---       Output to list file
            call WRIVL(io1,version,level,1)
            call WRISAMP(io1)
         else
c ---       Output to plot DATA files
c ---       Loop over SAMPLER data record entries
            do k=1,nsdat
               isrec=irsamp(k)
               jsrec=0
               call DEDAT(iedathr(k),kyr,kjul,khr)
               call PLTECHO(naverage(k),concg,caverage(1,k),conct,
     *                      kyr,kjul,khr,0)
            enddo
         endif
      endif

c --- Identify number of periods found to be calm of missing for
c --- CALPRO processing option
      if(mcalmpro.EQ.1) then
         write(io1,'(/////,a30)') 'MCALMPRO Processing Option:   '
         write(io1,'(a35,i6)')
     &    '   Number of calms identified    = ',ncalmplm
         write(io1,'(a35,i6)')
     &    '   Number of missings identified = ',nmissplm
      endif
c
c  Finished -- Print closing message to screen
c --------------------------------------------
      call FIN(myre,mjdaye,mhre,msece)

      stop
      end
c-----------------------------------------------------------------------
      block data
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080724        BLOCK DATA
c ---           D. Strimaitis, J. Scire
c
c     PURPOSE:     Provides initial values or default values to members
c                  of common blocks used in CALPOST code.
c
c --- UPDATES:
c     V6.22(080709) to V6.221(080724)
c               (DGS) Add NCALMPLM, NMISSPLM
c               (DGS) EENO2 default changed from 0.17 to 0.1755
c               (DGS) Add MVISCHECK, AREANAME
c     V6.2(070712) to V6.22(080709)
c               (DGS) Add RHFSML,RHFLRG,RHFSEA
c     V6.144(070130) to V6.2(070712)
c               (DGS) Add NO2 calculation from NOx concentrations
c     V6.141(061120) to V6.144(070130)
c               (DGS) Add MCALMPRO calm processing option defaults for
c                     non-CALPUFF/CALGRID output
c     V6.14(061107) to V6.141(061120)
c               (DGS) Replace IRHTYPE with M8_MODE
c     V6.12(060309) to V6.14(061107)
c               (DGS) Initialize Method 8 extinction efficiencies,
c                     background sea salt concentrations, and type of
c                     RH data in /VISB/
c               (DGS) Initialize NO2 variables in /VISB/
c     V6.1(050915) to V6.12(060309)
c               (DGS) Initialize NRNKV24 in /CTRL/
c               (DGS) Initialize MDVIS in /CTRL/
c               (DGS) Initialize DVISDAT in /FILNAM/
c     V5.65(050729) to V6.1(050915)
c               (DGS) Initialize NAVG[H,M,S] in /CTRL/
c               (DGS) Initialize ismin,ieyr,iemo,iedy,jeday,iehr,iemin,
c                     iesec in /CTRL/
c               (DGS) BTZONE stored in /CTRL/,BTZONE2 stored in /VISB/
c     V5.638(050408) to V5.65(050729)
c               (DGS) Add SAMPLER processing option (MSAMPLER)
c     V5.633(041202) to V5.638(050408)
c               (DGS) Initialize LPEAK in /CTRL/
c     V5.632(041130) to V5.633(041202)
c               (CEC) Initialize LAVER
c     V5.631(040917) to V5.632(041130)
c               (DGS) Initialize MFRH
c     V5.6(031017) to V5.631(040917)
c               (CEC) Initialize MM5 data- datum WGS-84
c                     include MM4hdo and coordlib.for
c     V5.5(030627) to V5.6(031017)
c               (DGS) Initialize MSRC variables
c     V5.4(030402) to V5.5(030627)
c               (DGS) Add station IDs for Method 7 weather stations
c               (DGS) Initialize base time zone to -999. (/HEAD/,/VISB/)
c     V5.2(991104d) to V5.4(030402)
c               (DGS) Add /CRTL/LDOC for printing doc records
c     V5.2(991104b) to V5.2(991104d)
c               (DGS) Set MVISBK=2 as default
c     V5.2(991104a) to V5.2(991104b)
c               (DGS) Add LDRING for reporting results by ring
c     V5.2(991104) to V5.2(991104a)
c               (DGS) Add Elemental Carbon as modeled species (visib.)
c     V5.1(990920) to V5.2(991104)
c               (DGS) Implement template for multiple time-series files
c     V5.1(990709) to V5.1(990920)
c               (DGS) Initialize time-series file name in /FILNAM/
c               (DGS) Initialize LTIME in /CTRL/
c               (DGS) Revise output logicals and initialize
c               (DGS) Initialize RHFAC(12)=0. and set MVISBK=6 as default
c     V5.0(990228a)to V5.1(990709)
c               (DGS) Initialize ISSEC in /CTRL/
c     V5.0(980821) to V5.0(990228a)
c               (DGS) Initialize NGRECP, NDAY, NCOUNT in /CTRL/
c               (DGS) Initialize LXDAY,LXDAY1,LXDAY3,LXDAY24,LXDAYN
c                     in /CTRL/
c
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      parameter(mxdrecm1=mxdrec-1)
      INCLUDE 'calmpro.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'filnam.pst'
      INCLUDE 'head.pst'
      INCLUDE 'mm4hdo.pst'
      INCLUDE 'no2nox.pst'
      INCLUDE 'source.pst'
      INCLUDE 'visb.pst'
c
c  Section for members of common /calmpro/:
      data fcalm/0.25/,met1fmt/1/,ncalmplm/0/,nmissplm/0/
c
c  Section for members of common /ctrl/:
      data issec/0/,ismin/0/
      data ieyr/0/,iemo/0/,iedy/0/,jeday/0/,iehr/0/,iemin/0/,iesec/0/
      data LSCALE/.true./,iecho/366*0/
      data LBACK/.false./
      data a/0./,b/0./
      data ilayer/1/, iprtu/1/
      data LG/.false./,LD/.false./,LCT/.FALSE./,LDRING/.FALSE./
      data LDEBUG/.false./, LVEXTHR/.false./
      data L1HR/.true./,L3HR/.true./,L24HR/.true./,LRUNL/.true./
      data LT50/.true./,LTOPN/.false./,LEXCD/.false./,
     &     LECHO/.false./,LTIME/.false./
      data LPLT/.false./,LGRD/.false./
      data navg/0/,ntop/4/,itop/1,2,3,4/
      data navgh/0/,navgm/0/,navgs/0/
      data thresh1/-1.0/,thresh3/-1.0/,thresh24/-1.0/,threshN/-1.0/
      data ncount/1/, nday/0/
      data LXDAY/.false./,LXDAY1/.false./,LXDAY3/.false./,
     &     LXDAY24/.false./,LXDAYN/.false./
      data nrep/1/
      data ndrecp/-1,mxdrecm1*0/
      data IBGRID/-1/,IEGRID/-1/,JBGRID/-1/,JEGRID/-1/
      data LGEXCLUDE/.false./
      data ngonoff/0/,ngrecp/mxgrec*1/
      data msrc/0/, msampler/0/
      data LDOC/.false./
      data LPEAK/.false./
      data BTZONE/-999./
      data MDVIS/0/
      data NRNKV24/22/
      data MCALMPRO/0/
      data NO2CALC/1/
      data MVISCHECK/1/
      data AREANAME/'User'/
c
c  Section for members of common /head/:
      data XBTZ/-999./
c
c  Section for members of common /no2nox/:
      data rno2nox/1.0/
c --- Initialize values to -1.0 for QA tests
      data binfno2/14*-1.0/
      data bincnox/14*-1.0/
      data fno2nox/-1.0/
c
c  Section for members of common /visb/:
      data LVSO4/.true./,LVNO3/.true./,LVPMC/.true./,LVPMF/.true./
      data LVOC/.true./,LVEC/.true./,LVBK/.true./
      data LAVER/.false./
      data MVISBK/8/,RHMAX/98./,BEXTRAY/10./
      data MFRH/4/
      data RHFAC/12*0.0/, M8_MODE/5/
      data RHFSML/12*0.0/, RHFLRG/12*0.0/, RHFSEA/12*0.0/
      data EESO4/3./,EENO3/3./,EEPMC/0.6/,EEPMF/1./,EEPMCBK/0.6/
      data EEOC/4./,EEEC/10./,EESOIL/1./
      data EESO4S/2.2/,EENO3S/2.4/,EESO4L/4.8/,EENO3L/5.1/
      data EEOCS/2.8/,EEOCL/6.1/,EESALT/1.7/
c --- Extinction efficiency for NO2 is 0.33 Mm-1/ppb
c --- Convert to Mm-1/ug/m3 by factor 0.523 ppb/ug/m3
c --- at 20C, 1atm
      data EENO2/0.1755/
      data SPECPMC/'PMC         '/,SPECPMF/'PMF         '/
      data nwsta/0/, idwsta/mxwsta*0/, tzone/mxwsta*0.0/
      data BTZONE2/-999./
c --- Weight factors for ammonia salts defined as
c --- formula weight of salt / formula weight of anion
      data xNO3/1.29/,xSO4/1.375/
c
c frr (09/01) non hourly prognostic data
c --- MM4HDO common block
      data isteppg /1/
      data datum3d/'NWS-84  '/
c
c --- FILNAM, FILLOG common blocks
      data pstinp/'calpost.inp'/,pstlst/'calpost.lst'/,
     1 moddat/'model.dat'/,visdat/'visb.dat'/,vsrdat/'vsrn.dat'/,
     2 backdat/'back.dat'/,sampdat/'samp.dat'/,dvisdat/'delvis.dat'/,
     3 met1dat/'surface.dat'/
      data npath/0/,ntspath/0/
      data lcfiles/.true./
c
c  Section for members of common /source/:
      data lds/.FALSE./, lgs/.FALSE./
      data isrec/0/, jsrec/0/
c
      end

c----------------------------------------------------------------------
c --- BRING IN CALPUFF SYSTEM UTILITY SUBROUTINES
      include 'calutils.for'
      include 'coordlib.for'
c----------------------------------------------------------------------

c-----------------------------------------------------------------------
      subroutine caps
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 890515              CAPS
c ---           J. Scire, SRC
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Converts lower-case characters within species name to
c               upper-case.
c
c  ARGUMENTS:   none
c
c  CALLING ROUTINES:    GETCTRL
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
c
      character*1 cchar,clc(29),cuc(29)
c
      data clc/'i','n','x','a','e','o','u','b','c','d','f','g','h',
     1 'j','k','l','m','p','q','r','s','t','v','w','y','z','-','.',
     2 '*'/
      data cuc/'I','N','X','A','E','O','U','B','C','D','F','G','H',
     1 'J','K','L','M','P','Q','R','S','T','V','W','Y','Z','-','.',
     2 '*'/
c
      do 100 i=1,12
      cchar=aspec(i:i)
c
      do 50 j=1,29
      if(cchar.eq.clc(j))then
         aspec(i:i)=cuc(j)
         go to 52
      endif
50    continue
52    continue
100   continue
c
      return
      end
c-----------------------------------------------------------------------
      subroutine gethead(io)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 070117           GETHEAD
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Reads header information from concentration file
c               and passes information to main program through
c               common /HEAD/.  Source names are passed into /SOURCE/.
c
c  UPDATES:
c     V6.133(060919) to V6.143(070117)
c               (DGS) Fix bug in dataset 2.2 units processing -- wrong
c                     variable in conditional check causes concentration
c                     units to be used for flux labels when datasets
c                     prior to 2.2 are processed.
c     V6.13(060324) to V6.133(060919)
c               (DGS) Remove explicit unit number in1 in several reads,
c                     and replace with io, passed as an argument
c     V6.12(060309) to V6.13(060324)
c               (DGS) Add dataset version 2.2 format
c     V5.637(050321) to V6.12(060309)
c               (CEC) check size of CALMET domain (ielmet, jelmet)against
c                     maximum limits for gridded receptors put in PARAMS.PST
c     V5.63(040623) to V5.637(050321)
c               (DGS) Fix typo in reading line source type 2 names into
c                     array -- 
c                     n2=21+nln2 should be n2=n2+nln2
c                     (CALPUFF dataset version 2.1)
c     V5.61(040123) to V5.63(040623)
c               (DGS) Fix bug in reading source names into array
c                     (CALPUFF dataset version 2.1)
c     V5.6(031017) to V5.61(040123)
c               (DGS) Fix bug that caused error in reading 'old'
c                     files (iptime replaces irhtime)
c     V5.5(030627) to V5.6(031017)
c               (DGS) Add dataset version 2.1 format
c     V5.4(030402) to V5.5(030627)
c               (DGS) Add structure for  dataset version > 2.0
c     V5.1(990709) to V5.4(030402)
c               (DGS) add list-file unit to INCR, YR4
c               (FRR) i2drh flag in all header records
c               (DGS) Dataset version 2.0 format
c     V5.0(980918) to V5.1(990709)
c               (DGS) begin-time end-time format resolved to seconds
c                     and enforce YYYY year format
c     V5.0(980515) to V5.0(980918)
c               (DGS) NSDUM replaces NSSTA
c     V5.0(960422) to V5.0(980515)
c               (DGS) Expand error reports
c
c  ARGUMENTS:
c     PASSED:   io      Unit number for accessing input file         [i]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   INCR, YR4, GETDOC, UTCBASR
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c -- Note: all declaration statements for data read in by GETHEAD
c          are contained in the included commons

      character*48 msg(8),msg0
      logical lfatal, lfatal2
      logical lspunit

      data msg0  /'  **********************************************'/
      data msg(1)/'  *      FATAL Problem Found in GETHEAD!       *'/
      data msg(2)/'  * ------------------------------------------ *'/
      data msg(3)/'  *  Size of Array(s) in MODEL.DAT too Large   *'/
      data msg(4)/'  *  Re-size Array Parameter(s) in PARAMS.PST  *'/
      data msg(5)/'  *         and Recompile CALPOST              *'/
      data msg(6)/'  * ------------------------------------------ *'/
      data msg(7)/'  * Parameter       PARAMS.PST       MODEL.DAT *'/
      data msg(8)/'  * ---------       ----------       --------- *'/

      lfatal=.FALSE.
      lfatal2=.FALSE.
      lspunit=.FALSE.

c --- Process first section of CALPUFF output file (including comment
c --- records), and determine dataset version
      call GETDOC(io,idtype,cpver)
c
c  Read control variables (record NCOM+3)
      if(idtype.EQ.0) then
         i2drh=0
         read(io) amodel,aver,alev,msyr,mjsday,mshr,mnhrs,mavg,
     *         ielmet,jelmet,delx,dely,nz,xorigkm,yorigkm,nsdum,
     *         iastar,iastop,jastar,jastop,
     *         isastr,jsastr,isastp,jsastp,
     *         meshdn,npts,nareas,nlines,nvols,
     *         ndrec,nctrec,LSGRID,nszout,lcomprs
         iptime=1
         isrcinfo=0
         isrcindv=0
         nsrc=0
      else
c ---    Dataset version 2.0 and later format
         if(cpver.EQ.'2.0             ') then
           read(io) amodel,aver,alev,msyr,mjsday,mshr,mnhrs,mavg,
     *         ielmet,jelmet,delx,dely,nz,xorigkm,yorigkm,nsdum,
     *         iastar,iastop,jastar,jastop,
     *         isastr,jsastr,isastp,jsastp,
     *         meshdn,npts,nareas,nlines,nvols,
     *         ndrec,nctrec,LSGRID,nszout,lcomprs,i2drh,
     *         iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
     *         pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
           iptime=1
           isrcinfo=0
           isrcindv=0
           nsrc=0

         elseif(cpver.EQ.'2.1             ') then
           read(io) amodel,aver,alev,msyr,mjsday,mshr,mssec,xbtz,
     *         mnrun,mavgpd,nsecdt,
     *         ielmet,jelmet,delx,dely,nz,xorigkm,yorigkm,nsdum,
     *         iastar,iastop,jastar,jastop,
     *         isastr,jsastr,isastp,jsastp,
     *         meshdn,npt1,npt2,nar1,nar2,nln1,nln2,nvl1,nvl2,msource,
     *         ndrec,nctrec,LSGRID,nszout,lcomprs,i2drh,
     *         iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
     *         pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
           iptime=2
           isrcinfo=1
           isrcindv=msource
           npts=npt1+npt2
           nareas=nar1+nar2
           nlines=nln1+nln2
           nvols=nvl1+nvl2
           nsrc=npts+nareas+nlines+nvols

         elseif(cpver.EQ.'2.2             ') then
           read(io) amodel,aver,alev,msyr,mjsday,mshr,mssec,abtz,
     *         mnrun,mavgpd,nsecdt,
     *         ielmet,jelmet,delx,dely,nz,xorigkm,yorigkm,nsdum,
     *         iastar,iastop,jastar,jastop,
     *         isastr,jsastr,isastp,jsastp,
     *         meshdn,npt1,npt2,nar1,nar2,nln1,nln2,nvl1,nvl2,msource,
     *         ndrec,nctrec,LSGRID,nszout,lcomprs,i2drh,
     *         iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
     *         pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
           iptime=2
           isrcinfo=1
           isrcindv=msource
           npts=npt1+npt2
           nareas=nar1+nar2
           nlines=nln1+nln2
           nvols=nvl1+nvl2
           nsrc=npts+nareas+nlines+nvols
c ---      Convert from UTC string to real base time zone
           call UTCBASR(abtz,xbtz)
c ---      Specie-level units are in header
           lspunit=.TRUE.
         else
           write(io1,*)'ERROR:  Unknown DATASET Version: ',cpver
           stop 'Halted in GETHEAD'
         endif
      endif

c --- Check year format
      call YR4(io1,msyr,ierry)
      if(ierry.NE.0) stop 'Halted in GETHDRH'

      if(iptime.EQ.1) then
c ---    Move starting time back 1 hour to identify the start of the
c ---    first hour
         call INCR(io1,msyr,mjsday,mshr,-1)
c ---    Set period step to 1 hour
         nsecdt=3600
         mssec=0
c ---    Rename averaging period variables
         mnrun=mnhrs
         mavgpd=mavg
      endif

c  Use nsdum to satisfy compiler checks
      if(nsdum .EQ. nssta) nsdum=nssta
c
c  Check size of array dimensions (compute gridded rec. array dims.)
c  with limits contained in PARAMS.PST file
      ngx=(isastp-isastr)*meshdn+1
      ngy=(jsastp-jsastr)*meshdn+1
      ngrec=ngx*ngy
      if(ngx .GT. mxgx) lfatal=.TRUE.
      if(ngy .GT. mxgy) lfatal=.TRUE.
      if(ndrec .GT. mxdrec) lfatal=.TRUE.
      if(nctrec .GT. mxctrec) lfatal=.TRUE.
      if(nszout .GT. mxsplv) lfatal=.TRUE.
      if(nsrc .GT. mxsrc) lfatal=.TRUE.
c
c --- CEC (060309) - check size of CALMET domain compare to maximum
c                    limits for gridded receptors put in PARAMS.PST
      IF(ielmet.gt.mxgx) lfatal2=.TRUE.
      IF(jelmet.gt.mxgy) lfatal2=.TRUE.

      if(LFATAL) then
c ---    Report problem information and quit
c ---    Screen (io6) and List File (io1):
c ---    Set for screen first
         iu=io6
         do k=1,2
           write(iu,*)
           write(iu,100) msg0
           do i=1,8
              write(iu,100) msg(i)
           enddo
           write(iu,101)'  *     MXGX',mxgx,ngx
           write(iu,101)'  *     MXGY',mxgy,ngy
           write(iu,101)'  *   MXDREC',mxdrec,ndrec
           write(iu,101)'  *  MXCTREC',mxctrec,nctrec
           write(iu,101)'  *   MXSPLV',mxsplv,nszout
           write(iu,101)'  *    MXSRC',mxsrc,nsrc
           write(iu,100) msg0
c ---      Set for list file unit
           iu=io1
         enddo

         stop
      endif
c
      if(LFATAL2) then
c ---    Report problem information and quit
c ---    Screen (io6) and List File (io1):
c ---    Set for screen first
         iu=io6
         do k=1,2
           write(iu,*)
           write(iu,100) msg0
           do i=1,8
              write(iu,100) msg(i)
           enddo
           write(iu,101)'  *     MXGX ',mxgx,ielmet
           write(iu,101)'  *     MXGY ',mxgy,jelmet
           WRITE(iu,*)'  *  Maximum of gridded receptor need to be    *'
           WRITE(iu,*)'  *  at least same size as CALMET grid         *'
           write(iu,100) msg0
c ---      Set for list file unit
           iu=io1
         enddo

         stop
      endif
c
c  Read title of run (record NCOM+4)
      read(io) atitle
c
c  Read specie/level list (record NCOM+5)
      read(io) (asplst(i),i=1,nszout)

c  Read units for each specie/level in list (record NCOM+5a)
c  (introduced in Dataset 2.2)
      if(LSPUNIT) then
         read(io) (aspunit(i),i=1,nszout)
      else
         do i=1,nszout
            if(asplst(i)(15:15).EQ.'F') then
               aspunit(i)='g/m2/s          '
            else
               aspunit(i)='g/m3            '
            endif
         enddo
      endif

c  Read coordinates of non-gridded receptors (record NCOM+6)
      if(ndrec .NE. 0) read(io) (xrec(i),i=1,ndrec),
     *                          (yrec(i),i=1,ndrec),
     *                          (zrec(i),i=1,ndrec)
c
c  Read coordinates of complex terrain receptors (record NCOM+7)
      if(nctrec .NE. 0) read(io) (xctr(i),i=1,nctrec),
     *                           (yctr(i),i=1,nctrec),
     *                           (zctr(i),i=1,nctrec),
     *                           (ihill(i),i=1,nctrec)
c
c  Generate coordinates of gridded receptors if any are used
      if(LSGRID .AND. ngrec .NE. 0) then
c  Compute coord. for lower-left point in sampling grid, and spacing
         gx0=xorigkm+delx*(isastr-0.5)
         gy0=yorigkm+dely*(jsastr-0.5)
         delgx=delx/meshdn
         delgy=dely/meshdn
c  Fill out arrays
         do i=1,ngx
            do j=1,ngy
               xgrd(i,j)=gx0+(i-1)*delgx
               ygrd(i,j)=gy0+(j-1)*delgy
            enddo
         enddo
      endif

c --- Dataset version 2.1 introduced source names
c --- HEADER RECORDs #NCOM+8 to 15 -- Source names
      if(isrcinfo.EQ.1) then
c ---    Place name for total from all sources in array element 0
         csource(0)='TOTAL           '
         lfatal=.FALSE.
         n2=0
         if(npt1.GT.0 .AND. .NOT.LFATAL) then
            n1=n2+1
            n2=n2+npt1
            read(io) itype,(csource(n),n=n1,n2)
            if(itype.NE.1) lfatal=.TRUE.
         endif
         if(npt2.GT.0 .AND. .NOT.LFATAL) then
            n1=n2+1
            n2=n2+npt2
            read(io) itype,(csource(n),n=n1,n2)
            if(itype.NE.2) lfatal=.TRUE.
         endif
         if(nar1.GT.0 .AND. .NOT.LFATAL) then
            n1=n2+1
            n2=n2+nar1
            read(io) itype,(csource(n),n=n1,n2)
            if(itype.NE.3) lfatal=.TRUE.
         endif
         if(nar2.GT.0 .AND. .NOT.LFATAL) then
            n1=n2+1
            n2=n2+nar2
            read(io) itype,(csource(n),n=n1,n2)
            if(itype.NE.4) lfatal=.TRUE.
         endif
         if(nln1.GT.0 .AND. .NOT.LFATAL) then
            n1=n2+1
            n2=n2+nln1
            read(io) itype,(csource(n),n=n1,n2)
            if(itype.NE.5) lfatal=.TRUE.
         endif
         if(nln2.GT.0 .AND. .NOT.LFATAL) then
            n1=n2+1
            n2=n2+nln2
            read(io) itype,(csource(n),n=n1,n2)
            if(itype.NE.6) lfatal=.TRUE.
         endif
         if(nvl1.GT.0 .AND. .NOT.LFATAL) then
            n1=n2+1
            n2=n2+nvl1
            read(io) itype,(csource(n),n=n1,n2)
            if(itype.NE.7) lfatal=.TRUE.
         endif
         if(nvl2.GT.0 .AND. .NOT.LFATAL) then
            n1=n2+1
            n2=n2+nvl2
            read(io) itype,(csource(n),n=n1,n2)
            if(itype.NE.8) lfatal=.TRUE.
         endif
         if(nsrc.NE.n2) lfatal=.TRUE.
         if(LFATAL) then
c ---       Report problem information and quit
c ---       Screen (io6) and List File (io1):
c ---       Set for screen first
            iu=io6
            do k=1,2
              write(iu,*)
              write(iu,100) msg0
              do i=1,2
                 write(iu,100) msg(i)
              enddo
              write(iu,100)'  *  Unexpected source type found, or     *'
              write(iu,100)'  *  Unexpected source number found.      *'
              write(iu,100)'  *  Number of sources expected:          *'
              write(iu,101)'  *     NPTS',npt1,npt2
              write(iu,101)'  *   NAREAS',nar1,nar2
              write(iu,101)'  *   NLINES',nln1,nln2
              write(iu,101)'  *    NVOLS',nvl1,nvl2
              write(iu,100)'  *  Source Names Read:                   *'
              do i=1,nsrc
                 write(iu,*)'     ',csource(i)
              enddo
              write(iu,100) msg0

c ---         Set for list file unit
              iu=io1
            enddo
            stop
         endif
      endif

100   format(1x,a48)
101   format(1x,a12,2(8x,i8),'   *')

      return
      end
c-----------------------------------------------------------------------
      subroutine gethdrh(io)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 060324           GETHDRH
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Reads header information from relative humidity file
c               and passes information to main program through
c               common /head/.
c
c  UPDATES:
c     V6.12(060309) to V6.13(060324)
c               (DGS) Add dataset version 2.2 format
c     V5.6(031017) to V6.12(060309)
c               (CEC) check size of CALMET domain (ielmet, jelmet)against
c                     maximum limits for gridded receptors put in PARAMS.PST
c     V5.5(030627) to V5.6(031017)
c               (DGS) Add dataset version 2.1 format
c     V5.4(030402) to V5.5(030627)
c               (DGS) Add structure for  dataset version > 2.0
c     V5.1(990709) to V5.4(030402)
c               (DGS) add list-file unit to INCR, YR4
c               (FRR) flag for 2D vs 1D RH array (noobs version)i2drh
c                     if i2drh=1 no info on nearest stations
c                     (records #5-6-7)
c               (DGS) Dataset version 2.0 format
c 
c     V5.0(980918) to V5.1(990709)
c               (DGS) begin-time end-time format resolved to seconds
c                     and enforce YYYY year format
c     V5.0(950106) to V5.0(980515)
c               (DGS) Expand error reports
c
c  ARGUMENTS:
c     PASSED:   io      Unit number for accessing input file         [i]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   INCR, YR4, GETDOC
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
c -- Note: all declaration statements for data read in by GETHEAD
c          are contained in this common
c
      character*15 cname,asprh
      character*16 arhunit

      character*48 msg(8),msg0
      logical lfatal,lfatal2
      logical lrhunit

      data msg0  /'  **********************************************'/
      data msg(1)/'  *      FATAL Problem Found in GETHDRH!       *'/
      data msg(2)/'  * ------------------------------------------ *'/
      data msg(3)/'  *   Size of Array(s) in VISB.DAT too Large   *'/
      data msg(4)/'  *  Re-size Array Parameter(s) in PARAMS.PST  *'/
      data msg(5)/'  *         and Recompile CALPOST              *'/
      data msg(6)/'  * ------------------------------------------ *'/
      data msg(7)/'  * Parameter       PARAMS.PST        VISB.DAT *'/
      data msg(8)/'  * ---------       ----------        -------- *'/

      lfatal=.FALSE.
      lfatal2=.FALSE.
      lrhunit=.FALSE.

c --- Process first section of CALPUFF output file (including comment
c --- records), and determine dataset version
      call GETDOC(io,idtype,crhver)
c
c  Read control variables
      if(idtype.EQ.0) then
         i2drh=0
         read(io) amodel,aver,alev,msyr,mjsday,mshr,mnhrs,mavg,
     *         ielmet,jelmet,delx,dely,nz,xorigkm,yorigkm,nssta,
     *         iastar,iastop,jastar,jastop,
     *         isastr,jsastr,isastp,jsastp,
     *         meshdn,npts,nareas,nlines,nvols,
     *         ndrec,nctrec,LSGRID,nszoutrh,lcomprs
         irhtime=1
      else
c ---    Dataset version 2.0 and later format
         if(crhver.EQ.'2.0             ') then
           read(io) amodel,aver,alev,msyr,mjsday,mshr,mnhrs,mavg,
     *         ielmet,jelmet,delx,dely,nz,xorigkm,yorigkm,nssta,
     *         iastar,iastop,jastar,jastop,
     *         isastr,jsastr,isastp,jsastp,
     *         meshdn,npts,nareas,nlines,nvols,
     *         ndrec,nctrec,LSGRID,nszoutrh,lcomprs,i2drh,
     *         iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
     *         pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
           irhtime=1

         elseif(crhver.EQ.'2.1             ') then
           read(io) amodel,aver,alev,msyr,mjsday,mshr,mssec,xbtz,
     *         mnrun,mavgpd,nsecdt,
     *         ielmet,jelmet,delx,dely,nz,xorigkm,yorigkm,nssta,
     *         iastar,iastop,jastar,jastop,
     *         isastr,jsastr,isastp,jsastp,
     *         meshdn,npt1,npt2,nar1,nar2,nln1,nln2,nvl1,nvl2,msource,
     *         ndrec,nctrec,LSGRID,nszoutrh,lcomprs,i2drh,
     *         iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
     *         pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
           irhtime=2

         elseif(crhver.EQ.'2.2             ') then
           read(io) amodel,aver,alev,msyr,mjsday,mshr,mssec,abtz,
     *         mnrun,mavgpd,nsecdt,
     *         ielmet,jelmet,delx,dely,nz,xorigkm,yorigkm,nssta,
     *         iastar,iastop,jastar,jastop,
     *         isastr,jsastr,isastp,jsastp,
     *         meshdn,npt1,npt2,nar1,nar2,nln1,nln2,nvl1,nvl2,msource,
     *         ndrec,nctrec,LSGRID,nszoutrh,lcomprs,i2drh,
     *         iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
     *         pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
           irhtime=2
c ---      Units provided for variable
           lrhunit=.TRUE.

         else
           write(io1,*)'ERROR:  Unknown DATASET Version: ',cpver
           stop 'Halted in GETHDRH'
         endif
      endif

c --- Check year format
      call YR4(io1,msyr,ierry)
      if(ierry.NE.0) stop 'Halted in GETHDRH'

      if(irhtime.EQ.1) then
c ---    Move starting time back 1 hour to identify the start of the
c ---    first hour
         call INCR(io1,msyr,mjsday,mshr,-1)
c ---    Set period step to 1 hour
         nsecdt=3600
         mssec=0
c ---    Rename averaging period variables
         mnrun=mnhrs
         mavgpd=mavg
      endif
c
c  Check size of array dimensions (compute gridded rec. array dims.)
c  with limits contained in PARAMS.PST file
      ngx=(isastp-isastr)*meshdn+1
      ngy=(jsastp-jsastr)*meshdn+1
      ngrec=ngx*ngy
      if(ngx .GT. mxgx) lfatal=.TRUE.
      if(ngy .GT. mxgy) lfatal=.TRUE.
      if(ndrec .GT. mxdrec) lfatal=.TRUE.
      if(nctrec .GT. mxctrec) lfatal=.TRUE.
      if(nszoutrh .GT. mxsplv) lfatal=.TRUE.
c
c --- CEC (060309) - check size of CALMET domain compare to maximum
c                    limits for gridded receptors put in PARAMS.PST
      IF(ielmet.gt.mxgx) lfatal2=.TRUE.
      IF(jelmet.gt.mxgy) lfatal2=.TRUE.
c
      if(LFATAL) then
c ---    Report problem information and quit
c ---    Screen (io6) and List File (io1):
c ---    Set for screen first
         iu=io6
         do k=1,2
           write(iu,*)
           write(iu,100) msg0
           do i=1,8
              write(iu,100) msg(i)
           enddo
           write(iu,101)'  *     MXGX',mxgx,ngx
           write(iu,101)'  *     MXGY',mxgy,ngy
           write(iu,101)'  *   MXDREC',mxdrec,ndrec
           write(iu,101)'  *  MXCTREC',mxctrec,nctrec
           write(iu,101)'  *   MXSPLV',mxsplv,nszoutrh
           write(iu,100) msg0
c ---      Set for list file unit
           iu=io1
         enddo

         stop
      endif
c
      if(LFATAL2) then
c ---    Report problem information and quit
c ---    Screen (io6) and List File (io1):
c ---    Set for screen first
         iu=io6
         do k=1,2
           write(iu,*)
           write(iu,100) msg0
           do i=1,8
              write(iu,100) msg(i)
           enddo
           write(iu,101)'  *     MXGX ',mxgx,ielmet
           write(iu,101)'  *     MXGY ',mxgy,jelmet
           WRITE(iu,*)'  *  Maximum of gridded receptor need to be    *'
           WRITE(iu,*)'  *  at least same size as CALMET grid         *'
           write(iu,100) msg0
c ---      Set for list file unit
           iu=io1
         enddo

         stop
      endif
c
c
c  Read title of run
      read(io) atitle
c
c  Read specie/level list
      read(io) asprh

c  Read units for specie/level list (Dataset 2.2 on)
      if(LRHUNIT) then
         read(io) arhunit
      else
         arhunit='percent         '
      endif

c  Read coordinates of non-gridded receptors if any are used
      if(ndrec .NE. 0) read(io) (xrec(i),i=1,ndrec),
     *                          (yrec(i),i=1,ndrec),
     *                          (zrec(i),i=1,ndrec)
c
c  Read coordinates of complex terrain receptors if any are used
      if(nctrec .NE. 0) read(io) (xctr(i),i=1,nctrec),
     *                           (yctr(i),i=1,nctrec),
     *                           (zctr(i),i=1,nctrec),
     *                           (ihill(i),i=1,nctrec)
c
c frr (4/02) no record of nearest station if 2D array of RH
      if (i2drh.eq.0) then
c        Read the grid of nearest met station indices (NEARS)
         call rdint(io,cname,nears,mxgx,mxgy,ielmet,jelmet)
c
c        Read x,y-coordinate of met stations (UTM-km)
         read(io) cname,(xkmsta(i),i=1,nssta)
         read(io) cname,(ykmsta(i),i=1,nssta)
      endif

100   format(1x,a48)
101   format(1x,a12,2(8x,i8),'   *')

c  Check for blank specie-level
      if(asprh .EQ. '               ') then
         stop 'GETHDRH: blank species listed in VISB.DAT'
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine gethdvsr(io)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 981116          GETHDVSR
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Reads header record from VSRN.DAT file and sets
c               the type of data in the file (nephelometer or
c               transmissometer)
c
c  UPDATES:
c     V5.0(980918) to V5.0(981116)
c               (DGS) Check mvisbk with data type
c
c  ARGUMENTS:
c     PASSED:   io      Unit number for accessing input file         [i]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'visb.pst'

      logical lfatal
      character*48 msg(8),msg0,msgx
      character*26 alittle
      character*3 aneph
      data aneph/'INS'/

      data msg0  /'  **********************************************'/
      data msg(1)/'  *      FATAL Problem Found in GETHDVSR       *'/
      data msg(2)/'  * ------------------------------------------ *'/
      data msg(3)/'  *  Unexpected type of data found in VSR.DAT  *'/
      data msg(4)/'  * Transmissometer data goes with MVISBK = 4  *'/
      data msg(5)/'  *    Nephelometer data goes with MVISBK = 5  *'/
      data msg(6)/'  * ------------------------------------------ *'/
      data msg(7)/'  *      Nephelometer?    MVISBK               *'/
      data msg(8)/'  *      -------------   --------              *'/

c --- First line of VSRN.DAT file is composed of column headings
c --- The nephelometer data includes the variable INS, which is not
c --- found in the transmissometer data
      read(io,'(a26)') alittle

      lnephel=.FALSE.
      if(alittle(24:26).EQ.aneph) lnephel=.TRUE.

c --- Verify that file content is consistent with background option
      lfatal=.FALSE.
      if(LNEPHEL .AND. mvisbk.NE.5) then
         lfatal=.TRUE.
         msgx='  *          TRUE                              *'
         write(msgx(29:29),'(a1)') mvisbk
      elseif(.not.LNEPHEL .AND. mvisbk.NE.4) then
         lfatal=.TRUE.
         msgx='  *         FALSE                              *'
         write(msgx(29:29),'(a1)') mvisbk
      endif

      if(LFATAL) then
c ---    Report problem information and quit
c ---    Screen:
         write(io6,*)
         write(io6,'(1x,a48)') msg0
         do i=1,8
            write(io6,'(1x,a48)') msg(i)
         enddo
         write(io6,'(1x,a48)') msgx
         write(io6,'(1x,a48)') msg0
c ---    List File:
         write(io1,*)
         write(io1,'(1x,a48)') msg0
         do i=1,8
            write(io1,'(1x,a48)') msg(i)
         enddo
         write(io1,'(1x,a48)') msgx
         write(io1,'(1x,a48)') msg0
         stop
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine rdint(iounit,cname,ioutarr,mx,my,nx,ny)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 940430             RDINT
c                J. Scire, SRC
c
c --- PURPOSE:  Read a gridded field of integers to binary file
c               (one 15-character identifier and a 2-D data array)
c
c --- INPUTS:
c          IOUNIT - integer      - Fortran unit no. of output file
c           CNAME - character*15 - Data identifier/name
c  IOUTARI(nx,ny) - int. array   - Data array
c              NX - integer      - Number of grid points in the
c                                  X direction
c              NY - integer      - Number of grid points in the
c                                  Y direction
c
c --- OUTPUT:  none
c
c --- RDINT called by:
c --- RDINT calls:      none
c
c----------------------------------------------------------------------
c
c      !integer ioutarr(nx,ny)
      integer ioutarr(mx,my)
      character*15 cname
c
      read(iounit)cname,((ioutarr(i,j),i=1,nx),j=1,ny)
      do i=1,nx
        do j=1,ny
          if(ioutarr(i,j).eq.0)then
            write(*,'("nears(",i2,",",i2,") = 0")')i,j
            stop 'Error in relative humidity file visb.dat'
          endif
        enddo
      enddo
c
      return
      end
c-----------------------------------------------------------------------
      subroutine qainp(io,naper)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080709             QAINP
c ---           D. Strimaitis
c
c  PURPOSE:     Compares information provided from concentration
c               file and program control file to check for
c               inconsistencies.  All information is written to
c               output file.  Number of averaging periods to skip
c               (naper) to get to first period to be processed is
c               computed.
c
c  UPDATES:
c
c  V6.22(080709) to V6.221(080724)
c               (DGS) Add MVISCHECK, AREANAME
c               (DGS) Do not require MFRH=4 with MVISBK=8 unless
c                     M_MODE = 1, 2, or 3
c
c  V6.2(070712) to V6.22(080709)
c               (DGS) Add RHFSML,RHFLRG,RHFSEA
c               (DGS) Revise M8_MODE range to 1-5
c
c  V6.144(070130) to V6.2(070712)
c               (DGS) Add NO2/NOx ratios
c               (DGS) Always write internal control file variables to
c                     list file
c
c  V6.141(061120) to V6.144(070130)
c               (DGS) Add CALM processing option for plume-type models
c
c  V6.14(061107) to V6.141(061120)
c               (DGS) Add use of daily averaged modeled concentrations
c                     to Method 8 (M8_MODE=3)
c               (DGS) Restrict Method 8, M8_MODE=3 applications to 24-hr
c                     and run-length extinction output (L24HR and LRUNL)
c               (DGS) Prohibit use of scaling and background concs
c                     when doing visibility
c  V6.131(060410) to V6.14(061107)
c               (DGS) Add checks for NO2 visibility option
c               (DGS) Add checks for Method 8 visibility option
c  V6.13(060324) to V6.131(060410)
c               (KAM-HATCH) Change DELHR and AVGHR to real*8 so
c                     that test for fractional averaging periods from
c                     beginning of run is correct
c  V6.12(060309) to V6.13(060324)
c               (DGS) Add section to replace normal units scaling if
c                     non-standard units are specified in dataset 2.2
c                     header
c  V6.1(050915) to V6.12(060309)
c               (DGS) Add MDVIS (auxiliary visibility output file)
c  V5.65(050729) to V6.1(050915)
c               (DGS) Report sub-hour times in minutes and seconds
c                     instead of seconds(0000-3599)
c               (DGS) Use issec4(0000-3600) for QA time checks
c               (DGS) Process and report NAVGH,NAVGM,NAVGS
c               (DGS) Compute iper1,iper3,iper24 for the number of
c                     periods in the standard 1,3,24-hr averages
c               (DGS) QA possible inputs for BTZONE in groups 1 and 2
c  V5.632(041130) to V5.65(050729)
c               (DGS) Added MSAMPLER to allow more than 1 receptor in
c                     TRACEBACK mode if processing SAMPLERS
c  V5.62(040503) to V5.632(041130)
c               (DGS) Added MFRH (hygroscopic growth curve option)
c  V5.6(031017) to V5.62(040503)
c               (DGS) Move BTZONE/XBTZ assignment and QA outside of
c                     the Method 7 construct
c  V5.5(030627) to V5.6(031017)
c               (DGS) Add source contribution (MSRC) checks
c               (DGS) Identify receptor used in source TRACEBACK mode
c  V5.2(991104c) to V5.5(030627)
c               (DGS) Add MVISBK=7 checks
c  V5.2(991104a) to V5.2(991104c)
c               (DGS) Add RING option
c  V5.1(990920) to V5.2(991104a)
c               (DGS) Add Elemental Carbon as modeled species
c  V5.1(990709) to V5.1(990920)
c               (DGS) Add time-series output
c               (DGS) Revise output options checks
c               (DGS) Revise output options logicals
c               (DGS) Add check on RHFAC(12) for MVISBK=6
c
c  V5.0(990228a)to V5.1(990709)
c               (DGS) Allow general run-steps, with time resolved to
c                     seconds
c
c  V5.0(981025) to V5.0(990228a)
c               (DGS) Interpret "-1"s provided in control file for
c                     range of gridded receptors to process (IBGRID,
c                     IEGRID,JBGRID,JEGRID) whenever CALPUFF file
c                     contains gridded receptors (LSGRID=T)
c               (DGS) Interpret "-1"s provided in control file for
c                     discrete receptors to process (NDRECP) whenever
c                     CALPUFF file contains discrete receptors
c                     (NDREC>0)
c               (DGS) Set 'LXDAY' logicals for saving daily exceedances
c
c  V5.0(980821) to V5.0(981025)
c               (DGS) Check gridded receptor range
c
c  V5.0(980515) to V5.0(980821)
c               (DGS) Revise visibility variables
c               (DGS) Check discrete receptor range
c
c  V5.0(980430) to V5.0(980515)
c               (DGS) Convert PERCENTAGE of particles affected by
c                     relative humidity to FRACTION
c               (DGS) Convert RHMAX (%) to relative humidity FRACTION
c
c  V5.0(980304) to V5.0(980430)
c               (DGS) Add Gridded plot-file logicals
c
c  V5.0(980304) to V5.0(980430)
c               (DGS) Add LCTSG to flag presence of data at CTSG recs
c
c  V5.0(971015) to V5.0(980304)
c               (AMK) Added write statement for LBACK used for
c                     adding hourly background
c
c  V3.1 to V3.2(960716)
c               (DGS) Accept start-time and run-length from puff
c                     output file for use if METRUN=1
c
c
c  ARGUMENTS:
c     PASSED:   io      Unit number for accessing output file        [i]
c              naper    Number of averaging periods in data record   [i]
c                       to skip over to reach the first period to
c                       be processed
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   DELTT
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'calmpro.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'no2nox.pst'
      INCLUDE 'source.pst'
      INCLUDE 'visb.pst'
c
      real*8 delhr8,avghr8
      logical lok, ltzone
      character*12 sulfate,nitrate,orgcarb,lmncarb
      data sulfate/'SO4         '/,nitrate/'NO3         '/
      data orgcarb/'SOA         '/,lmncarb/'EC          '/
      character*12 no2gas,noxgas
      data no2gas/'NO2         '/,noxgas/'NOX         '/
      character*15 char15

      character*6 azone,azone2
c
c  Initialize  fatal error flag
      ifatal=0
c
c  Set scaling logical from values of A,B. (FALSE if both are zero)
      if(a .EQ. 0.0 .AND. b .EQ. 0.0) LSCALE=.FALSE.
c
c  Compute number of averaging periods in file
      mnper=mnrun/mavgpd

c  Reset run-time information found in the control file to that found
c  in the concentration/flux file when METRUN=1
      if(metrun.EQ.1) then
         isyr=msyr
         jsday=mjsday
         ishr=mshr
         call GRDAY(ioout,isyr,jsday,ismo,isdy)
         issec4=mssec
         ismin=issec4/60
         issec=issec4-60*ismin
         nper=mnper
      endif

c  Establish consistent ending date-time and number of output periods
c  now that modeled output period is known
      mavgsec=mavgpd*nsecdt
      if(nper.EQ.0) then
c ---    Convert end date-time to nper
         call DELTT(isyr,jsday,ishr,ieyr,jeday,iehr,idelhr)
         irlgsec=idelhr*3600+(issec4-iesec4)
         nper=irlgsec/mavgsec
      endif
      ieyr=isyr
      jeday=jsday
      iehr=ishr
      iesec4=issec4
      irlgsec=nper*mavgsec
      call INCRS(ioout,ieyr,jeday,iehr,iesec4,irlgsec)
      iemin=iesec4/60
      iesec=iesec4-iemin*60
      call GRDAY(ioout,ieyr,jeday,iemo,iedy)

c  Compute number of periods in the standard averaging times
      iper1=3600/mavgsec
      iper3=3*3600/mavgsec
      iper24=24*3600/mavgsec

c  Check for valid selection of averaging periods
      if((iper1*mavgsec).NE.3600 .AND. L1HR) then
         write(io,*)
         write(io,*) '****************    FATAL    *****************'
         write(io,*) '1-Hour averaging time is not a multiple'
         write(io,*) 'of the averages written by the model!'
         write(io,*) 'Model Output(s) = ',mavgsec
         write(io,*) 'L1HR cannot be True'
         ifatal=1
      endif
      if((iper3*mavgsec).NE.(3*3600) .AND. L3HR) then
         write(io,*)
         write(io,*) '****************    FATAL    *****************'
         write(io,*) '3-Hour averaging time is not a multiple'
         write(io,*) 'of the averages written by the model!'
         write(io,*) 'Model Output(s) = ',mavgsec
         write(io,*) 'L3HR cannot be True'
         ifatal=1
      endif
      if((iper24*mavgsec).NE.(24*3600) .AND. L24HR) then
         write(io,*)
         write(io,*) '****************    FATAL    *****************'
         write(io,*) '24-Hour averaging time is not a multiple'
         write(io,*) 'of the averages written by the model!'
         write(io,*) 'Model Output(s) = ',mavgsec
         write(io,*) 'L24HR cannot be True'
         ifatal=1
      endif

c  Compute the number of periods in the user-specified averaging time
      if(navg.GT.0) then
c ---    User provided number so compute the averaging time
         navgsec=navg*mavgsec
         navgh=navgsec/3600
         navgm=(navgsec-navgh*3600)/60
         navgs=navgsec-navgh*3600-navgm*60
      elseif(navgh.GT.0 .OR. navgm.GT.0 .OR. navgs.GT.0) then
c ---    User provided the averaging time
         navgsec=navgh*3600+navgm*60+navgs
         navg=navgsec/mavgsec
c ---    Check that N-averaging time is a multiple of the output
c        averaging time
         nchecksec=navg*mavgsec
         if(nchecksec.NE.navgsec) then
           write(io,*)
           write(io,*) '****************    FATAL    *****************'
           write(io,*) 'User-specified averaging time is not a multiple'
           write(io,*) 'of the averages written by model!'
           write(io,*) 'Averaging Time(hh,mm,ss) = ',navgh,navgm,navgs
           write(io,*) 'Model Output(s) = ',mavgsec
           ifatal=1
         endif
      endif
c
c  Determine length of averaging period found in model file
      if(MOD(nsecdt,3600).EQ.0) then
c ---    Hours
         mavg=mavgpd*(nsecdt/3600)
         avtime='  HOUR'
      elseif(MOD(nsecdt,60).EQ.0) then
c ---    Minutes
         mavg=mavgpd*(nsecdt/60)
         avtime='MINUTE'
      else
c ---    Seconds
         mavg=mavgpd*nsecdt
         avtime='SECOND'
      endif
c
c  Set averaging time for reporting daily exceedance counts
      if(LEXCD) then
       if(nday.GT.0 .AND. (mavg.NE.1 .OR. avtime.NE.'  HOUR')) then
         write(io,*)
         write(io,*) '****************    FATAL    *****************'
         write(io,*) 'Daily Exceedance Analysis Requires Hourly Data!'
         write(io,*) 'MAVG = ',mavg,' ',avtime
         ifatal=1
       elseif(nday.GT.0) then
         if(L1HR) then
            lxday1=.TRUE.
            lxday =.TRUE.
         elseif(L3HR) then
            lxday3=.TRUE.
            lxday =.TRUE.
         elseif(L24HR) then
            lxday24=.TRUE.
            lxday =.TRUE.
         endif
c ---    Test for NAVG
         if(navg.GT.0 .AND. navg.LT.24) then
            if(.not.LXDAY) then
               lxdayn=.TRUE.
               lxday =.TRUE.
            elseif(lxday3 .AND. navg.LT.3) then
               lxdayn=.TRUE.
               lxday3=.FALSE.
            elseif(lxday24 .AND. navg.LT.24) then
               lxdayn=.TRUE.
               lxday24=.FALSE.
            endif
         endif
       endif
      endif
c
c  Set logical for reading concentrations at CTSG receptors ---
c  (already set TRUE if concentration file is processed)
      if(nctrec.LE.0) lctsg=.FALSE.

c --- Revise units reported to output files if non-standard units
c --- are specified in the CALPUFF (dataset 2.2 or later) file header
      k=0
      do i=1,nszout
         if(asplst(i).EQ.asplv) k=i
      enddo
      if(k.GT.0) then
       if(aspunit(k).NE.'g/m2/s          ' .AND.
     &   aspunit(k).NE.'g/m3            ') then
         write(io,*)
         write(io,*)
         write(io,*)
         write(io,*)
         write(io,*) '****************   WARNING   *****************'
         write(io,*) 'Original units from file are retained for output'
         write(io,*) 'Species-Level : ',asplst(k)
         write(io,*) '        Units : ',aspunit(k)
         write(io,*)
         write(io,*)
         iprtu=1
         rscale=1.0
         units='('//aspunit(k)(1:11)//')'
       endif
      endif
c
c  Identify model output file used in CALPOST application
      write(io,*)
      write(io,*)
      write(io,*)
      write(io,*) 'IDENTIFICATION OF PROCESSED MODEL FILE ----------'
      write(io,*)
      write(io,*) amodel,aver,alev
      write(io,*)
      write(io,*) atitle(1)
      write(io,*) atitle(2)
      write(io,*) atitle(3)
      write(io,*)
      write(io,*) 'Averaging time for values reported from model:'
      write(io,*) '     ',mavg,' ',avtime
      write(io,*)
      write(io,*) 'Number of averaging periods in file from model:'
      write(io,*) '     ',mnper
      write(io,*)
      write(io,*) 'Chemical species names for each layer in model:'
      do i=1,nszout
         write(io,*) asplst(i)
      enddo
c
c  Print contents of control file for debugging purposes
c --- Always write this to list file for QA
c --- if(LDEBUG) then
         write(io,*)
         write(io,*)
         write(io,*)
         write(io,*) 'QA Information -- Internal Representation of Data'
         write(io,*)
         write(io,*) 'CONTENTS OF CONTROL FILE ------------------------'
         write(io,*) ' METRUN              =',metrun
         if(metrun.EQ.1) then
            write(io,*) '     (so times in model output file are used)'
         endif
         write(io,*) ' isyr,ismo,isdy      =',isyr,ismo,isdy
         write(io,*) ' ishr,ismin,issec    =',ishr,ismin,issec
         write(io,*) ' ieyr,iemo,iedy      =',ieyr,iemo,iedy
         write(io,*) ' iehr,iemin,iesec    =',iehr,iemin,iesec
         write(io,*) ' nper                =',nper
         write(io,*) ' aspec,ilayer        =',aspec,ilayer
         write(io,*) ' asplv               =',asplv
         write(io,*) ' NO2CALC             =',no2calc
         if(no2calc.EQ.1) then
            write(io,*) '    RNO2NOX          =',rno2nox
         elseif(no2calc.EQ.2) then
            do k=1,14
               write(io,*) '    CNOX,TNO2NOX     =',bincnox(k),
     &                                              binfno2(k)
            enddo
         endif
         write(io,*) ' MSOURCE             =',msrc
         write(io,*) ' MCALMPRO            =',mcalmpro
         write(io,*) ' MET1FMT             =',met1fmt
         write(io,*) ' LG,LD,LCT,LDRING    =',LG,LD,LCT,LDRING
         write(io,*) ' IBGRID,IEGRID       =',IBGRID,IEGRID
         write(io,*) ' JBGRID,JEGRID       =',JBGRID,JEGRID
         write(io,*) ' NDRECP              ='
         write(io,'(40i2)') (ndrecp(i),i=1,ndrec)
         write(io,*) ' a,b,LSCALE          =',a,b,LSCALE
         write(io,*) ' LBACK               =',LBACK
         write(io,*) ' MVISBK              =',MVISBK
         write(io,*) ' MVISCHECK           =',MVISCHECK
         write(io,*) ' AREANAME            =',AREANAME
         write(io,*) ' MFRH                =',MFRH
         write(io,*) ' RHMAX,BEXTRAY       =',RHMAX,BEXTRAY
         write(io,*) ' RHFRAC,BEXTBK       =',RHFRAC,BEXTBK
         write(io,*) ' LVSO4,LVNO3,LVNO2   =',LVSO4,LVNO3,LVNO2
         write(io,*) ' LVOC,LVEC           =',LVOC,LVEC
         write(io,*) ' LVPMC,LVPMF,LVBK    =',LVPMC,LVPMF,LVBK
         write(io,*) ' SPECPMC,SPECPMF     =',SPECPMC,SPECPMF
         write(io,*) ' EEPMC,EEPMF,EEPMCBK =',EEPMC,EEPMF,EEPMCBK
         write(io,*) ' EESO4,EENO3,EEOC    =',EESO4,EENO3,EEOC
         write(io,*) ' EESO4S,EENO3S,EEOCS =',EESO4S,EENO3S,EEOCS
         write(io,*) ' EESO4L,EENO3L,EEOCL =',EESO4L,EENO3L,EEOCL
         write(io,*) ' EESOIL,EEEC,EENO2   =',EESOIL,EEEC,EENO2
         write(io,*) ' navg,ntop           =',navg,ntop
         write(io,*) ' navgh,navgm,navgs   =',navgh,navgm,navgs
         write(io,*) ' itop =',(itop(nn),nn=1,ntop)
         write(io,*) ' L[1,3,24]HR         =',L1HR,L3HR,L24HR
         write(io,*) ' LNAVG, LRUNL        =',LNAVG,LRUNL 
         write(io,*) ' LT50, LTOPN, LEXCD  =',LT50,LTOPN,LEXCD
         write(io,*) ' LECHO, LTIME, LPEAK =',LECHO,LTIME,LPEAK
         write(io,*) ' THRESH1             =',THRESH1
         write(io,*) ' THRESH3             =',THRESH3
         write(io,*) ' THRESH24            =',THRESH24
         write(io,*) ' THRESHN             =',THRESHN
         write(io,*) ' LPLT, LGRD          =',LPLT,LGRD
         write(io,*) ' MDVIS               =',MDVIS
         write(io,*) ' LDEBUG              =',LDEBUG
         write(io,*) ' LCTSG               =',LCTSG
         if(LECHO) then
            write(io,*) ' iecho(366) ='
            do i=1,12
               k=30*(i-1)+1
               l=30*i
               write(io,101) (iecho(j),j=k,l)
            enddo
            write(io,102) (iecho(j),j=361,366)
         endif
c
c  Print contents of /HEAD/ & /SOURCE/ for debugging purposes
         write(io,*)
         write(io,*) 'CONTENTS OF HEADER OF MODEL OUTPUT FILE ---------'
         write(io,*) ' model :   ',amodel,aver,alev
         write(io,*) ' msyr,mjsday      =',msyr,mjsday
         write(io,*) ' mshr,mssec       =',mshr,mssec
         write(io,*) ' nsecdt (period)  =',nsecdt
         write(io,*) ' xbtz             =',xbtz
         write(io,*) ' mnper,nszout,mavgpd =',mnper,nszout,mavgpd
         write(io,*) ' xorigkm,yorigkm,nssta = ',xorigkm,yorigkm,nssta
         write(io,*) ' ielmet,jelmet =',ielmet,jelmet
         write(io,*) ' delx,dely,nz =',delx,dely,nz
         write(io,*) ' iastar,iastop,jastar,jastop =',iastar,iastop,
     *                                             jastar,jastop
         write(io,*) ' isastr,isastp,jsastr,jsastp =',isastr,isastp,
     *                                             jsastr,jsastp
         write(io,*) ' (computed) ngx,ngy  =',ngx,ngy
         write(io,*) ' meshdn,npts,nareas  =',meshdn,npts,nareas
         write(io,*) ' nlines,nvols        =',nlines,nvols
         write(io,*) ' ndrec,nctrec,LSGRID =',ndrec,nctrec,LSGRID
         if(ndrec .GT. 0) then
            write(io,*) ' '
            write(io,*) 'Discrete Receptors (n,x,y,z):'
            do i=1,ndrec
               write(io,*) i,xrec(i),yrec(i),zrec(i)
            enddo
         endif
         if(nctrec .GT. 0) then
            write(io,*) ' '
            write(io,*) 'Complex Terrain Receptors (n,x,y,z,hill):'
            do i=1,nctrec
               write(io,*) i,xctr(i),yctr(i),zctr(i),ihill(i)
            enddo
         endif
         if (LVISIB) then
            write(io,*) ' '
            write(io,*) 'Surface Met Station UTMs (n,x,y):'
            do i=1,nssta
               write(io,*) i,xkmsta(i),ykmsta(i)
            enddo
         endif

         if(isrcinfo.EQ.1) then
            write(io,*)
            write(io,*)' Control-file POINT Sources : ',npt1
            write(io,*)' EMARB-file POINT Sources   : ',npt2
            write(io,*)' Control-file AREA Sources  : ',nar1
            write(io,*)' EMARB-file AREA Sources    : ',nar2
            write(io,*)' Control-file LINE Sources  : ',nln1
            write(io,*)' EMARB-file LINE Sources    : ',nln2
            write(io,*)' Control-file VOLUME Sources: ',nvl1
            write(io,*)' EMARB-file VOLUME Sources  : ',nvl2
            write(io,*)
            write(io,*)' Source Names'
            do i=1,nsrc
               write(io,*) csource(i)
            enddo
         endif
c --- endif
c
c  Set off data section from rest of output
      write(io,*)
      write(io,*)
      write(io,*)
c
c  Check starting hour and duration information in control file
c -- Find number of hours between start of data and processing start
      call DELTT(msyr,mjsday,mshr,isyr,jsday,ishr,idelhr)
      delhr8=DBLE(idelhr)+DBLE(issec4-mssec)/3600.D0
c -- Length of averaging period in hours
      avghr8=DBLE(nsecdt*mavgpd)/3600.D0
c -- Find number of averaging periods in this interval
      aper=SNGL(delhr8/avghr8)
      naper=NINT(aper)
c -- Length of interval (delhr) should be product of avg. time * # per,
c -- to the nearest second (.0002778 hour)
      if(ABS(DBLE(naper)*avghr8-delhr8).GT.0.0003D0) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'Start time is fractional number of averaging',
     &              ' periods from start of model data!'
         write(io,*) 'NPER= ',naper,' AVGHR= ',avghr8,' DELHR= ',delhr8
         ifatal=1
      endif
      if(naper .LT. 0) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'Start time preceeds beginning of data!'
         write(io,*) 'NAPER = ',naper
         ifatal=1
      endif
c -- Check for number of periods to process
      if(nper .GT. mnper) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'Number processing periods exceeds data periods!'
         write(io,*) 'NPER = ',nper,' MNPER = ',mnper
         ifatal=1
      endif
c -- Check ending hour in control file
      if(nper+naper .GT. mnper) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'End time extends beyond data!'
         write(io,*) 'NPER = ',nper,' NAPER = ',naper,' MNPER = ',mnper
         ifatal=1
      endif
c
c  Check options specified for receptor types
      if(LD .AND. ndrec .LT. 1) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'No discrete receptors found, so reset LD=F'
         write(io,*) 'LD = ',ld,' NDREC = ',ndrec
         ifatal=1
      elseif(ndrec .GT. 0) then
c ---    Condition discrete receptor range requested in control file
c ---    If first flag is -1, use all receptors
         if(ndrecp(1).EQ.-1) then
c ---       Reset to full range
            do ir=1,ndrec
               ndrecp(ir)=1
            enddo
            write(io,*)
            write(io,*) '**************    NOTICE    ***************'
            write(io,*) 'NDRECP array reset to full range: all 1s'
         endif
      endif
      if(LG .AND. .NOT.LSGRID) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'No grided receptors found, so reset LG=F'
         write(io,*) 'LG = ',lg,' LSGRID = ',lsgrid
         ifatal=1
      elseif(LSGRID) then
c ---    Condition gridded receptor range requested in control file
c ---    If all cell indexes are -1, use all receptors
         igtest=ibgrid+iegrid+jbgrid+jegrid
         if(igtest.EQ.-4) then
c ---       Reset to full range
            ibgrid=1
            jbgrid=1
            iegrid=ngx
            jegrid=ngy
            write(io,*)
            write(io,*) '**************    NOTICE    ***************'
            write(io,*) 'Gridded receptor range reset to NGX by NGY'
         else
c ---       Check for valid cell values
            if(ibgrid.LT.1 .OR. ibgrid.GT.ngx)      ifatal=-1
            if(iegrid.LT.ibgrid .OR. iegrid.GT.ngx) ifatal=-1
            if(jbgrid.LT.1 .OR. jbgrid.GT.ngy)      ifatal=-1
            if(jegrid.LT.jbgrid .OR. jegrid.GT.ngy) ifatal=-1
            if(ifatal.EQ.-1) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'Invalid gridded receptor subset specified'
               write(io,*) 'NGX, NGY       = ',ngx,ngy
               write(io,*) 'IBGRID,IEGRID  =',IBGRID,IEGRID
               write(io,*) 'JBGRID,JEGRID  =',JBGRID,JEGRID
               ifatal=1
            endif
         endif
      endif
      if(LCT .AND. nctrec .LT. 1) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'No terrain receptors found, so reset LCT=F'
         write(io,*) 'LCT = ',lct,' NCTREC = ',nctrec
         ifatal=1
      endif
      
c
c  Check dimension for "Top-N" tables: code for output must be changed
c  if more than the top 4 are needed
      if(mxtop .GT. 4) then
         write(io,*) 'Top-N tables limited to 4 ranks, MXTOP = ',mxtop
         write(io,*) 'The mxtop parameter should be set to 4   '
      endif
      if(ntop .GT. 4 .OR. ntop.GT.mxtop) then
         write(io,*) 'Top-N tables are limited to minimum of 4 or MXTOP'
         write(io,*) 'MXTOP, NTOP = ',mxtop,ntop
         ifatal=1
         ntop=MIN(4,mxtop)
      endif
c
c  Get rank of largest Nth-highest value specified
      nrnk=itop(1)
      do nn=2,ntop
         nrnk=MAX(nrnk,itop(nn))
      enddo
c
c  Check number of rank needed against maximum in parameter statement
      if(nrnk .GT. mxrnk) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'Rank exceeds allocated area!'
         write(io,*) '  NRNK, MXRNK =',nrnk,mxrnk
         write(io,*) '  Increase parameter MXRNK in PARAMS.PST, or'
         write(io,*) '  decrease largest value entered in itop array'
         ifatal=1
      endif
c
c  Check for species name
      if(.NOT.LVISIB) then
        char15=asplv
c ---   Special case: NO2 from NOx
        if(asplv(1:12).EQ.no2gas .AND. no2calc.GT.0) then
c ---      Need NOx in file to make NO2
           char15(1:12)=noxgas
c ---      Must not have both NO2 and NOx
           kno2=0
           knox=0
           do i=1,nszout
              if(asplst(i)(1:12) .EQ. no2gas) kno2 = kno2+1
              if(asplst(i)(1:12) .EQ. noxgas) knox = knox+1
           enddo
           if(kno2.GT.0 .AND. knox.GT.0) then
              write(io,*)
              write(io,*) '**************    FATAL    ***************'
              write(io,*)
     &        'Either NO2 or NOX (not both) is needed'
              write(io,*) 'Species list  = ',asplst
              ifatal=1
           endif
        endif
        do i=1,nszout
           if(char15 .EQ. asplst(i)) goto 50
        enddo
        write(io,*)
        write(io,*) '**************    FATAL    ***************'
        write(io,*) 'Species/level specified is not on modeled list!'
        write(io,*) 'Species/level = ',char15
        write(io,*) 'Species list  = ',asplst
        ifatal=1
50      continue
      endif

c  Check CALM processing (plume-type models)
c ------------------------------------------
      if(mcalmpro.EQ.1) then
         if(MET1FMT.NE.1) then
           write(io,*)
           write(io,*) '**************    FATAL    ***************'
           write(io,*) 'Invalid FORMAT for Single-Point met file: ',
     &                  met1fmt
           write(io,*) 'Expected AERMOD/AERMET -- MET1FMT = 1'
           ifatal=1
         endif
      elseif(mcalmpro.GT.1 .OR. mcalmpro.LT.0) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'Invalid CALM processing selection: ',mcalmpro
         write(io,*) 'Expected MCALMPRO = 0 or 1'
         write(io,*) 'Note: MCALMPRO = 0 unless you are processing an'
         write(io,*) 'output file from a plume-type model like AERMOD'
         ifatal=1
      endif

c  Check VISIBILITY inputs
c ------------------------
      if(LVISIB) then
c ---    Visibility processing only allowed for Hourly CALPUFF output
         if(nsecdt.NE.3600 .OR. mssec.NE.0) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'Visibility Processing Needs Hourly Data'
            write(io,*) 'Averaging period (sec) from model = ',nsecdt
            write(io,*) '     Starting at t(sec) in period = ',mssec
            ifatal=1
         endif

c ---    Conformity with FLAG 2008 configuration
         if(mvischeck.EQ.1)then
            if(aspec(1:5).NE.'VISIB') ifatal=-1
            if(.not.LVNO2) ifatal=-1
            if(no2calc.NE.1) ifatal=-1
            if(rno2nox.NE.1.0) ifatal=-1
            if(mvisbk.NE.8) ifatal=-1
            if(m8_mode.NE.5) ifatal=-1
            if(ifatal.EQ.-1) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'Visibility selections differ from FLAG 2008'
               write(io,*)'        Expected  Found'
               write(io,*)'ASPEC   = VISIB   ',aspec
               write(io,*)'LVNO2   = T       ',lvno2
               write(io,*)'NO2CALC = 1       ',no2calc
               write(io,*)'RNO2NOX = 1.0     ',rno2nox
               write(io,*)'MVISBK  = 8       ',mvisbk
               write(io,*)'M8_MODE = 5       ',m8_mode
               ifatal=1
            endif
         elseif(mvischeck.NE.0)then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*)'Visibility selection tests available are '
            write(io,*)'limited to two choices (0=not used or      '
            write(io,*)'1=FLAG 2008) -- Invalid value selected.   '   
            write(io,*)'MVISCHECK =  ',mvischeck
            write(io,*)
            ifatal=1
         endif

c --- Check to see if needed species/level is in model run list
         if(ilayer.ne.1) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*)
     &      'Visibility calculations can only be performed on layer 1'
            write(io,*) 'LVISIB = ',lvisib,' ILAYER = ',ilayer
            ifatal=1
         endif

         lok=.FALSE.
         if(LVSO4) then
            do i=1,nszout
               if(asplst(i)(1:12) .EQ. sulfate) lok = .TRUE.
            enddo
            if(.not.LOK) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'Sulfate species for visibility not found'
               write(io,*) 'Species list  = ',asplst
               ifatal=1
            endif
         endif
         lok=.FALSE.
         if(LVNO3) then
            do i=1,nszout
               if(asplst(i)(1:12) .EQ. nitrate) lok = .TRUE.
            enddo
            if(.not.LOK) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'Nitrate species for visibility not found'
               write(io,*) 'Species list  = ',asplst
               ifatal=1
            endif
         endif
         lok=.FALSE.
         if(LVNO2) then
c ---       Step 1:  Check for needed species
            if(no2calc.EQ.0) then
               do i=1,nszout
                  if(asplst(i)(1:12) .EQ. no2gas) lok = .TRUE.
               enddo
            elseif(no2calc.EQ.1 .OR. no2calc.EQ.2) then
               do i=1,nszout
                  if(asplst(i)(1:12) .EQ. noxgas) lok = .TRUE.
               enddo
            else
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'Invalid NO2CALC found in QAINP'
               write(io,*) 'Expected 0, 1, or 2 '
               write(io,*) 'NO2CALC  = ',no2calc
               ifatal=1
            endif
            if(.not.LOK) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'NO2 (or NOX) species for visibility not found'
               write(io,*) 'Species list  = ',asplst
               ifatal=1
            endif
c ---       Step 2:  Must not have both NO2 and NOx
            kno2=0
            knox=0
            do i=1,nszout
               if(asplst(i)(1:12) .EQ. no2gas) kno2 = kno2+1
               if(asplst(i)(1:12) .EQ. noxgas) knox = knox+1
            enddo
            if(kno2.GT.0 .AND. knox.GT.0) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'Either NO2 or NOX (not both) needed for visibility'
               write(io,*) 'Species list  = ',asplst
               ifatal=1
            endif
         endif
         lok=.FALSE.
         if(LVOC) then
            do i=1,nszout
               if(asplst(i)(1:12) .EQ. orgcarb) lok = .TRUE.
            enddo
            if(.not.LOK) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'Organic Carbon species for visibility not found'
               write(io,*) 'Species list  = ',asplst
               ifatal=1
            endif
         endif
         lok=.FALSE.
         if(LVEC) then
            do i=1,nszout
               if(asplst(i)(1:12) .EQ. lmncarb) lok = .TRUE.
            enddo
            if(.not.LOK) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'Elemental Carbon species for visibility not found'
               write(io,*) 'Species list  = ',asplst
               ifatal=1
            endif
         endif
         lok=.FALSE.
         if(LVPMC) then
            do i=1,nszout
               if(asplst(i)(1:12) .EQ. specpmc) lok = .TRUE.
            enddo
            if(.not.LOK) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'Coarse PM species for visibility not found'
               write(io,*) 'Species list  = ',asplst
               ifatal=1
            endif
         endif
         lok=.FALSE.
         if(LVPMF) then
            do i=1,nszout
               if(asplst(i)(1:12) .EQ. specpmf) lok = .TRUE.
            enddo
            if(.not.LOK) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*)
     &         'Fine PM species for visibility not found'
               write(io,*) 'Species list  = ',asplst
               ifatal=1
            endif
         endif
c
c --- Remove this check
cc  Check to see if VISB file and concentration file have same format
c        if(iptime.NE.irhtime .AND. .not.LRHFAC) then
c          write(io,*)
c          write(io,*) '**************    FATAL    ***************'
c          write(io,*) 'CONC file and VISB file must have same format'
c          write(io,*) 'CONC, VISB formats = ',iptime,irhtime
c          write(io,*) 'These files are from different CALPUFF versions'
c          ifatal=1
c        endif

c ---    MVISBK range
         if(mvisbk.LT.1 .OR. mvisbk.GT.8) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'MVISBK is out of range 1-8'
            write(io,*) '  MVISBK = ',mvisbk
            write(io,*)
            ifatal=1
         endif
c ---    MVISBK = 7
         if(mvisbk.EQ.7) then
            if(nwsta.LE.0) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'Weather station IDs missing for Method 7'
               write(io,*) '  NWSTA = ',nwsta
               write(io,*)
               ifatal=1
            endif
         endif

c ---    Check range (0. to 100.) of percentage of particles affected
c ---    by humidity and convert to fraction
         if(mvisbk.EQ.1) then
            if(rhfrac.GT.100. .OR. rhfrac.LT.0.) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'RHFRAC outside physical range!'
               write(io,*) '  RHFRAC =',rhfrac
               write(io,*) '  Input as % in the range 0 to 100'
               ifatal=1
            else
               rhfrac=rhfrac*.01
            endif
         endif

c ---    Check method selected for f(RH) curves (MFRH=1,2,3,4)
         if(mfrh.LT.1 .OR. mfrh.GT.4) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'MFRH outside expected range!'
            write(io,*) '  MFRH =',mfrh
            write(io,*) '  Input as 1, 2, 3, or 4'
            ifatal=1
         endif
         if(mvisbk.EQ.8 .AND. M8_MODE.LE.3 .AND. mfrh.NE.4) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'IMPROVE (2006) FRH curves used in Method 8'
            write(io,*) '  Expected MFRH = 4 with MVISBK = ',mvisbk
            write(io,*) '             along with M8_MODE = ',m8_mode
            write(io,*) '  Found    MFRH = ',mfrh
            ifatal=1
         endif
         if(mvisbk.NE.8 .AND. mfrh.EQ.4) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'IMPROVE (2006) FRH curves used in Method 8'
            write(io,*) '  Expected MVISBK = 8 with MFRH = ',mfrh
            write(io,*) '  Found    MVISBK = ',mvisbk
            ifatal=1
         endif

c ---    Check mode selected for Method 8 (M8_MODE=1,2,3,4,5)
         if(mvisbk.EQ.8 .AND.
     &     (m8_mode.LT.1 .OR. m8_mode.GT.5)) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) '  Expected M8_MODE = 1-5 for MVISBK = ',mvisbk
            write(io,*) '  Found    M8_MODE = ',m8_mode
            ifatal=1
         endif

c ---    Restrict extinction output (if selected) to 24-hr and
c ---    run-length averages when daily mode is selected for Method 8
c ---    (M8_MODE=3 or 5)
         if(mvisbk.EQ.8 .AND. (m8_mode.EQ.3 .OR. m8_mode.EQ.5)) then
           if(L1PD .OR. L1HR .OR. L3HR .OR. LNAVG) then
             write(io,*)
             write(io,*) '**************    FATAL    ***************'
             write(io,*) '  Averaging time selection for extinction '
             write(io,*) '  output for Method 8, Mode 3 or 5 is'
             write(io,*) '  restricted to 24-hr (L24HR) and'
             write(io,*) '  run-length (LRUNL):'
             write(io,*) '      L1PD = ',L1PD
             write(io,*) '      L1HR = ',L1HR
             write(io,*) '      L3HR = ',L3HR
             write(io,*) '     L24HR = ',L24HR
             write(io,*) '     LNAVG = ',LNAVG
             write(io,*) '     LRUNL = ',LRUNL
             ifatal=1
           endif
         endif

c ---    Check RHMAX (%) range (0. to 100.) and convert to fraction
         if(rhmax.GT.100. .OR. rhmax.LT.0.) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'RHMAX outside physical range!'
            write(io,*) '  RHMAX =',rhmax
            write(io,*) '  Input as % in the range 0 to 100'
            ifatal=1
         else
            rhmax=rhmax*.01
         endif

c ---    Check for valid Relative Humidity factors (if needed)
         if(LRHFAC) then
            rhmin6=1.0
            rhmin8=1.0
            if(mvisbk.EQ.6) then
c ---          Test RHFAC for Method 6
               do m=1,12
                  rhmin6=AMIN1(rhmin6,rhfac(m))
               enddo
            elseif(mvisbk.EQ.8) then
               if(m8_mode.EQ.2 .OR. m8_mode.EQ.3) then
c ---             Test RHFAC for Method 8
                  do m=1,12
                     rhmin6=AMIN1(rhmin6,rhfac(m))
                  enddo
               elseif(m8_mode.EQ.4 .OR. m8_mode.EQ.5) then
c ---             Test RHFSML, RHFLRG, RHFSEA for Method 8
                  do m=1,12
                     rhmin8=AMIN1(rhmin8,rhfsml(m))
                     rhmin8=AMIN1(rhmin8,rhflrg(m))
                     rhmin8=AMIN1(rhmin8,rhfsea(m))
                  enddo
               endif
            endif
            if(rhmin6.LE.0.0) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'RHFAC outside physical range!'
               write(io,*) '  RHFAC =',(rhfac(m),m=1,3)
               write(io,*) '         ',(rhfac(m),m=4,6)
               write(io,*) '         ',(rhfac(m),m=7,9)
               write(io,*) '         ',(rhfac(m),m=10,12)
               write(io,*) '  Must be > 0 for all 12 months'
               ifatal=1
            endif
            if(rhmin8.LE.0.0) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'RHFSML/RHFLRG/RHFSEA outside range!'
               write(io,*) '  RHFSML =',(rhfsml(m),m=1,3)
               write(io,*) '          ',(rhfsml(m),m=4,6)
               write(io,*) '          ',(rhfsml(m),m=7,9)
               write(io,*) '          ',(rhfsml(m),m=10,12)
               write(io,*) '  RHFLRG =',(rhflrg(m),m=1,3)
               write(io,*) '          ',(rhflrg(m),m=4,6)
               write(io,*) '          ',(rhflrg(m),m=7,9)
               write(io,*) '          ',(rhflrg(m),m=10,12)
               write(io,*) '  RHFSEA =',(rhfsea(m),m=1,3)
               write(io,*) '          ',(rhfsea(m),m=4,6)
               write(io,*) '          ',(rhfsea(m),m=7,9)
               write(io,*) '          ',(rhfsea(m),m=10,12)
               write(io,*) '  Must be > 0 for all 12 months'
               ifatal=1
            endif
         endif

c ---    Check auxiliary visibility output (MDVIS=0,1,2,3,4)
         if(mdvis.LT.0 .OR. mdvis.GT.4) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'MDVIS outside expected range!'
            write(io,*) '  MDVIS =',mdvis
            write(io,*) '  Input as 0, 1, 2, 3 or 4'
            ifatal=1
         endif

c ---    Check for ax+b scaling
         if(LSCALE) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'aX+b concentration scaling is not supported'
            write(io,*) 'with visibility calculations!'
            ifatal=1
         endif

c ---    Check for background concentration option
         if(LBACK) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'Adding a background concentration is not '
            write(io,*) 'supported with visibility calculations!'
            ifatal=1
         endif

      endif
c
c  Check for required exceedance thresholds
      lok=.TRUE.
      if(LEXCD) then
         if(L1HR .AND. thresh1.LT.0.0) lok=.FALSE.
         if(L3HR .AND. thresh3.LT.0.0) lok=.FALSE.
         if(L24HR .AND. thresh24.LT.0.0) lok=.FALSE.
         if(LNAVG .AND. threshN.LT.0.0) lok=.FALSE.
      endif
      if(.not.LOK) then
        write(io,*)
        write(io,*) '**************    FATAL    ***************'
        write(io,*)
     &  'Valid threshold for exceedance output not found'
        if(L1HR)  write(io,*) ' 1-HR Threshold = ',thresh1
        if(L3HR)  write(io,*) ' 3-HR Threshold = ',thresh3
        if(L24HR) write(io,*) '24-HR Threshold = ',thresh24
        if(LNAVG) write(io,*) ' N-HR Threshold = ',threshN
        ifatal=1
      endif

c  Restrictions on source contribution processing
      if(msrc.LT.0 .OR. msrc.GT.2) then
          write(io,*)
          write(io,*) '**************    FATAL    ***************'
          write(io,*) 'Invalid choice for MSOURCE:  ',msrc
          write(io,*) 'Value must be 0, 1, or 2'
          ifatal=1
      endif
      if(msrc.GT.0 .AND. isrcindv.EQ.0) then
          write(io,*)
          write(io,*) '**************    FATAL    ***************'
          write(io,*) 'MSOURCE:  ',msrc
          write(io,*) 'Requires source contributions in CALPUFF file'
          ifatal=1
      endif
      if(msrc.EQ.2) then
        if(LVISIB) then
c  Not available for Visibility processing
          write(io,*)
          write(io,*) '**************    FATAL    ***************'
          write(io,*) 'Source contribution TRACEBACK analysis is'
          write(io,*) 'not allowed for visibility Processing'
          ifatal=1
        endif
c  There must be only 1 receptor processed (which one?)
        icount=0
c ---   Count gridded receptors
        if(ngx .NE. 0 .AND. LG) then
          do j=jbgrid,jegrid
            do i=ibgrid,iegrid
              if(ngrecp(i,j).EQ.1) then
                icount=icount+1
                LGS=.TRUE.
                isrec=i
                jsrec=j
              endif
            enddo
          enddo
        endif
c ---   Count discrete receptors
        if(ld .AND. ndrec .NE. 0) then
          do i=1,ndrec
            if(ndrecp(i).EQ.1) then
              icount=icount+1
              LDS=.TRUE.
              isrec=i
              jsrec=0
            endif
          enddo
        endif
        if(icount.NE.1 .AND. MSAMPLER.LT.2) then
          write(io,*)
          write(io,*) '**************    FATAL    ***************'
          write(io,*) 'Source contribution TRACEBACK analysis is'
          write(io,*) 'available for 1 receptor ONLY.  The number'
          write(io,*) 'of gridded and discrete receptors selected'
          write(io,*) 'totals ',icount
          ifatal=1
        endif
      endif

c --- Check time zones in VSRN.DAT
      ltzone=.FALSE.
      do iw=1,mxwsta
         if(idwsta(iw).NE.0) then
            if(tzone(iw).LE.-13.0 .OR. tzone(iw).GE.13.0)
     &         ltzone=.TRUE.
         endif
      enddo

c --- TZONE
      if(nwsta.GT.0 .AND. LTZONE) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'Invalid time zone for 1 or more Weather'
         write(io,*) 'stations for Method 7.  Must be -12 to 12'
         write(io,*)
         ifatal=1
      endif

c --- METRUN
      if(metrun.LT.0 .OR. metrun.GT.1) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'METRUN is out of range 0-1'
         write(io,*) '  METRUN = ',metrun
         write(io,*)
         ifatal=1
      endif

c --- Compare BTZONE/BTZONE2 entries from CALPOST.INP (if any)
      if(btzone.LT.-900.) then
         btzone=btzone2
      elseif(btzone2.GT.-900.) then
         write(azone,'(f6.2)') btzone
         write(azone2,'(f6.2)') btzone2
         if(azone.NE.azone2) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'Different CALPUFF base time zones found in'
            write(io,*) 'control file input groups 1 and 2.'
            write(io,*) '  Group 1  BTZONE = ',btzone
            write(io,*) '  Group 2  BTZONE = ',btzone2
            write(io,*)
            ifatal=1
         endif
      endif

c --- BTZONE (from CALPOST.INP) / XBTZ (from CALPUFF.DAT)
      if(XBTZ.LE.-900.) then
c ---    Time zone missing in CALPUFF.DAT, assign CALPOST.INP value
         XBTZ=BTZONE
      elseif(BTZONE.LE.-900.) then
c ---    Time zone provided in CALPUFF.DAT, but not CALPOST.INP
         BTZONE=XBTZ
      endif
      if(XBTZ.LE.-900.) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'CALPUFF base time zone is required and'
         write(io,*) 'must be provided as BTZONE in control file'
         write(io,*)
         ifatal=1
      elseif(XBTZ.NE.BTZONE) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'CALPUFF base time zone found in CALPUFF.DAT'
         write(io,*) 'must match base time zone in control file.'
         write(io,*) '  CALPUFF.DAT  XBTZ   = ',xbtz
         write(io,*) '  CALPOST.INP  BTZONE = ',btzone
         write(io,*)
         ifatal=1
      endif
c --- IPRTU
      if(iprtu.LT.1 .OR. iprtu.GT.5) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'IPRTU is out of range 1-5'
         write(io,*) '  IPRTU = ',iprtu
         write(io,*)
         ifatal=1
      endif
c --- NTOP
      if(ntop.LT.1 .OR. ntop.GT.4) then
         write(io,*)
         write(io,*) '**************    FATAL    ***************'
         write(io,*) 'NTOP is out of range 1-4'
         write(io,*) '  NTOP = ',ntop
         write(io,*)
         ifatal=1
      endif

c --- Checks on NO2 processing options selected
      if(LVNO2 .OR. aspec.EQ.no2gas) then
         if(no2calc.EQ.1) then
c ---       Check range (0. to 1.0) of fraction of NOx used as NO2
            if(rno2nox.LT.0. .OR. rno2nox.GT.1.0) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'RNO2NOX is out of range 0.0-1.0'
               write(io,*) '  RNO2NOX = ',rno2nox
               write(io,*)
               ifatal=1
            endif
         elseif(no2calc.EQ.2) then
c ---       Check NO2/NOx tabulation
            itest=0
            ictest=0
            iftest=0
            do k=1,13
               if(bincnox(k).GT.bincnox(k+1)) itest=1
            enddo
            do k=1,14
               if(bincnox(k).LT.0.0) ictest=1
               if(binfno2(k).LT.0.0 .OR. binfno2(k).GT.1.0) iftest=1
            enddo
            if(itest.GT.0) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'Tabulated CNOX concentration sequence must'
               write(io,*) 'not decrease in value'
               write(io,*) '  CNOX = ',bincnox
               write(io,*)
               ifatal=1
            endif
            if(ictest.GT.0) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'Tabulated CNOX concentration is Invalid'
               write(io,*) '  CNOX = ',bincnox
               write(io,*)
               ifatal=1
            endif
            if(iftest.GT.0) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'Tabulated TNO2NOX Ratio is Invalid'
               write(io,*) '  TNO2NOX = ',binfno2
               write(io,*)
               ifatal=1
            endif
c ---       Check for CONCENTRATION
            if(aspec.EQ.no2gas .AND. ilayer.LE.0) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'Use of TNO2NOX Ratio Requires CONCENTRATION'
               write(io,*) '  Expected ILAYER > 0'
               write(io,*) '  Found    ILAYER = ',ilayer
               write(io,*)
               ifatal=1
            endif
c ---       Check for ax+b scaling
            if(LSCALE) then
               write(io,*)
               write(io,*) '**************    FATAL    ***************'
               write(io,*) 'aX+b concentration scaling cannot be used'
               write(io,*) 'with tabulated NO2/NOx ratios!'
               ifatal=1
            endif
         elseif(no2calc.NE.0) then
            write(io,*)
            write(io,*) '**************    FATAL    ***************'
            write(io,*) 'NO2CALC is out of range 0-2'
            write(io,*) '  NO2CALC = ',no2calc
            write(io,*)
            ifatal=1
         endif
      endif

c
c  Stop processing here if a fatal error has been found
      if(ifatal .EQ. 1) then
         write(*,*) 'FATAL ERRORS were found in CALPOST.INP!'
         write(*,*) 'Review the messages written to the list-file.'
         stop
      endif
c
      return
101   format(3(10i2,4x))
102   format(6i2)
      end
c-----------------------------------------------------------------------
      subroutine skiprec(io,tcg,tcd,tct,ix,iy,idrec,itrec,ieof)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017           SKIPREC
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Reads hourly data records from "concentration" file
c               and passes information to main program through
c               common /conc/, but does not process data.
c               (From GETRCRD)
c
c  UPDATES:
c  V5.1(990709) to V5.6(031017)
c               (DGS) Add changes for dataset version 2.1 with
c                     source contribution (use RDCONC)
c  V5.0(981025) to V5.1(990709)
c               (DGS) begin & end time resolved to seconds
c  V5.0(980918) to V5.0(981025)
c               (DGS) remove RH variables
c  V5.0(980821) to V5.0(980918)
c               (DGS) Do not skip records in VISB.DAT file, because
c                     it may cover a different period of time
c
c
c  ARGUMENTS:
c     PASSED:   io        Unit number for accessing input file       [i]
c               tcg(i,j)  Temporary gridded receptor array          [ra]
c               tcd(i)    Temporary discrete receptor array         [ra]
c               tct(i)    Temporary complex terrain receptor array  [ra]
c               ix,iy     Dimensions for tcg array                   [i]
c               idrec     Dimension for tcd array                    [i]
c               itrec     Dimension for tct array                    [i]
c               ieof      End of file flag (1=EOF encountered)       [i]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   RDCONC
c-----------------------------------------------------------------------
c
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c
c  Declare temporary variables using actual array dimensions
      character*15 tsplv
      real tcg(ix,iy),tcd(idrec),tct(itrec)
c
c  Initialize source loop variables
      isrc=0
      ktype0=-999
      knum0=-999

c  Loop over sources
c ------------------
1     isrc=isrc+1

c ---    Read concentration data 
c ---    Loop over chemical species/levels
         do isl=1,nszout
            call RDCONC(io,isrc,ktype0,knum0,isl,
     &                  tcg,tcd,tct,ix,iy,idrec,itrec,ieof,tsplv)
            if(ieof.EQ.1) return
         enddo

c --- Loop back for more data if reading source contributions
c --- There should be NSRC+1 (individuals plus total)
      if(isrcindv.GT.0 .AND. isrc.LE.nsrc) goto 1

      return
      end
c-----------------------------------------------------------------------
      subroutine getrcrd(io,iorh,tcg,tcd,tct,ix,iy,idrec,itrec,ieof)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080709           GETRCRD
c ---           D. Strimaitis
c
c  PURPOSE:     Reads data records from "concentration" file for one
c               time period and passes information to main program
c               through commons /conc/, /vispec/, and /specout/
c
c  UPDATES:
c  V6.212(080404) to V6.22(080709)
c               (DGS) Add logic for direct use of F(RH) in Method 8
c  V6.2(070712)   to V6.212(080404)
c               (CEC) NO2 processing was applied for only one of the
c                     receptor types (gridded, discrete, OR terrain)
c                     due to a bug in the logic.  Change to independent
c                     if-blocks for each receptor type.
c  V6.141(061120) to V6.2(070712)
c               (DGS) Add tabulated NO2/NOx ratios, and apply after all
c                     input is done (NO2/NOx may vary with NOx)
c  V6.14(061107) to V6.141(061120)
c               (DGS) Store all species for subsequent processing in
c                     ACONC arrays
c               (DGS) Use with visibility method 8 with M8_MODE=3
c                     (replace IRHTYPE with M8_MODE)
c  V6.12(060309) to V6.14(061107)
c               (DGS) Add Method 8 variable extinction efficiency
c                  -  Different F(RH) for small, large, and sea salt
c                     components
c                  -  Distinguish between Method 6 and Method 8 use of
c                     monthly RHFAC table
c                  -  Add NO2 extinctions (source emissions)
c  V5.65(050729) to V6.12(060309)
c               (CEC) Change mxgx and mxgy to ielmet and jelmet -
c               the actual size of CALMET grid when checking the
c               receptors indices against the CALMET grid.
c  V5.631(040917) to V5.65(050729)
c               (DGS) Add SAMPLER processing (call SUMSAMP)
c  V5.63(031017) to V5.631(040917)
c               (CEC) Add new extinction coefficient computations
c               for method 7 using MM5 liquid water content.
c  V5.5(030627) to V5.6(031017)
c               (DGS) Add changes for dataset version 2.1
c               (DGS) Add source contribution (restructure loops)
c               (DGS) Use discrete receptor array as a source array
c                     when using source TRACEBACK feature (MSRC=2)
c  V5.4(030402) to V5.5(030627)
c               (DGS) Add MVISBK=7 option
c               (DGS) Add additional DEBUG output for visibility
c               (DGS) Add hourly extinction output for visibility
c  V5.2(991104a) to V5.4(030402)
c               (FRR) 2D array of relative humidity  (dimensioned as
c                     actual met grid)
c               (DGS) add list-file unit to INCR, YR4, GRDAY
c               (DGS) replace ndathre with mdathre in GETVSR call
c  V5.1(990920) to V5.2(991104a)
c               (DGS) Add Elemental Carbon as modeled species
c  V5.1(990709) to V5.1(990920)
c               (DGS) Read, but do not process receptor values when
c                     corresponding LG/LD/LCT control is FALSE
c               (DGS) Add logic for MVISBK=6 (RHFACs from control file)
c  V5.0(990228a)to V5.1(990709)
c               (DGS) begin & end time resolved to seconds
c                     and enforce YYYY format for year
c  V5.0(981116) to V5.0(990228a)
c               (DGS) Add gridded receptor exclusion filter
c  V5.0(981025) to V5.0(981116)
c               (DGS) Add Rayleigh bext to nephelometer measurement
c  V5.0(980918) to V5.0(981025)
c               (DGS) Activate LVBK
c               (DGS) Place modeled extinction arrays in /VISPEC/
c               (DGS) Process sub-range of gridded receptors
c  V5.0(980821) to V5.0(980918)
c               (DGS) Add MVISBK=3 option
c               (DGS) Add MVISBK=4 option
c               (DGS) Add record-skipping logic for VISB.DAT file
c  V5.0(980430) to V5.0(980821)
c               (DGS) Add new visibility processing option (MVISBK=2)
c               (DGS) Report F(RH) arrays rather than RH arrays
c               (DGS) Process sub-range of discrete receptors
c
c  V5.0(971007) to V5.0(980430)
c               (DGS) Use LCTSG to determine if data available at
c                     CTSG receptors
c
c  V3.2(960716) to V5.0(971007)
c               (DGS) XWORK2(mxgrec) changed to XWORK2(mxdrec)
c               (DGS) Add EOF processing
c
c  ARGUMENTS:
c     PASSED:   io        Unit number for accessing input file       [i]
c               tcg(i,j)  Temporary gridded receptor array          [ra]
c               tcd(i)    Temporary discrete receptor array         [ra]
c               tct(i)    Temporary complex terrain receptor array  [ra]
c               ix,iy     Dimensions for tcg array                   [i]
c               idrec     Dimension for tcd array                    [i]
c               itrec     Dimension for tct array                    [i]
c               ieof      End of file flag (1=EOF encountered)       [i]
c            imetx,imety  Dimensions for irh2d array                 [i]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   RDCONC, RDRELH, GRDAY, GETWX, GETVSR,
c                       EXRH, GROWTH, SUMSAMP, FRH4, IMPRV06,
c                       MAKENO2
c-----------------------------------------------------------------------
c
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'source.pst'
      INCLUDE 'specout.pst'
      INCLUDE 'visb.pst'
      INCLUDE 'vispec.pst'
      INCLUDE 'weathr.pst'
      INCLUDE 'mm4hdo.pst'

c --- Local variables
      character*20 wstring
      CHARACTER*20 wstring3(mxnxp,mxnyp)
      logical luse

c --- Add local variables for NO2 calculation
      logical LNO2X
c --- Add local visibility NOx arrays for conversion to NO2
      real gnox(mxgx,mxgy),dnox(mxdrec),tnox(mxctrec)
c
c  Declare temporary variables using actual array dimensions
      character*16 cnamsrc
      character*15 tsplv
      character*12 nitrate, sulfate, orgcarb, lmncarb
      character*12 no2gas,noxgas
      real tcg(ix,iy),tcd(idrec),tct(itrec)
      REAL bvis3(mxnxp,mxnyp)
      integer irhss(mxss)
c frr (09/01) 2D array of relative humidity (dimensioned as met grid)
      integer irh2d(mxgx,mxgy)
c
      data one/1.0/
c
c --- Species names
      data nitrate/'NO3         '/,sulfate/'SO4         '/
      data orgcarb/'SOA         '/,lmncarb/'EC          '/
      data no2gas /'NO2         '/,noxgas /'NOX         '/
c
c --- Factor to convert g/m3 to micro-g/m3 for extinctions
      data fmicro/1.e6/
c
c  Initialize output arrays to zero

c --- Loop over species
      do k=1,nospec
         if(LSGRID) then
            do i=1,ngx
            do j=1,ngy
               aconcg(i,j,k)=0.0
               if(k.EQ.1) then
                  concg(i,j)=0.0
                  gso4(i,j)=0.0
                  gno3(i,j)=0.0
                  gno2(i,j)=0.0
                  gnox(i,j)=0.0
                  goc(i,j) =0.0
                  gec(i,j) =0.0
                  gpmc(i,j)=0.0
                  gpmf(i,j)=0.0
               endif
            enddo
            enddo
         endif
         if(ndrec .GT. 0) then
            do i=1,ndrec
               aconcd(i,k)=0.0
               if(k.EQ.1) then
                  concd(i)=0.0
                  dso4(i)=0.0
                  dno3(i)=0.0
                  dno2(i)=0.0
                  dnox(i)=0.0
                  doc(i) =0.0
                  dec(i) =0.0
                  dpmc(i)=0.0
                  dpmf(i)=0.0
               endif
            enddo
         endif
         if(nctrec .GT. 0) then
            do i=1,nctrec
               aconct(i,k)=0.0
               if(k.EQ.1) then
                  conct(i)=0.0
                  tso4(i)=0.0
                  tno3(i)=0.0
                  tno2(i)=0.0
                  tnox(i)=0.0
                  toc(i) =0.0
                  tec(i) =0.0
                  tpmc(i)=0.0
                  tpmf(i)=0.0
               endif
            enddo
         endif
      enddo
c
c  Initialize source loop variables
      isrc=0
      ktype0=-999
      knum0=-999

c --- Prepare for NO2 calculations
      LNO2X=.FALSE.
      if(no2calc.GT.0 .AND. asplv(1:12).EQ.'NO2         ') then
         LNO2X=.TRUE.
      endif
c
c  Top of loop over sources
c -------------------------
1     isrc=isrc+1

c  Read concentration data, keep species/level to be processed,
c  and store species used for visibility
c  Store all species from requested level in "aconc" arrays
c --- Loop over chemical species/levels
        do isl=1,nszout

          call RDCONC(io,isrc,ktype0,knum0,isl,
     &                tcg,tcd,tct,ix,iy,idrec,itrec,ieof,tsplv)
          if(ieof.EQ.1) return

c ---     Should these data be used?
          luse=.FALSE.
          if(isrcindv.EQ.0) then
c ---        Only the total from all sources is available
             luse=.TRUE.
          elseif(isrc.LE.nsrc .AND. (msrc.EQ.1 .OR. msrc.EQ.2)) then
c ---        This individual source contribution is needed
             luse=.TRUE.
          elseif(isrc.GT.nsrc .AND. msrc.EQ.0) then
c ---        This is the total, and it is needed for MSRC=0
             luse=.TRUE.
          endif

          if(LUSE) then

            if(msampler.GT.0) then
c ---          Process data for SAMPLER option
               call SUMSAMP(isrc,isl,tcd,idrec)
            endif

c ---       Identify species index in output processing list
            ispec=mapospec(isl)

            if(LSGRID) then
               if(msrc.EQ.2 .AND. LGS) then
c ---            Store value at target receptor into CONCD array
c ---            for this source
                 if(ispec.GT.0) aconcd(isrc,ispec)=tcg(isrec,jsrec)
                 if(tsplv .EQ. asplv) then
                    concd(isrc)=tcg(isrec,jsrec)
                 endif

               elseif(LVISIB .AND. LG)then
c ---            Update sum for processed species over all sources
                 if(ispec.GT.0) then
                    do i=ibgrid,iegrid
                      do j=jbgrid,jegrid
                        aconcg(i,j,ispec)=aconcg(i,j,ispec)+tcg(i,j)
                      enddo
                    enddo
                 endif
c ---            Store extinction coefficient components
                 if(tsplv(1:12) .EQ. nitrate .AND. LVNO3)then
                   do i=ibgrid,iegrid
                     do j=jbgrid,jegrid
                       gno3(i,j)=gno3(i,j)+tcg(i,j)
                     enddo
                   enddo
                 elseif(tsplv(1:12) .EQ. sulfate .AND. LVSO4)then
                   do i=ibgrid,iegrid
                     do j=jbgrid,jegrid
                       gso4(i,j)=gso4(i,j)+tcg(i,j)
                     enddo
                   enddo
                 elseif(tsplv(1:12) .EQ. orgcarb .AND. LVOC)then
                   do i=ibgrid,iegrid
                     do j=jbgrid,jegrid
                       goc(i,j)=goc(i,j)+tcg(i,j)
                     enddo
                   enddo
                 elseif(tsplv(1:12) .EQ. lmncarb .AND. LVEC)then
                   do i=ibgrid,iegrid
                     do j=jbgrid,jegrid
                       gec(i,j)=gec(i,j)+tcg(i,j)
                     enddo
                   enddo
                 elseif(tsplv(1:12) .EQ. specpmc .AND. LVPMC)then
                   do i=ibgrid,iegrid
                     do j=jbgrid,jegrid
                       gpmc(i,j)=gpmc(i,j)+tcg(i,j)
                     enddo
                   enddo
                 elseif(tsplv(1:12) .EQ. specpmf .AND. LVPMF)then
                   do i=ibgrid,iegrid
                     do j=jbgrid,jegrid
                       gpmf(i,j)=gpmf(i,j)+tcg(i,j)
                     enddo
                   enddo
                 elseif(tsplv(1:12) .EQ. no2gas .AND. LVNO2)then
                   do i=ibgrid,iegrid
                     do j=jbgrid,jegrid
                       gno2(i,j)=gno2(i,j)+tcg(i,j)
                     enddo
                   enddo
                 elseif(tsplv(1:12) .EQ. noxgas .AND. LVNO2)then
c ---              Fraction of NOX may be used as NO2
                   do i=ibgrid,iegrid
                     do j=jbgrid,jegrid
                       gnox(i,j)=gnox(i,j)+tcg(i,j)
                     enddo
                   enddo
                 endif

               elseif(LG) then
c ---            Update sum for processed species over all sources
                 if(ispec.GT.0) then
                    do i=ibgrid,iegrid
                      do j=jbgrid,jegrid
                        aconcg(i,j,ispec)=aconcg(i,j,ispec)+tcg(i,j)
                      enddo
                    enddo
                 endif
c ---            Update sum over all sources
                 if(tsplv .EQ. asplv) then
                    do i=ibgrid,iegrid
                      do j=jbgrid,jegrid
                        concg(i,j)=concg(i,j)+tcg(i,j)
                      enddo
                    enddo
                 endif
               endif
            endif
c
            if(ndrec .GT. 0) then
               if(msrc.EQ.2 .AND. LDS) then
c ---            Store value at target receptor into CONCD array
c ---            for this source
                 if(ispec.GT.0) aconcd(isrc,ispec)=tcd(isrec)
                 if(tsplv .EQ. asplv) then
                    concd(isrc)=tcd(isrec)
                 endif

               elseif(LVISIB .AND. LD)then
c ---            Update sum for processed species over all sources
                 if(ispec.GT.0) then
                    do i=1,ndrec
                      aconcd(i,ispec)=aconcd(i,ispec)+tcd(i)
                    enddo
                 endif
c ---            Store extinction coefficient components
                 if(tsplv(1:12) .EQ. nitrate .AND. LVNO3)then
                   do i=1,ndrec
                      dno3(i)=dno3(i)+tcd(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. sulfate .AND. LVSO4)then
                   do i=1,ndrec
                      dso4(i)=dso4(i)+tcd(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. orgcarb .AND. LVOC)then
                   do i=1,ndrec
                      doc(i)=doc(i)+tcd(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. lmncarb .AND. LVEC)then
                   do i=1,ndrec
                      dec(i)=dec(i)+tcd(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. specpmc .AND. LVPMC)then
                   do i=1,ndrec
                      dpmc(i)=dpmc(i)+tcd(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. specpmf .AND. LVPMF)then
                   do i=1,ndrec
                      dpmf(i)=dpmf(i)+tcd(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. no2gas .AND. LVNO2)then
                   do i=1,ndrec
                      dno2(i)=dno2(i)+tcd(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. noxgas .AND. LVNO2)then
c ---              Fraction of NOX may be used as NO2
                   do i=1,ndrec
                      dnox(i)=dnox(i)+tcd(i)
                   enddo
                 endif

               elseif(LD) then
c ---            Update sum for processed species over all sources
                 if(ispec.GT.0) then
                    do i=1,ndrec
                      aconcd(i,ispec)=aconcd(i,ispec)+tcd(i)
                    enddo
                 endif
c ---            Update sum over all sources
                 if(tsplv .EQ. asplv) then
                    do i=1,ndrec
                       concd(i)=concd(i)+tcd(i)
                    enddo
                 endif
               endif
            endif
c
            if(LCTSG) then
               if(LCT) then
c ---            Update sum for processed species over all sources
                 if(ispec.GT.0) then
                    do i=1,nctrec
                       aconct(i,ispec)=aconct(i,ispec)+tct(i)
                    enddo
                 endif
               endif
               if(LVISIB .AND. LCT)then
c ---            Store extinction coefficient components
                 if(tsplv(1:12) .EQ. nitrate .AND. LVNO3)then
                   do i=1,nctrec
                      tno3(i)=tno3(i)+tct(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. sulfate .AND. LVSO4)then
                   do i=1,nctrec
                      tso4(i)=tso4(i)+tct(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. orgcarb .AND. LVOC)then
                   do i=1,nctrec
                      toc(i)=toc(i)+tct(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. lmncarb .AND. LVEC)then
                   do i=1,nctrec
                      tec(i)=tec(i)+tct(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. specpmc .AND. LVPMC)then
                   do i=1,nctrec
                      tpmc(i)=tpmc(i)+tct(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. specpmf .AND. LVPMF)then
                   do i=1,nctrec
                      tpmf(i)=tpmf(i)+tct(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. no2gas .AND. LVNO2)then
                   do i=1,nctrec
                      tno2(i)=tno2(i)+tct(i)
                   enddo
                 elseif(tsplv(1:12) .EQ. noxgas .AND. LVNO2)then
c ---              Fraction of NOX may be used as NO2
                   do i=1,nctrec
                      tnox(i)=tnox(i)+tct(i)
                   enddo
                 endif

               elseif(LCT) then
c ---            Update sum over all sources
                 if(tsplv .EQ. asplv) then
                    do i=1,nctrec
                       conct(i)=conct(i)+tct(i)
                    enddo
                 endif
               endif
            endif

          endif
        enddo

c --- Loop back for more data if reading source contributions
c --- There should be NSRC+1 (individuals plus total)
      if(isrcindv.GT.0 .AND. isrc.LE.nsrc) goto 1
c -----------------------------------------------

c --- Apply NO2/NOx conversion ratio to add computed NO2 (from NOx)
c --- to any modeled NO2 (conditional)
      if(LSGRID) then
         if(msrc.EQ.2 .AND. LGS) then
c ---       Source TRACEBACK
            if(LNO2X) then
c ---          Update NO2 value at target receptor with NOx fraction
c ---          Loop over sources
               do ksrc=1,nsrc
                  call MAKENO2(aconcd(ksrc,inox),cno2)
                  concd(ksrc)=concd(ksrc)+cno2
                  aconcd(ksrc,ino2)=aconcd(ksrc,ino2)+cno2
               enddo
            endif
         elseif(LVISIB .AND. LG)then
c ---       Visibility
            if(no2calc.GT.0 .AND. LVNO2) then
               do i=ibgrid,iegrid
                 do j=jbgrid,jegrid
                   call MAKENO2(gnox(i,j),cno2)
                   gno2(i,j)=gno2(i,j)+cno2
                   aconcg(i,j,ino2)=aconcg(i,j,ino2)+cno2
                 enddo
               enddo
            endif
         elseif(LG) then
c ---       NO2 processing
            if(LNO2X) then
c ---          Update NO2 with NOx fraction
               do i=ibgrid,iegrid
                 do j=jbgrid,jegrid
                   call MAKENO2(aconcg(i,j,inox),cno2)
                   concg(i,j)=concg(i,j)+cno2
                   aconcg(i,j,ino2)=aconcg(i,j,ino2)+cno2
                 enddo
               enddo
            endif
         endif
      endif

      if(ndrec .GT. 0) then
         if(msrc.EQ.2 .AND. LDS) then
c ---       Source TRACEBACK
            if(LNO2X) then
c ---          Update NO2 value at target receptor with NOx fraction
c ---          Loop over sources
               do ksrc=1,nsrc
                  call MAKENO2(aconcd(ksrc,inox),cno2)
                  concd(ksrc)=concd(ksrc)+cno2
                  aconcd(ksrc,ino2)=aconcd(ksrc,ino2)+cno2
               enddo
            endif
         elseif(LVISIB .AND. LD)then
c ---       Visibility
            if(no2calc.GT.0 .AND. LVNO2) then
               do i=1,ndrec
                  call MAKENO2(dnox(i),cno2)
                  dno2(i)=dno2(i)+cno2
                  aconcd(i,ino2)=aconcd(i,ino2)+cno2
               enddo
            endif
         elseif(LD) then
c ---       NO2 processing
            if(LNO2X) then
c ---          Update NO2 with NOx fraction
               do i=1,ndrec
                  call MAKENO2(aconcd(i,inox),cno2)
                  concd(i)=concd(i)+cno2
                  aconcd(i,ino2)=aconcd(i,ino2)+cno2
               enddo
            endif
         endif
      endif

      if(LCTSG) then
         if(LVISIB .AND. LCT)then
c ---       Visibility
            if(no2calc.GT.0 .AND. LVNO2) then
               do i=1,nctrec
                  call MAKENO2(tnox(i),cno2)
                  tno2(i)=tno2(i)+cno2
                  aconct(i,ino2)=aconct(i,ino2)+cno2
               enddo
            endif
         elseif(LCT) then
c ---       NO2 processing
            if(LNO2X) then
c ---          Update NO2 with NOx fraction
               do i=1,nctrec
                  call MAKENO2(aconct(i,inox),cno2)
                  conct(i)=conct(i)+cno2
                  aconct(i,ino2)=aconct(i,ino2)+cno2
               enddo
            endif
         endif
      endif

c --- Set target date-time for RH data
      mdathrb=myrb*100000+mjdayb*100+mhrb
      mdathre=myre*100000+mjdaye*100+mhre
      call GRDAY(io1,myrb,mjdayb,imob,idayb)

c
c  Read RH data from VISB.DAT file and match current date-time
      if(LVISIB .AND. .not.LRHFAC) then
11       call RDRELH(iorh,iyrb,ijdayb,ihrb,isecb,iyre,ijdaye,ihre,isece,
     &                  irhss,irh2d,ieof)
         if(ieof.EQ.1) return

         idathrb=iyrb*100000+ijdayb*100+ihrb
         idathre=iyre*100000+ijdaye*100+ihre
         if(idathrb.LT.mdathrb) then
            goto 11
         elseif(idathrb.EQ.mdathrb .AND. isecb.LT.msecb) then
            goto 11
         elseif(idathrb.NE.mdathrb .OR. isecb.NE.msecb) then
            write(io6,*)'GETRCRD: date-time not found in VISB.DAT file'
            write(io1,*)'GETRCRD: date-time not found in VISB.DAT file'
            write(io1,*)'  Beginning date-Time needed: ',mdathrb,msecb
            write(io1,*)'     Ending date-Time needed: ',mdathre,msece
            write(io1,*)'   Beginning date-Time found: ',idathrb,isecb
            write(io1,*)'      Ending date-Time found: ',idathre,isece
            stop
         endif
      endif
c
c  Select dry and RH-augmented background extinction for begin-date
      if(LVISIB .AND. 
     &  (mvisbk.EQ.2 .OR. mvisbk.EQ.3 .OR. mvisbk.EQ.6)) then
         bextdry=bextdry2(imob)
         bexthyg=bexthyg2(imob)
      elseif(LVISIB .AND. mvisbk.EQ.8) then
c ---    Different F(RH) for small, large, and sea salt components
         bextdry=bextdry2(imob)
         bexthygs=bexthygs2(imob)
         bexthygl=bexthygl2(imob)
         bextsalt=bextsalt2(imob)
      elseif(LVISIB .AND. mvisbk.EQ.7) then
c ---    Use observed visual range from weather file (end-hour)
c ---    during natural rain/fog conditions (bvis > 0.0)
         if(itype.ne.3) then
            call GETWX(ldebug,mdathre,bvis,vrkm,wstring)
            if(bvis.GT.0.0) then
               bextdry=bvis
               bexthyg=0.0
            else
               bextdry=bextdry2(imob)
               bexthyg=bexthyg2(imob)
            endif
         else
            call GETWX3(ldebug,mdathre,bvis3,wstring3)
         endif
      elseif(LVSR) then
c ---    Use measured background from VSRN.DAT file (end-hour)
c ---    Dry bext is used since RH is already in measurement
c ---    If RH > 90, bmeas=-1.0, so negative value will be passed
         call GETVSR(mdathre,bmeas)
         bextdry=bmeas
         bexthyg=0.0
c ---    Add Rayleigh extinction to nephelometer measurement
         if(mvisbk.EQ.5) bextdry=bextdry+bextray
      endif

c  Set RH arrays for this hour, and compute background extinction
      if(LVISIB) then

c ---    Gridded receptors
         if(LSGRID) then
c ---       Set spatial increments for receptor grid
            dx=delx/float(meshdn)
            dy=dely/float(meshdn)
            do i=1,ngx
c ---          Compute MAP x-coord of grid point, and met grid index
               xgkm=xorigkm+delx*(isastr-0.5)+(i-1)*dx
               ixmet=INT((xgkm-xorigkm)/delx)+1
c --- cec(060309) mxgx and mxgy change to ielmet and jelmet - the actual size of CALMET grid.
c              if((ixmet.le.0).or.(ixmet.gt.mxgx))stop
               if((ixmet.le.0).or.(ixmet.gt.ielmet))stop
     *           'gridded receptors are off the grid'
            do j=1,ngy
c ---          Compute MAP y-coord of grid point, and met grid index
               ygkm=yorigkm+dely*(jsastr-0.5)+(j-1)*dy
               iymet=INT((ygkm-yorigkm)/dely)+1
               if((iymet.le.0).or.(iymet.gt.jelmet))stop
     *           'gridded receptors are off the grid'

               if(mvisbk.EQ.6) then
c ---             Use monthly F(RH)
                  gfrh(i,j)=rhfac(imob)
               elseif(mvisbk.EQ.8 .AND.
     &               (m8_mode.EQ.4 .OR. m8_mode.EQ.5)) then
c ---             Use monthly F(RH) for each curve
                  gfrhs(i,j)=rhfsml(imob)
                  gfrhl(i,j)=rhflrg(imob)
                  gfrhss(i,j)=rhfsea(imob)
               else
                  if(mvisbk.EQ.8 .AND.
     &              (m8_mode.EQ.2 .OR. m8_mode.EQ.3)) then
c ---                Use monthly relative humidity
                     irh=irhmonth(imob)
                  elseif(i2DRH.eq.1) then
c ---                Use 2D relative humidity if available
c                    (CALMET level>=010901)
                     irh= irh2d(ixmet,iymet)
                  else
c ---                Get nearest surface met station index
                     issta=nears(ixmet,iymet)
c ---                Extract RH
                     call EXRH(issta,xgkm,ygkm,irhss,irh)
                  endif
                  xrh=0.01*float(irh)
                  if(mvisbk.EQ.8) then
c ---                Different F(RH) for small, large, and salt
                     call FRH4(xrh,gfrhs(i,j),gfrhl(i,j),gfrhss(i,j))
                  else
                     gfrh(i,j)=GROWTH(xrh)
                  endif
               endif
c ---          Background extinction
               if(mvisbk.eq.7 .and. itype.eq.3) then
                  IF(bvis3(igrabg(i,j),jgrabg(i,j)).ne.0.0) then
                     bextdry=bvis3(igrabg(i,j),jgrabg(i,j))
                     bexthyg=0.0
                  else
                     bextdry=bextdry2(imob)
                     bexthyg=bexthyg2(imob)
                  endif
               end if
               if(mvisbk.EQ.8) then
                  gbext0(i,j)=bextdry+bexthygs*gfrhs(i,j)
     &                               +bexthygl*gfrhl(i,j)
     &                               +bextsalt*gfrhss(i,j)
               else
                  gbext0(i,j)=bextdry+bexthyg*gfrh(i,j)
               endif
            enddo
            enddo
         endif

c ---    Discrete receptors
         if(ndrec .GT. 0) then
            do i=1,ndrec
c ---          Compute met grid indices
               ixmet=INT((xrec(i)-xorigkm)/delx)+1
               iymet=INT((yrec(i)-yorigkm)/dely)+1
c --- cec(060309) mxgx and mxgy change to ielmet and jelmet - the actual size of CALMET grid.
               IF((ixmet.le.0).OR.(ixmet.gt.ielmet))stop
     *           'discrete receptors are off the grid'
               IF((iymet.le.0).OR.(iymet.gt.jelmet))stop
     *           'discrete receptors are off the grid'

               if(mvisbk.EQ.6) then
c ---             Use monthly F(RH)
                  dfrh(i)=rhfac(imob)
               elseif(mvisbk.EQ.8 .AND.
     &               (m8_mode.EQ.4 .OR. m8_mode.EQ.5)) then
c ---             Use monthly F(RH) for each curve
                  dfrhs(i)=rhfsml(imob)
                  dfrhl(i)=rhflrg(imob)
                  dfrhss(i)=rhfsea(imob)
               else
                  if(mvisbk.EQ.8 .AND.
     &              (m8_mode.EQ.2 .OR. m8_mode.EQ.3)) then
c ---                Use monthly relative humidity
                     irh=irhmonth(imob)
                  elseif(i2DRH.eq.1) then
c ---                Use 2D relative humidity if available
c                    (CALMET level>=010901)
                     irh= irh2d(ixmet,iymet)
                  else
c ---                Get nearest surface met station index
                     issta=nears(ixmet,iymet)
c ---                Extract RH
                     call EXRH(issta,xgkm,ygkm,irhss,irh)
                  endif
                  xrh=0.01*float(irh)
                  if(mvisbk.EQ.8) then
c ---                Different F(RH) for small, large, and salt
                     call FRH4(xrh,dfrhs(i),dfrhl(i),dfrhss(i))
                  else
                     dfrh(i)=GROWTH(xrh)
                  endif
               endif

c ---          Background extinction
               if(mvisbk.eq.7 .and. itype.eq.3) then
                  IF(bvis3(igrabd(i),jgrabd(i)).ne.0.0) then
                     bextdry=bvis3(igrabd(i),jgrabd(i))
                     bexthyg=0.0
                  else
                     bextdry=bextdry2(imob)
                     bexthyg=bexthyg2(imob)
                  endif
               end if
               if(mvisbk.EQ.8) then
                  dbext0(i)=bextdry+bexthygs*dfrhs(i)
     &                             +bexthygl*dfrhl(i)
     &                             +bextsalt*dfrhss(i)
               else
                  dbext0(i)=bextdry+bexthyg*dfrh(i)
               endif
            enddo
         endif

c ---    CTSG receptors
         if(LCTSG) then
            do i=1,nctrec
c ---          Compute met grid indices
               ixmet=INT((xctr(i)-xorigkm)/delx)+1
               iymet=INT((yctr(i)-yorigkm)/dely)+1
c --- cec(060309) mxgx and mxgy change to ielmet and jelmet - the actual size of CALMET grid.
               if((ixmet.le.0).or.(ixmet.gt.ielmet))stop
     *           'complex terrain receptors are off the grid'
               if((iymet.le.0).or.(iymet.gt.jelmet))stop
     *           'complex terrain receptors are off the grid'

               if(mvisbk.EQ.6) then
c ---             Use monthly F(RH)
                  tfrh(i)=rhfac(imob)
               elseif(mvisbk.EQ.8 .AND.
     &               (m8_mode.EQ.4 .OR. m8_mode.EQ.5)) then
c ---             Use monthly F(RH) for each curve
                  tfrhs(i)=rhfsml(imob)
                  tfrhl(i)=rhflrg(imob)
                  tfrhss(i)=rhfsea(imob)
               else
                  if(mvisbk.EQ.8 .AND.
     &              (m8_mode.EQ.2 .OR. m8_mode.EQ.3)) then
c ---                Use monthly relative humidity
                     irh=irhmonth(imob)
                  elseif(i2DRH.eq.1) then
c ---                Use 2D relative humidity if available
c                    (CALMET level>=010901)
                     irh= irh2d(ixmet,iymet)
                  else
c ---                Get nearest surface met station index
                     issta=nears(ixmet,iymet)
c ---                Extract RH
                     call EXRH(issta,xgkm,ygkm,irhss,irh)
                  endif
                  xrh=0.01*float(irh)
                  if(mvisbk.EQ.8) then
c ---                Different F(RH) for small, large, and salt
                     call FRH4(xrh,tfrhs(i),tfrhl(i),tfrhss(i))
                  else
                     tfrh(i)=GROWTH(xrh)
                  endif
               endif
c ---          Background extinction
               if (mvisbk.eq.7 .and. itype.eq.3) then
                  IF(bvis3(igrabc(i),jgrabc(i)).ne.0.0) then
                     bextdry=bvis3(igrabc(i),jgrabc(i))
                     bexthyg=0.0
                  else
                     bextdry=bextdry2(imob)
                     bexthyg=bexthyg2(imob)
                  endif
               end if
               if(mvisbk.EQ.8) then
                  tbext0(i)=bextdry+bexthygs*tfrhs(i)
     &                             +bexthygl*tfrhl(i)
     &                             +bextsalt*tfrhss(i)
               else
                  tbext0(i)=bextdry+bexthyg*tfrh(i)
               endif
            enddo
         endif
      endif

c  Compute and store extinction coefficients for visibility
c ---------------------------------------------------------------
c
      if(LSGRID) then
        if(LVISIB .AND. LG)then
          do i=ibgrid,iegrid
          do j=jbgrid,jegrid
            gec(i,j)=gec(i,j)*fmicro*eeec
            gpmc(i,j)=gpmc(i,j)*fmicro*eepmc
            gpmf(i,j)=gpmf(i,j)*fmicro*eepmf
            gno2(i,j)=gno2(i,j)*fmicro*eeno2
            if(mvisbk.EQ.8) then
               gso4(i,j)=gso4(i,j)*fmicro*xso4
               call IMPRV06(eeso4s,eeso4l,gfrhs(i,j),gfrhl(i,j),
     &                      bkso4(imob),gso4(i,j))
               gno3(i,j)=gno3(i,j)*fmicro*xno3
               call IMPRV06(eeno3s,eeno3l,gfrhs(i,j),gfrhl(i,j),
     &                      bkno3(imob),gno3(i,j))
               goc(i,j)=goc(i,j)*fmicro
               call IMPRV06(eeocs,eeocl,one,one,bkoc(imob),goc(i,j))
            else
               gno3(i,j)=gno3(i,j)*fmicro*eeno3*xno3*gfrh(i,j)
               gso4(i,j)=gso4(i,j)*fmicro*eeso4*xso4*gfrh(i,j)
               goc(i,j)=goc(i,j)*fmicro*eeoc
            endif
          enddo
          enddo
        endif
      endif
c
      if(ndrec .GT. 0) then
        if(LVISIB .AND. LD)then
          do i=1,ndrec
            dec(i)=dec(i)*fmicro*eeec
            dpmc(i)=dpmc(i)*fmicro*eepmc
            dpmf(i)=dpmf(i)*fmicro*eepmf
            dno2(i)=dno2(i)*fmicro*eeno2
            if(mvisbk.EQ.8) then
               dso4(i)=dso4(i)*fmicro*xso4
               call IMPRV06(eeso4s,eeso4l,dfrhs(i),dfrhl(i),
     &                      bkso4(imob),dso4(i))
               dno3(i)=dno3(i)*fmicro*xno3
               call IMPRV06(eeno3s,eeno3l,dfrhs(i),dfrhl(i),
     &                      bkno3(imob),dno3(i))
               doc(i)=doc(i)*fmicro
               call IMPRV06(eeocs,eeocl,one,one,bkoc(imob),doc(i))
            else
               dso4(i)=dso4(i)*fmicro*eeso4*xso4*dfrh(i)
               dno3(i)=dno3(i)*fmicro*eeno3*xno3*dfrh(i)
               doc(i)=doc(i)*fmicro*eeoc
            endif
          enddo
        endif
      endif
c
      if(LCTSG) then
        if(LVISIB .AND. LCT)then
          do i=1,nctrec
            tec(i)=tec(i)*fmicro*eeec
            tpmc(i)=tpmc(i)*fmicro*eepmc
            tpmf(i)=tpmf(i)*fmicro*eepmf
            tno2(i)=tno2(i)*fmicro*eeno2
            if(mvisbk.EQ.8) then
               tso4(i)=tso4(i)*fmicro*xso4
               call IMPRV06(eeso4s,eeso4l,tfrhs(i),tfrhl(i),
     &                      bkso4(imob),tso4(i))
               tno3(i)=tno3(i)*fmicro*xno3
               call IMPRV06(eeno3s,eeno3l,tfrhs(i),tfrhl(i),
     &                      bkno3(imob),tno3(i))
               toc(i)=toc(i)*fmicro
               call IMPRV06(eeocs,eeocl,one,one,bkoc(imob),toc(i))
            else
               tso4(i)=tso4(i)*fmicro*eeso4*xso4*tfrh(i)
               tno3(i)=tno3(i)*fmicro*eeno3*xno3*tfrh(i)
               toc(i)=toc(i)*fmicro*eeoc
            endif
          enddo
        endif
      endif

c
c  Compute the light extinction requested (only those components
c  selected in the control file are non-zero), and store 
      if(LVISIB)then
         if(LSGRID .AND. LG)then
            do i=ibgrid,iegrid
            do j=jbgrid,jegrid
               concg(i,j)=gso4(i,j)+gno3(i,j)+goc(i,j)+gec(i,j)+
     &                    gpmc(i,j)+gpmf(i,j)+gno2(i,j)
               if(LVBK) concg(i,j)=concg(i,j)+gbext0(i,j)
            enddo
            enddo
         endif
         if(ndrec.gt.0 .AND. LD)then
            do i=1,ndrec
               concd(i)=dso4(i)+dno3(i)+doc(i)+dec(i)+dpmc(i)+
     &                  dpmf(i)+dno2(i)
               if(LVBK) concd(i)=concd(i)+dbext0(i)
            enddo
         endif
         if(LCTSG .AND. LCT)then
            do i=1,nctrec
               conct(i)=tso4(i)+tno3(i)+toc(i)+tec(i)+tpmc(i)+
     &                  tpmf(i)+tno2(i)
               if(LVBK) conct(i)=conct(i)+tbext0(i)
            enddo
         endif
      endif

c --- Additional output for detailed analysis of visibility (debug)
      if(LVEXTHR .AND. LVISIB .AND. mvisbk.EQ.7) then
         if(bvis.LT.0) then
            wstring='-                   '
c            wstring='--------------------'
         endif
         if(LSGRID) then
            do i=1,ngx
            do j=1,ngy
               if(ngrecp(i,j).EQ.1) then
                  bmodel=concg(i,j)
                  if(LVBK) bmodel=concg(i,j)-gbext0(i,j)
                  backm2=bextdry2(imob)+bexthyg2(imob)*gfrh(i,j)
                  pcdelm2=100.*bmodel/backm2
                  pcdelm7=100.*bmodel/gbext0(i,j)
                  if (itype.eq.3) then
                  write(iohrv,901)idathre,i,j,bmodel,backm2,gfrh(i,j),
     &                       pcdelm2,wstring3(igrabg(i,j),jgrabg(i,j)),
     &                      vsrkm3(igrabg(i,j),jgrabg(i,j)),gbext0(i,j),
     &                       pcdelm7,igrabg(i,j),jgrabg(i,j)
                  else
                  write(iohrv,900)idathre,i,j,bmodel,backm2,gfrh(i,j),
     &                            pcdelm2,wstring,vrkm,gbext0(i,j),
     &                            pcdelm7
                  endif
               endif
            enddo
            enddo
         endif
         if(ndrec .GT. 0) then
            j=0
            do i=1,ndrec
               if(ndrecp(i).EQ.1) then
                  bmodel=concd(i)
                  if(LVBK) bmodel=concd(i)-dbext0(i)
                  backm2=bextdry2(imob)+bexthyg2(imob)*dfrh(i)
                  pcdelm2=100.*bmodel/backm2
                  pcdelm7=100.*bmodel/dbext0(i)
                  if (itype.eq.3) then
                  write(iohrv,901)idathre,i,j,bmodel,backm2,dfrh(i),
     &                       pcdelm2,wstring3(igrabd(i),jgrabd(i)),
     &                       vsrkm3(igrabd(i),jgrabd(i)),dbext0(i),
     &                       pcdelm7,igrabd(i),jgrabd(i)
                  else
                  write(iohrv,900)idathre,i,j,bmodel,backm2,dfrh(i),
     &                            pcdelm2,wstring,vrkm,dbext0(i),
     &                            pcdelm7
                  endif
               endif
            enddo
         endif
900      format(i10,2i5,2f10.3,2f7.2,2x,a20,f8.3,f10.3,f7.2)
901      format(i10,2i5,2f10.3,2f7.2,2x,a20,f8.3,f10.3,f7.2,2i4)
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine rdconc(io,isrc,ktype0,knum0,ispl,
     &                  tcg,tcd,tct,ix,iy,idrec,itrec,ieof,tsplv)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017            RDCONC
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Reads header and one block of data records from
c               "concentration" file for one species-level
c
c  ARGUMENTS:
c     PASSED:   io        Unit number for accessing input file       [i]
c               isrc      Current source index                       [i]
c               ktype0    Last source-type index                     [i]
c               knum0     Last source-number of type ktype0          [i]
c               ispl      Current species-level                      [i]
c               tcg(i,j)  Temporary gridded receptor array          [ra]
c               tcd(i)    Temporary discrete receptor array         [ra]
c               tct(i)    Temporary complex terrain receptor array  [ra]
c               ix,iy     Dimensions for tcg array                   [i]
c               idrec     Dimension for tcd array                    [i]
c               itrec     Dimension for tct array                    [i]
c   RETURNED:   ktype0    Last source-type index                     [i]
c               knum0     Last source-number of type ktype0          [i]
c               tcg(i,j)  Temporary gridded receptor array          [ra]
c               tcd(i)    Temporary discrete receptor array         [ra]
c               tct(i)    Temporary complex terrain receptor array  [ra]
c               ieof      End of file flag (1=EOF encountered)       [i]
c               tsplv     Current species-level name                 [c]
c
c  CALLING ROUTINES:    GETRCRD
c
c  EXTERNAL ROUTINES:   INCR, YR4, UNCOMPRS
c-----------------------------------------------------------------------
c
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'source.pst'

c --- Local DEBUG
      logical ldb

c --- Declare temporary variables using actual array dimensions
      character*16 cnamsrc
      character*15 tsplv
      real tcg(ix,iy),tcd(idrec),tct(itrec)
      real xwork1(mxgx,mxgy),xwork2(mxdrec),xwork3(mxctrec)

      ieof=0

      ldb=.FALSE.

c --- One set of data records contains header records and then a block
c --- of records for each species-level.  This subroutine reads one such
c --- block of records.

c --- Read header only for the first species-level
      if(ispl.EQ.1) then

c ---    Read time record from "concentration" file
c ---    Note that time uses the 00-23 hour convention so that the 24th
c ---    hour of day 12 has a start time of 23 on day 12 and an end time
c ---    of 00 on day 13

         if(iptime.EQ.1) then
            read(io,end=999) myre,mjdaye,mhre
            msece=0
            msecb=0
            myrb=myre
            mjdayb=mjdaye
            mhrb=mhre
            call INCR(io1,myrb,mjdayb,mhrb,-1)
         elseif(iptime.EQ.2) then
            read(io,end=999) myrb,mjdayb,mhrb,msecb,
     &                       myre,mjdaye,mhre,msece
         else
            write(io1,*)
            write(io1,*)'   Invalid time flag IPTIME: ',iptime
            write(io1,*)'       Expected either 1 or 2'
            write(io1,*)
            stop 'Halted in RDCONC --- see list file'
         endif

c ---    Process date-time information
c ---    Enforce YYYY format for year
         call YR4(io1,myrb,ierrb)
         call YR4(io1,myre,ierre)
         if(ierrb.NE.0 .OR. ierre.NE.0) stop 'Halted in RDCONC'

c ---    DEBUG output
         if(LDB) then
            write(io6,*)'RDCONC: myrb,mjdayb,mhrb,msecb = ',
     &                           myrb,mjdayb,mhrb,msecb 
            write(io6,*)'RDCONC: myre,mjdaye,mhre,msece = ',
     &                           myre,mjdaye,mhre,msece 
         endif

c ---    Read source information record from "concentration" file
         if(isrcinfo.EQ.1) then
            read(io,end=999) ktype,knum,cnamsrc,xskm,yskm
c ---       Total for all sources has ktype=0
            if(ktype.EQ.0) then
               ksrc=0
            else
               ksrc=isrc
            endif
c ---       Check for expected source
            ifatal=0
            if(cnamsrc.NE.csource(ksrc)) ifatal=1
            if(ktype.EQ.ktype0) then
               if(knum.NE.knum0+1) ifatal=1
            elseif(ktype.GT.ktype0 .OR. ktype.EQ.0) then
               if(knum.NE.1) ifatal=1
            else
               ifatal=1
            endif
            if(ifatal.EQ.1) then
               write(io1,*)
               write(io1,*)'This source type and number: ',ktype,knum
               write(io1,*)'Last source type and number: ',ktype0,knum0
               write(io1,*)'ERROR in RDCONC - sources are not in order'
               write(io1,*)'   Sequential source number: ',isrc
               write(io1,*)'       Expected source name: ',csource(ksrc)
               write(io1,*)'          Found source name: ',cnamsrc
               write(io1,*)
               stop 'Halted in RDCONC --- see list file'
            endif
c ---       Pass location to /SOURCE/ arrays
            xsrckm(ksrc)=xskm
            ysrckm(ksrc)=yskm
c ---       Update last source type and number
            ktype0=ktype
            knum0=knum
         endif
      endif

c --- Read "concentration" data for one species-level
c
      if(LSGRID) then
         if(lcomprs)then
            nwords=ix*iy
            read(io)ii
            call UNCOMPRS(xwork1,ii,io,nwords,tsplv,tcg)
         else
            read(io) tsplv,tcg
         endif
      endif
c
      if(ndrec .GT. 0) then
         if(lcomprs)then
            read(io)ii
            call UNCOMPRS(xwork2,ii,io,idrec,tsplv,tcd)
         else
            read(io) tsplv,tcd
         endif
      endif
c
      if(LCTSG) then
         if(lcomprs)then
            read(io)ii
            call UNCOMPRS(xwork3,ii,io,itrec,tsplv,tct)
         else
            read(io) tsplv,tct
         endif
      endif

      return
c
c  EOF encountered in header record
999   ieof=1
      return
c
      end
c-----------------------------------------------------------------------
      subroutine rdrelh(io,iyrb,ijdayb,ihrb,isecb,
     &                  iyre,ijdaye,ihre,isece,
     &                  irhss,irh2d,ieof)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017            RDRELH
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Reads data record groups from
c               "visibility" file for relative humidity
c
c  ARGUMENTS:
c     PASSED:   io        Unit number for accessing input file       [i]
c   RETURNED:   iyrb      Beginning year                             [i]
c               ijdayb    Beginning Julian day                       [i]
c               ihrb      Beginning hour (00-23)                     [i]
c               isecb     Beginning second (0000-3599)               [i]
c               iyre      Ending year                                [i]
c               ijdaye    Ending Julian day                          [i]
c               ihre      Ending hour (00-23)                        [i]
c               isece     Ending second (0000-3599)                  [i]
c               irh2d(i,j)Relative humidity array (2D grid)         [ia]
c               irhss(i)  Relative humidity array (surface stations)[ia]
c               ieof      End of file flag (1=EOF encountered)       [i]
c
c  CALLING ROUTINES:    GETRCRD
c
c  EXTERNAL ROUTINES:   INCR, YR4
c-----------------------------------------------------------------------
c
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'

c --- Declare temporary variables using actual array dimensions
      character*15 tsplv
      integer irhss(mxss)
c frr (09/01) 2D array of relative humidity (dimensioned as met grid)
      integer irh2d(mxgx,mxgy)

      ieof=0

c --- Read time record from relative humidity file
c --- Note that time uses the 00-23 hour convention so that the 24th
c --- hour of day 12 has a start time of 23 on day 12 and an end time
c --- of 00 on day 13

      if(irhtime.EQ.1) then
         read(io,end=999) iyre,ijdaye,ihre
         isece=0
         isecb=0
         iyrb=iyre
         ijdayb=ijdaye
         ihrb=ihre
         call INCR(io1,iyrb,ijdayb,ihrb,-1)
      elseif(irhtime.EQ.2) then
         read(io,end=999) iyrb,ijdayb,ihrb,isecb,
     &                    iyre,ijdaye,ihre,isece
      else
         write(io1,*)
         write(io1,*)'   Invalid time flag IRHTIME: ',irhtime
         write(io1,*)'       Expected either 1 or 2'
         write(io1,*)
         stop 'Halted in RDRELH --- see list file'
      endif

c --- Enforce YYYY format for year
      call YR4(io1,iyrb,ierrb)
      call YR4(io1,iyre,ierre)
      if(ierrb.NE.0 .OR. ierre.NE.0) stop 'Halted in RDRELH'

c --- Read relative humidity data
c frr (09/01) read in 2D array of relative humidity if available

      if (i2drh.eq.1) then
c frr bug fix (102602)
c         read(io) tsplv,((irh2d(i,j),j=1,jelmet),i=1,ielmet)
         read(io) tsplv,((irh2d(i,j),i=1,ielmet),j=1,jelmet)
      else
         read(io) tsplv,(irhss(is),is=1,nssta)
      endif

      return
c
c  EOF encountered in header record
999   ieof=1
      return
c
      end
c-----------------------------------------------------------------------
      subroutine getvsr(ndathr,bmeas)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 990709            GETVSR
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Reads hourly data records from "concentration" file
c               and passes information to main program through
c               common /conc/.
c
c  UPDATES:
c  V5.0(980918) to V5.1(990709)
c               (DGS) Use full 4-digit year
c
c  ARGUMENTS:
c     PASSED:  ndathr     Current year, Julian day, hour (ending)    [i]
c                         needed in YYYYJJJHH format
c              bmeas      Measured extinction (Mm-1)                 [i]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
c
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'visb.pst'

c --- Set missing value for returned Bmeas
      data bmiss/-99.9/
c --- Set high-RH value for returned Bmeas
      data bdrop/-99.9/

c --- Read record from VSRN.DAT file and match to time
10    continue
      if(LNEPHEL) then
         read(in5,101) iyr,ijday,ihr,meas,ivflag,rh
c ---    Set RH to integer, retaining information near 90%
         irh=NINT(rh)
         if(irh.EQ.90 .AND. rh.GT.90.) irh=91
      else
         read(in5,102) iyr,ijday,ihr,meas,ivflag,irh
      endif

      idathr=iyr*100000+ijday*100+ihr
      if(idathr.LT.ndathr) then
         goto 10
      elseif(idathr.GT.ndathr) then
         write(io6,*)'GETVSR: date-time not found in VSRN.DAT file'
         write(io1,*)'GETVSR: date-time not found in VSRN.DAT file'
         write(io1,*)'        Date-Time needed: ',ndathr
         write(io1,*)'         Date-Time found: ',idathr
         stop
      endif

c --- Check validity
      if(irh.GT.90) then
         bmeas=bdrop
      elseif(ivflag.NE.0) then
         bmeas=bmiss
      elseif(meas.LT.0) then
         bmeas=bmiss
      else
         bmeas=FLOAT(meas)
      endif    

c --- Debug output
c     if(LDEBUG) then
c        write(io1,*)'GETVSR:  meas,ivflag,irh = ',meas,ivflag,irh
c        write(io1,*)'                   bmeas = ',bmeas
c     endif

      return
101   format(5x,i4,5x,i3,1x,i2,8x,i6,8x,i2,40x,40x,40x,11x,f7.2)
102   format(5x,i4,5x,i3,1x,i2,2x,i5,22x,i2,17x,i2)

      end
c----------------------------------------------------------------------
      function growth(rh)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 041130            GROWTH
c               S. Musarra, STI
c
c --- PURPOSE:  Compute the increase in particle size and light
c               extinction due to condensation of water vapor
c
c  UPDATES:
c  V5.51(030709) to 5.632(041130)
c               (DGS) Create flag for choice of f(RH) curve, and add
c                     new table from EPA (2003) [Appendix A in report
c                     EPA-454/B-03-004 or EPA-454/B-03-005]
c  V5.5(030627) to V5.51(030709)
c               (DGS) Impose RH=1% as lower limit
c  V5.2(991104) to V5.5(030627)
c               (JSS) Add MVISBK=7 option
c  V5.1(990920) to V5.2(991104)
c               (JSS) Error messages written to list file
c                     in addition to screen
c  V5.0(981015) to V5.0(981228)
c               (DGS) Use new table also for MVISBK=5
c  V5.0(980918) to V5.0(981015)
c               (DGS) Use new table also for MVISBK=4, and allow
c                     actual values for RH>RHMAX to return
c  V5.0(980821) to V5.0(980918)
c               (DGS) Use new table also for MVISBK=3
c  V5.0(941102) to V5.0(980821)
c               (DGS) Add new table for use when MVISBK=2
c                     IMPROVE report, July 1996
c-----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'
c
      include 'visb.pst'

c --- MFRH = 1
c ------------
c --- IWAQM (1998) functional form of growth by: Naresh Kumar, STI
c     growth = 1                      rh <= 0.3
c     growth = 0.7 / (1-rh)           0.3 < rh <= 0.8
c     growth = 0.8064/(1.0304-rh)     0.8 < rh <= rhmax
c     growth = 0.8064/(1.0304-rhmax)  rh >= rhmax

c --- MFRH = 2
c ------------
c --- Tabulation from FLAG report, 2000
      real frhtab1(100)
      data frhtab1/1.,1.,1.,1.,1.,
     &             1.,1.,1.,1.,1.,
     1             1.,1.,1.,1.0001,1.0001,
     &             1.0004,1.0006,1.0024,1.0056,1.0089, 
     2             1.0097,1.0105,1.0111,1.0115,1.0118,
     &             1.0122,1.0126,1.0130,1.0135,1.0139, 
     3             1.0173,1.0206,1.0254,1.0315,1.0377,
     &             1.0486,1.0596,1.0751,1.0951,1.1151, 
     4             1.1247,1.1343,1.1436,1.1525,1.1615,
     &             1.1724,1.1833,1.1955,1.2090,1.2224, 
     5             1.2368,1.2512,1.2671,1.2844,1.3018,
     &             1.3234,1.3450,1.3695,1.3969,1.4243, 
     6             1.4628,1.5014,1.5468,1.5992,1.6516,
     &             1.6991,1.7466,1.7985,1.8549,1.9113, 
     7             1.9596,2.0080,2.0596,2.1146,2.1695,
     &             2.2630,2.3565,2.4692,2.6011,2.7330, 
     8             2.8461,2.9592,3.0853,3.2245,3.3637,
     &             3.5743,3.7849,4.0466,4.3594,4.6721, 
     9             5.3067,5.9412,6.9627,8.3710,9.7793,
     &             12.4288,15.0773,18.0590,21.3709,22. /

c --- MFRH = 3
c ------------
c --- Tabulation from EPA(2003) report EPA-454/B-03-004 (or 005)
      real frhtab2(100)
      data frhtab2/1.,1.,1.,1.,1.,
     &             1.,1.,1.,1.,1.,
     1             1.,1.,1.,1.,1.,
     &             1.,1.,1.,1.,1.,
     2             1.,1.,1.,1.,1.,
     &             1.,1.,1.,1.,1.,
     3             1.,1.,1.,1.,1.,
     &             1.00,1.02,1.04,1.06,1.08, 
     4             1.10,1.13,1.15,1.18,1.20,
     &             1.23,1.26,1.28,1.31,1.34, 
     5             1.37,1.41,1.44,1.47,1.51,
     &             1.54,1.58,1.62,1.66,1.70, 
     6             1.74,1.79,1.83,1.88,1.93,
     &             1.98,2.03,2.08,2.14,2.19, 
     7             2.25,2.31,2.37,2.43,2.50,
     &             2.56,2.63,2.70,2.78,2.86, 
     8             2.94,3.03,3.12,3.22,3.33,
     &             3.45,3.58,3.74,3.93,4.16, 
     9             4.45,4.84,5.37,6.16,7.40,
     &             9.59,14.1,26.4,26.4,26.4 /


c --- Set upper end depending on visibility method
      if(mvisbk.EQ.1) then
c ---    Cap RH at RHMAX
         xrh=AMIN1(rh,rhmax)
      elseif(mvisbk.EQ.2 .OR. mvisbk.EQ.7) then
c ---    Cap RH at RHMAX
         xrh=AMIN1(rh,rhmax)
      elseif(mvisbk.GE.3 .AND. mvisbk.LE.5) then
c ---    Cap RH at 100%
         xrh=AMIN1(rh,1.0)
      else
c ---    FATAL
         goto 100
      endif

c --- Obtain f(RH) from selected curve
      if(mfrh.EQ.1) then
c ---    Compute from curve
         if(xrh.le.0.3)then
            growth = 1.0
         elseif(xrh.le.0.8) then
            growth = 0.7/(1.-xrh)
         else
            growth = 0.8064/(1.0304-xrh)
         endif
      elseif(mfrh.GE.2 .AND. mfrh.LE.3) then
c ---    Convert from fraction to integer percent
         irh100=NINT(100.*xrh)
         irh100=MAX(1,irh100)
c ---    Extract from tabulation
         if(mfrh.EQ.2) growth=frhtab1(irh100)
         if(mfrh.EQ.3) growth=frhtab2(irh100)
      else
c ---    FATAL
         goto 110
      endif

      return

100   write(io6,*)'GROWTH:   Invalid MVISBK -- ',mvisbk
      write(*,*)'GROWTH:   Invalid MVISBK -- ',mvisbk
      stop

110   write(io6,*)'GROWTH:   Invalid MFRH -- ',mfrh
      write(*,*)'GROWTH:   Invalid MFRH -- ',mfrh
      stop

      end
c----------------------------------------------------------------------
      subroutine frh4(rh,fs,fl,fss)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 061107            FRH4
c               D. Strimaitis
c
c --- PURPOSE:  Compute the hygroscopic growth factor f(RH) using
c               tabulation option 4 (2006 IMPROVE method).
c
c --- INPUTS:
c               RH - real    - Relative humidity FRACTION (0.0 to 1.0)
c
c --- OUTPUT:
c               FS - real    - F(RH) factor for "small" conc component
c               FL - real    - F(RH) factor for "large" conc component
c              FSS - real    - F(RH) factor for sea salt
c-----------------------------------------------------------------------
c --- Include parameters
      include 'params.pst'

      include 'visb.pst'

c --- F(RH) for SMALL concentration range
c ---------------------------------------
      real frhtabs(100)
      data frhtabs/1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     1             1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     2             1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     3             1.00,1.00,1.00,1.00,1.00,1.00,1.38,1.40,1.42,1.44,
     4             1.46,1.48,1.49,1.51,1.53,1.55,1.57,1.59,1.62,1.64,
     5             1.66,1.68,1.71,1.73,1.76,1.78,1.81,1.83,1.86,1.89,
     6             1.92,1.95,1.99,2.02,2.06,2.09,2.13,2.17,2.22,2.26,
     7             2.31,2.36,2.41,2.47,2.54,2.60,2.67,2.75,2.84,2.93,
     8             3.03,3.15,3.27,3.42,3.58,3.76,3.98,4.23,4.53,4.90,
     9             5.35,5.93,6.71,7.78,9.34,9.34,9.34,9.34,9.34,9.34/

c --- F(RH) for LARGE concentration range
c ---------------------------------------
      real frhtabl(100)
      data frhtabl/1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     1             1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     2             1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     3             1.00,1.00,1.00,1.00,1.00,1.00,1.31,1.32,1.34,1.35,
     4             1.36,1.38,1.39,1.41,1.42,1.44,1.45,1.47,1.49,1.50,
     5             1.52,1.54,1.55,1.57,1.59,1.61,1.63,1.65,1.67,1.69,
     6             1.71,1.73,1.75,1.78,1.80,1.83,1.86,1.89,1.92,1.95,
     7             1.98,2.01,2.05,2.09,2.13,2.18,2.22,2.27,2.33,2.39,
     8             2.45,2.52,2.60,2.69,2.79,2.90,3.02,3.16,3.33,3.53,
     9             3.77,4.06,4.43,4.92,5.57,5.57,5.57,5.57,5.57,5.57/



c --- F(RH) for SEA SALT
c ----------------------
      real frhtabss(100)
      data frhtabss/1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     &              1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     1              1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     &              1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     2              1.00,1.00,1.00,1.00,1.00,1.00,2.36,2.38,2.42,2.45,
     &              2.48,2.50,2.51,2.53,2.56,2.58,2.59,2.62,2.66,2.69,
     3              2.73,2.78,2.83,2.83,2.86,2.89,2.91,2.95,3.01,3.05,
     &              3.13,3.17,3.21,3.25,3.27,3.35,3.42,3.52,3.57,3.63,
     4              3.69,3.81,3.95,4.04,4.11,4.28,4.49,4.61,4.86,5.12,
     &              5.38,5.75,6.17,6.72,7.35,7.35,7.35,7.35,7.35,7.35/


c --- Cap RH at RHMAX (up to 100%)
      xrh=AMIN1(rh,1.0)
      xrh=AMIN1(rh,rhmax)

c --- Convert from fraction to integer percent
      irh100=NINT(100.*xrh)
      irh100=MAX(1,irh100)

c --- Obtain f(RH) for each table
      fs=frhtabs(irh100)
      fl=frhtabl(irh100)
      fss=frhtabss(irh100)

      return
      end
c----------------------------------------------------------------------
      subroutine exrh(ista,xs,ys,irhss,irh)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 941101              EXRH
c                J. Scire, D. Strimaitis,  SRC
c
c --- PURPOSE:  Find the closest non-missing value of relative humidity
c               to a particular point in the met. grid
c
c --- INPUTS:
c             ISTA - integer - Met station index nearest met. grid point
c               XS - real    - X UTM (km) coord of closest  grid point
c               YS - real    - Y UTM (km) coord of closest  grid point
c            IRHSS - integer - Array of RH values reported at surface
c                              met stations
c
c --- OUTPUT:
c              IRH - integer - Relative humidity (percent)
c
c --- EXRH  called by:  GETRCRD
c --- EXRH  calls:      FINDR, FINDI
c----------------------------------------------------------------------
c
      include 'params.pst'
c
      include 'head.pst'
      integer irhss(mxss)
c
c --- Missing value indicator for integer variables
      data imiss/9999/
c
c --- RELATIVE HUMIDITY (percent)
      irh=irhss(ista)
      if(irh.ge.imiss)then
         call findi(xkmsta,ykmsta,nssta,irhss,xs,ys,
     1    ista,irh,ierr)
c
         if(ierr.eq.1)then
            write(io1,*)'ERROR in subr. EXRH -- All surface ',
     1      'relative humidity data missing -- IERR = ',ierr
            write(io1,*)'IRHSS = ',(irhss(n),n=1,nssta)
            stop
         endif
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine findi(xsta,ysta,nsta,idat,x,y,ista,ivalue,ierr)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 900228             FINDI
c                J. Scire, SRC
c
c --- PURPOSE:  Find the closest station to a specified (X,Y)
c               coordinate that has non-missing data and pass
c               the integer variable at that station back to the
c               calling routine
c
c --- INPUTS:
c       XSTA(nsta) - real    - Array of X coordinates (km) for each
c                              station (UTM)
c       YSTA(nsta) - real    - Array of Y coordinates (km) for each
c                              station (UTM)
c             NSTA - integer - Number of stations
c       IDAT(nsta) - integer - Values of the variable at each station
c                              (NOTE: 9999 used as a missing value
c                              indicator)
c                X - real    - Reference X coordinate (km)  (UTM)
c                Y - real    - Reference Y coordinate (km)  (UTM)
c
c --- OUTPUT:
c             ISTA - integer - Station number of closest station
c           IVALUE - real    - Value of from IDAT array for closest
c                              station "ISTA"
c             IERR - integer - Error code (0=no error, 1=all data
c                              missing, so VALUE = missing value
c                              indicator
c
c --- FINDI called by:  EXRH
c --- FINDI calls:      none
c----------------------------------------------------------------------
c
      real xsta(nsta),ysta(nsta)
      integer idat(nsta)
      data xmax/1.e38/,imiss/9999/
c
      ista=0
      ivalue=imiss
c
      dmin2=xmax
c
      do 10 i=1,nsta
c
c --- Compute the (distance)**2 to each station with non-missing data
      if(idat(i).lt.imiss)then
         dist2=(xsta(i)-x)**2+(ysta(i)-y)**2
c
c ---    Keep track of the closest station to the reference point
         if(dist2.lt.dmin2)then
            dmin2=dist2
            ista=i
         endif
      endif
10    continue
c
      if(ista.eq.0)then
         ierr=1
         go to 999
      endif
c
c --- Extract the value at closest station from the data array
      ivalue=idat(ista)
      ierr=0
c
999   continue
      return
      end
c-----------------------------------------------------------------------
      subroutine scale
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017             SCALE
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Applies scaling constants to all concentration data.
c               Data are changed directly in common /conc/.
c
c  UPDATES:
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.0(981025) to V5.1(990920)
c               (DGS) Remove NDRECP filter; add LG,LD,LCT filter
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c  V5.0(890515) to V5.0(980821)
c               (DGS) Process sub-range of discrete receptors
c
c  ARGUMENTS:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'

c  Scale sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            concd(i)=concd(i)*a+b
         enddo
         return
      endif
c
c  Scale gridded receptors
      if(LSGRID .AND. LG) then
         do 5 i=ibgrid,iegrid
         do 5 j=jbgrid,jegrid
5           concg(i,j)=concg(i,j)*a+b
      endif
c
c  Scale discrete receptors
      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
            concd(i)=concd(i)*a+b
         enddo
      endif
c
c  Scale complex terrain receptors
      if(nctrec .GT. 0 .AND. LCT) then
         do 7 i=1,nctrec
7           conct(i)=conct(i)*a+b
      endif
c
      return
      end

c-----------------------------------------------------------------------
      subroutine back(confact)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017              BACK
c ---           A. Klausmann and J. Scire, SRC
c
c  PURPOSE:     Adds monitored hourly background concentrations read
c               from an external file to all concentration data.
c               Data are changed directly in common /conc/.
c
c  UPDATES:
c  V5.4(030402) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990920) to V5.4(030402)
c               (DGS) add list-file unit to INCR, YR4
c  V5.1(990709) to V5.1(990920)
c               (DGS) Update date-time formats
c               (DGS) Remove NDRECP filter; add LG,LD,LCT filter
c  V5.0(981025) to V5.1(990709)
c               (DGS) Update date-time formats
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c  V5.0(980430) to V5.0(980821)
c               (DGS) Process sub-range of discrete receptors
c
c  ARGUMENTS:
c     PASSED: confact           factor to convert background data   [r]
c                               to g/m**3
c     /CONC/: myre,mjdaye,mhre  date & hour currently processed     [i]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   INCR, YR4
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'

c --- MDATE:model date-hr (end-time)
      mdate=myre*10000+mjdaye*100+mhre

115   read(in4,*,end=999) nyr,njday,nhr,xmhbu
      call YR4(io1,nyr,ierr)
      if(ierr.NE.0) stop 'Halted in BACK'
      if(nhr.eq.24) then
         nhr=nhr-1
         iadd=1
         call incr(io1,nyr,njday,nhr,iadd)
      endif
c --- IDATE:background data file date
      idate=nyr*10000+njday*100+nhr
      if(idate.lt.mdate) then
         goto 115
      elseif(idate.gt.mdate) then
         write(io6,405) idate,mdate
405      format(1x, 'error in subroutine BACK --'/
     1      'date/hour read is beyond current date/hour'/
     2      1x,'date/hour read (mdate)  = ',I8/
     3      1x,'current date/hour read (idate)  = ',I8)
         stop
      endif

c --- Scale current background value from user units to internal CALPUFF
c --- units (g/m**3 for concentrations)
      xmhb=xmhbu*confact

c --- Add to  sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            concd(i)=concd(i)+xmhb
         enddo
         return
      endif

c --- Add to gridded receptors
      if(LSGRID .AND. LG) then
         do i=ibgrid,iegrid
            do j=jbgrid,jegrid
               concg(i,j)=concg(i,j)+xmhb
            enddo
         enddo
      endif
c
c --- Add to discrete receptors
      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
            concd(i)=concd(i)+xmhb
         enddo
      endif
c
c --- Add to complex terrain receptors
      if(nctrec .GT. 0 .AND. LCT) then
         do i=1,nctrec
            conct(i)=conct(i)+xmhb
         enddo
      endif

      return

999   continue
      write(io6,406) mdate
406   format(1x,'End of file encountered in background data'/
     1 1x,'Number of hours in backround file does not'/
     2 1x,'Current date and hour (idate)=',2x,i9)
      stop
      end
c-----------------------------------------------------------------------
      subroutine sumconc(avcg,avcd,avct,avbg,avbd,avbt,avhg,avhd,avht,
     *     avpg,avpd,avpt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050915           SUMCONC
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Sums current concentrations to appropriate averaging
c               array.
c
c  UPDATES:
c  V5.633(041202) to V6.1(050915)
c               (DGS) Fix typo in argument list -- avpc is avpt
c  V5.6(031017) to V5.633(041202)
c               (CEC) Add computation of sum of
c                 (source extinction / background extinction) for Visibility
c                  called avpg, avpd, avpt
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.0(981025) to V5.1(990920)
c               (DGS) Remove NDRECP filter; add LG,LD,LCT filter
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c  V5.0(890515) to V5.0(980821)
c               (DGS) Process sub-range of discrete receptors
c
c  ARGUMENTS:
c     PASSED:   avc[g,d,t,p]      Updated summing arrays              [ra]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
      INCLUDE 'visb.pst'
c
      real avcg(mxgx,mxgy),avcd(mxdrec),avct(mxctrec)
      real avbg(mxgx,mxgy),avbd(mxdrec),avbt(mxctrec)
      real avhg(mxgx,mxgy),avhd(mxdrec),avht(mxctrec)
      real avpg(mxgx,mxgy),avpd(mxdrec),avpt(mxctrec)
c
c  Sum sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            avcd(i)=avcd(i)+concd(i)
         enddo
         return
      endif
c
c  Sum gridded receptor concentrations
      if(LSGRID .AND. LG) then
         do 5 i=ibgrid,iegrid
         do 5 j=jbgrid,jegrid
5           avcg(i,j)=avcg(i,j)+concg(i,j)
      endif
c
c  Sum discrete receptor concentrations
      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
            avcd(i)=avcd(i)+concd(i)
         enddo
      endif
c
c  Sum complex terrain receptor concentrations
      if(nctrec .GT. 0 .AND. LCT) then
         do 7 i=1,nctrec
7           avct(i)=avct(i)+conct(i)
      endif
c
      if(LVISIB)then
c  Sum additional visibility-related variables
        if(LSGRID .AND. LG)then
          do i=ibgrid,iegrid
            do j=jbgrid,jegrid
               avbg(i,j)=avbg(i,j)+gbext0(i,j)
               avhg(i,j)=avhg(i,j)+gfrh(i,j)
               if (gbext0(i,j).ne.0.) then
                if (LVBK) then
                avpg(i,j)=avpg(i,j)+(concg(i,j)-gbext0(i,j))/gbext0(i,j)
                else
                avpg(i,j)=avpg(i,j)+concg(i,j)/gbext0(i,j)
                endif
               else
               STOP ' background extinction is = 0.0'
               end if
            enddo
          enddo
        endif
        if(ndrec.gt.0 .AND. LD)then
          do i=1,ndrec
             avbd(i)=avbd(i)+dbext0(i)
             avhd(i)=avhd(i)+dfrh(i)
             if (dbext0(i).ne.0.) then
                 if (LVBK) then
                 avpd(i)=avpd(i)+(concd(i)-dbext0(i))/dbext0(i)
                 else
                 avpd(i)=avpd(i)+concd(i)/dbext0(i)
                 endif
             else
             STOP ' background extinction is = 0.0'
             end if
          enddo
        endif
        if(nctrec.gt.0 .AND. LCT)then
          do i=1,nctrec
             avbt(i)=avbt(i)+tbext0(i)
             avht(i)=avht(i)+tfrh(i)
             if (tbext0(i).ne.0.) then
                 if (LVBK) then
                 avpt(i)=avpt(i)+(conct(i)-tbext0(i))/tbext0(i)
                 else
                 avpt(i)=avpt(i)+conct(i)/tbext0(i)
                 endif
             else
             STOP ' background extinction is = 0.0'
             end if
          enddo
        endif
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine sumaconc(avcg,avcd,avct)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 061120          SUMACONC
c ---           D. Strimaitis
c
c  PURPOSE:     Sums current concentrations to appropriate averaging
c               array (all species version)
c
c  ARGUMENTS:
c     PASSED:   avc[g,d,t]      Updated summing arrays              [ra]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
      INCLUDE 'specout.pst'

      real avcg(mxgx,mxgy,mxspec),avcd(mxdrec,mxspec)
      real avct(mxctrec,mxspec)

c --- Sum sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do k=1,nospec
            do i=1,nsrc
               avcd(i,k)=avcd(i,k)+aconcd(i,k)
            enddo
         enddo
         return
      endif

c --- Sum gridded receptor concentrations
      if(LSGRID .AND. LG) then
         do k=1,nospec
            do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               avcg(i,j,k)=avcg(i,j,k)+aconcg(i,j,k)
            enddo
            enddo
         enddo
      endif

c --- Sum discrete receptor concentrations
      if(ndrec .GT. 0 .AND. LD) then
         do k=1,nospec
            do i=1,ndrec
               avcd(i,k)=avcd(i,k)+aconcd(i,k)
            enddo
         enddo
      endif

c --- Sum complex terrain receptor concentrations
      if(nctrec .GT. 0 .AND. LCT) then
         do k=1,nospec
            do i=1,nctrec
               avct(i,k)=avct(i,k)+aconct(i,k)
            enddo
         enddo
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine avgconc(n,avcg,avcd,avct,avbg,avbd,avbt,avhg,avhd,avht
     *   ,avpg,avpd,avpt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 041202           AVGCONC
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Divides summed concentration arrays by number of
c               averaging periods to obtain average concentrations
c
c  UPDATES:
c  V5.6(031017) to V5.633(041202)
c               (CEC) Add divides ratio (source extinction / background extinction)
c               by number of averaging periods.
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.0(981025) to V5.1(990920)
c               (DGS) Remove NDRECP filter; add LG,LD,LCT filter
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c  V5.0(890515) to V5.0(980821)
c               (DGS) Process sub-range of discrete receptors
c
c  ARGUMENTS:
c     PASSED:   n               Number of periods in sum             [i]
c   RETURNED:   avc[g,d,t,p]      Averaged concentration arrays       [ra]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'source.pst'
c
      real avcg(mxgx,mxgy),avcd(mxdrec),avct(mxctrec)
      real avbg(mxgx,mxgy),avbd(mxdrec),avbt(mxctrec)
      real avhg(mxgx,mxgy),avhd(mxdrec),avht(mxctrec)
      real avpg(mxgx,mxgy),avpd(mxdrec),avpt(mxctrec)
c
c  Average sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            avcd(i)=avcd(i)/n
         enddo
         return
      endif
c
c  Average gridded receptor concentrations
      if(LSGRID .AND. LG) then
         do 5 i=ibgrid,iegrid
         do 5 j=jbgrid,jegrid
5           avcg(i,j)=avcg(i,j)/n
      endif
c
c  Average discrete receptor concentrations
      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
            avcd(i)=avcd(i)/n
         enddo
      endif
c
c  Average complex terrain receptor concentrations
      if(nctrec .GT. 0 .AND. LCT) then
         do 7 i=1,nctrec
7           avct(i)=avct(i)/n
      endif
c
      if(LVISIB)then
        if(LSGRID .AND. LG)then
          do i=ibgrid,iegrid
            do j=jbgrid,jegrid
               avbg(i,j)=avbg(i,j)/n
               avhg(i,j)=avhg(i,j)/n
               avpg(i,j)=avpg(i,j)/n
            enddo
          enddo
        endif
        if(ndrec.gt.0 .AND. LD)then
          do i=1,ndrec
             avbd(i)=avbd(i)/n
             avhd(i)=avhd(i)/n
             avpd(i)=avpd(i)/n
          enddo
        endif
        if(nctrec.gt.0 .AND. LCT)then
          do i=1,nctrec
             avbt(i)=avbt(i)/n
             avht(i)=avht(i)/n
             avpt(i)=avpt(i)/n
          enddo
        endif
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine avgaconc(n,avcg,avcd,avct)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 061120          AVGACONC
c ---           D. Strimaitis
c
c  PURPOSE:     Divides summed concentration arrays by number of
c               averaging periods to obtain average concentrations
c               (all species version)
c
c  ARGUMENTS:
c     PASSED:   n               Number of periods in sum             [i]
c   RETURNED:   avc[g,d,t]      Averaged concentration arrays       [ra]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'source.pst'
      INCLUDE 'specout.pst'

      real avcg(mxgx,mxgy,mxspec),avcd(mxdrec,mxspec)
      real avct(mxctrec,mxspec)

c --- Average sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do k=1,nospec
            do i=1,nsrc
               avcd(i,k)=avcd(i,k)/n
            enddo
         enddo
         return
      endif

c --- Average gridded receptor concentrations
      if(LSGRID .AND. LG) then
         do k=1,nospec
            do i=ibgrid,iegrid
            do j=jbgrid,jegrid
               avcg(i,j,k)=avcg(i,j,k)/n
            enddo
            enddo
         enddo
      endif
c
c --- Average discrete receptor concentrations
      if(ndrec .GT. 0 .AND. LD) then
         do k=1,nospec
            do i=1,ndrec
               avcd(i,k)=avcd(i,k)/n
            enddo
         enddo
      endif
c
c --- Average complex terrain receptor concentrations
      if(nctrec .GT. 0 .AND. LCT) then
         do k=1,nospec
            do i=1,nctrec
               avct(i,k)=avct(i,k)/n
            enddo
         enddo
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine sumvisb(avcg,avcd,avct,avbg,avbd,avbt,avhg,avhd,avht,
     &                   avng,avnd,avnt,avpg,avpd,avpt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 041202           SUMVISB
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Sums current extinctions to appropriate averaging
c               array, for receptors with RH <= RHMAX.
c
c  UPDATES:
c  V5.1(990920) to V5.633(041202)
c               (CEC) Add computation of sum of
c                 (source extinction / background extinction) for Visibility
c                  called avpg, avpd, avpt
c  V5.0(981025) to V5.1(990920)
c               (DGS) Remove NDRECP filter; add LG,LD,LCT filter
c  V5.0(980918) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c
c  ARGUMENTS:
c     PASSED:   av[c,b,h,n,p][g,d,t]      Updated summing arrays              [ra]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'visb.pst'
c
      real avcg(mxgx,mxgy),avcd(mxdrec),avct(mxctrec)
      real avbg(mxgx,mxgy),avbd(mxdrec),avbt(mxctrec)
      real avhg(mxgx,mxgy),avhd(mxdrec),avht(mxctrec)
      real avng(mxgx,mxgy),avnd(mxdrec),avnt(mxctrec)
      real avpg(mxgx,mxgy),avpd(mxdrec),avpt(mxctrec)
c
c  QA: this better be a visibility run!
      if(.not.LVISIB) stop 'SUMVISB should only be called for VIS'

c  Sum gridded receptor concentrations
      if(LSGRID .AND. LG) then
         do j=jbgrid,jegrid
         do i=ibgrid,iegrid
             if(gbext0(i,j).GT.0. .AND. gfrh(i,j).LE.frhmax) then
               avng(i,j)=avng(i,j)+1.0
               avbg(i,j)=avbg(i,j)+gbext0(i,j)
               avhg(i,j)=avhg(i,j)+gfrh(i,j)
               avcg(i,j)=avcg(i,j)+concg(i,j)
               if (gbext0(i,j).ne.0.) then
                if (LVBK) then
                avpg(i,j)=avpg(i,j)+(concg(i,j)-gbext0(i,j))/gbext0(i,j)
                else
                avpg(i,j)=avpg(i,j)+concg(i,j)/gbext0(i,j)
                endif
               else
               STOP ' background extinction is = 0.0'
               end if
             endif
         enddo
         enddo
      endif
c
c  Sum discrete receptor concentrations

      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
            if(dbext0(i).GT.0. .AND. dfrh(i).LE.frhmax) then
               avnd(i)=avnd(i)+1.0
               avbd(i)=avbd(i)+dbext0(i)
               avhd(i)=avhd(i)+dfrh(i)
               avcd(i)=avcd(i)+concd(i)
               if (dbext0(i).ne.0.) then
                if (LVBK) then
                avpd(i)=avpd(i)+(concd(i)-dbext0(i))/dbext0(i)
                else
                avpd(i)=avpd(i)+concd(i)/dbext0(i)
                endif
               else
               STOP ' background extinction is = 0.0'
               end if
            endif
         enddo
      endif
c
c  Sum complex terrain receptor concentrations
      if(nctrec .GT. 0 .AND. LCT) then
         do i=1,nctrec
             if(tbext0(i).GT.0. .AND. tfrh(i).LE.frhmax) then
               avnt(i)=avnt(i)+1.0
               avbt(i)=avbt(i)+tbext0(i)
               avht(i)=avht(i)+tfrh(i)
               avct(i)=avct(i)+conct(i)
               if (tbext0(i).ne.0.) then
                if (LVBK) then
                avpt(i)=avpt(i)+(conct(i)-tbext0(i))/tbext0(i)
                else
                avpt(i)=avpt(i)+conct(i)/tbext0(i)
                endif
               else
               STOP ' background extinction is = 0.0'
               end if
             endif
         enddo
      endif
c
c
      return
      end
c-----------------------------------------------------------------------
      subroutine avgvisb(n,avcg,avcd,avct,avbg,avbd,avbt,avhg,avhd,avht,
     &                   avng,avnd,avnt,avpg,avpd,avpt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 041202           AVGVISB
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Divides summed extinction arrays by number of values
c               in the summation to obtain average extinction
c
c  UPDATES:
c  V5.1(990920) to V5.633(041202)
c               (CEC) Add computation of sum of
c                 (source extinction / background extinction) for Visibility
c                  called avpg, avpd, avpt
c  V5.0(981025) to V5.1(990920)
c               (DGS) Remove NDRECP filter; add LG,LD,LCT filter
c  V5.0(980918) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c
c  ARGUMENTS:
c     PASSED:                n  Number of periods in sum            [i]
c           av[c,b,h,n][g,d,t]  Summing arrays                     [ra]
c   RETURNED: av[c,b,h][g,d,t]  Averaged arrays                    [ra]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'conc.pst'
c
      real avcg(mxgx,mxgy),avcd(mxdrec),avct(mxctrec)
      real avbg(mxgx,mxgy),avbd(mxdrec),avbt(mxctrec)
      real avhg(mxgx,mxgy),avhd(mxdrec),avht(mxctrec)
      real avng(mxgx,mxgy),avnd(mxdrec),avnt(mxctrec)
      real avpg(mxgx,mxgy),avpd(mxdrec),avpt(mxctrec)
c
c  QA: this better be a visibility run!
      if(.not.LVISIB) stop 'AVGVISB should only be called for VIS'

c  Set minimum number of values in sum for valid average
      nmin=NINT(0.25*FLOAT(n))
      nmin=MAX0(1,nmin)
c
c  Average gridded receptor extinctions
      if(LSGRID .AND. LG) then
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               num=NINT(avng(i,j))
               if(num.GE.nmin) then
                  avbg(i,j)=avbg(i,j)/num
                  avhg(i,j)=avhg(i,j)/num
                  avcg(i,j)=avcg(i,j)/num
                  avpg(i,j)=avpg(i,j)/num
               else
                  avbg(i,j)=-99.
                  avhg(i,j)=0.0
                  avcg(i,j)=-99.
                  avpg(i,j)=-99.
               endif
            enddo
         enddo
      endif
c
c  Average discrete receptor concentrations
      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
            num=NINT(avnd(i))
            if(num.GE.nmin) then
               avbd(i)=avbd(i)/num
               avhd(i)=avhd(i)/num
               avcd(i)=avcd(i)/num
               avpd(i)=avpd(i)/num
            else
               avbd(i)=-99.
               avhd(i)=0.0
               avcd(i)=-99.
               avpd(i)=-99.
            endif
         enddo
      endif
c
c  Average complex terrain receptor concentrations
      if(nctrec .GT. 0 .AND. LCT) then
         do i=1,nctrec
            num=NINT(avnt(i))
            if(num.GE.nmin) then
               avbt(i)=avbt(i)/num
               avht(i)=avht(i)/num
               avct(i)=avct(i)/num
               avpt(i)=avpt(i)/num
            else
               avbt(i)=-99.
               avht(i)=0.0
               avct(i)=-99.
               avpt(i)=-99.
            endif
         enddo
      endif
c
c
      return
      end
c-----------------------------------------------------------------------
      subroutine sumex24(lfull)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 061107          SUMEX24
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Sums current extinctions to 24-hour averaging
c               array for each species.
c
c  UPDATES:
c  V5.2(991104a) to V6.14(061107)
c               (DGS) Add NO2 as modeled species
c  V5.1(990920) to V5.2(991104a)
c               (DGS) Add Elemental Carbon as modeled species
c  V5.0(981025) to V5.1(990920)
c               (DGS) Remove NDRECP filter; add LG,LD,LCT filter
c
c  ARGUMENTS:
c     PASSED:            lfull   Full set of 24 values used?        [L]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'visb.pst'
      INCLUDE 'vispec.pst'

      logical lfull
c
c  QA: this better be a visibility run!
      if(.not.LVISIB) stop 'SUMEX24 should only be called for VIS'

c  Sum gridded receptor extinctions
      if(LSGRID .AND. LG) then
         do j=jbgrid,jegrid
         do i=ibgrid,iegrid
            if(LFULL .OR.
     &         (gbext0(i,j).GT.0. .AND. gfrh(i,j).LE.frhmax)) then
               av24s4g(i,j)=av24s4g(i,j)+gso4(i,j)
               av24n3g(i,j)=av24n3g(i,j)+gno3(i,j)
               av24n2g(i,j)=av24n2g(i,j)+gno2(i,j)
               av24ocg(i,j)=av24ocg(i,j)+goc(i,j)
               av24ecg(i,j)=av24ecg(i,j)+gec(i,j)
               av24pcg(i,j)=av24pcg(i,j)+gpmc(i,j)
               av24pfg(i,j)=av24pfg(i,j)+gpmf(i,j)
            endif
         enddo
         enddo
      endif
c
c  Sum discrete receptor extinctions

      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
            if(LFULL .OR .
     &        (dbext0(i).GT.0. .AND. dfrh(i).LE.frhmax)) then
               av24s4d(i)=av24s4d(i)+dso4(i)
               av24n3d(i)=av24n3d(i)+dno3(i)
               av24n2d(i)=av24n2d(i)+dno2(i)
               av24ocd(i)=av24ocd(i)+doc(i)
               av24ecd(i)=av24ecd(i)+dec(i)
               av24pcd(i)=av24pcd(i)+dpmc(i)
               av24pfd(i)=av24pfd(i)+dpmf(i)
            endif
         enddo
      endif
c
c  Sum complex terrain receptor extinctions
      if(nctrec .GT. 0 .AND. LCT) then
         do i=1,nctrec
            if(LFULL .OR.
     &         (tbext0(i).GT.0. .AND. tfrh(i).LE.frhmax)) then
               av24s4t(i)=av24s4t(i)+tso4(i)
               av24n3t(i)=av24n3t(i)+tno3(i)
               av24n2t(i)=av24n2t(i)+tno2(i)
               av24oct(i)=av24oct(i)+toc(i)
               av24ect(i)=av24ect(i)+tec(i)
               av24pct(i)=av24pct(i)+tpmc(i)
               av24pft(i)=av24pft(i)+tpmf(i)
            endif
         enddo
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine avgex24(lfull,avng,avnd,avnt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 061107          AVGEX24
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Divides summed extinction arrays by number of values
c               in the summation to obtain extinction by species
c               for daily average
c
c  UPDATES:
c  V5.2(991104a) to V6.14(061107)
c               (DGS) Add NO2 as modeled species
c  V5.1(990920) to V5.2(991104a)
c               (DGS) Add Elemental Carbon as modeled species
c  V5.0(981025) to V5.1(990920)
c               (DGS) Remove NDRECP filter; add LG,LD,LCT filter
c
c  ARGUMENTS:
c     PASSED:            lfull  Full set of 24 values used?          [L]
c                   avn[g,d,t]  Number in sum                       [ra]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'vispec.pst'
c
      real avng(mxgx,mxgy),avnd(mxdrec),avnt(mxctrec)
      logical lfull

c  QA: this better be a visibility run!
      if(.not.LVISIB) stop 'AVGEX24 should only be called for VIS'

c  Set minimum number of values in sum for valid 24-hr average
      nmin=6

c  Set full number of hours in day
      num=24

c  Average gridded receptor extinctions
      if(LSGRID .AND. LG) then
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(.not.LFULL) num=NINT(avng(i,j))
               if(num.GE.nmin) then
                  av24s4g(i,j)=av24s4g(i,j)/num
                  av24n3g(i,j)=av24n3g(i,j)/num
                  av24n2g(i,j)=av24n2g(i,j)/num
                  av24ocg(i,j)=av24ocg(i,j)/num
                  av24ecg(i,j)=av24ecg(i,j)/num
                  av24pcg(i,j)=av24pcg(i,j)/num
                  av24pfg(i,j)=av24pfg(i,j)/num
               else
                  av24s4g(i,j)=0.
                  av24n3g(i,j)=0.
                  av24n2g(i,j)=0.
                  av24ocg(i,j)=0.
                  av24ecg(i,j)=0.
                  av24pcg(i,j)=0.
                  av24pfg(i,j)=0.
               endif
            enddo
         enddo
      endif
c
c  Average discrete receptor concentrations
      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
            if(.not.LFULL) num=NINT(avnd(i))
            if(num.GE.nmin) then
               av24s4d(i)=av24s4d(i)/num
               av24n3d(i)=av24n3d(i)/num
               av24n2d(i)=av24n2d(i)/num
               av24ocd(i)=av24ocd(i)/num
               av24ecd(i)=av24ecd(i)/num
               av24pcd(i)=av24pcd(i)/num
               av24pfd(i)=av24pfd(i)/num
            else
               av24s4d(i)=0.
               av24n3d(i)=0.
               av24n2d(i)=0.
               av24ocd(i)=0.
               av24ecd(i)=0.
               av24pcd(i)=0.
               av24pfd(i)=0.
            endif
         enddo
      endif
c
c  Average complex terrain receptor concentrations
      if(nctrec .GT. 0 .AND. LCT) then
         do i=1,nctrec
            if(.not.LFULL) num=NINT(avnt(i))
            if(num.GE.nmin) then
               av24s4t(i)=av24s4t(i)/num
               av24n3t(i)=av24n3t(i)/num
               av24n2t(i)=av24n2t(i)/num
               av24oct(i)=av24oct(i)/num
               av24ect(i)=av24ect(i)/num
               av24pct(i)=av24pct(i)/num
               av24pft(i)=av24pft(i)/num
            else
               av24s4t(i)=0.
               av24n3t(i)=0.
               av24n2t(i)=0.
               av24oct(i)=0.
               av24ect(i)=0.
               av24pct(i)=0.
               av24pft(i)=0.
            endif
         enddo
      endif
c
c
      return
      end
c-----------------------------------------------------------------------
      subroutine countx(thresh,arrg,arrd,arrt,ixcg,ixcd,ixct)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017            COUNTX
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Tests data arrays against threshold value, and counts
c               exceedances.
c
c  UPDATES:
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.0(981025) to V5.1(990920)
c               (DGS) Switch integer*2 arrays to integer (*4)
c               (DGS) Add LG,LD,LCT filter, and skip excluded gridded
c                     receptors (NGRECP array)
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c  V5.0(950531) to V5.0(980821)
c               (DGS) Process sub-range of discrete receptors
c
c  ARGUMENTS:
c     PASSED:   thresh          threshold value                      [r]
c               arr[g,d,t]      Data arrays                         [ra]
c   RETURNED:   ixc[g,d,t]      Updated exceedance count arrays     [ia]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c
      real arrg(mxgx,mxgy),arrd(mxdrec),arrt(mxctrec)
      integer ixcg(mxgx,mxgy),ixcd(mxdrec),ixct(mxctrec)
c
c  Process sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
           if(arrd(i) .GT. thresh) ixcd(i)=ixcd(i)+1
         enddo
         return
      endif
c
c  Process gridded receptors
      if(LSGRID .AND. LG) then
         do i=ibgrid,iegrid
            do j=jbgrid,jegrid
               if(ngrecp(i,j).EQ.1) then
                  if(arrg(i,j) .GT. thresh) ixcg(i,j)=ixcg(i,j)+1
               endif
            enddo
         enddo
      endif
c
c  Process discrete receptors
      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
           if(ndrecp(i).EQ.1) then
             if(arrd(i) .GT. thresh) ixcd(i)=ixcd(i)+1
           endif
         enddo
      endif
c
c  Process complex terrain receptors
      if(nctrec .GT. 0 .AND. LCT) then
         do i=1,nctrec
            if(arrt(i) .GT. thresh) ixct(i)=ixct(i)+1
         enddo
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine reset(arrg,arrd,arrt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017             RESET
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Resets real arrays for each receptor type to zero
c
c  UPDATES:
c  V5.1(980918) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c
c  ARGUMENTS:
c     PASSED:   arr[g,d,t]      Arrays for gridded, discrete and    [ra]
c                               CTSG receptors            
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'source.pst'
c
      real arrg(mxgx,mxgy),arrd(mxdrec),arrt(mxctrec)
c
c  Reset sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            arrd(i)=0.0
         enddo
         return
      endif
c
c  Reset gridded receptor number
      if(LSGRID) then
        do j=1,ngy
          do i=1,ngx
            arrg(i,j)=0.0
          enddo
        enddo
      endif
c
c  Reset discrete receptor number
      if(ndrec .GT. 0) then
         do i=1,ndrec
            arrd(i)=0.0
         enddo
      endif
c
c  Reset complex terrain receptor number
      if(nctrec .GT. 0) then
         do i=1,nctrec
            arrt(i)=0.0
         enddo
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine reseti(iarrg,iarrd,iarrt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 070501            RESETI
c ---           D. Strimaitis
c
c  PURPOSE:     Resets integer arrays for each receptor type to zero
c
c  ARGUMENTS:
c     PASSED:   iarr[g,d,t]     Arrays for gridded, discrete and    [ia]
c                               CTSG receptors            
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'source.pst'
c
      real iarrg(mxgx,mxgy),iarrd(mxdrec),iarrt(mxctrec)
c
c  Reset sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            iarrd(i)=0
         enddo
         return
      endif
c
c  Reset gridded receptor number
      if(LSGRID) then
        do j=1,ngy
          do i=1,ngx
            iarrg(i,j)=0
          enddo
        enddo
      endif
c
c  Reset discrete receptor number
      if(ndrec .GT. 0) then
         do i=1,ndrec
            iarrd(i)=0
         enddo
      endif
c
c  Reset complex terrain receptor number
      if(nctrec .GT. 0) then
         do i=1,nctrec
            iarrt(i)=0
         enddo
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine init50(vinit,t50c,it50,ct50,t50xy,t50b,t50h)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 070501            INIT50
c ---           D. Strimaitis
c
c  PURPOSE:     Initializes Top-50 arrays
c
c
c  ARGUMENTS:
c     PASSED:   vinit           Non-zero initialization value        [r]
c   RETURNED:   t50c(50)        50 top concentration averages       [ra]
c               it50(50,5)      Yr,day,time,(ix,iy):(irec) for      [ia]
c                               above where time is HHMM
c               ct50(50)        Receptor type [g,d,t]               [ca]
c               t50xy(50,2)     Receptor coordinates (km)           [ra]
c               t50b(50)        50 top extinctions                  [ra]
c               t50h(50)        Relative humidity GROWTH factor     [ra]
c                               for above
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      real t50xy(50,2)
      real t50c(50)
      real t50b(50)
      real t50h(50)
      integer it50(50,5)
      character*1 ct50(50)
      do k=1,50
         t50c(k)=vinit
         t50b(k)=vinit
         t50h(k)=vinit
         ct50(k)=' '
         do n=1,2
            t50xy(k,n)=0.0
         enddo
         do n=1,5
            it50(k,n)=0
         enddo
      enddo

      return
      end

c-----------------------------------------------------------------------
      subroutine upt50(myr,mjday,mhr,msec,
     *                 avcg,avcd,avct,t50c,it50,ct50,t50xy,
     *                 avbg,avbd,avbt,t50b,avhg,avhd,avht,t50h)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012             UPT50
c ---           D. Strimaitis, SRC
c ---           (adapted from POSTBLP)
c
c  PURPOSE:     Updates Top-50 arrays with current concentration data
c
c  UPDATES:
c  V5.6(031017) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990709) to V5.1(990920)
c               (DGS) Skip excluded gridded receptors (NGRECP array)
c  V5.0(981025) to V5.1(990709)
c               (DGS) Track end-times to the minute for Top-50 entries
c  V5.0(950531) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c               (DGS) Process sub-range of discrete receptors
c
c  ARGUMENTS:
c     PASSED:   myr,mjday,      Starting date-time for current       [i]
c               mhr,msec        averaging period
c               avc[g,d,t]      Averaged concentration arrays       [ra]
c   RETURNED:   t50c(50)        50 top concentration averages       [ra]
c               it50(50,5)      Yr,day,time,(ix,iy):(irec) for      [ia]
c                               above where time is HHMM
c               ct50(50)        Receptor type [g,d,t]               [ca]
c               t50xy(50,2)     Receptor coordinates (km)           [ra]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c
      real avcg(mxgx,mxgy),avcd(mxdrec),avct(mxctrec)
      real avbg(mxgx,mxgy),avbd(mxdrec),avbt(mxctrec)
      real avhg(mxgx,mxgy),avhd(mxdrec),avht(mxctrec)
      real t50xy(50,2)
      real t50c(50)
      real t50b(50)
      real t50h(50)
      integer it50(50,5)
      character*1 ct50(50)
c
c  Section for gridded receptor concentrations
      if(LG .AND. msrc.NE.2) then
         do 50 i=ibgrid,iegrid
         do 50 j=jbgrid,jegrid
            if(ngrecp(i,j).NE.1) goto 50
            if(avcg(i,j) .LE. t50c(50)) goto 50
            avc=avcg(i,j)
            if(LVISIB)avb=avbg(i,j)
            if(LVISIB)avh=avhg(i,j)
c -- Search for rank (irplac) within top 50 concentrations
            ip=24
            mag=24
            do 10 ii=1,3
               mag=mag/2
               isgn=1
               if(avc .GT. t50c(ip)) isgn=-1
               ip=ip+isgn*mag
10          continue
            il=ip-2
            ih=ip+3
            do 20 ii=il,ih
               if(avc .LT. t50c(ii)) goto 20
               irplac=ii
               goto 30
20          continue
            irplac=50
            if(avc .GE. t50c(49)) irplac=49
30          index=50-irplac
            if(index .NE. 0) then
c -- Shift entries in top 50 arrays to make room for new entry
               do 40 ii=1,index
                  jj=50-ii
                  kk=jj+1
                  t50c(kk)=t50c(jj)
                  if(LVISIB)t50b(kk)=t50b(jj)
                  if(LVISIB)t50h(kk)=t50h(jj)
                  it50(kk,1)=it50(jj,1)
                  it50(kk,2)=it50(jj,2)
                  it50(kk,3)=it50(jj,3)
                  it50(kk,4)=it50(jj,4)
                  it50(kk,5)=it50(jj,5)
                  ct50(kk)=ct50(jj)
                  t50xy(kk,1)=t50xy(jj,1)
                  t50xy(kk,2)=t50xy(jj,2)
40             continue
            endif
c -- Place current values into proper place in top 50 arrays
            t50c(irplac)=avc
            if(LVISIB)t50b(irplac)=avb
            if(LVISIB)t50h(irplac)=avh
            it50(irplac,1)=myr
            it50(irplac,2)=mjday
            it50(irplac,3)=mhr*100+msec/60
            it50(irplac,4)=i
            it50(irplac,5)=j
            ct50(irplac)="G"
            t50xy(irplac,1)=xgrd(i,j)
            t50xy(irplac,2)=ygrd(i,j)
50       continue
      endif
c
c  Section for discrete receptor (or source) concentrations
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(LD .OR. msrc.EQ.2) then
         num=ndrec
         if(msrc.EQ.2) num=nsrc
         do 150 i=1,num
           if(ndrecp(i).NE.1 .AND. msrc.NE.2) goto 150
           if(avcd(i) .LE. t50c(50)) goto 150
            avc=avcd(i)
            if(LVISIB)avb=avbd(i)
            if(LVISIB)avh=avhd(i)
c -- Search for rank (irplac) within top 50 concentrations
            ip=24
            mag=24
            do 110 ii=1,3
               mag=mag/2
               isgn=1
               if(avc .GT. t50c(ip)) isgn=-1
               ip=ip+isgn*mag
110         continue
            il=ip-2
            ih=ip+3
            do 120 ii=il,ih
               if(avc .LT. t50c(ii)) goto 120
               irplac=ii
               goto 130
120         continue
            irplac=50
            if(avc .GE. t50c(49)) irplac=49
130         index=50-irplac
            if(index .NE. 0) then
c -- Shift entries in top 50 arrays to make room for new entry
               do 140 ii=1,index
                  jj=50-ii
                  kk=jj+1
                  t50c(kk)=t50c(jj)
                  if(LVISIB)t50b(kk)=t50b(jj)
                  if(LVISIB)t50h(kk)=t50h(jj)
                  it50(kk,1)=it50(jj,1)
                  it50(kk,2)=it50(jj,2)
                  it50(kk,3)=it50(jj,3)
                  it50(kk,4)=it50(jj,4)
                  it50(kk,5)=it50(jj,5)
                  ct50(kk)=ct50(jj)
                  t50xy(kk,1)=t50xy(jj,1)
                  t50xy(kk,2)=t50xy(jj,2)
140            continue
            endif
c -- Place current values into proper place in top 50 arrays
            t50c(irplac)=avc
            if(LVISIB)t50b(irplac)=avb
            if(LVISIB)t50h(irplac)=avh
            it50(irplac,1)=myr
            it50(irplac,2)=mjday
            it50(irplac,3)=mhr*100+msec/60
            it50(irplac,4)=0
            it50(irplac,5)=i
            ct50(irplac)="D"
            if(msrc.EQ.2) ct50(irplac)="S"
            t50xy(irplac,1)=xrec(i)
            t50xy(irplac,2)=yrec(i)
150      continue
      endif
c
c  Section for complex terrain receptor concentrations
      if(LCT .AND. msrc.NE.2) then
         do 250 i=1,nctrec
            if(avct(i) .LE. t50c(50)) goto 250
            avc=avct(i)
            if(LVISIB)avb=avbt(i)
            if(LVISIB)avh=avht(i)
c -- Search for rank (irplac) within top 50 concentrations
            ip=24
            mag=24
            do 210 ii=1,3
               mag=mag/2
               isgn=1
               if(avc .GT. t50c(ip)) isgn=-1
               ip=ip+isgn*mag
210         continue
            il=ip-2
            ih=ip+3
            do 220 ii=il,ih
               if(avc .LT. t50c(ii)) goto 220
               irplac=ii
               goto 230
220         continue
            irplac=50
            if(avc .GE. t50c(49)) irplac=49
230         index=50-irplac
            if(index .NE. 0) then
c -- Shift entries in top 50 arrays to make room for new entry
               do 240 ii=1,index
                  jj=50-ii
                  kk=jj+1
                  t50c(kk)=t50c(jj)
                  if(LVISIB)t50b(kk)=t50b(jj)
                  if(LVISIB)t50h(kk)=t50h(jj)
                  it50(kk,1)=it50(jj,1)
                  it50(kk,2)=it50(jj,2)
                  it50(kk,3)=it50(jj,3)
                  it50(kk,4)=it50(jj,4)
                  it50(kk,5)=it50(jj,5)
                  ct50(kk)=ct50(jj)
                  t50xy(kk,1)=t50xy(jj,1)
                  t50xy(kk,2)=t50xy(jj,2)
240            continue
            endif
c -- Place current values into proper place in top 50 arrays
            t50c(irplac)=avc
            if(LVISIB)t50b(irplac)=avb
            if(LVISIB)t50h(irplac)=avh
            it50(irplac,1)=myr
            it50(irplac,2)=mjday
            it50(irplac,3)=mhr*100+msec/60
            it50(irplac,4)=0
            it50(irplac,5)=i
            ct50(irplac)="T"
            t50xy(irplac,1)=xctr(i)
            t50xy(irplac,2)=yctr(i)
250      continue
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine uptn(myr,mjday,mhr,msec,
     *                avcg,avcd,avct,tng,tnd,tnt,ng,nd,nt)
c     subroutine uptn(myr,mjday,mhr,msec,
c     *               avcg,avcd,avct,tng,tnd,tnt,ng,nd,nt,
c    *                avbg,avbd,avbt,tngb,tndb,tntb,
c    *                avhg,avhd,avht,tngh,tndh,tnth)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012              UPTN
c ---           D. Strimaitis, SRC
c ---           (adapted from POSTBLP)
c
c  PURPOSE:     Updates Top-N arrays with current concentration data
c
c  UPDATES:
c  V5.6(031017) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990806) to V5.1(990920)
c               (DGS) Skip excluded gridded receptors (NGRECP array)
c  V5.1(990709) to V5.1(990806)
c               (DGS) Use 2-D variable for date/time: 1=YYYYJJJ, 2=HHMM
c  V5.0(981025) to V5.1(990709)
c               (DGS) Track end-times to the minute for Top-50 entries
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c  V5.0(950531) to V5.0(980821)
c               (DGS) Process sub-range of discrete receptors
c
c  ARGUMENTS:
c     PASSED:   myr,mjday,      Starting date-time for current       [i]
c               mhr,msec        averaging period
c               avc[g,d,t]      Averaged concentration arrays       [ra]
c   RETURNED:   tn[g,d,t]       N top concentration averages        [ra]
c               n[g,d,t]        YYYYJJJ;HHMM for above              [ia]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c
      real avcg(mxgx,mxgy),avcd(mxdrec),avct(mxctrec)
      real tng(mxgx,mxgy,mxrnk),tnd(mxdrec,mxrnk),tnt(mxctrec,mxrnk)
c --------------------------------------------------------------------
c     real avbg(mxgx,mxgy),avbd(mxdrec),avbt(mxctrec)
c     real avhg(mxgx,mxgy),avhd(mxdrec),avht(mxctrec)
c     real tngb(mxgx,mxgy,mxrnk),tndb(mxdrec,mxrnk),tntb(mxctrec,mxrnk)
c     real tngh(mxgx,mxgy,mxrnk),tndh(mxdrec,mxrnk),tnth(mxctrec,mxrnk)
c --------------------------------------------------------------------
      integer ng(2,mxgx,mxgy,mxrnk),nd(2,mxdrec,mxrnk),
     &        nt(2,mxctrec,mxrnk)
c
      nrnkm1=nrnk-1
c
c  Section for gridded receptor concentrations
      if(LG .AND. msrc.NE.2) then
         do 50 i=ibgrid,iegrid
         do 50 j=jbgrid,jegrid
            if(ngrecp(i,j).NE.1) goto 50
            if(avcg(i,j) .LE. tng(i,j,nrnk)) goto 50
            avc=avcg(i,j)
c           if(LVISIB)avb=avbg(i,j)
c           if(LVISIB)avh=avhg(i,j)
c -- Search for rank (irplac) within top N concentrations
            do 20 ii=1,nrnkm1
               if(avc .LT. tng(i,j,ii)) goto 20
               irplac=ii
               goto 30
20          continue
            irplac=nrnk
30          index=nrnk-irplac
            if(index .NE. 0) then
c -- Shift entries in top N arrays to make room for new entry
               do 40 ii=1,index
                  jj=nrnk-ii
                  kk=jj+1
                  tng(i,j,kk)=tng(i,j,jj)
c                 if(LVISIB)tngb(i,j,kk)=tngb(i,j,jj)
c                 if(LVISIB)tngh(i,j,kk)=tngh(i,j,jj)
                  ng(1,i,j,kk)=ng(1,i,j,jj)
                  ng(2,i,j,kk)=ng(2,i,j,jj)
40             continue
            endif
c -- Place current values into proper place in top N arrays
            tng(i,j,irplac)=avc
c           if(LVISIB)tngb(i,j,irplac)=avb
c           if(LVISIB)tngh(i,j,irplac)=avh
            ng(1,i,j,irplac)=myr*1000+mjday
            ng(2,i,j,irplac)=mhr*100+msec/60
50       continue
      endif
c
c  Section for discrete receptor (or source) concentrations
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(LD .OR. msrc.EQ.2) then
         num=ndrec
         if(msrc.EQ.2) num=nsrc
         do 150 i=1,num
           if(ndrecp(i).EQ.1 .OR. msrc.EQ.2) then
             if(avcd(i) .LE. tnd(i,nrnk)) goto 150
             avc=avcd(i)
c            if(LVISIB)avb=avbd(i)
c            if(LVISIB)avh=avhd(i)
c -- Search for rank (irplac) within top N concentrations
             do 120 ii=1,nrnkm1
                if(avc .LT. tnd(i,ii)) goto 120
                irplac=ii
                goto 130
120          continue
             irplac=nrnk
130          index=nrnk-irplac
             if(index .NE. 0) then
c -- Shift entries in top N arrays to make room for new entry
                do 140 ii=1,index
                   jj=nrnk-ii
                   kk=jj+1
                   tnd(i,kk)=tnd(i,jj)
c                  if(LVISIB)tndb(i,kk)=tndb(i,jj)
c                  if(LVISIB)tndh(i,kk)=tndh(i,jj)
                   nd(1,i,kk)=nd(1,i,jj)
                   nd(2,i,kk)=nd(2,i,jj)
140             continue
             endif
c -- Place current values into proper place in top N arrays
             tnd(i,irplac)=avc
c            if(LVISIB)tndb(i,irplac)=avb
c            if(LVISIB)tndh(i,irplac)=avh
             nd(1,i,irplac)=myr*1000+mjday
             nd(2,i,irplac)=mhr*100+msec/60
           endif
150      continue
      endif
c
c  Section for complex terrain receptor concentrations
      if(LCT .AND. msrc.NE.2) then
         do 250 i=1,nctrec
            if(avct(i) .LE. tnt(i,nrnk)) goto 250
            avc=avct(i)
c           if(LVISIB)avb=avbt(i)
c           if(LVISIB)avh=avht(i)
c -- Search for rank (irplac) within top N concentrations
            do 220 ii=1,nrnkm1
               if(avc .LT. tnt(i,ii)) goto 220
               irplac=ii
               goto 230
220         continue
            irplac=nrnk
230         index=nrnk-irplac
            if(index .NE. 0) then
c -- Shift entries in top N arrays to make room for new entry
               do 240 ii=1,index
                  jj=nrnk-ii
                  kk=jj+1
                  tnt(i,kk)=tnt(i,jj)
c                 if(LVISIB)tntb(i,kk)=tntb(i,jj)
c                 if(LVISIB)tnth(i,kk)=tnth(i,jj)
                  nt(1,i,kk)=nt(1,i,jj)
                  nt(2,i,kk)=nt(2,i,jj)
240            continue
            endif
c -- Place current values into proper place in top N arrays
            tnt(i,irplac)=avc
c           if(LVISIB)tntb(i,irplac)=avb
c           if(LVISIB)tnth(i,irplac)=avh
            nt(1,i,irplac)=myr*1000+mjday
            nt(2,i,irplac)=mhr*100+msec/60
250      continue
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine wrivl(ichan,vers,lev,ipage)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 980515             WRIVL
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Write identification label that specifies version
c               number and level of CALPOST.
c
c  UPDATES:
c     V5.0(890515) to V5.0(980515)
c               (DGS) Change version and level to character*12
c
c  ARGUMENTS:
c     PASSED:   ichan   channel number for output file               [i]
c               vers    version number                               [c]
c               lev     level (modification date yymmdd)             [c]
c               ipage   flag for page break (0 = no page)            [i]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST, READCF
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      character*12 vers,lev
c  Define page break character pb:
      character*1 pb
      pb=' '
c
      if(ipage .NE. 0) write(ichan,*) pb
      write(ichan,100)
      write(ichan,110) vers,lev
      write(ichan,100)
      write(ichan,*) ' '
      write(ichan,*) ' '
100   format(13('**********'))
110   format(t47,'CALPOST  Version ',a12,'  Level ',a12)
c
      return
      end
c-----------------------------------------------------------------------
      subroutine echo_hd(ichan)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050915           ECHO_HD
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Write header information describing "echo" output
c
c  UPDATES:
c     V5.6(031017) to V6.1(050915)
c               (DGS) Identify standard averaging times
c     V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c     V5.1(990709) to V5.1(990920)
c               (DGS) Averaging time logicals used for all output
c     V5.0(890515) to V5.1(990709)
c               (DGS) MAVG labels based on AVTIME units
c
c  ARGUMENTS:
c     PASSED:   ichan   channel number for output file               [i]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
      character*6 avgper

c --- Set averaging time information
      avgper='  HOUR'
c
      write(ichan,*)
      write(ichan,*)
      write(ichan,*)
      write(ichan,*)'-------------------------------------------------'
      write(ichan,*) 'ECHO OPTION -'
      write(ichan,*)'-------------------------------------------------'
      if(msrc.NE.2) then
         write(ichan,100) cdname
      else
         write(ichan,101) cdname
      endif
      write(ichan,110) iecho
      write(ichan,120) mavg,avtime
c
      if(L1HR) then
         inum=1
         write(ichan,'(10x,i10,a7)') inum,avgper
      endif
      if(L3HR) then
         inum=3
         write(ichan,'(10x,i10,a7)') inum,avgper
      endif
      if(L24HR) then
         inum=24
         write(ichan,'(10x,i10,a7)') inum,avgper
      endif
      if(L1PD) then
         inum=mavg
         write(ichan,'(10x,i10,a7)') inum,avtime
      endif
      if(LNAVG) then
         inum=navg*mavg
         write(ichan,'(10x,i10,a7)') inum,avtime
      endif
      write(ichan,*)

      return

100   format(1x,a14,' AT EACH RECEPTOR IS PRINTED FOR THE',
     *       ' FOLLOWING DAYS (0=NOT printed; 1=PRINTED):')
101   format(1x,a14,' AT EACH SOURCE IS PRINTED FOR THE',
     *       ' FOLLOWING DAYS (0=NOT printed; 1=PRINTED):')
110   format(3(10(10i1,3x)/),6(10i1,3x),6i1)
120   format(' AND FOR THE FOLLOWING AVERAGING PERIODS:    (NOTE THAT',
     * ' THE AVERAGING PERIOD IN MODEL IS ',i3,1x,a6,' )')

      end
c-----------------------------------------------------------------------
      subroutine echo(ichan,navper,avcg,avcd,avct,avcr,
     *                myr,mjday,mhr,msec)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012              ECHO
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Write "echo" output
c
c  UPDATES:
c     V6.1(050915) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c     V5.6(031017) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c     V5.2(991104c) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c     V5.1(990920) to V5.2(991104c)
c               (DGS) Add RING receptors
c     V5.1(990709) to V5.1(990920)
c               (DGS) Set excluded gridded receptors to 'null' value
c               (DGS) Apply LG,LD,LCT filter
c     V5.0(981116) to V5.1(990709)
c               (DGS) Seconds added to end-time label
c                     MAVG label based on AVTIME units
c                     YYYY format for year
c     V5.0(981025) to V5.0(981116)
c               (DGS) Allow ouput of gridded and discrete receptor
c                     subsets
c     V5.0(971015) to V5.0(981025)
c               (DGS) Modify format to allow negatives for missing
c                     values (visibility)
c
c  ARGUMENTS:
c     PASSED:   ichan   channel number for output file               [i]
c               navper  number of periods in average                 [i]
c               avcg    average concentrations at grid receptors    [ra]
c               avcd    average concentrations at discrete receptors[ra]
c               avct    average concentrations at terrain receptors [ra]
c               avcr    peak average concentration on rings         [ra]
c               myr     year at start of averaging period            [i]
c               mjday   Julian day at start of averaging period      [i]
c               mhr     hour at start of averaging period (0-23)     [i]
c               msec    second at start of averaging period (0-3599) [i]
c
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   OUTGRD
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ring.pst'
      INCLUDE 'source.pst'
c
      character*70 messag
      character*6 avgper
c
c  Declare dummy arrays
      real avcg(mxgx,mxgy),avcd(mxdrec),avct(mxctrec),avcr(mxring)
      real avcgp(mxgx,mxgy)
c
c  Declare array for storing index of selected discrete receptors
      integer outrec(mxdrec)
c
c  Null value for reporting excluded receptor
      data anull/0.0/
c

c --- Set averaging time information
      if(navper.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(navper.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(navper.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=navper*mavg
         avgper=avtime
      endif

c --------
c  Section for sources (ECHO values and quit)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
c  Identify time-starting and averaging time
         write(ichan,*) ' '
         write(ichan,*) ' '
         write(ichan,*) ' '
         write(ichan,101) iavgpd,avgper,cdname,myr,mjday,mhr,msec
101      format(5x,i4,a7,' AVERAGE ',a14,' FOR EACH',
     * ' SOURCE FOR THE PERIOD STARTING',t84,'YEAR:',i5,' DAY:',i4,
     * ' HOUR:',i3,' SEC:',i5,/)
         write(ichan,*) ' '
         write(ichan,*) '       SOURCE TRACEBACK:  ',asplv
         write(ichan,*) ' '
            write(ichan,102) cdname,cdname
102      format('SOURCE      COORDINATES (km)',8x,a14,12x,
     *          'SOURCE      COORDINATES (km)',8x,a14,/)
c
c -- split data into 2 columns
         nr2=nsrc/2
         do j=1,nr2
            j2=nr2+j
            write(ichan,124)j,xsrckm(j),ysrckm(j),avcd(j),
     &                      j2,xsrckm(j2),ysrckm(j2),avcd(j2)
         enddo
         if(MOD(nsrc,2) .NE. 0) then
            k=nsrc
            write(ichan,126)k,xsrckm(k),ysrckm(k),avcd(k)
         endif
         return
      endif
c ---------

c
c  Identify time-starting and averaging time
      write(ichan,*) ' '
      write(ichan,*) ' '
      write(ichan,*) ' '
      write(ichan,100) iavgpd,avgper,cdname,myr,mjday,mhr,msec
100   format(5x,i4,a7,' AVERAGE ',a14,' AT EACH',
     * ' RECEPTOR FOR THE PERIOD STARTING',t84,'YEAR:',i5,' DAY:',i4,
     * ' HOUR:',i3,' SEC:',i5,/)
c
c  Section for gridded receptors
      if(LSGRID .AND. LG) then
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(ngrecp(i,j).EQ.0) then
                  avcgp(i,j)=anull
               else
                  avcgp(i,j)=avcg(i,j)
               endif
            enddo
         enddo
         write(ichan,*) 'GRIDDED RECEPTORS:'
         messag=asplv
            call OUTGRD(avcgp,5,messag,ibgrid,jbgrid,iegrid,jegrid,io1)
      endif
c
c Section for discrete receptors
      if(ndrec .GT. 0 .AND. LD) then
         write(ichan,*) ' '
         write(ichan,*) '     DISCRETE RECEPTORS:  ',asplv
         write(ichan,*) ' '
            write(ichan,121) cdname,cdname
121      format('RECEPTOR    COORDINATES (km)',8x,a14,12x,
     *          'RECEPTOR    COORDINATES (km)',8x,a14,/)

c -- place selected receptors into array, and count total
         nrec=0
         do k=1,ndrec
            if(ndrecp(k).EQ.1) then
               nrec=nrec+1
               outrec(nrec)=k
            endif
         enddo

         if(nrec .GT. 1) then
c -- split data into 2 columns
            nr2=nrec/2
            do 125 j=1,nr2
               j2=nr2+j
               i=outrec(j)
               i2=outrec(j2)
               write(ichan,124)i,xrec(i),yrec(i),avcd(i),
     &                         i2,xrec(i2),yrec(i2),avcd(i2)
124            format(1x,i5,3x,2f10.3,8x,1pe11.4,0p,15x,i5,3x,2f10.3,
     &                8x,1pe11.4)
125         continue
         endif
         if(MOD(nrec,2) .NE. 0) then
            k=outrec(nrec)
            write(ichan,126)k,xrec(k),yrec(k),avcd(k)
126         format(63x,i5,3x,2f10.3,8x,1pe11.4)
         endif
      endif
c
c Section for reporting discrete receptor results by ring
      if(ndrec .GT. 0 .AND. LDRING) then
         write(ichan,*) ' '
         write(ichan,*) 'DISCRETE RECEPTORS - Peak By RING:  ',asplv
         write(ichan,*) ' '
         write(ichan,131) cdname
131      format('RING ID       RADIUS (km)   ',8x,a14,/)
         do k=1,ndring
            if(nrrecp(k).EQ.1) write(ichan,134)k,rradkm(k),avcr(k)
         enddo
134      format(1x,i5,'R',7x,f10.3,13x,1pe11.4)
      endif
c
c Section for terrain receptors
      if(nctrec .GT. 0 .AND. LCT) then
         write(ichan,*) ' '
         write(ichan,*) '     TERRAIN RECEPTORS:   ',asplv
         write(ichan,*) ' '
         write(ichan,121) cdname,cdname
         if(nctrec .GT. 1) then
c -- split data into 2 columns
            nr2=nctrec/2
            do i=1,nr2
               i2=nr2+i
               write(ichan,124)i,xctr(i),yctr(i),avct(i),
     &                         i2,xctr(i2),yctr(i2),avct(i2)
            enddo
         endif
         if(MOD(nctrec,2) .NE. 0) then
            write(ichan,126)nctrec,xctr(nctrec),yctr(nctrec),
     &                      avct(nctrec)
         endif
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine outgrd(rarray,nsigd,messag,nbx,nby,nex,ney,iogrd)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 981116            OUTGRD
c ---           D. Strimaitis, SRC
c
c *** Adapted (REAL ARRAYS ONLY; DATE OPTION REMOVED) from :
c --- CALGRID   Version: 0.3            Level: 890131           OUT
c               J. Scire, SRC
c
c  PURPOSE:     Write a gridded field of real numbers
c
c  UPDATES:
c  V5.0(980515) to V5.0(981116)
c               (DGS) Allow subset of grid to be printed
c  V5.0(890515) to V5.0(980515)
c               (DGS) Increase i,j index format from i2 to i3
c
c  ARGUMENTS:
c     PASSED:
c        RARRAY(nx,ny) - Real array  - Array of real numbers to print
c                NSIGD - Integer     - No. digits to print (valid range
c                                      for NSIGD is 1 to 5)
c               MESSAG - Char.*70    - Label of table
c                  NBX - Integer     - X dimension of array (beginning)
c                  NBY - Integer     - Y dimension of array (beginning)
c                  NEX - Integer     - X dimension of array (ending)
c                  NEY - Integer     - Y dimension of array (ending)
c                iogrd - Integer     - Fortran unit no. of output
c
c   RETURNED:   none
c
c  CALLING ROUTINES:    ECHO
c
c  EXTERNAL ROUTINES:   WRT, WRT2
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
c
c  Define working arrays
      real rarray(mxgx,mxgy)
      integer icol(5)
      integer iwrk1(mxgx)
      character*1 cwrk2(mxgx)
c
      character*70 messag
      character*1 plus,minus
      character*24 form1(5)
      character*21 form2(5)
      character*18 form3(5)
c
      data icol /30,30,30,25,20/
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
c --- check that valid values of print digits (nsigd) have been passed
      if(nsigd.lt.1.or.nsigd.gt.5)then
         write(iogrd,*)
     1   'ERROR in SUBR. OUT -- invalid value of NSIGD -- ',
     2   'NSIGD = ',nsigd
         stop
      endif
c
c --- compute number of cells in print-range across page
      nx=nex-nbx+1
c
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
      xmax=-1.e-25
      xmin=1.e25
      do 10 i=nbx,nex
      do 10 j=nby,ney
      if(rarray(i,j).gt.xmax)xmax=rarray(i,j)
      if(rarray(i,j).lt.xmin)xmin=rarray(i,j)
10    continue
      if(xmin.ne.0.0.or.xmax.ne.0.0)go to 12
      write(iogrd,95)messag
      write(iogrd,11)
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
      ic1=nbx
      ic2=ic1+icol(nsigd)-1
      if(ic2.gt.nex)ic2=nex
c
      do 30 ipass=1,npass
c
      write(iogrd,95)messag
95    format(/1x,a70/)
      write(iogrd,109)nexp
109   format(1x,'Multiply all values by 10 ** ',i3/)
c
         do 20 jj=ney,nby,-1
         icnt=0
c
            do 18 i=ic1,ic2
            icnt=icnt+1
            if(rarray(i,jj).lt.0)then
               iwrk1(icnt)=-(rarray(i,jj)*xscale-0.5)
               cwrk2(icnt)=minus
            else
               iwrk1(icnt)=rarray(i,jj)*xscale+0.5
               cwrk2(icnt)=plus
            endif
18          continue
         call wrt(form1(nsigd),form2(nsigd),jj,iwrk1,cwrk2,icnt,iogrd)
20       continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(iogrd,101)(minus,n=1,nund)
101   format(5x,128a1)
      call wrt2(form3(nsigd),ic1,ic2,iogrd)
c
      ic1=ic1+icol(nsigd)
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nex)ic2=nex
30    continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine outifx(iarray,nifm,messag,nbx,nby,nex,ney,iogrd)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 990920            OUTIFX
c ---           J. Scire, SRC
c
c --- PURPOSE:  Write a gridded field of integer numbers with fixed
c               integer format
c
c --- UPDATES:
c     V5.0(971015) to V5.1(990920)
c               (DGS) Switch integer*2 arrays to integer (*4)
c     V5.0(971007) to V5.0(971015)
c               (JSS) Modified to allow i3 field for X or Y cell index
c
c
c --- INPUTS:
c     IARRAY(MXGX,MXGY) - Int*4 array - Array of integers to print
c                  NIFM - Integer     - Integer format (1-4)
c                                       (print format is I"NIFM")
c                MESSAG - Char.*70    - Label of table
c                   NBX - Integer     - Starting X cell to print
c                   NBY - Integer     - Starting Y cell to print
c                   NEX - Integer     - Ending X cell to print
c                   NEY - Integer     - Ending Y cell to print
c                 iogrd - Integer     - Fortran unit no. of output
c      Parameters:
c          MXGX, MXGY, ICOLS
c
c --- OUTPUT:  none
c
c --- OUTIFX  called by:  WRIEXC
c --- OUTIFX  calls:      WRTIFX,WRT2
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'
c
      integer iarray(mxgx,mxgy)
      integer iout(icols)
      character*70 messag
      character*1 csign(icols),plus,minus
      character*27 form1(4)
      character*21 form2
      character*18 form3
c
      data form1 /'(1x,i3,1x,1hI,25(3x,i1,1x))',
     1            '(1x,i3,1x,1hI,25(2x,i2,1x))',
     2            '(1x,i3,1x,1hI,25(1x,i3,1x))',
     3            '(1x,i3,1x,1hI,25(i4,1x))'/
      data form2 /'(5x,1hI,25(3x,a1,1x))'/
      data form3 /'(6x,25(i4,1x))'/
      data plus,minus /'+','-'/
      data nsigd/4/
c
c --- determine total no. cells to print
      nx=nex-nbx+1
c
c --- check for invalid value of "NIFM"
      if(nifm.lt.1.or.nifm.gt.4)then
         write(iogrd,*)'ERROR in SUBR. OUTIFX -- invalid value of ',
     1   'NIFM -- NIFM = ',nifm
         stop
      endif
c
      if(mod(nx,icols).eq.0)then
         npass=nx/icols
      else
         npass=nx/icols+1
      endif
c
      ic1=nbx
      ic2=ic1+icols-1
      if(ic2.gt.nex)ic2=nex
c
      do 30 ipass=1,npass
c
         write(iogrd,95)messag
95       format(/1x,a70/)
c
         do 20 jj=ney,nby,-1
         icnt=0
c
            do 18 i=ic1,ic2
            icnt=icnt+1
            if(iarray(i,jj).lt.0)then
               iout(icnt)=abs(iarray(i,jj))
               csign(icnt)=minus
            else
               iout(icnt)=iarray(i,jj)
               csign(icnt)=plus
            endif
18          continue
         call wrtifx(form1(nifm),form2,jj,iout,csign,icnt,iogrd)
20       continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(iogrd,101)(minus,n=1,nund)
101   format(6x,128a1)
      call wrt2(form3,ic1,ic2,iogrd)
c
      ic1=ic1+icols
      ic2=ic2+icols
      if(ic2.gt.nex)ic2=nex
30    continue
      return
      end
c----------------------------------------------------------------------
      subroutine wrtifx(form1,form2,jj,iout,csign,n,iou)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 950531            WRTIFX
c ---           J. Scire, SRC
c
c --- PURPOSE:  Write one Y row of gridded integer data in I format
c
c --- INPUTS:
c              FORM1 - Char.*27    - Format field for Y label and data
c                                    to be printed
c              FORM2 - Char.*21    - Format field for sign of data
c                 JJ - Integer     - Y grid cell number
c            IOUT(N) - Integer arr.- Array of data to be printed
c                                    (one Y row)
c           CSIGN(N) - Char.*1     - Array containing sign of data
c                                    ('+' or '-')
c                  N - Integer     - Number of cells in this row
c                IOU - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRTIFX called by:  OUTIFX
c --- WRTIFX calls:      none
c----------------------------------------------------------------------
      integer iout(n)
c
      character*1 csign(n)
      character*27 form1
      character*21 form2
c
      write(iou,form1)jj,iout
      write(iou,form2)csign
c
      return
      end
c----------------------------------------------------------------------
      subroutine pltrec(icg,icd)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 040503            PLTREC
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Writes processed receptor field to a plot-file for QA
c               (All complex terrain receptors are processed if LCT)
c               (Output file name is fixed)
c
c  UPDATES:
c  V5.61(990920) to V5.62(040503)
c               (DGS) Replace filename strings c*70 with c*132
c
c  ARGUMENTS:
c     PASSED:   ic[g,d]         receptor flag arrays (0=NOT Used)   [ia]
c                                                    (1=Processed)
c
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'filnam.pst'
      INCLUDE 'head.pst'
c
      integer icg(mxgx,mxgy),icd(mxdrec),ict
      character*132 fname
      character*70 line1

c --- No complex terrain receptors are excluded
      data ict/1/

c --- Set title
      line1='Receptor Processing Flags:  0=Not Processed ; 1=Processed'

c --- Set filename
      if(LCFILES) then
         fname='postrec.dat'
      else
         fname='POSTREC.DAT'
      endif

c --- Open file
      open(mapu,file=fname)

c --- Write information to head of DATA file
c --- Set title
      write(mapu,*) line1
      write(mapu,*)
c --  Identify control logicals
      write(mapu,*) 'Control Logicals: LG,LD,LCT= ',LG,LD,LCT
      write(mapu,*)

c ---    Write flags at all receptors in DATA format
c --------------------------------------------------
c ---    Write column headings
         write(mapu,110)
         write(mapu,*)

c ---    Section for gridded receptors
         if(ngx .NE. 0 .AND. LG) then
c ---       ISC plot file format
            do j=jbgrid,jegrid
               do i=ibgrid,iegrid
                  write(mapu,120) xgrd(i,j),ygrd(i,j),icg(i,j)
               enddo
            enddo
         endif
c
c ---    Section for discrete receptors
         if(ld .AND. ndrec .NE. 0) then
            do i=1,ndrec
               if(ndrecp(i).EQ.1)write(mapu,120) xrec(i),yrec(i),icd(i)
            enddo
         endif
c
c ---    Section for terrain receptors
         if(lct .AND. nctrec .NE. 0) then
            do i=1,nctrec
               write(mapu,120) xctr(i),yctr(i),ict
            enddo
         endif
c

c --- Close file
      close(mapu)
c
      return
110   format(5x,'RECEPTOR (x,y) km',9x,'FLAG')
120   format(2x,2f10.3,5x,(i10,4x))
      end
c----------------------------------------------------------------------
      subroutine pltecho(npd,cg,cd,ct,iyr,ijday,ihr,isec)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012           PLTECHO
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Prepares plot-file for current period in selected day
c
c  UPDATES:
c  V6.1(050915) to V6.11(051012)
c               (DGS) Test for planar GRD (SURFER) output fields, and
c                     set min/max in the header slightly different to
c                     force SURFER to make a plot with no contours
c               (DGS) Switch from ending time to starting time
c  V5.62(040503) to V6.1(050915)
c               (DGS) Add seconds to arg list (current end-time)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.6(031017) to V5.62(040503)
c               (DGS) Replace filename strings c*70 with c*132
c               (DGS) Construct new long filename for file
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990709) to V5.1(990920)
c               (DGS) Set excluded gridded receptors to 'null' value
c               (DGS) Apply LG,LD,LCT filter
c               (DGS) Revise output logicals
c  V5.0(981116) to V5.1(990709)
c               (DGS) Use AVTIME in label
c  V5.0(980430) to V5.0(981116)
c               (DGS) Allow gridded and discrete receptor subsets
c
c  ARGUMENTS:
c     PASSED:   npd             length of averaging period          [i]
c               c[g,d,c]        "concentration" arrays             [ra]
c               iyr             Year for current period             [i]
c               ijday           Julian day for current period       [i]
c               ihr             hour for start of current period    [i]
c               isec            seconds for start of current period [i]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   GRDAY, FILCASE
c-----------------------------------------------------------------------
c --- Note:  This routine produces a plot-file for one period.  The
c ---        filename contains the date and hour (end of the period),
c ---        species name, type of output, and receptor number if
c ---        MSRC = 2.
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'filnam.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c
      real cg(mxgx,mxgy),cd(mxdrec),ct(mxctrec)
      real cgp(mxgx,mxgy)
      character*5 afmt(10)
      character*132 fname
      character*31 cdatetm
      character*60 ctrail
      character*5 arec
      character*6 avgper
      logical long

c --- Null value for reporting excluded receptor
      data anull/0.0/

c --- Set integer format array
      data afmt/'(i1)','(i2)','(i3)','(i4)','(i5)',
     &          '(i6)','(i7)','(i8)','(i9)','(i10)'/

c --- Set averaging time information
      if(npd.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(npd.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(npd.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=npd*mavg
         avgper=avtime
      endif

c --- Get month and day
      call GRDAY(io1,iyr,ijday,imo,iday)

c --- Set UTC shift from time zone
      ishift=NINT(-xbtz*100.)

c --- Set minutes from the seconds
      imm=isec/60

c --- Construct full date-time portion of filename
c ---          1234567890123456789012345678901
      cdatetm='yyyy_Mmm_Ddd_hhmm(UTC+zzzz)_L00'
      write(cdatetm(1:4),'(i4)') iyr
      write(cdatetm(7:8),'(i2.2)') imo
      write(cdatetm(11:12),'(i2.2)') iday
      write(cdatetm(14:15),'(i2.2)') ihr
      write(cdatetm(16:17),'(i2.2)') imm
      if(ishift.LT.0) then
         cdatetm(22:22)='-'
         ishift=-ishift
      endif
      if(ishift .LT. 9999) then
c ---    Valid XBTZ found in CALPUFF file
         write(cdatetm(23:26),'(i4.4)') ishift
      endif

c --- Construct trailing part of file name
c --- Set length of species name (left-justified)
      do k=1,12
         if(aspec(k:k) .NE. ' ') lenspec=k
      enddo
c --- Set length of avg time string
      if(iavgpd .GE. 10000) then
         lenpd=5
      elseif(iavgpd .GE. 1000) then
         lenpd=4
      elseif(iavgpd .GE. 100) then
         lenpd=3
      elseif(iavgpd .GE. 10) then
         lenpd=2
      else
         lenpd=1
      endif
c --- Set length of selected receptor string
      lenrec=0
      if(msrc.EQ.2) then
         if(LGS) then        
            krec=(jsrec-1)*ngx+isrec
            arec='_GREC'
         elseif(LDS) then
            krec=isrec
            arec='_DREC'
         endif
         if(krec .GE. 1000000) then
            lenrec=7
         elseif(krec .GE. 100000) then
            lenrec=6
         elseif(krec .GE. 10000) then
            lenrec=5
         elseif(krec .GE. 1000) then
            lenrec=4
         elseif(krec .GE. 100) then
            lenrec=3
         elseif(krec .GE. 10) then
            lenrec=2
         else
            lenrec=1
         endif
      endif
c --- Make string
      ctrail(1:1)='_'
      ipos=1+lenspec
      ctrail(2:ipos)=aspec(1:lenspec)
      ipos=ipos+1
      ctrail(ipos:ipos)='_'
      ipos=ipos+1
      ipos2=ipos+lenpd-1
      write(ctrail(ipos:ipos2),afmt(lenpd)) iavgpd
      ipos=ipos2+1
      if(avgper.EQ.'  HOUR') then
         ipos2=ipos2+2
         ctrail(ipos:ipos2)='HR'
      elseif(avgper.EQ.'MINUTE') then
         ipos2=ipos2+3
         ctrail(ipos:ipos2)='MIN'
      elseif(avgper.EQ.'SECOND') then
         ipos2=ipos2+3
         ctrail(ipos:ipos2)='SEC'
      endif
      if(lenrec .GT. 0) then
         ipos=ipos2+1
         ipos2=ipos2+5
         ctrail(ipos:ipos2)=arec
         ipos=ipos2+1
         ipos2=ipos2+lenrec
         write(ctrail(ipos:ipos2),afmt(lenrec)) krec
      endif
      ipos=ipos2+1
      ipos2=ipos2+5
      if(ilayer .EQ. -1) then
         ctrail(ipos:ipos2)='_DFLX'
      elseif(ilayer .EQ. -2) then
         ctrail(ipos:ipos2)='_WFLX'
      elseif(ilayer .EQ. -3) then
         ctrail(ipos:ipos2)='_TFLX'
      else
         ctrail(ipos:ipos2)='_CONC'
      endif
      ipos=ipos2+1
      ipos2=ipos2+4
      if(LGRD) then
         ctrail(ipos:ipos2)='.GRD'
      else
         ctrail(ipos:ipos2)='.DAT'
      endif
c --- Total length
      lentrail=ipos2      

c --- Set full pathname for plot-file
      if(npath.GT.0) fname(1:npath)=plpath(1:npath)
      ipos=npath+1
      ipos2=npath+31
      fname(ipos:ipos2)=cdatetm
      ipos=ipos2+1
      ipos2=ipos2+lentrail
      fname(ipos:ipos2)=ctrail
      do i=ipos2+1,132
         fname(i:i)=' '
      enddo

c --- Enforce case
      call FILCASE(lcfiles,fname)

c --- Open file
      open(mapu,file=fname)

      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c ---    Insert 'null' value at excluded receptors
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(ngrecp(i,j).EQ.0) then
                  cgp(i,j)=anull
               else
                  cgp(i,j)=cg(i,j)
               endif
            enddo
         enddo
      endif

      if(LGRD .AND. LG .AND. msrc.NE.2) then
c ---    Write data at gridded receptors in GRID format
c -----------------------------------------------------
c ---    Scan values for max and min
         vmin=cgp(ibgrid,jbgrid)
         vmax=vmin
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(cgp(i,j).GT.vmax) vmax=cgp(i,j)
               if(cgp(i,j).LT.vmin) vmin=cgp(i,j)
            enddo
         enddo
c ---    Nudge vmin if field is planar
         if(vmin.EQ.vmax) then
            if(vmin.EQ.0.0) then
               vmin=-.00001
            else
               vmin=.99999*vmin
            endif
         endif
c ---    Set grid cell range
         nx=iegrid-ibgrid+1
         ny=jegrid-jbgrid+1
c ---    Header lines
         write(mapu,'(a4)') 'DSAA'
         write(mapu,'(2i12)') nx,ny
         write(mapu,'(2f12.3)') xgrd(ibgrid,jbgrid),xgrd(iegrid,jegrid)
         write(mapu,'(2f12.3)') ygrd(ibgrid,jbgrid),ygrd(iegrid,jegrid)
         write(mapu,'(2e12.4)') vmin,vmax
c ---    Data, in rows of constant Y
         do j=jbgrid,jegrid
           write(mapu,'(10000(1pe10.4,2x))') (cgp(i,j),i=ibgrid,iegrid)
         enddo
c ---    Add blank lines before extra annotation
         do k=1,5
            write(mapu,*)
         enddo
      endif

c --- Write information to tail of GRID file, or Head of DATA file
c --- Set title for table
      if(msrc.NE.2) then
         write(mapu,100) iavgpd,avgper,cdname,units
      else
         write(mapu,101) iavgpd,avgper,cdname,units
      endif
c --- Identify species and level (or deposition)
      write(mapu,90) asplv

      if(.not.LGRD) then
c ---    Write data at ALL receptor types in DATA format
c ------------------------------------------------------
c ---    Write column headings
         if(msrc.NE.2) then
            write(mapu,110)
         else
            if(LGS) then        
               xkm=xgrd(isrec,jsrec)
               ykm=ygrd(isrec,jsrec)
            elseif(LDS) then
               xkm=xrec(isrec)
               ykm=yrec(isrec)
            else
               xkm=-999.
               ykm=-999.
            endif
            write(mapu,91) xkm,ykm
            write(mapu,111)
         endif
         write(mapu,*)

c ---    Section for gridded receptors
         if(LSGRID .AND. LG .AND. msrc.NE.2) then
c ---       ISC plot file format
            do j=jbgrid,jegrid
               do i=ibgrid,iegrid
                  if(ngrecp(i,j).EQ.1) write(mapu,120) xgrd(i,j),
     &                                            ygrd(i,j),cg(i,j)
               enddo
            enddo
         endif
c
c ---    Section for discrete receptors (or sources)
c ---    Source TRACEBACK uses discrete receptor array to store impact of
c ---    each source at ONE designated receptor
         if(msrc.EQ.2) then
            do i=1,nsrc
               write(mapu,120) xsrckm(i),ysrckm(i),cd(i)
            enddo
         elseif(LD .AND. ndrec .NE. 0) then
            do i=1,ndrec
               if(ndrecp(i).EQ.1) write(mapu,120) xrec(i),yrec(i),cd(i)
            enddo
         endif
c
c ---    Section for terrain receptors
         if(LCTSG .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
            do i=1,nctrec
               write(mapu,120) xctr(i),yctr(i),ct(i)
            enddo
         endif
c
      endif

c --- Close file
      close(mapu)
c
      return
90    format(t20,a15,/)
91    format(1x,'RECEPTOR (x,y) km ',2f11.3,/)
100   format(5x,i4,a7,' AVERAGE ',a14,' VALUES ',
     * 'AT EACH RECEPTOR  ',a13/)
101   format(5x,i4,a7,' AVERAGE ',a14,' VALUES ',
     * 'FOR EACH SOURCE   ',a13/)
110   format(5x,'RECEPTOR (x,y) km',6x,'VALUE')
111   format(5x,'SOURCE   (x,y) km',6x,'VALUE')
120   format(2x,2f10.3,5x,(1pe10.4,4x))
      end
c----------------------------------------------------------------------
      subroutine plttopn(irank,npd,cg,cd,ct)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012           PLTTOPN
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Prepares plot-file for one TOPN rank
c
c  UPDATES:
c  V6.1(050915) to V6.11(051012)
c               (DGS) Test for planar GRD (SURFER) output fields, and
c               set min/max in the header slightly different to force
c               SURFER to make a plot with no contours
c  V5.62(040503) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.6(031017) to V5.62(040503)
c               (DGS) Replace filename strings c*70 with c*132
c               (DGS) Update filename content
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990709) to V5.1(990920)
c               (DGS) Set excluded gridded receptors to 'null' value
c               (DGS) Apply LG,LD,LCT filter
c               (DGS) Revise output logicals
c  V5.0(990228b)to V5.1(990709)
c               (DGS) Use AVTIME in label
c  V5.0(981116) to V5.0(990228b)
c               (DGS) Allow for negative values in output format
c  V5.0(980430) to V5.0(981116)
c               (DGS) Allow gridded and discrete receptor subsets
c
c  ARGUMENTS:
c     PASSED:   irank           Nth rank of data in arrays          [i]
c               npd             length of averaging period          [i]
c               c[g,d,c]        "concentration" arrays             [ra]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   FILCASE
c-----------------------------------------------------------------------
c --- Note:  This routine is used for both the TOP-n results and the
c ---        length-of-run results.
c-----------------------------------------------------------------------

      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'filnam.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c
      real cg(mxgx,mxgy),cd(mxdrec),ct(mxctrec)
      real cgp(mxgx,mxgy)
      character*132 fname
      character*5 afmt(10)
      character*6 avgper
c
c --- Set integer format array
      data afmt/'(i1)','(i2)','(i3)','(i4)','(i5)',
     &          '(i6)','(i7)','(i8)','(i9)','(i10)'/

c  Null value for reporting excluded receptor
      data anull/0.0/

c --- Set averaging time information
      if(npd.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(npd.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(npd.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=npd*mavg
         avgper=avtime
      endif

c --- Set filename for plot-file 
c -------------------------------

c --- Set length of species name (left-justified)
      do k=1,12
         if(aspec(k:k) .NE. ' ') lenspec=k
      enddo
c --- Set length of avg time string
      if(iavgpd .GE. 10000) then
         lenpd=5
      elseif(iavgpd .GE. 1000) then
         lenpd=4
      elseif(iavgpd .GE. 100) then
         lenpd=3
      elseif(iavgpd .GE. 10) then
         lenpd=2
      else
         lenpd=1
      endif
c --- Set length of rank string
      if(irank .GE. 10000) then
         lenrk=5
      elseif(irank .GE. 1000) then
         lenrk=4
      elseif(irank .GE. 100) then
         lenrk=3
      elseif(irank .GE. 10) then
         lenrk=2
      else
         lenrk=1
      endif

c --- Path
      if(npath.GT.0) fname(1:npath)=plpath(1:npath)
c --- File type
      ipos=npath+1
      ipos2=npath+5
      fname(ipos:ipos2)='RANK('
c --- Rank in file
      ipos=ipos2+1
      ipos2=ipos2+lenrk
      write(fname(ipos:ipos2),afmt(lenrk)) irank
      ipos=ipos2+1
      ipos2=ipos2+2
      fname(ipos:ipos2)=')_'
c --- Species name
      ipos=ipos2+1
      ipos2=ipos2+lenspec
      fname(ipos:ipos2)=aspec(1:lenspec)
      ipos2=ipos2+1
      fname(ipos2:ipos2)='_'
c --- Averaging time
      ipos=ipos2+1
      ipos2=ipos2+lenpd
      write(fname(ipos:ipos2),afmt(lenpd)) iavgpd
      ipos=ipos2+1
      if(avgper.EQ.'  HOUR') then
         ipos2=ipos2+2
         fname(ipos:ipos2)='HR'
      elseif(avgper.EQ.'MINUTE') then
         ipos2=ipos2+3
         fname(ipos:ipos2)='MIN'
      elseif(avgper.EQ.'SECOND') then
         ipos2=ipos2+3
         fname(ipos:ipos2)='SEC'
      endif
c --- Concentration or flux
      ipos=ipos2+1
      ipos2=ipos2+5
      if(ilayer .EQ. -1) then
         fname(ipos:ipos2)='_DFLX'
      elseif(ilayer .EQ. -2) then
         fname(ipos:ipos2)='_WFLX'
      elseif(ilayer .EQ. -3) then
         fname(ipos:ipos2)='_TFLX'
      else
         fname(ipos:ipos2)='_CONC'
      endif
c --- User characters (optional)
      if(ntunam.GT.0) then
         ipos2=ipos2+1
         fname(ipos2:ipos2)='_'
         ipos=ipos2+1
         ipos2=ipos2+ntunam
         fname(ipos:ipos2)=tunam(1:ntunam)
      endif
c --- Extension
      ipos=ipos2+1
      ipos2=ipos2+4
      if(LGRD) then
         fname(ipos:ipos2)='.GRD'
      else
         fname(ipos:ipos2)='.DAT'
      endif
c --- Fill
      do i=ipos2+1,132
         fname(i:i)=' '
      enddo

c --- Enforce case
      call FILCASE(lcfiles,fname)


c --- Open file
      open(mapu,file=fname)

      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c ---    Insert 'null' value at excluded receptors
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(ngrecp(i,j).EQ.0) then
                  cgp(i,j)=anull
               else
                  cgp(i,j)=cg(i,j)
               endif
            enddo
         enddo
      endif

      if(LGRD .AND. LG .AND. msrc.NE.2) then
c ---    Write data at gridded receptors in GRID format
c -----------------------------------------------------
c ---    Scan values for max and min
         vmin=cgp(ibgrid,jbgrid)
         vmax=vmin
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(cgp(i,j).GT.vmax) vmax=cgp(i,j)
               if(cgp(i,j).LT.vmin) vmin=cgp(i,j)
            enddo
         enddo
c ---    Nudge vmin if field is planar
         if(vmin.EQ.vmax) then
            if(vmin.EQ.0.0) then
               vmin=-.00001
            else
               vmin=.99999*vmin
            endif
         endif
c ---    Set grid cell range
         nx=iegrid-ibgrid+1
         ny=jegrid-jbgrid+1
c ---    Header lines
         write(mapu,'(a4)') 'DSAA'
         write(mapu,'(2i12)') nx,ny
         write(mapu,'(2f12.3)') xgrd(ibgrid,jbgrid),xgrd(iegrid,jegrid)
         write(mapu,'(2f12.3)') ygrd(ibgrid,jbgrid),ygrd(iegrid,jegrid)
         write(mapu,'(2e12.4)') vmin,vmax
c ---    Data, in rows of constant Y
         do j=jbgrid,jegrid
           write(mapu,'(10000(1pe11.4,1x))') (cgp(i,j),i=ibgrid,iegrid)
         enddo
c ---    Add blank lines before extra annotation
         do k=1,5
            write(mapu,*)
         enddo
      endif

c --- Write information to tail of GRID file, or Head of DATA file
c --- Set title
      if(msrc.NE.2) then
         write(mapu,100) iavgpd,avgper,cdname,units
      else
         write(mapu,101) iavgpd,avgper,cdname,units
      endif
c --  Identify species and level (or deposition)
      write(mapu,90) asplv

c --- Finish GRD file, or start main output to DAT file
      if(LGRD .AND. msrc.NE.2) then
         ir=MAX(irank,1)
         write(mapu,'(t10,''RANK   '',i8)') ir
      else
c ---    Write data at ALL receptor types in DATA format
c ------------------------------------------------------
c ---    Write column headings
         if(msrc.NE.2) then
            write(mapu,110)
         else
            if(LGS) then        
               xkm=xgrd(isrec,jsrec)
               ykm=ygrd(isrec,jsrec)
            elseif(LDS) then
               xkm=xrec(isrec)
               ykm=yrec(isrec)
            else
               xkm=-999.
               ykm=-999.
            endif
            write(mapu,91) xkm,ykm
            write(mapu,111)
         endif
         write(mapu,*)

c ---    Section for gridded receptors
         if(LSGRID .AND. LG .AND. msrc.NE.2) then
c ---       ISC plot file format
            do j=jbgrid,jegrid
               do i=ibgrid,iegrid
                  if(ngrecp(i,j).EQ.1) write(mapu,120) xgrd(i,j),
     &                                            ygrd(i,j),cg(i,j)
               enddo
            enddo
         endif
c
c ---    Section for discrete receptors (or sources)
c ---    Source TRACEBACK uses discrete receptor array to store impact of
c ---    each source at ONE designated receptor
         if(msrc.EQ.2) then
            do i=1,nsrc
               write(mapu,120) xsrckm(i),ysrckm(i),cd(i)
            enddo
         elseif(LD .AND. ndrec .NE. 0) then
            do i=1,ndrec
               if(ndrecp(i).EQ.1) write(mapu,120) xrec(i),yrec(i),cd(i)
            enddo
         endif
c
c ---    Section for terrain receptors
         if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
            do i=1,nctrec
               write(mapu,120) xctr(i),yctr(i),ct(i)
            enddo
         endif
c
      endif

c --- Close file
      close(mapu)
c
      return
90    format(t10,a15,/)
91    format(t10,'RECEPTOR (x,y) km ',2f11.3/)
100   format(5x,i4,a7,' AVERAGE ',a14,' VALUES ',
     * 'AT EACH RECEPTOR  ',a13/)
101   format(5x,i4,a7,' AVERAGE ',a14,' VALUES ',
     * 'FOR EACH SOURCE   ',a13/)
110   format(5x,'RECEPTOR (x,y) km',6x,'VALUE')
111   format(5x,'SOURCE   (x,y) km',6x,'VALUE')
120   format(2x,2f10.3,4x,(1pe11.4,3x))
      end
c----------------------------------------------------------------------
      subroutine pltexc(npd,thresh,icg,icd,ict)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012            PLTEXC
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Prepares Exceedance plot-file for one averaging pd
c
c  UPDATES:
c  V6.1(050915) to V6.11(051012)
c               (DGS) Test for planar GRD (SURFER) output fields, and
c               set min/max in the header slightly different to force
c               SURFER to make a plot with no contours
c  V5.62(040503) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.6(031017) to V5.62(040503)
c               (DGS) Replace filename strings c*70 with c*132
c               (DGS) Update filename content
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990709) to V5.1(990920)
c               (DGS) Switch integer*2 arrays to integer (*4)
c               (DGS) Skip excluded gridded receptors in 'DATA' output
c               (DGS) Revise output logicals
c  V5.0(981116) to V5.1(990709)
c               (DGS) Use AVTIME in label
c  V5.0(980430) to V5.0(981116)
c               (DGS) Allow gridded and discrete receptor subsets
c
c  ARGUMENTS:
c     PASSED:   npd             length of averaging period          [i]
c               thresh          theshold value                      [r]
c               ic[g,d,c]       exceedance count arrays            [ia]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   FILCASE
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'filnam.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'

      integer icg(mxgx,mxgy),icd(mxdrec),ict(mxctrec)
      character*132 fname
      character*70 message0

      character*5 afmt(10)
      character*6 avgper

c --- Set integer format array
      data afmt/'(i1)','(i2)','(i3)','(i4)','(i5)',
     &          '(i6)','(i7)','(i8)','(i9)','(i10)'/

c --- Set averaging time information
      if(npd.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(npd.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(npd.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=npd*mavg
         avgper=avtime
      endif

      message0='NUMBER OF AVERAGES   >              '

c --- Set filename for plot-file 
c -------------------------------

c --- Set length of species name (left-justified)
      do k=1,12
         if(aspec(k:k) .NE. ' ') lenspec=k
      enddo
c --- Set length of avg time string
      if(iavgpd .GE. 10000) then
         lenpd=5
      elseif(iavgpd .GE. 1000) then
         lenpd=4
      elseif(iavgpd .GE. 100) then
         lenpd=3
      elseif(iavgpd .GE. 10) then
         lenpd=2
      else
         lenpd=1
      endif

c --- Path
      if(npath.GT.0) fname(1:npath)=plpath(1:npath)
c --- File type
      ipos=npath+1
      ipos2=npath+7
      fname(ipos:ipos2)='EXCEED_'
c --- Species name
      ipos=ipos2+1
      ipos2=ipos2+lenspec
      fname(ipos:ipos2)=aspec(1:lenspec)
      ipos2=ipos2+1
      fname(ipos2:ipos2)='_'
c --- Averaging time
      ipos=ipos2+1
      ipos2=ipos2+lenpd
      write(fname(ipos:ipos2),afmt(lenpd)) iavgpd
      ipos=ipos2+1
      if(avgper.EQ.'  HOUR') then
         ipos2=ipos2+2
         fname(ipos:ipos2)='HR'
      elseif(avgper.EQ.'MINUTE') then
         ipos2=ipos2+3
         fname(ipos:ipos2)='MIN'
      elseif(avgper.EQ.'SECOND') then
         ipos2=ipos2+3
         fname(ipos:ipos2)='SEC'
      endif
c --- Concentration or flux
      ipos=ipos2+1
      ipos2=ipos2+5
      if(ilayer .EQ. -1) then
         fname(ipos:ipos2)='_DFLX'
      elseif(ilayer .EQ. -2) then
         fname(ipos:ipos2)='_WFLX'
      elseif(ilayer .EQ. -3) then
         fname(ipos:ipos2)='_TFLX'
      else
         fname(ipos:ipos2)='_CONC'
      endif
c --- User characters (optional)
      if(nxunam.GT.0) then
         ipos2=ipos2+1
         fname(ipos2:ipos2)='_'
         ipos=ipos2+1
         ipos2=ipos2+nxunam
         fname(ipos:ipos2)=xunam(1:nxunam)
      endif
c --- Extension
      ipos=ipos2+1
      ipos2=ipos2+4
      if(LGRD) then
         fname(ipos:ipos2)='.GRD'
      else
         fname(ipos:ipos2)='.DAT'
      endif
c --- Fill
      do i=ipos2+1,132
         fname(i:i)=' '
      enddo

c --- Enforce case
      call FILCASE(lcfiles,fname)

c --- Open file
      open(mapu,file=fname)


      if(LGRD .AND. LG .AND. msrc.NE.2) then
c ---    Write data at gridded receptors in GRID format
c -----------------------------------------------------
c ---    Scan values for max and min
         ivmin=icg(ibgrid,jbgrid)
         ivmax=ivmin
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(icg(i,j).GT.ivmax) ivmax=icg(i,j)
               if(icg(i,j).LT.ivmin) ivmin=icg(i,j)
            enddo
         enddo
c ---    Nudge ivmin if field is planar
         if(ivmin.EQ.ivmax) then
            if(ivmin.EQ.0) then
               ivmin=-1
            else
               ivmin=ivmin-1
            endif
         endif
c ---    Set grid cell range
         nx=iegrid-ibgrid+1
         ny=jegrid-jbgrid+1
c ---    Header lines
         write(mapu,'(a4)') 'DSAA'
         write(mapu,'(2i12)') nx,ny
         write(mapu,'(2f12.3)') xgrd(ibgrid,jbgrid),xgrd(iegrid,jegrid)
         write(mapu,'(2f12.3)') ygrd(ibgrid,jbgrid),ygrd(iegrid,jegrid)
         write(mapu,'(2i7)') ivmin,ivmax
c ---    Data, in rows of constant Y
         do j=jbgrid,jegrid
            write(mapu,'(10000(i6,2x))') (icg(i,j),i=ibgrid,iegrid)
         enddo
c ---    Add blank lines before extra annotation
         do k=1,5
            write(mapu,*)
         enddo
      endif

c --- Write information to tail of GRID file, or Head of DATA file
c --- Set title
      if(msrc.NE.2) then
         write(mapu,100) iavgpd,avgper,cdname
      else
         write(mapu,101) iavgpd,avgper,cdname
      endif
c --- Identify threshold
      write(mapu,102) message0,thresh,units
c --  Identify species and level (or deposition)
      write(mapu,90) asplv

      if(.not.LGRD .OR. msrc.EQ.2) then
c ---    Write data at ALL receptor types in DATA format
c ------------------------------------------------------
c ---    Write column headings
         if(msrc.NE.2) then
            write(mapu,110)
         else
            if(LGS) then        
               xkm=xgrd(isrec,jsrec)
               ykm=ygrd(isrec,jsrec)
            elseif(LDS) then
               xkm=xrec(isrec)
               ykm=yrec(isrec)
            else
               xkm=-999.
               ykm=-999.
            endif
            write(mapu,91) xkm,ykm
            write(mapu,111)
         endif
         write(mapu,*)

c ---    Section for gridded receptors
         if(LSGRID .AND. LG .AND. msrc.NE.2) then
c ---       ISC plot file format
            do j=jbgrid,jegrid
               do i=ibgrid,iegrid
                  if(ngrecp(i,j).EQ.1) write(mapu,120) xgrd(i,j),
     &                                            ygrd(i,j),icg(i,j)
               enddo
            enddo
         endif
c
c ---    Section for discrete receptors (or sources)
c ---    Source TRACEBACK uses discrete receptor array to store impact of
c ---    each source at ONE designated receptor
         if(msrc.EQ.2) then
            do i=1,nsrc
               write(mapu,120) xsrckm(i),ysrckm(i),icd(i)
            enddo
         elseif(LD .AND. ndrec .NE. 0) then
            do i=1,ndrec
               if(ndrecp(i).EQ.1)write(mapu,120) xrec(i),yrec(i),icd(i)
            enddo
         endif
c
c ---    Section for terrain receptors
         if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
            do i=1,nctrec
               write(mapu,120) xctr(i),yctr(i),ict(i)
            enddo
         endif
c
      endif

c --- Close file
      close(mapu)
c
      return
90    format(t10,a15,/)
91    format(t10,'RECEPTOR (x,y) km ',2f11.3/)
100   format(1x,'COUNTS OF ',i4,a7,' AVERAGE ',a14,' EXCEEDANCES ',
     * 'AT EACH RECEPTOR'/)
101   format(1x,'COUNTS OF ',i4,a7,' AVERAGE ',a14,' EXCEEDANCES ',
     * 'FOR EACH SOURCE'/)
102   format(t10,a25,5x,1pe10.4,2x,a13,/)
110   format(5x,'RECEPTOR (x,y) km',9x,'COUNTS')
111   format(5x,'SOURCE   (x,y) km',9x,'COUNTS')
120   format(2x,2f10.3,5x,(i10,4x))
      end
c-----------------------------------------------------------------------
      subroutine wrimap(npd,tng,tnd,tnt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050915            WRIMAP
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Writes Top-N results to file in form suitable for plots
c
c  UPDATES:
c  V5.62(040503) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.6(031017) to V5.62(040503)
c               (DGS) Replace filename strings c*70 with c*132
c               (DGS) Update filename content
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990709) to V5.1(990920)
c               (DGS) Skip excluded gridded receptors in output
c  V5.0(990228b)to V5.1(990709)
c               (DGS) Use AVTIME in label
c               (DGS) Swap 'nn' for averaging period longer than '99'
c  V5.0(990228a)to V5.0(990228b)
c               (DGS) Allow for negative values in output format
c  V5.0(980430) to V5.0(981116)
c               (DGS) Allow gridded and discrete receptor subsets
c  V5.0(971015) to V5.0(980430)
c               (DGS) Construct file name within subroutine
c  V5.0(971007) to V5.0(971015)
c               (JSS) Gridded receptor order of output reversed to
c                     match ISC convention
c
c  ARGUMENTS:
c     PASSED:   npd             Number of periods in average         [i]
c               tn[g,d,t]       N top concentration averages        [ra]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'filnam.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'source.pst'
c
      real tng(mxgx,mxgy,mxrnk),tnd(mxdrec,mxrnk),tnt(mxctrec,mxrnk)
      character*132 fname
      character*5 afmt(10)
      character*6 avgper

c --- Set integer format array
      data afmt/'(i1)','(i2)','(i3)','(i4)','(i5)',
     &          '(i6)','(i7)','(i8)','(i9)','(i10)'/

c --- Set averaging time information
      if(npd.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(npd.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(npd.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=npd*mavg
         avgper=avtime
      endif

c --- Set filename for plot-file 
c -------------------------------

c --- Set length of species name (left-justified)
      do k=1,12
         if(aspec(k:k) .NE. ' ') lenspec=k
      enddo
c --- Set length of avg time string
      if(iavgpd .GE. 10000) then
         lenpd=5
      elseif(iavgpd .GE. 1000) then
         lenpd=4
      elseif(iavgpd .GE. 100) then
         lenpd=3
      elseif(iavgpd .GE. 10) then
         lenpd=2
      else
         lenpd=1
      endif

c --- Path
      if(npath.GT.0) fname(1:npath)=plpath(1:npath)
c --- File type
      ipos=npath+1
      ipos2=npath+10
      fname(ipos:ipos2)='RANK(ALL)_'
c --- Species name
      ipos=ipos2+1
      ipos2=ipos2+lenspec
      fname(ipos:ipos2)=aspec(1:lenspec)
      ipos2=ipos2+1
      fname(ipos2:ipos2)='_'
c --- Averaging time
      ipos=ipos2+1
      ipos2=ipos2+lenpd
      write(fname(ipos:ipos2),afmt(lenpd)) iavgpd
      ipos=ipos2+1
      if(avgper.EQ.'  HOUR') then
         ipos2=ipos2+2
         fname(ipos:ipos2)='HR'
      elseif(avgper.EQ.'MINUTE') then
         ipos2=ipos2+3
         fname(ipos:ipos2)='MIN'
      elseif(avgper.EQ.'SECOND') then
         ipos2=ipos2+3
         fname(ipos:ipos2)='SEC'
      endif
c --- Concentration or flux
      ipos=ipos2+1
      ipos2=ipos2+5
      if(ilayer .EQ. -1) then
         fname(ipos:ipos2)='_DFLX'
      elseif(ilayer .EQ. -2) then
         fname(ipos:ipos2)='_WFLX'
      elseif(ilayer .EQ. -3) then
         fname(ipos:ipos2)='_TFLX'
      else
         fname(ipos:ipos2)='_CONC'
      endif
c --- User characters (optional)
      if(ntunam.GT.0) then
         ipos2=ipos2+1
         fname(ipos2:ipos2)='_'
         ipos=ipos2+1
         ipos2=ipos2+ntunam
         fname(ipos:ipos2)=tunam(1:ntunam)
      endif
c --- Extension
      ipos=ipos2+1
      ipos2=ipos2+4
      fname(ipos:ipos2)='.DAT'
c --- Fill
      do i=ipos2+1,132
         fname(i:i)=' '
      enddo

c --- Enforce case
      call FILCASE(lcfiles,fname)


c --- Open file
      open(mapu,file=fname)
c
c --- Set title for table
      if(msrc.NE.2) then
         write(mapu,100) ntop,iavgpd,avgper,cdname
c ---    Identify species and level (or deposition)
         write(mapu,90) asplv,units
c ---    Write column headings
         write(mapu,110)(itop(k),k=1,ntop)
         write(mapu,*)
      else
         if(LGS) then        
            xkm=xgrd(isrec,jsrec)
            ykm=ygrd(isrec,jsrec)
         elseif(LDS) then
            xkm=xrec(isrec)
            ykm=yrec(isrec)
         else
            xkm=-999.
            ykm=-999.
         endif
         write(mapu,101) ntop,iavgpd,avgper,cdname
c ---    Identify species and level (or deposition)
         write(mapu,90) asplv,units
         write(mapu,91) xkm,ykm
c ---    Write column headings
         write(mapu,111)(itop(k),k=1,ntop)
         write(mapu,*)
      endif

c  Section for gridded receptors
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c --- Fill out table
c --- Reverse order to match ISC plot file format (JSS, 10/97)
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(ngrecp(i,j).EQ.1) write(mapu,120) xgrd(i,j),
     &                         ygrd(i,j),(tng(i,j,itop(k)),k=1,ntop)
            enddo
         enddo
      endif
c
c  Section for discrete receptors (or sources)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
c --- Fill out table
         do i=1,nsrc
            write(mapu,120) xsrckm(i),ysrckm(i),
     &                      (tnd(i,itop(k)),k=1,ntop)
         enddo
      elseif(LD .AND. ndrec .NE. 0) then
c --- Fill out table
         do i=1,ndrec
            if(ndrecp(i).EQ.1) write(mapu,120) xrec(i),yrec(i),
     &                                        (tnd(i,itop(k)),k=1,ntop)
         enddo
      endif
c
c  Section for terrain receptors
      if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
c --- Fill out table
         do i=1,nctrec
            write(mapu,120) xctr(i),yctr(i),(tnt(i,itop(k)),k=1,ntop)
         enddo
      endif

c --- Close file
      close(mapu)

      return

90    format(t10,a15,5x,a13/)
91    format(t10,'RECEPTOR (x,y) km ',2f11.3/)
100   format(1x,i1,' RANKED  ',i4,a7,' AVERAGE ',a14,' VALUES ',
     * 'AT EACH RECEPTOR  '/)
101   format(1x,i1,' RANKED  ',i4,a7,' AVERAGE ',a14,' VALUES ',
     * 'FOR EACH SOURCE'/)
110   format(5x,'RECEPTOR (x,y) km',6x,i3,' RANK',6x,i3,' RANK',6x,
     * i3,' RANK',6x,i3,' RANK'/)
111   format(5x,'SOURCE   (x,y) km',6x,i3,' RANK',6x,i3,' RANK',6x,
     * i3,' RANK',6x,i3,' RANK'/)
120   format(2x,2f10.3,4x,4(1pe11.4,3x))
      end
c-----------------------------------------------------------------------
      subroutine wrimapr(ichan,navper,tng,tnd,tnt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050915           WRIMAPR
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Writes length-of-run to file in form suitable for plots
c
c  UPDATES:
c  V5.6(031017) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990709) to V5.1(990920)
c               (DGS) Skip excluded gridded receptors in output
c  V5.0(990228b)to V5.1(990709)
c               (DGS) Use AVTIME in label
c  V5.0(981116) to V5.0(990228b)
c               (DGS) Allow for negative values in output format
c  V5.0(971015) to V5.0(981116)
c               (DGS) Allow gridded and discrete receptor subsets
c  V5.0(971007) to V5.0(971015)
c               (JSS) Gridded receptor order of output reversed to
c                     match ISC convention
c
c  ARGUMENTS:
c     PASSED:   tn[g,d,t]       length-of-run  averages        [ra]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'source.pst'
c
      real tng(mxgx,mxgy),tnd(mxdrec),tnt(mxctrec)
c
      data ione/1/
      character*6 avgper

c --- Set averaging time information
      if(navper.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(navper.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(navper.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=navper*mavg
         avgper=avtime
      endif
c
c --- Set title for table
      if(msrc.NE.2) then
         write(ichan,100) ione,iavgpd,avgper,cdname
c ---    Identify species and level (or deposition)
         write(ichan,90) asplv,units
c ---    Write column headings
         write(ichan,110) ione
         write(ichan,*)
      else
         if(LGS) then        
            xkm=xgrd(isrec,jsrec)
            ykm=ygrd(isrec,jsrec)
         elseif(LDS) then
            xkm=xrec(isrec)
            ykm=yrec(isrec)
         else
            xkm=-999.
            ykm=-999.
         endif
         write(ichan,101) ione,iavgpd,avgper,cdname
c ---    Identify species and level (or deposition)
         write(ichan,90) asplv,units
         write(ichan,91) xkm,ykm
c ---    Write column headings
         write(ichan,111) ione
         write(ichan,*)
      endif

c  Section for gridded receptors
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c --- Reverse order to match ISC plot file format (JSS, 10/97)
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(ngrecp(i,j).EQ.1) write(ichan,120) xgrd(i,j),
     &                                          ygrd(i,j),tng(i,j)
            enddo
         enddo
      endif
c
c  Section for discrete receptors (or sources)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            write(ichan,120) xsrckm(i),ysrckm(i),tnd(i)
         enddo
      elseif(LD .AND. ndrec .NE. 0) then
         do i=1,ndrec
            if(ndrecp(i).EQ.1) write(ichan,120) xrec(i),yrec(i),tnd(i)
         enddo
      endif
c
c  Section for terrain receptors
      if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
         do i=1,nctrec
            write(ichan,120) xctr(i),yctr(i),tnt(i)
         enddo
      endif
c
      return
90    format(t10,a15,5x,a13/)
91    format(t10,'RECEPTOR (x,y) km ',2f11.3/)
100   format(1x,i1,' RANKED  ',i4,a7,' AVERAGE ',a14,' VALUES ',
     * 'AT EACH RECEPTOR  '/)
101   format(1x,i1,' RANKED  ',i4,a7,' AVERAGE ',a14,' VALUES ',
     * 'FOR EACH SOURCE'/)
110   format(5x,'RECEPTOR (x,y) km',6x,i3,' RANK',6x,i3,' RANK',6x,
     * i3,' RANK',6x,i3,' RANK'/)
111   format(5x,'SOURCE   (x,y) km',6x,i3,' RANK',6x,i3,' RANK',6x,
     * i3,' RANK',6x,i3,' RANK'/)
120   format(2x,2f10.3,4x,4(1pe11.4,3x))
      end
c-----------------------------------------------------------------------
      subroutine wrimapx(ichan,navper,ixg,ixd,ixt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050915           WRIMAPX
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Writes exceedance counts file in form suitable for plots
c
c  UPDATES:
c  V5.6(031017) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990709) to V5.1(990920)
c               (DGS) Switch integer*2 arrays to integer (*4)
c               (DGS) Skip excluded gridded receptors in output
c  V5.0(981116) to V5.1(990709)
c               (DGS) Use AVTIME in label
c  V5.0(971015) to V5.0(981116)
c               (DGS) Allow gridded and discrete receptor subsets
c  V5.0(971007) to V5.0(971015)
c               (JSS) Gridded receptor order of output reversed to
c                     match ISC convention
c
c  ARGUMENTS:
c     PASSED:   ix[g,d,t]       exceedance counts              [ia]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'source.pst'
c
      integer ixg(mxgx,mxgy),ixd(mxdrec),ixt(mxctrec)

      character*6 avgper

c --- Set averaging time information
      if(navper.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(navper.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(navper.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=navper*mavg
         avgper=avtime
      endif
c
c -- Set title for table
      if(msrc.NE.2) then
         write(ichan,100) iavgpd,avgper,cdname
c ---    Identify species and level (or deposition)
         write(ichan,90) asplv
c ---    Write column headings
         write(ichan,110)
         write(ichan,*)
      else
         if(LGS) then        
            xkm=xgrd(isrec,jsrec)
            ykm=ygrd(isrec,jsrec)
         elseif(LDS) then
            xkm=xrec(isrec)
            ykm=yrec(isrec)
         else
            xkm=-999.
            ykm=-999.
         endif
         write(ichan,101) iavgpd,avgper,cdname
c ---    Identify species and level (or deposition)
         write(ichan,90) asplv
         write(ichan,91) xkm,ykm
c ---    Write column headings
         write(ichan,111)
         write(ichan,*)
      endif

c  Section for gridded receptors
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c --- Reverse order to match ISC plot file format (JSS, 10/97)
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(ngrecp(i,j).EQ.1) write(ichan,120) xgrd(i,j),
     &                                          ygrd(i,j),ixg(i,j)
            enddo
         enddo
      endif
c
c  Section for discrete receptors
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            write(ichan,120) xsrckm(i),ysrckm(i),ixd(i)
         enddo
      elseif(LD .AND. ndrec .NE. 0) then
         do i=1,ndrec
            if(ndrecp(i).EQ.1) write(ichan,120) xrec(i),yrec(i),ixd(i)
         enddo
      endif
c
c  Section for terrain receptors
      if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
         do i=1,nctrec
            write(ichan,120) xctr(i),yctr(i),ixt(i)
         enddo
      endif
c
      return
90    format(t10,a15,/)
91    format(t10,'RECEPTOR (x,y) km ',2f11.3/)
100   format(1x,'COUNTS OF ',i4,a7,' AVERAGE ',a14,' EXCEEDENCES ',
     * 'AT EACH RECEPTOR'/)
101   format(1x,'COUNTS OF ',i4,a7,' AVERAGE ',a14,' EXCEEDENCES ',
     * 'FOR EACH SOURCE'/)
110   format(5x,'RECEPTOR (x,y) km',9x,'COUNTS')
111   format(5x,'SOURCE   (x,y) km',9x,'COUNTS')
120   format(2x,2f10.3,5x,(i10,4x))
      end
c----------------------------------------------------------------------
      subroutine wrt(form1,form2,jj,iout,sign,n,iou)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 890515               WRT
c ---           D. Strimaitis, SRC
c
c *** Adapted (REAL ARRAYS ONLY; DATE OPTION REMOVED) from :
c
c --- CALGRID    Version:  0.3     Level:  890131    WRT
c     J. Scire, SRC
c-----------------------------------------------------------------------
c
c --- Write one Y row of gridded data
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
c                IOU - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT called by:  OUT(GRD)
c --- WRT calls:      none
c----------------------------------------------------------------------
      integer iout(n)
      character*1 sign(n)
      character*24 form1
      character*21 form2
c
      write(iou,form1)jj,iout
      write(iou,form2)sign
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrt2(form,n1,n2,iou)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 890515              WRT2
c ---           D. Strimaitis, SRC
c
c *** Adapted (REAL ARRAYS ONLY; DATE OPTION REMOVED) from :
c --- CALGRID    Version:  0.3     Level:  890131    WRT2
c     J. Scire, SRC
c-----------------------------------------------------------------------
c
c --- Write a line labeling grid cell numbers
c
c --- INPUTS:
c               FORM - Char.*18    - Format field of data to be printed
c                 N1 - Integer     - Starting grid cell number
c                 N2 - Integer     - Ending grid cell number
c                IOU - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT2 called by:  OUT(GRD)
c --- WRT2 calls:      none
c-----------------------------------------------------------------------
      character*18 form
c
      write(iou,form)(i,i=n1,n2)
      return
      end
c-----------------------------------------------------------------------
      subroutine wriexc(ichan,navper,thresh,ixg,ixd,ixt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050915            WRIEXC
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Writes Exceedance Count arrays to output file
c
c  UPDATES:
c  V5.6(031017) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.2(991104c) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990920) to V5.2(991104c)
c               (DGS) Add RING processing
c  V5.1(990709) to V5.1(990920)
c               (DGS) Switch integer*2 arrays to integer (*4)
c  V5.0(981116) to V5.1(990709)
c               (DGS) Use AVTIME in label
c  V5.0(971015) to V5.0(981116)
c               (DGS) Allow gridded and discrete receptor subsets
c
c  ARGUMENTS:
c     PASSED:   thresh          threshold value                      [r]
c               ix[g,d,t]       exceedance counts                   [ia]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   OUTIFX
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'ring.pst'
      INCLUDE 'source.pst'
c
      integer ixg(mxgx,mxgy),ixd(mxdrec),ixt(mxctrec)
      integer ixr(mxring)
      character*70 messag0
      character*6 avgper
c
      messag0='NUMBER OF AVERAGES   >              '

c --- Set averaging time information
      if(navper.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(navper.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(navper.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=navper*mavg
         avgper=avtime
      endif
c
c  Initialize ring output
      if(LDRING) then
         do i=1,ndring
            ixr(i)=0
         enddo
      endif
c
c  Section for gridded receptors
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c -- Identify species and level (or deposition)
         write(ichan,90) asplv,units
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,100) iavgpd,avgper,cdname
c -- Create maps
         write(messag0(25:35),'(e10.4)') thresh
         call OUTIFX(ixg,4,messag0,ibgrid,jbgrid,iegrid,jegrid,ichan)
      endif
c
c  Section for discrete receptors (or sources)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
c -- Set title for table
         if(LGS) then        
            xkm=xgrd(isrec,jsrec)
            ykm=ygrd(isrec,jsrec)
         elseif(LDS) then
            xkm=xrec(isrec)
            ykm=yrec(isrec)
         else
            xkm=-999.
            ykm=-999.
         endif
         write(ichan,*) ' '
         write(ichan,201) iavgpd,avgper,cdname,xkm,ykm
         write(ichan,101) thresh,asplv,units
c -- Write column headings
         write(ichan,111)
c -- Fill out table
         do i=1,nsrc
            write(ichan,221) csource(i),xsrckm(i),ysrckm(i),ixd(i)
         enddo
      elseif(LD .AND. ndrec .NE. 0) then
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,200) iavgpd,avgper,cdname
         write(ichan,101) thresh,asplv,units
c -- Write column headings
         write(ichan,110)
c -- Fill out table
         do i=1,ndrec
            if(ndrecp(i).EQ.1) write(ichan,220) i,xrec(i),yrec(i),ixd(i)
         enddo
      endif
c
c  Section for discrete receptors reported by RING
      if(LD .AND. ndrec.NE.0 .AND. LDRING .AND. msrc.NE.2) then
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,400) iavgpd,avgper,cdname
         write(ichan,101) thresh,asplv,units
c -- Write column headings
         write(ichan,410)
c -- Search for max on each ring
         do i=1,ndrec
            if(ndrecp(i).EQ.1) then
               if(ixd(i).GT.ixr(idring(i))) ixr(idring(i))=ixd(i)
            endif
         enddo
c -- Fill out table
         do i=1,ndring
            if(nrrecp(i).EQ.1) write(ichan,420) i,rradkm(i),ixr(i)
         enddo
      endif
c
c  Section for terrain receptors
      if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,300) iavgpd,avgper,cdname
         write(ichan,101) thresh,asplv,units
c -- Write column headings
         write(ichan,110)
c -- Fill out table
         do i=1,nctrec
            write(ichan,220) i,xctr(i),yctr(i),ixt(i)
         enddo
      endif
c
      return
90    format(t57,a15,5x,a13/)
100   format(1x,'COUNTS OF ',i4,a7,' AVERAGE ',a14,' EXCEEDENCES ',
     * 'AT EACH GRIDDED RECEPTOR  '/)
101   format(10x,'THRESHOLD VALUE = ',1pe10.4,t57,a15,5x,a13/)
110   format(9x,'RECEPTOR',3x,'COORDINATES (km)',12X,'COUNT')
111   format(9x,'SOURCE  ',5x,'COORDINATES (km)',12X,'COUNT')
200   format(1x,'COUNTS OF ',i4,a7,' AVERAGE ',a14,' EXCEEDENCES ',
     * 'AT EACH DISCRETE RECEPTOR  '/)
201   format(1x,'COUNTS OF ',i4,a7,' AVERAGE ',a14,' EXCEEDENCES ',
     * 'FOR EACH SOURCE FOR RECEPTOR (x,y) km ',2f12.3/)
220   format(7x,i7,3x,2f10.3,6x,i10)
221   format(1x,a16,2x,2f10.3,6x,i10)
300   format(10x,'COUNTS OF ',i4,a7,' AVERAGE ',a14,' EXCEEDENCES ',
     * 'AT EACH TERRAIN RECEPTOR  '/)
400   format(1x,'COUNTS OF ',i4,a7,' AVERAGE ',a14,' EXCEEDENCES ',
     * 'FOR PEAK RECEPTOR ON EACH RING '/)
410   format(9x,'RING ID ',3x,'     RADIUS (km)',12X,'COUNT')
420   format(7x,i7,'R',7x,f10.3,11x,i10)
      end
c-----------------------------------------------------------------------
      subroutine writ50(ichan,navper,t50c,it50,ct50,t50xy,t50b,t50h)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 061218            WRIT50
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Writes Top-50 arrays to output file
c
c  UPDATES:
c  V6.11(051012) to V6.142(061218)
c               (DGS) Add NO2 (Gas) to HLABEL
c  V6.1(050915) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c  V5.636(050218) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.6(031017) to V5.636(050218)
c               (CEC) Format 130 and 140 were updated to accomodate
c                     Background extinction larger than 9999.999
c  V5.2(991104a) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990709)to V5.2(991104a)
c               (DGS) Add Elemental Carbon as a modeled species
c  V5.0(990228b)to V5.1(990709)
c               (DGS) Use AVTIME in label, expand hour (HH) to include
c                     minutes (MM), and use YYYY year
c  V5.0(981025) to V5.0(990228b)
c               (DGS) Allow for negative values in output format
c  V5.0(980430) to V5.0(981025)
c               (DGS) Revised visibility
c
c  ARGUMENTS:
c     PASSED:   ichan           channel number for output file       [i]
c               navper          number of periods in conc average    [i]
c               t50c(50)        50 top concentration averages       [ra]
c               it50(50,5)      Yr,day,time,(ix,iy):(irec) for above[ia]
c                               where time is (HHMM) starting
c               ct50(50)        Receptor type [g,d,t]               [ca]
c               t50xy(50,2)     x,y coordinates of receptors        [ra]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'visb.pst'
      INCLUDE 'source.pst'
c
      real t50c(50)
      real t50b(50)
      real t50h(50)
      real t50xy(50,2)
      integer it50(50,5)
      character*1 ct50(50)
      character*9 hlabel
      parameter(epsilon=1.e-30)
c
      character*6 avgper

c --- Set averaging time information
      if(navper.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(navper.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(navper.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=navper*mavg
         avgper=avtime
      endif
c
c  Identify species and level (or deposition)
      write(ichan,90) asplv
      if(msrc.EQ.2) then
         if(LGS) then        
            xkm=xgrd(isrec,jsrec)
            ykm=ygrd(isrec,jsrec)
         elseif(LDS) then
            xkm=xrec(isrec)
            ykm=yrec(isrec)
         else
            xkm=-999.
            ykm=-999.
         endif
         write(ichan,91) xkm,ykm
      endif
      if(.not.LVISIB)then
c
c  Set title for table
        write(ichan,100) iavgpd,avgper,cdname,units
c
        if(msrc.NE.2) then
c ---      For receptors
           write(ichan,110) cdname
           do i=1,50
c
c ---      Scale to USER output units (not applicable to vis.)
              t50c(i)=t50c(i)*rscale
c
              write(ichan,120) (it50(i,j),j=1,5),ct50(i),
     &                         t50c(i),(t50xy(i,k),k=1,2)
           enddo
        else
c ---      For sources
           write(ichan,111) cdname
           do i=1,50
c
c ---      Scale to USER output units (not applicable to vis.)
              t50c(i)=t50c(i)*rscale
c
              write(ichan,121) (it50(i,j),j=1,3),csource(it50(i,5)),
     &                         t50c(i),(t50xy(i,k),k=1,2)
           enddo
        endif
c
      else
c
        hlabel='________ '
        if(LVBK)  hlabel(1:1)='B'
        if(LVOC)  hlabel(2:2)='O'
        if(LVEC)  hlabel(3:3)='E'
        if(LVSO4) hlabel(4:4)='S'
        if(LVNO3) hlabel(5:5)='N'
        if(LVPMC) hlabel(6:6)='C'
        if(LVPMF) hlabel(7:7)='F'
        if(LVNO2) hlabel(8:8)='G'
c  Set title for table
        write(ichan,100) iavgpd,avgper,cdname
c  Write column headings
        write(ichan,130) hlabel
c  Fill out table
        do  i=1,50
           if(t50b(i).gt.epsilon)then
              bmdl=t50c(i)
              if(LVBK) bmdl=bmdl-t50b(i)
              ratio=bmdl/t50b(i)
              pchng=100.*ratio
              deltadv=10*alog(1+ratio)
           else
              pchng=-1
              deltadv=-1
           endif
           write(ichan,140) (it50(i,j),j=1,5),ct50(i),t50c(i),t50b(i),
     &                      pchng,deltadv,t50h(i),(t50xy(i,k),k=1,2)
        enddo
c
      endif
c
      return
90    format(t28,a15,/)
91    format(t18,'RECEPTOR (x,y) km ',2f11.3/)
100   format(13x,'TOP-50 ',i4,a7,' AVERAGE ',a14,' VALUES  ',a13/)
110   format(4x,'STARTING YEAR  DAY TIME(HHMM) RECEPTOR  TYPE  ',a14,
     &       '   COORDINATES (km)',/)
111   format(4x,'STARTING YEAR  DAY TIME(HHMM)   SOURCE        ',a14,
     &       '   COORDINATES (km)',/)
120   format(13x,i4,2x,i3,4x,i4.4,4x,'(',i3,',',i4,')',2x,a1,4x,
     &       1pe11.4,2x,0p,2f10.3)
121   format(13x,i4,2x,i3,4x,i4.4,4x,a16,1x,
     &       1pe11.4,2x,0p,2f10.3)
130   format(1x,/,'STARTING',t30,'BEXT   BEXT',/,
     &       'YEAR DAY TIME RECEPTOR TYPE',a11,
     &       '  BACKGND  %CHNG  DELTA DV  RH-FAC',
     &       '   COORDINATES (km)',/)
140   format(i4,1x,i3,1x,i4.4,1x,'(',i3,',',i4,')',1x,a1,1x,2f10.3,
     &       1x,f7.3,3x,f7.3,2x,f6.3,2f10.3)
      end
c-----------------------------------------------------------------------
      subroutine writopn(ichan,navper,tng,tnd,tnt,ng,nd,nt)
c     subroutine writopn(ichan,navper,tng,tnd,tnt,ng,nd,nt,
c    *                   tngb,tndb,tntb,tngh,tndh,tnth)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012           WRITOPN
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Writes Top-N arrays to output file
c
c  UPDATES:
c  V6.1(050915) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c  V5.6(031017) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990806) to V5.1(990920)
c               (DGS) Skip excluded gridded receptors in list output;
c                     and set null value for grid output
c  V5.1(990709) to V5.1(990806)
c               (DGS) Use 2-D variable for date/time: 1=YYYYJJJ, 2=HHMM
c  V5.0(990228b)to V5.1(990709)
c               (DGS) Use AVTIME in label and expand hour (HH) to
c                     include minutes (MM)
c  V5.0(981025) to V5.0(990228b)
c               (DGS) Allow for negative values in output format
c  V5.0(980515) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c               (DGS) Process sub-range of discrete receptors
c  V5.0(980430) to V5.0(980515)
c               (DGS) Increase i,j index format from i2 to i3
c               (DGS) Drop errant "else" from discrete receptor section
c
c  ARGUMENTS:
c     PASSED:   tn[g,d,t]       N top concentration averages        [ra]
c               n[g,d,t]        YYYYJJJ;HHMM for above              [ia]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   OUTGRD
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'source.pst'
c
      real tng(mxgx,mxgy,mxrnk),tnd(mxdrec,mxrnk),tnt(mxctrec,mxrnk)
c --------------------------------------------------------------------
c     real tngb(mxgx,mxgy,mxrnk),tndb(mxdrec,mxrnk),tntb(mxctrec,mxrnk)
c     real tngh(mxgx,mxgy,mxrnk),tndh(mxdrec,mxrnk),tnth(mxctrec,mxrnk)
c --------------------------------------------------------------------
      real tngtemp(mxgx,mxgy)
      integer ng(2,mxgx,mxgy,mxrnk),nd(2,mxdrec,mxrnk),
     &        nt(2,mxctrec,mxrnk)
      character*70 messag(4),messag0
      character*7 atime0
      character*4 atime1
      character*15 atime(mxtop),atime2

      data atime2/'(    ,   ,    )'/
      data anull/0.0/

c
c STILL NEED TO MODIFY FOR VISIBILITY SPECIES
c

      character*6 avgper

c --- Set averaging time information
      if(navper.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(navper.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(navper.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=navper*mavg
         avgper=avtime
      endif

      messag0='    - RANK HIGHEST VALUES FOR PERIOD'
c
c  Section for gridded receptors
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c -- Identify species and level (or deposition)
         write(ichan,90) asplv
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,100) ntop,iavgpd,avgper,cdname,units
c -- Write column headings
         write(ichan,110)(itop(k),k=1,ntop)
c -- Fill out table
         do i=ibgrid,iegrid
           do j=jbgrid,jegrid
             if(ngrecp(i,j).EQ.1) then
               do k=1,ntop
                  write(atime0,'(i7.7)') ng(1,i,j,itop(k))
                  write(atime1,'(i4.4)') ng(2,i,j,itop(k))
                  atime2(2:5)=atime0(1:4)
                  atime2(7:9)=atime0(5:7)
                  atime2(11:14)=atime1(1:4)
                  atime(k)=atime2
               enddo
               write(ichan,120) i,j,xgrd(i,j),ygrd(i,j),
     &                          (tng(i,j,itop(k)),atime(k),
     &                           k=1,ntop)
             endif
           enddo
         enddo
c -- Create maps
         do k=1,ntop
            write(messag0(1:3),'(i3)') itop(k)
            messag(k)=messag0
            do i=ibgrid,iegrid
               do j=jbgrid,jegrid
                  if(ngrecp(i,j).EQ.0) then
                     tngtemp(i,j)=anull
                  else
                     tngtemp(i,j)=tng(i,j,itop(k))
                  endif
               enddo
            enddo
            call OUTGRD(tngtemp,5,messag(k),ibgrid,jbgrid,
     &                  iegrid,jegrid,ichan)
         enddo
      endif
c
c  Section for discrete receptors (or sources)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
c -- Identify species and level (or deposition)
         write(ichan,90) asplv
         if(LGS) then        
            xkm=xgrd(isrec,jsrec)
            ykm=ygrd(isrec,jsrec)
         elseif(LDS) then
            xkm=xrec(isrec)
            ykm=yrec(isrec)
         else
            xkm=-999.
            ykm=-999.
         endif
         write(ichan,91) xkm,ykm
c -- Write column headings
c -- Set title for table
         write(ichan,201) ntop,iavgpd,avgper,cdname,units
         write(ichan,111) (itop(k),k=1,ntop)
c -- Fill out table
         do i=1,nsrc
            do k=1,ntop
               write(atime0,'(i7.7)') nd(1,i,itop(k))
               write(atime1,'(i4.4)') nd(2,i,itop(k))
               atime2(2:5)=atime0(1:4)
               atime2(7:9)=atime0(5:7)
               atime2(11:14)=atime1(1:4)
               atime(k)=atime2
            enddo
            write(ichan,221) csource(i),xsrckm(i),ysrckm(i),
     &                       (tnd(i,itop(k)),atime(k),k=1,ntop)
         enddo
      elseif(LD .AND. ndrec .NE. 0) then
c -- Identify species and level (or deposition)
         write(ichan,90) asplv
c -- Write column headings
c -- Set title for table
         write(ichan,200) ntop,iavgpd,avgper,cdname,units
         write(ichan,110) (itop(k),k=1,ntop)
c -- Fill out table
         do i=1,ndrec
           if(ndrecp(i).EQ.1) then
             do k=1,ntop
                write(atime0,'(i7.7)') nd(1,i,itop(k))
                write(atime1,'(i4.4)') nd(2,i,itop(k))
                atime2(2:5)=atime0(1:4)
                atime2(7:9)=atime0(5:7)
                atime2(11:14)=atime1(1:4)
                atime(k)=atime2
             enddo
             write(ichan,220) i,xrec(i),yrec(i),(tnd(i,itop(k)),
     &                        atime(k),k=1,ntop)
           endif
         enddo
      endif
c
c  Section for terrain receptors
      if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
c -- Identify species and level (or deposition)
         write(ichan,90) asplv
c -- Set title for table
         write(ichan,300) ntop,iavgpd,avgper,cdname,units
c -- Write column headings
         write(ichan,110) (itop(k),k=1,ntop)
c -- Fill out table
         do i=1,nctrec
            do k=1,ntop
               write(atime0,'(i7.7)') nt(1,i,itop(k))
               write(atime1,'(i4.4)') nt(2,i,itop(k))
               atime2(2:5)=atime0(1:4)
               atime2(7:9)=atime0(5:7)
               atime2(11:14)=atime1(1:4)
               atime(k)=atime2
            enddo
            write(ichan,220) i,xctr(i),yctr(i),(tnt(i,itop(k)),
     &                       atime(k),k=1,ntop)
         enddo
      endif
c
      return
90    format(t57,a15,/)
91    format(t44,'RECEPTOR (x,y) km ',2f11.3/)
100   format(1x,i1,' RANKED  ',i4,a7,' AVERAGE ',a14,' VALUES ',
     * 'AT EACH GRIDDED RECEPTOR  (YEAR,DAY,START TIME)   ',a13/)
110   format('RECEPTOR     COORDINATES (km)',11X,i3,' RANK',19x,i3,
     *       ' RANK',19x,i3,' RANK',19x,i3,' RANK')
111   format('   SOURCE',13x,'COORDINATES (km)',11X,i3,' RANK',19x,i3,
     *       ' RANK',19x,i3,' RANK',19x,i3,' RANK')
120   format(1x,i3,',',i3,2x,2f10.3,1x,4(1x,1pe11.4,1x,0p,a15))
200   format(1x,i1,' RANKED  ',i4,a7,' AVERAGE ',A14,' VALUES ',
     * 'AT EACH DISCRETE RECEPTOR  (YEAR,DAY,START TIME)   ',a13/)
201   format(1x,i1,' RANKED  ',i4,a7,' AVERAGE ',A14,' VALUES ',
     * 'FOR EACH SOURCE  (YEAR,DAY,START TIME)   ',a13/)
220   format(1x,i6,3x,2f10.3,1x,4(1x,1pe11.4,1x,0p,a15))
221   format(1x,a16,2x,2f10.3,1x,4(1x,1pe11.4,1x,0p,a15))
300   format(1x,i1,' RANKED  ',i4,a7,' AVERAGE ',A14,' VALUES ',
     * 'AT EACH TERRAIN RECEPTOR  (YEAR,DAY,START TIME)   ',a13/)
      end
c-----------------------------------------------------------------------
      subroutine wrisum(ichan)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012            WRISUM
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Writes title lines for summary section to output file
c
c  UPDATES:
c  V5.6(031017) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c  V5.1(990806) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990709) to V5.1(990806)
c               (DGS) Expand year format to YYYY
c  V5.0(980515) to V5.1(990709)
c               (DGS) Expand hour (HH) to include minutes (MM)
c  V5.0(960628) to V5.0(980515)
c               (DGS) Allow for increase in (i,j) format
c
c  ARGUMENTS:
c     PASSED:   ichan           I/O unit for file                    [i]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'

      character*15 sumry
      data sumry/'SUMMARY SECTION'/

c --- Identify species and level (or deposition)
      write(ichan,90) sumry
      write(ichan,90) asplv
      write(ichan,90) units

c --- Write column headings
      if(msrc.NE.2) then
         write(ichan,110)
      else
         if(LGS) then        
            xkm=xgrd(isrec,jsrec)
            ykm=ygrd(isrec,jsrec)
         elseif(LDS) then
            xkm=xrec(isrec)
            ykm=yrec(isrec)
         else
            xkm=-999.
            ykm=-999.
         endif
         write(ichan,91) xkm,ykm
         write(ichan,111)
      endif

      return

90    format(1x,t57,a15,//)
91    format(1x,t44,'RECEPTOR (x,y) km ',2f11.3//)
110   format(' RECEPTOR     COORDINATES (km)    TYPE   ',3X,'PEAK ',
     *       '(YEAR,DAY,START TIME)      FOR RANK    FOR',
     *       ' AVERAGE PERIOD')
111   format(' SOURCE     COORDINATES (km)    NAME   ',9X,'PEAK ',
     *       '(YEAR,DAY,START TIME)      FOR RANK    FOR',
     *       ' AVERAGE PERIOD')

      end
c-----------------------------------------------------------------------
      subroutine wripeak(ichan,navper,tng,tnd,tnt,ng,nd,nt)
c     subroutine wripeak(ichan,navper,tng,tnd,tnt,ng,nd,nt,
c    *                   tngb,tndb,tntb,tngh,tndh,tnth)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050915           WRIPEAK
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Finds peak values in Top-N arrays and writes summary
c               to output file
c
c  UPDATES:
c  V5.6(031017) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c  V5.2(991104c) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990920) to V5.2(991104c)
c               (DGS) Add RING processing output
c  V5.1(990806) to V5.1(990920)
c               (DGS) Skip excluded gridded receptors
c  V5.1(990709) to V5.1(990806)
c               (DGS) Use 2-D variable for date/time: 1=YYYYJJJ, 2=HHMM
c  V5.0(990228b)to V5.1(990709)
c               (DGS) Use AVTIME in label and expand hour (HH) to
c                     include minutes (MM)
c  V5.0(981025) to V5.0(990228b)
c               (DGS) Allow for negative values in output format
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c  V5.0(890515) to V5.0(980821)
c               (DGS) Process sub-range of discrete receptors
c  V5.0(980430) to V5.0(980515)
c               (DGS) Increase i,j index format from i2 to i3
c
c  ARGUMENTS:
c     PASSED:   ichan           I/O unit number for file             [i]
c               navper          Number of periods in averages        [i]
c               tn[g,d,t]       N top concentration averages        [ra]
c               n[g,d,t]        YYYYJJJ;HHMM for above              [ia]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'ring.pst'
      INCLUDE 'source.pst'
c
      real tng(mxgx,mxgy,mxrnk),tnd(mxdrec,mxrnk),tnt(mxctrec,mxrnk)
c --------------------------------------------------------------------
c     real tngb(mxgx,mxgy,mxrnk),tndb(mxdrec,mxrnk),tntb(mxctrec,mxrnk)
c     real tngh(mxgx,mxgy,mxrnk),tndh(mxdrec,mxrnk),tnth(mxctrec,mxrnk)
c --------------------------------------------------------------------
      real tnr(mxring,mxtop)
      real peak(mxtop),xr(mxtop),yr(mxtop)
      integer ng(2,mxgx,mxgy,mxrnk),nd(2,mxdrec,mxrnk),
     &        nt(2,mxctrec,mxrnk)
      integer nr(2,mxring,mxtop)
      integer ir(mxtop),jr(mxtop)
      character*7 atime0
      character*4 atime1
      character*15 atime(mxtop),atime2

      data atime2/'(    ,   ,    )'/

c  Set initial value for ring arrays
      data vinit/-9.99e25/

c
c STILL NEED TO MODIFY FOR VISIBILITY SPECIES
c
      character*6 avgper

c --- Set averaging time information
      if(navper.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(navper.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(navper.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=navper*mavg
         avgper=avtime
      endif
c
c -- Set off section with a blank line
      write(ichan,*) ' '
c
c  Section for gridded receptors
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c -- Find peak value for each "top-n"
         do k=1,ntop
            ir(k)=0
            jr(k)=0
            atime(k)='(0000,000,0000)'
            xr(k)=0.0
            yr(k)=0.0
            peak(k)=0.0
         enddo
         do i=ibgrid,iegrid
           do j=jbgrid,jegrid
             if(ngrecp(i,j).EQ.1) then
               do k=1,ntop
                 if(tng(i,j,itop(k)).GT.peak(k)) then
                    peak(k)=tng(i,j,itop(k))
                    ir(k)=i
                    jr(k)=j
                    xr(k)=xgrd(i,j)
                    yr(k)=ygrd(i,j)
                    write(atime0,'(i7.7)') ng(1,i,j,itop(k))
                    write(atime1,'(i4.4)') ng(2,i,j,itop(k))
                    atime2(2:5)=atime0(1:4)
                    atime2(7:9)=atime0(5:7)
                    atime2(11:14)=atime1(1:4)
                    atime(k)=atime2
                 endif
               enddo
             endif
           enddo
         enddo
c -- Write the peak values
         do k=1,ntop
            write(ichan,120) ir(k),jr(k),xr(k),yr(k),peak(k),
     &                       atime(k),itop(k),iavgpd,avgper
         enddo
      endif
c
c  Section for discrete receptors (or sources)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
c -- Find peak value for each "top-n"
         do k=1,ntop
            ir(k)=0
            atime(k)='(0000,000,0000)'
            xr(k)=0.0
            yr(k)=0.0
            peak(k)=0.0
         enddo
         do i=1,nsrc
            do k=1,ntop
              if(tnd(i,itop(k)).GT.peak(k)) then
                 peak(k)=tnd(i,itop(k))
                 ir(k)=i
                 xr(k)=xsrckm(i)
                 yr(k)=ysrckm(i)
                 write(atime0,'(i7.7)') nd(1,i,itop(k))
                 write(atime1,'(i4.4)') nd(2,i,itop(k))
                 atime2(2:5)=atime0(1:4)
                 atime2(7:9)=atime0(5:7)
                 atime2(11:14)=atime1(1:4)
                 atime(k)=atime2
              endif
            enddo
         enddo
c -- Write the peak values
         do k=1,ntop
            write(ichan,221) ir(k),xr(k),yr(k),csource(ir(k)),peak(k),
     &                       atime(k),itop(k),iavgpd,avgper
         enddo
c
      elseif(LD .AND. ndrec .NE. 0) then
c -- Find peak value for each "top-n"
         do k=1,ntop
            ir(k)=0
            atime(k)='(0000,000,0000)'
            xr(k)=0.0
            yr(k)=0.0
            peak(k)=0.0
         enddo
         do i=1,ndrec
           if(ndrecp(i).EQ.1) then
             do k=1,ntop
               if(tnd(i,itop(k)).GT.peak(k)) then
                  peak(k)=tnd(i,itop(k))
                  ir(k)=i
                  xr(k)=xrec(i)
                  yr(k)=yrec(i)
                  write(atime0,'(i7.7)') nd(1,i,itop(k))
                  write(atime1,'(i4.4)') nd(2,i,itop(k))
                  atime2(2:5)=atime0(1:4)
                  atime2(7:9)=atime0(5:7)
                  atime2(11:14)=atime1(1:4)
                  atime(k)=atime2
               endif
             enddo
           endif
         enddo
c -- Write the peak values
         do k=1,ntop
            write(ichan,220) ir(k),xr(k),yr(k),peak(k),
     &                       atime(k),itop(k),iavgpd,avgper
         enddo
      endif
c
c  Section for discrete receptors reported by RING
      if(LD .AND. ndrec .NE. 0 .AND. LDRING .AND. msrc.NE.2) then
c -- Initialize ring output
         do k=1,ntop
            do i=1,ndring
               tnr(i,k)=vinit
            enddo
         enddo
c -- Search for peak values on ring
         do i=1,ndrec
            if(ndrecp(i).EQ.1) then
               do k=1,ntop
                  if(tnd(i,itop(k)).GT.tnr(idring(i),itop(k))) then
                     tnr(idring(i),itop(k))=tnd(i,itop(k))
                     nr(1,idring(i),itop(k))=nd(1,i,itop(k))
                     nr(2,idring(i),itop(k))=nd(2,i,itop(k))
                  endif
               enddo
            endif
         enddo
c -- Report peak value for each "top-n" for each ring
         do i=1,ndring
           if(nrrecp(i).EQ.1) then
             do k=1,ntop
                write(atime0,'(i7.7)') nr(1,i,itop(k))
                write(atime1,'(i4.4)') nr(2,i,itop(k))
                atime2(2:5)=atime0(1:4)
                atime2(7:9)=atime0(5:7)
                atime2(11:14)=atime1(1:4)
                atime(k)=atime2
                write(ichan,420) i,rradkm(i),tnr(i,itop(k)),
     &                           atime(k),itop(k),iavgpd,avgper
             enddo
           endif
         enddo
      endif
c
c  Section for terrain receptors
      if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
c -- Find peak value for each "top-n"
         do k=1,ntop
            ir(k)=0
            atime(k)='(0000,000,0000)'
            xr(k)=0.0
            yr(k)=0.0
            peak(k)=0.0
         enddo
         do i=1,nctrec
            do k=1,ntop
               if(tnt(i,itop(k)).GT.peak(k)) then
                  peak(k)=tnt(i,itop(k))
                  ir(k)=i
                  xr(k)=xctr(i)
                  yr(k)=yctr(i)
                  write(atime0,'(i7.7)') nt(1,i,itop(k))
                  write(atime1,'(i4.4)') nt(2,i,itop(k))
                  atime2(2:5)=atime0(1:4)
                  atime2(7:9)=atime0(5:7)
                  atime2(11:14)=atime1(1:4)
                  atime(k)=atime2
               endif
            enddo
         enddo
c -- Write the peak values
         do k=1,ntop
            write(ichan,320) ir(k),xr(k),yr(k),peak(k),
     &                       atime(k),itop(k),iavgpd,avgper
         enddo
      endif
c
      return
120   format(1x,i3,',',i3,2x,2f10.3,'   GRIDDED ',(2x,1pe11.4,1x,0p,
     *       a15),6x,' RANK ',i2,7x,i4,a7)
220   format(1x,i6,3x,2f10.3,'  DISCRETE ',(2x,1pe11.4,1x,0p,
     *       a15),6x,' RANK ',i2,7x,i4,a7)
221   format(1x,i6,2x,2f10.3,2x,a16,(2x,1pe11.4,1x,0p,
     *       a15),6x,' RANK ',i2,7x,i4,a7)
320   format(1x,i6,3x,2f10.3,'    CTSG   ',(2x,1pe11.4,1x,0p,
     *       a15),6x,' RANK ',i2,7x,i4,a7)
420   format(1x,i6,'R',7x,f10.3,5x,'    RING   ',(2x,1pe11.4,1x,0p,
     *       a15),6x,' RANK ',i2,7x,i4,a7)
      end
c-----------------------------------------------------------------------
      subroutine wripeakr(ichan,navper,tng,tnd,tnt)
c     subroutine wripeakr(ichan,navper,tng,tnd,tnt,
c    *                    tngb,tndb,tntb,tngh,tndh,tnth)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017          WRIPEAKR
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Finds peak values in Length-of-Run and writes summary
c               to output file (Modified version of WRIPEAK)
c
c  UPDATES:
c  V5.2(991104c) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.1(990920) to V5.2(991104c)
c               (DGS) Add RING processing output
c  V5.1(990709) to V5.1(990920)
c               (DGS) Skip excluded gridded receptors
c  V5.0(990228b)to V5.1(990709)
c               (DGS) Use AVTIME in label
c  V5.0(981025) to V5.0(990228b)
c               (DGS) Allow for negative values in output format
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c  V5.0(890515) to V5.0(980821)
c               (DGS) Process sub-range of discrete receptors
c  V5.0(980430) to V5.0(980515)
c               (DGS) Increase i,j index format from i2 to i3
c
c  ARGUMENTS:
c     PASSED:   ichan           I/O unit number for file             [i]
c               navper          Number of periods in averages        [i]
c               tn[g,d,t]       Concentration averages              [ra]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'ring.pst'
      INCLUDE 'source.pst'
c
      real tng(mxgx,mxgy),tnd(mxdrec),tnt(mxctrec)
c --------------------------------------------------------------------
c     real tngb(mxgx,mxgy),tndb(mxdrec),tntb(mxctrec)
c     real tngh(mxgx,mxgy),tndh(mxdrec),tnth(mxctrec)
c --------------------------------------------------------------------
      real tnr(mxring)
      real peak,xr,yr
      integer ir,jr

c  Set initial value for ring arrays
      data vinit/-9.99e25/

c
c STILL NEED TO MODIFY FOR VISIBILITY SPECIES
c
      iavgpd=navper*mavg
      itop1=1
c
c -- Set off section with a blank line
      write(ichan,*) ' '
c
c  Section for gridded receptors
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c -- Find peak value
         ir=0
         jr=0
         xr=0.0
         yr=0.0
         peak=0.0
         do i=ibgrid,iegrid
           do j=jbgrid,jegrid
             if(ngrecp(i,j).EQ.1) then
               if(tng(i,j).GT.peak) then
                  peak=tng(i,j)
                  ir=i
                  jr=j
                  xr=xgrd(i,j)
                  yr=ygrd(i,j)
               endif
             endif
           enddo
         enddo
c -- Write the peak value
         write(ichan,120) ir,jr,xr,yr,peak,itop1,iavgpd,avtime
      endif
c
c  Section for discrete receptors (or sources)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
c -- Find peak value
         ir=0
         xr=0.0
         yr=0.0
         peak=0.0
         do i=1,nsrc
            if(tnd(i).GT.peak) then
               peak=tnd(i)
               ir=i
               xr=xsrckm(i)
               yr=ysrckm(i)
            endif
         enddo
c -- Write the peak value
         write(ichan,220) ir,xr,yr,csource(ir),peak,itop1,iavgpd,avtime
c
      elseif(LD .AND. ndrec .NE. 0) then
c -- Find peak value
         ir=0
         xr=0.0
         yr=0.0
         peak=0.0
         do i=1,ndrec
            if(ndrecp(i).EQ.1) then
               if(tnd(i).GT.peak) then
                  peak=tnd(i)
                  ir=i
                  xr=xrec(i)
                  yr=yrec(i)
               endif
            endif
         enddo
c -- Write the peak value
         write(ichan,220) ir,xr,yr,peak,itop1,iavgpd,avtime
      endif
c
c  Section for discrete receptors reported by RING
      if(LD .AND. ndrec .NE. 0 .AND. LDRING .AND. msrc.NE.2) then
c -- Initialize ring output
         do i=1,ndring
            tnr(i)=vinit
         enddo
c -- Search for peak value on rings
         do i=1,ndrec
            if(ndrecp(i).EQ.1) then
               if(tnd(i).GT.tnr(idring(i))) then
                  tnr(idring(i))=tnd(i)
               endif
            endif
         enddo
c -- Write the peak value on each ring
         do i=1,ndring
            if(nrrecp(i).EQ.1) write(ichan,420) i,rradkm(i),tnr(i),
     *                                          itop1,iavgpd,avtime
         enddo
      endif
c
c  Section for terrain receptors
      if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
c -- Find peak value
         ir=0
         xr=0.0
         yr=0.0
         peak=0.0
         do i=1,nctrec
            if(tnt(i).GT.peak) then
               peak=tnt(i)
               ir=i
               xr=xctr(i)
               yr=yctr(i)
            endif
         enddo
c -- Write the peak value
         write(ichan,320) ir,xr,yr,peak,itop1,iavgpd,avtime
      endif
c
      return
120   format(1x,i3,',',i3,2x,2f10.3,'   GRIDDED ',(2x,1pe11.4,1x,0p,
     *       11x),10x,' RANK ',i2,7x,i4,a7)
220   format(1x,i6,3x,2f10.3,'  DISCRETE ',(2x,1pe11.4,1x,0p,
     *       11x),10x,' RANK ',i2,7x,i4,a7)
221   format(1x,i6,2x,2f10.3,2x,a16,(2x,1pe11.4,1x,0p,
     *       11x),10x,' RANK ',i2,7x,i4,a7)
320   format(1x,i6,3x,2f10.3,'    CTSG   ',(2x,1pe11.4,1x,0p,
     *       11x),10x,' RANK ',i2,7x,i4,a7)
420   format(1x,i6,'R',7x,f10.3,5x,'    RING   ',(2x,1pe11.4,1x,0p,
     *       11x),10x,' RANK ',i2,7x,i4,a7)
      end
c-----------------------------------------------------------------------
      subroutine wrivis
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080724            WRIVIS
c ---           D. Strimaitis
c
c  PURPOSE:     Writes title lines for visibility section to output file
c
c  UPDATES:
c  V6.141(061120) to V6.221(080724)
c               (DGS) Move F(RH) column(s) to right side and drop RH
c                     component from run-length
c  V6.14(061107) to V6.141(061120)
c               (DGS) Revise F(RH) column for Method 8
c  V6.132(060519) to V6.14(061107)
c               (DGS) Add NO2 as a modeled species
c  V5.2(991104a) to V6.132(060519)
c               (DGS) Identify hour as starting time
c  V5.1(990709) to V5.2(991104a)
c               (DGS) Add Elemental Carbon as a modeled species
c  V5.0(990228) to V5.1(990709)
c               (DGS) Use 4-digit year
c  V5.0(981025) to V5.0(990228)
c               (DGS) Add 2 scratch files for run-length results
c  V5.0(980821) to V5.0(981025)
c               (DGS) Augment output and send to 2 scratch files
c
c  ARGUMENTS:
c
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'visb.pst'

      character*15 sumry
      character*21 Rsumry
      character*6 frh6, sml6, lrg6, sea6
      data sumry/'24HR VISIBILITY'/
      data Rsumry/'Run-Length VISIBILITY'/
      data frh6/' F(RH)'/,sml6/' Small'/,lrg6/' Large'/,sea6/' SSalt'/

c --- Set up scratch file header for extinction output
c --- Identify visibility components
      write(iox1,90) sumry
      write(iox1,90) asplv
      write(iox1,90) units

c --- Write column headings
      if(mvisbk.EQ.8) then
         write(iox1,221) sml6,lrg6,sea6,frh6,frh6,frh6
      else
         write(iox1,121) frh6
      endif

c --- Set up scratch file header for deciview output
c --- Identify visibility components
      write(iox2,90) sumry
      write(iox2,90) asplv
      write(iox2,90) '   (deciview)  '

c --- Write column headings
      if(mvisbk.EQ.8) then
         write(iox2,222) sml6,lrg6,sea6,frh6,frh6,frh6
      else
         write(iox2,122) frh6
      endif

c --- Set up scratch file header for run-length output (extinction)
c --- Identify visibility components
      write(iox3,91) Rsumry
      write(iox3,90) asplv
      write(iox3,90) units

c --- Write column headings
      write(iox3,123)
c      if(mvisbk.EQ.8) then
c         write(iox3,223) sml6,lrg6,sea6,frh6,frh6,frh6
c      else
c         write(iox3,123) frh6
c      endif

c --- Set up scratch file header for run-length output (deciview)
c --- Identify visibility components
      write(iox4,91) Rsumry
      write(iox4,90) asplv
      write(iox4,90) '   (deciview)  '

c --- Write column headings
      write(iox4,124)
c      if(mvisbk.EQ.8) then
c         write(iox4,224) sml6,lrg6,sea6,frh6,frh6,frh6
c      else
c         write(iox4,124) frh6
c      endif

      return

90    format(1x,t57,a15,//)
91    format(1x,t54,a21,//)

121   format('START TIME',80x,'Modeled Extinction by Species',/,
     &       'YEAR DAY HR  RECEPTOR    COORDINATES (km)  TYPE ',
     &       'BEXT(Model) BEXT(BKG) BEXT(Total) %CHANGE',
     &       '  bxSO4  bxNO3   bxOC   bxEC  bxPMC  bxPMF  bxNO2 ',
     &       a6)
221   format('START TIME',80x,'Modeled Extinction by Species',20x,
     &       3a6,/
     &       'YEAR DAY HR  RECEPTOR    COORDINATES (km)  TYPE ',
     &       'BEXT(Model) BEXT(BKG) BEXT(Total) %CHANGE',
     &       '  bxSO4  bxNO3   bxOC   bxEC  bxPMC  bxPMF  bxNO2 ',
     &       3a6)

122   format('START TIME',70x,'% of Modeled Extinction by Species',/,
     &       'YEAR DAY HR  RECEPTOR    COORDINATES (km)  TYPE ',
     &       ' DV(Total)    DV(BKG)  DELTA DV',
     &       '  %_SO4  %_NO3   %_OC   %_EC  %_PMC  %_PMF  %_NO2 ',
     &       3a6)
222   format('START TIME',70x,'% of Modeled Extinction by Species',
     &       15x,3a6,/
     &       'YEAR DAY HR  RECEPTOR    COORDINATES (km)  TYPE ',
     &       ' DV(Total)    DV(BKG)  DELTA DV',
     &       '  %_SO4  %_NO3   %_OC   %_EC  %_PMC  %_PMF  %_NO2 ',
     &       3a6)

123   format(/,'             RECEPTOR    COORDINATES (km)  TYPE ',
     &       'BEXT(Model) BEXT(BKG) BEXT(Total) %CHANGE ',a6)
223   format(t91,3a6,/,
     &       '             RECEPTOR    COORDINATES (km)  TYPE ',
     &       'BEXT(Model) BEXT(BKG) BEXT(Total) %CHANGE ',3a6)

124   format(/,'             RECEPTOR    COORDINATES (km)  TYPE ',
     &       ' DV(Total)    DV(BKG)  DELTA DV ',a6)
224   format(t81,3a6,/,
     &       '             RECEPTOR    COORDINATES (km)  TYPE ',
     &       ' DV(Total)    DV(BKG)  DELTA DV ',3a6)

      end
c-----------------------------------------------------------------------
      subroutine outvis(navper,tng,tnd,tnt,tngb,tndb,tntb,
     *                tngh,tndh,tnth,tngp,tndp,tntp,myr,mjday,mhr,msec)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080724             OUTVIS
c ---           D. Strimaitis
c
c  PURPOSE:     Computes change in visibility, identifies peak values,
c               and writes summary to scratch output file
c               (Modified version of WRIPEAKR)
c
c  UPDATES:
c  V6.14(061107) to V6.221(080724)
c               (DGS) Move F(RH) column(s) to right side and add new
c                     columns for small/large/seasalt and drop RH
c                     component from run-length
c  V5.633(041202) to V6.14(061107)
c               (DGS) Add NO2 as modeled species
c  V5.2(991104c) to V5.633(041202)
c               (CEC) Add the computation of 24h average percent change
c               in light extinction as the 24haverage of the ratio
c               if LAVER is true.
c  V5.2(991104a) to V5.2(991104c)
c               (DGS) Replace discrete receptor information with
c                     corresponding ring information for LDRING=T
c  V5.2(991104) to V5.2(991104a)
c               (DGS) Add Elemental Carbon as modeled species
c  V5.1(990920) to V5.2(991104)
c               (JSS) Error messages written to list file
c                     in addition to screen
c  V5.1(990709) to V5.1(990920)
c               (DGS) Skip excluded gridded receptors
c  V5.0(990228) to V5.1(990709)
c               (DGS) Use 4-digit year (ignore non-zero seconds)
c  V5.0(981116) to V5.0(990228)
c               (DGS) Add run-length tabulation
c  V5.0(981025) to V5.0(981116)
c               (DGS) Delta-dv reported for each modeled specie rather
c                     than dv
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c               (DGS) Augment output and write to 2 scratch files
c
c  ARGUMENTS:
c     PASSED:   navper          Number of periods in averages        [i]
c               tn[g,d,t]       Concentration averages              [ra]
c               tn[b,h]         Corresponding background ext and    [ra]
c                               RH-factor averages
c               myr,mjday,mhr   Date/time marker for period          [i]
c               msec            Seconds for date/time marker         [i]
c
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   GRDAY
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'ring.pst'
      INCLUDE 'visb.pst'
      INCLUDE 'vispec.pst'
c
      real tng(mxgx,mxgy),tnd(mxdrec),tnt(mxctrec)
      real tngb(mxgx,mxgy),tndb(mxdrec),tntb(mxctrec)
      real tngh(mxgx,mxgy),tndh(mxdrec),tnth(mxctrec)
      real tngp(mxgx,mxgy),tndp(mxdrec),tntp(mxctrec)
      real peak,xr,yr

c  Arrays for storing run length results by ring
      real peakR(mxring),frhR(mxring),bbR(mxring),bmR(mxring)
      real bpR(mxring)

      integer ir,jr
      character*4 rectype,rtype,rdtype
      logical l24
      logical lm8_month

c  Set character string for discrete receptors
      if(LDRING) then
         rdtype='  R '
      else
         rdtype='  D '
      endif

c  Should be 24-hour averages or run-length averages
      iavgpd=navper*mavg
      itest=myr+mjday+mhr
      if(iavgpd.EQ.24) then
         if(itest.EQ.0) then
            l24=.FALSE.
         else
            l24=.TRUE.
         endif
      elseif(itest.EQ.0) then
         l24=.FALSE.
      else
         write(io6,*) 'OUTVIS: must be 24-hour or run-length averages'
         write(io6,*) '        Averages found (hours):  ',iavgpd
         write(*,*) 'OUTVIS: must be 24-hour or run-length averages'
         write(*,*) '        Averages found (hours):  ',iavgpd
         stop
      endif

c  Should be true hourly time (seconds=0)
      if(msec.NE.0) then
         write(io6,*) 'OUTVIS: must be 1-hour periods with seconds = 0'
         write(io6,*) '                 Seconds found:  ',msec
         write(*,*) 'OUTVIS: must be 1-hour periods with seconds = 0'
         write(*,*) '                 Seconds found:  ',msec
         stop
      endif

c  Set initial values
      peak=-99.
      rectype='  N '
      ir=0
      jr=0
      xr=0.0
      yr=0.0
      frh=0.0
      bb=-99.
      bm=-99.
      br=-99.
      bso4=0.0
      bno3=0.0
      bno2=0.0
      boc=0.0
      bec=0.0
      bpmc=0.0
      bpmf=0.0

c  Set initial values for RINGs (run length processing)
      do i=1,ndring
         peakR(i)=-99.
         frhR(i)=0.0
         bbR(i)=-99.
         bmR(i)=-99.
         bpR(i)=-99.
      enddo

c  Set monthly f(RH) small/large/seasalt for 24-hr results
      if(L24) then
         call GRDAY(io6,myr,mjday,imo,iday)
         frhs=rhfsml(imo)
         frhl=rhflrg(imo)
         frhss=rhfsea(imo)
      else
         frhs=0.0
         frhl=0.0
         frhss=0.0
      endif
c  Set switch for monthly f(RH) small/large/seasalt
      if(mvisbk.EQ.8 .AND. (m8_mode.GE.2 .AND. m8_mode.LE.5)) then
         lm8_month=.true.
      else
         lm8_month=.false.
      endif

c  Section for gridded receptors
      if(LSGRID .AND. LG) then
         rtype='  G '
         do i=ibgrid,iegrid
          do j=jbgrid,jegrid
            if(ngrecp(i,j).EQ.1) then
c ---         Set modeled extinction without background
              tmdl=tng(i,j)
              if(tngb(i,j).LE.0.) then
                 deltadv=-999.
              else
                 if(LVBK) tmdl=tmdl-tngb(i,j)
                 if(LAVER) then
                    deltadv=10.*ALOG(1.+tngp(i,j))
                 else
                    deltadv=10.*ALOG(1.+tmdl/tngb(i,j))
                 endif
              endif
              if(L24) then
c ---            Find peak 24-hr value of DELTA-DECIVIEW
                 if(deltadv.GT.peak) then
                    rectype='  G '
                    peak=deltadv
                    ir=i
                    jr=j
                    xr=xgrd(i,j)
                    yr=ygrd(i,j)
                    frh=tngh(i,j)
                    bb=tngb(i,j)
                    bm=tmdl
                    br=tngp(i,j)
                    bso4=av24s4g(i,j)
                    bno3=av24n3g(i,j)
                    bno2=av24n2g(i,j)
                    boc=av24ocg(i,j)
                    bec=av24ecg(i,j)
                    bpmc=av24pcg(i,j)
                    bpmf=av24pfg(i,j)
                 endif
              else
c ---            Write run-length visibility results for this receptor
c ---            Compute measures
                 if(tngb(i,j).GT.0.) then
                    if(LAVER) then
                       rpct=100.*tngp(i,j)
                    else
                       rpct=100.*tmdl/tngb(i,j)
                    endif
                    rbtot=tmdl+tngb(i,j)
                    rdvtot=10.*ALOG(rbtot*0.1)
                    rdvbkg=10.*ALOG(tngb(i,j)*0.1)
                 else
                    rpct=0.0
                    rbtot=-99.
                    rdvtot=0.0
                    rdvbkg=0.0
                 endif
c                 write(iox3,123) i,j,xgrd(i,j),ygrd(i,j),rtype,
c     &                           tmdl,tngb(i,j),rbtot,rpct,tngh(i,j)
c                 write(iox4,124) i,j,xgrd(i,j),ygrd(i,j),rtype,
c     &                           rdvtot,rdvbkg,deltadv,tngh(i,j)
                 write(iox3,123) i,j,xgrd(i,j),ygrd(i,j),rtype,
     &                           tmdl,tngb(i,j),rbtot,rpct
                 write(iox4,124) i,j,xgrd(i,j),ygrd(i,j),rtype,
     &                           rdvtot,rdvbkg,deltadv
              endif
            endif
          enddo
         enddo
      endif
c
c  Section for discrete receptors
      if(LD .AND. ndrec .NE. 0) then
         rtype=rdtype
c ---    Find peak value
         do i=1,ndrec
            if(ndrecp(i).EQ.1) then
c ---          Set modeled extinction without background
               tmdl=tnd(i)
               if(tndb(i).LE.0.) then
                  deltadv=-999.
               else
                  if(LVBK) tmdl=tmdl-tndb(i)
                  if(LAVER) then
                     deltadv=10.*ALOG(1.+tndp(i))
                  else
                     deltadv=10.*ALOG(1.+tmdl/tndb(i))
                  endif
               endif
               if(L24) then
c ---             Find peak 24-hr value of DELTA-DECIVIEW
                  if(deltadv.GT.peak) then
                     rectype=rdtype
                     peak=deltadv
                     ir=i
                     xr=xrec(i)
                     yr=yrec(i)
                     frh=tndh(i)
                     bb=tndb(i)
                     bm=tmdl
                     br=tndp(i)
                     bso4=av24s4d(i)
                     bno3=av24n3d(i)
                     bno2=av24n2d(i)
                     boc=av24ocd(i)
                     bec=av24ecd(i)
                     bpmc=av24pcd(i)
                     bpmf=av24pfd(i)
                  endif
               elseif(LDRING) then
c ---             Save peak run-length value of DELTA-DECIVIEW
c ---             on each ring
                  if(deltadv.GT.peakR(idring(i))) then
                     peakR(idring(i))=deltadv
                     frhR(idring(i))=tndh(i)
                     bbR(idring(i))=tndb(i)
                     bmR(idring(i))=tmdl
                     bpR(idring(i))=tndp(i)
                  endif
               else
c ---             Write run-length visibility results for this receptor
c ---             Compute measures
                  if(tndb(i).GT.0.) then
                     if(LAVER) then
                        rpct=100.*tndp(i)
                     else
                        rpct=100.*tmdl/tndb(i)
                     endif
                     rbtot=tmdl+tndb(i)
                     rdvtot=10.*ALOG(rbtot*0.1)
                     rdvbkg=10.*ALOG(tndb(i)*0.1)
                  else
                     rpct=0.0
                     rbtot=-99.
                     rdvtot=0.0
                     rdvbkg=0.0
                  endif
c                  write(iox3,223) i,xrec(i),yrec(i),rtype,
c     &                            tmdl,tndb(i),rbtot,rpct,tndh(i)
c                  write(iox4,224) i,xrec(i),yrec(i),rtype,
c     &                            rdvtot,rdvbkg,deltadv,tndh(i)
                  write(iox3,223) i,xrec(i),yrec(i),rtype,
     &                            tmdl,tndb(i),rbtot,rpct
                  write(iox4,224) i,xrec(i),yrec(i),rtype,
     &                            rdvtot,rdvbkg,deltadv
               endif
            endif
         enddo
         if(.NOT.L24 .AND. LDRING) then
            do i=1,ndring
c ---          Write run-length visibility results for each ring
               if(nrrecp(i).EQ.1) then
c ---             Compute measures
                  if(bbR(i).GT.0.) then
                     IF(LAVER) then
                        rpct=100.*bpR(i)
                     else
                        rpct=100.*bmR(i)/bbR(i)
                     endif
                     rbtot=bmR(i)+bbR(i)
                     rdvtot=10.*ALOG(rbtot*0.1)
                     rdvbkg=10.*ALOG(bbR(i)*0.1)
                  else
                     rpct=0.0
                     rbtot=-99.
                     rdvtot=0.0
                     rdvbkg=0.0
                  endif
c                  write(iox3,323) i,rradkm(i),rtype,bmR(i),bbR(i),
c     &                            rbtot,rpct,frhR(i)
c                  write(iox4,324) i,rradkm(i),rtype,rdvtot,rdvbkg,
c     &                            peakR(i),frhR(i)
                  write(iox3,323) i,rradkm(i),rtype,bmR(i),bbR(i),
     &                            rbtot,rpct
                  write(iox4,324) i,rradkm(i),rtype,rdvtot,rdvbkg,
     &                            peakR(i)
               endif
            enddo
         endif
      endif
c
c  Section for terrain receptors
      if(lct .AND. nctrec .NE. 0) then
         rtype='  T '
c ---    Find peak value
         do i=1,nctrec
c ---       Set modeled extinction without background
            tmdl=tnt(i)
            if(tntb(i).LE.0.) then
               deltadv=-999.
            else
               if(LVBK) tmdl=tmdl-tntb(i)
               if (LAVER) then
                  deltadv=10.*ALOG(1.+tntp(i))
                  else
                  deltadv=10.*ALOG(1.+tmdl/tntb(i))
                  endif
            endif
            if(L24) then
c ---          Find peak 24-hr value of DELTA-DECIVIEW
               if(deltadv.GT.peak) then
                  rectype='  T '
                  peak=deltadv
                  ir=i
                  xr=xctr(i)
                  yr=yctr(i)
                  frh=tnth(i)
                  bb=tntb(i)
                  bm=tmdl
                  br=tntp(i)
                  bso4=av24s4t(i)
                  bno3=av24n3t(i)
                  bno2=av24n2t(i)
                  boc=av24oct(i)
                  bec=av24ect(i)
                  bpmc=av24pct(i)
                  bpmf=av24pft(i)
               endif
            else
c ---          Write run-length visibility results for this receptor
c ---          Compute measures
               if(tntb(i).GT.0.) then
                  if(LAVER) then
                     rpct=100.*tntp(i)
                  else
                     rpct=100.*tmdl/tntb(i)
                  endif
                  rbtot=tmdl+tntb(i)
                  rdvtot=10.*ALOG(rbtot*0.1)
                  rdvbkg=10.*ALOG(tntb(i)*0.1)
               else
                  rpct=0.0
                  rbtot=-99.
                  rdvtot=0.0
                  rdvbkg=0.0
               endif
c               write(iox3,223) i,xrec(i),yrec(i),rtype,
c     &                         tmdl,tndb(i),rbtot,rpct,tnth(i)
c               write(iox4,224) i,xrec(i),yrec(i),rtype,
c     &                         rdvtot,rdvbkg,deltadv,tnth(i)
               write(iox3,223) i,xrec(i),yrec(i),rtype,
     &                         tmdl,tndb(i),rbtot,rpct
               write(iox4,224) i,xrec(i),yrec(i),rtype,
     &                         rdvtot,rdvbkg,deltadv
            endif
         enddo
      endif

c --- Processing of run-length data is complete
      if(.NOT.L24) return

c  Compute measures derived for peak 24-hour average
c --- Set NULL values
      pct=0.0
      btot=-99.
      dvtot=0.0
      dvbkg=0.0
      fso4=0.0
      fno3=0.0
      fno2=0.0
      foc=0.0
      fec=0.0
      fpmc=0.0
      fpmf=0.0
      if(bb.GT.0.) then
         if(LAVER) then
           if(br.NE.-99.) then
              pct=100.*br
           else
              pct=-99.
           endif
         else
           if(bm.NE.-99.) then
              pct=100.*bm/bb
           else
              pct=-99.
           endif
         endif
         if(bm.NE.-99.) then
             btot=bm+bb
             dvtot=10.*ALOG(btot*0.1)
             dvbkg=10.*ALOG(bb*0.1)
         else
             btot=-99.
             dvtot=-99.
             dvbkg=-99.
         endif
c ---    Contribution to total modeled extinction (%)
         if(bm.GT.0.) then
            fso4=100.*bso4/bm
            fno3=100.*bno3/bm
            fno2=100.*bno2/bm
            foc=100.*boc/bm
            fec=100.*bec/bm
            fpmc=100.*bpmc/bm
            fpmf=100.*bpmf/bm
         endif
      endif

c  Write the peak 24 value data to scratch files (extinction, deciview)
      if(lm8_month) then
c ---   Write monthly f(RH) for small/large/seasalt
        if(rectype.EQ.'  G ') then
          write(iox1,121) myr,mjday,mhr,ir,jr,xr,yr,rectype,bm,bb,
     &                    btot,pct,bso4,bno3,boc,bec,bpmc,bpmf,bno2,
     &                    frhs,frhl,frhss
          write(iox2,122) myr,mjday,mhr,ir,jr,xr,yr,rectype,dvtot,
     &                    dvbkg,peak,fso4,fno3,foc,fec,fpmc,fpmf,fno2,
     &                    frhs,frhl,frhss
        elseif(rectype.EQ.'  R ') then
          write(iox1,321) myr,mjday,mhr,idring(ir),rradkm(idring(ir)),
     &                    rectype,bm,bb,btot,
     &                    pct,bso4,bno3,boc,bec,bpmc,bpmf,bno2,
     &                    frhs,frhl,frhss
          write(iox2,322) myr,mjday,mhr,idring(ir),rradkm(idring(ir)),
     &                    rectype,dvtot,dvbkg,
     &                    peak,fso4,fno3,foc,fec,fpmc,fpmf,fno2,
     &                    frhs,frhl,frhss
        else
          write(iox1,221) myr,mjday,mhr,ir,xr,yr,rectype,bm,bb,
     &                    btot,pct,bso4,bno3,boc,bec,bpmc,bpmf,bno2,
     &                    frhs,frhl,frhss
          write(iox2,222) myr,mjday,mhr,ir,xr,yr,rectype,dvtot,
     &                    dvbkg,peak,fso4,fno3,foc,fec,fpmc,fpmf,fno2,
     &                    frhs,frhl,frhss
        endif
      else
c ---   Write average f(RH)
        if(rectype.EQ.'  G ') then
          write(iox1,121) myr,mjday,mhr,ir,jr,xr,yr,rectype,bm,bb,
     &                    btot,pct,bso4,bno3,boc,bec,bpmc,bpmf,bno2,
     &                    frh
          write(iox2,122) myr,mjday,mhr,ir,jr,xr,yr,rectype,dvtot,
     &                    dvbkg,peak,fso4,fno3,foc,fec,fpmc,fpmf,fno2,
     &                    frh
        elseif(rectype.EQ.'  R ') then
          write(iox1,321) myr,mjday,mhr,idring(ir),rradkm(idring(ir)),
     &                    rectype,bm,bb,btot,
     &                    pct,bso4,bno3,boc,bec,bpmc,bpmf,bno2,
     &                    frh
          write(iox2,322) myr,mjday,mhr,idring(ir),rradkm(idring(ir)),
     &                    rectype,dvtot,dvbkg,
     &                    peak,fso4,fno3,foc,fec,fpmc,fpmf,fno2,
     &                    frh
        else
          write(iox1,221) myr,mjday,mhr,ir,xr,yr,rectype,bm,bb,
     &                    btot,pct,bso4,bno3,boc,bec,bpmc,bpmf,bno2,
     &                    frh
          write(iox2,222) myr,mjday,mhr,ir,xr,yr,rectype,dvtot,
     &                    dvbkg,peak,fso4,fno3,foc,fec,fpmc,fpmf,fno2,
     &                    frh
        endif
      endif

      return

121   format(i4,i4,i3,3x,i3,',',i3,1x,2f10.3,1x,a4,3(f10.3,1x),
     *       f8.2,1x,7f7.3,1x,3f6.3)
122   format(i4,i4,i3,3x,i3,',',i3,1x,2f10.3,1x,a4,2(f10.3,1x),
     *       f10.3,7f7.2,1x,3f6.3)
123   format(14x,i3,',',i3,1x,2f10.3,1x,a4,3(f10.3,1x),
     *       f8.2,1x,f7.3)
124   format(14x,i3,',',i3,1x,2f10.3,1x,a4,2(f10.3,1x),
     *       f10.3,f7.3)
221   format(i4,i4,i3,i6,5x,2f10.3,1x,a4,3(f10.3,1x),
     *       f8.2,1x,7f7.3,1x,3f6.3)
222   format(i4,i4,i3,i6,5x,2f10.3,1x,a4,2(f10.3,1x),
     *       f10.3,7f7.2,1x,3f6.3)
223   format(11x,i6,5x,2f10.3,1x,a4,3(f10.3,1x),
     *       f8.2,1x,f7.3)
224   format(11x,i6,5x,2f10.3,1x,a4,2(f10.3,1x),
     *       f10.3,f7.3)
321   format(i4,i4,i3,i6,'R',9x,f10.3,6x,a4,3(f10.3,1x),
     *       f8.2,1x,7f7.3,1x,3f6.3)
322   format(i4,i4,i3,i6,'R',9x,f10.3,6x,a4,2(f10.3,1x),
     *       f10.3,7f7.2,1x,3f6.3)
323   format(11x,i6,'R',9x,f10.3,6x,a4,3(f10.3,1x),
     *       f8.2,1x,f7.3)
324   format(11x,i6,'R',9x,f10.3,6x,a4,2(f10.3,1x),
     *       f10.3,f7.3)

      end
c-----------------------------------------------------------------------
      subroutine dvis_hd
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 060309           DVIS_HD
c ---          D. Strimaitis    Earth Tech, Inc
c
c  PURPOSE:     Write header information describing auxiliary output
c               file of visibility results
c
c  ARGUMENTS:
c     PASSED:   none
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'visb.pst'

      character*4 xyunit
      character*8 cbtzone
      character*16 dataset,dataver
      character*20 vistype(4)
      character*64 datamod
      character*100 comment1,comment2

      logical lutm,llcc,lps,lem,llaza,lttm

c --- Configure output variables
      data dataset/'DELVIS.DAT'/, dataver/'2.1'/
      data ncomment/2/
      data comment1/'Produced by CALPOST Version: '/
      data vistype/'Delta-Deciview      ','Delta-Extinction (%)',
     &             'Delta-Deciview      ','Delta-Extinction (%)'/
      data lutm/.false./, llcc/.false./, lps/.false./
      data lem/.false./, llaza/.false./, lttm/.false./
      data xyunit/'  KM'/

      datamod(1:35) ='Header comments, times with seconds'
      datamod(36:64)=', time zone, coord info      '

      comment2(1:40) ='YYYY JJJ HH  SEC YYYY JJJ HH  SEC  Rec  '
      comment2(41:74)='    (I , J)    X(km)      Y(km)   '

c --- Check MDVIS
      if(mdvis.LT.1 .OR. mdvis.GT.4) then
         write(*,*) 'DVIS_HD:  Invalid visibility selection'
         write(*,*) '          Expected 1,2,3, or 4'
         write(*,*) '          Found MDVIS = ',mdvis
         stop
      endif

c --- Add to comment2
      comment2(75:94)=vistype(mdvis)

c --- Construct the version-level comment string
      j=30
      do i=1,12
         if(mver(i:i).NE.' ') then
            comment1(j:j)=mver(i:i)
            j=j+1
         endif
      enddo
      j=j+1
      comment1(j:j+7)=' Level: '
      j=j+8
      do i=1,12
         if(mlevel(i:i).NE.' ') then
            comment1(j:j)=mlevel(i:i)
            j=j+1
         endif
      enddo

c --- Construct base time zone string
      cbtzone='UTC-0000'
      atzone=ABS(btzone)
      ibz=INT(atzone)
      mm=NINT((atzone-FLOAT(ibz))*60.)
      ibz=ibz*100+mm
      write(cbtzone(5:8),'(i4.4)') ibz
      if(btzone.LE.0) cbtzone(4:4)='+'

c --- Set logicals for map projection PMAP
      if(pmap.EQ.'UTM     ')  lutm =.TRUE.
      if(pmap.EQ.'LCC     ')  llcc =.TRUE.
      if(pmap.EQ.'PS      ')   lps =.TRUE.
      if(pmap.EQ.'EM      ')   lem =.TRUE.
      if(pmap.EQ.'LAZA    ') llaza =.TRUE.
      if(pmap.EQ.'TTM     ')  lttm =.TRUE.

c --- Write records
      write(iodv,'(2a16,a64)') dataset,dataver,datamod
      write(iodv,'(i4)') ncomment
      write(iodv,'(a100)') comment1
      write(iodv,'(a100)') comment2
      write(iodv,'(a8)') pmap
      if(LUTM) then
         write(iodv,'(i4,a4)') iutmzn,utmhem
      elseif(LLCC) then
         write(iodv,'(4a16)') clat0,clon0,clat1,clat2
      elseif(LPS) then
         write(iodv,'(3a16)') clat0,clon0,clat1
      elseif(LEM.or.LLAZA.or.LTTM) then
         write(iodv,'(2a16)') clat0,clon0
      endif
      if(LLCC.or.LLAZA.or.LTTM) then
         write(iodv,*) feast,fnorth
      endif
      write(iodv,'(a8,a12)') datum,daten
      write(iodv,'(a4)') xyunit
      write(iodv,'(a8)') cbtzone
      write(iodv,'(a20)') vistype(mdvis)

      return
      end
c-----------------------------------------------------------------------
      subroutine outdvis(iratio,tng,tnd,tnt,tngb,tndb,tntb,
     *                   myrb,mjdayb,mhrb,msec)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221         Level: 060919            OUTDVIS
c ---           D. Strimaitis, Earth Tech
c
c  PURPOSE:     Computes change in visibility at each selected receptor
c               and writes to output file
c
c  UPDATES:
c  V6.12(060309) to V6.133(060919)
c               (DGS) Fixed interpretation of averaging time provided
c                     in the argument list (start-time, not end-time)
c
c  ARGUMENTS:
c     PASSED:   iratio          Extinction ratio flag for tn[g,d,t]  [i]
c                                0: modeled Bext
c                                1: modeled/background Bext
c               tn[g,d,t]       Modeled Bext or ratio               [ra]
c               tnb[g,d,t]      Corresponding background Bext       [ra]
c            myrb,mjdayb,mhrb   Date/time marker for period start    [i]
c               msec            Seconds for date/time marker         [i]
c
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'visb.pst'

      real tng(mxgx,mxgy),tnd(mxdrec),tnt(mxctrec)
      real tngb(mxgx,mxgy),tndb(mxdrec),tntb(mxctrec)

      character*4 rtype

c --- Gregorian month,day not used here
      imo=0
      iday=0

c --- Should be whole-hour time (seconds=0)
      if(msec.NE.0) then
         write(io6,*) 'OUTDVIS: must be whole-hour periods'
         write(io6,*) '                 Seconds found:  ',msec
         write(*,*) 'OUTDVIS: must be whole-hour periods'
         write(*,*) '                 Seconds found:  ',msec
         stop
      endif

c --- Set Ending time
      if(MDVIS.LE.2) then
         nhrinc=24
      else
         nhrinc=1
      endif
      myre=myrb
      mjdaye=mjdayb
      mhre=mhrb
      call INCR(io6,myre,mjdaye,mhre,nhrinc)

c --- Set end-of-day using 2400 time
      if(mhre.EQ.0) then
         khre=24
         call MIDNITE(io6,'TO 24h',myre,imo,iday,mjdaye,
     &                             kyre,kmoe,kdaye,kjdaye)
      else
         kyre=myre
         kjdaye=mjdaye
         khre=mhre
      endif

c --- Section for gridded receptors
      if(LSGRID .AND. LG) then
         rtype='  G '
         do ir=ibgrid,iegrid
          do jr=jbgrid,jegrid
            if(ngrecp(ir,jr).EQ.1) then
              xr=xgrd(ir,jr)
              yr=ygrd(ir,jr)
              if(iratio.GT.0) then
c ---            tng array is ratio of modeled/background Bext
                 deltadv=10.*ALOG(1.+tng(ir,jr))
                 deltabx=100.*tng(ir,jr)
              else
c ---            Set modeled extinction without background
                 tmdl=tng(ir,jr)
                 if(LVBK) tmdl=tmdl-tngb(ir,jr)
                 if(tngb(ir,jr).LE.0.) then
                    deltadv=-999.
                    deltabx=-999.
                 else
                    deltadv=10.*ALOG(1.+tmdl/tngb(ir,jr))
                    deltabx=100.*tmdl/tngb(ir,jr)
                 endif
              endif
              dv=-999.
              if(mdvis.EQ.1 .OR. mdvis.EQ.3) dv=deltadv
              if(mdvis.EQ.2 .OR. mdvis.EQ.4) dv=deltabx
              write(iodv,120) myrb,mjdayb,mhrb,msec,
     &                        kyre,kjdaye,khre,msec,
     &                        rtype,ir,jr,xr,yr,dv
            endif
          enddo
         enddo
      endif

c --- Section for discrete receptors
      if(LD .AND. ndrec .NE. 0) then
         rtype='  D '
         jr=0
         do ir=1,ndrec
            if(ndrecp(ir).EQ.1) then
               xr=xrec(ir)
               yr=yrec(ir)
               if(iratio.GT.0) then
c ---             tnd array is ratio of modeled/background Bext
                  deltadv=10.*ALOG(1.+tnd(ir))
                  deltabx=100.*tnd(ir)
               else
c ---             Set modeled extinction without background
                  tmdl=tnd(ir)
                  if(LVBK) tmdl=tmdl-tndb(ir)
                  if(tndb(ir).LE.0.) then
                     deltadv=-999.
                     deltabx=-999.
                  else
                     deltadv=10.*ALOG(1.+tmdl/tndb(ir))
                     deltabx=100.*tmdl/tndb(ir)
                  endif
               endif
               dv=-999.
               if(mdvis.EQ.1 .OR. mdvis.EQ.3) dv=deltadv
               if(mdvis.EQ.2 .OR. mdvis.EQ.4) dv=deltabx
               write(iodv,120) myrb,mjdayb,mhrb,msec,
     &                         kyre,kjdaye,khre,msec,
     &                         rtype,ir,jr,xr,yr,dv
            endif
         enddo
      endif

c --- Section for terrain receptors
      if(lct .AND. nctrec .NE. 0) then
         rtype='  T '
         jr=0
         do ir=1,nctrec
            xr=xctr(ir)
            yr=yctr(ir)
            if(iratio.GT.0) then
c ---          tnt array is ratio of modeled/background Bext
               deltadv=10.*ALOG(1.+tnt(ir))
               deltabx=100.*tnt(ir)
            else
c ---          Set modeled extinction without background
               tmdl=tnt(ir)
               if(LVBK) tmdl=tmdl-tntb(ir)
               if(tntb(ir).LE.0.) then
                  deltadv=-999.
                  deltabx=-999.
               else
                  deltadv=10.*ALOG(1.+tmdl/tntb(ir))
                  deltabx=100.*tmdl/tntb(ir)
               endif
            endif
            dv=-999.
            if(mdvis.EQ.1 .OR. mdvis.EQ.3) dv=deltadv
            if(mdvis.EQ.2 .OR. mdvis.EQ.4) dv=deltabx
            write(iodv,120) myrb,mjdayb,mhrb,msec,
     &                      kyre,kjdaye,khre,msec,
     &                      rtype,ir,jr,xr,yr,dv
         enddo
      endif

      return

120   format(2(i4,i4.3,i3.2,i5.4,1x),a4,i8,i4,2(1x,f10.3),1pe12.4)

      end
c-----------------------------------------------------------------------
      subroutine vissumry
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080724          VISSUMRY
c ---           D. Strimaitis
c
c  PURPOSE:     Retrieves summary tables of peak 24-hr avg visibility
c               and writes to list file, and also to plot-file if
c               option is selected
c
c  UPDATES:
c  V6.14(061107) to V6.221(080724)
c               (DGS) Increase line-length )more f(RH) columns
c  V6.12(060309) to V6.14(061107)
c               (DGS) Add NO2 as modeled species
c  V5.2(991104a) to V6.12(060309)
c               (DGS) Add ranked tabulation for 24-hour visibility peak
c  V5.1(990709) to V5.2(991104a)
c               (DGS) Add Elemental Carbon as modeled species
c  V5.0(990228) to V5.1(990709)
c               (DGS) Increase internal reads by '1x' to match changes
c                     in OUTVIS
c  V5.0(981116) to V5.0(990228)
c               (DGS) Run-Length output added
c  V5.0(981025) to V5.0(981116)
c               (DGS) Summary statistics added at bottom of table
c
c  ARGUMENTS:
c     PASSED:   navper          Number of periods in averages        [i]
c
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   UPV24
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'filnam.pst'

      real text(3),tddv(3)
      integer next(3),nddv(3)
      character*162 a162,blank,asave

c --- Ranked tabulation of daily peaks
      real v24(mxv24)
      character*162 h1_162,h2_162,c162(mxv24)

c -- Set thresholds for counting exceedances
      data text/5.0, 10.0, 1.0/         ! Extinction Change (%)
      data tddv/0.5, 1.0, 0.1/          ! Delta-dv

c -- Number of header records in scratch file
      data nhdr/17/

c -- Create blank character variable
      do i=1,162
         blank(i:i)=' '
      enddo

c -- Open plot-file if option was selected
      if(LPLT) open(mapu,file=vpltdat)

c -- Rewind the scratch files
      rewind(iox1)
      rewind(iox2)
      rewind(iox3)
      rewind(iox4)

c -- Initialize counters for table summary
c -- Number of records exceeding extinction and delta-dv thresholds
      do i=1,3
         next(i)=0
         nddv(i)=0
      enddo

c -- Transfer records from EXTINCTION scratch files
c -------------------------------------------------

c -- (24-Hour)
c ------------
c -- Maximum change in extinction
      extmax=0.

c -- Initialize ranked arrays
      nrank=nrnkv24
      h1_162=blank
      h2_162=blank
      do k=1,mxv24
         v24(k)=0.
         c162(k)=blank
      enddo

c -- Header records
      do i=1,nhdr
         a162=blank
         read(iox1,'(a162)') a162
         write(io1,'(a162)') a162
         if(LPLT) write(mapu,'(a162)') a162
c --    Save the last 2 header records
         if(i.LT.nhdr) h1_162=a162
         if(i.EQ.nhdr) h2_162=a162
      enddo

c -- Data records
10    a162=blank
      read(iox1,'(a162)',end=15) a162
      write(io1,'(a162)') a162
      if(LPLT) write(mapu,'(a162)') a162

c -- Check for exceedances and maximum % change
      read(a162,'(80x,f8.2)') ext
      if(ext.GT.extmax) extmax=ext
      do j=1,2
         if(ext.GE.text(j)) next(j)=next(j)+1
      enddo
c --- Rank all that exceed first cutoff
      nrank=MAX(nrank,next(1))
      nrank=MIN(nrank,mxv24)

c -- Update largest NRANK values
      call UPV24(nrank,v24,c162,ext,a162)

      goto 10

c -- Report summary information
15    write(io1,*)
      write(io1,*)
      write(io1,'(a40)')' --- Ranked Daily Visibility Change --- '
      write(io1,'(a162)') h1_162
      write(io1,'(a162)') h2_162
      do k=1,nrank
         write(c162(k)(160:162),'(i3)') k
         write(io1,'(a162)') c162(k)
      enddo

      write(io1,*)
      do j=1,2
         write(io1,101) text(j),next(j)
      enddo
      write(io1,102) extmax

      if(LPLT) then
         write(mapu,*)
         do j=1,2
            write(mapu,101) text(j),next(j)
         enddo
         write(mapu,102) extmax
      endif

c -- (Run-Length)
c ---------------
c -- Maximum change in extinction
      extmax=0.
      asave=blank

c -- Header records
      do i=1,nhdr
         a162=blank
         read(iox3,'(a162)') a162
         write(io1,'(a162)') a162
         if(LPLT) write(mapu,'(a162)') a162
      enddo

c -- Data records
11    a162=blank
      read(iox3,'(a162)',end=16) a162
      if(LRUNL) then
         write(io1,'(a162)') a162
         if(LPLT) write(mapu,'(a162)') a162
      endif

c -- Check for exceedances and maximum % change (save line with max)
      read(a162,'(80x,f8.2)') ext
      if(ext.GT.extmax) then
         extmax=ext
         asave=a162
      endif
      if(ext.GE.text(3)) next(3)=next(3)+1
      goto 11

c -- Report summary information
16    write(io1,*)
      write(io1,'(a162)') asave
      write(io1,*)
      write(io1,103) text(3),next(3)
      write(io1,102) extmax

      if(LPLT) then
         write(mapu,*)
         write(mapu,'(a162)') asave
         write(mapu,*)
         write(mapu,103) text(3),next(3)
         write(mapu,102) extmax
      endif


c -- Transfer records from DECIVIEW scratch file
c ----------------------------------------------

c -- (24-Hour)
c ------------
c -- Maximum delta-dv
      ddvmax=0.

c -- Initialize ranked arrays
      nrank=nrnkv24
      h1_162=blank
      h2_162=blank
      do k=1,mxv24
         v24(k)=0.
         c162(k)=blank
      enddo

c -- Header records
      do i=1,nhdr
         a162=blank
         read(iox2,'(a162)') a162
         write(io1,'(a162)') a162
         if(LPLT) write(mapu,'(a162)') a162
c --    Save the last 2 header records
         if(i.LT.nhdr) h1_162=a162
         if(i.EQ.nhdr) h2_162=a162
      enddo

c -- Data records
20    a162=blank     
      read(iox2,'(a162)',end=30) a162
      write(io1,'(a162)') a162
      if(LPLT) write(mapu,'(a162)') a162

c -- Check for exceedances and maximum % change
      read(a162,'(69x,f10.3)') ddv
      if(ddv.GT.ddvmax) ddvmax=ddv
      do j=1,2
         if(ddv.GE.tddv(j)) nddv(j)=nddv(j)+1
      enddo
c --- Rank all that exceed first cutoff
      nrank=MAX(nrank,nddv(1))
      nrank=MIN(nrank,mxv24)

c -- Update largest NRNKV24 values
      call UPV24(nrank,v24,c162,ddv,a162)

      goto 20

c -- Report summary information
30    write(io1,*)
      write(io1,*)
      write(io1,'(a40)')' --- Ranked Daily Visibility Change --- '
      write(io1,'(a162)') h1_162
      write(io1,'(a162)') h2_162
      do k=1,nrank
         write(c162(k)(150:152),'(i3)') k
         write(io1,'(a162)') c162(k)
      enddo

      write(io1,*)
      do j=1,2
         write(io1,201) tddv(j),nddv(j)
      enddo
      write(io1,202) ddvmax

      if(LPLT) then
         write(mapu,*)
         do j=1,2
            write(mapu,201) tddv(j),nddv(j)
         enddo
         write(mapu,202) ddvmax
      endif

c -- (Run-Length)
c ---------------
c -- Maximum delta-dv
      ddvmax=0.
      asave=blank

c -- Header records
      do i=1,nhdr
         a162=blank
         read(iox4,'(a162)') a162
         write(io1,'(a162)') a162
         if(LPLT) write(mapu,'(a162)') a162
      enddo

c -- Data records
21    a162=blank     
      read(iox4,'(a162)',end=31) a162
      if(LRUNL) then
         write(io1,'(a162)') a162
         if(LPLT) write(mapu,'(a162)') a162
      endif

c -- Check for exceedances and maximum % change (save line with max)
      read(a162,'(69x,f10.3)') ddv
      if(ddv.GT.ddvmax) then
         ddvmax=ddv
         asave=a162
      endif
      if(ddv.GE.tddv(3)) nddv(3)=nddv(3)+1
      goto 21

c -- Report summary information
31    write(io1,*)
      write(io1,'(a162)') asave
      write(io1,*)
      write(io1,203) tddv(3),nddv(3)
      write(io1,202) ddvmax

      if(LPLT) then
         write(mapu,*)
         write(mapu,'(a162)') asave
         write(mapu,*)
         write(mapu,203) tddv(3),nddv(3)
         write(mapu,202) ddvmax
      endif


c -- Close plot-file
      if(LPLT) close(mapu)

      return

101   format(' --- Number of days with Extinction Change  => ',
     &       f5.1,' % :    ',i6)
102   format(' ---             Largest Extinction Change  =  ',
     &       13x,f6.2,' %')
103   format(' --- Number of recs with Extinction Change  >  ',
     &       f5.1,' % :    ',i6)
201   format(' --- Number of days with Delta-Deciview  => ',
     &       f6.2,':    ',i6)
202   format(' ---             Largest Delta-Deciview  =  ',
     &       11x,f6.3)
203   format(' --- Number of recs with Delta-Deciview  >  ',
     &       f6.2,':    ',i6)

      end
c-----------------------------------------------------------------------
      subroutine upv24(n,v24,c24,value,text)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080724             UPV24
c ---           D. Strimaitis
c
c  PURPOSE:     Updates Top visibility-change arrays with current
c               value
c
c  UPDATES:
c  V6.14(061107) to V6.221(080724)
c               (DGS) Increase line-length (more f(RH) columns, char162)
c  V6.12(060309) to V6.14(061107)
c               (DGS) Add NO2 as modeled species (char140 to char160)
c
c  ARGUMENTS:
c     PASSED:   n               Number of ranked values reported     [i]
c               v24             Ranked visibilty change array       [ra]
c               c24             Corresponding text string array     [ca]
c               value           Current visibilty change             [r]
c               text            Corresponding text string            [c]
c
c   RETURNED:   v24             Ranked visibilty change array       [ra]
c               c24             Corresponding text string array     [ca]
c
c  CALLING ROUTINES:    VISSUMRY
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'

c --- Declare arrays locally to actual size
      real v24(n)
      character*162 c24(n),text

c --- Check for extremes first
      if(value.LE.v24(n)) return

      nm1=n-1

c --- Search for rank (irplac) within top N
      do 120 ii=1,nm1
         if(value .LT. v24(ii)) goto 120
         irplac=ii
         goto 130
120   continue
      irplac=n
130   index=n-irplac
      if(index .NE. 0) then
c ---    Shift entries in rank arrays to make room for new entry
         do ii=1,index
            jj=n-ii
            kk=jj+1
            v24(kk)=v24(jj)
            c24(kk)=c24(jj)
         enddo
      endif
c --- Place current values into proper place in rank arrays
      v24(irplac)=value
      c24(irplac)=text

      return
      end

c----------------------------------------------------------------------
      subroutine readcf(ioin,ioout,version,level)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080724            READCF
c ---            J. Scire
c ---            Sections adapted from GETCTRL routine
c ---            D. Strimaitis
c
c --- PURPOSE:  Read the CALPOST control file inputs
c
c --- UPDATES:
c     V6.22(080709) to V6.221(080724)
c               (DGS) Add more digits to extinction efficiency in
c                     control file input summary section of list file
c               (DGS) Add MVISCHECK and AREANAME
c     V6.211(080214) to V6.22(080709)
c               (DGS) Add M8_MODE = 4,5
c               (DGS) Add RHFSML, RHFLRG, RHFSEA
c     V6.2(070712) to V6.211(080214)
c               (DGS) Report RNO2NOX in place of the retired FNO2NOX
c                     to the list file
c     V6.144(070130) to V6.2(070712)
c               (DGS) Add NO2/NOx ratios
c     V6.141(061120) to V6.144(070130)
c               (DGS) Add MCALMPRO option for calm processing in files
c                     created by plume models
c     V6.14(061107) to V6.141(061120)
c               (DGS) Replace IRHTYPE with M8_MODE, adding option to
c                     use 24-hr average concentration
c     V6.133(060919) to V6.14(061107)
c               (DGS) Add IMPROVE 2006 variable extinction efficiency
c                     with MFRH=4, MVISBK=8, and background sea salt;
c                     add IRHTYPE to allow "method 2 or method 6"
c                     treatment
c               (DGS) Add NO2 absorption option
c     V6.12(060309) to V6.133(060919)
c               (DGS) Initialize NHRS to zero (some compiler options
c                     flag this, others automatically start with 0)
c     V6.1(050915) to V6.12(060309)
c               (DGS) Add auxiliary output file option for visibility
c     V5.65(050729) to V6.1(050915)
c               (DGS) Sub-hour timestep changes to control file
c                     (older version supported)
c                     - start and end time (drop NHRS)
c                     - include minutes and seconds in times
c                     - replace NAVG with NAVGH,NAVGM,NAVGS
c                     - add 1-period output option (like 1-hr)
c               (DGS) Add the base time zone (BTZONE) input to control
c                     file group 1.  It may also be in the visibility
c                     Method 7 input section of group 2.  If it appears
c                     in both groups, the value must be the same
c                     (QAINP).
c     V5.638(050408) to V5.65(050729)
c               (DGS) Add SAMPLER processing option (MSAMPLER)
c     V5.635(050128) to V5.638(050408)
c               (DGS) Add PEAK VALUE option (LPEAK)
c     V5.634(050114) to V5.635(050128)
c               (DGS) Add call to COORDSVER and write info to list file
c     V5.633(041202) to V5.634(050114)
c               (DGS) Extra IDUM in call to READIN for input group 2 is
c                     removed.
c     V5.632(041130) to V5.633(041202)
c               (CEC) LAVER added (choice to 24h-averaged hourly ratios
c               source extinction over background extinction (True) or
c               24h-averaged source extinction and background extinction
c               separately and then make a ratio of them (False - Default)
c     V5.6(031017) to V5.632(041130)
c               (DGS) Added MFRH (hygroscopic growth curve option)
c     V5.5(030627) to V5.6(031017)
c               (DGS) Added MSRC (source contribution option)
c     V5.41(030528) to V5.5(030627)
c               (DGS) Add visibility method 7 IDWSTA and TZONE arrays
c               (DGS) Add BTZONE to CALPOST control file for method 7
c               (DGS) Add hourly extinction report for method 7
c     V5.4(030402) to V5.41(030528)
c               (FRR) Change variable format in write statement
c                (gridded receptor subset)
c     V5.2(991104b) to V5.4(030402)
c               (DGS) add list-file unit to INCR, JULDAY, GRDAY
c               (DGS) include params.cal
c               (DGS) add LDOC
c     V5.2(991104a) to V5.2(991104b)
c               (DGS) Add LDRING option (Discrete Ring receptors)
c     V5.2(991104) to V5.2(991104a)
c               (DGS) Add Elemental Carbon (EC) as modeled species
c     V5.1(990920) to V5.2(991104)
c               (DGS) Implement multiple time-series files
c               (DGS) Add TF for Total Flux (wet & dry)
c     V5.1(990709) to V5.1(990920)
c               (DGS) Add time-series option
c               (DGS) Revise output control logicals
c               (DGS) Add MVISBK=6 and RHFAC(12)
c     V5.0(990228a)to V5.1(990709)
c               (DGS) Allow general run-steps, with time resolved to
c                     seconds; Enforce YYYY year format
c     V5.0(981116) to V5.0(990228a)
c               (DGS) Add NGONOFF, and NGRECP for excluding specific
c                     gridded receptors from analysis
c               (DGS) Add NCOUNT and NDAY for reporting exceedance counts
c                     over multiple-day periods
c     V5.0(981025) to V5.0(981116)
c               (DGS) Add MVISBK=5 for reading nephelometer data
c     V5.0(980918) to V5.0(981025)
c               (DGS) Add range for gridded receptors processed
c     V5.0(980821) to V5.0(980918)
c               (DGS) Allow MVISBK=3 option
c               (DGS) Allow MVISBK=4 option with VSRN.DAT file
c     V5.0(980515) to V5.0(980821)
c               (DGS) Break control file input into 3 groups
c               (DGS) Add new visibility processing option
c               (DGS) Add range for discrete receptors processed
c
c     V5.0(980430) to V5.0(980515)
c               (DGS) Change version and level to character*12
c
c     V5.0(980304) to V5.0(980430)
c               (DGS) Add LCTSG to flag presence of data at CTSG
c                     receptors
c               (DGS) Change deposition units label from _g/m**2
c                     to _g/m**2/s
c               (DGS) Use 1/Mm units for visibility
c               (DGS) Revise plot file names; add format flags
c
c     V5.0(971015) to V5.0(980304)
c               (AMK) LBACK allows monitored hourly background data
c                     read from external file to be added to
c                     concentrations.
c
c     V5.0(971007) to V5.0(971015)
c               (JSS) Input NREP factor allows processing of every
c                     "NREP" hour of data
c
c     V3.1 to V3.2(960716)
c               (DGS) Accept start-time and run-length from puff
c                     output file for use if METRUN=1
c               (DGS) Accept choice of reporting units
c
c
c --- INPUTS:
c
c           IOIN - integer       - Fortran unit number of the CALPOST
c                                  control file (CALPOST.INP)
c          IOOUT - integer       - Fortran unit number of the CALPOST
c                                  output list file (CALPOST.LST)
c        VERSION - character*12  - Version of CALPOST
c          LEVEL - character*12  - Level of CALPOST
c       Parameters:
c           MXSG, MXVAR
c
c       Common block /SAMP/
c           IBUTC, NSAMP, IRSAMP
c
c --- OUTPUT:
c
c       Common block /CALMPRO/
c           MET1FMT
c       Common block /CTRL/
c           ISYR, JSDAY, ISHR, ISMIN, ISSEC, ISSEC4,
c           IEYR, JEDAY, IEHR, IEMIN, IESEC, IESEC4, NPER, ASPEC,
c           IBGRID,JBGRID,IEGRID,JEGRID, NO2CALC, MVISCHECK,
c           NDRECP,NGRECP,NGONOFF,MSRC,MSAMPLER,MDVIS,MCALMPRO,
c           ILAYER, LG, LD, LCT, A, B, LDEBUG, IPRTU, UNITS, NREP,
c           LDRING, LCTSG, LVISIB, LVSR
c           L1PD,L1HR,L3HR,L24HR,NAVG,NAVGH,NAVGM,NAVGS,LNAVG,LRUNL,
c           LTOPN, LT50, LEXCD, LECHO, LTIME, L24ECHO, L24TSER,
c           LPEAK, L24PEAK, NTSAVG, NTOP, ITOP(4),
c           THRESH1, THRESH3, THRESH24, THRESHN,
c           STHRESH1, STHRESH3, STHRESH24, STHRESHN, RSCALE,
c           NCOUNT, NDAY,
c           LPLT, LGRD,LDOC,
c           IECHO(366), AREANAME
c       Common block /NO2NOX/
c           FNO2NOX, RNO2NOX, BINCNOX(14), BINFNO2(14)
c       Common block /VISB/
c           LVSO4,LVNO3,LVNO2,LVOC,LVPMC,LVPMF,LVEC,LVBK,LRHFAC,
c           SPECPMC,SPECPMF,
c           EEPMC,EEPMF,EEPMCBK,EESO4,EENO3,EENO2,EEOC,EESOIL,EEEC,
c           MVISBK,MFRH,BEXTBK,RHFRAC,RHMAX,BEXTRAY,M8_MODE,
c           BKSO4(12),BKNO3(12),BKPMC(12),BKOC(12),BKSOIL(12),BKEC(12)
c           BKSALT(12),RHFAC(12),IDWSTA(mxwsta),TZONE(mxwsta),BTZONE2,
c           RHFSML(12),RHFLRG(12),RHFSEA(12)
c
c --- READCF called by:  MAIN
c --- READCF calls:      READFN, QAYR4, WRIVL, READIN, JULDAY, CAPS,
c                        OUTIFX, INCR, GRDAY, COORDSVER, RDSAMP
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'
      include 'params.cal'
c
      real rprtu(5)
      character*80 title(3)
      character*50 verdoc
      character*24 afrh(5)
      character*16 cdathr
      character*12 cvdic(mxvar,mxsg)
      character*12 version,level
      character*4 aspec4(12),specpmc4(12),specpmf4(12)
      character*4 avno2
      character*4 ctemp(60)
      character*1 cprtu(4)
      integer ivleng(mxvar,mxsg),ivtype(mxvar,mxsg)

c --- Declaration of local variables for gridded receptor flags
      integer ngxrecp(mxgx)
      character*70 messag0
      logical lsee

c --- Include common blocks
      INCLUDE 'calmpro.pst'
      include 'ctrl.pst'
      include 'filnam.pst'
      include 'no2nox.pst'
      include 'samp.pst'
      include 'visb.pst'

c --- Set description for f(RH) growth curves
      data afrh/'IWAQM (1998) Curve      ',
     &          'FLAG (2000) Tabulation  ',
     &          'EPA (2003) Tabulation   ',
     &          'IMPROVE (2006) Tables   ',
     &          'Invalid Selection       '/

c --- Set character for scale of output units (m:milli,u:micro,n:nano)
      data cprtu/' ','m','u','n'/

c --- Set units conversion factor array
      data rprtu/1.0,1.0e3,1.0e6,1.0e9,1.0/

c --- Set logical to write control file records to list file
      data lsee/.TRUE./

c --- Set character version of NO2 logical LVNO2 to 'UUUU' to identify
c --- user input
      data avno2/'UUUU'/

c --- Initialize NHRS to sero
      data nhrs/0/

c --- Set temporary variables used for QA
      data rno2nox0/-1.0/
      data no2calc0/-1/

      data cvdic/
     a 'METRUN','ISYR','ISMO','ISDY','ISHR','ISMIN','ISSEC',
     a 'IEYR','IEMO','IEDY','IEHR','IEMIN','IESEC','NHRS','ASPEC',
     a 'NREP','LBACK','ILAYER','LG','LD','LCT','LDRING','A','B',
     a 'NDRECP','IBGRID','JBGRID','IEGRID','JEGRID','NGONOFF',
     a 'MSOURCE','MSAMPLER','BTZONE','MCALMPRO','MET1FMT',
     a 'NO2CALC','RNO2NOX','CNOX','TNO2NOX', 21*' ',
     b 'NGXRECP',59*' ',
     c 'LVSO4','LVNO3','LVOC','LVPMC','LVPMF','LVEC','LVBK','LVNO2',
     c 'SPECPMC','SPECPMF','M8_MODE','FNO2NOX',
     c 'EEPMC','EEPMF','EEPMCBK','EESO4','EENO3','EEOC','EESOIL',
     c 'EEEC','EENO2','MVISBK','BEXTBK','RHFRAC','RHMAX','BEXTRAY',
     c 'BKSO4','BKNO3','BKPMC','BKOC','BKSOIL','BKEC','BKSALT',
     c 'RHFAC','RHFSML','RHFLRG','RHFSEA','IDWSTA','TZONE','BTZONE',
     c 'MFRH','LAVER','MVISCHECK', 'AREANAME',  16*' ',
     d 'IPRTU','NAVG','NAVGH','NAVGM','NAVGS','L1HR','L3HR','L24HR',
     d 'L1PD','LRUNL','NTOP','ITOP','LTOPN','LT50','LEXCD','LECHO',
     d 'LTIME','THRESH1','THRESH3','THRESH24','THRESHN','NCOUNT',
     d 'NDAY','LPLT','LGRD','LDEBUG','LVEXTHR','IECHO','LDOC','LPEAK',
     d 'MDVIS', 29*' '/
c
      data ivleng/
     a  14*1,12,9*1, mxdrec,12*1,14,14, 21*0,
     b  mxgx, 59*0,
     c  8*1,2*12,16*1,11*12,2*mxwsta,4*1,60, 16*0,
     d  11*1, 4, 15*1, 366, 3*1, 29*0/
      data ivtype/
     a  14*2,4,2,3,2,4*3,2*1,8*2,1,3*2,3*1, 21*0,
     b  2, 59*0,
     c  7*3,3*4,2,10*1,2,15*1,2,2*1,2,3,2,4, 16*0,
     d  5*2,5*3,2*2,5*3,4*1,2*2,4*3,2,2*3,2, 29*0/
c --- Note that variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
c
c --- Read title (First 3 lines of the control file)
      read(ioin,102)title
102   format(a80/a80/a80)
c
c ----------------------------------------------
c --- Read file names from control file - IG # 0
c ----------------------------------------------
c --- Set output unit number to the "screen" (io6) for errors
      call READFN(ioin,io6)
c
c --- Open the output LIST file
      open(ioout,file=pstlst,status='unknown')
c
c --- Write model version and level to output file (no page break)
      call WRIVL(io1,version,level,0)

c --- Obtain COORDS version information
      call COORDSVER(ioout,verdoc)
      write(ioout,*)
      write(ioout,*)
      write(ioout,*)'Internal Coordinate Transformations by ',verdoc
      write(ioout,*)
c
      write(ioout,1410)title
1410  format(//2x,'Run Title:'/
     1 3(5x,a80/))

c --- Initialize species name characters to blanks
      do ich=1,12
         aspec4(ich)=' '
         specpmc4(ich)=' '
         specpmf4(ich)=' '
      enddo
c
c ----------------------------------------------
c --- Read control file variables -- IG # 1
c ----------------------------------------------
      call READIN(cvdic(1,1),ivleng(1,1),ivtype(1,1),ioin,ioout,lsee,
     1 METRUN,ISYR,ISMO,ISDY,ISHR,ISMIN,ISSEC,IEYR,IEMO,IEDY,IEHR,
     2 IEMIN,IESEC,NHRS,ASPEC4,NREP,LBACK,
     3 ILAYER, LG, LD, LCT, LDRING, A, B, NDRECP,IBGRID,JBGRID,
     4 IEGRID,JEGRID,NGONOFF,MSRC,MSAMPLER,BTZONE,MCALMPRO,MET1FMT,
     5 NO2CALC0,RNO2NOX0,BINCNOX,BINFNO2,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum)

c --- Compute minutes in starting and ending times if needed
      if(issec.GE.60) then
         iminadd=issec/60
         ismin=ismin+iminadd
         issec=issec-60*iminadd
      endif
      if(iesec.GE.60) then
         iminadd=iesec/60
         iemin=iemin+iminadd
         iesec=iesec-60*iminadd
      endif
c --- Store minutes:seconds into sec4 variable (0000 to 3599)
      issec4=ismin*60+issec
      iesec4=iemin*60+iesec

c --- Rescale any NOx concentrations provided from ug/m3 to g/m3
      do k=1,14
         if(bincnox(k).GT.0.0) bincnox(k)=bincnox(k)*1.0e-06
      enddo

      if(MSAMPLER.EQ.0) then
c ---    Option not used so look for more input groups in file
c
c ----------------------------------------------
c ---    Read control file variables -- IG # 1a    (conditional)
c ----------------------------------------------
c ---    Loop over rows in sampling grid
         imax=1
         if(ngonoff.GT.0) lgexclude=.TRUE.
         do j=ngonoff,1,-1
c ---       Set ngxrecp array to 1's before reading line
            do i=1,mxgx
               ngxrecp(i)=1
            enddo
c ---       Read line
      call READIN(cvdic(1,2),ivleng(1,2),ivtype(1,2),ioin,ioout,lsee,
     1 NGXRECP,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c ---       Pass values into 2-D array NGRECP
            if(j.LE.mxgy) then
               do i=1,mxgx
                  ngrecp(i,j) = ngxrecp(i)
c ---             Update largest "i" that is given a 0-value
                  if(ngxrecp(i).EQ.0) then
                     if(i.GT.imax) imax=i
                  endif
               enddo
            endif
         enddo
c
c ----------------------------------------------
c ---    Read control file variables -- IG # 2
c ----------------------------------------------
c --- Initialize the temporary array
      do j=1,60
            ctemp(j)(1:1)=' '
      enddo
      call READIN(cvdic(1,3),ivleng(1,3),ivtype(1,3),ioin,ioout,lsee,
     1 LVSO4, LVNO3, LVOC, LVPMC, LVPMF, LVEC, LVBK, AVNO2, SPECPMC4,
     2 SPECPMF4, M8_MODE, FNO2NOX, EEPMC, EEPMF, EEPMCBK, EESO4, EENO3,
     3 EEOC, EESOIL, EEEC, EENO2, MVISBK, BEXTBK, RHFRAC, RHMAX,
     4 BEXTRAY, BKSO4, BKNO3, BKPMC, BKOC, BKSOIL, BKEC, BKSALT, RHFAC,
     5 RHFSML, RHFLRG, RHFSEA, IDWSTA, TZONE, BTZONE2, MFRH, LAVER,
     6 MVISCHECK,CTEMP,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     8 idum,idum,idum,idum)

c --- Either use default LVNO2 for the MVISBK option selected, or use
c --- value provided by user
      if(avno2.EQ.'UUUU') then
c ---    Value not provided, so use default
         if(mvisbk.EQ.8) then
            lvno2=.TRUE.
         else
            lvno2=.FALSE.
         endif
      else
c ---    Use value provided
         if(avno2(1:1).EQ.'T' .OR. avno2(1:1).EQ.'t') then
            lvno2=.TRUE.
         else
            lvno2=.FALSE.
         endif
      endif


c --- Interpret presence of retired control variable FNO2NOX
      if(fno2nox.GE.0.0) then
c ---    Retired variable found
         if(no2calc0.GE.0 .OR. rno2nox0.GE.0.0) then
c ---       Current variables are also found: ignore FNO2NOX
c ---       Assign values found, or retain defaults
            if(no2calc0.GE.0) no2calc=no2calc0
            if(rno2nox0.GE.0.0) rno2nox=rno2nox0
          else
c ---       Set current values from fno2nox
            rno2nox=fno2nox
            if(fno2nox.EQ.0.0) then
               no2calc=0
            else
               no2calc=1
            endif
         endif
      else
c ---    Retired variable NOT found
c ---    Assign values found, or retain defaults
         if(no2calc0.GE.0) no2calc=no2calc0
         if(rno2nox0.GE.0.0) rno2nox=rno2nox0
      endif

c --- Transfer Class I area name from array to string 
      if(ctemp(1)(1:1).ne.' ')areaname=' '
      do j=1,60
         if(ctemp(j)(1:1).ne.' ')areaname(j:j)=ctemp(j)(1:1)
      enddo

c
c ----------------------------------------------
c ---    Read control file variables -- IG # 3
c ----------------------------------------------
      call READIN(cvdic(1,4),ivleng(1,4),ivtype(1,4),ioin,ioout,lsee,
     1 IPRTU, NAVG, NAVGH, NAVGM, NAVGS, L1HR, L3HR, L24HR, L1PD,
     2 LRUNL, NTOP, ITOP, LTOPN, LT50, LEXCD, LECHO, LTIME,
     3 THRESH1, THRESH3, THRESH24, THRESHN, NCOUNT, NDAY,
     4 LPLT, LGRD, LDEBUG, LVEXTHR, IECHO,LDOC,LPEAK,MDVIS,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum)

      else
c --------------------------------------------------
c ---    Read SAMPDAT information and configure run
c --------------------------------------------------
         call RDSAMP
         call TDATE(ioout,2,cdathr,ibutc,isyr,ismo,isdy,isjul,ishr)
c ---    Shift to end-hour marker (UTC here!)
         call INCR(ioout,isyr,isjul,ishr,1)
         call GRDAY(ioout,isyr,isjul,ismo,isdy)
         call TDATE(ioout,2,cdathr,ieutc,ieyr,iemo,iedy,iejul,iehr)
         call DELTT(isyr,isjul,ishr,ieyr,iejul,iehr,nhrs)
         nhrs=nhrs+1
         if(MSAMPLER.GT.1) msrc=2
         do ic=1,12
            aspec4(ic)='X'
         enddo
         ndrecp(1)=0
         do k=1,nsdat
            ndrecp(irsamp(k))=1
         enddo
         METRUN=0
         LG=.FALSE.
         LD=.TRUE.
         LCT=.FALSE.
         LT50 = .FALSE.
         LTOPN = .FALSE.
         L3HR = .FALSE.
         L24HR = .FALSE.
         LRUNL = .FALSE.
      endif
c
c --- Close control file
      close(ioin)
c
c --- Special QA on starting year of simulation
      call QAYR4(ioout,isyr,metrun,ierr)
      if(ierr.NE.0) stop 'CORRECT THE CONTROL FILE'
c
c --- Swap NHRS into NPER for subsequent use
      nper=nhrs
c
c --- Set logical for user-supplied averaging time
      lnavg=.FALSE.
      ntest=navg+navgh+navgm+navgs
      if(ntest.GT.0) lnavg=.TRUE.
c
c --- Set logical for ECHO of 24-hour averages (helps simplify logic
c --- in MAIN program)
      l24echo=.FALSE.
      if(L24HR .AND. LECHO) l24echo=.TRUE.
c
c --- Set logical for 24-hour time-series average (helps simplify logic
c --- in MAIN program)
      l24tser=.FALSE.
      if(L24HR .AND. LTIME) l24tser=.TRUE.
      l24peak=.FALSE.
      if(L24HR .AND. LPEAK) l24peak=.TRUE.
c
c --- Compute starting/ending Julian day
      if(metrun.EQ.1) then
         jsday=0
         jeday=0
      else
         if(nper.GT.0) then
c ---       Starting date/hour and number of hours provided
            jeday=0
c ---       Convert end-time of first hour processed to begin-time
            call JULDAY(ioout,isyr,ismo,isdy,jsday)
            call INCR(ioout,isyr,jsday,ishr,-1)
            call GRDAY(ioout,isyr,jsday,ismo,isdy)
         else
c ---       Starting date/time and Ending date/time provided
            call JULDAY(ioout,isyr,ismo,isdy,jsday)
            call JULDAY(ioout,ieyr,iemo,iedy,jeday)
         endif
      endif
c
c --- Prepare any particulate species name included in the control
c --- file by erasing the DEFAULT name
      if(specpmc4(1)(1:1).ne.' ')specpmc=' '
      if(specpmf4(1)(1:1).ne.' ')specpmf=' '
c
c --- Transfer the PM char*4 data into the PM char*12 variables
      do j=1,12
         if(specpmc4(j)(1:1).ne.' ')specpmc(j:j)=specpmc4(j)(1:1)
         if(specpmf4(j)(1:1).ne.' ')specpmf(j:j)=specpmf4(j)(1:1)
      enddo
c
c --- Transfer ASPEC4 char*4 array values into char*12 ASPEC array
      do i=1,12
         aspec(i:i)=aspec4(i)
      enddo

c -- Set logical variable if species is a light extinction variable
c -- Make sure that all characters in species name are upper-case
      call CAPS
      lvisib=.FALSE.
      lvsr=.FALSE.
      lrhfac=.FALSE.
      if(aspec(1:5).eq.'VISIB')then
         lvisib=.TRUE.
         if(mvisbk.EQ.4 .OR. mvisbk.EQ.5) lvsr=.TRUE.
         if(mvisbk.EQ.6) lrhfac=.TRUE.
         if(mvisbk.EQ.8 .AND. m8_mode.GE.2) lrhfac=.TRUE.
      endif
c
c --- Construct units name
      if(ilayer.GT.0) then
         if(LVISIB) then
c ---       Visibility
c ---       Enforce single unit
            units='(1/Mega-m)   '
            iprtu=1
            rscale=1.0
         else
c ---       Concentration
            units='(Odour Units)'
            if(iprtu.LT.5) then
               units='( g/m**3)    '
               units(2:2)=cprtu(iprtu)
            endif
         endif
      else
c ---    Fluxes
         units='(-----------)'
         if(iprtu.LT.5) then
            units='( g/m**2/s)  '
            units(2:2)=cprtu(iprtu)
         endif
      endif
c
c --- Scale threshold values from specified units to internal units
      if(.not.LVISIB) rscale=rprtu(iprtu)
      dum=1./rscale
      sthresh1=thresh1*dum
      sthresh3=thresh3*dum
      sthresh24=thresh24*dum
      sthreshn=threshn*dum

c
c  Construct species/level name from aspec and ilayer, and set name
c  for either concentration or deposition
c  Also set LCTSG here (subject to NCTREC value in QAINP)
c
      asplv=aspec
      lctsg=.TRUE.
      if(LVISIB)then
         cdname="  EXTINCTION  "
         asplv(8:15)='________'
         if(LVBK)  asplv(8:9)  ='B'
         if(LVOC)  asplv(9:10) ='O'
         if(LVEC)  asplv(10:11)='E'
         if(LVSO4) asplv(11:11)='S'
         if(LVNO3) asplv(12:12)='N'
         if(LVPMC) asplv(13:13)='C'
         if(LVPMF) asplv(14:14)='F'
         if(LVNO2) asplv(15:15)='G'
      else if(ilayer .GT. 0) then
         write(asplv(13:15),'(i3)') ilayer
         cdname=" CONCENTRATION"
      elseif(ilayer .EQ. -1) then
         lctsg=.FALSE.
         asplv(13:15)=" DF"
         cdname="DRY DEPOSITION"
      elseif(ilayer .EQ. -2) then
         lctsg=.FALSE.
         asplv(13:15)=" WF"
         cdname="WET DEPOSITION"
      elseif(ilayer .EQ. -3) then
         lctsg=.FALSE.
         asplv(13:15)=" TF"
         cdname="W+D DEPOSITION"
      endif

c --- Count the number of weather stations in VSRN.DAT
      do iw=1,mxwsta
         if(idwsta(iw).NE.0) nwsta=nwsta+1
      enddo

c
c ----------------- Print input parameters ---------------
c
      call WRIVL(ioout,version,level,1)

c -- Note: day/hr is TIME BEGINNING label for the hour, on 0-23 clock
      write(ioout,1420) metrun,isyr,ismo,isdy,jsday,ishr,ismin,issec
1420  format(//2x,
     &'CALPOST Control File Input Summary -------------------------'//
     1 5x,'Replace run data with data in Puff file 1=Y: ',i4/
     2 5x,'                  Run starting date -- year: ',i4/
     3 5x,'                                      month: ',i4/
     4 5x,'                                        day: ',i4/
     5 5x,'                                 Julian day: ',i4/
     6 5x,'        Time at start of run  -  hour(0-23): ',i4/
     6 5x,'                              -      minute: ',i4/
     6 5x,'                              -      second: ',i4)

      if(nhrs.GT.0) then
        write(ioout,1421) nhrs
1421    format(/,
     1 5x,'                         Run length (hours): ',i4)
c-      write(ioout,*)
c-      write(ioout,*) 'Note: the length of a period is controlled by'
c-      write(ioout,*) '      the averaging time selected in the model'
        write(ioout,*)
      else
        write(ioout,1422) ieyr,iemo,iedy,jeday,iehr,iemin,iesec
1422    format(/,
     2 5x,'                    Run ending date -- year: ',i4/
     3 5x,'                                      month: ',i4/
     4 5x,'                                        day: ',i4/
     5 5x,'                                 Julian day: ',i4/
     6 5x,'          Time at end of run  -  hour(0-23): ',i4/
     6 5x,'                              -      minute: ',i4/
     6 5x,'                              -      second: ',i4/)
      endif

c --- Special note for SAMPLER option
      if(MSAMPLER.GT.0) then
         write(ioout,*)
         write(ioout,*)' --- Date/Time is UTC for SAMPLER option ---'
         write(ioout,*)
      endif

      if(btzone.LT.-900. .AND. btzone2.LT.-900.) then
         write(ioout,'(/5x,a57)')
     &    '                             Base time zone: from CALPUFF'
      else
         if(btzone.GT.-900.) 
     &   write(ioout,'(/5x,a45,f5.1)')
     &    '                   Base time zone (Group 1): ',btzone
         if(btzone2.GT.-900.) 
     &   write(ioout,'(/5x,a45,f5.1)')
     &    '                   Base time zone (Group 2): ',btzone2
      endif

c --- CALM processing option
      if(mcalmpro.EQ.1) then
         write(ioout,'(/5x,a45,i1)')
     &    '    CALM processing for averages - MCALMPRO: ',mcalmpro
         write(ioout,'(5x,a45,i1)')
     &    ' Surface meteorological file type - MET1FMT: ',met1fmt
      endif
c
      if(nrep.eq.1)then
         write(ioout,1423)nrep
1423     format(/1x,'Every period of data processed -- NREP = ',i1)
      else
         write(ioout,1424)nrep
1424     format(/1x,'Every "NREP" period processed -- NREP = ',i6)
      endif
c
      write(ioout,1430) aspec,ilayer,a,b,LBACK
1430  format(//2x,'Species & Concentration/Deposition Information'/
     1 5x,'                                    Species: ',a12,/
     2 5x,'                    Layer of processed data: ',i4,/
     3 5x,'(>0=conc, -1=dry flux, -2=wet flux, -3=wet & dry flux)'/
     4 5x,'              Multiplicative scaling factor: ',1pe10.4,0p/
     5 5x,'                    Additive scaling factor: ',1pe10.4/
     6 5x,'             Hourly background values used?: ',l1)
c
      write(ioout,1434) msampler
1434  format(//2x,'SAMPLER option'/
     1 5x,'                          Processing method: ',i1/
     2 5x,'              0= SAMPLER option not used',/
     3 5x,'              1= Report total modeled impact (list file)',/
     4 5x,'              2= TRACEBACK mode (DAT files)',/
     5 5x,'              3= TRACEBACK mode with sampling factor',
     6 ' (DAT files)')
c
      write(ioout,1435) msrc
1435  format(//2x,'Source information'/
     1 5x,'             Source contribution processing: ',i1/
     2 5x,'              0= No source contributions',/
     3 5x,'              1= Contributions are summed',/
     4 5x,'              2= TRACEBACK mode for 1 receptor',/
     5 5x,'              3= Reported TOTAL is processed')
c
      write(ioout,1440)lg,ld,lct
1440  format(//2x,'Receptor information'/
     1 5x,'               Gridded receptors processed?: ',L1/
     2 5x,'              Discrete receptors processed?: ',L1/
     3 5x,'  CTSG Complex terrain receptors processed?: ',L1)
      if(LG) then
         write(ioout,1441)
         igtest=ibgrid+jbgrid+iegrid+jegrid
         if(igtest.EQ.-4 .AND. .not.LGEXCLUDE) then
            write(ioout,*)'  (All Gridded Receptors are Used)'
         elseif(igtest.GE.4) then
c frr 030526: format change (integer not real otherwise: crash)
            write(ioout,'(30x,a20,i4)')'       Begin at ix: ',ibgrid
            write(ioout,'(30x,a20,i4)')'         End at ix: ',iegrid
            write(ioout,'(30x,a20,i4)')'       Begin at iy: ',jbgrid
            write(ioout,'(30x,a20,i4)')'         End at iy: ',jegrid
            if(LGEXCLUDE) then
               write(ioout,*)
               messag0=' Gridded Receptor Flags: Excluded = 0'
c --           Create map
               call OUTIFX(ngrecp,1,messag0,ibgrid,jbgrid,iegrid,
     &                                              jegrid,ioout)
            endif
         elseif(LGEXCLUDE) then
            write(ioout,*)
            messag0=' Gridded Receptor Flags: Excluded = 0'
c --        Create map
            illc=1
            jllc=1
            iurc=MIN(mxgx,imax)
            jurc=MIN(mxgy,ngonoff)
            call OUTIFX(ngrecp,1,messag0,illc,jllc,iurc,jurc,ioout)
         endif
      endif
1441  format(/2x,'Gridded Receptors Processed'/)
      if(LD) then
         write(ioout,1442)
         if(LDRING) write(ioout,1443)
         idlast=0
         if(ndrecp(1).EQ.-1) then
            write(ioout,*)'  (All Discrete Receptors are Used)'
         else
            do ir=1,mxdrec
               if(ndrecp(ir).EQ.1) idlast=ir
            enddo
            write(ioout,'(40i2)')(ndrecp(ir),ir=1,idlast)
         endif
      endif
1442  format(/2x,'Discrete Receptors Processed'/)
1443  format(2x,'Results are reported for Each Ring'/)
c
c -------------------------------------------------------------
c
c
      if(LVISIB) then
         write(ioout,'(//2x,a40)')
     &               'Visibility Processing Selected          '
c
         if(mvischeck.EQ.0) then
            write(ioout,'(/2x,a48)')
     &          'Visibility Options are NOT Checked for FLAG 2008'
         else
            write(ioout,'(/2x,a44)')
     &          'Visibility Options are Checked for FLAG 2008'
         endif
         write(ioout,'(/2x,a74)')
     &               'Class I Area: '//areaname
         write(ioout,'(/2x,a40)')
     &               'Extinction Computation includes:        '
         if(LVSO4) write(ioout,'(30x,a16)')'SULFATES        '
         if(LVNO3) write(ioout,'(30x,a16)')'NITRATES        '
         if(LVNO2) then
                   write(ioout,'(30x,a16)')'NO2 GAS         '
                   write(ioout,'(15x,a35,f7.3)')
     &            'Fraction CALPUFF NOx used as NO2 : ',rno2nox
         endif
         if(LVOC)  write(ioout,'(30x,a16)')'ORGANIC CARBON  '
         if(LVEC)  write(ioout,'(30x,a16)')'ELEMENTAL CARBON'
         if(LVPMC) write(ioout,'(30x,a16)')'COARSE PARTICLES'
         if(LVPMF) write(ioout,'(30x,a16)')'FINE PARTICLES  '
         if(LVBK)  write(ioout,'(30x,a16)')'BACKGROUND      '

         krh=mfrh
         if(krh.LT.1 .OR. krh.GT.4) krh=5
         write(ioout,'(15x,a35,a26)')
     &        'Particle f(RH) growth curve(s)   : ',afrh(krh)

         write(ioout,'(15x,a35,f7.3)')
     &        'Max. RH % for particle growth (%): ',rhmax
c
         if(LVPMC.OR.LVPMF) then
            write(ioout,'(/2x,a40)')
     &               'Species name for modeled particulates   '
         endif
         if(LVPMC) write(ioout,'(42x,a8,a12)')'coarse: ',specpmc
         if(LVPMF) write(ioout,'(42x,a8,a12)')'  fine: ',specpmf
c
         write(ioout,'(/2x,a40)')
     &               'Extinction Efficiency (1/Mm per ug/m**3)'
         if(mvisbk.EQ.8) then
            write(ioout,'(30x,a20,f9.4)')'ammonium sulfate S: ',eeso4s
            write(ioout,'(30x,a20,f9.4)')'ammonium sulfate L: ',eeso4l
            write(ioout,'(30x,a20,f9.4)')'ammonium nitrate S: ',eeno3s
            write(ioout,'(30x,a20,f9.4)')'ammonium nitrate L: ',eeno3l
            write(ioout,'(30x,a20,f9.4)')'  organic carbon S: ',eeocs
            write(ioout,'(30x,a20,f9.4)')'  organic carbon L: ',eeocl
            write(ioout,'(30x,a20,f9.4)')'          sea salt: ',eesalt
         else
            write(ioout,'(30x,a20,f9.4)')'  ammonium sulfate: ',eeso4
            write(ioout,'(30x,a20,f9.4)')'  ammonium nitrate: ',eeno3
            write(ioout,'(30x,a20,f9.4)')'    organic carbon: ',eeoc
         endif
         write(ioout,'(30x,a20,f9.4)')'           NO2 gas: ',eeno2
         write(ioout,'(30x,a20,f9.4)')'              soil: ',eesoil
         write(ioout,'(30x,a20,f9.4)')'  elemental carbon: ',eeec
         write(ioout,'(30x,a20,f9.4)')' MODELED coarse PM: ',eepmc
         write(ioout,'(30x,a20,f9.4)')' MODELED   fine PM: ',eepmf
         write(ioout,'(30x,a20,f9.4)')'BACKGRND coarse PM: ',eepmcbk  
c
         write(ioout,'(/2x,a41,i1)')
     &          'Background Extinction Calculation Method ',mvisbk
         if(mvisbk.EQ.1) then
            write(ioout,'(/6x,a44,f7.2)')
     &          '        Background light extinction (1/Mm): ',bextbk
            write(ioout,'(/6x,a44,f7.3)')
     &          '       RH-affected particle percentage (%): ',rhfrac
         elseif(mvisbk.EQ.4) then
            write(ioout,'(/6x,a29)')
     &          '  Transmissometer data used '
         elseif(mvisbk.EQ.5) then
            write(ioout,'(/6x,a29)')
     &          '  Nephelometer data used '
            write(ioout,'(/6x,a44,f7.2)')
     &          '     Rayleigh scattering extinction (1/Mm): ',bextray
         else
            if(mvisbk.EQ.8) then
               write(ioout,'(/6x,a44,i2)')
     &          '                             Method 8 Mode: ',m8_mode
               if(m8_mode.EQ.1) then
                  write(ioout,'(6x,a44)')
     &          '          (hourly conc. with hourly RH data)'
               elseif(m8_mode.EQ.2) then
                  write(ioout,'(6x,a44)')
     &          '         (hourly conc. with monthly RH data)'
               elseif(m8_mode.EQ.3) then
                  write(ioout,'(6x,a44)')
     &          '      (24-hr avg conc. with monthly RH data)'
               elseif(m8_mode.EQ.4) then
                  write(ioout,'(6x,a44)')
     &          '      (hourly conc. with monthly F(RH) data)'
               elseif(m8_mode.EQ.5) then
                  write(ioout,'(6x,a44)')
     &          '   (24-hr avg conc. with monthly F(RH) data)'
               endif
               if(m8_mode.EQ.2 .OR. m8_mode.EQ.3) then
                  write(ioout,'(/6x,a44,f7.2/)')
     &             ' Monthly RH factor for hygroscopic species: '
                  do im=1,12
                    write(ioout,'(i5,2x,e9.4)') im,rhfac(im)
                  enddo
               elseif(m8_mode.EQ.4 .OR. m8_mode.EQ.5) then
                  write(ioout,'(/6x,a44,f7.2/)')
     &             '    Monthly RH factor for small particles: '
                  do im=1,12
                    write(ioout,'(i5,2x,e9.4)') im,rhfsml(im)
                  enddo
                  write(ioout,'(/6x,a44,f7.2/)')
     &             '    Monthly RH factor for large particles: '
                  do im=1,12
                    write(ioout,'(i5,2x,e9.4)') im,rhflrg(im)
                  enddo
                  write(ioout,'(/6x,a44,f7.2/)')
     &             '           Monthly RH factor for sea salt: '
                  do im=1,12
                    write(ioout,'(i5,2x,e9.4)') im,rhfsea(im)
                  enddo
               endif
            elseif(LRHFAC) then
c ---          RH factor if not Method 8 is RHFAC(12)
               write(ioout,'(/6x,a44,f7.2/)')
     &          ' Monthly RH factor for hygroscopic species: '
               do im=1,12
                 write(ioout,'(i5,2x,e9.4)') im,rhfac(im)
               enddo
            endif
            write(ioout,'(/6x,a44,f7.2)')
     &          '     Rayleigh scattering extinction (1/Mm): ',bextray
            write(ioout,'(/6x,a44,f7.2)')
     &          '        Monthly background conc. (ug/m**3): '
            write(ioout,'(/5x,7a11)')'(NH4)2SO4','(NH4)NO3','PM-C',
     &                               'OC','SOIL','EC','SEA SALT'
            do im=1,12
               write(ioout,'(i5,7(2x,e9.4))') im,bkso4(im),bkno3(im),
     &                        bkpmc(im),bkoc(im),bksoil(im),bkec(im),
     &                        bksalt(im)
            enddo
         endif
         if(mvisbk.EQ.7) then
            write(ioout,'(/6x,a44)')
     &          '     Station IDs & time zones for Method 7: '
            do iw=1,nwsta
               write(ioout,'(50x,i7,f7.1)') idwsta(iw),tzone(iw)
            enddo
         endif
         write(ioout,'(/2x,a41,i1)')
     &          'Optional output file for visibility      ',mdvis
         if(mdvis.EQ.0) then
            write(ioout,'(/6x,a29)')
     &       '  Do Not create file         '
         elseif(mdvis.EQ.1) then
            write(ioout,'(/6x,a48)')
     &       '  Create file of DAILY (24 hour) Delta-Deciview '
         elseif(mdvis.EQ.2) then
            write(ioout,'(/6x,a48)')
     &       '  Create file of DAILY (24 hour) Bext Change (%)'
         elseif(mdvis.EQ.3) then
            write(ioout,'(/6x,a48)')
     &       '  Create file of HOURLY Delta-Deciview          '
         elseif(mdvis.EQ.4) then
            write(ioout,'(/6x,a48)')
     &       '  Create file of HOURLY Bext Change (%)         '
         else
            write(ioout,'(/6x,a20)')
     &       '  INVALID choice!   '
         endif
c
      else
         write(ioout,'(//2x,a40)')
     &               'Visibility Processing is NOT Selected   '
      endif
c
c -------------------------------------------------------------
c
      write(ioout,1460) units
1460  format(///2x,'Output options'/
     1 5x,'                 Units requested for output: ',a15)

      if(navg.GT.0) then
        write(ioout,1462)navg,L1PD,L1HR,L3HR,L24HR,LNAVG,LRUNL
1462    format(/2x,'Averaging time(s) selected'/
     1 6x,'User-specified averaging time (NAVG hours): ',i5,/
     2 6x,'                             1-pd averages: ',4x,L1/
     2 6x,'                             1-hr averages: ',4x,L1/
     3 6x,'                             3-hr averages: ',4x,L1/
     4 6x,'                            24-hr averages: ',4x,L1/
     5 6x,'                          NAVG-hr averages: ',4x,L1/
     6 6x,'                    Length of run averages: ',4x,L1)
      else
        write(ioout,1463)navgh,navgm,navgs,L1PD,L1HR,L3HR,L24HR,
     &                   LNAVG,LRUNL
1463    format(/2x,'Averaging time(s) selected'/
     1 6x,'  User-specified averaging time (hr:mm:ss): ',i5,2(':',i2),/
     2 6x,'                             1-pd averages: ',4x,L1/
     2 6x,'                             1-hr averages: ',4x,L1/
     3 6x,'                             3-hr averages: ',4x,L1/
     4 6x,'                            24-hr averages: ',4x,L1/
     5 6x,'                   User-specified averages: ',4x,L1/
     6 6x,'                    Length of run averages: ',4x,L1)
      endif

      write(ioout,1464) LT50,LTOPN,LEXCD,LDEBUG,LECHO,LTIME,LPEAK
1464  format(/2x,'Output components selected'/
     1 5x,'                                     Top-50: ',4x,L1/
     2 5x,'              Top-N values at each receptor: ',4x,L1/
     3 5x,'         Exceedance counts at each receptor: ',4x,L1/
     4 5x,'  Output selected information for debugging: ',4x,L1/
     5 5x,'              Echo tables for selected days: ',4x,L1/
     6 5x,'              Time-series for selected days: ',4x,L1/
     6 5x,'   Peak value Time-series for selected days: ',4x,L1)
c
      if(LTOPN) then
         write(ioout,1470)ntop,(itop(n),n=1,ntop)
1470     format(/2x,'Top "n" table control'/
     1 5x,'    Number of "top" values at each receptor: ',i5/
     2 5x,'    Specific ranks of "top" values reported: ',20(i3,1x))
      endif
c
      write(ioout,1490)LPLT
1490  format(//2x,'Plot file option'/
     1 5x,'                         Plot files created: ',4x,L1)
      if(LPLT.AND.LGRD) write(ioout,1491)
      if(LPLT.AND. .NOT.LGRD) write(ioout,1492)
1491  format(
     1 5x,'                Plot file format is GRIDDED: ',' .GRD')
1492  format(
     1 5x,'                   Plot file format is DATA: ',' .DAT')
c
      if(LEXCD) then
         write(ioout,1480)
1480     format(//2x,'Threshold Exceedance control'/
     & 5x,'  Exceedances of a specified value will be counted for --')
         if(L1HR) write(ioout,1481) THRESH1
         if(L3HR) write(ioout,1482) THRESH3
         if(L24HR) write(ioout,1483) THRESH24
         if(LNAVG) write(ioout,1484) THRESHN
         if(nday .GT. 0) write(ioout,1485) ncount,nday
1481  format(5x,'                    1-hr averages exceeding: ',1pe10.4)
1482  format(5x,'                    3-hr averages exceeding: ',1pe10.4)
1483  format(5x,'                   24-hr averages exceeding: ',1pe10.4)
1484  format(5x,'          User-specified averages exceeding: ',1pe10.4)
1485  format(5x,'  Report more than ',i3,' exceedances in any ',i3,
     *          ' day period')
      endif
c
      if(LECHO .OR. LTIME .OR. LPEAK) then
         write(ioout,1500)iecho
1500     format(//2x,'Days selected for output tables'/
     1 5x,10(10i1,2x)/5x,10(10i1,2x)/5x,10(10i1,2x)/
     2 5x,10(10i1,2x))
      endif

c
      return
      end
c----------------------------------------------------------------------
      subroutine uncomprs(xwork,ii,io,nwords,clabel,xdat)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 960422          UNCOMPRS
c                J. Scire, EARTH TECH
c
c --- PURPOSE:  Read a compressed data records and uncompress
c               the data
c
c --- INPUTS:
c        XWORK(nwork) - real array - Work array to temporarily store
c                                    compressed array
c                  II - integer    - Number of words in compressed
c                                    data record
c                  IO - integer    - Unit number of input file
c              NWORDS - integer    - Number of values in data array
c                                    after uncompression
c      PARAMETERS: IO6
c
c --- OUTPUTS:
c              CLABEL - char*15    - Character record header
c        XDAT(nwords) - real array - Array of uncompressed data to be
c                                    output
c
c --- UNCOMPRS called by: MAIN
c --- UNCOMPRS calls:     none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.pst'
c
      real xwork(ii),xdat(nwords)
      character*15 clabel
c
c --- Read the compressed data record
      read(io)clabel,xwork
c
c --- Uncompress the data
      jj=0
      do 100 i=1,ii
      if(xwork(i).gt.0.0)then
         jj=jj+1
         xdat(jj)=xwork(i)
      else
         nzero=-xwork(i)
         do j=1,nzero
            jj=jj+1
            xdat(jj)=0.0
         enddo
      endif
100   continue
c
c --- QA check that expanded array is correct size
      if(jj.ne.nwords)then
         write(io6,*)'ERROR in Subr. UNCOMPRS -- Expanded array ',
     1   'is not the correct size -- JJ = ',jj,' NWORDS = ',nwords
         stop
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine readfn(ioin,ioout)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 070130            READFN
c                J. Scire, SRC
c
c --- PURPOSE:  Read the file containing the file names of the
c               input and output files of the run
c
c --- UPDATES:
c     V6.12(060309) to V6.144(070130)
c               (DGS) Add MCALMPRO option for calm processing in files
c                     created by plume models (single-point met file)
c     V5.65(050729) to V6.12(060309)
c               (DGS) Add auxiliary output file option for visibility
c     V5.62(040503) to V5.65(050729)
c               (DGS) Add SAMPLER processing option (SAMPDAT file)
c     V5.2(991104) to V5.62(040503)
c               (DGS) Replace filename strings c*70 with c*132
c               (DGS) Drop user characters from echo filenames
c               (DGS) Replace template names with path and
c                     user names
c     V5.1(990920) to V5.2(991104)
c               (DGS) Implement multiple time-series files
c               (JSS) Error messages written to list file
c                     in addition to screen
c     V5.0(981025) to V5.1(990920)
c               (DGS) Add time-series file
c     V5.0(980918) to V5.0(981025)
c               (DGS) Add visibility plot-file
c     V5.0(980430) to V5.0(980918)
c               (DGS) Add visual range data file VSRN.DAT
c     V5.0(980304) to V5.0(980430)
c               (DGS) Revise filenames for plot files
c     V5.0(971015) to V5.0(980304)
c               (AMK) BACKDAT file used for adding hourly
c                     background concentrations.
c
c --- INPUTS:
c         IOIN - integer - Unit number of input control file
c        IOOUT - integer - Unit number of output list file
c
c --- Parameters:
c        IO6
c
c --- OUTPUT:
c
c ---    Common block /FILNAM/ variables:
c           pstlst,moddat,visdat,vsrdat,vpltdat,sampdat,met1dat,
c           plpath,tspath,tunam,xunam,tsunam,
c           npath,ntspath,ntunam,nxunam,ntsunam,
c           dvisdat
c           
c ---    Common block /FILLOG/ variables:
c           lcfiles
c
c --- READFN called by:  READCF
c --- READFN calls:      READIN, FILCASE
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'
c
      character*4 ctemp(132,10)
      character*4 ctempu(8,5)
      character*12 cvdic(16)

      character*8 eunam,vunam

      integer ivleng(16),ivtype(16)
      logical lecho
c
c --- Include common blocks
      include 'filnam.pst'
c
      data lecho/.false./
      data cvdic/'PSTLST','MODDAT','VISDAT','VSRDAT','BACKDAT',
     & 'SAMPDAT','DVISDAT','TSPATH','PLPATH','MET1DAT',
     & 'TUNAM','XUNAM','EUNAM','VUNAM','TSUNAM',
     & 'LCFILES'/
c
      data ivleng/10*132,5*8,1/
      data ivtype/15*4,3/
c --- Note that variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character

c --- Initialize user-supplied filename characters to ' ' for
c --- plot-files and time-series files
      tunam='        '
      xunam='        '
      eunam='        '
      vunam='        '
      tsunam='        '

c --- Read the file names from the data file
c
c ---    Initialize the temporary arrays
         do i=1,10
            do j=1,132
               ctemp(j,i)(1:1)=' '
            enddo
         enddo
         do i=1,5
            do j=1,8
               ctempu(j,i)(1:1)=' '
            enddo
         enddo
c
c ---    Read the file names for the model run
         call readin(cvdic(1),ivleng(1),ivtype(1),ioin,ioout,lecho,
     1   ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),ctemp(1,5),
     2   ctemp(1,6),ctemp(1,7),ctemp(1,8),ctemp(1,9),ctemp(1,10),
     3   ctempu(1,1),ctempu(1,2),ctempu(1,3),ctempu(1,4),ctempu(1,5),
     4   lcfiles,
     5   idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6   idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7   idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     8   idum,idum,idum,idum,idum,idum,idum,idum)
c
c ---    Prepare any filenames included in the I/O file by erasing
c ---    the default filename set above
         if(ctemp(1,1)(1:1).ne.' ')pstlst=' '
         if(ctemp(1,2)(1:1).ne.' ')moddat=' '
         if(ctemp(1,3)(1:1).ne.' ')visdat=' '
         if(ctemp(1,4)(1:1).ne.' ')vsrdat=' '
         if(ctemp(1,5)(1:1).ne.' ')backdat=' '
         if(ctemp(1,6)(1:1).ne.' ')sampdat=' '
         if(ctemp(1,7)(1:1).ne.' ')dvisdat=' '
         if(ctemp(1,8)(1:1).ne.' ')tspath=' '
         if(ctemp(1,9)(1:1).ne.' ')plpath=' '
         if(ctemp(1,10)(1:1).ne.' ')met1dat=' '
         if(ctempu(1,1)(1:1).ne.' ')tunam=' '
         if(ctempu(1,2)(1:1).ne.' ')xunam=' '
         if(ctempu(1,3)(1:1).ne.' ')eunam=' '
         if(ctempu(1,4)(1:1).ne.' ')vunam=' '
         if(ctempu(1,5)(1:1).ne.' ')tsunam=' '
c
c ---    Transfer the char*4 data into the char*132 variables
c ---    and track number of characters in pathname for plots and
c ---    time-series
         npath=0
         ntspath=0
         do j=1,132
            if(ctemp(j,1)(1:1).ne.' ')pstlst(j:j)=ctemp(j,1)(1:1)
            if(ctemp(j,2)(1:1).ne.' ')moddat(j:j)=ctemp(j,2)(1:1)
            if(ctemp(j,3)(1:1).ne.' ')visdat(j:j)=ctemp(j,3)(1:1)
            if(ctemp(j,4)(1:1).ne.' ')vsrdat(j:j)=ctemp(j,4)(1:1)
            if(ctemp(j,5)(1:1).ne.' ')backdat(j:j)=ctemp(j,5)(1:1)
            if(ctemp(j,6)(1:1).ne.' ')sampdat(j:j)=ctemp(j,6)(1:1)
            if(ctemp(j,7)(1:1).ne.' ')dvisdat(j:j)=ctemp(j,7)(1:1)
            if(ctemp(j,8)(1:1).ne.' ') then
               ntspath=ntspath+1
               tspath(ntspath:ntspath)=ctemp(j,8)(1:1)
            endif
            if(ctemp(j,9)(1:1).ne.' ') then
               npath=npath+1
               plpath(npath:npath)=ctemp(j,9)(1:1)
            endif
            if(ctemp(j,10)(1:1).ne.' ')met1dat(j:j)=ctemp(j,10)(1:1)
         enddo
c
c ---    Transfer the char*4 data into the char*8 variables
c ---    and track number of characters provided
         ntunam=0
         nxunam=0
         neunam=0
         nvunam=0
         ntsunam=0
         do j=1,8
            if(ctempu(j,1)(1:1).ne.' ') then
               ntunam=ntunam+1
               tunam(ntunam:ntunam)=ctempu(j,1)(1:1)
            endif

            if(ctempu(j,2)(1:1).ne.' ') then
               nxunam=nxunam+1
               xunam(nxunam:nxunam)=ctempu(j,2)(1:1)
            endif

            if(ctempu(j,3)(1:1).ne.' ') then
               neunam=neunam+1
               eunam(neunam:neunam)=ctempu(j,3)(1:1)
            endif

            if(ctempu(j,4)(1:1).ne.' ') then
               nvunam=nvunam+1
               vunam(nvunam:nvunam)=ctempu(j,4)(1:1)
            endif

            if(ctempu(j,5)(1:1).ne.' ') then
               ntsunam=ntsunam+1
               tsunam(ntsunam:ntsunam)=ctempu(j,5)(1:1)
            endif
         enddo

c ---    Form full visibility file name (include path))
         if(npath.GT.0) vpltdat(1:npath)=plpath(1:npath)
         ipos=npath+1
         ipos2=npath+11
         vpltdat(ipos:ipos2)='DAILY_VISIB'
         if(nvunam.GT.0) then
            ipos2=ipos2+1
            vpltdat(ipos2:ipos2)='_'
            ipos=ipos2+1
            ipos2=ipos2+nvunam
            vpltdat(ipos:ipos2)=vunam(1:nvunam)
         endif
         ipos=ipos2+1
         ipos2=ipos2+4
         vpltdat(ipos:ipos2)='.dat'


c ---    Convert the file names to the proper case
         call filcase(lcfiles,pstlst)
         call filcase(lcfiles,moddat)
         call filcase(lcfiles,visdat)
         call filcase(lcfiles,vsrdat)
         call filcase(lcfiles,backdat)
         call filcase(lcfiles,vpltdat)
         call filcase(lcfiles,sampdat)
         call filcase(lcfiles,dvisdat)
         call filcase(lcfiles,met1dat)

         call filcase(lcfiles,tsunam)
         call filcase(lcfiles,tunam)
         call filcase(lcfiles,xunam)
         call filcase(lcfiles,plpath)
         call filcase(lcfiles,tspath)

c --- QA on pathnames:  make sure that path ends in
c --- either '/' or '\', but do not assume which is correct for
c --- operating system (just report error)
      if(ntspath.GT.0) then
         if(tspath(ntspath:ntspath).NE.'/' .AND.
     &      tspath(ntspath:ntspath).NE.'\') then
            write(io6,*)
            write(io6,*)'READFN:  FATAL Error found in time-series path'
            write(io6,*)'         Path must end in / or \, whichever '
            write(io6,*)'         is correct for operating system'
            write(*,*)
            write(*,*)'READFN:  FATAL Error found in time-series path'
            write(*,*)'         Path must end in / or \, whichever '
            write(*,*)'         is correct for operating system'
            stop
         endif
      endif
      if(npath.GT.0) then
         if(plpath(npath:npath).NE.'/' .AND.
     &      plpath(npath:npath).NE.'\') then
            write(io6,*)
            write(io6,*)'READFN:  FATAL Error found in plot-file path'
            write(io6,*)'         Path must end in / or \, whichever '
            write(io6,*)'         is correct for operating system'
            write(*,*)
            write(*,*)'READFN:  FATAL Error found in plot-file path'
            write(*,*)'         Path must end in / or \, whichever '
            write(*,*)'         is correct for operating system'
            stop
         endif
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrfiles
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 070130           WRFILES
c                J. Scire, SRC
c
c --- PURPOSE:  Write a table of the input and output file names
c               for the current run
c
c --- UPDATES:
c     V6.12(060309) to V6.144(070130)
c               (DGS) Add MCALMPRO option for calm processing in files
c                     created by plume models (single-point met file)
c               (DGS) Blank RHS of output template string names written
c                     to list file
c     V5.65(050729) to V6.12(060309)
c               (DGS) Add auxiliary visibility output file
c     V5.62(040503) to V5.65(050729)
c               (DGS) Add SAMPLER processing option (SAMPDAT file)
c     V5.2(991104) to V5.62(040503)
c               (DGS) Replace filename strings c*70 with c*132
c               (DGS) Expand automatic file names
c     V5.1(990920) to V5.2(991104)
c               (DGS) Implement multiple time-series files
c     V5.0(990228) to V5.1(990920)
c               (DGS) Add time-series file
c               (DGS) Revise output logicals
c               (DGS) Add LRHFAC to control VISB.DAT use
c     V5.0(981025) to V5.0(990228)
c               (DGS) Condition output files list on ouputs selected
c     V5.0(980918) to V5.0(981025)
c               (DGS) Add visibility plot-file
c     V5.0(980430) to V5.0(980918)
c               (DGS) Add visual range data file
c     V5.0(960521) to V5.0(980430)
c               (DGS) Revise plot-files
c
c --- INPUTS:
c
c     Common block /CTRL/ variables:
c         L1HR,L3HR,L24HR,LNAVG,LRUNL,
c         LTOPN,LEXCD,LECHO,LPLT,LGRG,
c         LVISIB,LVSR,MSAMPLER,MDVIS,MCALMPRO
c     Common block /FILNAM/ variables:
c           pstlst,moddat,visdat,vsrdat,vpltdat,sampdat,
c           plpath,tspath,tunam,xunam,tsunam,
c           npath,ntspath,ntunam,nxunam,ntsunam,
c           dvisdat,met1dat
c     Common block /VISB/ variables:
c         LRHFAC
c     Parameters: IN1, IN2, IN3, IN4, IN5, IN6, IN7
c                 IO1, IOT1, IOT3, IOT24, IOTN, IODV
c                 MAPU
c
c --- OUTPUT:  none
c
c --- WRFILES called by:  MAIN
c --- WRFILES calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.pst'
c
c --- Include common blocks
      include 'ctrl.pst'
      include 'filnam.pst'
      include 'visb.pst'

      character*132 fname,tsdat,blnk132
      character*132 tpltdat,tpltgrd,xpltdat,xpltgrd,epltdat,epltgrd
      character*5 aconc

      do k=1,132
         blnk132(k:k)=' '
      enddo

c --- Set type of output
      if(ilayer .EQ. -1) then
         aconc='_DFLX'
      elseif(ilayer .EQ. -2) then
         aconc='_WFLX'
      elseif(ilayer .EQ. -3) then
         aconc='_TFLX'
      else
         aconc='_CONC'
      endif

c --- Set template name for timeseries output file
      tsdat=blnk132
      if(ntspath.GT.0) tsdat(1:ntspath)=tspath(1:ntspath)
      ipos=ntspath+1
      ipos2=ntspath+20
      tsdat(ipos:ipos2)='TSERIES_SPECIES_ttHR'
      ipos=ipos2+1
      ipos2=ipos2+5
      tsdat(ipos:ipos2)=aconc
      if(ntsunam.GT.0) then
         ipos2=ipos2+1
         tsdat(ipos2:ipos2)='_'
         ipos=ipos2+1
         ipos2=ipos2+ntsunam
         tsdat(ipos:ipos2)=tsunam(1:ntsunam)
      endif
      ipos=ipos2+1
      ipos2=ipos2+4
      tsdat(ipos:ipos2)='.DAT'


c --- Set template name for rank output file
      tpltdat=blnk132
      if(npath.GT.0) tpltdat(1:npath)=plpath(1:npath)
      ipos=npath+1
      ipos2=npath+19
      tpltdat(ipos:ipos2)='RANK()_SPECIES_ttHR'
      ipos=ipos2+1
      ipos2=ipos2+5
      tpltdat(ipos:ipos2)=aconc
      if(ntunam.GT.0) then
         ipos2=ipos2+1
         tpltdat(ipos2:ipos2)='_'
         ipos=ipos2+1
         ipos2=ipos2+ntunam
         tpltdat(ipos:ipos2)=tunam(1:ntunam)
      endif
      ipos=ipos2+1
      ipos2=ipos2+4
      tpltgrd=tpltdat
      tpltdat(ipos:ipos2)='.DAT'
      tpltgrd(ipos:ipos2)='.GRD'


c --- Set template name for exceedance output file
      xpltdat=blnk132
      if(npath.GT.0) xpltdat(1:npath)=plpath(1:npath)
      ipos=npath+1
      ipos2=npath+19
      xpltdat(ipos:ipos2)='EXCEED_SPECIES_ttHR'
      ipos=ipos2+1
      ipos2=ipos2+5
      xpltdat(ipos:ipos2)=aconc
      if(nxunam.GT.0) then
         ipos2=ipos2+1
         xpltdat(ipos2:ipos2)='_'
         ipos=ipos2+1
         ipos2=ipos2+nxunam
         xpltdat(ipos:ipos2)=xunam(1:nxunam)
      endif
      ipos=ipos2+1
      ipos2=ipos2+4
      xpltgrd=xpltdat
      xpltdat(ipos:ipos2)='.DAT'
      xpltgrd(ipos:ipos2)='.GRD'

c --- Set template name for echo output file
      epltdat=blnk132
      if(npath.GT.0) epltdat(1:npath)=plpath(1:npath)
      ipos=npath+1
      ipos2=npath+31
      epltdat(ipos:ipos2)='yyyy_Mmm_Ddd_hh00(UTC+0000)_L00'
      ipos=ipos2+1
      ipos2=ipos2+13
      epltdat(ipos:ipos2)='_SPECIES_ttHR'
      ipos=ipos2+1
      ipos2=ipos2+5
      epltdat(ipos:ipos2)=aconc
      ipos=ipos2+1
      ipos2=ipos2+4
      epltgrd=epltdat
      epltdat(ipos:ipos2)='.DAT'
      epltgrd(ipos:ipos2)='.GRD'

c
c --------------------------------------------------
c --- Write the list of INPUT files used in this run
c --------------------------------------------------
      write(io1,10)
10    format(//1x,13('----------')/10x,'INPUT FILES'//
     1 1x,'Default Name',5x,'Unit No.',5x,'File Name and Path'/
     2 1x,'------------',5x,'--------',5x,'------------------')
c
c --- CALPOST.INP
      write(io1,12)'CALPOST.INP',in2,pstinp
12    format(1x,a12,7x,i3,8x,a132)
c
c --- Concentration/wet flux/dry flux file (MODEL.DAT)
      write(io1,12)'MODEL.DAT',in1,moddat
c
c --- Visibility file (VISB.DAT)
      if(lvisib .AND. .not.lrhfac)write(io1,12)'VISB.DAT',in3,visdat
c
c --- SAMPLER file (SAMP.DAT)
      if(msampler.NE.0)write(io1,12)'SAMP.DAT',in6,sampdat
c
c --- Visual range data file (VSRN.DAT)
      if(lvsr)write(io1,12)'VSRN.DAT',in5,vsrdat
c
c --- Single-point met file for CALM processing (MET1DAT)
      if(mcalmpro.EQ.1)write(io1,12)'SURFACE.DAT',in7,met1dat
c
c ---------------------------------------------------
c --- Write the list of OUTPUT files used in this run
c ---------------------------------------------------
      write(io1,30)
30    format(//1x,13('----------')/10x,'OUTPUT FILES'//
     1 1x,'Default Name',5x,'Unit No.',5x,'File Name and Path'/
     2 1x,'------------',5x,'--------',5x,'------------------')
c
c --- CALPOST.LST
      write(io1,12)'CALPOST.LST',io1,pstlst
c
c --- Time-series template
      if(LTIME) then
         if(L1HR) write(io1,12)'(TSERIES 1)',iot1,tsdat
         if(L3HR) write(io1,12)'(TSERIES 3)',iot3,tsdat
         if(L24HR) write(io1,12)'(TSERIES 24)',iot24,tsdat
         if(LNAVG) write(io1,12)'(TSERIES N)',iotn,tsdat
      endif
c
c --- Output Plot-file templates
      if(LPLT)then
         if(LTOPN) then
            if(LGRD) then
               write(io1,12)'(TOPN)',mapu,tpltgrd
            else
               write(io1,12)'(TOPN)',mapu,tpltdat
            endif
         endif
         if(LEXCD) then
            if(LGRD) then
               write(io1,12)'(EXCEED)',mapu,xpltgrd
            else
               write(io1,12)'(EXCEED)',mapu,xpltdat
            endif
         endif
         if(LECHO) then
            if(LGRD) then
               write(io1,12)'(ECHO)',mapu,epltgrd
            else
               write(io1,12)'(ECHO)',mapu,epltdat
            endif
         endif
         if(LVISIB) then
            write(io1,12)'(VIS24)',mapu,vpltdat
            if(mdvis.GT.0) then
               write(io1,12)'(Delta-VIS)',iodv,dvisdat
            endif
         endif
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine setvisbk
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080709          SETVISBK
c ---           D. Strimaitis
c
c  PURPOSE:     Computes the dry and RH-dependent background
c               extinction coefficients (1/Mm), and sets growth factor
c               at RHMAX
c
c --- UPDATES:
c     V6.14(061107) to V6.22(080709)
c               (DGS) Add RHFSML,RHFLRG,RHFSEA
c     V5.5(030627) to V6.14(061107)
c               (DGS) Add MVISBK=8 option (Method 8 uses different F(RH)
c                     curves for small, large, and sea salt components;
c                     and different extinction efficiencies for small
c                     and large sulfate, nitrate and organic carbon
c                     particles
c     V5.1(990920) to V5.5(030627)
c               (DGS) Add MVISBK=7 option
c     V5.0(981228) to V5.1(990920)
c               (DGS) Add logic for MVISBK=6 with RHFAC(12) array
c     V5.0(981015) to V5.0(981228)
c               (DGS) Report error if MVISBK > 5 
c     V5.0(980918) to V5.0(981015)
c               (DGS) FRHMAX computed explicitly with RHMAX
c     V5.0(980821) to V5.0(980918)
c               (DGS) Allow MVISBK=3 option, and introduce FRHMAX
c
c
c  ARGUMENTS:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   GROWTH, FRH4
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'visb.pst'

c --- Local arrays to estimate RH asociated with the F(RH) values
      integer irh_s(12),irh_l(12),irh_ss(12)

c --- Set maximum growth factor
      if(LRHFAC) then
         if(mvisbk.EQ.8 .AND. (m8_mode.EQ.4 .OR. m8_mode.EQ.5)) then
c ---       Set FRHMAXS, FRHMAXL, FRHMAXSS from monthly values
            frhmaxs=0.0
            frhmaxl=0.0
            frhmaxss=0.0
            do i=1,12
               frhmaxs=AMAX1(frhmaxs,rhfsml(i))
               frhmaxl=AMAX1(frhmaxl,rhflrg(i))
               frhmaxss=AMAX1(frhmaxss,rhfsea(i))
            enddo
         else
c ---       Set FRHMAX from RHFAC(12) values
            frhmax=0.0
            do i=1,12
               frhmax=AMAX1(frhmax,rhfac(i))
            enddo
         endif
      else
c ---    Set FRHMAX from RHMAX
         if(mvisbk.EQ.8) then
            call FRH4(rhmax,frhmaxs,frhmaxl,frhmaxss)
         else
            frhmax=GROWTH(rhmax)
         endif
      endif

      if(mvisbk.EQ.1) then
c ---    Extinction coefficients are determined by RHFRAC and BEXTBK
         bextdry=bextbk*(1.-rhfrac)
         bexthyg=bextbk*rhfrac

      elseif(mvisbk.EQ.2 .OR. mvisbk.EQ.3 .OR. mvisbk.EQ.6 .OR.
     &       mvisbk.EQ.7) then
c ---    Extinction coefficients are determined from background conc.
c ---    Loop over months
         do i=1,12
c ---       DRY = Rayleigh + soot abs + coarse PM + soil + OC
            bextdry2(i)=bextray+eeec*bkec(i)+eepmcbk*bkpmc(i)+
     &                  eesoil*bksoil(i)+eeoc*bkoc(i)
c ---       HYG = (NH4)2SO4 + (NH4)NO3
            bexthyg2(i)=eeso4*bkso4(i)+eeno3*bkno3(i)
         enddo
      elseif(mvisbk.EQ.8) then
c ---    Extinction coefficients are determined from background conc.
c ---    using different extinction efficiencies for small and large
c ---    sulfate, nitrate, and organic carbon components
c ---    Loop over months
         do i=1,12
c ---       DRY = Rayleigh + soot + coarse PM + soil + OC
            flarge=AMIN1(1.0,bkoc(i)/20.0)
            fsmall=1.0-flarge
            bextdry2(i)=bextray+eeec*bkec(i)+eepmcbk*bkpmc(i)+
     &                  eesoil*bksoil(i)+
     &                  (eeocs*fsmall+eeocl*flarge)*bkoc(i)
c ---       HYG = (NH4)2SO4 + (NH4)NO3 + sea salt
            bexthyg2(i)=0.0
            fso4l=AMIN1(1.0,bkso4(i)/20.0)
            fso4s=1.0-fso4l
            fno3l=AMIN1(1.0,bkno3(i)/20.0)
            fno3s=1.0-fno3l
            bexthygs2(i)=eeso4s*fso4s*bkso4(i)+eeno3s*fno3s*bkno3(i)
            bexthygl2(i)=eeso4l*fso4l*bkso4(i)+eeno3l*fno3l*bkno3(i)
            bextsalt2(i)=eesalt*bksalt(i)
         enddo
      elseif(mvisbk.GT.8) then
c ---    FATAL ERROR
         write(io1,*)
     &  'SETVISBK:  Invalid background extinction option found',mvisbk
         write(io1,*)'Value should be 1, 2, 3, 4, 5, 6, 7 or 8'
         write(io6,*)
     &  'SETVISBK:  Invalid background extinction option found',mvisbk
         write(io6,*)'Value should be 1, 2, 3, 4, 5, 6, 7 or 8'
      endif

c --- Compute monthly relative humidity associated with the RH Factors
      if(LRHFAC .AND. mvisbk.EQ.8 .AND.
     &   (m8_mode.EQ.2 .OR. m8_mode.EQ.3)) then
c ---    Remember current MFRH and MVISBK
         mfrh0=mfrh
         mvisbk0=mvisbk
c ---    Set MFRH to EPA table for extracting RH from RHFAC, and
c ---    set MVISBK to 2 for using GROWTH function
         mfrh=3
         mvisbk=2
c ---    Loop over months
         do imo=1,12
            irhmonth(imo)=1
c ---       Increase rhfac by .0001 to allow for precision when
c ---       testing difference of reals
            fac=rhfac(imo)+.0001
            do k=1,100
               rk=FLOAT(k)*0.01
               if(GROWTH(rk).LE.fac) irhmonth(imo)=k
            enddo
         enddo
c ---    Restore current MFRH and MVISBK
         mfrh=mfrh0
         mvisbk=mvisbk0
c ---    Report results to list file
         write(io1,*)
         write(io1,*)'SETVISBK:  Method ',mvisbk
         write(io1,*)'Relative Humidity associated with RHFAC'
         write(io1,*)'Month   RHFAC   RH(%)'
            do imo=1,12
               write(io1,'(2x,i2,3x,f7.4,2x,i3)') imo,
     &                        rhfac(imo),irhmonth(imo)
            enddo
      endif

cc --- Development section of code
cc --- Compute monthly relative humidity associated with the RH Factors
cc --- for small, large, and sea salt particles for QA
c      if(LRHFAC .AND. mvisbk.EQ.8 .AND.
c     &   (m8_mode.EQ.4 .OR. m8_mode.EQ.5)) then
cc ---    Loop over months
c         do imo=1,12
c            irh_s(imo)=1
c            irh_l(imo)=1
c            irh_ss(imo)=1
cc ---       Increase rh factors by .0001 to allow for precision when
cc ---       testing difference of reals
c            fac_s=rhfsml(imo)+.0001
c            fac_l=rhflrg(imo)+.0001
c            fac_ss=rhfsea(imo)+.0001
c            do k=1,100
c               rk=FLOAT(k)*0.01
c               call FRH4(rk,f_s,f_l,f_ss)
c               if(f_s.LE.fac_s) irh_s(imo)=k
c               if(f_l.LE.fac_l) irh_l(imo)=k
c               if(f_ss.LE.fac_ss) irh_ss(imo)=k
c            enddo
c         enddo
cc ---    Report results to list file
c         write(io1,*)
c         write(io1,*)'SETVISBK:  Method ',mvisbk
c         write(io1,*)'Relative Humidity (%) associated with Factors'
c         write(io1,*)'(QA Information -- these RH values are not used)'
c         write(io1,*)'Month  FRH_SML  RH   FRH_LRG  RH   FRH_SEA  RH'
c            do imo=1,12
c               write(io1,'(3x,i2,3(2x,f7.4,2x,i3))') imo,
c     &                        rhfsml(imo),irh_s(imo),
c     &                        rhflrg(imo),irh_l(imo),
c     &                        rhfsea(imo),irh_ss(imo)
c            enddo
c      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine sunit(cg,cd,ct)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017             SUNIT
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Scale output values to user UNITS
c
c  UPDATES:
c  V5.0(981025) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.0(980821) to V5.0(981025)
c               (DGS) Process sub-range of gridded receptors
c  V5.0(960628) to V5.0(980821)
c               (DGS) Process sub-range of discrete receptors
c
c  ARGUMENTS:
c     PASSED:   cg    values at grid receptors                      [ra]
c               cd    values at discrete receptors                  [ra]
c               ct    values at terrain receptors                   [ra]
c   RETURNED:   same
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c
c  Declare dummy arrays
      real cg(mxgx,mxgy),cd(mxdrec),ct(mxctrec)
c
c  Scale sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            cd(i)=cd(i)*rscale
         enddo
         return
      endif
c
c  Section for gridded receptors
      if(ngx .GT. 0 .AND. LSGRID) then
c ---    Scale from standard units to user units
         do iy=jbgrid,jegrid
            do ix=ibgrid,iegrid
               cg(ix,iy)=cg(ix,iy)*rscale
            enddo
         enddo
      endif
c
c Section for discrete receptors
      if(ndrec .GT. 0) then
c ---    Scale from standard units to user units
         do i=1,ndrec
           if(ndrecp(i).EQ.1) cd(i)=cd(i)*rscale
         enddo
      endif
c
c Section for terrain receptors
      if(nctrec .GT. 0) then
c ---    Scale from standard units to user units
         do i=1,nctrec
            ct(i)=ct(i)*rscale
         enddo
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine cntxday(iarrg,iarrd,iarrt,myr,mjday,
     *                   ixc,ixcg,ixcd,ixct)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017           CNTXDAY
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Stores the daily exceedance count for each receptor
c
c --- UPDATES:
c     V5.2(991104a) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c     V5.1(990920) to V5.2(991104a)
c               (DGS) Halt with message when iday > mxday
c     V5.0(990228a)to V5.1(990920)
c               (DGS) Use YYYY format for year
c               (DGS) Replace integer*2 with integer (*4)
c               (DGS) Skip excluded gridded receptors
c               (DGS) Add LG,LD,LCT filters
c
c
c  ARGUMENTS:
c     PASSED:   iarr[g,d,t]     Cumulative exceedance count arrays  [ia]
c               myr,mjday       Year, Julian day of last hour of day [i]
c   RETURNED:   ixc             YYYYJJJ array                       [ia]
c               ixc[g,d,t]      Daily exceedance count arrays       [ia]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c
      integer   iarrg(mxgx,mxgy),iarrd(mxdrec),iarrt(mxctrec)
      integer   ixcg(0:mxday,mxgx,mxgy),ixcd(0:mxday,mxdrec),
     *          ixct(0:mxday,mxctrec)
      integer ixc(mxday)
c
      data iday/0/
c
c  Update the day counter (not a calendar day) at every entry
      iday=iday+1
c
c  Do not exceed MXDAY
      if(iday.GT.mxday) then
         write(io1,*) 'Parameter MXDAY exceeded.  Increase MXDAY in ',
     &                'your PARAMS.PST file and recompile CALPOST'
         write(io1,*) 'MXDAY must be >= number of days processed'
         write(io1,*) 'MXDAY = ',mxday
         write(io6,*) 'Parameter MXDAY exceeded.  Increase MXDAY in ',
     &                'your PARAMS.PST file and recompile CALPOST'
         write(io6,*) 'MXDAY must be >= number of days processed'
         write(io6,*) 'MXDAY = ',mxday
         stop 'CALPOST halted in CNTXDAY'
      endif
c
c  Assign date integer for current day to array
      ixc(iday)=myr*1000+mjday
c
c  Process sources and quit
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
            ixcd(iday,i)=iarrd(i)-ixcd(0,i)
            ixcd(0,i)=iarrd(i)
         enddo
         return
      endif
c
c  Process gridded receptors
      if(LSGRID .AND. LG) then
         do i=ibgrid,iegrid
            do j=jbgrid,jegrid
              if(ngrecp(i,j).EQ.1) then
                ixcg(iday,i,j)=iarrg(i,j)-ixcg(0,i,j)
                ixcg(0,i,j)=iarrg(i,j)
              endif
            enddo
         enddo
      endif
c
c  Process discrete receptors
      if(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
           if(ndrecp(i).EQ.1) then
             ixcd(iday,i)=iarrd(i)-ixcd(0,i)
             ixcd(0,i)=iarrd(i)
           endif
         enddo
      endif
c
c  Process complex terrain receptors
      if(nctrec .GT. 0 .AND. LCT) then
         do i=1,nctrec
             ixct(iday,i)=iarrt(i)-ixct(0,i)
             ixct(0,i)=iarrt(i)
         enddo
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine xcday(ixc,ixcg,ixcd,ixct)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017             XCDAY
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Processes daily exceedance count for each receptor and
c               reports violations to list file.  A violation occurs
c               at a receptor if more than NCOUNT exceedances of the
c               threshold concentration are predicted in a single
c               NDAY period.
c
c --- UPDATES:
c     V5.1(990920) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c     V5.0(990228a)to V5.1(990920)
c               (DGS) Use YYYY format for year
c               (DGS) Replace integer*2 with integer (*4)
c               (DGS) Skip excluded gridded receptors
c               (DGS) Add LG,LD,LCT filters
c
c
c  ARGUMENTS:
c     PASSED:   ixc             YYYYJJJ array                       [ia]
c               ixc[g,d,t]      Daily exceedance count arrays       [ia]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   WRIVIO
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'source.pst'
c
c  Declare arrays passed in argument list
      integer   ixcg(0:mxday,mxgx,mxgy),ixcd(0:mxday,mxdrec),
     *          ixct(0:mxday,mxctrec)
      integer ixc(mxday)
c
c  Declare arrays to store current series of days (window)
      integer   iwing(mxwin,mxgx,mxgy),iwind(mxwin,mxdrec),
     *          iwint(mxwin,mxctrec)
c
c  Declare arrays to store violations
      integer   ivg(mxgx,mxgy),ivd(mxdrec),ivt(mxctrec)
c
      logical lvio
      data iday/0/,kday/0/,lvio/.FALSE./
c
c  Set threshold for the selected averaging time
      thresh=-1.
      if(LXDAY1) then
         thresh=thresh1
         navper=1
      elseif(LXDAY3) then
         thresh=thresh3
         navper=3
      elseif(LXDAYN) then
         thresh=threshN
         navper=navg
      elseif(LXDAY24) then
         thresh=thresh24
         navper=24
      endif
c
c  Establish header for report to list-file
      write(io1,*)
      write(io1,*)
      write(io1,*)'VIOLATION REPORT ---------------------------------'
      write(io1,101) ncount,nday
101   format('(More than ',i3,' Exceedances in any ',i3,
     *          ' day period)')
      write(io1,*)
      write(io1,*)
      if(msrc.NE.2) then
         write(io1,*)'       YYYY JJJ HH  Receptor  Exceedances'
      else
         write(io1,*)'       YYYY JJJ HH  Source    Exceedances'
      endif
      write(io1,*)
c
c  Step through days in run
c --------------------------
c  Update the day counter (not a calendar day) at top of loop
10    iday=iday+1
      if(ixc(iday).EQ.0) goto 900
c
c  Update the slot index for current day in multiple-day window
      kday=kday+1
      if(kday.GT.nday) kday=1
c
c  Assign date-time for end-time of current day
      ih=0
      iyr=ixc(iday)/1000
      jul=ixc(iday)-iyr*1000
c
c  Process gridded receptors
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
         do i=ibgrid,iegrid
           do j=jbgrid,jegrid
             if(ngrecp(i,j).EQ.1) then
c ---          Place exceedance count for current day into window slot
               iwing(kday,i,j)=ixcg(iday,i,j)
c ---          Test for a violation at this receptor
               nexc=0
               do k=1,nday
                  nexc=nexc+iwing(k,i,j)
               enddo
               if(nexc.GT.ncount) then
                  lvio=.TRUE.
                  ivg(i,j)=ivg(i,j)+1
                  write(io1,'(i11,i4,i3,2x,a,2i4,3x,i6)') iyr,jul,ih,
     &                  'G',i,j,nexc
               endif
             endif
           enddo
         enddo
      endif
c
c  Process discrete receptors (or sources)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
         do i=1,nsrc
c ---       Place exceedance count for current day into window slot
            iwind(kday,i)=ixcd(iday,i)
c ---       Test for a violation
            nexc=0
            do k=1,nday
               nexc=nexc+iwind(k,i)
            enddo
            if(nexc.GT.ncount) then
              lvio=.TRUE.
              ivd(i)=ivd(i)+1
              write(io1,'(i11,i4,i3,2x,a16,2x,i6)') iyr,jul,ih,
     &                 csource(i),nexc
            endif
         enddo
c
      elseif(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
           if(ndrecp(i).EQ.1) then
c ---         Place exceedance count for current day into window slot
              iwind(kday,i)=ixcd(iday,i)
c ---         Test for a violation at this receptor
              nexc=0
              do k=1,nday
                 nexc=nexc+iwind(k,i)
              enddo
              if(nexc.GT.ncount) then
                 lvio=.TRUE.
                 ivd(i)=ivd(i)+1
                 write(io1,'(i11,i4,i3,2x,a,1x,i6,4x,i6)') iyr,jul,ih,
     &                 'D',i,nexc
              endif
           endif
         enddo
      endif
c
c  Process complex terrain receptors
      if(nctrec .GT. 0 .AND. LCT .AND. msrc.NE.2) then
         do i=1,nctrec
c ---       Place exceedance count for current day into window slot
            iwint(kday,i)=ixct(iday,i)
c ---       Test for a violation at this receptor
            nexc=0
            do k=1,nday
               nexc=nexc+iwint(k,i)
            enddo
            if(nexc.GT.ncount) then
               lvio=.TRUE.
               ivt(i)=ivt(i)+1
               write(io1,'(i11,i4,i3,2x,a,1x,i6,4x,i6)') iyr,jul,ih,
     &               'T',i,nexc
            endif
         enddo
      endif
c
c  Process next day
      goto 10

900   continue
c
c  All days are processed -- report number of violations
      if(LVIO) then
         call WRIVIO(navper,thresh,ivg,ivd,ivt)
      else
         write(io1,*)
         write(io1,*)' --- NO Violations Found! ---'
         write(io1,*)
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine wrivio(navper,thresh,ixg,ixd,ixt)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 031017            WRIVIO
c ---           D. Strimaitis, SRC
c
c  PURPOSE:     Writes Violation Count arrays to output file
c
c --- UPDATES:
c  V5.2(991104c) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c     V5.1(990920) to V5.2(991104c)
c               (DGS) Add RING processing output
c     V5.0(990228a)to V5.1(990920)
c               (DGS) Replace integer*2 with integer (*4)
c               (DGS) Skip excluded receptors (use null for gridded
c                     output)
c
c  ARGUMENTS:
c     PASSED:   navper          number of periods in average         [i]
c               thresh          threshold value                      [r]
c               ix[g,d,t]       violation counts                    [ia]
c   RETURNED:   none
c
c  CALLING ROUTINES:    XCDAY
c
c  EXTERNAL ROUTINES:   OUTIFX
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'ring.pst'
      INCLUDE 'source.pst'
c
      integer ixg(mxgx,mxgy),ixd(mxdrec),ixt(mxctrec)
      integer ixr(mxring)
      integer ixgp(mxgx,mxgy)
      character*70 messag0
c
c  Set null for excluded receptor
      data null/0/

      ichan=io1
c
      messag0=
     &'VIOLATION = MORE THAN   AVERAGES >            IN A   -DAY PERIOD'

      iavgpd=navper*mavg
c
c  Section for gridded receptors
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
c -- Identify species and level (or deposition)
         write(ichan,90) asplv
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,100) iavgpd,cdname,units
c -- Insert 'null' for excluded recptor
         do j=jbgrid,jegrid
           do i=ibgrid,iegrid
             if(ngrecp(i,j).EQ.0) then
               ixgp(i,j)=null
             else
               ixgp(i,j)=ixg(i,j)
             endif
           enddo
         enddo
c -- Create maps
         write(messag0(23:23),'(i1)') ncount
         write(messag0(35:45),'(e10.4)') thresh
         write(messag0(52:53),'(i2)') nday
         call OUTIFX(ixgp,4,messag0,ibgrid,jbgrid,iegrid,jegrid,ichan)
      endif
c
c  Section for discrete receptors (or sources)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      if(msrc.EQ.2) then
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,201) iavgpd,cdname,units
         write(ichan,101) thresh,asplv
c -- Write column headings
         write(ichan,111)
c -- Fill out table
         do i=1,nsrc
            write(ichan,220) i,xsrckm(i),ysrckm(i),ixd(i)
         enddo
c
      elseif(LD .AND. ndrec .NE. 0) then
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,200) iavgpd,cdname,units
         write(ichan,101) thresh,asplv
c -- Write column headings
         write(ichan,110)
c -- Fill out table
         do i=1,ndrec
            if(ndrecp(i).EQ.1) write(ichan,220) i,xrec(i),yrec(i),ixd(i)
         enddo
      endif
c
c  Section for discrete receptors reported by RING
      if(LD .AND. ndrec.NE.0 .AND. LDRING .AND. msrc.NE.2) then
c -- Initialize ring output
         do i=1,ndring
            ixr(i)=0
         enddo
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,400) iavgpd,cdname,units
         write(ichan,101) thresh,asplv
c -- Write column headings
         write(ichan,110)
c -- Search for max on each ring
         do i=1,ndrec
            if(ndrecp(i).EQ.1) then
               if(ixd(i).GT.ixr(idring(i))) ixr(idring(i))=ixd(i)
            endif
         enddo
c -- Fill out table
         do i=1,ndring
            if(nrrecp(i).EQ.1) write(ichan,420) i,rradkm(i),ixr(i)
         enddo
      endif
c
c  Section for terrain receptors
      if(LCT .AND. nctrec .NE. 0 .AND. msrc.NE.2) then
c -- Set title for table
         write(ichan,*) ' '
         write(ichan,300) iavgpd,cdname,units
         write(ichan,101) thresh,asplv
c -- Write column headings
         write(ichan,110)
c -- Fill out table
         do i=1,nctrec
            write(ichan,220) i,xctr(i),yctr(i),ixt(i)
         enddo
      endif
c
      return
90    format(t57,a15,/)
100   format(10x,'COUNTS OF ',i7,'-HOUR AVERAGE ',a14,' VIOLATIONS  ',
     * 'AT EACH GRIDDED RECEPTOR  ',a13/)
101   format(10x,'THRESHOLD VALUE = ',1pe10.4,t57,a15,/)
110   format(9x,'RECEPTOR',3x,'COORDINATES (km)',12X,'COUNT')
111   format(9x,'SOURCE  ',3x,'COORDINATES (km)',12X,'COUNT')
200   format(10x,'COUNTS OF ',i7,'-HOUR AVERAGE ',a14,' VIOLATIONS  ',
     * 'AT EACH DISCRETE RECEPTOR  ',a13/)
201   format(10x,'COUNTS OF ',i7,'-HOUR AVERAGE ',a14,' VIOLATIONS  ',
     * 'FOR EACH SOURCE  ',a13/)
220   format(7x,i7,3x,2f10.3,6x,i10)
300   format(10x,'COUNTS OF ',i7,'-HOUR AVERAGE ',a14,' VIOLATIONS  ',
     * 'AT EACH TERRAIN RECEPTOR  ',a13/)
400   format(10x,'COUNTS OF ',i7,'-HOUR AVERAGE ',a14,' VIOLATIONS  ',
     * 'FOR PEAK RECEPTOR ON EACH RING ',a13/)
420   format(7x,i7,'R',7x,f10.3,11x,i10)
      end
c-----------------------------------------------------------------------
      subroutine tseropen
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050915          TSEROPEN
c ---          D. Strimaitis    Earth Tech, Inc
c
c  PURPOSE:     Open File(s) for time-series
c
c  UPDATES:
c  V5.638(050408) to V6.1(050915)
c               (DGS) Account for non-hourly model averaging times
c  V5.62(040503) to V5.638(050408)
c               (DGS) Bug:  update NPATH to NTSPATH.  This was partially
c                     implemented so the path portion of the name only
c                     worked if npath=ntspath (typically 0)
c               (DGS) Use LTIME,LPEAK to configure timeseries files
c  V5.5(991104) to V5.62(040503)
c               (DGS) Update filename content
c
c  ARGUMENTS:
c     PASSED:   none
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   FILCASE
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'filnam.pst'
      INCLUDE 'head.pst'

      character*132 fname,fname1,fnamep
      character*5 afmt(10)
      character*6 avgper

c --- Set integer format array
      data afmt/'(i1)','(i2)','(i3)','(i4)','(i5)',
     &          '(i6)','(i7)','(i8)','(i9)','(i10)'/

c --- Return if no timeseries files are required
      if(.not.LTIME .AND. .not.LPEAK) return

c --- Set filename for plot-file 
c -------------------------------

c --- Set length of species name (left-justified)
      do k=1,12
         if(aspec(k:k) .NE. ' ') lenspec=k
      enddo
c --- Set length of NAVG time string
      if(navg .GE. 10000) then
         lenpd=5
      elseif(navg .GE. 1000) then
         lenpd=4
      elseif(navg .GE. 100) then
         lenpd=3
      elseif(navg .GE. 10) then
         lenpd=2
      else
         lenpd=1
      endif

c --- Establish full filename for a 1-hour average file
c -----------------------------------------------------
c --- Path
      if(ntspath.GT.0) fname1(1:ntspath)=tspath(1:ntspath)
c --- File type
      ipos=ntspath+1
      ipos2=ntspath+8
      fname1(ipos:ipos2)='TSERIES_'
c --- Species name
      ipos=ipos2+1
      ipos2=ipos2+lenspec
      fname1(ipos:ipos2)=aspec(1:lenspec)
      ipos2=ipos2+1
      fname1(ipos2:ipos2)='_'
c --- Averaging time
      mark=ipos2
      ipos=ipos2+1
      ipos2=ipos2+3
      fname1(ipos:ipos2)='1HR'
c --- Concentration or flux
      ipos=ipos2+1
      ipos2=ipos2+5
      if(ilayer .EQ. -1) then
         fname1(ipos:ipos2)='_DFLX'
      elseif(ilayer .EQ. -2) then
         fname1(ipos:ipos2)='_WFLX'
      elseif(ilayer .EQ. -3) then
         fname1(ipos:ipos2)='_TFLX'
      else
         fname1(ipos:ipos2)='_CONC'
      endif
c --- User characters (optional)
      if(ntsunam.GT.0) then
         ipos2=ipos2+1
         fname1(ipos2:ipos2)='_'
         ipos=ipos2+1
         ipos2=ipos2+ntsunam
         fname1(ipos:ipos2)=tsunam(1:ntsunam)
      endif
c --- Extension
      ipos=ipos2+1
      ipos2=ipos2+4
      fname1(ipos:ipos2)='.DAT'
c --- Fill out with blanks
      npos=ipos2+1
      do i=npos,132
         fname1(i:i)=' '
      enddo

c --- Establish full filename for a 1-period average file
c -------------------------------------------------------
c --- Set averaging time information
      npd=1
      if(npd.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(npd.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(npd.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=npd*mavg
         avgper=avtime
      endif
      if(iavgpd .GE. 10000) then
         lenpd=5
      elseif(iavgpd .GE. 1000) then
         lenpd=4
      elseif(iavgpd .GE. 100) then
         lenpd=3
      elseif(iavgpd .GE. 10) then
         lenpd=2
      else
         lenpd=1
      endif

c --- Path
      if(ntspath.GT.0) fnamep(1:ntspath)=tspath(1:ntspath)
c --- File type
      ipos=npath+1
      ipos2=npath+8
      fnamep(ipos:ipos2)='TSERIES_'
c --- Species name
      ipos=ipos2+1
      ipos2=ipos2+lenspec
      fnamep(ipos:ipos2)=aspec(1:lenspec)
      ipos2=ipos2+1
      fnamep(ipos2:ipos2)='_'
c --- Averaging time
      mark1=ipos2
      ipos=ipos2+1
      ipos2=ipos2+lenpd
      write(fnamep(ipos:ipos2),afmt(lenpd)) iavgpd
      ipos=ipos2+1
      if(avgper.EQ.'  HOUR') then
         ipos2=ipos2+2
         fnamep(ipos:ipos2)='HR'
      elseif(avgper.EQ.'MINUTE') then
         ipos2=ipos2+3
         fnamep(ipos:ipos2)='MIN'
      elseif(avgper.EQ.'SECOND') then
         ipos2=ipos2+3
         fnamep(ipos:ipos2)='SEC'
      endif
      mark2=ipos2
c --- Concentration or flux
      ipos=ipos2+1
      ipos2=ipos2+5
      if(ilayer .EQ. -1) then
         fnamep(ipos:ipos2)='_DFLX'
      elseif(ilayer .EQ. -2) then
         fnamep(ipos:ipos2)='_WFLX'
      elseif(ilayer .EQ. -3) then
         fnamep(ipos:ipos2)='_TFLX'
      else
         fnamep(ipos:ipos2)='_CONC'
      endif
c --- User characters (optional)
      if(ntsunam.GT.0) then
         ipos2=ipos2+1
         fnamep(ipos2:ipos2)='_'
         ipos=ipos2+1
         ipos2=ipos2+ntsunam
         fnamep(ipos:ipos2)=tsunam(1:ntsunam)
      endif
c --- Extension
      ipos=ipos2+1
      ipos2=ipos2+4
      fnamep(ipos:ipos2)='.DAT'
c --- Fill out with blanks
      npos=ipos2+1
      do i=npos,132
         fnamep(i:i)=' '
      enddo


c --- Open file for each averaging time
c -------------------------------------
      i1=ntspath+1
      i2=ntspath+8
      if(L1PD) then
         fname=fnamep
         if(LTIME) then
            fname(i1:i2)='TSERIES_'
            call FILCASE(lcfiles,fname)
            open(iotp,file=fname,status='unknown')
         endif
         if(LPEAK) then
            fname(i1:i2)='PEAKVAL_'
            call FILCASE(lcfiles,fname)
            open(iopp,file=fname,status='unknown')
         endif
      endif
      if(L1HR) then
         fname=fname1
         if(LTIME) then
            fname(i1:i2)='TSERIES_'
            call FILCASE(lcfiles,fname)
            open(iot1,file=fname,status='unknown')
         endif
         if(LPEAK) then
            fname(i1:i2)='PEAKVAL_'
            call FILCASE(lcfiles,fname)
            open(iop1,file=fname,status='unknown')
         endif
      endif
      if(L3HR) then
         fname=fname1
         ipos=mark+1
         ipos2=mark+1
         fname(ipos:ipos2)='3'
         if(LTIME) then
            fname(i1:i2)='TSERIES_'
            call FILCASE(lcfiles,fname)
            open(iot3,file=fname,status='unknown')
         endif
         if(LPEAK) then
            fname(i1:i2)='PEAKVAL_'
            call FILCASE(lcfiles,fname)
            open(iop3,file=fname,status='unknown')
         endif
      endif
      if(L24HR) then
         fname=fname1
         ipos=mark+1
         ipos2=mark+2
         fname(ipos:ipos2)='24'
         do i=ipos2+1,132
            k=i-1
            fname(i:i)=fname1(k:k)
         enddo
         if(LTIME) then
            fname(i1:i2)='TSERIES_'
            call FILCASE(lcfiles,fname)
            open(iot24,file=fname,status='unknown')
         endif
         if(LPEAK) then
            fname(i1:i2)='PEAKVAL_'
            call FILCASE(lcfiles,fname)
            open(iop24,file=fname,status='unknown')
         endif
      endif
      if(LNAVG) then
c ---    Set averaging time information
         npd=navg
         if(npd.EQ.iper1) then
            iavgpd=1
            avgper='  HOUR'
         elseif(npd.EQ.iper3) then
            iavgpd=3
            avgper='  HOUR'
         elseif(npd.EQ.iper24) then
            iavgpd=24
            avgper='  HOUR'
         else
            iavgpd=npd*mavg
            avgper=avtime
         endif
         if(iavgpd .GE. 10000) then
            lenpd=5
         elseif(iavgpd .GE. 1000) then
            lenpd=4
         elseif(iavgpd .GE. 100) then
            lenpd=3
         elseif(iavgpd .GE. 10) then
            lenpd=2
         else
            lenpd=1
         endif
c ---    Patch into 1-Pd name
         fname=fnamep
         ipos=mark1+1
         ipos2=mark1+lenpd
         write(fname(ipos:ipos2),afmt(lenpd)) iavgpd
         ipos=ipos2+1
         if(avgper.EQ.'  HOUR') then
            ipos2=ipos2+2
            fname(ipos:ipos2)='HR'
         elseif(avgper.EQ.'MINUTE') then
            ipos2=ipos2+3
            fname(ipos:ipos2)='MIN'
         elseif(avgper.EQ.'SECOND') then
            ipos2=ipos2+3
            fname(ipos:ipos2)='SEC'
         endif
c ---    Pick up the rest of the name
         k=mark2
         do i=ipos2+1,132
            if(k.LT.132) k=k+1
            fname(i:i)=fnamep(k:k)
         enddo
         if(LTIME) then
            fname(i1:i2)='TSERIES_'
            call FILCASE(lcfiles,fname)
            open(iotn,file=fname,status='unknown')
         endif
         if(LPEAK) then
            fname(i1:i2)='PEAKVAL_'
            call FILCASE(lcfiles,fname)
            open(iopn,file=fname,status='unknown')
         endif
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine tserset(io)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050408            TSERSET
c ---           D. Strimaitis    Earth Tech, Inc
c
c  UPDATES:
c  V5.6(031017) to V5.638(050408)
c               (DGS) Add LPEAK processing
c  V5.2(991104c) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c  V5.2(991104) to V5.2(991104c)
c               (DGS) Add LDRING option
c
c  V5.1(990920) to V5.2(991104)
c               (JSS) Error messages written to list file
c                     in addition to screen and time-series file
c
c  PURPOSE:     Set up data for time-series output
c
c  ARGUMENTS:
c     PASSED:   io      channel number for output file               [i]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ring.pst'
      INCLUDE 'tser.pst'
      INCLUDE 'source.pst'

c --- Count number of receptors of each type selected for output
c --- Process gridded receptors
      ngtser=0
      if(LSGRID .AND. LG .AND. msrc.NE.2) then
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(ngrecp(i,j).EQ.1) ngtser=ngtser+1
            enddo
         enddo
      endif

c --- Process discrete receptors (or sources)
c --- Source TRACEBACK uses discrete receptor array to store impact of
c --- each source at ONE designated receptor
      ndtser=0
      nrtser=0
      nstser=0
      if(msrc.EQ.2) then
c ---    Processing sources
         nstser=nsrc
      elseif(ndrec .GT. 0 .AND. LD .AND. LDRING) then
c ---    Discrete receptors are reported by ring
         do i=1,ndring
           if(nrrecp(i).EQ.1) nrtser=nrtser+1
         enddo
      elseif(ndrec .GT. 0 .AND. LD) then
         do i=1,ndrec
           if(ndrecp(i).EQ.1) ndtser=ndtser+1
         enddo
      endif

c --- Process complex terrain receptors
      nttser=0
      if(LCTSG .AND. msrc.NE.2) nttser=nctrec

c --- Set information before QA checks
      ntser=ngtser+ndtser+nttser+nrtser+nstser
      nrpr=1
      nrpt=1

c --- No further work is needed for LPEAK option, so return if LTIME=F
      if(.not.LTIME) return

c --- Check number against parameter limit
      if(ntser.GT.mxtser) then
c ---    Write error message to time-series file
         write(io,*)
         write(io,*) '****************    FATAL    *****************'
         write(io,*) 'Number of receptors selected for Time-series'
         write(io,*) 'exceeds the maximum number allocated (MXTSER)'
         write(io,*) 'NTSER = ',ntser,' MXTSER = ',mxtser
         write(io,*)
         write(io,*) ' Gridded Receptors: ',ngtser
         write(io,*) 'Discrete Receptors: ',ndtser
         write(io,*) '    RING Receptors: ',nrtser
         write(io,*) '    CTSG Receptors: ',nttser
         write(io,*) '           Sources: ',nstser
         write(io,*)
         write(io,*) 'Select fewer receptors, OR increase the maximum'
         write(io,*) 'number allocated (MXTSER) in the parameter file'
         write(io,*) 'and recompile CALPOST'
c ---    Write error message to list file
         write(io6,*)
         write(io6,*) '****************    FATAL    *****************'
         write(io6,*) 'Number of receptors selected for Time-series'
         write(io6,*) 'exceeds the maximum number allocated (MXTSER)'
         write(io6,*) 'NTSER = ',ntser,' MXTSER = ',mxtser
         write(io6,*)
         write(io6,*) ' Gridded Receptors: ',ngtser
         write(io6,*) 'Discrete Receptors: ',ndtser
         write(io6,*) '    RING Receptors: ',nrtser
         write(io6,*) '    CTSG Receptors: ',nttser
         write(io6,*) '           Sources: ',nstser
         write(io6,*)
         write(io6,*) 'Select fewer receptors, OR increase the maximum'
         write(io6,*) 'number allocated (MXTSER) in the parameter file'
         write(io6,*) 'and recompile CALPOST'

         write(*,*) 'TSERSET: Fatal Error - see List file for details'
         stop
      endif

c --- Place receptor information in arrays
      k=0
      if(ngtser.GT.0) then
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(ngrecp(i,j).EQ.1) then
                  k=k+1
                  atser(k)='GRID'
                  ixtser(k)=i
                  iytser(k)=j
                  xtser(k)=xgrd(i,j)
                  ytser(k)=ygrd(i,j)
               endif
            enddo
         enddo
      endif
c
      if(nstser.GT.0) then
         do i=1,nsrc
            k=k+1
            atser(k)='SRC '
            ixtser(k)=i
            iytser(k)=0
            xtser(k)=xsrckm(i)
            ytser(k)=ysrckm(i)
         enddo
      elseif(nrtser.GT.0) then
         do i=1,ndring
            if(nrrecp(i).EQ.1) then
               k=k+1
               atser(k)='RING'
               ixtser(k)=i
               iytser(k)=0
               xtser(k)=rradkm(i)
               ytser(k)=0.
            endif
         enddo
      elseif(ndtser.GT.0) then
         do i=1,ndrec
            if(ndrecp(i).EQ.1) then
               k=k+1
               atser(k)='DISC'
               ixtser(k)=i
               iytser(k)=0
               xtser(k)=xrec(i)
               ytser(k)=yrec(i)
            endif
         enddo
      endif
c
      if(nttser.GT.0) then
         do i=1,nctrec
            k=k+1
            atser(k)='CTSG'
            ixtser(k)=i
            iytser(k)=0
            xtser(k)=xctr(i)
            ytser(k)=yctr(i)
         enddo
      endif

c --- Check total again
      if(k.NE.ntser) then
         write(io6,*)'Unknown problem in TSERSET!'
         write(io6,*)'Total number of receptors for time-series:'
         write(io6,*)'NTSER = ',ntser
         write(io6,*)'    K = ',k
         write(io6,*)'Cumulative counter K should equal NTSER'
c
         write(*,*)'Unknown problem in TSERSET!'
         write(*,*)'Total number of receptors for time-series:'
         write(*,*)'NTSER = ',ntser
         write(*,*)'    K = ',k
         write(*,*)'Cumulative counter K should equal NTSER'
         stop
      endif

c --- Configure the number of records written each time to the
c --- time-series file (format currently allows up to 100 receptors
c --- in a record)
      nrpr=100
      nrpt=1+((ntser-1)/nrpr)

      return
      end
c-----------------------------------------------------------------------
      subroutine tser_hd(io,ntsavg)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012           TSER_HD
c ---          D. Strimaitis    Earth Tech, Inc
c
c  PURPOSE:     Write header information describing time-series output
c
c --- UPDATES:
c     V6.1(050915) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c     V5.6(031017) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c     V5.2(991104) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c     V5.1(990920) to V5.2(991104)
c               (DGS) Implement multiple time-series files
c
c  ARGUMENTS:
c     PASSED:    io      output unit number                          [i]
c              ntsavg    averaging periods                           [r]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'tser.pst'
      INCLUDE 'source.pst'

      character*6 avgper

c --- Set averaging time information
      if(ntsavg.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(ntsavg.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(ntsavg.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=ntsavg*mavg
         avgper=avtime
      endif

c --- Set title for table
      if(msrc.NE.2) then
         write(io,*) 'TIME-SERIES Output  --------  ',asplv
         write(io,*)
         write(io,100) iavgpd,avgper,cdname,units,ntser
      else
         if(LGS) then        
            xkm=xgrd(isrec,jsrec)
            ykm=ygrd(isrec,jsrec)
         elseif(LDS) then
            xkm=xrec(isrec)
            ykm=yrec(isrec)
         else
            xkm=-999.
            ykm=-999.
         endif
         write(io,*) 'TIME-SERIES Output  --------  ',asplv
         write(io,*) 'RECEPTOR (x,y) km ',xkm,ykm
         write(io,101) iavgpd,avgper,cdname,units,ntser
      endif
      write(io,*)

      if(ntser.LE.nrpr) then
c ---    Configuration for up to nrpr=100 receptors
         write(io,120)' Type:',(atser(k),k=1,ntser)
         write(io,121)'   ix:',(ixtser(k),k=1,ntser)
         write(io,121)'   iy:',(iytser(k),k=1,ntser)
         write(io,122)'x(km):',(xtser(k),k=1,ntser)
         write(io,122)'y(km):',(ytser(k),k=1,ntser)
      else
c ---    Configuration for more than nrpr=100 receptors
         write(io,120)' Type:',(atser(k),k=1,nrpr)
         write(io,220)         (atser(k),k=nrpr+1,ntser)
         write(io,121)'   ix:',(ixtser(k),k=1,nrpr)
         write(io,221)         (ixtser(k),k=nrpr+1,ntser)
         write(io,121)'   iy:',(iytser(k),k=1,nrpr)
         write(io,221)         (iytser(k),k=nrpr+1,ntser)
         write(io,122)'x(km):',(xtser(k),k=1,nrpr)
         write(io,222)         (xtser(k),k=nrpr+1,ntser)
         write(io,122)'y(km):',(ytser(k),k=1,nrpr)
         write(io,222)         (ytser(k),k=nrpr+1,ntser)
      endif
      write(io,*)
      write(io,'(a28)') ' YYYY JDY HHMM (START time) '
      write(io,*)

      return

100   format(5x,i4,a7,' Average ',a14,' Values ',
     *       'at Selected Receptors  ',a13/,
     *       i9,'   Receptors are Included')
101   format(5x,i4,a7,' Average ',a14,' Values ',
     *       'for Selected Sources    ',a13/,
     *       i9,'   Sources are Included')
110   format(3(10(10i1,3x)/),6(10i1,3x),6i1)
120   format(a16,100a14)
121   format(a16,100i14)
122   format(a16,1p,100e14.6)
220   format(16x,100a14)
221   format(16x,100i14)
222   format(16x,1p,100e14.6)

      end
c-----------------------------------------------------------------------
      subroutine peak_hd(io,ntsavg)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012           PEAK_HD
c ---          D. Strimaitis    Earth Tech, Inc
c
c  PURPOSE:     Write header information describing time-series output
c               of peak values
c
c --- UPDATES:
c     V6.1(050915) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c     V5.638(050408) to V6.1(050915)
c               (DGS) Identify standard averaging times from IPER1,
c                     IPER3,IPER24
c
c  ARGUMENTS:
c     PASSED:   io      output unit number                           [i]
c               avcd    averaging periods                            [r]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'tser.pst'
      INCLUDE 'source.pst'

      character*6 avgper

c --- Set averaging time information
      if(ntsavg.EQ.iper1) then
         iavgpd=1
         avgper='  HOUR'
      elseif(ntsavg.EQ.iper3) then
         iavgpd=3
         avgper='  HOUR'
      elseif(ntsavg.EQ.iper24) then
         iavgpd=24
         avgper='  HOUR'
      else
         iavgpd=ntsavg*mavg
         avgper=avtime
      endif

c --- Set title for table
      if(msrc.NE.2) then
         write(io,*) 'PEAK VALUE Output  --------  ',asplv
         write(io,*)
         write(io,100) iavgpd,avgper,cdname,units,ntser
      else
         if(LGS) then        
            xkm=xgrd(isrec,jsrec)
            ykm=ygrd(isrec,jsrec)
         elseif(LDS) then
            xkm=xrec(isrec)
            ykm=yrec(isrec)
         else
            xkm=-999.
            ykm=-999.
         endif
         write(io,*) 'PEAK VALUE Output  --------  ',asplv
         write(io,*) 'RECEPTOR (x,y) km ',xkm,ykm
         write(io,101) iavgpd,avgper,cdname,units,ntser
      endif
      write(io,*)

      write(io,*) 'PD START TIME'
      write(io,*) 'YYYY JDY HHMM   (I , J)  T     X(km)      Y(km)  '
      write(io,*)

      return

100   format(5x,i4,a7,' Average ',a14,' Values ',
     *       'at Selected Receptors  ',a13/,
     *       i9,'   Receptors are Included')
101   format(5x,i4,a7,' Average ',a14,' Values ',
     *       'for Selected Sources    ',a13/,
     *       i9,'   Sources are Included')

      end
c-----------------------------------------------------------------------
      subroutine tserout(io,avcg,avcd,avct,avcr,myr,mjday,mhr,msec)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012           TSEROUT
c ---          D. Strimaitis    Earth Tech, Inc
c
c  PURPOSE:     Write time-series output record
c
c --- UPDATES:
c     V6.1(050915) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c     V5.6(031017) to V6.1(050915)
c               (DGS) Add seconds to end-time
c     V5.2(991104c) to V5.6(031017)
c               (DGS) Add source TRACEBACK (MSRC=2)
c     V5.2(991104) to V5.2(991104c)
c               (DGS) Add RING receptors
c     V5.1(990920) to V5.2(991104)
c               (DGS) Add unit number to argument list
c
c  ARGUMENTS:
c     PASSED:   io      file unit number for output                  [i]
c               avcg    average concentrations at gridded receptors [ra]
c               avcd    average concentrations at discrete receptors[ra]
c               avct    average concentrations at CTSG receptors    [ra]
c               avcr    peak average concentration on receptor rings[ra]
c               myr     year (YYYY)                                  [i]
c               mjday   Julian day (JJJ)                             [i]
c               mhr     hour (HH)  - start of averaging period       [i]
c               msec    second (SSSS)  - start of averaging period   [i]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ring.pst'
      INCLUDE 'tser.pst'
      INCLUDE 'source.pst'

c --- Declare local arrays
      real avcg(mxgx,mxgy),avcd(mxdrec),avct(mxctrec),avcr(mxring)
      real avtser(mxtser)

c --- Build array for output record (order must match TSERSET)
c --- Gridded receptors
      klast=0
      if(ngtser.GT.0) then
         k1=klast+1
         klast=klast+ngtser
         do kg=k1,klast
            avtser(kg)=avcg(ixtser(kg),iytser(kg))
         enddo
      endif
c --- Ring OR Discrete receptors
      if(nstser.GT.0) then
         k1=klast+1
         klast=klast+nstser
         do k=k1,klast
            avtser(k)=avcr(ixtser(k))
         enddo
      elseif(nrtser.GT.0) then
         k1=klast+1
         klast=klast+nrtser
         do k=k1,klast
            avtser(k)=avcr(ixtser(k))
         enddo
      elseif(ndtser.GT.0) then
         k1=klast+1
         klast=klast+ndtser
         do k=k1,klast
            avtser(k)=avcd(ixtser(k))
         enddo
      endif
c --- CTSG receptors
      if(nttser.GT.0) then
         k1=klast+1
         klast=klast+nttser
         do k=k1,klast
            avtser(k)=avct(ixtser(k))
         enddo
      endif

c --- Change hour (HH) to time (HHMM)
      mtime=100*mhr+msec/60

c --- Write record(s)
      if(ntser.LE.nrpr) then
c ---    Configuration for up to nrpr=100 receptors
         write(io,122) myr,mjday,mtime,(avtser(k),k=1,ntser)
      else
c ---    Configuration for more than nrpr=100 receptors
         write(io,122) myr,mjday,mtime,(avtser(k),k=1,nrpr)
         write(io,222)                 (avtser(k),k=nrpr+1,ntser)
      endif

      return

122   format(i5,i4,i5.4,2x,1p,100e14.6)
222   format(16x,1p,100e14.6)

      end

c-----------------------------------------------------------------------
      subroutine peakout(io,avcg,avcd,avct,avcr,myr,mjday,mhr,msec)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 051012           PEAKOUT
c ---          D. Strimaitis    Earth Tech, Inc
c
c  PURPOSE:     Write time-series output record for peak value
c
c --- UPDATES:
c     V6.1(050915) to V6.11(051012)
c               (DGS) Switch from ending time to starting time
c     V5.638(050408) to V6.1(050915)
c               (DGS) Add seconds to end-time
c
c  ARGUMENTS:
c     PASSED:   io      file unit number for output                  [i]
c               avcg    average concentrations at gridded receptors [ra]
c               avcd    average concentrations at discrete receptors[ra]
c               avct    average concentrations at CTSG receptors    [ra]
c               avcr    peak average concentration on receptor rings[ra]
c               myr     year (YYYY)                                  [i]
c               mjday   Julian day (JJJ)                             [i]
c               mhr     hour (HH)  - start of averaging period       [i]
c               msec    second (SSSS)  - start of averaging period   [i]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ring.pst'
      INCLUDE 'source.pst'

c --- Declare locals
      real avcg(mxgx,mxgy),avcd(mxdrec),avct(mxctrec),avcr(mxring)
      character*1 cpk

c --- Cycle through receptors to identify peak
      ipk=0
      jpk=0
      val=0.0
      cpk=' '
      xpk=0.0
      ypk=0.0

c --- Gridded receptors
      if(LSGRID .AND. LG) then
         do j=jbgrid,jegrid
            do i=ibgrid,iegrid
               if(ngrecp(i,j).GT.0 .AND. val.LT.avcg(i,j)) then
                  val=avcg(i,j)
                  cpk='G'
                  ipk=i
                  jpk=j
               endif
            enddo
         enddo
      endif

c --- Ring receptors
      if(LDRING .AND. ndring.GT.0) then
         do i=1,ndring
            if(nrrecp(i).GT.0 .AND. val.LT.avcr(i)) then
               val=avcr(i)
               cpk='R'
               ipk=i
               jpk=0
            endif
         enddo
      endif

c --- Discrete receptors
      if(LD .AND. ndrec.GT.0) then
         do i=1,ndrec
            if(ndrecp(i).GT.0 .AND. val.LT.avcd(i)) then
               val=avcd(i)
               cpk='D'
               ipk=i
               jpk=0
            endif
         enddo
      endif


c --- CTSG receptors
      if(LCT .AND. nctrec.GT.0) then
         do i=1,nctrec
            if(val.LT.avct(i)) then
               val=avct(i)
               cpk='T'
               ipk=i
               jpk=0
            endif
         enddo
      endif

c --- Identify the xy location of the peak
      if(cpk.EQ.'G') then
         xpk=xgrd(ipk,jpk)
         ypk=ygrd(ipk,jpk)
      elseif(cpk.EQ.'D') then
         xpk=xrec(ipk)
         ypk=yrec(ipk)
      elseif(cpk.EQ.'T') then
         xpk=xctr(ipk)
         ypk=yctr(ipk)
      endif

c --- Change hour (HH) to time (HHMM)
      mtime=100*mhr+msec/60

c --- Write record
      write(io,122) myr,mjday,mtime,ipk,jpk,cpk,xpk,ypk,val

      return

122   format(i5,i4,i5.4,2x,2i4,a3,2f11.3,1p,e14.6)

      end

c-----------------------------------------------------------------------
      subroutine ringset(io)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 991104c          RINGSET
c ---           D. Strimaitis    Earth Tech, Inc
c
c
c  PURPOSE:     Set up data for ring-receptor output
c               (discrete receptors)
c
c  ARGUMENTS:
c     PASSED:   io      channel number for output file               [i]
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   LRSAME
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ring.pst'

      real xm(3),ym(3),rsq(3)
      logical LRSAME
      external LRSAME

      data r2d/57.29578/

c --- Set fractional tolerance in R^2 allowed for receptors on the
c --- same ring
      data frsq/0.02/

      if(LDEBUG) then
         write(io,*)
         write(io,*)
         write(io,*)'RINGSET --- Computed Receptor Ring Data'
      endif

c --- Initialize ring index and discrete receptor index
      ndring=0
      idrec=0

c --- Initialize process-ring flag to 0 (do not process)
      do i=1,mxring
         nrrecp(i)=0
      enddo

c --- Loop over discrete receptors
10    continue
c ---    Check for presence of 3 initial receptors on ring
         idrec0=idrec
         idrecp3=idrec0+3
         if(idrecp3.LE.ndrec) then
            ndring=ndring+1
            if(ndring.GT.mxring) then
               write(*,*)
               write(*,*)'RINGSET: Fatal Error - see List file'
               write(io,*)
               write(io,*)'RINGSET:  Fatal Error'
               write(io,*)'Too many rings found: ',ndring
               write(io,*)'    Parameter MXRING: ',mxring
               write(io,*)'Increase MXRING and recompile CALPOST'
               write(io,*)'or use fewer rings in CALPUFF'
               stop
            endif

c ---       Compute center of ring relative to first receptor
            xm(1)=0.0
            ym(1)=0.0
            rsq(1)=0.0
            id1=idrec0+1
            do i=2,3
               id=idrec0+i
               xm(i)=xrec(id)-xrec(id1)
               ym(i)=yrec(id)-yrec(id1)
               rsq(i)=xm(i)**2+ym(i)**2
            enddo
            dx12=xm(1)-xm(2)
            dx31=xm(3)-xm(1)
            dy12=ym(1)-ym(2)
            dy31=ym(3)-ym(1)
            x0m=0.5*(rsq(2)*dy31 + rsq(3)*dy12)/
     &              (xm(2)*dy31 +  xm(3)*dy12)
            y0m=0.5*(rsq(2)*dx31 + rsq(3)*dx12)/
     &              (ym(2)*dx31 +  ym(3)*dx12)

c ---       Shift center to original UTMs
            x0=x0m+xrec(id1)
            y0=y0m+yrec(id1)

c ---       Radius of ring (km)
            rsq0=x0m**2+y0m**2
            rradkm(ndring)=SQRT(rsq0)

c ---       Stepping angle (degrees CW from North)
            iang1=NINT(r2d*ASIN((xm(1)-x0m)/rradkm(ndring)))
            iang2=NINT(r2d*ASIN((xm(2)-x0m)/rradkm(ndring)))
            iang3=NINT(r2d*ASIN((xm(3)-x0m)/rradkm(ndring)))
            iang21=iang2-iang1
            iang32=iang3-iang2

c --- QA
c            if(iang1.NE.iang21 .OR. iang1.NE.iang32) then
            if(iang21.NE.iang32) then
               write(*,*)
               write(*,*)'RINGSET: Fatal Error - see List file'
               write(io,*)
               write(io,*)'RINGSET:  Fatal Error'
               write(io,*)'Stepping angle is not consistent!'
               write(io,*)'Ring Radius (km):  ',rradkm(ndring)
               write(io,*)'Angle Rec 1 (CW):  ',iang1
               write(io,*)'Angle Rec 2 (CW):  ',iang2
               write(io,*)'Angle Rec 3 (CW):  ',iang3
               stop
            endif

c ---       Number of receptors in ring
            nrrec=360/iang21
c ---       Allow for adding last receptor on "north"
            last=nrrec*iang21
            if(last.LT.360) nrrec=nrrec+1

c --- QA
            ntotal=idrec0+nrrec
            if(ntotal.GT.ndrec) then
               write(*,*)
               write(*,*)'RINGSET: Fatal Error - see List file'
               write(io,*)
               write(io,*)'RINGSET:  Fatal Error'
               write(io,*)'Too many receptors expected in this ring!'
               write(io,*)'Number at start:  ',idrec0
               write(io,*)' Number in ring:  ',nrrec
               write(io,*)' Expected total:  ',ntotal
               write(io,*)'   Total in run:  ',ndrec
               stop
            endif

c ---       Refine estimate of ring center and ring radius by using
c ---       3 distributed receptor locations in ring
c -----------------------------------------------------------------
            njump=nrrec/3
            id2=idrec0+njump
            id3=id2+njump
c ---       Compute center of ring relative to first receptor
            xm(2)=xrec(id2)-xrec(id1)
            ym(2)=yrec(id2)-yrec(id1)
            rsq(2)=xm(2)**2+ym(2)**2
            xm(3)=xrec(id3)-xrec(id1)
            ym(3)=yrec(id3)-yrec(id1)
            rsq(3)=xm(3)**2+ym(3)**2
            dx12=xm(1)-xm(2)
            dx31=xm(3)-xm(1)
            dy12=ym(1)-ym(2)
            dy31=ym(3)-ym(1)
            x0m=0.5*(rsq(2)*dy31 + rsq(3)*dy12)/
     &              (xm(2)*dy31 +  xm(3)*dy12)
            y0m=0.5*(rsq(2)*dx31 + rsq(3)*dx12)/
     &              (ym(2)*dx31 +  ym(3)*dx12)

c ---       Shift center to original UTMs
            x0=x0m+xrec(id1)
            y0=y0m+yrec(id1)

c ---       Radius of ring (km)
            rsq0=x0m**2+y0m**2
            rradkm(ndring)=SQRT(rsq0)

c ---       Report information for current ring
            if(LDEBUG) then
               write(io,*)
               write(io,*)'Ring         (#):  ',ndring
               write(io,*)'Ring Center (km):  ',x0,y0
               write(io,*)'Ring Radius (km):  ',rradkm(ndring)
               write(io,*)'Step Angle  (CW):  ',iang1
               write(io,*)'Number Ring Recs:  ',nrrec
            endif

c ---       Process ring and check for correct radius
            rbar=0.0
            do i=1,nrrec
               idrec=idrec+1
               idring(idrec)=ndring
               if(ndrecp(idrec).EQ.1) nrrecp(ndring)=1
               rsqi=(xrec(idrec)-x0)**2+(yrec(idrec)-y0)**2
               if(.NOT.LRSAME(frsq,rsq0,rsqi)) then
                  rexp=SQRT(rsq0)
                  rfnd=SQRT(rsqi)
                  ir=idrec-idrec0
                  write(*,*)
                  write(*,*)'RINGSET: Fatal Error - see List file'
                  write(io,*)
                  write(io,*)'RINGSET:  Fatal Error'
                  write(io,*)'Unexpected ring radius found'
                  write(io,*)'Ring, Receptor (#):  ',ndring,ir
                  write(io,*)'        Expected R:  ',rexp
                  write(io,*)'           Found R:  ',rfnd
                  stop
               else
                  rbar=rbar+SQRT(rsqi)
               endif
            enddo

c ---       Replace computed ring radius with the average radius
            rradkm(ndring)=rbar/nrrec

c ---       Check for more rings
            goto 10

         endif
c --- End of loop over discrete receptors

      return
      end

c-----------------------------------------------------------------------
      subroutine avgring(io,avcd,avcr)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 991104c          AVGRING
c ---           D. Strimaitis, Earth Tech
c
c  PURPOSE:     Extract discrete receptor results by ring
c
c
c  ARGUMENTS:
c     PASSED:   io      channel number for output file               [i]
c               avcd    average concentrations at discrete receptors[ra]
c               avcr    peak of average concentration on rings      [ra]
c
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'ring.pst'
c
c  Declare dummy arrays
      real avcd(mxdrec),avcr(mxring)
      logical problem
c
c  Set initial value for ring array
      data vinit/-9.99e25/
c
c  Null value for filling excluded receptor ring
      data anull/0.0/

c  Initialize ring output
      do i=1,ndring
         avcr(i)=vinit
      enddo

c  Process active discrete receptors
      do i=1,ndrec
         if(ndrecp(i).EQ.1) then
            avcr(idring(i))=AMAX1(avcd(i),avcr(idring(i)))
         endif
      enddo

c  QA results and impose null value for excluded rings
      problem=.FALSE.
      do i=1,ndring
         if(nrrecp(i).EQ.0 .AND. avcr(i).EQ.vinit) then
c ---       Consistent result for excluded ring (OK)
            avcr(i)=anull
         elseif(nrrecp(i).EQ.1 .AND. avcr(i).GT.vinit) then
c ---       Consistent result for included ring (OK)
            continue
         else
c ---       Inconsistent result (FATAL)
            problem=.TRUE.
         endif
      enddo
      if(problem) then
         write(*,*)
         write(*,*)'AVGRING: Fatal Error - see List file'
         write(io,*)
         write(io,*)'AVGRING:  Fatal Error'
         write(io,*)'Inconsistent Exclusion Result'
         write(io,*)'Exclusion Flag 0 should have Value > ',vinit
         write(io,*)'Exclusion Flag 1 should have Value = ',vinit
         write(io,*)
         write(io,*)'Ring ID    Exclusion    Value'
         do i=1,ndring
            write(io,*) i,nrrecp(i),avcr(i)
         enddo
         stop
      endif

      return
      end

c----------------------------------------------------------------------
      logical function lrsame(r0,r1,r2)
c----------------------------------------------------------------------
c
c --- CALPOST    Version: 6.221    Level: 991104c                LRSAME
c                D. Strimaitis,    Earth Tech
c
c --- PURPOSE:  Compare 2 real numbers (r1,r2) to determine if their
c               fractional difference exceeds r0
c
c --- INPUTS:
c            r0 - real       - Fractional difference allowed
c            r1 - real       - Real value 1
c            r2 - real       - Real value 2
c
c
c --- OUTPUT:
c        lrsame - logical    - Key indicating result of test
c                              .TRUE.  -- values are 'same'
c                              .FALSE. -- values are NOT 'same'
c
c
c --- LRSAME called by:  (utility)
c --- LRSAME calls:       none
c----------------------------------------------------------------------
c
      data half/0.5/

      lrsame=.TRUE.

c --- Direct comparison
      if(r1.EQ.r2) return

      rdif=ABS(r1-r2)
      ravg=half*ABS(r1+r2)

      if(rdif.GE.ravg) then
c ---    Fractional difference greater than one!
         lrsame=.FALSE.
      else
         ftest=rdif/ravg
         if(ftest.GT.r0) lrsame=.FALSE.
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine fin(myr,mjd,mhr,msc)
c----------------------------------------------------------------------
c
c --- CALPOST    Version: 6.221    Level: 991104d                   FIN
c ---            D. Strimaitis, Earth Tech
c
c --- PURPOSE:  Run termination routine
c
c --- INPUTS:
c           myr - real       - Year at end of last period processed
c           mjd - real       - Julian day at end of last period
c           mhr - real       - Time (Hour) at end of last period
c           msc - real       - Time (Second) at end of last period
c
c       Parameters: IO6
c
c --- OUTPUT:  none
c
c --- FIN called by:  MAIN
c --- FIN calls:      GRDAY
c----------------------------------------------------------------------
c --- include parameters
      INCLUDE 'params.pst'

      call GRDAY(io1,myr,mjd,nmo,nday)

c --- Write ending time for last period processed
      write(io6,5)myr,nmo,nday,mjd,mhr,msc
5     format(/2x,'CALPOST Application Completed',/2x,
     &       'Last Period Processed ENDS at:'/2x,'Year: ',i4,2x,
     &       'Month: ',i2,3x,'Day: ',i2,3x,'Julian day: ',i3,3x,
     &       'Hour: ',i2,3x,'Second: ',i4)
c
      return
      end
c----------------------------------------------------------------------
      subroutine getdoc(io,ifilver,dataver)
c----------------------------------------------------------------------
c
c --- CALPOST    Version: 6.221    Level: 030627                 GETDOC
c                D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  Read header comment records of the CALPUFF output
c               data file, and set file type.
c
c --- UPDATES:
c     V5.4(030402) to V5.4(030627)
c               (DGS) Add dataver to output arg list
c
c --- INPUTS:
c            io - integer    - Unit number for CALPUFF file
c
c     COMMON /CTRL/:
c           LDOC
c     Parameters:
c           IO1
c
c --- OUTPUT:
c       ifilver - integer    - Dataset version flag
c                              0: Before 2.0
c                              1: 2.0 or later
c       dataver - C*16       - Dataset version string
c
c --- GETDOC called by:  GETHEAD, GETHDRH
c --- GETDOC calls:      none
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'
      include 'ctrl.pst'

c --- Local Variables
      character*16 dataset,dataver,temp16
      character*33 blank33
      character*64 datamod
      character*132 comment1,blank

      data blank33/'                                 '/

c --- Set blank (132 characters)
      blank(1:33)=blank33
      blank(34:66)=blank33
      blank(67:99)=blank33
      blank(100:132)=blank33

c --- Read and test first record to check header format
c --- Record #1 - File Declaration -- 24 words
      read(io) dataset,dataver,datamod
      ifilver=0
      if(dataset.EQ.'CONC.DAT') then
         ifilver=1
      elseif(dataset.EQ.'DFLX.DAT') then
         ifilver=1
      elseif(dataset.EQ.'WFLX.DAT') then
         ifilver=1
      elseif(dataset.EQ.'VISB.DAT') then
         ifilver=1
      endif

      if(ifilver.EQ.0) then
c ---    Old file format with no comment records
         REWIND(io)

      elseif(ifilver.EQ.1) then
         if(LDOC) then
c ---      Read & Write to list file
           write(io1,*)
           write(io1,*)
           write(io1,*)'Header Documentation in CALPUFF file'
           write(io1,*)'-------------------------------------'
           write(io1,*)
           write(io1,*)
           write(io1,'(2a16,a64)') dataset,dataver,datamod
c ---      Number of comment records
           read(io) ncom
           do i=1,ncom
              comment1=blank
              read(io) comment1
              write(io1,'(a132)') comment1
           enddo
           write(io1,*)
           write(io1,*)
           write(io1,*)
         else
c ---      Read past doc records
c ---      Number of comment records
           read(io) ncom
           do i=1,ncom
              read(io) comment1
           enddo
         endif
      endif

c --- Prepare dataver string for testing in calling program
      temp16='                '
      k=0
      do i=1,16
         if(dataver(i:i).NE.' ') then
            k=k+1
            temp16(k:k)=dataver(i:i)
         endif
      enddo
      dataver=temp16

      return
      end
c----------------------------------------------------------------------
      subroutine prepwx
c----------------------------------------------------------------------
c
c --- CALPOST    Version: 6.221    Level: 040917                 PREPWX
c                D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  Preprocess the VSRN.DAT file containing weather
c               data for Method 7 visibility assessments.
c               Records for priority #1 station are transferred to
c               scratch file, and then records from other stations
c               on the list are substituted into the scratch file to
c               replace hours with missing weather codes and visible
c               range observations.
c
c --- MODIFICATION 
c     from Version 5.63, Level 030627 to Version 5.631, Level 040917
c     (CEC) - September 17, 2004.
c     Add the possibility of using a MM5 2D_LQW file as a source
c     of Background extinction coefficient and visual range
c     to compute method 7.
c
c --- INPUTS:
c
c     COMMON /CTRL/:
c           isyr,jsday,ishr,issec, LDEBUG
c     COMMON /HEAD/:
c           XBTZ
c     COMMON /VISB/:
c           NWSTA, IDWSTA(mxwsta), TZONE(mxwsta)
c     Parameters:
c           IO1,IN5,IOWX1,IOWX2,IOHRV
c
c --- OUTPUT:
c     COMMON /WEATHR/:
c           ITYPE. AWEATHER
c
c
c --- PREPWX called by:  (main)
c --- PREPWX calls:      ADJWX, JULDAY, GRDAY, INCR
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'
      include 'ctrl.pst'
      include 'head.pst'
      include 'visb.pst'
      INCLUDE 'weathr.pst'
      include 'mm4hdo.pst'

c --- Local Variables
      integer*8 ktime,kmaster,kmasterp,kline
      character*140 aline,amaster,alinex,blank140
      logical lhr00,lhrxx

c --- Build blank140
      do i=1,140
         blank140(i:i)=' '
      enddo

c --- Initialize & store previous and current weather records for
c --- later use
      pweather=blank140
      aweather=blank140

c --- Open 2 scratch files
      open(iowx1,status='scratch')
      open(iowx2,status='scratch')

c --- Set start of processing period window YYYYJJJ, hhmm
      isdate=isyr*1000+jsday
      istime=ishr*100+issec/60
c --- Set end of processing period window YYYYJJJ, hhmm
      ieyr=isyr
      jeday=jsday
      iehr=ishr
      nhours=nper+1
c --- Demand an explicit end-hour
      if(nper.EQ.99999) then
         write(io1,*)
         write(io1,*)'ERROR in PREPWX:  Too Many Periods'
         write(io1,*)'           NPER= ',nper
         write(io1,*)'Put explicit period in CALPOST Control File'
         write(io1,*)
         stop 'Halted in PREPWX -- See list file for error'
      endif

      call INCR(io1,ieyr,jeday,iehh,nhours)
      iedate=ieyr*1000+jeday
      ietime=iehr*100+issec/60

c --- Read and store header line
      aline=blank140
      read(in5,'(a140)') aline
      write(iowx1,'(a140)') aline

c --- Identify format from header record
c --- cec (040917)- add possible MM5 liquid water content
c --- as a third source of weather data (2D_LQW)
      if(aline(1:6).EQ.'STN---') then
         itype=1
         i1=13
         i2=52
      elseif(aline(1:6).EQ.'  USAF') then
         itype=2
         i1=14
         i2=53
      ELSEIF(aline(1:6).EQ.'3D.DAT' .OR. aline(1:6).EQ.'CALMM5') then
      call rdhd5
         itype=3
         i1=1
      else
         write(*,*) 'Unknown header for DATSAV3 - abbreviated file:'
         write(io1,*) 'Unknown header for DATSAV3 - abbreviated file:'
         write(io1,'(a140)') aline
         stop 'Halted in PREPWX'
      endif

c --- Set date-time for first 'current' record
      icdate=isdate
      icyr=isyr
      jcday=icdate-icyr*1000
      call GRDAY(io1,icyr,jcday,icmo,icdy)
      ichr=ishr

c --- Construct 'missing' record
      if(itype.EQ.1) then
         alinex(1:44)  ='000000 XXXX YYYYMMDDHHMM *** *** *** *** ***'
         alinex(45:88) =' * * * **** ** ** ** ** * **** **** ****** *'
         alinex(89:131)='**** ****** *** *** *****  *****  *****  **'
      ELSEIF(itype.EQ.2) then
         alinex(1:44)  ='000000 00000 YYYYMMDDHHMM *** *** *** *** **'
         alinex(45:88) ='* * * * **** ** ** ** * **** **** ****** ***'
         alinex(89:132)='** ****** *** *** ***** ***** ***** ***** **'
      endif
      write(alinex(i1:i1+3),'(i4)') icyr
      write(alinex(i1+4:i1+5),'(i2.2)') icmo
      write(alinex(i1+6:i1+7),'(i2.2)') icdy
      write(alinex(i1+8:i1+9),'(i2.2)') ichr
      alinex(i1+10:i1+11)='00'

c --- cec (040917) - if itype=3, just read line,
c ---                change time to local time and write line in scratch file
      if (itype.eq.3) then
      ideltzone=tzone(1)-xbtz
      ncount=0
11    aline=blank140
      READ(in5,'(a140)',END=100) aline
c ---   Adjust date-time to base time zone
        call ADJWX(i1,ideltzone,aline,ktime,mdate,mtime)
        mhour=mtime/100
c ---   Screen for date window
        if(mdate.LT.isdate) goto 11
        if(mdate.GT.iedate) GOTO 11
             if(mhour.EQ.ichr) then
             write(iowx1,'(a140)') aline
             do j=2,nyp
             READ(in5,'(a140)',END=100)aline
             call ADJWX(i1,ideltzone,aline,ktime,mdate,mtime)
             write(iowx1,'(a140)') aline
             end do
             do i=2,nxp
             do j=1,nyp
             READ(in5,'(a140)',END=100)aline
             call ADJWX(i1,ideltzone,aline,ktime,mdate,mtime)
             write(iowx1,'(a140)') aline
             end do
             end do
             endif
             alinex=aline
             call ADJWX(i1,1,alinex,ktime,icdate,ictime)
             ichr=ictime/100
             ncount=ncount+1
             GOTO 11
      else
c --- if itype is not 3 (not MM5 but weather observation station)
c --- Pass priority #1 station data within window to scratch file,
c --- writing 'missing' record for hours not available, to create
c --- a 'master' image
c ----------------------------------------------------------------
      ideltzone=tzone(1)-xbtz
      ncount=0
      lhr00=.FALSE.
10    aline=blank140
      read(in5,'(a140)',end=100) aline

c --- Screen for selected station
      read(aline(1:6),'(i6)') id
      if(id.NE.idwsta(1)) goto 10

c --- First target station record:  get full station marker for
c --- missing data record image
      if(ncount.EQ.0) alinex(1:12)=aline(1:12)

c --- Adjust date-time to base time zone
      call ADJWX(i1,ideltzone,aline,ktime,mdate,mtime)
      mhour=mtime/100

c --- Screen for date window
      if(mdate.LT.isdate) goto 10
      if(mdate.GT.iedate) goto 10

c --- OK, current record is within date window
20    continue

      if(mdate.GT.icdate) then
c ---    Data record is for a later day, write missing if current
c ---    hour has not been written and try the next hour
         if(.not.LHR00) write(iowx1,'(a140)') alinex
         call ADJWX(i1,1,alinex,ktime,icdate,ictime)
         ichr=ictime/100
         lhr00=.FALSE.
         goto 20
      elseif(mdate.EQ.icdate) then
c ---    Data record is for current day
         if(mhour.EQ.ichr) then
c ---       Match for current HHxx time found
            if(mtime.GT.ictime .AND. .not.LHR00) then
c ---          Current HH00 time is missing
               write(iowx1,'(a140)') alinex
            endif
c ---       Write data record but do not increment current time as
c ---       more data records may be available this hour
            write(iowx1,'(a140)') aline
            ncount=ncount+1
            lhr00=.TRUE.
            goto 10
         elseif(mhour.LT.ichr) then
c ---       Data record is for an earlier hour; get another record
            goto 10
         else
c ---       Data record is for a later hour
c ---       Write missing if current hour has not been written and
c ---       try the next hour
            if(.not.LHR00) write(iowx1,'(a140)') alinex
            call ADJWX(i1,1,alinex,ktime,icdate,ictime)
            ichr=ictime/100
            lhr00=.FALSE.
            goto 20
         endif
      else
c ---    Data record is for earlier day; get another record
         goto 10
      endif
      endif

cc --- No end date available if CALPUFF used 'process all hours option'
c100   continue
c --- File processed:  fill out remaining time periods with missings
100   if(icdate.LE.iedate.and.itype.ne.3) then
         if(.not.LHR00) write(iowx1,'(a140)') alinex
         call ADJWX(i1,1,alinex,ktime,icdate,ictime)
         ichr=ictime/100
         lhr00=.FALSE.
         goto 100
      endif
         
c --- Rewind scratch file
      REWIND(iowx1)

c --- Check number of data records found
      if(ncount.EQ.0) then
         write(io1,*)
         write(io1,*)
         write(io1,*)'ERROR in PREPWX:  No records found for:'
         write(io1,*)'  Priority 1 station   = ',idwsta(1)
         write(io1,*)'  Start Date (YYYYJJJ) = ',isdate
         write(io1,*)'  End Date (YYYYJJJ)   = ',iedate
         write(io1,*)
         stop 'Error in PREPWX for method 7 weather data'
      endif

c --- cec - if it is a weather observation station (no MM5, itype=1 or itype=2)
      if (itype.ne.3) then
c --- Use other listed stations to replace missing periods in
c --- the master
c -----------------------------------------------------------
      do iw=2,nwsta

c ---    Set time zone shift for this station
         ideltzone=tzone(iw)-xbtz

         REWIND(in5)
c ---    Header
         read(in5,'(a140)') aline

c ---    Get 1st data record for current replacement station
110      aline=blank140
         read(in5,'(a140)',end=300) aline
         read(aline(1:6),'(i6)') id
         if(id.NE.idwsta(iw)) goto 110

c ---    Adjust to base time zone
         call ADJWX(i1,ideltzone,aline,kline,mdate,mtime)

c ---    Move current scratch image to unit=iowx2
         REWIND(iowx2)
120         amaster=blank140
            read(iowx1,'(a140)',end=130) amaster
            write(iowx2,'(a140)') amaster
            goto 120
130      REWIND(iowx1)
         REWIND(iowx2)

c ---    Process header record
         amaster=blank140
         read(iowx2,'(a140)') amaster
         write(iowx1,'(a140)') amaster

c ---    Update data records on unit=iowx1
         lhr00=.FALSE.
         lhrxx=.FALSE.
150      amaster=blank140
         read(iowx2,'(a140)',end=200) amaster

         read(amaster(i1+10:i1+11),'(i2)') iminutes
         if(iminutes.EQ.0) then
            lhr00=.TRUE.
         elseif(amaster(i2:i2+3).NE.'****') then
            lhrxx=.TRUE.
         endif
         
         if(amaster(i2:i2+3).NE.'****') then
c ---       VSB field is NOT missing
            write(iowx1,'(a140)') amaster
            if(LHR00) then
               lhr00=.FALSE.
               lhrxx=.FALSE.
            endif
            goto 150
         elseif(LHRxx .AND. LHR00) then
c ---       OK to write missing hourly record because other
c ---       non-missing records are already written this hour
            write(iowx1,'(a140)') amaster
            lhr00=.FALSE.
            lhrxx=.FALSE.
            goto 150
         elseif(.not.LHR00) then
c ---       Skip missing non-hourly record
            goto 150
         else
c ---       Try to replace this missing hour
c ---       No records are in the master this hour, so look for
c ---       all records from substitution station this hour
c ---       from kmasterp to kmaster (YYYYMMDDHHmm)
            call ADJWX(i1,-1,amaster,kmasterp,jdatep,jtimep)
            call ADJWX(i1,1,amaster,kmaster,jdate,jtime)
160         continue
            if(kline.EQ.kmaster .AND. aline(i2:i2+3).NE.'****') then
c ---          Replace record and read next replacement record
               write(iowx1,'(a140)') aline
               lhr00=.FALSE.
               lhrxx=.FALSE.
               aline=blank140
161            read(in5,'(a140)',end=170) aline
               read(aline(1:6),'(i6)') id
               if(id.NE.idwsta(iw)) goto 161
               call ADJWX(i1,ideltzone,aline,kline,mdate,mtime)
               goto 150
            elseif(kline.LE.kmasterp) then
c ---          Read next replacement record
               aline=blank140
162            read(in5,'(a140)',end=170) aline
               read(aline(1:6),'(i6)') id
               if(id.NE.idwsta(iw)) goto 162
               call ADJWX(i1,ideltzone,aline,kline,mdate,mtime)
               goto 160
            elseif(kline.LT.kmaster) then
c ---          Write record and read next replacement record
               write(iowx1,'(a140)') aline
               aline=blank140
163            read(in5,'(a140)',end=170) aline
               read(aline(1:6),'(i6)') id
               if(id.NE.idwsta(iw)) goto 163
               call ADJWX(i1,ideltzone,aline,kline,mdate,mtime)
               goto 160
            else
c ---          Write missing record from master
               write(iowx1,'(a140)') amaster
               lhr00=.FALSE.
               lhrxx=.FALSE.
               goto 150
            endif
         endif

c ---    Transfer remaining records (no more replacement records) 
170      write(iowx1,'(a140)') amaster
         amaster=blank140
         read(iowx2,'(a140)',end=200) amaster
         goto 170

c ---    Image in scratch file at unit iowx1 is complete
200      REWIND(iowx1)
300      continue
      enddo
      endif

      close(iowx2)


c --- List scratch file results to a file (DEBUG)
c -----------------------------------------------
      if(LDEBUG) then
         open(iowx2,file='debug.wx1')
         write(iowx2,*)
         write(iowx2,*)'PREPWX:  Image of weather station data'
         write(iowx2,*)'--------------------------------------'
         write(iowx2,*)'  Priority 1 station   = ',idwsta(1)
         write(iowx2,*)'  Start Date (YYYYJJJ) = ',isdate
         write(iowx2,*)'  End Date (YYYYJJJ)   = ',iedate
         write(iowx2,*)
850      aline=blank140
         read(iowx1,'(a140)',end=900) aline
         write(iowx2,'(a140)') aline
         goto 850
900      write(io1,*)
         REWIND(iowx1)
         close(iowx2)
c ---    Now set up a second file for records actually used
         open(iowx2,file='debug.wx2')
         write(iowx2,*)
         write(iowx2,*)'Weather station data used for Extinction'
         write(iowx2,*)'----------------------------------------'
         write(iowx2,*)
         write(iowx2,'(1x,2a28)')'Yr  Mo Dy Hr VR(km)    Bext ',
     &                           '  Weather            Station'
      endif

c --- Report hourly extinction calculation results to a file
c ----------------------------------------------------------
      if(LVEXTHR) then
         open(iohrv,file='report.hrv')
c --     Identify species
         write(iohrv,91) 'HOURLY VISIBILITY EXTINCTIONS'
         write(iohrv,90) asplv
         write(iohrv,90) units
90       format(1x,t50,a15,//)
91       format(1x,t43,a29,//)
         if (itype.eq.3) then
         write(iohrv,'(3a38)')' Date-Time  Receptor    Model   Bkg-M2',
     &                        '     RH    M2 %                       ',
     &                        '   Obs    Bkg-M7    M7 % MM5 MM5      '
         write(iohrv,'(3a38)')' YYYYJJJHH  -i-  -j-    Bext     Bext ',
     &                        '     FAC  Change  Observed Weather    ',
     &                        '  VR(km)   Bext    Change i   j       '
         else
         write(iohrv,'(3a38)')' Date-Time  Receptor    Model   Bkg-M2',
     &                        '     RH    M2 %                       ',
     &                        '   Obs    Bkg-M7    M7 %              '
         write(iohrv,'(3a38)')' YYYYJJJHH  -i-  -j-    Bext     Bext ',
     &                        '     FAC  Change  Observed Weather    ',
     &                        '  VR(km)   Bext    Change             '
         endif
         write(iohrv,*)
      endif

c --- Skip header record to prepare master scratch file for processing
      read(iowx1,'(a140)') amaster

      return
      end
c----------------------------------------------------------------------
      subroutine adjwx(i1,idelhr,arecord,ktime, mdate,mtime)
c----------------------------------------------------------------------
c
c --- CALPOST    Version: 6.221    Level: 030627                  ADJWX
c                D. Strimaitis, Earth Tech, Inc.
c
c --- PURPOSE:  Adjust date-time of a data record from the VSRN.DAT
c               file containing weather data for Method 7 visibility
c               assessments.  Adjustment is in hours.
c               Also report the date-time as an integer.
c
c --- INPUTS:
c            i1 - integer    - Position of first year character in
c                              record
c        idelhr - integer    - Number of hours to ADD to time
c       arecord - char*140   - Full record
c
c     Parameters:
c           IO1
c
c --- OUTPUT:
c       arecord - char*140   - Full record (adjusted time)
c         ktime - integer*8  - YYYYMMDDHHmm time stamp
c         mdate - integer    - YYYYJJJ date
c         mtime - integer    - HHmm time
c
c
c --- ADJWX called by:  PREPWX
c --- ADJWX calls:      JULDAY, GRDAY, INCR
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'

c --- Local Variables
      integer*8 ktime
      character*140 arecord

c --- Extract date-time
      read(arecord(i1:i1+3),'(i4)') myear
      read(arecord(i1+4:i1+5),'(i2)') mmonth
      read(arecord(i1+6:i1+7),'(i2)') mday
      read(arecord(i1+8:i1+9),'(i2)') mhour
      read(arecord(i1+10:i1+11),'(i2)') mminute

c --- Adjust for time increment
      call JULDAY(io1,myear,mmonth,mday,jmday)
      call INCR(io1,myear,jmday,mhour,idelhr)
      call GRDAY(io1,myear,jmday,mmonth,mday)

c --- Update date-time field in record
      write(arecord(i1:i1+3),'(i4)') myear
      write(arecord(i1+4:i1+5),'(i2.2)') mmonth
      write(arecord(i1+6:i1+7),'(i2.2)') mday
      write(arecord(i1+8:i1+9),'(i2.2)') mhour

c --- Write full date-time as integer*8
      read(arecord(i1:i1+11),'(i12)') ktime

c --- Form integer*4 date and time
      mdate=myear*1000+jmday
      mtime=mhour*100+mminute

      return
      end
c-----------------------------------------------------------------------
      subroutine getwx(ldb,ndathr,bvis,vsrkm,wstring)
c-----------------------------------------------------------------------
c
c --- CALPOST    Version: 6.221         Level: 030627             GETWX
c                D. Strimaitis, Earth Tech, Inc.
c
c  PURPOSE:     Reads weather data records from VSRN.DAT file (DATSAV)
c               for method 7, and uses visual range when there is a
c               rain/fog event.
c
c  ARGUMENTS:
c     PASSED:  ldb        Control for debug output                   [L]
c              ndathr     Current year, Julian day, hour (ending)    [i]
c                         needed in YYYYJJJHH format
c              bvis       Calculated extinction (Mm-1)               [r]
c              vsrkm      Observed visual range (km)                 [r]
c              wstring    Interpreted weather code                   [c]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   JULDAY, CODEWX, TEXTWX
c-----------------------------------------------------------------------
c
      INCLUDE 'params.pst'
      INCLUDE 'weathr.pst'
      INCLUDE 'mm4hdo.pst'

c --- Local Variables
      character*140 blank140
      character*20 wstring
      character*6 astn
      character*4 avsb
      character*2 ww1,ww2,ww3
      logical lrain,ldriz,lsnow,lfog,lmist,lthndr
      logical ldb

c --- Set missing value for returned Bvis
      data bmiss/-99.9/

c --- Build blank140
      do i=1,140
         blank140(i:i)=' '
      enddo

c --- Initialize Bext to missing
      bvis=bmiss

c --- Initialize weather logicals
      lrain=.FALSE.
      ldriz=.FALSE.
      lsnow=.FALSE.
      lmist=.FALSE.
      lfog=.FALSE.
      lthndr=.FALSE.

c --- Initialize weather flag
      iweather=0

c --- Initialize record count and observed VR
      num=0
      vrsum=0

c --- Set date-time at beginning of hour
      iyr=ndathr/100000
      ijulhr=ndathr-iyr*100000
      ijul=ijulhr/100
      ihr=ijulhr-ijul*100
      call INCR(io1,iyr,ijul,ihr,-1)
      ndathr0=iyr*100000+ijul*100+ihr

c --- Read a record if last is not available
      if(aweather.EQ.blank140) read(iowx1,'(a140)') aweather

c --- Check for record format
      if(itype.EQ.1) then
         i1=13
      elseif(itype.EQ.2) then
         i1=14
      else
         write(io1,*)'GETWX:  Invalid file format ITYPE = ',itype
         stop 'Invalid file format ITYPE in GETWX'
      endif

c --- Compute date-time
10    read(aweather(i1:i1+3),'(i4)') myear
      read(aweather(i1+4:i1+5),'(i2)') mmonth
      read(aweather(i1+6:i1+7),'(i2)') mday
      read(aweather(i1+8:i1+9),'(i2)') mhour
      read(aweather(i1+10:i1+11),'(i2)') mmin
      call JULDAY(io1,myear,mmonth,mday,jmday)
      mdathr=myear*100000+jmday*100+mhour

      if(mdathr.LT.ndathr0) then
c ---    Record before start of hour:  get another
         read(iowx1,'(a140)') aweather
         goto 10
      elseif(mdathr.EQ.ndathr0 .AND. mmin.EQ.0) then
c ---    Record at end of previous hour:  get another
         read(iowx1,'(a140)') aweather
         goto 10

      elseif(mdathr.EQ.ndathr0) then
c ---    Record within hour:  process and get another
c ---    Extract weather and VSB
         if(itype.EQ.1) then
            avsb=aweather(52:55)
            ww1=aweather(57:58)
            ww2=aweather(60:61)
            ww3=aweather(63:64)
         elseif(itype.EQ.2) then
            avsb=aweather(53:56)
            ww1=aweather(58:59)
            ww2=aweather(61:62)
            ww3=aweather(64:65)
         endif
c ---    Station ID
         astn=aweather(1:6)
c ---    Interpret
         call CODEWX(ww1,ww2,ww3,iweather,
     &               lrain,ldriz,lsnow,lfog,lmist,lthndr)
         if(avsb.NE.'****') then
            read(avsb,'(f4.1)')vsrmi
            vrsum=vrsum+vsrmi
            num=num+1
         endif
c ---    Next record
         read(iowx1,'(a140)') aweather
         goto 10

      elseif(mdathr.EQ.ndathr .AND. mmin.EQ.0) then
c ---    Record at end of hour:  process
c ---    Extract weather and VSB
         if(itype.EQ.1) then
            avsb=aweather(52:55)
            ww1=aweather(57:58)
            ww2=aweather(60:61)
            ww3=aweather(63:64)
         elseif(itype.EQ.2) then
            avsb=aweather(53:56)
            ww1=aweather(58:59)
            ww2=aweather(61:62)
            ww3=aweather(64:65)
         endif
c ---    Station ID
         astn=aweather(1:6)
c ---    Interpret
         call CODEWX(ww1,ww2,ww3,iweather,
     &               lrain,ldriz,lsnow,lfog,lmist,lthndr)
         if(avsb.NE.'****') then
            read(avsb,'(f4.1)')vsrmi
            vrsum=vrsum+vsrmi
            num=num+1
         endif

c --- Record is after hour:  done
      endif
c
c --- Process accumulated weather codes and VR
      call TEXTWX(lrain,ldriz,lsnow,lfog,lmist,lthndr,wstring)
      if(num.GT.0) then
         vsrmi=vrsum/FLOAT(num)
         vsrmi=AMAX1(vsrmi,0.1)
         vsrkm=1.6093*vsrmi
         if(iweather.EQ.1) bvis=3912.0/vsrkm
      else
         vsrkm=-1.
      endif

      if(LDB) write(iowx2,'(i4,3i3,2x,f4.1,f9.1,3x,a20,a6)')
     &        myear,mmonth,mday,mhour,vsrkm,bvis,wstring,astn
      
      return
      end
c
c-----------------------------------------------------------------------
      subroutine getwx3(ldb,ndathr,bvis3,wstring3)
c-----------------------------------------------------------------------
c
c --- CALPOST    Version: 6.221         Level: 040917            GETWX3
c                D. Strimaitis, Earth Tech, Inc.
c
c  PURPOSE:     Reads weather data records from MM5 2D_LQW
c               for method 7, and uses visual range when there is a
c               rain/fog event.
c
c --- MODIFICATION of GETWX Version 5.631, level 040917
c --- (CEC) on September 17, 2004
c     Add some conditions when the weather file is a MM5 2D_LQW file (itype=3)
c     and not a Meteorological Observations station.
c
c  ARGUMENTS:
c     PASSED:  ldb        Control for debug output                   [L]
c              ndathr     Current year, Julian day, hour (ending)    [i]
c                         needed in YYYYJJJHH format
c              bvis3       Calculated extinction (Mm-1)  array       [r]
c              vsrkm3      Observed visual range (km)   array        [r]
c              wstring3    Interpreted weather code     array        [c]
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   JULDAY
c-----------------------------------------------------------------------
c
      INCLUDE 'params.pst'
      INCLUDE 'weathr.pst'
      INCLUDE 'mm4hdo.pst'

c --- Local Variables
      character*140 blank140
      character*20 wstring3(mxnxp,mxnyp)
      character*6 astn
      character*4 avsb
      character*2 ww1,ww2,ww3
      logical lrain,ldriz,lsnow,lfog,lmist,lthndr
      logical ldb
      REAL bvis3(mxnxp,mxnyp)

c --- Set missing value for returned Bvis
      data bmiss/-99.9/

c --- Build blank140
      do i=1,140
         blank140(i:i)=' '
      enddo

c --- Initialize Bext to missing
      do i=1,nxp
      do j=1,nyp
      bvis3(i,j)=bmiss
      end do
      end do

c --- Set date-time at beginning of hour
      iyr=ndathr/100000
      ijulhr=ndathr-iyr*100000
      ijul=ijulhr/100
      ihr=ijulhr-ijul*100
      call INCR(io1,iyr,ijul,ihr,-1)
      ndathr0=iyr*100000+ijul*100+ihr

c --- Read a record if last is not available
      if(aweather.EQ.blank140) read(iowx1,'(a140)') aweather

c --- Check for record format
      if(itype.EQ.3) then
         i1=1
      else
         write(io1,*)'GETWX:  Invalid file format ITYPE = ',itype
         stop 'Invalid file format ITYPE in GETWX'
      endif

c --- Compute date-time
10    read(aweather(i1:i1+3),'(i4)') myear
      read(aweather(i1+4:i1+5),'(i2)') mmonth
      read(aweather(i1+6:i1+7),'(i2)') mday
      read(aweather(i1+8:i1+9),'(i2)') mhour
      read(aweather(i1+10:i1+11),'(i2)') mmin
      call JULDAY(io1,myear,mmonth,mday,jmday)
      mdathr=myear*100000+jmday*100+mhour

      if(mdathr.LE.ndathr0) then
c ---    Record before start of hour:  get another
         read(iowx1,'(a140)') aweather
         goto 10

      elseif(mdathr.EQ.ndathr) then
c ---    Record at end of hour:  process
c ---    Extract weather and VSB
            READ(aweather(90:99),'(f10.2)')bvis3(1,1)
            READ(aweather(100:107),'(f8.2)')vsrkm3(1,1)
            wstring3(1,1)='  '
            IF(aweather(29:32).ne.'0.00') then
            wstring3(1,1)='rain'
            elseiF(aweather(61:65).ne.'0.000') then
            wstring3(1,1)='fog'
            elseIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(67:71).ne.'0.000')then
            wstring3(1,1)='q_rain'
            elseIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(79:83).ne.'0.000')then
            wstring3(1,1)='q_snow'
            ELSEIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(73:77).ne.'0.000') then
            wstring3(1,1)='q_ice'
            ELSEIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(85:89).ne.'0.000') then
            wstring3(1,1)='q_graupel'
            else
            wstring3(1,1)='   '
            endif
            astn='mm5'
            do i=2,nxp
            read(iowx1,'(a140)') aweather
            READ(aweather(90:99),'(f10.2)')bvis3(i,1)
            READ(aweather(100:107),'(f8.2)')vsrkm3(i,1)
            wstring3(i,1)='  '
            IF(aweather(29:32).ne.'0.00') then
            wstring3(i,1)='rain'
            elseiF(aweather(61:65).ne.'0.000') then
            wstring3(i,1)='fog'
            elseIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(67:71).ne.'0.000')then
            wstring3(i,1)='q_rain'
            elseIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(79:83).ne.'0.000')then
            wstring3(i,1)='q_snow'
            ELSEIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(73:77).ne.'0.000') then
            wstring3(i,1)='q_ice'
            ELSEIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(85:89).ne.'0.000') then
            wstring3(i,1)='q_graupel'
            else
            wstring3(i,1)='   '
            endif
            astn='mm5'
            end do
            do j=2,nyp
            do i=1,nxp
            read(iowx1,'(a140)') aweather
            READ(aweather(90:99),'(f10.2)')bvis3(i,j)
            READ(aweather(100:107),'(f8.2)')vsrkm3(i,j)
            wstring3(i,j)='  '
            IF(aweather(29:32).ne.'0.00') then
            wstring3(i,j)='rain'
            elseiF(aweather(61:65).ne.'0.000') then
            wstring3(i,j)='fog'
            elseIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(67:71).ne.'0.000')then
            wstring3(i,j)='q_rain'
            elseIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(79:83).ne.'0.000')then
            wstring3(i,j)='q_snow'
            ELSEIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(73:77).ne.'0.000') then
            wstring3(i,j)='q_ice'
            ELSEIF(aweather(29:32).eq.'0.00'.and.
     &      aweather(85:89).ne.'0.000') then
            wstring3(i,j)='q_graupel'
            else
            wstring3(i,j)='   '
            endif
            astn='mm5'
            end do
            end do
         endif
      read(aweather(i1:i1+3),'(i4)') myear
      read(aweather(i1+4:i1+5),'(i2)') mmonth
      read(aweather(i1+6:i1+7),'(i2)') mday
      read(aweather(i1+8:i1+9),'(i2)') mhour
      read(aweather(i1+10:i1+11),'(i2)') mmin
      call JULDAY(io1,myear,mmonth,mday,jmday)
      mdathr1=myear*100000+jmday*100+mhour
      if (mdathr1.ne.mdathr) then
      WRITE(*,*)'error in reading 2D_LQW file - changing time'
      WRITE(*,*)'between first grid point and last grid point'
      stop
      end if

      return
      end
c-----------------------------------------------------------------------
      subroutine codewx(ww1,ww2,ww3,iweather,
     &                  lrain,ldriz,lsnow,lfog,lmist,lthndr)
c-----------------------------------------------------------------------
c
c --- CALPOST    Version: 6.221         Level: 030627            CODEWX
c                D. Strimaitis, Earth Tech, Inc.
c
c  PURPOSE:     Interprets weather codes for using visual range
c
c  ARGUMENTS:
c --- INPUTS:
c              ww[1,2,3]  2-character weather codes                  [c]
c
c --- OUTPUT:
c              iweather   Calculate extinction ? (updated)           [i]
c                            0: NO
c                            1: YES
c              lrain(...) weather logicals (updated)                 [L]
c
c
c  CALLING ROUTINES:    GETWX
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
c
      character*2 ww1,ww2,ww3
      integer iww(3)
      logical lrain,ldriz,lsnow,lfog,lmist,lthndr

      do i=1,3
         iww(i)=0
      enddo


      if(ww1.EQ.'**' .OR. ww1.EQ.'00') then
         return
      else
         read(ww1(1:2),'(i2)') iww(1)
         if(ww2.NE.'**') read(ww2(1:2),'(i2)') iww(2)
         if(ww3.NE.'**') read(ww3(1:2),'(i2)') iww(3)
      endif

      do i=1,3
c ---    Search for codes
         if(iww(i).EQ.21 .OR. iww(i).EQ.25 .OR. iww(i).EQ.27) then
            lrain=.TRUE.
            iweather=1
         elseif(iww(i).GE.60 .AND. iww(i).LE.69) then
            lrain=.TRUE.
            iweather=1
         elseif(iww(i).GE.80 .AND. iww(i).LE.84) then
            lrain=.TRUE.
            iweather=1
         elseif(iww(i).GE.91 .AND. iww(i).LE.92) then
            lrain=.TRUE.
            iweather=1
         elseif(iww(i).GE.50 .AND. iww(i).LE.59) then
            ldriz=.TRUE.
            iweather=1
         elseif(iww(i).EQ.10) then
            lmist=.TRUE.
            iweather=1
         elseif(iww(i).EQ.28) then
            lfog=.TRUE.
            iweather=1
         elseif(iww(i).GE.40 .AND. iww(i).LE.49) then
            lfog=.TRUE.
            iweather=1
         elseif(iww(i).GE.22 .AND. iww(i).LE.24) then
            lsnow=.TRUE.
            iweather=1
         elseif(iww(i).EQ.26) then
            lsnow=.TRUE.
            iweather=1
         elseif(iww(i).GE.70 .AND. iww(i).LE.79) then
            lsnow=.TRUE.
            iweather=1
         elseif(iww(i).GE.85 .AND. iww(i).LE.90) then
            lsnow=.TRUE.
            iweather=1
         elseif(iww(i).GE.93 .AND. iww(i).LE.94) then
            lsnow=.TRUE.
            iweather=1
         elseif(iww(i).EQ.29) then
            lthndr=.TRUE.
            iweather=1
         elseif(iww(i).GE.95 .AND. iww(i).LE.99) then
            lthndr=.TRUE.
            iweather=1
         endif
      enddo

      return
      end
c-----------------------------------------------------------------------
      subroutine textwx(lrain,ldriz,lsnow,lfog,lmist,lthndr,wstring)
c-----------------------------------------------------------------------
c
c --- CALPOST    Version: 6.221         Level: 030627            TEXTWX
c                D. Strimaitis, Earth Tech, Inc.
c
c  PURPOSE:     Interprets weather codes
c
c  ARGUMENTS:
c --- INPUTS:
c              lrain(...) Weather logicals                           [L]
c
c --- OUTPUT:
c              wstring    Text string                                [c]
c
c
c  CALLING ROUTINES:    GETWX
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
c
      logical lrain,ldriz,lsnow,lfog,lmist,lthndr
      character*20 wstring,blank20
      data blank20/'                    '/


c --- Initialize weather string
      wstring=blank20

c --- Set string and weather flag (NOT in WW order)
      k=1
      if(lrain) then
         wstring(k:k+4)='Rain/'
         k=k+5
      endif
      if(lfog) then
         wstring(k:k+3)='Fog/'
         k=k+4
      endif
      if(ldriz) then
         wstring(k:k+7)='Drizzle/'
         k=k+8
      endif
      if(lsnow) then
         wstring(k:k+4)='Snow/'
         k=k+5
      endif
      if(lmist) then
         wstring(k:k+4)='Mist/'
         k=k+5
      endif
      if(lthndr) then
         wstring(k:k+7)='Thunder/'
         k=k+8
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine rdhd5
c----------------------------------------------------------------------
c
c --- CALMET   Version: 6.221     Level: 040917                   RDHD5
c              F.Robe
c              Modified by J. Scire (1/99, 11/03), Zhong Wu (11/00)
c                          F.Robe (09/01)
c              Added in CALPOST from CALMET to read MM5 like file
c              MM5_2DLQW output.
c
c --- PURPOSE: Read the header records from a MM5.DAT or 3D.DAT file
c              and compute closer MM5 grid point to receptor
c
c --- UPDATES
c --- V5.542 (031126) from CALMET to V5.631 (040917) (CEC)
c         - Adapted from CALMET subroutine to read MM5-2DLQW file
c --- V5.541 (030402) to V5.542 (031126) (J. Scire)
c       - Changes to allow new 3D.DAT file structure (Version 2.0) 
c         to be read while maintinaing backward compatibility with
c         older MM5.DAT and 3D.DAT/M3D.DAT/MM53D.DAT files
c --- V5.4 (000602d) to V5.5 (030402)  (DGS)
c               - Change documentation:  coordinates may be other than
c                 UTM or LLC
c --- V5.0-V5.1   991104  (DGS): YYYY format for year
c --- V5.0-V5.1   991104  (DGS): Allow either new or old header format
c
c --- INPUTS:
c
c        Parameters:
c            MXNXP, MXNYP, MXNZP, IO6, IO20, MXNX, MXNY
c        Common block /GRID/:
c            NX,NY,DGRID,XORIGR,YORIGR
c
c --- OUTPUT:
c
c        Common block /MM4HDO/ variables:
c            IBYRM, IBJULM, IBHRM, IEYRM, IEJULM, IEHRM,
c            NXMM4, NYMM4, NZP, PTOPMM4, I1, J1, NXP, NYP,
c            SIGMA(mxnzp), XLAT4(mxnxp,mxnyp), XLONG4(mxnxp,mxnyp),
c            IELEV4(mxnxp,mxnyp),ILU4(mxnxp,mxnyp),XLCMM4(mxnxp,mxnyp),
c            YLCMM4(mxnxp,mxnyp),IGRAB(mxnx,mxny,4),JGRAB(mxnx,mxny,4),
c            IOUTMM5
c
c --- RDHD5 called by:  READHD
c --- RDHD5 calls:      RDHD51, RDHD52, RDHD53
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'
c
      character*80 buff1, buff2
      character*12 cset3d
      CHARACTER*140 aline
      logical lprt
c
c --- Common blocks
c      include 'grid.met'
      include 'mm4hdo.pst'
      include 'head.pst'
c      include 'filnam.met'

      COMMON /D6/ IRD,IWR,IFILE,IRDP
c
c FRR (09/2001) - allows non hourly prognostic input
      COMMON /PROGSTEP/ ifirstpg
c
c      dimension neari(4),nearj(4),dnear(4)
      data lprt/.true./

c --- FRR (09/2001) - non hourly mm5 data
c --- Flag for initialization in rdmm5 subroutine
      ifirstpg = 0
c
c ---    Data is in new 3D.DAT format (dataset version 2.0 or
c ---    later)
         REWIND(in5)
         READ(in5,'(a140)')aline
         if (aline(1:6).EQ.'CALMM5') then
         call rdhd52
         ELSEIF (aline(1:6).EQ.'3D.DAT') then
         call rdhd53
         endif

c --------------------------------------------------------
c --- Print first and last lat/lon values to the list file
c --------------------------------------------------------
c     if(lprt)then
         write(io1,121)xlat4(1,1),xlong4(1,1),ielev4(1,1),
     &                 xlat4(nxp,nyp),xlong4(nxp,nyp),ielev4(nxp,nyp)
121      format(/5x,' Lat./Lon./Elev. of First Cell: ',f9.4,f10.4,i6/
     :           5x,' Lat./Lon./Elev. of Last  Cell: ',f9.4,f10.4,i6)
c      endif
c
c --- Find the 4 closest MM5 grid points to each receptor
c     (assume CALPUFF domain is inside MM5 grid section)
c     xx is x-coord of receptor
c     yy is y-coord of receptor
c
      if (LSGRID) then
      do i=1,ngx
        do j=1,ngy
        dnear = 9.9E19
          do ii = 1,nxp
          do jj = 1,nyp
            pdist = sqrt ((xlcmm4(ii,jj) - xgrd(i,j)) ** 2 +
     &                    (ylcmm4(ii,jj) - ygrd(i,j)) **2)
            if (pdist .lt. dnear) then
            dnear=pdist
            jgrabg(i,j)=jj
            igrabg(i,j)=ii
            GOTO 66
            endif
 66         continue
          enddo
          enddo
        enddo
      enddo
      endif
c

      if (ndrec.gt.0.) then
      do i=1,ndrec
      dnear = 9.9E19
       do ii = 1,nxp
          do jj = 1,nyp
            pdist = sqrt ((xlcmm4(ii,jj) - xrec(i)) ** 2 +
     &                    (ylcmm4(ii,jj) - yrec(i)) **2)
            if (pdist .lt. dnear) then
            dnear=pdist
            jgrabd(i)=jj
            igrabd(i)=ii
            GOTO 67
            endif
 67         continue
          enddo
          enddo
      end do
      end if
c
      if (nctrec.gt.0.) then
      do i=1,ndrec
      dnear = 9.9E19
       do ii = 1,nxp
          do jj = 1,nyp
            pdist = sqrt ((xlcmm4(ii,jj) - xctr(i)) ** 2 +
     &                    (ylcmm4(ii,jj) - yctr(i)) **2)
             if (pdist .lt. dnear) then
            dnear=pdist
            jgrabc(i)=jj
            igrabc(i)=ii
            GOTO 68
            endif
 68         continue
          enddo
          enddo
      end do
      end if
c 
      return
      end

c----------------------------------------------------------------------
      subroutine rdhd53
c----------------------------------------------------------------------
c
c --- CALMET   Version: 6.221     Level: 040917                  RDHD53
c              J. Scire, Earth Tech
c              Adapted from RDHD52
c              Adapted from RDHD53 from CALMET to read MM5-2DLQW
c
c --- PURPOSE: Read the header records from a file in 3D.DAT format 
c              (dataset Version 2.0 or later)
c
c --- MODIFICATION:
c     V5.545, level 031126 to V5.631, level 040917 (CEC)
c     RDHD53 from CALMET changed to read MM5_2DLQW
c
c --- INPUTS:
c
c        Parameters:
c            MXNXP, MXNYP, MXNZP, IO6, IO20, MXNX, MXNY
c        Common block /MAP/:
c            IUTMZN, UTMHEM, XLAT1, XLAT2, RELON0, RNLAT0,
c            FEAST, FNORTH, DATUM
c        Common block /GRID/:
c            NX,NY,DGRID,XORIGR,YORIGR
c        Common block /MM4HDO/ variables:
c            DATUM3D
c
c --- OUTPUT:
c
c        Common block /MM4HDO/ variables:
c            IBYRM, IBJULM, IBHRM, IEYRM, IEJULM, IEHRM,
c            NXMM4, NYMM4, NZP, PTOPMM4, I1, J1, NXP, NYP,
c            SIGMA(mxnzp), XLAT4(mxnxp,mxnyp), XLONG4(mxnxp,mxnyp),
c            IELEV4(mxnxp,mxnyp),ILU4(mxnxp,mxnyp),XLCMM4(mxnxp,mxnyp),
c            YLCMM4(mxnxp,mxnyp),IGRAB(mxnx,mxny,4),JGRAB(mxnx,mxny,4),
c            IOUTMM5,NCOMM3D, CNAME3D, CVER3D, CTITLE3D, COMM3D, 
c            DATUM3D
c
c --- RDHD53 called by:  RDHD5
c --- RDHD53 calls:      JULDAY, INCR, YR4, GLOBE1, GLOBE
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'
c
      character*128 ctemp3d
      logical lprt
c
c --- Common blocks
c      include 'grid.met'
      include 'mm4hdo.pst'
      include 'head.pst'

      COMMON /D6/ IRD,IWR,IFILE,IRDP

c --- For coordinate transformations
      character*8 cmapi,cmapo
      character*12 caction
      character*4 c4hem
      real*8 vecti(9),vecto(9)
c
      data lprt/.true./

c --- Scale factor for Tangential TM projection
      tmsone=1.00000
c --- Set translation vectors going from N.lat/E.lon
c --- to projection(x,y)km
      iutmo=iutmzn
      if(utmhem.EQ.'S   ' .AND. iutmzn.LT.900) iutmo=-iutmo
      cmapo=pmap
      if(cmapo.EQ.'TTM     ') cmapo='TM      '
      cmapi='LL      '
      idum=0
      rdum=0.0
      call GLOBE1(cmapi,idum,rdum,rdum,rdum,rdum,rdum,
     &            rdum,rdum,
     &            cmapo,iutmo,tmsone,xlat1,xlat2,rnlat0,relon0,
     &            feast,fnorth,
     &            caction,vecti,vecto)
c
      REWIND(in5)
c ------------------------------------------------------------------
c --- Read header record #1 (Dataset name, version and title
c ------------------------------------------------------------------
      read(in5,10)cname3d,cver3d,ctitle3d
10    format(2a16,a64)
      WRITE(*,*)cname3d,cver3d,ctitle3d
c ------------------------------------------------------------------
c --- Read header record #2 (Number of comment lines)
c ------------------------------------------------------------------
      read(in5,11)ncomm3d
11    format(i4)

c ------------------------------------------------------------------
c --- Next "NCOMM3d" lines (comment lines)
c ------------------------------------------------------------------
      if(ncomm3d.gt.0)then
         do i=1,ncomm3d
            read(in5,12)ctemp3d
12          format(a132)
c ---       Save first line of text for later printing
            if(i.eq.1)comm3d=ctemp3d
         enddo
      endif

c -------------------------------------------
c --- Next header record (MM5 output options)
c -------------------------------------------
      read(in5,43)ioutw,ioutq,ioutc,iouti,ioutg
      ioutmm5=81+10*ioutw+ioutq+ioutc+iouti+ioutg
43    format(5(i3))

c --------------------------------------------
c --- Skip next header record (map projection)
c --------------------------------------------
      read(in5,*)

c ------------------------------------------------
c --- Next header record (MM5 output options)
c     (Add 13 output options for surface variables)
c ------------------------------------------------
      read(in5,44) inhyd,imphys,icupa,ibltyp,ifrad,isoil
     :         ,ifddaan,ifddaob
     :         ,igrdt,ipbl,ishf,ilhf,iustr,iswdn
     :         ,ilwdn,ist1,ist2,ist3,ist4,ist5,ist6
44    format(30(i3))
c --------------------------------------
c --- Next header record (MM5 grid data)
c --------------------------------------
      read(in5,20)ibyrm,ibmom,ibdym,ibhrm,nhrsmm5,
     1             nxp,nyp,nzp
 20   format(i4,3i2,i5,3i4)
      call YR4(io6,ibyrm,ierrb)
      if(ierrb.NE.0)then
         write(io6,*)'Error encountered in Subr. YR4 '
         write(io6,*)'Execution stopping in Subr. RDHD53 ',
     1   '-- IERRB = ',ierrb
         stop
      endif
c
c --- Calculate Julian day
      call julday(io6,ibyrm,ibmom,ibdym,ibjulm)
c
c --- Compute ending date/time (comment out if using other format)
      ieyrm=ibyrm
      iejulm=ibjulm
      iehrm=ibhrm
      call incr(io6,ieyrm,iejulm,iehrm,nhrsmm5)
c
c --------------------------------------------------
c --- Next header record (extraction subdomain data)
c --------------------------------------------------
      read(in5,30)nx1,ny1,nx2,ny2,nz1,nz2,
     &             rxmin,rxmax,rymin,rymax
      i1=nx1
      j1=ny1
30    format(6i4,2f10.4,2f9.4)
c

c --- Check that array dimensions are not exceeded
      if(nxp.gt.mxnxp.or.nyp.gt.mxnyp.or.nzp.gt.mxnzp)then
         write(io6,*)'ERROR in subr. RDHD53 -- Array dimensions ',
     1   'are too small for data being read'
         write(io6,*)'Grid being read  (NXP, NYP, NZP) = ',
     1    nxp,nyp,nzp
         write(io6,*)'Array dimensions (MXNXP, MXNYP, MXNZP) = ',
     1    mxnxp,mxnyp,mxnzp
         stop
      endif
c --- Check consistency between nz1,nz2, and nzp
      if(nzp.ne.nz2-nz1+1) then
        write(io6,*)'Error in RDHD53: NZ1,NZ2 and NZP not consistent'
        write(io6,*)'nz1,nz2,nzp:',nz1,nz2,nzp
        stop
      endif
c
c ---------------------------------------------
c --- Next NZP records -- MM5 half-sigma levels
c ---------------------------------------------
      do 40 n=1,nzp
         read(in5,38)sigma(n)
38       format(f6.3)
40    continue
c
c --------------------------------------------------------
c --- Print the 3D.DAT header information to the list file
c --- (except for the gridded fields)
c --------------------------------------------------------
      if(lprt)then
         write(io1,101)cname3d,cver3d,ctitle3d,comm3d
101      format(//1x,'Information read from 3D.DAT file'/
     1   5x,'Dataset Name:    ',a12/
     2   5x,'Dataset Version: ',a12/
     3   5x,'Dataset Title:   ',a64/
     4   5x,'First line of comments: ',
     5   8x,a132)
c
         write(io1,102) inhyd,imphys,icupa,ibltyp,ifrad,isoil,
     :               ifddaan,ifddaob
102      format(/5x,'MM5 physics options: '/
     1           5x,'    Hydrostatic:           ',i2/
     1           5x,'    Moisture scheme:       ',i2/
     1           5x,'    Convection scheme:     ',i2/
     1           5x,'    Boundary layer scheme: ',i2/
     1           5x,'    Radiation scheme       ',i2/
     1           5x,'    Soil scheme:           ',i2/
     1           5x,'    Analysis FDDA:         ',i2/
     1           5x,'    Observation FDDA:      ',i2)


         write(io1,1021)igrdt,ipbl,ishf,ilhf,iustr,iswdn
     :         ,ilwdn,ist1,ist2,ist3,ist4,ist5,ist6
1021      format(/5x,'MM5 surface variable options: '/
     1           5x,'    Ground temperature:    ',i2/
     1           5x,'    PBL:                   ',i2/
     1           5x,'    Sensible heat flux:    ',i2/
     1           5x,'    Latent heat flux:      ',i2/
     1           5x,'    Frictional velocity:   ',i2/
     1           5x,'    Downward SW radiation: ',i2/
     1           5x,'    Downward LW radiation: ',i2/
     1           5x,'    Soil temp at layer 1:  ',i2/
     1           5x,'    Soil temp at layer 2:  ',i2/
     1           5x,'    Soil temp at layer 3:  ',i2/
     1           5x,'    Soil temp at layer 4:  ',i2/
     1           5x,'    Soil temp at layer 5:  ',i2/
     1           5x,'    Soil temp at layer 6:  ',i2)

         write(io1,103)1,ioutw,ioutq,ioutc,iouti,ioutg
103      format(/5x,'MM5 output fields (1 = YES; 0 = NO): '/
     1           5x,' Pressure, height,T,Wind speed and direction: ',i2/
     1           5x,'                           Vertical velocity: ',i2/
     1           5x,'                   RH and vapor mixing ratio: ',i2/
     1           5x,'                Cloud and rain mixing ratios: ',i2/
     1           5x,'                  Ice and Snow mixing ratios: ',i2/
     1           5x,'                                     Graupel: ',i2)

         write(io1,104)ibyrm,ibmom,ibdym,ibhrm,nhrsmm5,
     1                 nxp,nyp,nzp
104      format(/5x,'Date/time (YYYYMMDDHH) of MM5 data: '/
     1           5x,'                  Start = ',i4,3i2,' (GMT)'/
     2           5x,'              No. hours = ',i4,/
     3           5x,'Extraction Subdomain in MM5 file: '/
     4           5x,'            No. X cells = ',i4/
     5           5x,'            No. Y cells = ',i4/
     6           5x,'            No. layers  = ',i4 )
c
         write(io1,106)nx1,ny1,nz1,nx2,ny2,nz2
106      format(/5x,'            Beginning X = ',i4/
     2           5x,'            Beginning Y = ',i4/
     3           5x,'            Beginning Z = ',i4/
     4           5x,'               Ending X = ',i4/
     5           5x,'               Ending Y = ',i4/
     6           5x,'               Ending Z = ',i4)
c
         write(io1,107)rymin,rymax,rxmin,rxmax
107      format(/5x,' Latitude range : ',f9.4,'  -  ',f9.4/
     :           5x,' Longitude range: ',f10.4, ' - ' ,f10.4)

         write(io1,108)
108      format(/5x,'MM5 half-sigma levels'/5x,'Level',5x,'Sigma'/)
         do 110 i=1,nzp
            write(io1,109)i,sigma(i)
110      continue
109      format(4x,i4,6x,f6.4)
      endif
c
c ----------------------------------------------------
c --- Next NXP * NYP records -- lat., long., elevation
c ----------------------------------------------------
      do 50 j=1,nyp
      do 50 i=1,nxp
         read(in5,99)iindex,jindex,xlat4(i,j),xlong4(i,j),
     &                ielev4(i,j)
 99      format(2i4,f9.4,f10.4,i5)
c ---   Compute grid point locations from N.Lat and E.Lon
        call GLOBE(io6,caction,datum3d,vecti,datum,vecto,
     &             xlong4(i,j),xlat4(i,j),xlcmm4(i,j),ylcmm4(i,j),
     &             idum,c4hem)
c
c ---   QA check that I,J read match expected values
c
        icheck=iindex-i1+1
        jcheck=jindex-j1+1
        if(icheck.ne.i.or.jcheck.ne.j)then
          write(io6,*)'ERROR in subr. RDHD53 -- I,J do not match ',
     1      'values read on header record'
          write(io6,*)'I, J = ',i,j
          write(io6,*)'ICHECK, JCHECK = ',icheck,jcheck
          stop
        endif
50    continue

c -----------------------------------------------
c --- Print the 3D.DAT grid points to the QA file
c -----------------------------------------------
c      if(lprt)then
c         open(io4,file='QA3D.DAT',status='unknown')
c        
c         write(io4,*)'             3D.DAT Grid Points'
c         write(io4,*)'     X           Y        Longitude    Latitude'
c         write(io4,*)'   (km)        (km)        (deg E)     (deg N)'
c         do j=1,nyp
c         do i=1,nxp
c            write(io4,'(4f12.3)') xlcmm4(i,j),ylcmm4(i,j),
c     1                            xlong4(i,j),xlat4(i,j)
c         enddo
c         enddo
c          write(*,*)'             3D.DAT Grid Points'
c          write(*,*)'     X           Y        Longitude    Latitude'
c          write(*,*)'   (km)        (km)        (deg E)     (deg N)'
c          do j=1,nyp
c          do i=1,nxp
c             write(*,'(4f12.3)') xlcmm4(i,j),ylcmm4(i,j),
c     1                            xlong4(i,j),xlat4(i,j)
c         enddo
c         enddo
c
c         close(io4)
c      endif
c
      return
      end

c----------------------------------------------------------------------
      subroutine rdhd52
c----------------------------------------------------------------------
c
c --- CALMET   Version: 6.221     Level: 040917                  RDHD53
c              J. Scire, Earth Tech
c              Adapted from CALMET RDHD52 to read MM5_2DLQW
c
c --- PURPOSE: Read the header records from a file in 3D.DAT format 
c              (dataset Version 2.0 or later)
c
c --- MODIFICATION:
c     V5.545, level 031126 to V5.631, level 040917 (CEC)
c     RDHD52 from CALMET changed to read MM5_2DLQW
c
c --- INPUTS:
c
c        Parameters:
c            MXNXP, MXNYP, MXNZP, IO6, IO20, MXNX, MXNY
c        Common block /MAP/:
c            IUTMZN, UTMHEM, XLAT1, XLAT2, RELON0, RNLAT0,
c            FEAST, FNORTH, DATUM
c        Common block /GRID/:
c            NX,NY,DGRID,XORIGR,YORIGR
c        Common block /MM4HDO/ variables:
c            DATUM3D
c
c --- OUTPUT:
c
c        Common block /MM4HDO/ variables:
c            IBYRM, IBJULM, IBHRM, IEYRM, IEJULM, IEHRM,
c            NXMM4, NYMM4, NZP, PTOPMM4, I1, J1, NXP, NYP,
c            SIGMA(mxnzp), XLAT4(mxnxp,mxnyp), XLONG4(mxnxp,mxnyp),
c            IELEV4(mxnxp,mxnyp),ILU4(mxnxp,mxnyp),XLCMM4(mxnxp,mxnyp),
c            YLCMM4(mxnxp,mxnyp),IGRAB(mxnx,mxny,4),JGRAB(mxnx,mxny,4),
c            IOUTMM5,NCOMM3D, CNAME3D, CVER3D, CTITLE3D, COMM3D, 
c            DATUM3D
c
c --- RDHD53 called by:  RDHD5
c --- RDHD53 calls:      JULDAY, INCR, YR4, GLOBE1, GLOBE
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pst'
c
      character*128 ctemp3d
      logical lprt
c
c --- Common blocks
c      include 'grid.met'
      include 'mm4hdo.pst'
      include 'head.pst'

      COMMON /D6/ IRD,IWR,IFILE,IRDP

c --- For coordinate transformations
      character*8 cmapi,cmapo
      character*12 caction
      character*4 c4hem
      real*8 vecti(9),vecto(9)
c
      data lprt/.true./

c --- Scale factor for Tangential TM projection
      tmsone=1.00000
c --- Set translation vectors going from N.lat/E.lon
c --- to projection(x,y)km
      iutmo=iutmzn
      if(utmhem.EQ.'S   ' .AND. iutmzn.LT.900) iutmo=-iutmo
      cmapo=pmap
      if(cmapo.EQ.'TTM     ') cmapo='TM      '
      cmapi='LL      '
      idum=0
      rdum=0.0
      call GLOBE1(cmapi,idum,rdum,rdum,rdum,rdum,rdum,
     &            rdum,rdum,
     &            cmapo,iutmo,tmsone,xlat1,xlat2,rnlat0,relon0,
     &            feast,fnorth,
     &            caction,vecti,vecto)
c
      REWIND(in5)
c ------------------------------------------------------------------
c --- Read header record #1 (Dataset name, version and title
c ------------------------------------------------------------------
      read(in5,10)ctitle3d
10    format(a64)
      WRITE(io1,*)ctitle3d
c ------------------------------------------------------------------
c --- Read header record #2 (Number of comment lines)
c ------------------------------------------------------------------
      read(in5,11)cname3d,cver3d
11    format(a9,a6)

c -------------------------------------------
c --- Next header record (MM5 output options)
c -------------------------------------------
      read(in5,43)ioutw,ioutq,ioutc,iouti,ioutg
      ioutmm5=81+10*ioutw+ioutq+ioutc+iouti+ioutg
43    format(5(i3))

c --------------------------------------------
c --- Skip next header record (map projection)
c --------------------------------------------
      read(in5,*)

c ------------------------------------------------
c --- Next header record (MM5 output options)
c     (Add 13 output options for surface variables)
c ------------------------------------------------
      read(in5,44) inhyd,imphys,icupa,ibltyp,ifrad,isoil
     :         ,ifddaan,ifddaob
     :         ,igrdt,ipbl,ishf,ilhf,iustr,iswdn
     :         ,ilwdn,ist1,ist2,ist3,ist4,ist5,ist6
44    format(30(i3))
c --------------------------------------
c --- Next header record (MM5 grid data)
c --------------------------------------
      read(in5,20)ibyrm,ibmom,ibdym,ibhrm,nhrsmm5,
     1             nxp,nyp,nzp
 20   format(i4,3i2,i5,3i4)
      call YR4(io6,ibyrm,ierrb)
      if(ierrb.NE.0)then
         write(io6,*)'Error encountered in Subr. YR4 '
         write(io6,*)'Execution stopping in Subr. RDHD53 ',
     1   '-- IERRB = ',ierrb
         stop
      endif
c
c --- Calculate Julian day
      call julday(io6,ibyrm,ibmom,ibdym,ibjulm)
c
c --- Compute ending date/time (comment out if using other format)
      ieyrm=ibyrm
      iejulm=ibjulm
      iehrm=ibhrm
      call incr(io6,ieyrm,iejulm,iehrm,nhrsmm5)
c
c --------------------------------------------------
c --- Next header record (extraction subdomain data)
c --------------------------------------------------
      read(in5,30)nx1,ny1,nx2,ny2,nz1,nz2,
     &             rxmin,rxmax,rymin,rymax
      i1=nx1
      j1=ny1
30    format(6i4,2f10.4,2f9.4)
c

c --- Check that array dimensions are not exceeded
      if(nxp.gt.mxnxp.or.nyp.gt.mxnyp.or.nzp.gt.mxnzp)then
         write(io6,*)'ERROR in subr. RDHD53 -- Array dimensions ',
     1   'are too small for data being read'
         write(io6,*)'Grid being read  (NXP, NYP, NZP) = ',
     1    nxp,nyp,nzp
         write(io6,*)'Array dimensions (MXNXP, MXNYP, MXNZP) = ',
     1    mxnxp,mxnyp,mxnzp
         stop
      endif
c --- Check consistency between nz1,nz2, and nzp
      if(nzp.ne.nz2-nz1+1) then
        write(io6,*)'Error in RDHD53: NZ1,NZ2 and NZP not consistent'
        write(io6,*)'nz1,nz2,nzp:',nz1,nz2,nzp
        stop
      endif
c
c ---------------------------------------------
c --- Next NZP records -- MM5 half-sigma levels
c ---------------------------------------------
      do 40 n=1,nzp
         read(in5,38)sigma(n)
38       format(f6.3)
40    continue
c
c --------------------------------------------------------
c --- Print the 3D.DAT header information to the list file
c --- (except for the gridded fields)
c --------------------------------------------------------
      if(lprt)then
         write(io1,101)cname3d,cver3d,ctitle3d,comm3d
101      format(//1x,'Information read from 3D.DAT file'/
     1   5x,'Dataset Name:    ',a12/
     2   5x,'Dataset Version: ',a12/
     3   5x,'Dataset Title:   ',a64/
     4   5x,'First line of comments: ',
     5   8x,a132)
c
         write(io1,102) inhyd,imphys,icupa,ibltyp,ifrad,isoil,
     :               ifddaan,ifddaob
102      format(/5x,'MM5 physics options: '/
     1           5x,'    Hydrostatic:           ',i2/
     1           5x,'    Moisture scheme:       ',i2/
     1           5x,'    Convection scheme:     ',i2/
     1           5x,'    Boundary layer scheme: ',i2/
     1           5x,'    Radiation scheme       ',i2/
     1           5x,'    Soil scheme:           ',i2/
     1           5x,'    Analysis FDDA:         ',i2/
     1           5x,'    Observation FDDA:      ',i2)


         write(io1,1021)igrdt,ipbl,ishf,ilhf,iustr,iswdn
     :         ,ilwdn,ist1,ist2,ist3,ist4,ist5,ist6
1021      format(/5x,'MM5 surface variable options: '/
     1           5x,'    Ground temperature:    ',i2/
     1           5x,'    PBL:                   ',i2/
     1           5x,'    Sensible heat flux:    ',i2/
     1           5x,'    Latent heat flux:      ',i2/
     1           5x,'    Frictional velocity:   ',i2/
     1           5x,'    Downward SW radiation: ',i2/
     1           5x,'    Downward LW radiation: ',i2/
     1           5x,'    Soil temp at layer 1:  ',i2/
     1           5x,'    Soil temp at layer 2:  ',i2/
     1           5x,'    Soil temp at layer 3:  ',i2/
     1           5x,'    Soil temp at layer 4:  ',i2/
     1           5x,'    Soil temp at layer 5:  ',i2/
     1           5x,'    Soil temp at layer 6:  ',i2)

         write(io1,103)1,ioutw,ioutq,ioutc,iouti,ioutg
103      format(/5x,'MM5 output fields (1 = YES; 0 = NO): '/
     1           5x,' Pressure, height,T,Wind speed and direction: ',i2/
     1           5x,'                           Vertical velocity: ',i2/
     1           5x,'                   RH and vapor mixing ratio: ',i2/
     1           5x,'                Cloud and rain mixing ratios: ',i2/
     1           5x,'                  Ice and Snow mixing ratios: ',i2/
     1           5x,'                                     Graupel: ',i2)

         write(io1,104)ibyrm,ibmom,ibdym,ibhrm,nhrsmm5,
     1                 nxp,nyp,nzp
104      format(/5x,'Date/time (YYYYMMDDHH) of MM5 data: '/
     1           5x,'                  Start = ',i4,3i2,' (GMT)'/
     2           5x,'              No. hours = ',i4,/
     3           5x,'Extraction Subdomain in MM5 file: '/
     4           5x,'            No. X cells = ',i4/
     5           5x,'            No. Y cells = ',i4/
     6           5x,'            No. layers  = ',i4 )
c
         write(io1,106)nx1,ny1,nz1,nx2,ny2,nz2
106      format(/5x,'            Beginning X = ',i4/
     2           5x,'            Beginning Y = ',i4/
     3           5x,'            Beginning Z = ',i4/
     4           5x,'               Ending X = ',i4/
     5           5x,'               Ending Y = ',i4/
     6           5x,'               Ending Z = ',i4)
c
         write(io1,107)rymin,rymax,rxmin,rxmax
107      format(/5x,' Latitude range : ',f9.4,'  -  ',f9.4/
     :           5x,' Longitude range: ',f10.4, ' - ' ,f10.4)

         write(io1,108)
108      format(/5x,'MM5 half-sigma levels'/5x,'Level',5x,'Sigma'/)
         do 110 i=1,nzp
            write(io1,109)i,sigma(i)
110      continue
109      format(4x,i4,6x,f6.4)
      endif
c
c ----------------------------------------------------
c --- Next NXP * NYP records -- lat., long., elevation
c ----------------------------------------------------
      do 50 j=1,nyp
      do 50 i=1,nxp
         read(in5,99)iindex,jindex,xlat4(i,j),xlong4(i,j),
     &                ielev4(i,j)
 99      format(2i4,f9.4,f10.4,i5)
c ---   Compute grid point locations from N.Lat and E.Lon
        call GLOBE(io6,caction,datum3d,vecti,datum,vecto,
     &             xlong4(i,j),xlat4(i,j),xlcmm4(i,j),ylcmm4(i,j),
     &             idum,c4hem)
c
c ---   QA check that I,J read match expected values
c
        icheck=iindex-i1+1
        jcheck=jindex-j1+1
        if(icheck.ne.i.or.jcheck.ne.j)then
          write(io6,*)'ERROR in subr. RDHD53 -- I,J do not match ',
     1      'values read on header record'
          write(io6,*)'I, J = ',i,j
          write(io6,*)'ICHECK, JCHECK = ',icheck,jcheck
          stop
        endif
50    continue

c -----------------------------------------------
c --- Print the 3D.DAT grid points to the QA file
c -----------------------------------------------
c      if(lprt)then
c         open(io4,file='QA3D.DAT',status='unknown')
c        
c         write(io4,*)'             3D.DAT Grid Points'
c         write(io4,*)'     X           Y        Longitude    Latitude'
c         write(io4,*)'   (km)        (km)        (deg E)     (deg N)'
c         do j=1,nyp
c         do i=1,nxp
c            write(io4,'(4f12.3)') xlcmm4(i,j),ylcmm4(i,j),
c     1                            xlong4(i,j),xlat4(i,j)
c         enddo
c         enddo
c          write(*,*)'             3D.DAT Grid Points'
c          write(*,*)'     X           Y        Longitude    Latitude'
c          write(*,*)'   (km)        (km)        (deg E)     (deg N)'
c          do j=1,nyp
c          do i=1,nxp
c             write(*,'(4f12.3)') xlcmm4(i,j),ylcmm4(i,j),
c     1                            xlong4(i,j),xlat4(i,j)
c         enddo
c         enddo
c
c         close(io4)
c      endif
c
      return
      end

c-----------------------------------------------------------------------
      subroutine rdsamp
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050729            RDSAMP
c ---           D. Strimaitis, Earth Tech
c
c  PURPOSE:     Read sampler data into memory and set times to UTC
c
c  ARGUMENTS:
c     PASSED:   none
c
c --- OUTPUT:
c
c       Common block /SAMP/
c              nsamp,nsdat,ibutc,ibutcs,ieutc,ieutcs,
c              xsamp(mxsdat),irsamp(mxsdat),irmap(mxsamp),
c              ibdathr(mxsdat),iedathr(mxsdat),cidsamp(mxsamp),
c              csspec(mxsdat),cbsamp(mxsdat),cesamp(mxsdat)
c
c  CALLING ROUTINES:    READCF
c
c  EXTERNAL ROUTINES:   ALLCAP, DEBLNK, TLEFT, TRIGHT, TDATE, INCR
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'filnam.pst'
      INCLUDE 'samp.pst'

      character*12 aspec12, blank12
      character*16 dataset,dataver, cbdathr,cedathr
      character*24 char24,blank24
      character*28 cstart,cend
      character*64 datamod
      character*80 comment1
      character*120 aline, awork1, awork2, blank120

      logical lerror,ldb

      data lerror/.FALSE./,ldb/.FALSE./
      data blank12/'            '/
      data blank24/'                        '/

c --- Convert times to whole hours using a minutes cutoff MMCUT
c --- (starting HHMM for MM>60-MMCUT is set to HH+1;
c ---    ending HHMM for MM>MMCUT    is set to HH+1)
      data mmcut/15/

      i2=0
      do i=1,5
         i1=i2+1
         i2=i2+24
         blank120(i1:i2)=blank24
      enddo

c --- Open file
      open(in6,file=sampdat,status='old')

c --- Identify echo output to list file
      write(io1,*)
      write(io1,*)
      write(io1,*)'Header information from SAMPLER file:  ',sampdat
      write(io1,*)'-------------------------------------'
      write(io1,*)

c ------------------
c --- Header records
c ------------------

c --- Dataset, Version, Modifier
      read(in6,'(2a16,a64)') dataset,dataver,datamod
      write(io1,'(2a16,a64)') dataset,dataver,datamod
c --- Convert Dataset to upper case
      do i=1,16
         call ALLCAP(dataset(i:i),nlim)
      enddo

c --- Identify dataset type
      if(dataset.NE.'SAMPLER.DAT') then
c ---    FATAL ERROR
         write(io1,*)
         write(io1,*)'RDSAMP: Invalid input file DATASET type: ',dataset
         write(io1,*)'        Expected SAMPLER.DAT'
         lerror=.TRUE.
         goto 999
      endif

c --- Number of comment records
      read(in6,*) ncomment
      write(io1,*) ncomment
c --- Comments (optional/repeatable)
      do k=1,ncomment
         read(in6,'(a80)') comment1
         write(io1,'(a80)') comment1
      enddo

c --- Receptor mapping to samplers
      nsamp=0
10    aline=blank120
      read(in6,'(a80)') aline
      write(io1,'(a80)') aline
      char24=aline(1:24)
      if(char24.EQ.'Receptor -> Sampler ID :') then
c ---    Assign receptor index for sampler
         nsamp=nsamp+1
c ---    Isolate data fields
         aline(1:24)=blank24
         call TLEFT(aline,1,120,awork2,n2)
         call TRIGHT(awork2,1,n2,awork1,n1)
c ---    Identify assignment equals sign
         ieq=0
         do k=n1,1,-1
            if(awork1(k:k).EQ.'=') ieq=k
         enddo
c ---    Extract receptor index
         call DEBLNK(awork1,1,ieq-1,awork2,n2)
         read(awork2(1:n2),'(i121)') irmap(nsamp)
c ---    Extract sampler name
         call TLEFT(awork1,ieq+1,n1,awork2,n2)
         call TRIGHT(awork2,1,n2,awork1,n1)
         cidsamp(nsamp)=awork1(1:n1)
         goto 10
      elseif(char24.NE.'------------------------') then
c ---    FATAL ERROR
         write(io1,*)
         write(io1,*)'RDSAMP: Unexpected header record found'
         lerror=.TRUE.
         goto 999
      endif

c ----------------------------------
c --- Loop over sampler data records
c ----------------------------------
      ibutc=999999999
      ieutc=0
      ibutcs=0
      ieutcs=0
      nsdat=0
100   char24=blank24
      aspec12=blank12
      read(in6,*,end=200) char24,aspec12,cstart,cend,xval
      nsdat=nsdat+1
      if(nsdat.GT.mxsdat)then
c ---    FATAL ERROR
         write(io1,*)
         write(io1,*)'RDSAMP: Too many sampler data records: ',nsdat
         write(io1,*)'           MXSDAT parameter is set to: ',mxsdat
         lerror=.TRUE.
         goto 999
      endif
c --- Assign receptor index
      irsamp(nsdat)=0
      do k=1,nsamp
         if(char24.EQ.cidsamp(k)) irsamp(nsdat)=irmap(k)
      enddo
      if(irsamp(nsdat).EQ.0) then
c ---    FATAL ERROR
         write(io1,*)
         write(io1,*)'RDSAMP: Sampler ID in data does not match header'
         write(io1,*)'                    ID in data record: ',char24
         lerror=.TRUE.
         goto 999
      endif
c --- Assign species, dates, and measure/factor to arrays
      csspec(nsdat)=aspec12
      cbsamp(nsdat)=cstart
      cesamp(nsdat)=cend
      xsamp(nsdat)=xval

c --- Process start date-time to UTC (no fractional time zone yet!)
      read(cstart(22:24),'(i3)') ihhutc
      read(cstart(16:17),'(i2)') mm
      cbdathr=cstart(1:16)
      cbdathr(16:16)='0'
      call TDATE(io1,1,cbdathr,ibdathr(nsdat),ibyr,ibmo,ibday,ibjul,
     &           ibhr)
      nhrinc=-ihhutc
      if(mm.GE.(60-mmcut)) nhrinc=nhrinc+1
      call INCR(io1,ibyr,ibjul,ibhr,nhrinc)
      call TDATE(io1,4,cbdathr,ibdathr(nsdat),ibyr,ibmo,ibday,ibjul,
     &           ibhr)
c --- Save earliest date-hour
      if(ibdathr(nsdat).LT.ibutc) ibutc=ibdathr(nsdat)

c --- Process end date-time to UTC (no fractional time zone yet!)
      read(cend(22:24),'(i3)') ihhutc
      read(cend(16:17),'(i2)') mm
      cedathr=cend(1:16)
      cedathr(16:16)='0'
      call TDATE(io1,1,cedathr,iedathr(nsdat),ieyr,iemo,ieday,iejul,
     &           iehr)
      nhrinc=-ihhutc
      if(mm.GE.mmcut) nhrinc=nhrinc+1
      call INCR(io1,ieyr,iejul,iehr,nhrinc)
      call TDATE(io1,4,cedathr,iedathr(nsdat),ieyr,iemo,ieday,iejul,
     &           iehr)
c --- Save latest date-hour
      if(iedathr(nsdat).GT.ieutc) ieutc=iedathr(nsdat)

c --- Next
      goto 100


200   continue

c --- DEBUG
      if(LDB) then
      write(*,*)
      write(*,*)'RDSAMP Results -----------------------------'
      write(*,*)'nsamp,nsdat = ',nsamp,nsdat
      write(*,*)'ibutc,ieutc = ',ibutc,ieutc
      do k=1,nsdat
         write(*,*) irsamp(k),ibdathr(k),iedathr(k),csspec(k)
      enddo
      write(*,*)
      endif


999   continue
      if(lerror) then
         write(*,*)
         write(*,*)'FATAL ERROR in reading SAMPLER file ',sampdat
         stop 'Halted in RDSAMP --- see list file'
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine setsamp
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050729           SETSAMP
c ---           D. Strimaitis, Earth Tech
c
c  PURPOSE:     Using information from CALPUFF.DAT header, reset
c               date-times from UTC to LST and set species 
c               pointer array ISSAMP.
c
c --- INTPUTS:
c
c       Common block /HEAD/
c              nszout,xbtz,asplst(mxsplv)
c
c       Common block /CTRL/
c              isyr,ismo,isdy,jsday,ishr,asplv
c
c       Common block /SAMP/
c              nsamp,nsdat,
c              ibdathr(mxsdat),iedathr(mxsdat),csspec(mxsdat)
c
c --- OUTPUT:
c
c       Common block /CTRL/
c              isyr,ismo,isdy,jsday,ishr,aspec,asplv
c
c       Common block /SAMP/
c              issamp(mxsdat),ibdathr(mxsdat),iedathr(mxsdat)
c
c  CALLING ROUTINES:    READCF
c
c  EXTERNAL ROUTINES:   INCR, TDATE
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'samp.pst'

      character*16 cdathr
      logical lerror, ldb

      data lerror/.FALSE./,ldb/.FALSE./

c --- Time shift from UTC to LST (CALPUFF base time zone)
      nhrinc=-NINT(xbtz)

c --- Adjust the start time from UTC to LST (CALPUFF base time zone)
      call INCR(io1,isyr,jsday,ishr,nhrinc)
      call TDATE(io1,4,cdathr,idathr,isyr,ismo,isdy,jsday,ishr)

c --- Loop over data records from SAMPDAT file
      do i=1,nsdat
c ---    Start time
         call TDATE(io1,2,cdathr,ibdathr(i),iyr,imo,idy,jul,ihr)
         call INCR(io1,iyr,jul,ihr,nhrinc)
         call TDATE(io1,4,cdathr,ibdathr(i),iyr,imo,idy,jul,ihr)
c ---    End time
         call TDATE(io1,2,cdathr,iedathr(i),iyr,imo,idy,jul,ihr)
         call INCR(io1,iyr,jul,ihr,nhrinc)
         call TDATE(io1,4,cdathr,iedathr(i),iyr,imo,idy,jul,ihr)
c ---    Species
         asplv(1:12)=csspec(i)
         issamp(i)=0
         do k=1,nszout
            if(asplv.EQ.asplst(k)) issamp(i)=k
         enddo
         if(issamp(i).EQ.0) then
c ---       FATAL ERROR
            write(io1,*)
            write(io1,*)'SETSAMP: Sampler species not found: ',asplv
            write(io1,*)'         Expected:'
            do k=1,nszout
               write(io1,*)'                  ',asplst(k)
            enddo
            lerror=.TRUE.
            goto 999
         endif
      enddo

c --- Return the last species in the list as aspec
      aspec=asplv(1:12)

c --- DEBUG
      if(LDB) then
      write(*,*)
      write(*,*)'SETSAMP Results -----------------------------'
      write(*,*)'ibdathr,iedathr,csspec,asplst(issamp)'
      write(*,*)
      do k=1,nsdat
         write(*,*) ibdathr(k),iedathr(k),csspec(k),asplst(issamp(k))
      enddo
      write(*,*)
      endif

999   continue
      if(lerror) then
         write(*,*)
         write(*,*)'FATAL ERROR in SAMPLER species ',csspec(i)
         stop 'Halted in SETSAMP --- see list file'
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine sumsamp(isrc,ispl,tcd,idrec)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050729           SUMCONC
c ---           D. Strimaitis, Earth Tech
c
c  PURPOSE:     Sums current concentrations to appropriate averaging
c               array for SAMPLER processing
c
c  ARGUMENTS:
c     PASSED:   isrc      Current source index                       [i]
c               ispl      Current species-level                      [i]
c               tcd()     Temporary discrete receptor array         [ra]
c               idrec     Dimension for tcd array                    [i]
c
c       Common block /CONC/
c               myrb,mjdayb,mhrb,msecb,myre,mjdaye,mhre,msece
c       Common block /CTRL/
c               msampler
c       Common block /SAMP/
c               nsdat,irsamp,issamp,ibdathr,iedathr,
c               caverage,naverage
c
c   RETURNED:
c
c       Common block /SAMP/
c               caverage,naverage
c
c  CALLING ROUTINES:    GETRCRD
c
c  EXTERNAL ROUTINES:   TPERIOD
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'samp.pst'

      real tcd(idrec)
      logical ldb

      data ldb/.FALSE./

      nsec=0

c --- Set date-time markers for current concentrations
      ndathrb=myrb*100000+mjdayb*100+mhrb
      ndathre=myre*100000+mjdaye*100+mhre

c --- Loop over sampler data records
      do i=1,nsdat

c ---    Match species-level
         if(ispl.EQ.issamp(i)) then

c ---       Check for time period within sampling window
            call TPERIOD(-1,ndathrb,msecb,ibdathr(i),nsec,iedathr(i),
     &                   nsec,ilocb)
            call TPERIOD(1,ndathre,msece,ibdathr(i),nsec,iedathr(i),
     &                   nsec,iloce)
            if(ilocb.LT.1 .AND. iloce.GT.-1) then

               if(LDB) write(*,*)'SUMSAMP: matched endtime',ndathre
               if(LDB) write(*,*)'         for receptor   ',irsamp(i)
               if(LDB) write(*,*)'    in SAMPLER record   ',i

               if(msampler.EQ.1) then
c ---             Sum total impact at sampler
                  caverage(1,i)=caverage(1,i)+tcd(irsamp(i))
                  naverage(i)=naverage(i)+1
               else
c ---             Sum impact at sampler by "source"
                  caverage(isrc,i)=caverage(isrc,i)+tcd(irsamp(i))
                  if(isrc.EQ.1) naverage(i)=naverage(i)+1
               endif

            endif
         endif
      enddo

      if(ldb) then
         do k=1,nsdat
            write(*,*) caverage(1,k),naverage(k)
         enddo
         write(*,*)
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine avgsamp
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050729           AVGSAMP
c ---           D. Strimaitis, Earth Tech
c
c  PURPOSE:     Divides summed concentration arrays by number of
c               averaging periods to obtain average concentrations
c               for SAMPLER processing
c
c  ARGUMENTS:
c     PASSED:   
c
c       Common block /CTRL/
c               msampler
c       Common block /SAMP/
c               nsdat,caverage,naverage
c       Common block /SOURCE/
c               nsrc
c
c   RETURNED:
c
c       Common block /SAMP/
c               caverage
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'samp.pst'
      INCLUDE 'source.pst'

      logical ldb
      data ldb/.FALSE./

c --- Set "source" mode
      ksrc=1
      if(msampler.GT.1) ksrc=nsrc

c --- Loop over SAMPLER data records
      do i=1,nsdat
         if(naverage(i).LT.1) stop 'Halted in AVGSAMP -- N is 0'

c ---    Set scale factor
         factor=1.0
         if(msampler.EQ.3) factor=xsamp(i)

c ---    Source TRACEBACK uses discrete receptor array to store impact
c ---    of each source at ONE sampler (receptor)
         do k=1,ksrc
            caverage(k,i)=factor*caverage(k,i)/naverage(i)
         enddo
      enddo


      if(ldb) then
         do i=1,nsdat
            write(*,*) i,caverage(1,i)
         enddo
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine wrisamp(ichan)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050729           WRISAMP
c ---           D. Strimaitis, Earth Tech
c
c  PURPOSE:     Writes title lines and results for SAMPLER processing
c               to list file (average from all sources)
c
c  ARGUMENTS:
c     PASSED:   ichan           I/O unit for file                    [i]
c
c     Common block /CTRL/
c               units
c     Common block /HEAD/
c               xrec(mxdrec),yrec(mxdrec)
c     Common block /SAMP/
c               nsamp,nsdat,
c               xsamp,irsamp,issamp,
c               caverage,naverage,irmap,
c               cidsamp,csspec,cbsamp,cesamp
c
c   RETURNED:   none
c
c  CALLING ROUTINES:    CALPOST
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'samp.pst'

      character*15 sumry
      data sumry/'SAMPLER RESULTS'/

c --- Identify units
      write(ichan,90) sumry
      write(ichan,90) units

c --- Write column headings
      write(ichan,110)
      write(ichan,111)

c --- Loop over SAMPLER data records
      do i=1,nsdat
         k=irsamp(i)
         do j=1,nsamp
            if(k.EQ.irmap(j)) kk=j
         enddo
         write(ichan,120) k,xrec(k),yrec(k),csspec(i),cbsamp(i),
     &                    cesamp(i),caverage(1,i),naverage(i),
     &                    cidsamp(kk)
      enddo

      return

90    format(1x,t57,a15,//)
110   format(T103,'Modeled    Model')
111   format(' SAMPLER   COORDINATES (km) SPECIES  ',5x,
     *       'Starting Date-Time',10x,'Ending Date-Time',16x,
     *       'Average   Periods  SAMPLER ID')
120   format(i6,1x,2f10.3,1x,a12,2x,2a28,1x,1pe12.4,i6,4x,a24)

      end

c----------------------------------------------------------------------
      subroutine tdate(io,mode,cdathr,idathr,iyr,imo,iday,ijul,ihr)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050729            TDATE
c ---           D. Strimaitis, Earth Tech
c
c --- PURPOSE:  Transform date-hour variables
c
c --- INPUTS:
c                IO - integer    - Unit number for list file output
c              MODE - integer    - Start for transformation
c                                  1 - Convert from cdathr string
c                                  2 - Convert from idathr integer
c                                  3 - Convert from Gregorian date
c                                  4 - Convert from Julian date
c            CDATHR - character  - Date-time (yyyy_Mmm_Ddd_hh0)
c            IDATHR - integer    - Date-time (YYYYJJJHH)
c               IYR - integer    - Year (4 digits)
c               IMO - integer    - Month
c              IDAY - integer    - Day
c              IJUL - integer    - Julian day
c               IHR - integer    - Hour (0-23)
c
c --- OUTPUT:
c            CDATHR - character  - Date-time (yyyy_Mmm_Ddd_hh0)
c            IDATHR - integer    - Date-time (YYYYJJJHH)
c               IYR - integer    - Year (4 digits)
c               IMO - integer    - Month
c              IDAY - integer    - Day
c              IJUL - integer    - Julian day
c               IHR - integer    - Hour (0-23)
c
c --- TDATE called by:  host subroutines
c --- TDATE calls:      JULDAY, GRDAY
c----------------------------------------------------------------------

      character*16 cdathr

      if(mode.EQ.1) then
c ---    Convert from character string
         read(cdathr(1:4),'(i4)') iyr
         read(cdathr(7:8),'(i2)') imo
         read(cdathr(11:12),'(i2)') iday
         read(cdathr(14:15),'(i2)') ihr
         call JULDAY(io,iyr,imo,iday,ijul)
         idathr=iyr*100000+ijul*100+ihr
         
      elseif(mode.EQ.2) then
c ---    Convert from combined integer
         iyr=idathr/100000
         ijul=idathr/100-iyr*1000
         ihr=idathr-iyr*100000-ijul*100
         call GRDAY(io,iyr,ijul,imo,iday)
         cdathr='yyyy_Mmm_Ddd_hh0'
         write(cdathr(1:4),'(i4.4)') iyr
         write(cdathr(7:8),'(i2.2)') imo
         write(cdathr(11:12),'(i2.2)') iday
         write(cdathr(14:15),'(i2.2)') ihr
         
      elseif(mode.EQ.3) then
c ---    Convert from Gregorian date
         call JULDAY(io,iyr,imo,iday,ijul)
         idathr=iyr*100000+ijul*100+ihr
         cdathr='yyyy_Mmm_Ddd_hh0'
         write(cdathr(1:4),'(i4.4)') iyr
         write(cdathr(7:8),'(i2.2)') imo
         write(cdathr(11:12),'(i2.2)') iday
         write(cdathr(14:15),'(i2.2)') ihr
         
      elseif(mode.EQ.4) then
c ---    Convert from Julian date
         call GRDAY(io,iyr,ijul,imo,iday)
         idathr=iyr*100000+ijul*100+ihr
         cdathr='yyyy_Mmm_Ddd_hh0'
         write(cdathr(1:4),'(i4.4)') iyr
         write(cdathr(7:8),'(i2.2)') imo
         write(cdathr(11:12),'(i2.2)') iday
         write(cdathr(14:15),'(i2.2)') ihr
         
      else
c ---    Problem
         write(io,*)'Invalid MODE in subroutine TDATE'
         write(io,*)'Expected MODE= 1, 2, 3, or 4'
         write(io,*)'Found    MODE= ',mode
         stop

      endif

      return
      end

c----------------------------------------------------------------------
      subroutine tperiod(ibe,ndhr,nsec,ndhrb,nsecb,ndhre,nsece,ilocate)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 050729           TPERIOD
c ---           D. Strimaitis, Earth Tech
c
c --- PURPOSE:  Tests year/day/hour and seconds variables at beginning
c               and end of period against target date-time to see if
c               target lies within period
c
c --- INPUTS:
c           IBE - integer - Treatment of Beginning and End of period
c                           -1: Beginning time within period (not end)
c                            0: Beginning & End are within period
c                            1: Ending time within period (not begin)
c          NDHR - integer - YYYYJJJHH of target (year-day-hour)
c          NSEC - integer - SSSS of target (seconds)
c         NDHRB - integer - YYYYJJJHH at beginning of period
c         NSECB - integer - SSSS at beginning of period
c         NDHRE - integer - YYYYJJJHH at end of period
c         NSECE - integer - SSSS at end of period
c
c
c --- OUTPUT:
c       ILOCATE - integer - Location of target relative to period
c                           -1: target preceeds period
c                            0: target within period
c                            1: target lies beyond period
c
c --- TPERIOD called by:  host subroutines
c --- TPERIOD calls:      none
c----------------------------------------------------------------------

      ilocate=0
c
      if(ndhr.LT.ndhrb) then
c ---    Target date-hr before start of period
         ilocate=-1
      elseif(ndhr.GT.ndhre) then
c ---    Target date-hr after end of period
         ilocate=1
      elseif(ndhr.EQ.ndhrb) then
         if(nsec.LT.nsecb) then
c ---       Target time before start of period
            ilocate=-1
         elseif(ibe.EQ.1 .AND. nsec.EQ.nsecb) then
c ---       Target time before start of period
            ilocate=-1
         elseif(ndhrb.EQ.ndhre) then
            if(nsec.GT.nsece) then
c ---          Target time after end of period
               ilocate=1
            elseif(ibe.EQ.-1 .AND. nsec.EQ.nsece) then
c ---          Target time after end of period
               ilocate=1
            endif
         endif
      elseif(ndhr.EQ.ndhre) then
         if(nsec.GT.nsece) then
c ---       Target time after end of period
            ilocate=1
         elseif(ibe.EQ.-1 .AND. nsec.EQ.nsece) then
c ---       Target time after end of period
            ilocate=1
         endif
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine imprv06(ees,eel,frhs,frhl,back,value)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 061107          IMPRV06
c               D. Strimaitis
c
c --- PURPOSE:  Compute light extinction coefficient (Mm-1) for 1
c               species using the 2006 IMPROVE method.
c
c --- INPUTS:
c              EES - real    - Extinction efficiency for small particle
c                              component (Mm-1 / ug/m3)
c              EEL - real    - Extinction efficiency for large particle
c                              component (Mm-1 / ug/m3)
c             FRHS - real    - Humidity growth factor for small particle
c                              component
c             FRHL - real    - Humidity growth factor for large particle
c                              component
c             BACK - real    - Background concentration (ug/m3) for this
c                              species
c            VALUE - real    - Concentration (ug/m3) for this species
c
c --- OUTPUT:
c            VALUE - real    - Light extinction (Mm-1) for this species
c
c --- IMPRV06 called by:  GETRCRD
c --- IMPRV06 calls:      none
c-----------------------------------------------------------------------
c --- Large particle extinction is used for all concentrations > CLARGE
c --- (ug/m3)
      data clarge/20.0/

c --- Add background to target concentration for total extinction
      ctotal=value+back
      flarge=AMIN1(1.0,ctotal/clarge)
      fsmall=1.0-flarge
      btotal=ctotal*(fsmall*ees*frhs+flarge*eel*frhl)

c --- Process background alone
      flarge=AMIN1(1.0,back/clarge)
      fsmall=1.0-flarge
      bback=back*(fsmall*ees*frhs+flarge*eel*frhl)

c --- Report net change in extinction (total - background)
      value=btotal-bback

      return
      end

c----------------------------------------------------------------------
      subroutine mapspec
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 070712          MAPSPEC
c               D. Strimaitis
c
c --- PURPOSE:  Map array of species-levels in the input file to the
c               list of species-levels used to create outputs
c
c --- UPDATES:
c     V6.141(061120) to V6.2(070712)
c               (DGS) Add NO2 calculation from NOx concentrations
c
c --- INPUTS:
c     Common block /CTRL/
c               aspec,asplv,ilayer,lvisib,no2calc
c     Common block /HEAD/
c               nszout,asplst(mxsplv)
c     Common block /VISB/
c               specpmc,specpmf
c
c --- OUTPUT:
c     Common block /SPECOUT/
c               nospec,osplst(mxsplv),mapospec(mxsplv),
c               iso4,ino2,inox,ino3,ipmf,ipmc,ioc,iec,
c
c --- MAPSPEC called by:  main
c --- MAPSPEC calls:      none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'
      INCLUDE 'specout.pst'
      INCLUDE 'visb.pst'

      character*3 tlev, c3
      character*12 nitrate, sulfate, orgcarb, lmncarb
      character*12 no2gas,noxgas

c --- Species names for visibility processing
      data nitrate/'NO3         '/,sulfate/'SO4         '/
      data orgcarb/'SOA         '/,lmncarb/'EC          '/
      data no2gas /'NO2         '/,noxgas /'NOX         '/

      nospec=0
      iso4=0
      ino2=0
      inox=0
      ino3=0
      ipmf=0
      ipmc=0
      ioc=0
      iec=0

c --- Set target 'level' for this application
      if(LVISIB)then
         tlev='  1'
      elseif(ilayer .GT. 0) then
         write(tlev,'(i3)') ilayer
      elseif(ilayer .EQ. -1) then
         tlev=' DF'
      elseif(ilayer .EQ. -2) then
         tlev=' WF'
      elseif(ilayer .EQ. -3) then
         tlev=' TF'
      endif
      
c --- Loop over the array of species-level names in input file
      do i=1,nszout
         c3=asplst(i)(13:15)
         if(c3.NE.tlev) then
            mapospec(i)=0
         else
c ---       Store all species on the target level
            nospec=nospec+1
            mapospec(i)=nospec
            osplst(nospec)=asplst(i)
         endif
      enddo

c --- Loop over the array of species-level names used and set
c --- pointers to the species needed for visibility
      do i=1,nospec
         if(osplst(i)(1:12) .EQ. sulfate) iso4=i
         if(osplst(i)(1:12) .EQ. no2gas)  ino2=i
         if(osplst(i)(1:12) .EQ. noxgas)  inox=i
         if(osplst(i)(1:12) .EQ. nitrate) ino3=i
         if(osplst(i)(1:12) .EQ. specpmf) ipmf=i
         if(osplst(i)(1:12) .EQ. specpmc) ipmc=i
         if(osplst(i)(1:12) .EQ. orgcarb) ioc=i
         if(osplst(i)(1:12) .EQ. lmncarb) iec=i
      enddo

c --- Add NO2 if computed from NOx
      if(no2calc.GT.0 .AND. ino2.EQ.0) then
         nospec=nospec+1
         mapospec(nszout+1)=nospec
         osplst(nospec)(1:12)=no2gas
         osplst(nospec)(13:15)=asplst(1)(13:15)
         ino2=nospec
      endif

c --- Report result to list file
      write(io1,*)
      write(io1,*)'MAPSPEC:  Species Mapping'
      write(io1,*)'  Number of species-levels in file  : ',nszout
      write(io1,*)'  Number of species-levels processed: ',nospec
      write(io1,*)
      write(io1,*)'  Input ID  Processing ID     Name'
      do i=1,nszout
         write(io1,'(i8,i12,10x,a15)') i,mapospec(i),asplst(i)
      enddo
      write(io1,*)
      write(io1,*)'     Visibility Species'
      write(io1,*)'            Processing ID     Name'
      if(iso4.GT.0) 
     &   write(io1,'(a10,i10,10x,a15)')'sulfate   ',iso4,osplst(iso4)
      if(ino2.GT.0) 
     &   write(io1,'(a10,i10,10x,a15)')'no2gas    ',ino2,osplst(ino2)
      if(inox.GT.0) 
     &   write(io1,'(a10,i10,10x,a15)')'noxgas    ',inox,osplst(inox)
      if(ino3.GT.0) 
     &   write(io1,'(a10,i10,10x,a15)')'nitrate   ',ino3,osplst(ino3)
      if(ipmf.GT.0) 
     &   write(io1,'(a10,i10,10x,a15)')'specpmf   ',ipmf,osplst(ipmf)
      if(ipmc.GT.0) 
     &   write(io1,'(a10,i10,10x,a15)')'specpmc   ',ipmc,osplst(ipmc)
      if(ioc.GT.0) 
     &   write(io1,'(a10,i10,10x,a15)')'orgcarb   ',ioc,osplst(ioc)
      if(iec.GT.0) 
     &   write(io1,'(a10,i10,10x,a15)')'lmncarb   ',iec,osplst(iec)

      return
      end

c----------------------------------------------------------------------
      subroutine BEXT_24(avg,avd,avt,myr,mjday,mhr,msec,
     *                   aveg,aved,avet,avbg,avbd,avbt,
     *                   avhg,avhd,avht,avpg,avpd,avpt)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080709          BEXT_24
c               D. Strimaitis
c
c --- PURPOSE:  Compute the 24-hour extinction coefficients using
c               the 24-hour average modeled concentrations with monthly
c               f(RH) and monthly background (METHOD 8)
c
c --- UPDATES:
c     V6.2(070712) to V6.22(080709)
c               (DGS) Add M8_MODE=4,5 option for using F(RH) factors
c     V6.142(061218) to V6.2(070712)
c               (DGS) Remove FNO2NOX factor with NOx average because
c                     conversion NOx-to-NO2 is done in GRTRCRD
c     V6.141(061120) to V6.142(061218)
c               (DGS) Add visibility species selection screen
c
c --- INPUTS:
c         AVG - real array - 24-hour concentrations (gridded rec)
c         AVD - real array - 24-hour concentrations (discrete rec)
c         AVT - real array - 24-hour concentrations (terrain rec)
c         MYR - integer    - Year for date/time marker
c       MJDAY - integer    - Day for date/time marker
c         MHR - integer    - Hour for date/time marker
c        MSEC - integer    - Second for date/time marker
c
c     Common block /CTRL/
c               ibgrid,jbgrid,iegrid,jegrid,
c               LG,LD,LCT,LCTSG
c     Common block /HEAD/
c               ndrec,nctrec,LSGRID
c     Common block /SPECOUT/
c               nospec,
c               iso4,ino2,inox,ino3,ipmf,ipmc,ioc,iec
c     Common block /VISB/
c               LVSO4,LVNO3,LVNO2,LVOC,LVPMC,LVPMF,LVEC,LVBK,
c               eepmc,eepmf,eeec,eeno2,
c               eeso4s,eeno3s,eeocs,eeso4l,eeno3l,eeocl,
c               bkso4(12),bkno3(12),bkoc(12),
c               irhmonth(12),bextdry2(12),bexthyg2(12),
c               bexthygs2(12),bexthygl2(12),bextsalt2(12),
c               rhfsml(12),rhflrg(12),rhfsea(12),
c               xSO4,xNO3
c
c --- OUTPUT:
c  AVE[G,D,T] - real array - 24-hour extinctions
c  AVB[G,D,T] - real array - 24-hour background extinctions
c  AVH[G,D,T] - real array - 24-hour RH (fraction)
c  AVP[G,D,T] - real array - 24-hour source/background extinctions
c
c     Common block /VISEXT/
c             av24s4g(mxgx,mxgy),av24s4d(mxdrec),av24s4t(mxctrec),
c             av24n3g(mxgx,mxgy),av24n3d(mxdrec),av24n3t(mxctrec),
c             av24n2g(mxgx,mxgy),av24n2d(mxdrec),av24n2t(mxctrec),
c             av24ocg(mxgx,mxgy),av24ocd(mxdrec),av24oct(mxctrec),
c             av24ecg(mxgx,mxgy),av24ecd(mxdrec),av24ect(mxctrec),
c             av24pcg(mxgx,mxgy),av24pcd(mxdrec),av24pct(mxctrec),
c             av24pfg(mxgx,mxgy),av24pfd(mxdrec),av24pft(mxctrec),
c     Common block /CONC/
c             gbext0(mxgx,mxgy),dbext0(mxdrec),tbext0(mxctrec),
c             gfrhs(mxgx,mxgy),dfrhs(mxdrec),tfrhs(mxctrec),
c             gfrhl(mxgx,mxgy),dfrhl(mxdrec),tfrhl(mxctrec),
c             gfrhss(mxgx,mxgy),dfrhss(mxdrec),tfrhss(mxctrec)
c
c --- BEXT_24 called by:  main
c --- BEXT_24 calls:      GRDAY, FRH4, IMPRV06
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'
      parameter (mxsp1=mxspec+1)
      parameter (mxg=mxgx*mxgy*mxsp1)
      parameter (mxd=mxdrec*mxsp1)
      parameter (mxt=mxctrec*mxsp1)

      INCLUDE 'ctrl.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'head.pst'
      INCLUDE 'specout.pst'
      INCLUDE 'visb.pst'
      INCLUDE 'vispec.pst'

c --- 24-hour average concentrations passed in
      real avg(mxgx,mxgy,mxspec),avd(mxdrec,mxspec)
      real avt(mxctrec,mxspec)

c --- Local 24-hour averages with slot for species index 0
      real avg0(mxgx,mxgy,0:mxspec),avd0(mxdrec,0:mxspec)
      real avt0(mxctrec,0:mxspec)

c --- 24-hour extinctions, humidities, etc. returned
      real aveg(mxgx,mxgy),aved(mxdrec),avet(mxctrec)
      real avbg(mxgx,mxgy),avbd(mxdrec),avbt(mxctrec)
      real avhg(mxgx,mxgy),avhd(mxdrec),avht(mxctrec)
      real avpg(mxgx,mxgy),avpd(mxdrec),avpt(mxctrec)

c --- Initialize all elements of local arrays
      data avg0/mxg*0.0/, avd0/mxd*0.0/, avt0/mxt*0.0/

      data one/1.0/, zero/0.0/

c --- Factor to convert g/m3 to micro-g/m3 for extinctions
      data fmicro/1.e6/

c --- Zero contents of active area of local arrays
      do k=0,nospec
        do j=jbgrid,jegrid
        do i=ibgrid,iegrid
          avg0(i,j,k)=zero
        enddo
        enddo
        do i=1,ndrec
          avd0(i,k)=zero
        enddo
        do i=1,nctrec
          avt0(i,k)=zero
        enddo
      enddo

c --- Transfer the NOSPEC species selected for visibility
      do k=1,nospec
         kuse=0
         if(k.EQ.iso4 .AND. LVSO4) kuse=1
         if(k.EQ.ino2 .AND. LVNO2) kuse=1
         if(k.EQ.inox .AND. LVNO2) kuse=1
         if(k.EQ.ino3 .AND. LVNO3) kuse=1
         if(k.EQ.ipmf .AND. LVPMF) kuse=1
         if(k.EQ.ipmc .AND. LVPMC) kuse=1
         if(k.EQ.ioc .AND. LVOC) kuse=1
         if(k.EQ.iec .AND. LVEC) kuse=1
         if(kuse.EQ.1) then
            do j=jbgrid,jegrid
            do i=ibgrid,iegrid
              avg0(i,j,k)=avg(i,j,k)
            enddo
            enddo
            do i=1,ndrec
              avd0(i,k)=avd(i,k)
            enddo
            do i=1,nctrec
              avt0(i,k)=avt(i,k)
            enddo
         endif
      enddo
  
c --- Set month
      call GRDAY(io1,myr,mjday,imo,iday)

c --- Select dry and RH-augmented background extinction
      bextdry=bextdry2(imo)
      bexthygs=bexthygs2(imo)
      bexthygl=bexthygl2(imo)
      bextsalt=bextsalt2(imo)

      if(mvisbk.EQ.8 .AND. (m8_mode.EQ.2 .OR. m8_mode.EQ.3)) then
c ---    Set relative humidity
         irh=irhmonth(imo)
         xrh=0.01*float(irh)
c ---    Set different F(RH) for small, large, and salt
         call FRH4(xrh,frhs,frhl,frhss)
      elseif(mvisbk.EQ.8 .AND. (m8_mode.EQ.4 .OR. m8_mode.EQ.5)) then
         frhs=rhfsml(imo)
         frhl=rhflrg(imo)
         frhss=rhfsea(imo)
         irh=irhmonth(imo)
         xrh=0.01*float(irh)
      else
         stop 'BEXT_24:  Invalid Method 8 Configuration!'
      endif

c --- Compute background extinction (spatially constant)
      bext0=bextdry+bexthygs*frhs+bexthygl*frhl+bextsalt*frhss

c --- Compute and store modeled extinction coefficients for
c --- each species (those that are not selected remain zero)
      if(LSGRID .AND. LG) then
        do j=jbgrid,jegrid
        do i=ibgrid,iegrid
          av24ecg(i,j)=avg0(i,j,iec)*fmicro*eeec
          av24pcg(i,j)=avg0(i,j,ipmc)*fmicro*eepmc
          av24pfg(i,j)=avg0(i,j,ipmf)*fmicro*eepmf
          av24n2g(i,j)=avg0(i,j,ino2)*fmicro*eeno2
          av24s4g(i,j)=avg0(i,j,iso4)*fmicro*xso4
          call IMPRV06(eeso4s,eeso4l,frhs,frhl,
     &                 bkso4(imo),av24s4g(i,j))
          av24n3g(i,j)=avg0(i,j,ino3)*fmicro*xno3
          call IMPRV06(eeno3s,eeno3l,frhs,frhl,
     &                 bkno3(imo),av24n3g(i,j))
          av24ocg(i,j)=avg0(i,j,ioc)*fmicro
          call IMPRV06(eeocs,eeocl,one,one,bkoc(imo),av24ocg(i,j))
        enddo
        enddo
      endif
      if(ndrec.GT.0 .AND. LD) then
        do i=1,ndrec
          av24ecd(i)=avd0(i,iec)*fmicro*eeec
          av24pcd(i)=avd0(i,ipmc)*fmicro*eepmc
          av24pfd(i)=avd0(i,ipmf)*fmicro*eepmf
          av24n2d(i)=avd0(i,ino2)*fmicro*eeno2
          av24s4d(i)=avd0(i,iso4)*fmicro*xso4
          call IMPRV06(eeso4s,eeso4l,frhs,frhl,
     &                 bkso4(imo),av24s4d(i))
          av24n3d(i)=avd0(i,ino3)*fmicro*xno3
          call IMPRV06(eeno3s,eeno3l,frhs,frhl,
     &                 bkno3(imo),av24n3d(i))
          av24ocd(i)=avd0(i,ioc)*fmicro
          call IMPRV06(eeocs,eeocl,one,one,bkoc(imo),av24ocd(i))
        enddo
      endif
      if(LCTSG .AND. LCT) then
        do i=1,nctrec
          av24ect(i)=avt0(i,iec)*fmicro*eeec
          av24pct(i)=avt0(i,ipmc)*fmicro*eepmc
          av24pft(i)=avt0(i,ipmf)*fmicro*eepmf
          av24n2t(i)=avt0(i,ino2)*fmicro*eeno2
          av24s4t(i)=avt0(i,iso4)*fmicro*xso4
          call IMPRV06(eeso4s,eeso4l,frhs,frhl,
     &                 bkso4(imo),av24s4t(i))
          av24n3t(i)=avt0(i,ino3)*fmicro*xno3
          call IMPRV06(eeno3s,eeno3l,frhs,frhl,
     &                 bkno3(imo),av24n3t(i))
          av24oct(i)=avt0(i,ioc)*fmicro
          call IMPRV06(eeocs,eeocl,one,one,bkoc(imo),av24oct(i))
        enddo
      endif

c --- Compute the light extinction requested (only those components
c --- selected in the control file are non-zero), and store in arrays.
c --- Also transfer related visibility results into receptor arrays.
      if(LSGRID .AND. LG)then
         do j=jbgrid,jegrid
         do i=ibgrid,iegrid
            bsource=av24s4g(i,j)+av24n3g(i,j)+av24ocg(i,j)+
     &              av24ecg(i,j)+av24pcg(i,j)+av24pfg(i,j)+av24n2g(i,j)
            aveg(i,j)=bsource            
            if(LVBK) aveg(i,j)=bsource+bext0            
            avbg(i,j)=bext0
            avhg(i,j)=xrh
            if(bext0.GT.0.0) then
               avpg(i,j)=bsource/bext0
            else
               stop 'BEXT_24:  background extinction = 0!'
            endif
            gbext0(i,j)=bext0
            gfrhs(i,j)=frhs
            gfrhl(i,j)=frhl
            gfrhss(i,j)=frhss
         enddo
         enddo
      endif
      if(ndrec.gt.0 .AND. LD)then
         do i=1,ndrec
            bsource=av24s4d(i)+av24n3d(i)+av24ocd(i)+
     &              av24ecd(i)+av24pcd(i)+av24pfd(i)+av24n2d(i)
            aved(i)=bsource
            if(LVBK) aved(i)=aved(i)+bext0
            avbd(i)=bext0
            avhd(i)=xrh
            if(bext0.GT.0.0) then
               avpd(i)=bsource/bext0
            else
               stop 'BEXT_24:  background extinction = 0!'
            endif
            dbext0(i)=bext0
            dfrhs(i)=frhs
            dfrhl(i)=frhl
            dfrhss(i)=frhss
         enddo
      endif
      if(LCTSG .AND. LCT)then
         do i=1,nctrec
            bsource=av24s4t(i)+av24n3t(i)+av24oct(i)+
     &              av24ect(i)+av24pct(i)+av24pft(i)+av24n2t(i)
            avet(i)=bsource
            if(LVBK) avet(i)=avet(i)+bext0
            avbt(i)=bext0
            avht(i)=xrh
            if(bext0.GT.0.0) then
               avpt(i)=bsource/bext0
            else
               stop 'BEXT_24:  background extinction = 0!'
            endif
            tbext0(i)=bext0
            tfrhs(i)=frhs
            tfrhl(i)=frhl
            tfrhss(i)=frhss
         enddo
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine V24_RUNL(aveg,aved,avet,avbg,avbd,avbt,
     *                    avhg,avhd,avht,avpg,avpd,avpt,
     *                    rveg,rved,rvet,rvbg,rvbd,rvbt,
     *                    rvhg,rvhd,rvht,rvng,rvnd,rvnt,
     *                    rvpg,rvpd,rvpt)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 061120         V24_RUNL
c               D. Strimaitis
c
c --- PURPOSE:  Sum results in the AV__ arrays into the RV__ arrays
c
c --- INPUTS:
c  AVE[G,D,T] - real array - 24-hour extinctions
c  AVB[G,D,T] - real array - 24-hour background extinctions
c  AVH[G,D,T] - real array - 24-hour %RH
c  AVP[G,D,T] - real array - 24-hour source/background extinctions
c
c     Common block /CTRL/
c               ibgrid,jbgrid,iegrid,jegrid,
c               LG,LD,LCT,LCTSG
c     Common block /HEAD/
c               ndrec,nctrec,LSGRID
c
c --- OUTPUT: (these arrays are updated)
c  RVE[G,D,T] - real array - Run-length extinction sums
c  RVB[G,D,T] - real array - Run-length background extinction sums
c  RVH[G,D,T] - real array - Run-length RH fraction sums
c  RVN[G,D,T] - real array - Run-length count of number in sums
c  RVP[G,D,T] - real array - Run-length source/background ext. sums
c
c --- V24_RUNL called by:  main
c --- V24_RUNL calls:      none
c-----------------------------------------------------------------------
      INCLUDE 'params.pst'

      INCLUDE 'ctrl.pst'
      INCLUDE 'head.pst'

c --- Declare input arrays
      real aveg(mxgx,mxgy),aved(mxdrec),avet(mxctrec)
      real avbg(mxgx,mxgy),avbd(mxdrec),avbt(mxctrec)
      real avhg(mxgx,mxgy),avhd(mxdrec),avht(mxctrec)
      real avpg(mxgx,mxgy),avpd(mxdrec),avpt(mxctrec)

c --- Declare updated arrays
      real rveg(mxgx,mxgy),rved(mxdrec),rvet(mxctrec)
      real rvbg(mxgx,mxgy),rvbd(mxdrec),rvbt(mxctrec)
      real rvhg(mxgx,mxgy),rvhd(mxdrec),rvht(mxctrec)
      real rvpg(mxgx,mxgy),rvpd(mxdrec),rvpt(mxctrec)
      real rvng(mxgx,mxgy),rvnd(mxdrec),rvnt(mxctrec)

c --- Local testing
      idb=0

c --- Update stored run-length arrays with current 24-hour results
      if(LSGRID .AND. LG)then
         do i=ibgrid,iegrid
         do j=jbgrid,jegrid
            rveg(i,j)=rveg(i,j)+aveg(i,j)      
            rvbg(i,j)=rvbg(i,j)+avbg(i,j)
            rvhg(i,j)=rvhg(i,j)+avhg(i,j)
            rvpg(i,j)=rvpg(i,j)+avpg(i,j)
            rvng(i,j)=rvng(i,j)+1.0
         enddo
         enddo
      endif
      if(ndrec.gt.0 .AND. LD)then
         do i=1,ndrec
            rved(i)=rved(i)+aved(i)
            rvbd(i)=rvbd(i)+avbd(i)
            rvhd(i)=rvhd(i)+avhd(i)
            rvpd(i)=rvpd(i)+avpd(i)
            rvnd(i)=rvnd(i)+1.0
         enddo
      endif
      if(LCTSG .AND. LCT)then
         do i=1,nctrec
            rvet(i)=rvet(i)+avet(i)
            rvbt(i)=rvbt(i)+avbt(i)
            rvht(i)=rvht(i)+avht(i)
            rvpt(i)=rvpt(i)+avpt(i)
            rvnt(i)=rvnt(i)+1.0
         enddo
      endif

c --- DEBUG
      if(idb.EQ.0) return
      write(*,*)
      write(*,*)'V24_RUNL'
      write(*,*)'LSGRID,LG = ',LSGRID,LG 
      if(LSGRID .AND. LG)then
         i=1
         j=1
         write(*,*)'(i,j)    = ',i,j
         write(*,*)'rveg(i,j)= ',rveg(i,j)
         write(*,*)'rvbg(i,j)= ',rvbg(i,j)
         write(*,*)'rvhg(i,j)= ',rvhg(i,j)
         write(*,*)'rvpg(i,j)= ',rvpg(i,j)
         write(*,*)'rvng(i,j)= ',rvng(i,j)
      endif
      write(*,*)'ndrec,LD  = ',ndrec,LD
      if(ndrec.gt.0 .AND. LD)then
         i=1
         write(*,*)'(i)    = ',i
         write(*,*)'rved(i)= ',rved(i)
         write(*,*)'rvbd(i)= ',rvbd(i)
         write(*,*)'rvhd(i)= ',rvhd(i)
         write(*,*)'rvpd(i)= ',rvpd(i)
         write(*,*)'rvnd(i)= ',rvnd(i)
      endif
      write(*,*)'LCTSG,LCT = ',LCTSG,LCT
      if(LCTSG .AND. LCT)then
         i=1
         write(*,*)'(i)    = ',i
         write(*,*)'rvet(i)= ',rvet(i)
         write(*,*)'rvbt(i)= ',rvbt(i)
         write(*,*)'rvht(i)= ',rvht(i)
         write(*,*)'rvpt(i)= ',rvpt(i)
         write(*,*)'rvnt(i)= ',rvnt(i)
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine getcalm
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 080724           GETCALM
c ---           D. Strimaitis, TRC
c
c  PURPOSE:     Reads one record from single-point met data file for
c               CALM processing of output from plume-type model, and
c               determines if the corresponding AERMOD concentration
c               should be included in averages.  If met data show a
c               calm or missing data, the flag for "calm" (icalmflag)
c               is set to 1 (the flag is ZERO if data are valid)
c
c --- UPDATES:
c     V6.144(070130) to V6.221(080724)
c               (DGS) Check for missing due to bad upper-air data by
c                     testing for positive heat flux and negative
c                     convective mixing height, and keep track of 
c                     number of missings found.
c
c  ARGUMENTS:
c
c     PASSED:
c      PARAMS parameter in7, io6
c      common /CALMPRO/ met1fmt
c      common /CONC/ myrb,mjdayb,mhrb,msecb,myre,mjdaye,mhre,msece
c
c   RETURNED:   
c      common /CALMPRO/ icalmflag,ncalmplm,nmissplm
c
c  CALLING ROUTINES:    (main)
c
c  EXTERNAL ROUTINES:   INCR, YR4
c-----------------------------------------------------------------------
c
      INCLUDE 'params.pst'
      INCLUDE 'calmpro.pst'
      INCLUDE 'conc.pst'

      character*132 aline132
      logical ldb

c --- Local switch for debug output
      ldb=.FALSE.

c --- Form target date-time variables
      mbyjh=myrb*100000+mjdayb*100+mhrb
      meyjh=myre*100000+mjdaye*100+mhre

      if(LDB) then
         write(io6,*)'GETCALM: myrb,mjdayb,mhrb = ',myrb,mjdayb,mhrb
         write(io6,*)'GETCALM: myre,mjdaye,mhre = ',myre,mjdaye,mhre
      endif

c --- AERMOD/AERMET SURFACE file is currently the only type supported
      if(met1fmt.EQ.1) then
c --------------------------------------------------------------------
c  --    Read one record of met data from standard AERMET SURFACE file
c --------------------------------------------------------------------
c ---    Read entire line as character variable
4        read(in7,'(a132)',end=999) aline132

c ---    Extract fields up to wind speed/dir (internal read)
         read(aline132,*) iey,iem,ied,iejul,ieh,
     &                    hflx,ustr,wstr,vptgup,ziconv,zimech,
     &                    xmon,z0m,brat,alb,wsa,wda

      if(LDB) then
         write(io6,*)'GETCALM: read iey,iem,ied,iejul,ieh = ',
     &                iey,iem,ied,iejul,ieh
      endif

c ---    Condition the timestamp
c ---    Enforce YYYY format for year
         call YR4(io6,iey,ierrb)
         if(ierrb.NE.0) stop 'Halted in GETCALM (see LIST file)'
c ---    SURFACE.DAT uses a 1-24 clock (hour ending).  Get
c ---    time at the beginning of the hour (subtract 1 hour)
         ibh=ieh-1
         iby=iey
         ibjul=iejul
c ---    Now increment time 1 hour to get the correct end time
         ieh=ibh
         call INCR(io6,iey,iejul,ieh,1)
c ---    Form date-time variables
         ibyjh=iby*100000+ibjul*100+ibh
         ieyjh=iey*100000+iejul*100+ieh

      if(LDB) then
         write(io6,*)'GETCALM: iby,ibjul,ibh = ',iby,ibjul,ibh
         write(io6,*)'GETCALM: iey,iejul,ieh = ',iey,iejul,ieh
      endif

c ---    Seconds must match for this hourly dataset
         if(msecb.NE.0 .OR. msece.NE.0) then
            write(io6,*)'GETCALM: Met data are hourly so concentration'
            write(io6,*)'         data must be hourly as well'
            write(io6,*)'         Found non-zero seconds!'
            write(io6,*)' MSECB, MSECE = ',msecb,msece
            stop 'Halted in GETCALM (see LIST file)'
         endif

c ---    Check time period
         if(ibyjh.GE.meyjh) then
c ---       Period read comes after target period!
            write(io6,*)'GETCALM: Current period not found in Met data'
            write(io6,*)'         Expected begin,end = ',mbyjh,meyjh
            write(io6,*)'           Found  begin,end = ',ibyjh,ieyjh
            stop 'Halted in GETCALM (see LIST file)'
         elseif(ieyjh.LE.mbyjh) then
c ---       Period read comes before target period; Get another record
            goto 4
         endif

c ---    Set the calm/missing flag and return
         icalmflag=0
         if(wsa.LE.0.0 .AND. wda.LE.0.0) then
            icalmflag=1
            ncalmplm=ncalmplm+1
         elseif(ustr.LT.-1.0) then
            icalmflag=1
            nmissplm=nmissplm+1
         elseif(hflx.GE.0.0 .AND. ziconv.LT.0.0) then
            icalmflag=1
            nmissplm=nmissplm+1
         endif
      endif

      return

c --- EOF encountered
999   write(io6,*)'GETCALM:  EOF encountered in met file while'
      write(io6,*)'          searching for period = ',mbyjh,meyjh
      stop 'Halted in GETCALM (see LIST file)'

      end
c-----------------------------------------------------------------------
      subroutine ttest(idathrb,ibs,idathre,ies)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 070130             TTEST
c ---           D. Strimaitis, TRC
c
c  PURPOSE:     Compares a timestamp with that read from file being
c               processed
c
c  ARGUMENTS:
c
c     PASSED:
c     idathrb - integer    - Starting YYYYJJJHH of timestamp
c         ibs - integer    - Starting SSSS of timestamp
c     idathre - integer    - Ending YYYYJJJHH of timestamp
c         ies - integer    - Ending SSSS of timestamp
c
c      PARAMS parameter io1
c      common /CONC/ myrb,mjdayb,mhrb,msecb,myre,mjdaye,mhre,msece
c      common /FILNAM/ moddat
c
c   RETURNED:   
c      none
c
c  CALLING ROUTINES:    (main)
c
c  EXTERNAL ROUTINES:   none
c-----------------------------------------------------------------------
c
      INCLUDE 'params.pst'
      INCLUDE 'conc.pst'
      INCLUDE 'filnam.pst'

c --- Timestamp just read is in /CONC/
      mdathrb=myrb*100000+mjdayb*100+mhrb
      mdathre=myre*100000+mjdaye*100+mhre

      if(idathrb.NE.mdathrb .OR. ibs.NE.msecb .OR.
     &   idathre.NE.mdathre .OR. ies.NE.msece ) then
         write(io1,*)
         write(io1,*)'ERROR:  Expected timestamp not found in file'
         write(io1,*) moddat
         write(io1,*)
         write(io1,*)'Expected Period Start = ',idathrb,ibs
         write(io1,*)'Expected Period End   = ',idathre,ies
         write(io1,*)'Found Period Start    = ',mdathrb,msecb
         write(io1,*)'Found Period End      = ',mdathre,msece
         stop 'ERROR in data timestamp -- See list file'
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine tup(io,idathrb,isecb,idathre,isece,nsec)
c-----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 070130               TUP
c ---           D. Strimaitis, TRC
c
c  PURPOSE:     Updates timestamp for a period by a specified step
c
c  ARGUMENTS:
c
c     PASSED:
c          io - integer    - File unit number for listfile
c     idathrb - integer    - YYYYJJJHH of starting timestamp
c       isecb - integer    - SSSS of starting timestamp
c     idathre - integer    - YYYYJJJHH of ending timestamp
c       isece - integer    - SSSS of ending timestamp
c        nsec - integer    - Increment (seconds)
c
c   RETURNED:   
c      idathr - integer    - YYYYJJJHH of timestamp (updated)
c        isec - integer    - SSSS of timestamp (updated)
c
c  CALLING ROUTINES:    (main)
c
c  EXTERNAL ROUTINES:   DEDAT, INCRS
c-----------------------------------------------------------------------

c --- Set new start time to old end time
      idathrb=idathre
      isecb=isece

c --- Break out the individual date and time components
c --- of end time
      call DEDAT(idathre,iyre,ijde,ihre)

c --- Increment end time by NSEC seconds
      call INCRS(io,iyre,ijde,ihre,isece,nsec)
      idathre=iyre*100000+ijde*100+ihre

      return
      end
c----------------------------------------------------------------------
      subroutine makeno2(cnox,cno2)
c----------------------------------------------------------------------
c
c --- CALPOST   Version: 6.221          Level: 070712          MAKENO2
c               D. Strimaitis
c
c --- PURPOSE:  Compute NO2 concentration from modelled NOX
c               concentration using look-up table
c
c  ARGUMENTS:
c --- INPUTS:
c              cnox    NOX concentration (g/m3)                      [R]
c
c     common /CTRL/    no2calc
c     common /NO2NOX/  nbin, bincnox(nbin), binfno2(nbin)
c                      rno2nox 
c
c --- OUTPUT:
c              cno2   NO2 concentration (g/m3)                       [R]
c
c-----------------------------------------------------------------------
c --- Include parameters
      include 'params.pst'
      include 'ctrl.pst'
      include 'no2nox.pst'

      ierr=0
      cno2=0.0

      if(no2calc.EQ.1) then
c ---    Use constant NO2/NOx ratio
         f=rno2nox
         cno2=f*cnox

      elseif(no2calc.EQ.2) then
c ---    Use tabulated NO2/NOx ratios
c ---    Obtain fraction of NOX that is NO2 from the table in /NO2NOX/
c ---    Find upper end of interpolation range
         ktop=nbin+1
         do n=nbin,1,-1
            if(cnox.LE.bincnox(n)) ktop=n
         enddo
c ---    Compute fraction
         f=binfno2(MIN(ktop,nbin))
         if(ktop.GT.1 .AND. ktop.LE.nbin) then
            f=binfno2(ktop-1)+(binfno2(ktop)-binfno2(ktop-1))*
     &        (cnox-bincnox(ktop-1))/(bincnox(ktop)-bincnox(ktop-1))
         endif
c ---    Compute NO2
         cno2=f*cnox

      else
c ---    ERROR
         write(io6,*)'ERROR in MAKENO2: Invalid NO2CALC'
         write(io6,*)'   Expected NO2CALC= 1 or 2'
         write(io6,*)'   Found    NO2CALC= ',no2calc
         ierr=1
      endif

c --- QA
      if(cno2.LT.0.0 .OR. cnox.LT.0.0) then
         write(io6,*)'ERROR in MAKENO2: Negative concentration'
         write(io6,*)'   NO2, NOX = ',cno2,cnox
         ierr=1
      endif
      if(f.LT.0.0 .OR. f.GT.1.0) then
         write(io6,*)'ERROR in MAKENO2: Invalid NO2/NOx ratio'
         write(io6,*)'   Expected NO2/NOx between 0.0 and 1.0'
         write(io6,*)'   Found    NO2/NOx= ',f
         ierr=1
      endif

      if(ierr.NE.0) stop 'ERROR in MAKENO2 --- see list file'

      return
      end
