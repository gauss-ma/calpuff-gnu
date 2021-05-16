#!/bin/bash
source namelist.sh
# ======================================== #
# Armar archivo de control CALMET.INP
# ---------------------------------------- #

if [[ ! -f calmet.exe ]];then
	ln -sf ../exe/calmet.exe .
fi
#========================================================================
#01- TIME PARAMETERS
#	Set CALMET temporal and model run parameters
#	This function validates and writes CALMET parameters for the model starting and ending times, the time zone, and model timings.

ibtz=-3		    #base time zone
#nsecdt=$((60*60))  #time step in [secs]

#========================================================================
#02 - GRID
#	Set CALMET parameters for the map projection, datum, and grid definitions
#	This function validates and writes CALMET parameters for the map projection, datum, and grid definitions to the working CALMET.INP file.

#========================================================================
#03- OUTPUT OPTS 
#	Set CALMET parameters for the output options
# 	This function validates and writes CALMET parameters for the output options to the working CALMET.INP file.

iuvout="1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0"  #iuvout specification of which layers of U,V wind component to print; vector must be the same length as the number of levels (nz).
iwout="1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0"   #iwout specification of which layers of the W wind component to print; vector must be the same length as the number of levels (nz).
itout="1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0"   #itout specification of which levels of the 3D temperature field to print; vector must be the same length as the number of levels (nz).

#========================================================================
#04- METEO 
#	Set CALMET parameters for the meteorological data options
#	This function validates and writes CALMET parameters for the meteorological data options to the working CALMET.INP file.

#========================================================================
#05- WIND FIELDS 
#	Set CALMET parameters for wind field options and other options
# 	This function validates and writes CALMET parameters for wind field options and parameters to the working CALMET.INP file.

#========================================================================
#06- OTHER PARAMTERS 
# Set CALMET parameters for mixing height, temperature, and precipitation
# This function validates and writes CALMET parameters for wind field options and parameters to the working CALMET.INP file.

#========================================================================
#07- SURF/PRECIP/UPPER PARAMS 
	#Set CALMET surface station, precipitation station, and upper air parameters
	#This function validates and writes CALMET parameters for surface stations, precipitation stations, and upper air soundings to the working CALMET.INP file.

	sfc_x=$(bc <<<"x=$sfc_x;scale=3;x/1000")
	sfc_y=$(bc <<<"x=$sfc_y;scale=3;x/1000")
	sfc_z=$(bc <<<"x=$sfc_z;x+20.0")     # la altura del anemometro está (o debería) gralmente a 20m
	up_x=$(bc <<<"x=$up_x;scale=3;x/1000")
	up_y=$(bc <<<"x=$up_y;scale=3;x/1000")

ss1=$(printf '%4s,%7.0f,%7.3f,%7.3f,%7.0f,%7.0f' $sfc_name $sfc_id $sfc_x $sfc_y $utmzn $sfc_z)
us1=$(printf '%4s,%6.0f,%7.3f,%7.3f,%7.0f' $up_name $up_id $up_x $up_y $utmzn)
ps1=$(printf '%4s, %7.0f, %7.3f,%7.3f' $sfc_name $sfc_id $sfc_x $sfc_y)


#========================================================================
#Reemplazar en template y guardar en "calmet.inp"

cat << EOF > calmet.inp

      CALMET.INP      2.1             Hour Start and End Times with Seconds
      
      0 -- Input and Output File Names
      
      Input files
      ! GEODAT =geo.dat   !               #geodat the name of the intended GEO.DAT file.
      ! SRFDAT =sfc.dat   !               #srfdat the name of the intended SURF.DAT file.
      * CLDDAT =cloud.dat *               #clddat the name of the intended CLOUD.DAT file.
      ! PRCDAT =precip.dat!               #prcdat the name of the intended PRECIP.DAT file.
      * WTDAT  =wt.dat    *               #wtdat the name of the intended WT.DAT file.
                           
      Output files
      * METLST = *
      * METDAT = *
      * PACDAT = *
      
      ! LCFILES =${lcfiles} !
      
      Number of upper air stations and overwater met stations
      ! NUSTA  = 1 ! # nusta number of uperair stations used
      ! NOWSTA = 0 ! # nowsta number of overwater stations used;
      
      Number of prognostic and IGF-CALMET files
      * NM3D =  *
      * NIGF =  *
      ! END ! // end SECTION 0 - Part A
      
      Upper air files (one per station)
      ! UPDAT = up.dat  !		#upper data file
      ! END ! // end SECTION 0 - Part B
      
      Overwater station files (one per station)
      * SEADAT =   *
      ! END !  // end SECTION 0 - Part C
      
      
      MM4/MM5/3D.DAT files (consecutive or overlapping)
      * M3DDAT = *
      * END *  // end SECTION 0 - Part D
      
      IGF-CALMET.DAT files (consecutive or overlapping)
      * IGFDAT = *
      * END *  // end SECTION 0 - Part E
      
      Other CALMET modeling files
      * DIADAT = *
      * PRGDAT = *
      * TSTPRT = *
      * TSTOUT = *
      * TSTKIN = *
      * TSTFRD = *
      * TSTSLP = *
      * DCSTGD = *
      * END * // end SECTION 0
      -------------------------------------------------------------------------------
      
      1 -- Temporal Parameters
      
      Starting date/time
      ! IBYR  =${ibyr} !
      ! IBMO  =${ibmo} !
      ! IBDY  =${ibdy} !
      ! IBHR  =${ibhr} !
      
      Ending date/time
      * IEYR  =${ieyr} *
      * IEMO  =${iemo} *
      * IEDY  =${iedy} *
      * IEHR  =${iehr} *
      
      UTC time zone
      ! IBTZ =${ibtz} !
      
      Run parameters			
      ! IRLG    = ${total_horas} !  #duración de la corrida (en horas)        
      ! IRTYPE  = 1   !             #irtype   run type, (0: wind fields only; 1 computes U*, W*, L, Zi, etc.).
      ! LCALGRD = T   !             #lcalgrd  Compute special data fields required by CALGRID? (F/T)
      ! ITEST   = 1   !             #itest    Stop after setup phase? (0:no; 1:yes)  (checking for correctness in model inputs and associated files)
      ! MREG    = 1   !             #mreg     Use  EPA regulatory options? (0:no; 1:yes)    
      ! END ! // end SECTION 1          
      -------------------------------------------------------------------------------
      
      2 -- Map projection, datum, grid definitions
      
      ! PMAP   =${pmap} !
      * FEAST  =  *
      * FNORTH =  *
      ! IUTMZN =${utmzn} !
      ! UTMHEM =${utmhem} !
      * RLAT0  =   *
      * RLON0  =   *
      * XLAT1  =   *
      * XLAT2  =   *
      
      ! DATUM =${datum} !
      
      ! NX      =${nx} !
      ! NY      =${ny} !
      ! DGRIDKM =${dgridkm} !
      ! XORIGKM =${xorigkm} !
      ! YORIGKM =${yorigkm} !
      
      ! NZ    =${nz} !
      ! ZFACE =${zlevs[@]} !
        
      ! END ! // end SECTION 2
      
      -------------------------------------------------------------------------------
      
      3 -- Output Options
       
      ! LSAVE  =  T! #lsave save meteorological fields in an unformatted output file.
      ! IFORMO =  1! #iformo type of unformatted output file: (1) CALPUFF/CALGRID type file [CALMET.DAT], (2) MESOPUFF-II type file [PACOUT.DAT].
      ! LPRINT =  F! #lprint whether to print meteorological fields in the iuvout, iwout, and itout options.
      ! IPRINF =  1! #iprinf print interval in hours.

      ! IUVOUT    =${iuvout[@]}!
      ! IWOUT     =${iwout[@]} !
      ! ITOUT     =${itout[@]} !
 
     ! STABILITY = T ! #stability whether to print the PGT stability class (will only print if lprint is TRUE).
      ! USTAR     = T ! #ustar whether to print the friction velocity (will only print if lprint is TRUE).
      ! MONIN     = T ! #monin whether to print the Monin-Obukhov length (will only print if lprint is TRUE).
      ! MIXHT     = T ! #mixht whether to print the mixing height (will only print if lprint is TRUE).
      ! WSTAR     = T ! #wstar whether to print the convective velocity scale (will only print if lprint is TRUE).
      ! PRECIP    = T ! #precip whether to print the precipitation rate (will only print if lprint is TRUE).
      ! SENSHEAT  = T ! #sensheat whether to print the sensible heat flux (will only print if lprint is TRUE).
      ! CONVZI    = T ! #convzi whether to print the convective mixing height (will only print if lprint is TRUE).
      
      Testing and debug print options for micrometeorological module
      
      ! LDB    = T   ! #ldb whether to print input meteorological data and internal variables.
      ! NN1    = 1   ! #nn1 if ldb is T, the first time step for which debug data are printed.
      ! NN2    = 24  ! #nn2 if ldb is T, the last time step for which debug data are printed.
      ! LDBCST = F   ! #ldbcst whether to print distance to land internal variables (output will be a .GRD file DCST.GRD).
      ! IOUTD  = F   ! #ioutd whether to print the test/debug wind fields to disk files.
      ! NZPRN2 = F   ! #nzprn2 number of levels, starting at the surface, to print.
      ! IPR0   = F   ! #ipr0 print the interpolated wind components?
      ! IPR1   = F   ! #ipr1 print the terrain-adjusted surface wind components?
      ! IPR2   = F   ! #ipr2 print the smoothed wind components and the initial divergence fields?
      ! IPR3   = F   ! #ipr3 print the final wind speed and direction fields?
      ! IPR4   = F   ! #ipr4 print the final divergence fields?
      ! IPR5   = F   ! #ipr5 print the winds after kinematic effects are added?
      ! IPR6   = F   ! #ipr6 print the winds after the Froude number adjustment is made?
      ! IPR7   = F   ! #ipr7 print the winds after slope flows are added?
      ! IPR8   = F   ! #ipr8 print the final wind field components?
      
      ! END ! // end SECTION 3
      
      -------------------------------------------------------------------------------
      4 -- Meteorological data options                       
      Combinations of observations and prognostic data	    
      ! NOOBS = 0 ! #"no observation mode"                             
                                                            
      Numbers of surface met and precipitation stations
      ! NSSTA = 1!   # nssta number of surface meteorological stations used.
      ! NPSTA = 1!   # npsta number of precipitation stations used; a value of -1 is to be provided when MM5/3D precip data is used.
                         
      Cloud data options
      ! ICLOUD = 0 ! # cloud data to use,(0: cloud information is not to be used; 1:cloud.dat; 2: gridded cloud.dat; 3: from prognostic fields).
      
      File formats
      ! IFORMS =2! # surface meteorological data file format: 1 - unformatted (e.g., SMERGE output); 2 - formatted (as free-formatted user input).
      ! IFORMP =2! # precipitation data file format: 1 - unformatted (e.g., PMERGE output); 2 - formatted (free-formatted user input).
      ! IFORMC =2! # cloud data file format: 1 - unformatted as CALMET unformatted output; 2 - formatted (free-formatted CALMET output or user input).
      
      ! END ! // end SECTION 4
      
      -------------------------------------------------------------------------------
      								
      5 -- Wind field options and parameters
      
      ! IWFCOD  = 1   ! # (0) objective analysis only, or (1) diagnostic wind module.
      ! IFRADJ  = 1   ! # compute Froude number adjustment effects.
      ! IKINE   = 0   ! # compute kinematic effects.
      ! IOBR    = 0   ! # use O'Brien procedure for adjustment of the vertical velocity.
      ! ISLOPE  = 1   ! # compute slope flow effects.
      ! IEXTRP  = -4  ! # extrapolate surface wind observations to upper layers.
      ! ICALM   = 0   ! # extrapolate surface winds even if calm?
      ! RMIN2   = -1.0! # min distance from upper air station to surface station for which extrapolation of winds at surface station will be allowed.
      ! IPROG   = 0   ! # iprog use gridded prognostic wind field model output fields as input to the diagnostic wind field model.
      ! ISTEPPG = 1   ! # isteppg timestep in hours of the prognostic model input data.
      ! IGFMET  = 0   ! # igfmet use coarse CALMET fields as initial guess fields.
      ! LVARY   = T   ! # lvary use varying radius of influence.
      ! RMAX1   = 5.0 ! # maximum radius of influence over land in the surface layer.
      ! RMAX2   =10.0 ! # maximum radius of influence over land aloft.
      ! RMAX3   =20.0 ! # maximum radius of influence over water.
      ! RMIN    = 0.1 ! # min radius of influence used in the wind field interpolation.
      ! TERRAD  = 5.0 ! # terrad radius of influence of terrain features.
      ! R1      = 2.0 ! # relative weighting of the first guess field and observations in the surface layer.
      ! R2      = 1.0 ! # relative weighting of the first guess field and observations in layers aloft.
      ! RPROG   = 0.0 ! # relative weighting parameter of the prognostic wind field data.
      ! DIVLIM  =5E-6 ! # maximum acceptable divergence in the divergence minimization procedure.
      ! NITER   = 50  ! # maximum number of iterations in the divergence min. procedure.
      ! NINTR2  = 99  ! # maximum number of stations used in each layer for the interpolation of data to a grid point.
      ! CRITFN  = 1.0 ! # the critical Froude number.
      ! ALPHA   = 0.1 ! # an empirical factor controlling the influence of kinematic effects.
      ! NBAR    = 0   ! # number of barriers to interpolation of the wind fields.
      ! KBAR    = 10  ! # level up to which barriers apply (must be value in the range of 1 to NZ).
      ! XBBAR   = 0   ! # x coordinates for the beginning of each barrier.
      ! YBBAR   = 0   ! # y coordinates for the beginning of each barrier.
      ! XEBAR   = 0   ! # x coordinates for the ending of each barrier.
      ! YEBAR   = 0   ! # y coordinates for the ending of each barrier.
      ! IDIOPT1 = 0   ! # method for compute sfce temperatures: (0) compute internally from hourly sfce obs (1) read preprocessed values from (DIAG.DAT).
      ! ISURFT  = 1   ! # surface meteorological station to use for the surface temperature (must be value in the range of 1 to nssta).
      ! IDIOPT2 = 0   ! # method for compute domain-averaged temperature lapse rate: (0) compute internally from twice-daily upper air observ (1) read from DIAG.DAT
      ! IUPT    = 1   ! # upper air station to use for the domain-scale lapse rate (must be a value in the range of 1 to NUSTA).
      ! ZUPT    = 200 ! # Depth in meters through which the domain-scale lapse rate is computed.
      ! IDIOPT3 = 0   ! # method for computation of domain-averaged wind components: (0) compute internally from twice-daily upper air observ (1) read from DIAG.DAT.
      ! IUPWND  = -1  ! # upper air station to use for the domain-scale winds (must be a value in the range of -1 to NUSTA).
      ! ZUPWND  = 1,10! # bottom and top of layer through which the domain-scale winds are computed.
      ! IDIOPT4 = 0   ! # selection of observed surface wind components for wind field module: (0) SURF.DAT (1) DIAG.DAT.
      ! IDIOPT5 = 0   ! # selection of observed upper air wind components for wind field module: (0) UP.DAT (1) DIAG.DAT.
      ! LLBREZE = F   ! # use lake breeze module?
      ! NBOX    = 0   ! # number of lake breeze regions.
      ! XG1     = 0   ! #x direction grid line 1 defining the region of interest for the lake breeze module.
      ! XG2     = 0   ! #x direction grid line 2 defining the region of interest for the lake breeze module.
      ! YG1     = 0   ! #y direction grid line 1 defining the region of interest for the lake breeze module.
      ! YG2     = 0   ! #y direction grid line 2 defining the region of interest for the lake breeze module.
      ! XBCST   = 0   ! #beginning x point (in kilometers) defining the coastline (straight line).
      ! YBCST   = 0   ! #beginning y point (in kilometers) defining the coastline (straight line).
      ! XECST   = 0   ! #ending x point (in kilometers) defining the coastline (straight line).
      ! YECST   = 0   ! #ending y point (in kilometers) defining the coastline (straight line).
      ! NLB     = 0   ! #combined number of meteorological and upper air stations in the region.
      ! METBXID = 0   ! #station identifiers for the region; include surface stations first, then upper air stations.
                                                
      ! BIAS    = -1, -1, -0.5, -0.3, 0, 1, 1, 1, 1, 1, 1, 1! #layer-dependent biases modifying the weights of surface and upper air stations
      ! NSMTH   = 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 !        #number of passes in the smoothing procedure.
      ! FEXTR2  = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0!         #multiplicative scaling factors for extrapolation of surface observations to upper layers.
     ! END ! // end SECTION 5
      -------------------------------------------------------------------------------
      
      6 -- Mixing height, temperature, and precipitation parameters
      
      Empirical mixing height constants	      
      ! CONSTB  =${constb} !                  constb=1.41     #constb empirical mixing height constant for the neutral, mechanical equation.
      ! CONSTE  =${conste} !                  conste=0.15     #conste empirical mixing height constant for the convective mixing height equation.
      ! CONSTN  =${constn} !                  constn=2400     #constn empirical mixing height constant for the stable mixing height equation.
      ! CONSTW  =${constw} !                  constw=0.16     #constw empirical mixing height constant for the overwater mixing height equation.
      ! FCORIOL =${fcoriol} !                 fcoriol=1E-4     #fcoriol absolute value of the Coriolis parameter.                                  
                                                
      Spatial averaging of mixing heights       
      ! IAVEZI =${iavezi} !                    iavezi=1        #iavezi conduct spatial averaging of mixing heights?                                
      ! MNMDAV =${mnmdav} !                    mnmdav=10       #mnmdav maximum search radius (in grid cell units) for averaging of mixing heights.
      ! HAFANG =${hafang} !                    hafang=30       #hafang half-angle of upwind looking cone for averaging of mixing heights.
      ! ILEVZI =${ilevzi} !                    ilevzi=1        #ilevzi layer of winds used in upwind averaging of mixing heights.
      
      Convective mixing height options	    
      ! IMIXH   = -1   ! #method for convective mixing height:  !DEBE SER -1 PARA mreg=1                                                           
      ! THRESHL = 0.0  ! #threshold buoyancy flux required to sustain convective mixing height growth overland. DEBE SER 0.0 SI mreg=1             
      ! THRESHW = 0.05 ! #threshold buoyancy flux required to sustain convective mixing height growth overwater.                                   
      ! ITWPROG = 0    ! #option for overwater lapse rates used in convective mixing height growth:                                                
      ! ILUOC3D = 16   ! #land use category for ocean in 3D.DAT datasets.                                                                  
                                    
      Other mixing height variables 
      ! DPTMIN = 0.001  !  #min potential temp lapse rate in the stable layer above the current convective mixing height                  
      ! DZZI   = 200    !  #depth of layer (in meters) above current convective mixing height through which lapse rate is computed.       
      ! ZIMIN  =  50    !  #minimum overland mixing height (in meters).                                                                   
      ! ZIMAX  =3000    !  #maximum overland mixing height (in meters).                                                                                               
      ! ZIMINW =  50    !  #minimum overwater mixing height (in meters).                                                                                              
      ! ZIMAXW =3000    !  #maximum overwater mixing height (in meters).                                                                                              
                                                                
      Overwater surface fluxes methodology and parameters       
                                                                
      ! ICOARE =0 ! #method for computation of overwater surface fluxes: (!) DEBE SER 0 si mreg=1                                         
      ! DSHELF =0 ! #coastal/shallow water length scale (in kilometers) for modified z0 in shallow water.                                 
      ! IWARM  =0 ! #COARE warm layer computation: (0) off, (1) on.                                                                       
      ! ICOOL  =0 ! #COARE cool skin layer computation: (0) off, (1) on.                                                                  
                                                                
      Temperature parameters                                    
      ! ITPROG =  0    !  #selection of temperature data from observations or prognostic data                                                  
      ! IRAD   =  1    !  #interpolation type: (1) 1/R, (2) 1/R^2.                                                                            
      ! TRADKM =  500  !  #radius of influence (in kilometers) for temperature interpolation.                                                 
      ! NUMTS  =  5    !  #maximum number of stations to include in temperature interpolation.                                                
      ! IAVET  =  1    !  #conduct spatial averaging of temperatures?
      ! TGDEFB =-0.0098!  #default temperature gradient (in units of K/m) below the mixing height over water.
      ! TGDEFA =-0.0045!  #default temperature gradient (in units of K/m) above the mixing height over water.
      ! JWAT1  =  55   !  #beginning land use category value for temperature interpolation over water.
      ! JWAT2  =  55   !  #ending land use category value for temperature interpolation over water.
      
      Precipitation interpolation parameters
      ! NFLAGP =   2   !  #method of interpolation for precipitation: (1) 1/R, (2) 1/R^2, (3) exp/R^2.
      ! SIGMAP =  100  !  #radius of influence (in kilometers) for interpolation of precipitation.
      ! CUTP   =  0.01 !  #minimum precipitation rate cutoff (in units of mm/h).
      
      ! END ! // end SECTION 6
      -------------------------------------------------------------------------------
      
ES RE IMPORTANTE QUE EL NOMBRE DE LAS VARIABLES EMPIECEN EN LA POSICION 3     
  7 -- Surface meteorological station parameters
  SS1=${ss1} 
  8 --
  US1=${us1} 
  9 --
  PS1=${ps1} 
      -------------------------------------------------------------------------------




EOF

