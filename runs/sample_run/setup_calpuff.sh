#!/bin/bash

source namelist.sh

if [[ ! -f calpuff.exe ]]
then
	ln -s ../exe/calpuff.exe .
fi


cat << EOF > calpuff.inp

 CALPUFF.INP     2.0             File version record

#----------------------------------------------------------------------------------------------------
 0 -- Input and Output File Names
 
 Input files
 ! NMETDAT = 1           ! num meteo files
 ! METDAT  = calmet.dat  ! meteo file
 * RSTARTB =  *		   restart file
 
 Emission files (aribatarily-varying)
 * PTDAT  =  *	point-srcs
 * VOLDAT =  *	volume-srcs
 * ARDAT  =  *	bouyant-area-src
 * LNDAT  =  *	line-srcs
 
 Others
 * VDDAT   = *
 * CHEMDAT = *
 
 Output files
 ! PUFLST  =  calpuff.lst   !
 ! CONDAT  =  conc.dat      !
 * DFDAT   =  dflx.dat      *
 * WFDAT   =  wflx.dat      *
 * VISDAT  =  visb.dat      *
 * RSTARTE =  restarte.dat  *
 * DEBUG   =  debug.dat     *
 * FLXDAT  =  massflx.dat   *
 * BALDAT  =  massbald.dat  *
 
 Pasar todo a lowercase:
 ! LCFILES = T!
 
 ! END !
 --------------------------------------------------------------------------------
 1 -- General run control parameters
 
 	! METRUN =0 !
 
 	Starting date/time
 	! IBYR  = ${ibyr}  !
 	! IBMO  = ${ibmo}  !
 	! IBDY  = ${ibdy}  !
 	! IBHR  = ${ibhr}  !
 
	! XBTZ  = ${stat_time_offset} !

 	Duración de la corrida [hrs]
 	! IRLG =${total_horas}  !
 	
 	Number of chemical species
 	! NSPEC =1 !
 	
 	Number of chemical species to be emitted
 	! NSE =1 !
 	
 	Run parameters
 	! ITEST    =2    !	1:stops after setup, 2:continue exectuion 
 	! MRESTART =0    !	
 	! NRESPD   =0    !

 	! METFM    =1    !	met. format (1:calmet)
 	! MPRFFM   =1    !

 	! AVET     =60.0 !
 	! PGTIME   =60.0 !
 
 ! END !
 
 -------------------------------------------------------------------------------
 2 -- Technical options
 
 	! MREG     =1 !

 	! MGAUSS   =1 !
 	! MCTADJ   =3 !
 	! MTRANS   =1 !
 	! MTIP     =1 !
 	
 	! MCHEM    =0  !
 	! MWET     =1  !
 	! MDRY     =1  !
 	! MDISP    =3  !
 	! MROUGH   =0  !
 	! MPARTL   =1  !

 	! MCTSG    =0 !
 	! MSLUG    =0 !
 
 ! END ! 
 -------------------------------------------------------------------------------
 3 -- Species list
 
 	Modelled species
 	! CSPEC = CO! ! END !
 	
 	Species Name   Modelled,  Emitted,    DDep, Number
 	! CO    =         1   ,     1    ,     0  ,   0   !
 
 ! END ! 
 -------------------------------------------------------------------------------
 4 -- Grid control parameters
 
      Meteorological grid
      ! NX      =${nx} !
      ! NY      =${ny} !
      ! NZ      =${nz} !
      ! DGRIDKM =${dgridkm} !
      ! XORIGKM =${xorigkm} !
      ! YORIGKM =${yorigkm} !
      ! ZFACE = ${zlevs[@]} !

      ! IUTMZN =${utmzn} !
      ! UTMHEM =${utmhem}!
      ! PMAP   =${pmap}  !
      * FEAST  = *
      * FNORTH = *

      ! DATUM =${datum}  !
      
      Computational Grid
      *XLAT  = * 
      *XLONG = *
      ! IBCOMP =1   ! X index of LL corner
      ! JBCOMP =1   ! Y index of LL corner
      ! IECOMP =$nx ! X index of UR corner
      ! JECOMP =$ny ! Y index of UR corner
      
      Sampling Grid
      ! LSAMP =T      !
      ! IBSAMP =1     ! X index of LL corner
      ! JBSAMP =1     ! Y index of LL corner
      ! IESAMP =$nx   ! X index of UR corner
      ! JESAMP =$ny   ! Y index of UR corner
      
      Nesting factor of the sampling grid
      ! MESHDN =1 !
 
 ! END ! 
 
 -------------------------------------------------------------------------------
 5 -- Output Options
 
 	! ICON =1 !
 	! IDRY =1 !
 	! IWET =1 !
 	! IVIS =1 !
 	! IT2D =0 !
 	! IRHO =0 !
 	
 	! LCOMPRS =F !
 	
 	! IQAPLOT =T !
 	* IPFTRAK =0 *
 	! IMFLX   =F !
 	! IMBAL   =F !
 	* INRISE  =F *
 	! ICPRT   =1 !
 	! IDPRT   =F !
 	! IWPRT   =F !
 	! ICFRQ   =1 !
 	! IDFRQ   =1 !
 	! IWFRQ   =1 !
 	! IPRTU   =3 !
 	! IMESG   =2 !
 	
 	Options for printing debug quantities
 	
 	! LDEBUG =F !
 	! IPFDEB =1 !
 	! NPFDEB =1 !
 	! NN1    =1 !
 	! NN2    =10 !

 	Species list for output options
 	
 	Species Name       Conc (prn, dsk), Dflux (prn, dsk), Wflux (prn, dsk), Mflux (dsk)
 	
 	* CO             =  1, 1, 0, 0, 0, 0, 0, 0 *
 	! $POLLUTID_LIST =  1, 1, 0, 0, 0, 0, 0 ,0 !
 
 ! END ! 
 
 -------------------------------------------------------------------------------
 6 -- Subgrid scale complex terrain inputs
 
 	! NHILL   =0   !
 	! NCTREC  =0   !
 	! MHILL   =2   !
 	! XHILL2M =1.0 !
 	! ZHILL2M =1.0 !
 	! XCTDMKM =0   !
 	! YCTDMKM =0   !
 
 ! END ! 
 
 -------------------------------------------------------------------------------
 
 7 -- Dry deposition of gases
 
                  diffusivity, alpha star, reactivity,
                  mesophyll resistance, Henry's Law coefficient
 	* = *
 
 ! END !
 -------------------------------------------------------------------------------
 
 8 -- Dry deposition of particles
 
                   geometric mass mean diameter, geometric SD
 	* = *
 
 ! END !
 
 -------------------------------------------------------------------------------
 
 9 -- Misc. dry deposition parameters
 		Reference cuticle and ground resistances, reference pollutant reactivity, cegetation state.

 	! RCUTR  =30.0 !
 	! RGR    =5.0  !
 	! REACTR =8.0  !
 	! NINT   =9    !
 	! IVEG   =1    !
 
 ! END !
 
 -------------------------------------------------------------------------------
 
 10 -- Wet Deposition Parameters
                  scavenging coefficient (liquid precipitation),
                  scavenging coefficient (frozen precipitation)
 	* = *
 
 ! END !
 
 -------------------------------------------------------------------------------
 
 11 -- Chemistry Parameters
 
 Several parameters are needed for one or more of the chemical transformation
 mechanisms. Those used for each mechanism are:

 	! MOZ =0       ! Ozone data input option
 	! BCKO3 =40.0  ! Monthly ozone concentrations in ppb 
 	! BCKNH3 =10.0 ! Monthly ammonia concentrations in ppb
 	
 	! RNITE1 =0.2  ! Nighttime SO2 loss rate in %/hour
 	! RNITE2 =2.0  ! Nighttime NOx loss rate in %/hour
 	! RNITE3 =2.0  ! Nighttime HNO3 formation rate in %/hour
 	
! END !
--------------------------------------------------------------------------------
 
 12 -- Misc. Dispersion and Computational Parameters
 			Vertical dispersion ctes. , dispersion rate above BL, crossover distance to time-dependent dispersion coeffs.
			LU associtated with urban dispersion, site charact. parameters for single-point meteo datafiles, sampling constarints
			puff-splitting controls, plumepath coeffs, wind speed powerlaw exponents, default temperature gradients and wind speed classes.

 ! SYTDEP =550.0 !
 ! MHFTSZ =0     !
 ! JSUP   =5     !
 ! CONK1  =0.01  !
 ! CONK2  =0.1   !
 ! TBD    =0.5   !

 Range of land use categories for which urban dispersion is assumed
 * IURB1 =10 *
 * IURB2 =19 *

 Site characterization parameters for single-point meteorological data files 
 ! ILANDUIN =20   !
 ! Z0IN     =0.25 !
 ! XLAIIN   =3.0  !
 ! ELEVIN   =0.0  !
 ! XLATIN   =0.0  !
 ! XLONIN   =0.0  !
 ! ANEMHT   =10.0 !
 ! ISIGMAV  =1    !
 ! IMIXCTDM =0    !
 ! XMXLEN   =1.0  !
 ! XSAMLEN  =1.0  !
 ! MXNEW    =99   !
 ! MXSAM    =99   !
 ! NCOUNT   =2    !
 ! SYMIN    =1.0  !
 ! SZMIN    =1.0  !
	! SVMIN = 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.50, 0.50, 0.5, 0.50, 0.50, 0.50 !
        ! SWMIN = 0.200, 0.120, 0.080, 0.060, 0.030, 0.016, 0.200, 0.120, 0.080, 0.060, 0.030, 0.016 !
 ! CDIV    =0.0, 0.0  !
 ! WSCALM  =0.5       !
 ! XMAXZI  =3000.0    !
 ! XMINZI  =20.0      !
 
 Default wind speed classes
 ! WSCAT = 1.54, 3.09, 5.14, 8.23, 10.80 !
 
 Default wind speed profile power-law
 ! PLX0 = 0.07, 0.07, 0.10, 0.15, 0.35, 0.55 !
 
 Default potential temperature gradient for stable classes E, F (degK/m)
 ! PTG0 =0.020, 0.035 !
 
 Default plume path coefficients for each stability class 
 ! PPC =.50,  .50,  .50,  .50,  .35,  .35 !
 
 ! SL2PF =10.0  !
 ! NSPLIT   =3  !
 ! IRESPLIT =0  !
 
 ! ZISPLIT =100.0 !
 ! ROLDMAX =0.25  !
 
 
 ! EPSSLUG =1.0e-4  !
 ! EPSAREA =1.0e-6  !
 ! DSRISE  =1.0     !
 
 ! END ! 
 
 -------------------------------------------------------------------------------
 
 13 -- Point source parameters
 
---------------
Subgroup (13a)
---------------
 ! NPT1 = 1  !  Number of point sources with parameters provided below
 ! IPTU = 1  !  Units used for point source emissions below (1:g/s)
 ! NSPT1 = 0 !	Number of source-species combinations with variable
                emissions scaling factors
 ! NPT2 = 0  !  Number of point sources with variable emission parameters
                provided in an external file
 ! END ! // end SECTION 13 - Part A
 
---------------
Subgroup (13b)
---------------

 Point Source: Constant Data
 
   Source       X         Y       Stack    Base     Stack    Exit  Exit    Bldg.  Emission
    No.     Coordinate Coordinate Height Elevation Diameter  Vel.  Temp.   Dwash   Rates
               (km)      (km)       (m)      (m)       (m)  (m/s) (deg. K)         
   ------   ---------- ---------- ------  ------   -------- ----- -------- ----- --------
   1 ! SRCNAM = ${emis_name} !
   1 * X =      899.1,   6168.3,      55.0,  2.0,     4.0,     3.0,   290.0, 0.0,   24.0 *
   1 ! X = ${emis_x}, ${emis_y}, ${emis_h}, ${emis_z}, ${emis_d}, ${emis_u}, ${emis_t}, 0.0, ${emis_q} !
   1 ! ZPLTFM  =       .0   !  
   1 ! FMFAC  =       1.0   ! 
   
 ! END !
 
 Building dimension data for sources subject to downwash
 
 Source     Effective building height, width, length and X/Y offset (in meters)
 No.        every 10 degrees. LENGTH, XBADJ, and YBADJ are only needed for
            MBDW = 2 (PRIME downwash option)
 ------     --------------------------------------------------------------------
 
 
 Point Source: Variable Emissions Data
 
 
 -------------------------------------------------------------------------------
 
 14 -- Area source parameters
 
  Number of polygon area sources with		       * NAR1 =NULL *
 parameters specified below
  Units used for area source emissions below 	       * IARU =NULL *
  Number of source-species combinations with variable  * NSAR1 =NULL *
 emissions scaling factors
  Number of buoyant polygon area sources with variable * NAR2 =NULL *
 location and emission parameters
 
 ! END !
 
 Area Source: Constant Data
 
 Source           Effect.    Base      Initial    Emission
  No.             Height   Elevation   Sigma z     Rates
                    (m)       (m)        (m)      
 -------          ------    ------     --------   ---------
 
            COORDINATES (km) FOR EACH VERTEX(4) OF EACH POLYGON
            --------------------------------------------------------
 Source                                                               a
  No.       Ordered list of X followed by list of Y, grouped by source
 ------     ------------------------------------------------------------
 
 
 Area Source: Variable Emissions Data
 
 
 
 
 -------------------------------------------------------------------------------
 
 15 -- Line source parameters
 
 Number of buoyant line sources with variable
 location and emission parameters
 
 ! END !
 
 Buoyant Line Source: Constant Data
 
 Source     Beg. X      Beg. Y      End. X    End. Y     Release    Base        Emission
  No.     Coordinate  Coordinate  Coordinate Coordinate  Height    Elevation      Rates
             (km)        (km)        (km)       (km)       (m)       (m)          
 ------   ----------  ----------  ---------  ----------  -------   ---------    ---------
 
 
 Buoyant Line Source: Variable Emissions Data
 
 -------------------------------------------------------------------------------
 
 16 -- Volume source parameters
 
 Number of volume sources with parameters provided

 
 ! END !
 
 Volume Source: Constant Data
 
          X           Y        Effect.    Base     Initial    Initial    Emission
      Coordinate  Coordinate   Height   Elevation  Sigma y    Sigma z     Rates
         (km)       (km)         (m)       (m)        (m)       (m)      
      ----------  ----------   ------    ------    --------   --------   --------
 
 Volume Source: Variable Emissions Data
 
 -------------------------------------------------------------------------------
 
 17 -- Non-gridded (discrete) receptor information
 
 Number of non-gridded receptors
 * NREC = *
 
 ! END !
 


EOF
