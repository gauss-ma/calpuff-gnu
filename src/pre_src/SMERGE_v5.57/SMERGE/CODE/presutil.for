c------------------------------------------------------------------------------
c --- PRESUTIL -- CALPUFF SYSTEM UTILITIES FOR PRESSURE TRANSFORMATIONS
c------------------------------------------------------------------------------
c
c --- PRESUTIL   Version: 1.0      Level: 040709
c
c -----------------------------
c --- CONTENT:
c -----------------------------
c
c --- Pressure Conversions
c      subroutine alp2stp
c      subroutine stp2alp
c      subroutine slp2stp
c      subroutine stp2slp
c
c --- Analysis of Pressure Data
c      subroutine scan144
c      subroutine regress
c      subroutine scanishwo
c      subroutine sqaccum
c
c -----------------------------
c
c-----------------------------------------------------------------------
      subroutine alp2stp(alp,elev,stp)
c-----------------------------------------------------------------------
c      
c --- PRESUTIL    Version: 1.0      Level: 040709                ALP2STP   
c                 K. Morrison, Earth Tech
c
c --- PURPOSE:    Calculate surface pressure based on altimeter setting
c                 and elevation.  The conversion is based on the
c                 operational algorithm of the Atmospheric Environment
c                 Service Barometry Program of Environment Canada.
c
c --- INPUTS:     ALP  - real scalar - altimeter setting (mbar)
c                 ELEV - real scalar - elevation (m)
c
c --- OUTPUTS:    STP  - real scalar - surface pressure (mbar)
c
c --- CALLED BY:  Various (Utility program)
c
c --- CALLS:      None
c
c-----------------------------------------------------------------------
c
c --- Test that there are valid input values (not missing), or else
c     return a missing value
c
      if(alp.lt.9998..and.elev.gt.-500.) then
        stp=((alp**0.19026)-8.41717e-5*elev)**5.25593
      else
        stp=9999.
      endif
      return
      end
c-----------------------------------------------------------------------
      subroutine stp2alp(stp,elev,alp)
c-----------------------------------------------------------------------
c      
c --- PRESUTIL    Version: 1.0      Level: 040709                STP2ALP   
c                 K. Morrison, Earth Tech
c
c --- PURPOSE:    Calculate altimeter setting based on surface pressure 
c                 and elevation.  The conversion is based on the
c                 operational algorithm of the Atmospheric Environment
c                 Service Barometry Program of Environment Canada.
c
c --- INPUTS:     STP  - real scalar - surface pressure (mbar)
c                 ELEV - real scalar - elevation (m)
c
c --- OUTPUTS:    ALP  - real scalar - altimeter setting (mbar)
c
c --- CALLED BY:  Various (Utility program)
c
c --- CALLS:      None
c
c-----------------------------------------------------------------------
c
c --- Test that there are valid input values (not missing), or else
c     return a missing value
c
      if(stp.lt.9998..and.elev.gt.-500.) then
        alp=((stp**0.19026)+8.41717e-5*elev)**5.25593
      else
        alp=9999.
      endif
      return
      end
c-----------------------------------------------------------------------
      subroutine slp2stp(slp,elev,tempk,tempk_12,stp)
c-----------------------------------------------------------------------
c      
c --- PRESUTIL    Version: 1.0      Level: 040709                SLP2STP   
c                 K. Morrison, Earth Tech
c
c --- PURPOSE:    Calculate surface pressure based on sea-level
c                 pressure, elevation, current air temperature, and 
c                 air temperature 12 hours ago.  If this latter variable
c                 is missing, only current temperature is used, but
c                 with a loss of accuracy.  The conversion is based on 
c                 the operational algorithm of the Atmospheric 
c                 Environment Service Barometry Program of Environment
c                 Canada.  Note that this conversion is only an
c                 approximation, since the "Plateau" correction is only
c                 an average, while in fact it should be station-
c                 specific.
c
c --- INPUTS:     SLP  - real scalar - sea-level pressure (mbar)
c                ELEV  - real scalar - elevation (m)
c               TEMPK  - real scalar - current temperature (K)
c            TEMPK_12  - real scalar - temperature 12 hours ago (K)
c
c --- OUTPUTS:    STP  - real scalar - surface pressure (mbar)
c
c --- CALLED BY:  Various (Utility program)
c
c --- CALLS:      None
c
c-----------------------------------------------------------------------
c
c --- Test that there are valid input values (not missing)
c
      if(slp.lt.9998..and.elev.gt.-500..and.tempk.lt.9998.) then
        tempc=tempk-273.15
c        
c ---   If temperature 12 hours ago is missing, use only current 
c       temperature
c
        if(tempk_12.lt.9998.) then
          tav=(tempk+tempk_12)/2.
        else
          tav=tempk
        endif
c        
c ---   Calculate plateau correction, approx. vapor pressure, and 
c       elevation corrections to vp and temperature
c
        plateau=2.0-tempc*(0.45+.0026*tempc)
        vappr=tempk**(0.279+tempc*(0.0116-.00014*tempc))
        ch=0.10743+elev*(2.225e-5+2.8322e-9*elev)
        adiab= 0.0065*elev/2.
        tvirt=tav+adiab+(ch*vappr)+plateau
        stp=slp*exp(-0.0341636*elev/tvirt)
c      
c --- Insufficient data, so return a missing value  
c
      else
        stp=9999.
      endif
c      
      return
      end
c-----------------------------------------------------------------------
      subroutine stp2slp(stp,elev,tempk,tempk_12,slp)
c-----------------------------------------------------------------------
c      
c --- PRESUTIL    Version: 1.0      Level: 040709                STP2SLP   
c                 K. Morrison, Earth Tech
c
c --- PURPOSE:    Calculate sea-level pressure based on surface
c                 pressure, elevation, current air temperature, and 
c                 air temperature 12 hours ago.  If this latter variable
c                 is missing, only current temperature is used, but
c                 with a loss of accuracy.  The conversion is based on 
c                 the operational algorithm of the Atmospheric 
c                 Environment Service Barometry Program of Environment
c                 Canada.  Note that this conversion is only an
c                 approximation, since the "Plateau" correction is only
c                 an average, while in fact it should be station-
c                 specific.
c
c --- INPUTS:     STP  - real scalar - surface pressure (mbar)
c                ELEV  - real scalar - elevation (m)
c               TEMPK  - real scalar - current temperature (K)
c            TEMPK_12  - real scalar - temperature 12 hours ago (K)
c
c --- OUTPUTS:    SLP  - real scalar - sea-level pressure (mbar)
c
c --- CALLED BY:  Various (Utility program)
c
c --- CALLS:      None
c
c-----------------------------------------------------------------------
c
c --- Test that there are valid input values (not missing)
c
      if(stp.lt.9998..and.elev.gt.-500..and.tempk.lt.9998.) then
        tempc=tempk-273.15
c        
c ---   If temperature 12 hours ago is missing, use only current 
c       temperature
c
        if(tempk_12.lt.9998.) then
          tav=(tempk+tempk_12)/2.
        else
          tav=tempk
        endif
c        
c ---   Calculate plateau correction, approx. vapor pressure, and 
c       elevation corrections to vp and temperature
c
        plateau=2.0-tempc*(0.45+.0026*tempc)
        vappr=tempk**(0.279+tempc*(0.0116-.00014*tempc))
        ch=0.10743+elev*(2.225e-5+2.8322e-9*elev)
        adiab= 0.0065*elev/2.
        tvirt=tav+adiab+(ch*vappr)+plateau
        slp=stp*exp(0.0341636*elev/tvirt)
c      
c --- Insufficient data, so return a missing value  
c
      else
        slp=9999.
      endif
c      
      return
      end
c-----------------------------------------------------------------------
      subroutine scan144(ioin,xintercep,xslope)
c-----------------------------------------------------------------------
c      
c --- PRESUTIL    Version: 1.0      Level: 040709                SCAN144   
c                 K. Morrison, Earth Tech
c
c --- PURPOSE:    Read a CD-144 file to carry out regression of the
c                 ratio of station pressure/sea-level pressure on
c                 station temperature, to establish the equation
c                 ratio = intercept + slope*temperature
c
c --- INPUTS:     IOIN - integer scalar - unit number of the CD-144 
c                        file
c
c --- OUTPUTS:    XINTERCEP - real scalar - regression intercept
c                 XSLOPE    - real scalar - regression slope
c
c --- CALLED BY:  RDWRITS
c
c --- CALLS:      SQACCUM, REGRESS 
c
c-----------------------------------------------------------------------
      character cslp,cpres,ctemp
      real ratsum,ratsumsq,obsn,obst,tsum,tsumsq,tratcp
10    format(31x,a4,7x,a4,a3)
c
c --- clear local variables
c
      ratsum=0.
      ratsumsq=0.
      obsn=0.
      obst=0.
      tsum=0.
      tsumsq=0.
      tratcp=0.
      xintercep=-9999.
      xslope=-9999.
c
c --- loop through the observations
c
      do
        read(ioin,10,end=95) cslp,cpres,ctemp
        obst=obst+1.                   
c
c ---   If any variable is missing, skip the observation
c
        if(cslp.eq.'    '.or.cpres.eq.'    '.or.ctemp.eq.'   ') cycle
c
c ---   Read the variables, convert pressure to mbar and temperature
c       to K, and calculate the ratio
c
        read(cslp,fmt='(f4.1)') slp
        if(slp.lt.500.) slp=slp+1000.
        read(cpres,fmt='(f4.0)') pres
        pres=pres*.33863
c
c ---   If pressure is unrealistically low, skip the observation
c
        if(pres.lt.300.) cycle
c
        if(ctemp(1:1).eq.'X'.or.ctemp(1:1).eq.'x') then
          read(ctemp,fmt='(1x,f2.0)') temp
          temp=-temp
        else
          read(ctemp,fmt='(f3.0)') temp
        endif
        temp=(temp-32.)*5./9.+273.15
        rat=pres/slp
c
c ---   Accumulates the sums, sums of squares, and sum of
c       cross-products
c
        call sqaccum(rat,temp,ratsum,ratsumsq,tsum,tsumsq,tratcp)
        obsn=obsn+1.
c
      enddo
c
c --- EOF, so do a sanity check on total observations.  Then check
c     that at least 5% of the total observations had all variables
c     to carry out the regression
c
95    if(obst.lt.0.9) return
      if((obsn/obst).gt.0.05) call regress(ratsum,ratsumsq,tsum,
     1         tsumsq,tractcp,obsn,xintercep,xslope,rsquare)
      rewind(ioin)
      return
      end
c-----------------------------------------------------------------------
      subroutine regress(sumy,sumy2,sumx,sumx2,sumxy,obsn,
     1                   xintercep,slope,rsquare)
c-----------------------------------------------------------------------
c      
c --- PRESUTIL    Version: 1.0      Level: 040709                REGRESS   
c                 K. Morrison, Earth Tech
c
c --- PURPOSE:    Carry out simple linear regression
c
c --- INPUTS:     SUMY  - real scalar - sum of y values
c                 SUMY2 - real scalar - sum of y**2 values
c                 SUMX  - real scalar - sum of x values
c                 SUMX2 - real scalar - sum of x**2 values
c                 SUMXY - real scalar - sum of x*y values
c                 OBSN  - real scalar - number of values
c
c --- OUTPUTS:    XINTERCEP - real scalar - regression intercept
c                 SLOPE     - real scalar - regression slope
c                 RSQUARE   - real scalar - coefficient of determination
c
c --- CALLED BY:  SCAN144, SCANISHWO
c
c --- CALLS:      none 
c
c-----------------------------------------------------------------------
c
c --- Carry out corrections to sums of squares and cross-products
c
      sumy2=sumy2-(sumy*sumy/obsn)
      sumx2=sumx2-(sumx*sumx/obsn)
      sumxy=sumxy-(sumx*sumy/obsn)
c
c --- Catch a degenrate case of no variation
c
      if(min(sumy2,sumx2).lt.1.e-20) then
        slope=0.
        rsquare=0.
      else
        slope=sumxy/sumx2
        rsquare=slope*sumxy/sumy2
      endif
c
      xintercep=sumy/obsn-(sumx/obsn)*slope
      return
      end
c-----------------------------------------------------------------------
      subroutine scanishwo(ioin,jdat,xintercep,xslope,altrat)
c-----------------------------------------------------------------------
c      
c --- PRESUTIL    Version: 1.0      Level: 040709              SCANISHWO   
c                 K. Morrison, Earth Tech
c
c --- PURPOSE:    Read an ISHWO, TD3505, or TD9956 file to carry out 
c                 regression of the ratio of station pressure/sea-level 
c                 pressure on station temperature, to establish the 
c                 equation ratio = intercept + slope*temperature, as well 
c                 as to establish the ratio of station pressure/altimeter
c                 setting.
c
c --- INPUTS:     IOIN - integer scalar - unit number of the data 
c                        file
c                 JDAT - integer scalar - file type
c                                         (5=ISHWO, 6=TD3505, 7=TD9956)
c
c --- OUTPUTS:    XINTERCEP - real scalar - regression intercept
c                 XSLOPE    - real scalar - regression slope
c                 ALTRAT    - real scalar - average ratio of pressure/
c                                           altimeter setting
c
c --- CALLED BY:  RDWRITS
c
c --- CALLS:      SQACCUM, REGRESS 
c
c-----------------------------------------------------------------------
      real ratsum,ratsumsq,obsn,obst,tsum,tsumsq,tratcp
      real altrat,obsa
      integer*2 ipwout(3)
      logical leof,licc
c
c --- clear local variables
c
      ratsum=0.
      ratsumsq=0.
      obsn=0.
      obst=0.
      tsum=0.
      tsumsq=0.
      tratcp=0.
      altrat=0.
      obsa=0.
      leof=.false.
      licc=.false.
      maxap=1
      xintercep=-9999.
      xslope=-9999.
c
c --- loop through the observations
c
      do
        call readish(ioin,id,iyr,imo,ida,ihr,jday,wd,ws,
     *    iceil,icc,tempk,irh,pres,altim,slp,xprec,ipdur,
     *    ipcode,dtow,ipwout,visb,elevx,licc,ixtz,leof,maxap,
     *    jdat)
        if(leof) goto 95
        obst=obst+1.
c
c ---   Check that station pressure is present, otherwise skip
c       the observation
c
        if(pres.gt.9998..or.pres.lt.200.) cycle
c
c ---   If altimeter is present, use value
c
        if(altim.lt.9998..and.altim.gt.800.) then
          altrat=altrat+pres/altim
          obsa=obsa+1.
        endif
c
c ---   If both temperature and sea-level pressure are present,
c       accumulate sums, sums of squares, and sum of cross-products
c
        if(tempk.lt.9998..and.tempk.gt.200..and.slp.lt.9998..and.
     1     slp.gt.800.) then
          obsn=obsn+1
          rat=pres/slp
          call sqaccum(rat,tempk,ratsum,ratsumsq,tsum,tsumsq,
     1                 tratcp)
        endif
c
      enddo
c
c --- EOF, so do a sanity check on total observations.  Then check
c     that at least 5% of the total observations had all variables
c     to carry out the regression and calculate the altimeter ratio
c
95    if(obst.lt.0.9) return
      if((obsn/obst).gt.0.05) then
        call regress(ratsum,ratsumsq,tsum,tsumsq,tractcp,obsn,
     1         xintercep,xslope,rsquare)
      endif
      if((obsa/obst).gt.0.05) then
        altrat=altrat/obsa
      else
        altrat=-9999.
      endif
      rewind(ioin)
      return
      end
c-----------------------------------------------------------------------
      subroutine sqaccum(y,x,ysum,ysum2,xsum,xsum2,xysum)
c-----------------------------------------------------------------------
c      
c --- PRESUTIL    Version: 1.0      Level: 040709                SQACCUM   
c                 K. Morrison, Earth Tech
c
c --- PURPOSE:    Accumulate sums, sums of squares, and sum of cross-
c                 products for regression analysis
c
c --- INPUTS:     Y     - real scalar - y value
c                 X     - real scalar - x value
c
c --- OUTPUTS:    YSUM  - real scalar - sum of y values
c                 YSUM2 - real scalar - sum of y**2 values
c                 XSUM  - real scalar - sum of x values
c                 XSUM2 - real scalar - sum of x**2 values
c                 XYSUM - real scalar - sum of x*y values
c                 OBSN  - real scalar - number of values
c
c --- CALLED BY:  SCAN144, SCANISHWO
c
c --- CALLS:      none 
c
c-----------------------------------------------------------------------
      ysum=ysum+y
      ysumsq=ysumsq+y*y
      xsum=xsum+x
      xsumsq=xsumsq+x*x
      xysum=xysum+x*y
      return
      end
c

