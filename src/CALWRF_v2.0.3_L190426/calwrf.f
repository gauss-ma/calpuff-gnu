      Program CALWRF

C ----------------------------------------------------------------------
C ---  CALWRF -- Convert WRF output to 3D.DAT file format
C ----------------------------------------------------------------------
C
C   Copyright (c) 2007-2013 by Exponent, Inc.         
C
C --- CALWRF   Version: 2.0.3              Level: 190429          MAIN
C
C Purpose:
C     Create 3D.DAT file for WRF output files
C     and create the corresponding 2D.DAT file
C
C     Note: 3D.DAT version is "2.11", which includes prognostic
C           model name and the time stamp of 3D.DAT creation on the 
C           first line of the file after 3D.DAT name and version. 

C Zhong-Xiang Wu
C 6/27/2007
c
c --- UPDATES:

c --- Version 2.0.2, Level 131108 to Version 2.0.3, Level 190429
c     C.DesAutels
c     1. Revise method to calculate incremental rainfall.  Direct
c        access read will now read both current hour and prior hour
c        of rainfall.  This will avoid needing to store prior hour
c        values.  Prior method would result in errors for the first
c        hour read in 2nd or subsequent WRF.out file.  
c     2. Revise header read to allow WRF version 4. output.
c     3. Reduce screen output for out-of-range variables to improve
c        readability.
c
c **********************************************************************
c --- Exponent, Inc. Updates:
c **********************************************************************
c --- Version 2.0.1, Level: 130418 to Version 2.0.2, Level: 131108
c     1. Revise processing methods for replacing values that are out-of-
c        range.  Water content variables use the average of good values
c        in adjacent layers at the same location (no horizontal
c        averaging).  All other variables average adjacent good values
c        in the same layer (no vertical averaging).  When all adjacent
c        values are out-of-range, the replacement uses the appropriate
c        range limit.  Replaced values are not used to calculate
c        replacement values at other locations.
c        Exclude geopotential heights from out-of-range processing
c            Modified: (main), calwrf.cm2
c     2. Fix bug in obtaining values at adjacent locations used for
c        replacing out-of-range values
c            Modified: (main)
c     3. Remove the compile-time setting for selecting a replacement
c        method that uses the range-check limits alone (method is no
c        longer available)
c            Modified: (main), interph, calwrf.par
c **********************************************************************
C ----------------------------------------------------------------------
C --- Version 2.0.0, Level: 121203 to Version 2.0.1, Level: 130418
C     1. Prevent idatenext=-1
C     2. Fix print statement bug in 3d range check
C     3. Avoid insignificant warning messages for 3d range check due to
C        rounding errors near zero.
C
C     Amy McVey, Michael Newman
c
c **********************************************************************
c --- Exponent, Inc. Updates:
c **********************************************************************
c --- Version 1.4, Level: 100322 to Version 2.0.0, Level: 121203
c     1. Fix bug in precipitation rate.  The hourly precipitation rate
c        for first 2 hours selected for output from each WRF file did
c        not properly remove the accumulated precipitation amount from
c        earlier periods since the start of the WRF simulation.  This
c        will overstate the rate if there was precipitation in these
c        earlier periods.
c            Modified: (main)
c     2. Create a 2D.DAT file corresponding to the 3D.DAT file, and
c        allow multiple WRF files to be processed.  Also, add range
c        checks on the 2D/3D variables that are output.  Keep track of
c        2D and 3D dataset versions seperately.  The 2D.DAT dataset
c        extends the comment section to add the names (in order) of
c        the 2D variables in the file, along with units and brief
c        description
c            New:      calwrf.cm3, calwrf.blk
c            Modified: calwrf.cm1
c                      (main), setup, interph,  Outcomment(removed)
c     3. Fix the output record of "Flags for 2D variables" in the 3D.DAT
c        header (#5).  The record had missed one of the flags.  This bug
c        has no impact since the content of this record is not used in
c        processing the 3D data, and no 2D output had been previously
c        created.
c            Modified: (main)
c     4. Fix the center (lat,lon) of the map projection reported to the
c        output 3D.DAT file for nested domains.  The outermost grid
c        mapping center was correct.
c            Modified: (main)
c     5. Modify string input routine to initialize output string
c        variable to blanks
c            Modified: GETSTR
c **********************************************************************
c
c --- UPDATES:

c --- Version 1.3, Level 090914 to Version 1.4, Level 100322
c     1. Add Mercator map project
c     2. Add WRF code version in 3D.DAT header
C 
C     Zhong-Xiang Wu
c     3/22/2010

c --- Version 1.2, Level 090603 to Version 1.3, Level 090914
c     1. Clean up code version and level numbers.
c     
C     Zhong-Xiang Wu
c     9/14/2009

c --- Version 1.1, Level 080429 to Version 1.2, Level 090603
c     1. Include WRF Version 3 output files
c
C     Zhong-Xiang Wu
c     6/3/2009

c --- Version 1.0, Level 070627 to Version 1.1, Level 080429
c     4/29/2008 (DGS, ZWU)
c     1. Update COORDLIB from V1.98 Level 060911 to V1.99 Level 070921
c        - Conversion of point in S. hemisphere to UTM-N returned coord.
c          as UTM-S instead, for conversions from all map projections
c          except lat/lon. (does not affect CALWRF)
c        - Initialization of a few work arrays were missing.  These have
c          no effect on results.
c          Modified:  COORDS, PJINIT
C     2. Allow missing one of paired rain and cloud waters in early WRF 
C        output files.
C     3. Fix an bug for graupel compressed output (Thanks to B. Brashers)
C     4. Use the RH calculation method in CALMM5 to improve RH at high
C        altitude.
C     5. Set minimun of vertical velocity w=0 to eleminate
C        +/-0 differences between PC and UNIX/LINUX. 
C     6. Fix a bug of wind rotating (Thanks to B. Brashers)
c -----------------------------------------------------------------------
c
      include 'calwrf.par'
      include 'calwrf.cm1'
      include 'calwrf.cm2'
      include 'calwrf.cm3'

c --- Specify CALWRF code version and level
      codever='v2.0.3'
      codelevel='190426'

C     Specify 3D.DAT/2D.DAT data sets format Version and level number  

c --- v2.0.0, Level 121203
c --- Use explicit variables for 3D and 3D (they do not need to be the
c --- same)
c --- Revert 3D dataset to v2.1 since the format did not change
c --- Set new 2D dataset to v2.1 since this includes a variable-name
c --- block as part of the comment block rather than as a new block
c --- that changes the dataset format
c --- The datalevel is not used
      dataver2d='2.1'    
      dataver3d='2.1'    

c --- Specify output data set names from this code
      name3d='3D.DAT'
      name2d='2D.DAT'

      cmodel='WRF_ARW'

      call setup

C --- Processing WRF output files
      ifile=1

c --- v2.0.0, Level 121203
      ifirst=1
      idatenext=idateb
 2000 continue

      fin=flnames(ifile)
      write(ilg,201)ifile,trim(fin)
 201  format(' Processing WRF File:',i4,2x,a)

C --- Open WRF file
      istat=NF_OPEN(fin, NF_NOWRITE, ncid)
      if(istat .ne. 0) then
         write(ilg,*) ' error opening netcdf file ',trim(fin)
         print *,' error opening netcdf file ',trim(fin)
         stop
      endif

c --- v2.0.0, Level 121203
      write(ilg,*) ' Open WRF netcdf file ',ifile,': ',trim(fin)
      print *,' Open WRF netcdf file ',ifile,': ',trim(fin)

C --- Get model output times
      istat=NF_INQ_VARID ( ncid, 'Times', id_var)
      n_times=0
      if (istat .ne. 0) then
         n_times=1   ! Static - no time available
         static=.True.
         go to 1000
      else
         istat=NF_INQ_VARID(ncid,'Times',id_time)
      endif

      istat=NF_INQ_VAR(ncid,id_time,varnam,ivtype,ndims,dimids,natts)
      do i=1,ndims
         istat=NF_INQ_DIMLEN(ncid, dimids(i), dims(i))
      enddo

      n_times=dims(2)
      print *,' N_TIMES:',n_times

      nchars=dims(1)
      do i=1,n_times
         istart_t(1)=1
         iend_t(1)=nchars
         istart_t(2)=i
         iend_t(2)=1
         istat=NF_GET_VARA_TEXT  ( ncid, id_time,
     &         istart_t, iend_t, times(i))
      enddo

C --- Convert to ndates
      do i=1,dims(2)
         call cvtdate(times(i),ndates(i),nsecs(i))
      enddo

 1000 continue    ! come here direct for some static files

C --- Get general info
      istat=NF_INQ(ncid, nDims, nVars, nAtts, unlimDimID)
      nVars_sav=nVars

C     Get some header information
c --- v2.0.0, Level 121203
      btdim=24  ! max for geo_grid static data
      do ii=1, nDims
         istat=NF_INQ_DIM(ncid, ii, dname, dval)
         dnames(ii)=dname
         dvals(ii)=dval
         if     (dname .eq. 'west_east') then
            wedim=dval
         elseif (dname .eq. 'south_north') then
            sndim=dval
         elseif (dname .eq. 'bottom_top') then
            btdim=dval
         elseif(dname .eq. 'west_east_stag') then
            wedims=dval
         elseif (dname .eq. 'south_north_stag') then
            sndims=dval
         elseif (dname .eq. 'bottom_top_stag') then
            btdims=dval
         elseif (dname .eq. 'num_metgrid_levels') then
            btdim=dval
         endif
      enddo

C --- Get global attributes
      call gblatts

C --- Write 3D.DAT header
      write(datamod,401)cmodel,cmodel_ver,(vdate(ii),ii=1,3)
 401  format(2a16,' Created at ',i4.4,2('-',i2.2))

      write(comment,402)codever,codelevel
c --- v2.0.0, Level 121203
 402  format('Produced by CALWRF ',a12,'  Level: ',a8)

C --- Check dimension
      if(ie.gt.nx .or. je.gt.ny .or. ke.gt.nz) then
         write(ilg,136)ie,je,ke,nx,ny,nz
         print 136,ie,je,ke,nx,ny,nz
 136     format(' Error: Required I/J/K range out of WRF domain:',/
     &          '        Required I/J/K:    ',3i5,/
     &          '        WRF Range of I/J/K:',3i5)
         stop
      endif

C --- Re-Setup selection ranges for all grids for negative input
      id_reset=0
      if(ib.lt.1 .or. ie.lt.1) then
         ib=1
         ie=wedim
         id_reset=id_reset+1
      endif
      if(jb.lt.1 .or. je.lt.1) then
         jb=1
         je=sndim
         id_reset=id_reset+1
      endif
      if(kb.lt.1 .or. ke.lt.1) then
         kb=1
         ke=btdim
         id_reset=id_reset+1
      endif

C --- Reset time range for negative input
      if(id_reset.gt.0) then
         write(ilg,*)' Reset I/J/K ranges to:',ib,ie,jb,je,kb,ke
         print *,' Reset I/J/K ranges to:',ib,ie,jb,je,kb,ke
      endif

      if(idateb.lt.1 .or. idatee.lt.1) then
         idateb=ndates(1)
         idatenext=idateb  !TRC prevent idatenext=-1 04-18-2013
         idatee=ndates(n_times)
         write(ilg,145)idateb,idatee
 145     format(' Reset require time range:',2i12)
      endif

c --- v2.0.0, Level 121203
C --- Need one hour before idateb to get correct rainfall amount
      if(idateb.eq.ndates(1)) then
         idateb1=idateb
      else
         idateb1=idateb
         call stamptime(idateb1,iyr,imon,iday,ihour)
         call chgtim(iyr,imon,iday,ihour,-1)
         call timestamp(iyr,imon,iday,ihour,idateb1)
      endif
      write(ilg,1112)idateb,idateb1
 1112 format(' One-Hour less than idateb:',2i12)


c --- v2.0.0, Level 121203
      if(nfiles.eq.1) then
         if(idateb1.lt.ndates(1) .or. 
     &          idatee.gt.ndates(n_times)) then
            write(ilg,*)'ERROR: Required Time is out of range:'
            write(ilg,*)'   Range required: ',idateb1,idatee
            write(ilg,*)'       WRF output: ',ndates(1),ndates(n_times)
            stop
         endif
      else
         if(ifile.eq.1) then
            if(idateb1.lt.ndates(1)) then
               write(ilg,*)'ERROR: Required BegTime is out of range:'
               write(ilg,*)'    Required: ',idateb1
               write(ilg,*)'  WRF output: ',ndates(1)
               stop
            endif
         elseif(ifile.eq.nfiles) then
            if(idatee.gt.ndates(n_times)) then
               write(ilg,*)'ERROR: Required EndTime is out of range:'
               write(ilg,*)'    Required: ',idatee
               write(ilg,*)'  WRF output: ',ndates(n_times)
               stop
            endif
         else
            write(ilg,*)' Time setting: OK'
         endif

      endif

C --- Get total hours
      call gethours(idateb,idatee,nhours)
      nxsub=ie-ib+1
      nysub=je-jb+1
      nzsub=ke-kb+1

C --- Array allocation
c --- v2.0.0, Level 121203
      if(ifirst.eq.1) then

C --- Allocate 1-D arrays
      allocate(sig(btdim))

C --- Allocate 2-D arrays
      allocate(xlat(wedim,sndim))
      allocate(xlong(wedim,sndim))
      allocate(diff(wedim,sndim))
      allocate(alpha(wedim,sndim))
      allocate(ter(wedim,sndim))
      allocate(alu(wedim,sndim))

      allocate(ppnow(wedim,sndim))
      allocate(ppold(wedim,sndim))
      allocate(ppcur(wedim,sndim))
      allocate(ppc(wedim,sndim))
      allocate(ppcold(wedim,sndim))
      allocate(ppccur(wedim,sndim))
      allocate(ppnc(wedim,sndim))
      allocate(ppncold(wedim,sndim))
      allocate(ppnccur(wedim,sndim))

      allocate(u10(wedim,sndim))
      allocate(v10(wedim,sndim))
      allocate(u10t(wedim,sndim))
      allocate(v10t(wedim,sndim))
      allocate(wd10(wedim,sndim))
      allocate(ws10(wedim,sndim))

      allocate(x2tmp(wedim,sndim))

C --- Allocate 3-D arrays
      allocate(x3d(wedim,sndim,btdim,n3d))
      allocate(x2d(wedim,sndim,n2d2))

c --- v2.0.2, Level 131108
      allocate(x3dpk(wedim,sndim,btdim,n3d))
      allocate(x2dpk(wedim,sndim,n2d2))

      allocate(alpha3(wedim,sndim,btdim))
      
      allocate(pp(wedim,sndim,btdim))
      allocate(tk(wedim,sndim,btdim))

      allocate(gh(wedim,sndim,btdim+1))
      allocate(gh2(wedim,sndim,btdim))
      allocate(ww(wedim,sndim,btdim+1))
      allocate(ww2(wedim,sndim,btdim))

      allocate(uu(wedim+1,sndim,btdim))      
      allocate(vv(wedim,sndim+1,btdim))
      allocate(uu2(wedim,sndim,btdim))      
      allocate(vv2(wedim,sndim,btdim))
      allocate(uu2t(wedim,sndim,btdim))      
      allocate(vv2t(wedim,sndim,btdim))
      allocate(wd(wedim,sndim,btdim))      
      allocate(ws(wedim,sndim,btdim))

      endif

C --- lat/lon at mass points
      istart       =1
      iend         =1
      iend(1)      =wedim
      iend(2)      =sndim
      istat=NF_INQ_VARID(ncid, "XLAT", id_var)
      if (istat /= 0) istat=NF_INQ_VARID(ncid, "XLAT_M", id_var)
      istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,xlat)
      istat=NF_INQ_VARID(ncid, "XLONG", id_var)
      if (istat /= 0) istat=NF_INQ_VARID(ncid, "XLONG_M", id_var)
      istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,xlong)

      if(op_rot)then

         diff=xlong - stand_lon
         where(diff .gt. 180.) 
            diff=diff - 360.
         end where
         where(diff .lt. -180.) 
            diff=diff + 360.
         end where

         alpha=diff * cone * RPD
         where(xlat .lt. 0.)
            alpha=-1. * alpha
         end where

         iend(3)=btdim

         do k=1,iend(3)
            do j=1,iend(2)
               do i=1,iend(1)
                  alpha3(i,j,k)=alpha(i,j)
               enddo
            enddo
         enddo

      endif

C --- Terrain and land use at mass points
      istart       =1
      iend         =1
      iend(1)      =wedim
      iend(2)      =sndim

      istat=NF_INQ_VARID(ncid, "LU_INDEX", id_var)
      istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,alu)
      istat=NF_INQ_VARID(ncid, "HGT", id_var)
      istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,ter)

C --- ETA sigma half (mass) levels 
      istart       =1
      iend         =1
      iend(1)      =btdim
      istat=NF_INQ_VARID(ncid, "ZNU", id_var)
      istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,sig)

C --- Grid(1,1)-SW coordinates
      rlatc=xgatts(24)
      rlonc=xgatts(20)

      truelat1=xgatts(18)
      truelat2=xgatts(19)

      map_proj=xgatts(21)
      if(map_proj.eq.1) then
         cmap='LCC'
      elseif(map_proj.eq. 2) then
         cmap='PS'
      elseif(map_proj.eq. 3) then
         cmap='EM'
      else
         write(ilg,233)map_proj
         print 233,map_proj
 233     format(' Not coded for map_projection > 3:',i3)
         stop
      endif

      dxy=xgatts(1)/1000.  ! units in km
      if(xgatts(1).ne.xgatts(2)) then
         write(ilg,234)xgatts(1),xgatts(2)
         print 234,xgatts(1),xgatts(2)
 234     format(' Warning: dx <> dy:',2f12.3)
      endif

c --- Get East-North off-set
c --- Initialize GLOBE1
      cmapi='LL'
      iutmi=999.9
      tmsone=999.9
      rlat1i=truelat1
      rlat2i=truelat2
      rlat0i=rlatc
      rlon0i=rlonc
      feasti = 0.
      fnorti = 0.
      
      iutm=999.9
      tmsone=999.9
      rlat1=truelat1
      rlat2=truelat2
      rlat0=rlatc
      rlon0=rlonc
      feast = 0.
      fnorth = 0.
            
      utmhem='N'
      if(rlatc.lt.0) utmhem='S'
      cmapo=cmap

c --- Set translation vectors                   --- call GLOBE1
      call GLOBE1(cmapi,iutmi,tmsone,rlat1i,rlat2i,rlat0i,rlon0i,
     &            feasti,fnorti,
     &            cmapo,iutm,tmsone,rlat1,rlat2,rlat0,rlon0,
     &            feast,fnorth,
     &            caction,vecti,vecto)

c --- Done Setting translation vectors
c --- Translate coordinates                     --- call GLOBE
      flat=xlat(1,1)
      flon=xlong(1,1)
      cdatumi='NWS-84'
      cdatumo='NWS-84'
c      cdatumi='WGS-84  '
c      cdatumo='WGS-84  '

      call GLOBE(ilg,caction,cdatumi,vecti,cdatumo,vecto,
     &     flon,flat,x1dmn,y1dmn,iutm,utmhem)
      write(ilg,*)' x1dmn/y1dmn:',x1dmn,y1dmn

C --- Initial rain arrays
c --- v2.0.0, Level 121203
      ppcur=0
      ppnow=0
      ppold=0

C --------------- Main Loop: Over time periods --------
      time1=1
      time2=n_times

      do 5000 itm=time1,time2  ! open loop

      ctime=times(itm)
      idate=ndates(itm)

c --- v2.0.0, Level 121203
      if(idate.lt.idateb1 .or. idate.gt.idatee) then
         goto 5000
      endif

      call clean(ctime)

      write(ilg,113)itm,ctime(1:19),idate
      print 113,itm,ctime(1:19),idate
 113  format(' Process:',i5,2x,a19,2x,i12)

c     if(static)then
c        ctime=" "            
c     else
c        ctime=times(itm)
c        print*,"TIME: ",ctime(1:19)
c     endif

C --- Loop 2: Over extracted 3D-variables
      i3dpk=0
      x3d=0
      x2d=0
c --- v2.0.0, Level 121203
      ppcur=0

      do 5500 ivar=1,n3d

C --- Dimension settings
      istart       =1
      istart(4)    =itm
      iend         =1
      iend(1)      =wedim
      iend(2)      =sndim
      iend(3)      =btdim

C --- Set start time for prior hour to allow calculation
c --- if incremental rainfall for timestamps after first

      if (ifirst.ne.1) then
       istartp  = istart
       istartp(3) = itm-1
      else
       istartp = istart
      endif
      vname=blank
      vname=trim(v3dnames(ivar))
      if(trim(vname).eq.'V') goto 5500   ! Get V with U

      istat=NF_INQ_VARID(ncid, vname, id_var)
      if(istat.ne.0) then
c         write(ilg,115)ivar,trim(vname)
c         print 115,ivar,trim(vname)
 115     format(' 3D-Variable not found: ',i5,2x,a)

C ---    Minimum requirement: P,PH,T,U,V,W,QVAPOR
         if(ivar.le.7 .and. trim(vname).ne.'V') then
            write(ilg,116)ivar,trim(vname)
            print 116,ivar,trim(vname)
 116        format(' Error: This 3D-Variable can not be missing',/
     &           10x,i4,2x,a)
            stop
         else
            write(ilg,117)ivar,trim(vname)
            print 117,ivar,trim(vname)
 117        format(' 3D-variable missing:',5x,i4,2x,a)
            goto 5500
         endif
      endif

C --- Pressure
      if (vname.eq.'P') then
         istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,pp)
         istat=NF_INQ_VARID(ncid, "PB", id_var)
         if (allocated(data_r)) deallocate(data_r)
         allocate(data_r(iend(1),iend(2),iend(3)))
         istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,data_r)
         pp=(pp + data_r)*0.01 ! in mb
         call pass3d(x3d,pp,nx,ny,nz,n3d,ivar)
         i3dpk(ivar)=1
C --- Temperature 
      elseif (vname.eq.'T') then
         istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,tk)
         if(i3dpk(1).ne.1) then
            print *,' Error: No-Pressure exists',i3dpk(1)
            stop
         endif
         tk=(tk+300.)*(pp/1000.)**(287.04/1004.)
         call pass3d(x3d,tk,nx,ny,nz,n3d,ivar)
         i3dpk(ivar)=1
C --- GH - stage in Z
      elseif (vname.eq.'PH') then
         iend(3)      =btdim+1
         istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,gh)
         istat=NF_INQ_VARID(ncid, "PHB", id_var)
         if (allocated(data_r)) deallocate(data_r)
         allocate(data_r(iend(1),iend(2),iend(3)))
         istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,data_r)
         gh=(gh+data_r)/9.81  !units in m
         call interpz(gh,gh2,nx,ny,nz)
         call pass3d(x3d,gh2,nx,ny,nz,n3d,ivar)
         i3dpk(ivar)=1
C --- WW - stage in Z
      elseif (vname.eq.'W') then
         iend(3)      =btdim+1
         istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,ww)
         call interpz(ww,ww2,nx,ny,nz)
         call pass3d(x3d,ww2,nx,ny,nz,n3d,ivar)
         i3dpk(ivar)=1
C --- U - 3D: west_east_stag
      elseif (vname.eq.'U') then
         iend(1)      =wedim+1  ! but not in read_wrf_nc code
         iend(2)      =sndim
         iend(3)      =btdim
         istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,uu)
         call interpu(uu,uu2,nx,ny,nz)

C ---    Get V with U rightaway V - 3D: south_north_stag
         vname='V'
         iend(1)      =wedim  
         iend(2)      =sndim+1 ! but not in read_wrf_nc code
         iend(3)      =btdim
         istat=NF_INQ_VARID(ncid, vname, id_var)
         istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,vv)
         call interpv(vv,vv2,nx,ny,nz)

C ---    Rotate U/V if needed
         if(op_rot) then
            uu2t=uu2
            vv2t=vv2
            uu2=vv2t*sin(alpha3)+uu2t*cos(alpha3)
            vv2=vv2t*cos(alpha3)-uu2t*sin(alpha3)
         endif
C ---    Covert to WS,WD
         ws=uu2*uu2+vv2*vv2
         ws=sqrt(ws)
         wd=0.
         where(ws.ge.1.0E-9) 
            wd=270.-RDP*atan2(vv2,uu2)
         end where
         where(wd .gt. 360.) 
            wd=wd - 360.
         end where
         where(wd .le. 0.) 
            wd=wd + 360.
         end where

C ---    Save WD/WS
         call pass3d(x3d,wd,nx,ny,nz,n3d,ivar)
         call pass3d(x3d,ws,nx,ny,nz,n3d,ivar+1)
         i3dpk(ivar)=1
         i3dpk(ivar+1)=1
C --- Other 3D-variables on mass point
      else
         if (allocated(xtmp)) deallocate(xtmp)
         allocate(xtmp(iend(1),iend(2),iend(3)))
         istat=NF_GET_VARA_REAL( ncid,id_var,istart,iend,xtmp)
         call pass3d(x3d,xtmp,nx,ny,nz,n3d,ivar)
         i3dpk(ivar)=1
      endif 

C     Make sure units remain a clean field
      varnam=blank
      units=blank

C     Deallocate everything 
      if (allocated(data_r)) deallocate (data_r)
      if (allocated(data_dp_r)) deallocate (data_dp_r)
      if (allocated(data_i)) deallocate (data_i)

 5500 continue   ! end of 3D-ivar

C --- Check status of 3D array
c      do ivar=1,n3d
c         write(ilg,320)ivar,i3dpk(ivar),v3dnames(ivar),trim(ctime)
c         print 320,ivar,i3dpk(ivar),v3dnames(ivar),trim(ctime)
c 320     format(' Chk 3D-Vars:',i5,i3,2x,a20,a)
c      enddo
C --- Finish 3D vars

C --- 2D variables (All at mass point)
      i2dpk=0
      do 6500 ivar=1,n2d

C --- Dimension settings
      istart       =1
      istart(3)    =itm
      iend         =1
      iend(1)      =wedim
      iend(2)      =sndim

      vname=blank
      vname=trim(v2dnames(ivar))

      if(vname.eq.'V10') goto 6500   ! get V10 with U10

      istat=NF_INQ_VARID(ncid, vname, id_var)

      if(istat.ne.0) then
         write(ilg,335)ivar,trim(vname)
         print 115,ivar,trim(vname)
 335     format(' 2D-Variable not found: ',i5,2x,a)
         if(ivar.le.3) then
            write(ilg,336)
            print 336
 336        format(' Error: This 2D-Variable can not be missing')
            stop
         else
            goto 6500
         endif
      endif

C --- U10 - 2D: west_east (U10/V10 at mass point and not staged)
      if(vname.eq.'U10') then
         iend(1)      =wedim
         iend(2)      =sndim
         istat=NF_GET_VARA_REAL(ncid,id_var,istart,iend,u10)

C ---    Get V10 with U10 rightaway V10 - 2D
         vname='V10'
         istat=NF_INQ_VARID(ncid, vname, id_var)
         istat=NF_GET_VARA_REAL(ncid,id_var,istart,iend,v10)

C ---    Rotate U/V if needed
         if(op_rot) then
            u10t=u10
            v10t=v10
            u10=v10t*sin(alpha)+u10t*cos(alpha)
            v10=v10t*cos(alpha)-u10t*sin(alpha)
         endif

C ---    Covert to WS,WD
         ws10=u10*u10+v10*v10
         ws10=sqrt(ws10)

         wd10=0.
         where(ws10.ge.1.0E-9) 
            wd10=270.-RDP*atan2(v10,u10)
         end where

         where(wd10 .gt. 360.) 
            wd10=wd10 - 360.
         end where
         where(wd10 .le. 0.) 
            wd10=wd10 + 360.
         end where

C ---    Save WD/WS
c --- v2.0.0, Level 121203
         call pass2d(x2d,wd10,nx,ny,n2d2,ivar)
         call pass2d(x2d,ws10,nx,ny,n2d2,ivar+1)
         i2dpk(ivar)=1
         i2dpk(ivar+1)=1
      elseif(vname.eq.'RAINNC') then

c --- v2.0.0, Level 121203
         ppnc=0
         ppc=0
         ppnow=0
C ---    Non-convective (grid) rain
         istat=NF_GET_VARA_REAL(ncid,id_var,istart,iend,ppnc)
         istat=NF_GET_VARA_REAL(ncid,id_var,istartp,iend,ppncold)

C ---    Convective rain
         vname='RAINC'
         istat=NF_INQ_VARID(ncid, vname, id_var)
         if(istat.ne.0) then
            write(ilg,345)trim(vname)
            print 345,trim(vname)
 345        format(' 2D-Variable not found: ',a,/
     &            ,' Set RAINC to zero')
         else
            istat=NF_GET_VARA_REAL(ncid,id_var,istart,iend,ppc)
            istat=NF_GET_VARA_REAL(ncid,id_var,istartp,iend,ppcold)
         endif

C ---    Total accumlated rainfall
         ppnow=ppnc+ppc
         ppold=ppncold+ppcold

         if(ifirst.eq.1) then
            ppcur=ppnow
            ppccur=ppc
            ppnccur=ppnc
         else
            ppcur=ppnow-ppold
            ppccur=ppc-ppcold
            ppnccur=ppnc-ppncold
         endif

         call pass2d(x2d,ppcur,nx,ny,n2d2,ivar)
         call pass2d(x2d,ppccur,nx,ny,n2d2,n2d+1)
         call pass2d(x2d,ppnccur,nx,ny,n2d2,n2d+2)

         i2dpk(ivar)=1
      else
         istat=NF_GET_VARA_REAL(ncid,id_var,istart,iend,x2tmp)
         call pass2d(x2d,x2tmp,nx,ny,n2d2,ivar)
         i2dpk(ivar)=1
      endif 

C     make sure units remain a clean field
      varnam=blank
      units=blank

C     Deallocate everything 
      if (allocated(data_r)) deallocate (data_r)
      if (allocated(data_dp_r)) deallocate (data_dp_r)
      if (allocated(data_i)) deallocate (data_i)

 6500 continue   ! end of ivar

C --- Check status of 2D array
c      do ivar=1,n2d
c         write(ilg,321)ivar,i2dpk(ivar),v2dnames(ivar),trim(ctime)
c         print 321,ivar,i2dpk(ivar),v2dnames(ivar),trim(ctime)
c 321     format(' Chk 2-D Vars:',i5,i3,2x,a20,a)
c      enddo

C --- End of 2D-variables

C --- Create 3D file for current hour
      if(ifirst.ne.1) goto 6600

C --- Header Record #1/#2
      ncomm=1
c --- v2.0.0, Level 121203
      write(io,'(2(a16),a64)') name3d,dataver3d,datamod
      write(io,'(i4)')ncomm
      do i=1,ncomm
         write(io,'(a132)')comment
      enddo

c --- v2.0.0, Level 121203
      if(iosrf.eq.1) then
         write(isrf,'(2(a16),a64)') name2d,dataver2d,datamod
         ncomm=n2d2+2
         write(isrf,'(i4)')ncomm
         write(isrf,'(a132)')comment

         write(isrf,602)n2d2
 602     format(i3,'  Number of 2D Variables')
         do i=1,n2d2
            write(isrf,'(a)')trim(c2dlabels(i))
         enddo

      endif

C --- Record #3: ioutw, ioutq, ioutc, iouti, ioutg, iosrf
      
      ioutw=1  ! w - must
      ioutq=1  ! q - must
      ioutc=0
      iouti=0
      ioutg=0

      if(i3dpk(8).eq.1) then
         ioutc=1
         ncmp=1
      endif
      if(i3dpk(9).eq.1) then
         ioutc=1
         ncmp=2
      endif

      if(i3dpk(10).eq.1 .and. i3dpk(11).eq.1) then
         iouti=1
         ncmp=4
      endif
      if(i3dpk(12).eq.1) then
         ioutg=1
         ncmp=5
      endif

      write(ilg,211)ioutw,ioutq,ioutc,iouti,ioutg,iosrf
 211  format(1x,'ioutw:',i2/,1x,'ioutq:',i2/,1x,'ioutc:',i2/,
     :     1x,'iouti:',i2/,1x,'ioutg:',i2/,1x,'iosrf:',i2/)

      write(io,212)ioutw,ioutq,ioutc,iouti,ioutg,iosrf
 212  format(6(i3))
c --- v2.0.0, Level 121203
      if(iosrf.eq.1)
     &   write(isrf,212)ioutw,ioutq,ioutc,iouti,ioutg,iosrf

C     Record #4
      write(io,215)cmap,rlatc,rlonc,truelat1,truelat2,
     &        x1dmn,y1dmn,dxy,nx,ny,nz
 215  format(a4,f9.4,f10.4,2f7.2,2f10.3,f8.3,2i4,i3,f9.4,f10.4)
c --- v2.0.0, Level 121203
      if(iosrf.eq.1)
     &   write(isrf,215)cmap,rlatc,rlonc,truelat1,truelat2,
     &        x1dmn,y1dmn,dxy,nx,ny,nz

C     Record #5
      inhyd=1
      imphys=nint(xgatts(4))+10
      icupa=nint(xgatts(10))+10
      ibltyp=nint(xgatts(9))+10

      ifrad=nint(xgatts(6))+10   ! short wave (SW) only
      isoil=nint(xgatts(8))+10
      ifddaan=nint(xgatts(22))
      ifddaob=nint(xgatts(23))
      ilandmm5=25                ! set to USGS-25, need more digging 
      idum=0
c --- v2.0.0, Level 121203
      write(io,216)inhyd,imphys,icupa,ibltyp,ifrad,isoil,
     &     ifddaan,ifddaob,(idum,i=1,12),ilandmm5
 216  format (30(i3))
c --- v2.0.0, Level 121203
      if(iosrf.eq.1)
     &   write(isrf,216)inhyd,imphys,icupa,ibltyp,ifrad,isoil,
     &     ifddaan,ifddaob,(idum,i=1,12),ilandmm5

C     Record #6
      imthr=nint(xgatts(13))
      jmthr=nint(xgatts(14))
      irmthr=nint(xgatts(15))
      write(io,217)idateb,nhours,nxsub,nysub,nzsub
 217  format(i10,i5,7i4)
c --- v2.0.0, Level 121203
      if(iosrf.eq.1)
     &   write(isrf,217)idateb,nhours,nxsub,nysub,nzsub

c     Record #7:ib,ie,jb,je,kb,ke
      rlonmin=999
      rlonmax=-999
      rlatmin=999
      rlatmax=-999
      do j=jb,je
         do i=ib,ie
            alat=xlat(i,j)
            alon=xlong(i,j)
            if(alat.gt.rlatmax) rlatmax=alat
            if(alat.lt.rlatmin) rlatmin=alat
            if(alon.gt.rlonmax) rlonmax=alon
            if(alon.lt.rlonmin) rlonmin=alon
         enddo
      enddo

      write(io,218)ib,jb,ie,je,kb,ke
     &      ,rlonmin,rlonmax,rlatmin,rlatmax
 218  format(6i4,2f10.4,2f9.4)
c --- v2.0.0, Level 121203
      if(iosrf.eq.1)
     &   write(isrf,218)ib,jb,ie,je,kb,ke
     &      ,rlonmin,rlonmax,rlatmin,rlatmax

c     Next nz records:sigma levels (normalized by 1013.0 hPa) 
      do k=kb,ke
         write(io,219)sig(k)
         if(iosrf.eq.1) write(isrf,219)sig(k)
      enddo
 219  format(f6.3)

c     Geophysical records. 
c     note: All at temp points (No-staged points)
      idum=-999
      fdum=-999.
      do j=jb,je
         do i=ib,ie
            flat=xlat(i,j)
            flon=xlong(i,j)
            ihh=nint(ter(i,j))
            ilu=nint(alu(i,j))

            write(io,220)i,j,flat,flon,ihh,ilu,fdum,fdum,idum
c --- v2.0.0, Level 121203
            if(iosrf.eq.1)
     &         write(isrf,220)i,j,flat,flon,ihh,ilu,fdum,fdum,idum
         enddo
      enddo
 220  format(2i4,f9.4,f10.4,i5,i3,1x,f9.4,f10.4,i5)

      ifirst=0

 6600 continue

c --- v2.0.0, Level 121203
C --- Don't output for idateb1
      if(idate .lt. idatenext) then
         goto 5000
      endif

c --- v2.0.0, Level 121203
      if(itm.eq.time1) then
         if(idate .gt. idatenext) then
            write(ilg,225)idate,idatenext
 225        format(' Error: Missing time in WRF Output: ',/
     &           ' In WRF ',i10.10,' Required: ',i10.10)
            stop
         endif
      endif

c --- v2.0.2, Level 131108
c --- Set grid-edge
      nedge=2
      jb_edge=MIN(jb,nedge)
      je_edge=MAX(je,ny-(nedge-1))
      ib_edge=MIN(ib,nedge)
      ie_edge=MAX(ie,nx-(nedge-1))

c --- v2.0.0, Level 121203
C --- QA first
C     Check 2D

c --- v2.0.2, Level 131108
c --- Store original values in 'pk' array
      x2dpk=x2d

      do ivar=1,10
         cinfo=cname2d(ivar)

         scale=1.0
         if(ivar.eq.1) scale=100.    !PS
         if(ivar.eq.2) scale=10.     !PRE
         if(ivar.eq.7) scale=1/1000. !QQ2
         alow=alim2d(1,ivar)*scale
         ahig=alim2d(2,ivar)*scale

         do j=jb,je
         do i=ib,ie

c --- v2.0.2, Level 131108
            aa=x2dpk(i,j,ivar)

            if(aa.lt.alow .or. aa.gt.ahig) then
               write(ilg,141)cinfo,i,j,ivar,alow,ahig,aa
               write(*,141)cinfo,i,j,ivar,alow,ahig,aa
 141           format('Warning - 2D Outrange: ',a8,2x,3i6,3E14.4)         
               xwrk=-99999.
               j1=j-1
               j2=j+1
               i1=i-1
               i2=i+1

c --- v2.0.2, Level 131108
c ---          Account for edge of domain
               j1=MAX(j1,jb_edge)
               j2=MIN(j2,je_edge)
               i1=MAX(i1,jb_edge)
               i2=MIN(i2,je_edge)
c ---          Set adjacent 4 values
               xwrk(1)=x2dpk(i1,j,ivar)
               xwrk(2)=x2dpk(i2,j,ivar)
               xwrk(3)=x2dpk(i,j1,ivar)
               xwrk(4)=x2dpk(i,j2,ivar)

               if(aa.lt.alow) then
                  id_lmt=1
               else
                  id_lmt=2
               endif

               npts=4
               call interph(xwrk,npts,alow,ahig,id_lmt,aintp)

               x2d(i,j,ivar)=aintp

               write(ilg,142)idate,cinfo,i,j,ivar,x2d(i,j,ivar)
               write(*,142)idate,cinfo,i,j,ivar,x2d(i,j,ivar)
 142           format(' Change to: ',i10,2x,a8,2x,3i6,3E14.4)

            endif

         enddo
         enddo

      enddo

c --- v2.0.0, Level 121203
C     Check 3D

c --- v2.0.2, Level 131108
c --- Store original values in 'pk' array
      x3dpk=x3d

      do ivar=1,12

c ---    Do not replace geopotential heights
         if(ivar.EQ.2) CYCLE

         cinfo=cname3d(ivar)
         scale=1.0
         if(ivar.ge.7) scale=1/1000. !QQ/water_content

         alow=alim3d(1,ivar)*scale
         ahig=alim3d(2,ivar)*scale
         
         do k=kb,ke
         
         do j=jb,je
         do i=ib,ie

c --- v2.01_x1.1, Level 131108
            aa=x3dpk(i,j,k,ivar)
                       
            if(aa.lt.alow .or. aa.gt.ahig) then
               xwrk=-99999.
               if(aa.lt.alow) then
                  id_lmt=1
               else
                  id_lmt=2
               endif

c --- v2.0.2, Level 131108
               if(ivar.le.7) then   
c ---             Horizontal interpolation for these variables
                  j1=j-1
                  j2=j+1
                  i1=i-1
                  i2=i+1

c --- v2.0.2, Level 131108
c ---             Account for edge of domain
                  j1=MAX(j1,jb_edge)
                  j2=MIN(j2,je_edge)
                  i1=MAX(i1,jb_edge)
                  i2=MIN(i2,je_edge)
c ---             Set adjacent 4 values
                  xwrk(1)=x3dpk(i1,j,k,ivar)
                  xwrk(2)=x3dpk(i2,j,k,ivar)
                  xwrk(3)=x3dpk(i,j1,k,ivar)
                  xwrk(4)=x3dpk(i,j2,k,ivar)
                  npts=4
               else
c ---             Vertical interpolation for these variables
                  k1=k-1
                  k2=k+1
                  k1=MAX(k1,1)
                  k2=MIN(k2,nz)
c ---             Set values from adjacent 2 layers
                  xwrk(1)=x3dpk(i,j,k1,ivar)
                  xwrk(2)=x3dpk(i,j,k2,ivar)
                  npts=2
               endif

               call interph(xwrk,npts,alow,ahig,id_lmt,aintp)

               x3d(i,j,k,ivar)=aintp

c ---         Update Warning format for readability.  Reduce screen output.
               adiff = abs(aa-aintp)  !TRC added adiff check (04-18-2013)
               if(adiff.gt.1.0e-04) then
                 write(ilg,240)idate,cinfo,i,j,k,ivar,alow,ahig,aa
                 write(*, 241) idate,cinfo,i,j,k,ivar,aa !TRC added k (04-17-2013)
 240             format('Warning - 3D Outrange: ',i10,
     &                  2x,a8,2x,4i6,3E14.4)
 241             format('Warning - 3D Outrange: ',i10,
     &                  2x,a8,2x,4i6,E14.4)
c                 print *,' In interph'
c                 print *,xwrk

c --- v2.0.2, Level 131108
                 write(ilg,242)idate,cinfo,i,j,k,ivar,x3d(i,j,k,ivar)
 242             format(' Change to: ',i10,2x,a8,2x,4i6,3E14.4)         
               endif         

            endif

         enddo
         enddo
      enddo
      enddo

C --- Output data records (3D)
      do 10 j=jb,je
      do 10 i=ib,ie

c ---    record header
C        Order in x2d (10): Ps,PRE,ISNOW,RSWD,RLWD,T2,Q2,WD10,WS10,SST       
         ps=x2d(i,j,1)/100.   ! change units to hPa from Pa
         pre=x2d(i,j,2)/10.   ! change units to cm from mm
         isnow=nint(x2d(i,j,3))
         if(isnow.ne.1) isnow=0

         rsw=x2d(i,j,4)
         rlw=x2d(i,j,5)
         tk2=x2d(i,j,6)
         qq2=x2d(i,j,7)*1000.   ! change units to g/kg from kg/kg
         qq2=max(qq2,0.)
         dir10=x2d(i,j,8)
         if(dir10.lt.0.05) dir10=360.
         spd10=x2d(i,j,9)
         sst=x2d(i,j,10)

         write(io,221)idate,i,j,ps,pre,isnow,rsw,rlw,tk2,qq2
     &        ,dir10,spd10,sst
 221     format(i10,2i3,f7.1,f5.2,i2,3f8.1,f8.2,3f8.1)

c ---    loop over all half-sigma layers (from surface to top)
C        Order in x3d (12): P,H,T,WD,WS,W,Q,Wc,Wr,Wi,Ws,Wg

c --- v2.0.0, Level 121203
C              Note: RH is not in x3d, but added in output
     
         do 20 k=kb,ke

            buff=blank

            prs=x3d(i,j,k,1)
            hh=x3d(i,j,k,2)
            ttk=x3d(i,j,k,3)
            idr=nint(x3d(i,j,k,4))
            if(idr.eq.0) idr=360
            spd=x3d(i,j,k,5)
            wpd=x3d(i,j,k,6)
            if(abs(wpd).lt.wpdmin) wpd=0.0

            qq=x3d(i,j,k,7)*1000.    ! change units to g/kg from kg/kg
            qq=max(qq,0.)

c            call getrh(prs,ttk,qq,rh)
            call getrh1(prs,ttk,qq,rh)  ! better at high altitude

            irh=nint(rh)

c --- v2.0.0, Level 121203
            if(irh.lt.1) irh=1
            if(irh.gt.100) irh=100

            ipre=nint(prs)
            izh=nint(hh)

C ---       First part: basic vars
            write(buff,222)ipre,izh,ttk,idr,spd,wpd,irh,qq
 222        format(i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2)

C ---       Second part: five varying vars (water contents)
            xcmp=0
            do ii=1,ncmp
               xcmp(ii)=x3d(i,j,k,ii+7)*1000. ! change unints to k/kg
            enddo

            npos=len_trim(buff)+1
            call wrtcmp(buff,npos,xcmp,ncmp)

            nt=len_trim(buff)
            write(io,'(a)')buff(1:nt)

 20      continue

 10   continue

c --- v2.0.0, Level 121203
C --- Output 2D
      do ivar=1,n2d2
         vname=blank
         vname=c2dlabels(ivar)(6:15)
         write(isrf,'(i10.10,2x,a8)')idate,vname

         scale=1.0
         if(trim(vname).eq.'PSFC') scale=1.0/100.
         if(trim(vname).eq.'RAIN') scale=1.0/10.
         if(trim(vname).eq.'Q2') scale=1000.
         if(trim(vname).eq.'RAINC') scale=1.0/10.
         if(trim(vname).eq.'RAINNC') scale=1.0/10.

         do j=je,jb,-1
            write(isrf,1015)(x2d(i,j,ivar)*scale,i=ib,ie)
         enddo
 1015    format(8f10.3)
      enddo

 5000 continue   ! end of itm (time)

      istat=NF_CLOSE(ncid)

c --- v2.0.0, Level 121203
      write(*,*)'Completed WRF file: ',ifile,' ',trim(fin)
      write(ilg,*)'Completed WRF file: ',ifile,' ',trim(fin)
      call stamptime(idate,iyr,imon,iday,ihour)
      call chgtim(iyr,imon,iday,ihour,1)
      call timestamp(iyr,imon,iday,ihour,idatenext)
      ifile=ifile+1
      if(ifile.le.nfiles) goto 2000

      deallocate(sig)
      deallocate(xlat,xlong,diff,alpha,alpha3,ter,alu,x2tmp)
      deallocate(u10,v10,u10t,v10t,wd10,ws10)
      deallocate(ppnow,ppold,ppcur)
      deallocate(ppc,ppcold,ppccur)
      deallocate(ppnc,ppncold,ppnccur)
      deallocate(pp,gh,gh2,tk)
      deallocate(uu,vv,uu2,vv2,uu2t,vv2t,wd,ws)

c --- v2.0.2, Level 131108
      deallocate(x3d,x2d,x3dpk,x2dpk)

      print*
      print*,' CALWRF succeeded'
      write(ilg,*)' CALWRF succeeded'
      
      stop 99999
      end

      include 'coordlib.for'
      include 'calwrf.blk'

C ------------------------------------------------------------------------
      Subroutine setup 

C ---  CALWRF   Version: 2.0.3        Level: 130418
C
C Purpose:
C     Setup CALWRF with calwrf control file
c
c --- UPDATES:
c
c --- Version 1.4 (070627) to Version 2.0.0 (121203)
c      1. Implement 2D.DAT output file
C
C Zhong-Xiang Wu
C 6/27/2007

      include 'calwrf.par'
      include 'calwrf.cm1'

      character*80 ftmp

      numarg=iargc()
      if(numarg == 0)then
         fct='calwrf.inp'
      else
         call getarg(1,fct)
      endif

      print *,' Control inp file:',trim(fct)

      open(ict,file=fct,status='old',action='read')

      read(ict,101)title
 101  format(a)

      call getstr(ict,flg)

C --- Open log file
      open(ilg,file=flg,status='unknown',action='write')
      write(ilg,1010)codever,codelevel
 1010 format(' CALWRF Code ----- Version:',a16,' Level:',a16)

C --- Echo control info to log file
      call putstr(ilg,title)
      call putstr(ilg,fct)
      call putstr(ilg,flg)

      call getstr(ict,fio)
      call putstr(ilg,fio)

      read(ict,*)ib,ie,jb,je,kb,ke
      read(ict,*)idateb
      read(ict,*)idatee

      write(ilg,1011)ib,ie,jb,je,kb,ke
      write(ilg,1012)idateb,idatee
 1011 format(' Beg/End I/J/K:',6i6)
 1012 format(' Beg/End Dates:',i12)

      read(ict,*)nfiles
      write(ilg,*)' WRF Output files:',nfiles

      if(nfiles.ge.1) then
         do i=1,nfiles
            call getstr(ict,fin)
            flnames(i)=fin
            write(ilg,1013)i,trim(flnames(i))
         enddo
 1013    format(i4,' WRF File: ',a)
      else
         print *,' No WRF File: ',nfiles
         stop
      endif

c --- v2.0.0, Level 121203
C     2D.DAT flag
      read(ict,*,err=2000,end=2000)iosrf
      if(iosrf.eq.1) then
         call getstr(ict,fsrf)
         write(ilg,1014)trim(fsrf)
      endif
 1014 format(' 2D.DAT file name:',a)

      if(iosrf.eq.1) goto 2050

C --- Set 2D.DAT flag to 1 if no input for this flag
 2000 print *,' 2D.DAT flag input not exist, set to one'

c      iosrf=0
c      print *, ' Set 2D.DAT flag to zero (no 2D.DAT):',iosrf
C      Set iosrf=1, and use same m3d file name except extension "m2d"
      iosrf=1
      nt=index(fio,'.',BACK=.TRUE.)
      fsrf=fio(1:nt)//'m2d'

      print *, ' Set 2D.DAT flag to 1:',iosrf
      print *, ' Default 2D.DAT filename:',trim(fsrf)

      write(ilg,*)' Set 2D.DAT flag to 1:',iosrf
      write(ilg,*)' Default 2D.DAT filename:',trim(fsrf)

 2050 close(ict)

      write(ilg,*)' IOSRF (0-No 2D.DAT, 1-2D.DAT created):',
     &            iosrf

      open(io,file=fio,status='unknown',action='write')
c --- v2.0.0, Level 121203
      if(iosrf.eq.1)      
     &   open(isrf,file=fsrf,status='unknown',action='write')

C --- Initialize blank strings
      do i=1,80
         blank(i:i)=' '
      enddo
      do i=1,132
         blank132(i:i)=' '
      enddo

C --- Initialize variable name
      v3dnames(1)='P'
      v3dnames(2)='PH'
      v3dnames(3)='T'
      v3dnames(4)='U'
      v3dnames(5)='V'
      v3dnames(6)='W'
      v3dnames(7)='QVAPOR'
      v3dnames(8)='QCLOUD'
      v3dnames(9)='QRAIN'
      v3dnames(10)='QICE'
      v3dnames(11)='QSNOW'
      v3dnames(12)='QGRAUP'

      v2dnames(1)='PSFC' 
      v2dnames(2)='RAINNC'
      v2dnames(3)='SNOWC'
      v2dnames(4)='SWDOWN'
      v2dnames(5)='GLW'
c      v2dnames(5)='OLR'
      v2dnames(6)='T2'
      v2dnames(7)='Q2'
      v2dnames(8)='U10'
      v2dnames(9)='V10'
      v2dnames(10)='SST'

      v2dnames(11)='TSK'
      v2dnames(12)='PBLH'
      v2dnames(13)='HFX'
      v2dnames(14)='LH'
      v2dnames(15)='UST'

      v2dnames(16)='RAINC'
      v2dnames(17)='RAINNC'   ! Same as v2dname(2)

C --- Date/Time
      call date_and_time(cdate,ctm,czone,vdate)

      return
      end

C ---------------------------------------------------------------
      Subroutine getvars (ftmp,vnames,nn,blank)

C May not needed
C ---  CALWRF   Version: 2.0.3        Level: 070627
C
C Purpose:
C     Get variable names
C
C Zhong-Xiang Wu
C 6/27/2007

      character*80 ftmp,buff,blank
      character*20 vnames(nn)
      character*1 c1

      parameter(itmp=1)
C
C --- Read 3D-variable names and its data type

      open(itmp,file=ftmp,status='old',action='read')
      read(itmp,*)  ! skip title line

      ip=0

 1000 buff=blank
      read(itmp,101,end=2000)buff
 101  format(a)

      c1=buff(2:2)
      if(c1.eq.'f' .or. c1.eq.'i') then
         ip=ip+1
         nt1=index(buff,' ')+1
         nt2=index(buff,'(')-1
         nt3=index(buff,')')-1

         vnames(ip)=buff(nt1:nt2)
      endif
      goto 1000

 2000 close(itmp)

      if(ip.ne.nn) then
         print *,' Warning: in GETVAR: Not all vars available'
         print *,' Required Vars:',ip,'  In WRF Output:',nn
      endif

      return
      end

C -----------------------------------------------------------------
      Subroutine getstr(in,cstr)

C ---  CALWRF   Version: 2.0.3        Level: 121203
C
C Purpose:
C     Get a string from input file
c
c --- UPDATES:
c
c --- Version 1.4 (070627) to Version 2.0.0 (121203)
c      1. Initialize string variable CSTR to blank
C
C Zhong-Xiang Wu
C 6/27/2007

      character*132 cstr
      character*1 ckey

      data ckey/' '/

      do i=1,132
         cstr(i:i)=' '
      enddo

      read(in,'(a)')cstr
      nt=index(cstr,ckey)-1
      
      if(nt.lt.1) then
         print *,'Error in getstr: nt=',nt
         stop
      endif

      cstr=cstr(1:nt)

      return
      end

C -----------------------------------------------------------------
      Subroutine putstr(io,cstr)

C ---  CALWRF   Version: 2.0.3        Level: 070627
C
C Purpose:
C     Put a string to output file
C
C Zhong-Xiang Wu
C 6/27/2007

      character*132 cstr

      write(io,'(a)')trim(cstr)

      return
      end

C -----------------------------------------------------------------
      Subroutine cvtdate(ctime,idate,isecs)

C ---  CALWRF   Version: 2.0.3        Level: 070627
C
C Purpose:
C     Convert a time string to idate format
C
C Zhong-Xiang Wu
C 6/27/2007

C     ctime: 2000-01-25_00:00:00
C     idate: 2000012500 (YYYYMMDDHH)
C     isecs: second (0-3600)

      character*80 ctime
      integer idate

      read(ctime,1010)iyr,imn,idy,ihr,imm,isec
 1010 format(i4.4,5(1x,i2.2))

      call timestamp(iyr,imn,idy,ihr,idate)
      call mm2sec(imm,isec,isecs)

      return
      end

! --------------------------------------------------
      Subroutine mm2sec(imm,isec,isecs)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           

! --- PURPOSE:
!     Get seconds from minutes

      isecs=isec+imm*60

      Return
      end

! --------------------------------------------------
      Subroutine timestamp(iyr,imn,idy,ihr,ndate)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           

! --- PURPOSE:
!     Get time stamp in YYYYMMDDHH format

      ndate=ihr+idy*100+imn*10000+iyr*1000000

      return
      end

! --------------------------------------------------------------------
      subroutine stamptime(ndate,iyr,imon,iday,ihour)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Get year,month,day and hour from a time stamp

      parameter(ifour=10000,isix=1000000)

      iyr=ndate/isix
      imon=ndate/ifour-iyr*100
      iday=ndate/100-iyr*ifour-imon*100
      ihour=ndate-iyr*isix-imon*ifour-iday*100
      
      return
      end

! **********************************************************************
      subroutine chgtim(iyr,imon,iday,ihour,idt)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Increase or decrease hours. 
!     Used to convert Local Stand Time to GMT or to setup time stamp for loop

      parameter(nmonth=12)

      dimension ndays(nmonth)

      data ndays/31,28,31,30,31,30,31,31,30,31,30,31/

      if(mod(iyr,4).eq.0) then
         ndays(2)=29 
      else
         ndays(2)=28
      endif

      ihour=ihour+idt

      if(idt.lt.0) goto 2000

 1000 if(ihour.gt.23) then
         ihour=ihour-24
         iday=iday+1
         
         if(iday.gt.ndays(imon)) then
            iday=1
            imon=imon+1
            
            if(imon.gt.12) then
               imon=1
               iyr=iyr+1
               if(iyr/4 .eq. iyr/4.0) then
                  ndays(2)=29 
               else
                  ndays(2)=28
               endif
            endif
         endif
         goto 1000
      else
         return
      endif

 2000 continue

 3000 if(ihour.lt.0) then
         ihour=ihour+24
         iday=iday-1
         
         if(iday.le.0) then
            imon=imon-1
            if(imon.le.0) then
               iyr=iyr-1
               imon=12
               if(iyr/4 .eq. iyr/4.0) then
                  ndays(2)=29 
               else
                  ndays(2)=28
               endif
            endif
   
            iday=ndays(imon)
         endif
         goto 3000
      else
         return
      endif

      end

C ----------------------------------------------------------------
      Subroutine clean(cstr)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Clean string

      character cstr*80
      character c1

      c1=' '
      ic1=ichar(c1)
      c1='~'
      ic2=ichar(c1)

      do i=80,1,-1
         c1=cstr(i:i)
         k=ichar(c1)
         if(k.ge.ic1 .and. k.le.ic2) then
            return
         else
            c1=' '
         endif
         cstr(i:i)=c1
      enddo
      
      return
      end

C ----------------------------------------------------------------------

      Subroutine pass2d(x2d,data,nx,ny,n2d,ipos)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Pass 2D variables to a master array

      dimension x2d(nx,ny,n2d)
      dimension data(nx,ny)

      do j=1,ny
         do i=1,nx
            x2d(i,j,ipos)=data(i,j)
         enddo
      enddo

      return
      end

C ----------------------------------------------------------------------

      Subroutine pass3d(x3d,data,nx,ny,nz,n3d,ipos)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Pass 3D variables to a master array

      dimension x3d(nx,ny,nz,n3d)
      dimension data(nx,ny,nz)

      do k=1,nz
         do j=1,ny
            do i=1,nx
               x3d(i,j,k,ipos)=data(i,j,k)
            enddo
         enddo
      enddo

      return
      end

C ----------------------------------------------------------------------
      Subroutine interpz(xx1,xx2,nx,ny,nz)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Vertical interpolation for 3D variables

      dimension xx1(nx,ny,nz+1)
      dimension xx2(nx,ny,nz)

      do j=1,ny
         do i=1,nx
            do k=1,nz
               k1=k+1
               xx2(i,j,k)=(xx1(i,j,k)+xx1(i,j,k1))/2.0
            enddo
         enddo
      enddo

      return
      end

C ----------------------------------------------------------------------
      Subroutine interpu(xx1,xx2,nx,ny,nz)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     U-Interpolation for 3D variables

      dimension xx1(nx+1,ny,nz)
      dimension xx2(nx,ny,nz)

      do k=1,nz
         do j=1,ny
            do i=1,nx
               i1=i+1
               xx2(i,j,k)=(xx1(i,j,k)+xx1(i1,j,k))/2.0
            enddo
         enddo
      enddo

      return
      end

C ----------------------------------------------------------------------
      Subroutine interpv(xx1,xx2,nx,ny,nz)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     U-Interpolation for 3D variables

      dimension xx1(nx,ny+1,nz)
      dimension xx2(nx,ny,nz)

      do k=1,nz
         do i=1,nx
            do j=1,ny
               j1=j+1
               xx2(i,j,k)=(xx1(i,j,k)+xx1(i,j1,k))/2.0
            enddo
         enddo
      enddo

      return
      end

C -------------------------------------------------------------------
      Subroutine gblatts

! ---  CALWRF   Version: 2.0.3        Level: 190429
!
! --- Zhong-Xiang Wu           
!
c --- Version 2.0.2, Level 131108 to Version 2.0.3, Level 090914
c     1. Include WRF Version 4 output files
c --- Version 1.1, Level 080429 to Version 1.2, Level 090914
c     1. Include WRF Version 3 output files
c
C     Zhong-Xiang Wu
c     9/14/2009

! --- PURPOSE:
!     Extract Global Attributes

      include 'calwrf.par'
      include 'calwrf.cm1'
      include 'calwrf.cm2'

      vattnames(1)='DX'
      vattnames(2)='DY'
      vattnames(3)='FEEDBACK' !'DYN_OPT'
      vattnames(4)='MP_PHYSICS'
      vattnames(5)='RA_LW_PHYSICS'
      vattnames(6)='RA_SW_PHYSICS'
      vattnames(7)='SF_SFCLAY_PHYSICS'
      vattnames(8)='SF_SURFACE_PHYSICS'
      vattnames(9)='BL_PBL_PHYSICS'
      vattnames(10)='CU_PHYSICS'
      vattnames(11)='GRID_ID'
      vattnames(12)='PARENT_ID'
      vattnames(13)='I_PARENT_START'
      vattnames(14)='J_PARENT_START'
      vattnames(15)='PARENT_GRID_RATIO'
      vattnames(16)='CEN_LAT'
      vattnames(17)='CEN_LON'
      vattnames(18)='TRUELAT1'
      vattnames(19)='TRUELAT2'
      vattnames(20)='STAND_LON'
      vattnames(21)='MAP_PROJ'
      vattnames(22)='GRID_FDDA'
      vattnames(23)='OBS_NUDGE_OPT'
      vattnames(24)='MOAD_CEN_LAT'

      print*," "
      print*,"Processing GLOBAL ATTRIBUTES:"
      print*," "

C --- Check valid files
      att_name='TITLE'
      value_chr=blank
      istat=NF_GET_ATT_TEXT(ncid,nf_global,att_name,value_chr)
      istat=NF_INQ_ATT(ncid,nf_global,att_name,ivtype,attlen)

      !if(INDEX(value_chr,'OUTPUT FROM WRF V2') == 0)then
      !   if(INDEX(value_chr,'OUTPUT FROM WRF V3') == 0)then
      !     if(INDEX(value_chr,'OUTPUT FROM WRF V4') == 0)then
      !     !! diagnostics only available for wrfout data
      !     print*,"This is not a wrfout file "
      !     print*,"No 3D.DAT will be created"
      !     stop
      !    endif
      !  endif
      !endif

      nt1=INDEX(value_chr,'WRF')
      nt2=len_trim(value_chr)
      
      nt1=nt1+4
      nt2=nt2-6
      cmodel_ver=value_chr(nt1:nt2)

C --- Check GRIDTYPE first
      att_name='GRIDTYPE'
      value_chr=blank
      istat=NF_GET_ATT_TEXT(ncid,nf_global,att_name,value_chr)
      istat=NF_INQ_ATT(ncid,nf_global,att_name,ivtype,attlen)

      if(att_name.eq.'GRIDTYPE' .and. 
     &     value_chr(1:attlen).ne.'C') then
         write(ilg,102)value_chr(1:attlen)
         print 102,value_chr(1:attlen)
 102     format(' GridType other than C is not coded:',a) 
         stop
      endif

C --- Extract attributes
      do iatt=1,ngatts
         att_name=vattnames(iatt)
         istat=NF_INQ_ATT(ncid,nf_global,att_name,ivtype,attlen)
         if(istat.ne.NF_NOERR) then
            write(ilg,1031)iatt,trim(att_name),istat
            print 1031,iatt,trim(att_name),istat
 1031       format(' Warning: Attribute not exist:',i5,2x,a,2x
     &             ,i10,/,' Check whether this att. is critical')
            goto 1050
         endif

         if (ivtype .lt. 3) then
            istat=NF_GET_ATT_TEXT(ncid,nf_global,att_name,value_chr)
            write(ilg,103)att_name(1:40),value_chr(1:attlen)
            print 103,att_name(1:40),value_chr(1:attlen)
 103        format(' Attribute other than numerical is not coded:',/
     &             ,a,' : ',a) 
            stop
         elseif(ivtype == 4) then
            istat=NF_GET_ATT_INT(ncid,nf_global,att_name,value_int)
            xgatts(iatt)=value_int   ! convert to real
         elseif (ivtype == 5) then
            allocate (value_real(attlen))
            istat=NF_GET_ATT_REAL(ncid,nf_global,att_name,value_real)
            xgatts(iatt)=value_real(1)   ! chk position >=2 if needed
            deallocate (value_real)
         else
            print *,' Attribute for type: ',i3,' not coded'
            stop
         endif

 1050    continue

      enddo

C --- Map project cone factor
      op_rot=.TRUE.

      istat=NF_GET_ATT_INT(ncid, nf_global, "MAP_PROJ", map_proj)
      if(map_proj .ge. 3) op_rot=.FALSE. ! mercator, no need to rotate

      if(op_rot)then
 
      istat=NF_GET_ATT_REAL(ncid, nf_global, "TRUELAT1", truelat1)
      istat=NF_GET_ATT_REAL(ncid, nf_global, "TRUELAT2", truelat2)
      istat=NF_GET_ATT_REAL(ncid, nf_global, "STAND_LON", stand_lon)

      if(map_proj .eq. 2)cone=1.
      if(map_proj .eq. 1)then
        IF (ABS(truelat1-truelat2) .GT. 0.1) THEN
           cone=(ALOG(COS(truelat1*RPD))- ALOG(COS(truelat2*RPD))) /
     &           (ALOG(TAN((90.-ABS(truelat1))*RPD*0.5))- 
     &           ALOG(TAN((90.-ABS(truelat2))*RPD*0.5)))
        ELSE
           cone=SIN(ABS(truelat1)*RPD)
        ENDIF
      endif 

      endif

      write(ilg,*)' truelat1,truelat2,stand_lon,cone,op_rot:'
      write(ilg,*)truelat1,truelat2,stand_lon,cone,op_rot

C --- Get descriptions of2D varaibles
      do iatt=1,n2d2
         vname=v2dnames(iatt)
         istat=NF_INQ_VARID(ncid, vname, id_var)

         if(istat.ne.0) then
            write(ilg,1033)trim(vname)
            print 1033,trim(vname)
 1033       format(' 2D-Variable not found: ',a)
            goto 1060
         else
            att_name="description"
            value_chr=blank
            istat=NF_GET_ATT_TEXT(ncid,id_var,att_name,value_chr)
            istat=NF_INQ_ATT(ncid,nf_global,att_name,ivtype,attlen)
            c2ddscp=value_chr(1:attlen)
            attlen1=attlen

            att_name="units"
            value_chr=blank
            istat=NF_GET_ATT_TEXT(ncid,id_var,att_name,value_chr)
            istat=NF_INQ_ATT(ncid,nf_global,att_name,ivtype,attlen)
            c2dunits=value_chr(1:attlen)
            attlen2=attlen

C           Change some 2D name,description and unists
            if (iatt.eq.1) c2dunits='hPa'
            if (iatt.eq.2) then
               c2dunits='cm'
               vname='RAIN'
               c2ddscp='TOTAL PRECIPITATION'
            endif

            if (iatt.eq.3) c2dunits='none'
            if (iatt.eq.7) c2dunits='g kg-1'

            if (iatt.eq.8) then
               c2dunits='degree'
               vname='WD10'
               c2ddscp='WIND DIRECTION AT 10 M'
            endif

            if (iatt.eq.9) then
               vname='WS10'
               c2ddscp='WIND SPEED AT 10 M'
            endif

            if (iatt.eq.16) then
               c2dunits='cm'
               c2ddscp='CUMULUS PRECIPITATION'
            endif
            if (iatt.eq.17) then
               c2dunits='cm'
               c2ddscp='GRID SCALE PRECIPITATION'
            endif

            write(c2dlabels(iatt),1034)iatt,vname(1:10),c2dunits
     &                ,trim(c2ddscp)
 1034       format(i3,2(2x,a10),2x,a)

         endif
         
 1060    continue

      enddo

      return
      end

c --------------------------------------------------------------
	subroutine gethours(ndate1,ndate2,nhrs)

C --- CALWRF   Version: 2.0.3        Level: 061130

c Purpose:
C     Get hours between to dates
C
C --- Changes from Version 1.1 Level 060910 to Version 1.2 Level 061130
C     1. Change gethours subroutine to account for leap years

	if(ndate2.lt.ndate1) then
	   print *,'Error in gethours: ndate2 < ndate1'
	   print *,ndate1,ndate2
	   stop
	endif

        io=6
 	call stamptime(ndate1,iyr1,imon1,iday1,ihour1)
	call julday(io,iyr1,imon1,iday1,jday1)
	call stamptime(ndate2,iyr2,imon2,iday2,ihour2)
	call julday(io,iyr2,imon2,iday2,jday2)

        call deltt(iyr1,jday1,ihour1,iyr2,jday2,ihour2,nhrs)

        nhrs=nhrs+1   ! deltt is one hour less total hours

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
      function esat(tdegc)
c----------------------------------------------------------------------
c
c --- CALMET   Version: 6.211       Level: 901130                    ESAT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Compute the saturation water vapor pressure (mb) using
c               the method of Lowe (1977) (JAM, 16, pp 100-103).
c
c --- INPUT:
c                TDEGC - real          - Air temperature (deg. C)
c
c --- OUTPUT:
c                 ESAT - real          - Saturation water vapor
c                                        pressure (mb)
c
c --- ESAT called by: WATER
c --- ESAT calls:     none
c----------------------------------------------------------------------
      data a0/6.107799961/,a1/4.436518521e-1/,a2/1.428945805e-2/
      data a3/2.650648471e-4/,a4/3.031240396e-6/,a5/2.034080948e-8/
      data a6/6.136820929e-11/
c
c --- compute saturation water vapor pressure (mb)
c --- NOTE: temperature is in deg. C
      esat=a0+tdegc*(a1+tdegc*(a2+tdegc*(a3+tdegc*(a4+tdegc*
     1 (a5+tdegc*a6)))))
c
      return
      end

c ------------------------------------------------------------
      subroutine getrh(p,tk,qq,rh)

! ---  CALWRF   Version: 2.0.3        Level: 070627

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Get relative humidity

c --- inputs:
c           p - real      - pressure (mb)
c           tk - real     - temperature (K)
c           qq - real     - mixing ratio (g/kg)
c
c --- output:
c           tv - real      - virtual temperature(K)
c           rh - real      - relative humidity              
c
c----------------------------------------------------------------------
      tc=tk-273.15

      es0=esat(tc)
      
      qq2=qq/1000.0    ! qq2 in g/g
      ee=p*qq2/(0.622+0.378*qq2)

      rh=ee/es0*100.

      rh=min(rh,100.)
      rh=max(rh,0.)
      
      return
      end

C ---------------------------------------------------
      subroutine getrh1(p,tk,qq,rh)

! ---  CALWRF   Version: 2.0.3        Level: 080429

! --- Zhong-Xiang Wu           

c --- inputs:
c           p - real      - pressure (mb)
c           tk - real     - temperature (K)
c           qq - real     - mixing ratio (g/kg)
c
c --- output:
c           rh - real      - relative humidity              

      qq2=qq/1000.    ! change g/kg to kg/kg

      qsat=0.622 *6.112/p
     :     *exp(17.67* (tk-273.15)/(tk-29.65))
      rh=max(100.*qq2/qsat,0.)
      if(rh.gt.100.) rh=100.

      return
      end

c -------------------------------------------------------------------
      subroutine wrtcmp(buff,npos,xcmp,ncmp)

! ---  CALWRF   Version: 2.0.3        Level: 080429

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Output water contents in compressed format

C Changes from Version: V1.0 Level: 070627 to Version: 1.1 Level: 080429 
C     1. No compression if ncmp=1

      character*80 buff

      parameter (xzero=0.00049)

      include 'calwrf.par'
      dimension xcmp(mxcmp)

      if(ncmp.lt.1 .or. npos.lt.1 .or. npos.gt.80) then
         print *,'Compression Error in wrtcmp:  NCMP < 1'
         print *,'NCMP/NPOS = ',ncmp,npos
         stop
      endif

c      xcmp=max(xcmp,+0.)
      do i=1,ncmp
         if(xcmp(i).le.0) xcmp(i)=0
      enddo

      idcmp=1

C --- No compression if only one value (Some WRF output from BB)
      if(ncmp.le.1) then
         idcmp=0
         goto 1000
      endif

      do i=1,ncmp
         if(xcmp(i).ge.xzero) then
            idcmp=0
            goto 1000
         endif
      enddo

 1000 continue

      if(idcmp.eq.1) then
         write(buff(npos:),101)float(-ncmp)
      else
         write(buff(npos:),101)(xcmp(i),i=1,ncmp)
         do i=1,ncmp
            if(xcmp(i).lt.0) then
               write(18,*)i,xcmp(i)
               write(18,181)i,xcmp(i)
 181           format(i5,f6.3)
            endif
         enddo
      endif
 101  format(5f6.3)

      return
      end

C -------------------------------------------------------------

      Subroutine interph(xwrk,npts,alow,ahig,id_lmt,aintp)

! ---  CALWRF   Version: 2.0.3        Level: 131108

! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Interplate outrange valuses
!
! --- UPDATES:
! --- From V2.0.1 (121203) to V2.0.2 (131108)
!     Remove option for selecting a replacement method that uses
!     the range-check limits alone

      dimension xwrk(4)

      bb=0
      ip=0
      do i=1,npts
         aa=xwrk(i)
         if(aa.ge.alow .and. aa.le.ahig) then
            bb=bb+aa
            ip=ip+1
         endif
      enddo

      if(ip.gt.0) then
         aintp=bb/ip
      else
         if(id_lmt.eq.1) then
            aintp=alow
         else
            aintp=ahig
         endif
      endif

      return
      end


