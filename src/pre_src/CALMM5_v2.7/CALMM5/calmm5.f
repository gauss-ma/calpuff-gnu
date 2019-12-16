C ---------------------------------------------------------------------
      PROGRAM CALMM5
C (CALMM5 - V2.7 L061030)
C (No NETCDF and VIS5D options of 4,5)
C
C --------------------------------------------------------------------
C --- CALMM5 -- Read in MM5 Version 3 output directly and convert it 
C          to CALMET 3D.DAT/2D.DAT or MM4 input formats, or GrADS, Vis5D,
C          NetCDF, and GRIB formats.  
C
C --- CALMM5   Version: 2.7   Level: 061030                        Main
C
c --- Written by: Francoise Robe (02/03/98)
c                 Zhong-Xiang Wu (12/31/98)
c                 Earth Tech
c                 196 Baker Ave., Concord, MA
c
c --- REMARKS: - This version can not read MM5 outputs in Verions 2 format
c              - As MM5 uses a staggered C grid, MM5 scalar variables are 
c                interpolated to the mesh corners where MM5 velocities are
c                computed. All variables in 3D.DAT and 2D.DAT (input to 
c                CALMET) are thus colocated except landuse.
c        
c -----------------------------------------------------------------------
c --- Version 2.6, Level 060330 to Version 2.7, Level 061030 (Minor)
c --- Modifications by Zhong-Xiang Wu
C     10/30/2006
C     1. Update CALUTILS from V2.2 Level 030508 to V2.54 Level 061020
C     2. Update COORDLIB from V1.95 Level 050126 to V1.97 Level 060626
C
c --- Version 2.5, Level 050607 to Version 2.6, Level 060330 (Minor)
c --- Modifications by Zhong-Xiang Wu
C     3/30/2006
C     1. Fix a bug for taking off duplicated hours in cases of 
C        using multiple MM5 output files from different
C        MM5 runs with overlapping hours.  The changes made in 
C        Version 2.3 only apply multiple files from one MM5 runs.
C     2. Add checks for time consistency between current and previous 
C        hour
C
c --- Version 2.4, Level 050413 to Version 2.5, Level 050607 (Minor)
c --- Modifications by Zhong-Xiang Wu
C     6/7/2005
c     1. Fix a bug for output of more hours than required when 
c        multi-MM5 output files are used
c     2. Define c4hem in Sub lateql
c     3. Use sub. deltt to replace nhour calculation
c     4. Add version level number to subs. Set level number to 999999 if
c        can not determined
c     5. Account files not properly ended.
c  
c --- Version 2.31, Level 050314 to Version 2.4, Level 050413 (Minor)
c --- Modifications by Zhong-Xiang Wu
C     4/13/2005
c     1. Fix a bug for calculation of total hours across lead year

c --- Version 2.3, Level 041109 to Version 2.31, Level 050314 
c --- Modifications by D. Strimaitis
c     1. COORDLIB updated from Version 1.92, Level 031201
c                           to Version 1.95, Level 050126
c
c --- Version 2.2, Level 040921 to Version 2.3, Level 041109 
c --- Modifications by Zhong-Xiang Wu & M Phadnis
C     11/9/2004
c     1. Add ability to read multiple MM5 binary files
c        Need new calmm5.inp control file for multiple MM5 file, but
c        previoud calmm5.inp still works if one MM5 input file
c        is used
C   
c --- Version 2.1, Level 040112 to Version 2.2, Level 040921
c     Modifications by M Phadnis
c     9/21/2004
C     1. Use F6.3 nstead of 5.2 for water-related profile coulmns.
C     2. Include subsoutines of calutils and coordlib 
c 
c --- Version 2.0, Level 021111 to Version 2.1, Level 040112
c     Modifications by Zhong-Xiang Wu
c     1/12/2004 (Major changes)
c
c     (1) Add comment lines in the headers of output 3D.DAT/2D.DAT file.
c         The new headers are consistent with the standard data structure
c         used in CALMET/CALPUFF files. The output 3D.DAT/2D.DAT format
c         is Version 2.0, Level 040112. MAIN and SETUPMM5 updated.
c     (2) Fix a bug in relative humidity calculation in Sub. PROCESS.
c         PROCESS updated to Version 2.1, Level: 040112
c     (3) Set 3D.DAT/2D.DAT data format from this code verion to 
c         Version 2.0, assumming previous data in Version 1.XX (JSS, 1/13/2004)
c
c     11/11/2002 (Major changes)
c --- Made by Zhong-Xiang. Wu
c --- Version 1.1, Level 020322 to Version 2.0, Level 021111 (Major version)
c     Data format Version numbers changes from 3.0 to 3.1
c     1. Read MM5 output in Version 3 format only.  This version can not 
c        read V2 format
c     2. Add temperature and specific humidity at 2 m and U/V at 10 m,
c        and SST to the header of data record
c     3. Add polar stereographic and mercator map projections
c     4. Correct an error in the vertical interpolation of velocity. The
c        vertical index of W at U/V layer was shifted downward by one layer.
c     5. Clean sea level pressure calculations.  
c     6. Add landuse category type at the Record #5.
c     
c     10/22/2002 (Minor changes)
c --- Made by Zhong-Xiang. Wu
c --- Version 1.1, Level 020322 to Version 1.1, Level 021022 (Minor revision)
c     1. Add SST (Ground T for land) on data record
c
c     03/22/2002 (Minor changes)
c --- Made by Zhong-Xiang. Wu
c --- Version 1.1, Level 020123 to Version 1.1, Level 020322 (Minor revision)
c     1. Revised the time-checking procedure.  MM5V35 fixed the time 
c        stamp output problem. 
c
c     01/23/2002 (Minor changes)
c --- Made by Zhong-Xiang. Wu
c --- Version 1.1, Level 011221 to Version 1.1, Level 020123 (Minor revision)
c     1. Correct an error of time stamp in TYCO MM5 GRIB files.
c        In Version 3, minutes is included in date stamp, but not 
c        considered in Version 2.

c     12/21/2001 (Major changes)
c --- Made by Zhong-Xiang. Wu
c --- Version 1.1, Level 011108 to Version 1.1, Level 011221 (Major revision)
c     1. Add options of grid re-configuration for GRIB format (Format 6). 
c        New grid configuration can be in equal lat/lon form with original 
c        MM5 resolution, or in a user-specified resolution (in DEGREE) IF 
c        the original MM5 resolution is SMALLER than the user-specified 
c        resolution (no effect on original MM5 resolution larger than the
c        user-specfied one).
c        A input new line, including three input values, are needed at the end
c        of orginal calmm5.inp file for GRIB format.
c     2. Fix a bug for longitude interpolation across date-line
c
c     11/8/2001 (Minor changes)
c --- Made by Zhong-Xiang. Wu
c --- Version 1.1, Level 010717 to Version 1.1, Level 011108 (Minor revision)
c     1. No grib records for a array with all ZERO data to Correct 
c        an over-float problem when all data are zeros.
c
c     7/17/2001 (Major changes)
c --- Made by Zhong-Xiang. Wu
c --- Version 1.1, Level 010402 to Version 1.1, Level 010717 (Major revision)
c     1. Change grib output format for TYCO team needs (SLP, U10/V10 output
c        in TYCO format)
c     2. Add output of 2-D U/V at 10 m above the ground for MM5 V3.4 with
c        IBLTYP=5 (MRF option)
c     3. Add options of output temperature and mixing ratio at 2 meters
c     4. Add SWOUT, LWOUT output flags to CALMM5 M3D header and data 
c        record header (After MM5 V3.4)
c
c     4/02/2001 (Minor changes)
c --- Made by Zhong-Xiang. Wu
c --- Version 1.1, Level 010116 to Version 1.1, Level 010402 (Minor revision)
c     1. Correct a date output format error in MM4.DAT when 4-digit year is
c        used. Still use 2-digit for year in MM4.DAT
c     2. Covert YY to YYYY if control input date is in YY format

c     1/16/2001 (Minor changes)
c --- Made by Zhong-Xiang.
c --- Version 1.1, Level 001215 to Version 1.1, Level 010116 (Minor revision)
c     1. Move X/Y/Z range checks before sigma loop in subroutine hdproc.
c        to correct misleading debugging information about out-range error.
c     2. Elevation at cross point is added to the end of geographical record
c        of each selected grid in M3D and M2D headers. Note that the cross
c        point elevations at up and right boundaries should not be used since
c        the number of cross points is one less than that of dot points. 
c
c --- Version 1.1, Level 001115 to Version 1.1, Level 001215 (Major revision)
c --- Made by Zhong-Xiang. Wu on 12/15/2000
c     1. Add GRIB option to output format
c     2. Move vertical extraction range of nz1 and nz2 to the last two 
c        records of inp file to minimize the changes of inp file, since 
c        nz1,nz2 are only for CALMM5/MM53D format since Level 000817.
c 
c --- Version 1.1, Level 000905 to Version 1.1, Level 001115 (Minor revision)
c --- Made by Zhong-Xiang. Wu on 11/15/2000
c     1. Reverse Y-order in M2D output so that J=1 is at south (bottom) part
c        of output array. Orignal J=1 was in the north (top) part of output 
c        array.
c
c --- Version 1.1, Level 000817 to Version 1.1, Level 000905 (Minor revision)
c --- Made by Zhong-Xiang. Wu on 8/17/2000
c     1. Convert Version 2 idate in YY to YYYY form to account for runs
c        across 2000
c
c --- Version 1.1, Level 000726 to Version 1.1, Level 000817 (Major revision)
c --- Made by Zhong-Xiang. Wu on 8/17/2000
c     Note: Revision is needed in CALMET to read the calmm5.dat 
c           output from V1.1 since the output format (both in header
c           and data records) has been changed.
c     (1) Create CALMM5/MM5 in M3D.dat format. 
c     (2) Add option to output 2-D variables to an additional file
c     (3) Add ability to extract part of vertical levels
c     (4) Add output of SWDOWN and LWDOWN to record header
c     (5) Return the changes of format in (6) of V1.1 L000726 back to their
c         original formats in MM5.dat format
c     (6) Take off conefactor from header record 4 in MM5.dat format  
c
c --- Version 1.0, Level 000216 to Version 1.1, Level 000726 (Major revision)
c --- Made by Zhong-Xiang. Wu on 7/27/2000 
c     Note: Revision is needed in CALMET to read the calmm5.dat 
c           output from V1.1 since the output format (both in header
c           and data records) has been changed. 
c
c     (1) Expand header record #4 (original #3). Add LCC cone factor, 
c         MM5 domain, first dot (1,1) position, numbers of grids 
c         in X/Y/Z directions, and grid resolution. 
c     (2) Change latitude and longitude output format. Use f9.4 for 
c         latitude and f10.4 longitude. 
c     (3) Use YYYY form for year.     
c     (4) Add data set name. Move data set name, version, level to 
c         a separated line.
c     (5) Get model top pressure from model output instead hard-wired 
c         to 100 hPa
c     (6) Change output format for grid I/J from i3 to i4, 
c         for WS from f5.1 to f6.1, 
c         for RH from i3 to i4, and 
c         for VAPMR, CLDMR, RAINMR, ICEMR SNOWMR,GRPMR from f5.2 f6.2. 
c         These change will allow users to use free fromat to read output data
c
c --- Version 1.0, Level 991129 to Version 1.0, Level 000216
c --- Made by J. Scire
c
c    (1) Logic added to MAPG2L to accommodate -180 to +180 longitude 
c        boundary (routine substituted from CALMET 991104a
c    (2) Modify format statement 1019 to use character*12 format
c        for version and level numbers
c
c --- Version 1.0, Level 990318/991122 to Version 1.0, Level 991129
c --- Made by Zhong-Xiang Wu (11/29/99)
c
c    (1) Modify format statements for latitude/longitude in header 
c        records to fit the format used by CALMET. The changes are:
c
c        In the MM5 format output file
c          original - 99   format(2i3,f6.2,f8.2,i5,i3,1x,f6.2,f8.2)
c          revised  - 99   format(2i3,f7.3,f8.3,i5,i3,1x,f7.3,f8.3)
c
c    (2) Add version and level characters (character*12) at title line, 
c        use a80,2a12 to write title, version, and level
c
c --- INPUT : Binary output from MM5
c --- OUTPUT: ASCII files in 3D.DAT/2D.DAT and MM4.DAT, Binary files in GrADS, 
c         VIS5D and NetCDF formats 

c-------------------------------------------------------------------------
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'
      include 'calmm5.cm4'
c      include 'v5df.h'
c      include 'netcdf.inc'

c ----------------------------------------------------------------------
c --- Specify CALMM5 code version and level
      codever='2.7'
      codelevel='061030'

C     Specify 3D.DAT/2D.DAT data sets format Version and level number  
      dataver='2.1'    
      datalevel='040921'

c --- Specify output data set names from this code
      name3d='3D.DAT'
      name2d='2D.DAT'

      datamod='Header Structure with Comment Lines'
      ncomm=1
      write(commenthd,111)codever,codelevel
 111  format('Produced by CALMM5 Version: ',a8,', Level: ',a8)

      open(ict, file='calmm5.inp',status='old')

      call setup

C --- Different setup for different format
c --- open output file for iformat 1-2(CALMM5/MM5, MM4) or 6 (GRIB) 
      nt=index(outfile,' ')-1
      write(ilg,*)'----- Output File -----'
      write(ilg,1010)outfile(1:nt)
 1010 format('Output File Name:',a,/)

      if(iformat.le.2) open (iop,file=outfile,status='unknown')

      if(iformat.eq.6)
     & open (iop,file=outfile,access='sequential',status='unknown')
c     & open (iop,file=outfile,convert='big_endian'   ! not work
c     &       ,access='sequential',status='unknown')

c --- open output GrADS files in subroutine ctlgrd (iformat=3)
c --- open output V5D file in subroutine ctlv5d (iformat=4)
c --- open output NetCDF file in subroutine ctlnc (iformat=5)

c --- write out title of output file (record #1 and record #2)
c      if(iformat.le.2) then  ! for version 1.0 level 991122 or lower
c         call noblank(title,nt)
c         write(iop,'(a)') title(1:nt)
c      endif

      if(iformat.eq.1) then  ! for level 991129 and up
         io=iop
         call outcomment(io,name3d)
      elseif(iformat.eq.2) then
         write(iop,1020)title
 1020    format(a80)
      endif

c --- set up variables for different iformat
c     (no-setup is needed for iformat 2 - MM4)
      if(iformat.eq.1) call setupmm5     
      if(iformat.ge.3 .and. iformat.le.6) call getvar

C --- Loop over MM5 input files -----------------------------------
      ifile=1

 5000 if(nf.gt.1) then
         infile=fname(ifile)
      elseif(nf.le.0) then
         call getname(infile,fbas,ifile)
      endif

      print *,' Input MM5 File: ',ifile,' ',trim(infile)
      write(ilg,*)' Input MM5 File: ',ifile,' ',trim(infile)
      
      if(ifile.gt.1) then
         print *,' ******* Warning: *************** '
         print *,'     Need overlapping hours if this file'
         print *,'       is from different MM5 runs'
         print *
         write(ilg,*)' ******* Warning: *************** '
         write(ilg,*)'     Need overlapping hours if this file'
         write(ilg,*)'       is from different MM5 runs'
         write(ilg,*)
      endif

c     On Unix-Dec-Alpha
      open (imm5,file=infile,form='unformatted',status='old',
     &          convert='big_endian',action='read')
                                         
c --- Read mm5 header and first hour data
      if(ifile.eq.1) then
         print *,'Required beg/end dates:',idatebeg,idateend
         call initdata
         call readhd(nx,ny,nz)
      else
         print *,'Required beg/end dates:',idatenext_out,idateend
         call readhd2(nx,ny,nz)
      endif

c      print *,'Processed Date:',idate
c --- read mm5 2nd hour data and after
 1000 continue

c      write(ilg,*)'Read MM5 data:',current_date,' ',idate
      call initdata
      call rdv3data(nx,ny,nz)
      if(iflend.eq.1) then
         print *,' Reach End: ',ifile,infile
         goto 1500
      endif

      call chkcomp
      call chkdat
      call chk23d
      call process(nx,ny,nz)
c      print *,'Processed Date:',idate

 1500 continue
      if(idate.lt.idateend .and. iflend.eq.1) then
         close(imm5)
         ifile=ifile+1
         if(ifile.le.nf) then
            goto 5000
         else
            goto 5500
         endif

      endif

      if(idate.lt.idateend) goto 1000
 
c --- exit
 5500 continue

      if(iformat.le.2 .or. iformat.ge.6) close(iop)

c      if(iformat.eq.4) then 
c         nn=v5dclose()
c         if(nn.eq.0) then
c            write(ilg,*)'Close failed: NN= ',nn
c            write(ilg,*)'Close failed: NN= ',nn
c         endif
c      endif

c      if(iformat.eq.5) then
c         status = NF_CLOSE(ncid)
c         if(status .ne. NF_NOERR) call HANDLE_ERR(status)
c         write(ilg,*)'Succeeded: NF_CLOSE, ncid - ', ncid
c         write(ilg,*)'Succeeded: NF_CLOSE, ncid - ', ncid
c      endif

      if(idate.lt.iend) then
         write(ilg,*)'end of record reached before selected date'
         write(ilg,*)'last date processed: ',idate
      else
         write(ilg,*)'Data Created'
      endif

      write(ilg,*)
      write(ilg,*)'---- Successful Calmm5 Run -----'
      print *,'---------------------------------'
      print *,'---- Successful Calmm5 Run ----- '
      print *,'---------------------------------'

      stop
      end

      include 'calmm5.blk'
      include 'coordlib.f'
      include 'calutils.f'

c *******************************************************************
      subroutine calhdr(nz)

C --- CALMM5   Version: 2.7   Level: 999999

c purpose: write out output file header for MM5 format

c -------------------------------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm4'

c Note: records #1 & #2 written in main program
c       records #3 and #4 written in hdproc

c     Setup additional 3D.DAT header info missing in previous hdproc
c --- write out mm5 options to output header : record #5 (mm5.dat only)
      if (iformat.eq.1) then
c        Header Record #5
         write(iop,44)inhyd,imphys,icupa,ibltyp,ifrad,isoil,
     :        ifddaan,ifddaob,(idv2pk(i),i=4,n2dpk),ilandmm5
         if (iosrf.eq.1) then
            write(isrf,44)inhyd,imphys,icupa,ibltyp,ifrad,isoil,
     &           ifddaan,ifddaob,(idv2pk(i),i=4,n2dpk),ilandmm5
         endif
      endif
 44   format (30(i3))

c header record #6:
      idateout=ibeg

      if(idateout.lt.1.0e9) call ck2y(idateout)

      if(nz1.lt.1) then
         write(ilg,*)'Warning: NZ1 > 1. Set NZ1=1'
         nz1=1
      endif
      if(nz2.gt.nz) then
         write(ilg,*)'Warning: NZ2 > NZ. Set NZ2=NZ'
         nz2=nz
      endif

c     Convert user input nz1/nz2 (from bottom to top) to MM5 nz1/nz2
c     (from top to bottom)
      nz12=nz1
      nz22=nz2
      nz1=nz-nz22+1
      nz2=nz-nz12+1
      nzsub=nz2-nz1+1

      write(iop,96)idateout,nhours,nxsub,nysub,nzsub
      if(iosrf.eq.1)
     &  write(isrf,96)idateout,nhours,nxsub,nysub,nzsub

c header record #7:
      write(iop,97)nx1,ny1,nx2,ny2,nz12,nz22,rxmin,rxmax,rymin,rymax
      if(iosrf.eq.1)
     & write(isrf,97)nx1,ny1,nx2,ny2,nz12,nz22,rxmin,rxmax,rymin,rymax

c next nz records:sigma levels 
      do k=nz2,nz1,-1
         write(iop,98)hsig(k)
         if(iosrf.eq.1) write(isrf,98)hsig(k)
      end do

c geophysical records. 
c note: lat/lon and elev at dot point, land use at cross point
      do 1 j=ny1,ny2
         do 1 i=nx1,nx2
            write(iop,99)i,j,rlat(i,j),rlon(i,j),nint(elev(i,j))
     &          ,nint(rland(i,j)),rlatcrs(i,j),rloncrs(i,j)
     &          ,nint(elevcrs(i,j))
            if(iosrf.eq.1) 
     &          write(isrf,99)i,j,rlat(i,j),rlon(i,j),nint(elev(i,j))
     &          ,nint(rland(i,j)),rlatcrs(i,j),rloncrs(i,j)
     &          ,nint(elevcrs(i,j))  
 1    continue

 96   format(i10,i5,4i4)
 97   format(6i4,2f10.4,2f9.4)
 98   format(f6.3)
 99   format(2i4,f9.4,f10.4,i5,i3,1x,f9.4,f10.4,i5)

c 96   format(i8,i5,3i4)
c 97   format(4i4,4(f8.2))
c 99   format(2i3,f7.3,f8.3,i5,i3,1x,f7.3,f8.3)

      return
      end

c *******************************************************************
      subroutine calhdr4 (nx,ny,nz)

C --- CALMM5   Version: 2.7   Level: 999999

c -------------------------------------------------------------------

c purpose: write out output file header in the mm4.dat format

c -------------------------------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm4'

c note: record #1 and record # 2  have already been written in main program

c header record #3 (still use YYMMDDHR in date format):
      idatebeg2=idatebeg
      call k2y(idatebeg2)
      write(iop,96)idatebeg2,nhours,nx,ny,nz-8,ptop

c header record #4:
      write(iop,97)nx1,ny1,nxsub,nysub

c next (nz-8) records:first (nz-8) half-sigma levels
      do k=nz,9,-1
         write(iop,98)hsig(k)
      end do

c geophysical records
c note: lat/lon and elev at dot point, land use is on cross point
      do 1 j=ny1,ny2
         do 1 i=nx1,nx2
            write(iop,99)i,j,rlat(i,j),rlon(i,j),nint(elev(i,j)),
     &           nint(rland(i,j)),rlatcrs(i,j),rloncrs(i,j)
 1    continue

 96   format(i8,4i4,f6.1)
 97   format(4i4,f7.3,f7.2,f8.3,f8.3)
 98   format(f6.4)
 99   format(2i3,f7.3,f8.3,i5,i3,1x,f7.3,f8.3)
      return
      end

c *******************************************************************
      subroutine cleanrows(f,ix,jx,kx)

C --- CALMM5   Version: 2.7   Level: 999999

c -------------------------------------------------------------------
c   purpose:
c   the outer row and column of cross point data may have
c   strange values, so we set those questionable data to the
c   same value as the interior neighbor.  this is only for
c   cross point data.
c   written by: ncar
c --------------------------------------------------------------------
      dimension f(ix,jx,kx)
      do k=1,kx
         do j=1,jx-1
            f(ix,j,k)=f(ix-1,j,k)
         enddo
         do i=1,ix
            f(i,jx,k)=f(i,jx-1,k)
         enddo
      enddo
      
      return
      end

c *********************************************************************
      subroutine ctlgrd
      
C --- CALMM5   Version: 2.7   Level: 999999
  
c Purpose: Write GrADS control file and open GrADS data file
c Note: for fixed domain only, no moving of  MM5 domains.
c
c Zhong-Xiang Wu

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'

      character mname(12)*3
      data mname/'jan','feb','mar','apr','may','jun','jul',
     &     'aug','sep','oct','nov','dec'/

c --------------------------------------------------------------------
c --- GrADS control file name (.ctl) 

      nt=index(outfile,'.')
      gradsctl=outfile(1:nt)//'ctl'
      nt=index(options,' ')-1
      options=options(1:nt)

c --- Open GrADS data file
      if(options.eq.'DIRECT' .or. options.eq.'direct') then
         idirect=1
         len_rcd=nxsub*nysub
         open (iop,file=outfile,status='unknown',form='unformatted',
     &        access=options,recl=len_rcd)
      else
         idirect=0
         open (iop,file=outfile,status='unknown',form='unformatted',
     &        access=options)
      endif

c --- Open GrADS contol file
      open(igd,file=gradsctl,status='unknown')

c --- Write GrADS control file
      call noblank(outfile,nt)
      write(igd,501)outfile(1:nt)

      call noblank(title,nt)
      write(igd,502)title(1:nt)
      if(options.eq.'sequential' .or. options.eq.'SEQUENTIAL') 
     &   write(igd,503)options
      write(igd,504)fmiss
 501  format('DSET     ',a)
 502  format('TITLE     ',a)
 503  format('OPTIONS  ',a10)
 504  format('UNDEF    ',E14.5)

      xorg_sub=x1dmn+(nx1-1)*dxy
      yorg_sub=y1dmn+(nx1-1)*dxy
      write(igd,601)nxsub,xorg_sub,dxy
      write(igd,602)nysub,yorg_sub,dxy
      write(igd,603)nlevel,(level(i),i=1,nlevel)

      write(igd,604)nhours,ihourbeg,idaybeg,mname(imonthbeg),iyearbeg
 601  format('XDEF  ',i5,'  LINEAR   ',2f12.3)
 602  format('YDEF  ',i5,'  LINEAR   ',2f12.3)
 603  format('ZDEF  ',i5,'  LEVELS   ',50i5)
 604  format('TDEF  ',i5,'  LINEAR   ',i2.2,'Z',i2.2,a3,i4.4, ' 1hr')

c     only 3-D and 2-D in GrADS
      nvar=n3d+n2d    
      write(igd,701)nvar
 701  format('VARS  ',i5)
      
      do i=1,n3d
         write(igd,702)v3name(i),nlevel,v3name(i)
      enddo
 702  format(a8,2x,i4,'  0  ',a8)

      do i=1,n2d
         write(igd,703)v2name(i),v2name(i)
      enddo
 703  format(a8,2x,'  0   0  ',a8)

      write(igd,704)
 704  format('ENDVARS')

c     initial record # for direct accress format
      irecord=0

      return
      end

c **********************************************************************
      subroutine extra2d(dum2d,tmp2d,nxtmp,nytmp)
      
C --- CALMM5   Version: 2.7   Level: 999999

      include 'calmm5.par'
      include 'calmm5.cm2'
      dimension dum2d(mxnx,mxny),tmp2d(nxtmp,nytmp)

      do j=ny1,ny2
         jj=j-ny1+1
         do i=nx1,nx2
            ii=i-nx1+1
            tmp2d(ii,jj)=dum2d(i,j)
         enddo
      enddo

      return
      end

c***********************************************************************
       subroutine getflname(string,nt)

C --- CALMM5   Version: 2.7   Level: 999999

c ---   Get file name from an input line 
c       The file name is at the begining of and before the first 
c       blank space in the line

c Zhong-xiang Wu
c -------------------------------------------------------------------
       include 'calmm5.par'
       character*80 string

       nt=0
       nt=index(string,' ')-1

       if(nt.lt.1) then
          write(ilg,*)'Error in Filename - Blank String'
          stop
       endif

       string=string(1:nt)

       return
       end

c *********************************************************************
      subroutine getvar

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu
c -------------------------------------------------------------------
c --- Setup for GrADS, Vis5D, NetCDF, and Grib output formats
c --- Read in variables:
c     n3d, nlelev (integer): Number of 3-D variables and number of 
c                            their levels  
c     level (integer array): Levels of 3-D variables
c     v3nmae (char array)  : Names of 3-D variables 
c     n2d (integer)        : Number of 2-D variables
c     v2name (char array)  : Names of 2-D variables            
c     n1d (integer)        : Number of 1-D variables
c -------------------------------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm7'

c --- Read in variables needed by  GrADS
c     3-D variables
      read(ict,*)n3d,nlevel

      write(ilg,201)n3d,nlevel
 201  format('Numbers of 3-D variables and levels: ',2i6)
      if(nlevel.gt.mxnz) then
         print *,'nlevel can be > mxnz: ', nlevel, mxnz
         stop
      endif

      if(n3d.ge.1 .and. nlevel.ge.1) then
         read(ict,*)(level(i),i=1,nlevel)
         write(ilg,202)(level(i),i=1,nlevel)
 202     format('Vertical levels: ',30i3)

         do i=1,n3d
            read(ict,101)v3name(i)
            write(ilg,203)v3name(i)
 203        format('3-D variable name: ',a8)
         enddo
      endif
 101  format(a8)

c --- 2-D variables
      read(ict,*)n2d
      write(ilg,204)n2d
 204  format('Numbers of 2-D variables and levels: ',i6)

      if(n2d.ge.1) then
         do i=1,n2d
            read(ict,101)v2name(i)
            write(ilg,205)v2name(i)
 205        format('2-D variable name: ',a8)
         enddo
      endif

c --- 1-D variables
      read(ict,*)n1d
      write(ilg,206)n1d
 206  format('Numbers of 1-D variables and levels: ',i6)

      if(n1d.ge.1) then
         do i=1,n1d
            read(ict,101)v1name(i)
            write(ilg,207)v1name(i)
 207        format('1-D variable name: ',a8)
         enddo
      endif

c --- Flag for grid equal lat/lon output
      if(iformat.eq.6) then
         read(ict,*)ilateql,id_grdconf,grdconf
c Specific for TYCO runs- Save changes for calmm5.inp
c         ilateql=1
c         id_grdconf=1
c         grdconf=0.5

         write(ilg,208)ilateql,id_grdconf,grdconf
 208     format('Grib Equal Lat/Lon Output Flag: ',i2,/,
     &     'Minimum Grid Size Flag: ',i2,/,
     &     'Minimum Grid Size (Degree):', f12.4 )
      endif

      return
      end

c ******************************************************************
      subroutine hdproc(nx,ny,nz)

C --- CALMM5   Version: 2.7   Level: 999999

c ------------------------------------------------------------------
c purpose: Check and process MM5 header information
c -----------------------------------------------

      include 'calmm5.par'   
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'
      include 'calmm5.cm7'

      istop = 0
      
      mindex=bhi(1,1)
 
      if (mindex.ne.11) then
         write(ilg,*)'mindex; ',mindex
         write(ilg,*)'the input file is not a mm5 output file but is:'
         if(mindex.eq.1) write(ilg,*)'from terrain'
         if(mindex.eq.2) write(ilg,*)'from regrid'
         if(mindex.eq.3) write(ilg,*)'rawins/little_R 3D analysis'
         if(mindex.eq.4) write(ilg,*)'rawins surface 4dda'
         if(mindex.eq.5) write(ilg,*)'MM5 initial condition'
         if(mindex.eq.6) write(ilg,*)'MM5 lower boundary condition'
         if(mindex.eq.7) write(ilg,*)'MM5 lateral boundary condition'
         if(mindex.eq.8) write(ilg,*)
     :      'interpolated mm5 output on pressure surfaces from interp'
         stop
      endif

      write(ilg,*)' Porcessing mm5 big header'
      write(ilg,*) 

      iniyr=bhi(5,11)
      inimo=bhi(6,11)
      inidy=bhi(7,11)
      inihr=bhi(8,11)
      inimi=bhi(9,11)

      call julday(ilg,iniyr,inimo,inidy,ijul)
      if(inimi.gt.50) then
         call incr(ilg,iniyr,ijul,inihr,1)
         call grday(ilg,iniyr,ijul,inimo,inidy)
      endif

      call timestamp(iniyr,inimo,inidy,inihr,idate) 
      if(idate.lt.1.0e9) call ck2y(idate)

c     Need YY from YYYY for GRIB
      ihr_fst=0
      if(iniyr.ge.100) then
         icentury=iniyr/100 + 1
         iniyr=iniyr-iniyr/100*100
      else
         if(iniyr.gt.40) then
            icentury=20
         else
            icentury=21
         endif
      endif

      write(ilg,*)' starting date of mm5 output data:',idate
      write(*,*)' starting date of mm5 output data:',idate
      write(ilg,*)

      write(ilg,*)' Model initial hour: ',iniyr,inimo,inidy,inihr
      write(*,*)' Model initial hour: ',iniyr,inimo,inidy,inihr
      write(ilg,*)

c --- mm5 physical options
      write(ilg,*)'mm5 options:'
      write(ilg,*)
      
      ifdry=bhi(14,13)
      write(ilg,*)'Fake dry run: ',ifdry

      inhyd=1
      if(inhyd.eq.1) then
         write(ilg,*)'   non hydrostatic run'
         p00=bhr(2,5)
         write(ilg,211) p00
         ts0=bhr(3,5)
         write(ilg,212) ts0
         tlp=bhr(4,5)
         write(ilg,213) tlp
211      format(6x,'reference pressure p0 : ', f10.1,' pa')
212      format(6x,'reference temperature : ', f10.1,' k ')
213      format(6x,'ref. temperature lapse rate :', f5.1,' k/500mb')

      else if(inhyd.eq.0) then
         write(ilg,*)'   hydrostatic run'
      endif
      write(ilg,*)

      NPLEV=bhi(12,2)
      NTOTLV=bhi(12,3)

      ptop=bhr(2,2)/100.
      write(ilg,*)'Model Top Pressure: ',ptop

      iwater=bhi(23,1)
      if(iwater.eq.7) then
         ilandmm5=13
      elseif(iwater.eq.15) then
         ilandmm5=17
      elseif(iwater.eq.16) then
         ilandmm5=25
      else
         write(ilg,*)'Illegal land use category'
         print *,'Illegal land use category'
         stop
      endif

c --- map projection
      map=bhi(7,1)
      rlatc=bhr(2,1)
      rlonc=bhr(3,1)
      conefac=bhr(4,1)
      truelat1=bhr(5,1)
      truelat2=bhr(6,1)
      plat=bhr(7,1)

c --- domain dimensions (east; nx - north: ny - vertical; nz)       
      id=bhi(13,1)
      write(ilg,*)'mm5 domain id: ',id
      write(ilg,*)
      nx=bhi(17,1)
      ny=bhi(16,1)
      nz=bhi(12,11)
      dxy=bhr(9,1)/1000.   ! Change to km

      nxmm5=nx    ! Note: converted to conventional X/Y already
      nymm5=ny
      nzmm5=nz
      dxymm5=dxy

c --- Figure out the SW Dot point location (I=1,J=1 for current domain)
      nicoarse=bhi(5,1)
      njcoarse=bhi(6,1)
      dxycoarse=bhr(1,1)

      fi1dmn=bhr(10,1)
      fj1dmn=bhr(11,1)

c --- (current domain dot point 1,1:  east, x1dmn; north, y1dmn)
      x1coarse=-((njcoarse-1)*dxycoarse/2.0)
      y1coarse=-((nicoarse-1)*dxycoarse/2.0)

      x1dmn=x1coarse+(fj1dmn-1)*dxycoarse
      y1dmn=y1coarse+(fi1dmn-1)*dxycoarse

      x1dmn=x1dmn/1000.  ! Change to km
      y1dmn=y1dmn/1000.   

c --- write out mm5 map characteristics to list file and output header
      if (map.eq.1) then
         write(ilg,*)'lambert conformal map projection'
         write(ilg,*)'   center latitude  (degrees): ',rlatc
         write(ilg,*)'   center longitude (degrees): ',rlonc
         write(ilg,*)'   true latitude 1  (degrees): ',truelat1
         write(ilg,*)'   true latitude 2  (degrees): ',truelat2
         write(ilg,*)'   cone factor: ',conefac
         write(ilg,*)'   SW dot point X/Y: ',x1dmn,y1dmn
         write(ilg,*)

c        record #4 (mm5.dat only)
         if (iformat.eq.1)
     1        write(iop,45)rlatc,rlonc,truelat1,truelat2,
     2        x1dmn,y1dmn,dxy,nx,ny,nz 
 45      format('LCC ',f9.4,f10.4,2f7.2,2f10.3,f8.3,2i4,i3)
         if (iosrf.eq.1)
     1        write(isrf,45)rlatc,rlonc,truelat1,truelat2,
     2        x1dmn,y1dmn,dxy,nx,ny,nz 
      else if (map.eq.2) then
         if(conefac .ne. 1.0) then
            print *,'Error in Conefactor in polar proj:',conefac
            stop
         endif
         write(ilg,*) 'Polar tereographic map projection'
         write(ilg,*)'   center latitude  (degrees): ',rlatc
         write(ilg,*)'   center longitude (degrees): ',rlonc
         write(ilg,*)'   pole latitude    (degrees): ',plat
         write(ilg,*)'   true latitude 2  (degrees): ',truelat2
         write(ilg,*)'   cone factor: ',conefac
         write(ilg,*)'   SW dot point X/Y: ',x1dmn,y1dmn
         write(ilg,*)
         if (iformat.eq.1)
     1        write(iop,46)rlatc,rlonc,plat,truelat2,
     2        x1dmn,y1dmn,dxy,nx,ny,nz 
 46      format('PST ',f9.4,f10.4,2f7.2,2f10.3,f8.3,2i4,i3)
         if (iosrf.eq.1)
     1        write(isrf,46)rlatc,rlonc,plat,truelat2,
     2        x1dmn,y1dmn,dxy,nx,ny,nz 
      else if (map.eq.3) then
         write(ilg,*)'Mercator map projection'
         write(ilg,*)'   center latitude  (degrees): ',rlatc
         write(ilg,*)'   center longitude (degrees): ',rlonc
         write(ilg,*)'   pole latitude    (degrees): ',plat
         write(ilg,*)'   true latitude 2  (degrees): ',truelat2
         write(ilg,*)'   cone factor: ',conefac
         write(ilg,*)'   SW dot point X/Y: ',x1dmn,y1dmn
         write(ilg,*)
         if (iformat.eq.1)
     1        write(iop,47)rlatc,rlonc,plat,truelat2,
     2        x1dmn,y1dmn,dxy,nx,ny,nz 
 47      format('MER ',f9.4,f10.4,2f7.2,2f10.3,f8.3,2i4,i3)
         if (iosrf.eq.1)
     1        write(isrf,46)rlatc,rlonc,plat,truelat2,
     2        x1dmn,y1dmn,dxy,nx,ny,nz 
      endif

c --- 
      write(ilg,*)' nx in MM5 (east)    : ',nx
      write(ilg,*)' ny in MM5 (north)   : ',ny
      write(ilg,*)' nz in MM5 (vertical): ',nz
      write(ilg,*)' dxy in MM5 (km)     : ',dxy
      write(ilg,*)

      write(*,*)' nx in MM5 (east)    : ',nx
      write(*,*)' ny in MM5 (north)   : ',ny
      write(*,*)' nz in MM5 (vertical): ',nz
      write(*,*)' dxy in MM5 (km)     : ',dxy

c --- check that calmm5 array sizes are large enough
      if ((nx.gt.mxnx).or.(ny.gt.mxny).or.(nz.gt.mxnz)) then
         write(ilg,*)' calmm5 array sizes are not large enough'
         write(ilg,*)' edit calmm5.par and recompile'
         write(ilg,*)' mm5 nx,ny,nz:             ',nx,ny,nz
         write(ilg,*)' calmm5 mxnx,mxny,mxnz: ',mxnx,mxny,mxnz
         write(ilg,*)' stop'
         stop
      endif

      if(ibeg.lt.idate) then
         write(ilg,*)' requested starting date is too early '
         write(ilg,*)' starting date of mm5 output: ',idate
         write(ilg,*)' requested starting date:     ',ibeg
         write(ilg,*)' stop'
         stop
      endif

c --- mm5 physical options
      imphys  = bhi(3,13)
      icupa   = bhi(2,13)
      ibltyp  = bhi(4,13)
      ifrad   = bhi(1,13)
      isoil   = bhi(5,13)
      ifddaan = bhi(13,11)
      ifddaob = bhi(14,11)

      islp=1    ! Output Sea level pressure for TYCO Team

      return
      end

c **********************************************************************
      subroutine interp_latlon(xdot,xcrs,nx,ny)

C --- CALMM5   Version: 2.7   Level: 999999

c purpose: interpolate the latitude(or longitude), defined on cross points, 
c          to dot points, where u and v are defined.
c          4-point average except at boundaries
c          2-d arrays

      include 'calmm5.par'
      include 'calmm5.cm2'

      real xdot(mxnx,mxny),xcrs(mxnx,mxny)

c --- Expand one grid size for up-limit cross grids lat/lon
c     Right edge
      do j=1,ny-1
         xcrs(nx,j)=xcrs(nx-1,j)+(xcrs(nx-1,j)-xcrs(nx-2,j))
      enddo

c     Top edge
      do i=1,nx-1
         xcrs(i,ny)=xcrs(i,ny-1)+(xcrs(i,ny-1)-xcrs(i,ny-2))
      enddo

c     NE corner of cross point
      a1=xcrs(nx,ny-1)+(xcrs(nx,ny-1)-xcrs(nx,ny-2))
      a2=xcrs(nx-1,ny)+(xcrs(nx-1,ny)-xcrs(nx-2,ny))
      xcrs(nx,ny)=0.5*(a1+a2)

c --- interior dot points
      do j=2,ny
         do i=2,nx
            xdot(i,j)=0.25*(xcrs(i,j)+xcrs(i-1,j-1)
     :           +xcrs(i-1,j)+xcrs(i,j-1))
         enddo
      enddo

c --- left edge dot point (i=1, j=2,ny-1)
      do j=2,ny
         aa=0.5*(xcrs(1,j)+xcrs(1,j-1))
         d1=0.5*(xcrs(2,j-1)-xcrs(1,j-1))
         d2=0.5*(xcrs(2,j)-xcrs(1,j))
         dd=0.5*(d1+d2)
         xdot(1,j)=aa-dd
      enddo

c --- bottom edge (j=1, i=2,nx-1)
      do i=2,nx
         aa=0.5*(xcrs(i,1)+xcrs(i-1,1))
         d1=0.5*(xcrs(i-1,2)-xcrs(i-1,1))
         d2=0.5*(xcrs(i,2)-xcrs(i,1))
         dd=0.5*(d1+d2)
         xdot(i,1)=aa-dd
      enddo

c --- SW corner
      a1=xdot(2,1)-(xdot(3,1)-xdot(2,1))
      a2=xdot(1,2)-(xdot(1,3)-xdot(1,2))
      xdot(1,1)=0.5*(a1+a2)

      return
      end

c **********************************************************************
      subroutine interpol2 (x)

C --- CALMM5   Version: 2.7   Level: 999999

c purpose: interpolate the scalar variable x, defined on cross points, to 
c          dot points, where u and v are defined.
c          4-point average (except at boundaries)
c          2-d arrays

c ----------------------------------------------------------------------
      include 'calmm5.par'
      include 'calmm5.cm2'

      real x(mxnx,mxny),dum(mxnx,mxny)

c --- interior points
      do 1 j=ny11,ny2
         do 1 i=nx11,nx2
            dum (i,j)=0.25*(x(i,j)  +x(i-1,j-1)
     :           +x(i-1,j)+x(i,j-1))
 1    continue

      if (nx1.eq.1)then
         do 2 j=ny11,ny2
            dum (1,j)=0.5*(x(1,j)+x(1,j-1))
 2       continue
      endif

      if (ny1.eq.1)then
         do 3 i=nx11,nx2
            dum (i,1)=0.5*(x(i,1)+x(i-1,1))
 3       continue
      endif

      if ((nx1.eq.1).and.(ny1.eq.1))
     :     dum(1,1)=x(1,1)

      do 5 j=ny1,ny2
         do 5 i=nx1,nx2
            x(i,j)=dum(i,j)
 5    continue

      return
      end

c *****************************************************************************
      subroutine interpol3 (x,nz)

C --- CALMM5   Version: 2.7   Level: 999999

c purpose: interpolate the scalar variable x, defined on cross points, to 
c          dot points, where u and v are defined.
c          4-point average (except at boundaries)
c          3-d arrays

c ------------------------------------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm2'

      real x(mxnx,mxny,mxnz),dum(mxnx,mxny,mxnz)

c --- interior points
      do 4 k=1,nz
         do 1 j=ny11,ny2
            do 1 i=nx11,nx2
               dum (i,j,k)=0.25*(x(i,j,k)+x(i-1,j-1,k)
     :              +x(i-1,j,k)+x(i,j-1,k))
 1       continue
         if (nx1.eq.1)then
            do 2 j=ny11,ny2
               dum (1,j,k)=0.5*(x(1,j,k)+x(1,j-1,k))
 2          continue
         endif
         if (ny1.eq.1)then
            do 3 i=nx11,nx2
               dum (i,1,k)=0.5*(x(i,1,k)+x(i-1,1,k))
 3          continue
         endif
         if ((nx1.eq.1).and.(ny1.eq.1)) 
     :        dum(1,1,k)=x(1,1,k)
  4   continue

      do 5 k=1,nz
         do 5 j=ny1,ny2
            do 5 i=nx1,nx2
               x(i,j,k)=dum(i,j,k)
 5    continue
        
      return
      end

c *****************************************************************************
      subroutine interpdot (x,nx,ny,nz)

C --- CALMM5   Version: 2.7   Level: 999999

c purpose: interpolate the scalar variable x, defined on cross points, to 
c     dot points, where u and v are defined. 4-point average (except at 
c     boundaries) for 3/2-d arrays
c Note: The I/J ranges of arrays may be in MM5 definition. So NX/NY should
c     be large enough to hold max(nx,ny)
c ------------------------------------------------------------------------

      include 'calmm5.par'
c      include 'calmm5.cm2'

      real x(nx,ny,nz),dum(mxnx,mxny,mxnz)

c --- interior points
      do 100 k=1,nz
         do j=2,ny
            do i=2,nx
               dum (i,j,k)=0.25*(x(i,j,k)+x(i-1,j-1,k)
     &              +x(i-1,j,k)+x(i,j-1,k))
            enddo
         enddo

         do j=2,ny
            dum (1,j,k)=0.5*(x(1,j,k)+x(1,j-1,k))
         enddo

         do i=2,nx
            dum (i,1,k)=0.5*(x(i,1,k)+x(i-1,1,k))
         enddo

         dum(1,1,k)=x(1,1,k)

 100  continue

      do 200 k=1,nz
         do 200 j=1,ny
            do 200 i=1,nx
               x(i,j,k)=dum(i,j,k)
 200  continue
        
      return
      end

c **********************************************************************
      subroutine interpolw (w,nz)

C --- CALMM5   Version: 2.7   Level: 999999

c ------------------------------------------------------------------------
c purpose: interpolate w from full sigma surfaces to half sigma surfaces
c          linear interpolation
c
c-------------------------------------------------------------------------
      include 'calmm5.par'
      include 'calmm5.cm2'

      real w(mxnx,mxny,mxnz),dum(mxnx,mxny,mxnz+1)

c --- interior points

      do 2 j=ny1,ny2
         do 2 i=nx1,nx2
            do 1 k=2,nz
               dum (i,j,k-1)=0.5*(w(i,j,k-1)+w(i,j,k))
 1          continue
            dum (i,j,nz) =0.5*w(i,j,nz-1)
 2    continue

      do 5 k=1,nz
         do 5 j=ny1,ny2
            do 5 i=nx1,nx2
               w(i,j,k)=dum(i,j,k)
5     continue

      return
      end

c-----------------------------------------------------------------------
      subroutine mapg2l(rlat,rlon,tlat1,tlat2,alat,alon,xdist,ydist)
c-----------------------------------------------------------------------
c
c --- CALMM5   Version: 2.7       Level: 000216                   MAPG2L
c ---          Adapted from CALMET (991104a)
c ---          G. Moore (7/92), E. Chang (8/93)
c ---          Modified by J. Scire (11/96)
c ---          Modified by B. de Foy (7/99) for crossing 180 meridian 
c
c --- PURPOSE: Convert geodetic (LAT./LONG.) coordinates to
c ---          coordinates in a Lambert Conformal Conic projection.
c
c --- NOTE:    This routine uses the following conventions:
c                 Latitude   - Northern Hemisphere - positive
c                              Southern Hemisphere - negative
c                 Longitude  - Eastern Hemisphere  - positive
c                              Western Hemisphere  - negative
c
c --- Note by Zhongxiang Wu (12/12/2000):
c       
c       Constant rearth is 6367.47km in GRIB coding (assuming spherical 
c       earth). But CALMET and CALPUFF use 6370km all the place needed. 
c       Therefore it is kept unchanged. It may cause small difference 
c       in mapping CALMM5 data in GRIB format
c     End of Wu's note
c
c --- INPUTS:
c
c        RLAT - real     - Reference latitude (deg.) of the origin of
c                          the LCC projection
c        RLON - real     - Reference longitude (deg.) of the LCC
c                          projection.  LCC origin will be at the point
c                          RLAT, RLON and the Y axis of the LCC grid
c                          will be oriented along the RLON meridian.
c       TLAT1 - real     - Latitude (deg.) of the first standard
c                          parallel for the Lambert conformal proj.
c       TLAT2 - real     - Latitude (deg.) of the second standard
c                          parallel.
c        ALAT - real     - Latitude (deg.) of point to be converted
c                          to LCC projection
c        ALON - real     - Longitude (deg.) of point to be converted
c                          to LCC projection
c
c --- OUTPUTS:
c
c       XDIST - real     - X distance from LCC origin (km)
c       YDIST - real     - Y distance from LCC origin (km)
c
c --- MAPG2L called by:  READCF
c --- MAPG2L calls:      none
c----------------------------------------------------------------------
c
c --- Set constants: PI4 = pi/4., RADCON=pi/180.
      data pi4/0.78539816/,radcon/0.017453293/
      data rearth/6370.0/
c     Note: rearth is 6367.47km in GRIB coding (assuming spherical earth). 
c       But CALMET and CALPUFF use 6370km all the place needed. Therefore 
c       it is kept unchanged.
c     Zhongxiang Wu
c     12/12/2000  
c
c --- Compute constants depending on standard parallels and
c --- reference coordinates
      a1 = cos(tlat1*radcon)/cos(tlat2*radcon)
      a2 = tan(pi4 - 0.5*(radcon*tlat1))
      a3 = tan(pi4 - 0.5*(radcon*tlat2))
      a5 = tan(pi4 - 0.5*(radcon*rlat))
c
c --- Compute the cone constant (cc), (K in other people's notation)
      cc = alog(a1)/alog(a2/a3)
c
c --- Scaling function
      psi = rearth*cos(tlat1*radcon)/(cc*a2**cc)
      rho1 = psi*a5**cc
c
c --- Start estimate of (alat,alon) to (x,y)
c --- Polar angle
      a4 = tan(pi4 - (radcon*alat)/2.0)
c
      theta = alon - rlon
c reset theta to range -180 to 180 degrees - bdf
      if (theta.lt.-180.0) theta = theta + 360.0
      if (theta.gt.180.0) theta = theta - 360.0
c
      thetab = theta*cc*radcon
c
c --- Polar radii (origin,latitude(alat))
      rho = psi*a4**cc
c
c --- Cartesian distances for a round earth in meters (S=1)
      xdist = rho*sin(thetab)
      ydist = rho1 - rho*cos(thetab)
c
      return
      end

c***********************************************************************
      subroutine noblank2(string,nt)

C --- CALMM5   Version: 2.7   Level: 999999

c ---   Get file name from an input line 
c       The file name is at the begining of and before the first 
c       blank space in the line
c Zhong-xiang Wu

c -------------------------------------------------------------------
      character*9 string

      nt=9
      do i=9,1,-1
         if(string(i:i).ne.' ') then
            nt=i
            return
         endif
      enddo
      
      return
      end

c***********************************************************************
      subroutine noblank(string,nt)

C --- CALMM5   Version: 2.7   Level: 999999

c ---   Get file name from an input line 
c       The file name is at the begining of and before the first 
c       blank space in the line
c Zhong-xiang Wu

c -------------------------------------------------------------------
      character*80 string

      do i=80,1,-1
         if(string(i:i).ne.' ') then
            nt=i
            return
         endif
      enddo
        
      nt=10
      
      return
      end

c***********************************************************************
      subroutine noblank_uds(string,nt)

C --- CALMM5   Version: 2.7   Level: 999999

c ---   Get last no-blank position of a string
c       Add underscore if there are blanks between
c Zhong-xiang Wu

c -------------------------------------------------------------------
      character*80 string

      do i=80,1,-1
         if(string(i:i).ne.' ') then
            nt=i
            goto 1000
         endif
      enddo

      print *,'Error in noblank_uds'
      print *,'String is all blank'
      stop

 1000 do i=1,nt
         if(string(i:i).eq.' ') string(i:i)='_'
      enddo

      return
      end

C *******************************************************************
      Subroutine lonunif(rloncrs,nx,ny)

C --- CALMM5   Version: 2.7   Level: 999999

c Purpose: unify longitude representation
C

      include 'calmm5.par'
      include 'calmm5.cm2'

      parameter (d180=90.0)
      real rloncrs(mxnx,mxny)
      real tmp(mxnx,mxny)

      ichg=0
      i1=1
      j1=1
      aa=rloncrs(i1,j1)
      ipn=aa/abs(aa)

      do j=1,ny
         do i=1,nx
            bb=rloncrs(i,j)
            ipn1=bb/abs(bb)
            if(ipn1.ne.ipn) then
               print *,'Longitude change signs:'
               print *,'   at',i1,j1,aa
               print *,'   at',i,j,bb
               write(ilg,*)'Longitude change signs:'
               write(ilg,*)'   at',i1,j1,aa
               write(ilg,*)'   at',i,j,bb
               goto 1000
            endif
         enddo
      enddo

      goto 5000

 1000 ic=nx/2
      jc=ny/2
      xc=rloncrs(ic,jc)
      dd=abs(abs(xc)-180)
      
      if(dd.gt.d180) goto 5000

C     Unify longitude representation 
C     Use 0-360 for longitude. (-180 - 180 in MM5)

      ichg=1

      do j=1,ny
         do i=1,nx
            cc=rloncrs(i,j)
            if(cc.lt.0) then
               cc=cc+360
               rloncrs(i,j)=cc
            endif
         enddo
      enddo

 5000 return
      end

c *********************************************************************
      subroutine pass2d(x2d,dum2d,nx,ny)

C --- CALMM5   Version: 2.7   Level: 999999

c Purpose: Pass 2-D array contents from dum2d to x2d
c          Note: the I/J in dum2d is RESERVED to those in x2d
c ---------------------------------------------------------

      include 'calmm5.par'

      real x2d(mxnx,mxny)
      real dum2d(nx,ny)

      if(ny.gt.mxnx .or. nx.gt.mxny) then
         print *,'Dimension Error in PASS2D : '
         print *,'                  mxnx, ny: ',mxnx,ny
         print *,'                  mxny, nx: ',mxny,nx
         stop
      endif

      do j=1,ny
         do i=1,nx
            x2d(j,i)=dum2d(i,j)
         enddo
      enddo

      return
      end

c *********************************************************************
      subroutine pass2d_org(x2d,dum2d,nx,ny)

C --- CALMM5   Version: 2.7   Level: 999999

c Purpose: Pass 2-D array contents from dum2d to x2d
c          Note: the I/J in dum2d is the SAME as those in x2d
c ---------------------------------------------------------

      include 'calmm5.par'

      real x2d(mxnx,mxny)
      real dum2d(nx,ny)

      if(nx.gt.mxnx .or. ny.gt.mxny) then
         print *,'Dimension Error in PASS2D_ORG : '
         print *,'                  mxnx, nx: ',mxnx,nx
         print *,'                  mxny, ny: ',mxny,ny
         stop
      endif

      do j=1,ny
         do i=1,nx
            x2d(i,j)=dum2d(i,j)
         enddo
      enddo

      return
      end

c *********************************************************************
      subroutine passdata3d(dum3d,nx,ny,nz,ivar)

C --- CALMM5   Version: 2.7   Level: 999999

c Purpose: Pass MM5 data to master array. Convert MM5 I/J 
c          to conventional I/J. 
c          Note: the I/J in dum3d is in MM5 I/J, I/J in x3dpk is reversed 
c                to observe usual I/J
c ---------------------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm4'

      real dum3d(nx,ny,nz)

      if(ny.gt.mxnx .or. nx.gt.mxny .or. nz.gt.mxnz)then
         print *,'Dimension Error in PASSDATA3D : '
         print *,'    mxnx, nx: ',mxnx,ny
         print *,'    mxny, ny: ',mxny,nx
         print *,'    mxnz, nz: ',mxnz,nz
         stop
      endif

      do k=1,nz
         do j=1,ny
            do i=1,nx
               x3dpk(j,i,k,ivar)=dum3d(i,j,k)  ! Note: reverse I/J
            enddo
         enddo
      enddo

      return
      end
c *********************************************************************
      subroutine passdata2d(dum2d,nx,ny,ivar)

C --- CALMM5   Version: 2.7   Level: 999999

c Purpose: Pass MM5 data to master array. Convert MM5 I/J 
c          to conventional I/J. 
c          Note: the I/J in x2d is in MM5 I/J, I/J in x3dpk is reversed 
c                to observe usual I/J
c ---------------------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm4'

      real dum2d(nx,ny)

      if(ny.gt.mxnx .or. nx.gt.mxny)then
         print *,'Dimension Error in PASSDATA3D : '
         print *,'    mxnx, nx: ',mxnx,ny
         print *,'    mxny, ny: ',mxny,nx
         stop
      endif

      do j=1,ny
         do i=1,nx
            x2dpk(j,i,ivar)=dum2d(i,j)
         enddo
      enddo

      return
      end

c *********************************************************************
      subroutine pass3d(x3d,dum3d,nx,ny,nz)

C --- CALMM5   Version: 2.7   Level: 999999

c Purpose: Pass 3-D array contents from dum3d to x3d
c          Note: the I/J in dum3d is reversed to those in x3d
c ---------------------------------------------------------

      include 'calmm5.par'

      real x3d(mxnx,mxny,mxnz)
      real dum3d(ny,nx,nz)

      if(ny.gt.mxnx .or. nx.gt.mxny .or. nz.gt.mxnz) then
         print *,'Dimension Error in PASS3D : '
         print *,'                  mxnx, ny: ',mxnx,ny
         print *,'                  mxny, nx: ',mxny,nx
         print *,'                  mxnz, nz: ',mxnz,nz
         stop
      endif

      do k=1,nz
         do j=1,ny
            do i=1,nx
               x3d(i,j,k)=dum3d(j,i,k)
            enddo
         enddo
      enddo

      return
      end

c ******************************************************************
       subroutine process (nx,ny,nz)
c ------------------------------------------------------------------
c
c --- CALMM5   Version: 2.7   Level: 040112                  PROCESS 
c
c --- Purpose: Processing one-hour MM5 data and output in different 
c              format after processing
c
c --- Update
c     Version 2.1,  Level: 040112
c     Zhong-Xiang Wu, 1/12/2004
c
c     (1) Fix a bug in relative humidity calculation, correct I 
c         loop from 1 to nx.
c
c Changes:
c   For LCC mapping (map=1), re-compute U/V component arrays after 
c   rotating MM5 wind from model north to true north.
C Zhongxiang Wu
C 12/15/2000
c ------------------------------------------------------------------
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'
      include 'calmm5.cm4'

      real dz(mxnx,mxny,mxnz)

      data rdry /287./
      data g /9.81/
      gamad=0.0065
      
c --- hydrostatic equilibrium constants
c --- convert p00 from pa to mbar (hpa)
      p00=p00/100.
      cst1=rdry*tlp/g/2.
      cst2=rdry*ts0/g

c --- nonhydrostatic (reference pressure and altitude at DOT points)
c     Constant reference pressure and altitude
      if (ifirst.eq.1) then
         do 210 k=1,nz   
         do 210 j=1,ny
         do 210 i=1,nx
c  ---      pref, p00 and ptop in mbar(hpa),pstar in pa
            pref(i,j,k)=pstar(i,j)/100.*hsig(k)+ptop
            z(i,j,k)=-cst1*(alog(pref(i,j,k)/p00))**2
     &           -cst2* alog(pref(i,j,k)/p00)
c           dz:elevation above surface
            dz(i,j,k)=z(i,j,k)-elev(i,j)
 210     continue
      endif

c --- pressure (function of time) at dot points
      do 220 k=1,nz   
      do 220 j=1,ny
      do 220 i=1,nx
c ---    p and pref in mbar(hpa); pp in pa
         p(i,j,k)=pref(i,j,k)+pp(i,j,k)/100.  ! PP in dot point
 220  continue

c --- calculate see level pressure
      do j=1,ny
         do i=1,nx
            tvz=tk(i,j,nz)*(1+0.608*q(i,j,nz))
            hh=z(i,j,nz)
            tv0=tvz+gamad*hh
            aa=g/rdry/gamad
            bb=tv0/tvz
            psldot(i,j)=p(i,j,nz)*(bb**aa)
         enddo
      enddo

c --- relative humidity (with respect to water)
      do 350 k=1,nz   
      do 350 j=1,ny
      do 350 i=1,nx
c ---    p in mb, t in k, qsat and q in kg/kg, rh in %
         qsat(i,j,k)=0.622 *6.112/p(i,j,k)
     :        *exp(17.67* (tk(i,j,k)-273.15)/(tk(i,j,k)-29.65))
         rh(i,j,k)=max(100.*q(i,j,k)/qsat(i,j,k),0.)
         if(rh(i,j,k).gt.100.) rh(i,j,k)=100.
350   continue

c --- convert u,v to wind speed (m/s), wind direction
      do 450 k=1,nz   
      do 450 j=1,ny
      do 450 i=1,nx
         uu=u(i,j,k)
         vv=v(i,j,k)
         call uv2wds(uu,vv,awd,aws)
         ws(i,j,k)=aws
         wd(i,j,k)=awd
 450  continue

C --- 10 m U/V
      do 451 j=1,ny
      do 451 i=1,nx
         uu=u10(i,j)
         vv=v10(i,j)
         call uv2wds(uu,vv,awd,aws)
         ws10(i,j)=aws
         wd10(i,j)=awd
 451  continue

c --- if lambert conformal projection
c --- rotate winds such that direction is with respect to true north 
c     (for LCC, POL, and MER)
      if(map.eq.1) then
         if(conefac.lt.0 .or. conefac.gt.1) then
            print *,'Error: Conefactor in LCC projection:',conefac
            stop
         endif
      elseif(map.eq.2) then
         if(conefac .ne. 1) then
            print *,'Error: Conefactor in POL projection:',conefac
            stop
         endif
      elseif (map.eq.3) then
         if(conefac.ne.0) then
            print *,'Error: Conefactor in MER projection:',conefac
            stop
         endif
      else
         print *,'Illegal map projection'
         stop
      endif

c     3-D wind
      do j=1,ny
         do i=1,nx
            dlon=rlon(i,j)-rlonc
c ---       handle straddle over dateline
            if(dlon.lt.-180.) dlon=dlon+360.
            if(dlon.gt. 180.) dlon=dlon-360.
            alat=rlat(i,j)
            do 461 k=1,nz             
               aws=ws(i,j,k)
               awd=wd(i,j,k)
               call rotate(aws,awd,uu,vv,dlon,conefac,alat)
               wd(i,j,k)=awd
               u(i,j,k) = uu
               v(i,j,k) = vv
 461        continue

C           10 m wind
            aws=ws10(i,j)
            awd=wd10(i,j)
            call rotate(aws,awd,uu,vv,dlon,conefac,alat)
            wd10(i,j)=awd
            u10(i,j) = uu
            v10(i,j) = vv

         enddo
      enddo

c --- write out subdomain characteristics to list file
      if (ifirst.eq.1) then
         write(ilg,778)nx1,nx2,ny1,ny2,rymin,rymax,rxmin,rxmax
 778     format('   from grid point x=',i4,'  to ',i4/
     :            '   from grid point y=',i4,'  to ',i4/
     :            '   latitude  range: ',f10.3,'  to: ',f10.3/ 
     :            '   longitude range: ',f10.3,'  to: ',f10.3/ )    
      endif
                
c --- write out record to output file 
      if(idate.lt.idatebeg) then
         write(ilg,*)' Skip this time:',idate
         print *,' Skip this time:',idate
         return
      elseif(idate.gt.idateend) then
         print *,'No more hour needed:',idate
         return
      elseif(idate.eq.idatebeg) then
         idatenext_out=idatebeg
         print *,' ---  Start Output at: ',idate, ' ---- '
      elseif(idate.ne.idatenext_out) then
         write(ilg,*)' Skip this time:',idate
         print *,' Skip this time:',idate
         return
      endif

      write(ilg,*)' Output Date:',idate
      print *,' Output Date:',idate

c     CALMET MM5.DAT format
      if (iformat.eq.1) then        
c        if(icount.eq.1) call calhdr(nz)
         call wrtmm5(nz)
c     CALMET MM4.DAT format
      else if (iformat.eq.2) then    
c         if (icount.eq.1) call calhdr4 (nx,ny,nz)
         call wrtmm4(nz)
c     GrADS format 
      else if (iformat.eq.3) then    
         call wrtgrd(nz)
c     Vis5D format 
      else if (iformat.eq.4) then    
c         call wrtv5d(nz)
c     NetCDF format 
      else if (iformat.eq.5) then    
c         call wrtnc(nz)
      else if (iformat.eq.6) then    
         call wrtgrib(nz)
      endif

C --- Get next output hour time stamp
      call getnext(idate,ihrstep,idatenext_out,ilg)

      return
      end

c*******************************************************************
      subroutine readhd(nx,ny,nz)

C --- CALMM5   Version: 2.7   Level: 999999

c ------------------------------------------------------------------
c purpose: read and process the first mm5 header -
c         bhi  : array store integer variables information
c         bhr  : character array describs the mif array
c         bhic : array store real variables information
c         bhrc : character array describs the mrf array
c         nx   : X-dimension of MM5 (East)
c         ny   : Y-dimension of MM5 (North)
c         nz   : Z-dimension of MM5 (Vertical)
c -----------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'
      include 'calmm5.cm4'
      include 'calmm5.cm7'

      ifirst=1

      do j=1,mxny
         do i=1,mxnx
            rainold(i,j)=0
         enddo
      enddo

      read(imm5, iostat=ier) ibflag
      
      if(ibflag.ne.0) then
         print *,'Input MM5 file is not in Version 3 format'
         print *,'Use CALMM5 V1.0 Level 020322 or earlier'
         stop
      endif

      read(imm5,iostat=ier) bhi, bhr, bhic, bhrc
      if(ier.ne.0) then
         write(*,'("Error readimm5g big header")')
         call abort()
      endif

      call hdproc(nx,ny,nz)

C     Read first hour data to get MM5 output info
      call initidpk(idv3pk,idv2dpk,n3dpk,n2dpk)

      call rdv3data(nx,ny,nz)

      call chkcomp
      call chkdat
      call chk23d

C --- Determine user specified domain
      call getdom(nx,ny)

      if(iformat.eq.1) call calhdr(nz)
      if(iformat.eq.2) call calhdr4(nx,ny,nz)

C --- Setup for outformats of GrADS, V5D, NetCDF, and GRIB
c --- if icount=1 and iformat=3, write GrADS contol file and open
c     GrADS data file
      if(iformat.eq.3) call ctlgrd

c --- if icount=1 and iformat=4, open and setup v5d data file
c      if(iformat.eq.4) call ctlv5d

c --- if icount=1 and iformat=5, open and setup NetCDF data file
c      if(iformat.eq.5) call ctlnc(dum2d)

c --- if icount=1 and iformat=6, open and setup GRIB contol file
c      nbits=14
c      if(iformat.eq.6) call setgrib

c --- if icount=1, lateql=1, and iformat=6 (Grib), setup 
c     Equal-lat/lon grid configuration
      if(iformat.eq.6 .and. ilateql.eq.1) call lateql

c     Output one hour data if within required time period
      call process(nx,ny,nz)

      ifirst=0

      return

      end

c*******************************************************************
      subroutine readhd2(nx,ny,nz)

C --- CALMM5   Version: 2.7   Level: 041109                   READHD2
C
c ------------------------------------------------------------------
c Purpose: read and process the mm5 headers in second and later MM5 
c          input file
c Zhong Wu
C 11/9/2004
C
c         bhi  : array store integer variables information
c         bhr  : character array describs the mif array
c         bhic : array store real variables information
c         bhrc : character array describs the mrf array
c         nx   : X-dimension of MM5 (East)
c         ny   : Y-dimension of MM5 (North)
c         nz   : Z-dimension of MM5 (Vertical)
c -----------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'
      include 'calmm5.cm4'
      include 'calmm5.cm7'

      read(imm5, iostat=ier) ibflag
      
      if(ibflag.ne.0) then
         print *,'Input MM5 file is not in Version 3 format'
         print *,'Use CALMM5 V1.0 Level 020322 or earlier'
         stop
      endif

      read(imm5,iostat=ier) bhi, bhr, bhic, bhrc
      if(ier.ne.0) then
         write(*,'("Error readimm5g big header")')
         call abort()
      endif

      return
      end

c **********************************************************************
       Subroutine setupmm5

C --- CALMM5   Version: 2.7   Level: 040112                     SETUPMM5
c
c --- Setup for MM5.DAT output format
c Read in variables: ioutw, ioutq, ioutc, iouti, ioutg
c                    W, humidity, cloud and rain, ice and snow, graupel
c ----------------------------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'

c --- read in output variable flags for MM5.DAT from .inp file
      read (ict,*) ioutw, ioutq, ioutc, iouti, ioutg
          
c --- check output options:
      if( (ioutg.eq.1 .and. (ioutq*ioutc*iouti).eq.0)
     :     .or. (iouti.eq.1 .and. (ioutq*ioutc).eq.0)
     :     .or. (ioutc.eq.1 .and.  ioutq.eq.0)  ) then
         write(ilg,*)' check your ouput options ioutq, ioutc, iouti'
         write(ilg,*)' cloud only if humidty'
         write(ilg,*)' ice only if cloud and humidity'
         write(ilg,*)' graupel only if ice, cloud, and humidity'
         write(ilg,*)' stop'
         stop
      endif
c --- output format 
      ioutform=81+ioutw*10+ioutq+ioutc+iouti+ioutg

c --- Read surface 2-D output flag and file name if flag=1
      read(ict,*)iosrf
      write(ilg,*)'2-D output flag: ',iosrf

      if(iosrf.eq.1) then
         read(ict,'(a)')srfile
         call getflname(srfile,nts)    

         open (isrf,file=srfile,status='unknown')

         write(ilg,45)srfile(1:nts)
 45      format(' 2-D output file: ',a)

c        Record # 1 and Record #2 in surface 2-D file
         io=isrf
         call outcomment(io,name2d)
      endif

c --- write out output variable flags to list and output files
c --- header record #3 (mm5.dat only)
      write (iop,44) ioutw, ioutq, ioutc, iouti, ioutg, iosrf
 44   format (6(i3))

      if(iosrf.eq.1) 
     &  write (isrf,44) ioutw, ioutq, ioutc, iouti, ioutg, iosrf

      write(ilg,363) ioutw, ioutq, ioutc, iouti, ioutg, iosrf
 363  format(3x,'ioutw:',i2/,
     :     3x,'ioutq:',i2/,
     :     3x,'ioutc:',i2/,
     :     3x,'iouti:',i2/,
     :     3x,'ioutg:',i2/,
     :     3x,'iosrf:',i2/)

c --- Read vertial extration range (NZ1,NZ2)
      if(iformat.eq.1) then 
         read(ict,*)nz1
         read(ict,*)nz2
         if(nz1.gt.nz2) then
            write(ilg,*)'Error: NZ1 > NZ2 ',nz1,nz2
            stop
         endif
         nzsub=nz2-nz1+1
         write(ilg,*)'Vertical range extracted:',nz1,nz2
      endif

      return
      end

c **************************************************************************
       subroutine wrtgrd(nz) 

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu
c
c --------------------------------------------------------------------------
       
c purpose : write mm5 data to output file in a format compatible with GrADS
c
c output variables available:
c        pstar+ptop: surface pressure (in mbar)
c        rain      : total rain accumulated on the ground (in cm)
c        snow          : snow cover
c        p         : presssure (in mbar)
c        z         : elevation (in meters above sea level)
c        tk         : temperature (in degrees kelvin)
c        wd          : wind direction (in degrees, with respect to true north)
c        ws         : wind speed (in m/s)
c        w         : vertical velocity (in m/s)
c        rh         : relative humidity (in %)
c        q         : vapor mixing ratio (in g/kg )
c        qc         : cloud water mixing ratio (in g/kg)
c        qr         : rain water mixing ratio (in g/kg)
c        qi         : cloud ice mixing ratio (in g/kg)
c        qs         : falling snow mixing ratio (in g/kg)
c        qg         : graupel mixing ratio  (in g/kg)

c --------------------------------------------------------------------------
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm4'

      character*8 vargrd

c --- Output 3-D vriables
      do i3d=1,n3d
         vargrd=v3name(i3d)

         if(vargrd.eq.'U       ') then
            call wrtgrd3d(u,nz)
         else if(vargrd.eq.'V       ') then
            call wrtgrd3d(v,nz)
         else if(vargrd.eq.'T       ') then
            call wrtgrd3d(tk,nz)
         else if(vargrd.eq.'Q       ') then
            call wrtgrd3d(q,nz)
         else if(vargrd.eq.'CLW     ') then
            call wrtgrd3d(qc,nz)
         else if(vargrd.eq.'RNW     ') then
            call wrtgrd3d(qr,nz)
         else if(vargrd.eq.'ICE     ') then
            call wrtgrd3d(qi,nz)
         else if(vargrd.eq.'SNOW    ') then
            call wrtgrd3d(qs,nz)
         else if(vargrd.eq.'GRAUPEL ') then
            call wrtgrd3d(qg,nz)
         else if(vargrd.eq.'W       ') then
            call wrtgrd3d(w,nz)
         else if(vargrd.eq.'PP      ') then
            call wrtgrd3d(pp,nz)
         endif

      enddo

c --- Output 2-D vriables 
      do i2d=1,n2d 
         vargrd=v2name(i2d)

         if(vargrd.eq.'TERRAIN ') then
            call wrtgrd2d(elev)
         else if(vargrd.eq.'LATITCRS') then
            call wrtgrd2d(rlatcrs)
         else if(vargrd.eq.'LONGICRS') then
            call wrtgrd2d(rloncrs)
         else if(vargrd.eq.'LAND USE') then
            call wrtgrd2d(rland)
         else if(vargrd.eq.'PSTARCRS') then
            call wrtgrd2d(pstar)
         else if(vargrd.eq.'SLP') then
            call wrtgrd2d(psldot)
         else if(vargrd.eq.'SNOWCOVR') then
            call wrtgrd2d(snow)
         else if((vargrd.eq.'PSTARCRS').and.INHYD.eq.0) then
            call wrtgrd2d(pstar)
         else if( (vargrd.eq.'RAIN CON').or.(vargrd.eq.'RAINCON '))then
            call wrtgrd2d(raincon)
         else if((vargrd.eq.'RAIN NON').or.(vargrd.eq.'RAINNON '))then
            call wrtgrd2d(rainst)
         else if((vargrd.eq.'RAIN').or.(vargrd.eq.'RAINNON ')) then
            call wrtgrd2d(rain)
         endif
      enddo

      return
      end


c **************************************************************************
       subroutine wrtgrib(nz) 

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu
c
c --------------------------------------------------------------------------
       
c purpose : write mm5 data to output file in a format compatible with GRIB
c
c output variables available:
c        pstar+ptop: surface pressure (in mbar)
c        rain      : total rain accumulated on the ground (in cm)
c        snow      : snow cover
c        p         : presssure (in mbar)
c        z         : elevation (in meters above sea level)
c        tk        : temperature (in degrees kelvin)
c        wd        : wind direction (in degrees, with respect to true north)
c        ws        : wind speed (in m/s)
c        u         : U-componentwind (in m/s, with respect to true north)
c        v         : V-componentwind (in m/s, with respect to true north)
c        w         : vertical velocity (in m/s)
c        rh        : relative humidity (in %)
c        q         : vapor mixing ratio (in g/kg )
c        qc        : cloud water mixing ratio (in g/kg)
c        qr        : rain water mixing ratio (in g/kg)
c        qi        : cloud ice mixing ratio (in g/kg)
c        qs        : falling snow mixing ratio (in g/kg)
c        qg        : graupel mixing ratio  (in g/kg)

c --------------------------------------------------------------------------
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm4'
      include 'calmm5.cm7'

      dimension x2d(mxnx,mxny)

      character*8 vargrd

c --- Output 3-D vriables
      if(n3d.le.0) goto 5000
      do i3d=1,n3d
         vargrd=v3name(i3d)
         ilvltp=107

         if(vargrd.eq.'U       ') then
            ndscale=2
            ivar_tab2=33
            call wrtgrib3d(u,nz)
         else if(vargrd.eq.'V       ') then
            ndscale=2
            ivar_tab2=34
            call wrtgrib3d(v,nz)
         else if(vargrd.eq.'T       ') then
            ndscale=1
            ivar_tab2=11
            call wrtgrib3d(tk,nz)
c         else if(vargrd.eq.'Q       ') then
c            call wrtgrd3d(q,nz)
c         else if(vargrd.eq.'CLW     ') then
c            call wrtgrd3d(qc,nz)
c         else if(vargrd.eq.'RNW     ') then
c            call wrtgrd3d(qr,nz)
c         else if(vargrd.eq.'ICE     ') then
c            call wrtgrd3d(qi,nz)
c         else if(vargrd.eq.'SNOW    ') then
c            call wrtgrd3d(qs,nz)
c         else if(vargrd.eq.'GRAUPEL ') then
c            call wrtgrd3d(qg,nz)
c         else if(vargrd.eq.'W       ') then
c            call wrtgrd3d(w,nz)
c         else if(vargrd.eq.'PP      ') then
c            call wrtgrd3d(pp,nz)
         endif

      enddo

c --- Output 2-D vriables 
 5000 continue

      do i2d=1,n2d 
         vargrd=v2name(i2d)

         if(vargrd.eq.'SLP       ') then
            iheight=0
            ilvltp=102
            ndscale=0
            ivar_tab2=2

c           Change hPa to Pa
            do j=ny1,ny2
               do i=nx1,nx2
                  x2d(i,j)=psldot(i,j)*100
               enddo
            enddo
            call wrtgrib2d(x2d)

         else if(vargrd.eq.'U10     ') then
            iheight=10
            ilvltp=105
            ndscale=2
            ivar_tab2=33
            call wrtgrib2d(u10)
         else if(vargrd.eq.'V10     ') then
            iheight=10
            ilvltp=105
            ndscale=2
            ivar_tab2=34
            call wrtgrib2d(v10)
c         if(vargrd.eq.'TERRAIN ') then
c            call wrtgrd2d(elev)
c         else if(vargrd.eq.'LATITCRS') then
c            call wrtgrd2d(rlatcrs)
c         else if(vargrd.eq.'LONGICRS') then
c            call wrtgrd2d(rloncrs)
c         else if(vargrd.eq.'LAND USE') then
c            call wrtgrd2d(rland)
c         else if(vargrd.eq.'PSTARCRS') then
c            call wrtgrd2d(pstar)
c         else if(vargrd.eq.'SNOWCOVR') then
c            call wrtgrd2d(snow)
c         else if((vargrd.eq.'PSTARCRS').and.INHYD.eq.0) then
c            call wrtgrd2d(pstar)
c         else if( (vargrd.eq.'RAIN CON').or.(vargrd.eq.'RAINCON '))then
c            call wrtgrd2d(raincon)
c         else if((vargrd.eq.'RAIN NON').or.(vargrd.eq.'RAINNON '))then
c            call wrtgrd2d(rainst)
c         else if((vargrd.eq.'RAIN').or.(vargrd.eq.'RAINNON ')) then
c            call wrtgrd2d(rain)

         endif

      enddo

      return
      end


c *******************************************************************
      subroutine wrtgrib3d(x3d,nz)

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

c --- Write selected 3-D variables to GrADS file 
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm7'

      dimension x3d(mxnx,mxny,mxnz)
      dimension x2d(mxnx,mxny)

      do ilevel=1,nlevel

         k=nz-level(ilevel)+1
         iheight=hsig(k)*10000

         do j=ny1,ny2
            do i=nx1,nx2
               x2d(i,j)=x3d(i,j,k)
            enddo
         enddo

c        Max/Min of 2-D array and take off fmin from x2d

         call wrtgrib2d(x2d)

      enddo

      return
      end

c --------------------------------------------------------------
      Subroutine wrtgrib2d(x2d)

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

c --- Get max/min of a 2-D array
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm7'

      real    x2d(mxnx,mxny)

c     Local arrays
      real    x2d2(mxnx,mxny)
      integer ix2d(mxnx,mxny)

c     Horizontal interpolate LCC to equal lat/lon if ilateql=1
      if(ilateql.eq.1) call interpeql(x2d)

c     Re-arange array
      if(ireset.eq.1) then
         nxend=nx2r
         nyend=ny2r
      else
         nxend=nx2
         nyend=ny2
      endif

      n1=nxend-nx1+1
      n2=nyend-ny1+1

      do j=ny1,nyend
         jj=j-ny1+1
         do i=nx1,nxend
            ii=i-nx1+1
            x2d2(ii,jj)=x2d(i,j)
         enddo
      enddo

c     Scale to precision (D10 scaling)
      do j=1,n2
         do i=1,n1
            aa=x2d2(i,j)
            if(ndscale.gt.0) then
               do ii=1,ndscale
                  aa=aa*10
               enddo
            elseif (ndscale.lt.0) then
               do ii=1,ndscale
                  aa=aa/10
               enddo
            endif
            x2d2(i,j)=aa
         enddo
      enddo

c     Find the Max/Min of x2d2
      xmax=-99999999
      xmin=99999999

      do j=1,n2
         do i=1,n1
            aa=x2d2(i,j)
            if(aa.gt.xmax) xmax=aa
            if(aa.lt.xmin) xmin=aa
         enddo
      enddo

c     Reference value
      refval=xmin
      dd=xmax-xmin

      if(xmax.eq.0 .and. xmin.eq.0) then
         print *,'Warning: All ZERO in data'
         print *,'refval,xmax,xmin,dd:',refval,xmax,xmin,dd
         print *,'No GRIB record for this variable'
         goto 9000 
      endif

c     No-negative scaled array
      do j=1,n2
         do i=1,n1
            ix2d(i,j)=nint(x2d2(i,j)-refval)
         enddo
      enddo

c     Figure out bits needed (No E-scaling)
      idd=int(dd)
      if(idd/2 .ne. dd/2.0) idd=idd+1

      nbits=1
 100  if(idd.ge.2) then
         idd=idd/2
         nbits=nbits+1
         goto 100
      endif

c     Total nbits needed in one record
      lens0=8
      lens1=28
      lens3=0
      lens5=4

      if(ilateql.ne.1) then
         lens2=42             ! length of GSD in Octets
      else
         lens2=32             ! length of GSD in Octets            
      endif


c     Special care for Section 4 (in bits first, then to octet) 
      ibits4=nbits*n1*n2

      nzero=0
      if(ibits4/8 .ne. ibits4/8.0) then
         nn=ibits4-ibits4/8*8
         nzero=8-nn
         ibits4=ibits4+nzero
      endif

      lens4=ibits4/8

c     Add header octets of Section 4
      lens4=lens4+11

      if(lens4/2 .ne. lens4/2.) then
         lens4=lens4+1
         nzero=nzero+8
      endif

      if(nzero.gt.16) then
         print *,'Error: too much zero filling in Section4'
         print *,' nzero:',nzero
         stop
      endif

      lenall=lens0+lens1+lens2+lens3+lens4+lens5
c      print *,'lenall:',lenall

c     Lenall should be in even octet numbers 
      nn=mod(lenall,2)
      if(nn.ne.0) then
         print *,'Error: No even octet number'
         stop
      endif

      call codesect0
      call codesect1
      call codesect2
      call codesect4(ix2d,n1,n2)

 9000 continue

      return
      end

c **************************************************************

      subroutine codesect0

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

c     Write Section 0 of a grib record
      include 'calmm5.par'
      include 'calmm5.cm7'

c -------------------------------------------------------------------

C --- Section 0 -----

C     GRIB data ID (4 bytes)

c     Position of last bytes
      laspos=0
      ichk=0

      do i=1,4
         laspos=laspos+1
         one(laspos:laspos)=gribid(i:i)
      enddo

      ioff=laspos*8
      call SBYTES (ione,lenall,ioff,24,0,1)
      laspos=laspos+3
      ioff=laspos*8
      call SBYTES (ione,nedit,ioff,8,0,1)
      laspos=laspos+1

      two(1:8)=one(1:8)

      return
      end

c **************************************************************

      subroutine codesect1

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

c     Write Section 1 (PDS) of a grib record
      include 'calmm5.par'
      include 'calmm5.cm2'
      include 'calmm5.cm7'

c -------------------------------------------------------------------

C --- Section 1 (PDS) -----

      ipds(1)=28    ! length of PSD in Octets
c      ipds(2)=3     ! Parameter Table Version number
      ipds(2)=1     ! Parameter Table Version number - For TYCO team
      ipds(3)=61    ! Center ID (EarthTech. Un-assinged number in Table 0)
c      ipds(4)=32    ! Processing ID (Un-assinged number in Table A)
      ipds(4)=1    ! Processing ID (Un-assinged number in Table A) -TYCO team
      ipds(5)=255   ! Grid ID. (255: specified in GDS)
      ipds(6)=128   ! GDS and BMS flag. igds=1 and ibms=0
      ipds(7)=ivar_tab2  ! Parameter and Unit in Table 2
      ipds(8)=ilvltp     ! Level type
      ipds(9)=iheight    ! level (height, or sigma in 1/10000)

      call getdate(idate,iyr,imon,iday,ihour)

c     Need YY from YYYY
c      if(iyr.ge.100) then
c         icentury=iyr/100 + 1
c         iyr=iyr-iyr/100*100
c      else
c         if(iyr.gt.40) then
c            icentury=20
c         else
c            icentury=21
c         endif
c      endif

c      ipds(10)=iyr       ! Year of center
c      ipds(11)=imon      ! Month
c      ipds(12)=iday      ! Day
c      ipds(13)=ihour     ! Hour
      ipds(10)=iniyr      ! Year of center
      ipds(11)=inimo      ! Month
      ipds(12)=inidy      ! Day
      ipds(13)=inihr      ! Hour
      ipds(14)=0          ! Min
      ipds(15)=1          ! Forcast units (Table 4)
c      ipds(16)=0         ! P1 period: Analysis
      ipds(16)=ihr_fst    ! P1 period: Analysis
      ipds(17)=0          ! P2 period:
      ipds(18)=0          ! Time range
      ipds(19)=0         ! Number of averaging
      ipds(20)=0          ! Missing period in averaging
      ipds(21)=icentury   ! Century of Initial time
      ipds(22)=0          ! Sub-center
      ipds(23)=ndscale    ! 10-scale factor

C     Pack PDS (28 octets) in the GRIB record after Section 0 (IS - 8 octets)

c     To even bit numbers 
      do i=1,23
         if(i.eq.1) then
            lens=3
         elseif (i.eq.9 .or. i.eq.19 .or. i.eq.23) then
            lens=2
         else
            lens=1
         endif

         ioff=laspos*8
         ibits=lens*8
c         print *,'ioff=',ioff,ibits
         call SBYTES (ingrib,ipds(i),ioff,ibits,0,1)

         laspos=laspos+lens
      enddo

      return
      end

c **************************************************************

      subroutine codesect2

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

c     Coding Section 2 (GDS) of a grib record
c     Note: Coded only for Lambert conformal project only

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm4'
      include 'calmm5.cm7'

c -------------------------------------------------------------------


C --- Section 2 (GDS) -----
C     Set lat/lon variables
      alat1=rlat(nx1,ny1)
      alon1=rlon(nx1,ny1)
      alonc=rlonc
      alatt1=truelat1
      alatt2=truelat2
      alatsp=-90.
      alonsp=0

c     Change lon to 0-360
      if(alon1.lt.0) alon1=360+alon1
      if(alonc.lt.0) alonc=360+alonc

c     alatt1 in the one closer to pole
      if(alatt1.gt.0) then
         if(alatt1.lt.alatt2) then
            aa=alatt1
            alatt1=alatt2
            alatt2=aa
         endif
      else
         if(alatt1.gt.alatt2) then
            aa=alatt1
            alatt1=alatt2
            alatt2=aa
         endif
      endif

      ilat1=nint(alat1*1000)
      ilon1=nint(alon1*1000)
      ilonc=nint(alonc*1000)
      ilatt1=nint(alatt1*1000)
      ilatt2=nint(alatt2*1000)
      ilatsp=nint(alatsp*1000)
      ilonsp=alonsp   ! 0 anyway

c     Assume one 
      if(ilatt1.gt.0) then
         ipc=0
      else
         ipc=128
      endif

c     Set igds array
      igds(1)=lens2             ! length of GSD in Octets

      igds(2)=0     ! NV
      igds(3)=255   ! PV

      if(ilateql.eq.1) goto 1000 ! Different GDS for Equal Lat/Lon Grid

      igds(4)=3                 ! Data representation typ (Table 6)
                                ! (LCC projection
      igds(5)=nxsub   ! Nx - Number of grids in X-axis
      igds(6)=nysub   ! Ny - Number of grids in Y-axis
      igds(7)=ilat1   ! La1 - Latitude of first Grid
      igds(8)=ilon1   ! Lo1 - Longitude of first Grid
      igds(9)=128     ! Resolution and component flag
      igds(10)=ilonc  ! Lov - Orientation lon
      igds(11)=nint(dxy*1000)   ! Grid size in X (meters)
      igds(12)=nint(dxy*1000)   ! Grid size in Y (meters)
      igds(13)=ipc
      igds(14)=64 
      igds(15)=ilatt1    ! true lat1 (near polar)
      igds(16)=ilatt2    ! true lat2 (away from polar)
      igds(17)=ilatsp    ! lat of South pole
      igds(18)=ilonsp    ! Lon of South pole
      igds(19)=0         ! reserved

      ngds=19

c      do i=1,ngds
c         print *,'GDS: ',i,igds(i)
c      enddo

C     Pack GDS (42 octets) in the GRIB record after Section 0 (IS - 8 octets)

c     To even bit numbers 
      do i=1,ngds
         lens=3
         if(i.ge.2 .and. i.le.4) lens=1
         if(i.eq.9 .or. i.eq.13 .or. i.eq.14) lens=1

         if(i.ge.5 .and. i.le.6) lens=2
         if(i.eq.19) lens=2

         ioff=laspos*8
         ibits=lens*8

c        Set first bit to 1 if lat/lon is negative (SH or WH)
         if((i.ge.7 .and. i.le.8) .or. 
     &      (i.ge.15 .and. i.le.18) ) then
            ilat=igds(i)
            if(ilat.lt.0) then
               ilat=abs(ilat)
               call SBYTES (ingrib,ilat,ioff,ibits,0,1)
               ii=1
               call SBYTES (ingrib,ii,ioff,1,0,1)
            else
               call SBYTES (ingrib,igds(i),ioff,ibits,0,1)
            endif
         else
            call SBYTES (ingrib,igds(i),ioff,ibits,0,1)
         endif
         laspos=laspos+lens

      enddo

      goto 5000

c     Equal Lat/Lon GDS coding
 1000 continue

      igds(4)=0       ! Data representation typ (Table 6)
                      !    (Equal lat/lon grid)
      if(ireset.eq.1) then
         igds(5)=nxsubr          ! Nx - Number of grids in a latitude circle
         igds(6)=nysubr          ! Ny - Number of grids in a longitude circle
      else
         igds(5)=nxsub          ! Nx - Number of grids in a latitude circle
         igds(6)=nysub          ! Ny - Number of grids in a longitude circle
      endif

      igds(7)=nint(rlateq1*1000)   ! La1 - Latitude of first Grid

      rloneqx=rloneq1
      if(rloneqx.lt.0) rloneqx=rloneqx+360
      igds(8)=nint(rloneqx*1000)   ! Lo1 - Longitude of first Grid

      igds(9)=128                  ! Resolution and component flag

      if(ireset.eq.1) then
         igds(10)=nint(rlateq2r*1000) ! La2 - Latitude of first Grid
      else
         igds(10)=nint(rlateq2*1000) ! La2 - Latitude of first Grid
      endif

      if(ireset.eq.1) then
         rloneqx=rloneq2r
      else
         rloneqx=rloneq2
      endif
      if(rloneqx.lt.0) rloneqx=rloneqx+360
      igds(11)=nint(rloneqx*1000)  ! Lo2 - Longitude of first Grid

      igds(12)=nint(dloneq*1000)   ! Grid size in X (degree*1000)
      igds(13)=nint(dlateq*1000)   ! Grid size in Y (degree*1000)

      igds(14)=64                  ! Scanning mode
      igds(15)=0

      ngds=15

c      do i=1,ngds
c         print *,'GDS: ',i,igds(i)
c      enddo

C     Pack GDS (32 octets) in the GRIB record after Section 0 (IS - 8 octets)

c     To even bit numbers 
      do i=1,ngds
         lens=3
         if(i.ge.2 .and. i.le.4) lens=1
         if(i.eq.9 .or. i.eq.14) lens=1

         if(i.ge.5 .and. i.le.6) lens=2
         if(i.ge.12 .and. i.le.13) lens=2

         if(i.eq.15) lens=4

         ioff=laspos*8
         ibits=lens*8

c        Set first bit to 1 if lat/lon is negative (SH or WH)
         if((i.ge.7 .and. i.le.8) .or. 
     &      (i.ge.10 .and. i.le.11) ) then
            ilat=igds(i)
            if(ilat.lt.0) then
               ilat=abs(ilat)
               call SBYTES (ingrib,ilat,ioff,ibits,0,1)
               ii=1
               call SBYTES (ingrib,ii,ioff,1,0,1)
            else
               call SBYTES (ingrib,igds(i),ioff,ibits,0,1)
            endif
         else
            call SBYTES (ingrib,igds(i),ioff,ibits,0,1)
         endif
         laspos=laspos+lens

      enddo

 5000 continue

      return
      end

c **************************************************************

      subroutine codesect4(ix2d,n1,n2)

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

c     Coding Section 4 (BDS) of a grib record
c     Note: Coded only for Lambert conformal project only

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm4'
      include 'calmm5.cm7'

      integer ix2d(mxnx,mxny)

      equivalence(aa,iaa)
c -------------------------------------------------------------------
C --- Section 4 (BDS) -----
C     Octets 1-3
      
c      print *,'laspos 1 =',laspos

      lens=3
      ioff=laspos*8
      ibits=lens*8
      lenbds=lens4
      call SBYTES (ingrib,lenbds,ioff,ibits,0,1)
      laspos=laspos+lens

c      print *,'laspos 2 =',laspos

C     Octet 4
      iflag1=0
      iflag2=nzero
      lens=1
      ioff=laspos*8
      ibits=4
      call SBYTES (ingrib,iflag1,ioff,ibits,0,1)
      ioff=ioff+4
      call SBYTES (ingrib,iflag2,ioff,ibits,0,1)

      laspos=laspos+lens

c      print *,'laspos 3 =',laspos

C     Octet 5-6
      iescale=0
      lens=2
      ioff=laspos*8
      ibits=lens*8
      call SBYTES (ingrib,iescale,ioff,ibits,0,1)
      laspos=laspos+lens

c      print *,'laspos 4 =',laspos

C     Octet 7-10, reference val
      call ibmconv(refval,is,ia,ib)
      lens=4
      ioff=laspos*8
      call SBYTES (ingrib,is,ioff,1,0,1)
      call SBYTES (ingrib,ia,ioff+1,7,0,1)
      call SBYTES (ingrib,ib,ioff+8,24,0,1)
      laspos=laspos+lens

c      print *,'laspos 5 =',laspos

C     Octet 11
      lens=1
      ioff=laspos*8
      ibits=lens*8
      call SBYTES (ingrib,nbits,ioff,ibits,0,1)
      laspos=laspos+lens

c      print *,'laspos 6 =',laspos

C     Data after Octet 12
      iup=2**nbits

      nbits_tot=0

      do j=1,n2
         do i=1,n1
            ia=ix2d(i,j)
            if(ia.gt.iup) then
               print *,'Error: Binary integer too large in array'
               print *,'iup,ia',iup,ia
               stop
            endif
            if(j.eq.1 .and. i.eq.1) then
               ioff=laspos*8
            else
               ioff=ioff+nbits
            endif
            call SBYTES (ingrib,ia,ioff,nbits,0,1)
            nbits_tot=nbits_tot+nbits
         enddo
      enddo
      ioff=ioff+nbits

      npos=nbits_tot/8
      npos_off=nbits_tot-npos*8

      nzero1=0
c     Fill zero to the end of even octet if packed bit 
c     is not at the end of even octet
      if(npos_off.ne.0) then
         ibits=8-npos_off
         ia=0
         call SBYTES (ingrib,ia,ioff,ibits,0,1)
         ioff=ioff+ibits
         npos=npos+1
         nzero1=nzero1+ibits
      endif

      laspos=laspos+npos
c      print *,'laspos 62 =',laspos

c     Fill zero to an even octet number
      npos=npos+11
      if(npos/2 .ne. npos/2.0) then
         ibits=8
         ia=0
         call SBYTES (ingrib,ia,ioff,ibits,0,1)         
         ioff=ioff+ibits
         laspos=laspos+1
c         print *,'laspos 63 =',laspos
         nzero1=nzero1+ibits
      endif

      if(nzero1.ne.nzero) then 
         print *,'Error: Fill blank bits at the end of Section 4'
         print *,nzero,nzero1
         stop
      endif

c     Check the limit of last position

c      print *,'laspos 7 =',laspos

      if(laspos.ge.mxlen) then
         print *,'Error: Grib record exceeds the limit'
         print *,'laspos,mxlen: ',laspos,mxlen
      endif

      call swap4(ingrib(2),ingrib(2),laspos-4) ! DEC_ALPHA 1998MAY06

c     Write Section 5
      two(laspos+1:laspos+4)=c7777
      laspos=laspos+4

c      print *,'laspos 8 =',laspos

      Do i=1,laspos
         ist=fputc(iop,two(i:i))
         if(ist.ne.0) go to 9998
      enddo

c      print *,'Total octets output: ',laspos

      goto 9999

 9998 print *,'Error in writing GRIB Sections 0/2 in Sub. wrtsect0'
      stop

 9999 continue
 
      return
      end

      
c *******************************************************************
      subroutine wrtgrd2d(x2d)

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

c --- Write selected 2-D variables to GrADS file 
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'

      dimension x2d(mxnx,mxny)

      if(options.eq.'direct' .or. options.eq.'DIRECT') then
         irecord=irecord+1
         write(iop,rec=irecord)((x2d(i,j),i=nx1,nx2),j=ny1,ny2)
      else
         write(iop)((x2d(i,j),i=nx1,nx2),j=ny1,ny2)            
      endif

      return
      end

c *******************************************************************
      subroutine wrtgrd3d(x3d,nz)

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

c --- Write selected 3-D variables to GrADS file 
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'

      dimension x3d(mxnx,mxny,mxnz)

      do ilevel=1,nlevel
         k=nz-level(ilevel)+1
         if(options.eq.'direct' .or. options.eq.'DIRECT') then
            irecord=irecord+1
            write(iop,rec=irecord)((x3d(i,j,k),i=nx1,nx2),j=ny1,ny2)
         else
            write(iop)((x3d(i,j,k),i=nx1,nx2),j=ny1,ny2)            
         endif
      enddo

      return
      end

c **************************************************************************
       subroutine wrtmm4 (nz) 

C --- CALMM5   Version: 2.7   Level: 999999
c
c --------------------------------------------------------------------------
       
c purpose : write mm5 data to output file in a mm4.dat format 
c
c output variables
c
c        idate     : time of the record (yymmddhh)
c        i         : x index
c        j         : y index
c        p         : presssure (in tenths mbar)
c        z         : elevation (in meters above sea level)
c        tc        : temperature (tenths of degree celsius)
c       tdd        : dew point depression (1oth of deg celsius)
c        wd        : wind direction (in degrees, with respect to true north)
c        ws        : wind speed (in knots)
c --------------------------------------------------------------------------
      include 'calmm5.par'
      include 'calmm5.cm2'
      include 'calmm5.cm4'

c --- loop over all horizontal gridpoints
      do 1 j=ny1,ny2
      do 1 i=nx1,nx2
c ---   record header       
       isnow=nint(snow(i,j))
       if(isnow.ne.1) isnow=0

       idatetmp=idate
       call k2y(idatetmp)
       write(iop,98)idatetmp,i,j,psldot(i,j),rain(i,j),isnow
c ---   loop over all half-sigma layers (from surface to top)
        do 2 k=nz,1,-1
c ---      conversion from m/s to knots
           wsknot=ws(i,j,k)*1.9438
c ---      because of mm4.dat format, limit wspeed to 2 digits
           if (wsknot.gt.99.) then
              write(ilg,*)' on ',idate,'at i,j,k: ',i,j,k
              write(ilg,*)'wind speed exceeding 99 knots: ',wsknot
              wsknot=99.
           endif
c ---      conversion from degrees kelvin to tenths of degrees celsius
           tc=tk(i,j,k)-273.15
           itc=nint(tc*10)

c ---      with mm4.dat convention (see calmet manual) 
           itc2=mod(itc,2)
c ---      freezing temperatures (odd numbers)
           if (itc.lt.0) then
              if (itc2.eq.0) then
                 itc=abs(itc)+1
              else
                 itc=abs(itc)
              endif
c ---         non freezing temperature (even numbers)
           else 
              if (itc2.ne.0) itc=itc+1
           endif

c ---      dew point temperature (bolton's formula) (celsius)
c ---               ( prevent formula to diverge for td=0 c)
           argtd=p(i,j,k)*q(i,j,k)/.622/6.112
           if (abs(argtd-1).gt.1.e-5) then
              td=243.5/(17.67/log(argtd)-1.)
           else
             td=0.
           endif
c ---      dew point depressions (celsius)
           tdd=tc-td
           if (td.gt.tc) tdd=0.
c ---      mm4.dat format (see calmet manual): (in tenths of degrees) 
c ---      dew point depression between 5.6 and 6 c are not correctly 
c ---      passed on to calmet (which cannot do anything with them anyway)
           if(tdd.ge.6) then
             itd=nint(tdd+50)
           else
             itd=nint(tdd*10)
           endif
c ---      mm4.dat format: i2
           if(itd.gt.99) itd=99

           write(iop,99) nint(p(i,j,k)*10),nint(z (i,j,k)),itc,itd,
     :                   nint(wd(i,j,k)),nint(wsknot)
2          continue
1     continue

98    format(i8,2i3,f7.1,f5.2,i2)
99    format(i5,i6,i4,i2.2,i4,i2.2)

      return
      end

c **************************************************************************
      subroutine wrtmm5 (nz) 

C --- CALMM5   Version: 2.7   Level: 999999
c
c --------------------------------------------------------------------------
       
c purpose : write mm5 data to output file in a format compatible with calmet
c
c output variables
c
c        idate     : time of the record (yymmddhh)
c        i         : x index
c        j         : y index
c        pstar+ptop: surface pressure (in mbar)
c        rain      : total rain accumulated on the ground (in cm)
c        snow          : snow cover
c        p         : presssure (in mbar)
c        z         : elevation (in meters above sea level)
c        tk        : temperature (in degrees kelvin)
c        wd        : wind direction (in degrees, with respect to true north)
c        ws        : wind speed (in m/s)
c        w         : vertical velocity (in m/s)
c        rh        : relative humidity (in %)
c        q         : vapor mixing ratio (in g/kg )
c        qc        : cloud water mixing ratio (in g/kg)
c        qr        : rain water mixing ratio (in g/kg)
c        qi        : cloud ice mixing ratio (in g/kg)
c        qs        : falling snow mixing ratio (in g/kg)
c        qg        : graupel mixing ratio  (in g/kg)

c --------------------------------------------------------------------------
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm4'

      dimension xcmp(mxcmp)
      character*9 cstr

      idateout=idate
      if(idateout.lt.1.0E9) call ck2y(idateout)

c --- Processing surface 2-D output if iosrf=1
      if(iosrf.ne.1) goto 1000
      
C      write(isrf,98)idateout

      do ivar=4,n2dpk
         id=idv2pk(ivar)
         if(id.eq.0) goto 500
         cstr=v2mm5name(ivar)
         call wrtmm52d(x2dpk(1,1,ivar),isrf,cstr,idateout)
 500     continue
      enddo

 1000 continue

c --- Processing MM53D.DAT
c --- loop over all horizontal gridpoints

      do 10 j=ny1,ny2
      do 10 i=nx1,nx2

c ---    record header       
         isnow=nint(snow(i,j))
         if (isnow.ne.1)isnow=0
         qq2=q2(i,j)*1000.
         iwd10=nint(wd10(i,j))

         write(iop,98)idateout,i,j,psldot(i,j),rain(i,j),isnow
     1        ,radswd(i,j),radlwd(i,j),t2(i,j),qq2,wd10(i,j)
     2        ,ws10(i,j),sst(i,j)
 98      format(i10,2i3,f7.1,f5.2,i2,3f8.1,f8.2,3f8.1)

c ---   loop over all half-sigma layers (from surface to top)
         do 20 k=nz2,nz1,-1

            irh=nint(rh(i,j,k))
            irh=min(100,irh)

            ipre=nint(p(i,j,k))
            izh=nint(z(i,j,k))
            atk=tk(i,j,k)
            iwd=nint(wd(i,j,k))
            aws=ws(i,j,k)

            ncmp=0
            do ii=1,mxcmp
               xcmp(ii)=0
            enddo

            if(ioutform.ge.91) awz=w(i,j,k)
            if(ioutform.ge.92) aq=q(i,j,k)*1000.

c           Compress for output formats of 93-95 
            if(ioutform.ge.93) then
               aqc=qc(i,j,k)*1000.
               aqr=qr(i,j,k)*1000.
               ncmp=ncmp+1
               xcmp(ncmp)=aqc
               ncmp=ncmp+1
               xcmp(ncmp)=aqr
            endif
            if(ioutform.ge.94) then
               aqi=qi(i,j,k)*1000.
               aqs=qs(i,j,k)*1000.
               ncmp=ncmp+1
               xcmp(ncmp)=aqi
               ncmp=ncmp+1
               xcmp(ncmp)=aqs
            endif
            if(ioutform.ge.95) then
               aqg=qg(i,j,k)*1000.
               ncmp=ncmp+1
               xcmp(ncmp)=aqg
            endif

c           No format for hydrostatic runs (81-85, not in Ver3)
            if (ioutform.lt.91) then
               write(ilg,*)'Illegal format in MM5 VER3'
               print *,'Illegal format in MM5 VER3'
               stop
c           Non-hydrostatic
            else if (ioutform.eq.91) then
               write(iop,91) ipre,izh,atk,iwd,aws,awz
            else if (ioutform.eq.92) then
               write(iop,92) ipre,izh,atk,iwd,aws,awz,irh,aq
c           compress output for formats 93-95
            else if (ioutform.ge.93) then
               call wrtcmp(ipre,izh,atk,iwd,aws,awz,irh,aq,xcmp,ncmp)
            endif
            
 20      continue
 10   continue

c --- 5 available output formats : (80's) without w; (90's) with w
 91   format( i4,i6,f6.1,i4,f5.1,f6.2)
 92   format( i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2)
c 93   format( i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2,f5.2,f5.2)
c 94   format( i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2,f5.2,f5.2,f5.2,f5.2)
c 95   format( i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2,f5.2,f5.2,f5.2,f5.2,f5.2)

      return
      end

c *******************************************************************
      subroutine timestamp(iyr,imon,iday,ihour,idate) 

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

      include 'calmm5.par'

c     Get time stamp

      idate=ihour+iday*100+imon*ifour+iyr*isix

      return
      end

c *********************************************************************
      subroutine getdate(ndate,iyr,imon,iday,ihour)

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

      include 'calmm5.par'

      iyr=ndate/isix
      imon=ndate/ifour-iyr*100
      iday=ndate/100-iyr*ifour-imon*100
      ihour=ndate-iyr*isix-imon*ifour-iday*100
      
c      print *,ndate
c      print *,iyr,imon,iday,ihour

      return
      end
c *****************************************************************
      Subroutine k2y(idate)

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

      include 'calmm5.par'

C     Change date fromYYYYMMDDHH to YYMMDDHH form
      if(idate.lt.1e9) then
         print *,'IDATE is in YYMMDDHH form already'
         return
      endif

      kk=idate/ieight

      idate=idate-kk*ieight

      return
      end

c *****************************************************************
      Subroutine ck2y(idate)

C --- CALMM5   Version: 2.7   Level: 999999

c Zhong-xiang Wu

      include 'calmm5.par'

C     Change date from YYMMDDHH to YYYYMMDDHH form
      if(idate.gt.1e9) then
         print *,'IDATE is in YYYYMMDDHH form already'
         return
      endif

      kk=idate/isix

      if(kk.lt.50) then
         idate=idate+20*ieight
      else
         idate=idate+19*ieight
      endif
   
      return
      end

c -----------------------------------------------------
      subroutine wrtmm52d(xx,iunit,vname,idateout)

C --- CALMM5   Version: 2.7   Level: 001102

c     Output MM5 surface 2-D variable to MM52D.DAT
c
c     Reverse Y-order in M2D output so that J=1 is at south (bottom) part
c     of output array. Orignal J=1 was in the north (top) part of output 
c     array.
c     Zhongxiang Wu
c     11/2/2000

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'

      dimension xx(mxnx,mxny)
      character vname*9

      write(iunit,'(i10.10,2x,a8)')idateout,vname
      scale=1.0
      if(vname.eq.'Q2      ') scale=1000.

c      do j=ny1,ny2
      do j=ny2,ny1,-1
         write(iunit,1010)(xx(i,j)*scale,i=nx1,nx2)
      enddo
 1010 format(8f10.3)

      return
      end

c -------------------------------------------------------------------
      subroutine wrtcmp(ipre,izh,atk,iwd,aws,awz,irh,aq,xcmp,ncmp)

C --- CALMM5   Version: 2.7   Level: 040921

C Purpose:
c   Compressed output of MM5.dat data record for ioutform >= 93
C   when water-related variables are all zeros in veritical profiles
C   
c Zhong-xiang Wu
c
c Changes on 09/21/2004
c
c 1. Use f6.3 instead of f5.2 in output  
c Zhong-Xiang Wu

      parameter (xzero=0.0049)

      include 'calmm5.par'
      dimension xcmp(mxcmp)

      if(ncmp.lt.1) then
         print *,'Compression Error in wrtcmp:  NCMP < 1'
         print *,'NCMP = ',ncmp
         stop
      endif

      idcmp=0
      do i=1,ncmp
         if(xcmp(i).ge.xzero) then
            idcmp=1
            goto 1000
         endif
      enddo

 1000 continue

      if(idcmp.eq.0) then
         write(iop,95)ipre,izh,atk,iwd,aws,awz,irh,aq,float(-ncmp)
      else
         write(iop,95)ipre,izh,atk,iwd,aws,awz,irh,aq,
     &       (xcmp(i),i=1,ncmp)
      endif
 95   format( i4,i6,f6.1,i4,f5.1,f6.2,i3,f5.2,5f6.3)

      return
      end

C ------------------------------------------------------------------

      SUBROUTINE GBYTES (IN,IOUT,ISKIP,NBYTE,NSKIP,N)
C          Get bytes - unpack bits:  Extract arbitrary size values from a
C          packed bit string, right justifying each value in the unpacked
C          array.
      DIMENSION IN(*), IOUT(*)
C            IN    = packed array input
C            IO    = unpacked array output
C            ISKIP = initial number of bits to skip
C            NBYTE = number of bits to take
C            NSKIP = additional number of bits to skip on each iteration
C            N     = number of iterations
C************************************** MACHINE SPECIFIC CHANGES START HERE
C          Machine dependent information required:
C            LMWD   = Number of bits in a word on this machine
C            MASKS  = Set of word masks where the first element has only the
C                     right most bit set to 1, the second has the two, ...
C            LEFTSH = Shift left bits in word M to the by N bits
C            RGHTSH = Shift right
C            OR     = Logical OR (add) on this machine.
C            AND    = Logical AND (multiply) on this machine
C          This is for Sun UNIX Fortran, DEC Alpha, and RS6000
      PARAMETER (LMWD=32)
      DIMENSION MASKS(LMWD)
      SAVE      MASKS
      DATA      MASKS /'1'X,'3'X,'7'X,'F'X, '1F'X,'3F'X,'7F'X,'FF'X,
     +'1FF'X,'3FF'X,'7FF'X,'FFF'X, '1FFF'X,'3FFF'X,'7FFF'X,'FFFF'X,
     +'1FFFF'X,       '3FFFF'X,       '7FFFF'X,       'FFFFF'X,
     +'1FFFFF'X,      '3FFFFF'X,      '7FFFFF'X,      'FFFFFF'X,
     +'1FFFFFF'X,     '3FFFFFF'X,     '7FFFFFF'X,     'FFFFFFF'X,
     +'1FFFFFFF'X,    '3FFFFFFF'X,    '7FFFFFFF'X,    'FFFFFFFF'X/
C    +'1FFFFFFFF'X,   '3FFFFFFFF'X,   '7FFFFFFFF'X,   'FFFFFFFFF'X,
C    +'1FFFFFFFFF'X,  '3FFFFFFFFF'X,  '7FFFFFFFFF'X,  'FFFFFFFFFF'X,
C    +'1FFFFFFFFFF'X, '3FFFFFFFFFF'X, '7FFFFFFFFFF'X, 'FFFFFFFFFFF'X,
C    +'1FFFFFFFFFFF'X,'3FFFFFFFFFFF'X,'7FFFFFFFFFFF'X,'FFFFFFFFFFFF'X,
C    +'1FFFFFFFFFFFF'X,   '3FFFFFFFFFFFF'X,   '7FFFFFFFFFFFF'X,
C    +                                        'FFFFFFFFFFFFF'X,
C    +'1FFFFFFFFFFFFF'X,  '3FFFFFFFFFFFFF'X,  '7FFFFFFFFFFFFF'X,
C                                             'FFFFFFFFFFFFFF'X,
C    +'1FFFFFFFFFFFFFF'X, '3FFFFFFFFFFFFFF'X, '7FFFFFFFFFFFFFF'X,
C                                             'FFFFFFFFFFFFFFF'X,
C    +'1FFFFFFFFFFFFFFF'X,'3FFFFFFFFFFFFFFF'X,'7FFFFFFFFFFFFFFF'X,
C                                             'FFFFFFFFFFFFFFFF'X/
C          IBM PC using Microsoft Fortran uses different syntax:
C     DATA MASKS/16#1,16#3,16#7,16#F,16#1F,16#3F,16#7F,16#FF,
C    + 16#1FF,16#3FF,16#7FF,16#FFF,16#1FFF,16#3FFF,16#7FFF,16#FFFF,
C    + 16#1FFFF,16#3FFFF,16#7FFFF,16#FFFFF,16#1FFFFF,16#3FFFFF,
C    + 16#7FFFFF,16#FFFFFF,16#1FFFFFF,16#3FFFFFF,16#7FFFFFF,16#FFFFFFF,
C    + 16#1FFFFFFF,16#3FFFFFFF,16#7FFFFFFF,16#FFFFFFFF/
      INTEGER RGHTSH, OR, AND
      LEFTSH(M,N) = ISHFT(M,N)
      RGHTSH(M,N) = ISHFT(M,-N)
C     OR(M,N)  = M.OR.N
C     AND(M,N) = M.AND.N
C     OR(M,N)  = IOR(M,N)
C     AND(M,N) = IAND(M,N)
C************************************** MACHINE SPECIFIC CHANGES END HERE
C          History:  written by Robert C. Gammill, jul 1972.


C          NBYTE must be less than or equal to LMWD
      ICON = LMWD-NBYTE
      IF (ICON.LT.0) RETURN
      MASK = MASKS (NBYTE)
C          INDEX  = number of words into IN before the next "byte" appears
C          II     = number of bits the "byte" is from the left side of the word
C          ISTEP  = number of bits from the start of one "byte" to the next
C          IWORDS = number of words to skip from one "byte" to the next
C          IBITS  = number of bits to skip after skipping IWORDS
C          MOVER  = number of bits to the right, a byte must be moved to be
C                   right adjusted
      INDEX = ISKIP/LMWD
      II    = MOD (ISKIP,LMWD)
      ISTEP = NBYTE+NSKIP
      IWORDS= ISTEP/LMWD
      IBITS = MOD (ISTEP,LMWD)

      DO 6 I=1,N                                                                
      MOVER = ICON-II
      IF (MOVER) 2,3,4

C          The "byte" is split across a word break.
    2 MOVEL = -MOVER
      MOVER = LMWD-MOVEL
      NP1 = LEFTSH (IN(INDEX+1),MOVEL)
      NP2 = RGHTSH (IN(INDEX+2),MOVER)
      IOUT(I) = AND (OR (NP1,NP2) , MASK)
      GO TO 5                                                                   

C          The "byte" is already right adjusted.
    3 IOUT(I) = AND (IN (INDEX+1) , MASK)
      GO TO 5                                                                   

C          Right adjust the "byte".
    4 IOUT(I) = AND (RGHTSH (IN (INDEX+1),MOVER) , MASK)

    5 II = II+IBITS
      INDEX = INDEX+IWORDS
      IF (II .LT. LMWD) GO TO 6
      II = II-LMWD
      INDEX = INDEX+1
    6 CONTINUE                                                                  

      RETURN                                                                    
      END                                                                       
C ------------------------------------------------------------------

      SUBROUTINE SBYTES (IOUT,IN,ISKIP,NBYTE,NSKIP,N)
C          Store bytes - pack bits:  Put arbitrary size values into a
C          packed bit string, taking the low order bits from each value
C          in the unpacked array.
      DIMENSION IN(*), IOUT(*)
C            IOUT  = packed array output
C            IN    = unpacked array input
C            ISKIP = initial number of bits to skip
C            NBYTE = number of bits to pack
C            NSKIP = additional number of bits to skip on each iteration
C            N     = number of iterations
C************************************** MACHINE SPECIFIC CHANGES START HERE
C          Machine dependent information required:
C            LMWD   = Number of bits in a word on this machine
C            MASKS  = Set of word masks where the first element has only the
C                     right most bit set to 1, the second has the two, ...
C            LEFTSH = Shift left bits in word M to the by N bits
C            RGHTSH = Shift right
C            OR     = Logical OR (add) on this machine
C            AND    = Logical AND (multiply) on this machine
C            NOT    = Logical NOT (negation) on this machine
C          This is for Sun UNIX Fortran
      PARAMETER (LMWD=32)
      DIMENSION MASKS(LMWD)
      SAVE      MASKS
      DATA      MASKS /'1'X,'3'X,'7'X,'F'X, '1F'X,'3F'X,'7F'X,'FF'X,
     +'1FF'X,'3FF'X,'7FF'X,'FFF'X, '1FFF'X,'3FFF'X,'7FFF'X,'FFFF'X,
     +'1FFFF'X,       '3FFFF'X,       '7FFFF'X,       'FFFFF'X,
     +'1FFFFF'X,      '3FFFFF'X,      '7FFFFF'X,      'FFFFFF'X,
     +'1FFFFFF'X,     '3FFFFFF'X,     '7FFFFFF'X,     'FFFFFFF'X,
     +'1FFFFFFF'X,    '3FFFFFFF'X,    '7FFFFFFF'X,    'FFFFFFFF'X/
      INTEGER RGHTSH, OR, AND
      LEFTSH(M,N) = ISHFT(M,N)
      RGHTSH(M,N) = ISHFT(M,-N)
C     OR(M,N)  = M.OR.N
C     AND(M,N) = M.AND.N
C     OR(M,N)  = IOR(M,N)
C     AND(M,N) = IAND(M,N)
C     NOT(M)   = .NOT.M
C***********************************************************************        

C          NBYTE must be less than or equal to LMWD
      ICON = LMWD-NBYTE
      IF (ICON .LT. 0) RETURN
      MASK = MASKS(NBYTE)
C          INDEX  = number of words into IOUT the next "byte" is to be stored
C          II     = number of bits in from the left side of the word to store it
C          ISTEP  = number of bits from the start of one "byte" to the next
C          IWORDS = number of words to skip from one "byte" to the next
C          IBITS  = number of bits to skip after skipping IWORDS
C          MOVER  = number of bits to the right, a byte must be moved to be
C                   right adjusted
      INDEX = ISKIP/LMWD
      II    = MOD(ISKIP,LMWD)
      ISTEP = NBYTE+NSKIP
      IWORDS = ISTEP/LMWD
      IBITS = MOD(ISTEP,LMWD)

      DO 6 I=1,N                                                                
      J = AND (MASK,IN(I))
      MOVEL = ICON-II
      IF (MOVEL) 2,3,4

C          The "byte" is to be split across a word break
    2 MSK = MASKS (NBYTE+MOVEL)
      IOUT(INDEX+1) = OR (AND(NOT(MSK),IOUT(INDEX+1)),RGHTSH(J,-MOVEL))
      ITEMP = AND (MASKS(LMWD+MOVEL),IOUT(INDEX+2))
      IOUT(INDEX+2) = OR(ITEMP,LEFTSH(J,LMWD+MOVEL))
      GO TO 5                                                                   

C          The "byte" is to be stored right-adjusted
    3 IOUT(INDEX+1) = OR ( AND (NOT(MASK),IOUT(INDEX+1)) , J)
      GO TO 5                                                                   

C          The "byte" is to be stored in middle of word, so shift left.
    4 MSK = LEFTSH(MASK,MOVEL)
      IOUT(INDEX+1) = OR(AND(NOT(MSK),IOUT(INDEX+1)),LEFTSH(J,MOVEL))

    5 II = II+IBITS
      INDEX = INDEX+IWORDS
      IF (II .LT. LMWD) GO TO 6
      II = II-LMWD
      INDEX = INDEX+1
    6 CONTINUE

      RETURN                                                                    
      END                                                                       
c ************************************************************************
        subroutine swap4(in,io,nn)
c swaps bytes in groups of 4 to compensate for byte swapping within
c    words which occurs on DEC (VAX) and PC machines.
c
c in - input array to be swapped
c io - ouput array with bytes swapped
c nn - number of bytes to be swapped
c       logical*1 in(1),io(1),ih          ! non_ALPHA
        character*1 in(1),io(1),ih        ! DEC_ALPHA
        do 10 i=1,nn,4
        ih=in(i)
        io(i)=in(i+3)
        io(i+3)=ih
        ih=in(i+1)
        io(i+1)=in(i+2)
        io(i+2)=ih
   10   continue
        return
        end

C *************************************************************************
        subroutine swap2(in,io,nn)
c swaps bytes in groups of 2 to compensate for byte swapping within
c    words which occurs on DEC (VAX) and PC machines.
c
c in - input array to be swapped
c io - ouput array with bytes swapped
c nn - number of bytes to be swapped
c       logical*1 in(1),io(1),ih           ! non_ALPHA
        character*1 in(1),io(1),ih         ! DEC_ALPHA
        do 10 i=1,nn,2
        ih=in(i)
        io(i)=in(i+1)
        io(i+1)=ih
   10   continue
        return
        end

c ---------------------------------------------------------------------
      subroutine ibmconv(x,is,ia,ib)

C --- CALMM5   Version: 2.7   Level: 001222

c     Convert a DEC-ALPHA float format to IBM floating format
c     Zhong-Xiang Wu
c     12/22/2000

      real x,y
      integer is,ia,ib

      if(x.lt.0) then
         is=1
      else
         is=0
      endif

      xabs=abs(x)
      y=xabs

      ia=64
      if(y.lt.1) goto 200

 100  if(y.gt.1) then
         ia=ia+1
         y=y/16.
         goto 100
      endif

      goto 300

 200  if(y.lt.1) then
         ia=ia-1
         y=y*16.
         if(ia.gt.0) goto 200
      endif

 300  c=16.0**(ia-64)
      b=xabs/c*2**24 
      ib=nint(b)

      x1=(-1)**is*ib*1.0*16.0**(ia-64)/2.**24

      return
      end

c ----------------------------------------------------------------------
      subroutine lateql

C --- CALMM5   Version: 2.7   Level: 040921

c     Purpose:
c         Determine the range of equal lat/lon output domain and 
c         lat/lon resolutions
c     Zhong-Xiang Wu
c     7/23/2001

C Changes on 040921
C 1. Add GLOBE calls
C Zhongxiang Wu

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm4'
      include 'calmm5.cm7'
!      include 'input.crd'

      parameter(mxdum=1000)
      real xdum(mxdum)
      character*8 cmapi,cmapo,datumi,datum
      character*4 c4hem

c     Find Max/Min of lat/lon and terrain at dot points
c     Note: I/J are in conventional sense, not MM5 sense
      if(nx2.gt.mxdum .or. ny2.gt.mxdum) then
         print *, 'Error: Too Large dimension (mxdum,nx2,ny2): '
         print *,  mxdum,nx2,ny2
         stop
      endif

c     Get southern edge of equal lat/lon domain
      do i=nx1,nx2
         xdum(i)=rlat(i,ny1)
      enddo
      call getmax(xdum,mxdum,nx1,nx2,nieq1,rlateq1)

c     Get northern edge of equal lat/lon domain
      do i=nx1,nx2
         xdum(i)=rlat(i,ny2)
      enddo
      call getmin(xdum,mxdum,nx1,nx2,nieq2,rlateq2)

c     Get western edge of equal lat/lon domain
      do i=ny1,ny2
         xdum(i)=rlon(nx1,i)
      enddo
      call getmax(xdum,mxdum,ny1,ny2,njeq1,rloneq1)

c     Get eastern edge of equal lat/lon domain
      do i=ny1,ny2
         xdum(i)=rlon(nx2,i)
      enddo
      call getmin(xdum,mxdum,ny1,ny2,njeq2,rloneq2)

c     Configuration of equal lat/lon domain
c     Note: Keek same nxsub and nysub
      dlateq=(rlateq2-rlateq1)/(nysub-1)
      dloneq=(rloneq2-rloneq1)/(nxsub-1)

      if(dlateq .le. 0.0001) then
         print *,'Error in Equal Lat/Lon Output'
         print *,'  No Common Latitudes Can Be Selected'
         print *,'rlateq1/rlateq2 :', rlateq1,rlateq2
         stop
      endif

      if(dloneq .le. 0.0001) then
         print *,'Error in Equal Lat/Lon Output'
         print *,'  No Common Longitude Can Be Selected'
         print *,'rloneq1/rloneq2 :', rloneq1,rloneq2
         stop
      endif

c      write(ilg,101)nieq1,nieq2,njeq1,njeq2
      write(ilg,102)rlateq1,rlateq2,rloneq1,rloneq2
      write(ilg,103)dlateq,dloneq
      print 102,rlateq1,rlateq2,rloneq1,rloneq2
      print 103,dlateq,dloneq
 101  format('Equal Lat/Lon Location:  ',4i5)
 102  format('Equal Lat/Lon Ranges:  ',4f12.4)
 103  format('Equal Lat/Lon Spacing: ',2f12.4)

C     Specail for TYCO (Minimum grid size > grdconf=0.5 degree)
      if(id_grdconf.eq.0) goto 1000

      ddlat=rlateq2-rlateq1
      ddlon=rloneq2-rloneq1

      ireset=0
      if(dlateq.lt.grdconf .and. ddlat.gt.grdconf*3) then
         dlateq=grdconf
         ireset=1
      endif
      if(dloneq.lt.grdconf .and. ddlon.gt.grdconf*3) then
         dloneq=grdconf
         ireset=1
      endif

      if(ireset.eq.0) goto 1000

C     Reset grid configuration
      nysubr=int(ddlat/dlateq)+1
      nxsubr=int(ddlon/dloneq)+1
      nx2r=nx1+nxsubr-1
      ny2r=ny1+nysubr-1

      rlateq2r=rlateq1+(nysubr-1)*dlateq
      rloneq2r=rloneq1+(nxsubr-1)*dloneq

      rxminr=rloneq1
      rxmaxr=rloneq2r
      ryminr=rlateq1
      rymaxr=rlateq2r      

C     Check consistency
      if(nxsubr.gt.nxsub .or. nysubr.gt.nysub) then
         write(ilg,201)nxsub,nxsubr,nysub,nysubr
 201     format('Error in Reset grid(nxsub/nysub):',4i5)
         stop
      endif

      if(nx2r.gt.nx2 .or. ny2r.gt.ny2) then
         write(ilg,202)nx2,nx2r,ny2,ny2r
 202     format('Error in Reset grid(nx2/ny2):',4i5)
         stop
      endif

      if(rlateq2r.gt.rlateq2 .or.
     &   rloneq2r.gt.rloneq2) then
         write(ilg,203)rlateq2,rlateq2r,rloneq2,rloneq2r
 203     format('Error in Reset grid(lat2/lon2):',4f12.4)
         stop
      endif

      write(ilg,*)' *********************************** '
      write(ilg,*)'Reset lat/lon grid - Special for TYCO team'
c      write(ilg,101)nieq1,nieq2,njeq1,njeq2
      write(ilg,102)rlateq1,rlateq2r,rloneq1,rloneq2r
      write(ilg,103)dlateq,dloneq
      write(ilg,104)nx1,nx2r,nxsubr,ny1,ny2r,nysubr
      write(ilg,*)' *********************************** '
 104  format('Grid Number Settings (NX/NY):',6i5)

      print *,' *********************************** '
      print *,'Reset lat/lon grid - Special for TYCO team'
c      write(ilg,101)nieq1,nieq2,njeq1,njeq2
      print 102,rlateq1,rlateq2r,rloneq1,rloneq2r
      print 103,dlateq,dloneq
      print 104,nx1,nx2r,nxsubr,ny1,ny2r,nysubr
      print *,' *********************************** '

 1000 continue

c     Get weighting functions for four  LCC grids surrounding 
c     a equal lat/lon grid
      if(ireset.eq.0) then
         nxend=nx2
         nyend=ny2
      else
         nxend=nx2r
         nyend=ny2r
      endif

c --- NEW MAP TRANSFORMATION ROUTINES -----
c --- Initialize GLOBE
      cmapi='LL'
      iutmi=999.9
      tmsone=999.9
      rlat1i=truelat1
      rlat2i=truelat2
      rlat0i=rlatc
      rlon0i=rlonc
      feasti = 0.
      fnorti = 0.
      
      cmapo='LCC'
      iutmi=999.9
      tmsone=999.9
      rlat1=truelat1
      rlat2=truelat2
      rlat0=rlatc
      rlon0=rlonc
      feasto = 0.
      fnorto = 0.
            
c --- Set translation vectors
      call GLOBE(cmapi,iutmi,tmsone,rlat1i,rlat2i,rlat0i,rlon0i,
     &            feasti,fnorti,
     &            cmapo,iutm,tmsone,rlat1,rlat2,rlat0,rlon0,
     &            feast,fnorth,
     &            caction,vecti,vecto)
c --- Done Setting translation vectors

      do j=ny1,nyend

         flat=rlateq1+(j-ny1)*dlateq

         do i=nx1,nxend
            flon=rloneq1+(i-nx1)*dloneq

            call mapg2l(rlatc,rlonc,truelat1,truelat2,flat,flon,xeq,yeq)

c ---       Translate coordinates                     --- call GLOBE
            datumi='WSG-84'
            call GLOBE(ilg,caction,datumi,vecti,datum,vecto,
     &                flon,flat,xeq,yeq,izone,c4hem)
c ---       Done Translating coordinates                     --- call GLOBE

            fnieq=(xeq-xorg)/dxy+1
            fnjeq=(yeq-yorg)/dxy+1

            knieq=int(fnieq)
            knjeq=int(fnjeq)

c           Note: Order of weighting function array:
c           *     |       *
c           w4
c           -     *       -
c           w3
c           *  w1  |  w2  * 
       
            weq(1,i,j)=1.0-(fnieq-knieq)
            weq(2,i,j)=1.0-weq(1,i,j)
            weq(3,i,j)=1.0-(fnjeq-knjeq)
            weq(4,i,j)=1.0-weq(3,i,j)
            
            fijeql(1,i,j)=fnieq
            fijeql(2,i,j)=fnjeq
            
c           Check Equal Lat/Lon config settings
c            write(18,181)i,j,flat,flon,xeq,yeq,
c     &         (fijeql(ii,i,j),ii=1,2),(weq(ii,i,j),ii=1,4)
c 181        format(2i4,2f12.4,2f10.3,2f9.3,4f8.4)
         enddo
      enddo

      return
      end

c  -------------------------------------------------------
      subroutine getmax(xdum,mxdum,n1,n2,nmx,xmx)

C --- CALMM5   Version: 2.7   Level: 010723

c     Get maximum for a one-D array between two positions
c     Zhong-Xiang Wu
c     7/23/2001

      dimension xdum(mxdum)

      xmx=-999999.
      nmx=-999999

      do i=n1,n2
         if(xdum(i).gt.xmx) then
            xmx=xdum(i)
            nmx=i
         endif
      enddo

      if(nmx.ne.-999999) then
c         print *,'Found maximum: ',xmx,nmx
      else
         print *,'Maximum not found. Check your data'
      endif

      return
      end

c  -------------------------------------------------------
      subroutine getmin(xdum,mxdum,n1,n2,nmx,xmx)

C --- CALMM5   Version: 2.7   Level: 010723

c     Get maximum for a one-D array between two positions
c     Zhong-Xiang Wu
c     7/23/2001

      dimension xdum(mxdum)

      xmx=999999.
      nmx=999999

      do i=n1,n2
         if(xdum(i).lt.xmx) then
            xmx=xdum(i)
            nmx=i
         endif
      enddo

      if(nmx.ne.999999) then
c         print *,'Found the minimum: ',xmx,nmx
      else
         print *,'Minimum not found. Check your data'
      endif

      return
      end

c --------------------------------------------------------------
      subroutine interpeql(x2d)

C --- CALMM5   Version: 2.7   Level: 010723

c Purpose: Horizontal interpolate LCC to equal lat/lon
c
c Zhong-Xiang Wu
c 7/23/2001

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm7'

      dimension x2d(mxnx,mxny)
      dimension xtmp(mxnx,mxny),wgt(4)

      if(ireset.eq.1) then
         nxend=nx2r
         nyend=ny2r
      else
         nxend=nx2
         nyend=ny2
      endif

      do j=ny1,nyend
         do i=nx1,nxend

            fnieq=fijeql(1,i,j)
            fnjeq=fijeql(2,i,j)

            ni1=int(fnieq)
            nj1=int(fnjeq)
            ni2=ni1+1
            nj2=nj1+1

            x1=x2d(ni1,nj1)
            x2=x2d(ni2,nj1)
            x3=x2d(ni1,nj2)
            x4=x2d(ni2,nj2)

            do ii=1,4
               wgt(ii)=weq(ii,i,j)
            enddo

            xtmp(i,j)=(x1*wgt(1)+x2*wgt(2))*wgt(3)+
     &                (x3*wgt(1)+x4*wgt(2))*wgt(4)

c            print *,i,j,ni1,ni2,nj1,nj2
c            print 9,i,j,xtmp(i,j),x1,x2,x3,x4,wgt(1)
c     &             ,wgt(2),wgt(3),wgt(4)
c 9          format(2i4,9f8.1)
         enddo
      enddo

c     Put back to x2d array with value at equal lat/lon grid location
      do j=ny1,nyend
         do i=nx1,nxend
            x2d(i,j)=xtmp(i,j)
         enddo
      enddo

      return
      end

C -----------------------------------------------
      Subroutine interpw(dum3d,nx,ny,nz)

C --- CALMM5   Version: 2.7   Level: 999999

C     Get W at U/V level
      dimension dum3d(nx,ny,nz)

      do j=1,ny
         do i=1,nx
            do k=2,nz
               dum3d(i,j,k-1)=(dum3d(i,j,k)+dum3d(i,j,k-1))/2.0
            enddo
         enddo
      enddo

      nz=nz-1

      return
      end

C ------------------------------------------------------------
      Subroutine rdv3data(nx,ny,nz)

C --- CALMM5   Version: 2.7   Level: 999999

C     Read one variable from MM5 output
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'
      include 'calmm5.cm4'
      
      character cstr*9
      real dum3d(mxnx,mxny,mxnz),dum2d(mxnx,mxny),dum1d(mxnz)
      logical debug

c      debug=.TRUE.
      debug=.FALSE.
      iflend=0

      read(imm5, iostat=ier) isflag
      if(ier.ne.0) then
         iflend=1
         print *,'Reach end of file:',iflend,' ',infile(1:nti)
         return
      endif

      if(isflag.eq.1) goto 1050

      if(isflag.eq.2) then
         print *,'Reach the end of time period: ',isflag
         return
      else
         print *,'Warnning: File not properly closed'
         print *,'       Check output file CAREFULLY'
         stop
      endif

 1050 continue

      n3d=0
      n2d=0
      n1d=0

c --- initialize rain array 
c     (rain array is the rainfall for this hour or this output period)
      do 218 j=1,ny
         do 218 i=1,nx
           rain(i,j)=0.
 218  continue

 1000 read(imm5,iostat=ier) ndim5, start_index, end_index 
     &   ,time, staggering, ordering, current_date, name
     &   ,units, description
      if(ier.ne.0) then
         iflend=1
         print *,'Reach end of file:',iflend,' ',infile(1:nti)
         return
      endif

      read(current_date,310)iyr,imo,idy,ihr,imi
 310  format(i4.4,4(1x,i2.2))
      call julday(ilg,iyr,imo,idy,ijul)
      if(imi.gt.35) then
         call incr(ilg,iyr,ijul,ihr,1)
         call grday(ilg,iyr,ijul,imo,idy)
      endif

      call timestamp(iyr,imo,idy,ihr,idate) 
      if(idate.lt.1.0e9) call ck2y(idate)

      if (debug) then
         print*, 'ndim5: ', ndim5
         print*, 'start_index: ', start_index
         print*, 'end_index: ', end_index
         print*, 'time: ', time
         print*, 'staggering: #'//staggering//'#'
         print*, 'ordering: #'//ordering//'#'
         print*, 'date/time: #'//current_date//'#',idate
         print*, 'name: #'//name//'#'
         print*, 'units: #'//units//'#'
         print*, 'description: #'//description//'#'
      endif

C     Check time consitancy
      if(ifirst.ne.1 .and. n3d.eq.0) then
         if(idate.ne.idatenext) then
            write(ilg,303)idate,idatenext
            print 303,idate,idatenext
 303        format(' Not the date wanted:',2i14)
         endif
      endif

      n1=end_index(1)
      n2=end_index(2)
      n3=end_index(3)
      n4=end_index(4)

c      if(allocated(data)) deallocate(data)
c      allocate(data(n1,n2,n3,n4))
      if(n4.gt.1) then
         print *,'Error: Maximum dimension - 3:',n4
         stop
      endif
      in=imm5
      if(ndim5.eq.3) then
         call rd3d(in,dum3d,n1,n2,n3)
      elseif(ndim5.eq.2) then
         call rd2d(in,dum2d,n1,n2)
      elseif(ndim5.eq.1) then
         call rd1d(in,dum1d,n1)
      else
         print *,'Illegal nidm5:',ndim5
      endif

C     Sigma levels
      if(ndim5.eq.1 .and. name(1:6).eq.'SIGMAH' 
     &   .and.ifirst.eq.1) then
         do k=1,n1
            hsig(k)=dum1d(k)
         enddo
         nsigma=n1
      endif

      read(imm5, iostat=ier) isflag

      if(isflag.eq.1) then
         goto 1000
      elseif(isflag.eq.2) then
C        Get next hour time stamp for nest reading
         iyr2=iyr
         imo2=imo
         idy2=idy
         ihr2=ihr
         imi2=imi
         ijul2=ijul
         call incr(ilg,iyr2,ijul2,ihr2,1)
         call grday(ilg,iyr2,ijul2,imo2,idy2)
         call timestamp(iyr2,imo2,idy2,ihr2,idatenext) 

C        Get current hour rainfall
c ---    Total rain = raincon+rainst
c        Note: raincon/rainst is accumulative rainfall

         do j=1,ny
            do i=1,nx
               rrcon=raincon(i,j)
               rrst=rainst(i,j)
               if(rrcon.lt.0 .or. rrcon.gt.1000) then
c                print *,'Error in rrcon, set to zero: ',rrcon
                  rrcon=0
               endif
               if(rrst.lt.0 .or. rrst.gt.1000) then
c                 print *,'Error in rrcon, set to zero: ',rrst
                  rrst=0
               endif

               rainacu=rrcon+rrst
               rainpre=rainold(i,j)

               if(rainpre.lt.0 .or. rainpre.gt.1000) then
c                print *,'Error in previous hour rainfall : ',rainpre
c                print *,'Set previous hour rainfall to zero'
                  rainpre=0
               endif

               raincur=rainacu-rainpre

               if(raincur.lt.0.0) raincur=0.0

c              rainfall for current hour
               rain(i,j)=raincur

c              update rainold array
               rainold(i,j)=rainacu
               
            enddo
         enddo

         return
      else
         print *,'Error: illegal flag in rdv3data'
      endif

      return
      end

C ------------------------------------------------------------
      Subroutine rd3d(in,dum3d,n1,n2,n3)

C --- CALMM5   Version: 2.7   Level: 999999

C Read one 3D variables from MM5 V3 output
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'
      include 'calmm5.cm4'

      character cstr*9
      dimension dum3d(n1,n2,n3)

C     Note: dum3d still in MM5 I/J convention.  I/J reversed after
C           call passdata3d or pass3d

      read(in)dum3d

      if(staggering(1:1).eq.'C') call cleanrows(dum3d,n1,n2,n3)
      if(name(1:8).eq.'W       ') call interpw(dum3d,n1,n2,n3)

      if(staggering(1:1).eq.'C') call interpdot (dum3d,n1,n2,n3)

      call noblank2(name,nt_name)

      do ivar=1,n3dpk
         cstr=v3mm5namepk(ivar)
         call noblank2(cstr,nt)

         if(nt.ne.nt_name) goto 1000 

         if(name(1:nt).eq.cstr(1:nt)) then
            n3d=n3d+1
            if(n3d.gt.mxn3d) then
               print *,'Error: Too many 3D var.',n3d,mxn3d
               stop
            endif
            v3mm5name(ivar)=name

            idv3pk(ivar)=1
            call passdata3d(dum3d,n1,n2,n3,ivar)  ! Reverse I/J

            goto 2000
         endif

 1000    continue
      enddo

c      print *,'This 3-D variable is not needed:',name(1:nt)

 2000 continue
      return
      end

C ------------------------------------------------------------
      Subroutine rd2d(in,dum2d,n1,n2)

C --- CALMM5   Version: 2.7   Level: 999999

C Read 3D variables from MM5 V3 output
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'
      include 'calmm5.cm4'

      character cstr*9
      dimension dum2d(n1,n2)
C     Note: dum2d still in MM5 I/J convention.  I/J reversed after
C           call passdata2d or pass2d

      read(in)dum2d

      if(staggering(1:1).eq.'C') call cleanrows(dum2d,n1,n2,1)

      if(ifirst.eq.1) then  ! Cross point 
         if(name.eq.'TERRAIN ') then
            call pass2d(elevcrs,dum2d,n1,n2)
            call interpdot(dum2d,n1,n2,1)
            call pass2d(elev,dum2d,n1,n2)
         else if(name.eq.'LATITCRS') then
            call pass2d(rlatcrs,dum2d,n1,n2)  ! Reverse I/J
            call interp_latlon(rlat,rlatcrs,n2,n1)
         else if(name.eq.'LONGICRS') then
            call pass2d(rloncrs,dum2d,n1,n2)
            call lonunif(rloncrs,n2,n1)
            call interp_latlon(rlon,rloncrs,n2,n1)
         else if(name.eq.'LAND USE') then
            call pass2d(rland,dum2d,n1,n2)
         else if(name.eq.'PSTARCRS') then
            call pass2d(pstarcrs,dum2d,n1,n2)
            call interpdot (dum2d,n1,n2,1)
            call pass2d(pstar,dum2d,n1,n2)
         endif
      endif

      call noblank2(name,nt_name)

      do ivar=1,n2dpk
         cstr=v2mm5namepk(ivar)
         call noblank2(cstr,nt)
         if(nt.ne.nt_name) goto 1000 

         if(name(1:nt).eq.cstr(1:nt)) then
            n2d=n2d+1
            if(n2d.gt.mxn2d) then
               print *,'Error: Too many 2D var.',n2d,mxn2d
               stop
            endif
            v2mm5name(ivar)=name
            idv2pk(ivar)=1

            if(staggering(1:1).eq.'C') 
     &         call interpdot (dum2d,n1,n2,1)

            call passdata2d(dum2d,n1,n2,ivar)

            goto 2050

         endif

 1000    continue

      enddo

c      Print *,'This 2-D variable is not needed:',name

 2050 continue
      return
      end

C ------------------------------------------------------------
      Subroutine rd1d(in,dum1d,nx,n1d)

C --- CALMM5   Version: 2.7   Level: 999999

C Read 3D variables from MM5 V3 output
      include 'calmm5.par'
      include 'calmm5.cm3'
      include 'calmm5.cm4'

      dimension dum1d(nx)
C     Note: dum2d still in MM5 I/J convention

      read(in)dum1d

      return
      end

C ------------------------------------------------------------------
      Subroutine chkdat

C --- CALMM5   Version: 2.7   Level: 999999

C Check completion of selected data
      include 'calmm5.par'
      include 'calmm5.cm1'

      if(n3d.ne.n3dpk) then
         do i=1,n3dpk
            id=idv3pk(i)
            if(id.eq.0) then
               write(ilg,101)id,v3mm5namepk(i),v3mm5name(i)
c               print 101,id,v3mm5namepk(i),v3mm5name(i)
 101           format('Missing 3D Var:',i3,2a10)
            endif
         enddo
      elseif(n2d.ne.n2dpk) then
         do i=1,n2dpk
            id=idv2pk(i)
            if(id.eq.0) then
               write(ilg,102)id,v2mm5namepk(i),v2mm5name(i)
c               print 102,id,v2mm5namepk(i),v2mm5name(i)
 102           format('Missing 2D Var:',i3,2a10)
            endif
         enddo
      else
c         print *,'All required 3-D abnd 2-D variables have been read'
      endif

      return
      end

C ----------------------------------------------------
      Subroutine initidpk(idv3pk,idv2dpk,n3dpk,n2dpk
     &     ,v3mm5nave,v2dmm5name)

C --- CALMM5   Version: 2.7   Level: 999999

C     Initialize idpk arrays
      dimension idv3pk(n3dpk),idv2pk(n2dpk)
      character*9 v3mm5name(n3dpk),v2mm5name(n2dpk)
      character*9 strb

      data strb/'        '/

      do i=1,n3dpk
         idv3pk(i)=0
         v3mm5name(i)=strb
      enddo

      do i=1,n2dpk
         idv2pk(i)=0
         v2mm5name(i)=strb
      enddo
      
      return
      end

C --------------------------------------------------------------------
      subroutine getdom(nx,ny)

C ---   CALMM5 Version: 2.7  Level 040921
C 
C Determine user specified extraction domain
      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm4'
!      include 'input.crd'
      
c --- For coordinate transformations
      character*8 cmapi,cmapo,datumi,datum
      character*12 caction
      character*4 c4hem
      real*8 vecti(9),vecto(9)

      data dserr/10/

c --- No need to select domain if iselect=2 (input J/I limits in control) 
c     Set up J/I limits to MM5 domain limits if larger than MM5 domain
      if(iselect.eq.2) then
          if(nx2.gt.nx) then
            write(ilg,*)'NX2 > NX. Set NX2=NX: ',nx2,nx
            nx2=nx
         endif
         if(ny2.gt.ny) then
            write(ilg,*)'NY2 > NY. Set NY2=NY: ',ny2,ny
            ny2=ny
         endif

         goto 560 

      endif

c --- Otherwise, islect=1; selection based on input Lat/lon
c --- After ifirst, rlat,rlon must have been filled.
c     Determine which gridpoints are within selected domain
c     (based on dot point lat/lon, not cross point's)

c initialize extraction boundaries
      if(iselect.eq.1) then
         rymin=90.
         rymax=-90.
         rxmin=180.
         rxmax=-180.
         nx1=mxnx
         nx2=0
         ny1=mxny
         ny2=0
      endif

      if(ichg.eq.1) then
         if(rlonmin.lt.0) rlonmin=rlonmin+360
         if(rlonmax.lt.0) rlonmax=rlonmax+360
      endif

      do 213 j=1,ny
         do 213 i=1,nx
            flat=rlat(i,j)
            flon=rlon(i,j)

            if ((flat.ge.rlatmin).and.(flat.le.rlatmax)) then 
               if ((flat.lt.rymin).and.(j.lt.ny1)) then
                  rymin=flat
                  ny1=j
               else if ((flat.gt.rymax).and.(j.gt.ny2)) then
                  rymax=flat
                  ny2=j
               endif
            endif

            if ((flon.ge.rlonmin) .and. (flon.le.rlonmax)) then 
               if ((flon.lt.rxmin).and.(i.lt.nx1)) then
                  rxmin=flon
                  nx1=i
               else if ((flon.gt.rxmax).and.(i.gt.nx2)) then
                  rxmax=flon
                  nx2=i
               endif
            endif
 213  continue

 560  continue

      if(ny1.gt.1)then
         ny11=ny1
      else
         ny11=2
      endif
        
      if (nx1.gt.1)then
         nx11=nx1
      else
         nx11=2
      endif

c     Discourage users to select MM5 domain edge grids
      if(nx1.eq.1) then
         write(ilg,*)'Note: Seleted MM5 left edge grids'
         write(ilg,*)'      Not recommanded'
      endif
      if(nx2.ge.nx) then
         write(ilg,*)'Note: Selected MM5 right edge grids'  
         write(ilg,*)'      Not recommanded'
      endif
     
      if(ny1.eq.1) then
         write(ilg,*)'Note: Selected MM5 bottom edge grids'  
         write(ilg,*)'      Not recommanded'
      endif
      if(ny2.ge.ny) then
         write(ilg,*)'Note: Selected MM5 top edge grids'  
         write(ilg,*)'      Not recommanded'
      endif
     
c     find Max/Min of lat/lon and terrain at dot points
      flatmin=99999
      flatmax=-99999
      flonmin=99999
      flonmax=-99999
      elevmin=99999
      elevmax=-99999
      do j=ny1,ny2
         do i=nx1,nx2
            if(rlat(i,j).gt.flatmax) flatmax=rlat(i,j)
            if(rlat(i,j).lt.flatmin) flatmin=rlat(i,j)
            if(rlon(i,j).gt.flonmax) flonmax=rlon(i,j)
            if(rlon(i,j).lt.flonmin) flonmin=rlon(i,j) 
            if(elev(i,j).gt.elevmax) elevmax=elev(i,j)
            if(elev(i,j).lt.elevmin) elevmin=elev(i,j) 
         enddo
      enddo

c     set max/min lat/lon for iselect=2 
      if(iselect.eq.2) then
         rxmin=flonmin
         rxmax=flonmax
         rymin=flatmin
         rymax=flatmax
      endif

c --- NEW MAP TRANSFORMATION ROUTINES -----

c --- Initialize GLOBE 
      cmapi='LL'
      iutmi=999.9
      tmsone=999.9
      rlat1i=truelat1
      rlat2i=truelat2
      rlat0i=rlatc
      rlon0i=rlonc
      feasti = 0.
      fnorti = 0.
      
      cmapo='LCC'
      iutmi=999.9
      tmsone=999.9
      rlat1=truelat1
      rlat2=truelat2
      rlat0=rlatc
      rlon0=rlonc
      feast = 0.
      fnorth = 0.
            
c --- Set translation vectors
      call GLOBE(cmapi,iutmi,tmsone,rlat1i,rlat2i,rlat0i,rlon0i,
     &            feasti,fnorti,
     &            cmapo,iutm,tmsone,rlat1,rlat2,rlat0,rlon0,
     &            feast,fnorth,
     &            caction,vecti,vecto)
c --- Donr Setting translation vectors

c --- SW dot point LCC locations (km)
c     (Need further coding for polar/mercator projections)

      rlatsw=rlat(nx1,ny1)
      rlonsw=rlon(nx1,ny1)

      if(map.eq.1) then
c         call mapg2l(rlatc,rlonc,truelat1,truelat2
c     &     ,rlatsw,rlonsw,xorg,yorg)
c ---       Translate coordinates                     --- call GLOBE
            datumi='NWS-84'
            datum='NWS-84'
	    write(ilg,*)'Before Globe Call '
            call GLOBE(ilg,caction,datumi,vecti,datum,vecto,
     &                flon,flat,xorg,yorg,izone,c4hem)
c ---       Done Translating coordinates              --- call GLOBE
      else
         xorg=x1dmn
         yorg=y1dmn
      endif
         
      nxsub=nx2-nx1+1
      nysub=ny2-ny1+1
      write(ilg,214)nx1,nx2,ny1,ny2,nxsub,nysub
      write(ilg,215)rlatsw,rlonsw,xorg,yorg
 214  format('Selected domain I: ',2i5,/,
     &       '                J: ',2i5,/
     &       '  Number of Grids: ',2i5)
 215  format('Selected domain SW lat/lon: ',2f12.3,/,
     &       'Selected domain SW     X/Y: ',2f12.3)

c --- Check SW corner coordinates (grid 1,1)
      flat=rlat(1,1)
      flon=rlon(1,1)

      if(map.eq.1) then 
C        call mapg2l(rlatc,rlonc,truelat1,truelat2,flat,flon,xorg,yorg)
c ---       Translate coordinates                     --- call GLOBE
         datumi='NWS-84'
         datum='NWS-84'
         call GLOBE(ilg,caction,datumi,vecti,datum,vecto,
     &        flon,flat,xorg,yorg,izone,c4hem)
c ---    Done Translating coordinates              --- call GLOBE
      endif

      dd1=abs(xorg-x1dmn)
      dd2=abs(yorg-y1dmn)

      if(dd1.gt.dserr .or. dd2.gt.dserr) then
         write(ilg,216)dd1,dd2,xorg,yorg,x1dmn,y1dmn
         print 216,dd1,dd2,xorg,yorg,x1dmn,y1dmn
 216     format('Warning: SW dot position error:',2f10.3,/,
     &          '         SW X/Y from lat/lon:  ',2f12.3,/,
     &          '         SW X/Y from I/J:      ',2f12.3,/)      
      endif

      return
      end

C -----------------------------------------------------      
      Subroutine chkcomp

C --- CALMM5   Version: 2.7   Level: 999999

      include 'calmm5.par'
      include 'calmm5.cm1'

c --- Check compatibility between output flags and mm5 available variables
      write(ilg,*)
     : 'check inconsistency between mm5 options and output selections' 

c	do k=1,n3dpk
c           write(18,*)k,idv3pk(k)
c	enddo

      if(ioutc.eq.1) then
         if(idv3pk(5).eq.0 .and. idv3pk(6).eq.0) then
            write(ilg,*)
     :        ' warning: no explicit cloud or rain in mm5 simulation'
            istop =1
         endif
      endif

c      write(18,*)' istop1 =',istop

      if(ifdry.eq.1  .and. ioutq.eq.1) then
         write(ilg,*)' warning: dry mm5 simulation.'
         istop=1
      endif

      if(iouti.eq.1 ) then
         if(idv3pk(7).eq.0  .and. idv3pk(8).eq.0 ) then
         write(ilg,*)' warning: no ice or snow in mm5 simulation'
         istop =1
         endif
      endif

c      write(18,*)' istop2 =',istop

      if(idv3pk(9).eq.0 .and. ioutg.eq.1 ) then
         write(ilg,*)' warning: no graupel in mm5 simulation'
         istop =1
      endif

c      write(18,*)' istop3 =',istop

      if (istop.eq.1) then
         write(ilg,*)
     :        'Not all variables requested by user are available in mm5'
         write(ilg,*) 'stop'
         stop
      else
         write(ilg,*) 'end check : no incompatibility found. good! '
         write(ilg,*)
      endif

      return
      end

c ---------------------------------------------------------------
      Subroutine chk23d

C --- CALMM5   Version: 2.7   Level: 999999

      parameter(iprint=0)

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'
      include 'calmm5.cm3'
      include 'calmm5.cm4'
c Check 2/3-D master arrays

C     Check master arrays

      if(iprint.eq.0) return
      k=nzmm5   
      do ivar=1,n3dpk
         write(50,501)ivar,v3mm5name(ivar),k,idv3pk(ivar)
 501     format('3-D variable Name at level:',i5,a12,' K=',2i3)
         do j=1,nymm5
            if(ivar.ge.5 .and. ivar.le.10) then  ! Q,CNW in g/kg
               write(50,502)(x3dpk(i,j,k,ivar)*1000.,i=1,nxmm5)
            elseif(ivar.eq.11) then   !PP in mb
               write(50,502)(x3dpk(i,j,k,ivar)/100.,i=1,nxmm5)
            else
               write(50,502)(x3dpk(i,j,k,ivar),i=1,nxmm5)
            endif
 502        format(10f8.2)
         enddo
      enddo

      do ivar=1,n2dpk
         write(50,503)ivar,v2mm5name(ivar),idv2pk(ivar)
 503     format('2-D variable Name at level:',i5,a12,2x,2i3)
         do j=1,nymm5
            write(50,502)(x2dpk(i,j,ivar),i=1,nxmm5)
         enddo

      enddo
      
      return
      end
C ---------------------------------------------------------
      Subroutine initdata

C --- CALMM5   Version: 2.7   Level: 999999

      include 'calmm5.par'
      include 'calmm5.cm4'

C     Initialize master arrays
      do ivar=1,n3dpk
         do k=1,mxnz
            do j=1,mxny
               do i=1,mxnx
                  x3dpk(i,j,k,ivar)=0
               enddo
            enddo
         enddo
      enddo

      do ivar=1,n2dpk
         do j=1,mxny
            do i=1,mxnx
               x2dpk(i,j,ivar)=0
            enddo
         enddo
      enddo

      return
      end

C -----------------------------------------------
      Subroutine uv2wds(uu,vv,wd,ws)

C --- CALMM5   Version: 2.7   Level: 999999

C     Convert U/V to WD/WS
C     Zhong Wu
      ws=sqrt(uu*uu+vv*vv)
      if(ws.gt.1.e-9) then
         xwd=270.-57.295778*atan2(vv,uu)
         wd=amod(xwd,360.)
         if(wd.eq.0) wd=360
      else
         wd=0.
      endif

      return
      end


C  ----------------------------------------------------------
      Subroutine rotate(ws,wd,uu,vv,dlon,conefac,rlat)

C --- CALMM5   Version: 2.7   Level: 999999

C     Rotate wind to ture-north wind and recalculate U/V
C     (For LCC, POL, and MER)
C     Zhong Wu
      include 'calmm5.par'

      if(ws.gt.1.e-9) then
c ---    southern hemisphere
         if (rlat.lt.0.) then
            wd=wd-conefac*dlon
c ---    northern hemisphere
         else
            wd=wd+conefac*dlon
         endif
c ---    wind direction between 0 and 360 degrees
         if (wd.lt.  0.) wd=wd+360.
         if (wd.gt.360.) wd=wd-360.
      endif

c     Re-calculate U/V based on true-north wind
      wdrad = deg2rad*wd

      uu = -ws * sin(wdrad)
      vv = -ws * cos(wdrad)

      return
      end

C --------------------------------------------------------------------
      Subroutine Outcomment(io,cname)

C --- CALMM5   Version: 2.7   Level: 040112                   OUTCOMMENT
C --- Purpose:
C     Output header comment lines in 3D.DAT and 2D.DAT
C     Zhong-Xiang Wu, 1/12/2004
C
C --- Input: 
C     1. io: Output file number
C     2. cname: data set name
C     3. Others through calmm5.cm1
C
C --- Output: Header comment lines
C
C --- Called by Main
C --- Call Subs: None
C
C ----------------------------------------------------------------------

      include 'calmm5.par'
      include 'calmm5.cm1'
      character*16 cname

      write(io,101)cname,dataver,datamod
 101  format(2(a16),a64)
      write(io,'(i4)')ncomm
      do i=1,ncomm
         write(io,'(a132)')commenthd
      enddo

      return
      end

C ----------------------------------------------------------------
      Subroutine getname(fl,fbas,ifile)

C --- CALMM5   Version: 2.7   Level: 041109                        GETNAME
C
C Purpose:
C     Figure out filename from a base name
C Zhong Wu
C 11/9/2004

      character*80 fl,fbas

C     Local vars
      character*1 ckey
      data ckey/'?'/

      nt1=0
      nt=index(fbas,' ')-1
      nt1=index(fbas,ckey)

      nt2=0
      do i=nt,1,-1
         if(fbas(i:i).eq.ckey) then
            nt2=i
            goto 1000
         endif
      enddo

 1000 continue
      
      if(nt1.le.0 .or. nt2.le.0 .or.nt2.lt.nt1) then
         print *,' Error: No ckey found in base name:',fbas
         stop
      endif

      nn=nt2-nt1+1
      fl=fbas
      if(nn.eq.3) then
         write(fl(nt1:nt2),101)ifile
 101     format(i3.3)
      elseif(nn.eq.2) then
         write(fl(nt1:nt2),102)ifile
 102     format(i2.2)
      elseif(nn.eq.1) then
         write(fl(nt1:nt2),103)ifile
 103     format(i1.1)
      else
         Print *,'Error: ifile too big:',ifile
         stop
      endif

      return
      end

C -------------------------------------------------------------
      Subroutine setup

C --- CALMM5   Version: 2.7   Level: 050413                        SETUP
C Purpose:
C     Gerenal setup for common part of all formats
C Zhong-Xiang Wu
C 11/09/2004

c --- Modifications by Zhong-Xiang Wu
C     5/13/2005
c     1. Fix a bug for calculation of total hours across lead year

      include 'calmm5.par'
      include 'calmm5.cm1'
      include 'calmm5.cm2'

      character*80 buff
      character*6 formatname(6)
      data formatname/'MM5','MM4','GrADS','VIS5D','NetCDF','GRIB'/

c --- Process information from .inp control file -----
c --- read title,input,output,log file names
      read(ict,101) title 
      ntt=index(title,' ')-1

C --- NF meaning for input MM5 files: 
C     1, One (Default);
C     >1, more with input file name in control file; 
C     <=0, more with out-figure file names 
      nf=1    

      read(ict,*,err=113)nf
      if(nf.gt.mxfile) then
        print *,'nf > mxfile: ',nf,mxfile
        stop
      endif 

      if(nf.le.1) then
         goto 113               ! only one MM5 input file 
      else
         do k=1,nf
            read(ict,'(a)')buff    ! read in MM5 input names
            nt=index(buff,' ')-1
            fname(k)=buff(1:nt)
         enddo
      endif
      infile=fname(1)
      goto 114

C     calmm5.inp file without option of multi-input MM5 files  
 113  read(ict,101) infile
 114  continue
      read(ict,101) outfile
      read(ict,101) logfile 
 101  format (a)

      call getflname(infile,nti)    
      call getflname(outfile,nto)
      call getflname(logfile,ntl)

      if(nf.le.0) then
         fbas=infile(1:nti)
         ntbas=nti
      endif

c --- Open log files
      open (ilg,file=logfile,status='unknown')

c --- echo the the control input to log file
      write(ilg,1018)codever,codelevel
 1018 format(1x,'CALMM5 Version: ',a16,3x,'Level: ',a16//)
      write(ilg,1019)name3d,dataver,datalevel
 1019 format(1x,'Data Set Name: ',a16,/,
     &       1x,'Data Set Version: ',a16,3x,'Level: ',a16//)

C     Echo input options to log file
      write(ilg,1020)title(1:ntt)
      write(ilg,*)' Number of MM5 input files:',nf
      if(nf.eq.1) write(ilg,1021)infile(1:nti)
      write(ilg,1022)outfile(1:nto)
      write(ilg,1023)logfile(1:ntl)
      write(ilg,*)
 1020 format(a)
 1021 format('Input file:   ',a)
 1022 format('Output file:  ',a)
 1023 format('Log file:     ',a)

      if(nf.gt.1) then
         do k=1,nf
            nt=index(fname(k),' ')-1
            write(ilg,1120)k,fname(k)(1:nt)
 1120       format('Multi-MM5 Input File #',i3.3,':  ',a)
         enddo
      endif

      if(nf.le.0) then
         write(ilg,1121)fbas(1:ntbas)
 1121    format('MM5 Input File Base Name:',a)
      endif

c --- Selected domain range for CALMM5
c     Options for selecting a region: 1, use lat/lon; 2, use J/I
      read(ict,*)iselect
      write(ilg,1024)iselect
 1024 format('Select region based on (1, lat/lon; 2, J/I): ',i3) 

      if(iselect.eq.1) then  
c        latitude range - in decimal, < 0 for SH
         read(ict,*)rlatmin
         read(ict,*)rlatmax        
         write(ilg,377)rlatmin,rlatmax
 377     format('latitude  range:',2(1x,f10.4))
         if (rlatmin.ge.rlatmax) then
            write(ilg,*)' Error: latitude min > latitude max'
            write(ilg,*)' stop'
            stop
         endif

c        longitude range - in decimal, < 0 for WH
         read(ict,*)rlonmin
         read(ict,*)rlonmax
         write(ilg,378)rlonmin,rlonmax
 378     format('longitude range:',2(1x,f10.4))
         write(ilg,*)
         if (rlonmin.ge.rlonmax) then
            write(ilg,*)' Error: longitude min > longitude max'
            write(ilg,*)' stop'
            stop
         endif
      elseif(iselect.eq.2) then
         read(ict,*)ny1
         read(ict,*)ny2
         read(ict,*)nx1
         read(ict,*)nx2
       
         if(ny1.gt.ny2) then
            write(ilg,*)'Error: NY1 > NY2 ',ny1,ny2
            stop
         endif
         if(nx1.gt.nx2) then
            write(ilg,*)'Error: NX1 > NX2 ',nx1,nx2
            stop
         endif

         write(ilg,379)nx1,nx2,ny1,ny2
 379     format('Selected I/J range from Input: ',4i5)
      else
         write(ilg,*)'Error in options of selecting a region'
         write(ilg,*)'Only valid options: 1 and 2'
         write(ilg,*)'Your option: ',iselect
      endif

c --- starting and ending dates of data extraction (form: YYYYMMDDHH)
      read (ict,*) ibeg
      if(ibeg.lt.1.0E9) then
         print *,'Input Year in YY format, Convert to YYYY format'
         call ck2y(ibeg)
      endif

      write(ilg,*)'beginning date:',ibeg

      read (ict,*) iend
      if(iend.lt.1.0E9) then
         print *,'Input Year in YY format, Convert to YYYY format'
         call ck2y(iend)
      endif

      write(ilg,*)'ending    date:',iend
      write(ilg,*)

      if (ibeg.gt.iend) then
         write(ilg,*)'Error:  beginning date is after ending date'
         write(ilg,*)' stop'
         stop
      endif

c --- convert ibeg and iend to year, month, day and hour
c     (note: ibeg/iend in in YYMMDDHH form)
      call getdate(ibeg,iyearbeg,imonthbeg,idaybeg,ihourbeg)
      call julday(ilg,iyearbeg,imonthbeg,idaybeg,julbeg)

      call getdate(iend,iyearend,imonthend,idayend,ihourend)
      call julday(ilg,iyearend,imonthend,idayend,julend)

c --- number of hours of extract in the output (asked by the user)
      call deltt(iyearbeg,julbeg,ihourbeg,
     &           iyearend,julend,ihourend,nhours)
c --- Add one hour to nhours since deltt is one hour.
      nhours=nhours+1

c --- output format (1,2,3,4,5)
c     1-MM5, 2-MM4, 3-GrADS, 4-VIS5D, 5-NetCDF, 6-GRIB

      read (ict,*) iformat
      read (ict,*)

      if(iformat.eq.4 .or. iformat.eq.5) then
         Print *,'This is a SIMPLE version of CALMM5'
         print *,'No VIS5D and NetCDF formats allowed'
         stop
      endif

c --- echo control .inp options to log file
      write(ilg,355)iformat,formatname(iformat)
 355  format('Output format:',i3,' -- ',a6)
      write(ilg,*)

c --- Only supported format: ascii
      if (iformat.gt.6) then
         write(ilg,*)'Wrong output format - stop '
         write(ilg,*)'Available Options 1-6'
         write(ilg,*)'Required Option: ',iformat
         stop
      endif

      return
      end

C -----------------------------------------------------------------
      Subroutine getnext(idatec,ihrstep,idaten,ilg)

C --- CALMM5   Version 2.6      Level: 060330                  GETNEXT
C
C Zhong-Xiang Wu
C 4/13/2005
C Purpose:
C     Get next ETA file hour
      
      idatet=idatec
      call getdate(idatet,iyrt,imont,idayt,ihourt)
      call julday(ilg,iyrt,imont,idayt,ijult)
      call incr(ilg,iyrt,ijult,ihourt,ihrstep)
      call grday(ilg,iyrt,ijult,imont,idayt)
      call timestamp(iyrt,imont,idayt,ihourt,idaten)

      return
      end
