      program dd_dem

c --- Read USGS DEM file as downloaded and construct logical records
c --- of 1024 bytes (insert delimiters)

      character*40 filein,fileout
      character*1024 aline

      write(*,*)'       Enter USGS filename:  '
      read(*,*) filein
      write(*,*)' Enter filename for output:  '
      read(*,*) fileout

      !open(7,file=filein,form='binary',status='old')
      open(7,file=filein,form='unformatted',status='old',
     &   access='stream')
      open(8,file=fileout)

c --- Loop over physical records
10    read(7,err=999) aline
      write(8,'(a1024)') aline
      goto 10

999   stop
      end
