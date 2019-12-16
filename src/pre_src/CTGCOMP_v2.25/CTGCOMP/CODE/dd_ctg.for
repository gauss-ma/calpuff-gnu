      program dd_ctg

c --- Read USGS CTG file as downloaded and construct logical records
c --- of 80 bytes (insert delimiters)

      character*40 filein,fileout
      character*80 aline

      write(*,*)'       Enter USGS filename:  '
      read(*,*) filein
      write(*,*)' Enter filename for output:  '
      read(*,*) fileout

      open(7,file=filein,form='binary',access='transparent',
     &                   status='old')
      open(8,file=fileout)

c --- Loop over records
10    read(7,err=999) aline
      write(8,'(a80)') aline
      goto 10

999   stop
      end
