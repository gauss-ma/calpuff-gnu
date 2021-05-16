#!/bin/bash
source namelist.sh
#---------------------------#
# Make UP.DAT file & ejecutar read62.exe
#---------------------------#

#read62 toma las fechas  en horario universal (GMT)
read gmtbyr gmtbmo gmtbdy gmtbhr <<< $(date -u -d"$ibyr-$ibmo-$ibdy $ibhr +${tz}" '+%Y %m %d %H')
read gmteyr gmtemo gmtedy gmtehr <<< $(date -u -d"$ieyr-$iemo-$iedy $iehr +${tz}" '+%Y %m %d %H')

cat ${inp_up[@]} > up.fsl
#ln -s ${inp_up} up_file.fsl
inp_up=up.fsl


pstop=800       #pstop the top pressure level (in mb units) for which data are extracted. 
		#The pressure level must correspond to a height that equals or exceeds the top of the CALMET modeling domain,
		# or else CALMET will stop with an error message.

pvtop=1000      #pvtop if 'lxtop' is TRUE, then pvtop is the pressure level corresponding to where valid data must exist.
zvsfc=50        #zvsfc if 'lxsfc' is TRUE, then zvsfc is the height (in meters) corresponding to where valid data must exist.


cat << EOF > read62.inp  
READ62.INP      2.1             Hour Start and End Times with Seconds

0 -- Input and Output Files

! INDAT  =${inp_up}  !  archivo de entrada
* SUBDAT =	     *  archivo de entrada substituto
! UPDAT  =up.dat     !  archivo de salida
! RUNLST =read62.lst !  archivo de mensajes y errores.

! LCFILES = T        ! 	nombre de salidas lowercase? (T/F)

! END !
--------------------------------------------------------------------------------
1 -- Run control parameters

Starting date/time
! IBYR  =${gmtbyr} !
! IBMO  =${gmtbmo} !
! IBDY  =${gmtbdy} !
! IBHR  =${gmtbhr} !
* IBSEC =${gmtbsec} *
                  
Ending date/time  
! IEYR  =${gmteyr} !
! IEMO  =${gmtemo} !
! IEDY  =${gmtedy} !
! IEHR  =${gmtehr} !
* IESEC =${gmtesec}*             
                               
File Options		       
! JDAT = 2 !    #type of NCDC input sounding data file;
                #	1: is the TD-6201 format 
                #	2: is the NCDC FSL format.
! ISUB = 0 !    #isub the format of substitute UP.DAT input sounding data file;
                #    0: no substitute data file will be used,
                #    1: states that the delimiter between sounding levels is a forward slash (and WS and WD have integer representations), 
                #    2: states that the delimiter between sounding levels is a comma (and WS and WD have floating point representations). 
! IFMT = 2 !    #ifmt the format of the main UP.DAT input sounding data file;                                                                   
                #    1: states that the delimiter between sounding levels is a forward slash (and WS and WD have integer representations),       
                #    2: states that the delimiter between sounding levels is a comma (and WS and WD have floating point representations).

Processing Options              
! PSTOP =${pstop} !  #pstop the top pressure level (in mb units) for which data are extracted.                                         
                     #The pressure level must correspond to a height that equals or exceeds the top of the CALMET modeling domain,
                     # or else CALMET will stop with an error message.
 
! LHT   = T     !    #If height is missing from a level, that level will be rejected. [T/F]                                                 
! LTEMP = F     !    #If temperature is missing from a level, that level will be rejected. [T/F]                                            
! LWD   = F     !    #If wind direction is missing from a level, that level will be rejected. [T/F]                                         
! LWS   = F     !    #If wind speed is missing from a level, that level will be rejected. [T/F]                                             

! LXTOP = T     !    #extrapolate to extend missing profile data to PSTOP pressure level? [T/F].                  
! PVTOP =${pvtop}!   # if 'lxtop' is TRUE, then pvtop is the pressure level corresponding to where valid data must exist.              
! LXSFC = T      !   #extrapolate to extend missing profile data to the surface?                                 
! ZVSFC =${zvsfc}!   # if 'lxsfc' is TRUE, then zvsfc is the height (in meters) corresponding to where valid data must exist.          
                                
! END !
EOF

if [[ ! -f read62.exe ]]
then
	ln -sf ${dir}/exe/read62.exe .
	chmod 755 read62.exe
fi
./read62.exe read62.inp

sed -i '/->->->/d' up.dat
sed -i 's/\*/9/g' up.dat

