#!/bin/bash
#---------------------------#
# Make UP.DAT file & ejecutar read62.exe
#---------------------------#
up_output_file="UP.DAT"

start_date='2019-02-02_00:00:00'   #%Y-%M-%D_%H:%M:%D
end_date='2019-02-05_00:00:00'     #%Y-%M-%D_%H:%M:%D 

	#parse dates:
	read ibyr ibmo ibdy ibhr ibmin ibsec <<< ${start_date//[-:\/_ ]/ }
	read ieyr iemo iedy iehr iemin iesec <<< ${end_date//[-:\/_ ]/ }
                ibjdy=`date -d"$ibyr-$ibmo-$ibdy" +%j`
                iejdy=`date -d"$ieyr-$iemo-$iedy" +%j`

indat="datainput/2019.FSL"	#Nombre de inp file 1
#subdat="datainput/2019sub.FSL"  #Nombre de inp file 2
updat="UP.DAT"			#Nombre de out file
runlst="read62.lst"		#Nombre de log file
lcfiles="T"	#Nombre de salidas a lowercase?

jdat=2          #jdat the type of NCDC input sounding data file;
		#'1' is the TD-6201 format 
		#'2' is the NCDC FSL format.
isub=0          #isub the format of substitute UP.DAT input sounding data file;
                #       '0' indicates that no substitute will be used,
                #       '1' states that the delimiter between sounding levels is a forward slash (and WS and WD have integer representations), 
                #       '2' states that the delimiter between sounding levels is a comma (and WS and WD have floating point representations). 
ifmt=2          #ifmt the format of the main UP.DAT input sounding data file; 
                #       '1' states that the delimiter between sounding levels is a forward slash (and WS and WD have integer representations), 
                #       '2' states that the delimiter between sounding levels is a comma (and WS and WD have floating point representations).
pstop=750       #pstop the top pressure level (in mb units) for which data are extracted. The pressure level must correspond to a height that equals or exceeds the top of the CALMET modeling domain, or else CALMET will stop with an error message.
lht="T"         #If height is missing from a level, that level will be rejected. [T/F]
ltemp="F"       #If temperature is missing from a level, that level will be rejected. [T/F]
lwd="F"         #If wind direction is missing from a level, that level will be rejected. [T/F]
lws="F"         #If wind speed is missing from a level, that level will be rejected. [T/F]
lxtop="F"       #lxtop choice of whether to extrapolate to extend missing profile data to PSTOP pressure level.

pvtop=850       #pvtop if 'lxtop' is TRUE, then pvtop is the pressure level corresponding to where valid data must exist.
lxsfc="F"       #lxsfc choice of whether to extrapolate to extend missing profile data to the surface.
zvsfc=200       #zvsfc if 'lxsfc' is TRUE, then zvsfc is the height (in meters) corresponding to where valid data must exist.

##! SUBDAT =${subdat} !
#
#cat << EOF > read62.inp  
#READ62.INP      2.1             Hour Start and End Times with Seconds
#
#0 -- Input and Output Files
#
#! INDAT  =${indat} !
#! UPDAT  =${updat} !
#! RUNLST =${runlst} !
#
#! LCFILES =${lcfiles} !
#
#! END !
#
#--------------------------------------------------------------------------------
#1 -- Run control parameters
#
#Starting date/time
#! IBYR  =${ibyr} !
#! IBMO  =${ibmo} !
#! IBDY  =${ibdy} !
#! IBHR  =${ibhr} !
# IBSEC =${ibsec} 
#
#Ending date/time
#! IEYR  =${ieyr} !
#! IEMO  =${iemo} !
#! IEDY  =${iedy} !
#! IEHR  =${iehr} !
# IESEC =${iesec} 
#
#File Options
#! JDAT =${jdat} !
#! ISUB =${isub} !
#! IFMT =${ifmt} !
#
#Processing Options
#! PSTOP =${pstop} !
#! LHT   =${lht} !
#! LTEMP =${ltemp} !
#! LWD   =${lwd} !
#! LWS   =${lws} !
#! LXTOP =${lxtop} !
#! PVTOP =${pvtop} !
#! LXSFC =${lxsfc} !
#! ZVSFC =${zvsfc} !
#
#! END !
#EOF
#
#ln -sf ../exe/read62.exe .
#chmod 755 read62.exe
#./read62.exe read62.inp
#
#
#
#sed -i '/->->->/d' up.dat
#sed -i 's/\*/9/g' up.dat
#

printf '%s\n1\nProduced by Ramiro A. Espada from GAUSS-ma\nNONE\n' "UP.DAT">${up_output_file}
printf '%4.0f %4.0f %3.0f %4.0f %4.0f %3.0f %.0f %.0f\n' ${ibyr}  ${ibjdy}  ${ibhr}  ${ieyr}  ${iejdy}  ${iehr} ${stat_time_offset} ${#input_file[@]} >> ${sfc_output_file}
printf '%s1  %s1  %s1  %s1 ' $lht $ltemp $lwd $lws >> ${up_output_file}
printf '%.0f\n' ${stat_id} >> ${sfc_output_file}



echo "UP.DAT          2.0             Header structure with coordinate parameters"
echo "1"
echo "Produced by READ62 Version: 5.54  Level: 070627"
echo "NONE"
  2019   33    0 2019   36    0 750.    2    2
  $lht $ltemp $lwd $lws
   6201     87576   2019 2 121     63                               25
   1013.0,  20.,289.2,160,  3.6,   999999, 229.,286.8,150,  5.1,   9920.0, 296.,286.2,999,99999,   9780.0, 415.,287.4,999,99999,
   9557.0, 609.,99999,105,  4.6,   9250.0, 884.,284.8, 75,  5.1,   9217.0, 914.,99999, 70,  5.1,   9080.0,1038.,283.6,999,99999,
   8780.0,1316.,282.6,999,99999,   8500.0,1587.,282.6,140,  7.2,   7954.0,2133.,99999,115,  7.2,   7665.0,2438.,99999,130,  7.7,
   7560.0,2551.,281.0,999,99999,   7000.0,3184.,278.6,175,  8.2,   6348.0,3962.,99999,190, 10.3,   6109.0,4267.,99999,205, 10.8,
   5330.0,5352.,266.3,999,99999,   5000.0,5850.,262.5,215,  8.2,   4840.0,6096.,99999,220,  7.2,   4330.0,6935.,254.1,999,99999,
   4000.0,7520.,250.5,235, 18.0,   3626.0,8229.,99999,230, 18.5,   3430.0,8629.,243.7,999,99999,   3000.0,9570.,235.9,250, 27.3,
   2860.0,9897.,233.1,999,99999
   6201     87576   2019 2 221     63                               25
   999999,  20.,289.2,160,  3.6,   999999, 229.,286.8,150,  5.1,   9920.0, 296.,286.2,999,99999,   9780.0, 415.,287.4,999,99999,
   9557.0, 609.,99999,105,  4.6,   9250.0, 884.,284.8, 75,  5.1,   9217.0, 914.,99999, 70,  5.1,   9080.0,1038.,283.6,999,99999,
   8780.0,1316.,282.6,999,99999,   8500.0,1587.,282.6,140,  7.2,   7954.0,2133.,99999,115,  7.2,   7665.0,2438.,99999,130,  7.7,
   7560.0,2551.,281.0,999,99999,   7000.0,3184.,278.6,175,  8.2,   6348.0,3962.,99999,190, 10.3,   6109.0,4267.,99999,205, 10.8,
   5330.0,5352.,266.3,999,99999,   5000.0,5850.,262.5,215,  8.2,   4840.0,6096.,99999,220,  7.2,   4330.0,6935.,254.1,999,99999,
   4000.0,7520.,250.5,235, 18.0,   3626.0,8229.,99999,230, 18.5,   3430.0,8629.,243.7,999,99999,   3000.0,9570.,235.9,250, 27.3,
   2860.0,9897.,233.1,999,99999

