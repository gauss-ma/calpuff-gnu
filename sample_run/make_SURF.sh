#!/bin/bash
source namelist.sh
#---------------------------------o#
#Make SURF.DAT & PRECIP.DAT
#----------------------------------#
echo "Archivos: ${inp_sfc[@]}"
sfc_out_file="surf.dat"
ppt_out_file="precip.dat"

#(2) Write out the SURF.DAT file
#=========================================================================
#H E A D E R:
	#datos de la estacion
		line=0$(head -n1 ${inp_sfc[0]});line=$(echo $line | sed -e 's/+/ /g')
		read id lat lon elev <<<$(echo ${line:5:6} ${line:29:6} ${line:35:7} ${line:47:5})
			stat_id=0$id #010280
			lat=$(bc <<< "x=$lat;scale=2;x/1000.00")
        		lon=$(bc <<< "x=$lon;scale=2;x/1000.00")
        		elev=$(bc <<< "x=$elev;scale=2;x/1")

#SURF.DAT (HEADER)
printf '%s\n1\nProduced by Ramiro A. Espada from GAUSS-ma\nNONE\n' "SURF.DAT">${sfc_out_file}
printf '%4.0f %4.0f %3.0f %4.0f %4.0f %3.0f %.0f %.0f\n' ${ibyr}  ${ibjdy}  ${ibhr}  ${ieyr}  ${iejdy}  ${iehr} ${stat_time_offset} $nstats >> ${sfc_out_file}
printf '%.0f\n' ${stat_id} >> ${sfc_out_file}

#PRECIP.DAT (HEADER)
printf '%s\n1\nProduced by Ramiro A. Espada from GAUSS-ma\nNONE\n' "PRECIP.DAT">${ppt_out_file}
printf '%6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f\n' ${ibyr}  ${ibjdy}  ${ibhr}  ${ieyr}  ${iejdy}  ${iehr}  ${stat_time_offset} $nstats >> ${ppt_out_file}
printf '%6.0f\n' ${stat_id} >> ${ppt_out_file}

	#read stat_x stat_y nose <<<$(echo $lon $lat | gdaltransform -t_srs epsg:$epsg_local -s_srs epsg:$epsg_latlon) #station_UTM_easting_km & station_UTM_northing_km
	#stat_x=$(bc <<< "x=$stat_x;x/1000;scale=1;x/1")
	#stat_y=$(bc <<< "x=$stat_y;x/1000;scale=1;x/1")
	#read stat_x stat_y <<<$(printf '%.3f %.3f' $stat_x $stat_y)
	#stat_z=$(bc <<< "scale=1;elev=$elev;elev+20.0") #Altura del anemometro (fijo 20m) + elevacion de la estacion.

#cat << EOF > ${output_file}
#1.00 1.0000 $stat_x $stat_y $stat_time_offset $stat_z
#NONE
#UTC $stat_time_offset 00
#${ibyr}  ${ibjdy}  ${ibhr}  0  ${ieyr}  ${iejdy}  ${iehr}  0  ${#inp_sfc[@]}
#${stat_id}
#1.0000
#EOF
#=========================================================================
#B O D Y
while IFS= read -r line
do
	#Extraigo datos de cada linea:
	line=0${line}
	s=${line:0:105}; s=$(echo $s | sed -e 's/+/ /g')
	len=$(bc <<< "x=${line:1:4};x/1");
        a=${line:105:$len}

	#Station / meassure data:
	read id date lat lon elev <<<$(echo ${s:5:6} ${s:16:12} ${s:29:6} ${s:35:7} ${s:47:5})
	#lat=$(bc <<< "x=$lat;scale=2;x/1000.00")
        #lon=$(bc <<< "x=$lon;scale=2;x/1000.00")
        #elev=$(bc <<< "x=$elev;scale=2;x/1.00")

	read yr mo dy hr min <<< $(echo ${date:0:4} ${date:4:2} ${date:6:2} ${date:8:2} ${date:10:2})
	
	this_date=$(date --date="$yr-$mo-$dy $hr:$min:00" +"%Y-%m-%d %H:%M:%S")

	if [[ $this_date < $(date --date="$ibyr-$ibmo-$ibdy $ibhr:$ibmin:$ibsec" +"%Y-%m-%d %H:%M:%S") ]]; then 
		echo -e "$this_date"                                                           	
		continue;
	elif [[ $this_date > $(date --date="$ieyr-$iemo-$iedy $iehr:$iemin:$iesec" +"%Y-%m-%d %H:%M:%S") ]]; then 
		echo -e "\e[35m $(date --date="$ieyr-$iemo-$iedy $iehr:$iemin:$iesec" +"%Y-%m-%d %H:%M:%S") \e[0m"
		echo -e "\e[32m FIN. \e[0m"
		break;

	else
		echo -e "\e[32m $this_date \e[0m"
	fi;
	
	hour1=`date -u -d"$yr-$mo-$dy ${hr} hours" +"%Y  %j  %H"`
	read iyr ijul ihr <<< ${hour1//[-:\/_ ]/ }
	#hour2=`date -u -d"$yr-$mo-$dy $((10#${hr}+1)) hours" +"%Y %j  %H  0"`

	#Mandatory meteorological data:
	read wd ws ceil vis temp dwp pres <<< $(echo ${s:61:3} ${s:66:4} ${s:71:5} ${s:79:6} ${s:88:5} ${s:94:5} ${s:100:5})
	
	if [[ $temp == 99999 || $dwp == 99999 ]]; then rh=0; else rh=1; fi; #rh = 1 : calcular rh; rh=0 no calcular rh.
	if [[ $pres == 99999 ]]; then pres=$(echo $a | sed -e "s/.*MA1\(.\{5\}\).*/\1/");fi;
	if (( ${#pres} != 5 )); then pres=10132.5; fi; #presion por default = 1013.25 hPa
	
	wd=$(bc <<< "x=$wd;scale=0;x/1")
	ws=$(bc <<< "x=$ws;scale=5;o=x/10.;scale=2;o/1");
	ceil=$(bc <<< "x=$ceil;o=x*3.28084/100.0;scale=1;o/1");
	vis=$(bc <<< "x=$vis;scale=2; x/1");
	temp=$(bc <<< "x=$temp;scale=5;o=x/10.0+273.2;scale=2;o/1");
	dwp=$(bc <<< "x=$dwp;scale=5;o=x/10.0;scale=2;o/1");
	pres=$(bc <<< "x=$pres;scale=5;o=x/10.0;scale=3;o/1 ");
 
  	#Calculo de humedad relativa (usando la aprox de August-Roche-Magnus)
  	if [[ $rh == 1 ]]; then rh=$(echo "scale=5;d=${dwp};t=${temp};o=100*(e((17.625*d)/(243.04+d))/e((17.625*(t-273.2))/(243.04+(t-273.2))));scale=2;o/1" | bc -l);else rh=999.9;fi
  
  	#Calculo ppt rate y pp code
  	ppt=$(echo $a | sed -e "s/.*AA1\(.\{6\}\).*/\1/");if ((  ${#ppt} != 6 )); then ppt=$(printf '0%.0s' $(seq 1 6)); fi
  	read pp_period pp_mm <<< $(echo $ppt | sed -e 's/\(.\{2\}\)\(.\{4\}\)/\1 \2/')

  	if [[ $pp_mm == 9999 || $pp_period == 99 ]]; then pp_r=999.9; pp_code=999;
	elif [[ $pp_mm == 0000 || $pp_period == 00 ]]; then pp_r=0.0; pp_code=0;
  	else
  	               pp_r=$(bc <<< "scale=5;pp=$pp_mm;t=$pp_period;pp/t");
  	 #Calculo pp code  
  	        if [[ $(echo "t=$temp;r=$pp_r; if (t > 0 & r < 2.5) 1 else 0"| bc) -eq 1 ]]; then pp_code=1; fi;
  	        if [[ $(echo "t=$temp;r=$pp_r; if (t > 0 & r > 2.5 & r < 7.6) 1 else 0"| bc) -eq 1 ]]; then pp_code=2; fi;
  	        if [[ $(echo "t=$temp;r=$pp_r; if (t > 0 & r > 7.6 ) 1 else 0" | bc)  -eq 1 ]]; then pp_code=3; fi;
  	        if [[ $(echo "t=$temp;r=$pp_r; if (t < 0 & r < 2.5 ) 1 else 0" | bc)  -eq 1 ]]; then pp_code=19;fi;
  	        if [[ $(echo "t=$temp;r=$pp_r; if (t < 0 & r > 2.5 & r < 7.6) 1 else 0" | bc) -eq 1 ]]; then pp_code=20;fi;
  	        if [[ $(echo "t=$temp;r=$pp_r; if (t < 0 & r > 7.6 ) 1 else 0" | bc) -eq 1 ]]; then pp_code=21;fi;
  	fi;

	 #Sky cover
	 sky=$(echo $a | sed -e "s/.*GF1\(.\{2\}\).*/\1/g");if (( ${#sky} != 2 )); then sky=99; fi
       
#SURF.DAT
printf '%4.0f %4.0f %3.0f\n' $iyr $ijul $ihr >> ${sfc_out_file}
printf '%6.2f %6.1f %6.0f %6.0f %6.2f %6.0f %6.1f %6.0f \n ' $ws $wd $ceil $sky $temp $rh $pres $pp_code >> ${sfc_out_file}

#PRECIP.DAT
printf '%6.0f %6.0f %6.0f %6.2f\n' $iyr $ijul $ihr $pp_r >> ${ppt_out_file}

#cat << EOF >> ${output_file}
#${hour1}  ${hour2}
#EOF
#printf '%6.2f %6.0f %6.0f %6.0f %6.2f %6.0f %6.2f %6.0f \n ' $ws $wd $ceil $sky $temp $rh $pres $pp_code >> ${output_file}
#
done < <(cat ${inp_sfc[@]})




echo -e 
echo -e "Asegurarse que  la serie comiene con valores \e[32mNO NULOS \e[0m!"

