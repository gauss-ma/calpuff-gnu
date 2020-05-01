#!/bin/bas
export LC_NUMERIC="en_US.UTF-8"

dir=/home/alumno/Git/calpuff
#----------------------------------------------------------------------
#Archivos de entrada
inp_dem=data/dem/BA_1arcdeg.tif
inp_sfc=($(ls data/sfc/87593*))
inp_up=($(ls data/up/87576*))
#inp_buildings=${dir}/data/BUILDINGS/buildings.csv

#Corridas:
PROYECTO=SAMPLE                                 #Nombre del proyecto:
POLLUTID_LIST=(CO CO NOX NOX PMT PMT)           #CO NOX SOX PMT
AVGTIME_LIST=(1 8 1 ANNUAL 24 ANNUAL)           #PERIOD ANNUAL 1 8 24

#--------------------------------------------------------------------
#FECHAS:
ini_date="2019-02-12_00:00:00"   #YYYY-MM-DD_HH:MM:SS (%Y-%m-%d_%H:%M:%s)
end_date="2019-02-21_00:00:00"   #YYYY-MM-DD_HH:MM:SS (%Y-%m-%d_%H:%M:%s)
tz=3                    #time-zone
stat_time_offset=-3.0		   #La hora del input file esta en UTC! (local_time argentina = UTC - 3)
dt=$((60*60))	#time-step [secs]
#----------------------------------------------------------------------
#DOMINIO/GRILLA
epsg_latlon=4326        #latlon wgs84
epsg_utm=32720          #utm (zone:20S)
epsg_local=22196        #Sistema de Coords (Campo Inchauspe=epsg:2219F)
datum="WGS-84"
pmap='UTM'              #projection ("UTM", "TTM", "LCC", "PS", "EM", "LAZA")
xyunit="KM"		#"KM" 

clat=-34.5252118
clon=-58.6207262

nz=11		# numero de niveles verticales
zlevs="0, 20, 40, 80, 100, 150, 200, 300, 400, 800, 1400, 2000, 3000"

#----------------------------------------------------------------------
#EMISORES:
#Función donde se definen los parametros de los emisores para cada contaminante
emis_name=(CC1 CC2)
emis_type=(POINT POINT)
#Posicion y parametros:
emis_lon=(-57.9521 -57.9518)
emis_lat=(-34.8592 -34.8589)
emis_x=(5687343.18 5687371.30)
emis_y=(6141291.53 6141324.26)
emis_z=(2.0 2.0)       #altura base fuente
emis_h=(55.0 55.0)     #altura emision desde la base (m)
emis_d=(5.95 5.95)     #diametro m

emis_q=(617.1 617.1)   #caudal g/s
emis_t=(369.0 369.0)   #temperatura kelvin
emis_u=(30.0 30.0)     #velocidad m/s

#----------------------------------------------------------------------
#varables derivadas de las anteriores:

#Estaciones/observaciones utilizadas
read sfc_lat sfc_lon sfc_z sfc_id sfc_name <<<$(echo "-34.58 -57.54 23 875930 SADL")

read up_lat up_lon up_z up_id up_name <<<$(echo "-34.49 -58.32 20 87576 SAEZ");

        read sfc_x sfc_y nose <<< $(echo $sfc_lon $sfc_lat | gdaltransform -t_srs epsg:$epsg_utm -s_srs epsg:$epsg_latlon)
        read up_x up_y nose <<< $(echo $up_lon $up_lat | gdaltransform -t_srs epsg:$epsg_utm -s_srs epsg:$epsg_latlon)

nstats=1	#numero de estaciones usadas 




#	space
read xc yc zc <<<$( gdaltransform -s_srs epsg:${epsg_latlon} -t_srs epsg:${epsg_utm} <<< $(echo ${clon} ${clat}) )
        #grilla reglamentaria (opds):
        dx=50.0;    #Resolucion-X
        dy=50.0;    #Resolución-Y
        xini=$(bc<<<"$xc-3162")
        yini=$(bc<<<"$yc-3162")
        xfin=$(bc<<<"$xc+3162")
        yfin=$(bc<<<"$yc+3162")
        nx=$(bc<<< "($xfin-$xini)/$dx")
        ny=$(bc<<< "($yfin-$yini)/$dy")


	xorigkm=$( bc<<<"scale=5; $xini/1000")
	yorigkm=$( bc<<<"scale=5; $yini/1000")
	dgridkm=$( bc<<<"scale=5; $dx/1000")

#	date:
	#parse dates:
	read ibyr ibmo ibdy ibhr ibmin ibsec <<< ${ini_date//[-:\/_ ]/ }
	read ieyr iemo iedy iehr iemin iesec <<< ${end_date//[-:\/_ ]/ } 
	ibjdy=`date -d"$ibyr-$ibmo-$ibdy" +%j`
	iejdy=`date -d"$ieyr-$iemo-$iedy" +%j`
	total_horas=$((($(date -d"$ieyr-$iemo-$iedy" +%s)-$(date -d"$ibyr-$ibmo-$ibdy" +%s))/(60*60) ))


	#Calculate utmzone and hemisphere
	utmzn=$(bc <<< "(((${clon}+180)/6)%60)+ 1")
	if (( $(bc <<<"$clat < 0") )); then utmhem="S";else utmhem="N"; fi


	#Paso ibdy/iedy a dias julianos
	date_ini=`date -u -d"$ibyr-$ibmo-$ibdy ${ibhr} hours" +"%Y %m  %j  %H"`
	date_fin=`date -u -d"$ieyr-$iemo-$iedy ${iehr} hours" +"%Y %m  %j  %H"`
