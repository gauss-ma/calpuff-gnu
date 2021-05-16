#!/bin/bash
#---------------------------------
source namelist.sh
#---------------------------------
#Preaparacion de GEO.DAT
geo_output_file="geo.dat"

datenin="${ibyr}-${ibmo}-${ibdy}" # no se que es

#====================================================
#PREPARACION de ARCHIVOS de ENTRADA
dem_file=dem.tif
if [ ! -f "$dem_file" ]; then
        #gdalwarp -tr ${dx} ${dy} -r bilinear -t_srs epsg:${epsg_utm} -te ${xini} ${yini} ${xfin} ${yfin} -te_srs epsg:${epsg_utm} ${inp_dem} ${dem_file}
        gdalwarp -tr ${dx} ${dy} -r bilinear -t_srs epsg:${epsg_utm} -te ${xini} ${yini} ${xfin} ${yfin} ${inp_dem} ${dem_file}
fi

#if [ ! -f "$lu_file" ]; then
#       #gdalwarp -tr ${dx} ${dy} -r mode -t_srs epsg:${epsg_local} -te ${xini} ${yini} ${xfin} ${yfin} -te_srs epsg:${epsg_local} ${lu_inp_path} ${lu_file}
#       #gdalwarp -t_srs epsg:${epsg_nad83} ${lu_file} ${lu_file}
#
#fi 

#if [ ! -f "$receptores_file" ]; then
        #Construyo archivo con grilla:
        python ${dir}/utils/meshgrid.py ${xini} ${xfin} ${nx} ${yini} ${yfin} ${ny} | cat > gridXY.txt
        #(1) Armo la grilla de receptores.
        cat gridXY.txt | gdallocationinfo -geoloc -valonly ${dem_file} > gridZ.txt
        sed -i "s/$/.0/g" gridZ.txt
        #paste gridXY.txt gridZ.txt gridZ.txt | sed "s/\t/ /" > ${receptores_file}
        #sed -i "s/^/RE DISCCART   /g; s/$/  ${Zflag}/g; s/\t/  /g" ${receptores_file}
        # sed -i "s/$/  ${Zflag}/g" receptores.txt  # sed -i "s/\t/ /g" receptores.txt
#fi

#====================================================
#ESCRIBIR GEO.DAT

#·----------
#HEADER:
printf '%s\n' "GEO.DAT" > ${geo_output_file}
printf '%.0f\n%-30s\n' 1 "Produced by Ramiro A. Espada from GAUSS-ma" >> ${geo_output_file} #numero de lineas comentadas posteriores a esta.
printf '%s\n' "UTM" >>${geo_output_file}
printf '%.0f,%s\n' $utmzn $utmhem >>${geo_output_file}
printf '%5s %12s\n' $datum $datenin >>${geo_output_file}
printf '%.0f, %.0f, %.3f, %.3f, %.3f, %.3f\n' $nx $ny $xorigkm $yorigkm $dgridkm $dgridkm  >> ${geo_output_file}
printf '%-4s\n' $xyunit >> ${geo_output_file}
#·----------
#LAND USE:
LU_i=10
echo "0                 -  LAND USE DATA  - IOPT1:  0=DEFAULT CATEGORIES  1=NEW CATEGORIES">> ${geo_output_file}

for i in $(seq 1 $nx)
do
        for j in $(seq 1 $ny)
        do
                printf '%4.0f' ${LU_i} >>${geo_output_file}

        done;
        printf '\n' >> ${geo_output_file} #breakline
done;

#·----------
#ELEVATION:

echo "1.0 	 	-  TERRAIN HEIGHTS - HTFAC (factor de conversion a metros)">>${geo_output_file}

i=0
while IFS= read -r line
do
i=$(($i+1))
        printf '%5.1f' $line >> ${geo_output_file}
	if (( ${i}%${nx} == 0)); then printf '\n' >> ${geo_output_file};fi
done <"gridZ.txt"

#·----------
#SURFACE DESCRIPTORS:
cat << EOF >> ${geo_output_file}
0 - IOPT2 (z0)	-- (0=default z0-lu table,    1=new z0-lu table,    2=gridded)
0 - IOPT3 (alb) -- (0=default albedo-lu table,1=new albedo-lu table,2=gridded)
0 - IOPT4 (Bo) 	-- (0=default Bowen-lu table, 1=new Bowen-lu table, 2=gridded)
0 - IOPT5 (HCG) -- (0=default HCG-lu table,   1=new HCG-lu table,   2=gridded)
0 - IOPT6 (QF) 	-- (0=default QF-lu table,    1=new QF-lu table,    2=gridded)
0 - IOPT7 (LAI) -- (0=default XLAI-lu table,  1=new XLAI-lu table,  2=gridded)
EOF
		## CALMET LAND USE CATEOGORIES TABLE:
		#	typ	Description			z0	albedo	Bo   	HCG   	QF	LAI
		#	10    Urban or Built-up Land 		1.0 	0.18 	1.5 	.25 	0.0 	0.2
		#	20    Agricultural Land - Unirrigated 	0.25 	0.15 	1.0 	.15 	0.0 	3.0
		#	-20   Agricultural Land - Irrigated 	0.25 	0.15 	0.5 	.15 	0.0	3.0
		#	30    Rangeland 			0.05 	0.25 	1.0 	.15 	0.0	0.5
		#	40    Forest Land 			1.0 	0.10 	1.0 	.15 	0.0	7.0
		#	51    Small Water Body 			0.001 	0.10 	0.0 	1.0 	0.0	0.0
		#	54    Bays and Estuaries 		0.001 	0.10 	0.0 	1.0 	0.0	0.0
		#	55    Large Water Body 			0.001 	0.10 	0.0 	1.0 	0.0	0.0
		#	60    Wetland 				1.0 	0.10 	0.5 	.25 	0.0	2.0
		#	61    Forested Wetland 			1.0 	0.1 	0.5 	0.25 	0.0	2.0
		#	62    Nonforested Wetland 		0.2 	0.1 	0.1 	0.25 	0.0	1.0
		#	70    Barren Land 			0.05 	0.30 	1.0 	.15 	0.0	0.05
		#	80    Tundra 				0.20 	0.30 	0.5 	.15 	0.0	0.0
		#	90    Perennial Snow or Ice 		0.20 	0.70 	0.5 	.15 	0.0	0.0
		#
		#	*  (-) negative values means irrigated
		#	** z0=roughness length;Bo=bowens ratio; HCG:soil heat flux; QF: anthropo heat flux;

#·----------
#FIN.
#====================================================
