#!/bin/bash


echo "Compilando 'terrel.exe' .."
gfortran -O0 terrel.for -o ../EXE/terrel.exe

    if test -f "../EXE/terrel.exe"; then
      echo -e "El ejecutable \e[32m terrel.exe \e[0m fue creado."
    else 
      echo -e "\e[31m ERROR:  Compilación no exitosa ó directorio '../EXE'  NO existe!\e[0m"; exit;
    fi

echo "Compilando 'dd_dem.exe' .."
gfortran -O0 dd_dem.for -o ../EXE/dd_dem.exe
     if test -f "../EXE/dd_dem.exe"; then
       echo -e "El ejecutable \e[32m dd_dem.exe \e[0m fue creado."
     else
         echo -e "\e[31m ERROR:  Compilación no exitosa ó directorio '../EXE'  NO existe!\e[0m"; exit;
     fi
# Para compilar TERREL tuve que hacer las siguientes correcciones:
#  - Renombrar todos los scripts a lowercase (estaban en upper) for i in $( ls | grep [A-Z] ); do mv -i $i `echo $i | tr 'A-Z' 'a-z'`; done
#  - Comentar llamada subrutina etime(rcpu)     (vinculado a compilador Lahey)
#  - Agregar "trim()" en los inputs al argumento "access" en llamadas a funcion "open"
#  - Cambiar argumento "flen" por "recl" en llamada a funcion inquire
#  - Comentar llamada a subrutina getcl(text)   (vinculado a compilador Lahey)

# Para compilar DD_DEM tuve que hacer las siguientes correcciones:
# - En llamada a funcion "open" cambiar form='binary' por 'unformatted' y  access='transparent' por 'stream'.
