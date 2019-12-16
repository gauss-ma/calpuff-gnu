#!/bin/bash


echo "Compilando 'makegeo.exe' .."
gfortran -static -O0 -o makegeo.exe makegeo.for 2 > compile-makegeo-gfortran.log

    if test -f "../EXE/terrel.exe"; then
      echo -e "El ejecutable \e[32m terrel.exe \e[0m fue creado."
    else 
      echo -e "\e[31m ERROR:  Compilación no exitosa ó directorio '../EXE'  NO existe!\e[0m"; exit;
    fi

# Para compilar MAKEGEO tuve que hacer las siguientes correcciones:
#  - Renombrar todos los scripts a lowercase (estaban en upper) for i in $( ls | grep [A-Z] ); do mv -i $i `echo $i | tr 'A-Z' 'a-z'`; done
#  - Comentar llamada subrutina etime(rcpu)     (vinculado a compilador Lahey)
#  - Habilitar GETARGS (linea ~ 2722  de calutils.for).

# (!)  TERMINE COPIANDO DIRECTAMENTE EL CALUTILS DE LA COMPILACION DEL CALPUFF  (!)
