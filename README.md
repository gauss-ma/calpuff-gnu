# CALPUFF

> Compilación gnu del sistema de modelado CALPUFF y corrida de prueba.


Los ejecutables están en el directorio ``exe/ ``

En caso de querer realizar una nueva compilación:

```
cd src

chmod 755 build.sh

./build.sh

```

Para realizar al corrida de prueba:
```
cd sample_run
```

Modificar el archivo ``namelist.sh`` en caso de ser necesario (para correjir paths).

Y luego preparar campos de terreno, meteorolgía y radiosondeos::
```
./make_GEO.sh	#preparar campos de terreno
./make_SURF.sh
./make_UP.sh
```
Se van a crear los archivos ``geo.dat``, ``surf.dat``, ``precip.dat`` y ``up.dat``.

Luego para armar archivo de control y ejecutar el *calmet*:

```
./setup_calmet.sh
./calmet.exe
```

Deberia crearse el archivo ``calmet.dat`` que contiene todo lo necesario para la ejecución del *calpuff*:
```
./setup_calpuff.sh
./calpuff.exe

```

Para postprocesar las salidas del calpuff (``conc.dat``, ``df.dat``, etc.) se puede ejecutar el *calpost*
```
./setup_calpost.sh
./calpost.sh

```
este último genera tablas,series temporales  y grillas para graficar, con ayuda de un SIG, en el directorio ``out/``.
