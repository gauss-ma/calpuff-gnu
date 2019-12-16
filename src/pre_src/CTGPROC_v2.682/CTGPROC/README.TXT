----------------------------------------------------------------------
CTGPROC Land Use Processor
----------------------------------------------------------------------

Purpose:
--------
	  Reads compressed USGS LULC data files and determines the
          fractional land use for each grid cell in the domain (grid).
	  Continuation runs may be used.

Demonstration:
--------------

          Prepare a gridded fractional land use file for a 99km by 99km
          domain for which there are two CTG files.


Files for Demonstration:
------------------------

   NAME        TYPE     SIZE(bytes)             DESCRIPTION
___________   _______  ____________     ____________________________

 CTGPROC.EXE  Program       973,171     Executable

 CTGPROC.INP  Input           9,213     Control file
 LEWISTON.CMP Input         673,377     Compressed CTG file 1
 PORTLAND.CMP Input         818,822     Compressed CTG file 2

 LULC1KM.DAT  Output      2,352,556     Fractional land use for MAKEGEO
 CTGPROC.LST  Output         88,385     List-file for application



Procedure:
----------

1.  Run the application by typing:

    ctgproc ctgproc.inp [return]

2.  The program reads the processing instructions from CTGPROC.INP, and
    LULC1KM.DAT and CTGPROC.LST files listed above

3.  Review the .LST file for the configuration used, and intermediate
    information.  The warnings about cell coverage (for land use) can
    be ignored because land use is not provided for the offshore cells
    in this domain, and a water LU category will be used for these
    cells when MAKEGEO is applied.

