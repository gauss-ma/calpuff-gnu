----------------------------------------------------------------------
PXTRACT Precipitation Data Extraction Processor
----------------------------------------------------------------------

Purpose:
--------
          Reads one file of precipitation data (TD-3240, fixed record
          length format) containing many days of data from many sites,
          and writes selected data to station-files for use by PMERGE.
          Data are selected by specifying an extraction period, and by
          specifying individual stations by station ID, by state ID, or
          by accepting all stations in the file.

Demonstration:
--------------

          Extract data for a 2-week period at 16 stations located in
          3 states.


Files for Demonstration:
------------------------

   NAME        TYPE     SIZE(bytes)             DESCRIPTION
___________   _______  ____________     ____________________________

 PXTRACT.EXE  Program       606,649     Executable

 PXTRACT.INP  Input           5,531     Control file for application
 TD3240.DAT   Input       1,576,256     NCDC TD3240 Precip File

 PXTRACT.LST  Output          2,312     List-file for application
 170273.DAT   Output            220     Single-Station Precip File
 176905.DAT   Output            484     Single-Station Precip File
 177325.DAT   Output            220     Single-Station Precip File
 178641.DAT   Output            220     Single-Station Precip File
 270741.DAT   Output            176     Single-Station Precip File
 270998.DAT   Output            176     Single-Station Precip File
 272842.DAT   Output            264     Single-Station Precip File
 273182.DAT   Output            176     Single-Station Precip File
 274732.DAT   Output            308     Single-Station Precip File
 274808.DAT   Output            176     Single-Station Precip File
 275639.DAT   Output          3,080     Single-Station Precip File
 275780.DAT   Output            176     Single-Station Precip File
 276234.DAT   Output            308     Single-Station Precip File
 276818.DAT   Output            528     Single-Station Precip File
 278885.DAT   Output            220     Single-Station Precip File
 437054.DAT   Output            176     Single-Station Precip File


Procedure:
----------

1.  Run the application to extract the 16 station-files
    by typing:

    pxtract pxtract.inp [return]

2.  The program reads the processing instructions from PXTRACT.INP, and
    overwrites the PXTRACT.LST and all of the *.DAT output files
    listed above.

3.  Review the .LST file for the configuration used, and intermediate
    information.
