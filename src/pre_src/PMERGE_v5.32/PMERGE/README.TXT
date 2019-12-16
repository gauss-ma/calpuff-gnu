----------------------------------------------------------------------
PMERGE Precipitation Data Processor
----------------------------------------------------------------------

Purpose:
--------
          Reads one or more station-files of precipitation data
          (TD-3240, fixed record length format) extracted by PXTRACT,
          resolves accumulation periods, flags missing data, and
          reformats selected data for use by CALMET.  Output records
          are sorted by time, rather than station.
          Data are selected by specifying an extraction period, by
          specifying the individual station-files to merge, and by
          selecting specific station ID's to be retained from a 
          previously merged file.

Demonstration:
--------------

          Merge data for a 2-week period from 16 station-files.


Files for Demonstration:
------------------------

   NAME        TYPE     SIZE(bytes)             DESCRIPTION
___________   _______  ____________     ____________________________

 PMERGE.EXE   Program       680,361     Executable

 PMERGE.INP   Input           6,410     Control file for application
 170273.DAT   Input             220     Single-Station Precip File
 176905.DAT   Input             484     Single-Station Precip File
 177325.DAT   Input             220     Single-Station Precip File
 178641.DAT   Input             220     Single-Station Precip File
 270741.DAT   Input             176     Single-Station Precip File
 270998.DAT   Input             176     Single-Station Precip File
 272842.DAT   Input             264     Single-Station Precip File
 273182.DAT   Input             176     Single-Station Precip File
 274732.DAT   Input             308     Single-Station Precip File
 274808.DAT   Input             176     Single-Station Precip File
 275639.DAT   Input           3,080     Single-Station Precip File
 275780.DAT   Input             176     Single-Station Precip File
 276234.DAT   Input             308     Single-Station Precip File
 276818.DAT   Input             528     Single-Station Precip File
 278885.DAT   Input             220     Single-Station Precip File
 437054.DAT   Input             176     Single-Station Precip File

 PMERGE.LST   Output          5,991     List-file for application
 PRECIP.DAT   Output         58,860     Data file for CALMET



Procedure:
----------

1.  Run the application to merge the 16 station-files
    by typing:

    pmerge pmerge.inp [return]

2.  The program reads the processing instructions from PMERGE.INP, and
    overwrites the PMERGE.LST and PRECIP.DAT files listed above.

3.  Review the .LST file for the configuration used, and intermediate
    information.
