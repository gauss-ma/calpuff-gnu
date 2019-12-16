----------------------------------------------------------------------
TERREL Terrain Grid Processor
----------------------------------------------------------------------

Purpose:
--------
          Extract and average terrain elevation data from one or more
          files (and up to 4 formats) to produce a gridded array of
          elevations for the CALMET "GEO.DAT" input file.


Demonstration:
--------------

          Prepare terrain for a 99km by 99km domain covering portions
          of 4 USGS 1-degree DEM pages, using 4 files in USGS(1:250,000)
          format (downloaded from the USGS FTP site).


Files for Demonstration:
------------------------

   NAME        TYPE     SIZE(bytes)             DESCRIPTION
___________   _______  ____________     ____________________________

 TERREL.EXE   Program    13,121,239     Executable
 DD_DEM.EXE   Program       454,137     Executable

 TERREL.INP   Input          13,868     Control file
 LEWISTON.E   Input       9,859,860     USGS data file
 LEWISTON.W   Input       9,859,860     USGS data file
 PORTLAND.E   Input       9,859,860     USGS data file
 PORTLAND.W   Input       9,859,860     USGS data file

 TERR1KM.DAT  Output         80,702     Results ready for GEO.DAT
 TERR1KM.LST  Output        216,744     List-file for application
 QATERR.GRD   Output         80,478     Results in .GRD format
 TERR1KM.SAV  Output        117,668     Save-file for subsequent
                                        applications (if needed)




Procedure:
----------

1.  Unzip the four 1-deg DEM files.  The LEWISTON (east and west)
    and the PORTLAND (east and west) files were obtained from the
    USGS FTP site, and reformatted by running DD_DEM.EXE to add record
    delimiters.  Running TERREL in this directory will not alter these
    files.  DD_DEM.EXE (not illustrated in this demonstration) 
    prompts for the names of the input and output files, with no other
    control options, and may be used on PCs to perform the function of
    the DD UNIX utility described by the USGS documentation.

2.  Run the program by typing:

    terrel terrel.inp [return]

3.  The program reads the processing instructions from TERREL.INP, and
    overwrites each of the output files listed above.

4.  Review the .LST file for the configuration used, and intermediate
    information (e.g. area covered).
