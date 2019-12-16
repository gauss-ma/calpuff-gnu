----------------------------------------------------------------------
MAKEGEO "GEO.DAT" Processor
----------------------------------------------------------------------

Purpose:
--------
	  Assembles gridded land use and terrain elevation data into
	  GEO.DAT file for use in CALMET.  Land use data are re-mapped
	  and dominant land use is computed from fractional land use.
	  Surface parameters are computed from the fractional land use.

Demonstration:
--------------

          Prepare GEO.DAT for a 99km by 99km domain using terrain data
          gridded by TERREL, and fractional land use data gridded by
          CTGPROC.


Files for Demonstration:
------------------------

   NAME        TYPE     SIZE(bytes)             DESCRIPTION
___________   _______  ____________     ____________________________

 MAKEGEO.EXE  Program       887,975     Executable

 MAKEGEO.INP  Input          13,795     Control file
 TERR1KM.OUT  Input          80,702     Gridded terrain elevations
 LULC1KM.DAT  Input       2,352,556     Gridded fractional land use

 GEO1KM.DAT   Output        630,931     GEO.DAT ready for CALMET
 MAKEGEO.LST  Output        430,552     List-file for application
 QALUSE.GRD   Output         49,115     Landuse GRD file for plotting
 QATERR.GRD   Output        117,722     Terrain GRD file for plotting
 LUSE.CLR     Output            534     Default color file for plotting



Procedure:
----------

1.  Start with the terrain and land use files from TERREL and CTGPROC.
    Running MAKEGEO in this directory will not alter these files.

2.  Run the program by typing:

    makegeo makegeo.inp [return]

3.  The program reads the processing instructions from MAKEGEO.INP, and
    overwrites each of the output files listed above.

4.  Review the .LST file for the configuration used, and intermediate
    information.  The warnings about cell coverage (for land use) can
    be ignored because land use is not provided offshore in this domain
    and the water category has been used for these cells.
