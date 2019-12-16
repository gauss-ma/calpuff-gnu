----------------------------------------------------------------------
READ62 Upper Air Meteorological Data Processor
----------------------------------------------------------------------

Purpose:
--------
	  Reads one file of twice-daily wind and temperature soundings,
          reports problems if any are found, and reformats data to
          produce an UP.DAT file for CALMET.

Demonstration:
--------------

          Prepare three UP.DAT files for one two-week period, using
          RAOB files in NCDC CD-ROM (FSL) format.


Files for Demonstration:
------------------------

   NAME        TYPE     SIZE(bytes)             DESCRIPTION
___________   _______  ____________     ____________________________

 READ62.EXE   Program       632,844     Executable

 R62ALB.INP   Input           5,791     Control file for Albany, NY
 R62PWM.INP   Input           5,799     Control file for Portland, ME
 R62CHH.INP   Input           5,791     Control file for Chatham, MA
 90010100.ALB Input       3,293,112     RAOB data for Albany, NY
 90010100.PWM Input       3,134,226     RAOB data for Portland, ME
 90010100.CHH Input       3,194,906     RAOB data for Chatham, MA

 UPALB.DAT    Output         36,758     Upper Air Data for CALMET
 UPALBR.DAT   Output         36,582     EDITED Upper Air Data for CALMET
 UPPWM.DAT    Output         30,210     Upper Air Data for CALMET
 UPCHH.DAT    Output         27,353     Upper Air Data for CALMET
 R62ALB.LST   Output          4,560     List-file for Albany
 R62PWM.LST   Output          4,384     List-file for Portland
 R62CHH.LST   Output          4,384     List-file for Chatham



Procedure:
----------

1.  Three applications of READ62 are needed, and there is a control
    file for each application.

2.  Run the FIRST application to prepare the UP.DAT file for Albany
    by typing:

    read62 r62alb.inp [return]

3.  The program reads the processing instructions from R62ALB.INP, and
    overwrites the R62ALB.LST and UPALB.DAT files listed above.

4.  Run the SECOND application to prepare the UP.DAT file for Portland
    by typing:

    read62 r62PWM.inp [return]

5.  The program reads the processing instructions from R62PWM.INP, and
    overwrites the R62PWM.LST and UPPWM.DAT files listed above.

6.  Run the THIRD application to prepare the UP.DAT file for Portland
    by typing:

    read62 r62CHH.inp [return]

7.  The program reads the processing instructions from R62CHH.INP, and
    overwrites the R62CHH.LST and UPCHH.DAT files listed above.

8.  Review the .LST files for the configuration used, and intermediate
    information.  Time-periods with problems are identified.

9.  Review the "UP.DAT" files created for potential problems.  For
    example, check the uppermost level at which wind data are valid.
    If the level is below the top of the CALMET modeling domain, the
    sounding must be edited, or CALMET will stop with an error message.
    READ62 has screened the soundings for a number of problems like
    this, and has marked all that are found.  The error reports in the
    "UP.DAT" file must be removed prior to running CALMET.  For this
    period, only a few soundings from Albany (upalb.dat) must be
    modified.  The edited version of this file is upalbr.dat.  The 
    other files pass all screening checks, so no further editing is
    needed.
