----------------------------------------------------------------------
SMERGE Surface Meteorological Data Processor
----------------------------------------------------------------------

Purpose:
--------
	  Reads one or more files of hourly surface meteorological data
          and combines and reformats data to produce the SURF.DAT file
          for CALMET.  Continuation runs may be used to add or remove
          data from individual stations.

Demonstration:
--------------

          Prepare a SURF.DAT file for one week, combining several 
          files in CD144 format with one file in NCDC SAMSON
          format.


Files for Demonstration:
------------------------

   NAME        TYPE     SIZE(bytes)             DESCRIPTION
___________   _______  ____________     ____________________________

 SMERGE.EXE   Program       673,675     Executable

 SMERGE1.INP  Input           5,214     Control file for CD144 stations
 SMERGE2.INP  Input           4,958     Control file for adding SAMSON data
 BANGOR.144   Input         720,288     Surface Met File for 1 Station
 BRUNSWK.144  Input         720,288     Surface Met File for 1 Station
 BURLING.144  Input         720,288     Surface Met File for 1 Station
 CONCORD.144  Input         720,288     Surface Met File for 1 Station
 PORTLND.CDR  Input       1,226,674     Surface Met File for 1 Station
 FIRSTRUN.DAT Input          41,604     Continuation file (for run 2)

 FIRSTRUN.DAT Output         41,604     Continuation file (from run 1)
 SURF.DAT     Output         51,358     Surface Met Data for CALMET
 SMERGE1.LST  Output          2,280     List-file for run 1
 SMERGE2.LST  Output          2,712     List-file for run 2



Procedure:
----------

1.  Two applications of SMERGE are needed, and there is a control
    file for each application (SMERGE1.INP and SMERGE2.INP).

2.  Run the FIRST application to combine the 4 CD144-format files
    by typing:

    smerge smerge1.inp [return]

3.  The program reads the processing instructions from SMERGE1.INP, and
    overwrites the FIRSTRUN.DAT file listed above.

4.  Review the .LST file for the configuration used, and intermediate
    information.

5.  Run the SECOND application by typing:

    smerge smerge2.inp [return]

6.  The program reads the processing instructions from SMERGE2.INP, and
    overwrites the SURF.DAT file listed above.

7.  Review the .LST file for the configuration used, and intermediate
    information.  Data for all five surface meteorological stations
    for the 1-week period are now in SURF.DAT.  This ASCII file can be
    viewed to verify this.
