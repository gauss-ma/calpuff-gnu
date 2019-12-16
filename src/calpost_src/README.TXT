----------------------------------------------------------------------
CALPOST  Concentration, Deposition, and Visibility Processor
----------------------------------------------------------------------

Purpose:
--------
       Prepare and summarize averages and exceedances from a CALPUFF
          simulation; compute and summarize visibility impacts.

Demonstration:
--------------

          Analyze the 1-hour SO2 concentrations produced by CALPUFF 
          for a 4-hour simulation, and report the peak 1-hour average
          concentration at each gridded receptor, and the number of 
          hours in which a threshold concentration is exceeded.


Files for Demonstration:
------------------------

   NAME        TYPE                 DESCRIPTION
___________   _______         ____________________________

 CALPOST.EXE  Executable      CALPOST executable

 CALPOST.INP  Input           Control file
 CALPUFF.CON  Input           CALPUFF concentration file

 CALPOST.LST  Output          List-file for application



Procedure:
----------

1.  This application may be run from the DOS prompt, or by means of
    the graphical user interface (GUI).  If run from DOS, follow the
    directions in Step 2.  If using the GUI, we assume that the CALPOST 
    icon is available on your desktop.

2.  Instructions for DOS (optional)

    (a)  Run the program by typing:

         CALPOST calpost.inp [return]

    (b)  The program reads the processing instructions from CALPOST.INP,
         overwrites the output file listed above.

    (c)  Review the .LST file for the configuration used, and the 
         results obtained.


3.  Instructions for the GUI


    (a)  Double click on the CALPOST icon to access the General
         Information Screen.

    (b)  Verify that the system is using the "large" CALPOST executable
         and its associated parameter-file (CALPOSTL.EXE and
         PARAMSL.PST).

    (c)  If the "large" version is active, go to step (e).

    (d)  Configure the system for the "large" version as follows:
          Click on Setup.  Click on Set Executables.
          Click in the executable filename box.
                Edit the name to change CALPOST to CALPOSTL.
          (make no other changes to the pathname!)
          Click in the parameter filename box.
                Edit the name to change PARAMS to PARAMSL.
          (make no other changes to the pathname!)
                Hit OK.
                Allow GUI to shut down (hit OK).
                Do not save current control file (hit NO).
          Return to step (a).

    (e)  Set the working directory to where the input files for this
         demonstration are located.
          Click on File.  Click on Change Directory.
                Double click on demo (under c:\calpuff\).
                Double click on calpost (under c:\calpuff\demo\).
                Hit OK.

    (f)  Click on File.  Click on Open.  Select CALPOST.INP.

    (g)  Click on Run.  Click on Run CALPOST

    (h)  Click on OK.  Click on Run.

    (i)  A DOS window opens for the model run; enter any key to close
         this window when the run is completed.

    (j)  To view the output list file, click on Utilities.  Click on
         View File.  Select CALPOST.LST.  To exit view, hit X or ESC.
         Hit F1 for a list of commands when in the view utility.

    (k)  To exit the CALPOST GUI, click on File.  Click on Exit.

 