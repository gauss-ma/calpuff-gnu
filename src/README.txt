Codes & Related Processors: USEPA-Approved Version

The CALPUFF modeling system is comprised of CALMET, CALPUFF, geophysical data preprocessors, meteorological data preprocessors, and several postprocessors designed to operate on CALMET and CALPUFF output files.

The downloads available here focus individually on each component, providing a brief overview of the purpose, sample input and output files, the executable, and the FORTRAN code (in most cases). The sample input and output files are interrelated among the processors, producing a demonstration of the full system. This demonstration involves a single application to a hypothetical source, using real meteorological and geophysical data.

The demonstration is best approached in the following sequence: Geophysical Preprocessors, Meteorological Preprocessors, Main Models (CALMET, CALPUFF), and Postprocessors. Process the terrain data first, and then land use, etc. All of the processing should be done in the individual subdirectories. Go to each in turn, and read the README.TXT file. This file contains the documentation for each processor. Note that all of the processing relates to the same application except the examples for BUOY.

Once you have gone through the demonstration, you may delete the data files used in the demonstration, but keep the executables, documentation, and the control files for regular use.

Geophysical Data Processors and Data
  TERREL  Version 3.684 (070327) -- Combines and grids terrain data. Terrain Data
  CTGCOMP   Version 2.25 (070327) -- Compresses USGS land use data files. CTG Land Use Data
  CTGPROC   Version 2.682 (070430) -- Processes and grids land use data.
  MAKEGEO   Version 2.29 (070327) -- Merges land use and terrain data to produce the geophysical data file for CALMET.

 
Meteorological Preprocessors
  SMERGE  Version 5.57 (070627) -- Combines and formats files of surface meteorological data to produce a single file for use in CALMET.
  PXTRACT   Version 4.25 (070327) -- Extracts precipitation data from NCDC data sets.
  PMERGE  Version 5.32 (070627) -- Processes and merges precipitation data to produce a single file for use in CALMET.
  READ62  Version 5.54 (070627) -- Processes NCDC upper air meteorological data to produce upper air files for CALMET.
  BUOY  Version 1.245 (070327) -- Processes buoy data to produce overwater files for CALMET.
  CALMM5  Version 2.7 (061030) -- Reads MM5 output data and reformats it into 3D.DAT format suitable for input into CALMET.

Main Models
  CALMET  Version 5.8.5 (151214) -- 3-D diagnostic meteorological model.
  CALPUFF   Version 5.8.5 (151214) -- Non-steady-state transport, dispersion, and deposition model.

Postprocessors
  CALPOST   Version 6.221 (080724) -- Postprocessor for CALPUFF: performs visibility calculations, averages and summarizes concentrations and deposition fluxes, determines ranked concentration/flux/plume extinction values, creates list files and plot files. This version includes "Method 8" (MVISBK = 8, M8_MODE = 5, MVISCHECK = 1), which utilizes the revised IMPROVE equation per the Federal Land Managers' Air Quality Related Values Work Group (FLAG) revised October 2010 report.
  PRTMET  Version 4.34 (070627) -- Lists selected meteorological data from CALMET and creates plot files.
  APPEND  Version 2.34 (051122) -- Appends two or more CALPUFF concentration, wet flux, dry flux or relative humidity (visibility) files.
  CALSUM  Version 1.33 (051122) -- Sums and scales concentrations or wet/dry fluxes from two or more source groups from different CALPUFF runs.
  POSTUTIL  Version 1.56 (070627) -- Processes CALPUFF concentration and wet/dry flux files. Creates new species as weighted combinations of modeled species; merges species from different runs into a single output file; sums and scales results from different runs; repartitions nitric acid/nitrate based on total available sulfate and ammonia.
  
