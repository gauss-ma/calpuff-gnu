----------------------------------------------------------------------
CTGCOMP Compression Utility for USGS CTG Files
----------------------------------------------------------------------

Purpose:
--------
	  Reads and compresses a USGS LULC data file before data are
          processed by CTGPROC.

Demonstration:
--------------

          Prepare compressed CTG fractional land use files for a 99km
          by 99km domain for which there are two USGS CTG files.


Files for Demonstration:
------------------------

   NAME        TYPE     SIZE(bytes)             DESCRIPTION
___________   _______  ____________     ____________________________

 CTGCOMP.EXE  Program       564,204     Executable
 DD_CTG.EXE   Program       454,137     Executable

 CTGCOMP1.INP Input           1,156     Control file for CTG file 1
 CTGCOMP2.INP Input           1,156     Control file for CTG file 2
 LEWISTON.CTG Input      36,247,034     Uncompressed CTG file 1
 PORTLAND.CTG Input      31,067,504     Uncompressed CTG file 2

 LEWISTON.CMP Output        673,377     Compressed CTG file 1
 PORTLAND.CMP Output        818,822     Compressed CTG file 2
 CTGCOMP1.LST Output          1,366     List-file for application 1
 CTGCOMP2.LST Output          1,366     List-file for application 2



Procedure:
----------

1.  Two applications of CTGCOMP are needed, and there is a control
    file for each application.  The LEWISTON and PORTLAND files were
    obtained from the USGS FTP site, and reformatted by running 
    DD_CTG.EXE to add record delimiters.  Running CTGCOMP in this
    directory will not alter these files.  DD_CTG.EXE (not illustrated
    in this demonstration) prompts for the names of the input and
    output files, with no other control options, and may be used on PCs
    to perform the function of the DD UNIX utility described by the
    USGS documentation.

2.  Run the FIRST application by typing:

    ctgcomp ctgcomp1.inp [return]

3.  The program reads the processing instructions from CTGCOMP2.INP, and
    overwrites the LEWISTON.CMP and CTGCOMP1.LST files listed above.

4.  Review the .LST file for the configuration used, and intermediate
    information.

5.  Run the SECOND application by typing:

    ctgcomp ctgcomp2.inp [return]

6.  The program reads the processing instructions from CTGCOMP2.INP, and
    overwrites the PORTLAND.CMP and CTGCOMP2.LST files listed above.

7.  Review the .LST file for the configuration used, and intermediate
    information.
