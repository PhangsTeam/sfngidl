Cube Compare
------------

Contact person: Annie

Routines in this subdirectory are for generating an automatic summary
of the properties of two input cubes, e.g. the ALMA TP and
IRAM HERA cubes for NGC628.

The main outputs are:
(i) an IDL save structure containing measurements of different properties of both cubes
(ii) a PDF report (if requested)
(iii) lots of plots

The main routine is sfng_cube_compare.pro, which calls all the other routines that you will find here.

run_sfng_cube_compare_example.pro contains an example of a typical call to sfng_cube_compare.pro

***Note that consecutive runs of sfng_cube_compare.pro use the same
filenames for the plots, intermediate FITS files (to some degree) and
all the intermediate latex files that are used to generate the final
PDF report, so you should be careful about over-writing what you've
created***

Still to add:
* power spectra
