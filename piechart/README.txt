Analysis and plotting code for piechart paper.
Feel free to use code in this subdirectory if it's useful, but this part of the repository is mainly intended for Eva & Annie to share code.

master_sfco.pro -- wrapper that runs the code. use this to run the routines that do differents stages of the analysis

routines for three main stages:

1) prepare data, e.g. put data on same pixel grid, apply same FoV mask, convert units etc)
correct_data.pro
correct_ha.pro
correct_co.pro
make_coha_commonmask.pro


2) convolution, i.e. generate maps at matched set of physical resolutions 
convolve_for_sfco.pro

3) measure the various statistics for map pairs (overlap fraction etc). Generate plots and an IDL save file.
sfco_vs_scale.pro 
-- requires a 'key' file, e.g. SFNG_allgalaxies_80thresh.txt, that specifies which galaxies/image pairs to analyse
-- produces a .sav file, e.g. sfng_allgals_BGmaps_80thresh_pieonly.sav, where all the measurements are recorded

Code assumes that you already have the following libraries (among other things):
IDL Astro: https://github.com/wlandsman/IDLAstro
AKL's cpropstoo: https://github.com/akleroy/cpropstoo
AKL's gal_base: https://github.com/akleroy/galbase

There are also a few routines that read the measurement savefile and generate 'summary/overview' plots, e.g.


 




