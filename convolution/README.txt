Code to generate maps/cubes at a series of resolutions.

Warning: code may fail if you have pathological headers. So far only tested on small set of CO data. Please contact me (Annie) if you encounter maps/cubes that fail, and I will try to improve error trapping.

sfng_convolve_to_res.pro --  
     given an input map/cube, produces FITS files at a set of requested resolutions
     mostly a wrapper to AKL's conv_with_gauss
     uses galbase to convert physical resolutions to Gaussian smoothing kernels with angular size

run_sfng_convolve_to_res_example.pro
     some minimal examples of how to call sfng_convolve_to_res
     you need to modify the examples to provide an input dataset, and specify where you want the output FITS files 
     


