pro run_sfng_convolve_to_res_example

  goto, eg4

  eg1:
  cube=readfits('test_data/NGC5068_co21_cube.fits',h)
  sfng_convolve_to_res,idl_in=cube,hdr_in=h $
                       ,galaxy='NGC5068' $
                       ,datadir='test_data' $
                       ,outdir='./test_data/' $
                       ,target_res=[0.05,0.2,0.5,0.8]*1.e3 $
                       ,fits_out='NGC5068_co21_cube' $
                       ,/verbose

  stop

  eg2:
  sfng_convolve_to_res,fits_in='NGC5068_co21_cube_mom0.fits' $
                       ,galaxy='NGC5068' $
                       ,datadir='test_data/' $
                       ,outdir='test_data' $
                       ,target_res=[0.5,2,4,8] $
                       ,fits_out='NGC5068_co21_mom0' $
                       ,/verbose,/arcsecond,/strict

  stop

    eg3:
  sfng_convolve_to_res,fits_in='NGC5068_co21_cube_mom0.fits' $
                       ,distance=7. $
                       ,datadir='test_data/' $
                       ,outdir='test_data' $
                       ,target_res=15. $
                       ,/verbose,/arcsecond,/strict

  stop


  eg4:
  ; should fail because convolving beams are both too small
  sfng_convolve_to_res,fits_in='NGC5068_co21_cube_mom0.fits' $
                       ,distance=7. $
                       ,datadir='test_data/' $
                       ,outdir='test_data' $
                       ,target_res=[1.,2.] $
                       ,/verbose

  stop

end
