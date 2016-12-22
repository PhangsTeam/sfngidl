pro run_sfng_convolve_to_res_example

  goto, eg4

; in eg1, input data is provided as IDL cube and header
; target resolutions are in spatial units [50,200, 500, 800] pc (default)
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

; in eg2, input data is specified as FITS file 
; we use a 2D moment map rather than cube
; target resolutions are in angular units [0.5, 2, 4. 8] as (/arcsecond keyword is set) 
  eg2:
  sfng_convolve_to_res,fits_in='NGC5068_co21_cube_mom0.fits' $
                       ,galaxy='NGC5068' $
                       ,datadir='test_data/' $
                       ,outdir='test_data' $
                       ,target_res=[0.5,2,4,8] $
                       ,fits_out='NGC5068_co21_mom0' $
                       ,/verbose,/arcsecond,/strict

  stop

; in eg3,  we use a user-specified distance, rather than query galbase
  eg3:
  sfng_convolve_to_res,fits_in='NGC5068_co21_cube_mom0.fits' $
                       ,distance=7. $
                       ,datadir='test_data/' $
                       ,outdir='test_data' $
                       ,target_res=15. $
                       ,/verbose,/arcsecond,/strict

  stop


  ; eg4 should fail to produce any output FITS files because convolving beams are both too small
  eg4:
  sfng_convolve_to_res,fits_in='NGC5068_co21_cube_mom0.fits' $
                       ,distance=7. $
                       ,datadir='test_data/' $
                       ,outdir='test_data' $
                       ,target_res=[1.,2.] $
                       ,/verbose

  stop

end
