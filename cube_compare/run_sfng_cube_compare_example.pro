pro run_sfng_cube_compare_example
  
  use_c1file='12co21-30m-only.vcorr.LSR.fits'
  use_c2file='m74_TP_12CO21.LSR.fits'
  use_savefile='ccmp_results.sav'

  use_datadir = './data/'
  use_outdir ='./good_data/'
  use_plotdir = './plots/'
  use_reportdir = './report/'

  sfng_cube_compare,datadir=datadir,outdir=outdir,plotdir=plotdir,reportdir=reportdir $
                    , fits_in1=use_c1file,fits_in2=use_c2file,savefile=use_savefile $
                    , xygrid=2,vgrid=1,jy2k=[0,1]
  
  stop
  

end
