pro run_sfng_cube_compare_example
  
  use_c1file='12co21-30m-only.vcorr.LSR.fits'
  use_c2file='m74_TP_12CO21.LSR.fits'
  use_savefile='ccmp_results.sav'

  use_datadir = './orig_data/'
  use_outdir ='./good_data/'
  use_plotdir = './plots/'
  use_reportdir = './report/'
  use_savedir = './savefiles/'

  sfng_cube_compare,datadir=use_datadir,outdir=use_outdir,plotdir=use_plotdir $
                    ,reportdir=use_reportdir,savedir=use_savedir $
                    , fits_in1=use_c1file,fits_in2=use_c2file,savefile=use_savefile $
                    , xygrid=2,vgrid=1,jy2k=[0,1],rebaseline=[1,1],expand_mask_edges=[5,2] $
                    , target_beam=[30.,30.,0]
  
  stop
  

end
