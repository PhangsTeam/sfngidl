pro run_sfng_cube_inspect_example

;; following is to inspect output of pipeline reduction

pipe_v1p0_dr:

  cubename='_co21_12m+7m'
  mdlname='_co21_12m+7m_model'
  resname='_co21_12m+7m_residual'
  savename='_inspect.sav'
  
  gallist=[ 'IC5332', $
            'NGC0628', $
            'NGC1672', $
            'NGC2835', $
            'NGC3351', $
            'NGC3627', $
            'NGC3627', $
            'NGC4254', $
            'NGC4254', $
            'NGC4303', $
            'NGC4321', $
            'NGC4321', $
            'NGC4535', $
            'NGC5068', $
            'NGC5068', $
            'NGC6744', $
           'NGC6744']

    obslist=['ic5332', $
            'ngc0628', $
            'ngc1672', $
            'ngc2835', $
            'ngc3351', $
            'ngc3627north', $
            'ngc3627south', $
            'ngc4254north', $
            'ngc4254south', $
            'ngc4303', $
            'ngc4321north', $
            'ngc4321south', $
            'ngc4535', $
            'ngc5068north', $
            'ngc5068south', $
            'ngc6744north', $
           'ngc6744south']

; smooth each cube slightly to a round beam with this resolution in arcseconds
    tbmlist=[ 1.0, $            ;'ic5332', $
              1.2, $            ; 'ngc0628', $
              2.0, $            ; 'ng1672',
              1.0, $            ;'ngc2835', $
              1.5, $            ; 'ngc3351', $
              1.5, $            ;'ngc3627north', $
              1.5, $            ;'ngc3627south', $
              2.0, $            ;'ngc4254north', $
              2.0, $            ;'ngc4254south', $
              2.0, $            ;'ngc4303', $
              1.5, $            ; 'ngc4321north', $
              1.5, $            ;'ngc4321south', $
              2.0, $            ;'ngc4535', $
              1.0, $            ; 'ngc5068north', $
              1.0, $            ;'ngc5068south', $
              1.2, $            ; 'ngc6744north', $
              1.2]              ;'ngc6744south']

  cubelist=obslist+cubename
  mdllist=obslist+mdlname
  reslist=obslist+resname
  savelist=obslist+savename
  plotdirlist='./plots_'+obslist+'_inspect/'
  reportdirlist='./reports_'+obslist+'_inspect/'
  use_datadir = './orig_data/'
  use_outdir ='./good_data/'
   use_savedir = './savefiles/'

   ngals=n_elements(gallist)
  
  for i=0,ngals-1 do begin
     
     use_c1file=cubelist[i]
     use_m1file=mdllist[i]
     use_r1file=reslist[i]
     use_savefile=savelist[i]
     use_plotdir=plotdirlist[i]
     use_reportdir=reportdirlist[i]
     use_targetbeam=[tbmlist[i],tbmlist[i],0.]

     print,'Galaxy: ',use_c1file
     print,'Target beam: ',use_targetbeam

     ; downsample by a factor 2 because these cubes are huge
     sfng_resample_cube,infile=use_datadir+use_m1file+'.fits', factor=2.,outfile=use_datadir+mdllist[i]+'.resamp2.fits'
     sfng_resample_cube,infile=use_datadir+use_r1file+'.fits', factor=2.,outfile=use_datadir+reslist[i]+'.resamp2.fits'
     sfng_resample_cube,infile=use_datadir+use_c1file+'.fits', factor=2.,outfile=use_datadir+cubelist[i]+'.resamp2.fits'

     use_c1file=cubelist[i]+'.resamp2.fits'
     use_m1file=mdllist[i]+'.resamp2.fits'
     use_r1file=reslist[i]+'.resamp2.fits'


     sfng_cube_inspect,datadir=use_datadir,outdir=use_outdir,plotdir=use_plotdir $
                       ,reportdir=use_reportdir,savedir=use_savedir $
                       , fits_in=use_c1file, fits_mdl=use_m1file, fits_res=use_r1file,savefile=use_savefile $
                       , namestr=obslist[i], /extrachanmaps $
                       , jy2k=1,rebaseline=-1,expand_mask_edges=[5,2] $
                       ,/verb, galaxy=gallist[i],target_beam=use_targetbeam,/nostop  

     stop
     

  endfor 

  the_end:
end
