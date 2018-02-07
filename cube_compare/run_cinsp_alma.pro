pro run_cinsp_alma


;; example 

      sfng_cube_inspect,datadir='./data_7m_osu/',outdir='./tmp/',plotdir='./plots_7m_osu_ic5332/' $
                      ,reportdir='./report_7m_osu_ic5332/',savedir='./savefiles'$
                      , fits_in='ic5332_7m_co21_pbcorr_round_k.fits',savefile='ic5332_7m_insp.sav' $
                      , namestr='ic5332_7m_osu' $
                      , jy2k=0,rebaseline=-1,expand_mask_edges=[5,2] $
                      ,/verb, galaxy='IC5332',target_beam=[8.,8.,0],/nostop  


      stop
 
  the_end:
  stop
end
