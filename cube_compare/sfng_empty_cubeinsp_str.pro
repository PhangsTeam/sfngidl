function sfng_empty_cubeinsp_str

  nan = !values.f_nan
  stats_struct = sfng_empty_stats_str()
  
  cinsp_str = {c1_file: '' , $
              c1_rebase_flag: 0 , $
              c1_rebase_order: -1 , $
              c1_jy2k_flag: 0 , $
              cmp_fov_fits: '' , $
              c1_signalmask_fits: '' , $
              c1_nosignalmask_fits: '' , $
              c1_comments: '' , $
              c1_pixscale: nan , $
              c1_chanw: nan , $
              c1_bunit: '' , $
              c1_casa: '' , $
              c1_beam: fltarr(3) , $
              c1_dims: fltarr(3) , $
              pixscale_as: nan , $
              chanw_kms: nan , $
              angres_as: nan , $
              dims: fltarr(3) , $
              c1_rmsmap_fits: '' , $
              c1_rmscube_fits: '' , $
              c1_totflux: nan , $
              c1_peak: nan , $
              c1_totflux_signalmask: nan , $
              c1_totflux_nosignalmask: nan , $
              c1_npix_signalmask: nan , $
              npix_tot: nan , $
              npix_cmp_fov: nan , $
              npix_jointsignalmask: nan , $
              npix_nosignalmask: nan , $
              c1_fluxperchan: ptr_new() , $
              c1_fluxperchan_signalmask: ptr_new() , $
              emission_startchan:nan, $
              emission_endchan:nan, $
              c1_rmsperchan: ptr_new() , $
              c1_rmsperchan_nosignalmask: ptr_new() , $
              c1_noisestats: stats_struct , $
              c1_noisestats_nosignal: stats_struct $
             }

  return,cinsp_str
  
end
