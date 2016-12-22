function sfng_empty_cubecmp_str

  nan = !values.f_nan
  stats_struct = sfng_empty_stats_str()
  
  ccmp_str = {c1_file: '' , $
              c2_file: '' , $
              c1_rebase_flag: 0 , $
              c2_rebase_flag: 0 , $
              c1_rebase_order: -1 , $
              c2_rebase_order: -1 , $
              c1_jy2k_flag: 0 , $
              c2_jy2k_flag: 0 , $
              cmp_fov_fits: '' , $
              c1_signalmask_fits: '' , $
              c2_signalmask_fits: '' , $
              joint_signalmask_fits: '' , $
              joint_nosignalmask_fits: '' , $
              c1_comments: '' , $
              c2_comments: '' , $
              c1_pixscale: nan , $
              c2_pixscale: nan , $
              c1_chanw: nan , $
              c2_chanw: nan , $
              c1_bunit: '' , $
              c2_bunit: '' , $
              c1_beam: fltarr(3) , $
              c2_beam: fltarr(3) , $
              c1_dims: fltarr(3) , $
              c2_dims: fltarr(3) , $
              pixscale_as: nan , $
              chanw_kms: nan , $
              angres_as: nan , $
              dims: fltarr(3) , $
              c1_rmsmap_fits: '' , $
              c2_rmsmap_fits: '' , $
              c1_rmscube_fits: '' , $
              c2_rmscube_fits: '' , $
              c1_totflux: nan , $
              c2_totflux: nan , $
              c1_totflux_signalmask: nan , $
              c2_totflux_signalmask: nan , $
              c1_totflux_jointsignalmask: nan , $
              c2_totflux_jointsignalmask: nan , $
              c1_totflux_nosignalmask: nan , $
              c2_totflux_nosignalmask: nan , $
              totfluxdiff: nan , $
              totfluxdiff_jointsignalmask: nan , $
              totfluxdiff_nosignalmask: nan , $
              c1_npix_signalmask: nan , $
              c2_npix_signalmask: nan , $
              npix_cmp_fov: nan , $
              npix_jointsignalmask: nan , $
              npix_nosignalmask: nan , $
              c1_fluxperchan: ptr_new() , $
              c2_fluxperchan: ptr_new() , $
              c1_fluxperchan_jointsignalmask: ptr_new() , $
              c2_fluxperchan_jointsignalmask: ptr_new() , $
              c1_rmsperchan: ptr_new() , $
              c2_rmsperchan: ptr_new() , $
              c1_rmsperchan_nosignalmask: ptr_new() , $
              c2_rmsperchan_nosignalmask: ptr_new() , $
              fluxdiffperchan: ptr_new() , $
              fluxdiffperchan_jointsignalmask: ptr_new() , $
              fluxdiffperchan_nosignalmask: ptr_new() , $
              c1_noisestats: stats_struct , $
              c2_noisestats: stats_struct , $
              c1_noisestats_nosignal: stats_struct , $
              c2_noisestats_nosignal: stats_struct , $
              diffcube_stats: stats_struct , $
              diffcube_stats_nosignal: stats_struct , $
              rpx_c1c2: nan , $
              rank_c1c2: fltarr(2) , $
              lincorr_c1c2: fltarr(2) , $
              lincorr_c2c1: fltarr(2) , $
              logcorr_c1c2: fltarr(2) , $
              logcorr_c2c1: fltarr(2) $
             }

  return,ccmp_str
  
end
