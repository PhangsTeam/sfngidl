function empty_sfco_stats
;+
; Returns an empty structure to hold the results
; returned by sfco_versus_scale or sfco_versus_scale_physical
;-

  nan = !values.f_nan

  empty_sfco_stats = {gal:"", $
; BASIC INFORMATION ABOUT GALAXY (COPIED FROM GAL_BASE)
                      gal_dist:nan, $
                      gal_hi_mass:nan, $
                      gal_lfir_lsun:nan, $
                      gal_incl_deg:nan, $
                      gal_ttype:nan, $
                      gal_r25_deg:nan, $
                      gal_bar:nan, $
                      gal_vrot_kms:nan, $
                      gal_btc_mag:nan, $
                      gal_btc_absmag:nan, $
                      gal_logssfr:nan, $
                      gal_sfr:nan, $
                      gal_stellarmass:nan, $
; BASIC INFORMATION ABOUT INPUT PARAMETERS USED
                      tagx:"", $
                      tagy:"", $
                      co:"", $
                      sfr:"", $
                      mask:"", $
                      res_as:nan, $
                      res_pc:nan, $
                      perc:nan, $
                      cothreshval:nan, $
                      sfrthreshval:nan, $
                      env:0, $
                      force_pos:0, $
; BASIC INFORMATION ABOUT INPUT MAPS
                      co_flux_allpix:nan, $
                      co_flux_pospix:nan, $
                      sfr_flux_allpix:nan, $
                      sfr_flux_pospix:nan, $
                      co_flux_threshpix:nan, $
                      sfr_flux_threshpix:nan, $
                      co_fluxthresh:nan, $
                      sfr_fluxthresh:nan, $
                      co_numthresh:nan, $
                      sfr_numthresh:nan, $
                      co_byflux_im25:nan, $
                      co_byflux_im75:nan, $
                      sfr_byflux_im25:nan, $
                      sfr_byflux_im75:nan, $
                      co_bynum_im25:nan, $
                      co_bynum_im75:nan, $
                      sfr_bynum_im25:nan, $
                      sfr_bynum_im75:nan, $
; OVERLAP PARAMETERS
; RELATIVE TO PIXELS WITH EMISSION                      
                      ofrac_bynum:nan, $
                      sfr_only_bynum:nan, $
                      co_only_bynum:nan, $
                      sfr_all_bynum:nan, $
                      co_all_bynum:nan, $
; RELATIVE TO ALL MAP PIXELS                      
                      no_co_no_sfr_bynum:nan, $
                      co_and_sfr_bynum:nan, $
                      co_or_sfr_bynum:nan, $
                      pie_sfr_thresh1_bynum:nan, $
                      pie_sfr_thresh2_bynum:nan, $
                      pie_co_thresh1_bynum:nan, $
                      pie_co_thresh2_bynum:nan, $
; RELATIVE TO PIXELS WITH EMISSION                      
                      ofrac_byflux:nan, $
                      sfr_only_byflux:nan, $
                      co_only_byflux:nan, $
                      sfr_all_byflux:nan, $
                      co_all_byflux:nan, $
; RELATIVE TO ALL MAP PIXELS                      
                      no_co_no_sfr_byflux:nan, $
                      co_and_sfr_byflux:nan, $
                      co_or_sfr_byflux:nan, $
                      pie_sfr_thresh1_byflux:nan, $
                      pie_sfr_thresh2_byflux:nan, $
                      pie_co_thresh1_byflux:nan, $
                      pie_co_thresh2_byflux:nan, $
; PIECHART PARAMETERS
                      frac_p1_sfr_only_bynum:nan, $
                      frac_p1_co_only_bynum:nan, $
                      frac_p2_sfr_only_bynum:nan, $
                      frac_p2_co_only_bynum:nan, $
                      frac_p1_both_bynum:nan, $
                      frac_p2_both_bynum:nan, $
                      frac_p1_sfr_p2_co_bynum:nan, $
                      frac_p1_co_p2_sfr_bynum:nan, $
                      frac_p1_sfr_only_byflux:nan, $
                      frac_p1_co_only_byflux:nan, $
                      frac_p2_sfr_only_byflux:nan, $
                      frac_p2_co_only_byflux:nan, $
                      frac_p1_both_byflux:nan, $
                      frac_p2_both_byflux:nan, $
                      frac_p1_sfr_p2_co_byflux:nan, $
                      frac_p1_co_p2_sfr_byflux:nan, $
; CDFPARAMETERS
                      ks_cosfr_d:nan, $
                      ks_cosfr_p:nan, $
                      co_ff_p25:nan, $
                      co_ff_p50:nan, $
                      co_ff_p75:nan, $
                      sfr_ff_p25:nan, $
                      sfr_ff_p50:nan, $
                      sfr_ff_p75:nan, $
                      cobysfr_ff_p25:nan, $
                      cobysfr_ff_p50:nan, $
                      cobysfr_ff_p75:nan, $
                      sfrbyco_ff_p25:nan, $
                      sfrbyco_ff_p50:nan, $
                      sfrbyco_ff_p75:nan, $
                      co_np_f25:nan, $
                      co_np_f50:nan, $
                      co_np_f75:nan, $
                      sfr_np_f25:nan, $
                      sfr_np_f50:nan, $
                      sfr_np_f75:nan, $
                      cobysfr_np_f25:nan, $
                      cobysfr_np_f50:nan, $
                      cobysfr_np_f75:nan, $
                      sfrbyco_np_f25:nan, $
                      sfrbyco_np_f50:nan, $
                      sfrbyco_np_f75:nan, $
                      co_ncdf_pwidth:nan, $
                      sfr_ncdf_pwidth:nan, $
                      co_cdf_width:nan, $
                      sfr_cdf_width:nan, $
                      cobysfr_cdf_width:nan, $
                      sfrbyco_cdf_width:nan, $
                    co_ncdf_width:nan, $
                      sfr_ncdf_width:nan, $
                      cobysfr_ncdf_width:nan, $
                      sfrbyco_ncdf_width:nan, $
                     co_cdf_width_norm:nan, $
                      sfr_cdf_width_norm:nan, $
                      cobysfr_cdf_width_norm:nan, $
                      sfrbyco_cdf_width_norm:nan $
                        }

  return, empty_sfco_stats

end
