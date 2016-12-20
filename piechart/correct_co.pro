pro correct_co $
   , in_dir = in_dir $
   , out_dir = out_dir $
   , gals = gals $
   , nostop=nostop    

;+
;
; Put CO onto common astrometric grid and update headers
;
; galaxy parameters obtained from AKL's galbase IDL routines
; https://github.com/akleroy/galbase
;-

  
  @constants.bat
  nan = !values.f_nan

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DEFAULTS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; expects I(CO) data in K.km/s, mom1 & mom2 in km/s, Tpk in K

  use_alpha=4.35
  use_r21=0.7
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PROCESS USER INPUT
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  gals=strupcase(gals)
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; CO
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  print, ""
  print, "Generating CO Maps"
  print, ""


  for i=0,n_elements(gals)-1 do begin

     ; reinitialise defaults (individual galaxies can change these values)
     use_alpha=4.35
     use_r21=0.7

     CASE gals[i] OF
        'NGC3351': BEGIN
           gstr=gal_data('NGC3351')
           mom0file=in_dir+'NGC3351_co21_cube_mom0.fits'
           mom1file=in_dir+'NGC3351_co21_cube_mom1.fits'
           tpkfile=in_dir+'NGC3351_co21_cube_tpeak.fits'
           ;mom2file=in_dir+'ngc3351_co21_cube_mom2.fits'
           out_mom0file=out_dir+'ngc3351_co_mom0.fits'
           out_nh2file=out_dir+'ngc3351_co_msolpc2.fits'
           out_mom1file=out_dir+'ngc3351_co_mom1.fits'
           out_tpkfile=out_dir+'ngc3351_co_tpk.fits'
           END
        'NGC3627': BEGIN
           gstr=gal_data('NGC3627')
           mom0file=in_dir+'NGC3627_co21_cube_mom0.fits'
           mom1file=in_dir+'NGC3627_co21_cube_mom1.fits'
           tpkfile=in_dir+'NGC3627_co21_cube_tpeak.fits'
           ;mom2file=in_dir+'ngc3627_co21_cube_mom2.fits'
           out_mom0file=out_dir+'ngc3627_co_mom0.fits'
           out_nh2file=out_dir+'ngc3627_co_msolpc2.fits'
           out_mom1file=out_dir+'ngc3627_co_mom1.fits'
           out_tpkfile=out_dir+'ngc3627_co_tpk.fits'
           END
        'NGC4535': BEGIN
           gstr=gal_data('NGC4535')
           mom0file=in_dir+'NGC4535_co21_cube_mom0.fits'
           mom1file=in_dir+'NGC4535_co21_cube_mom1.fits'
           tpkfile=in_dir+'NGC4535_co21_cube_tpeak.fits'
           ;mom2file=in_dir+'NGC4535_co21_cube_mom2.fits'
           out_mom0file=out_dir+'ngc4535_co_mom0.fits'
           out_nh2file=out_dir+'ngc4535_co_msolpc2.fits'
           out_mom1file=out_dir+'ngc4535_co_mom1.fits'
           out_tpkfile=out_dir+'ngc4535_co_tpk.fits'
           ;out_mom2file=out_dir+'ngc4535_co_mom2.fits'
           END
        'NGC5068': BEGIN
           gstr=gal_data('NGC5068')
           mom0file=in_dir+'NGC5068_co21_cube_mom0.fits'
           mom1file=in_dir+'NGC5068_co21_cube_mom1.fits'
           tpkfile=in_dir+'NGC5068_co21_cube_tpeak.fits'
           ;mom2file=in_dir+'NGC5068_co21_cube_mom2.fits'
           out_mom0file=out_dir+'ngc5068_co_mom0.fits'
           out_nh2file=out_dir+'ngc5068_co_msolpc2.fits'
           out_mom1file=out_dir+'ngc5068_co_mom1.fits'
           out_tpkfile=out_dir+'ngc5068_co_tpk.fits'
           ;out_mom2file=out_dir+'ngc5068_co_mom2.fits'
           END
        'NGC4254': BEGIN
           gstr=gal_data('NGC4254')
           mom0file=in_dir+'NGC4254_co21_cube_mom0.fits'
           mom1file=in_dir+'NGC4254_co21_cube_mom1.fits'
           tpkfile=in_dir+'NGC4254_co21_cube_tpeak.fits'
           ;mom2file=in_dir+'ngc4254_co21_cube_mom2.fits'
           out_mom0file=out_dir+'ngc4254_co_mom0.fits'
           out_nh2file=out_dir+'ngc4254_co_msolpc2.fits'
           out_mom1file=out_dir+'ngc4254_co_mom1.fits'
           out_tpkfile=out_dir+'ngc4254_co_tpk.fits'
           END
        'NGC4321': BEGIN
           gstr=gal_data('NGC4321')
           mom0file=in_dir+'NGC4321_co21_cube_mom0.fits'
           mom1file=in_dir+'NGC4321_co21_cube_mom1.fits'
           tpkfile=in_dir+'NGC4321_co21_cube_tpeak.fits'
           ;mom2file=in_dir+'ngc4321_co21_cube_mom2.fits'
           out_mom0file=out_dir+'ngc4321_co_mom0.fits'
           out_nh2file=out_dir+'ngc4321_co_msolpc2.fits'
           out_mom1file=out_dir+'ngc4321_co_mom1.fits'
           out_tpkfile=out_dir+'ngc4321_co_tpk.fits'
           END
        'NGC5194': BEGIN
           gstr=gal_data('NGC5194')
           ;mom0file='../orig_data/paws.3as_mom0.blr.fits'
           ;mom1file='../orig_data/paws.3as_mom1.fits'
           ;tpkfile='../orig_data/paws.3as_peak_noblanks.fits'
           mom0file='../orig_data/paws.pdbionly.mom0.blr.fits'
           mom1file='../orig_data/paws.pdbionly.mom1.fits'
           tpkfile='../orig_data/paws.pdbionly.peak_noblanks.fits'
           ;mom2file='../orig_data/paws.3as_mom2.fits'
           out_mom0file=out_dir+'ngc5194_co_mom0.fits'
           out_nh2file=out_dir+'ngc5194_co_msolpc2.fits'
           out_mom1file=out_dir+'ngc5194_co_mom1.fits'
           out_tpkfile=out_dir+'ngc5194_co_tpk.fits'
           use_r21=1.0 ; because it's CO10
        END
        'NGC0628': BEGIN
           gstr=gal_data('NGC628')
           ;mom0file='../orig_data/m74_feather_12m+7m_HERA.K.3p0as.mmom0.blr.fits'
           ;mom1file='../orig_data/m74_feather_12m+7m_HERA.K.3p0as.mom1.fits'
           ;tpkfile='../orig_data/m74_feather_12m+7m_HERA.K.3p0as.tpk.fits'
           ;mom2file='../orig_data/'
           mom0file=in_dir+'NGC0628_co21_cube_mom0.fits'
           mom1file=in_dir+'NGC0628_co21_cube_mom1.fits'
           tpkfile=in_dir+'NGC0628_co21_cube_tpeak.fits'
           out_mom0file=out_dir+'ngc628_co_mom0.fits'
           out_nh2file=out_dir+'ngc628_co_msolpc2.fits'
           out_mom1file=out_dir+'ngc628_co_mom1.fits'
           out_tpkfile=out_dir+'ngc628_co_tpk.fits'
           END
     ENDCASE
     

     m0 = readfits(mom0file, hdr_m0)
     m1_in = readfits(mom1file, hdr_m1_in)
     tpk_in = readfits(tpkfile, hdr_tpk_in)
;  m2_in = readfits(mom2file, hdr_m2_in)
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Put  moment1, tpk and moment 2 on fiducial grid
; we assume the CO mom-0 is 'master' grid
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
     hastrom, m1_in, hdr_m1_in, m1, hdr_m1, hdr_m0, interp=2, missing=!values.f_nan, cubic=-0.5
     hastrom, tpk_in, hdr_tpk_in, tpk, hdr_tpk, hdr_m0, interp=2, missing=!values.f_nan, cubic=-0.5
;  hastrom, m2_in, hdr_m2_in, m2, hdr_m2, hdr_m0, interp=2, missing=!values.f_nan, cubic=-0.5

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; CO
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     ind = where(finite(tpk) eq 0, ct)

     if ct gt 0 then m0[ind] = !values.f_nan
     sxaddpar, hdr_m0, 'DATAMAX', max(m0,/nan)
     sxaddpar, hdr_m0, 'DATAMIN', min(m0,/nan)
     sxaddpar, hdr_m0, 'BUNIT', 'K.km/s'
     writefits, out_mom0file, m0, hdr_m0 
     
     if ct gt 0 then m1[ind] = !values.f_nan
     sxaddpar, hdr_m1, 'DATAMAX', max(m1,/nan)
     sxaddpar, hdr_m1, 'DATAMIN', min(m1,/nan)
     sxaddpar, hdr_m1, 'BUNIT', 'km/s'
     writefits, out_mom1file, m1, hdr_m1 

     if ct gt 0 then tpk[ind] = !values.f_nan
     sxaddpar, hdr_tpk, 'DATAMAX', max(tpk,/nan)
     sxaddpar, hdr_tpk, 'DATAMIN', min(tpk,/nan)
     sxaddpar, hdr_tpk, 'BUNIT', 'K'
     writefits, out_tpkfile, tpk, hdr_tpk 

;     if ct gt 0 then m2[ind] = !values.f_nan
;     sxaddpar, hdr_m2, 'DATAMAX', max(m2,/nan)
;     sxaddpar, hdr_m2, 'DATAMIN', min(m2,/nan)
;     sxaddpar, hdr_m2, 'BUNIT', 'km/s'
;     writefits, out_mom2file, m2, hdr_m2 
     
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Sigma_H2 
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     
     nh2=6.3*(0.7/use_r21)*(use_alpha/4.35)*cos(gstr.incl_deg*!dtor)*m0 ; leroyetal13
     hdr_nh2=hdr_m0
     sxaddpar, hdr_nh2, 'DATAMAX', max(nh2,/nan)
     sxaddpar, hdr_nh2, 'DATAMIN', min(nh2,/nan)
     sxaddpar, hdr_nh2, 'BUNIT', 'Msol/pc2'
     writefits, out_nh2file, nh2, hdr_nh2

  end
  
  if not keyword_set(nostop) then stop
  
end
  
