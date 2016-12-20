pro correct_ha $
   , in_dir = in_dir $
   , out_dir = out_dir $
   , gals = gals $
   , nostop=nostop $
   , nonegatives=nonegatives $
   , skysub=skysub

;+
;
; Put Halpha maps onto common astrometric grid, and do
; simple DIG/HII separation
;
; SFNG maps come in Jy/pixel units
; We use the SINGS delivery document v5 prescription to convert to
; erg/s/cm-2/pixel
; i.e. F[erg s-1 cm-2] = 3.e-5 * (CPS*PHOTFLAM) * FWHM / CW^2
;
; CPS*PHOTFLAM has units JY/PIXEL
; PHOTFLAM keyword is in header of SINGS maps
; FWHM and CW and in Table on page 34 of SINGS delivery document v5
;
;
;  
; Other galaxy parameters obtained from AKL's galbase IDL routines
; https://github.com/akleroy/galbase
;-

  
  @constants.bat

  nan = !values.f_nan
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DEFAULTS THAT ARE THE SAME FOR ALL GALAXIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  c_ms=c/1.e2
  ha_restwav=656.28e-9          ;[m]
  ha_restfreq=c_ms/ha_restwav    ;[Hz]
  sr_2_as2 = 4.2545168e+10
  pc_2_cm=3.086e18
  use_nonegatives=0
  do_skysub=0
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PROCESS USER INPUT
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  gals=strupcase(gals)
  if keyword_set(nonegatives) then use_nonegatives=1
  if keyword_set(skysub) then do_skysub=1
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Ha
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  print, ""
  print, "Generating Halpha Maps"
  print, ""

  for i=0,n_elements(gals)-1 do begin

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DEFAULTS THAT NEED TO BE RE_INITIALISED FOR EACH GALAXY
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     found_res=0
     res_str='TBC by PSF fitting. Assumed.'
     badhdr=0
     do_digseparation=1         ; for now we use super simple method of Hoopes ea 2001
     use_dig_thresh=0.          ; threshold used for DIG/HII region separation mask (need to experiment and adjust in CASE below)
     use_dig_scale_pc=1.e3      ; [pc] smoothing length for median filter that is used to separate DIG 

     CASE gals[i] OF
        'NGC3627': BEGIN
           gstr=gal_data('NGC3627')
           ;hafile=in_dir+'ngc3627_sings_ha_correct.fits' ; in erg/s/cm2/sr
           hafile=in_dir+'ngc3627_HA_dr4_sub.fits' ; in Jy/pixel
           cofile=out_dir+'ngc3627_co_mom0.fits' ; used for re-gridding
           out_hafile=out_dir+'ngc3627_ha'
           out_r25file=out_dir+'ngc3627_r25'
           out_hiifile=out_dir+'ngc3627_hii' ; don't use fits suffix here
           out_digfile=out_dir+'ngc3627_dig'
           do_digseparation=1   ; for now we use super simple method of Hoopes ea 2001
           use_dig_thresh=5.e-6    ; estimated by eye from map_mf
           use_dig_scale_pc=1.e3    ; [pc] smoothing length for median filter that is used to separate DIG
           use_angres=1.9/3600.     ; tbc  (from my memory of KPNO SINGS data)
           use_photflam = 2.0034e-5 ; from HDR
           use_filter = 'KP1564/6618' ; from HDR
           use_zpoint = 3732.       ; from HDR
           END
        'NGC3351': BEGIN
           gstr=gal_data('NGC3351')
           ;hafile=in_dir+'ngc3351_sings_ha_correct.fits' ; in erg/s/cm2/sr
           hafile=in_dir+'ngc3351_HA_dr4_sub.fits' ; in Jy/pixel
           cofile=out_dir+'ngc3351_co_mom0.fits' ; used for re-gridding
           out_hafile=out_dir+'ngc3351_ha'
           out_r25file=out_dir+'ngc3351_r25'
           out_hiifile=out_dir+'ngc3351_hii' ; don't use fits suffix here
           out_digfile=out_dir+'ngc3351_dig'
           do_digseparation=1   ; for now we use super simple method of Hoopes ea 2001
           use_dig_thresh=5.e-6    ; estimated by eye from map_mf
           use_dig_scale_pc=1.e3    ; [pc] smoothing length for median filter that is used to separate DIG
           use_angres=1.9/3600. ; tbc  (from my memory of KPNO SINGS data)
           use_photflam = 1.9979e-5 ; from HDR
           use_filter = 'KP1563/6573' ; from HDR
           use_zpoint = 3733.       ; from HDR
           END
 ;       'NGC4535': BEGIN
 ;          gstr=gal_data('NGC4535')
 ;          hafile='../orig_data/ngc4535Ha_cs_I_Ha_ksb2004.fits' ; does not exist in multilam yet, in ADU
 ;          cofile=out_dir+'ngc4535_co_mom0.fits'            ; used for re-gridding
 ;          out_hafile=out_dir+'ngc4535_ha'
 ;          out_hiifile=out_dir+'ngc4535_hii'
 ;          out_digfile=out_dir+'ngc4535_dig'
 ;          do_digseparation=1   ; for now we use super simple method of Hoopes ea 2001
 ;          use_dig_thresh=0.002    ;  estimated by eye from map_mf (in ADU)
 ;          use_dig_scale_pc=1.e3   ; [pc] smoothing length for median filter that is used to separate DIG
 ;          use_angres=1.9/3600.       ; tbc (from NED)
 ;          END
        'NGC5068': BEGIN
           gstr=gal_data('NGC5068')
           gstr.posang_deg=0.
;           badhdr=1
;           hafile='../orig_data/NGC5068_ha_sub_dr1.fits' ; does not exist in multilam directory yet, in counts?
           hafile=in_dir+'ngc5068_Ha_mhf2006_sub.fits' ; Jy/pixel
           cofile=out_dir+'ngc5068_co_mom0.fits'                 ; used for re-gridding
           out_hafile=out_dir+'ngc5068_ha'
           out_r25file=out_dir+'ngc5068_r25'
          out_hiifile=out_dir+'ngc5068_hii'
           out_digfile=out_dir+'ngc5068_dig'
           do_digseparation=1   ; for now we use super simple method of Hoopes ea 2001
          ; use_dig_thresh=80.    ;  estimated by eye from map_mf (in counts for the LVL map)
           use_dig_thresh=5.e-6    ;  estimated by eye from map_mf 
           use_dig_scale_pc=1.e3 ; [pc] smoothing length for median filter that is used to separate DIG
           use_angres=1.6/3600.    ; tbc (from SUNGG average PSF, Meurer ea 2006)
           ;use_photflam = 2.39471e-17 ; from HDR *** ERG CM-2 A-1 DN-1
           ;use_filter = '6568' ; ***CTIO*** from HDR
           ;use_zpoint = !values.f_nan       ; UNKNOWN from HDR
           use_photflam = 0.00130776 ; from HDR of N628 -- same observing set-up
           use_filter = 'CT6568' ; from HDR of N628 -- same observing set-up
           use_zpoint = 3732.      ; from HDR of N628  -- same observing set-up
           END
           'NGC4254': BEGIN
           gstr=gal_data('NGC4254')
;           hafile=in_dir+'ngc4254_sings_ha_correct.fits' ; in erg/s/cm2/sr
           hafile=in_dir+'ngc4254_HA_dr4_sub.fits' ; in Jy/pixel
           cofile=out_dir+'ngc4254_co_mom0.fits' ; used for re-gridding
           out_hafile=out_dir+'ngc4254_ha'
           out_r25file=out_dir+'ngc4254_r25'
           out_hiifile=out_dir+'ngc4254_hii' ; don't use fits suffix here
           out_digfile=out_dir+'ngc4254_dig'
           do_digseparation=1   ; for now we use super simple method of Hoopes ea 2001
           use_dig_thresh=5.e-6    ; estimated by eye from map_mf
           use_dig_scale_pc=1.e3    ; [pc] smoothing length for median filter that is used to separate DIG
           use_angres=1.9/3600. ; tbc  (from my memory of KPNO SINGS data)
           use_photflam = 2.0034e-5 ; from HDR
           use_filter = 'KP1564/6618' ; from HDR
           use_zpoint = 3732.       ; from HDR
        END
        'NGC4321': BEGIN
           gstr=gal_data('NGC4321')
;           hafile=in_dir+'ngc4321_sings_ha_correct.fits' ; in erg/s/cm2/sr
           hafile=in_dir+'ngc4321_HA_dr4_sub.fits'       ; in Jy/pixel
           cofile=out_dir+'ngc4321_co_mom0.fits'     ; used for re-gridding
           out_hafile=out_dir+'ngc4321_ha'
           out_r25file=out_dir+'ngc4321_r25'
           out_hiifile=out_dir+'ngc4321_hii' ; don't use fits suffix here
           out_digfile=out_dir+'ngc4321_dig'
           do_digseparation=1   ; for now we use super simple method of Hoopes ea 2001
           use_dig_thresh=5.e-6    ; estimated by eye from map_mf
           use_dig_scale_pc=1.e3    ; [pc] smoothing length for median filter that is used to separate DIG
           use_angres=1.9/3600. ; tbc  (from my memory of KPNO SINGS data)
           use_photflam = 1.9979e-5 ; from HDR
           use_filter = 'KP1563/6573' ; from HDR
           use_zpoint = 3733.       ; from HDR
           END
        'NGC5194': BEGIN
           gstr=gal_data('NGC5194')
;           hafile=in_dir+'ngc5194_sings_ha_correct.fits' ; in erg/s/cm2/sr
;           cofile=out_dir+'ngc5194_co_mom0.fits' ; used for re-gridding
;           out_hafile=out_dir+'ngc5194_ha'
;           out_hiifile=out_dir+'ngc5194_hii' ; don't use fits suffix here
;           out_digfile=out_dir+'ngc5194_dig'
;           do_digseparation=1   ; for now we use super simple method of Hoopes ea 2001
;           use_dig_thresh=5.e-6    ; estimated by eye from map_mf
;           use_dig_scale_pc=1.e3    ; [pc] smoothing length for median filter that is used to separate DIG
;           use_angres=1.9/3600. ; tbc  (from my memory of KPNO SINGS data)
           hafile='../orig_data/ngc5194_HA_sub_dr4_jypix.fits'       ; in Jy/pixel
           cofile=out_dir+'ngc5194_co_mom0.fits' ; used for re-gridding
           out_hafile=out_dir+'ngc5194_ha'
           out_r25file=out_dir+'ngc5194_r25'
           out_hiifile=out_dir+'ngc5194_hii' ; don't use fits suffix here
           out_digfile=out_dir+'ngc5194_dig'
           do_digseparation=1   ; for now we use super simple method of Hoopes ea 2001
           use_dig_thresh=5.e-6    ; estimated by eye from map_mf
           use_dig_scale_pc=1.e3    ; [pc] smoothing length for median filter that is used to separate DIG
           use_angres=1.9/3600. ; tbc  (from my memory of KPNO SINGS data)
           use_photflam = 1.9979e-5 ; from HDR 
           use_filter = 'KP1563/6573' ; from HDR
           use_zpoint = 3733.      ;  from HDR
        END
        'NGC0628': BEGIN
           gstr=gal_data('NGC628')
           ;hafile=in_dir+'ngc0628_sings_ha_correct.fits' ; in erg/s/cm2/sr
           hafile=in_dir+'ngc0628_HA_dr4_sub.fits'       ; in Jy/pixel
           cofile=out_dir+'ngc628_co_mom0.fits' ; used for re-gridding
           out_hafile=out_dir+'ngc628_ha'
           out_r25file=out_dir+'ngc628_r25'
           out_hiifile=out_dir+'ngc628_hii' ; don't use fits suffix here
           out_digfile=out_dir+'ngc628_dig'
           do_digseparation=1   ; for now we use super simple method of Hoopes ea 2001
           use_dig_thresh=5.e-6    ; estimated by eye from map_mf
           use_dig_scale_pc=1.e3    ; [pc] smoothing length for median filter that is used to separate DIG
           use_angres=1.9/3600. ; tbc  (from my memory of KPNO SINGS data)
           use_photflam = 0.00130776 ; from HDR 
           use_filter = 'CT6568' ; ***CTIO*** from HDR
           use_zpoint = 3732.      ; UNKNOWN from HDR
           END
     ENDCASE

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Read data
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     ha_in = readfits(hafile, hahdr_in)
     co_in = readfits(cofile, cohdr)
     use_solidang_fact=4.d*!pi*gstr.dist_mpc*gstr.dist_mpc*1.e6*1.e6*pc_2_cm*pc_2_cm
     use_dist_fact=1.*gstr.dist_mpc*1.e6/206265.
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Verify PSF information and propagate to BMIN
; Brent has entered the BMAJ infromation in arcseconds
; so here we also convert to degrees      
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     bmaj=sxpar(hahdr_in,'BMAJ',count=ct)
     if ct gt 0 then begin
        ;print,gals[i]+': Found resolution!'
        ;print,'BMAJ:'+sigfig(bmaj,4)
        use_angres=bmaj
        res_str='Assuming symmetric. Value from PSF-fitting, BGroves'
        found_res=1
     end
     sxaddpar, hahdr_in, 'BMAJ', use_angres/3600., res_str
     sxaddpar, hahdr_in, 'BMIN', use_angres/3600., res_str
     sxaddpar, hahdr_in, 'BPA', 0., 'Assumed.'

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Transform Jy/pixel to erg/s/cm2/sr
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; 1 Jy = 1e-23 erg/s/cm2/Hz
; Jy/pixel --> erg/s/pixel: *1.e-23*solidangle*Halpha frequency
; Jy/pixel --> erg/s/cm2/sr: *1.e-23*Halpha frequency*(# of pix in a sr)
     
; Start by getting appropriate FWHM and CW values
; this information comes from SINGS DDv5 table
     
     CASE use_filter of
        'KP1564/6618': begin
           use_cw=6618. & use_fwhm=74.
        end
        'KP1563/6573': begin
           use_cw=6573. & use_fwhm=67.
        end
        'CT6568': begin
           use_cw=6568. & use_fwhm=19.
        end
        ELSE: begin
           message, 'UNKNOWN FILTER'
        end
     ENDCASE

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Next need to determine initial pixel size so that we can convert to
; pixel-independent surface brightness units. Do not use the header
; modified by cd2astro_header_ah because hastrom prefers the original
; transformation matrix
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     
     cdelt1=sxpar(hahdr_in,'CDELT1',count=ct1)
     cdelt2=sxpar(hahdr_in,'CDELT2',count=ct2)
     if ct1+ct2 ne 2 then begin $
        message,gals[i]+': No CDELT keywords',/info
        message,'Converting CD matrix to cdelt keywords',/info
        hahdr_mod=cd2astro_header_ah(hahdr_in,/verb) ; works ngc3627, 3351, 5068, 4254, 4321, 628
        cdelt1=sxpar(hahdr_mod,'CDELT1',count=ct1)
        cdelt2=sxpar(hahdr_mod,'CDELT2',count=ct2)
        if ct1+ct2 ne 2 then message,'Cannot work out header astrometry'
     end

     pixarea_as2=abs(cdelt1*3600.)*abs(cdelt2*3600.)
     pixarea_fact=sr_2_as2/pixarea_as2

     ha_ergscm2pix_in = ha_in*3.e-5*use_fwhm/(use_cw*use_cw)

     ha_ergscm2sr_in = ha_ergscm2pix_in*pixarea_fact
     sxaddpar, hahdr_in, 'DATAMAX', max(ha_ergscm2sr_in,/nan)
     sxaddpar, hahdr_in, 'DATAMIN', min(ha_ergscm2sr_in,/nan)
     sxaddpar, hahdr_in, 'BUNIT', 'erg/s/cm2/sr'
     sxaddpar, hahdr_in, 'HISTORY', 'This is an Halpha map.'
     sxaddpar, hahdr_in, 'HISTORY', 'Converted to erg/s/cm2/sr using:'  
     sxaddpar, hahdr_in, 'HISTORY', 'F[erg s-1 cm-2]=3.e-5*(CPS*PHOTFLAM)*FWHM/CW^2'  
     writefits,out_hafile+'.ergscm2sr.fullmap.fits',ha_ergscm2sr_in,hahdr_in

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Background subtraction
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
;   MAKE IMAGE of galactocentric radius
    galpos = [gstr.posang_deg, gstr.incl_deg , gstr.ra_deg, gstr.dec_deg]
    make_axes, hahdr_in, raxis=raxis, daxis=daxis, rimg=rimg, dimg=dimg
    deproject, rimg, dimg, galpos, rgrid=rgalmap
    rgalmap /= gstr.r25_deg
    rghdr = hahdr_in
    sxaddpar, rghdr, 'DATAMAX', max(rgalmap,/nan)
    sxaddpar, rghdr, 'DATAMIN', min(rgalmap,/nan)
    sxaddpar, rghdr, 'BUNIT', 'R25'
    writefits,out_r25file+'.fits',rgalmap,rghdr

;   DETERMINE BACKGROUND FROM MEDIAN PIXEL VALUE OUTSIDE R25 
    if do_skysub eq 1 then begin
       sky_idx=where(rgalmap gt 1, skyct)
       if skyct gt 100 then begin
          print,'Using '+strtrim(string(skyct),2)+' pixels to determine sky level.'
          sky_level=median(ha_ergscm2sr_in[sky_idx])
          ha_ergscm2sr_in=ha_ergscm2sr_in-sky_level
          sxaddpar, hahdr_in, 'DATAMAX', max(ha_ergscm2sr_in,/nan)
          sxaddpar, hahdr_in, 'DATAMIN', min(ha_ergscm2sr_in,/nan)
          sxaddpar, hahdr_in, 'HISTORY', 'Removed sky level using pixels beyond R25'  
          sxaddpar, hahdr_in, 'HISTORY', 'Sky level: '+strtrim(sigfig(sky_level,5,/sci),2)+'erg/s/cm2/sr'
          writefits,out_hafile+'.ergscm2sr.fullmap.skysub.fits',ha_ergscm2sr_in,hahdr_in
       end else begin
          print,'Less than 100 pixels to determine sky level. Skipping sky subtraction'
       end
    end

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Put Halpha in erg/s/cm2/sr onto CO fiducial grid
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
     hastrom, ha_ergscm2sr_in, hahdr_in, ha, hdr_ha, cohdr, interp=2, missing=!values.f_nan, cubic=-0.5

     ind = where(finite(ha) eq 0, ct)
     if ct gt 0 then ha[ind] = !values.f_nan
;     sxaddpar, hdr_ha, 'HISTORY', 'This is an Halpha map.'
;     sxaddpar, hdr_ha, 'HISTORY', 'Converted to erg/s/cm2/sr using:'  
 ;    sxaddpar, hdr_ha, 'HISTORY', 'F[erg s-1 cm-2]=3.e-5*(CPS*PHOTFLAM)*FWHM/CW^2'  
     sxaddpar, hdr_ha, 'HISTORY', 'Regridded to match CO map astrometry.'  
     sxaddpar, hdr_ha, 'DATAMAX', max(ha,/nan)
     sxaddpar, hdr_ha, 'DATAMIN', min(ha,/nan)
     sxaddpar, hdr_ha, 'BUNIT', 'erg/s/cm2/sr'
     writefits, out_hafile+'.ergscm2sr.fits', ha, hdr_ha

     ind = where(finite(ha) eq 1 and ha lt 0., ct)
     hapos=ha
     hdr_hapos=hdr_ha
     if ct gt 0 then hapos[ind] = 0.
     sxaddpar, hdr_hapos, 'HISTORY', 'Replace negative pixels with zeroes'  
     sxaddpar, hdr_hapos, 'DATAMAX', max(hapos,/nan)
     sxaddpar, hdr_hapos, 'DATAMIN', min(hapos,/nan)
     sxaddpar, hdr_hapos, 'BUNIT', 'erg/s/cm2/sr'
     writefits, out_hafile+'.ergscm2sr.zeroed.fits', hapos, hdr_hapos

     if keyword_set(use_nonegatives) then begin
        ha=hapos & hdr_ha=hdr_hapos
        print,'Using zeroed map to calculate SFRs'
     end

;     print,'GAL: '+gals[i]+' BMAJ: '+strtrim(string(use_angres),2)+ $
;           ', Spatial: '+strtrim(sigfig(use_dist_fact*use_angres,5),2)+' pc'+ $
;           ', Sky level: '+strtrim(sigfig(sky_level,5,/sci),2)+' erg/s/cm2/sr'

     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Transform erg/s/cm2/sr to SFR recipes as per Leroy ea 12
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; re estimate pixel areas for regridded map
     cdelt1=sxpar(hdr_ha,'CDELT1',count=ct1)
     cdelt2=sxpar(hdr_ha,'CDELT2',count=ct2)
     if ct1+ct2 ne 2 then begin $
        message,gals[i]+': No CDELT keywords',/info
        message,'Converting CD matrix to cdelt keywords',/info
        hahdr_mod=cd2astro_header_ah(hdr_ha,/verb) ; works ngc3627, 3351, 5068, 4254, 4321, 628
        cdelt1=sxpar(hahdr_mod,'CDELT1',count=ct1)
        cdelt2=sxpar(hahdr_mod,'CDELT2',count=ct2)
        if ct1+ct2 ne 2 then message,'Cannot work out header astrometry'
     end

     pixarea_as2=abs(cdelt1*3600.)*abs(cdelt2*3600.)
     pixarea_fact=sr_2_as2/pixarea_as2
     pixarea_pc2=pixarea_as2*use_dist_fact*use_dist_fact
     pix2pc2_fact=1./pixarea_pc2
     
     ha_ergscm2pix = ha/pixarea_fact
     ha_ergspix = ha_ergscm2pix*use_solidang_fact

     hdr_ha_ergscm2pix=hdr_ha
     sxaddpar, hdr_ha_ergscm2pix, 'BUNIT', 'erg/s/cm2/pixel'
     sxaddpar,hdr_ha_ergscm2pix,'DATAMIN',min(ha_ergscm2pix,/nan)
     sxaddpar,hdr_ha_ergscm2pix,'DATAMAX',max(ha_ergscm2pix,/nan)
     writefits, out_hafile+'.ergs.fits', ha_ergscm2pix, hdr_ha_ergscm2pix

     hdr_ha_ergs=hdr_ha
     sxaddpar, hdr_ha_ergs, 'BUNIT', 'erg/s/pixel'
     sxaddpar,hdr_ha_ergs,'DATAMIN',min(ha_ergspix,/nan)
     sxaddpar,hdr_ha_ergs,'DATAMAX',max(ha_ergspix,/nan)
     writefits, out_hafile+'.ergs.fits', ha_ergspix, hdr_ha_ergs
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Transform to SFR/pixel estimates as per Leroy ea 12
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;SFR[Msol/yr]=5.3e-42 * Lha [erg/s] -- equation 3
;SFR[Msol/yr/kpc2]=634 * Iha [erg/s/cm2/sr] -- equation 4 (nb. units
;                                              in paper are wrong)

     hdr_sfrpix=hdr_ha_ergs
     sfr=5.3e-42*ha_ergspix
     sxaddpar, hdr_sfrpix, 'BUNIT', 'Msol/yr/pixel'
     sxaddpar,hdr_sfrpix,'DATAMIN',min(sfr,/nan)
     sxaddpar,hdr_sfrpix,'DATAMAX',max(sfr,/nan)
     sxaddpar,hdr_sfrpix,'HISTORY','Converted to SFR via Eqn3 Leroyea12'
     sxaddpar,hdr_sfrpix,'HISTORY','SFR[Msol/yr]=5.3e-42*LHa[erg/s]'
     writefits, out_hafile+'.sfr_msolyrpix.fits', sfr,hdr_sfrpix

;     hdr_sigsfr=hdr_sfrpix
;     sigsfr=sfr*pix2pc2_fact*1.e6
;     sxaddpar, hdr_sigsfr, 'BUNIT', 'Msol/yr/kpc2'
;     sxaddpar,hdr_sigsfr,'DATAMIN',min(sigsfr,/nan)
;     sxaddpar,hdr_sigsfr,'DATAMAX',max(sigsfr,/nan)
;     sxaddpar,hdr_sigsfr,'HISTORY','Converted to SigmaSFR'
;     sxaddpar,hdr_sigsfr,'HISTORY','Using pixel size [pc2]:'+sigfig(pixarea_pc2,3,/sci)
;     writefits, out_hafile+'.sfr_msolyrkpc2.fits', sigsfr,hdr_sigsfr


;16/11/2016 -- checked that the following gives the same answers as above
; when using ha map [i.e. in erg/s/cm2/sr units]
     hdr_sigsfr=hdr_ha
     sigsfr=ha*634.
     sxaddpar, hdr_sigsfr, 'BUNIT', 'Msol/yr/kpc2'
     sxaddpar,hdr_sigsfr,'DATAMIN',min(sigsfr,/nan)
     sxaddpar,hdr_sigsfr,'DATAMAX',max(sigsfr,/nan)
     sxaddpar,hdr_sigsfr,'HISTORY','Converted to SigmaSFR via Eqn4 Leroyea12'
     sxaddpar,hdr_sigsfr,'HISTORY','SFR[Msol/yr/kpc2]=634*IHa[erg/s/cm2/sr]'
     writefits, out_hafile+'.sfr_msolyrkpc2.fits', sigsfr,hdr_sigsfr
          
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; SIMPLE DIG/HII SEPARATION 
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  if keyword_set(do_digseparation) then begin

     dig_scale_ang=use_dig_scale_pc*206265.*1.e-6/gstr.dist_mpc

     ; do convolution on original pixel grid
     conv_with_gauss, $
        data=ha_ergscm2sr_in, $
        hdr=hahdr_in, $
        target_beam=dig_scale_ang*[1.,1.], $
        out_data=map_tmp, $
        out_hdr=hdr_tmp, $
        /quiet

     hastrom, map_tmp, hdr_tmp, map_smo, hdr_smo, cohdr, interp=2, missing=!values.f_nan, cubic=-0.5
     
     ind = where(finite(map_smo) eq 0, ct)
     if ct gt 0 then map_smo[ind] = !values.f_nan

     map_mf=ha-map_smo
     
     map_dig=ha
     map_nodig=ha
     hdr_dig=hdr_ha
     hdr_nodig=hdr_ha

     hii_idx=where(map_mf gt use_dig_thresh, hict, complement=dig_idx, ncomp=loct)
     
     if hict gt 0 then begin
        map_dig[hii_idx] = nan
        sxaddpar, hdr_dig, 'HISTORY', 'This is an estimate of the Halpha DIG.'
        sxaddpar, hdr_dig, 'HISTORY', 'Following Hoopes ea 2001 method.'  
        sxaddpar, hdr_dig, 'HISTORY', 'DIG threshold applied: '+sigfig(use_dig_thresh,3,/sci)  
        sxaddpar, hdr_dig, 'DATAMAX', max(map_dig,/nan)
        sxaddpar, hdr_dig, 'DATAMIN', min(map_dig,/nan)
        sxaddpar, hdr_dig, 'BUNIT', 'erg/s/cm2/sr'
        writefits,out_digfile+'.fits',map_dig,hdr_dig
        map_dig[hii_idx] = 0.
        sxaddpar, hdr_dig, 'HISTORY', 'HII regions set to zero'
        sxaddpar, hdr_dig, 'DATAMIN', min(map_dig,/nan)
        writefits,out_digfile+'.zeroed.fits',map_dig,hdr_dig

     end

     if loct gt 0 then begin
        map_nodig[dig_idx] = nan
        sxaddpar, hdr_nodig, 'HISTORY', 'This is an estimate of the Halpha after DIG subtraction.'
        sxaddpar, hdr_nodig, 'HISTORY', 'Following Hoopes ea 2001 method.'  
        sxaddpar, hdr_nodig, 'HISTORY', 'DIG threshold adopted: '+sigfig(use_dig_thresh,3,/sci)  
        sxaddpar, hdr_nodig, 'DATAMAX', max(map_nodig,/nan)
        sxaddpar, hdr_nodig, 'DATAMIN', min(map_nodig,/nan)
        sxaddpar, hdr_nodig, 'BUNIT', 'erg/s/cm2/sr'
        writefits,out_hiifile+'.fits',map_nodig,hdr_nodig
        map_nodig[dig_idx] = 0.
        sxaddpar, hdr_nodig, 'HISTORY', 'DIG set to zero'
        sxaddpar, hdr_nodig, 'DATAMIN', min(map_nodig,/nan)
        writefits,out_hiifile+'.zeroed.fits',map_nodig,hdr_nodig

        hdr_sigsfr=hdr_nodig
        sigsfr_nodig=map_nodig*634.
        sxaddpar, hdr_sigsfr, 'BUNIT', 'Msol/yr/kpc2'
        sxaddpar,hdr_sigsfr,'DATAMIN',min(sigsfr_nodig,/nan)
        sxaddpar,hdr_sigsfr,'DATAMAX',max(sigsfr_nodig,/nan)
        sxaddpar,hdr_sigsfr,'HISTORY','Converted to SFR via Eqn4 Leroyea12'
        sxaddpar,hdr_sigsfr,'HISTORY','SFR[Msol/yr/kpc2]=634*IHa[erg/s/cm2/sr (no dig)]'
        writefits, out_hiifile+'.sfr_msolyrkpc2.fits', sigsfr_nodig,hdr_sigsfr

     end
     
  end

  skip:
end
  
  

  if not keyword_set(nostop) then stop
  
end
