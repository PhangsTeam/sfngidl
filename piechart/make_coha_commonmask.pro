pro make_coha_commonmask $
   , data_dir = data_dir $
   , in_mask_dir = in_mask_dir $
   , out_mask_dir = out_mask_dir $
   , gals = gals $
   , dig = dig $
   , h2 = h2 $
   , sfr = sfr $
   , nostop=nostop $
   , applymask=applymask 
   

;+
;
; Having generated CO and Halpha maps on the CO grid,
;  determine common field-of-view and region that we will use for analysis
;
; For CO, we use Tpk maps since they still have the FoV information (mom0 have blank
; pixels set to 0 in Adam's delivered maps)
;
; galaxy parameters obtained from AKL's galbase IDL routines
; https://github.com/akleroy/galbase
;-

  
  @constants.bat
  nan = !values.f_nan

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DEFAULTS DIRECTORIES & GALAXIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  use_in_mask_dir = '/Users/anniehughes/Work/SFNG/piechart/analysis/multilam_v0p1/'
  use_out_mask_dir = '../masks/'
  use_data_dir='../good_data/'
  use_gals=['NGC3627','NGC4535','NGC5068']
  do_applymask=0
  do_dig =0
  do_sfr =0
  do_h2 =0
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PROCESS USER INPUT
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if keyword_set(data_dir) then use_data_dir=data_dir
  if keyword_set(in_mask_dir) then use_in_mask_dir=in_mask_dir
  if keyword_set(out_mask_dir) then use_out_mask_dir=out_mask_dir
  if keyword_set(gals) then use_gals=gals
  if keyword_set(applymask) then do_applymask=1
  if keyword_set(dig) then do_dig=1
  if keyword_set(h2) then do_h2=1
  if keyword_set(sfr) then do_sfr=1

  
  gals=strupcase(use_gals)
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Masks
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  print, ""
  print, "Generating Common Mask"
  print, ""


  for i=0,n_elements(gals)-1 do begin
     
     CASE gals[i] OF
        'NGC3627': BEGIN
           gstr=gal_data('NGC3627')
           comskfile=use_data_dir+'ngc3627_co_tpk.fits'
           cofile=use_data_dir+'ngc3627_co_mom0.fits'
           h2file=use_data_dir+'ngc3627_co_msolpc2.fits'
           hafile=use_data_dir+'ngc3627_ha.ergscm2sr.fits'
           sfrfile=use_data_dir+'ngc3627_ha.sfr_msolyrkpc2.fits'
           hiifile=use_data_dir+'ngc3627_hii.zeroed.fits'
           hiisfrfile=use_data_dir+'ngc3627_hii.sfr_msolyrkpc2.fits'
           digfile=use_data_dir+'ngc3627_dig.zeroed.fits'
           in_mskfile=use_in_mask_dir+'NGC3627.1.final_mask.fits'
           out_mskfile=use_out_mask_dir+'ngc3627_mask.fits'
           out_cofile=cofile
           out_h2file=h2file
           out_hafile=hafile
           out_hiifile=hiifile
           out_digfile=digfile
           out_sfrfile=sfrfile
           out_hiisfrfile=hiisfrfile
           END
        'NGC3351': BEGIN
           gstr=gal_data('NGC3351')
           comskfile=use_data_dir+'ngc3351_co_tpk.fits'
           cofile=use_data_dir+'ngc3351_co_mom0.fits'
           h2file=use_data_dir+'ngc3351_co_msolpc2.fits'
           hafile=use_data_dir+'ngc3351_ha.ergscm2sr.fits'
           sfrfile=use_data_dir+'ngc3351_ha.sfr_msolyrkpc2.fits'
           hiifile=use_data_dir+'ngc3351_hii.zeroed.fits'
           hiisfrfile=use_data_dir+'ngc3351_hii.sfr_msolyrkpc2.fits'
           digfile=use_data_dir+'ngc3351_dig.zeroed.fits'
           in_mskfile=use_in_mask_dir+'NGC3351.1.final_mask.fits'
           out_mskfile=use_out_mask_dir+'ngc3351_mask.fits'
           out_cofile=cofile
           out_h2file=h2file
           out_hafile=hafile
           out_hiifile=hiifile
           out_digfile=digfile
           out_sfrfile=sfrfile
           out_hiisfrfile=hiisfrfile
           END
        'NGC4535': BEGIN
           gstr=gal_data('NGC4535')
           comskfile=use_data_dir+'ngc4535_co_tpk.fits'
           cofile=use_data_dir+'ngc4535_co_mom0.fits'
           h2file=use_data_dir+'ngc4535_co_msolpc2.fits'
           hafile=use_data_dir+'ngc4535_ha.ergscm2sr.fits'
           sfrfile=use_data_dir+'ngc4535_ha.sfr_msolyrkpc2.fits'
           hiifile=use_data_dir+'ngc4535_hii.zeroed.fits'
           hiisfrfile=use_data_dir+'ngc4535_hii.sfr_msolyrkpc2.fits'
           digfile=use_data_dir+'ngc4535_dig.zeroed.fits'
           in_mskfile=use_in_mask_dir+'NGC4535.1.final_mask.fits'
           out_mskfile=use_out_mask_dir+'NGC4535_mask.fits'
           out_cofile=cofile
           out_hafile=hafile
           out_hiifile=hiifile
           out_digfile=digfile
           out_sfrfile=sfrfile
           out_hiisfrfile=hiisfrfile
           END
        'NGC5068': BEGIN
           gstr=gal_data('NGC5068')
           in_mskfile='../orig_data/NGC5068.1.final_mask.fits' ; does not exist in multilam directory yet!
           out_mskfile=use_out_mask_dir+'ngc5068_mask.fits'
           comskfile=use_data_dir+'ngc5068_co_tpk.fits'
           cofile=use_data_dir+'ngc5068_co_tpk.fits'
           h2file=use_data_dir+'ngc5068_co_msolpc2.fits'
           hafile=use_data_dir+'ngc5068_ha.ergscm2sr.fits'
           sfrfile=use_data_dir+'ngc5068_ha.sfr_msolyrkpc2.fits'
           hiifile=use_data_dir+'ngc5068_hii.zeroed.fits'
           hiisfrfile=use_data_dir+'ngc5068_hii.sfr_msolyrkpc2.fits'
           digfile=use_data_dir+'ngc5068_dig.zeroed.fits'
           out_cofile=cofile
           out_h2file=h2file
           out_hafile=hafile
           out_hiifile=hiifile
           out_digfile=digfile
           out_sfrfile=sfrfile
           out_hiisfrfile=hiisfrfile
        END
        'NGC4254': BEGIN
           gstr=gal_data('NGC4254')
           comskfile=use_data_dir+'ngc4254_co_tpk.fits'
           cofile=use_data_dir+'ngc4254_co_mom0.fits'
           h2file=use_data_dir+'ngc4254_co_msolpc2.fits'
           hafile=use_data_dir+'ngc4254_ha.ergscm2sr.fits'
           sfrfile=use_data_dir+'ngc4254_ha.sfr_msolyrkpc2.fits'
           hiifile=use_data_dir+'ngc4254_hii.zeroed.fits'
           digfile=use_data_dir+'ngc4254_dig.zeroed.fits'
           hiisfrfile=use_data_dir+'ngc4254_hii.sfr_msolyrkpc2.fits'
           in_mskfile=use_in_mask_dir+'NGC4254.1.final_mask.fits'
           out_mskfile=use_out_mask_dir+'ngc4254_mask.fits'
           out_cofile=cofile
           out_h2file=h2file
           out_hafile=hafile
           out_hiifile=hiifile
           out_digfile=digfile
           out_sfrfile=sfrfile
           out_hiisfrfile=hiisfrfile
           END
        'NGC4321': BEGIN
           gstr=gal_data('NGC4321')
           comskfile=use_data_dir+'ngc4321_co_tpk.fits'
           cofile=use_data_dir+'ngc4321_co_mom0.fits'
           h2file=use_data_dir+'ngc4321_co_msolpc2.fits'
           hafile=use_data_dir+'ngc4321_ha.ergscm2sr.fits'
           sfrfile=use_data_dir+'ngc4321_ha.sfr_msolyrkpc2.fits'
           hiifile=use_data_dir+'ngc4321_hii.zeroed.fits'
           hiisfrfile=use_data_dir+'ngc4321_hii.sfr_msolyrkpc2.fits'
           digfile=use_data_dir+'ngc4321_dig.zeroed.fits'
           in_mskfile=use_in_mask_dir+'NGC4321.1.final_mask.fits'
           out_mskfile=use_out_mask_dir+'ngc4321_mask.fits'
           out_cofile=cofile
           out_h2file=h2file
           out_hafile=hafile
           out_hiifile=hiifile
           out_digfile=digfile
           out_sfrfile=sfrfile
           out_hiisfrfile=hiisfrfile
           END
        'NGC5194': BEGIN
           gstr=gal_data('NGC5194')
           comskfile=use_data_dir+'ngc5194_co_tpk.fits'
           cofile=use_data_dir+'ngc5194_co_mom0.fits'
           h2file=use_data_dir+'ngc5194_co_msolpc2.fits'
           hafile=use_data_dir+'ngc5194_ha.ergscm2sr.fits'
           sfrfile=use_data_dir+'ngc5194_ha.sfr_msolyrkpc2.fits'
           hiifile=use_data_dir+'ngc5194_hii.zeroed.fits'
           hiisfrfile=use_data_dir+'ngc5194_hii.sfr_msolyrkpc2.fits'
           digfile=use_data_dir+'ngc5194_dig.zeroed.fits'
           in_mskfile=use_in_mask_dir+'NGC5194.1.final_mask.fits'
           out_mskfile=use_out_mask_dir+'ngc5194_mask.fits'
           out_cofile=cofile
           out_h2file=h2file
           out_hafile=hafile
           out_hiifile=hiifile
           out_digfile=digfile
           out_sfrfile=sfrfile
           out_hiisfrfile=hiisfrfile
           END
        'NGC0628': BEGIN
           gstr=gal_data('NGC628')
           comskfile=use_data_dir+'ngc628_co_tpk.fits'
           cofile=use_data_dir+'ngc628_co_mom0.fits'
           h2file=use_data_dir+'ngc628_co_msolpc2.fits'
           hafile=use_data_dir+'ngc628_ha.ergscm2sr.fits'
           sfrfile=use_data_dir+'ngc628_ha.sfr_msolyrkpc2.fits'
           hiifile=use_data_dir+'ngc628_hii.zeroed.fits'
           hiisfrfile=use_data_dir+'ngc628_hii.sfr_msolyrkpc2.fits'
           digfile=use_data_dir+'ngc628_dig.zeroed.fits'
           in_mskfile=use_in_mask_dir+'NGC0628.1.final_mask.fits'
           out_mskfile=use_out_mask_dir+'ngc628_mask.fits'
           out_cofile=cofile
           out_h2file=h2file
           out_hafile=hafile
           out_hiifile=hiifile
           out_digfile=digfile
            out_sfrfile=sfrfile
           out_hiisfrfile=hiisfrfile
          END
     ENDCASE
     
     mskco = readfits(comskfile, hdr_comsk)
     co = readfits(cofile, hdr_co)
     ha = readfits(hafile, hdr_ha)
     msk_in = readfits(in_mskfile, hdr_msk_in) & msk_in=float(msk_in)

     if keyword_set(do_dig) then begin
        hii = readfits(hiifile, hdr_hii)
        dig = readfits(digfile, hdr_dig)
        if keyword_set(do_sfr) then hiisfr = readfits(hiisfrfile, hdr_hiisfr)
     endif

     if keyword_set(do_h2) then $
        h2 = readfits(h2file, hdr_h2)

     if keyword_set(do_sfr) then $
        sfr = readfits(sfrfile, hdr_sfr)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Convert S4G integer msk to 1/0s
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     
     goodidx=where(msk_in eq 0,goodct, comp=badidx, ncomp=badct)
     if goodct ge 1 then msk_in[goodidx]=1
     if badct ge 1 then msk_in[badidx]=0
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Put Star Mask on fiducial grid
; we assume the CO mom-0 is 'master' grid
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
     hastrom, msk_in, hdr_msk_in, msk, hdr_msk, hdr_co, interp=0, missing=!values.f_nan
     sxaddpar, hdr_msk, 'DATAMAX', max(msk,/nan)
     sxaddpar, hdr_msk, 'DATAMIN', min(msk,/nan)
     sxaddpar, hdr_msk, 'HISTORY', 'Regridded to CO pixel-grid and astrometry'
     sxaddpar, hdr_msk, 'HISTORY', 'Converted to 1/0s from S4G integer mask'
     sxaddpar, hdr_msk, 'HISTORY', '1: Good pixel, 0: bad pixel'
     sxaddpar, hdr_msk, 'BUNIT', 'Mask'
     writefits, out_mskfile, msk, hdr_msk 

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; MAKE COMMON MASK
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     badidx = where(msk eq 0 or finite(co) eq 0 or $
                    finite(mskco) eq 0 or finite(ha) eq 0, ct,ncomp=gct,comp=goodidx)

     if ct gt 0 then begin

        msk[badidx]=0

        sxaddpar, hdr_msk, 'DATAMAX', max(msk,/nan)
        sxaddpar, hdr_msk, 'DATAMIN', min(msk,/nan)
        sxaddpar, hdr_msk, 'HISTORY', 'Undefined pixels in CO and Halpha maps set to BAD'
        sxaddpar, hdr_msk, 'BUNIT', 'Mask'
        writefits, out_mskfile, msk, hdr_msk 
        message,'Wrote '+out_mskfile,/info
        
        if keyword_set(do_applymask) then begin

           print,'Applying mask to CO and Halpha maps'
           
           co[badidx] = !values.f_nan
           ha[badidx] = !values.f_nan

           sxaddpar, hdr_co, 'DATAMAX', max(co,/nan)
           sxaddpar, hdr_co, 'DATAMIN', min(co,/nan)
           sxaddpar, hdr_co, 'HISTORY', 'Masked using: '+out_mskfile
           writefits, out_cofile, co, hdr_co 

           sxaddpar, hdr_ha, 'DATAMAX', max(ha,/nan)
           sxaddpar, hdr_ha, 'DATAMIN', min(ha,/nan)
           sxaddpar, hdr_ha, 'HISTORY', 'Masked using: '+out_mskfile
           writefits, out_hafile, ha, hdr_ha 

           if keyword_set(do_dig) then begin

              print,'Applying mask to HII-only and DIG maps'

              hii[badidx] = !values.f_nan
              dig[badidx] = !values.f_nan
              
              sxaddpar, hdr_hii, 'DATAMAX', max(hii,/nan)
              sxaddpar, hdr_hii, 'DATAMIN', min(hii,/nan)
              sxaddpar, hdr_hii, 'HISTORY', 'Masked using: '+out_mskfile
              writefits, out_hiifile, hii, hdr_hii 

              sxaddpar, hdr_dig, 'DATAMAX', max(dig,/nan)
              sxaddpar, hdr_dig, 'DATAMIN', min(dig,/nan)
              sxaddpar, hdr_dig, 'HISTORY', 'Masked using: '+out_mskfile
              writefits, out_digfile, dig, hdr_dig 

              if keyword_set(do_sfr) then begin
              
                 print,'Applying mask to Sigma_SFRHII maps (HIi regions only)'
                 
                 hiisfr[badidx] = !values.f_nan
                 
                 sxaddpar, hdr_hiisfr, 'DATAMAX', max(hiisfr,/nan)
                 sxaddpar, hdr_hiisfr, 'DATAMIN', min(hiisfr,/nan)
                 sxaddpar, hdr_hiisfr, 'HISTORY', 'Masked using: '+out_mskfile
                 writefits, out_hiisfrfile, hiisfr, hdr_hiisfr 
                 
              end

           end

           if keyword_set(do_h2) then begin
              
              print,'Applying mask to Sigma_H2 maps'
              
              h2[badidx] = !values.f_nan
              
              sxaddpar, hdr_h2, 'DATAMAX', max(h2,/nan)
              sxaddpar, hdr_h2, 'DATAMIN', min(h2,/nan)
              sxaddpar, hdr_h2, 'HISTORY', 'Masked using: '+out_mskfile
              writefits, out_h2file, h2, hdr_h2 
              
           end

           if keyword_set(do_sfr) then begin
              
              print,'Applying mask to Sigma_SFR maps (total Halpha)'
              
              sfr[badidx] = !values.f_nan
              
              sxaddpar, hdr_sfr, 'DATAMAX', max(sfr,/nan)
              sxaddpar, hdr_sfr, 'DATAMIN', min(sfr,/nan)
              sxaddpar, hdr_sfr, 'HISTORY', 'Masked using: '+out_mskfile
              writefits, out_sfrfile, sfr, hdr_sfr 
              
           end

 
        end

     end

  end
  
  if not keyword_set(nostop) then stop

  the_end:
end
  
