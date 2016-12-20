pro convolve_for_sfco, gals=gals $
                       , target_res_pc=target_res_pc $
                       , co=co $
                       , ha=ha $
                       , hii=hii $
                       , dig=dig $
                       , h2=h2 $
                       , sfr=sfr $
                       , nostop=nostop $
                       , data_dir=data_dir 

;+
;
; Convolve CO and SFR tracer data to a set of matched physical resolution
;
; galaxy parameters obtained from AKL's galbase IDL routines
; https://github.com/akleroy/galbase
;-

; ==============================
; DEFAULT PARAMS & GALAXIES
; ==============================

  use_target_res_pc=[30.,40.,50.,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600.]
  use_gals=['NGC0628','NGC3627','NGC3351','NGC5068','NGC4254','NGC4321','NGC5194']
  do_ha = 0 
  do_hii = 0 
  do_dig = 0 
  do_co = 0
  do_h2 = 0
  do_sfr = 0
  use_data_dir='../good_data/'

; ==============================
; PROCESS USER INPUTS
; ==============================

  if keyword_set(data_dir) then use_data_dir=data_dir
  if keyword_set(gals) then use_gals=gals
  if keyword_set(ha) then do_ha=1
  if keyword_set(co) then do_co=1
  if keyword_set(h2) then do_h2=1
  if keyword_set(sfr) then do_sfr=1
  if keyword_set(hii) then do_hii=1
  if keyword_set(dig) then do_dig=1
  if keyword_set(target_res_pc) then use_target_res_pc=target_res_pc

    ; enforce final back slash and make sure it exists
  use_data_dir=file_search(use_data_dir,/mark,/full)

  if use_data_dir eq ''  then $
     message,'Problem with data directory'

  use_gals=strupcase(use_gals)
  use_gals_lowcase=strlowcase(use_gals)


; ==============================
; LOOP THROUGH GALAXIES
; ==============================

  for i=0,n_elements(use_gals)-1 do begin

     gstr=gal_data(use_gals[i])
     galdist=gstr.dist_mpc*1.e6 ; distance in parsecs
     scale_3as=3.*galdist/206265.
     use_target_res_as=use_target_res_pc*206265./galdist
     use_target_res_as=round(use_target_res_as*100000)/100000. ; only care up to second decimal place
     
     CASE use_gals[i] OF
        'NGC3627': BEGIN
           co_infile='ngc3627_co_mom0.fits'
           co_outname='ngc3627_co_mom0'
           h2_infile='ngc3627_co_msolpc2.fits'
           h2_outname='ngc3627_co_msolpc2'
           ha_infile='ngc3627_ha.ergscm2sr.fits'
           ha_outname='ngc3627_ha'
           hii_infile='ngc3627_hii.zeroed.fits'
           hii_outname='ngc3627_hii'
           dig_infile='ngc3627_dig.zeroed.fits'
           dig_outname='ngc3627_dig'
           sfr_infile='ngc3627_ha.sfr_msolyrkpc2.fits'
           sfr_outname='ngc3627_ha_sigsfr'
           sfrhii_infile='ngc3627_hii.sfr_msolyrkpc2.fits'
           sfrhii_outname='ngc3627_hii_sigsfr'
        END
        'NGC3351': BEGIN
           co_infile='ngc3351_co_mom0.fits'
           co_outname='ngc3351_co_mom0'
           h2_infile='ngc3351_co_msolpc2.fits'
           h2_outname='ngc3351_co_msolpc2'
           ha_infile='ngc3351_ha.ergscm2sr.fits'
           ha_outname='ngc3351_ha'
           hii_infile='ngc3351_hii.zeroed.fits'
           hii_outname='ngc3351_hii'
           dig_infile='ngc3351_dig.zeroed.fits'
           dig_outname='ngc3351_dig'
           sfr_infile='ngc3351_ha.sfr_msolyrkpc2.fits'
           sfr_outname='ngc3351_ha_sigsfr'
           sfrhii_infile='ngc3351_hii.sfr_msolyrkpc2.fits'
           sfrhii_outname='ngc3351_hii_sigsfr'
        END
       'NGC4535': BEGIN
           co_infile='ngc4535_co_mom0.fits'
           co_outname='ngc4535_co_mom0'
           h2_infile='ngc4535_co_msolpc2.fits'
           h2_outname='ngc4535_co_msolpc2'
           ha_infile='ngc4535_ha.ergscm2sr.fits'
           ha_outname='ngc4535_ha'
           hii_infile='ngc4535_hii.zeroed.fits'
           hii_outname='ngc4535_hii'
           dig_infile='ngc4535_dig.zeroed.fits'
           dig_outname='ngc4535_dig'
           sfr_infile='ngc4535_ha.sfr_msolyrkpc2.fits'
           sfr_outname='ngc4535_ha_sigsfr'
           sfrhii_infile='ngc4535_hii.sfr_msolyrkpc2.fits'
           sfrhii_outname='ngc4535_hii_sigsfr'
        END
       'NGC5068': BEGIN
           co_infile='ngc5068_co_mom0.fits'
           co_outname='ngc5068_co_mom0'
           h2_infile='ngc5068_co_msolpc2.fits'
           h2_outname='ngc5068_co_msolpc2'
           ha_infile='ngc5068_ha.ergscm2sr.fits'
           ha_outname='ngc5068_ha'
           hii_infile='ngc5068_hii.zeroed.fits'
           hii_outname='ngc5068_hii'
          dig_infile='ngc5068_dig.zeroed.fits'
           dig_outname='ngc5068_dig'
           sfr_infile='ngc5068_ha.sfr_msolyrkpc2.fits'
           sfr_outname='ngc5068_ha_sigsfr'
           sfrhii_infile='ngc5068_hii.sfr_msolyrkpc2.fits'
           sfrhii_outname='ngc5068_hii_sigsfr'
        END
        'NGC4254': BEGIN
           co_infile='ngc4254_co_mom0.fits'
           co_outname='ngc4254_co_mom0'
           h2_infile='ngc4254_co_msolpc2.fits'
           h2_outname='ngc4254_co_msolpc2'
           ha_infile='ngc4254_ha.ergscm2sr.fits'
           ha_outname='ngc4254_ha'
           hii_infile='ngc4254_hii.zeroed.fits'
           hii_outname='ngc4254_hii'
           dig_infile='ngc4254_dig.zeroed.fits'
           dig_outname='ngc4254_dig'
           sfr_infile='ngc4254_ha.sfr_msolyrkpc2.fits'
           sfr_outname='ngc4254_ha_sigsfr'
           sfrhii_infile='ngc4254_hii.sfr_msolyrkpc2.fits'
           sfrhii_outname='ngc4254_hii_sigsfr'
        END
        'NGC4321': BEGIN
           co_infile='ngc4321_co_mom0.fits'
           co_outname='ngc4321_co_mom0'
           h2_infile='ngc4321_co_msolpc2.fits'
           h2_outname='ngc4321_co_msolpc2'
           ha_infile='ngc4321_ha.ergscm2sr.fits'
           ha_outname='ngc4321_ha'
           hii_infile='ngc4321_hii.zeroed.fits'
           hii_outname='ngc4321_hii'
          dig_infile='ngc4321_dig.zeroed.fits'
           dig_outname='ngc4321_dig'
           sfr_infile='ngc4321_ha.sfr_msolyrkpc2.fits'
           sfr_outname='ngc4321_ha_sigsfr'
           sfrhii_infile='ngc4321_hii.sfr_msolyrkpc2.fits'
           sfrhii_outname='ngc4321_hii_sigsfr'
        END
        'NGC5194': BEGIN
           co_infile='ngc5194_co_mom0.fits'
           co_outname='ngc5194_co_mom0'
           h2_infile='ngc5194_co_msolpc2.fits'
           h2_outname='ngc5194_co_msolpc2'
           ha_infile='ngc5194_ha.ergscm2sr.fits'
           ha_outname='ngc5194_ha'
           hii_infile='ngc5194_hii.zeroed.fits'
           hii_outname='ngc5194_hii'
           dig_infile='ngc5194_dig.zeroed.fits'
           dig_outname='ngc5194_dig'
           sfr_infile='ngc5194_ha.sfr_msolyrkpc2.fits'
           sfr_outname='ngc5194_ha_sigsfr'
           sfrhii_infile='ngc5194_hii.sfr_msolyrkpc2.fits'
           sfrhii_outname='ngc5194_hii_sigsfr'
        END
        'NGC0628': BEGIN
           co_infile='ngc628_co_mom0.fits'
           co_outname='ngc628_co_mom0'
           h2_infile='ngc628_co_msolpc2.fits'
           h2_outname='ngc628_co_msolpc2'
           ha_infile='ngc628_ha.ergscm2sr.fits'
           ha_outname='ngc628_ha'
           hii_infile='ngc628_hii.zeroed.fits'
           hii_outname='ngc628_hii'
           dig_infile='ngc628_dig.zeroed.fits'
           dig_outname='ngc628_dig'
           sfr_infile='ngc628_ha.sfr_msolyrkpc2.fits'
           sfr_outname='ngc628_ha_sigsfr'
           sfrhii_infile='ngc628_hii.sfr_msolyrkpc2.fits'
           sfrhii_outname='ngc628_hii_sigsfr'
        END
     ENDCASE
     
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; THE CO
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
     if do_co eq 1 then begin

        co=readfits(use_data_dir+'/'+co_infile,cohdr)
        use_co_lim=sxpar(cohdr,'BMAJ')*3600.

; first make a map matching 3" 
        if use_co_lim le 3. then begin
           tscstr=strtrim(string(fix(scale_3as)),2)
           outname=co_outname+'_'+tscstr+'pc.fits'
           conv_with_gauss $
              , data = use_data_dir+'/'+co_infile $
              , out_file = use_data_dir+'/'+outname $
              , target_beam=3.*[1.*1.]

           if do_h2 eq 1 then begin
              outname=h2_outname+'_'+tscstr+'pc.fits'
              conv_with_gauss $
                 , data = use_data_dir+'/'+h2_infile $
                 , out_file = use_data_dir+'/'+outname $
                 , target_beam=3.*[1.*1.]
           end

        end
        
        
        target_beams=use_target_res_as
        target_scales=use_target_res_pc
        goodbm=where(target_beams gt use_co_lim, goodct)

        if goodct ge 1 then begin
           target_beams=target_beams[goodbm]
           target_scales=target_scales[goodbm]

           print,'Creating CO maps for '+use_gals[i]+' at these scales: '+strtrim(string(fix(target_scales)),2)
           wait,2.
           
           for j=0,goodct-1 do begin

              tbstr=strtrim(string(round(target_beams[j])),2)
              tscstr=strtrim(string(round(target_scales[j])),2)
              ;outname=co_outname+'_'+tbstr+'as.fits'
              outname=co_outname+'_'+tscstr+'pc.fits'
              
              conv_with_gauss $
                 , data = use_data_dir+'/'+co_infile $
                 , out_file = use_data_dir+'/'+outname $
                 , target_beam=target_beams[j]*[1.*1.]


              data=readfits(use_data_dir+'/'+outname,hdr)
              sxaddpar,hdr,'DATAMAX',max(data,/nan)
              sxaddpar,hdr,'DATAMIN',min(data,/nan)
              writefits,use_data_dir+'/'+outname,data,hdr
              
              co_infile=outname ; smooth incrementally

;  H2 maps
              if do_h2 eq 1 then begin
                 print,'Also creating smoothed H2 map...'

;                 outname=h2_outname+'_'+tbstr+'as.fits'
                 outname=h2_outname+'_'+tscstr+'pc.fits'
              
                 conv_with_gauss $
                    , data = use_data_dir+'/'+h2_infile $
                    , out_file = use_data_dir+'/'+outname $
                    , target_beam=target_beams[j]*[1.*1.]
                 
                 data=readfits(use_data_dir+'/'+outname,hdr)
                 sxaddpar,hdr,'DATAMAX',max(data,/nan)
                 sxaddpar,hdr,'DATAMIN',min(data,/nan)
                 writefits,use_data_dir+'/'+outname,data,hdr
                 
                 h2_infile=outname ; smooth incrementally

              end

           end
        end else begin
           print,'No good target beams for CO data found'
        end
  
     end

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; THE Halpha
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

     if do_ha eq 1 then begin
        
        ha=readfits(use_data_dir+'/'+ha_infile,hahdr)
        use_ha_lim=sxpar(hahdr,'BMAJ')*3600.

        message,'Found beam '+strtrim(string(use_ha_lim),2)+' for '+use_gals[i],/info
        wait,2.

        ; first make maps matching 3" 
        if use_ha_lim le 3. then begin
           tscstr=strtrim(string(fix(scale_3as)),2)
           outname=ha_outname+'_'+tscstr+'pc.fits'
           conv_with_gauss $
              , data = use_data_dir+'/'+ha_infile $
              , out_file = use_data_dir+'/'+outname $
              , target_beam=3.*[1.*1.]

           if do_sfr eq 1 then begin
              outname=sfr_outname+'_'+tscstr+'pc.fits'
              conv_with_gauss $
                 , data = use_data_dir+'/'+sfr_infile $
                 , out_file = use_data_dir+'/'+outname $
                 , target_beam=3.*[1.*1.]
           end
           
           if do_hii eq 1 then begin
              outname=hii_outname+'_'+tscstr+'pc.fits'
              conv_with_gauss $
                 , data = use_data_dir+'/'+hii_infile $
                 , out_file = use_data_dir+'/'+outname $
                 , target_beam=3.*[1.*1.]
              
              if do_sfr eq 1 then begin
                 outname=sfrhii_outname+'_'+tscstr+'pc.fits'
                 conv_with_gauss $
                    , data = use_data_dir+'/'+sfrhii_infile $
                    , out_file = use_data_dir+'/'+outname $
                    , target_beam=3.*[1.*1.]
              end
           end
              
           if do_dig eq 1 then begin
              outname=dig_outname+'_'+tscstr+'pc.fits'
              conv_with_gauss $
                 , data = use_data_dir+'/'+dig_infile $
                 , out_file = use_data_dir+'/'+outname $
                 , target_beam=3.*[1.*1.]
           end
        end
        
        target_beams=use_target_res_as
        target_scales=use_target_res_pc
        goodbm=where(target_beams gt use_ha_lim, goodct)

        if goodct ge 1 then begin
           target_beams=target_beams[goodbm]
           target_scales=target_scales[goodbm]
           
           print,'Creating Halpha maps for '+use_gals[i]+' at these scales: '+strtrim(string(fix(target_scales)),2)
           wait,2.

           for j=0,goodct-1 do begin

              tbstr=strtrim(string(round(target_beams[j])),2)
              tscstr=strtrim(string(round(target_scales[j])),2)

; All Halpha
;              outname=ha_outname+'_'+tbstr+'as.fits'
              outname=ha_outname+'_'+tscstr+'pc.fits'

              conv_with_gauss $
                 , data = use_data_dir+'/'+ha_infile $
                 , out_file = use_data_dir+'/'+outname $
                 , target_beam=target_beams[j]*[1.*1.]

              data=readfits(use_data_dir+'/'+outname,hdr)
              sxaddpar,hdr,'DATAMAX',max(data,/nan)
              sxaddpar,hdr,'DATAMIN',min(data,/nan)
              writefits,use_data_dir+'/'+outname,data,hdr
 
              ha_infile=outname ; smooth incrementally

; SFR from total Halpha
              if do_sfr eq 1 then begin
                 print,'Also creating smoothed SFR (total Halpha) map...'

;                 outname=sfr_outname+'_'+tbstr+'as.fits'
                 outname=sfr_outname+'_'+tscstr+'pc.fits'
              
                 conv_with_gauss $
                    , data = use_data_dir+'/'+sfr_infile $
                    , out_file = use_data_dir+'/'+outname $
                    , target_beam=target_beams[j]*[1.*1.]
                 
                 data=readfits(use_data_dir+'/'+outname,hdr)
                 sxaddpar,hdr,'DATAMAX',max(data,/nan)
                 sxaddpar,hdr,'DATAMIN',min(data,/nan)
                 writefits,use_data_dir+'/'+outname,data,hdr
                 
                 sfr_infile=outname ; smooth incrementally

              end


              
; Halpha from HII regions
              if do_hii eq 1 then begin
                 print,'Also creating smoothed HII map...'

;                 outname=hii_outname+'_'+tbstr+'as.fits'
                 outname=hii_outname+'_'+tscstr+'pc.fits'
              
                 conv_with_gauss $
                    , data = use_data_dir+'/'+hii_infile $
                    , out_file = use_data_dir+'/'+outname $
                    , target_beam=target_beams[j]*[1.*1.]
                 
                 data=readfits(use_data_dir+'/'+outname,hdr)
                 sxaddpar,hdr,'DATAMAX',max(data,/nan)
                 sxaddpar,hdr,'DATAMIN',min(data,/nan)
                 writefits,use_data_dir+'/'+outname,data,hdr
                 
                 hii_infile=outname ; smooth incrementally

                 if do_sfr eq 1 then begin
                    
                    print,'Also creating smoothed SFR (HII regions only) map...'
                    outname=sfrhii_outname+'_'+tscstr+'pc.fits'
              
                    conv_with_gauss $
                       , data = use_data_dir+'/'+sfrhii_infile $
                       , out_file = use_data_dir+'/'+outname $
                       , target_beam=target_beams[j]*[1.*1.]
                    
                    data=readfits(use_data_dir+'/'+outname,hdr)
                    sxaddpar,hdr,'DATAMAX',max(data,/nan)
                    sxaddpar,hdr,'DATAMIN',min(data,/nan)
                    writefits,use_data_dir+'/'+outname,data,hdr
                    
                    sfrhii_infile=outname ; smooth incrementally
                    
                 end
              end
              

; Halpha from DIG regions
              if do_dig eq 1 then begin
                 print,'Also creating smoothed DIG map...'

                 ;outname=dig_outname+'_'+tbstr+'as.fits'
                 outname=dig_outname+'_'+tscstr+'pc.fits'

                 conv_with_gauss $
                    , data = use_data_dir+'/'+dig_infile $
                    , out_file = use_data_dir+'/'+outname $
                    , target_beam=target_beams[j]*[1.*1.]
                 
                 data=readfits(use_data_dir+'/'+outname,hdr)
                 sxaddpar,hdr,'DATAMAX',max(data,/nan)
                 sxaddpar,hdr,'DATAMIN',min(data,/nan)
                 writefits,use_data_dir+'/'+outname,data,hdr
                 
                 dig_infile=outname ; smooth incrementally

              end

              
           end
           
        end else begin
           print,'No good target beams for Halpha data found'
        end
        
     end

  end

  if not keyword_set(nostop) then stop

  the_end:
end
