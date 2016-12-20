pro make_galaxy_gallery,nostop=nostop



  
; ==============================
; DEFAULT PARAMS & GALAXIES
; ==============================

  use_gals=['NGC0628','NGC3627','NGC3351','NGC5068','NGC4254','NGC4321','NGC5194']
  ; best common spatial resolution for these galaxies is 300pc
  do_ha = 0
  do_hii = 0
  do_dig = 0 
  do_co = 0
  do_h2 = 1
  do_sfr = 1
  do_sfrhii = 1
  do_mask = 0
  use_data_dir='../good_data/'
  use_mask_dir='../masks/'
;  use_plot_dir='../plots/gallery_nativeres_naturalstretch/'
;  use_plot_dir='../plots/gallery_nativeres_fixedstretch/'
;  use_plot_dir='../plots/gallery_300pc_naturalstretch/'
  use_plot_dir='../plots/gallery_300pc_fixedstretch/'
  use_fixed_stretch=1 ; 0--> show [-1 to 85%], 1--> fixed stretch depending on tracer
  use_h2_thresh=5.
  use_sfr_thresh=5.e-3

  h2_fixed_imrange=[-5.,50.]     ; msol/pc2
  sfr_fixed_imrange=[-0.0001,0.05]     ; msol/yr/kpc2
  

  use_bestres=0                 ; 0 --> use best common resolution - 300pc, 1 --> use best native resolution (varies)
  use_mapct=39
  show_cts=1

  pdp_define_la_common
  loadct,39 & fgcolor=0
  win=0L
  firstplot=1

; ==============================
; PROCESS USER INPUTS
; ==============================

  if keyword_set(data_dir) then use_data_dir=data_dir
  if keyword_set(gals) then use_gals=gals
  if keyword_set(ha) then do_ha=1
  if keyword_set(co) then do_co=1
  if keyword_set(h2) then do_h2=1
  if keyword_set(sfr) then do_sfr=1
  if keyword_set(sfrhii) then do_sfrhii=1
  if keyword_set(hii) then do_hii=1
  if keyword_set(dig) then do_dig=1
  if keyword_set(target_res_pc) then use_target_res_pc=target_res_pc

    ; enforce final back slash and make sure it exists
  use_data_dir=file_search(use_data_dir,/mark,/full)

  if use_data_dir eq ''  then $
     message,'Problem with data directory'

  use_gals=strupcase(use_gals)
  use_gals_lowcase=strlowcase(use_gals)

  if use_bestres eq 1 then begin
     use_resstr='_3as'
     use_sfrstr='.sfr_msolyrkpc2'
  end else begin
     use_resstr='_300pc'
     use_sfrstr='_sigsfr_300pc'
  end
  
; ==============================
; LOOP THROUGH GALAXIES
; ==============================

  for i=0,n_elements(use_gals)-1 do begin

     print,'Processing: '+use_gals[i]
     
     CASE use_gals[i] OF
        'NGC3627': BEGIN
           co_infile='ngc3627_co_mom0'+use_resstr+'.fits'
           co_outname='ngc3627_co_mom0'
           h2_infile='ngc3627_co_msolpc2'+use_resstr+'.fits'
           h2_outname='ngc3627_co_msolpc2'
           ha_infile='ngc3627_ha.ergscm2sr.fits'
           ha_outname='ngc3627_ha'
           hii_infile='ngc3627_hii.zeroed.fits'
           hii_outname='ngc3627_hii'
           dig_infile='ngc3627_dig.zeroed.fits'
           dig_outname='ngc3627_dig'
           sfr_infile='ngc3627_ha'+use_sfrstr+'.fits'
           sfr_outname='ngc3627_ha_sigsfr'
           sfrhii_infile='ngc3627_hii'+use_sfrstr+'.fits'
           sfrhii_outname='ngc3627_hii_sigsfr'
           maskfile='ngc3627_mask.fits'
        END
        'NGC3351': BEGIN
           co_infile='ngc3351_co_mom0'+use_resstr+'.fits'
           co_outname='ngc3351_co_mom0'
           h2_infile='ngc3351_co_msolpc2'+use_resstr+'.fits'
           h2_outname='ngc3351_co_msolpc2'
           ha_infile='ngc3351_ha.ergscm2sr.fits'
           ha_outname='ngc3351_ha'
           hii_infile='ngc3351_hii.zeroed.fits'
           hii_outname='ngc3351_hii'
           dig_infile='ngc3351_dig.zeroed.fits'
           dig_outname='ngc3351_dig'
           sfr_infile='ngc3351_ha'+use_sfrstr+'.fits'
           sfr_outname='ngc3351_ha_sigsfr'
           sfrhii_infile='ngc3351_hii'+use_sfrstr+'.fits'
           sfrhii_outname='ngc3351_hii_sigsfr'
           maskfile='ngc3351_mask.fits'
        END
       'NGC4535': BEGIN
           co_infile='ngc4535_co_mom0'+use_resstr+'.fits'
           co_outname='ngc4535_co_mom0'
           h2_infile='ngc4535_co_msolpc2'+use_resstr+'.fits'
           h2_outname='ngc4535_co_msolpc2'
           ha_infile='ngc4535_ha.ergscm2sr.fits'
           ha_outname='ngc4535_ha'
           hii_infile='ngc4535_hii.zeroed.fits'
           hii_outname='ngc4535_hii'
           dig_infile='ngc4535_dig.zeroed.fits'
           dig_outname='ngc4535_dig'
           sfr_infile='ngc4535_ha'+use_sfrstr+'.fits'
           sfr_outname='ngc4535_ha_sigsfr'
           sfrhii_infile='ngc4535_hii'+use_sfrstr+'.fits'
           sfrhii_outname='ngc4535_hii_sigsfr'
           maskfile='ngc4535_mask.fits'
        END
       'NGC5068': BEGIN
           co_infile='ngc5068_co_mom0'+use_resstr+'.fits'
           co_outname='ngc5068_co_mom0'
           h2_infile='ngc5068_co_msolpc2'+use_resstr+'.fits'
           h2_outname='ngc5068_co_msolpc2'
           ha_infile='ngc5068_ha.ergscm2sr.fits'
           ha_outname='ngc5068_ha'
           hii_infile='ngc5068_hii.zeroed.fits'
           hii_outname='ngc5068_hii'
           dig_infile='ngc5068_dig.zeroed.fits'
           dig_outname='ngc5068_dig'
           sfr_infile='ngc5068_ha'+use_sfrstr+'.fits'
           sfr_outname='ngc5068_ha_sigsfr'
           sfrhii_infile='ngc5068_hii'+use_sfrstr+'.fits'
           sfrhii_outname='ngc5068_hii_sigsfr'
           maskfile='ngc5068_mask.fits'
        END
        'NGC4254': BEGIN
           co_infile='ngc4254_co_mom0'+use_resstr+'.fits'
           co_outname='ngc4254_co_mom0'
           h2_infile='ngc4254_co_msolpc2'+use_resstr+'.fits'
           h2_outname='ngc4254_co_msolpc2'
           ha_infile='ngc4254_ha.ergscm2sr.fits'
           ha_outname='ngc4254_ha'
           hii_infile='ngc4254_hii.zeroed.fits'
           hii_outname='ngc4254_hii'
           dig_infile='ngc4254_dig.zeroed.fits'
           dig_outname='ngc4254_dig'
           sfr_infile='ngc4254_ha'+use_sfrstr+'.fits'
           sfr_outname='ngc4254_ha_sigsfr'
           sfrhii_infile='ngc4254_hii'+use_sfrstr+'.fits'
           sfrhii_outname='ngc4254_hii_sigsfr'
           maskfile='ngc4254_mask.fits'
        END
        'NGC4321': BEGIN
           co_infile='ngc4321_co_mom0'+use_resstr+'.fits'
           co_outname='ngc4321_co_mom0'
           h2_infile='ngc4321_co_msolpc2'+use_resstr+'.fits'
           h2_outname='ngc4321_co_msolpc2'
           ha_infile='ngc4321_ha.ergscm2sr.fits'
           ha_outname='ngc4321_ha'
           hii_infile='ngc4321_hii.zeroed.fits'
           hii_outname='ngc4321_hii'
           dig_infile='ngc4321_dig.zeroed.fits'
           dig_outname='ngc4321_dig'
           sfr_infile='ngc4321_ha'+use_sfrstr+'.fits'
           sfr_outname='ngc4321_ha_sigsfr'
           sfrhii_infile='ngc4321_hii'+use_sfrstr+'.fits'
           sfrhii_outname='ngc4321_hii_sigsfr'
           maskfile='ngc4321_mask.fits'
        END
        'NGC5194': BEGIN
           co_infile='ngc5194_co_mom0'+use_resstr+'.fits'
           co_outname='ngc5194_co_mom0'
           h2_infile='ngc5194_co_msolpc2'+use_resstr+'.fits'
           h2_outname='ngc5194_co_msolpc2'
           ha_infile='ngc5194_ha.ergscm2sr.fits'
           ha_outname='ngc5194_ha'
           hii_infile='ngc5194_hii.zeroed.fits'
           hii_outname='ngc5194_hii'
           dig_infile='ngc5194_dig.zeroed.fits'
           dig_outname='ngc5194_dig'
           sfr_infile='ngc5194_ha'+use_sfrstr+'.fits'
           sfr_outname='ngc5194_ha_sigsfr'
           sfrhii_infile='ngc5194_hii'+use_sfrstr+'.fits'
           sfrhii_outname='ngc5194_hii_sigsfr'
           maskfile='ngc5194_mask.fits'
        END
        'NGC0628': BEGIN
           co_infile='ngc628_co_mom0'+use_resstr+'.fits'
           co_outname='ngc628_co_mom0'
           h2_infile='ngc628_co_msolpc2'+use_resstr+'.fits'
           h2_outname='ngc628_co_msolpc2'
           ha_infile='ngc628_ha.ergscm2sr.fits'
           ha_outname='ngc628_ha'
           hii_infile='ngc628_hii.zeroed.fits'
           hii_outname='ngc628_hii'
           dig_infile='ngc628_dig.zeroed.fits'
           dig_outname='ngc628_dig'
           sfr_infile='ngc628_ha'+use_sfrstr+'.fits'
           sfr_outname='ngc628_ha_sigsfr'
           sfrhii_infile='ngc628_hii'+use_sfrstr+'.fits'
           sfrhii_outname='ngc628_hii_sigsfr'
           maskfile='ngc628_mask.fits'
        END
     ENDCASE

     obp=[1.12,0,1.17,1]
     mask=readfits(use_mask_dir+maskfile,maskhdr)

     ; contours of mask
     sz=size(mask,/dim)
     ctim=fltarr(sz[0],sz[1],1)
     ctim[*,*,0]=mask
     cts_maskonly=create_contour_st(ctim,lev1=1.)
     cts_maskonly.(0).color=255
     cts_maskonly.(0).thick=1
     cts_maskonly.(0).linestyle=1


     if do_co gt 0 then begin

        co=readfits(use_data_dir+co_infile,cohdr)
        
        badpix=where(finite(co) eq 0, bct)
        if bct gt 0 then co[badpix]=!indef
        
        window,win,xsize=800,ysize=800 & !p.position=[0.1,0.1,0.8,0.8] & win=win+1
        tit=use_gals[i]+' CO Moment-0'
        image_cont20,co,cohdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=[-1.,percentile(co,0.15)],bar_tit='[K.km/s]',levels=cts

        ps_file=use_plot_dir+co_outname+'.ps'
        png_file=use_plot_dir+co_outname+'.png'
        image_cont20,co,cohdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=[-1.,percentile(co,0.15)],bar_tit='[K.km/s]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        if firstplot eq 1 then $
           image_cont20,co,cohdr,/square,/silent,tit=tit,off_bar=obp $
                        ,imrange=[-1.,percentile(co,0.15)],bar_tit='[K.km/s]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        firstplot=0
        cleanplot
        
     end



     if do_h2 gt 0 then begin

        h2=readfits(use_data_dir+h2_infile,h2hdr)
        
        badpix=where(finite(h2) eq 0, bct)
        if bct gt 0 then h2[badpix]=!indef
        
        ; redefine contours of mask and threshold
        sz=size(mask,/dim)
        ctims=fltarr(sz[0],sz[1],2)
        ctims[*,*,0]=mask
        ctims[*,*,1]=h2
        cts=create_contour_st(ctims,lev1=1.,lev2=use_h2_thresh)
        cts.(0).color=255
        cts.(0).linestyle=1
        cts.(0).thick=1
        cts.(1).color=255
        cts.(1).thick=1
        
        window,win,xsize=800,ysize=800 & !p.position=[0.1,0.1,0.8,0.8] & win=win+1
        tit=use_gals[i]+' Sigma H2'
        imrange=[-5.,percentile(h2,0.15)]
        if use_fixed_stretch eq 1 then imrange=h2_fixed_imrange
        
        if show_cts gt 0 then $
           image_cont20,h2,h2hdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=imrange,bar_tit='[Msol/pc2]',levels=cts,/nologo
        if show_cts eq 0 then $
           image_cont20,h2,h2hdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=imrange,bar_tit='[Msol/pc2]',/nologo,levels=cts_maskonly


        
        ps_file=use_plot_dir+h2_outname+'.ps'
        png_file=use_plot_dir+h2_outname+'.png'

        write_png,png_file,TVRD(/TRUE)
        
        image_cont20,h2,h2hdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=[-1.,percentile(h2,0.15)],bar_tit='[Msol/pc2]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        if firstplot eq 1 then $
           image_cont20,h2,h2hdr,/square,/silent,tit=tit,off_bar=obp $
                        ,imrange=[-1.,percentile(h2,0.15)],bar_tit='[Msol/pc2]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        firstplot=0
        cleanplot
        
     end


     
     if do_ha gt 0 then begin

        ha=readfits(use_data_dir+ha_infile,hahdr)

        ; image_cont20 needs CDELT1/2 keywords!
        cdelt1=sxpar(hahdr,'CDELT1',count=ct1)
        cdelt2=sxpar(hahdr,'CDELT2',count=ct2)
        if ct1+ct2 ne 2 then begin
           hahdr=cd2astro_header_ah(hahdr,/verb) ; works ngc3627, 3351, 5068, 4254, 4321, 628
           cdelt1=sxpar(hahdr,'CDELT1',count=ct1)
           cdelt2=sxpar(hahdr,'CDELT2',count=ct2)
           if cdelt1 gt 0 then sxaddpar,hahdr,'CDELT1',-1.*cdelt1
           if cdelt2 lt 0 then sxaddpar,hahdr,'CDELT2',-1.*cdelt2
        end
        
        badpix=where(finite(ha) eq 0, bct)
        if bct gt 0 then ha[badpix]=!indef

        window,win,xsize=800,ysize=800 & !p.position=[0.1,0.1,0.8,0.8]  & win=win+1
        tit=use_gals[i]+' Halpha'
        image_cont20,ha,hahdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=[-1.e-5,percentile(ha,0.15)],bar_tit='[erg/s/cm2/sr]',levels=cts

        ps_file=use_plot_dir+ha_outname+'.ps'
        png_file=use_plot_dir+ha_outname+'.png'
        image_cont20,ha,hahdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=[-1.e-5,percentile(ha,0.15)],bar_tit='[erg/s/cm2/sr]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        if firstplot eq 1 then $
           image_cont20,ha,hahdr,/square,/silent,tit=tit,off_bar=obp $
                        ,imrange=[-1.e-5,percentile(ha,0.15)],bar_tit='[erg/s/cm2/sr]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        firstplot=0
        cleanplot

     end


     if do_hii gt 0 then begin

        hii=readfits(use_data_dir+hii_infile,hiihdr)

        ; image_cont20 needs CDELT1/2 keywords!
        cdelt1=sxpar(hiihdr,'CDELT1',count=ct1)
        cdelt2=sxpar(hiihdr,'CDELT2',count=ct2)
        if ct1+ct2 ne 2 then begin
           hiihdr=cd2astro_header_ah(hiihdr,/verb) ; works ngc3627, 3351, 5068, 4254, 4321, 628
           cdelt1=sxpar(hiihdr,'CDELT1',count=ct1)
           cdelt2=sxpar(hiihdr,'CDELT2',count=ct2)
           if cdelt1 gt 0 then sxaddpar,hiihdr,'CDELT1',-1.*cdelt1
           if cdelt2 lt 0 then sxaddpar,hiihdr,'CDELT2',-1.*cdelt2
        end
        
        badpix=where(finite(hii) eq 0, bct)
        if bct gt 0 then hii[badpix]=!indef

        window,win,xsize=800,ysize=800 & !p.position=[0.1,0.1,0.8,0.8] & win=win+1
        tit=use_gals[i]+' Halpha (HII regions only)'
        image_cont20,hii,hiihdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=[-1.e-5,percentile(hii,0.15)],bar_tit='[erg/s/cm2/sr]',levels=cts

        ps_file=use_plot_dir+hii_outname+'.ps'
        png_file=use_plot_dir+hii_outname+'.png'
        image_cont20,hii,hiihdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=[-1.e-5,percentile(hii,0.15)],bar_tit='[erg/s/cm2/sr]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        if firstplot eq 1 then $
           image_cont20,hii,hiihdr,/square,/silent,tit=tit,off_bar=obp $
                        ,imrange=[-1.e-5,percentile(hii,0.15)],bar_tit='[erg/s/cm2/sr]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        firstplot=0
        cleanplot

     end
 
     
     if do_sfr gt 0 then begin

        sfr=readfits(use_data_dir+sfr_infile,sfrhdr)

        ; image_cont20 needs CDELT1/2 keywords!
        cdelt1=sxpar(sfrhdr,'CDELT1',count=ct1)
        cdelt2=sxpar(sfrhdr,'CDELT2',count=ct2)
        if ct1+ct2 ne 2 then begin
           sfrhdr=cd2astro_header_ah(sfrhdr,/verb) ; works ngc3627, 3351, 5068, 4254, 4321, 628
           cdelt1=sxpar(sfrhdr,'CDELT1',count=ct1)
           cdelt2=sxpar(sfrhdr,'CDELT2',count=ct2)
           if cdelt1 gt 0 then sxaddpar,sfrhdr,'CDELT1',-1.*cdelt1
           if cdelt2 lt 0 then sxaddpar,sfrhdr,'CDELT2',-1.*cdelt2
        end
        
        badpix=where(finite(sfr) eq 0, bct)
        if bct gt 0 then sfr[badpix]=!indef

                                ; redefine contours of mask and threshold
        sz=size(mask,/dim)
        ctims=fltarr(sz[0],sz[1],2)
        ctims[*,*,0]=mask
        ctims[*,*,1]=sfr
        cts=create_contour_st(ctims,lev1=1.,lev2=use_sfr_thresh)
        cts.(0).color=255
        cts.(0).thick=1
        cts.(0).linestyle=1
        cts.(1).color=255
        cts.(1).thick=1

        
        window,win,xsize=800,ysize=800 & !p.position=[0.1,0.1,0.8,0.8] & win=win+1
        tit=use_gals[i]+' SFR (from all Halpha)'
        imrange=[-5.e-3,percentile(sfr,0.15)]
        if use_fixed_stretch eq 1 then imrange=sfr_fixed_imrange
        
        if show_cts gt 0 then $
           image_cont20,sfr,sfrhdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=imrange,bar_tit='[Msol/yr/kpc2]',levels=cts,/nologo
        if show_cts eq 0 then $
           image_cont20,sfr,sfrhdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=imrange,bar_tit='[Msol/yr/kpc2]',/nologo,levels=cts_maskonly

        ps_file=use_plot_dir+sfr_outname+'.ps'
        png_file=use_plot_dir+sfr_outname+'.png'

        write_png,png_file,TVRD(/TRUE)

        image_cont20,sfr,sfrhdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=[-5.e-3,percentile(sfr,0.15)],bar_tit='[Msol/yr/kpc2]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        if firstplot eq 1 then $
           image_cont20,sfr,sfrhdr,/square,/silent,tit=tit,off_bar=obp $
                        ,imrange=[-5.e-3,percentile(sfr,0.15)],bar_tit='[Msol/yr/kpc2]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        firstplot=0
        cleanplot

     end

     if do_sfrhii gt 0 then begin

        sfrhii=readfits(use_data_dir+sfrhii_infile,sfrhiihdr)

        ; image_cont20 needs CDELT1/2 keywords!
        cdelt1=sxpar(sfrhiihdr,'CDELT1',count=ct1)
        cdelt2=sxpar(sfrhiihdr,'CDELT2',count=ct2)
        if ct1+ct2 ne 2 then begin
           sfrhiihdr=cd2astro_header_ah(sfrhiihdr,/verb) ; works ngc3627, 3351, 5068, 4254, 4321, 628
           cdelt1=sxpar(sfrhiihdr,'CDELT1',count=ct1)
           cdelt2=sxpar(sfrhiihdr,'CDELT2',count=ct2)
           if cdelt1 gt 0 then sxaddpar,sfrhiihdr,'CDELT1',-1.*cdelt1
           if cdelt2 lt 0 then sxaddpar,sfrhiihdr,'CDELT2',-1.*cdelt2
        end
        
        badpix=where(finite(sfrhii) eq 0, bct)
        if bct gt 0 then sfrhii[badpix]=!indef

       ; redefine contours of mask and threshold
        sz=size(mask,/dim)
        ctims=fltarr(sz[0],sz[1],2)
        ctims[*,*,0]=mask
        ctims[*,*,1]=sfrhii
        cts=create_contour_st(ctims,lev1=1.,lev2=use_sfr_thresh)
        cts.(0).color=255
        cts.(0).linestyle=1
        cts.(0).thick=1
        cts.(1).color=255
        cts.(1).thick=1

        window,win,xsize=800,ysize=800 & !p.position=[0.1,0.1,0.8,0.8] & win=win+1
        tit=use_gals[i]+' SFR (from HII regions only)'
        imrange=[-5.e-3,percentile(sfrhii,0.15)]
        if use_fixed_stretch eq 1 then imrange=sfr_fixed_imrange

        if show_cts gt 0 then $
           image_cont20,sfrhii,sfrhiihdr,/square,/silent,tit=tit,off_bar=obp $
                        ,imrange=imrange,bar_tit='[Msol/yr/kpc2]',levels=cts,/nologo
        if show_cts eq 0 then $
           image_cont20,sfrhii,sfrhiihdr,/square,/silent,tit=tit,off_bar=obp $
                        ,imrange=imrange,bar_tit='[Msol/yr/kpc2]',/nologo,levels=cts_maskonly

        ps_file=use_plot_dir+sfrhii_outname+'.ps'
        png_file=use_plot_dir+sfrhii_outname+'.png'

        write_png,png_file,TVRD(/TRUE)

        image_cont20,sfrhii,sfrhiihdr,/square,/silent,tit=tit,off_bar=obp $
                     ,imrange=[-1.e-2,percentile(sfrhii,0.15)],bar_tit='[Msol/yr/kpc2]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        if firstplot eq 1 then $
           image_cont20,sfrhii,sfrhiihdr,/square,/silent,tit=tit,off_bar=obp $
                        ,imrange=[-1.e-2,percentile(sfrhii,0.15)],bar_tit='[Msol/yr/kpc2]',postscript=ps_file,/ps_color $
                        ,image_color_table=use_mapct, axis_color_table=39,/nologo,levels=cts

        firstplot=0
        cleanplot
        
     end
 
  
  end
  

  if not keyword_set(nostop) then stop
the_end:
  
  end
