pro sfco_versus_scale, keyfile=keyfile $
                       , savefile=savefile $
                       , append=append $
                       , nostop=nostop $
                       , show=show $
                       , verbose=verbose $
                       , pie=pie $
                       , scatter=scatter $
                       , cdf=cdf $
                       , forcepositive=forcepositive $
                       , physical=physical $
                       , missing=missing $
                       , plot_dir=plot_dir

;+
;
; Generate plots and measurements related to piecharts, scatter plots and x/CDFs
; Save results to an IDL structure (which can be used to compare galaxies)
;
; galaxy parameters obtained from AKL's galbase IDL routines
; https://github.com/akleroy/galbase
;-

  
  @constants.bat
  nan = !values.f_nan

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DEFAULTS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  do_pie=0
  do_scatter=0
  do_cdf=0
  do_append=0
  do_show=0
  do_forcepositive=0
  do_physical=0
  use_missing=0.
  use_plot_dir='../plots/'
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PROCESS USER INPUTS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if keyword_set(pie) then do_pie = 1
  if keyword_set(cdf) then do_cdf = 1
  if keyword_set(scatter) then do_scatter = 1
  if keyword_set(append) then do_append = 1
  if keyword_set(show) then do_show = 1
  if keyword_set(physical) then do_physical = 1
  if keyword_set(missing) then use_missing=missing
  if keyword_set(plot_dir) then use_plot_dir=plot_dir
  if keyword_set(forcepositive) then do_forcepositive = 1

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; OPEN LOG SO WE CAN TRACK WHERE WE'RE UP TO
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  openw,lun,'sfng_sfco.log',/get_lun,/append
  printf,lun, "Reading in: ",keyfile
  nlines=file_lines(keyfile)
  print,"-----------------------------"
  print," Reading in: ",keyfile," with # lines: ",nlines
  print,"-----------------------------"

;=============================
; READ IN KEY FILE
; for now, we do not break individual galaxies into different environments
;=============================
  
  keyStruct = {flag:0, gal:'', co:'', sfr:'', mask:'', res:0., cothresh:0., sfrthresh:0., perc:0., env:0, tagx:'', tagy:''}
  keydata=replicate(keyStruct,nlines)

  readcol, keyfile $
           , tmpflag $
           , tmpgal $
           , tmpco $
           , tmpsfr $
           , tmpmask $
           , tmpres $
           , tmpcothresh $
           , tmpsfrthresh $
           , tmpperc $
           , tmpenv $
           , tmptagx $
           , tmptagy $
           , format='I,A,A,A,A,F,F,F,F,I,A,A' $
           , comment="#"
  
  keydata.flag=tmpflag
  keydata.gal=tmpgal
  keydata.co=tmpco
  keydata.sfr=tmpsfr
  keydata.mask=tmpmask
  keydata.res=tmpres
  keydata.env=tmpenv
  keydata.perc=tmpperc
  keydata.cothresh=tmpcothresh
  keydata.sfrthresh=tmpsfrthresh
  keydata.tagx=tmptagx
  keydata.tagy=tmptagy
  
;=============================
  ; create structure to hold results
;=============================

  tmpstr=empty_sfco_stats()

  
;=============================
  ; begin working on image pairs
;=============================

  for i=0,nlines-1 do begin
     
     galname = keydata(i).gal
     gstr=gal_data(galname)
     galdist=gstr.dist_mpc*1.e6 

     print, " "
     print, "Processing image pair, galaxy: ", i, galname

     if keydata(i).flag gt 0 then begin

        if keyword_set(verbose) then $
           print, "Getting measurements for: ",keydata(i).tagx, " and ", keydata(i).tagy, " in ", keydata(i).gal, " for top ", keydata(i).perc,"% of pixels"
        printf, lun, "Getting measurements for: ",keydata(i).tagx, " and ", keydata(i).tagy, " in ", keydata(i).gal, " for top ", keydata(i).perc,"% of pixels"

        resstr=strnsignif(keydata(i).res,2) +"pc"
        percstr=strnsignif(keydata(i).perc,2) +"pct"

        this_plotname = keydata(i).gal + "." + keydata(i).tagx +"."+ keydata(i).tagy+"."+resstr+"."+percstr+"." 
        use_plotname=use_plot_dir+this_plotname
        
;=============================
; INITIALISE SOME INFORMATION IN THE SAVE STRUCTURE
;=============================
          
        tmpstr.tagx=keydata(i).tagx
        tmpstr.tagy=keydata(i).tagy
        tmpstr.co=keydata(i).co
        tmpstr.sfr=keydata(i).sfr
        tmpstr.mask=keydata(i).mask
        tmpstr.res_as=keydata(i).res*206265./galdist
        tmpstr.res_pc=keydata(i).res
        tmpstr.perc=keydata(i).perc
        tmpstr.cothreshval=keydata(i).cothresh
        tmpstr.sfrthreshval=keydata(i).sfrthresh
        tmpstr.env=keydata(i).env
        tmpstr.gal=keydata(i).gal


        gstr_s4g=read_xcat('S4G_masses_SSFR_piechart.xcat')
        tt=where(gstr_s4g.galaxy eq keydata(i).gal,ct)
        if ct ne 1 then begin
           print,'Cannot find galaxy in S4G catalogue'
           stop
        end else begin
           tmpstr.gal_logssfr=gstr_s4g[tt].logssfr
           tmpstr.gal_stellarmass=gstr_s4g[tt].totmass
           tmpstr.gal_sfr=gstr_s4g[tt].sfr
        end
        
        tmpstr.gal_dist=gstr.dist_mpc
        tmpstr.gal_hi_mass=gstr.hi_msun
        tmpstr.gal_lfir_lsun=gstr.lfir_lsun
        tmpstr.gal_incl_deg=gstr.incl_deg
        tmpstr.gal_ttype=gstr.t
        tmpstr.gal_r25_deg=gstr.r25_deg
        tmpstr.gal_bar=gstr.bar
        tmpstr.gal_vrot_kms=gstr.vrot_kms
        tmpstr.gal_btc_mag=gstr.btc_mag
        tmpstr.gal_btc_absmag=gstr.btc_mag-5*(alog10(1.e6*gstr.dist_mpc)-1)

;=============================
; READ IN DATA AND MASK(s)
;=============================

        co_in = readfits(keydata(i).co, hdr_co)
        sfr_in = readfits(keydata(i).sfr, hdr_sfr)
        mask = readfits(keydata(i).mask, hdr_m)
        co_use=co_in & sfr_use=sfr_in

        co_flux_allpix=total(co_use,/nan)
        sfr_flux_allpix=total(sfr_use,/nan)
        print,'Total Flux in CO Map before Blanking Negatives:',co_flux_allpix
        print,'Total Flux in SFR Map before Blanking Negatives:',sfr_flux_allpix

        tmpstr.co_flux_allpix=co_flux_allpix
        tmpstr.sfr_flux_allpix=sfr_flux_allpix

        cozind = where(finite(co_use) eq 1 and co_use gt 0, posct)
        sfrzind = where(finite(sfr_use) eq 1 and sfr_use gt 0, posct)
        
        co_flux_pospix=total(co_use[cozind],/nan)
        sfr_flux_pospix=total(sfr_use[sfrzind],/nan)
        print,'Positive Flux in CO Map [%change]:',co_flux_pospix,abs(co_flux_pospix-co_flux_allpix)/co_flux_pospix
        print,'Positive Flux in SFR Map [%change]:',sfr_flux_pospix,abs(sfr_flux_pospix-sfr_flux_allpix)/sfr_flux_pospix
        
        tmpstr.co_flux_pospix=co_flux_pospix
        tmpstr.sfr_flux_pospix=sfr_flux_pospix
 
;=============================
; SET NEGATIVE VALUES TO ZERO OR NAN?
;=============================
        ;; cozind = where(finite(co_in) eq 1 and co_in lt 0, negct)
        ;; if negct gt 0 then co_use[cozind] = 0.
        ;; sfrzind = where(finite(sfr_in) eq 1 and sfr_in lt 0, negct)
        ;; if negct gt 0 then sfr_use[sfrzind] = 0.
        if keyword_set(do_forcepositive) then begin
           co_negind = where(finite(co_use) eq 1 and co_use lt 0, negct)
           if negct gt 0 then co_use[co_negind] = use_missing
           print,"Setting # negative pixels in CO map to MISSING_VAL: ",negct
           print,"Setting % negative pixels in CO map to MISSING_VAL: ",negct/float(n_elements(co_use))
           printf,lun,"Setting # negative pixels  in CO map to MISSING_VAL: ",negct
           printf,lun,"Setting % negative pixels in CO map to MISSING_VAL: ",negct/float(n_elements(co_use))
           sfr_negind = where(finite(sfr_use) eq 1 and sfr_use lt 0, negct)
           if negct gt 0 then sfr_use[sfr_negind] = use_missing
           print,"Setting # negative pixels in SFR map to MISSING_VAL: ",negct
           print,"Setting % negative pixels in SFR map to MISSING_VAL: ",negct/float(n_elements(sfr_use))
           printf,lun,"Setting # negative  pixels in SFR map to MISSING_VAL: ",negct
           printf,lun,"Setting % negative  pixels in SFR map to MISSING_VAL: ",negct/float(n_elements(sfr_use))
           tmpstr.force_pos=1
        end
        
;=============================
; OUR WORKING PIXELS
; co_vec/sfr_vec -- 1D arrays (use for CDFs, scatter plots)
; co_use/sfr_use -- 2D arrays (use for map analysis, e.g. overlap)
;=============================
;        ind = where(mask gt 0 and finite(co_in) eq 1 and finite(sfr_in) eq 1,complement=badind)
;        co_vec=co_in[ind] & sfr_vec=sfr_in[ind]
        ind = where(mask gt 0 and finite(co_use) eq 1 and finite(sfr_use) eq 1,ct,complement=badind)
        co_vec=co_use[ind] & sfr_vec=sfr_use[ind]
        co_use[badind]=!values.f_nan & sfr_use[badind]=!values.f_nan
        print,"Pixels in map (includes NaNs): ",n_elements(co_use),n_elements(sfr_use)
        print,"Pixels in vector (excludes NaNs, masked pixels): ",ct
        printf,lun,"Pixels in map (includes NaNs): ",n_elements(co_use),n_elements(sfr_use)
        printf,lun,"Pixels in vector (excludes NaNs, masked pixels): ",ct

;=============================
; FLUX THRESHOLD, i.e. do not include pixels fainter than requested
; flux threshold
;=============================

; OLDWAY -- USING PERCENTAGE OF TOTAL FLUX
;        co_sort=co_vec(sort(co_vec))
;        co_cdf_norm=total(co_sort,/cumul)/total(co_vec)
;        co_fluxthresh=interpol(co_sort,co_cdf_norm,(100-keydata(i).perc)/100.)

; NEWWAY -- ALLOW THRESHOLDS TO BE DEFINED PHYSICALLY OR AS A % OF
;           TOTAL POSITIVE FLUX

        if keyword_set(do_physical) then $
           co_fluxthresh=keydata[i].cothresh

        if not keyword_set(do_physical) then begin
           posind=where(co_vec gt 0.)
           co_posvec=co_vec[posind]
           co_sort=co_posvec(sort(co_posvec))
           co_cdf_norm=total(co_sort,/cumul)/total(co_posvec)
           co_fluxthresh=interpol(co_sort,co_cdf_norm,(100-keydata(i).cothresh)/100.)
        end
        
        co_use_byflux=co_use
        co_vec_byflux=co_vec
        ind=where(co_use gt co_fluxthresh,complement=badind,ct,ncomp=bct)
        vind=where(co_vec gt co_fluxthresh,vct,ncomp=bvct)
        if bct ge 1 then co_use_byflux[badind]=!values.f_nan
        if vct ge 1 then co_vec_byflux=co_vec[vind]
        print,"CO flux threshold, removing #pixels:",bct,bvct
        printf,lun,"CO flux threshold, removing #pixels:",bct,bvct
        print,"CO flux threshold, removing %pixels:",bct/float(n_elements(co_use)),bvct/float(n_elements(co_vec))
        printf,lun,"CO flux threshold, removing %pixels:",bct/float(n_elements(co_use)),bvct/float(n_elements(co_vec))

        print,"This threshold represents: ",total(co_use_byflux,/nan)/co_flux_pospix," of CO positive flux at this scale"

; OLDWAY -- USING PERCENTAGE OF TOTAL FLUX
;        sfr_sort=sfr_vec(sort(sfr_vec))
;        sfr_cdf_norm=total(sfr_sort,/cumul)/total(sfr_vec)
;        sfr_fluxthresh=interpol(sfr_sort,sfr_cdf_norm,(100-keydata(i).perc)/100.)
        
; NEWWAY -- ALLOW THRESHOLDS TO BE DEFINED PHYSICALLY OR AS A % OF
;           TOTAL POSITIVE FLUX

        if keyword_set(do_physical) then $
            sfr_fluxthresh=keydata[i].sfrthresh

       if not keyword_set(do_physical) then begin
           posind=where(sfr_vec gt 0.)
           sfr_posvec=sfr_vec[posind]
           sfr_sort=sfr_posvec(sort(sfr_posvec))
           sfr_cdf_norm=total(sfr_sort,/cumul)/total(sfr_posvec)
           sfr_fluxthresh=interpol(sfr_sort,sfr_cdf_norm,(100-keydata(i).sfrthresh)/100.)
        end

        sfr_use_byflux=sfr_use
        sfr_vec_byflux=sfr_vec
        ind=where(sfr_use gt sfr_fluxthresh,complement=badind,ct,ncomp=bct)
        vind=where(sfr_vec gt sfr_fluxthresh,vct,ncomp=bvct)
        if bct ge 1 then sfr_use_byflux[badind]=!values.f_nan
        if vct ge 1 then sfr_vec_byflux=sfr_vec(vind)
        print,"SFR flux threshold, removing #pixels:",bct,bvct
        printf,lun,"SFR flux threshold, removing #pixels:",bct,bvct
        print,"SFR flux threshold, removing %pixels:",bct/float(n_elements(sfr_use)),bvct/float(n_elements(sfr_vec))
        printf,lun,"SFR flux threshold, removing %pixels:",bct/float(n_elements(sfr_use)),bvct/float(n_elements(sfr_vec))

        print,"This threshold represents: ",total(sfr_use_byflux,/nan)/sfr_flux_pospix," of SFR positive flux at this scale"

;=============================
; ADD THRESHOLDING INFORMATION TO THE SAVE STRUCTURE
;=============================
        
        tmpstr.co_fluxthresh=co_fluxthresh
        tmpstr.sfr_fluxthresh=sfr_fluxthresh
        
        tmpstr.co_flux_threshpix=total(co_use_byflux,/nan)
        tmpstr.sfr_flux_threshpix=total(sfr_use_byflux,/nan)
        
;=============================
; NUMBER THRESHOLD, i.e. stop including fainter pixels when you have
; N% of all positive pixels
; we have a problem here with repeated values -- only an issue for 0.0
;                                                in CO maps?
;=============================

        ;; co_rsort=co_vec(reverse(sort(co_vec)))
        ;; sfr_rsort=sfr_vec(reverse(sort(sfr_vec)))

        ;; co_npix=n_elements(co_rsort) & sfr_npix=n_elements(sfr_rsort)
        ;; if co_npix ne sfr_npix then $
        ;;    message,'Maps have different pixelization?'

        ;; use_npix=round(co_npix*keydata(i).perc/100.)
        ;; co_vec_bynum=co_rsort[0:use_npix]
        ;; sfr_vec_bynum=sfr_rsort[0:use_npix]

        ;; if n_elements(sfr_vec_bynum) ne n_elements(co_vec_bynum) then $
        ;;    message,'Problem with CO/SFR bynum vectors'

        ;; co_numthresh=percentile(co_vec,keydata(i).perc)
        ;; co_use_bynum=co_use
        ;; ind=where(finite(co_use) eq 1 and co_use ge co_numthresh,complement=badind,ct,ncomp=bct)
        ;; if bct ge 1 then co_use_bynum[badind]=!values.f_nan
  
        ;; sfr_numthresh=percentile(sfr_vec,keydata(i).perc)
        ;; sfr_use_bynum=sfr_use
        ;; ind=where(finite(sfr_use) eq 1 and sfr_use ge sfr_numthresh,complement=badind,ct,ncomp=bct)
        ;; if bct ge 1 then sfr_use_bynum[badind]=!values.f_nan
      
        ;; if n_elements(sfr_vec_bynum) ne n_elements(co_vec_bynum) then stop
        
;=============================
; ADD THRESHOLDING INFORMATION TO THE SAVE STRUCTURE
;=============================
          
        ;; tmpstr.co_numthresh=co_numthresh
        ;; tmpstr.sfr_numthresh=sfr_numthresh

;=============================
; MAKE MAPS TO VISUALISE THE EMISSION WE ARE ANALYSING
;=============================
        
; define astrometric grid
        make_axes, hdr_co, ra=ra, da=da, ri=ri, di=di
        xoff = (ra - mean(ra))*cos(!dtor*mean(da))*!dtor*galdist/1.d3
        yoff = (da - mean(da))*!dtor*galdist/1d3

; image min and max
        co_immax=max(co_vec) & sfr_immax=max(sfr_vec)

; get 25 and 75% contours
        ;; co_bynum_im25=percentile(co_vec_bynum,25)
        ;; sfr_bynum_im25=percentile(sfr_vec_bynum,25)
        ;; co_bynum_im75=percentile(co_vec_bynum,75)
        ;; sfr_bynum_im75=percentile(sfr_vec_bynum,75)

        co_byflux_im25=percentile(co_vec_byflux,25)
        sfr_byflux_im25=percentile(sfr_vec_byflux,25)
        co_byflux_im75=percentile(co_vec_byflux,75)
        sfr_byflux_im75=percentile(sfr_vec_byflux,75)
        
; CO and SFR maps by number and by flux threshold
        
          ;; psfile = '../plots/'+plotname+'CO.bynum.eps'
          ;; cgPS_open, filename=psfile, /encapsulated, xsize=8, ysize=5, /times
          ;; !p.multi=0
          ;; loadct, 1
          ;; reversect

          ;; disp, co_use_bynum, max=co_immax, min=0., xoff, yoff, /sq, reserve=5 $
          ;;       , color=cgcolor('black',255), charsize=1.25, missing = -1 $
          ;;       , xtitle='Offset [kpc]', ytitle='Offset [kpc]' $
          ;;       ,title=keydata(i).gal+', CO, Cut by Number'
          ;; contour, co_use_bynum, xoff, yoff, lev=[co_bynum_im25] $
          ;;          , /overplot, c_color=cgcolor('black',255) $
          ;;          , c_thick=1
          ;; contour, co_use_bynum, xoff, yoff, lev=[co_bynum_im75] $
          ;;          , /overplot, c_color=cgcolor('gray',255) $
          ;;          , c_thick=1
          ;; al_legend, /top, /right, box=1, clear=1 $
          ;;            , background=cgcolor('lightgray',254) $
          ;;            , text=cgcolor('black',255) $
          ;;            , lines=-99,charsize=0.5 $
          ;;            , ['CO',resstr,percstr]
          ;; cgPS_close
          ;; if keyword_set(do_show) then spawn, 'gv '+psfile+' &'

          psfile = use_plotname+'CO.byflux.eps'
          cgPS_open, filename=psfile, /encapsulated, xsize=8, ysize=5, /times
          !p.multi=0
          loadct, 1
          reversect
          disp, co_use_byflux, max=co_immax, min=0., xoff, yoff, /sq, reserve=5 $
                , color=cgcolor('black',255), charsize=1.25, missing = -1 $
                , xtitle='Offset [kpc]', ytitle='Offset [kpc]' $
                ,title=keydata(i).gal+', CO, cut at: '+sigfig(co_fluxthresh,3,/sci)
          contour, co_use_byflux, xoff, yoff, lev=[co_byflux_im25] $
                   , /overplot, c_color=cgcolor('black',255) $
                   , c_thick=1
          contour, co_use_byflux, xoff, yoff, lev=[co_byflux_im75] $
                   , /overplot, c_color=cgcolor('gray',255) $
                   , c_thick=1
          al_legend, /top, /right, box=1, clear=1 $
                     , background=cgcolor('lightgray',254) $
                     , text=cgcolor('black',255) $
                     , lines=-99,charsize=0.5 $
                     , ['CO',resstr,percstr]

          cgPS_close
          if keyword_set(do_show) then spawn, 'gv '+psfile+' &'  

          
          ;; psfile = '../plots/'+plotname+'SFR.bynum.eps'
          ;; cgPS_open, filename=psfile, /encapsulated, xsize=8, ysize=5, /times
          ;; !p.multi=0
          ;; loadct, 3
          ;; reversect
          ;; disp, sfr_use_bynum, max=sfr_immax, min=0., xoff, yoff, /sq, reserve=5 $
          ;;       , color=cgcolor('black',255), charsize=1.25 , missing = -1 $
          ;;       , xtitle='Offset [kpc]', ytitle='Offset [kpc]' $
          ;;       ,title=keydata(i).gal+', SFR tracer, Cut by Number'
          
          ;; contour, sfr_use_bynum, xoff, yoff, lev=[sfr_byflux_im25] $
          ;;          , /overplot, c_color=cgcolor('black',255) $
          ;;          , c_thick=1
          ;; contour, sfr_use_bynum, xoff, yoff, lev=[sfr_byflux_im75] $
          ;;          , /overplot, c_color=cgcolor('gray',255) $
          ;;          , c_thick=1
          ;; al_legend, /top, /right, box=1, clear=1 $
          ;;            , background=cgcolor('lightgray',254) $
          ;;            , text=cgcolor('black',255) $
          ;;            , lines=-99,charsize=0.5 $
          ;;            , ['SFR',resstr,percstr]

          ;; cgPS_close
          ;; if keyword_set(do_show) then spawn, 'gv '+psfile+' &'  

          psfile = use_plotname+'SFR.byflux.eps'
          cgPS_open, filename=psfile, /encapsulated, xsize=8, ysize=5, /times
          !p.multi=0
          loadct, 3
          reversect
          disp, sfr_use_byflux, max=sfr_immax, min=0., xoff, yoff, /sq, reserve=5 $
                , color=cgcolor('black',255), charsize=1.25, missing = -1 $
                , xtitle='Offset [kpc]', ytitle='Offset [kpc]' $
                ,title=keydata(i).gal+', SFR tracer, flux cut at: '+sigfig(sfr_fluxthresh,3,/sci)

          contour, sfr_use_byflux, xoff, yoff, lev=[sfr_byflux_im25] $
                   , /overplot, c_color=cgcolor('black',255) $
                   , c_thick=1
          contour, sfr_use_byflux, xoff, yoff, lev=[sfr_byflux_im75] $
                   , /overplot, c_color=cgcolor('gray',255) $
                   , c_thick=1
;          contour, center_mask, xoff, yoff, /overplot, lev=[1] $
;                   , c_color=cgcolor('gray',255), c_thick=5, c_lines=1
          al_legend, /top, /right, box=1, clear=1 $
                     , background=cgcolor('lightgray',254) $
                     , text=cgcolor('black',255) $
                     , lines=-99,charsize=0.5 $
                     , ['SFR',resstr,percstr]

          cgPS_close
          if keyword_set(do_show) then spawn, 'gv '+psfile+' &'  

;=============================
; ADD PERCENTILE INFORMATION TO THE SAVE STRUCTURE
;=============================
          
          tmpstr.co_byflux_im25=co_byflux_im25
          tmpstr.co_byflux_im75=co_byflux_im75
          tmpstr.sfr_byflux_im25=sfr_byflux_im25
          tmpstr.sfr_byflux_im75=sfr_byflux_im75
          ;; tmpstr.co_bynum_im25=co_bynum_im25
          ;; tmpstr.co_bynum_im75=co_bynum_im75
          ;; tmpstr.sfr_bynum_im25=sfr_bynum_im25
          ;; tmpstr.sfr_bynum_im75=sfr_bynum_im75
          
;=============================
; SCATTER PLOTS
;=============================

          if keyword_set(do_scatter) then begin

             scatter_byflux_params = $
                soft_loglog_plot(co_cut=co_use_byflux $
                                 , sfr_cut = sfr_use_byflux $
                                 , co_all = co_use $
                                 , sfr_all = sfr_use $
                                 , hdr = hdr_co $
                                 , psname=use_plotname+'scatter.byflux.eps')
          
             ;; scatter_bynum_params = $
             ;;    soft_loglog_plot(co_cut=co_use_bynum $
             ;;                     , sfr_cut = sfr_use_bynum $
             ;;                     , co_all = co_use $
             ;;                     , sfr_all = sfr_use $
             ;;                     , hdr = hdr_co $
             ;;                     , psname=plotname+'scatter.bynum.eps')

             end
          
;=============================
; IMAGE OVERLAP, i.e. the piechart measurements
;=============================

          if keyword_set(do_pie) then begin

             ;; overlap_bynum_params = $
             ;;    image_overlap(co=co_use_bynum $ 
             ;;                  , sfr=sfr_use_bynum $
             ;;                  , mask=mask $
             ;;                  , hdr=hdr_co $
             ;;                  , /verbose, show=do_show, /nostop $
             ;;                  , tagco=keydata(i).tagx $
             ;;                  , tagsfr=keydata(i).tagy $
             ;;                  , psname=plotname+'olap.bynum.eps')
             
             overlap_byflux_params = $
                image_overlap(co=co_use_byflux $ 
                              , sfr=sfr_use_byflux $
                              , mask=mask $
                              , hdr=hdr_co $
                              , /verbose, show=do_show, /nostop $
                              , tagco=keydata(i).tagx $
                              , tagsfr=keydata(i).tagy $
                              , psname=use_plotname+'olap.byflux.eps' $
                              , /nozeroes)

             ;; pie_bynum_params = piechart(covec=co_vec_bynum $
             ;;                             , sfrvec=sfr_vec_bynum $
             ;;                             , comap=co_use_bynum $
             ;;                             , sfrmap=sfr_use_bynum $
             ;;                             , /verbose, show=do_show,/nostop $
             ;;                             , tagco=keydata(i).tagx $
             ;;                             , tagsfr=keydata(i).tagy $
             ;;                             , psname=plotname+'pie.bynum.eps')
             
             pie_byflux_params = piechart(covec=co_vec_byflux $
                                          , sfrvec=sfr_vec_byflux $
                                          , comap=co_use_byflux $
                                          , sfrmap=sfr_use_byflux $
                                          , /verbose, show=do_show, /nostop $
                                          , tagco=keydata(i).tagx $
                                          , tagsfr=keydata(i).tagy $
                                          , thresh=[-1,50.] $
                                          , psname=use_plotname+'pie.byflux.eps')
             

        ;; tmpstr.ofrac_bynum=overlap_bynum_params[0]
        ;; tmpstr.sfr_only_bynum=overlap_bynum_params[1]
        ;; tmpstr.co_only_bynum=overlap_bynum_params[2]
        ;; tmpstr.sfr_all_bynum=overlap_bynum_params[3]
        ;; tmpstr.co_all_bynum=overlap_bynum_params[4]
        ;; tmpstr.no_co_no_sfr_bynum=overlap_bynum_params[5]
        ;; tmpstr.co_and_sfr_bynum=overlap_bynum_params[6]
        ;; tmpstr.co_or_sfr_bynum=overlap_bynum_params[7]

        tmpstr.ofrac_byflux=overlap_byflux_params[0]
        tmpstr.sfr_only_byflux=overlap_byflux_params[1]
        tmpstr.co_only_byflux=overlap_byflux_params[2]
        tmpstr.sfr_all_byflux=overlap_byflux_params[3]
        tmpstr.co_all_byflux=overlap_byflux_params[4]
        tmpstr.no_co_no_sfr_byflux=overlap_byflux_params[5]
        tmpstr.co_and_sfr_byflux=overlap_byflux_params[6]
        tmpstr.co_or_sfr_byflux=overlap_byflux_params[7]

        ;; tmpstr.frac_p1_sfr_only_bynum=pie_bynum_params[0]
        ;; tmpstr.frac_p1_co_only_bynum=pie_bynum_params[1]
        ;; tmpstr.frac_p75_sfr_only_bynum=pie_bynum_params[2]
        ;; tmpstr.frac_p75_co_only_bynum=pie_bynum_params[3]
        ;; tmpstr.frac_p1_both_bynum=pie_bynum_params[4]
        ;; tmpstr.frac_p75_both_bynum=pie_bynum_params[5]
        ;; tmpstr.frac_p1_sfr_p75_co_bynum=pie_bynum_params[6]
        ;; tmpstr.frac_p1_co_p75_sfr_bynum=pie_bynum_params[7]
        ;; tmpstr.pie_sfr_thresh1_bynum=pie_bynum_params[8]
        ;; tmpstr.pie_sfr_thresh2_bynum=pie_bynum_params[9]
        ;; tmpstr.pie_co_thresh1_bynum=pie_bynum_params[10]
        ;; tmpstr.pie_co_thresh2_bynum=pie_bynum_params[11]
        
        tmpstr.frac_p1_sfr_only_byflux=pie_byflux_params[0]
        tmpstr.frac_p1_co_only_byflux=pie_byflux_params[1]
        tmpstr.frac_p2_sfr_only_byflux=pie_byflux_params[2]
        tmpstr.frac_p2_co_only_byflux=pie_byflux_params[3]
        tmpstr.frac_p1_both_byflux=pie_byflux_params[4]
        tmpstr.frac_p2_both_byflux=pie_byflux_params[5]
        tmpstr.frac_p1_sfr_p2_co_byflux=pie_byflux_params[6]
        tmpstr.frac_p1_co_p2_sfr_byflux=pie_byflux_params[7]
        tmpstr.pie_sfr_thresh1_byflux=pie_byflux_params[8]
        tmpstr.pie_sfr_thresh2_byflux=pie_byflux_params[9]
        tmpstr.pie_co_thresh1_byflux=pie_byflux_params[10]
        tmpstr.pie_co_thresh2_byflux=pie_byflux_params[11]

     end

;=============================
; CDFs: only do if we are analysing majority of pixels?
;=============================
          if keyword_set(do_cdf) then begin

; go right back to original datasets
; CDF only makes sense if we have              
; same number of pixels in each vector to xcross-sort

             co_use=co_in & sfr_use=sfr_in
             ind = where(mask gt 0 and finite(co_use) eq 1 and finite(sfr_use) eq 1,ct,complement=badind)
             co_vec=co_use[ind] & sfr_vec=sfr_use[ind]
        
             cdf_params = cdf_analyse_simple(co=co_vec $
                                      , sfr=sfr_vec $
                                      , tagco=keydata(i).tagx $
                                      , tagsfr=keydata(i).tagy $
                                      , psname=plotname $
                                      , verbose=1,show=1,nostop=1)

             tmpstr.ks_cosfr_d=cdf_params[0]
             tmpstr.ks_cosfr_p=cdf_params[1]
             tmpstr.co_ff_p25=cdf_params[2]
             tmpstr.co_ff_p50=cdf_params[3]
             tmpstr.co_ff_p75=cdf_params[4]
             tmpstr.sfr_ff_p25=cdf_params[5]
             tmpstr.sfr_ff_p50=cdf_params[6]
             tmpstr.sfr_ff_p75=cdf_params[7]
             tmpstr.cobysfr_ff_p25=cdf_params[8]
             tmpstr.cobysfr_ff_p50=cdf_params[9]
             tmpstr.cobysfr_ff_p75=cdf_params[10]
             tmpstr.sfrbyco_ff_p25=cdf_params[11]
             tmpstr.sfrbyco_ff_p50=cdf_params[12]
             tmpstr.sfrbyco_ff_p75=cdf_params[13]
             tmpstr.co_np_f25=cdf_params[14]
             tmpstr.co_np_f50=cdf_params[15]
             tmpstr.co_np_f75=cdf_params[16]
             tmpstr.sfr_np_f25=cdf_params[17]
             tmpstr.sfr_np_f50=cdf_params[18]
             tmpstr.sfr_np_f75=cdf_params[19]
             tmpstr.cobysfr_np_f25=cdf_params[20]
             tmpstr.cobysfr_np_f50=cdf_params[21]
             tmpstr.cobysfr_np_f75=cdf_params[22]
             tmpstr.sfrbyco_np_f25=cdf_params[23]
             tmpstr.sfrbyco_np_f50=cdf_params[24]
             tmpstr.sfrbyco_np_f75=cdf_params[25]
             tmpstr.co_ncdf_pwidth=cdf_params[26]
             tmpstr.sfr_ncdf_pwidth=cdf_params[27]
             tmpstr.co_ncdf_width=cdf_params[28]
             tmpstr.sfr_ncdf_width=cdf_params[29]
             tmpstr.cobysfr_ncdf_width=cdf_params[30]
             tmpstr.sfrbyco_ncdf_width=cdf_params[31]
             tmpstr.co_cdf_width=cdf_params[32]
             tmpstr.sfr_cdf_width=cdf_params[33]
             tmpstr.cobysfr_cdf_width=cdf_params[34]
             tmpstr.sfrbyco_cdf_width=cdf_params[35]
             tmpstr.co_cdf_width_norm=cdf_params[36]
             tmpstr.sfr_cdf_width_norm=cdf_params[37]
             tmpstr.cobysfr_cdf_width_norm=cdf_params[38]
             tmpstr.sfrbyco_cdf_width_norm=cdf_params[39]
          end
     
;=============================
; SAVE THE MEASUREMENTS TO AN IDL FILE
;=============================
  
  if keyword_set(do_append) then begin
     print,"Will append to data in savefile: ",savefile
     restore,savefile
     sfco_str=[sfco_str,tmpstr]
  endif else begin
     print,"Creating new savefile: ",savefile
     sfco_str=tmpstr
     do_append=1
  endelse
  
  save,sfco_str,filename=savefile

end
     
  endfor

;=============================
; FINISH UP
;=============================
  
  print,"-----------------------------"
  print,"Finished processing: ",keyfile
  print,"-----------------------------"
  printf, lun, "Finished: ",keyfile
  close,lun
  free_lun,lun

  if not keyword_set(nostop) then stop

  the_end:
  
end

