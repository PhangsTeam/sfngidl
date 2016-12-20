pro make_summary_piechart_plots, show=show,nostop=nostop

  ; METHOD 1
;  restore,'../savefiles/sfng_allgals_BGmaps_relthresh_pieonly.sav'
;  use_plot_dir='../plots/relthresh/'
  
  ; METHOD 2
  restore,'../savefiles/sfng_allgals_BGmaps_80thresh_pieonly.sav'
  use_plot_dir='../plots/thresh80/'
  ; METHOD 2
;  restore,'../savefiles/sfng_allgals_BGmaps_80thresh_pieonly.sav'
;  use_plot_dir='../plots/thresh80/'

    ; METHOD 3
 ; restore,'../savefiles/sfng_allgals_BGmaps_blobthresh_pieonly.sav'
;  use_plot_dir='../plots/blobthresh/'


  sortbymass=1 & sortbysfr=0 & sortbyssfr=0 & sortbyt=0 & sortbyr25=0 & sortbyinc=0 
  
  win=0L
  loadct,39 
  
;============  
; PLOT OVERLAP FRACTION AS A FUNCTION OF SPATIAL SCALE FOR A SINGLE GALAXY
;============  

  loadct,39                     ; use rainbow colours
  PLOTSYM, 0 ,1.5, /FILL          ;Plotting symbol is a filled circle,

  use_galname='ngc628'
  pngfile=use_plot_dir+'summary.sfng.ofrac.'+use_galname+'.png'

  !p.multi=0
  window,win,xsize=600,ysize=600 & win=win+1
  reversect
  xx=where(sfco_str.tagy eq 'hasfr' and sfco_str.gal eq use_galname)
  ngals=n_elements(rem_dup(sfco_str[xx].gal))
 
  plot,sfco_str[xx].res_pc,sfco_str[xx].ofrac_byflux,/nodata,/xst,/yst $
       ,charsize=2.2,charthick=2,color=fgcolor $
       ,xtit='Linear Resolution [pc]',ytit='% pixels with overlapping CO and Halpha', yr=[0.0,0.9],xr=[0,1800.]
  oplot,sfco_str[xx].res_pc,sfco_str[xx].ofrac_byflux,color=fsc_color('Royal Blue'),thick=4
  PLOTSYM, 0 ,1.9, /FILL        ;Plotting symbol is a filled circle,
  oplot,sfco_str[xx].res_pc,sfco_str[xx].ofrac_byflux,color=fgcolor,psym=8
  PLOTSYM, 0 ,1.5, /FILL        ;Plotting symbol is a filled circle,
  oplot,sfco_str[xx].res_pc,sfco_str[xx].ofrac_byflux,color=fsc_color('Royal Blue'),psym=8

  xx=where(sfco_str.tagy eq 'hiisfr' and sfco_str.gal eq use_galname)
  ngals=n_elements(rem_dup(sfco_str[xx].gal))
 
  oplot,sfco_str[xx].res_pc,sfco_str[xx].ofrac_byflux,color=fsc_color('Firebrick'),thick=4
  PLOTSYM, 0 ,1.9, /FILL        ;Plotting symbol is a filled circle,
  oplot,sfco_str[xx].res_pc,sfco_str[xx].ofrac_byflux,color=fgcolor,psym=8
  PLOTSYM, 0 ,1.5, /FILL        ;Plotting symbol is a filled circle,
  oplot,sfco_str[xx].res_pc,sfco_str[xx].ofrac_byflux,color=fsc_color('Firebrick'),psym=8

   PLOTSYM, 0 ,2., /FILL          ;Plotting symbol is a filled circle,
   al_legend, /top, /left, box=1, clear=0 $
             , background_color=cgcolor('light gray') $
             , [use_galname,'Halpha','Filtered Halpha'] $
             , lines=[-99,0,0] $
             , thick=4 $
             , psym=8 $
             , textcolor=255 $
             , color=[255,fsc_color('Royal Blue'),fsc_color('Firebrick')]$
              , charsize=2., charthick=2


   WRITE_PNG, pngfile, TVRD(/TRUE)

   stop

hasfr_ofrac:
   
;============  
; PLOT CO only fraction AS A FUNCTION OF SPATIAL SCALE, FLUX CUT 
;============  

  PLOTSYM, 0 ,1.5, /FILL          ;Plotting symbol is a filled circle,
  psfile="../plots/summary.sfng.cofrac.hasfr.eps"
  pngfile=use_plot_dir+"summary.allgals.sfng.cofrac.hasfr.png"

  !p.multi=0
  window,win,xsize=600,ysize=600 & win=win+1

  xx=where(sfco_str.tagy eq 'hasfr')
  yy=where(sfco_str.tagy eq 'hasfr' and sfco_str.res_pc eq 300)
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal

  if sortbymass eq 1 then galsort=sfco_str[xx[galidx]].gal_stellarmass
  if sortbysfr eq 1 then galsort=sfco_str[xx[galidx]].gal_sfr
  if sortbyssfr eq 1 then galsort=sfco_str[xx[galidx]].gal_logssfr
  if sortbyt eq 1 then galsort=sfco_str[xx[galidx]].gal_ttype
  if sortbyinc eq 1 then galsort=sfco_str[xx[galidx]].gal_incl_deg
  if sortbyr25 eq 1 then galsort=(sfco_str[xx[galidx]].r25_deg/3600.)*1.e6*(sfco_str[xx[galidx]].gal_dist)/206265.
  
  do_gals=do_gals(sort(galsort))
  
  cols=range_gen(ngals,[30,255-30])

  plot,sfco_str[xx].res_pc,sfco_str[xx].co_only_byflux,/nodata,/xst,/yst $
       ,charsize=2.2,charthick=2,color=fgcolor $
       ,xtit='Linear Resolution [pc]',ytit='% pixels with CO emission only', yr=[0.0,1.2],xr=[0,1800.]
  for i=0,ngals-1 do begin
     ind=where(sfco_str[xx].gal eq do_gals[i])
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_only_byflux,color=cols[i],thick=4
     PLOTSYM, 0 ,1.9, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_only_byflux,color=fgcolor,psym=8
     PLOTSYM, 0 ,1.5, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_only_byflux,color=cols[i],psym=8
  endfor

     PLOTSYM, 0 ,2., /FILL          ;Plotting symbol is a filled circle,
 al_legend, /top, /left, box=1, clear=0 $
             , background_color=cgcolor('light gray') $
             , do_gals $
             , lines=0 $
             , thick=4 $
             , psym=8 $
             , textcolor=fgcolor $
             , color=fix(cols) $
             , charsize=1.6, charthick=2.

 al_legend, /top, /right, box=0, clear=0 $
            , ['Total Halpha'] $
            , charsize=1.6, charthick=2.

   WRITE_PNG, pngfile, TVRD(/TRUE)

 ;  stop
   
;============  
; PLOT SFR only fraction AS A FUNCTION OF SPATIAL SCALE, FLUX CUT 
;============  

  PLOTSYM, 0 ,1.5, /FILL          ;Plotting symbol is a filled circle,
  psfile="../plots/summary.sfng.sfrfrac.hasfr.eps"
  pngfile=use_plot_dir+"summary.allgals.sfng.sfrfrac.hasfr.png"

  !p.multi=0
  window,win,xsize=600,ysize=600 & win=win+1

  xx=where(sfco_str.tagy eq 'hasfr')
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal
  
  if sortbymass eq 1 then galsort=sfco_str[xx[galidx]].gal_stellarmass
  if sortbysfr eq 1 then galsort=sfco_str[xx[galidx]].gal_sfr
  if sortbyssfr eq 1 then galsort=sfco_str[xx[galidx]].gal_logssfr
  if sortbyt eq 1 then galsort=sfco_str[xx[galidx]].gal_ttype
  if sortbyinc eq 1 then galsort=sfco_str[xx[galidx]].gal_incl_deg
  if sortbyr25 eq 1 then galsort=(sfco_str[xx[galidx]].r25_deg/3600.)*1.e6*(sfco_str[xx[galidx]].gal_dist)/206265.
  
  do_gals=do_gals(sort(galsort))
  
  cols=range_gen(ngals,[30,255-30])

  plot,sfco_str[xx].res_pc,sfco_str[xx].sfr_only_byflux,/nodata,/xst,/yst $
       ,charsize=2.2,charthick=2,color=fgcolor $
       ,xtit='Linear Resolution [pc]',ytit='% pixels with Halpha emission only', yr=[0.0,1.2],xr=[0,1800.]
  for i=0,ngals-1 do begin
     ind=where(sfco_str[xx].gal eq do_gals[i])
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].sfr_only_byflux,color=cols[i],thick=4
     PLOTSYM, 0 ,1.9, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].sfr_only_byflux,color=fgcolor,psym=8
     PLOTSYM, 0 ,1.5, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].sfr_only_byflux,color=cols[i],psym=8
  endfor

     PLOTSYM, 0 ,2., /FILL          ;Plotting symbol is a filled circle,
 al_legend, /top, /left, box=1, clear=0 $
             , background_color=cgcolor('light gray') $
             , do_gals $
             , lines=0 $
             , thick=4 $
             , psym=8 $
             , textcolor=fgcolor $
             , color=fix(cols) $
             , charsize=1.6, charthick=2.

  al_legend, /top, /right, box=0, clear=0 $
            , ['Total Halpha'] $
            , charsize=1.6, charthick=2.

   WRITE_PNG, pngfile, TVRD(/TRUE)

;   stop
   
;============  
; PLOT OVERLAP FRACTION AS A FUNCTION OF SPATIAL SCALE, FLUX CUT 
; ofrac only
;============  

  PLOTSYM, 0 ,1.5, /FILL          ;Plotting symbol is a filled circle,
  psfile="../plots/summary.sfng.ofrac.hasfr.eps"
  pngfile=use_plot_dir+"summary.allgals.sfng.ofrac.hasfr.png"

  !p.multi=0
  window,win,xsize=600,ysize=600 & win=win+1

  xx=where(sfco_str.tagy eq 'hasfr')
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal
  
  if sortbymass eq 1 then galsort=sfco_str[xx[galidx]].gal_stellarmass
  if sortbysfr eq 1 then galsort=sfco_str[xx[galidx]].gal_sfr
  if sortbyssfr eq 1 then galsort=sfco_str[xx[galidx]].gal_logssfr
  if sortbyt eq 1 then galsort=sfco_str[xx[galidx]].gal_ttype
  if sortbyinc eq 1 then galsort=sfco_str[xx[galidx]].gal_incl_deg
  if sortbyr25 eq 1 then galsort=(sfco_str[xx[galidx]].r25_deg/3600.)*1.e6*(sfco_str[xx[galidx]].gal_dist)/206265.
  
  do_gals=do_gals(sort(galsort))
  
  cols=range_gen(ngals,[30,255-30])

  plot,sfco_str[xx].res_pc,sfco_str[xx].ofrac_byflux,/nodata,/xst,/yst $
       ,charsize=2.2,charthick=2,color=fgcolor $
       ,xtit='Linear Resolution [pc]',ytit='% pixels with overlapping CO and Halpha', yr=[0.0,1.2],xr=[0,1800.]
  for i=0,ngals-1 do begin
     ind=where(sfco_str[xx].gal eq do_gals[i])
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].ofrac_byflux,color=cols[i],thick=4
     PLOTSYM, 0 ,1.9, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].ofrac_byflux,color=fgcolor,psym=8
     PLOTSYM, 0 ,1.5, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].ofrac_byflux,color=cols[i],psym=8
  endfor

     PLOTSYM, 0 ,2., /FILL          ;Plotting symbol is a filled circle,
 al_legend, /top, /left, box=1, clear=0 $
             , background_color=cgcolor('light gray') $
             , do_gals $
             , lines=0 $
             , thick=4 $
             , psym=8 $
             , textcolor=fgcolor $
             , color=fix(cols) $
             , charsize=1.6, charthick=2.

  al_legend, /top, /right, box=0, clear=0 $
            , ['Total Halpha'] $
            , charsize=1.6, charthick=2.

   WRITE_PNG, pngfile, TVRD(/TRUE)

   stop

hiisfr_ofrac:
   
;============  
; PLOT CO only fraction AS A FUNCTION OF SPATIAL SCALE, FLUX CUT 
;============  

  PLOTSYM, 0 ,1.5, /FILL          ;Plotting symbol is a filled circle,
  psfile="../plots/summary.sfng.cofrac.hiisfr.eps"
  pngfile=use_plot_dir+"summary.allgals.sfng.cofrac.hiisfr.png"

  !p.multi=0
  window,win,xsize=600,ysize=600 & win=win+1

  xx=where(sfco_str.tagy eq 'hiisfr')
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal
  
  if sortbymass eq 1 then galsort=sfco_str[xx[galidx]].gal_stellarmass
  if sortbysfr eq 1 then galsort=sfco_str[xx[galidx]].gal_sfr
  if sortbyssfr eq 1 then galsort=sfco_str[xx[galidx]].gal_logssfr
  if sortbyt eq 1 then galsort=sfco_str[xx[galidx]].gal_ttype
  if sortbyinc eq 1 then galsort=sfco_str[xx[galidx]].gal_incl_deg
  if sortbyr25 eq 1 then galsort=(sfco_str[xx[galidx]].r25_deg/3600.)*1.e6*(sfco_str[xx[galidx]].gal_dist)/206265.
  
  do_gals=do_gals(sort(galsort))
  
  cols=range_gen(ngals,[30,255-30])

  plot,sfco_str[xx].res_pc,sfco_str[xx].co_only_byflux,/nodata,/xst,/yst $
       ,charsize=2.2,charthick=2,color=fgcolor $
       ,xtit='Linear Resolution [pc]',ytit='% pixels with CO emission only', yr=[0.0,1.2],xr=[0,1800.]
  for i=0,ngals-1 do begin
     ind=where(sfco_str[xx].gal eq do_gals[i])
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_only_byflux,color=cols[i],thick=4
     PLOTSYM, 0 ,1.9, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_only_byflux,color=fgcolor,psym=8
     PLOTSYM, 0 ,1.5, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_only_byflux,color=cols[i],psym=8
  endfor

     PLOTSYM, 0 ,2., /FILL          ;Plotting symbol is a filled circle,
 al_legend, /top, /left, box=1, clear=0 $
             , background_color=cgcolor('light gray') $
             , do_gals $
             , lines=0 $
             , thick=4 $
             , psym=8 $
             , textcolor=fgcolor $
             , color=fix(cols) $
             , charsize=1.6, charthick=2.

  al_legend, /top, /right, box=0, clear=0 $
            , ['Filtered Halpha'] $
            , charsize=1.6, charthick=2.

   WRITE_PNG, pngfile, TVRD(/TRUE)

   ;stop
   
;============  
; PLOT SFR only fraction AS A FUNCTION OF SPATIAL SCALE, FLUX CUT 
;============  

  PLOTSYM, 0 ,1.5, /FILL          ;Plotting symbol is a filled circle,
  psfile="../plots/summary.sfng.sfrfrac.hiisfr.eps"
  pngfile=use_plot_dir+"summary.allgals.sfng.sfrfrac.hiisfr.png"

  !p.multi=0
  window,win,xsize=600,ysize=600 & win=win+1

  xx=where(sfco_str.tagy eq 'hiisfr')
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal
  
  if sortbymass eq 1 then galsort=sfco_str[xx[galidx]].gal_stellarmass
  if sortbysfr eq 1 then galsort=sfco_str[xx[galidx]].gal_sfr
  if sortbyssfr eq 1 then galsort=sfco_str[xx[galidx]].gal_logssfr
  if sortbyt eq 1 then galsort=sfco_str[xx[galidx]].gal_ttype
  if sortbyinc eq 1 then galsort=sfco_str[xx[galidx]].gal_incl
  if sortbyr25 eq 1 then galsort=(sfco_str[xx[galidx]].r25_deg/3600.)*1.e6*(sfco_str[xx[galidx]].gal_dist)/206265.
  
  do_gals=do_gals(sort(galsort))
  
  cols=range_gen(ngals,[30,255-30])

  plot,sfco_str[xx].res_pc,sfco_str[xx].sfr_only_byflux,/nodata,/xst,/yst $
       ,charsize=2.2,charthick=2,color=fgcolor $
       ,xtit='Linear Resolution [pc]',ytit='% pixels with Halpha emission only', yr=[0.0,1.2],xr=[0,1800.]
  for i=0,ngals-1 do begin
     ind=where(sfco_str[xx].gal eq do_gals[i])
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].sfr_only_byflux,color=cols[i],thick=4
     PLOTSYM, 0 ,1.9, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].sfr_only_byflux,color=fgcolor,psym=8
     PLOTSYM, 0 ,1.5, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].sfr_only_byflux,color=cols[i],psym=8
  endfor

     PLOTSYM, 0 ,2., /FILL          ;Plotting symbol is a filled circle,
 al_legend, /top, /left, box=1, clear=0 $
             , background_color=cgcolor('light gray') $
             , do_gals $
             , lines=0 $
             , thick=4 $
             , psym=8 $
             , textcolor=fgcolor $
             , color=fix(cols) $
             , charsize=1.6, charthick=2.

   al_legend, /top, /right, box=0, clear=0 $
            , ['Filtered Halpha'] $
            , charsize=1.6, charthick=2.

   WRITE_PNG, pngfile, TVRD(/TRUE)

;   stop
   
;============  
; PLOT OVERLAP FRACTION AS A FUNCTION OF SPATIAL SCALE, FLUX CUT 
; ofrac only
;============  

  PLOTSYM, 0 ,1.5, /FILL          ;Plotting symbol is a filled circle,
  psfile="../plots/summary.sfng.ofrac.hiisfr.eps"
  pngfile=use_plot_dir+"summary.allgals.sfng.ofrac.hiisfr.png"

  !p.multi=0
  window,win,xsize=600,ysize=600 & win=win+1

  xx=where(sfco_str.tagy eq 'hiisfr')
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal
  
  if sortbymass eq 1 then galsort=sfco_str[xx[galidx]].gal_stellarmass
  if sortbysfr eq 1 then galsort=sfco_str[xx[galidx]].gal_sfr
  if sortbyssfr eq 1 then galsort=sfco_str[xx[galidx]].gal_logssfr
  if sortbyt eq 1 then galsort=sfco_str[xx[galidx]].gal_ttype
  if sortbyinc eq 1 then galsort=sfco_str[xx[galidx]].gal_incl
  if sortbyr25 eq 1 then galsort=(sfco_str[xx[galidx]].r25_deg/3600.)*1.e6*(sfco_str[xx[galidx]].gal_dist)/206265.
  
  do_gals=do_gals(sort(galsort))
  
  cols=range_gen(ngals,[30,255-30])

  plot,sfco_str[xx].res_pc,sfco_str[xx].ofrac_byflux,/nodata,/xst,/yst $
       ,charsize=2.2,charthick=2,color=fgcolor $
       ,xtit='Linear Resolution [pc]',ytit='% pixels with overlapping CO and Halpha', yr=[0.0,1.2],xr=[0,1800.]
  for i=0,ngals-1 do begin
     ind=where(sfco_str[xx].gal eq do_gals[i])
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].ofrac_byflux,color=cols[i],thick=4
     PLOTSYM, 0 ,1.9, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].ofrac_byflux,color=fgcolor,psym=8
     PLOTSYM, 0 ,1.5, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].ofrac_byflux,color=cols[i],psym=8
  endfor

     PLOTSYM, 0 ,2., /FILL          ;Plotting symbol is a filled circle,
 al_legend, /top, /left, box=1, clear=0 $
             , background_color=cgcolor('light gray') $
             , do_gals $
             , lines=0 $
             , thick=4 $
             , psym=8 $
             , textcolor=fgcolor $
             , color=fix(cols) $
             , charsize=1.6, charthick=2.

   al_legend, /top, /right, box=0, clear=0 $
            , ['Filtered Halpha'] $
            , charsize=1.6, charthick=2.

   WRITE_PNG, pngfile, TVRD(/TRUE)


   stop
   
;============  
; PLOT EMISSION FRACTION in MAP AS A FUNCTION OF SPATIAL SCALE
;============  

  PLOTSYM, 0 ,1.5, /FILL          ;Plotting symbol is a filled circle,
  psfile="../plots/summary.sfng.ofrac.hasfr.eps"
  pngfile=use_plot_dir+"summary.allgals.sfng.mapfrac.hasfr.png"

  !p.multi=0
  window,win,xsize=600,ysize=600 & win=win+1

  xx=where(sfco_str.tagy eq 'hasfr')
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal
  
  if sortbymass eq 1 then galsort=sfco_str[xx[galidx]].gal_stellarmass
  if sortbysfr eq 1 then galsort=sfco_str[xx[galidx]].gal_sfr
  if sortbyssfr eq 1 then galsort=sfco_str[xx[galidx]].gal_logssfr
  if sortbyt eq 1 then galsort=sfco_str[xx[galidx]].gal_ttype
  if sortbyinc eq 1 then galsort=sfco_str[xx[galidx]].gal_incl
  if sortbyr25 eq 1 then galsort=(sfco_str[xx[galidx]].r25_deg/3600.)*1.e6*(sfco_str[xx[galidx]].gal_dist)/206265.
  
  do_gals=do_gals(sort(galsort))
  
  cols=range_gen(ngals,[30,255-30])

  plot,sfco_str[xx].res_pc,sfco_str[xx].co_or_sfr_byflux,/nodata,/xst,/yst $
       ,charsize=2.2,charthick=2,color=fgcolor $
       ,xtit='Linear Resolution [pc]',ytit='% pixels in map with CO or Halpha emission', yr=[0.0,1.2],xr=[0,1800.]
  for i=0,ngals-1 do begin
     ind=where(sfco_str[xx].gal eq do_gals[i])
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_or_sfr_byflux,color=cols[i],thick=4
     PLOTSYM, 0 ,1.9, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_or_sfr_byflux,color=fgcolor,psym=8
     PLOTSYM, 0 ,1.5, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_or_sfr_byflux,color=cols[i],psym=8
  endfor

     PLOTSYM, 0 ,2., /FILL          ;Plotting symbol is a filled circle,
 al_legend, /top, /left, box=1, clear=0 $
             , background_color=cgcolor('light gray') $
             , do_gals $
             , lines=0 $
             , thick=4 $
             , psym=8 $
             , textcolor=fgcolor $
             , color=fix(cols) $
             , charsize=1.6, charthick=2.

   WRITE_PNG, pngfile, TVRD(/TRUE)


;   stop
   




  PLOTSYM, 0 ,1.5, /FILL          ;Plotting symbol is a filled circle,
  psfile="../plots/summary.sfng.ofrac.hiisfr.eps"
  pngfile=use_plot_dir+"summary.allgals.sfng.mapfrac.hiisfr.png"

  !p.multi=0
  window,win,xsize=600,ysize=600 & win=win+1

  xx=where(sfco_str.tagy eq 'hiisfr')
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal
  
  if sortbymass eq 1 then galsort=sfco_str[xx[galidx]].gal_stellarmass
  if sortbysfr eq 1 then galsort=sfco_str[xx[galidx]].gal_sfr
  if sortbyssfr eq 1 then galsort=sfco_str[xx[galidx]].gal_logssfr
  if sortbyt eq 1 then galsort=sfco_str[xx[galidx]].gal_ttype
  if sortbyinc eq 1 then galsort=sfco_str[xx[galidx]].gal_incl
  if sortbyr25 eq 1 then galsort=(sfco_str[xx[galidx]].r25_deg/3600.)*1.e6*(sfco_str[xx[galidx]].gal_dist)/206265.
  
  do_gals=do_gals(sort(galsort))
  
  cols=range_gen(ngals,[30,255-30])

  plot,sfco_str[xx].res_pc,sfco_str[xx].co_or_sfr_byflux,/nodata,/xst,/yst $
       ,charsize=2.2,charthick=2,color=fgcolor $
       ,xtit='Linear Resolution [pc]',ytit='% pixels in map with CO or Halpha emission', yr=[0.0,1.2],xr=[0,1800.]
  for i=0,ngals-1 do begin
     ind=where(sfco_str[xx].gal eq do_gals[i])
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_or_sfr_byflux,color=cols[i],thick=4
     PLOTSYM, 0 ,1.9, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_or_sfr_byflux,color=fgcolor,psym=8
     PLOTSYM, 0 ,1.5, /FILL     ;Plotting symbol is a filled circle,
     oplot,sfco_str[xx[ind]].res_pc,sfco_str[xx[ind]].co_or_sfr_byflux,color=cols[i],psym=8
  endfor

     PLOTSYM, 0 ,2., /FILL          ;Plotting symbol is a filled circle,
 al_legend, /top, /left, box=1, clear=0 $
             , background_color=cgcolor('light gray') $
             , do_gals $
             , lines=0 $
             , thick=4 $
             , psym=8 $
             , textcolor=fgcolor $
             , color=fix(cols) $
             , charsize=1.6, charthick=2.

   WRITE_PNG, pngfile, TVRD(/TRUE)


;   stop
   


 
   
  stop
  
 the_end: 

end

