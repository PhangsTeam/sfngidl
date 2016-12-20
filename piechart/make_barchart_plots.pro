pro make_barchart_plots, show=show,nostop=nostop

    ; METHOD 1
;  restore,'../savefiles/sfng_allgals_BGmaps_relthresh_pieonly.sav'
;  use_plot_dir='../plots/relthresh/'
  
  ; METHOD 2
  restore,'../savefiles/sfng_allgals_BGmaps_80thresh_pieonly.sav'
  use_plot_dir='../plots/thresh80/'

    ; METHOD 3
 ; restore,'../savefiles/sfng_allgals_BGmaps_blobthresh_pieonly.sav'
;  use_plot_dir='../plots/blobthresh/'


  sortbymass=0 & sortbysfr=0 & sortbyssfr=0 & sortbyt=1 & sortbyr25=0 & sortbyinc=0
  

  use_perc=100.
    
; CGBARPLOT OF STAR-GAS CYCLING VERSUS RESOLUTION FOR EACH GALAXY

  xx=where(sfco_str.tagy eq 'hasfr')
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal
  cols=range_gen(ngals,[30,255-30])

  
  stop
  
  for i=0,ngals-1 do begin

     galname=do_gals[i]
     psfile=use_plot_dir+galname+'.bar.pie.coha.eps'
     pngfile=use_plot_dir+galname+'.bar.pie.coha.png'

     tt=sfco_str[where(sfco_str.gal eq do_gals[i])].res_pc
     tt=tt(sort(tt))
     reslist=tt(uniq(tt))
     
     usedata=where(sfco_str.tagy eq 'hasfr' and sfco_str.gal eq galname and sfco_str.perc eq use_perc,count)

     tmps=sfco_str[usedata]
      
     co1=tmps.FRAC_P1_CO_ONLY_BYFLUX
     co2=tmps.FRAC_P2_CO_ONLY_BYFLUX
     cosfr1=tmps.FRAC_P1_BOTH_BYFLUX
     cosfr2=tmps.FRAC_P2_BOTH_BYFLUX
     cosfr3=tmps.FRAC_P1_SFR_P2_CO_BYFLUX
     cosfr4=tmps.FRAC_P1_CO_P2_SFR_BYFLUX
     sfr2=tmps.FRAC_P2_SFR_ONLY_BYFLUX
     sfr1=tmps.FRAC_P1_SFR_ONLY_BYFLUX

      
      cgPS_open,psfile,/encapsulated, xsize=10, ysize=7
      res_kpc = 0.1*round(reslist/100.)
      barnames=strtrim(string(res_kpc,Format='(D3.1)'),2)
      tit='Gas-SF Cycling for '+galname
      ytit='Linear Resolution [kpc]'

      cgBarPlot, co1, XRANGE=[0, 1], COLORS=cgcolor('dodgerblue'), BARNAMES=barnames,/rotate,charthick=1.5,xtit='Fraction',tit=tit,position=[0.15,0.15,0.68,0.85]
      xyouts,0.08,0.28,ytit,/norm,orientation=90.
      cgBarplot, co2, /OVERPLOT, BASELINE=co1, COLORS=cgcolor('royalblue'),/rotate
      cgBarplot, cosfr1, /OVERPLOT, BASELINE=co1+co2, COLORS=cgcolor('plum'),/rotate
      cgBarplot, cosfr2, /OVERPLOT, BASELINE=co1+co2+cosfr1, COLORS=cgcolor('magenta'),/rotate
      cgBarplot, cosfr3, /OVERPLOT, BASELINE=co1+co2+cosfr1+cosfr2, COLORS=cgcolor('purple'),/rotate
      cgBarplot, cosfr4, /OVERPLOT, BASELINE=co1+co2+cosfr1+cosfr2+cosfr3, COLORS=cgcolor('maroon'),/rotate
      cgBarplot, sfr2, /OVERPLOT, BASELINE=co1+co2+cosfr1+cosfr2+cosfr3+cosfr4, COLORS=cgcolor('red'),/rotate
      cgBarplot, sfr1, /OVERPLOT, BASELINE=co1+co2+cosfr1+cosfr2+cosfr3+cosfr4+sfr2, COLORS=cgcolor('salmon'),/rotate

      al_legend $
         , lines=-99, position=[0.7,0.85], /norm, box=1, outline=cgcolor('black') $
         , background=cgcolor('lightgray') $
         , textcolor=[cgcolor('black') $
                      , cgcolor('dodgerblue') $
                      , cgcolor('royalblue') $
                      , cgcolor('plum') $
                      , cgcolor('magenta') $
                      , cgcolor('purple') $
                      , cgcolor('maroon') $
                      , cgcolor('red') $
                      , cgcolor('salmon')] $
         , ['Lines of Sight:' $
            , 'CO only' $
            , 'Bright CO only' $
            , 'Overlap' $
            , 'Bright Overlap' $
            , 'Bright CO, Faint SFR' $
            , 'Bright SFR, Faint CO' $
            , 'Bright SFR only' $
            , 'SFR only'] $
         , charthick=1.5, charsize=1.5
      cgPS_close
      
      if keyword_set(show) then spawn,'gv '+psfile+' &'
      
  end
  
  stop


  
; CGBARPLOT OF STAR-GAS CYCLING VERSUS RESOLUTION FOR EACH GALAXY

  xx=where(sfco_str.tagy eq 'hiisfr')
  galidx=rem_dup(sfco_str[xx].gal)
  ngals=n_elements(galidx)
  do_gals=sfco_str[xx[galidx]].gal
  cols=range_gen(ngals,[30,255-30])

  stop
  
  for i=0,ngals-1 do begin

     galname=do_gals[i]
     psfile=use_plot_dir+galname+'.bar.pie.cohii.eps'
     pngfile=use_plot_dir+galname+'.bar.pie.cohii.png'

     tt=sfco_str[where(sfco_str.gal eq do_gals[i])].res_pc
     tt=tt(sort(tt))
     reslist=tt(uniq(tt))
     
     usedata=where(sfco_str.tagy eq 'hiisfr' and sfco_str.gal eq galname and sfco_str.perc eq use_perc,count)

     tmps=sfco_str[usedata]
      
     co1=tmps.FRAC_P1_CO_ONLY_BYFLUX
     co2=tmps.FRAC_P2_CO_ONLY_BYFLUX
     cosfr1=tmps.FRAC_P1_BOTH_BYFLUX
     cosfr2=tmps.FRAC_P2_BOTH_BYFLUX
     cosfr3=tmps.FRAC_P1_SFR_P2_CO_BYFLUX
     cosfr4=tmps.FRAC_P1_CO_P2_SFR_BYFLUX
     sfr2=tmps.FRAC_P2_SFR_ONLY_BYFLUX
     sfr1=tmps.FRAC_P1_SFR_ONLY_BYFLUX

      
      cgPS_open,psfile,/encapsulated, xsize=10, ysize=7
      res_kpc = 0.1*round(reslist/100.)
      barnames=strtrim(string(res_kpc,Format='(D3.1)'),2)
      tit='Gas-SF Cycling for '+galname
      ytit='Linear Resolution [kpc]'

      cgBarPlot, co1, XRANGE=[0, 1], COLORS=cgcolor('dodgerblue'), BARNAMES=barnames,/rotate,charthick=1.5,xtit='Fraction',tit=tit,position=[0.15,0.15,0.68,0.85]
      xyouts,0.08,0.28,ytit,/norm,orientation=90.
      cgBarplot, co2, /OVERPLOT, BASELINE=co1, COLORS=cgcolor('royalblue'),/rotate
      cgBarplot, cosfr1, /OVERPLOT, BASELINE=co1+co2, COLORS=cgcolor('plum'),/rotate
      cgBarplot, cosfr2, /OVERPLOT, BASELINE=co1+co2+cosfr1, COLORS=cgcolor('magenta'),/rotate
      cgBarplot, cosfr3, /OVERPLOT, BASELINE=co1+co2+cosfr1+cosfr2, COLORS=cgcolor('purple'),/rotate
      cgBarplot, cosfr4, /OVERPLOT, BASELINE=co1+co2+cosfr1+cosfr2+cosfr3, COLORS=cgcolor('maroon'),/rotate
      cgBarplot, sfr2, /OVERPLOT, BASELINE=co1+co2+cosfr1+cosfr2+cosfr3+cosfr4, COLORS=cgcolor('red'),/rotate
      cgBarplot, sfr1, /OVERPLOT, BASELINE=co1+co2+cosfr1+cosfr2+cosfr3+cosfr4+sfr2, COLORS=cgcolor('salmon'),/rotate

      al_legend $
         , lines=-99, position=[0.7,0.85], /norm, box=1, outline=cgcolor('black') $
         , background=cgcolor('lightgray') $
         , textcolor=[cgcolor('black') $
                      , cgcolor('dodgerblue') $
                      , cgcolor('royalblue') $
                      , cgcolor('plum') $
                      , cgcolor('magenta') $
                      , cgcolor('purple') $
                      , cgcolor('maroon') $
                      , cgcolor('red') $
                      , cgcolor('salmon')] $
         , ['Lines of Sight:' $
            , 'CO only' $
            , 'Bright CO only' $
            , 'Overlap' $
            , 'Bright Overlap' $
            , 'Bright CO, Faint SFR' $
            , 'Bright SFR, Faint CO' $
            , 'Bright SFR only' $
            , 'SFR only'] $
         , charthick=1.5, charsize=1.5
      cgPS_close
      
      if keyword_set(show) then spawn,'gv '+psfile+' &'
      
  end
  
  stop


the_end:
  
end

