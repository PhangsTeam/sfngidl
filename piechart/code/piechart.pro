function piechart, covec=covec $
                   , sfrvec=sfrvec $
                   , comap=comap $
                   , sfrmap=sfrmap $
                   , psname=psname $
                   , verbose=verbose $
                   , show=show $
                   , nostop=nostop $
                   , tagco=tagco $
                   , tagsfr=tagsfr $
                   , thresh=thresh
  

;+
;
; "Pie chart" of SFR tracer and CO coverage within a map.
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS and PROCESS INPUTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; use_thresh values should be between
; 1 and 99, meaning pixel value
; corresponding to 1%/99% of flux

  use_thresh=[-1,-1]            ; default [-1,-1] means min,max
  use_p1str='min'
  use_p2str='max'
  if keyword_set(thresh) then use_thresh=thresh

  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; Work out overlaps at different threshold levels
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
  sort_co = covec[sort(covec)]
  sort_sfr = sfrvec[sort(sfrvec)]
  cdf_co = total(sort_co,/cumul)/total(covec)
  cdf_sfr = total(sort_sfr,/cumul)/total(sfrvec)

    if use_thresh[0] ge 100 then begin
       use_thresh[0] = -1
       print,'Using data minimum for CO and SFR threshold P1'
    end
   if use_thresh[1] ge 100 then begin
       use_thresh[1] = -1
       print,'Using data maximum for CO and SFR threshold P2'
    end
    
  if use_thresh[0] eq -1 then begin
     sfr_thresh_1=sort_sfr[0]
     co_thresh_1=sort_co[0]
     use_p1str="min"
  end else begin
     sfr_thresh_1=interpol(sort_sfr,cdf_sfr,use_thresh[0]/100.)
     co_thresh_1=interpol(sort_co,cdf_co,use_thresh[0]/100.)
     use_p1str=strtrim(string(fix(use_thresh[0])),2)
  end

  if use_thresh[1] eq -1 then begin
     sfr_thresh_2=sort_sfr[-1]
     co_thresh_2=sort_co[-1]
     use_p2str="max"
  end else begin
     sfr_thresh_2=interpol(sort_sfr,cdf_sfr,use_thresh[1]/100.)
     co_thresh_2=interpol(sort_co,cdf_co,use_thresh[1]/100.)
     use_p2str=strtrim(string(fix(use_thresh[1])),2)
  end
  
  p1_sfr = (sfrmap ge sfr_thresh_1 and finite(sfrmap))
  p1_co = (comap ge co_thresh_1 and finite(comap))
  p1_any = (p1_sfr or p1_co)
  p1_both = (p1_sfr and p1_co)
  ct_p1 = total(p1_any,/nan)

  p2_sfr = (sfrmap ge sfr_thresh_2 and finite(sfrmap))
  p2_co = (comap ge co_thresh_2 and finite(comap))
  p2_any = (p2_sfr or p2_co)
  p2_both = (p2_sfr and p2_co)
  ct_p2 = total(p2_any,/nan)

  frac_p1_sfr_only = total(p1_sfr and not p1_co and $
                              not p2_sfr,/nan)/(ct_p1*1.0)

  frac_p2_sfr_only = total(p2_sfr and not p1_co $
                          ,/nan)/(ct_p1*1.0)

  frac_p1_co_only = total(p1_co and not p1_sfr $
                              and not p2_co,/nan)/(ct_p1*1.0)

  frac_p2_co_only = total(p2_co and not p1_sfr $
                               ,/nan)/(ct_p1*1.0)

  frac_p1_both = total(p1_both and not p2_co $
                           and not p2_sfr,/nan)/(ct_p1*1.0)

  frac_p1_both_p2_co = $
     total(p1_both and p2_co and not p2_sfr,/nan)/(ct_p1*1.0)

  frac_p1_both_p2_sfr = $
     total(p1_both and p2_sfr and not p2_co,/nan)/ct_p1*1.0

  frac_p2_both = $
     total(p1_both and p2_sfr and p2_co,/nan)/ct_p1*1.0

  if keyword_set(verbose) then print, "This should sum to 1: ", total(frac_p1_sfr_only+frac_p1_co_only $
                                                                      +frac_p2_sfr_only+frac_p2_co_only $
                                                                      +frac_p1_both+frac_p2_both $
                                                                      +frac_p1_both_p2_co $
                                                                      +frac_p1_both_p2_sfr,/nan)
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; Generate pie chart plots
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
  ctrx = 0
  ctry = 0
  theta = findgen(1100)/1000.*!pi*2.
  x = cos(theta)
  y = sin(theta)
  
  psfile = '../plots/'+psname
  ps, /ps, /def, xsize=10, ysize=10, /color, /encapsulated, file=psfile
  loadct, 0
  
  plot, x, y, xstyle=4, ystyle=4 $
        , color=cgcolor('black',255), /nodata $
        , xrange=[-1.1,2.0], yrange=[-1.1,1.1], /iso $
        , xmargin=[0,0], ymargin=[0,0],title=psname $
        , charthick=4, charsize=1.5
 
  oplot, x, y, thick=10, color=cgcolor('black')
  
                                ; first loop does coloured wedges, second loop
                                ; does borders of wedges in black
  for i = 0, 1 do begin

     ; Bright SFR (sfr>sfr_thresh2, no gas tracer above co_thresh1)
     theta0 = 0.0
     theta1 = theta0+frac_p2_sfr_only*2.*!pi
     theta_ind = where(theta gt theta0 and theta le theta1,tct)
     if tct gt 0 then begin
        if i eq 0 then $
           polyfill, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
                     , color=cgcolor('firebrick') $
        else $
           oplot, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
                  , thick=5, color=cgcolor('black')
     end
     
     ; SFR (no gas tracer above co_thresh1)
     theta0 = theta1
     theta1 = theta0+frac_p1_sfr_only*2.*!pi
     theta_ind = where(theta gt theta0 and theta le theta1,tct)
     if tct gt 0 then begin
     if i eq 0 then $
        polyfill, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
                  , color=cgcolor('salmon') $
     else $
        oplot, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
               , thick=5, color=cgcolor('black')
     end

     ; CO (no SFR tracer above sfr_thresh1)
     theta0 = theta1
     theta1 = theta0+(frac_p1_co_only)*(2.*!pi)
     theta_ind = where(theta gt theta0 and theta le theta1,tct)
     if tct gt 0 then begin
     if i eq 0 then $
        polyfill, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
                  , color=cgcolor('dodgerblue') $
     else $
        oplot, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
               , thick=5, color=cgcolor('black')
     end

     ; Bright CO (co>co_thresh2, no SFR tracer above sfr_thresh1)
     theta0 = theta1
     theta1 = theta0+(frac_p2_co_only)*(2.*!pi)
     theta_ind = where(theta gt theta0 and theta le theta1,tct)
      if tct gt 0 then begin
    if i eq 0 then $
        polyfill, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
                  , color=cgcolor('royalblue') $
     else $
        oplot, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
               , thick=5, color=cgcolor('black')
end

; faint CO and SFR (co>co_thresh1, sfr>sfr_thresh1 but both < thresh2)
     theta0 = theta1
     theta1 = theta0+(frac_p1_both)*(2.*!pi)
     theta_ind = where(theta gt theta0 and theta le theta1,tct)
      if tct gt 0 then begin
    if i eq 0 then $
        polyfill, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
                  , color=cgcolor('plum') $
     else $
        oplot, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
               , thick=5, color=cgcolor('black')
end

     ; bright CO and SFR (co>co_thresh2, sfr>sfr_thresh2)

     theta0 = theta1
     theta1 = theta0+(frac_p2_both)*2.*!pi
     theta_ind = where(theta gt theta0 and theta le theta1,tct)
       if tct gt 0 then begin
   if i eq 0 then $
        polyfill, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
                  , color=cgcolor('magenta') $
     else $
        oplot, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
               , thick=5, color=cgcolor('black')
end


     ; bright CO and some SFR (co>co_thresh2, sfr>sfr_thresh1 but sfr < thresh2)

     theta0 = theta1
     theta1 = theta0+(frac_p1_both_p2_co)*2.*!pi
     theta_ind = where(theta gt theta0 and theta le theta1,tct)
       if tct gt 0 then begin
   if i eq 0 then $
        polyfill, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
                  , color=cgcolor('Purple') $
     else $
        oplot, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
               , thick=5, color=cgcolor('black')
end


     ; faint CO and bright SFR (co>co_thresh1, but co <thresh2, sfr>sfr_thresh2)

     theta0 = theta1
     theta1 = theta0+(frac_p1_both_p2_sfr)*2.*!pi
     theta_ind = where(theta gt theta0 and theta le theta1,tct)
       if tct gt 0 then begin
   if i eq 0 then $
        polyfill, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
                  , color=cgcolor('Maroon') $
     else $
        oplot, [ctrx, x[theta_ind], ctrx], [ctry, y[theta_ind], ctry] $
               , thick=5, color=cgcolor('black')
end
       


  endfor

  !p.charthick=5
  al_legend $
     , lines=-99, /top, /left, box=1, outline=cgcolor('black') $
     , background=cgcolor('lightgray') $
     , textcolor=[cgcolor('black') $
                  , cgcolor('salmon') $
                  , cgcolor('firebrick') $
                  , cgcolor('dodgerblue') $
                  , cgcolor('royalblue') ] $
     , ['Lines of Sight [%]:' $
        , 'SFR only: '+sigfig((round(frac_p1_sfr_only*10000.)/100.),2) $
        , 'Bright SFR only: '+sigfig((round(frac_p2_sfr_only*10000.)/100.),2) $
        , 'CO only: '+sigfig((round(frac_p1_co_only*10000.)/100.),2) $
        , 'Bright CO only: '+sigfig((round(frac_p2_co_only*10000.)/100.),2) $
     ], charthick=4, charsize=1.5

    al_legend $
     , lines=-99, /top, /right, box=1, outline=cgcolor('black') $
     , background=cgcolor('lightgray') $
     , textcolor=[cgcolor('black') $
                  , cgcolor('plum') $
                  , cgcolor('magenta') $
                  , cgcolor('Purple') $
                  , cgcolor('Maroon')] $
     , ['Lines of Sight [%]:' $
        , 'Overlap: '+sigfig((round(frac_p1_both*10000.)/100.),2) $
        , 'Bright Overlap: '+sigfig((round(frac_p2_both*10000.)/100.),2) $
        , 'Bright CO, Faint SFR: '+sigfig((round(frac_p1_both_p2_co*10000.)/100.),2) $
        , 'Bright SFR, Faint CO: '+sigfig((round(frac_p1_both_p2_sfr*10000.)/100.),2) $
     ], charthick=4, charsize=1.5

   
  al_legend, /bottom, /left, box=0, clear=0 $
             , ["!6Gas: "+tagco, $
                "!6SFR: "+tagsfr, $
                "!6P1: "+use_p1str, $
                "!6P2: "+use_p2str] $
             , lines=-99 $
             , textcolor=255, charsize=1.8, charthick=4

    al_legend, /bottom, /right, box=0, clear=0 $
             , ["!6Gas Threshold: "+sigfig(co_thresh_1,2), $
                "!6SFR Threshold: "+sigfig(sfr_thresh_1,2,/sci), $
                "!6Bright Gas Threshold: "+sigfig(co_thresh_2,2), $
                "!6Bright SFR Threshold: "+sigfig(sfr_thresh_2,2,/sci)] $
             , lines=-99 $
             , textcolor=255, charsize=1.8, charthick=4

  ps, /xw

  if keyword_set(show) then   spawn, 'gv '+psfile+' &'
  if not keyword_set(nostop) then stop

  return,[frac_p1_sfr_only $
          ,frac_p1_co_only $
          ,frac_p2_sfr_only $
          ,frac_p2_co_only $
          ,frac_p1_both $
          ,frac_p2_both $
          ,frac_p1_both_p2_co $
          ,frac_p1_both_p2_sfr $
          ,sfr_thresh_1 $
          ,sfr_thresh_2 $
          ,co_thresh_1 $
          ,co_thresh_2 $
         ]

  
  
end
