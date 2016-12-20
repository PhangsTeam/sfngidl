function image_overlap $
   , co=co $
   , sfr=sfr $
   , mask=mask $
   , hdr=hdr $
   , psname=psname $
   , verbose=verbose $
   , nostop=nostop $
   , show=show $
   , tagco=tagco $
   , tagsfr=tagsfr $
   , nozeroes=nozeroes

;+
;
; Plot a contour image showing the overlap of two images.
; We assume that all the relevant cuts to the input vectors
; have been done prior to calling this routine.
;
; the only exception is that the nozeroes keyword will blank pixels
; zeroes. If you have positive and negative values in the input maps,
; using this keyword will do strange things.
;
; In general, pixels that should not be counted should be set to NaN in the input.
;                                                 
;-
  

; DEFINE REGIONS 
  sfr_map = ((finite(sfr) eq 1) and mask eq 1)
  co_map = ((finite(co) eq 1) and mask eq 1)
  sfr_only_map = ((finite(sfr) eq 1) and (finite(co) eq 0) and mask eq 1)
  co_only_map = ((finite(co) eq 1) and (finite(sfr) eq 0) and mask eq 1)
  co_and_sfr_map = ((finite(co) eq 1) and (finite(sfr) eq 1) and mask eq 1)
  co_or_sfr_map = ((finite(co) eq 1) or (finite(sfr) eq 1) and mask eq 1)
  no_co_no_sfr_map = ((finite(co) eq 0) and (finite(sfr) eq 0) and mask eq 1) 

  if keyword_set(nozeroes) then begin
     sfr_map = ((finite(sfr) eq 1) and mask eq 1 and sfr ne 0.)
     co_map = ((finite(co) eq 1) and mask eq 1 and co ne 0.)
     sfr_only_map = ((finite(sfr) eq 1) and (finite(co) eq 0) and mask eq 1 and sfr ne 0.)
     co_only_map = ((finite(co) eq 1) and (finite(sfr) eq 0) and mask eq 1 and co ne 0.)
     co_and_sfr_map = ((finite(co) eq 1) and (finite(sfr) eq 1) and mask eq 1 and sfr ne 0. and co ne 0.)
     co_or_sfr_map = ((finite(co) eq 1) or (finite(sfr) eq 1) and mask eq 1 and sfr ne 0. and co ne 0.)
     no_co_no_sfr_map = ((finite(co) eq 0 or co eq 0.) and (finite(sfr) eq 0 or sfr eq 0.) and mask eq 1) 
  end
  
; PARAMETERS
  sfr_only_pix = total(sfr_only_map)
  co_only_pix = total(co_only_map)
  co_and_sfr_pix = total(co_and_sfr_map)
  co_or_sfr_pix = total(co_or_sfr_map)
  no_co_no_sfr_pix = total(no_co_no_sfr_map)
  sfr_pix = total(sfr_map)
  co_pix = total(co_map)
  map_pix = sfr_only_pix+co_only_pix+co_and_sfr_pix+no_co_no_sfr_pix
  emission_pix = sfr_only_pix+co_only_pix+co_and_sfr_pix

  overlap = co_and_sfr_pix/float(emission_pix)
  sfr_onlyfrac = sfr_only_pix/float(emission_pix)
  co_onlyfrac = co_only_pix/float(emission_pix)
  sfr_frac = sfr_pix/float(emission_pix)
  co_frac = co_pix/float(emission_pix)

  no_co_no_sfr_mfrac = no_co_no_sfr_pix/float(map_pix)
  co_and_sfr_mfrac = co_and_sfr_pix/float(map_pix)
  co_or_sfr_mfrac = co_or_sfr_pix/float(map_pix)

  if keyword_set(verbose) then begin
     print,"This should equal 1: ",sfr_onlyfrac+co_onlyfrac+overlap
     print,"This should equal 1: ",no_co_no_sfr_mfrac+co_or_sfr_mfrac
  end
  
; PLOT
  make_axes, hdr, ra=ra, da=da             
  loadct, 0
  reversect  

  dummy = cgcolor("black", 255)
  dummy = cgcolor("firebrick", 1)
  dummy = cgcolor("royalblue", 2)
  dummy = cgcolor("purple", 3)
  disp_map = sfr_map*0B
  disp_map[where(sfr_map)] = 1
  disp_map[where(co_map)] = 2
  disp_map[where(co_and_sfr_map)] = 3

  psfile = '../plots/'+psname
  if not keyword_set(tagextra) then tagextra = " "
  if not keyword_set(tagco) then tagco = "CO"
  if not keyword_set(tagsfr) then tagsfr = "SFR"
  
  ps, /ps, /def, xsize=10, ysize=10, /color, /encapsulated, file=psfile
;  fgcolor=0
;  loadct,0

  disp, disp_map $
        , ra, da $
        , xtitle="Right Ascension (J2000)" $
        , ytitle="Declination (J2000)" $
        , /sq, /radec, xthick=3, ythick=3 $
        , min=0, max=255, color=255 $
        , charsize=1.3, charthick=4 $
        , title=psname $
        , position=[0.1,0.1,0.8,0.8]

  al_legend, /bottom, /left, box=1, clear=0 $
            ,  outline=cgcolor('black') $
             , background=cgcolor('lightgray') $
             , ["!6Gas: "+tagco, $
                "!6SFR: "+tagsfr, $
                "!6Overlap = "+sigfig(overlap,2)] $
             , lines=-99 $
             , textcolor=[cgcolor('royalblue'),cgcolor('firebrick'),cgcolor('purple')] $
             , charsize=1.2, charthick=4

    ps,/xw

    if keyword_set(show) then spawn, 'gv '+psfile+' &'
    if not keyword_set(nostop) then stop
    
; RETURN SOME VALUES AND EXIT
  
    return, [overlap,sfr_onlyfrac,co_onlyfrac,sfr_frac,co_frac $
             ,no_co_no_sfr_mfrac,co_and_sfr_mfrac,co_or_sfr_mfrac]

 end
