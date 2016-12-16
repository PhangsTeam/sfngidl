pro sfng_correct_30m_velref,infile=infile, $
                            ,outfile=outfile, $
                            ,verbose=verbose $
                            ,help=help

; NAME:
;     sfng_correct_30m_velref
; CALLING SEQUENCE:
;     sfng_correct_30m_velref,infile='30m.fits',outfile='30m.vcorr.fits',[/verbose],[/help]
; PURPOSE:
;     corrects reference velocity mis-labelling in header of IRAM 30m data according
;     to A. Usero's formula: v_opt-v_rad=c*(v_opt/c)^2/(1+v_opt/c)
; INPUTS:
;     infile = name of 30m FITS file
; OPTIONAL INPUT:
;     None
; ACCEPTED KEY-WORDS:
;     verbose = print out the conversion between Vopt and Vrad
;     help = print this help
; EXAMPLES
;     sfng_correct_30m_velref,infile='30m.fits',outfile='30m.vcorr.fits',/verbose
; OUTPUTS:
;     outfile = name of 30m FITS file after correcting header
; PROCEDURES AND SUBROUTINES USED
;     sfng_constants.bat
;     Goddard Astron routines
; COMMONS:
;     None.
; SIDE EFFECTS:
;     None.
; MODIFICATION HISTORY:
;     written 22-10-2016 by AH
; COMMENTS AND TO DO LIST:
;     Minimally tested!
;     No error trapping implemented.
;-

  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_correct_30m_velref'
     goto,the_end
  ENDIF

  @sfng_constants.bat

  data=readfits(infile,hdr)

  crval3=sxpar(hdr,'CRVAL3',count=ct)
  if ct eq 0 then begin
     message,'Velocity reference value (CRVAL3) not found in header',/info
     goto,the_end
  end
  
  vopt=sxpar(hdr,'VELO-LSR',count=ct)
  if ct eq 0 then begin
     message,'VELO-LSR not found in header',/info
     goto,the_end
  end

  if vopt ne crval3 then begin
     message,'Velocity reference value (CRVAL3) is not the same as VELO-LSR',/info
      goto,the_end
  end

  vrad=vopt-((c_ms*(vopt/c_ms)^2)/(1+vopt/c_ms))
  if keyword_set(verbose) then print,"Input Vopt, Output Vrad ",vopt, vrad

  sxaddpar,hdr,'VELO-LSR',vrad
  sxaddpar,hdr,'CRVAL3',vrad
  sxaddpar,hdr,'HISTORY','VELO-LSR as recorded by observatory: '+sigfig(vopt,5,/sci)
  sxaddpar,hdr,'HISTORY','Modified velo-lsr using A. Usero correction:'
  sxaddpar,hdr,'HISTORY','v_opt-v_rad=c*(v_opt/c)^2/(1+v_opt/c)'
  sxaddpar,hdr,'HISTORY','CRVAL3 as recorded by observatory: '+sigfig(crval3,5,/sci)
  sxaddpar,hdr,'HISTORY','Modified crval3 to match new VELO-LSR'
  
  writefits,outfile+'.fits',data,hdr

  the_end:
  
end
