FUNCTION sfng_convert_cube_jy2k,in=in,hdr_in=hdr_in,hdr_out=hdr_out $
                                ,frequency=frequency,wavelength=wavelength $
                                ,factor=factor,reverse=reverse,help=help
  
; NAME:
;     sfng_convert_cube_jy2k
; CALLING SEQUENCE:
;     newcube=sfng_convert_cube_jy2k(in=cube,hdr_in=hdr_in,hdr_out=hdr_out,[factor=factor],[/reverse],[/help])
; PURPOSE:
;     scales data by factor corresponding to conversion from Jy/beam
;     to Kelvin (or vice-versa if /reverse keyword set) and updates
;     header information
; INPUTS:
;     in = vector/array/cube
;     hdr_in = header of FITS file 
;     hdr_out = modified FITS header 
; OPTIONAL INPUT:
;     factor = return the numerical factor that was used for the conversion
;     frequency = line frequency in GHz (required if wavelength not given and
;                 RESTFREQ not specified in header). Takes precedence
;                 over wavelength if both are specified. Default is
;                 230.598GHz, i.e. 12CO(2-1)
;     wavelength = line wavelength in millimetres (required if frequency not given and
;                 RESTFREQ not specified in header)
; ACCEPTED KEY-WORDS:
;     reverse = convert K to Jy/beam
;     help = print this help
; EXAMPLES
;     cube_K=sfng_convert_cube_jy2k(in=cube,hdr_in=hdr,hdr_out=hdr_K,factor=fact)
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     Goddard Astron libraries
;     jy2k.pro
; COMMONS:
;     
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;    written 25-11-2016 by AH
; COMMENTS AND TO DO LIST:
;    Minimally tested!
;    Error trapping not implemented.
;-

  @sfng_constants.bat
  
  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_convert_cube_jy2k'
     out=0
     hdr_out=0
     goto,the_end
  ENDIF

  use_frequency = restfreq_12co21/1.e9
  hdr_out=hdr_in

  if keyword_set(wavelength) then use_frequency=(c_ms/(1.e-3*wavelength))/1.e9
  if keyword_set(frequency) then use_frequency=frequency
  
  if not keyword_set(frequency) and not keyword_set(wavelength) then begin
     restfreq = sxpar(hdr_in,'RESTFREQ',count=freq_ct)
     if freq_ct gt 0 then use_frequency = restfreq
  end

  bmaj=sxpar(hdr_in,'BMAJ',count=bmaj_ct)
  bmin=sxpar(hdr_in,'BMIN',count=bmin_ct)

  if bmaj_ct eq 0 then begin
     message,'No beam information in header. Exiting'
     out=0 & hdr_out=0
     goto, the_end
  end

    if bmin_ct eq 0 and bmaj_ct eq 1 then begin
     message,'Assuming BMIN=BMAJ'
     bmin=bmaj
     sxaddpar,hdr_out,'BMIN',bmin,'Assuming same as BMAJ'
  end

  lambda=1.e3*c_ms/(use_frequency*1.e9)
  
  factor=jy2k(bmaj=bmaj*3600.,bmin=bmin*3600.,lambda=lambda)
  factor_str=strtrim(string(factor),2)
  
  if not keyword_set(reverse) then begin
     out=in*factor
     sxaddpar,hdr_out,'HISTORY','Converted from Jy/beam to K (Tmb) using factor: '+factor_str
     sxaddpar,hdr_out,'RESTFREQ',use_frequency
     sxaddpar,hdr_out,'BUNIT','K'
  endif else begin
     out=in/factor
     sxaddpar,hdr_out,'HISTORY','Converted from K (Tmb) to Jy/beam using factor: '+factor_str
     sxaddpar,hdr_out,'BUNIT','JY/BEAM'
     sxaddpar,hdr_out,'RESTFREQ',use_frequency
  end
  
the_end:

  RETURN,out

END
