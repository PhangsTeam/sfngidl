FUNCTION sfng_fix_nans,im,inhdr=inhdr,outhdr=outhdr,reverse=reverse,help=help,verbose=verbose

; NAME:
;     sfng_fix_nans
; CALLING SEQUENCE:
;     im=sfng_fix_nans(im,[inhdr=inhdr],[outhdr=outhdr],[/reverse],[/help])
; PURPOSE:
;     sets Inf and NaNs to la_undef() or vice-versa (using /reverse keyword)
; INPUTS:
;     im = vector/array/cube
; OPTIONAL INPUT:
;     inhdr = header of FITS file (if you want to add note in HISTORY)
;     outhdr = modified FITS header 
; ACCEPTED KEY-WORDS:
;     reverse = convert la_undef()=-32678 and Inf to !values.f_nan
;     help = print this help
;     verbose = print extra information to screen
; EXAMPLES
;     im=sfng_fix_nans(im,/reverse)
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     
; COMMONS:
;     needs LA common block initialised
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;    written 25-11-2016 by AH
; COMMENTS AND TO DO LIST:
;    Minimally tested!
;    Error trapping not implemented.
;-

  
  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_fix_nans'
     imout=0
     if keyword_set(outhdr) then outhdr=0
     goto,the_end
  ENDIF
  
  define_la_common

  imout=im
  if keyword_set(inhdr) then outhdr=inhdr
  
  ind_bad=where(finite(im) eq 0,count_bad)
  IF count_bad NE 0 THEN BEGIN
     imout(ind_bad)=la_undef()
     if keyword_set(verbose) then $
        print,'Found undefined values and setting them to LA_UNDEF()'
     IF keyword_set(inhdr) then $
        sxaddpar,outhdr,'HISTORY','Set undefined values to LA_UNDEF()'
  END
  
  if keyword_set(reverse) then begin
     ind_bad=where(finite(imout) eq la_undef(),count_bad)
     IF count_bad NE 0 THEN BEGIN
        if keyword_set(verbose) then $
        print,'Found LA_UNDEF() values and setting them to IDL fNaN'
        imout(ind_bad)=!values.f_nan
        IF keyword_set(inhdr) then $
        sxaddpar,outhdr,'HISTORY','Set LA_UNDEF() values to !Values.F_NAN'
     END
  end
  
the_end:

  RETURN,imout

END
