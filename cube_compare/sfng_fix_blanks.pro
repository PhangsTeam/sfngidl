FUNCTION sfng_fix_blankval,im,inhdr=inhdr,outhdr=outhdr,in_blankval=in_blankval,out_blankval=out_blankval,verbose=verbose,help=help,undefined=undefined

; NAME:
;     sfng_fix_blankval
; CALLING SEQUENCE:
;     im=sfng_fix_blankval(im,[inhdr=inhdr],[outhdr=outhdr],[/reverse],[/help],[/verbose],[/undefined])
; PURPOSE:
;     changes the blanking value used for an image/cube
; INPUTS:
;     im = vector/image array/cube
; OPTIONAL INPUT:
;     inhdr = header of FITS file (if you want to add note in HISTORY)
;     outhdr = modified FITS header 
;     in_blankval = blankval that you want to change. If not provided,
;     code will look for it in the inhdr.
;     ****NB. IN_BLANKVAL CANNOT BE SPECIFIED AS NAN. USE /UNDEFINED
;     KEYWORD TO TRAP NAN VALUES INSTEAD ****
;     out_blankval = value that you want to use as a blank (Default is !values.f_nan)
; ACCEPTED KEY-WORDS:
;     help = print this help
;     verbose = print extra information to screen
;     undefined = changes all non-finite values in data to out_blankval
; EXAMPLES
;     im=sfng_fix_blankval(im,/undefined,out_blankval=-32678.)
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     
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

  
  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_fix_blankval'
     imout=0
     if keyword_set(outhdr) then outhdr=0
     goto,the_end
  ENDIF
  
  imout=im
  if keyword_set(inhdr) then outhdr=inhdr

  if not keyword_set(inhdr) and not keyword_set(in_blankval) and not keyword_set(undefined) then begin
     message,'No information about blanking value to be modified. Exiting without changing data.',/info
     goto, the_end
  end
  
  if not keyword_set(in_blankval) and keyword_set(inhdr) and not keyword_set(undefined) then begin
     in_blankval=sxpar(inhdr,'BLANK',count=ct)
     if ct eq 0 then begin
        message,'No BLANK value not found in hdr. Exiting without changing data.',/info
        goto, the_end
     end 
  end

  if not keyword_set(out_blankval) then begin
     out_blankval=!values.f_nan
     out_blankval_str='IDL fNaN'
  end else begin
     out_blankval_str=strtrim(string(out_blankval),2)
  end
  
  if ct gt 0 or keyword_set(in_blankval) then begin
        bpix=where(data eq in_blankval,bct)
        if bct gt 0 then begin
           if keyword_set(verbose) then print,'Changing old blanking value:'+strtrim(string(in_blankval),2)
           data[bpix]=out_blankval
           if keyword_set(verbose) then print,'Setting new blank value to '+out_blankval_str
           sxaddpar,outhdr,'HISTORY','Set blank values to '+out_blankval_str
           sxaddpar,outhdr,'BLANK',out_blankval_str
        end           
     end
  end

  if keyword_set(undefined) then begin
        bpix=where(finite(data) eq 0,bct)
        if bct gt 0 then begin
           if keyword_set(verbose) then print,'Changing all undefined values'
           data[bpix]=out_blankval
           if keyword_set(verbose) then print,'Setting new blank value to '+out_blankval_str
           sxaddpar,outhdr,'HISTORY','Set blank values to '+out_blankval_str
           sxaddpar,outhdr,'BLANK',out_blankval_str
        end           
     end
  end


  
the_end:

  RETURN,imout

END
