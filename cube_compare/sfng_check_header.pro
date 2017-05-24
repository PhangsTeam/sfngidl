function sfng_check_header, hdr = hdr $
                            , comments = comments $
                            , fixhdr = fixhdr $
                            , pixscale = pixscale $
                            , beam = beam $
                            , bunit = bunit $
                            , chanw = chanw $
                            , unit = unit $
                            , help = help

;+  NAME:
;     sfng_check_header
; CALLING SEQUENCE:
;     pass=sfng_check_header(hdr=hdr,[comments=comments,fixhdr=fixhdr,pixscale=pixscale,beam=beam,bunit=bunit,chanw=chanw])
; PURPOSE:
;     checks to see if FITS cubes have header information that we need
;     to do comparison
; INPUTS:
;     hdr = a FITS header
; OPTIONAL INPUT:
;     fixhdr = modified output header. If present, sfng_check_header will try
;              to correct common problems
;     pixscale = pixscale in arcsecs, calculated from cdelt1 (or CD
;                matrix if cdelt1 not present)
;     beam     = [bmaj,bmin,bpa]
;     chanw    = channel width  
;     comments = summary of header information, attempted fixes etc.
; ACCEPTED KEY-WORDS:
;     help = print this help
; EXAMPLES
;       pass=sfng_check_header(hdr=c1hdr, fixhdr=c1hdr_fix, comments = $
;                              c1_comments, beam=beam, $
;                              pixscale=pixscale, chanw=chanw)
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     Goddard Astron routines
; COMMONS:
;     
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;    written 25-11-2016 by AH, based on AKL's cprops_check_header
; COMMENTS AND TO DO LIST:
;    Minimally tested!
;    Error trapping not implemented.
;-


  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_check_header'
     pass=0
     goto,the_end
  ENDIF


; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; INITIALIZE DEFAULTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  do_fix = 0
  pass = 1
  comments = ["sfng_check_header:"]
      
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ HEADER
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(hdr) eq 0 then begin
     message, 'No header supplied', /info
     pass = 0
     comments = [comments, "No header supplied"]
     return, pass
  endif

  if keyword_set(fixhdr) then begin
     do_fix=1
     fixhdr=hdr
  endif


; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; UNITS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  unit_str='unknown'
  units = sxpar(hdr, "BUNIT", count=unit_ct)

  if unit_ct eq 0 then begin
     pass = 0
     comments = [comments, "No BUNIT keyword"]
  end else begin
     unit_str=strupcase(strcompress(units,/rem))
     if unit_str ne "K" and unit_str ne "KELVIN" and unit_str ne "KELVINS" $
        and unit_str ne "K(TMB)" and unit_str ne "KELVIN(TMB)" then begin
        pass = 0
        comments = [comments, "Units are "+unit_str]
     end else begin
        comments = [comments, "Units are "+unit_str]
     end
  end 
  unit=unit_str
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; BEAM
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  bmaj = sxpar(hdr, "BMAJ", count=bmaj_ct)
  bmin = sxpar(hdr, "BMIN", count=bmin_ct)
  bpa = sxpar(hdr, "BPA", count=bpa_ct)
  beam=[bmaj,bmin,bpa]
  
  if bmaj_ct eq 0 then begin
     pass = 0
     comments = [comments, "No BMAJ keyword"]
  end else begin
     bmaj_str=strtrim(string(bmaj*3600.),2)
     comments = [comments, "BMAJ is "+bmaj_str+" arcsec"]     
  end

  if bmin_ct eq 0 then begin
     pass = 0
     comments = [comments, "No BMIN keyword"]
     if do_fix eq 1 and bmaj_ct eq 1 then begin
        comments = [comments, "Assuming BMIN = BMAJ"]
        bmin=bmaj & beam[1]=bmin
        bmin_ct=1
        pass = 1
        sxaddpar,fixhdr,'BMIN',bmin
        sxaddpar,fixhdr,'HISTORY','Added BMIN, assuming BMIN = BMAJ'
     end
  end else begin
     bmin_str=strtrim(string(bmin*3600.),2)
     comments = [comments, "BMIN is "+bmin_str+" arcsec"]     
  end
  

  if bpa_ct eq 0 then begin
     comments = [comments, "No BPA keyword"]
     if do_fix eq 1 then begin
        comments = [comments, "Assuming BPA=0"]
        bpa=0. & beam[1]=bpa
        sxaddpar,fixhdr,'BPA',bpa
        sxaddpar,fixhdr,'HISTORY','Added BPA, assuming BPA=0.'
     end

  end else begin
     bpa_str=strtrim(string(bpa),2)
     comments = [comments, "BPA is "+bpa_str+" degrees"]     
  end


; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; X-, Y- AXIS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

     cdelt1=sxpar(hdr,'CDELT1',count=cdelt1_ct)
     cdelt2=sxpar(hdr,'CDELT2',count=cdelt2_ct)
     cd11=sxpar(hdr,'CD1_1',count=cd11_ct)
     cd12=sxpar(hdr,'CD1_2',count=cd12_ct)
     cd21=sxpar(hdr,'CD2_1',count=cd21_ct)
     cd22=sxpar(hdr,'CD2_2',count=cd22_ct)

     if cdelt1_ct eq 0 and (cd11_ct eq 0 or cd12_ct eq 0 or cd21_ct eq 0 or cd22_ct eq 0) then begin
        comments = [comments, "No CDELT1 keyword or CD transformation matrix"]
        pass=0
     end
     if cdelt2_ct eq 0 and (cd11_ct eq 0 or cd12_ct eq 0 or cd21_ct eq 0 or cd22_ct eq 0) then begin
        comments = [comments, "No CDELT2 keyword or CD transformation matrix"]
        pass=0
     end

     if cdelt1_ct eq 1 and cdelt2_ct eq 1 and abs(cdelt1) ne abs(cdelt2) then $
        comments=[comments, "Non-square pixels? But pixscale assumes abs(cdelt1) = abs(cdelt2)"]

     if cdelt1_ct eq 1 and cdelt2_ct eq 1 then begin
        pixscale=abs(cdelt1)
        cdelt1_str=strtrim(string(abs(cdelt1)*3600.),2)
        comments = [comments, "Pixelscale from CDELT1 is "+cdelt1_str+" arcsec"]     
     end
     
     if cdelt1_ct eq 0 and (cd11_ct eq 1 or cd12_ct eq 1 or cd21_ct eq 1 or cd22_ct eq 1) then begin
        crota2 = atan(cd12,cd22)
        cdelt2=cd22/crota2
        cdelt1=-1.*cdelt2
        ; special case if rotation is 90 degrees and cd22=0.
        if abs(cd11)+abs(cd22) le 0.01*(abs(cd12)+abs(cd21)) then begin
           crota2 = atan(cd12,cd22)
           cdelt2=-cd12/sin(crota2)
           cdelt1=-1.*cdelt2
        end
        pixscale=abs(cdelt1)
        cdelt1_str=strtrim(string(abs(cdelt1)*3600.),2)
        comments = [comments, "Pixelscale from CD matrix is "+cdelt1_str+" arcsec"]     
     end
     
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; Z-AXIS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  valid_veltypes = $
     ["KM/S", $
      "VRAD", $
      "VELO-LSR", $
      "VELOCITY", $
      "FELO-HEL"]

  ctype3 = sxpar(hdr, "CTYPE3", count=ctype_ct)

  if ctype_ct eq 0 then begin
     pass = 0
     comments = [comments, "... MAJOR: no CTYPE3 keyword"]
  endif else begin
     if total(strcompress(strupcase(ctype3),/rem) eq valid_veltypes) eq 0 then begin
        comments = [comments, "CTYPE3 keyword does not match a velocity convention that I know."]     
     endif
  endelse

  channel = abs(sxpar(hdr, "CDELT3", count=cdelt_ct))
  cdelt3 = sxpar(hdr, "CDELT3", count=cdelt_ct)
  vel0 = abs(sxpar(hdr, "CRVAL3", count=crval3_ct))
  if cdelt_ct eq 0 or crval3_ct eq 0 then begin
     pass = 0
     comments = [comments, "No CDELT3/CRVAL3 keyword"]
  endif else begin
     if channel gt 100. and vel0 gt 1.e5 then begin
        comments = [comments, "CDELT3 and CRVAL3 indicate velocities in m/s units?"]
        if do_fix eq 1 then begin
           comments = [comments, "Assuming m/s and converting to km/s"]
           channel=channel/1000.
           cdelt3=cdelt3/1000.
           vel0=vel0/1000.
           sxaddpar,fixhdr,'CDELT3',cdelt3,'Modified CDELT3: assuming m/s and converting to km/s'
           sxaddpar,fixhdr,'HISTORY','Modified CDELT3: assuming m/s and converting to km/s'
           sxaddpar,fixhdr,'CRVAL3',vel0,'Modified CRVAL3: assuming m/s and converting to km/s'
           sxaddpar,fixhdr,'HISTORY','Modified CRVAL3: assuming m/s and converting to km/s'
        end
     endif
     chanw=channel
     channel_str=strtrim(string(channel),2)
     make_axes,fixhdr,vaxis=vaxis,/vonly
     nchans=n_elements(vaxis)
     startv_str=strtrim(string(vaxis[0]),2)
     endv_str=strtrim(string(vaxis[nchans-1]),2)
     comments = [comments, "Velocity in start/end channel: "+startv_str+","+endv_str]
     comments = [comments, "Channel width is "+channel_str]     
  endelse


  the_end:
  return, pass

end

