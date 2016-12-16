pro sfng_enlarge_mask,datadir=datadir,fits_in=fits_in $
                     ,idl_in=idl_in, hdr_in=hdr_in $
                     ,outdir=outdir,fits_out=fits_out $
                     ,idl_out=idl_out ,hdr_out=hdr_out $
                     ,idl_mask=idl_mask, hdr_mask=hdr_mask, fits_mask=fits_mask $ 
                     ,pixel_blank=pixel_blank,chan_blank=chan_blank $
                     ,help=help,verbose=verbose, applymask=applymask

;+ NAME:
;     sfng_enlarge_mask
; CALLING SEQUENCE:
;     sfng_enlarge_mask,idl_in=cube,hdr_in=hdr,pixel_blank=3,chan_blank=2 $
;                     ,[idl_out=out,hdr_out=hdr_out,fits_out='galaxy_blankedges.fits'] $
;                     ,[/verbose,/help] $
; PURPOSE:
;     increases the size of the blanking mask around the edges of the cube
; INPUTS:
;     fits_in = filename of input cube
;     idl_in = input cube 
;     hdr_in = FITS header of input cube
;     idl_out = modified cube
;     hdr_out = modified FITS header
;     idl_mask = output masking cube
;     hdr_mask =  FITS header of output masking cube
;     fits_out = filename of output cube
;     fits_mask = filename of output mask
;     pixel_blank = number of pixel around the edge of the FoV that
;                   should be blanked. Default is 5.
;     chan_blank = [n,m]: number of channels at [start,end] of cube that
;                  should be blanked.  Default is [2,2]
; OPTIONAL INPUT:
;     datadir = directory for input FITS files. Defaults to
;              current directory.
;     outdir = output directory for matched FITS files. Defaults to
;              current directory.
; ACCEPTED KEY-WORDS:
;     help = print this help
;     verbose = print extra information to screen
;     applymask = apply the new mask to the dataset
; EXAMPLES
;     sfng_enlarge_mask,idl_in=c1,hdr_in=c1hdr,pixel_blank=5,chan_blank=[2,3] $
;                   ,outdir='files_out/'
;
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     Goddard Astron libraries
;     JPB's enlarge.pro
; COMMONS:
;     
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;    written 6-12-2016 by AH
; COMMENTS AND TO DO LIST:
;    Minimally tested!
;    Error trapping not implemented.
;-

  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_enlarge_mask'
     goto,the_end
  ENDIF

;==============
; defaults
;==============

  use_outdir='./'
  use_datadir='./'
  do_applymask = 0
  use_pixel_blank = 5
  use_chan_blank = [2,2]
  use_verbose=0
  
;==============
; process user input
;==============

  if keyword_set(outdir) then use_outdir=outdir
  if keyword_set(datadir) then use_datadir=datadir
  if keyword_set(applymask) then do_applymask=1
  if keyword_set(verbose) then use_verbose=1
  if keyword_set(pixel_blank) then use_pixel_blank=pixel_blank
  if keyword_set(chan_blank) then use_chan_blank=chan_blank

;===================
; enforce final back slash and make sure directories exists
;===================
  use_datadir=file_search(use_datadir,/mark,/full)
  use_outdir=file_search(use_outdir,/mark,/full)

  if use_datadir eq '' or use_outdir eq '' then $
     message,'Problem with data/out directories. Do they exist?'

;==============
; read data
;==============
  
  if keyword_set(fits_in) then data_in=readfits(use_datadir+fits_in,hdr)
  if keyword_set(idl_in) then data_in=idl_in
  if keyword_set(hdr_in) then hdr=hdr_in

  if n_elements(data_in) eq 0 then begin
     message, 'Problem reading input data',/info
     goto, the_end
  end

  if n_elements(hdr) eq 0 then begin
     message, 'I need header information',/info
     goto, the_end
  end


;==============
; determine current observing mask
;==============

  sz=size(data_in)
  naxis1=sz[1] & naxis2=sz[2] & naxis3=sz[3]
  use_midx=fix(naxis1/2.)
  use_midy=fix(naxis2/2.)
  use_midchan=fix(naxis3/2.)
 
  mask_final=data_in         ; create a 3D container
  mask_final[*]=1.           ; Initial assumption is that all pixels are valid    
  hdr_mask=hdr_in
  sxaddpar,hdr_mask,'BUNIT','Mask'
  sxaddpar,hdr_mask,'DATAMIN',0
  sxaddpar,hdr_mask,'DATAMAX',1

  if use_pixel_blank gt 0 then begin
     
     for k=0,naxis3-1 do begin

        mask_2d=reform(data_in[*,*,k]) ; take this plane

        mpix=where(finite(mask_2d) eq 0, mct,comp=epix,ncomp=ect)

        mask_2d[mpix]=1 &  mask_2d[epix]=0 ; at this stage, mask is 1 for BAD and 0 for GOOD
        badregion=where(mask_2d eq 1)
        badregion_expand_idx=enlarge(badregion,use_pixel_blank,naxis1,naxis2)
        mask_2d[badregion_expand_idx]=1

        badidx=where(mask_2d eq 1,comp=goodidx) ; switch to mask is 0 for BAD and 1 for GOOD
        mask_2d[badidx]=0 & mask_2d[goodidx]=1

        mask_final[*,*,k]=mask_2d
     end

     use_pixstr=strtrim(string(use_pixel_blank),2)
     sxaddpar,hdr_mask,'HISTORY','Blanked '+use_pixstr+' pixels at edge of FoV'

  end
  
  if n_elements(use_chan_blank) gt 0 then begin

     if n_elements(use_chan_blank) eq 1 then $
        use_chan_blank=[use_chan_blank,use_chan_blank]
     
     for k=0,use_chan_blank[0]-1 do $
        mask_final[*,*,k]=0

     for k=naxis3-use_chan_blank[1]-1,naxis3-1 do $
        mask_final[*,*,k]=0

     use_chan0str=strtrim(string(use_chan_blank[0]),2)
     use_chan1str=strtrim(string(use_chan_blank[1]),2)
     sxaddpar,hdr_mask,'HISTORY','Blanked '+use_chan0str+' start channels'
     sxaddpar,hdr_mask,'HISTORY','Blanked '+use_chan1str+' end channels'

  end

  data_out=data_in
  hdr_out=hdr_in

  if keyword_set(do_applymask) then begin

     badidx = where(mask_final eq 0 or finite(data_out) eq 0, ct,ncomp=gct,comp=goodidx)

     if use_verbose eq 1 then print,'Applying mask to input cube'
     data_out[badidx] = !values.f_nan
     sxaddpar, hdr_out, 'DATAMAX', max(data_out,/nan)
     sxaddpar, hdr_out, 'DATAMIN', min(data_out,/nan)
     sxaddpar, hdr_out, 'HISTORY', 'Masked by sfng_enlarge_mask'

  end

  if keyword_set(fits_out) then writefits,use_outdir+fits_out,data_out,hdr_out
  if keyword_set(fits_mask) then writefits,use_outdir+fits_mask,mask_final,hdr_mask
  
  idl_out=data_out
  hdr_out=hdr_out
  idl_mask=mask_final
  hdr_mask=hdr_mask
        
  the_end:
    return
    
end
