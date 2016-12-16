pro sfng_make_commonfov,datadir=datadir,fits_in1=fits_in1,fits_in2=fits_in2,fits_mask=fits_mask $
                        ,idl_in1=idl_in1,idl_in2=idl_in2, idl_mask=idl_mask $
                        ,hdr_in1=hdr_in1,hdr_in2=hdr_in2, hdr_mask=hdr_mask $
                        ,outdir=outdir,fits_out1=fits_out1,fits_out2=fits_out2, fits_outmask=fits_outmask $
                        ,idl_out1=idl_out1,idl_out2=idl_out2, idl_outmask=idl_outmask $
                        ,hdr_out1=hdr_out1,hdr_out2=hdr_out2, hdr_outmask=hdr_outmask $
                        ,applymask=applymask, help=help,verbose=verbose 


;+ NAME:
;     sfng_make_commonfov
; CALLING SEQUENCE:
;     sfng_make_commonfov,idl_in1=cube1,idl_in2=cube2,hdr_in1=hdr1,hdr2_in=hdr2 $
;                     ,[idl_out1=out1,idl_out2=out2,hdr_out1=hdr_out1,hdr_out2=hdr_out2] $
;                     ,[/applymask,/verbose,/help] $
; PURPOSE:
;     determines a common FoV between two cubes/maps.
;     Optionally: combines this 2D FoV with an input mask (1/0s) and
;     applies to all planes of input cube
; INPUTS:
;     fits_in[1/2/mask] = filename of input cubes/mask
;     idl_in[1/2/mask] = input cube s/mask
;     hdr_in[1/2/mask] = FITS header of input cubes/mask
;     idl_out[1/2/mask] = modified cubes/mask
;     hdr_out[1/2/mask] = modified FITS headers
;     fits_in[1/2/mask] = filename of output cubes/mask
; OPTIONAL INPUT:
;     datadir = directory for input FITS files. Defaults to
;              current directory.
;     outdir = output directory for matched FITS files. Defaults to
;              current directory.
; ACCEPTED KEY-WORDS:
;     help = print this help
;     verbose = print extra information to screen
;     applymask = apply the common FoV (+input mask) to both datasets  
; EXAMPLES
;     sfng_make_commonfov,idl_in1=c1,idl_in2=c2,hdr_in1=c1hdr,hdr_in2=c2hdr $
;                   ,outdir='files_out/'
;
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     Goddard Astron libraries
; COMMONS:
;     
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;    written 25-11-2016 by AH
; COMMENTS AND TO DO LIST:
;    Minimally tested!
;    Error trapping not implemented.
;    Assumes input cubes will have matching (x,y) grid.
;    At this stage, only does 2D mask (same FoV is applied to all planes)
;-

  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_make_commonfov'
     goto,the_end
  ENDIF

  
  @sfng_constants.bat
  nan = !values.f_nan


;==============
; defaults
;==============

  use_outdir='./'
  use_datadir='./'
  do_applymask=0
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PROCESS USER INPUT
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if keyword_set(outdir) then use_outdir=outdir
  if keyword_set(datadir) then use_datadir=datadir
  if keyword_set(applymask) then do_applymask=1

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
  
  if keyword_set(fits_in1) then d1=readfits(use_datadir+fits_in1,h1)
  if keyword_set(fits_in2) then d2=readfits(use_datadir+fits_in2,h2)
  if keyword_set(fits_mask) then mask_in=readfits(use_datadir+fits_mask,hm_in)
  if keyword_set(idl_in1) then d1=idl_in1
  if keyword_set(idl_in2) then d2=idl_in2
  if keyword_set(hdr_in1) then h1=hdr_in1
  if keyword_set(hdr_in2) then h2=hdr_in2
  if keyword_set(idl_mask) then mask_in=idl_mask
  if keyword_set(hdr_mask) then hm_in=hdr_mask

  ; need to collapse cubes to create 2D FoV

  sz1=size(d1) & sz2=size(d2)

  if (sz1[0] ne 2 and sz1[0] ne 3) or (sz2[0] ne 2 and sz2[0] ne 3) then begin
     message,'Something wrong with dimensions of input cubes/maps?',/info
     goto,the_end
  end

  d2_plane=d2 & d1_plane=d1
  
  if sz2[0] eq 3 then $
     d2_plane=max(d2,/nan,dim=3)
  if sz1[0] eq 3 then $
     d1_plane=max(d1,/nan,dim=3)

  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Put optional input mask onto same (x,y) grid
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


  if n_elements(mask_in) gt 0 then begin
  
     hastrom, mask_in, hm_in, mask, hm, h1, interp=0, missing=!values.f_nan
     sxaddpar, hm, 'DATAMAX', max(mask,/nan)
     sxaddpar, hm, 'DATAMIN', min(mask,/nan)
     sxaddpar, hm, 'HISTORY', 'Regridded pixel-grid and astrometry'
     sxaddpar, hm, 'HISTORY', 'Assumes 1: good pixel, 0: bad pixel'
     sxaddpar, hm, 'BUNIT', 'mask'

  end else begin
     mask=d1_plane
     mask[*]=1.
     hm=twod_head(h1)
  end
  
 
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; MAKE 2D COMMON FOV MASK
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  badidx = where(mask eq 0 or finite(d1_plane) eq 0 or finite(d2_plane) eq 0, ct,ncomp=gct,comp=goodidx)
  
  if ct gt 0 then begin
     
     mask[badidx]=0
     sxaddpar, hm, 'DATAMAX', max(mask,/nan)
     sxaddpar, hm, 'DATAMIN', min(mask,/nan)
     sxaddpar, hm, 'HISTORY', 'Undefined pixels in input maps also set to BAD'
     sxaddpar, hm, 'BUNIT', 'Mask'     
     
     if keyword_set(do_applymask) then begin
        
        if sz1[0] eq 2 then begin
           print,'Applying mask to input map 1'
           d1[badidx] = !values.f_nan
           sxaddpar, h1, 'DATAMAX', max(d1,/nan)
           sxaddpar, h1, 'DATAMIN', min(d1,/nan)
           sxaddpar, h1, 'HISTORY', 'Masked to common FoV'
        end
        if sz2[0] eq 2 then begin
           print,'Applying mask to input map 2'
           d2[badidx] = !values.f_nan
           sxaddpar, h2, 'DATAMAX', max(d2,/nan)
           sxaddpar, h2, 'DATAMIN', min(d2,/nan)
           sxaddpar, h2, 'HISTORY', 'Masked to common FoV'
        end

        if sz1[0] eq 3 then begin
           print,'Applying mask to input cube 1'
           for i=0,sz1[3]-1 do begin
              this_plane_d1=d1[*,*,i]
              this_plane_d1[badidx] = !values.f_nan
              d1[*,*,i]=this_plane_d1
           end
           sxaddpar, h1, 'DATAMAX', max(d1,/nan)
           sxaddpar, h1, 'DATAMIN', min(d1,/nan)
           sxaddpar, h1, 'HISTORY', 'Masked to common FoV'
        end
        if sz2[0] eq 3 then begin
           print,'Applying mask to input cube 2'
           for i=0,sz2[3]-1 do begin
              this_plane_d2=d2[*,*,i]
              this_plane_d2[badidx] = !values.f_nan
              d2[*,*,i]=this_plane_d2
           end
           sxaddpar, h2, 'DATAMAX', max(d2,/nan)
           sxaddpar, h2, 'DATAMIN', min(d2,/nan)
           sxaddpar, h2, 'HISTORY', 'Masked to common FoV'
        end
        
     end
     
  end
  
  if keyword_set(fits_out1) then writefits,use_outdir+fits_out1,d1,h1
  if keyword_set(fits_out2) then writefits,use_outdir+fits_out2,d2,h2
  if keyword_set(fits_outmask) then writefits,use_outdir+fits_outmask,mask,hm

  idl_out1=d1
  idl_out2=d2
  idl_outmask=mask
  hdr_out1=h1
  hdr_out2=h2
  hdr_outmask=hm

  the_end:
  return
  
end
