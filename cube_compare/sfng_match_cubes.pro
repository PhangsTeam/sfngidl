pro sfng_match_cubes,datadir=datadir,fits_in1=fits_in1,fits_in2=fits_in2 $
                     ,idl_in1=idl_in1,idl_in2=idl_in2 $
                     ,hdr_in1=hdr_in1,hdr_in2=hdr_in2 $
                     ,outdir=outdir,fits_out1=fits_out1,fits_out2=fits_out2 $
                     ,idl_out1=idl_out1,idl_out2=idl_out2 $
                     ,hdr_out1=hdr_out1,hdr_out2=hdr_out2 $
                     ,xygrid_master=xygrid_master,vgrid_master=vgrid_master $
                     ,round=round, target_beam=target_beam $
                     ,no_convol=no_convol, no_regrid=no_regrid $
                     ,no_xyregrid=no_xyregrid, no_vregrid=no_vregrid, perbeam=perbeam $
                     ,help=help,verbose=verbose $
                     ,strict=strict

;+ NAME:
;     sfng_match_cubes
; CALLING SEQUENCE:
;     sfng_match_cubes,idl_in1=cube1,idl_in2=cube2,hdr_in1=hdr1,hdr2_in=hdr2 $
;                     ,[idl_out1=out1,idl_out2=out2,hdr_out1=hdr_out1,hdr_out2=hdr_out2] $
;                     ,[xygrid_master=2,vgrid_master=1,target_beam=target_beam] $
;                     ,[/no_convol,/no_regrid,perbeam=perbeam,/verbose,/help] $
; PURPOSE:
;     modifies two data cubes to have a common angular resolution and (x,y,v)
;     gridding 
; INPUTS:
;     fits_in[1/2] = filename of input cube
;     idl_in[1/2] = input cube 
;     hdr_in[1/2] = FITS header of input cube
;     idl_out[1/2] = modified cube
;     hdr_out[1/2] = modified FITS header
;     fits_out[1/2] = filename of output cube
;     xygrid_master = 1 or 2 -- which cube is master (x,y) grid?
;                               Default 1.
;     vgrid_master = 1 or 2 -- which cube is master velocity
;                               gridding? Default 1.
; OPTIONAL INPUT:
;     datadir = directory for input FITS files. Defaults to
;              current directory.
;     outdir = output directory for matched FITS files. Defaults to
;              current directory.
;     target_beam = three element vector of [bmaj,bmin,bpa] in
;                    [arcsec,arcsec,degree]. If present, takes
;                    precedence over default behaviour, which is to
;                    smooth higher resolution cube to match lower
;                    resolution cube.
;     no_convol = don't do the convolution (match x,y,v grid only)
;     no_xyregrid = don't regrid (X,Y) (only do convolution to
;                   common angular resolution and, if working cubes, velocity regrid)
;     no_vregrid = don't regrid velocity (only do convolution to
;                   common angular resolution and (X,Y) regrid)
;     no_regrid = don't regrid at all (only do convolution to
;                   common angular resolution)
;     perbeam = two-element vector containing flags whether flux units
;               are per-beam (handed to conv_with_gauss for
;               correct scaling of intensity after convolution with
;               Gaussian)
; ACCEPTED KEY-WORDS:
;     round = force beam to be round (Bmaj=bmin, bpa=0.) ***Currently
;             this is hard-coded to be true.***
;     help = print this help
;     strict = if set, don't modify the RA/DEC projection types to TAN
;     verbose = print extra information to screen
; EXAMPLES
;     sfng_match_cubes,idl_in1=c1,idl_in2=c2,hdr_in1=c1hdr,hdr_in2=c2hdr $
;                   ,xygrid_master=2,vgrid_master=1,target_beam=[30.,30.,0.],/round $
;                   ,outdir='files_out/'
;
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     Goddard Astron libraries
;     AKL's conv_with_gauss.pro, cube_hastrom.pro
; COMMONS:
;     
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;    written 25-11-2016 by AH
; COMMENTS AND TO DO LIST:
;    Currently only produces output with round beams.
;    Minimally tested!
;    Error trapping not implemented.
;-

  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_match_cubes'
     goto,the_end
  ENDIF

;==============
; defaults
;==============

  use_round=1 ; non-circular beams not yet implemented
  use_xygrid_master=1
  use_vgrid_master=1
  use_outdir='./'
  use_datadir='./'
  use_perbeam=0
  use_strict=0
  
;==============
; process user input
;==============

  if keyword_set(outdir) then use_outdir=outdir
  if keyword_set(datadir) then use_datadir=datadir
  if keyword_set(xygrid_master) then use_xygrid_master=xygrid_master
  if keyword_set(vgrid_master) then use_vgrid_master=vgrid_master
  if keyword_set(perbeam) then use_perbeam=1
  if keyword_set(strict) then use_strict=1


;==============
; read data
;==============
  
  if keyword_set(fits_in1) then d1=readfits(use_datadir+fits_in1,h1)
  if keyword_set(fits_in2) then d2=readfits(use_datadir+fits_in2,h2)
  if keyword_set(idl_in1) then d1=idl_in1
  if keyword_set(idl_in2) then d2=idl_in2
  if keyword_set(hdr_in1) then h1=hdr_in1
  if keyword_set(hdr_in2) then h2=hdr_in2

;==============
; extract header information that we need for convolution
;==============
  
  bmaj1=sxpar(h1,'BMAJ',count=bmaj1_ct)*3600.
  bmin1=sxpar(h1,'BMIN',count=bmin1_ct)*3600.
  bpa1=sxpar(h1,'BPA',count=bpa1_ct)
  
  bmaj2=sxpar(h2,'BMAJ',count=bmaj2_ct)*3600.
  bmin2=sxpar(h2,'BMIN',count=bmin2_ct)*3600.
  bpa2=sxpar(h2,'BPA',count=bpa2_ct)

  if not keyword_set(no_convol) then begin

    if bmaj1_ct eq 0 then begin
        print,'Cube 1 missing BMAJ information. Exiting'
        goto, the_end
    end
    if bmaj2_ct eq 0 then begin
        print,'Cube 2 missing BMAJ information. Exiting'
        goto, the_end
     end
    if bmaj1_ct eq 1 and bmin1_ct eq 0 then begin
       print,'Cube 1 missing BMIN information. Setting bmin=bmaj'
       bmin1=bmaj1/3600.
       sxaddpar,h1,'BMIN',bmin1,'Assuming BMIN=BMAJ'
    end
    if bmaj2_ct eq 1 and bmin2_ct eq 0 then begin
       print,'Cube 2 missing BMIN information. Setting bmin=bmaj'
       bmin2=bmaj2/3600.
       sxaddpar,h2,'BMIN',bmin2,'Assuming BMIN=BMAJ'
    end
    if bpa1_ct eq 0 then begin
       print,'Cube 1 missing BPA information. Setting bpa=0.'
       bpa1=0.
       sxaddpar,h1,'BPA',bpa1,'Assuming BPA=0.'
    end
    if bpa2_ct eq 0 then begin
       print,'Cube 2 missing BPA information. Setting bpa=0.'
       bpa2=0.
       sxaddpar,h2,'BPA',bpa2,'Assuming BPA=0.'
    end

;==============
; if keyword strict is not set, then blithely set SIN, GLS and ARC projections to TAN
;==============

    if use_strict ne 1 then begin
       print,'Recklessly setting all WCS RA/DEC PROJECTION TYPES to TAN for simplicity'
       print,'Re-run with strict=1 if you do not want this'
       sxaddpar,h1,'CTYPE1','RA---TAN','Manually forced to TAN'
       sxaddpar,h1,'CTYPE2','DEC--TAN','Manually forced to TAN'
       sxaddpar,h2,'CTYPE1','RA---TAN','Manually forced to TAN'
       sxaddpar,h2,'CTYPE2','DEC--TAN','Manually forced to TAN'
    end
    
;==============
; convolution to common resolution
;==============

; lower resolution cube dictates the resolution by default
    
    use_bmaj = (bmaj1 > bmaj2)
    use_bmin = (bmin1 > bmin2)
    if bmaj1 ge bmaj2 then use_bpa = bpa1
    if bmaj1 lt bmaj2 then use_bpa = bpa2

; override if target_beam keyword set

    if keyword_set(target_beam) then begin
        if n_elements(target_beam) eq 3 then begin
           use_bmaj=target_beam[0]
           use_bmin=target_beam[1]
           use_bpa=target_beam[2]
        end else if n_elements(target_beam) eq 2 then begin
           use_bmaj=target_beam[0]
           use_bmin=target_beam[1]
           use_bpa=0.
        end else if n_elements(target_beam) eq 1 then begin
           use_bmaj=target_beam[0]
           use_bmin=use_bmaj
           use_bpa=0.
        end
     end
     
     if (use_round) eq 1 then begin
        use_bmin=use_bmaj
        use_bpa=0.
     end

     if (use_bmaj gt bmaj1) or (use_bmin gt bmin1) then begin
        if keyword_set(verbose) then print,'Convolving Image 1 to: ',use_bmaj,use_bmin
        conv_with_gauss $
           , data=d1 $
           , hdr=h1 $
           , out_data=d1cvl $
           , out_hdr=h1cvl $
           ,target_beam=[use_bmaj,use_bmin,use_bpa] $
           ,perbeam=use_perbeam
        d1=d1cvl & h1=h1cvl
     end

     if (use_bmaj gt bmaj2) or (use_bmin gt bmin2) then begin
        if keyword_set(verbose) then print,'Convolving Image 2 to: ',bmaj,bmin
        conv_with_gauss $
           , data=d2 $
           , hdr=h2 $
           , out_data=d2cvl $
           , out_hdr=h2cvl $
           ,target_beam=[use_bmaj,use_bmin,use_bpa] $
           ,perbeam=use_perbeam
        d2=d2cvl & h2=h2cvl
     end

  end

;==============
; regrid to common (x,y,v) grid
;==============

  ; check that we have cubes if velocity regridding requested
  d1_sz=size(d1)
  d2_sz=size(d2)
  if d1_sz[0] ne 3 or d2_sz[0] ne 3 then no_vregrid=1

  use_operation='BOTH'
  if keyword_set(no_vregrid) and not keyword_set(no_xyregrid) then use_operation='POSITION'
  if keyword_set(no_xyregrid) and not keyword_set(no_vregrid) then use_operation='VELOCITY'
  if (keyword_set(no_xyregrid) and keyword_set(no_vregrid)) or keyword_set(no_regrid) then goto,skip_regrid

; (X,Y) regrid only requested
  if use_operation eq 'POSITION' then begin
     if use_xygrid_master eq 2 then begin
        cube_hastrom, data=d1, hdr_in = h1, target_hdr=h2 $
                      , outcube = d1xy, outhdr= h1_out, operation=use_operation
        d1=d1xy & h1=h1_out
        if keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded (X,Y) grid using template: ',fits_in2
        if not keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded (X,Y) grid using template'
     end else if use_xygrid_master eq 1 then begin
        cube_hastrom, data=d2, hdr_in = h2, target_hdr=h1 $
                   , outcube = d2xy, outhdr= h2_out, operation=use_operation
        d2=d2xy & h2=h2_out
        if keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded (X,Y) grid using template: ',fits_in1
        if not keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded (X,Y) grid using template'
     end else begin
        message,'XY grid template unknown',/info
        goto, the_end
     end
  end

  ; velocity regrid only requested
  ; this is a bit problematic since AKL's cube_hastrom only
; returns the spectral axis under operation='VELOCITY'          
; for now, we hack a fix here rather than ask AKL to modify cube_hastrom
if use_operation eq 'VELOCITY' then begin

   if use_vgrid_master eq 2 then begin

      cube_hastrom, data=d1, hdr_in = h1, target_hdr=h2 $
                      , outcube = d1v, outhdr= h1_out, operation=use_operation

      d1=d1v
      new_crval3=sxpar(h1_out,'CRVAL3')
      new_ctype3=sxpar(h1_out,'CTYPE3')
      new_cdelt3=sxpar(h1_out,'CDELT3')
      new_naxis3=sxpar(h1_out,'NAXIS3')
      new_cunit3=sxpar(h1_out,'CUNIT3')
      new_crpix3=sxpar(h1_out,'CRPIX3')
      new_specsys=sxpar(h1_out,'SPECSYS')
      sxaddpar,h1,'CRVAL3',new_crval3,'CUBE_HASTROM: regridded velocity axis'
      sxaddpar,h1,'CTYPE3',new_ctype3,'CUBE_HASTROM: regridded velocity axis'
      sxaddpar,h1,'CDELT3',new_cdelt3,'CUBE_HASTROM: regridded velocity axis'
      sxaddpar,h1,'NAXIS3',new_naxis3,'CUBE_HASTROM: regridded velocity axis'
      sxaddpar,h1,'CUNIT3',new_cunit3,'CUBE_HASTROM: regridded velocity axis'
      sxaddpar,h1,'CRPIX3',new_crpix3,'CUBE_HASTROM: regridded velocity axis'
      sxaddpar,h1,'SPECSYS',new_specsys,'CUBE_HASTROM: regridded velocity axis'
      
      if keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded velocity axis using template: ',fits_in2
      if not keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded velocity axis using template'

   end else if use_vgrid_master eq 1 then begin

      
      cube_hastrom, data=d2, hdr_in = h2, target_hdr=h1 $
                    , outcube = d2v, outhdr= h2_out, operation=use_operation
        d2=d2v
        new_crval3=sxpar(h2_out,'CRVAL3')
        new_ctype3=sxpar(h2_out,'CTYPE3')
        new_cdelt3=sxpar(h2_out,'CDELT3')
        new_naxis3=sxpar(h2_out,'NAXIS3')
        new_cunit3=sxpar(h2_out,'CUNIT3')
        new_crpix3=sxpar(h2_out,'CRPIX3')
        new_specsys=sxpar(h2_out,'SPECSYS')
        
        sxaddpar,h2,'CRVAL3',new_crval3,'CUBE_HASTROM: regridded velocity axis'
        sxaddpar,h2,'CTYPE3',new_ctype3,'CUBE_HASTROM: regridded velocity axis'
        sxaddpar,h2,'CDELT3',new_cdelt3,'CUBE_HASTROM: regridded velocity axis'
        sxaddpar,h2,'NAXIS3',new_naxis3,'CUBE_HASTROM: regridded velocity axis'
        sxaddpar,h2,'CUNIT3',new_cunit3,'CUBE_HASTROM: regridded velocity axis'
        sxaddpar,h2,'CRPIX3',new_crpix3,'CUBE_HASTROM: regridded velocity axis'
        sxaddpar,h2,'SPECSYS',new_specsys,'CUBE_HASTROM: regridded velocity axis'

        if keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded velocity axis using template: ',fits_in1
        if not keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded velocity axis using template'

     end else begin
        message,'Velocity axis template unknown',/info
        goto, the_end
     end
  end


; regridding in (X,Y) and velocity  requested
    if use_operation eq 'BOTH' then begin
       
; same target header for both types of regridding
       if use_vgrid_master eq use_xygrid_master then begin
          
          if use_vgrid_master eq 2 then begin
             cube_hastrom, data=d1, hdr_in = h1, target_hdr=h2 $
                           , outcube = d1xyv, outhdr= h1_out, operation=use_operation
             d1=d1xyv & h1=h1_out
             if keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded (X,Y,V) grid using template: ',fits_in2
             if not keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded (X,Y,V) grid using template'
          end else if use_vgrid_master eq 1 then begin
             cube_hastrom, data=d2, hdr_in = h2, target_hdr=h1 $
                           , outcube = d2xyv, outhdr= h2_out, operation=use_operation
             d2=d2xyv & h2=h2_out
             if keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded (X,Y,V) grid using template: ',fits_in1
             if not keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded (X,Y,V) grid using template'
          end else begin
             message,'(X,Y,V) grid template unknown',/info
             goto, the_end
          end
          
; different target header for position vs velocity regridding
; this is a bit problematic since AKL's cube_hastrom only
; returns the spectral axis under operation='VELOCITY'          
; for now, we hack a fix here rather than ask AKL to modify cube_hastrom
       end else begin

; cube2 is target header for velocity, and cube1 is target for (x,y)
          if use_vgrid_master eq 2 then begin

             cube_hastrom, data=d1, hdr_in = h1, target_hdr=h2 $
                           , outcube = d1v, outhdr= h1_out, operation='VELOCITY'

             
             d1=d1v
             new_crval3=sxpar(h1_out,'CRVAL3')
             new_ctype3=sxpar(h1_out,'CTYPE3')
             new_cdelt3=sxpar(h1_out,'CDELT3')
             new_naxis3=sxpar(h1_out,'NAXIS3')
             new_cunit3=sxpar(h1_out,'CUNIT3')
             new_crpix3=sxpar(h1_out,'CRPIX3')
             new_specsys=sxpar(h1_out,'SPECSYS')

             sxaddpar,h1,'CRVAL3',new_crval3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h1,'CTYPE3',new_ctype3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h1,'CDELT3',new_cdelt3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h1,'NAXIS3',new_naxis3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h1,'CUNIT3',new_cunit3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h1,'CRPIX3',new_crpix3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h1,'SPECSYS',new_specsys,'CUBE_HASTROM: regridded velocity axis'
             if keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded velocity axis using template: ',fits_in2
             if not keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded velocity axis using template'
             
             cube_hastrom, data=d2, hdr_in = h2, target_hdr=h1 $
                           , outcube = d2xy, outhdr= h2_out, operation='POSITION'
             
             d2=d2xy & h2=h2_out
             if keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded (X,Y) axis using template: ',fits_in1
             if not keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded (X,Y) axis using template'

; cube1 is target header for velocity, and cube2 is target for (x,y)
          end else if use_vgrid_master eq 1 then begin
             cube_hastrom, data=d2, hdr_in = h2, target_hdr=h1 $
                           , outcube = d2v, outhdr= h2_out, operation='VELOCITY'
             d2=d2v
             new_crval3=sxpar(h2_out,'CRVAL3')
             new_ctype3=sxpar(h2_out,'CTYPE3')
             new_cdelt3=sxpar(h2_out,'CDELT3')
             new_naxis3=sxpar(h2_out,'NAXIS3')
             new_cunit3=sxpar(h2_out,'CUNIT3')
             new_crpix3=sxpar(h2_out,'CRPIX3')
             new_specsys=sxpar(h2_out,'SPECSYS')

             sxaddpar,h2,'CRVAL3',new_crval3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h2,'CTYPE3',new_ctype3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h2,'CDELT3',new_cdelt3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h2,'NAXIS3',new_naxis3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h2,'CUNIT3',new_cunit3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h2,'CRPIX3',new_crpix3,'CUBE_HASTROM: regridded velocity axis'
             sxaddpar,h2,'SPECSYS',new_specsys,'CUBE_HASTROM: regridded velocity axis'

             if keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded velocity axis using template: ',fits_in1
             if not keyword_set(fits_in1) then sxaddpar,h2,'HISTORY','Regridded velocity axis using template'
             cube_hastrom, data=d1, hdr_in = h1, target_hdr=h2 $
                           , outcube = d1xy, outhdr= h1_out, operation='POSITION'
             d1=d1xy & h1=h1_out
             if keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded (X,Y) grid using template: ',fits_in2
             if not keyword_set(fits_in2) then sxaddpar,h1,'HISTORY','Regridded (X,Y) grid using template'

          end else begin
             message,'Problem with (X,Y,V) grid templates',/info
             goto, the_end
          end
       end
    end

    skip_regrid:

    if keyword_set(fits_out1) then writefits,use_outdir+fits_out1,d1,h1
    if keyword_set(fits_out2) then writefits,use_outdir+fits_out2,d2,h2

    idl_out1=d1
    idl_out2=d2
    hdr_out1=h1
    hdr_out2=h2
    
  the_end:
    return
    
end
