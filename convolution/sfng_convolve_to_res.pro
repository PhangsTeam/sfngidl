pro sfng_convolve_to_res, idl_in=idl_in, hdr_in=hdr_in, hdr_out=hdr_out, idl_out=idl_out $
                          , fits_in=fits_in, fits_out=fits_out $
                          , galaxy=galaxy $
                          , target_res=target_res $
                          , distance=distance $
                          , datadir=datadir $
                          , outdir=outdir $
                          , perbeam=perbeam $
                          , arcsecond=arcsecond $
                          , strict=strict $
                          , verbose=verbose $
                          , help=help $
                          , nowait=nowait

;+ NAME:
;     sfng_convolve_to_res
; CALLING SEQUENCE:
;     sfng_convolve_to_res,idl_in=idl_in,hdr_in=hdr_in,fits_out=fits_out,[fits_in=fits_in] $
;                          ,target_res=target_res,[galaxy=galaxy,distance=distance] $
;                          ,[datadir=datadir,outdir=outdir] $
;                          ,[/arcsecond,/strict,/perbeam,/verbose,/help] 
; PURPOSE:
;      given an input map/cube, produces FITS files at a set of requested resolutions
; INPUTS:
;     fits_in = filename of input cube
;     idl_in = input cube 
;     hdr_in = FITS header of input cube
;     fits_out = prefix of filename of output cube. 
;     idl_out = convolved cube
; OPTIONAL INPUT:
;     datadir    = directory for input FITS files. Defaults to
;                  current directory.
;     outdir     = output directory for matched FITS files. Defaults to
;                  current directory.
;     target_res = vector of desired linear resolutions (in parsecs,
;                  unless /arcsecond keyword is set) for output
;                  data. 
;     galaxy     = name of galaxy. If present, use GAL_BASE to find galaxy
;                  distance. Need either galaxy or distance.
;     distance   = distance in Mpc. If present, takes precedence over
;                  GAL_BASE distance. Need either galaxy or distance.
; ACCEPTED KEY-WORDS:
;     round = force beam to be round (Bmaj=bmin, bpa=0.) ***Currently this is hard-coded to be true.***
;     help = print this help
;     strict = if set, don't modify the RA/DEC projection types to TAN
;     verbose = print extra information to screen
;     arcsecond = interpret target_res inputs as being in arcseconds,
;                 not parsecs
;     strict = if set, don't modify the RA/DEC projection types to TAN
;     nowait = if set, don't pause for 1 second after showing verbose messages
;     perbeam = flag whether flux units are per-beam (passed to conv_with_gauss for
;               correct scaling of map intensity)
; EXAMPLES
;     sfng_convolve_to_res,idl_in=cube,hdr_in=hdr,galaxy='NGC4321', $
;                   target_res=[0.2,0.5,0.8]*1.e3,datadir='./inputdata/',outdir='./files_out/'
;
;     sfng_convolve_to_res,fits_in='mygalaxy.fits',distance=5., $
;                   target_res=[10,20,50],/arcsecond,datadir='./inputdata/',outdir='./files_out/',/strict
;
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     Goddard Astron libraries
;
;     Routines distributed with CPROPSTOO: conv_with_gauss.pro, cube_hastrom.pro 
;     https://github.com/akleroy/cpropstoo
;  
;     Galaxy parameters obtained from AKL's galbase IDL routines
;     https://github.com/akleroy/galbase
;
; COMMONS:
;     
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;    written 13-12-2016 by AH
; COMMENTS AND TO DO LIST:
;    Currently only produces output with round beams.
;    Minimally tested!
;    Error trapping not implemented.
;-

  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_convolve_to_res'
     goto,the_end
  ENDIF

; ==============================
; DEFAULT PARAMETERS & GALAXIES
; ==============================

  use_round=1 ; non-circular beams not yet implemented
  use_datadir='./'
  use_outdir='./'
  use_verbose=0
  use_arcsecond=0
  default_outname='my_galaxy'
  use_perbeam=0
  use_strict=0
  use_wait=1.
  
; ==============================
; PROCESS USER INPUTS
; ==============================

  if keyword_set(target_res) then use_target_res=target_res
  if keyword_set(datadir) then use_datadir=datadir
  if keyword_set(outdir) then use_outdir=outdir
  if keyword_set(galaxy) then use_galaxy=galaxy
  if keyword_set(distance) then use_distance=distance
  if keyword_set(arcsecond) then use_arcsecond=1
  if keyword_set(verbose) then use_verbose=1
  if keyword_set(strict) then use_strict=1
  if keyword_set(perbeam) then use_perbeam=1
  if keyword_set(nowait) then use_wait=0.
  if keyword_set(fits_out) then default_outname=fits_out

  if not keyword_set(use_target_res) then begin
     message,'No target resolution information provided',/info
     goto, the_end
  end

  ; enforce final back slash and make sure directories exists
  use_datadir=file_search(use_datadir,/mark,/full)
  use_outdir=file_search(use_outdir,/mark,/full)

  if use_datadir eq '' or use_outdir eq ''  then begin
     message,'Problem with data/output directory?',/info
     goto, the_end
  end

; ==============================
; READ DATA AND EXTRACT HEADER INFORMATION
; IDL_IN TAKES PRECEDENCE IF BOTH FITS/IDL INPUTS ARE PRESENT
; EXTRACT BMAJ/BMIN FROM HEADER  
; ==============================

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

  use_bmaj=sxpar(hdr,'BMAJ',count=bmaj_ct)
  use_bmin=sxpar(hdr,'BMIN',count=bmin_ct)
  use_bpa=sxpar(hdr,'BPA',count=bpa_ct)

  if bmaj_ct eq 0 then begin
     message,'Cube is missing BMAJ information. I need this for correct convolution. Exiting',/info
     goto, the_end
  end
  if bmaj_ct eq 1 and bmin_ct eq 0 then begin
     print,'Cube is missing BMIN information. Setting bmin=bmaj'
     use_bmin=use_bmaj
     sxaddpar,hdr,'BMIN',use_bmin,'Assuming BMIN=BMAJ'
  end
  if bpa_ct eq 0 then begin
     print,'Cube is missing BPA information. Setting bpa=0.'
     use_bpa=0.
     sxaddpar,hdr,'BPA',use_bpa,'Assuming BPA=0.'
  end

; don't attempt to convolve to resolutions smaller than the resolution of the input data
  use_res_lim=use_bmaj*3600. 

  if use_verbose eq 1 then begin
     print,'===================================================================='
     print,'Input data has resolution [as]: ',use_bmaj*3600.
     wait,use_wait
     print,'===================================================================='
   end
  
;==============
; if keyword strict is not set, then blithely set SIN, GLS and ARC projections to TAN
;==============

  if use_strict ne 1 then begin
     print,'===================================================================='
     print,'Recklessly setting WCS RA/DEC PROJECTION TYPES to TAN for simplicity'
     print,'Re-run with strict=1 if you do not want this'
     print,'===================================================================='
     wait,use_wait
     sxaddpar,hdr,'CTYPE1','RA---TAN','Manually forced to TAN'
     sxaddpar,hdr,'CTYPE2','DEC--TAN','Manually forced to TAN'
  end

; ==============================
; WORK OUT BASIC SET OF CONVOLVING BEAMS
; ==============================

  if not keyword_set(use_galaxy) and not keyword_set(use_distance) then begin
     message, 'Galaxy name and/or distance parameter must be supplied',/info
     goto, the_end
  end

  if keyword_set(use_galaxy) then begin
     use_galaxy=strupcase(use_galaxy)
     use_galaxy_lowcase=strlowcase(use_galaxy)
     gstr=gal_data(use_galaxy)
     use_distance=gstr.dist_mpc*1.e6 ; distance in parsecs
  end
  
  if keyword_set(distance) then begin
     use_distance = distance*1.e6
     if use_verbose eq 1 then begin
        print,'===================================================================='
        print,'Using user-supplied distance [Mpc]: ',use_distance/1.e6
        wait,use_wait
        print,'===================================================================='
        end
  end else begin
     if use_verbose eq 1 then begin
        print,'===================================================================='
        print,'Using GAL_BASE distance [Mpc]: ',use_distance/1.e6
        wait,use_wait
        print,'===================================================================='
     end
  end

  use_target_res_as=use_target_res*206265./use_distance
     
  if keyword_set(arcsecond) then begin
     use_target_res_as=use_target_res
     use_target_res=use_target_res_as*use_distance/206265.
  end


  target_beams=use_target_res_as
  target_scales=use_target_res

  goodbms=where(target_beams gt use_res_lim, goodct)

  if goodct ge 1 then begin
     target_beams=target_beams[goodbms]
     target_scales=target_scales[goodbms]

     if use_verbose eq 1 then begin
        print,'===================================================================='
        print,'Creating output maps at these scales: '+strjoin(strtrim(string(target_scales),2),' ')+' pc'
        wait,use_wait
        print,'===================================================================='
     end

; ==============================
; LOOP THROUGH GOOD CONVOLVING BEAMS 
; ==============================

     for j=0,goodct-1 do begin

        tscstr=strtrim(string(round(target_scales[j])),2)
        use_outname=default_outname+'_'+tscstr+'pc.fits'

        if use_verbose eq 1 then begin
           print,'===================================================================='
           print,'Working on '+use_outname
           wait,use_wait
           print,'===================================================================='
        end
           
        conv_with_gauss $
           , data = data_in $
           , hdr = hdr $
           , out_data = data_out $
           , out_hdr = out_hdr $
           , target_beam=target_beams[j]*[1.*1.] $
           , perbeam=use_perbeam
        
        sxaddpar,hdr_out,'DATAMAX',max(data_out,/nan)
        sxaddpar,hdr_out,'DATAMIN',min(data_out,/nan)
        writefits,use_outdir+use_outname,data_out,out_hdr
        
        data_in=data_out
        hdr=out_hdr            ; smooth incrementally

     end                        ; of loop
     
; ==============================
; SEND A MESSAGE IF NOTHING USEFUL CAN BE DONE
; ==============================
  end else begin
     print,'No good target beams for input dataset found. (Requested scales are smaller than current image resolution).'
  end

  idl_out=data_out
  hdr_out=out_hdr
  
  the_end:
  return
  stop
  
end
