pro sfng_cube_compare,datadir=datadir,outdir=outdir,plotdir=plotdir,reportdir=reportdir $
                      ,fits_in1=fits_in1,fits_in2=fits_in2 $
                      ,idl_in1=idl_in1,idl_in2=idl_in2 $
                      ,hdr_in1=hdr_in1,hdr_in2=hdr_in2 $
                      ,savefile=savefile,tagname=tagname $
                      ,galaxy=galaxy, line_frequency=line_frequency, target_beam=target_beam $
                      ,expand_mask_edges=expand_mask_edges $
                      ,jy2k=jy2k, rebaseline=rebaseline $
                      ,xygrid=xygrid,vgrid=vgrid $
                      ,help=help,verbose=verbose,noreport=noreport 

;+ NAME:
;     sfng_cube_compare
; CALLING SEQUENCE:
;     sfng_cube_compare,
; PURPOSE:
;     compares two cubes. Essentially a wrapper to a library of routines that bring
;     cubes to a matching grid and resolution, and calculate
;     statistics about emission and emission-free regions in the cubes.
; INPUTS:
;     idl_in1/2 = IDL cube files for comparison
;     hdr_in1/2 = corresponding input header
;     fits_in1/2 = input FITS files (either IDL/FITS input must be provided)
; OPTIONAL INPUT:
;     datadir = directory for input FITS files. Defaults to
;              current directory.
;     outdir = output directory for FITS files generated during comparison. Defaults to
;              current directory.
;     plotdir = output directory for plots generated during
;               comparison. Defaults to current directory
;     reportdir = output directory for final PDF report. Defaults to
;                current directory
;     jy2k = two-element vector representing flag (0-no,1-yes) about
;            whether cube requires conversion from Jy to K. Default is no.
;     rebaseline = two-element vector indicating whether profiles in cube requires rebaselining.
;                  -1: don't rebaseline, 0: remove offset, 1:
;                  linear, 2: quadratic. Default is -1 (no
;                  rebaselining done).
;     tagname = string that will be used for filename prefix. Defaults
;               to 'mygalaxy'
;     galaxy = string, galaxy name that will be input to GAL_BASE to
;              find galaxy parameters
;     line_frequency = frequency in GHz of emission line in
;                      cube. Defaults to 12CO(2-1). Only needed for
;                      Jy<->K conversion.
;     target_beam = three-element vector [Bmaj, BMin, Bpa] in
;                   [as,as,degrees] specifying resolution at which
;                   comparison conducted. Default is to match the
;                   parameters of the cube with lowest angular
;                   resolution.
;     xygrid = which cube should be master for (x,y) pixel grid?
;              Default is 1 (i.e. cube 1)  
;     vgrid = which cube should be master for channelisation grid?
;              Default is 1 (i.e. cube 1)
;     expand_mask_edges: two-element vector indicating # pixels and #
;                        of channels by which to reduce the comparison
;                        region. Default is zero (i.e. use observed
;                        FoV that is xommon to both cubes) 
; ACCEPTED KEY-WORDS:
;     help = print this help
;     verbose = print extra information to screen
;     noreport = don't generate a PDF file, just make the plots
;                and IDL save structure of results  
; EXAMPLES
;     sfng_cube_compare,
;
; OUTPUTS:
;     savefile = IDL save file with structure containing results of comparison
;     Plots (in plotdir) and, if requested, a summary report (in reportdir)
; PROCEDURE AND SUBROUTINE USED
;     Goddard IDLAstro library (not provided)
;     Freudenreich Robust library (contrib to IDLAstro, not provided)
;     AKL CPROPTOO library (not provided)
;     AKL GAL_BASE library (not provided)
;     JPBlib: (provided in aux_pro/ subdirectory)
;             enlarge.pro, read_xcat.pro, write_xcat.pro,
;             linear_mpfit.pro, linear4mpfit.pro 
;
;  
; COMMONS:
;     
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;    written 6-12-2016 by AH
; COMMENTS AND TO DO LIST:
;    Minimally tested!
;    Error trapping not implemented.
;    Automatically decide from header whether Jy/K conversion required.  
;-

  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_cube_compare'
     goto,the_end
  ENDIF

;===================
; defaults
;===================

  @sfng_constants.bat
    
  use_savefile='cubecmp_results.sav'
  use_c1file='IDL_CUBE1'
  use_c2file='IDL_CUBE2'
  use_datadir = './orig_data/'
  use_outdir ='./good_data/'
  use_plotdir = './plots/'
  use_reportdir = './report/'
  use_tagname = 'galaxy'
  use_line_frequency = restfreq_12co21/1.e9
  use_jy2k = [0,0]
  use_rebaseline = [-1,-1]
  use_expand_mask_edges = [0,0]
  use_xygrid=1
  use_vgrid=1
  do_report=1

  use_baseline_order = [1,1] ; 0 (offset), 1 (linear) or 2 (quadratic) -- one value for each cube
  use_win = 0L
  use_avgbox=3
  use_avgspecbox=0
  use_hithresh=5
  use_lothresh=2
  nan=!values.f_nan
  ms2kms=1/1000.d
  kms2ms=1000.d
  use_c1str='CUBE1'
  use_c2str='CUBE2'
 
;===================
; process user inputs
;===================

  if keyword_set(fits_in1) then use_c1file=fits_in1
  if keyword_set(fits_in2) then use_c2file=fits_in2
  if keyword_set(datadir) then use_datadir=datadir
  if keyword_set(outdir) then use_outdir=outdir
  if keyword_set(plotdir) then use_plotdir=plotdir
  if keyword_set(reportdir) then use_reportdir=reportdir
  if keyword_set(tagname) then use_tagname=tagname
  if keyword_set(line_frequency) then use_line_frequency=line_frequency
  if keyword_set(target_beam) then use_target_beam=target_beam
  if keyword_set(xygrid) then use_xygrid=xygrid
  if keyword_set(vgrid) then use_vgrid=vgrid
  if keyword_set(galaxy) then use_galaxy=galaxy
  if keyword_set(noreport) then do_report = 0
  if keyword_set(jy2k) then do_jy2k = jy2k
  if keyword_set(rebaseline) then use_rebaseline = rebaseline
  
;===================
; enforce final back slash and make sure directories exist
;===================
  use_datadir=file_search(use_datadir,/mark,/full)
  use_outdir=file_search(use_outdir,/mark,/full)
  use_plotdir=file_search(use_plotdir,/mark,/full)
  use_reportdir=file_search(use_reportdir,/mark,/full)

  if use_datadir eq '' or use_outdir eq '' or use_plotdir eq '' then $
     message,'Problem with data/out/plot directories. Do they exist?'

  if do_report eq 1 and use_reportdir eq '' then $
     message,'Problem with report directory?'

;==============
; read data
;==============
  
  if keyword_set(verbose) then message,'Reading data',/info
  if keyword_set(fits_in1) then c1=readfits(use_datadir+use_c1file,c1hdr)
  if keyword_set(fits_in2) then c2=readfits(use_datadir+use_c2file,c2hdr)
  if keyword_set(idl_in1) then c1=idl_in1
  if keyword_set(idl_in2) then c2=idl_in2
  if keyword_set(hdr_in1) then c1hdr=hdr_in1
  if keyword_set(hdr_in2) then c2hdr=hdr_in2

  if n_elements(c1) eq 0 or $
     n_elements(c2) eq 0 or $
     n_elements(c1hdr) eq 0 or $
     n_elements(c2hdr) eq 0 then begin
     message,'Problem with input data and/or header information',/info
     goto, the_end
  end
  
;==============
; initalize the results structure
;==============
  
  ccmp_str=sfng_empty_cubecmp_str()
  ccmp_str.c1_file=use_c1file
  ccmp_str.c2_file=use_c2file
  
;==============
; check header information
; verify that units are K, km/s, and that cubes have beam and grid information in header.
;==============
     
  c1hdr_fix=c1hdr
  c1beam=fltarr(3)+nan
  c1pixscale=nan
  c1chanw=nan
  if keyword_set(verbose) then message,'Checking header for data: '+use_c1file,/info

  pass1=sfng_check_header(hdr=c1hdr, fixhdr=c1hdr_fix, comments = c1_comments $
                            , beam=c1_beam, pixscale=c1_pixscale, chanw=c1_chanw)
     
  c2hdr_fix=c2hdr
  c2beam=fltarr(3)+nan
  c2pixscale=nan
  c2chanw=nan
  if keyword_set(verbose) then message,'Checking header for data: '+use_c2file,/info

  pass2=sfng_check_header(hdr=c2hdr, fixhdr=c2hdr_fix, comments = c2_comments $
                            , beam=c2_beam, pixscale=c2_pixscale, chanw=c2_chanw)
   
     
  if pass1 ne 1 or pass2 ne 1 or keyword_set(verbose) then begin
     print,'&%&%&%&%&%&%&% CUBE 1 &%&%&%&%&%&%&%'
     print, c1_comments, format='(a)'
     print,'&%&%&%&%&%&%&% CUBE 2 &%&%&%&%&%&%&%'
     print, c2_comments, format='(a)'
     print,'&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%'
  end
        
  ccmp_str.c1_comments=strjoin(c1_comments,';')
  ccmp_str.c2_comments=strjoin(c2_comments,';')
  ccmp_str.c1_pixscale=c1_pixscale*3600.
  ccmp_str.c2_pixscale=c2_pixscale*3600.
  ccmp_str.c1_beam=c1_beam
  ccmp_str.c2_beam=c2_beam
  ccmp_str.c1_chanw=c1_chanw
  ccmp_str.c2_chanw=c2_chanw
  
  if pass1 ne 1 or pass2 ne 1 then begin
     print,'Headers did not pass sfng_check_header. Check logs, then .c to continue'
     stop
  end
     
  c1hdr = c1hdr_fix
  c2hdr = c2hdr_fix
  
  writefits,use_outdir+use_c1file+'.fixhdr.fits',c1,c1hdr
  writefits,use_outdir+use_c2file+'.fixhdr.fits',c2,c2hdr
  

;==============
; convert to K from Jy/beam
;==============

  if do_jy2k[0] ne 0 then begin

     c1K=sfng_convert_cube_jy2k(in = c1, hdr_in=c1hdr, hdr_out=new_c1hdr, factor=c1fact, freq=use_line_frequency)
     c1fact_str=strtrim(string(c1fact),2)
     c1_comments=[c1_comments,'Converted from Jy/beam to K with factor = '+c1fact_str]
     c1_comments=[c1_comments,'New units are K']
     ccmp_str.c1_comments=strjoin(c1_comments,';')
     ccmp_str.c1_jy2k_flag=1
     c1=c1K & c1hdr=new_c1hdr
     if keyword_set(verbose) then print, 'Converted Cube 1 from Jy/beam to K with factor = '+c1fact_str
     writefits,use_outdir+use_c1file+'.K.fits',c1,c1hdr
     
  end


  if do_jy2k[1] ne 0 then begin
     
     c2K=sfng_convert_cube_jy2k(in = c2, hdr_in=c2hdr, hdr_out=new_c2hdr, factor=c2fact, freq=use_line_frequency)
     c2fact_str=strtrim(string(c2fact),2)
     c2_comments=[c2_comments,'Converted from Jy/beam to K with factor = '+c2fact_str]
     c2_comments=[c2_comments,'New units are K']
     ccmp_str.c2_comments=strjoin(c2_comments,';')
     ccmp_str.c2_jy2k_flag=1
     c2=c2K & c2hdr=new_c2hdr
     if keyword_set(verbose) then print, 'Converted Cube 2 from Jy/beam to K with factor = '+c2fact_str
     writefits,use_outdir+use_c2file+'.K.fits',c2,c2hdr
     
  end
  
;==============
; match the cubes -- resolution and gridding parameters
;==============
  stop
  
   sfng_match_cubes,idl_in1=c1,idl_in2=c2,hdr_in1=c1hdr,hdr_in2=c2hdr $
                    ,idl_out1=c1match,idl_out2=c2match,hdr_out1=c1hdr_out,hdr_out2=c2hdr_out $
                   ,xygrid_master=use_xygrid,vgrid_master=use_vgrid,target_beam=use_target_beam,/round $
                   ,outdir=use_outdir,fits_out1=use_c1file+'.match.fits',fits_out2=use_c2file+'.match.fits'

   c1=c1match & c1hdr=c1hdr_out
   c2=c2match & c2hdr=c2hdr_out

   match_cdelt1=sxpar(c1hdr,'CDELT1',count=cdelt1_ct)
   match_cdelt3=sxpar(c1hdr,'CDELT3',count=cdelt3_ct)
   match_bmaj=sxpar(c1hdr,'BMAJ',count=bmaj_ct)
   match_pixdim=size(c1,/dim)
   nx=match_pixdim[0] & ny=match_pixdim[1] & nchans=match_pixdim[2]
   
   ccmp_str.pixscale_as=abs(match_cdelt1)*3600.
   ccmp_str.chanw_kms=abs(match_cdelt3)
   ccmp_str.angres_as=match_bmaj*3600.
   ccmp_str.dims=match_pixdim

   c1_comments=[c1_comments,'Regridded and brought to matching resolution']
   c1_comments=[c1_comments,'Pixscale is now [as]: '+strtrim(string(abs(match_cdelt1)*3600.),2)]
   c1_comments=[c1_comments,'Channel width is now [km/s]: '+strtrim(string(match_cdelt3),2)]
   c1_comments=[c1_comments,'Resolution is now [as]: '+strtrim(string(abs(match_bmaj)*3600.),2)]
   c1_comments=[c1_comments,'Dimensions are now [x,y,v]: '+strtrim(string(nx),2)+','+strtrim(string(ny),2)+','+strtrim(string(nchans),2)]
   ccmp_str.c1_comments=strjoin(c1_comments,';')


   c2_comments=[c2_comments,'Regridded and brought to matching resolution']
   c2_comments=[c2_comments,'Pixscale is now [as]: '+strtrim(string(abs(match_cdelt1)*3600.),2)]
   c2_comments=[c2_comments,'Channel width is now [km/s]: '+strtrim(string(match_cdelt3),2)]
   c2_comments=[c2_comments,'Resolution is now [as]: '+strtrim(string(abs(match_bmaj)*3600.),2)]
   c2_comments=[c2_comments,'Dimensions are now [x,y,v]: '+strtrim(string(nx),2)+','+strtrim(string(ny),2)+','+strtrim(string(nchans),2)]
   ccmp_str.c2_comments=strjoin(c2_comments,';')


;==============
; apply a blanking mask corresponding to the common field of view
;==============
   
   sfng_make_commonfov,idl_in1=c1,idl_in2=c2,hdr_in1=c1hdr,hdr_in2=c2hdr $
                       ,fits_outmask=use_tagname+'_commonfov_mask.fits',/apply $
                       ,fits_out1=use_c1file+'.commFoV.fits' $
                       ,fits_out2=use_c2file+'.commFoV.fits' $
                       ,idl_out1=c1fov,idl_out2=c2fov,hdr_out1=c1hdr_out,hdr_out2=c2hdr_out $
                       ,idl_outmask=common_fov_mask, hdr_outmask=common_fov_hdr

   c1=c1fov & c1hdr=c1hdr_out
   c2=c2fov & c2hdr=c2hdr_out

   ccmp_str.cmp_fov_fits=use_tagname+'_commonfov_mask.fits'

   c1_comments=[c1_comments,'Applied common FoV mask']
   ccmp_str.c1_comments=strjoin(c1_comments,';')
   c2_comments=[c2_comments,'Applied common FoV mask']
   ccmp_str.c2_comments=strjoin(c2_comments,';')

;====================
; simple rebaseline
;====================

   if use_rebaseline[0] ne -1 then begin
  
; mask regions of strong signal before estimating new baselines
; we write FITS of signal mask explicitly to avoid make_cprops_mask
; throwing an error
   
      make_cprops_mask, indata = c1 $
                        , outmask = c1sigmask $
                        , hi_thresh = 8 $
                        , lo_thresh = 1.5 $
                        , hi_nchan=2 $
                        , lo_nchan=2

      c1sigmsk_hdr=c1hdr
      sxaddpar,c1sigmsk_hdr,'BUNIT','Mask'
      sxaddpar,c1sigmsk_hdr,'DATAMAX',1
      sxaddpar,c1sigmsk_hdr,'DATAMIN',0
      sxaddpar,c1sigmsk_hdr,'HISTORY','Signal mask generated by make_cprops_mask'
      writefits,use_outdir+use_tagname+'_c1_basic_sigmask.fits',c1sigmask,c1sigmsk_hdr
      
      sfng_rebaseline,idl_in=c1 $
                      ,hdr_in=c1hdr $
                      ,idl_mask=c1sigmask $
                      ,idl_out=c1_rebase $
                      ,hdr_out=c1_rebase_hdr $
                      ,order=use_rebaseline[0] $
                      ,fits_out=use_tagname+'_c1_rebase' 
      
      c1=c1_rebase & c1hdr=c1_rebase_hdr 
      
      ccmp_str.c1_rebase_flag=do_rebase1
      ccmp_str.c1_rebase_order=use_rebaseline[0]
      
      c1_comments=[c1_comments,'Rebaselined cube with order: '+strtrim(string(use_rebaseline[0]),2)]
      ccmp_str.c1_comments=strjoin(c1_comments,';')
      
   end

   if use_rebaseline[1] ne -1 then begin
  
; mask regions of strong signal before estimating new baselines
; we write FITS of signal mask explicitly to avoid make_cprops_mask
; throwing an error
   
      make_cprops_mask, indata = c2 $
                        , outmask = c2sigmask $
                        , hi_thresh = 8 $
                        , lo_thresh = 1.5 $
                        , hi_nchan=2 $
                        , lo_nchan=2
      
      c2sigmask_hdr=c2hdr
      sxaddpar,c2sigmask_hdr,'BUNIT','Mask'
      sxaddpar,c2sigmask_hdr,'DATAMAX',1
      sxaddpar,c2sigmask_hdr,'DATAMIN',0
      sxaddpar,c2sigmask_hdr,'HISTORY','Signal mask generated by make_cprops_mask'
      writefits,use_outdir+use_tagname+'_c2_basic_sigmask.fits',c2sigmask,c2sigmask_hdr
      
      sfng_rebaseline,idl_in=c2 $
                      ,hdr_in=c2hdr $
                      ,idl_mask=c2sigmask $
                      ,idl_out=c2_rebase $
                      ,hdr_out=c2_rebase_hdr $
                      ,order=use_rebaseline[1] $
                      ,fits_out=use_tagname+'_c2_rebase' 
      
      c2=c2_rebase & c2hdr=c2_rebase_hdr 
      
      ccmp_str.c2_rebase_flag=do_rebase2
      ccmp_str.c2_rebase_order=use_rebaseline[1]
      
      c2_comments=[c2_comments,'Rebaselined cube with order: '+strtrim(string(use_rebaseline[1]),2)]
      ccmp_str.c2_comments=strjoin(c2_comments,';')
      
   end
   
;==============
; generate noise map
;==============

   use_midx=fix(nx/2.)
   use_midy=fix(ny/2.)
   use_midchan=fix(nchans/2.)

   make_noise_cube, cube_in = c1 $
                    , cube_hdr = c1hdr $
                    , out_cube = c1noise $
                    , mask_in = c1sigmask $
                    , /iterate $
                    , box=use_avgbox $
                    , spec_box=use_avgspecbox

   c1noise_hdr=c1hdr
   sxaddpar,c1noise_hdr,'DATAMAX',max(c1noise,/nan)
   sxaddpar,c1noise_hdr,'DATAMIN',min(c1noise,/nan)
   sxaddpar,c1noise_hdr,'HISTORY','Noise cube generated by make_noise_cube'
   writefits,use_outdir+use_tagname+'_c1noise.fits',c1noise,c1noise_hdr

   make_noise_cube, cube_in = c1 $
                    , cube_hdr = c1hdr $
                    , out_cube = c1noise2d $
                    , mask_in = c1sigmask $
                    , /iterate $
                    , /twod $
                    , box=use_avgbox $
                    , spec_box=use_avgspecbox

   c1noise2d=c1noise[*,*,use_midchan]
   c1noise2d_hdr=twod_head(c1hdr)
   sxaddpar,c1noise2d_hdr,'DATAMAX',max(c1noise2d,/nan)
   sxaddpar,c1noise2d_hdr,'DATAMIN',min(c1noise2d,/nan)
   sxaddpar,c1noise2d_hdr,'HISTORY','Noise map generated by make_noise_cube'
   writefits,use_outdir+use_tagname+'_c1noise2d.fits',c1noise2d,c1noise2d_hdr
   
   make_noise_cube, cube_in = c2 $
                    , cube_hdr = c2hdr $
                    , out_cube = c2noise $
                    , mask_in = c2sigmask $
                    , /iterate $
                    , box=use_avgbox $
                    , spec_box=use_avgspecbox

   c2noise_hdr=c2hdr
   sxaddpar,c2noise_hdr,'DATAMAX',max(c2noise,/nan)
   sxaddpar,c2noise_hdr,'DATAMIN',min(c2noise,/nan)
   sxaddpar,c2noise_hdr,'HISTORY','Noise cube generated by make_noise_cube'
   writefits,use_outdir+use_tagname+'_c2noise.fits',c2noise,c2noise_hdr

   make_noise_cube, cube_in = c1 $
                    , cube_hdr = c1hdr $
                    , out_cube = c1noise2d $
                    , mask_in = c1sigmask $
                    , /iterate $
                    , /twod $
                    , box=use_avgbox $
                    , spec_box=use_avgspecbox

   c2noise2d=c2noise[*,*,use_midchan]
   c2noise2d_hdr=twod_head(c2hdr)
   sxaddpar,c2noise2d_hdr,'DATAMAX',max(c2noise2d,/nan)
   sxaddpar,c2noise2d_hdr,'DATAMIN',min(c2noise2d,/nan)
   sxaddpar,c2noise2d_hdr,'HISTORY','Noise map generated by make_noise_cube'
   writefits,use_outdir+use_tagname+'_c2noise2d.fits',c2noise2d,c2noise2d_hdr

   ccmp_str.c1_rmsmap_fits=use_tagname+'_c1noise2d.fits'
   ccmp_str.c2_rmsmap_fits=use_tagname+'_c2noise2d.fits'
   ccmp_str.c1_rmscube_fits=use_tagname+'_c1noise.fits'
   ccmp_str.c2_rmscube_fits=use_tagname+'_c2noise.fits'

;==============
; enlarge FoV mask, e.g. to exclude noisy edge pixels/channels 
;==============

if total(use_expand_mask_edges) ne 0 then begin

   use_pixel_blank=use_expand_mask_edges[0]
   use_chan_blank=use_expand_mask_edges[1]
   
   sfng_enlarge_mask,idl_in=c1,hdr_in=c1hdr $
                     ,idl_out=c1_newmask, hdr_out=c1hdrout $
                     ,pixel_blank=use_pixel_blank,chan_blank=use_chan_blank $
                     ,/apply $
                     ,outdir=use_outdir $
                     ,idl_mask=common_fov_mask_exp $
                     ,fits_mask=use_tagname+'_finalfovmask.fits' $
                     ,fits_out=use_tagname+'_c1_finalfovmask.fits'

   sfng_enlarge_mask,idl_in=c2,hdr_in=c2hdr $
                     ,idl_out=c2_newmask, hdr_out=c2hdrout $
                     ,pixel_blank=use_pixel_blank,chan_blank=use_chan_blank $
                     ,/apply $
                     ,outdir=use_outdir $
                     ,fits_out=use_tagname+'_c2_finalfovmask.fits'
   
   c1=c1_newmask & c1hdr=c1hdrout
   c2=c2_newmask & c2hdr=c2hdrout
   common_fov_mask=common_fov_mask_exp
   
   ccmp_str.cmp_fov_fits=use_tagname+'_finalfovmask.fits'


   c1_comments=[c1_comments,'Restricted FoV mask by '+strtrim(string(use_pixel_blank),2)+ $
                ' pixels, and '+strtrim(string(use_chan_blank),2)+' channels']
   ccmp_str.c1_comments=strjoin(c1_comments,';')
   c2_comments=[c2_comments,'Restricted FoV mask by '+strtrim(string(use_pixel_blank),2)+ $
                ' pixels, and '+strtrim(string(use_chan_blank),2)+' channels']
   ccmp_str.c2_comments=strjoin(c2_comments,';')

end

;==============
; final significant emission masks (after rebaselining) for each cube
; and joint signal/nosignal masks
;==============

   use_hithreshstr=strtrim(string(use_hithresh),2)
   use_lothreshstr=strtrim(string(use_lothresh),2)

   make_cprops_mask, indata = c1 $
                     , outmask = c1sigmask $
                     , hi_thresh = use_hithresh $
                     , lo_thresh = use_lothresh $
                     , hi_nchan=2 $
                     , lo_nchan=2

   c1sigmsk_hdr=c1hdr
   sxaddpar,c1sigmsk_hdr,'BUNIT','Mask'
   sxaddpar,c1sigmsk_hdr,'DATAMAX',1
   sxaddpar,c1sigmsk_hdr,'DATAMIN',0
   sxaddpar,c1sigmsk_hdr,'HISTORY','Final signal mask generated by make_cprops_mask'
   sxaddpar,c1sigmsk_hdr,'HISTORY','Hi Thresh :'+use_hithreshstr+', Lo Thresh: '+use_lothreshstr
   writefits,use_outdir+use_tagname+'_c1_signalmask.fits',c1sigmask,c1sigmsk_hdr

   make_cprops_mask, indata = c2 $
                     , outmask = c2sigmask $
                     , hi_thresh = use_hithresh $
                     , lo_thresh = use_lothresh $
                     , hi_nchan=2 $
                     , lo_nchan=2

   c2sigmsk_hdr=c2hdr
   sxaddpar,c2sigmsk_hdr,'BUNIT','Mask'
   sxaddpar,c2sigmsk_hdr,'DATAMAX',1
   sxaddpar,c2sigmsk_hdr,'DATAMIN',0
   sxaddpar,c2sigmsk_hdr,'HISTORY','Final signal mask generated by make_cprops_mask'
   sxaddpar,c2sigmsk_hdr,'HISTORY','Hi Thresh :'+use_hithreshstr+', Lo Thresh: '+use_lothreshstr
   writefits,use_outdir+use_tagname+'_c2_signalmask.fits',c2sigmask,c2sigmsk_hdr

   joint_sigmask=(c1sigmask eq 1) and (c2sigmask eq 1)
   no_sigmask=(c1sigmask eq 0) and (c2sigmask eq 0) and (finite(c1) eq 1) and (finite(c2) eq 1)
   
   jsigmsk_hdr=c1sigmsk_hdr
   writefits,use_outdir+use_tagname+'_jointsignalmask.fits',joint_sigmask,jsigmsk_hdr
   nosigmsk_hdr=c1sigmsk_hdr
   writefits,use_outdir+use_tagname+'_nosignalmask.fits',no_sigmask,nosigmsk_hdr

   ccmp_str.c1_signalmask_fits=use_tagname+'_c1_signalmask.fits'
   ccmp_str.c2_signalmask_fits=use_tagname+'_c2_signalmask.fits'
   ccmp_str.joint_signalmask_fits=use_tagname+'_jointsignalmask.fits'
   ccmp_str.joint_nosignalmask_fits=use_tagname+'_nosignalmask.fits'
   
;======================
; start of comparison tests 
;======================

; difference cube   
   diffcube=c1-c2

; expand fov mask to 3d if needed
   fovdim=size(common_fov_mask)
   if fovdim[0] eq 2 then begin
      common_fov_mask_exp=fltarr(fovdim[1],fovdim[2],nchans)
      for k=0,nchans-1 do begin
         common_fov_mask_exp[*,*,k]=common_fov_mask
      endfor
      common_fov_mask=common_fov_mask_exp
   end
   
;======================
; STATISTICS RE EMISSION EXTENT
;======================

   fovidx=where(common_fov_mask eq 1, fovct)  
   c1sigidx=where(c1sigmask eq 1, c1ct, comp=c1nosigidx)
   c2sigidx=where(c2sigmask eq 1, c2ct, comp=c2nosigidx)
   jsigidx=where(joint_sigmask eq 1, jct)
   nosigidx=where(no_sigmask eq 1, nsct)
   
   ccmp_str.npix_cmp_fov=fovct
   ccmp_str.c1_npix_signalmask=c1ct
   ccmp_str.c2_npix_signalmask=c2ct
   ccmp_str.npix_jointsignalmask=jct
   ccmp_str.npix_nosignalmask=nsct

;======================
; STATISTICS RE TOTAL FLUX
;======================
   
  ccmp_str.c1_totflux=total(c1,/nan)
  ccmp_str.c2_totflux=total(c2,/nan)
  ccmp_str.totfluxdiff=total(diffcube,/nan)
  if c1ct gt 0 then ccmp_str.c1_totflux_signalmask=total(c1[c1sigidx],/nan)
  if c2ct gt 0 then ccmp_str.c2_totflux_signalmask=total(c2[c2sigidx],/nan)
  if nsct gt 0 then ccmp_str.c1_totflux_nosignalmask=total(c1[nosigidx],/nan)
  if nsct gt 0 then ccmp_str.c2_totflux_nosignalmask=total(c2[nosigidx],/nan)
  if jct gt 0 then  ccmp_str.c1_totflux_jointsignalmask=total(c1[jsigidx],/nan)
  if jct gt 0 then  ccmp_str.c2_totflux_jointsignalmask=total(c2[jsigidx],/nan)
  if jct gt 0 then  ccmp_str.totfluxdiff_jointsignalmask=total(diffcube[jsigidx],/nan)
  if nsct gt 0 then ccmp_str.totfluxdiff_nosignalmask=total(diffcube[nosigidx],/nan)

  ccmp_str.npix_cmp_fov=fovct
  ccmp_str.c1_npix_signalmask=c1ct
  ccmp_str.c2_npix_signalmask=c2ct
  ccmp_str.npix_jointsignalmask=jct
  ccmp_str.npix_nosignalmask=nsct

;======================
; STATISTICS RE FLUX PER CHANNEL
;======================

  c1_chanflux=fltarr(nchans)
  c2_chanflux=fltarr(nchans)
  c1_chanflux_jsm=fltarr(nchans)
  c2_chanflux_jsm=fltarr(nchans)
  c1_chanflux_nosm=fltarr(nchans)
  c2_chanflux_nosm=fltarr(nchans)
  diff_chanflux=fltarr(nchans)
  diff_chanflux_jsm=fltarr(nchans)
  diff_chanflux_nosm=fltarr(nchans)
  c1_chanrms=fltarr(nchans)
  c2_chanrms=fltarr(nchans)
  c1_chanrms_nosm=fltarr(nchans)
  c2_chanrms_nosm=fltarr(nchans)

  for i=0,nchans-1 do begin

     c1_thisplane=c1[*,*,i]
     c1_noise_thisplane=c1noise[*,*,i]
     c2_thisplane=c2[*,*,i]
     c2_noise_thisplane=c2noise[*,*,i]
     jsm_thisplane=joint_sigmask[*,*,i]
     nosm_thisplane=no_sigmask[*,*,i]
     fov_thisplane=common_fov_mask[*,*,i]
     
     c1_thisplane_fov_idx=where(fov_thisplane eq 1 and finite(c1_thisplane) eq 1,fc1ct)
     c2_thisplane_fov_idx=where(fov_thisplane eq 1 and finite(c2_thisplane) eq 1,fc2ct)
     c1_thisplane_jsm_idx=where(jsm_thisplane eq 1 and finite(c1_thisplane) eq 1,c1ct)
     c2_thisplane_jsm_idx=where(jsm_thisplane eq 1 and finite(c2_thisplane) eq 1,c2ct)
     c1_thisplane_nosm_idx=where(nosm_thisplane eq 1 and finite(c1_thisplane) eq 1,nc1ct)
     c2_thisplane_nosm_idx=where(nosm_thisplane eq 1 and finite(c2_thisplane) eq 1,nc2ct)
     
     c1_chanflux[i]=total(c1_thisplane,/nan)
     c2_chanflux[i]=total(c2_thisplane,/nan)
     if c1ct gt 0 then c1_chanflux_jsm[i]=total(c1_thisplane[c1_thisplane_jsm_idx],/nan)
     if c2ct gt 0 then c2_chanflux_jsm[i]=total(c2_thisplane[c2_thisplane_jsm_idx],/nan)
     if nc1ct gt 0 then c1_chanflux_nosm[i]=total(c1_thisplane[c1_thisplane_nosm_idx],/nan)
     if nc2ct gt 0 then c2_chanflux_nosm[i]=total(c2_thisplane[c2_thisplane_nosm_idx],/nan)

     diff_chanflux[i]=c1_chanflux[i]-c2_chanflux[i]
     if c1ct gt 0 and c2ct gt 0 then diff_chanflux_jsm[i]=c1_chanflux_jsm[i]-c2_chanflux_jsm[i]
     if nc1ct gt 0 and nc2ct gt 0 then diff_chanflux_nosm[i]=c1_chanflux_nosm[i]-c2_chanflux_nosm[i]

     if nc1ct gt 0 then c1_chanrms_nosm[i]=robust_sigma(c1_thisplane[c1_thisplane_nosm_idx])
     if nc2ct gt 0 then c2_chanrms_nosm[i]=robust_sigma(c2_thisplane[c2_thisplane_nosm_idx])
     if fc1ct gt 0 then c1_chanrms[i]=robust_sigma(c1_thisplane[c1_thisplane_fov_idx])
     if fc2ct gt 0 then c2_chanrms[i]=robust_sigma(c2_thisplane[c2_thisplane_fov_idx])

  endfor

  ccmp_str.c1_fluxperchan=ptr_new(c1_chanflux)
  ccmp_str.c2_fluxperchan=ptr_new(c2_chanflux)
  ccmp_str.c1_fluxperchan_jointsignalmask=ptr_new(c1_chanflux_jsm)
  ccmp_str.c2_fluxperchan_jointsignalmask=ptr_new(c2_chanflux_jsm)
  ccmp_str.c1_rmsperchan=ptr_new(c1_chanrms)
  ccmp_str.c2_rmsperchan=ptr_new(c2_chanrms)
  ccmp_str.c1_rmsperchan_nosignalmask=ptr_new(c1_chanrms_nosm)
  ccmp_str.c2_rmsperchan_nosignalmask=ptr_new(c2_chanrms_nosm)
  ccmp_str.fluxdiffperchan=ptr_new(diff_chanflux)
  ccmp_str.fluxdiffperchan_jointsignalmask=ptr_new(diff_chanflux_jsm)
  ccmp_str.fluxdiffperchan_nosignalmask=ptr_new(diff_chanflux_nosm)

  ccmp_str.c1_noisestats=sfng_get_basic_stats(c1[c1nosigidx])
  ccmp_str.c2_noisestats=sfng_get_basic_stats(c2[c2nosigidx])
  ccmp_str.diffcube_stats=sfng_get_basic_stats(diffcube)

;======================
; generate 'spectra' -- flux/rms per chan figures
;======================

  chans=indgen(nchans)+1
  make_axes,c1hdr,vaxis=vchans,/vonly

;======================
; a. Flux per channel
;======================

  use_pngfile=use_tagname+'flux_per_channel.png'
  
  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  !p.position=[0.2,0.2,0.9,0.8]
  loadct,39 & fgcolor=255
  xr=[0,nchans] & xr1=[vchans[0],vchans[-1]]
  ymax=max(c1_chanflux,/nan) > max(c2_chanflux,/nan)
  yr=[-0.1*ymax,1.1*ymax]
  tit='Input Cubes, Common FoV'
  
  cgplot,chans,c1_chanflux,xtit='Channel number',ytit='Sum(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanflux,color=fsc_color('red'),psym=10,thick=2,/overplot
  cgplot,chans,c2_chanflux,color=fsc_color('blue'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2
  
  al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_c1str, $
               use_c2str] $
            ,lines=[-99,0,0] $
            ,colors=[fgcolor,fsc_color('red'),fsc_color('blue')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; b. Flux within joint signal mask per channel
;======================

  use_pngfile=use_tagname+'flux_per_channel_jointsignalmask.png'
  
  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  !p.position=[0.2,0.2,0.9,0.8]
  xr=[0,nchans] & xr1=[vchans[0],vchans[-1]]
  ymax=max(c1_chanflux,/nan) > max(c2_chanflux,/nan)
  yr=[-0.1*ymax,1.1*ymax]
  tit='Input Cubes, Common FoV, JointSigMask'
  
  cgplot,chans,c1_chanflux_jsm,xtit='Channel number',ytit='Sum(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanflux_jsm,color=fsc_color('red'),psym=10,thick=2,/overplot
  cgplot,chans,c2_chanflux_jsm,color=fsc_color('blue'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2
  
  al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_c1str, $
               use_c2str] $
            ,lines=[-99,0,0] $
            ,colors=[fgcolor,fsc_color('red'),fsc_color('blue')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; c. Flux difference per channel
;======================

  use_pngfile=use_tagname+'diffcube_flux_per_channel.png'

  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  ymax=max(diff_chanflux,/nan)
  ymin=min(diff_chanflux,/nan)
  
  if ymin lt 0 then yr=[1.1*ymin,1.1*ymax]
  if ymin ge 0 then yr=[0.8*ymin,1.1*ymax]
  tit='Difference Cube, Common FoV'
  
  cgplot,chans,diff_chanflux,xtit='Channel number',ytit='Sum(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,diff_chanflux,psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2
  
  al_legend,/bottom,/left,clear=0, box=0 $
            , [tit, $
               'C1-C2'] $
            ,lines=[-99,0] $
            ,charsize=1.,thick=2,charthick=1.8

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; d. Flux difference per channel inside the joint signal mask
;======================

  use_pngfile=use_tagname+'diffcube_flux_per_channel_jointsignalmask.png'

  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  ymax=max(diff_chanflux_jsm,/nan)
  ymin=min(diff_chanflux_jsm,/nan)
  
  if ymin lt 0 then yr=[1.1*ymin,1.1*ymax]
  if ymin ge 0 then yr=[0.8*ymin,1.1*ymax]
  tit='Difference Cube, Common FoV, JointSigMask'
  
  cgplot,chans,diff_chanflux_jsm,xtit='Channel number',ytit='Sum(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,diff_chanflux_jsm,psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2
  
  al_legend,/bottom,/left,clear=0, box=0 $
            , [tit, $
               'C1-C2'] $
            ,lines=[-99,0] $
            ,charsize=1.,thick=2,charthick=1.8

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

  
;======================
; a. RMS per channel
;======================

  use_pngfile=use_tagname+'rms_per_channel.png'

  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  ymax=max(c1_chanrms,/nan) > max(c2_chanrms,/nan)
  ymin=min(c1_chanrms,/nan) < min(c2_chanrms,/nan)
  
  if ymin lt 0 then yr=[1.1*ymin,1.1*ymax]
  if ymin ge 0 then yr=[0.8*ymin,1.1*ymax]
  tit='Input Cubes, Common FoV'
  
  cgplot,chans,c1_chanrms,xtit='Channel number',ytit='RMS(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanrms,color=fsc_color('red'),psym=10,thick=2,/overplot
  cgplot,chans,c2_chanrms,color=fsc_color('blue'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2

  
  al_legend,/bottom,/left,clear=0, box=0 $
            , [tit, $
               use_c1str, $
               use_c2str] $
            ,lines=[-99,0,0] $
            ,colors=[fgcolor,fsc_color('red'),fsc_color('blue')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)


  
;======================
; b. RMS per channel in nosigmask
;======================

  use_pngfile=use_tagname+'rms_per_channel_nosignalmask.png'

  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  ymax=max(c1_chanrms_nosm,/nan) > max(c2_chanrms_nosm,/nan)
  ymin=min(c1_chanrms_nosm,/nan) < min(c2_chanrms_nosm,/nan)
  
  if ymin lt 0 then yr=[1.1*ymin,1.1*ymax]
  if ymin ge 0 then yr=[0.8*ymin,1.1*ymax]
  tit='Input Cubes, Common FoV, No Signal Mask'
  
  cgplot,chans,c1_chanrms_nosm,xtit='Channel number',ytit='RMS(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanrms_nosm,color=fsc_color('red'),psym=10,thick=2,/overplot
  cgplot,chans,c2_chanrms_nosm,color=fsc_color('blue'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2

  
  al_legend,/bottom,/left,clear=0, box=0 $
            , [tit, $
               use_c1str, $
               use_c2str] $
            ,lines=[-99,0,0] $
            ,colors=[fgcolor,fsc_color('red'),fsc_color('blue')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; CORRELATION METRICS
;======================

  ; linear correlation between pixel values in C1 and C2
  startguess=[1.0,1.0]

  fitidx=where(finite(c1) eq 1 and finite(c2) eq 1 and joint_sigmask eq 1 and $
                  finite(c1noise) eq 1 and finite(c2noise) eq 1,fitct)

  res_linc1c2=linear_mpfit(c1[fitidx],c2[fitidx],c1noise[fitidx],c2noise[fitidx] $
                           ,startguess,/quiet)
  
  res_linc2c1=linear_mpfit(c2[fitidx],c1[fitidx],c2noise[fitidx],c1noise[fitidx] $
                           ,startguess,/quiet)

  fitidx_noneg=where(finite(c1) eq 1 and finite(c2) eq 1 and joint_sigmask eq 1 and $
                     finite(c1noise) eq 1 and finite(c2noise) eq 1 and $
                     c1 gt 0 and c2 gt 0, fitct_noneg)

  res_logc1c2=linear_mpfit(alog10(c1[fitidx_noneg]),alog10(c2[fitidx_noneg]),c1noise[fitidx_noneg],c2noise[fitidx_noneg] $
                           ,startguess,/quiet)
  
  res_logc2c1=linear_mpfit(alog10(c2[fitidx_noneg]),alog10(c1[fitidx_noneg]),c2noise[fitidx_noneg],c1noise[fitidx_noneg] $
                           ,startguess,/quiet)

  ccmp_str.rpx_c1c2=correlate(c1[fitidx],c2[fitidx])
  ccmp_str.rank_c1c2=r_correlate(c1[fitidx],c2[fitidx])

  ccmp_str.lincorr_c1c2=res_linc1c2
  ccmp_str.lincorr_c2c1=res_linc2c1
  ccmp_str.logcorr_c1c2=res_logc1c2
  ccmp_str.logcorr_c2c1=res_logc2c1

;======================
; generate correlation plots
;======================

  use_pngfile=use_tagname+'lincorr_c1c2.png'

  plotmax=1.25*(max(c1[jsigidx],/nan)>max(c2[jsigidx],/nan))
  xaxis=plotmax*findgen(100)/100.
  yfit=xaxis*res_linc1c2[0]+res_linc1c2[1]
  
  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  xr=[-0.05,plotmax] & yr=[-0.05,plotmax]

  cgplot,c1[jsigidx],c2[jsigidx],xtit=use_c1str,ytit=use_c2str,tit='Pixel-pixel correlation inside common mask', $
       charsize=1.5,/nodata,/xsty,/ysty,xr=xr,yr=yr,xthick=2,ythick=2,thick=2,charthick=1.8
  cgplot,c1[jsigidx],c2[jsigidx],psym=3,color=fsc_color('grey'),/overplot,thick=2
  cgplot,xaxis,yfit,color=fsc_color('blue'),lines=2,/overplot,thick=3
  equality,color=fsc_color('black')

  plotsym,0,1.2,/fill
  
  xmin = 0. & xmax = plotmax & binsize = round(plotmax*10.)/100.
  bin_prof, [c1[jsigidx]] $
            ,[c2[jsigidx]] $
            , xmin=xmin, xmax=xmax, binsize=binsize $
            , xmid=xmid, medprof=medprof, madprof=madprof
  oploterror, xmid, medprof, madprof $
              , errcolor=fsc_color('red'), color=fsc_color('red') $
              , errthick=2, psym=8, symsize=1.5
  oplot, xmid, medprof $
         , psym=8, symsize=1.5, color=fsc_color('red')

  
  al_legend,/top,/left,clear=0, box=0 $
            , ['Equality','Slope: '+sigfig(res_linc1c2[0],3)] $
            ,colors=[fsc_color('black'),fsc_color('blue')] $
            ,charsize=1.5,thick=[2,3],linsize=0.5,lines=[0,2],charthick=1.5

  al_legend,/bottom,/right,clear=0, box=0 $
            , ['Running median, y on x'] $
            ,colors=[fsc_color('red')] $
            ,charsize=1.5,psym=8,charthick=1.5

    write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

  stop
  
  the_end:
  return
  
end