pro sfng_cube_compare,datadir=datadir,outdir=outdir,plotdir=plotdir,reportdir=reportdir $
                      ,fits_in1=fits_in1,fits_in2=fits_in2 $
                      ,idl_in1=idl_in1,idl_in2=idl_in2 $
                      ,hdr_in1=hdr_in1,hdr_in2=hdr_in2 $
                      ,c1_name=c1_name,c2_name=c2_name $
                      ,savedir=savedir,savefile=savefile,tagname=tagname $
                      ,galaxy=galaxy, line_frequency=line_frequency, target_beam=target_beam $
                      ,expand_mask_edges=expand_mask_edges $
                      ,jy2k=jy2k, rebaseline=rebaseline $
                      ,xygrid=xygrid,vgrid=vgrid $
                      ,allchannels=allchannels $
                      ,help=help,verbose=verbose,noreport=noreport,nostop=nostop $
                      ,nice=nice

;+ NAME:
;     sfng_cube_compare
; PURPOSE:
;     compares two cubes. Essentially a wrapper to a library of subroutines that bring
;     cubes to a matching grid, resolution, common field-of-view etc., and calculate
;     various statistics about emission and emission-free regions in the cubes.
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
;     savedir = output directory for IDL save structure. Defaults to
;                current directory
;     jy2k = two-element vector representing flag (0-no,1-yes) about
;            whether cube requires conversion from Jy to K. Default is no.
;     rebaseline = two-element vector indicating whether profiles in cube requires rebaselining.
;                  -1: don't rebaseline, 0: remove offset, 1:
;                  linear, 2: quadratic. Default is -1 (no
;                  rebaselining done).
;     tagname = string that will be used for FITS filename prefix. Defaults
;               to 'mygalaxy'. 
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
;                        FoV that is common to both cubes) 
; ACCEPTED KEY-WORDS:
;     help = print this help
;     verbose = print extra information to screen
;     noreport = don't generate a PDF file, just make the plots
;                and IDL save structure of results
;     nostop = exit the routine when finished. If not set, execution
;              will pause just before return statement at end of routine.
;     allchannels = produce channel maps for all channels. Default is
;              to use the channels with emission, with a 20km/s margin
;              on each side.
;     nice  =  pause before deleting existing .png and .tex files in
;              the plots and report directories. Pause to review
;              header information
; EXAMPLES
;       sfng_cube_compare,datadir=use_datadir,outdir=use_outdir,plotdir=use_plotdir $
;                    ,reportdir=use_reportdir,savedir=use_savedir $
;                    , fits_in1=use_c1file,fits_in2=use_c2file,savefile=use_savefile $
;                    , xygrid=2,vgrid=1,jy2k=[0,1],rebaseline=[1,1],expand_mask_edges=[5,2] $
;                    , target_beam=[30.,30.,0],/verb,/nice
; OUTPUTS:
;     savefile = IDL save file with structure containing results of comparison
;     Plots (in plotdir) and, if requested, a summary report (in reportdir)
; PROCEDURE AND SUBROUTINE USED
;     Goddard IDLAstro library (not provided)
;     Freudenreich Robust routines (contrib to IDLAstro, not provided)
;     AKL CPROPTOO library (not provided)
;     AKL GAL_BASE library (not provided)
;     Coyote Graphics (not provided)
;     JPBlib: (provided in aux/ subdirectory)
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
  sfng_define_la_common
  
  ; things that user can change via keywords
  use_savefile='cubecmp_results.sav'
  use_c1file='IDL_CUBE1'
  use_c2file='IDL_CUBE2'
  use_datadir = './orig_data/'
  use_savedir = './savefiles/'
  use_outdir ='./good_data/'
  use_plotdir = './plots/'
  use_reportdir = './report/'
  use_tagname = 'mygalaxy'
  use_line_frequency = restfreq_12co21/1.e9
  use_jy2k = [0,0] ; cube [1,2] -- 1 means do Jy->K conversion
  use_rebaseline = [-1,-1] ; -1 (do nothing) 0 (offset), 1 (linear) or 2 (quadratic) -- one value for each cube
  use_expand_mask_edges = [0,0] ; [# edge_pixels, # edge_channels] to be blanked (in addition to common FoV)
  use_xygrid=1 ; which cube is master for (x,y) pixel grid?
  use_vgrid=1 ; which cube is master for channelisation?
  do_report=1
  use_allchannels=0
  use_c1str='CUBE1'
  use_c2str='CUBE2'

  ; things that user can change here 
  use_win = 0L
  use_avgbox=3
  use_avgspecbox=0
  use_hithresh=5
  use_lothresh=2
  nan=!values.f_nan
  ms2kms=1/1000.d
  kms2ms=1000.d
  Mpc_on_AU=1.e6/206265.
  use_binsize_2dfactor=20.
  
;===================
; process user inputs
;===================

  if keyword_set(fits_in1) then use_c1file=fits_in1
  if keyword_set(fits_in2) then use_c2file=fits_in2
  if keyword_set(datadir) then use_datadir=datadir
  if keyword_set(outdir) then use_outdir=outdir
  if keyword_set(plotdir) then use_plotdir=plotdir
  if keyword_set(reportdir) then use_reportdir=reportdir
  if keyword_set(savedir) then use_savedir=savedir
  if keyword_set(tagname) then use_tagname=tagname
  if keyword_set(line_frequency) then use_line_frequency=line_frequency
  if keyword_set(target_beam) then use_target_beam=target_beam
  if keyword_set(xygrid) then use_xygrid=xygrid
  if keyword_set(vgrid) then use_vgrid=vgrid
  if keyword_set(galaxy) then use_galaxy=galaxy
  if keyword_set(noreport) then do_report = 0
  if keyword_set(allchannels) then use_allchannels = 1
  if keyword_set(jy2k) then do_jy2k = jy2k
  if keyword_set(rebaseline) then use_rebaseline = rebaseline
  if keyword_set(c1_name) then use_c1str=c1_name
  if keyword_set(c2_name) then use_c2str=c2_name

;===================
; enforce final back slash and make sure directories exist
;===================
  use_datadir=file_search(use_datadir,/mark,/full)
  use_outdir=file_search(use_outdir,/mark,/full)
  use_plotdir=file_search(use_plotdir,/mark,/full)
  use_reportdir=file_search(use_reportdir,/mark,/full)
  use_savedir=file_search(use_savedir,/mark,/full)

  if use_datadir eq '' or use_outdir eq '' or $
     use_plotdir eq '' or use_savedir eq '' then $
     message,'Problem with data/out/plot/save directories. Do they exist?'

  if do_report eq 1 and use_reportdir eq '' then $
     message,'Problem with report directory?'

;==============
; clean output directories
;==============

  message,'Removing old files from report directory: '+use_reportdir,/info
  if keyword_set(nice) then stop
  spawn, 'rm -f '+use_reportdir+'/*tex'
  spawn, 'rm -f '+use_reportdir+'/*png'

  message,'Removing old files from plots directory: '+use_plotdir,/info
  if keyword_set(nice) then stop
  spawn, 'rm -f '+use_plotdir+'/*png'
  
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
; initialise galaxy information if set
;==============

  if keyword_set(use_galaxy) then $
     gstr=gal_data(use_galaxy)

;==============
; check header information
; verify that units are K, km/s, and that cubes have beam and grid information in header.
;==============
     
  if keyword_set(verbose) then message,'Checking header for data: '+use_c1file,/info

  c1hdr_fix=c1hdr
  pass1=sfng_check_header(hdr=c1hdr, fixhdr=c1hdr_fix, comments = c1_comments $
                            , beam=c1_beam, pixscale=c1_pixscale, chanw=c1_chanw, unit=c1_bunit)
     
  if keyword_set(verbose) then message,'Checking header for data: '+use_c2file,/info

  c2hdr_fix=c2hdr
  pass2=sfng_check_header(hdr=c2hdr, fixhdr=c2hdr_fix, comments = c2_comments $
                            , beam=c2_beam, pixscale=c2_pixscale, chanw=c2_chanw, unit=c2_bunit)
   
     
  if pass1 ne 1 or pass2 ne 1 or keyword_set(verbose) then begin
     print,'&%&%&%&%&%&%&% CUBE 1 &%&%&%&%&%&%&%'
     print, c1_comments, format='(a)'
     print,'&%&%&%&%&%&%&% CUBE 2 &%&%&%&%&%&%&%'
     print, c2_comments, format='(a)'
     print,'&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%'
;     stop
  end
        
  ccmp_str.c1_comments=strjoin(c1_comments,';')
  ccmp_str.c2_comments=strjoin(c2_comments,';')
  ccmp_str.c1_pixscale=c1_pixscale*3600.
  ccmp_str.c2_pixscale=c2_pixscale*3600.
  ccmp_str.c1_beam=c1_beam
  ccmp_str.c2_beam=c2_beam
  ccmp_str.c1_bunit=c1_bunit
  ccmp_str.c2_bunit=c2_bunit
  ccmp_str.c1_chanw=c1_chanw
  ccmp_str.c2_chanw=c2_chanw
  ccmp_str.c1_dims=size(c1,/dim)
  ccmp_str.c2_dims=size(c2,/dim)
  
  if pass1 ne 1 or pass2 ne 1 then begin
     print,'Headers did not pass sfng_check_header.'
     if keyword_set(nice) then begin
        print,'Check logs, then .c to continue'
        stop
     end
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
   chanw_kms=abs(match_cdelt3)
   pixscale_as=abs(match_cdelt1)*3600.
   angres_as=match_bmaj*3600.
   
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
                       ,outdir=use_outdir $
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
                      ,fits_out=use_tagname+'_c1_rebase' $
                      ,outdir=use_outdir 

      c1=c1_rebase & c1hdr=c1_rebase_hdr 
      
      ccmp_str.c1_rebase_flag=1
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
                      ,fits_out=use_tagname+'_c2_rebase' $
                      ,outdir=use_outdir 

      c2=c2_rebase & c2hdr=c2_rebase_hdr 
      
      ccmp_str.c2_rebase_flag=1
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

   c1_noisestats=sfng_get_basic_stats(c1noise,/nan)

   make_noise_cube, cube_in = c1 $
                    , cube_hdr = c1hdr $
                    , out_cube = c1noise2d $
                    , mask_in = c1sigmask $
                    , /iterate $
                    , /twod $
                    , box=use_avgbox $
                    , spec_box=use_avgspecbox

   c1noise2d=c1noise2d[*,*,use_midchan]
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

   c2_noisestats=sfng_get_basic_stats(c2noise,/nan)

   make_noise_cube, cube_in = c2 $
                    , cube_hdr = c2hdr $
                    , out_cube = c2noise2d $
                    , mask_in = c2sigmask $
                    , /iterate $
                    , /twod $
                    , box=use_avgbox $
                    , spec_box=use_avgspecbox

   c2noise2d=c2noise2d[*,*,use_midchan]
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
   c1sigidx=where(c1sigmask eq 1, c1ct, comp=c1nosigidx);,ncomp=noisect_c1)
   c2sigidx=where(c2sigmask eq 1, c2ct, comp=c2nosigidx);,ncomp=noisect_c2)
   jsigidx=where(joint_sigmask eq 1, jct)
   nosigidx=where(no_sigmask eq 1, nsct)

   noisect_c1=fovct-c1ct
   noisect_c2=fovct-c2ct
   
   ccmp_str.npix_tot=float(match_pixdim[0])*match_pixdim[1]*match_pixdim[2]
   ccmp_str.npix_cmp_fov=fovct
   ccmp_str.c1_npix_signalmask=c1ct
   ccmp_str.c2_npix_signalmask=c2ct
   ccmp_str.npix_jointsignalmask=jct
   ccmp_str.npix_nosignalmask=nsct

;==============================================
; STATISTICS RE TOTAL FLUX AND NUMBER OF PIXELS
;==============================================
   
  ccmp_str.c1_totflux=total(c1,/nan)
  ccmp_str.c2_totflux=total(c2,/nan)
  ccmp_str.c1_peak=max(c1,/nan)
  ccmp_str.c2_peak=max(c2,/nan)
  ccmp_str.totfluxdiff=total(diffcube,/nan)
  ccmp_str.diffabspeak=max(abs(diffcube),/nan)
  if c1ct gt 0 then ccmp_str.c1_totflux_signalmask=total(c1[c1sigidx],/nan)
  if c2ct gt 0 then ccmp_str.c2_totflux_signalmask=total(c2[c2sigidx],/nan)
  if nsct gt 0 then ccmp_str.c1_totflux_nosignalmask=total(c1[nosigidx],/nan)
  if nsct gt 0 then ccmp_str.c2_totflux_nosignalmask=total(c2[nosigidx],/nan)
  if jct gt 0 then  ccmp_str.c1_totflux_jointsignalmask=total(c1[jsigidx],/nan)
  if jct gt 0 then  ccmp_str.c2_totflux_jointsignalmask=total(c2[jsigidx],/nan)
  if jct gt 0 then  ccmp_str.totfluxdiff_jointsignalmask=total(diffcube[jsigidx],/nan)
  if jct gt 0 then  ccmp_str.c1_peak_jointsignalmask=max(c1[jsigidx],/nan)
  if jct gt 0 then  ccmp_str.c2_peak_jointsignalmask=max(c2[jsigidx],/nan)
  if jct gt 0 then  ccmp_str.diffabspeak_jointsignalmask=max(abs(diffcube[jsigidx]),/nan)
  if nsct gt 0 then ccmp_str.totfluxdiff_nosignalmask=total(diffcube[nosigidx],/nan)
  if nsct gt 0 then ccmp_str.diffabspeak_nosignalmask=max(abs(diffcube[nosigidx]),/nan)

  ccmp_str.npix_cmp_fov=fovct
  ccmp_str.c1_npix_signalmask=c1ct
  ccmp_str.c2_npix_signalmask=c2ct
  ccmp_str.npix_jointsignalmask=jct
  ccmp_str.npix_nosignalmask=nsct


;======================
; generate noise histograms
;======================

  nzmin=min(c1[c1nosigidx],/nan) < min(c2[c2nosigidx],/nan) < min(diffcube[nosigidx],/nan)
  nzmax=max(c1[c1nosigidx],/nan) > max(c2[c2nosigidx],/nan) > max(diffcube[nosigidx],/nan)
  nzmin_disp=percentile(c1[c1nosigidx],99) < percentile(c2[c2nosigidx],99) < percentile(diffcube[nosigidx],99)
  nzmax_disp=percentile(c1[c1nosigidx],1) > percentile(c2[c2nosigidx],1) > percentile(diffcube[nosigidx],1)
  nbins=round(noisect_c1/1000.) < round(noisect_c2/1000.) 
  xlim=abs(nzmin_disp)>abs(nzmax_disp)
  xr=[-xlim,xlim]
  !p.position=[0.2,0.2,0.8,0.8]

    
;======================
; CUBE 1
;======================

;  c1_noisestats_nosignal=sfng_get_basic_stats(c1[c1nosigidx],/nan)
;  hhcube=histogram(c1[c1nosigidx],max=nzmax,min=nzmin,locations=nzbins,nbins=nbins)
  c1_noisestats_nosignal=sfng_get_basic_stats(c1[nosigidx],/nan)
  hhcube=histogram(c1[nosigidx],max=nzmax,min=nzmin,locations=nzbins,nbins=nbins)

  use_pngfile='c1_noisehisto.png'

  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  yr=[0,1.2*(max(hhcube)/float(total(hhcube,/nan)))]
  fitlim=0.2*(max(hhcube)/float(total(hhcube,/nan)))
  tit='Pixels outside emission mask'
  
  cgplot,nzbins,(hhcube/float(total(hhcube,/nan))),/ysty,/xsty $
         ,/nodata,xr=xr,yr=yr,xtit='Tmb [K]',ytit='log(Npixels)',tit=tit $
         ,xthick=2,ythick=2,thick=2,charsize=1.8,charthick=1.7
  cgplot,[0,0],[-100,100],lines=1,/overplot,thick=2
  cgplot,nzbins,(hhcube/float(total(hhcube,/nan))),psym=10,/overplot,thick=2

  binsize=nzbins[1]-nzbins[0]
  binCenters = nzbins + (binsize/2.0)
  fitbins=where((hhcube/total(hhcube)) gt fitlim,fct)
  yfit = GaussFit(binCenters[fitbins],hhcube[fitbins]/float(total(hhcube,/nan)), coeffs, NTERMS=3)
  cgplot,binCenters[fitbins],yfit,color=cgcolor('red'),/overplot,thick=2

  al_legend, /top,/left, box=0,clear=0 $
                ,[use_c1str, $
                  'Mean: '+sigfig(c1_noisestats.mean,3,/sci), $
                  'RMS: '+ sigfig(c1_noisestats.rms,3,/sci)] $
                , lines=-99, charsize=1.8,charthick=1.7

  al_legend, /top,/right, box=0,clear=0 $
             ,['Gauss Fit', $
               'mu: '+sigfig(coeffs[1],3,/sci), $
               'width: '+sigfig(coeffs[2],3,/sci)] $
             , lines=-99, charsize=1.8,charthick=1.7

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; CUBE 2
;======================

;  c2_noisestats_nosignal=sfng_get_basic_stats(c2[c2nosigidx],/nan)
;  hhcube=histogram(c2[c2nosigidx],max=nzmax,min=nzmin,locations=nzbins,nbins=nbins)
  c2_noisestats_nosignal=sfng_get_basic_stats(c2[nosigidx],/nan)
  hhcube=histogram(c2[nosigidx],max=nzmax,min=nzmin,locations=nzbins,nbins=nbins)

    use_pngfile='c2_noisehisto.png'

  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  yr=[0,1.2*(max(hhcube)/float(total(hhcube,/nan)))]
  fitlim=0.2*(max(hhcube)/float(total(hhcube,/nan)))
  tit='Pixels outside emission mask'
  
  cgplot,nzbins,(hhcube/float(total(hhcube,/nan))),/ysty,/xsty $
         ,/nodata,xr=xr,yr=yr,xtit='Tmb [K]',ytit='log(Npixels)',tit=tit $
         ,xthick=2,ythick=2,thick=2,charsize=1.8,charthick=1.7
  cgplot,[0,0],[-100,100],lines=1,/overplot,thick=2
  cgplot,nzbins,(hhcube/float(total(hhcube,/nan))),psym=10,/overplot,thick=2

  binsize=nzbins[1]-nzbins[0]
  binCenters = nzbins + (binsize/2.0)
  fitbins=where((hhcube/total(hhcube)) gt fitlim,fct)
  yfit = GaussFit(binCenters[fitbins],hhcube[fitbins]/float(total(hhcube,/nan)), coeffs, NTERMS=3)
  cgplot,binCenters[fitbins],yfit,color=cgcolor('red'),/overplot,thick=2

  al_legend, /top,/left, box=0,clear=0 $
                ,[use_c2str, $
                  'Mean: '+sigfig(c2_noisestats.mean,3,/sci), $
                  'RMS: '+ sigfig(c2_noisestats.rms,3,/sci)] $
                , lines=-99, charsize=1.8,charthick=1.7

  al_legend, /top,/right, box=0,clear=0 $
             ,['Gauss Fit', $
               'mu: '+sigfig(coeffs[1],3,/sci), $
               'width: '+sigfig(coeffs[2],3,/sci)] $
             , lines=-99, charsize=1.8,charthick=1.7

    write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; DIFFCUBE
;======================
  diffcube_stats=sfng_get_basic_stats(diffcube,/nan)
  diffcube_stats_nosignal=sfng_get_basic_stats(diffcube[nosigidx],/nan)

  hhcube=histogram(diffcube[nosigidx],max=nzmax,min=nzmin,locations=nzbins,nbins=nbins)

  use_pngfile='diffcube_noisehisto.png'

  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  yr=[0,1.2*(max(hhcube)/float(total(hhcube,/nan)))]
  fitlim=0.2*(max(hhcube)/float(total(hhcube,/nan)))
  tit='Pixels outside emission mask'
  
  cgplot,nzbins,(hhcube/float(total(hhcube,/nan))),/ysty,/xsty $
         ,/nodata,xr=xr,yr=yr,xtit='Tmb [K]',ytit='log(Npixels)',tit=tit $
         ,xthick=2,ythick=2,thick=2,charsize=1.8,charthick=1.7
  cgplot,[0,0],[-100,100],lines=1,/overplot,thick=2
  cgplot,nzbins,(hhcube/float(total(hhcube,/nan))),psym=10,/overplot,thick=2

  binsize=nzbins[1]-nzbins[0]
  binCenters = nzbins + (binsize/2.0)
  fitbins=where((hhcube/total(hhcube)) gt fitlim,fct)
  yfit = GaussFit(binCenters[fitbins],hhcube[fitbins]/float(total(hhcube,/nan)), coeffs, NTERMS=3)
  cgplot,binCenters[fitbins],yfit,color=cgcolor('red'),/overplot,thick=2

  al_legend, /top,/left, box=0,clear=0 $
                ,['Difference Cube', $
                  'Mean: '+sigfig(c1_noisestats.mean,3,/sci), $
                  'RMS: '+ sigfig(c1_noisestats.rms,3,/sci)] $
                , lines=-99, charsize=1.8,charthick=1.7

  al_legend, /top,/right, box=0,clear=0 $
             ,['Gauss Fit', $
               'mu: '+sigfig(coeffs[1],3,/sci), $
               'width: '+sigfig(coeffs[2],3,/sci)] $
             , lines=-99, charsize=1.8,charthick=1.7

    write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; SAVE NOISE STATISTICS
;======================

  ccmp_str.c1_noisestats=c1_noisestats
  ccmp_str.c2_noisestats=c2_noisestats
  ccmp_str.c1_noisestats_nosignal=c1_noisestats_nosignal
  ccmp_str.c2_noisestats_nosignal=c2_noisestats_nosignal
  ccmp_str.diffcube_stats=diffcube_stats
  ccmp_str.diffcube_stats_nosignal=diffcube_stats_nosignal


;======================
; OVERALL FIDELITY CUBE
;======================

  use_fidel_levs=[5,10,25,50] ; % of image peak
  fidel_medians_c1=fltarr(4)
  fidel_medians_c2=fltarr(4)
  fidel_rmsfactor=0.7*diffcube_stats.rms
  fidelcube_c1=abs(c1)/(abs(diffcube) > fidel_rmsfactor)
  fidelcube_c2=abs(c2)/(abs(diffcube) > fidel_rmsfactor)

  use_fidelidx_c1=where(finite(fidelcube_c1) and joint_sigmask eq 1,fct1)
  if fct1 gt 0 then fidel_vec_c1 = fidelcube_c1[use_fidelidx_c1]
  fidel_vec_c1=fidel_vec_c1[sort(fidel_vec_c1)]
  nfidelpix_c1=lindgen(fct1)+1
  fidel_levs_c1=(use_fidel_levs/100.)*max(c1[jsigidx],/nan)

  for i=0,3 do begin
     fidel_median_c1_idx=where(finite(fidelcube_c1) and finite(c1) and c1 gt fidel_levs_c1[i] and joint_sigmask eq 1,mct)
     if mct gt 0 then fidel_medians_c1[i]=median(fidelcube_c1[fidel_median_c1_idx])
  end

  
  use_fidelidx_c2=where(finite(fidelcube_c2) and joint_sigmask eq 1,fct2)
  if fct2 gt 0 then fidel_vec_c2 = fidelcube_c2[use_fidelidx_c2]
  fidel_vec_c2=fidel_vec_c2[sort(fidel_vec_c2)]
  nfidelpix_c2=lindgen(fct2)+1
  fidel_levs_c2=(use_fidel_levs/100.)*max(c2[jsigidx],/nan)
  
  for i=0,3 do begin
     fidel_median_c2_idx=where(finite(fidelcube_c2) and finite(c2) and c2 gt fidel_levs_c2[i] and joint_sigmask eq 1,mct)
     if mct gt 0 then fidel_medians_c2[i]=median(fidelcube_c2[fidel_median_c2_idx])
  end

  
  use_pngfile='fidelity_cube1_cdf.png'
  yr=[1,fct1>fct2] & xr=[(min(fidel_vec_c1)<min(fidel_vec_c2)),(max(fidel_vec_c1)>max(fidel_vec_c2))]
  !p.position=[0.2,0.2,0.8,0.8]
  
  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  tit='Fidelity CDF inside common mask'
  cgplot,fidel_vec_c1,reverse(nfidelpix_c1),/ysty,/xsty,/ylo $
         ,/nodata,xr=xr,yr=yr,xtit='Fidelity Value',ytit='Npix(F>F*)',tit=tit $
         ,xthick=2,ythick=2,thick=2,charsize=1.8,charthick=1.7
  cgplot,fidel_vec_c1,reverse(nfidelpix_c1),psym=10,/overplot,thick=2
  cgplot,[fidel_medians_c1[0],fidel_medians_c1[0]],yr,lines=0,thick=2,color=cgcolor('green'),/overplot
  cgplot,[fidel_medians_c1[1],fidel_medians_c1[1]],yr,lines=0,thick=2,color=cgcolor('red'),/overplot
  cgplot,[fidel_medians_c1[2],fidel_medians_c1[2]],yr,lines=0,thick=2,color=cgcolor('blue'),/overplot
  cgplot,[fidel_medians_c1[3],fidel_medians_c1[3]],yr,lines=0,thick=2,color=cgcolor('black'),/overplot

  al_legend,/top,/right,clear=0, box=0 $
            , [use_c1str+', median fidelity:','pix > '+strtrim(string(use_fidel_levs),2)+'% of pk: '+sigfig(fidel_medians_c1,3)] $
            ,lines=[-99,0,0,0,0] $
            ,colors=[cgcolor('black'),cgcolor('green'),cgcolor('red'),cgcolor('blue'),cgcolor('black')] $
            ,charsize=1.,thick=2,charthick=1.8

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

  use_pngfile='fidelity_cube2_cdf.png'

  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  tit='Fidelity CDF inside common mask'
  cgplot,fidel_vec_c2,reverse(nfidelpix_c2),/ysty,/xsty,/ylo $
         ,/nodata,xr=xr,yr=yr,xtit='Fidelity Value',ytit='Npix(F>F*)',tit=tit $
         ,xthick=2,ythick=2,thick=2,charsize=1.8,charthick=1.7
  cgplot,fidel_vec_c2,reverse(nfidelpix_c2),psym=10,/overplot,thick=2
  cgplot,[fidel_medians_c2[0],fidel_medians_c2[0]],yr,lines=0,thick=2,color=cgcolor('green'),/overplot
  cgplot,[fidel_medians_c2[1],fidel_medians_c2[1]],yr,lines=0,thick=2,color=cgcolor('red'),/overplot
  cgplot,[fidel_medians_c2[2],fidel_medians_c2[2]],yr,lines=0,thick=2,color=cgcolor('blue'),/overplot
  cgplot,[fidel_medians_c2[3],fidel_medians_c2[3]],yr,lines=0,thick=2,color=cgcolor('black'),/overplot

  al_legend,/top,/right,clear=0, box=0 $
            , [use_c2str+', median fidelity:','pix > '+strtrim(string(use_fidel_levs),2)+'% of pk: '+sigfig(fidel_medians_c2,3)] $
            ,lines=[-99,0,0,0,0] $
            ,colors=[cgcolor('black'),cgcolor('green'),cgcolor('red'),cgcolor('blue'),cgcolor('black')] $
            ,charsize=1.,thick=2,charthick=1.8

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)


;======================
; SAVE OVERALL FIDELITY STATISTICS
;======================

  ccmp_str.c1_fidel_stats=fidel_medians_c1
  ccmp_str.c2_fidel_stats=fidel_medians_c2
  ccmp_str.fidel_levs=use_fidel_levs
  ccmp_str.c1_fidel_levs=use_fidel_levs*ccmp_str.c1_peak_jointsignalmask
  ccmp_str.c2_fidel_levs=use_fidel_levs*ccmp_str.c2_peak_jointsignalmask
  
;======================
; STATISTICS RE FLUX, FIDELITY, RMS PER CHANNEL
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
  c1_chanfidel_jsm=fltarr(nchans)
  c2_chanfidel_jsm=fltarr(nchans)
  
  for i=0,nchans-1 do begin

     c1_thisplane=c1[*,*,i]
     c1_noise_thisplane=c1noise[*,*,i]
     c2_thisplane=c2[*,*,i]
     c2_noise_thisplane=c2noise[*,*,i]
     c1_fidel_thisplane=fidelcube_c1[*,*,i]
     c2_fidel_thisplane=fidelcube_c2[*,*,i]
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
     if c1ct gt 0 then c1_chanfidel_jsm[i]=median(c1_fidel_thisplane[c1_thisplane_jsm_idx])
     if c2ct gt 0 then c2_chanfidel_jsm[i]=median(c2_fidel_thisplane[c2_thisplane_jsm_idx])

     diff_chanflux[i]=c1_chanflux[i]-c2_chanflux[i]
     if c1ct gt 0 and c2ct gt 0 then diff_chanflux_jsm[i]=c1_chanflux_jsm[i]-c2_chanflux_jsm[i]
     if nc1ct gt 0 and nc2ct gt 0 then diff_chanflux_nosm[i]=c1_chanflux_nosm[i]-c2_chanflux_nosm[i]

     if nc1ct gt 0 then c1_chanrms_nosm[i]=robust_sigma(c1_thisplane[c1_thisplane_nosm_idx])
     if nc2ct gt 0 then c2_chanrms_nosm[i]=robust_sigma(c2_thisplane[c2_thisplane_nosm_idx])
     if fc1ct gt 0 then c1_chanrms[i]=robust_sigma(c1_thisplane[c1_thisplane_fov_idx])
     if fc2ct gt 0 then c2_chanrms[i]=robust_sigma(c2_thisplane[c2_thisplane_fov_idx])

  endfor

  emission_startchan=0
  emission_endchan=nchans-1
  emission_idx=where(c1_chanflux_jsm gt 0, emct)
  if emct gt 0 then begin
     emission_startchan=emission_idx[0]
     emission_endchan=emission_idx[emct-1] ; can't use -1 index for idl v7
  end

  
  
  ccmp_str.c1_fluxperchan=ptr_new(c1_chanflux)
  ccmp_str.c2_fluxperchan=ptr_new(c2_chanflux)
  ccmp_str.c1_fluxperchan_jointsignalmask=ptr_new(c1_chanflux_jsm)
  ccmp_str.c2_fluxperchan_jointsignalmask=ptr_new(c2_chanflux_jsm)
  ccmp_str.c1_fidelperchan_jointsignalmask=ptr_new(c1_chanfidel_jsm)
  ccmp_str.c2_fidelperchan_jointsignalmask=ptr_new(c2_chanfidel_jsm)
  ccmp_str.c1_rmsperchan=ptr_new(c1_chanrms)
  ccmp_str.c2_rmsperchan=ptr_new(c2_chanrms)
  ccmp_str.c1_rmsperchan_nosignalmask=ptr_new(c1_chanrms_nosm)
  ccmp_str.c2_rmsperchan_nosignalmask=ptr_new(c2_chanrms_nosm)
  ccmp_str.fluxdiffperchan=ptr_new(diff_chanflux)
  ccmp_str.fluxdiffperchan_jointsignalmask=ptr_new(diff_chanflux_jsm)
  ccmp_str.fluxdiffperchan_nosignalmask=ptr_new(diff_chanflux_nosm)
  ccmp_str.emission_startchan=emission_startchan
  ccmp_str.emission_endchan=emission_endchan

;======================
; generate 'spectra' -- flux/rms per chan figures
;======================

  chans=indgen(nchans)+1
  make_axes,c1hdr,vaxis=vchans,/vonly
  
  
;======================
; a. Flux per channel
;======================

  use_pngfile='flux_per_channel.png'
  
  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  !p.position=[0.2,0.2,0.9,0.8]
  loadct,39 & fgcolor=255
  xr=[0,nchans] & xr1=[vchans[0],vchans[nchans-1]]
  ymax=max(c1_chanflux,/nan) > max(c2_chanflux,/nan)
  yr=[-0.1*ymax,1.1*ymax]
  tit='Input Cubes, Common FoV'
  
  cgplot,chans,c1_chanflux,xtit='Channel number',ytit='Sum(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanflux,color=cgcolor('red'),psym=10,thick=2,/overplot
  cgplot,chans,c2_chanflux,color=cgcolor('blue'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1
  
  al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_c1str, $
               use_c2str] $
            ,lines=[-99,0,0] $
            ,colors=[fgcolor,cgcolor('red'),cgcolor('blue')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

  
;======================
; b. luminosity per channel -- only done if we know galaxy distance
;======================

  if keyword_set(use_galaxy) then begin
     use_pngfile='luminosity_per_channel.png'
     
     c1_chanlum=c1_chanflux*pixscale_as*pixscale_as*gstr.dist_mpc*gstr.dist_mpc*Mpc_on_AU*Mpc_on_AU
     c2_chanlum=c2_chanflux*pixscale_as*pixscale_as*gstr.dist_mpc*gstr.dist_mpc*Mpc_on_AU*Mpc_on_AU

     window,use_win,xsize=900,ysize=400 & use_win=use_win+1
     !p.position=[0.2,0.2,0.9,0.8]
     loadct,39 & fgcolor=255
     xr=[0,nchans] & xr1=[vchans[0],vchans[nchans-1]]
     ymax=max(c1_chanlum,/nan) > max(c2_chanlum,/nan)
     yr=[-0.1*ymax,1.1*ymax]
     tit='Input Cubes, Common FoV'
     
     cgplot,chans,c1_chanlum,xtit='Chans',ytit='Luminosity [K pc2]',charsize=1.5,color=fgcolor,charthick=1.8 $
            ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
     cgplot,chans,chans*0,lines=1,/overplot
     cgplot,chans,c1_chanlum,color=cgcolor('red'),psym=10,thick=2,/overplot
     cgplot,chans,c2_chanlum,color=cgcolor('blue'),psym=10,thick=2,/overplot
     cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1
     
     al_legend,/top,/left,clear=0, box=0 $
               , [tit, $
               use_c1str, $
                  use_c2str] $
               ,lines=[-99,0,0] $
               ,colors=[fgcolor,cgcolor('red'),cgcolor('blue')] $
               ,charsize=1.,thick=2,charthick=1.8
     
     
     write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
     
  end
  
;======================
; c. Flux within joint signal mask per channel
;======================

  use_pngfile='flux_per_channel_jointsignalmask.png'
  
  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  !p.position=[0.2,0.2,0.9,0.8]
  xr=[0,nchans] & xr1=[vchans[0],vchans[nchans-1]]
  ymax=max(c1_chanflux,/nan) > max(c2_chanflux,/nan)
  yr=[-0.1*ymax,1.1*ymax]
  tit='Input Cubes, Common FoV, JointSigMask'
  
  cgplot,chans,c1_chanflux_jsm,xtit='Channel number',ytit='Sum(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanflux_jsm,color=cgcolor('red'),psym=10,thick=2,/overplot
  cgplot,chans,c2_chanflux_jsm,color=cgcolor('blue'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1
  
  al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_c1str, $
               use_c2str] $
            ,lines=[-99,0,0] $
            ,colors=[fgcolor,cgcolor('red'),cgcolor('blue')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; d. Flux difference per channel
;======================

  use_pngfile='diffcube_flux_per_channel.png'

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
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1
  
  al_legend,/bottom,/left,clear=0, box=0 $
            , [tit, $
               'C1-C2'] $
            ,lines=[-99,0] $
            ,charsize=1.,thick=2,charthick=1.8

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

  ;======================
; e. Relative Flux difference per channel
;======================

  use_pngfile='diffcube_relflux_per_channel.png'

  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  ymax=max(diff_chanflux/c1_chanflux,/nan)
  ymin=min(diff_chanflux/c1_chanflux,/nan)
  
  if ymin lt 0 then yr=[1.1*ymin,1.1*ymax]
  if ymin ge 0 then yr=[0.8*ymin,1.1*ymax]
  tit='Relative Difference Cube, Common FoV'
  ;yr=[-0.5,0.5]
  
  cgplot,chans,diff_chanflux/float(c1_chanflux),xtit='Channel number',ytit='[Cube1 - Cube2]/Cube 1',charsize=1.5,color=fgcolor,charthick=1.8,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,chans*0+0.25,lines=1,/overplot
  cgplot,chans,chans*0-0.25,lines=1,/overplot
  cgplot,chans,diff_chanflux/float(c1_chanflux),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1
  
  al_legend,/bottom,/left,clear=0, box=0 $
            , [tit, $
               '(C1-C2)/C1'] $
            ,lines=[-99,0] $
            ,charsize=1.,thick=2,charthick=1.8

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;  stop

;======================
; f. Flux difference per channel inside the joint signal mask
;======================

  use_pngfile='diffcube_flux_per_channel_jointsignalmask.png'

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
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1
  
  al_legend,/bottom,/left,clear=0, box=0 $
            , [tit, $
               'C1-C2'] $
            ,lines=[-99,0] $
            ,charsize=1.,thick=2,charthick=1.8

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
  
;======================
; a. RMS per channel
;======================

  use_pngfile='rms_per_channel.png'

  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  ymax=max(c1_chanrms,/nan) > max(c2_chanrms,/nan)
  ymin=min(c1_chanrms,/nan) < min(c2_chanrms,/nan)
  
  if ymin lt 0 then yr=[1.1*ymin,1.1*ymax]
  if ymin ge 0 then yr=[0.8*ymin,1.1*ymax]
  tit='Input Cubes, Common FoV'
  
  cgplot,chans,c1_chanrms,xtit='Channel number',ytit='RMS(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanrms,color=cgcolor('red'),psym=10,thick=2,/overplot
  cgplot,chans,c2_chanrms,color=cgcolor('blue'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1

  
  al_legend,/bottom,/left,clear=0, box=0 $
            , [tit, $
               use_c1str, $
               use_c2str] $
            ,lines=[-99,0,0] $
            ,colors=[fgcolor,cgcolor('red'),cgcolor('blue')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)


  
;======================
; b. RMS per channel in nosigmask
;======================

  use_pngfile='rms_per_channel_nosignalmask.png'

  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  ymax=max(c1_chanrms_nosm,/nan) > max(c2_chanrms_nosm,/nan)
  ymin=min(c1_chanrms_nosm,/nan) < min(c2_chanrms_nosm,/nan)
  
  if ymin lt 0 then yr=[1.1*ymin,1.1*ymax]
  if ymin ge 0 then yr=[0.8*ymin,1.1*ymax]
  tit='Input Cubes, Common FoV, No Signal Mask'
  
  cgplot,chans,c1_chanrms_nosm,xtit='Channel number',ytit='RMS(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanrms_nosm,color=cgcolor('red'),psym=10,thick=2,/overplot
  cgplot,chans,c2_chanrms_nosm,color=cgcolor('blue'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1

  
  al_legend,/bottom,/left,clear=0, box=0 $
            , [tit, $
               use_c1str, $
               use_c2str] $
            ,lines=[-99,0,0] $
            ,colors=[fgcolor,cgcolor('red'),cgcolor('blue')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; generate fidelity 'spectra' -- median fidelity per chan figures
;======================

   use_pngfile='fidelity_perchannel_jsm.png'
  
  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  !p.position=[0.2,0.2,0.9,0.8]
  loadct,39 & fgcolor=255
  xr=[0,nchans] & xr1=[vchans[0],vchans[nchans-1]]

  ymax=max(c1_chanfidel_jsm,/nan) > max(c2_chanfidel_jsm,/nan)
  yr=[-0.1*ymax,1.1*ymax]
  tit='Input Cubes, Common FoV, JointSigMask'
  
  cgplot,chans,c1_chanfidel_jsm,xtit='Channel number',ytit='Median Fidelity Value in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanfidel_jsm,color=cgcolor('red'),psym=10,thick=2,/overplot
  cgplot,chans,c2_chanfidel_jsm,color=cgcolor('blue'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1
  
  al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_c1str, $
               use_c2str] $
            ,lines=[-99,0,0] $
            ,colors=[fgcolor,cgcolor('red'),cgcolor('blue')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; CHANNEL MAPS
;======================

; default is to show only emission
; channels with 20km/s margin on each side
  start_chan=emission_startchan-fix(20./chanw_kms)
  end_chan=emission_endchan+fix(20./chanw_kms)
  start_chan=0>start_chan & end_chan=end_chan<(nchans-1)

; if use_allchannels is set -- show all channels in cube
  if use_allchannels eq 1 then begin
     start_chan=0 & end_chan=nchans-1
  end
  
  loadct,20,rgb_table=palette
  TVLCT, cgColor('gray', /Triple), 0

  c1def=c1
  c2def=c2
  c1indefidx=where(finite(c1) eq 0,comp=c1defidx,ict,ncomp=dct)
  if ict gt 0 then c1def=c1[c1defidx]
  c2indefidx=where(finite(c2) eq 0,comp=c2defidx,ict,ncomp=dct)
  if ict gt 0 then c2def=c2[c2defidx]
;  immax=percentile(c1def,1)>percentile(c2def,1) & immin=percentile(c1def,95)<percentile(c2def,95)
  immax=percentile(c1def,0.5)>percentile(c2def,0.5) & immin=percentile(c1def,85)<percentile(c2def,85)

  print,'Image minimum is:',immin
  print,'Image maximum is:',immax

  cube1_chans:
; cube 1
  use_win=use_win+1
  for k=start_chan,end_chan do begin
     window,use_win,xsize=400,ysize=400 
     cgImage, c1[*,*,k],  /Axes, Palette=palette, charsize=0.8, minval=immin, maxval=immax $
              ,tit='channel '+strtrim(STRING(fix(k+1)),2) $
              ,Position=[0.05,0.05,0.9,0.9] 
     use_pngfile='c1_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
     write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

  endfor

;  stop
;  if good_color_range eq 0 then goto, cube1_chans
  
; cube 2
  use_win=use_win+1
  for k=start_chan,end_chan do begin
     window,use_win,xsize=400,ysize=400 
     cgImage, c2[*,*,k],  /Axes, Palette=palette, charsize=0.8, minval=immin, maxval=immax $
              , tit='channel '+strtrim(STRING(fix(k+1)),2) $
              ,Position=[0.05,0.05,0.9,0.9]
     use_pngfile='c2_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
     write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
 endfor

  ;stop
  ;if good_color_range eq 0 then goto, cube1_chans

diff_chans:
; diffcube
  use_win=use_win+1
  immin=percentile(diffcube,85) & immax=percentile(diffcube,5)
  for k=start_chan,end_chan do begin
     window,use_win,xsize=400,ysize=400 
     cgImage, diffcube[*,*,k],  /Axes, Palette=palette, charsize=0.8, minval=immin, maxval=immax $
              , Position=[0.05,0.05,0.9,0.9] $
              ,tit='channel '+strtrim(STRING(fix(k+1)),2)
     use_pngfile='diffcube_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
     write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
  endfor

  ;stop
  ;if good_color_range eq 0 then goto, diff_chans

; joint emission mask

  use_win=use_win+1
  loadct,0
  reversect
  immin=0. & immax=1.
  for k=start_chan,end_chan do begin
     window,use_win,xsize=400,ysize=400 
     cgImage, joint_sigmask[*,*,k],  /Axes,  charsize=0.8, minval=immin, maxval=immax $
              , Position=[0.05,0.05,0.9,0.9] $
              , tit='channel '+strtrim(STRING(fix(k+1)),2)
     use_pngfile='jointsignalmask_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
     write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
  endfor
  
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

  
;======================
; Linear Cube 1 vs Cube 2
;======================
  
  use_pngfile='lincorr_c1c2.png'

  plotmax=1.25*(max(c1[jsigidx],/nan)>max(c2[jsigidx],/nan))
  xaxis=plotmax*findgen(100)/100.
  yfit=xaxis*res_linc1c2[0]+res_linc1c2[1]
  
  !p.position=[0.2,0.2,0.8,0.8]
  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  xr=[-0.05,plotmax] & yr=[-0.05,plotmax]

  cgplot,c1[jsigidx],c2[jsigidx],xtit=use_c1str+' [K]',ytit=use_c2str+' [K]',tit='Pixel-pixel correlation inside common mask', $
       charsize=1.5,/nodata,/xsty,/ysty,xr=xr,yr=yr,xthick=2,ythick=2,thick=2,charthick=1.8
  cgplot,c1[jsigidx],c2[jsigidx],psym=3,color=cgcolor('dark grey'),/overplot,thick=2
  cgplot,xaxis,yfit,color=cgcolor('blue'),lines=2,/overplot,thick=3
  equality,color=cgcolor('black'),thick=2

  plotsym,0,1.2,/fill
  
  xmin = 0. & xmax = plotmax & binsize = round(plotmax*10.)/100.
  bin_prof, [c1[jsigidx]] $
            ,[c2[jsigidx]] $
            , xmin=xmin, xmax=xmax, binsize=binsize $
            , xmid=xmid, medprof=medprof, madprof=madprof
  oploterror, xmid, medprof, madprof $
              , errcolor=cgcolor('red'), color=cgcolor('red') $
              , errthick=2, psym=8, symsize=1.5
  oplot, xmid, medprof $
         , psym=8, symsize=1.5, color=cgcolor('red')

  
  al_legend,/top,/left,clear=0, box=0 $
            , ['Equality','Slope: '+sigfig(res_linc1c2[0],3)] $
            ,colors=[cgcolor('black'),cgcolor('blue')] $
            ,charsize=1.5,thick=[2,3],linsize=0.5,lines=[0,2],charthick=1.5

  al_legend,/bottom,/right,clear=0, box=0 $
            , ['Running median, y on x'] $
            ,colors=[cgcolor('red')] $
            ,charsize=1.5,psym=8,charthick=1.5

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)


;================
; generate density plot -- linear axes, c1 (x) vs c2 (y)
;================

  use_pngfile='lindens_c1c2.png'

  binsize2d=binsize/use_binsize_2dfactor ; sometimes this needs to be tweaked depending on input map Npix& range
  density = Hist_2D(c1[jsigidx],c2[jsigidx], Min1=xr[0], Max1=xr[1], Bin1=binsize2d, $
                           Min2=yr[0], Max2=yr[1], Bin2=binsize2d)   
                           
   maxDensity = Ceil(Max(density)/1e2) * 1e2
   scaledDensity = BytScl(density, Min=0, Max=maxDensity)
                           
   window,use_win,xsize=600,ysize=600 & use_win=use_win+1
   loadct,20,rgb_table=palette
   TVLCT, cgColor('gray', /Triple), 0
   use_tit='Pixel values inside common mask'
   
   cgImage, scaledDensity, XRange=xr, YRange=yr, /Axes, Palette=palette, $
      XTitle=use_c1str+' [K]', YTitle=use_c2str+' [K]', Title=use_tit, $
      Position=[0.2,0.2,0.8,0.8]

     equality,color=cgcolor('black'),thick=2

   cgColorbar, position=[0.825,0.2,0.85,0.8]  $
               , Title='Npixels', /vertical, /right $
               ,Range=[0, maxDensity], NColors=254, Bottom=1,Palette=palette $
               ,charsize=1.1,tcharsize=1.1

   write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

    
;======================
; Linear Cube 2 vs Cube 1
;======================

  use_pngfile='lincorr_c2c1.png'

  plotmax=1.25*(max(c1[jsigidx],/nan)>max(c2[jsigidx],/nan))
  xaxis=plotmax*findgen(100)/100.
  yfit=xaxis*res_linc2c1[0]+res_linc2c1[1]
  
  !p.position=[0.2,0.2,0.8,0.8]
  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  xr=[-0.05,plotmax] & yr=[-0.05,plotmax]

  cgplot,c2[jsigidx],c1[jsigidx],xtit=use_c2str+' [K]',ytit=use_c1str+' [K]',tit='Pixel-pixel correlation inside common mask', $
       charsize=1.5,/nodata,/xsty,/ysty,xr=xr,yr=yr,xthick=2,ythick=2,thick=2,charthick=1.8
  cgplot,c2[jsigidx],c1[jsigidx],psym=3,color=cgcolor('dark grey'),/overplot,thick=2
  cgplot,xaxis,yfit,color=cgcolor('blue'),lines=2,/overplot,thick=3
  equality,color=cgcolor('black'),thick=2

  plotsym,0,1.2,/fill
  
  xmin = 0. & xmax = plotmax & binsize = round(plotmax*10.)/100.
  bin_prof, [c2[jsigidx]] $
            ,[c1[jsigidx]] $
            , xmin=xmin, xmax=xmax, binsize=binsize $
            , xmid=xmid, medprof=medprof, madprof=madprof
  oploterror, xmid, medprof, madprof $
              , errcolor=cgcolor('red'), color=cgcolor('red') $
              , errthick=2, psym=8, symsize=1.5
  oplot, xmid, medprof $
         , psym=8, symsize=1.5, color=cgcolor('red')

  
  al_legend,/top,/left,clear=0, box=0 $
            , ['Equality','Slope: '+sigfig(res_linc2c1[0],3)] $
            ,colors=[cgcolor('black'),cgcolor('blue')] $
            ,charsize=1.5,thick=[2,3],linsize=0.5,lines=[0,2],charthick=1.5

  al_legend,/bottom,/right,clear=0, box=0 $
            , ['Running median, y on x'] $
            ,colors=[cgcolor('red')] $
            ,charsize=1.5,psym=8,charthick=1.5

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;================
; generate density plot - linear axes, c2 (x) vs c1 (y)
;================

  use_pngfile=+'lindens_c2c1.png'
    
  binsize2d=binsize/use_binsize_2dfactor
  density = Hist_2D(c2[jsigidx],c1[jsigidx], Min1=xr[0], Max1=xr[1], Bin1=binsize2d, $
                           Min2=yr[0], Max2=yr[1], Bin2=binsize2d)   
                           
  maxDensity = Ceil(Max(density)/1e2) * 1e2
  scaledDensity = BytScl(density, Min=0, Max=maxDensity)
                           
  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  loadct,20,rgb_table=palette
  TVLCT, cgColor('gray', /Triple), 0
  use_tit='Pixel values inside common mask'
     
  cgImage, scaledDensity, XRange=xr, YRange=yr, /Axes, Palette=palette, $
           XTitle=use_c1str+' [K]', YTitle=use_c2str+' [K]', Title=use_tit,$
           Position=[0.2,0.2,0.8,0.8]

    equality,color=cgcolor('black'),thick=2

  cgColorbar, position=[0.825,0.2,0.85,0.8]  $
              , Title='Npixels', /vertical, /right $
              ,Range=[0, maxDensity], NColors=254, Bottom=1,Palette=palette $
              ,charsize=1.1,tcharsize=1.1

    write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; Log Cube 1 vs Cube 2
;======================
    
  use_pngfile='logcorr_c1c2.png'

  plotmax=1.25*(max(c1[jsigidx],/nan)>max(c2[jsigidx],/nan))
  plotmin=0.75*(min(c1[jsigidx],/nan)<min(c2[jsigidx],/nan))
  
   !p.position=[0.2,0.2,0.8,0.8]
 window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  xr=[plotmin,plotmax] & yr=[plotmin,plotmax]

  cgplot,c1[jsigidx],c2[jsigidx],xtit=use_c1str+' [K]',ytit=use_c2str+' [K]',tit='Pixel-pixel correlation inside common mask', $
       charsize=1.5,/nodata,/xsty,/ysty,xr=xr,yr=yr,xthick=2,ythick=2,thick=2,charthick=1.8,/xlo,/ylo
  cgplot,c1[jsigidx],c2[jsigidx],psym=3,color=cgcolor('dark grey'),/overplot,thick=2
  equality,color=cgcolor('black'),thick=2

  plotsym,0,1.2,/fill
  
  xmin = plotmin & xmax = plotmax
  binsize = round(plotmax*10.)/100.
  
  bin_prof, [c1[jsigidx]] $
            ,[c2[jsigidx]] $
            , xmin=xmin, xmax=xmax, binsize=binsize $
            , xmid=xmid, medprof=medprof, madprof=madprof ;, madlogprof=madlogprof
;  oploterror, xmid, medprof, madprof $
;              , errcolor=cgcolor('red'), color=cgcolor('red') $
;              , errthick=2, psym=8, symsize=1.5
  oplot, xmid, medprof $
         , psym=8, symsize=1.5, color=cgcolor('red')

  
;  al_legend,/top,/left,clear=0, box=0 $
;            , ['Equality','Slope: '+sigfig(res_linc1c2[0],3)] $
;            ,colors=[cgcolor('black'),cgcolor('blue')] $
;            ,charsize=1.5,thick=[2,3],linsize=0.5,lines=[0,2],charthick=1.5

;  al_legend,/bottom,/right,clear=0, box=0 $
;            , ['Running median, y on x'] $
;            ,colors=[cgcolor('red')] $
;            ,charsize=1.5,psym=8,charthick=1.5

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)


  logdens_plot:
;================
; generate density plot - logarithmic axes, c1 (x) vs c2 (y)
;================

  use_pngfile='logdens_c1c2.png'
  
  binsize2d=binsize/use_binsize_2dfactor ; sometimes this needs to be tweaked depending on input map Npix& range
  density = Hist_2D(alog10(c1[jsigidx]),alog10(c2[jsigidx]) $
                    , Min1=alog10(xr[0]), Max1=alog10(xr[1]), Bin1=binsize2d $
                           , Min2=alog10(yr[0]), Max2=alog10(yr[1]), Bin2=binsize2d)   
                           
  maxDensity = Ceil(Max(density)/1e2) * 1e2
  scaledDensity = BytScl(density, Min=0, Max=maxDensity)
                           
  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  loadct,20,rgb_table=palette
  TVLCT, cgColor('gray', /Triple), 0
     use_tit='Pixel values inside common mask'

  cgImage, scaledDensity, XRange=alog10(xr), YRange=alog10(yr), /Axes, Palette=palette, $
           XTitle=use_c1str+' [log K]', YTitle=use_c2str+' [log K]', Title=use_tit,$
           Position=[0.2,0.2,0.8,0.8]

  equality,color=cgcolor('black'),thick=2
    
  cgColorbar, position=[0.825,0.2,0.85,0.8]  $
              , Title='Npixels', /vertical, /right $
              ,Range=[0, maxDensity], NColors=254, Bottom=1,Palette=palette $
              ,charsize=1.1,tcharsize=1.1

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;  stop
;  if good_bin_range eq 0 then goto,   logdens_plot

;======================
; Log Cube 2 vs Cube 1
;======================
  
  use_pngfile='logcorr_c2c1.png'

  plotmax=1.25*(max(c1[jsigidx],/nan)>max(c2[jsigidx],/nan))
  plotmin=0.75*(min(c1[jsigidx],/nan)<min(c2[jsigidx],/nan))
  
  !p.position=[0.2,0.2,0.8,0.8]
  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  xr=[plotmin,plotmax] & yr=[plotmin,plotmax]

  cgplot,c2[jsigidx],c1[jsigidx],xtit=use_c2str+' [K]',ytit=use_c1str+' [K]',tit='Pixel-pixel correlation inside common mask', $
       charsize=1.5,/nodata,/xsty,/ysty,xr=xr,yr=yr,xthick=2,ythick=2,thick=2,charthick=1.8,/xlo,/ylo
  cgplot,c2[jsigidx],c1[jsigidx],psym=3,color=cgcolor('dark grey'),/overplot,thick=2
  equality,color=cgcolor('black'),thick=2

  plotsym,0,1.2,/fill
  
  xmin = plotmin & xmax = plotmax
  binsize = round(plotmax*10.)/100.
  bin_prof, [c2[jsigidx]] $
            ,[c1[jsigidx]] $
            , xmin=xmin, xmax=xmax, binsize=binsize $
            , xmid=xmid, medprof=medprof, madprof=madprof ;, madlogprof=madlogprof
;  oploterror, xmid, medprof, madprof $
;              , errcolor=cgcolor('red'), color=cgcolor('red') $
;              , errthick=2, psym=8, symsize=1.5
  oplot, xmid, medprof $
         , psym=8, symsize=1.5, color=cgcolor('red')

  
;  al_legend,/top,/left,clear=0, box=0 $
;            , ['Equality','Slope: '+sigfig(res_linc1c2[0],3)] $
;            ,colors=[cgcolor('black'),cgcolor('blue')] $
;            ,charsize=1.5,thick=[2,3],linsize=0.5,lines=[0,2],charthick=1.5

;  al_legend,/bottom,/right,clear=0, box=0 $
;            , ['Running median, y on x'] $
;            ,colors=[cgcolor('red')] $
;            ,charsize=1.5,psym=8,charthick=1.5

    write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

    
;================
; generate density plot - logarithmic axes, c2 (x) vs c1 (y)
;================

  use_pngfile='logdens_c2c1.png'
  
  binsize2d=binsize/use_binsize_2dfactor
  density = Hist_2D(alog10(c2[jsigidx]),alog10(c1[jsigidx]) $
                    , Min1=alog10(xr[0]), Max1=alog10(xr[1]), Bin1=binsize2d $
                           , Min2=alog10(yr[0]), Max2=alog10(yr[1]), Bin2=binsize2d)   
                           
  maxDensity = Ceil(Max(density)/1e2) * 1e2
  scaledDensity = BytScl(density, Min=0, Max=maxDensity)
                           
  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  loadct,20,rgb_table=palette
  TVLCT, cgColor('gray', /Triple), 0
  use_tit='Pixel values inside common mask'

  cgImage, scaledDensity, XRange=alog10(xr), YRange=alog10(yr), /Axes, Palette=palette, $
           XTitle=use_c2str+' [log K]', YTitle=use_c1str+' [log K]', Title=use_tit,$
           Position=[0.2,0.2,0.8,0.8]

  equality,color=cgcolor('black'),thick=2

  cgColorbar, position=[0.825,0.2,0.85,0.8]  $
              , Title='Npixels', /vertical, /right $
              ,Range=[0, maxDensity], NColors=254, Bottom=1,Palette=palette $
              ,charsize=1.1,tcharsize=1.1

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;  stop
;  if good_bin_range eq 0 then goto,   logdens_plot


  
save_structure:
;================
;save the results structure
;================

   save,file=use_savedir+savefile,ccmp_str

   produce_report:
;================
;
;================

   if do_report eq 1 then begin
      sfng_make_latex_elements,ccmp_str,reportdir=use_reportdir,plotdir=use_plotdir
      sfng_compile_latex,reportdir=use_reportdir,plotdir=use_plotdir
   end

   if keyword_set(verbose) then message,'Finished sfng_cube_compare.pro for '+use_c1file+' and '+use_c2file,/info
   if not keyword_set(nostop) then stop

  the_end:
  return
  
end
