pro sfng_cube_inspect,datadir=datadir,outdir=outdir,plotdir=plotdir,reportdir=reportdir $
                      ,fits_in=fits_in $
                      ,fits_mdl=fits_mdl $
                      ,fits_res=fits_res $
                      ,idl_in=idl_in $
                      ,hdr_in=hdr_in $
                      ,namestr=namestr $
                      ,savedir=savedir,savefile=savefile,tagname=tagname $
                      ,galaxy=galaxy, line_frequency=line_frequency, target_beam=target_beam $
                      ,expand_mask_edges=expand_mask_edges $
                      ,jy2k=jy2k, rebaseline=rebaseline $
                      ,allchannels=allchannels $
                      ,help=help,verbose=verbose,noreport=noreport,nostop=nostop $
                      ,nice=nice, extrachanmaps=extrachanmaps,simple=simple

;+ NAME:
;     sfng_cube_inspect
; PURPOSE:
;     inspect a cube. Essentially a wrapper to a library of subroutines that calculate
;     various statistics about emission, masks and emission-free regions in a cube.
; INPUTS:
;     idl_in = IDL cube file 
;     hdr_in = corresponding input header
;     fits_in = input FITS file (either IDL/FITS input must be provided)
;     fits_mdl = input FITS file for the model cube
;     fits_res = input FITS file for the residuals cube
;     fits_clean = input FITS file for the clean support cube
; OPTIONAL INPUT:
;     datadir = directory for input FITS file. Defaults to
;              current directory.
;     outdir = output directory for FITS files generated during inspection. Defaults to
;              current directory.
;     plotdir = output directory for plots generated during
;               comparison. Defaults to current directory
;     reportdir = output directory for final PDF report. Defaults to
;                current directory
;     savedir = output directory for IDL save structure. Defaults to
;                current directory
;     jy2k =  flag (0-no,1-yes) about whether cube requires conversion from Jy to K. Default is no.
;     rebaseline = flag indicating whether profiles in cube requires rebaselining.
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
;                   cube inspection is conducted. Default is to
;                   inspect input cube without smoothing.
;     expand_mask_edges: two-element vector indicating # pixels and #
;                        of channels by which to reduce the inspection
;                        region. Default is zero (i.e. use observed FoV) 
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
;     extrachanmaps = generate channel maps of the model, residual and
;                     clean support cubes (if provided)
;     simple =  create a PDF with the channel maps only.
; EXAMPLES
;       sfng_cube_inspect,datadir=use_datadir,outdir=use_outdir,plotdir=use_plotdir $
;                    ,reportdir=use_reportdir,savedir=use_savedir $
;                    , fits_in=use_file,savefile=use_savefile $
;                    , jy2k=1,rebaseline=-1,expand_mask_edges=[5,2] $
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
     doc_library,'sfng_cube_inspect'
     goto,the_end
  ENDIF

;===================
; defaults
;===================

  @sfng_constants.bat
  sfng_define_la_common
  
  ; things that user can change via keywords
  use_savefile='cubeinspect_results.sav'
  use_file='IDL_CUBE'
  use_datadir = './orig_data/'
  use_savedir = './savefiles/'
  use_outdir ='./good_data/'
  use_plotdir = './plots/'
  use_reportdir = './report/'
  use_tagname = 'mygalaxy'
  use_line_frequency = restfreq_12co21/1.e9
  do_jy2k = 0 ; -- 1 means do Jy->K conversion
  use_rebaseline = -1 ; -1 (do nothing) 0 (offset), 1 (linear) or 2 (quadratic) -- one value for each cube
  use_expand_mask_edges = [0,0] ; [# edge_pixels, # edge_channels] to be blanked (in addition to common FoV)
  do_report=1
  use_allchannels=0
  use_namestr='CUBE'
  
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

  
;===================
; process user inputs
;===================

  if keyword_set(fits_in) then use_file=fits_in
  if keyword_set(datadir) then use_datadir=datadir
  if keyword_set(outdir) then use_outdir=outdir
  if keyword_set(plotdir) then use_plotdir=plotdir
  if keyword_set(reportdir) then use_reportdir=reportdir
  if keyword_set(savedir) then use_savedir=savedir
  if keyword_set(tagname) then use_tagname=tagname
  if keyword_set(line_frequency) then use_line_frequency=line_frequency
  if keyword_set(target_beam) then use_target_beam=target_beam
  if keyword_set(galaxy) then use_galaxy=galaxy
  if keyword_set(noreport) then do_report = 0
  if keyword_set(allchannels) then use_allchannels = 1
  if keyword_set(jy2k) then do_jy2k = jy2k
  if keyword_set(rebaseline) then use_rebaseline = rebaseline
  if keyword_set(namestr) then use_namestr=namestr

;===================
; enforce final back slash and make sure directories exist
;===================
  ;; use_datadir=file_search(use_datadir,/mark,/full)
  ;; use_outdir=file_search(use_outdir,/mark,/full)
  ;; use_plotdir=file_search(use_plotdir,/mark,/full)
  ;; use_reportdir=file_search(use_reportdir,/mark,/full)
  ;; use_savedir=file_search(use_savedir,/mark,/full)

  ;; if use_datadir eq '' or use_outdir eq '' or $
  ;;    use_plotdir eq '' or use_savedir eq '' then $
  ;;    message,'Problem with data/out/plot/save directories. Do they exist?'

  ;; if do_report eq 1 and use_reportdir eq '' then $
  ;;    message,'Problem with report directory?'

  spawn,'ls -dp1 '+use_datadir, use_datadir,exit_status=datadir_status
  spawn,'ls -dp1 '+use_plotdir, use_plotdir,exit_status=plotdir_status
  spawn,'ls -dp1 '+use_outdir, use_outdir,exit_status=outdir_status
  spawn,'ls -dp1 '+use_savedir, use_savedir,exit_status=savedir_status
  spawn,'ls -dp1 '+use_reportdir, use_reportdir,exit_status=reportdir_status

  if datadir_status ne 0 or plotdir_status ne 0 or $
     outdir_status ne 0 or savedir_status ne 0 then $
     message,'Problem with data/out/plot/save directories. Do they exist?'

  if do_report eq 1 and reportdir_status ne 0 then $
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
  if keyword_set(fits_in) then c1=readfits(use_datadir+use_file,c1hdr)
  if keyword_set(idl_in) then c1=idl_in
  if keyword_set(hdr_in) then c1hdr=hdr_in

  if n_elements(c1) eq 0 or $
     n_elements(c1hdr) eq 0 then begin
     message,'Problem with input data and/or header information',/info
     goto, the_end
  end

;==============
; initalize the results structure
;==============
  
  cinsp_str=sfng_empty_cubeinsp_str()
  cinsp_str.c1_file=use_file

;==============
; initialise galaxy information if set
;==============

  if keyword_set(use_galaxy) then $
     gstr=gal_data(use_galaxy)

;==============
; check header information
; verify that units are K, km/s, and that cubes have beam and grid information in header.
;==============
     
  if keyword_set(verbose) then message,'Checking header for data: '+use_file,/info

  c1hdr_fix=c1hdr
  pass1=sfng_check_header(hdr=c1hdr, fixhdr=c1hdr_fix, comments = c1_comments $
                            , beam=c1_beam, pixscale=c1_pixscale, chanw=c1_chanw, unit=c1_bunit,casa_version=c1_casa)
     
    
  if pass1 ne 1 or keyword_set(verbose) then begin
     print,'&%&%&%&%&%&%&% CUBE INFO &%&%&%&%&%&%&%'
     print, c1_comments, format='(a)'
;     stop
  end
        
  cinsp_str.c1_comments=strjoin(c1_comments,';')
  cinsp_str.c1_pixscale=c1_pixscale*3600.
  cinsp_str.c1_beam=c1_beam
  cinsp_str.c1_bunit=c1_bunit
  cinsp_str.c1_casa=c1_casa
  cinsp_str.c1_chanw=c1_chanw
  cinsp_str.c1_dims=size(c1,/dim)
  
  if pass1 ne 1 then begin
     print,'Header did not pass sfng_check_header.'
     if keyword_set(nice) then begin
        print,'Check logs, then .c to continue'
        stop
     end
  end
     
  c1hdr = c1hdr_fix
  
  writefits,use_outdir+use_file+'.fixhdr.fits',c1,c1hdr

;==============
; convert to K from Jy/beam
;==============

  if do_jy2k ne 0 then begin

     c1K=sfng_convert_cube_jy2k(in = c1, hdr_in=c1hdr, hdr_out=new_c1hdr, factor=c1fact, freq=use_line_frequency)
     c1fact_str=strtrim(string(c1fact),2)
     c1_comments=[c1_comments,'Converted from Jy/beam to K with factor = '+c1fact_str]
     c1_comments=[c1_comments,'New units are K']
     cinsp_str.c1_comments=strjoin(c1_comments,';')
     cinsp_str.c1_jy2k_flag=1
     c1=c1K & c1hdr=new_c1hdr
     if keyword_set(verbose) then print, 'Converted cube from Jy/beam to K with factor = '+c1fact_str
     writefits,use_outdir+use_file+'.K.fits',c1,c1hdr
     
  end


;==============
; convolve the cube 
;==============

   sfng_convolve_to_res,idl_in=c1,hdr_in=c1hdr $
                    ,idl_out=c1match,hdr_out=c1hdr_out $
                   ,target_res=use_target_beam[0],/arcsec,galaxy=use_galaxy $
                   ,outdir=use_outdir,fits_out=use_file+'.convolve.fits',/verbose

   c1=c1match & c1hdr=c1hdr_out

   match_cdelt1=sxpar(c1hdr,'CDELT1',count=cdelt1_ct)
   match_cdelt3=sxpar(c1hdr,'CDELT3',count=cdelt3_ct)
   match_bmaj=sxpar(c1hdr,'BMAJ',count=bmaj_ct)
   match_pixdim=size(c1,/dim)
   nx=match_pixdim[0] & ny=match_pixdim[1] & nchans=match_pixdim[2]
   chanw_kms=abs(match_cdelt3)
   pixscale_as=abs(match_cdelt1)*3600.
   angres_as=match_bmaj*3600.
   
   cinsp_str.pixscale_as=abs(match_cdelt1)*3600.
   cinsp_str.chanw_kms=abs(match_cdelt3)
   cinsp_str.angres_as=match_bmaj*3600.
   cinsp_str.dims=match_pixdim

   c1_comments=[c1_comments,'Brought to requested resolution']
   c1_comments=[c1_comments,'Pixscale is now [as]: '+strtrim(string(abs(match_cdelt1)*3600.),2)]
   c1_comments=[c1_comments,'Channel width is now [km/s]: '+strtrim(string(match_cdelt3),2)]
   c1_comments=[c1_comments,'Resolution is now [as]: '+strtrim(string(abs(match_bmaj)*3600.),2)]
   c1_comments=[c1_comments,'Dimensions are now [x,y,v]: '+strtrim(string(nx),2)+','+strtrim(string(ny),2)+','+strtrim(string(nchans),2)]
   cinsp_str.c1_comments=strjoin(c1_comments,';')

;====================
; simple rebaseline
;====================

   if use_rebaseline ne -1 then begin
      
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
                      ,order=use_rebaseline $
                      ,fits_out=use_tagname+'_c1_rebase' $
                      ,outdir=use_outdir 

      c1=c1_rebase & c1hdr=c1_rebase_hdr 
      
      cinsp_str.c1_rebase_flag=1
      cinsp_str.c1_rebase_order=use_rebaseline[0]
      
      c1_comments=[c1_comments,'Rebaselined cube with order: '+strtrim(string(use_rebaseline),2)]
      cinsp_str.c1_comments=strjoin(c1_comments,';')
      
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
   
   cinsp_str.c1_rmsmap_fits=use_tagname+'_c1noise2d.fits'
   cinsp_str.c1_rmscube_fits=use_tagname+'_c1noise.fits'

;==============
; enlarge FoV mask, e.g. to exclude noisy edge pixels/channels 
;==============

   common_fov_mask=fix(c1*0.)
   fovidx=where(finite(c1) eq 1, fct) 
   common_fov_mask[fovidx]=1
   
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

  
   c1=c1_newmask & c1hdr=c1hdrout
   common_fov_mask=common_fov_mask_exp
   
   cinsp_str.cmp_fov_fits=use_tagname+'_finalfovmask.fits'

   c1_comments=[c1_comments,'Restricted FoV mask by '+strtrim(string(use_pixel_blank),2)+ $
                ' pixels, and '+strtrim(string(use_chan_blank),2)+' channels']
   cinsp_str.c1_comments=strjoin(c1_comments,';')

end

;==============
; final significant emission masks (after rebaselining) for each cube
; and signal/nosignal masks
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

   no_sigmask=(c1sigmask eq 0)  and (finite(c1) eq 1)
   
   nosigmsk_hdr=c1sigmsk_hdr
   writefits,use_outdir+use_tagname+'_c1_nosignalmask.fits',no_sigmask,nosigmsk_hdr

   cinsp_str.c1_signalmask_fits=use_tagname+'_c1_signalmask.fits'
   cinsp_str.c1_nosignalmask_fits=use_tagname+'_c1_nosignalmask.fits'
   
 
;======================
; STATISTICS RE EMISSION EXTENT
;======================

   fovidx=where(common_fov_mask eq 1, fovct)  
   c1sigidx=where(c1sigmask eq 1, c1ct, comp=c1nosigidx);,ncomp=noisect_c1)
   nosigidx=where(no_sigmask eq 1, nsct)

   noisect_c1=fovct-c1ct
   
   cinsp_str.npix_tot=float(match_pixdim[0])*match_pixdim[1]*match_pixdim[2]
   cinsp_str.npix_cmp_fov=fovct
   cinsp_str.c1_npix_signalmask=c1ct
   cinsp_str.npix_nosignalmask=nsct

;==============================================
; STATISTICS RE TOTAL FLUX AND NUMBER OF PIXELS
;==============================================
   
  cinsp_str.c1_totflux=total(c1,/nan)
  cinsp_str.c1_peak=max(c1,/nan)
  if c1ct gt 0 then cinsp_str.c1_totflux_signalmask=total(c1[c1sigidx],/nan)
  if nsct gt 0 then cinsp_str.c1_totflux_nosignalmask=total(c1[nosigidx],/nan)

  cinsp_str.npix_cmp_fov=fovct
  cinsp_str.c1_npix_signalmask=c1ct
  cinsp_str.npix_nosignalmask=nsct


;======================
; generate noise histograms
;======================

  nzmin=min(c1[nosigidx],/nan) 
  nzmax=max(c1[nosigidx],/nan)
  nzmin_disp=percentile(c1[nosigidx],99)
  nzmax_disp=percentile(c1[nosigidx],1) 
  nbins=round(noisect_c1/1000.)
  xlim=abs(nzmin_disp)>abs(nzmax_disp)
  xr=[-xlim,xlim]
  !p.position=[0.2,0.2,0.8,0.8]

    
;======================
; CUBE 1
;======================

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
                ,[use_namestr, $
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

  cinsp_str.c1_noisestats=c1_noisestats
  cinsp_str.c1_noisestats_nosignal=c1_noisestats_nosignal
 
;======================
; STATISTICS RE FLUX, FIDELITY, RMS PER CHANNEL
;======================

  c1_chanflux=fltarr(nchans)
  c1_chanflux_sm=fltarr(nchans)
  c1_chanflux_nosm=fltarr(nchans)
  c1_chanrms=fltarr(nchans)
  c1_chanrms_nosm=fltarr(nchans)
  
  for i=0,nchans-1 do begin

     c1_thisplane=c1[*,*,i]
     c1_noise_thisplane=c1noise[*,*,i]
     nosm_thisplane=no_sigmask[*,*,i]
     sm_thisplane=c1sigmask[*,*,i]
     fov_thisplane=common_fov_mask[*,*,i]
     
     c1_thisplane_fov_idx=where(fov_thisplane eq 1 and finite(c1_thisplane) eq 1,fc1ct)
     c1_thisplane_nosm_idx=where(nosm_thisplane eq 1 and finite(c1_thisplane) eq 1,nc1ct)
     c1_thisplane_sm_idx=where(sm_thisplane eq 1 and finite(c1_thisplane) eq 1,c1ct)
     
     c1_chanflux[i]=total(c1_thisplane,/nan)
     if c1ct gt 0 then c1_chanflux_sm[i]=total(c1_thisplane[c1_thisplane_sm_idx],/nan)
     if nc1ct gt 0 then c1_chanflux_nosm[i]=total(c1_thisplane[c1_thisplane_nosm_idx],/nan)


     if nc1ct gt 0 then c1_chanrms_nosm[i]=robust_sigma(c1_thisplane[c1_thisplane_nosm_idx])
     if fc1ct gt 0 then c1_chanrms[i]=robust_sigma(c1_thisplane[c1_thisplane_fov_idx])

  endfor

  emission_startchan=0
  emission_endchan=nchans-1
  emission_idx=where(c1_chanflux_sm gt 0, emct)
  if emct gt 0 then begin
     emission_startchan=emission_idx[0]
     emission_endchan=emission_idx[emct-1] ; can't use -1 index for idl v7
  end

  
  
  cinsp_str.c1_fluxperchan=ptr_new(c1_chanflux)
  cinsp_str.c1_fluxperchan_signalmask=ptr_new(c1_chanflux_sm)
  cinsp_str.c1_rmsperchan=ptr_new(c1_chanrms)
  cinsp_str.c1_rmsperchan_nosignalmask=ptr_new(c1_chanrms_nosm)
  cinsp_str.emission_startchan=emission_startchan
  cinsp_str.emission_endchan=emission_endchan

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
  ymax=max(c1_chanflux,/nan) 
  yr=[-0.1*ymax,1.1*ymax]
  tit='Input Cube'
  
  cgplot,chans,c1_chanflux,xtit='Channel number',ytit='Sum(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanflux,color=cgcolor('red'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1
  
  al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_namestr] $
            ,lines=[-99,0] $
            ,colors=[fgcolor,cgcolor('red')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

  
;======================
; b. luminosity per channel -- only done if we know galaxy distance
;======================

  if keyword_set(use_galaxy) then begin
     use_pngfile='luminosity_per_channel.png'
     
     c1_chanlum=c1_chanflux*pixscale_as*pixscale_as*gstr.dist_mpc*gstr.dist_mpc*Mpc_on_AU*Mpc_on_AU

     window,use_win,xsize=900,ysize=400 & use_win=use_win+1
     !p.position=[0.2,0.2,0.9,0.8]
     loadct,39 & fgcolor=255
     xr=[0,nchans] & xr1=[vchans[0],vchans[nchans-1]]
     ymax=max(c1_chanlum,/nan)
     yr=[-0.1*ymax,1.1*ymax]
     tit='Input Cubes'
     
     cgplot,chans,c1_chanlum,xtit='Chans',ytit='Luminosity [K pc2]',charsize=1.5,color=fgcolor,charthick=1.8 $
            ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
     cgplot,chans,chans*0,lines=1,/overplot
     cgplot,chans,c1_chanlum,color=cgcolor('red'),psym=10,thick=2,/overplot
     cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1

      al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_namestr] $
            ,lines=[-99,0] $
            ,colors=[fgcolor,cgcolor('red')] $
            ,charsize=1.,thick=2,charthick=1.8

     write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
     
  end
  
;======================
; c. Flux within signal mask per channel
;======================

  use_pngfile='flux_per_channel_signalmask.png'
  
  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  !p.position=[0.2,0.2,0.9,0.8]
  xr=[0,nchans] & xr1=[vchans[0],vchans[nchans-1]]
  ymax=max(c1_chanflux,/nan) 
  yr=[-0.1*ymax,1.1*ymax]
  tit='Input Cube, SigMask'
  
  cgplot,chans,c1_chanflux_sm,xtit='Channel number',ytit='Sum(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanflux_sm,color=cgcolor('red'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1

  al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_namestr] $
            ,lines=[-99,0] $
            ,colors=[fgcolor,cgcolor('red')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; a. RMS per channel
;======================

  use_pngfile='rms_per_channel.png'

  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  ymax=max(c1_chanrms,/nan)
  ymin=min(c1_chanrms,/nan) 
  
  if ymin lt 0 then yr=[1.1*ymin,1.1*ymax]
  if ymin ge 0 then yr=[0.8*ymin,1.1*ymax]
  tit='Input Cube'
  
  cgplot,chans,c1_chanrms,xtit='Channel number',ytit='RMS(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanrms,color=cgcolor('red'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1

   al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_namestr] $
            ,lines=[-99,0] $
            ,colors=[fgcolor,cgcolor('red')] $
            ,charsize=1.,thick=2,charthick=1.8


  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)


  
;======================
; b. RMS per channel in nosigmask
;======================

  use_pngfile='rms_per_channel_nosignalmask.png'

  window,use_win,xsize=900,ysize=400 & use_win=use_win+1
  ymax=max(c1_chanrms_nosm,/nan)
  ymin=min(c1_chanrms_nosm,/nan)
  
  if ymin lt 0 then yr=[1.1*ymin,1.1*ymax]
  if ymin ge 0 then yr=[0.8*ymin,1.1*ymax]
  tit='Input Cube, No Signal Mask'
  
  cgplot,chans,c1_chanrms_nosm,xtit='Channel number',ytit='RMS(Tmb [K]) in Channel',charsize=1.5,color=fgcolor,charthick=1.8 $
       ,/nodata,xstyle=8,/ysty,yr=yr,xr=xr,xthick=2,ythick=2,thick=2
  cgplot,chans,chans*0,lines=1,/overplot
  cgplot,chans,c1_chanrms_nosm,color=cgcolor('red'),psym=10,thick=2,/overplot
  cgaxis,/xaxis,xr=xr1,tit='Velocity [km/s]',charthick=1.8,charsize=1.5,xthick=2,xstyle=1

     al_legend,/top,/left,clear=0, box=0 $
            , [tit, $
               use_namestr] $
            ,lines=[-99,0] $
            ,colors=[fgcolor,cgcolor('red')] $
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
  c1indefidx=where(finite(c1) eq 0,comp=c1defidx,ict,ncomp=dct)
  if ict gt 0 then c1def=c1[c1defidx]
  immax=percentile(c1def,0.5) & immin=percentile(c1def,85)

;  print,'Image minimum is:',immin
;  print,'Image maximum is:',immax

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

; emission mask

  use_win=use_win+1
  loadct,0
  reversect
  immin=0. & immax=1.
  for k=start_chan,end_chan do begin
     window,use_win,xsize=400,ysize=400 
     cgImage, c1sigmask[*,*,k],  /Axes,  charsize=0.8, minval=immin, maxval=immax $
              , Position=[0.05,0.05,0.9,0.9] $
              , tit='channel '+strtrim(STRING(fix(k+1)),2)
     use_pngfile='signalmask_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
     write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
  endfor



  if keyword_set(extrachanmaps) then begin

     if keyword_set(fits_mdl) then begin
        m1=readfits(use_datadir+fits_mdl,mhdr)
        use_idx=where(finite(m1) eq 1 and m1 ne 0)
        immax=percentile(m1[use_idx],0.5) & immin=percentile(m1[use_idx],99.5)
        loadct,12,rgb_table=palette
        TVLCT, cgColor('gray', /Triple), 0
        use_win=use_win+1
        for k=start_chan,end_chan do begin
           window,use_win,xsize=400,ysize=400 
           cgImage, m1[*,*,k],  /Axes, Palette=palette, charsize=0.8, minval=immin, maxval=immax $
                    ,tit='channel '+strtrim(STRING(fix(k+1)),2) $
                    ,Position=[0.05,0.05,0.9,0.9] 
           use_pngfile='mdl1_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
           write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
        endfor
     end
     
     if keyword_set(fits_res) then begin
        r1=readfits(use_datadir+fits_res,rhdr)
        immax=percentile(r1,0.5) & immin=percentile(r1,85)
        loadct,20,rgb_table=palette
        TVLCT, cgColor('gray', /Triple), 0
        use_win=use_win+1
        for k=start_chan,end_chan do begin
           window,use_win,xsize=400,ysize=400 
           cgImage, r1[*,*,k],  /Axes, Palette=palette, charsize=0.8, minval=immin, maxval=immax $
                    ,tit='channel '+strtrim(STRING(fix(k+1)),2) $
                    ,Position=[0.05,0.05,0.9,0.9] 
           use_pngfile='res1_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
           write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
        endfor
     end

     if keyword_set(fits_clean) then begin
        csm1=readfits(use_datadir+fits_res,rhdr)
        loadct,0
        reversect
        immin=0. & immax=1.
        use_win=use_win+1
        for k=start_chan,end_chan do begin
           window,use_win,xsize=400,ysize=400 
           cgImage, csm1[*,*,k],  /Axes, Palette=palette, charsize=0.8, minval=immin, maxval=immax $
                    ,tit='channel '+strtrim(STRING(fix(k+1)),2) $
                    ,Position=[0.05,0.05,0.9,0.9] 
           use_pngfile='cln1_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
           write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
        endfor
     end

  end
  
  
  
save_structure:
;================
;save the results structure
;================

   save,file=use_savedir+savefile,cinsp_str

   produce_report:
;================
;make a PDF report
;================

   if do_report eq 1 then begin
      sfng_make_latex_elements,cinsp_str,reportdir=use_reportdir,plotdir=use_plotdir,/inspect,type='INSPECT'
      sfng_compile_latex,reportdir=use_reportdir,plotdir=use_plotdir,/inspect
   end

   if keyword_set(verbose) then message,'Finished sfng_cube_inspect.pro for '+use_file,/info
   if not keyword_set(nostop) then stop

  the_end:
  return
  
end
