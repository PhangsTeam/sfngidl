pro sfng_cube_show,galaxy,datadir=datadir,plotdir=plotdir,reportdir=reportdir $
                      ,cube_fits=cube_fits, cube_in=cube_in, cube_hdr=cube_hdr $
                      ,mask_fits=mask_fits, mask_in=mask_in, mask_hdr=mask_hdr $
                      ,noise_fits=noise_fits, noise_in=noise_in, noise_hdr=noise_hdr $
                      ,resids_fits=resids_fits, resids_in=resids_in, resids_hdr=resids_hdr $
                      ,namestr=namestr $
                      ,help=help,verbose=verbose,noreport=noreport,nostop=nostop $
                      ,nice=nice

;+ NAME:
;     sfng_cube_show
; PURPOSE:
;     make pdf with several diagnostics:
;     (i)  4x4 figure of channel maps of a cube, overlaid with
;     deconvolution mask contours
;     (ii)  4x4 figure of residual cube
;     (iii) histogram of residual values in quadrants, overlaid with
;     deconvolution mask contours
;     (iv) a figure showing the noise map
;     (v) histogram of noise values in quadrants
; As per ADR minutes, the checks are:  
; The noise level should be homogeneous except at the very edge of the mosaics.
; The deconvolution masks should encompass all the signal. 
; There should be no more spikes.
; Residual cubes should look like noise.
;
; INPUTS:
;     galaxy = string, name of galaxy to check
; OPTIONAL INPUT:
;     datadir = directory for input FITS files. Defaults to
;              current directory.
;     plotdir = output directory for plots generated during
;               comparison. Defaults to current directory
;     reportdir = output directory for final PDF report. Defaults to
;                current directory
;     cube_in/fits/hdr = data IDL/header/FITS file (if specified, overrides default)
;     mask_in/fits/hdr = deconvolution mask IDL/header/FITS file (if specified, overrides default)
;     noise_in/fits/hdr = noise IDL/header/FITS file (if specified, overrides default)
;     resids_in/fits/hdr = resids IDL/header/FITS file (if specified, overrides default)
  
; ACCEPTED KEY-WORDS:
;     help = print this help
;     verbose = print extra information to screen
;     noreport = don't generate a PDF file, just make the plots
;                and IDL save structure of results
;     nostop = exit the routine when finished. If not set, execution
;              will pause just before return statement at end of routine.
;     nice  =  pause before deleting existing .png and .tex files in
;              the plots and report directories. Pause to review
;              header information
; EXAMPLES
;       sfng_cube_show,'NGC628',datadir=use_datadir,plotdir=use_plotdir $
;                    ,reportdir=use_reportdir, /verb,/nice
; OUTPUTS:
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
;-

  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_cube_show'
     goto,the_end
  ENDIF

  IF not keyword_set(galaxy) or size(galaxy,/type) ne 7 THEN BEGIN
     message,'sfng_cube_show requires galaxy name (string) as input'
     goto,the_end
  ENDIF

;===================
; defaults
;===================

  @sfng_constants.bat
  sfng_define_la_common
  use_cubesuffix='_7m_co21_pbcorr_round_k.fits'
  use_noisesuffix='_7m_co21_noise_pbcorr_round_k.fits'
  use_residsuffix='_7m_co21_residual.fits' ; to be changed
  use_masksuffix='_7m_co21_mask.fits' ; to be changed
  
  
  ; things that user can change via keywords
  use_cube_file=galaxy+use_cubesuffix
  use_noise_file=galaxy+use_noisesuffix
  use_resids_file=galaxy+use_residsuffix
  use_mask_file=galaxy+use_masksuffix
  use_datadir = './'
  use_plotdir = './'
  use_reportdir = './'
  do_report=1
  use_namestr=galaxy
  
  ; things that user can change here 
  use_win = 0L
  nan=!values.f_nan

  
;===================
; process user inputs
;===================

  if keyword_set(datadir) then use_datadir=datadir
  if keyword_set(plotdir) then use_plotdir=plotdir
  if keyword_set(reportdir) then use_reportdir=reportdir
  if keyword_set(noreport) then do_report = 0
  if keyword_set(namestr) then use_namestr=namestr
  if keyword_set(cube_fits) then use_cube_file=cube_fits
  if keyword_set(noise_fits) then use_noise_file=noise_fits
  if keyword_set(mask_fits) then use_mask_file=mask_fits
  if keyword_set(resids_fits) then use_resids_file=resids_fits
  
;===================
; enforce final back slash and make sure directories exist
;===================

  spawn,'ls -dp1 '+use_datadir, use_datadir,exit_status=datadir_status
  spawn,'ls -dp1 '+use_plotdir, use_plotdir,exit_status=plotdir_status
  spawn,'ls -dp1 '+use_reportdir, use_reportdir,exit_status=reportdir_status

  if datadir_status ne 0 or plotdir_status ne 0 then $
     message,'Problem with data/plot directories. Do they exist?'

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
; initialize the results structure
;==============

  cstr=sfng_empty_cubeshow_str()

;==============
; read data
;==============
  
  if keyword_set(verbose) then message,'Reading data',/info

  if keyword_set(cube_in) and keyword_set(cube_hdr)  then begin
     c1=cube_in & c1hdr=cube_hdr
  end else c1=readfits(use_datadir+use_cube_file,c1hdr)

  if n_elements(c1) eq 0 or $
     n_elements(c1hdr) eq 0 then begin
     message,'Problem with input data and/or header information',/info
     goto, the_end
  end

  ;; make_cprops_mask, indata = c1 $
  ;;                   , outmask = c1sigmask $
  ;;                   , hi_thresh = 8 $
  ;;                   , lo_thresh = 1.5 $
  ;;                   , hi_nchan=2 $
  ;;                   , lo_nchan=2

  ;; make_noise_cube, cube_in = c1 $
  ;;                   , cube_hdr = c1hdr $
  ;;                   , out_cube = n1 $
  ;;                   , mask_in = c1sigmask $
  ;;                   , /iterate $
  ;;                   , box=3 $
  ;;                   , spec_box=0

  ;;  n1hdr=c1hdr
  ;;  sxaddpar,n1hdr,'DATAMAX',max(n1,/nan)
  ;;  sxaddpar,n1hdr,'DATAMIN',min(n1,/nan)
  ;;  sxaddpar,n1hdr,'HISTORY','Noise cube generated by make_noise_cube'
  ;;  writefits,use_datadir+use_noise_file,n1,n1hdr

  ;;  stop

  
  if keyword_set(noise_in) and keyword_set(noise_hdr)  then begin
     n1=noise_in & n1hdr=noise_hdr
  end else n1=readfits(use_datadir+use_noise_file,n1hdr)

  if n_elements(n1) eq 0 or $
     n_elements(n1hdr) eq 0 then begin
     message,'Problem with input noise and/or header information',/info
     goto, the_end
  end
  
  if keyword_set(mask_in) and keyword_set(mask_hdr)  then begin
     m1=mask_in & m1hdr=mask_hdr
  end else m1=readfits(use_datadir+use_mask_file,m1hdr)

  if n_elements(m1) eq 0 or $
     n_elements(m1hdr) eq 0 then begin
     message,'Problem with input mask and/or header information',/info
     goto, the_end
  end

  if keyword_set(resids_in) and keyword_set(resids_hdr)  then begin
     r1=resids_in & r1hdr=resids_hdr
  end else r1=readfits(use_datadir+use_resids_file,r1hdr)

  if n_elements(r1) eq 0 or $
     n_elements(r1hdr) eq 0 then begin
     message,'Problem with input resids and/or header information',/info
     goto, the_end
  end
  
;==============
; regrid if needed
 ;==============

  if total(size(n1,/dim)) ne total(size(c1,/dim)) then begin
     message,'Regridding noise cube',/info
     cube_hastrom,data=n1,hdr_in=n1hdr $
                  ,outcube=n1rgd,outhdr=n1rgd_hdr $
                  ,target_hdr=c1hdr,operati='BOT'
     n1=n1rgd & n1hdr=n1rgd_hdr
     cstr.n1_rgd=1
  end

  if total(size(r1,/dim)) ne total(size(c1,/dim)) then begin
     message,'Regridding residuals cube',/info
     cube_hastrom,data=r1,hdr_in=r1hdr $
                  ,outcube=r1rgd,outhdr=r1rgd_hdr $
                  ,target_hdr=c1hdr,operati='BOT'
     r1=r1rgd & r1hdr=r1rgd_hdr
     cstr.r1_rgd=1
  end

  if total(size(m1,/dim)) ne total(size(c1,/dim)) then begin
     message,'Regridding mask cube',/info
     cube_hastrom,data=m1,hdr_in=m1hdr $
                  ,outcube=m1rgd,outhdr=m1rgd_hdr $
                  ,target_hdr=c1hdr,operati='BOT',pinterp=0,vinterp=0
     m1=m1rgd & m1hdr=m1rgd_hdr
     cstr.m1_rgd=1
  end

  
;==============
; get some basic information about cubes
;==============

  naxis1=sxpar(c1hdr,'NAXIS1')
  naxis2=sxpar(c1hdr,'NAXIS2')
  x0=naxis1/4 & x1=3*x0
  y0=naxis2/4 & y1=3*y0

  c1sigidx=where(m1 eq 1, c1ct, comp=c1nosigidx,ncomp=noisect) ;,ncomp=noisect_c1)

  cstr.galaxy=galaxy
  cstr.c1_file=use_cube_file
  cstr.n1_file=use_noise_file
  cstr.r1_file=use_resids_file
  cstr.m1_file=use_mask_file
  
  cstr.c1_totflux=total(c1,/nan)
  cstr.c1_peak=max(c1,/nan)

  if c1ct gt 0 then begin
     cstr.c1_totflux_signalmask=total(c1[c1sigidx],/nan)
     cstr.c1_peak_signalmask=max(c1[c1sigidx],/nan)
  end
  if noisect gt 0 then begin
     cstr.c1_totflux_nosignalmask=total(c1[c1nosigidx],/nan)
     cstr.c1_peak_nosignalmask=max(c1[c1nosigidx],/nan)
  end
  cstr.r1_totflux=total(r1,/nan)
  cstr.r1_peak=max(abs(r1),/nan)

;==============
; check header information
; verify that units are K, km/s, and that cubes have beam and grid information in header.
;==============
     
  if keyword_set(verbose) then message,'Checking header for data: '+use_cube_file,/info

  c1hdr_fix=c1hdr
  pass1=sfng_check_header(hdr=c1hdr, fixhdr=c1hdr_fix, comments = c1_comments $
                            , beam=c1_beam, pixscale=c1_pixscale, chanw=c1_chanw, unit=c1_bunit, casa_version=c1_casa)
     
    
  if pass1 ne 1 or keyword_set(verbose) then begin
     print,'&%&%&%&%&%&%&% CUBE INFO &%&%&%&%&%&%&%'
     print, c1_comments, format='(a)'
;     stop
  end
        
  cstr.c1_comments=strjoin(c1_comments,';')
  cstr.c1_pixscale=c1_pixscale*3600.
  cstr.c1_beam=c1_beam
  cstr.c1_bunit=c1_bunit
  cstr.c1_casa=c1_casa
  cstr.c1_chanw=c1_chanw
  pixdim=size(c1,/dim)
  cstr.c1_dims=pixdim
  nchans=pixdim[2]
  
  if pass1 ne 1 then begin
     print,'Header did not pass sfng_check_header.'
     if keyword_set(nice) then begin
        print,'Check logs, then .c to continue'
        stop
     end
  end
     
  c1hdr = c1hdr_fix
  
;======================
; 
;======================


  
;======================
; CHANNEL MAPS -- DATA
;======================

; hack while we don't have a mask
;  thresh=max(c1,/nan)
;  bad=where(c1 lt 0.1*thresh or finite(c1) eq 0,comp=good)
;  m1(good)=1 & m1(bad)=0

;  show all channels in cube
  start_chan=0 & end_chan=nchans-1
  
  use_win = 0L
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
              ,Position=[0.05,0.05,0.9,0.9],/save
     cgContour, m1[*,*,k], LEVELS=1, /OnImage, Color='black',thick=2
     use_pngfile='c1_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
     write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
;     wait,5

  endfor

;======================
; CHANNEL MAPS -- RESIDUALS
;======================
  
  use_win = 0L
  loadct,20,rgb_table=palette
  TVLCT, cgColor('gray', /Triple), 0

  r1def=r1
  r1indefidx=where(finite(r1) eq 0,comp=r1defidx,ict,ncomp=dct)
  if ict gt 0 then r1def=r1[r1defidx]
  immax=percentile(r1def,0.5) & immin=percentile(r1def,85)

;  print,'Image minimum is:',immin
;  print,'Image maximum is:',immax

  resids_chans:
; resids
  use_win=use_win+1
  for k=start_chan,end_chan do begin
     window,use_win,xsize=400,ysize=400 
     cgImage, r1[*,*,k],  /Axes, Palette=palette, charsize=0.8, minval=immin, maxval=immax $
              ,tit='channel '+strtrim(STRING(fix(k+1)),2) $
              ,Position=[0.05,0.05,0.9,0.9],/save
     cgContour, m1[*,*,k], LEVELS=1, /OnImage, Color='black',thick=2
     use_pngfile='r1_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
     write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
;     wait,5

  endfor

;======================
; NOISE MAP (CUBE?)
;======================
  
  use_win = 0L
  loadct,20,rgb_table=palette
  TVLCT, cgColor('gray', /Triple), 0

  n1def=n1
  n1indefidx=where(finite(n1) eq 0,comp=n1defidx,ict,ncomp=dct)
  if ict gt 0 then n1def=n1[n1defidx]
  immax=percentile(n1def,0.5) & immin=percentile(n1def,85)

;  print,'Image minimum is:',immin
;  print,'Image maximum is:',immax

  noise_chans:
; resids
  use_win=use_win+1
;  for k=start_chan,end_chan do begin
  k=0
  window,use_win,xsize=400,ysize=400 
  cgImage, n1[*,*,k],  /Axes, Palette=palette, charsize=0.8, minval=immin, maxval=immax $
           ,tit='channel '+strtrim(STRING(fix(k+1)),2) $
           ,Position=[0.05,0.05,0.9,0.9],/save
  use_pngfile='n1_chan'+STRING(k+1, FORMAT='(I3.3)')+'.png'
  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
;     wait,5

;  endfor


;======================
; NOISE HISTOGRAM
;======================

; Noise

  noisect=n_elements(n1)
  nzmin=min(n1,/nan)
  nzmax=max(n1,/nan)
  nzmin_disp=percentile(n1,99) 
  nzmax_disp=percentile(n1,1) 
  xr=[nzmin,nzmax]
  !p.position=[0.2,0.2,0.8,0.8]
  nbins=round(noisect/1.e4) 

  nq1=cube_hextract(cube_in=n1, hdr_in=n1hdr, hdr_out=nq1hdr $
                    , x0=x0,x1=x1,y0=y0,y1=y1)

  noisestats=sfng_get_basic_stats(n1,/nan,/rob)
  noisestats_q=sfng_get_basic_stats(nq1,/nan,/rob)

  cstr.n1_stats=noisestats
  cstr.n1_stats_quarter=noisestats_q
  
  hh=histogram(n1,max=nzmax,min=nzmin,locations=nzbins,nbins=nbins)
  hhq=histogram(nq1,max=nzmax,min=nzmin,locations=nzbins,nbins=nbins)

  use_pngfile='noisehisto.png'

  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  yr=[0,1.2*(max(hh)/float(total(hh,/nan))) > 1.2*(max(hhq)/float(total(hhq,/nan)))]
  
  cgplot,nzbins,(hh/float(total(hh,/nan))),/ysty,/xsty $
         ,/nodata,xr=xr,yr=yr,xtit='Tmb [K]',ytit='% pixels' $
         ,xthick=2,ythick=2,thick=2,charsize=1.8,charthick=1.7
  cgplot,[0,0],[-100,100],lines=1,/overplot,thick=2
  cgplot,nzbins,(hh/float(total(hh,/nan))),psym=10,/overplot,thick=2
  cgplot,nzbins,(hhq/float(total(hhq,/nan))),psym=10,/overplot,thick=1,color=cgcolor('blue')

  al_legend, /top,/right, box=0,clear=0 $
                ,['Noise', $
                  'Mean: '+sigfig(noisestats.mean,3,/sci), $
                  'RMS: '+ sigfig(noisestats.rms,3,/sci)] $
                , lines=-99, charsize=1.4,charthick=1.7

  ;; al_legend, /top,/right, box=0,clear=0 $
  ;;            ,['Gauss Fit', $
  ;;              'mu: '+sigfig(coeffs[1],3,/sci), $
  ;;              'width: '+sigfig(coeffs[2],3,/sci)] $
  ;;            , lines=-99, charsize=1.8,charthick=1.7

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)

;======================
; RESIDUALS HISTOGRAM
;======================

  residsct=n_elements(r1)
  rzmin=min(r1,/nan)
  rzmax=max(r1,/nan)
  rzmin_disp=percentile(r1,99) 
  rzmax_disp=percentile(r1,1) 
  xlim=abs(rzmin_disp)>abs(rzmax_disp)
  xr=[-xlim,xlim]
  nbins=round(residsct/1000.) 

  rq1=cube_hextract(cube_in=r1, hdr_in=r1hdr, hdr_out=rq1hdr $
                    , x0=x0,x1=x1,y0=y0,y1=y1)

  residsstats=sfng_get_basic_stats(r1,/nan,/rob)
  residsstats_q=sfng_get_basic_stats(rq1,/nan,/rob)
  c1stats_nosig=sfng_get_basic_stats(c1[c1nosigidx],/nan,/rob)
  
  cstr.r1_stats=residsstats
  cstr.r1_stats_quarter=residsstats_q
  cstr.c1_stats_nosignal=c1stats_nosig

  hh=histogram(r1,max=rzmax,min=rzmin,locations=rzbins,nbins=nbins)
  hhq=histogram(rq1,max=rzmax,min=rzmin,locations=rzbins,nbins=nbins)
  hhc=histogram(c1[c1nosigidx],max=rzmax,min=rzmin,locations=rzbins,nbins=nbins)

  use_pngfile='residshisto.png'

  window,use_win,xsize=600,ysize=600 & use_win=use_win+1
  yr=[0,1.2*(max(hh)/float(total(hh,/nan))) > 1.2*(max(hhc)/float(total(hhc,/nan))) > 1.2*(max(hhq)/float(total(hhq,/nan)))]
  fitlim=0.2*(percentile(hh,1)/float(total(hh,/nan)))
  
  cgplot,rzbins,(hh/float(total(hh,/nan))),/ysty,/xsty $
         ,/nodata,xr=xr,yr=yr,xtit='Tmb [K]',ytit='% pixels' $
         ,xthick=2,ythick=2,thick=2,charsize=1.8,charthick=1.7
  cgplot,[0,0],[-100,100],lines=1,/overplot,thick=2
  cgplot,rzbins,(hh/float(total(hh,/nan))),psym=10,/overplot,thick=2
  cgplot,rzbins,(hhq/float(total(hhq,/nan))),psym=10,/overplot,thick=1,color=cgcolor('blue')
  cgplot,rzbins,(hhc/float(total(hhc,/nan))),psym=10,/overplot,thick=1,color=cgcolor('green')

  binsize=rzbins[1]-rzbins[0]
  binCenters = rzbins + (binsize/2.0)
  fitbins=where((hh/total(hh)) gt fitlim,fct)
  yfit = GaussFit(binCenters[fitbins],hh[fitbins]/float(total(hh,/nan)), coeffs, NTERMS=3)
  cgplot,binCenters[fitbins],yfit,color=cgcolor('red'),/overplot,thick=2

  al_legend, /top,/left, box=0,clear=0 $
                ,['Residuals', $
                  'Mean: '+sigfig(residsstats.mean,3,/sci), $
                  'RMS: '+ sigfig(residsstats.rms,3,/sci)] $
                , lines=-99, charsize=1.4,charthick=1.7

  al_legend, /top,/right, box=0,clear=0 $
             ,['Gauss Fit', $
               'mu: '+sigfig(coeffs[1],3,/sci), $
               'width: '+sigfig(coeffs[2],3,/sci)] $
             , lines=-99, charsize=1.4,charthick=1.7

  write_png,use_plotdir+use_pngfile,TVRD(/TRUE)
 
   produce_report:
;================
;make a PDF report
;================

   if do_report eq 1 then begin
      sfng_make_latex_elements,cstr,reportdir=use_reportdir,plotdir=use_plotdir,type='SHOW'
      sfng_compile_latex,reportdir=use_reportdir,plotdir=use_plotdir,/show
   end

   if keyword_set(verbose) then message,'Finished sfng_cube_show.pro for '+galaxy,/info
   if not keyword_set(nostop) then stop

   stop
   
  the_end:
  return
  
end
