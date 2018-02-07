pro sfng_cube_show,datadir=datadir,plotdir=plotdir,reportdir=reportdir $
                      ,fits_in=fits_in $
                      ,cube_in=cube_in $
                      ,hdr_in=hdr_in $
                      ,namestr=namestr $
                      ,help=help,verbose=verbose,noreport=noreport,nostop=nostop $
                      ,nice=nice

;+ NAME:
;     sfng_cube_show
; PURPOSE:
;     make pdf with 4x4 figure of channel maps of a cube
; INPUTS:
;     cube_in = IDL cube file 
;     hdr_in = corresponding input header
;     fits_in = input FITS file (either IDL/FITS input must be provided)
; OPTIONAL INPUT:
;     datadir = directory for input FITS file. Defaults to
;              current directory.
;     plotdir = output directory for plots generated during
;               comparison. Defaults to current directory
;     reportdir = output directory for final PDF report. Defaults to
;                current directory
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

;===================
; defaults
;===================

  @sfng_constants.bat
  sfng_define_la_common
  
  ; things that user can change via keywords
  use_file='IDL_CUBE'
  use_datadir = './'
  use_plotdir = './'
  use_reportdir = './'
  do_report=1
  use_namestr='CUBE'
  
  ; things that user can change here 
  use_win = 0L
  nan=!values.f_nan

  
;===================
; process user inputs
;===================

  if keyword_set(fits_in) then use_file=fits_in
  if keyword_set(datadir) then use_datadir=datadir
  if keyword_set(plotdir) then use_plotdir=plotdir
  if keyword_set(reportdir) then use_reportdir=reportdir
  if keyword_set(noreport) then do_report = 0
  if keyword_set(namestr) then use_namestr=namestr

;===================
; enforce final back slash and make sure directories exist
;===================
;  use_datadir=file_search(use_datadir,/mark,/full)
;  use_plotdir=file_search(use_plotdir,/mark,/full)
;  use_reportdir=file_search(use_reportdir,/mark,/full)
;  use_datadir=file_search(use_datadir,/mark,/full)
;  use_plotdir=file_search(use_plotdir,/mark,/full)
;  use_reportdir=file_search(use_reportdir,/mark,/full)

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
; read data
;==============
  
  if keyword_set(verbose) then message,'Reading data',/info
  if keyword_set(fits_in) then c1=readfits(use_datadir+use_file,c1hdr)
  if keyword_set(cube_in) then c1=cube_in
  if keyword_set(hdr_in) then c1hdr=hdr_in

  if n_elements(c1) eq 0 or $
     n_elements(c1hdr) eq 0 then begin
     message,'Problem with input data and/or header information',/info
     goto, the_end
  end

 ;==============
; initalize the results structure
;==============
  
  cstr=sfng_empty_cubeinsp_str()
  cstr.c1_file=use_file
 

;==============
; check header information
; verify that units are K, km/s, and that cubes have beam and grid information in header.
;==============
     
  if keyword_set(verbose) then message,'Checking header for data: '+use_file,/info

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
; CHANNEL MAPS
;======================

;  show all channels in cube
  start_chan=0 & end_chan=nchans-1
  
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

   produce_report:
;================
;make a PDF report
;================

   if do_report eq 1 then begin
      sfng_make_latex_elements,cstr,reportdir=use_reportdir,plotdir=use_plotdir,type='SIMPLE'
      sfng_compile_latex,reportdir=use_reportdir,plotdir=use_plotdir,/inspect,/simple
   end

   if keyword_set(verbose) then message,'Finished sfng_cube_show.pro for '+use_file,/info
   if not keyword_set(nostop) then stop

  the_end:
  return
  
end
