pro run_cube_compare_example

;===================
; defaults
;===================

  @sfng_constants.bat
    
  use_savefile='ccmp_results.sav'
  use_c1file='12co21-30m-only.vcorr.LSR'
  use_c2file='m74_TP_12CO21.LSR'
  use_data_dir = './data/'
  use_out_dir ='./good_data/'
  use_plot_dir = './plots/'
  use_report_dir = './report/'
  use_win = 0L
  use_basename = 'galaxy'
  use_baseline_order = 1 ; 0, 1 or 2
  verbose=1
  
  nan=!values.f_nan
  ms2kms=1/1000.d
  kms2ms=1000.d
  use_line_frequency = restfreq_12co21/1.e9
  use_target_beam = [30.,30.,0.]
  
  c1str='CUBE1'
  c2str='CUBE2'
  
  do_check_header=1
  do_jy2k_c1=0
  do_jy2k_c2=1
  do_match=1
  do_commonfov=1
  do_rebase1=0
  do_rebase2=0
  do_signalmask=0
  do_enlarge_mask=0
  do_report=0

;===================
; process user inputs
;===================

  if keyword_set(c1file) then use_c1file=c1file
  if keyword_set(c2file) then use_c2file=c2file
  if keyword_set(data_dir) then use_data_dir=data_dir
  if keyword_set(out_dir) then use_out_dir=out_dir
  if keyword_set(plot_dir) then use_plot_dir=plot_dir
  if keyword_set(report_dir) then use_report_dir=report_dir
  if keyword_set(basename) then use_basename=basename
  if keyword_set(baseline_order) then use_baseline_order=baseline_order

;===================
; enforce final back slash and make sure directories exists
;===================
  use_data_dir=file_search(use_data_dir,/mark,/full)
  use_out_dir=file_search(use_out_dir,/mark,/full)
  use_plot_dir=file_search(use_plot_dir,/mark,/full)
  use_report_dir=file_search(use_report_dir,/mark,/full)

  if use_data_dir eq '' or use_out_dir eq '' or use_plot_dir eq '' then $
     message,'Problem with data/out/plot directories. Do they exist?'

  if do_report eq 1 and use_report_dir eq '' then $
     message,'Problem with report directory?'

;==============
; initalize the results structure
;==============
  
  ccmp_str=sfng_empty_cubecmp_str()

;==============
; read the data
;==============

  if keyword_set(verbose) then message,'Reading data: '+use_c1file,/info
  c1=readfits(use_data_dir+use_c1file+'.fits',c1hdr)
  if keyword_set(verbose) then message,'Reading data: '+use_c2file,/info
  c2=readfits(use_data_dir+use_c2file+'.fits',c2hdr)

  ccmp_str.c1_file=use_c1file
  ccmp_str.c2_file=use_c2file
  
;==============
; check header information
; verify that units are K, km/s, and that cubes have beam and grid information in header.
;==============

  if do_check_header gt 0 then begin
     
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
   
     
     if pass1 ne 1 or keyword_set(verbose) then begin
        print,'&%&%&%&%&%&%&% CUBE 1 &%&%&%&%&%&%&%'
        print, c1_comments, format='(a)'
        print,'&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%'
     end
     
     if pass2 ne 1 or keyword_set(verbose) then begin
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
        print,'Headers did not pass sfng_check_header. Check logs before .c to continue'
        stop
     end
     
     c1hdr = c1hdr_fix
     c2hdr = c2hdr_fix
     writefits,use_out_dir+use_c1file+'.fixhdr.fits',c1,c1hdr
     writefits,use_out_dir+use_c2file+'.fixhdr.fits',c2,c2hdr

  endif

;==============
; convert to K from Jy/beam
;==============

if do_jy2k_c1 gt 0 then begin

   c1K=sfng_convert_cube_jy2k(in = c1, hdr_in=c1hdr, hdr_out=new_c1hdr, factor=c1fact, freq=use_line_frequency)
  c1fact_str=strtrim(string(c1fact),2)
  c1_comments=[c1_comments,'Converted from Jy/beam to K with factor = '+c1fact_str]
  c1_comments=[c1_comments,'New units are K']
  ccmp_str.c1_comments=strjoin(c1_comments,';')
  ccmp_str.c1_jy2k_flag=1
  c1=c1K & c1hdr=new_c1hdr
  if keyword_set(verbose) then print, 'Converted Cube 1 from Jy/beam to K with factor = '+c1fact_str
  writefits,use_out_dir+use_c1file+'.K.fits',c1,c1hdr

end


if do_jy2k_c2 gt 0 then begin

   c2K=sfng_convert_cube_jy2k(in = c2, hdr_in=c2hdr, hdr_out=new_c2hdr, factor=c2fact, freq=use_line_frequency)
  c2fact_str=strtrim(string(c2fact),2)
  c2_comments=[c2_comments,'Converted from Jy/beam to K with factor = '+c2fact_str]
  c2_comments=[c2_comments,'New units are K']
  ccmp_str.c2_comments=strjoin(c2_comments,';')
  ccmp_str.c2_jy2k_flag=1
  c2=c2K & c2hdr=new_c2hdr
  if keyword_set(verbose) then print, 'Converted Cube 2 from Jy/beam to K with factor = '+c2fact_str
  writefits,use_out_dir+use_c2file+'.K.fits',c2,c2hdr

end

;==============
; match the cubes -- resolution and gridding parameters
;==============

if do_match eq 1 then begin

   
   sfng_match_cubes,idl_in1=c1,idl_in2=c2,hdr_in1=c1hdr,hdr_in2=c2hdr $
                    ,idl_out1=c1match,idl_out2=c2match,hdr_out1=c1hdr_out,hdr_out2=c2hdr_out $
                   ,xygrid_master=1,vgrid_master=2,target_beam=use_target_beam,/round $
                   ,outdir=use_out_dir,fits_out1=use_c1file+'.match.fits',fits_out2=use_c2file+'.match.fits'

   c1=c1match & c1hdr=c1hdr_out
   c2=c2match & c2hdr=c2hdr_out
   
end


;==============
; apply a blanking mask corresponding to the common field of view
;==============

if do_commonfov eq 1 then begin
   
   sfng_make_commonfov,idl_in1=c1,idl_in2=c2,hdr_in1=c1hdr,hdr_in2=c2hdr $
                       ,fits_outmask=use_basename+'_commonfov_mask.fits',/apply $
                       ,fits_out1=use_c1file+'.commFoV.fits' $
                       ,fits_out2=use_c2file+'.commFoV.fits' $
                       ,idl_out1=c1fov,idl_out2=c2fov,hdr_out1=c1hdr_out,hdr_out2=c2hdr_out 

   c1=c1fov & c1hdr=c1hdr_out
   c2=c2fov & c2hdr=c2hdr_out
   
end

;====================
; simple rebaseline
;====================

if do_rebase1 eq 1 then begin

   ; mask regions of strong signal
   make_cprops_mask, indata = c1 $
                     , outmask = c1sigmask $
                     , outfile= use_outdir+use_basename+'_c1_basic_sigmask.fits' $
                     , hi_thresh = 8 $
                     , lo_thresh = 1.5 $
                     , hi_nchan=2 $
                     , lo_nchan=2 

   sfng_rebaseline,idl_in=c1 $
                   ,hdr_in=c1hdr $
                   ,idl_mask=c1sigmask $
                   ,idl_out=c1_rebase
                   ,order=use_baseline_order $
                   ,/robust $
                   ,fits_out=use_out_dir+use_basename+'_c1_rebase.fits')

   stop
   
end


   
;  c2sigmask=sfng_signalmask(in=c2,hdr=c2hdr,fits_out=use_out_dir+use_basename+'_c2_basic_sigmask.fits')

;  c1_rebase=sfng_rebaseline(in=c1,hdr=c1hdr,mask=c1sigmask $
;                            ,order=use_baseline_order,/robust $
;                            ,fits_out=use_out_dir+use_basename+'_c1_rebase.fits')

;  c2_rebase=sfng_rebaseline(in=c2,hdr=c2hdr,mask=c2sigmask $
;                            ,order=use_baseline_order,/robust $
;                            ,fits_out=use_out_dir+use_basename+'_c2_rebase.fits')


;==============
; generate noise map
;==============

;  c1_noise=make_noise_cube()
;  c2_noise=make_noise_cube()
  
;==============
; enlarge FoV mask, e.g. to exclude noisy edge pixels/channels 
;==============

;  c1_newmask=sfng_enlarge_mask(in1=c1,hdr1=c1hdr,/apply,fits_out=use_out_dir+use_basename+'_c1_fovmask.fits')
;  c2_newmask=sfng_enlarge_mask(in1=c2,hdr1=c2hdr,/apply,fits_out=use_out_dir+use_basename+'_c2_fovmask.fits')
;  c1_noise_newmask=sfng_enlarge_mask(in1=c1_noise,hdr1=c1hdr,/apply,fits_out=use_out_dir+use_basename+'_c1_rms_fovmask.fits')
;  c2_noise_newmask=sfng_enlarge_mask(in1=c2_noise,hdr1=c2hdr,/apply,fits_out=use_out_dir+use_basename+'_c2_rms_fovmask.fits')
  
;==============
; final significant emission masks (after rebaselining, and comparison FoV determined)
;==============

;  c1sigmask=sfng_signalmask(in=c1,hdr=c1hdr,fits_out=use_out_dir+use_basename+'_c1_sigmask.fits')
;  c2sigmask=sfng_signalmask(in=c2,hdr=c2hdr,fits_out=use_out_dir+use_basename+'_c2_sigmask.fits')

;======================
; start of comparison tests
;======================

  ccmp_str=sfng_empty_cubecmp_str()

  ccmp_str.c1_file=use_c1file
  ccmp_str.c2_file=use_c2file
  ccmp_str.c1_rebase_flag=0.
  ccmp_str.c2_rebase_flag=0.
  ccmp_str.cmp_fov_fits=0.
  ccmp_str.c1_signalmask_fits=0.
  ccmp_str.c2_signalmask_fits=0.
  ccmp_str.joint_signalmask_fits=0.
  ccmp_str.joint_nosignalmask_fits=0.
  ccmp_str.pixscale_as=0.
  ccmp_str.chanw_kms=0.
  ccmp_str.angres_as=0.
  ccmp_str.c1_rmsmap=0.
  ccmp_str.c2_rmsmap=0.

  ccmp_str.c1_totflux=0.
  ccmp_str.c2_totflux=0.
  ccmp_str.c1_totflux_signalmask=0.
  ccmp_str.c2_totflux_signalmask=0.
  ccmp_str.c1_totflux_nosignalmask=0.
  ccmp_str.c2_totflux_nosignalmask=0.
  ccmp_str.c1_totflux_jointsignalmask=0.
  ccmp_str.c2_totflux_jointsignalmask=0.
  ccmp_str.c1_totflux_jointnosignalmask=0.
  ccmp_str.c2_totflux_jointnosignalmask=0.
  ccmp_str.totfluxdiff=0.
  ccmp_str.totfluxdiff_jointsignalmask=0.
  ccmp_str.totfluxdiff_jointnosignalmask=0.

  ccmp_str.c1_npix_signalmask=0.
  ccmp_str.c2_npix_signalmask=0.
  ccmp_str.c1_npix_nosignalmask=0.
  ccmp_str.c2_npix_nosignalmask=0.
  ccmp_str.npix_jointsignalmask=0.
  ccmp_str.npix_jointnosignalmask=0.

   
  ccmp_str.c1_fluxperchan=0.
  ccmp_str.c2_fluxperchan=0.
  ccmp_str.c1_rmsperchan=0.
  ccmp_str.c2_rmsperchan=0.
  ccmp_str.c1_rmsperchan_jointnosignalmask=0.
  ccmp_str.c2_rmsperchan_jointnosignalmask=0.
  ccmp_str.fluxdiffperchan=0.

  ccmp_str.c1_noisestats=0.
  ccmp_str.c2_noisestats=0.
  ccmp_str.diffcube_stats=0.

  ccmp_str.lincorr_c1c2=0.
  ccmp_str.lincorr_c2c1=0.
  ccmp_str.logcorr_c1c2=0.
  ccmp_str.logcorr_c2c1=0.

  stop
  
  the_end:
  
end
