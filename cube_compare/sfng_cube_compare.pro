pro cube_compare

;===================
; defaults
;===================

  @sfng_constants.bat
    
  use_savefile='cubecmp_results.sav'
  use_c1file='12co21-30m-only.vcorr.LSR'
  use_c2file='m74_TP_12CO21.LSR'
  use_data_dir = './orig_data/'
  use_out_dir ='./good_data/'
  use_plot_dir = './plots/'
  use_report_dir = './report/'
  use_win = 0L
  use_basename = 'galaxy'
  use_baseline_order = [1,1] ; 0 (offset), 1 (linear) or 2 (quadratic) -- value for each cube
  verbose=1
  use_avgbox=3
  use_avgspecbox=0
  use_hithresh=5
  use_lothresh=2
  
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
  do_rebase1=1
  do_rebase2=1
  do_noise=1
  do_mask_edges=1
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
; read the data
;==============

  if keyword_set(verbose) then message,'Reading data: '+use_c1file,/info
  c1=readfits(use_data_dir+use_c1file+'.fits',c1hdr)
  if keyword_set(verbose) then message,'Reading data: '+use_c2file,/info
  c2=readfits(use_data_dir+use_c2file+'.fits',c2hdr)

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


   match_cdelt1=sxpar(c1hdr,'CDELT1',count=cdelt1_ct)
   match_cdelt3=sxpar(c1hdr,'CDELT3',count=cdelt3_ct)
   match_bmaj=sxpar(c1hdr,'BMAJ',count=bmaj_ct)
   match_pixdim=size(c1,/dim)
   nx=match_pixdim[0] & ny=match_pixdim[1] & nchans=match_pixdim[2]
   
   ccmp_str.pixscale_as=match_cdelt1*3600.
   ccmp_str.chanw_kms=match_cdelt3
   ccmp_str.angres_as=match_bmaj
   ccmp_str.dims=match_pixdim

end


;==============
; apply a blanking mask corresponding to the common field of view
;==============

if do_commonfov eq 1 then begin
   
   sfng_make_commonfov,idl_in1=c1,idl_in2=c2,hdr_in1=c1hdr,hdr_in2=c2hdr $
                       ,fits_outmask=use_basename+'_commonfov_mask.fits',/apply $
                       ,fits_out1=use_c1file+'.commFoV.fits' $
                       ,fits_out2=use_c2file+'.commFoV.fits' $
                       ,idl_out1=c1fov,idl_out2=c2fov,hdr_out1=c1hdr_out,hdr_out2=c2hdr_out $
                       ,idl_outmask=common_fov_mask, hdr_outmask=common_fov_hdr

   c1=c1fov & c1hdr=c1hdr_out
   c2=c2fov & c2hdr=c2hdr_out

   ccmp_str.cmp_fov_fits=use_basename+'_commonfov_mask.fits'
   
end

;====================
; simple rebaseline
;====================

if do_rebase1 eq 1 then begin
  
; mask regions of strong signal before estimating new baselines
; we write FITS of signal mask explicitly to avoid make_cprops_mask
; throwing an error
   
   make_cprops_mask, indata = c1 $
                     , outmask = c1sigmask $
                     ;, outfile= use_out_dir+use_basename+'_c1_basic_sigmask.fits' $
                     , hi_thresh = 8 $
                     , lo_thresh = 1.5 $
                     , hi_nchan=2 $
                     , lo_nchan=2

   c1sigmsk_hdr=c1hdr
   sxaddpar,c1sigmsk_hdr,'BUNIT','Mask'
   sxaddpar,c1sigmsk_hdr,'DATAMAX',1
   sxaddpar,c1sigmsk_hdr,'DATAMIN',0
   sxaddpar,c1sigmsk_hdr,'HISTORY','Signal mask generated by make_cprops_mask'
   writefits,use_out_dir+use_basename+'_c1_basic_sigmask.fits',c1sigmask,c1sigmsk_hdr

   sfng_rebaseline,idl_in=c1 $
                   ,hdr_in=c1hdr $
                   ,idl_mask=c1sigmask $
                   ,idl_out=c1_rebase $
                   ,hdr_out=c1_rebase_hdr $
                   ,order=use_baseline_order[0] $
                   ,fits_out=use_basename+'_c1_rebase' 

   c1=c1_rebase & c1hdr=c1_rebase_hdr 

   ccmp_str.c1_rebase_flag=do_rebase1
   ccmp_str.c1_rebase_order=use_baseline_order[0]

end

if do_rebase2 eq 1 then begin
  
; mask regions of strong signal before estimating new baselines
; we write FITS of signal mask explicitly to avoid make_cprops_mask
; throwing an error
   
   make_cprops_mask, indata = c2 $
                     , outmask = c2sigmask $
                     ;, outfile= use_out_dir+use_basename+'_c2_basic_sigmask.fits' $
                     , hi_thresh = 8 $
                     , lo_thresh = 1.5 $
                     , hi_nchan=2 $
                     , lo_nchan=2

   c2sigmask_hdr=c2hdr
   sxaddpar,c2sigmask_hdr,'BUNIT','Mask'
   sxaddpar,c2sigmask_hdr,'DATAMAX',1
   sxaddpar,c2sigmask_hdr,'DATAMIN',0
   sxaddpar,c2sigmask_hdr,'HISTORY','Signal mask generated by make_cprops_mask'
   writefits,use_out_dir+use_basename+'_c2_basic_sigmask.fits',c2sigmask,c2sigmask_hdr

   sfng_rebaseline,idl_in=c2 $
                   ,hdr_in=c2hdr $
                   ,idl_mask=c2sigmask $
                   ,idl_out=c2_rebase $
                   ,hdr_out=c2_rebase_hdr $
                   ,order=use_baseline_order[1] $
                   ,fits_out=use_basename+'_c2_rebase' 

   c2=c2_rebase & c2hdr=c2_rebase_hdr 

   ccmp_str.c2_rebase_flag=do_rebase2
   ccmp_str.c1_rebase_order=use_baseline_order[1]

end

   
;==============
; generate noise map
;==============

if do_noise eq 1 then begin

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
   writefits,use_out_dir+use_basename+'_c1noise.fits',c1noise,c1noise_hdr

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
   writefits,use_out_dir+use_basename+'_c1noise2d.fits',c1noise2d,c1noise2d_hdr

   
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
   writefits,use_out_dir+use_basename+'_c2noise.fits',c2noise,c2noise_hdr

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
   writefits,use_out_dir+use_basename+'_c2noise2d.fits',c2noise2d,c2noise2d_hdr


   ccmp_str.c1_rmsmap_fits=use_basename+'_c1noise2d.fits'
   ccmp_str.c2_rmsmap_fits=use_basename+'_c2noise2d.fits'
   ccmp_str.c1_rmscube_fits=use_basename+'_c1noise.fits'
   ccmp_str.c2_rmscube_fits=use_basename+'_c2noise.fits'

end

;==============
; enlarge FoV mask, e.g. to exclude noisy edge pixels/channels 
;==============

if do_mask_edges eq 1 then begin
   
   sfng_enlarge_mask,idl_in=c1,hdr_in=c1hdr $
                     ,idl_out=c1_newmask, hdr_out=c1hdrout $
                     ,pixel_blank=use_pixel_blank,chan_blank=use_chan_blank $
                     ,/apply $
                     ,outdir=use_out_dir $
                     ,idl_mask=common_fov_mask_exp $
                     ,fits_mask=use_basename+'_finalfovmask.fits' $
                     ,fits_out=use_basename+'_c1_finalfovmask.fits'

   sfng_enlarge_mask,idl_in=c2,hdr_in=c2hdr $
                     ,idl_out=c2_newmask, hdr_out=c2hdrout $
                     ,pixel_blank=use_pixel_blank,chan_blank=use_chan_blank $
                     ,/apply $
                     ,outdir=use_out_dir $
                     ,fits_out=use_basename+'_c2_finalfovmask.fits'
   
   c1=c1_newmask & c1hdr=c1hdrout
   c2=c2_newmask & c2hdr=c2hdrout
   common_fov_mask=common_fov_mask_exp
   
   ccmp_str.cmp_fov_fits=use_basename+'_finalfovmask.fits'

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
   writefits,use_out_dir+use_basename+'_c1_signalmask.fits',c1sigmask,c1sigmsk_hdr

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
   writefits,use_out_dir+use_basename+'_c2_signalmask.fits',c2sigmask,c2sigmsk_hdr

   joint_sigmask=(c1sigmask eq 1) and (c2sigmask eq 1)
   no_sigmask=(c1sigmask eq 0) and (c2sigmask eq 0) and (finite(c1) eq 1) and (finite(c2) eq 1)
   
   jsigmsk_hdr=c1sigmsk_hdr
   writefits,use_out_dir+use_basename+'_jointsignalmask.fits',joint_sigmask,jsigmsk_hdr
   nosigmsk_hdr=c1sigmsk_hdr
   writefits,use_out_dir+use_basename+'_nosignalmask.fits',no_sigmask,nosigmsk_hdr

   ccmp_str.c1_signalmask_fits=use_basename+'_c1_signalmask.fits'
   ccmp_str.c2_signalmask_fits=use_basename+'_c2_signalmask.fits'
   ccmp_str.joint_signalmask_fits=use_basename+'_jointsignalmask.fits'
   ccmp_str.joint_nosignalmask_fits=use_basename+'_nosignalmask.fits'
   

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
   jsigidx=where(joint_signalmask eq 1, jct)
   nosigidx=where(no_signalmask eq 1, nsct)

   ccmp_str.npix_cmp_fov=fovct
  ccmp_str.c1_npix_signalmask=c1ct
  ccmp_str.c2_npix_signalmask=c2ct
  ccmp_str.npix_jointsignalmask=jct
  ccmp_str.npix_nosignalmask=nsct

;======================
; STATISTICS RE TOTAL FLUX
;======================

  diffcube=c1-c2
   
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
     jsm_thisplane=jointsignalmask[*,*,i]
     nosm_thisplane=nosignalmask[*,*,i]
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
     if c2ct gt 0 then c1_chanflux_jsm[i]=total(c2_thisplane[c2_thisplane_jsm_idx],/nan)

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

  stop
  ccmp_str.lincorr_c1c2=0.
  ccmp_str.lincorr_c2c1=0.
  ccmp_str.logcorr_c1c2=0.
  ccmp_str.logcorr_c2c1=0.

  stop
  
  the_end:
  
end
