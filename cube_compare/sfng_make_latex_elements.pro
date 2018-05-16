PRO sfng_make_latex_elements,cube_str $
                             ,reportdir=reportdir $
                             ,plotdir=plotdir $
                             ,figures_only=figures_only $
                             ,tables_only=tables_only $
                             ,help=help,verbose=verbose, inspect=inspect $
                             ,show=show $
                             ,type=type

; NAME:
;     sfng_make_latex_elements
; CALLING SEQUENCE:
;     sfng_make_latex_elements,cube_str,[[/figures],[/tables],[/verbose],[/help],[/inspect])
; PURPOSE:
;     creates the inputs (tables/figures) that are included in a PDF
;     summary document for sfng_cube_compare or sfng_cube_inspect
; INPUTS:
;     cube_str = results structure from sfng_cube_compare or sfng_cube_inspect
; OPTIONAL INPUT:
;
; ACCEPTED KEY-WORDS:
;     figures_only = only make the .tex files needed to include figures
;     tables_only = only make the .tex files needed to include tables
;     logs_only = only make the .tex files needed to include the
;                 processing logs
;     help = print this help
;     verbose = print extra information to screen
; EXAMPLES
;     sfng_make_latex_elements,cube_str
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     
; COMMONS:
;     
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;    written 25-11-2016 by AH
; COMMENTS AND TO DO LIST:
;    Minimally tested!
;    Error trapping not implemented.
;-

  
  IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_make_latex_elements'
     goto,the_end
  ENDIF

;================
; Defaults
;================
  
  nan=!values.f_nan
  use_reportdir='./report/'
  use_plotdir='./plots/'
  use_type='SHOW'
  
;================
; Process user inputs
;================

  IF keyword_set(reportdir) then use_reportdir=reportdir
  IF keyword_set(plotdir) then use_plotdir=plotdir
  IF keyword_set(type) then use_type=strupcase(type)
  

  CASE use_type of
     'COMPARE': begin

;================
; Strip special characters from filenames to stop latex throwing an error
;================

  use_c1str=repstr(cube_str.c1_file,'_','\_')
  use_c2str=repstr(cube_str.c2_file,'_','\_')

;================
; COMPARISON LATEX
;================

  IF keyword_set(figures_only) THEN goto,compare_figures_only
  IF keyword_set(tables_only) THEN goto,compare_tables_only
  IF keyword_set(logs_only) THEN goto,compare_logs_only
  
  compare_logs_only:
;================
;== Make the input .tex file containing the processing logs
;================

  file_out=use_reportdir+'processing_logs.tex'
  openw,unit,file_out,/get_lun
  hdr_lines=['%This Latex text was generated automatically by sfng_make_latex_elements.pro on'+systime(), '%-----',' ']
  c1_startstr='\noindent Processing log for '+use_c1str
  c1_logstr=repstr('\noindent '+cube_str.c1_comments,'_','\_')
  c1_logstr_exp=[' ',strsplit(c1_logstr,';',/extract)+'\\',' ','%-----','%-----']

  c2_startstr='\noindent  Processing log for '+use_c2str
  c2_logstr=repstr('\noindent '+cube_str.c2_comments,'_','\_')
  c2_logstr_exp=[' ',strsplit(c2_logstr,';',/extract)+'\\',' ','%-----','%-----']

  proclog_lines=[hdr_lines,c1_startstr,c1_logstr_exp,c2_startstr,c2_logstr_exp]

  Nlines=n_elements(proclog_lines)
  FOR i=0L,Nlines-1 DO printf,unit,proclog_lines[i]
  close,unit
  free_lun,unit

  if keyword_set(logs_only) then goto, the_end
  
compare_tables_only:
;================
;== Make the input cube table
;================

  one_st={file:'',pixscale:nan,chanw:nan,bmaj:nan,bmin:nan,bpa:nan,bunit:'',casa:'',nx:0,ny:0,nv:0}
  st=replicate(one_st,2)

  st[0].file=cube_str.c1_file
  st[1].file=cube_str.c2_file
  st[0].pixscale=cube_str.c1_pixscale
  st[1].pixscale=cube_str.c2_pixscale
  st[0].chanw=cube_str.c1_chanw
  st[1].chanw=cube_str.c2_chanw
  st[0].bmaj=cube_str.c1_beam[0]*3600.
  st[0].bmin=cube_str.c1_beam[1]*3600.
  st[0].bpa=cube_str.c1_beam[2]
  st[1].bmaj=cube_str.c2_beam[0]*3600.
  st[1].bmin=cube_str.c2_beam[1]*3600.
  st[1].bpa=cube_str.c2_beam[2]
  st[0].nx=cube_str.c1_dims[0]
  st[0].ny=cube_str.c1_dims[1]
  st[0].nv=cube_str.c1_dims[2]
  st[1].nx=cube_str.c2_dims[0]
  st[1].ny=cube_str.c2_dims[1]
  st[1].nv=cube_str.c2_dims[2]
  st[0].bunit=cube_str.c1_bunit
  st[1].bunit=cube_str.c2_bunit
  st[0].casa=cube_str.c1_casa
  st[1].casa=cube_str.c2_casa
  
  fileout=use_reportdir+'inputcube_table.tex'
  caption='Basic parameters of input cubes.'
  label='tab:input_cubes'

  frmt='(A40, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ", A10, " & ", A20, " & ",I0, " & ",I0," & ", I0," \\")'
  units=['','[as]','[km/s]', '[as]','[as]','[deg]' , '', '' , '', '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=1 & small=0 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt, long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the matched cube table
;================

  one_st={angres:nan,chanwidth:nan,pixscale:nan,nx:0,ny:0,nv:0}
  st=replicate(one_st,1)
  
  st[0].angres=cube_str.angres_as
  st[0].chanwidth=cube_str.chanw_kms
  st[0].pixscale=cube_str.pixscale_as
  st[0].nx=cube_str.dims[0]
  st[0].ny=cube_str.dims[1]
  st[0].nv=cube_str.dims[2]

  fileout=use_reportdir+'matchcube_table.tex'
  caption='Parameters of matched cubes.'
  label='tab:matched_cubes'

  frmt='(F0.2, " & ",F0.2, " & ",F0.2, " & ",I0, " & ",I0, " & ",I0," \\")'
  units=['\relax [as]','[km/s]', '[as]', '' , '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt, long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the emission masks table
;================


  one_st={npix_tot:nan,npix_fov:nan,npix_c1msk:nan,npix_c2msk:nan,npix_jointmsk:nan}
  st=replicate(one_st,1)
  label='tab:emission_masks'

   st[0].npix_tot=cube_str.npix_tot
   st[0].npix_fov=cube_str.npix_cmp_fov
   st[0].npix_c1msk=cube_str.c1_npix_signalmask
   st[0].npix_c2msk=cube_str.c2_npix_signalmask
   st[0].npix_jointmsk=cube_str.npix_jointsignalmask

  fileout=use_reportdir+'emission_masks_table.tex'
  caption='Pixels in Emission Masks'
  label='tab:emission_masks'

  frmt='( I0, " & ", I0, " & ", I0, " & ", I0, " & ", I0, " \\")'
  units=['', '', '', '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt,long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the flux statistics table
;================

  one_st={file:'',total_flux:nan,peak:nan,total_flux_jointmask:nan,peak_jointmask:nan}
  st=replicate(one_st,3)
  label='tab:total_flux'

  st[0].file=cube_str.c1_file
  st[1].file=cube_str.c2_file
  st[2].file='Difference Cube'
  st[0].total_flux=cube_str.c1_totflux
  st[1].total_flux=cube_str.c2_totflux
  st[2].total_flux=cube_str.TOTFLUXDIFF
  st[0].peak=cube_str.c1_peak
  st[1].peak=cube_str.c2_peak
  st[2].peak=cube_str.diffabspeak
  st[0].total_flux_jointmask=cube_str.c1_totflux_jointsignalmask
  st[1].total_flux_jointmask=cube_str.c2_totflux_jointsignalmask
  st[2].total_flux_jointmask=cube_str.TOTFLUXDIFF_jointsignalmask
  st[0].peak_jointmask=cube_str.c1_peak_jointsignalmask
  st[1].peak_jointmask=cube_str.c2_peak_jointsignalmask
  st[2].peak_jointmask=cube_str.diffabspeak_jointsignalmask

  fileout=use_reportdir+'flux_table.tex'
  caption='Total Flux Statistics'

  frmt='(A40, " & ",E0.3, " & ",F0.2, " & ",E0.3, " & ",F0.2, " \\")'
  units=['', '[K.km/s.pix]' , '[K]', '[K.km/s.pix]' ,'[K]'] ; order is order of tags in the structure, not replace arrays
  tiny=1 & small=0 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt, long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the noise statistics table
;================

  one_st={file:'',rms_cube:nan,rms_nosignal:nan}
  st=replicate(one_st,3)

  st[0].file=cube_str.c1_file
  st[1].file=cube_str.c2_file
  st[2].file='Difference Cube'

  st[0].rms_cube=cube_str.c1_noisestats.mean
  st[1].rms_cube=cube_str.c2_noisestats.mean
  st[2].rms_cube=cube_str.diffcube_stats.mean
  st[0].rms_nosignal=cube_str.c1_noisestats_nosignal.rms
  st[1].rms_nosignal=cube_str.c2_noisestats_nosignal.rms
  st[2].rms_nosignal=cube_str.diffcube_stats_nosignal.mean
  
  fileout=use_reportdir+'noise_table.tex'
  caption='Noise Statistics'
  label='tab:noise_statistics'

  frmt='(A40, " & ",E0.3, " & ",E0.3, " \\")'
  units=['', '[K]' , '[K]'] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=0

  struct2latex_table,st,fileout,use_all_format=frmt,  long=long,/force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the correlation metrics table
;================

  one_st={rpx:nan, rank:nan, slope_yonx:nan, slope_xony:nan}
  st=replicate(one_st,1)

  st[0].rpx=cube_str.rpx_c1c2
  st[0].rank=cube_str.rank_c1c2[0]
  st[0].slope_yonx=cube_str.lincorr_c1c2[0]
  st[0].slope_xony=cube_str.lincorr_c2c1[0]
  
  fileout=use_reportdir+'correlation_table.tex'
  caption='Correlation Metrics'
  label='tab:correlation_metrics'

  frmt='(F0.2, " & ",F0.2, " & ",F0.2, " & ", F0.2,  " \\")'
  units=['', '' , '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=0

  struct2latex_table,st,fileout,use_all_format=frmt,  long=long,/force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

  
;================
;== Make the correlation metrics table
;================

  one_st={file:'',levels_perc:'',levels_abs:'',median_fidelities:''}
  st=replicate(one_st,2)

  st[0].file=cube_str.c1_file
  st[1].file=cube_str.c2_file
  st[0].levels_perc='['+strjoin(sigfig(cube_str.fidel_levs,2),',')+']'
  st[1].levels_perc='['+strjoin(sigfig(cube_str.fidel_levs,2),',')+']'
  st[0].levels_abs='['+strjoin(sigfig(cube_str.c1_fidel_levs,4),',')+']'
  st[1].levels_abs='['+strjoin(sigfig(cube_str.c2_fidel_levs,4),',')+']'
  st[0].median_fidelities='['+strjoin(sigfig(cube_str.c1_fidel_stats,4),',')+']'
  st[1].median_fidelities='['+strjoin(sigfig(cube_str.c2_fidel_stats,4),',')+']'

  fileout=use_reportdir+'fidelities_table.tex'
  caption='Fidelity Statistics'
  label='tab:fidelity_statistics'

  frmt='(A40, " & ", A30, " & ", A30, " & ", A30,  " \\")'
  units=['', '[\% of peak]' , '[K]', ''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt, long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info


  if keyword_set(tables_only) then goto, the_end
 

compare_figures_only:

;================
;== Make the channel map figures (multi-panel)
;================

; settings for 'channel-map' type figures
  
  newpage=0
  dimension_type='width'
  dimension_value=3.           ;cm
  dimension_unit='cm'
  angle=0.
  centering=0


; cube1 chan maps
  
  caption='Cube 1: Individual Channels of Matched Cube'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/c1_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'cube1_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:cube1_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  if keyword_set(chanmaps_only) then goto, the_end

; cube2 chan maps
  
  caption='Cube 2: Individual Channels of Matched Cube'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/c2_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'cube2_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:cube2_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  
; diffcube chan maps
  
  caption='Difference Cube: Individual Channels of Matched Cube'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/diffcube_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'diffcube_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:diffcube_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info



; emission_mask chan maps
  
  caption='Joint Emission Mask: Individual Channels'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/joint*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'emissionmask_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:jsm_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info




  
  ; powerspectra
  
  caption='Powerspectra: Individual Channels of Matched Cubes'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/powerspec_*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'powerspec_figs.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:powerspec_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info


  
;================
;== Make the flux per channel figure
;================
 

; settings for 'spectra' type figures
  
  newpage=0
  dimension_type='width'
  dimension_value=16            ;cm
  dimension_unit='cm'
  angle=0.
  centering=0
  
  ; flux per channel
  file='flux_per_channel.png'
  caption='Flux per channel: matched cubes'
  label='fig:flux_per_chan'
  tex_file_name=use_reportdir+'flux_perchannel_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info
  
  ; flux per channel in joint signal mask
  file='flux_per_channel_jointsignalmask.png'
  caption='Flux per channel: matched cubes in region where significant emission identified in both cubes'
  label='fig:flux_per_chan_jsm'
  tex_file_name=use_reportdir+'flux_perchannel_jsm_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  
; diffcube
  file='diffcube_flux_per_channel.png'
  caption='Flux per channel in difference cube'
  label='fig:diffcube_flux_per_chan'
  tex_file_name=use_reportdir+'diffcube_flux_perchannel_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  ; relative flux in diffcube
  file='diffcube_relflux_per_channel.png'
  caption='Relative Flux per channel in difference cube'
  label='fig:diffcube_relflux_per_chan'
  tex_file_name=use_reportdir+'diffcube_relflux_perchannel_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  ; diffcube -- inside joint signal mask
  file='diffcube_flux_per_channel_jointsignalmask.png'
  caption='Flux per channel in difference cube inside the region where signal is identified in both cubes'
  label='fig:diffcube_flux_per_chan_jsm'
  tex_file_name=use_reportdir+'diffcube_flux_perchannel_jsm_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info


;================
;== Make the rms per channel figures
;================
 
  ; rms per channel
  file='rms_per_channel.png'
  caption='RMS per channel: matched cubes, all pixels'
  label='fig:rms_per_chan'
  tex_file_name=use_reportdir+'rms_perchannel_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  
  ; rms per channel in joint no signal mask
  file='rms_per_channel_nosignalmask.png'
  caption='RMS per channel, measured where there is no signal identified in either cube'
  label='fig:rms_per_chan_nosm'
  tex_file_name=use_reportdir+'rms_perchannel_nosm_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info


  
  ; Fidelity per channel in joint signal mask
  file='fidelity_perchannel_jsm.png'
  caption='Median fidelity value per channel in region where emission is identified in both cubes.' 
  label='fig:fidelity_perchannel_jsm'
  tex_file_name=use_reportdir+'fidelity_perchannel_jsm_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info


;================
;== Make the noise histogram figures
;================

; settings for 'square' figures -- noise histos, correlation plots
  
  newpage=0
  dimension_type='width'
  dimension_value=12            ;cm
  dimension_unit='cm'
  angle=0.
  centering=0

  
  ; Noise histogram -- cube 1
  file='c1_noisehisto.png'
  caption='Histogram of pixel values in joint signal-free region (i.e. signal free in both cubes): '+use_c1str
  label='fig:rms_per_chan_nosm'
  tex_file_name=use_reportdir+'c1_noisehisto_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info


  

    
  ; Noise histogram -- cube 2
  file='c2_noisehisto.png'
  caption='Histogram of pixel values in joint signal-free region: '+use_c2str
  label='fig:rms_per_chan_nosm'
  tex_file_name=use_reportdir+'c2_noisehisto_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

    ; Noise histogram -- diffcube
  file='diffcube_noisehisto.png'
  caption='Histogram of pixel values in joint signal-free region: Difference Cube'
  label='fig:rms_per_chan_nosm'
  tex_file_name=use_reportdir+'diffcube_noisehisto_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

;;================
;== Make the fidelity figures
;;================

; Fidelity CDF -- Cube 1
  file='fidelity_cube1_cdf.png'
  caption='Fidelity values in region where emission is identified in both cubes : '+use_c1str+'. Vertical lines represent the median fidelity values for pixels above the thresholds indicated (see legend)'
  label='fig:fidelity_cube1_cdf'
  tex_file_name=use_reportdir+'fidelity_cube1_cdf_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  ; Fidelity CDF -- Cube 2
  file='fidelity_cube2_cdf.png'
  caption='Fidelity values in region where emission is identified in both cubes : '+use_c2str+'. Vertical lines represent the median fidelity values for pixels above the thresholds indicated (see legend)'
  label='fig:fidelity_cube2_cdf'
  tex_file_name=use_reportdir+'fidelity_cube2_cdf_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info
 
;================
;== Make the correlation figures
;================

  file='lincorr_c1c2.png'
  caption='Linear correlation plot between cube 1 (x-axis, '+use_c1str+') and cube 2 (y-axis, '+use_c2str+')'
  label='fig:lincorr_c1c2'
  tex_file_name=use_reportdir+'lincorr_c1c2_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  file='lindens_c1c2.png'
  caption='Linear density plot between cube 1 (x-axis, '+use_c1str+') and cube 2 (y-axis, '+use_c2str+')'
  label='fig:lindens_c1c2'
  tex_file_name=use_reportdir+'lindens_c1c2_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  file='logcorr_c1c2.png'
  caption='Log correlation plot between cube 1 (x-axis, '+use_c1str+') and cube 2 (y-axis, '+use_c2str+')'
  label='fig:logcorr_c1c2'
  tex_file_name=use_reportdir+'logcorr_c1c2_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  file='logdens_c1c2.png'
  caption='Log density plot between cube 1 (x-axis, '+use_c1str+') and cube 2 (y-axis, '+use_c2str+')'
  label='fig:logdens_c1c2'
  tex_file_name=use_reportdir+'logdens_c1c2_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info


  
  file='lincorr_c2c1.png'
  caption='Linear correlation plot between cube 2 (x-axis, '+use_c2str+') and cube 1 (y-axis, '+use_c1str+')'
  label='fig:lincorr_c2c1'
  tex_file_name=use_reportdir+'lincorr_c2c1_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  file='lindens_c2c1.png'
  caption='Linear density plot between cube 2 (x-axis, '+use_c2str+') and cube 1 (y-axis, '+use_c1str+')'
  label='fig:lindens_c2c1'
  tex_file_name=use_reportdir+'lindens_c2c1_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  file='logcorr_c2c1.png'
  caption='Log correlation plot between cube 2 (x-axis, '+use_c2str+') and cube 1 (y-axis, '+use_c1str+')'
  label='fig:logcorr_c2c1'
  tex_file_name=use_reportdir+'logcorr_c2c1_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  file='logdens_c2c1.png'
  caption='Log density plot between cube 2 (x-axis, '+use_c2str+') and cube 1 (y-axis, '+use_c1str+')'
  label='fig:logdens_c2c1'
  tex_file_name=use_reportdir+'logdens_c2c1_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  goto, the_end
  
end
     'INSPECT': begin

;================
; Strip special characters from filenames to stop latex throwing an error
;================

  use_c1str=repstr(cube_str.c1_file,'_','\_')

;================
; INSPECT LATEX
;================

   IF keyword_set(figures_only) THEN goto,inspect_figures_only
  IF keyword_set(tables_only) THEN goto,inspect_tables_only
  IF keyword_set(logs_only) THEN goto,inspect_logs_only
 
    inspect_logs_only:
;================
;== Make the input .tex file containing the processing logs
;================

  file_out=use_reportdir+'processing_logs.tex'
  openw,unit,file_out,/get_lun
  hdr_lines=['%This Latex text was generated automatically by sfng_make_latex_elements_inspect.pro on'+systime(), '%-----',' ']
  c1_startstr='\noindent Processing log for '+use_c1str
  c1_logstr=repstr('\noindent '+cube_str.c1_comments,'_','\_')
  c1_logstr_exp=[' ',strsplit(c1_logstr,';',/extract)+'\\',' ','%-----','%-----']

  proclog_lines=[hdr_lines,c1_startstr,c1_logstr_exp]

  Nlines=n_elements(proclog_lines)
  FOR i=0L,Nlines-1 DO printf,unit,proclog_lines[i]
  close,unit
  free_lun,unit

  if keyword_set(logs_only) then goto, the_end
  
  inspect_tables_only:
;================
;== Make the input cube table
;================

  one_st={file:'',pixscale:nan,chanw:nan,bmaj:nan,bmin:nan,bpa:nan,bunit:'',casa:'',nx:0,ny:0,nv:0}
  st=replicate(one_st,1)

  st[0].file=cube_str.c1_file
  st[0].pixscale=cube_str.c1_pixscale
  st[0].chanw=cube_str.c1_chanw
  st[0].bmaj=cube_str.c1_beam[0]*3600.
  st[0].bmin=cube_str.c1_beam[1]*3600.
  st[0].bpa=cube_str.c1_beam[2]
  st[0].nx=cube_str.c1_dims[0]
  st[0].ny=cube_str.c1_dims[1]
  st[0].nv=cube_str.c1_dims[2]
  st[0].bunit=cube_str.c1_bunit
  st[0].bunit=cube_str.c1_casa

  fileout=use_reportdir+'inputcube_table.tex'
  caption='Basic parameters of input cube.'
  label='tab:input_cube'

  frmt='(A40, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ", A10, " & ", A20, " & ",I0, " & ",I0," & ", I0," \\")'
  units=['','[as]','[km/s]', '[as]','[as]','[deg]' , '', '' , '', '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=1 & small=0 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt, long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the final cube table
;================

  one_st={angres:nan,chanwidth:nan,pixscale:nan,nx:0,ny:0,nv:0}
  st=replicate(one_st,1)
  
  st[0].angres=cube_str.angres_as
  st[0].chanwidth=cube_str.chanw_kms
  st[0].pixscale=cube_str.pixscale_as
  st[0].nx=cube_str.dims[0]
  st[0].ny=cube_str.dims[1]
  st[0].nv=cube_str.dims[2]

  fileout=use_reportdir+'finalcube_table.tex'
  caption='Parameters of final cube.'
  label='tab:final_cube'

  frmt='(F0.2, " & ",F0.2, " & ",F0.2, " & ",I0, " & ",I0, " & ",I0," \\")'
  units=['\relax [as]','[km/s]', '[as]', '' , '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=0

  struct2latex_table,st,fileout,use_all_format=frmt, long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the emission masks table
;================


  one_st={npix_tot:nan,npix_fov:nan,npix_c1msk:nan}
  st=replicate(one_st,1)
  label='tab:emission_mask'

   st[0].npix_tot=cube_str.npix_tot
   st[0].npix_fov=cube_str.npix_cmp_fov
   st[0].npix_c1msk=cube_str.c1_npix_signalmask

  fileout=use_reportdir+'emission_mask_table.tex'
  caption='Pixels in Emission Mask'
  label='tab:emission_masks'

  frmt='( I0, " & ", I0, " & ", I0,  " \\")'
  units=['', '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt,  long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the flux statistics table
;================

  one_st={file:'',total_flux:nan,peak:nan,total_flux_jointmask:nan}
  st=replicate(one_st,1)
  label='tab:total_flux'

  st[0].file=cube_str.c1_file
  st[0].total_flux=cube_str.c1_totflux
  st[0].peak=cube_str.c1_peak
  st[0].total_flux_jointmask=cube_str.c1_totflux_signalmask

  fileout=use_reportdir+'flux_table.tex'
  caption='Total Flux Statistics'

  frmt='(A40, " & ",E0.3, " & ",F0.2, " & ",E0.3,  " \\")'
  units=['', '[K.km/s.pix]' , '[K]', '[K.km/s.pix]'] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt,  long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the noise statistics table
;================

  one_st={file:'',rms_cube:nan,rms_nosignal:nan}
  st=replicate(one_st,1)

  st[0].file=cube_str.c1_file

  st[0].rms_cube=cube_str.c1_noisestats.mean
  st[0].rms_nosignal=cube_str.c1_noisestats_nosignal.rms
  
  fileout=use_reportdir+'noise_table.tex'
  caption='Noise Statistics'
  label='tab:noise_statistics'

  frmt='(A40, " & ",E0.3, " & ",E0.3, " \\")'
  units=['', '[K]' , '[K]'] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=0

  struct2latex_table,st,fileout,use_all_format=frmt,  long=long,/force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

  if keyword_set(tables_only) then goto, the_end
 

inspect_figures_only:

;================
;== Make the channel map figures (multi-panel)
;================

; settings for 'channel-map' type figures
  
  newpage=0
  dimension_type='width'
  dimension_value=3.           ;cm
  dimension_unit='cm'
  angle=0.
  centering=0

; emission_mask chan maps
  
  caption='Emission Mask: Individual Channels'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/signalmask_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'emissionmask_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:jsm_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info


; cube1 chan maps
  
  caption='Individual Channels of Final Cube'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/c1_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'cube1_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:cube1_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info




; model chan maps
  
  caption='Individual Channels of Model Cube'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/mdl1_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'model1_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:mdl1_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info



; residual chan maps
  
  caption='Individual Channels of Residual Cube'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/res1_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'residual1_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:res1_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info



; clean support chan maps
  
  caption='Individual Channels of Clean Support Cube'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/cln1_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'clean1_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:cln1_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info




;================
;== Make the flux per channel figure
;================
 

; settings for 'spectra' type figures
  
  newpage=0
  dimension_type='width'
  dimension_value=16            ;cm
  dimension_unit='cm'
  angle=0.
  centering=0
  
  ; flux per channel
  file='flux_per_channel.png'
  caption='Flux per channel: final cube'
  label='fig:flux_per_chan'
  tex_file_name=use_reportdir+'flux_perchannel_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info
  
 ; flux per channel in  signal mask
  file='flux_per_channel_signalmask.png'
  caption='Flux per channel in region where significant emission identified'
  label='fig:flux_per_chan_sm'
  tex_file_name=use_reportdir+'flux_perchannel_sm_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info
  
;================
;== Make the rms per channel figures
;================
 
  ; rms per channel
  file='rms_per_channel.png'
  caption='RMS per channel: final cube, all pixels'
  label='fig:rms_per_chan'
  tex_file_name=use_reportdir+'rms_perchannel_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

  
  ; rms per channel in no signal mask
  file='rms_per_channel_nosignalmask.png'
  caption='RMS per channel, measured where no signal identified'
  label='fig:rms_per_chan_nosm'
  tex_file_name=use_reportdir+'rms_perchannel_nosm_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info




;================
;== Make the noise histogram figure
;================

; settings for 'square' figures -- noise histos, correlation plots
  
  newpage=0
  dimension_type='width'
  dimension_value=12            ;cm
  dimension_unit='cm'
  angle=0.
  centering=0

  
  ; Noise histogram -- cube 1
  file='c1_noisehisto.png'
  caption='Histogram of pixel values in signal-free region: '+use_c1str
  label='fig:rms_per_chan_nosm'
  tex_file_name=use_reportdir+'c1_noisehisto_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info


  

end
     'SHOW' : begin


;================
; Strip special characters from filenames to stop latex throwing an error
;================

  use_c1str=repstr(cube_str.c1_file,'_','\_')

;================
; SHOW LATEX
;================

  ;================
;== Make the input files table
;================

  one_st={type:'',file:''}
  st=replicate(one_st,4)

  st[0].type='DATA'
  st[1].type='NOISE'
  st[2].type='MASK'
  st[3].type='RESIDUALS'
  st[0].file=cube_str.c1_file
  st[1].file=cube_str.n1_file
  st[2].file=cube_str.m1_file
  st[3].file=cube_str.r1_file

  fileout=use_reportdir+'inputfiles_table.tex'
  caption='List of Input Files'
  label='tab:files'

  frmt='(A40, " & ", A40," \\")'
  units=['',''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=0 & long=0

  struct2latex_table,st,fileout,use_all_format=frmt, long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the input cube table
;================

  one_st={file:'',pixscale:nan,chanw:nan,bmaj:nan,bmin:nan,bpa:nan,bunit:'',casa:'',nx:0,ny:0,nv:0}
  st=replicate(one_st,1)

  st[0].file=cube_str.c1_file
  st[0].pixscale=cube_str.c1_pixscale
  st[0].chanw=cube_str.c1_chanw
  st[0].bmaj=cube_str.c1_beam[0]*3600.
  st[0].bmin=cube_str.c1_beam[1]*3600.
  st[0].bpa=cube_str.c1_beam[2]
  st[0].nx=cube_str.c1_dims[0]
  st[0].ny=cube_str.c1_dims[1]
  st[0].nv=cube_str.c1_dims[2]
  st[0].bunit=cube_str.c1_bunit
  st[0].casa=cube_str.c1_casa

  fileout=use_reportdir+'inputcube_table.tex'
  caption='Basic parameters of input cube.'
  label='tab:input_cube'

  frmt='(A40, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ", A10, " & ", A20, " & ",I0, " & ",I0," & ", I0," \\")'
;  frmt='(F0.2, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ",F0.2, " & ", A10, " & ", A20, " & ",I0, " & ",I0," & ", I0," \\")'
  units=['','[as]','[km/s]', '[as]','[as]','[deg]' , '', '' , '', '', ''] ; order is order of tags in the structure, not replace arrays
;  units=[' [as]','[km/s]','[as]','[as]','[deg]' , '', '' , '', '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=1 & small=0 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt, long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info



;================
;== Make the flux statistics table
;================

  one_st={galaxy:'',totflux:nan,peak:nan,totflux_inmask:nan,peak_inmask:nan,totflux_outmask:nan,peak_outmask:nan}
  st=replicate(one_st,1)
  label='tab:flux'

  st[0].galaxy=cube_str.galaxy
  st[0].totflux=cube_str.c1_totflux
  st[0].peak=cube_str.c1_peak
  st[0].totflux_inmask=cube_str.c1_totflux_signalmask
  st[0].totflux_outmask=cube_str.c1_totflux_nosignalmask
  st[0].peak_inmask=cube_str.c1_peak_signalmask
  st[0].peak_outmask=cube_str.c1_peak_nosignalmask

  fileout=use_reportdir+'flux_table.tex'
  caption='Total Flux Statistics'

  frmt='(A10, " & ",E0.3, " & ",F0.2, " & ",E0.3, " & ",F0.2,  " & ",E0.3, " & ",F0.2, " \\")'
  units=['', '[K.km/s.pix]' , '[K]', '[K.km/s.pix]' ,'[K]','[K.km/s.pix]' , '[K]'] ; order is order of tags in the structure, not replace arrays
  tiny=1 & small=0 & landscape=0 & long=1

  struct2latex_table,st,fileout,use_all_format=frmt, long=long, /force, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;; ;================
;; ;== Make the noise statistics table
;; ;================

;;   one_st={file:'',rms_cube:nan,rms_nosignal:nan}
;;   st=replicate(one_st,3)

;;   st[0].file=cube_str.c1_file
;;   st[1].file=cube_str.c2_file
;;   st[2].file='Difference Cube'

;;   st[0].rms_cube=cube_str.c1_noisestats.mean
;;   st[1].rms_cube=cube_str.c2_noisestats.mean
;;   st[2].rms_cube=cube_str.diffcube_stats.mean
;;   st[0].rms_nosignal=cube_str.c1_noisestats_nosignal.rms
;;   st[1].rms_nosignal=cube_str.c2_noisestats_nosignal.rms
;;   st[2].rms_nosignal=cube_str.diffcube_stats_nosignal.mean
  
;;   fileout=use_reportdir+'noise_table.tex'
;;   caption='Noise Statistics'
;;   label='tab:noise_statistics'

;;   frmt='(A40, " & ",E0.3, " & ",E0.3, " \\")'
;;   units=['', '[K]' , '[K]'] ; order is order of tags in the structure, not replace arrays
;;   tiny=0 & small=1 & landscape=0 & long=0

;;   struct2latex_table,st,fileout,use_all_format=frmt,  long=long,/force, $
;;                      /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

;;   message,'Wrote '+fileout,/info

  
;================
;== Make the channel map figures (multi-panel)
;================

; settings for 'channel-map' type figures
  
  newpage=0
  dimension_type='width'
  dimension_value=3.           ;cm
  dimension_unit='cm'
  angle=0.
  centering=0

; cube1 chan maps
  
  caption='Individual channels of data cube, deconvolution mask overplotted (black contours).'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/c1_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'cube1_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:cube1_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info



; residuals chan maps
  
  caption='Individual channels of residuals cube, deconvolution mask overplotted (black contours).'
  counter=1
  allfiles=file_basename(file_search(use_plotdir+"/r1_chan*png"))
  nfiles=n_elements(allfiles)
  nfigs=ceil(nfiles/16.) ; we want 4x4 panels in each figure
  start_idx_i=0 & end_idx_i=15

  tex_file_name=use_reportdir+'resids_chanmaps_fig.tex'
  openw,unit,tex_file_name,/get_lun

  for k=0,nfigs-1 do begin
     if counter eq 2 then caption=caption+' (cont.)'
     label='fig:resids_chanmaps_'+strtrim(string(fix(counter)),2)
     start_idx=start_idx_i+k*16
     end_idx=end_idx_i+k*16
     if end_idx ge nfiles then end_idx=nfiles-1
     use_files=allfiles[start_idx:end_idx]
     fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=use_files,_extra=_extra)
     lst=latex_figst2figstr(fig_st)
  
     FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
     printf,unit,' '
     counter=counter+1
  end
  
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info



;================
;== Make the noise histogram figures
;================

; settings for 'square' figures -- noise histos, correlation plots
  
  newpage=0
  dimension_type='width'
  dimension_value=12            ;cm
  dimension_unit='cm'
  angle=0.
  centering=0

  
  ; Noise histogram -- cube 1
  file='noisehisto.png'
  caption='Histogram of pixel values in the entire noise cube (black), inner quarter of FoV of noise cube (blue).'
  label='fig:noisehisto'
  tex_file_name=use_reportdir+'noisehisto_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info

    
  ; Residuals histogram
  file='residshisto.png'
  caption='Histogram of pixel values in entire residuals cube (black), inner quarter of FoV of residuals cube (blue), and signal-free region of data cube (green).'
  label='fig:residshisto'
  tex_file_name=use_reportdir+'residshisto_fig.tex'
  fig_st=make_latex_fig_structure(position=position,double_column=double_column,centering=centering, $
                                    dimension_type=dimension_type,dimension_value=dimension_value,dimension_unit=dimension_unit, $
                                    label=label,caption=caption,newpage=newpage,ps_file_names=file,_extra=_extra)
  lst=latex_figst2figstr(fig_st,show=show)
  openw,unit,tex_file_name,/get_lun
  FOR i=0L,n_elements(lst)-1 DO printf,unit,lst(i)
  close,unit
  free_lun,unit
  message,'Wrote '+tex_file_name,/info


  
;================
;  USER ERROR: Unknown report type requested
;================

end
     else : begin
        message,'Unknown report type requested?',/info
        stop
        end
  endcase
  

  the_end:
  return
  
END
