PRO sfng_make_latex_elements,cube_cmp_str $
                             ,figures_only=figures_only $
                             ,tables_only=tables_only $
                             ,help=help,verbose=verbose
                             

; NAME:
;     sfng_make_latex_elements
; CALLING SEQUENCE:
;     sfng_make_latex_elements,cube_cmp_str,[[/figures],[/tables],[/verbose],[/help])
; PURPOSE:
;     creates the inputs (tables/figures) that are included in the PDF
;     summary document
; INPUTS:
;     cube_cmp_str = cube comparison results structure
; OPTIONAL INPUT:
;
; ACCEPTED KEY-WORDS:
;     figures_only = only make the .tex files needed to include figures
;     tables_only = only make the .tex files needed to include tables
;     help = print this help
;     verbose = print extra information to screen
; EXAMPLES
;     sfng_make_latex_elements,cube_cmp_str
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
  
;================
; Process user inputs
;================

  IF keyword_set(reportdir) then use_reportdir=reportdir
  IF keyword_set(plotdir) then use_plotdir=plotdir
  
  IF keyword_set(figures_only) THEN goto,figures_only
  IF keyword_set(tables_only) THEN goto,tables_only
;  IF keyword_set(logs_only) THEN goto,logs_only

;================
; Strip special characters from filenames to stop latex throwing an error
;================

  use_c1str=repstr(cube_cmp_str.c1_file,'_','\_')
  use_c2str=repstr(cube_cmp_str.c2_file,'_','\_')
  
;    logs_only:
;================
;== Make the input .tex file containing the processing logs
;================
;  if keyword_set(logs_only) then goto, the_end
  
  tables_only:
;================
;== Make the input cube table
;================

  one_st={file:'',pixscale:nan,chanw:nan,bmaj:nan,bmin:nan,bpa:nan,bunit:'',nx:nan,ny:nan,nv:nan}
  st=replicate(one_st,2)

  st[0].file=cube_cmp_str.c1_file
  st[1].file=cube_cmp_str.c2_file
  st[0].pixscale=cube_cmp_str.c1_pixscale
  st[1].pixscale=cube_cmp_str.c2_pixscale
  st[0].chanw=cube_cmp_str.c1_chanw
  st[1].chanw=cube_cmp_str.c2_chanw
  st[0].bmaj=cube_cmp_str.c1_beam[0]*3600.
  st[0].bmin=cube_cmp_str.c1_beam[1]*3600.
  st[0].bpa=cube_cmp_str.c1_beam[2]
  st[1].bmaj=cube_cmp_str.c2_beam[0]*3600.
  st[1].bmin=cube_cmp_str.c2_beam[1]*3600.
  st[1].bpa=cube_cmp_str.c2_beam[2]
  st[0].nx=cube_cmp_str.c1_dims[0]
  st[0].ny=cube_cmp_str.c1_dims[1]
  st[0].nv=cube_cmp_str.c1_dims[2]
  st[1].nx=cube_cmp_str.c2_dims[0]
  st[1].ny=cube_cmp_str.c2_dims[1]
  st[1].nv=cube_cmp_str.c2_dims[2]
  st[0].bunit=cube_cmp_str.c1_bunit
  st[1].bunit=cube_cmp_str.c2_bunit

  fileout=use_reportdir+'inputcube_table.tex'
  caption='Basic parameters of input cubes.'
  label='tab:input_cubes'

  frmt='(A40, " & ",F10.3, " & ",F10.2, " & ",F10.2, " & ",F10.2, " & ",F10.2, " & ", A10, " & ", I5.5, " & ",I5.5, " & ",I5.5," \\")'
  units=['','[as]','[km/s]', '[as]','[as]','[deg]' , '', '' , '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=1

  struct2latex_table,st,fileout,use_all_format=frmt, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the matched cube table
;================

  one_st={angres:0,chanwidth:0,pixscale:0,nx:0,ny:0,nv:0}
  st=replicate(one_st,1)
  
  st[0].angres=cube_cmp_str.angres_as
  st[0].chanwidth=cube_cmp_str.chanw_kms
  st[0].pixscale=cube_cmp_str.pixscale_as
  st[0].nx=cube_cmp_str.dims[0]
  st[0].ny=cube_cmp_str.dims[1]
  st[0].nv=cube_cmp_str.dims[2]

  fileout=use_reportdir+'matchcube_table.tex'
  caption='Parameters of matched cubes.'
  label='tab:matched_cubes'

  frmt='(F10.3, " & ",F10.2, " & ",F10.2, " & ",I5.5, " & ",I5.5, " & ",I5.5," \\")'
  units=['\relax [as]','[km/s]', '[as]', '' , '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=1

  struct2latex_table,st,fileout,use_all_format=frmt, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

 
;================
;== Make the flux statistics table
;================

  one_st={file:'',total_flux:0,total_flux_jointmask:0}
  st=replicate(one_st,3)
  label='tab:total_flux'

  st[0].file=cube_cmp_str.c1_file
  st[1].file=cube_cmp_str.c2_file
  st[2].file='Difference Cube'
  st[0].total_flux=cube_cmp_str.c1_totflux
  st[1].total_flux=cube_cmp_str.c2_totflux
  st[2].total_flux=cube_cmp_str.TOTFLUXDIFF
  st[0].total_flux_jointmask=cube_cmp_str.c1_totflux_jointsignalmask
  st[1].total_flux_jointmask=cube_cmp_str.c2_totflux_jointsignalmask
  st[2].total_flux_jointmask=cube_cmp_str.TOTFLUXDIFF_jointsignalmask

  fileout=use_reportdir+'flux_table.tex'
  caption='Total Flux Statistics'

  frmt='(A40, " & ",E10.3, " & ",E10.2, " \\")'
  units=['', '[K.km/s.pix]' , '[K.km/s.pix]'] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=1

  struct2latex_table,st,fileout,use_all_format=frmt, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the noise statistics table
;================

  one_st={file:'',rms_cube:0,rms_nosignal:0}
  st=replicate(one_st,3)

  st[0].file=cube_cmp_str.c1_file
  st[1].file=cube_cmp_str.c2_file
  st[2].file='Difference Cube'

  st[0].rms_cube=cube_cmp_str.c1_noisestats.mean
  st[1].rms_cube=cube_cmp_str.c2_noisestats.mean
  st[2].rms_cube=cube_cmp_str.diffcube_stats.mean
  st[0].rms_nosignal=cube_cmp_str.c1_noisestats_nosignal.rms
  st[1].rms_nosignal=cube_cmp_str.c2_noisestats_nosignal.rms
  st[2].rms_nosignal=cube_cmp_str.diffcube_stats_nosignal.mean
  
  fileout=use_reportdir+'noise_table.tex'
  caption='Noise Statistics'
  label='tab:noise_statistics'

  frmt='(A40, " & ",E10.3, " & ",E10.2, " \\")'
  units=['', '[K]' , '[K]'] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=1

  struct2latex_table,st,fileout,use_all_format=frmt, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

;================
;== Make the correlation metrics table
;================

  one_st={rpx:0, rank:0, slope_yonx:0, slope_xony:0}
  st=replicate(one_st,1)

  st[0].rpx=cube_cmp_str.rpx_c1c2
  st[0].rank=cube_cmp_str.rank_c1c2[0]
  st[0].slope_yonx=cube_cmp_str.lincorr_c1c2[0]
  st[0].slope_xony=cube_cmp_str.lincorr_c2c1[0]
  
  fileout=use_reportdir+'correlation_table.tex'
  caption='Correlation Metrics'
  label='tab:correlation_metrics'

  frmt='(F10.2, " & ",F10.2, " & ",F10.2, " & ", F10.2,  " \\")'
  units=['', '' , '', ''] ; order is order of tags in the structure, not replace arrays
  tiny=0 & small=1 & landscape=1

  struct2latex_table,st,fileout,use_all_format=frmt, $
                     /silent,caption=caption,label=label,units=units,tiny=tiny,small=small,landscape=landscape

  message,'Wrote '+fileout,/info

  if keyword_set(tables_only) then goto, the_end


figures_only:
;================
;== Make the flux per channel figure
;================
 
  newpage=0
  dimension_type='width'
  dimension_value=14            ;cm
  dimension_unit='cm'
  angle=0.
  centering=1
  
  ; flux per channel
  file='flux_per_channel.png'
  caption='Flux per channel: Matched cubes'
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
  caption='Flux per channel in joint signal mask'
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

  ; diffcube -- inside joint signal mask
  file='diffcube_flux_per_channel_jointsignalmask.png'
  caption='Flux per channel in difference cube inside joint signal mask'
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
 
  newpage=0
  dimension_type='width'
  dimension_value=16            ;cm
  dimension_unit='cm'
  angle=0.

  ; rms per channel
  file='rms_per_channel.png'
  caption='RMS per channel: Matched cubes'
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
  caption='RMS per channel, measured within joint no signal mask'
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
;== Make the noise histogram figures
;================
    
  ; Noise histogram -- cube 1
  file='c1_noisehisto.png'
  caption='Histogram of pixel values in signal-free channels: '+use_c1str
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
  caption='Histogram of pixel values in signal-free channels: '+use_c2str
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
  caption='Histogram of pixel values in signal-free channels: Difference Cube'
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

 
  
  the_end:
  return
  
END
