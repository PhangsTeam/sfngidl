FUNCTION latex_figst2figstr,fig_st,show=show

;str=latex_figst2figstr(make_latex_fig_structure(),/show)

;From figure structure to figure latex string
;%2014_01_30_Defocalisation_0_mat_64 ======================================================
;\newpage
;\begin{figure}[H]
;\centering
;\includegraphics[height=5 cm]{Figures/CNES_Tests_2014/2014_01_30_Defocalisation_0_mat_64_microscan_log_pos1.ps}
;\includegraphics[height=5 cm]{Figures/CNES_Tests_2014/2014_01_30_Defocalisation_0_mat_64_microscan_log_pos2.ps}
;\includegraphics[height=5 cm]{Figures/CNES_Tests_2014/2014_01_30_Defocalisation_0_mat_64_microscan_log_pos3.ps}
;\includegraphics[height=5 cm]{Figures/CNES_Tests_2014/2014_01_30_Defocalisation_0_mat_64_microscan_log_pos4.ps}
;\caption{\label{fig:ResDefoc_0_mat_64} Results for session
;  2014\_01\_30\_Defocalisation\_0\_mat\_64 position 1}
;\end{figure}
;\newpage

pos_str=fig_st.position ;like H,t, ....
double_str=''
IF fig_st.double_column EQ 1 THEN double_str='*'
;double_str=fig_st.double_column   ;
centering=fig_st.centering
wh_str=fig_st.dimension_type
wh_value=strtrim(fig_st.dimension_value,2)
wh_unit=fig_st.dimension_unit
caption_str=fig_st.caption
fig_label_str=fig_st.label
newpage=fig_st.newpage
orient_str=strtrim(fig_st.angle,2)

ps_file_names=*fig_st.ps_file_names

strs=[' ']
str='%comment'
strs=[strs,str]
str='\begin{figure'+double_str+'}['+pos_str+']'
strs=[strs,str]
IF centering EQ 1 THEN BEGIN
  str='\centering'
  strs=[strs,str]
ENDIF
Nfigs=n_elements(ps_file_names)
FOR i=0L,Nfigs-1 DO BEGIN
  str='\includegraphics['+wh_str+'='+wh_value+wh_unit+',angle='+orient_str+']{'+ps_file_names(i)+'}'
  strs=[strs,str]
ENDFOR
label_str='\label{'+fig_label_str+'}'
str='\caption{'+label_str+caption_str+'}'
strs=[strs,str]
str='\end{figure'+'}'
strs=[strs,str]
IF newpage EQ 1 THEN BEGIN
  str='\newpage'
  strs=[strs,str]
ENDIF

IF keyword_set(show) THEN BEGIN
  hprint,strs
ENDIF

;stop

RETURN,strs

END
