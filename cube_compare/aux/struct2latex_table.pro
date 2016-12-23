PRO struct2latex_table,st,fileout,exp_accur=exp_accur,silent=silent,formats=formats,indef_replace=indef_replace, $
                       error_st=error_st,use_all_format=use_all_format,erindex=erindex,parents=parents, $
                       replace_from=replace_from,replace_to=replace_to,nowrite=nowrite,caption=caption, label=label, $
                       replace_tagnames_only=replace_tagnames_only,small=small,tiny=tiny,landscape=landscape, units=units, $
                       forcepos=forcepos

;+
; NAME:
;       struct2latex_table
; CALLING SEQUENCE:
;       struct2latex_table,st,fileout,exp_accur=exp_accur
; PURPOSE:
;	Generates a LaTeX table from an idl structure array
; INPUTS:
;       st          =IDL structure
;       fileout     =File to be written
; OPTIONAL INPUT:
;    exp_accur      = Accuracy for floating point values
;    formats        = Formats of all columns (eg ['A','F','E'])
;    error_st       = error structure. Same shape as st
;    erindex        = column number for which errors should be given
;    replace_from   = a set of strings to be replaced
;    replace_to     = a set of strings to be replaced by
;    nwrite         = if set, no file is written
; OUTPUTS:
;    None
; PROCEDURE AND SUBROUTINE USED
;    Straightforwrd
; SIDE EFFECTS:
;    None
; MODIFICATION HISTORY:
;    written by JPB Feb. 21th 2007
;-

IF N_PARAMS(0) NE 2 THEN BEGIN
  print,"struct2latex_table,st,fileout"
  print,"Accepted Key-Words: [exp_accur=][formats=['','',...]][/silent][/indef_replace][,erindex=,error_st=][use_all_format=''][,/parents][replace_from=,replace_to=]"
  GOTO,sortie
ENDIF

struct2latex_table_sign='%This Latex table was generated automatically by struct2latex_table.pro on '+systime(0)
use_caption='Caption here'
IF keyword_set(caption) THEN use_caption=caption
use_label='tab:'
IF keyword_set(label) THEN use_label=label

use_exp_accur=3
IF keyword_set(exp_accur) THEN BEGIN
  use_exp_accur=long(exp_accur)
ENDIF

exp_tot=7+use_exp_accur

Ncol=N_tags(st)
Nel=n_elements(st.(0))
;erindex=lindgen(Ncol)

str_vec=strarr(Ncol,Nel)       ;contains data in st in latex string format
str_vec_rep=strarr(Ncol,Nel)   ;latex strings to replace
all_formats=strarr(Ncol)       ;contains format of each column
col_typ=strarr(Ncol)           ;short indicator of string format
all_strings=strarr(Nel)        ;all lines of the latex table

IF keyword_set(error_st) THEN BEGIN
  IF n_tags(error_st) NE n_tags(st) THEN BEGIN
    message,'input structure and error strcuture must have same number of columns (I know, its anoying ...)'
  ENDIF
ENDIF

;=== compute the format string
format='('
FOR i=0L,Ncol-1 DO BEGIN
  CASE (size([st.(i)]))(2) OF
    2:BEGIN    ;integer
      all_formats(i)='I5'
      col_typ(i)='I'
    END
    3:BEGIN    ;long
      all_formats(i)='I5'
      col_typ(i)='I'
    END
    4:BEGIN    ;float     
;      ff_vec(i)='F6.1'
      IF not keyword_set(formats) THEN BEGIN
        all_formats(i)='E'+strtrim(7+use_exp_accur,2)+'.'+strtrim(use_exp_accur,2)
        col_typ(i)='E'
      ENDIF ELSE BEGIN
        IF formats(i) EQ 'E' THEN BEGIN
          all_formats(i)='E'+strtrim(7+use_exp_accur,2)+'.'+strtrim(use_exp_accur,2)
          col_typ(i)='E'
        ENDIF ELSE BEGIN
          all_formats(i)='F'+strtrim(7+use_exp_accur,2)+'.'+strtrim(use_exp_accur,2)
          col_typ(i)='F'
        ENDELSE
      ENDELSE
    END
    5:BEGIN    ;Double     
      IF not keyword_set(formats) THEN BEGIN
        all_formats(i)='D'+strtrim(7+use_exp_accur,2)+'.'+strtrim(use_exp_accur,2)
        col_typ(i)='D'
      ENDIF ELSE BEGIN
        IF formats(i) EQ 'D' THEN BEGIN
          all_formats(i)='D'+strtrim(7+use_exp_accur,2)+'.'+strtrim(use_exp_accur,2)
          col_typ(i)='D'
        ENDIF ELSE BEGIN
          all_formats(i)='D'+strtrim(7+use_exp_accur,2)+'.'+strtrim(use_exp_accur,2)
          col_typ(i)='D'
        ENDELSE
      ENDELSE
    END
    7:BEGIN    ;Character
      max_len=max(strlen(st.(i)))
      all_formats(i)='A'+strtrim(max_len+1,2)
      col_typ(i)='A'
    END
  ENDCASE
  format=format+all_formats(i)+','
  IF i NE Ncol-1 THEN format=format+' " & ",'
ENDFOR
format=format+'" \\")'

;=== This is to replace the above format by the one provided by user
IF keyword_set(use_all_format) THEN BEGIN
  format=use_all_format
;  stop
  try=str_sep(strmid(format,1,strlen(format)+1),',')
  indd=lindgen(Ncol)*2
  all_formats=try(indd)
ENDIF

IF not keyword_set(silent) THEN BEGIN
  message,format,/info
ENDIF

use_st=st

;=== compute powers of 10 for numeric values
;=== and devide values to get mantisse
Ntags=n_tags(st)
p_of_10=fltarr(Nel,Ntags)+!indef
ind=where(strmid(all_formats,0,1) EQ 'E',count)
FOR j=0L,Nel-1 DO BEGIN
  FOR k=0,count-1 DO BEGIN
    p_of_10(j,ind(k))=floor(alog10(abs((st(j).(ind(k))))))
    use_st(j).(ind(k))=st(j).(ind(k))/10.^p_of_10(j,ind(k))
  ENDFOR
ENDFOR
;=== devide error values to get mantisse
IF keyword_set(error_st) THEN BEGIN
  use_error_st=error_st
  FOR j=0L,Nel-1 DO BEGIN
    FOR k=0L,count-1 DO BEGIN
      use_error_st(j).(ind(k))=error_st(j).(ind(k))/10.^p_of_10(j,ind(k))
    ENDFOR
  ENDFOR
ENDIF
;=== change all_formats accordingly to reflect mantisse only
use_all_formats=all_formats
FOR k=0,count-1 DO BEGIN
  use_all_formats(ind(k))='F'+strmid(all_formats(ind(k)),1,100)
ENDFOR

use_format='('
FOR i=0L,Ncol-1 DO BEGIN
  use_format=use_format+use_all_formats(i)+','
  IF i NE Ncol-1 THEN use_format=use_format+' " & ",'
ENDFOR
use_format=use_format+'" \\")'

;stop

;=== Make strings, using the format
FOR j=0L,Nel-1 DO BEGIN
  all_strings(j)=string(use_st(j),format=use_format)
ENDFOR

;stop
;hprint,all_strings
pst1=''
pst2=''
IF keyword_set(parents) THEN BEGIN
  pst1='('
  pst2=')'
ENDIF

;=== add errors
IF keyword_set(error_st) THEN BEGIN
  Nerindex=n_elements(erindex)
  FOR i=0L,Nel-1 DO BEGIN
    try=all_strings(i)
    pos=strpos(try,'\\')
    try=strmid(try,0,pos)
    toto=str_sep(try,'&')
    FOR j=0L,Nerindex-1 DO BEGIN
      frmt='('+use_all_formats(erindex(j))+')'
      toto(erindex(j))=strtrim(toto(erindex(j)),2)+'$\pm$'+strtrim(string(use_error_st(i).(erindex(j)),format=frmt),2)
;      toto(erindex(j))=toto(erindex(j))+'$\,10^{'+strtrim(fix(p_of_10(i,erindex(j))),2)+'}$'
    ENDFOR
    all_strings(i)=toto(0)
    FOR j=1L,Ncol-1 DO BEGIN
      all_strings(i)=all_strings(i)+' & '+toto(j)
    ENDFOR
    all_strings(i)=all_strings(i)+'\\'
  ENDFOR
ENDIF

;=== put powers of 10
FOR i=0L,Nel-1 DO BEGIN
  try=all_strings(i)
  pos=strpos(try,'\\')
  try=strmid(try,0,pos)
  toto=str_sep(try,'&')
  FOR k=0L,count-1 DO BEGIN
    toto(ind(k))=strtrim(toto(ind(k)),2)
    IF fix(p_of_10(i,ind(k))) NE 0 THEN BEGIN
      toto(ind(k))=pst1+toto(ind(k))+pst2+'$\,10^{'+strtrim(fix(p_of_10(i,ind(k))),2)+'}$'
   ENDIF
  ENDFOR
  all_strings(i)=toto(0)
  FOR j=1L,Ncol-1 DO BEGIN
      all_strings(i)=all_strings(i)+' & '+toto(j)
  ENDFOR
  all_strings(i)=all_strings(i)+'\\'
ENDFOR

;stop
;hprint,all_strings

;== replace E by 10^
;; ind_rep=where(col_typ EQ 'E',count_rep)
;; IF count_rep NE 0 THEN BEGIN
;;   FOR j=0L,Nel-1 DO BEGIN
;;     try=all_strings(j)
;;     FOR ii=0L,count_rep-1 DO BEGIN
;;       i=ind_rep(ii)
;;       st_to_replace=string((st.(i))(j),format='('+all_formats(i)+')')
;;       st_to_replace=strtrim(st_to_replace,2)
;;       st_replaced=textoidl_str_replace(st_to_replace,'E','\,10^{')
;;       st_replaced='$'+st_replaced+'}$'
;;       toto=textoidl_str_replace(try,st_to_replace,st_replaced)
;;       try=toto
;;     ENDFOR
;;     all_strings(j)=toto
;;   ENDFOR
;; ENDIF

;stop
;hprint,all_strings

;== replace undefined values
ind_rep=where(col_typ EQ 'E' OR col_typ EQ 'F',count_rep)
FOR j=0L,Nel-1 DO BEGIN
  try=all_strings(j)
  IF keyword_set(indef_replace) THEN BEGIN
    FOR ii=0L,count_rep-1 DO BEGIN
      i=ind_rep(ii)
      st_to_replace=string(!indef,format='('+all_formats(i)+')')
      st_to_replace=strtrim(st_to_replace,2)
      st_replaced='--'
      toto=textoidl_str_replace(try,st_to_replace,st_replaced)
      try=toto
    ENDFOR
  ENDIF
  all_strings(j)=try
ENDFOR

;stop

;== replace 10^{+00} by nothing
FOR j=0L,Nel-1 DO BEGIN
  all_strings(j)=textoidl_str_replace(all_strings(j),'\,10^{+00}','')
ENDFOR
;== replace 10^{+0 by 10^{+
;== replace 10^{-0 by 10^{-
FOR j=0L,Nel-1 DO BEGIN
  all_strings(j)=textoidl_str_replace(all_strings(j),'\,10^{+0','\,10^{+')
  all_strings(j)=textoidl_str_replace(all_strings(j),'\,10^{-0','\,10^{-')
ENDFOR

;== If nedeed, replace strings
IF keyword_set(replace_from) and keyword_set(replace_to) and not keyword_set(replace_tagnames_only) THEN BEGIN
  Nrep=n_elements(replace_from)
  FOR i=0L,Nrep-1 DO BEGIN
    FOR j=0L,Nel-1 DO BEGIN
      str=textoidl_str_replace(all_strings(j),replace_from(i),replace_to(i))
      all_strings(j)=str
    ENDFOR
  ENDFOR
ENDIF

;Add table header and trailer
col_form='|'
FOR i=0L,Ncol-1 DO BEGIN
  col_form=col_form+'l'
ENDFOR
col_form=col_form+'|'
tab_header=[struct2latex_table_sign]
IF keyword_set(landscape) THEN BEGIN
  tab_header=[tab_header,'\begin{landscape}']
ENDIF  

;tab_header=[tab_header,'\begin{table}']
use_beginstr='\begin{table}'
if keyword_set(forcepos) then use_beginstr='\begin{table}[H]'
tab_header=[tab_header,use_beginstr]

IF keyword_set(small) THEN BEGIN
  tab_header=[tab_header,'{\small']
ENDIF
IF keyword_set(tiny) THEN BEGIN
  tab_header=[tab_header,'{\tiny']
ENDIF
tab_header=[tab_header, $
'\caption[ ]{\label{'+use_label+'} '+use_caption+'}', $
'\begin{flushleft}', $
'\begin{tabular}{'+col_form+'}', $
'\hline', $
'\hline']
col_names=tag_names(st)
;=== Check if name contains _ symbols
FOR i=0L,Ncol-1 DO BEGIN
  col_names(i)=textoidl_str_replace(col_names(i),'_','\_')
ENDFOR
IF keyword_set(replace_from) and keyword_set(replace_to) THEN BEGIN
  Nrep=n_elements(replace_from)
  FOR i=0L,Nrep-1 DO BEGIN
    FOR j=0L,Ncol-1 DO BEGIN
      str=textoidl_str_replace(col_names(j),replace_from(i),replace_to(i))
      col_names(j)=str
    ENDFOR
  ENDFOR
ENDIF

tab_col_names=''
FOR i=0L,Ncol-1 DO BEGIN
  IF i NE Ncol-1 THEN BEGIN
    tab_col_names=tab_col_names+col_names(i)+' & '
  ENDIF ELSE BEGIN
    tab_col_names=tab_col_names+col_names(i)+' \\'
  ENDELSE
ENDFOR
tab_units=''
IF keyword_set(units) THEN BEGIN
  FOR i=0L,Ncol-1 DO BEGIN
    IF i NE Ncol-1 THEN BEGIN
      tab_units=tab_units+units(i)+' & '
    ENDIF ELSE BEGIN
      tab_units=tab_units+units(i)+' \\'
    ENDELSE
  ENDFOR
ENDIF
toto=[tab_header,tab_col_names,tab_units,'\hline']
tab_header=toto
tab_trailer=[ $
'\hline', $
'\end{tabular}', $
'\end{flushleft}', $
' ', $
'%$^\dag$ insert table footnotes there', $
' ']
IF keyword_set(small) THEN BEGIN
  tab_trailer=[tab_trailer,'}']
ENDIF
IF keyword_set(tiny) THEN BEGIN
  tab_trailer=[tab_trailer,'}']
ENDIF
tab_trailer=[tab_trailer, $
'\end{table}']
IF keyword_set(landscape) THEN BEGIN
  tab_trailer=[tab_trailer,'\end{landscape}']
ENDIF  


FOR i=0L,Nel-1 DO BEGIN
  all_strings(i)=textoidl_str_replace(all_strings(i),'_','\_')
ENDFOR

;stop

toto=[tab_header,all_strings,tab_trailer]
all_strings=toto

;write the ouptut file
IF not keyword_set(nowrite) THEN BEGIN
  Nlines=N_elements(all_strings)
  openw,unit,fileout,/get_lun
  FOR j=0L,Nlines-1 DO BEGIN
;  stop
    IF not keyword_set(silent) THEN print,all_strings(j)
    printf,unit,all_strings(j)
;  printf,unit,st(j),format=format
  ENDFOR
  close,unit
  free_lun,unit
ENDIF

;write on screen
IF not keyword_set(silent) THEN BEGIN
  Nlines=N_elements(all_strings)
  FOR j=0L,Nlines-1 DO BEGIN
    print,all_strings(j)
  ENDFOR
ENDIF

sortie:

END
