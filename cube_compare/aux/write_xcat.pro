PRO write_xcat,struct,filename, $
               silent=silent, $
               comments=comments, $
               nowrite=nowrite, $
               frmt_double=frmt_double, $
               frmt_char=frmt_char, $
               frmt_int=frmt_int, $
               frmt_long=frmt_long, $
               frmt_real=frmt_real, $
               wiki=wiki

;+
; NAME:
;       write_xcat
; CALLING SEQUENCE:
;       write_xcat,struct,filename
; PURPOSE:
;       write an IDL structure into an ASCII IPAC format file
; INPUTS:
;       filename = file name
;       struct   = IDL structure
; OPTIONAL INPUT:
;       None
; OUTPUTS:
;	   None
; PROCEDURE AND SUBROUTINE USED
;
; SIDE EFFECTS:
;       The file is created
; MODIFICATION HISTORY:
;       written by Jean-Philippe Bernard 16-May-97 @ IAS
; EXAMPLE:
;       file=!jpblib_example_dir+'xcat_example.xcat'
;       st=read_xcat(file)
;       help,st,/str
;       file_out=!jpblib_example_dir+'xcat_output.xcat'
;       write_xcat,st,file_out
;-

IF N_PARAMS(0) LT 1 OR N_PARAMS(0) GT 2 THEN BEGIN
  print,'Calling Sequence: write_xcat,struct,filename[,frmt_char=][,frmt_int=][,frmt_real=][,frmt_long=][,frmt_double=]'
  print,'Accepted K-words: comments=['','',...]'
  GOTO,sortie
ENDIF

wiki_line=''
use_wiki_end=''
IF keyword_set(wiki) THEN BEGIN
  wiki_line="'|',"
  use_wiki_end="|"
ENDIF

tags=tag_names(struct)
use_frmt_char='('+wiki_line+'A)'
use_frmt_int='('+wiki_line+'A)'
use_frmt_long='('+wiki_line+'A)'
use_frmt_real='('+wiki_line+'A)'
use_frmt_double='('+wiki_line+'A)'
IF keyword_set(frmt_char) THEN use_frmt_char=frmt_char
IF keyword_set(frmt_int) THEN use_frmt_int=frmt_int
IF keyword_set(frmt_long) THEN use_frmt_long=frmt_long
IF keyword_set(frmt_real) THEN use_frmt_real=frmt_real
IF keyword_set(frmt_double) THEN use_frmt_double=frmt_double
;=== write the xcat header
si=size(struct) & Nline=si(1)
;stop
n=n_struct(struct,ntags)
tyyp=intarr(Ntags)
FOR i=0,Ntags-1 DO BEGIN
  si=size([struct.(i)])
  tyyp(i)=si(2)
ENDFOR
tags_typ=strarr(Ntags)
tags_frmt=strarr(Ntags)
ind=where(tyyp EQ 7,count)
IF count NE 0 THEN BEGIN
  tags_typ(ind)='char'
  tags_frmt(ind)=use_frmt_char
ENDIF
ind=where(tyyp EQ 2,count)
IF count NE 0 THEN BEGIN
  tags_typ(ind)='int'
  tags_frmt(ind)=use_frmt_int
ENDIF
ind=where(tyyp EQ 3,count)
IF count NE 0 THEN BEGIN
  tags_typ(ind)='long'
  tags_frmt(ind)=use_frmt_long
ENDIF
ind=where(tyyp EQ 4,count)
IF count NE 0 THEN BEGIN
  tags_typ(ind)='real'
  tags_frmt(ind)=use_frmt_real
ENDIF
ind=where(tyyp EQ 5,count)
IF count NE 0 THEN BEGIN
  tags_typ(ind)='double'
  tags_frmt(ind)=use_frmt_double
;  tags_frmt(ind)='(D15.4)'
ENDIF

;Find out length of each column
len=intarr(Ntags)
FOR i=0,Ntags-1 DO BEGIN
;  str=strtrim(struct.(i),2)
  str=strtrim(string(struct.(i),format=tags_frmt(i)),2)
  strr=strtrim(tags_typ(i))
  strrr=strtrim(tags(i))
  len(i)=max([strlen(str),strlen(strr),strlen(strrr)])+1
ENDFOR
str1='' & str2='|' & str3='|' & str5='|'
FOR i=0,total(len)+Ntags-1 DO str1=str1+'='
FOR i=0L,Ntags-1 DO BEGIN
  str2=str2+tags_typ(i)
  FOR j=strlen(tags_typ(i)),len(i)-1 DO BEGIN
    str2=str2+' '
  ENDFOR
  str2=str2+'|'
  str3=str3+tags(i)
  FOR j=strlen(tags(i)),len(i)-1 DO BEGIN
    str3=str3+' '
  ENDFOR
  str3=str3+'|'
  str5=str5+'NULL'
  FOR j=strlen('NULL'),len(i)-1 DO BEGIN
    str5=str5+' '
  ENDFOR
  str5=str5+'|'
ENDFOR
str4=str1
IF not keyword_set(silent) THEN BEGIN
  print,str1
  print,str3
  print,str2
  print,str5
  print,str4
ENDIF
FOR i=0L,Nline-1 DO BEGIN
  str=''
  FOR j=0L,Ntags-1 DO BEGIN
;    strr=strtrim(struct(i).(j),1)
    strr=strtrim(string(struct(i).(j),format=tags_frmt(j)),1)
    str=str+strr
    FOR k=strlen(strr),len(j)-1 DO BEGIN
      str=str+' '
    ENDFOR
    str=str+' '
  ENDFOR
  str=str+use_wiki_end
  IF not keyword_set(silent) THEN print,str
ENDFOR
;stop

;now write the file
IF not keyword_set(nowrite) THEN BEGIN
  comment='\Written by write_xcat.pro on '+systime(0)
  GET_LUN,unit
  openw,unit,filename
  printf,unit,comment
  IF keyword_set(comments) THEN BEGIN
    Ncom=n_elements(comments)
    FOR i=0L,Ncom-1 DO BEGIN
      printf,unit,'\'+comments(i)
    ENDFOR
  ENDIF
  printf,unit,str3
  printf,unit,str2
  printf,unit,str5
  FOR i=0L,Nline-1 DO BEGIN
     str=''
     FOR j=0L,Ntags-1 DO BEGIN
;        strr=strtrim(struct(i).(j),1)
        strr=strtrim(string(struct(i).(j),format=tags_frmt(j)),1)
        str=str+strr
        FOR k=strlen(strr),len(j)-1 DO BEGIN
           str=str+' '
        ENDFOR
        str=str+' '
     ENDFOR
     str=str+use_wiki_end
     printf,unit,str
  ENDFOR
  close,unit
  free_lun,unit
ENDIF

sortie:

END

