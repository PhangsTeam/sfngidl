FUNCTION read_xcat,filename,tagnames=tagnames,silent=silent

;+
; NAME:
;       read_xcat
; CALLING SEQUENCE:
;       list=read_xcat(filename)
; PURPOSE:
;       read an IPAC ASCII format table.
;       The content of the table is put the result into an IDL
;       structure.
;       The name and types of the structure tags corresponds to that
;       of the columns
; INPUTS:
;       filename = file name
; OPTIONAL INPUT:
;       silent   = If set, behaves silently.
; OUTPUTS:
;	   list = output structure.
; OPTIONAL OUTPUT:
;       tagnames = structure tagnames
; PROCEDURE AND SUBROUTINE USED
;       my_create_struct, remove_tmp_files
;	   round
; SIDE EFFECTS:
;       ?
; EXAMPLE:
;       file=!jpblib_example_dir+'xcat_example.xcat'
;       st=read_xcat(file)
;       help,st,/str
;       file_out=!jpblib_example_dir+'xcat_output.xcat'
;       write_xcat,st,file_out; MODIFICATION HISTORY:
;       written by Jean-Philippe Bernard 23-Jul-93 @ IPAC
;       Modified JPB 30-Jul-98 to include /silent and missing free_lun
;       Modified JPB 02-Dec-98 to get number of lignes right
;       Modified JPB 27/12/98 to remove random name.
;       Modified JPB 16/06/99 to disregard lines starting with '\' or
;       ';'
;       Modified JPB 26/10/03 to use my_create_struct with /nodelete
;                             otherwise, pb with new IDLv6.0 in OSX
;                             which apparently cannot resolve_routine
;                             in /tmp if /tmp/temp.pro does not exist !
;-

IF N_PARAMS(0) LT 1 THEN begin
  print,'Calling Sequence: struc=read_xcat(filename)'
  print,'Accepted K-words: tagnames='
  GOTO,sortie
ENDIF

;==  read the file to derive Nline,Ncol , etc ...
line='\                                                                    '
head_lines=['']
OPENR,unit,filename,/get_lun
WHILE strmid(line,0,1) EQ '\' OR strmid(line,0,1) EQ '|' OR strmid(line,0,1) EQ ';' OR strlen(strtrim(line,2))EQ 0 DO BEGIN
  head_lines=[head_lines,line]
  READF,unit,line
ENDWHILE
head_lines=head_lines(2:*)
all_head=head_lines
ind=where(strmid(head_lines,0,1) EQ '|',count)
head_lines=head_lines(ind)
;Analyse first header line
fields=str_sep(head_lines(0),'|') & si=size(fields) & Ncol=si(1)-2
col_larg=strlen(fields) & col_larg=col_larg(1:Ncol) & col_larg=col_larg+1
fields=strtrim(fields(1:si(1)-2),2)
;Analyse second header line
field_type=str_sep(head_lines(1),'|') & field_type=strtrim(field_type(1:si(1)-2),2)
;Analyse third header line (not used for now)
;keep counting lines to the end of file
Nline=1L & Nline_tot=1L
WHILE not eof(unit) DO BEGIN
  READF,unit,line
  first_char=strmid(line,0,1)
  IF first_char NE '\' AND first_char NE ';' THEN BEGIN
    Nline=Nline+1
    Nline_tot=Nline_tot+1
  ENDIF ELSE BEGIN
    Nline_tot=Nline_tot+1
ENDELSE
ENDWHILE
close,unit & free_lun,unit
a=strarr(Nline_tot)

IF not keyword_set(silent) THEN BEGIN
  FOR i=0,(size(all_head))(1)-1 DO BEGIN
    print,all_head(i)
ENDFOR
  print,'Number of Columns in file=  '+strtrim(Ncol,2)
  Print,'Number of lines in file= '+strtrim(nline,2)
ENDIF

type_tag=''
FOR i=0,Ncol-1 DO BEGIN
  IF field_type(i) EQ 'real' OR field_type(i) EQ 'r' THEN type_tag=type_tag+'F'
  IF field_type(i) EQ 'double' OR field_type(i) EQ 'd' THEN type_tag=type_tag+'D'
  IF field_type(i) EQ 'int'  OR field_type(i) EQ 'i' THEN type_tag=type_tag+'I'
  IF field_type(i) EQ 'char' OR field_type(i) EQ 'c' THEN type_tag=type_tag+'A'
  IF field_type(i) EQ 'long' OR field_type(i) EQ 'l' THEN type_tag=type_tag+'J'
  IF i NE Ncol-1 THEN type_tag=type_tag+','
ENDFORtagset=fields
 ;st_name='Read_Xcat'
st_name=''     ;THIS IS TO CREATE AN ANONYMOUS STRUCTURE

;stop
my_create_struct,one_ps,st_name,tagset,type_tag

IF !version.os EQ 'MacOS' THEN BEGIN
;  remove_tmp_files
    ENDIF
ps_st=replicate(one_ps,Nline)

;print,'Opening the file to read ',Nline,' sources'
OPENR,unit,filename,/get_lun
FOR i=0,(size(all_head))(1)-1 DO BEGIN
  READF,unit,line
  ENDFOR
READF,unit,a
CLOSE,unit
free_lun,unit
IF not keyword_set(silent) THEN print,'Reading completed'

i=0L
FOR ii=0L,Nline_tot-1 DO BEGIN
  deb=0
  first_char=strmid(a(ii),0,1)
  IF first_char NE ' ;' AND first_char NE '\' THEN BEGIN
    FOR j=0L,Ncol-1 DO BEGIN
      width=col_larg(j)
      truc=strtrim(strmid(a(ii),deb,width),2)
      IF truc EQ 'null' or truc EQ 'NULL' THEN BEGIN
;        stop
        truc=string(!indef)
      ENDIF
      IF strlen(truc) EQ 0 THEN truc=!indef
;      stop
      ps_st(i).(j)=truc
      deb=deb+width
  ENDFOR
    i=i+1
ENDIF
ENDFOR

IF keyword_set(tagnames) THEN tagnames=fields

return,ps_st
free_lun,unit

sortie:

END

