PRO sfng_compile_latex,cube_cmp_str,reportdir=reportdir,plotdir=plotdir $
                      ,template=template,tagname=tagname $
                      ,help=help,verbose=verbose

;+
; NAME
;  sfng_compile_latex
; PURPOSE
;  creates a .tex file describing SFNG cube comparison tests and compiles it 
; CALLING SEQUENCE
;  sfng_compile_latex, cube_cmp_str, [plotdir=plotdir,reportdir=reportdir,/help,/verbose]
; INPUTS
;  cube_cmp_str: structure containing results of cube comparison tests
; OPTIONAL INPUT:
;     plotdir = output directory for plots generated during
;               comparison. Defaults to current directory
;     reportdir = output directory for final PDF report. Defaults to
;                current directory
;     tagname = optional additional part of filename    
; ACCEPTED KEY-WORDS:
;     help = print this help
;     verbose = print extra information to screen
; OUTPUTS
;     PDF document
; COMMENTS
;
; HISTORY
;  15-11-2016: Written, AH.
;-

 IF keyword_set(help) THEN BEGIN
     doc_library,'sfng_compile_latex'
     goto,the_end
  ENDIF

;===================
; defaults
;===================

 use_plotdir = './plots/'
 use_reportdir = './report/'
 use_tagname = 'galaxy'
 use_template = './sfng_cube_compare_pdftemplate.tex'
; use_datestr=strsplit(timestamp(/off),'.',/extract)
; use_datestr=STRJOIN(STRSPLIT(use_datestr[0], /EXTRACT,':'), '-')
 use_datestr=strsplit(cgtimestamp(9),'.',/extract)
 use_datestr=STRJOIN(STRSPLIT(use_datestr[0], /EXTRACT,':'), '-')

;===================
; process user inputs
;===================
  if keyword_set(template) then use_template=template
  if keyword_set(plotdir) then use_plotdir=plotdir
  if keyword_set(reportdir) then use_reportdir=reportdir
  if keyword_set(tagname) then use_tagname=tagname

;===================
; enforce final back slash and make sure directories exist
;===================
  use_plotdir=file_search(use_plotdir,/mark,/full)
  use_reportdir=file_search(use_reportdir,/mark,/full)

  if use_plotdir eq '' or use_reportdir eq '' then $
     message,'Problem with plots/report directory?'

  use_template=file_search(use_template,/mark,/full)

  if use_template eq '' then $
       message,'Template file not found'

  
;===================
;=== copy the template latex file, adding the date
;===================
  file_in=use_template
  file_out=use_reportdir+'sfng_cube_compare_'+use_tagname+use_datestr+'.tex'
  str='cp '+file_in+' '+file_out
  spawn,str

  ;Generate temporary compilation script
  script_file='sfng_latex_scr'
  openw,unit,script_file,/get_lun
  commands=['cp '+use_plotdir+'*.png '+use_reportdir, $
         'cd '+use_reportdir, $
        'rm *.aux', $
        'pdflatex '+file_out]
  Ncoms=n_elements(commands)
  FOR i=0L,Ncoms-1 DO printf,unit,commands[i]
  close,unit
  free_lun,unit

  ;Compile the latex
  str='chmod +rwx '+script_file
  spawn,str
  str='source '+script_file
  spawn,str

  
the_end:
  return
  
END
