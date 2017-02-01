PRO PRINT_LOGFILE, logfile_type, task=task
;+ 
; NAME: PRINT_LOGFILE
; PURPOSE:
;   give user access to a currently open logfile (print, browse, edit, ...)
; CATEGORY: I-1-b
; CALLING SEQUENCE: 
;   PRINT_LOGFILE, logfile_type, task=task
; INPUTS:
;   logfile_type -- string : logfile to be read (error, master, session or user)
; OPTIONAL INPUT PARAMETERS:
;   none
; KEYED INPUTS: 
;   task         -- string : command to be applyed to the logfile
;                   (default is VMS "type /page")
; OUTPUTS: 
;   none
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE:
;   ICE>  PRINT_LOGFILE, 'master'
;   MASTER LOGFILE : MASTER_LOGFILE_NAME_940929
;   29-Sep-1994 09:21:46.00 creation by user VIVARES
;
; ALGORITHM:
;   close the logfile
;   spwan task on the logfile
;   reopen the logfile
; DEPENDENCIES:
;   a logfile of type logfile_type should be open
; COMMON BLOCKS: 
;     SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL
;     LOGFILE_BLOCK, MASTER_LOGFILE_LUN,
;                    SESSION_LOGFILE_LUN,
;                    USER_LOGFILE_LUN,
;                    ERROR_LOGFILE_LUN
; SIDE EFFECTS:
;    use SPAWN with command task logfile
; RESTRICTIONS:
;    untested under UNIX environment
; CALLED PROCEDURES AND FUNCTIONS:
;    IDL SPAWN with task 
; MODIFICATION HISTORY: 
;    22-Dec-1993  FV written with template_gen from ICCRED print_logfile
;    04-Jan-1994  FV test and update
;    22-Aug-1994  FV correct when no file open for logfile_type.
;    30-Sep-1994  FV V1.0 for configuration control
;     6-MAR-1995  FV correct quit feature
;-
 
;-----------------------------------------------------------
; common blocks 
;-----------------------------------------------------------
 
; environment parameters 
 COMMON SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL     
 
 COMMON LOGFILE_BLOCK, MASTER_LOGFILE_LUN, $
                       SESSION_LOGFILE_LUN,$
                       USER_LOGFILE_LUN,   $
                       ERROR_LOGFILE_LUN   
 
;-----------------------------------------------------------
; on error conditions
;-----------------------------------------------------------
 ON_ERROR,  ERROR_CURRENT
 
 ON_IOERROR, IOERROR
 GOTO, START
 IOERROR:
       IF n_elements(name) LE 0 THEN name = ''
       STATUS(0) = ['VMS IO ERROR with file :'+strtrim(name,2), 'E']
       PRINT, STATUS(2), ' - ', STATUS(1), ' - ', STATUS(0)
       PRINT, STRMESSAGE(!ERR)
       GOTO, CLOSING
 START:
 
;-----------------------------------------------------------
; initialization
;-----------------------------------------------------------
 
 ROUTINE_NAME = 'PRINT_LOGFILE'
 VERSION = '1.0'
 CATEGORY = 'I-1-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['logfile_type', 'task']
  s_log_t = CONV_STRING(logfile_type)
  s_task = CONV_STRING(task)
 CALLING_VALUES = [s_log_t, s_task]
 
;-----------------------------------------------------------
; parameters check
;-----------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'PRINT_LOGFILE, logfile_type, task'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

; check logfile type

 logt = strmid(STRTRIM(STRLOWCASE(logfile_type),2),0,4)

 IF ((logt NE 'erro') AND (logt NE 'user')) AND (((logt NE 'mast') AND $
     (logt NE 'sess')) AND (logt NE 'test')) THEN BEGIN
   PRINT, 'possible LOGFILE_TYPE are currently: ', $
           'error, user, tests, master or session'
   PRINT, 'invalid type : ', LOGFILE_TYPE
   STATUS(0) = ['INVALID TYPE', 'E']
   GOTO, CLOSING
 ENDIF          
 
;-----------------------------------------------------------
; function body
;-----------------------------------------------------------

 CASE logt OF
   'erro' : logfile_lun = error_logfile_lun
   'test' : logfile_lun = user_logfile_lun 
   'user' : logfile_lun = user_logfile_lun 
   'mast' : logfile_lun = master_logfile_lun
   'sess' : logfile_lun = session_logfile_lun
   ELSE :
 ENDCASE

; if required logfile is not a file (lun=-2,-1 or 0) then printing is not
; feasible

 IF logfile_lun LE 0 THEN BEGIN
   PRINT, logfile_type, " logfile is not currently printable"
   STATUS(0) = ['INVALID logfile ('+strtrim(logfile_lun,2)+')', 'E']
   GOTO, CLOSING
 ENDIF          

; extract logfile name with normaly its full pathname 
 fs=fstat(logfile_lun)

 IF fs.open EQ 0 THEN BEGIN
   PRINT, " No ", logfile_type, " logfile currently active"
   STATUS(0) = ['No ' + logfile_type + ' logfile', 'E']
   GOTO, CLOSING
 ENDIF
 
 name=fs.name
 IF !version.os EQ 'vms' THEN CLOSE, logfile_lun

 IF KEYWORD_SET(task) THEN $
    calling = strcompress(task, /remove_all) + ' ' + name $
 ELSE calling = 'type '+ name 

 SPAWN, calling, result, count=cpt

; pour l'instant, on envoie le tout a l'ecran 
; Z! pour des sessions non vraiement interactives, il faudrait en fait
; une widget qui supporterait directement l'appel

 i = 0
 IF NOT(KEYWORD_SET(task)) THEN WHILE i LT cpt DO BEGIN
    PRINT, result(i)
    i = i + 1
    v = 'G'
    IF (i mod 20) EQ 0 THEN READ, 'return to continue, Q for quit', v
    IF strupcase(strtrim(v,2)) EQ 'Q' THEN i = cpt
 ENDWHILE

; open again the logfile with same lun

 IF !version.os EQ 'vms' THEN OPENW, logfile_lun, name , /append
 
;-----------------------------------------------------------
; closing
;-----------------------------------------------------------
 
 CLOSING:
  ACTUAL_VALUES=['',''] 
  RECORD_LOGFILE, STATUS, CALLING_VALUES, VAR_NAMES, ACTUAL_VALUES
  RETURN
 
 END
