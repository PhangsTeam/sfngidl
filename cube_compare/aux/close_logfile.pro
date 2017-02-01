PRO CLOSE_LOGFILE, logfile_type
;+ 
; NAME: CLOSE_LOGFILE
; PURPOSE:
;   close an active logfile. Messages sent to this file after closing are 
;   either sent to the screen (error logfile) or else discarded (for other 
;   logfiles)
;   On closing, file name is printed to the screen.
; CATEGORY: I-1-b
; CALLING SEQUENCE: 
;   CLOSE_LOGFILE, logfile_type
; INPUTS: 
;   logfile_type   -- string with logfile type tests or user, error,
;                     master, session
; OPTIONAL INPUT PARAMETERS:
;   none
; KEYED INPUTS:
;   none
; OUTPUTS:
;   none
; OPTIONAL OUTPUT PARAMETERS:
;   none
; EXAMPLE:
;   ICE> close_logfile, 'error'
;   error logfile is now inactive
;   File ERRO_CFDPLOG_940929_VIVARES.LF is closed
; ALGORITHM:
;   straightforward 
; COMMON BLOCKS: 
;     SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL    
;     LOGFILE_BLOCK, MASTER_LOGFILE_LUN, 
;                    SESSION_LOGFILE_LUN,
;                    USER_LOGFILE_LUN,   
;                    ERROR_LOGFILE_LUN   
;
; SIDE EFFECTS:
;   close current logfile of given type (if effectively in a file)
; RESTRICTIONS:
;   none, if the file is already closed (or non existing) a warning is
;   printed instead
; CALLED PROCEDURES AND FUNCTIONS:
;   none 
; MODIFICATION HISTORY: 
;    4-Jan-1994  written with template_gen          FV IAS
;   30-Sep-1994  V. 1.0 for configuration control   FV IAS
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
       STATUS(0) = ['VMS IO ERROR', 'E']
       PRINTF, current_logfile_lun, STATUS(2), ' - ', $
               STATUS(1), ' - ', STATUS(0)
       PRINTF, current_logfile_lun, STRMESSAGE(!ERR)
       GOTO, CLOSING
 START:
 
;-----------------------------------------------------------
; initialization
;-----------------------------------------------------------
 
 ROUTINE_NAME = 'CLOSE_LOGFILE'
 VERSION = '0.1'
 CATEGORY = 'I-1-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['logfile_type']
  s_logfile_type = CONV_STRING(logfile_type)
 CALLING_VALUES = [s_logfile_type]
 current_logfile_lun=-1
 
;-----------------------------------------------------------
; parameters check
;-----------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: CLOSE_LOGFILE, logfile_type'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

; check logfile type

 logt = STRTRIM(STRLOWCASE(logfile_type),2)

 IF ((logt NE 'error') AND (logt NE 'user')) AND (((logt NE 'master') AND $
     (logt NE 'session')) AND (logt NE 'tests')) THEN BEGIN
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
   'error'   : logfile_lun = error_logfile_lun
   'tests'   : logfile_lun = user_logfile_lun
   'user'    : logfile_lun = user_logfile_lun
   'master'  : logfile_lun = master_logfile_lun
   'session' : logfile_lun = session_logfile_lun
   ELSE : GOTO, CLOSING
 ENDCASE
                                
; if required logfile is not a file (lun=-2,-1 or 0) then closing is not
; feasible

 IF logfile_lun LE 0 THEN BEGIN
  PRINT, logfile_type, " logfile cannot be closed"
   STATUS(0) = ['INVALID TYPE', 'E']
   GOTO, CLOSING
 ENDIF

; if required logfile is not open nothing to do
 sstat=fstat(logfile_lun)
 IF sstat.open EQ 0 THEN GOTO, CLOSING

; find logfile name
 POINT_LUN, logfile_lun, 0
 line=string(bytarr(20))
 READF, logfile_lun, line

 line_len = strlen (line)
 name_pos = strpos(line, ':') + 1
 name = strcompress(strmid(line, name_pos, line_len), /remove_all)

 IF (logfile_lun LE 128) AND (logfile_lun GE 100) THEN FREE_LUN, logfile_lun $
 ELSE CLOSE, logfile_lun

 IF N_ELEMENTS(session_logfile_lun) EQ 0 THEN session_logfile_lun=-1   

; signal closing to session_logfile or to screen  
 PRINT, logfile_type, ' logfile is now inactive'
 PRINT, 'File ', name, '.LF is closed' 

; set logfile_lun to 0
 CASE logt OF
   'error'   : error_logfile_lun=-1
   'tests'   : user_logfile_lun=-1
   'user'    : user_logfile_lun=-1
   'master'  : master_logfile_lun=-1
   'session' : session_logfile_lun=-1
   ELSE : GOTO, CLOSING
 ENDCASE
                                
 
;-----------------------------------------------------------
; closing
;-----------------------------------------------------------
 
 CLOSING:
  ACTUAL_VALUES = [CONV_STRING(name)]
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN $
     RECORD_LOGFILE, STATUS, CALLING_VALUES, VAR_NAMES, ACTUAL_VALUES
   
  RETURN
 
 END
