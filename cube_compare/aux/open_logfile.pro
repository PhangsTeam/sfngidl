PRO OPEN_LOGFILE, logfile_name, logfile_type, dir=dir, new=new, ok=ok
;+ 
; NAME: OPEN_LOGFILE
; PURPOSE:
;          open an existing file as logfile of type logfile_type.
;          if a logfile is aleardy active, close it first.
;          If no file of name logfile_name exists create it.
; CATEGORY: I-1-b
; CALLING SEQUENCE: 
;    OPEN_LOGFILE, logfile_name, logfile_type, dir=dir
; INPUTS:
;    logfile_name -- string : file name without extension
; OPTIONAL INPUT PARAMETERS:                       
;    logfile_type -- string : type of the logfile (default is master logfile)
; KEYED INPUTS: 
;    dir          -- string : directory where to find the logfile (default is
;                    current directory) 
; OUTPUTS: 
;   none
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; KEYED OUTPUTS:
;   new           -- 0 if no new logfile open, else 1
; EXAMPLE:
;   ICE> OPEN_LOGFILE, 'ERRO_LOGFILE', 'error', dir='logfile_dir'
;   error logfile open : ERRO_LOGFILE.LF
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;     SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL    
;     LOGFILE_BLOCK, MASTER_LOGFILE_LUN, 
;                    SESSION_LOGFILE_LUN,
;                    USER_LOGFILE_LUN,   
;                    ERROR_LOGFILE_LUN 
; SIDE EFFECTS: 
;   open logfile with name logfile_name
; RESTRICTIONS:
;   do not works if a logfile of type logfile_type is currently active
; CALLED PROCEDURES AND FUNCTIONS:
;   CHECK_FILE
; MODIFICATION HISTORY: 
;          28-Jan-1994 written               F.V. IAS
;          15-Jul-1994 add file creation     F.V. IAS
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
       IF n_elements(ok) ne 0 THEN BEGIN
          ok = 0 & new = 0 & GOTO, CLOSING
       END

       PRINT, STATUS(2), ' - ', STATUS(1), ' - ', STATUS(0)
       PRINT, STRMESSAGE(!ERR)
       GOTO, CLOSING
 START:
 
;-----------------------------------------------------------
; initialization
;-----------------------------------------------------------
 
 ROUTINE_NAME = 'OPEN_LOGFILE'
 VERSION = '1.0'
 CATEGORY = 'I-1-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['logfile_name', 'logfile_type', 'dir']
  s_log_n = CONV_STRING(logfile_name)  
  s_log_t = CONV_STRING(logfile_type)  
  s_dir = CONV_STRING(dir)
 CALLING_VALUES = [s_log_n, s_log_t, s_dir]
 
;-----------------------------------------------------------
; parameters check
;-----------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'OPEN_LOGFILE, logfile_name, logfile_type, dir=dir, new=new'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

; check logfile type

 IF N_ELEMENTS(logfile_type) EQ 0 THEN logfile_type='MASTER'
 logt = STRMID(STRTRIM(STRLOWCASE(logfile_type),2),0,4)

 IF ((logt NE 'erro') AND (logt NE 'user')) AND (((logt NE 'mast') AND $
     (logt NE 'sess')) AND (logt NE 'test')) THEN BEGIN
   PRINT, 'possible LOGFILE_TYPE are currently: ', $
           'error, user, tests, master or session'
   PRINT, 'invalid type : ', LOGFILE_TYPE
   STATUS(0) = ['INVALID TYPE:'+logfile_type, 'E']
   GOTO, CLOSING
 ENDIF          
 
;-----------------------------------------------------------
; function body
;-----------------------------------------------------------

; test if logfile_type logfile is already active, in that case it cannot be
; open with new value

 lstat= fstat(1)

 open=0
 logfile_lun = 0
 CASE logt OF
   'erro' : IF n_elements(error_logfile_lun) NE 0 THEN BEGIN
               logfile_lun = error_logfile_lun  
               lstat=fstat(logfile_lun)
               open = lstat.open
             ENDIF  
   'test' : IF n_elements(user_logfile_lun) NE 0 THEN BEGIN
               logfile_lun = user_logfile_lun  
               lstat=fstat(logfile_lun)
               open = lstat.open
            ENDIF
   'user' : IF n_elements(user_logfile_lun) NE 0 THEN BEGIN
               logfile_lun = user_logfile_lun  
               lstat=fstat(logfile_lun)
               open = lstat.open
            ENDIF
   'mast' : IF n_elements(master_logfile_lun) NE 0 THEN BEGIN
               logfile_lun = master_logfile_lun  
               lstat=fstat(logfile_lun)
               open = lstat.open
            ENDIF 
   'sess' : IF n_elements(session_logfile_lun) NE 0 THEN BEGIN
               logfile_lun = session_logfile_lun  
               lstat=fstat(logfile_lun)
               open = lstat.open
            ENDIF 
   ELSE : GOTO, CLOSING
 ENDCASE


; check logfile_name file effectively exists

 cpt=CHECK_FILE(logfile_name, 'lf', dir, pathnames=fullname)

 lng =strlen(lstat.name)
 IF open and lstat.name ne strmid(fullname(0), 0, lng) THEN new = 1 ELSE new=0

 IF (open EQ 1) and (logfile_lun GT 0) THEN free_lun, logfile_lun

 get_lun, logfile_lun 

; else create it and initialize
 IF cpt(0) EQ 0 THEN BEGIN
    init_value0 = STRUPCASE(logfile_type) + ' LOGFILE : ' + logfile_name
    init_value1 = !stime + ' creation by user '
    prt_tbp = "new " + LOGFILE_TYPE + " logfile created : " + logfile_name + $
              '.lf'
    IF n_elements(dir) eq 1 THEN BEGIN
       IF !version.os eq 'vms' THEN BEGIN
          pos = strpos(dir, ":")
          IF pos GE 0 THEN adddir = dir ELSE adddir = dir + ':'
        ENDIF ELSE adddir = dir + '/'
    ENDIF
    fullname = adddir + logfile_name + '.lf'
 ENDIF ELSE BEGIN
    init_value0 = '----  '
    init_value1 = !stime + ' new session by user ' 
    prt_tbp = LOGFILE_TYPE + " logfile open : " + logfile_name + '.lf'
 ENDELSE

; open required logfile in extension and initialize it

 OPENW, logfile_lun, fullname(0), /append
 ident = GETENV('USER')
 PRINTF, logfile_lun, INIT_VALUE0
 PRINTF, logfile_lun, INIT_VALUE1 + ident
 PRINTF, logfile_lun, ' '
 PRINT, prt_tbp
 
; update logfile_lun for common access
 CASE logt OF
   'erro' : error_logfile_lun = logfile_lun
   'test' : user_logfile_lun = logfile_lun 
   'user' : user_logfile_lun = logfile_lun 
   'mast' : master_logfile_lun = logfile_lun
   'sess' : session_logfile_lun = logfile_lun
   ELSE :
 ENDCASE
 
;-----------------------------------------------------------
; closing
;-----------------------------------------------------------
 
 CLOSING:
  s_log_n = CONV_STRING(logfile_name)  
  s_log_t = CONV_STRING(logfile_type)  
  s_dir = CONV_STRING(dir)
  ACTUAL_VALUES = [s_log_n, s_log_t, s_dir] 

  RETURN
 
 END
