PRO CREATE_LOGFILE, LOGFILE_TYPE, SESSION_NAME, IDENT, DIR=DIR, OUT=OUT 
;+ 
; NAME: CREATE_LOGFILE
; PURPOSE:
;          Create, open and initialize a file of given type to be used as
;          logfile.
;          If a logfile of type logfile_type is currently active, the old one
;          is closed before. 
;          If logfile is a master logfile, the file name is build concatenating
;          session_name, ident and current date, else with logfile_type,
;          session_name, ident and a version number.
;          File extension is "lf" 
; CATEGORY: I-1-b
; CALLING SEQUENCE: 
;   CREATE_LOGFILE, LOGFILE_TYPE, SESSION_NAME, IDENT
; INPUTS:
;    LOGFILE_TYPE -- master, session, user, tests or error
;    SESSION_NAME -- string. If master logfile, a purpose else master logfile
;                    name.
;                    If session_name is longer than 20 chars (24 chars if no
;                    ident given) only the first 20 chars are used to generate
;                    the log file name
;    IDENT        -- string : if master logfile, campaign identifier else, user
;                    identifier.
;                    If ident is longer than 8 chars, only the first 8 chars
;                    are used.
; OPTIONAL INPUT PARAMETERS:
;    none
; KEYED INPUTS:
;    DIR          -- directory where to create such logfile, default is
;                    current directory
;    OUT          -- logfile name without extension
; OUTPUTS:
;    none 
; OPTIONAL OUTPUT PARAMETERS:
;    none 
; EXAMPLE:
;   this instruction
;     CREATE_LOGFILE, 'ERROR', 'TESTS_UNITAIRES', 'ICE_USER', $
;                     DIR = 'logfile_dir'
;   create the file in directory defined by logical name logfile_dir :
;     ERRO_TESTS_UNITAIRE_ICE_USER001.LF
;   the file is initialized with its name and current date and time and will
;   log execution of routines where an error is detected and execution of the
;   callers. 
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   If called with a logfile_type different from Master, a master logfile
;   should be active.
; COMMON BLOCKS: 
;     SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL    
;     LOGFILE_BLOCK, MASTER_LOGFILE_LUN, 
;                    SESSION_LOGFILE_LUN,
;                    USER_LOGFILE_LUN,   
;                    ERROR_LOGFILE_LUN
; SIDE EFFECTS:
;   open a file and affect relevant lun. 
; RESTRICTIONS:
;   generation of version number work up to 999 versions
;   if logfile_type is not a master logfile, a message is sent to master log
;   in order to record name of new current logfile
; CALLED PROCEDURES AND FUNCTIONS:
;        create_tname 
; MODIFICATION HISTORY: 
;       21-Dec-1993  written with template_gen              FV IAS 
;       04-Jan-1994  test and update                        FV IAS 
;       26-Jan-1994  update logfile name policy             FV IAS 
;       24-Fev-1994  restrict filename length               FV IAS 
;       30-Sep-1994  V1.0 for configuration control         FV IAS
;-
 
;-----------------------------------------------------------
; common blocks 
;-----------------------------------------------------------
 
; general environment parameters 
 COMMON SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL     

; logfiles parameters 
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
        PRINT, STATUS(2) + ' - ' + STATUS(1) + ' - ' + STATUS(0)
        PRINT, STRMESSAGE(!ERR)
        GOTO, CLOSING
 START:
 
;-----------------------------------------------------------
; initialization
;-----------------------------------------------------------
 
 ROUTINE_NAME = 'CREATE_LOGFILE'
 VERSION = '1.0'
 CATEGORY = 'I-1-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['LOGFILE_TYPE', 'SESSION_NAME', 'IDENT']
  s_logfile_type = CONV_STRING(logfile_type)
  s_session_name = CONV_STRING(session_name)
  s_ident = CONV_STRING(ident)
 CALLING_VAL = [ s_logfile_type, s_session_name, s_ident ]
 
;-----------------------------------------------------------
; parameters check
;-----------------------------------------------------------
 
 IF N_PARAMS() LT 3 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: CREATE_LOGFILE, LOGFILE_TYPE, SESSION_NAME, IDENT'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 LOGT = STRMID(STRTRIM(strlowcase(LOGFILE_TYPE),2),0,4)

 IF ((LOGT NE 'erro') AND (LOGT NE 'user')) AND (((LOGT NE 'mast') AND $
     (LOGT NE 'sess')) AND (LOGT NE 'test')) THEN BEGIN
   PRINT, 'possible LOGFILE_TYPE are currently: error, user, tests, ', $
          'master or session'
   PRINT, 'invalid type : ', LOGFILE_TYPE 
   STATUS(0) = ['INVALID TYPE', 'E']
   GOTO, CLOSING
 ENDIF
 
;-----------------------------------------------------------
; function body
;-----------------------------------------------------------
 
; create new current logfile name
 
; check number of characters, if greater than 20 then cut -- arbitrary value,
; but cut should avoid operating system limitation upon filename length

 callname=strmid(session_name,0,24)
 IF (strlen(session_name) LE 20) AND (strlen(IDENT) GT 0) $
 THEN callname=callname + '_' + strmid(ident,0,8)

 IF LOGT NE 'mast' THEN name = logt + '_' + callname $
                   ELSE name = CREATE_TNAME(callname)

; check if a logfile with such name already exists

 wildname = name + '*'
 count = CHECK_FILE(wildname, 'lf', dir)

; extract the 3 last numerical characters in count (number of logfiles with
; name beginning with "name"), and replace ' ' by 0, in order to build a
; relevant extension
 ct = strmid(string(count(0)), 9, 3)
 posb=strpos(ct, ' ')
 WHILE posb NE -1 DO BEGIN
       STRPUT, ct, '0', posb
       posb=strpos(ct,' ')
 ENDWHILE

; add count in order to have version number at the end of the name
 name = name + ct

; then affect relevant lun and open file in writing
 CASE LOGT OF
   'erro' : BEGIN
             IF N_ELEMENTS(error_logfile_lun) NE 0 THEN $
                IF (error_logfile_lun LE 128) AND (error_logfile_lun GE 100)$
                THEN FREE_LUN, error_logfile_lun 
             GET_LUN, error_logfile_lun
             logfile_lun = error_logfile_lun
             INIT_VALUE = 'ERROR LOGFILE : '
            END
   'test' : BEGIN 
             IF N_ELEMENTS(user_logfile_lun) NE 0 THEN $
                IF (user_logfile_lun LE 128) AND (user_logfile_lun GE 100)$
                THEN FREE_LUN, user_logfile_lun 
             GET_LUN, user_logfile_lun
             logfile_lun = user_logfile_lun
             INIT_VALUE = 'TESTS LOGFILE : '
            END 
   'user' : BEGIN 
             IF N_ELEMENTS(user_logfile_lun) NE 0 THEN $
                IF (user_logfile_lun LE 128) AND (user_logfile_lun GE 100)$
                THEN FREE_LUN, user_logfile_lun 
             GET_LUN, user_logfile_lun
             logfile_lun = user_logfile_lun
             INIT_VALUE = 'USER LOGFILE : '
             END 
   'mast' : BEGIN 
             IF N_ELEMENTS(master_logfile_lun) NE 0 THEN IF $
                (master_logfile_lun LE 128) AND (master_logfile_lun GE 100)$
                THEN FREE_LUN, master_logfile_lun 
             GET_LUN, master_logfile_lun
             logfile_lun = master_logfile_lun
             INIT_VALUE = 'MASTER LOGFILE : '
            END
   'sess' : BEGIN 
             IF N_ELEMENTS(session_logfile_lun) NE 0 THEN IF $
                (session_logfile_lun LE 128) AND (session_logfile_lun GE 100)$
                THEN FREE_LUN, session_logfile_lun 
             GET_LUN, session_logfile_lun
             logfile_lun = session_logfile_lun
             INIT_VALUE = 'SESSION LOGFILE : '
            END
   ELSE :
 ENDCASE

 adddir = ''
 IF n_elements(dir) eq 1 THEN BEGIN
    IF !version.os eq 'vms' THEN BEGIN
       pos = strpos(dir, ":")
       IF pos GT 0 THEN adddir = dir ELSE adddir= dir + ':'
    ENDIF ELSE adddir = dir +'/'
 ENDIF

 OPENW,  logfile_lun, adddir + name + '.lf'
 PRINTF, logfile_lun, INIT_VALUE, name 
 PRINTF, logfile_lun, !stime, '  creation by user ', GETENV('USER')
 PRINTF, logfile_lun, ' '
 string_tbp= "new " + LOGFILE_TYPE + " logfile created : " + name + '.lf'
 PRINT, string_tbp

; signal logfile creation to master logfile :
 IF (logfile_lun NE master_logfile_lun) and (master_logfile_lun GE 100) THEN $
   INFO_LOGFILE, status, string_tbp

; extract name root for further logfile name creation
 OUT=name 

;-----------------------------------------------------------
; closing
;-----------------------------------------------------------
 
 CLOSING:
  s_logfile_type = CONV_STRING(logfile_type)
  s_session_name = CONV_STRING(session_name)
  s_ident = CONV_STRING(ident)
  ACTUAL_VAL = [ s_logfile_type, s_session_name, s_ident ]

  RETURN
 
 END
