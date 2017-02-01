PRO SET_LOGFILE, logname, noerror=noerror, nosession=nosession, test=test, $
                  policy=policy, reset=reset, dir=dir, nomaster=nomaster, $
                  newsite = newsite
;+ 
; NAME: SET_LOGFILE
; PURPOSE:
;   Initializes the logfile parameters at the first call.
;   Creates (or resets) and opens logfiles at the following calls according 
;   to the specified policy. Logfile names are built following their purpose :
;   . master log : logname +'_' + current date
;   . test    : test + master log name + user + version number
;   . error   : erro + ...
;   . session : sess + ...
;
;   By default, all log but test logfile are created.
;   If no change in logname nor in current date keep same master log and
;   open only missing logs (and close non required ones). Else redefine them.
;   If key reset set, recreate every log but master logfile.
;   Changes are recorded into the master logfile (old and new if exist).
;
;   By default Recording policy is as follow :
;   . level IV routines are recorded into the master log,
;   . errors (the routine in which the error occurs , plus all its callers) are
;   recorded into the error logfile,
;   . level III and IV routines are recorded into the test log (if exists)
;   . session logfile allows to record manully user informations
;   If policy given, 0 is default, 1 make the test log record all routine
;   execution,  if a cathegory, level or sub-level given, execution of
;   all routine of this cathegory, level or sub-level is recorded into the
;   test logfile.
; CATEGORY: I-1-b
; CALLING SEQUENCE: 
;   SET_LOGFILE  [, logname] , /noerror, /nosession, /nomaster, /test$
;                policy=policy, reset=reset, dir=dir, newsite=newsite
; INPUTS: 
;   none
; OPTIONAL INPUT PARAMETERS: 
;   logname       -- string : root of logfiles name (default is "log")
; KEYED INPUTS: 
;   noerror       -- 0/1 : avoid error logfile creation (error messages are
;                    sent to the screen)
;   nosession     -- 0/1 : avoid session logfile creation
;   test          -- 0/1 : command tests logfile creation
;   reset         -- 0/1 : command creation of new test, error or session logs
;   policy        -- 0/1 or string (routine category or level) : command
;                    test logfile recording policy
;   dir           -- string : directory where to create logfiles (default is
;                    "logfile_dir")
;   newsite       -- string : site name (default site is current one -- if
;                    exists -- else it is "IAS") 
;   nomaster      -- 0/1 : avoid master logfile creation
; OUTPUTS: 
;   none
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE:
;   Reset all standard logfiles with :
;   ICE> set_logfile, /reset
;  
;   If you want to record only SW dedicated routines :
;   ICE> set_logfile, policy='III-4'  
; ALGORITHM: 
;   . set global variables if non already defined (on_error to 2, site to
;   current node, version to 'calibration system V0', project to 'ISOCAM',
;   user to current user, session_mode to 'test_lib')
;   . set undefined parameters to their default values 
;   . check policy, if 1, set session_mode to 'test' (value for recording all
;   procedure), if 0 let it to test_lib (value for recording level III
;   routines), if a string, set class_cur (calss_cur command recording of
;   routines of this level and all its sub-levels)
;   . defined master logfile name adding current date to logname
;   . open it (if exists) or recreate it with OPEN_LOGFILE
;   . if reset set, create new error, session or test logfiles (following
;   noerror, nosession or test keys), with CREATE_LOGFILE, else, open existing
;   ones and create missing ones with OPEN_LOGFILE
; DEPENDENCIES: 
;   none
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
;    LOGFILE_BLOCK, MASTER_LOGFILE_LUN, 
;                   SESSION_LOGFILE_LUN,
;                   USER_LOGFILE_LUN,   
;                   ERROR_LOGFILE_LUN 
;    TEST_BLOCK, class_cur
;    SESSION_PARAMS, SITE, VERSION, PROJECT, USER  , node
; SIDE EFFECTS:
;    create or close relevant logfile, and record their lun into variable of
;    logfile block. 
; RESTRICTIONS: 
;       UNTESTED
; CALLED PROCEDURES AND FUNCTIONS:
;    OPEN_LOGFILE, CREATE_LOGFILE, INFO_LOGFILE 
; MODIFICATION HISTORY: 
;    13-Feb-1996  written with template_gen                    FV IAS
;-
 
;------------------------------------------------------------
; common blocks 
;------------------------------------------------------------
 
; environment parameters 
 COMMON SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL
 
 COMMON LOGFILE_BLOCK, MASTER_LOGFILE_LUN, $
                       SESSION_LOGFILE_LUN,$
                       USER_LOGFILE_LUN,   $
                       ERROR_LOGFILE_LUN   

 COMMON TEST_BLOCK, class_cur

 COMMON SESSION_PARAMS, SITE, VERSION, PROJECT, USER, node
 
;------------------------------------------------------------
; on error conditions
;------------------------------------------------------------

 IF n_elements(error_current) eq 0 THEN ERROR_CURRENT = 2

 ON_ERROR,  ERROR_CURRENT
 
 ON_IOERROR, IOERROR
 GOTO, START
 IOERROR:
       STATUS(0) = ['VMS IO ERROR', 'E']
       PRINT, STATUS(2), ' : ', STATUS(0)
       PRINT, STRMESSAGE(!ERR)
       GOTO, CLOSING
 START:
 
;------------------------------------------------------------
; initialization
;------------------------------------------------------------
 
 ROUTINE_NAME = 'SET_LOGFILE'
 ROUTINE_VERSION = '1.0' 
 CATEGORY = 'I-1-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + ROUTINE_VERSION, CATEGORY]
 VAR_NAMES= ['logname', '/noerror', '/nosession', '/test', '/policy']
  s_logname = CONV_STRING(logname)
  s_noerror = CONV_STRING(noerror)
  s_nosession = CONV_STRING(nosession)
  s_test = CONV_STRING(test)
  s_policy = CONV_STRING(policy)
 CALL_VAL = [s_logname, s_noerror, s_nosession, s_test, s_policy]

 STATUS_BOOL = 0
 IF n_elements(session_mode) eq 0 THEN SESSION_MODE = 'test_lib'

;=== Modif Mac JPB
; IF n_elements(node) eq 0 THEN $
;    IF !version.os eq 'vms' THEN node = GETENV("SYS$DECDTM_NODE_NAME") $
;    ELSE BEGIN 
;       SPAWN, 'hostname', node
;       node = node(0)
;    ENDELSE
 IF n_elements(node) eq 0 THEN BEGIN
    CASE !version.os OF
      'vms':BEGIN
         node = GETENV("SYS$DECDTM_NODE_NAME")
      END
      'hp-ux':BEGIN
         SPAWN, 'hostname', node
         node = node(0)
      END
      'OSF':BEGIN
         SPAWN, 'hostname', node
         node = node(0)
      END
      'linux':BEGIN
         SPAWN, 'hostname', node
         node = node(0)
      END
      'MacOS':BEGIN
          print,'set_logfile:Doing nothing for Mac ...'
      END
    ENDCASE
ENDIF

;=== End Modif Mac JPB

 IF n_elements(newsite) GT 0 THEN site = newsite
 IF n_elements(site) eq 0 THEN site = 'IAS'
 IF n_elements(version) eq 0 THEN VERSION= ' calibration system V0 '
 IF n_elements(project) eq 0 THEN PROJECT= 'ISOCAM'

 user=GETENV('USER')
 IF n_elements(logname) eq 0 THEN logname = 'log'

 IF n_elements(dir) eq 0 THEN BEGIN
    IF !version.os eq 'vms' THEN dir = 'logfile_dir' ELSE dir = '$logfile_dir'
 ENDIF

 IF n_elements(master_logfile_lun) eq 0 THEN master_logfile_lun = -1
 IF n_elements(error_logfile_lun) eq 0 THEN error_logfile_lun = -2

;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
; IF N_PARAMS() LT 0 THEN BEGIN
;   PRINT, 'CALLING SEQUENCE: ', $ 
;    'SET_LOGFILE  [, logname, /noerror, /nosession, /test, $'
;   PRINT, '                  /reset, policy=policy]'
;   STATUS(0) = ['PARAMETER MISSING', 'E']
;   GOTO, CLOSING
; ENDIF
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------

 IF n_elements(policy) ne 0 THEN BEGIN
    szp = size(policy)
    tp = szp(szp(0)+1)
    IF (tp NE 7) and (policy EQ '1') THEN session_mode = 'test' $
    ELSE session_mode='test_lib'
    IF tp EQ 7 THEN class_cur = policy
 ENDIF

 x = fstat(3) 
 master_name=CREATE_TNAME(logname)
 count_pos = strlen(master_name)

 ; just retake existing master logfile if exists
 new=0
 OK = 1
 IF not KEYWORD_SET(nomaster) THEN $
    OPEN_LOGFILE, master_name, DIR=dir, new=new, ok = ok

 ; if already used chose between nolog at all, creating a new log or reuse
 ; next available one
 IF not OK THEN BEGIN
    count = fix(strmid(master_name, count_pos, 3)) + 10000
    char = "N"
    PRINT, master_name, " master logfile already used."
    READ, " Create a new master log (Y/N) ? (N)", char
    char = strupcase(strmid(char, 0, 1))
    IF char eq "Y" THEN BEGIN
       CREATE_LOGFILE, 'master', master_name, '', dir=dir, out=out
       OK = 1 & master_name = out & new = 1 
    ENDIF ELSE GOTO, CLOSING
 ENDIF

 IF new THEN reset = 1

 ; other log are recreated or only reopened :
 IF n_elements(session_logfile_lun) GT 0 THEN x = fstat(session_logfile_lun) $
 ELSE x = fstat(1) 
 IF not KEYWORD_SET(nosession) THEN BEGIN
       IF x.OPEN EQ 0 or KEYWORD_SET(reset) THEN $
            CREATE_LOGFILE, 'SESSION', master_name, user, DIR=dir
 ENDIF ELSE IF x.OPEN and x.unit GT 0 THEN close_logfile, 'session'

 IF n_elements(error_logfile_lun) GT 0 THEN x = fstat(error_logfile_lun) 
 IF not KEYWORD_SET(noerror) THEN BEGIN
       IF x.OPEN EQ 0 or KEYWORD_SET(reset) THEN $
          CREATE_LOGFILE, 'ERROR', master_name, user, DIR=dir
 ENDIF ELSE IF x.OPEN and x.unit GT 0 THEN CLOSE_LOGFILE, 'error'

 IF n_elements(user_logfile_lun) GT 0 THEN x = fstat(user_logfile_lun) $
 ELSE x = fstat(1)
 IF KEYWORD_SET(test) THEN BEGIN
       IF x.OPEN EQ 0 or KEYWORD_SET(reset) THEN $
          CREATE_LOGFILE, 'TESTS',  master_name, user, dir=dir
 ENDIF ELSE IF x.OPEN and x.unit GT 0 THEN close_logfile, 'user'    

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  RETURN
 END
