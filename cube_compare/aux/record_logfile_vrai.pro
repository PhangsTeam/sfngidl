PRO RECORD_LOGFILE, STATUS_VAL, CALL_VAL, VAR_NAMES, ACT_VAL
;+ 
; NAME: RECORD_LOGFILE
; PURPOSE:
;       Record information in active logfiles.
;       Variables and execution status are recorded into test logfile.
;       If an error status, execution is also recorded into error logfile.
;       Execution of High-level routines is also recorded into master logfile,
;       as well of routine under test (with category into class_cur)
; CATEGORY: I-1-b (logfile recording)
; CALLING SEQUENCE:
;       RECORD_LOGFILE, STATUS_VAL, CAL_VAL, VAR_NAMES, ACT_VAL
; INPUTS:
;         STATUS_VAL  -- string_array with possible error message, status,
;                        routine name and category.
;         CAL_VAL     -- string_array with parameters values at calling
;                        (string converted)
;         VAR_NAMES   -- string_array with parameters names
;         ACT_VAL     -- string_array with parameters values at error or
;                         end  (string converted)
; OPTIONAL INPUT PARAMETERS:
;    none
; KEYED INPUTS:
;   none
; OUTPUTS:
;   none
; OPTIONAL OUTPUT PARAMETERS:
;   none
; EXAMPLE:
;   ICE> status = ['SUCCESS', 'S','CREATE_TNAME V.1.0','I-4']
;   ICE> RECORD_LOGFILE, status, ['init', ''], $
;        ['init_string', 'output_string'], ['init', create_tname('init')]
;   ICE> print_logfile, 'TESTS'
;   ...
;   29-Sep-1994 11:15:04.00  CREATE_TNAME < SUCCESS - S >
;    >init_string : init -> init
;    >output_string :  -> INIT_940929
;       
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   logfile of relevant types should exist 
; COMMON BLOCKS: 
;      SESSION_COMMON, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL
;      LOGFILE_COMMON, MASTER_LOGFILE_LUN,
;                      SESSION_LOGFILE_LUN,
;                      USER_LOGFILE_LUN,
;                      ERROR_LOGFILE_LUN
;
; SIDE EFFECTS:
;   write information in logfiles
; RESTRICTIONS:
;   should not be used manualy. A call is automatically put in routines
;   template by the template generator.
; WARNINGS:
;   If no error logfile, error messages are sent to the screen
; CALLED PROCEDURES AND FUNCTIONS:
;   none
; MODIFICATION HISTORY:  
;        7-Dec-1993  written                       FV IAS
;       20-Dec-1993  update logfiles lun names     FV IAS
;       04-Jan-1994  test and common update        FV IAS
;       11-Avr-1994  update routine_name presentation and correct error in
;                    recursive call                FV IAS
;-
 
;-----------------------------------------------------------
; common blocks 
;-----------------------------------------------------------
 COMMON SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 

 COMMON LOGFILE_BLOCK,           $
             MASTER_LOGFILE_LUN, $
             SESSION_LOGFILE_LUN,$
             USER_LOGFILE_LUN,   $
             ERROR_LOGFILE_LUN

 COMMON TEST_BLOCK, class_cur

;-----------------------------------------------------------
; on error conditions
;-----------------------------------------------------------
  ON_ERROR, ERROR_CURRENT  
  ON_IOERROR, IOERRORREF
  GOTO, START
  IOERRORREF:
        STATUS(0) = ['VMS IO ERROR', 'E']
        PRINT, STATUS(2), ' - ', STATUS(1), ' - ', STATUS(0)
        PRINT, STRMESSAGE(!ERR)
  GOTO, CLOSING
  START: 
 
;-----------------------------------------------------------
; initialization
;-----------------------------------------------------------

 ROUTINE_NAME = 'RECORD_LOGFILE'
 VERSION = '1.0'
 CATEGORY='I-1-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]

 category_val = STATUS_VAL(3)
 class = STRMID(category_val, 0, STRPOS(STATUS_VAL(3), '-')) 
 logfile_lun = -1 

 master_bool=0

 IF N_ELEMENTS(session_logfile_lun) EQ 0 THEN session_logfile_lun=-1 $
 ELSE BEGIN
   sstat=fstat(session_logfile_lun)
   IF sstat.open EQ 0 THEN session_logfile_lun = -1
 ENDELSE
 
 IF STATUS_BOOL and (N_ELEMENTS(error_logfile_lun) NE 0) THEN BEGIN
       stat = fstat(error_logfile_lun)
       IF (stat.open NE 0) THEN error_lun = ERROR_LOGFILE_LUN $
                           ELSE error_lun = -1
 ENDIF ELSE error_lun=-1

 IF (N_ELEMENTS(user_logfile_lun) NE 0) THEN BEGIN
       stat = fstat(user_logfile_lun)
       IF (stat.open NE 0) THEN BEGIN 
          IF (session_mode EQ 'testlib') and (class EQ 'III') $
          THEN logfile_lun = user_logfile_lun
          IF (session_mode EQ 'test') THEN logfile_lun = user_logfile_lun     
       ENDIF
 ENDIF

 IF (N_ELEMENTS(master_logfile_lun) NE 0) THEN BEGIN
    stat = fstat(master_logfile_lun)
    IF (stat.open NE 0) THEN BEGIN
       IF (class EQ 'IV') THEN master_bool=1
       IF n_elements(class_cur) NE 0 THEN $
          IF (strmid(category_val,0,strlen(class_cur)) EQ class_cur) $
          THEN logfile_lun = master_logfile_lun
    ENDIF  
 ENDIF

;-----------------------------------------------------------
; parameters check
;-----------------------------------------------------------
 
 IF N_PARAMS(DUMMY) LT 4 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: '
   PRINT, '  RECORD_LOGFILE, STATUS_VAL,', $
                  ' CALL_VAL, VAR_NAMES, ACT_VAL'  
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 sc = n_elements(CALL_VAL)
 sn = n_elements(VAR_NAMES)
 sa = n_elements(ACT_VAL)

 IF ((sc NE sn) OR (sn NE sa)) THEN BEGIN 
   PRINT, STATUS_VAL(2), ' (', STATUS_VAL(3), ') ', $
             ':  arrays should have the same size'
   PRINT, sc, ' calling parameters'
   PRINT, sn, ' parameters names'
   PRINT, sa, ' actual parameters'   
   STATUS(0) = ['INCORRECT SIZES', 'E']
   GOTO, CLOSING
 ENDIF

;-----------------------------------------------------------
; function body
;-----------------------------------------------------------
; print status on current logfile for the function where the error occurs
 IF (STATUS_VAL(1) NE 'S') and (session_mode EQ 'test') THEN BEGIN     
    PRINT, STATUS_VAL(2), ' : '
    PRINT, '   ', STATUS_VAL(1), ' - ', STATUS_VAL(0)
    ; space between call blocks in error logfile
    IF (N_ELEMENTS(error_lun) NE 0) THEN IF error_lun GT 0 THEN BEGIN 
       PRINTF, error_lun, '  ----- '
       PRINTF, error_lun, ' '
    ENDIF
 ENDIF

 IF STATUS_BOOL THEN BEGIN
    PRINTF, error_lun, !STIME, '  ', STATUS_VAL(2), ' < ', $
                       STATUS_VAL(0), ' - ', STATUS_VAL(1), ' >'
    help, calls = calls
    ncalls=n_elements(calls) - 1
    ; calls contains :
    ; 0                -- ref to record_logfile (unusefull)
    ; 1 ... ncalls - 2 -- intermediate calls
    ; ncalls - 1       -- highest level function
    ; ncalls           -- $MAIN$     (unusefull but kept for interactive call)
    IF (STATUS_VAL(1) NE 'S') THEN BEGIN  
       FOR i=1, ncalls DO PRINTF, error_lun, ' ->', calls(i)
    END
    ; if no intermediate calls ncalls = 2 (reset status_bool)
    IF ncalls LT 3 THEN STATUS_BOOL = 0
    FOR i=0, (sc - 1) DO $
        PRINTF, error_lun, ' >', VAR_NAMES(i), ' : ', CALL_VAL(i),$
                                ' -> ', ACT_VAL(i)
    PRINTF, error_lun, ' '
 ENDIF

; if high level function : record in master logfile
 IF master_bool EQ 1 THEN BEGIN
    ; date and time, then function name and parameters
    PRINTF, master_logfile_lun, !stime, STATUS_VAL(2), ' < ', $
                       STATUS_VAL(0), ' - ', STATUS_VAL(1), ' >' 
    FOR i=0, (sc - 1) DO $
        PRINTF, master_logfile_lun, ' >', VAR_NAMES(i), ' : ', CALL_VAL(i),$
                            ' -> ', ACT_VAL(i)
    PRINTF, master_logfile_lun, ' '
    ; and spaces in user or error logfile
    IF logfile_lun GT 0 THEN PRINTF, logfile_lun, ' '
 ENDIF  
 
; print detailed information in logfile
; no date nor time necessary as they are recorded at logfile creation and such
; logfiles - user and error - are local to one session

 IF (logfile_lun GT 0) THEN BEGIN
    PRINTF, logfile_lun, !STIME, '  ', STATUS_VAL(2), ' < ', $
                         STATUS_VAL(0), ' - ', STATUS_VAL(1), ' >'
    FOR i=0, (sc - 1) DO BEGIN
        IF strlen(call_val(i)) GT 500 THEN $
           call_val(i) = strmid(call_val(i),0,450) + "..."
        IF strlen(act_val(i)) GT 500 THEN $
           act_val(i) = strmid(act_val(i),0,450) + "..."
        PRINTF, logfile_lun, ' >', VAR_NAMES(i), ' : ', CALL_VAL(i),$
                                ' -> ', ACT_VAL(i)
    ENDFOR
    PRINTF, logfile_lun, ' '  ; aerons, aerons ...    
 ENDIF   
 
;-----------------------------------------------------------
; closing
;-----------------------------------------------------------

 CLOSING:
 IF (STRMID(STATUS(1),0,1) NE 'S') THEN $
    PRINT, STATUS(2),'(', STATUS(3), ') -- ', STATUS(0)
 
 RETURN
 
 END
