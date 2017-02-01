PRO INFO_LOGFILE, STATUS_VAL, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, $
    i12, i13, i14, i15, i16, i17, i18, i19, i20, logfiletype=logfiletype, $
    sess=sess
;+ 
; NAME:
;    INFO_LOGFILE
; PURPOSE:
;    Record additional information into logfile in addition to those recorded
;    by record_logfile.
;    Put ">>" before any line of the master logfile as comment character
; CATEGORY: I-1-b (logfile management)
; CALLING SEQUENCE:
;    INFO_LOGFILE, STATUS_VAL, INFO, logfiletype=logfiletype
; INPUTS:
;    STATUS_VAL  -- array of 4 strings : current error message, status ('E',
;                   'S' or 'W'), routine name at the origin of the message and
;                   its category.
;                   if the first variable is not an array of 4 strings, then
;                   it is considered as a variable. 
;    INFO        -- up to 20 variables (21 if no status given) 
; OPTIONAL INPUT PARAMETERS: 
;    none
; KEYED INPUTS:
;    logfiletype -- string : target logfile (default is master logfile)
;    sess        -- 0/1 : if set logfiletype is set to session
; OUTPUTS: 
;   none
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE:
;   ICE>  INFO_LOGFILE, 'information'
;   ICE>  PRINT_LOGFILE, 'master'
;   ...
;          >> information
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   target logfile (given with logfiletype) must exists 
; COMMON BLOCKS: 
;   SESSION_COMMON, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL  
;   LOGFILE_COMMON, MASTER_LOGFILE_LUN,
;                   SESSION_LOGFILE_LUN,
;                   USER_LOGFILE_LUN,
;                   ERROR_LOGFILE_LUN
; SIDE EFFECTS:
;   write additional information into logfiles
; RESTRICTIONS:
;   target logfile must have been initialized
; CALLED PROCEDURES AND FUNCTIONS:
;   none
; MODIFICATION HISTORY:
;   13 Jun 1994   written from record_logfile             FV IAS
;   30 Sep 1994   V1.0 for configuration control          FV IAS
;   27 Feb 1994   add 20 possible parameters              FV IAS  
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

;-----------------------------------------------------------
; on error conditions
;-----------------------------------------------------------
  ON_ERROR, ERROR_CURRENT
  
  ON_IOERROR, IOERRORREF
  GOTO, START
  IOERRORREF:
        STATUS(0) = ['VMS IO ERROR', 'E']
        PRINT, STRMESSAGE(!ERR)
  GOTO, CLOSING
  START: 
 
;-----------------------------------------------------------
; initialization
;-----------------------------------------------------------

 ROUTINE_NAME = 'INFO_LOGFILE'
 VERSION = '1.0'
 CATEGORY='I-1-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY] 
 logfile_lun = - 1

 IF KEYWORD_SET(sess) THEN logfiletype = 'session'
            
 IF n_elements(logfiletype) LE 0 THEN logt = 'mast' $
 ELSE logt = strmid(strlowcase(logfiletype),0,4)

 CASE logt OF
         'test' : IF (N_ELEMENTS(user_logfile_lun) NE 0) THEN $
                     logfile_lun = user_logfile_lun
         'user' : IF (N_ELEMENTS(user_logfile_lun) NE 0) THEN $
                     logfile_lun = user_logfile_lun
         'mast' : IF (N_ELEMENTS(master_logfile_lun) NE 0) THEN $
                     logfile_lun = master_logfile_lun
         'erro' : IF (N_ELEMENTS(error_logfile_lun) NE 0) THEN $
                     logfile_lun = error_logfile_lun
         'sess' : IF (n_elements(session_logfile_lun) NE 0) THEN $
                     logfile_lun = session_logfile_lun
         ELSE : logfile_lun = - 1
 ENDCASE

;-----------------------------------------------------------
; parameters check
;-----------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $
          ' INFO_LOGFILE [, STATUS_VAL], $'
   PRINT, '                  exp1, ..., expn, logfiletype=logfiletype, /sess'  
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 ns = n_elements(status_val)
 ssv = size(status_val)
 tsv = ssv(ssv(0)+1)

 nostatus = 0  & info = ''

 IF (ns ne 4) or (tsv ne 7) THEN BEGIN
    info = string(status_val)
    nostatus=1
 ENDIF

;-----------------------------------------------------------
; function body
;-----------------------------------------------------------

 IF n_elements(i1) eq 0 THEN i1 = ''
 IF n_elements(i2) eq 0 THEN i2 = ''
 IF n_elements(i3) eq 0 THEN i3 = ''
 IF n_elements(i4) eq 0 THEN i4 = ''
 IF n_elements(i5) eq 0 THEN i5 = ''
 IF n_elements(i6) eq 0 THEN i6 = ''
 IF n_elements(i7) eq 0 THEN i7 = ''
 IF n_elements(i8) eq 0 THEN i8 = ''
 IF n_elements(i9) eq 0 THEN i9 = ''
 IF n_elements(i10) eq 0 THEN i10 = ''
 IF n_elements(i11) eq 0 THEN i11 = ''
 IF n_elements(i12) eq 0 THEN i12 = ''
 IF n_elements(i13) eq 0 THEN i13 = ''
 IF n_elements(i14) eq 0 THEN i14 = ''
 IF n_elements(i15) eq 0 THEN i15 = ''
 IF n_elements(i16) eq 0 THEN i16 = ''
 IF n_elements(i17) eq 0 THEN i17 = ''
 IF n_elements(i18) eq 0 THEN i18 = ''
 IF n_elements(i19) eq 0 THEN i19 = ''
 IF n_elements(i20) eq 0 THEN i20 = ''

 info = string(info, i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,$
               i17, i18,i19,i20,/print)
 ni = n_elements(info)

 IF ni eq 1 and info(0) eq '' THEN ni = 0

 IF logt eq 'mast' THEN prefix = '  >> ' ELSE prefix = ''

; print information in logfile
 IF (logfile_lun GT 0) THEN BEGIN
    IF not nostatus THEN PRINTF, logfile_lun, !stime, ' ', STATUS_VAL(2), $
                         ' < ', STATUS_VAL(0), ' - ', STATUS_VAL(1), ' >'
    FOR i=0, (ni -1) DO PRINTF, logfile_lun, prefix, INFO(i)
    PRINTF, logfile_lun, ' '    
 ENDIF ELSE status(0) = ['Logfile either not open or not a file', 'E']
    
;-----------------------------------------------------------
; closing
;-----------------------------------------------------------

 CLOSING:
 IF (STRMID(STATUS(1),0,1) NE 'S') THEN $
    PRINT, STATUS(2),' - ', STATUS(1), ' - ', STATUS(0)
 
 RETURN
 
 END
