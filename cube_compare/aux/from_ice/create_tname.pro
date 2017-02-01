FUNCTION CREATE_TNAME, init_string, full=full
;+ 
; NAME: CREATE_TNAME
; PURPOSE:
;    create a new string adding current date to init_string
; CATEGORY: I-4-a
; CALLING SEQUENCE: 
;   output_string = CREATE_TNAME(init_string [, /full])
; INPUTS:
;   init_string      -- string : root name
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   /full           -- 0/1 : if set, add hours, minutes, second and
;                      deciseconds (with two chars each)
; OUTPUTS:
;   output_string   -- string : init_string + current date
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE:
;   On sept, 1994 the 29th :  
;   ICE> help, create_tname('init')
;   <Expression>    STRING    = 'INIT_940929'
; ALGORITHM:
;   get time : under vms with spawn, else with !stime
;   extract all components (number, month, year, hour, minute, second)
;   translate alphabetic month into numerical month
;   remove all blanks into int_string and build output_string as
;   init_string + "_" + year + month + day
;   if full then add hour + minute + seconds + deciseconds
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;   SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL    
; SIDE EFFECTS:
;   none 
; RESTRICTIONS:
;   none 
; CALLED PROCEDURES AND FUNCTIONS:
;   none
; MODIFICATION HISTORY: 
;       21-Dec-1993  written, with template_gen        FV IAS
;       30-Sep-1994  V1.0 for configuration control    FV IAS
;       12-Dec-1994  add option /full                  FV IAS
;        7-Mar-1995  under VMS system get system time  FV IAS
;-
 
 
;-----------------------------------------------------------
; common blocks 
;-----------------------------------------------------------
 
; general environment parameters 
 COMMON SESSION_BLOCK, SESSION_MODE,  $
                       ERROR_CURRENT, $
                       STATUS_BOOL     
 
;-----------------------------------------------------------
; on error conditions
;-----------------------------------------------------------
  ON_ERROR,  ERROR_CURRENT
 
;-----------------------------------------------------------
; initialization
;-----------------------------------------------------------
 
 ROUTINE_NAME = 'CREATE_TNAME'
 VERSION = '1.0'
 CATEGORY = 'I-4-a'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= [ 'init_string', 'output_string' ]
 s_init_string = CONV_STRING(init_string)
 CALLING_VALUES = [s_init_string, '']
 output_string=''
 
;-----------------------------------------------------------
; parameters check
;-----------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output_string = CREATE_TNAME(init_string, /full)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
;-----------------------------------------------------------
; function body
;-----------------------------------------------------------

 IF !version.os eq 'vms' and KEYWORD_SET(full) THEN $
   SPAWN, "write sys$output F$TIME()", time ELSE time = !STIME
 time = time(0)

; extract date from time in format eg 22-Dec-1993 12:18:18.00 
 date = STRTRIM(STRMID(time, 0, 11),2)
 monthpos=strpos(date,'-')
 day = STRMID(date,0,monthpos)
 IF monthpos eq 1 THEN day = '0' + day
 month = STRUPCASE(STRMID(date,monthpos+1,3))
 CASE month OF
      'JAN' : month = '01'
      'FEB' : month = '02'
      'MAR' : month = '03'
      'APR' : month = '04'
      'MAY' : month = '05'
      'JUN' : month = '06'
      'JUL' : month = '07'
      'AUG' : month = '08'
      'SEP' : month = '09'
      'OCT' : month = '10'
      'NOV' : month = '11'
      'DEC' : month = '12'
 ENDCASE
 year = strmid(date,monthpos + 7,4)

 IF !version.os eq 'vms' THEN $
    output_string = strcompress(strupcase(init_string), /remove_all) $
 ELSE output_string = strcompress(init_string, /remove_all) 

 output_string = output_string + "_" + year + month + day

 IF KEYWORD_SET(full) THEN BEGIN
    hours= strmid(time, 12, 2)
    minutes = strmid(time, 15, 2)
    sec = strmid(time, 18, 2)
    dsec = strmid(time, 21, 2) 
    output_string = output_string + hours + minutes + sec + dsec
 ENDIF

;-----------------------------------------------------------
; closing
;-----------------------------------------------------------
 
 CLOSING:
 
  s_init_string = CONV_STRING(init_string) 
  s_output_string = CONV_STRING(output_string) 
  ACTUAL_VALUES = [s_init_string, s_output_string]
 
  IF (STRMID(STATUS(1),0,1) eq 'E') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALLING_VALUES, VAR_NAMES, ACTUAL_VALUES 

  RETURN, output_string
 
 END
