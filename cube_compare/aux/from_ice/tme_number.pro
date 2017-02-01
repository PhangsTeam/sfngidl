FUNCTION TME_NUMBER, date, undef=undef
;+ 
; NAME: TME_NUMBER
; PURPOSE: 
;   Convert time given as a string into a number
; CATEGORY: I-4-c
; CALLING SEQUENCE: 
;   output=TME_NUMBER(date)
; INPUTS: 
;   date   -- string : e.g. 18-OCT-1995 12:09:33
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   undef  -- string : output if date undefined
; OUTPUTS: 
;   output -- string : time e.g 951018_120933
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE: 
; ALGORITHM: 
; DEPENDENCIES: 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS: 
; RESTRICTIONS: 
;       UNTESTED
; CALLED PROCEDURES AND FUNCTIONS: 
; MODIFICATION HISTORY: 
;    18-Oct-1995  written with template_gen 
;-
 
;------------------------------------------------------------
; common blocks 
;------------------------------------------------------------
 
; environment parameters 
 COMMON SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL
 
;------------------------------------------------------------
; on error conditions
;------------------------------------------------------------
 ON_ERROR,  ERROR_CURRENT
 
;------------------------------------------------------------
; initialization
;------------------------------------------------------------
 
 ROUTINE_NAME = 'TME_NUMBER'
 VERSION = '1.0' 
 CATEGORY = 'I-4-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['date', 'output']
  s_date = CONV_STRING(date)
 CALL_VAL = [s_date, '']
 output=''
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=TME_NUMBER(date)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
 IF n_elements(date) eq 0 THEN BEGIN
    output = undef
    GOTO, CLOSING
 ENDIF

;------------------------------------------------------------
; function body
;------------------------------------------------------------
 
 monthpos=strpos(date,'-')
 IF monthpos GE 0 THEN day = STRMID(date,0,monthpos) ELSE day = '00'
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
 year = strmid(date,monthpos + 7,2)

 output = year + month + day

 IF strlen(date) GT 12 THEN BEGIN
    hours= strmid(date, 12, 2)
    minutes = strmid(date, 15, 2)
    sec = strmid(date, 18, 2)
    dsec = strmid(date, 21, 2) 
    output = output + hours + minutes + sec + dsec
 ENDIF
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_date = CONV_STRING(date)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_date, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
