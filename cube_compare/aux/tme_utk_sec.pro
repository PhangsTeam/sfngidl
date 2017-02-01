FUNCTION TME_UTK_SEC, utk_start, utk_loc
;+ 
; NAME: TME_UTK_SEC
; PURPOSE: 
;   Compute UTK from utk_start and utk_loc as stored into trends, in seconds
; CATEGORY: I-4-c
; CALLING SEQUENCE: 
;   output=TME_UTK_SEC(utk_start, utk_loc)
; INPUTS: 
;   utk_start -- flt array  : (utk / 10000) * 10000. (first chars)
;   utk_loc   -- flt array  : (utk mod 10000)        (last chars)
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   none
; OUTPUTS: 
;    output   -- dble array : utk   ( in seconds)
; OPTIONAL OUTPUT PARAMETERS: 
; EXAMPLE: 
; ALGORITHM: 
;   A simple addition with double conversion
; DEPENDENCIES:
;   Used for explicit time scale
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;    none 
; RESTRICTIONS: 
;    none
; CALLED PROCEDURES AND FUNCTIONS:
;    none 
; MODIFICATION HISTORY: 
;    10-Nov-1995  written with template_gen from                  FV IAS
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
 
 ROUTINE_NAME = 'TME_UTK_SEC'
 VERSION = '1.0' 
 CATEGORY = 'I-4-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['output']
 CALL_VAL = ['']
 output=-1
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output=TME_UTK_SEC(utk_start, utk_loc)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------
 
 output = double(utk_start) + double(utk_loc)
 output = output * 10.

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
