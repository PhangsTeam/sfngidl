FUNCTION NBYTES, type
;+ 
; NAME: NBYTES
; PURPOSE:
;    return number of bytes IDL used to implement a given type.
;    for strings (7) return 1 
; CATEGORY: I-4-b
; CALLING SEQUENCE: 
;   output=NBYTES(type)
; INPUTS: 
;   type     -- integer (1-7) IDL type number
; OPTIONAL INPUT PARAMETERS:
;   none
; KEYED INPUTS: 
;   none
; OUTPUTS: 
;    output  -- corresponding byte number
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE:
;   ICE>  print, NBYTES(4)
;         4
;   means that floats (IDL type 4) are implemented on 4 bytes 
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS: 
;   none
; RESTRICTIONS:
;   none
; CALLED PROCEDURES AND FUNCTIONS: 
;   none
; MODIFICATION HISTORY: 
;    14-Jun-1994  written from ICCRED2 S_N_BYTES    FV IAS
;    30-Sep-1994  V1.0 for configuration control    FV IAS
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
 
 ROUTINE_NAME = 'NBYTES'
 VERSION = '1.0'
 CATEGORY = 'I-4-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['type', 'output']
  s_type = CONV_STRING(type)
 CALL_VAL = [s_type, '']
 output=0
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=NBYTES(type)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------

 CASE long(type) OF
      7 : output = 1 ; string
      1 : output = 1 ; byte
      2 : output = 2 ; integer
      4 : output = 4 ; float  
      3 : output = 4 ; long  
      5 : output = 8 ; double   
      6 : output = 8 ; complex   
      ELSE : STATUS(0) = ['Invalid Type', 'E']
 ENDCASE
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
 
  s_type = CONV_STRING(type)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_type, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
