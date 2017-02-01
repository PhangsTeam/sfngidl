FUNCTION MA_ADD, var1, var2
;+ 
; NAME: MA_ADD
; PURPOSE:
;   merge two different masks 
; CATEGORY: I-5-d
; CALLING SEQUENCE: 
;   output=MA_ADD(var1, var2)
; INPUTS: 
;   var1, var2  -- byte arrays of same size
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   none
; OUTPUTS: 
;    output     -- byte array of same size as var1 and var2
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE:
;   ICE> print, ma_add(byte([[0,0], [3,1]]), byte([[1,0], [2,0]]))
;      1   0
;      3   1 
; ALGORITHM:
;   check parameter types and size
;   apply or command in order to merge the two masks 
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
;    12-Mar-1995  written with template_gen            FV IAS
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
 
 ROUTINE_NAME = 'MA_ADD'
 VERSION = '1.0' 
 CATEGORY = 'I-5-d'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', 'var2', 'output']
  s_var1 = CONV_STRING(var1)
  s_var2 = CONV_STRING(var2)
 CALL_VAL = [s_var1, s_var2, '']
 output=-1l
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=MA_ADD(var1, var2)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 szv1= size(var1)
 tv1 = szv1(szv1(0) + 1)
 ; check we have byte type
 IF (tv1 NE 1) THEN BEGIN
    status(0) = ['Wrong type for var1:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
 szv2= size(var2)
 tv2 = szv2(szv2(0) + 1)
 ; check we have byte type
 IF (tv2 NE 1) THEN BEGIN
    status(0) = ['Wrong type for var2:'+CONV_STRING(tv2), 'E']
    GOTO, CLOSING
 ENDIF

 ; check size identity
 NOK = where(szv1 ne szv2, cpt)
 IF n_elements(szv1) ne n_elements(szv2) or cpt GT 0 THEN BEGIN
    status(0) = ['Wrong dimension for var2:'+CONV_STRING(szv2), 'E']
    GOTO, CLOSING
 ENDIF
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------

output = var1 or var2 
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_var1 = CONV_STRING(var1)
  s_var2 = CONV_STRING(var2)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_var1, s_var2, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
