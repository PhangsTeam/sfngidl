FUNCTION MA_TEST, var1, pos, bit=bit
;+ 
; NAME: MA_TEST
; PURPOSE:
;   Extract mask value following bit "bit".
; CATEGORY: I-5-d
; CALLING SEQUENCE: 
;   output=MA_TEST(var1  [, pos] , bit=bit)
; INPUTS: 
;   var1    -- byte array : mask
; OPTIONAL INPUT PARAMETERS: 
;   pos     -- integer or integer array : index between mask, if given select
;              only pos values
; KEYED INPUTS: 
;   bit     -- (1-8) : bit position (default is 1) 
; OUTPUTS: 
;   output  -- (0/1) array same dimension as var1 : projection following "bit"
;              bit (initialized to 0l)
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE: 
;   ICE> print, ma_test([1b,2b,3b,4b])
;          1       0       1       0
;   ICE> print, ma_test([1b,2b,3b,4b], bit=2)
;          0       1       1       0
; ALGORITHM:
;   initialize output to 0 and check input parameters
;   initialize output as var1 (if pos given var1(pos))
;   extract "bit" bit with the and operator
;   replace non zero value by 1.
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
;   12-Mar-1995  written with template_gen               FV IAS 
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
 
 ROUTINE_NAME = 'MA_TEST'
 VERSION = '1.0' 
 CATEGORY = 'I-5-d'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', 'pos', '/bit', 'output']
  s_var1 = CONV_STRING(var1)
  s_pos = CONV_STRING(pos)
  s_bit = CONV_STRING(bit)
 CALL_VAL = [s_var1, s_pos, s_bit, '']
 output=0l
  
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=MA_TEST(var1  [, pos] , bit=bit)'
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

 szv1= size(pos)
 tv1 = szv1(szv1(0) + 1)
 ; check we have integer types
 IF (tv1 GT 3) THEN BEGIN
    status(0) = ['Wrong type for pos:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF

 ; check pos versus var1 dimension
 no = n_elements(var1)
 IF n_elements(pos) GT 0 THEN NOK = where(pos GT no or pos LT 0, cpt) $
 ELSE cpt = 0
 IF cpt GT 0 THEN BEGIN
    status(0) = ['Wrong value for pos :'+CONV_STRING(index) + $
                 ' versus var1 : '+CONV_STRING(dim), 'E']
    output = 0
    GOTO, CLOSING
 ENDIF

 IF n_elements(bit) eq 0 THEN bit = 1
 szv1= size(bit)
 tv1 = szv1(szv1(0) + 1)
 ; check we have integer types
 IF (tv1 LT 1) or (tv1 GT 3) THEN BEGIN
    status(0) = ['Wrong type for bit:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
 IF bit LT 1 or bit GT 8 THEN BEGIN
    status(0) = ['Wrong value for bit:'+strtrim(bit,2), 'E']
    GOTO, CLOSING
 ENDIF
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------

; select pos values if require
 IF n_elements(pos) GT 0 THEN output = var1(pos) ELSE output = var1

; compute bit value 
 value = 2^(bit - 1)

 output = (output and value) / value
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_var1 = CONV_STRING(var1)
  s_pos = CONV_STRING(pos)
  s_bit = CONV_STRING(bit)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_var1, s_pos, s_bit, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
