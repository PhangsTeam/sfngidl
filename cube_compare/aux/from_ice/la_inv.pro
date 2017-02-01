FUNCTION LA_INV, var1
;+ 
; NAME: LA_INV
; PURPOSE:
;          calculate the inverse each element of an array variable, taking
;          into account
;          undefined values and 0 values (which give undefined values)
; CATEGORY: I-5-b
; CALLING SEQUENCE: 
;   output=LA_INV(var1)
; INPUTS: 
;   var1     -- array (or scalar) variable with mathematic type
; OPTIONAL INPUT PARAMETERS:
;   none 
; KEYED INPUTS:
;   none
; OUTPUTS: 
;   output   -- float (or double) array with same dimension as var1 
; OPTIONAL OUTPUT PARAMETERS:
;   none
; EXAMPLE:
;   ICE> print, la_inv([1, 4, 8])
;         1.00000     0.250000     0.125000
; ALGORITHM:
;   initialize output to an undefined array of same dimension as var1
;   and of type double if var1 is of type double, else of type float. 
;   scan var1 for defined values and extract their location
;   set corresponding output to the inverse
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;   none 
; RESTRICTIONS: 
;   none
; CALLED PROCEDURES AND FUNCTIONS: 
;    LA_UNDEF 
; MODIFICATION HISTORY: 
;    8-Jul-1994  written with template_gen                 FV IAS
;    5-Oct-1994 V.1.0 for configuration control            FV IAS
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
 
 ROUTINE_NAME = 'LA_INV'
 VERSION= '1.0'
 CATEGORY = 'I-5-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', 'output']
  s_var1 = CONV_STRING(size(var1))
 CALL_VAL = [s_var1, '']
 output = -1.

;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=LA_INV(var1)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 szv1 = size(var1)
 tv1 = szv1(szv1(0) + 1)
 ; check we have arithmetic types (1 to 5: byte, int, long int, float or
 ; double)
 IF (tv1 LT 1) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var1:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
 ; convert into float for integers
 IF tv1 LE 3 THEN szv1(szv1(0)+1) = 4

 undef = la_undef(szv1(szv1(0)+1))

 ; initialize output to undefined cube
 IF szv1(0) GT 0 THEN output=make_array(size=szv1, value=undef) $
 ELSE output=undef
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------
good_values = where((var1 ne undef) and (var1 ne 0), cpt) 
 
IF cpt GT 0 THEN output(good_values) = 1. / var1(good_values)
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  
  s_var1 = CONV_STRING(size(var1))
  s_output = CONV_STRING(size(output))
  ACTL_VAL = [s_var1, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
