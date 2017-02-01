FUNCTION LA_MASK, var, bit=bit
;+ 
; NAME: LA_MASK
; PURPOSE:
;   create a mask pointing on values of var set to undefined value.
;   See SCD definition for Mask conventions 
; CATEGORY: I-5-e
; CALLING SEQUENCE: 
;   mask=LA_MASK(var, bit=bit)
; INPUTS: 
;   var     -- array of any arithmetic type
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   bit     -- int (1 to 8) : bit to set to 1, default is the first one
; OUTPUTS: 
;    mask -- byte array of same size as var
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE: 
;   ICE> print, la_mask([la_undef(), 2, 3,4])
;      1   0   0   0
;   ICE> print, la_mask([la_undef(), 2, 3,4], bit=2)
;      2   0   0   0
;   ICE> print, la_mask([la_undef(), 2, 3,4], bit=3)
;      4   0   0   0
;   ICE> print, la_mask([la_undef(), 2, 3,4], bit=4)
;      8   0   0   0
; ALGORITHM:
;   initialize mask to a zero byte array of same dimension as var
;   compute maskval as ( 2^(bit-1)), i.e : bit-th bit set to 1
;   scan var for undefined values and set corresponding mask values
;   to maskval 
; DEPENDENCIES: 
;   basic subroutine linking LA routines and masked values
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;   none 
; RESTRICTIONS: 
;   none
; CALLED PROCEDURES AND FUNCTIONS:
;   LA_UNDEF 
; MODIFICATION HISTORY: 
;    22-Jan-1995  written with template_gen                 FV IAS
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
 
 ROUTINE_NAME = 'LA_MASK'
 VERSION = '1.0' 
 CATEGORY = 'I-5-e'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var', 'mask']
  s_var = CONV_STRING(var)
 CALL_VAL = [s_var, '']
 mask = -1
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'mask=LA_MASK(var, bit=bit)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 szv1= size(var)
 tv1 = szv1(szv1(0) + 1)
 IF (tv1 LT 1) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
 undef = la_undef(tv1)

 IF n_elements(bit) eq 0 THEN bit = 1
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------

 ; initialize mask
 mask = byte(var)
 mask(*) = 0b

 index = where(var eq undef, cpt)
 maskbit = 2b ^ (bit -1)

 IF cpt GT 0 THEN mask(index) = maskbit
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_var = CONV_STRING(var)
  s_mask = CONV_STRING(mask)
  ACTL_VAL = [s_var, s_mask]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, mask
 
 END


