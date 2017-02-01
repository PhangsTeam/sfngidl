FUNCTION MA_CREATE, index, dim, bit=bit
;+ 
; NAME: MA_CREATE
; PURPOSE:
;   create a SCD like mask knowing global index of masked pixels, dimensions
;   of the mask and bit posiiotn.
; CATEGORY: I-5-d
; CALLING SEQUENCE: 
;   output=MA_CREATE(index, dim, bit=bit)
; INPUTS: 
;   index     -- long array : global index masked positions
;   dim       -- long array : mask dimension
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   bit       -- 1-8 : bit position (default is 1)
; OUTPUTS: 
;    output   -- byte array : SCD mask (with good values on zero)
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE: 
;   ICE> print, ma_create([2, 4, 6], [8,2], bit=3)
;      0   0   4   0   4   0   4   0
;      0   0   0   0   0   0   0   0
; ALGORITHM:
;   check parameter type and sizes
;   check index versus dimensions
;   initialize output to a zero mask, define masked value following bit value
;   set bad values 
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
;    12-Mar-1995  written with template_gen          FV IAS
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
 
 ROUTINE_NAME = 'MA_CREATE'
 VERSION = '1.0' 
 CATEGORY = 'I-5-d'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['index', 'dim', '/bit', 'output']
  s_index = CONV_STRING(index)
  s_dim = CONV_STRING(dim)
  s_bit = CONV_STRING(bit)
 CALL_VAL = [s_index, s_dim, s_bit, '']
 output=-1l
  
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=MA_CREATE(index, dim, bit=bit)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
 szv1= size(index)
 tv1 = szv1(szv1(0) + 1)
 ; check we have integer types
 IF (tv1 LT 1) or (tv1 GT 3) THEN BEGIN
    status(0) = ['Wrong type for index:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
            
 szv1= size(dim)
 tv1 = szv1(szv1(0) + 1)
 ; check we have integer types
 IF (tv1 LT 1) or (tv1 GT 3) THEN BEGIN
    status(0) = ['Wrong type for dim:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
 NOK = where(dim LT 1, cpt)
 IF cpt GT 0 THEN BEGIN
    status(0) = ['Wrong value for dim:'+CONV_STRING(dim), 'E']
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
; initialize output to a  zero (good) mask :
 output = make_array(dim=dim, /byte)

; check index versus output dimension
 no = n_elements(output)
 NOK = where(index GT no, cpt)
 IF cpt GT 0 THEN BEGIN
    status(0) = ['Wrong value for index :'+CONV_STRING(index) + $
                 ' versus dim : '+CONV_STRING(dim), 'E']
    output = -1l
    GOTO, CLOSING
 ENDIF

; initialize bit value
 value = 2^(bit -1)

; set index values to masked value
 output(index) = value
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_index = CONV_STRING(index)
  s_dim = CONV_STRING(dim)
  s_bit = CONV_STRING(bit)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_index, s_dim, s_bit, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
