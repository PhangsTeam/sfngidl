FUNCTION LA_NORM, var1, dim=dim, mean=mean, median=median
;+ 
; NAME: LA_NORM
; PURPOSE:
;          normalize var1 to between 0 and 1
;          if keyword dim set normalization is effected plane by plane 
;          (previous normalize_cube)
;          undefined values are set to 0
;          zero images becomes undefined images of same dimension 
; CATEGORY: I-5-b
; CALLING SEQUENCE: 
;   output=LA_NORM(var1, /dim, /mean, /median)
; INPUTS: 
;   var1     -- array of arithmetic type
; OPTIONAL INPUT PARAMETERS:
;   none 
; KEYED INPUTS:
;   dim      -- point that normalization should not be applied to the whole
;               "cube" (or hypercube) but "plane" (or hyperplane) by "plane"
;               if dim is set to -1 normalization is applyed vector by vector 
;   mean     -- 0/1 : if set normalize following mean value
;   median   -- 0/1 : if set normalize following median value
 
; OUTPUTS: 
;    output  -- float (or double) array of same dimensions as var1
; OPTIONAL OUTPUT PARAMETERS:
;    none 
; EXAMPLE:
;     ICE> print, la_norm([[6,3,2,1], [8,6,0,0]])
;        0.750000     0.375000     0.250000     0.125000
;        1.00000     0.750000     0.000000     0.000000
;     ICE> print, la_norm([[6,3,2,1], [8,6,0,0]] , /dim)
;         1.00000     0.500000     0.333333     0.166667
;         1.00000     0.750000     0.000000     0.000000
;     ICE> print, la_norm([[6,3,2,1], [8,6,0,0]] , dim=-1)
;        0.750000     0.500000      1.00000      1.00000
;         1.00000      1.00000     0.000000     0.000000
; ALGORITHM:
;   straightforward
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;    none 
; RESTRICTIONS: 
;    none
; CALLED PROCEDURES AND FUNCTIONS :
;    LA_MAX
;    LA_DIV
;    LA_UNDEF 
; MODIFICATION HISTORY: 
;    11-Jul-1994  written with template_gen             FV IAS
;    26-Aug-1994  add keyword dim                       FV IAS
;    3-Oct-1994  V.1.0 for configuration control        FV IAS
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
 
 ROUTINE_NAME = 'LA_NORM'
 VERSION = '1.0'
 CATEGORY = 'I-5-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', '/dim', 'output']
  s_var1 = CONV_STRING(var1)
  s_dim = CONV_STRING(dim)
 CALL_VAL = [s_var1, s_dim, '']
 output=-1
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=LA_NORM(var1, /dim, /mean, /median)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 szv1 = size(var1)
 tv1 = szv1(szv1(0) + 1)
 ; check we have arithmetic types 
 IF (tv1 LT 1) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var1:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
 undef = la_undef(tv1)
 IF tv1 GT 3 THEN output = var1 ELSE output = float(var1)

;------------------------------------------------------------
; function body
;------------------------------------------------------------

 index = where(var1 eq undef, cpt)
 IF cpt GT 0 THEN output(index) = 0

 IF KEYWORD_SET(mean) THEN $
    div_array = LA_MEAN(abs(output), dim=dim) ELSE $
    IF KEYWORD_SET(median) THEN $
       div_array = LA_MEDIAN(abs(output), dim=dim) ELSE $
       div_array = LA_MAX(abs(output), dim=dim)

 output = LA_DIV(output, div_array)

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  
  s_var1 = CONV_STRING(var1)
  s_dim = CONV_STRING(dim)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_var1, s_dim, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
