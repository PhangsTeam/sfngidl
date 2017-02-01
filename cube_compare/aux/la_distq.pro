FUNCTION LA_DISTQ, var1, var2, propdef, undef_ne=undef_ne
;+ 
; NAME: LA_DISTQ
; PURPOSE:
;          compute distance between arrays as square root of square differences
;          between elementary values 
;          treat two undefined values as equal unless undef_ne, set
;          return proportion of comparable values with propdef 
; CATEGORY: I-5-b
; CALLING SEQUENCE: 
;   output=LA_DISTQ(var1, var2, propdef, /undef_ne)
; INPUTS: 
;   var1, var2 -- arrays of same dimensions, sizes and types 
; OPTIONAL INPUT PARAMETERS:
;   none 
; KEYED INPUTS: 
;   undef_eq   -- 0/1 : if set consider undef not equal as undef for propdef
; OUTPUTS: 
;   output     -- float (or double) : distance between var1 and var2
; OPTIONAL OUTPUT PARAMETERS: 
;   propdef    -- double : proportion of comparable values
; EXAMPLE:
;   ICE> undef = la_undef()
;   ICE> help, la_distq([0,0,undef], [1,2,undef], propdef), propdef
;   <Expression>    FLOAT     =       2.23607
;   PROPDEF         DOUBLE    =        1.0000000
;   ICE> help, la_distq([0,0,undef], [1,2,undef], propdef, /undef_ne), propdef
;   <Expression>    FLOAT     =       2.23607
;   PROPDEF         DOUBLE    =       0.66666667
; ALGORITHM:
;    straightforward
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
;    8-Jul-1994  written with template_gen                  FV IAS
;    7-Oct-1994  V.1.0 for configuration control            FV IAS
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
 
 ROUTINE_NAME = 'LA_DISTQ'
 VERSION = '1.0'
 CATEGORY = 'I-5-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', 'var2', 'propdef', '/undef_ne', 'output']
 propdef = 0.
  s_var1 = CONV_STRING(size(var1))
  s_var2 = CONV_STRING(size(var2))
  s_propdef = CONV_STRING(propdef)
  s_undef_ne = CONV_STRING(undef_ne)
 CALL_VAL = [s_var1, s_var2,s_propdef, s_undef_ne, '']
 output= - 1
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output=LA_DISTQ(var1, var2 [, propdef, /undef_ne])'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
 szv1= size(var1)
 tv1 = szv1(szv1(0) + 1)
 szv2= size(var2)
 tv2 = szv2(szv2(0) + 1)

 ; check we have arithmetic types
 IF (tv1 LT 1) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var1:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF

 undef = la_undef(tv1)
 output = la_undef(4)

 IF (tv2 NE tv1) THEN BEGIN
    status(0) = ['Wrong type for var2:'+CONV_STRING(tv2), 'E']
    GOTO, CLOSING
 ENDIF

 ; check dimension consistency
 IF (szv2(0) NE szv1(0)) THEN BEGIN
    status(0) = ['Wrong dimension for var2:'+strtrim(szv2(0),2)+' versus '+$
                 strtrim(szv1(0),2), 'E']
    GOTO, CLOSING
 ENDIF
 ok = 1
 FOR i=1, szv2(0) DO ok = ok and (szv1(i) eq szv2(i))

 IF not OK THEN BEGIN
    status(0) = ['Wrong size for var2:'+CONV_STRING(szv2)+ ' versus var1: '+$
                 CONV_STRING(szv1), 'E']
    GOTO, CLOSING
 ENDIF
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------
 
 index = where((var1 NE undef) and (var2 NE undef), cpt)
 IF cpt GT 0 THEN $
    output = sqrt(total( (var1(index) - var2(index)) ^ 2 ))
   
 totvar = n_elements(var1)
 index = where((var1 eq undef) and (var2 eq undef), totundef)
 IF KEYWORD_SET(undef_ne) THEN propdef = double(cpt) / double(totvar) $
 ELSE BEGIN
      propdef = double(cpt + totundef) / double(totvar)
      IF totundef EQ totvar THEN output = 0.
 ENDELSE

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
 
  s_var1 = CONV_STRING(size(var1))
  s_var2 = CONV_STRING(size(var2))
  s_propdef = CONV_STRING(propdef)
  s_undef_ne = CONV_STRING(undef_ne)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_var1, s_var2, s_propdef, s_undef_ne, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
