FUNCTION LA_DISTM, var1, var2, propdef, undef_ne=undef_ne
;+ 
; NAME: LA_DISTM
; PURPOSE:
;          compute distance between arrays as max of absolute difference
;          between elementary values
;          treat undefined values as equal unless undef_neq set
;          return proportion of uncomparable values with propundef 
; CATEGORY: I-5-b
; CALLING SEQUENCE: 
;   output=LA_DISTM(var1, var2, propdef, undef_ne=undef_ne)
; INPUTS: 
;   var1, var2  -- array of same dimensions, sizes and types
; OPTIONAL INPUT PARAMETERS: 
;    propdef    -- double : proportion of comparable values
; KEYED INPUTS: 
;    undef_ne   -- 0/1 : if set consider undef not equal as undef
; OUTPUTS: 
;    output     -- same type as var1 and var2 : max of absolute value of
;                  differences
; OPTIONAL OUTPUT PARAMETERS:
;    none 
; EXAMPLE:
;    ICE> undef = la_undef()
;    ICE> help, la_distm([0,0,undef], [1,2,undef], propdef), propdef
;    <Expression>    INT       =        2
;    PROPDEF         DOUBLE    =        1.0000000
;    ICE> help, la_distm([0,0,undef],[1,2,undef],propdef,/undef_ne), propdef
;    <Expression>    INT       =        2
;    PROPDEF         DOUBLE    =       0.66666667
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
 
 ROUTINE_NAME = 'LA_DISTM'
 VERSION = '1.0'
 CATEGORY = 'I-5-b'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', 'var2', 'propdef', '/undef_ne', 'output']
 propdef = 0     
  s_var1 = CONV_STRING(var1)
  s_var2 = CONV_STRING(var2)
  s_propdef = CONV_STRING(propdef)
  s_undef_eq = CONV_STRING(undef_ne)
 CALL_VAL = [s_var1, s_var2,s_propdef, s_undef_eq, '']
 output = -1
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $
          'output=LA_DISTM(var1, var2 [, propdef, /undef_ne ])'
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
 output = undef

 IF (tv2 NE tv1) THEN BEGIN
    status(0) = ['Wrong type for var2:'+CONV_STRING(tv2), 'E']
    GOTO, CLOSING
 ENDIF

 ; check dimension consistency
 IF szv2(0) NE szv1(0) THEN BEGIN
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
 
 good_index = where((var1 NE undef) and (var2 NE undef), cpt)
 IF cpt GT 0 THEN output = max(abs(var1(good_index) - var2(good_index)))

 totvar = n_elements(var1)
 good_index = where((var1 eq undef) and (var2 eq undef), totundef)
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
