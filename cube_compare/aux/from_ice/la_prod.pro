FUNCTION LA_PROD, var1, dim=dim
;+ 
; NAME: LA_PROD
; PURPOSE: 
;   compute the product of values of var1 without using undefined value
; CATEGORY: I-5-c
; CALLING SEQUENCE: 
;   output=LA_PROD(var1, dim=dim)
; INPUTS: 
;   var1   -- array of arithmetic type
; OPTIONAL INPUT PARAMETERS:
;   none 
; KEYED INPUTS: 
;   dim    -- dimension of output :
;           0 : return product of every elementary defined values (default)\\
;           1 : return vector with product of every value within a "planes".
;               (with as many elements as in the last dimension) \\
;          -1 : return product "plane" (one dimension less than var1)
; OUTPUTS: 
;   output -- scalar or array (see dim) with same type as var1
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE:
;   ICE> print, la_prod([[1, 2], [0, 3]])
;          0
;   ICE> print, la_prod([[1, 2], [0, 3]], dim=1)
;              1           0
;   ICE> print, la_prod([[1, 2], [0, 3]], dim=-1)
;          0       2
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;   none 
; RESTRICTIONS: 
;   Overflow not checked
; CALLED PROCEDURES AND FUNCTIONS:
;    LA_UNDEF 
; MODIFICATION HISTORY: 
;    1-Aug-1994  written with template_gen         FV IAS
;    3-Oct-1994  V.1.0 for configuration control   FV IAS
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
 
 ROUTINE_NAME = 'LA_PROD'
 VERSION = '1.0'
 CATEGORY = 'I-5-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', ' /dim ', 'output']
  s_var1 = CONV_STRING(var1)
  s_dim = CONV_STRING(dim)
 CALL_VAL = [s_var1, s_dim, '']
 output=-1
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=LA_PROD(var1, dim=dim)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 szv1 = size(var1)
 tv1 = szv1(szv1(0)+1)

 ; check we have arithmetic type
 IF (tv1 LT 1) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var1:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
 undef = la_undef(tv1)
 output = undef

 IF n_elements(dim) eq 0 THEN dim = 0
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------

 IF szv1(0) EQ 0 THEN BEGIN
    output = var1
    GOTO, CLOSING
 ENDIF

 n_planes = szv1(szv1(0))
 nele1 = n_elements(var1) / n_planes
 CASE dim OF
      0 : BEGIN
            output = 1
            good_values = where(var1 ne undef, cpt)
            FOR j=0,cpt-1 DO output = output * var1(good_values(j))
            IF cpt LE 0 THEN output = undef
          END
      1 : BEGIN
            output = replicate(undef, n_planes)
            FOR i = 0l, n_planes-1 DO BEGIN
                index1= i * nele1
                index2= (i+1) * nele1 - 1
                plane = var1(index1:index2)
                good_values = where(plane NE undef, cpt)
                IF cpt GT 0 THEN output(i) = 1
                FOR j=0,cpt-1 DO $
                    output(i) = output(i) * plane(good_values(j))
            ENDFOR
          END
       -1 : BEGIN
            odim = szv1(1:szv1(0)-1)
            output = make_array(dimension=odim, type=tv1)
            FOR i = 0l, nele1-1 DO BEGIN
                index = indgen(n_planes) * nele1 + i
                var_array = var1(index)
                index = where(var_array NE undef, cpt)
                IF cpt GT 0 THEN output(i) = 1 ELSE output(i) = undef
                FOR j=0,cpt-1 DO $
                    output(i) = output(i) * var_array(index(j))
            ENDFOR
          END
        ELSE : status = ['Irrelevant key dim ' + CONV_STRING(dim), 'E']
   END
  
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
