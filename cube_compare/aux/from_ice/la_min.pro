FUNCTION LA_MIN, var1, min_pos, dim=dim, mask=mask
;+ 
; NAME: LA_MIN
; PURPOSE: 
;         compute min value without taking into account undefined value
; CATEGORY: I-5-c
; CALLING SEQUENCE: 
;   output=LA_MIN(var1, dim=dim, mask=mask)
; INPUTS: 
;   var1   -- array of arithmetic type
; OPTIONAL INPUT PARAMETERS:
;   min_pos -- long same size as output : min position into var1
; KEYED INPUTS: 
;   dim    -- dimension of output :
;           0 : return min of every elementary defined values (default)\\
;           1 : return vector with min values of every "planes". (with as
;               many elements as in the last dimension) \\
;          -1 : return min "plane" (one dimension less than var1)
;   mask   -- byte array : dimensions should be
;                if dim eq 0 : same as var1
;                if dim eq 1 : same as var1 but possibly 1 on last one
;                if dim eq -1 : same as var1 or vector
;              should follow SCD mask conventions : good values are 0 masked
;              values.
; OUTPUTS: 
;   output -- scalar or array (see dim) with same type as var1
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE:
;   ICE> print, la_min([[1, 2], [0, 3]])
;          0
;   ICE> print, la_min([[1, 2], [0, 3]], dim=1)
;              1           0
;   ICE> print, la_min([[1, 2], [0, 3]], dim=-1)
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
 
 ROUTINE_NAME = 'LA_MIN'
 VERSION = '1.0'
 CATEGORY = 'I-5-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', ' /dim ', 'output']
  s_var1 = CONV_STRING(var1)
  s_dim = CONV_STRING(dim)
 CALL_VAL = [s_var1, s_dim, '']
 output= la_undef()
 min_pos = -1l

;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=LA_MIN(var1, dim=dim)'
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
 
 ; check mask

 szm = size(mask)
 tm = szm(szm(0)+1)
 IF (tm GT 3) THEN BEGIN
    status(0) = ['Wrong type for mask:'+CONV_STRING(tm), 'E']
    GOTO, CLOSING
 ENDIF

 OK = 1
 FOR i=1,szm(0) DO OK = OK and (szm(i) eq szv1(i))
 IF (not ok) and ((dim ne -1) or (szm(0) ne 1) or (szm(1) ne szv1(szv1(0)))) $
 THEN BEGIN
    status(0) = ['WRONG dimensions for mask:'+CONV_STRING(szm), 'E']
    GOTO, CLOSING
 ENDIF
;------------------------------------------------------------
; function body
;------------------------------------------------------------

 IF n_elements(mask) LE 0 THEN mask = 0

 IF szv1(0) EQ 0 THEN BEGIN
    IF mask(0) eq 0 THEN output = var1
    GOTO, CLOSING
 ENDIF

 n_planes = szv1(szv1(0))
 nele1 = n_elements(var1) / n_planes
 CASE dim OF
      0 : BEGIN
            output = undef
            good_values = where((var1 ne undef) and (mask eq 0), cpt)
            IF cpt NE 0 THEN BEGIN
               output = MIN(var1(good_values), mp)
               min_pos = good_values(mp)
            ENDIF
          END
      1 : BEGIN
            output = replicate(undef, n_planes)
            min_pos = replicate(-1l, n_planes)
            IF szm(0) EQ szv1(0) -1 THEN mask0 = mask
            IF szm(0) EQ 0 THEN mask0 = 0
            FOR i = 0l, n_planes-1 DO BEGIN
                index1= i * nele1
                index2= (i+1) * nele1 - 1
                plane = var1(index1:index2)
                IF szm(0) eq szv1(0) THEN mask0 = mask(index1:index2)
                good_values = where((plane NE undef) and (mask0 eq 0), cpt)
                IF cpt GT 0 THEN BEGIN
                   output(i) = MIN(plane(good_values), mp)
                   min_pos(i) = index1 + good_values(mp)
                ENDIF
            ENDFOR
          END
       -1 : BEGIN
            odim = szv1(1:szv1(0)-1)
            output = make_array(dimension=odim, type=tv1, value=undef)
            min_pos = make_array(dimension=odim, value=-1l)
            IF szm(0) EQ 1 THEN mask0 = mask
            IF szm(0) EQ 0 THEN mask0 = 0
            FOR i = 0l, nele1-1 DO BEGIN
                index = indgen(n_planes) * nele1 + i
                var_array = var1(index)
                IF szm(0) eq szv1(0) THEN mask0 = mask(index)
                good_values = where((var_array NE undef) and (mask0 eq 0), cpt)
                IF cpt GT 0 THEN BEGIN
                   output(i) = MIN(var_array(good_values), mp)
                   min_pos(i) = index(good_values(mp))                   
                ENDIF
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
