FUNCTION LA_SKEW, var1, dim=dim, mask=mask, mean=x, sigma=y
;+ 
; NAME: LA_SKEW
; PURPOSE:
;   compute second moment of var1
;   undefined values are discarded from calculus
; CATEGORY: I-5-c
; CALLING SEQUENCE: 
;   skew=LA_SKEW(var1, dim=dim, mask=mask, mean=x, sigma=y)
; INPUTS: 
;   var1   -- array of arithmetic type
; OPTIONAL INPUT PARAMETERS:
;   none 
; KEYED INPUTS: 
;   dim    -- dimension of result :
;             0 : return skew of every elementary defined values (default)\\
;             1 : return vector with skew of every "planes". (with as
;                many elements as in the last dimension) \\
;            -1 : return skew "plane" (one dimension less than var1) 
;   mask   -- byte array : dimensions should be
;                if dim eq 0 : same as var1
;                if dim eq 1 : same as var1 but possibly 1 on last one
;                if dim eq -1 : same as var1 or vector
;              should follow SCD mask conventions : good values are 0 masked
;              values.
;   mean   -- mean value (with same size convention as for output) 
;   sigma  -- sigma (idem)
; OUTPUTS: 
;   skew   -- real, scalar, vector or array of one dim less than var1
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE:
;   ICE> print, la_skew([[1, 2], [0, 3]])
;          1
;   ICE> print, la_skew([[1, 2], [0, 3]], /dim)
;          0       1
;   ICE> print, la_skew([[1, 2], [0, 3]], dim=-1)
;          0       0
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;   none 
; RESTRICTIONS:
;   none
; CALLED PROCEDURES AND FUNCTIONS:       
;   LA_UNDEF
; MODIFICATION HISTORY: 
;   13-Feb-1995  written from LA_SIGMA             FV IAS
;-      
 
;------------------------------------------------------------
; common blocks 
;------------------------------------------------------------
 
; environment parameters 
 COMMON SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL
 
;------------------------------------------------------------
; on error condition
;------------------------------------------------------------
 ON_ERROR,  ERROR_CURRENT
 
;------------------------------------------------------------
; initialization
;------------------------------------------------------------
 
 ROUTINE_NAME = 'LA_SKEW'
 VERSION = '1.0' 
 CATEGORY = 'I-5-c'
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
   PRINT, 'CALLING SEQUENCE: output=LA_SKEW(var1, dim=dim, mask=mask)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 szv1= size(var1)
 tv1 = szv1(szv1(0) + 1)
 ; check we have an arithmetical type
 IF (tv1 LE 0) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var1:'+ strtrim(tv1,2), 'E']
    GOTO, CLOSING
 ENDIF
 undef = la_undef(tv1)

 IF n_elements(dim) eq 0 THEN dim = 0
 dim = dim(0)
 IF (dim NE 0) and (dim NE 1) and (dim NE -1) THEN BEGIN
    status(0) = ['Irrelevant value for dim '+strtrim(dim), 'E']
    GOTO, CLOSING
 ENDIF

 ; check mask
 IF n_elements(mask) eq 0 THEN mask=byte(var1) * 0
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

; general case skew is computed from all values in var1
 IF dim eq 0 THEN BEGIN 
    good_values = where((var1 NE undef) and (mask eq 0), cpt)
    output = la_undef(tv1 > 4)
    IF cpt eq n_elements(var1) THEN BEGIN
       x = total(var1) / cpt
       y = total((var1 - x)^2)
       y = sqrt(y / cpt)
       z = total((var1 - x)^3)
       IF (y GT 0.0001) THEN output(0) = z / (cpt * (y^3))
       GOTO, CLOSING
    ENDIF        
    IF cpt GT 0 THEN BEGIN
       x = total(var1(good_values)) / cpt
       y = total((var1(good_values) - x)^2)
       y = sqrt(y / cpt)
       z = total((var1(good_values) - x)^3)
       IF (y GT 0.0001) THEN output(0) = z / (cpt * (y^3))
    ENDIF
    GOTO, CLOSING
 ENDIF

; we compute one value by element of the last dim. (map on last dim)
 IF (dim EQ 1) THEN BEGIN
    IF szv1(0) LT 2 THEN GOTO, CLOSING
    n_planes = szv1(szv1(0))
    output = make_array(n_planes, value=la_undef(tv1 >4))
    x = output & y = output
    IF szm(0) EQ szv1(0) -1 THEN mask0 = mask
    IF szm(0) EQ 0 THEN mask0 = 0
    nele1 = n_elements(var1) / n_planes      ; number of elements
    FOR i =0 , n_planes-1 DO BEGIN
        var2 = var1(indgen(nele1) + i*nele1) ; value of interest
        IF szm(0) eq szv1(0) THEN mask0 = mask(indgen(nele1) + i * nele1)
        good_values = where(var2 NE undef and (mask0 eq 0), cpt)
        IF cpt GT 0 THEN BEGIN
           x(i) = total(var2(good_values)) / cpt
           y(i) = total((var2(good_values) - x(i))^2)
           y(i) = sqrt(y(i) / cpt)
           z = total((var2(good_values) - x(i))^3)
           IF (x(i) ne undef) and (y(i) ne undef) and (y(i) GT 0.0001) THEN $
               output(i) = z / (cpt * (y(i)^3))
        ENDIF
    ENDFOR
 END 

 ; one value by elements but on last dimension (map on every dim but the last
 ; one)
 IF (dim EQ -1) THEN BEGIN
    n_planes = szv1(szv1(0))
    nele1 = n_elements(var1) / n_planes         ; number of elements
    output = make_array(dim=szv1(1:szv1(0)-1), value=la_undef(tv1 > 4))
    x = output & y = output  
    IF szm(0) EQ 1 THEN mask0 = mask
    IF szm(0) EQ 0 THEN mask0 = 0
    FOR i =0 , nele1-1 DO BEGIN
        var2 = var1(indgen(n_planes)*nele1 + i) ; value of interest
        IF szm(0) eq szv1(0) THEN mask0 = mask(indgen(n_planes)*nele1 + i)
        good_values = where(var2 NE undef and (mask0 eq 0), cpt)
        IF cpt GT 0 THEN BEGIN
           x(i) = total(var2(good_values)) / cpt
           y(i) = total((var2(good_values) - x(i))^2)
           y(i) = sqrt(y(i) / cpt)
           z = total((var2(good_values) - x(i))^3)
           IF (x(i) ne undef) and (y(i) ne undef) and (y(i) GT 0.0001) THEN $
               output(i) = z / (cpt * (y(i)^3))
        ENDIF
    ENDFOR
 ENDIF

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
