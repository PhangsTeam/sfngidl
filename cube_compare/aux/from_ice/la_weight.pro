FUNCTION LA_WEIGHT, var1, dim=dim, mask=mask
;+ 
; NAME: LA_WEIGHT
; PURPOSE:
;   compute number of defined values (weigth) inside a lacunar array
;   If mask given extract compute it only inside good values of mask  
; CATEGORY: I-5-c
; CALLING SEQUENCE: 
;   output=LA_WEIGHT(var1, dim=dim, mask=mask)
; INPUTS: 
;   var1
; OPTIONAL INPUT PARAMETERS:
;   none 
; KEYED INPUTS: 
;   dim     -- dimension of the result 
;                0 : return number of every elementary defined values (default)
;                1 : return vector with number of defined values of every
;                    "planes". (with as many elements as in the last dimension)
;               -1 : return numebr of defined values following last dimension
;                    (a "plane" -- one dimension less than var1)
;   mask    -- byte array : dimensions should be
;                if dim eq 0 : same as var1
;                if dim eq 1 : same as var1 but possibly 1 on last one
;                if dim eq -1 : same as var1 or vector 
;              should follow SCD mask conventions : good values are 0 masked
;              values.
; OUTPUTS: 
;  output   -- long int : weight 
; OPTIONAL OUTPUT PARAMETERS:
;  none 
; EXAMPLE:
;  ICE> print, la_weight([[1, 2], [0, 3]])
;         3
;  ICE> print, la_weight([[1, 2], [0, 3]], dim=1)
;             2           3
;  ICE> print, la_weight([[1, 2], [0, 3]], dim=-1)
;             1           3
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;   none 
; RESTRICTIONS: 
;   the routine la_undef should be up to date
; CALLED PROCEDURES AND FUNCTIONS: 
;    LA_UNDEF
; MODIFICATION HISTORY: 
;   13-Feb-1994  written from LA_MAX             FV IAS
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
 
 ROUTINE_NAME = 'LA_WEIGHT'
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
   PRINT, 'CALLING SEQUENCE: output=LA_WEIGHT(var1, dim=dim)'
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
 output = 0

 IF n_elements(dim) eq 0 THEN dim = 0

 ; check mask
 IF n_elements(mask) eq 0 THEN mask = byte(var1) * 0
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
    IF var1(0) NE undef and mask(0) eq 0 THEN output = 1
    GOTO, CLOSING
 ENDIF

 n_planes = szv1(szv1(0))
 nele1 = n_elements(var1) / n_planes
 CASE dim OF
      0 : BEGIN
            good_values = where((var1 ne undef) and (mask eq 0), cpt)
            output = cpt
          END
      1 : BEGIN
            output = replicate(0, n_planes)
            IF szm(0) EQ szv1(0) -1 THEN mask0 = mask
            IF szm(0) EQ 0 THEN mask0 = 0
            FOR i = 0l, n_planes-1 DO BEGIN
                index1= i * nele1
                index2= (i+1) * nele1 - 1
                plane = var1(index1:index2)
                IF szm(0) eq szv1(0) THEN mask0 = mask(index1:index2)
                good_values = where((plane NE undef) and (mask0 eq 0), cpt)
                output(i) = cpt
            ENDFOR
          END
       -1 : BEGIN
            odim = szv1(1:szv1(0)-1)
            output = make_array(dimension=odim, value=0)
            IF szm(0) EQ 1 THEN mask0 = mask
            IF szm(0) EQ 0 THEN mask0 = 0
            FOR i = 0l, nele1-1 DO BEGIN
                index = indgen(n_planes) * nele1 + i
                var_array = var1(index)
                IF szm(0) eq szv1(0) THEN mask0 = mask(index)
                index = where((var_array NE undef) and (mask0 eq 0), cpt)
                output(i) = cpt              
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
