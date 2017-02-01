FUNCTION LA_MUL, var1, var2
;+ 
; NAME: LA_MUL
; PURPOSE:
;          mulpiply two cubes, or a cube and a scalar or a cube and an image
;          (like IDL '*' taking into account undefined values)
;          same function as ICCRED2 multi_cube_...
; CATEGORY: I-5-b
; CALLING SEQUENCE: 
;   output=LA_MUL(var1, var2)
; INPUTS: 
;   var1   -- array
;   var2   -- either scalar or array with compatible dimension
; OPTIONAL INPUT PARAMETERS:
;   none 
; KEYED INPUTS:
;   none
; OUTPUTS:
;   output  -- array of same type and dimension than var1
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE:
;   ICE> undef = la_undef()
;   ICE>  print, la_mul([6, 3, 2, 1], 3)
;         18       9       6       3
;   ICE> print, la_mul([6, 3, 2, undef], [2, 3, 0, 1])
;         12       9       0  -32768
;   ICE> print, la_mul([[6, 3, 2, 1], [undef, 6, 0, 0]], [2, 3, 0, 1])
;         12       6       4       2
;     -32768      18       0       0
;   ICE> print, la_mul([[6,3,2,1], [8,6,0,0]], [undef, 3])
;     -32768  -32768  -32768  -32768
;         24      18       0       0
; ALGORITHM:
;   check var2 size
;   if var1 and var2 have same size and dimensions then la_mul arrays value
;      by value
;   if var2 is a scalar then la_mul every value of var1 by var2
;   if var2 is a vector of n elements and var1 an array with n elements in its
;      most internal dimension, then la_mul every "plane" of var1 by var2(i)
;   if var2 has one dimension less than var1, then la_mul value by value,
;   every "plane" of var1 by var2
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;   none 
; RESTRICTIONS: 
;   No overflow check
; CALLED PROCEDURES AND FUNCTIONS: 
;    LA_UNDEF 
; MODIFICATION HISTORY: 
;    8-Jul-1994  written with template_gen              FV IAS
;    7-Oct-1994  V.1.0 for configuration control        FV IAS
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
 
 ROUTINE_NAME = 'LA_MUL'
 VERSION = '1.0'
 CATEGORY = 'I-5-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', 'var2', 'output']
  s_var1 = CONV_STRING(size(var1))
  s_var2 = CONV_STRING(size(var2))
 CALL_VAL = [s_var1, s_var2, '']
 output=-1l
  
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=LA_MUL(var1, var2)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
 szv1= size(var1)
 tv1 = szv1(szv1(0) + 1)
 szv2= size(var2)
 tv2 = szv2(szv2(0) + 1)

 ; check we have arithmetic types (1 to 5: byte, int, long int, float or
 ; double)
 IF (tv1 LT 1) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var1:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF

 undef = la_undef(tv1)
 output = undef

 IF (tv2 NE tv1) and ((tv2 LE 1) or (tv2 GT 5)) THEN BEGIN
    status(0) = ['Wrong type for var2:'+CONV_STRING(tv2), 'E']
    GOTO, CLOSING
 ENDIF

 ; check dimension consistency
 IF szv2(0) GT szv1(0) THEN BEGIN
    status(0) = ['Wrong dimension for var2:'+strtrim(szv2(0),2)+' versus '+$
                 strtrim(szv2(0),2), 'E']
    GOTO, CLOSING
 ENDIF

 ok = 1
 FOR i=1, szv2(0) DO ok = ok and (szv1(i) eq szv2(i))
 ok = ok or ((szv2(0) EQ 1) and (szv2(1) EQ szv1(szv1(0)) ) )
 IF not OK THEN BEGIN
    status(0) = ['Wrong size for var2:'+CONV_STRING(szv2)+ ' versus var1: '+$
                 CONV_STRING(szv1), 'E']
    GOTO, CLOSING
 ENDIF

 ; initialize output to undefined cube
 undef = la_undef(tv1 > tv2)
 output = undef

 IF szv1(0) GT 0 THEN output =make_array(dim=szv1(1:szv1(0)), value=undef)

;------------------------------------------------------------
; function body
;------------------------------------------------------------
 IF szv2(0) EQ szv1(0) THEN BEGIN
    ; update output only on defined values
    good_values = where((var1 eq undef) or (var2 eq undef), cpt)
    IF cpt EQ 0 THEN BEGIN & output = var1*var2 & GOTO, CLOSING & ENDIF
    good_values = where((var1 ne undef) and (var2 ne undef), cpt)
    IF cpt GT 0 THEN output(good_values) = var1(good_values)*var2(good_values)
    GOTO, CLOSING
 ENDIF
 
 IF (szv2(0) EQ 0) and (var2(0) NE undef) THEN BEGIN
    good_values = where(var1 eq undef, cpt)
    IF cpt EQ 0 THEN BEGIN & output = var1*var2 & GOTO, CLOSING & ENDIF
    good_values = where(var1 ne undef, cpt)
    IF cpt ne 0 THEN output(good_values) = var1(good_values) * var2(0)
    GOTO, CLOSING
 ENDIF

 IF (szv2(0) EQ 1) and (szv2(1) EQ szv1(szv1(0))) THEN BEGIN
    bad_v1 = where(var1 eq undef, cpt)
    n_planes = szv1(szv1(0))
    nele1 = n_elements(var1) / n_planes ; number of elements within one "image"
    FOR i = 0l, n_planes-1 DO BEGIN
        index1= i * nele1
        index2= (i+1) * nele1 - 1
        IF var2(i) NE undef THEN $
           output(index1:index2) = var1(index1:index2) * var2(i)
    ENDFOR
    IF cpt NE 0 THEN output(bad_v1) = undef
    GOTO, CLOSING
 ENDIF
 
 IF szv2(0) EQ szv1(0) - 1 THEN BEGIN
    bad_v2 = where(var2 eq undef, cpt2)
    n_planes = szv1(szv1(0))
    nele1 = n_elements(var1) / n_planes
    FOR i = 0l, n_planes-1 DO BEGIN
        index1= i * nele1
        index2= (i+1) * nele1 - 1
        plane = var1(index1:index2)
        good_v = where((plane ne undef) and (var2 ne undef), cpt)
        IF cpt GT 0 THEN plane(good_v) = plane(good_v) * var2(good_v)
        IF cpt2 gt 0 THEN plane(bad_v2) = undef
        output(index1:index2) = plane
    ENDFOR
 ENDIF
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:

  s_var1 = CONV_STRING(size(var1))
  s_var2 = CONV_STRING(size(var2))
  s_out = CONV_STRING(size(output))
  ACTL_VAL = [s_var1, s_var2, s_out]

  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
