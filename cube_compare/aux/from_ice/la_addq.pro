FUNCTION LA_ADDQ, var1, var2
;+ 
; NAME: LA_ADDQ
; PURPOSE: 
;       compute SQRT (var2 ^2 + var1^2 ).
;       The operation is not applied to the undefined values.
; CATEGORY: I-5-b
; CALLING SEQUENCE: 
;   output=LA_ADDQ(var1, var2)
; INPUTS: 
;   var1, var2 -- arrays of compatible dimensions and of type integer or real
;                 (bytes are not allowed) 
; OPTIONAL INPUT PARAMETERS:
;   none 
; KEYED INPUTS:
;   none
; OUTPUTS: 
;   output    -- float (or double) : array of same size and dimensions than
;                var1, type is defaultly float, double if either var1 or var2
;                is of double type
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE:
;   ICE>  print, la_addq([3, 0, 2, 1], 4)
;         3.00000      4.00000      4.47214      4.12311
;   ICE>  print, la_addq([3, 0, 2, 1], [0,4,0,0])
;         3.00000      4.00000      2.00000      1.00000
;   ICE> print, la_addq([[3, 0, 0], [3, 0, 0]], [4,0])
;         5.00000      4.00000      4.00000
;         3.00000     0.000000     0.000000
;   ICE> print, la_addq([[3, 0, 0], [3, 0, 0]], [4,0,0])
;         5.00000     0.000000     0.000000
;         5.00000     0.000000     0.000000
; ALGORITHM:
;   test var2 size
;   if var2 is a scalar, compute SQRT (var1(i) ^2 + var2^2 ) on every value
;   if var2 have same size and dimension than var1 compute
;      SQRT (var1(i) ^2 + var2(i)^2 )
;   if var2 is a vector compute SQRT (var1(*,...,*,i) ^2 + var2(i)^2 )
;   if var2 has one dimension less than var1 compute
;      SQRT (var1(*,...,*,i) ^2 + var2(*,...,*)^2 )
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
;   11-Jul-1994  written with template_gen        FV IAS
;    3-Oct-1994  V.1.0 for configuration control  FV IAS
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
 
 ROUTINE_NAME = 'LA_ADDQ'
 VERSION = '1.0'
 CATEGORY = 'III-1'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', 'var2', 'output']
  s_var1 = CONV_STRING(size(var1))
  s_var2 = CONV_STRING(size(var2))
 CALL_VAL = [s_var1, s_var2, '']
 output= - 1
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=LA_ADDQ(var1, var2)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
 szv1= size(var1)
 tv1 = szv1(szv1(0) + 1)
 szv2= size(var2)
 tv2 = szv2(szv2(0) + 1)

 ; check we have arithmetic types (2 to 5: int, long, float or double)
 IF (tv1 LT 1) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var1:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
 if tv1 LE 3 THEN szv1(szv1(0)+1) = 4

 IF (tv2 ne tv1) and  ((tv2 LE 1) or (tv2 GT 5)) THEN BEGIN
    status(0) = ['Wrong type for var2:'+CONV_STRING(tv2), 'E']
    GOTO, CLOSING
 ENDIF

 undef = la_undef((4 > tv1) > tv2)   ; float or double undefined value

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
 IF szv1(0) NE 0 THEN output=make_array(dim=szv1(1:szv1(0)),value=undef)

;------------------------------------------------------------
; function body
;------------------------------------------------------------
; variables of same dimensions
 IF szv2(0) EQ szv1(0) THEN BEGIN
    ; update output only on defined values
    good_values = where((var1 ne undef) and (var2 ne undef), cpt)
    IF cpt ne 0 THEN $
       output(good_values) = sqrt( float(var1(good_values))^2 $
                                  +float(var2(good_values))^2 )
    GOTO, CLOSING
 ENDIF

; one array variable and one scalar value
 IF (szv2(0) EQ 0) and (var2(0) NE undef) THEN BEGIN
    good_values = where(var1 ne undef, cpt)
    IF cpt ne 0 THEN $
       output(good_values) = sqrt( float(var1(good_values))^2 $
                                 + float(var2(0))^2 )
    GOTO, CLOSING
 ENDIF

; one dimension for var2 
; we add a scalar value, var2(i) on every instance of last dimension
; (output(*...*,i) <- var1(*...*,i)^2+var2(i)^2 -- mapping de l'addition
; quadratique d'une cste sur le "cube" avec -1 dimension
 IF (szv2(0) EQ 1) and ( szv2(1) eq szv1(szv1(0))) THEN BEGIN
    bad_v1 = where(var1 eq undef, cpt)
    n_planes = szv1(szv1(0))
    nele1 = n_elements(var1) / n_planes ; number of elements within one "image"
    FOR i = 0l, n_planes-1 DO BEGIN
        index1= i * nele1
        index2= (i+1) * nele1 - 1
        IF var2(i) NE undef THEN $
           output(index1:index2) = sqrt( float(var1(index1:index2))^2 $
                                       + float(var2(i)) ^2)
    ENDFOR
    IF cpt NE 0 THEN output(bad_v1) = undef
    GOTO, CLOSING
 ENDIF

; just one dimension less (previous addquad_cube_im), we add var2 on every
; instance of last dimension (output(*...*,i) <- var1(*...*,i)^2+var2(*...*)^2
 IF szv2(0) EQ szv1(0) -1 THEN BEGIN
    bad_v1 = where(var1 eq undef, cpt)
    bad_v2 = where(var2 eq undef, cpt2)
    n_planes = szv1(szv1(0))
    nele1 = n_elements(var1) / n_planes
    FOR i = 0l, n_planes-1 DO BEGIN
        index1= i * nele1
        index2= (i+1) * nele1 - 1
        plane = sqrt(float(var1(index1:index2))^2 + float(var2)^2)
        IF cpt2 GT 0 THEN plane(bad_v2) = undef
        output(index1:index2) = plane
    ENDFOR
    IF cpt GT 0 THEN output(bad_v1) = undef
 ENDIF
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
 
  s_var1 = CONV_STRING(var1)
  s_var2 = CONV_STRING(var2)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_var1, s_var2, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
