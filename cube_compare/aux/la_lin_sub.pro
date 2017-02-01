FUNCTION LA_LIN_SUB, var1, var2, alpha, decal=decal
;+ 
; NAME: LA_LIN_SUB
; PURPOSE:
;   for each "image" plane of var1, compute :
;   var1(i) - alpha * var2(i) - (1 - alpha) * var2(i+1)
; CATEGORY: I-5-e
; CALLING SEQUENCE: 
;   output=LA_LIN_SUB(var1, var2  [, alpha] , decal=decal)
; INPUTS: 
;   var1     -- integer of float 3D array : supposed to be image planes
;   var2     -- same type and dim. : supposed to be reset planes
; OPTIONAL INPUT PARAMETERS: 
;   alpha    -- float between 0 to 1 : factor between successive reset planes
;               (default is 0) 
; KEYED INPUTS: 
;   decal    -- int (-1,0,1) : real index of var2 % var1 (default is 0)
;               if -1 var2(0) is supposed to correspond to var1(-1), var2(1) to
;               var1(0), etc
;               if 1 var2(0) correspond to var1(1), etc..
;               else var1 and var2 are supposed to be correctly synchronized
; OUTPUTS: 
;   output   -- integer or float 3D array : the effective plane number is about
;               the min number of var1 number and var2 number +/- decal
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE: 
;   ICE> print, la_lin_sub(indgen(3,2)+1, indgen(3,2))
;          1       1       1
;          1       1       1
;   ICE> var2(*,*,0)=indgen(3,2)
;   ICE> var2(*,*,1)=indgen(3,2)+1
;   ICE> print, la_lin_sub(indgen(3,2)+1, var2, 0.5)
;         -2      -2      -2
;         -2      -2      -2
; ALGORITHM:
;   .check var1 and var2 type and dimensions (2 or 3 D, same size on two first
;   dimension, numerical type)
;   if 2D, reform them with a third D set to 1
;   . check alpha and decal
;   . If decal eq 1, (var2(*,*,0) correspond to var1(*,*,1)), cut var1 first
;   plane, if decal eq -1, cut var2 first plane
;   . initialize output to array of size min of var1 and var2
;   . FOR all plane of output :
;     compute reset plane, either (i+1), i, or (alpha*i + (1-alpha)*(i+1))
;     subtract reset from var1 plane
; DEPENDENCIES: 
;   work on either lacunar or normal array
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS: 
;   WARNING : resize var1 and var2 to be 3D (if 2D) and cut them (if decal)
; RESTRICTIONS: 
;   UNTESTED
; CALLED PROCEDURES AND FUNCTIONS: 
;   LA_UNDE, LA_ADD, LA_SUB, LA_MUL
; MODIFICATION HISTORY: 
;    31-Mar-1995  written with template_gen                     FV IAS
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
 
 ROUTINE_NAME = 'LA_LIN_SUB'
 VERSION = '1.0' 
 CATEGORY = 'I-5-e'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', 'var2', 'alpha', '/decal', 'output']
  s_var1 = CONV_STRING(var1)
  s_var2 = CONV_STRING(var2)
  s_alpha = CONV_STRING(alpha)
  s_decal = CONV_STRING(decal)
 CALL_VAL = [s_var1, s_var2, s_alpha, s_decal, '']
 output=''
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output=LA_LIN_SUB(var1, var2  [, alpha] , decal=decal)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 ; type checking
  szv1= size(var1)
 tv1 = szv1(szv1(0) + 1)
 szv2= size(var2)
 tv2 = szv2(szv2(0) + 1)

 ; check we have arithmetic types (1 to 5: byte, int, long int, float or
 ; double)
 IF (tv1 LT 1) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var1:'+strtrim(tv1,2), 'E']
    GOTO, CLOSING
 ENDIF

 IF (tv2 LT 1) or (tv2 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var2:'+strtrim(tv2,2), 'E']
    GOTO, CLOSING
 ENDIF

 tout = (tv1 > tv2)
 undef = LA_UNDEF(tout)
 output = undef

 ; check dimensions
 IF szv1(0) eq 2 THEN var1 = reform(var1, szv1(1), szv1(2), 1, /overwrite)
 IF szv2(0) eq 2 THEN var2 = reform(var2, szv2(1), szv2(2), 1, /overwrite)
 IF szv1(0) eq 3 THEN n1 = szv1(szv1(0)) ELSE n1 = 1
 IF szv2(0) eq 3 THEN n2 = szv2(szv2(0)) ELSE n2 = 1
 
 IF szv1(0) LT 2 or szv1(0) GT 3 THEN BEGIN
    status(0) = ['Wrong dimension for var1:'+strtrim(szv1(0),2)+' while 3 '+$
                 'expected', 'E']
    GOTO, CLOSING
 ENDIF
 IF szv2(0) LT 2 or szv2(0) GT 3 THEN BEGIN
    status(0) = ['Wrong dimension for var2:'+strtrim(szv2(0),2)+' while 3 '+$
                 'expected', 'E']
    GOTO, CLOSING
 ENDIF

 IF szv2(1)ne szv1(1) or szv2(2) ne szv1(2) THEN BEGIN
    status(0) = ['Wrong size for var2:'+strtrim(szv2(1),2)+","+$
                 strtrim(szv2(2),2)+' while var1 :'+strtrim(szv1(1),2)+$
                 strtrim(szv1(2),2), 'E']
    GOTO, CLOSING
 ENDIF

 IF n_elements(alpha) eq 0 THEN alpha = 1
 IF alpha LT 0 or alpha GT 1 THEN BEGIN
    status(0) =['Out of bound variable : alpha ', 'E']
    goto, closing
 ENDIF

 ; compute nout from n1, n2 and decal
 IF n_elements(decal) eq 0 THEN decal = 0
 CASE decal OF
      1 : BEGIN & IF n1 GT 1 THEN b1=1 & b2=0 & n1 = n1-1 & END
     -1 : BEGIN & IF n2 GT 1 THEN b2=1 & b1=0 & n2 = n2-1 & END
   ELSE : BEGIN & b1=0 & b2=0 & END
 ENDCASE

 ; if i+1 plane required for i plane var2 should have one plane more than var1
 IF alpha eq 1 THEN nout = (n1 < n2) ELSE nout = (n1 < (n2-1))

 ; if proportion between past and next reset, then output type should
 ; be real
 IF alpha GT 0 and alpha LT 1 THEN tout = (tout > 4)

 IF nout LE 0 THEN BEGIN
    status(0) = ['Not enough data', 'E']
    GOTO, CLOSING
 ENDIF
  
;------------------------------------------------------------
; function body
;------------------------------------------------------------
 
 ; with current updates on var1 and var2, we can compute extactly plane (i)
 output = make_array(szv1(1), szv2(2), nout, type=tout)

 CASE alpha OF
      0 : output = LA_SUB(var1(*,*,b1:b1+nout-1), var2(*,*,b2+1:b2+nout))
      1 : output = LA_SUB(var1(*,*,b1:b1+nout-1), var2(*,*,b2:b2+nout-1))
   ELSE : output = LA_SUB(var1(*,*,b1:b1+nout-1), $
                          LA_ADD(LA_MUL(var2(*,*,b2:b2+nout-1), alpha), $
                                 LA_MUL(var2(*,*,b2+1:b2+nout), (1-alpha))))
 ENDCASE
  
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_var1 = CONV_STRING(var1)
  s_var2 = CONV_STRING(var2)
  s_alpha = CONV_STRING(alpha)
  s_decal = CONV_STRING(decal)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_var1, s_var2, s_alpha, s_decal, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
