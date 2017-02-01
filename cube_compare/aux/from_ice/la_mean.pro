FUNCTION LA_MEAN, var1, dim=dim, mask=mask
;+ 
; NAME: LA_MEAN
; PURPOSE: 
;       Return the mean value of the defined values of var1.
;       If mask given, used only non masked values
;       Mask should be standard SCD mask (good values are pointed by 0)
; CATEGORY: I-5-c
; CALLING SEQUENCE: 
;   output=LA_MEAN(var1, dim=dim, mask=mask)
; INPUTS: 
;   var1      -- array
; OPTIONAL INPUT PARAMETERS:
;   none
; KEYED INPUTS:
;   dim       -- dimension of required result \\
;                0 : return mean of every elementary defined values (default)\\
;                1 : return vector with mean values of every "planes". (with as
;                many elements as in the last dimension) \\
;               -1 : return mean "plane" (one dimension less than var1)
;                (ICCRED2 function mean_cube for a 3D var1)
;   mask      -- byte array : dimensions should be
;                if dim eq 0 : same as var1
;                if dim eq 1 : same as var1 but possibly 1 on last one
;                if dim eq -1 : same as var1 or vector
;                should follow SCD mask conventions : good values are 0 masked
;                values.
; OUTPUTS: 
;    output   -- float (or double) : mean 
; OPTIONAL OUTPUT PARAMETERS:
;    none 
; EXAMPLE:
;   ICE> print, la_mean([[1, 2], [0, 3]])
;          1.50000
;   ICE> print, la_mean([[1, 2], [0, 3]], /dim)
;          1.50000      1.50000
;   ICE> print, la_mean([[1, 2], [0, 3]], dim=-1)
;          0.500000      2.50000
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;   none 
; RESTRICTIONS:
;      WARNING: Overflow not checked 
; CALLED PROCEDURES AND FUNCTIONS:
;      LA_UNDEF 
; MODIFICATION HISTORY: 
;    8-Jul-1994  written with template_gen                  FV IAS
;    3-Oct-1994  V.1.0 for configuration control            FV IAS
;   13-Feb-1994  add KEY mask                       FV IAS
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
 
 ROUTINE_NAME = 'LA_MEAN'
 VERSION = '1.0'
 CATEGORY = 'I-5-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var1', 'output']
  s_var1 = CONV_STRING(var1)
 CALL_VAL = [s_var1, '']
 output=-1

;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=LA_MEAN(var1, dim=dim, mask=mask)'
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
 output = float(undef)
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

; scalar case
 IF szv1(0) EQ 0 THEN BEGIN
    IF n_elements(mask) LE 0 THEN mask = 0
    IF mask(0) EQ 0 THEN output = var1
    GOTO, CLOSING
 ENDIF


; array case
 n_planes = szv1(szv1(0)) 
 nele1 = n_elements(var1) / n_planes
 CASE dim OF
      0 : BEGIN
            IF n_elements(mask) LE 0 THEN mask = 0
            output = undef 
            good_values = where((var1 ne undef) and (mask eq 0), cpt)
            IF cpt GT 0 THEN $
               output = float(total(double(var1(good_values))) / cpt)
          END
      1 : BEGIN
            output = make_array(value=undef, n_planes, /float)
            IF szm(0) EQ szv1(0) -1 THEN mask0 = mask
            IF szm(0) EQ 0 THEN mask0 = 0
            FOR i = 0l, n_planes-1 DO BEGIN
                index1= i * nele1
                index2= (i+1) * nele1 - 1
                plane = var1(index1:index2)
                IF szm(0) eq szv1(0) THEN mask0 = mask(index1:index2)
                good_values = where((plane NE undef) and (mask0 eq 0), cpt)
                IF cpt GT 0 THEN $
                   output(i) = float(total(plane(good_values))) / cpt $
                ELSE output(i) = undef 
            ENDFOR
          END
       -1 : BEGIN
            IF nele1 GT 1 THEN BEGIN
               odim = szv1(1:szv1(0)-1) 
               output = make_array(dimension=odim, /float)
            ENDIF ELSE output = undef
            IF szm(0) EQ 1 THEN mask0 = mask
            IF szm(0) EQ 0 THEN mask0 = 0
            FOR i = 0l, nele1-1 DO BEGIN
;JPB on Jan 12 2016: replaced indgen by lindgen
                ;index = indgen(n_planes) * nele1 + i
                index = lindgen(n_planes) * nele1 + i
                vector = var1(index)
                IF szm(0) eq szv1(0) THEN mask0 = mask(index)
                values = where(vector NE undef and (mask0 eq 0), cpt)
                IF cpt GT 0 THEN output(i) = total(vector(values)) / cpt $
                ELSE output(i) = undef
            ENDFOR
          END
        ELSE : status = ['Irrelevant value for key "dim" :' + $
                         CONV_STRING(dim), 'E']
  END
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  
  s_var1 = CONV_STRING(size(var1))
  s_output = CONV_STRING(size(output))
  ACTL_VAL = [s_var1, s_output]

  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
