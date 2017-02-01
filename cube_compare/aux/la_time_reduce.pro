FUNCTION LA_TIME_REDUCE, cube, evolnames, mask=mask, dim2=dim2 
;+ 
; NAME: LA_TIME_REDUCE
; PURPOSE:
;   Compute time evolution of cube (third dimension is considered as time) 
; CATEGORY: I-5-e
; CALLING SEQUENCE: 
;   output = LA_TIME_REDUCE(cube  [, evolnames] , mask=mask, /dim2)
; INPUTS: 
;   cube       -- array of arithmetic type
; OPTIONAL OUTPUT PARAMETERS: 
;   evolnames  -- string array : parameters names in the same order as in cube
; KEYED INPUTS: 
;   mask       -- byte array of same size as cube : SCD-like mask (good values
;                 are set to 0)
;   dim2       -- if set and cube 2D array, second dimension is considered as
;                 time (defaulty cube would be considered as 3d array with last
;                 dim set to 1)
; OUTPUTS:
;   output     -- 8*as many planes as in cube real array : statistics on cube
;                 time evolution
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE: 
; ALGORITHM: 
; DEPENDENCIES: 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS: 
; RESTRICTIONS: 
;       UNTESTED
; CALLED PROCEDURES AND FUNCTIONS:
;   LA_UNDEF 
; MODIFICATION HISTORY: 
;    9-Apr-1995  written with template_gen                    FV IAS
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
 
 ROUTINE_NAME = 'LA_TIME_REDUCE'
 VERSION = '1.0' 
 CATEGORY = 'I-5-e'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['cube', 'evolnames', '/mask', '/dim2']
  s_cube = CONV_STRING(cube)
  s_evolnames = CONV_STRING(evolnames)
  s_mask = CONV_STRING(mask)
  s_no_skew = CONV_STRING(dim2)
 CALL_VAL = [s_cube, s_evolnames, s_mask, s_no_skew]
  evolnames = '' & output = -1
  szc = size(cube)
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output = LA_TIME_REDUCE(cube  [, evolnames , mask=mask, /dim2])'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 tc = szc(szc(0)+1)
 IF tc LE 1 or tc GT 5 THEN BEGIN
    status(0) = ['Wrong type for cube:'+strtrim(tc,2), 'E']
    GOTO, CLOSING
 ENDIF

 nele1 = szc(1) * szc(2)
 IF szc(0) EQ 2 THEN BEGIN 
 IF KEYWORD_SET(dim2) THEN BEGIN
    cube = reform(cube, [szc(1), 1, szc(2)], /overwrite)
    nplanes = szc(2)
    nele1 = szc(1)
 ENDIF ELSE BEGIN
    cube = reform(cube, [szc(1), szc(2), 1], /overwrite)
    nplanes = 1
 ENDELSE
 ENDIF ELSE nplanes=szc(szc(0))

 undef = la_undef(tc > 4)

 szm = size(mask)
 tm = szm(szm(0)+1)
 IF (tm GT 3) THEN BEGIN
    status(0) = ['Wrong type for mask:'+CONV_STRING(tm), 'E']
    GOTO, CLOSING
 ENDIF

 OK = 1
 FOR i=1,szm(0) DO OK = OK and (szm(i) eq szc(i))
 IF (not ok) and ((szm(0) ne 1) or (szm(1) ne szc(szc(0)))) $
 THEN BEGIN
    status(0) = ['WRONG dimensions for mask:'+CONV_STRING(szm), 'E']
    GOTO, CLOSING
 ENDIF

 output = make_array(7, nplanes, value=undef) 

;------------------------------------------------------------
; function body
;------------------------------------------------------------

 IF szm(0) EQ szc(0) -1 THEN mask0 = mask
 IF szm(0) EQ 0 THEN mask0 = 0

 FOR i=0l,nplanes-1 DO BEGIN
     var2 = cube(*,*,i)
     IF szm(0) eq szc(0) THEN mask0 = mask(indgen(nele1) + i * nele1)
     good_values = where(var2 NE undef and (mask0 eq 0), cpt)
     output(0,i) = cpt
     IF cpt GT 0 THEN BEGIN
        output(1,i) = MIN(var2(good_values))
        output(2,i) = MAX(var2(good_values))
        output(6,i) = MEDIAN(var2(good_values))
        ; mean
        output(3,i) = total(var2(good_values)) / cpt
        ; rms
        y = total((var2(good_values) - output(3,i))^2)
        output(4,i) = sqrt(y / cpt)
        ; skew
        IF output(4,i) GT 0.0001 THEN BEGIN
           y = total((var2(good_values) - output(3,i))^3)
           output(5,i) = y / (cpt * (output(4,i) ^3))
        ENDIF
      ENDIF
 ENDFOR

 evolnames = ['WEIGHT', 'MIN', "MAX", 'MEAN', 'SIGMA', 'SKEW', 'MEDIAN']

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 CLOSING:

 IF szc(0) eq 2 THEN cube = reform(cube, szc(1), szc(2), /overwrite)
 
  s_cube = CONV_STRING(cube)
  s_evolnames = CONV_STRING(evolnames)
  s_mask = CONV_STRING(mask)
  s_no_skew = CONV_STRING(dim2)
  ACTL_VAL = [s_cube, s_evolnames, s_mask, s_no_skew]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
