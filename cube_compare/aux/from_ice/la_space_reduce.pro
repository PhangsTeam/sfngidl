FUNCTION LA_SPACE_REDUCE, cube, names, mask=mask, dim2=dim2 
;+ 
; NAME: LA_SPACE_REDUCE
; PURPOSE:
;   Reduce cube following time (third dimension is considered as time), in
;   order to get mean cube, rms and weight 
; CATEGORY: I-5-e
; CALLING SEQUENCE: 
;   output = LA_SPACE_REDUCE(cube  [, names] , mask=mask, /dim2)
; INPUTS: 
;   cube       -- array of arithmetic type
; OPTIONAL OUTPUT PARAMETERS: 
;   names      -- string array : parameters names in the same order as in cube
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
;   ICE> print, la_space_reduce(indgen(4,1,10))
;        18.0000      19.0000        20.0000      21.0000
;
;        11.4891      11.4891        11.4891      11.4891
;
;        10.0000      10.0000        10.0000      10.0000
; ALGORITHM:
;   check type and dimensions
;   call la_sigma, and get rms, mean and ndef 
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
;   LA_SIGMA 
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
 
 ROUTINE_NAME = 'LA_SPACE_REDUCE'
 VERSION = '1.0' 
 CATEGORY = 'I-5-e'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['cube', 'names', '/mask', '/dim2']
  s_cube = CONV_STRING(cube)
  s_names = CONV_STRING(names)
  s_mask = CONV_STRING(mask)
  s_no_skew = CONV_STRING(dim2)
 CALL_VAL = [s_cube, s_names, s_mask, s_no_skew]
  names = ''
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output = LA_SPACE_REDUCE(cube  [, names , mask=mask, /dim2])'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 szc = size(cube)
 tc = szc(szc(0)+1)
 IF tc LE 1 or tc GT 5 THEN BEGIN
    status(0) = ['Wrong type for cube:'+strtrim(tc,2), 'E']
    GOTO, CLOSING
 ENDIF
 n1 = szc(1)
 n2 = szc(2)

 IF szc(0) EQ 2 and KEYWORD_SET(dim2) THEN n2 = 1

 undef = la_undef(tc > 4)

 output = make_array(n1, n2, 3, value=undef) 

;------------------------------------------------------------
; function body
;------------------------------------------------------------

 IF szc(0) eq 2 and not KEYWORD_SET(dim2) THEN BEGIN
    output(*,*,1)=0
    output(*,*,0)=cube
    ; test undefined values for weight
    weight = cube*0 + 1
    index = where(cube eq la_undef(tc),cpt)
    IF cpt GT 0 THEN weight(index) = 0
    output(*,*,2) = weight
 ENDIF ELSE BEGIN
    output(*,*,1) = LA_SIGMA(cube, mask=mask, dim=-1, mean=x,ndef=ndef)
    output(*,*,0) = x 
    output(*,*,2) = ndef
 ENDELSE
 names = ['MEAN', 'SIGMA', 'WEIGHT']

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_cube = CONV_STRING(cube)
  s_names = CONV_STRING(names)
  s_mask = CONV_STRING(mask)
  s_no_skew = CONV_STRING(dim2)
  ACTL_VAL = [s_cube, s_names, s_mask, s_no_skew]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
