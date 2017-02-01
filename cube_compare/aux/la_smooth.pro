FUNCTION LA_SMOOTH, data, nsmooth, dim=dim, mask=mask, $
                    edge_truncate=edge_truncate
;+ 
; NAME: LA_SMOOTH
; PURPOSE: 
;    Splitting mean on successive nsmooth values (like smooth function)
;    taking undefined values into account
;    If /dim compute it following last dim
; CATEGORY: III-5-c
; CALLING SEQUENCE: 
;   output=LA_SMOOTH(data, nsmooth, dim=dim, mask=mask)
; INPUTS: 
;   data,   -- integer or real array of any dim 
;   nsmooth -- integer 
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   dim     -- 0/(1 or -1) : if 1 (or -1), data is considered as a cube and
;              smooth is comptued on all pixels (default is 0) 
;   mask    -- byte array : if given non zero masked values are considered
;              as undefined              
;   edge_truncate    --  SMOOTH EDGE_TRUNCATE keyword
;
; OUTPUTS: 
;    output -- 1 or 2D array, same type as data, same total of elements : 
;              smoothed data
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE: 
;   ICE>  print, la_smooth([2., 0., 1., 2.], 3)
;         2.00000      1.00000      1.00000      2.00000
;   ICE>  print, la_smooth([2., undef, 1., 2.], 3)
;         2.00000      1.50000      1.50000      2.00000
;   ICE>  print, la_smooth([[2., 2.],[undef, 0.],[1.,1.]], 3, /dim)
;         2.00000      2.00000
;         1.50000      1.00000
;         1.00000      1.00000
; ALGORITHM: 
;   Check data and nsmooth
;   If no dim, set it to 0
;   If dim eq 0 THEN
;   . set undefined values to 0
;   . apply smooth function
;   . get data weight as 1 for all defined values else 0
;   . apply smooth on weight
;   . correct output dividing by smoothed weight and reset undefined values
;   to undefined
;  Else select successive "plane" and apply same sequence on them 
; DEPENDENCIES: 
;   none
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS: 
;   none
; RESTRICTIONS: 
;   nsmooth must be between 2 and data size
; CALLED PROCEDURES AND FUNCTIONS: 
;   LA_UNDEF
;   IDL routine SMOOTH
; MODIFICATION HISTORY: 
;    3-Nov-1995  written with template_gen          FV/FXD    IAS
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
 
 ROUTINE_NAME = 'LA_SMOOTH'
 VERSION = '1.0' 
 CATEGORY = 'III-5-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['data', 'nsmooth', '/dim', '/mask', 'output']
  s_data = CONV_STRING(data)
  s_nsmooth = CONV_STRING(nsmooth)
  s_dim = CONV_STRING(dim)
  s_mask = CONV_STRING(mask)
 CALL_VAL = [s_data, s_nsmooth, s_dim, s_mask, '']
 output= -1
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output=LA_SMOOTH(data, nsmooth, dim=dim, mask=mask)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
 sdata= size( data) & td = sdata(sdata(0)+1)
 if td LT 1 or td GT 5 or sdata(0) eq 0 then BEGIN
    status(0) = ['Wrong type or size data', 'E']
    GOTO, CLOSING
 ENDIF
 n_planes = sdata(sdata(0))
 nele = n_elements(data)
 nele1 = nele /n_planes

 sns= size(nsmooth) & tn = sns(sns(0)+1)
 if tn LT 1 or tn GT 5 or sns(0) ne 0 then BEGIN
    status(0) = ['Wrong type or size for nsmooth', 'E']
    GOTO, CLOSING
 ENDIF

 IF nsmooth LE 1 or nsmooth ge nele THEN BEGIN
    status(0) = ['Width must be > 2 and < array dim', 'E']
    GOTO, CLOSING
 ENDIF

 IF n_elements(mask) GT 0 THEN IF n_elements(mask) ne sdata(sdata(0)+2) $
 THEN BEGIN
    status(0) = ['Inconsistent mask', 'E']
    GOTO, CLOSING
 ENDIF

 IF n_elements(dim) eq 0 THEN dim=0

 IF not keyword_set(edge_truncate) then edge_truncate=0

;------------------------------------------------------------
; function body
;------------------------------------------------------------
 
 undef= la_undef(td > 4)

 ; dim eq 0 result is formated as 1D array
 IF dim eq 0 THEN BEGIN
    inter= reform(data, nele)

    ; set undefined values to 0
    u=where(inter eq undef, nu)
    if nu gt 0 then inter(u)=0.
    inter= SMOOTH(temporary(inter), nsmooth, edge_truncate=edge_truncate)

    ; get real weight and compute correction factor
    weight= inter*0.+1.
    if nu gt 0 then weight(u)= 0.
    wout= SMOOTH(temporary(weight), nsmooth)

    ; correct weight for undefined values
    output = replicate(undef, nele)
    v= where( wout ne 0., nv)
    if nv ne 0 then output(v)= temporary(inter(v))/ temporary(wout(v))
 ENDIF ELSE BEGIN

 ; dim eq 1 or -1, result is formated as a 2D array, last dim of data
 ; is saved
    output = replicate(undef, nele1, n_planes)
    FOR i=0l,nele1-1 DO BEGIN
        inter= reform(data(indgen(n_planes)*nele1 + i))

        ; set undefined values to 0
        u=where(inter eq undef, nu)
        if nu gt 0 then inter(u)=0.
        inter= SMOOTH(temporary(inter), nsmooth)

        ; get real weight and compute correction factor
        weight= inter*0.+1.
        if nu gt 0 then weight(u)= 0.
        wout= SMOOTH(temporary(weight), nsmooth)

        ; correct weight for undefined values
        v= where( wout ne 0., nv)
        if nv ne 0 then output(i,v)= temporary(inter(v))/ temporary(wout(v))
    ENDFOR
ENDELSE 
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_data = CONV_STRING(data)
  s_nsmooth = CONV_STRING(nsmooth)
  s_dim = CONV_STRING(dim)
  s_mask = CONV_STRING(mask)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_data, s_nsmooth, s_dim, s_mask, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
