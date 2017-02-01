FUNCTION LA_HISTO, var, X, binsize=binsize, max=max, min=min, $
                   omax=omax, omin=omin, reverse_indices=reverse_indices
;+ 
; NAME: LA_HISTO
; PURPOSE:
;   apply IDL function HISTOGRAM to a lacunar array, discarding first undefined
;   values.
;   HISTOGRAM compute the density function of var.
;   var must be an array with at least two different defined values.  
; CATEGORY: I-5-c
; CALLING SEQUENCE: 
;   output=LA_HISTO(var, binsize=binsize, max=max, min=min, $
;          omax=omax, omin=omin, reverse_indices=reverse_indices)
; INPUTS: 
;   var
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   binsize, max, min, omax, omin, reverse_indices : HISTOGRAM keywords
;   see IDL reference guide.
; OUTPUTS: 
;    output
; OPTIONAL OUTPUT PARAMETERS: 
;   X         -- array : var values
; EXAMPLE: 
; ALGORITHM:
;   set output to 0
;   scan array for defined values
;   call histogram on defined values
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
;   LA_MIN, LA_MAX
;   IDL routine HISTOGRAM 
; MODIFICATION HISTORY: 
;    13-Feb-1995  written with template_gen              FV IAS 
;    19-Feb-1995  set output to number of defined values
;                 in case of constant var                FV IAS
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
 
 ROUTINE_NAME = 'LA_HISTO'
 VERSION = '1.0' 
 CATEGORY = 'I-5-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var', '/binsize', '/max', '/min', '/omax', '/omin', $
             '/reverse_indices', 'output']
  s_var = CONV_STRING(var)
  s_binsize = CONV_STRING(binsize)
  s_max = CONV_STRING(max)
  s_min = CONV_STRING(min)
  s_omax = CONV_STRING(omax)
  s_omin = CONV_STRING(omin)
  s_reverse_indices = CONV_STRING(reverse_indices)
 CALL_VAL = [s_var, s_binsize, s_max, s_min, s_omax, s_omin, $
             s_reverse_indices, '']
 output=-1
  
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output=LA_HISTO(var, [X,] binsize=binsize, max=max, $'
   PRINT, '                   min=min, omax=omax, omin=omin, ', $
          ' reverse_indices=reverse_indices)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
 s_var = size(var)
 tva = s_var(s_var(0)+1)

 IF tva LT 1 or (tva GT 5) THEN BEGIN
    status(0) = ['Wrong type for var :' + strtrim(tva), 'E']
    GOTO, CLOSING
 ENDIF

 IF n_elements(var) LE 1 THEN BEGIN
    status(0) = ['var should be an array of more than one value', 'E'] 
    GOTO, CLOSING
 ENDIF

 undef = LA_UNDEF(tva)

;------------------------------------------------------------
; function body
;------------------------------------------------------------

; set output to 0 (zero density, if no defined value found)
 output = 0

; extract defined values location and apply histogram
 good_pixels = where(var ne undef, cpt)

 IF n_elements(binsize) eq 0 THEN binsize=1
 IF n_elements(max) eq 0 THEN max=LA_MAX(var)
 IF n_elements(min) eq 0 THEN min=LA_MIN(var)
 omax = max & omin = min

 IF min eq max THEN BEGIN
    status(0) = ['Cannot apply HISTOGRAM on constant array', 'E']
    output = [cpt]
    GOTO, CLOSING
 ENDIF

 IF cpt GT 0 THEN output = HISTOGRAM(var(good_pixels), binsize=binsize, $
                           max=max, min=min, reverse_indices=reverse_indices) 
 
 X = (INDGEN( FIX( (MAX- MIN) / BINSIZE+ 1.5)) + 0.5)* BINSIZE + MIN

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_var = CONV_STRING(var)
  s_binsize = CONV_STRING(binsize)
  s_max = CONV_STRING(max)
  s_min = CONV_STRING(min)
  s_omax = CONV_STRING(omax)
  s_omin = CONV_STRING(omin)
  s_reverse_indices = CONV_STRING(reverse_indices)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_var, s_binsize, s_max, s_min, s_omax, s_omin, $
              s_reverse_indices, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END


