FUNCTION TME_UTK, abs_time, f2_time, utk_start, utk_loc
;+ 
; NAME: TME_UTK
; PURPOSE: 
;   Compute "Orsay utk" knowing that utk = utk_start + utk_loc, as
;   abs_time + f2_time * camtu * 10 (in deci-second)
;   Number of ds since January 1, 1990.
; CATEGORY: I-4-c
; CALLING SEQUENCE: 
;   output=TME_UTK(abs_time, f2_time, utk_start, utk_loc)
; INPUTS: 
;   abs_time  -- array with F2_ABS_TIME (deciseconds)
;   f2_time   -- array with F2_TIME     (camtu)
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   none
; OUTPUTS: 
;    output   -- long array : utk   (deciseconds)
; OPTIONAL OUTPUT PARAMETERS: 
;   utk_start -- flt array  : (utk / 10000) * 10000. (first chars)
;   utk_loc   -- flt array  : (utk mod 10000)        (last chars)
; EXAMPLE: 
; ALGORITHM: 
; DEPENDENCIES:
;   Ground calibration dedicated (In flight data should already contain 
;   ISO project UTK)
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;    none 
; RESTRICTIONS: 
;    none
; CALLED PROCEDURES AND FUNCTIONS:
;   IA_CAMTU
; MODIFICATION HISTORY: 
;    10-Nov-1995  written with template_gen from 
;                 ICCRED 3 CREATE_TIME_CAM                   FV IAS
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
 
 ROUTINE_NAME = 'TME_UTK'
 VERSION = '1.0' 
 CATEGORY = 'I-4-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['abs_time', 'f2_time', 'output']
  s_abs_time = CONV_STRING(abs_time)
  s_f2_time = CONV_STRING(f2_time)
 CALL_VAL = [s_abs_time, s_f2_time, '']
 output=-1
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 2 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output=TME_UTK(abs_time, f2_time, utk_start, utk_loc)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------
 
 output = DOUBLE (abs_time ) + DOUBLE (f2_time) * ia_camtu(1) * 10.
 output = reform(long(output + 0.5))

 utk_start = float(output / 10000) * 10000.
 utk_loc = float(output mod 10000)

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_abs_time = CONV_STRING(abs_time)
  s_f2_time = CONV_STRING(f2_time)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_abs_time, s_f2_time, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
