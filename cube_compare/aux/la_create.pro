FUNCTION LA_CREATE, var, mask, binary_mask=binary_mask
;+ 
; NAME: LA_CREATE
; PURPOSE:
;   create a lacunar array from an array and a mask pointing on bad pixels.
;   If key binary_mask not set, mask respect ISOCAM SCD conventions :
;    - 0 for good pixels
;    - first bit set (+1) for bad pixels (without any type)
;    - second bit set (+2) for dead pixels
;    - third bit set (+4) for pixels corrupted by memory effects
;    - fourth bit set (+8) for glitched pixels
;    ...
;    so values with a non zero mask are set to the undefined value
;    If key binar_mask set, it is the contrary : zero masked values are set to
;    undefined value
; CATEGORY: I-5-e
; CALLING SEQUENCE: 
;   output=LA_CREATE(var, mask)
; INPUTS: 
;   var           -- array of arithmetic type
; OPTIONAL INPUT PARAMETERS: 
;   mask          -- byte array of same size as var : every value which mask ne
;                    0 is set to undef. If no mask given, do nothing
; KEYED INPUTS: 
;   binary_mask   -- 0/1 : if set mask should be a binary mask and zero masked
;                    values are set to the undefined value 
; OUTPUTS: 
;    output       -- lacunar array of same size and type than var
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE:
;
;
;
; 
; ALGORITHM: 
;   set output to var
;   If binary_mask, search for positions with a zero mask value, else 
;   search for positions of non zero mask values
;   set them to undef into output
; DEPENDENCIES: 
;   initialization of lacunar array routines
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;    none 
; RESTRICTIONS: 
;    none
; CALLED PROCEDURES AND FUNCTIONS:
;    LA_UNDEF 
; MODIFICATION HISTORY: 
;    22-Jan-1995  written with template_gen         FV IAS
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
 
 ON_IOERROR, IOERROR
 GOTO, START
 IOERROR:
       STATUS(0) = ['VMS IO ERROR', 'E']
       PRINT, STATUS(2), ' : ', STATUS(0)
       PRINT, STRMESSAGE(!ERR)
       GOTO, CLOSING
 START:
 
;------------------------------------------------------------
; initialization
;------------------------------------------------------------
 
 ROUTINE_NAME = 'LA_CREATE'
 VERSION = '1.0' 
 CATEGORY = 'I-5-e'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var', 'mask', 'output']
  s_var = CONV_STRING(var)
  s_mask = CONV_STRING(mask)
 CALL_VAL = [s_var, s_mask, '']
 output= -1
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output=LA_CREATE(var, mask)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 szv = size(var)
 tv1 = szv(szv(0) + 1)
 ; check we have arithmetic types (1 to 5: byte, int, long int, float or
 ; double)
 IF (tv1 LT 1) or (tv1 GT 5) THEN BEGIN
    status(0) = ['Wrong type for var:'+CONV_STRING(tv1), 'E']
    GOTO, CLOSING
 ENDIF
 undef = la_undef(tv1)
 output = var

 IF n_elements(mask) THEN GOTO, CLOSING

 szv2 = size(mask)
 tv2 = szv2(szv2(0) + 1)
 ; check we have a byte or integer mask
 IF (tv2 GT 3) THEN BEGIN
    status(0) = ['Wrong type for mask:'+CONV_STRING(tv2), 'E']
    GOTO, CLOSING
 ENDIF

 ; check dimension consistency
 IF szv2(0) GT szv(0) THEN BEGIN
    status(0) = ['Wrong dimension for mask:'+strtrim(szv2(0),2)+' versus '+$
                 strtrim(szv(0),2), 'E']
    GOTO, CLOSING
 ENDIF
 ok = 1
 FOR i=1, szv2(0) DO ok = ok and (szv(i) eq szv2(i))
 IF not OK THEN BEGIN
    status(0) = ['Wrong size for mask:'+CONV_STRING(szv2)+ ' versus var: '+$
                 CONV_STRING(szv), 'E']
    GOTO, CLOSING
 ENDIF
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------

 output = var

 IF KEYWORD_SET(binary_mask) THEN bad_pixels = where(mask eq 0, cpt) $
 ELSE bad_pixels = where(mask ne 0, cpt)

 IF cpt GT 0 THEN output(bad_pixels) = undef 
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_var = CONV_STRING(var)
  s_mask = CONV_STRING(mask)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_var, s_mask, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
