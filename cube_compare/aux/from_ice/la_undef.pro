FUNCTION LA_UNDEF, type
;+ 
; NAME: LA_UNDEF
; PURPOSE:
;   return value considered as undefined for a given type   
;   if type is outside the range 1-5, return the empty string 
; CATEGORY: I-5-a
; CALLING SEQUENCE: 
;   output=LA_UNDEF(type)
; MANDATORY INPUTS:
;   none 
; OPTIONAL INPUT PARAMETERS:
;   type         -- integer with IDL scalar type (1 to 5), default is 2
; KEYED INPUTS:
;   none 
; OUTPUTS: 
;   output       -- undefined value
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE:
;   ICE> help, la_undef()
;   <Expression>    INT       =   -32768
; ALGORITHM:
;   straightforward 
; DEPENDENCIES:
;   none 
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL           
; SIDE EFFECTS:
;   none 
; RESTRICTIONS:
;   do not accept IDL types : complex and structure (6 and 8)
; CALLED PROCEDURES AND FUNCTIONS:
;   none 
; MODIFICATION HISTORY: 
;    18-Apr-1994  written with template_gen         FV IAS
;     5-Oct-1994  V.1.0 for configuration control   FV IAS
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
 
 ROUTINE_NAME = 'LA_UNDEF'
 VERSION = '1.0'
 CATEGORY = 'I-5-a'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME + ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['type', 'output']
 stype = CONV_STRING(type)
 CALLING_VAL = [stype,'']
 output=''

;------------------------------------------------------------
; function body
;------------------------------------------------------------

 IF n_elements(type) EQ 0 THEN type =2

 CASE type OF
      1 : output = 255b
      2 : output = fix(-32768)
      3 : output = -32768l
      4 : output = -32768.
      5 : output = -32768d
      7 :
     12 : output = uint(65535)    ;unsigned integer (added JPB)
      ELSE : status(0) = ['Irrelevant type :' + strtrim(type, 2), 'E']
 ENDCASE

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  sout = CONV_STRING(output)
  ACTL_VAL = [stype, sout]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALLING_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
