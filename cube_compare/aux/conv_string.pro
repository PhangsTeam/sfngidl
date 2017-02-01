FUNCTION CONV_STRING, var
;+ 
; NAME: CONV_STRING
; PURPOSE:
;    convert a value (scalar or array) into string
;    for structures return the size.
;    for large arrays takes the first 40 values
;    IDL type number is put before 
; CATEGORY: I-4-b (IDL type handling)
; CALLING SEQUENCE:
;    string_var = CONV_STRING(var)
; INPUTS:
;    var        -- scalar or array of any IDL type
; OPTIONAL INPUT PARAMETERS: 
;    none
; KEYED INPUTS: 
;    none
; OUTPUTS:
;    string_var -- string converted variable
; OPTIONAL OUTPUT PARAMETERS: 
;    none
; EXAMPLE:
;    ICE> help, conv_string(indgen(5))
;    <Expression>    STRING    = '(2) 0, 1, 2, 3, 4' 
; ALGORITHM:
;    straightforward
; DEPENDENCIES:
;    used by logfile recording routines 
; RESTRICTIONS:
;    no logfile recording      
; CALLED PROCEDURES AND FUNCTIONS:
;    none 
; MODIFICATION HISTORY:  
;        8-Dec-1993  written by Florence Vivares    IAS
;       20-Dec-1993  commentary update        F.V.  IAS
;       25-Jan-1994  update for array         FV    IAS
;       30-Sep-1994  V 1.0 for configuration control FV IAS     
;-
;-----------------------------------------------------------
; on error condition
;-----------------------------------------------------------
  ON_ERROR, 2 

;-----------------------------------------------------------
; function body   
;-----------------------------------------------------------
  string_var = '<undefined>'

  size_var = size(var)
  tvar = size_var(size_var(0) + 1)

  IF tvar NE 0 THEN BEGIN
     IF tvar EQ 1 THEN var0 = FIX(var) ELSE var0 = var
     IF tvar EQ 8 THEN var0 = size_var
     IF tvar NE 8 THEN $
        string_var = '('+ strtrim(tvar,2) + ') ' + STRTRIM(var0(0),2) $
     ELSE string_var = 'size : ' + STRTRIM(var0(0),2)
     maxindex = MIN([n_elements(var0)-1, 20])
     FOR i=1, maxindex DO $
         string_var = string_var + ', ' + STRTRIM(var0(i),2)
     IF maxindex LT n_elements(var0)-1 THEN string_var = string_var + ', ...'
  ENDIF
 
;-----------------------------------------------------------
; closing
;-----------------------------------------------------------
  CLOSING:
 
  RETURN, string_var      
 
 END
