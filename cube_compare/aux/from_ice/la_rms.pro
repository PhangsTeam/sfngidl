FUNCTION la_rms,array,dim=dim

;+ 
; NAME: LA_RMS
; PURPOSE:
;   compute RMS on every element
; CALLING SEQUENCE: 
;   output=LA_RMS(var)
; INPUTS: 
;   var1,    -- array variable of arithmetic type
; OPTIONAL INPUT PARAMETERS:
;   none 
; KEYED INPUTS:
;   none 
; OUTPUTS: 
;    output  -- float (or double) array variable with same dimension as var
; OPTIONAL OUTPUT PARAMETERS:
;   none 
; EXAMPLE:
;   ICE> print, la_rms([[6,3,2,1], [8,6,0,0]])
;         4.3301272
; ALGORITHM:
;   straightforward
; DEPENDENCIES:
;   none 
; SIDE EFFECTS:
;    none 
; RESTRICTIONS: 
;    No overflow check
; CALLED PROCEDURES AND FUNCTIONS: 
;    LA_UNDEF 
; MODIFICATION HISTORY: 
;    6-Nov-2015  written with template_gen              RM IRAP
;-
 
rms=la_power(la_div(la_tot(la_power(array,2),dim=dim),n_elements(array)),0.5)

RETURN,rms

END
