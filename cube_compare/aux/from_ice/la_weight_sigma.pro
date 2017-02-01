FUNCTION la_weight_sigma,var,weight,dim=dim

;+
; NAME:
;  la_weight_sigma
; CALLING SEQUENCE:
;  array=la_weight_sigmas(var,weight)
; PURPOSE:
;  Compute weight mean sigma using la_ function
; 
;                            var_1^2*weight_1^2+var_2^2*weight_2^2+...
; weight mean sigma = sqet( ------------------------------------------ )
;                                 weight_1^2+weight_2^2+...
;  
; INPUTS:
;
; OPTIONAL INPUT:
;   dim       = dimension of required result 
;               0 : return sum of every elementary defined values (default)
;               1 : return vector with sum of every "planes". (with as
;               many elements as in the last dimension)
;              -1 : return sum "plane" (one dimension less than var1)
; OUTPUTS:
;  weighted mean sigma
;     
; COMMONS:
;  None
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;  written 7-Nov-15 by Ruka Misawa
;-

vvar=la_power(var,2)
wweight=la_power(weight,2)

w_sigma=la_div(la_tot(la_mul(vvar,wweight),dim=dim),la_tot(wweight))

RETURN,w_sigma  

END
