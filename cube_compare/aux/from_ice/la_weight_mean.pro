FUNCTION la_weight_mean,var,weight,dim=dim,mask=mask

;+
; NAME:
;  la_weight_mean
; CALLING SEQUENCE:
;  array=la_weight_mean(var,weight)
; PURPOSE:
;  Compute weight mean using la_ function
; 
;               var_1*weight_1+var_2*weight_2+...
; weight mean = ----------------------------------  
;                     weight_1+weight_2+...
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
;  weighted mean
;     
; COMMONS:
;  None
; SIDE EFFECTS:
;     
; MODIFICATION HISTORY:
;  written 7-Nov-15 by Ruka Misawa
;-

w_mean=la_div(la_tot(la_mul(var,weight),dim=dim),la_tot(weight))
  
RETURN,w_mean
  
END
