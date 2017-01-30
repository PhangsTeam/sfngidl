FUNCTION linear_mpfit,x,y,errx,erry,start, $
                      perror=perror,bestnorm=bestnorm,_extra=_extra

;+
; NAME:
;       linear_mpfit
; CALLING SEQUENCE:
;       res=linear_mpfit(x,y,errx,erry,start)
; PURPOSE:
;	makes a linear fit using mpfit_fun
; INPUTS:
;       x,y         = data to fit
;       errx,erry   = corresponding 1-sig uncertainties
;       start  = strat parameters [slope,cste]
; OPTIONAL OUPUT:
;    perror   = parameter errors
;    bestnorm   = bestnorm chi^2
; OUTPUTS:
;    None
; PROCEDURE AND SUBROUTINE USED
;	Straightforwrd
; SIDE EFFECTS:
;    None
; MODIFICATION HISTORY:
;    written JPB
;-

;on_error,2

IF N_PARAMS(0) NE 5 THEN BEGIN
  print,'res=linear_mpfit(x,y,errx,erry,start)'
  print,'Accepted Key-Words: [,perror=][,bestnorm=][,_extra=]'
  GOTO,sortie
ENDIF

w=1./(errx^2+erry^2)
;in fact erry in call below not used. weights is used instead.
res=mpfitfun('linear4mpfit',x,y,erry,start, $
             weights=w,bestnorm=bestnorm,perror=perror,_extra=_extra)

sortie:
RETURN,res

END
