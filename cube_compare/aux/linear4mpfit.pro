FUNCTION linear4mpfit,x,param

;param=[a,b] for y=a*x+b
;meant to be used by linear_mpfit.pro

RETURN,param[0]*x+param[1]

END
