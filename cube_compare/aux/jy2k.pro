function jy2k, bmaj=bmaj, bmin=bmin, lambda=lambda

  ; lambda should be in mm
  ; beam parameters in arcseconds

  if not keyword_set(bmin) then bmin=bmaj

  bmaj=double(bmaj)
  bmin=double(bmin)
  lambda=double(lambda)
  
  factor=13.6*(lambda*lambda)/(1.1133*bmin*bmaj)

  return, factor

end

