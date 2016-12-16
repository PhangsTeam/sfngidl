function sfng_get_basic_stats,data,la_flag=la_flag,robust=robust,dim=dim

  s=sfng_empty_stats_str()

  if not keyword_set(la_flag) then begin
     s.min=min(data,/nan)
     s.max=max(data,/nan)
     s.mean=avg(data,/nan)
     s.median=median(data) ; by default, IDL median treats NaNs as missing data
     s.rms=stddev(data,/nan)
     s.sum=total(data,/nan)
     if keyword_set(robust) then s.rms=robust_sigma(data)
  end else begin
     s.min=la_min(data,dim=dim)
     s.max=la_max(data,dim=dim)
     s.mean=la_mean(data,dim=dim)
     s.median=la_median(data,dim=dim)
     s.rms=la_sigma(data,dim=dim)
     s.sum=la_tot(data,dim=dim)
  end

  return,s

end
  
