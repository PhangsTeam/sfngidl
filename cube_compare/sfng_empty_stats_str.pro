function sfng_empty_stats_str

  nan = !values.f_nan

  stats_struct = {min:nan, $
                  max:nan,$
                  mean:nan,$
                  median:nan,$
                  rms:nan, $
                  sum:nan, $
                  ndata:0L }

  return, stats_struct

 end
