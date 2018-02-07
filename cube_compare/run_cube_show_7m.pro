pro run_cube_show_7m
;;

  pipe_7m_osu:
  
  cubename='_7m_co21_pbcorr_round_k'
  use_datadir='data_7m_osu/'
  reportdir='report_7m_osu_'
  plotdir='plots_7m_osu_'
  filelist=file_basename(file_search(use_datadir+'/*'+cubename+'.fits',count=nfiles,/test_read))


  for i=0,nfiles-1 do begin
;     use_c1file=(strtok(filelist[i],'.',/extract))[0]
     use_galname=(strtok(filelist[i],'_',/extract))[0]
     use_reportdir=reportdir+use_galname
     use_plotdir=plotdir+use_galname

     message,'Working on '+filelist[i],/info
     message,'Will create REPORT directory '+use_reportdir,/info
     message,'Will create PLOT directory '+use_plotdir,/info
     spawn,'mkdir '+use_reportdir+' &'
     spawn,'mkdir '+use_plotdir+' &'
     sfng_cube_show,datadir=use_datadir,plotdir=use_plotdir,reportdir=use_reportdir $
                    , fits_in=filelist[i] $
                    , namestr=use_galname+'_7m_osu',/nostop ;,/nice

   end
  
  stop
  
  the_end:
  stop
end
