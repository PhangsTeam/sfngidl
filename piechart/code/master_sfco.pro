pro master_sfco

;+
;
; This is a wrapper to routines that prepare the data and then run the analysis routine
;
;-

; ==============================
; CONTROL FLOW
; ==============================
  do_correct=0
  do_convolve=0
  do_analysis=1

  use_gals=['NGC0628','NGC3627','NGC3351','NGC5068','NGC4254','NGC4321','NGC5194']
;  use_gals=['NGC5194']

; galaxy parameters obtained from AKL's galbase IDL routines
; https://github.com/akleroy/galbase

  
; get basic data into shape
  if keyword_set(do_correct) then begin
     
     correct_data,/co,gals=use_gals,/nostop

     correct_data,/ha,gals=use_gals,/nostop

     correct_data,/applymask,gals=use_gals,/nostop

  end

; convolution of gas and SFR maps to matched resolution
  if keyword_set(do_convolve) then begin
     
     convolve_for_sfco,gals=use_gals,/co,/ha,/hii,/dig,/h2,/sfr
     
  end

  
; analyse SFR-CO metrics as a function of scale
; generate PIE/SCATTER/CDF plots and create results structure  
  if keyword_set(do_analysis) then begin

     ; method 2 with 80% flux threshold
     sfco_versus_scale, keyfile='SFNG_allgalaxies_80thresh.txt', savefile='../savefiles/sfng_allgals_BGmaps_80thresh_pieonly.sav',/pie,/force,missing=0.,/nostop,plot_dir='../plots/thresh80/'

     stop
     
     ; method 2 with 90% flux threshold
     sfco_versus_scale, keyfile='SFNG_allgalaxies_90thresh.txt', savefile='../savefiles/sfng_allgals_BGmaps_90thresh_pieonly.sav',/pie,/force,missing=0.,/nostop,plot_dir='../plots/thresh90/'

     stop
     
; method 1 -- physical threshold at 300pc, then follow the same flux %
;             at other scales     
;     sfco_versus_scale, keyfile='SFNG_allgalaxies_relthresh.txt', savefile='../savefiles/sfng_allgals_BGmaps_relthresh_pieonly.sav',/pie,/force,missing=0.,/nostop,plot_dir='../plots/relthresh/'

;     stop
     
; method 3 -- baed on model of smoothing an isolated blob     
;     sfco_versus_scale, keyfile='SFNG_allgalaxies_blobthresh.txt', savefile='../savefiles/sfng_allgals_BGmaps_blobthresh_pieonly.sav',/pie,/force,/physical,missing=0.,/nostop,plot_dir='../plots/blobthresh/'

;     stop
     
  end
  
  stop

end
