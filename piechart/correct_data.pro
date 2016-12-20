pro correct_data, in_dir=in_dir $
                  , in_mask_dir=in_mask_dir $
                  , out_mask_dir=out_mask_dir $
                  , out_dir=out_dir $
                  , gals=gals $
                  , ha=ha $
                  , co=co $
                  , nostop=nostop $
                  , applymask=applymask


;+
;
; Clean up headers, do basic masking and generate maps on common grid
;
;-

  
; ==============================
; DEFAULT DIRECTORIES & GALAXIES
; ==============================
  
  use_in_co_dir = '/Users/anniehughes/Work/SFNG/piechart/analysis/co_v0p2/'
  use_in_ha_dir = '/Users/anniehughes/Work/SFNG/piechart/analysis/ha_v0p1/'
  use_in_mask_dir = '/Users/anniehughes/Work/SFNG/piechart/analysis/multilam_v0p1/'
  use_out_mask_dir='../masks/'
  use_out_dir='../good_data/'
  use_gals=['NGC0628','NGC3627','NGC3351','NGC5068','NGC4254','NGC4321']
  do_ha = 0 ; includes basic DIG subtraction (via large-scale filtering)
  do_co = 0
  do_applymask=0
  
; ==============================
; PROCESS USER INPUTS
; ==============================

  if keyword_set(in_co_dir) then use_in_co_dir=in_co_dir
  if keyword_set(in_ha_dir) then use_in_ha_dir=in_ha_dir
  if keyword_set(out_dir) then use_out_dir=out_dir
  if keyword_set(in_mask_dir) then use_in_mask_dir=in_mask_dir
  if keyword_set(out_mask_dir) then use_out_mask_dir=out_mask_dir
  if keyword_set(gals) then use_gals=gals
  if keyword_set(ha) then do_ha=1
  if keyword_set(co) then do_co=1
  if keyword_set(applymask) then do_applymask=1

  ; enforce final back slash and make sure it exists
  use_in_co_dir=file_search(use_in_co_dir,/mark,/full)
  use_in_ha_dir=file_search(use_in_ha_dir,/mark,/full)
  use_out_dir=file_search(use_out_dir,/mark,/full)
  use_in_mask_dir=file_search(use_in_mask_dir,/mark,/full)
  use_out_mask_dir=file_search(use_out_mask_dir,/mark,/full)

  if use_in_co_dir eq '' or use_in_ha_dir eq '' or use_out_dir eq '' or $
     use_in_mask_dir eq '' or  use_out_mask_dir eq '' then $
     message,'Problem with in/out/mask directories'
  
; ==============================
; CORRECT DATASETS REQUESTED
; ==============================
 
  if do_co then begin
     
     correct_co $
        , in_dir = use_in_co_dir $
        , out_dir = use_out_dir $
        , gals = use_gals $
        , nostop = nostop
     
  endif

  if do_ha then begin
     
     correct_ha $
        , in_dir = use_in_ha_dir $
        , out_dir = use_out_dir $
        , gals = use_gals $
        , nostop = nostop $
        , /nonegatives $
        , /skysub

  endif

  if do_applymask then begin
     
    make_coha_commonmask $
       , data_dir = use_out_dir $
       , in_mask_dir = use_in_mask_dir $
       , out_mask_dir = use_out_mask_dir $
       , gals = use_gals $
       , /dig, /h2, /sfr $
       , applymask=do_applymask $
       , nostop = nostop

  endif

  if not keyword_set(nostop) then stop
  
  the_end:

end
