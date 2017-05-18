pro run_sfng_cube_compare_example
  
  use_c1file='12co21-30m-only.vcorr.LSR.fits'
  use_c2file='m74_TP_12CO21.LSR.fits'
  use_savefile='ccmp_results.sav'

  ; these must exist, i.e. IDL won't force their creation
  use_datadir = './orig_data/'
  use_outdir ='./good_data/'
  use_plotdir = './plots_M74/'
  use_reportdir = './report_M74/'
  use_savedir = './savefiles/'

; In this example, I am comparing 12co21-30m-only.vcorr.LSR.fits
; (cube1) and m74_TP_12CO21.LSR.fits (cube2).

; I generate a PDF report (the keyword /noreport is absent), which
; will be saved in a subdirectory called report/ with a name like
; sfng_cube_compare_galaxy_2016-12-23T12-40-43.pdf (you can't
; change this name, except for a tag that replaces 'galaxy' in the above
; name string).

; The measurements are saved in an IDL save structure, which is saved
; in in a subdirectory called savefiles/ and called (in this example)
; 'ccmp_results.sav'

; Intermediate FITS files, e.g. the cubes after resolution matching,
; the final masks, noise cubes are saved in a subdirectory called
; good_data/
    
; I use the pixel grid of cube2  (xygrid=2) and the channelisation of
; cube1 (vgrid=1).

; Cube 1 is already in Kelvin, but cube 2 is in Jy/beam, so I switch
; Jy-K conversion ON(=1) for cube2 only (jy2k=[0,1])

; I request that the cubes both be rebaselined. A linear baseline is
; calculated and removed for each spectrum in each cube
; (rebaseline=[1,1]). Other options would be -1 = don't
; rebaseline (DEFAULT), 0 = remove constant offset, or 2 = remove
; quadratic. Higher order baselines are not implemented yet.

; In addition to the common FoV, I remove 5 spatial pixels around the
; edge and 2 edge channels at the start and end of the commonly
; observed (x,y,v) region of the matched cubes before making the
; comparison. (expand_mask_edges=[5,2], Default is [0,0], i.e. no
; additional pixel/channel masking)

; I ask that the comparison be done at 30 arcsecond
; resolution. Default behaviour is to smooth the higher resolution cube to match
; the lower resolution cube (for this, you don't pass a target_beam keyword)
  
  sfng_cube_compare,datadir=use_datadir,outdir=use_outdir,plotdir=use_plotdir $
                    ,reportdir=use_reportdir,savedir=use_savedir $
                    , fits_in1=use_c1file,fits_in2=use_c2file,savefile=use_savefile $
                    , xygrid=2,vgrid=1,jy2k=[0,1],rebaseline=[0,0],expand_mask_edges=[5,2] $
                    , target_beam=[30.,30.,0],/verb,/nice
  
  stop
  

end
