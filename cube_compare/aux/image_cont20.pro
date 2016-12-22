PRO image_cont20,aim,h $
                ;,print=print $
                 ,noerase=noerase $
                 ,silent=silent $
                 ,coo1=coo1 $
                 ,coo2=coo2 $
                 ,invert=invert $
                 ,spline=spline $
                 ,square=square $ ;kept for backward compatibility Now default
                 ,no_square=no_square $
                 ,title=title $
                 ,xlabel=xlabel $
                 ,ylabel=ylabel $
                 ,labelsize=labelsize $
                 ,mlabelsize=mlabelsize $
                 ,postcript=postcript $ ;kept for backward compatibility Now default
                 ,postscript=postscript $
                 ,rebin=rebin $
                 ,NOGREY=nogrey $
                 ,GRID=grid $
                 ;,pcl=pcl $
                 ,XGRID=xgrid $
                 ,YGRID=ygrid $
                 ,deltaxy=deltaxy $
                 ,smooth=smooth $
                 ,bar_pos=bar_pos $
                 ,off_bar_pos=off_bar_pos $
                 ,bar_tickv=bar_tickv $
                 ,bar_tickname=bar_tickname $
                 ,bar_minmax=bar_minmax $
                 ,bar_title=bar_title $
                 ,bar_charsize=bar_charsize $
                 ,imrange=imrange $
                 ,levels=levels $
                 ,ps_color=ps_color $ ;probably obsolete
                 ,ccolor=ccolor $
                 ,bw=bw $
                 ,overlay=overlay $
                 ,no_x_label=no_x_label $
                 ,no_y_label=no_y_label $
                 ,noclose=noclose $
                 ,nologo=nologo $
                 ,values_grid_coo1=values_grid_coo1 $
                 ,values_grid_coo2=values_grid_coo2 $
                 ,axis_color_value=axis_color_value $
                 ,position_x=position_x $
                 ,position_y=position_y $
                 ,orientation=orientation $
                 ,bit=bit $
                 ,polar=polar $
                 ;,rgb=rgb $
                 ,rescale_rgb=rescale_rgb $
                 ,range_rgb=range_rgb $
                 ,help=help $
                 ,get_x=get_x $
                 ,get_y=get_y $
                 ,image_color_table=image_color_table $
                 ,axis_color_table=axis_color_table $
                 ,no_units=no_units $
                 ,hfi_plot=hfi_plot $  ;should be fixed back to original
                 ,lic_iqu=lic_iqu $
                 ,lic_rotate_iqu=lic_rotate_iqu $
                 ,lic_niter=lic_niter $
                 ,lic_length=lic_length $
                 ,lic_map=lic_map $
                 ,lic_transparency=lic_transparency

;+
; NAME:
;       image_cont20
; CALLING SEQUENCE:
;       image_cont20, a, h
; PURPOSE:
;     - Display an Astronomical image on screen.
;     - Coordinates on the axis are computed from the fits header
;       Tick positions are decided automatically, unless
;	!x.ticks or !y.ticks are non zero when calling the procedure, in
;	which case the ticks positions, labels, ... must be provided explicitely
;	by the user into !x.tickv, !x.tickname, ...
;     - The position of the plot on a page (or screen) is determined from
;	the !p.position parameter. By defaults, a similar linear scale (cm/pixel) on both axis is imposed
;     - A coordinate grid can be overlaid, which can be either
;       computed from the header or provided
;     - Levels in the image or another image with same size can be overlaid
;     - polarization vectors or LIC map can be overlaid
;     - The position of sources can be overlaid
;     - A bar giving the color level intensity can be drawn
;     - Postscript files can be generated 
; INPUTS:
;       a	= Image to be drawn. 2dim or 3dim [N,N,3] (rgb)
;       h	= image fits header
; OPTIONAL INPUT:
;	None
; ACCEPTED KEY-WORDS:
;       rescale_rgb= if set, automatically rescales rgb cube
;       range_rgb=   if set, rescale rgb cube to that range ([minr,maxr,ming,maxg,minb,maxb])
;       lic_iqu= IQU cube from which to construct LIC map (N,N,3)
;       lic_rotate_iqu= if set, rotates LIC map by 90 deg (to show B field)
;       lic_niter=  LIC number of iterations
;       lic_length= length used for the LIC map [pixels]
;       lic_map = if set, use this LIC map. outputs the LIC map used.
;       lic_transparency = if set transparency for the LIC (default=[0.5,1])
;	levels  = structure of levels to be plotted (use create_contour_st)
;	print	= if set, output directed to printer
;	postcript= if set, output directed to this postcript file
;	invert	= to invert the image (usefull when printing on paper)
;	square	= if set the paper output will be square (same deg/cm)
;	title   = to write a main title
;	nogrey  = to plot with no greyscale map (only contours)
;	grid    = if set a grid of coordinates is drawn
;	nologo  = if set, logo not plotted
;       xgrid   = set of grid values to draw on x (deg) (if !x.ticks <> 0)
;	ygrid   = set of grid values to draw on y (deg)	(if !y.ticks <> 0)
;	deltaxy = spacing between labels (default [1,1],>0)
;	rebin   = if set and GT 1, the images are rebined by *rebin
;	silent  = to make the routine it silent
;	coo1    = 2D array of x coordinates (in case /grid is set)
;	coo2    = 2D array of y coordinates
;	bar_pos = If set, a color bar is displayed at !p.position=bar_pos
;	off_bar_pos = if set, the bar position is in offset compared to
;		      the !p.position. Recommended with /print
;       values_grid_coo1 = values to be drawn for the grid on coo1 (degrees)
;       values_grid_coo2 = values to be drawn for the grid on coo2 (degrees)
;       position_x = position along the y axis to be used to derive tick positions for x axis
;                 ('low','high' or 'medium')
;       position_y = position along the x axis to be used to derive tick positions for y axis
;                 ('low','high' or 'medium')
;       orientation = orientation of the postcript file.
;                     'portrait' or 'landscape' (default=landscape)
;       get_x       = get the used value of !x (useful to modify axis
;                     of previous plot)
;       get_y       = get the used value of !y (useful to modify axis
;                     of previous plot)
;       noclose     = do not close postcript file (for multiplotting)
;       image_color_table= color table to be used for the image
;       axis_color_table= color table to be used for axis
;       ps_color    = if set, make postcript be in color
; EXAMPLES
;    ra=ten(16.,24,0)/24.*360. & dec=ten(-20.,0.,0.)
;    file=locate_issa(ra,dec,band=4,closest=2)
;    d=readfits(file(0),h)
;    !p.position=[0.1,0.1,0.8,0.8]
;    tit='Image_cont20 Example'
;    create_coo2,h,coo1,coo2,/silent,/lbl
;    lev=[100,500.,2000.]
;    levels=create_contour_st(d,lev1=lev)
;    levels.set0.color=[50,100,150]
;    over=create_overlay_st([2,1])
;    over.set0.coo1(0)=ra & over.set0.coo2(0)=dec
;    over.set0.sym_type(0)='triangle' & over.set0.sym_angle(0)=45.
;    over.set0.sym_size(0)=10. & over.set0.sym_color(0)=0.
;    loadct,13
;    image_cont20,d,h,imrange=[-50,200],off_bar_pos=[1.2,0.,1.3,1],overlay=over, $
;    title=tit,/square,coo1=coo1,coo2=coo2,/grid,levels=levels,/silent
; OUTPUTS:
;     None
; PROCEDURE AND SUBROUTINE USED
;     sxpar
;     mk_grid_val, my_bar20, rest_graph_var, save_graph_var, jpb_plotsym
; COMMONS:
;    @proj_common.com
;    @sysgraphvar.com
;    @imcont8_common.com
; SIDE EFFECTS:
;	use imrange to define the range to be plotted, not im>value<value
;	The routine will be in trouble close to the pole, where latitudes cannot
;	be written along the axis (!)
;	For some reason, the routine cannot be bracketed by laser, laserout.
;	Use the /print instead, but this imply you cannot plot several figures
;	on the same piece of paper taking advantage of the !p.position
;	parameter.
;	When the routine stops on an error (which happens), !x.ticks and !y.ticks
;	are not properly resetted. One should do !x.ticks=0 & !y.ticks=0 before
;	rerunning image_cont20 to obtain default axis.
;	Some identified bugs:
;	Sometimes print on paper with a smaller size
;	On paper, do not ignore !indef values when contouring ...
;	The bar doesn't (yet) have the proper size when in a poscript file
;
;	all undefined				-> screen display
;	print=0,ps_color=1,postcript=set,pcl=0	-> color postcript file
;	print=0,ps_color=0,postcript=set,pcl=1	-> PCL             file
;	print=0,ps_color=0,postcript=set,pcl=1	-> B&W color post  file
;	print=1,ps_color=1,postcript=und,pcl=0	-> color PS        print
;	print=1,ps_color=0,postcript=set,pcl=1	-> color PCL       print
; MODIFICATION HISTORY:
;    written 25-07-92 by Jean-Philippe Bernard, Nagoya University.
;    modified JPB 03-03-93 to include key-words
;    modified JPB 19-03-93 to fix a bug when plotting coordinates with SQUARE
;    modified JPB 15-05-93 to adapt to IPAC Version 2.0.1
;    modified JPB Fri Jan 27 95 to decode correct cor---proj strings
;    modified JPB Tue Jul 25 95 to adapt to new IDL version
;    modified JPB Wed Jul 30 97 to use contour and overplot structures
;    modified JPB Tue Feb 16 99 to use values_grid_coo1 and values_grid_coo2
;    modified JPB 15/08/00 to add orientation Keyword
;    modified JPB 8/04/01 to ignore undefined values in the contour plottings
;    modified JPB 11/04/01 to fix color table problem in PS files
;    modified JPB 22/12/02 to set to bit=8 even for black and white
;                          (for PS files to be recognized by illustrator 10 under macOSX)
;-

;on_error,2

@proj_common.com
@sysgraphvar.com
@imcont8_common.com
;common colors,r_orig,g_orig,b_orig,r_curr,g_curr,b_curr

old_pposition=!p.position

IF keyword_set(help) THEN BEGIN
  doc_library,'image_cont20'
  goto,sortie
ENDIF

IF not keyword_set(position_y) THEN position_y='low'
IF not keyword_set(position_x) THEN position_x='low'

;This is the current color table
;rr=r_curr & gg=g_curr & bb=b_curr

IF not keyword_set(previous_psfile) THEN previous_psfile=''
;stop

IF N_PARAMS(0) LT 2 THEN BEGIN
  mess='Calling Sequence: image_cont20, image, h'
  print,mess
  print,'see image_cont20,/help for help'
  goto,sortie
ENDIF

IF not keyword_set(spline) THEN spline=0

landscape_ori=1 & portrait_ori=0
IF keyword_set(orientation) THEN BEGIN
  IF orientation EQ 'portrait' THEN BEGIN
    landscape_ori=0 & portrait_ori=1
  ENDIF
ENDIF

si=size(aim) & Nx=si(1) & Ny=si(2)
rgb_on=0
IF si[0] EQ 3 THEN BEGIN
  rgb_on=1
ENDIF
use_lic_length=max([Nx,Ny])/10. ;default value is 1/10th of image
IF keyword_set(lic_length) THEN use_lic_length=lic_length

ps_on=0
IF keyword_set(postscript) OR keyword_set(postcript) THEN BEGIN
  ps_on=1
  IF keyword_set(postscript) THEN BEGIN
    ps_file_name=postscript
  ENDIF ELSE BEGIN
   ps_file_name=postcript
  ENDELSE
ENDIF

;=========== Check the input parameters
IF keyword_set(overx) and not keyword_set(overy) THEN BEGIN
  message,'both overx and overy should be set',/info
  goto,sortie
ENDIF
IF keyword_set(overy) and not keyword_set(overx) THEN BEGIN
  message,'both overx and overy should be set',/info
  goto,sortie
ENDIF

units=sxpar(h,'BUNIT')

IF not keyword_set(deltaxy) THEN BEGIN
  deltaxy=[1,1]
ENDIF ELSE BEGIN
  deltaxy=round(deltaxy)	;make sure this is integer
  ind=where(deltaxy LT 1,count) ;make sure this is >1
  IF count NE 0 THEN BEGIN
    deltaxy(ind)=1
    message,'deltaxy<1 set to 1',/info
  ENDIF
ENDELSE

;IF not keyword_set(levels) THEN BEGIN
;  lev=[min(aim)-1]
;ENDIF ELSE BEGIN
;  lev=levels(sort(levels))		;re-order levels
;ENDELSE

;============= put projection parameters into common
h2pix,h,silent=silent

;============= Check that image and header are compatible
sz=size(aim)
IF sz(1) NE naxis1 OR sz(2) NE naxis2 THEN BEGIN
  message,'First array dimensions incompatible with header',/info
  goto,sortie
ENDIF
;sz=size(bim)
;IF sz(1) NE naxis1 OR sz(2) NE naxis2 THEN BEGIN
;  print,'Second array dimensions incompatible with header'
;  goto,sortie
;ENDIF
huse=h

;============= Set rebin factors
rebfact=1
IF keyword_set(rebin) THEN BEGIN
  IF rebin GT 1 THEN BEGIN
    rebfact=FIX(rebin)
    cdelt1=cdelt1/(1.*rebfact)
    cdelt2=cdelt2/(1.*rebfact)
    crpix1=crpix1*rebfact+1
    crpix2=crpix2*rebfact+1
    naxis1=naxis1*rebfact
    naxis2=naxis2*rebfact
    sxaddpar,huse,'CDELT1',cdelt1*!radeg
    sxaddpar,huse,'CDELT2',cdelt2*!radeg
    sxaddpar,huse,'CRPIX1',crpix1
    sxaddpar,huse,'CRPIX2',crpix2
    sxaddpar,huse,'NAXIS1',naxis1
    sxaddpar,huse,'NAXIS2',naxis2
;    huse=write_header(huse,'CDELT1',cdelt1*!radeg)
;    huse=write_header(huse,'CDELT2',cdelt2*!radeg)
;    huse=write_header(huse,'CRPIX1',crpix1)
;    huse=write_header(huse,'CRPIX2',crpix2)
;    huse=write_header(huse,'NAXIS1',naxis1)
;    huse=write_header(huse,'NAXIS2',naxis2)
  ENDIF
ENDIF

h2pix,huse,silent=silent

;Before all, save all graphic kwords
save_graph_var

!psym=0

;============== Set the !p.position array
IF max(!p.position) EQ 0 THEN !p.position=[0.1,0.1,0.9,0.9]

;Set plot type for !d variables
;IF keyword_set(print) OR keyword_set(postcript) OR keyword_set(postscript) THEN BEGIN
IF ps_on THEN BEGIN
;  IF keyword_set(pcl) THEN BEGIN
;    set_plot,'PCL'
;  ENDIF ELSE BEGIN
    set_plot,'PS'
;  ENDELSE
ENDIF ELSE BEGIN
  IF !version.os EQ 'MacOS' THEN BEGIN
    set_plot,'MAC'
  ENDIF ELSE BEGIN
    set_plot,'X'
  ENDELSE
ENDELSE

;============== Modify !p.position to make a square display, if necessary
use_square=1  ;default is square plots
IF keyword_set(no_square) THEN BEGIN
  use_square=0
ENDIF
IF use_square THEN BEGIN
  rapax=(1.*!d.x_vsize)/(1.*!d.y_vsize)
  sid1=abs(cdelt1)*naxis1
  sid2=abs(cdelt2)*naxis2
  IF sid1/sid2 GT rapax THEN BEGIN
    ;change !p.position in y
    toto=!p.position
    toto(3)=toto(1)+rapax*(toto(2)-toto(0))*(1.*sid2)/(1.*sid1)
    !p.position=toto
  ENDIF ELSE BEGIN
    ;change !p.position in x
    toto=!p.position
    toto(2)=toto(0)+(toto(3)-toto(1))*(1.*sid1)/(1.*sid2)/rapax
    !p.position=toto
  ENDELSE
ENDIF

;============== Set the positions for the plot (posx,posy)
;============== and sizes (x_size, y_size)
;stop
px=[!p.position(0),!p.position(2)]*!d.x_vsize
py=[!p.position(1),!p.position(3)]*!d.y_vsize
sx=px(1)-px(0)
sy=py(1)-py(0)
posx=px(0)
posy=py(0)
x_size=px(1)-px(0)
y_size=py(1)-py(0)

;come back to default
IF !version.os EQ 'MacOS' THEN BEGIN
  set_plot,'MAC'
ENDIF ELSE BEGIN
  set_plot,'X'
ENDELSE

;bb_im=bim
a_inv=aim
IF keyword_set(rebin) THEN BEGIN
  IF rebin gt 1 THEN BEGIN
    a_inv=rebin(aim,naxis1,naxis2)
    indbad=where(a_inv EQ !indef,count)
    IF count NE 0 THEN BEGIN
      ind2=enlarge(indbad,rebfact,naxis1,naxis2)
      a_inv(ind2)=!indef
    ENDIF
  ENDIF
ENDIF
last_image=a_inv

IF keyword_set(imrange) THEN BEGIN
  last_image=a_inv
  a_inv=a_inv>min(imrange)<max(imrange)
ENDIF else begin
  ind=where(a_inv ne !indef,count)
  IF count ne 0 THEN BEGIN
    imrange=[min(a_inv(ind)),max(a_inv(ind))]
  ENDIF ELSE imrange=[!indef,!indef+1]
ENDELSE

IF keyword_set(invert) THEN BEGIN
  message,'Inverting image',/info
  ind=where(a_inv NE !indef,count)
  IF count ne 0 THEN BEGIN
    ma=imrange(1)
    mi=imrange(0)
    a_inv(ind)=(1.-(a_inv(ind)-mi)/(ma-mi))*(ma-mi)+mi
  ENDIF ELSE BEGIN
    ma=imrange(1)
    mi=imrange(0)
    a_inv=(1.-(a_inv-mi)/(ma-mi))*(ma-mi)+mi
    a_inv(ind)=mi
  ENDELSE
ENDIF

IF keyword_set(smooth) THEN BEGIN
  if FIX(smooth) GT 1 THEN BEGIN
    smoothfact=smooth*rebfact
;    bb_im=smooth(bb_im,smoothfact)
    a_inv=smooth(a_inv,smoothfact)
  ENDIF ELSE BEGIN
    message,'Smoothing factor should be >1',/info
    message,'No smoothing applied',/info
  ENDELSE
ENDIF
sz=size(a_inv)

IF not keyword_set(title) THEN BEGIN
  !mtitle=''
ENDIF ELSE BEGIN
  !mtitle=title
ENDELSE

;=====================image for Grey level plot
;== added postcript test so postcript does not erase latest X-window plot
;IF NOT keyword_set(noerase) AND NOT keyword_set(print) AND $
;   NOT keyword_set(postcript) AND NOT keyword_set(postscript) THEN ERASE
IF NOT keyword_set(noerase) AND NOT ps_on THEN ERASE

;stop
IF keyword_set(lic_iqu) OR keyword_set(lic_map) THEN BEGIN
  IF NOT keyword_set(lic_map) THEN BEGIN
    lic_map=iqu2lic(lic_iqu,h,rotate_iqu=lic_rotate_iqu,niter=lic_niter,length=use_lic_length)
  ENDIF ELSE BEGIN
    ;check that LIC map has the right dimension
    IF (size(lic_map))(1) NE Nx OR (size(lic_map))(1) NE Ny THEN BEGIN
      message,'The LIC map entered has the wrong dimension'
    ENDIF
  ENDELSE
ENDIF

;IF not keyword_set(print) AND not keyword_set(postcript) AND not keyword_set(postscript) and not rgb_on THEN BEGIN
IF not ps_on and not rgb_on THEN BEGIN
  ;At first run here, sy sometimes weird.
  ;This is linked to the bug of small image drawn first
                                ;This bug is linked to the fact that a
                                ;X window is not already created.
  toto=poly_2d(a_inv,[[0,0],[sz(1)/sx,0]],[[0,sz(2)/sy],[0,0]],0,sx,sy)
  last_image=poly_2d(last_image,[[0,0],[sz(1)/sx,0]],[[0,sz(2)/sy],[0,0]],0,sx,sy)
ENDIF ELSE BEGIN
  toto=a_inv
ENDELSE

;IF not keyword_set(print) AND not keyword_set(postcript) AND not keyword_set(postscript) THEN BEGIN
IF not ps_on THEN BEGIN
  IF keyword_set(lic_iqu) OR keyword_set(lic_map) THEN BEGIN
    ;IF not keyword_set(lic_map) THEN BEGIN
    ;  lic_map=iqu2lic(lic_iqu,h,rotate_iqu=lic_rotate_iqu,niter=lic_niter,length=use_lic_length/Nx*sx)
    ;ENDIF ELSE BEGIN
    ;  ;check that LIC map has the right dimension
    ;  IF (size(lic_map))(1) NE Nx OR (size(lic_map))(1) NE Ny THEN BEGIN
    ;    message,'The LIC map entered has the wrong dimension'
    ;  ENDIF
    ;ENDELSE
    use_lic_map=poly_2d(lic_map,[[0,0],[sz(1)/sx,0]],[[0,sz(2)/sy],[0,0]],0,sx,sy)
  ENDIF
ENDIF ELSE BEGIN
  IF keyword_set(lic_iqu) OR keyword_set(lic_map) THEN BEGIN
    ;IF not keyword_set(lic_map) THEN BEGIN
    ;  lic_map=iqu2lic(lic_iqu,h,rotate_iqu=lic_rotate_iqu,niter=lic_niter,length=use_lic_length/Nx*sx)
    ;ENDIF ELSE BEGIN
    ;  ;check that LIC map has the right dimension
    ;  IF (size(lic_map))(1) NE Nx OR (size(lic_map))(1) NE Ny THEN BEGIN
    ;    message,'The LIC map entered has the wrong dimension'
    ;  ENDIF
    ;ENDELSE
    use_lic_map=lic_map
  ENDIF
ENDELSE

;=================== Preparing grid values
coo_ok=0
IF keyword_set(coo1) AND keyword_set(coo2) THEN BEGIN
  coo_ok=1
  NxC1=(size(coo1))(1) & NyC1=(size(coo1))(2)
  NxC2=(size(coo2))(1) & NyC2=(size(coo2))(2)
  IF NxC1 NE Nx OR NyC1 NE Ny OR NxC2 NE Nx OR NyC2 NE Ny THEN coo_ok=0
ENDIF
IF keyword_set(grid) AND not coo_ok THEN BEGIN
  create_coo2,h,coo1,coo2
ENDIF

;=================== Setting color and size for dots and grids
IF not keyword_set(ccolor) THEN BEGIN
  IF not keyword_set(invert) THEN BEGIN
    ccolor=!d.n_colors-1
  ENDIF ELSE BEGIN
    ccolor=0
  ENDELSE
ENDIF

IF ps_on THEN BEGIN
  ;IF keyword_set(PCL) THEN BEGIN
  ;  set_plot,'PCL'
  ;  device,/ordered
  ;ENDIF ELSE BEGIN
    set_plot,'PS'
    device,/times
    IF keyword_set(ps_color) THEN BEGIN
      device,bit=8   ;,/encapsulated
;      device,bit=8,/encapsulated
    ENDIF ELSE BEGIN
      IF keyword_set(bit) THEN bbit=bit ELSE bbit=8
      device,bit=bbit                         ;set to 8 bits anyway, in order to work with illustrator 10
    ENDELSE
;    IF keyword_set(color_table) THEN loadct,color_table
;    DEVICE,/landscape
  ;ENDELSE
  device,landscape=landscape_ori,portrait=portrait_ori
  IF keyword_set(ps_color) THEN device,color=1 ELSE device,color=0
;  Works to set the PS file for Mac, but does not allow several
;  plots in same file ...
   IF ps_on THEN BEGIN
     IF ps_file_name NE previous_psfile THEN BEGIN
       device,filename=ps_file_name
       previous_psfile=ps_file_name
     ENDIF
     LoadCT, 0, /Silent
     Device, DECOMPOSED=1, COLOR=1, BITS_PER_PIXEL=8
     IF keyword_set(ps_color) and rgb_on THEN BEGIN
;     tvlct,rr,gg,bb
;     must force load of colour table 0 for postscript rgb images
;        LoadCT, 0, /Silent
;        Device, DECOMPOSED=1, COLOR=1, BITS_PER_PIXEL=8
     ENDIF  
  ENDIF
ENDIF

IF (!d.name eq 'X') AND (keyword_set(bw)) THEN BEGIN
    message,'inverting the colors',/info
    tvlct,r,g,b,/get
    print,'r(*) = ',r(indgen(!d.n_colors/10)*10)
    r=rotate(r,2)
    g=rotate(g,2)
    b=rotate(b,2)
    tvlct,r,g,b
    tvlct,r,g,b,/get
    oldr=r
    print,'r(*) = ',r(indgen(!d.n_colors/10)*10)
ENDIF

;Print,'Imrange in image_cont20 = ',imrange 

;Adjust the top of toto
;; IF not keyword_set(pcl) THEN BEGIN
;; ;  topps=180
;; ;  toto=bytscl(toto,min=min(toto),max=max(toto),top=topps)
;; ;  off_ps=0						;may need to be <>0
;;   toto=bytscl(toto,min=imrange(0),max=imrange(1),top=!d.n_colors-1)
;; ENDIF else begin
;; ;  toppcl=150
;;   toto=bytscl(a_inv,min=imrange(0),max=imrange(1),top=!d.n_colors-1)
;; ENDELSE
;; ;Print,'Max toto = ',max(toto),', max color =',!d.n_colors

; =================== Drawing Color image
IF rgb_on THEN BEGIN
   rgb=aim
   ;========== RGB image
   red_in=reform(rgb[*,*,0])
   green_in=reform(rgb[*,*,1])
   blue_in=reform(rgb[*,*,2])
   IF ps_on THEN BEGIN
     red_use=red_in
     green_use=green_in
     blue_use=blue_in
   ENDIF ELSE BEGIN
     red_use=poly_2d(red_in,[[0,0],[sz(1)/sx,0]],[[0,sz(2)/sy],[0,0]],0,sx,sy)
     green_use=poly_2d(green_in,[[0,0],[sz(1)/sx,0]],[[0,sz(2)/sy],[0,0]],0,sx,sy)
     blue_use=poly_2d(blue_in,[[0,0],[sz(1)/sx,0]],[[0,sz(2)/sy],[0,0]],0,sx,sy)
   ENDELSE
   sz_use=size(red_use,/dim)
   NNX=sz_use[0] & NNy=sz_use[1]
   rgb_use=intarr(NNx,NNy,3)
   rgb_use[*,*,0]=red_use
   rgb_use[*,*,1]=green_use
   rgb_use[*,*,2]=blue_use
   IF keyword_set(rescale_rgb) THEN BEGIN
     max_red=percentile(red_use,1)
     max_green=percentile(green_use,1)
     max_blue=percentile(blue_use,1)
     min_red=max(0.>percentile(red_use,99.9))
     min_green=max(0.>percentile(green_use,99.9))
     min_blue=max(0.>percentile(blue_use,99.9))
     IF keyword_set(range_rgb) and n_elements(range_rgb) ne 6 then $
       message,'range_rgb: 6-element vector [rmin,rmax,gmin,gmax,bmin,bmax]. Using min,max of each channel instead.',/info
     IF keyword_set(range_rgb) and n_elements(range_rgb) eq 6 then begin
       min_red=range_rgb[0]
       max_red=range_rgb[1]
       min_green=range_rgb[2]
       max_green=range_rgb[3]
       min_blue=range_rgb[4]
       max_blue=range_rgb[5]
     ENDIF
     red_sca=bytscl(red_use,min=min_red,max=max_red)
     green_sca=bytscl(green_use,min=min_green,max=max_green)
     blue_sca=bytscl(blue_use,min=min_blue,max=max_blue)
     rgb_use[*,*,0]=red_sca
     rgb_use[*,*,1]=green_sca
     rgb_use[*,*,2]=blue_sca
   ENDIF
   image=rgb_use
   ;stop
   ;tv,rgb_use,posx+1.,posy+1.,xsize=x_size,ysize=y_size,true=3,/device
ENDIF ELSE BEGIN
  ;Adjust the top of toto
  toto=bytscl(toto,min=imrange(0),max=imrange(1),top=!d.n_colors-1)
  IF keyword_set(image_color_table) THEN loadct,image_color_table
  NNx=(size(toto))(1)
  NNy=(size(toto))(2)
  image=bytarr(NNx,NNy,3)
  image[*,*,0]=toto
  image[*,*,1]=toto
  image[*,*,2]=toto
  tvlct,r,g,b,/get
  vv=lindgen(n_elements(r))
;      toto=bytscl(toto,min=imrange(0),max=imrange(1),top=!d.n_colors-1)
  a=interpol(r,vv,toto[10,10])
  image[*,*,0]=interpol(r,vv,image[*,*,0])
  image[*,*,1]=interpol(g,vv,image[*,*,1])
  image[*,*,2]=interpol(b,vv,image[*,*,2])
  ;=== The following is needed for poscript correct output
  IF ps_on THEN BEGIN
    loadct, 0, /Silent
    device, DECOMPOSED=1, COLOR=1, BITS_PER_PIXEL=8
  ENDIF
ENDELSE
;=== apply LIC map if needed
IF keyword_set(lic_iqu) OR keyword_set(lic_map) THEN BEGIN
  ;backgnd = bytarr(NNx,NNy,3) + 0B ;Black Background
  backgnd = bytarr(NNx,NNy,3) + 255B ;White Background
  IF keyword_set(lic_transparency) THEN BEGIN
    minalpha=min(lic_transparency)>0
    maxalpha=max(lic_transparency)<1
  ENDIF ELSE BEGIN
    minalpha=0.5
    maxalpha=1.
  ENDELSE
  alpha=use_lic_map-min(use_lic_map)
  alpha=alpha/max(alpha)                     ;now from 0. to 1.
  alpha=(maxalpha-minalpha)*alpha+minalpha   ;now from minalpha to maxalpha
  FOR i=0L,2 DO image[*,*,i]=image[*,*,i]*alpha+backgnd[*,*,i]*(1.-alpha)
ENDIF
help,image
tv,image,posx+1.,posy+1.,xsize=x_size,ysize=y_size,/device,true=3

IF keyword_set(axis_color_table) THEN loadct,axis_color_table
; =================== Doing the Contour
;IF not keyword_set(pcl) THEN BEGIN
  ppos=[posx,posy,posx+x_size,posy+y_size]
;ENDIF ELSE BEGIN
;  color=!d.n_colors-1
;  ppos=[!p.position(0)*!d.x_size,!p.position(1)*!d.y_size, $
;             !p.position(2)*!d.x_size,!p.position(3)*!d.y_size]
;  ssx=ppos(2)-ppos(0)
;  ssy=ppos(3)-ppos(1)
;ENDELSE

IF keyword_set(levels) THEN BEGIN
   IF keyword_set(axis_color_table) THEN loadct,axis_color_table,rgb_table=rgbtab
   N_set=N_tags(levels)
  FOR i=0,N_set-1 DO BEGIN
    levs=[levels.(i).value]
    si=size(levs) & Nlev=si(1)
    cont_im=levels.(i).image
;    miss=levels.(i).missing
;    stop
    FOR j=0,Nlev-1 DO BEGIN
;      stop
       col=[levels.(i).color(j)]
       if keyword_set(postcript) or keyword_set(postscript) then begin
          r=rgbtab[levels.(i).color(j),0]
          g=rgbtab[levels.(i).color(j),1]
          b=rgbtab[levels.(i).color(j),2]
          col=r+g*256L+b*(256L)^2
       end
      col2=[!d.n_colors/2.+col MOD !d.n_colors]
      thi=[levels.(i).thick(j)]
      listy=[levels.(i).linestyle(j)]
      dou=levels.(i).double(j)
      spli=levels.(i).spline(j)
      lev=[levels.(i).value(j)]
      lab=levels.(i).label(j)
      downh=levels.(i).downhill(j)
      miss=levels.(i).missing(j) & minval=miss
      csiz=levels.(i).charsize(j)
      clab=0
      IF lab NE '' THEN clab=1
      IF dou NE 0 THEN BEGIN
        contour,cont_im,xstyle=5,ystyle=5,/noerase,levels=lev,pos=ppos,/dev, $
        spline=spli,follow=spli,c_thick=thi*2.,c_color=col2,min_value=minval, $
        c_label=clab,c_linestyle=listy,min_value=!indef+1,/closed,charsize=mlabelsize
      ENDIF
;      stop
      contour,cont_im,xstyle=5,ystyle=5,/noerase,levels=lev,pos=ppos,/dev, $
        spline=spli,follow=spli,c_colors=col,c_annot=[lab], $
        downhill=downh,c_thick=thi,c_linestyle=listy,c_label=clab,c_charsize=csiz, $
        min_value=!indef+1,/closed,charsize=charsize
    ENDFOR
  ENDFOR
ENDIF ELSE BEGIN
  ;This is strange, but absolutely needed to set the ranges etc for the axis
   lev_bidon=[!indef-10.]
  IF not rgb_on THEN BEGIN
    contour,a_inv,xstyle=5,ystyle=5,/noerase,levels=lev_bidon,pos=ppos,/dev, $
            min_value=!indef+1,/closed,charsize=mlabelsize
  ENDIF ELSE BEGIN
    contour,a_inv[*,*,0],xstyle=5,ystyle=5,/noerase,levels=lev_bidon,pos=ppos,/dev, $
            min_value=!indef+1,/closed,charsize=mlabelsize
  ENDELSE
ENDELSE

IF not keyword_set(nologo) THEN BEGIN
  strlogo='!6IDL JPBlib V'+!jpblib_version
  csize_logo=0.7
;  width_logo=10*csize_logo
  width_logo=!d.y_ch_size
  xlogo=ppos(2)+width_logo
  ylogo=ppos(1)
  alig_logo=0
  xyouts,xlogo,ylogo,strlogo,align=alig_logo,orient=90.,/device,charsize=csize_logo
ENDIF
;IF keyword_set(double) THEN BEGIN
;    contour,bb_im,xstyle=1,ystyle=1,/noerase,levels=lev, $
;        pos=ppos,/dev, $
;        spline=spline,follow=spline,c_thick=!p.thick+3. ; ,color=color
;ENDIF

;stop
;This plots contours, if any, but no axis
;contour,bb_im,xstyle=5,ystyle=5,/noerase,levels=lev,pos=ppos,/dev, $
;        spline=spline,follow=spline,c_colors=ccolor ;,color=color

;create_coo2,huse,coo1,coo2
mk_grid_val,huse,xpos,ypos,vv,aa,nmx,nmy,l_grid,b_grid,gridx,gridy,coo1,coo2, $
	    grid=grid,silent=silent,no_x_label=no_x_label, $
	    no_y_label=no_y_label,position_x=position_x,position_y=position_y, $
;	    no_y_label=no_y_label,position_x='low',position_y='middle', $
            ra_grid=ra_grid,dec_grid=dec_grid,hfi_plot=hfi_plot,no_units=no_units

;=============== Plot the grid
IF keyword_set(grid) THEN BEGIN
;  Added JPB March 6 1998 to have all grids, including those not on
;  the lower axis
   IF keyword_set(values_grid_coo1) THEN BEGIN
     l_grid=values_grid_coo1
   ENDIF ELSE BEGIN
     l_grid=ra_grid*!radeg
   ENDELSE
   IF keyword_set(values_grid_coo2) THEN BEGIN
     b_grid=values_grid_coo2
   ENDIF ELSE BEGIN
     b_grid=dec_grid*!radeg
   ENDELSE
;   stop
;  IF not keyword_set(coo1) THEN BEGIN
;;   This is to plot l,b grid (values to plot are in l_grid,b_grid)
;    plot_grid,gridx,gridy
;  ENDIF ELSE BEGIN
    IF keyword_set(rebin) THEN BEGIN
      coo=rebin(coo1,naxis1,naxis2)
    ENDIF ELSE BEGIN
      coo=coo1
    ENDELSE
    ind=where(l_grid LT 0,count)
    IF count NE 0 THEN l_grid(ind)=l_grid(ind)+360.
    sl=sort(l_grid) & sb=sort(b_grid)
    all_lev=l_grid(sl)
    all_lev=all_lev(uniq(all_lev(sort(all_lev))))
    si=size(all_lev) & Nlev=si(1)
;    stop
    IF Nlev LT 30 THEN BEGIN
      contour,coo,levels=all_lev,/noerase,xstyle=5,ystyle=5,title=' ', $
      pos=ppos,/dev,color=ccolor
    ENDIF ELSE BEGIN
      ncou=Nlev/30
      FOR i=0,ncou do begin
	deb=i*30 & fin=(deb+29)<(Nlev-1)
	levs=all_lev(deb:fin)
        contour,coo,levels=levs,/noerase,xstyle=5,ystyle=5,title=' ', $
        pos=ppos,/dev,color=ccolor
      ENDFOR
    ENDELSE
    IF keyword_set(rebin) THEN BEGIN
      coo=rebin(coo2,naxis1,naxis2)
    ENDIF ELSE BEGIN
      coo=coo2
    ENDELSE
    contour,coo,levels=b_grid(sb),/noerase,xstyle=5,ystyle=5,title=' ', $
    pos=ppos,/dev,color=ccolor
;  ENDELSE
ENDIF

;goto,jump_axis
;================== Draw the axis
if keyword_set(xlabel) then xtitle=xlabel
if keyword_set(ylabel) then ytitle=ylabel
axis_jpb,xaxis=0,color=axis_color_value,xtitle=xtitle,charsize=labelsize ;lower axis
axis_jpb,yaxis=0,color=axis_color_value,label_ori=90,ytitle=ytitle,charsize=labelsize ;left axis

get_x=!x                           ;This is just for outputing !x through get_x keyword
get_y=!y                           ;This is just for outputing !y through get_y keyword
;== This is to get no label on right and upper axis
!x.tickname(*)=' '
!y.tickname(*)=' '
axis_jpb,yaxis=1,ytitle=' ',color=axis_color_value  ;right axis
!x.ticks=0 & !y.ticks=0
;== This is for the upper axis to have ticks at right position
mk_grid_val,huse,xpos,ypos,vv,aa,nmx,nmy,l_grid,b_grid,gridx,gridy,coo1,coo2, $
	    grid=grid,silent=silent,no_x_label=no_x_label, $
;            no_y_label=no_y_label,position_x='high'
;            no_y_label=no_y_label,position_x='middle'
            no_y_label=no_y_label,position_x=position_x,position_y=position_y,no_unit=no_units
!x.tickname(*)=' '
axis_jpb,xaxis=1,xtitle=' ',color=axis_color_value ;upper axis

;================== End of Draw the axis
;If ticks were at same postion on upper and lower axis, the above could almost be replaced by:
;jump_axis:
;plot, [0,1], [0,1],/nodata,/noerase,color=axis_color_value
;Except that then, the tics positions seem wrong on both axis.

;stop
IF ps_on THEN BEGIN
;  alpha_coef=2.3853E-5	;"theoretical value"
  alpha_coef=2.3E-5	;fine tuning to get the right size on printer !
ENDIF ELSE BEGIN
  alpha_coef=8.283E-4
ENDELSE
alpha_coef=alpha_coef*200.
size_coef=alpha_coef/abs(cdelt1*!radeg)/naxis1*(ppos(2)-ppos(0))
;=============== Plot the Overlaid symbols
IF keyword_set(overlay) THEN BEGIN
   IF keyword_set(axis_color_table) THEN loadct,axis_color_table,rgb_table=rgbtab
   N_set=n_tags(overlay)
  FOR i=0L,N_set-1 DO BEGIN
    overx=overlay.(i).coo1
    overy=overlay.(i).coo2
    coo2pix2,overx,overy,xpix,ypix,silent=silent
    si=size(overx) & Npoints=si(1)
    FOR j=0L,Npoints-1 DO BEGIN
      name=overlay.(i).label(j)
      IF name EQ '' THEN BEGIN
        ;second term on angle is to align with coordinate system
        ;-1. is for the convention to be positive counterclockwise when ra
        ;increases to the left
        rotsym=(-1.)*overlay.(i).sym_angle(j)+coo2para(huse,overlay.(i).coo1(j),overlay.(i).coo2(j))
        psym=overlay.(i).sym_type(j)
        fill=overlay.(i).sym_fill(j)
        double=overlay.(i).sym_double(j)
        csize=size_coef*overlay.(i).sym_size(j)
        color=overlay.(i).sym_color(j)
        if keyword_set(postcript)  or keyword_set(postscript) then begin
           r=rgbtab[overlay.(i).sym_color(j),0]
           g=rgbtab[overlay.(i).sym_color(j),1]
           b=rgbtab[overlay.(i).sym_color(j),2]
           color=r+g*256L+b*(256L)^2
        end
        thick=overlay.(i).sym_thick(j)
        csizex=size_coef*overlay.(i).sym_sizex(j)
        jpb_plotsym,psym,csize,fill=fill,rotation=rotsym,sizex=csizex,thick=thick
        !psym=8
        oplot,[xpix(j)],[ypix(j)],color=color ;,thick=thick
        IF double NE 0 THEN BEGIN
          color2=color+244/2.
          jpb_plotsym,psym,csize-csize/2,fill=fill,rotation=rotsym,sizex=csizex,thick=thick
          !psym=8
;          stop
          oplot,[xpix(j)],[ypix(j)],color=color2;,thick=thick ;,xtit=' ',ytit=' '
        ENDIF
      ENDIF ELSE BEGIN
        csize=overlay.(i).label_size(j)
        color=overlay.(i).label_color(j)
        if keyword_set(postcript) or keyword_set(postscript) then begin
           r=rgbtab[overlay.(i).label_color(j),0]
           g=rgbtab[overlay.(i).label_color(j),1]
           b=rgbtab[overlay.(i).label_color(j),2]
           color=r+g*256L+b*(256L)^2
        end
        thick=overlay.(i).label_thick(j)
        orient=overlay.(i).label_angle(j)
        align=overlay.(i).label_alignment(j)
        IF xpix(j) GE 0 and xpix(j) LE naxis1-1 and $
	                ypix(j) GE 0 and ypix(j) LE naxis2-1 THEN BEGIN
          xyouts,xpix(j),ypix(j),name,charsize=csize,color=color, $
	         charthick=thick,align=align,orientation=orient
        ENDIF
      ENDELSE
    ENDFOR
 ENDFOR
ENDIF

;stop
;=============== Plot polarization if needed
IF keyword_set(polar) THEN BEGIN
   IF keyword_set(axis_color_table) THEN loadct,axis_color_table,rgb_table=rgbtab
   N_set=n_tags(polar)
  FOR i=0L,N_set-1 DO BEGIN
    overx=polar.(i).coo1
    overy=polar.(i).coo2
    h2pix,h,/silent
    coo2pix2,overx,overy,xpix,ypix,silent=silent
    si=size(overx) & Npointsx=si(1) & Npointsy=si(2)
;    stop
    indok=where(polar.(i).sym_size NE !indef,countok)
    jk=index2ij(indok,[Npointsx,Npointsy])
    IF countok NE 0 THEN BEGIN
      FOR kk=0L,countok-1 DO BEGIN
        j=jk(kk,0) & k=jk(kk,1)
;      ENDFOR
;    ENDIF
;    FOR j=0L,Npointsx-1 DO BEGIN
;      FOR k=0L,Npointsy-1 DO BEGIN
;        IF polar.(i).sym_size(j,k) NE !indef THEN BEGIN
          rotsym=polar.(i).sym_angle(j,k)
          psym=polar.(i).sym_type(j,k)
          fill=polar.(i).sym_fill(j,k)
          double=polar.(i).sym_double(j,k)
          csize=size_coef*polar.(i).sym_size(j,k)
          color=polar.(i).sym_color(j,k)
          if keyword_set(postcript) or keyword_set(postscript) then begin
             r=rgbtab[polar.(i).sym_color(j,k),0]
             g=rgbtab[polar.(i).sym_color(j,k),1]
             b=rgbtab[polar.(i).sym_color(j,k),2]
             color=r+g*256L+b*(256L)^2
          end
          thick=polar.(i).sym_thick(j,k)
;        stop
          jpb_plotsym,psym,csize,fill=fill,rotation=rotsym,thick=thick
          !psym=8
;The following is to go from the coordinate indexes (xpix,ypix)
;to the oplot indexes.
;CAUTION: in principle, the same should be done for overlays above
          ijcoo2ijplot,xpix(j,k),ypix(j,k),ioplot,joplot,Naxis1,Naxis2
;        ioplot=(1.*Naxis1-1)/(Naxis1)*(xpix(j,k)+0)+0.5*(Naxis1-1)/Naxis1
;        joplot=(1.*Naxis2-1)/(Naxis2)*(ypix(j,k)+0)+0.5*(Naxis2-1)/Naxis2
          oplot,[ioplot],[joplot],color=color;,thick=thick
          IF double NE 0 THEN BEGIN
            color2=color+244/2.
            jpb_plotsym,psym,csize-csize/2,fill=fill,rotation=rotsym,thick=thick
            !psym=8
;          stop
;          oplot,[xpix(j,k)],[ypix(j,k)],color=color2,thick=thick,xtit=' ',ytit=' '
            oplot,[ioplot],[joplot],color=color2;,thick=thick ;,xtit=' ',ytit=' '
          ENDIF
;        ENDIF
;      ENDFOR
;    ENDFOR
      ENDFOR
    ENDIF
  ENDFOR
ENDIF

;stop

;==================== plot the bar if needed
IF keyword_set(bar_pos) OR keyword_set(off_bar_pos) THEN BEGIN
  IF not keyword_set(bar_title) THEN use_bar_title=' ' ELSE  use_bar_title=bar_title
  IF not keyword_set(bar_charsize) THEN BEGIN
    bar_charsize=!x.charsize
  ENDIF
  IF not keyword_set(bar_minmax) THEN BEGIN
    IF keyword_set(imrange) THEN BEGIN
      bar_minmax=imrange
    ENDIF ELSE BEGIN
      bar_minmax=[min(a_inv),max(a_inv)]
    ENDELSE
  ENDIF
  IF not keyword_set(bar_tickv) THEN BEGIN
    bar_tickv=indgen(11)/10.*(bar_minmax(1)-bar_minmax(0))
    bar_tickname=strtrim(string(round(bar_tickv)),2)
  ENDIF
  
  !x.tickv=bar_tickv & sibar=size(bar_tickv) & !x.ticks=sibar(1)-1
  !x.tickname=bar_tickname & !mtitle=use_bar_title
  !x.charsize=bar_charsize
  !x.ticklen=-0.2 & !x.minor=0
  mibar=min(bar_minmax) & mabar=max(bar_minmax)
  print,'mibar=',mibar,'mabar=',mabar
  rev=0
  IF keyword_set(invert) THEN BEGIN
    rev=1
  ENDIF
  IF keyword_set(bar_pos) THEN BEGIN
      my_bar20,type='yr',bar_pos=bar_pos,min=mibar,max=mabar,rev=rev, $
               pcl=pcl,print=print,title=use_bar_title,axis_color_value=axis_color_value,hfi_plot=hfi_plot, $
               axis_color_table=axis_color_table,image_color_table=image_color_table
  ENDIF ELSE BEGIN
      my_bar20,type='yr',off_bar_pos=off_bar_pos,min=mibar,max=mabar, $
               rev=rev,pcl=pcl,print=print,title=use_bar_title,axis_color_value=axis_color_value,hfi_plot=hfi_plot, $
               axis_color_table=axis_color_table,image_color_table=image_color_table
  ENDELSE
ENDIF


;================= close the file and print
IF ps_on THEN BEGIN
  IF not keyword_set(noclose) THEN BEGIN
    device,/close
    previous_psfile=''
    IF !version.os NE 'MacOS' THEN set_plot,'X' ELSE set_plot,'MAC'
  ENDIF
ENDIF

;========= restore old parameters
rest_graph_var

;========= store last parameters used for image_cont20
last_header=huse
last_sz=sz & last_sx=1.*FIX(sx) & last_sy=1.*FIX(sy)
last_pos=[posx,posy]

sortie:
!p.position=old_pposition

END
