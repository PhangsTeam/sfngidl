FUNCTION LA_CLEAN, var, dim=dim, nundef=nundef, nclean=nclean, $
                   newundef=newundef, mask=mask
;+ 
; NAME: LA_CLEAN
; PURPOSE:
;   transform a lacunar array into a classical one 
;   replacing undefined values by the mean of its closest defined neighbours.
;   By default use every neighbour following every dimension.
;   If less than two are defined, add one to nundef and keep it undefined.  
; CATEGORY: I-5-d
; CALLING SEQUENCE: 
;   output=LA_CLEAN(var, dim=dim, nundef=nundef, nclean=nclean)
; INPUTS: 
;   var            -- lacunar array
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   dim            -- int or int array : dimensions following which mean is
;                     computed
;   mask           -- bytarr of size size and dim as var1 : undefined values
;                     mask (non zero masked values are considered as undefined)
; OUTPUTS: 
;   output         -- array of same size and type as var
; KEYED OUTPUTS:
;   nundef         -- int : number of non treated values (i.e number of
;                           undefined values into output)
;   nclean         -- int : number of treated values (nundef + nclean = number
;                           of undefined values into var) 
;   newundef       -- if given undefined values are replaced by newundef
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE: 
;   ICE> print, la_clean([2, la_undef(), 6])
;          2       4       6
;   ICE> print, la_clean([2, la_undef(), 6], newundef=0)
;          2       0       6
; ALGORITHM: 
;   compute offsets between one pixel and its neighbours following required
;   dimension
;   set output to var
;   for each undefined value,
;   . compute neighbours position
;   . check it is not out of bounds
;   . select defined pixels between allowed neighbours
;   . if more than two defined pixels replace undefined value by the mean in
;     output and increment nclean else increment nundef
; DEPENDENCIES: 
;   none
; COMMON BLOCKS: 
;   SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;   none 
; RESTRICTIONS: 
;   UNTESTED
; CALLED PROCEDURES AND FUNCTIONS:
;   LA_UNDEF
;   LA_PROD
; MODIFICATION HISTORY: 
;    22-Jan-1995  written with template_gen              FV IAS
;    08-Nov-1995  add key newundef                       FV IAS
;-
 
;------------------------------------------------------------
; common blocks 
;------------------------------------------------------------
 
; environment parameters 
 COMMON SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL
 
;------------------------------------------------------------
; on error conditions
;------------------------------------------------------------
 ON_ERROR,  ERROR_CURRENT
 
;------------------------------------------------------------
; initialization
;------------------------------------------------------------
 
 ROUTINE_NAME = 'LA_CLEAN'
 VERSION = '1.0' 
 CATEGORY = 'I-5-d'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['var', '/dim', 'output']
  s_var = CONV_STRING(var)
  s_dim = CONV_STRING(dim)
 CALL_VAL = [s_var, s_dim, '']
 output=var
 nundef = 0
 nclean = 0
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: ', $ 
    'output=LA_CLEAN(var, dim=dim, nclean=nclean, nundef=nundef, mask=mask)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF

 s_var = size(var)
 tva = s_var(s_var(0)+1)

 IF tva LT 1 or (tva GT 5) THEN BEGIN
    status(0) = ['Wrong type for var :'+ strtrim(tva), 'E']
    GOTO, CLOSING
 ENDIF

 undef = LA_UNDEF(tva)

 ; check dim
 IF N_ELEMENTS(dim) eq 0 THEN dim = indgen(s_var(0))+1
 bad_dim = where(dim GT s_var(0),cpt)
 IF cpt GT 0 THEN BEGIN
    status(0) = ['Wrong dim :' + strtrim(dim,2) + ' max. expected :' $
                + strtrim(s_var(0),2), 'E']
    GOTO, CLOSING
 ENDIF
 
 IF n_elements(mask) eq 0 THEN mask = 0

;------------------------------------------------------------
; function body
;------------------------------------------------------------

 bad_pixels = where(var eq undef or mask ne 0, cpt)

 IF n_elements(newundef) eq 1 THEN BEGIN
    IF cpt GT 0 THEN output(bad_pixels) = newundef
    GOTO, CLOSING
 ENDIF

 ; compute offsets between neighbours following dimensions
 offsets = intarr(n_elements(dim)+1)
 offsets(*) = 1 
 FOR j=1,n_elements(dim) DO offsets(j) = LA_PROD(s_var(1:dim(j-1)))
 no = n_elements(offsets)-1
 tot = n_elements(var)

 FOR i=0l, (cpt-1) DO BEGIN
     ; extract neighbours location
     pix_pos = bad_pixels(i)
     nnplus = pix_pos + offsets(0:no-1) 
     nmoins = pix_pos - offsets(0:no-1)

     ; select out of bounds offsets
     non_bound_plus = where ( $
         ((pix_pos mod offsets(1:*)) + offsets(0:no-1)) LT offsets(1:*), $
         cpt_plus) 
     non_bound_moins = where( $
         ((pix_pos mod offsets(1:*)) - offsets(0:no-1)) GE 0, cpt_moins)

     ; set valid neighbours
     IF cpt_plus GT 0 THEN IF cpt_moins GT 0 THEN $
        neigh = [nnplus(non_bound_plus), nmoins(non_bound_moins)] $
     ELSE neigh = nnplus(non_bound_plus) $
     ELSE IF cpt_moins GT 0 THEN $
        neigh = nmoins(non_bound_moins) $
     ELSE neigh = pix_pos

     ; erase undefined neighbours pixels
     good_neigh = where(var(neigh) ne undef, cpt_neigh)
     IF cpt_neigh GT 0 THEN neigh = neigh(good_neigh) 

     ; compute mean value if and only if there are at least two defined
     ; values
     IF cpt_neigh LT 2 THEN nundef = nundef + 1 $
     ELSE BEGIN 
        ; replace by the mean
        output(pix_pos)= total(var(neigh)) / cpt_neigh
        nclean = nclean + 1
     ENDELSE
 ENDFOR 
 
;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_var = CONV_STRING(var)
  s_dim = CONV_STRING(dim)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_var, s_dim, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
