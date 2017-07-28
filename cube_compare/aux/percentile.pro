FUNCTION percentile,array,perc,low=low,values=values,Nvalues=Nvalues,indexes=indexes

;+
; NAME:
;       percentile
; CALLING SEQUENCE:
;       result=percentile(array,perc,low=low)
; PURPOSE:
;       Makes a binned correlation plot
; INPUTS:
;       array = input array
;       perc =percentile
; OPTIONAL INPUT:
;       low   = If set compute lower percentile
; OUTPUTS:
;	result= percentile value
;       values= values of the upper/lower percentile
;       Nvalues= # of elements of values
;       indexes= indexes of above in original array
; OPTIONAL OUTPUT:
;       None
; PROCEDURE AND SUBROUTINE USED
;       None
; SIDE EFFECTS:
;       None
; EXAMPLE:
;       
; MODIFICATION HISTORY:
;       written by Jean-Philippe Bernard
;-

IF N_PARAMS(0) LT 1 THEN begin
  print,'result=percentile(array,perc,low=low)'
  print,'perc in %'
  sarray=[0] & x=0L
  GOTO,sortie
ENDIF

sarray=array
ind=where(finite(sarray) EQ 1,count)
sarray=sarray[ind]

N=n_elements(sarray)
order=sort(sarray)
sarray=sarray(order)  ;sorted array

IF keyword_set(low) THEN BEGIN
  x=long(N*(perc/100.))>0<(N-1)
  values=sarray(0:x)
  indexes=order(0:x)
ENDIF ELSE BEGIN
  x=N-long(N*(perc/100.))>0<(N-1)
  values=sarray(x:N-1)
  indexes=order(x:N-1)
ENDELSE
Nvalues=n_elements(values)

sortie:

RETURN,sarray(x)

END
