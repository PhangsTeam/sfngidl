FUNCTION TME_SEC_DATE, sec 
;+ 
; NAME: TME_SEC_DATE
; PURPOSE: 
;       Convert  Seconds since January 1 1990 in TIME
;       (Same format than the IDL system variable !STIME).
; CATEGORY: I-4-c
; CALLING SEQUENCE: 
;   output=TME_SEC_DATE(sec)
; INPUTS: 
;   sec        -- double : seconds since 1-1-1990
; OPTIONAL INPUT PARAMETERS: 
;   none
; KEYED INPUTS: 
;   none
; OUTPUTS: 
;    output    -- string : TIME= e.g. '10-JUL-1990 12:30'
; OPTIONAL OUTPUT PARAMETERS: 
;   none
; EXAMPLE:
;  ICE> help, tme_sec_date(365l*24l*3600l*4)
;  <Expression>    STRING    = '31-DEC-1993 00:00:00.00'
; ALGORITHM: 
;  straightforward
; DEPENDENCIES: 
;  none
; COMMON BLOCKS: 
;    SESSION_BLOCK, SESSION_MODE, ERROR_CURRENT, STATUS_BOOL 
; SIDE EFFECTS:
;   none 
; RESTRICTIONS: 
;   Incorrect after year 2000
; WTBD : add a key with variant initial date
; CALLED PROCEDURES AND FUNCTIONS: 
;   none
; MODIFICATION HISTORY: 
;    10-Nov-1995  written with template_gen         FV IAS
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
 
 ROUTINE_NAME = 'TME_SEC_DATE'
 VERSION = '1.0' 
 CATEGORY = 'I-4-c'
 STATUS = ['SUCCESS', 'S', ROUTINE_NAME+ ' V.' + VERSION, CATEGORY]
 VAR_NAMES= ['sec', 'output']
  s_sec = CONV_STRING(sec)
 CALL_VAL = [s_sec, '']
 output=''
 
;------------------------------------------------------------
; parameters check
;------------------------------------------------------------
 
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, 'CALLING SEQUENCE: output=TME_SEC_DATE(sec)'
   STATUS(0) = ['PARAMETER MISSING', 'E']
   GOTO, CLOSING
 ENDIF
 
;------------------------------------------------------------
; function body
;------------------------------------------------------------
 
;---------------------------------------------------------------
; Number of days since the 1 January 1990 and the beginning of the current year
;---------------------------------------------------------------
N_DAYS =  [ 0, 365, 365, 366, 365, 365, 365, 366, 365, 365, 365 ]
DAYS_BEFORE= LONARR( N_ELEMENTS( N_DAYS))
FOR I= 0, N_ELEMENTS( N_DAYS)- 1 DO DAYS_BEFORE( I) =TOTAL ( N_DAYS ( 0: I))

;---------------------------------------------------------------
; Conversion
;---------------------------------------------------------------
SECONDS = DOUBLE ( SEC ) 
N_DAYS= LONG (SECONDS  / ( DOUBLE(3600) * 24.) )
GOOD= WHERE( (N_DAYS- DAYS_BEFORE) GE 0, COUNT) 
IF COUNT LT 0 THEN BEGIN 
	STATUS(0) = 'INPUT NOT CORRECT' 
	STATUS(1) = 'E'
	GOTO, CLOSING
ENDIF
YEAR= 1990+ MAX( GOOD)
SECONDS_YEAR = SECONDS - DAYS_BEFORE( MAX( GOOD))* 3600L * 24
DAYS = LONG ( DOUBLE(SECONDS_YEAR) / ( 24L * 3600) ) + 1 
SECONDS_DAYS = SECONDS_YEAR - ( DAYS - 1 ) * 24L * 3600

;---------------------------------------------------------------
; Number of days since the 1 January of the current year
;---------------------------------------------------------------
N_DAYS =  [ 0 , 31 , 28 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31 ]
IF ( ( YEAR EQ 1992 ) OR ( YEAR EQ 1996 ) ) THEN N_DAYS (2) = 29   ;!!!
TOTAL_DAYS = LONARR (13) 
FOR I = 0 , 12 DO TOTAL_DAYS(I) = TOTAL ( N_DAYS ( 0 : I ) ) 

GOOD = WHERE ( ( DAYS - TOTAL_DAYS ) GT 0 , COUNT )  
MONTH = COUNT ; Number of month
DAY = LONG ( DAYS - TOTAL_DAYS ( MONTH - 1 ) )
HOUR = LONG ( DOUBLE(SECONDS_DAYS) / 3600. ) 
SECONDS_HOURS = SECONDS_DAYS - HOUR * 3600
MINUTE = LONG ( DOUBLE(SECONDS_HOURS) / 60. ) 
SECONDS_MINUTE = SECONDS_HOURS - MINUTE * 60
SECOND = LONG ( SECONDS_MINUTE )

; add 0.5 avoiding fasle rounding
CENTI_SECOND = LONG ( ( SECONDS_MINUTE - SECOND) * 100 + 0.5)

CASE MONTH OF
	 01 : S_MONTH = 'JAN'
	 02 : S_MONTH = 'FEB'	
	 03 : S_MONTH = 'MAR'
	 04 : S_MONTH = 'APR'
	 05 : S_MONTH = 'MAY'
	 06 : S_MONTH = 'JUN'
	 07 : S_MONTH = 'JUL'
	 08 : S_MONTH = 'AUG'
	 09 : S_MONTH = 'SEP'
	 10 : S_MONTH = 'OCT'
	 11 : S_MONTH = 'NOV'
	 12 : S_MONTH = 'DEC'	
    ELSE : BEGIN & status(0) = ['Out of bound date', 'E']
           GOTO, CLOSING & END
ENDCASE

S_HOUR = STRTRIM(STRING ( HOUR ) , 2 ) 
IF (HOUR LT 10) THEN S_HOUR = '0' + S_HOUR

S_MINUTE = STRTRIM(STRING ( MINUTE ) , 2 ) 
IF (MINUTE LT 10) THEN S_MINUTE = '0' + S_MINUTE

S_SECOND = STRTRIM(STRING ( SECOND ) , 2 ) 
IF (SECOND LT 10) THEN S_SECOND = '0' + S_SECOND

S_CENTI_SECOND = STRTRIM(STRING ( CENTI_SECOND ) , 2 ) 
IF (CENTI_SECOND LT 10) THEN S_CENTI_SECOND = '0' + S_CENTI_SECOND

S_YEAR = STRTRIM(STRING ( YEAR ) , 2 )

S_DAY  = STRTRIM(STRING ( DAY ) , 2 )
IF (DAY LT 10) THEN S_DAY = '0' + S_DAY

 output = S_DAY + '-' + S_MONTH + '-' + S_YEAR + $
	' ' + S_HOUR + ':' + S_MINUTE + ':' + S_SECOND + '.' + S_CENTI_SECOND

;------------------------------------------------------------
; closing
;------------------------------------------------------------
 
 CLOSING:
  s_sec = CONV_STRING(sec)
  s_output = CONV_STRING(output)
  ACTL_VAL = [s_sec, s_output]
 
  IF (STRMID(STATUS(1),0,1) NE 'S') THEN STATUS_BOOL=1 
  RECORD_LOGFILE, STATUS, CALL_VAL, VAR_NAMES, ACTL_VAL
 
  RETURN, output
 
 END
