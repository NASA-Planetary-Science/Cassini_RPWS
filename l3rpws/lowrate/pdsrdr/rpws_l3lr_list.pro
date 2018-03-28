;-----------------------------------------------------------------------
;	lrf_list reads Cassini RPWS Low Rate Full Calibration Data
;	sets and produces an ASCII text fields of the data.

;	Written by Robert Johnson
;	Date: December 20, 2003

;	Converted to IDL by Julie Dowell
;	February 8, 2007
;-----------------------------------------------------------------------

;------------------------------------------------------------
;	Converts the standard JPL binary scet into a string.
;	    nDoy  - is the number of days since Jan. 1, 2958
;	    nMsec - is the milliseconds since the beginning of the day
;	    pSin  - is a pointer to the return string. If pSin is a
;		NULL pointer, the string is statically stored in the function.
;	Returns:  The standard JPL spacecraft event time format as a string,
;			year-doyThh:mm:ss.mil 
;---------------------------------------------------------------
function nScet_to_sScet, nDoy, nMsec, first_date

	nYear = 0	&	nhour = 0	&	nMin = 0
	nSec = 0	&	nDays = 0	&	nTotal = 0
	tnDoy = nDoy	&	tnMsec = nMsec
	arStr = ''

	ms_per_day = long(24.0*60.0*60.0*1000.0)

	; normalize the incoming data

	if (tnMsec LT ms_per_day) then	Ready = 1 $
	else	Ready = 0

	while (NOT Ready) do begin
		tnMsec = tnMsec - ms_per_day	&	tnDoy = tnDoy + 1
		if (tnMsec GT ms_per_day) then	Ready = 1 $
		else if (tnMsec EQ ms_per_day) then	Ready = 1
	endwhile

	; convert days since Jan. 1, 1958 [0-356] to day of year [1-366]
	tnDoy = tnDoy + 1

	Days = 365	&	Year = 1959	; initial conditions 365 days in 1958
	nTotal = Days

	while (tnDoy GT nTotal) do begin
		if (Year mod 100 GT 0) then begin	; Year is NOT a century year
			if (Year mod 4 GT 0) then Days = 365 $
			else	Days = 366	; if mult. of 4, leap year
		endif else begin		; Year is a century year
			if (Year mod 400 GT 0) then	Days = 365 $
			else	Days = 366	; if mult of 400, leap year
	endelse

	nTotal = nTotal + Days		&	Year = Year + 1
	endwhile

	Year = Year - 1	& 	nTotal = nTotal - Days  
	tnDoy = tnDoy - nTotal	; days since jan 1 [0-365]

	tMsec = tnMsec mod 1000	&	tnMsec = tnMsec/1000
	nSec = tnMsec mod 60	&	tnMsec = tnMsec/60
	nMin = tnMsec mod 60	&	nHour = tnMsec/60

	arStr = string(Year, '-', tnDoy, 'T', nHour, ':', nMin, ':', nSec, $
	'.', tMsec, format = $
	'(i4.4, a1, i3.3, a1, i2.2, a1, i2.2, a1, i2.2, a1, i3.3)')
	; yyyy-doyThh:mm:ss.mil

	return, arStr
end

function get_hfr_ABC_bandwidth, arMiniPkt, first_time

	i = 0	&	fBandWidth = 0.0
	bandA = fltarr(3, 32)	&	bandB = fltarr(3, 32)
	bandC = fltarr(3, 32)

	if (NOT first_time) then begin  ; generate the hfr band ABC frequencies

		first_time = first_time + 1

		for i = 0, 7 do begin	; hfr band ABC frequencies, 8 filters
			bandA(0, i) = 3.6*4.5^((0.5+i)/8)*1.0e+03
			bandB(0, i) = 3.6*4.5^((0.5+i+8)/8)*1.0e+03
			bandC(0, i) = 3.6*4.5^((0.5+i+16)/8)*1.0e+03
		endfor

		for i = 0, 15 do begin	; hfr band ABC freqencies, 16 filters
			bandA(1, i) = 3.6*4.5^((0.5+i)/16)*1.0e+03
			bandB(1, i) = 3.6*4.5^((0.5+i+16)/16)*1.0e+03
			bandC(1, i) = 3.6*4.5^((0.5+i+32)/16)*1.0e+03
		endfor

		for i = 0, 31 do begin	; hfr band ABC frequencies 32 filters
			bandA(2, i) = 3.6*4.5^((0.5+i)/32)*1.0e+03
			bandB(2, i) = 3.6*4.5^((0.5+i+32)/32)*1.0e+03
			bandC(2, i) = 3.6*4.5^((0.5+i+64)/32)*1.0e+03
		endfor
	endif

  ; the hfr band A,B,C consists of N logrithmicly spaced channels
  ; for lots of details of decoding the mini-packet header, refer
  ; to the RPWS Users Guide.  All that we are interested is wheither
  ; the hfr was in 8, 16, or 32 filter mode

	if (NOT (arMiniPkt(9) AND '07'XB)) then begin
		; sanity check: Band A, B, or C selected
		return, 0 
	endif 
 
	if ((arMiniPkt(14) AND '03'XB) EQ 0) then begin	; 8 filters per band

		fBandWidth = alog10(bandA(0, 1)) - alog10(bandA(0, 0))	; ~0.08165

	endif else if ((arMiniPkt(14) AND '03'XB) EQ 1) then begin
		; 16 filters per band

		fBandWidth = alog10(bandA(1, 1)) - alog10(bandA(1, 0))	; ~0.04083
	endif else  begin	; bit patterns 0x03 and 0x02 imply 32 filters per band
	
		fBandWidth = alog10(bandA(2, 1)) - alog10(bandA(2, 0))	; ~0.02041
	endelse
	
; since Bands A,B,C are log spaced, it doesn't matter which we choose. 
; The calculation for bandwidth of a frequency of ABC becomes is a 
; constant delta, in log space.

	return,  fBandWidth
end

function get_hfr_HF1_bandwidth, arMiniPkt

	nFilters = 0

	if (NOT (arMiniPkt(9) AND '08'XB)) then begin
		; sanity check: Band Hf1 selected
		return, 0.0 
	endif 

	if ((arMiniPkt(15) AND '03'XB) EQ '00'XB) then $	; 1 filter per band
		nFilters = 1 $
	else if ((arMiniPkt(15) AND '03'XB) EQ '01'XB) then $  ; 2 filters per band
		nFilters = 2 $
	else if ((arMiniPkt(15) AND '03'XB) EQ '02'XB) then $  ; 4 filters per band
		nFilters = 4 $
	else if ((arMiniPkt(15) AND '03'XB) EQ '03'XB) then $  ; 8 filters per band
		nFilters = 8 

	return, (25.0e+03/nFilters)	; Bandwidth in KHz
end

function get_hfr_HF2_bandwidth, arMiniPkt

	nFilters = 0

	if (NOT(arMiniPkt(9) AND '10'XB)) then begin
		; sanity check: Band Hf2 selected
		return, 0.0 
	endif 

	if ((arMiniPkt(16) AND '03'XB) EQ '00'XB) then $	; 1 filter per band
		nFilters = 1 $
	else if ((arMiniPkt(16) AND '03'XB) EQ '01'XB) then $  ; 2 filters per band
		nFilters = 2 $
	else if ((arMiniPkt(16) AND '03'XB) EQ '02'XB) then $  ; 4 filters per band
		nFilters = 4 $
	else if ((arMiniPkt(16) AND '03'XB) EQ '03'XB) then $  ; 8 filters per band
		nFilters = 8

	return, (25.0e+03/nFilters)	; Bandwidth in KHz
end

;------------------ get_bandwidth() --------------------------------
;	calculates the bandwidth of the individual spectral 
;	density measurements.
;
;	*arFreq     array containing the frequencies of the measurements
;	nItems      number of elements in the frequency array
;	nMode       instrument id from the archive file header 
;	arMiniPkt   engineering status of the receiver from the archive
;			file header
;	Returns:
;	an array, nItems long, of bandwidths corresponding to the
;		frequencies in arFreq.  The units are in hertz.
;--------------------------------------------------------------------
function get_bandwidth, arFreq, nItems, nMode, arMiniPkt, first_time

	i = 0	&	maxItems = nItems - 1

	arBandWidth = fltarr(nItems)
	fBandwidth = 0.0	&	fUpper = 0.0
	fLower = 0.0

	;  in Hertz
	Lfdr_Bandwidth = [ 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, $
	0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, $
	0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2045, 0.2871, $
	0.5719, 0.5719, 0.7373, 0.7373, 1.0429, 1.0429, 1.3182, $
	1.4466, 1.8386, 2.0797, 2.5488, 2.6172 ]

	; in Hertz
	Mfdr_Bandwidth = [ 14.6071, 14.6071, 14.6071,  14.6071,  14.6071,  $
	14.6071,  14.6071,  14.6071, 14.6071, 14.6071, 14.6071,  $
	14.6071,  14.6071,  14.6071,  14.6071,  14.6071, 14.6071, $
	14.6071, 14.6071,  20.5071,  40.8500,  40.8500,  52.6643, $
	52.6643, 74.4929, 74.4929, 94.1571, 103.3286, 131.3286, $
	148.5500, 182.0571, 186.9429 ]

	RECEIVER_MASK = 'FF000000'XL
	LFDR_Normal = '71000000'XL	; low frequency digital receiver
	MFDR_Normal = '72000000'XL	; medium frequency digital receiver
	MFR_Normal = '11000000'XL	; medium frequency receiver
	MFR_FastToggle = '12000000'XL	; medium frequency receiver
	HFR_Analysis = '21000000'XL	; high frequency receiver
	HFR_Millisecond = '28000000'XL	; high frequency receiver
	HFR_BandABC = '0x00007000'XL	; high frequency receiver
	HFR_BandHf1 = '0x00008000'XL	; high frequency receiver
	HFR_BandHf2 = '0x00010000'XL	; high frequency receiver

	if (LFDR_Normal EQ (nMode AND RECEIVER_MASK)) then begin 

		; center freq. range 0.19Hz-24.3Hz
		for i = 0, 31 do $	; 32 log spaced channels
			arBandWidth(i) = Lfdr_Bandwidth(i)

	endif else if (MFDR_Normal EQ (nMode AND RECEIVER_MASK))  then begin

		; center freq. range 13.9Hz-1.7KHz
		for i = 0, 31 do $	; 32 log spaced channels
			arBandWidth(i) = Mfdr_Bandwidth(i)

	endif else if (MFR_Normal EQ (nMode AND RECEIVER_MASK)) then begin

		; center freq. range from 23.80Hz to 169.0Hz
		for i = 0, 31 do $	; band 1 sweeps 1 and 2 (16+16=32)
			arBandWidth(i) = 5.6

		; center freq. range from 192.11Hz to 1.470KHz
		for i = 32, 95 do $	; band 2 sweeps 1 and 2 (32+32=64)
			arBandWidth(i) = 19.4

		; band 3 sweeps 1,2,3, & 4 (32+32+32+32=128)
		; center freq. range from 1.536KHz to 11.799KHz
		for i = 96, 223 do $
			arBandWidth(i) = 139.0

	endif else if (MFR_FastToggle EQ (nMode AND RECEIVER_MASK)) then begin

		; Fast Toggle Mode

		; center freq. range from 23.80Hz to 169.0Hz
		for i = 0, 15 do $	; band 1 sweeps 1 (16) )
			arBandWidth(i) = 5.6

		; center freq. range from 192.11Hz to 1.470KHz
		for i = 16, 47 do $	; band 2 sweeps 1 (32)
			arBandWidth(i) = 19.4

		; center freq. range from 1.536KHz to 11.799KHz
		for i = 48, 112 do $	; band 3 sweeps 1 and 2 (32+32=64)
			arBandWidth(i) = 139.0

	endif else if (HFR_Analysis EQ (nMode AND RECEIVER_MASK)) then begin

		if (nMode AND HFR_BandABC) then begin		; log spaced bandwidths

			; A 3685.61 - 15823.72
			fBandWidth = get_hfr_ABC_bandwidth(arMiniPkt, first_time)
			for i = 0, maxItems do begin	; B 16585.23 -  71206.73
				fUpper = alog10(arFreq(i))	; C 74633.53 - 320430.31 */
				fLower = alog10(arFreq(i))
				fLower = fLower -  fBandWidth/2.0
				fUpper = fUPPer + fBandWidth/2.0
				arBandWidth(i) = 10.0^fUpper - 10.0^fLower
			endfor 
		endif else if (nMode AND HFR_BandHf1) then begin	; 25KHz/filters

			for i = 0, maxItems do $	; 100KHz - 4.xMHz
				arBandWidth(i) = get_hfr_HF1_bandwidth(arMiniPkt, first_time)

		endif else if (nMode AND HFR_BandHf2) then begin	; 25KHz/filters

			for i = 0, maxItems do $	; 100KHz - 16.xMHz
				arBandWidth(i) = get_hfr_HF2_bandwidth(arMiniPkt, first_time)

		endif else begin

			for i = 0, maxItems do $
				arBandWidth(i) = 1.0	; this should never be executed
		endelse
	endif else if (HFR_Millisecond EQ (nMode AND RECEIVER_MASK)) then begin

		for i = 0, maxItems do $	; 25KHz/filters
			arBandWidth(i) = 25.0e+03

	endif else begin

		for i = 0, maxItems do $
			arBandWidth(i) = 1.0	; this should never be executed

	endelse

	return, arBandWidth
end

pro show_help

	print
	print
	print, '	Usage lrf_list, keyword = value, ...'
	print
	print, $
'	lrf_list reads RPWS Cassini Low Rate Full Calibration PDS Archive files ', $
'	*.DAT.  If FILENAME is not supplied then no output will be produced'
	print

	print, 'Keywords:'
	print
	print, 'infile = filename { a string }'
	print
	print, '  NoHeader { 1 => do not output header info, default = 0 }'
	print
	print, '  NData { 1 => do not output data, default = 0 }'
	print
	print, '  DF = N  { output data format, default N = 0, more below }'
	print, '          0   s/c event time (utc) in the form 2003-175T12:45:59.246'
	print, '          1   offset from beginning of capture, 5.345 seconds'
	print
	print, '  Units = N  { units for the data samples, default N=0, more below }'
	print, '          0   spectral density [V^2/m^2/Hz] and [nT^2/Hz]'
	print, '          1   power flux [W/m^2/Hz] and [nT^2/Hz]'
	print, '          2   field [V/m] and [nT]'
	print
	print, '  NoError { 1 = silent operation, no error messages output, Def = 0 }'
	print, '  Verbose { 1 = verbose operation, be verbose in the output, Def = 0 }'
	print
	print, '  This output will be shown when no keywords are set'

end

pro lrf_list, infile = infile, NoHeader = NoHeader, NoError = NoError, $
	NData = NData, units = units, Verbose = Verbose, DF = DF

	first_time = 0
	sTmp = bytarr(128)	&	sFileName = ''
	sSensor = ''	&	first_date = 0
	key_count = 0

	if (keyword_set(infile)) then begin
		sFileName = infile
		key_count = key_count + 1
	endif

	if (keyword_set(NData)) then begin
		bData = 0
		key_count = key_count + 1
	endif else		bData = 1

	if (keyword_set(NoHeader)) then begin
		bHeader = 0
		key_count = key_count + 1
	endif else		bHeader = 1

	if (keyword_set(DF)) then begin
		nDataFormat = df
		key_count = key_count + 1
	endif else		nDataFormat = 0

	if (keyword_set(units)) then begin
		nunits = units
		key_count = key_count + 1
	endif else		nunits = 0

	if (keyword_set(NoError)) then begin
		bSilent = NoError
		key_count = key_count + 1
	endif else		bSilent = 0

	if (keyword_set(Verbose)) then begin
		bVerbose = 1
		key_count = key_count + 1
	endif else		bVerbose = 0

	if (key_count LT 1) then begin
		show_help	&	return
	endif

	if (bVerbose) then	bSilent = 0;

	; check the byte order of the machine

	if (NOT bSilent) then	print, 'lrf_list'

	if (strlen(sFileName) GT 0) then begin
		openr, hFile, sFileName, /get_lun
		on_ioerror, bad_open
		print, '	Opening ', sFileName
	endif else begin
		print, 'Error: No Input File Specified'
		exit, status = 1
	endelse

	if (bVerbose) then	print, 'reading ', sFileName

	sTmp = bytarr(16)

	; determine, the length and number of each record in the file
	readu, hFile, sTmp
	on_ioerror, bad_read

	nRecLen = long(sTmp(8))	&	nRecLen = ishft(nRecLen, 24)
	iTmp = long(sTmp(9))	&	iTmp = ishft(iTmp, 16)
	nRecLen = nRecLen OR iTmp	; explicitly promote to an 32 bit unsigned
	iTmp = long(sTmp(10))	&	iTmp = ishft(iTmp, 8)
	nRecLen = nRecLen OR iTmp	; quanity for non-ansi C compiliers.
	nRecLen = nRecLen OR long(sTmp(11))  

	nNumRec = long(sTmp(12))	&	nNumRec = ishft(nNumRec, 24)
	iTmp = long(sTmp(13))	&	iTmp = ishft(itmp, 16)
	nNumRec = nNumRec OR iTmp
	iTmp = long(sTmp(14))	&	iTmp = ishft(itmp, 8)
	nNumRec = nNumRec OR iTmp
	nNumRec = nNumRec OR long(sTmp(15))

	nItems = long((nRecLen - 16)/4)
	maxItems = nItems - 1

	; rewind file pointer to the beginning and read the entire header record
	close, hFile	&	free_lun, hFile

	arTime = fltarr(nItems)	; will hold time_offsets of each spectral density
				; after the begin time of the record
	arFreq = fltarr(nItems)	; will hold frequencies for all snapshots
	arDens = fltarr(nItems)	; will hold spectral densities for each snapshot

	outstr = ''

	; Bit patterns need to decode the 'nMode' field in the header
	MODE_MASK = '00FFF000'XL	; not used in this version
	ANT_MASK = '00000FFF'XL	; not used in this version

	RECEIVER_MASK = 'FF000000'XL
	LFDR_Normal = '71000000'XL  ; low frequency digital receiver
	MFDR_Normal = '72000000'XL  ; medium frequency digital receiver
	MFR_Normal = '11000000'XL  ; medium frequency receiver
	MFR_FastToggle = '12000000'XL	; medium frequency receiver
	HFR_Analysis = '21000000'XL	; high frequency receiver
	HFR_Millisecond = '28000000'XL	; high frequency receiver

	openr, hFile, sFileName, /get_lun
	on_ioerror, bad_open

	arHdr = bytarr(nRecLen)

	readu, hFile, arHdr
	on_ioerror, bad_read

	; receiver type & mode
	nMode = long(arHdr(16))	&	nMode = ishft(nMode, 24)
	iTmp = long(arHdr(17))	&	iTmp = ishft(iTmp, 16)
	nMode = nMode OR iTmp
	iTmp = long(arHdr(18))	&	iTmp = ishft(iTmp, 8)
	nMode = nMode OR iTmp
	nMode = nMode OR long(arHdr(19))  
 
	arMiniPkt = bytarr(24)

	for i = 0, 23 do $ ; mini-packet header, contains instrument mode
		arMiniPkt(i) = arHdr(24+i) ; info, see RPWS Users Guide for details

	tMode = nMode AND RECEIVER_MASK
	case (tMode) of
		LFDR_Normal:	sReceiver = 'Lfdr'
		MFDR_Normal:	sReceiver = 'Mfdr'
		MFR_Normal:	sReceiver = 'Mfr Normal'
		MFR_FastToggle:	sReceiver = 'Mfr Fast Toggle'
		HFR_Analysis:	sReceiver = 'Hfr Analysis'
		HFR_Millisecond:	sReceiver = 'Hfr Millisecond'
	else:	sReceiver = string(nMode, format = '(z8.8)')
	endcase

	pBuf = bytarr(nRecLen)
	if (bVerbose) then begin
	print, 'Record Length = ', nRecLen, ' (bytes), Number of Records = ', $
		nNumRec, ' and nItems = ', nItems

	print, 'first 80 bytes of the archive'
	print, format = '(16(1x, z2.2))', arHdr(0:15)
	print, '  '

	tstr = ''
	for i = 0, 15 do begin
		if (('1F'XB LT arHdr(i)) AND (arHdr(i) LT '7F'XB))  then begin
			tstr = tstr + string(arHdr(i))
		endif else	tstr = tstr + '.'
	endfor
	print, tstr

	print, format = '(16(1x, z2.2))', arHdr(16:31)

	tstr = ''
	for i = 16, 31 do begin
		if (('1F'XB LT arHdr(i)) AND (arHdr(i) LT '7F'XB)) then begin
			tstr = tstr + string(arHdr(i))
		endif else	tstr = tstr + '.'
	endfor
	print, tstr

	print, format = '(16(1x, z2.2))', arHdr(32:47)
	print, '  '
	tstr = ''
	for i = 32, 47 do begin
		if (('1F'XB LT arHdr(i)) AND (arHdr(i) LT '7F'XB)) then begin
			tstr = tstr + string(arHdr(i))
		endif else	tstr = tstr + '.'
	endfor
	print, tstr

	print, format = '(16(1x, z2.2))', arHdr(48:63)
	print, '  '
	tstr = ''
	for i = 48, 63 do begin
		if (('1F'XB LT arHdr(i)) AND (arHdr(i) LT '7F'XB)) then begin
			tstr = tstr + string(arHdr(i))
		endif else	tstr = tstr + '.'
	endfor
	print, tstr

	print, format = '(16(1x, z2.2))', arHdr(64:79)
	print, '  '
	tstr = ''
	for i = 64, 79 do begin
		if (('1F'XB LT arHdr(i)) AND (arHdr(i) LT '7F'XB)) then begin
			tstr = tstr + string(arHdr(i))
		endif else	tstr = tstr + '.'
	endfor
	print, tstr

	endif

	;  move to the beginning of the second record, time record

	; skip the first 16 bytes of information, sclk, scet, etc and 
	readu, hFile, sTmp	; skip the first 16 bytes of these records

	;  put the time offset from beginning of capture in an array
	readu, hFile, arTime

	;  move to the beginning of the third record, frequency record
	readu, hFile, sTmp
	readu, hFile, arFreq

	; the data is read in in floats, so byteorder, x, /XDRTOF will reorder
	; the bytes of x from IEEE floats to native floats, little endian
	; and in IDL we can fix an entire array in one step

	byteorder, arTime, /XDRTOF, /swap_if_little_endian
	byteorder, arFreq, /XDRTOF, /swap_if_little_endian

	; read the third record, frequency */

	readu, hFile, sTmp
	on_ioerror, bad_read

	OK = 1
	while (OK) do begin
	readu, hFile, arDens
	on_ioerror, Done

	byteorder, arDens, /XDRTOF, /swap_if_little_endian

; the beginning of each record contains the clock counter & time info

	pDword = 0L
	for i = 0, 3 do $
		pDword = (ishft(pDword, 8) OR long(sTmp(i)))

	nSclkSec = pDword

	nSclkFine = long(sTmp(5))
	nSclkFine = nSclkFine AND 'FF'XB;

	nSclkPart = long(sTmp(4))
	nSclkPart = nSclkPart AND 'FF'XB

	nScetDays = long(sTmp(6))
	nScetDays = ishft(nScetDays, 8) OR long(sTmp(7))

	pDword = 0L
	for i = 8, 11 do $
		pDword = ishft(pDword, 8) OR long(sTmp(i))

	nScetMils = pDword

	pDword = 0L
	for i = 12, 15 do $
		pDword = ishft(pDword, 8) OR long(sTmp(i))

	nDataQuality = pDword

	nSensor = nDataQuality

	nDataQuality = nDataQuality AND 'FFFFFFF0'XL

	nSensor = nSensor AND '0000000F'XL

	sDataQuality = ''

	if ((nDataQuality AND '80000000'XL) NE 0) then $
		sDataQuality = 'Packet Errors '

	if ((nDataQuality AND '40000000'XL) NE 0) then $
		sDataQuality = 'Hfr Sounder Active '

	if ((nDataQuality AND '20000000'XL) NE 0) then $
		sDataQuality = 'Lp Raw Sweep Active '

	if ((nDataQuality AND '10000000'XL) NE 0) then $
		sDataQuality = 'Ground Generated Data '


	sUnitsName = ''		&	sUnits = ''
	if (nSensor LE 3)  then begin  ; Eu,Ev,Ex,Ew
		case nUnits of
		1: begin
			sUnitsName = 'Power Flux'
			sUnits = 'W/m**2/hz'
		end
		2: begin
			sUnitsName = 'Electric Field'
			sUnits = 'V/m'
		end
		else: begin
			sUnitsName = 'Spectral Density'
			sUnits = 'V**2/m**2/hz'
		end
		endcase
	endif else if ((4 LE nSensor) AND (nSensor LE 6)) then begin  ; Bx,ByBz
		case nUnits of
		1: begin
			sUnitsName = 'Power Flux'
			sUnits = 'nT**2/hz'
		end
		2: begin
			sUnitsName = 'Magnetic Field'
			sUnits = 'nT'
		end
		else: begin
			sUnitsName = 'Spectral Density'
			sUnits = 'nT**2/hz'
		end
		endcase
	endif else begin
		sUnits = 'TBD'
		case nUnits of
		1:  sUnitsName = 'Power Flux'
		2:  sUnitsName = 'Magnetic Field'
		else: sUnitsName = 'Spectral Density'
		endcase
	endelse

	case nSensor of
		0:  sSensor = 'Ex'
		1:  sSensor = 'Eu'
		2:  sSensor = 'Ev'
		3:  sSensor = 'Ew'
		4:  sSensor = 'Bx'
		5:  sSensor = 'By'
		6:  sSensor = 'Bz'
		8:  sSensor = 'Hf'
		11: sSensor = 'Lp'
		else: sSensor, 'unknown'
	endcase

	if (nSclkPart EQ 0) then	nSclkPart = 1 

	if (bHeader) then begin
		date_string = nScet_to_sScet(nScetDays, nScetMils, first_date)
		outstr = string(sSensor, sReceiver, date_string, $
		'0x', nSclkSec, '.', nSclkFine, format = $
		'(a2, 1x, a15, 1x, a21, 1x, a2, Z8.8, a1, Z2.2)')
		print, outstr
		if (strlen(sDataQuality) GT 0) then begin
		outstr = '  ' + sDataQuality
		print, outstr
		endif
	endif
	
	if (bData) then begin

		if (bHeader) then begin
			outstr = string('S/C Event Time', 'Frequency', sUnitsName, $
			format = '(a21, 1x, a15, 1x, a17)')
			print, outstr
			outstr = string('UTC', 'hertz', sUnits, $
				format = '(a21, 1x, a15, 1x, a17)')
			print, outstr
		endif

		arBandWidth = get_bandwidth(arFreq, nItems, nMode, arMiniPkt, $
			first_time)
		if (nUnits EQ 1) then begin	; Power Flux
			if (nSensor LE 3) then begin	; Eu,Ev,Ex,Ew

				; divide by the impedance of free space
				arDens = arDens/377.0

			endif
		endif else if (nUnits EQ 2) then begin
			; Electric and Magnetic Field Strength
			for i = 0, maxItems do begin
				if (arDens(i) LT 0.0) then begin
					; negative data => interference present
					arDens(i) = arDen(i)*(-1.0)
					arDens(i) = sqrt(arDens(i)*arBandWidth(i))
					arDens(i) = arDens(i)*(-1.0)
				endif else begin
					arDens(i) = sqrt(arDens(i)*arBandWidth(i))
				endelse
			endfor
		endif

		if (nDataFormat EQ 1) then begin
			; time is seconds from beg. of capture
			for i = 0, maxItems do begin
				; static const char *sDataFormat1  = "%21.3f %15.2f %#17.5G\n";
				outstr = string(arTime(i), arFreq(i), arDens(i), $
					format = '(f21.3, f16.3, G18.5)')
				print, outstr
			endfor 
		endif else if (nDataFormat EQ 2) then begin
			; time is scet, include bandwidths
			for i = 0, maxItems do begin
					nMsec = nScetMils + long(arTime(i)*1000L)
				; static const char *sDataFormat2 = "%21s %15.2f %#17.5G   (bandwidth=%9.3f)\n";
		  		date_string = nScet_to_sScet(nScetDays, nMsec, first_date)
				outstr = string(date_string, arFreq(i), arDens(i), $
					'   (bandwidth=', arBandWidth(i), ')', format = $
					'(a21, 16.2f, G18.5, a14, f9.3, a1)')
				print, outstr
			endfor 
		endif else if (nDataFormat EQ 3) then begin
			; time is second, include bandwidths
			for i = 0, maxItems do begin
				; static const char *sDataFormat3 = "%21.3f %15.2f %#17.5G   (bandwidth=%9.3f)\n";
				outstr = string(arTime(i), arFreq(i), arDens(i), $
					'   (bandwidth=', arBandWidth(i), ')', $
					format = '(f21.3, f16.2, G18.5, a14, f9.3, a1)')
				print, outstr
			endfor 
		endif else begin	; time is scet, include bandwidths
			for i = 0, maxItems do begin
				nMsec = nScetMils + long(arTime(i)*1000)
				; static const char *sDataFormat0 = "%21s %15.2f %#17.5G\n";
				date_string = nScet_to_sScet(nScetDays, nMsec, first_date)
				outstr = string(date_string, arFreq(i), arDens(i), $
					format = '(a21, f16.2, G18.5)')
				print, outstr
			endfor 
		endelse 
	endif
	readu, hFile, sTmp
	on_ioerror, Done

	endwhile

	Done:	close, hFile	&	free_lun, hFile

	return	; normal return

	;---------  what to do if errors are found ---------------

	bad_open:	print, '	Unable to open ', sFileName
		exit, status = 1

	bad_read:	print, '	Error reading ', sFileName
		exit, status = 1
end

;;; sample main program

;	in_file = 'LRF_INPUT.DAT'

;	lrf_list, infile = in_file

	print
	print, 'To test lrf_list, type:  lrf_list, infile = ''LRF_INPUT.DAT'' <Enter>'
	print, 'at the IDL> prompt'
	print
	print, 'To see keyword options, type:  lrf_list <Enter>'
	print, 'at the IDL> prompt'

;	 lrf_list

end
