;Test for IR detector. Connect IRDATA to RA5 (input).
;Press start to check for presence of candle; LCD displays state.
;RA5 is 1 for yes candle, low for no candle.

    list p=16f877
      #include <p16f877.inc>
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF


    #define	RS 	PORTD,2
	#define	E 	PORTD,3
    #define IRDATA  PORTA,5


    cblock  0x70
        Table_Counter   ; for LCD stuff
		com
		dat
        del1            ; for delay 5ms delay routine
        del2
        hdelH          ;for delay 0.5s routine
        hdelM
        hdelL
    endc

Display macro	Message
		local	loop_disp
		local 	end_disp
		clrf	Table_Counter
		clrw
loop_disp
    	movf	Table_Counter,W
		call 	Message
		xorlw	B'00000000' ;check WORK reg to see if 0 is returned
		btfsc	STATUS,Z
		goto	end_disp
		call	WR_DATA
		incf	Table_Counter,F
		goto	loop_disp
end_disp
		endm


movlf   macro   l, f
        movlw   l
        movwf   f
        endm

    ORG       0x000
    goto      init
    ORG       0x004
    retfie


Candle_Msg
		addwf	PCL,F
		dt		"Yes candle", 0
No_Candle_Msg
        addwf   PCL,F
        dt      "No candle",0
Testing_Msg
        addwf   PCL,F
        dt      "Checking. . .",0


init
    clrf        INTCON

    banksel     TRISA
    movlf      b'100000', TRISA
    movlf     b'11110010', TRISB
    clrf        TRISD
    movlf   0x07, ADCON1        ;digital input

    banksel     PORTB
    clrf        PORTA
    clrf        PORTB
    clrf        PORTD

    call        InitLCD
    bcf       STATUS,RP0          ; back to bank0
    call    Clear_Display


waiting
         btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		waiting

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         xorlw      0xC         ;Will be all zeros if its "START"
         btfsc      STATUS,Z    ;and Z will be high, so skip if not high
         goto       start

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-1
         goto       waiting

start
    call    Clear_Display
    Display Testing_Msg
    btfss   IRDATA          ;IRDATA is 1 if there's a light, 0 if there's no light
	goto    no_candle
yes_candle
    call    HalfS
    call    Clear_Display
    Display     Candle_Msg
    goto waiting
no_candle
    call    HalfS
    call    Clear_Display
    Display     No_Candle_Msg
    goto waiting


; DELAY 0.5S SUBROUTINE (from generator at http://www.piclist.com/techref/piclist/codegen/delay.htm)
; Delays exactly 0.5sec
HalfS
      movlf 0x8A, hdelH
      movlf 0xBA, hdelM
      movlf 0x03, hdelL
HalfS_0
      decfsz	hdelH, F
	  goto	$+2
	  decfsz	hdelM, F
	  goto	$+2
	  decfsz	hdelL, F
	  goto	HalfS_0

	  goto	$+1
	  nop
	  return

; DELAY 5ms SUBROUTINE. (from generator at http://www.piclist.com/techref/piclist/codegen/delay.htm)
; Useful for LCD because PIC is way faster than it can handle
; Delays exactly 5ms
delay5ms
	movlf	0xC3, del1
	movlf	0x0A, del2
Delay_0
	decfsz	del1, f
	goto	$+2
	decfsz	del2, f
	goto	Delay_0
    return


; Initialize the LCD
InitLCD
	bcf STATUS,RP0
	bsf E     ;E default high

	;Wait for LCD POR to finish (~15ms)
	call delay5ms
	call delay5ms
	call delay5ms

	;Ensure 8-bit mode first (no way to immediately guarantee 4-bit mode)
	; -> Send b'0011' 3 times
	movlw	b'00110011'
	call	WR_INS
	call delay5ms
	call delay5ms
	movlw	b'00110010'
	call	WR_INS
	call delay5ms
	call delay5ms

	; 4 bits, 2 lines, 5x7 dots
	movlw	b'00101000'
	call	WR_INS
	call delay5ms
	call delay5ms

	; display on/off
	movlw	b'00001100'
	call	WR_INS
	call delay5ms
	call delay5ms

	; Entry mode
	movlw	b'00000110'
	call	WR_INS
	call delay5ms
	call delay5ms

	; Clear ram
	movlw	b'00000001'
	call	WR_INS
	call delay5ms
	call delay5ms
	return

; Clear the display
Clear_Display
		movlw	B'00000001'
		call	WR_INS
		return

;Switch lines
Switch_Lines
		movlw	B'11000000'
		call	WR_INS
		return

; Write an instruction to the LCD (see page 7-104)
; The instruction must be in W
WR_INS
	bcf		RS				;clear RS
	movwf	com				;W --> com
	andlw	0xF0			;mask 4 bits MSB w = X0
	movwf	PORTD			;Send 4 bits MSB
	bsf		E				;
	call	delay5ms	;__    __
	bcf		E				;  |__|
	swapf	com,w
	andlw	0xF0			;1111 0010
	movwf	PORTD			;send 4 bits LSB
	bsf		E				;
	call	delay5ms	;__    __
	bcf		E				;  |__|
	call	delay5ms
	return

; Write data at current cursor location
; Character code (see page 7-104) must be in W
WR_DATA
	bsf		RS
	movwf	dat
	movf	dat,w
	andlw	0xF0
	addlw	4
	movwf	PORTD
	bsf		E				;
	call	delay5ms	;__    __
	bcf		E				;  |__|
	swapf	dat,w
	andlw	0xF0
	addlw	4
	movwf	PORTD
	bsf		E				;
	call	delay5ms	;__    __
	bcf		E				;  |__|
	return

    END


