;Test for motor controlled by L298N. 
;Want to rotate 36deg = 15.5 steps, so alternate between 15 and 16.
;Also note we save which step we're on (for the 15 step, you go back one each cycle)
;Excitation sequence is 1-2-3-4 (four steps) controlled by RA[3:0]
;   1 = 1001
;   2 = 1010
;   3 = 0110
;   4 = 0101
;http://duvindu92.blogspot.ca/2013/07/driving-bipolar-stepper-motor-with.html

 list p=16f877
      #include <p16f877.inc>
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

    #define	RS 	PORTD,2
	#define	E 	PORTD,3

    cblock  0x70
        del1
        del2
        hdelH
        hdelM
        hdelL
        start_step  ;this is new
        step_count  ;this is new
        step_max    ;this is new
        dat
        com
    endc

movlf   macro   l, f
        movlw   l
        movwf   f
        endm

writeBCD    macro   reg         ; from a register containing BCD
            movf    reg, W
            addlw   B'00110000'
            call WR_DATA
            endm

    ORG       0x000
    goto      main
    ORG       0x004
    retfie

main
    clrf        INTCON
    banksel     TRISA
    clrf        TRISA
    movlf       b'11110010', TRISB
    clrf        TRISD
    movlf       0x07, ADCON1
    banksel     PORTA
    clrf        PORTA
    clrf        PORTB
    clrf        PORTD
    call        InitLCD
    movlf       d'1', start_step
    movlf       d'15', step_max

waiting
         btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		waiting

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         xorlw      0xC         ;Will be all zeros if its "START"
         btfsc      STATUS,Z    ;and Z will be high, so skip if not high
         call       ROTATEMOTOR

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-1
         goto       waiting

ROTATEMOTOR
    call    Clear_Display
    writeBCD    step_max
    writeBCD    start_step

    clrf    step_count

    ;go to the right starting step
    movlw   d'1'
    subwf   start_step, W
    btfsc   STATUS,Z
    goto    firststep

    movlw   d'2'
    subwf   start_step, W
    btfsc   STATUS,Z
    goto    secondstep

    movlw   d'3'
    subwf   start_step, W
    btfsc   STATUS,Z
    goto    thirdstep

    movlw   d'4'
    subwf   start_step, W
    btfsc   STATUS,Z
    goto    fourthstep

four_steps

firststep
    movf    step_max, W
    subwf   step_count, W
    btfss   STATUS, Z
    goto    pulse1
    movlf   d'1', start_step
    goto    end_rotate
pulse1
    movlf   B'1001', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS
    incf    step_count, F

secondstep
    movf    step_max, W
    subwf   step_count, W
    btfss   STATUS, Z
    goto    pulse2
    movlf   d'2', start_step
    goto    end_rotate
pulse2
    movlf   B'1010', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS
    incf    step_count, F

thirdstep
    movf    step_max, W
    subwf   step_count, W
    btfss   STATUS, Z
    goto    pulse3
    movlf   d'3', start_step
    goto    end_rotate
pulse3
    movlf   B'0110', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS
    incf    step_count, F

fourthstep
    movf    step_max, W
    subwf   step_count, W
    btfss   STATUS, Z
    goto    pulse4
    movlf   d'4', start_step
    goto    end_rotate
pulse4
    movlf   B'0101', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS
    incf    step_count, F

    goto    four_steps

end_rotate
   ;if step_max is 15 incr, 16 dec
    movlw    d'16'
    subwf   step_max, W
    btfss   STATUS,Z
    goto    must_inc
must_dec
    decf    step_max, F
    return
must_inc
    incf     step_max, F
    return













delay5ms
	movlf	0xC3, del1
	movlf	0x0A, del2
Delay_0
	decfsz	del1, f
	goto	$+2
	decfsz	del2, f
	goto	Delay_0
    return

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
	call	delay5ms
    call delay5ms
	call delay5ms;__    __
	bcf		E				;  |__|
	swapf	com,w
	andlw	0xF0			;1111 0010
	movwf	PORTD			;send 4 bits LSB
	bsf		E				;
	call	delay5ms
    call delay5ms
	call delay5ms	;__    __
	bcf		E				;  |__|
	call	delay5ms
    call delay5ms
	call delay5ms
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
	call	delay5ms
    call delay5ms
	call delay5ms	;__    __
	bcf		E				;  |__|
	swapf	dat,w
	andlw	0xF0
	addlw	4
	movwf	PORTD
	bsf		E				;
	call	delay5ms
    call delay5ms
	call delay5ms	;__    __
	bcf		E				;  |__|
	return

    END



