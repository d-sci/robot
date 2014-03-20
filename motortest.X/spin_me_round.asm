;Test for motor controlled by L298N. 
;Want to rotate 36deg = 20 steps
;Excitation sequence is 1-2-3-4 (four steps) controlled by RA[3:0]
;   1 = 1001
;   2 = 1010
;   3 = 0110
;   4 = 0101
;http://duvindu92.blogspot.ca/2013/07/driving-bipolar-stepper-motor-with.html
; Also scans RE0 (IRDATA). If it's ever 1 (because candle passed by it), saves that the candle is present via a 1 in present,0

 list p=16f877
      #include <p16f877.inc>
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

    #define	RS 	PORTD,2
	#define	E 	PORTD,3
    #define IRDATA     PORTE, 0

    cblock  0x70
        del1
        del2
        delH
        delM
        delL
        Table_Counter
;        start_step
;        step_count
;        step_max
        motor_count
        dat
        com
        present
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

    ORG       0x000
    goto      main
    ORG       0x004
    retfie

Candle_Msg
		addwf	PCL,F
		dt		"Yes candle", 0
No_Candle_Msg
        addwf   PCL,F
        dt      "No candle",0

main
    clrf        INTCON
    banksel     TRISA
    clrf        TRISA
    movlf       b'11110010', TRISB
    clrf        TRISD
    movlf      b'001', TRISE         ; IRDATA is RE0
    movlf       0x07, ADCON1
    banksel     PORTA
    clrf        PORTA
    clrf        PORTB
    clrf        PORTD
    clrf        PORTE
    call        InitLCD
;    movlf       d'1', start_step
;    movlf       d'15', step_max

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
    bcf     present, 0
    movlf   d'5', motor_count
start_rot
    movlf   b'1001', PORTA
    call    motor_del
    movlf   b'1000', PORTA
    call    motor_del
    movlf   b'1010', PORTA
    call    motor_del
    movlf   b'0010', PORTA
    call    motor_del
    movlf   b'0110', PORTA
    call    motor_del
    movlf   b'0100', PORTA
    call    motor_del
    movlf   b'0101', PORTA
    call    motor_del
    movlf   b'0001', PORTA
    call    motor_del

    decfsz  motor_count
    goto    start_rot
    clrf    PORTA
    ;btfsc   present, 0
    btfsc   IRDATA
    goto    yes_candle
no_candle
    Display No_Candle_Msg
    return
yes_candle
    Display Candle_Msg
    return

;    call    Clear_Display
;    writeBCD    step_max
;    writeBCD    start_step
;
;    clrf    step_count
;
;    ;go to the right starting step
;    movlw   d'1'
;    subwf   start_step, W
;    btfsc   STATUS,Z
;    goto    firststep
;
;    movlw   d'2'
;    subwf   start_step, W
;    btfsc   STATUS,Z
;    goto    secondstep
;
;    movlw   d'3'
;    subwf   start_step, W
;    btfsc   STATUS,Z
;    goto    thirdstep
;
;    movlw   d'4'
;    subwf   start_step, W
;    btfsc   STATUS,Z
;    goto    fourthstep
;
;four_steps
;
;firststep
;    movf    step_max, W
;    subwf   step_count, W
;    btfss   STATUS, Z
;    goto    pulse1
;    movlf   d'1', start_step
;    goto    end_rotate
;pulse1
;    movlf   B'1001', PORTA
;    call    motor_del
;    incf    step_count, F
;
;secondstep
;    movf    step_max, W
;    subwf   step_count, W
;    btfss   STATUS, Z
;    goto    pulse2
;    movlf   d'2', start_step
;    goto    end_rotate
;pulse2
;    movlf   B'1010', PORTA
;    call    motor_del
;    incf    step_count, F
;
;thirdstep
;    movf    step_max, W
;    subwf   step_count, W
;    btfss   STATUS, Z
;    goto    pulse3
;    movlf   d'3', start_step
;    goto    end_rotate
;pulse3
;    movlf   B'0110', PORTA
;    call    motor_del
;    incf    step_count, F
;
;fourthstep
;    movf    step_max, W
;    subwf   step_count, W
;    btfss   STATUS, Z
;    goto    pulse4
;    movlf   d'4', start_step
;    goto    end_rotate
;pulse4
;    movlf   B'0101', PORTA
;    call    motor_del
;    incf    step_count, F
;
;    goto    four_steps
;
;end_rotate
;   ;if step_max is 15 incr, 16 dec
;    clrf    PORTA
;    movlw    d'16'
;    subwf   step_max, W
;    btfss   STATUS,Z
;    goto    must_inc
;must_dec
;    decf    step_max, F
;    return
;must_inc
;    incf     step_max, F
;    return



motor_del
      movlf 0xF3, delH
      movlf 0x35, delL
motor_del_0
      ;btfsc     IRDATA
      ;bsf       present, 0
      decfsz	delH, F
	  goto      $+2
	  decfsz	delL, F
	  goto      motor_del_0
	  return


delay5ms
	movlf	0xC3, delH
	movlf	0x0A, delL
Delay_0
	decfsz	delH, f
	goto	$+2
	decfsz	delL, f
	goto	Delay_0
    return

HalfS
      movlf 0x8A, delH
      movlf 0xBA, delM
      movlf 0x03, delL
HalfS_0
      decfsz	delH, F
	  goto	$+2
	  decfsz	delM, F
	  goto	$+2
	  decfsz	delL, F
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



