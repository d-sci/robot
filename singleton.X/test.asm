;Full test for a single candle at a time
;Pressing start will rotate one position, detect presence and state of candle,
;and display result followed by operation time.
;Connect motor to RA[3:0], IRDATA to RE0, PHOTODATA to RE1

 list p=16f877
      #include <p16f877.inc>
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

    #define	RS 	PORTD,2
	#define	E 	PORTD,3
    #define IRDATA     PORTE, 0
    #define PHOTODATA   PORTE, 1
    #define threshold1  D'7'
    #define threshold2  D'69'

    cblock  0x70 ;(exactly to 7F!!)
        del1
        del2
        delH
        delM
        delL
        Table_Counter
        motor_count
        dat
        com
        ;present
        photocount
        photoval
        w_isr
        status_isr
        count38
        op_time
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


writeBCD    macro   reg         ; from a register containing BCD
            movf    reg, W
            addlw   B'00110000'
            call WR_DATA
            endm

movlf   macro   l, f
        movlw   l
        movwf   f
        endm

movff   macro   source, dest
        movf    source, W
        movwf   dest
        endm

    ORG       0x000
    goto      init
    ORG       0x004
    goto       isr


Pass
		addwf	PCL,F
		dt		"Pass",0
LED_fail
		addwf	PCL,F
		dt		"LED fail",0
Flick_fail
		addwf	PCL,F
		dt		"Flicker fail",0
Not_present
        addwf   PCL,F
        dt      "Not present",0
Testing
        addwf   PCL,F
        dt      "Testing...",0


init
    movlf      b'00100000', INTCON   ;timer intterupt ready enabled

    banksel     TRISA               ;bank1
    clrf        TRISA
    movlf       B'11000111', OPTION_REG ; 1:256 prescaler
    movlf       b'11110010', TRISB
    clrf        TRISD
    movlf       b'011', TRISE
    movlf       0x07, ADCON1        ;digital input

    banksel     PORTA               ;bank0
    clrf        PORTA
    clrf        PORTB
    clrf        PORTC
    clrf        PORTD
    clrf        PORTE

    call        InitLCD


waiting
         btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		waiting

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         xorlw      0xC         ;Will be all zeros if its "START"
         btfsc      STATUS,Z    ;and Z will be high, so skip if not high
         call       start

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-1
         goto       waiting

start
    ;Start the timer
        movlf       D'38', count38
        clrf        op_time
        bsf         INTCON, GIE ;enable interrupts
        clrf        TMR0

    call    Clear_Display
    ;bcf     present, 0
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
    btfsc    IRDATA
    goto    test_candle
no_candle
    Display Not_present
    goto    finish


test_candle
    Display Testing
    clrf    photocount
    call    HalfS       ; delay 2 sec or whatever
    call    HalfS
    call    HalfS
    call    HalfS
	movff   photocount, photoval        ;to ensure it wont change again
check_threshold1
    movlw    threshold1
    subwf   photoval, W
    btfsc   STATUS, C       ;if  photoval < threshold 1, C = 0
    goto check_threshold2
    call    Clear_Display
    Display LED_fail        ; < threshold 1 means led fail
    goto    finish
check_threshold2
    movlw    threshold2
    subwf   photoval, W
    btfsc   STATUS, C       ;if  photoval < threshold 2, C = 0
    goto aboveboth
    call    Clear_Display
    Display Pass      ; < threshold 2 means pass
    goto    finish
aboveboth
    call    Clear_Display
    Display  Flick_fail      ; else flicker fail

finish
    call    Switch_Lines
    bcf     INTCON, GIE     ;disable interrupts
    writeBCD    op_time
    return

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


; DELAY 0.5S SUBROUTINE (from generator at http://www.piclist.com/techref/piclist/codegen/delay.htm)
; Delays exactly 0.5sec
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

; DELAY 5ms SUBROUTINE. (from generator at http://www.piclist.com/techref/piclist/codegen/delay.htm)
; Useful for LCD because PIC is way faster than it can handle
; Delays exactly 5ms
delay5ms
	movlf	0xC3, delH
	movlf	0x0A, delL
Delay_0
	decfsz	delH, f
	goto	$+2
	decfsz	delL, f
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


isr
    movwf   w_isr           ;save W and status
    swapf   STATUS, W
    clrf    STATUS
    movwf   status_isr
;    movf    PCLATH, W      ;if using pages
;    movwf   pclath_isr
;    clrf    PCLATH

    decfsz  count38, F     ;if count38 gets to 38 it's been one second
    goto    end_isr
    movlf   D'38', count38  ;so reset count38
    incf    op_time, F         ; and increment op_time

end_isr

    btfsc   PHOTODATA       ;if PHOTODATA is 1, light is on
    incf    photocount, F       ;if it is 1, light is on so photocount++

;    movf    pclath_isr, W  ;if using pages
;    movwf    PCLATH
    swapf   status_isr, W   ;restore W and status
    movwf   STATUS
    swapf   w_isr, F
    swapf   w_isr, W
    bcf     INTCON, T0IF    ;clear the interrupt flag
    retfie


    END






