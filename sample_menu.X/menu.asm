    list p=16f877                 ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF


;***************************************
; VARIABLES
;***************************************
    cblock	0x70
        msgtemp
		Table_Counter
		com
		dat
        lcd_d1
        lcd_d2
	endc


;***************************************
; DEFINITIONS
;***************************************
    #define	RS 	PORTD,2
	#define	E 	PORTD,3


;***************************************
; MACROS
;***************************************
LCD_DELAY macro
	movlw   0xFF
	movwf   lcd_d1
	decfsz  lcd_d1,f
	goto    $-1
	endm

Display macro	Message
		local	loop_
		local 	end_
		clrf	Table_Counter
		clrw
loop_	movf	Table_Counter,W
		call 	Message
		xorlw	B'00000000' ;check WORK reg to see if 0 is returned
		btfsc	STATUS,Z
			goto	end_
		call	WR_DATA
		incf	Table_Counter,F
		goto	loop_
end_
		endm



;***************************************
; MAIN CODE
;***************************************

    ORG       0x0000             ;RESET vector must always be at 0x00
         goto      init          ;Just jump to the main code section.

init
         clrf      INTCON         ; No interrupts

         bsf       STATUS,RP0     ; select bank 1
         clrf      TRISA          ; All port A is output
         movlw     b'11110010'    ; Set required keypad inputs
         movwf     TRISB
         clrf      TRISC          ; All port C is output
         clrf      TRISD          ; All port D is output

         bcf       STATUS,RP0     ; select bank 0
         clrf      PORTA
         clrf      PORTB
         clrf      PORTC
         clrf      PORTD

         call      InitLCD    ;Initialize the LCD 
         Display    Welcome_Msg

test     btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		$-1

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         call     writemessage

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-1
         goto     test


;***************************************
; LOOK UP TABLE (MESSAGES)
;***************************************
Welcome_Msg
		addwf	PCL,F
		dt		"Pick A B or C", 0
Message_A
		addwf	PCL,F
		dt		"You picked A",0
Message_B
		addwf	PCL,F
		dt		"You picked B",0
Message_C
		addwf	PCL,F
		dt		"You picked C",0
Message_D
		addwf	PCL,F
		dt		"D??? You devil!",0




;***************************************
; MESSAGE SELECTOR ROUTINE (written by me)
;***************************************
writemessage
    movwf   msgtemp             ; save value of W in msgtemp

check_a
    xorlw   0x3         ;will be all zeros if its A
    btfss   STATUS,Z    ;and Z will be high, so skip if not high
    goto check_b
    call Clear_Display
    Display Message_A
    return

check_b
    movf    msgtemp,W      ;W is selected key again
    xorlw   0x7         ;will be all zeros if its B
    btfss   STATUS,Z    ;and Z will be high, so skip if not high
    goto check_c
    call Clear_Display
    Display Message_B
    return

check_c
    movf    msgtemp,W      ;W is selected key again
    xorlw   0xB         ;will be all zeros if its B
    btfss   STATUS,Z    ;and Z will be high, so skip if not high
    goto check_d
    call Clear_Display
    Display Message_C
    return

check_d
    movf    msgtemp,W      ;W is selected key again
    xorlw   0xF         ;will be all zeros if its B
    btfss   STATUS,Z    ;and Z will be high, so skip if not high
    goto default
    call Clear_Display
    Display Message_D
    return

default
    call Clear_Display
    Display Welcome_Msg
    return



;***************************************
; DISPLAY ROUTINES (copied from sample code)
;***************************************

InitLCD
	bcf STATUS,RP0
	bsf E     ;E default high

	;Wait for LCD POR to finish (~15ms)
	call lcdLongDelay
	call lcdLongDelay
	call lcdLongDelay

	;Ensure 8-bit mode first (no way to immediately guarantee 4-bit mode)
	; -> Send b'0011' 3 times
	movlw	b'00110011'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay
	movlw	b'00110010'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay

	; 4 bits, 2 lines, 5x7 dots
	movlw	b'00101000'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay

	; display on/off
	movlw	b'00001100'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay

	; Entry mode
	movlw	b'00000110'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay

	; Clear ram
	movlw	b'00000001'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay
	return
 

Clear_Display
		movlw	B'00000001'
		call	WR_INS
		return


WR_INS
	bcf		RS				;clear RS
	movwf	com				;W --> com
	andlw	0xF0			;mask 4 bits MSB w = X0
	movwf	PORTD			;Send 4 bits MSB
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	swapf	com,w
	andlw	0xF0			;1111 0010
	movwf	PORTD			;send 4 bits LSB
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	call	lcdLongDelay
	return


WR_DATA
	bsf		RS
	movwf	dat
	movf	dat,w
	andlw	0xF0
	addlw	4
	movwf	PORTD
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	swapf	dat,w
	andlw	0xF0
	addlw	4
	movwf	PORTD
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	return

lcdLongDelay
    movlw d'20'
    movwf lcd_d2
LLD_LOOP
    LCD_DELAY
    decfsz lcd_d2,f
    goto LLD_LOOP
    return

    END