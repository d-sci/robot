;*******************************************************************
; Keypad/LCD Test Code
; Assembler : mpasm.exe
; Linker    : mplink.exe
; Written By : Kevin Lam
;*******************************************************************

      list p=16f877                 ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF


	;Declare unbanked variables (at 0x70 and on)
	cblock  0x70
		lcd_tmp
		lcd_d1
		lcd_d2
	endc

	;Declare constants for pin assignments (LCD on PORTD)
		RS 	equ 2
		E 	equ 3

	;Helper macros
WRT_LCD macro val
	movlw   val
	call    WrtLCD
	endm

;Delay: ~160us
LCD_DELAY macro
	movlw   0xFF
	movwf   lcd_d1
	decfsz  lcd_d1,f
	goto    $-1
	endm


         ORG       0x0000     ;RESET vector must always be at 0x00
         goto      init       ;Just jump to the main code section.

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

         call      InitLCD    ;Initialize the LCD (code in lcd.asm; imported by lcd.inc)

test     btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		$-1

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         call     KPHexToChar ;Convert keypad value to LCD character (value is still held in W)
         call     WrtLCD      ;Write the value in W to LCD

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-1
         goto     test

KPHexToChar
          addwf     PCL,f
          dt        "123A456B789C*0#D"


;******* LCD-related subroutines *******


    ;***********************************
InitLCD
	bcf STATUS,RP0
	bsf PORTD,E     ;E default high

	;Wait for LCD POR to finish (~15ms)
	call lcdLongDelay
	call lcdLongDelay
	call lcdLongDelay

	;Ensure 8-bit mode first (no way to immediately guarantee 4-bit mode)
	; -> Send b'0011' 3 times
    bcf     PORTD,RS       ;Instruction mode
	movlw   B'00110000'
	call    MovMSB
	call lcdLongDelay
	call lcdLongDelay
	call    ClkLCD         ;Finish last 4-bit send (if reset occurred in middle of a send)
	call    lcdLongDelay   ;->max instruction time ~= 5ms
	call    ClkLCD         ;Assuming 4-bit mode, set 8-bit mode
	call    ClkLCD         ;(note: if it's in 8-bit mode already, it will stay in 8-bit mode)

    ;Now that we know for sure it's in 8-bit mode, set 4-bit mode.
	movlw B'00100000'
	call MovMSB
	call lcdLongDelay
	call lcdLongDelay
	call ClkLCD

	;Give LCD init instructions
	WRT_LCD B'00101000' ; 4 bits, 2 lines,5X8 dot
	call lcdLongDelay
	call lcdLongDelay
	WRT_LCD B'00001111' ; display on,cursor,blink
	call lcdLongDelay
	call lcdLongDelay
	WRT_LCD B'00000110' ; Increment,no shift
	call lcdLongDelay
	call lcdLongDelay
	;Ready to display characters
	call    ClrLCD
    bsf     PORTD,RS    ;Character mode
	return
    ;************************************

	;WrtLCD: Clock MSB and LSB of W to PORTD<7:4> in two cycles
WrtLCD
	movwf   lcd_tmp ; store original value
	call    MovMSB  ; move MSB to PORTD
	call    ClkLCD
	swapf   lcd_tmp,w ; Swap LSB of value into MSB of W
    call    MovMSB    ; move to PORTD
    call    ClkLCD
    return

    ;ClrLCD: Clear the LCD display
ClrLCD
    bcf     PORTD,RS       ;Instruction mode
    WRT_LCD b'00000001'
    call    lcdLongDelay
    return

    ;ClkLCD: Pulse the E line low
ClkLCD
    LCD_DELAY
    bcf PORTD,E
    LCD_DELAY   ; __    __
    bsf PORTD,E ;   |__|
    return

    ;****************************************

    ;MovMSB: Move MSB of W to PORTD, without disturbing LSB
MovMSB
    andlw 0xF0
    iorwf PORTD,f
    iorlw 0x0F
    andwf PORTD,f
    return

    ;Delay: ~5ms
lcdLongDelay
    movlw d'20'
    movwf lcd_d2
LLD_LOOP
    LCD_DELAY
    decfsz lcd_d2,f
    goto LLD_LOOP
    return


	END



