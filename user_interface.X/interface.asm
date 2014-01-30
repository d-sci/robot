    list p=16f877                 ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF


;***************************************
; VARIABLES
;***************************************
    cblock	0x20
        ; for use in code
        keytemp
        statustemp
		Table_Counter
		com
		dat
        lcd_d1
        lcd_d2
        COUNTH
        COUNTM
        COUNTL
        op_time_save
        huns
        tens
        ones
        count
        ; values to display (generated by main machine code)
        op_time     ;in seconds
        status1     ;where   0 = none      1 = pass
        status2     ;        2 = led fail  3 = flicker fail
        status3
        status4
        status5
        status6
        status7
        status8
        status9
        num_def
        num_tot
        
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

movlf   macro   l, f
        movlw   l
        movwf   f
        endm

movff   macro   source, dest
        movf    source, W
        movwf   dest
        endm

writenum    macro   number
            movlw    number
            addlw   B'00110000'
            movwf   dat
            call WR_DATA
            endm

writenum_reg    macro   reg
            movfw    reg
            addlw   B'00110000'
            movwf   dat
            call WR_DATA
            endm


;***************************************
; VECTORS
;***************************************

    ORG       0x0000             ;RESET vector must always be at 0x00
         goto      init          ;Just jump to the main code section.


;***************************************
; LOOK UP TABLE (MESSAGES)
;***************************************
Standby_Msg
		addwf	PCL,F
		dt		"STANDBY", 0
Start_Msg
        addwf   PCL,F
        dt      "Inspecting . . .",0
End_Msg
        addwf   PCL,F
        dt      "Complete",0
Time_Msg
		addwf	PCL,F
		dt		"Operation time:",0
Seconds
        addwf   PCL,F
        dt      " seconds", 0
Not_present
        addwf   PCL,F
        dt      ": not present",0
Pass
		addwf	PCL,F
		dt		": pass",0
LED_fail
		addwf	PCL,F
		dt		": LED fail",0
Flick_fail
		addwf	PCL,F
		dt		": flicker fail",0
Total_Msg
        addwf   PCL,F
        dt      "Total candles: ",0
Defective
        addwf   PCL,F
        dt      "Defective: ",0
None
        addwf   PCL,F
        dt      "      none", 0
Spacebar
        addwf   PCL,F
        dt      " ",0

;***************************************
; MAIN PROGRAM
;***************************************

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
        Display   Standby_Msg

  waiting
        btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		$-1

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         xorlw      0xD         ;will be all zeros if its "START:
         btfsc      STATUS,Z    ;and Z will be high, so skip if not high
         goto       start

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-1
         goto       waiting

;--------------------------------------
;       ACTUAL OPERATION (im just adding values manually now)

start
        call        Clear_Display
        Display     Start_Msg

; just shitting around
        call    HalfS
        call Clear_Display
        writenum    0x5
        call    HalfS
        call Clear_Display
        writenum    0x4
        call    HalfS
        call Clear_Display
        writenum    0x3
        call    HalfS
        call Clear_Display
        writenum    0x2
        call    HalfS
        call Clear_Display
        writenum    0x1
        call    HalfS

; Putting values in manually. This will be done automatically in main program
         movlf     D'74', op_time
         movlf     B'00', status1
         movlf     B'01', status2
         movlf     B'10', status3
         movlf     B'11', status4
         movlf     B'01', status5
         movlf     B'01', status6
         movlf     B'01', status7
         movlf     B'11', status8
         movlf     B'00', status9
         movlf     D'3', num_def
         movlf     D'7', num_tot


        call        Clear_Display
        Display    End_Msg
        call    HalfS
        call    time
        call    HalfS
        call    HalfS
        call    summary
        call    HalfS
        call    HalfS
        call    defective
;-----------------------------------------------------------------------

; Data display interface

poll     btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		$-1

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         call       information

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-1
         goto       poll

;-------------------------------------------------------------------------



;***************************************
; KEY SELECTOR ROUTINE (written by me)
;***************************************
information
    movwf   keytemp             ; save value of W in msgtemp

check_1
    xorlw   0x0         ;will be all zeros if its 1
    btfss   STATUS,Z    ;and Z will be high, so skip 
    goto check_2
    call Clear_Display
    writenum 0x1
    movf status1, W
    call display_status
    return

check_2
    movf    keytemp, W
    xorlw   0x1         
    btfss   STATUS,Z    
    goto check_3
    call Clear_Display
    writenum 0x2
    movf status2, W
    call display_status
    return

check_3
    movf    keytemp, W
    xorlw   0x2
    btfss   STATUS,Z
    goto check_4
    call Clear_Display
    writenum 0x3
    movf status3, W
    call display_status
    return

check_4
    movf    keytemp, W
    xorlw   0x4
    btfss   STATUS,Z
    goto check_5
    call Clear_Display
    writenum 0x4
    movf status4, W
    call display_status
    return

check_5
    movf    keytemp, W
    xorlw   0x5
    btfss   STATUS,Z
    goto check_6
    call Clear_Display
    writenum 0x5
    movf status5, W
    call display_status
    return

check_6
    movf    keytemp, W
    xorlw   0x6
    btfss   STATUS,Z
    goto check_7
    call Clear_Display
    writenum 0x6
    movf status6, W
    call display_status
    return

check_7
    movf    keytemp, W
    xorlw   0x8
    btfss   STATUS,Z
    goto check_8
    call Clear_Display
    writenum 0x7
    movf status7, W
    call display_status
    return

check_8
    movf    keytemp, W
    xorlw   0x9
    btfss   STATUS,Z
    goto check_9
    call Clear_Display
    writenum 0x8
    movf status8, W
    call display_status
    return

check_9
    movf    keytemp, W
    xorlw   0xA
    btfss   STATUS,Z
    goto check_summary
    call Clear_Display
    writenum 0x9
    movf status9, W
    call display_status
    return

check_summary           
    movf    keytemp, W
    xorlw   0x3
    btfss   STATUS,Z
    goto check_defective
    call    summary
    return

check_defective
    movf    keytemp, W
    xorlw   0x7
    btfss   STATUS,Z
    goto check_time
    call defective
    return

check_time
    movf    keytemp, W
    xorlw   0xB
    btfss   STATUS,Z
    goto check_start
    call    time
    return

check_start
    movf    keytemp, W
    xorlw   0xD
    btfss   STATUS,Z
    goto check_standby
    goto start

check_standby
    movf    keytemp, W
    xorlw   0xE
    btfss   STATUS,Z
    goto    default_key
    call    Clear_Display
    Display Standby_Msg
    goto    waiting

default_key
    return


;display status subroutine
display_status
    movwf  statustemp

check_none
    xorlw   0x0
    btfss   STATUS,Z
    goto check_pass
    Display Not_present
    return

check_pass
    movf    statustemp, W
    xorlw   0x1
    btfss   STATUS,Z
    goto check_LED
    Display Pass
    return

check_LED
    movf    statustemp, W
    xorlw   0x2
    btfss   STATUS,Z
    goto check_flick
    Display LED_fail
    return

check_flick
    movf    statustemp, W
    xorlw   0x3
    btfss   STATUS,Z
    goto default_status
    Display Flick_fail
    return

default_status
    return

; Summary Subroutine
summary
    call            Clear_Display
    Display         Total_Msg
    writenum_reg    num_tot
    call            Switch_Lines
    Display         Defective
    writenum_reg    num_def
    return

; Defective candles Subroutine
defective
    call            Clear_Display
    Display         Defective
    call            Switch_Lines
    movf            num_def, F
    btfss           STATUS,Z         ;if none defective, just say "none"
    goto            one
    Display         None
    return
one
   btfss           status1, 1          ;defective if status is 10 or 11
    goto            two
    writenum        0x1
    Display         Spacebar
two
    btfss           status2, 1          ;defective if status is 10 or 11
    goto            three
    writenum        0x2
    Display         Spacebar
three
    btfss           status3, 1          ;defective if status is 10 or 11
    goto            four
    writenum        0x3
    Display         Spacebar
four
    btfss           status4, 1          ;defective if status is 10 or 11
    goto            five
    writenum        0x4
    Display         Spacebar
five
    btfss           status5, 1          ;defective if status is 10 or 11
    goto            six
    writenum        0x5
    Display         Spacebar
six
    btfss           status6, 1          ;defective if status is 10 or 11
    goto            seven
    writenum        0x6
    Display         Spacebar
seven
    btfss           status7, 1          ;defective if status is 10 or 11
    goto            eight
    writenum        0x7
    Display         Spacebar
eight
    btfss           status8, 1          ;defective if status is 10 or 11
    goto            nine
    writenum        0x8
    Display         Spacebar
nine
    btfss           status9, 1          ;defective if status is 10 or 11
    goto            end_defective
    writenum        0x9
end_defective
    return

; Time subroutine
time
    call Clear_Display
    Display Time_Msg
    call Switch_Lines
    movf    op_time, W
    call    big_number
    movf    huns, W
    btfsc   STATUS,Z    ;if huns is zero don't display it
    goto    no_huns
    writenum_reg    huns
no_huns
    movf    tens, W
    btfsc   STATUS,Z    ;if tens is zero don't display it
    goto    no_tens
    writenum_reg    tens
no_tens
    writenum_reg    ones
    Display Seconds
    return




;***************************************
; DISPLAY BIG NUMBER ROUTINE
; modified from http://www.piclist.com/techref/microchip/math/radix/b2a-8b3d-ab.htm
; converts 8-bit binary number to three BCDs representing huns, tens, ones
; uses "shift and add 3" algorithm
;***************************************

big_number      
    movff   op_time, op_time_save         ;save the original op_time
    movlf   8, count                      ;8 bits per register
    clrf    huns
    clrf    tens
    clrf    ones

BCDadd3
    movlw   0x5
    subwf   huns, W
    btfsc   STATUS, C       
    call    add3huns
    
    movlw   0x5
    subwf   tens, W
    btfsc   STATUS, C
    call    add3tens

    movlw   0x5
    subwf   ones, W
    btfsc   STATUS, C
    call    add3ones

    decf    count, 1
    bcf     STATUS, C
    rlf     op_time, 1
    rlf     ones, 1
    btfsc   ones,4 ;
    call    carryones
    rlf     tens, 1

    btfsc   tens,4 ;
    call    carrytens
    rlf     huns,1
    bcf     STATUS, C

    movf    count, W
    btfss   STATUS, Z
    goto    BCDadd3

    movff    op_time_save, op_time     ;restore the original op_time
    return

add3huns
    movlw 0x3
    addwf huns,F
    return

add3tens
    movlw 0x3
    addwf tens,F
    return

add3ones
    movlw 0x3
    addwf ones,F
    return

carryones
    bcf ones, 4
    bsf STATUS, C
    return

carrytens
    bcf tens, 4
    bsf STATUS, C
    return
    

;***************************************
; LCD ROUTINES (from sample code)
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

Switch_Lines
		movlw	B'11000000'
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
    


;***************************************
; DELAY 0.5S ROUTINE (from sample code)
;***************************************
HalfS
	local	HalfS_0
      movlw 0x88
      movwf COUNTH
      movlw 0xBD
      movwf COUNTM
      movlw 0x03
      movwf COUNTL

HalfS_0
      decfsz COUNTH, f
      goto   $+2
      decfsz COUNTM, f
      goto   $+2
      decfsz COUNTL, f
      goto   HalfS_0

      goto $+1
      nop
      nop
		return

    END