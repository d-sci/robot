    list p=16f877                 ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

      #include <rtc_macros.inc>


;***************************************
; DEFINITIONS
;***************************************
    #define	RS 	PORTD,2
	#define	E 	PORTD,3


;***************************************
; VARIABLES
;***************************************

; Also see i2c_commmon.asm -> has stuff at 0x70 on for RTC

    cblock	0x20
        ; temps, counters, etc.
        keytemp         ; for data display mode
        statetemp
		Table_Counter   ; for LCD stuff
		com
		dat
        lcd_d1
        lcd_d2
        COUNTH          ;for delay 0.5s routine
        COUNTM
        COUNTL
        op_time_save    ;for operation time
        huns
        tens
        ones
        bignumcount
        count38         ;for isr
        w_isr           ;I think this needs to be in all banks??
        status_isr

        ; important information
        op_time    ;in seconds
        state1     ;where   0 = none      1 = pass
        state2     ;        2 = led fail  3 = flicker fail
        state3
        state4
        state5
        state6
        state7
        state8
        state9
        num_def
        num_tot
        start_year10    ;starting time (for log)
        start_year1
        start_month10
        start_month1
        start_date10
        start_date1
        start_hour10
        start_hour1
        start_min10
        start_min1

	endc


;***************************************
; MACROS
;***************************************

;LCD Delay
LCD_DELAY   macro
            movlw   0xFF
            movwf   lcd_d1
            decfsz  lcd_d1,f
            goto    $-1
            endm

; Display a msg on LCD
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

;Move cursor one to the right
spacebar    macro
            movlw   B'00010100'
            call    WR_INS
            endm

;Shortcuts for moving literals / registers
movlf   macro   l, f
        movlw   l
        movwf   f
        endm

movff   macro   source, dest
        movf    source, W
        movwf   dest
        endm

; Display a single digit number (0x0 to 0x9) on LCD as decimal
writenum    macro   number          ; literal
            movlw    number
            addlw   B'00110000'
            movwf   dat
            call WR_DATA
            endm

writenum_reg    macro   reg         ; from a register
            movfw    reg
            addlw   B'00110000'
            movwf   dat
            call WR_DATA
            endm


;***************************************
; VECTORS
;***************************************

    ORG       0x000
         goto      init
    ORG       0x004
        goto    isr


;***************************************
; TABLES (MESSAGES)
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
Export_Msg
        addwf   PCL,F
        dt      "Exporting . . .",0


;***************************************
; MAIN PROGRAM
;***************************************

init
        movlf     b'10000000', INTCON   ;interrupts enabled

        bsf       STATUS,RP0     ; select bank 1
        clrf      TRISA          ; All port A is output
        movlw     b'11110010'    ; Set required keypad inputs
        movwf     TRISB
        clrf      TRISC          ; All port C is output
        clrf      TRISD          ; All port D is output

        ;Set SDA and SCL to high-Z first as required for I2C
		bsf	   TRISC,4
		bsf	   TRISC,3

        bcf       STATUS,RP0     ; select bank 0
        clrf      PORTA
        clrf      PORTB
        clrf      PORTC
        clrf      PORTD

        call 	  i2c_common_setup  ;Set up I2C for communication
        call      InitLCD           ;Initialize the LCD
		
        Display Standby_Msg
        call    Switch_Lines
      
 ;--------------------------------------------------
 ;      STANDBY MODE

waiting
        ; Display date and time. 
        ; Also save starting time for log (will stop updating once we start)
        movlw	"2"				;First line shows 20**/**/**
		call	WR_DATA
		movlw	"0"
		call	WR_DATA
		rtc_read	0x06		;Read Address 0x06 from DS1307---year
		movfw	0x77
        movf    start_year10    ;Save starting year dig10
		call	WR_DATA
		movfw	0x78
        movf    start_year1    ;Save starting year dig1
		call	WR_DATA
		movlw	"/"
		call	WR_DATA
		rtc_read	0x05		;Read Address 0x05 from DS1307---month
		movfw	0x77
        movf    start_month10    ;Save starting month dig10
		call	WR_DATA
		movfw	0x78
        movf    start_month1    ;Save starting month dig1
		call	WR_DATA
		movlw	"/"
		call	WR_DATA
		rtc_read	0x04		;Read Address 0x04 from DS1307---date
		movfw	0x77
        movf    start_date10    ;Save starting date dig10
		call	WR_DATA
		movfw	0x78
        movf    start_date1    ;Save starting date dig1
		call	WR_DATA
		spacebar
		rtc_read	0x02		;Read Address 0x02 from DS1307---hour
		movfw	0x77
        movf    start_hour10    ;Save starting hour dig10
		call	WR_DATA
		movfw	0x78
        movf    start_hour1    ;Save starting hour dig1
		call	WR_DATA
		movlw	":"
		call	WR_DATA
		rtc_read	0x01		;Read Address 0x01 from DS1307---min
		movfw	0x77
        movf    start_min10    ;Save starting min dig10
		call	WR_DATA
		movfw	0x78
        movf    start_min1    ;Save starting min dig1
		call	WR_DATA
		      
        ; Move cursor back to start of second line
        ; We will update displayed time but not the word "STANDBY"
        movlw   B'11000000'
        call    WR_INS

        ;Poll to start (will have to hold key for ~0.5sec)
         btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		waiting

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         xorlw      0xD         ;Will be all zeros if its "START"
         btfsc      STATUS,Z    ;and Z will be high, so skip if not high
         goto       start

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-1
         goto       waiting

;-----------------------------------------------------------
;       ACTUAL OPERATION (I'm just adding values manually now)

start
        ;Start the timer
        movlf       D'38', count38
        clrf        op_time
        bsf         INTCON, T0IE ;enable Timer0 interrupt
        clrf        TMR0
        banksel     OPTION_REG
        movlf       B'11000111', OPTION_REG ; 1:256 prescaler
        bcf        STATUS,RP0     ; back to bank 0

        ;Display starting message
        call        Clear_Display
        Display     Start_Msg       ;"Inspecting. . ."

        ; just delaying for now (11s or 1s delay if you decomment first block)
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS
;        call        HalfS

        call        HalfS
        call        HalfS

        ; Putting values in manually. This will be done automatically in main program
        movlf     B'00', state1
        movlf     B'01', state2
        movlf     B'10', state3
        movlf     B'11', state4
        movlf     B'01', state5
        movlf     B'01', state6
        movlf     B'01', state7
        movlf     B'11', state8
        movlf     B'00', state9
        movlf     D'3', num_def
        movlf     D'7', num_tot

        ; Stop the timer
         bcf         INTCON, T0IE  ;disable Timer0 interrupt

        ; Display ending messages
        call       Clear_Display
        Display    End_Msg          ; "Complete"
        call       HalfS
        call       time             ; "Operation time: X sec"
        call       HalfS
        call       HalfS
        call       summary          ; "Total candles: X. Defective: Y"
        call       HalfS
        call       HalfS
        call       defective        ; "Defective: a b c"
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

; END OF MAIN PROGRAM
;-------------------------------------------------------------------------


;***************************************
; DATA DISPLAY ROUTINE
; Which key was pressed is stored in W (0000 for "1" to 1111 for "D")
; Determines which key it was and displays appropriate info / branch
;***************************************

information
    movwf   keytemp             ; Save which key was pressed

check_1
    xorlw   0x0         ;will be all zeros if its 1
    btfss   STATUS,Z    ;and Z will be high, so skip
    goto    check_2
    call    Clear_Display
    writenum 0x1
    movf    state1, W
    call    display_state
    return

check_2
    movf    keytemp, W
    xorlw   0x1
    btfss   STATUS,Z
    goto    check_3
    call    Clear_Display
    writenum 0x2
    movf    state2, W
    call    display_state
    return

check_3
    movf    keytemp, W
    xorlw   0x2
    btfss   STATUS,Z
    goto    check_4
    call    Clear_Display
    writenum 0x3
    movf    state3, W
    call    display_state
    return

check_4
    movf    keytemp, W
    xorlw   0x4
    btfss   STATUS,Z
    goto    check_5
    call    Clear_Display
    writenum 0x4
    movf    state4, W
    call    display_state
    return

check_5
    movf    keytemp, W
    xorlw   0x5
    btfss   STATUS,Z
    goto    check_6
    call    Clear_Display
    writenum 0x5
    movf    state5, W
    call    display_state
    return

check_6
    movf    keytemp, W
    xorlw   0x6
    btfss   STATUS,Z
    goto    check_7
    call    Clear_Display
    writenum 0x6
    movf    state6, W
    call    display_state
    return

check_7
    movf    keytemp, W
    xorlw   0x8
    btfss   STATUS,Z
    goto    check_8
    call    Clear_Display
    writenum 0x7
    movf    state7, W
    call    display_state
    return

check_8
    movf    keytemp, W
    xorlw   0x9
    btfss   STATUS,Z
    goto    check_9
    call    Clear_Display
    writenum 0x8
    movf    state8, W
    call    display_state
    return

check_9
    movf    keytemp, W
    xorlw   0xA
    btfss   STATUS,Z
    goto    check_summary
    call    Clear_Display
    writenum 0x9
    movf    state9, W
    call    display_state
    return

check_summary
    movf    keytemp, W
    xorlw   0x3
    btfss   STATUS,Z
    goto    check_defective
    call    summary
    return

check_defective
    movf    keytemp, W
    xorlw   0x7
    btfss   STATUS,Z
    goto    check_time
    call    defective
    return

check_time
    movf    keytemp, W
    xorlw   0xB
    btfss   STATUS,Z
    goto    check_export
    call    time
    return

check_export                ;doesn't actually export yet
    movf    keytemp, W
    xorlw   0xF
    btfss   STATUS,Z
    goto    check_start
    call    export
    return

check_start
    movf    keytemp, W
    xorlw   0xD
    btfss   STATUS,Z
    goto    check_standby
    goto    start

check_standby
    movf    keytemp, W
    xorlw   0xE
    btfss   STATUS,Z
    goto    default_key
    call    Clear_Display
    Display Standby_Msg
    call    Switch_Lines
    goto    waiting

default_key         ; will never get here
    return


;Display state subroutine
;stateN is in W
display_state
    movwf  statetemp    ; save stateN

check_none
    xorlw   0x0
    btfss   STATUS,Z
    goto    check_pass
    Display Not_present
    return

check_pass
    movf    statetemp, W
    xorlw   0x1
    btfss   STATUS,Z
    goto    check_LED
    Display Pass
    return

check_LED
    movf    statetemp, W
    xorlw   0x2
    btfss   STATUS,Z
    goto    check_flick
    Display LED_fail
    return

check_flick
    movf    statetemp, W
    xorlw   0x3
    btfss   STATUS,Z
    goto    default_state
    Display Flick_fail
    return

default_state   ; should never get here
    return

; Summary Subroutine
; Displays total number of candles and number of defective candles
; Reads data from num_tot and num_def
summary
    call            Clear_Display
    Display         Total_Msg
    writenum_reg    num_tot
    call            Switch_Lines
    Display         Defective
    writenum_reg    num_def
    return

; Defective candles Subroutine
; Displays index of each defective candle
; Reads data from state1 - state9
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
    btfss           state1, 1          ;defective if state is 10 or 11
    goto            two
    writenum        0x1
    spacebar
two
    btfss           state2, 1          ;defective if state is 10 or 11
    goto            three
    writenum        0x2
    spacebar
three
    btfss           state3, 1          ;defective if state is 10 or 11
    goto            four
    writenum        0x3
    spacebar
four
    btfss           state4, 1          ;defective if state is 10 or 11
    goto            five
    writenum        0x4
    spacebar
five
    btfss           state5, 1          ;defective if state is 10 or 11
    goto            six
    writenum        0x5
    spacebar
six
    btfss           state6, 1          ;defective if state is 10 or 11
    goto            seven
    writenum        0x6
    spacebar
seven
    btfss           state7, 1          ;defective if state is 10 or 11
    goto            eight
    writenum        0x7
    spacebar
eight
    btfss           state8, 1          ;defective if state is 10 or 11
    goto            nine
    writenum        0x8
    spacebar
nine
    btfss           state9, 1          ;defective if state is 10 or 11
    goto            end_defective
    writenum        0x9
end_defective
    return

; Time subroutine
; Right now this takes the time in seconds as a binary number in op_time
; and puts it into huns,tens,ones to display. It assumes op_time was getting
; incremented every second. However I may just take the stop time - start time
; from the RTC data so this might change a lot.
time
    call    Clear_Display
    Display Time_Msg
    call    Switch_Lines
    movf    op_time, W
    call    big_number
    movf    huns, W
    btfsc   STATUS,Z        ;if huns is zero don't display it
    goto    no_huns
    writenum_reg    huns
no_huns
    movf    tens, W
    btfsc   STATUS,Z        ;if tens is zero don't display it
    goto    no_tens
    writenum_reg    tens
no_tens
    writenum_reg    ones
    Display Seconds
    return

; Export subroutine         (DOESNT ACTUALLY EXPORT YET!)
; Eventual this will export data to external source
; Right now it just displays "Exporting. . ." then "Complete"
export
    call    Clear_Display
    Display Export_Msg
    call    HalfS
    call    HalfS
    call    Clear_Display
    Display End_Msg
    return


;***************************************
; DISPLAY BIG NUMBER SUBROUTINE
; Modified from http://www.piclist.com/techref/microchip/math/radix/b2a-8b3d-ab.htm
; Converts 8-bit binary number op_time to three BCDs representing huns, tens, ones
; Uses "shift and add 3" algorithm
;***************************************
big_number
    movff   op_time, op_time_save         ;save the original op_time
    movlf   8, bignumcount                ;will shift 8 times
    clrf    huns
    clrf    tens
    clrf    ones

BCDadd3                             ; if any digit > 5, add3
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

    decf    bignumcount, 1
    bcf     STATUS, C
    rlf     op_time, 1              ; shift
    rlf     ones, 1
    btfsc   ones,4 ;
    call    carryones               ; carry if too large
    rlf     tens, 1

    btfsc   tens,4 ;
    call    carrytens
    rlf     huns,1
    bcf     STATUS, C

    movf    bignumcount, W
    btfss   STATUS, Z
    goto    BCDadd3                 ; repeat until you've shifted it 8 times

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
; LCD SUBROUTINES (from sample code)
;***************************************

; Initialize the LCD
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

;Delay routine because PIC is way faster than the LCD can handle
lcdLongDelay
    movlw d'20'
    movwf lcd_d2
LLD_LOOP
    LCD_DELAY
    decfsz lcd_d2,f
    goto LLD_LOOP
    return


;***************************************
; DELAY 0.5S SUBROUTINE (from sample code)
; Delays exactly 0.5sec
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


;***************************************
; ISR
; Currently only care about TMR0
; TMR0 overflows at 256*256; each time, decrement count38
; count38 thus hits 0 every 256*256*38 cycles = 1sec with 10MHz clock
; When this hapens, op_time increments
;***************************************
isr
    movwf   w_isr           ;save W and status
    swapf   STATUS, W
    bcf     STATUS, RP0
    movwf   status_isr

    decfsz    count38     ;if count38 gets to 38 it's been one second
    goto end_isr
    movlf   D'38', count38  ;so reset count38
    incf    op_time         ; and increment op_time

end_isr
    swapf   status_isr, W   ;restore W and status
    movwf   STATUS
    swapf   w_isr, F
    swapf   w_isr, W
    bcf     INTCON, T0IF    ;clear the interrupt flag
    retfie


    END





