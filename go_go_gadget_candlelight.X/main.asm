    list p=16f877                 ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

      #include <rtc_macros.inc>


;***************************************
; DEFINITIONS
;***************************************
    #define	RS 	PORTD,2
	#define	E 	PORTD,3
    #define threshold1  D'15'
    #define threshold2  D'65'
    #define IRLIGHT    PORTA, 0
    #define IRDATA     PORTA, 1
    #define PHOTODATA  PORTA, 2
   ; note: check analog v digital!


;***************************************
; VARIABLES
;***************************************

; Also see i2c_commmon.asm -> has stuff at 0x71-0x78 on for RTC!

    cblock	0x20
    ; Important information (will be displayed)
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
        num_LF      ;BCD
        num_FF      ;BCD
        num_tot     ;BCD
        start_year10    ;starting time (for log)
        start_year1     ; note: these are all ALREADY ASCII!
        start_month10
        start_month1
        start_date10
        start_date1
        start_hour10
        start_hour1
        start_min10
        start_min1
; For general program / interface: temps, counters, etc.
        keytemp         ; for data display mode
        statetemp
		Table_Counter   ; for LCD stuff
		com
		dat
        del1            ; for delay 5ms delay routine
        del2
        hdelH          ;for delay 0.5s routine
        hdelM
        hdelL
        op_time_save    ;for operation time
        huns
        tens
        ones
        bignumcount
        count38         ;for isr
; For machine program: temps, counters, etc.
        candle_index
        photocount
        photoval

    endc

    cblock  0x79        ;ISR stuff that needs to be in all registers
        w_isr           
        status_isr
	endc


;***************************************
; MACROS
;***************************************

; Display a msg on LCD
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

;Move cursor right one position
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

; Display a BCD on LCD as decimal
writenum    macro   number          ; literal
            movlw    number
            addlw   B'00110000'
            movwf   dat
            call WR_DATA
            endm

writenum_reg    macro   reg         ; from a register containing BCD
            movf    reg,W
            addlw   B'00110000'
            movwf   dat
            call WR_DATA
            endm

;Write to PC (hyperterminal)
printchar   macro   char            ;direct ASCII code literal (or in "")
            movlw   char
            call    writetoPC
            endm

printASC    macro   reg            ;ASCII code inside a register
            banksel reg
            movf    reg,W
            call    writetoPC
            endm

printBCD    macro   BCD            ;BCD inside a register
            banksel BCD
            movf    BCD, W
            addlw   0x30
            call    writetoPC
            endm


newline     macro
            printchar   0xA ;changes line
            printchar   0xD ;goes back to left
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
        dt      ": Not present",0
Pass
		addwf	PCL,F
		dt		": Pass",0
LED_fail
		addwf	PCL,F
		dt		": LED fail",0
Flick_fail
		addwf	PCL,F
		dt		": Flicker fail",0
Total_Msg
        addwf   PCL,F
        dt      "Total candles: ",0
LF
        addwf   PCL,F
        dt      "LF: ",0
FF
        addwf   PCL,F
        dt      "FF: ",0
None
        addwf   PCL,F
        dt      "<none>", 0
Logs_Msg1
        addwf   PCL,F
        dt      "1-4 to see log",0
Logs_Msg2
        addwf   PCL,F
        dt      "STANDBY to exit",0

;***************************************
; MAIN PROGRAM
;***************************************

init
        movlf     b'10000000', INTCON   ;interrupts enabled

        bsf       STATUS,RP0            ; select bank 1
        movlf     b'000110', TRISA      ; PortA *may* be used for Photo and IR stuff
        movlf     b'11110010', TRISB    ; PortB[7:4] and [1] are keypad inputs (rest unused; RB0 may be external interrupt)
                                        ; note can disable keypad to free up these ports during operation if necessary.
        movlf     b'00011000', TRISC    ; PortC[4:3] is RTC; [7:6] is RS-232; rest unused
        clrf      TRISD                 ; PortD[2:7] is LCD output (rest unused)
        clrf      TRISE                 ; PortE is output- *may* be used for motor (only [2:0] tho!)

        bcf       STATUS,RP0     ; select bank 0
        clrf      PORTA
        clrf      PORTB
        clrf      PORTC
        clrf      PORTD
        clrf      PORTE

        call 	  i2c_common_setup  ;Set up I2C for communication
        call      InitLCD           ;Initialize the LCD
        call      InitUSART         ;Set up USART for RS232

        bcf       STATUS,RP0          ; bank0
		
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
		movf	0x77,W
        movwf    start_year10    ;Save starting year dig10
		call	WR_DATA
		movf	0x78,W
        movwf    start_year1    ;Save starting year dig1
		call	WR_DATA
		movlw	"/"
		call	WR_DATA
		rtc_read	0x05		;Read Address 0x05 from DS1307---month
		movf	0x77,W
        movwf    start_month10    ;Save starting month dig10
		call	WR_DATA
		movf	0x78,W
        movwf    start_month1    ;Save starting month dig1
		call	WR_DATA
		movlw	"/"
		call	WR_DATA
		rtc_read	0x04		;Read Address 0x04 from DS1307---date
		movf	0x77,W
        movwf    start_date10    ;Save starting date dig10
		call	WR_DATA
		movf	0x78,W
        movwf    start_date1    ;Save starting date dig1
		call	WR_DATA
		spacebar
		rtc_read	0x02		;Read Address 0x02 from DS1307---hour
		movf	0x77,W
        movwf    start_hour10    ;Save starting hour dig10
		call	WR_DATA
		movf	0x78,W
        movwf    start_hour1    ;Save starting hour dig1
		call	WR_DATA
		movlw	":"
		call	WR_DATA
		rtc_read	0x01		;Read Address 0x01 from DS1307---min
		movf	0x77,W
        movwf    start_min10    ;Save starting min dig10
		call	WR_DATA
		movf	0x78,W
        movwf    start_min1    ;Save starting min dig1
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
         movwf      keytemp
         xorlw      0xC         ;Will be all zeros if its "START"
         btfsc      STATUS,Z    ;and Z will be high, so skip if not high
         goto       start

         movf       keytemp,W     ;Go to log interface
         xorlw      0xE
         btfsc      STATUS,Z
         goto       logs

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


;****FAKE CODE ******************************************
        ; Just delaying
        call        HalfS
        call        HalfS

       ; Putting values in manually.
        movlf     B'01', state1     ;pass
        movlf     B'01', state2     ;pass
        movlf     B'10', state3     ;LED fail
        movlf     B'10', state4     ;LED fail
        movlf     B'01', state5     ;pass
        movlf     B'00', state6     ;not present
        movlf     B'01', state7     ;pass
        movlf     B'11', state8     ;flicker fail
        movlf     B'01', state9     ;pass
        movlf     D'2', num_LF
        movlf     D'1', num_FF
        movlf     D'8', num_tot
;*******************************************************

; choose fake or real!

;;****REAL CODE ******************************************
;    movlf    D'0', candle_index
;    bcf	STATUS, IRP
;    movlf   0x20, FSR       ;pointing at right before state1
;    bsf     IRLIGHT     ;turn on IR
;
;rotate
;	movlw   0x9                 ; stop operation after 9 rotations
;    subwf   candle_index,W      ; candle_index is # you've already tested before rotating
;    btfsc   STATUS,Z
;	goto    end_operation
;	call    ROTATEMOTOR          ; else rotate motor and n++
;	incf    candle_index, F
;    incf    FSR, F
;
;detect_candle
;	btfss   IRDATA      ;IRDATA is 1 if there's no light, 0 if there's a light
;	goto    test_candle     ;yes candle, go test it
;    movlf   D'0', INDF      ;no candle, state = not present
;	goto rotate                 ;and go try next
;
;test_candle
;	incf    num_tot, F			; keeping track of total number of candles
;	; Assume candle is already turned on
;	clrf    photocount
;	call    HalfS       ; delay 2 sec or whatever
;   call    HalfS
;   call    HalfS
;   call    HalfS
;	movff   photocount, photoval        ;to ensure it wont change again
;check_threshold1
;    movlw    threshold1
;    subwf   photoval, W
;    btfsc   STATUS, C       ;if  photoval < threshold 1, C = 0
;    goto check_threshold2
;    movlf   D'2', INDF      ; < threshold 1 means led fail
;	 incf    num_LF, F
;    goto    end_test_candle
;check_threshold2
;    movlw    threshold2
;    subwf   photoval, W
;    btfsc   STATUS, C       ;if  photoval < threshold 2, C = 0
;    goto aboveboth
;    movlf   D'1', INDF      ; < threshold 2 means pass
;    goto    end_test_candle
;aboveboth
;   movlf   D'3', INDF       ;else flicker fail
;   incf    num_FF, F
;end_test_candle
;	call    TURNOFF     ;pulse solenoid to turn off candle
;    goto    rotate
;
;
;ROTATEMOTOR ;rotates stepper motor 40deg
;    return
;
;TURNOFF ;pulses solenoid to turn off candle
;    return
; ;****************************************************

end_operation
        ;Turn off the IR
        ;bcf     IRLIGHT

        ; Stop the timer
         bcf         INTCON, T0IE  ;disable Timer0 interrupt


        ;GOOD PLACE TO SAVE INFO FOR LOGS


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
        call       defective        ; "FF: a b c. LF: d e f"
;-----------------------------------------------------------------------
; Data display interface

poll     btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		$-1

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         call       information ;Do the right thing

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-1
         goto       poll


;-------------------------------------------------------------------------
; LOGS INTERFACE            (DONT ACTUALLY HAVE LOGS YET!)
; Eventually this will show logs
; Right now it just displays random crap
; Access from STANDBY and return to STANDBY

logs
    call    Clear_Display
    Display Logs_Msg1
    call    Switch_Lines
    Display Logs_Msg2

polling
    btfss		PORTB,1     ;Wait until data is available from the keypad
    goto		$-1

    swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
    andlw		0x0F
    movwf       keytemp     ; Save which key was pressed

check_log1
    xorlw   0x0         ;will be all zeros if its 1
    btfss   STATUS,Z    ;and Z will be high, so skip
    goto    check_log2
    call    Clear_Display
    Display None
    call    HalfS
    goto    logs

check_log2
    movf    keytemp, W
    xorlw   0x1
    btfss   STATUS,Z
    goto    check_log3
    call    Clear_Display
    Display None
    call    HalfS
    goto    logs

check_log3
    movf    keytemp, W
    xorlw   0x2
    btfss   STATUS,Z
    goto    check_log4
    call    Clear_Display
    Display None
    call    HalfS
    goto    logs

check_log4
    movf    keytemp, W
    xorlw   0x4
    btfss   STATUS,Z
    goto    check_done
    call    Clear_Display
    Display None
    call    HalfS
    goto    logs

check_done
    movf    keytemp, W
    xorlw   0xD
    btfss   STATUS,Z
    goto    badkey
    call    Clear_Display
    Display Standby_Msg
    call    Switch_Lines
    goto    waiting

badkey
    btfsc		PORTB,1     ;Wait until key is released
    goto		$-1
    goto        polling
  
; END OF MAIN PROGRAM
;------------------------------------------------------------


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

check_export               
    movf    keytemp, W
    xorlw   0xF
    btfss   STATUS,Z
    goto    check_standby
    call    export
    return

check_standby
    movf    keytemp, W
    xorlw   0xD
    btfss   STATUS,Z
    goto    check_start
    call    Clear_Display
    Display Standby_Msg
    call    Switch_Lines
    goto    waiting

check_start
    movf    keytemp, W
    xorlw   0xC
    btfss   STATUS,Z
    goto    default_key
    goto    start

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
; Reads data from num_tot, num_LF, num_FF
summary
    call            Clear_Display
    Display         Total_Msg
    writenum_reg    num_tot
    call            Switch_Lines
    Display         LF
    writenum_reg    num_LF
    spacebar
    spacebar
    Display         FF
    writenum_reg    num_FF
    return

; Defective candles Subroutine
; Displays index of each defective candle (LF or FF)
; Reads data from state1 - state9
defective
    call            Clear_Display

    Display         LF              ; first look at LF
    movf            num_LF, F
    btfss           STATUS,Z         ;if none LF, just say "none"
    goto            check_LF
    Display         None
    goto            now_FF

check_LF                            ; list all LF candles
    movlw           B'00000010'
    call            check_for_fail

now_FF                              ; now look at FF
    call            Switch_Lines
    Display         FF
    movf            num_FF, F
    btfss           STATUS,Z         ;if none FF, just say "none"
    goto            check_FF
    Display         None
    return

check_FF                            ; list all FF candles
    movlw           B'00000011'
    call            check_for_fail
    return

check_for_fail  ;lists all candles that have status currently in W "stateX"
    movwf   statetemp
	movlf	D'0', candle_index
	bcf	STATUS, IRP
	movlf	0x20, FSR           ;index of right before state1
check_next
	movlw	D'9'                ;exit loop if at 9
	subwf	candle_index, W
	btfsc	STATUS,W
	goto	end_check_fail
	incf	FSR,F               ; increment
	incf	candle_index,F
	movf	statetemp, W		;see if stateN = stateX
	subwf	INDF, W
	btfss	STATUS, Z
	goto	check_next		;if not, go to next
	writenum_reg	candle_index	;if so, write down the number
	spacebar
	goto check_next
end_check_fail
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
    movf    huns, F
    btfsc   STATUS,Z        ;if huns is zero don't display it
    goto    no_huns
    writenum_reg    huns
no_huns
    movf    tens, F
    btfsc   STATUS,Z        ;if tens is zero don't display it
    goto    no_tens
    writenum_reg    tens
no_tens
    writenum_reg    ones
    Display Seconds
    return

; Export subroutine         
; Exports data from current run to hyperterminal
; reads start time and num_tot/num_ff/num_lf data
export
    printchar   0x7       ;makes a noise on the PC
    printchar   0xD       ;go back to left edge
    printchar   "T"
    printchar   "e"
    printchar   "s"
    printchar   "t"
    printchar   " "
    printchar   "a"
    printchar   "t"
    printchar   ":"
    printchar   " "
    printchar   "2"
    printchar   "0"
    printASC   start_year10
    printASC   start_year1
    printchar   "/"
    printASC   start_month10
    printASC   start_month1
    printchar   "/"
    printASC   start_date10
    printASC   start_date1
    printchar   " "
    printASC   start_hour10
    printASC   start_hour1
    printchar   ":"
    printASC   start_min10
    printASC   start_min1
    newline
    printchar   "T"
    printchar   "i"
    printchar   "m"
    printchar   "e"
    printchar   ":"
    printchar   " "
    printchar   " "
    printchar   " "
    printchar   " "
    banksel huns
    movf    huns, F
    btfsc   STATUS,Z        ;if huns is zero don't display it
    goto    nohuns
    printBCD   huns
nohuns
    banksel tens
    movf    tens, F
    btfsc   STATUS,Z        ;if tens is zero don't display it
    goto    notens
    printBCD   tens
notens
    printBCD   ones
    printchar   " "
    printchar   "s"
    printchar   "e"
    printchar   "c"
    newline
    printchar   "C"
    printchar   "a"
    printchar   "n"
    printchar   "d"
    printchar   "l"
    printchar   "e"
    printchar   "s"
    printchar   ":"
    printchar   " "
    printchar   " "
    printchar   " "
    printchar   " "
    printchar   " "
    printchar   " "
    printBCD    num_tot
    newline
    printchar   "L"
    printchar   "E"
    printchar   "D"
    printchar   " "
    printchar   "f"
    printchar   "a"
    printchar   "i"
    printchar   "l"
    printchar   ":"
    printchar   " "
    printchar   " "
    printchar   " "
    printchar   " "
    printchar   " "
    printBCD    num_LF
    newline
    printchar   "F"
    printchar   "l"
    printchar   "i"
    printchar   "c"
    printchar   "k"
    printchar   "e"
    printchar   "r"
    printchar   " "
    printchar   "f"
    printchar   "a"
    printchar   "i"
    printchar   "l"
    printchar   ":"
    printchar   " "
    printBCD    num_FF
    newline
    newline  
    bcf STATUS,RP0  ; back to bank 0
    return


;***************************************
; GENERAL PURPOSE SUBROUTINES
;***************************************

; DISPLAY BIG NUMBER SUBROUTINE
; Modified from http://www.piclist.com/techref/microchip/math/radix/b2a-8b3d-ab.htm
; Converts 8-bit binary number op_time to three BCDs representing huns, tens, ones
; Uses "shift and add 3" algorithm
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

;***************************************
; LCD SUBROUTINES (from sample code)
;***************************************

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


;***************************************
; PC INTERFACE SUBROUTINES (from sample code)
;***************************************
InitUSART
        bsf       STATUS,RP0     ; select bank 1
        movlw     d'15'          ; BAUD rate 9600, assuming 10MHz oscillator
        movwf     SPBRG
        clrf      TXSTA          ; 8 bits data ,no,1 stop

        bcf       STATUS,RP0     ; select bank 0
        bsf       RCSTA,SPEN     ; Asynchronous serial port enable
        bsf       RCSTA,CREN     ; continuous receive

        bsf       STATUS,RP0     ; select bank 1
        bsf       TXSTA,TXEN     ; Transmit enable
        return


writetoPC
; Writes the data in W to the PC
; end up in bank1!
        bcf       STATUS,RP0     ; Go to bank 0
        movwf     TXREG          ; Send contents of W to RS232
        bsf       STATUS,RP0     ; Go to bank with TXSTA
        btfss     TXSTA,1        ; check TRMT bit in TXSTA (FSR) until TRMT=1
        goto      $-1
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

;    btfss   PHOTODATA       ;if PHOTODATA is 1, light is off
;    incf    photocount, F       ;if it is 0, light is on so photocount++

;    movf    pclath_isr, W  ;if using pages
;    movwf    PCLATH
    swapf   status_isr, W   ;restore W and status
    movwf   STATUS
    swapf   w_isr, F
    swapf   w_isr, W
    bcf     INTCON, T0IF    ;clear the interrupt flag
    retfie


    END





