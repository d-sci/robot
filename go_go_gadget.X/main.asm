; GO-GO-GADGET CANDLELIGHT!
; David Simons 996639124
; AER201 Spring 2014
; Written for PIC16F877 on DevBugger

;***************************************
;               CONTENTS
;
; Configuration                     31
; Definitions                       38
; Variables                         49
; Macros                            110
; Vectors                           190
; Tables                            200
; Main program                      263
;   Standby                         292
;   Operation                       373
;   Data display interface          555
;   Logs interface                  708
;   Calibration module              915
; Motor subroutines                 967
; Data display subroutines          1045
; General purpose subroutines       1192
; Delay subroutines                 1266
; LCD subroutines                   1315
; PC interface subroutines          1412
; ISR                               1554
;***************************************

   list p=16f877                   ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

      #include <rtc_macros.inc>

;***************************************
; DEFINITIONS
;***************************************
    #define	RS          PORTD,2
	#define	E           PORTD,3
    #define threshold1  D'2'
    #define threshold2  D'151'
    #define IRDATA      PORTE, 0
    #define PHOTODATA   PORTE, 1
    #define continuous  PORTC, 0

;***************************************
; VARIABLES
;***************************************

; Also see i2c_commmon.asm -> has stuff at 0x71-0x78 on for RTC!

    cblock	0x20        ;bank0 variables
    ; Important information (will be displayed)
        state1     ;where   0 = none      1 = pass
        state2     ;        2 = led fail  3 = flicker fail
        state3
        state4
        state5
        state6
        state7
        state8
        state9
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
        op_time     ;BCD in seconds
        num_tot     ;BCD
        num_LF      ;BCD
        num_FF      ;BCD
; For general program / interface: temps, counters, etc.
        keytemp         ; for data display mode
        statetemp
		Table_Counter   ; for LCD stuff
		com
		dat
        delH            ;for delay 0.5s routine
        delM
        delL
        op_time_save    ;for big_number routine
        huns
        tens
        ones
        bignumcount
        count76         ;for isr
        morethansix     ;for displaying defective
; For machine program: temps, counters, etc.
        candle_index
        photocount      ;for testing candle
        photoval
        motor_count     ;for rotating motor
        startfrom3      ;for rotate troubleshooting
    endc

    cblock  0x79        ;stuff that needs to be in all banks (for ISR)
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

;Shortcuts for moving literals / registers
movlf   macro   l, f
        movlw   l
        movwf   f
        endm

movff   macro   source, dest
        movf    source, W
        movwf   dest
        endm

; Write to LCD (all in bank0 please)
writeBCD    macro   reg             ;from a register containing BCD
            movf    reg, W
            addlw   B'00110000'
            call    WR_DATA
            endm

writechar   macro   asc             ;ASCII code literal (or in "")
            movlw   asc
            call    WR_DATA
            endm

writeASC    macro   reg             ;from a register containing ASCII
            movf    reg, W
            call    WR_DATA
            endm

spacebar    macro                   ; move LCD over 1 space
            movlw   B'00010100'
            call    WR_INS
            endm

;Print to PC (hyperterminal). Any bank OK
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

newline     macro                  ;new line on computer
            printchar   0xA         ;changes line
            printchar   0xD         ;goes back to left margin
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
        dt      "1-9 to see logs",0
Logs_Msg2
        addwf   PCL,F
        dt      "STANDBY to exit",0
Logs_Msg3
        addwf   PCL,F
        dt      "Export?",0
Logs_Msg4
        addwf   PCL,F
        dt      "LOGS to return",0
Op_at
        addwf   PCL,F
        dt      "Operation at:",0
Cal_Msg
        addwf   PCL,F
        dt      "Calibration test",0


;***************************************
; MAIN PROGRAM
;***************************************

init
        movlf     b'00100000', INTCON   ; no interrupts yet, but Timer0 ready

        banksel   TRISA                 ; bank 1
        movlf     b'11000110', OPTION_REG ; 1:128 prescaler for timer
        clrf      TRISA                 ; PortA[3:0] used for motor
        movlf     b'11110010', TRISB    ; PortB[7:4] and [1] are keypad inputs
        movlf     b'00011001', TRISC    ; PortC[4:3] is RTC; [7:6] is RS-232; [0] for continuous
        clrf      TRISD                 ; PortD[2:7] is LCD output
        movlf     b'011', TRISE         ; PortE is IR and PHOTODATA
        movlf     0x07, ADCON1          ; digital input

        bcf       STATUS,RP0        ; bank 0
        clrf      PORTA
        clrf      PORTB
        clrf      PORTC
        clrf      PORTD
        clrf      PORTE

        call 	  i2c_common_setup      ; Set up I2C for communication. End in bank1.
        call      InitLCD               ; Initialize the LCD. End in bank0.
        call      InitUSART             ; Set up USART for RS232. End in bank1.

        bcf       STATUS,RP0            ; back to bank0

 ;--------------------------------------------------
 ;      STANDBY MODE

standby
        call        Clear_Display
        Display     Standby_Msg
        call        Switch_Lines

waiting
        ; Display date and time.
        ; Also save starting time for log (will stop updating once we start)
        writechar   "2"             ;first line shows 20**/**/**
        writechar   "0"
        rtc_read	0x06            ;read Address 0x06 from DS1307---year
		movf        0x77,W
        movwf       start_year10    ;save starting year dig10
		call        WR_DATA
		movf        0x78,W
        movwf       start_year1     ;save starting year dig1
		call        WR_DATA
		writechar   "/"
		rtc_read	0x05            ;read Address 0x05 from DS1307---month
		movf        0x77,W
        movwf       start_month10   ;save starting month dig10
		call        WR_DATA
		movf        0x78,W
        movwf       start_month1    ;save starting month dig1
		call        WR_DATA
		writechar	"/"
		rtc_read	0x04            ;read Address 0x04 from DS1307---date
		movf        0x77,W
        movwf       start_date10    ;save starting date dig10
		call        WR_DATA
		movf        0x78,W
        movwf       start_date1     ;save starting date dig1
		call        WR_DATA
		spacebar
		rtc_read	0x02            ;read Address 0x02 from DS1307---hour
		movf        0x77,W
        movwf       start_hour10    ;save starting hour dig10
		call        WR_DATA
		movf        0x78,W
        movwf       start_hour1     ;save starting hour dig1
		call        WR_DATA
		writechar   ":"
		rtc_read	0x01            ;read Address 0x01 from DS1307---min
		movf        0x77,W
        movwf       start_min10     ;save starting min dig10
		call        WR_DATA
		movf        0x78,W
        movwf       start_min1      ;save starting min dig1
		call        WR_DATA

        ; Move cursor back to start of second line
        ; We will update displayed time but not the word "STANDBY"
        movlw       B'11000000'
        call        WR_INS

        ;Poll to start (will have to hold key for ~0.5sec)
         btfss		PORTB,1     ;wait until data is available from the keypad
         goto		waiting     ;meanwhile just update time

         swapf		PORTB,W     ;read PortB<7:4> into W<3:0>
         andlw		0x0F
         movwf      keytemp
         xorlw      0xC         ;will be all zeros if its "START"
         btfsc      STATUS,Z    ;and Z will be high, so skip if not high
         goto       start       ;go to start

         movf       keytemp,W   ;go to log interface if its "LOGS"
         xorlw      0xE
         btfsc      STATUS,Z
         goto       logs

         movf       keytemp,W   ;do calibrate module if its 9
         xorlw      0xA
         btfsc      STATUS,Z
         goto       calibrate

         goto       waiting     ;otherwise just go back

;-----------------------------------------------------------
;       ACTUAL OPERATION

start
        ;Start the timer
        movlf       D'38', count76  ;start with half a second (to round)
        clrf        op_time
        clrf        TMR0
        bsf         INTCON, GIE     ;enable interrupts

        ;Clear accumulators
        clrf    num_FF
        clrf    num_tot
        clrf    num_LF

        ;Display starting message
        call        Clear_Display
        Display     Start_Msg       ;"Inspecting. . ."

        clrf    candle_index        ; n = 0
        bcf     STATUS, IRP
        movlf   0x1F, FSR           ;pointing at right before state1
        movlf	D'5', motor_count   ;first rotation is 20 steps

rotate
        movf	candle_index, F
        btfss	STATUS,Z
        call	interim_display     ; display recent state (but not first)
 
        call    ROTATEMOTOR         ; rotate (4 x motor_count) steps
        incf    candle_index, F     ; n++
        incf    FSR, F
		movlw   D'10'               ; done inspecting once candlex_index (n) = 10
        subwf   candle_index,W      ; n is also # rotations performed
        btfsc   STATUS,Z
        goto    end_operation

detect_candle
   btfsc   IRDATA
   goto    test_candle      ;yes candle, go test it
   call     two_steps       ; two more steps
   btfsc    IRDATA
   goto     test_candle     ;yes candle, it just lagged 2 steps
   call     two_steps       ; two more steps
   btfsc    IRDATA
   goto     test_candle     ;yes candle, it just lagged 4 steps
   call     two_steps       ; two more steps
   btfsc    IRDATA
   goto     test_candle     ;yes candle, it just lagged 6 steps
   call     two_steps       ; two more steps
   btfsc    IRDATA
   goto     test_candle     ;yes candle, it just lagged 8 steps
   call     two_steps       ; two more steps
   btfsc    IRDATA
   goto     test_candle     ;yes candle, it just lagged 10 steps
   call     two_steps       ; two more steps
   btfsc    IRDATA
   goto     test_candle     ;yes candle, it just lagged 12 steps
   movlf    D'0',INDF       ;really no candle
   movlf    D'2', motor_count ;keep rotating, but only 8 more steps
   goto     rotate

test_candle
   incf     num_tot, F          ; keeping track of total number of candles
   clrf     photocount
   call     HalfS               ; test for 2 seconds
   call     HalfS
   call     HalfS
   call     HalfS
   movff    photocount, photoval   ;to ensure it wont change again
check_threshold1
    movlw   threshold1
    subwf   photoval, W
    btfsc   STATUS, C           ;if  photoval < threshold 1, C = 0
    goto    check_threshold2
    movlf   D'2', INDF          ; < threshold 1 means led fail
	incf    num_LF, F
	movlf	D'5', motor_count
    goto    rotate
check_threshold2
    movlw    threshold2
    subwf   photoval, W
    btfsc   STATUS, C           ;if  photoval < threshold 2, C = 0
    goto aboveboth
    movlf   D'1', INDF          ; < threshold 2 means pass
    movlf	D'5', motor_count
    goto    rotate
aboveboth
    movlf   D'3', INDF          ;else flicker fail
    incf    num_FF, F
    movlf	D'5', motor_count
    goto    rotate

end_operation
        ;If it skipped, rotate until back in a serviceable position
        btfss   IRDATA
        goto    worked_fine
        movlf   D'5', motor_count
        call    ROTATEMOTOR
        goto    end_operation

worked_fine
        bcf     INTCON, GIE         ; disable interrupts to stop timer

        ;Display "complete"
        btfsc      continuous
        goto       skip_msg         ; don't display completion msg in continuous mode
        call       Clear_Display
        Display    End_Msg          ; "Complete"

skip_msg
        ; Shift logs 1-8 -> 2-9
shiftlogs
        banksel     EEADR               ; bank 2
        movlf       D'111', EEADR       ; start shifting from 111->125
shiftlogs_0
        banksel     EECON1              ;bank 3
        bcf         EECON1, EEPGD
        bsf         EECON1, RD          ;read (EEADR) to EEDATA
        btfsc       EECON1, WR          ; ensure a write is not in progress
        goto        $-1
        banksel     EEADR               ;bank 2
        movlw       D'14'               ;add 14 to EEADR to shift 14 places
        addwf       EEADR, F
        banksel     EECON1              ;bank 3
        bcf         EECON1, EEPGD
        bsf         EECON1, WREN
        movlw       0x55
        movwf       EECON2
        movlw       0xAA
        movwf       EECON2
        bsf         EECON1, WR          ; write EEDATA to (EEADR+14)
        bcf         EECON1, WREN
        banksel     EEADR               ;bank 2
        movlw       D'14'
        subwf       EEADR, W
        btfsc       STATUS, Z
        goto        write_log1          ; if EEADR = 14 we're done 
        banksel     op_time             ; for some reason I need to delay here
        call        delay5ms             ; or else I get an infinite loop
        banksel     EEADR
        movlw       D'15'               ;else EEADR -= 15 to shift next byte
        subwf       EEADR, F
        goto        shiftlogs_0

        ; Then store current run as log 1
write_log1
        banksel     EEADR
        bcf         STATUS, IRP
        movlf       0x37, FSR           ;points to 1 after last important data
        movlf       D'14', EEADR        ;points to 1 after last place to write
write_log1_0
        banksel     EECON1              ;bank3
        btfsc       EECON1, WR          ;ensure a write is not in progress
        goto        $-1
        banksel     EEADR               ;bank 2
        decf        EEADR, F            ;writing backwords
        decf        FSR, F
        movff       INDF, EEDATA
        banksel     EECON1              ;bank 3
        bcf         EECON1, EEPGD
        bsf         EECON1, WREN
        movlw       0x55
        movwf       EECON2
        movlw       0xAA
        movwf       EECON2
        bsf         EECON1, WR          ; write (INDF) to EEADR
        bcf         EECON1, WREN
        banksel     EEADR               ;bank 2
        movf        EEADR, F
        btfss       STATUS, Z           ;if EEADR = 0 we're done
        goto        write_log1_0
        bcf         STATUS, RP0         ;so go back to bank 0 and continue
        bcf         STATUS, RP1

        ;just restart if in continuous mode
        btfsc   continuous
        goto    start

        ; Display defective screen
        call       defective        ; "FF: a b c. LF: d e f"

;-----------------------------------------------------------------------
; DATA DISPLAY INTERFACE

poll     btfss		PORTB,1     ;Wait until data is available from the keypad
         goto		$-1
         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
         movwf      keytemp     ; Save which key was pressed

check_1
    xorlw       0x0             ;will be all zeros if its 1
    btfss       STATUS,Z        ;and Z will be high, so skip
    goto        check_2         ;otherwise go check if its 2
    call        Clear_Display
    writechar   "1"             ;for 1-9, will display its state
    movf        state1, W
    call        display_state
    goto        poll

check_2
    movf        keytemp, W
    xorlw       0x1
    btfss       STATUS,Z
    goto        check_3
    call        Clear_Display
    writechar   "2"
    movf        state2, W
    call        display_state
    goto        poll

check_3
    movf        keytemp, W
    xorlw       0x2
    btfss       STATUS,Z
    goto        check_4
    call        Clear_Display
    writechar   "3"
    movf        state3, W
    call        display_state
    goto        poll

check_4
    movf        keytemp, W
    xorlw       0x4
    btfss       STATUS,Z
    goto        check_5
    call        Clear_Display
    writechar   "4"
    movf        state4, W
    call        display_state
    goto        poll

check_5
    movf        keytemp, W
    xorlw       0x5
    btfss       STATUS,Z
    goto        check_6
    call        Clear_Display
    writechar   "5"
    movf        state5, W
    call        display_state
    goto        poll

check_6
    movf        keytemp, W
    xorlw       0x6
    btfss       STATUS,Z
    goto        check_7
    call        Clear_Display
    writechar   "6"
    movf        state6, W
    call        display_state
    goto        poll

check_7
    movf        keytemp, W
    xorlw       0x8
    btfss       STATUS,Z
    goto        check_8
    call        Clear_Display
    writechar   "7"
    movf        state7, W
    call        display_state
    goto        poll

check_8
    movf        keytemp, W
    xorlw       0x9
    btfss       STATUS,Z
    goto        check_9
    call        Clear_Display
    writechar   "8"
    movf        state8, W
    call        display_state
    goto        poll

check_9
    movf        keytemp, W
    xorlw       0xA
    btfss       STATUS,Z
    goto        check_summary
    call        Clear_Display
    writechar   "9"
    movf        state9, W
    call        display_state
    goto        poll

check_summary                       ;display summary screen
    movf        keytemp, W
    xorlw       0x3
    btfss       STATUS,Z
    goto        check_defective
    call        summary
    goto        poll

check_defective                     ;display defective screen
    movf        keytemp, W
    xorlw       0x7
    btfss       STATUS,Z
    goto        check_time
    call        defective
    goto        poll

check_time                          ;display operation time
    movf        keytemp, W
    xorlw       0xB
    btfss       STATUS,Z
    goto        check_export
    call        time
    goto        poll

check_export                        ;export data
    movf    keytemp, W
    xorlw   0xF
    btfss   STATUS,Z
    goto    check_standby
    call    export
    goto    poll

check_standby                       ;return to standby
    movf    keytemp, W
    xorlw   0xD
    btfss   STATUS,Z
    goto    check_start
    goto    standby

check_start                         ;start next operation
    movf    keytemp, W
    xorlw   0xC
    btfss   STATUS,Z
    goto    poll                    ;if you hit "LOGS", do nothing
    goto    start

;-------------------------------------------------------------------------
; LOGS INTERFACE
; Shows runs of last 9 logs
; Access from STANDBY and return to STANDBY

logs
    call        Clear_Display
    Display     Logs_Msg1
    call        Switch_Lines
    Display     Logs_Msg2

polling
    btfss		PORTB,1     ;Wait until data is available from the keypad
    goto		$-1
    swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
    andlw		0x0F
    movwf       keytemp     ;Save which key was pressed

check_log1
    xorlw   0x0             ;will be all zeros if its 1
    btfss   STATUS,Z        ;and Z will be high, so skip
    goto    check_log2
    banksel EEADR           ;for 1-9, retrieve and display data from EEPROM
    movlf   d'0', EEADR
    goto    display_log

check_log2
    movf    keytemp, W
    xorlw   0x1
    btfss   STATUS,Z
    goto    check_log3
    banksel EEADR
    movlf   d'14', EEADR
    goto    display_log

check_log3
    movf    keytemp, W
    xorlw   0x2
    btfss   STATUS,Z
    goto    check_log4
    banksel EEADR
    movlf   d'28', EEADR
    goto    display_log

check_log4
    movf    keytemp, W
    xorlw   0x4
    btfss   STATUS,Z
    goto    check_log5
    banksel EEADR
    movlf   d'42', EEADR
    goto    display_log

check_log5
    movf    keytemp, W
    xorlw   0x5
    btfss   STATUS,Z
    goto    check_log6
    banksel EEADR
    movlf   d'56', EEADR
    goto    display_log

check_log6
    movf    keytemp, W
    xorlw   0x6
    btfss   STATUS,Z
    goto    check_log7
    banksel EEADR
    movlf   d'70', EEADR
    goto    display_log

check_log7
    movf    keytemp, W
    xorlw   0x8
    btfss   STATUS,Z
    goto    check_log8
    banksel EEADR
    movlf   d'84', EEADR
    goto    display_log

check_log8
    movf    keytemp, W
    xorlw   0x9
    btfss   STATUS,Z
    goto    check_log9
    banksel EEADR
    movlf   d'98', EEADR
    goto    display_log

check_log9
    movf    keytemp, W
    xorlw   0xA
    btfss   STATUS,Z
    goto    check_done
    banksel EEADR
    movlf   d'112', EEADR
    goto    display_log

check_done                  ;return to standby
    movf    keytemp, W
    xorlw   0xD
    btfsc   STATUS,Z
    goto    standby

    btfsc	PORTB,1     ;Otherwise wait until key is released
    goto	$-1
    goto    polling     ;and continue polling


display_log                 ;starts in bank2

    ; move EEPROM data back to "current" data
    bcf     STATUS, IRP
    movlf   0x29, FSR
read_EEPROM
    banksel EECON1          ;bank3
    bcf     EECON1, EEPGD
    bsf     EECON1, RD      ; read EEPROM
    banksel EEDATA          ;bank 2
    movff   EEDATA, INDF    ; move EEDATA to "current" data
    incf    FSR, F
    incf    EEADR, F
    movlw   0x37            ;done if FSR = 0x37
    subwf   FSR, W
    btfss   STATUS, Z
    goto    read_EEPROM

    ;display data
    banksel Table_Counter   ; bank0
    call    Clear_Display
    ;if first data is 0xFF (no log) just display "none"
    movlw   0xFF
    subwf   start_year10, W
    btfss   STATUS, Z
    goto yes_log
no_log
    Display None
    call    HalfS
    goto    logs
    ;else display the log
yes_log
    Display     Op_at
    call        Switch_Lines
    writechar   "2"
    writechar   "0"
    writeASC    start_year10
    writeASC    start_year1
    writechar   "/"
    writeASC    start_month10
    writeASC    start_month1
    writechar   "/"
    writeASC    start_date10
    writeASC    start_date1
    writechar   " "
    writeASC    start_hour10
    writeASC    start_hour1
    writechar   ":"
    writeASC    start_min10
    writeASC    start_min1
    call        HalfS
    call        HalfS
    call        time
    call        HalfS
    call        HalfS
    call        summary
    call        HalfS
    call        HalfS

    ;option to Export
    call        Clear_Display
    Display     Logs_Msg3
    call        Switch_Lines
    Display     Logs_Msg4

wanna_export
    btfss		PORTB,1     ;Wait until data is available from the keypad
    goto		$-1
    swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
    andlw		0x0F
    movwf       keytemp     ; Save which key was pressed

check_wanna                 ;export
    xorlw   0xF
    btfss   STATUS,Z
    goto    check_nothx
    call    export
    goto    logs

check_nothx
    movf    keytemp,W
    andlw   B'0011'
    xorlw   B'0011'
    btfsc   STATUS,Z
    goto    wanna_export    ;summary,defective, or time -> do nothing

    movf    keytemp,W
    xorlw   0xC
    btfsc   STATUS,Z
    goto    wanna_export    ;start -> do nothing

    movf    keytemp,W
    xorlw   0xE
    btfsc   STATUS,Z
    goto    logs            ;logs -> back to logs menu

    goto    check_log1      ;otherwise it was 1-9 or standby

;-------------------------------------------------------------------------
; CALIBRATION MODULE
; For adjusting sensitivity of photoresistor.
; Tests a candle and displays # of times PR reads high (0-152)
; As well as what state this means (LF, PASS, FF)

calibrate
    call        Clear_Display
    Display     Cal_Msg
    call        Switch_Lines
    clrf        photocount
    clrf        TMR0
    bsf         INTCON, GIE     ;enable interrupts
    call        HalfS
    call        HalfS
    call        HalfS
    call        HalfS
    movff       photocount, op_time ;temporary use op_time
    bcf         INTCON,GIE
    call        big_number          ;so can display as decimanl
    writeBCD    huns
    writeBCD    tens
    writeBCD    ones
cal1
    movlw       threshold1
    subwf       op_time, W
    btfsc       STATUS, C       ;if  < threshold 1, C = 0
    goto        cal2
    Display     LED_fail        ; < threshold 1 means led fail
    goto        end_calibrate
cal2
    movlw       threshold2
    subwf       op_time, W
    btfsc       STATUS, C       ;if  < threshold 2, C = 0
    goto        cal3
    Display     Pass            ; < threshold 2 means pass
    goto        end_calibrate
cal3
    writechar   ":"             ; else flicker fail
    writechar   " "                  ; but write "FF" so itll fit
    writechar   "F"
    writechar   "F"

end_calibrate
    call        HalfS
    call        HalfS
    goto        standby

; END OF MAIN PROGRAM
;------------------------------------------------------------


;***************************************
; MOTOR SUBROUTINES
; ROTATEMOTOR will rotate motor by number of steps in motor_count times 4.
; (5 gives 20 steps = 36 deg = one slot)
; pulses ABCD assuming startfrom3 = 0; else pulses CDAB
; uses half-stepping
;***************************************

ROTATEMOTOR
    btfsc   startfrom3, 0
    goto    halfway
start_rot
    call	pulseA
    call	pulseB
    btfsc   startfrom3, 0
    goto    decrement
halfway
    call	pulseC
    call	pulseD
    btfsc   startfrom3, 0
    goto    start_rot
decrement
    decfsz  motor_count
    goto    restart_motor
    goto    finish_motor
restart_motor
    btfsc   startfrom3, 0
    goto    halfway
    goto    start_rot
finish_motor
    clrf    PORTA
    return

two_steps                      ; pulses only two steps
    btfsc   startfrom3, 0
    goto    CD
AB
    call	pulseA
    call	pulseB
    clrf    PORTA
    bsf     startfrom3, 0
    return
CD
	call	pulseC
	call	pulseD
	clrf	PORTA
	bcf		startfrom3, 0
	return

pulseA
	movlf   b'1001', PORTA
    call    motor_del
    movlf   b'1000', PORTA
    call    motor_del
    return

pulseB
	movlf   b'1010', PORTA
    call    motor_del
    movlf   b'0010', PORTA
    call    motor_del
    return

pulseC    
    movlf   b'0110', PORTA
    call    motor_del
    movlf   b'0100', PORTA
    call    motor_del
    return

pulseD
    movlf   b'0101', PORTA
    call    motor_del
    movlf   b'0001', PORTA
    call    motor_del
    return
    
    
;***************************************
; DATA DISPLAY SUBROUTINES
;***************************************

;Display state subroutine
;stateN is in W
display_state
    movwf  statetemp            ; save stateN
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
default_state                   ; should never get here
    return

; Summary subroutine
; Displays total number of candles and number of defective candles
; Reads data from num_tot, num_LF, num_FF
summary
    call            Clear_Display
    Display         Total_Msg
    writeBCD        num_tot
    call            Switch_Lines
    Display         LF
    writeBCD        num_LF
    spacebar
    spacebar
    Display         FF
    writeBCD        num_FF
    return

; Defective candles subroutine
; Displays index of each defective candle (LF or FF)
; Reads data from state1 - state9
defective
    call            Clear_Display
    clrf            morethansix
    movlw           d'7'
    subwf           num_LF, W
    btfsc           STATUS, C       ;if  num_LF < 7, C = 0
    bsf             morethansix,0
    movlw           d'7'
    subwf           num_FF, W
    btfsc           STATUS, C       ;if  num_FF < 7, C = 0
    bsf             morethansix,0
    Display         LF              ; first look at LF
    movf            num_LF, F
    btfss           STATUS,Z        ;if none LF, just say "none"
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

check_for_fail                      ; lists all candles that have status currently in W
    movwf       statetemp
	movlf       D'0', candle_index
	bcf         STATUS, IRP
	movlf       0x1F, FSR           ;index of right before state1
check_next
	movlw       D'9'                ;exit loop if at 9
	subwf       candle_index, W
	btfsc       STATUS,W
	goto        end_check_fail
	incf        FSR,F               ; increment
	incf        candle_index,F
	movf        statetemp, W		;see if stateN = stateX
	subwf       INDF, W
	btfss       STATUS, Z
	goto        check_next          ;if not, go to next
	writeBCD	candle_index        ;if so, write down the number
    btfsc       morethansix,0       ;don't write spaces if need more than 6
    goto        more_than_six
    spacebar
more_than_six
	goto        check_next
end_check_fail
	return

; Time subroutine
; Takes the time in seconds as a binary number in op_time
; and puts it into huns,tens,ones to display.
time
    call    Clear_Display
    Display Time_Msg
    call    Switch_Lines
    call    big_number      ;convert to BCDs
    movf    huns, F
    btfsc   STATUS,Z        ;if huns is zero don't display it
    goto    no_huns
    writeBCD    huns
no_huns
    movf    tens, F
    btfsc   STATUS,Z        ;if tens is zero don't display it
    goto    no_tens
    writeBCD    tens
no_tens
    writeBCD    ones
    Display Seconds
    return

; Interim display subroutine
; Displays state of candle as it's being tested
interim_display
	call		Clear_Display
	writeBCD	candle_index
	movf		INDF, W
	call		display_state
	return


;***************************************
; GENERAL PURPOSE SUBROUTINES
;***************************************

; DISPLAY BIG NUMBER SUBROUTINE
; Converts 8-bit binary number op_time to 3 BCDs representing huns, tens, ones
; Uses "shift and add 3" algorithm, modified from:
; http://www.piclist.com/techref/microchip/math/radix/b2a-8b3d-ab.htm

big_number
    movff   op_time, op_time_save   ;save the original op_time
    movlf   8, bignumcount          ;will shift 8 times
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
    goto    BCDadd3                   ;repeat until you've shifted it 8 times

    movff   op_time_save, op_time     ;restore the original op_time
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
; DELAY SUBROUTINES
;***************************************

; DELAY 0.5S SUBROUTINE 
; Delays exactly 0.5sec, from generator at:
; http://www.piclist.com/techref/piclist/codegen/delay.htm
HalfS
      movlf     0x8A, delH
      movlf     0xBA, delM
      movlf     0x03, delL
HalfS_0
      decfsz	delH, F
	  goto      $+2
	  decfsz	delM, F
	  goto      $+2
	  decfsz	delL, F
	  goto      HalfS_0
	  goto      $+1
	  nop
	  return

; DELAY 5ms SUBROUTINE
; Useful for LCD because PIC is way faster than it can handle
; Delays exactly 5ms, from generator at:
; http://www.piclist.com/techref/piclist/codegen/delay.htm
delay5ms
	movlf       0xC3, delH
	movlf       0x0A, delL
Delay_0
	decfsz      delH, f
	goto        $+2
	decfsz      delL, f
	goto        Delay_0
    return

; MOTOR DELAY SUBROUTINE.
; Delays ~25ms for the motor. (~1sec per total rotation)
motor_del
      movlf     0xF3, delH
      movlf     0x37, delL
motor_del_0
      decfsz	delH, F
	  goto      $+2
	  decfsz	delL, F
	  goto      motor_del_0
	  return


;***************************************
; LCD SUBROUTINES (from sample code)
;***************************************

; Initialize the LCD
InitLCD
	bcf STATUS,RP0          ;bank0
	bsf E                   ;E default high
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
	call	delay5ms        ;__    __
	bcf		E				;  |__|
	swapf	com,w
	andlw	0xF0			;1111 0010
	movwf	PORTD			;send 4 bits LSB
	bsf		E				;
	call	delay5ms        ;__    __
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
	call	delay5ms        ;__    __
	bcf		E				;  |__|
	swapf	dat,w
	andlw	0xF0
	addlw	4
	movwf	PORTD
	bsf		E				;
	call	delay5ms        ;__    __
	bcf		E				;  |__|
	return


;***************************************
; PC INTERFACE SUBROUTINES (modified from sample code)
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
    printASC    start_year10
    printASC    start_year1
    printchar   "/"
    printASC    start_month10
    printASC    start_month1
    printchar   "/"
    printASC    start_date10
    printASC    start_date1
    printchar   " "
    printASC    start_hour10
    printASC    start_hour1
    printchar   ":"
    printASC    start_min10
    printASC    start_min1
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
    banksel     op_time
    call        big_number
    movf        huns, F
    btfsc       STATUS,Z        ;if huns is zero don't display it
    goto        nohuns
    printBCD    huns
nohuns
    banksel     tens
    movf        tens, F
    btfsc       STATUS,Z        ;if tens is zero don't display it
    goto        notens
    printBCD    tens
notens
    printBCD    ones
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
    bcf         STATUS,RP0
    bcf         STATUS,RP1     ; back to bank 0
    return


;***************************************
; ISR
; Currently only care about TMR0
; TMR0 overflows at 256*128; each time, decrement count76
; count76 thus hits 0 every 256*128*76 cycles = 1sec with 10MHz clock
; When this hapens, op_time increments
; Also check photodata every time for 2 sec = total of 152 reads
;***************************************

isr
    movwf   w_isr           ;save W and status
    swapf   STATUS, W
    clrf    STATUS
    movwf   status_isr

    decfsz  count76, F     ;if count76 gets to 76 it's been one second
    goto    end_isr
    movlf   D'76', count76  ;so reset count76
    incf    op_time, F         ; and increment op_time

end_isr
    btfsc   PHOTODATA       ;if PHOTODATA is 1, light is on
    incf    photocount, F       ;if it is 1, light is on so photocount++

    swapf   status_isr, W   ;restore W and status
    movwf   STATUS
    swapf   w_isr, F
    swapf   w_isr, W
    bcf     INTCON, T0IF    ;clear the interrupt flag
    retfie


    END
