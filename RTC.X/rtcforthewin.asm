      list p=16f877                 ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

      #include <lcd.inc>			   ;Import LCD control functions from lcd.asm
	  #include <rtc_macros.inc>

	udata_shr
	COUNTH	res 1	;const used in delay
	COUNTM	res	1	;const used in delay
	COUNTL	res	1	;const used in delay


         ORG       0x0000     ;RESET vector must always be at 0x00
         goto      init       ;Just jump to the main code section.

         ORG	   0x100

;***************************************
; Initilization of both LCD and RTC
;***************************************
init
         clrf      INTCON         ; No interrupts

         bsf       STATUS,RP0     ; select bank 1
         clrf      TRISA          ; All port A is output
         clrf	   TRISB		  ; All port B is output
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

		 ;Set up I2C for communication
		 call 	   i2c_common_setup
		 ;rtc_resetAll

		 ;Used to set up time in RTC, load to the PIC when RTC is used for the first time
		 call	   set_rtc_time

         call      InitLCD    ;Initialize the LCD (code in lcd.asm; imported by lcd.inc)

show_RTC
		;clear LCD screen
		movlw	b'00000001'
		call	WR_INS

		;Get year
		movlw	"2"				;First line shows 20**/**/**
		call	WR_DATA
		movlw	"0"
		call	WR_DATA
		rtc_read	0x06		;Read Address 0x06 from DS1307---year
		movfw	0x77
		call	WR_DATA
		movfw	0x78
		call	WR_DATA

		movlw	"/"
		call	WR_DATA

		;Get month
		rtc_read	0x05		;Read Address 0x05 from DS1307---month
		movfw	0x77
		call	WR_DATA
		movfw	0x78
		call	WR_DATA

		movlw	"/"
		call	WR_DATA

		;Get day
		rtc_read	0x04		;Read Address 0x04 from DS1307---day
		movfw	0x77
		call	WR_DATA
		movfw	0x78
		call	WR_DATA

		movlw	B'11000000'		;Next line displays (hour):(min):(sec) **:**:**
		call	WR_INS

		;Get hour
		rtc_read	0x02		;Read Address 0x02 from DS1307---hour
		movfw	0x77
		call	WR_DATA
		movfw	0x78
		call	WR_DATA
		movlw			":"
		call	WR_DATA

		;Get minute
		rtc_read	0x01		;Read Address 0x01 from DS1307---min
		movfw	0x77
		call	WR_DATA
		movfw	0x78
		call	WR_DATA
		movlw			":"
		call	WR_DATA

		;Get seconds
		rtc_read	0x00		;Read Address 0x00 from DS1307---seconds
		movfw	0x77
		call	WR_DATA
		movfw	0x78
		call	WR_DATA

		call	OneS			;Delay for exactly one seconds and read DS1307 again
		goto	show_RTC

;***************************************
; Setup RTC with time defined by user
;***************************************
set_rtc_time

		rtc_resetAll	;reset rtc

		rtc_set	0x00,	B'10000000'

		;set time
		rtc_set	0x06,	B'00010100'		; Year
		rtc_set	0x05,	B'00000011'		; Month
		rtc_set	0x04,	B'00100000'		; Date
		rtc_set	0x03,	B'00000100'		; Day
		rtc_set	0x02,	B'010100000'		; Hours
		rtc_set	0x01,	B'00100011'		; Minutes
		rtc_set	0x00,	B'00100000'		; Seconds
		return


;***************************************
; Delay 1s
;***************************************
OneS
		local	OneS_0
      movlw 0x10
      movwf COUNTH
      movlw 0x7A
      movwf COUNTM
      movlw 0x06
      movwf COUNTL

OneS_0
      decfsz COUNTH, f
      goto   $+2
      decfsz COUNTM, f
      goto   $+2
      decfsz COUNTL, f
      goto   OneS_0

      goto $+1
      nop
      nop
		return


          END






