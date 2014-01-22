
#include <p18f4620.inc>
#include <lcd18.inc>
    list P=18F4620, F=INHX32, C=160, N=80, ST=OFF, MM=OFF, R=DEC

;;;;;;Configuration Bits;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		CONFIG OSC=HS, FCMEN=OFF, IESO=OFF
		CONFIG PWRT = OFF, BOREN = SBORDIS, BORV = 3
		CONFIG WDT = OFF, WDTPS = 32768
		CONFIG MCLRE = ON, LPT1OSC = OFF, PBADEN = OFF, CCP2MX = PORTC
		CONFIG STVREN = ON, LVP = OFF, XINST = OFF
		CONFIG DEBUG = OFF
		CONFIG CP0 = OFF, CP1 = OFF, CP2 = OFF, CP3 = OFF
		CONFIG CPB = OFF, CPD = OFF
		CONFIG WRT0 = OFF, WRT1 = OFF, WRT2 = OFF, WRT3 = OFF
		CONFIG WRTB = OFF, WRTC = OFF, WRTD = OFF
		CONFIG EBTR0 = OFF, EBTR1 = OFF, EBTR2 = OFF, EBTR3 = OFF
		CONFIG EBTRB = OFF


;;;;;;Vectors;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			org		0x0000
			goto	Init
			org		0x08				;high priority ISR
			retfie
			org		0x18				;low priority ISR
			retfie

		code

Init

         clrf      INTCON         ; No interrupts
         clrf      TRISA          ; All port A is output
         movlw     b'11110010'    ; Set required keypad inputs
         movwf     TRISB
         clrf      TRISC          ; All port C is output
         clrf      TRISD          ; All port D is output

         clrf      LATA
         clrf      LATB
         clrf      LATC
         clrf      LATD

         call      InitLCD    ;Initialize the LCD (code in lcd.asm; imported by lcd.inc)

test     btfss		PORTB,1   ;Wait until data is available from the keypad
         goto		test

         swapf		PORTB,W     ;Read PortB<7:4> into W<3:0>
         andlw		0x0F
		 rlncf		WREG, W		;Program Memory in PIC18 counts up by 2
         call     	KPHexToChar ;Convert keypad value to LCD character (value is still held in W)
         call     	WrtLCD      ;Write the value in W to LCD

         btfsc		PORTB,1     ;Wait until key is released
         goto		$-2
         goto    	test

KPHexToChar
          addwf     PCL,f
          dt        "123A456B789C*0#D"

          END


