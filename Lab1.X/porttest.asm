;Port Tester for PIC184620
;Sequentially turns on all pins that have debug lights on the DevBugger board
#include <p18f4620.inc>
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

;;;;;;Variables;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      cblock   0x70
         d1                   ;at 0x70
         d2                   ;at 0x71
      endc

;;;;;;Macros;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ROTATE   macro    PORT
         bcf      STATUS,C    ;Clear the Carry bit so it isn't rotated into PORT
         rlncf    PORT        ;Rotate PORT to the left (bitshift)
         call     delay       ;Delay for a bit so we can actually see it
         endm


;;;;;;Code;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      org 0x00
         clrf     INTCON         ;Turn off interrupts
         clrf     TRISA          ; All ports are completely output
         clrf     TRISB
         clrf     TRISC
         clrf     TRISD
         clrf     TRISE
         movlw    0x0F           ;Turn off A/D conversion
         movwf    ADCON1

         ;Initialize all ports to 0
         clrf     LATA
         clrf     LATB
         clrf     LATC
         clrf     LATD
         clrf     LATE

begin    movlw    d'1'     ;Start PORTE as 0b00000001
         movwf    LATE
         call     delay
         ROTATE   LATE    ;PORTE = 0b00000010
         clrf     LATE    ;PORTE = 0

         movlw    d'1'
         movwf    LATA    ;PORTA = 0b00000001
         call     delay
         ROTATE   LATA    ;PORTA = b'00000010'
         ROTATE   LATA    ;PORTA = b'00000100'
         ROTATE   LATA    ;PORTA = b'00001000'
         ROTATE   LATA    ;PORTA = b'00010000'
         ROTATE	  LATA	  ;PORTA = b'00100000;
         clrf     LATA    ;PORTA = 0

         movlw    d'1'
         movwf    LATB
         call     delay
         ROTATE   LATB
         ROTATE   LATB
         ROTATE   LATB
         ROTATE   LATB
         ROTATE   LATB
         ROTATE   LATB
         ROTATE   LATB
         clrf     LATB

         movlw    d'1'
         movwf    LATC
         call     delay
         ROTATE   LATC
         ROTATE   LATC
         ROTATE   LATC
         ROTATE   LATC
         ROTATE   LATC
         ROTATE   LATC
         ROTATE   LATC
         clrf     LATC

         movlw    d'1'
         movwf    LATD
         call     delay
         ROTATE   LATD
         ROTATE   LATD
         ROTATE   LATD
         ROTATE   LATD
         ROTATE   LATD
         ROTATE   LATD
         ROTATE   LATD
         clrf     LATD

         goto     begin

         ;DELAY FUNCTION
delay    ;Executes loop 65536 times, with approx. 3 instructions per loop (decf=1, goto=2).
		 clrf	  d1
		 clrf	  d2
delay1	 decfsz   d1, 1	;Count from 255 to 0 (we rely on overflow to reset the counter)
         goto     $-2   ;Note: each instruction is 2 bytes long, so to go to the previous instruction, $-2 is needed
        ;goto	  delay1	- Alternatively, could use label
		 decfsz   d2, 1
         goto     $-8   ;NOTE: each goto instruction is 2 words long (4 bytes) so $-8 is needed to go to label delay1
		;goto	  delay1
		 return
         end