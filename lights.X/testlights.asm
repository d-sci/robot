;Port Tester
;Sequentially turns on all pins that have debug lights on the DevBugger board

      processor pic16f877
      #include <p16f877.inc>
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

      cblock   0x70
         d1
         d2
		 d3
         i
         mask
      endc

ROTATE   macro    PORT
         bcf      STATUS,C    ;Clear the Carry bit so it isn't rotated into PORT
         rlf      PORT        ;Rotate PORT to the left (bitshift)
         call     delay       ;Delay for a bit so we can actually see it
         endm

      org 0x00
         clrf     INTCON         ;Turn off interrupts
         bsf      STATUS,RP0     ; select bank 1
         clrf     TRISA          ; All ports are completely output
         clrf     TRISB
         clrf     TRISC
         clrf     TRISD
         clrf     TRISE
         movlw    0x06           ;Turn off A/D conversion
         movwf    ADCON1

         ;Reset delay counters
         clrf     d1
         clrf     d2

         ;Initialize all ports to 0
         banksel  PORTA
         clrf     PORTA
         clrf     PORTB
         clrf     PORTC
         clrf     PORTD
         clrf     PORTE

begin    movlw    d'1'     ;Start PORTE as 0b00000001
         movwf    PORTE
         call     delay
         ROTATE   PORTE    ;PORTE = 0b00000010
         clrf     PORTE    ;PORTE = 0

         movlw    d'1'
         movwf    PORTA    ;PORTA = 0b00000001
         call     delay
         ROTATE   PORTA    ;PORTA = b'00000010'
         ROTATE   PORTA    ;PORTA = b'00000100'
         ROTATE   PORTA    ;PORTA = b'00001000'
         ROTATE   PORTA    ;PORTA = b'00010000'
         movlw    b'00100000'  ;Note: RLF doesn't work after RA4 if RA4 not pulled up.
         movwf    PORTA    ; This is because RLF reads PORTA, rotates it, and writes it again.
         call     delay
         clrf     PORTA    ;PORTA = 0

         movlw    d'1'
         movwf    PORTB
         call     delay
         ROTATE   PORTB
         ROTATE   PORTB
         ROTATE   PORTB
         ROTATE   PORTB
         ROTATE   PORTB
         ROTATE   PORTB
         ROTATE   PORTB
         clrf     PORTB

         movlw    d'1'
         movwf    PORTC
         call     delay
         ROTATE   PORTC
         ROTATE   PORTC
         ROTATE   PORTC
         ROTATE   PORTC
         ROTATE   PORTC
         ROTATE   PORTC
         ROTATE   PORTC
         clrf     PORTC

         movlw    d'1'
         movwf    PORTD
         call     delay
         ROTATE   PORTD
         ROTATE   PORTD
         ROTATE   PORTD
         ROTATE   PORTD
         ROTATE   PORTD
         ROTATE   PORTD
         ROTATE   PORTD
         clrf     PORTD

         goto     begin

         ;DELAY FUNCTION
delay    ;Executes loop 65536 times, with approx. 3 instructions per loop (decf=1, goto=2).
		 clrf	  d1
		 clrf	  d2
         ;movlw	  0x7F
		 ;movwf	  d3
		 decfsz   d1
         goto     $-1   ;Count from 255 to 0 (we rely on overflow to reset the counter)
         decfsz   d2
         goto     $-3   ;Nested loop; outer loop also from 255 to 0
         ;decfsz   d3
         ;goto     $-5   ;Nested loop; outer loop also from 255 to 0
         return

      end


