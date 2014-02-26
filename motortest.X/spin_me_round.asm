;Test for motor controlled by L298N. Want to rotate 40deg = 17 steps.
;Excitation sequence is 1-2-3-4 (four steps) controlled by RA[3:0]
;   1 = 1001
;   2 = 1010
;   3 = 0110
;   4 = 0101
;http://duvindu92.blogspot.ca/2013/07/driving-bipolar-stepper-motor-with.html

 list p=16f877
      #include <p16f877.inc>
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF


    cblock  0x70
        del1
        del2
        hdelH
        hdelM
        hdelL
    endc

movlf   macro   l, f
        movlw   l
        movwf   f
        endm

    ORG       0x000
    goto      main
    ORG       0x004
    retfie

main
    clrf        INTCON
    banksel     TRISA
    clrf        TRISA
    movlf       0x07, ADCON1
    banksel     PORTA
    clrf        PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS
    call        ROTATEMOTOR
    goto        $

ROTATEMOTOR
    call    four_steps
    call    four_steps
    call    four_steps
    call    four_steps

    movlf   B'1001', PORTA      ;17th step? seems to go backwards once at beginning then forward 18
    call    delay5ms
    call    delay5ms
    call    HalfS

    movlf   B'1010', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS

    movlf   B'0110', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS

    clrf    PORTA

    return

four_steps
    movlf   B'1001', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS

    movlf   B'1010', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS

    movlf   B'0110', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS

    movlf   B'0101', PORTA
    call    delay5ms
    call    delay5ms
    call    HalfS
    return

delay5ms
	movlf	0xC3, del1
	movlf	0x0A, del2
Delay_0
	decfsz	del1, f
	goto	$+2
	decfsz	del2, f
	goto	Delay_0
    return

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

    END



