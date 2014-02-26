; Test for pushing solenoid to turn candle off.

list p=16f877
      #include <p16f877.inc>
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

    cblock  0x70
        del1
        del2
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
    banksel     TRISE
    clrf        TRISE
    movlf       0x07, ADCON1
    banksel     PORTE
    clrf        PORTE
    call        PULSESOLENOID
    goto        $

PULSESOLENOID
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

    END


