;Test for motor. Want to rotate 40deg = 17 steps.
;Excitation sequence is 1-2-3-4-1 (four steps) controlled by RE[1:0]
;   1 = 11
;   2 = 01
;   3 = 00
;   4 = 10
;RE2 is PWM(?). ON is 0; OFF is 1.
;See Kim pg 156

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
    banksel     PORTE
    clrf        PORTE
    call        ROTATEMOTOR
    goto        $

ROTATEMOTOR
    bcf     PORTE, 2            ; power ON
    call    four_steps
    call    four_steps
    call    four_steps
    call    four_steps
    movlf   B'011', PORTE       ;last step??
    call    delay5ms
    call    delay5ms
    call    delay5ms
    call    delay5ms
    movlf   B'001', PORTE
    call    delay5ms
    call    delay5ms
    call    delay5ms
    call    delay5ms
    bsf     PORTE, 2            ; power OFF
    return

four_steps
    movlf   B'011', PORTE
    call    delay5ms
    call    delay5ms
    call    delay5ms
    call    delay5ms
    movlf   B'001', PORTE
    call    delay5ms
    call    delay5ms
    call    delay5ms
    call    delay5ms
    movlf   B'000', PORTE
    call    delay5ms
    call    delay5ms
    call    delay5ms
    call    delay5ms
    movlf   B'10', PORTE
    call    delay5ms
    call    delay5ms
    call    delay5ms
    call    delay5ms
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
