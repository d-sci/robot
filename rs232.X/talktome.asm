;RS232 Test Program
;Sends a welcome message and echoes back incoming data

        processor   pic16f877
        include     <p16f877.inc>
        __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

        cblock  0x20
            testnum2
            testnum4
            endc


printchar   macro   char
            movlw   char
            call    writetoPC
            endm

printBCD    macro   BCD
            movfw   BCD
            addlw   0x30
            call    writetoPC
            endm



offset  EQU       0x20
temp    EQU       0x21

        ORG     0x0000
;************ initialize *******************
init    bsf       STATUS,RP0     ; select bank 1
        ;clrf      TRISD

        ;Setup USART for RS232
        movlw     d'15'          ; BAUD rate 9600, assuming 10MHz oscillator
        movwf     SPBRG
        clrf      TXSTA          ; 8 bits data ,no,1 stop

        bcf       STATUS,RP0     ; select bank 0
        bsf       RCSTA,SPEN     ; Asynchronous serial port enable
        bsf       RCSTA,CREN     ; continuous receive

        bsf       STATUS,RP0     ; select bank 1
        bsf       TXSTA,TXEN     ; Transmit enable

;********** Send welcome message **********************

    movlw   D'2'
    movwf   testnum2
    movlw   D'4'
    movwf   testnum4

    printchar   "H"
    printchar   "I"
    printchar   " "
    printBCD    testnum4
    printBCD    testnum2

      goto  $
        
writetoPC
; Writes the data in W to the PC
; end up in bank1!
        bcf       STATUS,RP0     ; Go to bank 0
        movwf     TXREG          ; Send contents of W to RS232
        bsf       STATUS,RP0     ; Go to bank with TXSTA
        btfss     TXSTA,1        ; check TRMT bit in TXSTA (FSR) until TRMT=1
        goto      $-1
        return


;********** Start echoing characters back on RS232 *******************
;run     btfss     PIR1,RCIF      ; Check RCIF  bit in PIR1 register until RCIF=1
;        goto      $-1
;
;        movf      RCREG,w        ; Read the character received from RS232
;        movwf     TXREG          ; Write it right back to RS232
;
;        goto      run

;********* Table containing welcome message ********************
;TAB     addwf     PCL,f            ; Move offset to PC lower
;        dt  "Hello David, This is PICtricia talking.",0

        end


