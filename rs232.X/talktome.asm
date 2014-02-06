;RS232 Test Program
;Sends a welcome message and echoes back incoming data

        processor   pic16f877
        include     <p16f877.inc>
        __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

offset  EQU       0x20
temp    EQU       0x21

        ORG     0x0000
;************ initialize *******************
init    bsf       STATUS,RP0     ; select bank 1
        clrf      TRISD

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
        bcf       STATUS,RP0     ; select bank 0
        clrf      offset         ; Reset offset to 0

start   movf      offset,w       ; Load TAB[offset] into W
        call      TAB
        addlw     0              ; Character = 00 ?
        btfsc     STATUS,Z       ; (Adding 0 causes Z bit to be set if W contains 0)
        goto      run            ; Yes , Z = 1 => We're done sending welcome message
                                 ; No  , Z = 0 => Keep sending welcome message

        movwf     TXREG          ; Send contents of W to RS232

        bsf       STATUS,RP0     ; Go to bank with TXSTA
        btfss     TXSTA,1        ; check TRMT bit in TXSTA (FSR) until TRMT=1
        goto      $-1
        bcf       STATUS,RP0     ; Go back to bank 0

        incf      offset,f       ; Move to next character in welcome message and repeat
        goto      start

;********** Start echoing characters back on RS232 *******************
run     btfss     PIR1,RCIF      ; Check RCIF  bit in PIR1 register until RCIF=1
        goto      $-1

        movf      RCREG,w        ; Read the character received from RS232
        movwf     TXREG          ; Write it right back to RS232

        goto      run

;********* Table containing welcome message ********************
TAB     addwf     PCL,f            ; Move offset to PC lower
        dt  "Hello David, This is PICtricia talking.",0

        end


