; ============================================================================
; AER201 Chip-Packing Machine
; Programmer: Yih Tang Yeo
; Note: all changes should be committed using Git and pushed up to Bitbucket
; Please read: https://bitbucket.org/yihtangyeo/aer201/wiki/Home before coding
; ============================================================================
    ERRORLEVEL  -207,-205

#include <p18f4620.inc>
    list P=18F4620, F=INHX32, C=160, N=80, ST=OFF, MM=OFF, R=DEC

    ; Configuration bits, refer link below for documentation
    ; http://ww1.microchip.com/downloads/en/DeviceDoc/51537a.pdf
    config OSC  = HS, FCMEN = OFF, IESO = OFF
    config PWRT = OFF, BOREN = SBORDIS, BORV = 3
    config WDT  = OFF, WDTPS = 32768
    config MCLRE = ON, LPT1OSC = OFF, PBADEN = OFF, CCP2MX = PORTC
    config STVREN = ON, LVP = OFF, XINST = OFF
    config DEBUG = OFF
    config CP0 = OFF, CP1 = OFF, CP2 = OFF, CP3 = OFF, CPB = OFF, CPD = OFF
    config WRT0 = OFF, WRT1 = OFF, WRT2 = OFF, WRT3 = OFF
    config WRTB = OFF, WRTC = OFF, WRTD = OFF
    config EBTR0 = OFF, EBTR1 = OFF, EBTR2 = OFF, EBTR3 = OFF, EBTRB = OFF

; ============================================================================
; Definitions
; ============================================================================
#define  LCD_RS  PORTD, 2
#define  LCD_E   PORTD, 3

; ============================================================================
; General Purpose Registers (using Access Bank)
; ============================================================================

KeypadToCheck               equ 0x20    ;
KeypadCheckResult           equ 0x21    ;
DelayCounter1               equ 0x22    ;
DelayCounter2               equ 0x23    ;
DelayCounter3               equ 0x24    ;

UserSelectPattern1          equ 0x25    ;
UserSelectPattern2          equ 0x26    ;
TurntablePattern            equ 0x27    ;
DispenserPattern            equ 0x28    ;
CountingPattern             equ 0x29    ;

PortB_data                  equ 0x2A    ;

TurntableTotalStepsH        equ 0x30    ;
TurntableTotalStepsL        equ 0x31    ;
DispenserTotalStepsH        equ 0x32    ;
DispenserTotalStepsL        equ 0x33    ;
CountingTotalStepsH         equ 0x34    ;
CountingTotalStepsL         equ 0x35    ;

TurntableTargetH            equ 0x36    ;
TurntableTargetL            equ 0x37    ;
TurntableTargetIndexH       equ 0x38    ;
TurntableTargetIndexL       equ 0x39    ;
TurntableMagneticWidth      equ 0x3A
TurntableStepEEPROMIndexH   equ 0x3B
TurntableStepEEPROMIndexL   equ 0x3C

ChipCountAmber              equ 0x3D    ;
ChipCountCamel              equ 0x3E    ;
ChipAmountToDispense        equ 0x3F    ;
ChipDispensedOrNot_IR       equ 0x40    ;

DivideDividendH             equ 0x41    ;
DivideDividendL             equ 0x42    ;
DivideDivisor               equ 0x43    ;
DivideQuotient              equ 0x44    ;

RTC_Second                  equ 0x45    ;
RTC_Minute                  equ 0x46    ;
RTC_Hour                    equ 0x47    ;
RTC_Day                     equ 0x48    ;
RTC_Date                    equ 0x49    ;
RTC_Month                   equ 0x4A    ;
RTC_Year                    equ 0x50    ;
RTC_L                       equ 0x51    ;
RTC_H                       equ 0x52    ;

RTC_Second_Old              equ 0x53    ;
RTC_Minute_Old              equ 0x54    ;
RTC_Hour_Old                equ 0x55    ;

RTC_Second_Diff             equ 0x56    ;
RTC_Minute_Diff             equ 0x57    ;

EEPROM_CurrentLocationH     equ 0x58    ;
EEPROM_CurrentLocationL     equ 0x59    ;

DispenserChipPatternH       equ 0x5A    ;
DispenserChipPatternL       equ 0x5B    ;
TemporaryVariable           equ 0x5C    ;
TemporaryVariable2          equ 0x5D    ;
TemporaryVariable3          equ 0x5E    ;
TemporaryVariable4          equ 0x5F    ;
TemporaryVariable5          equ 0x60    ;
TemporaryVariable6          equ 0x61    ;

TemporaryVariable7          equ 0x62    ;
TemporaryVariable8          equ 0x63    ;
TemporaryVariable9          equ 0x64
TemporaryVariable10         equ 0x65
TemporaryVariable11         equ 0x66
TemporaryVariable12         equ 0x67

ChipCountHeightAmberH       equ 0x68    ;
ChipCountHeightAmberL       equ 0x69    ;
ChipCountHeightCamelH       equ 0x6A    ;
ChipCountHeightCamelL       equ 0x6B    ;

DispenseSuccessOrFail       equ 0x6C ;
DispenseContainerCount      equ 0x6D ;

ToolsMenuCount              equ 0x6E

ClosingContainer1StepCount  equ 0x6F    ;
ClosingCount1True           equ 0x70    ;
ClosingContainer2StepCount  equ 0x71    ;
ClosingCount2True           equ 0x72    ;
ClosingPositionFromEEPROM   equ 0x73    ;
VibratorCountDown           equ 0x74    ;
VibratorCountUp             equ 0x75    ;
VibratorUpEnabled           equ 0x76
VibratorDownEnabled         equ 0x77

TableLastLine1U             equ 0x78
TableLastLine1H             equ 0x79
TableLastLine1L             equ 0x7A
TableLastLine2U             equ 0x7B
TableLastLine2H             equ 0x7C
TableLastLine2L             equ 0x7D

NumberOfAmberRequested      equ 0x7E
NumberOfCamelRequested      equ 0x7F

; ===========================================================================
; Macros
; ============================================================================

; -----------------------------------------------------------------------------
; General purpose macros
; -----------------------------------------------------------------------------

; ifequal macro. If (Var == Value), Then goto Label
ifequal  macro  Var, Value, Label
    movlw   Value
    subwf   Var, 0                      ; check if Var-Value=0
    bz      Label
    endm

; ifequal_reg macro. If (Var == Value), Then goto Label
ifequal_reg macro  Var, Value, Label
    movff   Value, WREG
    subwf   Var, 0                      ; check if Var-Value!=0
    bz     Label
    endm

; ifequal macro. If (Var != Value), Then goto Label
ifnequal macro  Var, Value, Label
    movlw   Value
    subwf   Var, 0                      ; check if Var-Value!=0
    bnz     Label
    endm

; ifequal_reg macro. If (Var != Value), Then goto Label
ifnequal_reg macro  Var, Value, Label
    movff   Value, WREG
    subwf   Var, 0                      ; check if Var-Value!=0
    bnz     Label
    endm

; store macro. Put a literal Value right into a file register at Destination
store macro Destination, Value
    movlw   Value
    movwf   Destination
    endm


; -----------------------------------------------------------------------------
; LCD related macros
; -----------------------------------------------------------------------------

; display macro. Take in the TableName and display the characters on LineNumber
display  macro  TableName, LineNumber
    movlw   LineNumber                  ; go to line 1 if LineNumber 1000000
    call    WriteInstToLCD              ; go to line 2 if LineNumber 1100000
    movlw   upper TableName             ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableName
    movwf   TBLPTRH
    movlw   low TableName
    movwf   TBLPTRL
    ; Do 16 Loops to display all 16 characters in the TableName
    call    Write16DataToLCD
    endm

backupdisplay1  macro   TableName
    movlw   upper   TableName
    movwf   TableLastLine1U
    movlw   high   TableName
    movwf   TableLastLine1H
    movlw   low   TableName
    movwf   TableLastLine1L
    endm

backupdisplay2  macro   TableName
    movlw   upper   TableName
    movwf   TableLastLine2U
    movlw   high   TableName
    movwf   TableLastLine2H
    movlw   low   TableName
    movwf   TableLastLine2L
    endm


; printch macro. Display Char on the LCD screen, at specific Position
printch  macro Char, Position
    movlw   Position
    call    WriteInstToLCD
    movlw   Char
    call    WriteDataToLCD
    endm

; ---------------------------------------------------------------------------
; 16 bit data operation
; ---------------------------------------------------------------------------

; rrcf16 macro. Rotate right for 16 bit data
rrcf16 macro bit8H, bit8L
    bcf     STATUS, 0                   ; clear carry flag
    rrcf    bit8H, 1                    ; rotate right with carry flag
    rrcf    bit8L, 1                    ; rotate right with carry flag
                                        ; bit8L MSB will be filled by values
                                        ; from the bit8H LSB
    bcf     STATUS, 0                   ; clear carry flag again
    endm

; addwf16 macro. Add a Value to 16 bit data
addwf16 macro bit8H, bit8L, Value
    bcf     STATUS, 0                   ; clear carry flag
    movlw   Value                       ; store value to working register
    addwf   bit8L, 1                    ; add to lower 8 bit
                                        ; if there is a carry out, C = 1
    movlw   d'0'
    addwfc  bit8H, 1                    ; bit8H = bit8H + 0 + C
    bcf     STATUS, 0                   ; clear carry flag again
    endm

; addwf16 macro. Add a Value to 16 bit data
addwf16reg macro bit8H, bit8L, Register
    bcf     STATUS, 0                   ; clear carry flag
    movff   Register, WREG              ; store value to working register
    addwf   bit8L, 1                    ; add to lower 8 bit
                                        ; if there is a carry out, C = 1
    movlw   d'0'
    addwfc  bit8H, 1                    ; bit8H = bit8H + 0 + C
    bcf     STATUS, 0                   ; clear carry flag again
    endm

; subwf16 macro. Deduct a Value from 16 bit data
subwf16 macro bit8H, bit8L, Value
    bcf     STATUS, 0                   ; clear carry flag
    movlw   Value                       ; store value to working register
    subwf   bit8L, 1                    ; normal substraction, if there is a
                                        ; borrow, the C = 0
    movlw   d'0'
    subwfb  bit8H, 1                    ; bit8L = bit8L - 0 - !C
    endm

; ---------------------------------------------------------------------------
; EEPROM related macros
; ---------------------------------------------------------------------------

; readEEPROM macro. Read a byte at a time from Addr and store it at EEPROM_dest
; Reference: PIC18F4620 Data Sheet
readEEPROM  macro AddrH, AddrL, EEPROM_dest
    movlw   AddrH
    movwf   EEADRH
    movlw   AddrL
    movwf   EEADR
    bcf     EECON1, EEPGD           ; Point to Data memory
    bcf     EECON1, CFGS            ; Access EEPROM
    bsf     EECON1, RD              ; EEPROM read
    movff   EEDATA, EEPROM_dest
    endm

; readEEPROM_REG macro. Read a byte at a time, using value STORED in a Register
; that has an address REG_Addr, and store the output at EEPROM_dest
readEEPROM_REG  macro REG_AddrH, REG_AddrL, EEPROM_dest
    movff   REG_AddrH, EEADRH
    movff   REG_AddrL, EEADR
    bcf     EECON1, EEPGD           ; Point to Data memory
    bcf     EECON1, CFGS            ; Access EEPROM
    bsf     EECON1, RD              ; EEPROM read
    movff   EEDATA, EEPROM_dest
    endm


; writeEEPROM_REG macro. Write a byte at a time, read from a register
; named EEPROM_data_reg
writeEEPROM  macro  REG_AddrH, REG_AddrL, EEPROM_data_reg
    movff   REG_AddrH, EEADRH
    movff   REG_AddrL, EEADR
    movff   EEPROM_data_reg, EEDATA
    btfsc   EECON1, WR              ; check if WR = 0
    bra     $-2
    bcf     EECON1, EEPGD           ; Point to Data memory
    bcf     EECON1, CFGS            ; Access EEPROM
    bsf     EECON1, WREN            ; Enable Write
    bcf     INTCON, GIE             ; Disable Interrupts
    bcf     PIR2, EEIF
    movlw   0x55
    movwf   EECON2                  ; Write 55h (Must)
    movlw   0xAA
    movwf   EECON2                  ; Write 0AAh (Must)
    bsf     EECON1, WR              ; set WR to begin write
    btfsc   EECON1, WR              ; check if WR = 0
    bra     $-2                     ; if not 0, keep checking
    bsf     INTCON, GIE             ; reenable interrupt
    bcf     EECON1, WREN            ; disable write
    endm


writeEEPROM_inst  macro  REG_AddrH, REG_AddrL, EEPROM_data
    movlw   REG_AddrH
    movwf   EEADRH
    movlw   REG_AddrL
    movwf   EEADR
    movlw   EEPROM_data
    movwf   EEDATA
    btfsc   EECON1, WR              ; check if WR = 0
    bra     $-2
    bcf     EECON1, EEPGD           ; Point to Data memory
    bcf     EECON1, CFGS            ; Access EEPROM
    bsf     EECON1, WREN            ; Enable Write
    bcf     INTCON, GIE             ; Disable Interrupts
    bcf     PIR2, EEIF
    movlw   0x55
    movwf   EECON2                  ; Write 55h (Must)
    movlw   0xAA
    movwf   EECON2                  ; Write 0AAh (Must)
    bsf     EECON1, WR              ; set WR to begin write
    btfsc   EECON1, WR              ; check if WR = 0
    bra     $-2                     ; if not 0, keep checking
    bsf     INTCON, GIE             ; reenable interrupt
    bcf     EECON1, WREN            ; disable write
    endm

writeEEPROM_REG  macro  REG_AddrH, REG_AddrL, EEPROM_data
    movlw   REG_AddrH
    movwf   EEADRH
    movlw   REG_AddrL
    movwf   EEADR
    movff   EEPROM_data, EEDATA
    btfsc   EECON1, WR              ; check if WR = 0
    bra     $-2
    bcf     EECON1, EEPGD           ; Point to Data memory
    bcf     EECON1, CFGS            ; Access EEPROM
    bsf     EECON1, WREN            ; Enable Write
    bcf     INTCON, GIE             ; Disable Interrupts
    bcf     PIR2, EEIF
    movlw   0x55
    movwf   EECON2                  ; Write 55h (Must)
    movlw   0xAA
    movwf   EECON2                  ; Write 0AAh (Must)
    bsf     EECON1, WR              ; set WR to begin write
    btfsc   EECON1, WR              ; check if WR = 0
    bra     $-2                     ; if not 0, keep checking
    bsf     INTCON, GIE             ; reenable interrupt
    bcf     EECON1, WREN            ; disable write
    endm

; ===========================================================================
; Code begins
; ===========================================================================
    org     0x0000                  ; Reset vector
    goto    Main

    org     0x0008                  ; When Low Priority Interrupt occurs
    goto    Interrupt

    org     0x0018                  ; When High Priority Interrupt occurs    
    retfie

; ===========================================================================
; Table for LCD Display
; ===========================================================================
TableIdle1              db      "500-HOUR MACHINE", 0
TableIdle2              db      "  :         on>A", 0
TableOn1                db      "A<off    tools>B", 0
TableOn2                db      "      dispense>C", 0
TableLoadChips1         db      "   LOAD CHIPS   ", 0
TableLoadChips2         db      " IN RESERVOIRS  ", 0
TableEStopArmed1        db      " EMERGENCY STOP ", 0
TableEStopArmed2        db      "   ACTIVATED    ", 0
TableEStopDisarmed1     db      "    RESUMING    ", 0
TableEStopDisarmed2     db      "  OPERATIONS... ", 0
TablePattern11          db      "PATTERN #1:     ", 0
TablePattern21          db      "PATTERN #2:     ", 0
TablePattern2           db      "<1-6>  <0-empty>", 0
TablePatternInvalid     db      "<invalid input> ", 0
TableReadyToStart1      db      "SELECTED:       ", 0
TableReadyToStart2      db      "C<cancel start>A", 0
TableDispense1          db      "   DISPENSING   ", 0
TableDispense2          db      " Please wait... ", 0
TableDisplay11          db      "PACKING COMPLETE", 0
TableDisplay12          db      "Status:         ", 0
TableDisplay13          db      "Duration: __m__s", 0
TableDisplay14          db      "Pattern: _,_    ", 0
TableDisplay15          db      "Amber Chip: __  ", 0
TableDisplay16          db      "Camel Chip: __  ", 0
TableDisplay2           db      " <hold any key> ", 0
TablePCInterface1       db      "  PC INTERFACE  ", 0
TablePCInterface2       db      "C<exit          ", 0
TableUseTools1          db      "TOOLS:     <1-9>", 0
TableUseTools2          db      "C<exit   <<A B>>", 0
TableUseTools21         db      "1 PC Interface  ", 0
TableUseTools22         db      "2 Read Log Info ", 0
TableUseTools23         db      "3 Calb Turntable", 0
TableUseTools24         db      "4 Calb Dispenser", 0
TableUseTools25         db      "5 Calb Counting ", 0
TableUseTools26         db      "6 Test Vibrator ", 0
TableUseTools27         db      "7 Calb Closing  ", 0
TableUseTools28         db      "8 Test Closing  ", 0
TableUseTools29         db      "9 Test Counting ", 0
TableUseTools3          db      "<enter 1-9 now> ", 0
TableReadLogInfo1       db      "READING LOG...  ", 0
TableReadLogInfo2       db      "A<exit    next>B", 0
TableReadLogInfo1time   db      "MM/DD HH:MM:SS  ", 0
TableReadLogInfo2info   db      "<Info goes here>", 0
TableCalibrateTurntable1    db  "CALIB TURNTABLE ", 0
TableCalibrateTurntable2    db  "C<exit   start>A", 0
TableCalibrateDispenser1    db  "CALIB DISPENSER ", 0
TableCalibrateDispenser2    db  "C<exit   start>A", 0
TableTestTurntable1         db  "CAL CLOSING MECH", 0
TableTestTurntable21        db  "Press A to Mark ", 0
TableTestTurntable22        db  "Press B to Reset", 0
TableTestTurntable23        db  "C<exit   start>A", 0
TableTestVibrator1          db  " TEST VIBRATOR  ", 0
TableTestVibrator2          db  "C<exit   start>A", 0
TableTestDispenser1         db  " TEST DISPENSER ", 0
TableTestDispenser21        db  "press A - Amber ", 0
TableTestDispenser22        db  "press B - Camel ", 0
TableTestDispenser23        db  "press C - Cancel", 0
TableTestClosing1           db  "  TEST CLOSING  ", 0
TableTestClosing2           db  "  Executing...  ", 0
TableCalibrateCounting1     db  " CAL COUNT MECH ", 0
TableCalibrateCounting2     db  "Have 15 Chips In", 0
TableCalibrateCounting3     db  "C<   Amb>A Cam>B", 0
TableTestCounting1          db  " TEST COUNTING  ", 0
TableTestCounting2          db  "C<exit   start>A", 0
TableCalibrateDone1         db  "CALIBRATION DONE", 0
TableCalibrateDone2         db  "Return to menu..", 0
TableTestDone1              db  "  TESTING DONE  ", 0
TableTestDone2              db  "Return to menu..", 0
TableTestDoneCounting       db  "A#__     C#__   ", 0   ; 2,3,11,12
TableGoodbye                db  "    GOODBYE!    ", 0
TableGoodbye2               db  "Have a nice day!", 0
TableRunning1               db  "PROCESS RUNNING ", 0
TableRunning2               db  " Please Wait... ", 0
TablePreRunCalib1           db  " CALIBRATIING.. ", 0
TablePreRunCalib2           db  " Please Wait... ", 0
TableShutContainer1         db  "    CLOSING     ", 0
TableShutContainer2         db  "   CONTAINER    ", 0
TableWarningInsuffAmber1    db  " WARNING: Amber ", 0
TableWarningInsuffAmber2    db  " Insufficient!  ", 0
TableWarningInsuffAmber3    db  " Insuff. Amber  ", 0
TableWarningInsuffAmber4    db  "C<exit proceed>A", 0
TableWarningInsuffCamel1    db  " WARNING: Camel ", 0
TableWarningInsuffCamel2    db  " Insufficient!  ", 0
TableWarningInsuffCamel3    db  " Insuff. Camel  ", 0
TableWarningInsuffCamel4    db  "C<exit proceed>A", 0

; ============================================================================
; Table for Log
; ============================================================================

TableLogIdle            db      "Machine idle", 0
TableLogTurnOn          db      "Machine turn on", 0
TableLogTurnOff         db      "Machine turn off", 0
TableLogEStop           db      "E-Stop activated", 0
TableLogUserSelectPattern   db  "User selected Pattern: ", 0
TableLogVibratorOn      db      "Reservoir vibrator On", 0
TableLogVibratorOff     db      "Reservoir vibrator Off", 0
TableLogDispenseEnd     db      "Dispense complete", 0
TableLogPCInterface     db      "PC interface established", 0
TableLogReadLog         db      "Reading log", 0
TableLogUserSelectPatternCancel db  "Cancelled dispense entry", 0
TableLogCalibrateCounting       db  "Calibrating counting mechanism", 0
TableLogCalibrateDispenser      db  "Calibrating dispenser", 0
TableLogCalibrateTurntable      db  "Calibrating turntable", 0
TableLogTestVibrator    db      "Testing vibrator", 0
TableLogCalibClosing    db      "Cal. Closing Mech", 0
TableLogTestDispenser   db      "Testing dispenser", 0
TableLogTestCounting    db      "Testing counting", 0
TableLogEStopDeactivated    db  "E-Stop deactivated", 0
TableLogDispenseStarted     db  "Dispensing started", 0

; ============================================================================
; Main Program starts here
; ============================================================================

Main
    Initialize
        ; set all ports as output
        clrf    TRISA
        clrf    TRISB
        clrf    TRISC
        clrf    TRISD
        clrf    TRISE
        ; set all ports to low
        clrf    PORTA
        clrf    PORTB
        clrf    PORTC
        clrf    PORTD
        clrf    PORTE

        ; initialize GPR
        ; start EEPROM at 100, before that will be for configuration bits
        store   EEPROM_CurrentLocationH, 0x01
        store   EEPROM_CurrentLocationL, 0x00

        ; pin assignment
        ; Refer Eagle schematic diagram
        store   TRISA,  B'11001111'
        store   TRISB,  B'11110111'
        store   TRISC,  B'11011000'
        store   TRISD,  B'00000000'
        store   TRISE,  B'00000111'
        store   ADCON1, B'00001111'     ; set to digital input output

        ; reset stepper motor pattern
        store   DispenserPattern, B'00011110'
        store   TurntablePattern, B'00011110'
        store   CountingPattern, B'00011110'

        ; set interrupts
        clrf    INTCON
        bcf     RCON, IPEN                  ; disable priority interrupt
        bsf		INTCON, GIE                 ; enable global interrupt
        bsf     INTCON, INT0IE              ; enable INT0 int flag bit on RB0
        bsf     INTCON2, INTEDG0            ; set INT0 to detect rising edge

        call    InitializeLCD
        call    InitializeI2C

                ; for UART
                store   SPBRGH, d'0'                ; according to formula X = ((F_osc/baudrate)/64)-1
                store   SPBRG, d'15'                ; set baud rate to 9600, see table 18-3
                bcf     BAUDCON, BRG16              ; not using 16 bit
                bcf     TXSTA, BRGH
                bcf     TXSTA, SYNC                 ; asynchronous communication
                bsf     TRISC, 6                    ; C6 as input
                bsf     TRISC, 7                    ; C7 as input
                bsf     RCSTA, SPEN                 ; enable serial port
                bcf     PIE1, TXIE                  ; disable interrupt
                bcf     TXSTA, TX9                  ; want 8 bit only
                bsf     TXSTA, TXEN                 ; enable transmission

        ; TODO turn on calibration upon startup
        display TablePreRunCalib1, B'10000000'
        backupdisplay1  TablePreRunCalib1
        display TablePreRunCalib2, B'11000000'
        backupdisplay2  TablePreRunCalib2
        ;call    CalibrateTurntable
        call    CalibrateDispenser
        call    MoveCounterToTop
        ;call    CalibrateCounting


        writeEEPROM_inst    0x00, 0x10, 0x04;
        writeEEPROM_inst    0x00, 0x11, 0xA3
        writeEEPROM_inst    0x00, 0x12, 0x0B;
        writeEEPROM_inst    0x00, 0x13, 0x78
        writeEEPROM_inst    0x00, 0x14, 0x11;
        writeEEPROM_inst    0x00, 0x15, 0x6B
        writeEEPROM_inst    0x00, 0x16, 0x17;
        writeEEPROM_inst    0x00, 0x17, 0x8B
        writeEEPROM_inst    0x00, 0x18, 0x1D;
        writeEEPROM_inst    0x00, 0x19, 0xEA
        writeEEPROM_inst    0x00, 0x1A, 0x24;
        writeEEPROM_inst    0x00, 0x1B, 0x36
        writeEEPROM_inst    0x00, 0x1C, 0x2A;
        writeEEPROM_inst    0x00, 0x1D, 0x6E
        writeEEPROM_inst    0x00, 0x1E, 0x2F;
        writeEEPROM_inst    0x00, 0x1F, 0xFC
        writeEEPROM_inst    0x00, 0x20, 0x35;
        writeEEPROM_inst    0x00, 0x21, 0x61
        writeEEPROM_inst    0x00, 0x22, 0x3B;
        writeEEPROM_inst    0x00, 0x23, 0xC8

        writeEEPROM_inst    0x00, 0x24, 0x04;
        writeEEPROM_inst    0x00, 0x25, 0x83
        writeEEPROM_inst    0x00, 0x26, 0x0B;
        writeEEPROM_inst    0x00, 0x27, 0x0A
        writeEEPROM_inst    0x00, 0x28, 0x11;
        writeEEPROM_inst    0x00, 0x29, 0x71
        writeEEPROM_inst    0x00, 0x2A, 0x17;
        writeEEPROM_inst    0x00, 0x2B, 0x98
        writeEEPROM_inst    0x00, 0x2C, 0x1D;
        writeEEPROM_inst    0x00, 0x2D, 0xDD
        writeEEPROM_inst    0x00, 0x2E, 0x24;
        writeEEPROM_inst    0x00, 0x2F, 0x3D
        writeEEPROM_inst    0x00, 0x30, 0x29;
        writeEEPROM_inst    0x00, 0x31, 0xE8
        writeEEPROM_inst    0x00, 0x32, 0x30;
        writeEEPROM_inst    0x00, 0x33, 0x0A
        writeEEPROM_inst    0x00, 0x34, 0x36;
        writeEEPROM_inst    0x00, 0x35, 0x9C
        writeEEPROM_inst    0x00, 0x36, 0x3C;
        writeEEPROM_inst    0x00, 0x37, 0x89

        ; for closing mechanism precalibrated
        writeEEPROM_inst    0x00, 0x0A, 0x3E;




    MainLoop
        ; --------------------------------------------------------------------
        ; MODE 1: Idle Screen, Welcome Screen
        ; --------------------------------------------------------------------
        IdleScreen  
                call    LogIdle
                call    ClearLCD               

                ; reset EEPROM
                store   EEPROM_CurrentLocationH, 0x00
                store   EEPROM_CurrentLocationL, 0x80

                ;call    SetRTC
                ;call    StepTurntable_DEBUG

                display TableIdle1, B'10000000'
                backupdisplay1  TableIdle1
                display TableIdle2, B'11000000'
                backupdisplay2  TableIdle2

        IdleScreen_Loop 

                

                call    DisplayRTC_BottomLeft                

                store   KeypadToCheck, b'00000011'     ; check for key A
                call    CheckForKeypress
                ifequal KeypadCheckResult, d'1', MenuScreen
        
        goto    IdleScreen_Loop

        ; --------------------------------------------------------------------
        ; MODE 2: Menu Screen
        ; --------------------------------------------------------------------
        MenuScreen
                call    LogTurnOn
                call    ClearLCD

                display TableLoadChips1, B'10000000'
                backupdisplay1  TableLoadChips1
                display TableLoadChips2, B'11000000'
                backupdisplay2  TableLoadChips2

                store   VibratorCountDown, 0xAA
                store   VibratorCountUp, 0x00

                store   VibratorUpEnabled, 0x00
                store   VibratorDownEnabled, 0x01


                call    DelayMenu
                call    MoveCounterToTop
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow
                call    StepCountingDownSlow

                display TableOn1, B'10000000'
                backupdisplay1  TableOn1
                display TableOn2, B'11000000'
                backupdisplay2  TableOn2

        MenuScreen_Loop


                    CheckIfVibratorDown
                    ifnequal    VibratorDownEnabled, 0x01, CheckIfVibratorUp
                    ifnequal    VibratorCountDown, 0x00, VibratorDownMenu
                    ; if equal to 0
                    store   VibratorCountUp, 0xAA
                    store   VibratorDownEnabled, 0x00
                    store   VibratorUpEnabled, 0x01
                    ;call    Delay5ms

                    CheckIfVibratorUp
                    ifnequal    VibratorUpEnabled, 0x01, NoVibrate
                    ifnequal    VibratorCountUp, 0x00, VibratorUpMenu
                    ; if equal to 0
                    store   VibratorCountDown, 0xAA
                    store   VibratorDownEnabled, 0x01
                    store   VibratorUpEnabled, 0x00
                    ;call    Delay5ms

                    VibratorDownMenu
                        call    StepCountingDown
                        decf    VibratorCountDown
                        goto    NoVibrate

                    VibratorUpMenu
                        call    StepCountingUp
                        decf    VibratorCountUp

                    NoVibrate

                    store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', IdleScreen_short
                        bra     MenuScreen_Next
                    IdleScreen_short
                        display TableGoodbye, B'10000000'
                        backupdisplay1  TableGoodbye
                        display TableGoodbye2, B'11000000'
                        backupdisplay2  TableGoodbye2
                        call    DelayMenu
                        goto    IdleScreen

                    MenuScreen_Next

                    store   KeypadToCheck, b'00000111'     ; check for key B
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', UseTools_short
                        bra     MenuScreen_Next2
                    UseTools_short
                        goto    UseTools
                
                MenuScreen_Next2

                store   KeypadToCheck, b'00001011'     ; check for key C
                call    CheckForKeypress
                ifequal KeypadCheckResult, d'1', SelectPattern_short
                    bra     MenuScreen_Next3
                SelectPattern_short
                    goto    SelectPattern

                MenuScreen_Next3

        goto    MenuScreen_Loop


        ; --------------------------------------------------------------------
        ; MODE 3: Select Pattern Screen
        ; --------------------------------------------------------------------
        
        SelectPattern
                call    ClearLCD
                store   NumberOfAmberRequested, d'0'
                store   NumberOfCamelRequested, d'0'


            SelectPattern_Loop1
                    display TablePattern11, B'10000000'
                    backupdisplay1  TablePattern11
                    display TablePattern2, B'11000000'
                    backupdisplay2  TablePattern2

                    call    CheckKeyForDispense
                    ; KeypadCheckResult = 0-6 means OK result, 9 = invalid
                    ifnequal KeypadCheckResult, d'9', SelectPattern2
            goto    SelectPattern_Loop1

            SelectPattern2
                ; store Pattern 1 first
                movff   KeypadCheckResult, UserSelectPattern1

                call    ClearLCD                

            SelectPattern_Loop2
                    display TablePattern21, B'10000000'
                    backupdisplay1  TablePattern21
                    display TablePattern2, B'11000000'
                    backupdisplay2  TablePattern2

                    call    CheckKeyForDispense
                    ; KeypadCheckResult = 0-6 means OK result, 9 = invalid
                    ifnequal KeypadCheckResult, d'9', SelectPattern3
            goto    SelectPattern_Loop2

            SelectPattern3
                ; store Pattern 2 first
                movff   KeypadCheckResult, UserSelectPattern2

                call    ClearLCD                


                    display TableReadyToStart1, B'10000000'
                    backupdisplay1  TableReadyToStart1
                    display TableReadyToStart2, B'11000000'
                    backupdisplay2  TableReadyToStart2
                    call    LogUserSelectPattern

                    ; display user selected pattern on LCD
                    movlw   B'10001010'
                    call    WriteInstToLCD

                    movff   UserSelectPattern1, WREG
                    call    ConvertDecimalToASCII
                    call    WriteDataToLCD

                    movlw   ','
                    call    WriteDataToLCD

                    movff   UserSelectPattern2, WREG
                    call    ConvertDecimalToASCII
                    call    WriteDataToLCD

            SelectPattern_Loop3

                    store   KeypadToCheck, b'00001011'     ; check for key C
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', MenuScreen_short
                        bra     SelectPattern_Next
                    MenuScreen_short
                        call    LogUserSelectPatternCancelled
                        goto    MenuScreen

                    SelectPattern_Next

                    store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', DispenseScreen_short
                        bra     SelectPattern_Next2
                    DispenseScreen_short
                        goto    DispenseScreen

                    SelectPattern_Next2

            goto    SelectPattern_Loop3

        ; --------------------------------------------------------------------
        ; MODE 4: Tools (Debug) Screen
        ; --------------------------------------------------------------------
        UseTools
                call    ClearLCD
                store   ToolsMenuCount, d'0'

                UseTools_Loop

                    display TableUseTools1, B'10000000'
                    backupdisplay1  TableUseTools1

                    ifequal ToolsMenuCount, d'0', ToolsDisplay0
                    ifequal ToolsMenuCount, d'1', ToolsDisplay1
                    ifequal ToolsMenuCount, d'2', ToolsDisplay2
                    ifequal ToolsMenuCount, d'3', ToolsDisplay3
                    ifequal ToolsMenuCount, d'4', ToolsDisplay4
                    ifequal ToolsMenuCount, d'5', ToolsDisplay5
                    ifequal ToolsMenuCount, d'6', ToolsDisplay6_short
                    ifequal ToolsMenuCount, d'7', ToolsDisplay7_short
                    ifequal ToolsMenuCount, d'8', ToolsDisplay8_short
                    ifequal ToolsMenuCount, d'9', ToolsDisplay9_short

                    ToolsDisplay6_short
                            goto    ToolsDisplay6
                    ToolsDisplay7_short
                            goto    ToolsDisplay7
                    ToolsDisplay8_short
                            goto    ToolsDisplay8
                    ToolsDisplay9_short
                            goto    ToolsDisplay9


                    ToolsDisplay0
                            display TableUseTools2, B'11000000'
                            backupdisplay2  TableUseTools2
                            goto    CheckKeypadToolsMenu
                    ToolsDisplay1
                            display TableUseTools21, B'11000000'
                            backupdisplay2  TableUseTools21
                            goto    CheckKeypadToolsMenu
                    ToolsDisplay2
                            display TableUseTools22, B'11000000'
                            backupdisplay2  TableUseTools22
                            goto    CheckKeypadToolsMenu
                    ToolsDisplay3
                            display TableUseTools23, B'11000000'
                            backupdisplay2  TableUseTools23
                            goto    CheckKeypadToolsMenu
                    ToolsDisplay4
                            display TableUseTools24, B'11000000'
                            backupdisplay2  TableUseTools24
                            goto    CheckKeypadToolsMenu
                    ToolsDisplay5
                            display TableUseTools25, B'11000000'
                            backupdisplay2  TableUseTools25
                            goto    CheckKeypadToolsMenu
                    ToolsDisplay6
                            display TableUseTools26, B'11000000'
                            backupdisplay2  TableUseTools26
                            goto    CheckKeypadToolsMenu
                    ToolsDisplay7
                            display TableUseTools27, B'11000000'
                            backupdisplay2  TableUseTools27
                            goto    CheckKeypadToolsMenu
                    ToolsDisplay8
                            display TableUseTools28, B'11000000'
                            backupdisplay2  TableUseTools28
                            goto    CheckKeypadToolsMenu
                    ToolsDisplay9
                            display TableUseTools29, B'11000000'
                            backupdisplay2  TableUseTools29
                            goto    CheckKeypadToolsMenu

                    CheckKeypadToolsMenu

                    ; Key C: Cancel
                    store   KeypadToCheck, b'00001011'     ; check for key C
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', MenuScreen_short9
                        bra     UseTools_Next1
                    MenuScreen_short9
                        goto    MenuScreen

                    UseTools_Next1

                    ; Key 1
                    store   KeypadToCheck, b'00000000'     ; check for key 1
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', PCInterface_short2
                        bra     UseTools_Next2
                    PCInterface_short2
                        goto    PCInterface

                    UseTools_Next2

                    ; Key 2
                    store   KeypadToCheck, b'00000001'     ; check for key 2
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', ReadLogInformation_short
                        bra     UseTools_Next3
                    ReadLogInformation_short
                        goto    ReadLogInformation

                    UseTools_Next3

                    ; Key 3
                    store   KeypadToCheck, b'00000010'     ; check for key 3
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', CalibrateTurntableScreen_short
                        bra     UseTools_Next4
                    CalibrateTurntableScreen_short
                        goto    CalibrateTurntableScreen

                    UseTools_Next4

                    ; Key 4
                    store   KeypadToCheck, b'00000100'     ; check for key 4
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', CalibrateDispenserScreen_short
                        bra     UseTools_Next5
                    CalibrateDispenserScreen_short
                        goto    CalibrateDispenserScreen

                    UseTools_Next5

                    ; Key 5
                    store   KeypadToCheck, b'00000101'     ; check for key 5
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', CalibrateCountingScreen_short
                        bra     UseTools_Next6
                    CalibrateCountingScreen_short
                        goto    CalibrateCountingScreen

                    UseTools_Next6

                    ; Key 6
                    store   KeypadToCheck, b'00000110'     ; check for key 6
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', TestVibratorScreen_short
                        bra     UseTools_Next7
                    TestVibratorScreen_short
                        goto    TestVibratorScreen

                    UseTools_Next7

                    ; Key 7
                    store   KeypadToCheck, b'00001000'     ; check for key 7
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', CalibrateClosingMech_short
                        bra     UseTools_Next8
                    CalibrateClosingMech_short
                        goto    CalibrateClosingMech

                    UseTools_Next8

                    ; Key 8
                    store   KeypadToCheck, b'00001001'     ; check for key 8
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', TestClosingScreen_short
                        bra     UseTools_Next9
                    TestClosingScreen_short
                        goto    TestClosingScreen

                    UseTools_Next9

                    ; Key 9
                    store   KeypadToCheck, b'00001010'     ; check for key 9
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', TestCountingScreen_short
                        bra     UseTools_Next10
                    TestCountingScreen_short
                        goto    TestCountingScreen

                    UseTools_Next10

                    ; Key B
                    store   KeypadToCheck, b'00000111'     ; check for key B
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', IncfToolsMenu_short
                        bra     UseTools_Next11
                    IncfToolsMenu_short
                        incf    ToolsMenuCount
                        ; check if it is larger than 9 for overscrolling
                        ifequal ToolsMenuCount, B'00001010', ResetToolsMenu
                                bra     UseTools_Next11
                                ResetToolsMenu
                                        store   ToolsMenuCount, d'0'

                    UseTools_Next11

                    ; Key A
                    store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', DecfToolsMenu_short
                        bra     UseTools_Next12
                    DecfToolsMenu_short
                        decf    ToolsMenuCount
                        ; if it is less than 0, set to 9
                        ifequal ToolsMenuCount, B'11111111', ResetToolsBackward
                                bra     UseTools_Next12
                                ResetToolsBackward
                                        store   ToolsMenuCount, B'00001001'

                    UseTools_Next12 

            goto    UseTools_Loop
                
        ; -------------------------------------------------------------------
        ; MODE 4-1: PC Interface
        ; -------------------------------------------------------------------

        PCInterface
                call    ClearLCD
                call    LogPCInterface

                display TablePCInterface1, B'10000000'
                backupdisplay1  TablePCInterface1
                display TablePCInterface2, B'11000000'
                backupdisplay2  TablePCInterface2

;        PCInterface_Loop
;
;                readEEPROM_REG  TemporaryVariable, TemporaryVariable2, WREG
;                    movff   WREG, TXREG
;                CheckTXDone
;                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
;                    bra     CheckTXDone      ; when done, TRMT = 1
;                addwf16     TemporaryVariable, TemporaryVariable2, 1
                ;bsf     TXSTA, TXEN                 ; enable transmission
                ;bsf     RCSTA, CREN                    ; enable receive CREN


            PCInterface_CheckLoop
                

                store   KeypadToCheck, b'00001011'     ; check for key C
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', UseTools_short2
                        bra     PCInterface_Next
                    UseTools_short2
                        goto    UseTools

                PCInterface_Next

                bcf     TXSTA, TXEN
                bsf     RCSTA, CREN

                btfss   PIR1, RCIF                     ; poll for data RCIF
                goto    PCInterface_CheckLoop         ; if not set, check key again

                ProcessRXData
                movff   RCREG, TemporaryVariable
                bcf     RCSTA, CREN
                bsf     TXSTA, TXEN
                ; check if x (ASCII 0x78)
                ifequal TemporaryVariable, 0x78, SendLog

                SendCharByChar
                        store   TemporaryVariable2, 0x00
                        readEEPROM_REG  TemporaryVariable2, TemporaryVariable, TXREG
                        CheckTXDone_CharByChar
                            btfss   TXSTA, TRMT             ; if TRMT = 1, skip out from loop
                            bra     CheckTXDone_CharByChar  ; when done, TRMT = 1
                        goto    EndOfProcessRXData
;
;                SendEEPROMAddrHigh
;                        movff   EEPROM_CurrentLocationH, TXREG
;                        CheckTXDone_EEPROMAddrHigh
;                            btfss   TXSTA, TRMT             ; if TRMT = 1, skip out from loop
;                            bra     CheckTXDone_EEPROMAddrHigh  ; when done, TRMT = 1
;                        goto    EndOfProcessRXData
;
;                SendEEPROMAddrLow
;                        movff   EEPROM_CurrentLocationL, TXREG
;                        CheckTXDone_EEPROMAddrLow
;                            btfss   TXSTA, TRMT             ; if TRMT = 1, skip out from loop
;                            bra     CheckTXDone_EEPROMAddrLow  ; when done, TRMT = 1
;                        goto    EndOfProcessRXData

                SendLog
                        store   TemporaryVariable, 0x00
                        store   TemporaryVariable2, 0x80

                        ; continuously send data until reaches the end
                        SendLog_Loop
                                readEEPROM_REG  TemporaryVariable, TemporaryVariable2, WREG
                                movff   WREG, TXREG
                                CheckTXDone_Log
                                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                                    bra     CheckTXDone_Log      ; when done, TRMT = 1
                                addwf16     TemporaryVariable, TemporaryVariable2, 1

                                ifnequal_reg    TemporaryVariable, EEPROM_CurrentLocationH, NoResetEEPROM
                                ifnequal_reg    TemporaryVariable2, EEPROM_CurrentLocationL, NoResetEEPROM

                                ResetEEPROM
                                            movlw   0x7A        ; send z to terminate
                                            movwf   TXREG
                                            CheckTXDone_EndChar
                                                btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                                                bra     CheckTXDone_EndChar      ; when done, TRMT = 1
                                            goto    EndOfProcessRXData
                                NoResetEEPROM
                                            goto    SendLog_Loop

                        goto    EndOfProcessRXData


                EndOfProcessRXData
        goto    PCInterface_CheckLoop

        ; -------------------------------------------------------------------
        ; MODE 4-2: Read Log
        ; -------------------------------------------------------------------

        ReadLogInformation
                call    ClearLCD
                call    LogReadLog

                store   TemporaryVariable, 0x00
                store   TemporaryVariable2, 0x80

                ; check keypad press
             ReadLogInfo_Loop_Keypad

               display TableReadLogInfo1, B'10000000'
               backupdisplay1    TableReadLogInfo1
               display TableReadLogInfo2, B'11000000'
               backupdisplay2    TableReadLogInfo2

                ; Key A: Exit
               store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', UseTools_short3
                        bra     ReadLogInfo_Next
                    UseTools_short3
                        goto    UseTools

                ReadLogInfo_Next

                ; Key B: Scroll Down
                store   KeypadToCheck, b'0000111'     ; check for key B
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', ReadLogInfo_Loop_Outer_short
                        bra     ReadLogInfo_Next2
                    ReadLogInfo_Loop_Outer_short
                        goto    ReadLogInfo_Loop_Outer ; simply read next log

                ReadLogInfo_Next2

            goto    ReadLogInfo_Loop_Keypad

            ReadLogInfo_Loop_Outer
        
                call    ClearLCD
                movlw   B'10000000'
                call    WriteInstToLCD

                ; initialize varaible to read only the first 15 characters
                ; which they are just time and date
                store   TemporaryVariable3, b'0'

                ReadLogInfo_Loop_Inner1      ; loop that reads date/time

                        ; check if overflow EEPROM last bit
                        ifnequal_reg    TemporaryVariable, EEPROM_CurrentLocationH, NoResetEEPROMaddr
                        ifnequal_reg    TemporaryVariable2, EEPROM_CurrentLocationL, NoResetEEPROMaddr

                        ResetEEPROMLogaddr
                                store   TemporaryVariable, 0x01
                                store   TemporaryVariable2, 0x00
                        NoResetEEPROMaddr

                        readEEPROM_REG  TemporaryVariable, TemporaryVariable2, WREG
                        call    WriteDataToLCD
                        addwf16 TemporaryVariable, TemporaryVariable2, 1
                        incf    TemporaryVariable3      ; increment TemporaryVariable

                        ; Check if temporaryvariable = 15
                        ifnequal    TemporaryVariable3, d'15', ReadLogInfo_Loop_Inner1
                        ; else, if equal to 15, display log information to 2nd line

                movlw   B'11000000'
                call    WriteInstToLCD

                ReadLogInfo_Loop_Inner2
                        readEEPROM_REG  TemporaryVariable, TemporaryVariable2, TemporaryVariable3
                        addwf16 TemporaryVariable, TemporaryVariable2, 1

                        ; before writing to LCD, check if it's a newline
                        ifequal     TemporaryVariable3, 0x0A,  EndLogSingleRead
                        ; if it's not equal to newline, 

                        ; Display log and keep looping
                        movff   TemporaryVariable3, WREG
                        call    WriteDataToLCD
                        bra     ReadLogInfo_Loop_Inner2

                ; if it's new line charater
                EndLogSingleRead
                ; after reading single log, check for user key!
                
                goto    ReadLogInfo_Loop_Keypad

           

        ; -------------------------------------------------------------------
        ; MODE 4-3: Calibrate Turntable
        ; -------------------------------------------------------------------

        CalibrateTurntableScreen
                call    ClearLCD
                call    LogCalibrateTurntable

                CalibTurntScreen_CheckKeypess

                    display TableCalibrateTurntable1, B'10000000'
                    backupdisplay1   TableCalibrateTurntable1
                    display TableCalibrateTurntable2, B'11000000'
                    backupdisplay2   TableCalibrateTurntable2

                    ; Key A: Start
                    store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', StartCalibrateTurntable

                    ; Key C: Cancel
                    store   KeypadToCheck, b'00001011'     ; check for key C
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', UseTools_short4
                        bra     CalibTurntScreen_CheckKeypess
                    UseTools_short4
                        goto    UseTools

                StartCalibrateTurntable

                    display TableRunning1, B'10000000'
                    backupdisplay1   TableRunning1
                    display TableRunning2, B'11000000'
                    backupdisplay2   TableRunning2

                    call    CalibrateTurntable

                    display TableCalibrateDone1, B'10000000'
                    backupdisplay1   TableCalibrateDone1
                    display TableCalibrateDone2, B'11000000'
                    backupdisplay2  TableCalibrateDone2
                    call    DelayMenu
                    goto    UseTools

        
        ; -------------------------------------------------------------------
        ; MODE 4-4: Calibrate Dispenser
        ; -------------------------------------------------------------------

        CalibrateDispenserScreen
                call    ClearLCD
                call    LogCalibrateDispenser


                CalibDispScreen_CheckKeypess

                    display TableCalibrateDispenser1, B'10000000'
                    backupdisplay1  TableCalibrateDispenser1
                    display TableCalibrateDispenser2, B'11000000'
                    backupdisplay2  TableCalibrateDispenser2

                    ; Key A: Start
                    store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', StartCalibrateDispenser

                    ; Key C: Cancel
                    store   KeypadToCheck, b'00001011'     ; check for key C
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', UseTools_short6
                        bra     CalibDispScreen_CheckKeypess
                    UseTools_short6
                        goto    UseTools

                StartCalibrateDispenser

                    display TableRunning1, B'10000000'
                    backupdisplay1  TableRunning1
                    display TableRunning2, B'11000000'
                    backupdisplay2  TableRunning2

                    call    CalibrateDispenser

                    display TableCalibrateDone1, B'10000000'
                    backupdisplay1  TableCalibrateDone1
                    display TableCalibrateDone2, B'11000000'
                    backupdisplay1  TableCalibrateDone2
                    call    DelayMenu
                    goto    UseTools

        ; -------------------------------------------------------------------
        ; MODE 4-5: Calibrate Counting Mechanism
        ; -------------------------------------------------------------------

        CalibrateCountingScreen

                call    ClearLCD
                call    LogCalibrateCounting
                display TableCalibrateCounting1, B'10000000'
                backupdisplay1  TableCalibrateCounting1
                display TableCalibrateCounting2, B'11000000'
                backupdisplay2  TableCalibrateCounting2

                call    DelayMenu
                display TableCalibrateCounting3, B'11000000'
                backupdisplay2  TableCalibrateCounting3

                CalibCountScreen_CheckKeypess

                    ; Key A: Amber
                    store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', StartCalibrateCountingA

                    ; Key B: Camel
                    store   KeypadToCheck, b'00000111'     ; check for key B
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', StartCalibrateCountingB

                    ; Key C: Cancel
                    store   KeypadToCheck, b'00001011'     ; check for key C
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', UseTools_short7
                        bra     CalibCountScreen_CheckKeypess
                    UseTools_short7
                        goto    UseTools

                StartCalibrateCountingA

                    display TableRunning1, B'10000000'
                    backupdisplay1  TableRunning1
                    display TableRunning2, B'11000000'
                    backupdisplay2  TableRunning2

                    call    CalibrateCountingAmber
                    goto    CalibrateCountingDone

                StartCalibrateCountingB

                    display TableRunning1, B'10000000'
                    backupdisplay1  TableRunning1
                    display TableRunning2, B'11000000'
                    backupdisplay2  TableRunning2

                    call    CalibrateCountingCamel
                    goto    CalibrateCountingDone

                CalibrateCountingDone

                display TableCalibrateDone1, B'10000000'
                backupdisplay1  TableCalibrateDone1
                display TableCalibrateDone2, B'11000000'
                backupdisplay2  TableCalibrateDone2
                call    DelayMenu
                goto    UseTools

        ; -------------------------------------------------------------------
        ; MODE 4-6: Test vibrator
        ; -------------------------------------------------------------------

        TestVibratorScreen

                call    ClearLCD
                call    LogTestVibrator

                TestVibratorScreen_CheckKeypess

                    display TableTestVibrator1, B'10000000'
                    backupdisplay1  TableTestVibrator1
                    display TableTestVibrator2, B'11000000'
                    backupdisplay2  TableTestVibrator2

                    ; Key A: Start
                    store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', StartTestVibrator

                    ; Key C: Cancel
                    store   KeypadToCheck, b'00001011'     ; check for key C
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', UseTools_shortA
                        bra     TestVibratorScreen_CheckKeypess
                    UseTools_shortA
                        goto    UseTools

                StartTestVibrator
                    display TableRunning1, B'10000000'
                    backupdisplay1  TableRunning1
                    display TableRunning2, B'11000000'
                    backupdisplay2  TableRunning2
                    call    TurnOnVibrator
                    display TableTestDone1, B'10000000'
                    backupdisplay1  TableTestDone1
                    display TableTestDone2, B'11000000'
                    backupdisplay2  TableTestDone2
                    call    DelayMenu
                    goto    UseTools



        ; -------------------------------------------------------------------
        ; MODE 4-7: Calibrate Closing Mech
        ; -------------------------------------------------------------------

        CalibrateClosingMech

                call    ClearLCD
                call    LogTestTurntable
                display TableTestTurntable1, B'10000000'
                backupdisplay1  TableTestTurntable1
                display TableTestTurntable21, B'11000000'
                backupdisplay2  TableTestTurntable21

                call    DelayMenu
                display TableTestTurntable22, B'11000000'
                backupdisplay2  TableTestTurntable22

                call    DelayMenu

                CalibClosingMech_CheckKeypess

                    display TableTestTurntable1, B'10000000'
                    backupdisplay1  TableTestTurntable1
                    display TableTestTurntable23, B'11000000'
                    backupdisplay2  TableTestTurntable23

                    ; Key A: Start
                    store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', StartCalibClosing

                    ; Key C: Cancel
                    store   KeypadToCheck, b'00001011'     ; check for key C
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', UseTools_short5
                        bra     CalibClosingMech_CheckKeypess
                    UseTools_short5
                        goto    UseTools

                StartCalibClosing

                    display TableRunning1, B'10000000'
                    backupdisplay1  TableRunning1
                    display TableRunning2, B'11000000'
                    backupdisplay2  TableRunning2

                    call    CalibrateClosingMechanism

                    display TableTestDone1, B'10000000'
                    backupdisplay1  TableTestDone1
                    display TableTestDone2, B'11000000'
                    backupdisplay2  TableTestDone2
                    call    DelayMenu
                    goto    UseTools

        ; -------------------------------------------------------------------
        ; MODE 4-8: Test Closing
        ; -------------------------------------------------------------------

        TestClosingScreen

                call    ClearLCD
                call    LogTestDispenser

                display TableTestClosing1, B'10000000'
                backupdisplay1  TableTestClosing1
                display TableTestClosing2, B'11000000'
                backupdisplay2  TableTestClosing2

                call    ShutContainer

;                display TableTestDispenser1, B'10000000'
;                display TableTestDispenser21, B'11000000'
;
;                call    DelayMenu
;                display TableTestDispenser22, B'11000000'
;
;                call    DelayMenu
;                display TableTestDispenser23, B'11000000'
;
;                TestDispenserScreen_CheckKeypess
;
;                    display TableTestDispenser1, B'10000000'
;
;                    ; Key A: Start
;                    store   KeypadToCheck, b'00000011'     ; check for key A
;                    call    CheckForKeypress
;                    ifequal KeypadCheckResult, d'1', StartTestDispenser_Amber
;
;                    ; Key B: Start
;                    store   KeypadToCheck, b'00000111'     ; check for key A
;                    call    CheckForKeypress
;                    ifequal KeypadCheckResult, d'1', StartTestDispenser_Camel
;
;                    ; Key C: Cancel
;                    store   KeypadToCheck, b'00001011'     ; check for key C
;                    call    CheckForKeypress
;                    ifequal KeypadCheckResult, d'1', UseTools_short7
;                        bra     TestDispenserScreen_CheckKeypess
;                    UseTools_short7
;                        goto    UseTools
;
;                StartTestDispenser_Amber
;
;                    display TableRunning1, B'10000000'
;                    display TableRunning2, B'11000000'
;
;                    call    DispenseAmberChip
;                    bra     EndOfTestDispenser
;
;                StartTestDispenser_Camel
;
;                    display TableRunning1, B'10000000'
;                    display TableRunning2, B'11000000'
;
;                    call    DispenseCamelChip
;                    bra     EndOfTestDispenser
;
;                EndOfTestDispenser

                    display TableTestDone1, B'10000000'
                    backupdisplay1  TableTestDone1
                    display TableTestDone2, B'11000000'
                    backupdisplay2  TableTestDone2
                    call    DelayMenu
                    goto    UseTools

        ; -------------------------------------------------------------------
        ; MODE 4-9: Test Counting
        ; -------------------------------------------------------------------

        TestCountingScreen

                call    ClearLCD
                call    LogTestCounting

                TestCountingScreen_CheckKeypess

                    display TableTestCounting1, B'10000000'
                    backupdisplay1  TableTestCounting1
                    display TableTestCounting2, B'11000000'
                    backupdisplay2  TableTestCounting2

                    ; Key A: Start
                    store   KeypadToCheck, b'00000011'     ; check for key A
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', StartTestCounting

                    ; Key C: Cancel
                    store   KeypadToCheck, b'00001011'     ; check for key C
                    call    CheckForKeypress
                    ifequal KeypadCheckResult, d'1', UseTools_short8
                        bra     TestCountingScreen_CheckKeypess
                    UseTools_short8
                        goto    UseTools

                StartTestCounting
                    display TableRunning1, B'10000000'
                    backupdisplay1  TableRunning1
                    display TableRunning2, B'11000000'
                    backupdisplay2  TableRunning2

                    call    CountingChips

                    display TableTestDone1, B'10000000'
                    backupdisplay1  TableTestDone1
                    display TableTestDoneCounting, B'11000000'
                    backupdisplay2  TableTestDoneCounting

                    movlw   B'11000010'
                    call    WriteInstToLCD

                    ; display remaining chips
                    ; check if larger than 10 first
                    movlw   d'10'
                    subwf   ChipCountAmber, 0
                            ; ChipCountAmber - 10 = WREG
                    bn      AmberLessThan10_Test

                    AmberMoreThan10_Test
                        movlw   '1'
                        call    WriteDataToLCD
                        movlw   d'10'
                        subwf   ChipCountAmber, 0   ; -10
                        bra     AmberNextDigit_Test
                    AmberLessThan10_Test
                        movlw   '0'
                        call    WriteDataToLCD
                        movff   ChipCountAmber, WREG
                        bra     AmberNextDigit_Test

                    AmberNextDigit_Test
                        call    ConvertDecimalToASCII
                        call    WriteDataToLCD
                    

                    movlw   B'11001011'
                    call    WriteInstToLCD

                    ; display remaining chips
                    ; check if larger than 10 first
                    movlw   d'10'
                    subwf   ChipCountCamel, 0
                            ; ChipCountAmber - 10 = WREG
                    bn      CamelLessThan10_Test

                    CamelMoreThan10_Test
                        movlw   '1'
                        call    WriteDataToLCD
                        movlw   d'10'
                        subwf   ChipCountCamel, 0   ; -10
                        bra     CamelNextDigit_Test
                    CamelLessThan10_Test
                        movlw   '0'
                        call    WriteDataToLCD
                        movff   ChipCountCamel, WREG
                        bra     CamelNextDigit_Test

                    CamelNextDigit_Test
                        call    ConvertDecimalToASCII
                        call    WriteDataToLCD
                    
                    call    DelayMenu
                    call    DelayMenu
                    call    DelayMenu
                    goto    UseTools

        ; -------------------------------------------------------------------
        ; MODE 5: Dispense Screen
        ; -------------------------------------------------------------------

        DispenseScreen

                call    ClearLCD
                call    ReadRTC
                store   ClosingCount1True, 0x00
                store   ClosingCount2True, 0x00

                ; store old data to log time elapse
                movff   RTC_Second, RTC_Second_Old
                movff   RTC_Minute, RTC_Minute_Old
                movff   RTC_Hour, RTC_Hour_Old

                call    LogDispenseStarted

                ; read turntable closing position from EEPROM
                store   TemporaryVariable, 0x00
                store   TemporaryVariable2, 0x0A
                readEEPROM_REG  TemporaryVariable, TemporaryVariable2, ClosingPositionFromEEPROM

                ; always assume it is a success, will be changed to fail
                ; if failure occurs
                store   DispenseSuccessOrFail, d'1'
                store   DispenseContainerCount, d'0'

                display TableDispense1, B'10000000'
                backupdisplay1  TableDispense1
                display TableDispense2, B'11000000'
                backupdisplay2  TableDispense2

                call    CountingChips

                ; check if chips are sufficient
                CheckAmberSufficient
                        movff   NumberOfAmberRequested, WREG
                        subwf   ChipCountAmber, 0
                        bn      WarnInsufficientAmberChips
                CheckCamelSufficient
                        movff   NumberOfCamelRequested, WREG
                        subwf   ChipCountCamel, 0
                        bn      WarnInsufficientCamelChips

                ProceedToDispensing
                call    StepTurntable

                call    DelayMenu
                goto    DisplayScreen

                WarnInsufficientAmberChips
                        display TableWarningInsuffAmber1, B'10000000'
                        backupdisplay1  TableWarningInsuffAmber1
                        display TableWarningInsuffAmber2, B'11000000'
                        backupdisplay2  TableWarningInsuffAmber2    
                        call    DelayMenu
                        display TableWarningInsuffAmber3, B'10000000'
                        backupdisplay1  TableWarningInsuffAmber3                        
                        display TableWarningInsuffAmber4, B'11000000'
                        backupdisplay2  TableWarningInsuffAmber4
                        goto    InsuffChipCheckResponse 
                        
               WarnInsufficientCamelChips
                        display TableWarningInsuffCamel1, B'10000000'
                        backupdisplay1  TableWarningInsuffCamel1                        
                        display TableWarningInsuffCamel2, B'11000000'
                        backupdisplay2  TableWarningInsuffCamel2    
                        call    DelayMenu
                        display TableWarningInsuffCamel3, B'10000000'
                        backupdisplay1  TableWarningInsuffCamel3                        
                        display TableWarningInsuffCamel4, B'11000000'
                        backupdisplay2  TableWarningInsuffCamel4
                        goto    InsuffChipCheckResponse

               InsuffChipCheckResponse
                         ; Key A: Continue
                        store   KeypadToCheck, b'00000011'     ; check for key A
                        call    CheckForKeypress
                        ifequal KeypadCheckResult, d'1', ProceedToDispensing_short

                        ; Key C: Cancel
                        store   KeypadToCheck, b'00001011'     ; check for key C
                        call    CheckForKeypress
                        ifequal KeypadCheckResult, d'1', CancelDispense_short
                            bra     InsuffChipCheckResponse
                        ProceedToDispensing_short
                            goto    ProceedToDispensing
                        CancelDispense_short
                            goto    MenuScreen

        ; -------------------------------------------------------------------
        ; MODE 6: Display Information After Dispense
        ; -------------------------------------------------------------------

        DisplayScreen

                call    ClearLCD

                call    ReadRTC
                ; calculate time difference
                call    CalculateTimeDiff
                call    LogDispenseEnd

                ; transfer record backward #2 to #3
                readEEPROM      0x00, 0x51, TemporaryVariable
                writeEEPROM_REG 0x00, 0x61, TemporaryVariable
                readEEPROM      0x00, 0x52, TemporaryVariable
                writeEEPROM_REG 0x00, 0x62, TemporaryVariable
                readEEPROM      0x00, 0x53, TemporaryVariable
                writeEEPROM_REG 0x00, 0x63, TemporaryVariable
                readEEPROM      0x00, 0x54, TemporaryVariable
                writeEEPROM_REG 0x00, 0x64, TemporaryVariable
                readEEPROM      0x00, 0x55, TemporaryVariable
                writeEEPROM_REG 0x00, 0x65, TemporaryVariable
                readEEPROM      0x00, 0x56, TemporaryVariable
                writeEEPROM_REG 0x00, 0x66, TemporaryVariable
                readEEPROM      0x00, 0x57, TemporaryVariable
                writeEEPROM_REG 0x00, 0x67, TemporaryVariable
                readEEPROM      0x00, 0x58, TemporaryVariable
                writeEEPROM_REG 0x00, 0x68, TemporaryVariable
                readEEPROM      0x00, 0x59, TemporaryVariable
                writeEEPROM_REG 0x00, 0x69, TemporaryVariable
                readEEPROM      0x00, 0x5A, TemporaryVariable
                writeEEPROM_REG 0x00, 0x6A, TemporaryVariable
                readEEPROM      0x00, 0x5B, TemporaryVariable
                writeEEPROM_REG 0x00, 0x6B, TemporaryVariable
                readEEPROM      0x00, 0x5C, TemporaryVariable
                writeEEPROM_REG 0x00, 0x6C, TemporaryVariable
                readEEPROM      0x00, 0x5D, TemporaryVariable
                writeEEPROM_REG 0x00, 0x6D, TemporaryVariable
                readEEPROM      0x00, 0x5E, TemporaryVariable
                writeEEPROM_REG 0x00, 0x6E, TemporaryVariable

                ; transfer record backward #1 to #2
                readEEPROM      0x00, 0x41, TemporaryVariable
                writeEEPROM_REG 0x00, 0x51, TemporaryVariable
                readEEPROM      0x00, 0x42, TemporaryVariable
                writeEEPROM_REG 0x00, 0x52, TemporaryVariable
                readEEPROM      0x00, 0x43, TemporaryVariable
                writeEEPROM_REG 0x00, 0x53, TemporaryVariable
                readEEPROM      0x00, 0x44, TemporaryVariable
                writeEEPROM_REG 0x00, 0x54, TemporaryVariable
                readEEPROM      0x00, 0x45, TemporaryVariable
                writeEEPROM_REG 0x00, 0x55, TemporaryVariable
                readEEPROM      0x00, 0x46, TemporaryVariable
                writeEEPROM_REG 0x00, 0x56, TemporaryVariable
                readEEPROM      0x00, 0x47, TemporaryVariable
                writeEEPROM_REG 0x00, 0x57, TemporaryVariable
                readEEPROM      0x00, 0x48, TemporaryVariable
                writeEEPROM_REG 0x00, 0x58, TemporaryVariable
                readEEPROM      0x00, 0x49, TemporaryVariable
                writeEEPROM_REG 0x00, 0x59, TemporaryVariable
                readEEPROM      0x00, 0x4A, TemporaryVariable
                writeEEPROM_REG 0x00, 0x5A, TemporaryVariable
                readEEPROM      0x00, 0x4B, TemporaryVariable
                writeEEPROM_REG 0x00, 0x5B, TemporaryVariable
                readEEPROM      0x00, 0x4C, TemporaryVariable
                writeEEPROM_REG 0x00, 0x5C, TemporaryVariable
                readEEPROM      0x00, 0x4D, TemporaryVariable
                writeEEPROM_REG 0x00, 0x5D, TemporaryVariable
                readEEPROM      0x00, 0x4E, TemporaryVariable
                writeEEPROM_REG 0x00, 0x5E, TemporaryVariable

                ; now write new data into the new #1 location
                writeEEPROM_REG 0x00, 0x41, RTC_Year
                writeEEPROM_REG 0x00, 0x42, RTC_Month
                writeEEPROM_REG 0x00, 0x43, RTC_Date
                writeEEPROM_REG 0x00, 0x44, RTC_Hour_Old
                writeEEPROM_REG 0x00, 0x45, RTC_Minute_Old
                writeEEPROM_REG 0x00, 0x46, RTC_Second_Old
                writeEEPROM_REG 0x00, 0x47, UserSelectPattern1
                writeEEPROM_REG 0x00, 0x48, UserSelectPattern2
                writeEEPROM_REG 0x00, 0x49, ChipCountAmber
                writeEEPROM_REG 0x00, 0x4A, ChipCountCamel
                movff   RTC_Minute_Diff, WREG
                call    ConvertRTC
                movlw   0x0F
                andwf   RTC_H, 1
                movlw   0x0F
                andwf   RTC_L, 1
                writeEEPROM_REG 0x00, 0x4B, RTC_H
                writeEEPROM_REG 0x00, 0x4C, RTC_L
                movff   RTC_Second_Diff, WREG
                call    ConvertRTC
                movlw   0x0F
                andwf   RTC_H, 1
                movlw   0x0F
                andwf   RTC_L, 1
                writeEEPROM_REG 0x00, 0x4D, RTC_H
                writeEEPROM_REG 0x00, 0x4E, RTC_L

                display TableDisplay11, B'10000000'
                backupdisplay1  TableDisplay11
                display TableDisplay2,  B'11000000'
                backupdisplay2  TableDisplay2

        DisplayScreen_Loop

                display TableDisplay11, B'10000000'
                backupdisplay1  TableDisplay11
                display TableDisplay2,  B'11000000'
                backupdisplay2  TableDisplay2
                call    DelayMenu

                    call    CheckAnyKeypress
                    ifequal KeypadCheckResult, d'1', MenuScreen_short1
                        bra     DisplayScreen_Next1
                    MenuScreen_short1
                        goto    MenuScreen
                    DisplayScreen_Next1

                display TableDisplay13, B'10000000'
                backupdisplay1  TableDisplay13
                display TableDisplay2,  B'11000000'
                backupdisplay2  TableDisplay2
                    movlw   B'10001010'         ; start writing at position 10
                    call    WriteInstToLCD
                    ; take the minute difference and convert them into ascii
                    movff   RTC_Minute_Diff, WREG
                    call    ConvertRTC
                    movff   RTC_H, WREG
                    call    WriteDataToLCD
                    movff   RTC_L, WREG
                    call    WriteDataToLCD
                    movlw   'm'
                    call    WriteDataToLCD
                    movff   RTC_Second_Diff, WREG
                    call    ConvertRTC
                    movff   RTC_H, WREG
                    call    WriteDataToLCD
                    movff   RTC_L, WREG
                    call    WriteDataToLCD
                    movlw   's'
                    call    DelayMenu

                    call    CheckAnyKeypress
                    ifequal KeypadCheckResult, d'1', MenuScreen_short3
                        bra     DisplayScreen_Next3
                    MenuScreen_short3
                        goto    MenuScreen
                    DisplayScreen_Next3

                ; Pattern dispensed
                display TableDisplay14, B'10000000'
                backupdisplay1  TableDisplay14
                    movlw   B'10001001'         ; start writing at position 9
                    call    WriteInstToLCD
                    ; display user selected pattern
                    movff   UserSelectPattern1, WREG
                    call    ConvertDecimalToASCII
                    call    WriteDataToLCD
                    movlw   ','
                    call    WriteDataToLCD
                    movff   UserSelectPattern2, WREG
                    call    ConvertDecimalToASCII
                    call    WriteDataToLCD
                call    DelayMenu

                    call    CheckAnyKeypress
                    ifequal KeypadCheckResult, d'1', MenuScreen_short4
                        bra     DisplayScreen_Next4
                    MenuScreen_short4
                        goto    MenuScreen
                    DisplayScreen_Next4

                display TableDisplay15, B'10000000'
                backupdisplay1  TableDisplay15
                display TableDisplay2,  B'11000000'
                backupdisplay2  TableDisplay2
                    movlw   B'10001100'         ; start writing at position 13
                    call    WriteInstToLCD
                    ; display remaining chips
                    ; check if larger than 10 first
                    movlw   d'10'
                    subwf   ChipCountAmber, 0
                            ; ChipCountAmber - 10 = WREG
                    bn      AmberLessThan10

                    AmberMoreThan10
                        movlw   '1'
                        call    WriteDataToLCD
                        movlw   d'10'
                        subwf   ChipCountAmber, 0   ; -10
                        bra     AmberNextDigit
                    AmberLessThan10
                        movlw   '0'
                        call    WriteDataToLCD
                        movff   ChipCountAmber, WREG
                        bra     AmberNextDigit

                    AmberNextDigit
                        call    ConvertDecimalToASCII
                        call    WriteDataToLCD
                call    DelayMenu

                    call    CheckAnyKeypress
                    ifequal KeypadCheckResult, d'1', MenuScreen_short5
                        bra     DisplayScreen_Next5
                    MenuScreen_short5
                        goto    MenuScreen
                    DisplayScreen_Next5

                display TableDisplay16, B'10000000'
                backupdisplay1  TableDisplay16
                movlw   B'10001100'         ; start writing at position 13
                    call    WriteInstToLCD
                    ; display remaining chips
                    ; check if larger than 10 first
                    movlw   d'10'
                    subwf   ChipCountCamel, 0
                            ; ChipCountAmber - 10 = WREG
                    bn      CamelLessThan10

                    CamelMoreThan10
                        movlw   '1'
                        call    WriteDataToLCD
                        movlw   d'10'
                        subwf   ChipCountCamel, 0   ; -10
                        bra     CamelNextDigit
                    CamelLessThan10
                        movlw   '0'
                        call    WriteDataToLCD
                        movff   ChipCountCamel, WREG
                        bra     CamelNextDigit

                    CamelNextDigit
                        call    ConvertDecimalToASCII
                        call    WriteDataToLCD
                call    DelayMenu

                    call    CheckAnyKeypress
                    ifequal KeypadCheckResult, d'1', MenuScreen_short6
                        bra     DisplayScreen_Next6
                    MenuScreen_short6
                        goto    MenuScreen
                    DisplayScreen_Next6

        goto    DisplayScreen_Loop

        ; ------------------------------------------------------------------

    goto        MainLoop

; ============================================================================
; Interrupt Service Routine
; ============================================================================
Interrupt
    CheckForEStop
        bcf     INTCON, INT0IF              ; clear interrupt flag
            ; at this point E-Stop is armed
        call        LogEStop
        display     TableEStopArmed1, B'10000000'
        display     TableEStopArmed2, B'11000000'
        call    CheckForEmergencyStop
        bra     EndInterrupt

    EndInterrupt
    retfie


; ============================================================================
; Subroutine calls
; ============================================================================

; ---------------------------------------------------------------------------
; General Purposes
; ---------------------------------------------------------------------------

TurnOnVibrator

    store   VibratorCountDown, 0xFF
    VibratorDown
        call    StepCountingDown
        decfsz  VibratorCountDown, 1
        bra     VibratorDown

    call    Delay5ms

    store   VibratorCountUp, 0xFF
    VibratorUp
        call    StepCountingUp
        decfsz  VibratorCountUp, 1
        bra     VibratorUp

    return

CheckForEmergencyStop
CheckForEmergencyStop_Loop
    bcf         INTCON, GIE                 ; disable interrupt
    movff       PORTB, PortB_data
    btfsc       PortB_data, 0               ; check if B0 is high
    goto        CheckForEmergencyStop_Loop          ; if not, end

    call        Delay5ms                    ; check for debouncing
    movff       PORTB, PortB_data
    btfsc       PortB_data, 0               ; check if B0 is high
    goto        CheckForEmergencyStop_Loop          ; if not, end


    ; at this point E-Stop has been diarmed
    display     TableEStopDisarmed1, B'10000000'
    display     TableEStopDisarmed2, B'11000000'
    call        LogEStopDeactivated
    call        DelayHalfSec
    call        ClearLCD

    movlw   B'10000000'                   ; restore line 1
    call    WriteInstToLCD
    movff   TableLastLine1U, TBLPTRU
    movff   TableLastLine1H, TBLPTRH
    movff   TableLastLine1L, TBLPTRL
    ; Do 16 Loops to display all 16 characters in the TableName
    call    Write16DataToLCD

    movlw   B'11000000'                   ; restore line 2
    call    WriteInstToLCD
    movff   TableLastLine2U, TBLPTRU
    movff   TableLastLine2H, TBLPTRH
    movff   TableLastLine2L, TBLPTRL
    ; Do 16 Loops to display all 16 characters in the TableName
    call    Write16DataToLCD

    EndOfEStopChecking
            bsf         INTCON, GIE         ; enable interrupt
    return

ConvertDecimalToASCII                       ; input WREG, output WREG
    andlw       0x0F                        ; mask to have lower nibble only
    addlw       0x30                        ; add 0011 byte in front
    return

; Divide function
; Input: DivideDividendH, DivideDividendL, DivideDivisor
; Output: DivideQuotient
Divide
    store DivideQuotient, d'0' ; initialize    
SubtractLoop
    movff   DivideDivisor, WREG
    ; substract 16 bit
    bcf     STATUS, 0                   ; clear carry flag
    subwf   DivideDividendL, 1          ; normal substraction, if there is a
                                        ; borrow, the C = 0
    movlw   d'0'
    subwfb  DivideDividendH, 1          ; bit8L = bit8L - 0 - !C

    bn      StopDivide

    ; if it's not negative, then increment DivideQuotient
    incf    DivideQuotient, 1           ; store into itself
    bra     SubtractLoop                ; keep dividing
StopDivide
    movff   DivideDivisor, WREG         ; move it back to WREG
    addwf   DivideDividendL, 1          ; add it back to dividend to show the
                                        ; remainder of the division
    ; if the remainder is larger than half of the divisor, then round up
    ; and increment the quotient

    ; rotate the divisor to the right to divide by 2
    rrncf       DivideDivisor, 1
    ; Divisor/2 - Remainder
    ; if negative, then Quotient is more than half, means we should increment
    movff       DivideDividendL, WREG
    subwf       DivideDivisor, 0        ; store result in wreg
    bn          RoundUpRemainder
    RoundDownRemainder
            bra     EndDivide           ; don't need to increment
    RoundUpRemainder
            incf    DivideQuotient      ; increment to round up
    EndDivide
    return

StepTurntable
    store       TurntableTotalStepsH, d'0'
    store       TurntableTotalStepsL, d'0'
    store       TurntableTargetH, d'0'
    store       TurntableTargetL, d'0'
    store       TurntableTargetIndexH, d'0'
    store       TurntableTargetIndexL, d'0'

    ; read from EEPROM for calibration data, using TurntableTargetIndexH,L
    ; use TurntableTotalStepsH,L to keep track of number os steps
    ; keep checking TurntableTotalSteps to see if matches with TurntableTargetIndex
    ; if not, keep stepping
    ; if yes, check reed switch
    ; if no reed, step forward until detect reed switch
        ; keep checking reed switch when stepping forward
        ; if yes, keep stepping forward until there is no reed switch, user
        ;         DispenserTargetIndexL to store width
                ; check if reed switch is no longer there
                ; if no reed, divide DispenserTargetIndexL by 2 and then
                ;             step backward by the same amount to center it
                ; if still got reed, keep stepping
        ; if no, keep stepping forward
    ; if yes, read EEPROM for next target location
    ; call dispense (check if DispenserTargetL = 2, 4)
    ; then check if DispenserTargetL = 8
    ; if not equal to 8 then LOOP, if 8 then we are done.

            KeepRotatingTurntable

                GoToNextContainer
                    ifequal         DispenseContainerCount, d'0', SkipFixOffsideContainer
                    ifequal         DispenseContainerCount, d'3', SkipFixOffsideContainer
                    call            FixOffsideTurntable
                    bra             StartDispenseFromTurntable
                        
                    SkipFixOffsideContainer
                    call            DontFixOffsideTurntable

                    StartDispenseFromTurntable
                            ifequal         DispenseContainerCount, d'0', SkipFirstContainer
                            ifequal         DispenseContainerCount, d'1', CallDispenseChips
                            ifequal         DispenseContainerCount, d'2', CallDispenseChips
                            ifequal         DispenseContainerCount, d'3', EndDispenseChips
                            ;   if not 2 nor 4, then keep rotating turntable
                            goto            KeepRotatingTurntable

                            CallDispenseChips
                                    call            DispenseChips

                                    call        DelayMenu
                                    ; delay by 1s to fix the issue where the turntable
                                    ; turns away before the chip actually falls into
                                    ; the reservoir

                            SkipFirstContainer
                                    ; step it away from the current container
                                    ; so the program can proceed to process the
                                    ; second container
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise

                                    incf            DispenseContainerCount

                                    goto            KeepRotatingTurntable

                            EndDispenseChips
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
                                    call        StepTurntableClockwise
    return


CalibrateCountingAmber


    call    MoveCounterToTop

    store   ChipCountHeightAmberH, 0x00
    store   ChipCountHeightAmberL, 0x00

    ; LEFT side first

    call    CountCountingStepsAmber

    ; 15 chips
    store   TemporaryVariable3, 0x00
    store   TemporaryVariable4, 0x10
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 14 chips
    call    DispenseAmberChip
    call    CountCountingStepsAmber
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 13 chips
    call    DispenseAmberChip
    call    CountCountingStepsAmber
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 12 chips
    call    DispenseAmberChip
    call    CountCountingStepsAmber
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 11 chips
    call    DispenseAmberChip
    call    CountCountingStepsAmber
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 10 chips
    call    DispenseAmberChip
    call    CountCountingStepsAmber
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 9 chips
    call    DispenseAmberChip
    call    CountCountingStepsAmber
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 8 chips
    call    DispenseAmberChip
    call    CountCountingStepsAmber
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 7 chips
    call    DispenseAmberChip
    call    CountCountingStepsAmber
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 6 chips
    call    DispenseAmberChip
    call    CountCountingStepsAmber
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightAmberL

    ; 5 chips
    call    DispenseAmberChip
    call    Delay5ms
    ; 4 chips
    call    DispenseAmberChip
    call    Delay5ms
    ; 3 chips
    call    DispenseAmberChip
    call    Delay5ms
    ; 2 chips
    call    DispenseAmberChip
    call    Delay5ms
    ; 1 chips
    call    DispenseAmberChip
    call    Delay5ms
    ; 0 chips
    call    DispenseAmberChip
    call    Delay5ms

    call    MoveCounterToTop

    return


CalibrateCountingCamel

    call    MoveCounterToTop
    ; RIGHT side next

    store   ChipCountHeightCamelH, 0x00
    store   ChipCountHeightCamelL, 0x00

    call    CountCountingStepsCamel

    ; 15 chips
    store   TemporaryVariable3, 0x00
    store   TemporaryVariable4, 0x24
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 14 chips
    call    DispenseCamelChip
    call    CountCountingStepsCamel
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 13 chips
    call    DispenseCamelChip
    call    CountCountingStepsCamel
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 12 chips
    call    DispenseCamelChip
    call    CountCountingStepsCamel
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 11 chips
    call    DispenseCamelChip
    call    CountCountingStepsCamel
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 10 chips
    call    DispenseCamelChip
    call    CountCountingStepsCamel
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 9 chips
    call    DispenseCamelChip
    call    CountCountingStepsCamel
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 8 chips
    call    DispenseCamelChip
    call    CountCountingStepsCamel
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 7 chips
    call    DispenseCamelChip
    call    CountCountingStepsCamel
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 6 chips
    call    DispenseCamelChip
    call    CountCountingStepsCamel
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelH
    incf    TemporaryVariable4
    writeEEPROM TemporaryVariable3, TemporaryVariable4, ChipCountHeightCamelL

    ; 5 chips
    call    DispenseCamelChip
    call    Delay5ms
    ; 4 chips
    call    DispenseCamelChip
    call    Delay5ms
    ; 3 chips
    call    DispenseCamelChip
    call    Delay5ms
    ; 2 chips
    call    DispenseCamelChip
    call    Delay5ms
    ; 1 chips
    call    DispenseCamelChip
    call    Delay5ms
    ; 0 chips
    call    DispenseCamelChip
    call    Delay5ms

    call    MoveCounterToTop

    return

MoveCounterToTop

    ; start moving upward
    MoveCounterToTop_Loop

        ; increment stepper pattern
        call    StepCountingUp

            ; check if top microswitch is pressed
            btfss   PORTA, 3
            bra     MoveCounterToTop_Loop          ; if pressed, A3 = 1

             ; debouncing
             call    Delay44us
             btfss   PORTA, 3
             bra     MoveCounterToTop_Loop          ; if pressed, A3 = 1

    return

CountCountingStepsAmber

    KeepCountingAmber

        call    StepCountingDown
        addwf16 ChipCountHeightAmberH, ChipCountHeightAmberL, d'1'

        btfss   PORTE, 0
        ; if PORTE0 is 1, then everything is OK else keep stepping
        bra     KeepCountingAmber

        ; debouncing
         call    Delay44us
         btfss   PORTE, 0
         bra     KeepCountingAmber

    return

CountCountingStepsCamel

    KeepCountingCamel

        call    StepCountingDown
        addwf16 ChipCountHeightCamelH, ChipCountHeightCamelL, d'1'

        btfss   PORTE, 1
        ; if PORTE0 is 1, then everything is OK else keep stepping
        bra     KeepCountingCamel

        ; debouncing
         call    Delay44us
         btfss   PORTE, 1
         bra     KeepCountingCamel

    return

DontFixOffsideTurntable
    DontFixOffsideTurntable_Loop
        call    StepTurntableClockwise
        btfss   PORTB, 2
        ; if PORTB2 is 1, then everything is OK else keep stepping
        bra     DontFixOffsideTurntable_Loop

        ; debouncing
         call    Delay44us
         btfss   PORTB, 2
        bra     DontFixOffsideTurntable_Loop
    return

FixOffsideTurntable

    FixOffsideTurntable_Loop
        call    StepTurntableClockwise
        btfss   PORTB, 2
        ; if PORTB2 is 1, then everything is OK else keep stepping
        bra     FixOffsideTurntable_Loop

        ; debouncing
         call    Delay44us
         btfss   PORTB, 2
         bra     FixOffsideTurntable_Loop

        ; call once to fix to the midpoint

; remove reversal of stepper due to slipping

    store   TemporaryVariable5, d'0'
    ; here, the PORTB2 reed switch is 1
    ; use TemporaryVariable5 to calculate width
    CalculateWidthOfMagnet
            call    StepTurntableClockwise
            incf    TemporaryVariable5,1
            btfsc   PORTB, 2
           ; if PORTB is 1, keep stepping!
            bra     CalculateWidthOfMagnet

            ; debouncing
             call    Delay44us
             btfsc   PORTB, 2
             bra     CalculateWidthOfMagnet

            ; here, we have calculated the width
            ; divide the width by 2 to take center
            ; by rotating right
            bcf     STATUS, 0           ; clear C flag
            rrcf    TemporaryVariable5    ; right rotate

            call    Delay5ms

            StepTurntableBackToCenter
                    call    StepTurntableCounterClockwise
                    decfsz  TemporaryVariable5
                   ; if not zero, then keep doing
                    bra     StepTurntableBackToCenter
                   ; here we already have it centered

    return

CalibrateClosingMechanism

    store       TurntableStepEEPROMIndexH, 0x00    ; for placeholder purpose only
    store       TurntableStepEEPROMIndexL, 0x0A    ; used to calculate number of 90 deg turns we have made

    ; algorithm: quickly step to the middle of the tab on the turntable,
    ; then from there, start moving extremely slow while counting the number
    ; of steps. Once the container moves to the desired location for capping,
    ; the user will have to press A, and then the location will be stored into
    ; the EEPROM

    KeepTurningTurnable_CM

    ; fix if offside
    call        FixOffsideTurntable
    store       TurntableTotalStepsL, d'0'

    StepAndCheckKeypress
            call    StepTurntableClockwise_SLOW
            incf    TurntableTotalStepsL

            ; check for keypress
            store   KeypadToCheck, b'00000011'     ; check for key A
            call    CheckForKeypress
            ifequal KeypadCheckResult, d'1', StoreClosingMechIntoEEPROM
            bra     CheckForNextKey

            StoreClosingMechIntoEEPROM
            writeEEPROM TurntableStepEEPROMIndexH, TurntableStepEEPROMIndexL, TurntableTotalStepsL
            bra         FinishCalibCountingMech

            CheckForNextKey
            ; check for keypress
            store   KeypadToCheck, b'00000111'     ; check for key B
            call    CheckForKeypress
            ifequal KeypadCheckResult, d'1', KeepTurningTurnable_CM
            bra     StepAndCheckKeypress


    FinishCalibCountingMech
    return

CalibrateTurntable
    store       TurntableTotalStepsH, d'0'
    store       TurntableTotalStepsL, d'0'
    store       TurntableStepEEPROMIndexH, 0x00    ; for placeholder purpose only
    store       TurntableStepEEPROMIndexL, 0x00    ; used to calculate number of 90 deg turns we have made

    ; fix if offside
    call        FixOffsideTurntable

    KeepSteppingTurntable
        ; call number of steps
        call        StepTurntableClockwise
        addwf16     TurntableTotalStepsH, TurntableTotalStepsL, d'1'

        ; test RB2 for reed switch
        btfss       PORTB, 2
        bra         KeepSteppingTurntable

        ; debouncing code
         call        Delay5ms
         btfss       PORTB, 2
         bra         KeepSteppingTurntable

        ; now step TurntableMagneticWidth to 0
        ; this will serve as the "width" of the magnetic field
        store       TurntableMagneticWidth, d'0'

        KeepSteppingTurntable_Inner
                call        StepTurntableClockwise
                incf        TurntableMagneticWidth

                ; test if reed switch is cleared
                btfsc       PORTB, 2
                bra         KeepSteppingTurntable_Inner
                ; if not clear, keep stepping the motor

                ; debouncing code
                 call        Delay5ms
                 btfsc       PORTB, 2
                 bra         KeepSteppingTurntable_Inner

                ; else
                ; divide by 2 to take the location of the center of the magnet
                bcf         STATUS, 0           ; clear carry bit
                rrcf        TurntableMagneticWidth, 1
                addwf16reg  TurntableTotalStepsH, TurntableTotalStepsL, TurntableMagneticWidth

                ; now write the location H and L into EEPROM address:
                ; 0x00 0x01 for first turn
                ; 0x02 0x03 for second turn
                ; 0x04 0x05 for third run
                ; 0x06 0x07 for fourth run
                ; by the end of everything we should have 0x08, and that will be
                ; used to check if we are done with calibrating
                writeEEPROM TurntableStepEEPROMIndexH, TurntableStepEEPROMIndexL, TurntableTotalStepsH
                incf        TurntableStepEEPROMIndexL
                writeEEPROM TurntableStepEEPROMIndexH, TurntableStepEEPROMIndexL, TurntableTotalStepsL
                incf        TurntableStepEEPROMIndexL

                ; just now we added half of the steps to TurntableTotalSteps
                ; now we need to add another half to prevent disaster
                addwf16reg  TurntableTotalStepsH, TurntableTotalStepsL, TurntableMagneticWidth

        ; check if 0x08
        ifequal     TurntableStepEEPROMIndexL, d'8', EndTurntableCalibration
        ; if not equal, keep repeating
        goto        KeepSteppingTurntable

        EndTurntableCalibration
                ; now, at this point, we should be at the very right of the
                ; magnetic field. But we want to center it so we can start
                ; measuring straight
                    call    StepTurntableCounterClockwise
                    decfsz  TurntableMagneticWidth
                    ; if not zero, then keep doing
                    bra     EndTurntableCalibration
                    ; here we already have it centered
    return


DispenseChips
    ; TurntableTargetIndexL = 2  = First Container
    ; 4 = Second Container
    ifequal     DispenseContainerCount, d'1', CheckFirstUserSelectPattern
    ifequal     DispenseContainerCount, d'2', CheckSecondUserSelectPattern
    ;   if not 2, 4, go to the end, it must have been a mistake
    goto        EndOfDispense

    ; using what user selected from the menu, choose which chips to be
    ; dispensed in sequence
    CheckFirstUserSelectPattern
            ; reset steps to shut the cap
            store       ClosingContainer1StepCount, 0x00
            store       ClosingCount1True, 0x01

            ; if user selects zero, just skip dispensing
            ifnequal    UserSelectPattern1, d'0', CheckFirst
                goto    EndOfDispense                
            CheckFirst
                movff   UserSelectPattern1, WREG
                call    DetermineChipPattern
                bra     StartDispense
    CheckSecondUserSelectPattern
            ; reset steps to shut the cap
            store       ClosingContainer2StepCount, 0x00
            store       ClosingCount2True, 0x01

            ; if user selects zero, just skip dispensing
            ifnequal    UserSelectPattern2, d'0', CheckSecond
                goto    EndOfDispense                
            CheckSecond
                movff   UserSelectPattern2, WREG
                call    DetermineChipPattern
                bra     StartDispense

    ; Start dispensing
    ; First check the first chip colour to be dispensed
    ; do that by reading the lowest bit of DispenserChipPatternL
    ; call appropriate subroutine for each colour
    ; then rotate DispenserChipPattern to the right by one
    ; while substrating the dispense count of 10
    StartDispense
            store   ChipAmountToDispense, d'10'

            DispenseOne

                movlw   B'00000001'
                andwf   DispenserChipPatternL, 0        ; store AND result in WREG
                bz     DispenseOneAmber
                DispenseOneCamel
                        call    DispenseCamelChip
                        bra     DoneDispenseOneChip
                DispenseOneAmber
                        call    DispenseAmberChip
                DoneDispenseOneChip
                        ; rotate to get the next chip in the pattern
                        rrcf16  DispenserChipPatternH, DispenserChipPatternL
                        decfsz  ChipAmountToDispense
                        goto    DispenseOne
    
    EndOfDispense
    return

DispenseAmberChip
    ;store       CountingTotalStepsH, d'0'
    ;store       CountingTotalStepsL, d'0'
    readEEPROM  0x00, 0x08, DispenserTotalStepsH
    readEEPROM  0x00, 0x09, DispenserTotalStepsL

    StepDispenserToTheLeft_Amber
        call    StepDispenserLeft
        ; increment step count
        ;addwf16 CountingTotalStepsH, CountingTotalStepsL, d'1'
        ; need to check if the left microswitch is pressed
        btfss   PORTA, 0            ; left microswitch
        bra     StepDispenserToTheLeft_Amber
        
        ; debounce: delay and test again
        call    Delay1ms
        btfss   PORTA, 0
        bra     StepDispenserToTheLeft_Amber
        
        ; here we can confirm that the left microswitch has been pressed
        ; now we need to reverse the direction

        call    Delay5ms

        ; extra few steps to make sure it's perfectly dispensed
        ; and for speed up purpose
        call    StepDispenserRight
        call    StepDispenserRight
        call    StepDispenserRight

    ReverseDispenser_Amber
        call    StepDispenserRight
        ; decrememnt step count
        subwf16 DispenserTotalStepsH, DispenserTotalStepsL, d'1'
                
        ; check if CountingTotalStepsH
        CheckIfStepCountReached_Amber
                ifequal    DispenserTotalStepsH, d'0', CheckDispenserLowBit_Amber
                goto        ReverseDispenser_Amber
                CheckDispenserLowBit_Amber
                    ifequal    DispenserTotalStepsL, d'0', EndOfDispense_Amber
                    goto        ReverseDispenser_Amber

                            EndOfDispense_Amber
                                ; decrement count of chips in reservoir
                                decf    ChipCountAmber, 1
                                call    Delay5ms
    return


DispenseCamelChip
    ;store       CountingTotalStepsH, d'0'
    ;store       CountingTotalStepsL, d'0'
    readEEPROM  0x00, 0x08, DispenserTotalStepsH
    readEEPROM  0x00, 0x09, DispenserTotalStepsL

    StepDispenserToTheRight_Camel
        call    StepDispenserRight
        ; increment step count
        ;addwf16 CountingTotalStepsH, CountingTotalStepsL, d'1'
        ; need to check if the left microswitch is pressed
        btfss   PORTA, 1            ; right microswitch
        bra     StepDispenserToTheRight_Camel

        ; debounce: delay and test again
        call    Delay1ms
        btfss   PORTA, 1
        bra     StepDispenserToTheRight_Camel

        call    Delay5ms

        ; here we can confirm that the left microswitch has been pressed
        ; now we need to reverse the direction

        ; extra few steps to make sure it's perfectly dispensed
        ; and for speed up purpose
        call    StepDispenserLeft
        call    StepDispenserLeft
        call    StepDispenserLeft

    ReverseDispenser_Camel
        call    StepDispenserLeft
        ; decrememnt step count
        subwf16 DispenserTotalStepsH, DispenserTotalStepsL, d'1'
        

        ; check if CountingTotalStepsH
        CheckIfStepCountReached_Camel
                ifequal    DispenserTotalStepsH, d'0', CheckDispenserLowBit_Camel
                goto        ReverseDispenser_Camel
                CheckDispenserLowBit_Camel
                    ifequal    DispenserTotalStepsL, d'0', EndOfDispense_Camel
                    goto        ReverseDispenser_Camel                    

              
                            EndOfDispense_Camel
                                ; decrement count of chips in reservoir
                                decf    ChipCountCamel, 1
                                call    Delay5ms
    return

DetermineChipPattern
    movff   WREG, TemporaryVariable8

    ifequal TemporaryVariable8, d'6', PatternSix
    ifequal TemporaryVariable8, d'5', PatternFive
    ifequal TemporaryVariable8, d'4', PatternFour
    ifequal TemporaryVariable8, d'3', PatternThree
    ifequal TemporaryVariable8, d'2', PatternTwo
    ifequal TemporaryVariable8, d'1', PatternOne
    bra     CheckPatternEnd

    PatternSix
        store   DispenserChipPatternH, B'00000010'
        store   DispenserChipPatternL, B'10101010'
        bra     CheckPatternEnd
    PatternFive
        store   DispenserChipPatternH, B'00000000'
        store   DispenserChipPatternL, B'11001100'
        bra     CheckPatternEnd
    PatternFour
        store   DispenserChipPatternH, B'00000011'
        store   DispenserChipPatternL, B'00110011'
        bra     CheckPatternEnd
    PatternThree
        store   DispenserChipPatternH, B'00000000'
        store   DispenserChipPatternL, B'00011111'
        bra     CheckPatternEnd
    PatternTwo
        store   DispenserChipPatternH, B'00000011'
        store   DispenserChipPatternL, B'11111111'
        bra     CheckPatternEnd
    PatternOne
        store   DispenserChipPatternH, B'00000000'
        store   DispenserChipPatternL, B'00000000'
CheckPatternEnd
    return

CalibrateDispenser
    ; this calibration will make the dispenser to go all the way to the left
    ; then start going to the right while counting the number of steps needed
    ; to reach the right, divide by 2 to go to the middle

    KeepSteppingDispenserLeft
            call        StepDispenserLeft
            btfss       PORTA, 0                ; test left uswitch
            bra         KeepSteppingDispenserLeft

            ; debouncing: delay and test again
            call        Delay5ms
            btfss       PORTA, 0
            bra         KeepSteppingDispenserLeft

            store       DispenserTotalStepsH, d'0'
            store       DispenserTotalStepsL, d'0'

            ; start stepping right while counting
            KeepSteppingDispenseRight
                    call    StepDispenserRight
                    addwf16 DispenserTotalStepsH, DispenserTotalStepsL, d'1'
                    btfss   PORTA, 1            ; test right uswitch
                    bra     KeepSteppingDispenseRight

                    ; debouncing: delay and test again
                    call    Delay5ms
                    btfss   PORTA, 1
                    bra     KeepSteppingDispenseRight

                    ; at this point the stepping calibration is done
                    ; divide two by rotating right
                    rrcf16  DispenserTotalStepsH, DispenserTotalStepsL
                    ; store data into EEPROM
                    store       TemporaryVariable3, 0x00
                    store       TemporaryVariable4, 0x08
                    writeEEPROM TemporaryVariable3, TemporaryVariable4, DispenserTotalStepsH
                    incf        TemporaryVariable4
                    writeEEPROM TemporaryVariable3, TemporaryVariable4, DispenserTotalStepsL

                    ; return to center
                    ReverseSteppingDispenseLeft
                            call    StepDispenserLeft
                            subwf16 DispenserTotalStepsH, DispenserTotalStepsL, d'1'

                            ifnequal DispenserTotalStepsH, d'0', ReverseSteppingDispenseLeft
                            ifnequal DispenserTotalStepsL, d'0', ReverseSteppingDispenseLeft

                            ; here we are already at the center. Done!
    return

TestSerialCountingChips
    call    StepCountingDownSlow
    call    StepCountingDownSlow
    call    StepCountingDownSlow

    CheckChipUSwitchPressed_Test
        call    StepCountingDown
        btfss   PORTA, 2
        bra     CheckChipUSwitchPressed_Test      ; if pressed

        ; debouncing
         call    Delay44us
         btfss   PORTA, 2
         bra     CheckChipUSwitchPressed_Test      ; if pressed


    InitializeCounting_Test
        ; initialize variables
        store   ChipCountHeightAmberH, d'0'
        store   ChipCountHeightAmberL, d'0'
        store   ChipCountHeightCamelH, d'0'
        store   ChipCountHeightCamelL, d'0'
        store   CountingTotalStepsH, d'0'
        store   CountingTotalStepsL, d'0'

        ; start moving upward
    ChipCounterMoveUpward_Test

        ; increment stepper pattern
        call    StepCountingUp
        addwf16 CountingTotalStepsH, CountingTotalStepsL, d'1'

        CheckAmberIR_Test
            ; check if Amber is detected, ACTIVE LOW, will check for 1
            btfsc   PORTE, 0
            bra     CheckCamelIR_Test            ; if 1, then don't add to count

            ; debouncing
             call    Delay44us
             btfsc   PORTE, 0
             bra     CheckCamelIR_Test            ; if 1, then don't add to count

             ; if zero, increment total current count of Amber
            addwf16 ChipCountHeightAmberH, ChipCountHeightAmberL, d'1'
            movlw   0x0A
            movff   WREG, TXREG
            CheckTXDone_A
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_A      ; when done, TRMT = 1

            movff   ChipCountHeightAmberH, TXREG
            CheckTXDone_A1
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_A1      ; when done, TRMT = 1

            movff   ChipCountHeightAmberL, TXREG
            CheckTXDone_A2
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_A2      ; when done, TRMT = 1

        CheckCamelIR_Test
            ; check if Amber is detected, ACTIVE LOW, will check for 1
            btfsc   PORTE, 1
            bra     CheckTopUSwitch_Test         ; if 1, then don't add to count

            ; debouncing
             call    Delay44us
             btfsc   PORTE, 1
             bra     CheckTopUSwitch_Test            ; if 1, then don't add to count

            ; if zero, increment total current count of Camel
            addwf16 ChipCountHeightCamelH, ChipCountHeightCamelL, d'1'
            movlw   0x0C
            movff   WREG, TXREG
            CheckTXDone_C
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_C      ; when done, TRMT = 1

            movff   ChipCountHeightCamelH, TXREG
            CheckTXDone_C1
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_C1      ; when done, TRMT = 1

            movff   ChipCountHeightCamelL, TXREG
            CheckTXDone_C2
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_C2      ; when done, TRMT = 1

        CheckTopUSwitch_Test
            ; check if top microswitch is pressed
            btfsc   PORTA, 3
            bra     CalculateChips_Test          ; if pressed, A3 = 1

             ; debouncing
             call    Delay44us
             btfsc   PORTA, 3
             bra     CalculateChips_Test          ; if pressed, A3 = 1

            ; else if not pressed, increment total count and keep moving upward

        goto    ChipCounterMoveUpward_Test

    CalculateChips_Test


    return

CountingChips

    ; move to the top
    call    MoveCounterToTop

    InitializeCounting
        ; initialize variables
        store   ChipCountHeightAmberH, d'0'
        store   ChipCountHeightAmberL, d'0'
        store   ChipCountHeightCamelH, d'0'
        store   ChipCountHeightCamelL, d'0'
        store   CountingTotalStepsH, d'0'
        store   CountingTotalStepsL, d'0'

        ; start moving upward
    ChipCounterMoveDownward

        ; increment stepper pattern
        call    StepCountingDown
        addwf16 CountingTotalStepsH, CountingTotalStepsL, d'1'

        CheckAmberIR
            ; check if Amber is detected, ACTIVE LOW, will check for 1
            btfsc   PORTE, 0
            bra     CheckCamelIR            ; if 1, then don't add to count

            ; debouncing
             call    Delay44us
             btfsc   PORTE, 0
             bra     CheckCamelIR            ; if 1, then don't add to count

             ; if zero, increment total current count of Amber
            addwf16 ChipCountHeightAmberH, ChipCountHeightAmberL, d'1'
            

        CheckCamelIR
            ; check if Amber is detected, ACTIVE LOW, will check for 1
            btfsc   PORTE, 1
            bra     CheckBothPressed         ; if 1, then don't add to count

            ; debouncing
             call    Delay44us
             btfsc   PORTE, 1
             bra     CheckBothPressed            ; if 1, then don't add to count

            ; if zero, increment total current count of Camel
            addwf16 ChipCountHeightCamelH, ChipCountHeightCamelL, d'1'


        CheckBothPressed
            ; check if Amber is detected, ACTIVE LOW, will check for 1
            btfss   PORTE, 1
            bra     CheckBottomUSwitch         ; if 1, then don't add to count

            ; debouncing
             call    Delay44us
             btfss   PORTE, 1
             bra     CheckBottomUSwitch            ; if 1, then don't add to count

             btfss   PORTE, 0
            bra     CheckBottomUSwitch         ; if 1, then don't add to count

            ; debouncing
             call    Delay44us
             btfss   PORTE, 0
             bra     CheckBottomUSwitch            ; if 1, then don't add to count
             goto    CalculateChips

            

        CheckBottomUSwitch
            ; check if bottom microswitch is pressed
            btfss   PORTA, 2
            bra     CheckCountingAgain          ; if pressed, A3 = 1

             ; debouncing
             call    Delay44us
             btfss   PORTA, 2
             bra     CheckCountingAgain          ; if pressed, A3 = 1
             goto    CalculateChips

            ; else if not pressed, increment total count and keep moving downward
         CheckCountingAgain
        
        goto    ChipCounterMoveDownward

    CalculateChips

        store   TemporaryVariable11, 0x00
        store   TemporaryVariable12, 0x10

        ; TemporaryVariable9 is the value from EEPROM H
        ; TemporaryVariable10 is the value from EEPROM L

        ; assume initial ChipCountAmber = 16
        store   ChipCountAmber, d'16'

        LoopingCountAmber

                ; for UART
                store   SPBRGH, d'0'                ; according to formula X = ((F_osc/baudrate)/64)-1
                store   SPBRG, d'15'                ; set baud rate to 9600, see table 18-3
                bcf     BAUDCON, BRG16              ; not using 16 bit
                bcf     TXSTA, BRGH
                bcf     TXSTA, SYNC                 ; asynchronous communication
                bsf     TRISC, 6                    ; C6 as input
                bsf     TRISC, 7                    ; C7 as input
                bsf     RCSTA, SPEN                 ; enable serial port
                bcf     PIE1, TXIE                  ; disable interrupt
                bcf     TXSTA, TX9                  ; want 8 bit only
                bsf     TXSTA, TXEN                 ; enable transmission

            ; ifequal, or larger than EEPROM address, then minus
            readEEPROM_REG TemporaryVariable11, TemporaryVariable12, TemporaryVariable9
            incf           TemporaryVariable12
            readEEPROM_REG TemporaryVariable11, TemporaryVariable12, TemporaryVariable10
            incf           TemporaryVariable12

             

            ; if Temporary9|Temporary10 > CountingTotalStepsH|CountingTotalStepsL
            ; then  stop counting

            movff   TemporaryVariable9, WREG
            subwf   ChipCountHeightAmberH, 0         ; CountingTotalStepsH-TemporaryVariable9
            bn      StopLoopingCountAmber            ; if negative, means > holds
            ifequal_reg  ChipCountHeightAmberH, TemporaryVariable9, CheckLowBitAmber
            bra     DecAmber

            CheckLowBitAmber
            movff   TemporaryVariable10, WREG
            subwf   ChipCountHeightAmberL, 0
            bn      StopLoopingCountAmber

            DecAmber
            decf    ChipCountAmber, 1

            ; when chips is 5 or less, will never be able to be counted
            ifequal TemporaryVariable12, 0x24, StopLoopingCountAmber

        goto    LoopingCountAmber

        StopLoopingCountAmber

        ; assume initial ChipCountCamel = 16
        store   ChipCountCamel, d'16'

        store   TemporaryVariable11, 0x00
        store   TemporaryVariable12, 0x24

        LoopingCountCamel
            ; ifequal, or larger than EEPROM address, then minus
            readEEPROM_REG TemporaryVariable11, TemporaryVariable12, TemporaryVariable9
            incf           TemporaryVariable12
            readEEPROM_REG TemporaryVariable11, TemporaryVariable12, TemporaryVariable10
            incf           TemporaryVariable12

            ; if Temporary9|Temporary10 > CountingTotalStepsH|CountingTotalStepsL
            ; then  stop counting

            movff   TemporaryVariable9, TXREG
                CheckTXDone_Container1
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_Container1      ; when done, TRMT = 1

                movff   TemporaryVariable10, TXREG
                CheckTXDone_Container2
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_Container2      ; when done, TRMT = 1

                    movlw   0x20
                    movff   WREG, TXREG
                CheckTXDone_Space2
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_Space2      ; when done, TRMT = 1

                    movff   ChipCountHeightCamelH, TXREG
                CheckTXDone_Container0
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_Container0      ; when done, TRMT = 1

                    movff   ChipCountHeightCamelL, TXREG
                CheckTXDone_Container0L
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_Container0L      ; when done, TRMT = 1

                    movlw   0x0A
                    movff   WREG, TXREG
                CheckTXDone_NewLine
                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
                    bra     CheckTXDone_NewLine      ; when done, TRMT = 1

            movff   TemporaryVariable9, WREG
            subwf   ChipCountHeightCamelH, 0         ; CountingTotalStepsH-TemporaryVariable9
            bn      StopLoopingCountCamel            ; if negative, means > holds
            ifequal_reg TemporaryVariable9, ChipCountHeightCamelH, CheckLowBitCamel
            bra     DecCamel

            CheckLowBitCamel
            movff   TemporaryVariable10, WREG
            subwf   ChipCountHeightCamelL, 0
            bn      StopLoopingCountCamel

            DecCamel
            decf    ChipCountCamel, 1

            ; when chips is 5 or less, will never be able to be counted
            ifequal TemporaryVariable12, 0x38, StopLoopingCountCamel

        goto    LoopingCountCamel

        StopLoopingCountCamel

        ifequal     ChipCountAmber, d'16', DecrementAmber16
        bra         FinishCalculationAmber

        DecrementAmber16
                decf    ChipCountAmber

        FinishCalculationAmber
        ifequal     ChipCountCamel, d'16', DecrementCamel16
        bra     FinishCalculationCamel

        DecrementCamel16
                decf    ChipCountCamel

        FinishCalculationCamel

    return

ShutContainer

    display TableShutContainer1, B'10000000'
    backupdisplay1  TableShutContainer1
    display TableShutContainer2, B'11000000'
    backupdisplay2  TableShutContainer2

    bsf     PORTB, 3
    call    Delay5ms
    call    Delay5ms
    call    Delay5ms
    call    Delay5ms
    call    Delay5ms
    call    Delay5ms
    call    Delay5ms
    call    Delay5ms
    bcf     PORTB, 3

    display TableDispense1, B'10000000'
    backupdisplay1  TableDispense1
    display TableDispense2, B'11000000'
    backupdisplay2  TableDispense2

    return

StepTurntableClockwise_SLOW
    ; rotate the bits twice
    rrncf       TurntablePattern
    rrncf       TurntablePattern

    movff       TurntablePattern, WREG
    ; move the respective bytes to D0 and D1 without affecting the rest
    movff       PORTD, TemporaryVariable6
    andlw       B'00000011'
    iorwf       TemporaryVariable6,1    ; OR operation and store it in File Reg
    iorlw       B'11111100'
    andwf       TemporaryVariable6,1    ; AND operation and store it in File Reg
    movff       TemporaryVariable6, PORTD
    call        DelayHalfSec

    return

StepTurntableClockwise
    ; rotate the bits twice
    rrncf       TurntablePattern
    rrncf       TurntablePattern

    movff       TurntablePattern, WREG
    ; move the respective bytes to D0 and D1 without affecting the rest
    movff       PORTD, TemporaryVariable6
    andlw       B'00000011'
    iorwf       TemporaryVariable6,1    ; OR operation and store it in File Reg
    iorlw       B'11111100'
    andwf       TemporaryVariable6,1    ; AND operation and store it in File Reg
    movff       TemporaryVariable6, PORTD


;
;
;                    movff   ClosingContainer1StepCount, TXREG
;                CheckTXDone_Container1
;                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
;                    bra     CheckTXDone_Container1      ; when done, TRMT = 1
;
;                    movlw   0x20
;                    movff   WREG, TXREG
;                CheckTXDone_Space
;                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
;                    bra     CheckTXDone_Space      ; when done, TRMT = 1
;
;                movff   ClosingContainer2StepCount, TXREG
;                CheckTXDone_Container2
;                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
;                    bra     CheckTXDone_Container2      ; when done, TRMT = 1
;
;                    movlw   0x20
;                    movff   WREG, TXREG
;                CheckTXDone_Space2
;                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
;                    bra     CheckTXDone_Space2      ; when done, TRMT = 1
;
;                    movff   ClosingPositionFromEEPROM, TXREG
;                CheckTXDone_Container0
;                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
;                    bra     CheckTXDone_Container0      ; when done, TRMT = 1
;
;                    movlw   0x0A
;                    movff   WREG, TXREG
;                CheckTXDone_NewLine
;                    btfss   TXSTA, TRMT      ; if TRMT = 1, skip out from loop
;                    bra     CheckTXDone_NewLine      ; when done, TRMT = 1

    ifequal     ClosingCount1True, 0x01, IncrementClosingContainer1    
    goto        EndOfIncrementClosingContainer1

    IncrementClosingContainer1
            incf        ClosingContainer1StepCount
            ifequal_reg ClosingContainer1StepCount, ClosingPositionFromEEPROM, ShutContainer1
            bra         EndOfIncrementClosingContainer1
            ShutContainer1
                        store   ClosingCount1True, 0x00
                        store   ClosingContainer1StepCount, 0x00
                        call    ShutContainer

    EndOfIncrementClosingContainer1

    ifequal     ClosingCount2True, 0x01, IncrementClosingContainer2
    goto        EndOfIncrementClosingContainer2

    IncrementClosingContainer2
            incf        ClosingContainer2StepCount
            ifequal_reg ClosingContainer2StepCount, ClosingPositionFromEEPROM, ShutContainer2
            bra         EndOfIncrementClosingContainer2
            ShutContainer2
                        store   ClosingCount2True, 0x00
                        store   ClosingContainer2StepCount, 0x00
                        call    ShutContainer

    EndOfIncrementClosingContainer2
    goto        DelayTurntable

    DelayTurntable

    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    return


StepTurntableCounterClockwise
    ; rotate the bits twice
    rlncf       TurntablePattern
    rlncf       TurntablePattern

    movff       TurntablePattern, WREG
    ; move the respective bytes to D0 and D1 without affecting the rest
    movff       PORTD, TemporaryVariable6
    andlw       B'00000011'
    iorwf       TemporaryVariable6,1    ; OR operation and store it in File Reg
    iorlw       B'11111100'
    andwf       TemporaryVariable6,1    ; AND operation and store it in File Reg
    movff       TemporaryVariable6, PORTD

    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    call        Delay5ms
    return

StepDispenserLeft
    ; rotate the bits twice
    rlncf       DispenserPattern
    rlncf       DispenserPattern

    movff       DispenserPattern, WREG
    ; move the respective bytes to C0 and C1 without affecting the rest
    movff       PORTC, TemporaryVariable
    andlw       B'00000011'
    iorwf       TemporaryVariable,1    ; OR operation and store it in File Reg
    iorlw       B'11111100'
    andwf       TemporaryVariable,1    ; AND operation and store it in File Reg
    movff       TemporaryVariable, PORTC

    call        Delay1ms
    call        Delay1ms
    call        Delay1ms
    call        Delay1ms

    return

StepDispenserLeftSlow
    ; rotate the bits twice
    rlncf       DispenserPattern
    rlncf       DispenserPattern

    movff       DispenserPattern, WREG
    ; move the respective bytes to C0 and C1 without affecting the rest
    movff       PORTC, TemporaryVariable
    andlw       B'00000011'
    iorwf       TemporaryVariable,1    ; OR operation and store it in File Reg
    iorlw       B'11111100'
    andwf       TemporaryVariable,1    ; AND operation and store it in File Reg
    movff       TemporaryVariable, PORTC

    call        Delay5ms
    return

StepDispenserRight
    ; rotate the bits twice
    rrncf       DispenserPattern
    rrncf       DispenserPattern

    movff       DispenserPattern, WREG
    ; move the respective bytes to C0 and C1 without affecting the rest
    movff       PORTC, TemporaryVariable
    andlw       B'00000011'
    iorwf       TemporaryVariable,1    ; OR operation and store it in File Reg
    iorlw       B'11111100'
    andwf       TemporaryVariable,1    ; AND operation and store it in File Reg
    movff       TemporaryVariable, PORTC

    call        Delay1ms
    call        Delay1ms
    call        Delay1ms
    call        Delay1ms
    return

StepDispenserRightSlow
    ; rotate the bits twice
    rrncf       DispenserPattern
    rrncf       DispenserPattern

    movff       DispenserPattern, WREG
    ; move the respective bytes to C0 and C1 without affecting the rest
    movff       PORTC, TemporaryVariable
    andlw       B'00000011'
    iorwf       TemporaryVariable,1    ; OR operation and store it in File Reg
    iorlw       B'11111100'
    andwf       TemporaryVariable,1    ; AND operation and store it in File Reg
    movff       TemporaryVariable, PORTC

    call        Delay5ms
    return

StepCountingUpSlow
    ; rotate the bits twice
    rlncf       CountingPattern
    rlncf       CountingPattern

    movff       PORTC, TemporaryVariable    ; store it to change both pins at the same time
    ; write to output pin
    movlw       B'00000001'                 ; check the LSB
    andwf       CountingPattern, 0          ; AND operation store result in W
    bz          ChipCountWriteZeroToC2_Slow
    ChipCountWriteOneToC2_Slow
        bsf     TemporaryVariable, 2
        bra     ChipCountSignalC5_Slow
    ChipCountWriteZeroToC2_Slow
        bcf     TemporaryVariable, 2
        bra     ChipCountSignalC5_Slow

        ChipCountSignalC5_Slow
            movlw   B'00000010'             ; check the 2nd LSB
            andwf   CountingPattern, 0      ; AND operation store result in W
            bz      ChipCountWriteZeroToC5_Slow
            ChipCountWriteOneToC5_Slow
                bsf     TemporaryVariable, 5
                bra     EndChipCountSignal_Slow
            ChipCountWriteZeroToC5_Slow
                bcf     TemporaryVariable, 5
                bra     EndChipCountSignal_Slow

        EndChipCountSignal_Slow
            movff   TemporaryVariable, PORTC ; move the modified version back


    call        Delay5ms

    return

StepCountingUpFast
    ; rotate the bits twice
    rlncf       CountingPattern
    rlncf       CountingPattern

    movff       PORTC, TemporaryVariable    ; store it to change both pins at the same time
    ; write to output pin
    movlw       B'00000001'                 ; check the LSB
    andwf       CountingPattern, 0          ; AND operation store result in W
    bz          ChipCountWriteZeroToC2_Fast
    ChipCountWriteOneToC2_Fast
        bsf     TemporaryVariable, 2
        bra     ChipCountSignalC5_Fast
    ChipCountWriteZeroToC2_Fast
        bcf     TemporaryVariable, 2
        bra     ChipCountSignalC5_Fast

        ChipCountSignalC5_Fast
            movlw   B'00000010'             ; check the 2nd LSB
            andwf   CountingPattern, 0      ; AND operation store result in W
            bz      ChipCountWriteZeroToC5_Fast
            ChipCountWriteOneToC5_Fast
                bsf     TemporaryVariable, 5
                bra     EndChipCountSignal_Fast
            ChipCountWriteZeroToC5_Fast
                bcf     TemporaryVariable, 5
                bra     EndChipCountSignal_Fast

        EndChipCountSignal_Fast
            movff   TemporaryVariable, PORTC ; move the modified version back


    call        Delay5ms
    call        Delay1ms

    return


StepCountingUp
    ; rotate the bits twice
    rlncf       CountingPattern
    rlncf       CountingPattern

    movff       PORTC, TemporaryVariable    ; store it to change both pins at the same time
    ; write to output pin
    movlw       B'00000001'                 ; check the LSB
    andwf       CountingPattern, 0          ; AND operation store result in W
    bz          ChipCountWriteZeroToC2
    ChipCountWriteOneToC2
        bsf     TemporaryVariable, 2
        bra     ChipCountSignalC5
    ChipCountWriteZeroToC2
        bcf     TemporaryVariable, 2
        bra     ChipCountSignalC5

        ChipCountSignalC5
            movlw   B'00000010'             ; check the 2nd LSB
            andwf   CountingPattern, 0      ; AND operation store result in W
            bz      ChipCountWriteZeroToC5
            ChipCountWriteOneToC5
                bsf     TemporaryVariable, 5
                bra     EndChipCountSignal
            ChipCountWriteZeroToC5
                bcf     TemporaryVariable, 5
                bra     EndChipCountSignal

        EndChipCountSignal
            movff   TemporaryVariable, PORTC ; move the modified version back


    call        Delay1ms
    call        Delay1ms
    call        Delay1ms
    call        Delay1ms

    return

StepCountingDown
    ; rotate the bits twice
    rrncf       CountingPattern
    rrncf       CountingPattern

    movff       PORTC, TemporaryVariable    ; store it to change both pins at the same time
    ; write to output pin
    movlw       B'00000001'                 ; check the LSB
    andwf       CountingPattern, 0          ; AND operation store result in W
    bz          ChipCountWriteZeroToC2_2
    ChipCountWriteOneToC2_2
        bsf     TemporaryVariable, 2
        bra     ChipCountSignalC5_2
    ChipCountWriteZeroToC2_2
        bcf     TemporaryVariable, 2
        bra     ChipCountSignalC5_2

        ChipCountSignalC5_2
            movlw   B'00000010'             ; check the 2nd LSB
            andwf   CountingPattern, 0      ; AND operation store result in W
            bz      ChipCountWriteZeroToC5_2
            ChipCountWriteOneToC5_2
                bsf     TemporaryVariable, 5
                bra     EndChipCountSignal_2
            ChipCountWriteZeroToC5_2
                bcf     TemporaryVariable, 5
                bra     EndChipCountSignal_2

        EndChipCountSignal_2
            movff   TemporaryVariable, PORTC ; move the modified version back


    call        Delay1ms
    call        Delay1ms
    call        Delay1ms
    call        Delay1ms

    return


StepCountingDownSlow
    ; rotate the bits twice
    rrncf       CountingPattern
    rrncf       CountingPattern

    movff       PORTC, TemporaryVariable    ; store it to change both pins at the same time
    ; write to output pin
    movlw       B'00000001'                 ; check the LSB
    andwf       CountingPattern, 0          ; AND operation store result in W
    bz          ChipCountWriteZeroToC2_2_Slow
    ChipCountWriteOneToC2_2_Slow
        bsf     TemporaryVariable, 2
        bra     ChipCountSignalC5_2_Slow
    ChipCountWriteZeroToC2_2_Slow
        bcf     TemporaryVariable, 2
        bra     ChipCountSignalC5_2_Slow

        ChipCountSignalC5_2_Slow
            movlw   B'00000010'             ; check the 2nd LSB
            andwf   CountingPattern, 0      ; AND operation store result in W
            bz      ChipCountWriteZeroToC5_2_Slow
            ChipCountWriteOneToC5_2_Slow
                bsf     TemporaryVariable, 5
                bra     EndChipCountSignal_2_Slow
            ChipCountWriteZeroToC5_2_Slow
                bcf     TemporaryVariable, 5
                bra     EndChipCountSignal_2_Slow

        EndChipCountSignal_2_Slow
            movff   TemporaryVariable, PORTC ; move the modified version back


    call        Delay5ms
    call        Delay1ms

    return

StepCountingDownFast
    ; rotate the bits twice
    rrncf       CountingPattern
    rrncf       CountingPattern

    movff       PORTC, TemporaryVariable    ; store it to change both pins at the same time
    ; write to output pin
    movlw       B'00000001'                 ; check the LSB
    andwf       CountingPattern, 0          ; AND operation store result in W
    bz          ChipCountWriteZeroToC2_2_Fast
    ChipCountWriteOneToC2_2_Fast
        bsf     TemporaryVariable, 2
        bra     ChipCountSignalC5_2_Fast
    ChipCountWriteZeroToC2_2_Fast
        bcf     TemporaryVariable, 2
        bra     ChipCountSignalC5_2_Fast

        ChipCountSignalC5_2_Fast
            movlw   B'00000010'             ; check the 2nd LSB
            andwf   CountingPattern, 0      ; AND operation store result in W
            bz      ChipCountWriteZeroToC5_2_Fast
            ChipCountWriteOneToC5_2_Fast
                bsf     TemporaryVariable, 5
                bra     EndChipCountSignal_2_Fast
            ChipCountWriteZeroToC5_2_Fast
                bcf     TemporaryVariable, 5
                bra     EndChipCountSignal_2_Fast

        EndChipCountSignal_2_Fast
            movff   TemporaryVariable, PORTC ; move the modified version back


    return


; ---------------------------------------------------------------------------
; Delay subroutines
; ---------------------------------------------------------------------------
Delay44us
    store   DelayCounter1, 0x23
    Delay44usLoop
        decfsz  DelayCounter1, 1
        bra     Delay44usLoop
    return

Delay5ms
    store   DelayCounter2, d'110'
    Delay5msLoop
        call    Delay44us
        decfsz  DelayCounter2, 1
        bra     Delay5msLoop
    return

Delay1ms
    store   DelayCounter2, d'22'
    Delay1msLoop
        call    Delay44us
        decfsz  DelayCounter2, 1
        bra     Delay1msLoop
    return

DelayMenu
    store   DelayCounter3, 0xD8
    DelayMenuLoop
        call    Delay5ms
        decfsz  DelayCounter3, 1
        bra     DelayMenuLoop
    return

DelayHalfSec
    store   DelayCounter3, 0x6C
    DelayHalfSecLoop
        call    Delay5ms
        decfsz  DelayCounter3, 1
        bra     DelayHalfSecLoop
    return

; ---------------------------------------------------------------------------
; LCD Display related
; ---------------------------------------------------------------------------

; InitializeLCD: set configuration for the LCD Display
; Input: None                       Output: None
InitializeLCD
    call        Delay5ms
    call        Delay5ms
    movlw       B'00110011'         ; set for 8 bit twice
    call        WriteInstToLCD
    movlw       B'00110011'         ; set for 8 bit
    call        WriteInstToLCD
    movlw       B'00110011'         ; set for 8 bit once again, then 4 bit
    call        WriteInstToLCD
    movlw       B'00101000'         ; 4 bits, 2 lines, 5x8
    call        WriteInstToLCD
    movlw       B'00001100'         ; display on/off
    call        WriteInstToLCD
    movlw       B'00000110'         ; entry mode
    call        WriteInstToLCD
    movlw       B'00000001'         ; clear ram
    call        WriteInstToLCD
    return

; WriteInstToLCD: sequences of command to modify the config of LCD
; Input: WREG                       Output: None
WriteInstToLCD
    bcf     LCD_RS                  ; clear RS to enter instruction mode
    movwf   TemporaryVariable5       ; store into temporary register
    call    MoveMSB
    bsf     LCD_E                   ; pulse LCD high
    nop
    bcf     LCD_E                   ; pulse LCD low
    swapf   TemporaryVariable5, 0    ; swap nibbles
    call    MoveMSB
    bsf     LCD_E                   ; pulse LCD high
    nop                             ; wait
    bcf     LCD_E                   ; pulse LCD low
    call    Delay5ms
    return

; WriteDataToLCD: sequences of command to display a character on LCD
; Input: WREG                       Output: None
WriteDataToLCD
    bsf     LCD_RS                  ; set RS for data mode
    movwf   TemporaryVariable5       ; store into temporary register
    call    MoveMSB
    bsf     LCD_E                   ; pulse LCD high
    nop
    bcf     LCD_E                   ; pulse LCD low
    swapf   TemporaryVariable5, 0    ; swap nibbles
    call    MoveMSB
    bsf     LCD_E                   ; pulse LCD high
    nop                             ; wait
    bcf     LCD_E                   ; pulse LCD low
    call    Delay44us
    return

; Write16DataToLCD: Take 16 Character from the table, and display them one by
;                   one, by invoking the subroutine WriteDataToLCD for 16 times
; Input: tblrd (Table Pointer)      Output: None
Write16DataToLCD
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    Loop16LCD
        call    WriteDataToLCD
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     Loop16LCD
    return

; MoveMSB: Move the upper 4 bits of WREG into PORTD without affecting current
;          values in it
; Input: WREG                   Output: None
MoveMSB
    movff   PORTD, TemporaryVariable6
    andlw   0xF0
    iorwf   TemporaryVariable6,1    ; OR operation and store it in File Reg
    iorlw   0x0F
    andwf   TemporaryVariable6,1    ; AND operation and store it in File Reg
    movff   TemporaryVariable6, PORTD
    return

; ClearLCD: Clear the entire LCD
; Input: None                   Output: None
ClearLCD
    movlw   B'11000000'         ; 2nd line
    call    WriteInstToLCD
    movlw   B'00000001'         ; clear 2nd line
    call    WriteInstToLCD
    movlw   B'10000000'         ; 1st line
    call    WriteInstToLCD
    movlw   B'00000001'         ; clear 1st line
    call    WriteInstToLCD
    return

; ---------------------------------------------------------------------------
; Keypad related
; ---------------------------------------------------------------------------

; CheckForKeypress: Check for specific key
; Input: KeypadToCheck              Output: KeypadCheckResult
CheckForKeypress
    movff   PORTB, PortB_data

    btfss   PortB_data, 1           ; poll B1 for Keypad Available
    goto    NoKeyPress

    swapf   PortB_data, w
    andlw   b'00001111'             ; mask to read only the lower 4 bit
    subwf   KeypadToCheck, 0        ; substract to check if equal
    bnz     NoKeyPress              ; if not zero, not the key we are looking for

    ; hold, wait for user to unpress
    CheckForKeypress_Loop
            movff   PORTB, PortB_data
            btfsc   PortB_data, 1
            bra     CheckForKeypress_Loop

    store   KeypadCheckResult, d'1'
    bra     EndKeyPress

    NoKeyPress
            store   KeypadCheckResult, d'0'
    EndKeyPress
    return

; CheckAnyKeypress: Check for long press, no wait for user to release their hand
; Output: KeypadCheckResult
CheckAnyKeypress
    movff   PORTB, PortB_data

    btfss   PortB_data, 1           ; poll B1 for Keypad Available
    goto    NoAnyKeyPress

    store   KeypadCheckResult, d'1'
    bra     EndAnyKeyPress

    NoAnyKeyPress
            store   KeypadCheckResult, d'0'
    EndAnyKeyPress
    return


; CheckKeyForDispense: Check all keys, but only 0-6 are valid
CheckKeyForDispense
    movff   PORTB, PortB_data

    btfss   PortB_data, 1           ; poll B1 for Keypad Available
    goto    NoDispenseKeyPress

    ; hold, wait for user to unpress
    CheckForAllKeypress_Loop
            movff   PORTB, PortB_data
            btfsc   PortB_data, 1
            bra     CheckForAllKeypress_Loop

    swapf   PortB_data, w
    andlw   b'00001111'             ; mask to read only the lower 4 bit
    movwf   PortB_data              ; put it back

    ; check for 0-6
    ifequal PortB_data, d'13', DispenseKey0
    ifequal PortB_data, d'0', DispenseKey1
    ifequal PortB_data, d'1', DispenseKey2
    ifequal PortB_data, d'2', DispenseKey3
    ifequal PortB_data, d'4', DispenseKey4
    ifequal PortB_data, d'5', DispenseKey5
    ifequal PortB_data, d'6', DispenseKey6

    ; if not those, then we have an invalid key
    display TablePatternInvalid, B'11000000'
    backupdisplay2  TablePatternInvalid
    call    DelayMenu
    display TablePattern2, B'11000000'
    backupdisplay2  TablePattern2
    bra     NoDispenseKeyPress

    NoDispenseKeyPress
            store   KeypadCheckResult, d'9'
            bra     CheckForAllKeypress_End
            
    DispenseKey0         store  KeypadCheckResult, d'0'
                         bra    CheckForAllKeypress_End
    DispenseKey1         store  KeypadCheckResult, d'1'
                         movlw  0x0A
                         addwf  NumberOfAmberRequested, 1
                         movlw  0x00
                         addwf  NumberOfCamelRequested, 1
                         bra    CheckForAllKeypress_End
    DispenseKey2         store  KeypadCheckResult, d'2'
                         movlw  0x00
                         addwf  NumberOfAmberRequested, 1
                         movlw  0x0A
                         addwf  NumberOfCamelRequested, 1
                         bra    CheckForAllKeypress_End
    DispenseKey3         store  KeypadCheckResult, d'3'
                         movlw  0x05
                         addwf  NumberOfAmberRequested, 1
                         movlw  0x05
                         addwf  NumberOfCamelRequested, 1
                         bra    CheckForAllKeypress_End
    DispenseKey4         store  KeypadCheckResult, d'4'
                         movlw  0x04
                         addwf  NumberOfAmberRequested, 1
                         movlw  0x06
                         addwf  NumberOfCamelRequested, 1
                         bra    CheckForAllKeypress_End
    DispenseKey5         store  KeypadCheckResult, d'5'
                         movlw  0x06
                         addwf  NumberOfAmberRequested, 1
                         movlw  0x04
                         addwf  NumberOfCamelRequested, 1
                         bra    CheckForAllKeypress_End
    DispenseKey6         store  KeypadCheckResult, d'6'
                         movlw  0x05
                         addwf  NumberOfAmberRequested, 1
                         movlw  0x05
                         addwf  NumberOfCamelRequested, 1

    CheckForAllKeypress_End
    return

; ---------------------------------------------------------------------------
; I2C related
; Reference: http://www.edaboard.com/thread208083.html
; ---------------------------------------------------------------------------
InitializeI2C
    store   SSPCON1, B'00101000'    ; enable MSSP mode, set to I2C master mode
    store   SSPSTAT, B'10000000'    ; disable slew rate control for 100kHz
    store   SSPADD, D'24'           ; F_osc/(4*(SSPADD+1))
    call    StartI2CBit
    movlw   0xD0                    ; RTC slave address | write bit
    call    WriteI2C
    movlw   0x07                    ; points to RTC register address
    call    WriteI2C
    movlw   0x90                    ; Enable square wave output on RTC
    call    WriteI2C                ; refer DS1307 datasheet
    call    StopI2CBit
    return

StartI2CBit
    bsf     SSPCON2,SEN             ; enable start
    call    WaitI2C
    return

StopI2CBit
    bsf     SSPCON2,PEN             ; enable stop
    call    WaitI2C
    return

WriteI2C
    movwf   SSPBUF
    call    WaitI2C
    return

WaitI2C
    btfss   PIR1,SSPIF              ; wait for SSPIF = 1
    goto    $-2
    bcf     PIR1,SSPIF              ; clear SSPIF in software
    return

SetRTC                              ; NOTE: only for initialization purpose
                                    ; when battery is removed
    store   TemporaryVariable, 0x00     ; set second
    store   TemporaryVariable2, 0x00
    call    WriteToI2CBuffer
    store   TemporaryVariable, 0x01     ; set minutes
    store   TemporaryVariable2, 0x13
    call    WriteToI2CBuffer
    store   TemporaryVariable, 0x02     ; set hour
    store   TemporaryVariable2, 0x15
    call    WriteToI2CBuffer
    store   TemporaryVariable, 0x03     ; set day
    store   TemporaryVariable2, 0x01
    call    WriteToI2CBuffer
    store   TemporaryVariable, 0x04     ; set date
    store   TemporaryVariable2, 0x08
    call    WriteToI2CBuffer
    store   TemporaryVariable, 0x05     ; set month
    store   TemporaryVariable2, 0x04
    call    WriteToI2CBuffer
    store   TemporaryVariable, 0x06     ; set year
    store   TemporaryVariable2, 0x13
    call    WriteToI2CBuffer


    return

WriteToI2CBuffer                    ; take TemporaryVariable as address
                                    ; take TemporaryVariable2 as data
    call    StartI2CBit
    movlw   0xD0                    ; RTC slave address | write bit
    call    WriteI2C
    movf    TemporaryVariable, w    ; points to RTC register address
    call    WriteI2C
    movf    TemporaryVariable2, w
    call    WriteI2C
    call    StopI2CBit
    return

ReadFromI2CBuffer                   ; output to WREG
    btfsc   SSPSTAT, 2              ; check if address needs to be updated
    bra     $-2
    bsf     SSPCON2, RCEN           ; enable receive
    call    WaitI2C
    bcf     PIR1, SSPIF             ; clear interrupt flag
    call    I2C_ACK                 ; send ACK bit to keep reading
    movf    SSPBUF, W               ; move data from buffer to W register
    return

ReadFromI2CBufferNACK               ; output to WREG
    btfsc   SSPSTAT, 2              ; check if address needs to be updated
    bra     $-2
    bsf     SSPCON2, RCEN           ; enable receive
    call    WaitI2C
    bcf     PIR1, SSPIF             ; clear interrupt flag
    call    I2C_NACK                ; send NACK bit to stop reading
    movf    SSPBUF, W               ; move data from buffer to W register
    return

ReadRTC
    call    StartI2CBit
    movlw   0xD0                    ; RTC slave addr | write
    call    WriteI2C
    movlw   0x00                    ; points to register 0x00
    call    WriteI2C
    call    StopI2CBit

    call    StartI2CBit
    movlw   0xD1                    ; RTC slave addr | read
    call    WriteI2C

    call    ReadFromI2CBuffer
    movwf   RTC_Second
    call    ReadFromI2CBuffer
    movwf   RTC_Minute
    call    ReadFromI2CBuffer
    movwf   RTC_Hour
    call    ReadFromI2CBuffer
    movwf   RTC_Day
    call    ReadFromI2CBuffer
    movwf   RTC_Date
    call    ReadFromI2CBuffer
    movwf   RTC_Month
    call    ReadFromI2CBufferNACK
    movwf   RTC_Year

    call    StopI2CBit
    return

I2C_ACK
    bcf     SSPCON2, ACKDT          ; acknowledge receiving
    bsf     SSPCON2, ACKEN
    call    WaitI2C
    return

I2C_NACK
    bsf     SSPCON2, ACKDT          ; not acknowledge receiving
    bsf     SSPCON2, ACKEN
    call    WaitI2C
    return

ConvertRTC							; convert upper and lower nibble into ASCII
                                    ; input: WREG, output: RTC_H, RTC_L
	movwf	TemporaryVariable
	swapf	TemporaryVariable,W
	andlw	B'00001111'
	addlw	0x30
	movwf	RTC_H
	movf	TemporaryVariable,W
	andlw	B'00001111'
	addlw	0x30
	movwf	RTC_L
	return

DisplayRTC_BottomLeft
    call    ReadRTC

    ; display time and date
    movlw   B'11000000'             ; go to second line 1st char
    call    WriteInstToLCD

    movlw   0x5B                    ; [ character
    call    WriteDataToLCD

    movff   RTC_Hour, WREG
    call    ConvertRTC
    movff   RTC_H, WREG
    call    WriteDataToLCD
    movff   RTC_L, WREG
    call    WriteDataToLCD

    movlw   0x3A                    ; : character
    call    WriteDataToLCD

    movff   RTC_Minute, WREG
    call    ConvertRTC
    movff   RTC_H, WREG
    call    WriteDataToLCD
    movff   RTC_L, WREG
    call    WriteDataToLCD

    movlw   0x5D                    ; ] character
    call    WriteDataToLCD

    return

CalculateTimeDiff

    LowSecond
    ; lower digit of second
        movff   RTC_Second, WREG
        andlw   0x0F                    ; read lower nibble only
        movwf   TemporaryVariable
        movff   RTC_Second_Old, WREG
        andlw   0x0F
        movwf   TemporaryVariable2
        ; TemporaryVariable (new) - TemporaryVariable2 (old)
        movff   TemporaryVariable2, WREG
        subwf   TemporaryVariable, 0    ; put result in WREG
        bnn     HighSecond_temp         ; if result not negative
                                        ; go deal with higher bit
        ; if negative, then add 10 to it
        addlw   d'10'
        andlw   0x0F                    ; mask to have lower bit only
        movwf   RTC_Second_Diff
        ; and add 1 to the high digit of second of the OLD one
        swapf   RTC_Second_Old, 0       ; swap to lower nibble and store in W
        andlw   0x0F                    ; mask it
        incf    WREG                    ; add one
        swapf   WREG                    ; swap back to upper nibble
        movwf   RTC_Second_Old          ; move it back to old data
        bra     HighSecond

        HighSecond_temp
                andlw   0x0F
                movwf   RTC_Second_Diff

    HighSecond
        ; high digit of second
        swapf   RTC_Second, w
        andlw   0x0F                    ; read lower nibble only
        movwf   TemporaryVariable
        swapf   RTC_Second_Old, w
        andlw   0x0F
        movwf   TemporaryVariable2
        ; TemporaryVariable (new) - TemporaryVariable2 (old)
        movff   TemporaryVariable2, WREG
        subwf   TemporaryVariable, 0    ; put result in WREG
        bnn     LowMinute_temp          ; if result not negative
                                        ; go deal with higher bit
        ; if negative, then add 6 to it
        addlw   d'6'
        swapf   WREG
        andlw   0xF0                    ; mask to only have upper bit
        addwf   RTC_Second_Diff, 1      ; add to store into the second difference
        ; and add 1 to the low digit of minute of the OLD one
        ; before adding it, need to check if it's 9. If it is, then will need
        ; to add one to HighMinute
        sublw   d'9'
        bnz     MinuteLowAddOne
        MinuteHighAddOne
            movlw   0xF0                ; make the lower bit 0 (bcoz 9+1=10)
            andwf   RTC_Minute_Old, 1
            movlw   0x10
            addwf   RTC_Minute_Old, 1   ; add 1 to the high nibble
            bra     LowMinute

        MinuteLowAddOne
            ; simply add 1 to the RTC_Minute_Old
            incf     RTC_Minute_Old, 1
            bra     LowMinute

        LowMinute_temp
                andlw   0x0F
                swapf   WREG
                addwf   RTC_Second_Diff

    LowMinute
        ; lower digit of minute
        movff   RTC_Minute, WREG
        andlw   0x0F                    ; read lower nibble only
        movwf   TemporaryVariable
        movff   RTC_Minute_Old, WREG
        andlw   0x0F
        movwf   TemporaryVariable2
        ; TemporaryVariable (new) - TemporaryVariable2 (old)
        movff   TemporaryVariable2, WREG
        subwf   TemporaryVariable, 0    ; put result in WREG
        bnn     HighMinute_temp         ; if result not negative
                                        ; go deal with higher bit
        ; if negative, then add 10 to it
        addlw   d'10'
        andlw   0x0F                    ; mask to have lower bit only
        movwf   RTC_Minute_Diff
        ; and add 1 to the high digit of minute of the OLD one
        swapf   RTC_Minute_Old, 0       ; swap to lower nibble and store in W
        andlw   0x0F                    ; mask it
        incf    WREG                    ; add one
        swapf   WREG                    ; swap back to upper nibble
        movwf   RTC_Minute_Old          ; move it back to old data
        bra     HighMinute

        HighMinute_temp
                andlw   0x0F
                movwf   RTC_Minute_Diff

     HighMinute
        ; high digit of minute
        swapf   RTC_Minute, w
        andlw   0x0F                    ; read lower nibble only
        movwf   TemporaryVariable
        swapf   RTC_Minute_Old, w
        andlw   0x0F
        movwf   TemporaryVariable2
        ; TemporaryVariable (new) - TemporaryVariable2 (old)
        movff   TemporaryVariable2, WREG
        subwf   TemporaryVariable, 0    ; put result in WREG
        bnn     EndCalculation_temp     ; if result not negative
                                        ; go deal with higher bit
        ; if negative, then add 6 to it
        addlw   d'6'
        swapf   WREG
        andlw   0xF0                    ; mask to only have upper bit                    ; mask to only have upper bit
        addwf   RTC_Minute_Diff, 1      ; add to store into the second difference
        ; and add 1 to the low digit of hour of the OLD one
        bra     EndCalculation

        EndCalculation_temp
                andlw   0x0F
                swapf   WREG
                addwf   RTC_Minute_Diff

    EndCalculation

    return

; -----------------------------------------------------------------------------
; Log related
; -----------------------------------------------------------------------------
LogDateTime
    call    ReadRTC

    movff   RTC_Month, WREG
    call    ConvertRTC
    movff   RTC_H, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
    movff   RTC_L, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movlw   '/'
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movff   RTC_Date, WREG
    call    ConvertRTC
    movff   RTC_H, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
    movff   RTC_L, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movlw   ' '
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movff   RTC_Hour, WREG
    call    ConvertRTC
    movff   RTC_H, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
    movff   RTC_L, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movlw   ':'
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movff   RTC_Minute, WREG
    call    ConvertRTC
    movff   RTC_H, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
    movff   RTC_L, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movlw   ':'
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movff   RTC_Second, WREG
    call    ConvertRTC
    movff   RTC_H, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
    movff   RTC_L, WREG
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movlw   ' '
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    ; convert them
    return

LogUserSelectPattern
    call    LogDateTime

    movlw   upper TableLogUserSelectPattern    ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogUserSelectPattern
    movwf   TBLPTRH
    movlw   low TableLogUserSelectPattern
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogUserSelectPattern_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogUserSelectPattern_Loop

    movff       UserSelectPattern1, WREG
    addlw       0x30                ; convert ASCII
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movlw       ','
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movff       UserSelectPattern2, WREG
    addlw       0x30                ; convert ASCII
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return


LogIdle
    call    LogDateTime

    movlw   upper TableLogIdle    ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogIdle
    movwf   TBLPTRH
    movlw   low TableLogIdle
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogIdle_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogIdle_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogTurnOn
    call    LogDateTime

    movlw   upper TableLogTurnOn    ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogTurnOn
    movwf   TBLPTRH
    movlw   low TableLogTurnOn
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogTurnOn_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogTurnOn_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogTurnOff
    call    LogDateTime

    movlw   upper TableLogTurnOff   ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogTurnOff
    movwf   TBLPTRH
    movlw   low TableLogTurnOff
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogTurnOff_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogTurnOff_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogEStop
    call    LogDateTime

    movlw   upper TableLogEStop     ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogEStop
    movwf   TBLPTRH
    movlw   low TableLogEStop
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogEStop_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogEStop_Loop
    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogVibratorOff
    call    LogDateTime

    movlw   upper TableLogVibratorOff ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogVibratorOff
    movwf   TBLPTRH
    movlw   low TableLogVibratorOff
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogVibratorOff_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogVibratorOff_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogVibratorOn
    call    LogDateTime

    movlw   upper TableLogVibratorOn ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogVibratorOn
    movwf   TBLPTRH
    movlw   low TableLogVibratorOn
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogVibratorOn_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogVibratorOn_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogDispenseEnd
    call    LogDateTime

    movlw   upper TableLogDispenseEnd ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogDispenseEnd
    movwf   TBLPTRH
    movlw   low TableLogDispenseEnd
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogDispenseEnd_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogDispenseEnd_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogUserSelectPatternCancelled
    call    LogDateTime

    movlw   upper TableLogUserSelectPatternCancel ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogUserSelectPatternCancel
    movwf   TBLPTRH
    movlw   low TableLogUserSelectPatternCancel
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogUserSelectPatternCanc_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogUserSelectPatternCanc_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogPCInterface
    call    LogDateTime

    movlw   upper TableLogPCInterface ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogPCInterface
    movwf   TBLPTRH
    movlw   low TableLogPCInterface
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogPCInterface_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogPCInterface_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogReadLog
    call    LogDateTime

    movlw   upper TableLogReadLog ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogReadLog
    movwf   TBLPTRH
    movlw   low TableLogReadLog
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogReadLog_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogReadLog_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogCalibrateDispenser
    call    LogDateTime

    movlw   upper TableLogCalibrateDispenser ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogCalibrateDispenser
    movwf   TBLPTRH
    movlw   low TableLogCalibrateDispenser
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogCalibrateDispenser_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogCalibrateDispenser_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return


LogCalibrateTurntable
    call    LogDateTime

    movlw   upper TableLogCalibrateTurntable ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogCalibrateTurntable
    movwf   TBLPTRH
    movlw   low TableLogCalibrateTurntable
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogCalibrateTurntable_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogCalibrateTurntable_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return


LogCalibrateCounting
    call    LogDateTime

    movlw   upper TableLogCalibrateCounting ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogCalibrateCounting
    movwf   TBLPTRH
    movlw   low TableLogCalibrateCounting
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogCalibrateCounting_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogCalibrateCounting_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogTestCounting
    call    LogDateTime

    movlw   upper TableLogTestCounting ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogTestCounting
    movwf   TBLPTRH
    movlw   low TableLogTestCounting
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogTestCounting_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogTestCounting_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogTestVibrator
    call    LogDateTime

    movlw   upper TableLogTestVibrator ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogTestVibrator
    movwf   TBLPTRH
    movlw   low TableLogTestVibrator
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogTestVibrator_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogTestVibrator_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogTestTurntable
    call    LogDateTime

    movlw   upper TableLogCalibClosing ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogCalibClosing
    movwf   TBLPTRH
    movlw   low TableLogCalibClosing
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogTestTurntable_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogTestTurntable_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogTestDispenser
    call    LogDateTime

    movlw   upper TableLogTestDispenser ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogTestDispenser
    movwf   TBLPTRH
    movlw   low TableLogTestDispenser
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogTestDispenser_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogTestDispenser_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogEStopDeactivated
    call    LogDateTime

    movlw   upper TableLogEStopDeactivated ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogEStopDeactivated
    movwf   TBLPTRH
    movlw   low TableLogEStopDeactivated
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogEStopDeactivated_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogEStopDeactivated_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

LogDispenseStarted
    call    LogDateTime
    movlw   upper TableLogDispenseStarted ; TablePointers to point to TableName
    movwf   TBLPTRU
    movlw   high TableLogDispenseStarted
    movwf   TBLPTRH
    movlw   low TableLogDispenseStarted
    movwf   TBLPTRL
    tblrd*                          ; read first character in table
    movf    TABLAT, W
    LogDispenseStarted_Loop
        writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG
        addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'
        tblrd+*                     ; increment pointer then read
        movf    TABLAT, W
        bnz     LogDispenseStarted_Loop

    movlw       0x0A
    writeEEPROM EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, WREG ;newline
    addwf16     EEPROM_CurrentLocationH, EEPROM_CurrentLocationL, d'1'

    return

   end