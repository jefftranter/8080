        INCLUDE macros.mac

;;;     H19 TERMINAL FIRMWARE
;
;
;       COPYRIGHT 1978 BY HEATH COMPANY
;
;       WRITTEN BY R. N. BORCHARDT AUGUST 1, 1978

;       ASCII CHARACTER EQUIVALENCES
;
NULL    EQU     00000000B       ; NULL
BELL    EQU     00000111B       ; BELL
BS      EQU     00001000B       ; BACKSPACE
HT      EQU     00001001B       ; HORIZONTAL TAB
LF      EQU     00001010B       ; LINE FEED
CR      EQU     00001101B       ; CARRIAGE RETURN
XON     EQU     00010001B       ; DC1 (X-ON)
XOFF    EQU     00010011B       ; DC4 (X-OFF)
CAN     EQU     00011000B       ; CANCEL
ESC     EQU     00011011B       ; ESCAPE
RUBOUT  EQU     01111111B       ; RUBOUT/DELETE

;       I/O PORT EQUIVALENCES
;

;       8250 ACE PORTS
;
AP.RBR  EQU     100Q            ; RECEIVER BUFFER REGISTER PORT

AP.THR  EQU     100Q            ; TRANSMITTER HOLDING REGISTER PORT

AP.IER  EQU     101Q            ; INTERRUPT ENABLE REGISTER PORT
AB.ERDA EQU     00000001B       ; ENABLE RECEIVED DATA AVAILABLE INTERRUPT
AB.ETRE EQU     00000010B       ; ENABLE TRANSMITTER HOLDING REGISTER EMPTY
AB.ERLS EQU     00000100B       ; ENABLE RECEIVER LINE STATUS INTERRUPT
AB.EMS  EQU     00001000B       ; ENABLE MODEM STATUS INTERRUPT

AP.IIR  EQU     102Q
AB.DRAI EQU     100B            ; RECEIVED DATA REGISTER INTERRUPT
AB.TREI EQU     010B            ; TRANSMITTER REGISTER EMPTY INTERRUPT
AB.IIP  EQU     00000001B       ; INVERTED INTERRUPT PENDING
AB.IID  EQU     00000110B       ; INTERRUPT IDENTIFICATION BITS

AP.LCR  EQU     103Q            ; LINE CONTROL REGISTER PORT
AB.5BW  EQU     00000000B       ; FIVE BIT WORD
AB.6BW  EQU     00000001B       ; SIX BIT WORD
AB.7BW  EQU     00000010B       ; SEVEN BIT WORD
AB.8BW  EQU     00000011B       ; EIGHT BIT WORD
AB.2SB  EQU     00000100B       ; TWO STOP BITS
AB.PEN  EQU     00001000B       ; PARITY ENABLE
AB.EPS  EQU     00010000B       ; EVEN PARITY SELECT
AB.SP   EQU     00100000B       ; STICK PARITY
AB.SBRK EQU     01000000B       ; SET BREAK
AB.DLAB EQU     10000000B       ; DIVISOR LATCH ACCESS BIT

AP.MCR  EQU     104Q            ; MODEM CONTROL REGISTER PORT
AB.DTR  EQU     00000001B       ; DATA TERMINAL READY
AB.RTS  EQU     00000010B       ; REQUEST TO SEND
AB.OUT1 EQU     00000100B       ; OUTPUT #1
AB.OUT2 EQU     00001000B       ; OUTPUT #2
AB.LOOP EQU     00010000B       ; LOOP

AP.LSR  EQU     105Q            ; LINE STATUS REGISTER PORT
AB.DR   EQU     00000001B       ; DATA READY
AB.OR   EQU     00000010B       ; OVERRUN ERROR
AB.PE   EQU     00000100B       ; PARITY ERROR
AB.FE   EQU     00001000B       ; FRAMING ERROR
AB.BI   EQU     00010000B       ; BREAK INTERRUPT
AB.THRE EQU     00100000B       ; TRANSMITTER HOLDING REGISTER EMPTY
AB.TSRE EQU     01000000B       ; TRANSMITTER SHIFT REGISTER EMPTY

AP.MSR  EQU     106Q            ; MODEM STATUS REGISTER PORT
AB.DCTS EQU     00000001B       ; DELTA CLEAR TO SEND
AB.DDSR EQU     00000010B       ; DELTA DATA SET READY
AB.DRLS EQU     00001000B       ; DELTA RECEIVE LINE SIGNAL DETECT
AB.CTS  EQU     00010000B       ; CLEAR TO SEND
AB.DSR  EQU     00100000B       ; DATA SET READY
AB.RLSD EQU     10000000B       ; RECEIVED LINE SIGNAL DETECT

AP.DLL  EQU     100Q            ; DIVISOR LATCH LSB

AP.DLM  EQU     101Q            ; DIVISOR LATCH MSB

;       CRT VIDEO CONTROLLER PORTS
;
VP.AR   EQU     140Q            ; VIDEO ADDRESS REGISTER OUTPUT PORT
VA.HD   EQU     1               ; HORIZONTAL DISPLAYED REGISTER
VA.HSP  EQU     2               ; HORIZONTAL SYNC POSITION REGISTER
VA.HSW  EQU     3               ; HORIZONTAL SYNC WIDTH REGISTER
VA.VT   EQU     4               ; VERTICAL TOTAL REGISTER
VA.VTA  EQU     5               ; VERTICAL TOTAL ADJUST REGISTER
VA.VD   EQU     6               ; VERTICAL DISPLAYED REGISTER
VA.VSP  EQU     7               ; VERTICAL SYNC POSITION REGISTER
VA.IM   EQU     8               ; INTERLACE MODE REGISTER
VA.MLSA EQU     9               ; MAXIMUM SCAN LINE ADDRESS REGISTER
VA.CS   EQU     10              ; CURSOR START REGISTER
VB.CBE  EQU     01000000B       ; CURSOR BLINK ENABLE
VB.CBPS EQU     00100000B       ; CURSOR BLINK PERIOD SLOW
VB.CND  EQU     00100000B       ; CURSOR NOT DISPLAYED
VA.CE   EQU     11              ; CURSOR END REGISTER
VA.SAM  EQU     12              ; START ADDRESS (MSB) REGISTER
VA.SAL  EQU     13              ; START ADDRESS (LSB) REGISTER
VA.CAM  EQU     14              ; CURSOR ADDRESS (MSB) REGISTER
VA.CAL  EQU     15              ; CURSOR ADDRESS (LSB) REGISTER
VA.LPM  EQU     16              ; LIGHT PEN ADDRESS (MSB) REGISTER
VA.LPL  EQU     17              ; LIGHT PEN ADDRESS (LSB) REGISTER

VP.REGO EQU     141Q            ; VIDEO REGISTER OUTPUT PORT

VP.REGI EQU     143Q            ; VIDEO REGISTER INPUT PORT
VB.RBD  EQU     00001000B       ; REVERSE VIDEO DISABLE (WHEN ADDED TO CRTC PORT#)
VB.NMI  EQU     00000100B       ; CAUSE NMI (MUST BE ADDED TO CRTC PORT #)

;       KEYBOARD PORTS
;
KP.1    EQU     200Q            ; KEYBOARD PORT #1
KB.CHAR EQU     01111111B       ; KEY VALUE
KB.CTL  EQU     10000000B       ; CONTROL KEY
KB.EX1  EQU     00001011B       ; EXTRA ENCODED KEY #1
KB.EX2  EQU     00001100B       ; EXTRA ENCODED KEY #2

KP.2    EQU     240Q            ; KEYBOARD PORT #1
KB.SHFT EQU     00000001B       ; SHIFT KEY(S)
KB.CPLK EQU     00000010B       ; CAPS LOCK KEY
KB.BRK  EQU     00000100B       ; BREAK KEY
KB.ONLN EQU     00001000B       ; ON-LINE KEY
KB.RPT  EQU     01000000B       ; REPEAT KEY
KB.STB  EQU     10000000B       ; KEYBOARD STROBE

;       MISC. I/O PORTS
;
MP.TICK EQU     300Q            ; TICK PORT

MP.BELL EQU     340Q            ; BELL PORT

MP.PUP1 EQU     000Q            ; POWER UP CONFIGURATION PORT #1
P1.BR   EQU     00001111B       ; BAUD RATE SELECTION SWITCHES
P1.PEN  EQU     00010000B       ; PARITY ENABLE SWITCH
P1.EPS  EQU     00100000B       ; EVEN PARITY SELECT SWITCH
P1.SPS  EQU     01000000B       ; STICK PARITY SELECT SWITCH
P1.FDX  EQU     10000000B       ; FULL DUPLEX SELECT SWITCH

MP.PUP2 EQU     040Q            ; POWER UP CONFIGURATION PORT #2
P2.CBLK EQU     00000001B       ; CURSOR = BLOCK
P2.NOTK EQU     00000010B       ; NO TICK ON KEYBOARD STRIKES
P2.WRAP EQU     00000100B       ; WRAP AROUND TO BEGINNING OF NEXT LINE
P2.ALF  EQU     00001000B       ; AUTO LINE FEED ON CARRIAGE RETURN
P2.NOSC EQU     00010000B       ; SCROLL KEY = NO SCROLL (EX HOLD SCREEN MODE)
P2.VT52 EQU     00100000B       ; VT52 MODE
P2.KPDS EQU     01000000B       ; KEYPAD SHIFTED
P2.50HZ EQU     10000000B       ; 50 HERTZ LINE FREQUENCY

;       INSTRUCTION EQUIVALENCES
;
I.BITA  EQU     11001011B       ; BIT B,R (BYTE A)
I.BITB  EQU     01000000B       ; BIT B,R (BYTE B) LESS BIT AND REGISTER
I.BITHA EQU     11001011B       ; BIT B,(HL) (BYTE A)
I.BITHB EQU     01000110B       ; BIT B,(HL) (BYTE B)
I.IM1A  EQU     11101101B       ; IM1 (BYTE A)
I.IM1B  EQU     01010110B       ; IM1 (BYTE B)
I.NEGA  EQU     11101101B       ; NEG (BYTE A)
I.NEGB  EQU     01000100B       ; NEG (BYTE B)
I.SETA  EQU     11001011B       ; SET B,R (BYTE A)
I.SETB  EQU     11000000B       ; SET B,R (BYTE B)
I.SETHA EQU     11001011B       ; SET B,(HL)  (BYTE A)
I.SETHB EQU     11000110B       ; SET B,(HL)  (BYTE B)
I.SHDA  EQU     11101101B       ; SBC HL,DE (BYTE A)
I.SHDB  EQU     01010010B       ; SBC HL,DE (BYTE B)
I.RESA  EQU     11001011B       ; RES B,R (BYTE A)
I.RESB  EQU     10000000B       ; RES B,R (BYTE B)
I.RESHA EQU     11001011B       ; RES B,(HL)  (BYTE A)
I.RESHB EQU     10000110B       ; RES B,(HL)  (BYTE B)
I.EXAF  EQU     00001000B       ; EX AF,AF'
I.EXX   EQU     11011001B       ; EXX
I.LDEDA EQU     11101101B       ; LD DE,(NN) (BYTE A)
I.LDEDB EQU     01011011B       ; LD DE,(NN) (BYTE B)
I.LDDA  EQU     11101101B       ; LDD (BYTE A)
I.LDDB  EQU     10101000B       ; LDD (BYTE B)
I.LDDRA EQU     11101101B       ; LDDR (BYTE A)
I.LDDRB EQU     10111000B       ; LDDR (BYTE B)
I.LDIA  EQU     11101101B       ; LDI (BYTE A)
I.LDIB  EQU     10100000B       ; LDI (BYTE B)
I.LDIRA EQU     11101101B       ; LDIR (BYTE A)
I.LDIRB EQU     10110000B       ; LDIR (BYTE B)
I.OUT   EQU     11010011B       ; OUTPUT
I.RET   EQU     11001001B       ; RETURN
I.JMP   EQU     11000011B       ; JUMP
I.RETNA EQU     11101101B       ; RETURN FROM NMI (BYTE A)
I.RETNB EQU     01000101B       ; RETURN FROM NMI (BYTE B)

;       BIT INSTRUCTION BIT AND REGISTER DEFINITIONS
;
;       BYTE B OF BIT AND SET OPERATIONS ARE XXBBBRRR.  WHERE XX IS
;       DEFINED BY THE INSTRUCTION, BBB DEFINES THE BIT, AND RRR DEFINES
;       THE REGISTER
;
IB.CPLK EQU     1               ; CAPS LOCK = BIT 1
IB.ESCF EQU     7               ; ESCAPE CODE FLAG = BIT 7
IB.ETRE EQU     1               ; ENABLE TRANSMITTER REGISTER EMPTY INTERRUPT
IB.IFF  EQU     7*8             ; INPUT FIFO FLAG = BIT 7
IB.KCB  EQU     7               ; KEYBOARD CONTROL KEY BIT = BIT 7
IB.KSB  EQU     0*8             ; KEYBOARD SHIFT KEY BIT = BIT 0
IB.HSM  EQU     0*8             ; HOLD SCREEN MODE = BIT 7
IB.RV   EQU     7               ; REVERSE VIDEO MODE = BIT 7
IB.ICM  EQU     6               ; INSERT CHARACTER MODE = BIT 6
IB.KPDA EQU     7*8             ; KEYPAD ALTERNATE MODE = BIT 5
IB.KPDS EQU     6               ; KEYPAD SHIFTED MODE = BIT 4
IB.ONLN EQU     3               ; KEYPAD SHIFTED MODE = BIT 3
IB.BRK  EQU     2*8             ; BREAK KEY = BIT 2
IB.GRPH EQU     1               ; TERMINAL IN GRAPHICS MODE = BIT 1
IB.PWE  EQU     0*8             ; PREVIOUS CHARACTER WAS AN ESCAPE = BIT 0
IB.XOFF EQU     4               ; XOFF SENT TO HOST = BIT 4
IB.XMTG EQU     1*8             ; TRANSMIT MODE = GRAPHICS = BIT 1
IB.XMTR EQU     2*8             ; TRANSMIT MODE = REVERSE VIDEO = BIT 2

IR.A    EQU     111B            ; REGISTER = A
IR.B    EQU     000B            ; REGISTER = B
IR.C    EQU     001B            ; REGISTER = C
IR.D    EQU     010B            ; REGISTER = D
IR.E    EQU     011B            ; REGISTER = E
IR.H    EQU     100B            ; REGISTER = H
IR.L    EQU     101B            ; REGISTER = L

;;      LABEL EQUIVALENCES
;
IFF     EQU     10000000B       ; INPUT FIFO FLAG
ESCF    EQU     10000000B       ; ESCAPE FLAG
SCRL    EQU     00011111B       ; KEYBOARD VALUE FOR SCROLL KEY


;;;     INITIALIZE SYSTEM
;
;       JUMP TO THE ROUTINE WHICH SETS UP ALL THE PORTS, RAM, AND THE CRTC
;
        ORG     0

START   JMP     INIT
        DB      43H+41H+47H     ; ENGINEERING CODE

;;;     MAIN - MAIN CONTROL LOOP
;
;       *MAIN* SETS ITSELF UP AS THE FINAL RETURN ADDRESS FOR ALL ROUTINES
;       AND IS IN CHARGE OF GIVING CONTROL TO THE PROPER ROUTINE WHENEVER
;       THERE IS A CHARACTER PRESENT IN ONE OF THE FIFOS
;
;       THE ORDER OF PRIORITY OF SERVICE IS IN THE ORDER IN WHICH THE
;       FIFOS ARE TESTED (KEYBOARD FIFO = #1, INPUT FIFO = #2, OUTPUT
;       FIFO = #3)
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


MAIN    LXI     H,MAIN          ; SET SELF AS RETURN ADDRESS ON STACK
        PUSH    H

        CALL    FCIF            ; GET CHARACTER FROM INPUT FIFO
        JNC     IFCP            ; IF PRESENT, PROCESS IT

MAIN.N  CALL    FVKF            ; FETCH VALUES FROM KEYBOARD FIFO
        JNC     KCE             ; IF CHARACTER PRESENT, ENCODE IT

        CALL    UCP             ; UPDATE THE CURSOR POSITION

        LDA     MODEA           ; GET MODE FLAGS
        ANI     MA.BRK          ; HAS BREAK BEEN SET?
        CNZ     AKI             ; IF BREAK IS SET, SEE IF WE CAN TURN IT OFF

MAIN1   LDA     MODEI           ; GET CURRENT MODE FLAGS
        ANI     MI.ONLN         ; IS TERMINAL ON-LINE?
        RZ                      ; IF NOT ON LINE

;       ALTERNATE ENTRY POINT FOR SETTING THE XMIT INTERRUPT ONLY
;
MAINA   DI                      ; ELSE, LOCK OUT INTERRUPTS
        LDA     OFC             ; SEE IF OUTPUT FIFO HAS ANY CHARACTERS
        ORA     A
        CPU     Z80
        JR      Z,MAIN2         ; IF NO CHARACTERS TO OUTPUT
        CPU     8080

        IN      AP.IER          ; IF CHARACTERS PRESENT, SEE IF XMIT INT. IS ON
        CPU     Z80
        BIT     IB.ETRE,A
        JR      NZ,MAIN2        ; IF INTERRUPT IS ENABLED
        CPU     8080

        ORI     AB.ETRE         ; ELSE, ENABLE INTERRUPT SO OUTPUT CAN BEGIN
        OUT     AP.IER
MAIN2   EI                      ; ENABLE INTERRUPTS
        RET

;;;     AKI - ACE/KEYBOARD INTERRUPT
;
;       *AKI* IS ACCESSED WHENEVER AN INTERRUPT IS RECEIVED FROM EITHER
;       THE ACE, THE KEYBOARD ENCODER, OR THE KEYBOARD BREAK KEY.  ALL
;       VALID INTERRUPT SOURCES ARE SAMPLES TO ENSURE THAT NO FALSE
;       INTERRUPTS ARE SERVICED.
;
;
;       ENTRY   NONE
;
;       EXIT     NONE
;
;       USES    A',B',C',D',E',H',L',F'

        ERRNZ   $-70Q           ; Z80 MODE ONE INTERRUPT ADDRESS
        CPU     Z80
AKI     EX      AF,AF'          ; EXCHANGE ALL REGISTERS
        EXX
        CPU     8080

AKI1    IN      KP.2            ; READ KEYBOARD PORT #2
        XRI     11110110B       ; INVERT SWITCHES (EXCEPT OFF-LINE)
        MOV     D,A             ; SAVE SWITCH VALUES
        ANI     KB.ONLN         ; MASK FOR ON-LINE ACTIVE
        MOV     C,A             ; SAVE RESULT
        LDA     MODEI           ; GET CURRENT MODE
        ANI     377Q-MI.ONLN    ; CLEAR PREVIOUS ON-LINE STATUS
        ORA     C               ; REPLACE WITH NEW STATUS
;       ERRNZ   KB.ONLN-MI.ONLN ; SWITCH AND MODE FLAG MUST BE THE SAME
        STA     MODEI           ; UPDATE *MODE*
        MOV     A,D             ; GET SWITCH VALUES
        ANI     KB.BRK          ; MASK FOR BREAK SWITCH
        LDA     MODEA           ; GET MORE FLAGS
        JZ      AKI1.5          ; IF NO BREAK KEY

        MOV     B,A             ; SAVE MODE A
        LDA     MODEI           ; GET MODE I
        ANI     MI.KID          ; SEE IF KEYBOARD IS DISABLED
        MOV     A,B             ; (A) = MODE A
        JNZ     AKI1.5          ; IF CAN'T RESPOND TO BREAK

        ORI     MA.BRK          ; SET BREAK FLAG
        STA     MODEA
        JMP     AKI1.3          ; CONTINUE PAST NMI ROUTINE

;;      NMI - NON MASKABLE INTERRUPT USES FOR CRTC HOME POSITION UPDATE
;
;       NMI OUTPUTS THE CURRENT DISPLAY INFORMATION TO THE CRTC WHEN
;       CAUSED BY VERT SYNC.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    NONE

        ERRNZ   $-146Q

NMI     PUSH    PSW             ; SAVE REGISTERS
        PUSH    B
        PUSH    D
        PUSH    H

;       UPDATE VERTICAL DISPLAYED REGISTER
;
        MVI     A,VA.VD         ; SET VERTICAL DISPLAYED ADDRESS
        OUT     VP.AR
        LDA     VI.VD           ; GET CURRENT # OF LINES TO DISPLAY
        OUT     VP.REGO

;       UPDATE CURSOR TYPE REGISTERS
;
        LHLD    VI.CSE          ; GET CURSOR START AND END PARAMETERS
        MVI     A,VA.CS         ; GET CRTC CURSOR START ADDRESS
        OUT     VP.AR
        MOV     A,H             ; OUTPUT CURSOR END ADDRESS
        OUT     VP.REGO
        MVI     A,VA.CE         ; SET CURSOR END ADDRESS
        OUT     VP.AR
        MOV     A,L             ; OUTPUT CURSOR END INFO
        OUT     VP.REGO

;       UPDATE VIDEO HOME ADDRESS
;
        LHLD    VI.SA           ; GET START ADDRESS
        MVI     A,VA.SAM        ; START ADDRESS MSB
        OUT     VP.AR
        MOV     A,H             ; OUTPUT ADDRESS MSB
        OUT     VP.REGO
        MVI     A,VA.SAL        ; START ADDRESS LSB
        OUT     VP.AR
        MOV     A,L             ; OUTPUT ADDRESS LSB
        OUT     VP.REGO

;       UPDATE CURSOR ADDRESS
;
        LHLD    VI.CA           ; GET CURSOR ADDRESS
        MVI     A,VA.CAM        ; CURSOR ADDRESS MSB
        OUT     VP.AR
        MOV     A,H             ; CURSOR MSB
        OUT     VP.REGO
        MVI     A,VA.CAL        ; CURSOR ADDRESS LSB
        OUT     VP.AR
        MOV     A,L             ; CURSOR LSB
        OUT     VP.REGO

;       ALLOW NEXT NMI
;
        POP     H               ; RESTORE ALL REGISTERS
        POP     D
        POP     B
        POP     PSW
        CPU     Z80
        RETN
        CPU     8080

AKI1.3  IN      AP.LCR          ; SET BREAK SIGNAL
        ORI     AB.SBRK
        OUT     AP.LCR
        CPU     Z80
        JR      AKI1.7          ; CONTINUE
        CPU     8080

AKI1.5  ANI     377Q-MA.BRK     ; MAKE SURE BREAK FLAG IS OFF
        STA     MODEA
        IN      AP.LCR          ; CLEAR ANY BREAK
        ANI     377Q-AB.SBRK
        OUT     AP.LCR

AKI1.7  IN      AP.IIR          ; SEE IF ACE WAS SOURCE OF INTERRUPT
        CPI     AB.DRAI         ; SEE IF THERE IS RECEIVED DATA AVAILABLE
        CPU     Z80
        JR      NZ,AKI2         ; IF NOT A DATA AVAILABLE INTERRUPT
        CPU     8080

;       INTERRUPT CAUSED BY DATA AVAILABLE IN ACE RECEIVER BUFFER
;
        IN      AP.RBR          ; INPUT DATA
        ANI     01111111B       ; TOSS PARITY BIT
        JZ      AKI6            ; IF CHARACTER WAS A NULL, EXIT

        CPU     Z80
        BIT     IB.ONLN,C       ; SEE IF TERMINAL IS ON-LINE
        JR      Z,AKI6          ; IF OFF LINE, EXIT
        CPU     8080

        MOV     D,A             ; SAVE INPUT CHARACTER
        LDA     IFC             ; GET INPUT FIFO COUNTER
        CPI     IFMAX-16        ; SEE IF WITHIN 16 BYTES OF OVERFLOW
        CPU     Z80
        JR      C,AKI1.8        ; IF NOT < 16 BYTES LEFT
        CPU     8080

AKI1.75 IN      AP.LSR          ; SEE IF UART CAN TAKE A CHARACTER TO OUTPUT
        ANI     AB.THRE
        CPU     Z80
        JR      Z,AKI1.75
        CPU     8080

        MVI     A,XOFF          ; ELSE, SEND CTL-S
        OUT     AP.THR
        LDA     MODEI           ; GET MODE FLAGS
        ORI     MI.XOFF         ; SET XOFF SENT
        STA     MODEI

AKI1.8  MOV     A,D             ; GET INPUT CHARACTER
        CALL    PCIF            ; ELSE, PUT CHARACTER IN INPUT FIFO
        CPU     Z80
        JR      C,AKI5          ; IF FIFO IS FULL, TOSS CHARACTER AND DING BELL
        JR      AKI6            ; IF FIFO NOT FULL, EXIT WITHOUT WELL
        CPU     8080

AKI2    CPI     AB.TREI         ; SEE IF INTERRUPT WAS FROM XMIT BUFFER EMPTY
        CPU     Z80
        JR      NZ,AKI4         ; IF NOT FROM ACE TRANSMITTER

;       INTERRUPT CAUSED BY ACE TRANSMITTER HOLDING REGISTER EMPTY
;
        BIT     IB.ONLN,C       ; SEE IF ON-LINE
        JR      Z,AKI3          ; IF OFF-LINE, DON'T OUTPUT CHARACTER
        CPU     8080

        CALL    FCOD            ; ELSE, FETCH A CHARACTER FROM THE OUTPUT FIFO
        CPU     Z80
        JR      C,AKI3          ; IF NO CHARACTER IN FIFO
        CPU     8080

        OUT     AP.THR          ; OUTPUT CHARACTER TO ACE
        CPU     Z80
        JR      AKI6            ; EXIT
        CPU     8080

AKI3    IN      AP.IER          ; INPUT ACE INTERRUPT ENABLE REGISTER
        ANI     377Q-AB.ETRE    ; CLEAR TRANSMITTER REGISTER EMPTY INTERRUPT
        OUT     AP.IER
        CPU     Z80
        JR      AKI6            ; EXIT
        CPU     8080

AKI4    IN      KP.2            ; READ SECOND KEYBOARD PORT
        XRI     11110110B       ; INVERT SWITCHES (EXCEPT OFF-LINE)
        MOV     B,A             ; SAVE INPUT
        ANI     KB.STB          ; IS A KEYBOARD STROBE PRESENT?
        CPU     Z80
        JR      Z,AKI6          ; IF NO STROBE
        CPU     8080

;       INTERRUPT CAUSED BY A KEY STRIKE ON THE KEYBOARD
;
        IN      KP.1            ; INPUT KEY VALUE
        MOV     B,A
        IN      KP.2            ; INPUT SECOND PORT
        XRI     11110110B       ; INVERT SWITCHES (EXCEPT OFF LINE)
        MOV     C,A

;       SEE IF CHARACTER IS FROM EXTRA KEY #1.  IF SO, INVERT KEYBOARD DISABLE
;
        MOV     A,B             ; GET KEY VALUE
        CPI     KB.CTL+KB.EX1   ; MUST BE CTL-EX1
        LDA     MODEI           ; GET MODE FLAGS
        CPU     Z80
        JR      NZ,AKI4.3       ; IF NOT CONTROL AND EX1
        CPU     8080

        XRI     MI.KID          ; INVERT KEYBOARD DISABLE FLAG
        STA     MODEI           ; UPDATE FLAGS
        CPU     Z80
        JR      AKI4.4          ; SEE IF TO TICK-ET
        CPU     8080

;       SEE IF KEYBOARD IS DISABLED
;
AKI4.3  ANI     MI.KID          ; MASK FOR KEYBOARD INPUT DISABLED FLAG
        CPU     Z80
        JR      NZ,AKI6         ; IF DISABLED, EXIT
        CPU     8080

;       SEE IF TO TICK UPON KEYBOARD INPUT
;
AKI4.4  LDA     MODEB           ; GET PROPER FLAGS
        ANI     MB.NOTK         ; NO TICK?
        CPU     Z80
        JR      NZ,AKI4.8
        CPU     8080

        OUT     MP.TICK         ; TICK TICKER TO INDICATE KEY STRIKE

;       CAN PUT CHARACTER IN FIFO TO BE PROCESSED BY KCE
;
AKI4.8  LHLD    KBDFP           ; GET KEYBOARD FIFO POINTER
        MOV     A,L             ; GET POINTER LSB
        CPI     KBDFMAX&377Q    ; SEE IF ROOM IN FIFO
        CPU     Z80
        JR      Z,AKI5          ; IF NO ROOM, DING BELL
        CPU     8080

        MOV     M,B             ; PLACE IN FIFO
        INX     H
        MOV     M,C             ; PLACE IN FIFO
        INX     H
        SHLD    KBDFP           ; UPDATE FIFO POINTER
        CPU     Z80
        JR      AKI6
        CPU     8080

AKI5    OUT     MP.BELL         ; DING THE DANG BELL

        CPU     Z80
AKI6    EX      AF,AF'          ; EXCHANGE ALL REGISTERS
        EXX
        CPU     8080
        EI                      ; ENABLE NEW INTERRUPTS
        RET

;;;     KCE - KEYBOARD CHARACTER ENCODER
;
;       *KCE* IS RESPONSIBLE FOR TAKING THE VALUE SUPPLIED BY THE HARDWARE
;       KEYBOARD ENCODER AND DETERMINING IF THE VALUE IS A LEGITIMATE ASCII
;       VALUE.  IF THE VALUE DOES NOT REPRESENT THE ACTUAL ASCII VALUE FOR
;       THE KEY STRUCK, *KCE* FORMS THE CORRECT VALUE.
;
;       THE HARDWARE ENCODER ALSO SIGNALS WHETHER THE 'CONTROL' OR 'SHIFT'
;       KEY WAS PRESSED.  IF THE HARDWARE ENCODER DOES NOT REFLECT THE
;       CONTROL OR SHIFT VALUE FOR THE KEY, *KCE* SETS THE CORRECT VALUE
;       PRIOR TO EXITING.
;
;       *KCE* ALSO PLACES THE ENCODED ASCII VALUE OF THE KEY INTO EITHER
;       THE INPUT OR OUTPUT FIFO.  IF THE KEY STRUCK IS A FUNCTION KEY
;       RATHER THAN AN ALPHABETICAL OR NUMERICAL KEY AND THE CONTROL KEY
;       WAS ALSO STRUCK, THE ENCODED VALUE IS PLACED IN THE INPUT FIFO.
;       ALL OTHER VALUES ARE PLACED IN THE OUTPUT FIFO.
;
;
;       ENTRY   (D,E) = VALUE OF KEY SUPPLIED BY THE HARDWARE ENCODER
;       EXIT    NONE
;       USES    A,B,C,D,E,H,L

KCE     MOV     A,D             ; PLACE KEY VALUE IN ACC
        ANI     01111111B       ; MASK OFF ANY CONTROL BIT

;       ENCODE KEY VALUES 000Q THROUGH 017Q AND 033Q
;
        CPI     033Q            ; CHECK FOR 'ESC' KEY
        CPU     Z80
        JR      Z,KCE3          ; IF KEY WAS THE 'ESC' KEY
        CPU     8080

        CPI     020Q            ; KEY < 020Q ?
        JNC     KCE6            ; IF KEY > 017Q

KCE1    PUSH    D               ; SAVE KEYBOARD VALUES
        LXI     H,KAE1          ; POINT TO ASCII EQUIV.  TABLE #1
        MVI     D,KAE1L         ; (D) = LENGTH OF TABLE
        MVI     E,KAE1W         ; (E) = WIDTH OF TABLE IN BYTES
        CALL    STAB            ; SEARCH TABLE
        POP     D               ; (D,E) = KEYBOARD VALUES
        CPU     Z80
        JR      C,KCE1.5        ; IF NOT ENTRY WAS FOUND
        CPU     8080

        CPI     ESCF+'J'        ; WAS IT THE ERASE KEY?
        CPU     Z80
        JR      NZ,KCE2         ; IF NOT

        BIT     IB.KSB,E        ; ELSE, SEE IF SHIFT KEY ALSO
        JR      Z,KCE2          ; IF NOT SHIFT KEY, JUST SEND AN ERM
        CPU     8080

        LDA     MODEB           ; GET MODE FLAGS
        ANI     MB.ANSI         ; IN ANSI MODE?
        CPU     Z80
        JR      Z,KCE1.4        ; IF NOT IN ANSI MODE

        BIT     IB.KCB,D        ; SEE IF CONTROL KEY WAS DOWN
        JR      Z,KCE1.2        ; IF NO CONTROL KEY
        CPU     8080

        CALL    PSIF            ; CONTROL DOWN, PUT STRING IN INPUT FIFO
        DB      ESC,'[','2','J'+200Q
        RET

KCE1.2  CALL    PSOF            ; NO CONTROL, PUT STRING IN OUTPUT FIFO
        DB      ESC,'[','2','J'+200Q
        RET

KCE1.4  MVI     A,ESCF+'E'      ; ELSE SEND A CLR
        CPU     Z80
        JR      KCE2
        CPU     8080

KCE1.5  MOV     A,D             ; ELSE, PLACE ORIGINAL VALUE BACK IN ACC
        ANI     377Q-KB.CTL     ; NO CONTROL = INPUT FIFO ON CONTROL CODES HERE
        MOV     D,A             ; UPDATE (D)
        CPU     Z80
        JR      KCE3            ; GO PLACE ASCII VALUE IN APPROPRIATE FIFO

;       PLACE ASCII VALUE(S) IN THE APPROPRIATE FIFO
;       IF BIT 7 IS SET IN VALUE, FIRST PLACE AN 'ESC' FOLLOWED BY THE SEVEN LSB
;
KCE2    BIT     IB.ESCF,A       ; SEE IF 'ESC' TO BE SENT BEFORE ALPHA CHARACTER
        JR      Z,KCE3          ; IF NO 'ESC' IS TO BE SENT

        CPU     8080
        ANI     377Q-ESCF       ; REMOVE ESCAPE FLAG
        PUSH    PSW             ; ELSE, SAVE VALUE FROM TABLE
        MVI     A,ESC           ; SET (A) = 'ESC'

;       CHECK FOR 'CONTROL' KEY.  IF STRUCK, PLACE CHARACTERS IN INPUT FIFO
;
        CPU     Z80
        BIT     IB.KCB,D        ; SEE IF CONTROL KEY WAS STRUCK ON KEYBOARD
        JR      Z,KCE4          ; IF CONTROL KEY NOT STRUCK
        CPU     8080

        DI                      ; LOCK OUT OTHER INPUTS (*AKI*)
        CALL    PCIF            ; PUT CHARACTER IN FIFO
        EI                      ; ALLOW OTHER INPUTS FROM *AKI*
        LDA     MODEB           ; GET MODE FLAGS
        ANI     MB.ANSI         ; SEE IF IN ANSI MODE
        CPU     Z80
        JR      Z,KCE2.7        ; IF HEATH MODE IS SELECTED
        CPU     8080

        POP     PSW             ; GET CHARACTER
        PUSH    PSW
        CPI     'P'             ; SEE IF CHAR < P
        MVI     A,'['           ; IF < P, OUTPUT A '['
        CPU     Z80
        JR      C,KCE2.3
        CPU     8080

        MVI     A,'O'           ; ELSE, OUTPUT AN 'O'
KCE2.3  DI
        CALL    PCIF
        EI
KCE2.7  POP     PSW             ; GET ASCII VALUE
        CPU     Z80
KCE3    BIT     IB.KCB,D        ; TEST CONTROL KEY FOR PLACEMENT OF THIS CHARACTER
        JR      Z,KCE5          ; IF CONTROL KEY NOT STRUCK
        CPU     8080

        DI                      ; LOCK OUT OTHER INPUTS
        CALL    PCIF            ; PLACE CHARACTER IN INPUT FIFO
        EI                      ; ALLOW *AKI* INPUTS
        RET

KCE4    CALL    PCOFT           ; PLACE CHARACTER IN OUTPUT FIFO
        LDA     MODEB           ; GET MODE FLAGS
        ANI     MB.ANSI         ; SEE IF IN ANSI MODE
        CPU     Z80
        JR      Z,KCE4.7        ; IF IN HEATH MODE
        CPU     8080

        POP     PSW             ; GET CHARACTER
        PUSH    PSW
        CPI     'P'             ; SEE IF < 'P'
        MVI     A,'['           ; IF < P, OUTPUT A'['
        CPU     Z80
        JR      C,KCE4.3        ; IF < P
        CPU     8080

        MVI     A,'O'           ; ELSE, OUTPUT AN 'O'
KCE4.3  CALL    PCOFT

KCE4.7  POP     PSW             ; GET ASCII CHARACTER
KCE5    JMP     PCOFT           ; PLACE CHARACTER IN OUTPUT FIFO

;       ENCODE KEY VALUES 020Q THRU 034Q (EXCEPT 033Q)
;       THESE VALUES ARE FROM THE 12 KEY NUMERIC PAD
;
KCE6    CPI     035Q            ; KEY < 35Q ?
        JNC     KCE12           ; IF KEY > 34Q

        PUSH    D               ; SAVE KEYBOARD VALUES
        LXI     H,KAE2          ; POINT TO SECOND ASCII EQUIVALENCE TABLE
        MVI     D,KAE2L         ; GET TABLE LENGTH
        MVI     E,KAE2W         ; GET TABLE WIDTH
        CALL    STAB            ; SEARCH TABLE
        POP     D               ; (D,E) = KEYBOARD VALUES

;       CHECK FOR SHIFT KEY AND/OR KEYPAD SHIFT MODE
;
        CPU     Z80
        BIT     IB.KSB,E        ; SHIFT KEY DOWN?
        CPU     8080
        LDA     MODEB           ; GET MODE FLAGS
        CPU     Z80
        JR      Z,KCE7          ; IF NO SHIFT KEY

        BIT     IB.KPDS,A       ; TEST FOR KEYPAD SHIFT MODE
        JR      NZ,KCE9         ; IF SHIFT KEY & SHIFT MODE, DON'T SHIFT!
        JR      KCE8            ; ELSE SHIFT KEY & NO SHIFT MODE, SO SHIFT

KCE7    BIT     IB.KPDS,A       ; TEST FOR KEYPAD SHIFT MODE
        JR      Z,KCE9          ; NO SHIFT KEY & NO SHIFT MODE
        CPU     8080

KCE8    INX     H               ; POINT TO SHIFTED BYTE IN TABLE

KCE9    ANI     MB.KPDA         ; TEST FOR KEYPAD IN ALTERNATE MODE
        CPU     Z80
        JR      Z,KCE10         ; IF NOT IN ALT MODE
        CPU     8080

        INX     H               ; ELSE, POINT TO ALT MODE BYTE
        INX     H
KCE10   XRA     A               ; CLEAR ACC AND GET BYTE FROM TABLE (SET FLAGS)
        ADD     M
        JP      KCE3            ; IF NOT AN ESC FUNCTION, GO PUT IN FIFO
        CPU     8080

        CPI     ESCF+'M'        ; CHECK FOR ESC-M (COULD BE ESC-M OR ESC-?-M)
        CPU     Z80
        JR      Z,KCE10.5
        CPU     8080

        CPI     11100000B       ; CHECK FOR LOWER CASE WITH 'ESC' (ALT MODE CHAR)
        CPU     Z80
        JR      NC,KCE11        ; IF NOT A CURSOR FUNCTION
        CPU     8080

        CPI     ESCF+'N'        ; SEE IF DELETE CHARACTER
        CPU     Z80
        JR      NZ,KE10.07      ; IF NOT THE DC KEY
        CPU     8080

        MOV     B,A             ; ELSE, SAVE CHARACTER
        LDA     MODEB           ; GET MODE FLAGS
        ANI     MB.ANSI         ; SEE IF IN ANSI MODE
        MOV     A,B             ; (A) = CHARACTER
        CPU     Z80
        JR      Z,KE10.07       ; IF NOT IN ANSI MODE

        BIT     IB.KCB,D        ; SEE IF CONTROL KEY PRESSED
        JR      Z,KE10.03       ; IF NO CONTROL KEY
        CPU     8080

        CALL    PSIF            ; ELSE, PUT IN INPUT FIFO
        DB      ESC,'[','P'+200Q
        RET

KE10.03 CALL    PSOF            ; PUT STRING IN OUTPUT BUFFER
        DB      ESC,'[','P'+200Q
        RET

KE10.07 CPI     ESCF+'@'        ; CHECK FOR INSERT CHARACTER CODE
        JNZ     KCE2            ; IF NOT INSERT CHARACTER KEY

        LDA     MODEB           ; GET MODE FLAGS
        ANI     MB.ANSI         ; SEE IF IN ANSI MODE
        CPU     Z80
        JR      Z,KCE10.4       ; IF IN HEATH MODE
        CPU     8080

        LDA     MODEA           ; ELSE GET MODEA FLAGS
        ANI     MA.ICM          ; SEE IF IN ICM
        CPU     Z80
        JR      Z,KCE10.2       ; IF NOT ALREADY IN INSERT MODE

        BIT     IB.KCB,D        ; SEE IF CONTROL KEY WAS DOWN
        JR      Z,KCE10.1       ; IF NOT CONTROL KEY
        CPU     8080

        CALL    PSIF            ; ELSE PUT STRING IN INPUT FIFO
        DB      ESC,'[','4','l'+200Q
        RET

KCE10.1 CALL    PSOF            ; PUT STRING IN OUTPUT FIFO
        DB      ESC,'[','4','l'+200Q
        RET

        CPU     Z80
KCE10.2 BIT     IB.KCB,D        ; CHECK FOR CONTROL KEY
        JR      Z,KCE10.3       ; IF NO CONTROL KEY
        CPU     8080

        CALL    PSIF            ; ELSE PUT STRING IN INPUT FIFO
        DB      ESC,'[','4','h'+200Q
        RET

KCE10.3 CALL    PSOF            ; PUT STRING IN OUTPUT FIFO
        DB      ESC,'[','4','h'+200Q
        RET

KCE10.4 LDA     MODEA           ; GET MODE FLAGS
        ANI     MA.ICM          ; SEE IF ALREADY IN INSERT MODE
        MVI     A,EICSEQ+ESCF   ; ENTER INSERT MODE
        JZ      KCE2            ; IF NOT ALREADY IN INSERT MODE

        MVI     A,XICSEQ+ESCF   ; ELSE, EXIT INSERT MODE
        JMP     KCE2

KCE10.5 MOV     A,D             ; WAS 'M', SEE IF FROM THE 3 KEY (DELETE LINE)
        ANI     01111111B       ; TOSS CONTROL BIT
        CPI     023Q            ; 3 KEY?
        MOV     A,M             ; REPLACE TABLE VALUE IN A
        JZ      KCE2            ; IF FROM 3 KEY GO PLACE 'ESC','M' IN FIFO

;       HAVE A 'KEYPAD ALTERNATE MODE' CHARACTER
;       PLACE AN 'ESC' A '?' (OR '0' IF IN ANSI MODE) AND 7 LSB
;       FROM TABLE IN FIFO
;
KCE11   PUSH    PSW             ; SAVE TABLE CHARACTER
        MVI     A,ESC           ; PLACE 'ESC' IN FIFO
        CALL    PCOFT
        LDA     MODEB           ; GET MODE FLAGS
        ANI     MB.ANSI         ; IN ANSI MODE?
        MVI     A,'?'           ; FOR HEATH
        CPU     Z80
        JR      Z,KCE11.5       ; IF IN HEATH MODE
        CPU     8080

        MVI     A,'O'           ; FOR ANSI
KCE11.5 CALL    PCOFT           ; PLACE IN FIFO
        POP     PSW             ; GET TABLE VALUE BACK
        ANI     01111111B       ; TOSS ESC BIT
        JMP     PCOFT           ; PLACE LAST CHARACTER IN FIFO

;       ENCODE KEY VALUES FOR THE SCROLL KEY (037Q)
;
KCE12   CPI     037Q            ; WAS IT THE SCROLL KEY?
        CPU     Z80
        JR      NZ,KCE13        ; IF NOT THE SCROLL KEY
        CPU     8080

        LXI     H,HSMLC         ; ELSE, POINT TO THE HSM LINE COUNTER
        CPU     Z80
        BIT     IB.KSB,E        ; SEE IF SHIFTED SCROLL
        JR      NZ,KCE12.5      ; IF SHIFTED
        CPU     8080
        INR     M               ; INCREMENT LINE COUNTER FOR ONE MORE LINE
        RET                     ; EXIT

KCE12.5 MVI     M,24            ; SET LINE COUNTER TO 24 LINES
        RET                     ; EXIT

;       AT LAST! SIMPLE ASCII KEYS!!!
;       ENCODE KEY VALUES FOR 040Q THROUGH 177Q
;
        CPU     Z80
KCE13   BIT     IB.KSB,E        ; SHIFT KEY STRUCK?
        JR      Z,KCE13.5       ; IF NO SHIFT
        CPU     8080

        PUSH    D               ; SAVE KEYBOARD VALUES
        LXI     H,KAE3          ; ELSE POINT TO EQUIV TABLE #3 FOR SPEC SHIFTS
        MVI     D,KAE3L         ; SET TABLE LENGTH
        MVI     E,KAE3W         ; SET TABLE WIDTH
        CALL    STAB            ; SEARCH TABLE
        POP     D               ; (D,E) = KEYWORD VALUES

        CPU     Z80
KCE13.5 BIT     IB.CPLK,E       ; TEST FOR 'CAPS LOCK' ON
        JR      Z,KCE14         ; IF CAPS LOCK NOT ON
        CPU     8080

        CPI     'a'             ; TEST FOR < LOWER CASE A
        CPU     Z80
        JR      C,KCE14         ; IF LESS THAN A LOWER CASE CHARACTER
        CPU     8080

        CPI     '{'             ; TEST FOR < LEFT BRACE
        CPU     Z80
        JR      NC,KCE14        ; IF GREATER THAN A LOWER CASE CHARACTER
        CPU     8080

        ANI     11011111B       ; IS LOWER CASE, MAKE IT UPPER CASE

        CPU     Z80
KCE14   BIT     IB.KCB,D        ; TEST FOR CONTROL KEY
        CPU     8080
        JZ      PCOFT           ; IF CONTROL NOT STRUCK

        CPI     '@'             ; NO CONTROL IF < 100Q
        JC      PCOFT           ; IF < 100Q

        CPI     '{'             ; NO CONTROL CODES IF > 172Q
        JNC     PCOFT           ; IF > 172Q

        ANI     00011111B       ; ELSE FORM CONTROL CODE
        JMP     PCOFT           ; GOT PLACE CODE IN FIFO

;;      KEYBOARD TO ASCII EQUIVALENCE TABLES
;

;       *KAE1* IS A LOOK UP TABLE FOR FUNCTION KEY VALUES
;
;       TABLE ENTRIES ARE: KEY VALUE FOLLOWED BY ASCII VALUE. *ESCF* IS USED
;       TO SIGNAL THAT AN ESCAPE CODE IS TO PRECEDE THE SEVEN BIT ASCII VALUE

KAE1
        DB      0,ESCF+'S'      ; KEY VALUE 0 = ESC-S
        DB      1,ESCF+'T'      ; KEY VALUE 1 = ESC-T
        DB      2,ESCF+'U'      ; KEY VALUE 2 = ESC-U
        DB      3,ESCF+'V'      ; KEY VALUE 3 = ESC-V
        DB      4,ESCF+'W'      ; KEY VALUE 4 = ESC-W
        DB      5,ESCF+'J'      ; KEY VALUE 5 = ESC-J
        DB      6,ESCF+'P'      ; KEY VALUE 6 = ESC-P
        DB      7,ESCF+'Q'      ; KEY VALUE 7 = ESC-Q
        DB      16Q,ESCF+'R'    ; KEY VALUE 13Q = ESC-R
KAE1W   EQU     2               ; TABLE IS TWO BYTES WIDE
KAE1L   EQU     ($-KAE1)/KAE1W

;       KAE2 CONTAINS THE ASCII VALUES FOR THE VARIOUS MODES OF
;       THE 12 KEY NUMERIC KEY PAD.  TABLE ENTRIES ARE: KEY VALUE,
;       UNSHIFTED ASCII, SHIFTED ASCII, KEYPAD ALTERNATE MODE ASCII,
;       AND KEYPAD ALTERNATE MODE SHIFTED ASCII.
;
KAE2    EQU     $     
        DB      20Q,'0','0',ESCF+'p',ESCF+'p'           ; p = LOWER CASE P
        DB      21Q,'1',ESCF+'L',ESCF+'q',ESCF+'L'      ; q = LOWER CASE Q
        DB      22Q,'2',ESCF+'B',ESCF+'r',ESCF+'B'      ; R = LOWER CASE R
        DB      23Q,'3',ESCF+'M',ESCF+'s',ESCF+'M'      ; S = LOWER CASE S
        DB      24Q,'4',ESCF+'D',ESCF+'t',ESCF+'D'      ; Y = LOWER CASE T
        DB      25Q,'5',ESCF+'H',ESCF+'u',ESCF+'H'      ; U = LOWER CASE U
        DB      26Q,'6',ESCF+'C',ESCF+'v',ESCF+'C'      ; V = LOWER CASE V
        DB      27Q,'7',ESCF+'@',ESCF+'w',ESCF+'@'      ; W = LOWER CASE W
        DB      30Q,'8',ESCF+'A',ESCF+'x',ESCF+'A'      ; X = LOWER CASE X
        DB      31Q,'9',ESCF+'N',ESCF+'y',ESCF+'N'      ; Y = LOWER CASE Y
        DB      32Q,'.','.',ESCF+'n',ESCF+'n'           ; n = LOWER CASE N
        DB      34Q,CR,CR,ESCF+'M',ESCF+'M'
KAE2W   EQU     5               ; TABLE WIDTH = 5 BYTES
KAE2L   EQU     ($-KAE2)/KAE2W

;       *KAE3* CONTAINS SHIFT VALUES FOR KEYS WHERE THE SHIFTED VALUE OF THE
;       KEY CANNOT BE FORMED BY A NORMAL ASCII BIT SHIFT
;
;       TABLE ENTRIES ARE: UNSHIFTED VALUE FOLLOWED BY THE SHIFTED VALUE


KAE3
        DB      "'",'"'
        DB      '-','_'
        DB      '0',')'
        DB      '2','@'
        DB      '6','^'
        DB      '7','&'
        DB      '8','*'
        DB      '9','('
        DB      ';',':'
        DB      '=','+'
        DB      '[',']'
        DB      '`','~'
        DB      '{','}'
KAE3W   EQU     2               ; TABLE WIDTH = 2 BITS
KAE3L   EQU     ($-KAE3)/KAE3W

;;;     IFCP - INPUT FIFO CHARACTER PROCESSOR
;
;       *IFCP* INTERPRETS THE CHARACTER WHICH HAS BEEN RECEIVED (OR STRUCK
;       ON THE KEYBOARD IF OFF-LINE OR KEYED AS A LOCAL FUNCTION) TO SEE IF
;       IT IS THE SECOND CHARACTER OF AN ESCAPE SEQUENCE, A CONTROL CHARACTER
;       OR A CHARACTER TO BE DISPLAYED.  IF THE CHARACTER IS PART OF AN
;       ESCAPE SEQUENCE WHICH CANNOT BE FOUND IN *ESCTAB* THE ESCAPE SEQUENCE
;       FLAG IS CLEARED AND THE CHARACTER IS DISPLAYED.  IF THE CHARACTER IS
;       A CONTROL CHARACTER, AND CANNOT BE FOUND IN *CTLTAB*, THE CHARACTER IS
;       DISCARDED.  IF THE CHARACTER IS FOUND IN THE APPROPRIATE TABLE, THE
;       PROGRAM COUNTER IS SET TO THE ASSOCIATED ADDRESS.
;
;
;       ENTRY   (A) = CHARACTER FROM INPUT FIFO
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


IFCP    MOV     D,A             ; SAVE CHARACTER
        LDA     MODEI           ; GET MODE FLAGS
        CPU     Z80
        BIT     IB.XOFF,A       ; SEE IF XOFF SENT
        JR      Z,IFCP0.5       ; IF NO XOFF HAS BEEN SENT
        CPU     8080

        MOV     E,A             ; SAVE MODE FLAGS
        LDA     IFC             ; GET INPUT FIFO COUNTER
        CPI     IFMAX-32        ; SEE IF AT LEAST 32 BYTES ARE FREE
        MOV     A,E             ; GET MODE FLAGS BACK
        CPU     Z80
        JR      NC,IFCP0.5      ; IF NOT ENOUGH BYTES OPEN
        CPU     8080

        ANI     255-MI.XOFF     ; RESET XOFF FLAG
        STA     MODEI
        MOV     E,A
        MVI     A,XON           ; SEND XON TO HOST
        CALL    PCOFT
        MOV     A,E             ; GET MODE FLAGS BACK

        CPU     Z80
IFCP0.5 BIT     IB.PWE,A        ; SEE IF PREVIOUS CHARACTER WAS AN ESCAPE CODE
        CPU     8080
        MOV     A,D             ; (A) = CHARACTER
        CPU     Z80
        JR      Z,IFCP1         ; IF CHARACTER IS NOT PART OF AN ESCAPE SEQ.
        CPU     8080

;       CHARACTER IS PART OF AN ESCAPE SEQUENCE
;
        LDA     MODEI           ; GET MODE AGAIN
        ANI     377Q-MI.PWE     ; CLEAR 'PREVIOUS WAS AN ESCAPE' FLAG
        STA     MODEI
        LDA     MODEB           ; GET MODE B FLAGS
        ANI     MB.ANSI         ; SEE IF IN ANSI MODE
        MOV     A,D             ; (A) = CHARACTER
        CPU     Z80
        JR      Z,IFCP0.7       ; IF NOT IN ANSI MODE
        CPU     8080

        LXI     H,AESCT         ; (H,L) = ANSI ESCAPE TABLE
        MVI     D,AESCTL        ; (D) = TABLE LENGTH
        MVI     E,AESCTW        ; (E) = TABLE WIDTH
        CPU     Z80
        JR      IFCP0.9         ; SEARCH TABLE
        CPU     8080

IFCP0.7 LXI     H,ESCTAB        ; (H,L) = HEATH ESCAPE SEQUENCE TABLE
        MVI     D,ESCTABL       ; (D) = TABLE LENGTH
        MVI     E,ESCTABW       ; (E) = TABLE WIDTH

IFCP0.9 CALL    STAB            ; SEARCH TABLE
        CPU     Z80
        JR      NC,IFCP3        ; IF A MATCH WAS FOUND IN THE TABLE
        CPU     8080

        RET                     ; ELSE, FORGET ABOUT THIS CHARACTER AND EXIT

IFCP1   CPI     LF              ; IS CHARACTER A LINE FEED?
        CPU     Z80
        JR      NZ,IFCP1.9      ; IF NOT A LINE FEED
        CPU     8080

;       CHARACTER IS A LINE FEED,  CHECK FOR HOLD SCREEN MODE
;
        LDA     MODEA           ; GET MODEA FLAGS
        ANI     MA.HSM
        JZ      PLFCR           ; IF NOT IN HOLD SCREEN MODE, GO DO LINE FEED

;       CHARACTER IS A LINE FEED AND TERMINAL IS IN THE HOLD SCREEN MODE.
;       THE TERMINAL OPERATION WILL NOT CHANGE UNLESS OR UNTIL THE 24TH
;       LINE IS REACHED.  A 'SCROLL' KEY FROM THE KEYBOARD WILL CAUSE ONE
;       MORE LINE TO BE DISPLAYED.  A SHIFTED 'SCROLL' KEY WILL CASE 24
;       LINES TO BE INPUT AND DISPLAYED AND ALL THE CHARACTERS UP TO THE NEXT LINE
;       FEED WILL BE DISPLAYED.
;
        LDA     CURVP           ; GET VERTICAL POSITION
        CPI     23              ; SEE IF ON LAST LINE
        JNZ     PLFCR           ; IF NOT LAST LINE, DON'T HOLD YET

        LXI     H,HSMLC         ; POINT TO HOLD SCREEN MODE LINE COUNTER
        DCR     M               ; DECREMENT COUNTER
        JNZ     PLFCR           ; IF NOT ZERO, DO THIS LF AND WAIT FOR NEXT

        MVI     A,XOFF          ; SEND CTL-S TO HOST
        CALL    PCOFT           ; PLACE IN OUTPUT FIFO
        CALL    MAINA           ; TURN ON XMIT INTERRUPT

IFCP1.1 CALL    FVKF            ; GET NEXT VALUE FROM KEYBOARD FIFO
        CPU     Z80
        JR      NC,IFCP1.5      ; IF A KEY WAS STRUCK
        CPU     8080

        CALL    UCP             ; UPDATE CURSOR
IFCP1.3 LDA     IFC             ; SEE IF INPUT FIFO IS FULL
        CPI     IFMAX
        CPU     Z80
        JR      NZ,IFCP1.1      ; IF NOT FULL, CHECK ON KEYBOARD AGAIN
        CPU     8080

        MVI     A,1             ; ELSE SET COUNTER TO INPUT ONE MORE LINE
        STA     HSMLC
        JMP     PLFCR           ; DO THE LINE FEED AND EMPTY THE FIFO

IFCP1.5 MOV     A,D             ; GET KEY VALUE
        ANI     01111111B       ; TOSS THE CONTROL BIT
        CPI     SCRL            ; WAS KEY THE SCROLL KEY?
        CPU     Z80
        JR      Z,IFCP1.7       ; IF SCROLL KEY
        CPU     8080

        CALL    KCE             ; ELSE ENCODE THE ASCII VALUE
        CALL    MAINA           ; SEE IF THERE IS A CHARACTER TO OUTPUT
        CPU     Z80
        JR      IFCP1.3         ; SEE IF KEYSTRIKE FILLED THE INPUT FIFO

IFCP1.7 BIT     IB.KSB,E        ; SEE IF THIS WAS A SHIFTED SCROLL KEY
        CPU     8080
        MVI     A,1             ; SET LINE COUNTER TO ONE
        CPU     Z80
        JR      Z,IFCP1.8       ; IF SCROLL KEY WAS NOT SHIFTED
        CPU     8080

        MVI     A,24            ; ELSE, SET LINE COUNTER TO 24
IFCP1.8 STA     HSMLC
        CALL    PLFCR           ; DO THE LINE FEED
        MVI     A,XON           ; SEND CTL-Q TO HOST FOR SOME CHARACTERS
        JMP     PCOFT

IFCP1.9 CPI     RUBOUT          ; CHECK FOR RUBOUT AND OTHER CONTROL CODES
        CPU     Z80
        JR      Z,IFCP2         ; IF RUBOUT, SEE IF THERE IS A HANDLER
        CPU     8080

        CPI     ' '             ; CHECK FOR OTHER CONTROL CODES
        CPU     Z80
        JR      NC,IFCP4        ; IF NOT A CONTROL CODE
        CPU     8080

;       CHARACTER IS A CONTROL CHARACTER
;
IFCP2   LXI     H,CTLTAB        ; (H,L) = CONTROL CHARACTER TABLE
        MVI     D,CTLTABL       ; (B = TABLE LENGTH
        MVI     E,CTLTABW       ; (C) = TABLE WIDTH
        CALL    STAB            ; SEARCH TABLE
        RC                      ; IF NOT IN TABLE, TOSS IT AND EXIT

;       CHARACTER FOUND IN TABLE, GO TO ASSOCIATED ADDRESS
;
IFCP3   MOV     E,A             ; (E) = ADDRESS LSB
        INX     H
        MOV     D,M             ; (D) = ADDRESS MSB
        XCHG                    ; (H,L) = ADDRESS OF SPECIAL ROUTINE
        LXI     B,MODEB         ; (B,C) = MODEB IN CASE OF A CHANGE
        LXI     D,MODEA         ; (D,E) = MODEA IN CASE OF A CHANGE
        PCHL                    ; (PC) = ROUTINE

IFCP4   LDA     MODEA           ; GET MODE FLAGS
        ANI     MA.GRPH         ; SEE IF IN GRAPHICS MODE
        MOV     A,D             ; (A) = CHARACTER
        CPU     Z80
        JR      Z,IFCP6         ; IF NOT IN GRAPHICS MODE
        CPU     8080

        CPI     136Q            ; IS CHARACTER IN THE GRAPHICS RANGE?
        JC      IFCP6           ; IF NOT IN GRAPHIC RANGE

        CPU     Z80
        JR      NZ,IFCP5        ; IF CHARACTER IS NOT 136Q
        CPU     8080

        MVI     A,177Q          ; ELSE MAKE 136Q A 177Q
        CPU     Z80
        JR      IFCP6           ; DISPLAY IT
        CPU     8080

IFCP5   ANI     00011111B       ; MAKE CHARACTER INTO A GRAPHICS DISPLAY CHAR.

;       CHARACTER IS A DISPLAYABLE CHARACTER
;
IFCP6   MOV     D,A             ; SAVE CHARACTER IN D
        LDA     MODEA           ; GET MODE FLAGS
        ANI     MA.RV           ; MASK FOR REVERSE VIDEO FLAG
        ORA     D               ; ADD FLAG STATUS TO CHARACTER
;       ERRNZ   MA.RV-10000000B ; R.RV MUST BE BIT 7
        PUSH    PSW             ; SAVE CHARACTER
        LDA     MODEA           ; GET MODE AGAIN
        ANI     MA.ICM          ; CHECK FOR INSERT CHARACTER MODE
        CNZ     PIC             ; IF SELECTED, DO AN INSERT CHARACTER

        POP     PSW             ; ELSE, GET CHARACTER AND DISPLAY IT
        LHLD    CURAD           ; GET CURSOR ADDRESS
        MOV     M,A             ; PUT CHARACTER AT CURSOR
        INX     H               ; POINT TO NEXT COLUMN
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        LDA     CURHP           ; GET CURRENT HORIZONTAL POSITION
        CPI     79              ; AT END OF LINE?
        CPU     Z80
        JR      NZ,IFCP7        ; IF NOT AT END OF LINE
        CPU     8080

        LDA     MODEB           ; ELSE, GET MODE FLAGS
        ANI     MB.WRAP         ; SEE IF TO WRAP AROUND

        RZ                      ; IF NO WRAP

;       LDA     CURVP           ; GET LINE NUMBER
;       CPI     23              ; < 22?
;       RNC                     ; IF ON LAST LINE(S)

        CALL    PLF             ; ELSE, LFCR
        JMP     PCR

IFCP7   INR     A               ; ELSE, POINT TO NEXT COLUMN
        STA     CURHP
        SHLD    CURAD           ; UPDATE CURAD

        RET                     ; EXIT

;;      ESCTAB - ESCAPE SEQUENCE TABLE
;
;       *ESCTAB* CONTAINS THE SECOND CHARACTER OF ALL ESCAPE SEQUENCES
;       THE H19 WILL RESPOND TO.
;
;       THE TABLE ENTRIES ARE: THE SECOND CHARACTER OF THE ESCAPE SEQUENCE,
;       FOLLOWED BY THE ADDRESS OF THE ROUTINE WHICH PERFORMS THE
;       REQUESTED OPERATION

ESCTAB  EQU     $
        DB      '<'             ; ESC <
        DW      EAM             ; ENTER ANSI MODE

        DB      ESC             ; ESC ESC
        DW      SPWE            ; SET PREVIOUS WAS AN ESCAPE AGAIN

        DB      '='             ; ESC =
        DW      EKAM            ; ENTER KEYPAD ALTERNATE MODE

        DB      '>'             ; ESC >
        DW      XKAM            ; EXIT KEYPAD ALTERNATE MODE

        DB      '#'             ; ESC #
        DW      XMTP            ; TRANSMIT PAGE

EICSEQ  EQU     '@'             ; ENTER INSERT CHARACTER MODE SEQUENCE
        DB      EICSEQ          ; ESQ @
        DW      EICM            ; GET INSERT CHARACTER MODE

        DB      'A'             ; ESC A
        DW      CUP             ; CURSOR UP

        DB      'B'             ; ESC B
        DW      CDN             ; CURSOR DOWN

        DB      'C'             ; ESC C
        DW      CRT             ; CURSOR RIGHT

        DB      'D'             ; ESC D
        DW      CLFT            ; CURSOR LEFT

        DB      'E'             ; ESC E
        DW      CLR             ; CLEAR DISPLAY

        DB      'F'             ; ESC F
        DW      EGM             ; ENTER GRAPHICS MODE

        DB      'G'             ; ESC G
        DW      XGM             ; EXIT GRAPHICS MODE

        DB      'H'             ; ESC H
        DW      SCH             ; PERFORM CURSOR HOME

        DB      'I'             ; ESC I
        DW      PRLF            ; PERFORM REVERSE LINE FEED

        DB      'J'             ; ESC J
        DW      ERM             ; ERASE TO END OF MEMORY (PAGE)

        DB      'K'             ; ESC K
        DW      EOL             ; ERASE TO END OF LINE

        DB      'L'             ; ESC L
        DW      PIL             ; PERFORM INSERT LINE

        DB      'M'             ; ESC M
        DW      PDL             ; PERFORM DELETE LINE

        DB      'N'             ; ESC N
        DW      PDC             ; PERFORM DELETE CHARACTER

XICSEQ  EQU     'O'             ; EXIT INSERT CHARACTER MODE SEQUENCE
        DB      XICSEQ          ; ESC O
        DW      XICM            ; EXIT INSERT CHARACTER MODE

        DB      'Y'             ; ESC Y
        DW      PCA             ; PERFORM CURSOR ADDRESSING

        DB      'Z'             ; ESC Z
        DW      IDT             ; IDENTIFY TERMINAL

        DB      '['             ; ESC [
        DW      EHSM            ; ENTER HOLD SCREEN MODE

        DB      '\\'            ; ESC backslash
        DW      XHSM            ; EXIT HOLD SCREEN MODE

        DB      ']'             ; ESC ]
        DW      XMT25           ; TRANSMIT 25TH LINE (IF ENABLED)

        DB      'b'             ; ESC b (LOWER CASE B)
        DW      EBD             ; ERASE BEGINNING OF DISPLAY

        DB      'j'             ; ESC j (LOWER CASE J)
        DW      SCP             ; SAVE CURSOR POSITION

        DB      'k'             ; ESC k (LOWER CASE K)
        DW      USCP            ; UNSAVE CURSOR POSITION

        DB      'l'             ; ESC l (LOWER CASE L)
        DW      EEL             ; ERASE ENTIRE LINE

        DB      'n'             ; ESC n (LOWER CASE N)
        DW      CPR             ; CURSOR POSITION REPORT

        DB      'o'             ; ESC o (LOWER CASE O)
        DW      EBL             ; ERASE BEGINNING OF LINE

        DB      'p'             ; ESC p (LOWER CASE P)
        DW      ERVM            ; ENTER REVERSE VIDEO MODE

        DB      'q'             ; ESC q (LOWER CASE Q)
        DW      XRVM            ; EXIT REVERSE VIDEO MODE

        DB      'r'             ; ESC r (LOWER CASE R)
        DW      SBR             ; SET BAUD RATE

        DB      't'             ; ESC t (LOWER CASE T)
        DW      EKSM            ; ENTER KEYPAD SHIFTED MODE)

        DB      'u'             ; ESC u (LOWER CASE U)
        DW      XKSM            ; EXIT KEYPAD SHIFTED MODE)

        DB      'v'             ; ESC v (LOWER CASE V)
        DW      WEOL            ; WRAP AROUND AT END OF LINE

        DB      'w'             ; ESC w (LOWER CASE W)
        DW      DEOL            ; DISCARD AT END OF LINE

        DB      'x'             ; ESC x (LOWER CASE X)
        DW      SMS             ; HEATH SET MODE

        DB      'y'             ; ESC y (LOWER CASE Y)
        DW      RMS             ; HEATH RESET MODE

        DB      'z'             ; ESC z (LOWER CASE Z)
        DW      RAMP            ; RESET ALL MODES TO POWER UP CONFIGURATION

        DB      '{'             ; ESC { (LEFT BRACE)
        DW      EKI             ; ENABLE KEYBOARD INPUT

        DB      '}'             ; ESC } (RIGHT BRACE)
        DW      DKI             ; DISABLE KEYBOARD INPUT

ESCTABW EQU     3               ; TABLE IS THREE BYTES WIDE
ESCTABL EQU     ($-ESCTAB)/ESCTABW

;       AESCT - ANSI ESCAPE TABLE
;
;       *AESCT* CONTAINS THE SECOND CHARACTER OF THE ANSI ESCAPE TABLES
;
;       TABLE ENTRIES ARE THE SECOND CHARACTER FOLLOWED BY THE ADDRESS
;       OF THE ROUTINE FOR THE REQUESTED FUNCTION

AESCT   EQU     $

        DB      '='             ; ESC =
        DW      EKAM            ; ENTER KEYPAD ALTERNATE MODE

        DB      '>'             ; ESC >
        DW      XKAM            ; EXIT KEYPAD ALTERNATE MODE

        DB      'M'             ; ESC M
        DW      PRLF            ; REVERSE INDEX (REVERSE LINE FEED)

        DB      '['             ; ESC [
        DW      ELB             ; ESCAPE LEFT BRACKET

AESCTW  EQU     3               ; TABLE IS THREE BYTES WIDE
AESCTL  EQU     ($-AESCT)/AESCTW

;       CTLTAB - CONTROL CHARACTER TABLE
;
;       *CTLTAB* CONTAINS ALL CONTROL CODES WHICH THE H19 WILL RESPOND TO
;
;       TABLE ENTRIES ARE: THE CONTROL CHARACTER FOLLOWED BY THE ADDRESS
;       OF THE ROUTINE WHICH PERFORMS THE REQUESTED OPERATION

CTLTAB  EQU     $
        DB      BELL            ; BELL
        DW      DING            ; DING THE BELL

        DB      BS              ; BACKSPACE
        DW      PBS             ; PERFORM A BACKSPACE
        DB      HT              ; HORIZONTAL TABULATION
        DW      TAB             ; TAB TO NEXT EIGHTH COLUMN

        DB      LF              ; LINE FEED
        DW      PLFCR           ; PERFORM LINE FEED AND/OR CARRIAGE RETURN

        DB      CR              ; CARRIAGE RETURN
        DW      PCRLF           ; PERFORM CARRIAGE RETURN AND/OR LINE FEED

        DB      ESC             ; ESCAPE
        DW      SPWE            ; SET PREVIOUS WAS AN ESCAPE FLAG
CTLTABW EQU     3               ; TABLE IS THREE BYTES WIDE
CTLTABL EQU     ($-CTLTAB)/CTLTABW

;;;     GENERAL USE SUBROUTINES
;
;       *A1M* INPUTS THE PARAMETER STRING AND FINAL CHARACTER FOR THE
;       ESC [ ? SEQUENCE
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    (A) = FINAL CHARACTER
;               (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       USES    A,B,C,H,L,F

A1M     MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RNZ                     ; IF PN ALREADY INPUT, ILLEGAL, EXIT

        CALL    PSD             ; INPUT PARAMETER STRING NOW

;       FINAL CHARACTER MUST BE A LOWER CASE H OR A LOWER CASE L
;
        CPI     'h'             ; FINAL = LOWER CASE H?
        CPU     Z80
        JR      Z,A1SM          ; IF H, IS SET HOME SEQUENCE
        CPU     8080

        CPI     'l'             ; FINAL = LOWER CASE L?
        CPU     Z80
        JR      Z,A1RM          ; IF L, IS RESET MODE SEQUENCE
        CPU     8080

        RET                     ; IF NEITHER, EXIT

;       A1SM - ANSI MODE #1 SET MODE SEQUENCE
;
;       *A1SM* SET THE SPECIFIED MODE(S)
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,H,L,F

A1SM    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RZ                      ; IF NO PN, EXIT
        XCHG
        LXI     B,MODEB         ; (B,C) = MODEB
A1SM1   MOV     A,M             ; GET PN
        CPI     2               ; PN = 2?
        CZ      EHM             ; IF 2, ENTER HEATH MODE

        MOV     A,M             ; GET PN
        CPI     7               ; PN = 7?
        CZ      WEOL            ; IF 7, SET WRAP AROUND AT END OF LINE

        MOV     A,M             ; GET PN
        CPI     'h'             ; PN = FINAL?
        RZ                      ; IF FINAL CHARACTER, EXIT

        INX     H               ; ELSE, POINT TO NEXT PN
        CPU     Z80
        JR      A1SM1           ; DECODE IT
        CPU     8080

;       A1RM - ANSI MODE #1 RESET MODE SEQUENCE
;
;       *A1RM* RESETS THE MODE(S) SPECIFIED BY PN
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSW
;
;       EXIT    NONE
;
;       USES    A,B,C,H,L,F

A1RM    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RZ                      ; IF NO PN, EXIT

        XCHG                    ; (H,L) = PSDW
        LXI     B,MODEB
A1RM1   MOV     A,M
        CPI     7               ; PN = 7?
        CZ      DEOL            ; IF 7, SET DISCARD FAST END OF LINE

        MOV     A,M             ; GET PN
        CPI     'l'             ; PN = FINAL?
        RZ                      ; IF FINAL, EXIT

        INX     H               ; ELSE, POINT TO NEXT PN
        CPU     Z80
        JR      A1RM1
        CPU     8080

;;      A2M - ANSI MODE #2
;
;       *A2M* INPUTS THE PARAMETER STRING AND THE FINAL CHARACTER FOR
;       THE SEQUENCE ESC [ >
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    (A) = FINAL CHARACTER
;               (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       USES    A,B,C,D,E,H,L,F

A2M     MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RNZ                     ; IF ALREADY INPUT, ILLEGAL, EXIT

        CALL    PSD             ; INPUT CHARACTER STRING
        CPI     'h'             ; FINAL = LOWER CASE H?
        CPU     Z80
        JR      Z,A2SM          ; IF H, GO SET MODE
        CPU     8080

        CPI     'l'             ; FINAL - LOWER CASE L?
        CPU     Z80
        JR      Z,A2RM          ; IF L, GO RESET MODE
        CPU     8080

        RET                     ; ELSE, EXIT

;       A2SM = ANSI MODE #2 SET MODE SEQUENCE
;
;       *A2SM* SETS THE MODE(S) SPECIFIED BY PN FOR THE SEQUENCE ESC [ >
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F

A2SM    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RZ                      ; IF NO PN, EXIT

A2SM1   LXI     H,SMST          ; (H,L) = SET MODE SEQUENCE TABLE
        LDAX    D               ; GET PN
        CPI     10              ; < 10?
        RNC                     ; IF OUT OF RANGE OR FINAL

        PUSH    D               ; SAVE PN POINTER
        CALL    SMSB            ; SET MODE
        POP     D
        INX     D               ; POINT TO NEXT PN
        CPU     Z80
        JR      A2SM1
        CPU     8080

;       A2RM - ANSI MODE #2 RESET MODE SEQUENCE
;
;       *A2RM* RESET THE MODE(S) SPECIFIED BY PN FOR THE SEQUENCE ESC [ >
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


A2RM    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RZ                      ; IF NO PN

A2RM1   LXI     H,RMST          ; (H,L) = RESET MODE SEQUENCE TABLE
        LDAX    D               ; GET PN
        CPI     10              ; <9?
        RNC                     ; IF OUT OF RANGE OR FINAL

        PUSH    D               ; SAVE PN POINTER
        CALL    SMSB            ; RESET MODE
        POP     D
        INX     D               ; POINT TO NEXT PN
        CPU     Z80
        JR      A2RM1
        CPU     8080

;;      ACDN - ANSI CURSOR DOWN
;
;       *ACDN* MOVES THE CURSOR TOWARD BOTTOM OF THE DISPLAY THE SPECIFIED
;       NUMBER OF LINES
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


ACDN    MOV     A,B             ; SEE IF THERE WAS A PN INPUT
        ORA     A
        JZ      CDN             ; IF NO PN, DO ONLY ONE LINE

        LDAX    D               ; GET PN
        ORA     A               ; SEE IF ZERO
        JZ      CDN             ; IF ZERO, DEFAULT TO ONE

ACDN1   PUSH    PSW             ; SAVE PN
        CALL    CDN             ; MOVE CURSOR DOWN ONE LINE
        POP     PSW
        DCR     A               ; PN TIMES
        CPU     Z80
        JR      NZ,ACDN1        ; IF NOT DONE
        CPU     8080

        RET

;;      ACLFT - ANSI CURSOR LEFT
;
;       *ACLFT* MOVES THE CURSOR TO THE BEGINNING OF THE LINE PN TIMES
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USED    A,B,D,E,H,L,F


ACLFT   MOV     A,B             ; SEE IF HAD PN
        ORA     A
        JZ      CLFT            ; IF NONE, DEFAULT TO ONE

        LDAX    D               ; GET PN
        ORA     A               ; SEE IF ZERO
        JZ      CLFT

        MOV     B,A             ; SAVE COUNT
ACLFT1  CALL    CLFT            ; CURSOR LEFT ONE COLUMN
        DCR     B
        CPU     Z80
        JR      NZ,ACLFT1       ; TIL DONE
        CPU     8080

        RET

;;      ACPR - ANSI CURSOR POSITION REPORT
;
;       *ACPR* OUTPUTS THE CURRENT CURSOR POSITION IN THE ANSI FORM
;               ** ESC [ Pl ; Pc r **
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


ACPR    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RZ                      ; IF NO PN, ILLEGAL, EXIT

        LDAX    D               ; GET PN
        CPI     6               ; MUST BE 6 FOR ACPR
        RNZ                     ; IF NOT SIX, ILLEGAL, EXIT

        CALL    PSOF            ; OUTPUT FIRST PART OF STRING
        DB      ESC,'['+200Q    ; ESC [
        LDA     CURVP           ; GET CURRENT LINE NUMBER
        INR     A               ; HOME LINE = 1 FOR ANSI
        CALL    EDD             ; ENCODE DECIMAL DIGITS
        MOV     A,D             ; GET TENS DIGIT
        ORA     A               ; SEE IF A LEADING ZERO
        CPU     Z80
        JR      Z,ACPR1         ; IF ZERO DROP IT
        CPU     8080

        ORI     00110000B       ; MAKE BCD INTO ASCII
        CALL    PCOFT           ; ELSE PLACE IN FIFO
ACPR1   MOV     A,E             ; GET ONES DIGIT
        ORI     00110000B       ; MAKE BCD INTO ASCII
        CALL    PCOFT           ; PUT IN FIFO

        CALL    PSOF            ; SEPARATE LINE FROM COLUMN
        DB      ';'+200Q

        LDA     CURHP           ; GET CURRENT COLUMN POSITION
        INR     A               ; HOME COLUMN = 1 FOR ANSI
        CALL    EDD             ; ENCODE DIGITS
        MOV     A,D             ; GET TENS
        ORA     A               ; SEE IF ZERO
        CPU     Z80
        JR      Z,ACPR2         ; IF TENS IS ZERO, FORGET IT
        CPU     8080

        ORI     00110000B       ; MAKE BCD INTO ASCII
        CALL    PCOFT           ; OUTPUT TENS
ACPR2   MOV     A,E             ; GET ONES DIGIT
        ORI     00110000B       ; MAKE BCD INTO ASCII
        CALL    PCOFT           ; OUTPUT ONES
        CALL    PSOF            ; OUTPUT FINAL
        DB      'R'+200Q
        RET

;;      ACRT - ANSI CURSOR RIGHT
;
;       *ACRT* MOVES THE CURSOR ONE COLUMN TOWARD THE END OF THE LINE
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,D,E,H,L,F


ACRT    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        JZ      CRT             ; IF NO PN, DEFAULT TO ONE

        LDAX    D               ; GET PN
        ORA     A               ; SEE IF ZERO
        JZ      CRT             ; IF ZERO, DEFAULT TO ONE

        MOV     B,A             ; SAVE COUNT
ACRT1   CALL    CRT             ; CURSOR RIGHT ONE COLUMN
        DCR     B

        CPU     Z80
        JR      NZ,ACRT1        ; 'TIL DONE
        CPU     8080

        RET

;;      ACUP - ANSI CURSOR UP
;
;       *ACUP* MOVES THE CURSOR TOWARD THE TOP OF THE SCREEN PN TIMES
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


ACUP    MOV     A,B             ; SEE IF A PN WAS INPUT
        ORA     A
        JZ      CUP             ; IF NONE, DEFAULT TO ONE

        LDAX    D               ; GET PN
        ORA     A               ; ZERO?
        JZ      CUP             ; IF ZERO, DEFAULT TO ONE

ACUP1   PUSH    PSW             ; SAVE PSW
        CALL    CUP             ; CURSOR UP ONE LINE
        POP     PSW
        DCR     A               ; COUNT - 1
        CPU     Z80
        JR      NZ,ACUP1        ; 'TIL DONE
        CPU     8080

        RET


;;      APCA - ANSI PERFORM CURSOR ADDRESSING
;
;       *APCA* CHECKS TO SEE THAT THE LINE AND COLUMN VALUES ARE LEGAL
;       AND SETS THE CURSOR TO THE REQUESTED ADDRESS.  IF THE VALUES
;       ARE OUT OF RANGE, THE OLD LINE NUMBER REMAINS AND/OR THE CURSOR
;       GOES TO THE LAST COLUMN ON THE LINE
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


APCA    MOV     A,B             ; SEE IF PS WAS INPUT
        ORA     A
        JZ      SCH             ; IF NO ADDRESS WAS SPECIFIED, PLACE CURSOR HOME

        XCHG                    ; (H,L) = PS WORK AREA
        MOV     A,M             ; GET LINE NUMBER
        ORA     A               ; SEE IF ZERO
        CPU     Z80
        JR      Z,APCA1         ; IF ZERO, THIS IS FIRST LINE
        CPU     8080

        DCR     A               ; ELSE, LINE = LINE#-1
APCA1   CPI     24              ; LINE IN RANGE?
        CPU     Z80
        JR      C,APCA2         ; IF 0 TO 23

        JR      NZ,APCA3        ; IF NOT REQUESTING 25TH LINE
        CPU     8080

        MOV     B,A             ; SAVE LINE NUMBER
        LDA     MODEI           ; GET MODE FLAGS
        ANI     MI.25L          ; 25TH LINE ENABLED?
        CPU     Z80
        JR      Z,APCA3         ; IF 25TH LINE IS OFF
        CPU     8080

        MOV     A,B
APCA2   STA     CURVP           ; SET NEW LINE NUMBER

APCA3   INX     H               ; GET NEXT PN
        MOV     A,M             ; COLUMN NUMBER
        ORA     A               ; ZERO?
        CPU     Z80
        JR      Z,APCA4         ; IF ZERO
        CPU     8080

        DCR     A               ; ELSE COLUMN = COLUMN#-1
APCA4   CPI     80              ; SEE IF IN RANGE
        CPU     Z80
        JR      C,APCA5         ; IF 0 TO 79
        CPU     8080

        MVI     A,79            ; ELSE, SET LAST COLUMN
APCA5   STA     CURHP           ; SET COLUMN POSITION
        JMP     SNCP            ; SET NEW CURSOR POSITION

;;      APDC - ANSI PERFORM DELETE CHARACTER
;
;       *APDC* DELETES THE SPECIFIED NUMBER OF CHARACTERS FROM THE LINE
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


APDC    MOV     A,B             ; SEE IF INPUT PN
        ORA     A
        JZ      PDC             ; IF NO PN, DEFAULT TO ONE

        LDAX    D               ; GET PN
        ORA     A               ; ZERO?
        JZ      PDC             ; IF ZERO, DEFAULT TO ONE

APDC1   PUSH    PSW             ; SAVE PN
        CALL    PDC
        POP     PSW
        DCR     A
        CPU     Z80
        JR      NZ,APDC1        ; 'TIL DONE
        CPU     8080

        RET

;;      APDL - ANSI PERFORM DELETE LINE
;
;       *APDL* DELETES THE SPECIFIED NUMBER OF FOLLOWING LINES
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


APDL    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        JZ      PDL             ; IF NO PN, DEFAULT TO ONE

        LDAX    D               ; GET PN
        ORA     A               ; ZERO?
        JZ      PDL             ; IF ZERO, DEFAULT TO ONE

APDL1   PUSH    PSW             ; SAVE COUNT
        CALL    PDL             ; DELETE ONE LINE
        POP     PSW
        DCR     A               ; COUNT -1
        CPU     Z80
        JR      NZ,APDL1        ; 'TIL DONE
        CPU     8080

        RET

;;      APIL - ANSI PERFORM INSERT LINE
;
;       *APIL* INSERTS THE SPECIFIED NUMBER OF LINES AT THE CURRENT LINE
;       OF THE CURSOR POSITION
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


APIL    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        JZ      PIL             ; IF NO PN, INSERT ONE LINE

        LDAX    D               ; GET PN
        ORA     A               ; ZERO?
        JZ      PIL             ; IF ZERO, DEFAULT TO ONE LINE

APIL1   PUSH    PSW             ; SAVE COUNT
        CALL    PIL             ; INSERT ONE LINE
        POP     PSW
        DCR     A
        CPU     Z80
        JR      NZ,APIL1        ; 'TIL DONE
        CPU     8080

        RET

;;      ARM - ANSI RESET MODE
;
;       *ARM* RESETS THE MODE(S) SPECIFIED BY PN FOR THE SEQUENCE  ESC [ Pn l
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


ARM     MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RZ                      ; IF NO PN, EXIT

        XCHG                    ; (H,L) = PSDW
        LXI     B,MODEB         ; (B,C) = MODEB
ARM1    LXI     D,MODEA         ; (D,E) = MODEA
        MOV     A,M             ; GET PN
        CPI     2               ; PN = 2>
        CZ      EKI             ; IF 2, ENABLE KEYBOARD INPUT

        MOV     A,M             ; GET PN
        CPI     4               ; PN = 4?
        PUSH    H               ; SAVE PN POINTER
        CZ      XICM            ; IF 4, EXIT INSERT CHARACTER MODE
        POP     H

        MOV     A,M             ; GET PN
        CPI     20              ; PN = 20?
        CZ      XACR            ; IF 20, EXIT AUTO CARRIAGE RETURN

        MOV     A,M             ; GET PN
        CPI     'l'             ; SEE IF PN = FINAL
        RZ                      ; IF FINAL

        INX     H               ; ELSE POINT TO NEXT PN
        CPU     Z80
        JR      ARM1
        CPU     8080

;;      ASBR - ANSI SET BAUD RATE
;
;       *ASBR* SETS THE BAUD RATE TO THAT SPECIFIED BY PN
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


ASBR    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        CPU     Z80
        JR      Z,ASBR1         ; IF NON SPECIFIED, DEFAULT 10 110 BAUD
        CPU     8080

        XCHG                    ; (H,L) = PSDW
        MOV     A,M             ; GET PN
        CPI     14              ; IN RANGE?
        RNC                     ; IF NOT, EXIT

ASBR1   JMP                     SBR.

;;      ASGM - ANSI SET GRAPHICS MODE
;
;       *ASGM* SETS OR RESETS THE GRAPHICS MODE AND/OR THE REVERSE VIDEO MODE
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,D,E,H,L,F


ASGM    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        CPU     Z80
        JR      Z,ASGM1.5       ; IF NOT PN, DEFAULT TO REVERSE VIDEO OFF
        CPU     8080

        PUSH    D               ; SAVE PN POINTER

ASGM1   POP     D               ; GET PN POINTER
        LDAX    D               ; GET PN
        INX     D               ; POINT TO NEXT
        PUSH    D               ; SAVE FOR NEXT PASS(S)
        LXI     H,ASGM1         ; SET RETURN ADDRESS
        PUSH    H

ASGM1.5 LXI     H,ASGMT         ; (H,L) = GRAPHIC MODE TABLE ADDRESS
        MVI     D,ASGMTL        ; (D) = TABLE LENGTH
        MVI     E,ASGMTW        ; (E) = TABLE WIDTH
        CALL    STAB            ; SEARCH TABLE FOR PN VALUE
        CPU     Z80
        JR      C,ASGM2         ; IF NOT IN TABLE, MUST BE BAD NO. OR FINAL
        CPU     8080

        INX     H               ; ELSE, IN TABLE, GOT TO ROUTINE
        MOV     H,M             ; GET MSB
        MOV     L,A             ; LSB
        LXI     D,MODEA         ; (D,E) = MODEA FOR ROUTINES
        PCHL

ASGM2   POP     H               ; TOSS FORCED RETURN ADDRESS
        POP     D               ; EVEN STACK
        RET

;       ASGMT - ANSI SET GRAPHICS MODE TABLE
;
;       *ASGMT* CONTAINS THE ADDRESS OF THE ROUTINES WHICH SET
;       THE REQUESTED GRAPHIC OR REVERSE VIDEO MODE

ASGMT   EQU     $

        DB      0               ; ZERO
        DW      XRVM            ; EXIT REVERSE VIDEO MODE

        DB      7               ; SEVEN
        DW      ERVM            ; ENTER REVERSE VIDEO MODE

        DB      10              ; TEN
        DW      EGM             ; ENTER GRAPHICS MODE

        DB      11              ; ELEVEN
        DW      XGM             ; EXIT GRAPHICS MODE

ASGMTW  EQU     3               ; TABLE IS THREE BYTES WIDE
ASGMTL  EQU     ($-ASGMT)/ASGMTW ; TABLE LENGTH

;;      ASM - ANSI SET MODE
;
;       *ASM* SETS THE MODE(S) SPECIFIED BY PN
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F

ASM     MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RZ                      ; IF NO PN

        XCHG                    ; (H,L) = PSDW
        LXI     B,MODEB         ; (B,C) = MODEB
ASM1    LXI     D,MODEA         ; (D,E) = MODEA
        MOV     A,M             ; GET PN
        CPI     2               ; PN = 2?
        CZ      DKI             ; IF 2, DISABLE KEYBOARD INPUT

        MOV     A,M             ; GET PN
        PUSH    H               ; SAVE PN POINTER
        CPI     4               ; PN = 4?
        CZ      EICM            ; IF 4, ENTER INSERT CHARACTER MODE
        POP     H

        MOV     A,M             ; GET PN
        CPI     20              ; PN = 20?
        CZ      EACR            ; IF 20, ENTER AUTO CARRIAGE RETURN

        MOV     A,M             ; GET PN
        CPI     'h'             ; SEE IF PN = FINAL
        RZ

        INX     H               ; ELSE, POINT TO NEXT PN
        CPU     Z80
        JR      ASM1
        CPU     8080

;;      CDN - CURSOR DOWN
;
;       *CDN* MOVES THE CURSOR DOWN ONE LINE ON THE DISPLAY, BUT DOES
;       NOT CAUSE A SCROLL PAST THE LAST LINE.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


CDN     LDA     CURVP           ; GET CURRENT VERTICAL POSITION
        CPI     23              ; ON LAST LINE(S)?
        RNC                     ; IF SO, EXIT

;       JMP     PLF             ; ELSE, DO A LINE FEED
;       ERRNZ   $-PLF

;;      PLF - PERFORM LINE FEED
;
;       *PLF* MOVES THE CURSOR DOWN ONE LINE IN THE DISPLAY MEMORY.  IF
;       THE CURSOR WAS ON THE 23RD LINE, THE DISPLAYED VIDEO IS SCROLLED
;       UP ONE LINE AND THE NEW LINE IS WRITTEN FULL OF SPACES.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


PLF     LDA     CURVP           ; GET VERTICAL POSITION
        CPI     24              ; ON 25TH LINE?
        RZ                      ; IF SO, EXIT WITHOUT ANY ACTION

PLF0.2  LXI     D,80            ; LINES = 80 CHARACTERS
        LHLD    CLSA            ; GET CURRENT LINE STARTING ADDRESS
        DAD     D               ; ADD A LINE
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    CLSA            ; SET NEW LINE VALUE
        LDA     CURVP           ; GET CURSOR VERTICAL POSITION
        CPI     23              ; ON LAST LINE?
        CPU     Z80
        JR      NZ,PLF1         ; IF NOT ON LAST LINE
        CPU     8080

;       SCROLL  VIDEO TO DISPLAY A NEW LINE
;
        LDA     MODEI           ; GET MODE FLAGS
        ANI     MI.25L          ; SEE IF 25TH LINE IS ENABLED
        CPU     Z80
        JR      Z,PLF0.5
        CPU     8080

        PUSH    H               ; SAVE NEW LINE STARTING ADDRESS
        LXI     D,80            ; ADD 80 FOR BEGINNING OF 26TH LINE
        DAD     D
        XCHG                    ; (D,E) = BEGINNING OF 26TH LINE
        POP     H               ; (H,L) = BEGINNING OF 25TH LINE
        PUSH    H               ; SAVE AGAIN FOR LATER
        MVI     B,80/16         ; SET MOD 16 COUNT FOR ONE LINE
        CALL    CPM16           ; COPY LINE
        POP     H               ; (H,L) = BEGINNING OF 25TH LINE

PLF0.5  MVI     B,80/16         ; SET MOD-16 ERASE COUNT TO 5 (80 CHARACTERS)
        CALL    WSVA            ; WRITE 80 SPACES TO VIDEO TO BLANK THIS LINE
        LHLD    SHOME           ; GET OLD HOME POSITION
        LXI     D,80            ; ADD ONE LINE
        DAD     D
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    SHOME           ; UPDATE HOME POSITION
        MOV     A,H             ; SET VALUE OF HOME POSITION FOR CRTC
        ANI     HOMAX/256
        MOV     H,A
        SHLD    VI.SA
        CPU     Z80
        JR      PLF2            ; UPDATE CURSOR ADDRESS
        CPU     8080

PLF1    INR     A               ; ADD ONE TO LINE COUNTER
        STA     CURVP

PLF2    LHLD    CURAD           ; GET CURRENT CURSOR POSITION
        DAD     D               ; ADD ONE LINE
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    CURAD           ; UPDATE CURSOR ADDRESS

        JMP     UCP.            ; CAUSE NMI TO UPDATE SCREEN IN CASE OF SCROLL

;;      PLFCR - PERFORM LINE FEED AND/OR CARRIAGE RETURN
;
;       *PLFCR* PERFORMS A CARRIAGE RETURN PRIOR TO PERFORMING A LINE
;       FEED IF THE AUTO CARRIAGE RETURN FUNCTION IS SELECTED
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


PLFCR   CALL    PLF             ; DO LINE FEED
        LDA     MODEB           ; GET MODE FLAGS
        ANI     MB.ACR          ; SEE IF ACR SELECTED
        JNZ     PCR             ; IF SELECTED

        RET                     ; ELSE, JUST RETURN

;;      PBS - PERFORM BACKSPACE
;
;       CLFT - CURSOR LEFT
;
;       *PBS*-*CLFT* STEPS THE CURSOR ONE POSITION TO THE LEFT, BUT DOES NOT
;       WRAP AROUND TO THE PREVIOUS LINE AFTER REACHING COLUMN ZERO
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,H,L,F


PBS     EQU     $
CLFT    EQU     $
        LDA     CURHP           ; GET CURRENT POSITION ON LINE
        ORA     A               ; SEE IF COLUMN ZERO
        RZ                      ; IF AT BEGINNING OF LINE

        DCR     A               ; ELSE, DECREMENT CURSOR POSITION
        STA     CURHP
        LHLD    CURAD           ; GET CURSOR ADDRESS
        DCX     H               ; DECREMENT ADDRESS
PBS1    MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    CURAD

        RET                     ; EXIT WITH OR WITHOUT

;;      CPR - CURSOR POSITION REPORT
;
;       *CPR* OUTPUTS THE CURSOR POSITION IN THE HEATH FORMAT
;               ** ESC Y Pl Pc **
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,H,L,F

CPR     CALL    PSOF            ; OUTPUT FIRST PART OF REPORT
        DB      ESC,'Y'+200Q    ; ESC Y
        LDA     CURVP           ; GET CURRENT LINE NUMBER
        ADI     40Q             ; MAKE ASCII
        CALL    PCOFT           ; PLACE LINE NUMBER IN FIFO
        LDA     CURHP           ; GET CURRENT COLUMN NUMBER
        ADI     40Q
        CALL    PCOFT           ; PUT IN FIFO
        RET

;;      CRT - CURSOR RIGHT
;
;       *CRT* MOVES THE CURSOR RIGHT ONE COLUMN.  IF ALREADY AT THE LAST
;       COLUMN, THE CURSOR IS ADVANCED TO THE BEGINNING OF THE NEXT LINE.
;       *CRT* WILL NOT ADVANCE THE CURSOR PAST COLUMN 79 OF LINE 23.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,H,L,F


CRT     LDA     CURHP           ; GET CURRENT HORIZONTAL POSITION
        CPI     79              ; CURSOR AT LAST COLUMN?
        RZ                      ; EXIT IF AT END OF LINE

        INR     A               ; ELSE, INCREMENT CURSOR POSITION
        STA     CURHP
        LHLD    CURAD           ; GET CURSOR ADDRESS
        INX     H               ; INCREMENT ADDRESS
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    CURAD           ; UPDATE CURAD

        RET                     ; RETURN REGARDLESS

;;      CUP - CURSOR UP
;
;       *CUP* MOVES THE CURSOR UP ONE LINE ON THE DISPLAY.  *CUP* WILL
;       NOT MOVE THE CURSOR PAST LINE ZERO
;
;
;       ENTRY   NONE
;
;       EXIT     NONE
;
;       USES    A,B,C,D,E,H,L,F


CUP     LDA     CURVP           ; GET CURRENT VERTICAL POSITION
        ORA     A               ; CHECK FOR LINE ZERO
        RZ                      ; IF LINE ZERO, EXIT WITHOUT MOVING CURSOR

;       JMP     PRLF            ; DO A REVERSE LINE FEED
;       ERRNZ   $-PRLF

;;      PRLF - PERFORM A REVERSE LINE FEED
;
;       *PRLF* MOVES THE CURSOR UP ONE LINE IN THE VIDEO MEMORY.  IF THE
;       CURSOR WAS ALREADY ON THE TOP LINE OF THE DISPLAY, THE DISPLAY IS
;       SCROLLED DOWN AND THE NEW LINE IS FILLED WITH SPACES.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


PRLF    LDA     CURVP           ; GET VERTICAL POSITION
        CPI     24              ; 25TH LINE?
        RZ                      ; IF SO, FORGET RLF

PRLF0.2 LXI     D,-80           ; (D,E) = TWOS COMPLEMENT OF 80
        LHLD    CLSA            ; GET CURRENT LINE ADDRESS
        DAD     D               ; SUBTRACT ONE LINE
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    CLSA            ; UPDATE LINE ADDRESS
        LDA     CURVP           ; GET CURRENT DISPLAYED LINE NUMBER
        ORA     A               ; CHECK FOR LINE ZERO
        CPU     Z80
        JR      NZ,PRLF1        ; IF NOT ON LINE ZERO
        CPU     8080

        LDA     MODEI           ; GET MODE FLAGS
        ANI     MI.25L          ; IS 25TH LINE ON?
        CPU     Z80
        JR      Z,PRLF0.5       ; IF NOT ON
        CPU     8080

;       25TH LINE IS ON, COPY TO 24TH BEFORE DISPLAY IS MOVED
;
        LHLD    SHOME           ; GET HOME ADDRESS
        LXI     D,23*80         ; FIND ADDRESS OF LINE #23
        DAD     D
        XCHG                    ; (D,E) = BEGINNING OF LINE 23
        LXI     H,80            ; FIND ADDRESS OF LINE #24
        DAD     D
        MVI     B,80/16         ; COPY ONE LINE
        CALL    CPM16

;       ERASE TOP LINE
;
PRLF0.5 LHLD    CLSA            ; LINE ZERO ADDRESS TO (H,L) FOR ERASE
        MVI     B,5             ; ERASE LINE (5*16) SPACES
        CALL    WSVA
        LHLD    SHOME           ; GET OLD HOME POSITION
        LXI     D,-80
        DAD     D               ; SUBTRACT ONE LINE
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    SHOME           ; UPDATE HOME POSITION
        MOV     A,H             ; SET HOME POSITION FOR CRTC
        ANI     HOMAX/256
        MOV     H,A
        SHLD    VI.SA
        CPU     Z80
        JR      PRLF2           ; UPDATE CURSOR
        CPU     8080

PRLF1   DCR     A               ; DECREMENT LINE COUNTER
        STA     CURVP

PRLF2   LHLD    CURAD           ; GET CURRENT CURSOR ADDRESS
        DAD     D               ; SUBTRACT ONE LINE
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    CURAD           ; UPDATE CURSOR ADDRESS

        JMP     UCP.            ; EXIT

;;      EID - ERASE IN DISPLAY
;
;       *EID* ERASES THE AMOUNT OF THE SCREEN SPECIFIED BY PN
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


EID     MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        JZ      ERM             ; IF NO PN, ERASE TO END OF SCREEN

        XCHG                    ; (H,L) = PSDW
        MOV     A,M             ; GET PN
        ORA     A               ; ZERO?
        JZ      ERM             ; IF ZERO, ERASE TO END OF PAGE

        DCR     A               ; ONE?
        JZ      EBD             ; IF ONE, ERASE BEGINNING OF DISPLAY

        DCR     A               ; TWO?
        RNZ                     ; IF NOT TWO, EXIT

;       JMP     CLR             ; ELSE, CLEAR DISPLAY
;       ERRNZ   $-CLR

;;      CLR - CLEAR
;
;       *CLR* PLACES THE SOFTWARE HOME POSITION BACK TO THE HARDWARE
;       HOME POSITION (BEGINNING OF VIDEO RAM) AND WRITES ASCII SPACES
;       INTO THE FIRST 1920 BYTES (24 LINES X 80 CHARACTERS)
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


CLR     LDA     CURVP           ; GET VERTICAL POSITION
        CPI     24              ; SEE IF ON 25TH LINE
        JZ      EEL             ; IF SO, JUST ERASE LINE

        CALL    SCH             ; SET CURSOR TO HOME POSITION (H,L) = CURAD
        MVI     B,1920/16       ; ERASE 24 LINES WORTH OF CHARACTERS
        JMP     WSVA

;;      CPM16 - COPY MEMORY - MODULO SIXTEEN
;
;       *CPM16* COPIES SIXTEEN BYTES FROM AND TO VIDEO RAM.  FROM AND TO
;       ADDRESSES MUST BE AT THE BEGINNING OF A LINE/
;
;
;       ENTRY   (H,L) = ADDRESS TO COPY FROM
;               (D,E) = ADDRESS TO COPY TO
;                 (B) = NUMBER OF BYTES TO COPY /16
;
;       EXIT    NONE
;
;       USES    A,B,D,E,H,L,F

CPM16   MOV     A,D             ; KEEP BOTH ADDRESSES IN VIDEO RAM AREA
        ORI     VRAMS/256
        MOV     D,A
        MOV     A,H
        ORI     VRAMS/256
        MOV     H,A

;       COPY SIXTEEN BYTES
;
        MOV     A,M             ; GET BYTE FROM FIRST LINE
        STAX    D               ; PUT IN SAME COLUMN AS OTHER LINE
        INX     H               ; POINT TO NEXT COLUMN ON BOTH LINES
        INX     D

        MOV     A,M             ; *2
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *3
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *4
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *5
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *6
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *7
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *8
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *9
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *10
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *11
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *12
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *13
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *14
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *15
        STAX    D
        INX     H
        INX     D

        MOV     A,M             ; *16
        STAX    D
        INX     H
        INX     D

        CPU     Z80
        DJNZ    CPM16
        CPU     8080
        RET                     ; TIL DONE

;;      D25L - DISABLE 25TH LINE
;
;       *D25L* DISABLES THE DISPLAY OF THE 25TH LINE
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,F


D25L    LDA     MODEI           ; GET MODE FLAGS
        ANI     255-MI.25L      ; CLEAR 25TH LINE FLAG
        STA     MODEI
        MVI     A,24
        STA     VI.VD           ; UPDATE VIDEO DISPLAYED INFO FOR NMI
        RET

;;      DALF - DISABLE AUTO LINE FEED
;
;       *DALF* DISABLES A CARRIAGE RETURN CAUSING AN AUTOMATIC LINE FEED
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


DALF    LDAX    B               ; GET MODE FLAGS
        ANI     255-MB.ALF      ; CLEAR AUTO LINE FEED
        STAX    B
        RET

;;      DC - DISABLE CURSOR
;
;       *DC* INHIBITS THE DISPLAY OF THE CURSOR.  ALL OTHER FUNCTIONS
;       PERFORM AS THEY WOULD IF THE CURSOR WERE ON
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A


DC      LDA     MODEA           ; GET MODE FLAGS
        ORI     MA.CD           ; SET CURSOR DISABLED
        STA     MODEA
        MVI     H,VB.CND        ; CURSOR NOT DISPLAYED
        MVI     L,0             ; SET CURSOR END ADDRESS TO ZERO
        SHLD    VI.CSE
        RET

;;      DEOL - DISCARD AT END OF LINE
;
;       *DEOL* RESETS THE WRAP AROUND AT END OF LINE FLAG.  CHARACTERS
;       INCOMING CHARACTERS PAST COLUMN 79 ARE PLACED IN COLUMN 79 UNTIL
;       A CARRIAGE RETURN IS RECEIVED
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


DEOL    LDAX    B               ; GET MODE FLAGS
        ANI     255-MB.WRAP     ; CLEAR WRAP AROUND FLAG
        STAX    B
        RET

;;      DING - DING BELL
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    NONE


DING    EQU     $
        OUT     MP.BELL
        RET

;;      DKI - DISABLE KEYBOARD INPUT
;
;       *DKI* CAUSES ALL KEYBOARD ENTRIES TO BE IGNORED
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,F


DKI     LDA     MODEI           ; GET PROPER MODE FLAGS
        ORI     MI.KID          ; SET KEYBOARD INPUT DISABLED
        STA     MODEI
        RET

;;      E25L - ENABLE 25TH LINE
;
;       *E25L* ENABLES THE DISPLAY OF THE 25TH LINE ONLY IF IT HAS NOT
;       BEEN PREVIOUSLY ENABLED.  THE 25TH LINE IS CLEARED AT THE TIME
;       IT IS ENABLED.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


E25L    LDA     MODEI           ; GET MODE FLAGS
        MOV     B,A             ; SAVE
        ANI     MI.25L          ; SEE IF 25TH LINE ALREADY ON
        RNZ                     ; IF ON

        MVI     A,MI.25L        ; ELSE, SET FLAG
        ORA     B
        STA     MODEI
        LHLD    SHOME           ; SET CURRENT HOME POSITION
        LXI     D,1920          ; ADD 24 LINE SIZE FOR BEGINNING OF 25TH
        DAD     D               ; (H,L) = STARTING ADDRESS OF LINE 25
        MVI     B,80/16         ; SET MODULO 16 COUNT FOR ERASE
        CALL    WSVA.           ; ERASE LINE

        MVI     A,25            ; SET VERTICAL DISPLAYED INFO TO 25 LINES
        STA     VI.VD
        RET

;;      EACR - ENTER AUTO CARRIAGE RETURN
;
;       *EACR* ENABLES THE TERMINAL TO PERFORM AN AUTOMATIC CARRIAGE
;       RETURN UPON RECEIPT OF A LINE FEED CHARACTER
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


EACR    LDAX    B               ; GET MODE FLAGS
        ORI     MB.ACR          ; SET AUTO CARRIAGE RETURN
        STAX    B
        RET

;;      EALF - ENABLE AUTO LINE FEED
;
;       *EALF* ENABLED THE TERMINAL TO PERFORM AN AUTOMATIC LINE FEED
;       UPON RECEIPT OF A CARRIAGE RETURN
;
;
;       ENTRY   (B, C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


EALF    LDAX    B               ; GET MODE FLAGS
        ORI     MB.ALF
        STAX    B
        RET

;;      EAM - ENTER ANSI MODE
;
;       *EAM* PLACES THE TERMINAL IN THE ANSI MODE FOR ESCAPE CODE.
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


EAM     LDAX    B               ; GET MODE FLAGS
        ORI     MB.ANSI         ; GET ANSI MODE FLAG
        STAX    B
        RET

;;      EBD - ERASE BEGINNING OF DISPLAY
;
;       *EBD* ERASES THE SCREEN FROM 'HOME' TO THE CURRENT CURSOR POSITION
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


EBD     EQU     $
        LDA     CURVP           ; GET VERTICAL POSITION
        CPI     24              ; SEE IF ON 25TH LINE
        CPU     Z80
        JR      Z,EBL           ; IF SO, JUST DO BEGINNING OF THIS LINE
        CPU     8080

;       ERASE ALL LINES ABOVE CURRENT LINE
;
        LDA     CURVP           ; GET CURRENT LINE NUMBER
        ORA     A               ; SEE IF ALREADY ON HOME LINE
        CPU     Z80
        JR      Z,EBD1          ; IF SO, NO FULL LINES TO ERASE
        CPU     8080

        MOV     L,A             ; MULTIPLY LINE COUNT BY 5 FOR MODULO 16 COLUMNS
        MOV     E,A
        MVI     H,0
        MOV     D,A
        DAD     D               ; *2
        DAD     D               ; *3
        DAD     D               ; *4
        DAD     D               ; *5
        MOV     B,L             ; (B) = RESULT
        LHLD    SHOME           ; (H,L) = FIRST ADDRESS TO SPACE
        CALL    WSVA            ; WRITE SPACES

EBD1    EQU     $
;       JMP     EBL             ; ERASE BEGINNING OF CURRENT LINE
;       ERRNZ   $-EBL

;;      EBL - ERASE BEGINNING OF LINE
;
;       *EBL* ERASES FROM THE BEGINNING OF THE CURRENT LINE TO THE CURSOR
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,H,L,D


EBL     LHLD    CLSA            ; GET ADDRESS OF FIRST COLUMN ON THIS LINE
        LDA     CURHP           ; GET COLUMN COUNT
        INR     A               ; ADD ONE FOR THE CURSOR POSITION
        MOV     B,A             ; (B) = COUNT

        MVI     A,' '           ; WRITE SPACES
EBL1    MOV     M,A             ; PUT SPACE IN DISPLAY
        INX     H               ; POINT TO NEXT
        CPU     Z80
        DJNZ    EBL1            ; UNTIL DONE
        CPU     8080

        RET

;;      EC - ENABLE CURSOR
;
;       *EC* ENABLED THE DISPLAY OF THE CURSOR
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,F


EC      LDA     MODEA           ; GET MODE FLAGS
        ANI     255-MA.CD       ; SHOW CURSOR AS NOT DISABLED
        STA     MODEA
        LDA     MODEB           ; GET OTHER MODE FLAGS
        ANI     MB.CBLK         ; BLOCK CURSOR SELECTED?
        JNZ     SBC.            ; IF BLOCK SELECTED, SET IT

        JMP     SUC.            ; ELSE, SET UNDERSCORE CURSOR

;;      EDD - ENCODE DECIMAL DIGITS
;
;       *EDD* ENCODES TWO DECIMAL DIGITS FROM THE BINARY VALUE SUPPLIED.
;       THE DECIMAL DIGITS ARE IN THE FORM OF TWO BCD BYTES (NOT ASCII).
;
;
;       ENTRY   (A) = BINARY VALUE TO BE DECODED
;
;       EXIT    (D) = DECIMAL TENS DIGIT (IN BCD)
;               (E) = DECIMAL ONES DIGIT (IN BCD)
;
;       USES    A,D,E


EDD     MVI     D,0             ; CLEAR DECIMAL COUNTERS
EDD1    CPI     10              ; SEE IF BINARY IS MORE THAN NINE
        CPU     Z80
        JR      C,EDD2          ; IF LESS THAN TEN
        CPU     8080

        SUI     10              ; ELSE, SUBTRACT TEN FROM BINARY
        INR     D               ; ADD ONE TO TENS DIGIT
        CPU     Z80
        JR      EDD1            ; SEE IF MORE TENS
        CPU     8080

EDD2    MOV     E,A             ; LEFTOVERS ARE ONES DIGIT
        RET

;;      EIL - ERASE IN LINE
;
;       *EIL* ERASES THE PORTION OF THE CURRENT LINE AS SPECIFIED BY PN
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;               (D,E) = PSDW
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


EIL     MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        JZ      EOL             ; IF NO PN, ERASE TO END OF LINE

        XCHG                    ; (H,L) = PSDW
        MOV     A,M             ; GET PN
        ORA     A               ; ZERO?
        JZ      EOL             ; IF ZERO, ERASE TO END OF LINE

        DCR     A               ; ONE?
        CPU     Z80
        JR      Z,EBL           ; IF ONE, ERASE BEGINNING OF LINE
        CPU     8080

        DCR     A               ; TWO?
        RNZ                     ; IF NOT TWO, ILLEGAL, EXIT

;       JMP     EEL             ; ELSE, ERASE ENTIRE LINE
;       ERRNZ   *-EEL

;;      EEL - ERASE ENTIRE LINE
;
;       *EEL* WRITE SPACES INTO THE ENTIRE LINE WHERE THE CURSOR RESIDES
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,H,L,F


EEL     LHLD    CLSA            ; START ERASING AT BEGINNING OF CURRENT LINE
        MVI     B,80/16         ; MODULO 16 COUNT FOR NUMBER TO WRITE
        JMP     WSVA            ; WRITE SPACES AND EXIT

;;      EGM - ENTER GRAPHICS MODE
;
;       *EGM* SETS THE GRPAHICS MODE FLAG
;
;
;       ENTRY   (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,F


EGM     XCHG                    ; (H,L) = MODE
        CPU     Z80
        SET     IB.GRPH,(HL)    ; SET GRAPHICS MODE FLAG
        CPU     8080
        RET

;;      EHM - ENTER HEATH MODE
;
;       *EHM* TAKES THE TERMINAL OUT OF THE ANSI MODE AND INTO THE HEATH MODE
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,F


EHM     LDA     MODEB           ; GET MODE FLAGS
        ANI     255-MB.ANSI
        STA     MODEB
        RET

;;      EHSM - ENTER HOLD SCREEN MODE
;
;
;       *EHSM* SETS THE HOLD SCREEN MODE FLAG
;
;
;       ENTRY   (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,F

EHSM    XCHG                    ; (H,L) = MODEA
        CPU     Z80
        SET     IB.HSM,(HL)     ; SET HOLD SCREEN MODE FLAG
        CPU     8080
        MVI     A,1             ; SET LINE COUNTER TO ONE
        STA     HSMLC
        RET

;;      EICM - ENTER INSERT CHARACTER MODE
;
;       *EICM* SETS THE INSERT CHARACTER MODE FLAG
;
;
;       ENTRY   (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,F


EICM    XCHG                    ; (H,L) = MODEA
        CPU     Z80
        SET     IB.ICM,(HL)     ; SET INSERT CHARACTER MODE FLAG
        CPU     8080
        RET

;;      EKAM - ENTER KEYPAD ALTERNATE MODE
;
;       *EKAM* SETS THE KEYPAD ALTERNATE MODE FLAG IN *MODE*
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


EKAM    LDAX    B               ; GET MODEB FLAGS
        ORI     MB.KPDA         ; SET KEYPAD ALTERNATE MODE
        STAX    B
        RET

;;      EKC - ENABLE KEYPAD CLICK
;
;       *EKC* RESETS THE FLAG WHICH INHIBITS KEY CLICK
;
;
;       ENTRY   (BC) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


EKC     LDAX    B               ; GET MODE FLAGS
        ANI     255-MB.NOTK     ; CLEAR NO TICK FLAG
        STAX    B
        RET


;;      EKI - ENABLE KEYBOARD INPUT
;
;       *EKI* RESETS THE FLAG WHICH DISABLES INPUT FROM THE KEYBOARD
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,F


EKI     LDA     MODEI           ; GET MODE FLAGS
        ANI     255-MI.KID      ; TOSS KEYBOARD DISABLE FLAG
        STA     MODEI
        RET

;;      EKSM - ENTER KEYPAD SHIFTED MODE
;
;       *EKSM* SETS THE KEYPAD SHIFTED MODE FLAG
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


EKSM    LDAX    B               ; GET MODEB FLAGS
        ORI     MB.KPDS
        STAX    B
        RET

;;      ELB - ESCAPE LEFT BRACKET
;
;       *ELB: IS A CONTINUATION OF THE ESCAPE SEQUENCE PROCESSING FOR
;       ANSI ESCAPE SEQUENCES.  THE FINAL CHARACTER OF THE SEQUENCE IS
;       DECODED WITH OR WITHOUT A PRECEDING PARAMETER STRING AND
;       CONTROL IS PASSED ON TO THE ASSOCIATED ROUTINE
;
;
;       ENTRY   NONE
;
;       EXIT    TO REQUESTED ROUTINE WITH
;                       (B) = ZERO IF NO PARAMETER STRING WAS INPUT
;
;       USES    A,B,C,D,E,H,L,F


ELB     CALL    PSD             ; DECODE PARAMETER STRING
        PUSH    B               ; SAVE (B)

;       SEARCH TABLE FOR FINAL CHARACTER OF SEQUENCE
;
        MVI     D,ELBTL         ; SET TABLE LENGTH
        MVI     E,ELBTW         ; SET TABLE WIDTH
        LXI     H,ELBT          ; SET TABLE ADDRESS
        CALL    STAB            ; SEARCH TABLE
        POP     B               ; RESTORE (B)
        RC                      ; IF CHARACTER NOT FOUND IN TABLE, EXIT NOW

        INX     H               ; ELSE, GET ADDRESS OF ROUTINE
        MOV     H,M             ; GET MSB
        MOV     L,A             ; GET LSB
        LXI     D,PSDW          ; (D,E) = PSD WORK AREA
        PCHL

;;      ELBT - ESCAPE LEFT BRACKET TABLE
;
;       *ELBT* CONTAINS THE THIRD AND/OR FINAL CHARACTERS OF THE ANSI
;       ESCAPE SEQUENCES


ELBT    EQU    $

        DB     '>'              ; ESC [ >
        DW     A2M              ; ANSI SET MODE #2

        DB     '?'              ; ESC [ ?
        DW     A1M              ; ANSI SET MODE #1

        DB     'A'              ; ESC [ A
        DW     ACUP             ; ANSI CURSOR UP

        DB     'B'              ; ESC [ B
        DW     ACDN             ; ANSI CURSOR DOWN

        DB     'C'              ; ESC [ C
        DW     ACRT             ; ANSI  CURSOR RIGHT

        DB     'D'              ; ESC [ D
        DW     ACLFT            ; ANSI CURSOR LEFT

        DB     'H'              ; ESC [ H
        DW     APCA             ; ANSIR PERFORM CURSOR ADDRESSING

        DB     'J'              ; ESC [ J
        DW     EID              ; ERASE IN DISPLAY

        DB     'K'              ; ESC [ K
        DW     EIL              ; ERASE IN LINE

        DB     'L'              ; ESC [ L
        DW     APIL             ; ANSI PERFORM LINE INSERT

        DB     'M'              ; ESC [ M
        DW     APDL             ; ANSI PERFORM DELETE LINE

        DB     'P'              ; ESC [ P
        DW     APDC             ; ANSI PERFORM DELETE CHARACTER

        DB     'f'              ; ESC [ f         (LOWER CASE F)
        DW     APCA             ; ANSI PERFORM CURSOR ADDRESSING

        DB     'h'              ; ESC [ h         (LOWER CASE H)
        DW     ASM              ; ANSI SET MODE

        DB     'l'              ; ESC [ l         (LOWER CASE L)
        DW     ARM              ; ANSI RESET MODE

        DB     'm'              ; ESC [ m         (LOWER CASE M)
        DW     ASGM             ; ANSI SET GRAPHICS MODE

        DB     'n'              ; ESC [ n         (LOWER CASE N)
        DW     ACPR             ; ANSIR CURSOR POSITION REPORT

        DB     'p'              ; ESC [ p         (LOWER CASE P)
        DW     AXMTP            ; ANSI TRANSMIT PAGE

        DB     'q'              ; ESC [ q         (LOWER CASE Q)
        DW     AXMT25           ; ANSI TRANSMIT 25TH LINE

        DB     'r'              ; ESC [ r         (LOWER CASE R)
        DW     ASBR             ; ANSI SET BAUD RATE

        DB     's'              ; ESC [ s         (LOWER CASE S)
        DW     ASCP             ; ANSI SAVE CURSOR POSITION

        DB     'u'              ; ESC [ u         (LOWER CASE U)
        DW     AUSCP            ; ANSI UNSAVE CURSOR POSITION

        DB     'z'              ; ESC [ z         (LOWER CASE Z)
        DW     ARAMP            ; ANSI RESET ALL MODES TO POWER UP CONFIGURATION

ELBTW   EQU    3                ; TABLE WIDTH IS 3
ELBTL   EQU    ($-ELBT)/ELBTW   ; TABLE LENGTH

;;      EOL - ERASE TO END OF LINE
;
;       *EOL* PLACES SPACES IN VIDEO RAM FROM THE CURRENT CURSOR POSITION
;       TO THE END OF THE CURRENT LINE.  CURSOR POSITION DOES NOT CHANGE.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


EOL     LHLD    CURAD           ; GET CURSOR ADDRESS
        LDA     CURHP           ; GET CURSOR COLUMN POSITION
        CPU     Z80
        NEG                     ; SUBTRACT COLUMN COUNTER FROM 80
        CPU     8080
        ADI     80
        MOV     E,A             ; PLACE COUNT IN D & E
        MVI     D,0
        JMP     WSV             ; WRITE SPACES ON REST OF LINE

;;      ERM - ERASE REST OF MEMORY
;
;       *ERM* ERASES THE SCREEN FROM THE CURRENT CURSOR POSITION TO
;       THE END OF THE SCREE
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


ERM     LDA     CURVP           ; GET CURRENT VERTICAL POSITION
        CPI     24              ; SEE IF ON 25TH LINE
        CPU     Z80
        JR      Z,EOL           ; IF SO, JUST SETTLE FOR END OF THIS LINE
        CPU     8080

        LHLD    SHOME           ; GET HOME ADDRESS
        LXI     D,1919          ; AND DISPLAY SIZE TO FIND END
        DAD     D
        STC                     ; CLEAR CARRY BIT FOR NEXT OPERATION
        CMC
        CPU     Z80
        LD      DE,(CURAD)
        SBC     HL,DE           ; SUBTRACT CURSOR ADDRESS FROM END OF DISPLAY
        CPU     8080
        INX     H               ; ADD ONE TO ERASE CHARACTER AT (CURAD)
        XCHG                    ; (D,E) = COUNT TO END OF DISPLAY
        MOV     A,D             ; MASK TO KEEP COUNT UNDER 2K
        ANI     00000111B
        MOV     D,A
        LHLD    CURAD           ; (H,L) = ADDRESS OF FIRST SPACE TO WRITE
        JMP     WSV             ; WRITE SPACES

;;      ERVM - ENTER REVERSE VIDEO MODE
;
;       *ERVM* SETS THE REVERSE VIDEO MODE FLAG
;
;
;       ENTRY   (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,F


ERVM    EQU     $


        XCHG                    ; (H,L) = MODE
        MOV     A,M             ; GET MODE FLAGS
        ORI     MA.RV+MA.RVP    ; SET REVERSE VIDEO AND RV PRESENT FLAGS
        MOV     M,A
        RET

;;      FCIF - FETCH CHARACTER FROM INPUT FIFO
;
;       *FCIF* IS USED TO FETCH A SINGLE CHARACTER FROM THE INPUT FIFO
;       IF ANY ARE AVAILABLE.
;
;
;       ENTRY   NONE
;
;       EXIT    (A) = CHARACTER IF ONE IS AVAILABLE
;               'C' = SET IF NO CHARACTER
;               'C' = CLEAR IF CHARACTER IN ACCUMULATOR
;
;       USES    A,B,H,L,F


FCIF    LDA     IFC             ; GET INPUT FIFO COUNTER
        ORA     A               ; SEE IF FIFO IS EMPTY
        STC                     ; SET 'C' FOR NO CHARACTER
        RZ                      ; IF NOT CHARACTER IN FIFO, EXIT

        DI                      ; LOCK OUT ANY ENTRIES FOR NOW
        LDA     IFC             ; DECREMENT INPUT FIFO COUNTER
        DCR     A
        STA     IFC
        LDA     IFP             ; GET INPUT FIFO POINTER
        MOV     B,A             ; SAVE VALUE
        LXI     H,INFI          ; POINT TO INPUT FIFO
        ADD     L               ; ADD POINTER
        MOV     L,A
        MOV     A,B             ; INCREMENT POINTER
        INR     A
        ANI     IFCMSK          ; KEEP POINTER IN FIFO
        STA     IFP             ; UPDATE POINTER
        MOV     A,M             ; READ CHARACTER FROM FIFO
        STC                     ; CLEAR 'C'
        CMC
        EI                      ; ALLOW NEW ENTRIES TO FIFO
        RET

;;      FCOF - FETCH CHARACTER FROM OUTPUT FIFO
;
;       *FCOF* FETCHES A CHARACTER FROM THE OUTPUT FIFO IF ANY ARE AVAILABLE
;
;
;       ENTRY   NONE
;
;       EXIT    (A) = CHARACTER IF AVAILABLE
;               'C' = SET IF NO CHARACTERS IN FIFO
;               'C' = CLEAR IF CHARACTER IN ACCUMULATOR
;
;       USES    A,B,H,L,F


FCOD    LDA     OFC             ; GET OUTPUT FIFO COUNTER
        ORA     A               ; SEE IF FIFO IS EMPTY
        STC                     ; SET 'C' FOR NO CHARACTERS
        RZ                      ; IF NO CHARACTERS IN FIFO

        DCR     A               ; DECREMENT COUNTER
        STA     OFC
        LDA     OFP             ; GET OUTPUT FIFO COUNTER
        MOV     B,A             ; SAVE FOR LATER
        LXI     H,OUTF          ; POINT TO OUTPUT FIFO
        ADD     L               ; ADD POINTER
        MOV     L,A
        MOV     A,B             ; INCREMENT POINTER
        INR     A
        ANI     OFCMSK          ; KEEP POINTER IN FIFO
        STA     OFP             ; UPDATE OUTPUT FIFO POINTER
        MOV     A,M             ; READ CHARACTER FROM FIFO
        STC                     ; CLEAR 'C'
        CMC
        RET

;;      FNCP - FETCH NEXT CHARACTER FOR PRIVATE PROCESSING
;
;       FNCP GETS THE NEXT CHARACTER AVAILABLE FROM THE INPUT FIFO.
;       IF NONE ARE AVAILABLE, FNCP CONTINUES TO PROCESS KEYBOARD
;       CHARACTERS AND OUTPUT FIFO CHARACTERS
;
;       ENTRY   NONE
;
;       EXIT    (A) = CHARACTER FROM INPUT FIFO
;               'C' = CLEARED
;
;       USES    A,G


FNCP    PUSH    B               ; SAVE REGISTERS
        PUSH    D
        PUSH    H
FNCP1   CALL    MAIN.N          ; SERVICE KEYBOARD AND OUTPUT FIFO
        CALL     FCIF           ; SEE IF THERE IS A CHARACTER IN THE INPUT FIFO
        CPU      Z80
        JR       C,FNCP1        ; IF NO CHARACTER YET
        CPU      8080

        POP      H              ; ELSE, RESTORE REGISTERS AND EXIT
        POP      D
        POP      B
        RET

;;      FVKF - FETCH VALUE FROM KEYBOARD FIFO
;
;       *FVKF* FETCHES A TWO BYTE VALUE FROM THE KEYBOARD FIFO
;       AND THEN DOES A BUBBLE DOWN ON THE REST OF THE CONTENTS OF THE FIFO
;       AND UPDATES THE KEYBOARD FIFO POINTER
;
;       ENTRY   NONE
;
;       EXIT    (D,E) = TWO BYTE VALUE FROM KEYBOARD IF AVAILABLE
;               'C' = SET IF FIFO WAS EMPTY
;               'C' = CLEAR IF VALUE WAS PLACED IN (D,E)
;
;       USES    A,D,E,F


FVKF    PUSH    B               ; SAVE (B,C,H,L)
        PUSH    H
        DI                      ; INHIBIT ANY NEW ENTRIES WHILE REMOVING OLD
        LHLD    KBDFP           ; (H,L) = KEYBOARD FIFO POINTER
        MOV     A,L             ; CHECK LSB TO SEE IF THERE ARE ANY ENTRIES
        CPI     KBDFMIN&377Q
        STC                     ; SET CARRY IN CASE NO ENTRY
        CPU     Z80
        JR      Z,FVKF1         ; IF FIFO EMPTY
        CPU     8080

        DCX     H               ; ELSE, UPDATE FIFO POINTER
        DCX     H
        SHLD    KBDFP
        LXI     H,KBDF          ; (H,L) = BEGINNING OF FIFO
        MOV     D,M             ; (D) = VALUE FROM IP.KBD1
        INX     H
        MOV     E,M             ; (E) = VALUE FROM IP.KBD2
        INX     H               ; POINT TO NEXT ENTRY
        PUSH    D               ; SAVE KEYBOARD VALUES
        LXI     D,KBDF          ; (D,E) = BEGINNING OF FIFO FOR BUBBLE DOWN
        LXI     B,KBDFL-2       ; (B,C) = NUMBER OF BYTES TO BUBBLE
        CPU     Z80
        LDIR                    ; BUBBLE UNTIL (BC) = 0
        CPU     8080
        STC                     ; CLEAR CARRY BIT TO INDICATE VALUE FOUND
        CMC
        POP     D               ; (D,E) = KEYBOARD VALUES
FVKF1   POP     H               ; RESTORE REGISTERS
        POP     B
        EI
        RET

;;      SBR - SET BAUD RATE
;
;       *SBR* ALLOWS THE BAUD RATE TO BE SENT INDEPENDENT OF THE POWER-UP
;       SWITCH CONFIGURATION
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


SBR     CALL    FNCP            ; FETCH NEXT CHARACTER

;       INPUT CHARACTER MUST BE AN ASCII A,B,C,D,E,F,G OR H.  (A=110, B=150,
;       C=300, D=600, E=1200, F=1800, G=2000, H=2400, I=3600, J=4800, K=7200,
;       L=9600)
;
SBR1    CPI     'A'             ; SEE IF CHARACTER IS IN RANGE
        RC                      ; IF LESS THAN AN 'A'

        CPI     'N'
        RNC                     ; IF GREATER THAN A 'P'

        ANI     P1.BR           ; MASK FOR LOWER BITS

;       ALTERNATE ENTRY POINT FROM *ASBR*
;
SBR.    MOV     B,A             ; SAVE RESULT
        LDA     MODES           ; GET SERIAL I/O MODE
        ANI     377Q-MS.BR      ; TOSS OLD BAUD RATE
        ORA     B               ; REPLACE WITH NEW BAUD RATE
        STA     MODES           ; UPDATE 'IMAGE'
;       JMP     IACE            ; SET ACE TO NEW RATE
;       ERRNZ   $-ACE


;;      IACE - INITIALIZE ACE (UART)
;
;       *IACE* SETS UP THE DEFAULT I/O PARAMETERS ACCORDING TO THE SWITCH
;       POSITIONS ON PORT MP.PUP1
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


IACE    MVI     A,AB.DLAB       ; SET DIVISOR LATCH ACCESS BIT
        OUT     AP.LCR

;       SET BAUD RATE DIVISORS TO DESIRED BAUD RATE
;
        LDA     MODES           ; GET SERIAL I/O MODE (POWER-UP SWITCH #1)
        MOV     C,A             ; SAVE IMAGE
        ANI     MS.BR           ; MASK FOR BAUD RATE SWITCHES
        CPU     Z80
        JR      Z,IACE0.5       ; IF SWITCHES ARE SET TO ZERO KEEP 110 BAUD
        CPU     8080

        DCR     A               ; ELSE, SWITCHES = SWITCHES-1
IACE0.5 MOV     B,A
        RLC                     ; SWITCHES*2 = TABLE VECTOR
        LXI     H,BRTAB         ; POINT TO BAUD RATE DIVISOR TABLE
        ADD     L               ; ADD VECTOR
        MOV     L,A
        MOV     A,M             ; GET DESIRED DIVISOR LSB
        OUT     AP.DLL          ; OUTPUT TO ACE
        INX     H
        MOV     A,M             ; GET DIVISOR MSB
        OUT     AP.DLM          ; OUTPUT TO ACE

;       SET WORD CONFIGURATION
;
        XRA     A               ; CLEAR ACC
        ORA     B               ; SEE IF 110 BAUD
        MVI     B,0             ; SET ONE STOP BIT IF NOT 110 BAUD
        CPU     Z80
        JR      NZ,ACE1         ; IF (B) NOT 110 BAUD
        CPU     8080

        MVI     B,AB.2SB        ; ELSE, SET TWO STOP BITS IN B
ACE1    MOV     A,C             ; GET PARITY CONFIGURATION
        ANI     P1.PEN+P1.EPS+P1.SPS
        RRC                     ; SHIFT INTO POSITION FOR UART
        ORA     B               ; ADD NUMBER OF STOP BITS
        MOV     B,A             ; SAVE RESULT
        ANI     P1.PEN/2        ; SEE IF PARITY WAS ON
        MVI     A,AB.7BW        ; SET SEVEN BIT WORD IF PARITY ON
        CPU     Z80
        JR      NZ,ACE2         ; IF PARITY ON
        CPU     8080

        MVI     A,AB.8BW        ; ELSE, SET AN 8 BIT WORD WITH NO PARITY
ACE2    ORA     B               ; ADD TO STOP BITS AND PARITY SELECT/TYPE
        OUT     AP.LCR          ; OUTPUT WORD SIZE AND PARITY SELECTION TO ACE

        MVI     A,AB.ERDA       ; ENABLE RECEIVED DATA AVAILABLE INTERRUPTS
        OUT     AP.IER
        MVI     A,AB.DTR+AB.RTS ; SET DATA TERMINAL READY
        OUT     AP.MCR
        RET

;;      BRTAB - BAUD RATE DIVISOR TABLE
;
;       *BRTAB* CONTAINS THE ACE DIVISOR LSB FOLLOWED BY THE MSB
;
;       TABLE MUST RESIDE IN ONE PAGE


BRTAB   EQU     $
BR110   DB      209,6           ; 110 BAUD
BR150   DB      0,5             ; 150 BAUD
BR300   DB      123,2           ; 300 BAUD
BR600   DB      64,1            ; 600 BAUD
BR1200  DB      160,0           ; 1200 BAUD
BR1800  DB      107,0           ; 1800 BAUD
BR2000  DB      96,0            ; 2000 BAUD
BR2400  DB      80,0            ; 2400 BAUD
BR3600  DB      53,0            ; 3600 BAUD
BR4800  DB      40,0            ; 4800 BAUD
BR7200  DB      27,0            ; 7200 BAUD
BR9600  DB      20,0            ; 9600 BAUD
BR19.2K DB      10,0            ; 19,200 BAUD
        ERRNZ   $/256-BRTAB/256

;;      ICRT - INITIALIZE CRT CONTROLLER
;
;       *ICRT* SETS THE CRT CONTROLLER FOR AN 80 COLUMN, 24 LINE
;       DISPLAY WITH THE DISPLAY HOME ADDRESS AND THE CURSOR ADDRESS
;       AT *VRAMS*
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,H,L,F


ICRT    EQU     $

        IN      MP.PUP2         ; GET CONFIGURATION SWITCH INFO

        ANI     P2.50HZ
        LXI     H,VPARD50       ; (H,L) = 50 HERTZ VIDEO PARAMETERS
        CPU     Z80
        JR      NZ,ICRT0.5      ; IF SET FOR 50 HZ
        CPU     8080

        LXI     H,VPARD60       ; (H,L) = 60 HERTZ VIDEO PARAMETERS

ICRT0.5 MVI     B,16            ; 16 REGISTERS TO INITIALIZE IN CRTC
        MVI     C,0             ; START WITH REGISTER 0

ICRT1   MOV     A,C             ; GET REGISTER ADDRESS
        OUT     VP.AR           ; SET ADDRESS REGISTER IN CRTC
        MOV     A,M             ; GET DATA FOR CRTC REGISTER
        OUT     VP.REGO         ; OUTPUT TO REGISTER
        INR     C               ; POINT TO NEXT REGISTER
        INX     H               ; POINT TO NEXT REGISTER'S DATA
        CPU     Z80
        DJNZ    ICRT1           ; IF NOT DONE WITH ALL REGISTERS
        CPU     8080

        RET

;;      IDT - IDENTIFY TERMINAL
;
;       *IDT* IDENTIFIES THE TERMINAL AS A DEC VT52 SO THAT EXISTING DEC
;       SOFTWARE WHICH INTERROGATES THE CONSOLE TYPE WILL OPERATE AS IT
;       WOULD WITH A VT52
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,H,L,F

IDT     CALL    PSOF            ; REPLY WITH 'ESC / K'
        DB      ESC,'/','K'+200Q
        RET

;;      ARAMP - ANSI RESET ALL MODES TO POWERUP CONFIGURATION
;
;       *ARAMP* RESETS ALL FLAGS ETC., TO THE CONFIGURATION OF THE
;       POWER UP SWITCHES.
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;
;       EXIT    NONE
;
;       USES    ALL


ARAMP   MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RNZ                     ; IF PN WAS INPUT, ILLEGAL, EXIT

;       JMP     RAMP            ; ELSE, CONTINUE LIKE IN HEATH MODE
;       ERRNZ   $-RAMP

;;      RAMP - RESET ALL MODES TO POWER UP CONFIGURATION
;
;       RAMP PROVIDES THE SAME FUNCTION AS A HARDWARE RESET
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    ALL


RAMP    DI                      ; LOCK OUT THE WORLD
;       JMP     INIT
;       ERRNZ   $-INIT


;;;     INIT - INITIALIZE THE SYSTEM ON POWER UP
;
;       INIT INITIALIZES THE SYSTEM BY CLEARING THE I/O PORTS, INITIALIZING
;       THE SCRATCHPAD, INITIALIZING THE CRT CONTROLLER, INITIALIZING THE
;       ACE SERIAL PORT, AND JUMPING TO THE MAIN CONTROL LOOP


INIT    EQU        $
;       JMP        IRAM         ; INITIALIZE RAM


;;      IRAM - INITIALIZE RAM
;
;       *IRAM* SETS ALL RAM LOCATIONS TO ZERO AND THEN COPIES IN THE
;       DEFAULT DATA FROM PRSTAB
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


IRAM    LXI     H,RAM           ; POINT TO BEGINNING OF SCRATCHPAD
        LXI     D,RAM+1         ; COPY TO NEXT LOCATION
        LXI     B,255           ; COPY 255 TIMES
        MVI     M,0             ; PLACE ZERO IN FOR COPY
        CPU     Z80
        LDIR
        CPU     8080

        LXI     H,KBDF          ; KEYBOARD FIFO
        SHLD    KBDFP
        MVI     A,24            ; SET VIDEO INFORMATION FOR NMI
        STA     VI.VD           ; VIDEO DISPLAYED
        MVI     H,VB.CBE+8      ; FAST BLINKING CURSOR ON LINE 0
        MVI     L,8             ; END ON LINE 8 TOO
        SHLD    VI.CSE

        IN      MP.PUP1         ; INPUT POWER-UP SWITCH #1

        STA     MODES           ; SAVE AS SERIAL I/O MODE

        IN      MP.PUP2         ; INPUT POWER SWITCH #2
        ANI     01111111B       ; TOSS 50HZ BIT

        STA     MODEB           ; SAVE AS SPECIAL SETUP MODE

        IN      MP.PUP2         ; INPUT POWER-UP SWITCH #2
        ANI     377Q-P2.50HZ    ; GET ALL BUT LINE FREQUENCY SWITCHES
        STA     MODEB           ; SAVE AS MODEB FLAGS
        XRA     A               ; CLEAR ALL INTERNAL FLAGS
        STA     MODEI           ; SAVE AS INTERNAL MODE WITH ALL OTHER FLAGS = ZERO

        LXI     SP,RAM+256      ; SET STACK POINTER TO TOP OF RAM

        LXI     H,VRAMS         ; SET HOME, CURRENT LINE, AND CURSOR ADDRESSES
        SHLD    SHOME
        SHLD    CLSA
        SHLD    CURAD
        LXI     D,2048          ; WRITE SPACES TO ALL OF VIDEO RAM
        CALL    WSV

;;      CONTINUE INITIALIZATION OF SYSTEM
;

INIT1   CALL    ICRT            ; INITIALIZE CRTC
        CALL    IACE            ; INITIALIZE ACE
        CALL    EC              ; GET PROPER CURSOR TYPE
        CPU     Z80
        IM      1               ; SET INTERRUPT MODE 1
        CPU     8080
        MVI     A,0             ; CAUSE FIRST NMI
        OUT     VP.AR+VB.NMI
        EI                      ; LET IT ALL BEGIN
        JMP     MAIN            ; GO TO MAIN LOOP

;;      MPY80 - MULTIPLY BY EIGHTY
;
;       MULTIPLY AN 8 BIT NUMBER BY EIGHTY
;
;
;       ENTRY   (A) = MULTIPLICAND
;
;       EXIT    (H,L) = (A)*80
;
;       USES    D,E,H,L,F

MPY80   MOV     L,A             ; VALUE TO MULTIPLY TO (H,L)
        MVI     H,0
        DAD     H               ; *16
        DAD     H
        DAD     H
        DAD     H
        MOV     D,H             ; *5
        MOV     E,L
        DAD     D
        DAD     D
        DAD     D
        DAD     D
        RET


;;      NKC - NO KEYBOARD CLICK
;
;       *NKC* SETS THE FLAG WHICH DISABLES THE KEYBOARD CLICK DURING *AKI*
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


NKC     LDAX    B               ; GET CURRENT FLAGS
        ORI     MB.NOTK         ; SET NO TICK
        STAX    B
        RET

;;      PCA - PERFORM CURSOR ADDRESSING
;
;       *PCA* SETS THE CURSOR LINE AND COLUMN VALUES ACCORDING TO THE
;       NEXT TWO BYTES FROM THE INPUT FIFO.  LINE NUMBER 40Q IS THE TOP
;       LINE OF THE DISPLAY.  COLUMN 40Q IS THE LEFTMOST COLUMN.  AN ILLEGAL
;       LINE NUMBER WILL CAUSE THE CURSOR TO REMAIN ON THE CURRENT LINE.
;       AN ILLEGAL COLUMN NUMBER WILL CAUSE THE CURSOR TO BE PLACED AT
;       THE END OF THE CURRENT LINE.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F

PCA     CALL    FNCP            ; FETCH NEXT INPUT FIFO CHARACTER
        CPI     CAN             ; SEE IF TO CANCEL THIS SEQUENCE
        RZ                      ; IF CANCEL

;       HAVE A LINE NUMBER, PROCESS IT
;
PCA1    CPI     40Q             ; SEE IF LINE NUMBER IN RANGE
        CPU     Z80
        JR      C,PCA2          ; IF LESS THAN LINE ZERO, USE SAME LINE
        CPU     8080

        CPI     40Q+24
        CPU     Z80
        JR      C,PCA1.5        ; IF LINE 23 OR LESS

        JR      NZ,PCA2         ; IF NOT 24, USE SAVE
        CPU     8080

        MOV     B,A             ; ELSE, SAVE LINE NUMBER
        LDA     MODEI           ; GET MODE FLAGS
        ANI     MI.25L          ; SEE IF 25TH LINE IS ENABLED
        CPU     Z80
        JR      Z,PCA2          ; IF LINE 24 REQUESTED AND NOT AVAILABLE
        CPU     8080

        MOV     A,B             ; ELSE, GET LINE 24 VALUE BACK AND USE IT

PCA1.5  SUI     40Q             ; MASK FOR LINE VALUE
        STA     CURVP           ; SAVE NEW VERTICAL POSITION

PCA2    CALL    FNCP            ; FETCH NEXT INPUT FIFO CHARACTER
        CPI     CAN             ; SEE IF TO CANCEL NOW
        CPU     Z80
        JR      Z,PCA6          ; IF TO CANCEL
        CPU     8080

;       HAVE A COLUMN NUMBER
;
PCA3    CPI     40Q             ; SEE IF IN RANGE
        CPU     Z80
        JR      C,PCA4          ; IF LESS THAN COLUMN ZERO
        CPU     8080

        CPI     40Q+80
        CPU     Z80
        JR      C,PCA5          ; IF COLUMN ZERO THRU 79
        CPU     8080

PCA4    MVI     A,40Q+79        ; OUT OF RANGE, SET CURSOR TO END OF LINE
PCA5    SUI     40Q             ; MASK FOR COLUMN VALUE
        STA     CURHP           ; UPDATE COLUMN COUNTER

PCA6    JMP     SNCP            ; SET CURSOR POSITION

;;      PCIF - PLACE CHARACTER IN FIFO
;
;       *PCIF* PLACES A SINGLE CHARACTER INTO THE INPUT FIFO
;
;
;       ENTRY   (A) = CHARACTER
;
;       EXIT    (A) = CHARACTER
;               'C' = SET IF NO ROOM IN FIFO
;
;       USES    A,B,C,H,L,F


PCIF    MOV     C,A             ; SAVE CHARACTER FOR FIFO
        LDA     IFC             ; GET INPUT FIFO CHARACTER
        CPI     IFMAX           ; CHECK FOR FIFO ALREADY FULL
        STC                     ; SET 'C' FOR FIFO FULL
        JZ      DING            ; DING BELL AND EXIT

        MOV     B,A             ; ELSE, SAVE FIFO COUNTER
        LDA     IFP             ; GET INPUT FIFO POINTER
        ADD     B               ; ADD COUNT FOR VECTOR TO NEXT CURSOR ADDRESS
        ANI     IFCMSK          ; KEEP VECTOR IN FIFO
        LXI     H,INFI          ; POINT TO INPUT FIFO
        ADD     L               ; ADD VECTOR
        MOV     L,A
        MOV     M,C             ; PLACE CHARACTER IN FIFO
        MOV     A,B             ; INCREMENT INPUT FIFO COUNTER
        INR     A
        STA     IFC
        STC                     ; CLEAR 'C' TO SHOW THAT CHARACTER WAS PLACED
        CMC
        MOV     A,C             ; CHARACTER TO (A)
        RET

;;      PCOF - PLACE CHARACTER IN OUTPUT FIFO
;
;       *PCOF* PLACES A SINGLE CHARACTER INTO THE OUTPUT FIFO
;
;
;       ENTRY   (A) = CHARACTER
;
;       EXIT    (A) = CHARACTER
;               'C' = SET IF NO ROOM IN FIFO
;
;       USES    A,B,C,H,L,F


PCOF    MOV     C,A             ; SAVE CHARACTER
        LDA     MODEI           ; GET MODE FLAGS
        ANI     MI.ONLN         ; IS TERMINAL ON-LINE?
        CPU     Z80
        JR      NZ,PCOF1        ; IF TERMINAL IS ON-LINE
        CPU     8080

;       TERMINAL IS OFF-LINE, PLACE CHARACTER IN INPUT FIFO INSTEAD
;
        MOV     A,C
        CALL    PCIF            ; PLACE CHARACTER IN INPUT FIFO
        RET

PCOF1   LDA     MODES           ; GET MODE FOR SERIAL I/O
        ANI     MS.FDX          ; IS FULL DUPLEX SELECTED?
        MOV     A,C             ; (A) = CHARACTER
        CZ      PCIF            ; IF HALF DUPLEX, PLACE CHARACTER IN BOTH FIFOS

        LDA     OFC             ; GET OUTPUT FIFO COUNTER
        CPI     OFMAX           ; SEE IF FIFO IS FULL
        STC                     ; SET 'C' FOR FULL
        CPU     Z80
        JR      Z,PCOF2         ; IF FIFO IS ALREADY FULL
        CPU     8080

        MOV     B,A             ; SAVE CURRENT COUNT
        LDA     OFP             ; GET OUTPUT FIFO POINTER
        ADD     B               ; ADD TO COUNT FOR VECTOR TO ADDRESS FOR THIS CHAR
        ANI     OFCMSK          ; KEEP VECTOR IN FIFO
        LXI     H,OUTF          ; POINT TO OUTPUT FIFO
        ADD     L               ; ADD VECTOR
        MOV     L,A
        MOV     M,C             ; PUT CHARACTER IN FIFO
        MOV     A,B             ; INCREMENT OUTPUT FIFO COUNTER
        INR     A
        STA     OFC
        STC                     ; CLEAR 'C' TO SHOW CHARACTER PLACED
        CMC
        MOV     A,C             ; (A) = OUTPUT CHARACTER
PCOF2   RET

;;      PCOFT - PLACE CHARACTER IN OUTPUT FIFO DURING TASK TIME
;
;
;       ENTRY   (A) = CHARACTER
;
;       EXIT    'C' SET IF NO ROOM
;
;       USES    A,B,C,H,L,F


PCOFT   DI                      ; LOCK OUT INTERRUPT CHARACTER REMOVALS
        CALL    PCOF            ; PLACE CHARACTER
        EI                      ; ALLOW INTERRUPTS NOW
        RET

;;      PCRLF - PERFORM CARRIAGE RETURN AND/OR LINE FEED
;
;       *PCRLF* PERFORMS A LINE FEED PRIOR TO PERFORMING A CARRIAGE
;       RETURN IF THE AUTO LINE FEED FUNCTION IS SELECTED
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,H,L,F


PCRLF   LDA     MODEB           ; GET MODE FLAGS
        ANI     MB.ALF          ; SEE IF AUTO LINE FEED IS SELECTED
        CNZ     PLF             ; IF SELECTED
;       JMP     PCR             ; PERFORM CARRIAGE RETURN
;       ERRNZ   $-PCT

;;      PCR - PERFORM CARRIAGE RETURN
;
;       PCR MOVES THE CURSOR TO THE BEGINNING OF THE CURRENT LINE
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,H,L,F


PCR     MVI     A,0             ; SET CURSOR HORIZONTAL POSITION TO ZERO
        STA     CURHP
        LHLD    CLSA            ; SET CURSOR ADDRESS TO BEGINNING OF LINE
        SHLD    CURAD


        RET                     ; EXIT

;;      PDC - PERFORM DELETE CHARACTER
;
;       *PDC* DELETES THE CHARACTER AT THE CURSOR POSITION BY MOVING
;       THE REMAINING CHARACTERS ON THE LINE TO THE LEFT ONE COLUMN AND
;       INSERTING A SPACE IN THE LAST COLUMN ON THE LINE
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


PDC     EQU     $


        LDA     CURHP           ; GET CURRENT COLUMN POSITION
        CPU     Z80
        NEG                     ; SUBTRACT FORM 79 FOR NUMBER OF MOVES
        CPU     8080
        ADI     79
        MOV     C,A             ; SAVE RESULT
        MVI     B,0
        CPU     Z80
        LD      DE,(CURAD)      ; (D,E) = CURSOR ADDRESS
        JR      Z,PDC2          ; IF ALREADY AT COLUMN 79
        CPU     8080

        LHLD    CURAD           ; (H,L) = CURSOR ADDRESS + 1
        INX     H

PDC1    MOV     A,D             ; KEEP POINTERS IN VIDEO RAM
        ORI     VRAMS/256
        MOV     D,A
        MOV     A,H
        ORI     VRAMS/256
        MOV     H,A
        CPU     Z80
        LDI                     ; COPY FROM (H,L) TO (D,E)
        CPU     8080
        MOV     A,B             ; SEE IF (B,C) = ZERO
        ORA     C
        CPU     Z80
        JR      NZ,PDC1         ; IF NOT DONE WITH LAST CHARACTER
        CPU     8080

PDC2    MVI     A,' '           ; PUT SPACE IN LAST COLUMN
        STAX    D
        RET

;;      PDL - PERFORM DELETE LINE
;
;       *PDL* MOVES THE REMAINING LINES OF THE DISPLAY UP ONE LINE, WRITES
;       SPACES INTO THE LAST LINE, AND MOVES THE CURSOR TO THE BEGINNING
;       OF THE CURRENT LINE.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


PDL     EQU     $

        MVI     A,0             ; SET COLUMN COUNTER TO ZERO
        STA     CURHP
        LHLD    CLSA            ; SET *CURAD* TO BEGINNING OF THIS LINE
        SHLD    CURAD
        XCHG                    ; (D,E) = BEGINNING OF THIS LINE
        LXI     H,80            ; SET (H,L) TO BEGINNING OF NEXT LINE
        DAD     D
        LDA     CURVP           ; GET CURRENT LINE NUMBER
        CPU     Z80
        NEG                     ; SUBTRACT FROM 23 FOR NUMBER OF LINES TO MOVE
        CPU     8080
        ADI     23
        CPU     Z80
        JR      Z,PDL2          ; IF ON LINE 23, JUST BLANK IT
        CPU     8080

        MOV     C,A             ; LINES LEFT*5 = MOD 16 MOVE COUNT
        ADD     C
        ADD     C
        ADD     C
        ADD     C
        MOV     C,A

PDL1    PUSH    B               ; SAVE (B,C)
        MOV     A,D             ; KEEP POINTERS IN VIDEO RAM
        ORI     VRAMS/256
        MOV     D,A
        MOV     A,H
        ORI     VRAMS/256
        MOV     H,A
        MVI     C,16            ; COPY 16 BYTES
        MVI     B,0
        CPU     Z80
        LDIR
        CPU     8080
        POP     B               ; RESTORE (B,C)
        DCR     C               ; DECREMENT MOD 16 COUNT
        CPU     Z80
        JR      NZ,PDL1         ; IF NOT DONE, DO 16 MORE
        CPU     8080

PDL2    XCHG                    ; (H,L) = LINE 23
        MVI     B,5             ; ERASE 80 CHARACTERS (5*16)
        JMP     WSVA            ; WRITE 80 SPACES AND EXIT

;;      PIC - PERFORM INSERT CHARACTER
;
;       *PIC* MOVES ALL CHARACTERS THAT ARE AT AND TO THE RIGHT OF THE
;       CURSOR ONE COLUMN TO THE RIGHT
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


PIC     EQU     $


        LDA     CURHP           ; GET CURRENT COLUMN POSITION
        CPU     Z80
        NEG                     ; SUBTRACT FROM 79 FOR NUMBER OF CHAR. TO MOVE
        CPU     8080
        ADI     79
        RZ                      ; IF AT COLUMN 79, NO COPY NEEDED

        MOV     C,A             ; (B,C) = RESULT
        MVI     B,0
        LHLD    CLSA            ; GET CURRENT LINE STARTING ADDRESS
        LXI     D,79            ; ADD 79 FOR END OF LINE ADDRESS
        DAD     D
        MOV     D,H             ; (D,E) = ADDRESS OF END OF LINE
        MOV     E,L
        DCX     H               ; (H,L) = ADDRESS OF END OF LINE - 1

PIC1    MOV     A,D             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     D,A
        MOV     A,H
        ORI     VRAMS/256
        MOV     H,A
        CPU     Z80
        LDD                     ; COPY CHARACTER
        CPU     8080
        MOV     A,B             ; SEE IF COUNT EXHAUSTED
        ORA     C
        CPU     Z80
        JR      NZ,PIC1         ; IF NOT DONE YET
        CPU     8080

        LHLD    CURAD           ; GET CURSOR ADDRESS TO PLACE CHARACTER
        RET

;;      PIL - PERFORM INSERT LINE
;
;       *PIL* INSERTS A BLANK LINE AT THE CURSOR POSITION AFTER MOVING
;       THE REMAINING LINES DOWN ONE LINE.  DATA ON LINE 23 IS LOST.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


PIL     EQU     $


        LDA     CURVP           ; GET CURRENT LINE NUMBER
        CPU     Z80
        NEG                     ; SUBTRACT FROM 23 FOR NUMBER OF LINES TO MOVE
        CPU     8080
        ADI     23
        CPU     Z80
        JR      Z,PIL2          ; IF ON LAST LINE, SKIP COPY
        CPU     8080

        MOV     C,A             ; LINES*5 = MODE 16 CHARACTER COUNT
        ADD     C
        ADD     C
        ADD     C
        ADD     C
        MOV     C,A             ; (C) = RESULT
        CPU     Z80
        LD      DE,(SHOME)      ; (D,E) = CURRENT HOME POSITION
        CPU     8080
        LXI     H,1919          ; ADD NUMBER OF CHARACTERS TO POINT TO LAST CHAR.
        DAD     D               ; (H,L) = LAST ADDRESS IN DISPLAY
        PUSH    H               ; SAVE ON STACK
        LXI     H,1839          ; ADD NUMBER OF CHARACTERS TO END OF LINE 22
        DAD     D               ; (H,L) = END OF LINE 22
        POP     D               ; (D,E) = END OF LINE 23

PIL1    PUSH    B               ; SAVE (B,C)
        MOV     A,D             ; KEEP POINTERS IN VIDEO RAM
        ORI     VRAMS/256
        MOV     D,A
        MOV     A,H
        ORI     VRAMS/256
        MOV     H,A
        MVI     C,16            ; COPY 16 BYTES
        MVI     B,0
        CPU     Z80
        LDDR
        CPU     8080
        POP     B               ; RESTORE (B,C)
        DCR     C               ; COPY COMPLETE?
        CPU     Z80
        JR      NZ,PIL1         ; IF NOT DONE
        CPU     8080

        MOV     A,D             ; KEEP POINTERS IN VIDEO RAM
        ORI     VRAMS/256
        MOV     D,A
        MOV     A,H
        ORI     VRAMS/256
        MOV     H,A
        CPU     Z80
        LDD                     ; COPY FIRST CHARACTER ON THIS LINE
        CPU     8080

PIL2    MVI     A,0             ; SET CURSOR TO BEGINNING OF LINE
        STA     CURHP
        LHLD    CLSA            ; SET CURAD TO BEGINNING OF LINE
        SHLD    CURAD
        MVI     B,5             ; WRITE 80 SPACES (5*16)
        JMP     WSVA

;;      PSD - PARAMETER STRING DECODER
;
;       *PSD* INPUTS A PARAMETER STRING OF DECIMAL NUMBERS SEPARATED BY
;       A SEMICOLON (TO A MAX OF 15) UNTIL THE FINAL CHARACTER OF THE
;       ESCAPE SEQUENCE (NON DECIMAL AND NOT A SEMICOLON) IS INPUT
;
;
;       ENTRY   NONE
;
;       EXIT    (A) = FINAL CHARACTER
;               (B) = ZERO IF NO PARAMETER STRING PRECEDING FINAL CHARACTER
;
;       USES    A,B,C,D,H,L,F


PSD     LXI     H,PSDW          ; POINT TO PSD WORK AREA
        LXI     B,0             ; NO PARAMETER STRING YET
        MOV     M,B             ; FIRST VALUE = ZERO
        CALL    FNCP            ; INPUT FIRST CHARACTER
        CPI     ';'             ; CAN'T HAVE A SEMICOLON AS FIRST CHARACTER
        RZ                      ; IF SEMICOLON, END SEQUENCE

PSD1    CPI     '0'             ; SEE IF LESS THAN DECIMAL
        CPU     Z80
        JR      C,PSD4          ; IF TOO LOW, EXIT
        CPU     8080

        CPI     '9'+1           ; SEE IF GREATER THAN DECIMAL
        CPU     Z80
        JR      NC,PSD2         ; IF NOT DECIMAL
        CPU     8080

;       INPUT CHARACTER IS DECIMAL
;
        ANI     00001111B       ; MASK FOR BINARY
        MOV     D,A             ; SAVE RESULT

;       NEW DIGITS VALUE, MULTIPLY OLD VALUE BY TEN
;
        MOV     A,M             ; GET OLD VALUE
        RLC                     ; *2
        MOV     M,A
        RLC                     ; *4
        RLC                     ; *8
        ADD     M               ; *2 + *8 = *10
        ADD     D               ; ADD NEW DIGITS
        MOV     M,A             ; SAVE RESULT
        INR     B               ; SHOW THAT A PN HAS BEEN INPUT
        INR     C
        CPU     Z80
        JR      PSD3.5          ; INPUT NEXT CHARACTER
        CPU     8080

PSD2    CPI     ';'             ; SEE IF NEXT CHARACTER IS A SEMICOLON
        CPU     Z80
        JR      NZ,PSD4         ; IF NOT, THEN CHARACTER IS FINAL
        CPU     8080

        MOV     A,L             ; ELSE, SEE IF ROOM FOR THIS PARAMETER
        CPI     PSDWE-2         ; SAVE ROOM FOR LAST PLUS FINAL
        CPU     Z80
        JR      Z,PSD3          ; IF NO MORE ROOM
        CPU     8080

        INX     H               ; POINT TO NEXT CELL
PSD3    MVI     C,0             ; RESET PARAMETER BUILD REGISTER
        MVI     M,0             ; TOSS OLD PN
PSD3.5  CALL    FNCP            ; GET NEXT CHARACTER
        CPU     Z80
        JR      PSD1            ; DECODE CHARACTER
        CPU     8080

PSD4    MOV     D,A             ; SAVE FINAL CHARACTER
        XRA     A               ; SEE IF PN IS IN MEMORY AT CURRENT ADDRESS
        ORA     C
        CPU     Z80
        JR      Z,PSD5          ; IF NOTHING IN PROGRESS
        CPU     8080

        INX     H               ; ELSE PUT FINAL IN NEXT BYTE
PSD5    MOV     M,D             ; PUT FINAL CHARACTER IN WORK AREA
        MOV     A,D             ; AND (A)
        LXI     D,PSDW          ; (D,E) = PSDW
        RET

;;      PSIF - PUT STRING IN INPUT FIFO
;
;       *PSIF* PLACES THE STRING IMMEDIATELY FOLLOWING THE CALL TO
;       THIS ROUTINE INTO THE INPUT FIFO
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


PSIF    POP     D               ; GET ADDRESS OF CHARACTERS TO OUTPUT
PSIF1   LDAX    D               ; GET CHARACTER
        INX     D               ; POINT TO NEXT CHARACTER
        ORA     A               ; GET CPU FLAGS
        JM      PSIF2           ; IF THIS IS LAST CHARACTER TO OUTPUT

        DI                      ; LOCK OUT INTERRUPTS
        CALL    PCIF            ; PLACE CHARACTER
        EI
        CPU     Z80
        JR      PSIF1           ; GET NEXT CHARACTER
        CPU     8080

PSIF2   ANI     01111111B       ; MASK OFF TERMINATOR
        PUSH    D               ; SET RETURN ADDRESS BACK ON STACK
        DI
        CALL    PCIF            ; PLACE LAST CHARACTER
        EI
        RET

;;      PSOF - PUT STRING IN OUTPUT FIFO
;
;       *PSOF* PLACES THE CHARACTER STRING IMMEDIATELY FOLLOWING THE
;       CALL TO THIS ROUTINE INTO THE OUTPUT FIFO
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


PSOF    POP     D               ; GET ADDRESS OF CHARACTERS TO OUTPUT
PSOF1   LDAX    D               ; GET CHARACTER
        INX     D               ; POINT TO NEXT CHARACTER
        ORA     A               ; SET CPU FLAGS
        JM      PSOF2           ; IF THIS IS LAST CHARACTER TO OUTPUT

        CALL    PCOFT           ; PLACE CHARACTER IN OUTPUT FIFO
        CPU     Z80
        JR      PSOF1           ; GET NEXT CHARACTER
        CPU     8080

PSOF2   ANI     01111111B       ; TOSS TERMINATOR BIT
        PUSH    D               ; SET RETURN ADDRESS
        JMP     PCOFT           ; OUTPUT LAST CHARACTER

;;      *RMS* RESETS THE MODE SPECIFIED BY THE LAST CHARACTER IN THE SEQUENCE
;
;
;       ENTRY   (B,C) = MODEB
;               (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


RMS     LXI     H,RMST          ; (H,L) = RESET MODE SEQUENCE TABLE
        CPU     Z80
        JR      SMSA            ; CONTINUE SAME AS SET MODE SEQUENCE
        CPU     8080

;;      ASCP - ANSI SAVE CURSOR POSITION
;
;       *ASCP* SAVES THE CURRENT LINE AND COLUMN NUMBERS OF THE CURSOR
;       POSITION
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;
;       EXIT    NONE
;
;       USES    A,H,L,F


ASCP    MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RNZ                     ; IF INPUT, ILLEGAL, EXIT

;       JMP     SCP             ; ELSE, SAVE CURSOR POSITION
;       ERRNZ   $-SCP

;;      SCP - SAVE CURSOR POSITION
;
;       *SCP* SAVES THE CURRENT POSITION OF THE CURSOR WHICH MAY BE RESTORED
;       WITH THE USE OF *USCP*
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    H,L


SCP     LHLD    CURHP           ; GET CURSOR POSITIONS
;       ERRNZ   CURVP-CURHP-1   ; MUST BE CONTIGUOUS
        SHLD    CSA             ; KEEP IN CURSOR SAVED ADDRESS
        RET

;;      SMS - SET MODE SEQUENCE
;
;       *SMS* SETS THE MODE SPECIFIED BY THE LAST CHARACTER IN THE SEQUENCE
;
;
;       ENTRY   (B,C) = MODEB
;               (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


SMS     LXI     H,SMST          ; (H,L) = SET MODE SEQUENCE TABLE

SMSA    CALL    FNCP            ; GET FINAL SEQUENCE CHARACTER
        CPI     CAN             ; SEE IF TO CANCEL
        RZ                      ; IF CANCEL, EXIT NOW

        CPI     '1'             ; CAN'T BE LOWER THAN ONE
        JC      IFCP            ; IF LOWER, EXIT LIKE NEVER HERE

        CPI     '9'+1           ; <= 9
        JNC     IFCP            ; IF GREATER THAN 9

        ANI     00001111B       ; FORM HALF ASCII FOR TABLE OFFSET
SMSB    DCR     A               ; 1 = FIRST ENTRY
        RLC                     ; *2 FOR TABLE WIDTH
        ADD     L               ; ADD TO STARTING ADDRESS OF TABLE
        MOV     L,A
        MOV     A,M             ; GET FUNCTION ADDRESS LSB
        INX     H
        MOV     H,M             ; MSB
        MOV     L,A
        LXI     B,MODEB         ; (B,C) = MODEB
        LXI     D,MODEA         ; (D,E) = MODEA
        PCHL                    ; GO TO FUNCTION ROUTINE

;;      SMST - SET MODE SEQUENCE TABLE
;
;       *SMST* CONTAINS THE ADDRESS OF THE ROUTINE WHICH WILL SET
;       THE MODE REQUESTED


SMST    EQU     $
        DW      E25L            ; ENABLE 25TH LINE
        DW      NKC             ; NO KEYBOARD CLICK
        DW      EHSM            ; ENTER HOLD SCREEN MODE
        DW      SBC             ; SET 'BLOCK' CURSOR
        DW      DC              ; DISABLE CURSOR
        DW      EKSM            ; ENTER KEYPAD SHIFTED MODE
        DW      EKAM            ; ENTER KEYPAD ALTERNATE MODE
        DW      EALF            ; ENABLE AUTO LINE FEED ON CARRIAGE RETURN
        DW      EACR            ; ENABLE AUTO CARRIAGE RETURN ON LINE FEED
;       ERRNZ   SHMST/256-$/256 ; TABLE MUST RESIDE IN ONE PAGE

;;      RMST - RESET MODE SEQUENCE TABLE
;
;       *RMST* CONTAINS THE ADDRESSES OF THE ROUTINES WHICH RESET THE
;       MODE SPECIFIED


RMST    EQU     $
        DW      D25L            ; DISABLE 25TH LINE
        DW      EKC             ; ENABLE KEYBOARD CLICK
        DW      XHSM            ; EXIT HOLD SCREEN MODE
        DW      SUC             ; SET 'UNDERSCORE' CURSOR
        DW      EC              ; ENABLE CURSOR
        DW      XKSM            ; EXIT KEYPAD SHIFTED MODE
        DW      XKAM            ; EXIT KEYPAD ALTERNATE MODE
        DW      XALF            ; EXIT AUTO LINE FEED ON CARRIAGE RETURN
        DW      XACR            ; EXIT AUTO CARRIAGE RETURN ON LINE FEED
;       ERRNZ   RMST/256-$/256  ; MUST NOT CROSS PAGE BOUNDARY

;;      SBC - SET BLOCK CURSOR
;
;       *SBC* PROGRAMS THE CRTC TO GENERATE A SLOW BLINKING BLOCK CURSOR
;
;
;       ENTRY   (B,C) = MODEB
;               (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,F


SBC     LDAX    B               ; GET MODE FLAGS
        ORI     MB.CBLK         ; FLAG BLOCK CURSOR
        STAX    B
        LDAX    D
        ANI     MA.CD           ; SEE IF CURSOR DISABLED
        RNZ                     ; IF DISABLED, EXIT

SBC.    MVI     H,0+VB.CBE+VB.CBPS ; START ON LINE ZERO, BLINK, SLOW
        MVI     L,9             ; STOP ON LINE NINE
        SHLD    VI.CSE
        RET

;;      SCH - SET CURSOR HOME
;
;       *SCH* PLACES THE CURSOR AT LINE ZERO, COLUMN ZERO
;
;
;       ENTRY   NONE
;
;       EXIT    (H,L) = CURSOR ADDRESS
;
;       USES    A,H,L,F


SCH     LXI     H,0             ; (H,L) = ZERO
        SHLD    CURHP           ; (CURHP & CURVP) = ZERO
;       ERRNZ   CURVP-CURHP-1
        LHLD    SHOME           ; RESET CURSOR AND LINE ADDRESSES
        SHLD    CURAD
        SHLD    CLSA


        RET                     ; EXIT WITH OR WITHOUT

;;      AUSCP - ANSI UNSAVE CURSOR POSITION
;
;       *AUSCP* RETURNS THE CURSOR TO THE POSITION PREVIOUSLY SAVED BY *ASCP*
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


AUSCP   MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RNZ                     ; IF PN INPUT, ILLEGAL, EXIT

;       JMP     USCP            ; ELSE, UNSAVE CURSOR POSITION
;       ERRNZ   $-USCP

;;      USCP - UNSAVE CURSOR POSITION
;
;       *USCP* RETURNS TO CURHP AND CURVP THE VALUES SAVED WITH THE
;       USE OF *SCP*
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    H,L


USCP    LHLD    CSA             ; GET SAVED ADDRESS
        SHLD    CURHP           ; UPDATE CURRENT POINTERS
;       ERRNZ   CURVP-CURHP-1   ; POINTERS ARE CONTIGUOUS

;       JMP     SNCP            ; SET NEW CURSOR POSITION
;       ERRNZ   $-SNCP

;;      SNCP - SET NEW CURSOR POSITION
;
;       *SNCP* USES THE CURRENT VALUES OF THE VERTICAL AND HORIZONTAL
;       POSITION CELLS TO SET THE CURSOR POSITION
;
;
;       ENTRY   *CURVP* - VERTICAL POSITION TO BE SET
;               *CURHP* - HORIZONTAL POSITION TO BE SET
;
;       EXIT    'C = CLEARED
;
;       USES    A,D,E,H,L,F


SNCP    LDA     CURVP           ; GET VERTICAL POSITION
        CALL    MPY80           ; *80
        XCHG                    ; (D,E) = NUMBER OF CHARACTERS TO NEW LINE
        LHLD    SHOME           ; GET HOME ADDRESS
        DAD     D               ; ADD OFFSET FOR THIS LINE'S STARTING ADDRESS
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    CLSA            ; SET CURRENT LINE STARTING ADDRESS

        LDA     CURHP           ; GET HORIZONTAL POSITION
        MOV     E,A             ; ADD TO LINE STARTING ADDRESS
        MVI     D,0
        DAD     D
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    CURAD           ; SET CURSOR ADDRESS
        RET

;;      STAB - SEARCH TABLE
;
;       *STAB* SEARCHES TABLES OF SPECIFIED LENGTH AND WIDTH FOR
;       A MATCH WITH THE FIRST CHARACTER IN AN ENTRY
;
;
;       ENTRY   (A) = CHARACTER TO MATCH
;               (D) = TABLE LENGTH
;               (E) = TABLE WIDTH
;               (H,L) = TABLE ADDRESS
;
;       EXIT    (A) = FIRST CHARACTER IN TABLE AFTER MATCH OR
;                     ORIGINAL CHARACTER IF NO MATCH
;               (H,L) = FIRST ADDRESS AFTER CHARACTER MATCHES
;               'C' = SET IF NOT MATCH
;
;       USES    A,B,D,D,E,H,L,F


STAB    MOV     C,A             ; SAVE CHARACTER TO MATCH
STAB1   XRA     M               ; CHARACTER MATCH TABLE ENTRY?
        CPU     Z80
        JR      Z,STAB3         ; IF CHARACTER MATCHED
        CPU     8080

        MOV     B,E             ; ELSE, GET WIDTH AND ADVANCE TO NEXT ENTRY
        MOV     A,C             ; GET CHARACTER TO MATCH
STAB2   INX     H               ; ADVANCE (E) BYTES
        CPU     Z80
        DJNZ    STAB2
        CPU     8080

        DCR     D               ; DECREMENT LENGTH COUNTER
        CPU     Z80
        JR      NZ,STAB1        ; IF NOT LAST ENTRY
        CPU     8080

        MOV     A,C             ; ELSE, RESTORE ORIGINAL CHARACTER AND EXIT
        STC                     ; SET 'C'
        RET

STAB3   INX     H               ; MATCH FOUND, GET NEXT BYTE
        MOV     A,M
        RET

;;      SPWE - SET PREVIOUS WAS AN ESCAPE FLAG
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,F


SPWE    LDA     MODEI           ; GET INTERNAL MODE FLAGS
        ORI     MI.PWE          ; SET PWE
        STA     MODEI           ; UPDATE FLAGS
        RET

;;      SUC - SET UNDERSCORE CURSOR
;
;       *SUC* PROGRAMS THE CRTC FOR A SINGLE SCAN LINE CURSOR ON SCAN
;       LINE EIGHT OF THE CHARACTER ROW
;
;
;       ENTRY   (B,C) = MODEB
;               (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,B,C


SUC     LDAX    B               ; GET MODE FLAGS
        ANI     255-MB.CBLK     ; RESET CURSOR = BLOCK FLAG
        STAX    B
        LDAX    D
        ANI     MA.CD           ; SEE IF CURSOR IS DISABLED
        RNZ                     ; IF DISABLED, EXIT

SUC.    MVI     H,8+VB.CBE      ; START ON LINE 8, BLINK FAST
        MVI     L,8             ; ALSO END ON LINE 8
        SHLD    VI.CSE
        RET

;;      TAB - TAB CURSOR TO NEXT EIGHTH COLUMN
;
;       *TAB* PLACES THE CURSOR AT THE NEXT MULTIPLE OF 8 COLUMNS
;       FROM THE BEGINNING OF THE LINE UNLESS THE CURSOR IS WITHIN SEVEN
;       COLUMNS OF THE END, IN WHICH CASE THE CURSOR IS MOVED ONLY ONE COLUMN.
;       THE CURSOR IS NOT WRAPPED TO THE NEXT LINE.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,D,E,H,L,F


TAB     LDA     CURHP           ; GET CURRENT HORIZONTAL POSITION
        ADI     8               ; ADD 8 COLUMNS
        ANI     377Q-7          ; TOSS ANY LEFT OVERS LESS THAN EIGHT
        CPI     80              ; PAST END OF LINE?
        CPU     Z80
        JR      NZ,TAB2         ; IF STILL ON SAME LINE
        CPU     8080

        LDA     CURHP           ; GET CURRENT POSITION ON LINE
        CPI     79              ; AT LAST COLUMN YET?

        CPU     Z80
        JR      Z,TAB1.5        ; IF AT LAST COLUMN, STAY
        CPU     8080

TAB1    INR     A               ; ELSE, MOVE ONE COLUMN CLOSER
        CPU     Z80
        JR      TAB2            ; UPDATE CURSOR TO NEW POSITION
        CPU     8080

TAB1.5  MVI     A,79            ; ELSE, STOP AT LAST COLUMN OF LAST LINE
        CPU     Z80
        JR      TAB2            ; UPDATE CURSOR ADDRESS
        CPU     8080

TAB2    STA     CURHP           ; UPDATE COLUMN COUNTER
        MOV     E,A             ; ADD TO LINE ADDRESS
        MVI     D,0
        LHLD    CLSA            ; GET CURRENT LINE ADDRESS
        DAD     D
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        SHLD    CURAD           ; UPDATE CURSOR ADDRESS

        RET                     ; EXIT

;;      UCP - UPDATE CURSOR POSITION
;
;       *UCP* SENDS THE LOWER ELEVEN BITS OF THE CURSOR ADDRESS TO THE CRTC
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,H,L,F


UCP     LDA     CURVP           ; GET LINE COUNT
        CALL    MPY80           ; MULTIPLY BY 80 CHARACTERS PER LINE
        LDA     CURHP           ; GET COLUMN COUNT
        MOV     E,A             ; ADD TO LINE COUNT
        MVI     D,0
        DAD     D
        XCHG                    ; (D,E) = CHARACTER OFFSET FROM HOME POSITION
        LHLD    SHOME           ; GET HOME POSITION
        MOV     A,H             ; MASK FOR A 2K ADDRESS
        ANI     HOMAX/256
        MOV     H,A
        DAD     D               ; ADD OFFSET
        MOV     A,H             ; GET CURAD MSB
        ANI     CURMAX/256      ; TOSS UPPER 5 BITS
        MOV     H,A
        SHLD    VI.CA

;       HIT NMI TO MAKE SURE ITS RUNNING
;
UCP.    MVI     A,0             ; SET REGISTER ZERO AS DUMMY
        OUT     VP.AR+VB.NMI    ; OUT NMI REQUEST WITH PORT NUMBER
        RET

;;      WEOL - WRAP AROUND AT END OF LINE
;
;       *WEOL* SETS THE FLAG WHICH CAUSES THE TERMINAL TO PERFORM
;       A CARRIAGE RETURN AND LINE FEED WHEN THE 81ST CHARACTER FOR A
;       LINE IS RECEIVED
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


WEOL    LDAX    B               ; GET MODE FLAGS
        ORI     MB.WRAP         ; SET WRAP AROUND FLAG
        STAX    B
        RET

;;      WSV - WRITE SPACES TO VIDEO
;
;       *WSV* WRITES N ASCII SPACE CODES INTO THE VIDEO RAM.  THE LAST SPACE
;       WRITTEN MUST BE AT THE END OF A VIDEO LINE (X*80+VRAMS)
;
;
;       ENTRY   (D,E) = NUMBER OF SPACES TO BE WRITTEN
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


WSV     EQU     $
WSV0.5  MOV     A,L             ; GET LSB OF ADDRESS
        ANI     00001111B       ; MASK FOR ANY CARRY PAST MOD 16
        CPU     Z80
        JR      Z,WSV2          ; IF NO EXTRAS
        CPU     8080

        MVI     C,' '           ; SPACE = FILL CHARACTER
        MOV     M,C             ; PLACE SPACE IN MEMORY
        INX     H               ; POINT TO NEXT
        DCX     D               ; DECREMENT COUNT
        CPU     Z80
        JR      WSV0.5
        CPU     8080

WSV2    MVI     B,4             ; ROTATE COUNT TO A MOD 16 COUNTER
WSV3    MOV     A,D             ; ROTATE MSB
        RAR
        MOV     D,A
        MOV     A,E             ; ROTATE LSB
        RAR
        MOV     E,A
        CPU     Z80
        DJNZ    WSV3            ; LOOP 4 TIMES
        CPU     8080

        MOV     A,E             ; CHECK FOR ZERO COUNT
        ORA     A
        RZ                      ; IF NO SPACES LEFT TO BE WRITTEN

        MOV     B,E             ; PLACE MOD 16 COUNT IN B
WSVA    EQU     $               ; ALTERNATE ENTRY POINT FROM *PLF*

WSVA.   MVI     C,' '           ; (C) = ASCII SPACE
WSV4    MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        MOV     M,C             ; PLACE 16 SPACES IN MEMORY
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        MOV     M,C
        INX     H
        CPU     Z80
        DJNZ    WSV4            ; LOOP UNTIL MOD 16 COUNTER = ZERO
        CPU     8080

        RET

;;      XACR - EXIT AUTO CARRIAGE RETURN
;
;       *XACR* CLEARS THE AUTO CARRIAGE RETURN FLAG
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


XACR    LDAX    B               ; GET MODE FLAGS
        ANI     255-MB.ACR      ; CLEAR AUTO CARRIAGE RETURN
        STAX    B
        RET

;;      XALF - EXIT AUTO LINE FEED MODE
;
;       *XALF* CLEARS THE AUTO LINE FEED MODE FLAG
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


XALF    LDAX    B               ; GET MODE FLAGS
        ANI     255-MB.ALF
        STAX    B
        RET

;;      XGM - EXIT GRAPHICS MODE
;
;       *XGM* CLEARS THE GRAPHICS MODE FLAG
;
;       ENTRY   (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,F


XGM     XCHG                    ; (H,L) = MODEA
        CPU     Z80
        RES     IB.GRPH,(HL)    ; RESET GRAPHICS MODE FLAG
        CPU     8080
        RET

;;      XHSM - EXIT HOLD SCREEN MODE
;
;       *XHSM* CLEARS THE HOLD SCREEN MODE FLAG
;
;
;       ENTRY   (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,F


XHSM    XCHG                    ; (H,L) = MODEA
        CPU     Z80
        RES     IB.HSM,(HL)
        CPU     8080
        RET

;;      XICM - EXIT INSERT CHARACTER MODE
;
;       *XICM* CLEARS THE INSERT CHARACTER MODE FLAG
;
;
;       ENTRY   (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,F


XICM    XCHG                    ; (H,L) = MODEA
        CPU     Z80
        RES     IB.ICM,(HL)     ; RESET INSERT CHARACTER MODE FLAG
        CPU     8080
        RET

;;      XKAM - EXIT KEYPAD ALTERNATE MODE
;
;       *XKAM* CLEARS THE KEYPAD ALTERNATE MODE FLAG
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F

XKAM    LDAX    B               ; SET MODEB FLAGS
        ANI     377Q-MB.KPDA
        STAX    B
        RET

;;      XKSM - EXIT KEYPAD SHIFTED MODE
;
;       *XKSM* CLEARS THE KEYPAD SHIFTED MODE FLAG
;
;
;       ENTRY   (B,C) = MODEB
;
;       EXIT    NONE
;
;       USES    A,F


XKSM    LDAX    B               ; GET MODEB FLAGS
        ANI     377Q-MB.KPDS
        STAX    B
        RET

;;      AXMT25 - ANSI TRANSMIT 25TH LINE
;
;       *AXMT25* IS THE ENTRY POINT FOR *XMT25* WHEN THE TERMINAL IS
;       IN ANSI MODE
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;
;       EXIT    TO *XMT25*
;
;       USES    A,B,C,D,E,H,L,F


AXMT25  MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RNZ                     ; IF INPUT, ILLEGAL, EXIT

;       JMP     XMT25
;       ERRNZ   $-XMT25

;;      XMT25 - TRANSMIT 25TH LINE
;
;       *SMT25* TRANSMITS THE LINE IN THE SAME MANNER AS *XMTP*, BUT ONLY IF
;       THE 25TH LINE IS ENABLED.
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F

XMT25   LDA     MODEI           ; GET MODE FLAGS
        ANI     MI.25L          ; IS 25TH LINE ON?
        CPU     Z80
        JR      Z,XMT25.1       ; IF NOT ON, JUST SEND CR
        CPU     8080

        LHLD    SHOME           ; GET CURRENT HOME POSITION
        LXI     D,1920          ; ADD 24 LINES*80 COLUMNS
        DAD     D
        MOV     A,H             ; STAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     H,A
        XCHG                    ; FIRST ADDRESS ON LINE TO (D,E)
        MVI     B,0             ; CURRENT TRANSMIT MODE = NONE
        CALL    XMTL            ; TRANSMIT LINE
XMT25.1 MVI     A,CR            ; TERMINATE LINE WITH A CARRIAGE RETURN
        CALL    XMTC
        OUT     MP.BELL         ; SIGNAL DONE TO USER
        RET

;;      XMTC - TRANSMIT CHARACTER
;
;       *XMTC* PLACES THE GIVEN CHARACTER INTO THE OUTPUT FIFO AND
;       ASSURES THAT THE TERMINAL IS IN THE ON-LINE STATE UNTIL THE CHARACTER
;       IS SENT
;
;
;               ************* CAUTION: THIS ROUTINE MAY        **************
;               ************* ONLY BE USED BY  *XMT??* ROUTINES *************
;
;
;       ENTRY   (A) = CHARACTER TO SEND
;
;       EXIT    NONE
;
;       USES    A,D,E,H,L,F


XMTC    PUSH    PSW             ; SAVE CHARACTER TO SEND

XMTC1   IN      AP.LSR          ; GET LINE STATUS REGISTER OF UART
        ANI     AB.THRE         ; HOLDING REGISTER EMPTY
        CPU     Z80
        JR      Z,XMTC1         ; IF NOT EMPTY
        CPU     8080

        POP     PSW             ; ELSE OUTPUT CHARACTER
        OUT     AP.THR
        RET

;;      XMTL - TRANSMIT LINE
;
;       *XMTL* TRANSMITS ONE LINE TO THE HOST INCLUDING THE APPROPRIATE
;       ESCAPE SEQUENCES FOR ENTERING AND EXITING THE GRAPHICS AND
;       REVERSE VIDEO MODES
;
;
;       ENTRY   (B) = CURRENT TRANSMIT MODE (RV AND/OR GRAPHICS)
;               (D,E) = ADDRESS OF BEGINNING OF LINE
;
;       EXIT    (B) = CURRENT TRANSMIT MODE (UPDATED)
;               (D,E) = LAST ADDRESS OF LINE +1
;
;       USES    A,B,C,D,E,H,L,F


XMTL    MVI     H,80            ; SET NUMBER OF CHARACTERS TO SEND
XMTL1.1 PUSH    H
        LDAX    D               ; GET CHARACTER
        MOV     C,A             ; SAVE FOR LATER USE
        INX     D               ; INCREMENT MEMORY POINTER
        MOV     A,D             ; SAY IN VIDEO RAM
        ORI     VRAMS/256
        MOV     D,A
        PUSH    D               ; SAVE ADDRESS
        MOV     A,C
        ANI     01111111B       ; TOSS ANY REVERSE VIDEO BIT FOR NOW
        CPI     ' '             ; SEE IF LESS THAN A SPACE (GRAPHICS)
        CPU     Z80
        JR      C,XMTL1         ; IF A GRAPHIC CHARACTER
        CPU     8080

        CPI     177Q            ; 177Q IS ALSO A GRAPHIC DISPLAY CHARACTER
        CPU     Z80
        JR      NZ,XMTL4        ; IF NOT GRAPHIC
        CPU     8080

;       CHARACTER IS GRAPHIC
;
        MVI     A,'^'           ; SET PRINTABLE EQUIVALENT OF 177Q
        CPU     Z80
        JR      XMTL2
        CPU     8080

XMTL1   CPI     37Q             ; SEE IF GRAPHIC '-'
        CPU     Z80
        JR      NZ,XMTL1.3      ; IF NOT 37Q
        CPU     8080

        MVI     A,'_'           ; ELSE, SET PRINTABLE
        CPU     Z80
        JR      XMTL2           ; CHECK GRAPHIC MODE
        CPU     8080

XMTL1.3 ORI     01100000B       ; MAKE GRAPHIC PRINTABLE
XMTL2   PUSH    PSW             ; SAVE CHARACTER TO OUTPUT

;       SEE IF LAST CHARACTER OUTPUT WAS ALSO GRAPHIC
;
        CPU     Z80
        BIT     1,B
        JR      NZ,XMTL6        ; IF LAST CHARACTER WAS ALSO GRAPHIC

        SET     1,B             ; ELSE, SET MODE TO INCLUDE  GRAPHICS
        CPU     8080
        LDA     MODEB           ; SEE IF IN ANSI MODE
        ANI     MB.ANSI
        CPU     Z80
        JR      Z,XMTL3         ; IF IN HEATH MODE
        CPU     8080

        CALL    XMTS            ; TRANSMIT ANSI 'ENTER GRAPHICS' SEQUENCE
        DB      ESC,'[','10','m'+200Q

        CPU     Z80
        JR      XMTL6           ; GO CHECK FOR REVERSE VIDEO
        CPU     8080

XMTL3   CALL    XMTS            ; TRANSMIT HEATH 'ENTER GRAPHICS' SEQUENCE
        DB      ESC,'F'+200Q
        CPU     Z80
        JR      XMTL6           ; CHECK FOR REVERSE VIDEO
        CPU     8080

;       CHARACTER IS NOT GRAPHIC
;
XMTL4   PUSH    PSW             ; SAVE CHARACTER TO OUTPUT
        CPU     Z80
        BIT     1,B             ; SEE IF LAST WAS GRAPHIC
        JR      Z,XMTL6         ; IF LAST WAS NOT GRAPHIC EITHER

        RES     1,B             ; ELSE, SET NO GRAPHIC
        CPU     8080

        LDA     MODEB           ; SEE IF IN ANSI MODE
        ANI     MB.ANSI
        CPU     Z80
        JR      Z,XMTL5         ; IF IN HEATH MODE
        CPU     8080

        CALL    XMTS            ; ELSE, TRANSMIT ANSI 'EXIT GRAPHICS' SEQUENCE
        DB      ESC,'[','11','m'+200Q
        CPU     Z80
        JR      XMTL6           ; CHECK FOR REVERSE VIDEO
        CPU     8080

XMTL5   CALL    XMTS            ; TRANSMIT HEATH 'EXIT GRAPHICS' SEQUENCE
        DB      ESC,'G'+200Q

;       SEE IF CHARACTER IS IN REVERSE VIDEO
;
XMTL6   EQU     $

        CPU     Z80
        BIT     7,C             ; SEE IF MSB IS SET FOR RV
        JR      Z,XMTL7.5       ; IF NOT REV

;       CHARACTER IS REVERSE VIDEO
;
        BIT     2,B             ; SEE I LAST WAS RV
        JR      NZ,XMTL9        ; IF LAST WAS ALSO RV

        SET     2,B             ; ELSE, SET RV MODE AS CURRENT
        CPU     8080
        LDA     MODEB           ; SEE IF IN HEATH MODE
        ANI     MB.ANSI
        CPU     Z80
        JR      Z,XMTL7         ; IF IN HEATH MODE
        CPU     8080

        CALL    XMTS            ; ELSE, TRANSMIT ANSI 'ENTER REVERSE VIDEO'
        DB      ESC,'[','7','m'+200Q
        CPU     Z80
        JR      XMTL9           ; GO TRANSMIT CHARACTER
        CPU     8080

XMTL7   CALL    XMTS            ; TRANSMIT HEATH 'ENTER REVERSE VIDEO'
        DB      ESC,'p'+200Q
        CPU     Z80
        JR      XMTL9           ; OUTPUT CHARACTER
        CPU     8080

;       CHARACTER IS NOT REVERSE VIDEO
;
XMTL7.5 EQU     $
        CPU     Z80
        BIT     2,B             ; SEE IF LAST CHARACTER WAS RC
        JR      Z,XMTL9         ; IF LAST WASN'T EITHER

        RES     2,B             ; ELSE, SET CURRENT MODE AS NOT REVERSE
        CPU     8080

        LDA     MODEB           ; SEE IF IN ANSI MODE
        ANI     MB.ANSI
        CPU     Z80
        JR      Z,XMTL8         ; IF IN HEATH MODE
        CPU     8080

        CALL    XMTS            ; ELSE SEND ANSI 'EXIT RV'
        DB      ESC,'[','m'+200Q
        CPU     Z80
        JR      XMTL9           ; SEND CHARACTER
        CPU     8080

XMTL8   CALL    XMTS            ; SEND HEATH 'EXIT RV' SEQUENCE
        DB      ESC,'q'+200Q

XMTL9   POP     PSW             ; GET CHARACTER TO SEND
        CALL    XMTC            ; SEND IT
        POP     D               ; GET LINE ADDRESS
        POP     H               ; GET COLUMN COUNTER
        DCR     H               ; SEE IF ANY LEFT TO SEND
        JNZ     XMTL1.1         ; IF NOT DONE YET

        RET

;;      AXMTP - ANSI TRANSMIT PAGE
;
;       *AXMTP* IS THE ENTRY POINT FOR *XMTP* WHEN THE TERMINAL IS IN
;       THE ANSI MODE
;
;
;       ENTRY   (B) = ZERO IF NO PN WAS INPUT
;
;       EXIT    TO *XMTP*
;
;       USES    A,B,C,D,E,H,L,F


AXMTP   MOV     A,B             ; SEE IF PN WAS INPUT
        ORA     A
        RNZ                     ; IF INPUT, ILLEGAL, EXIT

;       JMP     XMTP            ; ELSE, TRANSMIT PAGE

;;      XMTP - TRANSMIT PAGE
;
;       *XMTP* TRANSMITS THE ENTIRE CONTENTS OF THE PAGE (EXCLUDING THE 25TH LINE)
;       IF THE TERMINAL IS OFF LINE, THE CONTENTS OF THE PAGE IS STILL
;       TRANSMITTED TO THE HOST AND THE TERMINAL IS RETURNED TO THE CURRENT
;       STATE OF THE OFF LINE SWITCH AFTER THE PAGE HAS BEEN TRANSMITTED.
;       IF GRAPHICS CHARACTERS OR REVERSE VIDEO CHARACTERS ARE ENCOUNTERED.
;       THE PROPER ESCAPE CODES FOR ENTERING AND EXITING THESE MODES ARE SENT
;       TO THE HOST IN THE SAME MANNER AS THE HOST WOULD USE TO PLACE THESE
;       CHARACTERS ON THE SCREEN
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    A,B,C,D,E,H,L,F


XMTP    LHLD    SHOME           ; GET FIRST ADDRESS ON SCREEN
        XCHG                    ; TO (D,E)
        MVI     B,0             ; CLEAR CURRENT TRANSMIT MODE
        MVI     H,24            ; SEND 24 LINES
XMTP1   PUSH    H               ; SAVE COUNT
        CALL    XMTL            ; TRANSMIT ONE LINE
        POP     H               ; GET LINE COUNTER
        DCR     H               ; COUNT - 1
        CPU     Z80
        JR      NZ,XMTP1        ; IF NOT DONE
        CPU     8080

        MVI     A,CR            ; SEND A CARRIAGE RETURN TO TERMINATE PAGE
        CALL    XMTC            ; TRANSMIT CHARACTER
        OUT     MP.BELL         ; SIGNAL DONE TO USER
        RET

;;      XMTS - TRANSMIT STRING
;
;       *XMTS* PLACES A STRING OF CHARACTERS IN THE OUTPUT FIFO AND
;       ASSURES THAT THEY ARE SENT REGARDLESS OF WHETHER THE TERMINAL
;       IS ON LINE OR NOT
;
;
;       ENTRY   STRING TO BE SENT MUST IMMEDIATELY FOLLOW THE CALL
;                       TO THIS ROUTINE.  FINAL CHARACTER MUST HAVE
;                       BIT SEVEN SET
;
;       EXIT    NONE
;
;       USES    A,D,E,H,L,F


XMTS    POP     D               ; GET ADDRESS OF CHARACTERS TO BE SENT
XMTS1   LDAX    D               ; GET CHARACTER TO SEND
        INX     D               ; POINT TO NEXT
        ORA     A               ; SET CPU FLAGS
        JM      XMTS2           ; IF LAST CHARACTER TO SEND

        CALL    XMTC            ; SEND ONE CHARACTER
        CPU     Z80
        JR      XMTS1           ; GET NEXT
        CPU     8080

XMTS2   ANI     01111111B       ; TOSS TERMINATOR BIT
        CALL    XMTC            ; OUTPUT LAST CHARACTER
        PUSH    D               ; GET RETURN ADDRESS
        RET


;;      XRVM - EXIT REVERSE VIDEO MODE
;
;       *XRVM* CLEARS THE REVERSE VIDEO MODE FLAG
;
;
;       ENTRY   (D,E) = MODEA
;
;       EXIT    NONE
;
;       USES    A,F


XRVM    EQU     $

        XCHG                    ; (H,L) = MODEA
        CPU     Z80
        RES     IB.RV,(HL)      ; RESET FLAG
        RET

;;      VPARD60 - VIDEO PARAMETER DATA
;
;       THE CONTENTS OF VPARD60 ARE COPIED TO THE CTRC BY *ICRT* IF
;       THE POWER UP CONFIGURATION SWITCH IS SET TO THE 60HZ POSITION

VPARD60 EQU     $
        DB      96              ; HORIZONTAL TOTAL
        DB      80              ; HORIZONTAL DISPLAYED
        DB      84              ; HORIZONTAL SYNC POSITION
        DB      8               ; HORIZONTAL SYNC WIDTH
        DB      25              ; VERTICAL TOTAL
        DB      4               ; VERTICAL TOTAL ADJUST
        DB      24              ; VERTICAL DISPLAYED
        DB      25              ; VERTICAL SYNC POSITION
        DB      0               ; INTERLACE SCAN
        DB      9               ; MAXIMUM SCAN LINE
        DB      01001000B       ; FAST BLINK CURSOR STARTING AT LINE 8
        DB      8               ; CURSOR END AT LINE 8
        DB      0               ; MEMORY STARTING ADDRESS (MSB)
        DB      0               ; MEMORY STARTING ADDRESS (LSB)
        DB      0               ; CURSOR STARTING ADDRESS (MSB)
        DB      0               ; CURSOR STARTING ADDRESS (lSB)

;;      VPAR50 - VIDEO PARAMETER DATA
;
;       THE CONTENTS OF VPARD65 ARE COPIED TO THE CTRC BY *ICRT* IF
;       THE POWER UP CONFIGURATION SWITCH IS SET TO THE 50HZ POSITION

VPARD50 EQU     $
        DB      96              ; HORIZONTAL TOTAL
        DB      80              ; HORIZONTAL DISPLAYED
        DB      84              ; HORIZONTAL SYNC POSITION
        DB      8               ; HORIZONTAL SYNC WIDTH
        DB      30              ; VERTICAL TOTAL
        DB      7               ; VERTICAL TOTAL ADJUST
        DB      24              ; VERTICAL DISPLAYED
        DB      27              ; VERTICAL SYNC POSITION
        DB      0               ; INTERLACE SCAN
        DB      9               ; MAXIMUM SCAN LINE
        DB      01001000B       ; FAST BLINK CURSOR STARTING AT LINE 8
        DB      8               ; CURSOR END AT LINE 8
        DB      0               ; MEMORY STARTING ADDRESS (MSB)
        DB      0               ; MEMORY STARTING ADDRESS (LSB)
        DB      0               ; CURSOR STARTING ADDRESS (MSB)
        DB      0               ; CURSOR STARTING ADDRESS (lSB)

        DB      48H+43H+41H     ; MORAL SUPPORT
        DB      'H19'           ; FINAL CODE IDENTIFIER
        DB      52H,4EH,42H     ; CODE SUPPORT CODE



;;;     RAM ALLOCATIONS

        ORG     40000Q
RAM     EQU     $               ; 256 BYTE SCRATCHPAD RAM AREA
INFI    DS      128             ; INPUT FIFO
;       ERRNZ   INF&1111111B    ; 7 LSB MUST BE ZERO
;       ERRNZ   $-INF/256       ; FIFO MUST RESIDE IN ONE PAGE
IFMAX   EQU     200Q            ; $-INF
OUTF    DS      32              ; OUTPUT FIFO
        ERRNZ   OUTF&11111B     ; 5 LSB MUST BE ZERO
;       ERRNZ   $-OUTF/256      ; FIFO MUST RESIDE IN ONE PAGE
OFMAX   EQU     $-OUTF

IFP     DS      1               ; INPUT FIFO POINTER
IFC     DS      1               ; INPUT FIFO COUNTER
IFCMSK  EQU     IFMAX-1
OFP     DS      1               ; OUTPUT FIFO POINTER
OFC     DS      1               ; OUTPUT FIFO COUNTER
OFCMSK  EQU     OFMAX-1

KBDFMIN EQU     $               ; BEGINNING OF FIFO
KBDF    DS      16              ; KEYBOARD FIFO (8 ENTRIES)
KBDFMAX EQU     $               ; END OF FIFO
KBDFL   EQU     KBDFMAX-KBDFMIN ; KEYBOARD FIFO LENGTH
KBDFP   DS      2               ; KEYBOARD FIFO POINTER

;       VIDEO AND CURSOR POSITIONS
;
SHOME   DS      2               ; SOFTWARE HOME POSITION
CLSA    DS      2               ; CURRENT LINE STARTING ADDRESS
CURHP   DS      1               ; CURSOR HORIZONTAL POSITION
CURVP   DS      1               ; CURSOR VERTICAL POSITION

CURAD   DS      2               ; CURSOR ADDRESS (ACTUAL MEMORY ADDRESS)
CURMAX  EQU     37777Q          ; MAXIMUM VALUE OF CURAD FOR CRTC (11 BITS)

;       VIDEO PARAMETERS TO BE SENT TO THE CRTC BY *NMI*
;

VI.VD   DS      1               ; VERTICAL DISPLAYED VALUE
VI.CSE  DS      2               ; CURSOR START/OFF AND END VALUES
VI.SA   DS      2               ; VIDEO START ADDRESS (LIMIT 2K)
VI.CA   DS      2               ; CURSOR ADDRESS

CSA     DS      2               ; CURSOR SAVED ADDRESS

;       MODE AND STATUS DEFINITIONS
;
MODEA   DS      1               ; MODE REGISTER A
MA.RV   EQU     10000000B       ; REVERSE VIDEO MODE
MA.ICM  EQU     01000000B       ; INSERT CHARACTER MODE
MA.BRK  EQU     00100000B       ; BREAK KEY FLAG
MA.CD   EQU     00010000B       ; CURSOR DISABLED
MA.RVP  EQU     00001000B       ; REVERSE VIDEO PRESENT
MA.     EQU     00000100B
MA.GRPH EQU     00000010B       ; IN GRAPHICS MODE
MA.HSM  EQU     00000001B       ; HOLD SCREEN MODE

MODEB   DS      1               ; MODE REGISTER B
MB.CBLK EQU     00000001B       ; CURSOR = BLOCK
MB.NOTK EQU     00000010B       ; NO TICK ON KEYBOARD
MB.WRAP EQU     00000100B       ; WRAP AROUND AT END OF LINE
MB.ALF  EQU     00001000B       ; AUTO LINE FEED ON CARRIAGE RETURN
MB.ACR  EQU     00010000B       ; AUTO CARRIAGE RETURN ON LINE FEED
MB.ANSI EQU     00100000B       ; ANSI ESCAPE MODE
MB.KPDS EQU     01000000B       ; KEYPAD SHIFTED
MB.KPDA EQU     10000000B       ; KEYPAD ALTERNATE

MODEI   DS      1               ; MODE REGISTER 1 (INTERNAL ONLY)
MI.PWE  EQU     00000001B       ; PREVIOUS WAS AN ESCAPE CHARACTER
MI.XMTM EQU     00000010B       ; TRANSMIT MODE (FORCE TERMINAL ON LINE)
MI.KID  EQU     00000100B       ; KEYBOARD INPUT DISABLED
MI.ONLN EQU     00001000B       ; TERMINAL ON LINE
MI.XOFF EQU     00010000B       ; XOFF SENT
MI.     EQU     00100000B
MI.PFP  EQU     01000000B       ; PROTECTED FIELDS PRESENT
MI.25L  EQU     10000000B       ; 25TH LINE ENABLED

MODES   DS      1               ; MODE REGISTER S (SERIAL I/O)
MS.BR   EQU     00001111B       ; BAUD RATE TABLE VECTOR
MS.PEN  EQU     00010000B       ; PARITY ENABLE
MS.EPS  EQU     00100000B       ; EVEN PARITY SELECT
MS.SPS  EQU     01000000B       ; STICK PARITY SELECT
MS.FDX  EQU     10000000B       ; FULL DUPLEX

HSMLC   DS      1               ; HOLD SCREEN MODE LINE COUNTER

PSDW    DS      16              ; PARAMETER STRING DECODING WORK AREA
PSDWE   EQU     $&255           ; LSB OF END ADDRESS +1 OF WORK AREA

        ORG     174000Q
VRAMS   EQU     $               ; VIDEO RAM STARTING ADDRESS
HOMAX   EQU     177777Q-VRAMS   ; MAXIMUM VIDEO ADDRESS MASK
