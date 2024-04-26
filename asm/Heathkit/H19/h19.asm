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
IB.RV   EQU     7*8             ; REVERSE VIDEO MODE = BIT 7
IB.ICM  EQU     6*8             ; INSERT CHARACTER MODE = BIT 6
IB.KPDA EQU     7*8             ; KEYPAD ALTERNATE MODE = BIT 5
IB.KPDS EQU     6               ; KEYPAD SHIFTED MODE = BIT 4
IB.ONLN EQU     3               ; KEYPAD SHIFTED MODE = BIT 3
IB.BRK  EQU     2*8             ; BREAK KEY = BIT 2
IB.GRPH EQU     1*8             ; TERMINAL IN GRAPHICS MODE = BIT 1
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

        CALL    FCIC            ; GET CHARACTER FROM INPUT FIFO
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
        JMP     AKI1.5          ; CONTINUE PAST NMI ROUTINE

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
        JR      AKI1.7          ; CONTINUE
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
        JR      Z,KCE4          ; IF CONTROL KEY NOT STRUCK
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
        DB      ESC,'[','P'+0200Q
        RET

KE10.03 CALL    PSOF            ; PUT STRING IN OUTPUT BUFFER
        DB      ESC,'[','P'+0200Q
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

        CALL    PSID            ; ELSE PUT STRING IN INPUT FIFO
        DB      ESC,'[','4','1'+200Q
        RET

KCE10.1 CALL    PSOF            ; PUT STRING IN OUTPUT FIFO
        DB      ESC,'[','4','1'+200Q
        RET

        CPU     Z80
KCE10.2 BIT     IB.KCB,D        ; CHECK FOR CONTROL KEY
        JR      Z,KCE10.3       ; IF NO CONTROL KEY
        CPU     8080

        CALL    PSIF            ; ELSE PUT STRING IN INPUT FIFO
        DB      ESC,'[','4','H'+200Q
        RET

KCE10.3 CALL    PSOD            ; PUT STRING IN OUTPUT FIFO
        DB      ESC,'[','4','H'+200Q
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
        JMP     PCOST           ; PLACE LAST CHARACTER IN FIFO

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

        CPI     'A'             ; TEST FOR < LOWER CASE A
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
;       TABLE ENTRYS ARE: KEY VALUE FOLLOWED BY ASCII VALUE. *ESCF* IS USED
;       TO SIGNAL THAT AN ESCAPE CODE IS TO PRECEED THE SEVEN BIT ASCII VALUE

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
;       THE 12 KEY NUMERIC KEY PAD.  TABLE ENTRYS ARE: KEY VALUE,
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
        DB      34Q,CR,CR,ESCF+'M',ESCF+'N'
KAE2W   EQU     5               ; TABLE WIDTH = 5 BYTES
KAE2L   EQU     ($-KAE2)/KAE2W

;       *KAE3* CONTAINS SHIFT VALUES FOR KEYS WHERE THE SHIFTED VALUE OF THE
;       KEY CANNOT BE FORMED BY A NORMAL ASCII BIT SHIFT
;
;       TABLE ENTRYS ARE: UNSHIFTED VALUE FOLLOWED BY THE SHIFTED VALUE


KAE3
        DB      "'",'.'
        DB      '-','_'
        DB      '0','}'
        DB      '2','@'
        DB      '6','~'
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
        CALL    MAIN1           ; SEE IF THERE IS A CHARACTER TO OUTPUT
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
        CPU     Z80
        JR      MB.WRAP         ; SEE IF TO WRAP AROUND
        CPU     8080

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
;       THE TABLE ENTRYS ARE: THE SECOND CHARACTER OF THE ESCAPE SEQUENCE,
;       FOLLOWED BY THE ADDRESS OF THE ROUTINE WHICH PERFORMS THE
;       REQUESTED OPERATION

ESCTAB  EQU     $
        DB      '<'             ; ESC <
        DW      EAM             ; ENTER ANSI MODE

        DB      ESC             ; ESC ESC
        DW      SPWE            ; SET PREVIOUS WAS AN ESCAPE AGAIN

        DB      '='             ; ESC =
        DW      EKAMQ           ; ENTER KEYPAD ALTERNATE MODE

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

        DB      '\'             ; ESC \
        DW      XHSM            ; EXIT HOLD SCREEN MODE

        DB      ']'             ; ESC ]
        DW      XMT25           ; TRANSMIT 25TH LINE (IF ENABLED)

        DB      'b'             ; ESC b (LOWER CASE B)
        DW      EBD             ; ERASE BEGINNING OF DISPLAY

        DB      'j'             ; ESC j (LOWER CASE J)
        DW      SCP             ; SAVE CURSOR POSITION

        DB      'k'             ; ESC k (LOWER CASE K)
        DW       USCP           ; UNSAVE CURSOR POSITION

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

        DW      'w'             ; ESC w (LOWER CASE W)
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
;       TABLE ENTRYS ARE THE SECOND CHARACTER FOLLOWED BY THE ADDRESS
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
;       TABLE ENTRYS ARE: THE CONTROL CHARACTER FOLLOWED BY THE ADDRESS
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
A1SM1   MOV     A,B             ; GET PN
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
        JR      NZ,AXDN1        ; IF NOT DONE
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
        JZ      FDL             ; IF ZERO, DEFAULT TO ONE

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

        PUSH    H               ; SAVE NEW LINE STARTING ADDRESS
        LXI     D,80            ; ADD 80 FOR BEGINNING OF 26TH LINE
        DAD     D
        XCHG                    ; (D,E) = BEGINNING OF 26TH LINE
        POP     H               ; (H,L) = BEGINNING OF 25TH LINE
        PUSH    H               ; SAVE AGAIN FOR LATER
        MVI     D,80/16         ; SET MOD 16 COUNT FOR ONE LINE
        CALL    CPM16           ; COPY LINE
        POP     H               ; (H,L) = BEGINNING OF 25TH LINE

PLF0.5  MVI     B,80/60         ; SET MOD-16 ERASE COUNT TO 5 (80 CHARACTERS)
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
        OUT     MB.BELL
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

















;;;     RAM ALLOCATIONS

        ORG     40000Q
RAM     EQU     $               ; 256 BYTE SCRATCHPAD RAM AREA
INF     DS      128             ; INPUT FIFO
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
OFCMASK EQU     OFMAX-1

KBDFMIN EQU     $               ; BEGINNING OF FIFO
KBDF    DS      16              ; KEYBOARD FIFO (8 ENTRYS)
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

CASE    DS      2               ; CURSOR SAVED ADDRESS

;       MODE AND STATUS DEFINITIONS
;
MODEA   DS      1               ; MODE REGISTER A
MA.RV   EQU     10000000B       ; REVERSE VIDEO MODE
MA.ICM  EQU     0100000B        ; INSERT CHARACTER MODE
MA.BRK  EQU     0010000B        ; BREAK KEY FLAG
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
MB.KBDS EQU     01000000B       ; KEYPAD SHIFTED
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
MS.SPS  EQU     0100000B        ; STICK PARITY SELECT
MS.FDX  EQU     10000000B       ; FULL DUPLEX

HSMLC   DS      1               ; HOLD SCREEN MODE LINE COUNTER

PSDW    DS      16              ; PARAMETER STRING DECODING WORK AREA
PSDWE   EQU     $&255           ; LSB OF END ADDRESS +1 OF WORK AREA

        ORG     174000Q
VRAMS   EQU     $               ; VIDEO RAM STARTING ADDRESS
HOMAX   EQU     177777Q         ; MAXIMUM VIDEO ADDRESS MASK
