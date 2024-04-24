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
IB.ESC  EQU     7*8             ; ESCAPE CODE FLAG = BIT 7
IB.ETRE EQU     1               ; ENABLE TRANSMITTER REGISTER EMPTY INTERRUPT
IB.IFF  EQU     7*8             ; INPUT FIFO FLAG = BIT 7
IB.KCB  EQU     7               ; KEYBOARD CONTROL KEY BIT = BIT 7
IB.KSP  EQU     0*8             ; KEYBOARD SHIFT KEY BIT = BIT 0
IB.HSM  EQU     0*8             ; HOLD SCREEN MODE = BIT 7
IB.RV   EQU     7*8             ; REVERSE VIDEO MODE = BIT 7
IB.ICM  EQU     6*8             ; INSERT CHARACTER MODE = BIT 6
IB.KPDA EQU     7*8             ; KEYPAD ALTERNATE MODE = BIT 5
IB.KPDS EQU     6               ; KEYPAD SHIFTED MODE = BIT 4
IB.ONLN EQU     3               ; KEYPAD SHIFTED MODE = BIT 3
IB.BRK  EQU     2*8             ; BREAK KEY = BIT 2
IB.GRPH EQU     1*8             ; TERMINAL IN GRAPHICS MODE = BIT 1
IB.PWE  EQU     0*8             ; PREVIOUS CHARACTER WAS AN ESCAPE = BIT 0
IB.XOFF EQU     4*8             ; XOFF SENT TO HOST = BIT 4
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

        ERRNZ   *-70Q           ; Z80 MODE ONE INTERRUPT ADDRESS
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
        ERRNZ   KB.ONLN-MI.ONLN ; SWITCH AND MODE FLAG MUST BE THE SAME
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

        ERRNZ   *-146A

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
        MVI     A,VE.CE         ; SET CURSOR END ADDRESS
        OUT     VP.AR
        MOV     A,L             ; OUTPUT CURSOR END INFO
        OUT     VP.REGO

;       UPDATTE VIDEO HOME ADDRESS
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
        LHLD    CI.CA           ; GET CURSOR ADDRESS
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
        JR      AK11.7          ; CONTINUE
        CPU     8080

AK11.5  ANI     377Q-MA.BRK     ; MAKE SURE BREAK FLAG IS OFF
        STA     MODEA
        IN      AP.LCR          ; CLEAR ANY BREAK
        ANI     377Q-AB.SBRK
        OUT     AP.LCR

AK11.7  IN      AP.IIR          ; SEE IF ACE WAS SOURCE OF INTERRUPT
        CPI     AB.RDAI         ; SEE IF THERE IS RECEIVED DATA AVAILABLE
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
        JR      AK11.7          ; CONTINUE
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
        JR      AK16            ; IF FIFO NOT FULL, EXIT WITHOUT WELL
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

AK13    IN      IP.IER          ; INPUT ACE INTERRUPT ENABLE REGISTER
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
        JR      Z.AKI6          ; IF NO STROBE
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
        JR      NZ,AK16         ; IF DISABLED, EXIT
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
        JR      NZ,KDE2         ; IF NOT

        BIT     IB.KSB,E        ; ELSE, SEE IF SHIFT KEY ALSO
        JR      Z,KCE2          ; IF NOT SHIFT KEY, JUST SEND AN ERM
        CPU     8080

        LDA     MODEB           ; GET MODE FLAGS
        ANI     MB.ANSI         ; IN ANSI MODE?
        CPU     Z80
        JR      Z,KCE1.4        ; IF NOT IN ANSI MODE

        BIT     IB.KCB,D        ; SEE IF CONTROL KEY WAS DOWN
        JR      Z,KDE1.2        ; IF NO CONTROL KEY
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
        BIT     IB.KCB,D        ; TEST CONTROL KEY FOR PLACEMENT OF THIS CHARACTER
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

KDE10.07
        CPI     ESCF+'@'        ; CHECK FOR INSERT CHARACTER CODE
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
        JR      Z,KDE10.1       ; IF NOT CONTROL KEY
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
        JR      Z,KDE13.5       ; IF NO SHIFT
        CPU     8080

        PUSH    D               ; SAVE KEYBOARD VALUES
        LXI     H,KAE3          ; ELSE POINT TO EQUIV TABLE #3 FOR SPEC SHIFTS
        MVI     D,KAE3L         ; SET TABLE LENGTH
        MVI     E,KAE3W         ; SET TABLE WIDTH
        CALL    STAB            ; SEARCH TABLE
        POP     D               ; (D,E) = KEYWORD VALUES

        CPU     Z80
KCE13.5 BIT     IB.CPLK,E       ; TEST FOR 'CAPS LOCK' ON
        JR      Z,KCE14         ; IF CAP SLOCK NOT ON
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
        
