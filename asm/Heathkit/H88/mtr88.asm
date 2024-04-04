        INCLUDE macros.mac

;       MTR88 - H88 MONITOR
;
;       MTR88 IS AN ADAPTATION OF PAM/8 ORIGINALLY WRITTEN FOR THE
;       HEATH H8 COMPUTER BY J. G. LETWIN IN 1976 AND MODIFIED BY
;       R. N. BORCHARDT IN 1979 FOR USE IN THE HEATH H88/H89
;       COMPUTERS.

;
;       MTR88 PROVIDES COMPATIBILITY WITH PAM/8 SUCH THAT ALL ROUTINES
;       HAVE RETAINED PREVIOUSLY DESCRIBED ENTRY POINTS AND ENTRY AND
;       EXIT CONDITIONS. ROUTINES WHICH ARE NOT APPLICABLE SUCH AS
;       THOSE PERTAINING TO THE FRONT PANEL DISPLAY HAVE BEEN DELETED.
;
;
;       COPYRIGHT  05/1976, WINTEK CORPORATION.
;                           902 N. 9TH ST.
;                           LAFAYETTE, IND.
;
;       COPYRIGHT  01/1979, HEATH COMPANY
;                           BENTON HARBOR, MI.
;

ROMDD   EQU     14000Q          ; HDOS BOOT ADDRESS

;       MTR88 - H88/H89 MONITOR.
;
;       THIS PROGRAM RESIDES (IN ROM) IN THE LOW 2048 BYTES OF THE HEATH
;       H88/H89 COMPUTERS.

;       INTERRUPTS.
;
;       MTR88 IS THE PRIMARY PROCESSOR FOR ALL INTERRUPTS.
;       THEY ARE PROCESSED AS FOLLOWS:
;
;       RST     USE
;
;       0       MASTER CLEAR. (NEVER USED FOR I/O OR REST)
;
;       1       CLOCK INTERRUPT. NORMALLY TAKEN BY MTR88,
;               SETTING BIT *UO.CLK* IN BYTE *MFLAG* ALLOWS
;               USER PROCESSING (VIA A JUMP THROUGH *UIVEC*).
;               UPON ENTRY OF THE USER ROUTINE, THE STACK
;               CONTAINS:
;               (STACK+0)  = RETURN ADDRESS (TO MTR88)
;               (STACK+2)  = (STACKPTR+14)
;               (STACK+4)  = (AF)
;               (STACK+6)  = (BC)
;               (STACK+8)  = (DE)
;               (STACK+10) = (HL)
;               (STACK+12) = (PC)
;               THE USER'S ROUTINE SHOULD RETURN TO MTR88 VIA
;               A *RET* WITHOUT ENABLING INTERRUPTS.
;
;       2       SINGLE STEP INTERRUPTS RECEIVED WHEN IN
;               USER MODE CAUSES A JUMP THROUGH *UIVEC*+3.
;               STACK UPON USER ROUTINE ENTRY:
;               (STACK+0)  = (STACKPTR+12)
;               (STACK+2)  = (AF)
;               (STACK+4)  = (BC)
;               (STACK+6)  = (DE)
;               (STACK+8)  = (HL)
;               (STACK+10) = (PC)
;               THE USER'S ROUTINE SHOULD HANDLE IT'S OWN RETURN
;               FROM THE INTERRUPT.  THAT IS, *EI* FOLLOWED BY *RET*.
;
;       THE FOLLOWING INTERRUPTS ARE VECTORED DIRECTLY THROUGH *UIVEC*.
;       THE USER ROUTINE MUST HAVE SETUP A JUMP IN *UIVEC* BEFORE ANY
;       OF THESE INTERRUPTS MAY OCCUR. RETURN IS VIA *EI* AND THEN *RET*.
;
;       3       I/O 3. CAUSES A DIRECT JUMP THROUGH *UIVEC*+6
;
;       4       I/O 4. CAUSES A DIRECT JUMP THROUGH *UIVEC*+9
;
;       5       I/O 5. CAUSES A DIRECT JUMP THROUGH *UIVEC*+12
;
;       6       I/O 6. CAUSES A DIRECT JUMP THROUGH *UIVEC*+15
;
;       7       I/O 7. CAUSES A DIRECT JUMP THROUGH *UIVEC*+18

;       ASSEMBLY CONSTANTS

;       IO PORTS

;       ALL REFERENCES TO THE H8 FRONT PANEL PORTS ARE TRAPPED BY THE
;       Z80 NMI OF THE H88/H89.  OP.CTL WILL STILL PERFORM AS IN AN H8
;       IN RESPECT TO THE CLOCK AND SINGLE STEP CONTROL.  FOR MORE
;       INFORMATION SEE THE NMI ROUTINE.
;
IP.PAD  EQU     360Q            ; PAD INPUT PORT
OP.CTL  EQU     360Q            ; CONTROL OUTPUT PORT
OP.DIG  EQU     360Q            ; DIGIT SELECT OUTPUT PORT
OP.SEQ  EQU     361Q            ; SEGMENT SELECT OUTPUT PORT

;       H88/H89 CONTROL PORT
H88.CTL EQU     362Q            ; H88/H89 PORT FOR CLOCK AND SINGLE STEP
H88B.CK EQU     00000010B       ; 2MS CLOCK ENABLE/DISABLE
H88B.SS EQU     00000001B       ; SINGLE STEP ENABLE/DISABLE

H88.SW  EQU     362Q            ; 8 POSITION DIP SWITCH
H88S.BR EQU     11000000B       ; BAUD RATE SWITCHES
H88S.M  EQU     00100000B       ; MEMORY TEST/NORMAL OPERATION SWITCH

;       CASSETTE PORTS

IP.TPC  EQU     371Q            ; TAPE CONTROL IN
OP.TPC  EQU     371Q            ; TAPE CONTROL OUT
IP.TPD  EQU     370Q            ; TAPE DATA IN
OP.TPD  EQU     370Q            ; TAPE DATA OUT

;       ASCII CHARACTERS

A.SYN   EQU     026Q            ; SYNC CHARACTER
A.STX   EQU     002Q            ; STX CHARACTER
A.BEL   EQU     007Q            ; BELL CHARACTER
A.BKS   EQU     010Q            ; BACKSPACE CHARACTER
A.LF    EQU     012Q            ; LINE FEED CHARACTER
A.CR    EQU     0150            ; CARRIAGE RETURN CHARACTER
A.ESC   EQU     033Q            ; ESCAPE CHARACTER
A.DEL   EQU     177Q            ; DELETE OR RUBOUT CHARACTER

;       FRONT PANEL HARDWARE CONTROL BITS.

CB.SSI  EQU     00010000B       ; SINGLE STEP INTERRUPT
CB.MTL  EQU     00100000B       ; MONITOR LIGHT
CB.CLI  EQU     01000000B       ; CLOCK INTERRUPT ENABLE
CB.SPK  EQU     10000000B       ; SPEAKER ENABLE

;       DISPLAY MODE FLAGS (IN *DSPMOD*)

DM.MR   EQU     0               ; MEMORY READ
DM.MW   EQU     1               ; MEMORY WRITE
DM.RR   EQU     2               ; REGISTER READ
DM.RW   EQU     3               ; REGISTER WRITE

        INCLUDE tape.asm

;       MACHINE INSTRUCTIONS

MI.HLT  EQU     01110110B       ; HALT
MI.RET  EQU     11001001B       ; RETURN
MI.IN   EQU     11011011B       ; INPUT
MI.OUT  EQU     11010011B       ; OUTPUT
MI.LDA  EQU     00111010B       ; LDA
MI.ANI  EQU     11100110B       ; ANI
MI.LXID EQU     00010001B       ; LXI D
MI.JMP  EQU     11000011B       ; JMP
MI.LDXA EQU     11011101B       ; LD IX,  (BYTE A)
MI.LDXB EQU     00100001B       ; LD IX,  (BYTE B)
MI.LDYA EQU     11111101B       ; LD IY,  (BYTE A)
MI.LDYB EQU     00100001B       ; LD IY,  (BYTE B)
MI.EXAF EQU     00001000B       ; EX AD,AF'
MI.JIXA EQU     11011101B       ; JP (IX)  (BYTE A)
MI.JIXB EQU     11101001B       ; JP (IX)  (BYTE B)
MI.JIYA EQU     11111101B       ; JP (IY)  (BYTE A)
MI.JIYB EQU     11111001B       ; JP (IY)  (BYTE B)

;       USER OPTION BITS
;
;       THESE BITS ARE IN SELL MFLAG.
;

UO.HLT  EQU     10000000B       ; DISABLE HALT PROCESSING
UO.NFR  EQU     CB.CLI          ; NO REFRESH OF FRONT PANEL
UO.DDU  EQU     00000010B       ; DISABLE DISPLAY UPDATE
UO.CLK  EQU     00000001B       ; ALLOW PRIVATE INTERRUPT PROCESSING

        INCLUDE u8251.asm       ; DEFINE 8251 USART BITS

        INCLUDE u8250.asm       ; DEFINE 8250 ACE BITS

;       INTERRUPT VECTORS.
;

;       LEVEL 0 - RESET
;
;       THIS 'INTERRUPT' MAY NOT BE PROCESSED BY A USER PROGRAM.

INIT0   JMP     INIT0X          ; DO H88 EXTENSION OF INITIALIZATION
INIT0.0 LXI     H,PRSRAM+PRSL-1 ; (HL) = RAM DESTINATION FOR CODE
        JMP     INIT            ; INITIALIZE

        ERRPL   INIT-1000Q      ; BYTE IN WORD 10A MUST BE 0

;       LEVEL 1 - CLOCK

INT1    EQU     10Q             ; INTERRUPT ENTRY POINT

        ERRNZ   *-110           ; INT0 TAKES UP ONE BYTE

        CALL    SAVALL          ; SAVE USER REGISTERS
        MVI     D,0
        JMP     CLOCK           ; PROCESS CLOCK INTERRUPT
        ERRPL   CLOCK-1000Q     ; EXTRA BYTE MUST BE 0

;       LEVEL 2 - SINGLE STEP

INT2    EQU     20Q             ; LEVEL 2 ENTRY

        ERRNZ   *-21Q           ; INT1 TAKES EXTRA BYTE

        CALL    SAVALL          ; SAVE REGISTERS
        LDAX    D               ; (A) = (CTLFLG)
;       SET     CTLFLG
        JMP     STPRTN          ; STEP RETURN

;       I/O INTERRUPT VECTORS.
;
;       INTERRUPTS 3 THROUGH 7 ARE AVAILABLE FOR GENERA I/O USE.
;
;       THESE INTERRUPTS ARE NOT SUPPORTED BY MTR88, AND SHOULD
;       NEVER OCCUR UNLESS THE USER HAS SUPPLIED HANDLER ROUTINES
;       (THROUGH UIVEC).

        ORG     30Q

INT3    JMP     UIVEC+6         ; JUMP TO USER ROUTINE

        DB      '44440'         ; HEATH PART NUMBER 444-10

        ORG     40Q

INT4    JMP     UIVEC+9         ; JUMP TO USER ROUTINE

        DB      40Q,122Q,116Q,102Q,44Q ; SUPPORT CODE

        ORG     50Q

INT5    JMP     UIVEC+12        ; JUMP TO USER ROUTINE


;       DLY - DELAY TIME INTERVAL
;
;       ENTRY   (A) = MILLISECOND DELAY COUNT/2
;       EXIT    NONE
;       USES    A,F

        ERRNZ   *-53A

DLY     PUSH    PSW             ; SAVE COUNT
        XRA     A               ; DONT SOUND HORN
        JMP     HRN0            ; PROCESS AS HORN

        ORG     60Q

INT6    JMP     UIVEC+15        ; JUMP TO USER ROUTINE

GO.     MVI     A,CB.SSI+CB.CLI+CB.SPK ; OFF MONITOR MODE LIGHT
        JMP     SST1            ; RETURN TO USER PROGRAM

        ORG     70Q

INT7    JMP     UIVEC+18        ; JUMP TO USER ROUTINE

;       INIT - INITIALIZE SYSTEM
;
;       INIT IS CALLED WHENEVER A HARDWARE MASTER-CLEAR IS INITIATED.
;
;       SETUP MTR88 CONTROL CELLS IN RAM.
;       DECODE HOW MUCH MEMORY EXISTS. SETUP STACKPOINTER, AND
;       ENTER THE MONITOR LOOP.
;
;       ENTRY   FROM MASTER CLEAR
;       EXIT    INTO MTR88 MAIN LOOP

        ERRNZ   *-73Q

INIT    LDAX    D               ; COPY *PRSROM* INTO RAM
        MOV     M,A             ; MOVE BYTE
        DCX     H               ; DECREMENT DESTINATION
        INR     E               ; INCREMENT SOURCE
        JNZ     INIT            ; IF NOT DONE

SINCR   EQU     4000Q           ; SEARCH INCREMENT

        MVI     D,SINCR/256     ; (DE) = SEARCH INCREMENT
        LXI     H,START-SINCR   ; (HL) = FIRST RAM - SEARCH INCREMENT

;       DETERMINE MEMORY LIMIT

INIT1   MOV     M,A             ; RESTORE VALUE READ
        DAD     D               ; INCREMENT TRIAL ADDRESS
        MOV     A,M             ; (A) = CURRENT MEMORY VALUE
        DCR     M               ; TRY TO CHANGE IT
        CMP     M
        JNZ     INIT1           ; IF MEMORY CHANGED

INIT2   DCX     H

        SPHL                    ; SET STACKPOINTER = MEMORY LIMIT -1

        PUSH    H               ; SET *PC* VALUE ON STACK
        LXI     H,ERROR
        PUSH    H               ; SET 'RETURN ADDRESS'

;       CONFIGURE LOAD/DUMP UART

        MVI     A,UMI.1B+UMI.L8+UMI.16X
        OUT     OP.TPC          ; SET 8 BIT, NO PARITY, 1 STOP, 16X

;       SAVALL - SAVE ALL REGISTERS ON STACK.
;
;       SAVALL IS CALLED WHEN AN INTERRUPT IS ACCEPTED, IN ORDER TO
;       SAVE THE CONTENTS OF THE REGISTERS ON THE STACK.
;
;       ENTRY   CALLED DIRECTLY FROM INTERRUPT ROUTINE
;       EXIT    ALL REGISTERS PUSHED ON STACK.
;               IF NO YET IN MONITOR MODE, REGPTR = ADDRESS OF REGISTERS
;               ON STACK.
;               (DE) = ADDRESS OF CTLFLG

        ERRNZ   *-132Q

SAVALL  XTHL                    ; SET H,L ON STACK TOP
        PUSH    D
        PUSH    B
        PUSH    PSW
        XCHG                    ; (D,E) = RETURN ADDRESS
        LXI     H,10
        DAD     SP              ; (H,L) = ADDRESS OF USERS SP

;       REPLACE THE INSTRUCTIONS WITH A JUMP AROUND THE NMI VECTOR JUMP
;
;       PUSH    H               ; SET ON STACK AS REGISTER
;       PUSH    D               ; SET RETURN ADDRESS
;       LXI     D,CTLFLG
;       LDAX    D               ; (A) = CTLFLG

        JMP     SAVALLX         ; GO TO SAVALL EXTENSION

;       ENTRY POINT FOR THE Z80 NMI
;

        ERRNZ   *-66H           ; Z80 NMI ADDRESS

NMIENT  JMP     NMI

        ERRNZ   SAVALLR-151Q    ; DO NOT CHANGE ORGANIZATION

SAVALLR                         ; SAVALL EXTENSION RETURN ADDRESS

        CMA
        ANI     CB.MTL+CB.SSI   ; SAVE REGISTER ADDR IF USER OR SINGLE STEP
        RZ                      ; RETURN IF WAS INTERRUPT OF MONITOR LOOP
        LXI     H,2
        DAD     SP              ; (H,L) = ADDRESS OF 'STACKPTR' ON STACK
        SHLD    REGPTR
        RET

;       CUI - CHECK FOR USER INTERRUPT PROCESSING.
;
;       CUI IS CALLED TO SEE IF THE USER HAS SPECIFIED PROCESSING
;       FOR THE CLOCK INTERRUPT.

        ERRNZ   *-165Q

;       SET     MFLAG           ; REFERENCE TO MFLAG
CUI1    LDAX    B               ; (A) = MFLAG
        ERRNZ   UO.CLK-1        ; CODE ASSUMED = 01
        RRC
        CC      UIVEC           ; IF SPECIFIED, TRANSFER TO USER

;       RETURN TO PROGRAM FROM INTERRUPT.

        ERRNZ  *-172Q

INTXIT  POP     PSW             ; REMOVE FAKE 'STACK REGISTER'
        POP     PSW
        POP     B
        POP     D
        POP     H
        EI
        RET

;       CLOCK - PROCESS CLOCK INTERRUPT
;
;       CLOCK IS ENTERED WHENEVER A 2-MILLISECOND CLOCK INTERRUPT IS
;       PROCESSED.
;
;       TICCNT IS INCREMENTED EVERY INTERRUPT.

        ERRNZ   *-201Q

CLOCK   LHLD    TICCNT
        INX     H
        SHLD    TICCNT          ; INCREMENT TICCOUNT

        LDA     CTLFLG          ; CLEAR CLOCK INTERRUPT FLIP-FLOP
        OUT     OP.CTL

;       EXIT CLOCK INTERRUPT.

        LXI     B,CTLFLG
        LDAX    B               ; (A) = CTLFLG
        ANI     CB.MTL
        JNZ     INTXIT          ; IF IN MONITOR MODE
        DCX     B
        ERRNZ   CTLFLG-MFLAG-1
        LDAX    B               ; (A) = MFLAG
        ERRNZ   UO.HLT-200Q     ; ASSUME HIGH-ORDER
        RAL
        JC      CLK4            ; SKIP IT

;       NOT IN MONITOR MODE. CHECK FOR HALT

        MVI     A,10            ; (A) = INDEX OF *P* REG
        CALL    LRA.            ; LOCATE REGISTER ADDRESS
        MOV     E,M
        INX     H
        MOV     D,M             ; (D,E) = PC CONTENTS
        DCX     D
        LDAX    D
        CPI     MI.HLT          ; CHECK FOR HALT
        JNZ     CUI1

        MVI     A,A.BEL         ; DING BELL
        CALL    WCC
        MVI     A,'H'           ; "H" FOR HALT
        CALL    WCC
        JMP     ERROR

;       JZ      ERROR           ; IF HALT, BE IN MONITOR MODE

;       NONE OF THE ABOVE, SO ALLOW USER PROCESSING OF CLOCK INTERRUPT

CLK4    JMP     CUI1            ; ALLOW USER PROCESSING OF CLOCK

;       NOTE: SOURCE CODE FOR THE FOLLOWING WAS NOT PUBLISHED.
;       PRESUMABLY IT WAS PART OF THE MEMORY TEST AND FLOPPY DISK
;       ROTATIONAL SPEED TEST ROUTINES MENTIONED IN THE MANUAL.

        ORG        322Q

;       ERROR - COMMAND ERROR.
;
;       ERROR IS CALLED AS A 'BAIL-OUT' ROUTINE.
;
;       IT RESETS THE OPERATIONAL MODE, AND RESTORES THE STACKPOINTER.
;
;       ENTRY   NONE
;       EXIT    TO HTR LOOP
;               CTLFLG SET
;               MFLAG CLEARED
;       USES    ALL

        ERRNZ   *-322Q

ERROR   LXI     H,MFLAG
        MOV     A,M             ; (A) = MFLAG
        ANI     377Q-UO.DDU-UO.NFR ; RE-ENABLE DISPLAYS
        MOV     M,A             ; REPLACE
        INX     H
        MVI     M,CB.SSI+CB.MTL+CB.CLI+CB.SPK ; RESTORE *CTLFLG*
        ERRNZ   CTLFLG-.MFLAG-1
        EI
        LHLD    REGPTR
        SPHL                    ; RESTORE STACK POINTER TO EMPTY STATE
        CALL    ALARM

;       MTR - MONITOR LOOP.
;

        ERRNZ   *-344Q

MTR     EI

MTR1    LXI     H,MTR1
        PUSH    H               ; SET 'MTR1' AS RETURN ADDRESS
        LXI     H,MSG.PR        ; TYPE PROMPT MESSAGE
        CALL    TYPMSG

MTR.2   CALL    RCC             ; READ A CONSOLE CHARACTER
        ANI     01011111B       ; MAKE SURE ITS UPPER CASE TO MATCH TABLE
        LXI     H,MTRA          ; LOOK UP CHARACTER IN *MTRA*
        MVI     B,MTRAL         ; (B) = LENGTH OF TABLE
MTR.3   CMP     M               ; SEE IF CHARACTER FROM CONSOLE = TABLE ENTRY
        JZ      MTR.4           ; IF EQUAL

        INX     H               ; POINT TO NEXT TABLE ENTRY
        INX     H
        INX     H
        DCR     B               ; SEE IF PAST END OF TABLE
        JNZ     MTR.3           ; IF NOT PAST

        MVI     A,A.BEL         ; ELSE, DING ERROR
        CALL    WCC
        JMP     MTR.2           ; TRY AGAIN

MTR.4   CALL    WCC             ; WRITE CHARACTER BACK TO CONSOLE
        INX     H               ; GET ROUTINE ADDRESS LSB
        MOV     A,M
        INX     H               ; GET MSB
        MOV     H,M
        MOV     L,A             ; (H,L) = ROUTINE ADDRESS

MTRA                            ; JUMP TABLE

        DB      'G'             ; GO TO USER ROUTINE
        DW      GO88

        DB      'L'             ; CASSETTE LOAD
        DW      SRMEM

        DB      'D'             ; SET UP CASSETTE DUMP
        DW      SWMEM

        DB      'S'             ; SUBSTITUTE MEMORY MODE
        DW      SUBM

        DB      'P'             ; PROGRAM COUNTER ALTER MODE
        DW      PCA

        DW      'B'             ; BOOT HDOS
        DW      BOOT

MTRAL   EQU     ($-MTRA)/3      ; NUMBER OF TABLE ENTRYS   /JWT 790507/
;       LON     L

;       SAE - STORE ABUSS AND EXIT.
;
;       ENTRY   (HL) = ABUSS VALUE
;       EXIT    TO (RET)
;       USES    NONE

        ERRNZ   *-1063Q

SAE     SHLD    ABUSS
        RET

;       SRMEM - H88/H89 ENTRY POINT FOR A CASSETTE LOAD
;

SRMEM   LXI     H,MSG.LD        ; COMPLETE MESSAGE
        CALL    TYPMSG
        CALL    WCR             ; WAIT FOR A CARRIAGE RETURN
        JMP     RMEM            ; LOAD TAPE

;       PCA - PROGRAM COUNTER ALTER
;
;       PCA INPUTS AND/OR DISPLAYS THE CURRENT USER PROGRAM VALUE AND ALLOWS
;       A NEW VALUE TO BE ENTERED OR RETAINS THE CURRENT VALUE IF
;       A CR IS TYPED
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,D,E,H,L,F

PCA     LXI     H,MSG.PC        ; COMPLETE PC MESSAGE
        CALL    TYPMSG
        MVI     A,10            ; GET LOCATION OF USER PC
        CALL    LRA.
        MOV     E,M             ; (D,E) = USER PC VALUE
        INX     H
        MOV     D,M
        XCHG                    ; (H,L) = USER PC VALUE

        CALL    IROC            ; INPUT NEXT CHARACTER
        JC      PCA1            ; IF FIRST CHARACTER WAS OCTAL, INPUT NEW PC

        CALL    TOA             ; ELSE, OUTPUT CURRENT VALUE
        CALL    IROC            ; SEE IF USER WANTS TO CHANGE IT NOW
        RNC                     ; IF NOT CHANGE, EXIT

;       ENTER NEW USER PC VALUE

PCA1    XCHG                    ; (H,L) = ADDRESS OF USER PC VALUE
        MVI     D,A.CR          ; END BYTE WITH A RETURN
        CALL    IOA             ; INPUT NEW ADDRESS
        RET                     ; EXIT

;       GO88 - GO TO USER ROUTINE FROM H88 MONITOR
;
;       GO88 WAITS FOR A CARRIAGE RETURN OR A NEW ADDRESS TERMINATED WITH
;       A CARRIAGE RETURN.  IF NO ADDRESS IS ENTERED, GO88 TRANSFERS
;       CONTROL TO THE ADDRESS SPECIFIED BY THE USER PC VALUE

GO88    LXI     H,MSG.GO        ; COMPLETE GO MESSAGE
        CALL    TYPMSG
        CALL    IROC            ; INPUT A RETURN OR AN OCTAL CHARACTER
        JNC     GO88.1          ; IF RETURN, GO TO CURRENT USER PC

        PUSH    PSW             ; ELSE SAVE OCTAL CHARACTER AND FLAGS
        MVI     A,10            ; GET ADDRESS OF USER PC
        CALL    LRA.
        INX     H               ; POINT TO MSB
        POP     PSW             ; GET FIRST CHARACTER BACK
        MVI     D,A.CR          ; END ADDRESS WITH A RETURN
        CALL    IOA             ; INPUT NEW GO ADDRESS
GO88.1  CALL    WCC             ; ECHO RETURN
        MVI     A,A.LF          ; LINE FEED
        CALL    WCC
        JMP     GO              ; EXECUTE USER ROUTINE

;       GO - RETURN TO USER MODE
;
;       ENTRY   NONE

        ERRNZ   *-1222Q

GO      JMP     GO.             ; ROUTINE IS IN WASTE SPACE

;       SSTEP - SINGLE STEP INSTRUCTION.
;
;       ENTRY   NONE

        ERRNZ   *-1225Q

SSTEP                           ; SINGLE STEP
        DI                      ; DISABLE INTERRUPTS UNTIL THE RIGHT TIME
        LDA     CTLFLG
        XRI     CB.SSI          ; CLEAR SINGLE STEP INHIBIT
        OUT     OP.CTL          ; PRIME SINGLE STEP INTERRUPT
SST1    STA     CTLFLG          ; SET NEW FLAG VALUES
        POP     H               ; CLEAN STACK
        JMP     INTXIT          ; RETURN TO USER ROUTINE FOR STEP

;       STPRTN - SINGLE STEP RETURN

        ERRNZ   *-1244Q

STPRTN
        ORI     CB.SSI          ; DISABLE SINGLE STEP INTERRUPTION
        OUT     OP.CTL          ; TURN OFF SINGLE STEP ENABLE
;       SET     CTLFLG
        STAX    D
        ANI     CB.MTL          ; SEE IF IN MONITOR MODE
        JNZ     MTR
        JMP     UIVEC+3         ; TRANSFER TO USER'S ROUTINE


;       RMEM - LOAD MEMORY FROM TAPE
;

        ERRNZ   *-1261Q

RMEM    LXI     H,TPABT
        SHLD    TPERRX          ; SETUP ERROR EXIT ADDRESS
;       JMP     LOAD

;       LOAD - LOAD MEMORY FROM TAPE
;
;       READ THE NEXT RECORD FROM THE CASSETTE TAPE.
;
;       USE THE LOAD ADDRESS IN THE TAPE RECORD.
;
;       ENTRY   (HL) = ERROR EXIT ADDRESS
;       EXIT    USER P-REG (IN STACK) SET TO ENTRY ADDRESS
;               TO CALLER IF ALL OK
;               TO ERROR EXIT IF TAPE ERRORS DETECTED.

        ERRNZ   *-1267Q

LOAD
        LXI     B,1000Q-RT.MI*256-256 ; (BC) = - REQUIRED TYPE AND #
LOA0    CALL    SRS             ; SCAN FOR RECORD COUNT
        MOV     L,A             ; (HL) = COUNT
        XCHG                    ; (DE) = COUNT, (HL) = TYPE AND #
        DCR     C               ; (C) = NEXT #
        DAD     B
        MOV     A,H
        PUSH    B               ; SAVE TYPE AND #
        PUSH    PSW             ; SAVE TYPE CODE
        ANI     177Q            ; CLEAR END FLAG BIT
        ORA     L
        MVI     A,2             ; SEQUENCE ERROR
        JNZ     TPERR           ; IF NOT RIGHT TYPE OR SEQUENCE
        CALL    RNP             ; READ ADDR
        MOV     B,H
        MOV     C,A             ; (BC) = P-REG ADDRESS
        MVI     A,10
        PUSH    D               ; SAVE (DE)
        CALL    LRA.            ; LOCATE REG ADDRESS
        POP     D               ; RESTORE (DE)
        MOV     M,C             ; SET P-REG IN MEM
        INX     H
        MOV     H,B
        CALL    RNP             ; READ ADDRESS
        MOV     L,A             ; (HL) = ADDRESS, (DE) = COUNT
        SHLD    START

LOA1    CALL    RNB             ; READ BYE
        MOV     M,A

; SHOW H88 THAT SOMETHING IS HAPPENING

        CALL    TPDSP           ; DISPLAY TO H88 USER THAT WE ARE LOADING

        INX     H
        DCX     D
        MOV     A,D
        ORA     E
        JNZ     LOA1            ; IF MORE TO GO

        CALL    CTC             ; CHECK TAPE CHECKSUM

;       READ NEXT BLOCK

        POP     PSW             ; (A) = FILE TYPE BYTE
        POP     B               ; (BC) = -(LAST TYPE, LAST #)
        RLC
        JC      TFT             ; ALL DONE - TURN OFF TAPE
        JMP     LOA0            ; READ ANOTHER RECORD

;       DUMP - DUMP MEMORY TO MAG TAPE.
;
;       DUMP SPECIFIED MEMORY RANGE TO MAG TAPE.
;
;       ENTRY   (START) = START ADDRESS
;               (ABUSS) = END ADDRESS
;               USER PC = ENTRY POINT ADDRESS
;       EXIT    TO CALLER.

        ERRNZ   *-1374Q

WMEM
        LXI     H,TPABT
        SHLD    TPERRX          ; SETUP ERROR EXIT

        ERRNZ   *-2002Q

DUMP    MVI     A,UCI.TE
        OUT     OP.TPC          ; SETUP TAPE CONTROL
        MVI     A,A.SYN
        MVI     H,32            ; (H) = # OF SYNC CHARACTERS
WME1    CALL    WNB
        DCR     H
        JNZ     WME1            ; WRITE SYN HEADER
        MVI     A,A.STX
        CALL    WNB             ; WRITE STX
        MOV     L,H             ; (HL) = 00
        SHLD    CRCSUM          ; CLEAR CRC 16
        LXI     H,RT.MI+80H*256+1 ; FIRST AND LAST MI RECORD
        CALL    WNP             ; WRITE HEADER
        LHLD    START
        XCHG                    ; (D,E) = START ADDRESS
        LHLD    ABUSS           ; (H,L) = STOP ADDR
        INX     H               ; COMPUTE WITH STOP+1
        MOV     A,L
        SUB     E
        MOV     L,A
        MOV     A,H
        SBB     D
        MOV     H,A             ; (HL) = COUNT
        CALL    WNP             ; WRITE COUNT
        PUSH    H
        MVI     A,10
        PUSH    D               ; SAVE (DE)
        CALL    LRA.            ; LOCATE P-REG ADDRESS
        MOV     A,M
        INX     H
        MOV     H,M
        MOV     L,A             ; (HL) = CONTENTS OF PC
        CALL    WNP             ; WRITE HEADER
        POP     H               ; (HL) = ADDRESS
        POP     D               ; (DE) = COUNT
        CALL    WNP

WME2    MOV     A,M
        CALL    WNB             ; WRITE BYTE

;       SHOW H88 USER THAT DUMP IS DUMPING

        CALL    TPDSP           ; DISPLAY DUMP

        INX     H
        DCX     D
        MOV     A,D
        ORA     E
        JNZ     WME2            ; IF MORE TO GO

;       WRITE CHECKSUM

        LHLD    CRCSUM
        CALL    WNP             ; WRITE IT
        CALL    WNP             ; FLUSH CHECKSUM
;       JMP     TFT

;       TFT - TURN OFF TAPE.
;
;       STOP THE TAPE TRANSPORT.
;

        ERRNZ   *-2133Q

TFT     XRA     A
        OUT OP.TPC              ; TURN OFF TAPE

;       HORN - MAKE NOISE.
;
;       ENTRY   (A) = (MILLISECOND COUNT)/2
;       EXIT    NONE
;       USES    A,F

        ERRNZ   *-2136Q

ALARM
        CPU     Z80
        JR      ALARMB          ; BRANCH TO A JUMP TO NOISE TO DING BELL
        CPU     8080

        ERRNZ   *-2140Q

HORN    PUSH    PSW
        MVI     A,CB.SPK        ; TURN ON SPEAKER

HRN0    XTHL                    ; SAVE (HL), (H) = COUNT
        PUSH    D               ; SAVE (DE)
        XCHG                    ; (D) = LOOP COUNT
        LXI     H,CTLFLG
        XRA     M
        MOV     E,M             ; (E) = OLD CTLFLG VALUE
        MOV     M,A             ; TURN ON HORN
        MVI     L,TICCNT/256

        MOV     A,D             ; (A) = CYCLE COUNT
        ADD     M
HRN2    CMP     M               ; WAIT REQUIRED TICCOUNTS
        JNZ     HRN2

        JMP     HRNX            ; JUMP TO AN EXTENSION OF HORN SO ROOM
                                ; CAN BE MADE FOR A JUMP TO NOISE

ALARMB  JMP     NOISE           ; SEND A BELL TO THE CONSOLE

;       CTC - VERIFY CHECKSUM.
;
;       ENTRY   TAPE JUST BEFORE CRC
;       EXIT    TO CALLER IF OK
;               TO *TPERR* IF BAD
;       USES    A,F,H,L

        ERRNZ   *-2172Q

CTC     CALL    RNP             ; READ NEXT PAIR
        LHLD    CRCSUM
        MOV     A,M
        ORA     L
        RZ                      ; RETURN IF OK
        MVI     A,1             ; CHECKSUM ERROR
;       JMP     TPERR           ; (B) = CODE

;       TPERR - PROCESS TAPE ERROR
;
;       DISPLAY ERR NUMBER IF LOW BYTE OF ABUSS
;
;       IF ERROR NUMBER EVEN, DON'T ALLOW #
;       IF ERROR NUMBER ODD, ALLOW #
;
;       ENTRY   (B) = PATTERN

        ERRNZ   *-2205Q

TPERR   MOV     B,A             ; (B) = CODE
        CALL    TPERMSG         ; DISPLAY ERROR NUMBER ON CONSOLE
        CALL    TFT             ; TURN OFF TAPE

;       IS #, RETURN (IF PARITY ERROR)

        DB      MI.ANI          ; FALL THROUGH WITH CARRY CLEAR
TER3    MOV     A,B

        RRC
        RC                      ; RETURN IF OK

;       BEEP AND FLASH ERROR NUMBER

TER1    CC      ALARM           ; ALARM IF PROPER TIME
        CALL    TPXIT           ; SEE IF #
        IN      IP.PAD
        CPI     00101111B       ; CHECK FOR #
        JZ      TER3            ; IF #
        LDA     TICCNT+1
        RAR                     ; 'C' SET IF 1/2 SECOND
        JMP     TER1

;       TPABT - ABORT TAPE LOAD OR DUMP.
;
;       ENTERED WHEN LOADING OR DUMPING, AND THE '*' KEY
;       IS STRUCK.

        ERRNZ   *-2249Q

TPABT   XRA     A
        OUT     OP.TPC          ; OFF TAPE
        JMP     ERROR

;       TPXIT - CHECK FOR USER FORCED EXIT.
;
;       TPXIT CHECKS FOR AN '*' KEYPAD ENTRY. IF SO, TAKE
;       THE TAPE DRIVER ABNORMAL EXIT.
;
;       ENTRY   NONE
;       EXIT    TO *RET* IF NOT '*'
;                (A) = PORT STATUS
;               TO (TPERRX) IF '*' DOWN
;       USES    A,F

        ERRNZ   *-2252Q

TPXIT   IN      IP.PAD
        CPI     00001111B       ; *
        IN      IP.TPC          ; READ TAPE STATUS
        RNZ                     ; NOT '*', RETURN WITH STATUS
        LHLD    TPERRX

        ERRNZ   *-2264Q

        PCHL                    ; ENTER (TPERRX)

;       SRS - SCAN RECORD START
;
;       SRS READS BYTES UNTIL IT RECOGNIZES THE START OF A RECORD.
;
;       THIS REQUIRES
;       AT LEAST 10 SYNC CHARACTERS
;       1 STX CHARACTER
;
;       THE CRC-16 IS THEN INITIALIZED.
;
;       ENTRY   NONE
;       EXIT    TAPE POSITIONED (AND MOVING), CRCSUM =0
;               (DE) = HEADER BYTES
;               (HA) = RECORD COUNT
;       USES    A,F,D,E,H,L

        ERRNZ   *-2265Q

SRS
SRS1    MVI     D,0
        MOV     H,D
        MOV     L,D             ; (HL) = 0
SRS2    CALL    RNB             ; READ NEXT BYTE
        INR     D
        CPI     A.SYN
        JZ      SRS2            ; HAVE SYN
        CPI     A.STX
        JNZ     SRS1            ; NOT STX - START OVER

        MVI     A,10
        CMP     D               ; SEE IF ENOUGH SYNC CHARACTERS
        JNC     SRS1            ; NOT ENOUGH
        SHLD    CRCSUM          ; CLEAR CRC-16
        CALL    RNP             ; READ LEADER
        MOV     D,H
        MOV     E,A
;       JMP     RNP             ; READ COUNT

;       RNP - READ NEXT PAIR
;
;       RNP READS THE NEXT TWO BYTES FROM THE INPUT DEVICE.
;
;       ENTRY   NONE
;       EXIT    (H,A) = BYTE PAIR
;       USES    A,F,H

        ERRNZ   *-2325Q

RNP     CALL    RNB             ; READ NEXT BYTE
        MOV     M,A
;       JMP     RNB             ; READ NEXT BYTE

;       RNB - READ NEXT BYTE
;
;       RNB READS THE NEXT SINGLE BYTE FROM THE INPUT DEVICE.
;       THE CHECKSUM IS TAKEN FOR THE CHARACTER.
;
;       ENTRY   NONE
;       EXIT    (A) = CHARACTER
;       USES    A,F

        ERRNZ   *-2331Q

RNB     MVI     A,UCI.RO+UCI.ER+UCI.RE ; TURN ON READ FOR NEXT BYTE
        OUT     OP.TPC
RNB1    CALL    TPXIT           ; CHECK FOR #, READ STATUS
        ANI     USR.RXR
        JZ      RNB1            ; IF NOT READY
        IN      IP.TPD          ; INPUT DATA
;       JMP     CRC             ; CHECKSUM

;       CRC - COMPUTE CRC-16
;
;       CRC COMPUTES A CRC-16 CHECKSUM FROM THE POLYNOMIAL
;
;       (X + 1) * (X^15 + X + 1)
;
;       SINCE THE CHECKSUM GENERATED IS A DIVISION REMAINDER,
;       A CHECKSUMED DATA SEQUENCE CAN BE VERIFIED BY RUNNING
;       THE DATA THROUGH CRC, AND THEN RUNNING THE PREVIOUSLY OBTAINED
;       CHECKSUM THROUGH CRC. THE RESULTANT CHECKSUM SHOULD BE 0.
;
;       ENTRY   (CRCSUM) = CURRENT CHECKSUM
;               (A) = BYTE
;       EXIT    (CRCSUM) UPDATED
;               (A) UNCHANGED.
;       USES    F

        ERRNZ   *-2347Q

CRC     PUSH    B               ; SAVE (BC)
        MVI     B,8             ; (B) = BIT COUNT
        PUSH    H
        LHLD    CRCSUM
CRC1    RLC
        MOV     C,A             ; (C) = BIT
        MOV     A,L
        ADD     A
        MOV     L,A
        MOV     A,H
        RAL
        MOV     H,A
        RAL
        XRA     C
        RRC
        JNC     CRC2            ; IF NOT TO XOR
        MOV     A,H
        XRI     200Q
        MOV     H,A
        MOV     A,L
        XRI     5Q
        MOV     L,A
CRC2    MOV     A,C
        DCR     B
        JNZ     CRC1            ; IF MORE TO GO
        SHLD    CRCSUM
        POP     H               ; RETURN (HL)
        POP     B               ; RESTORE (BC)
        RET                     ; EXIT

;       WNP - WRITE NEXT PAIR.
;
;       WNP WRITES THE NEXT TWO BYTES TO THE CASSETTE DRIVE
;
;       ENTRY   (H,L) = BYTES
;       EXIT    WRITTEN.
;       USES    A,F

        ERRNZ   *-3017Q

WNP     MOV     A,H
        CALL    WNB
        MOV     A,L
;       JMP     WNB             ; WRITE NEXT BYTE

;       WNB - WRITE BYTE
;
;       WNB WRITE THE NEXT BYTE TO THE CASSETTE TAPE.
;
;       ENTRY   (A) = BYTE
;       EXIT    NONE.
;       USES    F

        ERRNZ   *-3024A

WNB     PUSH    PSW
WNB1    CALL    TPXIT           ; CHECK FOR *, READ STATUS
        ANI     USR.TXR
        JZ      WNB1            ; IF MORE TO GO
        MVI     A,UCI.ER+UCI.TE ; ENABLE TRANSMITTER
        OUT     OP.TPC          ; TURN ON TAPE
        POP     PSW
        OUT     OP.TPD          ; OUTPUT DATA
        JMP     CRC             ; COMPUTE CRC

;       LRA - LOCATE REGISTER ADDRESS
;
;       ENTRY   NONE.
;       EXIT    (A) = REGISTER INDEX
;               (H,L) = STORAGE ADDRESS
;               (D,E) = (O,A)
;       USES    A,D,E,H,L,F

        ERRNZ   *-3047Q

LRA     LDA     REGI
LRA.    MOV     E,A
        MVI     D,0
        LHLD    REGPTR
        DAD     D               ; (DE) = (REGPTR)+(REGI)
        RET

;       IOA - INPUT OCTAL ADDRESS.
;
;       ENTRY   (H,L) = ADDRESS OF RECEPTION DOUBLE BYTE.
;               (D) = TERMINATING CHARACTER
;       EXIT    NONE
;       USES    A,D,E,H,L,F

        ERRNZ   *-3062Q

IOA     JMP     IOA1
        NOP                     ; RETAIN H8 ORG

;       IOB - INPUT OCTAL BYTE
;
;       READ ONE OCTAL BYTE FROM THE KEYSET.
;
;       ENTRY   (H,L) = ADDRESS OF BYTE TO HOLD VALUE
;               'C' SET IF FIRST DIGIT N (A)
;       EXIT    NONE
;       USES    A,D,E,H,L,F

        ERRNZ   *-3066Q

IOB     MVI     M,0             ; ZERO OUT OLD VALUE
IOB1    CNC     RCC             ; READ CONSOLE CHARACTER

;       SEE IF CHARACTER IS A VALID OCTAL VALUE
;
        CPI     '0'             ; LESS THAN ZERO?
        JC      IOB2            ; IF (A) < 0, SEE IF A TERMINATING CHARACTER
        CPI     '8'             ; GREATER THAN 7?
        JNC     IOB1            ; IF TOO LARGE, TRY AGAIN

;       HAVE AN OCTAL DIGIT
;
        CALL    WCC             ; ECHO CHARACTER
        ANI     00000111B       ; MASK FOR BINARY VALUE
        MOV     E,A             ; (E) = VALUE
        MOV     A,M             ; GET OLD VALUE
        RLC                     ; SHIFT 3
        RLC
        RLC
        JMP     IOB1.5          ; JUMP AROUND AN H88/H89 TO H8 FAKE ROUTINE

;       FAKE OUT ROUTINE FOR CALLERS OF *DOD* FROM THE H8 FRONT PANEL

        ERRNZ    *-3122Q

DOD     INX      H
        INX      H
        INX      H
        RET

;       CONTINUE

IOB1.5  ANI     11111000B       ; TOSS OLD LSB DIGIT
        ORA     E               ; REPLACE WITH NEW VALUE
        MOV     M,A
        JMP     IOB1            ; INPUT ANOTHER CHARACTER

;       CHECK FOR A CARRIAGE RETURN TO TERMINATE BYTE
;
IOB2    CPI     A.CR            ; CARRIAGE RETURN?
        RZ                      ; RETURN IF CARRIAGE RETURN         /JWT 790507/
        XRA     A               ; CLEAR CARRY                       /JWT 790507/
        CPU     Z80
        JR      IOB1            ; GET A NEW CHARACTER               /JWT 790507/
        CPU     8080

; MORE CODE BELOW THAT WAS NOT PUBLISHED. REPLACED WITH DISASSEMBLY OF
; ROM.

        ORG     1660Q

;       RCK - READ CONSOLE KEYPAD
;
;       RCK IS CALLED TO READ A KEYSTROKE FROM THE CONSOLE FRONT PANEL KEYPAD.
;       SINCE THE H88/H89 DOES NOT HAVE A FRONT PANEL, THIS ROUTINE IS PROVIDED
;       ONLY TO MAINTAIN COMPATIBILITY WITH PAM-8.
;       RCK WILL IMMEDIATELY RETURN WITH A VALUE OF 0 (ZERO) IN THE ACCUMULATOR.
;
;       ENTRY   NONE
;       EXIT    (A) = 0
;       USES    A,F

;       RCK MUST HAVE SAME ENTRY AS RCK IN PAM-8
        ERRNZ   *-3260Q

RCK     XRA     A
        RET


;       RCC - READ CONSOLE CHARACTER.
;
;       RCC IS CALLED TO READ A KEYSTROKE FROM THE CONSOLE.
;       IF A RUBOUT/DELETE IS RECEIVED, EXIT IT TO *ERROR*.
;
;       ENTRY   NONE
;       EXIT    TO ERROR - IF A DELETE OR RUBOUT IS ENCOUNTERED
;               TO CALLER - WHEN A KEY IS HIT
;               (A) = ASCII KEY VALUE
;       USES    A,F

RCC

RCC1    IN      SC.ACE+UR.LSR   ; INPUT ACE LINE STATUS REGISTER
        ANI     UC.DR           ; SEE IF THERE IS A DATA READY
        CPU     Z80
        JR      Z,RCC1
        CPU     8080

        IN      SC.ACE+UR.RBR   ; ELSE, INPUT CHARACTER
        ANI     01111111B       ; TOSS ANY PARITY
        CPI     A.DEL
        JZ      ERROR           ; IF RUBOUT, EXIT TO CALLER

;       WCC - WRITE CONSOLE CHARACTER
;
;       WRITE A CHARACTER TO THE CONSOLE UART PORT
;
;       ENTRY   (A) = ASCII CHARACTER TI OUTPUT
;       EXIT    NONE
;       USES    NONE

WCC     PUSH    PSW             ; SAVE CHARACTER
WCC1    IN      SC.ACE+UR.LSR   ; INPUT ACE STATUS
        ANI     UC.THE          ; SEE IF TRANSMITTER HOLDING IS EMPTY
        CPU     Z80
        JR      Z,WCC1
        CPU     8080

        POP     PSW             ; GET CHARACTER
        OUT     SC.ACE+UR.THR   ; OUTPUT TO CONSOLE
        RET

; MORE CODE BELOW THAT WAS NOT PUBLISHED. REPLACED WITH DISASSEMBLY OF
; ROM.

        ORG     1771Q

;       IO ROUTINES TO BE COPIED INTO AND USED IN RAM.
;
;       MUST CONTINUE TO 3777A FOR PROPER COPY.
;       THE TABLE MUST ALSO BE BACKWARDS TO THE FINAL RAM

        ERRNZ   4000Q-7-*

PRSROM
        DB      1               ; REFIND
        DB      0               ; CTLFLG
        DB      0               ; MFLAG
        DB      0               ; DSPMOD
        DB      0               ; DSPROT
        DB      10              ; REGI
        DB      MI.RET

        ERRNZ   *-4000Q

;       INIT0X - EXTENSION OF INIT0 TO SUPPORT H88

INIT0X  MVI     A,00000010B
        OUT     H88.CTL

;       SET UP ACE FOR CONSOLE COMMUNICATIONS
;
        MVI     A,UC.DLA        ; SET DIVISOR LATCH ACCESS BIT
        OUT     SC.ACE+UR.LCR
        LXI     H,BRTAB         ; (H,L) = BEGINNING OF BAUD RATE TABLE
        IN      H88.SW          ; INPUT SWITCHES FOR DESIRED BAUD RATE
        ANI     H88S.BR         ; MASK FOR BAUD RATE SWITCHES ONLY
        RRC                     ; SHIFT FOR A *2 FOR TABLE
        RRC
        RRC
        RRC
        RRC
        ADD     L               ; ADD DISPLACEMENT FROM BEGINNING OF TABLE
        MOV     L,A
        MOV     A,M             ; GET MSB OF DIVISOR
        OUT     SC.ACE+UR.DLM
        INX     H               ; GET LSB
        MOV     A,M
        OUT     SC.ACE+UR.DLL
        MVI     A,UC.8BW        ; SET 8 BITS, 1 STOP BIT, NO PARITY
        OUT     SC.ACE+UR.LCR
        MVI     A,0             ; SET NO INTERRUPTS
        OUT     SC.ACE+UR.IER

;       WAIT A WHILE TO ALLOW THE CONSOLE RESET TO FINISH SO IT CAN
;       ACCEPT THE FIRST PROMPT
;
        LXI     B,65000Q        ; APPROX. 100 MS
INIT0X1 DCR     C
        JNZ     INIT0X1

        DCR     B
        JNZ     INIT0X1

;       INPUT SWITCH TO SEE IF TO BEGIN OPERATION OF MEMORY TEST
;
        IN      H88.SW          ; GET SWITCHES
        ANI     H88S.M          ; MASK FOR MEMORY TEST ONLY
        JZ      DYMEM           ; IF TO PERFORM MEMORY TESTS

;       REPLACE WHAT WAS ORIGINALLY AT THE JUMP WHICH GOT US HERE
;
        LXI     D,PRSROM        ; (DE) = ROM COPY OF PRS CODE
        JMP     INIT0.0         ; RETURN TO ORIGINAL CODE

;       BRTAB - BAUD RATE DIVISOR TABLE
;
BRTAB

BR96    DB      0,12            ;   9600 BAUD
BR19.2  DB      0,6             ; 19,200 BAUD
BR38.4  DB      0,3             ; 38,400 BAUD
BR56.0  DB      0,2             ; 56,000 BAUD

;       SET     */256
        ERRNZ   ERTAB/256-.     ; TABLE MUST BE IN ONE PAGE

;       SAVALLX - SAVALL EXTENSION TO MAKE ROOM FOR A JUMP TO THE NMI HANDLER

SAVALLX                         ; REPLACE OLD CODE
        PUSH     H              ; SET ON STACK AS 'REGISTER'
        PUSH     D              ; SET RETURN ADDRESS
        LXI      D,CTLFLG
        LDAX     D
        JMP      SAVALLR        ; RETURN TO OLD CODE

;       NMI - NON MASKABLE INTERRUPT
;
;       NMI IS USED AS THE TRAP FOR ALL ILLEGAL PORT REQUESTS
;
;       PORT ADDRESSES TRAPPED ARE:
;
;                       IN 360Q FRONT PANEL KEYBOARD INPUT
;                      OUT 360Q FRONT PANEL CONTROL
;                      OUT 361Q FRONT PANEL DISPLAY CONTROL
;                   IN/OUT 372Q CONSOLE DATA FOR AN 8251A
;                      OUT 373Q CONSOLE CONTROL FOR AN 8251A
;
;
;               THESE PORT REQUESTS ARE RESPONDED TO AS FOLLOWS:
;
;                       IN 360Q RETURNS WITH (A) = 377Q TO SHOW THAT
;                               NO FRONT PANEL SWITCHES ARE PRESSED
;
;                      OUT 360Q MOVES BIT 6 (CB.CLI) TO BIT 1, AND
;                              BIT 4 (CB.SSI) INVERTED, TO BIT 0. AND
;                              OUTPUTS THESE BITS TO PORT 362Q TO
;                              CONTROL THE CLOCK AND SINGLE STEP INTERRUPTS
;
;                      OUTPUTS TO 361Q, 372Q, AND 373Q JUST RETURN
;
;                      INPUTS FROM 361Q, 372Q, AND 373Q RETURN WITH (A) = 0
;                              TO INDICATE AN EMPTY BUSS
;
;
;       ENTRY   NONE
;
;       EXIT    NONE
;
;       USES    (A) ONLY IF 'FAKING' AN INPUT

NMI     XTHL                    ; GET RETURN ADDRESS FROM STACK
        SHLD    NMIRET          ; SAVE FOR LATER USE
        XTHL                    ; PUT RETURN ADDRESS BACK ON STACK

        PUSH    H               ; SAVE REGISTERS
        PUSH    B
        PUSH    PSW
        MOV     B,A             ; SAVE (A) PRIOR TO I/O
        LHLD    NMIRET          ; GET RETURN ADDRESS
        DCX     H               ; BACK UP TO PORT # WHICH GOT US HERE
        MOV     A,M             ; GET PORT #

        CPI     360Q            ; PORT 360?
        JZ      NMI1            ; IF PORT WAS 360Q

;       PORT REFERENCED WAS 361Q, 372Q, OR 373Q
;
        CPI     361Q            ; MAKE SURE PORT IS LEGAL
        JZ      NMI0.5          ; IF LEGAL

        CPI     372Q
        JZ      NMI0.5

        CPI     373Q
        JNZ     NMI2.5          ; IF NONE OF THE ABOVE, EXIT

NMI0.5  DCX     H               ; POINT TO IN/OUT INSTRUCTION
        MOV     A,M             ; SEE IF INPUT OR OUTPUT
        CPI     MI.OUT
        JZ      NMI2.5          ; IF OUTPUT, JUST EXIT

        CPI     MI.IN
        JNZ     NMI2.5          ; IF NOT INPUT EITHER, ILLEGAL SO EXIT

        POP     PSW             ; RESTORE FLAGS
        MVI     A,0             ; ELSE, RETURN LIKE AN EMPTY BUSS
        JMP     NMI3            ; EXIT

NMI1    DCX     H               ; POINT TO IN/OUT INSTRUCTION
        MOV     A,M             ; GET I/O INSTRUCTION
        CPI     MI.IN           ; INPUT?
        JNZ     NMI1.5          ; IF NOT 'IN'

        POP     PSW             ; RESTORE FLAGS
        MVI     A,11111111B     ; SHOW 'NO KEYS PRESSED'
        JMP     NMI3            ; EXIT

NMI1.5  CPI     MI.OUT          ; MAKE SURE INSTRUCTION IF AN 'OUT'
        JNZ     NMI2.5          ; IF NOT

NMI2    MOV     A,B             ; GET OUTPUT DATA AGAIN
        ANI     CB.CLI          ; MOVE CLOCK INFO TO BIT 1
        RRC
        RRC
        RRC
        RRC
        MOV     C,A             ; SAVE IN C
        MOV     A,B             ; GET OUTPUT DATA AGAIN
        ANI     CB.SSI          ; GET SINGLE STEP INFO
        RRC                     ; MOVE TO BIT 1
        RRC
        RRC
        RRC
        ORA     C               ; ADD TO CLOCK DATA
        XRI     00000001B       ; INVERT THIS BIT PRIOR TO OUTPUT
        OUT     H88.CTL         ; SET IN HARDWARE

NMI2.5  POP     PSW             ; RESTORE (A,F)

NMI3    POP     B
        POP     H
;       RET                     ; Z80 RETURN FROM NMI
        DB      355Q,105Q

;               BOOT HDOS ENTRY POINT FOR H88
;
;       ENTRY   NONE
;       EXIT    TO HDOS BOOT ROM
;       USES    ALL

BOOT    LXI     H,MSG.BT        ; COMPLETE BOOT MESSAGE
        CALL    TYPMSG
        CALL    WCR             ; WAIT FOR A CARRIAGE RETURN
        MVI     A,10
        CALL    LRA.            ; GET LOCATION OF USER PC
        LXI     D,ROMDD         ; SET ITS VALUE TO THE BOOT ROM
        MOV     M,E
        INX     H
        MOV     M,D

;       TELL USER TO "TYPE SPACES TO DETERMINE BAUD RATE"
;
        LXI     H,MSG.SP
        CALL    TYPMSG

        JMP     GO.             ; DO IT

;       SWMEM - SET UP FOR WMEM TO DUMP A CASSETTE FROM THE MONITOR LEVEL
;
;       SWMEM ALLOWS THE USER TO EITHER ENTER A NEW STARTING AND
;       ENDING ADDRESS FOR THIS DUMP OR USE THE ADDRESS OF THE
;       PREVIOUS LOAD OR DUMP.  THE PREVIOUS ADDRESSES ARE USED IF
;       THE FIRST CHARACTER IS AN ASCII CARRIAGE RETURN.  IF THE
;       FIRST CHARACTER IS AN OCTAL CHARACTER, BOTH BEGINNING AND
;       ENDING ADDRESSES MUST BE ENTERED SEPARATED BY A DASH AND
;       FOLLOWED BY A CARRIAGE RETURN.
;
;       ENTRY   USER PC VALUE ON STACK = PROGRAM START ADDRESS FOR THIS TAPE
;       EXIT    TO WMEM
;       USES    ALL

SWMEM   LXI     H,MSG.DMP       ; COMPLETE DUMP MESSAGE
        CALL    TYPMSG
        CALL    IROC            ; INPUT FIRST CHARACTER
        JNC     SWMEM4          ; IF FIRST CHARACTER IS OCTAL

        LXI     H,START+1       ; ELSE, INPUT STARTING ADDRESS
        MVI     D,'-'           ; FIRST BYTE MUST END WITH A DASH
        CALL    IOA
SWMEM2  LXI     H,ABUSS+1       ; ENTER ENDING ADDRESS
        STC                     ; SHOW NO CHARACTER IN (A)
        CMC
        MVI     D,A.CR          ; LAST CHARACTER MUST BE A RETURN
        CALL    IOA
SWMEM4  MVI     A,10            ; GET USER PC VALUE FOR DISPLAY
        CALL    LRA.
        MOV     E,M
        INX     H
        MOV     D,M
        XCHG                    ; (H,L) = USER PC VALUE
        CALL    TOA             ; TYPE OCTAL ADDRESS
        JMP     WMEM            ; DO THE DUMP

;       SUBM - SUBSTITUTE MEMORY
;
;       SUBM INPUTS A MEMORY ADDRESS FROM THE CONSOLE AND THEN DISPLAYS
;       THAT ADDRESS AND ITS CONTENTS.  IF A CARRIAGE RETURN IS THEN TYPED,
;       CONTROL RETURNS TO THE MONITOR.  IF A SPACE IS TYPED, THE NEXT
;       MEMORY LOCATION AND CONTENTS ARE DISPLAYED.  IF A MINUS SIGN IS
;       TYPED, THE PREVIOUS MEMORY LOCATION AND CONTENTS ARE DISPLAYED.
;       IF AN OCTAL CHARACTER IS TYPED, A BYTE IS ENTERED AN PLACED AT THE
;       CURRENT MEMORY LOCATION.
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,E,H,L,F

SUBM    LXI     H,MSG.SUB       ; COMPLETE SUBSTITUTE MESSAGE
        CALL    TYPMSG
        CALL    IROC            ; INPUT FIRST CHARACTER
        RNC                     ; IF A RETURN EXIT

        LXI     H,IOWRK+1       ; ELSE, INPUT A STARTING ADDRESS
        MVI     D,A.CR          ; ENDING WITH A RETURN
        CALL    IOA
        XCHG                    ; (H,L) = INPUT ADDRESS

SUBM1   CALL    TOA             ; TYPE CRLF, ADDRESS, AND A SPACE
        MOV     A,M             ; GET MEMORY CONTENTS FOR DISPLAY
        CALL    TOB
        MVI     A,' '           ; SPACE
        CALL    WCC

SUBM2   CALL    IOC             ; INPUT FIRST CHARACTER
        JNC     SUBM7           ; IF FIRST CHARACTER IS OCTAL

        CPI     ' '             ; SPACE?
        JNZ     SUBM4           ; IF NOT A SPACE

SUBM3   INX     H               ; POINT TO NEXT ADDRESS
        JMP     SUBM1           ; DISPLAY NEXT

SUBM4   CPI     '-'             ; MINUS?
        JNZ     SUBM4           ; IF NOT

SUBM5   CALL    WCC             ; ECHO HYPHEN
        DCX     H               ; POINT TO PREVIOUS ADDRESS
        JMP     SUBM1           ; DISPLAY PREVIOUS

SUBM6   CPI     A.CR            ; RETURN?
        RZ                      ; IF RETURN, EXIT

        MVI     A,A.BEL         ; ELSE, DING BELL
        CALL    WCC
        JMP     SUBM2           ; TRY AGAIN

SUBM7   MVI     M,0             ; ZERO BYTE TO BE BUILT

SUBM8   CALL    WCC             ; ECHO OCTAL CHARACTER
        ANI     00000111B       ; GET BINARY VALUE
        MOV     E,A             ; SAVE PARTIAL
        MOV     A,M             ; GET CURRENT
        RLC                     ; MAKE ROOM FOR NEW CHARACTER
        RLC
        RLC
        ANI     11111000B       ; TOSS PREVIOUS LSB
        ORA     E               ; ADD NEW
        MOV     M,A             ; SAVE NEW TOTAL
SUBM9   CALL    IOC             ; INPUT NEXT CHARACTER
        JNC     SUBM8           ; IF OCTAL

        CPI     ' '             ; SPACE?
        JZ      SUBM3           ; IF SPACE, DISPLAY NEXT BYTE

        CPI     '-'             ; MINUS?
        JZ      SUBM5           ; IF MINUS, DISPLAY PREVIOUS

        CPI     A.CR            ; RETURN?
        RZ                      ; IF RETURN, EXIT

        MVI     A,A.BEL         ; ELSE, DING BELL
        CALL    WCC
        JMP     SUBM7           ; TRY AGAIN

;       IROC - INPUT A RETURN OR AN OCTAL CHARACTER
;
;       IROC INPUTS A CHARACTER FROM THE CONSOLE AND WAITS UNTIL IT
;       RECEIVES EITHER A VALID OCTAL CHARACTER OR A CARRIAGE RETURN
;
;       ENTRY   NONE
;       EXIT    (A) = INPUT CHARACTER
;               'C' = SET IF CHARACTER IS OCTAL
;       USES    A,F

IROC    CALL    RCC             ; INPUT CHARACTER
        CPI     A.CR            ; RETURN?
        RZ                      ; IF A CR

        CPI     'O'             ; < 0?
        JC      IROC1           ; IF < OCTAL

        CPI     '0'             ; > 0?
        RC                      ; IF OCTAL

IROC1   MVI     A,A.BEL         ; ELSE, RING BELL
        CALL    WCC
        JMP     IROC            ; TRY AGAIN

;       IOA1 - INPUT OCTAL ADDRESS
;
;       IOA1 IS A CONTINUATION OF *IOA* AND INPUTS A SPLIT OCTAL ADDRESS
;       WITHOUT REQUIRING LEADING ZEROS
;
;       ENTRY   (H,L) = ADDRESS + 1 WHERE INPUT ADDRESS IS TO BE PLACED
;               (A) = FIRST OCTAL CHARACTER IF 'C' IS SET
;       EXIT    (D,E) = INPUT ADDRESS
;               (A) = LAST INPUT CHARACTER
;       USES    A,D,E,H,L,F

IOA1    PUSH    B               ; SAVE (B,C)
        MOV     B,D             ; (B) = TERMINATION CHARACTER
        PUSH    H               ; SAVE ADDRESS WHERE INPUT IS TO BE PLACED
        LXI     H,0             ; SET NEW VALUE TO ZERO
IOA2    CNC     RCC             ; IF CARRY SET, FIRST CHARACTER IS IN ACC
        CPI     'O'             ; MAKE SURE CHARACTER IS OCTAL
        JC      IOA3            ; IF < OCTAL

        CPI     '8'
        JNC     IOA3            ; IF > OCTAL

        CALL    WCC             ; ECHO OCTAL CHARACTER
        ANI     00000111B       ; GET BINARY VALUE
        PUSH    PSW             ; SAVE NEW CHARACTER VALUE
        DAD     H               ; SHIFT THREE TO MAKE ROOM FOR NEW CHARACTER
        DAD     H
        DAD     H
        PUSH    PSW             ; SAVE CARRY FORM DAD
        POP     D               ; SAVE FLAG RESULT IN E
        POP     PSW             ; RETURN NEW CHARACTER VALUE TO (A)
        ADD     L
        MOV     L,A
        JMP     IOA2            ; SEE IF MORE CHARACTERS

IOA3    CMP     B               ; TERMINATING CHARACTER?
        JZ      IOA4            ; IF EQUAL

        MVI     A,A.BEL         ; ELSE, DING BELL
        CALL    WCC
        STC                     ; TRY AGAIN
        CMC
        JMP     IOA2

;       END OF INPUT, PUT VALUE IN MEMORY AND EXIT

IOA4    CALL    WCC             ; ECHO CHARACTER
        MOV     D,A             ; LAST CHARACTER TO D
        PUSH    D
        POP     PSW             ; (PSW) = RESULT OF DAD
        MOV     A,H             ; MAKE (A) INTO SPLIT OCTAL
        RAR
        MOV     H,A
        MOV     A,D             ; RESTORE LAST INPUT CHARACTER
        XCHG                    ; (D,E) = INPUT ADDRESS
        POP     H               ; (H,L) = LOCATION TO PLACE THIS ADDRESS
        MOV     M,D
        DCX     H
        MOV     M,E
        POP     B               ; RESTORE (B,C)
        RET

;       IOC - INPUT OCTAL CHARACTER
;
;
;       ENTRY   NONE
;       EXIT    (A) = INPUT CHARACTER
;               'X' SET IF CHARACTER IS NOT OCTAL
;       USES    A,F

IOC     CALL    RCC             ; INPUT CHARACTER
        CPI     '0'
        RC                      ; IF CHARACTER < OCTAL

        CPI     '8'             ; CHARACTER > OCTAL?
        CMC                     ; 'C' IF GREATER THAN
        RET

;       TOA - TYPE OCTAL ADDRESS
;
;       TOA OUTPUTS TO THE CONSOLE A CRLF, THE SPECIFIED ADDRESS AND A SPACE
;
;       ENTRY   (H,L) = ADDRESS TO BE DISPLAYED
;       EXIT    NONE
;       USES    A,B,C,F

TOA     MVI     A,A.CR          ; CRLF
        CALL    WCC
        MVI     A,A.LF
        CALL    WCC

TOA.    MOV     A,H             ; ADDRESS
        CALL    TOB
        MOV     A,L
        CALL    TOB

        MVI     A,' '           ; SPACE
        RET

;       TOB - TYPE OCTAL BYTE
;
;       TOB OUTPUTS TO THE CONSOLE IN OCTAL, THE BYTE IN A
;
;       ENTRY   (A) = BYTE TO BE OUTPUT
;       EXIT    NONE
;       USES    A,F

TOB     PUSH    B
        MVI     B,2             ; NUMBER OF CHARACTERS - 1
        MOV     C,A             ; SAVE ORIGINAL BYTE
        ORA     A               ; ASSURE 'C' = ZERO
        RAR
        RAR                     ; SHIFT TOP BYTE TO LSB
        RAR
TOB1    RAR                     ; SHIFT MIDDLE BYTE TO LSB
        RAR
        RAR
        ANI     00000111B       ; MASK FOR HALF ASCII
        ORI     00110000B       ; MASK FOR WHOLE ASCII
        CALL    WCC             ; OUTPUT TO CONSOLE
        MOV     A,C             ; GET ORIGINAL BYTE
        DCR     B
        JNZ     TOB1            ; IF SECOND BYTE STILL NEEDS TO BE OUTPUT

        ANI     00000111B       ; ELSE, OUTPUT LAST CHARACTER
        ORI     00110000B
        CALL    WCC
        POP     B
        RET

;       WCR - WAIT FOR A CARRIAGE RETURN
;
;       WCR INPUTS CHARACTERS FROM THE CONSOLE UNTIL A CARRIAGE RETURN
;       IS RECEIVED AND THEN ECHOS A CRLF
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,F

WCR     CALL    RCC             ; INPUT CHARACTER
        CPI     A.CR
        JNZ     WCR             ; IF NOT A CR

        CALL    WCC             ; ELSE ECHO CR
        MVI     A,A.LF          ; LINE FEED
        CALL    WCC
        RET

;       TPDSP - TAPE DISPLAY
;
;       SHOW H88 USER THAT THERE IS SOME ACTIVITY DURING A LOAD OR A DUMP
;
TPDSP   SHLD    ABUSS           ; UPDATE ABUSS

        MVI     A,A.CR          ; RETURN
        CALL    WCC

        MOV     A,H             ; ADDRESS
        CALL    TOB
        MOV     A,L
        CALL    TOB
        RET

;       HRNX - HORN EXTENSION ROUTINE
;
;       THIS IS AN EXTENSION TO *HORN* TO MAKE ROOM FOR A JUMP
;

HRNX    MVI     L,CTLFLG/256
        MOV     M,E             ; TURN OFF HORN
        POP     D
        POP     H
        RET

;       NOISE - DING BELL ON CONSOLE
;
;       THIS IS A MODIFICATION TO ALLOW THE H88/H89 TO USE THE CONSOLE BELL
;

NOISE   MVI     A,A.BEL
        CALL    WCC
        JMP     HORN            ; CONTINUE WITH NORMAL HORN DELAY

;       TPERMSG - TAPE ERROR MESSAGE
;
;       DISPLAY THE TAPE ERROR NUMBER ON THE CONSOLE

TPERMSG STA     ABUSS          ; SAVE ERROR NUMBER
        MVI     A,' '          ; OUTPUT A SPACE
        CALL    WCC
        MOV     A,B            ; OUTPUT NUMBER
        CALL    TOB
        RET

;       TYPMSG - TYPE MESSAGE TO CONSOLE
;
;       TYPMSG OUTPUTS AN ASCII MESSAGE FROM MEMORY TO THE CONSOLE
;       UNTIL A NULL IS SENSED
;
;       ENTRY   (H,L) = ADDRESS OF MESSAGE
;       EXIT    NONE
;       USES    A,H,L,F

TYPMSG  MOV     A,M             ; GET CHARACTER
        ORA     A               ; SEE IF NULL
        RZ                      ; IF NULL, EXIT

        CALL    WCC             ; ELSE OUTPUT CHARACTER TO CONSOLE
        INX     H               ; POINT TO NEXT CHARACTER
        JMP     TYPMSG          ; OUTPUT IT

;       MSG.PR - MESSAGE FOR MONITOR PROMPT
;
;       CRLF, "  H: "

MSG.PR  DB      A.CR,A.LF,"  H: ",0

;       MSG.SP - MESSAGE TO TELL USER TO TYPE SPACES
;
;       "Type spaces to determine baud rate"

MSG.SP  DB      "Type spaces to determine baud rate" ,0

;       MSG.GO - (G)O
;
;       "GO"

MSG.GO  DB      "o ",0

;;      MSG.LD - (L)OAD
;
;       "LOAD"

MSG.LD  DB      "oad",0

;       MSG.DMP - (D)UMP
;
;       "DUMP"

MSG.DMP DB      "UMP",0

;       MSG.SUB - (S)UBSTITUTE
;
;       "SUBSTITUTE"

MSG.SUB DB      "ubstitute ",0

;       MSG.PC - (P)ROGRAM COUNTER
;
;       "PROGRAM COUNTER"

MSG.PC  DB       "rogram Counter ",0

;       MSG.BT - (B)OOT
;
;       "BOOT"

MSG.BT  DB      "oot",0

; MORE CODE BELOW THAT WAS NOT PUBLISHED. REPLACED WITH DISASSEMBLY OF
; ROM.

        ORG     3772Q

;       ENTRY POINT FOR FLOPPY DISK ROTATIONAL SPEED TEST
;
        ERRNZ   10000A-6-*      ; MUST BE 6 BYTES BEFORE END

SPEED   EQU     6240Q

ESPEED  JMP     SPEED

;       ENTRY POINT FOR DYNAMIC MEMORY TEST
;
        ERRNZ   10000A-3-*      ; MUST BE 3 BYTES BEFORE END

DYMEM   EQU     7116Q

EDTMEM  JMP     DYMEM

        ERRNZ   *-10000A        ; MUST NOT EXCEED 2K BYTES

;       THE FOLLOWING ARE CONTROL CELLS AND FLAGS USED BY THE KEYSET
;       MONITOR.

        ORG     20000Q          ; 8192

START   DS      2               ; DUMP STARTING ADDRESS
IOWRK   DS      2               ; IN OUR OUT INSTRUCTION
PRSRAM                          ; FOLLOWING CELLS INITIALIZED FROM ROM
        DS      1               ; RET
REGI    DS      1               ; INDEX OF REGISTER UNDER DISPLAY
DSPROT  DS      1               ; PERIOD FLAG BYTE
DSPMOD  DS      1               ; DISPLAY MODE

MFLAG   DS      1               ; USER FLAG OPTIONS
                                ; SEE *UO.XXX* BITS DESCRIBED AT FRONT

CTLFLG  DS      1               ; FRONT PANEL CONTROL BITS
REFIND  DS      1               ; REFRESH INDEX (0 TO 7)
PRSL    EQU     7               ; END OF AREA INITIALIZED FROM ROM

FPLEDS                          ; FRONT PANEL LEDS PATTERNS
ALEDS   DS      1               ; ADDR 0
        DS      1               ; ADDR 1
        DS      1               ; ADDR 2

        DS      1               ; ADDR 3
        DS      1               ; ADDR 4
        DS      1               ; ADDR 5

DLEDS   DS      1               ; DATA 0
        DS      1               ; DATA 1
        DS      1               ; DATA 2

ABUSS   DS      2               ; ADDRESS BUSS
RCCA    DS      1               ; RCC SAVE AREA
CRCSUM  DS      2               ; CRC-16 CHECKSUM
TPERRX  DS      2               ; TAPE ERROR EXIT ADDRESS
TICCNT  DS      2               ; CLOCK TIC COUNTER

REGPTR  DS      2               ; REGISTER CONTENTS POINTER

UIVEC                           ; USER INTERRUPT VECTORS
        DS      3               ; JUMP TO CLOCK PROCESSOR
        DS      3               ; JUMP TO SINGLE STEP PROCESSOR
        DS      3               ; JUMP TO I/O 3
        DS      3               ; JUMP TO I/O 3
        DS      3               ; JUMP TO I/O 5
        DS      3               ; JUMP TO I/O 6
        DS      3               ; JUMP TO I/O 7

;       H88/H89 RAM USAGE BEYOND THAT OF H8MTRF
;
NMIRET  DS      2

        END
