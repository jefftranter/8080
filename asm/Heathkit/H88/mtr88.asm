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

ROMADDR EQU     14000Q          ; HDOS BOOT ADDRESS

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
H88.CRL EQU     362Q            ; H88/H89 PORT FOR CLOCK AND SINGLE STEP
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
        ANI     CB+MTL+CB.SSI   ; SAVE REGISTER ADDR IF USER OR SINGLE STEP
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

CLK     JMP     CUI1            ; ALLOW USER PROCESSING OF CLOCK

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
        MVI     A,M             ; (A) = MFLAG
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

SRMEM   LXI     H,MSO.LD        ; COMPLETE MESSAGE
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
        LDA     CTLGL
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

        DB      MI,ANI          ; FALL THROUGH WITH CARRY CLEAR
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
        JMP     TER2

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
        IN      IP.TPX          ; READ TAPE STATUS
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
