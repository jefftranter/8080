        INCLUDE macros.mac

;       PAM/8 - H8 FRONT PANEL MONITOR
;
;       JGL, 05/01/76.
;
;       FOR *WINTEK* INC.
;
;       COPYRIGHT  05/1976, WINTEK CORPORATION
;       902 N. 9TH ST.
;       LAFAYETTE, IND.

;       PAM/8 - H8 FRONT PANEL MONITOR
;
;       THIS PROGRAM RESIDES (IN ROM) IN THE LOW 1024 BYTES OF THE HEATH
;       H8 COMPUTER. IT ACTUALLY CONSISTS OF TWO VIRTUALLY INDEPENDENT
;       ROUTINES: A TASK-TIME PROGRAM WHICH PROVIDES SOPHISTICATED
;       FRONT PANEL MONITOR SERVICE, AND AN INTERRUPT-TIME PROGRAM WHICH
;       PROVIDES BOTH A REAL-TIME CLOCK AND EMULATES AN EFFECTIVE
;       HARDWARE FRONT PANEL.

;       INTERRUPTS.
;
;       PAM/8 IS THE PRIMARY PROCESSOR FOR ALL INTERRUPTS.
;       THEY ARE PROCESSED AS FOLLOWS:
;
;       RST     USE
;
;       0       MASTER CLEAR. (NEVER USES FOR I/O OR RESET)
;
;       1       CLOCK INTERRUPT. NORMALLY TAKEN BY PAM/8.
;               SETTING BIT *U0.CLK* IN BYTE *.MFLAG* ALLOWS
;               USER PROCESSING (VIA A JUMP THROUGH *UIVEC*).
;               UPON ENTRY OF THE USER ROUTINE, THE STACK
;               CONTAINS:
;               (STACK+0)  = RETURN ADDRESS (TO PAM/8)
;               (STACK+2)  = (STACKPTR+4)
;               (STACK+4)  = (AF)
;               (STACK+6)  = (BC)
;               (STACK+8)  = (DE)
;               (STACK+10) = (HL)
;               (STACK+12) = (PC)
;               THE USER'S ROUTINE SHOULD RETURN TO PAM/8 VIA
;               A *RET* WITHOUT ENABLING INTERRUPTS.
;
;       2       SINGLE STEP. SINGLE STEP INTERRUPTS GENERATED
;               BY PAM/8 ARE PROCESSED BY PAM/8.
;               ANY SINGLE STEP INTERRUPT RECEIVED WHEN IN
;               USER MODE CAASES A JUMP THROUGH *UIVEC*+3.
;               STACK UPON USER ROUTINE ENTRY;
;               (STACK+0)  = (STACKPTR+12)
;               (STACK+2)  = (AF)
;               (STACK+4)  = (BC)
;               (STACK+6)  = (DE)
;               (STACK+8)  = (HL)
;               (STACK+10) = (PC)
;               THE USER'S ROUTINE SHOULD HANDLE ITS OWN RETURN
;               FROM THE INTERRUPT.
;
;
;       THE FOLLOWING INTERRUPTS ARE VECTORED DIRECTLY THROUGH *UIVEC*.
;       THE USER ROUTINE MUST HAVE SETUP A JUMP IN *UIVEC* BEFORE ANY
;       OF THESE INTERRUPTS MAY OCCUR.
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


;               ASSEMBLY CONSTANTS

;               I/O PORTS

IP.PAD  EQU     360Q            ; PAD INPUT PORT
OP.CTL  EQU     360Q            ; CONTROL OUTPUT PORT
OP.DIG  EQU     360Q            ; DIGIT SELECT OUTPUT PORT
OP.SEQ  EQU     361Q            ; SEGMENT SELECT OUTPUT PORT
IP.TPC  EQU     371Q            ; TAPE CONTROL IN
OP.TPC  EQU     371Q            ; TAPE CONTROL OUT
IP.TPD  EQU     370Q            ; TAPE DATA IN
OP.TPD  EQU     370Q            ; TAPE DATA OUT

;               ASCII CHARACTERS.

A.SYN   EQU     026Q            ; SYNC CHARACTER
A.STX   EQU     002Q            ; STX CHARACTER

;               FRONT PANEL HARDWARE CONTROL BITS.

CB.SSI  EQU     00010000B       ; SINGLE STEP INTERRUPT
CB.MTL  EQU     00100000B       ; MONITOR LIGHT
CB.CLI  EQU     01000000B       ; CLOCK INTERRUPT ENABLE
CB.SPK  EQU     10000000B       ; SPEAKER ENABLE

;               DISPLAY MODE FLAGS (IN *DSPMOD*)

DM.MR   EQU     0               ; MEMORY READ
DM.MW   EQU     1               ; MEMORY WRITE
DM.RR   EQU     2               ; REGISTER READ
DM.RW   EQU     3               ; REGISTER WRITE
        INCLUDE tape.asm        ; TAPE DEFINITIONS

;               MACHINE INSTRUCTIONS.

MI.HLT  EQU     01110110B       ; HALT
MI.RET  EQU     11001001B       ; RETURN
MI.IN   EQU     11011011B       ; INPUT
MI.OUT  EQU     11010011B       ; OUTPUT
MI.LDA  EQU     00111010B       ; LDA
MI.ANI  EQU     11100110B       ; ANI
MI.LXID EQU     00010001B       ; LXI D

;               USER OPTION BITS.
;
;               THESE BITS ARE SET IN CELL .MFLAG.

UO.HLT  EQU     10000000B       ; DISABLE HALT PROCESSING
UO.NFR  EQU     00000010B       ; NO REFRESH OF FRONT PANEL
UO.DDU  EQU     00000010B       ; DISABLE DISPLAY UPDATE
UI.CLK  EQU     00000001B       ; ALLOW CLOCK INTERRUPT PROCESSING

        INCLUDE u8251.asm       ; DEFINE THE 8251 USART BITS

;       INTERRUPT VECTORS.
;

;       LEVEL 0 - RESET
;
;       THIS 'INTERRUPT' MAY NOT BE PROCESSED BY A USER PROGRAM.

        ORG     00Q

INIT0   LXI     D,PRSROM        ; (DE) = ROM COPY OF PRS CODE
        LXI     H,PRSRAM+PRSL-1 ; (HL) = RAM DESTINATION FOR CODE
        JMP     INIT            ; INITIALIZE
        ERRPL   INIT-1000Q      ; BYTE IN WORD MUST BE 0

;       LEVEL 1 - CLOCK

INT1    EQU     10Q             ; INTERRUPT ENTRY POINT

        ERRNZ   *-11Q           ; INTO TAKE UP ONE BYTE
        CALL    SAVALL          ; SAVE USER REGISTERS
        MVI     D,0
        JMP     CLOCK           ; PROCESS CLOCK INTERRUPT
        ERRPL   CLOCK-1000Q     ; EXTRA BYTE MUST BE 0

;       LEVEL 2 - SINGLE STEP
;
;       IF THIS INTERRUPT IS RECEIVED WHEN NOT IN MONITOR MODE,
;       THEN IT IS ASSUMED TO BE GENERATED BY A USER PROGRAM
;       (SINGLE STEPPING OR BREAKPOINTING). IN SUCH CASE, THE
;       USER PROGRAM IS ENTERED THROUGH (UIVEC+3

INT2    EQU     20Q             ; LEVEL 2 ENTRY

        ERRNZ   *-21Q           ; INT1 TAKES EXTRA BYTE
        CALL    SAVALL          ; SAVE REGISTERS
        LDAX    D               ; (A) = (CTLFLG)
;       SET     CTLFLG
        JMP     STPRTN          ; STEP RETURN

;       I/O INTERRUPT VECTORS
;
;       INTERRUPTS 3 THROUGH 7 ARE AVAILABLE FOR GENERAL I/O USE.
;
;       THESE INTERRUPTS ARE NOT SUPPORTED BY PAM/8, AND SHOULD
;       NEVER OCCUR UNLESS THE USER HAS SUPPLIED HANDLER ROUTINES
;       (THROUGH UIVEC)

        ORG     30Q
INT3    JMP     UIVEC+6         ; JUMP TO USER ROUTINE

        DB      '44413'         ; HEATH PART NUMBER 444-13

        ORG     40Q
INT4    JMP     UIVEC+9         ; JUMP TO USER ROUTINE

        DB      100Q,112Q,107Q,114Q,100Q  ; SUPPORT CODE

        ORG     50Q
INT5    JMP     UIVEC+12        ; JUMP TO USER ROUTINE


;       DLY - DELAY TIME INTERVAL.
;
;       ENTRY   (A) = MILLISECONDS DELAY COUNT/2
;       EXIT    NONE
;       USES    A,F

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
;       SETUP PAM/8 CONTROL CELLS IN RAM.
;       DECODE HOW MUCH MEMORY EXISTS, SETUP STACKPOINTER, AMD
;       ENTER THE MONITOR LOOP.
;
;       ENTRY   FROM MASTER CLEAR
;       EXIT    INTO PAM/8 MAIN LOOP

INIT    LDAX    D               ; COPY *PRSROM* INTO RAM
        MOV     M,A             ; MOVE BYTE
        DCX     H               ; DECREMENT DESTINATION
        INR     E               ; INCREMENT SOURCE
        JNZ     INIT            ; IF NOT DONE

SINCR   EQU     4000Q           ; SEARCH INCREMENT

        MVI     D,SINCR/256     ; (DE) = SEARCH INCREMENT
        LXI     H,START-SINCR   ; (HL) = FIRST RAM - SEARCH INCREMENT

;       DETERMINE MEMORY LIMIT.

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
        OUT     OP.TPC          ; SET 8 BIT, NO PARITY, 1 STOP, X16

;       SAVALL - SAVE ALL REGISTERS ON STACK.
;
;       SAVALL IS CALLED WHEN AN INTERRUPT IS ACCEPTED, IN ORDER TO
;       SAVE THE CONTENTS OF THE REGISTERS ON THE STACK.
;
;       ENTRY   CALLED DIRECTLY FROM THE INTERRUPT ROUTINE.
;       EXIT    ALL REGISTERS PUSHED ON STACK.
;               IF NOT YET IN MONITOR MODE, REGPTR = ADDRESS OF REGISTERS
;               ON STACK.
;               (DE) = ADDRESS OF CTLFLG


SAVALL  XTHL                    ; SET H,L ON STACK TOP
        PUSH    D
        PUSH    B
        PUSH    PSW
        XCHG                    ; (D,E) = RETURN ADDRESS
        LXI     H,10
        DAD     SP              ; (H,L) = ADDRESS OF USERS SP
        PUSH    H               ; SET ON STACK AS 'REGISTER'
        PUSH    D               ; SET RETURN ADDRESS
        LXI     D,CTLFLG
        LDAX    D               ; (A) = CTLFLG
        CMA
        ANI     CB.MTL+CB.SSI   ; SAVE REGISTER ADDR IF USER OR SINGLE-STEP
        RZ                      ; RETURN IF WAS INTERRUPT OR MONITOR LOOP
        LXI     H,2
        DAD     SP              ; (H,L) = ADDRESS OF 'STACKPTR' ON STACK
        SHLD    REGPTR
        RET


;       CUI - CHECK FOR USER INTERRUPT PROCESSING.
;
;       CUI IS CALLED TO SEE IF THE USER HAS SPECIFIED PROCESSING
;       FOR THE CLOCK INTERRUPT.

;       SET     .MFLAG          ; REFERENCE TO MFLAG
CUI1    LDAX    B               ; (A) = .MFLAG
        ERRNZ   U0.CLK-1        ; CODE ASSUMED = 01
        RRC
        CC      VIVEC           ; IF SPECIFIED, TRANSFER TO USER

;       RETURN TO PROGRAM FROM INTERRUPT.

INTXIT  POP     PSW             ; REMOVE FAKE 'STACK REGISTER'
        POP     PSW
        POP     B
        POP     D
        POP     H
        EI
        RET


;       CLOCK - PROCESS CLOCK INTERRUPT
;
;       CLOCK IS ENTERED WHENEVER A MILLISECOND CLOCK INTERRUPT IS
;       PROCESSED.
;
;       TICCNT IS INCREMENTED EVERY INTERRUPT.

CLOCK   LHLD    TICCNT
        INX     H
        SHLD    TICCNT          ; INCREMENT TICCOUNT

;       REFRESH FRONT PANEL.
;
;       THIS CODE DISPLAYS THE APPROPRIATE PATTERN ON THE
;       FRONT PANEL LEDS. THE LEDS ARE PAINTED IN REVERSE ORDER,
;       ONE PER INTERRUPT. FIRST, NUMBER 9 IS LIT, THEN NUMBER 8,
;       ETC.

        LXI     H,.MFLAG
        MOV     A,M
        MOV     B,A             ; (B) = CURRENT FLAG
        ANI     UO.NFR          ; SEE IF FRONT PANEL REFRESH WANTED
        INX     H
        ERRNZ   CTLFLG-.MFLAG-1
        MOV     A,M             ; (A) = CTLFLG
        MOV     C,D             ; (C) = 0 IN CASE NO PANEL DISPLAY
        JNZ     CLK3            ; IF NOT
        ERRNZ   REFIND-CTLFLG-1
        DCR     M               ; DECREMENT DIGIT INDEX
        JNZ     CLK2            ; IF NOT WRAP-AROUND
        MVI     M,9             ; WRAP DISPLAY AROUND
CLK2    MOV     E,M
        DAD     D               ; (H,L) = ADDRESS OF PATTERN
        MOV     C,E
CLK3                            ; (A) = CTLNLG
        ORA     C               ; (A) = INDEX + FIXED BITS
        OUT     OP.DIG          ; SELECT DIGIT
        MOV     A,M
        OUT     OP.SEG          ; SELECT SEGMENT

;       SEE IF TIME TO DECODE DISPLAY VALUES

        MVI     L,TICCNT
        MOV     A,M
        ANI     37Q             ; EVERY 32 INTERRUPTS
        CZ      UFD             ; UPDATE FRONT PANEL DISPLAYS

;       EXIT CLOCK INTERRUPT

        LXI     B,CTLFLG
        LDAX    B               ; (A) = CTLFLG
        ANI     CB.MTL
        JNZ     INTXIT          ; IF IN MONITOR CODE
        DCX     B
        ERRNZ   CTLFLG-.MFLAG01
        LDAX    B               ; (A) = .MFLAG
        ERRNZ   UO.HLT-2000     ; ASSUME HIGH-ORDER
        RAL
        JC      CLK4            ; SKIP IT

;       NOT IN MONITOR MODE, CHECK FOR HALT

        MVI     A,10            ; (A) = INDEX OF *P* REG
        CALL    LRA.            ; LOCATE REGISTER ADDRESS
        MOV     E,M
        INX     H
        MOV     D,M             ; (D,E) = PC CONTENTS
        DCX     D
        LDAX    D
        CPI     MI.HLT          ; CHECK FOR HALT
        JZ      ERROR           ; IF HALT, BE IN MONITOR MODE

;       CHECK FOR 'RETURN TO MONITOR' KEY ENTRY.
CLK4
        IN      IP.PAD
        CPI     56Q             ; SEE IF '0' AND '#'
        JNZ     CUI1            ; IF NOT, ALLOW USER PROCESSING OF CLOCK

;       ERROR - COMMAND ERROR.
;
;       ERROR IS CALLED AS A 'BAIL-OUT' ROUTINE.
;
;       IT RESETS THE OPERATIONAL MODE, AND RESTORES THE STACKPOINTER.
;
;       ENTRY   NONE
;       EXIT    TO MTR LOOP
;               CTLFLG SET
;               .MFLAG CLEARED
;       USES    ALL

ERROR
        LXI     H,.MFLAG
        MOV     A,H             ; (A) = .MFLAG
        ANI     377Q-UO.DDU-UO.NFR ; RE-ENABLE DISPLAYS
        MOV     M,A             ; REPLACE
        INX     H
        MVI     M,CB.SSI+CB.MTL+CB.CLI+CB.SPK ; RESTORE *CTLFLG*
        ERRNZ   CTLFLG-.MFLAG-1
        EI
        LHLD    REGPTR
        SPHL                    ; RESTORE STACK POINTER TO EMPTY STATE
        CALL    ALARM           ; ALARM FOR 200 MS

;       MTR - MONITOR LOOP.
;
;       THIS IS THE MAIN EXECUTIVE LOOP FOR THE FRONT PANEL EMULATOR.

MTR     EI

MTR1    LXI     H,HTR1
        PUSH    H               ; SET 'MTR1' AS RETURN ADDRESS
        LXI     B,DSPMOD        ; (BC) = #DSPMOD
        LDAX    B
        ANI     1               ; (A) = 1 IF ALTER
        CMA
        STA     DSPROT          ; ROTATE LED PERIODS IF ALTER

;       READ KEY

        CALL    RCK             ; READ CONSOLE KEYPAD
        LHLD    ABUSS
        CPI     10
        JNC     MTR4            ; IF IN 'ALWAYS VALID' GROUP
        MOV     E,A             ; SAVE VALUE
;       SET     DSPMOD
        LDAX    B               ; (A) = DSPMOD
        RRC
        JC      MTR5            ; IF IN ALTER MODE
        MOV     A,E             ; (A) = CODE

;       HAVE A COMMAND (NOT A VALUE)

MTR4    SUI     4               ; (A) = COMMAND
        JC      ERROR           ; IF BAD
        MOV     E,A
        PUSH    H               ; SAVE ABUSS VALUE
        LXI     H,MTRA
        MVI     D,0
        DAD     D               ; (H,L) = ADDRESS OF TABLE ENTRY
        MOV     E,M
        DAD     D               ; (H,L) = ADDRESS OF PROCESSOR
        XTHL                    ; SET ADDRESS, (H,L) = (ABUSS)
        LXI     D,REGI          ; (D,E) = ADDRESS OF REG INDEX
;       SET     DSPMOD
        LDAX    B               ; (A) = DSPMOD
        ANI     2               ; SET 'Z' IF MEMORY
        LDAX    B               ; (A) = DSPMOD
        RET                     ; JUMP TO PROCESSOR

RTRA                            ; JUMP TABLE
        DB      GO-$            ; 4 - GO
        DB      IN- $           ; 5 - INPUT
        DB      OUT-$           ; 6 - OUTPUT
        DB      SSTEP-$         ; 7 - SINGLE STEP
        DB      RMEM-$          ; 8 - CASSETTE LOAD
        DB      WMEM-$          ; 9 - CASSETTE DUMP
        DB      NEXT-$          ; + - NEXT
        DB      LAST-$          ; - - LAST
        DB      ABORT-$         ; * - ABORT
        DB      RW-$            ; / - DISPLAY/ALTER
        DB      MEMM-$          ; # - MEMORY MODE
        DB      REGM-$          ; . - REGISTER MODE

;       PROCESS MEMORY/REGISTER ALTERATIONS.
;
;       THIS CODE IS ENTERED IF
;
;       1) AM IN ALTER MODE, AND
;       2) A KEY FROM 0-7 WAS ENTERED.

MTR5    RRC
        MOV     A,E             ; (A) = VALUE
        JC      MTR6            ; IS REGISTER
        STC                     ; INDICATE 1ST DIGIT IS IN (A)
        CALL    IOB             ; INPUT OCTAL BYTE
        INX     H               ; DISPLAY NEXT LOCATION

;       SAE - STORE ABUSS AND EXIT.
;
;       ENTRY   (HL) = ABUSS VALUE
;       EXIT    TO (RET)
;       USES    NONE

SAE     SHLD    ABUSS
        RET

;       ALTER REGISTER

MTR6    PUSH    PSW             ; SAVE CODE
        CALL    LRA             ; LOCATE REGISTER ADDRESS
        ANA     A
        JZ      ERROR           ; NOT ALLOWED TO ALTER STACKPOINTER
        INX     H
        POP     PSW             ; RESTORE VALUE AND CARRY FLAG
        JMP     IOA             ; INPUT OCTAL ADDRESS


;       REGM - ENTER REGISTER DISPLAY MODE.
;
;       ENTRY   (A) = DSPMOD
;               (BC) = #DSPMOD

REGM    MVI     A,2             ; SET DISPLAY REGISTER MODE
;       SET     DSPMOD
        STAX    B               ; SET DISPLAY REGISTER MODE
        ERRNZ   DSPMOD-DSPROT-1
        DCX     B               ; (BC) = #DSPROT
        XRA     A
        STAX    B               ; SET ALL PERIODS ON
        CALL    RCK             ; READ KEY ENTRY
        DCR     A               ; DISPLACE
        CPI     6
        JNC     ERROR           ; NOT 1-6
        RLC
        STAX    D               ; SET NEW REG IND
;       SET     REGI
        RET

;       RW - TOGGLE DISPLAY/ALTER MODE.
;
;       ENTRY   (A) = DSPMOD
;               (BC) = ADDRESS OF DSPMOD

;       SET     DSPMOD
RW      XRI     1
        STAX    B
        RET

;       NEXT - INCREMENT DISPLAY ELEMENT
;
;       ENTRY   (HL) = (ABUSS)
;               (DE) = ADDRESS OF REGIND

NEXT    INX     H
        JZ      SAE             ; IF MEMORY, STORE VALUES AND EXIT

;       IS REGISTER MODE.

;       SET     REGI
        LDAX    D               ; (A) = REGI
        ADI     2               ; INCREMENT REGISTER INDEX
        STAX    D               ; WRAP TO *SP*
        CPI     12
        RC                      ; IF NOT TOO LARGE, EXIT
        XRA     A               ; OVERFLOW
        STAX    D
ABORT   RET

;       LAST - INCREMENT DISPLAY ELEMENT
;
;       ENTRY   (HL) = (ABUSS)
;               (DE) = ADDRESS OF REGIND
;

LAST    DCX     H
        JZ      SAE             ; IF MEMORY, STORE AND EXIT

;       IS REGISTER MODE

;       SET     REGI
LST2    LDAX    D               ; (A) = REGI
        SUI     2
        STAX    D
        RNC                     ; IF OK
        MVI     A,10            ; UNDERFLOW TO *PC*
        STAX    D
        RET

;       MEMM - ENTER DISPLAY MEMORY MODE
;
;       ENTRY   (BC) = ADDRESS OF DSPMOD

MEMM    XRA     A               ; (A) = 0
;       SET     DSPMOD
        STAX    B               ; SET DISPLAY MEMORY MODE
        ERRNZ   DSPMOD-DSPROT-1
        DCX     B               ; (BC) = #DSPROT
        STAX    B               ; SET ALL PERIODS ON
        LXI     H,ABUSS+1
        JMP     IOA             ; INPUT OCTAL ADDRESS

;       IN - INPUT DATA BYTE.
;
;       OUT - OUTPUT DATA BYTE.
;
;       ENTRY   (HL) = (ABUSS)

IN      MVI     B,MI.IN
        DB      MI.LXID         ; SKIP NEXT INSTRUCTION
OUT     MVI     B,MI.OUT
        MOV     A,H             ; (A) = VALUE
        MOV     H,L             ; (H) = PORT
        MOV     L,B             ; (L) = IN/OUT INSTRUCTION
        SHLD    IOWRK
        CALL    IOWRK           ; PERFORM IO
        MOV     L,H             ; (L) = PORT
        MOV     H,A             ; (H) = VALUE
        JMP     SAE             ; STORE ABUSS AND EXIT

;       GO - RETURN TO USER MODE
;
;       ENTRY   NONE

GO      JMP     GO.             ; ROUTINE IS IN WASTE SPACE

;       SSTEP - SINGLE STEP INSTRUCTION
;
;       ENTRY   NONE

SSTEP                           ; SINGLE STEP
        DI                      ; DISABLE INTERRUPTS UNTIL THE RIGHT TIME
        LDA     CTLFLG
        XRI     CB.SSI          ; CLEAR SINGLE STEP INHIBIT
        OUT     OP.CTL          ; PRIME SINGLE STEP INTERRUPT
SST1    STA     CTLFLG          ; SET NEW FLAG VALUES
        POP     H               ; CLEAN STACK
        JMP     INTXIT          ; RETURN TO USER ROUTINE FOR STEP

;       STPRTN - SINGLE STEP RETURN

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

RMEM    LXI     H,YPABT
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

LOAD
        LXI     B,1000Q-RT.MI*256-256 ; (BC) = - REQUIRED TYPE AND #
LOA0    CALL    SRC             ; SCAN FOR RECORD START
        MOV     L,A             ; (HL) = COUNT
        XCHG                    ; (DE) = COUNT, (HL) = TYPE AND #
        DCR     C               ; (C) = - NEXT #
        DAD     B
        MOV     A,H
        PUSH    B               ; SAVE TYPE AND #
        PUSH    PSW             ; SAVE TYPE CODE
        ANI     177Q            ; CLEAR END FLAG BIT
        ORA     L
        MVI     A,2             ; SEQUENCE ERROR
        JNZ     TPERR           ; IF NOT RIGHT TYPE OF SEQUENCE
        CALL    RNP             ; READ ADDR
        MOV     B,H
        MOV     C,A             ; (BC) = P-REG ADDRESS
        MVI     A,10
        PUSH    D               ; SAVE (DE)
        CALL    LRA.            ; LOCATE REG ADDRESS
        POP     D               ; RESTORE (DE)
        MOV     M,C             ; SET P-REG IN MEM
        INX     H
        MOV     M,B
        CALL    RNP             ; READ ADDRESS
        MOV     L,A             ; (HL) = ADDRESS, (DE) = COUNT
        SHLD    START

LOA1    CALL    RNB             ; READ BYTE
        MOV     M,A
        SHLD    ABUSS           ; SET ABUSS FOR DISPLAY
        INX     H
        DCX     D
        MOV     A,D
        ORA     E
        JNZ     LOA1            ; IF MORE TO GO

        CALL    CTC             ; CHECK TAPE CHECKSUM

;       READ NEXT BLOCK

        POP     PSW             ; (A) = FILE TYPE BYTE
        POP     B               ; (BC) = -(LAST TYPE, LAST #0
        RLC
        JC      TFT             ; ALL DONE - TURN OFF TAPE
        JMP     LOAD            ; READ ANOTHER RECORD

;       DUMP - DUMP MEMORY TO MAG TAPE.
;
;       DUMP SPECIFIED MEMORY RANGE TO MAG TAPE.
;
;       ENTRY   (START) = START ADDRESS
;               (ABUSS) = END ADDRESS
;               USER PC = ENTRY POINT ADDRESS
;       EXIT    TO CALLER.

WHEN
        LXI     H,TPABT
        SHLD    TPERRX          ; TAPE ERROR EXIT

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
        CALL    WNP
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

WHE2    MOV     A,M
        CALL    WNB             ; WRITE BYTE
        SHLD    ABUSS           ; SET ADDRESS FOR DISPLAY
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

TFT     XRA     A
        OUT     OP.TPC          ; TURN OFF TAPE

;       HORN - MAKE NOISE.
;
;       ENTRY   (A) = (MILLISECOND COUNT)/2
;       EXIT    NONE
;       USES    A,F

ALARM   MVI     A,200/2         ; 200 MS BEEP
HORN    PUSH    PSW
        MVI     A,CB.SPK        ; TURN ON SPEAKER

HRN0    XTHL                    ; SAVE (HL), (H) = COUNT
        PUSH    D               ; SAVE (DE)
        XCHG                    ; (D) = LOOP COUNT
        LXI     H,CTLFLG
        XRA     M
        MOV     E,M             ; (E) = OLD CTLFLG VALUE
        MOV     M,A             ; TURN ON HORN
        MVI     L,TICCNT

        MOV     A,B             ; (A) = CYCLE COUNT
        ADD     M
HRN2    CMP     M               ; WAIT REQUIRED TICCOUNTS
        JNZ     HRN2
        MVI     L,CTLFLG
        MOV     M,E             ; TURN HORN OFF
        POP     D
        POP     H
        RET

;       CTC - VERIFY CHECKSUM.
;
;       ENTRY   TAPE JUST BEFORE CRC
;       EXIT    TO CALLER IF OK
;               TO *TPERR* IF BAD
;       USES    A,F,H,L

CTC     CALL    RNP             ; READ NEXT PAIR
        LHLD    CRCSUM
        MOV     A,H
        ORA     L
        RZ                      ; RETURN IF OK
        MVI     A,1             ; CHECKSUM ERROR
;       JMP     TPERR           ; (B) = CODE

;       TPERR - PROCESS TAPE ERROR.
;
;       DISPLAY ERR NUMBER IN LOW BYTE OF ABUSS
;
;       IF ERROR NUMBER EVEN, DON'T ALLOW #
;       IF ERROR NUMBER ODD, ALLOW #
;
;       ENTRY   (A) = NUMBER

TPERR   STA     ABUSS
        MOV     B,A             ; (B) = CODE
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

TPABT   XRA     A
        OUT     OP.TPC          ; OFF TAPE
        JMP     ERROR

;       TPXIT - CHECK FOR USER FORCED EDIT.
;
;       TPXIT CHECKS FOR AN `*` KEYPAD ENTRY. IF SO, TAKE
;       THE TAPE DRIVER ABNORMAL EXIT.
;
;       ENTRY   NONE
;       EXIT    TO *RET* IF NOT '*'
;               (A) = PORT STATUS
;               TO (TPERRX) IF '*' DOWN
;       USES    A,F

TPXIT   IN      IP.PAD
        CPI     01101111B       ; *
        IN      IP.TPC          ; READ TAPE STATUS
        RNZ                     ; NOT '*', RETURN WITH STATUS
        LHLD    TPERRX
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
;       EXIT    TAPE POSITIONED (AND MOVING), CRCSUM=0
;               (DE) = HEADER BYTES
;               (HL) = RECORD COUNT
;       USES    A,F,D,E,H,L

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
        JNC     SRA1            ; NOT ENOUGH
        SHLD    CRCSUM          ; CLEAR CRC-16
        CALL    RNP             ; READ LEADER
        MOV     D,H
        MOV     E,A
;       JMP     RNP             ; READ COUNT

;       RNP - READ NEXT PAIR.
;
;       RNP READS THE NEXT TWO BYTES FROM THE INPUT DEVICE.
;
;       ENTRY   NONE
;       EXIT    (H,A) = BYTE PAIR
;       USES    A,F,H

RNP     CALL    RNB             ; READ NEXT BYTE
        MOV     H,A
;       JMP     RNB             ; READ NEXT BYTE

;       RNB - READ NEXT BYTE
;
;       RNB READS THE NEXT SINGLE BYTE FROM THE INPUT DEVICE.
;       THE CHECKSUM IS TAKEN FOR THE CHARACTER.
;
;       ENTRY   NONE
;       EXIT    (A) = CHARACTER
;       USES    A,F

RNB     MVI     A,UCI.R0+UCI.CR+UCI.RE ; TURN ON READER FOR NEXT BYTE
        OUT     OP.TPC
RNB1    CALL    TPXIT           ; CHECK FOR *, READ STATUS
        ANI     USR.RXR
        JZ      RNB1            ; IF NOT READY
        IN      IP.RFB          ; INPUT DATA
;       JMP     CRC             ; CHECKSUM

;       CRC - COMPUTE CRC-16
;
;       CRC COMPUTES A CRC-16 CHECKSUM FOR THE POLYNOMIAL
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
;               (A) UNCHANGED
;       USES    F

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
        MOV A,L
        XRI     5Q
        MOV     L,A
CRC2    MOV     A,C
        DCR     B
        JNZ     CRC1            ; IF MORE TO GO
        SHLD    CRCSUM
        POP     H               ; RESTORE (HL)
        POP     B               ; RESTORE (BC)
        RET                     ; EXIT

;       WNP - WRITE NEXT PAIR
;
;       WNP WRITE THE NEXT TWO BYTES TO THE CASSETTE DRIVE.
;
;       ENTRY   (H,L) = BYTES
;       EXIT    WRITTEN.
;       USES    A,F

WNP     MOV     A,H
        CALL    WNB
        MOV     A,L
;       JMP     WNB             ; WRITE NEXT BYTE

;       WNB - WRITE NEXT BYTE
;
;       WNB WRITE THE NEXT BYTE TO THE CASSETTE TAPE.
;
;       ENTRY   (A) = BYTE
;       EXIT    NONE.
;       USES    F

WNB     PUSH    PSW
WNB1    CALL    TPXIT           ; CHECK FOR #, READ STATUS
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
;               (D,E) = (0,A)
;       USES    A,D,E,H,L,F

LRA     LDA     REGI
LRA.    MOV     E,A
        MVI     D,0
        LHLD    REGPTR
        DAD     D               ; (DE) = (REGPTR)+(REGI)
        RET

;       IOA - INPUT OCTAL NUMBER
;
;       ENTRY   (H,L) = ADDRESS OF RECEPTION DOUBLE BYTE.
;       EXIT    TO *RET* IF ERROK
;               TO *RET*+1 IF OK, VALUE IN MEMORY.
;       USES    A,B,E,H,C,F

IOA     CALL    IOB             ; INPUT BYTE
        DCX     H

;       IOB - INPUT OCTAL BYTE.
;
;       READ ONE OCTAL BYTE FROM THE KEYSET.
;
;       ENTRY   (H,L) = ADDRESS OF BYTE TO HOLD VALUE
;               'C' SET IF FIRST DIGIT IN (A)
;       EXIT    TO *RET* IF ALL OK
;               TO *ERROR* IF ERROR
;       USES    A,D,E,H,L,F

IOB     MVI     D,3             ; (D) = DIGIT COUNT
IOB1    CNC     RCK             ; READ CONSOLE KEYSET

        CPI     8
        JNC     ERROR           ; IF ILLEGAL DIGIT

        MOV     E,A             ; (E) = VALUE
        MOV     A,H
        RLC                     ; SHIFT 3
        RLC
        RLC
        ANI     370Q
        ORA     E
        MOV     M,A             ; REPLACE
        DCR     D
        JNZ     IOB1            ; IF NOT DONE
        MVI     A,30/2          ; BEEP FOR 30 MS
        JMP     HORN

;       DOD - DECODE FOR OCTAL DISPLAY
;
;       ENTRY   (H,L) = ADDRESS OF LED REFRESH AREA
;               (B) = *OR* PATTERN TO FORCE ON BARS OR PERIODS
;               (A) = OCTAL VALUE
;       EXIT    (H,L) = HEX DIGIT ADDRESS
;       USES    A,B,C,D,H,L

DOD     PUSH    D
        MVI     D,DODA/256
        MVI     C,3
DOD1    RAL                     ; LEFT 3 PLACES
        RAL
        RAL
        PUSH    PSW             ; SAVE FOR NEXT DIGIT
        ANI     7
        ADI     DODA
        MOV     E,A             ; (D) = INDEX
        LDAX    D               ; (A) = PATTERN
        XRA     B
        ANI     177Q
        XRA     B
        MOV     M,A             ; SET IN MEMORY
        INX     H
        MOV     A,B
        RLC
        MOV     B,A
        POP     PSW             ; (A) = VALUE
        DCR     C
        JNZ     DOD1            ; IF MORE TO GO
        POP     D
        RET                     ; RETURN

;       UFD - UPDATE FRONT PANEL DISPLAYS.
;
;       UFD IS CALLED BY THE CLOCK INTERRUPT PROCESSOR WHEN IT IS
;       TIME TO UPDATE THE DISPLAY CONTENTS. CURRENTLY, THIS IS DONE
;       EVERY 32 INTERRUPTS, OR ABOUT 32 TIMES A SECOND.
;
;       ENTRY   (H,L) = ADDRESS OF REFCNT
;       EXIT    NONE
;       USES    ALL

UFD
        MVI     A,UO.DDU
        ANA     B
        RNZ                     ; IF NOT TO HANDLE UPDATE

        MVI     L,DSPROT
        MOV     A,M
        RLC
        MOV     M,A             ; ROTATE PATTERN
        MOV     B,A
        INX     H
        ERRNZ   DSPMOD-DSPROT-1
        MOV     A,M             ; (A) = DSPMOD
        ANI     2
        LHLD    ABUSS
        JZ      UFDI            ; IF MEMORY

;       AM DISPLAYING REGISTERS

        CALL    RA              ; LOCATE REGISTER ADDRESS
        PUSH    H
        LXI     H,DSPA
        DAD     D               ; (H,L) = ADDRESS OF REG NAME PATTERNS
        MOV     A,M
        INX     H
        MOV     H,M
        MOV     L,A             ; (H,L) = REG NAME PATTERN
        XTHL
        ORA     H               ; CLEAR 'Z'
        MOV     A,M
        INX     H
        MOV     H,M
        MOV     L,A             ; (HL) = ADDRESS OF REGISTER PAIR CONTENTS

;       SETUP DISPLAY

UFD1    PUSH    PSW
        XCHG
        LXI     H,ALEDS
        MOV     A,D
        CALL    DOB             ; FORMAT ABANK HIGH HALF
        MOV     A,E
        CALL    DOD             ; FORMAT ABANK LOW HALF
        POP     PSW
        LDAX    D
        JZ      DOD             ; IF MEMORY, DECODE BYTE VALUE

;       IS REGISTER, SET REGISTER NAME,

        MVI     M,377Q          ; CLEAR DIGIT
        POP     H
        SHLD    DLEDS+1
        RET

;       RCK - READ CONSOLE KEYPAD.
;
;       RCK IS CALLED TO READ A KEYSTROKE FROM THE CONSOLE KEYPAD.
;       WHENEVER A KEY IS ACCEPTED.
;       RCK PERFORMS DEBOUNCING, AND AUTO-REPEAT. A *BIP* IS SOUNDED
;       WHEN A VALUE IS ACCEPTED.
;
;       KEY PAD VALUES:
;
;       1111 1110  -  0
;       1111 1100  -  1
;       1111 1010  -  2
;       1111 1000  -  3
;       1111 0110  -  4
;       1111 0100  -  5
;       1111 0010  -  6
;       1111 0000  -  7
;       1110 1111  -  8
;       1100 1111  -  9
;       1010 1111  -  +
;       1000 1111  -  -
;       0110 1111  -  *
;       0100 1111  -  /
;       0010 1111  0  #
;       0000 1111  -  .
;
;
;       ENTRY   NONE
;       EXIT    TO CALLER WHEN A KEY IS HIT
;               (A) = 0 - '0'
;                     1 - '1'
;                     2 - '2'
;                     3 - '3'
;                     4 - '4'
;                     5 - '5'
;                     6 - '6'
;                     7 - '7'
;                     8 - '8'
;                     9 - '9'
;                    10 - '+ '
;                    11 - '-'
;                    12 - '*'
;                    13 - '/'
;                    14 - '#'
;                    15 - '.'
;       USES    A,F

RCK
        PUSH    H
        PUSH    B
        MVI     C,400/20        ; WAIT 400 MS
        LXI     H,RCKA

RCK1    IN      IP.PAD          ; INPUT PAD VALUE
        MOV     B,A             ; (B) = VALUE
        MVI     A,20/2          ; WAIT 20 MS
        CALL    DLY
        MOV     A,B
        CMP     M
        JNZ     RCK2            ; HAVE A CHANGE
        DCR     C
        JNZ     RCK1            ; WAIT N CYCLE

;       HAVE KEY VALUE

RCK2    MOV     M,A             ; UPDATE RCKA
        XRI     376Q            ; INVERT ALL BUT GROUP 0 FLAG
        RRC
        JNC     RCK3            ; HIT BANK 0
        RRC
        RRC
        RRC
        RRC
        JNC     RCK1            ; NO HIT AT ALL
RCK3    MOV     B,A             ; (B) = CODE
        MVI     A,4/2
        CALL    HORN            ; MAKE BIP
        MOV     A,B
        ANI     17Q
        POP     B
        POP     H
        RET                     ; RETURN

;       DISPLAY SEGMENT CODING:
;
;       BYTE = 76 5453 210
;
;          1
;         ---
;       6|   |2
;        | 0 |
;        ---
;       5|   |3
;        |   |
;         --- .7
;          4

;       REGISTER INDEX TO 7-SEGMENT PATTERN

DSPA
        DW      1001100010100100B ; SP
        DW      1001110010010000B ; AF
        DW      1000110110000110B ; BC
        DW      1000110011000010B ; DE
        DW      1000111110010010B ; HL
        DW      1100111010011000B ; PC

;       OCTAL TO 7-SEGMENT PATTERN

DODA
        DB      00000001B       ; 0
        DB      01110011B       ; 1
        DB      01001000B       ; 2
        DB      01100000B       ; 3
        DB      00110010B       ; 4
        DB      00100100B       ; 5
        DB      00000100B       ; 6
        DB      01110001B       ; 7
        DB      00000000B       ; 8
        DB      00100000B       ; 9


;       I/O ROUTINES TO BE COPIED INTO AND USED IN RAM.
;
;       MUST CONTINUE TO 3777A FOR PROPER COPY.
;       THE TABLE MUST ALSO BE BACKWARDS TO THE FINAL RAM.

        ORG     4000Q-7

PRSROM
        DB      1               ; REFIND
        DB      0               ; CTLFLG
        DB      0               ; .MFLAG
        DB      0               ; DSPMOD
        DB      0               ; DSPROT
        DB      10              ; REGI

        ERRNZ   *-4000Q

;       THE FOLLOWING ARE CONTROL CELLS AND FLAGS USED BY THE KEYPAD
;       MONITOR.

        ORG     20000Q          ; 8192
START   DS      2               ; DUMP STARTING ADDRESS
IOWRK   DS      2               ; IN OR OUT INSTRUCTION
PRSRAM                          ; FOLLOWING CELLS INITIALIZED FROM ROM
        DS      1               ; RET

REGI    DS      1               ; INDEX OF REGISTER UNDER DISPLAY
DSPROT  DS      1               ; PERIOD FLAG BYTE
DSPMOD  DS      1               ; DISPLAY MODE

.MFLAG  DS      1               ; USER FLAG OPTIONS
                                ; SEE *UI.XXX* BITS DESCRIBED AT FRONT

CTLFLG  DS      1               ; FRONT PANEL CONTROL BITS
REFIND  DS      1               ; REFRESH INDEX (0 TO 7)
PRSL    EQU     7               ; END OF AREA INITIALIZED FROM ROM

FPLEDS                          ; FRONT PANEL LED PATTERNS
ALEDS   DS      1               ; ADDR 0
        DS      1               ; ADDR 1
        DS      1               ; ADDR 2

        DS      1               ; ADDR 3
        DS      1               ; ADDR 4
        DS      1               ; ADDR 5

DLEDS   DS      1               ; DATA 0
        DS      1               ; DATA 1
        DS      1               ; DATA 2

ABUSS   DS      2               ; ADDRESS BUS
ACKA    DS      1               ; RCK SAVE AREA
CRCSUM  DS      2               ; CRC-16 CHECKSUM
TPERRX  DS      2               ; TAPE ERROR EXIT ADDRESS
TICCNT  DS      2               ; CLOCK TIC COUNTER

REGPTR  DS      2               ; REGISTER CONTENTS POINTER

UIVEC                           ; USER INTERRUPT VECTOR
        DS      3               ; JUMP TO CLOCK PROCESSOR
        DS      3               ; JUMP TO SINGLE STEP PROCESSOR
        DS      3               ; JUMP TO I/O 3
        DS      3               ; JUMP TO I/O 4
        DS      3               ; JUMP TO I/O 5
        DS      3               ; JUMP TO I/O 6
        DS      3               ; JUMP TO I/O 7

        END
