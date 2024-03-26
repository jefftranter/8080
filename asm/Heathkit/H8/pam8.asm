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
        JMP     HRNO            ; PROCESS AS HORN

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
        OUT     OP.IPC          ; SET 8 BIT, NO PARITY, 1 STOP, X16

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
