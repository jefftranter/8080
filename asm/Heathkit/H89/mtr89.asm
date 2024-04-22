        INCLUDE macros.mac

;       MTR88 - H89 MONITOR
;
;       MTR89 IS A MODIFICATION OF MTR88 BY REX CHEN IN MAY, 1980.
;       MTR89 IS IDENTICAL TO THE MTR88 IN THAT ALL ENTRY POINTS TO
;       THE CURRENT ROUTINES REMAIN UNCHANGED AND ALL ROUTINES
;       REMAIN UNALTERED WITH THE FOLLOWING EXCEPTIONS:
;
;         (1). ALL CODE WHICH SUPPORTS THE CASSETTE IS REMOVED.
;              THIS INCLUDES THE LOAD ('L') AND DUMP ('D') COMMANDS.
;              AS WELL AS ALL OF THE DEVICE DRIVERS.
;         (2). 'TYPE SPACES TO DETERMINE BAUD RATE' MESSAGE IS REMOVED.
;         (3). THE BOOTSTRAP FOR THE Z-47 IS INSTALLED.
;         (4). 15 SECONDS TIME OUT FOR Z-87, OR H-17 AND Z-47 IS INSERTED.
;         (5). <DELETE> KEY SERVES AS AN ABORT-BOOT KEY.
;         (6). ALLOWS BOOT FROM SELECTED DEVICE AND UNIT.
;
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
;       COPYRIGHT  05/1980, ZENITH DATA SYSTEMS INC.
;                           ST. JOSEPH, MI.

RAM     EQU     0

        IF      RAM
        ENDIF

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

        INCLUDE  mtr88.asm      ; DEFINE MTR88 OLD EQUATES

        INCLUDE  z47def.asm     ; DEFINE Z47 EQUATES

        INCLUDE  h17def.asm     ; EQUATES FOR H17 BOOT ROM

        INCLUDE  hosequ.asm     ; HDOS EQUATES

        INCLUDE  misc.asm       ; MISCELLANEOUS EQUATES FOR H17 BOOT ROM

        INCLUDE u8251.asm       ; DEFINE 8251 USART BITS

        INCLUDE u8250.asm       ; DEFINE 8250 ACE BITS

;       INTERRUPT VECTORS.
;

;       LEVEL 0 - RESET
;
;       THIS 'INTERRUPT' MAY NOT BE PROCESSED BY A USER PROGRAM.

        IF      RAM
        ELSE
        ORG     00Q
        ENDIF

INIT0   JMP     INIT0X          ; DO H88 EXTENSION OF INITIALIZATION
INIT0.0 LXI     H,PRSRAM+PRSL-1 ; (HL) = RAM DESTINATION FOR CODE
        JMP     INIT            ; INITIALIZE

        IF      RAM
        ELSE
        ERRPL   INIT-1000Q      ; BYTE IN WORD 10A MUST BE 0
        ENDIF

;       LEVEL 1 - CLOCK

        IF      RAM
        ELSE
INT1    EQU     10Q             ; INTERRUPT ENTRY POINT

        ERRNZ   *-110           ; INT0 TAKES UP ONE BYTE
        ENDIF

        CALL    SAVALL          ; SAVE USER REGISTERS
        MVI     D,0
        JMP     CLOCK           ; PROCESS CLOCK INTERRUPT
        IF      RAM
        ELSE
        ERRPL   CLOCK-1000Q     ; EXTRA BYTE MUST BE 0
        ENDIF

;       LEVEL 2 - SINGLE STEP
;
;       IF THIS INTERRUPT IS RECEIVED WHEN NOT IN MONITOR MODE,
;       THEN IT IS ASSUMED TO BE GENERATED BY A USER PROGRAM
;       (SINGLE STEPPING OR BREAKPOINTING). IN SUCH CASE, THE
;       USER PROGRAM IS ENTERED THROUGH (UIVEC+3)

        IF      RAM
        ELSE
INT2    EQU     20Q             ; LEVEL 2 ENTRY

        ERRNZ   *-21Q           ; INT1 TAKES EXTRA BYTE
        ENDIF

        CALL    SAVALL          ; SAVE REGISTERS
        LDAX    D               ; (A) = (CTLFLG)
;       SET     CTLFLG
        JMP     STPRTN          ; STEP RETURN

;       I/O INTERRUPT VECTORS.
;
;       INTERRUPTS 3 THROUGH 7 ARE AVAILABLE FOR GENERAL I/O USE.
;
;       THESE INTERRUPTS ARE NOT SUPPORTED BY MTR88, AND SHOULD
;       NEVER OCCUR UNLESS THE USER HAS SUPPLIED HANDLER ROUTINES
;       (THROUGH UIVEC).

        IF      RAM
        ELSE
        ORG     30Q
        ENDIF

INT3    JMP     UIVEC+6         ; JUMP TO USER ROUTINE

        DB      '44440'         ; HEATH PART NUMBER 444-10

        IF      RAM
        ELSE
        ORG     40Q
        ENDIF

INT4    JMP     UIVEC+9         ; JUMP TO USER ROUTINE

        DB      44Q,122Q,116Q,102Q,44Q ; SUPPORT CODE

        IF      RAM
        ELSE
        ORG     50Q
        ENDIF

INT5    JMP     UIVEC+12        ; JUMP TO USER ROUTINE


;       DLY - DELAY TIME INTERVAL
;
;       ENTRY   (A) = MILLISECOND DELAY COUNT/2
;       EXIT    NONE
;       USES    A,F

        IF      RAM
        ELSE
        ERRNZ   *-53A
        ENDIF

DLY     PUSH    PSW             ; SAVE COUNT
        XRA     A               ; DONT SOUND HORN
        JMP     HRN0            ; PROCESS AS HORN

        IF      RAM
        ELSE
        ORG     60Q
        ENDIF

INT6    JMP     UIVEC+15        ; JUMP TO USER ROUTINE

GO.     MVI     A,CB.SSI+CB.CLI+CB.SPK ; OFF MONITOR MODE LIGHT
        JMP     SST1            ; RETURN TO USER PROGRAM

        IF      RAM
        ELSE
        ORG     70Q
        ENDIF

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

        IF      RAM
        ELSE
        ERRNZ   *-73Q
        ENDIF

INIT    LDAX    D               ; COPY *PRSROM* INTO RAM
        MOV     M,A             ; MOVE BYTE
        DCX     H               ; DECREMENT DESTINATION
        INR     E               ; INCREMENT SOURCE
        JNZ     INIT            ; IF NOT DONE

SINCR   EQU     2000Q           ; SEARCH INCREMENT

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

        IF      RAM
        ELSE
        SPHL                    ; SET STACKPOINTER = MEMORY LIMIT -1
        ENDIF

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

        IF      RAM
        ELSE
        ERRNZ   *-132Q
        ENDIF

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

        IF      RAM
        ELSE
;       ENTRY POINT FOR THE Z80 NMI
;

        ERRNZ   *-66H           ; Z80 NMI ADDRESS
        ENDIF

NMIENT  JMP     NMI

        IF      RAM
        ELSE
        ERRNZ   SAVALLR-151Q    ; DO NOT CHANGE ORGANIZATION
        ENDIF

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

        IF      RAM
        ELSE
        ERRNZ   *-165Q
        ENDIF

;       SET     MFLAG           ; REFERENCE TO MFLAG
CUI1    LDAX    B               ; (A) = MFLAG
        ERRNZ   UO.CLK-1        ; CODE ASSUMED = 01
        RRC
        CC      UIVEC           ; IF SPECIFIED, TRANSFER TO USER

;       RETURN TO PROGRAM FROM INTERRUPT.

        IF      RAM
        ELSE
        ERRNZ  *-172Q
        ENDIF

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

        IF       RAM
        ELSE
        ERRNZ   *-201Q
        ENDIF

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


;       THIS IS ONLY A PORTION OF THE DYNAMIC RAM TEST!!
;
;       WAIT BEFORE MAKING ANOTHER LOOP

DYMEM6  LXI     H,0
DYMEM7  DCX     H
        MOV     A,H
        ORA     L
        JNZ     DYMEM7          ; IF (B,C) NOT ZERO

        JMP     DYMEM4          ; TRY AGAIN BY INCREMENTING ONCE MORE

;       HAVE A FAILURE PRIOR TO REACHING END OF MEMORY!
;
DYMEM9  XCHG
        LXI     H,MSG.ERR       ; DISPLAY ERROR MESSGE

;       LD      IX,DY9.3        ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DY9.3

        JMP     DYMSG

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

        IF      RAM
        ELSE
        ERRNZ   *-322Q
        ENDIF

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

        IF      RAM
        ELSE
        ERRNZ   *-344Q
        ENDIF

MTR     EI

MTR1    LXI     H,MTR1
        PUSH    H               ; SET 'MTR1' AS RETURN ADDRESS
        LXI     H,MSG.PR        ; TYPE PROMPT MESSAGE
MTR.15  CALL    TYPMSG

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
        PCHL                    ; GO TO ROUTINE

MTRA                            ; JUMP TABLE

        DB      'G'             ; GO TO USER ROUTINE
        DW      GO88

        DB      'S'             ; SUBSTITUTE MEMORY MODE
        DW      SUBM

        DB      'P'             ; PROGRAM COUNTER ALTER MODE
        DW      PCA

        DB      'B'             ; BOOT H-17 OR Z-47 DRIVE
        DW      BOOT

MTRAL   EQU     ($-MTRA)/3      ; NUMBER OF TABLE ENTRYS   /JWT 790507/

;       BSMSG   - BOOT SECONDARY DEVICE MESSAGE

BSMSG   DB      ' SD',0         ; 'SECONDARY DEVICE'

ERRMSG  DB      '?',0           ; ERROR MESSAGE

        ORG     447Q
;       MSG.ERR - ERROR MESSAGE FOR RAM TEST
;
;       "ERROR @ "

MSG.ERR DB      A.CR,A.LF,A.LF
        DB      'ERROR @ '
        DB      0


;       SAE - STORE ABUSS AND EXIT.
;
;       ENTRY   (HL) = ABUSS VALUE
;       EXIT    TO (RET)
;       USES    NONE

        IF      RAM
        ELSE
        ERRNZ   *-1063Q
        ENDIF

SAE     SHLD    ABUSS
        RET

;       PIN     -       PORT IN
;
;       PIN INPUTS A BYTE FROM DISK
;
;       ENTRY:  NONE
;
;       EXIT:   (A) = INPUT BYTE FROM Z47
;
;       USE:    AF

PIN     CALL    IN.             ; GET STATUS
        ANI     S.DTR           ; CHECK FOR DATA TERMINAL REQUEST
        CPU     Z80
        JR      Z,PIN           ; IF READY, WAIT
        CPU     8080
        CALL    IN1.            ; INPUT A BYTE FROM PORT
        RET

        ORG     503Q
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

;       AUTOBO - AUTO BOOT
;
;       ENTRY:  NONE
;
;       EXIT:   (SEE 'DEVICE' ROUTINE)
;
;       USE:    ALL

AUTOBO  XRA     A               ; SET TO PRIMARY FLAG
        CALL    DEVICE          ; CHECK DEVICE INFORMATION
        JMP     BOOT0           ; GOTO BOOT IT

        ORG     622Q
;       GO - RETURN TO USER MODE
;
;       ENTRY   NONE

        IF      RAM
        ELSE
        ERRNZ   *-1222Q
        ENDIF

GO      JMP     GO.             ; ROUTINE IS IN WASTE SPACE

;       SSTEP - SINGLE STEP INSTRUCTION.
;
;       ENTRY   NONE

        IF      RAM
        ELSE
        ERRNZ   *-1225Q
        ENDIF

SSTEP                           ; SINGLE STEP
        DI                      ; DISABLE INTERRUPTS UNTIL THE RIGHT TIME
        LDA     CTLFLG
        XRI     CB.SSI          ; CLEAR SINGLE STEP INHIBIT
        OUT     OP.CTL          ; PRIME SINGLE STEP INTERRUPT
SST1    STA     CTLFLG          ; SET NEW FLAG VALUES
        POP     H               ; CLEAN STACK
        JMP     INTXIT          ; RETURN TO USER ROUTINE FOR STEP

;       STPRTN - SINGLE STEP RETURN

        IF      RAM
        ELSE
        ERRNZ   *-1244Q
        ENDIF

STPRTN
        ORI     CB.SSI          ; DISABLE SINGLE STEP INTERRUPTION
        OUT     OP.CTL          ; TURN OFF SINGLE STEP ENABLE
;       SET     CTLFLG
        STAX    D
        ANI     CB.MTL          ; SEE IF IN MONITOR MODE
        JNZ     MTR
        JMP     UIVEC+3         ; TRANSFER TO USER'S ROUTINE

;       NBOOT   -               NORMAL BOOT
;
;       NBOOT IS ENTERED WHEN USER TYPES 'BOOT:' COMMAND FROM MONITOR.
;       IT WILL ACCEPT THE BOOT DEVICE AS WELL AS THE UNIT NUMBER FROM
;       CONSOLE AND GO TO THE BOOT CODE.
;
;       ENTRY:  NONE
;
;       EXIT:   (AIO.UNI) = UNIT NUMBER TO BOOT
;               (PRIM) = PORT ADDRESS OF THE BOOT DEVICE
;               (TMFG) = DEVICE TYPE, =1 IS Z47, =0 IS H17
;
;       USE:    ALL

NBOOT   XRA     A               ; SET Z FLAG TO PRIMARY DEVICE
NBOOT0  CALL    DEVICE          ; READ SWITCH TO DETERMINE BOOT DEVICE
START1  CALL    RCC             ; INPUT FROM KB
        CPI     A.CR            ; IF INPUT IS CR
        CPU     Z80
        JR      Z,BOOT0         ;     THEN TAKE IT AS DRIVE 0
        CPU     8080
        CPI     '0'             ; CHECK INPUT IS WITHIN DRIVE 0 - (B)
        CPU     Z80
        JR      C,WRONG         ;   IF LESS THEN 0, WRONG INPUT
        CPU     8080
        CMP     B
        DB      MI.EXAF         ; SAVE INPUT, CHECK PRIM OR SEC?
        CPU     Z80
        JR      Z,NB7           ; IF PRIMARY, CHECK 'S'
        CPU     8080
        DB      MI.EXAF         ; RESTORE (Z) FLAG
WRONG
        MVI     A,A.BEL         ; NOT THE CASES, BEEP!
        CALL    WCC
        CPU     Z80
        JR      START1          ; AND TRY AGAIN
        CPU     8080

NB7     DB      MI.EXAF         ; RESTORE INPUT & PRIM, SEC FLAG
        ANI     01011111B       ; MASK TO UPPER CASE LETTER
        CPI     'S'             ; CHECK THE USER LIKE TO BOOT FROM
        CPU     Z80
        JR      NZ,WRONG        ; BOOT SECONDARY DEVICE
        CPU     8080

;       USER WISHES TO BOOT FROM SECONDARY DEVICE

BSEC
        LXI     H,BSMSG         ; PRINT BOOT SECONDARY MESSAGE
        CALL    TYPMSG
        INR     A               ; SET (Z)=0 FOR SECONDARY DEVICE
        CPU     Z80
        JR      NBOOT0
        CPU     8080

;       SAVE THE AIO.UNI, CHECK IF THERE IS THE BOOT DEVICE AND GO!

BOOT0   XRA     A               ; TAKE CR OR AUTO BOOT AS DRIVE 0
        CPU     Z80
        JR      BOOT6
        CPU     8080

BOOT5   CALL    WCC             ; PRINT UNIT NUMBER
        SUI     '0'             ; MAKE IT BINARY
        MOV     B,A             ; SAVE THE UNIT #
        CALL    WCR             ; WAIT FOR A CR
        MOV     A,B             ; GET UNIT NUMBER BACK
BOOT6   STA     AIO.UNI         ; STORE THE UNIT #
        MOV     A,H             ; CHECK IF NO DEVICE AT ADDR. PORT
        ANA     A
        JZ      NODEV           ; NO DEVICE
        PCHL                    ; JMP TO THE EXECUTION ROUTINE

;       Z47     -               BOOT FORM Z47 DISK DRIVE
;
;       Z47 WILL LOAD DATA FROM DISK TRACK 0 SECTOR 1 AND 2 TO
;       USER FIRST AVAILABLE RAM LOCATION. IF THE BOOT IS SUCCED,
;       CONTROL OASS TO THAT LOCATION.
;
;       ENTRY:  (AIO.UNI) = UNIT NUMBER TO BOOT
;
;       EXIT:   NONE
;
;       USE:    ALL

Z47
;       LD      (STK),SP        ; SAVE STACK POINTER FOR RE-BOOT
        DB      355Q,163Q
        DW      STK

Z47A
        EI                      ; LET THE TIMER FLY
        LDA     AIO.UNI         ; GET UNIT NUMBER
        RLC                     ; SET TO SIDE/UNIT/SECTOR FORMAT
        RLC
        RLC
        RLC
        RLC
        INR     A               ; SET TO SECTOR 1
        MOV     C,A             ; SAVE SIDE/UNIT/SECTOR (SIDE=0)
RESET   MVI     A,W.RES         ; RESET Z47
        CALL    OUT.

; DETERMINE THE DISK IS SINGLE OR DOUBLE DENSITY

        MVI     A,DC.RAS        ; SEND READ AUX. STATUS COMMAND
        CALL    COM
        MOV     A,C             ; GET SIDE/UNIT/SECTOR
        CALL    DAT             ; SEND SECOND COMMAND BYTE
        CALL    PIN             ; GET AUX. STATUS BYTE
        ANI     AS.0DD          ; CHECK IT IS SINGLE OR DOUBLE DENSITY
        RLC
        XRI     10000000B       ; REVERSE THE 7TH BIT, MAKE THE SECTOR
        MOV     B,A             ; # TO 128 OR 256(B=0) BYTES

;       READ BOOT CODE FROM Z47

        LXI     H,USERFWA       ; BOOT DESTINATION
        PUSH    B               ; SAVE SECTOR SIZE & SIDE/UNIT/SECTOR
        CALL    RDBLCK          ; READ A SECTOR FROM DISK
        POP     B               ; GET SECTOR SIZE & SUS BACK
        INR     C               ; SET TO NEXT SECTOR
        CALL    RDBLCK          ; READ ANOTHER SECTOR

;       CHECK ANY ERROR DURING BOOT

        CALL    IN.             ; GET INTERFACE STATUS
        ANI     S.ERR           ; IS THERE ANY ERROR WHEN BOOT
        CPU     Z80
        JR      NZ,NODEV        ;      THEN ABORT
        CPU     8080
        STA     MFLAG           ; STOP TIMER
        JMP     USERFWA

;       RETRY   -               RE-BOOT Z47
;
;       RETRY IS ENTERED WHEN 3.5 SECONDS TIME OUT & BOOT Z47
;       STILL NO SUCCEED. IT RESTORE STACK & JUMP TO BOOT Z47 ROUTINE
;
;       ENTRY:  NONE
;
;       EXIT:   (HL) = (SP)
;
;       USE:    HL, SP

RETRY   LHLD    STK             ; GET OLD STACK ADDRESS
        SPHL                    ; SET TO STACK POINTER
        CPU     Z80
        JR      Z47A            ; RE-BOOT
        CPU     8080


;       R.SDP   - SET DEVICE PARAMETER, ALLOW TO SET DRIVE 0, 1, AND 2.
;       (MORE INFORMATION CAN BE FOUND IN H17 ROM CODE 36062A)

R.SDP   MVI     A,ERPTCNT
        STA     D.OECNT         ; SET MAX ERROR COUNT FOR OPERATION
        LDA     AIO.UNI         ; LOAD DRIVE NUMBER
        PUSH    PSW             ; SAVE IT
        CPI     2               ; IS IT DRIVE 2?
        CPU     Z80
        JR      C,R.SDP1        ; IF NOT JMP TO H17 ROM ROUTINE
        CPU     8080
        MVI     A,3
R.SDP1  JMP     SDP3

;       CKAUTO  -       CHECK IF IT IS AUTO BOOT
;
;       CKAUTO IS ENTERED FROM MONITOR LOOP. IT WILL CHECK IF AUTO BOOT
;       CONDITION IS TRUE.IF NOT, BACK TO MONITOR LOOP.
;               IF AUTO BOOT, JUMP TO BOOT DEVICE ROUTINE.
;
;       ENTRY:  NONE
;
;       EXIT:   NONE
;
;       USE:    ALL

CKAUTO  IN      H88.SW          ; GET SWITCH DATA
        ANI     H88S.AT         ; CHECK AUTO BOOT SWITCH BIT SET
        CPU     Z80
        JR      Z,CHAT2         ; NOT SET
        CPU     8080
        LXI     H,AUTOB         ; SET AUTO BOOT FLAG ADDR.
        CMP     M               ; CHECK AUTO BOOT BEFORE?
        JNZ     ATB             ; YES, AUTO BOOT
CHAT2   LXI     H,MSG.PR        ; LOAD 'HI' ADDR.
        JMP     MTR.15          ; BACK TO MONITOR LOOP


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
        MVI     L,TICCNT#256

        MOV     A,D             ; (A) = CYCLE COUNT
        ADD     M
HRN2    CMP     M               ; WAIT REQUIRED TICCOUNTS
        JNZ     HRN2

        JMP     HRNX            ; JUMP TO AN EXTENSION OF HORN SO ROOM
                                ; CAN BE MADE FOR A JUMP TO NOISE

ALARMB  JMP     NOISE           ; SEND A BELL TO THE CONSOLE


;       NODEV   - NO DEVICE AT THE UNIT USER INDICATE
;
;       NODEV IS ENTERED WHEN:  1. 15 SECONDS TIME OUT
;                            OR 2. NO DEVICE IS INDICATED ON SWITCH
;                            OR 3. USER HIT <DELETE> TO ABORT BOOT
;                            OR 4. BOOT ERROR
;       IT WILL EXIT TO 'ERROR' ROUTINE AND MONITOR LOOP
;
;       ENTRY:  NONE
;
;       EXIT:   (A) = 0
;
;       USE:    AF, HL

NODEV   LXI     H,ERRMSG        ; PRINT ERROR MESSAGE
        CALL    TYPMSG
        STA     MFLAG           ; STOP TIMER
        OUT     DP.DC           ; OFF DISK
        JMP     ERROR           ; BACK TO MOHITOR LOOP


;       H17     - BOOT FROM H17 DISK SYSTEM
;                 (THIS IS THE MODIFICATION OF THE H17 BOOT ROUTINE.
;                  MORE INFORMATION CAN BE FOUND O H17 BOOT ROM 300008)
;
;       ENTRY:  NONE 
;
;       USE:    ALL

H17     LXI     B,BOOTAL        ; SET THE COUNT TO MOVE IN CONSTANTS AND VECTORS
        LXI     D,BOOTA         ; SET THE SOURCE ADDRESS
        LXI     H,D.CON         ; SET THE DESTINATION ADDRESS
        CALL    DMOVE           ; MOVE IT

;       SET ADDRESS FOR 'SET DEVICE PARAMETER' ROUTINE
;       TO HANDLE DISK DRIVE 0, 1, AND 2.

        LXI     H,R.SDP         ; SET THIS ROM ROUTINE ADDRESS
        SHLD    D.SDP           ; SET INTO RAM JUMP VECTOR
        EI                      ; RESTORE INTERRUPT

;       WAIT TILL USER INSERT THE DISK AND CLOSE THE DOOR
;       (TIMER INTERRUPT IS AFFECTED NOW)

        MVI    B,10             ; LOOK FOR SOME HOLE AND NO HOLE
        CALL   R.SDP            ; SELECT UNIT & MOTOR ON
H17A    CALL   WNH              ; WAIT FOR NO HOLE
        CALL   WHD              ; WAIT FOR HOLE
        CPU    Z80
        DJNZ   H17A
        CPU    8080

;       READ BOOT CODE

        CALL   R.ABORT          ; RESET THE DISK DRIVE
        LXI    D,USERFWA        ; SET THE LOAD LOCATION
        LXI    B,9*256          ; LOAD 9 SECTORS
        LXI    H,0              ; LOAD FROM TRACK 0 SECTOR 1
        CALL   R.READ           ; READ DISK BOOT CODE
        CPU    Z80
        JR     C,NODEV          ;   ERROR ON BOOT, BACK TO 'H:'
        CPU    8080

;       SETUP CLOCK INTERRUPT FOR H17 ONLY

        LXI   H,CLOCK17         ; LOAD CLOCK ROUTINE ADDRESS
        SHLD  UIVEC+1           ; SET IT INTO VECTOR LOCATION
        JMP   USERFWA           ; GOTO BOOT OCDE

;      DEVICE - DETERMINE BOOT WHICH DEVICE AT WHICH PORT
;
;       ENTRY:  Z FLAG ( Z=1 FOR PRIMARY, Z=0 FOR SECONDARY)
;
;       EXIT:   HL = DEVICE BOOT EXECUTION ADDRESS
;                     IF H=0 THEN NO DEVICE THERE
;                     (I.E. THE EXEC. ADDR. MUST RESIDENT > 1000A)
;               REG B = PRIMARY MAXI. DRIVE NUMBER
;                    IF Z47, #='4'; IF HL7, #='3'
;               (PRIM) = PRIMARY DEVICE PORT ADDRESS
;                    IF Z47 THEN THE PORT IS EITHER 170Q OR 174Q
;                    IF H17 THEN DON'T CARE (H17 BOOT ROM TAKE CARE IT)
;               (TMFG) = 1 IF BOOT FROM Z47, = 0 IF FROM H17
;
;       USE:    ALL

DEVICE  DB      MI.EXAF         ; SAVE Z FLAG

;       INITIAL VARIABLES

        DI                      ; NO INTERRUPT
        LXI     H,D.RAM         ; CLEAR H17 WORK RAM AREA
        MVI     B,D.RAML        ; LENGTH TO CLEAR
        CALL    DZERO
        OUT     DP.DC           ; OFF DISK
        STA     TICCNT          ; 0 TIMER COUNTER
        STA     MYCNT           ; 0.5 SECOND TIMER = 0

        INR     A               ; (A)=1
        STA     TMFG            ; SET TIMER TO Z47 FLAG
        ERRNZ   UO.CLK-1        ; TIMER INTERRUPT MUST = 1
        STA     MFLAG           ; ALLOW TIMER INTERRUPT
        LXI     H,UIVEC         ; SET ALL VECTOR TO EI/RET ADDRESS
BOOT2   MVI     M,MI.JMP
        INX     H
        MVI     H,EIXIT#256     ; STORE LS BYTE
        INX     H
        MVI     M,EIXIT/256     ; STORE MS BYTE
        INX     H
        ADD     A
        JP      BOOT2

        LXI     H,TMOUT         ; SET TIMER INTERRUPT VECTOR
        SHLD    UIVEC+1

        MVI     A,D.STA         ; ASSUME ALL DEVICE ARE Z47 & BOOT AT 170Q
        STA     PRIM            ; SINCE H17 BOOT ROM WILL TAKE CARE OF ITS MATTER
        LXI     H,Z47           ; SET Z47 BOOT ADD.
        MVI     B,'4'           ; SET MAX. UNIT TO 4

;       DETERMINE BOOT DEVICE AND ITS INFORMATION

        IN      H88.SW          ; READ SWITCH DATA
        PUSH    PSW             ; SAVE IN STACK
        ANI     H88S.DV         ; CHECK PRIMARY DEVICE ADDRESS
        DB      MI.EXAF         ; SAVE Z FLAG & GET Z' FOR PRIM. SEC. FLAG
        CPU     Z80
        JR      NZ,SECOND       ; IT SECONDARY
        DB      MI.EXAF
        JR      NZ,B170
        JR      B174
SECOND  DB      MI.EXAF
        JR      Z,B170          ; BOOT PRIMARY AT 170Q
        CPU     8080
B174    MVI     A,UP.DP         ; PRIMARY DEVICE IS AT 174Q
        STA     PRIM
        POP     PSW             ; GET SWITCH DATA BACK
        ANI     H88S.4          ; CHECK THIS IS Z47 OR H17
        CPU     Z80
        JR      Z,BH17          ; IT H17
        CPU     8080
        DCR     A
DEV2    RZ                      ; IT IS Z47
        DCR     H               ; NO DEVICE THERE, Z47 LOCATION MUST ON 1***A
        RET
        ERRNZ   Z47/256-1

;       PRIMARY DEVICE IS H17

BH17    LXI     H,H17           ; SET TO H17 EXECUTION LOCATION
        DCR     B               ; SET TO MAX 3 DRIVE
        STA     TMFG            ; SET TIMER INTERRUPT = 0 FOR H17
        RET

;       PRIMARY DEVICE IS AT PORT 170Q

B170    POP     PSW             ; GET SWITCH DATA
        ANI     H88S.0          ; CHECK ANY DEVICE IN 170Q
        CPI     00000100B       ; CHECK IF IT IS Z47
        CPU     Z80
        JR      DEV2
        CPU     8080


;       LRA - LOCATE REGISTER ADDRESS
;
;       ENTRY   NONE.
;       EXIT    (A) = REGISTER INDEX
;               (H,L) = STORAGE ADDRESS
;               (D,E) = (O,A)
;       USES    A,D,E,H,L,F

        IF      RAM
        ELSE
        ERRNZ   *-3047Q
        ENDIF

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

        IF      RAM
        ELSE
        ERRNZ   *-3062Q
        ENDIF

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

        IF      RAM
        ELSE
        ERRNZ   *-3066Q
        ENDIF

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

        IF      RAM
        ELSE
        ERRNZ    *-3122Q
        ENDIF

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


;       DYASC - DYNAMIC RAM ASCII OUTPUT TO CONSOLE
;
;       ENTRY:  (A) = CHARACTER TO OUTPUT
;               (IY) = RETURN ADDRESS
;
;       EXIT:   TO (IY)
;       USES:   A,C,F

DYASC
;       EX      AF,AF'          ; SAVE CHARACTER TO OUTPUT
        DB      MI.EXAF
DYASC1  IN      SC.ACE+UR.LSR   ; READ LINE STATUS REGISTER
        ANI     UC.THE
        JZ      DYASC1          ; WAIT IF UART CAN'T HOLD ANOTHER CHARACTER

;       EX      AF,AF'          ; GET CHARACTER TO OUTPUT
        DB      MI.EXAF
        OUT     SC.ACE+UR.THR   ; OUTPUT TO UART
;       JP      (IY)            ; RETURN TO CALLER
        DB      MI.JIYA,MI.JIYB

;       DYBYT - DYNAMIC RAM BYTE OUTPUT
;
;       ENTRY:  (A) = BYTE TO OUTPUT AS OCTAL
;               (IX) = RETURN ADDRESS
;       EXIT:   TO (IX)
;       USES    A,C,IF.F

DYBYT   MOV     C,A             ; SAVE CHARACTER
        ANI     11000000B       ; OUTPUT FIRST CHARACTER OF OCTAL VALUE
        RRC
        RRC
        RRC
        RRC
        RRC
        RRC
        ORI     00110000B       ; MAKE INTO ASCII

;       LD      IY,DYBYT.2
        DB      MI.LDYA,MI.LDYB
        DW      DYBYT,2

        JMP     DYASC

DYBYT.2 MOV     A,C             ; OUTPUT SECOND CHARACTER
        ANI     00111000B
        RRC
        RRC
        RRC
        ORI     00110000B       ; MAKE INTO ASCII

;       LD      IY,DYBYT.R      ; RETURN ADDRESS
        DB      MI.LDYA,MI.LDYB
        DW      DYBYT.4
        JMP     DYASC

DYBYT.4 MOV     A,C             ; OUTPUT LAST CHARACTER
        ANI     00000111B
        ORI     00110000B       ; MAKE ASCII

;       LD      IY,DYBYT.6      ; RETURN ADDRESS
        DB      MI.LDYA,MI.LDYB
        DW      DYBYT.6

        JMP     DYASC

DYBYT.6
;       JP      (IX)            ; RETURN TO CALLER
        DB      MI.JIXA,MI.JIXB

;       MSQ.PAS - PASS MESSAGE FOR DYNAMIC RAM TEST
;

MSG.PAS DB      A.CR,A.LF
        DB      '     Pass =   '
        DB      0


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

        RET                     ; ELSE, EXIT TO CALLER

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


;       THE FOLLOWING IS ONLY A PORTIJ OF THE DYNAMIC RAM TEST!!
;
DY9.3   XCHG
        MOV     A,H             ; OUTPUT MSB

;       LD      IX,DY9.4        ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DY9.4

        JMP     DYBYT

DY9.4   MOV     A,L             ; OUTPUT LSB

;       LD      IX,DY9.5        ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DY9.5

        CPU     Z80
        JR      DYBYT
        CPU     8080

DY9.5   XCHG                    ; SAVE ERROR ADDRESS
        LXI     H,MSG.EQ        ; OUTPUT ' = '

;       LD      IX,DY9.8        ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DY9.8

        JMP     DYMSG           ; OUTPUT STRING

DY9.8   LDAX    D               ; OUTPUT RAM CONTENTS

;       LD      IX,DYMEM10      ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DYMEM10

        JMP     DYBYT

DYMEM10 MVI     A,A.BEL         ; DING BELL

;       LD      IY,DY10.5       ; RETURN ADDRESS
        DB      MI.LDYA,MI.LDYB
        DW      DY10.5

        JMP     DYASC


;       IO ROUTINES TO BE COPIED INTO AND USED IN RAM.
;
;       MUST CONTINUE TO 3777A FOR PROPER COPY.
;       THE TABLE MUST ALSO BE BACKWARDS TO THE FINAL RAM

        IF      RAM
        ELSE
        ERRNZ   4000Q-7-*
        ENDIF

PRSROM
        DB      1               ; REFIND
        DB      0               ; CTLFLG
        DB      0               ; MFLAG
        DB      0               ; DSPMOD
        DB      0               ; DSPROT
        DB      10              ; REGI
        DB      MI.RET

        IF      RAM
        ELSE
        ERRNZ   *-4000Q
        ENDIF

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
        LXI     B,32400Q        ; APPROX. 100 MS
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


;       ATB     - AUTO BOOT ROUTINE CONTINUE

ATB     MOV     M,A             ; SET AUTO BOOT FLAG
        MVI     A,10            ; SET TO AUTO BOOT ROUTINE
        CALL    LRA.
        LXI     D,AUTOBO        ; SET AUTO BOOT ROUTINE
        CPU     Z80
        JR      BOOTX
        CPU     8080

        ORG     2256Q
;       BOOT H-17 OR Z47 ENTRY POINT FOR H88
;
;       ENTRY   NONE
;
;       EXIT    (DE) = NORMAL BOOT ROUTINE ADDRESS
;
;       USES    ALL

BOOT    LXI     H,MSG.BT        ; COMPLETE BOOT MESSAGE
        CALL    TYPMSG
        MVI     A,10
        CALL    LRA.            ; GET LOCATION OF USER PC
        LXI     D,NBOOT         ; SET ITS VALUE TO THE NORMAL BOOT ROUTINE
BOOTX   MOV     M,E
        INX     H

        JMP     GO.             ; DO IT


;       TMOUT   - BOOT CODE TIME OUT ROUTINE
;
;       TMOUT IS ENTERED FROM TIMER INTERRUPT EVER 100MS. AND IT WILL
;       EXIT:   IF BOOT SUCCESS THEN TIMER OFF.
;               IF 15 SECONDS TIME OUT AND BOOT IS NOT SUCCESS YES
;                  THEN ABORT BOOT Z47 & TO MONITOR LOOP
;               IF < 15S & 3.5S THEN RE-BOOT
;
;       ENTRY:  (TMFG)  = 1 IF THE TIME OUT IS FOR Z47
;                       = 0 IF THE TIME OUT IS FOR H17
;
;       EXIT:   NONE
;
;       USE:    ALL (WHEN RETURN, ALL REGISTERS ARE RESTORED)

TMOUT   IN      SC.ACE+UR.LSR   ; INPUT ACE LINE STATUS REGISTER
        ANI     UC.DR           ; SEE IF THERE IS A DATA READY
        CPU     Z80
        JR      Z,TMOUT4        ; CHECK IF IT IS <DELETE>
        CPU     8080

        IN      SC.ACE+UR.RBR   ; INPUT DATA FROM KB
        ANI     01111111B       ; IS IT <DEL>?
        CPI     A.DEL
        JZ      NODEV           ; IF IT, ABORT THE BOOT
                                ; ELSE IGNORE THE INPUT

TMOUT4  LXI     H,TMFG
        MOV     A,M
        ANA     A
        DB      MI.EXAF         ; SAVE Z FLAG
        LDA     TICCNT          ; GET TIC
        ANA     A               ; SET ZERO FLAG
        CPU     Z80
        JR      NZ,TMOUT2       ; NOT IN 0.5 SECOND
        CPU     8080
        INX     H               ; SET TO MYCNT
        ERRNZ   MYCNT-TMFG-1    ; MYCNT MUST FOLLOW TMFG
        INR     M               ; INCREASE THE COUNT FOR 0.5 SECOND
        MOV     A,M
        CPI     30              ; CHECK IF MORE THAN 15 SECONDS
        JNC     NODEV           ; NO DEVICE?
TMOUT1  SBI     7               ; IS IT 3.5 SECONDS?
        CPU     Z80
        JR      C,TMOUT2        ; IF NOT, WAIT
        JR      NZ,TMOUT1       ; CHECK MORE
        CPU     8080
        DB      MI.EXAF
        JNZ     RETRY           ; IF IT IS Z47, THEN RE-BOOT
        CPU     Z80
        JR      TMOUT3          ; IT IS H-17, CONTINUE IT CLOCK ROUTINE
        CPU     8080
TMOUT2  DB      MI.EXAF         ; CHECK IT IS Z47 OR H17
        RNZ                     ; Z47, THEN RETURN
TMOUT3  JMP     CLOCK17         ; CONTINUE H17 CLOCK ROUTINE


        ORG     2370Q
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
        JNZ     SUBM6           ; IF NOT

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
        JMP     SUBM9           ; TRY AGAIN

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

        CPI     '0'             ; < 0?
        JC      IROC1           ; IF < OCTAL

        CPI     '8'             ; > 8?
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
        CPI     '0'             ; MAKE SURE CHARACTER IS OCTAL
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
        CALL    WCC
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


;       DAT     - DATA BYTE OUTPUT TO Z-47
;
;       ENTRY:  (A) = BYTE TO OUTPUT
;
;       EXIT:   (A) = BYTE TO OUTPUT
;               (D) = S.DTR
;
;       USES:   AF, D

DAT     MVI     D,S.DTR         ; SET MATCH CONDITION TO DATA TRANSFER
        CPU     Z80
        JR      COM1            ; REQUEST BIT
        CPU     8080


;       COM      - OUTPUT COMMAND BYTE TO Z-47
;
;       ENTRY:  (A) = COMMAND BYTE
;
;       EXIT:   (A) = COMMAND BYTE
;               (D) = S.DON
;
;       USES:   AF, D

COM     MVI     D,S.DON         ; SET MATCH CONDITION TO DONE BIT
COM1    PUSH    PSW
WTDON1  CALL    IN.             ; READ CONTROLLER STATUS REGISTER
        ANA     D               ; GET MATCH BIT ONLY
        CPU     Z80
        JR      Z,WTDON1        ; IF NO MATCH, WAIT
        CPU     8080
        POP     PSW
        CALL    OUT1.           ; OUTPUT THE BYTE TO THE DATA PORT
        RET


        ORG     3045Q
;       HRNX - HORN EXTENSION ROUTINE
;
;       THIS IS AN EXTENSION TO *HORN* TO MAKE ROOM FOR A JUMP
;

HRNX    MVI     L,CTLFLG#256
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


;       OUT.    - OUTPUT BYTE TO Z-47
;
;       ENTRY:  (A) = OUTPUT BYTE
;
;       EXIT:   NONE
;
;       USES:   NONE

OUT.    PUSH    B
        MOV     B,A             ; SAVE THE OUTPUT DATA
        LDA     PRIM            ; GET PORT ADDRESS
OUT.1   MOV     C,A             ; SET TO REG C
        MOV     A,B             ; GET OUTPUT BYTE DATA BACK
;       OUT     (C),A           ; OUTPUT BYTE
        DB      355Q,171Q
        POP     B
        RET

        ORG     3100Q
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


;       RDBLCK  - INPUT A BLOCK FROM Z-47
;
;       RDBLCK READS IN A BLOCK FROM THE DISK CONTROLLER
;
;       ENTRY:
;               HL = LOAD ADDRESS
;               B  = COUNT
;               C  = SIDE/UNIT/SECTOR
;
;       EXIT:   NONE
;
;       USE:    ALL

RDBLCK
RD1     MVI     A,DC.REAB
        CALL    COM             ; SEND THE COMMAND
        XRA     A               ; FOR TRACK 0
        CALL    DAT             ; SEND IT TO DISK
        MOV     A,C             ; LOAD SIDE/UNIT/SECTOR
        CALL    DAT             ; SEND IT TO DISK

RD2     CALL    PIN             ; INPUT A BYTE FROM DISK
        MOV     M,A             ; STORE IN BUFFER
        INX     H               ; BUFFER TO NEXT ADDRESS
        CPU     Z80
        DJNZ    RD2
        CPU     8080
        RET                     ; CONTINUE


;       OUT1.   - OUTPUT A BYTE TO PORT (PRIM+1)
;
;       ENTRY:  (A) = OUTPUT PORT
;
;       EXIT:   NONE
;
;       USE:    NONE

OUT1.   PUSH    B
        MOV     B,A             ; SAVE THE OUTPUT DATA
        LDA     PRIM            ; GET PORT ADDRESS
        INR     A               ; SET TO (PRIM+1)
        CPU     Z80
        JR      OUT.1           ; GO TO OUTPUT ROUTINE
        CPU     8080

;       IN1.    - INPUT BYTE FROM (PRIM+1) PORT
;
;       ENTRY:  NONE
;
;       EXIT:   (A) = INPUT BYTE
;
;       USES:   A

IN1.    PUSH    B
        LDA     PRIM            ; GET PORT ADDRESS
        INR     A               ; SET TO (PRIM+1)
        CPU     Z80
        JR      IN.1
        CPU     8080

        ORG     3165Q
;       MSG.GO - (G)O
;
;       "GO"

MSG.GO  DB      "o ",0

;       IN.     - INPUT BYTE FROM PORT (PRIM)
;
;       ENTRY:  NONE
;
;       EXIT:   (A) = INPUT BYTE
;
;       USES:   A

IN.     PUSH    B
        LDA     PRIM            ; GET PORT ADDRESS
IN.1    MOV     C,A             ; SET ADDR. TO REG C.
;       IN      A,(C)
;       DB      355Q,170Q       ; INPUT BYTE
        POP     B
        RET

        ORG     3101Q
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


;       SPEED - ROTATIONAL SPEED TEST FOR 5.25 INCH DISK DRIVE
;
;       *SPEED* IS USED ONLY FOR GROSS ADJUSTMENT OF DRIVE ROTATIONAL
;       SPEED IF THE FIRST READ/WRITE TEST OF THE UNIT FAILS DURING SETUP.
;
;               USE OF *SPEED* IS AS FOLLOWS:
;
;                        1.  ENTER 'GO AND THE ENTRY ADDRESS OF *SPEED*
;                        2.  ADJUST DRIVE SPEED UNTIL DATA AT DISPLAYED
;                            EQUALS 200
;                                A,  IF SPEED < 200, TURN ADJUSTMENT CLOCKWISE
;                                B,  IF SPEED < 200, TURN COUNTERCLOCKWISE
;
;       THE ABOVE TEST ADJUSTS SY0:.  TO ADJUST SY1:, USE HDOS

;       LABLE EQUIVALENCES
;
;       I/O PORTS
OP.DC   EQU     177Q            ; DRIVE CONTROL OUTPUT PORT
IP.DS   EQU     177Q            ; DRIVE STATUS INPUT PORT

;       MASKS
;
DS.HOLE EQU     00000001B       ; DRIVE STATUS SECTOR/INDEX HOLD

;       CONSTANTS
;
ONDR0   EQU     022Q            ; TURN ON SY0:


SPEED   LXI     H,MSG.SPD       ; OUTPUT SPEED MESSAGE
        CALL    TYPMSG
        MVI     A,0             ; SET FLAG AT IOWRK FOR "WORKING" MESSAGE
        STA     IOWRK
        MVI     A,ONDR0         ; TURN ON DRIVE ZERO
        OUT     OP.DC
SPEED1  LHLD    TICCNT          ; GET TICK COUNTER
        MOV     A,M             ; FORM TWO'S COMPLEMENT OF TICK COUNTER
        CMA
        MOV     D,A             ; (D,E) = NEGATIVE TICK COUNTER
        MOV     A,L
        CMA
        INR     A
        MOV     E,A
        JNC     SPEED2          ; IF NO CARRY FROM LSB

        INR     D               ; ELSE, INCREMENT MSB
SPEED2  LXI     B,0             ; ZERO REV COUNTERS
SPEED3  IN      IP.DS           ; INPUT DISK STATUS
        ANI     DS.HOLE         ; MASK FOR SECTOR/INDEX PULSES
        JZ      SPEED3          ; IF NO HOLE PRESENT

; HOLE PRESENT, WAIT FOR IT TO LEAVE
;
SPEED4  IN      IP.DS           ; GET DISK STATUS
        ANI     DS.HOLE         ; GET HOLE PULSES
        JNZ     SPEED4          ; WAIT UNTIL HOLE IS GONE AND WE HAVE MEDIA

        INR     B               ; INCREMENT HOLE COUNTER
        MOV     A,B             ; TEST FOR FIVE REVOLUTIONS
        CPI     56
        JNZ     SPEED3          ; NOT FIVE, WAIT FOR MORE HOLES

;       HAVE FIVE REVS, DISPLAY DIFFERENCE OF TICK COUNTER AND EXPECTED TIME DIF

        LHLD    TICCNT          ; GET CURRENT TICK VALUE
        DAD     D               ; SUBTRACT START VALUE
        LXI     D,177777Q-500+1+200Q ; SUBTRACT 500 FOR REVS, +200Q FOR OFFSET
        DAD     B               ; (H,L) = OFFSET RESULT
        PUSH    H               ; SAVE RESULT
        LXI     H,MSG.WRK       ; POINT TO 'WORKING' MESSAGE
        LDA     IOWRK           ; GET 'WORKING' FLAG
        XRI     1               ; INVERT LOWER BIT
        STA     IOWRK           ; SAVE NEW VALUE
        JNZ     SPEED5          ; IF TO DISPLAY 'WORKING'

        LXI     H,MSG.HSS       ; POINT TO 'HOME', 'SPACES', AND SPEED MSG
SPEED5  CALL    TYPMSG          ; OUTPUT MESSAGE
        POP     H               ; GET TEST RESULT
        CALL    TOA.            ; OUTPUT RESULT TO CONSOLE
        JMP     SPEED1          ; PERFORM ANOTHER SAMPLE

;       MSG.SPD - SPEED TEST MESSAGE
;
;       '       Disk drive rotational speed test.
;
;
;                       Drive speed = '
MSG.SPD DB      A.ESC,'E',A.LF
        DB      '       Disk drive rotational speed test.',A.CR,A.LF,A.LF
        DB      '               Drive speed = '
        DB      0

;       MSG.WRK - 'WORKING' MESSAGE FOR SPEED TEST
;
;       DISPLAYS 'WORKING' AT HOME POSITION AND RETURNS CURSOR TO SPEED =

MSG.WRK DB      A.ESC,'H'       ; CURSOR HOME
        DB      'Working'
        DB      A.ESC,'Y#>'     ; CURSOR ADDRESS OF SPEED = VALUE
        DB      0               ; END MESSAGE

;       MSG.HSS - BLANKS 'WORKING' MESSAGE
;

MSG.HSS DB      A.ESC,'H'       ; CURSOR HOME
        DB      '       '       ; BLANKS
        DB      A.ESC,'Y#>'     ; CURSOR ADDRESS OF SPEED = VALUE
        DB      0               ; END MESSAGE


;       DYMEM - DYNAMIC MEMORY TEST
;
;       DYMEM TEST THE DYNAMIC MEMORY IN THE H88/H89 BY PLACING
;       A KNOWN PATTERN IN EACH DYNAMIC MEMORY CELL AND THEN
;       PERFORMING A READ, INCREMENT, READ SEQUENCE WITH A DELAY
;       BETWEEN EACH PASS OF THE TEST.
;
;       ENTRY:  NONE
;
;       EXIT:   ON RESET
;
;       USES:   A,B,C,D,E,H,L,F,A',F',IX,IY

DYMEM   MVI     A,0             ; MAKE SURE CLOCK AND SINGLE STEP ARE OFF
        OUT     H88.CTL

;       DETERMINE END OF MEMORY

        IF      RAM
        ELSE
DYMEM1  LXI     H,START
        ENDIF
        MVI     A,1
DYMEM2  MVI     M,0             ; SET RAM TO ZERO
        INR     M               ; SET MEMORY TO ONE
        CMP     M               ; SEE IF (A) = ((H,L))
        CPU     Z80
        JR      NZ,DYMEM3       ; IF NOT EQUAL, THE END OF RAM HAS BEEN REACHED
        CPU     8080     

        INX     H               ; ELSE, POINT TO NEXT LOCATION IN RAM
        CPU     Z80
        JR      DYMEM2
        CPU     8080

DYMEM3  DCX     H               ; POINT TO LAST GOOD LOCATION
        XCHG                    ; PUT ENDING ADDRESS IN D,E
        LXI     H,MSG.RAM       ; OUTPUT ENDING ADDRESS

;       LD      IX,DY3.3        ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DY3.3

        CPU     Z80
        JR      DYMSG
        CPU     8080

DY3.3   MOV     A,D             ; OUTPUT ADDRESS MSB

;       LD      IX,DY3.5        ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DY3.5

        JMP     DYBYT

DY3.5   MOV     A,E             ; LSB

;       LD      IX,DY3.7        ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DY3.7

        JMP     DYBYT

DY3.7   INX     D               ; (D,E) = LAST BYTE OF RAM + 1

;       TEST MEMORY
;
        MVI     B,1             ; (B) = CONTENTS OF RAM AFTER SIZING
        LXI     H,MSG.PAS       ; OUTPUT PASS MESSAGE

;       LD      IX,DYMEM4       ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DYMEM4

        CPU     Z80
        JR      DYMSG
        CPU     8080

        IF      RAM
        ELSE
DYMEM4  LXI     H,START         ; POINT BACK TO BEGINNING OF RAM
        ENDIF
DYMEM5  MOV     A,M             ; READ CURRENT CONTENTS
        CMP     B               ; SEE OF CORRENT CONTENTS STILL REMAIN
        JNZ     DYMEM9          ; FAILURE, SEE IF AT END OF RAM

        INR     A
        MOV     M,A             ; INCREMENT RAM
        CMP     M               ; SEE OF WRITE WAS SUCCESSFUL
        JNZ     DYMEM9

        INX     H
        MOV     A,L             ; GET LSB AND TEST FOR REACHING END OF RAM
        CMP     E
        CPU     Z80
        JR      NZ,DYMEM5       ; IF LSB NOT EQUAL
        CPU     8080

        MOV     A,H             ; CHECK LSB
        CMP     D
        CPU     Z80
        JR      NZ,DYMEM5
        CPU     8080

;       HAVE REACHED THE END OF MEMORY!
;       OUTPUT LAST VALUE TESTED
;
        MVI     H,3             ; OUTPUT 3 BACKSPACES
        MVI     A,A.BKS

DYME5.5
;       LD      IY,DY5.53       ; RETURN ADDRESS
        DB      MI.LDYA,MI.LDYB
        DW      DY5.53

        JMP     DYASC

DY5.53  DCR     H
        CPU     Z80
        JR      NZ,DYME5.5
        CPU     8080
        INR     B               ; SHOW NEXT PASS VALUE
        MOV     A,B             ; VALUE TESTED

;       LD      IX,DYMEM6       ; RETURN ADDRESS
        DB      MI.LDXA,MI.LDXB
        DW      DYMEM6

        JMP     DYBYT

;       THE DYNAMIC RAM TEST CONTINUES ELSEWHERE!!
;       AND THEN RETURNS TO HERE!!!!!!!!!!!!!!!!!!

DY10.5  LXI      H,0            ; DELAY AND DING BELL AGAIN
        MVI      B,2            ; 2 LOOPS
DYMEM11 DCR      H
        CPU      Z80
        JR       NZ,DYMEM11
        CPU      8080

        DCR      L
        CPU      Z80
        JR       NZ,DYMEM11
        CPU      8080

        DCR      B
        CPU      Z80
        JR       NZ,DYMEM11
        CPU      8080

        JMP      DYMEM10        ; AGAIN

;       DYMSG - DYNAMIC RAM TEST MESSAGE OUTPUT ROUTINE
;
;       ENTRY:  (H,L) = MESSAGE ADDRESS
;               (IX) = RETURN ADDRESS
;
;       EXIT:   TI (IX)
;
;       USES:   A,H,L,F,IY

DYMSG   MOV     A,M             ; GET MESSAGE BYTE

;       LD      IY,DYMSG.5      ; RETURN ADDRESS
        DB      MI.LDYA,MI.LDYB
        DW      DYMSG.5

        JMP     DYASC           ; OUTPUT ASCII

DYMSG.5 ORA     A               ; SEE IF NULL TO END STRING
        INX     H               ; POINT TO NEXT CHARACTER
        CPU     Z80
        JR      NZ,DYMSG        ; IF NOT DONE YET
        CPU     8080

;       JP      (IX)            ; RETURN TO CALLER
;       DB      MI.JIXA,MI.JIXB


;       MSG.RAM - RAM TEST MESSAGE
;

MSG.RAM DB      A.ESC,'E'
        DB      'Dynamic RAM test'
        DB      A.CR,A.LF,A.LF
        DB      '         LWA = '
        DB      0

;       MSG.EQ - EQUALS MESSAGE
;

MSG.EQ  DB      ' = '
        DB      0

        DB      'GAC.'

;       ENTRY POINT FOR FLOPPY DISK ROTATIONAL SPEED TEST
;
        IF      RAM
        ELSE
        ERRNZ   10000A-6-*      ; MUST BE 6 BYTES BEFORE END
        ENDIF

ESPEED  JMP     SPEED

;       ENTRY POINT FOR DYNAMIC MEMORY TEST
;
        IF      RAM
        ELSE
        ERRNZ   10000A-3-*      ; MUST BE 3 BYTES BEFORE END
        ENDIF

EDTMEM  JMP     DYMEM

        IF      RAM
        ELSE
        ERRNZ   *-10000A        ; MUST NOT EXCEED 2K BYTES
        ENDIF

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

;UIVEC                          ; USER INTERRUPT VECTORS
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

        ORG     20520Q
PRIM    DS      1               ; PRIMARY DEVICE ADDR. PORT
TMFG    DS      1               ; TIMER INTERRUPT FLAG, =1 FOR Z47, =0 FOR H17
MYCNT   DS      1               ; COUNTER FOR TIMER INTERRUPT
AUTOB   DS      1               ; AUTO BOOT FLAG
STK     DS      1               ; STACK POINTER FOR RE-BOOT

        ORG     20066
DATA    DS      1               ; OUTPUT 362Q DATA SAVE AREA
        END
