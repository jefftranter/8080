        INCLUDE macros.mac

;       MTR90 - H/Z-89 MONITOR                  ISSUE 09.02.01
;
;       MTR89 IS A MODIFICATION OF MTR88 BY REX CHEN IN MAY, 1980.
;       MTR89 IS IDENTICAL TO THE MTR88 IN THAT ALL ENTRY POINTS TO
;       THE CURRENT ROUTINES REMAIN UNCHANGED AND ALL ROUTINES
;       REMAIN UNALTERED WITH THE FOLLOWING EXCEPTIONS:
;
;         (1). 'TYPE SPACES TO DETERMINE BAUD RATE' MESSAGE IS REMOVED.
;         (2). THE BOOTSTRAP FOR THE Z-47 IS INSTALLED.
;         (3). 15 SECONDS TIME OUT FOR Z-87, OR H-17 AND Z-47 IS INSERTED.
;         (4). <DELETE> KEY SERVES AS AN ABORT-BOOT KEY.
;         (5). ALLOWS BOOT FROM SELECTED DEVICE AND UNIT.
;
;       MTR90 IS A MODIFICATION OF MTR89 TO ALLOW BOOTING FROM
;       THE H67, H37, AND 1 FUTURE DEVICE. ALSO THE H47 CODE WAS
;       CHANGED, AND HEXADECIMAL ROUTINES WERE ADDED.
;       SEVERAL NEW *CONVENIENCE* COMMANDS WERE ADDED, THANKS TO
;       THE ADDITION OF THE EXTRA 2K SPACE.
;
;       MTR90-1 Employs a software fix for a hardware deficiency in disk
;       drives. It seems that a disk drive head may go into the negative
;       track area (-1, -2, ...) and not known it, so all disk drivers
;       have been modified to step the head in and then issuing a second
;       restore command. This can be taken care of in the hardware, but
;       people are opposed to adjusting hardware properly.
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

MI.EXX  EQU     331Q            ; Z80 EXX INSTRUCTION

        INCLUDE mtr88.asm       ; DEFINE MTR88 OLD EQUATES

        INCLUDE h17def.asm      ; EQUATES FOR H17 BOOT ROM

        INCLUDE h37def.asm      ; DEFINE H37 PARAMETERS

        INCLUDE z47def.asm      ; DEFINE Z47 EQUATES

        INCLUDE h67def.asm      ; DEFINE H67 PARAMETERS

        INCLUDE hosequ.asm      ; HDOS EQUATES

        INCLUDE dirdef.asm

        INCLUDE esint.asm

        INCLUDE misc.asm        ; MISCELLANEOUS EQUATES FOR H17 BOOT ROM

        INCLUDE u8251.asm       ; DEFINE 8251 USART BITS

        INCLUDE u8250.asm       ; DEFINE 8250 ACE BITS

;       INTERRUPT VECTORS.
;

;       LEVEL 0 - RESET
;
;       THIS 'INTERRUPT' MAY NOT BE PROCESSED BY A USER PROGRAM.

        ORG     00Q

INIT0   JMP     INIT0X          ; DO H88 EXTENSION OF INITIALIZATION
INIT0.0 LXI     H,PRSRAM+PRSL-1 ; (HL) = RAM DESTINATION FOR CODE
        JMP     INIT            ; INITIALIZE

;       ERRPL   INIT-1000Q      ; BYTE IN WORD 10A MUST BE 0

;       LEVEL 1 - CLOCK

INT1    EQU     10Q             ; INTERRUPT ENTRY POINT

        ERRNZ   $-11Q           ; INT0 TAKES UP ONE BYTE

        CALL    SAVALL          ; SAVE USER REGISTERS
        MVI     D,0
        JMP     CLOCK           ; PROCESS CLOCK INTERRUPT
;       ERRPL   CLOCK-1000Q     ; EXTRA BYTE MUST BE 0

;       LEVEL 2 - SINGLE STEP
;
;       IF THIS INTERRUPT IS RECEIVED WHEN NOT IN MONITOR MODE,
;       THEN IT IS ASSUMED TO BE GENERATED BY A USER PROGRAM
;       (SINGLE STEPPING OR BREAKPOINTING). IN SUCH CASE, THE
;       USER PROGRAM IS ENTERED THROUGH (UIVEC+3)

INT2    EQU     20Q             ; LEVEL 2 ENTRY

        ERRNZ   $-21Q           ; INT1 TAKES EXTRA BYTE

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

        ORG     30Q

INT3    JMP     UIVEC+6         ; JUMP TO USER ROUTINE

        DB      102Q,61Q,64Q,62Q,102Q ; HEATH PART NUMBER 444-142

        ORG     40Q

INT4    JMP     UIVEC+9         ; JUMP TO USER ROUTINE

        DB      44Q,122Q,116Q,102Q,44Q ; SUPPORT CODE

        ORG     50Q

INT5    JMP     UIVEC+12        ; JUMP TO USER ROUTINE


;       DLY - DELAY TIME INTERVAL
;
;       ENTRY   (A) = MILLISECOND DELAY COUNT/2
;       EXIT    NONE
;       USES    A,F

        ERRNZ   $-53Q

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

        ERRNZ   $-73Q

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

        SPHL                    ; SET STACKPOINTER = MEMORY LIMIT -1

        PUSH    H               ; SET *PC* VALUE ON STACK
        LXI     H,ERROR
        PUSH    H               ; SET 'RETURN ADDRESS'

        DB      0,0,0,0         ; UNUSED BYTES

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

        ERRMI   132Q-$
        ORG     132Q
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

        ERRNZ   $-66H           ; Z80 NMI ADDRESS

NMIENT  JMP     NMI

;       ERRNZ   SAVALLR-151Q    ; DO NOT CHANGE ORGANIZATION

SAVALLR EQU     $               ; SAVALL EXTENSION RETURN ADDRESS

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

        ERRNZ   $-165Q

;       SET     MFLAG           ; REFERENCE TO MFLAG
CUI1    LDAX    B               ; (A) = MFLAG
        ERRNZ   UO.CLK-1        ; CODE ASSUMED = 01
        RRC
        CC      UIVEC           ; IF SPECIFIED, TRANSFER TO USER

;       RETURN TO PROGRAM FROM INTERRUPT.

        ERRNZ  $-172Q

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

        ERRNZ   $-201Q

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
;       ERRNZ   CTLFLG-MFLAG-1
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
        LXI     H,MSG.ERR       ; DISPLAY ERROR MESSAGE

        CPU     Z80
        LD      IX,DY9.3        ; RETURN ADDRESS
        CPU     8080

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

        ERRNZ   $-322Q

ERROR   LXI     H,MFLAG
        MOV     A,M             ; (A) = MFLAG
        ANI     377Q-UO.DDU-UO.NFR ; RE-ENABLE DISPLAYS
        MOV     M,A             ; REPLACE
        INX     H
        MVI     M,CB.SSI+CB.MTL+CB.CLI+CB.SPK ; RESTORE *CTLFLG*
;       ERRNZ   CTLFLG-.MFLAG-1
        EI
        LHLD    REGPTR
        SPHL                    ; RESTORE STACK POINTER TO EMPTY STATE
        CALL    ALARM

;       MTR - MONITOR LOOP.
;

        ERRNZ   $-344Q

MTR     EI

MTR1    LXI     H,MTR1
        PUSH    H               ; SET 'MTR1' AS RETURN ADDRESS
        JMP     CKAUTO          ; CHECK AUTO BOOT. IF NOT CONTROL BACK TO NEXT
MTR.15  CALL    TYPMSG          ; PRINT 'H:'

MTR.2   CALL    RCC             ; READ A CONSOLE CHARACTER
        ANI     01011111B       ; MAKE SURE ITS UPPER CASE TO MATCH TABLE
        LXI     H,MTRA          ; LOOK UP CHARACTER IN *MTRA*
        MVI     B,MTRAL         ; (B) = LENGTH OF TABLE
MTR.3   CMP     M               ; SEE IF CHARACTER FROM CONSOLE = TABLE ENTRY
        INX     H               ; ML = ADDRESS
        JZ      MTR.4           ; IF EQUAL

        INX     H               ; POINT TO NEXT TABLE ENTRY
        INX     H
        DCR     B               ; SEE IF PAST END OF TABLE
        JNZ     MTR.3           ; IF NOT PAST

        MVI     A,A.BEL         ; ELSE, DING ERROR
        CALL    WCC
        JMP     MTR.2           ; TRY AGAIN

MTR.4   CALL    WCC             ; WRITE CHARACTER BACK TO CONSOLE
        MOV     A,M
        INX     H               ; GET MSB
        MOV     H,M
        MOV     L,A             ; (H,L) = ROUTINE ADDRESS
        PCHL                    ; GO TO ROUTINE

;;      GETBND1 - CONTINUATION OF GETBND
;

GETBND1 LXI     H,IOWRK+1
        MVI     D,A.CR
        CALL    IOA

        LHLD    IOWRK
        MOV     C,L
        MOV     B,H
        POP     D
        POP     H
        RET

;;      VIEW - VIEW MEMORY BLOCKS
;
;       VIEW START,STOP

VIEW    LXI     H,MSG.VEW
        CALL    TYPMSG
        JMP     VIEW3A          ; GET START IN DE, STOP IN BC

VIEW1   SHLD    BLKICW          ; SAVE START ADDRESS FOR ASCII STUFF
        JMP     VIEW2

;       SAE - STORE ABUSS AND EXIT.
;
;       ENTRY   (HL) = ABUSS VALUE
;       EXIT    TO (RET)
;       USES    NONE

        ERRMI   463Q-$
        ORG     463Q

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

PIN     EQU     $
        CALL    IN.             ; GET STATUS
        ANI     S.DTR+S.DON     ; CHECK FOR DATA TERMINAL REQUEST
        CPU     Z80
        JR      Z,PIN           ; IF NOT READY, WAIT
        CPU     8080
        STC
        RP
        JMP     IN1.            ; INPUT A BYTE FROM PORT

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
GO88.1  CALL    WCR.            ; ECHO RETURN
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

        DB      0,0,0,0,0,0     ; UNUSED BYTES

        ORG     622Q
;       GO - RETURN TO USER MODE
;
;       ENTRY   NONE

        ERRNZ   $-622Q

GO      JMP     GO.             ; ROUTINE IS IN WASTE SPACE

;       SSTEP - SINGLE STEP INSTRUCTION.
;
;       ENTRY   NONE

        ERRNZ   $-625Q

SSTEP                           ; SINGLE STEP
        DI                      ; DISABLE INTERRUPTS UNTIL THE RIGHT TIME
        LDA     CTLFLG
        XRI     CB.SSI          ; CLEAR SINGLE STEP INHIBIT
        OUT     OP.CTL          ; PRIME SINGLE STEP INTERRUPT
SST1    STA     CTLFLG          ; SET NEW FLAG VALUES
        POP     H               ; CLEAN STACK
        JMP     INTXIT          ; RETURN TO USER ROUTINE FOR STEP

;       STPRTN - SINGLE STEP RETURN

        ERRNZ   $-644Q

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
        JR      Z,BOOT0.        ;     THEN TAKE IT AS DRIVE 0
        CPU     8080
        CALL    BOOT7
        CPU     Z80
        JR      C,WRONG
        CPU     8080
        CMP     B
        CPU     Z80
        JR      C,BOOT5         ;   IF WITHIN THE RANGE, BOOT IT!
        EX      AF,AF'          ; SAVE INPUT, CHECK PRIM OR SEC?
        JR      Z,NB7           ; IF PRIMARY, CHECK 'S'
        EX      AF,AF'          ; RESTORE (Z) FLAG
        CPU     8080
WRONG   EQU     $
        MVI     A,A.BEL         ; NOT THE CASE, BEEP!
        CALL    WCC
        CPU     Z80
        JR      START1          ; AND TRY AGAIN

NB7     EX      AF,AF'          ; RESTORE INPUT & PRIM. SEC FLAG
        CPU     8080
        ANI     01011111B       ; MASK TO UPPER CASE LETTER
        CPI     'S'             ; CHECK THE USER LIKE TO BOOT FROM
        CPU     Z80
        JR      NZ,WRONG        ; BOOT SECONDARY DEVICE
        CPU     8080

;       USER WISHES TO BOOT FROM SECONDARY DEVICE

BSEC    EQU     $
        LXI     H,BSMSG         ; PRINT BOOT SECONDARY MESSAGE
        CALL    TYPMSG
        INR     A               ; SET (Z)=0 FOR SECONDARY DEVICE
        CPU     Z80
        JR      NBOOT0
        CPU     8080

;       SAVE THE AIO.UNI, CHECK IF THERE IS THE BOOT DEVICE AND GO!

BOOT0.  CALL    WCR.            ; PRINT CR FOR GOOD LOOKS

BOOT0   XRA     A               ; TAKE CR OR AUTO BOOT AS DRIVE 0
        LXI     SP,21200Q       ; SET STACK FOR NO COMMAND LINE
        CPU     Z80
        JR      BOOT6
        CPU     8080

BOOT5   CALL    WCC             ; PRINT UNIT NUMBER
        SUI     '0'             ; MAKE IT BINARY
        JMP     CCL             ; CHECK FOR COMMAND LINE
BOOT6   STA     AIO.UNI         ; STORE THE UNIT #
        MOV     A,H             ; CHECK IF NO DEVICE AT ADDR. PORT
        ANA     A
        EI                      ; INSURE INTERRUPTS READY
        JZ      NODEV           ; NO DEVICE
        PCHL                    ; JMP TO THE EXECUTION ROUTINE

;       Z47     -               BOOT FROM Z47 DISK DRIVE
;
;       Z47 WILL LOAD DATA FROM DISK TRACK 0 SECTOR 10 thru 9 2 TO
;       USER FIRST AVAILABLE RAM LOCATION. IF THE BOOT IS SUCCEED,
;       CONTROL PASS TO THAT LOCATION.
;
;       ENTRY:  (AIO.UNI) = UNIT NUMBER TO BOOT
;
;       EXIT:   NONE
;
;       USE:    ALL

Z47     EQU     $
;       LD      (STK),SP        ; SAVE STACK POINTER FOR RE-BOOT
        DB      355Q,163Q
        DW      STK

Z47A    EQU     $
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
        CALL    Z47X            ; DO Z47 EXTENSION

;       READ BOOT CODE FROM Z47

        LXI     H,USERFWA       ; BOOT DESTINATION
        CALL    RDBLCK          ; READ A SECTOR FROM DISK
        JC      NODEV           ; IF READ ERROR

        LHLD    STK
        SPHL                    ; RESTORE STACK

        JMP     EUC             ; SET CLOCK AND ENTER USER CODE

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

R.SDP   EQU   $
        MVI     A,ERPTCNT
        STA     D.OECNT         ; SET MAX ERROR COUNT FOR OPERATION
        LDA     AIO.UNI         ; LOAD DRIVE NUMBER
        PUSH    PSW             ; SAVE IT
        CPI     2               ; IS IT DRIVE 2?
        CPU     Z80
        JR      C,R.SDP1        ; IF NOT JMP TO H17 ROM ROUTINE
        CPU     8080
        MVI     A,3
R.SDP1  JMP     SDP3

;;      VIEW2 - CONTINUE *VIEW* COMMAND
;

VIEW2   MOV     A,M             ; A = BYTE
        CALL    TOB
        MVI     A,' '           ; SPACE BETWEEN
        CALL    WCC
        CALL    VIEW4           ; CHECK FOR END
        JZ      VIEW9           ; IF ALL DONE
        CALL    VIEW3.          ; CHECK FOR END OF LINE
        JMP     VIEW3

MSG.VEW DB      'iew ',0

        DB      0,0,0,0,0,0,0   ; UNUSED BYTES
        DB      0,0,0,0,0,0,0

        ERRMI   1136Q-$
        ORG     1136Q
;       HORN - MAKE NOISE.
;
;       ENTRY   (A) = (MILLISECOND COUNT)/2
;       EXIT    NONE
;       USES    A,F

        ERRNZ   $-1136Q

ALARM   EQU     $
        CPU     Z80
        JR      ALARMB          ; BRANCH TO A JUMP TO NOISE TO DING BELL
        CPU     8080

        ERRNZ   $-1140Q

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
        CPU     Z80
        JR      NZ,HRN2
        CPU     8080

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

NODEV   EQU     $
        LXI     H,ERRMSG        ; PRINT ERROR MESSAGE
        CALL    TYPMSG
NODEV1  STA     MFLAG           ; STOP TIMER
        OUT     DP.DC           ; OFF DISK
        JMP     ERROR           ; BACK TO MONITOR LOOP


;       H17     - BOOT FROM H17 DISK SYSTEM
;                 (THIS IS THE MODIFICATION OF THE H17 BOOT ROUTINE.
;                  MORE INFORMATION CAN BE FOUND O H17 BOOT ROM 30000 A)
;
;       ENTRY:  (AIO.UNI) = THE UNIT TO BOOT
;
;       EXIT:   NONE
;
;       USE:    ALL

H17     EQU     $
        LXI     B,BOOTAL        ; SET THE COUNT TO MOVE IN CONSTANTS AND VECTORS
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

        CALL   H17X             ; H17 Extension Routine
        LXI    D,USERFWA        ; SET THE LOAD LOCATION
        LXI    B,9*256          ; LOAD 9 SECTORS
        LXI    H,0              ; LOAD FROM TRACK 0 SECTOR 1
        CALL   R.READ           ; READ DISK BOOT CODE
        CPU    Z80
        JR     C,NODEV          ;   ERROR ON BOOT, BACK TO 'H:'
        CPU    8080
        JMP    EUC.             ; VECTORS ALREADY IN

;      DEVICE - DETERMINE BOOT WHICH DEVICE AT WHICH PORT
;
;       ENTRY:  Z FLAG ( Z=1 FOR PRIMARY, Z=0 FOR SECONDARY)
;
;       EXIT:   HL = DEVICE BOOT EXECUTION ADDRESS
;               REG B = PRIMARY MAXI. DRIVE NUMBER
;                    IF Z47 ='4'; H17 ='3';H37 ='4';H67 ='4'
;               (PRIM) = PRIMARY DEVICE PORT ADDRESS
;               (TMFG) = SET UP FROM TABLE
;
;       USE:    ALL

        CPU     Z80
DEVICE  EQU     $
        EX      AF,AF'          ; SAVE Z FLAG
        CPU     8080

;       INITIAL VARIABLES

        DI                      ; NO INTERRUPT
        LXI     H,D.RAM         ; CLEAR H17 WORK RAM AREA
        MVI     B,D.RAML        ; LENGTH TO CLEAR
        CALL    DZERO
        OUT     DP.DC           ; OFF DISK
        STA     TICCNT          ; 0 TIMER COUNTER
        STA     MYCNT           ; 0.5 SECOND TIMER = 0

        INR     A               ; (A)=1
        ERRNZ   UO.CLK-1        ; TIMER INTERRUPT MUST = 1
        STA     MFLAG           ; ALLOW TIMER INTERRUPT
        LXI     H,UIVEC         ; SET ALL VECTOR TO EI/RET ADDRESS
BOOT2   MVI     M,MI.JMP
        INX     H
        MVI     M,EIXIT#256     ; STORE LS BYTE
        INX     H
        MVI     M,EIXIT/256     ; STORE MS BYTE
        INX     H
        ADD     A
        JP      BOOT2

        LXI     H,TMOUT         ; SET TIMER INTERRUPT VECTOR
        SHLD    UIVEC+1

;       DETERMINE BOOT DEVICE AND ITS INFORMATION

        IN      H88.SW          ; READ SWITCH DATA
        ANI     H88S.DV         ; DETERMINE WHICH TABLE IS PRIMARY
        CPU     Z80
        JR      Z,DEV174        ;   IF PORT 174 IS PRIMARY
        CPU     8080

;       PRIMARY DEVICE IS AT 170q

DEV170  IN      H88.SW          ; GET DEVICE SWITCHES

        CPU     Z80
        EX      AF,AF'          ; GET 'SD' FLAGS
        CPU     8080
        LXI     H,BT174         ; ASSUME PORT 174
        CPU     Z80
        JR      NZ,DEV2         ;    IF WAS 174

        JR      DEV1.           ; DO PORT 170 STUFF
        CPU     8080

;       DEVICE IS AT 174q

DEV174  IN     H88.SW           ; GET DEVICE DIPS

        CPU    Z80
        EX     AF,AF'           ; SAVE DIPS, RESTORE 'SD' FLAG
        CPU    8080
        LXI    H,BT174          ; ASSUME PRIMARY
        CPU    Z80
        JR     Z,DEV2

DEV1.   EX     AF,AF'           ; GET SWITCHES BACK
        CPU    8080
        RRC
        RRC                     ; MOVE BITS DOWN
        CPU    Z80
        EX     AF,AF'           ; AND SAVE AGAIN
        CPU    8080
        LXI    H,BT170          ; WAS PORT 170
;       JR     DEV2
;       ERRNZ  $-DEV2

;       HL = ADDRESS OF FWA PROPER TABLE

DEV2    MOV    A,M              ; FIRST BYTE IS PORT NUMBER
        STA    PRIM             ; (A) = DEVICE ADDRESS

        CPU    Z80
        EX     AF,AF'           ; (A) = DEVICE SPECIFIC FLAG
        CPU    8080
        ANI    H88S.4           ; MASK OFF UNIT BITS
        ADD    A
        ADD    A                ; 4 BYTE ENTRIES
        INX    H                ; HL = FWA OF TABLE ENTRIES
        MOV    E,A
        MVI    D,0              ; DE = OFFSET
        DAD    D                ; HL = ADDRESS OF DEVICE ENTRY

        MOV    A,M
        STA    TMFG             ; 1ST ENTRY IS TIME-OUT FLAG

        INX    H
        MOV    B,M              ; 2ND ENTRY IS UNIT NUMBER

        INX    H
        MOV    E,M
        INX    H                ; 3RD ENTRY IS BOOT ROUTINE ADDRESS
        MOV    D,M
        XCHG                    ; MOVE IT INTO HL
        RET

        DB      0,0,0,0,0,0,0   ; UNUSED BYTES

;       ERRMI   1447A-$
        ORG     1447Q
;       LRA - LOCATE REGISTER ADDRESS
;
;       ENTRY   NONE.
;       EXIT    (A) = REGISTER INDEX
;               (H,L) = STORAGE ADDRESS
;               (D,E) = (O,A)
;       USES    A,D,E,H,L,F

        ERRNZ   $-1447Q

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

        ERRNZ   $-1462Q

IOA0    JMP     IOA1
        NOP                     ; RETAIN H8 ORG

;       IOB - INPUT OCTAL BYTE
;
;       READ ONE OCTAL BYTE FROM THE KEYSET.
;
;       ENTRY   (H,L) = ADDRESS OF BYTE TO HOLD VALUE
;               'C' SET IF FIRST DIGIT N (A)
;       EXIT    NONE
;       USES    A,D,E,H,L,F

        ERRNZ   $-1466Q

IOB0    MVI     M,0             ; ZERO OUT OLD VALUE
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

        ERRNZ    $-1522Q

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

DYASC   EQU     $
        CPU     Z80
        EX      AF,AF'          ; SAVE CHARACTER TO OUTPUT
        CPU     8080
DYASC1  IN      SC.ACE+UR.LSR   ; TERMINAL READY?
        ANI     UC.THE
        JZ      DYASC1          ; NOT YET.

        CPU     Z80
        EX      AF,AF'          ; GET CHARACTER TO OUTPUT
        CPU     8080
        OUT     SC.ACE+UR.THR   ; OUTPUT TO UART
        CPU     Z80
        JP      (IY)            ; RETURN TO CALLER
        CPU     8080

;       DYBYT - DYNAMIC RAM BYTE OUTPUT
;
;       ENTRY:  (A) = BYTE TO OUTPUT AS OCTAL
;               (IX) = RETURN ADDRESS
;       EXIT:   TO (IX)
;       USES    A,C,IF.F

DYBYT   JMP     DYBYTX
DYBYT.1 ORI     '0'             ; MAKE ASCII

        CPU     Z80
        LD      IY,DYBYT.2
        CPU     8080

        JMP     DYASC

DYBYT.2 MOV     A,C
        ANI     00111000B
        RRC
        RRC
        RRC
        ORI     '0'

        CPU     Z80
        LD      IY,DYBYT.4      ; RETURN ADDRESS
        CPU     8080
        JMP     DYASC

DYBYT.4 MOV     A,C             ; OUTPUT LAST CHARACTER
        ANI     00000111B
        ORI     '0'

        CPU     Z80
        LD      IY,DYBYT.6      ; RETURN ADDRESS
        CPU     8080

        JMP     DYASC

DYBYT.6 EQU     $
        CPU     Z80
        JP      (IX)            ; RETURN TO CALLER
        CPU     8080

;       MSQ.PAS - PASS MESSAGE FOR DYNAMIC RAM TEST
;

MSG.PAS DB      A.CR,A.LF,A.LF
        DB      '	 Pass =',11Q,'   '
        DB      0

        DB      0,0,0,0,0,0,0   ; UNUSED BYTES

        ERRMI   1660Q-$
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
        ERRNZ   $-1660Q

RCK     EQU     $
        XRA     A
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

RCC     EQU     $

RCC1    IN      SC.ACE+UR.LSR   ; INPUT ACE LINE STATUS REGISTER
        ANI     UC.DR           ; SEE IF THERE IS A DATA READY
        CPU     Z80
        JR      Z,RCC1
        CPU     8080

RCC2    IN      SC.ACE+UR.RBR   ; ELSE, INPUT CHARACTER
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
        ANI     UC.THE          ; SEE IF TRANSMITTER HOLDING REGISTER IS EMPTY
        CPU     Z80
        JR      Z,WCC1
        CPU     8080

        POP     PSW             ; GET CHARACTER
        OUT     SC.ACE+UR.THR   ; OUTPUT TO CONSOLE
        RET

;       THE FOLLOWING IS ONLY A PORTION OF THE DYNAMIC RAM TEST!!
;
DY9.5   XCHG                    ; SAVE ERROR ADDRESS
        LXI     H,MSG.EQ        ; OUTPUT ' = '

        CPU     Z80
        LD      IX,DY9.8        ; RETURN ADDRESS
        CPU     8080

        JMP     DYMSG           ; OUTPUT STRING

DY9.8   LDAX    D               ; OUTPUT RAM CONTENTS

        CPU     Z80
        LD      IX,DYMEM10      ; RETURN ADDRESS
        CPU     8080

        JMP     DYBYT

;;      VIEW3. - CONTINUATION OF *VIEW*
;
;       SEE IF END OF BYTES
;

VIEW3.  INX     H               ; BUMP POINTER
        CALL    CHKRAD          ; GET RADIX
        MVI     A,11110000B     ; ASSUME HEX
        CPU     Z80
        JR      NZ,VIEW3.A      ; IF IT WAS HEX
        CPU     8080
        MVI     A,11111000B
VIEW3.A ANA     L               ; (A) = MASKED ADDR LSB
        CMP     L               ; SAME?
        RET                     ; LET CALLER DECIDE

;;      VIEW9 = DP THE ASCII
;

VIEW9   LHLD    BLKICW          ; RESTORE REGISTERS
        JMP     VIEW5

        DB      0,0,0,0,0,0     ; UNUSED BYTES

;       IO ROUTINES TO BE COPIED INTO AND USED IN RAM.
;
;       MUST CONTINUE TO 3777A FOR PROPER COPY.
;       THE TABLE MUST ALSO BE BACKWARDS TO THE FINAL RAM

        ERRMI   2000Q-7-$
        ORG     2000Q-7

PRSROM  EQU     $
        DB      1               ; REFIND
        DB      0               ; CTLFLG
        DB      0               ; MFLAG
        DB      0               ; DSPMOD
        DB      0               ; DSPROT
        DB      10              ; REGI
        DB      MI.RET

        ERRNZ   $-2000Q

;       INIT0X - EXTENSION OF INIT0 TO SUPPORT H88

INIT0X  MVI     A,H88B.CK       ; ENABLE CLOCK
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
        JMP     INT0X0          ; DO OTHER STUFF FIRST
INIT0X1 DCR     C
        CPU     Z80
        JR      NZ,INIT0X1

        DJNZ    INIT0X1
        CPU     8080

;       INPUT SWITCH TO SEE IF TO BEGIN OPERATION OR MEMORY TEST
;
        IN      H88.SW          ; GET SWITCHES
        ANI     H88S.M          ; MASK FOR MEMORY TEST ONLY
        JZ      MEMORY.         ; IF TO PERFORM MEMORY TESTS

;       REPLACE WHAT WAS ORIGINALLY AT THE JUMP WHICH GOT US HERE
;
        LXI     D,PRSROM        ; (DE) = ROM COPY OF PRS CODE
        XRA     A
        STA     AUTOB           ; INITIAL AUTO BOOT FLAG
        STA     DATA            ; INITIAL 362Q PORT DATA SAVE BYTE
        JMP     INIT0.0         ; RETURN TO ORIGINAL CODE

;       BRTAB - BAUD RATE DIVISOR TABLE
;
BRTAB   EQU     $

BR96    DB      0,12            ;   9600 BAUD
BR19.2  DB      0,6             ; 19,200 BAUD
;BR38.4 DB      0,3             ; 38,400 BAUD
;BR56.0 DB      0,2             ; 56,000 BAUD

;       SET     */256
;       ERRNZ   ERTAB/256-.     ; TABLE MUST BE IN ONE PAGE

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
        CPU     Z80
        JR      Z,NMI1          ; IF PORT WAS 360Q
        CPU     8080

;       PORT REFERENCED WAS 361Q, 372Q, OR 373Q
;
        CPI     361Q            ; MAKE SURE PORT IS LEGAL
        CPU     Z80
        JR      Z,NMI0.5        ; IF LEGAL
        CPU     8080

        CPI     372Q
        CPU     Z80
        JR      Z,NMI0.5
        CPU     8080

        CPI     373Q
        CPU     Z80
        JR      NZ,NMI2.5       ; IF NONE OF THE ABOVE, EXIT
        CPU     8080

NMI0.5  DCX     H               ; POINT TO IN/OUT INSTRUCTION
        MOV     A,M             ; SEE IF INPUT OR OUTPUT
        CPI     MI.OUT
        CPU     Z80
        JR      Z,NMI2.5        ; IF OUTPUT, JUST EXIT
        CPU     8080

        CPI     MI.IN
        CPU     Z80
        JR      NZ,NMI2.5       ; IF NOT INPUT EITHER, ILLEGAL SO EXIT
        CPU     8080

        POP     PSW             ; RESTORE FLAGS
        MVI     A,0             ; ELSE, RETURN LIKE AN EMPTY BUSS
        CPU     Z80
        JR      NMI3            ; EXIT
        CPU     8080

NMI1    DCX     H               ; POINT TO IN/OUT INSTRUCTION
        MOV     A,M             ; GET I/O INSTRUCTION
        CPI     MI.IN           ; INPUT?
        CPU     Z80
        JR      NZ,NMI1.5       ; IF NOT 'IN'
        CPU     8080

        POP     PSW             ; RESTORE FLAGS
        MVI     A,11111111B     ; SHOW 'NO KEYS PRESSED'
        CPU     Z80
        JR      NMI3            ; EXIT
        CPU     8080

NMI1.5  CPI     MI.OUT          ; MAKE SURE INSTRUCTION IF AN 'OUT'
        CPU     Z80
        JR      NZ,NMI2.5       ; IF NOT
        CPU     8080

NMI2    MOV     A,B             ; GET OUTPUT DATA AGAIN
        ANI     CB.CLI+CB.SSI   ; MOVE CLOCK INFO TO BIT 1
        RRC
        RRC
        RRC
        RRC
        RRC
        CPU     Z80
        JR      C,NMI2.2
        CPU     8080
        INR     A
NMI2.2  LXI     H,DATA          ; OR WITH THE BYTE IN RAM
        ORA     M               ; BEFORE OUTPUT IT
        OUT     H88.CTL         ; SET IN HARDWARE
        ANI     11111100B
        MOV     M,A

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

        ERRMI   2256-$
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
        DI
        MVI     A,10
        CALL    LRA.            ; GET LOCATION OF USER PC
        LXI     D,NBOOT         ; SET ITS VALUE TO THE NORMAL BOOT ROUTINE
BOOTX   MOV     M,E
        INX     H
        MOV     M,D
        EI
        JMP     GO.             ; DO IT


;       TMOUT   - BOOT CODE TIME OUT ROUTINE
;
;       TMOUT IS ENTERED FROM TIMER INTERRUPT EVER 100MS. AND IT WILL
;       EXIT:   IF BOOT SUCCESS THEN TIMER OFF.
;               IF 15 SECONDS TIME OUT AND BOOT IS NOT SUCCESS YES
;                  THEN ABORT BOOT Z47 & TO MONITOR LOOP
;               IF < 15S & 3.5S THEN RE-BOOT
;
;       NOTE: Because the H37 and H67 run with interrupts disabled
;             during portions of the code, they handle their own
;             time outs.
;
;       ENTRY:  (TMFG)  = 1 IF THE TIME OUT IS FOR Z47
;                       = 0 IF THE TIME OUT IS FOR H17
;
;       EXIT:   NONE
;
;       USE:    ALL (WHEN RETURN, ALL REGISTERS ARE RESTORED)

TMOUT   EQU     $
        IN      SC.ACE+UR.LSR   ; INPUT ACE LINE STATUS REGISTER
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
        CPU     Z80
        EX      AF,AF'          ; SAVE Z FLAG
        CPU     8080
        LDA     TICCNT          ; GET TIC
        ANA     A               ; SET ZERO FLAG
        CPU     Z80
        JR      NZ,TMOUT2       ; NOT IN 0.5 SECOND
        CPU     8080
        INX     H               ; SET TO MYCNT
;       ERRNZ   MYCNT-TMFG-1    ; MYCNT MUST FOLLOW TMFG
        INR     M               ; INCREASE THE COUNT FOR 0.5 SECOND
        MOV     A,M
        CPI     30              ; CHECK IF MORE THAN 15 SECONDS
        JNC     NODEV           ; NO DEVICE?
TMOUT1  SBI     7               ; IS IT 3.5 SECONDS?
        CPU     Z80
        JR      C,TMOUT2        ; IF NOT, WAIT
        JR      NZ,TMOUT1       ; CHECK MORE
        EX      AF,AF'
        CPU     8080
        JNZ     RETRY           ; IF IT IS Z47, THEN RE-BOOT
        CPU     Z80
        JR      TMOUT3          ; IT IS H-17, CONTINUE IT CLOCK ROUTINE
TMOUT2  EX      AF,AF'          ; CHECK IT IS Z47 OR H17
        CPU     8080
        RNZ                     ; Z47, THEN RETURN
TMOUT3  JMP     CLOCK17         ; CONTINUE H17 CLOCK ROUTINE

        ERRMI   2370Q-4
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
        CALL    SUBM10
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

IROCO   CALL    RCC             ; INPUT CHARACTER
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
        MVI     E,0             ; CLEAR PSEUDO FLAGS
        PUSH    H               ; SAVE ADDRESS WHERE INPUT IS TO BE PLACED
        LXI     H,0             ; SET NEW VALUE TO ZERO
IOA2    CNC     RCC             ; IF CARRY SET, FIRST CHARACTER IS IN ACC
        CALL    IOC.            ; CHECK VALIDITY
        JC      IOA3            ; IF < OCTAL

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
        ANA     A               ; CLEAR CARRY
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

IOC0    CALL    RCC             ; INPUT CHARACTER
IOC.    CPI     '0'
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

TOA0    MVI     A,A.CR          ; CRLF
        CALL    WCR.

TOA.    MOV     A,H             ; ADDRESS
        CALL    TOB0
        MOV     A,L
        CALL    TOB0

        MVI     A,' '           ; SPACE
        JMP     WCC

;       TOB - TYPE OCTAL BYTE
;
;       TOB OUTPUTS TO THE CONSOLE IN OCTAL, THE BYTE IN A
;
;       ENTRY   (A) = BYTE TO BE OUTPUT
;       EXIT    NONE
;       USES    A,F

TOB0    PUSH    B
        MVI     B,2             ; NUMBER OF CHARACTERS - 1
        MOV     C,A             ; SAVE ORIGINAL BYTE
        ANA     A               ; CLEAR CARRY
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
        POP     B
        JMP     WCC

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
        CPU     Z80
        JR      NZ,WCR          ; IF NOT A CR
        CPU     8080

WCR.    CALL    WCC             ; ELSE ECHO CR
        MVI     A,A.LF          ; LINE FEED
        JMP     WCC


;;      VIEW3 - *VIEW* CONTINUATION
;

VIEW3   JNZ     VIEW2           ; IF NOT END OF LINE
        CALL    VIEW9           ; END OF LINE, RESTORE ADDRESS
        MVI     A,A.CR
        JMP     VIEW3A.         ; DO ASCII STUFF

        DB      0,0,0,0,0,0,0,0 ; FILL UNUSED BYTES

        ERRMI   3023Q-$
        ORG     3023Q

;       DAT     - DATA BYTE OUTPUT TO Z-47
;
;       ENTRY:  (A) = BYTE TO OUTPUT
;
;       EXIT:   (A) = BYTE TO OUTPUT
;               (D) = S.DTR
;
;       USES:   AF, D

DAT     EQU     $
        MVI     D,S.DTR         ; SET MATCH CONDITION TO DATA TRANSFER
        CPU     Z80
        JR      COM1            ; REQUEST BIT
        CPU     8080
        ERRMI   3027Q-$
        ORG     3027Q

;       COM      - OUTPUT COMMAND BYTE TO Z-47
;
;       ENTRY:  (A) = COMMAND BYTE
;
;       EXIT:   (A) = COMMAND BYTE
;               (D) = S.DON
;
;       USES:   AF, D

COM     EQU     $
        MVI     D,S.DON         ; SET MATCH CONDITION TO DONE BIT
COM1    PUSH    PSW
WTDON1  CALL    IN.             ; READ CONTROLLER STATUS REGISTER
        ANA     D               ; GET MATCH BIT ONLY
        CPU     Z80
        JR      Z,WTDON1        ; IF NO MATCH, WAIT
        CPU     8080
        POP     PSW
        JMP     COM2            ; CONTINUE *COM* ROUTINE

        DB      0               ; FILL UNUSED BYTES

        ERRMI   3045Q-$
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

OUT.    EQU     $
        PUSH    B
        MOV     B,A             ; SAVE THE OUTPUT DATA
        LDA     PRIM            ; GET PORT ADDRESS
OUT.1   MOV     C,A             ; SET TO REG C
        MOV     A,B             ; GET OUTPUT BYTE DATA BACK
        CPU     Z80
        OUT     (C),A           ; OUTPUT BYTE
        CPU     8080
        POP     B
        RET

        DB      0,0             ; FILL UNUSED BYTES

        ERRMI   3100Q-$
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
        CPU     Z80
        JR      TYPMSG          ; OUTPUT IT
        CPU     8080

;       RDBLCK  - INPUT A BLOCK FROM Z-47
;
;       RDBLCK READS IN A BLOCK FROM THE DISK CONTROLLER
;
;       ENTRY:
;               HL = LOAD ADDRESS
;               C  = SIDE/UNIT/SECTOR
;
;       EXIT:   BLOCK IN READ IN MEMORY
;
;       USE:    ALL

RDBLCK  MVI     A,DD.REAB
        CALL    COM             ; SEND THE COMMAND
        XRA     A               ; FOR TRACK 0
        CALL    DAT             ; SEND IT TO DISK
        MOV     A,C             ; LOAD SIDE/UNIT/SECTOR
        CALL    DAT             ; SEND IT TO DISK

RD2     CALL    PIN             ; INPUT A BYTE FROM DISK
        JC      WDN             ; 'C' SET IF S.DON

        MOV     M,A
        INX     H 
        CPU     Z80
        JR      RD2             ; CONTINUE TRANSFER
        CPU     8080


;       OUT1.   - OUTPUT A BYTE TO PORT (PRIM+1)
;
;       ENTRY:  (A) = OUTPUT PORT
;
;       EXIT:   NONE
;
;       USE:    NONE

OUT1.   EQU     $
        PUSH    B
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

IN1.    EQU     $
        PUSH    B
        LDA     PRIM            ; GET PORT ADDRESS
        INR     A               ; SET TO (PRIM+1)
        ANA     A
        CPU     Z80
        JR      IN.1
        CPU     8080

        DB      0,0,0,0,0       ; FILL UNUSED BYTES

        ERRMI   3165Q-$
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

IN.     EQU     $
        PUSH    B
        LDA     PRIM            ; GET PORT ADDRESS
IN.1    MOV     C,A             ; SET ADDR. TO REG C.
        CPU     Z80
        IN      A,(C)
        CPU     8080
        POP     B
        RET

        ERRMI   3201Q-$
        ORG     3201Q
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

;       LABEL EQUIVALENCES
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
        MOV     A,H             ; FORM TWO'S COMPLEMENT OF TICK COUNTER
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
        DAD     D               ; (H,L) = OFFSET RESULT
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
        DB      '	Disk drive rotational speed test.',A.CR,A.LF,A.LF
        DB      '		Drive speed = '
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

DYMEM1  LXI     H,START
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

        CPU     Z80
        LD      IX,DY3.3        ; RETURN ADDRESS

        JR      DYMSG
        CPU     8080
DY3.7   INX     D               ; (D,E) = LAST BYTE OF RAM + 1

;       TEST MEMORY
;
        MVI     B,1             ; (B) = CONTENTS OF RAM AFTER SIZING
        LXI     H,MSG.PAS       ; OUTPUT PASS MESSAGE

        CPU     Z80
        LD      IX,DYMEM4       ; RETURN ADDRESS

        JR      DYMSG
        CPU     8080

DYMEM4  LXI     H,START         ; POINT BACK TO BEGINNING OF RAM
DYMEM5  MOV     A,M             ; READ CURRENT CONTENTS
        CMP     B               ; SEE IF CURRENT CONTENTS STILL REMAIN
        JNZ     DYMEM9          ; FAILURE, SEE IF AT END OF RAM

        INR     A
        MOV     M,A             ; INCREMENT RAM
        CMP     M               ; SEE IF WRITE WAS SUCCESSFUL
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

        JMP     DYMM5           ; HOW MANY TO BACK SPACE?

DYME5.5 EQU     $
        CPU     Z80
        LD      IY,DY5.53       ; RETURN ADDRESS
        CPU     8080

        JMP     DYASC

DY5.53  DCR     H
        CPU     Z80
        JR      NZ,DYME5.5
        CPU     8080
        INR     B               ; SHOW NEXT PASS VALUE
        MOV     A,B             ; VALUE TESTED

        CPU     Z80
        LD      IX,DYMEM6       ; RETURN ADDRESS
        CPU     8080

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

        CPU      Z80
        LD      IY,DYMSG.5      ; RETURN ADDRESS
        CPU      8080

        JMP     DYASC           ; OUTPUT ASCII

DYMSG.5 ORA     A               ; SEE IF NULL TO END STRING
        INX     H               ; POINT TO NEXT CHARACTER
        CPU     Z80
        JR      NZ,DYMSG        ; IF NOT DONE YET

        JP      (IX)            ; RETURN TO CALLER
        CPU     8080


;       MSG.RAM - RAM TEST MESSAGE
;

MSG.RAM DB      A.ESC,'E'
        DB      'Dynamic RAM test'
        DB      A.CR,A.LF,A.LF
        DB      '	 LWA = '
        DB      0

;       MSG.EQ - EQUALS MESSAGE
;

MSG.EQ  DB      ' = '
        DB      0

        DB      'GAC.'

;;      VIEW3A - *VIEW* CONTINUED
;

VIEW3A  CALL    VIEW8           ; GET BOUNDARIES
        XCHG
VIEW3A. CALL    VIEW12          ; PRINT CRLF AND ADDRESS
        JMP     VIEW1           ; AND START NEXT LINE

VIEW4   MOV     A,H
        CMP     B               ; Compare BC and DE
        RNZ
        MOV     A,L
        CMP     C
        RET

        DB      0               ; FILL UNUSED BYTES

;       ENTRY POINT FOR FLOPPY DISK ROTATIONAL SPEED TEST
;
        ERRMI   4000Q-6-$       ; MUST BE 6 BYTES BEFORE END
        ORG     4000Q-6

ESPEED  JMP     SPEED

;       ENTRY POINT FOR DYNAMIC MEMORY TEST
;
        ERRNZ   4000Q-3-$      ; MUST BE 3 BYTES BEFORE END

EDYMEM  JMP     MEMORY.

;;      Z47X - EXTENSION TO Z47 ROUTINE
;

Z47X    CALL    OUT.           ; SEND RESET COMMAND

        CALL    WDN            ; WAIT FOR HIM TO WAKE UP
        JC      NODEV          ; ERROR WAITING FOR DONE

Z47X.   CALL    RRDY
        JC      NODEV
        CALL    RRDY
        JC      NODEV

        LDA     AIO.UNI        ; (A)=UNIT NUMBER
        MOV     B,A
        XRA     A
        CALL    BITS           ; SET UNIT BIT MASK
        ANA     L
        CPU     Z80
        JR      NZ,Z47X.
        CPU     8080

        MVI     A,DD.RAS
        CALL    COM            ; READ AUX STAT
        MOV     A,C
        CALL    DAT
        CALL    PIN
        JC      NODEV          ; PREMATURE DONE

;       SET TRANSFER COUNT O 9 SECTORS

        MVI     A,DD.LSC
        CALL    COM            ; SEND 'LOAD COUNT'

        XRA     A
        CALL    DAT            ; SEND HIGH ORDER BYTE

        MVI     A,10
        CALL    DAT            ; SEND LOW ORDER BYTE

        CALL    WDN            ; WAIT FOR DONE, THEN EXIT
        JC      NODEV

        RET

;;      WDB - WAIT FOR DONE
;
;       WDN waits for the done bit to be set.
;
;       time-out is in effect at this point
;
;       ENTRY:  NONE
;
;       EXIT:   PSG     'C' SET IF ERROR
;                       'C' CLEAR IF DONE
;
;       USES:   PSW
;

WDN     DI
        PUSH    B              ; SAVE BC
        LXI     B,WDNA

WDN1    DCX     B
        MOV     A,B
        ORA     C              ; IF TIMED-OUT
        STC
        CPU     Z80
        JR      Z,WDN2
        CPU     8080

        CALL    IN.
        ANI     S.DON
        CPU     Z80
        JR      Z,WDN1         ; IF NOT DONE YET
        CPU     8080

        CALL    IN.            ; S.ERR VALID ONLY IF S.DONE SET
        ANI     S.ERR
        STC
        CPU     Z80
        JR      NZ,WDN2        ; IF ERROR BIT SET
        CPU     8080

        ANA     A              ; CLEAR CARRY

WDN2    POP     B
        EI
        RET                    ; ALL OK.

WDNA    EQU     32000          ; TIME OUT COUNTER

;;      RRDY - CHECK DEVICE READY
;
;       RRDY RETURNS THE DEVICE READY BITS IN
;       THE L REGISTER. BITS 'ON' INDICATE
;       UNIT NOT READY.
;

RRDY    MVI     A,DD.RRDY
        CALL    COM

        CALL    PIN

        MOV     L,A
        JMP     WDN

;;      COM2 - *COM* ROUTINE CONTINUATION
;
;       OUTPUT COMMAND TO 47 AND THEN DELAY
;

COM2    CALL    OUT1.           ; SEND COMMAND BYTE
        MVI     A,40Q
        ANA     A               ; CLEAR 'Z'

COM3    DCR     A
        JNZ     COM3            ; SHORT DELAY

        RET

;;      VIEW5 - *VIEW* CONTINUED
;
;       VIEW5 DOES THE ASCII PORTION OF THE *VIEW* ROUTINE
;

VIEW5   CALL    PCFA            ; POSITION CURSOR FOR ASCII
VIEW5A  MOV     A,M             ; GET A BYTE
        ANA     A               ; CHECK PARITY
        JM      VIEW7
        CPI     177Q
        JZ      VIEW5.          ; IF DELETE
        CPI     ' '             ; PRINTABLE?
        CPU     Z80
        JR      NC,VIEW6        ; YES
        CPU     8080
VIEW5.  PUSH    H
        LXI     H,VEW.NPC       ; NON-PRINTABLE CHARACTER
        CALL    TYPMSG
        POP     H
        CPU     Z80
        JR      VIEW6.
        CPU     8080
VIEW6   CALL    WCC             ; PRINT IT
VIEW6.  CALL    VIEW4
        RZ                      ; IF LAST BYTE DONE
        CALL    VIEW3.          ; CHECK FOR END
        CPU     Z80
        JR      NZ,VIEW5A       ; NO, DO MORE
        CPU     8080
        RET

VIEW7   ANI     177Q            ; STRIP PARITY
        PUSH    PSW             ; SAVE IT
        MVI     A,33Q
        CALL    WCC
        MVI     A,'p'
        CALL    WCC             ; GO TO REVERSE VIDEO
        POP     PSW
        CPI     177Q
        JZ      VIEW7A
        CPI     ' '
        CPU     Z80
        JR      NC,VIEW7.
        CPU     8080
VIEW7A  PUSH    H
        LXI     H,VEW.NPC
        CALL    TYPMSG
        POP     H
        CPU     Z80
        JR      VIEW7..
        CPU     8080
VIEW7.  CALL    WCC             ; PRINT IT
VIEW7.. MVI     A,33Q
        CALL    WCC
        MVI     A,'q'           ; EXIT REVERSE VIDEO
        CPU     Z80
        JR      VIEW6           ; AND FINISH UP
        CPU     8080

PCFA    PUSH    H
        CALL    CHKRAD
        LXI     H,PCF.MO        ; ASSUME OCTAL
        JZ      PCFAA
        LXI     H,PCF.MH        ; WAS HEX
PCFAA   CALL    TYPMSG
        POP     H
        MVI     A,1             ; Skip 1 space per letter


PCFA.   PUSH    H
        PUSH    B
        MOV     B,A             ; B=SKIP COUNT
        CALL    CHKRAD
        JZ      PCFA1

        MVI     A,11110000B     ; MASK FOR HEX
        JMP     PCFA2


PCFA1   MVI     A,11111000B     ; MASK FOR OCTAL

PCFA2   ANA     L               ; MASK LOW ORDER, RESULT IN A

PCFA3   CMP     L
        JZ      PCFA4           ; IF A=L, DONE
        DCR     L
        PUSH    PSW
        PUSH    B
PCFA3.  MVI     A,' '
        CALL    WCC
        DCR     B
        JNZ     PCFA3.

        POP     B
        POP     PSW
        JMP     PCFA3           ; PRINT (B) SPACES AND CHECK AGAIN

PCFA4   POP     B
        POP     H
        RET

PCF.MH  DB      33Q,'Y',1,54+31,0 ; Hex Version
PCF.MO  DB      33Q,'Y',1,40+31,0 ; Octal Version

VEW.NPC DB      33Q,'F^',33Q,'G',0 ; ESC,GRAPHICS,|,ESC,NO-GRAPHICS

;;      VIEW8 - GET BOUNDARIES
;

VIEW8   CALL    IROC            ; GET CHARACTER OR RETURN
        CPU     Z80
        JR      NZ,VIEW8A
        CPU     8080

        LHLD    VEWHLD          ; GET LAST ONE
        INX     H               ; START AT NEXT ONE
        XCHG
        LXI     B,0             ; SET LENGTH TO 0
        CPU     Z80
        JR      VIEW8B
        CPU     8080

VIEW8A  CALL    GETBND.         ; 'C' IS SET, FIRST CHARACTER IN A

VIEW8B  MOV     L,C
        MOV     H,B
        SHLD    VEWHLD          ; SAVE LAST

        MOV     A,B
        ORA     C               ; LAST = 0
        RNZ                     ; NO, OK
        LXI     H,200Q-1        ; ADD 177Q TO VALUE
        DAD     D               ; HL = DE + 177Q

        CALL    CHKRAD
        CPU     Z80
        JR      Z,VIEW8.
        CPU     8080

        PUSH    D
        LXI     D,200Q
        DAD     D               ; ADD IN ANOTHER
        POP     D

VIEW8.  SHLD    VEWHLD          ; UPDATE END ADDRESS
        MOV     C,L
        MOV     B,H
        RET

;;      VIEW12 - Print address and position cursor
;

VIEW12  EQU     $
        CALL    TOA
        CALL    CHKRAD
        JZ      VIEW3B
        MVI     A,3             ; NUMBER OF ASCII FOR BYTES
        JMP     VIEW3C
VIEW3B  MVI     A,4
VIEW3C  CALL    PCFA.           ; SKIP TO START IN SCREEN
        RET

;;      CKAUTO - CHECK FOR AUTO BOOT
;
;       CKAUTO IS ENTERED DURING THE MONITOR LOOP TO CHECK
;       IF THE AUTO BOOT SWITCH IS SET.
;
;       THIS ROUTINE WAS MOVED FROM UP FRONT TO MAKE ROOM
;

CKAUTO  IN      H88.SW
        ANI     H88S.AT         ; CHECK SWITCH
        CPU     Z80
        JR      Z,CHAT2         ; NOT AUTO BOOT
        CPU     8080
        LXI     H,AUTOB
        CMP     M               ; HAVE WE BEEN HERE BEFORE?
        JNZ     ATB             ; NO, DO AUTO BOOT

CHAT2   LXI     H,MSG.PR
        JMP     MTR.15          ; RETURN TO MONITOR LOOP

;;      DYMEM EXTENSION
;

DY9.3   XCHG
        MOV     A,H
        CPU     Z80
        LD      IX,DY9.4
        CPU     8080
        JMP     DYBYT

DY9.4   MOV     A,L
        CPU     Z80
        LD      IX,DY9.5
        CPU     8080
        JMP     DYBYT

;       ANOTHER EXTENSION!

DY3.3   MOV     A,D
        CPU     Z80
        LD      IX,DY3.5
        CPU     8080
        JMP     DYBYT

DY3.5   MOV     A,E
        CPU     Z80
        LD      IX,DY3.7
        CPU     8080
        JMP     DYBYT

;;      H37 - ENTRY POINT TO BOOT FROM H37
;

H37     XRA     A
        OUT     DK.INT          ; SET FLIP LATCH

        MVI     A,FDC.FI
        OUT     FD.CMD          ; SET NOT BUSY

        MVI     A,1
        CALL    DLY             ; DLY 2 MILLISECONDS

        IN      FD.STAT         ; CLEAR INTERRUPTS

        LXI     H,MYINT
        SHLD    UIVEC+9+1       ; SET INTERRUPT ROUTINE
        MVI     A,MI.JMP
        STA     UIVEC+9

        LDA     AIO.UNI
        ADI     4
        MOV     B,A
        XRA     A
        CALL    BITS            ; GET DEVICE CODE

        ORI     CON.MO+CON.EI+CON.MFM
        OUT     DK.CON
        MOV     B,A
        PUSH    B

        EI                      ; Insure Interrupts on

        MVI     A,150           ; 300MS ON DELAY
        CALL    DLY

        LXI     H,H371
        SHLD    BLKICW          ; SET RETURN ADDRESS
        MVI     A,FDC.RST+FDF.S30
        OUT     FD.CMD

        LXI     B,-1            ; ABOUT 5 SECONDS
        MVI     D,4             ;   DOUBLED.
H37.    DCX     B
        MOV     A,B
        ORA     C
        CPU     Z80
        JR      NZ,H37.         ; IF BC>0
        CPU     8080

        DCR     D
        CPU     Z80
        JR      NZ,H37.         ; IF D>0
        CPU     8080

        MVI     A,FDC.FI
        OUT     FD.CMD

        CPU     Z80
        JR      H373            ; TIMED OUT
        CPU     8080

H371    LXI     H,H371B
        SHLD    BLKICW          ; LOAD RETURN ADDRESS
        MVI     A,10            ; NUMBER OF TRACKS TO STEP
        OUT     FD.DAT          ; SET TRACK NUMBER TO 10
        MVI     A,FDC.SEK+FDF.S30
        OUT     FD.CMD
        JMP     $               ; Wait for interrupt

;       Return here after doing seek

H371B   LXI     H,H371C
        SHLD    BLKICW
        MVI     A,FDC.RST+FDF.S30
        OUT     FD.CMD
        JMP     $

;       Here after final RESTORE

H371C   ANI     FDS.TK0         ; Be sure track zero switch on
        CPU     Z80
        JR      Z,H373          ; If not there
        CPU     8080

;       Over track zero, Wait for head to settle

        LXI     B,3200          ; 40 mS DELAY
H371.   DCX     B
        MOV     A,B
        ORA     C
        CPU     Z80
        JR      NZ,H371.        ; ALLOW HEAD SETTLE TIME
        CPU     8080

        POP     B
        MOV     A,B             ; (A) = Device Control Bits
        ORI     CON.DRQ         ; Turn on DRQ Interrupt
        MOV     B,A
        PUSH    B               ; save device control bits
        OUT     DK.CON          ; READY FOR TRANSFERS

        CALL    READT           ; Read a track
        POP     B
        PUSH    PSW             ; SAVE RETURN STATUS
        MOV     A,B
        ANI     377Q-CON.MFM    ; OFF DBL DENSITY
        MOV     B,A
        POP     PSW
        CPU     Z80
        JR      NZ,H372         ;    IF READ FAILURE
        CPU     8080

        LXI     H,-USERFWA
        DAD     D               ;    HL = Bytes Read
        MOV     A,H
        CPI     (2048+256)/256  ; See if 2.25K
        JNC     EUC             ; If got it all

H372    MOV     A,B
        OUT     DK.CON

        CALL    READT           ; TRY SINGLE DENSITY
        CPU     Z80
        JR      NZ,H373         ;    IF FAILURE
        CPU     8080

        LXI     H,-USERFWA
        DAD     D               ;    HL = Bytes Read
        MOV     A,H
        CPI     (2048+256)/256  ; See if 2.25K
        JNC     EUC             ; More than 2.25K read, is ok        

H373    XRA     A
        OUT     DK.CON          ; TURN OFF DEVICE
        JMP     NODEV

READT   MVI     A,CON.ST
        OUT     DK.INT
        OUT     FD.SEC
;       ERRNZ   CON.ST-1
        XRA     A
        OUT     DK.INT
        ERRNZ   CON.CD

        LXI     H,READT2
        SHLD    BLKICW          ; SET RETURN ADDRESS
        LXI     H,READT1
        LXI     D,USERFWA
        MVI     A,FDC.RDS+FDF.DLF+FDF.MRF+FDF.SLF
        OUT     FD.CMD

READT1  HLT
        IN      FD.DAT          ; *TIME DEPENDENT*
        STAX    D
        INX     D
        PCHL

READT2  PUSH    PSW
        MVI     A,CON.MO
        OUT     DK.CON
        POP     PSW
        ANI     FDS.NRD+FDS.LDT+FDS.CRC+FDS.RTE
        RET

;;      MYINT - H37 Interrupt Routine
;
;       This routine is entered when a level 4 interrupt
;       is received from the H37 Hardware.
;
;       Control is passed to the address in BLKICW
;
;       ENTRY:  NONE    (From the disk routine via level 4 interrupt)
;
;       EXIT:   PSW     =           Status byte from controller
;               HL      =           Return address to routine

MYINT   IN      FD.STAT
        POP     H
        LHLD    BLKICW
        EI
        PCHL
        INCLUDE bits.asm

;;      H67 - BOOT H67
;
;       The section of this code most likely to 'HANG' because
;       of no controller is timed using the BC register pair
;       for approximately 3 seconds.
;

H67     MVI     A,BC.RST
        CALL    OUT1.           ; RESET THE CONTROLLER

        MVI     A,4
        CALL    DLY

        LXI     H,AIO.DIR       ; SCRATCH AREA FOR CDB
        MVI     M,D.TDR         ; TEST FOR READY
        MVI     C,5
H671    INX     H
        MVI     M,0             ; FILL CDB WITH 0
        DCR     C
        CPU     Z80
        JR      NZ,H671
        CPU     8080

        CALL    H67UNI          ; GET UNIT NUMBER
        STA     AIO.DIR+1       ; SET THE LUN

H671.   CALL    GETCON          ; CHECK READY
        CPU     Z80
        JR      NC,H672         ; IF DRIVE IS READY
        CPU     8080

        JZ      NODEV           ; IF WAS TIME-OUT PROBLEM

        MVI     A,377Q
        CALL    DLY             ; WAIT ABOUT 1/2 SECOND
        CPU     Z80
        JR      H671.
        CPU     8080

H672    LXI     H,AIO.DIR
        MVI     M,D.REC         ; RECAL THE DRIVE

        CALL    GETCON          ; DO THE RECAL
        JC      NODEV           ; ERROR IN RECAL

;       Now cause the drive to step out 10 tracks

        LDA     AIO.UNI         ; Only for the hard disk
        ANA     A
        JNZ     H673            ; If unit = 1, is 8" floppy

        LXI     H,AIO.DIR
        MVI     M,D.SEK
        INX     H               ; HL over logical address 0
        INX     H
        MVI     M,7             ; Seek block (7*256)

        CALL    GETCON          ; Do the seek
        JC      NODEV           ; If error during Seek

        LXI     H,AIO.DIR
        MVI     M,D.REC
        INX     H
        INX     H
        MVI     M,0             ; Do another Recal
        CALL    GETCON
        JC      NODEV

H673    LXI     H,AIO.DIR       ; SET UP READ COMMAND
        MVI     M,D.REA
        INX     H               ; HL = LUN
        INX     H
        INX     H
        INX     H
        MVI     M,10            ; SET 10 SECTOR READ
        INX     H
        MVI     M,080H          ; CONTROL BYTE

        CALL    H67UNI
        STA     AIO.DIR+1       ; SET LUN TO READ

        CALL    GETCON
        JC      NODEV           ; IF READ ERROR
        JMP     EUC             ; ENTER USER CODE

H67UNI  LDA     AIO.UNI         ; (A)=UNIT NUMBER
        RRC
        RRC
        RRC                     ; MOVE IT INTO PLACE
        ANI     ST.LUN
        RET

GETCON  DI                      ; GET CONTROLLER ATTENTION

        LXI     B,65535         ; ABOUT 5 SECONDS FOR RESPONSE
        MVI     D,2             ; 3 BYTE COUNTER (D,B,C)

GTCON   CALL    IN1.            ; GET BUSS STATUS
        ANI     BS.BSY
        CPU     Z80
        JR      Z,GTCON1        ; WAIT FOR BUSY TO LEAVE
        CPU     8080

        DCX     B               ; COUNT DOWN
        MOV     A,B
        ORA     C
        CPU     Z80
        JR      NZ,GTCON        ; NO TIMEOUT YET
        CPU     8080

        DCR     D
        CPU     Z80
        JR      NZ,GTCON        ; DEC 3RD BYTE
        CPU     8080

        STC                     ; INDICATE ERROR
        RET

GTCON1  MVI     A,BC.SEL
        CALL    OUT1.           ; OUTPUT TO (PRIM)

CBUSY   CALL    IN1.
        ANI     BS.BSY
        CPU     Z80
        JR      NZ,CBUSY1       ;    WAIT FOR CONTROLLER
        CPU     8080

        DCX     B               ; CONTINUE COUNTING
        MOV     A,B
        ORA     C
        CPU     Z80
        JR      NZ,CBUSY
        CPU     8080
        STC
        RET                     ; TIMED OUT

CBUSY1  MVI     A,BC.EDT
        CALL    OUT1.
;
;       HAVE CONTROLLER, SEND HIM COMMAND
;
OUTCOM  LXI     H,AIO.DIR       ; FWA OF COMMAND BUFFER

COMREQ  CALL    IN1.
        MOV     C,A
        ANA     A
        JP      COMREQ
        ERRNZ   200Q-BS.REQ     ; WAIT FOR REQUEST BIT

        ANI     BS.COM
        CPU     Z80
        JR      Z,TFDATA        ;   COMMAND DONE, SEND DATA
        CPU     8080

        MOV     A,C             ; (A)=BUSS STATUS BYTE
        ANI     BS.DTD          ; CHECK DIRECTION
        CPU     Z80
        JR      Z,GETST         ;   IF DONE, GET STATUS
        CPU     8080

        MOV     A,M             ; (A)=COMMAND BYTE
        CALL    OUT.            ; SEND OUT DATA
        INX     H               ; BUMP POINTER
        CPU     Z80
        JR      COMREQ          ; CONTINUE SENDING BYTES
        CPU     8080
;
;       GET STATUS - COMPLETION BYTE SHOULD BE ZEROS AND
;       STATUS BYTE SHOULD BE ZERO IN 2 LS BITS
;
GETST   CALL    IN1.
        ANI     BS.REQ+BS.DTD+BS.COM
        CPI     BS.REQ+BS.COM
        CPU     Z80 
        JR      NZ,GETST        ; WAIT FOR CONTROLLER
        CPU     8080

        CALL    IN.             ;                            /2.1b/
        MOV     C,A             ; (C)=STATUS BYTE
        STA     AIO.DIR+6

GETCPT  CALL    IN1.
        MOV     B,A
        STA     AIO.DIR+7

        ANI     BS.REQ+BS.DTD+BS.MTY+BS.COM
        CPI     BS.REQ+BS.MTY+BS.COM
        CPU     Z80
        JR      NZ,GETCPT       ;    WAIT FOR MESSAGE
        CPU     8080
        STA     AIO.DIR+8       ; SAVE BYTES FOR DEBUG

        EI

        CALL    IN.             ; (A)=COMPLETION BYTE
        ORA     A               ; CHECK COMPLETION
        STC
        RNZ                     ; SHOULD BE ZERO

        MOV     A,C
        ANI     00000011B       ; CHECK FOR ERRORS
        STC
        RNZ                     ; IF BIT IS SET

        MOV     A,B
        ANI     00000010B
        STC
        RNZ                     ; IF INTERFACE ERROR

        XRA     A               ; CLEAR CARRY
        RET

TFDATA  LXI     H,USERFWA       ; HL = LOAD ADDRESS

TFREQ   CALL    IN1.
        MOV     C,A
        ANI     BS.REQ
        CPU     Z80
        JR      Z,TFREQ         ;   WAIT FOR REQUEST
        CPU     8080

        MOV     A,C
        ANI     BS.COM
        CPU     Z80
        JR      NZ,GETST        ;      IF DONE, CHECK STATUS
        CPU     8080

        CALL    IN.             ;        GET DATA BYTE
        MOV     M,A
        INX     H
        CPU     Z80
        JR      TFREQ           ; CONTINUE UNTIL DONE
        CPU     8080

;;      FEDEV - FUTURE EXPANSION DEVICE
;
;       CURRENTLY, FEDEV JUST PRINTS "UNKNOWN DEVICE"
;

FEDEV   LXI     H,MSG.FE
        CALL    TYPMSG
        JMP     NODEV1          ; ENTER COMMON RECOVERY CODE

MSG.FE  DB      '?Unkown Device',0

;;      DYMEM10 - DYNAMIC RAM TEST CONTINUED
;

DYMEM10 MVI     A,A.BEL
        CPU     Z80
        LD      IY,DY10.5
        CPU     8080

        JMP     DYASC

;;      CCL - CHECK COMMAND LINE
;
;       CCL CHECKS TO SEE IF THE USER WISHES TO PASS A COMMAND
;       TO THE BOOT ROUTINE. IF THE USER SIMPLY TYPES A CARRIAGE
;       RETURN, THEN NO COMMAND LINE IS PRESENT AND (S) = 42.200
;       OTHERWISE THE COMMAND LINE IS PUSHED ONTO THE STACK A LA HDOS
;       AND THE BOOT ROUTINES CAN DO WITH IT AS THEY SEE FIT.
;
;       ENTRY:  NONE
;
;       EXIT:   (SP)    =       42.200
;                               NO COMMAND LINE
;               (S)     <>      42.200
;                               COMMAND ON STACK TERMINATED WITH 000Q
;
;       USES:   SP
;

CCL     STA     START           ; SAVE UNIT NUMBER
        SHLD    IOWRK           ; SAVE DEVICE ADDRESS
        LXI     SP,21200Q       ; SET STACK

        LXI     H,AIO.DIR
        MVI     C,PRIM-AIO.DIR-1 ; (C) = MAXIMUM ALLOWABLE LENGTH

;       GET 1ST CHARACTER

CCL1    CALL    RCC             ; READ KEYBOARD
        CPI     A.CR            ; IS HE DONE?
        CPU     Z80
        JR      Z,CCL3
        CPU     8080
        CPI     ':'             ; COMMAND LINE FOLLOWS
        CPU     Z80
        JR      Z,CCL4
        CPU     8080
        CPI     ' '             ; ALLOW A SPACE
        CPU     Z80
        JR      Z,CCL2
        CPU     8080
        MVI     A,A.BEL
CCL2    CALL    WCC             ; ECHO CHARACTER
        CPU     Z80
        JR      CCL1
        CPU     8080

;       JUST A CARRIAGE RETURN, NO COMMAND

CCL3    CALL    WCR.            ; ECHO CRLF
CCL3.   LHLD    IOWRK
        LDA     START           ; RESTORE REGISTERS
        JMP     BOOT6           ; RETURN TO CALLED

;       HAD ':', COMMAND LINE FOLLOWS

CCL4    CALL    WCC             ; ECHO THE CHARACTER
CCL5    CALL    RCC             ; GET NEXT
        CPI     A.CR
        CPU     Z80
        JR      Z,CCL6          ;   IF END OF LINE
        CPU     8080
        MOV     M,A             ; SAVE CHARACTER
        INX     H
        DCR     C
        CPU     Z80
        JR      NZ,CCL4         ;   IF NOT TOO MANY
        CPU     8080
        INR     C               ; RESET COUNTER
        DCX     H               ; IGNORE IT
        MVI     A,A.BEL         ; BEEP
        CPU     Z80
        JR      CCL4
        CPU     8080

;       END OF COMMAND LINE

CCL6    CALL    WCR.
        MVI     M,0             ; NUL TERMINATOR
        XCHG                    ; (DE)=LWA OF COMMAND
        LXI     H,0
        DAD     SP              ; HL = STACK
        DCX     H               ; HL = LWA OF COMMAND LINE (NULL BYTE)

;       MOVE COMMAND INTO STACK AREA

        DI                      ; NO CLOCK INTERRUPTS

CCL7    LDAX    D               ; DE = COMMAND BYTE
        MOV     M,A             ; MOVE IT IN
        DCX     H
        DCX     D               ; BUMP POINTERS
        SHLD    BLKICW          ; SAVE FOR A SECOND
        LXI     H,AIO.DIR-1     ; AM I DONE?
        MOV     A,H
        CMP     D
        CPU     Z80
        JR      NZ,CCL8         ;   NO
        CPU     8080
        MOV     A,L
        CMP     E
        CPU     Z80
        JR      Z,CCL9          ;  YES, FINISH UP
        CPU     8080

CCL8    LHLD    BLKICW
        CPU     Z80
        JR      CCL7
        CPU     8080

;       FINISHED WITH COMMAND LINE, (BLKICW)=FWA-1

CCL9    LHLD     BLKICW
        INX      H
        SPHL
        CPU     Z80
        JR       CCL3.          ; AND GO BACK
        CPU     8080

;;      BSMSG - BOOT SECONDARY MESSAGE
;

BSMSG   DB      ' SD',0


;;      ERRMSG - GENERAL ERROR MESSAGE

ERRMSG  DB     '?Boot Error',0

;;      MSG.PR - Prompt Message
;
MSG.PR  DB      A.CR,A.LF,'  H: ',0

;;      RADIX - ASSIGN DEFAULT RADIX
;
;       RADIX SETS THE SYSTEM RADIX TO OCTAL OR HEX
;

RADIX   LXI     H,MSG.RAD
        CALL    TYPMSG          ; COMPLETE NAME

RADIX1  CALL    RCC             ; READ CHARACTER
        CALL    MCU             ; MAP TO UPPER
        CPI     'O'
        CPU     Z80
        JR      Z,RADIX2
        CPU     8080
        CPI     'H'
        CPU     Z80
        JR      Z,RADIX3
        CPU     8080
        CPI     A.CR
        CPU     Z80
        JR      Z,RADIX4
        CPU     8080
        MVI     A,A.BEL
        CALL    WCC
        CPU     Z80
        JR      RADIX1
        CPU     8080

;       SET OCTAL RADIX

RADIX2  LXI     H,RAD.OCT
        CALL    TYPMSG
        XRA     A
        STA     RADFLG          ; SET FLAG
        RET

;       SET HEX RADIX

RADIX3  LXI H,RAD.HEX
        CALL    TYPMSG
        MVI     A,1
        STA     RADFLG
        RET

;       SHOW CURRENT SETTING

RADIX4  LXI     H,RAD.OCT
        CALL    CHKRAD          ; ASSUME OCTAL
        CPU     Z80
        JR      Z,RADIX5        ; WAS OCTAL
        CPU     8080
        LXI     H,RAD.HEX

RADIX5  MVI     A,A.CR
        CALL    WCR.            ; PRINT CRLF
        JMP     TYPMSG          ; TYPE NAME

;       MESSAGES

MSG.RAD DB      'adix ',0
RAD.OCT DB      'Octal',0
RAD.HEX DB      'Hexadecimal',0

;;      INPUT - PORT INPUT
;
;       INPUT INPUTS THE VALUE FROM THE SPECIFIED
;       PORT NUMBER. THIS VALUE IS THEN PRINTED.
;

INPUT   LXI     H,MSG.INP       ; FINISH COMMAND
        CALL    TYPMSG

;       GET DESIRED PORT NUMBER

        LXI     H,PRIM
        ANA     A               ; CLEAR CARRY
        CALL    IOB             ; GET PORT

;       READ DATA FROM THAT PORT

        CALL    IN.             ; GET DATA AT (PRIM)

;       NOW PRINT RESULT

        PUSH    PSW
        MVI     A,A.CR
        CALL    WCR.            ; PRINT CRLF
        POP     PSW
        JMP     TOB             ; TYPE THE BYTE

;       OUTPUT - PORT OUTPUT
;
;       OUTPUT SENDS DATA OUT THE DESIRED PORT
;       IN KEEPING WITH THE TAPE LOAD/DUMP ROUTINES, THE
;       PORT NUMBER IS SPECIFIED FIRST, FOLLOWED BY A HYPHEN
;       AND FOLLOWED BY DATA:
;
;       OUTPUT AAA,DDD<CR>
;

OUTPUT  LXI     H,MSG.OUT
        CALL    TYPMSG

        LXI     H,IOWRK+1       ; STORE INFO IN IOWRK
        MVI     D,','           ; TERMINATE PORT BY HYPHEN
        ANA     A               ; CLEAR CARRY

        CALL    IOA             ; INPUT ADDRESS

        LDA     IOWRK           ; (A)=PORT NUMBER
        STA     PRIM            ; SAVE IT

        ANA     A
        LXI     H,IOWRK         ; GET DATA
        CALL    IOB             ; GET BYTE AND <CR>
        LDA     IOWRK           ; GET DATA IN (A)

        JMP     OUT.            ; OUT (PRIM) WITH (A)

;;      MSG.XXX - INPUT/OUTPUT MESSAGES
;

MSG.INP DB      'n ',0
MSG.OUT DB      'ut ',0
MSG.ERR DB      A.CR,A.LF,A.LF,'Error @ ',0

;;      SUBM10 - SUBSTITUTE PREFIX
;

SUBM10  CALL    CHKRAD
        CPU     Z80
        JR      NZ,SUBM11
        CPU     8080
        ANI     00000111B       ; GET BINARY VALUE
        MOV     E,A             ; SAVE PARTIAL
        MOV     A,M             ; GET CURRENT
        RLC                     ; MAKE ROOM FOR NEW CHARACTER
        RLC
        RLC
        ANI     11111000B       ; TOSS PREVIOUS LSB
SUBM10. ORA     E               ; ADD NEW
        MOV     M,A             ; SAVE NEW TOTAL
        RET
SUBM11  CALL    CHC             ; CONVERT IT TO HEX
        ANI     00001111B
        MOV     E,A
        MOV     A,M
        RLC
        RLC
        RLC
        RLC
        ANI     11110000B
        CPU     Z80
        JR      SUBM10.
        CPU     8080

;;      PREFIXES
;
;       THESE ROUTINES ARE PREFIXES TO THE IOA, IOB,
;       TOA, AND TOB ROUTINES. THESE PREFIXES DETERMINE
;       THE PROPER BASES TO USE, AND TRANSFER CONTROL
;       TO THE NEEDED ROUTINES
;

IROC    CALL    CHKRAD
        JZ      IROCO
        JMP     IROCH


IOA     PUSH    PSW
        CALL    CHKRAD          ; CHECK RADIX
        JNZ     IHA
        POP     PSW             ; SAVE CARRY FLAG
        JMP     IOA0

IOB     PUSH    PSW
        CALL    CHKRAD
        JNZ     IHB
        POP     PSW
        JMP     IOB0

IOC     PUSH    PSW
        CALL    CHKRAD
        JNZ     IHC
        POP     PSW
        JMP     IOC0

TOA     PUSH    PSW
        CALL    CHKRAD
        JNZ     THA
        POP     PSW
        JMP     TOA0

TOB     PUSH    PSW
        CALL    CHKRAD
        JNZ     THB
        POP     PSW
        JMP     TOB0

;       CHECK CURRENT RADIX

CHKRAD  PUSH    B
        MOV     B,A
        LDA     RADFLG
        ANA     A
        MOV     A,B
        POP     B
        RET

;;      HEX ROUTINES
;
;       THESE ROUTINES ARE THE HEX EQUIVALENT OF THE
;       OCTAL ROUTINES PREFIXES ABOVE
;
;       NOTE: THESE ROUTINES ARE ENTERED WITH PSW ON THE STACK
;

IHB     MVI     M,0             ; CLEAR RESULT
        POP     PSW
IHB1    CNC     RCC

;       CHECK FOR VALIDITY

        CALL    CCH             ; CHECK CHARACTER FOR VALID HEX
        CPU     Z80
        JR      NC,IHB2
        CPU     8080
        CPI     A.CR            ; RETURN?
        RZ                      ; YES, DONE
        ANA     A               ; INSURE CARRY OFF
        MVI     A,A.BEL
        CALL    WCC
        CPU     Z80
        JR      IHB1
        CPU     8080

;       HAVE A VALID HEX CHARACTER

IHB2    CALL    WCC
        CALL    CHC             ; CONVERT HEX CHARACTER
        MOV     E,A
        MOV     A,M             ; GET VALUE SO FAR
        RLC
        RLC                     ; MOVE UP NIBBLE
        RLC
        RLC
        ANI     11110000B       ; THROW AWAY LAST
        ORA     E
        MOV     M,A             ; SET NEW NIBBLE
        CPU     Z80
        JR      IHB1
        CPU     8080

;;      CHECK FOR VALID HEX CHARACTER
;
;       CCH CHECKS (A) FOR HEX VALIDITY
;       'C' IS SET IF INVALID
;

CCH     CALL    MCU             ; MAP TO UPPER
CPI     CPI     '0'
        RC                      ; IF LESS THAN ZERO
        CPI     '9'+1
        CMC
        RNC                     ; BETWEEN 0 AND 9
        CPI     'A'
        RC                      ; LOWER CASE IS NOT VALID
        CPI     'F'+1
        CMC
        RET

;;      IHC - INPUT HEX CHARACTER
;

IHC     POP     PSW
        CALL    RCC             ; GET CHARACTER
        JMP     CCH             ; CHECK FOR VALID HEX

;;      MCU - MAP CASE TO UPPER
;

MCU     CPI     'a'
        RC                      ; LESS THAN 'A'
        CPI     'z'+1
        RNC
        ANI     01011111B
        RET

;;      CONVERT HEX TO BINARY
;
;       CHC CONVERTS THE ASCII CHARACTER IN (A) INTO
;       IT'S 4 BIT HEX EQUIVALENT
;

CHC     SUI     '0'
        CPI     9+1
        RC                      ; IF DONE
        SUI     7
        RET                     ; CONVERT A - F

;       INPUT HEX ADDRESS

IHA     POP     PSW
IHA.    PUSH    B
        MOV     B,D
        PUSH    H               ; B - DELIMITER
        LXI     H,0
IHA1    CNC     RCC
        CALL    CCH             ; CHECK FOR HEX
        CPU     Z80
        JR      C,IHA3          ;   IF NOT, CHECK DELIMITER
        CPU     8080
        CALL    WCC
        CALL    CHC             ; CONVERT IT
        DAD     H
        DAD     H
        DAD     H
        DAD     H               ; HL = HL + 16
        ADD     L
        MOV     L,A             ; MOVE IN NEW NIBBLE
        CPU     Z80
        JR      IHA1
        CPU     8080

IHA3    CMP     B
        CPU     Z80
        JR      Z,IHA4          ; IF VALID DELIMITER
        CPU     8080
        MVI     A,A.BEL
        CALL    WCC
        ANA     A
        CPU     Z80
        JR      IHA1
        CPU     8080

;       END OF INPUT

IHA4    CALL    WCC             ; PRINT DELIMITER
        XCHG
        POP     H
        MOV     M,D
        DCX     H
        MOV     M,E
        POP     B
        RET

;       IROC REPLACEMENT

IROCH   CALL    RCC
        CPI     A.CR
        RZ                      ; IF CARRIAGE RETURN

        CALL    CCH
        CMC
        RC                      ; IF VALID

        MVI     A,A.BEL
        CALL    WCC
        CPU     Z80
        JR      IROCH
        CPU     8080

;       TYPE BYTE REPLACEMENT

THB     POP     PSW
THB1    PUSH    PSW
        ANI     11110000B
        RRC
        RRC
        RRC
        RRC                     ; DO HIGH NIBBLE FIRST
        CALL    THB2
        POP     PSW
        ANI     00001111B

;       THB1 - TYPE NIBBLE

THB2    ADI     '0'
        CPI     '9'+1
        CPU     Z80
        JR      C,THB3
        CPU     8080
        ADI     7
THB3    JMP     WCC

;       THA - TYPE HEX ADDRESS

THA     POP     PSW
        MVI     A,A.CR
        CALL    WCR.

THA1    MOV     A,H
        CALL    THB1
        MOV     A,L
        CALL    THB1
        MVI     A,' '
        JMP     WCC

;;      MEMORY - MEMORY DIAGNOSTIC
;
;       MEMORY IS THE PREFACE TO THE MEMORY
;       DIAGNOSTIC UTILITY
;
MEMORY  LXI     H,MSG.MEM
        CALL    TYPMSG
MEMORY. LDA     RADFLG
        CPU     Z80
        EXX
        CPU     8080
        MOV     L,A
        CPU     Z80
        EXX
        CPU     8080
        JMP     DYMEM

MSG.MEM DB      'est Memory',A.CR,A.LF,0

;;      GETBND - GET BOUNDARIES
;
;       GETBND GETS THREE ADDRESS BOUNDARIES, RETURNING
;       THE FIRST IN HL, THE SECOND IN DE AND THE THIRD
;       IN BC.

GETBND  LXI     H,IOWRK+1
        MVI     D,','
        CALL    IOA             ; GET FIRST

        LHLD    IOWRK
GETBND. PUSH    H               ; ENTRY POINT FOR DE,BC ONLY

        LXI     H,IOWRK+1
        MVI     D,','
        CALL    IOA

        LHLD    IOWRK           ; SAVE SECOND
        PUSH    H
        JMP     GETBND1         ; CONTINUE ELSEWHERE

;       INT0X0 - EXTENSION TO INT0X
;
;       INT0X0 CLEANS UP SOME OF THE RAM CELLS
;

INT0X0  XRA     A
        STA     RADFLG
        LXI     H,-1
        SHLD    VEWHLD
        LXI     B,16000
        JMP     INIT0X1


;;      BOOT7 - EXTENSION TO BOOT ROUTINE
;
;       THIS ROUTINE HANDLES BOOTING FROM DEVICE
;       ZERO WITH COMMAND LINES
;

BOOT7   CPI     ' '
        CPU     Z80
        JR      Z,BOOT71        ; TYPES SPACE, MUST WANT COMMAND LINE
        CPU     8080
        CPI     ':'
        CPU     Z80
        JR      Z,BOOT72        ; TYPE :, HERE COMES COMMAND
        CPU     8080
        CPI     '0'
        RET                     ; OTHERWISE, MAYBE UNIT NUMBER
BOOT71  CALL    WCC
        XRA     A               ; ENTER CCL AS UNIT 0
        JMP     CCL

;       HE ALREADY STARTED THE COMMAND LINE, LET'S CATCH UP!

BOOT72  XRA     A
        STA     START           ; SAVE UNIT NUMBER
        SHLD    IOWRK           ; SAVE DEVICE ADDRESS
        LXI     SP,21200Q       ; SET UP STACK
        LXI     H,AIO.DIR
        MVI     C,PRIM-AIO.DIR-1
        MVI     A,':'           ; ECHO THE COLON
        JMP     CCL4            ; CONTINUE FROM HERE

;;      EUC - ENTER USER CODE
;
;       EUC ENTERS THE USER BOOT CODE, AFTER RE-VECTORING
;       THE CLOCK INTERRUPT REQUEST VECTORS
;
;       THE H17 RAM CONSTANTS ETC. ARE ALSO MOVED IN

EUC     LXI     B,BOOTAL        ; SET THE COUNT TO MOVE IN CONSTANTS AND VECTORS
        LXI     D,BOOTA         ; SET THE SOURCE ADDRESS
        LXI     H,D.CON         ; SET THE DESTINATION ADDRESS
        CALL    DMOVE           ; MOVE IT

;       ENTRY POINT FROM H17 (CONSTANTS ALREADY MOVED IN)

EUC.    DI                      ; STOP CLOCK
        LXI     H,CLOCK17       ; LOAD CLOCK ROUTINE ADDRESS
        SHLD    UIVEC+1         ; SET IT INTO VECTOR LOCATION
        EI

;       Zero out H67 operating system info

;       ERRNZ   S.OSZ-S.OSI02   ; MUST BE CONTIGUOUS BYTES
        LXI     H,S.OSI
        MVI     B,1+1+3
        CALL    DZERO           ; Zero area

        JMP     USERFWA

;;      DYBYTX
;
;       DYBYTX DETERMINES WHETHER TO OUTPUT THE BYTE
;       IN HEX OR OCTAL. IF IN OCTAL WE MUST REPLACE
;       THE CODE WE PATCHED TO GET US HERE.
;
;       ENTRY:  (A) = BYTE OF TO OUTPUT

DYBYTX  MOV     C,A             ; SAVE BYTE
        CPU     Z80
        EXX
        CPU     8080
        MOV     A,L             ; GET RADIX FLAG
        CPU     Z80
        EXX
        CPU     8080
        ANA     A               ; 'Z' SET IF OCTAL
        MOV     A,C             ; RESTORE A
        CPU     Z80
        JR      NZ,DYBYTH       ; IF IN HEX
        CPU     8080

DYBYT0  MOV     C,A
        ANI     11000000B
        RLC
        RLC
        ANI     00000011B
        JMP     DYBYT.1         ; FINISH UP OLD OCTAL ROUTINE

;       IS HEX

DYBYTH  MOV     C,A
        ANI     11110000B
        RRC
        RRC
        RRC
        RRC                     ; MOVE DOWN HIGH HALF
        ANI     00001111B
        ADI     '0'
        CPI     '9'+1
        CPU     Z80
        JR      C,DYBYTH1
        CPU     8080
        ADI     7
        CPU     Z80
DYBYTH1 LD      IY,DYBYTH2      ; SET RETURN ADDRESS
        CPU     8080
        JMP     DYASC

DYBYTH2 MOV     A,C
        ANI     00001111B
        ADI     '0'
        CPI     '9'+1
        CPU     Z80
        JR      C,DYBYTH3
        CPU     8080
        ADI     7
        CPU     Z80
DYBYTH3 LD      IY,DYBYTH4
        CPU     8080
        JMP     DYASC

        CPU     Z80
DYBYTH4 JP      (IX)
        CPU     8080

;;      DYMM5
;
;       DETERMINE NUMBER OF BACKSPACES
;       TO TYPE FOR EACH CHARACTER INPUT
;

        CPU     Z80
DYMM5   EXX
        CPU     8080
        MOV     A,L
        CPU     Z80
        EXX
        CPU     8080
        ANA     A
        MVI     A,A.BKS
        MVI     H,3
        JZ      DYME5.5
        DCR     H
        JMP     DYME5.5

;;      CONVERT - BASE CONVERSION
;
;       CONVERT CONVERTS THE INPUT IN THE OPPOSITE
;       RADIX AND CHANGES IT TO THE CURRENT RADIX
;

CONVERT LXI     H,MSG.CON
        CALL    TYPMSG
        LXI     H,IOWRK+1
        MVI     D,A.CR
        CALL    CHKRAD
        CPU     Z80
        JR      Z,CONV.O        ; IF OCTAL
        CPU     8080

CONV.H  CALL    IOA0
        CPU     Z80
        JR      CONV.E
        CPU     8080

CONV.O  CALL    IHA.

CONV.E  LHLD    IOWRK
        XCHG
        JMP     TOA

MSG.CON DB      'onvert ',0

;;      H17X - H17 Extension routine
;
;       H17x is the extension to the H17 Abort command
;

H17X    CALL    R.ABORT

;       Step the heat out 10 tracks

        CALL    R.SDP           ; Set up device
        MVI     A,10
        STA     D.TT            ; Set target track to 10
        CALL    D.SDT           ; Seek Desired track

        JMP     R.ABORT         ; Abort and return

;;      MTRA - COMMAND DESCRIPTOR TABLE
;
;       THIS TABLE CONTAINS THE SINGLE LETTER COMMANDS
;       UNDERSTOOD BY MTR90. THE ENTRIES IN THIS TABLE
;       CONSIST OF A SINGLE LETTER (THE COMMAND KEY) FOLLOWED
;       BY A WORD ADDRESS OF THE ROUTINE.
;
;       NOTE: THIS TABLE WAS MOVED FROM UP FRONT BECAUSE
;               OF SIZE CONSIDERATIONS
;

MTRA    EQU     $
        DB      'G'             ; *GO*
        DW      GO88

        DB      'S'             ; *SUBSTITUTE*
        DW      SUBM

        DB      'P'             ; *PROGRAM COUNTER"
        DW      PCA

        DB      'B'             ; *BOOT*
        DW      BOOT

        DB      'I'             ; *INPUT*
        DW      INPUT

        DB      'O'             ; *OUTPUT*
        DW      OUTPUT

        DB      'R'             ; *RADIX*
        DW      RADIX

        DB      'T'             ; *TEST RAM*
        DW      MEMORY

        DB      'V'             ; *VIEW*
        DW      VIEW

        DB      'C'             ; *CONVERT*
        DW      CONVERT
MTRAL   EQU     ($-MTRA)/3      ; NUMBER OF ENTRIES            /JWT  790507/

;;      BT170, BY174 - BOOT TABLES
;
;       THESE TABLES DEFINE DEVICE DEPENDENT INFORMATION USED
;       TO DETERMINE WHICH DEVICE IS TO BE BOOTED FROM
;
;       THE ORGANIZATION OF THE TWO TABLES IS IDENTICAL:
;
;       BYTE 1          -       PORT NUMBER OF THESE DEVICES
;
;       BYTE 2          -       DEVICE 0 TMFG
;       BYTE 3          -           MAX UNITS
;       BYTE 4,5        -           BOOT CODE ADDRESS
;
;       BYTE 6          -       DEVICE 1 TMFG
;       BYTE 7          -           MAX UNITS
;       BYTE 8,9        -           BOOT CODE ADDRESS
;
;       etc., etc., etc. THRU DEVICE 3
;
;       NO END-OF-TABLE CHECK IS MADE, THEREFORE 4 ENTRIES
;       MUST EXIST PER TABLE

BT174   DB     174Q             ; PORT ADDRESS
BT174E  EQU    $

BTH174  DB     0                ; TMFG = 0
        DB     '3'              ; MAX UNIT = 4
        DW     H17              ; BOOT ADDRESS

BTH474  DB     1
        DB     '4'
        DW     Z47

BTH674  DB     0
        DB     '4'
        DW     H67

BTHFE4  DB     0
        DB     '1'
        DW     FEDEV

BT174L  EQU    ($-BT174E)/4     ; INSURE ALL ENTRIES FILLED
        ERRNZ  BT174L-4

BT170   DB     170Q             ; PORT ADDRESS
BT170E  EQU    $

BTH370  DB     0
        DB     '4'
        DW     H37

BTH470  DB     1
        DB     '4'
        DW     Z47

BTH670  DB     0
        DB     '4'
        DW     H67

BTHFE0  DB     0
        DB     '1'
        DW     FEDEV

BT170L  EQU    ($-BT170E)/4
        ERRNZ  BT170L-4

        DB     10000Q-$ DUP 0   ; FILL REST OF ROM WITH ZEROES

        ERRMI  10000Q-$         ; MUST NOT EXCEED 4K BYTES

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
DATA    DS      1               ; OUTPUT TO 362Q DATA SAVE
BLKICW  DS      2               ; H37 INTERRUPT RETURN ADDRESS
RADFLG  DS      1               ; RADIX FLAG
VEWHLD  DS      2
MEML    EQU     $
        ERRMI   20520Q-MEML
        ORG     20520Q
PRIM    DS      1               ; PRIMARY DEVICE ADDR. PORT
TMFG    DS      1               ; TIMER INTERRUPT FLAG, =1 FOR Z47, =0 FOR H17
MYCNT   DS      1               ; COUNTER FOR TIMER INTERRUPT
AUTOB   DS      1               ; AUTO BOOT FLAG
STK     DS      2               ; STACK POINTER FOR RE-BOOT
