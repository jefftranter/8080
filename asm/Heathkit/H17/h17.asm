;;;     SYDD - SYSTEM DEVICE DRIVER.
;
;       J.G. LETWIN, OCT 77
;
;       COPYRIGHT HEATH CO.

;;;     SYDD - SYSTEM DEVICE DRIVER.
;
;       SYDD IS THE DEVICE DRIVER FOR THE SYSTEM DEVICE, AN H17 MINI/-FLOPPY.

;;      ASSEMBLY CONSTANTS

MI.CPI  EQU     376Q            ; CPI INSTRUCTION
ERPTCNT EQU     10              ; SOFT ERROR RETRY COUNT

        INCLUDE mtr.asm
        INCLUDE u8251.asm
        INCLUDE ascii.asm
        INCLUDE hosdef.asm
        INCLUDE dirdef.asm
        INCLUDE devdef.asm
        INCLUDE h17def.asm
        INCLUDE ecdef.asm
        INCLUDE dddef.asm
        INCLUDE picdef.asm
        INCLUDE hosequ.asm
        INCLUDE edcon.asm
        INCLUDE edvec.asm
        INCLUDE edram.asm
        INCLUDE esval.asm
        INCLUDE esint.asm

        ORG     14000Q

        JMP     BOOT            ; BOOT CODE

;;      MEMORY DIAGNOSTIC.
;

        LXI     H,-64
        DAD     SP              ; (HL) = END
        XCHG                    ; (DE) = END+1
        LXI     H,20100Q        ; (HL) = START
        HLT                     ; PAUSE FOR ADJUSTMENT

;       (HL) = START
;       (DE) = END

;       ZERO TEST AREA

        SHLD    20100Q-2
MEM1    MVI     M,0
        INX     H
        CALL    DCDEHL
        JNZ     MEM1

;       START TESTING MEMORY. INCREMENT EACH BYTE IN TURN, AND COMPARE
;       THAT RESULT TO THE EXPECTED VALUE

        MVI     B,0             ; (B) = EXPECTED VALUE
MEM2    LHLD    20100Q-2
        INR     B

MEM3    INR     M
        MOV     A,M             ; (A) = VALUE
        CMP     B
        JZ      MEM4            ; IS OK

;       HAVE ERROR. (HL) = ADDRESS OF BYTE IN ERROR

        HLT
        NOP

MEM4    INX     H
        CALL    DCDEHL
        JNZ     MEM3            ; NOT AT END OF PASS
        JMP     MEM2            ; AT END OF PASS

        INCLUDE comp.asm
        INCLUDE dada.asm
        INCLUDE dada2.asm
        INCLUDE du66.asm
        INCLUDE hlihl.asm
        INCLUDE chl.asm         ; COMPLEMENT (HL)
        INCLUDE indl.asm        ; INDEXED LOAD

;;      DMOVE - MOVE DATA
;
;       DMOVE MOVES A BLOCK OF BYTES TO A NEW MEMORY ADDRESS.
;       IF THE MOVE IS TO A LOWER ADDRESS, THE BYTES ARE MOVED FROM
;       FIRST TO LAST.
;
;       IF THE MOVE IS TO A HIGHER ADDRESS, THE BYTES ARE MOVED FROM
;       LAST TO FIRST.
;
;       THIS IS DONE SO THAT AN OVERLAPPED MOVE WILL NOT 'RIPPLE'.
;
;       ENTRY   (BC) = COUNT
;               (DE) = FROM
;               (HL) = TO
;       EXIT    MOVED
;               (DE) = ADDRESS OF NEXT FROM BYTE
;               (HL) = ADDRESS OF NEXT *TO* BYTE
;               'C' CLEAR
;       USES    ALL

DDMOVE  EQU     $
        MOV     A,B
        ORA     C
        RZ                      ; NONE TO MOVE
        MOV     A,L             ; COMPARE *FROM* TO *TO*
        SUB     E
        MOV     A,H
        SBB     D
        JC      MOV2            ; IS MOVE DOWN (TO LOWER ADDRESSES)

;       IF MOVE UP (TI HIGHER ADDRESSES)

        DCX     B
        DAD     B               ; (HL) = *TO* LUA
        PUSH    H               ; SAVE *TO* LIMIT
        XCHG
        DAD     B               ; (HL) = *FROM* LWA
        PUSH    H               ; SAVE *FROM* LIMIT

MOV1    MOV     A,M             ; MOVE BYTE
        STAX    D
        DCX     D               ; DECREMENT *TO* ADDRESS
        DCX     H               ; DECREMENT *FROM* ADDRESS
        DCX     B               ; DECREMENT COUNT
        MOV     A,B
        ANA     A
        JP      MOV1            ; MORE TO GO
        POP     D               ; (DE) = *FROM* LIMIT
        POP     H               ; (HL) = *TO* LIMIT
        INX     D
        INX     H
        RET                     ; DONE

;       IS MOVE DOWN (TO LOWER ADDRESSES)

MOV2    LDAX    D               ; MOVE BYTE
        MOV     M,A
        INX     H               ; INCREMENT *FROM*
        INX     D               ; INCREMENT *TO*
        DCX     B               ; DECREMENT COUNT
        MOV     A,B
        ORA     C
        JNZ     MOV2            ; IF NOT DONE
        RET                     ; DONE

        INCLUDE mu10.asm
        INCLUDE mu66.asm
        INCLUDE mu86.asm
        INCLUDE savall.asm
        INCLUDE tjmp.asm
        INCLUDE tbra.asm
        INCLUDE udd.asm
        INCLUDE zero.asm

;       DWDR - WRITE DISABLE RAM.
;
;       DWDR IS CALLED TO DISABLE THE WRITABILITY OF THE
;       H17 CONTROLLER RAM AREA.
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    NONE


DWRD    PUSH    PSW
        DI
        LDA     D.DVCTL
        ANI     377Q-DF.WR
DWDR1   STA     D.DVCTL
        OUT     DP.DC
        EI
        POP     PSW
        RET

;;      DWER - WRITE ENABLE RAM.
;
;       DWER IS CALLED TO ENABLE WRITING TO THE H17 CONTROLLER RAM AREA.
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    NONE


DWER    PUSH    PSW
        DI
        LDA     D.DVCTL
        ORI     DF.WR
        JMP     DWDR1

;;      D.DISK - DEVICE DRIVER READ CODE.
;
;       ENTRY   (BC) = COUNT (IN SECTORS)
;               (DE) = ADDRESS
;               (HL) = SECTOR
;       EXIT    'C' CLEAR IF OK, EXIT TO CALLER
;               'C' SET IF ERROR
;                TO S.FASER (FATAL SYSTEM ERROR) IF UNIT 0
;                TO CALLER IF OTHER UNIT
;                 (A) = ERROR CODE


DWRITE  MVI     A,DC.WRI
        DB      MI.CPI          ; SKIP NEXT
DREAD   XRA     A               ; SET READ CODE

        CALL    SYDD            ; CALL DEVICE DRIVER
        RNC                     ; IF OK
        PUSH    PSW             ; SAVE CODE
        LDA     AIO.UNI
        ANA     A
        CZ      S.FASER         ; IF SY0:
        POP     PSW
        RET                     ; RETURN WITH BAD NEWS

;;      SREAD - READ FROM SYSTEM DISK.
;
;
;       ENTRY   (BC) = COUNT (IN SECTORS)
;               (DE) = ADDRESS
;               (HL) = SECTOR
;       EXIT    TO CALLER IF OK
;               TO S.FASER (FATAL SYSTEM ERROR) IF ERROR


SREAD   LDA     AIO.UNI
        PUSH    PSW             ; SAVE CURRENT LIMIT
        XRA     A
        STA     AIO.UNI
SREAD1  CALL    SYDD
        CC      S.FASER         ; READ ERROR
        POP     PSW
        STA     AIO.UNI
        RET


;;      CONSTANT ZEROS

ZEROS   DB      0,0,0,0,0,0,0,0

;;      SWRITE - WRITE TO SYSTEM DISK.
;
;       ENTRY   (BC) = COUNT (IN SECTORS)
;               (DE) = ADDRESS
;               (HL) = SECTOR
;       EXIT    TO CALLER IF OK
;               TO S.FASER (FATAL SYSTEM ERROR) IF ERROR


SWRITE  LDA     AIO.UNI
        PUSH    PSW             ; SAVE OLD UNIT #
        XRA     A
        STA     AIO.UNI         ; SET SYSTEM UNIT
        INR     A               ; (A) = DC.WRI
        JMP     SREAD1

;;      ERR.FNO - ERROR: FILE NOT OPEN

ERR.FNO MVI     A,EC.FNO        ; FILE NOT OPEN
        STC
        RET                     ; ERROR CODE

;;      ERR.ILR - ERROR: ILLEGAL REQUEST

ERR.ILR MVI     A,EC.ILR        ; ILLEGAL REQUEST
        STC
        RET

;;      CFF - CHAIN FREE BLOCK TO FILE.
;
;       CFF UNCHAINS A FREE BLOCK FROM THE FREE LIST, AND CHAINS
;       IT ONTO THE END OF THE ACTIVE FILE.

;       ENTRY   (HL) = ADDRESS IN GROUP TABLE OF THE GROUP IN QUESTION.
;               (E) = INDEX OF PREVIOUS GROUP IN LIST
;               AIO.XXX SETUP
;       EXIT    AIO.LGN = (L) (AT ENTRY)
;               AIO.LSI = 0
;       USES    A,F,D,H,L


CFF     MOV     A,M             ; (A) = NEXT FREE
        MVI     M,0             ; NEW BLOCK IS END OF CHAIN FOR FILE
        MOV     D,L             ; (D) = NEW INDEX
        MOV     L,E             ; (HL) = ADDRESS OF PREVIOUS BLOCK
        MOV     M,A             ; UNCHAIN FROM FREE CHAIN
        LDA     AIO.LGN         ; (A) = LAST GROUP #
        MOV     L,A             ; (HL) = ADDRESS OF FILE LAST GROUP
        MOV     M,D             ; LINK TO NEW LAST BLOCK
        LXI     H,AIO.LGN
        MOV     M,D             ; SET NEW LGN
        INX     H
        MVI     M,0             ; CLEAR LSI
        RET

;;      DCA - DETERMINE CONTIGUOUS AREA.
;
;       DCA IS CALLED TO FIND HOW MANY OF THE SECTORS WHICH ARE TO BE
;       READ ARE CONTIGUOUS.
;
;       ENTRY   (B) = SECTORS DESIRED
;                     AIO.XXX SETUP
;       EXIT    (B) = SECTORS-AIO.CNT
;               AIO.CNT = SECTORS WHICH ARE CONTIGUOUS
;               AIO.EOF = EC.EOF*2+1 IF EOF
;               AIO.TFP = SETUP WITH GROUP AND INDEX OF START OF AREA
;       USES    ALL


DCA1    CALL    FFL             ; FOLLOW FORWARD LINK

DCA     LHLD    AIO.CGN         ; (H) = CURRENT GP #, (L) = CUR SEC INDEX
        SHLD    AIO.TFP         ; TEMP FILE POINTER
        CALL    TFE             ; TEST FOR EOF
        STA     AIO.EOF         ; SET FLAG
        STA     AIO.CNT         ; SET CNT=0 IF EOF
        RZ                      ; IS EOF
        LDA     AIO.CNT         ; (A) = CURRENT SECTOR INDEX
        MOV     H,A
        LDA     AIO.SPG
        CMP     H               ; SEE IF GROUP EXHAUSTED
        JZ      DCA1            ; WAS POINTING AT END OF GROUP

;       SEE IF MORE NEEDED.

DCA2    MOV     A,B
        ANA     A
        RZ                      ; NO MORE SECTORS TO CHECK

;       SEE HOW MANY SECTORS ARE LEFT IN THIS GROUP

        LHLD    AIO.LGN         ; (L) = AIO.LGN, (H) = AIO.LSI
        LDA     AIO.CGN
        CMP     L               ; SEE IF WE ARE POINTED AT LAST GROUP
        JZ      DCA3            ; WE ARE
        LHLD    AIO.SPG-1       ; (H) = AIO.SPG
DCA3    PUSH    PSW             ; SAVE STATUS
        LDA     AIO.CSI         ; (A) = CURRENT SECTOR INDEX
        SUB     H               ; (A) = -SECTORS LEFT IN GROUP
        CMA
        INR     A               ; (A) = +SECTORS LEFT IN GROUP
        CMP     B
        JC      DCA4            ; NEED STILL MORE
        MOV     A,B             ; DON'T TAKE MORE THAN WE NEED
DCA4    MOV     C,A             ; (C) = AMOUNT TO TAKE
        LXI     H,AIO.CSI
        ADD     M               ; UPDATE CSI TO INDICATE NUMBER TO BE READ
        MOV     M,A
        MOV     A,C             ; (A) = NUMBER TO BE READ
        LXI     H,AIO.CNT
        ADD     M               ; ADD TO COUNT
        MOV     M,A
        MOV     A,B             ; (A) = AMOUNT NEEDED
        SUB     C
        MOV     B,A
        POP     PSW
        RZ                      ; WAS ON LAST TRACK: AM DONE
        MOV     A,B
        ANA     A
        RZ                      ; NO MORE NEEDED: AM DONE

;       USED UP THIS BLOCK, LINK TO THE NEXT.
;       IF NOT CONTIGUOUS, STOP HERE

        LDA    AIO.CGN
        INR    A
        PUSH   PSW              ; SAVE NEXT CONTIGUOUS BLOCK #
        CALL   FFL              ; FOLLOW FILE LINK
        POP    PSW
        CMP    L
        JZ     DCA2             ; GOT IT, WAS CONTIGUOUS
        RET                     ; STOP HERE

;;      FFB - FIND FREE BLOCK.
;
;       FFB IS CALLED TO LOCATE A FREE BLOCK IN THE GRT'S
;       FREE CHAIN.
;
;       FFB WILL ATTEMPT TO GET A 'PREFERRED BLOCK', IF POSSIBLE.
;       IF THE PREFERRED BLOCK IS NOT AVAILABLE, FFB WILL (OPTIONALLY) DO
;       THE BEST HE CAN: START A VIRGIN CLUSTER, IF POSSIBLE. THEN
;       JUST SETTLE FOR ANYTHING.
;
;       ENTRY   (D) = PREFERRED BLOCK NUMBER (O IF NONE)
;               (C) = PREFERRED FLAG (=0, WILL TAKE SOMETHING ELSE,
;                                    <>0, MUST HAVE PREFERRED BLOCK (OR NOTHING)
;       EXIT    'C' SET, EOM ON DEVICE
;               'C' CLEAR, NOT EOM
;               'Z' CLEAR, COULDN'T GET PREFERRED BLOCK (ONLY IF (C)<>0 ON ENTRY)
;               'Z' SET, GOT A BLOCK (PREFERRED OR NOT)
;               (HL) = ADDRESS OF BLOCK IN GRT TABLE
;               (E) = INDEX OF FREE BLOCK BEFORE THE FOUND ONE
;       USES    A,F,E,H,L


FFB     LHLD    AIO.GRT
        MOV     A,M             ; (A) = FIRST FREE BLOCK
        ANA     A
        STC                     ; ASSUME FROM
        RZ                      ; END OF MEDIA

;       NOT END OF MEDIA, TRY TO FIND THE CONTIGUOUS BLOCK IN THE FREE LIST.

        MOV     E,L             ; (E) = INDEX OF PREVIOUS BYTE
        INR     L
        MOV     M,A             ; FLAG CHANGE IN GRT
FFB4    MOV     L,A             ; (HL) = ADDRESS OF NEXT BYTE IN FREE CHAIN
        CMP     D
        RZ                      ; GOT THE ONE WE NEED
        JNC     FFB5            ; GONE TOO FAR
        MOV     E,L             ; SAVE THIS BLOCK INDEX
        MOV     A,M
        ANA     A
        JNZ     FFB4            ; TRY AGAIN

;       COULDN'T FIND CONTIGUOUS BLOCK. THIS MEANS A BREAK IN
;       CONTINUITY. IF WE HAVE ANYTHING, RETURN WITH IT.
;       IF WE HAVE NOTHING YET, TRY TO FIND A VIRGIN CLUSTER.

FFB5    MOV     A,C
        ANA     A
        RNZ                     ; MUST NOT CONTINUE
        MOV     L,A             ; (HL) = (AIO.GRT)
FFB6    MOV     E,L             ; (E) = INDEX OF PREVIOUS MODE
        MOV     L,M             ; LINK FORWARD
        LDA     AIO.DIR+DIR.CLU
        ANA     L               ; SEE IF START OF CLUSTER
        RZ                      ; GOT VIRGIN CLUSTER
        MOV     A,M
        ANA     A
        JNZ     FFB6            ; TRY AGAIN

;       CAN'T FIND VIRGIN CLUSTER. WILL TAKE WHATEVER WE CAN GET.

        MOV     L,A
        MOV     E,L             ; (E) = INDEX OR PREVIOUS MODE
        MOV     L,M             ; (HL) = ADDRESS OF FIRST FREE BLOCK BYTE
        RET                     ; RETURN WITH 'Z': GOT ONE

;;      FFL - FOLLOW FORWARD LINK.
;
;       FFL LINK AIO.CON TO THE NEXT GROUP
;
;       ENTRY   NONE
;       EXIT    AIO.CGN = LINK(AIO.CGN)
;               AIO.CSI = 0
;               (L) = AIO.CGN
;       USES    A,F,H,L

FFL     LHLD    AIO.GRT
        LDA     AIO.CGN
        MOV     L,A             ; (HL) = ADDRESS
        MOV     L,M             ; (L) = LINK
        MVI     H,0
        SHLD    AIO.CGN         ; SET CGN, CLEAR CSI
        RET

;;      LDD - LOAD DEVICE DRIVER.
;
;       LDD IS CALLED TO PERFORM THE SUSPENDED LOAD OF A DEVICE DRIVER.
;
;       IF SOME OVL CODE WISHES TO LOAD A DEVICE DRIVER, IT MUST
;       SUSPEND THE REQUEST, SINCE THE DEVICE DRIVER WILL OVERLAY THE
;       OVL CODE. AFTER THE OVL CODE EXITS, THE RESIDENT CODE WILL CALL
;       LDD TO PERFORM THE ACTUAL LOAD, OVER THE OVL.
;
;       ENTRY   DD.IOC = POINTER TO IOC.DDA
;               DD.LDA = LOAD ADDRESS
;               DD.LEN = LOAD LENGTH
;               DD.SEC = SECTOR INDEX ON SYSTEM DEVICE
;               DD.DTA = DEV.RES ADDRESS
;               DD.OPE = OPEN CODE (DC.OPR, DC.OPW, DC.OPU)
;       EXIT    OVL CODE DESTROYED
;       USES    NONE

S.DDSEC EQU     S.DDGRP         ; REFERENCE TO MAKE ASSEMBLE OK

LDD     CALL    DSAVALL         ; SAVE REGS

;       CLEAR OVL RESIDENT FLAG

        LXI     H,S.OVLFL
        MOV     A,M
        ANI     377Q-OVL.IN
        MOV     M,A             ; CLEAR IN FLAG

;       LOAD OVERLAY

        LHLD    S.DDLEN         ; (HL) = LENGTH
        MOV     B,H
        MOV     C,L             ; (BC) = LENGTH
        LHLD    S.DDLDA         ; (HL) = LOAD ADDRESS
        PUSH    H               ; SAVE FOR LATER
        XCHG
        LXI     H,SECSCR+255    ; FORCE NEW DISK READ RIGHT AWAY

;       LOAD BINARY

LDD2    CALL    LDD8            ; FIND NEXT BYTE
        MOV     A,M             ; (A) = NEXT BYTE
        STAX    D               ; COPY
        INX     D
        DCX     B
        MOV     A,B
        ORA     C
        JNZ     LDD2            ; MORE TO GO

;       CODE ALL LOADED, RELOCATE IT

        POP     B               ; (BC) = REL FACTOR
        DCR     B
        DCR     B
        CALL    LDD8
        MOV     E,M
        CALL    LDD8
        MOV     D,M             ; (DE) = REL ADDRESS OF WORD TO RELOCATE
        MOV     A,D
        ORA     E
        JZ      LDD4            ; ALL DONE
        XCHG                    ; (HL) = REL ADDRESS OF WORD TO RELOCATE
        DAD     B               ; (HL) = ABS ADDRESS OF WORD TO RELOCATE
        MOV     A,M
        ADD     C
        MOV     M,A
        INX     H
        MOV     A,M
        ADC     B
        MOV     M,A
        XCHG                    ; RESTORE (HL)
        JMP                     LDD3

;       SETUP ENTRY ADDRESSES IN TABLES

LDD4    LHLD    S.DDLDA
        XCHG                    ; (DE) = ENTRY ADDRESS
        LHLD    S.DDDTA         ; (HL) = ADDRESS OF DEVLST ENTRY
        MOV     A,M
        ORI     DR.IM           ; SET IN MEMORY
        MOV     M,A
        INX     H
        INX     H
        MOV     M,E
        INX     H
        MOV     M,D             ; SET ADDRESS IN TABLE
        XCHG                    ; (HL) = ENTRY POINT ADDRESS
        XRA     A
        STA     S.DDLDA+1       ; CLEAR LOAD FLAG
        LDA     S.DDOPC         ; (A) = OPEN CODE
        CALL    PCHL            ; CALL CODE
        JMP     DRSTALL         ; RESTORE REGISTERS

PCHL    PCHL

;;      LDD8 - READ A BYTE FROM THE FILE.
;
;       ENTRY   (HL) = SECSCR POINTER OF CURRENT BYTE
;               S.DDESC = SECTOR NUMBER OF NEXT SECTOR
;       EXIT    (HL) = ADDRESS OF NEXT BYTE
;       USES    L


LDD8    INR     L               ; POINT TO NEXT BYTE
        RNZ                     ; GOT IT

;       MUST READ ANOTHER

        PUSH    B
        PUSH    D
        PUSH    H
        XCHG                    ; (DE) = ADDRESS
        LXI     B,256
        LHLD    S.DDSEC         ; (HL) = SECTOR NUMBER TO READ
        INX     H
        SHLD    S.DDSEC         ; (HL) = NEXT SECTOR NUMBER TO READ
        DCX     H               ; RESTORE (HL)
        CALL    SREAD           ; READ IT
        POP     H
        POP     D
        POP     B
        RET

;       LDO - LOAD OVL CODE.
;
;       LDO IS CALLED WHEN THE OVL OVERLAY MUST BE LOADED.
;
;       IF USER HIGH MEM IS TOO HIGH, PART OF THE USER CODE WILL
;       HAVE TO BE SAVED ON THE SWAP AREA, BEFORE THE OVL CODE CAN BE
;       LOADED.
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,F,H,L


LDO     PUSH    D
        PUSH    B

;       SEE IF WILL HAVE TO PAGE USER CODE

        LHLD   S.OVLS           ; (HL) = SIZE OF HDOSOVL
        CALL   DCHL             ; COMPLEMENT (HL)
        XCHG                    ; (DE) = -SIZE
        LHLD   S.SYSM           ; (HL) = CURRENT FWA
        DAD    D                ; (HL) = NEW FWA WITH OVL
        SHLD   S.UCSF           ; SET USER SWAP (IN CASE IT IS SWAPPED)
        XCHG
        LHLD   S.USRM
        MOV    A,L
        SUB    C
        MOV    L,A
        MOV    A,H
        SBB    B
        MOV    H,A              ; (HL) = AMOUNT TO SWAP
        JC     LDO1             ; NO NEED TO SWAP

;       MUST DUMP (HL) BYTES OF USER CODE STARTING AT (DE)

        PUSH   B                ; SAVE ADDRESS

