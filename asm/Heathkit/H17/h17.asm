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
        INCLUDE cdehl.asm
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

DMOVE   EQU     $
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
        LDA     AIO.CSI         ; (A) = CURRENT SECTOR INDEX
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
LDD3    CALL    LDD8
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

        LHLD    S.OVLS          ; (HL) = SIZE OF HDOSOVL
        CALL    DCHL            ; COMPLEMENT (HL)
        XCHG                    ; (DE) = -SIZE
        LHLD    S.SYSM          ; (HL) = CURRENT FWA
        DAD     D               ; (HL) = NEW FWA WITH OVL
        SHLD    S.UCSF          ; SET USER SWAP (IN CASE IT IS SWAPPED)
        XCHG
        LHLD    S.USRM
        MOV     A,L
        SUB     E
        MOV     L,A
        MOV     A,H
        SBB     D
        MOV     H,A             ; (HL) = AMOUNT TO SWAP
        JC      LDO1            ; NO NEED TO SWAP

;       MUST DUMP (HL) BYTES OF USER CODE STARTING AT (DE)

        PUSH    D               ; SAVE ADDRESS
        SHLD    S.UCSL          ; SET LENGTH OF DUMP
        MOV     B,H
        MOV     C,L             ; (BC) = COUNT
        LHLD    S.SSN           ; (HL) = SECTOR FOR SWAP (SET BY BOOT)
        CALL    SWRITE
        LXI     H,S.OVLFL
        MVI     A,OVL.UCS
        ORA     M               ; SET USER CODE SWAPPED
        MOV     M,A
        POP     D               ; (DE) = ADDRESS TO LOAD

;       AM READY TO LOAD OVL OVERLAY.
;
;       (DE) = ADDRESS

LDO1    LHLD    S.OVLS
        MOV     B,H
        MOV     C,L             ; (BC) = SIZE OF OVERLAY
        LHLD    S.OSN
        CALL    SREAD           ; READ IT
        LXI     H,S.OVLFL
        MOV     A,M
        ORI     OVL.IN          ; SET IT IN
        MOV     M,A

;       RELOCATE OVL

        LHLD    S.UCSF          ; (HL) = FWA OVERLAY LOAD
        LXI     D,PIC.COD
        MOV     B,H
        MOV     C,L             ; (BC) = OVL FWA
        DAD     D               ; (HL) = ADDRESS OF ENTRY POINT
        SHLD    S.OVLE          ; SET ENTRY POINT
        DCX     H
        MOV     A,M
        DCX     H
        MOV     L,M
        MOV     H,A             ; (HL) = REL ADDRESS OF TABLE
        DAD     B               ; (HL) = ANS ADDRESS OF TABLE
        CALL    REL.            ; RELOCATE OVL

        POP     B
        POP     D
        RET

;;      PDI - PREPARE FOR DEVICE I/O
;
;       PDI PREPARES FOR PHYSICAL I/O, BY
;
;       1) COMPUTING THE PHYSICAL ADDRESS
;       2) PREPARE THE COUNT
;
;       ENTRY   AIO.XXX SETUP
;       EXIT    (BC) = COUNT
;               (HL) = SECTOR
;               (A) = 0
;       USES    A,F,B,C,H,L

PDI     LHLD    AIO.TFP         ; (L) = AIO.CGN, (H) = AIO.CSI
        LDA     AIO.SPG         ; (A) = SECTORS PER GROUP
        MOV     C,A
        MOV     A,L             ; (A) = GROUP NUMBER
        MOV     L,H
        MVI     H,0             ; (HL) = (0,CSI)
        MOV     B,H             ; (BC) = (0,SPG)

;       COMPUTE SECTOR NUMBER BY ADDING SPG 'BLOCK NUMBER' TIMES.

PDI1    DAD     B               ; ADD
        DCR     A
        JNZ     PDI1            ; MORE TO GO
        LDA     AIO.CNT         ; (A) = COUNT
        MOV     C,B             ; (C) = 0
        MOV     B,A             ; (B) = SECTOR COUNT
        XRA     A               ; CLEAR A
        RET

;;      REL - RELOCATE CODE.
;
;       REL PROCESSES A RELOCATION LIST.
;
;       ENTRY   (BC) = DISPLACEMENT FROM ASSEMBLED ADDRESS
;               (DE) = RELOCATION FACTOR (FROM CURRENT ADDRESS)
;               (HL) = FWA RELOCATION LIST
;       EXIT    NONE
;       USES    ALL


REL.    MOV     D,B             ; ENTRY FOR CODE DISPLACE = REL FACTOR
        MOV     E,C

REL     PUSH    D               ; SAVE RELOCATION FACTOR
        MOV     E,M
        INX     H
        MOV     D,M
        INX     H               ; (DE) = REL ADDRESS OF WORD TO RELOCATE
        MOV     A,D
        ORA     E
        JNZ     REL1            ; MORE TO DO
        POP     D
        RET                     ; EXIT

;       (DE) = INDEX OF WORD TO RELOCATE
;       (HL) = RELOCATION TABLE ADDRESS
;       (BC) = CODE DISPLACEMENT FACTOR
;       ((SP)) = CODE RELOCATION FACTOR

REL1    XCHG
        DAD     B               ; (HL) = ABS ADDR OF WORD TO REL
        XCHG                    ; (DE) = ABS CODE ADDRESS, (HL) = REL TABLE ADDR
        XTHL                    ; (HL) = CODE REL FACTOR
        LDAX    D
        ADD     L               ; RELOCATE WORD OF CODE
        STAX    D
        INX     D
        LDAX    D
        ADC     H
        STAX    D               ; RELOCATE
        XCHG                    ; (DE) = RELOCATION FACTOR
        POP     H               ; (HL) = RELOCATION TABLE ENTRY ADDRESS
        JMP     REL             ; DO IT AGAIN

;;      TFE - TEST FOR EOF.
;
;       TFE CHECKS FOR AN END OF FILE, INDICATE BY
;              AIO.CON = AIO.LGN
;              AIO.CSI = AIO.LSI
;
;       ENTRY   NONE
;       EXIT    'Z' CLEAR IF NOT EOF
;                (A) = 0
;               'Z' SET IF EOF
;                'C' SET
;                (A) = EC.EOF
;       USES    A,F,H,L


TFE     LHLD    AIO.LGN
        LDA     AIO.CGN
        CMP     L
        MVI     A,0
        RNZ                     ; NOT EOF
        LDA     AIO.CSI
        CMP     H
        MVI     A,0
        RNZ                     ; NOT EOF
        MVI     A,EC.EOF*2+1    ; SET EOF CODE
        RET

;;      RUC - RESTORE USER CODE.
;
;       RUC RESTORES THE USER PROGRAM CODE WHICH WAS SWAPPED
;       FOR THE OVL CODE.
;
;       SINCE RUC RESIDES IN THE OVL AREA, IT MAY NOT RETURN AFTER THE
;       DISK I/O CALL.
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    NONE


RUC     CALL    DSAVALL         ; SAVE REGISTERS
        LXI     H,DRSTALL
        PUSH    H               ; RETURN VIA DRSTALL
        LXI     H,S.OVLFL
        MOV     A,M
        ANA     A
        RP                      ; NOT SWAPPED
        ANI     377Q-OVL.UCS-OVL.IN ; RESTORE USER CODE, REMOVE OVL
        MOV     M,A

;       RESTORE USER CODE

        LHLD    S.UCSL
        MOV     B,H
        MOV     C,L             ; (BC) = COUNT
        LHLD    S.UCSF
        XCHG                    ; (DE) = ADDRESS
        LHLD    S.SSN           ; (HL) = SECTORS FOR SWAP
        JMP     SREAD           ; READ AND EXIT


;;      SYDD - SYSTEM DISK DEVICE DRIVER.
;
;       SYDD IS THE HDOS SYSTEM H17 DEVICE DRIVER.
;
;       ENTRY   (A) = DC.XXX FUNCTION CODE
;               OTHER REGISTERS SET AS NEEDED BY FUNCTION
;       EXIT    'C' CLEAR, OK
;                REGISTERS SET BY FUNCTION
;               'C' SET, ERROR
;                (A) = ERROR CODE
;       USES    ALL

R.SYDD  EQU     $
        ANA     A
        JZ      D.READ
        DCR     A
        JZ      D.WRITE
        DCR     A
        JZ      D.READR         ; READ REGARDLESS
        CPI     DC.ABT-2
        JC      D.XOK           ; IS NOT ABORT OR MOUNT, IGNORE
        JZ      D.ABORT         ; IS ABORT
        JMP     D.MOUNT

;;      MOUNT - MOUNT NEW DEVICE.
;
;       MOUNT PROCESSES DEVICE DEPENDENT MOUNTING OF A NEW MEDIA.
;
;       THE VOLUME SERIAL IS READ INTO THE VOLUME TABLE, AND
;       THE HEADS ARE HOMED.
;
;       ENTRY   (L) = VOLUME NUMBER (IF ANY)


R.MOUNT EQU     $
        MOV     B,L             ; (B) = VOLUME SERIAL
        LXI     H,0             ; SET SECTOR INDEX
        CALL    D.SDP           ; SET DEVICE PARAMETERS
        CALL    D.STZ           ; SEEK TRACK ZERO
        LHLD    D.VOLPT
        MOV     M,B             ; SET VOLUME NUMBER
        JMP     D.XOK           ; EXIT WITH STUFF OK


;;;     ABORT - ABORT ANY ACTIVE I/O.
;
;       ABORT CAUSES ANY ONGOING I/O TO BE ABORTED.

R.ABORT EQU     $
        CALL    D.SDP           ; SET DEVICE PARAMETERS
        CALL    D.STZ           ; SEE TRACK ZERO
;       JMP     D.XOK           ; EXIT AS IF OK

;;      XOK - EXIT WITH ALL OK FLAG.
;
R.XOK   XRA     A
R.XIT   PUSH    PSW             ; SAVE STATUS
XIT1    LDA     D.DLYHS
        ANA     A
        JNZ     XIT1            ; WAIT FOR HARDWARE DELAYS
        DI                      ; LOCK OUT CLOCK
        LDA     D.DVCTL
        ANI     DF.MO+DF.WR     ; REMOVE DEVICE SELECT
        OUT     DP.DC           ; DESELECT MOTOR
        STA     D.DVCTL         ; UPDATE BYTE
        LHLD    D.XITA
        SHLD    D.DLYMO         ; SET 120/2 SECONDS OF MOTOR ON
        POP     PSW
EIXIT   EI                      ; RESTORE INTERRUPTS
        RET


;;      CLOCK - PROCESS CLOCK INTERRUPTS.
;


CLOCK   LDA     TICCNT
        RRC
        RC                      ; NOT EVEN
        ANA     A
        LXI     H,D.DLYMO
        JNZ     CLOCK1          ; NOT HALF SECOND
        DCR     A               ; (A) = -1
        ADD     M               ; SUBTRACT ONE
        JNC     CLOCK1          ; WAS 0
        MOV     M,A             ; UPDATE
        JNZ     CLOCK1          ; NOT TIME FOR MOTOR OFF
        LDA     D.DVCTL
        ANI     DF.WR           ; REMOVE ALL BUT RAM/WRITE
        STA     D.DVCTL
        OUT     DP.DC           ; OFF MOTOR
CLOCK1  INX     H               ; (HL) = DDLYHS
        MOV     A,M             ; (A) = D.DLYHS
        SUI     1
        RC                      ; WAS 0
        MOV     M,A
        RET



;;;     READ - READ FROM DISK.
;
;       ENTRY   (BC) = COUNT
;               (DE) = ADDRESS
;               (HL) = BLOCK #
;               INTERRUPTS ENABLED
;       EXIT    (DE) = NEXT UNUSED ADDRESS
;               INTERRUPTS DISABLED
;       USES    ALL


R.READ  PUSH    H               ; SAVE (HL)
        CALL    D.SDP           ; SETUP DEVICE PARAMETERS
        LHLD    D.OPR
        INX     H
        SHLD    D.OPR           ; COUNT OPERATION

;       READ TO READ SECTOR
;
;       (BC) = AMOUNT
;       (DE) = ADDRESS
;       ((SP)) = SECTOR NUMBER

READ1   POP     H               ; (HL) = SECTOR NUMBER
        PUSH    D               ; SAVE ADDR
        MOV     A,C             ; ADJUST (B) SO THAT (B) = # OF WHOLE OR PARTIAL
        ANA     A               ;  SECTORS TO READ, (C) = BYTES OF LAST SECTOR TO
        JZ      READ1.5         ;  READ, (C)=0 IF TO READ ENTIRE LAST SECTOR
        INR     B

;       * * NOTE * *
;
;       THIS CODE RUNS WITH INTERRUPTS DISABLED FROM HERE ON

READ1.5 PUSH    B               ; SAVE COUNT
        CALL    D.DTS           ; DECODE TRACK AND SECTOR
READ2   MVI     A,1             ; (A) = DELAY COUNT FOR START

;       LOOK FOR RIGHT SECTOR.
;       (A) = DELAY COUNT BEFORE SEARCH

READ2.4 CALL    D.UDLY          ; DELAY SOME MICROSECONDS
        CALL    D.LPS           ; LOCATE PROPER SECTOR
        JC      READ7           ; ERROR
        POP     B               ; (BC) = COUNT
        POP     H               ; (HL) = ADDRESS FOR DATA

;       CHECK AMOUNT TO READ

READ3   MOV     A,B
        ORA     C
        JZ      READ8           ; NO MORE TO READ
        PUSH    H
        PUSH    B               ; SAVE COUNT AND ADDRESS IN CASE OF ERROR
        DCR     B               ; SEE IF ON LAST (MAYBE PARTIAL) SECTOR
        JZ      READ3.5         ; ON LAST SECTOR, READ (C) COUNT
        MVI     C,0             ; WILL READ ALL 256 BYTES
READ3.5 MOV     B,C             ; (B) = # TO READ+1, (C) = # TO SKIP
        CALL    D.WSC           ; WAIT SYNC CHARACTER
        JC      READ71          ; DIDN'T GET ONE

;       READ DATA

READ4   CALL    D.RDB           ; READ BYTE
        MOV     M,A             ; STORE
        INX     H
        DCR     B
        JNZ     READ4           ; MORE TO GO
        MOV     A,C
        ANA     A
        JZ      READ6           ; NONE TO DISCARD

;       READ, CHECKSUM, AND DISCARD DATA

READ5   CALL    D.RDB
        INR     C
        JNZ     READ5
READ6   MOV     B,D             ; (B) = CHECKSUM
        CALL    D.RDB
        CMP     B
        JNZ     READ72          ; CHECKSUM ERROR

;       GOT GOOD SECTOR

        POP     B               ; (BC) = OLD COUNT
        DCR     B               ; COUNT SECTOR READ
        JZ      READ8           ; JUST READ LAST ONE

;       HAVE MORE TO READ

        XTHL                    ; SAVE ADDRESS
        PUSH    B               ; SAVE COUNT
        LXI     H,D.TS
        INR     M               ; COUNT SECTOR
        MVI     A,10
        SUB     M
        MVI     A,0
        JNZ     READ2.4         ; NOT AT END OF TRACK
        MOV     M,A             ; SECTOR # = 0
        DCX     H
        INR     M
        EI                      ; RESTORE INTERRUPTS UNTIL *STS* CALLED
        CALL    D.SDT           ; SEE DESIRED TRACK
        JMP     READ2

;       CAN'T GET DATA, HEADER OR CHECKSUM PROBLEM

READ71  LXI     H,D.E.MDS       ; MISSING DATA SYNC ERROR
        CALL    D.ERRT
        JMP     READ7

READ72  LXI     H,D.E.CHK       ; CHECKSUM ERROR
        CALL    D.ERRT

READ7   CALL    D.CDE           ; COUNT DISK ERROR
        JNC     READ2           ; TRY AGAIN
        POP     B
        POP     D
        MVI     A,EC.RF         ; READ FAILURE
        JMP     D.XIT           ; TOO MANY ERRORS, TOO BAD...

;       ENTIRE READ WAS OK

READ8   POP     H               ; CLEAN STACK
        JMP     D.XOK           ; EXIT OK


;;;     READR -  READ DISK REGARDLESS OF VOLUME PROTECTION.
;
;       ENTRY   (BC) = COUNT
;               (DE) = ADDRESS
;               (HL) = BLOCK #
;       EXIT    (DE) = NEXT USED ADDRESS
;       USES    ALL


R.READR PUSH    H               ; SAVE (HL)
        CALL    D.SDP           ; SETUP DEVICE PARAMETERS
        LXI     H,ZEROS
        SHLD    D.VOLPT
        JMP     READ1           ; PROCESS AS REGULAR READ


;;;     WRITE - PROCESS DISK WRITE.
;
;       ENTRY   (BC) = COUNT
;               (DE) = ADDRESS
;               (HL) = BLOCK #
;       EXIT    (LINK) = LAST BLOCK #
;       USES    ALL


R.WRITE EQU     $
        PUSH    H               ; SAVE BLOCK NUMBER
        CALL    D.SDP           ; SET DEVICE PARAMETERS
        LHLD    D.OPW
        INX     H
        SHLD    D.OPW           ; COUNT OPERATION
        IN      DP.DC           ; SEE IF DISK WRITE PROTECTED
        ANI     DF.WP
        STC
        MVI     A,EC.WP
        JNZ     WRITE8          ; DISK IS WRITE PROTECTED

;       READY TO WRITE SECTOR
;
;               (BC) = COUNT
;               (DE) = ADDRESS
;               ((SP)) = SECTOR NUMBER

        LXI     H,377Q
        DAD     B
        MOV     B,H             ; (B) = # OF SECTORS TO WRITE

WRITE1  POP     H               ; (HL) = SECTOR NUMBER
        PUSH    D               ; SAVE ADDR

;       * * NOTE * *
;
;       THIS CODE RUNS WITH INTERRUPTS DISABLED FROM THIS POINT ON

        CALL    D.DTS           ; DETERMINE TRACK AND SECTOR
WRITE2  MVI     A,1             ; (A) = SHORT DELAY COUNT

;       FIND RIGHT SECTOR
;       (A) = DELAY COUNT

WRIT2.5 CALL    D.UDLY          ; DELAY SOME MICROSECONDS
        PUSH    B               ; SAVE COUNT
        CALL    D.LPS           ; LOCATE PROPER SECTOR
        POP     B               ; (BC) = COUNT
        JC      WRITE7          ; CAN'T FIND IT
        POP     H               ; (HL) = ADDR
        LDA     D.WRITA         ; (A) = GUARDBAND DELAY
WRITE4  DCR     A
        JNZ     WRITE4          ; PAUSE OVER GUARDBAND
        LDA     D.WRITB
        MOV     C,A             ; (C) = # OF 00 CHARACTERS
        LDA     D.WRITC         ; (A) = 128/8 = TWO CHARACTER TIMES BEFORE WRITING
        CALL    D.WSP           ; WRITE SYNC PATTERN

;       OUT WITH THE DATA

WRITE5  MOV     A,M
        CALL    D.WNB
        INX     H
        DCR     C
        JNZ     WRITE5          ; NOT DONE YET
        MOV     A,D             ; (A) = CHECKSUM
        CALL    D.WNB           ; WRITE CHECKSUM

;       HAVE DONE WRITING. LEAVE WRITE-GATE OPEN FOR 3 CHARACTER TIMES
;       TO FINISH TUNNEL ERASING.

        CALL    D.WNB
        CALL    D.WNB
        CALL    D.WNB
        LDA     D.DVCTL
        OUT     DP.DC           ; OFF DISK CONTROL
        DCR     B
        JZ      D.XOK           ; ALL DONE
        PUSH    H               ; SAVE ADDRESS
        LXI     H,D.TS
        INR     M
        MVI     A,10
        SUB     M
        MVI     A,0
        JNZ     WRIT2.5         ; NOT AT END OF TRACK

;       MOVE TO NEXT TRACK

        MOV     M,A             ; CLEAR CURRENT SECTOR INDEX
        DCX     H
        INR     M
        EI                      ; RESTORE INTERRUPTS UNTIL *STS* CALL
        CALL    D.SDT           ; SEEK DESIRED TRACK
        JMP     WRITE2

;       ERROR

WRITE7  CALL    D.CDE           ; COUNT DISK ERROR
        JNC     WRITE2          ; TRY AGAIN
        MVI     A,EC.WF         ; WRITE FAILURE
WRITE8  POP     H               ; RESTORE STACK
        JMP     D.XIT           ; TOO MANY... TRY AGAIN

;;      CDE - COUNT DISK ERRORS.
;
;       CDE IS CALLED WHEN A DISK SOFT ERROR OCCURS. IF THERE HAVE
;       OCCURRED 10 SOFT ERRORS FOR THIS OPERATION, THEN A HARD ERROR
;       IS FLAGGED.
;
;       ENTRY   NONE
;       EXIT    'C' SET IF HARD ERROR
;               INTERRUPTS DISABLED
;       USES    A,F,H,L

R.CDE   EI                      ; RESTORE INTERRUPTS
        CALL    D.STZ           ; SEEK TRACK ZERO
        CALL    D.SDT           ; SEEK DESIRED TRACK
        ANA     A               ; CLEAR CARRY
        LHLD    D.SECNT
        INX     H
        SHLD    D.SECNT         ; INCREMENT COUNT
        LXI     H,D.OECNT       ; (HL) = #OPERATION ERROR COUNT
        DCR     M
        RP                      ; NOT TOO MANY
        DCX     H
        MVI     A,-ERPTCNT
        ADD     M               ; REMOVE SOFT COUNT
        MOV     M,A
        INR     M               ; COUNT HARD ERROR
        STC
        RET                     ; EXIT WITH 'C' SET

;;      DTS - DECODE TRACK AND SECTOR.
;
;       DTS DECODES THE TRACK AND SECTOR NUMBER FROM
;       THE SUPPLIED SECTOR INDEX.
;
;       ENTRY   (HL) = SECTOR INDEX
;               INTERRUPTS ENABLED
;       EXIT    D.TS = SECTOR NUMBER
;               D.TT = TRACK
;               INTERRUPTS DISABLED
;       USES    A,F,H,L


R.DTS   PUSH    B               ; SAVE (BC)
        LXI     B,-10
        MOV     A,B             ; (A) = 377Q
DTS1    INR     A
        DAD     B
        JC      DTS1
        STA     D.TT            ; SET TRACK NUMBER
        MOV     A,L
        ADI     10
        STA     D.TS            ; SET SECTOR
        POP     B               ; RESTORE (BC)
        JMP     R.SDT           ; SEE DESIRED TRACK

;;      SDT - SEEK DESIRED TRACK.
;
;       SDT MOVES THE DISK ARM TO THE DESIRED (D.TT) TRACK.
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,F,H,L


;       MOVE ARM IN

SDT3    INR     M
        CALL    D.MAI


R.SDT   LHLD    D.TRKPT
        LDA     D.TT
        CMP     M
        JZ      D.STS           ; GOT THERE
        JP      SDT3            ; MUST MOVE IN

;       MOVE ARM OUT

SDT1    DCR     M               ; UPDATE TRACK NUMBER
        CALL    D.MAO           ; MOVE ARM OUT
        JMP     R.SDT           ; SEE IF THERE YET

;;      MAI - MOVE DISK ARM IN ONE TRACK.
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,F


;;      MAO - MOVE ARM OUT.
;
;       USES    A,F

R.MAI   MVI     A,DF.DI         ; SET DIRECTION

        DB      MI.CPI          ; GOBBLE XRA INSTRUCTION
R.MAO   XRA     A               ; SET DIRECTION

        PUSH    H
        MOV     H,A
        LDA     D.DVCTL
        ANI     377Q-DF.DI-DF.ST
        ORA     H               ; SET DIRECTION
        OUT     DP.DC           ; SET DIRECTION
        POP     H               ; RESTORE (HL)
        ORI     DF.ST
        OUT     DP.DC           ; START STEP
        XRI     DF.ST
        OUT     DP.DC           ; COMPLETE STEP
        LDA     D.MAIA          ; (A) = MS/2 FOR TRACK TIMING
;       JMP     D.DLY           ; DELAY 8 MS

;;      DLY - DELAY BY FRONT PANEL CLOCK.
;
;       ENTRY   (A) = MILLISECOND COUNT/2
;       EXIT    NONE
;       USES    A,F


R.DLY   PUSH    H
        LXI     H,TICCNT
        ADD     M
DLY1    CMP     M
        JNZ     DLY1
        POP     H
        RET

;;      LPS - LOCATE PROPER SECTOR.
;
;       LPS READS OVER SECTOR HEADERS UNTIL THE PROPER SECTOR
;       IS FOUND.
;
;       UPON ENTRY, THE ARM SHOULD BE POSITIONED OVER THE SECTOR.
;
;       D.TT = DESIRED TRACK
;       D.TS - DESIRED SECTOR
;
;       ENTRY   NONE
;       EXIT    INTERRUPTS DISABLED
;               'C' SET IF ERROR
;       USES    ALL BUT C


LPS0    CALL    D.STS           ; SKIP THIS SECTOR

R.LPS   LDA     D.LPSA          ; (A) = #OF TRIES FOR THIS SECTOR
        MOV     B,A
        LDA     D.DLYHS
        ANA     A
        JNZ     LPS0            ; WAIT FOR HEADS TO SETTLE

LPS1    DI                      ; DISABLE INTERRUPTS
        CALL    D.WSC           ; WAIT SYNC CHARACTER
        JC      LPS3            ; NONE
        LHLD    D.VOLPT
        CALL    D.RDB
        CMP     M               ; SEE IF PROPER VOLUME
        JNZ     LPS4            ; WRONG VOLUME
        LXI     H,D.TT
        CALL    D.RDB
        CMP     M               ; SEE IF PROPER TRACK
        JNZ     LPS5            ; WRONG TRACK
        INX     H
        CALL    D.RDB
        CMP     M
        JNZ     LPS2            ; WRONG SECTOR

;       GOT RIGHT SECTOR, READ CHECKSUM

        MOV     H,D
        CALL    D.RDB
        CMP     H
        RZ                      ; ALL OK
        MVI     L,D.E.HCK#256   ; HEADER CHECKSUM ERROR
LPS1.5  MVI     H,D.ERR/256     ; (HL) = ERROR BY ADDRESS
        CALL    D.ERRT          ; COUNT ERROR

;       WRONG SECTOR OR BAD DATA. TRY SOME MORE

LPS2    CALL    D.STS           ; SKIP THIS SECTOR
        DCR     B
        JNZ     LPS1            ; TRY AGAIN
        STC                     ; ENOUGH TRIES
        RET                     ; ERROR

LPS3    MVI     L,D.E.HSY#256   ; HEADER SYNC ERROR
        JMP     LPS1.5

LPS4    MVI     L,D.E.VOL#256   ; BAD VOLUME NUMBER
        JMP     LPS1.5

LPS5    MVI     L,D.E.TRK#256   ; BAD TRACK NUMBER
        JMP     LPS1.5

;;      RDB - READ BYTE FROM DISK.
;
;       RDB READS THE NEXT BYTE FROM THE DISK.
;
;       ENTRY   (D) = CHECKSUM
;       EXIT    (A) = BYTE
;               (D) UPDATED
;       USES    A,F,D,E


R.RDB   IN      UP.ST
        RAR
        JNC     R.RDB           ; NOT READY YET
        IN      UP.DP           ; (A) = DATA
        MOV     E,A
        XRA     D               ; DIFFER
        RLC                     ; SHIFT LEFT
        MOV     D,A             ; REPLACE
        MOV     A,E             ; (A) = CHAR
        RET                     ; EXIT

;;      SDP - SET DEVICE PARAMETERS
;
;       SDP SET UP ARGUMENTS FOR THE SPECIFIC UNIT
;
;       D.DVCTL = MOTOR ON, DEVICE SELECT
;       D.TRKPT = ADDRESS OF DEVICE TRACK NUMBER
;
;       ENTRY   AIO.UNI = UNIT NUMBER
;       EXIT    (HL) = (D.TRKPT)
;       USES    A,F,H,L


R.SDP   MVI     A,ERPTCNT
        STA     D.OECNT         ; SET MAX ERROR COUNT FOR OPERATION
        LDA     AIO.UNI
        PUSH    PSW             ; SAVE UNIT NUMBER
        INR     A               ; (A) = 1 IF DEV 0, 2 IF DEV 1
        ADD     A
        DI                      ; INTERLOCK CODE INTERRUPTS
        LXI     H,D.DVCTL
        XRA     M               ; MERGE WITH DF.WR BIT FROM D.DVCTL
        ANI     377Q-DF.WR
        XRA     M
        ORI     DF.MO           ; MOTOR ON
        MOV     M,A             ; UPDATE
        OUT     DP.DC           ; SELECT DRIVE, LOAD HEAD

;       SEE IF HEADS HAVE BEEN UNLOADED LONG ENOUGH TO REQUIRE LOAD DELAY

        LXI     H,D.DLYHS
        MOV     A,M             ; (A) = FLAG SET BY XIT
        ANA     A
        MVI     M,0             ; ASSUME NO RE-LOAD
        JNZ     SDP1            ; NO RE-LOAD
        LDA     D.SDPA          ; (A) = HEAD SETTLE WAIT TIME/4
        MOV     M,A             ; SET FOR CLOCK TIMER
SDP1    DCX     H
        MOV     A,M             ; (A) = MOTOR ON DELAY
        MVI     M,120           ; 60 SECONDS BEFORE TURN OFF AGAIN
        ANA     A               ; 'Z' IF MOTOR TURNED OFF
        INX     H               ; (HL) = #D.DLYHS
        JNZ     SDP2            ; MOTOR IS STILL ON
        LDA     D.SDPB          ; (A) = MOTOR WAIT TIME (MS/2)
        MOV     M,A
SDP2    EI                      ; RESTORE INTERRUPTS
        POP     PSW             ; (A) = UNIT NUMBER
        ADD     A               ; (A) = 2*UNIT NUMBER
        LXI     H,D.DRVTB
        ADD     L
        MOV     L,A             ; (HL) = ADDRESS OF TRACK ENTRY
        SHLD    D.TRKPT
        INX     H
        SHLD    D.VOLPT         ; SET VOLUME NUMBER
        RET

;;      STS - SKIP THIS SECTOR.
;
;       STS IS CALLED TO SKIP THE CURRENT SECTOR, REGARDLESS OF WHERE
;       THE HEAD IS POSITIONED.
;
;       STS WILL EXIT AT THE BEGINNING OF THE NEXT SECTOR.
;
;       1. IF THE HEAD IS NOT OVER A HOLE, WAIT 8 MS WHILE
;          HOLE CHECKING. IF NO HOLE IN THIS TIME, WHEN WE ARE IN
;          A REGULAR GAP, WAIT FOR THE NEXT HOLE AND EXIT.
;
;       2. IF THE HEAD IS OVER A HOLE, OR BECOMES SO DURING THE 8 MS
;          WAIT, THEN WAIT FOR THE HOLE TO PASS, WAIT 12 MILLISECONDS
;          IN CASE OF THE INDEX HOLE, THEN WAIT FOR THE NEXT HOLE AND EXIT.
;
;       ENTRY   NONE
;       EXIT    INTERRUPTS DISABLED
;       USES    A,F,H,L


R.STS   EI
        PUSH    B               ; SAVE (BC)
        IN      DP.DC
        RAR
        JC      STS2            ; AM CURRENTLY OVER HOLE

;       NO HOLE YET, WAIT 8 MS MIN (10 MAX) FOR HOLE TO
;       APPEAR

        LXI     H,TICCNT
        MOV     B,M             ; (B) = CURRENT TIME
STS1    IN      DP.DC
        RAR
        JC      STS2            ; GOT HOLE
        LDA     D.STSA          ; (A) = DELAY COUNT
        ADD     B               ; 10 MS MAX, 8 MS MIN
        CMP     M
        JNZ     STS1            ; 8 MS NOT UP YET
        JMP     STS3            ; AM IN SECTOR GAP

;       HAVE HOLE, SKIP IT AND WAIT 12 MS

STS2    CALL    WNH             ; WAIT FOR NO HOLE
        LDA     D.STSB          ; (A) = COUNT (10 MS MIN, 12 M MAX)
        CALL    D.DLY           ; WAIT
STS3    POP     B               ; RESTORE (BC)
        DI
;       JMP     WHD             ; WAIT HOLE DETECT

;;      WHD - WAIT HOLE DETECT.
;
;       WHD WAITS UNTIL A HOLE IS LOCATED.
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,F


WHD     IN      DP.DC
        RAR
        JNC     WHD             ; WAIT UNTIL FOUND
        LDA     D.WHDA          ; (A) = LOOP DELAY COUNT
        JMP     D.UDLY

;;      STZ - SEEK TRACK ZERO.
;
;       STZ SEEKS THE SELECTED UNIT ARM OUTWARDS UNTIL IT REACHES
;       TRACK ZERO.
;
;       THE ARM POSITION BYTE IS THEN UPDATED TO 0.
;
;       ENTRY   INTERRUPTS ENABLED
;       EXIT    INTERRUPTS ENABLED
;       USES    A,F,H,L


STZ0    CALL    D.MAO           ; MOVE ARM OUT

R.STZ   IN      DP.DC
        ANI     DF.T0
        JZ      STZ0            ; NOT TRACK 0 YET
        LHLD    D.TRKPT
        MVI     M,0             ; SET TRACK POINTER
        RET

;;      WNH - WAIT FOR NO HOLE.
;
;       WNH WAITS UNTIL THE CURRENT HOLE IS PAST.
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,F


WNH     IN      DP.DC
        RAR
        JC      WNH             ; STILL HOLE
        LDA     D.WNHA          ; (A) = DEBOUNCE COUNT
;       JMP     D.UDLY          ; WAIT A LITTLE

;;      UDLY - MICROSECOND DELAY
;
;       UDLY IS CALLED (WITH INTERRUPTS DISABLED)
;       TO WAIT FOR A CERTAIN NUMBER OF MICROSECONDS.
;
;       EACH TIME THROUGH THE DELAY LOOP CAUSES A PAUSE OF 15/2.048
;       MICROSECONDS.
;
;       ENTRY   (A) = LOOP COUNT (ZERO TAKEN AS 256)
;       EXIT    (A) = 0
;       USES    A,F


R.UDLY  DCR     A
        JNZ     R.UDLY
        RET


;;      WSC - WAIT SYNC CHARACTER.
;
;       WSC WAITS FOR THE APPEARANCE OF A SYNC CHARACTER. THE DISK SHOULD BE
;       SELECTED, MOVING, AND THE HEAD SHOULD BE OVER THE PRE-SYNC
;       ZERO BAND.
;
;       IF A SYNC IS NOT DETECTED IN 25 CHARACTER TIMES, AN ERROR IS RETURNED.
;
;       ENTRY   NONE
;       EXIT    'C' CLEAR IF OK, SYNC CHARACTER READ
;                (D) = 0 (CHECKSUM)
;               'C' SET OF NO SYNC FOUND
;       USES    A,F,D


R.WSC   MVI     A,C.DSYN
        OUT     UP.SC           ; SET SYNC CHARACTER
        IN      UP.SC           ; RESET SYNC SEARCH
        LDA     D.WSCA          ; (A) = NUMBER OF LOOPS IN 25 CHARACTERS
        MOV     D,A
WSC1    IN      DP.DC
        ANI     DF.SD           ; SEE IF SYNC
        JNZ     WSC2            ; GOT SYNC
        DCR     D
        JNZ     WSC1            ; TRY SOME MORE

;       COULDN'T FIND SYNC

        STC                     ; CAN'T FIND IT
        RET

;       FOUND IT

WSC2    IN      UP.DP           ; GOBBLE SYNC CHARACTER
        MVI     D,0             ; CLEAR CHECKSUM
        RET

;;      WSP - WRITE SYNC PATTERN.
;
;       WSP WRITES A SYNC PATTERN OF ZEROS, FOLLOWED BY A SYNC
;       CHARACTER.
;
;       ENTRY   (A) = INITIAL DELAY COUNTER
;               (C) = # OF ZERO BYTES TO WRITE
;       EXIT    (D) = CHECKSUM
;               (C) = 0
;       USES    A,F,C,D,E


R.WSP   DCR     A
        JNZ     R.WSP           ; DELAY

;       DELAY IS UP, ON WRITE GATE

        LDA     D.DVCTL
        INR     A               ; SET WRITE GATE
        OUT     DP.DC           ; SET GATE

;       USED AS ENTRY POINT BY DDIAG ...

WSP1    XRA     A
        CALL    D.WNB
        DCR     C
        JNZ     WSP1            ; DO MORE
        MVI     A,C.DSYN
        MOV     D,A             ; PRE-CLEAR CHECKSUM SO WNB EXITS WITH (D) = 0
        JMP     D.WNB           ; WRITE NEXT BYTE

;;      WNB - WRITE NEXT BYTE.
;
;       WNB WRITE A BYTE TO THE DISK, ASSUMING THAT THE WRITE GATE
;       IS ALREADY SELECTED.
;
;       ENTRY   (A) = CHARACTER
;               (D) = CHECKSUM
;       EXIT    (D) = CHECKSUM
;       USES    A,F,D,E


R.WNB   MOV     E,A
WNB1    IN      UP.ST
        ANA     A
        JP      WNB1            ; NOT READY
        MOV     A,E
        OUT     UP.DP           ; OUT DATA
        XRA     D
        RLC
        MOV     D,A
        RET

        DB      'G+S'

;;      BOOT CODE.
;
;       ENTERED TO BOOT DISK SYSTEM.

BOOT    DI                      ; WANT NO TROUBLES WITH INTERRUPTS !
        LXI     SP,STACK        ; CLEAR STACK
        LXI     B,BOOTAL
        LXI     D,BOOTA
        LXI     H,D.CON
        CALL    DMOVE           ; MOVE IN CONSTANTS AND VECTORS

;       ZERO WORK FIELD

        LXI     H,D.RAM
        MVI     B,D.RAML
        CALL    DZERO           ; ZERO MEMORY
        STA     AIO.UNI
        OUT     DP.DC           ; OFF DISK

;       SETUP ALL INTERRUPT VECTORS TO AN EI/RET SEQUENCE

        INR     A               ; (A) = UO.CLK
        STA     MFLAG           ; REQUEST CLOCK INTERRUPTS (*DI* STILL IN EFFECT NOW!)

        LXI     H,UIVEC         ; (HL) = UIVEC ADDRESS, (A) = 1
BOOT2   MVI     M,303Q
        INX     H
        MVI     M,EIXIT#256
        INX     H
        MVI     M,EIXIT/256
        INX     H
        ADD     A               ; SHIFT '1' IN (A) LEFT 1
        JP      BOOT2

;       SETUP CLOCK INTERRUPTS

BOOT3   LXI     H,CLOCK
        SHLD    UIVEC+1
        EI                      ; RESTORE INTERRUPTS

;       READ BOOT CODE

        CALL    R.ABORT         ; RESET DISK 0
        LXI     D,USERFWA
        LXI     B,9*256
        LXI     H,0
        CALL    R.READ          ; READ SYSTEM DISK BOOT CODE
        JNC     USERFWA         ; IS ALL OK

;       WAIT FOR HIM TO HIT A CHARACTER

        HLT
        JMP     BOOT            ; BOOT AGAIN


;;      DISK CONSTANT AND VECTOR INITIALIZATION TABLE.

BOOTA   EQU     $

        DW      2*256+120       ; HEAD UNSETTLE AND MOTOR ON TIMES
        DB      20              ; GUARDBAND COUNT FOR WRITE
        DB      10              ; NUMBER OF ZERO CHARACTERS AFTER HOLE EDGE
        DB      128/8           ; TWO CHARACTER DELAY BEFORE WRITING
        DB      15              ; TRACK-TO-TRACK STEP TIMES
        DB      20              ; NUMBER OF TRIES FOR CORRECT SECTOR
        DB      70/4            ; 70 MILLISECONDS WAIT FOR HEAD SETTLE
        DB      1000/4          ; 1 SECOND WAIT FOR MOTOR ON
        DB      8/2+1           ; MS/2 TO WAIT FOR INDEX HOLE
        DB      12/2+1          ; MS/2 TO WAIT PAST INDEX HOLE
        DB      20              ; UDLY COUNT FOR HOLE DEBOUNCE
        DB      20              ; UDLY COUNT FOR HOLE DEBOUNCE
        DB      64*25/20        ; LOOP COUNT FOR 25 CHARACTERS

;;      ERRT - ERROR TEST LOOP

R.ERRT  INR     M               ; COUNT ERROR
        RET                     ; EXIT

;       JMP VECTORS

        JMP     R.SYDD          ; D.SYDD (MUST BE FIRST)
        JMP     R.MOUNT         ; D.MOUNT
        JMP     R.XOK           ; D.XOK
        JMP     R.ABORT         ; D.ABORT
        JMP     R.XIT           ; D.XIT
        JMP     R.READ          ; D.READ
        JMP     R.READR         ; D.READR
        JMP     R.WRITE         ; D.WRITE
        JMP     R.CDE           ; D.CDE
        JMP     R.DTS           ; D.DTS
        JMP     R.SDT           ; D.SDT
        JMP     R.MAI           ; D.MAI
        JMP     R.MAO           ; D.MAO
        JMP     R.LPS           ; D.LPS
        JMP     R.RDB           ; D.RDB
        JMP     R.SDP           ; D.SDP
        JMP     R.STS           ; D.STS
        JMP     R.STZ           ; D.STZ
        JMP     R.UDLY          ; D.UDLY
        JMP     R.WSC           ; D.WSC
        JMP     R.WSP           ; D.WSP
        JMP     R.WNB           ; D.WNB
        JMP     R.ERRT          ; D.ERRT
        JMP     R.DLY           ; D.DLY
BOOTAL  EQU     $-BOOTA

; The remaining bytes were reverse engineered from the Heathkit ROM.

        DB      076Q, 031Q, 323Q, 177Q, 016Q, 372Q, 171Q, 315Q
        DB      303Q, 035Q, 171Q, 315Q, 303Q, 035Q, 363Q, 315Q
        DB      306Q, 037Q, 373Q, 166Q, 315Q, 355Q, 036Q, 001Q
        DB      134Q, 014Q, 076Q, 107Q, 315Q, 373Q, 036Q, 013Q
        DB      170Q, 261Q, 302Q, 314Q, 037Q, 076Q, 030Q, 323Q
        DB      177Q, 076Q, 333Q, 062Q, 125Q, 040Q, 315Q, 307Q
        DB      036Q, 330Q, 001Q, 132Q, 014Q, 315Q, 044Q, 036Q
        DB      376Q, 107Q, 300Q, 013Q, 170Q, 261Q, 302Q, 347Q
        DB      037Q, 373Q, 166Q, 000Q
        DB      "JGL", 0        ; INITIALS OF J.G. LETWIN
        DB      "HEATH", 0
