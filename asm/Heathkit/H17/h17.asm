;;;     SYDD - SYSTEM DEVICE DRIVER.
;
;       J.G. LETWIN, OCT 77
;
;       COPYRIGHT HEATH CO.

;;;     SYDD - SYSTEM DEVICE DRIVER.
;
;       SYDD IS THE DEVICE DRIVER FOR THE SYSTEM DEVICE, AN H17 MINI/-FLOPPY.

;;      ASSEMBLY CONSTANTS

MI.CPT  EQU     376Q            ; CPI INSTRUCTION
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

