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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ORG     14000Q

        JMP     BOOT            ; BOOT CODE

;;      MEMORY DIAGNOSTIC.
;

        LXI     H,-64
        DAD     SP              ; (HL) = END
        XCHG                    ; (DE) = END+1
        LXI     H,20100Q        ; (HL) = START
        HLT                     ; PAUSE FOR ADJUSTMENT

