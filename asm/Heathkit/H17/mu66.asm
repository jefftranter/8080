;;      DMU66 - UNSIGNED 16X16 MULTIPLY.
;
;       ENTRY   (BC) = MULTIPLICAND
;               (DE) = MULTIPLIER
;       EXIT    (HL) = RESULT
;               'Z' SET IF NOT OVERFLOW
;       USES    ALL


DMU66   XRA     A
        PUSH    PSW             ; SAVE OVERFLOW STATUS
        LXI     H,0             ; (HL) = RESULT ACCUMULATOR

MU661   MOV     A,B
        RAR
        MOV     B,A
        MOV     A,C
        RAR
        MOV     C,A
        JNC     MU662           ; IF BIT CLEAR
        DAD     D
        JNC     MU662           ; IF NOT OVERFLOW
        POP     PSW
        INR     A
        PUSH    PSW
MU662   MOV     A,B
        ORA     C               ; SEE IF MULTIPLIER 0
        JZ      MU663           ; IF ZERO; AM DONE
        XCHG
        DAD     H               ; (D,E) = (DE)*2
        XCHG
        JNC     MU661           ; IF NOT OVERFLOW
        POP     PSW
        INR     A
        PUSH    PSW             ; FLAG OVERFLOW
        JMP     MU661           ; PROCESS NEXT BIT

MU663   POP     PSW             ; (A,F) = OVERFLOW STATUS
        RET
