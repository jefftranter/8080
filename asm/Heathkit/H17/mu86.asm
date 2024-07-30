;;      DMJ86 - MULTIPLY 8X16 UNSIGNED.
;
;       DMU86 MULTIPLIES A 16 BIT VALUE BY A 8
;       BUT VALUE.
;
;       ENTRY   (A) = MULTIPLIER
;               (DE) = MULTIPLICAND
;       EXIT    (HL) = RESULT
;               'Z' SET IF NOT OVERFLOW
;       USES    A,F,H,L


DMU86   LXI     H,0             ; (HL) = RESULT ACCUMULATOR
        PUSH    B
        MOV     B,H             ; (B) = OVERFLOW FLAG
MU860   ORA     A               ; CLEAR CARRY

MU861   RAR
        JNC     MU862           ; IF NOT TO ADD
        DAD     D
        JNC     MU862           ; NOT OVERFLOW
        INR     B
MU862   ORA     A
        JZ      MU863           ; IF DONE
        XCHG
        DAD     H
        XCHG
        JNC     MU861           ; LOOP IF NOT OVERFLOW
        INR     B
        JMP     MU860

MU863   ORA     B               ; SET *Z* FLAG IF NOT OVERFLOW
        POP     B               ; RESTORE (BC)
        RET
