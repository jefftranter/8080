;;      DDU66 - UNSIGNED 16 / 16 DIVIDE.
;
;       (HL) = (BC)/(DE)
;
;       ENTRY   (BC), (DE) PRESET
;       EXIT    (HL) = RESULT
;               (DE) = REMAINDER
;       USES    ALL


DDU66   MOV     A,D             ; TWOS COMPLEMENT (DE)
        CMA
        MOV     D,A
        MOV     A,E
        CMA
        MOV     E,A
        INX     D
        MOV     A,D
        ORA     E
        JZ      DU665           ; IF DIVIDE BY 0
        XRA     A

;       SHIFT (DE) LEFT UNTIL:
;
;       1) DE < BL
;       2) OVERFLOW

DU661   MOV     H,D
        MOV     L,E
        DAD     B
        JNC     DU662           ; IS TOO LARGE
        INR     A               ; COUNT SHIFT
        MOV     H,D
        MOV     L,E
        DAD     H
        XCHG                    ; (DE) = (DE)*2
        JC      DU661           ; IF NOT OVERFLOW

;       (DE) OVERFLOWED, PUT IT BACK.

        XCHG
        DCR     A               ; REMOVE EXTRA COUNT

;       REAXY TO START SUBTRACTING. (A) = LOOP COUNT

DU662   MOV   H,B               ; (H,L) = WORKING VALU
        MOV   L,C
        LXI   B,0               ; (BC) = RESULT
DU663   PUSH  PSW               ; SAVE (A)
        DAD   D
        JC    DU664             ; IF SUBTRACT OK
        MOV   A,L               ; ADD BACK IN
        SUB   E
        MOV   L,A
        MOV   A,H
        SBB   D
        MOV   H,A
DU664   MOV   A,C
        RAL
        MOV   C,A
        MOV   A,B
        RAL
        MOV   B,A

;       RIGHT SHIFT (DE)

        STC
        MOV   A,D
        RAR
        MOV   D,A
        MOV   A,E
        POP   PSW
        DCR   A
        JP    DU663             ; IF NOT DONE
DU665   XCHG                    ; (D,E) = REMAINDER
        MOV   H,B               ; (HL) = RESULT
        MOV   L,C
        RET
