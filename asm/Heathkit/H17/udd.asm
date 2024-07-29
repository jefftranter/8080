;;      DUDD - UNPACK DECIMAL DIGITS.
;
;       UDD CONVERTS A 16 BIT VALUE INTO A SPECIFIED NUMBER OF
;       DECIMAL DIGITS. THE RESULT IS ZERO FILLED.
;
;       ENTRY   (B,C) = ADDRESS VALUE
;               (A) = DIGIT COUNT
;               (H,L) = MEMORY ADDRESS
;       EXIT    (HL) = (HL) + (A)
;       USES    ALL


DUDD    EQU     $
        CALL    DDADA
        PUSH    H               ; SAVE FINAL (H,L) VALUE

UDD1    PUSH    PSW
        PUSH    H
        LXI     D,10
        CALL    DDU66           ; (H,L) = VALUE/10
        PUSH    H
        POP     B               ; (B,C) = REMAINDER
        POP     H
        MVI     A,'0'
        ADD     E               ; ADD REMAINDER
        DCX     H
        MOV     M,A             ; STORE DIGIT
        POP     PSW
        DCR     A
        JNZ     UDD1            ; IF MORE TO GO
        POP     H               ; RESTORE H
        RET                     ;    RETURN
