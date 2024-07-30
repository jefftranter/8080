;;      DCDEHL - COMPARE (DE) TO (HL)
;
;       DCDEHL COMPARES (DE) TO (HL) FOR EQUALITY.
;
;       ENTRY   NONE
;       EXIT    'Z' SET IF (DE) = (HL)
;       USES    A,F

DCDEHL  MOV     A,E
        XRA     L
        RNZ                     ; IF DIFFERENT
        MOV     A,D
        XRA     H
        RET
