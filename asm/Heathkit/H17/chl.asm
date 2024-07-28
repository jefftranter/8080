;;      DCHL - COMPLEMENT (HL).
;
;       (HL) = -(HL)            TWO'S COMPLEMENT
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,F,H,L

DCHL    MOV     A,H
        CMA
        MOV     H,A
        MOV     A,L
        CMA
        MOV     L,A
        INX     H
        RET
