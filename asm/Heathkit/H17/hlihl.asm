;;      DHLIHL - LOAD HL INDIRECT THROUGH HL.
;
;       (HL) = ((HL))
;
;       ENTRY   NONE
;       EXIT    NONE
;       USES    A,H,L

DHLIHL  MOV     A,M
        INX     H
        MOV     H,M
        MOV     L,A
        RET
