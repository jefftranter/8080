;;      DINDL - INDEXED LOAD.
;
;       DINDL LOADS DE WITH THE TWO BYTES AT (HL)+DISPLACEMENT
;
;       THIS ACTS AS AN INDEXED FULL WORD LOAD.
;
;       (DE) = ( (HL) + DISPLACEMENT )
;
;       ENTRY   ((RET)) = DISPLACEMENT (FULL WORD)
;               (HL) = TABLE ADDRESS
;       EXIT    TO (RET+2)
;       USES    A,F,D,E

DINDL   XTHL                    ; (HL) = RET, ((SP) = TBL ADDRESS
        MOV     E,M
        INX     H
        MOV     D,M             ; (DE) = DISPLACEMENT

        INX     H
        XTHL                    ; ((SP)) = RET, (HL) = TBL ADDRESS
        XCHG                    ; (DE) = TBL ADDRESS, (HL) = DISPLACEMENT
        DAD     D               ; (HL) = TARGET ADDRESS
        MOV     A,M
        INX     H
        MOV     H,M
        MOV     L,A             ; (HL) = ((HL))
        XCHG                    ; (DE) = VALUE, (HL) = TABLE ADDRESS
        RET
