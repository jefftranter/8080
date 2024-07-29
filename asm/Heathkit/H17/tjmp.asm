;;      DTJMP - TABLE JUMP.
;
;       USAGE
;
;       CALL    DTMP            (A) = INDEX
;       DW      ADDR1           INDEX = 0
;       .       .
;       .       .
;       .       .
;       DW      ADDRN           INDEX = N01
;
;       ENTRY   (A) = INDEX
;       EXIT    TO PROCESSOR
;               (A) = INDEX*2
;       USES    A,F


DTJMP   RLC                     ; (A) = INDEX*2

DTJMP.  EQU     $
        XTHL                    ; (HL) = TABLE ADDRESS
        PUSH    PSW             ; SAVE INDEX*2
        CALL    DDADA.
        MOV     A,M
        INX     H
        MOV     H,M
        MOV     L,A
        POP     PSW             ; (A) = INDEX*2
        XTHL                    ; ADDRESS IS ON STACK
        RET                     ; JUMP TO PROCESSOR
