;;      DZERO - ZERO MEMORY
;
;       DZERO ZEROS A BLOCK OF MEMORY.
;
;       ENTRY   (HL) = ADDRESS
;               (B) = COUNT
;       EXIT    (A) = 0
;       USES    A,B,F,H,L


DZERO   XRA     A
ZRO1    MOV     M,A
        INX     H
        DCR     B
        JNZ     ZRO1            ; IF MORE
        RET
