;;      DDATA. - ADD (0,A) TO (H,L)
;
;       ENTRY   NONE
;       EXIT    (H,L) = (H,L) + (0A)
;       USES    A,F,H,L

DDADA.  ADD     L
        MOV     L,A
        RNC
        INR     H
        RET
