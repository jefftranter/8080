;;      DDADA - PERFORM (H,L) = (H,L) + (0,A)
;
;       ENTRY   (H,L) = BEFORE VALUE
;               (A) = BEFORE VALUE
;       EXIT    (H,L) = (H,L) + (0,A)
;               'C' SET IF OVERFLOW
;       USES    F,H,L

DDATA   PUSH    D
        MOV     E,A
        MVI     D,0
        DAD     D
        POP     D
        RET                     ; EXIT
