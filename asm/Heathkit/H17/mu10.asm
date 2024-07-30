;;      DMU10 - MULTIPLY UNSIGNED 16 BIT QUANTITY BY 10.
;
;       (HL) = (DE)*10
;
;       ENTRY   (DE) = MULTIPLIER
;       EXIT    'C' CLEAR IF OK
;               (HL) = PRODUCT
;               'C' SET IF ERROR
;       USES    D,E,H,L,F


DMU10   XCHG                    ; (HL) = MULTIPLIER
        DAD     H               ; (HL) = X*2
        RC
        MOV     D,H
        MOV     E,L
        DAD     H               ; (HL) = X*4
        RC
        DAD     H               ; (HL) = X*8
        RC
        DAD     D               ; (HL) = X*10
        RET



