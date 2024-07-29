;;      DRSTALL - RESTORE ALL REGISTERS.

;
;       DRSTALL RESTORES ALL THE REGISTERS OFF THE STACK, AND
;       RETURNS TO THE PREVIOUS CALLER.
;
;       ENTRY   (SP) = PWS
;               (SP+2) = BC
;               (SP+4) = DE
;               (SP+6) = HL
;               (SP+8) = RET
;       EXIT    TO *RET*, REGISTERS RESTORED
;       USES    ALL


DRSTALL POP     PSW
        POP     B
        POP     D
        POP     H
        RET

;;      DSAVALL - SAVE ALL REGISTERS ON STACK.
;
;       DSAVALL SAVES ALL THE REGISTERS ON THE STACK.
;
;       ENTRY   NONE
;       EXIT    (SP) = PSW
;               (SP+2) = BC
;               (SP+4) = DE
;               (SP+6) = HL
;       USES    H,L


DSAVALL XTHL                    ; PUSH H, (HL) = RETURN ADDRESS
        PUSH    D
        PUSH    B
        PUSH    PSW
        PCHL                    ; RETURN TO CALLER
