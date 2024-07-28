;;      DCOMP - COMPARE TO CHARACTER STRINGS
;
;       DCOMP COMPARES TWO BYTE STRINGS.
;
;       ENTRY   (C) = COMPARE COUNT
;               (DE) = FWA OF STRING #1
;               (HL) = FWA OF STRING #2
;       EXIT    'Z' CLEAR, IF MIS-MATCH
;               (C) = LENGTH REMAINING
;               (DE) = ADDRESS OF MISMATCH IN STRING #1
;               (HL) = ADDRESS OF MISMATCH IN STRING #2
;               'C' SET, HAVE MATCH
;               (C) = 0
;               (DE) = (DE) + (OC)
;               (HL) = (HL) + (OC)
;       USES    A,F,C,D,E,H,L

DCOMP   LDAX    D
        CMP     M               ; COMPARE
        RNZ                     ; NO MATCH
        INX     D
        INX     H
        DCR     C
        JNZ     DCOMP           ; TRY SOME MORE
        RET
