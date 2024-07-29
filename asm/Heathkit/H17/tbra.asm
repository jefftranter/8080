;;      DTBRA - BRANCH RELATIVE THROUGH TABLE.
;
;       DTBRA USES THE SUPPLIED INDEX TO SELECT A BYTE FROM THE
;       JUMP TABLE. THE CONTENTS OF THIS BYTE ARE ADDED TO THE
;       ADDRESS OF THE BYTE, YIELDING THE PROCESSOR ADDRESS.
;
;       CALL    DTBRA
;       DB      LAB1-*          ; INDEX = 0 FOR LAB1
;       DB      LAB2-*          ; INDEX = 0 FOR LAB2
;       DB      LABN-*          ; INDEX = 0 FOR LABN
;
;       ENTRY   (A) = INDEX
;               (RET) = TABLE FWA
;       EXIT    TO COMPUTED ADDRESS
;       USES    F,H,L


DTBRA   EQU     $
        XTHL                    ; (HL) = TABLE ADDRESS
        PUSH    D
        MOV     E,A
        MVI     D,0
        DAD     D               ; (HL) = ADDRESS OF ELEMENT
        MOV     E,M
        DAD     D               ; (HL) = PROCESSOR ADDRESS
        POP     D
        XTHL
        RET

;;      DTBLS - TABLE SEARCH
;
;       TABLE FORMAT
;
;       DB      KEY1,VAL1,
;       .       .
;       .       .
;       DB      KEYN,VALN
;       DB      0
;
;       ENTRY   (A) = PATTERN
;               (HL) = TABLE FWA
;       EXIT    (A) = PATTERN IF FOUND
;               'Z' SET IF FOUND
;       USES    A,F,H,L


DTBLS   PUSH    B
        MOV     B,A
DTBL1   MOV     A,M             ; (A) = CHARACTER
        CMP     B
        JZ      DTBL2           ; IF MATCH
        ANA     A
        INX     H
        INX     H               ; SKIP PAST
        JNZ     DTBL1           ; IF NOT END OF TABLE
        DCX     H
        DCX     H
        ORA     H               ; CLEAR 'Z'
        MVI     A,0             ; SET (A) = 0 FOR OLD USERS

;       DONE

DTBL2   POP     B
        INX     H
        RET

;;      DTYPTX - TYPE TEXT.
;
;       DTYPTX IS CALLED TO TYPE A BLOCK OF TEXT ON THE SYSTEM CONSOLE.
;
;       EMBEDDED ZERO BYTES INDICATE A CARRIAGE RETURN LINE FEED,
;       A BYTE WITH THE 200Q BIT SET IS THE LAST BYTE IN THE MESSAGE.
;
;       ENTRY   (RET) = TEXT
;       EXIT    TO (RET+LENGTH)
;       USES    A,F


DTYPTX  XTHL                    ; (HL) = TEXT ADDRESS
        CALL    DTYPTX.         ; TYPE IT
        XTHL
        RET

DTYPTX. MOV     A,M
        ANI     177Q
        DB      SYSCALL,SCOUT
        CMP     M
        INX     H
        JZ      DTYPTX.         ; MORE TO GO
        RET

