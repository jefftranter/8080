;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       DEMO: MTR-88
;
;       SYSTEM DEFINITIONS
;
ALARM   EQU     1136Q           ; MAKE NOISE
WCC     EQU     1702Q           ; WRITE CHAR TO CONSOLE
MFLAG   EQU     20010Q          ; USER FLAG OPTIONS
UIVEC   EQU     20037Q          ; USER INTERRUPT VECTOR
UO.CLK  EQU     1Q              ; ALLOW CLOCK INTERRUPT PROCESSING
MI.JMP  EQU     303Q            ; MACHINE INSTRUCTION JUMP
IOB     EQU     1466Q           ; INPUT OCTAL BYTE
TOB     EQU     2743Q           ; TYPE OCTAL BYTE

ESC     EQU     33Q
CR      EQU     15Q
LF      EQU     12Q

        ORG     20100Q

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       ERASE SCREEN
;
MTR88   MVI     A,ESC           ; ESCAPE SEQUENCE TO
        CALL    WCC
        MVI     A,'E'           ;     ERASE SCREEN
        CALL    WCC

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       READ A OCTAL INTEGER FROM KEYBOARD
;          STORE THE NUMBER.
;
        LXI     H,NUMBER        ; GET ADDRESS OF NUMBER
        ANA     A               ; CLEAR CARRY (SIDE EFFECT)
        CALL    IOB             ; INPUT OCTAL BYTE
        CALL    SETICK          ; SETUP TICK TO 500 FOR ONE SEC

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       INITIALIZE SERVICE INTERRUPT ROUTINE
;          LOAD THE USER INTERRUPT VECTOR (UIVEC) WITH A
;          JUMP INSTRUCTION AND THE ADDRESS OF THE SERVICE
;          ROUTINE.  ENABLE USER CLOCK INTERRUPT!
;
        MVI     A,MI.JMP        ; SET-UP JUMP INSTRUCTION
        STA     UIVEC           ; STORE 'JMP' INSTRUCTION
        LXI     H,INTRP         ;    USER INTERRUPT ADDRESS
        SHLD    UIVEC+1         ;       POSITIONED
        MVI     A,UO.CLK
        STA     MFLAG           ; ENABLE CLOCK INTERRUPT

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       WAIT FOR CLOCK TO REACH ZERO
;
LOOP    LDA     NUMBER          ; DO NOTHING LOOP.
        CPI     0               ;    WAIT FOR END
        JNZ     LOOP            ;    OF COUNT DOWN.

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       RETURN TO NORMAL INTERRUPT STATUS & HALT
;          DISABLE INTERRUPT & TURN ON SPEAKER
;
        MVI     A,0
        STA     MFLAG           ; DISABLE CLOCK INTERRUPT
        CALL    ALARM
        HLT

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       INTERRUPT ROUTINE
;          CLOCK AND DISPLAY INTERRUPT
;
INTRP   LHLD    TICK            ; GET COUNT (BETWEEN 0 & 500)
        DCX     H               ; TICK=TICK-1
        SHLD    TICK            ; STORE COUNT
        MOV     A,L             ; TEST FOR ZERO
        ORA     H               ;    COMPARE WITH 'H'
        RNZ                     ;    EXIT IF .NE. 0

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       UPDATE DISPLAY FOR 'NEW' NUMBER.
;
        MVI     A,CR            ; DO CARRIAGE RETURN
        CALL    WCC
        MVI     A,LF            ;    AND LINE FEED
        CALL    WCC
        LDA     NUMBER          ; GET NUMBER
        DCR     A               ; NUMBER=NUMBER-1
        STA     NUMBER          ; SAVE NUMBER
        CALL    TOB             ; TYPE OCTAL BYTE
SETICK  LXI     H,500           ; RESTORE COUNT
        SHLD    TICK            ;   WITH 500
        RET

;       STORAGE AREA & END ASSEMBLY
TICK    DS      2
NUMBER  DS      1
        END
