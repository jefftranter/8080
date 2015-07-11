; TITLE '8080 system monitor, ver 1'
;
; July 11, 2015
;
TOP     EQU     24      ;MEMORY TOP, K BYTES
ORGIN   EQU     (TOP-2)*1024    ;PROGRAM START
;
        ORG     ORGIN
;
;
HOME    EQU     0       ;ABORT (VER 1-2)
;HOME   EQU     ORGIN   ;ABORT ADDRESS
VERS    EQU     '1'
STACK   EQU     ORGIN-$60
CSTAT   EQU     $10     ;CONSOLE STATUS
CDATA   EQU     CSTAT+1 ;CONSOLE DATA
CSTAT0  EQU     CSTAT   ;CON OUT STATUS
CDATA0  EQU     CSTAT0+1 ;OUT DATA
INMSK   EQU     1        ;INPUT MASK
OMSK    EQU     2        ;OUTPUT MASK
;
PORTN   EQU     STACK   ;3BYTES I/O
IBUFP   EQU     STACK+3 ;BUFFER POINTER
IBUFC   EQU     IBUFF+2 ;BUFFER COUNT
IBUFF   EQU     IBUFP+3 ;INPUT BUFFER
;
CTRH    EQU     8       ;^H BACKSPACE
TAB     EQU     9       ;^I
CTRQ    EQU     17      ;^Q
CTRS    EQU     19      ;^S
CTRX    EQU     24      ;^X, ABORT
BACKUP  EQU     CTRH    ;BACKUP CHAR
DEL     EQU     127     ;RUBOUT
ESC     EQU     27      ;ESCAPE
APOS    EQU     (39-'0') & $FF
CR      EQU     13       ;CARRIAGE RET
LF      EQU     10       ;LINE FEED
INC     EQU     $0DB     ;IN OP CODE
OUTC    EQU     $0D3     ;OUT OP CODE
RETC    EQU     $0C9     ;RET OP CODE
;
START:
        JMP     COLD    ;COLD START
RESTRT: JMP     WARM    ;WARM START
;
; CONSOLE INPUT ROUTINE
;
INPUTT: CALL    INSTAT  ;CHECK STATUS
        JZ      INPUTT  ;NOT READY ***
INPUT2: IN      CDATA   ;GET BYTE
        ANI     DEL
        CPI     CTRX    ;ABORT?
        JZ      HOME    ;YES
        RET
;
; GET CONSOLE-INPUT STATUS
;
INSTAT: IN      CSTAT
        ANI     INMSK
        RET
;
; CONSOLE OUTPUT ROUTINE
;
OUTT:   PUSH    PSW
OUT2:   CALL    INSTAT  ;INPUT?
        JZ      OUT4    ;NO ***
        CALL    INPUT2  ;GET INPUT
        CPI     CTRS    ;FREEZE?
        JNZ     OUT2    ;NO
;
; FREEZE OUTPUT UNTIL ^Q OR ^X
;
OUT3:   CALL    INPUTT  ; INPUT?
        CPI     QTRQ    ;RESUME?
        JNZ     OUT3    ;NO
        JMP     OUT2
;
OUT4:   IN      CSTATO  ;CHECK STATUS
        ANI     OMSK
        JZ      OUT2    ;NOT READY***
        POP     PSW
        OUT     CDATAO  ;SEND DATA
        RET
;
SIGNON: DB      CR,LF
        DB      ' Ver '
        DB      0
;
; CONTINUATION OF COLD START
;
COLD:   LXI     SP,STACK
        LXI     D,SIGNON ;MESSAGE
        CALL    SENDM   ;SEND IT
;
; WARM-START ENTRY
;
WARM:   LXI     H,WARM  ;RETURN HERE
