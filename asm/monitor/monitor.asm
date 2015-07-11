; TITLE '8080 system monitor, ver 1'
;
; July 11, 2015
;
TOP     EQU     24      ;MEMORY TOP, K BYTES
ORGIN   EQU     (TOP-2)*1024  ;PROGRAM START
;
        CPU     8080
        ORG     ORGIN
;
;
HOME    EQU     0       ;ABORT (VER 1-2)
;HOME   EQU     ORGIN   ;ABORT ADDRESS
VERS    EQU     '1'
STACK   EQU     ORGIN-60H
CSTAT   EQU     10H     ;CONSOLE STATUS
CDATA   EQU     CSTAT+1 ;CONSOLE DATA
CSTATO  EQU     CSTAT   ;CON OUT STATUS
CDATAO  EQU     CSTATO+1 ;OUT DATA
INMSK   EQU     1        ;INPUT MASK
OMSK    EQU     2        ;OUTPUT MASK
;
PORTN   EQU     STACK   ;3BYTES I/O
IBUFP   EQU     STACK+3 ;BUFFER POINTER
IBUFC   EQU     IBUFP+2 ;BUFFER COUNT
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
APOS    EQU     (39-'0') & 0FFH
CR      EQU     13      ;CARRIAGE RET
LF      EQU     10      ;LINE FEED
INC     EQU     0DBH    ;IN OP CODE
OUTC    EQU     0D3H    ;OUT OP CODE
RETC    EQU     0C9H    ;RET OP CODE
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
OUT3:   CALL    INPUTT  ;INPUT?
        CPI     CTRQ    ;RESUME?
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
        DB      " Ver "
        DW      VERS
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
        PUSH    H
        CALL    CRLF    ;NEW LINE
        CALL    INPLN   ;CONSOLE LINE
        CALL    GETCH   ;GET CHAR
        CPI     'D'     ;DUMP
        JZ      WARM    ;(VER 1-2)
;       JZ      DUMP    ;HEX/ASCII (2)
        CPI     'C'     ;CALL
        JZ      WARM    ;(VER 1-2)
;       JZ      CALLS   ;SUBROUTINE (3)
        CPI     'G'     ;GO
        JZ      WARM    ;(VER 1-2)
;       JZ      GO      ;SOMEWHERE (3)
        CPI     'L'     ;LOAD
        JZ      WARM    ;(VER 1-3)
;       JZ      LOAD    ;INTO MEMORY (4)
        JMP     WARM    ;TRY AGAIN
;
; INPUT A LINE FROM CONSOLE AND PUT IT
; INTO THE BUFFER. CARRIAGE RETURN ENS
; THE LINE. RUBOUT OR ^H CORRECTS LAST
; LAST ENTRY. CONTROL-X RESTARTS LINE.
; OTHER CONTROL CHARACTERS ARE IGNORED.
;
INPLN:  MVI     A,'>'   ;PROMPT
        CALL    OUTT
INPL2:  LXI     H,IBUFF ;BUFFER ADDR
        SHLD    IBUFP   ;SAVE POINTER
        MVI     C,0     ;COUNT
INPLI:  CALL    INPUTT  ;CONSOLE CHAR
        CPI     ' '     ;CONTROL?
        JC      INPLC   ;YES
        CPI     DEL     ;DELETE
        JZ      INPLB   ;YES
        CPI     'Z'+1   ;UPPER CHAR?
        JC      INPL3   ;YES
        ANI     5FH     ;MAKE UPPER
INPL3:  MOV     M,A     ;INTO BUFFER
        MVI     A,32    ;BUFFER SIZE
        CMP     C       ;FULL?
        JZ      INPLI   ;YES. LOOP
        MOV     A,M     ;GET CHAR
        INX     H       ;INCR POINTER
        INR     C       ;AND COUNT
INPLE:  CALL    OUTT    ;SHOW CHAR
        JMP     INPLI   ;NEXT CHAR
;
; PROCESS CONTROL CHARACTER
;
INPLC:  CPI     CTRH    ;^H?
        JZ      INPLB   ;YES
        CPI     CR      ;RETURN:
        JNZ     INPLI   ;NO, IGNORE
;
; END OF INPUT LINE
;
        MOV     A,C     ;COUNT
        STA     IBUFC   ;SAVE
;
; CARRIAGE-RETURN, LINE-FEED ROUTINE
;
CRLF:   MVI     A,CR
        CALL    OUTT    ;SEND CR
        MVI     A,LF
        JMP     OUTT    ;SEND LF
;
; DELETE PRIOR CHARACTER IF ANY
;
INPLB:  MOV     A,C     ;CHAR COUNT
        ORA     A       ;ZERO?
        JZ      INPLI   ;YES
        DCX     H       ;BACK POINTER
        DCR     C       ;AND COUNT
        MVI     A,BACKUP ;CHARACTER
        JMP     INPLE   ;SEND
;
; GET A CHARACTER FROM CONSOLE BUFFER
; SET CARRY IF EMPTY
;
GETCH:  PUSH    H       ;SAVE REGS
        LHLD    IBUFP   ;GET POINTER
        LDA     IBUFC   ;AND COUNT
        SUI     1       ;DECR WITH CARRY
        JC      GETC4   ;NO MORE CHAR
        STA     IBUFC   ;SAVE NEW COUNT
        MOV     A,M     ;GET CHARACTER
        INX     H       ;INCR POINTER
        SHLD    IBUFP   ;AND SAVE
GETC4:  POP     H       ;RESTORE REGS
        RET
;
; SEND ASCII MESSAGE UNTIL BINARY ZERO
; IS FOUND. POINTER IS D,E
;
SENDM:  LDAX    D       ;GET BYTE
        ORA     A       ;ZERO?
        RZ              ;YES, DONE
        CALL    OUTT    ;SEND IT
        INX     D       ;POINTER
        JMP     SENDM   ;NEXT
;
        END
