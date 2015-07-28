        CPU     8080
        ORG     0F00H
STACK   EQU     0E26EH
;
;
;       ZMON 6.2
;
;
        JMP     INIT
        JMP     CONIN
        JMP     CONOUT
        JMP     PRINT
        JMP     CONSTAT
        JMP     INIT
;
;       COLD START MONITOR
;
INIT:
        LXI     SP,STACK
        CALL    CLRSCR
        PUSH    H
        LXI     H,SOMSG
        CALL    OSTR
        POP     H
        CALL    PINIT
;
;GET A COMMAND AND PARSE IT
;
PARSE:  IN      1
        CALL    PROMPT
        CALL    CMD.
        CPI     'E'
        JZ      EXAM
        CPI     'M'
        JZ      DUMP
;
;       ERROR IN COMMAND
;
ERROR:
        MVI     A,'?'
        CALL    PNT
        JMP     PARSE
;
;       EXAMINE MEMORY
;
EXAM:
        CALL    EPROMPT
        CALL    LOADHL
EXMN:
        CALL    LFCR
        CALL    POH
        MVI     A,':'
        CALL    PNT
        CALL    CMD
        CPI     ' '
        JZ      NEXT
        CPI     'R'
        JZ      RUN
        CPI     0DH
        JZ      PARSE
        JMP     DEPOS
;
;       EXAMINE NEXT
;
NEXT:
        INX     H
        JMP     EXMN
;
;       RUN
;
RUN:
        PCHL
;
;       DEPOSIT
;
DEPOS:
        CALL    MORE
        MOV     M,A
        CALL    SPACE
        MOV     A,M
        CALL    PHCHAR
        INX     H
        JMP     EXAM
;
;
;       DUMP MEMORY
;
DUMP:
        CALL    EPROMPT
        CALL    BOUND
        CALL    CLRSCR
D0:     MVI     B,18H   ;SETB FOR 24 LINES ON A SCREEN
D1:     CALL    LFCR
        MVI     C,10H   ;SET C FOR 16 BYTES ACROSS
;
;       PRINT ADDRESS STARTING THIS LINE
;
        MOV     A,H
        CALL    PHCHAR
        MOV     A,L
        CALL    PHCHAR
;
        CALL    SPACE
PNTBYTE:
        MOV     A,M     ;GET THE BYTE FROM MEMORY
        CALL    PHCHAR  ;AND PRINT IT
        CALL    SPACE   ;AND A SPACE
;
;       ARE WE DONE
;
        MOV     A,H
        CMP     D
        JNZ     KEEPON  ;NOT DONE
        MOV     A,L
        CMP     E
        JZ      PARSE   ;DONE
;
KEEPON: INX     H
        DCR     C
        JZ      LINEDONE        ;THIS LINE DONE
;
;       IS HE BEATING ON THE KEYBOARD?
;
        IN      0
        RRC
        JNC     PARSE
;
        JMP     PNTBYTE
;
LINEDONE:
        DCR     B
        JNZ     D1      ;
        CALL    CMD
        MVI     B,10H
        JMP     D0
;
;       INIT PRINTER
;
PINIT:
        MVI     A,0
        OUT     0C0H    ;SET UP
        OUT     0C2H    ;DDRS
        OUT     0C1H    ;INPUT
        CMA
        OUT     0C3H    ;OUTPUT
        MVI     A,"="
        OUT     0C0H
        MVI     A,"="
        OUT     0C2H
        CALL    STAT
        RET
;
;       PRINT A CHARACTER
;
PRINT:
        PUSH    B
        MVI     B,64H   ;RETRY COUNT
PNT1:   DCR     B

