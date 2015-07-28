        CPU     8080
        ORG     0F00H
STACK   EQU     0E26EH
;
;
;       ZMON 6.2
;
;
        JMP     INIT    ;0F021H
        JMP     CONIN   ;0F389H
        JMP     CONOUT  ;0F37EH
        JMP     PRINT   ;0F1A1H
        JMP     CONSTAT ;0F3ADH
        JMP     INIT    ;0F021H
;
;       COLD START MONITOR
;
INIT:           ;F021
        LXI     SP,STACK        ;0E26EH
        CALL    CLRSCR  ;0F3A4H
        PUSH    H
        LXI     H,SOMSG ;0F3D5H
        CALL    OSTR    ;0F394H
        POP     H
        CALL    PINIT   ;0F18AH
;
;GET A COMMAND AND PARSE IT
;
PARSE:  IN      1
        CALL    PROMPT  ;0F348H
        CALL    CMD.    ;0F36BH
        CPI     'E'     ;45H
        JZ      EXAM    ;0F070H
        CPI     'M'     ;4DH
        JZ      DUMP    ;0F146H
;
;       ERROR IN COMMAND
;
ERROR:          ;F067
        MVI     A,'?'   ;3FH
        CALL    PNT     ;0F373H
        JMP     PARSE   ;0F032H
;
;       EXAMINE MEMORY
;
EXAM:           ;F070
        CALL    EPROMPT ;0F353H
        CALL    LOADHL  ;0F1F0H
EXMN:           ;F076
        CALL    LFCR    ;0F35EH
        CALL    POH     ;0F1FBH
        MVI     A,':'   ;3AH
        CALL    PNT     ;0F373H
        CALL    CMD     ;0F36BH
        CPI     ' '     ;20H
        JZ      NEXT    ;0F096H
        CPI     'R'     ;52H
        JZ      RUN     ;0F09AH
        CPI     0DH
        JZ      PARSE   ;0F032H
        JMP     DEPOS   ;0F09BH
;
;       EXAMINE NEXT
;
NEXT:
