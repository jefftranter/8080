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
        CALL    CLRSCR  ;0F3D5H
        PUSH    H
        LXI     H,SOMSG ;0F3D5H
        POP     H
        CALL    PINIT   ;0F18AH
;
;GET A COMMAND AND PARSE IT
;
PARSE:
