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
        JZ      FAULT
        IN      0C2H
        BIT     7,A
        JRZ     PNT1
        POP     B
        MOV     A,C
        OUT     0C3H
        MVI     A,'4'
        OUT     0C2H
        MVI     A,'<'
        OUT    0C2H
        IN      0C3H
        RET
FAULT:
        POP     B
        CALL    STAT
        JMP     PRINT
;
STAT:   IN      0C3H
        BIT     3,A
        RNZ
        LXI     H,PFMSG
        CALL    OSTR
        CALL    CONIN
        RET
;
BOUND:
        CALL    LOADHL
        CALL    SPACE
        MVI     A,'T'
        CALL    PNT
        MVI     A,'0'
        CALL    PNT
        CALL    SPACE
        PUSH    H
        CALL    LOADHL
        MOV     D,H
        MOV     E,L
        POP     H
        CALL    LCR
        RET
;
LOADHL:
        PUSH    PSW
        CALL    TOBIN
        MOV     H,A
        CALL    TOBIN
        MOV     L,A
        POP     PSW
        RET
;
;       PRINT HL THEN : THEN MEMORY CONTENTS
;
POH:
        PUSH    PSW
        CALL    POH1
        MVI     A,':'
        CALL    PNT
        CALL    POH2
        POP     PSW
        RET
;
;       PRINT HL IN HEX-ASCII
;
POH1:
        PUSH    PSW
        MOV     A,H
        CALL    PHCHAR
        MOV     A,L
        CALL    PHCHAR
        POP     PSW
        RET
;
;       PRINTS M POINTED AT BY HL IN HEX-ASCII
;
POH2:
        PUSH    PSW
        MOV     A,M
        CALL    PHCHAR
        POP     PSW
        RET
;
;       CONVERTS BINARY VALUE IN A TO HEX-ASCII
;       AND PRINTS IT
;
PHCHAR:
        PUSH    B
        CALL    BHCONV
        MOV     A,B
        CALL    PNT
        MOV     A,C
        CALL    PNT
        POP     B
        RET
;
;       CONVERTS BYTE IN ACCUM TO TWO
;       HEX-ASCII DIGITS IN BC
;
BHCONV:
        PUSH    H
        MOV     L,A
        RAR
        RAR
        RAR
        RAR
        CALL    BIN1
        MOV     B,A
        MOV     A,L
        CALL    BIN1
        MOV     C,A
        POP     H
        RET
;
;       CONVERT THE BOTTOM NYBBLE OF THE BYTE IN A
;
BIN1:
        ANI     0FH
        ADI     '0'
        CPI     ':'
        RC
        ADI     7
        RET
;
;       CONVERT UP TO 4 HEX DIGITS TO BINARY
;
TOBIN:
     
