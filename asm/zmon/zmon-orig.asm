        CPU     8080
        ORG     0F000H
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
        CALL    CMD
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
        JMP     EXMN
;
;
;       DUMP MEMORY
;
DUMP:
        CALL    EPROMPT
        CALL    BOUND
        CALL    CLRSCR
D0:     MVI     B,18H   ;SET B FOR 24 LINES ON A SCREEN
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
        CPU     Z80
        BIT     7,A     ;Note: Z80 opcode!
        CPU     8080
        CPU     Z80
        JR      Z,PNT1  ;Note: Z80 opcode!
        CPU     8080
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
        CPU     Z80
        BIT     3,A     ;Note: Z80 opcode!
        CPU     8080
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
        MVI     A,'O'
        CALL    PNT
        CALL    SPACE
        PUSH    H
        CALL    LOADHL
        MOV     D,H
        MOV     E,L
        POP     H
        CALL    LFCR
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
        CALL    INECHO
        CPI     0DH
        JNZ     MORE
        LXI     SP,STACK
        JMP     PARSE
MORE:
        PUSH    H
        LXI     H,0
        CALL    CHECK
        CALL    ATOB
        ADD     A         
        ADD     A         
        ADD     A         
        ADD     A         
        MOV     L,A
        CALL    INECHO
        CALL    CHECK
        CALL    ATOB
        ANI     0FH
        ORA     L
        POP     H
        RET
;
;       CHECK FOR LEGAL HEX-ASCII IN A
;       RETURN TO COMMAND MODE IF NOT
;
CHECK:
        CPI     '0'
        JC      CHERROR
        CPI     'A'
        JC      NUMBER
        CPI     'G'
        JNC     CHERROR
        RET
NUMBER:
        CPI     ':'
        JNC     CHERROR
        RET
CHERROR:
        LXI     SP,STACK
        JMP     ERROR
;
;       CONVERT ASCII DIGIT TO BINARY
;
ATOB:
        SUI     '0'
        CPI     0AH
        RC
        SUI     7
        RET
;
;       PRINT AN ASCII SPACE
;
SPACE:
        PUSH    PSW
        MVI     A,' '
        CALL    PNT
        POP     PSW
        RET
;
;       PRINT MONITOR PROMPT
;
PROMPT:
        PUSH    PSW
        CALL    LFCR
        MVI     A,':'
        CALL    PNT
        POP     PSW
        RET
;
;       PRINT THE > PROMPT
;
EPROMPT:
        PUSH    PSW
        CALL    LFCR
        MVI     A,'>'
        CALL    PNT
        POP     PSW
        RET
;
; PRINT LF,CR
;
LFCR:
        PUSH    PSW
        MVI     A,0AH
        CALL    PNT
        MVI     A,0DH
        CALL    PNT
        POP     PSW
        RET
;
;       INPUT ROUTINE THAT RETURNS TO THE
;       MONITOR ON A CR - ECHOS CHARACTER
;
CMD:
        CALL    CONIN
        CPI     0DH
        JZ      PARSE
;
;       PRINT TO THE CONSOLE
;
PNT:
        PUSH    PSW
PRDY:
        IN      0
        RLC
        JC      PRDY
        POP     PSW
        OUT     1
        RET
;
;       CONSOLE OUT - CP/M COMPATABLE
;
CONOUT:
        MOV     A,C
        JMP     PNT
;
;       GET CHAR FROM CONSOLE AND ECHO IT
;
INECHO:
        CALL    CONIN
        CALL    PNT
        RET
;
;       CONSOLE IN
;
CONIN:
        IN      0
        RRC
        JC      CONIN
        IN      1
        ANI     7FH
        RET
;
;       STRING PRINTER
;       PRINTS TILL A BYTE OF FF IS FOUND
;
OSTR:
        PUSH    PSW
OST1:   MOV     A,M
        CPI     0FFH
        JZ      OST2
        CALL    PNT
        INX     H
        JMP     OST1
OST2:   POP     PSW
        RET
;
;       CLEAR THE TERMINAL SCREEN
;
CLRSCR:
        PUSH    H
        LXI     H,CLRMSG
        CALL    OSTR
        POP     H
        RET
;
;       GET CONSOLE STATUS CP/M COMPATABLE
;
CONSTAT:
        IN      0
        RRC
        MVI     A,0
        RC
        MVI     A,0FFH
        RET
;
CLRMSG:
        DB      1BH,'E',0FFH
SOMSG:
        DB      "Z-MON  V5L3",0FFH
PFMSG:
        DB      0DH,0AH,"FAULT",0FFH
;
        END
