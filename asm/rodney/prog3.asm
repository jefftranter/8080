; Third simple test program.
; From page 151 of "How to Build Your Own Self-Programming Robot" by
; David L. Heiserman.

        CPU     8085

        ORG     0000H

PORT0   EQU     0800H

BLKT:
        LXI     H,PORT0  ;SET PORT POINTER TO PORT 0
STA:    XRA     A        ;CLEAR A TO 0'S
MOA:    MOV     M,A
        MVI     B,0FFH   ;INITIALIZE B
INC:    MVI     C,0FFH
IND:    MVI     D,0FFH

DCD:    DCR     D
        JNZ     DCD      ;IF NOT ZERO, JUMP TO DCD
        DCR     C        ;ELSE DECREMENT C
        JNZ     IND      ;IF NOT ZERO, JUMP BACK TO IND
        DCR     B        ;ELSE DECREMENT B
        JNZ     INC      ;IF NOT ZERO, JUMP BACK TO INC
        CPI     0FFH     ;ELSE COMPARE WITH 1'S
                         ;IF ALL 1'S, Z
                         ;IF 0'S, BZ
        JZ      STA      ;IF ALL 1'S, JUMP BACK TO STA

        MVI     A,0FFH   ;ELSE SET A TO ALL 1'S
        JMP     MOA      ;AND JUMP BACK TO MOA
                         ;END OF BLKT

        END
