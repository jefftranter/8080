; Second simple test program.
; From page 149 of "How to Build Your Own Self-Programming Robot" by
; David L. Heiserman.

        CPU     8085

        ORG     0000H

PORT0   EQU     0800H

OTST:   LXI     H,PORT0
        MVI     A,0FFH
        MOV     M,A
        RST     0

        END
