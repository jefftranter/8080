; First simple test program.
; From page 148 of "How to Build Your Own Self-Programming Robot" by
; David L. Heiserman.

        CPU     8085

        ORG     0000H

TEST:   NOP
        RST     0        ;RETURN TO TEST
