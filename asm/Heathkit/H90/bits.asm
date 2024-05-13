;;      BITS - BIT SET
;
;       BITS SETS THE SPECIFIED BIT IN THE ACCUMULATOR.
;
;       ENTRY:  A       - ORIGINAL A
;               B       - NUMBER OF BIT TO SET ( 7=HIGH,...,0=LOW )
;
;       EXIT:   A       - ORIGINAL  A  WITH  BIT(B) SET
;
;       USES:   PSW
;

BITS    PUSH    B

        PUSH    PSW
        MVI     A,10000000B
        INR     B
BITS1   RLC
        DCR     B
        JNZ     BITS1

        MOV     C,A
        POP     PSW
        ORA     C

        POP     B
        RET
