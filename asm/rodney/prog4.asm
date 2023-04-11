; The Alpha Rodney-One program.
; From page 170 of "How to Build Your Own Self-Programming Robot" by
; David L. Heiserman.

        CPU     8085

        ORG     0000H

PORT0   EQU     0800H
TOPS    EQU     03FFH
TSWR    EQU     0802H

        LXI     SP,TOPS  ;INITIALIZE STACK AT TOP
        LXI     H,PORT0  ;INITIALIZE HL FOR PORT-0 OPERATIONS
FETEN:  MOV     A,M
        CALL    RTLM    ;CALL "RUN TO LM"
        JZ      RURM    ;IF N, JUMP TP "RUN TO RM"
        CALL    LMRN    ;ELSE CALL "LM RUN"
        MOV     A,C     ;FETCH STALL FLAG
        ANI     10H     ;ISOLATE STALL FLAG
        JZ      FEED    ;IF N, JUMP TO "FEED"
RURM:   CALL    RTRM
        JNZ     RULM2   ;IF N, JUMP TO SECOND "RUN TO LM"
RMN:    CALL    RMRN    ;ELSE CALL "RM RUN"
        MOV     A,C     ;FETCH STALL FLAG
        ANI     10H     ;ISOLATE STALL FLAG
        JZ      FEED    ;IF N, JUMP TO "FEED"
        JMP     FETEN   ;ELSE RETURN TO "FETCH ENVL"
        NOP
FEED:   MOV     A,M     ;FETCH ENVL
        ANI     40H     ;ISOLATE FEED BIT
        JZ      FTR     ;IF N, JUMP TO "FETCH RAND"
        MVI     M,0F0H  ;"OUTPUT STOP ACT"
        JMP     FETEN   ;RETURN TO "FETCH ENVL"
FTR:    CALL    FETR
        JMP     FETEN   ;RETURN TO "FETCH ENVL"
RULM2:  CALL    RTLM
        JNZ     RMN     ;IF Y, JUMP TO "RM RUN"
        JMP     FEED    ;ELSE JUMP TO "FEED"
                        ;END

        ORG     0100H

RTLM:                   ;START WITH STATUS WORD IN REGISTER A
        MOV     B,A     ;SAVE A IN B
        ADD     A       ;SHIFT LEFT TO LINE UP LMR AND LMF
        XRA     B       ;EXOR TO COMPARE BIT VALUES
        ANI     02H     ;ISOLATE LM BIT
                        ;IF Y, THEN NZ
                        ;IF N, THEN Z
        RET             ;RETURN
                        ;END OF RTLM

        ORG     110H

RTRM:
                        ;START WITH STATUS WORD IN REGISTER a
        MOV     B,A     ;SAVE A IN B
        ADD     A       ;SHIFT LEFT TO LINE UP RMR AND RMF
        XRA     B       ;EXOR TO COMPARE BIT VALUES
        ANI     08H     ;ISOLATE RM BIT
                        ;IF Y, THEN NZ
                        ;IF N, THENZ
        RET             ;RETURN
                        ;END OF RTRM

        ORG     0150H

LMRN:
        MOV     A,M     ;"FETCH ENVL"
        ANI     0EFH    ;SET FSLL BIT TO ZERO
        MOV     M,A     ;"TURN ON FSLL"
        MVI     C,04H   ;INITIALIZE C COUNT AND LMS STATUS MEMORY
        MVI     D,08H   ;INITIALIZE D COUNT

CTIM:   CALL    TIME1
        MOV     A,M     ;"FETCH ENVL"
        XRA     C       ;EX-OR TO COMPARE BIT VALUES
        ANI     10H     ;ISOLATE LMS BIT
                        ;IF SAME, Z
                        ;IF NOT SAME, NZ
        JZ      DCONT   ;IF Y, THEN JUMP TP "D COUNT"
        XRA     C       ;ELSE RESTORE NEW LMS BIT
        MOV     C,A     ;"SAVE LMS"
        ANI     0FH     ;ISOLATE C COUNT
        DCR     A       ;DECREMENT A
        JZ      SETOK   ;IF Z, JUMP TO "SET OK"
        DCR     C       ;"C COUNT"
DCONT:  DCR     D
        JNZ     CTIM    ;JUMP BACK TO "TIME 1" IF N
SNOK:   MVI     C,00H
TOFL:   MOV     A,M
        ORI     0F0H    ;SET FSLL BIT TO ONE
        MOV     M,A     ;"TURN OFF FSLL"
        RET             ;RETURN-D4 OF C IS NZ
                        ;IF LEFT MOTOR IS TURNING
                        ;ELSE Z
SETOK:  MVI     C,10H
        JMP     TOFL    ;JUMP TO "TURN OFF FSLL"
                        ; END OF LMRN

        ORG     0250H

RMRN:
        MOV     A,M     ;"FETCH ENVL"
        ANI     0DFH    ;SET FSLR BIT TO ZERO
        MOV     M,A     ;"TURN ON FSLR"
        MVI     C,04H   ;INITIALIZE C COUNT AND RMS STATUS MEMORY
        MVI     D,08H   ;INITIALIZE D COUNT
CTIM2:  CALL    TIME1
        MOV     A,M     ;"FETCH ENVL"
        XRA     C       ;EX-OR TO COMPARE BIT VALUES
        ANI     20H     ;ISOLATE RMS BIT
                        ;IF SAME, Z
                        ;IF NOT SAME, NZ
        JZ      DCONT2  ;IF Y, THEN JUMP TO "D COUNT"
        XRA     C       ;ELSE RESTORE NEW RMS BIT
        MOV     C,A     ;"SAVE LMS"
        ANI     0FH     ;ISOLATE C COUNT
        DCR     A       ;DECREMENT A
        JZ      SETOK2  ;IF Z, JUMP TO "SET OK"
        DCR     C       ;"C COUNT"
DCONT2: DCR     D
        JNZ     CTIM2   ;IF N, JUMP BACK TO "TIME 1"
SNOK2:  MVI     C,00H
TOFL2:  MOV     A,M
        ORI     0F0H    ;SET FSLR BIT TO 1
        MOV     M,A     ;"TURN OFF FSLR"
        RET             ;RETURN - D4 OF C IS NZ
                        ;IF RIGHT MOTOR IS TURNING
                        ;ELSE Z
SETOK2: MVI     C,10H
        JMP     TOFL2   ;JUMP TO "TURN OFF FSLR"
                        ;END OF RMRN

        ORG     0300H

TIME1:
        MVI     B,0FFH  ;INITIALIZE HIGH-ORDER COUNT REGISTER
SETE:   MVI     E,0FFH
DCRE:   DCR     E       ;DECREMENT LOW-ORDER COUNT REGISTER
        JNZ     DCRE    ;IF NZ, DECREMENT E AGAIN
        DCR     B       ;ELSE DECREMENT B
        JNZ     SETE    ;IF NZ, JUMP TO LOW-ORDER COUNT CYCLE
        RET             ;ELSE RETURN
                        ;END OF TIME1

        ORG     0350H

FETR:
        LDA     TSWR    ;FETCH RANDOM NUMBER FROM PORT 2
                        ;BEGIN "VALID"
        MOV      C,A    ;SAVE TSWR IN C
        CALL     RTLM
        JNZ      ONAC   ;IF Y, THEN JUMP TO "OUTPUT NEW ACT"
        MOV      A,C    ;ELSE FETCH TSWR FROM C
        CALL     RTRM   ;CHECK RIGHT MOTOR CODE
        JNZ      ONAC   ;IF Y, THEN JUMP TO "OUTPUT NEW ACT"
        JMP      FETR   ;ELSE FETCH NEW RANDOM NUMBER
ONAC:   MOV      A,M
        ORI      0F0H   ;SET ACTL
        MOV      M,A    ;"OUTPUT NEW ACT"
        RET             ;RETURN

        END
