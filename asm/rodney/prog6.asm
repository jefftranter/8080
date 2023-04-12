; Beta Rodney 1 program.
; From page 195 of "How to Build Your Own Self-Programming Robot" by
; David L. Heiserman.

        CPU     8085

        ORG     0000H

MMALT   EQU     0FFH
TOPS    EQU     033FH
PORT0   EQU     0800H
TSWR    EQU     0802H
PORT4   EQU     0804H
PORT6   EQU     0806H

BETA1:
        LXI     SP,TOPS ;SET STACK POINTER TO TOP
        CALL    CLRM    ;"CLEAR MM"
        LXI     H,PORT0 ;SET PORT POINTER TO PORT 0

FETEN:  CALL    RUAPX
        MOV     A,M     ;FETCH ENVL
        JNZ     SSB1    ;IF "OK" IS Y, JUMP TO "SET STALL TO 1"
        ANI     0FH     ;ELSE SET "STALL BITS TO 0"
        JMP     AMM     ;AND JUMP TO "ADDRESS MM"
SSB1:   ORI     30H     ;SET STALL BITS TO 1
AMM:    STA     PORT4
SAVD:   STA     PORT6
        MOV     B,A     ;SAVE MMD IN B
        ANI     0C0H    ;ISOLATE CONL BITS
        JZ      FEDUP   ;IF "BEFORE" IS N, JUMP TO "FEED"
        MOV     A,B     ;ELSE FETCH MMD FROM B
        ORI     0F0H    ;CONDITION ACTL
        MOV     M,A     ;AND "OUT MMD AS ACT"
        CALL    RUAPX   ;CALL "RUN ALPHA-PLEX"
        JZ      RMMD    ;IF "OK" IS N, JUMP TO "READ MMD"
        LDA     PORT6   ;ELSE FETCH MMD
        ANI     0C0H    ;ISOLATE CONL BITS
        CPI     0C0H    ;"CONL=3" - Z IF YES, NZ IF NO
        JZ      FETEN   ;IF YES, LOOP BACK TO START OF CYCLE
        ADI     40H     ;ELSE "INR CONL"
        MOV     C,A     ;SAVE CONL IN C
        LDA     PORT6   ;FETCH MMD
        ANI     3FH     ;SET CONL BITS TO ZERO
        ORA     C       ;COMPOSE NEW MML WORD
SAMMD:  STA     PORT6   ;"STORE AS MMDL"
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        JMP     FETEN   ;LOOP BACK TO START OF CYCLE
RMMD:   LDA     PORT6   ;FETCH MMD
        SUI     40H     ;"DOR CONL"
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        JMP     SAVD    ;JUMP BACK TO "BEFORE"
FEDUP:  LDA     PORT4   ;FETCH MMA/ENVL
        ANI     80H     ;ISOLATE FEED BIT
                        ;Z IF YES, NZ IF NO
        JNZ     FETPL   ;IF NO, JUMP TO "FETCH PLEX"
        XRA     A       ;CLEAR ACCUMULATOR
        JMP     OUACT   ;JUMP TO "OUTPUT AS ACTL"
FETPL:  CALL    FETR
OUACT:  MOV     M,A     ;"OUTPUT AS ACTL"
        ORI     80H     ;"SET CONL=1"
        JMP     SAMMD   ;JUMP TO "STORE AS MMDL:
                        ;END OF BETA 1

        ORG     0100H

CLRM:
        MVI     B,MMALT ;SET MMA POINTER TO TOP ADDRESS
        XRA     A       ;CLEAR TO ALL ZEROS
        LXI     H,PORT4 ;SET PORT POINTER TO MMAL
NEWAD:  MOV     M,B
        LXI     H,PORT6 ;SET PORT POINTER TO MMDL
        MOV     M,A     ;MOVE 0;S TO MMD
        DCR     B       ;DECREMENT MMA POINTER
        JNZ     NEWAD   ;IF NOT ZERO, DECREMENT TO A NEW ADDRESS
        MOV     M,A     ;ELSE CLEAR BOTTOM MMA
        RET             ;RETURN TO MAIN PROGRAM
                        ;END OF CLRM

        ORG     0120H

FETR:
STRT:   LDA     TSWR
        MOV     C,A
        CALL    RTLM
        RNZ
        MOV     A,C
        CALL    RTRM
        RNZ
        JMP     STRT

        ORG     03A0H

TIME1:
        MVI     B,0FFH
SETE:   MVI     E,0FFH
DCRE:   DCR     E
        JMP     DCRE
        DCR     B
        JNZ     SETE
        RET

        ORG     0150H

RUAPX:
        MOV     A,M     ;FETCH NEW ENVL
        CALL    RTLM
        JZ      RURM
        CALL    LMRN
        MOV     A,C
        ANI     10H
        JZ      FEED
RURM:   CALL    RTRM
        JNZ     RULM2
RMN:    CALL    RMRN
        MOV     A,C
        ANI     10H
        JZ      FEED
        RET             ;RETURN WITH Y AS NZ
FEED:   MOV     A,M
        ANI     40H
        RET             ;RETURN WITH Y AS NZ OR N AS Z

RULM2:  CALL    RTLM
        JNZ     RMN
        JMP     FEED
        RST     0       ;RESET ENTIRE PROGRAM IF THINGS BLOW UP
                        ;END OF RUAPX

        ORG     0200H

RTLM:   MOV     B,A
        ADD     A
        XRA     B
        ANI     02H
        RET
                        ;END OF RTLM

        ORG     0225H

RTRM:
        MOV     B,A
        ADD     A
        XRA     B
        ANI     08H
        RET
                        ;END OF RTRM

        ORG     0250H

LMRN:
        MOV     A,M
        ANI     0EFH
        MOV     M,A
        MVI     C,04H
        MVI     D,08H
CTIM:   CALL    TIME1
        MOV     A,M
        XRA     C
        ANI     10H
        JZ      DCONT
        XRA     C
        MOV     C,A
        ANI     0FH
        DCR     A
        JZ      SETOK
        DCR     C
DCONT:  DCR     D
        JNZ     CTIM
SNOK:   MVI     C,00H
TOFL:   MOV     A,M
        ORI     0F0H
        MOV     M,A
        RET
SETOK:  MVI     C,10H
        JMP     TOFL
                        ;END OF LMRN

RMRN:
        MOV     A,M
        ANI     0DFH
        MOV     M,A
        MVI     C,04H
        MVI     D,08H
CTIM2:  CALL    TIME1
        MOV     A,M
        XRA     C
        ANI     20H
        JZ      DCONT2
        XRA     C
        MOV     C,A
        ANI     0FH
        DCR     A
        JZ      SETOK2
        DCR     C
DCONT2: DCR     D
        JNZ     CTIM2
SNOK2:  MVI     C,00H
TOFL2:  MOV     A,M
        ORI     0F0H
        MOV     M,A
        RET
SETOK2: MVI     C,10H
        JMP     TOFL2
                        ;END OF RMRN

        END
