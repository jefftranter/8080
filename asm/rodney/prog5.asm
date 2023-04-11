; Memory diagnostic program.
; From page 185 of "How to Build Your Own Self-Programming Robot" by
; David L. Heiserman.

        CPU     8085

        ORG     0000H

TOPS    EQU     033FH
TCOD    EQU     0200h
TOPMM   EQU     0FFFFH
PORT0   EQU     0800H
PORT4   EQU     0804H
PORT5   EQU     0805H
PORT6   EQU     0806H

MMDIG:
        LXI     SP,TOPS ;SET STACK POINTER
        LXI     H,PORT0
        MVI     M,00H   ;TURN OFF MOTOR, STATUS LAMPS
        LXI     H,TCOD
        MOV     D,M     ;FETCH DATA CODE TO BE TESTED
        LXI     B,TOPMM ;INITIALIZE MMA POINTER
START:  LXI     H,PORT4 ;INITIALIZE PORT POINTER
        CALL    ADRK
        CNZ     NOTOK   ;IF BAD CHECK, CALL "LIGHT ACT LAMPS" AND HALT
        CALL    DECRAD  ;ELSE SET NEXT MMA
        CZ      ISOK    ;IF TEST IS DONE, CALL "FLASH ACT LAMPS"
        JMP     START   ;ELSE CYCLE AGAIN
                        ;END OF MAIN PROGRAM

        ORG     0100H

ADRK:
        MOV     M,C     ;LOAD MMAL AT PORT 4
        MOV     A,C     ;FETCH MMAL FROM MMAL POINTER
        CMP     M       ;COMPARE MMAL WITH ORIGINAL
        RNZ             ;RETURN WITH NZ IF NOT SAME
        INR     L       ;CHANGE PORT POINTER TO PORT 5
        MOV     M,B     ;LOAD MMAH AT PORT 5
        MOV     A,B     ;FETCH MMAH FROM MMAH POINTER
        CMP     M       ;COMPARE MMAH WITH ORIGINAL
        RNZ             ;RETURN WITH NZ IF NOT SAME
        INR     L       ;ELSE ADVANCE PORT POINTER
        MOV     A,D     ;FETCH DATA TO BE LOADED
        MOV     M,A     ;LOAD DATA AT PORT 6
        CMP     M       ;COMPARE MMD WITH ORIGINAL
        RNZ             ;RETURN WITH NZ IF NOT SAME
        RET             ;ELSE RETURN WITH Z

        ORG     0150H

NOTOK:
        LXI     H,PORT0 ;POINT TO PORT 0
        MVI     M,0FH   ;SET FOR "LIGHT ACT LAMPS"
        HLT             ;HALT THE TEST UNTIL MANUALLY RESET

        ORG     0250H

ISOK:
        XRA     A       ;CLEAR REGISTER A
        LXI     H,PORT0 ;POINT TO PORT 0
SETE:   MVI     E,08H   ;INITIALIZE E COUNTER
SETB:   MVI     B,0FFH  ;INITIALIZE B COUNTER
SETC:   MVI     C,0FFH  ;INITIALIZE C COUNTER
DECC:   DCR     C       ;DECREMENT C COUNTER
        JNZ     DECC    ;IF NOT ZERO, DECREMENT AGAIN
        DCR     B       ;ELSE DECREMENT B COUNTER
        JNZ     SETC    ;IF NOT ZERO, RESET C COUNTER AND DO AGAIN
        DCR     E       ;ELSE DECREMENT E COUNTER
        JNZ     SETB    ;IF NOT ZERO, RESET B COUNTER AND DO AGAIN
        CMA             ;ELSE COMPLEMENT A
        MOV     M,A     ;CHANGE LAMP STATUS
        JMP     SETE    ;AND REPEAT ENTIRE CYCLE
                        ;END OF ISOK
        ORG     0200H

DECRAD:
        DCR     C       ;DECREMENT MMAL POINTER
        RNZ             ;IF NOT ZERO, RETURN WITH NZ
        DCR     B       ;ELSE DECREMENT MMAH POINTER
        RZ              ;IF ZERO, RETURN WITH Z
        MVI     B,0FFH  ;ELSE RESET MMAL POINTER
        RET             ;AND RETURN WITH NZ
                        ;END OF DECRAD

        END
