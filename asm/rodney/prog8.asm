; Cassette record - ROM version.
; From page 224 of "How to Build Your Own Self-Programming Robot" by
; David L. Heiserman.

        CPU     8085

        ORG     0400H

HMMAX   EQU     03H
TOPS    EQU     033FH
PORT0   EQU     0800H


RECO:   LXI     SP,TOPS ;SET STACK POINTER TO TOP OF RAM SPACE
        LXI     H,0000H ;INITIALIZE MEMORY POINTER
CBKRD:  CALL    BKRCD   ;CALL RECORD SEQUENCE
        INR     H       ;INCREMENT H COUNTER
        CPI     HMMAX
        JNZ     CBKRD   ;IF NOT DONE, JUMP BACK TO RECORD SEQUENCE
        CALL    FLASH   ;ELSE CALL FLASH TO SIGNAL END OF RECORDING

BKRCD:  MVI     C,0FAH  ;SET LEADER BURST LENGTH
        MVI     A,0C0H
BR1:    CALL    BURST   ;OUTPUT TONE
        DCR     C
        JNZ     BR1     ;SUSTAIN LEADER TONE
        XRA     A       ;CLEAR ACCUMULATOR
        CALL    BURST
BR2:    MOV     C,M     ;FETCH DATA BYTE TO BE RECORDED
        CALL    CASO    ;OUTPUT BYTE TO RECORDER
        INR     L       ;POINT TO NEXT BYTE
        JNZ     BR2
        RET
                        ;END OF BKRCD SUBROUTINE

        ORG     0430H

BURST:  MVI     D,10H   ;SET NUMBER OF CYCLES
BU1:    SIM
        MVI     E,1EH
BU2:    DCR     E       ;REGULATE TONE FREQUENCY
        JNZ     BU2
        XRI     80H     ;COMPLEMENT SOD DATA BIT
        DCR     D
        JNZ     BU1     ;CONTINUE UNTIL BURST OR PAUSE IS FINISHED
        RET             ;RETURN
                        ;END OF BURST SUBROUTINE

CASO:   DI              ;DISABLE INTERRUPTS
        PUSH    D       ;SAVE D IN STACK
        MVI     B,09H   ;SET BYTE SIZE
CO1:    XRA     A       ;CLEAR ACCUMULATOR
        MVI     A,0C0H  ;SET A FOR TONE BURST
        CALL    BURST
        MOV     A,C     ;FETCH NEXT BIT TO BE RECORDED
        RAR             ;ROTATE TO SOD POSITION
        MOV     C,A     ;MAKE CARRY BIT THE SOD ENABLE
        MVI     A,01H
        RAR
        RAR
        CALL    BURST
        XRA     A       ;CLEAR A
        CALL    BURST
        DCR     B       ;DECREMENT BIT COUNT
        JNZ     CO1     ;REPEAT UNTIL ALL BITS IN THE BYTE ARE RECORDED
        POP     D       ;RECOVER D STATUS FROM THE STACK
        EI              ;ENABLE INTERRUPTS
        RET             ;RETURN
                        ;END OF CASO SUBROUTINE

        ORG     0470H

FLASH:  LXI     H,PORT0 ;SET PORT POINTER TO PORT 0, ACTL
        MVI     M,0FFH  ;TURN ON STATUS LAMPS
CONT:   MVI     E,0FH   ;INITIALIZE E COUNTER
        MVI     D,0FFH  ;INITIALIZE D COUNTER
        MVI     C,0FFH  ;INITIALIZE C COUNTER
DCC:    DCR     C       ;COUNT C
        JNZ     DCC     ;IF NOT ZERO, JUMP BACK TO "COUNT C"
        DCR     D       ;COUNT D
        JNZ     DCC     ;IF NOT ZERO, JUMP BACK TO "COUNT C"
        DCR     E       ;ELSE COUNT E
        JNZ     DCC     ;IF NOT ZERO, JUMP BACK TO "COUNT C"
        XRI     0FH     ;ELSE COMPLEMENT STATUS LAMPS
        JMP     DCC     ;AND RESTART COUNTING CYCLE
                        ;END OF FLASH SUBROUTINE

; Cassette playback - ROM version.
; From page 226 of "How to Build Your Own Self-Programming Robot" by
; David L. Heiserman.

        CPU     8085

        ORG     04A0H

PLBK:   LXI     H,0000H ;INITIALIZE MEMORY POINTER
        MVI     C,0FAH  ;START CHECKING FOR LEADER
PB1:    CALL    BITIN
        JNC     PLBK    ;CONTINUE LOOKING FOR LEADER ON THE TAPE
        DCR     C
        JNZ     PB1     ;WAIT FOR END OF LEADER
PB2:    CALL    CASI    ;COMPILE THE BYTE TO BE ENTERED
        MOV     M,C     ;STORE THE BYTE
        INR     L       ;INCREMENT MEMORY POINTER
        JNZ     PB2
        INR     H
        JNZ     PB2
        CALL    FLASH   ;CALL FLASH
                        ;END OF PLBK

        ORG     04C0H

BITIN:  MVI     E,16H
BIT1:   DCR     E
        JNZ     BIT1
        RIM             ;SAMPLE SID
        RAL             ;MOVE BIT INTO CY POSITION
        RET             ;RETURN
                        ;END OF BITIN SUBROUTINE
        ORG     04D0H

CASI:   MVI     B,09H
TI1:    MVI     D,00H
TI2:    DCR     D
        CALL    BITIN
        JC      TI2
TI3:    INR     D
        CALL    BITIN
        JNC     TI3
        MOV     A,D
        RAL
        MOV     A,C
        RAR
        MOV     C,A
        DCR     B
        JNZ     TI1
        RET             ;RETURN
                        ;END OF CASI

        END
