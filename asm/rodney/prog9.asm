; Expanding the Rodney system.
; From page 230 of "How to Build Your Own Self-Programming Robot" by
; David L. Heiserman.

        CPU     8085

PORT0   EQU     0800H
PORT1   EQU     0801H
PORT4   EQU     0804H

        ORG     0000H

; LOW BATTERY SENSING CIRCUIT

        LDA     PORT0   ;FETCH ENVL
        ANI     08H     ;ISOLATE HUNGRY BIT
                        ;IF HUNGRY, Z
                        ;IF NOT HUNGRY, NZ

; GIVING RODNEY AN "EYE"

        LDA     PORT1   ;FETCH ENVH
        ANI     02H     ;ISOLATE LTIN BIT

; GIVING RODNEY A "BEEP-BEEP" VOICE

        LDA     PORT1   ;FETCH ENVH
        ORI     02H     ;SET "BEEP" BIT TO 1
        STA     PORT1   ;OUTPUT AS ACTH

        LDA     PORT1   ;FETCH ENVH
        ANI     0FDH    ;SET "BEEP" BIT TO 0
        STA     PORT1   ;OUTPUT AS ACTH

; GIVING RODNEY AN "EAR"

        LDA     PORT1   ;FETCH ENVH
        ANI     01H     ;ISOLATE SOND BIT

; ACCESSING A TWO-BYTE MAIN MEMORY ADDRESS

        LXI     H,PORT4 ;SET PORT POINTER TO MMAL
        MVI     A,34H   ;LOAD LOWER-BYTE ADDR INTO REGISTER A
        MOV     M,A     ;LOAD LOWER-BYTE ADDR INTO MMAL
        INR     L       ;SET PORT POINTER TO MMAH, PORT 5
        MVI     A,12H   ;LOAD UPPER-BYTE ADDR INTO REGISTER A
        MOV     M,A     ;LOAD UPPER-BYTE ADDR INTO MMAL
        INR     L       ;SET PORT POINTER TO MMDL--PORT 6
        MVI     A,56H   ;LOAD DATA INTO REGISTER A
        MOV     M,A     ;LOAD DATA INTO MEMORY
        DCR     L
        DCR     L       ;RETURN PORT POINTER TO PORT 4

        END
