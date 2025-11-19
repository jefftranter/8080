*** **************************************************
*
*       DEMO: MTR-88
*
*       SYSTEM DEFINITIONS
*
ALARM   EQU     2136A           MAKE NOISE
WCC     EQU     3302A           WRITE CHAR TO CONSOLE
.MFLAG   EQU    40010A          USER FLAG OPTIONS
UIVEC   EQU     40037A          USER INTERRUPT VECTOR
UO.CLK  EQU     1A              ALLOW CLOCK INTERRUPT PROCESSING
MI.JMP  EQU     303A            MACHINE INSTRUCTION JUMP
IOB     EQU     3066A           INPUT OCTAL BYTE
TOB     EQU     5343A           TYPE OCTAL BYTE

ESC     EQU     33A
CR      EQU     15A
LF      EQU     12A

        ORG     40100A

*** **************************************************
*
*       ERASE SCREEN
*
MTR88   MVI     A,ESC           ESCAPE SEQUENCE TO
        CALL    WCC
        MVI     A,'E'               ERASE SCREEN
        CALL    WCC

*** **************************************************
*
*       READ A OCTAL INTEGER FROM KEYBOARD
*          STORE THE NUMBER.
*
        LXI     H,NUMBER        GET ADDRESS OF NUMBER
        ANA     A               CLEAR CARRY (SIDE EFFECT)
        CALL    IOB             INPUT OCTAL BYTE
        CALL    SETICK          SETUP TICK TO 500 FOR ONE SEC

*** **************************************************
*
*       INITIALIZE SERVICE INTERRUPT ROUTINE
*          LOAD THE USER INTERRUPT VECTOR (UIVEC) WITH A
*          JUMP INSTRUCTION AND THE ADDRESS OF THE SERVICE
*          ROUTINE.  ENABLE USER CLOCK INTERRUPT!
*
        MVI     A,MI.JMP        SET-UP JUMP INSTRUCTION
        STA     UIVEC           STORE 'JMP' INSTRUCTION
        LXI     H,INTRP            USER INTERRUPT ADDRESS
        SHLD    UIVEC+1               POSITIONED
        MVI     A,UO.CLK
        STA     .MFLAG          ENABLE CLOCK INTERRUPT

*** **************************************************
*
*       WAIT FOR CLOCK TO REACH ZERO
*
LOOP    LDA     NUMBER          DO NOTHING LOOP.
        CPI     0                  WAIT FOR END
        JNZ     LOOP               OF COUNT DOWN.

*** **************************************************
*
*       RETURN TO NORMAL INTERRUPT STATUS & HALT
*          DISABLE INTERRUPT & TURN ON SPEAKER
*
        MVI     A,0
        STA     .MFLAG          DISABLE CLOCK INTERRUPT
        CALL    ALARM
        HLT

*** **************************************************
*
*       INTERRUPT ROUTINE
*          CLOCK AND DISPLAY INTERRUPT
*
INTRP   LHLD    TICK            GET COUNT (BETWEEN 0 & 500)
        DCX     H               TICK=TICK-1
        SHLD    TICK            STORE COUNT
        MOV     A,L             TEST FOR ZERO
        ORA     H                  COMPARE WITH 'H'
        RNE                        EXIT IF .NE. 0

*** **************************************************
*
*       UPDATE DISPLAY FOR 'NEW' NUMBER.
*
        MVI     A,CR            DO CARRIAGE RETURN
        CALL    WCC
        MVI     A,LF               AND LINE FEED
        CALL    WCC
        LDA     NUMBER          GET NUMBER
        DCR     A               NUMBER=NUMBER-1
        STA     NUMBER          SAVE NUMBER
        CALL    TOB             TYPE OCTAL BYTE
SETICK  LXI     H,500           RESTORE COUNT
        SHLD    TICK              WITH 500
        RET

*       STORAGE AREA & END ASSEMBLY
TICK    DS      2
NUMBER  DS      1
        END     MTR88
