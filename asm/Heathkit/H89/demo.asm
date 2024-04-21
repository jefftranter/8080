;                               APPENDIX B
;                              MTR-88 DEMO
; 
; The sample program that follows shows some of the advanced features
; that are available to you with MTR-88. The program is not designed to
; be efficient or particularly useful by itself. It uses the H88 clock,
; console terminal, and interrupt capability to create an accurate
; interval timer that will time up to 377(octal) seconds. When the
; interval ends, the H88 audio alarm is sounded.
; 
; Use the H89 keyboard and the "Substitute" command to enter the machine
; code and start the program. You will also use the keyboard to enter
; the octal time.
; 
; The demo uses the MTR-88 firmware (program in a ROM) for most of the
; working routines, and you should look up the details of these routines
; (in Appendix A). The listing of the demo was prepared using the text
; editor and assembler that are available for the H88. However, the
; program should be loaded by hand using the "Substitute" command.
; 
;                           THE SAMPLE PROGRAM
; 
; This program initially blanks out the screen and then waits for you
; to enter an octal value. The MTR-88 routine WCC is used to send the
; characters to the screen, and IOB is used to Input an Octal Byte.
; 
; The most subtle part of the program is the interrupt processing.
; First, a jump to the interrupt processor is planted in UIVEC to allow
; processing of the clock interrupts. Then .MFLAG is set so MTR-88 will
; pass interrupts to the program. Finally, interrupts are enabled.
; 
; The main part of the program is a "do-nothing" loop that waits for the
; time to count down to zero. When the time is exhausted, the program
; restores the original state of .MFLAG and stops.
; 
; The interrupt processor keeps its own local TICCNT and counts it down
; from 500. When this count reaches zero, one second has elapses and the
; new reduced time is displayed on the screen using TOB (Type Octal
; Byte). The local TICCNT is reset to 500. When the time is exhausted,
; the main program stops clock processing, so the processor is not
; called again.

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       DEMO: MTR-88
;
;       SYSTEM DEFINITIONS
;
ALARM   EQU     1136Q           ; MAKE NOISE
WCC     EQU     1702Q           ; WRITE CHAR TO CONSOLE
MFLAG   EQU     20010Q          ; USER FLAG OPTIONS
UIVEC   EQU     20037Q          ; USER INTERRUPT VECTOR
UO.CLK  EQU     1Q              ; ALLOW CLOCK INTERRUPT PROCESSING
MI.JMP  EQU     303Q            ; MACHINE INSTRUCTION JUMP
IOB     EQU     1466Q           ; INPUT OCTAL BYTE
TOB     EQU     2743Q           ; TYPE OCTAL BYTE

ESC     EQU     33Q
CR      EQU     15Q
LF      EQU     12Q

        ORG     20100Q

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       ERASE SCREEN
;
MTR88   MVI     A,ESC           ; ESCAPE SEQUENCE TO
        CALL    WCC
        MVI     A,'E'           ;     ERASE SCREEN
        CALL    WCC

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       READ A OCTAL INTEGER FROM KEYBOARD
;          STORE THE NUMBER.
;
        LXI     H,NUMBER        ; GET ADDRESS OF NUMBER
        ANA     A               ; CLEAR CARRY (SIDE EFFECT)
        CALL    IOB             ; INPUT OCTAL BYTE
        CALL    SETICK          ; SETUP TICK TO 500 FOR ONE SEC

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       INITIALIZE SERVICE INTERRUPT ROUTINE
;          LOAD THE USER INTERRUPT VECTOR (UIVEC) WITH A
;          JUMP INSTRUCTION AND THE ADDRESS OF THE SERVICE
;          ROUTINE.  ENABLE USER CLOCK INTERRUPT!
;
        MVI     A,MI.JMP        ; SET-UP JUMP INSTRUCTION
        STA     UIVEC           ; STORE 'JMP' INSTRUCTION
        LXI     H,INTRP         ;    USER INTERRUPT ADDRESS
        SHLD    UIVEC+1         ;       POSITIONED
        MVI     A,UO.CLK
        STA     MFLAG           ; ENABLE CLOCK INTERRUPT

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       WAIT FOR CLOCK TO REACH ZERO
;
LOOP    LDA     NUMBER          ; DO NOTHING LOOP.
        CPI     0               ;    WAIT FOR END
        JNZ     LOOP            ;    OF COUNT DOWN.

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       RETURN TO NORMAL INTERRUPT STATUS & HALT
;          DISABLE INTERRUPT & TURN ON SPEAKER
;
        MVI     A,0
        STA     MFLAG           ; DISABLE CLOCK INTERRUPT
        CALL    ALARM
        HLT

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       INTERRUPT ROUTINE
;          CLOCK AND DISPLAY INTERRUPT
;
INTRP   LHLD    TICK            ; GET COUNT (BETWEEN 0 & 500)
        DCX     H               ; TICK=TICK-1
        SHLD    TICK            ; STORE COUNT
        MOV     A,L             ; TEST FOR ZERO
        ORA     H               ;    COMPARE WITH 'H'
        RNZ                     ;    EXIT IF .NE. 0

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       UPDATE DISPLAY FOR 'NEW' NUMBER.
;
        MVI     A,CR            ; DO CARRIAGE RETURN
        CALL    WCC
        MVI     A,LF            ;    AND LINE FEED
        CALL    WCC
        LDA     NUMBER          ; GET NUMBER
        DCR     A               ; NUMBER=NUMBER-1
        STA     NUMBER          ; SAVE NUMBER
        CALL    TOB             ; TYPE OCTAL BYTE
SETICK  LXI     H,500           ; RESTORE COUNT
        SHLD    TICK            ;   WITH 500
        RET

;       STORAGE AREA & END ASSEMBLY
TICK    DS      2
NUMBER  DS      1
        END
