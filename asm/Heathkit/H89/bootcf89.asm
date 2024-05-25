; H89 CF Card Boot ROM - Disassembled from binary
;
; Written by Rick Davis Jr
; See http://koyado.com/heathkit/New-H8-Website/h89-cf-serial-parallel-rtc.html

; Constants

CR      EQU     0DH             ; Carriage Return
LF      EQU     0AH             ; Line Feed
BELL    EQU     07H             ; Bell

; External ROM routines

DMOVE   EQU    18AAH            ; Move data routine
DZERO   EQU    198AH            ; Zero RAM routine
IOA     EQU    0640H            ; Input octal address
RCC     EQU    03B2H            ; Read console character
WCC     EQU    03C2H            ; Write console character

DATA    EQU    2036H            ; Output 362Q data save area (40.066)

; RAM usage

VARS    EQU     2C80H           ; Variables stored here

        ORG     1000H

; Entry point

        DI                      ; Disable interrupts
        LXI     SP,227FH        ; Set stack
        LXI     B,0400H         ; Set count for DMOVE
        LXI     D,L5            ; Set source address for DMOVE
        LXI     H,3092H         ; Set destination address for DMOVE
        CALL    DMOVE           ; Call move routine
        LXI     H,VARS          ; Set address for DZERO
        MVI     B,13H           ; Set length for DZERO
        CALL    DZERO           ; Call zero routine
        EI                      ; Enable interrupts
        MVI     A,0D4H
        STA     VARS+3
        CALL    32ABH           ; Jump to code that was moved to RAM (L25)
        LXI     H,MSG1          ; Address of message to display
        CALL    IOA             ; Input octal address from user (drive number to boot from)
L3      DI                      ; Disable interrupts
        CALL    RCC             ; Read character from console
        EI                      ; Enable interrupts
        CPI     CR              ; Is it <Return>?
        JNZ     L1              ; Branch if not
        MVI     A,'0'           ; Otherwise default to drive 0
L1      CPI     '0'
        JC      L2              ; Branch if less than '0'
        CPI     '7'
        JC      L4              ; Branch if less than '7'
L2      MVI     A,BELL          ; Invalid entry. Ring bell and try again.
        CALL    WCC
        JMP     L3

MSG1    DB      CR,LF,"BOOT CF LOGICAL DRIVE (0-6)? <0> ",0

L4      CALL    WCC             ; Echo character to console
        ANI     07H             ; Convert drive number from ASCII to binary
        STA     VARS+11
        XRA     A
        STA     VARS+8
        CALL    322CH           ; L7
        XRA     A
        CALL    3220H           ; L8
        CALL    3223H           ; L9
        CALL    3226H           ; L10
        CALL    3236H           ; L11
        MVI     A,07H
        CALL    324DH           ; L12
L6      CALL    3261H           ; L13
        ANI     0C0H
        CPI     40H
        JNZ     L6
        XRA     A
        STA     VARS
        JMP     3092H           ; L5

; The code below is copied to RAM at run time to address 3092H and
; executed from there, as it uses self-modifying code.

L5      LXI     D,VARS+18
        LXI     H,1
        SHLD    VARS+4
        XRA     A
        STA     VARS+8
        LDA     VARS+11
        STA     VARS+6
        MVI     A,01H
        STA     VARS+7
        XRA     A
        STA     VARS
        CALL    32C1H           ; L14
        LHLD    VARS+48
        INX     H
        SHLD    VARS+4
        LXI     D,VARS+18
        MVI     A,01H
        STA     VARS+7
        CALL    32C1H           ; L14
        LHLD    VARS+4
        INX     H
        SHLD    VARS+4
L37     LXI     D,2D92H
        MVI     A,01H
        STA     VARS+7
        CALL    32C1H           ; L14
        LDA     VARS+18
        CPI     48H
        JZ      31ADH           ; L37
        CPI     68H
        JZ      31ADH           ; L37
        CPI     43H
        JZ      310EH           ; L38
        CPI     63H
        JZ      310EH           ; L38
        LXI     H,30F5H
        CALL    IOA
        JMP     00D2H

        DB      "Unknown partition type",CR,LF,0

L38     DI
        MVI     A,22H
        STA     DATA
        OUT     0F2H
        STA     DATA
        LXI     H,0
        MVI     C,0
L26     MVI     M,0
        INX     H
        DCR     C
        JNZ     311EH           ; L26
        STA     000DH
        LXI     SP,00FFH
        LXI     H,00F1H
        SHLD    VARS+4
        MVI     C,1EH
        LXI     H,4380H
L28     PUSH    B
        PUSH    H
        MVI     A,01H
        STA     VARS+7
        STA     VARS
        LXI     D,VARS+18
        CALL    32C1H           ; L14
        POP     H
        POP     B
        MVI     A,01H
        CMP     C
        PUSH    B
        LXI     B,0180H
        JZ      3155H           ; L39
        LXI     B,0200H
L39     LXI     D,VARS+18
L27     LDAX    D
        MOV     M,A
        INX     H
        INX     D
        DCX     B
        MOV     A,C
        ORA     B
        JNZ     3158H           ; L27
        POP     B
        XCHG
        LHLD    VARS+4
        INX     H
        SHLD    VARS+4
        XCHG
        DCR     C
        JNZ     3136H           ; L28
        LDA     5A02H
        DCR     A
        MOV     H,A
        MVI     L,0
        LXI     D,0EA00H
        DAD     D
        LXI     D,4400H
L29     LDAX    D
        MOV     M,A
        INX     H
        INX     D
        MOV     A,L
        ORA     H
        JNZ     317EH           ; L29
        XRA     A
        STA     000EH
        MVI     A,0A0H
        STA     0048H
        MVI     A,58H
        STA     0049H
        LDA     5A02H
        DCR     A
        MOV     H,A
        MVI     L,0
        MVI     A,3
        STA     0004H
        MOV     C,A
        LDA     VARS+11
        MOV     B,A
        LDA     VARS+3
        SHLD    43FEH
        PCHL
        LHLD    VARS+279
        INX     H
        SHLD    VARS+4
        XRA     A
        STA     VARS
        MVI     C,0AH
        LXI     D,2280H
L30     MVI     A,01H
        STA     VARS+7
        PUSH    B
        CALL    32C1H           ; L14
        POP     B
        LHLD    VARS+4
        INX     H
        SHLD    VARS+4
        DCR     C
        JNZ     31BDH           ; L30
        DI
        LXI     H,20A0H
        LXI     B,001FH
        CALL    DZERO
        OUT     7FH
        STA     201BH
        STA     2152H
        INR     A
        STA     2008H
        LXI     H,201FH
L41     MVI     M,0C3H
        INX     H
        MVI     M,17H
        INX     H
        MVI     M,1CH
        INX     H
        ADD     A
        JP      31EBH           ; L41

        LXI     B,0058H         ; Set count for DMOVE
        LXI     D,1F5AH         ; Set source address for DMOVE
        LXI     H,2048H         ; Set destination address for DMOVE
        CALL    DMOVE           ; Call move routine
        XRA     A
        STA     2131H
        LXI     H,1C19H
        SHLD    2020H
        LXI     H,2156H         ; Set address for DZERO
        MVI     B,05H           ; Set length for DZERO
        CALL    DZERO           ; Call zero routine
        EI
        JMP     2280H

L18     IN      0D4H            ; Here and below, the port is set using self-modifying code
        RET

L20     IN      0D5H
        RET

L8      OUT     0D4H
        RET

L9      OUT     0D5H
        RET

L10     OUT     0D6H
        RET

L15     OUT     0D7H
        RET

L7      MVI     A,92H
        JMP     3229H           ; L15

L16     MVI     A,80H
        JMP     3229H           ; L15

L11     MVI     A,0FH
        CALL    3229H           ; L15
        MVI     A,64H
L31     DCR     A
        JNZ     323DH           ; L31
        MVI     A,0EH
        CALL    3229H           ; L15
        MVI     A,0C8H
L32     DCR     A
        JNZ     3248H           ; L32
        RET

L12     JMP     3226H           ; L10

L19     CALL    3231H           ; L16
        MVI     A,06H
        CALL    324DH           ; L12
        LDA     VARS+8
        ORI     0E0H
        CALL    327CH           ; L17
        RET

L13     MVI     A,07H
        CALL    3229H           ; L15
        MVI     A,0DH
        CALL    3229H           ; L15
        NOP
        CALL    321AH           ; L18
        PUSH    PSW
        MVI     A,0CH
        CALL    3229H           ; L15
        MVI     A,06H
        CALL    3229H           ; L15
        POP     PSW
        RET

L17     CALL    3220H           ; L8
        MVI     A,07H
        CALL    3229H           ; L15
        MVI     A,0BH
        CALL    3229H           ; L15
        NOP
        MVI     A,0AH
        CALL    3229H           ; L15
        MVI     A,06H
        CALL    3229H           ; L15
        RET

L21     MVI     A,07H
        CALL    3229H           ; L15
        MVI     A,0DH
        CALL    3229H           ; L15
        RET

L22     MVI     A,0CH
        CALL    3229H           ; L15
        MVI     A,06H
        CALL    3229H           ; L15
        RET

L25     STA     321BH           ; L18+1
        STA     3221H           ; L8+1
        INR     A
        STA     321EH           ; L20+1
        STA     3224H           ; L9+1
        INR     A
        STA     3227H           ; L10+1
        INR     A
        STA     322AH           ; L15+1
        RET

L14     CALL    3250H           ; L19
        CALL    322CH           ; L7
        MVI     A,07H
        CALL    324DH           ; L12
L33     CALL    3261H           ; L13
        ANI     0C0H
        CPI     40H
        JNZ     32CCH           ; L33
        CALL    3231H           ; L16
        LHLD    VARS+4
        MVI     A,03H
        CALL    324DH           ; L12
        MOV     A,L
        CALL    327CH           ; L17
        MVI     A,04H
        CALL    324DH           ; L12
        MOV     A,H
        CALL    327CH           ; L17
        MVI     A,05H
        CALL    324DH           ; L12
        LDA     VARS+6
        CALL    327CH           ; L17
        MVI     A,02H
        CALL    324DH           ; L12
        LDA     VARS+7
        CALL    327CH           ; L17
        MVI     A,07H
        CALL    324DH           ; L12
        MVI     A,20H
        CALL    327CH           ; L17
        CALL    322CH           ; L7
        MVI     A,07H
        CALL    324DH           ; L12
L34     CALL    3261H           ; L14
        ANI     88H
        CPI     08H
        JNZ     3316H           ; L34
        MVI     C,80H
        LDA     VARS
        ANA     A
        JZ      332BH           ; L40
        MVI     C,0
L40     MVI     A,0
        CALL    324DH           ; L12
L35     MVI     A,7
        CALL    3229H           ; L15
        MVI     A,0DH
        CALL    3229H           ; L15
        CALL    321AH           ; L18
        STAX    D
        INX     D
        CALL    321DH           ; L20
        STAX    D
        INX     D
        MVI     A,0CH
        CALL    3229H           ; L15
        MVI     A,6
        CALL    3229H           ; L15
        DCR     C
        JNZ     3330H           ; L35
        LDA     VARS
        ANA     A
        JNZ     3365H           ; L36
        MVI     C,80H
L24     CALL    3295H           ; L21
        CALL    32A0H           ; L22
        DCR     C
        JNZ     335BH           ; L24
L36     MVI     A,7
        CALL    324DH           ; L12
L23     CALL    3261H           ; L13
        ANI     0C0H
        CPI     40H
        JNZ     336AH           ; L23
        XRA     A
        RET

        DB      4EH,36H,59H,50H,43H

        DB      1800H-$ DUP 0   ; Fill rest of ROM with zeroes
