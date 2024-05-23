; H89 CF Card Boot ROM - Disassembled from binary
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

        ORG     1000H

; Entry point

        DI                      ; Disable interrupts
        LXI     SP,227FH        ; Set stack
        LXI     B,0400H         ; Set count for DMOVE
        LXI     D,L5            ; Set source address for DMOVE
        LXI     H,3092H         ; Set destination address for DMOVE
        CALL    DMOVE           ; Call move routine
        LXI     H,2C80H         ; Set address for DZERO
        MVI     B,13H           ; Set length for DZERO
        CALL    DZERO           ; Call zero routine
        EI                      ; Enable interrupts
        MVI     A,0D4H
        STA     02C83H
        CALL    32ABH           ; Jump to code that was moved to RAM
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
        ANI     07H             ; Convert driver number from ASCII to binary
        STA     2C8BH
        XRA     A
        STA     2C88H
        CALL    322CH
        XRA     A
        CALL    3220H
        CALL    3223H
        CALL    3226H
        CALL    3236H
        MVI     A,07H
        CALL    324DH
L6      CALL    3261H
        ANI     0C0H
        CPI     40H
        JNZ     L6
        XRA     A
        STA     2C80H
        JMP     3092H
L5      LXI     D,2C92H
        LXI     H,0001H
        SHLD    2C84H
        XRA     A
        STA     2C88H
        LDA     2C8BH
        STA     2C86H
        MVI     A,01H
        STA     2C87H
        XRA     A
        STA     2C80H
        CALL    32C1H
        LHLD    2CB0H
        INX     H
        SHLD    2C84H
        LXI     D,2C92H
        MVI     A,01H
        STA     2C87H
        CALL    32C1H
        LHLD    2C84H
        INX     H
        SHLD    2C84H
        LXI     D,2D92H
        MVI     A,01H
        STA     2C87H
        CALL    32C1H
        LDA     2C92H
        CPI     48H
        JZ      31ADH
        CPI     68H
        JZ      31ADH
        CPI     43H
        JZ      310EH
        CPI     63H
        JZ      310EH
        LXI     H,30F5H
        CALL    IOA
        JMP     00D2H

        DB      "Unknown partition type",CR,LF,0

        DI
        MVI     A,22H
        STA     2036H
        OUT     0F2H
        STA     2036H
        LXI     H,0000H
        MVI     C,00H
        MVI     M,00H
        INX     H
        DCR     C
        JNZ     311EH
        STA     000DH
        LXI     SP,00FFH
        LXI     H,00F1H
        SHLD    2C84H
        MVI     C,1EH
        LXI     H,4380H
        PUSH    B
        PUSH    H
        MVI     A,01H
        STA     2C87H
        STA     2C80H
        LXI     D,2C92H
        CALL    32C1H
        POP     H
        POP     B
        MVI     A,01H
        CMP     C
        PUSH    B
        LXI     B,0180H
        JZ      3155H
        LXI     B,0200H
        LXI     D,2C92H
        LDAX    D
        MOV     M,A
        INX     H
        INX     D
        DCX     B
        MOV     A,C
        ORA     B
        JNZ     3158H
        POP     B
        XCHG
        LHLD    2C84H
        INX     H
        SHLD    2C84H
        XCHG
        DCR     C
        JNZ     3136H
        LDA     5A02H
        DCR     A
        MOV     H,A
        MVI     L,00H
        LXI     D,0EA00H
        DAD     D
        LXI     D,4400H
        LDAX    D
        MOV     M,A
        INX     H
        INX     D
        MOV     A,L
        ORA     H
        JNZ     317EH
        XRA     A
        STA     000EH
        MVI     A,0A0H
        STA     0048H
        MVI     A,58H
        STA     0049H
        LDA     5A02H
        DCR     A
        MOV     H,A
        MVI     L,00H
        MVI     A,03H
        STA     0004H
        MOV     C,A
        LDA     2C8BH
        MOV     B,A
        LDA     2C83H
        SHLD    43FEH
        PCHL
        LHLD    2D97H
        INX     H
        SHLD    2C84H
        XRA     A
        STA     2C80H
        MVI     C,0AH
        LXI     D,2280H
        MVI     A,01H
        STA     2C87H
        PUSH    B
        CALL    32C1H
        POP     B
        LHLD    2C84H
        INX     H
        SHLD    2C84H
        DCR     C
        JNZ     31BDH
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
        MVI     M,0C3H
        INX     H
        MVI     M,17H
        INX     H
        MVI     M,1CH
        INX     H
        ADD     A
        JP      31EBH
        LXI     B,0058H
        LXI     D,1F5AH
        LXI     H,2048H
        CALL    DMOVE
        XRA     A
        STA     2131H
        LXI     H,1C19H
        SHLD    2020H
        LXI     H,2156H
        MVI     B,05H
        CALL    DZERO
        EI
        JMP     2280H
        IN      0D4H
        RET
        IN      0D5H
        RET
        OUT     0D4H
        RET
        OUT     0D5H
        RET
        OUT     0D6H
        RET
        OUT     0D7H
        RET
        MVI     A,92H
        JMP     3229H
        MVI     A,80H
        JMP     3229H
        MVI     A,0FH
        CALL    3229H
        MVI     A,64H
        DCR     A
        JNZ     323DH
        MVI     A,0EH
        CALL    3229H
        MVI     A,0C8H
        DCR     A
        JNZ     3248H
        RET
        JMP     3226H
        CALL    3231H
        MVI     A,06H
        CALL    324DH
        LDA     2C88H
        ORI     0E0H
        CALL    327CH
        RET
        MVI     A,07H
        CALL    3229H
        MVI     A,0DH
        CALL    3229H
        NOP
        CALL    321AH
        PUSH    PSW
        MVI     A,0CH
        CALL    3229H
        MVI     A,06H
        CALL    3229H
        POP     PSW
        RET
        CALL    3220H
        MVI     A,07H
        CALL    3229H
        MVI     A,0BH
        CALL    3229H
        NOP
        MVI     A,0AH
        CALL    3229H
        MVI     A,06H
        CALL    3229H
        RET
        MVI     A,07H
        CALL    3229H
        MVI     A,0DH
        CALL    3229H
        RET
        MVI     A,0CH
        CALL    3229H
        MVI     A,06H
        CALL    3229H
        RET
        STA     321BH
        STA     3221H
        INR     A
        STA     321EH
        STA     3224H
        INR     A
        STA     3227H
        INR     A
        STA     322AH
        RET
        CALL    3250H
        CALL    322CH
        MVI     A,07H
        CALL    324DH
        CALL    3261H
        ANI     0C0H
        CPI     40H
        JNZ     32CCH
        CALL    3231H
        LHLD    2C84H
        MVI     A,03H
        CALL    324DH
        MOV     A,L
        CALL    327CH
        MVI     A,04H
        CALL    324DH
        MOV     A,H
        CALL    327CH
        MVI     A,05H
        CALL    324DH
        LDA     2C86H
        CALL    327CH
        MVI     A,02H
        CALL    324DH
        LDA     2C87H
        CALL    327CH
        MVI     A,07H
        CALL    324DH
        MVI     A,20H
        CALL    327CH
        CALL    322CH
        MVI     A,07H
        CALL    324DH
        CALL    3261H
        ANI     88H
        CPI     08H
        JNZ     3316H
        MVI     C,80H
        LDA     2C80H
        ANA     A
        JZ      332BH
        MVI     C,00H
        MVI     A,00H
        CALL    324DH
        MVI     A,07H
        CALL    3229H
        MVI     A,0DH
        CALL    3229H
        CALL    321AH
        STAX    D
        INX     D
        CALL    321DH
        STAX    D
        INX     D
        MVI     A,0CH
        CALL    3229H
        MVI     A,06H
        CALL    3229H
        DCR     C
        JNZ     3330H
        LDA     2C80H
        ANA     A
        JNZ     3365H
        MVI     C,80H
        CALL    3295H
        CALL    32A0H
        DCR     C
        JNZ     335BH
        MVI     A,07H
        CALL    324DH
        CALL    3261H
        ANI     0C0H
        CPI     40H
        JNZ     336AH
        XRA     A
        RET
        DB      4EH,36H,59H,50H,43H

        DB      1800H-$ DUP 0   ; Fill rest of ROM with zeroes
