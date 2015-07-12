; TITLE '8080 system monitor, ver 1'
;
; July 11, 2015
;
TOP     EQU     24      ;MEMORY TOP, K BYTES
ORGIN   EQU     (TOP-2)*1024  ;PROGRAM START
;
        CPU     8080
        ORG     ORGIN
;
;
HOME    EQU     ORGIN   ;ABORT ADDRESS
VERS    EQU     '7'     ;VERSION NUMBER
STACK   EQU     ORGIN-60H
CSTAT   EQU     10H     ;CONSOLE STATUS
CDATA   EQU     CSTAT+1 ;CONSOLE DATA
CSTATO  EQU     CSTAT   ;CON OUT STATUS
CDATAO  EQU     CSTATO+1 ;OUT DATA
INMSK   EQU     1        ;INPUT MASK
OMSK    EQU     2        ;OUTPUT MASK
;
PORTN   EQU     STACK   ;3BYTES I/O
IBUFP   EQU     STACK+3 ;BUFFER POINTER
IBUFC   EQU     IBUFP+2 ;BUFFER COUNT
IBUFF   EQU     IBUFP+3 ;INPUT BUFFER
;
CTRH    EQU     8       ;^H BACKSPACE
TAB     EQU     9       ;^I
CTRQ    EQU     17      ;^Q
CTRS    EQU     19      ;^S
CTRX    EQU     24      ;^X, ABORT
BACKUP  EQU     CTRH    ;BACKUP CHAR
DEL     EQU     127     ;RUBOUT
ESC     EQU     27      ;ESCAPE
APOS    EQU     (39-'0') & 0FFH
CR      EQU     13      ;CARRIAGE RET
LF      EQU     10      ;LINE FEED
INC     EQU     0DBH    ;IN OP CODE
OUTC    EQU     0D3H    ;OUT OP CODE
RETC    EQU     0C9H    ;RET OP CODE
;
; VECTORS TO USEFUL ROUTINES
;
START:  JMP     COLD    ;COLD START
RESTRT: JMP     WARM    ;WARM START
COUT:   JMP     OUTT    ;OUTPUT CHAR
CIN:    JMP     INPUTT  ;INPUT CHAR
INLN:   JMP     INPLN   ;INPUT LINE
GCHAR:  JMP     GETCH   ;GET CHAR
OUTH:   JMP     OUTHX   ;BIN TO HEX
;
; CONSOLE INPUT ROUTINE
;
INPUTT: CALL    INSTAT  ;CHECK STATUS
        JZ      INPUTT  ;NOT READY ***
INPUT2: IN      CDATA   ;GET BYTE
        ANI     DEL
        CPI     CTRX    ;ABORT?
        JZ      HOME    ;YES
        RET
;
; GET CONSOLE-INPUT STATUS
;
INSTAT: IN      CSTAT
        ANI     INMSK
        RET
;
; CONSOLE OUTPUT ROUTINE
;
OUTT:   PUSH    PSW
OUT2:   CALL    INSTAT  ;INPUT?
        JZ      OUT4    ;NO ***
        CALL    INPUT2  ;GET INPUT
        CPI     CTRS    ;FREEZE?
        JNZ     OUT2    ;NO
;
; FREEZE OUTPUT UNTIL ^Q OR ^X
;
OUT3:   CALL    INPUTT  ;INPUT?
        CPI     CTRQ    ;RESUME?
        JNZ     OUT3    ;NO
        JMP     OUT2
;
OUT4:   IN      CSTATO  ;CHECK STATUS
        ANI     OMSK
        JZ      OUT2    ;NOT READY***
        POP     PSW
        OUT     CDATAO  ;SEND DATA
        RET
;
SIGNON: DB      CR,LF
        DB      " Ver "
        DW      VERS
        DB      0
;
; CONTINUATION OF COLD START
;
COLD:   LXI     SP,STACK
        LXI     D,SIGNON ;MESSAGE
        CALL    SENDM   ;SEND IT
;
; WARM-START ENTRY
;
WARM:   LXI     H,WARM  ;RETURN HERE
        PUSH    H
;
; FIND TOP OF USABLE MEMORY.
; CHECK FIRST BYTE OF EACH PAGE OF MEMORY
; STARTING AT ADDRESS ZERO.  STOP AT STACK
; OR MISSING/DEFECTIVE/PROTECTED MEMORY.
; DISPLAY HIGH BYTE OF MEMORY TOP.
        LXI     H,0     ;PAGE ZERO
        MVI     B,STACK >> 8
NPAGE:  MOV     A,M     ;GET BYTE
        CMA             ;COMPLEMENT
        MOV     M,A     ;PUT IT BACK
        CMP     M       ;SAME?
        JNZ     MSIZE   ;NO, MEM TOP
        CMA             ;ORIG BYTE
        MOV     M,A     ;RESTORE IT
        INR     H       ;NEXT PAGE
        DCR     B
        JNZ     NPAGE
MSIZE:  MOV     C,H     ;MEM TOP
        CALL    CRLF    ;NEW LINE
        CALL    OUTHX   ;PRINT MEM SIZE
        CALL    INPLN   ;CONSOLE LINE
        CALL    GETCH   ;FIRST CHAR
;
; MAIN COMMAND PROCESSOR
;
        SUI     'A'     ;CONVERT OFFSET
        JC      ERROR   ; < A
        CPI     'Z'-'A'+1
        JNC     ERROR   ; > Z
        ADD     A       ;DOUBLE
        LXI     H,TABLE ;START
        MVI     D,0
        MOV     E,A     ;OFFSET
        DAD     D       ;ADD TO TABLE
        MOV     E,M     ;LOW BYTE
        INX     H
        MOV     D,M     ;HIGH BYTE
        XCHG            ;INTO H,L
        PCHL            ;GO THERE
;
; COMMAND TABLE
;
TABLE:  DW      ERROR   ;A, ASCII
        DW      ERROR   ;B
        DW      CALLS   ;C, CALL SUBR
        DW      DUMP    ;D, DUMP
        DW      ERROR   ;E
        DW      ERROR   ;F, FILL
        DW      GO      ;G, GO
        DW      ERROR   ;H, HEX MATH
        DW      ERROR   ;I, PORT INPUT
        DW      ERROR   ;J, MEMORY TEST
        DW      ERROR   ;K
        DW      LOAD    ;L, LOAD
        DW      ERROR   ;M, MOVE
        DW      ERROR   ;N
        DW      ERROR   ;O, PORT OUTPUT
        DW      ERROR   ;P
        DW      ERROR   ;Q
        DW      ERROR   ;R, REPLACE
        DW      ERROR   ;S, SEARCH
        DW      ERROR   ;T
        DW      ERROR   ;U
        DW      ERROR   ;V, VERIFY MEM
        DW      ERROR   ;W
        DW      ERROR   ;X, STACK POINTER
        DW      ERROR   ;Y
        DW      ERROR   ;Z, ZERO
;
; INPUT A LINE FROM CONSOLE AND PUT IT
; INTO THE BUFFER. CARRIAGE RETURN ENDS
; THE LINE. RUBOUT OR ^H CORRECTS LAST
; ENTRY. CONTROL-X RESTARTS LINE.
; OTHER CONTROL CHARACTERS ARE IGNORED.
;
INPLN:  MVI     A,'>'   ;PROMPT
        CALL    OUTT
INPL2:  LXI     H,IBUFF ;BUFFER ADDR
        SHLD    IBUFP   ;SAVE POINTER
        MVI     C,0     ;COUNT
INPLI:  CALL    INPUTT  ;CONSOLE CHAR
        CPI     ' '     ;CONTROL?
        JC      INPLC   ;YES
        CPI     DEL     ;DELETE
        JZ      INPLB   ;YES
        CPI     'Z'+1   ;UPPER CHAR?
        JC      INPL3   ;YES
        ANI     5FH     ;MAKE UPPER
INPL3:  MOV     M,A     ;INTO BUFFER
        MVI     A,32    ;BUFFER SIZE
        CMP     C       ;FULL?
        JZ      INPLI   ;YES. LOOP
        MOV     A,M     ;GET CHAR
        INX     H       ;INCR POINTER
        INR     C       ;AND COUNT
INPLE:  CALL    OUTT    ;SHOW CHAR
        JMP     INPLI   ;NEXT CHAR
;
; PROCESS CONTROL CHARACTER
;
INPLC:  CPI     CTRH    ;^H?
        JZ      INPLB   ;YES
        CPI     CR      ;RETURN:
        JNZ     INPLI   ;NO, IGNORE
;
; END OF INPUT LINE
;
        MOV     A,C     ;COUNT
        STA     IBUFC   ;SAVE
;
; CARRIAGE-RETURN, LINE-FEED ROUTINE
;
CRLF:   MVI     A,CR
        CALL    OUTT    ;SEND CR
        MVI     A,LF
        JMP     OUTT    ;SEND LF
;
; DELETE PRIOR CHARACTER IF ANY
;
INPLB:  MOV     A,C     ;CHAR COUNT
        ORA     A       ;ZERO?
        JZ      INPLI   ;YES
        DCX     H       ;BACK POINTER
        DCR     C       ;AND COUNT
        MVI     A,BACKUP ;CHARACTER
        JMP     INPLE   ;SEND
;
; GET A CHARACTER FROM CONSOLE BUFFER
; SET CARRY IF EMPTY
;
GETCH:  PUSH    H       ;SAVE REGS
        LHLD    IBUFP   ;GET POINTER
        LDA     IBUFC   ;AND COUNT
        SUI     1       ;DECR WITH CARRY
        JC      GETC4   ;NO MORE CHAR
        STA     IBUFC   ;SAVE NEW COUNT
        MOV     A,M     ;GET CHARACTER
        INX     H       ;INCR POINTER
        SHLD    IBUFP   ;AND SAVE
GETC4:  POP     H       ;RESTORE REGS
        RET
;
; SEND ASCII MESSAGE UNTIL BINARY ZERO
; IS FOUND. POINTER IS D,E
;
SENDM:  LDAX    D       ;GET BYTE
        ORA     A       ;ZERO?
        RZ              ;YES, DONE
        CALL    OUTT    ;SEND IT
        INX     D       ;POINTER
        JMP     SENDM   ;NEXT
;
; DUMP MEMORY IN HEXADECIMAL AND ASCII
;
DUMP:   CALL    RDHLDE  ;RANGE
DUMP2:  CALL    CRHL    ;NEW LINE
DUMP3:  MOV     C,M     ;GET BYTE
        CALL    OUTHX   ;PRINT
        INX     H       ;POINTER
        MOV     A,L
        ANI     0FH     ;LINE END?
        JZ      DUMP4   ;YES, ASCII
        ANI     3       ;SPACE
        CZ      OUTSP   ; 4 BYTES
        JMP     DUMP3   ;NEXT HEX
DUMP4:  CALL    OUTSP
        PUSH    D
        LXI     D,-10H  ;RESET LINE
        DAD     D
        POP     D
DUMP5:  CALL    PASCI   ;ASCII DUMP
        CALL    TSTOP   ;DONE?
        MOV     A,L     ;NO
        ANI     0FH     ;LINE END?
        JNZ     DUMP5   ;NO
        JMP     DUMP2
;
; DISPLAY MEMORY BYTE IN ASCII IF
; POSSIBLE, OTHERWISE GIVE DECIMAL PNT
;
PASCI:  MOV     A,M     ;GET BYTE
        CPI     DEL     ;HIGH BIT ON?
        JNC     PASC2   ;YES
        CPI     ' '     ;CONTROL CHAR?
        JNC     PASC3   ;NO
PASC2:  MVI     A,'.'   ;CHANGE TO DOT
PASC3:  JMP     OUTT    ;SEND
;
; GET H,L ADN D,E FROM CONSOLE
; CHECK THAT D,E IS LARGER
;
RDHLDE: CALL    HHLDE
RDHDD2: MOV     A,E
        SUB     L       ;E - L
        MOV     A,D
        SBB     H       ;D - H
        JC      ERROR   ;H,L BIGGER
        RET
;
; INPUT H,L AND D,E. SEE THAT
; 2 ADDRESSES ARE ENTERED
;
HHLDE:  CALL    READHL  ;H,L
        JC      ERROR   ;ONLY 1 ADDR
        XCHG            ;SAVE IN D,E
        CALL    READHL  ;D,E
        XCHG            ;PUT BACK
        RET
;
; INPUT H,L FROM CONSOLE
;
READHL: PUSH    D
        PUSH    B       ;SAVE REGS
        LXI     H,0     ;CLEAR
RDHL2:  CALL    GETCH   ;GET CHAR
        JC      RDHL5   ;LINE END
        CALL    NIB     ;TO BINARY
        JC      RDHL4   ;NOT HEX
        DAD     H       ;TIMES 2
        DAD     H       ;TIMES 4
        DAD     H       ;TIMES 8
        DAD     H       ;TIMES 16
        ORA     L       ;ADD NEW CHAR
        MOV     L,A
        JMP     RDHL2   ;NEXT
;
; CHECK FOR BLANK AT END
;
RDHL4:  CPI     APOS    ;APOSTROPHE
        JZ      RDHL5   ;ASCII INPUT
        CPI     (' '-'0') & 0FFH
        JNZ     ERROR   ;NO
RDHL5:  POP     B
        POP     D       ;RESTORE
        RET
;
; CONVERT ASCII CHARACTERS TO BINARY
;
NIB:    SUI     '0'     ;ASCII BIAS
        RC      ; < 0
        CPI     'F'-'0'+1
        CMC             ;INVERT
        RC              ;ERROR, > F
        CPI     10
        CMC             ;INVERT
        RNC             ;NUMBER 0-9
        SUI     'A'-'9'-1
        CPI     10      ;SKIP : TO
        RET             ;LETTER A-F
;
; PRINT ? ON IMPROPER INPUT
;
ERROR:  MVI     A,'?'
        CALL    OUTT
        JMP     START   ;TRY AGAIN
;
; START NEW LINE, GIVE ADDRESS
;
CRHL:   CALL    CRLF    ;NEW LINE
;
; PRINT H,L IN HEX
;
OUTHL:  MOV     C,H
        CALL    OUTHX   ;H
OUTLL:  MOV     C,L
;
; OUTPUT HEX BYTE FROM C AND A SPACE
;
OUTHEX: CALL    OUTHX
;
; OUTPUT A SPACE
;
OUTSP:  MVI     A,' '
        JMP     OUTT
;
; OUTPUT A HEX BYTE FROM C
; BINARY TO ASCII HEX CONVERSION
;
OUTHX:  MOV     A,C
        RAR             ;ROTATE
        RAR             ; FOUR
        RAR             ; BITS TO
        RAR             ; RIGHT
        CALL    HEX1    ;UPPER CHAR
        MOV     A,C     ;LOWER CHAR
HEX1:   ANI     0FH     ;TAKE 4 BITS
        ADI     90H
        DAA             ;DAA TRICK
        ACI     40H
        DAA
        JMP     OUTT
;
; CHECK FOR END, H,L MINUS D,E
; INCREMENT H,L
;
TSTOP:  INX     H
        MOV     A,E
        SUB     L       ; E - L
        MOV     A,D
        SBB     H       ; D - H
        RNC             ;NOT DONE
        POP     H       ;RAISE STACK
        RET
;
; ROUTINE TO GO ANYWHERE IN MEMORY
; ADDRESS OF WARM IS ON STACK, SO A
; SIMPLE RET WILL RETURN TO THIS MONITOR
;
GO:     POP     H       ;RAISE STACK
CALLS:  CALL    READHL  ;GET ADDRESS
        PCHL            ;GO THERE
;
; LOAD HEX OR ASCII CHAR INTO MEMORY
; FROM CONSOLE. CHECK TO SEE IF
; THE DATA ACTUALLY GOT THERE
; APOSTROPHE PRECEEDS ASCII CHAR
; CARRIAGE RETURN PASSES OVER LOCATION
;
LOAD:   CALL    READHL  ;ADDRESS
LOAD2:  CALL    OUTHL   ;PRINT IT
        CALL    PASCI   ;ASCII
        CALL    OUTSP
        MOV     C,M     ;ORIG BYTE
        CALL    OUTHEX  ;HEX
        PUSH    H       ;SAVE PNTR
        CALL    INPL2   ;INPUT
        CALL    READHL  ; BYTE
        MOV     B,L     ; TO B
        POP     H
        CPI     APOS
        JZ      LOAD6   ;ASCII INPUT
        MOV     A,C     ;HOW MANY?
        ORA     A       ;NONE?
        JZ      LOAD3   ;YES
LOAD4:  CALL    CHEKM   ;INTO MEMORY
LOAD3:  INX     H       ;POINTER
        JMP     LOAD2
;
; LOAD ASCII CHARACTER
;
LOAD6:  CALL    GETCH
        MOV     B,A
        JMP     LOAD4
;
; COPY BYTE FROM B TO MEMORY
; AND SEE THAT IT GOT THERE
;
CHEKM:  MOV     M,B     ;PUT IN MEM
        MOV     A,M     ;GET BACK
        CMP     B       ;SAME?
        RZ              ;YES
        JMP     ERROR   ;BAD
;
        END
