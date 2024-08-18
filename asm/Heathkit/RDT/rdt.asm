;      TITLE '   RDT  VERSION 1.0  12-29-80'
;**************
;             *
;    R D T    *
;             *
;**************

; A SELF-RELOCATING OCTAL-HEX
; CONSOLE DEBUG MONITOR
; FOR THE HEATH H8 COMPUTER

; BY  PATRICK SWAYNE
;     2155 WELCH DRIVE
;     STEVENSVILLE, MI 49127

; ASSEMBLY OPTION
; SET LABEL "INCLUDE" EQUAL TO 1 IF YOU
; DO NOT WANT TO INCLUDE THE DISASSEMBLER

;       LON     I         ; LIST CONDITIONAL CODE
INCLUDE EQU     1

; SYSCALL macro
SCALL   MACRO   call
        RST     7
        DB      call
        ENDM

; RDT COMMANDS:
; COMMAND WORDS ARE COMPLETED BY RDT
; WHEN THEIR FIRST LETTERS ARE TYPED

; COMMAND    USE

; ADd/subtract          ADD AND SUBTRACT NUMBERS
; AScii                 VIEW OR CHANGE MEMORY IN ASCII
; Base                  SET NUMBER BASE
; CHange                CHANGE MEMORY
; COmpare               COMPARE MEMORY
; Display               DISPLAY MEMORY
; Examine               EXAMINE MEMORY BY DISASSEMBLING
; Fill                  FILL MEMORY WITH A CONSTANT
; Go to                 GO TO ADDRESS, SET BREAKPOINTS
; Hex                   CONVERT OCTAL TO HEX
; Input mode            SET CHARACTER INPUT MODE FOR USER
; Load                  LOAD A FILE FROM DISK
; Move                  MOVE A BLOCK OF MEMORY
; Octal                 CONVERT HEX TO OCTAL
; Printer               CONTROL OUTPUT TO LINE PRINTER
; Register              DISPLAY AND ALTER REGISTERS
; SAve                  SAVE A FILE ON DISK
; SEarch                SEARCH MEMORY FOR A CONSTANT
; TLoad                 LOAD A BINARY CASSETTE TAPE
; TSave                 SAVE A FILE ON CASSETTE TAPE


; CONSTANTS, RAM CELLS, EXTERNAL CALLS

CR      EQU     0DH       ; CARRIAGE RETURN
LF      EQU     0AH       ; LINE FEED
RST20   EQU     0D7H      ; RESTART 2 INSTRUCTION
RS2     EQU     2022H     ; RESTART 2 VECTOR
PLOAD   EQU     01DBH     ; PAM-8 TAPE LOAD ROUTINE
TPERRX  EQU     2019H     ; TAPE ERROR EXIT ADDR
SRS     EQU     02B5H     ; SCAN FOR TAPE START
RNP     EQU     02D5H     ; READ ADDRESS FROM TAPE
WNB     EQU     0314H     ; WRITE BYTE ON TAPE
WNP     EQU     030FH     ; WRITE ADDR ON TAPE
CRCSUM  EQU     2017H     ; CHECKSUM STORAGE
PDUMP   EQU     0241H     ; PAM-8 TAPE DUMP ROUTINE
CTLFLG  EQU     2009H     ; MACHINE CONTROL FLAG
FSTART  EQU     2278H     ; MACHINE FILE START ADDRESS

; HDOS DEFINITIONS

DEXIT   EQU     0
DSCIN   EQU     1
DSCOUT  EQU     2
DPRINT  EQU     3
DREAD   EQU     4
DWRITE  EQU     5
DCONSL  EQU     6
DCLRCO  EQU     7
DLOADO  EQU     8
DCTLC   EQU     41Q
DOPENR  EQU     42Q
DOPENW  EQU     43Q
DCLOSE  EQU     46Q
DDELETE EQU     50Q
DSETTOP EQU     52Q
DERROR  EQU     57Q
DLOADD  EQU     62Q

; PROGRAM INITIALIZATION

        ORG     2280H     ; START HERE
PRESET  LXI     SP,PRESET ; TEMPORARY STACK
        LXI     H,TITLE
        CALL    PSTRNG    ; PRINT TITLE
        LXI     H,DEV
        CALL    PSTRNG
        SCALL   DCLRCO
        CALL    INCH      ; GET DEVICE DRIVER NAME
        CPI     0DH       ; NULL?
        JZ      NODEV
        STA     PNAME
        CALL    INCH
        STA     PNAME+1
        SCALL   DCLRCO
        LXI     H,PNAME
        SCALL   DLOADD    ; LOAD DEVICE DRIVER
NODEV   XRA     A
        SCALL   DLOADO    ; LOAD FIRST OVERLAY
        JC      IOERR
        MVI     A,1
        SCALL   DLOADO    ; LOAD SECOND OVERLAY
        JC      IOERR
        LXI     H,-1
        SCALL   DSETTOP   ; FIND END OF USER RAM
        IF      INCLUDE
        LXI     D,-1800H
        ELSE
        LXI     D,-1000H
        ENDIF
        DAD     D         ; GET COM-8 START ADDR
        SHLD    MVADR     ; STORE IT
        IF      INCLUDE
        LXI     D,1790H
        ELSE
        LXI     D,990H
        ENDIF
        DAD     D         ; GET TOP ADDRESS - 10H
        SCALL   DSETTOP
        JC      IOERR
        JMP     XPORT     ; MOVE CODE TO NEW LOCATION

MVADR   DS      2

; INITIALIZE USER REGISTERS , STACK,
; AND BREAKPOINT VECTOR

SETUP   LXI     SP,EXIT   ; SET RDT STACK
        LXI     H,2280H   ; SET USER STACK
        PUSH    H
        LXI     H,0
        PUSH    H         ; PUSH USER REGISTERS
        PUSH    H         ; ON STACK
        PUSH    H
        XRA     A
        STA     SSFLAG    ; CLEAR SINGLE STEPS
        INR     A
        LXI     H,RESTRT
        SCALL   DCTLC     ; SET UP CONTROL-A EXIT
        MVI     A,1       ; SET NUMBER BASE FLAG
        STA     BFLAG     ; FOR OCTAL
        MVI     A,0C3H
        STA     RS2
        LXI     H,RSTB    ; SET UP RESTART 2
        SHLD    RS2+1     ; FOR BREAKPOINTS

; MAIN COMMAND LOOP

START   EI
        CALL    FIXIN     ; FIX INPUT MODE
        CALL    CRLF      ; TYPE CR AND LF
        MVI     A,']'     ; PROMPT WITH "]"
        STA     MMFLAG    ; IN MONITOR MODE
        STA     ASFLAG
        CALL    OUTCH     ; PRINT PROMPT
        MVI     C,2       ; PARAMETER CONTROL
        CALL    ECHO      ; GET A CHARACTER
        CPI     'A'       ; TRY TO MATCH ENTRY
        JZ      ACMD
        CPI     'B'
        JZ      BASE
        CPI     'C'
        JZ      CCMD
        CPI     'D'
        JZ      DISP
        IF      INCLUDE
        CPI     'E'
        JZ      EXAM
        ENDIF
        CPI     'F'
        JZ      FILL
        CPI     'G'
        JZ      GO
        CPI     'H'
        JZ      HEX
        CPI     'I'
        JZ      INPUT
        CPI     'L'
        JZ      LOAD
        CPI     'M'
        JZ      MOVE
        CPI     'O'
        JZ      OCT
        CPI     'P'
        JZ      PCMD
        CPI     'R'
        JZ      REG
        CPI     'S'
        JZ      SCMD
        CPI     'T'
        JZ      TCMD
        CPI     CR
        JZ      START
        JMP     ERROR     ; BAD ENTRY

; LETTER "A" COMMANDS

ACMD    CALL    ECHO    ; GET RESPONSE
        CPI     'D'     ; ADD/SUBTRACT?
        JZ      ADD
        CPI     'S'     ; ASCII?
        JZ      ASCII
        JMP     ERROR   ; BAD ENTRY

; ADD AND SUBTRACT HEX OR OCTAL NUMBERS
; IF OCTAL, RESULTS OVER 377Q ARE IN
; SPLIT OCTAL

ADD     LXI     H,DD      ; COMPLETE COMMAND
        CALL    PSTRNG
        CALL    GETLN     ; GET COMMAND LINE
        CALL    DATA      ; GET TWO NUMBERS
        CALL    CRLF      ; PRINT CR,LF
        POP     D         ; SECOND NUMBER
        POP     H         ; FIRST NUMBER
        PUSH    H         ; SAVE IT
        DAD     D         ; ADD THEM
        CALL    PADR      ; PRINT RESULTS
        CALL    SPACE     ; SPACE BETWEEN NUMBERS
        POP     H         ; RESTORE FIRST NUMBER
        MOV     A,L       ; SUBTRACT NUMBERS
        SUB     E
        MOV     L,A
        MOV     A,H
        SBB     D
        MOV     H,A
        CALL    PADR      ; PRINT RESULTS
        JMP     START

; EXAMINE OR ALTER MEMORY IN ASCII

ASCII   LXI     H,CII
        CALL    PSTRNG    ; COMPLETE COMMAND
        CALL    GETLN     ; GET COMMAND LINE
        XRA     A
        STA     ASFLAG    ; SET FLAG FOR ASCII CMD
        DCR     C
        CALL    DATA      ; GET ADDRESS
        CALL    FIXIN     ; FIX INPUT MODE
        POP     H
ASCII0  CALL    CRLF
        CALL    PADR      ; PRINT ADDRESS
        CALL    SPACE     ; SPACE AFTER
        CALL    SPACE
        MOV     A,M       ; GET WHAT'S THERE
        ANI     7FH       ; STRIP PARITY
        CPI     20H       ; LESS THAN SPACE
        JNC     ASCII1    ; CONVERT CONTROL CHARS
        PUSH    PSW       ; SAVE CHARACTER
        MVI     A,'^'
        CALL    OUTCH     ; PRINT "^"
        POP     PSW       ; RESTORE CHARACTER
        ADI     40H       ; CONVERT TO CONTROL TO LETTER
ASCII1  CALL    OUTCH     ; PRINT CHARACTER
        MVI     A,'/'
        CALL    OUTCH     ; PRINT SLASH
        CALL    INCH      ; GET RESPONSE
        CPI     0DH       ; CR?
        JZ      START     ; EXIT
        PUSH    PSW
        CPI     20H       ; CONTROL CHARACTER
        JNC     ASCII2
        MVI     A,'^'
        CALL    OUTCH
        POP     PSW
        PUSH    PSW
        ADI     40H
ASCII2  CALL    OUTCH     ; ECHO CHARACTER
        POP     PSW
        CPI     '-'       ; MINUS?
        DCX     H         ; BACK UP POINTER
        JZ      ASCII0    ; PRINT PREVIOUS DATA
        INX     H         ; RESTORE POINTER
        CPI     '/'       ; SLASH?
        JZ      ASCII3    ; DELIMITER, NO CHANGE
        MOV     M,A       ; STORE CHARACTER
        CALL    ECHO      ; GET ANOTHER CHARACTER
        CPI     0DH       ; CR?
        JZ      START
        CPI     '/'       ; SLASH?
        JZ      ASCII3
        DCX     H
        CPI     '-'       ; BACK UP?
        JZ      ASCII0
        JMP     ERROR     ; BAD RESPONSE
ASCII3  INX     H         ; GET NEXT ADDRESS
        JMP     ASCII0

; SET NUMBER BASE

BASE    LXI     H,ASE     ; COMPLETE COMMAND
        CALL    PSTRNG
        CALL    ECHO
        CPI     'H'       ; WANT HEX?
        JNZ     BASE0     ; NO, OCTAL
        LXI     H,EX
        CALL    PSTRNG    ; PRINT "HEX"
        XRA     A
        STA     BFLAG     ; SET FLAG FOR HEX
        JMP     START
BASE0   CPI     'O'       ; WANT OCTAL?
        JNZ     ERROR     ; BAD ENTRY
        LXI     H,CTAL
        CALL    PSTRNG    ; PRINT "OCTAL"
        MVI     A,1
        STA     BFLAG     ; SET FLAG FOR OCTAL
        JMP     START

; LETTER "C" COMMANDS

CCMD    CALL    ECHO      ; GET SECOND CHARACTER
        CPI     'H'       ; CHANGE?
        JZ      CHG
        CPI     'O'       ; COMPARE?
        JZ      COMP
        JMP     ERROR     ; BAD ENTRY

; CHANGE MEMORY

CHG     LXI     H,NGE     ; COMPLETE COMMAND
        CALL    PSTRNG
        DCR     C         ; GET ONE PARAMETER
        CALL    GETLN     ; GET COMMAND INFO
        CALL    DATA
        CALL    FIXIN     ; FIX INPUT MODE
        POP     H         ; ADDRESS OF FIRST CHANGE
CHG0    CALL    CRLF
        CALL    PADR      ; PRINT CURRENT ADDRESS
        CALL    SPACE     ; PRINT SPACES
        CALL    SPACE
        MOV     A,M
        CALL    PBYTE     ; PRINT CURRENT BYTE
        MVI     A,'/'
        CALL    OUTCH     ; PRINT SLASH
        CALL    ECHO      ; GET A CHARACTER
        CPI     '-'       ; MINUS?
        DCX     H         ; BACK UP POINTER
        JZ      CHG0      ; SHOW PREVIOUS ADDRESS
        INX     H         ; FIX POINTER
        CALL    CHECK0    ; CHECK CHARACTER
        JC      START     ; CR ENTERED, EXIT
        JZ      CHG1      ; COMMA OR SPACE, NO CHANGE
        PUSH    H         ; SAVE ADDR
        CALL    CDATA     ; GET NEW VALUE
        POP     D         ; VALUE
        POP     H         ; ADDRESS
        MOV     M,E       ; MAKE CHANGE
        MOV     A,E
        CMP     M
        JNZ     ERROR     ; BAD RAM
        MOV     A,B       ; GET DELIMITER
        CPI     '-'       ; MINUS?
        DCX     H         ; DCR POINTER
        JZ      CHG0      ; GET PREVIOUS BYTE
        INX     H         ; FIX POINTER
        CPI     CR        ; CR ENTERED?
        JZ      START     ; EXIT
CHG1    INX     H
        JMP     CHG0

; COMPARE TWO BLOCKS OF MEMORY

COMP    LXI     H,MPAR
        CALL    PSTRNG    ; COMPLETE NAME
        INR     C         ; GET THREE PARAMETERS
        CALL    GETLN     ; GET COMMAND INFO
        CALL    DATA
        CALL    CRLF
        POP     B         ; BLOCK 2 START
        POP     D         ; BLOCK 1 END
        POP     H         ; BLOCK 1 START
COMP0   LDAX    B         ; GET BYTE FROM BLOCK 2
        CMP     M         ; COMPARE WITH BLOCK 1
        JZ      COMP1     ; MATCH
        PUSH    H         ; SAVE HL
        MOV     H,B       ; PUT BC IN HL
        MOV     L,C
        CALL    PADR      ; PRINT MISMATCH ADDRESS
        POP     H         ; RESTORE HL
        CALL    CRLF
COMP1   INX     B         ; INCREMENT BLOCK 2 POINTER
        CALL    CPHD      ; END OF BLOCK?
        JNC     COMP0     ; NO, CONTINUE
        JMP     START     ; END, EXIT

; DISPLAY MEMORY ON CONSOLE

DISP    LXI     H,ISP     ; COMPLETE COMMAND
        CALL    PSTRNG
        CALL    GETLN
        CALL    DATA      ; GET TWO ADDRESSES
        POP     D         ; HIGH ADDRESS
        POP     H         ; LOW ADDRESS
DISP0   CALL    CRLF
        CALL    PADR      ; PRINT CURRENT ADDRESS
        CALL    SPACE
DISP1   CALL    SPACE
        MOV     A,M       ; GET CURRENT BYTE
        CALL    PBYTE     ; PRINT IT
        CALL    CPHD      ; TEST FOR COMPLETION
        JC      START     ; DONE, EXIT
        LDA     BFLAG     ; WHAT BASE?
        ORA     A
        MVI     B,0FH     ; HEX -- 16 BYTES/LINE
        JZ      DISP2
        MVI     B,07H     ; OCTAL -- 8 BYTES/LINE
DISP2   MOV     A,L
        ANA     B         ; END OF LINE?
        JNZ     DISP1     ; NO, CONTINUE
        JMP     DISP0     ; START NEW LINE

; EXAMINE MEMORY BY DISASSEMBLING IT

        IF      INCLUDE
        INCLUDE dis.acm
        ENDIF

; FILL RAM MEMORY WITH A CONSTANT

FILL    LXI     H,ILL     ; COMPLETE COMMAND
        CALL    PSTRNG
        INR     C         ; GET THREE PARAMETERS
        CALL    GETLN     ; GET COMMAND INFO
        CALL    DATA
        POP     B         ; CONSTANT IN C
        POP     D         ; HIGH ADDRESS
        POP     H         ; LOW ADDRESS
FILL0   MOV     M,C       ; INSERT NEW DATA
        MOV     A,C
        CMP     M         ; CHECK RAM
        JNZ     ERROR     ; BAD RAM
        CALL    CPHD      ; END OF BLOCK?
        JNC     FILL0     ; NO, CONTINUE
        JMP     START     ; EXIT

; GO TO USER PROGRAM
; OPTIONALLY SET BREAKPOINTS

GO      LXI     H,OTO     ; COMPLETE COMMAND
        CALL    PSTRNG
SSGO    CALL    GETLN     ; GET COMMAND INFO
        CALL    CHECK     ; GET A CHARACTER
        JC      GO2       ; CR ENTERED, EXIT TO PGM
        JZ      GO0       ; SET BREAKPOINTS
        CALL    CDATA     ; GET NEW PC VALUE
        POP     D         ; NEW VALUE
        LXI     H,PCTR
        DAD     SP        ; LOCATE "PC" ADDRESS
        MOV     M,D
        DCX     H
        MOV     M,E       ; STORE VALUE
        MOV     A,B       ; GET DELIMITER
        CPI     CR        ; SET BREAKPOINTS?
        JZ      GO2       ; NO, EXECUTE USER PGM
GO0     MVI     D,2       ; SET 1 OR 2 BREAKPOINTS
        LXI     H,TRAP
        DAD     SP        ; LOCATE TRAP ADDRESS
GO1     PUSH    H         ; SAVE TRAP ADDRESS
        PUSH    D         ; SAVE DE
        MVI     C,1
        CALL    DATA      ; GET A BREAKPOINT ADDR
        MOV     A,B       ; SAVE DELIMITER
        POP     B         ; BREAKPOINT ADDRESS
        POP     D         ; RESTORE DE
        MOV     E,A       ; DELIMITER IN E
        POP     H         ; PLACE TO STORE IT
        MOV     M,C       ; STORE ADDRESS
        INX     H
        MOV     M,B
        INX     H
        LDAX    B         ; GET OPCODE
        MOV     M,A       ; STORE IT
        INX     H
        MVI     A,RST20   ; RESTART 2
        STAX    B         ; PUT IN OPCODE'S PLACE
        MOV     A,E       ; TEST DELIMITER
        CPI     CR
        JZ      GO2       ; DONE, EXIT TO PGM
        DCR     D         ; SET ANOTHER BREAKPOINT?
        JNZ     GO1       ; YES, GET IT
GO2     LDA     CONFLG    ; GET CONSOLE MODE FLAG
        ORA     A         ; LINE MODE?
        JZ      GO3
        XRA     A
        LXI     B,8181H
        SCALL   DCONSL    ; SET CONSOLE FOR CHAR MODE
GO3     LDA     SSFLAG
        ORA     A         ; IN SINGLE STEP MODE?
        JNZ     SSTEP0    ; IF SO, GO TO STEPPER
        SCALL   DCLRCO
        XRA     A
        STA     MMFLAG    ; IN USER MODE
        JMP     EXIT      ; GO TO USER PGM

; SET CHARACTER INPUT MODE

INPUT   LXI     H,NPUT
        CALL    PSTRNG    ; COMPLETE COMMAND
        CALL    ECHO      ; GET RESPONSE
        MVI     B,0
        CPI     'L'       ; LINE MODE?
        JZ      LINE
        CPI     'C'       ; CHAR MODE?
        JNZ     ERROR     ; BAD ENTRY
        INR     B
        LXI     H,CHAR
        CALL    PSTRNG    ; PRINT "CHARACTER"
        JMP     SETIN     ; SET THE FLAG
LINE    LXI     H,LINEM
        CALL    PSTRNG    ; PRINT "LINE"
SETIN   MOV     A,B
        STA     CONFLG    ; SET THE CONSOLE FLAG
        JMP     START

; CONVERT OCTAL TO HEX

HEX     LXI     H,EX      ; COMPLETE COMMAND
        CALL    PSTRNG
        DCR     C         ; GET ONE NUMBER
        CALL    GETLN
        CALL    ODATA     ; IN OCTAL
        CALL    CRLF
        POP     H         ; NUMBER TO CONVERT
        MOV     A,H
        CALL    HBYTE     ; CONVERT HIGH BYTE
        MOV     A,L
        CALL    HBYTE     ; CONVERT LOW BYTE
        JMP     START

; LOAD A FILE FROM DISK

LOAD    LXI     H,OAD
        CALL    PSTRNG    ; COMPLETE COMMAND
        CALL    GETLN     ; LINE INPUT MODE
        CALL    GETNAM    ; GET FILE NAME
        MVI     A,1
        LXI     D,DEFALT
        LXI     H,FNAME
        SCALL   DOPENR    ; OPEN THE FILE
        JC      IOERR
        MVI     A,1
        STA     FSTAT     ; FLAG FILE OPEN
        LXI     B,256     ; READ ONE SECTOR
        LXI     D,FSTART  ; PUT IT HERE
        SCALL   DREAD
        JNC     LOAD0
        CPI     1
        JNZ     IOERR
LOAD0   MVI     A,1
        CALL    CLOSE     ; FLAG FILE CLOSED
        SCALL   DCLOSE    ; CLOSE THE FILE
        JC      IOERR
        LHLD    FSTART    ; GET ID CODE
        MOV     A,L
        CPI     0FFH      ; IS IT MACHINE FILE?
        JNZ     TERROR    ; TYPE ERROR
        MOV     A,H
        ORA     A
        JNZ     TERROR
        LHLD    FSTART+2  ; GET FILE START
        XCHG              ; IN DE
        LHLD    FSTART+4  ; GET FILE SIZE
        DAD     D         ; ADD TO START
        SHLD    FEND      ; STORE TOP OF FILE
        XCHG              ; TOP OF FILE IN DE
        INR     D         ; ADD 256
        LHLD    TOP       ; GET TOP OF AVAILABLE RAM
        XCHG              ; RAM TOP IN DE, FILE TOP IN HL
        CALL    CPHD      ; CHECK IF ENOUGH RAM
        JC      TOOBIG    ; FILE TOO BIG
        LHLD    FSTART+6  ; GET PC START ADDR
        XCHG              ; IN DE
        LXI     H,PCTR
        DAD     SP        ; LOCATE USER PC
        MOV     M,D       ; SET TO PROGRAM START
        DCX     H
        MOV     M,E
        MVI     A,1
        LXI     D,DEFALT
        LXI     H,FNAME
        SCALL   DOPENR    ; OPEN THE FILE
        JC      IOERR
        LHLD    FSTART+2  ; GET PROGRAM START
        MVI     D,0FFH
        MVI     E,0F8H    ; -8 IN DE
        DAD     D         ; SUBTRACT 8
        SHLD    FBGN      ; STORE FILE BEGINNING
        CALL    NEG       ; MAKE IT NEGATIVE
        LHLD    TOP       ; GET TOP OF RAM
        DAD     D         ; SUBTRACT BEGINNING
        MOV     B,H       ; HIGH BYTE IN B
        DCR     B
        MVI     C,0       ; MULTIPLE OF 256
        LHLD    FBGN      ; GET BEGINNING
        XCHG              ; PUT IT IN DE
        MVI     A,1
        STA     FSTAT     ; FLAG FILE OPEN
        SCALL   DREAD     ; READ IN THE FILE
        JNC     LOAD1
        CPI     1
        JNZ     IOERR
LOAD1   MVI     A,1
        CALL    CLOSE     ; FLAG FILE CLOSED
        SCALL   DCLOSE    ; CLOSE THE FILE
        JC      IOERR
        LXI     H,STARTS
        CALL    PSTRNG    ; PRINT "STARTS"
        LHLD    FSTART+2
        CALL    PADR      ; PRINT START ADDRESS
        LXI     H,ENDS
        CALL    PSTRNG    ; PRINT "ENDS"
        LHLD    FEND      ; GET FILE END
        CALL    PADR      ; PRINT END ADDRESS
        JMP     START

CLOSE   DCR     A
        STA     FSTAT     ; FLAG FILE CLOSED
        INR     A         ; FIX A
        RET

TERROR  LXI     H,TYPE    ; GET TYPE ERROR MSG
        CALL    PSTRNG    ; PRINT IT
        JMP     START
TOOBIG  LXI     H,TBIG    ; GET TOO BIG MSG
        CALL    PSTRNG
        JMP     START

; GET DISK FILE NAMES

GETNAM  LXI     H,FNAME   ; POINT TO BUFFER
GETNA0  CALL    INCH      ; GET A CHARACTER
        MOV     M,A       ; STORE CHARACTER
        INX     H         ; INCR POINTER
        CPI     CR        ; CR?
        JZ      GETNA1    ; GET CHARACTERS UNTIL CR
        CPI     ','       ; COMMA?
        JNZ     GETNA0    ; COMMA CAN END FILE ALSO
GETNA1  DCX     H         ; DCR POINTER
        MVI     M,0       ; TERMINATE NAME WITH ZERO
        RET

; CASSETTE TAPE COMMAND PROCESSOR

TCMD    CALL    ECHO      ; GET SECOND CHARACTER
        CPI     'L'       ; LOAD?
        JZ      TLOAD
        CPI     'S'       ; SAVE?
        JZ      TSAVE
        CALL    OUTCH
        JMP     ERROR     ; BAD COMMAND

; LOAD CASSETTE TAPE

TLOAD   LXI     H,OAD     ; COMPLETE COMMAND
        CALL    PSTRNG
        CALL    CHECK     ; GET CHARACTER
        JNC     ERROR     ; NOT A CR, EXIT
        CALL    CRLF
        LXI     H,ERROR
        SHLD    TPERRX    ; SET UP ERROR EXIT
        LXI     H,START   ; GET "RETURN" ADDRESS
        PUSH    H         ; PUT IT ON STACK
        MVI     B,376Q    ; TYPE AND NUMBER
        MVI     C,0       ; IN BC
        CALL    SRS       ; FIND START OF RECORD
        MOV     L,A       ; HL = COUNT
        XCHG              ; DE = COUNT, HL = TYPE
        DCR     C         ; C = NEXT NO.
        DAD     B
        MOV     A,H
        PUSH    B         ; SAVE TYPE AND NO.
        PUSH    PSW       ; SAVE TYPE CODE
        ANI     7FH       ; CLEAR FLAG BIT
        ORA     L
        JNZ     ERROR     ; SEQ. ERROR
        CALL    RNP
        MOV     B,H
        MOV     C,A       ; BC = PGM COUNTER
        LXI     H,PCTR+6  ; GET RDT PC
        DAD     SP        ; STORAGE ADDR
        MOV     M,B
        DCX     H
        MOV     M,C       ; STORE PC
        JMP     PLOAD     ; CALL PAM-8 LOAD

; MOVE A BLOCK OF MEMORY

MOVE    LXI     H,OVE     ; COMPLETE COMMAND
        CALL    PSTRNG
        INR     C         ; GET THREE PARAMETERS
        CALL    GETLN
        CALL    DATA
        POP     B         ; DESTINATION
        POP     D         ; SOURCE END
        POP     H         ; SOURCE BEGINNING
MOVE0   MOV     A,M       ; GET DATA BYTE
        STAX    B         ; STORE AT DESTINATION
        INX     B         ; MOVE DESTINATION POINTER
        CALL    CPHD      ; TEST FOR COMPLETION
        JNC     MOVE0     ; NOT FINISHED
        JMP     START     ; EXIT

; CONVERT HEX TO OCTAL

OCT     LXI     H,CTAL    ; COMPLETE COMMAND
        CALL    PSTRNG
        DCR     C         ; GET ONE NUMBER
        CALL    GETLN
        CALL    HDATA
        CALL    CRLF
        POP     H         ; NUMBER TO CONVERT
        MOV     A,H       ; GET FIRST BYTE
        CALL    OBYTE     ; CONVERT IT
        MVI     A,'.'
        CALL    OUTCH     ; PRINT "." BETWEEN BYTES
        MOV     A,L       ; GET SECOND BYTE
        CALL    OBYTE     ; CONVERT IT
        JMP     START

; PROCESS P COMMANDS

PCMD    CALL    ECHO      ; GET NEXT CHARACTER
        CPI     'O'
        JZ      PORT
        CPI     'R'
        JZ      PRINT
        JMP     ERROR     ; BAD ENTRY

; PORT COMMANDS

PORT    LXI     H,RT
        CALL    PSTRNG    ; COMPLETE COMMAND
        CALL    ECHO      ; GET RESPONSE
        CPI     'O'
        JZ      OUT
        CPI     'I'
        JNZ     ERROR

; INPUT FROM PORT

        LXI     H,IN
        CALL    PSTRNG
        CALL    GETLN
        DCR     C
        CALL    DATA      ; GET PORT ADDRESS
        POP     H
        MOV     A,L
        STA     INPORT
        CALL    FIXIN     ; FIX INPUT MODE (CHAR)
IN0     CALL    CRLF
        IN      0
INPORT  EQU     $-1
        CALL    PBYTE     ; PRINT DATA AT PORT
        CALL    CHECK     ; GET A CHARACTER
        JC      START     ; CR, EXIT
        JZ      IN0       ; DELIMITER, CONTINUE
        JMP     ERROR

; OUTPUT TO PORT

OUT     LXI     H,UT
        CALL    PSTRNG
        CALL    GETLN
        DCR     C
        CALL    DATA      ; GET PORT ADDRESS
        POP     H
        MOV     A,L
        STA     OUTPORT
        CALL    FIXIN     ; FIX INPUT MODE (CHAR)
OUT0    CALL    CRLF
        MVI     A,'='
        CALL    OUTCH     ; PRINT "="
        CALL    SPACE     ; SPACE AFTER
        CALL    CHECK     ; CHECK RESPONSE
        JC      START     ; CR, EXIT
        CALL    CDATA     ; GET DATA
        POP     H
        MOV     A,L
        OUT     0         ; SEND DATA TO PORT
OUTPORT EQU     $-1
        MOV     A,B       ; GET DELIMITER
        CALL    CHECK0    ; CHECK IT
        JC      START     ; CR, EXIT
        JZ      OUT0      ; DELIMITER, CONTINUE
        JMP     ERROR

; PRINTER CONTROL

PRINT   LXI     H,RINTER  ; COMPLETE COMMAND
        CALL    PSTRNG
        CALL    ECHO      ; GET RESPONSE
        CPI     'N'
        JZ      NOPRNT
        CPI     'Y'
        JNZ     ERROR

; TURN PRINTER ON

        LDA     PFLAG     ; GET PRINTER FLAG
        ORA     A         ; IS IT SET
        JNZ     START     ; PRINTER ALREADY ON
        MVI     B,0
        MOV     C,B       ; C IS A COUNTER
        LXI     H,PBUFF
        SHLD    PBPTR
PLOOP   MOV     M,B       ; CLEAR PRINTER BUFFER
        INX     H
        DCR     C         ; DCR COUNTER
        JNZ     PLOOP     ; LOOP UNTIL DONE
        MVI     A,2
        LXI     D,PDFLT
        LXI     H,PNAME
        SCALL   DOPENW    ; OPEN THE DRIVER
        JC      IOERR
        MVI     A,2
        STA     PFLAG     ; FLAG PRINTER ON
        DCR     A
        STA     PNCTR     ; SET PRINT COUNTER
        JMP     START

;TURN PRINTER OFF

NOPRNT  LDA     PFLAG     ; GET PRINTER FLAG
        ORA     A         ; IS IT SET?
        JZ      START     ; PRINTER ALREADY OFF
        MVI     A,0AH
        CALL    LPRINT    ; PRINT CRLF
        CALL    RESET     ; EMPTY THE BUFFER
        MVI     A,2
        SCALL   DCLOSE    ; CLOSE THE DRIVER
        JC      IOERR
        XRA     A
        STA     PFLAG     ; FLAG PRINTER OFF
        JMP     START

; LPRINT -- SEND CHARACTERS TO A HARDCOPY DEVICE
; THIS ROUTINE IS CALLED EACH TIME A CHARACTER
; IS SENT TO THE CONSOLE IF "PFLAG" IS SET.
; IT PUTS THE CHARACTER INTO A BUFFER, AND WHEN IT
; IS FULL, IT SENDS THE BUFFER TO THE PRINTER.

LPRINT  PUSH    H
        PUSH    D
        PUSH    B         ; SAVE ALL REGISTERS
        PUSH    PSW       ; SAVE CHARACTER
        LDA     RFLAG
        ORA     A         ; BUFFER FULL?
        CZ      RESET     ; IF SO, PRINT
        POP     PSW       ; RESTORE CHAR
        LHLD    PBPTR
        MOV     M,A       ; STORE CHARACTER IN BUFFER
        INX     H
        SHLD    PBPTR
        LDA     PNCTR
        DCR     A         ; DECREMENT COUNTER
        STA     PNCTR
        JNZ     LPEXIT
        STA     RFLAG     ; FLAG BUFFER FULL
LPEXIT  POP     B         ; RESTORE REGISTERS
        POP     D
        POP     H
        RET

RESET   MVI     A,2
        LXI     B,256
        LXI     D,PBUFF
        SCALL   DWRITE    ; PRINT BUFFER CONTENTS
        JC      IOERR
        MVI     B,0
        MOV     C,B
        LXI     H,PBUFF
        SHLD    PBPTR
RESLP   MOV     M,B       ; CLEAR BUFFER
        INX     H
        DCR     C         ; DECREMENT COUNTER
        JNZ     RESLP
        MVI     A,1
        STA     RFLAG     ; FLAG BUFFER EMPTY
        RET

; EXAMINE AND MODIFY CPU REGISTERS

REG     LXI     H,EGIS    ; COMPLETE COMMAND
        CALL    PSTRNG
        CALL    ECHO      ; GET REGISTER IDENTIFIER
        LXI     H,TABLE   ; POINT TO REG TABLE
        CPI     CR        ; DISPLAY ALL REGISTERS?
        JNZ     REG0      ; NO, GET SINGLE REGISTER
        CALL    CRLF
        CALL    REG6      ; PRINT ALL REGISTERS
        JMP     START     ; EXIT
REG0    CMP     M         ; FOUND REG IN TABLE?
        INX     H
        JZ      REG1      ; YES, DISPLAY VALUE
        PUSH    PSW       ; SAVE CHARACTER
        MOV     A,M       ; TEST FOR END OF TABLE
        ORA     A
        JM      ERROR     ; BAD ENTRY
        POP     PSW       ; RESTORE CHARACTER
        INX     H
        INX     H
        JMP     REG0      ; TRY NEXT TABLE REG
REG1    CALL    SPACE
        CALL    GETLN     ; GET INFO
        MOV     A,M       ; DISPLACEMENT POINTER
        XCHG              ; SAVE POINTER
        MOV     L,A
        MVI     H,0
        DAD     SP        ; LOCATE REGISTER VALUE
        XCHG              ; RESTORE POINTER
        INX     H
        MOV     B,M       ; GET BYTE COUNT
        INX     H         ; POINT TO NEXT REG
        LDAX    D         ; GET VALUE FOR DISPLAY
        CALL    PBYTE     ; DISPLAY 8 BITS
        DCR     B         ; 16 - BIT REGISTER?
        JZ      REG3      ; NO
        LDA     BFLAG
        ORA     A         ; HEX OR OCTAL?
        JZ      REG2      ; HEX
        MVI     A,'.'     ; OCTAL, PRINT PERIOD
        CALL    OUTCH     ; BETWEEN BYTES
REG2    DCX     D
        LDAX    D         ; GET REST OF REG
        CALL    PBYTE     ; DISPLAY IT
REG3    INR     B         ; RESTORE COUNT
        MVI     A,'/'
        CALL    OUTCH     ; PRINT SLASH
        CALL    CHECK     ; WANT TO MODIFY?
        JC      START     ; CR ENTERED, EXIT
        JZ      ERROR     ; BAD ENTRY
        PUSH    B         ; SAVE COUNT
        PUSH    D         ; SAVE REG VALUE ADDR
        CALL    CDATA     ; GET NEW VALUE
        POP     H         ; NEW VALUE
        POP     D         ; VALUE ADDR
        MOV     A,L       ; GET NEW VALUE LSB
        STAX    D         ; STORE IT
        POP     B         ; COUNT
        DCR     B         ; 2 - BYTE REGISTER?
        JZ      REG4      ; YES
        INX     D         ; NO
        MOV     A,H       ; GET MSB
        STAX    D         ; STORE IT
REG4    JMP     START
REG6    CALL    CRLF      ; START FULL REG DISPLAY
REG7    CALL    SPACE     ; SPACE BETWEEN REGISTERS
        MOV     A,M       ; GET REG CHARACTER
        INX     H         ; POINT TO DISPLACEMENT
        ORA     A         ; DONE?
        RM                ; YES, EXIT
        CALL    OUTCH     ; PRINT REG NAME
        MVI     A,'='
        CALL    OUTCH     ; PRINT EQUAL SIGN
        MOV     A,M       ; GET DISPLACEMENT
        INX     H         ; POINT TO COUNT
        XCHG              ; SAVE POINTER
        INR     A         ; CORRECT POINTER FOR
        INR     A         ;   ONE STACK USAGE
        MOV     L,A
        MVI     H,0       ; GET ADDRESS
        DAD     SP        ; OF REGISTER VALUE
        XCHG              ; HL = POINTER, DE = ADDR
        MOV     B,M       ; GET COUNT
        INX     H         ; POINT TO NEXT REG
        LDAX    D         ; GET LSB OF REGISTER
        CALL    PBYTE     ; DISPLAY IT
        DCR     B         ; 2 - BYTE REGISTER?
        JZ      REG7      ; NO, GET NEXT REGISTER
        LDA     BFLAG
        ORA     A         ; HEX OR OCTAL?
        JZ      REG8      ; HEX
        MVI     A,'.'     ; OCTAL, PRINT PERIOD
        CALL    OUTCH     ; BETWEEN BYTES
REG8    DCX     D         ; POINT TO MSB
        LDAX    D         ; GET IT
        CALL    PBYTE     ; DISPLAY IT
        JMP     REG7      ; CONTINUE

; LETTER S COMMANDS

SCMD    CALL    ECHO      ; GET SECOND CHARACTER
        CPI     'A'       ; SAVE?
        JZ      SAVE
        CPI     'E'       ; SEARCH?
        JZ      SERCH
        CPI     'I'       ; SINGLE STEP?
        JZ      SSTEP
        JMP     ERROR

; SAVE FILE TO DISK

SAVE    LXI     H,AVE+1
        CALL    PSTRNG    ; COMPLETE COMMAND
        CALL    GETLN     ; LINE INPUT MODE
        CALL    GETNAM    ; GET FILE NAME
        INR     C         ; GET 3 PARAMETERS
        CALL    DATA
        MVI     A,1
        LXI     D,DEFALT
        LXI     H,FNAME
        SCALL   DOPENW    ; OPEN THE FILE
        JC      IOERR
        POP     B         ; GET PC
        POP     H         ; GET FILE END
        SHLD    FEND
        POP     H         ; GET FILE BEGINNING
        SHLD    FBGN      ; STORE IT
        MOV     D,H       ; BEGINNING IN DE
        MOV     E,L
        PUSH    D         ; SAVE DE
        MVI     D,0FFH    ; -8 IN DE
        MVI     E,0F8H
        DAD     D         ; SUBTRACT 8 FROM START
        POP     D         ; RESTORE DE
        MVI     M,0FFH    ; INSERT ID CODE
        INX     H
        MVI     M,0
        INX     H
        MOV     M,E       ; INSERT START ADDR
        INX     H
        MOV     M,D
        INX     H
        INX     H
        INX     H
        MOV     M,C       ; INSERT PC VALUE
        INX     H
        MOV     M,B
        LHLD    FBGN      ; GET FILE BEGINNING
        CALL    NEG       ; MAKE IT NEGATIVE
        LHLD    FEND      ; GET FILE END
        DAD     D         ; SUBTRACT BEGINNING
        MOV     B,H       ; RESULT IN BC
        MOV     C,L
        LHLD    FBGN      ; GET FILE BEGINNING
        DCX     H         ; MOVE TO SIZE STORAGE
        DCX     H
        DCX     H
        MOV     M,B       ; STORE SIZE
        DCX     H
        MOV     M,C
        MOV     H,B       ; SIZE IN HL
        MOV     L,C
        LXI     B,8
        DAD     B         ; ADD 8
        MOV     B,H       ; CORRECT SIZE IN BC
        INR     B         ; MAKE MULTIPLE OF 256
        MVI     C,0
        LHLD    FBGN      ; GET FILE BEGINNING
        MVI     D,0FFH
        MVI     E,0F8H    ; -8 IN DE
        DAD     D         ; SUBTRACT 8
        XCHG              ; BEGINNING IN DE
        MVI     A,1
        STA     FSTAT     ; FLAG FILE OPEN
        SCALL   DWRITE    ; WRITE THE FILE
        JC      IOERR
        MVI     A,1
        CALL    CLOSE     ; FLAG FILE CLOSED
        SCALL   DCLOSE    ; CLOSE THE FILE
        JC      IOERR
        JMP     START

; MAKE HL NEGATIVE -- RESULT IN DE

NEG     MOV     A,L
        CMA               ; COMPLEMENT L
        MOV     E,A
        MOV     A,H
        CMA               ; COMPLEMENT H
        MOV     D,A
        INX     D         ; CORRECT RESULT
        RET

; SEARCH MEMORY FOR A BYTE OR WORD

SERCH   LXI     H,ARCH    ; COMPLETE COMMAND
        CALL    PSTRNG
        CALL    ECHO      ; GET A CHARACTER
        CPI     'B'       ; BYTE SEARCH?
        MVI     B,0       ; SET FLAG
        JNZ     SERCH0    ; WORD SEARCH
        LXI     H,YTE
        CALL    PSTRNG    ; PRINT "BYTE"
        JMP     SERCH1
SERCH0  CPI     'W'       ; WORD SEARCH?
        JNZ     ERROR     ; BAD ENTRY
        LXI     H,ORD
        CALL    PSTRNG    ; PRINT "WORD"
        INR     B         ; SET FLAG
SERCH1  MOV     A,B
        STA     STYPE     ; STORE FLAG
        INR     C         ; GET THREE PARAMETERS
        CALL    GETLN     ; LINE INPUT
        CALL    DATA
        POP     B         ; SEARCH VALUE
        POP     D         ; END OF SEARCH AREA
        POP     H         ; START OF SEARCH AREA
SERCH2  MOV     A,M       ; LOOK FOR MATCH
        CMP     C         ; GOT ONE?
        CZ      SERCH3    ; PROCESS MATCH
        CALL    CPHD      ; TEST FOR COMPLETION
        JNC     SERCH2    ; CONTINUE
        JMP     START     ; DONE, EXIT
SERCH3  LDA     STYPE     ; GET FLAG
        ORA     A         ; BYTE SEARCH?
        JNZ     SERCH5    ; NO, WORD
SERCH4  CALL    CRLF
        CALL    PADR      ; PRINT ADDRESS OF MATCH
        RET
SERCH5  INX     H         ; CHECK NEXT BYTE FOR
        MOV     A,M       ; MATCH ALSO
        CMP     B
        DCX     H         ; BACK UP
        RNZ               ; NOT A MATCH
        JMP     SERCH4    ; PRINT ADDRESS OF MATCH

; SINGLE STEP COMMAND

SSTEP   LXI     H,NGLE
        CALL    PSTRNG    ; COMPLETE COMMAND
        MVI     A,1
        STA     SSFLAG    ; PRIME SINGLE STEP FLAG
        JMP     SSGO      ; GET ADDRESSES AND GO
SSTEP0  LXI     H,TRAP    ; FIND WHERE COUNT IS STORED
        DAD     SP
        MOV     A,M       ; GET COUNT
        STA     SSFLAG    ; STORE IN SSFLAG
SSTEP1  LDA     SSFLAG    ; GET STEP COUNT
        DCR     A         ; DECREMENT IT
        STA     SSFLAG    ; STORE NEW COUNT
        XRA     A
        STA     MMFLAG    ; IN USER MODE
        DI                ; DISABLE INTERRUPTS
        LDA     CTLFLG    ; GET CONTROL FLAG
        XRI     20Q
        OUT     360Q      ; PRIME SINGLE STEP INTERRUPT
        STA     CTLFLG    ; CLEAR SINGLE STEP INHIBIT
        JMP     EXIT      ; GO TO USER PROGRAM

; MAKE A MEMORY IMAGE TAPE

TSAVE   LXI     H,AVE     ; COMPLETE COMMAND
        CALL    PSTRNG
        INR     C         ; GET THREE PARAMETERS
        CALL    GETLN
        CALL    DATA
        CALL    CRLF
        LXI     H,ERROR
        SHLD    TPERRX    ; SET ERROR EXIT
        MVI     A,1
        OUT     371Q      ; TURN ON TAPE
        MVI     A,26Q     ; SYNC CHARACTER
        MVI     H,32      ; NUMBER OF CHARACTERS
TAPE0   CALL    WNB       ; WRITE CHARACTER
        DCR     H         ; DONE?
        JNZ     TAPE0     ; NO, CONTINUE
        MVI     A,2
        CALL    WNB       ; WRITE "STX" CHAR
        MOV     L,H       ; HL = 0
        SHLD    CRCSUM    ; SET CHECKSUM
        MVI     H,201Q    ; TYPE NUMBER
        MVI     L,1       ; IN HL
        CALL    WNP       ; WRITE IT
        POP     B         ; PC
        POP     H         ; END ADDRESS
        POP     D         ; START ADDRESS
        INX     H
        MOV     A,L       ; COMPUTE COUNT
        SUB     E
        MOV     L,A
        MOV     A,H
        SBB     D
        MOV     H,A
        CALL    WNP       ; WRITE IT
        PUSH    H         ; SAVE COUNT
        MOV     H,B
        MOV     L,C       ; HL = PC
        CALL    WNP       ; WRITE IT
        POP     H
        XCHG              ; HL = ADDR, DE = COUNT
        CALL    PDUMP
        JMP     START

; INPUT MODE HANDLERS

GETLN   PUSH    PSW
        PUSH    B
        PUSH    D
        PUSH    H         ; SAVE REGISTERS
        SCALL   DCLRCO    ; CLEAR CONSOLE
        XRA     A
        STA     INFLAG    ; SET MODE FLAG
        LXI     B,0FFH
        SCALL   DCONSL    ; SET CONSOLE FOR LINE MODE
        POP     H
        POP     D
        POP     B
        POP     PSW       ; RESTORE REGISTERS
        RET

FIXIN   PUSH    PSW
        PUSH    B         ; SAVE B, PSW
        XRA     A
        LXI     B,8181H
        SCALL   DCONSL    ; SET CONSOLE FOR CHAR MODE
        MVI     A,1
        STA     INFLAG    ; SET MODE FLAG
        POP     B
        POP     PSW
        RET

; ERROR EXIT

ERROR   XRA     A
        OUT     371Q      ; TURN OFF TAPE
        STA     SSFLAG    ; CLEAR SINGLE STEP MODE
        LXI     SP,STAK   ; COMPUTE STACK LOCATION
        SCALL   DCLRCO    ; CLEAR CONSOLE
        LXI     H,QUES
        CALL    PSTRNG    ; PRINT " ? "
        JMP     START

; IO ERROR PROCESSING ROUTINE

IOERR   PUSH    PSW       ; SAVE ERROR CODE
        LXI     H,IOERRM
        CALL    PSTRNG    ; PRINT I/O ERROR MESSAGE
        POP     PSW       ; GET CODE
        MVI     H,7       ; BELL
        SCALL   DERROR    ; PRINT ERROR MESSAGE
        LDA     FSTAT
        ORA     A         ; FILE LEFT OPEN?
        JZ      ERROR     ; IF NOT, EXIT
        SCALL   DCLOSE    ; IF SO, CLOSE IT
        MVI     A,0
        STA     FSTAT     ; FLAG FILE CLOSED
        JC      IOERR
        JMP     ERROR

; PRINT A SPACE

SPACE   MVI     A,' '

; CONSOLE OUTPUT ROUTINE

OUTCH   SCALL   DSCOUT
POUTCH  PUSH    PSW       ; SAVE CHARACTER
        LDA     PFLAG
        ORA     A         ; PRINTER ON?
        JZ      OUTCH0    ; IF NOT, SKIP
        POP     PSW       ; RESTORE CHAR
        PUSH    PSW       ; SAVE AGAIN
        CALL    LPRINT    ; PRINT IT
OUTCH0  POP     PSW       ; RESTORE CHAR
        RET

; CONSOLE INPUT ROUTINE

INCH    SCALL   DSCIN     ; GET A CHARACTER
        JC      INCH
        ANI     7FH
        PUSH    B         ; SAVE BC
        MOV     B,A       ; SAVE A
        LDA     ASFLAG
        ORA     A         ; IN ASCII CMD
        MOV     A,B       ; RESTORE CHARACTER
        POP     B
        JZ      INCH0     ; IF IN ASCII, ALLOW LC
        CPI     60H       ; LOWER CASE?
        JC      INCH0
        ANI     5FH       ; CAPITALIZE
INCH0   CPI     4         ; CONTROL-D?
        JZ      DDEXIT    ; IF SO, EXIT
        CPI     LF        ; NEW LINE CHAR?
        RNZ
        MVI     A,CR      ; CHANGE TO CR
        RET
DDEXIT  LXI     H,SURE
        CALL    PSTRNG    ; ASK "ARE YOU SURE?"
        CALL    ECHO
        CPI     'Y'
        JNZ     ERROR
        LDA     PFLAG
        ORA     A         ; IS PRINTER ON
        JZ      DEXIT0    ; IF NOT, EXIT
        CALL    RESET     ; EMPTY PRINTER BUFFER
        MVI     A,2
        SCALL   DCLOSE    ; CLOSE THE DRIVER
DEXIT0  MVI     A,LF
        CALL    OUTCH
        XRA     A
        SCALL   DEXIT     ; EXIT TO HDOS

; INPUT A CHARACTER, AND ECHO IT

ECHO    CALL    INCH
        PUSH    B         ; SAVE BC
        MOV     B,A       ; SAVE CHAR
        LDA     INFLAG
        ORA     A         ; IN LINE INPUT MODE?
        MOV     A,B       ; RESTORE CHAR
        POP     B         ; RESTORE BC
        JZ      POUTCH    ; RETURN IF IN LINE MODE
        JMP     OUTCH

; TYPE CR AND LF

CRLF    MVI     A,LF
        JMP     OUTCH

; EVALUATE DATA PARAMETERS
; THE C REGISTER CONTAINS THE NUMBER
; OF PARAMETERS REQUIRED.  PARAMETERS
; ARE RETURNED ON THE STACK
; HEX VERSION FIRST, THEN OCTAL

DATA    LDA     BFLAG
        ORA     A         ; HEX OR OCTAL?
        JNZ     ODATA     ; OCTAL
HDATA   LXI     H,0       ; INITIALLY ZERO
DATA0   CALL    ECHO      ; GET A CHARACTER
DATA1   MOV     B,A       ; SAVE IT
        CALL    BIN       ; CONVERT TO BINARY
        JC      DATA2     ; NOT A NUMBER
        DAD     H         ; MOVE LAST ENTRY
        DAD     H         ; OVER 4 PLACES
        DAD     H
        DAD     H
        ORA     L
        MOV     L,A       ; ADD LATEST ENTRY
        JMP     DATA0
DATA2   XTHL              ; SWAP RETURN ADDR, HL
        PUSH    H         ; REPLACE RETURN ADDR
        MOV     A,B
        CALL    CHECK0    ; TEST FOR DELIMITER
        JNC     DATA3     ; COMMA OR SPACE
        DCR     C         ; CR ENTERED
        JNZ     ERROR     ; NOT ENOUGH PARAMETERS
        RET               ; EXIT
DATA3   JNZ     ERROR     ; BAD DELIMITER
        DCR     C         ; DONE?
        JNZ     DATA      ; NO, CONTINUE
        RET               ; DONE
CDATA   PUSH    PSW       ; SAVE CHARACTER
        LDA     BFLAG     ; FOR CONDITIONAL TEST
        ORA     A         ; OCTAL?
        JNZ     OCDAT
        POP     PSW       ; RESTORE CHARACTER
        MVI     C,1       ; TEST ONE PARAMETER
        LXI     H,0       ; ZERO INITIALLY
        JMP     DATA1     ; PROCESS IT

; EVALUATE OCTAL DATA PARAMETERS
; THIS PROGRAM INPUTS OCTAL NUMBERS
; WITHOUT LEADING ZEROS

ODATA   LXI     H,0
ODAT0   CALL    ECHO      ; GET A CHARACTER
ODAT1   MOV     B,A       ; SAVE CHARACTER IN B
        CALL    BIN       ; CONVERT TO BINARY
        JC      ODAT2
        CPI     8
        JNC     ERROR     ; ILLEGAL CHARACTER
        DAD     H         ; MOVE PREVIOUS CHARACTERS UP
        DAD     H
        DAD     H
        PUSH    PSW       ; SAVE FLAG FROM DAD
        POP     D         ; FLAG IN E
        ADD     L         ; ADD IN LATEST CHARACTER
        MOV     L,A
        JMP     ODAT0
ODAT2   MOV     D,A       ; LAST CHAR IN D
        PUSH    D
        POP     PSW       ; GET DAD FLAG
        MOV     A,H       ; CONVERT TO SPLIT OCTAL
        RAR
        MOV     H,A
        XTHL              ; SWAP RETURN ADDR AND HL
        PUSH    H         ; REPLACE RET ADDR
        MOV     A,B       ; GET LAST CHARACTER
        CALL    CHECK0    ; WHAT WAS IT?
        JNC     ODAT5     ; COMMA OR SPACE
        DCR     C         ; CR ENTERED, DONE?
        JNZ     ERROR     ; NOT ENOUGH PARAMETERS
        RET
ODAT5   JNZ     ERROR     ; ILLEGAL CHARACTER
        DCR     C         ; DONE?
        JNZ     ODATA     ; CONTINUE
        RET
OCDAT   MVI     C,1       ; GET ONE PARAMETER
        LXI     H,0
        POP     PSW       ; RESTORE CHARACTER
        JMP     ODAT1

; COMPARE HL WITH DE
; IF HL <= DE THEN CARRY = 0
; IF HL > DE THEN CARRY = 1

CPHD    INX     H         ; INCREMENT HL
        MOV     A,H       ; IF HL ZERO NOW
        ORA     L         ; WAS FFFFH
        STC               ; SET CARRY
        RZ
        MOV     A,E       ; SUBTRACT
        SUB     L         ; HL FROM DE
        MOV     A,D       ; SUBTRACTION
        SBB     H         ; DETERMINES CARRY
        RET

; PRINT CONTENTS OF HL

PADR    MOV     A,H
        CALL    PBYTE     ; PRINT H
        LDA     BFLAG
        ORA     A         ; HEX OR OCTAL?
        JZ      PADR0     ; HEX
        MVI     A,'.'     ; OCTAL, PERIOD
        CALL    OUTCH     ; BETWEEN BYTES
PADR0   MOV     A,L       ; PRINT L
;       JMP     PBYTE

; PRINT BYTE IN A

PBYTE   PUSH    PSW       ; SAVE CHARACTER
        LDA     BFLAG
        ORA     A         ; HEX OR OCTAL
        JNZ     OBYT      ; OCTAL
        POP     PSW       ; RESTORE CHARACTER
HBYTE   PUSH    PSW       ; SAVE CHARACTER
        RRC               ; MOVE HIGH NIBBLE DOWN
        RRC
        RRC
        RRC
        ANI     0FH       ; ISOLATE IT
        CALL    PASC      ; PRINT IT
        POP     PSW       ; RESTORE CHARACTER
        ANI     0FH       ; GET LOW NIBBLE
;       JMP     PASC      ; PRINT IT

; CONVERT TO ASCII AND PRINT

PASC    ADI     90H       ; "A" OR GREATER SETS CARRY
        DAA
        ACI     40H       ; ADJUST UPPER NIBBLE
        DAA
        JMP     OUTCH

; PRINT BYTE IN A IN OCTAL

OBYT   POP      PSW       ; RESTORE CHARACTER
OBYTE   PUSH    PSW       ; SAVE CHARACTER
        RRC               ; MOVE THE TWO
        RRC               ; HIGH BITS DOWN
        RRC
        RRC
        RRC
        RRC
        ANI     3         ; ISOLATE THEM
        CALL    PASC      ; PRINT THEM
        POP     PSW       ; RESTORE CHARACTER
        PUSH    PSW       ; SAVE IT
        RRC               ; MOVE NEXT THREE
        RRC               ; BITS DOWN
        RRC
        ANI     7         ; ISOLATE THEM
        CALL    PASC      ; PRINT THEM
        POP     PSW       ; RESTORE CHARACTER
        ANI     7         ; GET LEAST BITS
        JMP   PASC        ; PRINT THEM

; DECODE ASCII CHARACTER TO BINARY
; RETURN CARRY IF CHARACTER IS NOT
; 0 THROUGH 9 OR A THROUGH F

BIN     SUI     '0'
        RC                ; LESS THAN "0" IS BAD
        ADI     '0'-'G'
        RC                ; GREATER THAN "F"
        ADI     6
        JP      BIN0      ; PROCESS "A" - "F"
        ADI     7
        RC                ; ":" THROUGH "@"
BIN0    ADI     10        ; ADJUST
        ORA     A         ; CLEAR CARRY
        RET

; TEST DELIMITER CHARACTERS
; COMMA AND SPACE RETURN Z
; CR RETURNS C
; OTHERS RETURN NOT C

CHECK   CALL    ECHO      ; GET A CHARACTER
CHECK0  CPI     ' '       ; SPACE
        RZ
        CPI     ','       ; COMMA
        RZ
        CPI     '-'       ; MINUS SIGN
        RZ
        CPI     CR        ; CARRIAGE RETURN
        STC
        CMC             ; SET NOT CARRY
        RNZ             ; NOT A CR
        STC             ; SET CARRY
        RET

; ENTRY POINT FOR CONTROL-A PROCESSING

RESTRT  POP     PSW       ; REMOVE HDOS ADDRESS
        POP     PSW       ; REMOVE PSW
        PUSH    H         ; SAVE MACHINE STATE
        PUSH    D
        PUSH    B
        PUSH    PSW
        XRA     A
        STA     SSFLAG    ; CLEAR SINGLE STEPS
        PUSH    PSW       ; EXTRA PUSH MATCHES PAM-8

; RESTART 2, PROGRAMMED BREAKPOINT

RSTB    POP     PSW       ; REMOVE EXTRA PAM-8 PUSH
        LDA     MMFLAG    ; IN MONITOR MODE?
        ORA     A
        JZ      RSTB0     ; IF NOT, PROCESS INT.
        EI                ; ENABLE INTERRUPTS
        JMP     ERROR     ; IN MONITOR MODE
RSTB0   MVI     A,1       ; SET FLAG TO INDICATE
        STA     MMFLAG    ; IN MONITOR MODE
        EI                ; ENABLE CPU INTERRUPTS
        LXI     D,EXIT    ; COMPUTE STACK LOCATION
        LXI     H,10      ; MOVE TO TOP
        DAD     SP        ; OF PUSHED REGISTERS
        MVI     B,4       ; COUNT FOR TRANSFER
        XCHG
RST0    DCX     H         ; MOVE REGISTER VALUES
        MOV     M,D
        DCX     H
        MOV     M,E
        POP     D         ; GET ANOTHER VALUE
        DCR     B         ; END OF TRANSFER?
        JNZ     RST0      ; NO, CONTINUE
        POP     B         ; OLD PC
        DCX     B         ; POINT AT TRAP
        SPHL              ; SET RDT STACK
        LXI     H,TRAP    ; GET TRAP STORAGE
        DAD     SP        ; LOCATION
        MOV     A,M       ; PROGRAMMED RST
        SUB     C         ; OR FORCED RETURN?
        INX     H
        JNZ     RST1      ; NO TRAP HERE
        MOV     A,M
        SUB     B
        JZ      RST3      ; FIRST TRAP WAS HIT
RST1    INX     H
        INX     H
        MOV     A,M
        SUB     C
        JNZ     RST2      ; NO TRAP HERE, EITHER
        INX     H
        MOV     A,M
        SUB     B
        JZ      RST3      ; SECOND TRAP HIT
RST2    INX     B         ; CORRECT PC VALUE
RST3    LXI     H,LREG    ; SAVE USER REGISTER
        DAD     SP        ; VALUES, STARTING WITH L
        MOV     M,E
        INX     H
        MOV     M,D       ; HL SAVED
        INX     H         ; POINT TO PC
        INX     H
        INX     H
        MOV     M,C
        INX     H
        MOV     M,B       ; PC SAVED
        LXI     H,TRAP    ; PREPARE TO CLEAR TRAPS
        DAD     SP
        MVI     D,2       ; 2 TRAPS
RST4    MOV     C,M       ; GET LSB OF ADDRESS
        MVI     M,0       ; CLEAR STORAGE AREA
        INX     H
        MOV     B,M       ; GET MSB OF ADDRESS
        MVI     M,0       ; CLEAR AREA
        INX     H         ; SAVED OP CODE HERE
        MOV     A,C
        ORA     B         ; TEST IF TRAP SET
        JZ      RST5      ; NOT SET, CHECK NEXT
        MOV     A,M       ; GET OPCODE
        STAX    B         ; REPLACE IT
RST5    INX     H         ; NEXT TRAP ADDRESS
        DCR     D         ; DONE?
        JNZ     RST4      ; NO, CONTINUE
        LXI     H,TABLE   ; GET TABLE OF REGISTERS
        CALL    REG6      ; PRINT REGISTERS
        LDA     SSFLAG
        ORA     A         ; IN SINGLE STEP MODE?
        JZ      START     ; IF NOT, EXIT
        JMP     SSTEP1    ; GO TO NEXT STEP

; PRINT STRINGS

PSTRNG  MOV     A,M       ; GET CHARACTER
        PUSH    PSW       ; SAVE IT
        ANI     7FH       ; STRIP 8TH BIT
        CALL    OUTCH     ; PRINT IT
        POP     PSW       ; RESTORE ORIGINAL
        ORA     A         ; CHECK IF END
        RM                ; RETURN IF SO
        INX     H         ; GET NEXT CHARACTER
        JMP     PSTRNG

; TABLE OF REGISTERS
; CONTAINS IDENTIFIER, STACK DISPLACEMENT,
; AND BYTE COUNT

TABLE   DB      'A',AREG,1
        DB      'F',FLAG,1
        DB      'B',BREG,2
        DB      'D',DREG,2
        DB      'H',HREG,2
        DB      'P',PCTR,2
        DB      'S',SPTR,2
        DB      0FFH,0FFH ; END OF TABLE

; DISPLACEMENT VALUES

AREG    EQU     5
FLAG    EQU     4
BREG    EQU     3
DREG    EQU     1
HREG    EQU     0FH
LREG    EQU     0EH
PCTR    EQU     13H
SPTR    EQU     7
TRAP    EQU     14H

; RAM CELLS USED BY RDT

MMFLAG  DS      1         ; MONITOR MODE FLAG
ASFLAG  DS      1         ; ASCII COMMAND FLAG
BFLAG   DS      1         ; NUMBER BASE FLAG
CONFLG  DB      0         ; CONSOLE MODE FLAG
INFLAG  DS      1         ; INPUT MODE FLAG
FSTAT   DB      0         ; FILE STATUS FLAG
STYPE   DS      1         ; SEARCH TYPE FLAG
SSFLAG  DS      1         ; SINGLE STEP FLAG
TOP     DS      2         ; TOP OF USER MEMORY UNDER RDT
FBGN    DS      2         ; FILE BEGINNING ADDRESS
FEND    DS      2         ; FILE ENDING ADDRESS
DEFALT  DB      'SY0ABS'  ; DEFAULT DEVICE AND EXT.
PBPTR   DS      2         ; PRINTER BUFFER POINTER
PNCTR   DS      1         ; PRINTER COUNTER
PFLAG   DB      0         ; PRINTER FLAG
RFLAG   DB      1         ; RESET PRINT BUFFER FLAG
PDFLT   DB      'SY0',0,0,0
PNAME   DB      'LP:',0
        DB      0FFH      ; BEGINNING OF NAME BUFFER
FNAME   DS      20        ; FILE NAME BUFFER
PBUFF   DS      256       ; PRINTER BUFFER
BUFEND  EQU     $
        IF      INCLUDE
        INCLUDE dist.acm
BUFF    DS      55
        ENDIF

; COMMAND COMPLETION STRINGS

DD      DB      'D/SUBTRACT',240Q    ; ADD/SUBTRACT
CII     DB      'CII',240Q           ; ASCII
ASE     DB      'ASE',240Q           ; BASE
NGE     DB      'ANGE',240Q          ; CHANGE
MPAR    DB      'MPARE',240Q         ; COMPARE
ISP     DB      'ISPLAY',240Q        ; DISPLAY
        IF      INCLUDE
XAMIN   DB      'XAMINE',240Q        ; EXAMINE
        ENDIF
ILL     DB      'ILL',240Q            ; FILL
OTO     DB      'O TO',240Q           ; GO TO
NPUT    DB      'NPUT MODE',240Q      ; INPUT MODE
CHAR    DB      'HARACTE','R'+200Q
LINEM   DB      'IN','E'+200Q
EX      DB      'EX',240Q             ; HEX
OAD     DB      'OAD',240Q            ; LOAD
OVE     DB      'OVE',240Q            ; MOVE
CTAL    DB      'CTAL',240Q           ; OCTAL
RT      DB      'RT',240Q             ; PORT
IN      DB      'N',240Q              ; IN
UT      DB      'UT',240Q             ; OUT
RINTER  DB      'INTER? (Y OR N)',240Q
EGIS    DB      'EGISTER',240Q        ; REGISTER
ARCH    DB      'ARCH',240Q           ; SEARCH
YTE     DB      'YTE',240Q            ; BYTE
ORD     DB      'ORD',240Q            ; WORD
AVE     DB      'AVE',240Q            ; TAPE
NGLE    DB      'NGLE STEP',240Q      ; SINGLE STEP
TYPE    DB      'ERROR -- FILE IS NOT MACHINE COD','E'+200Q
TBIG    DB      'ERROR -- FILE TOO BI','G'+200Q
STARTS  DB      LF,'STARTS ',240Q
ENDS    DB      LF,'ENDS   ',240Q
SURE    DB      '^D',LF,'ARE YOU SURE?',240Q
QUES    DB      ' ?',240Q
IOERRM  DB      'I/O ERROR:',240Q

; MANIPULATED EXIT CODE
; USED FOR JUMP TO USER PROGRAM

        DS      98H       ; SPACE FOR STACK
STAK    DS      8         ; RDT STACK HERE
;                         ; REGISTERS HERE
EXIT    POP     D         ; RESTORE D,E
        POP     B         ; RESTORE B,C
        POP     PSW       ; RESTORE A AND FLAGS
        POP     H
        SPHL              ; RESTORE USER STACK
        LXI     H,0       ; RESTORE H,L
        EI                ; ENABLE INTERRUPTS
        JMP     0         ; TAKE THE JUMP
        DW      0         ; FIRST TRAP ADDRESS
        DB      0         ; FIRST TRAP VALUE
        DW      0         ; SECOND TRAP ADDRESS
ENDEX   DB      0         ; SECOND TRAP VALUE

; TRANSPORT RDT TO TOP OF MEMORY

XPORT   LXI     H,STARTS  ; PRINT "STARTS"
        CALL    PSTRNG
        LHLD    MVADR     ; GET ADDRESS TO MOVE TO
        CALL    PADR      ; PRINT IT
        LXI     D,SETUP   ; GET ORG ADDRESS
        MOV     A,L       ; COMPUTE BIAS
        SUB     E
        MOV     L,A
        MOV     A,H
        SBB     D
        MOV     H,A       ; HL = BIAS
        SHLD    BIAS      ; STORE IT
        LXI     H,ENDS    ; PRINT "ENDS"
        CALL    PSTRNG
        LHLD    BIAS
        LXI     D,ENDEX+3 ; COMPUTE END ADDRESS
        DAD     D
        CALL    PADR      ; PRINT IT
        CALL    CRLF
        LXI     H,SETUP   ; HL = ORG ADDR
        LXI     D,TABLE-1 ; END OF CODE TO FIX
XPORT0  MOV     A,M       ; GET OPCODE
        MOV     B,A       ; SAVE IT
        PUSH    H         ; SAVE ADDRESS

; LOCATE TWO AND THREE BYTE OPCODES

        LXI     H,THBYT   ; THREE BYTE TABLE
XPORT1  MOV     A,M       ; GET TABLE OPCODE
        CMP     B         ; MATCHES PROGRAM OPCODE?
        JZ      XPORT2    ; YES, PROCESS IT
        CPI     0F7H      ; END OF TABLE?
        INX     H
        JNZ     XPORT1    ; IF NOT, CONTINUE
        LXI     H,TWBYT   ; TWO BYTE TABLE
XPORTA  MOV     A,M       ; GET TABLE OPCODE
        CMP     B         ; MATCHES PROGRAM OPCODE?
        JZ      XPORTB    ; YES, SKIP DATA BYTE
        CPI     0F7H      ; END OF TABLE?
        INX     H         ; MOVE POINTER
        JNZ     XPORTA    ; IF NOT, CONTINUE
        JMP     XPORTC    ; ONE BYTE INSTRUCTION
XPORTB  POP     H         ; RESTORE ADDRESS POINTER
        INX     H         ; SKIP DATA BYTE
        PUSH    H         ; SAVE ADDRESS
XPORTC  POP     H         ; RESTORE ADDRESS
        CALL    CPHD      ; END OF RDT?
        JNC     XPORT0    ; IF NOT, CONTINUE
        JMP     XPORT5    ; CODE FIXED, MOVE IT

; FIX THREE BYTE INSTRUCTIONS

XPORT2  POP     H         ; RESTORE ADDRESS
        INX     H         ; POINT TO CODE ARGUMENT
        PUSH    D         ; SAVE END POINTER
        MOV     E,M
        INX     H
        MOV     D,M       ;  DE = ADDR TO CHECK
        PUSH    H         ;  SAVE ADDRESS POINTER
        LXI     H,SETUP-1 ; CHECK IF ADDRESS IS
        CALL    CPHD      ; BELOW RDT
        JC      XPORT3    ; IF SO, DO NOT ADJUST
        POP     H         ; RESTORE ADDRESS
        POP     D         ; RESTORE END POINTER
        DCX     H         ; ADDR LOW BYTE
        LDA     BIAS      ; FIX CODE
        ADD     M
        MOV     M,A
        INX     H
        LDA     BIAS+1
        ADC     M
        MOV     M,A       ; ADDR = ADDR + BIAS
        JMP     XPORT4
XPORT3  POP     H         ; RESTORE ADDRESS
        POP     D         ; RESTORE END POINTER
XPORT4  CALL    CPHD      ; END OF FIXABLE CODE?
        JNC     XPORT0    ; IF NOT, CONTINUE

; MOVE TO NEW LOCATION

XPORT5  LHLD    MVADR     ; GET DESTINATION
        MOV     B,H       ; PUT IN BC
        MOV     C,L
        LXI     H,SETUP   ; HL = START OF RDT
        LXI     D,ENDEX   ; DE = END OF RDT
XPORT6  MOV     A,M       ; GET BYTE TO MOVE
        STAX    B         ; STORE AT NEW ADDR
        INX     B         ; INCREMENT DEST POINTER
        CALL    CPHD      ; DONE?
        JNC     XPORT6    ; NO, KEEP MOVING CODE
        LHLD    BIAS
        LXI     D,TOP     ; COMPUTE THE ADDRESS OF
        DAD     D         ; THE LABEL "TOP"
        XCHG              ; PUT IT IN DE
        LHLD    MVADR     ; GET START ADDR OF RDT
        DCX     H         ; USER MEMORY ENDS HERE
        MOV     A,L
        STAX    D         ; STORE LOW BYTE
        MOV     A,H
        INX     D
        STAX    D         ; STORE HIGH BYTE

; JUMP TO NEW RDT

        LHLD    MVADR     ; GET START ADDRESS
        PCHL              ; TAKE THE JUMP

; TABLE OF THREE BYTE INSTRUCTIONS
; THAT REQUIRE FIXING

THBYT   DB      11H       ; LXI   D
        DB      21H       ; LXI   H
        DB      31H       ; LXI   SP
        DB      22H       ; SHLD
        DB      2AH       ; LHLD
        DB      32H       ; STA
        DB      3AH       ; LDA
        DB      0CDH      ; CALL
        DB      0CCH      ; CZ
        DB      0C3H      ; JMP
        DB      0C2H      ; JNZ
        DB      0CAH      ; JZ
        DB      0D2H      ; JNC
        DB      0DAH      ; JC
        DB      0F2H      ; JP
        DB      0FAH      ; JM
        DB      0F7H      ; END OF TABLE

; TWO BYTE OPCODES

TWBYT   DB      3EH       ; MVI   A
        DB      6         ; MVI   B
        DB      0EH       ; MVI   C
        DB      16H       ; MVI   D
        DB      26H       ; MVI   H
        DB      2EH       ; MVI   L
        DB      36H       ; MVI   M
        DB      0C6H      ; ADI
        DB      0CEH      ; ACI
        DB      0D6H      ; SUI
        DB      0E6H      ; ANI
        DB      0EEH      ; XRI
        DB      0FEH      ; CPI
        DB      0D3H      ; OUT
        DB      0DBH      ; IN
        DB      0FFH      ; SCALL
        DB      0F7H      ; END OF TABLE

; PROGRAM TITLE

TITLE   DB      LF,'SELF-RELOCATING DEBUGGING TOOL (RDT) Version 1.0',212Q
DEV     DB      LF,'Enter Printer Device Name: <none>',240Q

; RELOCATION RAM CELL

BIAS    DS      2         ; RELOCATION BIAS

        END     PRESET
