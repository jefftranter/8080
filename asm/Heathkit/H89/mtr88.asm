;       IO PORTS

;       ALL REFERENCES TO THE H8 FRONT PANEL PORTS ARE TRAPPED BY THE
;       Z80 NMI OF THE H88/H89.  OP.CTL WILL STILL PERFORM AS IN AN H8
;       IN RESPECT TO THE CLOCK AND SINGLE STEP CONTROL.  FOR MORE
;       INFORMATION SEE THE NMI ROUTINE.
;
IP.PAD  EQU     360Q            ; PAD INPUT PORT
OP.CTL  EQU     360Q            ; CONTROL OUTPUT PORT
OP.DIG  EQU     360Q            ; DIGIT SELECT OUTPUT PORT
OP.SEQ  EQU     361Q            ; SEGMENT SELECT OUTPUT PORT

;       H88/H89 CONTROL PORT
H88.CTL EQU     362Q            ; H88/H89 PORT FOR CLOCK AND SINGLE STEP
H88B.CK EQU     00000010B       ; 2MS CLOCK ENABLE/DISABLE
H88B.SS EQU     00000001B       ; SINGLE STEP ENABLE/DISABLE

H88.SW  EQU     362Q            ; 8 POSITION DIP SWITCH
H88S.AT EQU     10000000B       ; AUTO BOOT SWITCH
H88S.BR EQU     01000000B       ; BAUD RATE SWITCHE        **/RNC/**
H88S.M  EQU     00100000B       ; MEMORY TEST/NORMAL OPERATION SWITCH
H88S.DV EQU     00010000B       ; = 0, BOOT FROM DEVICE AT 174-177Q
                                ; = 1, BOOT FROM DEVICE AT 170-173Q
H88S.0  EQU     00001100B       ; = 00, NO DEVICE INSTALLED AT 170-173Q
                                ; = 01, DEVICE AT 170-173Q = Z47
H88S.4  EQU     00000011B       ; = 00, DEVICE AT 174-177Q = H17
                                ; = 01, DEVICE AT 174-177Q = Z47

;       CASSETTE PORTS

IP.TPC  EQU     371Q            ; TAPE CONTROL IN
OP.TPC  EQU     371Q            ; TAPE CONTROL OUT
IP.TPD  EQU     370Q            ; TAPE DATA IN
OP.TPD  EQU     370Q            ; TAPE DATA OUT

;       ASCII CHARACTERS

A.SYN   EQU     026Q            ; SYNC CHARACTER
A.STX   EQU     002Q            ; STX CHARACTER
A.BEL   EQU     007Q            ; BELL CHARACTER
A.BKS   EQU     010Q            ; BACKSPACE CHARACTER
A.LF    EQU     012Q            ; LINE FEED CHARACTER
A.CR    EQU     015Q            ; CARRIAGE RETURN CHARACTER
A.ESC   EQU     033Q            ; ESCAPE CHARACTER
A.DEL   EQU     177Q            ; DELETE OR RUBOUT CHARACTER

;       FRONT PANEL HARDWARE CONTROL BITS.

CB.SSI  EQU     00010000B       ; SINGLE STEP INTERRUPT
CB.MTL  EQU     00100000B       ; MONITOR LIGHT
CB.CLI  EQU     01000000B       ; CLOCK INTERRUPT ENABLE
CB.SPK  EQU     10000000B       ; SPEAKER ENABLE

;       DISPLAY MODE FLAGS (IN *DSPMOD*)

DM.MR   EQU     0               ; MEMORY READ
DM.MW   EQU     1               ; MEMORY WRITE
DM.RR   EQU     2               ; REGISTER READ
DM.RW   EQU     3               ; REGISTER WRITE

        INCLUDE tape.asm

;       MACHINE INSTRUCTIONS

MI.HLT  EQU     01110110B       ; HALT
MI.RET  EQU     11001001B       ; RETURN
MI.IN   EQU     11011011B       ; INPUT
MI.OUT  EQU     11010011B       ; OUTPUT
MI.LDA  EQU     00111010B       ; LDA
MI.ANI  EQU     11100110B       ; ANI
MI.LXID EQU     00010001B       ; LXI D
MI.JMP  EQU     11000011B       ; JMP
MI.LDXA EQU     11011101B       ; LD IX,  (BYTE A)
MI.LDXB EQU     00100001B       ; LD IX,  (BYTE B)
MI.LDYA EQU     11111101B       ; LD IY,  (BYTE A)
MI.LDYB EQU     00100001B       ; LD IY,  (BYTE B)
MI.EXAF EQU     00001000B       ; EX AF,AF'
MI.JIXA EQU     11011101B       ; JP (IX)  (BYTE A)
MI.JIXB EQU     11101001B       ; JP (IX)  (BYTE B)
MI.JIYA EQU     11111101B       ; JP (IY)  (BYTE A)
MI.JIYB EQU     11101001B       ; JP (IY)  (BYTE B)

;       USER OPTION BITS
;
;       THESE BITS ARE IN SELL MFLAG.
;

UO.HLT  EQU     10000000B       ; DISABLE HALT PROCESSING
UO.NFR  EQU     CB.CLI          ; NO REFRESH OF FRONT PANEL
UO.DDU  EQU     00000010B       ; DISABLE DISPLAY UPDATE
UO.CLK  EQU     00000001B       ; ALLOW PRIVATE INTERRUPT PROCESSING
