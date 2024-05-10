;;      H47DEF - H47 Constant Definitions
;
;       Z80 Instructions

M.INI   EQU     10100010B*256+11101101B ; INI Instruction
M.OUTI  EQU     10100011B*256+11101101B ; OUTI Instruction


;
;       DISK INTERFACE CONSTANTS
;
D.STA   EQU     170Q            ; INTERFACE STATUS PORT
D.DAT   EQU     D.STA+1         ; INTERFACE DATA PORT
;
S.ERR   EQU     00000001B       ; ERROR BIT
S.DON   EQU     00100000B       ; DONE
S.IEN   EQU     01000000B       ; INTERRUPT ENABLE
S.DTR   EQU     10000000B       ; DATA TRANSFER REQUEST
;
S.SW0   EQU     00000010B       ; DIP SWITCH 0
S.SW1   EQU     00000100B       ; DIP SWITCH 1
S.SW2   EQU     00001000B       ; DIP SWITCH 2
S.SW3   EQU     00010000B       ; DIP SWITCH 3
;
W.RES   EQU     00000010B       ; RESET COMMAND

;;      STATUS BYTE FLAGS
;

SB.UNR  EQU     10000000B       ; UNIT NOT READY
SB.WPD  EQU     01000000B       ; WRITE PROTECTED DRIVE
SB.DLD  EQU     00100000B       ; DELETED DATA
SB.NRF  EQU     00010000B       ; NO RECORD FOUND
SB.CRC  EQU     00001000B       ; CRC ERROR
SB.LTD  EQU     00000100B       ; LATE DATA
SB.ILC  EQU     00000010B       ; ILLEGAL COMMAND
SB.BTO  EQU     00000001B       ; BAD TRACK OVERFLOW

;       AUXILIARY STATUS BYTE FLAGS
;
AS.0DD  EQU     01000000B       ; TRACK 0 DOUBLE DENSITY
AS.1DD  EQU     00100000B       ; TRACK 1-76 DOUBLE DENSITY
AS.S1A  EQU     00010000B       ; SIDE 1 AVAILABLE
AS.SLM  EQU     00000011B       ; SECTOR LENGTH MASK

;       DISK COMMANDS
;
        ORG     0
DD.BOOT DS      1               ; BOOT
DD.RST  DS      1               ; READ CONTROLLER STATUS
DD.RAS  DS      1               ; READ AUX. STATUS
DD.LSC  DS      1               ; LOAD SECTOR COUNT
DD.RAD  DS      1               ; READ ADDR. OF LAST SECTOR ADDRESSED
DD.REA  DS      1               ; READ SECTORS
DD.WRI  DS      1               ; WRITE SECTORS
DD.REAB DS      1               ; READ SECTORS BUFFERED
DD.WRIB DS      1               ; WRITE SECTORS BUFFERED
DD.WRD  DS      1               ; WRITE SECTORS & DELETE
DD.WRDB DS      1               ; WRITE SECTORS BUFFERED & DELETE
DD.CPY  DS      1               ; COPY
DD.FRM0 DS      1               ; FORMAT IBM SD
DD.FRM1 DS      1               ; FORMAT     SD
DD.FRM2 DS      1               ; FORMAT IBM DD
DD.FRM3 DS      1               ; FORMAT     DD
DD.RRDY DS      1               ; READ READY

;;      Special De-Bug functions
;

        ORG     010H
DD.SPF0 DS      1               ; SPECIAL FUNCTION 0
DD.SPF1 DS      1               ; SPECIAL FUNCTION 1
DD.SPF2 DS      1               ; SPECIAL FUNCTION 2
DD.SPF3 DS      1               ; SPECIAL FUNCTION 3
DD.SPF4 DS      1               ; SPECIAL FUNCTION 4
DD.SPF5 DS      1               ; SPECIAL FUNCTION 5

;;      Special Heath Functions
;

        ORG     080H
DD.SDC  DS      1               ; SET DRIVE CHARACTERISTIC
DD.ST   DS      1               ; SEEK TO TRACK
DD.DS   DS      1               ; DISK STATUS
DD.RDL  DS      1               ; READ LOGICAL
DD.WRL  DS      1               ; WRITE LOGICAL
DD.RDBL DS      1               ; READ BUFFERED LOGICAL
DD.WTBL DS      1               ; WRITE BUFFERED LOGICAL
DD.WTDL DS      1               ; WRITE DELETED DATA LOGICAL
DD.WDLB DS      1               ; WRITE BUFFERED DELETED DATA LOGICAL

;;      USEFUL FLAGS
;

UNT.0   EQU     00000000B       ; UNIT 0
UNT.1   EQU     00100000B       ; UNIT 1
UNT.2   EQU     01000000B       ; UNIT 2
UNT.3   EQU     01100000B       ; UNIT 3

UNT.M   EQU     01100000B       ; Unit mask



SID.0   EQU     00000000B       ; Side: 0
SID.1   EQU     10000000B       ; Side: 1

SID.M   EQU     10000000B       ; Side Mask


SEC.M   EQU     00011111B       ; Track Mask



SSIZ.M  EQU     1024            ; Maximum Sector Size


C.256   EQU     256             ; SECTOR SIZE = 256 BYTES
C.128   EQU     128             ; SECTOR SIZE
C.26    EQU     26
