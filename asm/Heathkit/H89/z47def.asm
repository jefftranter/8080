;
;       DISK INTERFACE CONSTANTS
;
D.STA   EQU     170Q            ; INTERFACE STATUS PORT
D.DAT   EQU     D.STA+1         ; INTERFACE DATA PORT
;
S.ERR   EQU     00000001B       ; ERROR BIT
S.DON   EQU     00100000B       ; DONE
S.DTR   EQU     10000000B       ; DATA TRANSFER REQUEST
;
W.RES   EQU     00000010B       ; RESET COMMAND

;       CONTROLLER STATUS REGISTER
;
CS.UNR  EQU     10000000B       ; UNIT NOT READY
CS.WPD  EQU     01000000B       ; WRITE PROTECTED DRIVE

;       AUXILIARY STATUS REGISTER
;
AS.0DD  EQU     01000000B       ; TRACK 0 DOUBLE DENSITY
AS.1DD  EQU     00100000B       ; TRACK 1 - 76 DOUBLE DENSITY
AS.S1A  EQU     00010000B       ; SIDE 1 AVAILABLE
AS.SWL  EQU     00000011B       ; SECTOR LENGTH MASK

;       DISK COMMANDS
;
DC.BOOT EQU     0               ; BOOT
DC.RST  EQU     1               ; READ CONTROLLER STATUS
DC.RAS  EQU     2               ; READ AUX. STATUS
DC.LSC  EQU     3               ; LOAD SECTOR COUNT
DC.RAD  EQU     4               ; READ ADDR. OF LAST SECTOR ADDRESSED
DC.REA  EQU     5               ; READ SECTORS
DC.WRI  EQU     6               ; WRITE SECTORS
DC.REAB EQU     7               ; READ SECTORS BUFFERED
DC.WRIB EQU     8               ; WRITE SECTORS BUFFERED
DC.WRD  EQU     9               ; WRITE SECTORS & DELETE
DC.WRDB EQU     10              ; WRITE SECTORS BUFFERED & DELETE
DC.CPY  EQU     11              ; COPY
DC.FRM0 EQU     12              ; FORMAT IBM SD
DC.FRM1 EQU     13              ; FORMAT     SD
DC.FRM2 EQU     14              ; FORMAT IBM DD
DC.FRM3 EQU     15              ; FORMAT     DD

;       USEFUL FLAGS
;
UNT.0   EQU     00000000B       ; UNIT 0
UNT.1   EQU     00100000B       ; UNIT 1
UNT.2   EQU     01000000B       ; UNIT 2
UNT.3   EQU     01100000B       ; UNIT 3
;
C.256   EQU     256             ; SECTOR SIZE = 256 BYTES
C.128   EQU     128             ; SECTOR SIZE
