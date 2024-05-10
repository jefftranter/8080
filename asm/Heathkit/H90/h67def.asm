;;      H67 Disk Controller Definitions
;

;;      Register addresses
;

BASE    EQU     170Q            ; Controller base address

RI.DAT  EQU     0               ; Data In/Out (Read/Write)
RI.CON  EQU     1               ; Control (write Only)
RI.BST  EQU     1               ; Bus Status (Read Only)

;       Control Register Definition

BC.SEL  EQU     01000000B       ; Select and data bit 0
BC.IE   EQU     00100000B       ; Interrupt Enable
BC.RTS  EQU     00010000B       ; Reset
BC.EDT  EQU     00000010B       ; Enable Data

;       Bus Status Register Definition

BS.REQ  EQU     10000000B       ; Data Transfer Request
BS.DTD  EQU     01000000B       ; Data Transfer Direction
BS.IN   EQU     00000000B       ;  Data to Host
BS.OUT  EQU     01000000B       ;  Data to Controller
BS.LMB  EQU     00100000B       ; Last byte in data/command string
BS.MTY  EQU     00010000B       ; Message type
BS.DAT  EQU     00000000B       ;  Data
BS.COM  EQU     00010000B       ;  Command
BS.BSY  EQU     00001000B       ; Busy
BS.INT  EQU     00000100B       ; Interrupt Pending
BS.PE   EQU     00000010B       ; Parity Error
BS.HID  EQU     00000001B       ; Hardware Identification

;       Status Byte Definitions

ST.LUN  EQU     01100000B       ; Logical Unit
ST.SPR  EQU     00011100B       ; Spare
ST.ERR  EQU     00000010B       ; Error
ST.PER  EQU     00000001B       ; Parity Error

;;      Commands
;

CLASSM  EQU     11100000B       ; Class Mask

CLASS0  EQU     0000000B        ; Class 0
CLASS1  EQU     0010000B        ; Class 1
CLASS6  EQU     1100000B        ; Class 6

OPCODM  EQU     00011111B       ; Op-code Mask
LUNM    EQU     01100000B       ; Logical Unit Mask
LSA.2   EQU     00011111B       ; Logical Sector Address (2)

;       Class 0 Commands

D.TDR   EQU     CLASS0+0        ; Test drive ready
D.REC   EQU     CLASS0+1        ; Recalibrate drive
D.RSY   EQU     CLASS0+2        ; Request Syndrome
D.RSE   EQU     CLASS0+3        ; Request Sense
D.FOR   EQU     CLASS0+4        ; Format Drive
D.CTF   EQU     CLASS0+5        ; Check track format
D.FT    EQU     CLASS0+6        ; Format Track
D.FBS   EQU     CLASS0+7        ; Format bad sector
D.REA   EQU     CLASS0+8        ; Read
D.WPS   EQU     CLASS0+9        ; Write protect the sector
D.WRI   EQU     CLASS0+10       ; Write
D.SEK   EQU     CLASS0+11       ; Seek

;       Class 1 Commands

D.CPB   EQU     CLASS1+0        ; Copy block

;       Class 6 Commands

D.FFD   EQU     CLASS6+0        ; Format floppy disk

;       Type 0 error codes (Drive error Codes)

TO.NST  EQU     0               ; No status
TO.NIS  EQU     1               ; No Index signal
TO.NSC  EQU     2               ; No seek complete
TO.WFT  EQU     3               ; Write fault
TO.DNR  EQU     4               ; Drive not ready
TO.DNS  EQU     5               ; Drive not selected
TO.NT0  EQU     6               ; No track zero
TO.MDS  EQU     7               ; Multi-drive selected

;       Type 1 error codes (data error codes)

TI.ID   EQU     0               ; ID Read Error
TI.UDE  EQU     1               ; Uncorrectable data error
TI.IDNF EQU     2               ; ID Address Mark not found
TI.DMNF EQU     3               ; Data Address Mark Not Found
TI.RNF  EQU     4               ; Record Not Found
TI.SKE  EQU     5               ; Seek Error
TI.DTE  EQU     6               ; DMA Time-out Error  (not used)
TI.WP   EQU     7               ; Write protected
TI.CDE  EQU     8               ; Correctable Data field error
TI.BBF  EQU     9               ; Bad Block Found
TI.FE   EQU     10              ; Format Error

;       Type 2 Error Codes (Command error codes)

T2.ILC  EQU     0               ; Illegal Command
T2.IDA  EQU     1               ; Illegal Disk address
T2.IFN  EQU     2               ; Illegal Function
