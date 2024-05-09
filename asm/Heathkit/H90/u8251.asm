;       8251 USART BIT DEFINITIONS.
;

;       PORT ADDRESSES

UDR     EQU     0               ; DATA REGISTER IS EVEN
UBR     EQU     1               ; STATUS REGISTER IS NEXT

SC.UART EQU     372Q            ; CONSOLE USART ADDRESS (IFF 8251)


;       MODE INSTRUCTION CONTROL BITS.

UMI.1B  EQU     01000000B       ; 1 STOP BIT
UMI.HB  EQU     10000000B       ; 1 1/2 STOP BITS
UMI.2B  EQU     11000000B       ; 2 STOP BITS
UMI.PE  EQU     00100000B       ; EVEN PARITY
UMI.PA  EQU     00010000B       ; USE PARITY
UMI.L5  EQU     00000000B       ; 5 BIT CHARACTERS
UMI.L6  EQU     00000100B       ; 6 BIT CHARACTERS
UMI.L7  EQU     00001000B       ; 7 BIT CHARACTERS
UMI.L8  EQU     00001100B       ; 8 BIT CHARACTERS
UMI.1X  EQU     00000001B       ; CLOCK X 1
UMI.16X EQU     00000010B       ; CLOCK X 16
UMI.64X EQU     00000011B       ; CLOCK X 64

;       COMMAND INSTRUCTION BITS.

UCI.IR  EQU     01000000B       ; INTERNAL RESET
UCI.RO  EQU     00100000B       ; READER-ON CONTROL FLAG
UCI.ER  EQU     00010000B       ; ERROR RESET
UCI.RE  EQU     00000100B       ; RECEIVE ENABLE
UCI.IE  EQU     00000010B       ; ENABLE INTERRUPTS FLAG
UCI.TE  EQU     00000001B       ; TRANSMIT ENABLE

;       STATUS READ BITS.

USR.FE  EQU     00100000B       ; FRAMING ERROR
USR.OE  EQU     00010000B       ; OVERRUN ERROR
USR.PE  EQU     00001000B       ; PARITY ERROR
USR.TXE EQU     00000100B       ; TRANSMITTER EMPTY
USR.RXR EQU     00000010B       ; RECEIVER READY
USR.TXR EQU     00000001B       ; TRANSMITTER READY
