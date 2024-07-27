AAAAAAAABBBBBBBBCCCCCCCCDDDDDDDD
;;      HOSDEF - DEFINE HOS PARAMETER.
;

SYSCALL EQU     377Q            ; SYSCALL INSTRUCTION

        ORG     0

;       RESIDENT FUNCTIONS

.EXIT   DS      1               ; EXIT (MUST BE FIRST)
.SCIN   DS      1               ; SCIN
.SCOUT  DS      1               ; SCOUT
.PRINT  DS      1               ; PRINT
.READ   DS      1               ; READ
.WRITE  DS      1               ; WRITE
.CONSL  DS      1               ; SET/CLEAR CONSOLE OPTIONS
.CLRCO  DS      1               ; CLEAR CONSOLE BUFFER
.SYSRES DS      1               ; PRECEDING FUNCTIONS ARE RESIDENT

;       HDOSOVL0.SYS FUNCTIONS

        ORG     40Q

.LINK   DS      1               ; LINK  (MUST BE FIRST)
.CTLC   DS      1               ; CTL-C
.OPENR  DS      1               ; OPENR
.OPENW  DS      1               ; OPENW
.OPENU  DS      1               ; OPENU
.OPENC  DS      1               ; OPENC
.CLOSE  DS      1               ; CLOSE
.POSIT  DS      1               ; POSITION
.DELET  DS      1               ; DELETE
.RENAM  DS      1               ; RENAME
.SETTP  DS      1               ; SETTOP
.DECODE DS      1               ; NAME DECODE
.NAME   DS      1               ; GET FILE NAME FROM CHANNEL
.CLEAR  DS      1               ; CLEAR CHAN
.CLEARA DS      1               ; CLEAR ALL CHANS
.ERROR  DS      1               ; LOOKUP ERROR
.CHFLG  DS      1               ; CHANGE FLAGS
.DISMT  DS      1               ; FLAG SYSTEM DISK DISMOUNTED
