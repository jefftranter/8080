;;      S.INT - SYSTEM INTERNAL WORKAREA DEFINITIONS
;
;       THESE CELLS ARE REFERENCED AS OVERLAYS AMD MAIN CODE AND
;       MUST THEREFORE RESIDE IN FIXED LOW MEMORY.


        ORG     S.INT

;;      CONSOLE STATUS FLAGS

S.CDB   DS      1               ; CONSOLE DESCRIPTOR BYTE
CDB.H85 EQU     00000000B
CDB.H84 EQU     00000001B       ; =0 IF H8-5, =1 IF H8-4
S.BAUD  DS      2               ; [0-14]  H8-4 BAUD RATE, =0 IF H8-5
;                               ; [15]    =1 IF 2 STOP BITS

;;      TABLE ADDRESS WORDS

S.DLINK DS      2               ; ADDRESS OF DATA IN HDOS CODE
S.OFWA  DS      2               ; FWA  OVERLAY  TABLE
S.CFWA  DS      2               ; FWA  CHANNEL  TABLE
S.DFWA  DS      2               ; FWA  DEVICE   TABLE
S.RFWA  DS      2               ; FWA  RESIDENT HDOS CODE

;;      DEVICE DRIVER DELAYED LOAD FLAGS

S.DDLDA DS      2               ; DRIVER LOAD ADDRESS (HIGH BYTE=0 IF NO LOAD PENDING)
S.DDLEN DS      2               ; CODE LENGTH IN BYTES
S.DDGRP DS      1               ; GROUP NUMBER FOR DRIVER
        DS      1               ; HOLD PLACE
;S.DDSEC DS     2               ; SECTOR NUMBER FOR DRIVER ( * OBSOLETE ! * )
S.DDTA  DS      2               ; DEVICE'S ADDRESS IN DEVLST +DEV.RES
S.DDOPC DS      1               ; OPEN OPCODE PENDING

;;      OVERLAY MANAGEMENT FLAGS

OVL.IN  EQU     00000001B       ; IN MEMORY
OVL.RES EQU     00000010B       ; PERMANENTLY RESIDENT
OVL.NUM EQU     00001100B       ; OVERLAY NUMBER MASK
OVL.UCS EQU     10000000B       ; USER CODE SWAPPED FOR OVERLAY

S.OVLFL DS      1               ; OVERLAY FLAG
S.UCSF  DS      2               ; FWA SWAPPED USER CODE
S.UCSL  DS      2               ; LENGTH SWAPPED USER CODE
S.OVLS  DS      2               ; SIZE OF OVERLAY CODE
S.OVLE  DS      2               ; ENTRY POINT OF OVERLAY CODE

S.SSW   DS      2               ; SWAP AREA SECTOR NUMBER
S.OSN   DS      2               ; OVERLAY SECTOR NUMBER

;       SYSCALL PROCESSING WORK AREAS

S.CACC  DS      1               ; (ACC) UPON SYSCALL
S.CODE  DS      1               ; SYSCALL INDEX IN PROGRESS

;       JUMPS TO ROUTINES IN RESIDENT HDOS CODE

S.JUMPS EQU     $               ; START OF JUMP VECTORS
S.SDD   DS      3               ; JUMP TO STAND-IN DEVICE DRIVER
S.FASER DS      3               ; JUMP TO FATSERR (FATAL SYSTEM ERROR)
S.DIREA DS      3               ; JUMP TO DIREAD (DISK FILE READ)
S.FCI   DS      3               ; JUMP TO FCI (FETCH CHANNEL INFO)
S.SCI   DS      3               ; JUMP TO SCI (STORE CHANNEL INFO)
S.GUP   DS      3               ; JUMP TOP GUP (GET UNIT POINTER)

S.MOUNT DS      1               ; <>0 IF THE SYSTEM DISK IS MOUNTED
S.DCS   DS      1               ; DEFAULT CLUSTER SIZE-1

S.BOOTF DS      1               ; BOOT FLAGS
BOOT.P  EQU     00000001B       ; EXECUTE PROLOGUE UPON BOOTUP
BOOT.SY EQU     00000010B       ; SY: Device Driver loaded                 /2.1b/

;       STACK VALUE SAVED FOR OVERLAY SYSCALLS

S.OVSTK DS      2               ; VALUE OF SP UPON SYSCALLS USING OVERLAY

        DS      1               ; RESERVED

;;      ACTIVE I/O AREA/
;
;       THE AIO.XXX AREA CONTAINS INFORMATION ABOUT THE I/O OPERATION
;       CURRENTLY BEING PERFORMED. THE INFORMATION IS OBTAINED FROM
;       THE CHANNEL TABLE, AND WILL BE RESTORED THERE WHEN DONE.
;
;       NORMALLY, THE AIO.XXX INFORMATION WOULD BE OBTAINED DIRECTLY
;       FROM VARIOUS SYSTEM TABLES VIA POINTER REGISTERS. SINCE THE
;       8080 HAS NO GOOD INDEXED ADDRESSING, THE DATA IS MANUALLY
;       COPIES INTO THE AIO.XXX CELLS BEFORE PROCESSING, AND
;       BACKDATED AFTER PROCESSING.

AIO.VEC DS      3               ; JUMP INSTRUCTION
AIO.DOA EQU     $-2             ; DEVICE DRIVER INDEX
AIO.FLG DS      1               ; FLAG BYTE
AIO.GRT DS      2               ; ADDRESS OF GROUP RESERV TABLE
AIO.SPG DS      1               ; SECTORS PER GROUP
AIO.CGN DS      1               ; CURRENT GROUP NUMBER
AIO.CSI DS      1               ; CURRENT SECTOR INDEX
AIO.LGN DS      1               ; LAST GROUP NUMBER
AIO.LSI DS      1               ; LAST SECTOR INDEX
AIO.DTA DS      2               ; DEVICE TABLE ADDRESS
AIO.DES DS      2               ; DIRECTORY SECTOR
AIO.DEV DS      2               ; DEVICE CODE
AIO.UNI DS      1               ; UNIT NUMBER (0-9)

AIO.DIR DS      DIRELEN         ; DIRECTORY ENTRY

AIO.CNT DS      1               ; SECTOR COUNT
AIO.EOM DS      1               ; END OF MEDIA FLAG
AIO.EOF DS      1               ; END OF FILE FLAG
AIO.TFP DS      2               ; TEMP FILE POINTERS
AIO.CHA DS      2               ; ADDRESS OF CHANNEL BLOCK (IOC.DDA)



S.BDA   DS      1               ; Boot Device address (Setup by ROM) /80.09.gc/
S.SCR   DS      2               ; SYSTEM SCRATCH AREA ADDRESS
        DS      3
        ERRNZ   $-20526Q
S.OSI   DS      1               ; Operating System ID             /2.1b/
S.OSO   DS      2               ; Operating System Occurrance      /2.1b/
S.OSZ   DS      3               ; Operating System Sector Zero    /2.1b/
