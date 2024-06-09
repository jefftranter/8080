VERS    EQU     02
;  BIOS2, a BIOS module for CP/M 2.2
;  for use with Heath/Zenith H/Z89 and H-8 computers
;      and H17/H77/H87 5 1/4 inch disks
;      and H47/Z47     8     inch disks
;
; Copyright 1980, Heath Company, Benton Harbor, Michigan
;
;       Jim Tittsler
;       Heath/Zenith Software Group
;       Hilltop Road
;       Saint Joseph, Michigan

;FALSE  EQU     0
;TRUE   EQU     NOT FALSE

BIOS    EQU     $
BIOSSIZ EQU     1600H
BDOS    EQU     BIOS-0E00H
CCP     EQU     BDOS-0800H
CCPCLR  EQU     CCP+3
MEMT    EQU     BIOS+BIOSSIZ
BOOT    EQU     0000H           ;BASE OF USABLE RAM
IOBYTE  EQU     BOOT+3          ;I/O DEVICE ASSIGNMENT BYTE
LOGDSK  EQU     BOOT+4          ;WHERE CPM STORES DEFAULT DRIVE
FCB     EQU     BOOT+5CH        ;DEFAULT FILE CONTROL BLOCK
BUFF    EQU     BOOT+80H        ;DEFAULT DISK BUFFER
TPA     EQU     BOOT+100H       ;BASE OF TRANSIENT PGM AREA
STACK   EQU     MEMT            ;WHERE TO PUT BIOS STACK
;
;
; h17 disk related equates
;

MIDJMP  EQU     0C3H            ;8080 JUMP INSTRUCTION

H8CTL   EQU     0F0H            ;H8 CONTROL PORT
H8TR    EQU     0D0H            ;H8 CLOCK TICK RESET

H88CTL  EQU     0F2H            ;H88 CONTROL PORT
M1H     EQU     020H            ;KEEP RAM AT 0
CLKE    EQU     002H            ;TURN ON 2MS CLOCK

UPDP    EQU     07CH            ;DISK DATA PORT
UPFC    EQU     07DH            ;FILL CHARACTER
UPST    EQU     07DH            ;STATUS FLAGS
UPSC    EQU     07EH            ;SYNC CHARACTER (OUTPUT)
UPSR    EQU     07EH            ;SYNC RESET (INPUT)
DPDC    EQU     07FH            ;DISK CONTROL PORT

U0      EQU     02H             ;H17    UNIT 0
U1      EQU     04H             ;       UNIT 1
U2      EQU     08H             ;       UNIT 2
DFMO    EQU     10H             ;MOTOR ON (ALL DRIVES)
DFDI    EQU     20H             ;DIRECTION (0 = OUT)
DFST    EQU     40H             ;STEP COMMAND (ACTIVE HIGH)

DFT0    EQU     02H             ;TRACK 0 DETECT
DFWP    EQU     04H             ;WRITE PROTECT
DFSD    EQU     08H             ;SYNC DETECT

DSYN    EQU     0FDH            ;PREFIX SYNC CHARACTER

LPSA    EQU     20              ;NUMBER OF TRIES FOR CORRECT SECTOR
STSA    EQU     8/2+1           ;MS/2 TO WAIT FOR INDEX HOLE
STSB    EQU     12/2+1          ;MS/2 TO WAIT PAST INDEX HOLE
WHDA    EQU     20              ;UDLY COUNT FOR HOLE DEBOUNCE
WHNA    EQU     20              ;UDLY COUNT FOR HOLE DEBOUNCE
WSCA    EQU     64*25/20        ;LOOP COUNT FOR 25 CHARACTERS
WRITA   EQU     20              ;GUARDBAND COUNT FOR WRITE
WRITB   EQU     10              ;NUMBER OF ZERO CHARACTERS AFTER HOLE EDGE
WRITC   EQU     128/8           ;TWO CHARACTER DELAY BEFORE WRITING
READA   EQU     48              ;DELAY BEFORE HUNT MODE
SPD     EQU     250             ;250 * 4mS = 1 S
HLTG    EQU     20              ; 20 * 4mS = 80 mS
HST     EQU     24/4            ;HEAD SETTLE TIME 24 mS
STEPR   EQU     30/2            ;STEP RATE mS/2
DELAYS  EQU     6*256+15        ;HEAD LOAD AND MOTOR ON TIMER VALUES
RETRIES EQU     10              ;NUMBER OF RETRIES

CLKVEC  EQU     0008H           ;CLOCK INTERRUPT VECTOR
TICCNT  EQU     000BH           ;TWO BYTE TICK COUNTER
CTLPRT  EQU     000DH           ;CURRENT CONTENTS OF '89 CONTROL LATCH
H8FLAG  EQU     000EH           ;CONTENTS = 0 FOR H/Z89, = H8TR FOR H8
DEVCTL  EQU     000FH           ;CURRENT CONTENTS OF H17 CONTROL LATCH

DDEDTRK EQU     001H            ;BAD TRACK ERROR
DDEDHSY EQU     002H            ;HEADER SYNC ERROR
DDEDHCK EQU     004H            ;HEADER CHECKSUM
DDEDCHK EQU     008H            ;CHECKSUM ERROR
DDEDRNF EQU     010H            ;RECORD NOT FOUND
DDEDMDS EQU     020H            ;MISSING DATA SYNC
DDEDWRP EQU     040H            ;WRITE PROTECT ERROR
;
; H/Z47 DISK EQUATES
;
DSKCTL  EQU     078H            ;CONTROL PORT
DSKDAT  EQU     079H            ;DATA PORT

DSERR   EQU     001H            ;ERROR
DSDONE  EQU     020H            ;DONE (I.E. NOT BUSY)
DSIE    EQU     040H            ;INT ENABLE
DSTR    EQU     080H            ;TR

DCRES   EQU     002H            ;RESET
DCIE    EQU     040H            ;INT ENABLE

DRS     EQU     001H            ;READ STATUS
DRAS    EQU     002H            ;READ AUXILIARY STATUS
DSNS    EQU     003H            ;SET NUMBER OF SECTORS
DRD     EQU     007H            ;READ (BUFFERED)
DWR     EQU     008H            ;WRITE (BUFFERED)
;
;
;MISC CPM EQUATES
;

NDISKS  EQU     05              ;MAX NUM OF DISKS IN THIS SYSTEM
NSECTS  EQU     44              ;NUM SECTS TO READ ON WM BOOT
HSTSIZ  EQU     1024            ;MAX HOST (PHYSICAL) SECTOR SIZE

BTDWM   EQU     0FFH            ;WARM BOOT FLAG
BTDCD   EQU     000H            ;COLD BOOT FLAG
;
;DEFAULT PORT ASSIGNMENTS
;
H85CRT  EQU     372Q
H84TTY  EQU     0D0H
H84CRT  EQU     0E8H
H84LPT  EQU     0E0H
H84RDP  EQU     0D8H
;
;BAUD RATE DIVISORS FOR 8250'S
;
B75     EQU     1536
B110    EQU     1047
B134    EQU     857
B300    EQU     384
B600    EQU     192
B1200   EQU     96
B2400   EQU     48
B4800   EQU     24
B9600   EQU     12
B19200  EQU     6
;
;ASCII VALUES
;
NULL    EQU     00H
CTLC    EQU     03H
BELL    EQU     07H
CR      EQU     0DH
LF      EQU     0AH
PADCH   EQU     CR              ;CHAR THAT GETS NULL PADDING, MUST NOT BE NULL
;
;DEFAULT I/O BYTE
;       CON: = CRT:
;       RDR: = UR1:
;       PUN: = UP1:
;       LST: = UL1:
;

TTY     EQU     0

CRT     EQU     1
PTR     EQU     1
PTP     EQU     1

BAT     EQU     2
UR1     EQU     2
UP1     EQU     2
LPT     EQU     2

UC1     EQU     3
UR2     EQU     3
UP2     EQU     3
UL1     EQU     3

DIOB    EQU     (CRT) || (UR1 << 2) || (UP1 << 4) || (UL1 << 6)
;
;
; ENTRY POINT TABLE
;
        jmp     cboot           ;from cold start loader
wboote: jmp     wboot           ;to initiate a warm boot
        jmp     const           ;check console status
        jmp     conin           ;read console char
        jmp     conout          ;write console char
        jmp     list            ;write list device char
        jmp     punch           ;write punch device char
        jmp     reader          ;read char from reader
        jmp     home            ;set disk to track zero
        jmp     setdsk          ;select disk drive
        jmp     settrk          ;seek to track
        jmp     setsec          ;set sector number
        jmp     setdma          ;set starting address for disk I/O
        jmp     read            ;read selected sector
        jmp     write           ;write selected sector
        jmp     listst          ;check list device status
        jmp     sectran         ;sector translate routine

        db      0,0,0           ;expansion vector
;
        db      VERS            ;BIOS version
defiob: db      DIOB            ;default IOBYTE
        db      0               ;console type byte (reserved)
mun:    db      000H            ;disk mapping unit
mode:   db      00000000B       ;b0 = 1 means CRT on H8-5 card
;                               ;b1 = 1 means extended disk error messages
;                               ;b6 = 1 means run AUTO on warm boot
;                               ;b7 = 1 means run AUTO on cold boot
; serial device structures
;       db      portDnumber
;       dw      controlDword
;       where the controlDword contains
;               b15     map lower to upper case
;               b14-b12 number of nulls after a cr
;               b11-b00 baud rate divisor

h84pt1: db      H84CRT
crtbaud:dw      B9600
h84pt2: db      H84TTY
ttybaud:dw      B300
h84pt3: db      H84LPT
lptbaud:dw      B1200
h84pt4: db      H84RDP
rdpbaud:dw      B300
        db      0               ;reserved for 5th serial device structure
        dw      0
;
        dw      BIOSEND         ;reserved
secnt:  dw      0               ;h17 soft error count (since cold boot)
        db      NDISKS          ;number of disks possible in this BIOS
dpbase  equ     $
dpe0:   dw      xlt17,0000h     ;translate table
        dw      0000h,0000h     ;scratch
        dw      dirbuf,dpb17s   ;dir buff,parm blk
        dw      csv0,alv0       ;check,alloc vec
hdt0:   db      01000000b       ;disk type, address
        db      U0              ;select code
        db      2               ;number of 128 byte records/physical sector
        db      8               ;number of records/allocation block
trk0:   db      0FFh            ;track location
        db      STEPR           ;step rate
        db      0               ;reserved
ldm:    db      0               ;last disk mounted on drive
dpe1:   dw      xlt17,0000h
        dw      0000h,0000h
        dw      dirbuf,dpb17s
        dw      csv1,alv1
        db      01000000b
        db      U1
        db      2
        db      8
        db      0ffh
        db      STEPR
        db      0,0
dpe2:   dw      xlt17,0000h
        dw      0000h,0000h
        dw      dirbuf,dpb17s
        dw      csv2,alv2
        db      01000000b
        db      U2
        db      2
        db      8
        db      0ffh
        db      STEPR
        db      0,0
dpe3:   dw      xlt0s,0000H     ;translate table
        dw      0000H,0000H     ;scratch
        dw      dirbuf,dpb0ss   ;dir buff, parm blk
        dw      csv3,alv3       ;check, alloc vec
        db      10000000b
        db      000h
        db      1
        db      8
        db      0,0,0,0
dpe4:   dw      xlt0s,0000H
        dw      0000H,0000H
        dw      dirbuf,dpb0ss
        dw      csv4,alv4
        db      10000000b
        db      020h
        db      1
        db      8
        db      0,0,0,0
;
;
;       WARM BOOT -- read in BDOS and CCP 
;                    initialize
;                    jump to CCP
;

wboot:  lxi     sp,STACK        ;set stack pointer

        xra     a
        mov     c,a             ;boot from drive 0
        mov     e,a             ;act like this is the first login
        call    setdsk
        push    h               ;get pointer to xlate and spt value for
                                ; tracks 1 to n
        call    hlihl           ;get the pointer to the translate table
        shld    xltw1           ; and save it
        pop     h
        push    h
        lxi     d,10            ;get the pointer to the disk param block
        dad     d
        call    hlihl
        mov     a,m             ;the first entry of which is the number
        sta     spt1            ; of sectors per track

        pop     h
        lxi     d,16
        dad     d               ;get pointer to heath disk table
        mov     a,m
        ani     080h
        lda     spt1
        lhld    xltw1
        jz      wbt0            ;if not a '47, then use these values
        mvi     a,26            ;  else use single density values
        lxi     h,xlt0s         ;    for track 0
wbt0:   sta     spt
        shld    xltw

        xra     a               ;starting track = 0
        lxi     b,NSECTS*256+1  ; b = # of sectors, c = starting sector
        lxi     h,CCP-128       ; hl = starting address of track

wbt1:   sta     sektrk
        shld    dmab            ;starting dma adr for this track

wbt2:   push    b
        mvi     b,0             ;translate sector in bc
        lhld    xltw
        xchg
        call    sectran
        mov     c,l             ;translated sector in hl
        mov     a,l
        push    psw
        call    setsec          ;set this as the sector to read
        pop     psw
        dcr     a               ;change sector number to 0 thru spt-1
        call    cda             ;correct dma address for this sector
        shld    dmaadr
        mov     a,h
        cpi     BIOS/256        ;is this sector part of the BIOS
        jnc     wbt3            ;yes, so don't really read it

        call    read            ;read this sector

        ora     a               ;check for errors
        jnz     wbte

        pop     b
        dcr     b               ;count this sector as read
        jz      wbt4            ;if that is the last one, set pointers & leave

        push    b
wbt3:   pop     b
        inr     c               ;next sector
        lda     spt             ;number of sectors per track
        cmp     c               ;have we overflowed to next track?
        jnz     wbt2            ;no
        mvi     c,0
        call    cda             ;update track starting address

        push    h
        lda     spt1            ;beyond track 0,
        sta     spt             ; so update spt and xltw to be the values
        lhld    xltw1           ; for tracks 1 and beyond
        shld    xltw
        pop     h

        lda     sektrk
        inr     a
        jmp     wbt1

wbt4:   mvi     a,BTDWM         ;flag this as a warm boot

gow:    push    psw             ;save the boot type

        mvi     a,MIDJMP        ;initialize BIOS and BDOS vectors
        lxi     h,wboote
        sta     BOOT
        shld    BOOT+1
        lxi     h,BDOS+6
        sta     BOOT+5
        shld    BOOT+6

        lxi     b,BUFF          ;set default DMA address
        call    setdma

        xra     a               ;0 to accumulator
        sta     hstact          ;host buffer inactive
        sta     unacnt          ;clear unalloc count

        pop     psw             ;get the boot type
        rrc                     ; carry set if warm boot
        lda     mode
        jnc     gow1            ;if warm boot
        ral                     ; then shift left twice
gow1:   ral                     ;carry set if to run AUTO
        lda     logdsk          ;get disk number to
        sta     sekdsk          ; save as desired disk
        mov     c,a             ; pass to CCP in C
        jc      CCP             ;execute AUTO
        jmp     CCPCLR          ;execute CCP


wbte:   lxi     h,btmsg         ;print boot error message
        call    pmsg
        call    conin           ;wait for keyboard
        jmp     wboot           ;try again
;
;cda - correct dma address
;       entry   a       = records to adjust
;               dmab    = starting address of track
;       exit    hl      = corrected address
;

cda:    lhld    dmab            ;get starting address of track
        ora     a               ;clear carry
        rar                     ;divide by 2
        mov     d,a             ;  d = a/2
        mvi     a,0
        rar
        mov     e,a             ;  e = 00H or 80H
        dad     d
        ret
;
;
;       HOME - seek head to track 0 (deferred)
;

home:   lda     hstwrt          ;check for pending write
        ora     a
        jnz     homed
        sta     hstact          ;clear host active flag
homed:  mvi     c,0             ;set to track 0

;
;       SETTRK - set disk track number
;               entry:  c       desired track ( 0 to n-1 )
;

settrk: mov     a,c             ;get track number
        sta     sektrk
        ret
;
;
;       SETDSK - select a disk drive
;               entry:  c       desired disk
;                       e       lsb = 0 if first login
;

setdsk: lxi     h,0000H         ;default to error (hl=0000)
        mov     a,c             ;get the drive number from c
        cpi     NDISKS          ;is it a legal value?
        rnc                     ;no, return flagging error
        sta     lun             ;save logical unit name for mapping
        lxi     h,040h
        call    dada
        mov     a,m
        sta     sekdsk          ;save the disk number
        mov     l,a
        mov     a,e             ;save first login flag

        call    lt24            ;hl = 24 * 0,l
        lxi     d,dpbase        ;base of disk tables
        dad     d
        push    h
        lxi     d,16            ;offset to heath extended disk table
        dad     d
        shld    dpbx            ;save a pointer to drive specific values

        rar                     ;if not first login
        jc      setdx           ;  then parameters already known

        mov     a,m             ;get disk drive type
        ral                     ;msb=1 for non-h17 devices
        jnc     setdx           ;if type is h17 only one format available

        mvi     a,DRAS
        call    wcd

        inx     h
        mov     a,m
        ori     001h
        call    wbd

        call    w4tr
        in      DSKDAT          ;read the auxiliary status information
        push    psw

        ani     03h             ;find sector length
        cpi     2
        jc      setd1           ;if len <> 128 or 256
        mvi     a,2             ;  then make index 2

setd1:  add     a               ;*2 to allow for single/double density
        mov     d,a             ;save index to date

        pop     psw
        ani     10h             ;check for side 1
        mov     a,d
        jz      setd2

        ori     1               ;flag side 1 available

setd2:  add     a               ;*2
        add     a               ;*4
        add     a               ;*8

        lxi     h,h47pms
        call    dada

        xchg                    ;de now points to parameters for this disk
        pop     h               ;hl points to dpe for this drive
        push    h
        ldax    d               ;store the new translate table
        mov     m,a
        inx     d
        inx     h
        ldax    d
        mov     m,a
        inx     d

        push    d
        lxi     d,9             ;bump hl to point to dpb pointer
        dad     d
        pop     d

        ldax    d               ;now set up new dpb pointer
        mov     m,a
        inx     d
        inx     h
        ldax    d
        mov     m,a
        inx     d

        lhld    dpbx            ;get pointer to this drive
        mov     a,m             ;flag sidedness
        ani     0ffh-1
        mov     b,a
        ldax    d       
        ora     b
        mov     m,a             ;records per sector
        inx     d
        inx     h
        inx     h
        ldax    d
        mov     m,a             ;and records per allocation
        inx     d
        inx     h
        ldax    d
        mov     m,a

setdx:  pop     h               ;restore pointer to dpe in hl
        ret
;
;
;       8 inch disk descriptors
;
;       dw      &xlateDtable    ;pointer to translate table
;       dw      &paramDblock    ;pointer to parameter block
;       db      sides           ;0 = single ; 1 = double
;       db      recordsDperDsector
;       db      recordsDperDallocation
;       db      reserved


h47pms: dw      xlt0s,dpb0ss    ;for    single density  single sided
        db      0,1,8,0

        dw      xlt0s,dpb0sd    ;       single          double
        db      1,1,16,0

        dw      xlt0d,dpb0ds    ;       double          single
        db      0,2,16,0

        dw      xlt0d,dpb0dd    ;       double          double
        db      1,2,16,0

        dw      xlt0e,dpb0es    ;       extended        single
        db      0,8,16,0

        dw      xlt0e,dpb0ed    ;       extended        double
        db      1,8,16,0
;
;       SETSEC - set sector number
;               entry   c       desired sector (numbered 1 to spt )
;

setsec: mov     a,c             ;get sector number
        dcr     a               ;save 0 to spt-1
        sta     seksec
        ret

;
;       SETDMA - set disk I/O address
;

setdma: mov     h,b             ;move argument from bc to hl
        mov     l,c
        shld    dmaadr
        ret

;
;       SECTRAN - translate sector c using table at de
;

sectran: xchg                   ;hl points to table
        dad     b
        mov     l,m             ;l contains the translate sector
        mvi     h,0
        ret
;
;
;       READ - read the (logical) record set by setdsk, settrk, setsec
;              into memory at dmaadr, deblocking as necessary
;

wrall   equ     0               ;write to allocated
wrdir   equ     1               ;write to directory
wrual   equ     2               ;write to unallocated
;
;
;       read the selected CP/M sector
read:   xra     a
        sta     unacnt
        mvi     a,1
        sta     readop          ;read operation
        sta     rsflag          ;must read data
        mvi     a,wrual
        sta     wrtype          ;treat as unalloc
        jmp     rwoper          ;to perform the read
;
;       WRITE - write the (logical) record set by setdsk, settrk, setsec
;               from memory at dmaadr, blocking as necessary
;

write:  xra     a               ;0 to accumulator
        sta     readop          ;not a read operation
        mov     a,c             ;write type in c
        sta     wrtype
        cpi     wrual           ;write unallocated?
        jnz     chkuna          ;check for unalloc
;
;       write to unallocated, set parameters
        lhld    dpbx            ;set number of records per allocation
        inx     h
        inx     h
        inx     h
        mov     a,m
        sta     unacnt
        lda     sekdsk          ;disk to seek
        sta     unadsk          ;unadsk = sekdsk
        lda     sektrk
        sta     unatrk          ;unatrk = sectrk

        lhld    dpbx
        lxi     d,-16
        dad     d               ;hl points to pointer to xlate table
        call    hlihl           ;hl points to xlate table

        lda     seksec          ;get desired sector
        inr     a               ;correct to 1 to spt
        mvi     c,0             ;initialize index

write1: cmp     m               ;find sector's index
        jz      write2          ;  (which is the untranslated sector-1)

        inr     c               ;not this one, try the next
        inx     h
        jmp     write1

write2: mov     a,c             ;get the index
        sta     unasi           ;save it

;
;       check for write to unallocated sector
chkuna: lda     unacnt          ;any unalloc remain?
        ora     a
        jz      alloc           ;skip if not
;
;       more unallocated records remain
        dcr     a               ;unacnt = unacnt-1
        sta     unacnt
        lda     sekdsk          ;same disk?
        lxi     h,unadsk
        cmp     m               ;sekdsk = unadsk?
        jnz     alloc           ;skip if not
;
;       disks are the same
        lda     sektrk
        lxi     h,unatrk
        cmp     m               ;sektrk = unatrk?
        jnz     alloc           ;skip if not
;
;       tracks are the same
        lhld    dpbx
        lxi     d,-16
        dad     d
        call    hlihl
        lda     unasi
        call    dada
        lda     seksec
        inr     a
        cmp     m               ;seksec = unasi?
        lxi     h,unasi
        jnz     alloc           ;skip if not
;
;       match, move to next sector for future ref
        inr     m               ;unasi = unasi+1
        mov     a,m             ;end of track?
        push    h
        push    psw
        lhld    dpbx            ;get number of sectors/track from dpb
        lxi     d,-16+10
        dad     d
        call    hlihl           ;get dbpx
        pop     psw
        cmp     m               ;first entry of which is sec/track
        pop     h
        jc      noovf           ;skip if no overflow
;
;       overflow to next track
        mvi     m,0             ;unasi = 0
        lda     unatrk
        inr     a
        sta     unatrk          ;unatrk = unatrk+1
;
;       match found, mark as unnecessary read
noovf:  xra     a               ;0 to accumulator
        sta     rsflag          ;rsflag = 0
        jmp     rwoper          ;to perform the write
;
;       not an unallocated record, requires pre-read
alloc:  xra     a               ;0 to accum
        sta     unacnt          ;unacnt = 0
        inr     a               ;1 to accum
        sta     rsflag          ;rsflag = 1
;
;       enter here to perform the logical read/write
rwoper: xra     a               ;zero to accum
        sta     erflag          ;no errors (yet)
        lhld    dpbx            ;find logical sectors per physical
        mov     a,m             ;get disk type
        inx     h
        inx     h
        mov     b,m
        ani     080h            ;is this a '47?
        jz      rw0             ;if not, skip this check
        lda     sektrk          ;what track are we after?
        ora     a               ;if not track 0
        jnz     rw0             ;  then use the value of lsp in dpbx
        mvi     b,1             ;  else on track 1 it is 1!
rw0:    mov     a,b
        sta     lsp

        lda     seksec          ;compute host sector
rw1:    push    psw             ;save the phsyical sector (to date)
        mov     a,b             ;get shift factor (shift log2 sec per rec)
        rar
        mov     b,a
        jc      rw2
        pop     psw
        ora     a
        rar
        jmp     rw1
rw2:    pop     psw
        sta     sekhst          ;host sector to seek
;
;       active host sector?
        lxi     h,hstact        ;host active flag
        mov     a,m
        mvi     m,1             ;always becomes 1
        ora     a               ;was it already?
        jz      filhst          ;fill host if not
;
;       host buffer active, same as seek buffer?
        lda     sekdsk
        lxi     h,hstdsk        ;same disk?
        cmp     m               ;sekdsk = hstdsk?
        jnz     nomatch
;
;       same disk, same track?
        lda     sektrk
        lxi     h,hsttrk
        cmp     m               ;sektrk = hsttrk?
        jnz     nomatch
;
;       same disk, same track, same buffer?
        lda     sekhst
        lxi     h,hstsec        ;sekhst = hstsec?
        cmp     m
        jz      match           ;skip if match
;
;       proper disk, but not correct sector
nomatch: lda    hstwrt          ;host written?
        ora     a
        cnz     writehst        ;clear host buff
;
;       may have to fill the host buffer
filhst: lda     sekdsk
        sta     hstdsk
        lda     sektrk
        sta     hsttrk
        lda     sekhst
        sta     hstsec
        lda     readop
        ora     a
        jnz     fil1            ;yes it was a read              
        lda     lsp
        dcr     a
        jz      fil2            ;don't need to preread if physical=logical

fil1:   lda     rsflag          ;need to read?
        ora     a
        cnz     readhst         ;yes, if 1
fil2:   xra     a               ;0 to accum
        sta     hstwrt          ;no pending write
;
;       copy data to or from buffer
match:  lda     lsp
        dcr     a
        lxi     h,seksec
        ana     m
        lxi     h,0
        jz      m2
        lxi     d,128
m1:     dad     d
        dcr     a
        jnz     m1
;       hl has relative host buffer address
m2:     lxi     d,hstbuf
        dad     d               ;hl = host address
        xchg                    ;now in DE
        lhld    dmaadr          ;get/put CP/M data
        mvi     c,128           ;length of move
        lda     readop          ;which way?
        ora     a
        jnz     rwmove          ;skip if read
;
;       write operation, mark and switch direction
        mvi     a,1
        sta     hstwrt          ;hstwrt = 1
        xchg                    ;source/dest swap
;
;       C initially 128, DE is source, HL is dest
rwmove: ldax    d               ;source character
        inx     d
        mov     m,a             ;to dest
        inx     h
        dcr     c               ;loop 128 times
        jnz     rwmove
;
;       data has been moved to/from host buffer
        lda     wrtype          ;write type
        cpi     wrdir           ;to directory?
        lda     erflag          ;in case of errors
        rnz                     ;no further processing
;
;       clear host buffer for directory write
        ora     a               ;errors?
        rnz                     ;skip if so
        xra     a               ;0 to accum
        sta     hstwrt          ;buffer written
        call    writehst
        lda     erflag
        ret
;
;       WRITEHST performs the physical write to
;       the host disk, READHST reads the physical
;       disk.
;
;       READHST - perform physical sector read
;               hstdsk = host disk #, hsttrk = host track #,
;               hstsec = host sect #. read "hstsiz" bytes
;               into hstbuf and return error flag in erflag.
;

readhst: lda    hstdsk          ;get host disk
        call    shd             ;set host device pointer
                                ;  and perform logical to physical mapping
        sta     unit            ;shd returns unit in a

        mov     a,m
        ral
        jnc     readh1

rdh2:   mvi     a,DRD           ;do a buffered read
        call    setup
        jc      rderr

rdh3:   in      DSKCTL          ;get the control port
        ani     DSTR+DSDONE+DSERR ;wait for any line of interest
        jz      rdh3            ;nothing yet
        ani     DSDONE+DSERR    ;look for error or end of sector
        jnz     rdh4
        in      DSKDAT          ;get data byte
        mov     m,a             ;save it
        inx     h               ;bump memory pointer
        jmp     rdh3

rdh4:   in      DSKCTL          ;reread in case ERROR was late
        ani     DSERR           ;if there was no error
        rz                      ;  then return

rderr:  lxi     h,rdmsg
er47:   lda     mode            ;check for extended disk error messages
        ani     2               ;if they are not specified
        jz      er471           ;  then skip their printing

        mvi     a,DRS           ;read disk subsystem status
        call    wcd
        call    w4tr
        in      DSKDAT
        push    psw             ;save error code
        call    pmsg            ;print origin message
        lxi     h,errmsg        ;print "ERROR"
        call    pmsg
        mvi     c,'0'           ;set error from '47
        call    conout
        pop     psw             ;restore error code
        call    hout            ;print in hex
        lxi     h,crlf          ;point to crlf
        call    pmsg            ;print them
er471:  mvi     a,DCRES         ;reset the disk subsystem
        out     DSKCTL
        ei
        mvi     a,100           ;delay 200 mS (=100 * 2mS ticks)
        call    dly
        call    w4done          ;wait for the controller to be DONE

        mvi     a,0FFH          ;flag error
        sta     erflag
        ret

readh1: lda     hstsec
        sta     sector

        lda     hsttrk          ;get track
        sta     track

        call    rd17
        mvi     a,00h
        jnc     rdh1            ;carry set indicates error
        dcr     a               ;  if error, make erflag = 0FFh
rdh1:   sta     erflag
        ret
;
;
;       WRITEHST - write physical sector
;               hstdsk = host disk #, hsttrk = host track #,
;               hstsec = host sect #. write "hstsiz" bytes
;               from hstbuf and return error flag in erflag.
;               return erflag non-zero if error
;

writehst: lda   hstdsk
        call    shd             ;get hstdpb, also do physical -> logical map

        sta     unit

        mov     a,m             ;check device type
        ral
        jc      wrh2

        lda     hstsec          ;get sector number
        sta     sector

        lda     hsttrk
        sta     track

        call    wr17
        mvi     a,00h
        jnc     wrh1            ;carry set on error
        dcr     a               ;  make erflag = 0FFh on error
wrh1:   sta     erflag
        ret

wrh2:   mvi     a,DWR           ;do a h47 buffered write
        call    setup           ;do common setup
        jc      wrerr
        in      DSKCTL          ;check on disk status
        ani     DSERR
        jnz     wrerr           ;whoops
wrh3:   in      DSKCTL          ;check disk status
        ani     DSTR+DSDONE+DSERR
        jz      wrh3
        ani     DSDONE+DSERR
        jnz     wrh4
        mov     a,m             ;write out the next byte
        out     DSKDAT
        inx     h               ;bump memory pointer
        jmp     wrh3

wrh4:   in      DSKCTL          ;reread in case ERROR was late
        ani     DSERR
        rz

wrerr:  lxi     h,wrmsg
        jmp     er47            ;print the error message and code
;
;
;       SETUP - common h47 setup for read/write
;

setup:  call    wcd             ;write the command in a to disk controller
        rc

        lhld    hstdpb
        mov     a,m
        rar                     ;if least significant bit is 1 then ds
        mvi     a,0
        sta     side
        lda     hsttrk
        jnc     setup2          ;disk is single sided
        rrc                     ;divide track by two, side into msb
        push    psw
        ani     080h
        sta     side
        pop     psw
        ani     07fh

setup2: call    wbd
        rc

        push    b
        lhld    hstdpb
        inx     h
        mov     b,m             ;get specified unit

        lda     hstsec          ;get desired sector
        inr     a               ;IBM disk sectors numbered 1 to n
        ora     b               ;or in the selected drive

        lxi     h,side
        ora     m

        pop     b

        call    wbd
        rc

        lxi     h,hstbuf        ;(temporary) destination for data
        ret
;
;       WCD - write command to disk
;               disk should be "done" to accept a new command

wcd:    call    w4done          ;wait unitl done before commanding
        rc                      ;error - done timeout
        out     DSKDAT          ;send command
        call    w4nd
        ret

;
;       WBD - write byte to disk
;               a byte can be sent when TR is asserted

wbd:    call    w4tr
        rc
        out     DSKDAT
        ret

;
;       W4DONE - wait for done to be asserted
;               time out in about 4 sec, return with C set

w4done: push    psw
        push    b
        lxi     b,0FFFFH
w4d1:   in      DSKCTL
        ani     DSDONE          ;is it done yet?
        jnz     w4d2            ;yes, clean up and return
        dcx     b               ;decrement time out timer
        mov     a,b             ;is it zero yet?
        ora     c
        jnz     w4d1            ;no, wait a while longer
        pop     b               ;time out - return with C set
        pop     psw
        stc
        ret

w4d2:   pop     b
        pop     psw
        ora     a               ;clear carry
        ret

;
;       W4ND - wait for not done
;

w4nd:   push    psw
w4nd1:  in      DSKCTL
        ani     DSDONE
        jnz     w4nd1
        pop     psw             ;return after done removed
        ret

;
;       W4TR - wait for TR to be asserted
;

w4tr:   push    psw
w4tr1:  in      DSKCTL          ;get the disk status
        ani     DSDONE+DSTR
        jz      w4tr1
        ani     DSDONE
        jnz     w4tr2
        pop     psw
        ora     a
        ret
w4tr2:  pop     psw
        stc
        ret
;
; shd - set host dpb pointer
;       entry   a       host disk
;       exit    a       physical unit
;               hstdpb  points to host disk parameters
;

shd:    mov     l,a
        call    lt24            ;hl = 24 * 0,l
        lxi     d,DPBASE+16
        dad     d
        shld    hstdpb

; hl points to heath disk table for this unit

; perform logical to physical mapping
;   check for mapping unit which is guaranteed to exist

        push    h
        lxi     h,mun
        cmp     m
        pop     h
        jz      shd1            ;it is mapping unit

;   for other drives, check to see if they actually exist

        push    psw
        mov     a,m             ;get drive type
        ani     0C0h            ;if it is a real drive
        jnz     shd6            ;  then treat it as normal drive
        pop     psw

shd1:   lxi     h,ldm           ;  else see which disk was last here
        cmp     m
        jz      shd4            ;ok, new disk same as old

        push    h
        push    psw

        lda     lun             ;get logical disk name
        adi     'A'
        sta     mnmsga          ;save disk name in message

        lda     mun
        adi     '0'
        sta     mnmsgb          ;save unit number in message

        di
        xra     a
        lxi     h,dlymo
        mov     m,a             ;set dlymo to zero
        inx     h
        mov     m,a             ;  zero dlyh
        inx     h
        mov     m,a             ;  zero dlyw
        ei

        sta     DEVCTL
        out     DPDC            ;turn off drives

        lxi     h,mnmsg
        call    pmsg            ;prompt the user to change disks

shd2:   call    conin           ;get a character from the console

        cpi     CR              ;if char == CR
        jz      shd3            ;  then go ahead

        mvi     c,BELL          ;  else BELL terminal
        call    conout
        jmp     shd2            ;    and wait for another character

shd3:   lxi     h,CRLF
        call    pmsg

        mvi     a,0FFh          ;mark track as unknown for the new disk
        sta     trk0

        pop     psw
        pop     h
        mov     m,a             ;set last unit mounted

shd4:   lda     mun             ;new unit is mun
        mov     l,a             ;hl should point to table for mun
        call    lt24            ;hl = 24 * 0,l
        lxi     d,dpbase+16
        dad     d
        shld    hstdpb

shd5:   ret

shd6:   pop     psw             ;restore unit
        ret

mnmsg:  db      CR,LF
        db      'Put disk '
mnmsga: db      '. in 5.25 inch drive '
mnmsgb: db      '. and press RETURN',0
;
;
;  2 mS clock interrupt service routine
;

dlymo:  db      0
dlyh:   db      0
dlyw:   db      0

clock:  shld    hsav            ;save af,hl
        pop     h               ;get the return address
        shld    retsav          ;save it, but not on user stack
        push    psw             ;save af, hl

        lxi     h,CTLPRT        ;get the current value of the control port
        mov     a,m
        out     H88CTL          ;and output again, resetting int req

        inx     h               ;point to the H8FLAG
        mov     a,m
        ora     a               ;if 0 then running in H/Z89
        jz      clk0            ;  then don't output to 360Q
        out     H8CTL           ;  else contains H8TR to reset H8 clock

clk0:   lhld    TICCNT          ;get the tick counter
        inx     h               ;increment it
        shld    TICCNT

        mov     a,l             ;is it a multiple of 1/2 second?
        ora     a
        jnz     clkret          ;  if not

        lxi     h,dlymo         ;pointer to motor delay timer
        mov     a,m
        ora     a               ;if already zero
        jz      clkret          ;  then don't decrement

        dcr     m               ;decrement timer
        jnz     clk1            ;  if it has not timed out check heads

        lda     DEVCTL          ;get the current value of control port
        ani     0ffh-DFMO       ;turn off motor
        sta     DEVCTL
        out     DPDC

clk1:   inx     h               ;point to the head delay
        mov     a,m
        ora     a               ;if already zero
        jz      clkret          ;  then don't decrement

        dcr     m               ;decrement timer
        jnz     clkret          ;  if it has not timed out then skip

        lda     DEVCTL          ;deselect the drive
        ani     0ffh-U0-U1-U2
        sta     DEVCTL
        out     DPDC

clkret: lda     ticcnt  
        rar                     ;is it even, making 4mS big ticks
        jc      clkr2
        lxi     h,dlyw          ;check wait timer
        mov     a,m             ;  and decrement it if it is not
        ora     a               ;  already zero
        jz      clkr2
        dcr     m
clkr2:  pop     psw             ;restore the machine state
        lhld    retsav
        push    h
        lhld    hsav
        ei
        ret
;
;
; xok - exit from disk operation without error
; xit - exit flagging error in carry
;

xok:    xra     a               ;clear carry
xit:    push    psw
        di
        lxi     h,DELAYS        ;set disk motor and select timers
        shld    dlymo
        pop     psw
        ei
        ret
;
; rd17 - read a selected sector
;

rd17:   call    sdp             ;set parameters for this operation
rd171:  call    sdt             ;seek the desired track

        call    lps             ;find the proper sector
        jc      rd17e           ;couldn't find it
        mvi     b,0             ;read 256 bytes
        lxi     h,hstbuf        ;pointer to buffer
        call    wsc             ;wait for sync
        mvi     a,DDEDMDS       ;missing data sync error
        jc      rd17e           ;missing sync byte

rd172:  call    rdb             ;read a byte from the disk
        mov     m,a             ;put it in memory
        inx     h               ;increment pointer

        dcr     b               ;count byte as read
        jnz     rd172           ;more to read

        mov     b,d
        call    rdb             ;check checksum
        cmp     b
        jz      xok             ;everything is okay
        mvi     a,DDEDCHK       ;signal checksum error

rd17e:  call    h17e            ;h17 error handler
        jnc     rd171           ;try again
        jmp     xit             ;return, flagging error in carry
;
;
; wr17 - write a sector
;

wr17:   call    sdp             ;set disk parameters

wr171:  in      DPDC            ;see if write protected
        ani     DFWP
        mvi     a,DDEDWRP       ;possible write protect error
        jnz     wr17e           ;yes, it is a write protect error

        call    sdt             ;get correct track

        call    lps             ;find the proper sector
        jc      wr17e           ;couldn't find it

        mvi     b,0             ;256 bytes/sector
        lxi     h,hstbuf        ;pointer to source of data

        mvi     a,WRITA
wr172:  dcr     a
        jnz     wr172

        mvi     c,WRITB
        mvi     a,WRITC
        call    wsp             ;write the sync pattern

wr173:  mov     a,m
        call    wnb             ;write this data byte
        inx     h
        dcr     b
        jnz     wr173           ;loop to write all 256 bytes

        mov     a,d             ;write checksum
        call    wnb

        call    wnb             ;continue tunnel erase
        call    wnb             ;for 3 character times
        call    wnb

        lda     DEVCTL          ;off write gate
        out     DPDC
        jmp     xok

wr17e:  ori     080h            ;mark as a write error
        call    h17e            ;call the h17 error handler
        jnc     wr171           ;try again
        jmp     xit             ;return flagging error in carry
;
; h17e - h17 error handler
;       entry   a - error type
;       exit    C - set if retries exhausted

h17e:   ei
        sta     errtyp          ;save the error type

        ani     07Fh            ;strip off read/write
        cpi     DDEDWRP         ;is it a write protect violation?
        jz      h17e4           ;if so, then automatic hard error

        lhld    secnt           ;bump the soft error count
        inx     h
        shld    secnt

        lxi     h,errcnt        ;get a pointer to the retry counter
        dcr     m               ;  decrement the retry counter
        jz      h17e4           ;retries exhausted, flag hard error

;       based on error type and retry number
;         select from the following retry actions
;               seek track 0    if bad track error or retry 5
;               just try again  if retry odd
;               move in then out        if retry & 2 = 0
;               move out then in        if retry & 2 = 1

        lda     errtyp
        ani     07Fh            ;strip off possible write flag
        cpi     DDEDTRK         ;was it a bad track error
        jz      h17e2           ;yes, go do a seek track zero

        mov     a,m             ;fetch retry count
        cpi     5               ;if == 5
        jz      h17e2           ;  then seek track zero

        rar                     ;if odd, then try again in place
        cmc                     ;complement carry
        rnc                     ;return with carry clear

        lhld    trkpt           ;get pointer to current track

        rar                     ;is b1 of errcnt = 1?
        mov     a,m             ;get current track
        jc      h17e1           ;yes, so move out then in

        cpi     39              ;if it is already at the maximum track
        jz      h17e3           ;  then don't do anything

        inr     m               ;increment current track
        call    mai             ;  move arm in one track
        jmp     h17e3

h17e1:  ora     a               ;if it is already at minimum track
        jz      h17e3           ;  then don't do anything

        dcr     m               ;decrement current track
        call    mao             ;  move arm out one track
        jmp     h17e3

h17e2:  call    stz

h17e3:  xra     a               ;clear carry
        ret

;       hard error has occurred
;         (optionally) print error message
;         return with Carry set

h17e4:  lda     mode            ;does the user want error messages
        rar
        rar
        jnc     h17e6           ;no error messages

        lda     errtyp
        ral                     ;read or write error
        lxi     h,rdmsg         ;assume read
        jnc     h17e5           ;if msb set
        lxi     h,wrmsg         ;  then assumption wrong, it is write error
h17e5:  call    pmsg            ;print the message
        lxi     h,errmsg        ;print ' ERROR '
        call    pmsg
        mvi     c,'1'           ;flag this as an h17 error
        call    conout
        lda     errtyp          ;get the error type
        ani     07FH            ;and off the read/write flag
        call    hout            ;then print it out in hex
        lxi     h,crlf          ;finish message with cr, lf
        call    pmsg

h17e6:  stc                     ;return flagging hard error in carry
        ret
;
;
; sdp - set device parameters
;       set retry count, make sure motor is on and drive selected
;

sdp:    ei                      ;make certain interrupts are enabled
                                ;  for disk I/O
        mvi     a,RETRIES
        sta     errcnt

        lxi     h,0
        shld    dlymo

        lhld    hstdpb          ;get device select code
        inx     h
        mov     a,m
        ori     DFMO            ;turn on the motor
        out     DPDC

        mov     b,a
        lxi     h,DEVCTL        ;what was its state?
        mov     a,m
        ani     DFMO            ;was the motor on?
        jnz     spd2            ;yes, don't have to wait for it

        push    h
        lhld    hstdpb          ;find out how fast it comes up
        lxi     d,5
        dad     d
        mov     a,m
        pop     h
        ral
        mvi     a,SPD           ;up to speed in SPD * 4 mS
        jnc     spd3
        rar                     ;new drives up in 1/4 time
        rar
        ani     03fh
        jmp     spd3

spd2:   mov     a,m
        ani     U0+U1+U2        ;check the available units
        ana     b               ;was this unit selected?
        mvi     a,0
        jnz     spd3            ;this head was already loaded
        mvi     a,HLTG          ;must wait for head load timing
spd3:   sta     dlyw

        mov     a,b
        sta     DEVCTL

        lhld    hstdpb          ;get the current track for this disk
        lxi     d,4             ;offset to track in disk tables
        dad     d               ;get address of track for this drive
        shld    trkpt
        mov     a,m
        ral                     ;if msb is 0
        rnc                     ;  then track is pointed to by trkpt
        call    stz             ;  else head position is unknown and
        ret                     ;    is zeroed
;
; sdt - seek desired track
;       seek to track updating *trkpt
;
sdt0:   inr     m
        call    mai

sdt:    lhld    trkpt
        lda     track
        cmp     m
        jz      sdt1            ;at desired track
        jp      sdt0            ;  must move arm in
                                ;  else must move arm out
        dcr     m
        call    mao

        jmp     sdt


sdt1:   lda     dlyw            ;make certain to delay for head settle time
        cpi     HST             ;is wait already > head settle
        rnc                     ;if so, return
        mvi     a,HST           ;  else be sure to delay for head settle
        sta     dlyw
        ret

;
; stz - seek track 0
;       called during error recovery and to initially position heads

stz0:   call    mao             ;move the arm out
stz:    in      DPDC            ;check the track zero sensor
        ani     DFT0
        jz      stz0            ;if not set, then move out another track
        lhld    trkpt           ;zero track indicator for this drive
        mvi     m,0
        jmp     sdt1            ;head settle delay in case going to 0

;
; mai - move arm in
; mao - move arm out
;

mai:    mvi     a,DFDI          ;set direction
        jmp     mao1
mao:    xra     a               ;set direction
mao1:   push    h
        mov     h,a
        lda     DEVCTL          ;get current value of disk port
        ora     h               ;or in direction
        out     DPDC            ; send it to disk
        ori     DFST            ;or in step
        out     DPDC            ; send it to disk
        xri     DFST            ;clear step
        out     DPDC            ; send it to disk
        lhld    hstdpb          ;get step rate
        lxi     d,5
        dad     d
        mov     a,m
        ani     07Fh
        pop     h
;       call    dly             ;implicit call dly and ret
;       ret

;
; dly - delay A * 2 mS
;

dly:    push    h
        lxi     h,TICCNT        ;pointer to tick counter, incremented every 2mS
        add     m               ;value of TICCNT when delay completed
dly1:   cmp     m               ;wait for TICCNT to catch up
        jnz     dly1
        pop     h
        ret

;
; lps - locate proper sector
;

lps0:   call    sts             ;skip this sector

lps:    lda     dlyw            ;ready to read yet?
        ora     a
        jnz     lps0            ;if not, wait a sector time

        mvi     b,LPSA

lps1:   di
        call    wsc             ;wait for a sync character
        mvi     a,DDEDHSY       ;flag header sync error
        jc      lps2            ;couldn't find one

        call    rdb             ;read the volume number
        call    rdb             ;read the track number
        lxi     h,track
        cmp     m
        mvi     a,DDEDTRK       ;bad track error
        jnz     lps2            ;wrong track

        call    rdb             ;read the sector number
        inx     h               ;point to sector
        cmp     m
        mvi     a,DDEDRNF       ;record not found error
        jnz     lps2            ;wrong sector

        mov     h,d
        call    rdb             ;do checksum on header
        cmp     h
        rz                      ;okay
        mvi     a,DDEDHCK       ;header checksum is wrong

lps2:   push    psw
        call    sts             ;skip this sector
        pop     psw
        dcr     b               ;another time passes quickly past
        jnz     lps1

        stc                     ;enough already
        ret

;
; sts - skip this sector
;       exit at beginning of next sector
;       1. If head is not over a hole, wait 8 mS while hole checking.
;          If no hole in this time, then we are in a regular gap.
;          Wait for the next hole and exit.
;       2. If the head is over a hole, or becomes so during the 8 mS
;          wait, then wait for the hole to pass, wait 12 mS in case of
;          the index hole, then wait for the next hole and exit.
;

sts:    ei
        push    b
        in      DPDC            ;check the disk port
        rar                     ;for sector holes
        jc      sts2            ;currently over a hole

;       no hole yet, wait 8 mS min (10 mS max) for hole to appear

        lxi     h,ticcnt
        mov     b,m
sts1:   in      DPDC
        rar
        jc      sts2            ;found a hole

        mvi     a,STSA
        add     b
        cmp     m
        jnz     sts1            ;8 mS still not up
        jmp     sts3            ;found a sector gap

;       have hole. skip it and wait 12 mS

sts2:   call    wnh             ;wait for no hole
        mvi     a,STSB
        call    dly
sts3:   pop     b
        di

;
; whd - wait hole detect
;

whd:    in      DPDC            ;watch the disk control port
        rar                     ;until a hole is found
        jnc     whd             ; still no hole
        mvi     a,WHDA          ;set up loop delay count
        jmp     udly

;
; wnh - wait for no hole
;

wnh:    in      DPDC            ;watch the disk control port
        rar                     ;until the current hole is past
        jc      wnh
        mvi     a,WHNA          ;set up loop delay count

;
; udly - microsecond delay
;       called with interrupts disabled to wait
;       A * ( 15 / 2.048 ) microseconds on 8080
;       A * ( 14 / 2.048 ) microseconds on Z80

udly:   dcr     a
        jnz     udly
        ret

;
; rdb - read byte from disk
;

rdb:    in      UPST            ;is a byte ready?
        rar
        jnc     rdb             ;  wait until ready

        in      UPDP            ;get the byte
        mov     e,a             ;save it in E
        xra     d               ;update CRC
        rlc
        mov     d,a
        mov     a,e             ;restore byte read to A
        ret

;
; wsc - wait sync character
;       wsc waits for the appearance of a sync character.  The disk
;       should be selected, moving, and the head should be over the
;       pre-sync zero band
;
;       if sync is not found in 25 character times, error

wsc:    mvi     a,READA         ;delay past garbage byte
wsc0:   dcr     a
        jnz     wsc0
        mvi     a,DSYN          ;set up sync character
        out     UPSC
        in      UPSR            ;reset sync search
        mvi     a,WSCA          ;number of loops in 25 characters
        mov     d,a
wsc1:   in      DPDC
        ani     DFSD            ;check for sync
        jnz     wsc2            ;got it
        dcr     d
        jnz     wsc1            ;try until time-out

        stc                     ;couldn't find sync
        ret

;       found sync character

wsc2:   in      UPDP            ;gobble the sync character
        mvi     d,0             ;clear checksum
        ret

;
; wsp - write sync pattern
;       wsp writes a sync pattern of zeros, followed by a sync character
;
;       entry   A       initial delay counter
;               C       number of zero bytes to write

wsp:    dcr     a               ;delay
        jnz     wsp

;       delay is up, turn on write gate

        lda     DEVCTL
        inr     a               ;set write gate on
        out     DPDC

;       write # of zeros specified in C

wsp1:   xra     a
        call    wnb             ;write a zero
        dcr     c               ;count it
        jnz     wsp1

        mvi     a,DSYN          ;write a sync character
        mov     d,a             ;pre-clear checksum, so wnb exits with D = 0
;       jmp     wnb             ;implicit call, return wnb

;
; wnb - write next byte
;       write a byte to disk presuming write gate already on
;

wnb:    mov     e,a             ;save character to be written
wnb1:   in      UPST            ;is USRT ready for another character
        ana     a               ;set flags
        jp      wnb1            ;not ready, wait some more

        mov     a,e             ;get character
        out     UPDP            ;write it to disk
        xra     d               ;update CRC
        rlc
        mov     d,a
        ret
;
;
; dada -  add 0,a  to hl
;       uses af

dada:   add     l
        mov     l,a
        rnc
        inr     h
        ret
;
; hlihl - load hl indirect through hl
;       uses af

hlihl:  mov     a,m
        inx     h
        mov     h,m
        mov     l,a
        ret
;
; lt24 - multiply l by 24, result in hl
;       uses de,f

lt24:   mvi     h,0
        dad     h               ;*2
        dad     h               ;*4
        dad     h               ;*8
        mov     d,h
        mov     e,l
        dad     h               ;*16
        dad     d               ;*24 = *16 + *8
        ret
;
;**********************************************
;
;       LOGICAL DEVICE ROUTINES
;
;       these routines handle the logical to physical
;       device mapping established by the CP/M IOBYTE
;

;
; console status
;

const:  call    cons            ;get status of specific device
        ora     a
        rz                      ;if not ready return 0 in a
        mvi     a,0FFH          ; else return FF
        ret
;
cons:   lda     IOBYTE          ;use bits 1-0 for console device
        call    indxit
        dw      ttystat
        dw      crtstat
        dw      rdrst           ;2: batch mode (use reader device)
        dw      crtstat
;
; reader status

rdrst:  lda     IOBYTE
        rrc
        call    gotoit
        dw      ttystat
        dw      busy            ;unimplemented inputs
        dw      mdstat
        dw      crtstat
;
; console input
;
conin:  lda     IOBYTE
        call    indxit
        dw      ttyin           ;0: TTY 
        dw      crtin           ;1: CRT
        dw      reader          ;2: BAT (reader input)
        dw      crtin           ;UC1: CRT input, LST: output
;
; console out
;
conout: lda     IOBYTE
        call    indxit
        dw      ttyout          ;0: TTY
        dw      crtout          ;1: CRT
        dw      list            ;2: BAT (output to LST)
        dw      list            ;UC1: CRT input, LST: output
;
; listst - list status check
;

listst: lda     IOBYTE          ;get the current IOBYTE
        rlc                     ;shift into position
        rlc
        call    indxit
        dw      ttyos           ;0: tty
        dw      crtos           ;1: crt
        dw      lptos           ;2: lpt
        dw      dbdos           ;3: diablo

;
; list out
;

list:   lda     IOBYTE
        rlc                     ;bits 7-6 to 2-1
        rlc
        call    indxit
        dw      ttyout          ;0: tty
        dw      crtout          ;1: crt
        dw      lptout          ;2: lpt
        dw      dbd             ;3: diablo
;
; punch out
;

punch:  lda     IOBYTE          ;bits 4-5 to 1-2
        rrc
        rrc
        rrc
        call    gotoit
        dw      ttyout          ;0: TTY
        dw      dmyout
        dw      mdout           ;2: UP1 modem port output
        dw      crtout
;
; reader in
;

reader: lda     IOBYTE          ;bits 3-2 to 2-1
        rrc
        call    gotoit
        dw      ttyin           ;0: TTY
        dw      dmyin
        dw      mdin            ;2: UR1 modem port input
        dw      crtin
;
; dispatch subroutine - indexed table jump
;
indxit: rlc
gotoit: ani     06H             ;mask bits
        xthl                    ;save hl, get table address
        call    dada            ;add 0,a to hl
        call    hlihl           ;get address in hl
        xthl                    ;xchg routine address, old hl
        ret                     ;dispatch
;
;*****************************************************
;
;       PHYSICAL DEVICE ROUTINES
;
; accessed via the logical device routines above
;

;
;"CRT" physical status routine
;       uses h84pt1

crtstat:
        lda     mode            ;get the mode byte
        rar                     ;if the lsb = 1
        jc      crts1           ;  then console on H8-5

        lxi     h,h84pt1        ;pointer to base port
        jmp     us              ;get status

crts1:  in      H85CRT+1        ;get 8251 status register
        ani     02H             ;mask RxRDY
        ret

;
;"CRT" physical input routine
;

crtin:  lda     mode            ;get mode byte
        rar                     ;if lsb = 1
        jc      crti1           ;  then console on H8-5

        lxi     h,h84pt1
        call    ui              ;get char from 8250
        ani     7Fh             ;mask parity
        ret

crti1:  in      H85CRT+1        ;check if RxRDY
        ani     02h
        jz      crti1           ;wait for character

        in      H85CRT          ;get character
        ani     7Fh             ;mask parity
        ret

;
;"CRT" physical output routines
;

crtout: call    crtos
        ora     a
        jz      crtout

        lda     mode            ;get mode byte
        rar                     ;if lsb = 1
        jc      crto1           ;  then console on H8-5

        jmp     uo              ;output character in c

crto1:  push    h
        inx     h
        inx     h               ;point to flag byte
        mov     a,m
        ral
        pop     h
        mov     a,c
        cc      muc             ;map to upper case
        out     H85CRT

        jmp     pout2           ;check for nulls

;
; tty input
;

ttyin:  lxi     h,h84pt2
        call    ui
        ani     7Fh             ;mask parity
        ret
;
; tty status
;

ttystat:lxi     h,h84pt2
        jmp     us              ;get status

;
; tty output
;

ttyout: call    ttyos
        ora     a
        jz      ttyout
        jmp     uo              ;output character in c

;
; line printer out
;

lptout: lda     dclpos          ;if DON'T CHECK LP OUTPUT STATUS
        ora     a
        jnz     lptou2          ;  then skip the test

lptou1: call    lptos           ;  else, wait for READY LP output status
        ora     a
        jz      lptou1

lptou2: lxi     h,h84pt3        ;pointer to device structure
        lxi     d,lptcts        ;and one to char to send

        xra     a               ;force a check of LP OUTPUT STATUS
        sta     dclpos          ;  next time

        jmp     uo

;
; Diablo ETX/ACK Protocol Driver
;

dbd:    call    dbdos
        ora     a
        jz      dbd
        call    uo              ;send character in C to printer

        lxi     h,hscnt         ;update handshake count
        dcr     m
        cpi     01Bh            ;ESC?
        mov     a,m
        jnz     dbd1            ;was not an escape
        cpi     2               ;last char was escape,
        rnc                     ;  make certain at least two chars follow
        mvi     m,2             ;  without intervening ETX
        ret
dbd1:   ora     a               ;time to handshake?
        rnz
        mvi     a,1             ;tell dbdos it is time to handshake
        stax    d
        ret

hscnt:  db      32

;
; mdin - modem input routine
;

mdin:   lxi     h,h84pt4
        jmp     ui

;
; mdstat - modem input status
;

mdstat: lxi     h,h84pt4
        jmp     us

;
; mdout - modem output
;

mdout:  call    mdos
        ora     a
        jz      mdout
        jmp     uo

;
; mdos, ttyos, and crtos - modem, tty, and crt output status
;       returns 00 for busy
;               FF for ready to accept another character

mdos:   lxi     h,h84pt4
        lxi     d,mdcts
        jmp     crtos1

ttyos:  lxi     h,h84pt2
        lxi     d,ttycts
        jmp     crtos1

crtos:  lxi     h,h84pt1
        lxi     d,crtcts

        lda     mode            ;handle H8-5 case specially
        rar
        jc      crtos3

crtos1: mov     a,m             ;check to see if the UART can take a char
        adi     5
        call    pin
        ani     020h            ;if not
        jz      crtosb          ;  then return flagging BUSY

        ldax    d               ;see if there are any NULLs to be sent
        ora     a
        jnz     crtos2          ;if so, go send one

        dcr     a               ;else, set READY
        ret

crtos2: dcr     a               ;count this NULL as sent
        stax    d
        push    b

        mvi     c,NULL          ;send a NULL
        call    uo

        pop     b

crtosb: xra     a               ;return claiming to be still busy
        ret

crtos3: in      H85CRT+1        ;special case: H8-5 serial card
        rar
        jnc     crtosb          ;still busy

        ldax    d               ;any NULLs still to send?
        ora     a
        jnz     crtos4          ;if so, go send one

        dcr     a               ;return READY
        ret

crtos4: dcr     a               ;count this NULL as sent
        stax    d

        mvi     a,NULL          ;send a NULL
        out     H85CRT

        xra     a               ;return BUSY
        ret

;
; lptos - line printer output status
;       with hardware handshake
;

lptos:  lxi     h,h84pt3
        lxi     d,lptcts

        mov     a,m             ;check to see if the UART can take a char
        adi     5
        call    pin
        ani     20h
        jz      lptosb          ;the UART is still busy

;       CTS handshake used for H14/WH24
;               if your printer does not use CTS handshake to indicate "busy"
;               delete the next 5 lines

        mov     a,m             ;get the base address
        adi     6               ;add the offset to point to 8250 modem status
        call    pin
        ani     010H            ;mask clear to send bit
        jnz     lptosb          ;the printer is busy

        ldax    d               ;any nulls to send?
        ora     a
        jnz     lptos1          ;yes, there are nulls required

        dcr     a               ;no, return with a = 0FFh indicating READY
        sta     dclpos          ;flag don't check lp status
        ret

lptos1: dcr     a               ;count this null as sent
        stax    d

        push    b               ;save the original character

        mvi     c,NULL
        call    uo

        pop     b

lptosb: xra     a               ;indicate busy
        ret

;
; dbdos - diablo output status
;       if cts == 0 then okay to send characters
;          cts == 1 then send ETX, set cts to 2
;          cts == 2 then wait for ACK, then set cts to 0

dbdos:  lxi     h,h84pt3
        lxi     d,dbdcts

        ldax    d               ;find out the state of output
        cpi     2               ;if not 2,
        jnz     dbdos1          ;  then go do output

; must receive an ACK from the printer

        mov     a,m             ;check UART for incoming
        adi     5
        call    pin
        ani     1
        jz      dbdosb          ;no character back from printer yet
                                ;  so flag BUSY
        mov     a,m
        call    pin             ;get the character
        ani     07Fh            ;strip off parity
        sui     'F' # 32      ;compare it to ACK
        jnz     dbdosb          ;not an ACK, so still busy
        stax    d               ;was an ACK, so able to send more chars

        mvi     a,32            ;reset the handshake count
        sta     hscnt

dbdos1: mov     a,m             ;is UART ready to send another char?
        adi     5
        call    pin
        ani     020h
        jz      dbdosb          ;UART is not ready to accept a character

        ldax    d               ;is it time to send ETX?
        ora     a
        jnz     dbdos2          ;yes, go send ETX

        dcr     a               ;no, indicate READY (a == 0FFh)
        ret

dbdos2: inr     a               ;flag that the next thing to do is wait for ACK
        stax    d

        push    b

        mvi     c,'C' # 32      ;send the ETX
        call    uo

        pop     b

busy:
dbdosb: xra     a
        ret

;
; dummy input and output routines
;

dmyin:  mvi     a,'Z'-40H       ;unimplemented inputs return CTL-Z
dmyout: ret                     ;dummy outputs do nothing

;
;
; 8250 I/O routines
;

; US - get uart (input) status

us:     mov     a,m
        adi     5               ;offset to the status register
        call    pin
        ani     1               ;mask the data available bit
        ret

; UO - output to uart

uo:     mov     a,m
;       jmp     pout

;
; pout - output byte in c to port in a
;

pout:   sta     pout1+1
        push    h
        inx     h
        inx     h               ;point to flag byte
        mov     a,m
        ral
        pop     h
        mov     a,c
        cc      muc             ;map to upper case
pout1:  out     00h             ;self-modifying code
pout2:  cpi     PADCH           ;check if this char needs padding (usually CR)
        rnz                     ;no

        push    h               ;find out number of nulls required
        inx     h
        inx     h
        mov     a,m             ;get count from data structure
        pop     h
        rar                     ;shift into least sig 3 bits
        rar
        rar
        rar
        ani     07h             ;mask only count
        rz                      ;return if no nulls are required
        stax    d               ;save count of nulls to send in XXXCTS

        ret


; UI - input from uart

ui:     mov     a,m
        adi     5
        call    pin
        rar
        jnc     ui
        mov     a,m
;       jmp     pin

;
; pin - input byte from port in a
;

pin:    sta     pin1+1
pin1:   in      00h             ;self-modifying code
        ret

;
; muc - map character in a to upper case
;

muc:    cpi     'a'             ;if less than lower case a
        rc                      ;  then already upper case
        cpi     'z'+1           ;if greater than lower case z
        rnc                     ;  then not a lower case letter
        sui     'a'-'A'         ;convert to upper case
        ret

;
; PMSG - print the message at hl until null
;

pmsg:   mov     a,m             ;get a char
        ora     a               ;check for null
        rz                      ;end of message
        mov     c,a             ; else
        push    h               ;save the pointer
        call    conout          ; print this character
        pop     h
        inx     h               ; point to next
        jmp     pmsg            ;repeat

;
; HOUT - hex output routine
;       type contents of a in hex on console

hout:   push    psw             ;save contents of a
        rrc
        rrc
        rrc
        rrc
        call    nibble          ;put out high order nibble
        pop     psw             ;fall through to put out low nibble
nibble: ani     0FH             ;mask
        cpi     10              ;> 10 ?
        jm      nibbl1          ;if 0-9
        adi     7               ;  else convert to A-F
nibbl1: adi     30H             ;binary to ASCII
        mov     c,a             ;type it on the console
        jmp     conout
;
; BIOS messages
;
btmsg:  db      CR,LF
        db      'Error during warm boot - press any key',0
rdmsg:  db      CR,LF,'READ',0
wrmsg:  db      CR,LF,'WRITE',0
errmsg: db      ' ERROR ',0
crlf:   db      CR,LF,0

dclpos: db      0               ;force a check of lp output status
ttycts: db      0               ;chracters to send count for    tty
crtcts: db      0               ;                               crt
lptcts: db      0               ;                               lpt
mdcts:  db      0               ;                               modem
dbdcts: db      0               ;output state machine for       dbd

;
dpb17s: dw      20              ;sec per track
        db      3
        db      7
        db      0
        dw      91              ;disk size (in k) - 1
        dw      63
        db      192
        db      0
        dw      16
        dw      3

dpb0ss: dw      26              ;sectors per track
        db      3,7,0           ;block shift, block mask, extent mask
        dw      242             ;disk size - 1
        dw      63              ;dir max
        db      192,0           ;alloc0,alloc1
        dw      16              ;check size
        dw      2               ;offset

dpb0sd: dw      26              ;sectors per track
        db      4,15,1          ;block shift, mask, extent mask
        dw      246             ;disk size - 1
        dw      127             ;dir max
        db      0c0h,000h       ;allocation
        dw      32
        dw      2

dpb0ds: dw      52
        db      4,15,0
        dw      242
        dw      127
        db      0c0h,000h
        dw      32
        dw      2

dpb0dd: dw      52
        db      4,15,0
        dw      493
        dw      255
        db      0f0h,000h
        dw      64
        dw      2

dpb0es: dw      64
        db      4,15,0
        dw      299
        dw      127
        db      0c0h,000h
        dw      32
        dw      2

dpb0ed: dw      64
        db      4,15,0
        dw      607
        dw      255
        db      0f0h,000h
        dw      64
        dw      2
;
xlt17:  db      1,2,9,10,17,18
        db      5,6,13,14
        db      3,4,11,12,19,20
        db      7,8,15,16

xlt0s:  db      1,7,13,19,25
        db      5,11,17,23
        db      3,9,15,21
        db      2,8,14,20,26
        db      6,12,18,24
        db      4,10,16,22

xlt0d:  db      1,2,19,20,37,38
        db      3,4,21,22,39,40
        db      5,6,23,24,41,42
        db      7,8,25,26,43,44
        db      9,10,27,28,45,46
        db      11,12,29,30,47,48
        db      13,14,31,32,49,50
        db      15,16,33,34,51,52
        db      17,18,35,36

xlt0e:  db      1,2,3,4,5,6,7,8
        db      9,10,11,12,13,14,15,16
        db      17,18,19,20,21,22,23,24
        db      25,26,27,28,29,30,31,32
        db      33,34,35,36,37,38,39,40
        db      41,42,43,44,45,46,47,48
        db      49,50,51,52,53,54,55,56
        db      57,58,59,60,61,62,63,64
;
;
hstbuf  equ     $

;
; the following "one-time" code gets overlaid by disk buffers
;

;
; BOOT - executed for cold start
;
cboot:  di
        lxi     sp,STACK

        lxi     h,040h          ;pointer to logical to physical mapping
        push    psw             ;save the boot unit
        ral
        jc      cbtb            ;booted from z/h47

                                ;else booted from h17/h77/z87
        pop     psw             ;a contains boot unit
        sta     mun             ;mapping unit
        sta     ldm             ;also, last disk mounted on mapping unit
        mov     c,a             ;set up the logical to physical mapping
        call    cbtfil          ;  using the boot unit as drive 0 (==A)
        mvi     m,3             ;8 inch disks become D and E
        inx     h
        mvi     m,4

; set all unused units to imaginary

        lda     mun             ;if mapping unit
        ora     a               ; ==0
        jz      cbta1           ;  then there are no unused units
        mov     b,a             ;else,
        lxi     h,hdt0          ;  make unused (up to boot unit) imaginary
        lxi     d,24            ;offset between entries in table
cbta:   mov     a,m
        ani     0FFh-0C0h       ;make drive type imaginary
        mov     m,a
        dad     d               ;point to next entry in table
        dcr     b               ;this one is done
        jnz     cbta

cbta1:  mvi     a,DFMO          ;make the h17 motor appear on
        jmp     cbtc

cbtb:   pop     psw             ;booted from h/z47
        ani     7fh             ;this unit is 0, alias drive A
        adi     3
        mov     m,a
        inx     h               ;other drive of 47 is b
        cpi     3               ;3 if other was 4, or 4 if other was 3
        mvi     m,4
        jz      cbtb1
        mvi     m,3
cbtb1:  inx     h
        push    h

; find the first real 5.25 inch drive

        mvi     c,0
        lxi     d,24
        lxi     h,hdt0

cbtb2:  mov     a,m
        ani     0c0h
        jnz     cbtb3           ;found it
        dad     d
        inr     c
        mov     a,c
        cpi     3
        jc      cbtb2

        mvi     c,0             ;pretend unit 0 exists
cbtb3:  mov     a,c
        sta     mun             ;set this as the mapping unit
        sta     ldm             ;set this as the last disk mounted on mun
        pop     h
        call    cbtfil
        xra     a               ;make h17 motor appear off

cbtc:   sta     DEVCTL          ;contents of h17/h77/z87 control latch

        mvi     a,MIDJMP
        lxi     h,clock
        sta     CLKVEC
        shld    CLKVEC+1

        lxi     h,CTLPRT        ;get the current value of the RAM at 0 port
        mov     a,m             ;  established by BLDR
        out     H88CTL          ;reset the clock on the H/Z89

        inx     h               ;point to H8FLAG
        mov     a,m
        ora     a               ;if 0 then running on H/Z89
        jz      cbt0            ;  then don't output to 360Q
        out     H8CTL           ;  else contains H8TR to reset H8 clock

;
; initialize 8251 (only if used)
;

cbt0:   lda     mode            ;first, assume it is not used
        ani     0FEh
        sta     mode

        lda     H8FLAG          ;if on z/h89
        ora     a               ; then the console is not an H8-5
        jz      cbt1            ;console on H8-4 card

        lxi     h,h84pt1        ;point to crt port
        mov     a,m             ;get base port number
        adi     3               ;see if you can get a response from 8250 @ 350Q
        sta     outh84+1
        push    psw
        mvi     a,3             ;set 8 bit words
        call    outh84
        pop     psw
        call    pin
        cpi     3
        jz      cbt1            ;console on H8-4 card
        lda     mode
        ori     1               ;console must be on H8-5 card then
        sta     mode            ; so set mode

        mvi     a,15h           ;dummy mode byte
        out     H85CRT+1

        mvi     a,40h           ;reset 8251
        out     H85CRT+1

        mvi     a,4Eh           ;8 bit words, 1 stop bit, no parity
        out     H85CRT+1

        mvi     a,15h           ;enable Tx and Rx with interrupts off
        out     H85CRT+1

;
; now initialize the 8250s
;
cbt1:   lhld    crtbaud         ;pick up baud rate
        lda     h84pt1          ;  and the port number
        call    in8250          ;initialize this uart
;
        lhld    ttybaud
        lda     h84pt2
        call    in8250
;
        lhld    lptbaud
        lda     h84pt3
        call    in8250
;
        lhld    rdpbaud
        lda     h84pt4
        call    in8250

        lda     defiob          ;set the default IOBYTE
        sta     IOBYTE

        ei

        lxi     h,smsg0         ;print the signon message
        call    pmsg
        mvi     a,MEMT >> 8    ;find out our newly relocated size
        rar                     ;get the value in k by dividing by 4
        rar                     ;  (done at run time for relocation)
        ani     03FH
        jnz     cboot1          ;if the top of memory is not 0000h
        mvi     a,64            ; else take care of the 64k case
cboot1: call    tydn            ;type a 2 digit decimal number
        lxi     h,smsg1
        call    pmsg
        xra     a               ;make A the default drive
        sta     logdsk

        mvi     a,BTDCD         ;flag as a cold boot
        jmp     gow

smsg0:  db      CR,LF,LF,0
smsg1:  db      'K Heath/Zenith CP/M 2.2.'
        db      VERS/10+'0',(VERS # 10)+'0'
        db      CR,LF,0
;
; CBTFIL - fill the logical to physical mapping table
;

cbtfil: mvi     b,3             ;number of drives
cbtf1:  mov     a,c             ;get this drive's number
        cpi     3               ;mod 3
        jc      cbtf2
        sui     3
cbtf2:  mov     m,a             ;set it as the drive to use here
        inx     h               ;point to next entry in log->phys table
        inr     c               ;round robin to next drive
        dcr     b               ;count this one as done
        jnz     cbtf1
        ret
;
; TYDN - type a two digit decimal number on console
;       entry   a       value
;

tydn:   mvi     c,0             ;initialize quotient
tydn1:  sui     10              ;repeatedly subtract 10
        jc      tydn2           ;if underflow
        inr     c               ;else increment the quotient
        jmp     tydn1           ;and subtract again

tydn2:  adi     10              ;correct the underflow
        push    psw             ;save the remainder
        mov     a,c             ;get the quotient
        adi     030H            ;ASCII adjust it
        mov     c,a
        call    conout          ;send it to console
        pop     psw             ;recall remainder
        adi     030H            ;ASCII adjust
        mov     c,a
        jmp     conout          ;print it, with implicit return
;
; IN8250 - initialize an 8250
;    hl contains baud rate divisor (word)
;    a  has base port number
;

in8250: mov     b,a             ;save base port number in b
        xchg                    ;move baud rate divisor to de
        lxi     h,outh84+1      ;point to port in out instruction
        mvi     a,3             ;baud rate access bit on base+3 port
        add     b               ;get actual port
        mov     c,a             ;save in c for later
        mov     m,a             ;and modify output instruction
        mvi     a,83H           ;set divisor latch access bit
        call    outh84          ;to a "1"
        inr     m               ;point to modem control register
        mvi     a,03H           ;and set DSR & CTS high for diablo
        call    outh84          ; and other terminals which require them
        mov     m,b             ;set port to least sig byte
        mov     a,e
        call    outh84
        mov     a,d             ;now do most sig byte
        ani     0FH             ;and off control flags
        inr     m               ;on next port
        call    outh84
        mov     m,c             ;reset port to divisor latch access
        cpi     B110 >> 8      ;if set for greater than 110
        mvi     a,3             ; then set no parity, 8 bit words, 1 stop bit
        jc      in821
        ori     4               ; else set two stop bits for 110 and below
in821:  call    outh84
        dcr     m               ;now set port for interupt control
        dcr     m
        xra     a               ;disable all device interrupts
        call    outh84          ;disable ints

; delay for approximately two character times

        xchg                    ;put baud rate divisor in hl
        dad     h               ;multiply by 16 to get delay
        dad     h
        dad     h
        dad     h
loop1:  dcx     h
        mov     a,l
        ora     h
        jnz     loop1
        ret

; self modifying out instruction used by in8250

outh84: out     0               ;port is modified
        ret

clen    equ     $-hstbuf        ;cold boot code length
        if      (clen-hstsiz) >> 15    ;if clen smaller than host sector size
        ds      hstsiz-clen             ;  fill out hstbuf with ds
        endif
;
dirbuf: ds      128
alv0:   ds      12
csv0:   ds      16
alv1:   ds      12
csv1:   ds      16
alv2:   ds      12
csv2:   ds      16
alv3:   ds      77
csv3:   ds      64
alv4:   ds      77
csv4:   ds      64
;
hsav:   ds      2               ;saved hl during clock interrupt service
retsav: ds      2               ;saved return address during clock ticks

dpbx:   ds      2
hstdpb: ds      2

dmab:   ds      2               ;dma buffer - used to store starting address
                                ;  of track during warm boot
spt:    ds      1               ;number of sectors per track (during wboot)
xltw:   ds      2               ;sector xlate table (during wboot)
spt1:   ds      1               ;ditto, beyond track 0
xltw1:  ds      2               ;ditto, beyond track 0

unit:   ds      1               ;h17    unit
track:  ds      1               ;       track
sector: ds      1               ;       sector
side:   ds      1
lun:    ds      1
lsp:    ds      1               ;logical sectors per physical
errcnt: ds      1               ;retry counter
errtyp: ds      1               ;type of error
trkpt:  ds      2               ;contains pointer to track register
                                ;for current drive
;
sekdsk: ds      1               ;seek disk number
sektrk: ds      2               ;seek track number
seksec: ds      1               ;seek sector number
;
hstdsk: ds      1               ;host disk number
hsttrk: ds      2               ;host track number
hstsec: ds      1               ;host sector number
;
sekhst: ds      1               ;seek shr secshf
hstact: ds      1               ;host active flag
hstwrt: ds      1               ;host written flag
;
unacnt: ds      1               ;unalloc rec cnt
unadsk: ds      1               ;last unalloc disk
unatrk: ds      2               ;last unalloc track
unasi:  ds      1               ;last unalloc sector index into xlt table
;
erflag: ds      1               ;error reporting
rsflag: ds      1               ;read sector flag
readop: ds      1               ;1 if read operation
wrtype: ds      1               ;write operation type
dmaadr: ds      2               ;last dma address

BIOSEND EQU     $
        end
