;;      EDRAM - DISK RAM WORAREA DEFINITION.
;
;       ZEROED UPON BOOTING UP.
;
;       HOSEQU MUST BE CHANGED WHEN THIS DECK IS CHANGED.

        ORG     D.RAM

D.TT    DS      1               ; TARGET TRACK (CURRENT OPERATION)
D.TS    DS      1               ; TARGET SECTOR (CURRENT OPERATION)

D.DVCTL DS      1               ; DEVICE CONTROL BYTE

D.DLYMO DS      1               ; MOTOR ON DELAY COUNT
D.DLYMS DS      1               ; HEAD SETTLE DELAY COUNTER

D.TRKPT DS      2               ; ADDRESS IN D.DRVTB FOR TRACK NUMBER
D.VOLPT DS      2               ; ADDRESS IN D.DRVTB FOR VOLUME NUMBER

D.DRVTB DS      2*4             ; TRACK NUMBER AND VOLUME NUMBER FOR 4 DRIVES

D.HECNT DS      1               ; HARD ERROR COUNT
D.SECNT DS      2               ; SOFT ERROR COUNT
D.OECTN DS      1               ; OPERATION ERROR COUNT

;       GLOBAL DISK ERROR COUNTERS

D.ERR                           ; BEGINNING OF ERROR BLOCK
D.E.MDS DS     1                ; MISSING DATA SYNC
D.E.HSY DS     1                ; MISSING HEADER SYNC
D.E.CHK DS     1                ; DATA CHECKSUM
D.E.HCK DS     1                ; HEADER CHECKSUM
D.E.VOL DS     1                ; WRONG VOLUME NUMBER
D.E.TRK DS     1                ; BAD TRACK SEEK
D.ERRL                          ; LIST OF ERROR COUNTERS

;       I/O OPERATION COUNTS

D.OPR   DS  2
D.OPW   DS  2

D.RAML  EQU $-D.RAM
