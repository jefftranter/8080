;       HDOS SYSTEM EQUIVALENCES.
;

S.GRT   EQU     12000Q          ; SYSTEM AREA FOR GRT0
S.GRT1  EQU     12400Q          ; SYSTEM AREA FOR GRT 1
SECSCR  EQU     13000Q          ; SYSTEM 512 BYTE SCRATCH AREA
ROMBOOT EQU     14000Q          ; ROM BOOT ENTRY


        ORG     20100Q          ; FREE SPACE FROM PAM-8

        DS      8               ; JUMP TO SYSTEM EXIT
D.CON   DS      16              ; DISK CONSTANTS
SYDD                            ; SYSTEM DISK ENTRY POINT
D.VEC   DS      24*3            ; SYSTEM ROM ENTRY VECTORS
D.RAM   DS      31              ; SYSTEM ROM WORK AREA
S.VAL   DS      36              ; SYSTEM VALUES
S.INT   DS      115             ; SYSTEM INTERNAL WORK AREAS
        DS      16
S.OVR   DS      2               ; STACK OVERFLOW WARNING
        DS      280             ; SYSTEM STACK
STACKL  EQU     432Q            ; STACK SIZE

STACK                           ; LWA+1 SYSTEM STACK
USERFWA                         ; USER FWA
