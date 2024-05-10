;       HDOS SYSTEM EQUIVALENCES.
;

S.GRT   EQU     12000Q          ; SYSTEM AREA FOR GRT0
S.GRT1  EQU     12400Q          ; SYSTEM AREA FOR GRT 1
S.GRT2  EQU     13000Q          ; SYSTEM AREA FOR GRT 2

ROMBOOT EQU     14000Q          ; ROM BOOT ENTRY


        ORG     20100Q          ; FREE SPACE FROM PAM-8

        DS      8               ; JUMP TO SYSTEM EXIT
D.CON   DS      16              ; DISK CONSTANTS
SYDD    EQU     $               ; SYSTEM DISK ENTRY POINT
D.VEC   DS      24*3            ; SYSTEM ROM ENTRY VECTORS
D.RAM   DS      31              ; SYSTEM ROM WORK AREA
S.VAL   DS      36              ; SYSTEM VALUES
S.INT   DS      115             ; SYSTEM INTERNAL WORK AREAS
        DS      16
S.OVR   DS      2               ; STACK OVERFLOW WARNING
        DS      280             ; SYSTEM STACK
STACKL  EQU     432Q            ; STACK SIZE

STACK   EQU     $               ; LWA+1 SYSTEM STACK
USERFWA EQU     $               ; USER FWA
