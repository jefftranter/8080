;;	HDOS SYSTEM EQUIVALENCES.
;

S.GRT   EQU     12000Q          ; SYSTEM AREA FOR GRT0
S.GRT1  EQU     12400Q          ; SYSTEM AREAS FOR GRT 1
SECSCR  EQU     13000Q          ; SYSTEM 512 BYTE SCRATCH AREA
ROMBOOT EQU     14000Q          ; ROM BOOT ENTRY

        ORG     020100Q         ; FREE SPACE FROM PAM-8

        DS      8
D.CON   DS      16              ; DISK CONSTANTS        
SYDD    EQU     $               ; SYSTEM DISK ENTRY POINT
D.VEC   DS      24*3            ; SYSTEM ROM ENTRY VECTORS
D.RAM   DS      31              ; SYSTEM ROM WORK AREA
S.VAL   DS      38              ; SYSTEM VALUES
S.INT   DS      113             ; SYSTEM INTERNAL WORK AREAS
        DS      16
S.SOVR  DS      2               ; STACK OVERFLOW WARNING
        DS      21200Q-$        ; SYSTEM STACK

STACK   EQU     $               ; LWA+1 SYSTEM STACK
USERFWA EQU     $               ; USER FWA
