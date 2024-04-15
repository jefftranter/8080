; Source code for "Computing Test" program in H88 Operation Manual.
;
; It can be entered as octal numbers from the ROM monitor using the
; S(ubstitute) command. It starts at (and begins execution at) address
; 040100.
;
; When run, it continuously prints a series of ASCII characters on the
; screen. It can be stopped by pressing <SHIFT><RESET>.


WCC     EQU     1702Q           ; WRITE CHAR TO CONSOLE

        ORG     20100Q          ; 040.100 in split octal

START   MVI     A,9
LOOP    CALL    WCC
        INR     A
        CPI     155Q
        JZ      START
        JMP     LOOP

        END
