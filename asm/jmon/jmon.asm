;
; JMON - Jeff's Monitor Program
; ------------------------------
; 
; A machine language monitor program for the Briel Altair 8800.
; Inspired by JMON for the Apple Replica 1 and 6502 processor.
; I wrote this mostly as an exercise to learn 8080 assembly language.
; 
; Copyright (C) 2014 by Jeff Tranter <tranter@pobox.com>
; 
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
; 
;   http://www.apache.org/licenses/LICENSE-2.0
; 
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
; 
; Commands:
;   DUMP: D <START>
;   GO: G <ADDRESS>
;   CLR SCREEN: L
;   INFO: N
;   REGISTERS: R
;   HELP: ?
;
; Possible future commands:
;
; COPY: C <START> <END> <DEST>
; FILL: F <START> <END> <DATA>...
; CHECKSUM: K <START> <END>
; SEARCH: S <START> <END> <DATA>...
; TEST: T <START> <END>
; VERIFY: V <START> <END> <DEST>
; WRITE: : <ADDRESS> <DATA>...
; MATH: = <ADDRESS> +/- <ADDRESS>

        cpu     8080
        org     0000H   ; Use 0100H if you want to run under CP/M

; Constants

prompt  equ '?'          ; Prompt character
CR      equ '\r'         ; Carriage Return
NL      equ '\n'         ; Newline
stack   equ 7000h        ; Starting address for stack

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Main program entry point
 
start:
        lxi     sp,stack        ; Set up stack pointer
        call    ClearScreen     ; Clear screen
        lxi     h,strStartup    ; Print startup message
        call    PrintString

mainloop:
        mvi     a,prompt        ; Display command prompt
        call    PrintChar

        call    GetChar         ; Get a command (letter)
        call    ToUpper         ; Convert to upper case

        cpi     'D'             ; DUMP command?
        jnz     tryG
        call    DumpCommand
        jmp     mainloop

tryG:
        cpi     'G'             ; GO command?
        jnz     tryL
        call    GoCommand
        jmp     mainloop

tryL:
        cpi     'L'             ; CLEAR command?
        jnz     tryN
        call    ClearCommand
        jmp     mainloop

tryN:   cpi     'N'             ; INFO command?
        jnz     tryR
        call    InfoCommand
        jmp     mainLoop

tryR:
        cpi     'R'             ; REGISTERS command?
        jnz     tryHelp
        call    RegistersCommand
        jmp     mainloop

tryHelp:
        cpi     '?'             ; HELP command?
        jnz     invalid
        call    HelpCommand
        jmp     mainloop

invalid:
        call    PrintCR
        lxi     h,strInvalid    ; print error message
        call    PrintString

        jmp   mainloop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Commands

DumpCommand:
        call    PrintChar       ; Echo command back
        call    PrintCR
        ret

GoCommand:
        call    PrintChar       ; Echo command back
        call    PrintCR
        ret

; CLEAR command.
; Sends code to clear terminal screen.

ClearCommand:
        call    PrintChar       ; Echo command back
        call    ClearScreen     ; Clear screen
        ret


; INFO command.
InfoCommand:
        call    PrintChar       ; Echo command back
        call    PrintCR
        lxi     h,strStartup    ; Print startup message
        call    PrintString

; Detect CPU type. 8080 flags have XX0X0X1X. If different, then assume Z80.

        lxi     h,strCpuType
        call    PrintString

        push    psw             ; Push A and flags
        pop     b               ; Pop B and C, flags now in C
        mov     a,c             ; Move flags to A
        ani     00101010b       ; Mask out bit we don't care about
        cpi     00000010b       ; Should have this value for 8080
        jnz     z80             ; If not, assume Z80 CPU
        lxi     h,str8080       ; It is an 8080
prnt:   call    PrintString     ; Print CPU type
        call    PrintCR
        ret                     ; Return
z80:    
        lxi     h,strZ80        ; It is a Z80
        jmp     prnt


; REGISTERS command.
; Example output:
; A=01 BC=4E56 DE=0000 HL=021C .Z-.-P-C SP=6FFE PC=00C3

RegistersCommand:
        call    PrintChar       ; Echo command back
        call    PrintCR
        ret


; HELP command.
; Displays list of valid commands.

HelpCommand:
        call    PrintChar       ; Echo command back
        lxi     h,strHelp
        call    PrintString
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Utility Routines

; The Briel Altair 8800 emulates a MITS 88-2SIO serial interface to the
; console. The hardware interface is as follows:
; 
; I/O Port  Read             Write
; --------  ---------------  ----------------
; 10h       Status Register  Control Register
; 11h       Input Data       Output Data
; 
; The control register can set parameters such as bit rate. It does not
; need to be initialized on the Briel Altair.
; 
; The status register has a number of bits. Not all are emulated. The
; following are the ones of interest for console serial input/output:
; 
; Bit Function
; 
; 0   Receive Data Register Full (RDRF) . Set to 1 when data is ready to
;     be read. Reading the input data register clears it.
; 
; 1   Transmit Data Register Empty (TDRE). Set to 1 when output data has
;     been transferred out and new data may be entered.
; 
; References:
; 1. MITS 88-2 Serial Input/Output Board Theory of Operation
; 

SREG    port    10h
CREG    port    10h
DREG    port    11h

; PrintChar
; Output character in A register to console.
; Registers affected: none.

PrintChar:
        push    psw             ; Save A register
loop1:  in      SREG            ; Read status register
        rrc                     ; Move TDRE to carry bit
        rrc
        jnc     loop1           ; Repeat until TDRE is set
        pop     psw             ; Restore A
        out     DREG            ; Output it to data register
        ret                     ; And return

; GetChar
; Read character from console and return in A register. The character
; is not echoed. Waits for character to be entered.
; Registers affected: A.

GetChar:
        in      SREG            ; Read status register
        rrc                     ; Move RDRF to carry bit
        jnc     GetChar         ; Repeat until RDRF is set
        in      DREG            ; Read character from data register
        ret                     ; And return

; PrintCR
; Print carriage return/newline.
; Registers affected: none

PrintCR:
        push    psw             ; Save A reg
        mvi     a,CR            ; Carriage Return character
        call    PrintChar       
        mvi     a,NL            ; Newline character
        call    PrintChar
        pop     psw             ; Restore A reg
        ret

; ClearScreen
; Clear screen. Assumes an VT100/ANSI terminal.
; Registers affected: HL, A.

ClearScreen:
        lxi     h,strClearScreen
        call    PrintString
        ret

; PrintString
; Print string pointed to by HL until null found.
; Registers affected: HL

PrintString:
        push    psw             ; Save A register
nextch: mov     a,m             ; Get a character
        cpi     0               ; Is it a null?
        jz      eos             ; If so, exit
        call    PrintChar       ; Print the character
        inx     h               ; Advance pointer to next character
        jmp     nextch          ; And repeat
eos:    pop     psw             ; Restore A register
        ret                     ; Return

; PrintSpace
; Print space character.
; Registers affected: none
PrintSpace:
        push    psw             ; Save A reg
        mvi     a,' '           ; Space character
        call    PrintChar       
        pop     psw             ; Restore A reg
        ret

; ToUpper
; Convert character in A to uppercase if it is a letter.
; Registers affected: A
ToUpper:
        cpi     'a'             ; Less than 'a' ?
        jc      notUpper        ; If so, branch
        cpi     'z'+1           ; Greater than 'z'?
        jnc     notUpper
        ani     11011111b       ; Convert to upper case
notUpper:
        ret

PrintByte:

PrintAddress:

GetHex:

GetByte:

GetAddress:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Strings

strStartup:
        db      "JMON Monitor 0.1 by Jeff Tranter\r\n",0

strInvalid:
        db      "Invalid command. Type ? for help.",0

strHelp:
        db      "\r\n"
        db      "Valid commands:\r\n"
        db      "D <address>      Dump memory\r\n"
        db      "G <address>      Go\r\n"
        db      "L                Clear screen\r\n"
        db      "N                Show Info\r\n"
        db      "R                Show registers\r\n"
        db      "?                Help\r\n",0

strClearScreen:
        db      "\e[2J\e[H",0       ; VT100/ANSI clear screen, cursor home

strCpuType:
        db      "CPU type: ",0
str8080:
        db      "8080",0
strZ80:
        db      "Z80",0

        end
