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
;   REGISTERS: R
;   Exit: X
;   HELP: ?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Constants

prompt equ '?'          ; prompt character

        cpu     8080

        org     0000H   ; Use 0100H if you want to run under CP/M

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Main program entry point
 
start:

; Initialize:
;   set up stack pointer
;   initialize any variables
;

        call    ClearScreen       ; Clear screen

        lxi     h,strStartup    ; Print startup message
        call    PrintString


        mvi     a,prompt        ; Display command prompt
        call    PrintChar
loop:
        call    GetChar       ; Get a character
        call    PrintChar     ; Echo it back
        cpi     'X'           ; Is it X?
        jnz     loop         ; If not, repeat
        ret                   ; Otherwise return

; while true:
;   print command prompt
;   get command (letter)
;   case D:
;     call dump
;   case G:
;     call go
;   case L:
;     call clear screen
;   case R:
;     call registers
;   case X:
;     call exit
;   case ?:
;     call print help
;   default:
;     print error message
; end while

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
        push    psw     ; Save A register
loop1:  in      SREG    ; Read status register
        rrc             ; Move TDRE to carry bit
        rrc
        jnc     loop1   ; Repeat until TDRE is set
        pop     psw     ; Restore A
        out     DREG    ; Output it to data register
        ret             ; And return

; GetChar
; Read character from console and return in A register. The character
; is not echoed. Waits for character to be entered.
; Registers affected: A.

GetChar:
        in      SREG    ; Read status register
        rrc             ; Move RDRF to carry bit
        jnc     GetChar ; Repeat until RDRF is set
        in      DREG    ; Read character from data register
        ret             ; And return

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

; Strings

strStartup:
        db      "JMON Monitor 0.1 by Jeff Tranter\r\n",0

strInvalid:
        db      "Invalid command",0

strHelp:
        db      "Valid commands:\r\n"
        db      "D <address>      Dump memory\r\n"
        db      "G <address>      Go\r\n"
        db      "L                Clear screen\r\n"
        db      "R                Show registers\r\n"
        db      "X                Exit\r\n"
        db      "?                Help\r\n",0

strClearScreen:
        db      "\e[2J",0       ; VT100/ANSI clear screen code

        end
