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
; 
; DUMP: D <START>
; GO: G <ADDRESS>
; CLR SCREEN: L
; REGISTERS: R
; Exit: X
; HELP: ?
; 
; JMON Monitor 0.99 by Jeff Tranter
; ? 
; 
; Pseudocode:
; 
; call Initialize
; call ClearScreen
; Print startup message
; 
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
; 
; Initialize:
;   set up stack pointer
;   initialize any variables
; 
; PrintChar
; 
; GetChar
; 
; PrintString
; 
; ClearScreen
; 
; GetAddress
;

        cpu     8080
        org     0100H

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

; Main program
;
; Display a '>" prompt, then get characters from the console and echo
; them back. Return if an uppercase X is typed.

START:
        mvi     a,'>'   ; Display prompt character
loop2:
        call    CONOUT  ; Get a character
        call    CONIN   ; Echo it back
        cpi     'X'     ; Is it X?
        jnz     loop2   ; If not, repeat
        ret             ; Otherwise return

; CONOUT
; Output character in A register to console.
; Registers affected: none.

CONOUT: push    psw     ; Save A register
loop1:  in      SREG    ; Read status register
        rrc             ; Move TDRE to carry bit
        rrc
        jnc     loop1   ; Repeat until TDRE is set
        pop     psw     ; Restore A
        out     DREG    ; Output it to data register
        ret             ; And return

; CONIN
; Read character from console and return in A register. The character
; is not echoed. Waits for character to be entered.
; Registers affected: A.

CONIN:  in      SREG    ; Read status register
        rrc             ; Move RDRF to carry bit
        jnc     CONIN   ; Repeat until RDRF is set
        in      DREG    ; Read character from data register
        ret             ; And return

        end
