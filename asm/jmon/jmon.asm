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
;
; To Do:
; Implement DUMP
; Implement GO
; Allow changing registers
; Implement other commands

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

; Save registers on entry (for later use by commands like REGISTERS and GO)
        sta     save_a
        mov     a,b
        sta     save_b
        mov     a,c
        sta     save_c
        mov     a,d
        sta     save_d
        mov     a,e
        sta     save_e
        mov     a,h
        sta     save_h
        mov     a,l
        sta     save_l
        push    psw             ; Push A and Flags
        pop     b               ; Pull A and flags to B,C
        mov     a,c             ; Put flags in A
        sta     save_f          ; Save flags
        mvi     a,0             ; Store zero as initial value of PC
        sta     save_pc
        sta     save_pc+1
        lxi     h,stack         ; Set initial value of SP
        mov     a,h
        sta     save_sp
        mov     a,l
        sta     save_sp+1

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
        call    PrintSpace
        call    GetHex          ; Prompt for address
        call    PrintSpace
        call    PrintByte
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
; A=01 BC=4E56 DE=0000 HL=021C F=10101011 SP=6FFE PC=00C3

RegistersCommand:
        call    PrintChar       ; Echo command back
        call    PrintCR
        mvi     a,'A'
        call    PrintChar
        call    PrintEquals
        lda     save_a
        call    PrintByte
        call    PrintSpace
        mvi     a,'B'
        call    PrintChar
        mvi     a,'C'
        call    PrintChar
        call    PrintEquals
        lda     save_b
        call    PrintByte
        lda     save_c
        call    PrintByte
        call    PrintSpace
        mvi     a,'D'
        call    PrintChar
        mvi     a,'E'
        call    PrintChar
        call    PrintEquals
        lda     save_d
        call    PrintByte
        lda     save_e
        call    PrintByte
        call    PrintSpace
        mvi     a,'H'
        call    PrintChar
        mvi     a,'L'
        call    PrintChar
        call    PrintEquals
        lda     save_h
        call    PrintByte
        lda     save_l
        call    PrintByte
        call    PrintSpace

; print flags in binary

        mvi     a,'F'
        call    PrintChar
        call    PrintEquals
        lda     save_f          ; Get flags
        mvi     l,8             ; Want to test 8 bits
nextbit:
        ral                     ; Rotate into carry bit
        cc      PrintOne        ; Print "1" if set
        cnc     PrintZero       ; Print "0" if cleared
        dcr     l               ; Decrement counter
        jnz     nextbit         ; Repeat until all bits done

        call    PrintSpace
        mvi     a,'S'
        call    PrintChar
        mvi     a,'P'
        call    PrintChar
        call    PrintEquals
        lda     save_sp
        call    PrintByte
        lda     save_sp+1
        call    PrintByte
        call    PrintSpace
        mvi     a,'P'
        call    PrintChar
        mvi     a,'C'
        call    PrintChar
        call    PrintEquals
        lda     save_pc
        call    PrintByte
        lda     save_pc+1
        call    PrintByte
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
; 0   Receive Data Register Full (RDRF). Set to 1 when data is ready to
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
        ani     02h             ; Mask out TDRE bit
        jz      loop1           ; Repeat until TDRE is set
        call    Delay           ; Delay seems to be needed to avoid dropped characters
        pop     psw             ; Restore A
        out     DREG            ; Output it to data register
        ret                     ; And return

; GetChar
; Read character from console and return in A register. The character
; is not echoed. Waits for character to be entered.
; Registers affected: A.

GetChar:
        in      SREG            ; Read status register
        ani     01H             ; Mask out RDRF bit
        jz      GetChar         ; Repeat until RDRF is set
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

; PrintEquals
; Print equals sign.
; Registers affected: none

PrintEquals:
        push    psw             ; Save A reg
        mvi     a,'='           ; Equals character
        call    PrintChar       
        pop     psw             ; Restore A reg
        ret

; Print "0"
; Registers affected: none
PrintZero:
        push    psw
        mvi     a,'0'
        call    PrintChar
        pop     psw
        ret


; Print "1"
; Registers affected: none
PrintOne:
        push    psw
        mvi     a,'1'
        call    PrintChar
        pop     psw
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

; PrintByte
; Print 8-bit value in A as two ASCII hex characters
; Registers affected: A
PrintByte:
        push    b               ; Save B reg
        call    bhconv          ; Convert to two hex digits
        mov     a,b             ; Get first digit
        call    PrintChar       ; Print it
        mov     a,c             ; Get second digit
        call    PrintChar       ; Print it
        pop     b               ; Restore B reg
        ret                     ; Return

; Convert byte in A to two hex ASCII digits and return in B,C.
bhconv:
        push    h               ; Save HL
        mov     l,a             ; Save original byte
        rar                     ; Shift upper nybble into lower nybble
        rar
        rar
        rar
        call    bin1            ; Convert digit to ASCII
        mov     b,a             ; Put it in B
        mov     a,l             ; Get original byte
        call    bin1            ; Convert digit to ASCII
        mov     c,a             ; Put it in C
        pop     h               ; Restore H
        ret                     ; Return

; Convert bottom nybble of byte on A to ASCII
bin1:
        ani     0Fh             ; Mask out lower nybble
        adi     '0'             ; Convert to ASCII digit, e.g. 0->'0'
        cpi     '9'+1           ; Is it greater than '9'?
        rc                      ; If not, we are done
        adi     'A'-'9'-1       ; Add offset to convert to hex letter A-F
        ret                     ; Return


; Delay
; Delay of a few 10s of microseconds. Seems to be needed during
; serial out to avoid dropped characters. Value determined
; experimentally.
; Registers affected: none

Delay:
        push    psw             ; Save A
        mvi     a,10            ; Delay constant
decr:   dcr     a               ; Decrement counter
        jnz     decr            ; Repeat until A reaches zero
        pop     psw             ; Restore A
        ret                     ; Return


PrintAddress:

; GetHex
; Gets a single hex digit from the keyboard, 0-9 or A-F or a-f.
; Ignores invalid characters. <Esc> cancels and sets carry bit.
; Returns binary nybble in A.
; Registers affected: A

GetHex:
        call    GetChar         ; Get a character
        cpi     '\e'            ; Is it <Escape> ?
        jnz     next            ; Branch if not
        sub     a               ; Set A to zero
        stc                     ; Otherwise set carry and return.
        ret
next:   cpi     '0'             ; Less than '0'?
        jc      GetHex          ; Yes, ignore and try again
        cpi     '9'+1           ; Greater than 9?
        jc      validDigit      ; Branch if not (is 0-9)
        cpi     'A'             ; Less than 'A'?
        jc      GetHex          ; Yes, ignore and try again
        cpi     'F'+1           ; Greater than 'F'?
        jc      validULetter    ; Branch if not (is A-F)
        cpi     'a'             ; less that 'a'?
        jc      GetHex          ; Yes, ignore and try again
        cpi     'f'+1           ; Greater than 'f'?
        jc      validLLetter    ; Branch if not (is a-f)
        jmp     GetHex          ; Invalid, try again
validDigit:
        call    PrintChar       ; Echo the character
        sui     '0'             ; Convert digit to binary
        jmp     done
validLLetter:
        ani     11011111b       ; Convert to lowercase letter to upper case
validULetter:
        call    PrintChar       ; Echo the character
        sui     'A'-10          ; Convert uppercase letter to binary
        jmp     done
done:   stc                     ; Weird 8080 way to clear carry
        cmc                     ; Set it and then complement it
        ret


; GetByte
; Gets a two character hex number from the keyboard.
; Ignores invalid characters. <Esc> cancels and sets carry bit.
; Returns binary byte in A.
; Registers affected: A

GetByte:


; GetAddress
; Gets a four character hex number from the keyboard.
; Ignores invalid characters. <Esc> cancels and sets carry bit.
; Returns binary word in HL.
; Registers affected: HL

GetAddress:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Strings

strStartup:
        db      "JMON Monitor 0.1 by Jeff Tranter\r\n",0

strInvalid:
        db      "Invalid command. Type ? for help.\r\n",0

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Variables

save_a  db      ?               ; Saved values of registers
save_f  db      ?
save_b  db      ?
save_c  db      ?
save_d  db      ?
save_e  db      ?
save_h  db      ?
save_l  db      ?
save_sp dw      ?
save_pc dw      ?

        end
