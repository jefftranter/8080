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
;   COPY: C <START> <END> <DEST>
;   DUMP: D <START>
;   FILL: F <START> <END> <DATA>...
;   GO: G <ADDRESS>
;   INFO: I
;   CHECKSUM: K <START> <END>
;   CLR SCREEN: L
;   REGISTERS: R
;   SEARCH: S <START> <END> <DATA>...
;   TEST: T <START> <END>
;   VERIFY: V <START> <END> <DEST>
;   WRITE: : <ADDRESS> <DATA>...
;   MATH: = <ADDRESS> +/- <ADDRESS>
;   HELP: ?
;
; Revision History
; Version Date         Comments
; 0.1     27-Mar-2014  First version started.
; 0.2     30-Mar-2014  Implemented a few commands.

; To Do:
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
 
Start:

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
        call    PrintSpace

        call    GetChar         ; Get a command (letter)
        call    ToUpper         ; Convert to upper case

        cpi     'C'
        jnz     tryD
        call    CopyCommand
        jmp     mainloop
tryD:
        cpi     'D'
        jnz     tryF
        call    DumpCommand
        jmp     mainloop
tryF:
        cpi     'F'
        jnz     tryG
        call    FillCommand
        jmp     mainloop
tryG:
        cpi     'G'
        jnz     tryI
        call    GoCommand
        jmp     mainloop
tryI:
        cpi     'I'
        jnz     tryK
        call    InfoCommand
        jmp     mainloop
tryK:
        cpi     'K'
        jnz     tryL
        call    ChecksumCommand
        jmp     mainloop
tryL:
        cpi     'L'
        jnz     tryR
        call    ClearCommand
        jmp     mainloop
tryR:
        cpi     'R'
        jnz     tryS
        call    RegistersCommand
        jmp     mainloop
tryS:
        cpi     'S'
        jnz     tryT
        call    SearchCommand
        jmp     mainloop
tryT:
        cpi     'T'
        jnz     tryV
        call    TestCommand
        jmp     mainloop
tryV:
        cpi     'V'
        jnz     tryColon
        call    VerifyCommand
        jmp     mainloop
tryColon:
        cpi     ':'
        jnz     tryEquals
        call    MemoryCommand
        jmp     mainloop
tryEquals:
        cpi     '='
        jnz     tryHelp
        call    MathCommand
        jmp     mainloop
tryHelp:
        cpi     '?'
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


; Dump. Dumps memory in hex and ascii, as below:
;
; 0000: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................
;
; Prompts to continue after a page of lines dumped.

BYTES equ 16                    ; Number of bytes to dump per line
LINES equ 24                    ; Number of lines to dump per page

DumpCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for address
        jnc     startScreen     ; Carry set indicates <ESC> pressed
        call    PrintCR
        ret
startScreen:
        call    PrintCR
        mvi     c,LINES         ; Counts number of lines to be displayed
startLine:
        push    h               ; Save address in HL
        call    PrintAddress    ; Print address
        mvi     a,':'           ; Print colon
        call    PrintChar
        mvi     b,BYTES         ; Counts number of bytes to be displayed
doline:
        call    PrintSpace      ; Print space
        mov     a,m             ; Get data at current address
        call    PrintByte       ; Print it
        inx     h               ; Increment current address
        dcr     b               ; Decrement byte count
        jnz     doline          ; Continue until full line displayed

; Now dump line of data in ASCII

        call    PrintSpace      ; Print space
        pop     h               ; Get start address of line
        mvi     b,BYTES         ; Counts number of bytes to be displayed
doAscii:
        mov     a,m             ; Get data at current address
        call    PrintAscii      ; Print it
        inx     h               ; Increment current address
        dcr     b               ; Decrement byte count
        jnz     doAscii         ; Continue until full line displayed

        call    PrintCR
        dcr     c               ; Decrement count of lines printed
        jnz     startLine       ; Do the next line
        push    h               ; Save HL
        lxi     h,strContinue   ; Prompt whether to continue
        call    PrintString
        pop     h               ; Restore HL
cont:   call    GetChar         ; Get key
        cpi     '\e'            ; Escape?
        jnz     trySpace        
        call    PrintCR         ; If so, return
        ret
trySpace:
        cpi     ' '             ; Space?
        jz      startScreen     ; If so, do next screen
        jmp     cont            ; Invalid key, try again


; Go.
; Prompts user for address, restores saved registers, and transfers
; control to address.

GoCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for address
        jnc     contgo          ; Carry set indicates <ESC> pressed
        call    PrintCR
        ret
contgo:
        shld    save_pc         ; Save it
        call    PrintCR
                                ; Restore saved registers
        lhld    Start           ; Push start address of JMON on stack so that if
        push    h               ; called code returns, will go back to monitor.
        lda     save_pc
        mov     l,a
        lda     save_pc+1
        mov     h,a
        push    h               ; push go address so we can use ret to go to it
                                ; TODO: Restore stack pointer?
        lda     save_h
        mov     h,a
        lda     save_l
        mov     l,a
        lda     save_d
        mov     d,a
        lda     save_e
        mov     e,a
        lda     save_b
        mov     b,a
        lda     save_c
        mov     c,a
                                ; TODO: Restore flags?
        lda     save_a
        ret                     ; This jumps to go address


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
; TODO: Print flags symbolically

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
        lda     save_pc+1
        call    PrintByte
        lda     save_pc
        call    PrintByte
        call    PrintCR
; TODO: Add support for editing registers
        ret


; HELP command.
; Displays list of valid commands.

HelpCommand:
        call    PrintChar       ; Echo command back
        lxi     h,strHelp
        call    PrintString
        ret


; Fill command.
; Fill memory with bytes over a range of addresses.
; TODO: Check that start < end.
FillCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for start address
        jc      finish          ; Carry set indicates <ESC> pressed
        xchg                    ; Put HL (start address) in DE
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jc      finish          ; Carry set indicates <ESC> pressed
        call    PrintSpace
        xchg                    ; Put HL (end address) in DE, start address goes back in HL
        call    GetByte         ; Prompt for fill byte
        jc      finish          ; Carry set indicates <ESC> pressed
        mov     b,a             ; Store fill byte in B
fill:
        mov     m,b             ; Fill address with byte
        inx     h               ; Increment current address in HL
        mov     a,h             ; Get H
        cmp     d               ; Compare to D
        jnz     fill            ; If no match, continue filling
        mov     a,l             ; Get L
        cmp     e               ; Compare to E
        jnz     fill            ; If no match, continue filling
        mov     m,b             ; We are at last address, write byte to it
finish:
        call    PrintCR
        ret


; Copy Command
; Copy a block of memory from one location to another.
; TODO: Change command options to C <start> <end> <dest>
; TODO: Try to minimize copying between registers and memory
; TODO: Check that start < end and handle overlap.

CopyCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for start address
        jc      finish          ; Carry set indicates <ESC> pressed
        mov     a,l             ; Save source address in src (low,high)
        sta     src
        mov     a,h
        sta     src+1
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jc      finish          ; Carry set indicates <ESC> pressed
        mov     a,l             ; Save destination address in dst (low,high)
        sta     dst
        mov     a,h
        sta     dst+1
        call    PrintSpace
        call    GetAddress      ; Prompt for number of bytes
        jc      finish          ; Carry set indicates <ESC> pressed
        mov     a,l             ; Save length in size (low,high)
        sta     size
        mov     a,h
        sta     size+1
        lda     size            ; Put size in BC
        mov     c,a
        lda     size+1
        mov     b,a
        lda     dst             ; Put destination in HL
        mov     l,a
        lda     dst+1
        mov     h,a
        lda     src             ; Put source in DE
        mov     e,a
        lda     src+1
        mov     d,a
copy:   mov     a,b             ; Get B (remaining bytes)
        ora     c               ; Also get C
        jz      finish          ; If BC is zero, we are done, so return
        ldax    d               ; Get byte from source address (DE)
        mov     m,a             ; Store byte in destination address (HL)
        inx     d               ; Increment source address
        inx     h               ; Increment destination address
        dcx     b               ; Decrement count of bytes
        jmp     copy            ; Repeat


; Checksum Command
; Calculate 16-bit checksum of a block of memory.

ChecksumCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for start address
        jc      finish          ; Carry set indicates <ESC> pressed
        xchg                    ; Swap HL and DE (put start in DE)
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jc      finish          ; Carry set indicates <ESC> pressed
        xchg                    ; Swap HL and DE
                                ; HL holds start/current address
                                ; DE holds end address
                                ; BC will hold checksum
        lxi     b,0000h         ; Clear checksum total
checkloop:
        stc                     ; Clear carry
        cmc
        mov     a,c             ; Get LSB of checksum
        adc     m               ; Add next date byte of memory
        mov     c,a             ; Store LSB of checksum
        mov     a,b             ; Get MSB of checksum
        aci     0               ; Add possible carry from LSB
        mov     b,a             ; Store MSB of checksum
        mov     a,h             ; See if MSB of pointer has reached end address yet
        cmp     d               ; e.g. H = D
        jnz     inc
        mov     a,l             ; See if MSB of pointer has reached end address yet
        cmp     e               ; e.g. L = E
        jnz     inc
        call    PrintSpace      ; Done, print checksum value
        mov     h,b             ; Put value in HL
        mov     l,c
        call    PrintAddress
        call    PrintCR
        ret
inc:    inx     h               ; Increment address pointer
        jmp     checkloop


; Unimplemented commands
SearchCommand:
TestCommand:
VerifyCommand:
MemoryCommand:
MathCommand:
        call    PrintChar        ; Echo the command back
        call    PrintCR
        lxi     h,strNotImplemented
        call    PrintString
        call    PrintCR
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


; PrintAscii
; If character in A is printable ASCII, print it, otherwise print "."
; Registers affected: none.

PrintAscii:
        push    psw             ; Save A
        cpi     ' '             ; Less than <Space> ?
        jc      notPrintable    ; If so, not printable
        cpi     '~'+1           ; Greater than tilde?
        jnc     notPrintable    ; If so, not printable
ppr:    call    PrintChar       ; Print character
        pop     psw             ; Restore A
        ret
notPrintable:
        mvi     a,'.'
        jmp     ppr


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
        push    b               ; Save BC reg
        call    bhconv          ; Convert to two hex digits
        mov     a,b             ; Get first digit
        call    PrintChar       ; Print it
        mov     a,c             ; Get second digit
        call    PrintChar       ; Print it
        pop     b               ; Restore BC reg
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
        pop     h               ; Restore HL
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
        mvi     a,50            ; Delay constant
decr:   dcr     a               ; Decrement counter
        jnz     decr            ; Repeat until A reaches zero
        pop     psw             ; Restore A
        ret                     ; Return


; PrintAddress
; Print a two byte address passed in H,L.
; Registers affected: A.
PrintAddress:
        mov     a,h             ; Get MSB
        call    PrintByte       ; Print it
        mov     a,l             ; Get LSB
        call    PrintByte       ; Print it
        ret                     ; Return


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
; Registers affected: A,B

GetByte:
        call    GetHex          ; Get most significant nybble
        rc                      ; Exit if <ESC> pressed
        rlc                     ; Shift to upper nybble
        rlc
        rlc
        rlc
        mov     b,a             ; Save result in B register
        call    GetHex          ; Get least significant nybble
        rc                      ; Exit if <ESC> pressed
        add     b               ; Add upper nybble to lower
        ret                     ; Return


; GetAddress
; Gets a four character hex number from the keyboard.
; Ignores invalid characters. <Esc> cancels and sets carry bit.
; Returns binary word in HL.
; Registers affected: A,B,H,L

GetAddress:
        call    GetByte         ; Get MSB
        rc                      ; Exit if <ESC> pressed
        mov     h,a             ; Save MSB in H
        call    GetByte         ; Get LSB
        rc                      ; Exit if <ESC> pressed
        mov     l,a             ; Save LSB in L
        ret                     ; Return

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
        db      "C <src> <dest> <num bytes> Copy memory\r\n"
        db      "D <address>                Dump memory\r\n"
        db      "F <start> <end> <data>     Fill memory\r\n"
        db      "G <address>                Go\r\n"
        db      "I                          Show info\r\n"
        db      "K <start> <end>            Checksum\r\n"
        db      "L                          Clear screen\r\n"
        db      "R                          Examine registers\r\n"
        db      "S <start> <end> <data>     Search memory\r\n"
        db      "T <start> <end>            Test memory\r\n"
        db      "V <start> <end> <dest>     Verify memory\r\n"
        db      ": <address> <data>...      Write to memory\r\n"
        db      "= <address> +/- <address>  Hex math calculation\r\n"
        db      "?                          Help\r\n",0

strClearScreen:
        db      "\e[2J\e[H",0       ; VT100/ANSI clear screen, cursor home

strCpuType:
        db      "CPU type: ",0
str8080:
        db      "8080",0
strZ80:
        db      "Z80",0
strContinue:
        db      "Press <Space> to continue, <ESC> to stop ",0
strNotImplemented:
        db      "Sorry, command not yet implemented",0

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
save_pc dw      0000h
src     dw      ?               ; Used for commands like Copy
dst     dw      ?
size    dw      ?


        end
