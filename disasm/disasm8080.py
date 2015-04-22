#! /usr/bin/env python3
#
# Disassembler for Intel 8080 microprocessor.
# Copyright (c) 2013-2015 by Jeff Tranter <tranter@pobox.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Possible enhancements:
# - Read Intel HEX file format
# - binary (front panel) output mode, e.g.
#
#       ADDRESS              DATA
# . ... ... ... ... ...   .. ... ...
# 0 001 001 000 110 100   11 000 011  JMP
# 0 001 001 000 110 101   00 110 100  064
# 0 001 001 000 110 110   00 001 001  011

import sys
import fileinput
import argparse
import signal

# Avoids an error when output piped, e.g. to "less"
signal.signal(signal.SIGPIPE, signal.SIG_DFL)

# Lookup table - given opcode byte as index, return mnemonic of instruction
# and length of instruction.

# Alternative opcodes have a '*' prepended.
lookupTable = [
    ["nop",         1],  # 00
    ["lxi     b,",  3],  # 01
    ["stax    b",   1],  # 02
    ["inx     b",   1],  # 03
    ["inr     b",   1],  # 04
    ["dcr     b",   1],  # 05
    ["mvi     b,",  2],  # 06
    ["rlc",         1],  # 07
    ["*nop",        1],  # 08
    ["dad     b",   1],  # 09
    ["ldax    b",   1],  # 0A
    ["dcx     b",   1],  # 0B
    ["inr     c",   1],  # 0C
    ["dcr     c",   1],  # 0D
    ["mvi     c,",  2],  # 0E
    ["rrc",         1],  # 0F

    ["*nop",        1],  # 10
    ["lxi     d,",  3],  # 11
    ["stax    d",   1],  # 12
    ["inx     d",   1],  # 13
    ["inr     d",   1],  # 14
    ["dcr     d",   1],  # 15
    ["mvi     d,",  2],  # 16
    ["ral",         1],  # 17
    ["*nop",        1],  # 18
    ["dad",         1],  # 19
    ["ldax    d",   1],  # 1A
    ["dcx     d",   1],  # 1B
    ["inr     e",   1],  # 1C
    ["dcr     e",   1],  # 1D
    ["mvi     e,",  2],  # 1E
    ["rar",         1],  # 1F

    ["*nop",        1],  # 20
    ["lxi     h,",  3],  # 21
    ["shld    ",    3],  # 22
    ["inx     h",   1],  # 23
    ["inr     h",   1],  # 24
    ["dcr     h",   1],  # 25
    ["mvi     h,",  2],  # 26
    ["daa",         1],  # 27
    ["*nop",        1],  # 28
    ["dad     h",   1],  # 29
    ["lhld    ",    3],  # 2A
    ["dcx     h",   1],  # 2B
    ["inr     l",   1],  # 2C
    ["dcr     l",   1],  # 2D
    ["mvi     l,",  2],  # 2E
    ["cma",         1],  # 2F

    ["*nop",        1],  # 30
    ["lxi     sp,", 3],  # 31
    ["sta     ",    3],  # 32
    ["inx     sp",  1],  # 33
    ["inr     m",   1],  # 34
    ["dcr     m",   1],  # 35
    ["mvi     m,",  2],  # 36
    ["stc",         1],  # 37
    ["*nop",        1],  # 38
    ["dad     sp",  1],  # 39
    ["lda     ",    3],  # 3A
    ["dcx     sp",  1],  # 3B
    ["inr     a",   1],  # 3C
    ["dcr     a",   1],  # 3D
    ["mvi     a,",  2],  # 3E
    ["cmc",         1],  # 3F

    ["mov     b,b", 1],  # 40
    ["mov     b,c", 1],  # 41
    ["mov     b,d", 1],  # 42
    ["mov     b,e", 1],  # 43
    ["mov     b,h", 1],  # 44
    ["mov     b,l", 1],  # 45
    ["mov     b,m", 1],  # 46
    ["mov     b,a", 1],  # 47
    ["mov     c,b", 1],  # 48
    ["mov     c,c", 1],  # 49
    ["mov     c,d", 1],  # 4A
    ["mov     c,e", 1],  # 4B
    ["mov     c,h", 1],  # 4C
    ["mov     c,l", 1],  # 4D
    ["mov     c,m", 1],  # 4E
    ["mov     c,a", 1],  # 4F

    ["mov     d,b", 1],  # 50
    ["mov     d,c", 1],  # 51
    ["mov     d,d", 1],  # 52
    ["mov     d,e", 1],  # 53
    ["mov     d,h", 1],  # 54
    ["mov     d,l", 1],  # 55
    ["mov     d,m", 1],  # 56
    ["mov     d,a", 1],  # 57
    ["mov     e,b", 1],  # 58
    ["mov     e,c", 1],  # 59
    ["mov     e,d", 1],  # 5A
    ["mov     e,e", 1],  # 5B
    ["mov     e,h", 1],  # 5C
    ["mov     e,l", 1],  # 5D
    ["mov     e,m", 1],  # 5E
    ["mov     e,a", 1],  # 5F

    ["mov     h,b", 1],  # 60
    ["mov     h,c", 1],  # 61
    ["mov     h,d", 1],  # 62
    ["mov     h,e", 1],  # 63
    ["mov     h,h", 1],  # 64
    ["mov     h,l", 1],  # 65
    ["mov     h,m", 1],  # 66
    ["mov     h,a", 1],  # 67
    ["mov     l,b", 1],  # 68
    ["mov     l,c", 1],  # 69
    ["mov     l,d", 1],  # 6A
    ["mov     l,e", 1],  # 6B
    ["mov     l,h", 1],  # 6C
    ["mov     l,l", 1],  # 6D
    ["mov     l,m", 1],  # 6E
    ["mov     l,a", 1],  # 6F

    ["mov     m,b", 1],  # 70
    ["mov     m,c", 1],  # 71
    ["mov     m,d", 1],  # 72
    ["mov     m,e", 1],  # 73
    ["mov     m,h", 1],  # 74
    ["mov     m,l", 1],  # 75
    ["hlt",         1],  # 76
    ["mov     m,a", 1],  # 77
    ["mov     a,b", 1],  # 78
    ["mov     a,c", 1],  # 79
    ["mov     a,d", 1],  # 7A
    ["mov     a,e", 1],  # 7B
    ["mov     a,h", 1],  # 7C
    ["mov     a,l", 1],  # 7D
    ["mov     a,m", 1],  # 7E
    ["mov     a,a", 1],  # 7F

    ["add     b",   1],  # 80
    ["add     c",   1],  # 81
    ["add     d",   1],  # 82
    ["add     e",   1],  # 83
    ["add     h",   1],  # 84
    ["add     l",   1],  # 85
    ["add     m",   1],  # 86
    ["add     a",   1],  # 87
    ["adc     b",   1],  # 88
    ["adc     c",   1],  # 89
    ["adc     d",   1],  # 8A
    ["adc     e",   1],  # 8B
    ["adc     h",   1],  # 8C
    ["adc     l",   1],  # 8D
    ["adc     m",   1],  # 8E
    ["adc     a",   1],  # 8F

    ["sub     b",   1],  # 90
    ["sub     c",   1],  # 91
    ["sub     d",   1],  # 92
    ["sub     e",   1],  # 93
    ["sub     h",   1],  # 94
    ["sub     l",   1],  # 95
    ["sub     m",   1],  # 96
    ["sub     a",   1],  # 97
    ["sbb     b",   1],  # 98
    ["sbb     c",   1],  # 99
    ["sbb     d",   1],  # 9A
    ["sbb     e",   1],  # 9B
    ["sbb     h",   1],  # 9C
    ["sbb     l",   1],  # 9D
    ["sbb     m",   1],  # 9E
    ["sbb     a",   1],  # 9F

    ["ana     b",   1],  # A0
    ["ana     c",   1],  # A1
    ["ana     d",   1],  # A2
    ["ana     e",   1],  # A3
    ["ana     h",   1],  # A4
    ["ana     l",   1],  # A5
    ["ana     m",   1],  # A6
    ["ana     a",   1],  # A7
    ["xra     b",   1],  # A8
    ["xra     c",   1],  # A9
    ["xra     d",   1],  # AA
    ["xra     e",   1],  # AB
    ["xra     h",   1],  # AC
    ["xra     l",   1],  # AD
    ["xra     m",   1],  # AE
    ["xra     a",   1],  # AF

    ["ora     b",   1],  # B0
    ["ora     c",   1],  # B1
    ["ora     d",   1],  # B2
    ["ora     e",   1],  # B3
    ["ora     h",   1],  # B4
    ["ora     l",   1],  # B5
    ["ora     m",   1],  # B6
    ["ora     a",   1],  # B7
    ["cmp     b",   1],  # B8
    ["cmp     c",   1],  # B9
    ["cmp     d",   1],  # BA
    ["cmp     e",   1],  # BB
    ["cmp     h",   1],  # BC
    ["cmp     l",   1],  # BD
    ["cmp     m",   1],  # BE
    ["cmp     a",   1],  # BF

    ["rnz",         1],  # C0
    ["pop     b",   1],  # C1
    ["jnz     ",    3],  # C2
    ["jmp     ",    3],  # C3
    ["cnz     ",    3],  # C4
    ["push    b",   1],  # C5
    ["adi     ",    2],  # C6
    ["rst     0",   1],  # C7
    ["rz",          1],  # C8
    ["ret",         1],  # C9
    ["jz      ",    3],  # CA
    ["*jmp     ",   3],  # CB
    ["cz      ",    3],  # CC
    ["call    ",    3],  # CD
    ["aci     ",    2],  # CE
    ["rst     1",   1],  # CF

    ["rnc",         1],  # D0
    ["pop     d",   1],  # D1
    ["jnc     ",    3],  # D2
    ["out     ",    2],  # D3
    ["cnc     ",    3],  # D4
    ["push    d",   1],  # D5
    ["sui     ",    2],  # D6
    ["rst     2",   1],  # D7
    ["rc",          1],  # D8
    ["*ret",        1],  # D9
    ["jc      ",    3],  # DA
    ["in      ",    2],  # DB
    ["cc      ",    3],  # DC
    ["*call    ",   3],  # DD
    ["sbi     ",    2],  # DE
    ["rst     3",   1],  # DF

    ["rpo",         1],  # E0
    ["pop     h",   1],  # E1
    ["jpo     ",    3],  # E2
    ["xthl",        1],  # E3
    ["cpo     ",    3],  # E4
    ["push    h",   1],  # E5
    ["ani     ",    2],  # E6
    ["rst     4",   1],  # E7
    ["rpe",         1],  # E8
    ["pchl",        1],  # E9
    ["jpe     ",    3],  # EA
    ["xchg",        1],  # EB
    ["cpe     ",    3],  # EC
    ["*call    ",   3],  # ED
    ["xri     ",    2],  # EE
    ["rst     5",   1],  # EF

    ["rp",          1],  # F0
    ["pop     psw", 1],  # F1
    ["jp      ",    3],  # F2
    ["di",          1],  # F3
    ["cp      ",    3],  # F4
    ["push    psw", 1],  # F5
    ["ori     ",    2],  # F6
    ["rst     6",   1],  # F7
    ["rm",          1],  # F8
    ["sphl",        1],  # F9
    ["jm      ",    3],  # FA
    ["ei",          1],  # FB
    ["cm      ",    3],  # FC
    ["*call    ",   3],  # FD
    ["cpi     ",    2],  # FE
    ["rst     7",   1],  # FF
]

# Indicates if uppercase option is in effect.
upperOption = False

# Functions


def isprint(c):
    "Return if character is printable ASCII"
    if c >= '@' and c <= '~':
        return True
    else:
        return False


def case(s):
    "Return string or uppercase version of string if option is set."
    global upperOption
    if upperOption:
        return s.upper()
    else:
        return s


def formatByte(data):
    "Format an 8-bit byte using the current display format (e.g. hex or octal)"
    global args
    if args.format == 4:  # Octal
        return "%03o" % data
    else:  # Hex
        return "%02X" % data


def formatAddress(data):
    "Format a 16-bit address using the current display format (e.g. hex or octal)"
    global args
    if args.format == 4:  # Octal
        return "%06o" % data
    else:  # Hex
        return "%04X" % data

# Parse command line options
parser = argparse.ArgumentParser()
parser.add_argument("filename", help="Binary file to disassemble")
parser.add_argument("-n", "--nolist",
                    help="Don't list  instruction bytes (make output suitable for assembler)",
                    action="store_true")
parser.add_argument("-u", "--uppercase", help="Use uppercase for mnemonics", action="store_true")
parser.add_argument("-a", "--address", help="Specify decimal starting address (defaults to 0)", default=0, type=int)
parser.add_argument("-f", "--format",
                    help="Use number format: 1=$1234 2=1234h 3=1234 4=177777 (default 1)",
                    default=1, type=int, choices=range(1, 5))
args = parser.parse_args()

# Get filename from command line arguments.
filename = args.filename

# Current instruction address. Silently force it to be in valid range.
address = args.address & 0xffff

# Set uppercase output option.
upperOption = args.uppercase

# Contains a line of output
line = ""

# Open input file.
# Display error and exit if filename does not exist.
try:
    f = open(filename, "rb")
except FileNotFoundError:
    print("error: input file '%s' not found." % filename, file=sys.stderr)
    sys.exit(1)

# Print initial origin address
if args.nolist is False:
    if args.format == 1:
        print("%04X            %s     $%04X" % (address, case("org"), address))
    elif args.format == 2:
        print("%04X            %s     %04X%s" % (address, case("org"), address, case("h")))
    elif args.format == 3:
        print("%04X            %s     %04X" % (address, case("org"), address))
    else:
        print("%06o               %s     %06o" % (address, case("org"), address))

while True:
    try:
        b = f.read(1)  # Get binary byte from file

        if len(b) == 0:  # EOF
            if args.nolist is False:
                if args.format == 4:
                    print("%06o               %s" % (address, case("end")))  # Exit if end of file reached.
                else:
                    print("%04X            %s" % (address, case("end")))  # Exit if end of file reached.
            break

        if args.nolist is False:
            line = "%s  " % formatAddress(address)  # Print current address

        op = ord(b)  # Get opcode byte

        n = lookupTable[op][1]  # Look up number of instruction bytes

        mnem = case(lookupTable[op][0])  # Get mnemonic

        # Print instruction bytes
        if n == 1:
            if args.nolist is False:
                if args.format == 4:
                    line += "%03o          " % op
                else:
                    line += "%02X        " % op
        elif n == 2:
            try:  # Possible to get exception here if EOF reached.
                op1 = ord(f.read(1))
            except TypeError:
                op1 = 0  # Fake it to recover from EOF
            if args.nolist is False:
                if args.format == 4:
                    line += "%03o %03o      " % (op, op1)
                else:
                    line += "%02X %02X     " % (op, op1)
        elif n == 3:
            try:  # Possible to get exception here if EOF reached.
                op1 = ord(f.read(1))
                op2 = ord(f.read(1))
            except TypeError:
                op1 = 0  # Fake it to recover from EOF
                op2 = 0
            if args.nolist is False:
                line += "%s %s %s  " % (formatByte(op), formatByte(op1), formatByte(op2))
        if args.nolist is True:
            line += " "

        # If opcode starts with '*' then put in comment that this is an alternative op code (likely an error).
        if mnem[0] == "*":
            alternative = True
            mnem = mnem.replace(mnem[:1], '')  # Remove the star
        else:
            alternative = False

        line += mnem

        # Handle any operands
        if n == 2:
            if isprint(chr(op1)):
                line += "'%c'" % op1
            else:
                if args.format == 1:
                    line += "$%s" % formatByte(op1)
                elif args.format == 2:
                    line += "%s%s" % (formatByte(op1), case("h"))
                else:
                    line += "%s" % formatByte(op1)
        elif n == 3:
            if args.format == 1:
                line += "$%s%s" % (formatByte(op2), formatByte(op1))
            elif args.format == 2:
                line += "%s%s%s" % (formatByte(op2), formatByte(op1), case("h"))
            else:
                line += "%s%s" % (formatByte(op2), formatByte(op1))

        if alternative:
            mnem = mnem.replace(mnem[:1], '')  # Remove the star
            # Line up comment at fixed column position
            if args.nolist is False:
                line += ";Note: Alternative opcode used".rjust(67 - len(line))
            else:
                line += ";Note: Alternative opcode used".rjust(51 - len(line))

        # Update address
        address += n

        # Check for address exceeding 0xFFFF, if so wrap around.
        if address > 0xffff:
            address = address & 0xffff

        # Finished a line of disassembly
        print(line)
        line = ""

    except KeyboardInterrupt:
        # Exit if end of file reached.
        print("Interrupted by Control-C", file=sys.stderr)
        if args.format == 4:
            print("%s               %s" % (formatAddress(address), case("end")))
        else:
            print("%s            %s" % (formatAddress(address), case("end")))
        break
