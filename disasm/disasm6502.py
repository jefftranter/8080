#! /usr/bin/env python3
#
# Disassembler for 6502 microprocessor.
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

import sys
import fileinput
import argparse
import signal

# Avoids an error when output piped, e.g. to "less"
signal.signal(signal.SIGPIPE, signal.SIG_DFL)

# Addressing modes. Used as indices into opcode table.
implicit    = 0  # e.g. rts
absolute    = 1  # e.g. lda $1234
absoluteX   = 2  # e.g. lda $1234,x
absoluteY   = 3  # e.g. lda $1234,Y
accumulator = 4  # e.g. asl a
immediate   = 5  # e.g. lda #$12
indirectX   = 6  # e.g. lda ($12,X)
indirectY   = 7  # e.g. lda ($12),Y
indirect    = 8  # e.g. jmp ($1234)
relative    = 9  # e.g. bne $1234
zeroPage    = 10 # e.g. lda #12
zeroPageX   = 11 # e.g. lda $12,X
zeroPageY   = 12 # e.g. lda $12,Y

# Lookup table - given addressing mode, returns length of instruction in bytes.
lengthTable = [
  1, # 0 - implicit
  3, # 1 - absolute
  3, # 2 - absolute X
  3, # 3 - absolute Y
  1, # 4 - accumulator
  2, # 5 - immediate
  2, # 6 - indirect X
  2, # 7 - indirect Y
  3, # 8 - indirect
  2, # 9 - relative
  2, # 10 - zero page
  2, # 11 - zero page X
  2  # 12 - zero page Y
]

# Lookup table - given opcode byte as index, return mnemonic of instruction and addressing mode.
# Invalid opcodes are listed as "???".
opcodeTable = [
  [ "brk", implicit ],    # 00
  [ "ora", indirectX ],   # 01
  [ "???", implicit ],    # 02
  [ "???", implicit ],    # 03
  [ "???", implicit ],    # 04
  [ "ora", zeroPage ],    # 05
  [ "asl", zeroPage ],    # 06
  [ "???", implicit ],    # 07
  [ "php", implicit ],    # 08
  [ "ora", immediate ],   # 09
  [ "asl", accumulator ], # 0A
  [ "???", implicit ],    # 0B
  [ "???", implicit ],    # 0C
  [ "ora", absolute ],    # 0D
  [ "asl", absolute ],    # 0E
  [ "???", implicit ],    # 0F

  [ "bpl", relative ],    # 10
  [ "ora", indirectY ],   # 11
  [ "???", implicit ],    # 12
  [ "???", implicit ],    # 13
  [ "???", implicit ],    # 14
  [ "ora", zeroPageX ],    # 15
  [ "asl", zeroPageX ],    # 16
  [ "???", implicit ],    # 17
  [ "clc", implicit ],    # 18
  [ "ora", absoluteY ],   # 19
  [ "???", implicit ],    # 1A
  [ "???", implicit ],    # 1B
  [ "???", implicit ],    # 1C
  [ "ora", absoluteX ],   # 1D
  [ "asl", absoluteX],    # 1E
  [ "???", implicit ],    # 1F

  [ "jsr", absolute ],    # 20
  [ "XXX", implicit ],    # 21
  [ "XXX", implicit ],    # 22
  [ "XXX", implicit ],    # 23
  [ "XXX", implicit ],    # 24
  [ "XXX", implicit ],    # 25
  [ "XXX", implicit ],    # 26
  [ "XXX", implicit ],    # 27
  [ "XXX", implicit ],    # 28
  [ "XXX", implicit ],    # 29
  [ "XXX", implicit ],    # 2A
  [ "XXX", implicit ],    # 2B
  [ "XXX", implicit ],    # 2C
  [ "XXX", implicit ],    # 2D
  [ "XXX", implicit ],    # 2E
  [ "XXX", implicit ],    # 2F

  [ "XXX", implicit ],    # 30
  [ "XXX", implicit ],    # 31
  [ "XXX", implicit ],    # 32
  [ "XXX", implicit ],    # 33
  [ "XXX", implicit ],    # 34
  [ "XXX", implicit ],    # 35
  [ "XXX", implicit ],    # 36
  [ "XXX", implicit ],    # 37
  [ "XXX", implicit ],    # 38
  [ "XXX", implicit ],    # 39
  [ "XXX", implicit ],    # 3A
  [ "XXX", implicit ],    # 3B
  [ "XXX", implicit ],    # 3C
  [ "XXX", implicit ],    # 3D
  [ "XXX", implicit ],    # 3E
  [ "XXX", implicit ],    # 3F

  [ "XXX", implicit ],    # 40
  [ "XXX", implicit ],    # 41
  [ "XXX", implicit ],    # 42
  [ "XXX", implicit ],    # 43
  [ "XXX", implicit ],    # 44
  [ "XXX", implicit ],    # 45
  [ "XXX", implicit ],    # 46
  [ "XXX", implicit ],    # 47
  [ "pha", implicit ],    # 48
  [ "XXX", implicit ],    # 49
  [ "XXX", implicit ],    # 4A
  [ "XXX", implicit ],    # 4B
  [ "jmp", absolute ],    # 4C
  [ "XXX", implicit ],    # 4D
  [ "XXX", implicit ],    # 4E
  [ "XXX", implicit ],    # 4F

  [ "XXX", implicit ],    # 50
  [ "XXX", implicit ],    # 51
  [ "XXX", implicit ],    # 52
  [ "XXX", implicit ],    # 53
  [ "XXX", implicit ],    # 54
  [ "XXX", implicit ],    # 55
  [ "XXX", implicit ],    # 56
  [ "XXX", implicit ],    # 57
  [ "XXX", implicit ],    # 58
  [ "XXX", implicit ],    # 59
  [ "XXX", implicit ],    # 5A
  [ "XXX", implicit ],    # 5B
  [ "XXX", implicit ],    # 5C
  [ "XXX", implicit ],    # 5D
  [ "XXX", implicit ],    # 5E
  [ "XXX", implicit ],    # 5F

  [ "rts", implicit ],    # 60
  [ "XXX", implicit ],    # 61
  [ "XXX", implicit ],    # 62
  [ "XXX", implicit ],    # 63
  [ "XXX", implicit ],    # 64
  [ "XXX", implicit ],    # 65
  [ "XXX", implicit ],    # 66
  [ "XXX", implicit ],    # 67
  [ "pla", implicit ],    # 68
  [ "adc", immediate ],   # 69
  [ "XXX", implicit ],    # 6A
  [ "XXX", implicit ],    # 6B
  [ "XXX", implicit ],    # 6C
  [ "XXX", implicit ],    # 6D
  [ "XXX", implicit ],    # 6E
  [ "XXX", implicit ],    # 6F

  [ "XXX", implicit ],    # 70
  [ "XXX", implicit ],    # 71
  [ "XXX", implicit ],    # 72
  [ "XXX", implicit ],    # 73
  [ "XXX", implicit ],    # 74
  [ "XXX", implicit ],    # 75
  [ "XXX", implicit ],    # 76
  [ "XXX", implicit ],    # 77
  [ "XXX", implicit ],    # 78
  [ "XXX", implicit ],    # 79
  [ "XXX", implicit ],    # 7A
  [ "XXX", implicit ],    # 7B
  [ "XXX", implicit ],    # 7C
  [ "XXX", implicit ],    # 7D
  [ "XXX", implicit ],    # 7E
  [ "XXX", implicit ],    # 7F

  [ "XXX", implicit ],    # 80
  [ "XXX", implicit ],    # 81
  [ "XXX", implicit ],    # 82
  [ "XXX", implicit ],    # 83
  [ "sty", zeroPage ],    # 84
  [ "sta", zeroPage ],    # 85
  [ "stx", zeroPage ],    # 86
  [ "XXX", implicit ],    # 87
  [ "XXX", implicit ],    # 88
  [ "XXX", implicit ],    # 89
  [ "XXX", implicit ],    # 8A
  [ "XXX", implicit ],    # 8B
  [ "XXX", implicit ],    # 8C
  [ "sta", absolute ],    # 8D
  [ "XXX", implicit ],    # 8E
  [ "XXX", implicit ],    # 8F

  [ "bcc", relative ],    # 90
  [ "XXX", implicit ],    # 91
  [ "XXX", implicit ],    # 92
  [ "XXX", implicit ],    # 93
  [ "XXX", implicit ],    # 94
  [ "XXX", implicit ],    # 95
  [ "XXX", implicit ],    # 96
  [ "XXX", implicit ],    # 97
  [ "tya", implicit ],    # 98
  [ "XXX", implicit ],    # 99
  [ "txs", implicit ],    # 9A
  [ "XXX", implicit ],    # 9B
  [ "XXX", implicit ],    # 9C
  [ "XXX", implicit ],    # 9D
  [ "XXX", implicit ],    # 9E
  [ "XXX", implicit ],    # 9F

  [ "ldy", immediate ],   # A0
  [ "XXX", implicit ],    # A1
  [ "ldx", immediate ],   # A2
  [ "XXX", implicit ],    # A3
  [ "ldy", zeroPage ],    # A4
  [ "lda", zeroPage ],    # A5
  [ "ldx", zeroPage ],    # A6
  [ "XXX", implicit ],    # A7
  [ "tay", implicit ],    # A8
  [ "lda", immediate ],   # A9
  [ "XXX", implicit ],    # AA
  [ "XXX", implicit ],    # AB
  [ "XXX", implicit ],    # AC
  [ "XXX", implicit ],    # AD
  [ "XXX", implicit ],    # AE
  [ "XXX", implicit ],    # AF

  [ "XXX", implicit ],    # B0
  [ "lda", indirectY ],   # B1
  [ "XXX", implicit ],    # B2
  [ "XXX", implicit ],    # B3
  [ "XXX", implicit ],    # B4
  [ "XXX", implicit ],    # B5
  [ "XXX", implicit ],    # B6
  [ "XXX", implicit ],    # B7
  [ "XXX", implicit ],    # B8
  [ "XXX", implicit ],    # B9
  [ "XXX", implicit ],    # BA
  [ "XXX", implicit ],    # BB
  [ "XXX", implicit ],    # BC
  [ "XXX", implicit ],    # BD
  [ "XXX", implicit ],    # BE
  [ "XXX", implicit ],    # BF

  [ "cpy", immediate ],   # C0
  [ "XXX", implicit ],    # C1
  [ "XXX", implicit ],    # C2
  [ "XXX", implicit ],    # C3
  [ "XXX", implicit ],    # C4
  [ "XXX", implicit ],    # C5
  [ "XXX", implicit ],    # C6
  [ "XXX", implicit ],    # C7
  [ "iny", implicit ],    # C8
  [ "cmp", immediate ],   # C9
  [ "XXX", implicit ],    # CA
  [ "XXX", implicit ],    # CB
  [ "XXX", implicit ],    # CC
  [ "XXX", implicit ],    # CD
  [ "XXX", implicit ],    # CE
  [ "XXX", implicit ],    # CF

  [ "bne", relative ],    # D0
  [ "XXX", implicit ],    # D1
  [ "XXX", implicit ],    # D2
  [ "XXX", implicit ],    # D3
  [ "XXX", implicit ],    # D4
  [ "XXX", implicit ],    # D5
  [ "XXX", implicit ],    # D6
  [ "XXX", implicit ],    # D7
  [ "cld", implicit ],    # D8
  [ "XXX", implicit ],    # D9
  [ "XXX", implicit ],    # DA
  [ "XXX", implicit ],    # DB
  [ "XXX", implicit ],    # DC
  [ "XXX", implicit ],    # DD
  [ "XXX", implicit ],    # DE
  [ "XXX", implicit ],    # DF

  [ "XXX", implicit ],    # E0
  [ "XXX", implicit ],    # E1
  [ "XXX", implicit ],    # E2
  [ "XXX", implicit ],    # E3
  [ "XXX", implicit ],    # E4
  [ "XXX", implicit ],    # E5
  [ "inc", zeroPage ],    # E6
  [ "XXX", implicit ],    # E7
  [ "XXX", implicit ],    # E8
  [ "XXX", implicit ],    # E9
  [ "nop", implicit ],    # EA
  [ "XXX", implicit ],    # EB
  [ "XXX", implicit ],    # EC
  [ "XXX", implicit ],    # ED
  [ "XXX", implicit ],    # EE
  [ "XXX", implicit ],    # EF

  [ "beq", relative ],    # F0
  [ "XXX", implicit ],    # F1
  [ "XXX", implicit ],    # F2
  [ "XXX", implicit ],    # F3
  [ "XXX", implicit ],    # F4
  [ "XXX", implicit ],    # F5
  [ "XXX", implicit ],    # F6
  [ "XXX", implicit ],    # F7
  [ "XXX", implicit ],    # F8
  [ "XXX", implicit ],    # F9
  [ "XXX", implicit ],    # FA
  [ "XXX", implicit ],    # FB
  [ "XXX", implicit ],    # FC
  [ "XXX", implicit ],    # FD
  [ "XXX", implicit ],    # FE
  [ "XXX", implicit ],    # FF

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
    if args.format == 4: # Octal
        return "%03o" % data
    else: # Hex
        return "%02X" % data

def formatAddress(data):
    "Format a 16-bit address using the current display format (e.g. hex or octal)"
    global args
    if args.format == 4: # Octal
        return "%06o" % data
    else: # Hex
        return "%04X" % data

# Parse command line options
parser = argparse.ArgumentParser()
parser.add_argument("filename", help="Binary file to disassemble")
parser.add_argument("-n", "--nolist", help="Don't list  instruction bytes (make output suitable for assembler)", action="store_true")
parser.add_argument("-u", "--uppercase", help="Use uppercase for mnemonics", action="store_true")
parser.add_argument("-a", "--address", help="Specify decimal starting address (defaults to 0)", default=0, type=int)
parser.add_argument("-f", "--format", help="Use number format: 1=$1234 2=1234h 3=1234 4=177777 (default 1)", default=1, type=int, choices=range(1, 5))
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
if args.nolist == False:
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
        b = f.read(1) # Get binary byte from file

        if len(b) == 0: # EOF
            if args.nolist == False:
                if args.format == 4:
                    print("%06o               %s" % (address, case("end"))) # Exit if end of file reached.
                else:
                    print("%04X            %s" % (address, case("end"))) # Exit if end of file reached.
            break

        if args.nolist == False:
            line = "%s  " % formatAddress(address) # Print current address

        op = ord(b) # Get opcode byte

        mnem = case(opcodeTable[op][0]) # Get mnemonic

        mode = opcodeTable[op][1] # Get addressing mode

        n = lengthTable[mode] # Look up number of instruction bytes

#       print("*** mnem =", mnem, "mode =", mode, "len =", n)

        # Print instruction bytes
        if (n == 1):
            if args.nolist == False:
                if args.format == 4:
                    line += "%03o          " % op
                else:
                    line += "%02X        " % op
        elif (n == 2):
            try: # Possible to get exception here if EOF reached.
                op1 = ord(f.read(1))
            except TypeError:
                op1 = 0 # Fake it to recover from EOF
            if args.nolist == False:
                if args.format == 4:
                    line += "%03o %03o      " % (op, op1)
                else:
                    line += "%02X %02X     " % (op, op1)
        elif (n == 3):
            try: # Possible to get exception here if EOF reached.
                op1 = ord(f.read(1))
                op2 = ord(f.read(1))
            except TypeError:
                op1 = 0 # Fake it to recover from EOF
                op2 = 0
            if args.nolist == False:
                line += "%s %s %s  " % (formatByte(op), formatByte(op1), formatByte(op2))
        if args.nolist == True:
            line += " "

        line += mnem

        if (mode == absolute):
            if args.format == 1:
                line += "    $%s%s" % (formatByte(op2), formatByte(op1))
            elif args.format == 2:
                line += "    %s%s%s" % (formatByte(op2), formatByte(op1), case("h"))
            else:
                line += "    %s%s" % (formatByte(op2), formatByte(op1))

        elif (mode == absoluteX):
            if args.format == 1:
                line += "    $%s%s,%s" % (formatByte(op2), formatByte(op1), case("x"))
            elif args.format == 2:
                line += "    %s%s%s,%s" % (formatByte(op2), formatByte(op1), case("x"))
            else:
                line += "    %s%s,%s" % (formatByte(op2), formatByte(op1), case("x"))

        elif (mode == absoluteY):
            if args.format == 1:
                line += "    $%s%s,%s" % (formatByte(op2), formatByte(op1), case("y"))
            elif args.format == 2:
                line += "    %s%s%s,%s" % (formatByte(op2), formatByte(op1), case("y"))
            else:
                line += "    %s%s,%s" % (formatByte(op2), formatByte(op1), case("y"))

        elif (mode == accumulator):
                line += "    %s" % (("a"))

        elif (mode == immediate):
            if isprint(chr(op1)):
                line += "    #'%c'" % op1
            else:
                if args.format == 1:
                    line += "    #$%s" % formatByte(op1)
                elif args.format == 2:
                    line += "    #%s%s" % (formatByte(op1), case("h"))
                else:
                    line += "    #%s" % formatByte(op1)

        elif (mode == indirectX):
            if args.format == 1:
                line += "    ($%s,%s)" % (formatByte(op1), case("x"))
            elif args.format == 2:
                line += "    (%s%s,%s)" % (formatByte(op1), case("h"), case("x"))
            else:
                line += "    (%s,%s)" % (formatByte(op1), case("x"))

        elif (mode == indirectY):
            if args.format == 1:
                line += "    ($%s),%s" % (formatByte(op1), case("y"))
            elif args.format == 2:
                line += "    (%s%s),%s" % (formatByte(op1), case("h"), case("y"))
            else:
                line += "    (%s),%s" % (formatByte(op1), case("y"))

        elif (mode == indirect):
            if args.format == 1:
                line += "    ($%s%s)" % (formatByte(op2), formatByte(op1))
            elif args.format == 2:
                line += "    (%s%s%s)" % (formatByte(op2), formatByte(op1), case("h"))
            else:
                line += "    (%s%s)" % (formatByte(op2), formatByte(op1))

        elif (mode == relative):
            if (op1 < 128):
                dest = address + op1 + 2
            else:
                dest = address - (256 - op1) + 2
            if args.format == 1:
                line += "    $%s" % formatAddress(dest)
            elif args.format == 2:
                line += "    %s%s" % (formatAddress(desdt), case("h"))
            else:
                line += "    %s%s" % (formatAddress(dest), formatByte(op1))

        elif (mode == zeroPage):
            if args.format == 1:
                line += "    $%s" % formatByte(op1)
            elif args.format == 2:
                line += "    %s%s" % (formatByte(op1), case("h"))
            else:
                line += "    %s" % formatByte(op1)

        elif (mode == zeroPageX):
            if args.format == 1:
                line += "    $%s,%s" % (formatByte(op1), case("x"))
            elif args.format == 2:
                line += "    %s%s,%s" % (formatByte(op1), case("h"), case("x"))
            else:
                line += "    %s,%s" % (formatByte(op1), case("x"))

        elif (mode == zeroPageY):
            if args.format == 1:
                line += "    $%s,%s" % (formatByte(op1), case("y"))
            elif args.format == 2:
                line += "    %s%s,%s" % (formatByte(op1), case("h"), case("y"))
            else:
                line += "    %s,%s" % (formatByte(op1), case("y"))

        # Update address
        address += n

        # Check for address exceeding 0xFFFF, if so wrap around.
        if (address > 0xffff):
            address = address & 0xffff

        # Finished a line of disassembly
        print(line)
        line = ""

    except KeyboardInterrupt:
        print("Interrupted by Control-C", file=sys.stderr)
        if args.format == 4:
            print("%s               %s" % (formatAddress(address), case("end"))) # Exit if end of file reached.
        else:
            print("%s            %s" % (formatAddress(address), case("end"))) # Exit if end of file reached.
        break
