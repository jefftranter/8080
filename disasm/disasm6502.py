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
  [ "???", implicit ],    # 21
  [ "???", implicit ],    # 22
  [ "???", implicit ],    # 23
  [ "???", implicit ],    # 24
  [ "???", implicit ],    # 25
  [ "???", implicit ],    # 26
  [ "???", implicit ],    # 27
  [ "???", implicit ],    # 28
  [ "???", implicit ],    # 29
  [ "???", implicit ],    # 2A
  [ "???", implicit ],    # 2B
  [ "???", implicit ],    # 2C
  [ "???", implicit ],    # 2D
  [ "???", implicit ],    # 2E
  [ "???", implicit ],    # 2F

  [ "???", implicit ],    # 30
  [ "???", implicit ],    # 31
  [ "???", implicit ],    # 32
  [ "???", implicit ],    # 33
  [ "???", implicit ],    # 34
  [ "???", implicit ],    # 35
  [ "???", implicit ],    # 36
  [ "???", implicit ],    # 37
  [ "???", implicit ],    # 38
  [ "???", implicit ],    # 39
  [ "???", implicit ],    # 3A
  [ "???", implicit ],    # 3B
  [ "???", implicit ],    # 3C
  [ "???", implicit ],    # 3D
  [ "???", implicit ],    # 3E
  [ "???", implicit ],    # 3F

  [ "???", implicit ],    # 40
  [ "???", implicit ],    # 41
  [ "???", implicit ],    # 42
  [ "???", implicit ],    # 43
  [ "???", implicit ],    # 44
  [ "???", implicit ],    # 45
  [ "???", implicit ],    # 46
  [ "???", implicit ],    # 47
  [ "???", implicit ],    # 48
  [ "???", implicit ],    # 49
  [ "???", implicit ],    # 4A
  [ "???", implicit ],    # 4B
  [ "???", implicit ],    # 4C
  [ "???", implicit ],    # 4D
  [ "???", implicit ],    # 4E
  [ "???", implicit ],    # 4F

  [ "???", implicit ],    # 50
  [ "???", implicit ],    # 51
  [ "???", implicit ],    # 52
  [ "???", implicit ],    # 53
  [ "???", implicit ],    # 54
  [ "???", implicit ],    # 55
  [ "???", implicit ],    # 56
  [ "???", implicit ],    # 57
  [ "???", implicit ],    # 58
  [ "???", implicit ],    # 59
  [ "???", implicit ],    # 5A
  [ "???", implicit ],    # 5B
  [ "???", implicit ],    # 5C
  [ "???", implicit ],    # 5D
  [ "???", implicit ],    # 5E
  [ "???", implicit ],    # 5F

  [ "???", implicit ],    # 60
  [ "???", implicit ],    # 61
  [ "???", implicit ],    # 62
  [ "???", implicit ],    # 63
  [ "???", implicit ],    # 64
  [ "???", implicit ],    # 65
  [ "???", implicit ],    # 66
  [ "???", implicit ],    # 67
  [ "???", implicit ],    # 68
  [ "???", implicit ],    # 69
  [ "???", implicit ],    # 6A
  [ "???", implicit ],    # 6B
  [ "???", implicit ],    # 6C
  [ "???", implicit ],    # 6D
  [ "???", implicit ],    # 6E
  [ "???", implicit ],    # 6F

  [ "???", implicit ],    # 70
  [ "???", implicit ],    # 71
  [ "???", implicit ],    # 72
  [ "???", implicit ],    # 73
  [ "???", implicit ],    # 74
  [ "???", implicit ],    # 75
  [ "???", implicit ],    # 76
  [ "???", implicit ],    # 77
  [ "???", implicit ],    # 78
  [ "???", implicit ],    # 79
  [ "???", implicit ],    # 7A
  [ "???", implicit ],    # 7B
  [ "???", implicit ],    # 7C
  [ "???", implicit ],    # 7D
  [ "???", implicit ],    # 7E
  [ "???", implicit ],    # 7F

  [ "???", implicit ],    # 80
  [ "???", implicit ],    # 81
  [ "???", implicit ],    # 82
  [ "???", implicit ],    # 83
  [ "???", implicit ],    # 84
  [ "???", implicit ],    # 85
  [ "???", implicit ],    # 86
  [ "???", implicit ],    # 87
  [ "???", implicit ],    # 88
  [ "???", implicit ],    # 89
  [ "???", implicit ],    # 8A
  [ "???", implicit ],    # 8B
  [ "???", implicit ],    # 8C
  [ "sta", absolute ],    # 8D
  [ "???", implicit ],    # 8E
  [ "???", implicit ],    # 8F

  [ "???", implicit ],    # 90
  [ "???", implicit ],    # 91
  [ "???", implicit ],    # 92
  [ "???", implicit ],    # 93
  [ "???", implicit ],    # 94
  [ "???", implicit ],    # 95
  [ "???", implicit ],    # 96
  [ "???", implicit ],    # 97
  [ "???", implicit ],    # 98
  [ "???", implicit ],    # 99
  [ "txs", implicit ],    # 9A
  [ "???", implicit ],    # 9B
  [ "???", implicit ],    # 9C
  [ "???", implicit ],    # 9D
  [ "???", implicit ],    # 9E
  [ "???", implicit ],    # 9F

  [ "ldy", immediate ],   # A0
  [ "???", implicit ],    # A1
  [ "ldx", immediate ],   # A2
  [ "???", implicit ],    # A3
  [ "???", implicit ],    # A4
  [ "???", implicit ],    # A5
  [ "???", implicit ],    # A6
  [ "???", implicit ],    # A7
  [ "???", implicit ],    # A8
  [ "lda", immediate ],   # A9
  [ "???", implicit ],    # AA
  [ "???", implicit ],    # AB
  [ "???", implicit ],    # AC
  [ "???", implicit ],    # AD
  [ "???", implicit ],    # AE
  [ "???", implicit ],    # AF

  [ "???", implicit ],    # B0
  [ "???", implicit ],    # B1
  [ "???", implicit ],    # B2
  [ "???", implicit ],    # B3
  [ "???", implicit ],    # B4
  [ "???", implicit ],    # B5
  [ "???", implicit ],    # B6
  [ "???", implicit ],    # B7
  [ "???", implicit ],    # B8
  [ "???", implicit ],    # B9
  [ "???", implicit ],    # BA
  [ "???", implicit ],    # BB
  [ "???", implicit ],    # BC
  [ "???", implicit ],    # BD
  [ "???", implicit ],    # BE
  [ "???", implicit ],    # BF

  [ "???", implicit ],    # C0
  [ "???", implicit ],    # C1
  [ "???", implicit ],    # C2
  [ "???", implicit ],    # C3
  [ "???", implicit ],    # C4
  [ "???", implicit ],    # C5
  [ "???", implicit ],    # C6
  [ "???", implicit ],    # C7
  [ "???", implicit ],    # C8
  [ "???", implicit ],    # C9
  [ "???", implicit ],    # CA
  [ "???", implicit ],    # CB
  [ "???", implicit ],    # CC
  [ "???", implicit ],    # CD
  [ "???", implicit ],    # CE
  [ "???", implicit ],    # CF

  [ "???", implicit ],    # D0
  [ "???", implicit ],    # D1
  [ "???", implicit ],    # D2
  [ "???", implicit ],    # D3
  [ "???", implicit ],    # D4
  [ "???", implicit ],    # D5
  [ "???", implicit ],    # D6
  [ "???", implicit ],    # D7
  [ "cld", implicit ],    # D8
  [ "???", implicit ],    # D9
  [ "???", implicit ],    # DA
  [ "???", implicit ],    # DB
  [ "???", implicit ],    # DC
  [ "???", implicit ],    # DD
  [ "???", implicit ],    # DE
  [ "???", implicit ],    # DF

  [ "???", implicit ],    # E0
  [ "???", implicit ],    # E1
  [ "???", implicit ],    # E2
  [ "???", implicit ],    # E3
  [ "???", implicit ],    # E4
  [ "???", implicit ],    # E5
  [ "???", implicit ],    # E6
  [ "???", implicit ],    # E7
  [ "???", implicit ],    # E8
  [ "???", implicit ],    # E9
  [ "???", implicit ],    # EA
  [ "???", implicit ],    # EB
  [ "???", implicit ],    # EC
  [ "???", implicit ],    # ED
  [ "???", implicit ],    # EE
  [ "???", implicit ],    # EF

  [ "???", implicit ],    # F0
  [ "???", implicit ],    # F1
  [ "???", implicit ],    # F2
  [ "???", implicit ],    # F3
  [ "???", implicit ],    # F4
  [ "???", implicit ],    # F5
  [ "???", implicit ],    # F6
  [ "???", implicit ],    # F7
  [ "???", implicit ],    # F8
  [ "???", implicit ],    # F9
  [ "???", implicit ],    # FA
  [ "???", implicit ],    # FB
  [ "???", implicit ],    # FC
  [ "???", implicit ],    # FD
  [ "???", implicit ],    # FE
  [ "???", implicit ],    # FF

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
            if args.format == 1:
                line += "FIXME    $%s%s" % (formatByte(op2), formatByte(op1))
            elif args.format == 2:
                line += "    %s%s%s" % (formatByte(op2), formatByte(op1), case("h"))
            else:
                line += "    %s%s" % (formatByte(op2), formatByte(op1))

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
