#! /usr/bin/env python3
#
# Disassembler for Zilog Z80 microprocessor.
# Copyright (c) 2013 by Jeff Tranter <tranter@pobox.com>
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
# - Option for octal output
# - Read Intel HEX file format

import sys
import fileinput
import argparse
import signal

# Avoids an error when output piped, e.g. to "less"
signal.signal(signal.SIGPIPE, signal.SIG_DFL)

# Lookup table - given opcode byte as index, return mnemonic of instruction and length of instruction.
lookupTable = [
  [ "nop", 1 ],         # 00
  [ "ld      bc,", 3 ],  # 01
  [ "ld      (bc),a", 1 ], # 02
  [ "inc     bc", 1 ],   # 03
  [ "inc     b", 1 ],   # 04
  [ "dec     b", 1 ],   # 05
  [ "ld      b,", 2 ],  # 06
  [ "rlca",      1 ],   # 07
  [ "ex      af,af", 1 ], # 08
  [ "add     hl,bc", 1 ], # 09
  [ "ld      a,(bc)", 1 ], # 0A
  [ "dec     bc", 1 ],   # 0B
  [ "inc     c", 1 ],   # 0C
  [ "dec     c", 1 ],   # 0D
  [ "ld      c,", 2 ],  # 0E
  [ "rrca",       1 ],  # 0F

  [ "djnz    ", 1 ], # 10
  [ "ld      de,", 3 ],  # 11
  [ "ld      (de),a,", 1 ],  # 12
  [ "inc     de", 1 ],  # 13
  [ "inc     d", 1 ],   # 14
  [ "dec     d", 1 ],   # 15
  [ "ld      d,", 2 ],  # 16
  [ "rla", 1 ],         # 17
  [ "jr      ", 1 ],    # 18
  [ "add     hl,de", 1 ], # 19
  [ "ld      a,(de)", 1 ],  # 1A
  [ "dec     de", 1 ],   # 1B
  [ "inc     e", 1 ],   # 1C
  [ "dec     e", 1 ],   # 1D
  [ "ld      e,", 2 ],  # 1E
  [ "rra", 1 ],         # 1F

  [ "jr      nz,", 1 ], # 20
  [ "ld      hl,", 3 ],  # 21
  [ "ld      (", 3 ],    # 22
  [ "inc     hl", 1 ],   # 23
  [ "inc     h", 1 ],   # 24
  [ "dec     h", 1 ],   # 25
  [ "ld      h,", 2 ],  # 26
  [ "daa", 1 ],         # 27
  [ "jr      z,", 1 ],        # 28
  [ "add     hl,jl", 1 ],   # 29
  [ "ld      hl,(", 3 ],    # 2A
  [ "dec     hl", 1 ],   # 2B
  [ "inc     l", 1 ],   # 2C
  [ "dec     l", 1 ],   # 2D
  [ "ld      l,", 2 ],  # 2E
  [ "cpl", 1 ],         # 2F

  [ "j       nc,", 1 ],        # 30
  [ "ld      sp,", 3 ], # 31
  [ "ld      (", 3 ],    # 32
  [ "inc     sp", 1 ],  # 33
  [ "inc     (hl)", 1 ],   # 34
  [ "dec     (hl)", 1 ],   # 35
  [ "ld      (hl),", 2 ],  # 36
  [ "scf", 1 ],         # 37
  [ "jr      c,", 1 ],        # 38
  [ "add     hl,sp", 1 ],  # 39
  [ "ld      a,(", 3 ],    # 3A
  [ "dec     sp", 1 ],  # 3B
  [ "inc     a", 1 ],   # 3C
  [ "dec     a", 1 ],   # 3D
  [ "ld      a,", 2 ],  # 3E
  [ "ccf", 1 ],         # 3F

  [ "ld      b,b", 1 ], # 40
  [ "ld      b,c", 1 ], # 41
  [ "ld      b,d", 1 ], # 42
  [ "ld      b,e", 1 ], # 43
  [ "ld      b,h", 1 ], # 44
  [ "ld      b,l", 1 ], # 45
  [ "ld      b,m", 1 ], # 46
  [ "ld      b,a", 1 ], # 47
  [ "ld      c,b", 1 ], # 48
  [ "ld      c,c", 1 ], # 49
  [ "ld      c,d", 1 ], # 4A
  [ "ld      c,e", 1 ], # 4B
  [ "ld      c,h", 1 ], # 4C
  [ "ld      c,l", 1 ], # 4D
  [ "ld      c,m", 1 ], # 4E
  [ "ld      c,a", 1 ], # 4F

  [ "ld      d,b", 1 ], # 50
  [ "ld      d,c", 1 ], # 51
  [ "ld      d,d", 1 ], # 52
  [ "ld      d,e", 1 ], # 53
  [ "ld      d,h", 1 ], # 54
  [ "ld      d,l", 1 ], # 55
  [ "ld      d,m", 1 ], # 56
  [ "ld      d,a", 1 ], # 57
  [ "ld      e,b", 1 ], # 58
  [ "ld      e,c", 1 ], # 59
  [ "ld      e,d", 1 ], # 5A
  [ "ld      e,e", 1 ], # 5B
  [ "ld      e,e", 1 ], # 5C
  [ "ld      e,h", 1 ], # 5D
  [ "ld      e,m", 1 ], # 5E
  [ "ld      e,a", 1 ], # 5F

  [ "ld      h,b", 1 ], # 60
  [ "ld      h,c", 1 ], # 61
  [ "ld      h,d", 1 ], # 62
  [ "ld      h,e", 1 ], # 63
  [ "ld      h,h", 1 ], # 64
  [ "ld      h,l", 1 ], # 65
  [ "ld      h,m", 1 ], # 66
  [ "ld      h,a", 1 ], # 67
  [ "ld      l,b", 1 ], # 68
  [ "ld      l,c", 1 ], # 69
  [ "ld      l,d", 1 ], # 6A
  [ "ld      l,e", 1 ], # 6B
  [ "ld      l,h", 1 ], # 6C
  [ "ld      l,l", 1 ], # 6D
  [ "ld      l,m", 1 ], # 6E
  [ "ld      l,a", 1 ], # 6F

  [ "ld      m,b", 1 ], # 70
  [ "ld      m,c", 1 ], # 71
  [ "ld      m,d", 1 ], # 72
  [ "ld      m,e", 1 ], # 73
  [ "ld      m,h", 1 ], # 74
  [ "ld      m,l", 1 ], # 75
  [ "hlt",         1 ], # 76
  [ "ld      (hl),a", 1 ], # 77
  [ "ld      a,b", 1 ], # 78
  [ "ld      a,c", 1 ], # 79
  [ "ld      a,d", 1 ], # 7A
  [ "ld      a,e", 1 ], # 7B
  [ "ld      a,h", 1 ], # 7C
  [ "ld      a,l", 1 ], # 7D
  [ "ld      a,m", 1 ], # 7E
  [ "ld      a,a", 1 ], # 7F

  [ "add     b", 1 ],   # 80
  [ "add     c", 1 ],   # 81
  [ "add     d", 1 ],   # 82
  [ "add     e", 1 ],   # 83
  [ "add     h", 1 ],   # 84
  [ "add     l", 1 ],   # 85
  [ "add     m", 1 ],   # 86
  [ "add     a", 1 ],   # 87
  [ "adc     b", 1 ],   # 88
  [ "adc     c", 1 ],   # 89
  [ "adc     d", 1 ],   # 8A
  [ "adc     e", 1 ],   # 8B
  [ "adc     h", 1 ],   # 8C
  [ "adc     l", 1 ],   # 8D
  [ "adc     m", 1 ],   # 8E
  [ "adc     a", 1 ],   # 8F

  [ "sub     b", 1 ],   # 90
  [ "sub     c", 1 ],   # 91    
  [ "sub     d", 1 ],   # 92
  [ "sub     e", 1 ],   # 93
  [ "sub     h", 1 ],   # 94
  [ "sub     l", 1 ],   # 95
  [ "sub     m", 1 ],   # 96
  [ "sub     a", 1 ],   # 97
  [ "sbb     b", 1 ],   # 98
  [ "sbb     c", 1 ],   # 99
  [ "sbb     d", 1 ],   # 9A
  [ "sbb     e", 1 ],   # 9B
  [ "sbb     h", 1 ],   # 9C
  [ "sbb     l", 1 ],   # 9D
  [ "sbb     m", 1 ],   # 9E
  [ "sbb     a", 1 ],   # 9F

  [ "ana     b", 1 ],   # A0
  [ "ana     c", 1 ],   # A1
  [ "ana     d", 1 ],   # A2
  [ "ana     e", 1 ],   # A3
  [ "ana     h", 1 ],   # A4
  [ "ana     l", 1 ],   # A5
  [ "ana     m", 1 ],   # A6
  [ "ana     a", 1 ],   # A7
  [ "xra     b", 1 ],   # A8
  [ "xra     c", 1 ],   # A9
  [ "xra     d", 1 ],   # AA
  [ "xra     e", 1 ],   # AB
  [ "xra     h", 1 ],   # AC
  [ "xra     l", 1 ],   # AD
  [ "xra     m", 1 ],   # AE
  [ "xra     a", 1 ],   # AF

  [ "ora     b", 1 ],   # B0
  [ "or      c", 1 ],   # B1
  [ "ora     d", 1 ],   # B2
  [ "ora     e", 1 ],   # B3
  [ "ora     h", 1 ],   # B4
  [ "ora     l", 1 ],   # B5
  [ "ora     m", 1 ],   # B6
  [ "ora     a", 1 ],   # B7
  [ "cmp     b", 1 ],   # B8
  [ "cmp     c", 1 ],   # B9
  [ "cmp     d", 1 ],   # BA
  [ "cmp     e", 1 ],   # BB
  [ "cmp     h", 1 ],   # BC
  [ "cmp     l", 1 ],   # BD
  [ "cmp     m", 1 ],   # BE
  [ "cmp     a", 1 ],   # BF

  [ "rnz", 1 ],         # C0
  [ "pop     b", 1 ],   # C1
  [ "jnz     ", 3 ],    # C2
  [ "jp      ", 3 ],    # C3
  [ "cnz     ", 3 ],    # C4
  [ "push    b", 1 ],   # C5
  [ "adi     ", 2 ],    # C6
  [ "rst     0", 1 ],   # C7
  [ "ret     z", 1 ],          # C8
  [ "ret", 1 ],         # C9
  [ "jz      ", 3 ],    # CA
  [ "table", 3 ],   # CB
  [ "cz      ", 3 ],    # CC
  [ "call    ", 3 ],    # CD
  [ "aci     ", 2 ],    # CE
  [ "rst     1", 1 ],   # CF

  [ "rnc", 1 ],         # D0
  [ "pop     d", 1 ],   # D1
  [ "jnc     ", 3 ],    # D2
  [ "out     ", 2 ],    # D3
  [ "cnc     ", 3 ],    # D4
  [ "push    d", 1 ],   # D5
  [ "sui     ", 2 ],    # D6
  [ "rst     2", 1 ],   # D7
  [ "rc", 1 ],          # D8
  [ "*ret", 1 ],        # D9
  [ "jc      ", 3 ],    # DA
  [ "in      ", 2 ],    # DB
  [ "cc      ", 3 ],    # DC
  [ "table", 3 ],   # DD
  [ "sbi     ", 2 ],    # DE
  [ "rst     3", 1 ],   # DF

  [ "rpo", 1 ],         # E0
  [ "pop     h", 1 ],   # E1
  [ "jpo     ", 3 ],    # E2
  [ "xthl", 1 ],        # E3
  [ "cpo     ", 3 ],    # E4
  [ "push    h", 1 ],   # E5
  [ "ani     ", 2 ],    # E6
  [ "rst     4", 1 ],   # E7
  [ "rpe", 1 ],         # E8
  [ "pchl", 1 ],        # E9
  [ "jpe     ", 3 ],    # EA
  [ "xchg", 1 ],        # EB
  [ "cpe     ", 3 ],    # EC
  [ "table", 3 ],   # ED
  [ "xri     ", 2 ],    # EE
  [ "rst     5", 1 ],   # EF

  [ "rp", 1 ],          # F0
  [ "pop     psw", 1 ], # F1
  [ "jp      ", 3 ],    # F2
  [ "di", 1 ],          # F3
  [ "cp      ", 3 ],    # F4
  [ "push    psw", 1 ], # F5
  [ "ori     ", 2 ],    # F6
  [ "rst     6", 1 ],   # F7
  [ "rm", 1 ],          # F8
  [ "sphl", 1 ],        # F9
  [ "jm      ", 3 ],    # FA
  [ "ei", 1 ],          # FB
  [ "cm      ", 3 ],    # FC
  [ "table", 3 ],   # FD
  [ "cpi     ", 2 ],    # FE
  [ "rst     38", 1 ],   # FF
]

upperOption = False

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

# Parse command line options
parser = argparse.ArgumentParser()
parser.add_argument("filename", help="Binary file to disassemble")
parser.add_argument("-n", "--nolist", help="Don't list  instruction bytes (make output suitable for assembler)", action="store_true")
parser.add_argument("-u", "--uppercase", help="Use uppercase for mnemonics", action="store_true")
parser.add_argument("-a", "--address", help="Specify decimal starting address (defaults to 0)", default=0, type=int)
parser.add_argument("-f", "--format", help="Use number format: 1 = $1234 2 = 1234h 3 = 1234 (default 1)", default=1, type=int, choices=range(1, 4))
args = parser.parse_args()

# Get filename from command line arguments.
filename = args.filename

# Current instruction address. Silently force it to be in valid range.
address = args.address & 0xffff

# Uppercase output option
upperOption = args.uppercase

# Contains a line of output
line = ""

# Open input file.
# Display error and exit if filename does not exist.
try:
    f = open(filename, "rb")
except FileNotFoundError:
    print("error: input file '" + filename + "' not found.", file=sys.stderr)
    sys.exit(1)

# Print initial origin address
if args.nolist == False:
    if args.format == 1:
        print("%04X            %s     $%04X" % (address, case("org"), address))
    elif args.format == 2:
        print("%04X            %s     %04Xh" % (address, case("org"), address))
    else:
        print("%04X            %s     %04X" % (address, case("org"), address))

while True:
    try:
        b = f.read(1) # Get binary byte from file

        if len(b) == 0:
            if args.nolist == False:
                print("%04X            %s" % (address, case("end"))) # Exit if end of file reached.
            break

        if args.nolist == False:
            line = "%04X  " % address # Print current address

        op = ord(b) # Get opcode byte

        n = lookupTable[op][1] # Look up number of instruction bytes

        mnem = case(lookupTable[op][0]) # Get mnemonic

        # Print instruction bytes
        if (n == 1):
            if args.nolist == False:
                line += "%02X        " % op
        elif (n == 2):
            try: # Possible to get exception here if EOF reached.
                op1 = ord(f.read(1))
            except TypeError:
                op1 = 0 # Fake it to recover from EOF
            if args.nolist == False:
                line += "%02X %02X     " % (op, op1)
        elif (n == 3):
            try: # Possible to get exception here if EOF reached.
                op1 = ord(f.read(1))
                op2 = ord(f.read(1))
            except TypeError:
                op1 = 0 # Fake it to recover from EOF
                op2 = 0
            if args.nolist == False:
                line += "%02X %02X %02X  " % (op, op1, op2)
        if args.nolist == True:
            line + " "

        # If opcode starts with '*' then put in comment that this is an alternative op code (likely an error).
        if mnem[0] =="*":
            alternative = True
            mnem = mnem.replace(mnem[:1], '') # Remove the star
        else:
            alternative = False

        line += mnem

        # Print any operands
        if (n == 2):
            if isprint(chr(op1)):
                line += "'%c'" % op1
            else:
                if args.format == 1:
                    line += "$%02X" % op1
                elif args.format == 2:
                    line += "%02Xh" % op1
                else:
                    line += "%02X" % op1
        elif (n == 3):
            if args.format == 1:
                line += "$%02X%02X" % (op2, op1)
            elif args.format == 2:
                line += "%02X%02Xh" % (op2, op1)
            else:
                line += "%02X%02X" % (op2, op1)

        if alternative:
            mnem = mnem.replace(mnem[:1], '') # Remove the star
            # Line up comment at fixed column position
            if args.nolist == False:
                line += ";Note: Alternative opcode used".rjust(67 - len(line))
            else:
                line += ";Note: Alternative opcode used".rjust(51 - len(line))

        # Update address
        address = address + n

        # Check for address exceeding 0xFFFF, if so wrap around.
        if (address > 0xffff):
            address = address & 0xffff

        # Finished a line of disassembly
        print(line)
        line = ""

    except KeyboardInterrupt:
        print("Interrupted by Control-C", file=sys.stderr)
        print("%04X            %s" % (address, case("end"))) # Exit if end of file reached.
        break
