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
  [ "nop", 1 ],            # 00
  [ "ld      bc,", 3 ],    # 01
  [ "ld      (bc),a", 1 ], # 02
  [ "inc     bc", 1 ],     # 03
  [ "inc     b", 1 ],      # 04
  [ "dec     b", 1 ],      # 05
  [ "ld      b,", 2 ],     # 06
  [ "rlca",      1 ],      # 07
  [ "ex      af,af", 1 ],  # 08
  [ "add     hl,bc", 1 ],  # 09
  [ "ld      a,(bc)", 1 ], #  0A
  [ "dec     bc", 1 ],     #  0B
  [ "inc     c", 1 ],      # 0C
  [ "dec     c", 1 ],      # 0D
  [ "ld      c,", 2 ],     # 0E
  [ "rrca",       1 ],     # 0F

  [ "djnz    ", 1 ],       # 10
  [ "ld      de,", 3 ],    # 11
  [ "ld      (de),a", 1 ], # 12
  [ "inc     de", 1 ],     # 13
  [ "inc     d", 1 ],      # 14
  [ "dec     d", 1 ],      # 15
  [ "ld      d,", 2 ],     # 16
  [ "rla", 1 ],            # 17
  [ "jr      ", 1 ],       # 18
  [ "add     hl,de", 1 ],  # 19
  [ "ld      a,(de)", 1 ], # 1A
  [ "dec     de", 1 ],     # 1B
  [ "inc     e", 1 ],      # 1C
  [ "dec     e", 1 ],      # 1D
  [ "ld      e,", 2 ],     # 1E
  [ "rra", 1 ],            # 1F

  [ "jr      nz,", 1 ],    # 20
  [ "ld      hl,", 3 ],    # 21
  [ "ld      (", 3 ],      # 22 then append "),hl"
  [ "inc     hl", 1 ],     # 23
  [ "inc     h", 1 ],      # 24
  [ "dec     h", 1 ],      # 25
  [ "ld      h,", 2 ],     # 26
  [ "daa", 1 ],            # 27
  [ "jr      z,", 1 ],     # 28
  [ "add     hl,jl", 1 ],  # 29
  [ "ld      hl,(", 3 ],   # 2A then append ")"
  [ "dec     hl", 1 ],     # 2B
  [ "inc     l", 1 ],      # 2C
  [ "dec     l", 1 ],      # 2D
  [ "ld      l,", 2 ],     # 2E
  [ "cpl", 1 ],            # 2F

  [ "jr      nc,", 2 ],    # 30
  [ "ld      sp,", 3 ],    # 31
  [ "ld      (", 3 ],      # 32 then append "),a"
  [ "inc     sp", 1 ],     # 33
  [ "inc     (hl)", 1 ],   # 34
  [ "dec     (hl)", 1 ],   # 35
  [ "ld      (hl),", 2 ],  # 36
  [ "scf", 1 ],            # 37
  [ "jr      c,", 1 ],     # 38
  [ "add     hl,sp", 1 ],  # 39
  [ "ld      a,(", 3 ],    # 3A then append ")"
  [ "dec     sp", 1 ],     # 3B
  [ "inc     a", 1 ],      # 3C
  [ "dec     a", 1 ],      # 3D
  [ "ld      a,", 2 ],     # 3E
  [ "ccf", 1 ],            # 3F

  [ "ld      b,b", 1 ],    # 40
  [ "ld      b,c", 1 ],    # 41
  [ "ld      b,d", 1 ],    # 42
  [ "ld      b,e", 1 ],    # 43
  [ "ld      b,h", 1 ],    # 44
  [ "ld      b,l", 1 ],    # 45
  [ "ld      b,(hl)", 1 ], # 46
  [ "ld      b,a", 1 ],    # 47
  [ "ld      c,b", 1 ],    # 48
  [ "ld      c,c", 1 ],    # 49
  [ "ld      c,d", 1 ],    # 4A
  [ "ld      c,e", 1 ],    # 4B
  [ "ld      c,h", 1 ],    # 4C
  [ "ld      c,l", 1 ],    # 4D
  [ "ld      c,(hl)", 1 ], # 4E
  [ "ld      c,a", 1 ],    # 4F

  [ "ld      d,b", 1 ],    # 50
  [ "ld      d,c", 1 ],    # 51
  [ "ld      d,d", 1 ],    # 52
  [ "ld      d,e", 1 ],    # 53
  [ "ld      d,h", 1 ],    # 54
  [ "ld      d,l", 1 ],    # 55
  [ "ld      d,(hl)", 1 ], # 56
  [ "ld      d,a", 1 ],    # 57
  [ "ld      e,b", 1 ],    # 58
  [ "ld      e,c", 1 ],    # 59
  [ "ld      e,d", 1 ],    # 5A
  [ "ld      e,e", 1 ],    # 5B
  [ "ld      e,h", 1 ],    # 5C
  [ "ld      e,l", 1 ],    # 5D
  [ "ld      e,(hl)", 1 ], # 5E
  [ "ld      e,a", 1 ],    # 5F

  [ "ld      h,b", 1 ],    # 60
  [ "ld      h,c", 1 ],    # 61
  [ "ld      h,d", 1 ],    # 62
  [ "ld      h,e", 1 ],    # 63
  [ "ld      h,h", 1 ],    # 64
  [ "ld      h,l", 1 ],    # 65
  [ "ld      h,(hl)", 1 ], # 66
  [ "ld      h,a", 1 ],    # 67
  [ "ld      l,b", 1 ],    # 68
  [ "ld      l,c", 1 ],    # 69
  [ "ld      l,d", 1 ],    # 6A
  [ "ld      l,e", 1 ],    # 6B
  [ "ld      l,h", 1 ],    # 6C
  [ "ld      l,l", 1 ],    # 6D
  [ "ld      l,(hl)", 1 ], # 6E
  [ "ld      l,a", 1 ],    # 6F

  [ "ld      (hl),b", 1 ], # 70
  [ "ld      (hl),c", 1 ], # 71
  [ "ld      (hl),d", 1 ], # 72
  [ "ld      (hl),e", 1 ], # 73
  [ "ld      (hl),h", 1 ], # 74
  [ "ld      (hl),l", 1 ], # 75
  [ "halt",           1 ], # 76
  [ "ld      (hl),a", 1 ], # 77
  [ "ld      a,b", 1 ],    # 78
  [ "ld      a,c", 1 ],    # 79
  [ "ld      a,d", 1 ],    # 7A
  [ "ld      a,e", 1 ],    # 7B
  [ "ld      a,h", 1 ],    # 7C
  [ "ld      a,l", 1 ],    # 7D
  [ "ld      a,(hl)", 1 ], # 7E
  [ "ld      a,a", 1 ],    # 7F

  [ "add     a,b", 1 ],    # 80
  [ "add     a,c", 1 ],    # 81
  [ "add     a,d", 1 ],    # 82
  [ "add     a,ee", 1 ],   # 83
  [ "add     a,h", 1 ],    # 84
  [ "add     a,l", 1 ],    # 85
  [ "add     a,(hl)", 1 ], # 86
  [ "add     a,a", 1 ],    # 87
  [ "adc     a,b", 1 ],    # 88
  [ "adc     a,c", 1 ],    # 89
  [ "adc     a,d", 1 ],    # 8A
  [ "adc     a,e", 1 ],    # 8B
  [ "adc     a,h", 1 ],    # 8C
  [ "adc     a,l", 1 ],    # 8D
  [ "adc     a,(hl)", 1 ], # 8E
  [ "adc     a,a", 1 ],    # 8F

  [ "sub     b", 1 ],      # 90
  [ "sub     c", 1 ],      # 91    
  [ "sub     d", 1 ],      # 92
  [ "sub     e", 1 ],      # 93
  [ "sub     h", 1 ],      # 94
  [ "sub     l", 1 ],      # 95
  [ "sub     (hl)", 1 ],   # 96
  [ "sub     a", 1 ],      # 97
  [ "sbc     a,b", 1 ],    # 98
  [ "sbc     a,c", 1 ],    # 99
  [ "sbc     a,d", 1 ],    # 9A
  [ "sbc     a,e", 1 ],    # 9B
  [ "sbc     a,h", 1 ],    # 9C
  [ "sbc     a,l", 1 ],    # 9D
  [ "sbc     a,(hl)", 1 ], # 9E
  [ "sbc     a,a", 1 ],    # 9F

  [ "and     b", 1 ],      # A0
  [ "and     c", 1 ],      # A1
  [ "and     d", 1 ],      # A2
  [ "and     e", 1 ],      # A3
  [ "and     h", 1 ],      # A4
  [ "and     l", 1 ],      # A5
  [ "and     (hl)", 1 ],   # A6
  [ "and     a", 1 ],      # A7
  [ "xor     b", 1 ],      # A8
  [ "xor     c", 1 ],      # A9
  [ "xor     d", 1 ],      # AA
  [ "xor     e", 1 ],      # AB
  [ "xor     h", 1 ],      # AC
  [ "xor     l", 1 ],      # AD
  [ "xor     (hl)", 1 ],   # AE
  [ "xor     a", 1 ],      # AF

  [ "or      b", 1 ],      # B0
  [ "or      c", 1 ],      # B1
  [ "or      d", 1 ],      # B2
  [ "or      e", 1 ],      # B3
  [ "or      h", 1 ],      # B4
  [ "or      l", 1 ],      # B5
  [ "or      (hl)", 1 ],   # B6
  [ "or      a", 1 ],      # B7
  [ "cp      b", 1 ],      # B8
  [ "cp      c", 1 ],      # B9
  [ "cp      d", 1 ],      # BA
  [ "cp      e", 1 ],      # BB
  [ "cp      h", 1 ],      # BC
  [ "cp      l", 1 ],      # BD
  [ "cp      (hl)", 1 ],   # BE
  [ "cp      a", 1 ],      # BF

  [ "ret     nz", 1 ],     # C0
  [ "pop     bc", 1 ],     # C1
  [ "jp      nz,", 3 ],    # C2
  [ "jp      ", 3 ],       # C3
  [ "call    nz,", 3 ],    # C4
  [ "push    bc", 1 ],     # C5
  [ "ada     a,", 2 ],     # C6
  [ "rst     00", 1 ],     # C7
  [ "ret     z", 1 ],      # C8
  [ "ret", 1 ],            # C9
  [ "jp      z,", 3 ],     # CA
  [ "prefix", 2 ],         # CB
  [ "call    z,", 3 ],     # CC
  [ "call    ", 3 ],       # CD
  [ "adc     a,", 2 ],      # CE
  [ "rst     08", 1 ],     # CF

  [ "ret     nc", 1 ],     # D0
  [ "pop     de", 1 ],     # D1
  [ "jp      nc,", 3 ],    # D2
  [ "out     (", 2 ],      # D3 then append "),a"
  [ "call    nc,", 3 ],    # D4
  [ "push    de", 1 ],     # D5
  [ "sub     ", 2 ],       # D6
  [ "rst     10", 1 ],     # D7
  [ "ret     c", 1 ],      # D8
  [ "exx", 1 ],            # D9
  [ "jp      c,", 3 ],     # DA
  [ "in      a,(", 2 ],    # DB then append ")"
  [ "call    c,", 3 ],     # DC
  [ "prefix", 2 ],         # DD
  [ "sbc     a,", 2 ],     # DE
  [ "rst     18", 1 ],     # DF

  [ "ret     po", 1 ],     # E0
  [ "pop     hl", 1 ],     # E1
  [ "jp      po,", 3 ],    # E2
  [ "ex      (sp),hl", 1 ],# E3
  [ "call    po,", 3 ],    # E4
  [ "push    hl", 1 ],     # E5
  [ "and     ", 2 ],       # E6
  [ "rst     20", 1 ],     # E7
  [ "ret     pe", 1 ],     # E8
  [ "jp      (hl)", 1 ],   # E9
  [ "jp      pe,", 3 ],    # EA
  [ "ex      de,hl", 1 ],  # EB
  [ "call    pe,", 3 ],    # EC
  [ "prefix", 2 ],         # ED
  [ "xor     ", 2 ],       # EE
  [ "rst     28", 1 ],     # EF

  [ "ret     p", 1 ],      # F0
  [ "pop     af", 1 ],     # F1
  [ "jp      p,", 3 ],     # F2
  [ "di", 1 ],             # F3
  [ "call    p,", 3 ],     # F4
  [ "push    af", 1 ],     # F5
  [ "or      ", 2 ],       # F6
  [ "rst     30", 1 ],     # F7
  [ "ret     m", 1 ],      # F8
  [ "ld      sp,phl", 1 ], # F9
  [ "jp      m,", 3 ],     # FA
  [ "ei", 1 ],             # FB
  [ "call    m,", 3 ],     # FC
  [ "prefix", 2 ],         # FD
  [ "cp      ", 2 ],       # FE
  [ "rst     38", 1 ],     # FF
]

# Lookup table for multibyte instructions starting with 0xCB
lookupTableCB = [
  [ "rlc     b", 2 ],      # 00
  [ "rlc     c", 2 ],      # 01
  [ "rlc     d", 2 ],      # 02
  [ "rlc     e", 2 ],      # 03
  [ "rlc     h", 2 ],      # 04
  [ "rlc     l", 2 ],      # 05
  [ "rlc     (hl)", 2 ],   # 06
  [ "rlc     a", 2 ],      # 07
  [ "rrc     b", 2 ],      # 08
  [ "rrc     c", 2 ],      # 09
  [ "rrc     d", 2 ],      # 0A
  [ "rrc     e", 2 ],      # 0B
  [ "rrc     h", 2 ],      # 0C
  [ "rrc     l", 2 ],      # 0D
  [ "rrc     (hl)", 2 ],   # 0E
  [ "rrc     a", 2 ],      # 0F

  [ "rl      b", 2 ],      # 10
  [ "rl      c", 2 ],      # 11
  [ "rl      d", 2 ],      # 12
  [ "rl      e", 2 ],      # 13
  [ "rl      h", 2 ],      # 14
  [ "rl      l", 2 ],      # 15
  [ "rl      (hl)", 2 ],   # 16
  [ "rl      a", 2 ],      # 17
  [ "rr      b", 2 ],      # 18
  [ "rr      c", 2 ],      # 19
  [ "rr      d", 2 ],      # 1A
  [ "rr      e", 2 ],      # 1B
  [ "rr      h", 2 ],      # 1C
  [ "rr      l", 2 ],      # 1D
  [ "rr      (hl)", 2 ],   # 1E
  [ "rr      e", 2 ],      # 1F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # 2F

  [ "", 2 ],      # 20
  [ "", 2 ],      # 21
  [ "", 2 ],      # 22
  [ "", 2 ],      # 23
  [ "", 2 ],      # 24
  [ "", 2 ],      # 25
  [ "", 2 ],      # 26
  [ "", 2 ],      # 27
  [ "", 2 ],      # 28
  [ "", 2 ],      # 29
  [ "", 2 ],      # 2A
  [ "", 2 ],      # 2B
  [ "", 2 ],      # 2C
  [ "", 2 ],      # 2D
  [ "", 2 ],      # 2E
  [ "", 2 ],      # FF
]

# Lookup table for multibyte instructions starting with 0xED
# Note that first index is 0x40, not 0x00
lookupTableED = [
  [ "in      b,(c)", 2 ],      # 40
]

# Lookup table for multibyte instructions starting with 0xDD
lookupTableDD = [
]

# Lookup table for multibyte instructions starting with 0xFD
lookupTableFD = [
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

        # Handle getting length of multi-byte opcodes (listed in table as "prefix").
        if (mnem == "prefix"):
            assert(op in [0xcb, 0xed, 0xdd, 0xfd])
            if (op == 0xcb):
                n = lookupTableCB[op][1] # Look up number of instruction bytes
#            elif (op == 0xed):
#                n = lookupTableED[op][1]
#            elif (op == 0xdd):
#                n = lookupTableDD[op][1]
#            elif (op == 0xfd):
#                n = lookupTableDD[op][1]

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

        # Handle mnemonic of multi-byte opcodes (listed in table as "prefix").
        if mnem == "prefix":
            if (op == 0xcb):
                mnem = case(lookupTableCB[op1][0]) # Get mnemonic

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

        # Handle opcodes that are special cases that need additional characters appended at the end
        if (op == 0x22):
            line += "),hl"
        elif (op in [0x2a, 0x3a, 0xdb]):
            line += ")"
        elif (op in [0x32, 0xd3]):
            line += "),a"

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
