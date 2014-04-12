This is the source and binaries for TUTILS, a set of utilities for
transferring files between an SD Card and CP/M disk images running on
the Briel Altair 8800 with an 88-DSK RAM Drive card. The TUTILS were
written by Garry Jordan.

For more information on the Briel Altair 8800, see
http://brielcomputers.com

The images/ directory contains the CP/M disk images and some
documentation files. The source/ directory contains the files
contained in the disk images, including all TUTILS source files and
tools needed to build them.

To run them, copy both DSK files to an SD card. Then, on the Briel
Altair 8800, transfer REDCPM3.DSK to drive A: and TUTILSRC.DSK to
drive B:. Then boot up CP/M.

I have made a few minor changes to the source files.

---

Contents of the disk images:

A>dir a:
A: CPM3     SYS : TDEL     COM : MAC      COM : DEVICE   COM : TDIR     COM 
A: TDSKDIR  COM : PROFILE  SUB : DSKPARMS COM : FORMAT   COM : RENAME   COM 
A: TREAD    COM : DATE     COM : SUBMIT   COM : INITDIR  COM : TWRITE   COM 
A: TYPE     COM : SET      COM : W        COM : HELP     COM : COPYSYS  COM 
A: CLS      COM : ERASE    COM : SETDEF   COM : PIP      COM : R        COM 
A: SHOW     COM : HELP     HLP : SID      COM : LS       COM : MBASIC   COM 
A: DUMP     COM : RED      COM : RM       COM : TAIL     COM : CP       COM 
A: HEXCOM   COM 
SYSTEM FILE(S) EXIST

A>dir b:
B: CC       COM : CC2      COM : CLINK    COM : RHG      SUB : L2       COM 
B: C        CCC : CLIB     COM : TWRITE   C   : DEFF2    CRL : STDIO    H   
B: TWRITE   COM : DEFF     CRL : WCTEST   C   : PROPIO   CSM : TUTILS   H   
B: CASM     COM : TTEST    C   : WILDEXP  C   : DIRENT   COM : CLOAD    COM 
B: TDEL     COM : CSM      SUB : TDEL     CRL : BDS      LIB : WSD      SUB 
B: RSD      SUB : CP       C   : RMT      SUB : TDIR     C   : TCOMMON  CRL 
B: TDSKDIR  C   : TCOMMON  C   : BLDTLIBS SUB : TREAD    C   : TDEL     C   
B: AWHG     SUB : DSKPARMS C   : WILDEXP  CRL : PROPIO   CRL : CPT      SUB 
B: TDIR     COM : TDIR     CRL : BLDTPRGS SUB : BLDTALL  SUB : C        SUB 
B: DIRENT   C   : PROPIO   SYM : L        SUB : WHG      SUB : TDSKDIR  COM 
B: BLDTEST  SUB : DSKPARMS COM : DIRENT   CRL : DSKPARMS CRL : TDSKDIR  CRL 
B: TREAD    COM : CTC      SUB : TREAD    CRL : TWRITE   CRL : RM       C   

---

To build everything from source, run the batch file BLDTALL. This will
take a while (about 10 minutes): The output of a sample run is listed
below.

A>b:
B>submit bldtall

B>;Build all T* link libraries.
B>submit bldtlibs

B>;Build the T* utility C source link libraries.
B>cc tcommon.c -w -x
BD Software C Compiler v1.60  (part I)
  17K elbowroom
BD Software C Compiler v1.60 (part II)
  19K to spare
B>cc wildexp.c -w -x
BD Software C Compiler v1.60  (part I)
  25K elbowroom
BD Software C Compiler v1.60 (part II)
  23K to spare
B>;Use CASM to build the PROPIO assembler link library.
B>submit csm propio

B>casm PROPIO
BD Software CRL-format ASM Preprocessor v1.60
Processing the PCONSTAT function...          Processing the PPUTCHAR function...          Processing the PGETCHAR function...          Processing the PFILETOSD function...          Processing the PSDTOFILE function...          Processing the PSDTOMEM function...          Processing the ALTSLEEP function...          PROPIO.ASM is ready to be assembled.

B>mac PROPIO
CP/M MACRO ASSEM 2.0
0305
018H USE FACTOR
END OF ASSEMBLY

B>era PROPIO.asm
B>era PROPIO.prn
B>cload PROPIO
CASM Image Hex-to-Crl Converter -- v1.6
Copyright (c) 1983 William C. Colley III

PROPIO.CRL successfully generated.

B>era PROPIO.hex
B>;Build all T* utility programs.
B>submit c tdel

B>;Compile and link a single target file with the TUTIL libraries.
B>;b:
B>era progerrs.$$$
No File
B>era TDEL.log
No File
B>cc TDEL.c -w -x
BD Software C Compiler v1.60  (part I)
  22K elbowroom
BD Software C Compiler v1.60 (part II)
  23K to spare
B>l2 TDEL -n -l tcommon propio wildexp
L2 Linker v3.0
Loading  TDEL.CRL
Scanning TCOMMON.CRL
Scanning PROPIO.CRL
Scanning WILDEXP.CRL
Scanning DEFF.CRL
Scanning DEFF2.CRL


Link statistics:
  Number of functions: 34
  Code ends at: 0x2254
  Externals begin at: 0x2254
  Externals end at: 0x225A
  End of current TPA: 0xCD06
  Jump table bytes saved: 0xE1
  Link space remaining: 13K

B>submit c tdir

B>;Compile and link a single target file with the TUTIL libraries.
B>;b:
B>era progerrs.$$$
No File
B>era TDIR.log
No File
B>cc TDIR.c -w -x
BD Software C Compiler v1.60  (part I)
  21K elbowroom
BD Software C Compiler v1.60 (part II)
  22K to spare
B>l2 TDIR -n -l tcommon propio wildexp
L2 Linker v3.0
Loading  TDIR.CRL
Scanning TCOMMON.CRL
Scanning PROPIO.CRL
Scanning WILDEXP.CRL
Scanning DEFF.CRL
Scanning DEFF2.CRL


Link statistics:
  Number of functions: 42
  Code ends at: 0x28BC
  Externals begin at: 0x28BC
  Externals end at: 0x28C4
  End of current TPA: 0xCD06
  Jump table bytes saved: 0x114
  Link space remaining: 12K

B>submit c tdskdir

B>;Compile and link a single target file with the TUTIL libraries.
B>;b:
B>era progerrs.$$$
No File
B>era TDSKDIR.log
No File
B>cc TDSKDIR.c -w -x
BD Software C Compiler v1.60  (part I)
  17K elbowroom
BD Software C Compiler v1.60 (part II)
  20K to spare
B>l2 TDSKDIR -n -l tcommon propio wildexp
L2 Linker v3.0
Loading  TDSKDIR.CRL
Scanning TCOMMON.CRL
Scanning PROPIO.CRL
Scanning WILDEXP.CRL
Scanning DEFF.CRL
Scanning DEFF2.CRL


Link statistics:
  Number of functions: 34
  Code ends at: 0x263C
  Externals begin at: 0x263C
  Externals end at: 0x2644
  End of current TPA: 0xCD06
  Jump table bytes saved: 0xDE
  Link space remaining: 12K

B>submit c tread

B>;Compile and link a single target file with the TUTIL libraries.
B>;b:
B>era progerrs.$$$
No File
B>era TREAD.log
No File
B>cc TREAD.c -w -x
BD Software C Compiler v1.60  (part I)
  19K elbowroom
BD Software C Compiler v1.60 (part II)
  20K to spare
B>l2 TREAD -n -l tcommon propio wildexp
L2 Linker v3.0
Loading  TREAD.CRL
Scanning TCOMMON.CRL
Scanning PROPIO.CRL
Scanning WILDEXP.CRL
Scanning DEFF.CRL
Scanning DEFF2.CRL


Link statistics:
  Number of functions: 46
  Code ends at: 0x2F58
  Externals begin at: 0x2F58
  Externals end at: 0x2F5E
  End of current TPA: 0xCD06
  Jump table bytes saved: 0x129
  Link space remaining: 10K

B>submit c twrite

B>;Compile and link a single target file with the TUTIL libraries.
B>;b:
B>era progerrs.$$$
No File
B>era TWRITE.log
No File
B>cc TWRITE.c -w -x
BD Software C Compiler v1.60  (part I)
  21K elbowroom
BD Software C Compiler v1.60 (part II)
  22K to spare
B>l2 TWRITE -n -l tcommon propio wildexp
L2 Linker v3.0
Loading  TWRITE.CRL
Scanning TCOMMON.CRL
Scanning PROPIO.CRL
Scanning WILDEXP.CRL
Scanning DEFF.CRL
Scanning DEFF2.CRL


Link statistics:
  Number of functions: 50
  Code ends at: 0x33F2
  Externals begin at: 0x33F2
  Externals end at: 0x33F8
  End of current TPA: 0xD506
  Jump table bytes saved: 0x126
  Link space remaining: 11K

B>
