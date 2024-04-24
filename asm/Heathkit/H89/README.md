This is the source code for the Heathkit H89 monitor ROM (MTR89). It
was based on the published source code with minimal changes to adapt
it to the ASL assembler. The listing is in split octal, like the
original Heathkit assembler listing.

I have confirmed that the output matches the published binary for the
ROM.

Some notes on porting to the ASL assembler:

The code runs on a Z80, but Heathkit used an 8080 assembler. They
occasionally used Z80 instructions and entered then using DC/DW
directives. I've converted these to Z80 instructions by changing the
CPU type on the fly.

The original code used many split octal constants (suffixed by A).
This is not supported by ASL and they have been converted to octal
(suffix Q).

Macros, like ERRNZ, for assembly time assertion checks could not be
implemented with ASM so I have created macro definitions that simply
do nothing.

Unused code was filled with zeroes to match the original ROM binaries.

A few obvious spelling and grammatical errors in comments have been
corrected.

References:

http://www.bitsavers.org/pdf/zenith/z89/595-2508_MTR-89_Monitor_1980.pdf

http://john.ccac.rwth-aachen.de:8000/as/
