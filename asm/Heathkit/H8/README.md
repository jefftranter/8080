This is the source code for the Heathkit H8 monitor ROM (PAM-8). It
was based on the published source code with minimal changes to adapt
it to the ASL assembler. The listing is in split octal, like the
original Heathkit assembler listing.

I have confirmed that the output matches the published binary for the
ROM.

Some notes on porting to the ASL assembler:

The original code used many split octal constants (suffixed by A).
This is not supported by ASL and they have been converted to octal
(suffix Q).

Unused code was filled with zeroes to match the original ROM binaries.

A few obvious spelling and grammatical errors in comments have been
corrected.

References:

https://heathkit.garlanger.com/software/OSes/HDOS/2.0/Manual/595-2348_Volume-2_Chapter-2_FrontPanelMonitor_PAM-8.pdf

https://sebhc.github.io/sebhc/documentation/software/roms/Heath_595-2348_PAM-8_Listing.pdf

http://john.ccac.rwth-aachen.de:8000/as/
