This is the source code for the Heathkit H17 floppy ROM. It was based
on the published source code with minimal changes to adapt it to the
ASL assembler. The listing is in split octal, like the original
Heathkit assembler listing.

I have confirmed that the output matches the published binary for the
ROM.

Some notes on porting to the ASL assembler:

Symbols starting with "." have had it removed and symbols starting
with "$" use a "D" instead in order to build under the ASL assembler.

The original code used many split octal constants (suffixed by A).
This is not supported by ASL and they have been converted to octal
(suffix Q).

Assembly time checking using macros like ERRNZ have been omitted
as some could not be supported by the ASL assembler.

Some source code that was not included in the published listing has
been reverse-engineered.

Unused code was filled with zeroes to match the original ROM binaries.

A few obvious spelling and grammatical errors in comments have been
corrected.

References:

https://sebhc.github.io/sebhc/software/Roms/SYDD/HOS1SL_2_07_SYDD.PDF

http://john.ccac.rwth-aachen.de:8000/as/
