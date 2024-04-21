This is the source code for the Heathkit H88/H89 monitor ROM (MTR88). It
was based on the published source code with minimal changes to adapt
it to the ASL assembler. The listing is in split octal, like the
original Heathkit assembler listing.

Some source code (for the memory test and and floppy disk rotational
speed test) was not published in the Heathkit manuals. It has been
reverse engineered in this code from the Heathkit ROMs.

I have confirmed that the output matches the published binary for the
ROM.

The following text taken from the Heathkit manual may be helpful in
understanding the code:

"This appendix contains a listing of MTR-88. MTR-88 resides in the low
2K (2048) bytes of the H88 or H89 computer's memory. It contains all
the control for primitive keyboard input and screen output as ell as
cassette tape load and dump facilities. MTR-88 needs RAM locations
available in locations 40.000 through 40.077, and it also needs 80
bytes of stack area in high memory.

The first few pages of the listing show definitions that are used. The
last portion of the listing contains references to the symbols that
are used in MTR-88. Just before this cross-reference listing is the
definition of RAM locations in 40.000 through 40.077.

Note that most of the PAM-8 entry points are preserved in MTR-88.
(PAM-8 is the equivalent of MTR-88 on the H8 computer.) This was done
to allow compatibility between H8 and H88 programs. Of course, H8
front panel routines will not operate, but they will return properly.

Because PAM-8 entry points have been preserved, the MTR-88 code has to
jump around in a somewhat arbitrary manner. Also, the Memory Test and
Floppy Disk Rotational Speed Test routines are scattered throughout
memory. The listings of these two routines are not shown. The Memory
Test entry point is 7.375 and the Floppy Speed Test entry point is
7.372."

References:

https://sebhc.github.io/sebhc/documentation/software/roms/Heath_595-2349_MTR-88_Listing.pdf

https://heathkit.garlanger.com/software/OSes/HDOS/2.0/Manual/595-2349_Volume-2_Chapter-3_Monitor_MTR-88.pdf

http://john.ccac.rwth-aachen.de:8000/as/
