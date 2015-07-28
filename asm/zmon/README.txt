Zmon is a small machine language monitor program for Intel 8080
computers from Appendix B of the book "The Microcomputer Builder's
Bible" by Chris Johnston.

This version is based on the original listing in the book, adapted to
build with the AS macro assembler and run on the Briel Altair 8800
computer.

I've omitted the comments that simply consisted of hex addresses
because they do not match the assembled code (even the published one).

zmon-orig.asm is the original listing for the author's homebrew Z-80
computer system.

zmon.asm is a port to the Briel Altair 8800 computer.

Commands:

E       Examine memory
M       Dump memory in hex

In examine memory mode:

<address>           Examine memory location
<address> <data>    Write data to address
<space>             Display next address
R                   Run from current address
