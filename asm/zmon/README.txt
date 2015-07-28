Zmon is a small machine language monitor program for Intel 8080
computers from Appendix B of the book "The Microcomputer Builder's
Bible" by Chris Johnston.

This version is based on the original listing in the book, adapted to
build with the AS macro assembler and run on the Briel Altair 8800
computer.

I;ve omitted the comments that simply consisted of hex addresses
because they do not match the assembled code (even the published one).

Commands:

<address>           Examine memory location
<address> <data>    Write data to address
<space>             Display next address
M                   Display memory in hex
B                   Boot the operating system
