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

A make file is provided to build the software. You may need to adjust
it for your environment.

The program can be run by copying the file monitor.bin to the SD card
then loading and running it. The start address is 0000.

Commands:

M  Dump memory in hex. Prompts for start and end addresses. Pressing
   any key while printing will cancel the dump. Will stop after 24
   lines of output, pressing space will display another page.

E  Examine memory. Prompts for an address then displays the address and
   its contents. At that point you can enter data to write to the
   address, press space to display the next address, or any key to
   leave examine mode. R will execute (run) at the current address.

Sample output:

Z-MON  V5L3
:M
>1000 TO 100F
1000 FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF 
:M
>0000 TO 0020
0000 C3 0F 00 C3 C3 01 C3 B8 01 C3 E8 01 C3 0F 00 31 
0010 00 70 CD DF 01 E5 21 F9 01 CD CF 01 E1 DB 11 CD 
0020 81 
:E
>1000
1000:FF:00 00
1001:FF:01 01
1002:FF: 
1003:FF: 
1004:FF: 
1005:FF:AA AA
1006:FF:
:X?
:E
>0000
0000:C3:R
MON  V5L3
:
