This is a simple machine language monitor program for Intel 8080
microprocessor that appeared in the book "8080/Z80 Assembly Language
Techniqes for Improved Programming" by Alan R. Miller. The code appears
in chapter six.

I have ported it run on the Briel Altair 8800.

The code is written to be assembled using the AS macro assembler. See
http://john.ccac.rwth-aachen.de:8000/as/index.html

A make file is provided to build the software. You may need to adjust
it for your environment.

The program can be loaded in several ways:

1. Toggle in the binary code using the front panel.
2. Load the .BIN file from the SD card.
3. Copy the .HEX file to CP/M, run LOAD on it to generate a .COM file.
   and run the COM file.

Jeff Tranter <tranter@pobox.com>
