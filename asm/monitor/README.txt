This is a simple machine language monitor program for Intel 8080
microprocessor that appeared in the book "8080/Z80 Assembly Language
Techniques for Improved Programming" by Alan R. Miller. The code
appears in chapter six.

I have confirmed that it runs on the Briel Altair 8800.

The code is written to be assembled using the AS macro assembler. See
http://john.ccac.rwth-aachen.de:8000/as/index.html

A make file is provided to build the software. You may need to adjust
it for your environment.

The program can be loaded in several ways:

1. Toggle in the binary code using the front panel.
2. Load the .BIN file from the SD card.
3. Copy the .HEX file to CP/M, run LOAD on it to generate a .COM file.
   and run the COM file.

The start address is 5800.

The prompt, e.g. "57>" shows the high byte of the stack address.

Summary of commands:

AD<start> <end>        Dump memory in ASCII from <start> to <end>.

AL<address>            Load ASCII data to <address>. End with Control-X.

AS<start> <end> <chars>
                       Search for ASCII data consisting of one or two
                       characters, from <start> through <end> address.

C<address>             Call routine at <address> (should end in RET instruction).

D<start> <end>         Dump memory in hex and ASCII from <start> to <end>.

F<start> <end> <data>  Fill memory from <start> to <end> with byte <data> (hex or
                       ASCII preceded by "'".

G<address>             Go to routine at <address>.

H<address1> <address2> Hex math: report sum and difference of two hex addresses.

I<port>                Input (read) and display data from i/o port <port>.

J<start> <end>         Perform memory test from <start> to <end> address.

L<address>             Load or edit memory from <address>. Enter new data in hex,
                       ASCII preceded by "'", or <Return> to advance.
                       <Control>-X will return to prompt.

M<start> <end> <new>   Move (copy) memory from <start> through <end> address to <new>
                       start address.

O<port> <data>         Output (send) data to i/o port <port>.

R<start> <end> <from> <to>
                       Replace every occurrence of hex byte <from> to <to>, in range
                       of addresses <start> through <end>.

S<start> <end> <byte> <byte>
                       Search for byte or word size data from start through end address.

V<start> <end> <to>    Compare (verify) data between <start> and <end> address with data
                       starting at <to> address.

X                      Show current stack pointer address.

Z<start> <end>         Write zeroes to memory from addresses <start> through <end>.

Keys:

<Control>-Q  - resume output.
<Control>-S  - suspend output.
<Control>-X  - abort, jump to monitor warm start.
<Delete>     - delete last character entered.

Jeff Tranter <tranter@pobox.com>
