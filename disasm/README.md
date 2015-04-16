This is a simple disassembler for the Intel 8080 microprocessor. It
reads a binary file specified on the command line and produces a
disassembly. It requires Python 3. It has been tested on Linux but
should work on any platform that supports Python. See the source code
for more details.

Also available is a version that disassembles code for the Z80
microprocessor. It is not yet complete.

Finally, there is a version for the 6502 microprocessor.

The files ex1.bin, ex2.bin, and ex3.bin are some example binary files
that can be used to test the disassembler.

usage: disasm8080.py [-h] [-n] [-u] [-a ADDRESS] [-f {1,2,3,4}] filename

positional arguments:
  filename              Binary file to disassemble

optional arguments:
  -h, --help            show this help message and exit
  -n, --nolist          Don't list instruction bytes (make output suitable for
                        assembler)
  -u, --uppercase       Use uppercase for mnemonics
  -a ADDRESS, --address ADDRESS
                        Specify decimal starting address (defaults to 0)
  -f {1,2,3,4}, --format {1,2,3,4}
                        Use number format: 1=$1234 2=1234h 3=1234 4=177777
                        (default 1)
