all:	bin hex

bin:	prog1.bin prog2.bin prog3.bin prog4.bin prog5.bin

hex:	prog1.hex prog2.hex prog3.hex prog4.hex prog5.hex

%.hex:	%.p
	p2hex -F Intel $*.p

%.bin:	%.p
	p2bin -r '$$-$$' $*.p

%.p:    %.asm
	asl -L $*.asm

clean:
	$(RM) *.lst *.p *.bin *.hex
