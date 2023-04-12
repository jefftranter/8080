all:	bin hex

bin:	prog1.bin prog2.bin prog3.bin prog4.bin prog5.bin prog6.bin prog7.bin prog8.bin prog9.bin

hex:	prog1.hex prog2.hex prog3.hex prog4.hex prog5.hex prog6.hex prog7.hex prog8.hex prog9.hex

%.hex:	%.p
	p2hex -F Intel $*.p

%.bin:	%.p
	p2bin -r '$$-$$' $*.p

%.p:    %.asm
	asl -L $*.asm

clean:
	$(RM) *.lst *.p *.bin *.hex
