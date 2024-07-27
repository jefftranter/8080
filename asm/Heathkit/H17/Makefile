all: h17.hex h17.bin

h17.hex: h17.p
	p2hex -F Intel h17.p

h17.bin: h17.p
	p2bin -r '$$-$$' h17.p

h17.p: h17.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . h17.asm

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex
