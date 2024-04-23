all: h19.hex h19.bin

h19.hex: h19.p
	p2hex -F Intel h19.p

h19.bin: h19.p
	p2bin -r '$$-$$' h19.p

h19.p: h19.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . h19.asm

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex
