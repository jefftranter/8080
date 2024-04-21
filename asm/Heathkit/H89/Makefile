all: mtr89.hex mtr89.bin

mtr89.hex: mtr89.p
	p2hex -F Intel mtr89.p

mtr89.bin: mtr89.p
	p2bin -r '$$-$$' mtr89.p

mtr89.p: mtr89.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . mtr89.asm

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex
