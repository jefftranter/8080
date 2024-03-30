all: mtr88.hex mtr88.bin

mtr88.hex: mtr88.p
	p2hex -F Intel mtr88.p

mtr88.bin: mtr88.p
	p2bin -r '$$-$$' mtr88.p

mtr88.p: mtr88.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . mtr88.asm

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex
