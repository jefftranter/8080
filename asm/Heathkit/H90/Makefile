all: mtr90.hex mtr90.bin

mtr90.hex: mtr90.p
	p2hex -F Intel mtr90.p

mtr90.bin: mtr90.p
	p2bin -r '$$-$$' mtr90.p

mtr90.p: mtr90.asm h17def.asm hosequ.asm macros.mac misc.asm mtr88.asm u8250.asm tape.asm u8251.asm z47def.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . mtr90.asm

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex
