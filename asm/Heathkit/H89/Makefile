all: mtr89.hex mtr89.bin bootcf89.hex bootcf89.bin bios.bin

mtr89.hex: mtr89.p
	p2hex -F Intel mtr89.p

mtr89.bin: mtr89.p
	p2bin -r '$$-$$' mtr89.p

mtr89.p: mtr89.asm h17def.asm hosequ.asm macros.mac misc.asm mtr88.asm u8250.asm tape.asm u8251.asm z47def.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . mtr89.asm

bootcf89.hex: bootcf89.p
	p2hex -F Intel bootcf89.p

bootcf89.bin: bootcf89.p
	p2bin -r '$$-$$' bootcf89.p

bootcf89.p: bootcf89.asm
	asl -L -cpu 8080 bootcf89.asm

bios.bin: bios.p
	p2bin -r '$$-$$' bios.p

bios.p: bios.asm
	asl -L -cpu 8080 bios.asm

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex
