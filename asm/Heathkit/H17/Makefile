all: h17.hex h17.bin

h17.hex: h17.p
	p2hex -F Intel h17.p

h17.bin: h17.p
	p2bin -r '$$-$$' h17.p

h17.p: h17.asm ascii.asm cdehl.asm chl.asm comp.asm dada2.asm dada.asm dddef.asm devdef.asm dirdef.asm du66.asm ecdef.asm edcon.asm edram.asm edvec.asm esint.asm esval.asm h17.asm h17def.asm hlihl.asm hosdef.asm hosequ.asm indl.asm mtr.asm mu10.asm mu66.asm mu86.asm picdef.asm savall.asm tbra.asm tjmp.asm u8251.asm udd.asm zero.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . h17.asm

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex
