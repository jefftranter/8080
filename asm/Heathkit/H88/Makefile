all: mtr88.hex mtr88.bin demo.hex demo.bin

mtr88.hex: mtr88.p
	p2hex -F Intel mtr88.p

mtr88.bin: mtr88.p
	p2bin -r '$$-$$' mtr88.p

mtr88.p: mtr88.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . mtr88.asm

demo.hex: demo.p
	p2hex -F Intel demo.p

demo.bin: demo.p
	p2bin -r '$$-$$' demo.p

demo.p: demo.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . demo.asm

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex
