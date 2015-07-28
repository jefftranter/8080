all: zmon.hex zmon.bin

zmon.hex: zmon.p
	p2hex -F Intel zmon.p

zmon.bin: zmon.p
	p2bin -r '$$-$$' zmon.p

zmon.p: zmon.asm
	asl -L zmon.asm

clean:
	$(RM) *.lst *.p *.bin *.hex
