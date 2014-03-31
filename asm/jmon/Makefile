all: jmon.hex jmon.bin

jmon.hex: jmon.p
	p2hex -F Intel jmon.p

jmon.bin: jmon.p
	p2bin -r '$$-$$' jmon.p

jmon.p: jmon.asm
	asl -L -cpu 8080 jmon.asm

clean:
	$(RM) *.lst *.p *.bin *.hex
