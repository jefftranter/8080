all: pam8.hex pam8.bin

pam8.hex: pam8.p
	p2hex -F Intel pam8.p

pam8.bin: pam8.p
	p2bin -r '$$-$$' pam8.p

pam8.p: pam8.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . pam8.asm

clean:
	$(RM) *.lst *.p *.bin *.hex
