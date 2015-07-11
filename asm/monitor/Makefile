all: monitor.hex monitor.bin

monitor.hex: monitor.p
	p2hex -F Intel monitor.p

monitor.bin: monitor.p
	p2bin -r '$$-$$' monitor.p

monitor.p: monitor.asm
	asl -L monitor.asm

clean:
	$(RM) *.lst *.p *.bin *.hex
