all: monitor.hex monitor.bin monitorz80.hex monitorz80.bin

monitor.hex: monitor.p
	p2hex -F Intel monitor.p

monitor.bin: monitor.p
	p2bin -r '$$-$$' monitor.p

monitor.p: monitor.asm
	asl -L monitor.asm

monitorz80.hex: monitorz80.p
	p2hex -F Intel monitorz80.p

monitorz80.bin: monitorz80.p
	p2bin -r '$$-$$' monitorz80.p

monitorz80.p: monitorz80.asm
	asl -L monitorz80.asm

clean:
	$(RM) *.lst *.p *.bin *.hex
